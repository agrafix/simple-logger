{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Logger.Simple
    ( withGlobalLogging, LogConfig(..)
    , setLogLevel, LogLevel(..)
    , logTrace, logDebug, logInfo, logNote, logWarn, logError
    , logFail
    , showText, (<>)
    )
where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import System.IO.Unsafe
import System.Log.FastLogger
import qualified Data.Text as T
import qualified Data.Traversable as T
import qualified GHC.SrcLoc as GHC
import qualified GHC.Stack as GHC

data Loggers
    = Loggers
    { l_file :: Maybe (FastLogger, IO ())
    , l_stderr :: Maybe (FastLogger, IO ())
    , l_timeCache :: IO FormattedTime
    }

data LogConfig
    = LogConfig
    { lc_file :: Maybe FilePath
    , lc_stderr :: Bool
    }

data LogLevel
    = LogTrace
    | LogDebug
    | LogInfo
    | LogNote
    | LogWarn
    | LogError
    deriving (Eq, Show, Read, Ord)

showText :: Show a => a -> T.Text
showText = T.pack . show

logTrace :: (?callStack :: GHC.CallStack) => MonadIO m => T.Text -> m ()
logTrace = doLog LogTrace

logDebug :: (?callStack :: GHC.CallStack) => MonadIO m => T.Text -> m ()
logDebug = doLog LogDebug

logInfo :: (?callStack :: GHC.CallStack) => MonadIO m => T.Text -> m ()
logInfo = doLog LogInfo

logNote :: (?callStack :: GHC.CallStack) => MonadIO m => T.Text -> m ()
logNote = doLog LogNote

logWarn :: (?callStack :: GHC.CallStack) => MonadIO m => T.Text -> m ()
logWarn = doLog LogWarn

logError :: (?callStack :: GHC.CallStack) => MonadIO m => T.Text -> m ()
logError = doLog LogError

-- | Log on error level and calls 'fail'
logFail :: (?callStack :: GHC.CallStack) => MonadIO m => T.Text -> m a
logFail t =
    do doLog LogError t
       fail (T.unpack t)

doLog :: (?callStack :: GHC.CallStack) => MonadIO m => LogLevel -> T.Text -> m ()
doLog ll txt =
    liftIO $
    readIORef logLevel >>= \logLim ->
    when (ll >= logLim) $
    do lgrs <- readIORef loggers
       time <- l_timeCache lgrs
       let loc =
               case GHC.getCallStack ?callStack of
                   ((_, l):_) ->
                       GHC.srcLocFile l <> ":" <> show (GHC.srcLocStartLine l)
                   _ -> "unknown"
           msg =
               "[" <> renderLevel <> " "
               <> toLogStr time
               <> " "
               <> toLogStr loc
               <> "] "
               <> toLogStr txt
               <> "\n"
       forM_ (l_stderr lgrs) $ \(writeLog, _) -> writeLog (renderColor <> msg <> resetColor)
       forM_ (l_file lgrs) $ \(writeLog, _) -> writeLog msg
    where
        renderLevel =
            case ll of
              LogTrace -> "TRACE"
              LogDebug -> "DEBUG"
              LogInfo -> "INFO"
              LogNote -> "NOTE"
              LogWarn -> "WARN"
              LogError -> "ERROR"
        resetColor =
            "\o33[0;0m"
        renderColor =
            case ll of
              LogTrace -> "\o33[0;30m"
              LogDebug -> "\o33[0;34m"
              LogInfo -> "\o33[0;34m"
              LogNote -> "\o33[1;32m"
              LogWarn -> "\o33[0;33m"
              LogError -> "\o33[1;31m"

loggers :: IORef Loggers
loggers =
    unsafePerformIO $
    do tc <- newTimeCache timeFormat
       newIORef (Loggers Nothing Nothing tc)
{-# NOINLINE loggers #-}

logLevel :: IORef LogLevel
logLevel = unsafePerformIO $ newIORef LogDebug
{-# NOINLINE logLevel #-}

-- | Set the verbosity level. Messages at our higher than this level are
-- displayed.  It defaults to 'LogDebug'.
setLogLevel :: LogLevel -> IO ()
setLogLevel = atomicWriteIORef logLevel

-- | Setup global logging. Wrap your 'main' function with this.
withGlobalLogging :: LogConfig -> IO a -> IO a
withGlobalLogging lc f =
    bracket initLogger flushLogger (const f)
    where
      flushLogger (Loggers a b _) =
          do forM_ a $ \(_, flush) -> flush
             forM_ b $ \(_, flush) -> flush
      initLogger =
          do fileLogger <-
                 flip T.mapM (lc_file lc) $ \fp ->
                 do let spec =
                            FileLogSpec
                            { log_file = fp
                            , log_file_size = 1024 * 1024 * 50
                            , log_backup_number = 5
                            }
                    newFastLogger (LogFile spec defaultBufSize)
             stderrLogger <-
                 if lc_stderr lc then Just <$> newFastLogger (LogStderr defaultBufSize) else pure Nothing
             tc <- newTimeCache timeFormat
             let lgrs = Loggers fileLogger stderrLogger tc
             writeIORef loggers lgrs
             pure lgrs

timeFormat :: TimeFormat
timeFormat = "%Y-%m-%d %T %z"
