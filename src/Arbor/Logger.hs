{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Arbor.Logger
( runTimedLogT
, logDebug, logInfo, logWarn, logError
, logDebug', logInfo', logWarn', logError'
, logWithoutLoc
, pushLogMessage
, createTimedFastLogger
, withStdOutTimedFastLogger
, MonadLogger(..), LogLevel(..), LoggingT(..), TimedFastLogger
, ToLogStr(..)
)
where

import           Control.Monad.IO.Class
import           Control.Monad.Logger   hiding (logDebug, logError, logInfo, logWarn)
import qualified Data.ByteString.Char8  as S8
import qualified Data.Text              as T
import           System.Log.FastLogger

runTimedLogT :: MonadIO m => LogLevel -> TimedFastLogger -> LoggingT m a -> m a
runTimedLogT logLevel logger =
  runTimedFastLoggerLoggingT logger . filterLogger (\_ lvl -> lvl >= logLevel)

withStdOutTimedFastLogger :: (TimedFastLogger -> IO a) -> IO a
withStdOutTimedFastLogger f = do
  tc <- newTimeCache "%Y-%m-%d %T"
  withTimedFastLogger tc (LogStdout defaultBufSize) $ \logger -> f logger

createTimedFastLogger :: IO (TimedFastLogger, IO ())
createTimedFastLogger = do
  tc <- newTimeCache "%Y-%m-%d %T"
  newTimedFastLogger tc (LogStdout defaultBufSize)

logDebug, logInfo, logWarn, logError :: MonadLogger m => String -> m ()
logDebug = logDebug'
{-# INLINE logDebug #-}

logInfo = logInfo'
{-# INLINE logInfo #-}

logWarn = logWarn'
{-# INLINE logWarn #-}

logError = logError'
{-# INLINE logError #-}

logDebug', logInfo', logWarn', logError' :: (MonadLogger m, ToLogStr s) => s -> m ()
logDebug' = logWithoutLoc "" LevelDebug
{-# INLINE logDebug' #-}

logInfo' = logWithoutLoc "" LevelInfo
{-# INLINE logInfo' #-}

logWarn' = logWithoutLoc "" LevelWarn
{-# INLINE logWarn' #-}

logError' = logWithoutLoc "" LevelError
{-# INLINE logError' #-}

pushLogMessage :: (ToLogStr s) => TimedFastLogger -> LogLevel -> s -> IO ()
pushLogMessage t l s = t (defaultTimedLogStr defaultLoc "" l (toLogStr s))

-- | Run a block using a 'TimedFastLogger'.
runTimedFastLoggerLoggingT :: TimedFastLogger -> LoggingT m a -> m a
runTimedFastLoggerLoggingT tfl m = runLoggingT m $ \a b c d -> tfl (defaultTimedLogStr a b c d)

defaultTimedLogStr :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> FormattedTime
#if MIN_VERSION_fast_logger(0, 2, 0)
              -> LogStr
#else
              -> S8.ByteString
#endif
defaultTimedLogStr loc src level msg time =
#if MIN_VERSION_fast_logger(0, 2, 0)
    "[" `mappend` defaultLogLevelStr level `mappend`
    (if T.null src
        then mempty
        else "#" `mappend` toLogStr src) `mappend`
    "] " `mappend` "[" `mappend` toLogStr time `mappend` "] " `mappend`
    msg `mappend`
    (if isDefaultLoc loc
        then "\n"
        else
            " @(" `mappend`
            toLogStr (S8.pack (fileLocStr loc)) `mappend`
            ")\n")
#else
    S8.concat
        [ S8.pack "["
        , case level of
            LevelOther t -> encodeUtf8 t
            _            -> encodeUtf8 $ pack $ drop 5 $ show level
        , if T.null src
            then S8.empty
            else encodeUtf8 $ '#' `T.cons` src
        , S8.pack "] "
        , S8.pack "["
        , time
        , S8.pack "] "
        , case msg of
            LS s -> encodeUtf8 $ pack s
            LB b -> b
        , S8.pack " @("
        , encodeUtf8 $ pack (fileLocStr loc)
        , S8.pack ")\n"
        ]
#endif

defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr level = case level of
    LevelOther t -> toLogStr t
    _            -> toLogStr $ S8.pack $ drop 5 $ show level

defaultLogStr :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
#if MIN_VERSION_fast_logger(0, 2, 0)
              -> LogStr
#else
              -> S8.ByteString
#endif
defaultLogStr loc src level msg =
#if MIN_VERSION_fast_logger(0, 2, 0)
    "[" `mappend` defaultLogLevelStr level `mappend`
    (if T.null src
        then mempty
        else "#" `mappend` toLogStr src) `mappend`
    "] " `mappend`
    msg `mappend`
    (if isDefaultLoc loc
        then "\n"
        else
            " @(" `mappend`
            toLogStr (S8.pack (fileLocStr loc)) `mappend`
            ")\n")
#else
    S8.concat
        [ S8.pack "["
        , case level of
            LevelOther t -> encodeUtf8 t
            _            -> encodeUtf8 $ pack $ drop 5 $ show level
        , if T.null src
            then S8.empty
            else encodeUtf8 $ '#' `T.cons` src
        , S8.pack "] "
        , case msg of
            LS s -> encodeUtf8 $ pack s
            LB b -> b
        , S8.pack " @("
        , encodeUtf8 $ pack (fileLocStr loc)
        , S8.pack ")\n"
        ]
#endif

-- taken from file-location package
-- turn the TH Loc loaction information into a human readable string
-- leaving out the loc_end parameter
fileLocStr :: Loc -> String
fileLocStr loc = loc_package loc ++ ':' : loc_module loc ++
  ' ' : loc_filename loc ++ ':' : line loc ++ ':' : char loc
  where
    line = show . fst . loc_start
    char = show . snd . loc_start

isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) = True
isDefaultLoc _                                                     = False
