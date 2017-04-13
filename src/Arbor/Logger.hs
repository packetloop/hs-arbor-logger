{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module is redundant when https://github.com/kazu-yamamoto/logger/pull/87
-- is merged that updates 'monad-logger' to support timestamps

module Arbor.Logger
( runLogT, runLogT'
, logDebug, logInfo, logWarn, logError)
where

import           Control.Exception.Lifted    (bracket)
import           Control.Monad.Base          (MonadBase (liftBase))
import           Control.Monad.IO.Class
import           Control.Monad.Logger        hiding (logDebug, logError,
                                              logInfo, logWarn)
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import qualified Data.ByteString.Char8       as S8
import qualified Data.Text                   as T
import           System.Log.FastLogger

runLogT :: LogLevel -> LoggingT IO () -> IO ()
runLogT logLevel f = liftIO $ do
  tc <- newTimeCache "%Y-%m-%d %T"
  withTimedFastLogger tc (LogStdout defaultBufSize) $ \logger ->
    runTimedFastLoggerLoggingT logger . filterLogger (\_ lvl -> lvl >= logLevel) $ f

runLogT' :: MonadBaseControl IO m
         => LogLevel
         -> LoggingT m a
         -> m a
runLogT' logLevel f = bracket
  (liftBase mkLogger)
  (liftBase . snd)
  $ \(l, _) -> runTimedFastLoggerLoggingT l . filterLogger (\_ lvl -> lvl >= logLevel) $ f
  where
    mkLogger = liftBase $ do
      tc <- newTimeCache "%Y-%m-%d %T"
      newTimedFastLogger tc (LogStdout defaultBufSize)

logDebug :: MonadLogger m => String -> m ()
logDebug = logDebugN . T.pack
{-# INLINE logDebug #-}

logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoN . T.pack
{-# INLINE logInfo #-}

logWarn :: MonadLogger m => String -> m ()
logWarn = logWarnN . T.pack
{-# INLINE logWarn #-}

logError :: MonadLogger m => String -> m ()
logError = logErrorN . T.pack
{-# INLINE logError #-}

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
