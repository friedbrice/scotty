{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Scotty.Impl where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Int
import Data.Maybe
import Data.String
import Data.Typeable
import Data.Word
import Numeric.Natural

import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString)
import Control.Monad.Base (MonadBase (..), liftBaseDefault)
import Control.Monad.Catch (Exception, MonadCatch (..), MonadThrow (..), SomeException)
import Control.Monad.Except (ExceptT (..), MonadError (..), mapExceptT, runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, mapReaderT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Default.Class (Default, def)
import GHC.Base (assert)
import Text.Regex (matchRegexAll, mkRegex)

import Network.Wai hiding (Application, Middleware)
import qualified Network.Wai as Wai

import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans.Control as MTC
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Parse as Parse

import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Encoding.Error as TS

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- | Run a scotty application using the warp server.
-- NB: scotty p === scottyT p id
scottyT :: (Monad m, MonadIO n)
        => Warp.Port
        -> (m Response -> IO Response) -- ^ Run monad 'm' into 'IO', called at each action.
        -> ScottyT e m ()
        -> n ()
scottyT p = scottyOptsT def { settings = Warp.setPort p (settings def) }

-- | Run a scotty application using the warp server, passing extra options.
-- NB: scottyOpts opts === scottyOptsT opts id
scottyOptsT :: (Monad m, MonadIO n)
            => Options
            -> (m Response -> IO Response) -- ^ Run monad 'm' into 'IO', called at each action.
            -> ScottyT e m ()
            -> n ()
scottyOptsT opts runActionToIO s = do
    when (verbose opts > 0) .
        liftIO . putStrLn $ "Setting phasers to stun... (port " <> show (Warp.getPort (settings opts)) <> ") (ctrl-c to quit)"
    liftIO . Warp.runSettings (settings opts) =<< scottyAppT runActionToIO s

-- | Run a scotty application using the warp server, passing extra options, and
-- listening on the provided socket.
-- NB: scottySocket opts sock === scottySocketT opts sock id
scottySocketT :: (Monad m, MonadIO n)
              => Options
              -> Socket.Socket
              -> (m Response -> IO Response)
              -> ScottyT e m ()
              -> n ()
scottySocketT opts sock runActionToIO s = do
    when (verbose opts > 0) $ do
        d <- liftIO $ socketDescription sock
        liftIO . putStrLn $ "Setting phasers to stun... (" <> d <> ") (ctrl-c to quit)"
    liftIO . Warp.runSettingsSocket (settings opts) sock =<< scottyAppT runActionToIO s

-- | Turn a scotty application into a WAI 'Application', which can be
-- run with any WAI handler.
-- NB: scottyApp === scottyAppT id
scottyAppT :: (Monad m, Monad n)
           => (m Response -> IO Response) -- ^ Run monad 'm' into 'IO', called at each action.
           -> ScottyT e m ()
           -> n Wai.Application
scottyAppT runActionToIO defs = do
    let s = State.execState (runS defs) def
    let rapp req callback = runActionToIO (foldl (flip ($)) notFoundApp (routes s) req) >>= callback
    return $ foldl (flip ($)) rapp (middlewares s)

notFoundApp :: Monad m => Application m
notFoundApp _ = return . responseBuilder HTTP.status404 [("Content-Type","text/html")]
                       $ fromByteString "<h1>404: File Not Found!</h1>"

-- | Global handler for uncaught exceptions.
--
-- Uncaught exceptions normally become 500 responses.
-- You can use this to selectively override that behavior.
--
-- Note: IO exceptions are lifted into 'ScottyError's by 'stringError'.
-- This has security implications, so you probably want to provide your
-- own defaultHandler in production which does not send out the error
-- strings as 500 responses.
defaultHandler :: (ScottyError e, Monad m) => (e -> ActionT e m ()) -> ScottyT e m ()
defaultHandler f = ScottyT . State.modify . addHandler $ Just (\e -> status HTTP.status500 >> f e)

-- | Use given middleware. Middleware is nested such that the first declared
-- is the outermost middleware (it has first dibs on the request and last action
-- on the response). Every middleware is run on each request.
middleware :: Wai.Middleware -> ScottyT e m ()
middleware = ScottyT . State.modify . addMiddleware

-- | Set global size limit for the request body. Requests with body size exceeding the limit will not be
-- processed and an HTTP response 413 will be returned to the client. Size limit needs to be greater than 0,
-- otherwise the application will terminate on start.
setMaxRequestBodySize :: Kilobytes -> ScottyT e m ()
setMaxRequestBodySize i = assert (i > 0) . ScottyT . State.modify . updateMaxRequestBodySize $ def { maxRequestBodySize = Just i }

-- | get = 'addroute' 'GET'
get :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
get = addroute HTTP.GET

-- | post = 'addroute' 'POST'
post :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
post = addroute HTTP.POST

-- | put = 'addroute' 'PUT'
put :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
put = addroute HTTP.PUT

-- | delete = 'addroute' 'DELETE'
delete :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
delete = addroute HTTP.DELETE

-- | patch = 'addroute' 'PATCH'
patch :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
patch = addroute HTTP.PATCH

-- | options = 'addroute' 'OPTIONS'
options :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
options = addroute HTTP.OPTIONS

-- | Add a route that matches regardless of the HTTP verb.
matchAny :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
matchAny pattern action = ScottyT . State.modify $ \s -> addRoute (route (routeOptions s) (handler s) Nothing pattern action) s

{- | Specify an action to take if nothing else is found. Note: this _always_ matches,
 so should generally be the last route specified.
-}
notFound :: (ScottyError e, MonadIO m) => ActionT e m () -> ScottyT e m ()
notFound action = matchAny (Function (\req -> Just [("path", path req)])) (status HTTP.status404 >> action)

{- | Define a route with a 'HTTP.StdMethod', 'TL.Text' value representing the path spec,
 and a body ('Action') which modifies the response.

 > addroute GET "/" $ text "beam me up!"

 The path spec can include values starting with a colon, which are interpreted
 as /captures/. These are named wildcards that can be looked up with 'param'.

 > addroute GET "/foo/:bar" $ do
 >     v <- param "bar"
 >     text v

 >>> curl http://localhost:3000/foo/something
 something
-}
addroute :: (ScottyError e, MonadIO m) => HTTP.StdMethod -> RoutePattern -> ActionT e m () -> ScottyT e m ()
addroute method pat action = ScottyT . State.modify $ \s -> addRoute (route (routeOptions s) (handler s) (Just method) pat action) s

route :: (ScottyError e, MonadIO m) => RouteOptions -> ErrorHandler e m -> Maybe HTTP.StdMethod -> RoutePattern -> ActionT e m () -> Middleware m
route opts h method pat action app req =
  let tryNext = app req

      methodMatches :: Bool
      methodMatches =
        case method of
          Nothing -> True
          Just m -> Right m == HTTP.parseMethod (requestMethod req)
   in if methodMatches
        then case matchRoute pat req of
          Just captures -> do
            env <- liftIO $ catch (Right <$> mkEnv req captures opts) (return . Left)
            res <- evalAction h env action
            maybe tryNext return res
          Nothing -> tryNext
        else tryNext

evalAction :: (ScottyError e, Monad m) => ErrorHandler e m -> Either ScottyException ActionEnv -> ActionT e m () -> m (Maybe Response)
evalAction _ (Left (RequestException msg s)) _ = return . Just . responseBuilder s [("Content-Type", "text/html")] $ fromByteString msg
evalAction h (Right env) action = runAction h env action

matchRoute :: RoutePattern -> Request -> Maybe [Param]
matchRoute (Literal pat) req
  | pat == path req = Just []
  | otherwise = Nothing
matchRoute (Function fun) req = fun req
matchRoute (Capture pat) req = go (TL.split (== '/') pat) (compress . TL.split (== '/') $ path req) []
 where
  go [] [] prs = Just prs -- request string and pattern match!
  go [] r prs
    | TL.null (mconcat r) = Just prs -- in case request has trailing slashes
    | otherwise = Nothing -- request string is longer than pattern
  go p [] prs
    | TL.null (mconcat p) = Just prs -- in case pattern has trailing slashes
    | otherwise = Nothing -- request string is not long enough
  go (p : ps) (r : rs) prs
    | p == r = go ps rs prs -- equal literals, keeping checking
    | TL.null p = Nothing -- p is null, but r is not, fail
    | TL.head p == ':' = go ps rs $ (TL.tail p, r) : prs -- p is a capture, add to params
    | otherwise = Nothing -- both literals, but unequal, fail
  compress ("" : rest@("" : _)) = compress rest
  compress (x : xs) = x : compress xs
  compress [] = []

-- Pretend we are at the top level.
path :: Request -> TL.Text
path = TL.fromStrict . TS.cons '/' . TS.intercalate "/" . pathInfo

-- Stolen from wai-extra's Network.Wai.Parse, modified to accept body as list of Bytestrings.
-- Reason: WAI's getRequestBodyChunk is an IO action that returns the body as chunks.
-- Once read, they can't be read again. We read them into a lazy Bytestring, so Scotty
-- user can get the raw body, even if they also want to call wai-extra's parsing routines.
parseRequestBody ::
  MonadIO m =>
  [BS.ByteString] ->
  Parse.BackEnd y ->
  Request ->
  m ([Parse.Param], [Parse.File y])
parseRequestBody bl s r =
  case Parse.getRequestBodyType r of
    Nothing -> return ([], [])
    Just rbt -> do
      mvar <- liftIO $ newMVar bl -- MVar is a bit of a hack so we don't have to inline
      -- large portions of Network.Wai.Parse
      let provider = modifyMVar mvar $ \bsold -> case bsold of
            [] -> return ([], BS.empty)
            (b : bs) -> return (bs, b)
      liftIO $ Parse.sinkRequestBody s rbt provider

mkEnv :: forall m. MonadIO m => Request -> [Param] -> RouteOptions -> m ActionEnv
mkEnv req captures opts = do
  bodyState <- liftIO $ newMVar BodyUntouched

  let rbody = getRequestBodyChunk req

      safeBodyReader :: IO BS.ByteString
      safeBodyReader = do
        state' <- takeMVar bodyState
        let direct = putMVar bodyState BodyCorrupted >> rbody
        case state' of
          s@(BodyCached _ []) ->
            do
              putMVar bodyState s
              return BS.empty
          BodyCached b (chunk : rest) ->
            do
              putMVar bodyState $ BodyCached b rest
              return chunk
          BodyUntouched -> direct
          BodyCorrupted -> direct

      bs :: IO BL.ByteString
      bs = do
        state' <- takeMVar bodyState
        case state' of
          s@(BodyCached b _) ->
            do
              putMVar bodyState s
              return b
          BodyCorrupted -> throwM BodyPartiallyStreamed
          BodyUntouched ->
            do
              chunks <- readRequestBody rbody return (maxRequestBodySize opts)
              let b = BL.fromChunks chunks
              putMVar bodyState $ BodyCached b chunks
              return b

      shouldParseBody = isJust $ Parse.getRequestBodyType req

  (formparams, fs) <-
    if shouldParseBody
      then liftIO $ do
        wholeBody <- BL.toChunks `fmap` bs
        parseRequestBody wholeBody Parse.lbsBackEnd req
      else return ([], [])

  let convert (k, v) = (strictByteStringToLazyText k, strictByteStringToLazyText v)
      parameters = captures <> fmap convert formparams <> queryparams
      queryparams = parseEncodedParams $ rawQueryString req

  return $ Env req parameters bs safeBodyReader [(strictByteStringToLazyText k, fi) | (k, fi) <- fs]

parseEncodedParams :: BS.ByteString -> [Param]
parseEncodedParams bs = [(TL.fromStrict k, TL.fromStrict $ fromMaybe "" v) | (k, v) <- HTTP.parseQueryText bs]

{- | Match requests using a regular expression.
   Named captures are not yet supported.

 > get (regex "^/f(.*)r$") $ do
 >    path <- param "0"
 >    cap <- param "1"
 >    text $ mconcat ["Path: ", path, "\nCapture: ", cap]

 >>> curl http://localhost:3000/foo/bar
 Path: /foo/bar
 Capture: oo/ba
-}
regex :: String -> RoutePattern
regex pattern = Function $ \req ->
  fmap
    (fmap (bimap (TL.pack . show) TL.pack) . zip [0 :: Int ..] . strip)
    (matchRegexAll rgx . TL.unpack $ path req)
 where
  rgx = mkRegex pattern
  strip (_, match, _, subs) = match : subs

{- | Standard Sinatra-style route. Named captures are prepended with colons.
   This is the default route type generated by OverloadedString routes. i.e.

 > get (capture "/foo/:bar") $ ...

   and

 > {\-# LANGUAGE OverloadedStrings #-\}
 > ...
 > get "/foo/:bar" $ ...

   are equivalent.
-}
capture :: String -> RoutePattern
capture = fromString

{- | Build a route based on a function which can match using the entire 'Request' object.
   'Nothing' indicates the route does not match. A 'Just' value indicates
   a successful match, optionally returning a list of key-value pairs accessible
   by 'param'.

 > get (function $ \req -> Just [("version", TL.pack $ show $ httpVersion req)]) $ do
 >     v <- param "version"
 >     text v

 >>> curl http://localhost:3000/
 HTTP/1.1
-}
function :: (Request -> Maybe [Param]) -> RoutePattern
function = Function

-- | Build a route that requires the requested path match exactly, without captures.
literal :: String -> RoutePattern
literal = Literal . TL.pack

-- Nothing indicates route failed (due to Next) and pattern matching should continue.
-- Just indicates a successful response.
runAction :: (ScottyError e, Monad m) => ErrorHandler e m -> ActionEnv -> ActionT e m () -> m (Maybe Response)
runAction h env action = do
    (e,r) <- flip State.runStateT def
           . flip runReaderT env
           . runExceptT
           . runAM
           $ action `catchError` defH h
    return $ either (const Nothing) (const . Just $ mkResponse r) e

-- | Default error handler for all actions.
defH :: (ScottyError e, Monad m) => ErrorHandler e m -> ActionError e -> ActionT e m ()
defH _          (Redirect url)    = do
    status HTTP.status302
    setHeader "Location" url
defH Nothing    (ActionError s e)   = do
    status s
    let code = TL.pack . show $ HTTP.statusCode s
    let msg = TL.fromStrict . TS.decodeUtf8 $ HTTP.statusMessage s
    html $ mconcat ["<h1>", code, " ", msg, "</h1>", showError e]
defH h@(Just f) (ActionError _ e)   = f e `catchError` defH h -- so handlers can throw exceptions themselves
defH _          Next              = next
defH _          Finish            = return ()

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions
-- turn into HTTP 500 responses.
raise :: (ScottyError e, Monad m) => e -> ActionT e m a
raise = raiseStatus HTTP.status500

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions turn into HTTP responses corresponding to the given status.
raiseStatus :: (ScottyError e, Monad m) => HTTP.Status -> e -> ActionT e m a
raiseStatus s = throwError . ActionError s

-- | Abort execution of this action and continue pattern matching routes.
-- Like an exception, any code after 'next' is not executed.
--
-- As an example, these two routes overlap. The only way the second one will
-- ever run is if the first one calls 'next'.
--
-- > get "/foo/:bar" $ do
-- >   w :: Text <- param "bar"
-- >   unless (w == "special") next
-- >   text "You made a request to /foo/special"
-- >
-- > get "/foo/:baz" $ do
-- >   w <- param "baz"
-- >   text $ "You made a request to: " <> w
next :: (ScottyError e, Monad m) => ActionT e m a
next = throwError Next

-- | Catch an exception thrown by 'raise'.
--
-- > raise "just kidding" `rescue` (\msg -> text msg)
rescue :: (ScottyError e, Monad m) => ActionT e m a -> (e -> ActionT e m a) -> ActionT e m a
rescue action h = catchError action $ \e -> case e of
    ActionError _ err -> h err            -- handle errors
    other             -> throwError other -- rethrow internal error types

-- | Like 'liftIO', but catch any IO exceptions and turn them into 'ScottyError's.
liftAndCatchIO :: (ScottyError e, MonadIO m) => IO a -> ActionT e m a
liftAndCatchIO io = ActionT $ do
    r <- liftIO $ fmap Right io `catch` (\ e -> return . Left . stringError $ show (e :: SomeException))
    either throwError return r

-- | Redirect to given URL. Like throwing an uncatchable exception. Any code after the call to redirect
-- will not be run.
--
-- > redirect "http://www.google.com"
--
-- OR
--
-- > redirect "/foo/bar"
redirect :: (ScottyError e, Monad m) => TL.Text -> ActionT e m a
redirect = throwError . Redirect

-- | Finish the execution of the current action. Like throwing an uncatchable
-- exception. Any code after the call to finish will not be run.
--
-- /Since: 0.10.3/
finish :: (ScottyError e, Monad m) => ActionT e m a
finish = throwError Finish

-- | Get the 'Request' object.
request :: Monad m => ActionT e m Request
request = ActionT $ asks getReq

-- | Get list of uploaded files.
files :: Monad m => ActionT e m [File]
files = ActionT $ asks getFiles

-- | Get a request header. Header name is case-insensitive.
header :: (ScottyError e, Monad m) => TL.Text -> ActionT e m (Maybe TL.Text)
header k = do
    hs <- fmap requestHeaders request
    return . fmap strictByteStringToLazyText $ lookup (CI.mk (lazyTextToStrictByteString k)) hs

-- | Get all the request headers. Header names are case-insensitive.
headers :: (ScottyError e, Monad m) => ActionT e m [(TL.Text, TL.Text)]
headers = do
    hs <- fmap requestHeaders request
    return [ ( strictByteStringToLazyText (CI.original k)
             , strictByteStringToLazyText v)
           | (k,v) <- hs ]

-- | Get the request body.
body :: (ScottyError e,  MonadIO m) => ActionT e m BL.ByteString
body = ActionT ask >>= liftIO . getBody

-- | Get an IO action that reads body chunks
--
-- * This is incompatible with 'body' since 'body' consumes all chunks.
bodyReader :: Monad m => ActionT e m (IO BS.ByteString)
bodyReader = ActionT $ asks getBodyChunk

-- | Parse the request body as a JSON object and return it.
--
--   If the JSON object is malformed, this sets the status to
--   400 Bad Request, and throws an exception.
--
--   If the JSON fails to parse, this sets the status to
--   422 Unprocessable Entity.
--
--   These status codes are as per https://www.restapitutorial.com/httpstatuscodes.html.
jsonData :: (Aeson.FromJSON a, ScottyError e, MonadIO m) => ActionT e m a
jsonData = do
    b <- body
    when (b == "") $ do
      let htmlError = "jsonData - No data was provided."
      raiseStatus HTTP.status400 $ stringError htmlError
    case Aeson.eitherDecode b of
      Left err -> do
        let htmlError = "jsonData - malformed."
              `mappend` " Data was: " `mappend` BL.unpack b
              `mappend` " Error was: " `mappend` err
        raiseStatus HTTP.status400 $ stringError htmlError
      Right value -> case Aeson.fromJSON value of
        Aeson.Error err -> do
          let htmlError = "jsonData - failed parse."
                `mappend` " Data was: " `mappend` BL.unpack b `mappend` "."
                `mappend` " Error was: " `mappend` err
          raiseStatus HTTP.status422 $ stringError htmlError
        Aeson.Success a ->
          return a

-- | Get a parameter. First looks in captures, then form data, then query parameters.
--
-- * Raises an exception which can be caught by 'rescue' if parameter is not found.
--
-- * If parameter is found, but 'read' fails to parse to the correct type, 'next' is called.
--   This means captures are somewhat typed, in that a route won't match if a correctly typed
--   capture cannot be parsed.
param :: (Parsable a, ScottyError e, Monad m) => TL.Text -> ActionT e m a
param k = do
    val <- ActionT $ asks (lookup k . getParams)
    case val of
        Nothing -> raise . stringError $ "Param: " <> TL.unpack k <> " not found!"
        Just v  -> either (const next) return $ parseParam v

-- | Get all parameters from capture, form and query (in that order).
params :: Monad m => ActionT e m [Param]
params = ActionT $ asks getParams

-- | Minimum implemention: 'parseParam'
class Parsable a where
    -- | Take a 'TL.Text' value and parse it as 'a', or fail with a message.
    parseParam :: TL.Text -> Either TL.Text a

    -- | Default implementation parses comma-delimited lists.
    --
    -- > parseParamList t = mapM parseParam (TL.split (== ',') t)
    parseParamList :: TL.Text -> Either TL.Text [a]
    parseParamList t = mapM parseParam (TL.split (== ',') t)

-- No point using 'read' for Text, ByteString, Char, and String.
instance Parsable TL.Text where parseParam = Right
instance Parsable TS.Text where parseParam = Right . TL.toStrict
instance Parsable BS.ByteString where parseParam = Right . lazyTextToStrictByteString
instance Parsable BL.ByteString where parseParam = Right . TL.encodeUtf8
-- | Overrides default 'parseParamList' to parse String.
instance Parsable Char where
    parseParam t = case TL.unpack t of
                    [c] -> Right c
                    _   -> Left "parseParam Char: no parse"
    parseParamList = Right . TL.unpack -- String
-- | Checks if parameter is present and is null-valued, not a literal '()'.
-- If the URI requested is: '/foo?bar=()&baz' then 'baz' will parse as (), where 'bar' will not.
instance Parsable () where
    parseParam t = if TL.null t then Right () else Left "parseParam Unit: no parse"

instance (Parsable a) => Parsable [a] where parseParam = parseParamList

instance Parsable Bool where
    parseParam t
      | t' == TL.toCaseFold "true" = Right True
      | t' == TL.toCaseFold "false" = Right False
      | otherwise = Left "parseParam Bool: no parse"
      where
          t' = TL.toCaseFold t

instance Parsable Double where parseParam = readEither
instance Parsable Float where parseParam = readEither

instance Parsable Int where parseParam = readEither
instance Parsable Int8 where parseParam = readEither
instance Parsable Int16 where parseParam = readEither
instance Parsable Int32 where parseParam = readEither
instance Parsable Int64 where parseParam = readEither
instance Parsable Integer where parseParam = readEither

instance Parsable Word where parseParam = readEither
instance Parsable Word8 where parseParam = readEither
instance Parsable Word16 where parseParam = readEither
instance Parsable Word32 where parseParam = readEither
instance Parsable Word64 where parseParam = readEither
instance Parsable Natural where parseParam = readEither

-- | Useful for creating 'Parsable' instances for things that already implement 'Read'. Ex:
--
-- > instance Parsable Int where parseParam = readEither
readEither :: Read a => TL.Text -> Either TL.Text a
readEither t = case [ x | (x,"") <- reads (TL.unpack t) ] of
                [x] -> Right x
                []  -> Left "readEither: no parse"
                _   -> Left "readEither: ambiguous parse"

-- | Set the HTTP response status. Default is 200.
status :: Monad m => HTTP.Status -> ActionT e m ()
status = ActionT . State.modify . setStatus

-- Not exported, but useful in the functions below.
changeHeader :: Monad m
             => (CI.CI BS.ByteString -> BS.ByteString -> [(HTTP.HeaderName, BS.ByteString)] -> [(HTTP.HeaderName, BS.ByteString)])
             -> TL.Text -> TL.Text -> ActionT e m ()
changeHeader f k = ActionT
                 . State.modify
                 . setHeaderWith
                 . f (CI.mk $ lazyTextToStrictByteString k)
                 . lazyTextToStrictByteString

-- | Add to the response headers. Header names are case-insensitive.
addHeader :: Monad m => TL.Text -> TL.Text -> ActionT e m ()
addHeader = changeHeader add

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
setHeader :: Monad m => TL.Text -> TL.Text -> ActionT e m ()
setHeader = changeHeader replace

-- | Set the body of the response to the given 'TL.Text' value. Also sets \"Content-Type\"
-- header to \"text/plain; charset=utf-8\" if it has not already been set.
text :: (ScottyError e, Monad m) => TL.Text -> ActionT e m ()
text t = do
    changeHeader addIfNotPresent "Content-Type" "text/plain; charset=utf-8"
    raw $ TL.encodeUtf8 t

-- | Set the body of the response to the given 'TL.Text' value. Also sets \"Content-Type\"
-- header to \"text/html; charset=utf-8\" if it has not already been set.
html :: (ScottyError e, Monad m) => TL.Text -> ActionT e m ()
html t = do
    changeHeader addIfNotPresent "Content-Type" "text/html; charset=utf-8"
    raw $ TL.encodeUtf8 t

-- | Send a file as the response. Doesn't set the \"Content-Type\" header, so you probably
-- want to do that on your own with 'setHeader'. Setting a status code will have no effect
-- because Warp will overwrite that to 200 (see 'Network.Wai.Handler.Warp.Internal.sendResponse').
file :: Monad m => FilePath -> ActionT e m ()
file = ActionT . State.modify . setContent . ContentFile

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json; charset=utf-8\" if it has not already been set.
json :: (Aeson.ToJSON a, ScottyError e, Monad m) => a -> ActionT e m ()
json v = do
    changeHeader addIfNotPresent "Content-Type" "application/json; charset=utf-8"
    raw $ Aeson.encode v

-- | Set the body of the response to a Source. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'setHeader'.
stream :: Monad m => StreamingBody -> ActionT e m ()
stream = ActionT . State.modify . setContent . ContentStream

-- | Set the body of the response to the given 'BL.ByteString' value. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'setHeader'.
raw :: Monad m => BL.ByteString -> ActionT e m ()
raw = ActionT . State.modify . setContent . ContentBuilder . fromLazyByteString

lazyTextToStrictByteString :: TL.Text -> BS.ByteString
lazyTextToStrictByteString = TS.encodeUtf8 . TL.toStrict

strictByteStringToLazyText :: BS.ByteString -> TL.Text
strictByteStringToLazyText = TL.fromStrict . TS.decodeUtf8With TS.lenientDecode

setContent :: Content -> ScottyResponse -> ScottyResponse
setContent c sr = sr{srContent = c}

setHeaderWith :: ([(HTTP.HeaderName, BS.ByteString)] -> [(HTTP.HeaderName, BS.ByteString)]) -> ScottyResponse -> ScottyResponse
setHeaderWith f sr = sr{srHeaders = f (srHeaders sr)}

setStatus :: HTTP.Status -> ScottyResponse -> ScottyResponse
setStatus s sr = sr{srStatus = s}

-- Note: we currently don't support responseRaw, which may be useful
-- for websockets. However, we always read the request body, which
-- is incompatible with responseRaw responses.
mkResponse :: ScottyResponse -> Response
mkResponse sr = case srContent sr of
  ContentBuilder b -> responseBuilder s h b
  ContentFile f -> responseFile s h f Nothing
  ContentStream str -> responseStream s h str
 where
  s = srStatus sr
  h = srHeaders sr

-- Note: we assume headers are not sensitive to order here (RFC 2616 specifies they are not)
replace :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
replace k v = add k v . filter ((/= k) . fst)

add :: a -> b -> [(a, b)] -> [(a, b)]
add k v m = (k, v) : m

addIfNotPresent :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
addIfNotPresent k v = go
 where
  go [] = [(k, v)]
  go l@((x, y) : r)
    | x == k = l
    | otherwise = (x, y) : go r

-- Assemble a description from the Socket's PortID.
socketDescription :: Socket.Socket -> IO String
socketDescription sock = do
  sockName <- Socket.getSocketName sock
  case sockName of
    Socket.SockAddrUnix u -> return $ "unix socket " <> u
    _ -> (\port -> "port " <> show port) <$> Socket.socketPort sock

-- return request body or throw an exception if request body too big
readRequestBody :: IO BS.ByteString -> ([BS.ByteString] -> IO [BS.ByteString]) -> Maybe Kilobytes -> IO [BS.ByteString]
readRequestBody rbody prefix maxSize = do
  b <- rbody
  if BS.null b
    then prefix []
    else do
      checkBodyLength maxSize
      readRequestBody rbody (prefix . (b :)) maxSize
 where
  checkBodyLength :: Maybe Kilobytes -> IO ()
  checkBodyLength (Just maxSize') = prefix [] >>= \bodySoFar -> when (isBigger bodySoFar maxSize') readUntilEmpty
  checkBodyLength Nothing = return ()
  isBigger bodySoFar maxSize' = (BS.length . BS.concat $ bodySoFar) > maxSize' * 1024
  readUntilEmpty = rbody >>= \b -> if BS.null b then throwM (RequestException (TS.encodeUtf8 . TS.pack $ "Request is too big Jim!") HTTP.status413) else readUntilEmpty

--------------------- Options -----------------------
data Options = Options
  { -- | 0 = silent, 1(def) = startup banner
    verbose :: Int
  , -- | Warp 'Settings'
    -- Note: to work around an issue in warp,
    -- the default FD cache duration is set to 0
    -- so changes to static files are always picked
    -- up. This likely has performance implications,
    -- so you may want to modify this for production
    -- servers using `setFdCacheDuration`.
    settings :: Warp.Settings
  }

instance Default Options where
  def = Options 1 Warp.defaultSettings

newtype RouteOptions -- max allowed request size in KB
  = RouteOptions {maxRequestBodySize :: Maybe Kilobytes}

instance Default RouteOptions where
  def = RouteOptions Nothing

type Kilobytes = Int

----- Transformer Aware Applications/Middleware -----
type Middleware m = Application m -> Application m
type Application m = Request -> m Response

--------------- Scotty Applications -----------------
data ScottyState e m = ScottyState
  { middlewares :: [Wai.Middleware]
  , routes :: [Middleware m]
  , handler :: ErrorHandler e m
  , routeOptions :: RouteOptions
  }

instance Default (ScottyState e m) where
  def = ScottyState [] [] Nothing def

addMiddleware :: Wai.Middleware -> ScottyState e m -> ScottyState e m
addMiddleware m s@ScottyState{middlewares = ms} = s{middlewares = m : ms}

addRoute :: Middleware m -> ScottyState e m -> ScottyState e m
addRoute r s@ScottyState{routes = rs} = s{routes = r : rs}

addHandler :: ErrorHandler e m -> ScottyState e m -> ScottyState e m
addHandler h s = s{handler = h}

updateMaxRequestBodySize :: RouteOptions -> ScottyState e m -> ScottyState e m
updateMaxRequestBodySize RouteOptions{..} s@ScottyState{routeOptions = ro} =
  let ro' = ro{maxRequestBodySize = maxRequestBodySize}
   in s{routeOptions = ro'}

newtype ScottyT e m a = ScottyT {runS :: State.State (ScottyState e m) a}
  deriving (Applicative, Functor, Monad) via State.State (ScottyState e m)

------------------ Scotty Errors --------------------
data ActionError e
  = Redirect TL.Text
  | Next
  | Finish
  | ActionError HTTP.Status e

{- | In order to use a custom exception type (aside from 'Text'), you must
 define an instance of 'ScottyError' for that type.
-}
class ScottyError e where
  stringError :: String -> e
  showError :: e -> TL.Text

instance ScottyError TL.Text where
  stringError = TL.pack
  showError = id

instance ScottyError e => ScottyError (ActionError e) where
  stringError = ActionError HTTP.status500 . stringError
  showError (Redirect url) = url
  showError Next = TL.pack "Next"
  showError Finish = TL.pack "Finish"
  showError (ActionError _ e) = showError e

type ErrorHandler e m = Maybe (e -> ActionT e m ())

data ScottyException
  = RequestException BS.ByteString HTTP.Status
  deriving (Show, Typeable)

instance Exception ScottyException

------------------ Scotty Actions -------------------
type Param = (TL.Text, TL.Text)

type File = (TL.Text, Parse.FileInfo BL.ByteString)

data ActionEnv = Env
  { getReq :: Request
  , getParams :: [Param]
  , getBody :: IO BL.ByteString
  , getBodyChunk :: IO BS.ByteString
  , getFiles :: [File]
  }

data RequestBodyState
  = BodyUntouched
  | BodyCached BL.ByteString [BS.ByteString] -- whole body, chunks left to stream
  | BodyCorrupted

data BodyPartiallyStreamed = BodyPartiallyStreamed deriving (Show, Typeable)

instance Exception BodyPartiallyStreamed

data Content
  = ContentBuilder Builder
  | ContentFile FilePath
  | ContentStream StreamingBody

data ScottyResponse = SR
  { srStatus :: HTTP.Status
  , srHeaders :: HTTP.ResponseHeaders
  , srContent :: Content
  }

instance Default ScottyResponse where
  def = SR HTTP.status200 [] (ContentBuilder mempty)

newtype ActionT e m a = ActionT {runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (State.StateT ScottyResponse m)) a}
  deriving (Applicative, Functor, MonadIO) via ExceptT (ActionError e) (ReaderT ActionEnv (State.StateT ScottyResponse m))

instance (Monad m, ScottyError e) => Monad (ActionT e m) where
  return = ActionT . return
  ActionT m >>= k = ActionT (m >>= runAM . k)

instance (Monad m, ScottyError e) => MonadFail (ActionT e m) where
  fail = ActionT . throwError . stringError

instance (Monad m, ScottyError e) => Alternative (ActionT e m) where
  empty = mzero
  (<|>) = mplus

instance (Monad m, ScottyError e) => MonadPlus (ActionT e m) where
  mzero = ActionT . ExceptT . return $ Left Next
  ActionT m `mplus` ActionT n = ActionT . ExceptT $ do
    a <- runExceptT m
    case a of
      Left _ -> runExceptT n
      Right r -> return $ Right r

instance MonadTrans (ActionT e) where
  lift = ActionT . lift . lift . lift

instance (ScottyError e, Monad m) => MonadError (ActionError e) (ActionT e m) where
  throwError = ActionT . throwError

  catchError (ActionT m) f = ActionT (catchError m (runAM . f))

instance (MonadBase b m, ScottyError e) => MonadBase b (ActionT e m) where
  liftBase = liftBaseDefault

instance (MonadThrow m, ScottyError e) => MonadThrow (ActionT e m) where
  throwM = ActionT . throwM

instance (MonadCatch m, ScottyError e) => MonadCatch (ActionT e m) where
  catch (ActionT m) f = ActionT (m `catch` (runAM . f))

instance MTC.MonadTransControl (ActionT e) where
  type StT (ActionT e) a = MTC.StT (State.StateT ScottyResponse) (MTC.StT (ReaderT ActionEnv) (MTC.StT (ExceptT (ActionError e)) a))
  liftWith = \f ->
    ActionT
      ( MTC.liftWith $ \run ->
          MTC.liftWith $ \run' ->
            MTC.liftWith $ \run'' ->
              f $ run'' . run' . run . runAM
      )
  restoreT = ActionT . MTC.restoreT . MTC.restoreT . MTC.restoreT

instance (ScottyError e, MTC.MonadBaseControl b m) => MTC.MonadBaseControl b (ActionT e m) where
  type StM (ActionT e m) a = MTC.ComposeSt (ActionT e) m a
  liftBaseWith = MTC.defaultLiftBaseWith
  restoreM = MTC.defaultRestoreM

instance (MonadReader r m, ScottyError e) => MonadReader r (ActionT e m) where
  {-# INLINE ask #-}
  ask = lift ask
  {-# INLINE local #-}
  local f = ActionT . mapExceptT (mapReaderT (State.mapStateT $ local f)) . runAM

instance (State.MonadState s m, ScottyError e) => State.MonadState s (ActionT e m) where
  {-# INLINE get #-}
  get = lift State.get
  {-# INLINE put #-}
  put = lift . State.put

instance Semigroup a => Semigroup (ScottyT e m a) where
  x <> y = (<>) <$> x <*> y

instance Monoid a => Monoid (ScottyT e m a) where
  mempty = return mempty

instance (Monad m, Semigroup a) => Semigroup (ActionT e m a) where
  x <> y = (<>) <$> x <*> y

instance (Monad m, ScottyError e, Monoid a) => Monoid (ActionT e m a) where
  mempty = return mempty

------------------ Scotty Routes --------------------
data RoutePattern
  = Capture TL.Text
  | Literal TL.Text
  | Function (Request -> Maybe [Param])

instance IsString RoutePattern where
  fromString = Capture . TL.pack
