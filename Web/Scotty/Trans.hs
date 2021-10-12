-- | It should be noted that most of the code snippets below depend on the
-- OverloadedStrings language pragma.
--
-- The functions in this module allow an arbitrary monad to be embedded
-- in Scotty's monad transformer stack in order that Scotty be combined
-- with other DSLs.
--
-- Scotty is set up by default for development mode. For production servers,
-- you will likely want to modify 'settings' and the 'defaultHandler'. See
-- the comments on each of these functions for more information.
module Web.Scotty.Trans
  ( -- * scotty-to-WAI
    scottyT
  , scottyAppT
  , scottyOptsT
  , scottySocketT
  , Options (..)
    -- * Defining Middleware and Routes
    --
    -- | 'Middleware' and routes are run in the order in which they
    -- are defined. All middleware is run first, followed by the first
    -- route that matches. If no route matches, a 404 response is given.
  , middleware
  , get
  , post
  , put
  , delete
  , patch
  , options
  , addroute
  , matchAny
  , notFound
  , setMaxRequestBodySize
    -- ** Route Patterns
  , capture
  , regex
  , function
  , literal
    -- ** Accessing the Request, Captures, and Query Parameters
  , request
  , header
  , headers
  , body
  , bodyReader
  , param
  , params
  , jsonData
  , files
    -- ** Modifying the Response and Redirecting
  , status
  , addHeader
  , setHeader
  , redirect
    -- ** Setting Response Body
    --
    -- | Note: only one of these should be present in any given route
    -- definition, as they completely replace the current 'Response' body.
  , text
  , html
  , file
  , json
  , stream
  , raw
    -- ** Exceptions
  , raise
  , raiseStatus
  , rescue
  , next
  , finish
  , defaultHandler
  , ScottyError (..)
  , liftAndCatchIO
    -- * Parsing Parameters
  , Param
  , Parsable (..)
  , readEither
    -- * Types
  , RoutePattern
  , File
  , Kilobytes
    -- * Monad Transformers
  , ScottyT
  , ActionT
  ) where

import Web.Scotty.Impl
