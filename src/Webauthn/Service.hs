module Webauthn.Service (service) where

import Common

import Web.Scotty.Trans (ScottyT)
import qualified Web.Scotty.Trans as Scotty

service :: ScottyT App ()
service = do
  Scotty.post "/webauthn/register/begin" undefined
  Scotty.post "/webauthn/register/complete" undefined
  Scotty.post "/webauthn/login/begin" undefined
  Scotty.post "/webauthn/login/complete" undefined

