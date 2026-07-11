module Homepage
    ( guest
    , logged
    ) where

import Common

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)

import Auth.Types

guest :: Handler ()
guest = withLayout $ do
    h1 "Simple Web Framework"
    p "Welcome in our new web app when live reloading almost works!"
    button "Example button"
    a ! href "/webauthn/register" $ button "Register"

logged :: SessionData -> Handler()
logged _ = withLayout $ do
    h1 "Welcome my friend!"
