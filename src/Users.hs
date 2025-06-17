module Users ( users ) where

import Common
import Homepage (layout)

import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT, ActionT)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text

users :: ScottyT App ()
users = do
    Scotty.get "/users" listOfUsers
    Scotty.get "/add-user" addUser

listOfUsers :: ActionT App ()
listOfUsers = Scotty.html . renderHtml $ markup
    where
        markup :: Html
        markup = layout $ do
            h1 "Users"
            a ! href "/add-user" $ "Add new user"
            table $ do
                caption "Total users: 4"
                thead $ tr $ do
                    th ! scope "col" $ "#"
                    th ! scope "col" $ "First name"
                    th ! scope "col" $ "Last name"
                    th ! scope "col" $ "Email"
                tbody $ do
                    tr $ do
                        th ! scope "col" $ "1"
                        td "Mark"
                        td "Otto"
                        td "MarkOtto@gmail.com"
                    tr $ do
                        th ! scope "col" $ "2"
                        td "Mark"
                        td "Otto"
                        td "MarkOtto@gmail.com"
                    tr $ do
                        th ! scope "col" $ "3"
                        td "Mark"
                        td "Otto"
                        td "MarkOtto@gmail.com"
                    tr $ do
                        th ! scope "col" $ "4"
                        td "Mark"
                        td "Otto"
                        td "MarkOtto@gmail.com"

addUser :: ActionT App ()
addUser = Scotty.html . renderHtml $ markup
    where
        markup = layout $ form $ do
            label ! for "email" $ "Email"
            input ! required "required"
            label ! for "password" $ "Password"
            input ! required "required"
            label ! for "confirm-password" $ "Confirm password"
            input ! required "required"
            button ! type_ "submit" $ "Save"
