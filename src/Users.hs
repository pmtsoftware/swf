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
        markup = layout $ div ! class_ "p-5 mb-4 bg-body-tertiary rounded-3" $ do
            h1 "Users"
            a ! href "/add-user" $ "Add new user"
            table ! class_ "table table-striped table-borderless mt-2" $ do
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
        markup = layout $ div ! class_ "p-5 mb-4 bg-body-tertiary rounded-3" $ do
            form $ do
                div ! class_ "row mb-3" $ do
                    label ! class_ "col-sm-2 col-form-label" ! for "email" $ "Email"
                    div ! class_ "col-sm-10" $ do
                        input ! type_ "email" ! class_ "form-control" ! id "email" ! required "required"
                div ! class_ "row mb-3" $ do
                    label ! class_ "col-sm-2 col-form-label" ! for "password" $ "Password"
                    div ! class_ "col-sm-10" $ do
                        input ! type_ "password" ! class_ "form-control" ! id "password" ! required "required"
                div ! class_ "row mb-3" $ do
                    label ! class_ "col-sm-2 col-form-label" ! for "confirm-password" $ "Confirm password"
                    div ! class_ "col-sm-10" $ do
                        input ! type_ "password" ! class_ "form-control" ! id "confirm-password" ! required "required"
                button ! class_ "btn btn-primary" ! type_ "submit" $ "Save"
