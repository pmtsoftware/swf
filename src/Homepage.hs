module Homepage (renderHomepage, renderUsers) where

import Relude hiding (head, id, div)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Text

renderHomepage :: LText
renderHomepage = renderHtml homepage

renderUsers :: LText
renderUsers = renderHtml users

homepage :: Html
homepage = docTypeHtml ! dataAttribute "bs-theme" "dark" $ do
    head $ do
        title "Hello world!!!"
        link ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.5/dist/css/bootstrap.min.css" ! rel "stylesheet"
    body $ do
        main $ do
            navbar
            div ! class_ "container py-4" $ do
                div ! class_ "p-5 mb-4 bg-body-tertiary rounded-3" $ do
                    div ! class_ "container-fluid py-5" $ do
                        h1 ! class_ "display-5 fw-bold" $ "Hello world app"
                        p ! class_ "col-md-8 fs-4" $ "Welcome in our new web app"
                        button ! class_ "btn btn-primary btn-lg" $ "Example button"

navbar :: Html
navbar = nav ! class_ "navbar navbar-expand-lg bg-body-tertiary" $ do
    div ! class_ "container-fluid" $ do
        a ! class_ "navbar-brand" ! href "/" $ "SWF"
        ul ! class_ "navbar-nav me-auto mb-2 mb-lg-0" $ do
            li ! class_ "nav-item" $ a ! class_ "nav-link" ! href "/users" $ "Użytkownicy"

users :: Html
users = layout $ div ! class_ "p-5 mb-4 bg-body-tertiary rounded-3" $ do
    h1 "Users"
    button ! class_ "btn btn-primary mb-2" $ "Add new user"
    table ! class_ "table table-striped table-borderless" $ do
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

layout :: Html -> Html
layout innerHtml = docTypeHtml ! dataAttribute "bs-theme" "dark" $ do
    head $ do
        title "Hello world!!!"
        link ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.5/dist/css/bootstrap.min.css" ! rel "stylesheet"
    body $ do
        main $ do
            navbar
            div ! class_ "container py-4" $ innerHtml
