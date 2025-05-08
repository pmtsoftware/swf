module Homepage (renderHomepage) where

import Relude hiding (head, id, div)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Text

renderHomepage :: LText
renderHomepage = renderHtml homepage

homepage :: Html
homepage = docTypeHtml $ do
    head $ do
        title "Hello world!!!"
        link ! href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.5/dist/css/bootstrap.min.css" ! rel "stylesheet"
    body $ do
        main $ do
            div ! class_ "container py-4" $ do
                div ! class_ "p-5 mb-4 bg-body-tertiary rounded-3" $ do
                    div ! class_ "container-fluid py-5" $ do
                        h1 ! class_ "display-5 fw-bold" $ "Hello world app"
                        p ! class_ "col-md-8 fs-4" $ "Welcome in our new web app"
                        button ! class_ "btn btn-primary btn-lg" $ "Example button"

