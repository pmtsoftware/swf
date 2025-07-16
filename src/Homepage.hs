module Homepage (renderHomepage, layout) where

import Relude hiding (head, id, div)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Text

renderHomepage :: LText
renderHomepage = renderHtml homepage

homepage :: Html
homepage = layout $ do
    h1 "Hello world app"
    p "Welcome in our new web app when live reloading almost works!"
    button "Example button"

-- navbar :: Html
-- navbar = nav ! class_ "navbar navbar-expand-lg bg-body-tertiary" $ do
--     div ! class_ "container-fluid" $ do
--         a ! class_ "navbar-brand" ! href "/" $ "SWF"
--         ul ! class_ "navbar-nav me-auto mb-2 mb-lg-0" $ do
--             li ! class_ "nav-item" $ a ! class_ "nav-link" ! href "/users" $ "Użytkownicy"


layout :: Html -> Html
layout innerHtml = docTypeHtml ! dataAttribute "bs-theme" "dark" $ do
    head $ do
        meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        title "Hello world!!!"
        link ! rel "icon" ! href "/static/favicon.ico" ! type_ "image/x-icon"
        link ! href "https://cdn.jsdelivr.net/npm/water.css@2/out/dark.min.css" ! rel "stylesheet"
        -- link ! href "https://cdn.jsdelivr.net/npm/sakura.css/css/sakura-dark.css" ! rel "stylesheet"
        link ! href "/static/swf.css" ! rel "stylesheet"
        script ! type_ "module" ! src "/static/dev.js" $ mempty
        script ! src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js" $ mempty
    body ! htmxBoost $ do
        main $ do
            -- navbar
            innerHtml

htmxBoost :: Attribute
htmxBoost = customAttribute "hx-boost" "true"
