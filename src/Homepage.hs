module Homepage (renderHomepage, layoutM) where

import Common

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Text

renderHomepage :: Bool -> ByteString -> LText
renderHomepage dev cssSha = renderHtml . layout dev cssSha $ do
    h1 "Hello world app"
    p "Welcome in our new web app when live reloading almost works!"
    button "Example button"
    a ! href "/webauthn/register" $ button "Register"

layout :: Bool -> ByteString -> Html -> Html
layout devMode cssChecksum innerHtml = docTypeHtml ! dataAttribute "bs-theme" "dark" $ do
    head $ do
        meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        title "Hello world!!!"
        link ! rel "icon" ! href "/static/favicon.ico" ! type_ "image/x-icon"
        link ! href "https://cdn.jsdelivr.net/npm/water.css@2/out/dark.min.css" ! rel "stylesheet"
        -- link ! href "https://cdn.jsdelivr.net/npm/sakura.css/css/sakura-dark.css" ! rel "stylesheet"
        link ! href ("/static/swf.css?checksum=" <> checksumAV) ! rel "stylesheet"
        when devMode $ script ! type_ "module" ! src "/static/dev.js" $ mempty
        script ! src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js" $ mempty
    body ! htmxBoost $ do
        main $ do
            -- navbar
            innerHtml
    where
        checksumAV = toValue . decodeUtf8 @Text $ cssChecksum

layoutM :: Handler (Html -> Html)
layoutM = do
    AppEnv{..} <- lift ask
    return $ layout dev cssChecksum


htmxBoost :: Attribute
htmxBoost = customAttribute "hx-boost" "true"
