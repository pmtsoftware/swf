import Relude

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import qualified Network.Wreq as Wreq
import Control.Lens

import App
import Config

import Control.Concurrent (forkIO)

main :: IO ()
main = do
    config <- loadAppConfig
    m <- newEmptyMVar
    let appUp = putMVar m ()
    _ <- forkIO $ startWithConfig appUp config
    _ <- takeMVar m
    specs <- concat <$> mapM testSpecs [spec_hello_world config]
    defaultMain $ testGroup "Tests" [testGroup "Specs" specs]

spec_hello_world :: AppConfig -> Spec
spec_hello_world AppConfig{..} = do
    let rootUrl = "http://" <> appHost <> ":" <> show appPort <> "/"
    describe "Homepage" $ do
        r <- runIO $ Wreq.get (toString rootUrl)
        it "simple GET returns 200" $ do
            r ^. Wreq.responseStatus . Wreq.statusCode `shouldBe` 200
        it "Content-Type is html" $
            r ^. Wreq.responseHeader "Content-Type" `shouldBe` "text/html; charset=utf-8"
