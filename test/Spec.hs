import Relude

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Network.HTTP.Req

import App
import Config

import Control.Concurrent (forkIO)

main :: IO ()
main = do
    config <- loadTestConfig
    m <- newEmptyMVar
    let appUp = putMVar m ()
    _ <- forkIO $ startWithConfig appUp config
    _ <- takeMVar m
    specs <- concat <$> mapM testSpecs [spec_hello_world config]
    defaultMain $ testGroup "Tests" [testGroup "Specs" specs]

spec_hello_world :: AppConfig -> Spec
spec_hello_world AppConfig{..} = do
    let url = http appHost
        urlOpts = port appPort
    describe "Homepage" $ do
        r <- runIO $ runReq defaultHttpConfig $ req GET url NoReqBody bsResponse urlOpts
        it "simple GET returns 200" $ do
            responseStatusCode r `shouldBe` 200
        it "Content-Type is html" $
            responseHeader r "Content-Type" `shouldBe` Just "text/html; charset=utf-8"
