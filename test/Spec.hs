import Relude

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import qualified Network.Wreq as Wreq
import Control.Lens

import App
import Control.Concurrent (forkIO)

main :: IO ()
main = do
    _ <- forkIO run
    specs <- concat <$> mapM testSpecs [spec_hello_world]
    defaultMain $ testGroup "Tests" [testGroup "Specs" specs]

spec_hello_world :: Spec
spec_hello_world =
    describe "Homepage" $ do
        r <- runIO $ Wreq.get "http://127.0.0.1:3000/"
        it "simple GET returns 200" $ do
            r ^. Wreq.responseStatus . Wreq.statusCode `shouldBe` 200
        it "Content-Type is html" $
            r ^. Wreq.responseHeader "Content-Type" `shouldBe` "text/html; charset=utf-8"
