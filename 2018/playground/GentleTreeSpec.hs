import Test.Hspec
import GentleTree

main :: IO ()
main = hspec $ do
  describe "tree formatting" $ do
    it "formats nested tree correctly" $ do
      let tree = Branch (Branch (Leaf (1 :: Int)) (Leaf 2)) (Leaf 3)
          expectation = "<<1,2>,3>"
      showTree1 tree `shouldBe` expectation
      showTree2 tree `shouldBe` expectation
