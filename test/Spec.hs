import Test.Hspec
import Lib
import Data
import Data.Maybe

game = makeGame grid languages

grid' = gameGrid game

gwc = gridWithCoords grid

testFindWord word = 
    let (Just result) = findWord gwc word
        string = map cell2char result
    in string `shouldBe` word

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "Should Concat every line with a newline" $ do
      (formatGrid (gridWithCoords ["abc","def","ghi"])) `shouldBe` "abc\ndef\nghi\n"
  
  describe "findWord" $ do
    it "Should find words that exist on the grid" $ do
      testFindWord "HASKELL"
      testFindWord "PERL"
    it "Should not find words that do not exist on the grid" $ do
      findWord gwc "HAMSTER" `shouldBe` Nothing

  describe "findWords" $ do
    it "Should find all the words that exist on the grid" $ do
      let found = findWords gwc languages
          asString = map (map cell2char) found
      asString `shouldBe` languages
    it "Should not find words that do not exist on the grid" $ do
      findWords gwc ["FRENCH","GERMAN"] `shouldBe` []


