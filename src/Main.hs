module Main where

import           Data.List  (intercalate, intersperse, sort)
import           Test.Hspec (describe, hspec, it, shouldBe)

scrambled :: [(Int, String)]
scrambled = [(26,"as"), (20,"because"), (2,"cannot"), (24,"details"),
    (29,"does"), (1,"i"), (23,"idle"), (7,"in"), (21,"it"), (25,"just"),
    (28,"memory"), (12,"night"), (17,"night"), (10,"of"), (27,"our"),
    (18,"pleases"), (9,"solitude"), (6,"suburbs"), (22,"suppresses"),
    (15,"that"), (5,"the"), (8,"the"), (11,"the"), (16,"the"),
    (14,"thinking"), (4,"through"), (19,"us"), (3,"walk"), (13,"without")]

-- Define this function
-- Hint: you may need some of those Data.List functions
-- and you may also want some functions from the Prelude
unscramble :: [(Int, String)] -> String
unscramble xs = ""

-- | Here are some tests that a valid solution will pass
main :: IO ()
main =
    let overSubstrings f = sum (map (f . snd) scrambled)
        actual = unscramble scrambled
    in  hspec $ do
            describe "Length of solution" $
                it "should be total length of substrings plus spaces separating words" $ do
                    let spaces        = length scrambled - 1
                        lenSubstrings = overSubstrings length
                    length actual `shouldBe` lenSubstrings + spaces

            describe "Starting character" $
                it "should be \"i\" (by inspection)" $
                    head actual `shouldBe` 'i'

            describe "Conservation text property" $
                it "should keep the same number of letter 'e's" $ do
                    let countEs = length . filter (== 'e')
                    countEs actual `shouldBe` overSubstrings countEs