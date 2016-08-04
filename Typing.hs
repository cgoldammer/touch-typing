{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

import Prelude hiding (mapM, mapM_, all, sequence)
import Data.FileEmbed
import qualified Data.ByteString as BS
import Reflex
import Reflex.Dom
import Control.Monad
-- import Data.Monoid ((<>))
import Data.Map.Strict as Map
import Reflex.Dom.Widget.Basic 
import Data.Char (chr)
-- import Text.Regex.Base

main :: IO ()
main = mainWidgetWithCss cssCombined typing

cssBootstrap :: BS.ByteString
cssBootstrap = $(embedFile "bootstrap.css")

cssStyle :: BS.ByteString
cssStyle = $(embedFile "style.css")

cssCombined :: BS.ByteString
cssCombined = BS.concat [cssBootstrap, cssStyle]

baseText :: String
baseText = "Copy this string"

correctPart :: String -> String -> [Bool]
correctPart target [] = []
correctPart [] (entered:rest) = False : (correctPart [] rest)
correctPart (a:as) (b:bs) = (a == b) : (correctPart as bs)

data LetterState = Correct | Incorrect | ToEnter deriving (Eq, Show)
type CharState = (Char, LetterState)

correctToState :: Bool -> LetterState
correctToState False = Incorrect
correctToState True = Correct


letterStates :: String -> String -> [LetterState]
letterStates target entered = enteredCorrect ++ toEnter
    where enteredCorrect = fmap correctToState correct
          toEnter = take (length target - length entered) $ repeat ToEnter
          correct = correctPart target entered

charStates :: String -> String -> [CharState]
charStates t e = zip e (letterStates t e)

stateCss :: LetterState -> String
stateCss Correct = "correct"
stateCss Incorrect = "incorrect"
stateCss ToEnter = "to-enter"

displayState :: MonadWidget t m => CharState -> m ()
displayState (ch, ls) = elAttr "span" ("class" =: (stateCss ls)) $ do
  text $ [ch]
  return ()


displayStates :: MonadWidget t m => [CharState] -> m ()
displayStates s = sequence (fmap displayState s) >> return ()

typing :: MonadWidget t m => m ()
typing = do
  el "h1" $ text "Touch typing"
  (keyListenerDiv, _) <- elAttr' "div" (Map.fromList [("id", "h"), ("class", "overlay"), ("tabindex", "1")]) $ text ""
  let stroke = domEvent Keypress keyListenerDiv
  dynAgg :: Dynamic t String <- foldDyn (\i s -> s ++ [(chr i)]) "" stroke
  el "p" $ do text baseText
  dynCharStates :: Dynamic t [CharState] <- mapDyn (charStates baseText) dynAgg
  dynM :: Dynamic t (m ()) <- mapDyn displayStates dynCharStates
  dyn dynM

  dynC <- dynCount dynAgg
  dynC2 <- mapDyn snd dynC
  dynList <- mapDyn assocs dynC2
  simpleList dynList countWidget
  return ()


frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

countWidget :: MonadWidget t m
         => Dynamic t (Char, Int)
         -> m ()
countWidget tuple = do
  el "p" $ do
    str <- mapDyn (\(c, i) -> show c ++ ": " ++ show i) tuple
    dynText str
    return ()
  return ()

dynCount :: MonadWidget t m => Dynamic t String -> m (Dynamic t StateWithCount)
dynCount textDyn = foldDyn updater initialStateWithCount (updated $ textDyn)

ts :: MonadWidget t m => StateWithCount -> m[()]
ts (current, state) = mapM textSpan (assocs state)

textSpan :: MonadWidget t m => (Char, Int) -> m ()
textSpan (letter, count) = elAttr "span" ("class" =: "none") $ do
  text $ (letter:[]) ++ show count
  return ()

type Count = Map.Map Char Int
type StateWithCount = (String, Count)

differentLetter :: Char -> Char -> Maybe Char
differentLetter a b
  | a /= b = Just a
  | otherwise = Nothing

addError :: Maybe Char -> Count -> Count
addError Nothing d = d
addError (Just a) d = insertWith (\_ n -> n + 1) a 1 d

-- Fold over errorCounter to get current state of errors
errorCounter :: StateWithCount -> String -> String -> StateWithCount
errorCounter (initial, counter) new correct
  | newLength > correctLength = (new, counterWithLast)
  | newLength == (length initial) + 1 = (new, counterWithDifferent)
  | otherwise = (new, counter)
  where newLength = (length new)
        atNewLength s = s !! (newLength-1)
        newLetter = atNewLength new
        correctLetter = atNewLength correct
        correctLength = length correct
        counterWithLast = addError (Just newLetter) counter
        counterWithDifferent = addError (differentLetter correctLetter newLetter) counter

initialStateWithCount :: StateWithCount
initialStateWithCount = ("", fromList [])

updater :: String -> StateWithCount -> StateWithCount
updater str s = errorCounter s str baseText

