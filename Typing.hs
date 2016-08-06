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
import qualified Data.List as List
-- import Text.Regex.Base

main :: IO ()
main = mainWidgetWithCss cssCombined typing

cssCombined :: BS.ByteString
cssCombined = BS.concat [$(embedFile "css/pure-min.css"), $(embedFile "css/style.css")]

texts :: [String]
texts = [take i $ repeat 'a' | i <- [5..]]

correctPart :: String -> String -> [Bool]
correctPart target [] = []
correctPart [] (entered:rest) = False : (correctPart [] rest)
correctPart (a:as) (b:bs) = (a == b) : (correctPart as bs)

data LetterType = Correct | Incorrect | ToEnter deriving (Eq, Show)
type DisplayLetter = (Char, LetterType)
type DisplayString = [DisplayLetter]

correctToType :: Bool -> LetterType
correctToType False = Incorrect
correctToType True = Correct

updateDisplayString :: DisplayString -> Char -> DisplayString
updateDisplayString ds c 
  | notCompleted = left ++ [(c, correctToType (currentLetter == c))] ++ right
  | otherwise = ds
  where numberEntered = numberLettersEntered ds
        notCompleted = numberEntered < length ds
        left = take numberEntered ds
        right = drop (numberEntered + 1) ds
        currentLetter = fst (ds !! numberEntered)

letterTypes :: String -> String -> [LetterType]
letterTypes target entered = enteredCorrect ++ toEnter
    where enteredCorrect = fmap correctToType correct
          toEnter = take (length target - length entered) $ repeat ToEnter
          correct = correctPart target entered

charStates :: String -> String -> DisplayString
charStates t e = zip e (letterTypes t e)

stateCss :: LetterType -> String
stateCss Correct = "correct"
stateCss Incorrect = "incorrect"
stateCss ToEnter = "to-enter"

data MainState = MainState
  { currentTextNumber :: Int
  , currentText :: String
  , currentTextState :: DisplayString
  , completed :: Bool }
  deriving Show

numberLettersEntered :: DisplayString -> Int
numberLettersEntered ds = maybe (length ds) id maybePosition
  where maybePosition = List.findIndex (==ToEnter) dt
        dt = fmap snd ds

stateUpdater :: Char -> MainState -> MainState
stateUpdater c (MainState ctn ct cts cp)
  | numberEntered <= (length ct) - 1 = MainState ctn ct (updateDisplayString cts c) False
  | otherwise = MainState nextCount nextText (initialDisplayString nextText) False
  where nextText = texts !! nextCount
        nextCount = ctn + 1
        numberEntered = numberLettersEntered cts

initialDisplayString :: String -> DisplayString
initialDisplayString s = zip s (repeat ToEnter)

startingMainState = MainState 0 newText (initialDisplayString newText) False
  where newText = texts !! 0

displayState :: MonadWidget t m => DisplayLetter -> m ()
displayState (ch, ls) = elAttr "span" ("class" =: (stateCss ls)) $ do
  text $ [ch]
  return ()

displayStates :: MonadWidget t m => DisplayString -> m ()
displayStates s = sequence (fmap displayState s) >> return ()

displayMainState :: MonadWidget t m => MainState -> m ()
displayMainState (MainState ctn ct cts cp) = do
  el "p" $ do displayStates cts
  let enteredString = fmap fst cts
  let eCounter = errCounter enteredString ct
  displayCounter eCounter
  return ()

typing :: MonadWidget t m => m ()
typing = do
  el "h1" $ text "Touch typing"
  (keyListenerDiv, _) <- elAttr' "div" (Map.fromList [("id", "h"), ("class", "overlay"), ("tabindex", "1")]) $ text ""
  let stroke = domEvent Keypress keyListenerDiv
  let enteredChar = fmap chr stroke
  currentState :: Dynamic t MainState <- foldDyn stateUpdater startingMainState enteredChar
  -- dyn s
  csm <- mapDyn displayMainState currentState
  dyn csm
  return ()

type Count = Map.Map Char Int

displayCounter :: MonadWidget t m => Count -> m ()
displayCounter counter = do
  mapM textSpan (assocs counter)
  return ()

textSpan :: MonadWidget t m => (Char, Int) -> m ()
textSpan (letter, count) = elAttr "span" ("class" =: "none") $ do
  el "p" $ do
    text $ [letter] ++ ": " ++ show count
  return ()

ec :: Count -> String -> String -> Count
ec ct (eChar:eRest) (cChar:cRest)
  | eChar == cChar = ec ct eRest cRest
  | eChar /= cChar = ec (insertWith (+) eChar 1 ct) eRest cRest
ec ct s1 s2 = ct

errCounter = ec Map.empty
