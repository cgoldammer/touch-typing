{-# LANGUAGE AllowAmbiguousTypes, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

import Prelude hiding (mapM, mapM_, all, sequence)
import Data.FileEmbed
import qualified Data.ByteString as BS
import Reflex
import Reflex.Dom
import Control.Monad
import Data.Map.Strict as Map
import Reflex.Dom.Widget.Basic 
import Data.Char (chr)
import qualified Data.List as List
import qualified GHCJS.DOM.Element as Element
import qualified Control.Monad.IO.Class as IOClass

main :: IO ()
main = mainWidgetWithCss cssCombined typing

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

data NextEvent = Next | Repeat

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

updateMainStateWithChar :: MainState -> Char -> MainState
updateMainStateWithChar (MainState ctn ct cts cp) c = MainState ctn ct (updateDisplayString cts c) completed
  where numberEntered = numberLettersEntered cts
        completed = numberEntered >= (length ct) - 1

updateMainStateWithNext :: MainState -> NextEvent -> MainState
updateMainStateWithNext (MainState ctn ct cts cp) Next = MainState nextCount nextText (initialDisplayString nextText) False
  where nextCount = ctn + 1
        nextText = texts !! nextCount
updateMainStateWithNext (MainState ctn ct cts cp) Repeat = MainState ctn ct (initialDisplayString ct) False

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

hiddenDivAttr = (Map.fromList [("id", "h"), ("class", "overlay"), ("tabindex", "1")]) 

nextButtons :: MonadWidget t m => MainState -> m (Event t NextEvent)
nextButtons (MainState _ _ _ True) = el "div" $ do
  next <- button "next"
  repeat <- button "repeat"
  return $ leftmost [fmap (const Next) next, fmap (const Repeat) repeat]
nextButtons _ = do
  blank
  return never

performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (IOClass.liftIO . f) x)

typing :: MonadWidget t m => m ()
typing = do
  el "h1" $ text "Touch typing"
  (keyListenerDiv, _) <- elAttr' "div" hiddenDivAttr $ do text ""

  schedulePostBuild $ IOClass.liftIO $ Element.focus $ _el_element keyListenerDiv
  let charEvent = fmap chr $ domEvent Keypress keyListenerDiv 
  let charTransformer = fmap (flip updateMainStateWithChar) charEvent

  rec currentState :: Dynamic t MainState <- foldDyn ($) startingMainState $ mergeWith (.) [charTransformer, nextTransformer]
      nextB <- mapDyn nextButtons currentState
      nextEv :: Event t (Event t NextEvent) <- dyn nextB
      nextSimple :: Behavior t (Event t NextEvent) <- hold never nextEv
      let nextTransformer = fmap (flip updateMainStateWithNext) (switch nextSimple)
      all :: Dynamic t (m ()) <- mapDyn displayMainState currentState
      dyn all
  void $ performArg (const $ Element.focus (_el_element keyListenerDiv)) $ nextTransformer

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

