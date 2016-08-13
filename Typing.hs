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
import Data.Time.Clock

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
  return ()

hiddenDivAttr = (Map.fromList [("id", "h"), ("class", "overlay"), ("tabindex", "1")]) 

nextButtons :: MonadWidget t m => MainState -> m (Event t NextEvent)
nextButtons (MainState ctn ct cts True) = el "div" $ do
  next <- button "next"
  repeat <- button "repeat"
  silly <- button "silly"
  displayCounter $ errCounter (fmap fst cts) ct
  return $ leftmost [fmap (const Next) next, fmap (const Repeat) repeat]
nextButtons _ = do
  blank
  return never

performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (IOClass.liftIO . f) x)


typing :: MonadWidget t m => m ()
typing = do
  el "h1" $ text "Touch typing"
  (keyListenerDiv, _) <- elAttr' "div" hiddenDivAttr $ do text "hidden div"

  schedulePostBuild $ IOClass.liftIO $ Element.focus $ _el_element keyListenerDiv
  let charEvent = fmap chr $ domEvent Keypress keyListenerDiv 
  let charTransformer = fmap (flip updateMainStateWithChar) charEvent

  rec currentState :: Dynamic t MainState <- foldDyn ($) startingMainState $ mergeWith (.) [charTransformer, nextTransformer]
      all :: Dynamic t (m ()) <- mapDyn displayMainState currentState
      dyn all
      nextB <- mapDyn nextButtons currentState
      nextEv :: Event t (Event t NextEvent) <- dyn nextB
      nextSimple :: Behavior t (Event t NextEvent) <- hold never nextEv
      let nextTransformer = fmap (flip updateMainStateWithNext) (switch nextSimple)

      let event = leftmost [fmap (const NewEntered) nextTransformer, fmap (const CharEntered) charTransformer] -- Event t EventType
      dIsFirst :: Dynamic t (Bool, Bool) <- foldDyn isFirstChar (True, False) event

      let eIsFirst = ffilter hasStarted (updated dIsFirst)

      let eHasCompleted = attachDynWith hasCompleted currentState charEvent
      let eHasStarted = fmap fst $ ffilter fst (updated dIsFirst)

      levelEndGate :: Behavior t Bool <- hold False eHasCompleted
      let dynNew2 = dynNew levelEndGate eHasStarted

      dNew :: Dynamic t (m ()) <- foldDyn (\a b -> dynNew2) dynNew2 eIsFirst
      dyn dNew

  void $ performArg (const $ Element.focus (_el_element keyListenerDiv)) $ nextTransformer

  return ()

dynNew :: MonadWidget t m => Behavior t Bool -> Event t Bool -> m ()
dynNew e hasStarted = do
  curTime <- IOClass.liftIO getCurrentTime
  tl :: Dynamic t TickInfo <- clockLossy aSecond curTime
  let tlTime = fmap (show . _tickInfo_n) (updated tl) -- Event t String
  let tlTimeGated = gate e tlTime -- Event t String
  let tlTime2 = leftmost [fmap (const "0") hasStarted, tlTimeGated]
  clock <- holdDyn "0" tlTime2
  el "p" $ do
    text "Time elapsed: "
    dynText clock
  return ()

hasCompleted :: MainState -> a -> Bool
hasCompleted ms _ = not $ completed ms

hasStarted :: (Bool, Bool) -> Bool
hasStarted (_, True) = True
hasStarted _ = False

data TypeEvent = NewEntered | CharEntered

isFirstChar :: TypeEvent -> (Bool, Bool) -> (Bool, Bool)
isFirstChar CharEntered (True, _) = (False, True)
isFirstChar NewEntered (_, _) = (True, False)
isFirstChar CharEntered (_, _) = (False, False)

newLevel :: MonadWidget t m => Event t a -> m ()
newLevel e = do
  curTime <- IOClass.liftIO getCurrentTime
  tl :: Event t TickInfo <- tickLossyFrom aSecond curTime e
  let tlTime = fmap (show . _tickInfo_n) tl -- Event t String
  clock <- holdDyn "0" tlTime
  dynText clock
  return ()



data LevelSummary = LevelSummary {numberLetters :: Int, numberCorrect :: Int}
 
levelSummary :: Count -> String -> LevelSummary
levelSummary errors target = LevelSummary numberLetters numberCorrect
  where numberLetters = length target
        numberErrors =  Map.foldr (+) 0 errors
        numberCorrect = numberLetters - numberErrors


displayLevelSummary :: MonadWidget t m => LevelSummary -> m ()
displayLevelSummary ls = el "div" $ do
  el "p" $ text $ show (numberCorrect ls)
  el "p" $ text $ show (numberLetters ls)
  return ()

aSecond :: NominalDiffTime
aSecond = 0.1

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


