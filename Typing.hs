{-# LANGUAGE AllowAmbiguousTypes, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, GADTs, ConstraintKinds, TemplateHaskell, OverloadedStrings, FlexibleInstances, ExtendedDefaultRules #-}


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
import System.Random
import System.IO.Unsafe
import qualified Data.Text as T
import Data.Maybe

default (T.Text)

main :: IO ()
main = mainWidgetWithCss cssCombined typing

cssCombined = BS.concat [$(embedFile "css/pure-min.css"), $(embedFile "css/grids-responsive-min.css"), $(embedFile "css/typing.css")]

texts :: [String]
texts = [take 10 $ randomRs ('a', 'z') $ unsafePerformIO newStdGen | i <- [0..]::[Int]]

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

stateCss :: LetterType -> T.Text
stateCss Correct = "correct"
stateCss Incorrect = "incorrect"
stateCss ToEnter = "to-enter"

data NextEvent = Next | Repeat

data MainState = MainState
  { currentTextNumber :: Int
  , currentText :: String
  , currentTextState :: DisplayString
  , completed :: Bool 
  , timeElapsed :: Integer}
  deriving Show

numberLettersEntered :: DisplayString -> Int
numberLettersEntered ds = maybe (length ds) id maybePosition
  where maybePosition = List.findIndex (==ToEnter) dt
        dt = fmap snd ds


updateMainStateWithChar :: MainState -> Char -> MainState
updateMainStateWithChar (MainState ctn ct cts cp t) c = MainState ctn ct (updateDisplayString cts c) completed t
  where numberEntered = numberLettersEntered cts
        completed = numberEntered >= (length ct) - 1

updateMainStateWithNext :: MainState -> NextEvent -> MainState
updateMainStateWithNext (MainState ctn ct cts cp t) Next = MainState nextCount nextText (initialDisplayString nextText) False t
  where nextCount = ctn + 1
        nextText = texts !! nextCount
updateMainStateWithNext (MainState ctn ct cts cp t) Repeat = MainState ctn ct (initialDisplayString ct) False t

updateMainStateWithClock (MainState ctn ct cts cp t) t2 = MainState ctn ct cts cp t2

initialDisplayString :: String -> DisplayString
initialDisplayString s = zip s (repeat ToEnter)

startingMainState = MainState 0 newText (initialDisplayString newText) False 0
  where newText = texts !! 0

displayState :: MonadWidget t m => DisplayLetter -> m ()
displayState (ch, ls) = elAttr (T.pack "span") ((T.pack "class") =: (stateCss ls)) $ do
  text $ T.pack [ch]
  return ()

displayStates :: MonadWidget t m => String -> DisplayString -> m ()
displayStates correct entered = sequence (fmap displayState (zip correct (fmap snd entered))) >> return ()

displayMainState :: MonadWidget t m => MainState -> m ()
displayMainState (MainState ctn ct cts cp t) = do
  elClass "div" "banner" $ do
    elClass "h1" "banner-head" $ do displayStates ct cts
  return ()


hiddenDivAttr = (Map.fromList [("id", "h"), ("class", "overlay"), ("tabindex", "1")]) 

nextButtons :: MonadWidget t m => MainState -> m (Event t NextEvent)
nextButtons (MainState ctn ct cts True _) = elClass "div" "l-content" $ do
  (n, r) <- elClass "div" "next-tables pure-g" $ do
    next <- elClass "div" "pure-u-1 pure-u-md-1-2" $ do
      (b, _) <- elAttr' "button" (Map.fromList [("class", "pure-button button-next")]) $ text "next"
      return $ domEvent Click b
    repeat <- elClass "div" "pure-u-1 pure-u-md-1-2" $ do
      (b, _) <- elAttr' "button" (Map.fromList [("class", "pure-button button-next")]) $ text "repeat"
      return $ domEvent Click b
    return (next, repeat)

  -- displayCounter $ errCounter (fmap fst cts) ct
  return $ leftmost [fmap (const Next) n, fmap (const Repeat) r]
nextButtons _ = do
  blank
  return never

performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (IOClass.liftIO . f) x)

charToNext ' ' = Next
charToNext '\r' = Repeat



getReq = xhrRequest "GET" "/snap/api/levels/user" def {_xhrRequestConfig_withCredentials = True}
logoutReq = xhrRequest "GET" "/snap/logout" def {_xhrRequestConfig_withCredentials = True}

getBody :: XhrResponse -> T.Text
getBody x = maybe (T.pack "") id (_xhrResponse_responseText x)


addInfo :: (T.Text, T.Text) -> T.Text
addInfo (a, b) = T.pack $ "login=" ++ (T.unpack a) ++ "&password=" ++ (T.unpack b)

-- display  n
-- header :: MainState -> MonadWidget t m => m ()
-- if not logged in, show newUser and login buttons
-- if logged in, show logout button
postData :: String -> T.Text -> T.Text -> XhrRequest T.Text
postData contentType  url text =
    XhrRequest "POST" url $ def { _xhrRequestConfig_headers = (T.pack "Content-type") =: (T.pack contentType)
                                , _xhrRequestConfig_sendData = text} 

postForm :: T.Text -> T.Text -> XhrRequest T.Text
postForm = postData "application/x-www-form-urlencoded"

toHint a = fromList [((T.pack "class"), T.pack ("hint " ++ a))]

userCleaner :: T.Text -> Maybe T.Text
userCleaner x
  | x == T.pack "" = Nothing
  | otherwise = Just x


buttonConfig :: ConnectType -> (T.Text, T.Text)
buttonConfig Login = (T.pack "ok to log in", T.pack "/snap/login")
buttonConfig _ = (T.pack "ok to register", T.pack "/snap/register")

data ConnectType = Register | Login | Logout deriving (Eq, Show)
data DisplayState = DisplayState {userId :: Maybe T.Text, connectType :: ConnectType} deriving Show
displayUpdateType (DisplayState _ _) Logout = startingDisplayState
displayUpdateType (DisplayState u t) t' = DisplayState u t'
displayUpdateUser (DisplayState u t) u' = DisplayState u' t
startingDisplayState = DisplayState Nothing Register


connect :: MonadWidget t m => DisplayState -> m (Event t DisplayState)
connect st@(DisplayState Nothing connectType) = el (T.pack "div") $ do
    bLogin <- button (T.pack "log in")
    bRegister <- button (T.pack "register")
    let registerEvent = fmap (const Register) bRegister
    let loginEvent = fmap (const Login) bLogin
    postReqEvent <- loginForm connectType
    e :: Event t XhrResponse <- performRequestAsync ((fmap . fmap) T.unpack postReqEvent)
    let getReqEvent = fmap (const getReq) e
    getResponse :: Event t XhrResponse <- performRequestAsync getReqEvent
    getText :: Dynamic t T.Text <- holdDyn (T.pack "not sent off yet") $ fmap getBody getResponse
    dynText getText
    let userEvent = updated $ fmap userCleaner getText
    let connectEventUser = fmap (displayUpdateUser st) userEvent -- Event t DisplayState
    let connectEventRegister = fmap (displayUpdateType st) $ leftmost [registerEvent, loginEvent] -- Event t ConnectEvent
    let connectEvent = leftmost [connectEventUser, connectEventRegister]
    return connectEvent
connect (DisplayState (Just x) _) = do
    logout <- button (T.pack "log out")
    ev <- performRequestAsync $ fmap (const logoutReq) logout
    return $ fmap (const startingDisplayState) ev


type X = XhrRequest T.Text

loginForm :: forall t m. MonadWidget t m => ConnectType -> m (Event t X)
loginForm Register = el (T.pack "div") $ do
    name <- textInput def
    password <- textInput def
    let bothDyns = zipDynWith (,) (_textInput_value name) (_textInput_value password)
    let reqString = fmap addInfo bothDyns
    b <- button $ T.pack "ok to register"
    let reqStringButton = tagPromptlyDyn reqString b -- Event t Text 
    let registerPost = fmap (postForm (T.pack "/snap/register")) reqStringButton -- Event t XhrRequest
    registerPostEvent :: Event t XhrResponse <- performRequestAsync registerPost
    let registerLogin = tagPromptlyDyn (fmap (postForm (T.pack "/snap/login")) reqString) registerPostEvent
    return registerLogin

loginForm Login = el (T.pack "div") $ do
    name <- textInput def
    password <- textInput def
    let bothDyns = zipDynWith (,) (_textInput_value name) (_textInput_value password)
    let reqString = fmap addInfo bothDyns
    
    b <- button $ T.pack "ok to login"
    return $ tagPromptlyDyn (fmap (postForm (T.pack "/snap/login")) reqString) b




typing :: MonadWidget t m => m ()
typing = do
  (keyListenerDiv, _) <- elAttr' (T.pack "div") hiddenDivAttr $ do text ""
  
  schedulePostBuild $ IOClass.liftIO $ Element.focus $ _el_element keyListenerDiv
  let allCharEvent = fmap chr $ domEvent Keypress keyListenerDiv -- Event t Char
  let charEvent = ffilter ((flip elem) ['a'..'z']) allCharEvent
  let charTransformer = fmap (flip updateMainStateWithChar) charEvent

  divClass <- holdDyn "half" (fmap (const "nothing") charEvent)
  dynAtt <- mapDyn toHint divClass
  elDynAttr (T.pack "p") dynAtt $ text $ T.pack "Just type..."

  rec displayState :: Dynamic t DisplayState <- foldDyn ($) startingDisplayState $ mergeWith (.) [userTransformer]
      loginWidget :: Dynamic t (m (Event t DisplayState)) <- mapDyn connect displayState 
      w :: Event t (Event t DisplayState) <- dyn loginWidget
      w2 :: Behavior t (Event t DisplayState) <- hold never w
      let connectEvent = switch w2 -- Event t DisplayState
      let userTransformer = fmap const connectEvent

  dAsText :: Dynamic t T.Text <- mapDyn (T.pack . show) displayState
  dynText dAsText

  rec currentState :: Dynamic t MainState <- foldDyn ($) startingMainState $ mergeWith (.) [charTransformer, nextTransformer, clockTransformer]
      all :: Dynamic t (m ()) <- mapDyn displayMainState currentState
      dyn all
      nextSimple :: Behavior t (Event t NextEvent) <- hold never nextEv
      let nextCharEvent = fmap charToNext $ ffilter (\a -> a==' ' || a=='\r') allCharEvent
      let nextEvent = leftmost [nextCharEvent, switch nextSimple]
      let nextTransformer = fmap (flip updateMainStateWithNext) nextEvent

      let event = leftmost [fmap (const NewEntered) nextTransformer, fmap (const CharEntered) charTransformer] -- Event t EventType
      dIsFirst :: Dynamic t (Bool, Bool) <- foldDyn isFirstChar (True, False) event

      let eIsFirst = ffilter hasStarted (updated dIsFirst)

      let eHasCompleted = attachDynWith hasCompleted currentState charEvent
      let eHasStarted = fmap fst $ ffilter fst (updated dIsFirst)

      levelEndGate :: Behavior t Bool <- hold False eHasCompleted
      let dynNew2 = dynNew levelEndGate eHasStarted

      dNew :: Dynamic t (m (Dynamic t Integer)) <- foldDyn (\a b -> dynNew2) dynNew2 eIsFirst
      clockTime :: Event t (Dynamic t Integer) <- dyn dNew
      dyndyn :: Dynamic t (Dynamic t Integer) <- holdDyn (constDyn 0) clockTime
      let clockDyn = joinDyn dyndyn

      let clockTransformer = fmap (flip updateMainStateWithClock) (updated clockDyn) -- Event t (MainState -> MainState)

      dynSummary <- mapDyn levelSummary currentState
      summaryWidget <- mapDyn displayLevelSummary dynSummary
      dyn summaryWidget

      nextB <- mapDyn nextButtons currentState
      nextEv :: Event t (Event t NextEvent) <- dyn nextB

      stateAsText :: Dynamic t T.Text <- mapDyn (T.pack . show) currentState
      dynText stateAsText

      -- Upon complete level, push LevelSummary to db
      -- liftIO submitLevelSummary $ levelSummary 
  void $ performArg (const $ Element.focus (_el_element keyListenerDiv)) $ nextTransformer
  return ()


dynNew :: MonadWidget t m => Behavior t Bool -> Event t Bool -> m (Dynamic t Integer)
dynNew e hasStarted = do
  curTime <- IOClass.liftIO getCurrentTime
  tl :: Dynamic t TickInfo <- clockLossy aSecond curTime
  let tlTime = fmap (\a -> a + 1) $ fmap (_tickInfo_n) (updated tl) -- Event t Integer
  let tlTimeGated = gate e tlTime -- Event t Integer
  let tlTime2 = leftmost [fmap (const 0) hasStarted, tlTimeGated]
  clock :: Dynamic t Integer <- holdDyn 0 tlTime2
  -- el "p" $ do
  --   text "Time elapsed: "
  --   dynText clock
  return $ clock


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
  clock :: Dynamic t String <- holdDyn "0" tlTime
  dynText (fmap T.pack clock)
  return ()


data LevelSummary = LevelSummary {numberLetters :: Int, numberCorrect :: Int, levelTime :: Integer}
 
levelSummary :: MainState -> LevelSummary
levelSummary ms = LevelSummary numberLetters numberCorrect t
  where numberLetters = length (currentText ms)
        numberCorrect = length $ List.filter (==Correct) $ fmap snd (currentTextState ms)
        t = timeElapsed ms


column :: MonadWidget t m => String -> String -> m ()
column name value = elClass (T.pack "div") (T.pack "pure-u-1 pure-u-md-1-3") $ do 
      elClass (T.pack "div") (T.pack "typing-table-header") $ do
        el (T.pack "h2") $ text (T.pack name)
        elClass (T.pack "span") (T.pack "typing-table-type") $ do text (T.pack value)

displayLevelSummary :: MonadWidget t m => LevelSummary -> m ()
displayLevelSummary ls = elClass (T.pack "div") (T.pack "l-content") $ do
  elClass (T.pack "div") (T.pack "typing-tables pure-g") $ do
    column "Time" $ (show (levelTime ls)) ++ "s"
    column "Letters" $ show (numberLetters ls)
    column "Correct" $ show (numberCorrect ls)
  return ()

aSecond :: NominalDiffTime
aSecond = 1

type Count = Map.Map Char Int

displayCounter :: MonadWidget t m => Count -> m ()
displayCounter counter = do
  mapM textSpan (assocs counter)
  return ()

textSpan :: MonadWidget t m => (Char, Int) -> m ()
textSpan (letter, count) = elAttr "span" ("class" =: "none") $ do
  el (T.pack "p") $ text $ T.pack $ [letter] ++ ": " ++ show count
  return ()

ec :: Count -> String -> String -> Count
ec ct (eChar:eRest) (cChar:cRest)
  | eChar == cChar = ec ct eRest cRest
  | eChar /= cChar = ec (insertWith (+) eChar 1 ct) eRest cRest
ec ct s1 s2 = ct

errCounter = ec Map.empty
