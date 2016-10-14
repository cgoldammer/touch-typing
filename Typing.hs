{-# LANGUAGE AllowAmbiguousTypes, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, GADTs, ConstraintKinds, TemplateHaskell, OverloadedStrings, FlexibleInstances, ExtendedDefaultRules, DeriveGeneric #-}


import Prelude hiding (mapM, mapM_, all, sequence)
import Data.FileEmbed
import Data.Tuple
import qualified Data.ByteString as BS
import Reflex
import Reflex.Dom
import Control.Monad
import Control.Lens
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
import GHCJS.DOM.Element hiding (drop)
import Data.Monoid ((<>))
import Data.Aeson
import GHC.Generics


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

displayMainStateBool :: MonadWidget t m => Bool -> MainState -> m ()
displayMainStateBool True ms = displayMainState ms
displayMainStateBool False _ = text "not showing"


displayMainState :: MonadWidget t m => MainState -> m ()
displayMainState (MainState ctn ct cts cp t) = do
  elClass "div" "banner" $ do
    elClass "h1" "banner-head" $ do displayStates ct cts
  return ()


hiddenDivAttr = (Map.fromList [("id", "h"), ("class", "overlay"), ("tabindex", "1")]) 

nextButton :: MonadWidget t m => T.Text -> m (Event t ())
nextButton t = do
  next <- elClass "div" "pure-u-1 pure-u-md-1-2" $ do
    (b, _) <- elAttr' "button" (Map.fromList [("class", "pure-button button-next")]) $ text "next"
    return $ domEvent Click b
  return next

nextButtons :: MonadWidget t m => MainState -> m (Event t NextEvent)
nextButtons (MainState ctn ct cts True _) = elClass "div" "l-content" $ do
  (n, r) <- elClass "div" "next-tables pure-g" $ do
    next <- nextButton "next"
    repeat <- nextButton "repeat"
    return (next, repeat)
  return $ leftmost [fmap (const Next) n, fmap (const Repeat) r]
nextButtons _ = do
  blank
  return never

performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (IOClass.liftIO . f) x)

charToNext ' ' = Next
charToNext '\r' = Repeat

getUserReq = xhrRequest "GET" "/snap/api/levels/user" def {_xhrRequestConfig_withCredentials = True}
getSummaries = xhrRequest "GET" "/snap/api/levels" def {_xhrRequestConfig_withCredentials = True}
logoutReq = xhrRequest "GET" "/snap/logout" def {_xhrRequestConfig_withCredentials = True}

data LevelSummaryResponse = LevelSummaryResponse {time :: String, textS :: String, levelTimeS :: Int, userIdS :: Int, numberCorrectS :: Int, idS :: Int, levelIdS :: Int} deriving (Show)

-- instance ToJSON LevelSummaryResponse
instance FromJSON LevelSummaryResponse where
  parseJSON (Object v) = LevelSummaryResponse <$> 
                          v.: "time" <*>
                          v.: "text" <*>
                          v.: "levelTime" <*>
                          v.: "userId" <*>
                          v.: "numberCorrect" <*>
                          v.: "id" <*>
                          v.: "levelId"

data L = L {aaa :: Int} deriving (Show)
instance FromJSON L where
  parseJSON (Object v) = L <$> v.: "id"
  
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

data ActiveTab = Typing | Summary deriving Show

data ConnectType = Register | Login | Logout | CloseWindow | GoToTyping | GoToSummary | Failure deriving (Eq, Show)
data DisplayState = DisplayState {
    userId :: Maybe T.Text,
    connectType :: ConnectType,
    open :: Bool,
    activeTab :: ActiveTab} deriving Show

displayUpdateType (DisplayState u t o a) t' = DisplayState u t' True a
displayUpdateUser (DisplayState u t o a) u' = DisplayState u' t False a
displayUpdateFromLoginForm t' u' st@(DisplayState u t o a)
  | t'==Login || t'==Register = DisplayState u' t' False a
  | t'==CloseWindow = DisplayState u t' False a
  | t'==Logout = startingDisplayState
  | t'==Failure = st

startingDisplayState = DisplayState Nothing Register False Typing

menuLink :: MonadWidget t m => T.Text -> m (Event t ())
menuLink t = do
    (e, _) <- elClass (T.pack "li") (T.pack "pure-menu-item") $ do
      elClass' "div" "pure-menu-link" $ do
        elAttr "a" (Map.fromList [("class", "pure-menu-link")]) $ do
          text t
    return $ domEvent Click e

userGreeting :: Maybe User -> T.Text
userGreeting Nothing = T.pack ""
userGreeting (Just u) = T.concat [T.pack "Welcome, ", u, T.pack "!"]

menu :: MonadWidget t m => [(T.Text, ConnectType)] -> Dynamic t (Maybe User) -> m (Event t ConnectType)
menu ls user = do
    events :: [Event t ()] <- elClass (T.pack "div") (T.pack "pure-menu pure-menu-horizontal") $ do
      elClass (T.pack "ul") (T.pack "pure-menu-list") $ do  
        elClass "li" "pure-menu-heading" $ do
            text "Touch typist"
        typingLink <- menuLink "Typing"
        summaryLink <- menuLink "Summary"
        ev <- mapM menuLink $ fmap fst ls
        dynText $ fmap userGreeting user
        return $ [typingLink, summaryLink] ++ ev
    let connectEvents = [GoToTyping, GoToSummary] ++ fmap snd ls
    let menuEv = fmap (\(a, b) -> fmap (const b) a) $ zip events connectEvents
    return $ leftmost menuEv
        

connect :: MonadWidget t m => DisplayState -> m (Event t DisplayState)
connect st@(DisplayState user connectType open activeTab) = el (T.pack "div") $ do
    loginFormEvent <- if open
        then do
            e <- loginForm connectType
            return e
        else do
            return never
    let userEvent = fmap snd loginFormEvent
    userDyn <- holdDyn Nothing userEvent

    menuEvent <- if isJust user
      then menu [("logout", Logout)] (constDyn user)
      else menu [("login", Login), ("register", Register)] (constDyn user)

    let menuEventResult = fmap (displayUpdateType st) menuEvent
    let loginFormResult = fmap (\(a, b) -> displayUpdateFromLoginForm a b st) loginFormEvent

    return $ leftmost [loginFormResult, menuEventResult]
    -- let getUserReqEvent = fmap (const getUserReq) e
    -- getResponse :: Event t XhrResponse <- performRequestAsync getUserReqEvent
    -- getText :: Dynamic t T.Text <- holdDyn (T.pack "") $ fmap getBody getResponse
    -- dynText getText
    -- let userEvent = updated $ fmap userCleaner getText
    -- let connectEventUser = fmap (displayUpdateUser st) userEvent -- Event t DisplayState
    -- let connectEventRegister = fmap (displayUpdateType st) $ leftmost [menuEvent, closeEvent] -- Event t ConnectEvent
    -- let connectEvent = leftmost [connectEventUser, connectEventRegister]
    -- return connectEvent

type X = XhrRequest T.Text

pButton :: MonadWidget t m => T.Text -> m (Event t T.Text)
pButton t = do
    (b, _) <- elClass' "button" "button pure-button" $ do
        text t
    return $ fmap (const t) $ domEvent Click b


loginFields :: MonadWidget t m => m ((Dynamic t T.Text, Dynamic t T.Text, Event t T.Text, Event t ConnectType))
loginFields = do
                name <- textInput $ def & attributes .~ constDyn ("placeholder" =: "email")
                let defPassword = set textInputConfig_inputType "password" def
                password <- textInput $ defPassword & attributes .~ constDyn ("placeholder" =: "password")
                b <- pButton "ok to login"
                closeButton <- pButton "close window"
                return ((_textInput_value name), (_textInput_value password), b, fmap (const CloseWindow) closeButton)
                
type User = T.Text

-- Event t (ConnectType, Maybe User)
-- A loginform returns: What was clicked, and whether a user resulted from it.
loginForm :: MonadWidget t m => ConnectType -> m (Event t (ConnectType, Maybe User))
loginForm Register = elAttr "div" ("class" =: "modal") $ do
    elAttr "div" ("class" =: "modalDialog") $ do
        elClass "div" "pure-form" $ do
            el "fieldset" $ do
              (name, password, b, closeEvent) <- loginFields
              let bothDyns = zipDynWith (,) name password
              let reqString = fmap addInfo bothDyns
              let reqStringButton = tagPromptlyDyn reqString b -- Event t Text 
              let registerPost = fmap (postForm (T.pack "/snap/register")) reqStringButton -- Event t XhrRequest
              e :: Event t XhrResponse <- performRequestAsync registerPost
              userRegisterResponseText :: Dynamic t T.Text <- holdDyn (T.pack "Log") $ fmap getBody e
              dynText userRegisterResponseText
              let registerSuccessEvent  = fmap (getEventGivenUser . getBody) e -- Event t Bool
              let loginClickEvent = tagPromptlyDyn (fmap (postForm (T.pack "/snap/login")) reqString) (ffilter (==True) registerSuccessEvent)
              e :: Event t XhrResponse <- performRequestAsync ((fmap . fmap) T.unpack loginClickEvent)
              userLoginResponseText :: Dynamic t T.Text <- holdDyn (T.pack "Log") $ fmap getBody e
              let loginSuccessEvent  = fmap (getEventGivenUser . getBody) e -- Event t Bool
              let getUserReqEvent = fmap (const getUserReq) (ffilter (==True) loginSuccessEvent)
              getResponse :: Event t XhrResponse <- performRequestAsync getUserReqEvent
              getText :: Dynamic t (Maybe User) <- holdDyn (Just (T.pack "nouser")) $ fmap (parseUserBody . getBody) getResponse -- Dynamic t (Maybe User)
              let getTextSucceeded = ffilter isJust (updated getText)
              dynText $ fmap (maybe "User response" id) getText -- Printing the username
              let connectEvent = leftmost [fmap (const Login) getTextSucceeded, closeEvent]
              return $ fmap swap $ attachPromptlyDyn getText connectEvent
loginForm Login = elAttr "div" ("class" =: "modal") $ do
    elAttr "div" ("class" =: "modalDialog") $ do
        elClass "div" "pure-form" $ do
            el "fieldset" $ do
              (name, password, b, closeEvent) <- loginFields
              let bothDyns = zipDynWith (,) name password
              let reqString = fmap addInfo bothDyns
              let loginClickEvent = tagPromptlyDyn (fmap (postForm (T.pack "/snap/login")) reqString) b
              e :: Event t XhrResponse <- performRequestAsync ((fmap . fmap) T.unpack loginClickEvent)
              userLoginResponseText :: Dynamic t T.Text <- holdDyn (T.pack "Log") $ fmap getBody e
              dynText userLoginResponseText
              let loginSuccessEvent  = fmap (getEventGivenUser . getBody) e -- Event t Bool
              let getUserReqEvent = fmap (const getUserReq) (ffilter (==True) loginSuccessEvent)
              getResponse :: Event t XhrResponse <- performRequestAsync getUserReqEvent
              getText :: Dynamic t (Maybe User) <- holdDyn (Just (T.pack "nouser")) $ fmap (parseUserBody . getBody) getResponse -- Dynamic t (Maybe User)
              let getTextSucceeded = ffilter isJust (updated getText)
              dynText $ fmap (maybe "User response" id) getText -- Printing the username
              let connectEvent = leftmost [fmap (const Login) getTextSucceeded, closeEvent]
              return $ fmap swap $ attachPromptlyDyn getText connectEvent
loginForm GoToSummary = do
  return never
loginForm GoToTyping = do
  return never
loginForm Logout = do
  pb <- getPostBuild
  ev <- performRequestAsync $ fmap (const logoutReq) pb
  return $ fmap (const (Logout, Nothing)) ev

parseUserBody :: T.Text -> Maybe User
parseUserBody t
  | T.length t == 0 = Nothing
  | otherwise = Just t

-- Todo:
-- Login:
-- - validation for email
-- - login:
--   - wrong password
--   - username does not exist
-- - register: username already exists
-- - remember user (note: loginUser arguments in Snap auth)
-- https
-- submit data using JSON

getEventGivenUser x
  | (T.length x) < 5 = True
  | otherwise = False

typingWidget :: MonadWidget t m => Dynamic t DisplayState -> Event t Char -> m ()
typingWidget displayState allCharEvent = do
  let charEvent = ffilter ((flip elem) ['a'..'z']) allCharEvent
  let charTransformer = fmap (flip updateMainStateWithChar) charEvent
  divClass <- holdDyn (T.pack "half") (fmap (const (T.pack "")) charTransformer)

  rec currentState :: Dynamic t MainState <- foldDyn ($) startingMainState $ mergeWith (.) [charTransformer, nextTransformer, clockTransformer]
      dyn $ fmap displayMainState currentState
      nextSimple :: Behavior t (Event t NextEvent) <- hold never nextEv
      let nextCharEvent = fmap charToNext $ ffilter (\a -> a==' ' || a=='\r') allCharEvent
      let nextEvent = leftmost [nextCharEvent, switch nextSimple]
      let nextTransformer = fmap (flip updateMainStateWithNext) nextEvent

      let event = leftmost [fmap (const NewEntered) nextTransformer, fmap (const CharEntered) charTransformer] -- Event t EventType
      dIsFirst :: Dynamic t (Bool, Bool) <- foldDyn isFirstChar (True, False) event

      let eIsFirst = ffilter hasStarted (updated dIsFirst)

      let eHasNotCompleted = attachPromptlyDynWith hasNotCompleted currentState charTransformer
      let eHasCompleted = attachPromptlyDynWith hasCompleted currentState charTransformer
      let eHasStarted = fmap fst $ ffilter fst (updated dIsFirst)

      notLevelEnd :: Behavior t Bool <- hold False eHasNotCompleted
      let dynNew2 = dynNew notLevelEnd eHasStarted
      let d = postForm (T.pack "/snap/api/levels") -- T.Text -> XhRRequest
      let sf ms = summary (currentText ms) ((numberCorrect . levelSummary) ms) (timeElapsed ms)
      let dLevelS = fmap (d . sf) currentState -- Dynamic t XhrRequest
      let lastCharTransformer = gate (current (fmap lastEntered currentState)) charTransformer
      let sendPost = tagPromptlyDyn dLevelS lastCharTransformer
      registerPostEvent :: Event t XhrResponse <- performRequestAsync sendPost

      dNew :: Dynamic t (m (Dynamic t Integer)) <- foldDyn (\a b -> dynNew2) dynNew2 eIsFirst
      clockTime :: Event t (Dynamic t Integer) <- dyn dNew
      dyndyn :: Dynamic t (Dynamic t Integer) <- holdDyn (constDyn 0) clockTime
      let clockTransformer = fmap (flip updateMainStateWithClock) (updated (join dyndyn)) -- Event t (MainState -> MainState)
      dyn $ fmap (displayLevelSummary . levelSummary) currentState

      nextEv :: Event t (Event t NextEvent) <- dyn $ fmap nextButtons currentState
      dynText $ fmap (T.pack . show) displayState
      dynText $ fmap (T.pack . show) currentState
  return ()
  -- void $ performArg (const $ Element.focus (_element_raw keyListenerDiv)) $ nextTransformer

typing :: MonadWidget t m => m ()
typing = do
  (keyListenerDiv, _) <- elAttr' (T.pack "div") hiddenDivAttr $ do text ""
  let allCharEvent = fmap chr $ domEvent Keypress keyListenerDiv -- Event t Char

  rec displayState :: Dynamic t DisplayState <- foldDyn ($) startingDisplayState $ mergeWith (.) [userTransformer]
      w :: Event t (Event t DisplayState) <- dyn $ fmap connect displayState
      w2 :: Behavior t (Event t DisplayState) <- hold never w
      let connectEvent = switch w2 -- Event t DisplayState
      let userTransformer = fmap const connectEvent

  let getReqEvent = fmap (const getSummaries) $ ffilter (\a -> connectType a == GoToSummary) connectEvent
  getResponse :: Event t XhrResponse <- performRequestAsync getReqEvent
  let summ = (fmap decodeXhrResponse getResponse) :: Event t (Maybe [LevelSummaryResponse])
  let sum2 = fmap (T.pack . (maybe "nothing" show)) summ -- Event t T.Text
  let lsr = fmap (maybe [] id) summ -- Event t [LevelSummaryResponse]
  lsrD <- holdDyn [] lsr -- Dynamic t [LevelSummaryResponse]
  let sl = fmap (displayLSRL . take 5) lsrD
  void $ performArg (const $ Element.focus (_element_raw keyListenerDiv)) $ userTransformer
  let isSummary = fmap (\s -> connectType s == GoToSummary) displayState -- Dynamic t Bool
  let ww = fmap (widg displayState allCharEvent sl) isSummary
  dyn ww
  return ()

widg :: MonadWidget t m => Dynamic t DisplayState -> Event t Char -> Dynamic t (m ()) -> Bool -> m ()
widg ds ac sl True = do
  text "Summaries"
  dyn sl
  return ()
widg ds ac sl False = typingWidget ds ac

displayLSRD :: MonadWidget t m => Dynamic t LevelSummaryResponse -> m ()
displayLSRD dslr = do
  let mm = fmap displayLSR dslr
  dyn mm
  return ()

displayLSRL :: MonadWidget t m => [LevelSummaryResponse] -> m ()
displayLSRL dslr = do
  let fields = ["time", "level text", "time in seconds", "number correct", "share correct"]
  elClass "div" "pure-g" $ do
    mapM (cellHeader . T.pack) fields
  mapM displayLSR dslr
  return ()


cellType :: MonadWidget t m => Bool -> T.Text -> m ()
cellType header val = do 
  let s = if header then "header" else "cell"
  elClass "div" (T.pack ("pure-u-1-5 " ++ s)) $ do
    text val
  return ()

cell :: MonadWidget t m => T.Text -> m ()
cell = cellType False

cellHeader :: MonadWidget t m => T.Text -> m ()
cellHeader = cellType True

displayLSR :: MonadWidget t m => LevelSummaryResponse -> m ()
displayLSR lsr = do
  elClass "div" "pure-g" $ do
    let text = textS lsr
    let correct = numberCorrectS lsr
    cell $ T.pack $ time lsr
    cell $ T.pack text
    cell $ T.pack $ show $ levelTimeS lsr
    cell $ T.pack $ show correct
    cell $ T.pack $ show $ quot (correct * 100) (length text)
  return ()

lastEntered ms = length (List.filter (==ToEnter) $ fmap snd $ currentTextState ms) == 1

summary :: String -> Int -> Integer -> T.Text
summary t nc lt = T.pack $ "text=" ++ t ++ "&numberCorrect=" ++ (show nc) ++ "&levelTime=" ++ (show lt)

dynNew :: MonadWidget t m => Behavior t Bool -> Event t Bool -> m (Dynamic t Integer)
dynNew e hasStarted = do
  curTime <- IOClass.liftIO getCurrentTime
  tl :: Dynamic t TickInfo <- clockLossy aSecond curTime
  let tlTime = fmap (\a -> a + 1) $ fmap (_tickInfo_n) (updated tl) -- Event t Integer
  let tlTimeGated = gate e tlTime -- Event t Integer
  let tlTime2 = leftmost [fmap (const 0) hasStarted, tlTimeGated]
  clock :: Dynamic t Integer <- holdDyn 0 tlTime2
  return $ clock


hasNotCompleted :: MainState -> a -> Bool
hasNotCompleted ms _ = not $ completed ms

hasCompleted :: MainState -> a -> Bool
hasCompleted ms _ = completed ms

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
