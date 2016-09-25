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
import GHCJS.DOM.Element hiding (drop)
import Data.Monoid ((<>))


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

data ActiveTab = Typing | Summary deriving Show

data ConnectType = Register | Login | Logout | CloseWindow | GoToTyping | GoToSummary deriving (Eq, Show)
data DisplayState = DisplayState {
    userId :: Maybe T.Text,
    connectType :: ConnectType,
    open :: Bool,
    activeTab :: ActiveTab} deriving Show
displayUpdateType (DisplayState _ _ _ _) Logout = startingDisplayState
displayUpdateType (DisplayState _ _ _ _) CloseWindow = startingDisplayState
displayUpdateType (DisplayState u t o a) t' = DisplayState u t' True a
displayUpdateUser (DisplayState u t o a) u' = DisplayState u' t False a
startingDisplayState = DisplayState Nothing Register False Typing

menuLink :: MonadWidget t m => T.Text -> m (Event t ())
menuLink t = do
    (e, _) <- elClass (T.pack "li") (T.pack "pure-menu-item") $ do
      elClass "div" "pure-menu-link" $ do
        elAttr' "a" (Map.fromList [("class", "pure-menu-link")]) $ do
          text t
    return $ domEvent Click e


menu :: MonadWidget t m => [(T.Text, ConnectType)] -> m (Event t ConnectType)
menu ls = do
    events :: [Event t ()] <- elClass (T.pack "div") (T.pack "pure-menu pure-menu-horizontal") $ do
      elClass (T.pack "ul") (T.pack "pure-menu-list") $ do  
        elClass "li" "pure-menu-heading" $ do
            text "Touch typist"
        typingLink <- menuLink "Typing"
        summaryLink <- menuLink "Summary"
        ev <- mapM menuLink $ fmap fst ls
        return $ [typingLink, summaryLink] ++ ev
    let connectEvents = [GoToTyping, GoToSummary] ++ fmap snd ls
    let menuEv = fmap (\(a, b) -> fmap (const b) a) $ zip events connectEvents
    return $ leftmost menuEv
        

connect :: MonadWidget t m => DisplayState -> m (Event t DisplayState)
connect st@(DisplayState Nothing connectType open activeTab) = el (T.pack "div") $ do
    menuEvent <- menu [("login", Login), ("register", Register)]
    (closeEvent, e) <- if open
        then do
            (closeEvent, postReqEvent) <- loginForm connectType
            e :: Event t XhrResponse <- performRequestAsync ((fmap . fmap) T.unpack postReqEvent)
            return (closeEvent, e)
        else do
            return (never, never)
    let getReqEvent = fmap (const getReq) e
    getResponse :: Event t XhrResponse <- performRequestAsync getReqEvent
    getText :: Dynamic t T.Text <- holdDyn (T.pack "not sent off yet") $ fmap getBody getResponse
    dynText getText
    let userEvent = updated $ fmap userCleaner getText
    let connectEventUser = fmap (displayUpdateUser st) userEvent -- Event t DisplayState
    let connectEventRegister = fmap (displayUpdateType st) $ leftmost [menuEvent, closeEvent] -- Event t ConnectEvent
    let connectEvent = leftmost [connectEventUser, connectEventRegister]
    return connectEvent
connect (DisplayState (Just x) _ _ activeTab) = do
    logoutClick <- menu [("logout", Logout)]
    ev <- performRequestAsync $ fmap (const logoutReq) logoutClick
    return $ fmap (const startingDisplayState) ev

type X = XhrRequest T.Text

-- Todo: Register endpoint should return "username exists" if it does.
-- In that case, stay at the register screen.
pButton :: MonadWidget t m => T.Text -> m (Event t T.Text)
pButton t = do
    (b, _) <- elClass' "button" "button pure-button" $ do
        text t
    return $ fmap (const t) $ domEvent Click b


loginFields :: MonadWidget t m => m ((Dynamic t T.Text, Dynamic t T.Text, Event t T.Text, Event t ConnectType))
loginFields = do
    elAttr "div" ("class" =: "modalDialog") $ do
        elClass "div" "pure-form" $ do
            el "fieldset" $ do
                name <- textInput $ def & attributes .~ constDyn (mconcat ["type" =: "email", "placeholder" =: "email"])
                password <- textInput $ def & attributes .~ constDyn ("type" =: "password" <> "placeholder" =: "Password")
                b <- pButton "ok to login"
                closeButton <- pButton "close window"
                return ((_textInput_value name), (_textInput_value password), b, fmap (const CloseWindow) closeButton)
                
loginForm :: MonadWidget t m => ConnectType -> m ((Event t ConnectType, Event t X))
loginForm Register = elAttr "div" ("class" =: "modal") $ do
    (name, password, b, closeEvent) <- loginFields
    let bothDyns = zipDynWith (,) name password
    let reqString = fmap addInfo bothDyns
    let reqStringButton = tagPromptlyDyn reqString b -- Event t Text 
    let registerPost = fmap (postForm (T.pack "/snap/register")) reqStringButton -- Event t XhrRequest
    registerPostEvent :: Event t XhrResponse <- performRequestAsync registerPost
    let registerLogin = tagPromptlyDyn (fmap (postForm (T.pack "/snap/login")) reqString) registerPostEvent
    return (closeEvent, registerLogin)
loginForm Login = elAttr "div" ("class" =: "modal") $ do
    (name, password, b, closeEvent) <- loginFields
    let bothDyns = zipDynWith (,) name password
    let reqString = fmap addInfo bothDyns
    let loginEvent = tagPromptlyDyn (fmap (postForm (T.pack "/snap/login")) reqString) b
    return (closeEvent, loginEvent)
loginForm GoToSummary = do
  return (never, never)
loginForm GoToTyping = do
  return (never, never)


typing :: MonadWidget t m => m ()
typing = do
  (keyListenerDiv, _) <- elAttr' (T.pack "div") hiddenDivAttr $ do text "hidden div"
  let allCharEvent = fmap chr $ domEvent Keypress keyListenerDiv -- Event t Char
  let charEvent = ffilter ((flip elem) ['a'..'z']) allCharEvent
  let charTransformer = fmap (flip updateMainStateWithChar) charEvent

  divClass <- holdDyn "half" (fmap (const "nothing") charEvent)
  rec displayState :: Dynamic t DisplayState <- foldDyn ($) startingDisplayState $ mergeWith (.) [userTransformer]
      w :: Event t (Event t DisplayState) <- dyn $ fmap connect displayState
      w2 :: Behavior t (Event t DisplayState) <- hold never w
      let connectEvent = switch w2 -- Event t DisplayState
      let userTransformer = fmap const connectEvent
  void $ performArg (const $ Element.focus (_element_raw keyListenerDiv)) $ userTransformer
  elDynAttr (T.pack "p") (fmap toHint divClass) $ text $ T.pack "Just type..."
  dynText $ fmap (T.pack . show) displayState

  rec currentState :: Dynamic t MainState <- foldDyn ($) startingMainState $ mergeWith (.) [charTransformer, nextTransformer, clockTransformer]
      dyn $ fmap displayMainState currentState
      nextSimple :: Behavior t (Event t NextEvent) <- hold never nextEv
      let nextCharEvent = fmap charToNext $ ffilter (\a -> a==' ' || a=='\r') allCharEvent
      let nextEvent = leftmost [nextCharEvent, switch nextSimple]
      let nextTransformer = fmap (flip updateMainStateWithNext) nextEvent

      let event = leftmost [fmap (const NewEntered) nextTransformer, fmap (const CharEntered) charTransformer] -- Event t EventType
      dIsFirst :: Dynamic t (Bool, Bool) <- foldDyn isFirstChar (True, False) event

      let eIsFirst = ffilter hasStarted (updated dIsFirst)

      let eHasNotCompleted = attachPromptlyDynWith hasNotCompleted currentState charEvent
      let eHasCompleted = attachPromptlyDynWith hasCompleted currentState charEvent
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
      dynText $ fmap (T.pack . show) currentState

      -- Upon complete level, push LevelSummary to db
      -- liftIO submitLevelSummary $ levelSummary 
  void $ performArg (const $ Element.focus (_element_raw keyListenerDiv)) $ nextTransformer
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
  -- el "p" $ do
  --   text "Time elapsed: "
  --   dynText clock
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
