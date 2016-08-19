{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.IORef
import Data.Sequence
import Network.HTTP.Types.Status
import System.Time
import Web.Scotty
-- import qualified Data.ByteString.Lazy.Char8 as BSL

import Nivix.Types

-- Nivix is the home of the firemind, a place where all crazy experiments live
-- Nivix zal vier taken hebben:
-- 1. Dashboard server.
-- 2. Dead mans snitch functionaliteit
-- 3. Stayalives teruggeven wanneer dat nodig is. 
-- 4. Alle events persisten naar disk.

mainlog = "nivix-log.txt"
keepAliveFile = "keepalive.txt"

main = do
    firemind <- newChan :: IO (Chan Event)   --main event channel, hiervan gaat iedereen lezen of een dupchan maken
    forkIO $ persister firemind         --persister schrijft alle items veilig weg naar disk
    seqref <- newIORef empty
    (dupChan firemind) >>= forkIO . dashboardBufferer seqref
    nu <- getClockTime
    snitchLastTime <- newIORef nu -- :: IO (IORef ClockTime)
    snitchChan <- (dupChan firemind)
    forkIO $ snitch snitchLastTime snitchChan
    scotty 3000 $ do
        get  "/" $ showDashboard --serve een static file met de homepage
        get  "/history" $ getHistory --seqref
        -- get  "/snitch" $ getSnitch snitchLastTime
        post "/" $ receiveEvent firemind --een nieuwe event
        notFound show404                        -- deze matcht alles, net als "otherwise" in een guard

--serve homepage/dashboard
showDashboard :: ActionM () --ActionM () is de type signature van een route handler, het is een monad waarin je de response kan samenstellen
showDashboard = do
    setHeader "Content-Type" "text/plain" --sets the headers, in dit geval de Content-Type header met value text/plain
    file "scottytest.hs" --body van de response is een file (scottytest.hs dus)
        
--ontvang een POST request, als het een geldig event is, schrijf het weg naar de 
receiveEvent :: Chan Event -> ActionM ()
receiveEvent firemind = do
    now <- liftIO getClockTime
    liftIO $ print $ "received event at " ++ (show now)
    bdy <- body
    event <- (return $ decode bdy :: ActionM (Maybe Event))
    case event of
        Nothing -> show404
        Just (STUW si sv) -> (liftIO getClockTime) >>= (\(TOD x _) -> liftIO (writeChan firemind (STUWWITHTIME x si sv))) --voegt tijd toe aan de event, want de Pi heeft geen real time clock
        Just evt -> liftIO (writeChan firemind evt)
    setHeader "Content-Type" "text/plain"
    shouldStayAlive <- liftIO $ readFile keepAliveFile
    case shouldStayAlive of
        "YES"   -> text "stayalive"
        "NO"    -> text "NO"
        _       -> text "NO"
    

--schrijft alle events naar het main logfile
persister :: Chan Event -> IO ()
persister firemind = forever $ do
    currentEvent <- readChan firemind
    now <- getClockTime
    (appendFile mainlog) $ concat [(show now)," ",(show currentEvent), "\n"]
    print $ currentEvent

--
dashboardBufferer :: IORef (Seq Event) -> Chan Event -> IO ()
dashboardBufferer seqref chan = forever $ do
    currentEvent <- readChan chan
    now <- getClockTime
    appendToBuffer seqref currentEvent

-- 
appendToFixedSizeSequence :: Int -> Event -> Seq Event -> Seq Event
appendToFixedSizeSequence maxsize newEvent startSeq = if (intermediateLength > maxsize) then newseq else intermediateSeq
    where   intermediateSeq = startSeq |> newEvent
            intermediateLength = Data.Sequence.length intermediateSeq
            (_ :< newseq) = viewl intermediateSeq
            
appendToBuffer :: IORef (Seq Event) -> Event -> IO ()
appendToBuffer ref evt = modifyIORef' ref $ appendToFixedSizeSequence 200 evt

--deze draait als aparte thread, elke keer als er een thread ontvangen wordt dan reset de snitchTimeout
snitch :: IORef ClockTime -> Chan Event -> IO ()
snitch ref chan = forever $ do
    _ <- readChan chan --hierop zit je het meeste gedeelte van de tijd geblokt
    now <- getClockTime 
    writeIORef ref now --update de tijd

--
-- getHistory :: IORef (Seq Event) -> ActionM ()
getHistory = do
    setHeader "Content-Type" "text/html"
    html "TODO: history implementeren"

--returns het aantal seconden tussen nu en de laatste event die je tegen kwam
-- getSnitch :: IORef ClockTime -> ActionM ()
-- getSnitch ref = do
    -- now <- liftIO getClockTime
    -- (TOD last _) <- liftIO $ readIORef ref
    -- setHeader "Content-Type" "text/html"
    -- html $ BL.pack . show (now-last) --(aantal seconden tussen toen en nu)
    -- html "TODO: snitch implementeren"

show404 :: ActionM ()
show404 = do
    setHeader "Content-Type" "text/html"
    status notFound404  -- moet eigenlijk een 404 zijn natuurlijk
    html "Your page was not found :("
