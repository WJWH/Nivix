import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.Aeson
import Data.IORef
import Data.Sequence



-- Nivix is the home of the firemind, a place where all crazy experiments live
-- Nivix zal vier taken hebben:
-- 1. Dashboard server.
-- 2. Dead mans snitch functionaliteit
-- 3. Stayalives teruggeven wanneer dat nodig is. 
-- 4. Alle events persisten naar disk.


main = do
firemind <- newChan :: Chan Event
forkIO persister firemind
bufferChan <- dupChan firemind
forkIO dashboardBuffer
scotty 3000 $ do
get  "/" $ fromFile homepage.html --serve een static file met de homepage
get  "/dynamic" $ json buffer --haal wat JSON op uit de buffer
post "/event/:topic" $ writeEvent firemind --een nieuwe event

 

writeEvent :: Chan Event -> ScottyM
writeEvent firemind = do
    event <- getPostContents
    writeChan firemind $ decode event
 
persister :: Chan Event -> IO ()
persister firemind = forever $ do
    currentEvent <- readChan firemind
    BL.appendFile mainlog $ decode

dashboardBufferer :: Chan Event -> IO ()
--hmmmmmmmmmmmmmmmmmmmmmmmm
-- Houdt een Data.Sequence bij van events?
-- Hoe krijgt de scotty unit toegang tot de buffer? Die twee threads moeten gaan communiceren, kutzooi.
-- State monad is weer een enorme kutzooi volgens mij, want het wordt lastig om dat over meerdere threads te doen.