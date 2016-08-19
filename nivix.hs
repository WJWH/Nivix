{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Control.Monad.IO.Class
import Data.Aeson
import Database.SQLite.Simple
import Data.Time
import GHC.Generics
import Network.HTTP.Types
import Web.Scotty

--simple IOT server for the sensors in the watermill
--current functions: 
-- receive posts from the sensor and persist them to a sqlite db
-- show the contents of the db in a JSON format
-- serve a webpage that will parse the JSON from the last line into a nice looking graph

dbpath = "nivixdb.sqlite"

main = do
    conn <- open dbpath
    scotty 80 $ do
        get  "/" $ showDashboard --serve een static file met de homepage
        get  "/history" $ getHistory conn
        post "/" $ receiveEvent conn --een nieuwe event
        notFound show404 -- deze matcht alles, net als "otherwise" in een guard

--serve homepage/dashboard
showDashboard :: ActionM () --ActionM () is de type signature van een route handler, het is een monad waarin je de response kan samenstellen
showDashboard = do
    setHeader "Content-Type" "text/html" --sets the headers, in dit geval de Content-Type header met value text/plain
    file "homepage.html" --body van de response is een file (scottytest.hs dus)
        
--ontvang een POST request, als het een geldig event is, schrijf het weg naar de 
receiveEvent :: Connection -> ActionM ()
receiveEvent conn = do
    bdy <- body
    event <- (return $ decode bdy :: ActionM (Maybe Event))
    case (decode bdy) of
        Nothing -> return ()
        Just (Stuw batlevel temp) -> liftIO $ do
            now <- getCurrentTime
            execute conn "INSERT INTO Measurements VALUES (?,?,?)" (now,batlevel,temp)
    setHeader "Content-Type" "text/plain"
    text "NO" --could also be "keepalive", but that's for later

--dit is eigenlijk een Stuw batlevel temp, maar is gedefinierd zonder namen voor de types omdat 
--de logger op de RPi op een versie van GHC werkt die geen Template Haskell ondersteunt vanwege
--dat de Pi een ARM processor heeft. TH is nodig om Aeson te draaien. Dit is een goed compromis
--omdat de JSON encoding van deze ook makkelijk handmatig te genereren is op de Pi.
data Event = Stuw Double Double deriving (Show,Generic)
instance ToJSON Event
instance FromJSON Event

--
getHistory :: Connection -> ActionM ()
getHistory conn = do
    setHeader "Content-Type" "text/html"
    html "TODO: history implementeren"

show404 :: ActionM ()
show404 = do
    setHeader "Content-Type" "text/html"
    status notFound404  -- moet eigenlijk een 404 zijn natuurlijk
    html "Your page was not found :("
