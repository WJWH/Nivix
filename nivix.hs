{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Database.SQLite.Simple
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding
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

main = scotty 80 $ do
    get  "/dashboard" $ showDashboard --serve een static file met de homepage
    get  "/history" $ getHistory
    post "/postmeasurement" $ receiveEvent --een nieuwe event
    notFound show404 -- deze matcht alles, net als "otherwise" in een guard

--serve homepage/dashboard
showDashboard :: ActionM () --ActionM () is de type signature van een route handler, het is een monad waarin je de response kan samenstellen
showDashboard = do
    setHeader "Content-Type" "text/html" --sets the headers, in dit geval de Content-Type header met value text/html
    setHeader "Access-Control-Allow-Origin" "*"
    file "homepage.html" --body van de response is een file (scottytest.hs dus)

--ontvang een POST request, als het een geldig event is, schrijf het weg naar de database
receiveEvent :: ActionM ()
receiveEvent = do
    liftIO $ print "Event received"
    conn <- liftIO $ open dbpath
    bdy <- body
    event <- (return $ decode bdy :: ActionM (Maybe Event))
    case (decode bdy) of
        Nothing -> do
          liftIO $ print "unrecognized event:"
          liftIO $ print (show bdy)
          return ()
        Just (Stuw batlevel temp sensor) -> liftIO $ do
            now <- getCurrentTime
            execute conn "INSERT INTO Measurements VALUES (?,?,?,?)" (now,batlevel,temp,sensor)
    setHeader "Content-Type" "text/plain"
    text "NO" --could also be "keepalive", but that's for later

--dit is eigenlijk een Stuw batlevel temp, maar is gedefinierd zonder namen voor de types omdat 
--de logger op de RPi op een versie van GHC werkt die geen Template Haskell ondersteunt vanwege
--dat de Pi een ARM processor heeft. TH is nodig om Aeson te draaien. Dit is een goed compromis
--omdat de JSON encoding van deze ook makkelijk handmatig te genereren is op de Pi.
data Event = Stuw Double Double Double deriving (Show,Generic)
instance ToJSON Event
instance FromJSON Event

-- Will respond with a JSON blob containing the measurements so far
getHistory :: ActionM ()
getHistory = do
    conn <- liftIO $ open dbpath
    results <- liftIO $ (query_ conn "SELECT * from Measurements" :: IO [EventWithTime])
    setHeader "Content-Type" "text/json"
    setHeader "Access-Control-Allow-Origin" "*"
    text . TL.fromStrict . decodeLatin1 . BL8.toStrict $ encode results

data EventWithTime = EventWithTime  { time :: UTCTime
                                    , battery :: Double
                                    , temperature :: Double
                                    , sensorLevel :: Double
                                    } deriving (Show,Eq,Generic)
instance ToJSON EventWithTime
instance FromJSON EventWithTime
instance FromRow EventWithTime where
      fromRow = EventWithTime <$> field <*> field <*> field <*> field


show404 :: ActionM ()
show404 = do
    setHeader "Content-Type" "text/html"
    status notFound404  -- moet eigenlijk een 404 zijn natuurlijk
    html "Your page was not found :("
