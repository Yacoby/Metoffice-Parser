import Network.HTTP
import Text.XML.Light

import Maybe 
import System

import Print


weatherTypes = zip [0..] [
        "Clear sky",
        "Sunny",
        "Partly cloudy",
        "Sunny intervals",
        "Dust",
        "Mist",
        "Fog",
        "Medium-level cloud",
        "Low-level cloud",
        "Light rain shower",
        "Light rain shower",
        "Drizzle",
        "Light rain",
        "Heavy rain shower",
        "Heavy rain shower",
        "Heavy rain",
        "Sleet shower",
        "Sleet shower",
        "Sleet",
        "Hail shower",
        "Hail shower",
        "Hail",
        "Light snow shower",
        "Light snow shower",
        "Light snow",
        "Heavy snow shower",
        "Heavy snow shower",
        "Heavy snow",
        "Thundery shower",
        "Thundery shower",
        "Thunder storm",
        "Tropical storm",
        "",
        "Haze",
        "No sensor or reading" ]


data Weather = Weather {
    time :: String,
    feelsLike :: Float,
    gusts :: Float,
    temp :: Float,
    direction :: String,
    weatherType :: Integer
} deriving Show

data Day = Day{
    date :: String,
    weather :: [Weather]
} deriving Show

openUrl :: String -> IO String
openUrl x = getResponseBody =<< simpleHTTP (getRequest x)

main :: IO ()
main = do
    args <- getArgs

    xmlStr <- openUrl "http://www.metoffice.gov.uk/public/data/PWSCache/BestForecast/Forecast/351351"

    days <- return $ (parseXmlData xmlStr) 




    printTable ["date", "time", "temp", "weather"] $ convDaysToTbl days

    return ()

-- -----------------------------------------------------------------------------
-- Converting into a table
convDaysToTbl days = concat $ map convDayToTbl days

convDayToTbl day = [date day, "", "", "", ""] : ((weatherToTable (weather day)) ++ [["\n", "", "", "", ""]])

weatherToTable :: [Weather] -> [[String]]
weatherToTable w = map wtot w

wtot :: Weather -> [String]
wtot w = ["",
          timeToString (time w),
          (show $ round (temp w)) ++ "c",
          fromJust $ lookup (weatherType w) weatherTypes]

timeToString (x:y:_) = [x, y] ++ "00"

-- -----------------------------------------------------------------------------
-- XML Parsing stuff
parseXmlData str = map parseDay days
    where 
        xml = fromJust $ parseXMLDoc str
        days = findElements (QName "Day" Nothing Nothing) xml 

parseDay :: Element -> Day
parseDay day = Day date ( concat weather )
    where
        date = fromJust $ findAttr (QName "date" Nothing Nothing) day
        weather = map parseTimeStep $ findElements (QName "TimeStep" Nothing Nothing) day


parseTimeStep ts = map (parseWeather time) $ findElements (QName "WeatherParameters" Nothing Nothing) ts
    where
        time = fromJust $ findAttr (QName "time" Nothing Nothing) ts

parseWeather time wp = Weather time feelsLike gusts temp direction weatherType
    where
        feelsLike = read ( getStrVal "FeelsLikeTemperature" )  :: Float
        gusts = read ( getStrVal "WindGust" )  :: Float
        temp = read ( getStrVal "Temperature" )  :: Float
        direction = getStrVal "WindDirection"
        weatherType = read ( getStrVal "WeatherType" )  :: Integer

        getStrVal name =  strContent $ fromJust $ findElement ( QName name Nothing Nothing ) wp

-- -----------------------------------------------------------------------------
