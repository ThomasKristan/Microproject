-- this Microproject was produced by Peter Feldgrill and Thomas Kristan

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Network.HTTP.Conduit
import Data.Aeson
import Data.Aeson.Types (Parser, Value, parseEither)
import qualified Control.Monad as M
import qualified Data.Vector as V
import System.Exit (exitSuccess) -- exitSuccess is used to nicely terminate the application
import GHC.Exts
import GHC.Generics
import System.Environment (getArgs)
import qualified Data.Set as Set
import Data.List (sortOn, isInfixOf)
import Data.Char (toLower, isDigit)
import Text.Printf
import qualified Numeric.Sum as N -- we use Numeric.Sum.sum in order to avoid calculation errors while summing up floating point numbers.



help :: IO ()
help = do
  putStrLn "\t 'help' ... print this message"
  putStrLn "\t 'exit' ... terminate this application"
  putStrLn "\t 'payers' ... print a list of all payers"
  putStrLn "\t 'recipients' ... print a list of all recipients"
  putStrLn "\t 'quarters' ... print an overview of the currently loaded quarters for every payment category" -- quarters was added here even though it was not in the instructions since it is still a command that is available.
  putStrLn "\t 'top' n 'payers'|'recipients' '§2'|'§4'|'§31' ... print the n biggest payers|recipients for the given payment type"
  putStrLn "\t 'search' 'payers'|'recipients' searchTerm ... print a list of all payers|recipients containing the given searchTerm"
  putStrLn "\t 'load' quarter1 quarter2 .. quartern ... load date for the given list of quarters"
  putStrLn "\t 'details' 'payers'|'recipients' organization ... print a list of all payments payed or received by the given payers/recipient"


data Entry = Entry {rechtstraeger :: String,
                    quartal :: String,
                    bekanntgabe :: Int,
                    medieninhaber :: String,
                    euro :: Double} deriving (Show, Read, Eq, Ord, Generic)

parseEntry :: Value -> Parser Entry
parseEntry = withObject "object" $ \o -> do
  re <- o .: "rechtstraeger"
  qu <- o .: "quartal"
  be <- o .: "bekanntgabe"
  me <- o .: "mediumMedieninhaber"
  eu <- o .: "euro"
  return $ Entry re qu be me eu


listParse :: Value -> Parser [Entry]
listParse = withArray "array" $ \arr ->
  M.mapM parseEntry (V.toList arr)


listRecParse :: Value -> Parser [Entry]
listRecParse = withObject "object" $ \o -> do
  dat <- o .: "data"
  listParse dat


createURL :: String -> String
createURL quarter = firstPart ++ quarter ++ lastPart where
    firstPart = "https://data.rtr.at/api/v1/tables/MedKFTGBekanntgabe.json?quartal="
    lastPart = "&leermeldung=0&size=0"


--createQuarter :: String -> IO [Entry]
createQuarter quarter = do
  src <- simpleHttp $ createURL quarter
  case decode src of
    Just result -> case parseEither listRecParse result of
        Left err -> putStrLn ("there was an error parsing data for " ++ quarter) >> return []
        Right r -> putStrLn ("loaded data for " ++ quarter) >> return r
    Nothing     -> putStrLn (quarter ++ " appears to be empty...") >> return []


printRechtstraeger :: Entry -> String
printRechtstraeger (Entry rechtstraeger _ _ _ _) = rechtstraeger

printQuartale :: Entry -> String
printQuartale (Entry _ quartal _ _ _) = quartal

printBekanntgabe :: Entry -> String
printBekanntgabe (Entry _ _ bekanntgabe _ _) = show bekanntgabe

printRecipient :: Entry -> String
printRecipient (Entry _ _ _ recipients _) = recipients

printMoney :: Entry -> Double
printMoney (Entry _ _ _ _ euro) = euro


--listParticipants :: IO [a] -> (a -> String) -> IO ()
listParticipants parsedData participant = do
  dataSet <- parsedData
  putStrLn $ unlines $ sortOn (map toLower) $ Set.elems $ Set.fromList $ map participant dataSet


topFunc n participant paragraph parsedData = do
  dataSet <- parsedData
  let formatter tuple = printf "%-60s: %12.2f" (fst tuple) (snd tuple)
--  let formatter tuple = "" ++ fst tuple ++ "\t" ++ show (snd tuple)
--  let sumName name = (name,sum $ map (\(Entry _ _ _ _ euro) -> euro) $ filter (\(Entry rechtstraeger _ bekanntgabe medieninhaber _) -> (if participant == "payers" then rechtstraeger else medieninhaber) == name && bekanntgabe == paragraph) dataSet)
  let sumName name = (name,N.sum N.kbn $ map printMoney $ filter (\(Entry rechtstraeger _ bekanntgabe medieninhaber _) -> (if participant == "payers" then rechtstraeger else medieninhaber) == name && bekanntgabe == paragraph) dataSet)
  let filterAndFormat list = map formatter $ reverse $ sortOn snd $ Set.elems $ Set.fromList $ filter (\(_,x) -> x /= 0) list
  let topMoney = unlines $ take n $ filterAndFormat $ map (sumName . (if participant == "payers" then printRechtstraeger else printRecipient)) dataSet
  putStrLn topMoney


paymentsPerQuarter parsedData quarters = do
  dataSet <- parsedData
--  let geldProParagraph paragraph = map printMoney (filter (\(Entry _ quartal bekanntgabe _ _) -> bekanntgabe == paragraph && quartal == quarters) dataSet)
  let calculation = sortOn (\[(x,_,_,_)]->read x::Int) $ map (bekanntgabeSums [dataSet]) quarters
--  let calculation = sortOn (\[(x,_,_,_)]->read x::Int) $ map (bekanntgabeSums [dataSet]) quarters
--  let format [(quarter,zwei, vier, einunddreissig)] = quarter ++ ", \t\t" ++ show zwei ++ " (§2), \t\t" ++ show vier ++ " (§4), \t\t" ++ show einunddreissig ++ " (§31)"
  let format [(quarter,zwei, vier, einunddreissig)] = printf "%s, %20.2f (§2), %20.2f (§4), %20.2f (§31)" quarter zwei vier einunddreissig
  let output = unlines (map format calculation)
  putStrLn output

bekanntgabeSums parsedData quarter = do
  dataSet <- parsedData
  let geldProParagraph paragraph = map printMoney (filter (\(Entry _ quartal bekanntgabe _ _) -> bekanntgabe == paragraph && quartal == quarter) dataSet)
--  print $ printMoney $ head (filter (\(Entry _ quartal bekanntgabe _ _) -> bekanntgabe == 2 && quartal == quarter) dataSet)
  return (quarter,N.sum N.kbn (geldProParagraph 2), N.sum N.kbn (geldProParagraph 4), N.sum N.kbn (geldProParagraph 31))


--inputFunc :: IO [Entry] -> IO b
inputFunc parsedData quarters = do
  putStrLn "Enter your command or type 'help' for assistance"
  input <- getLine
  let com1 = unwords $ take 1 $ words input
  let com2 = unwords $ drop 1 $ words input
  let com3 = unwords $ drop 2 $ words input
  let com4 = unwords $ drop 3 $ words input
  mainMenu parsedData com1 com2 com3 com4 input quarters


--mainMenu :: IO [Entry] -> String -> String -> String -> IO a
mainMenu parsedData com1 com2 com3 com4 input quarters
    | input == "help"  = help >> inputFunc parsedData quarters
    | input == "exit"  = putStrLn "Bye!" >> exitSuccess
    | input == "payers" = listParticipants parsedData printRechtstraeger >> inputFunc parsedData quarters
    | input == "recipients" = listParticipants parsedData printRecipient >> inputFunc parsedData quarters
    | input == "quarters" = paymentsPerQuarter parsedData quarters >> inputFunc parsedData quarters
    | com1 == "top" && isDigit (head com2) && not (null (words com4)) && (head(words com3) == "payers" || head(words com3) == "recipients") && head (head(words com4)) == '§'
      = topFunc (read (head (words com2))::Int) (head(words com3)) (read (tail (head(words com4)))::Int) parsedData >> inputFunc parsedData quarters
    | com1 == "search" && (head(words com2) == "payers" || head(words com2) == "recipients") = searchFunc (head(words com2)) parsedData (tail (words com2)) >> inputFunc parsedData quarters
    | com1 == "load" && not (null com2) = loadQuarters (words com2) >> inputFunc parsedData quarters
    | com1 == "details" && (head(words com2) == "payers" || head(words com2) == "recipients") = detailsFunc (head(words com2)) parsedData com3 >> inputFunc parsedData quarters
    |otherwise =  putStrLn ("Sorry, the command '" ++ input ++"' is unknown!") >> inputFunc parsedData quarters


--loadQuarters :: [String] -> IO b
loadQuarters dataForQuarters = do
  let quarters = if null dataForQuarters then ["20184"] else dataForQuarters
  putStr "loading data for "
  print quarters
  parsedData <- mapM createQuarter quarters
  inputFunc (return (concat parsedData)) quarters -- here we combine the parsedData into one dataSet so that we dont have to call multiple dataSets. Since they each have their data entry "quartal" they are still identifiable
  -- we also pass on the loaded quarters here since we need it later for the quarters function (so that we can dynamically create only the entries for our loaded quarters)



detailsFunc participant parsedData name = do
  alle <- parsedData

  --let formatter tuple = "" ++ fst tuple ++ "\t" ++ show (snd tuple)
  let formatter tuple = printf "%-80s %12.2f" (fst tuple) (snd tuple)
  let filteredParagraph paragraph = filter (\(Entry rechtstraeger _ bekanntgabe medieninhaber _) -> (if participant == "payers" then rechtstraeger else medieninhaber) == name && bekanntgabe == paragraph) alle
  let sumName source name = (name,N.sum N.kbn $ map printMoney $ filter (\(Entry rechtstraeger _ _ medieninhaber _) -> (if participant == "payers" then medieninhaber else rechtstraeger) == name) source)

  let filterAndFormat list = unlines $ map formatter $ reverse $ sortOn snd $ Set.elems $ Set.fromList $ filter (\(_,x) -> x /= 0) list

  let details paragraph = filterAndFormat $ map (sumName (filteredParagraph paragraph) . (if participant == "payers" then printRecipient else printRechtstraeger)) alle
  --let detp4 = filterAndFormat $ map (sumName (filteredP 4) . (if participant == "payers" then printRecipient else printRechtstraeger)) alle
  --let detp31 = filterAndFormat $ map (sumName (filteredP 31) . (if participant == "payers" then printRecipient else printRechtstraeger)) alle

  putStrLn "\nPayments according to §2:"
  putStrLn (details 2)

  putStrLn "Payments according to §4:"
  putStrLn (details 4)

  putStrLn "Payments according to §31:"
  putStrLn (details 31)
  --let recipients2 = map printRecipient filteredp2
  --let sum2 = sum $ map printMoney filteredp2
  --putStrLn (unwords recipients2 ++ "\t\t" ++ show sum2)


--searchFunc :: (IsString t, Eq t) => t -> IO [Entry] -> [String] -> IO ()
searchFunc participant parsedData name = do
  alle <- parsedData
  let part = map (if participant == "payers" then printRechtstraeger else printRecipient) alle
  let filtered = filter (isInfixOf (map toLower (unwords name)) . map toLower) part
  putStrLn $ unlines $ sortOn (map toLower) $ Set.elems $ Set.fromList filtered


main :: IO()
main = do
  dataForQuarters <- getArgs
  loadQuarters dataForQuarters
