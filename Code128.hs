module Code128 where

import           Data.Char
import           Data.List
import           Data.Map (Map, (!), lookup, insert)
import qualified Data.Map as Map
import           System.Environment
import           System.Exit


--------------------------------------------------------------------------------
-- Data Representation

data BCString =            -- "Barcode String"
  BCString [Symbol]        --   Start, data, checksum, and stop symbols.
                           --   The final bar ("11") is implicit.
                           --   No quiet zones.

type Symbol   = [Module]   -- Always length 11
type Module   = Bool       -- True/False means part of bar/space

type BC =                  -- "Barcode" (internal representation)
  ( SymbolId               --   Start symbol
  , [SymbolId]             --   Encoded data
  , SymbolId               --   Checksum
  , SymbolId               --   Stop symbol
  )                        --   Final bar is implicit

type SymbolId = Int
type BValue   = Either Char String
type CValue   = Either (Char, Char) String

data TheCodes =
  TheCodes
    { startB       :: SymbolId
    , startC       :: SymbolId
    , stop         :: SymbolId
    , idsToSymbols :: Map SymbolId Symbol
    , symbolsToIds :: Map Symbol SymbolId
    , bEncodings   :: Map Char SymbolId
    , cEncodings   :: Map (Char, Char) SymbolId
    , bDecodings   :: Map SymbolId BValue
    , cDecodings   :: Map SymbolId CValue
    } deriving (Show)

type Error = String


--------------------------------------------------------------------------------
-- 1. Data Loading

loadTheCodes :: IO TheCodes
loadTheCodes = do
  rows <- map (splitOn ',') <$> lines <$> readFile "code128.csv"
  return $ rowsToCodes $ dropFirstAndLast rows

rowsToCodes :: [[String]] -> TheCodes
rowsToCodes rows =

  -- Perfect opportunity for NamedFieldPuns. Try it!
  TheCodes
    { startB = 104, startC = 105, stop = 106
    , idsToSymbols = idsToSymbols
    , symbolsToIds = symbolsToIds
    , bEncodings = bEncodings
    , cEncodings = cEncodings
    , bDecodings = bDecodings
    , cDecodings = cDecodings
    }

  where
    (idsToSymbols, symbolsToIds, bEncodings, cEncodings, bDecodings, cDecodings) =
      foldr processRow
        (Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
        rows

    processRow row accumulators =
      accumulators' where

        [_id, _pattern, _, _, _bValueInit, _cValueInit] = 
          row

        _bValue = if (_bValueInit == "space") then " "  
                  else if (_bValueInit == "comma") then ","
                  else _bValueInit

        _cValue = if (_cValueInit == "space") then " " 
                  else if (_cValueInit == "comma") then ","
                  else _cValueInit

        (idsToSymbols, symbolsToIds, bEncodings, cEncodings, bDecodings, cDecodings) =
          accumulators

        accumulators' =
          (idsToSymbols', symbolsToIds', bEncodings', cEncodings', bDecodings', cDecodings')

        symbolId = 
          read _id :: SymbolId

        idsToSymbols' = 
          Map.insert symbolId (readSymbol _pattern) idsToSymbols

        symbolsToIds' =  
          Map.insert (readSymbol _pattern) symbolId symbolsToIds

        bEncodings' =   
          Map.insert c symbolId bEncodings where
            (c:_) = _bValue

        cEncodings' =    
          Map.insert (c1, c2) symbolId cEncodings where 
            (c1:c2:_) = _cValue

        bDecodings' =  
          Map.insert symbolId (readBValue _bValue) bDecodings

        cDecodings' =  
          Map.insert symbolId (readCValue _cValue) cDecodings

splitOn :: Char -> String -> [String]         
splitOn delim str = case break (== delim) str of
  (first, delim : rest) -> first : splitOn delim rest
  (only, "") -> [only] 

dropFirstAndLast :: [a] -> [a]               
dropFirstAndLast [] = []
dropFirstAndLast [c] = []
dropFirstAndLast [c1, c2] = []
dropFirstAndLast (c:cs) = init cs

readSymbol :: String -> Symbol        
readSymbol [] = [] 
readSymbol (c:cs)
  | c == '1' = True : readSymbol cs
  | c == '0' = False : readSymbol cs

readBValue :: String -> BValue      
readBValue (c:_) = Left c 
readBValue _ = Right "Invalid B Value" 

readCValue :: String -> CValue      
readCValue (c1:c2:_) = Left (c1, c2) 
readCValue _ = Right "Invalid C Value"


--------------------------------------------------------------------------------
-- 2. Basic Encodings

encodeB :: TheCodes -> String -> Either Error BC      
encodeB TheCodes{startB = startB, bEncodings = bEncodings, stop = stop} bStr =
  if all isPrintable bStr then do
    let encodedData = map ((!) bEncodings) bStr 
    let checksum = computeChecksum startB encodedData
    Right (startB, encodedData, checksum, stop) 
  else
    Left "encodeB: unsupported characters"

encodeC :: TheCodes -> String -> Either Error BC     
encodeC TheCodes{startC = startC, cEncodings = cEncodings, stop = stop} cStr =
  if odd $ length cStr then 
    Left "encodeC: odd number of characters" 
  else do
    let cPairs = adjacentPairs cStr
    let encodedMaybeData = map (flip Map.lookup $ cEncodings) cPairs
    case sequenceMaybe encodedMaybeData of
      Nothing          -> Left "encodeC: unsupported characters"
      Just encodedData -> do 
        let checksum = computeChecksum startC encodedData
        Right (startC, encodedData, checksum, stop)

computeChecksum :: SymbolId -> [SymbolId] -> Int    
computeChecksum startSymbol symbols =
  mod (foldr (+) startSymbol (zipWith (*) [1..] symbols)) 103

isPrintable :: Char -> Bool     
isPrintable c =
  (32 <= (ord c)) && ((ord c) <= 126)

adjacentPairs :: [a] -> [(a, a)]     
adjacentPairs [] = []
adjacentPairs (x1:x2:xs) = (x1, x2) : adjacentPairs xs

sequenceMaybe :: [Maybe a] -> Maybe [a]    
sequenceMaybe [] = Just []
sequenceMaybe (Nothing:_) = Nothing
sequenceMaybe ((Just a):mbas) = 
  case sequenceMaybe mbas of
    Nothing -> Nothing
    Just as -> Just (a : as)

--------------------------------------------------------------------------------
-- 3. Basic Decodings

decodeB :: TheCodes -> BC -> Either Error String 
decodeB TheCodes{startB = startB, stop = stop, bDecodings = bDecodings}
       (start_, data_, checksum_, stop_) =
  if start_ /= startB 
    then Left $ "decodeB: bad start: " ++ show start_
  else if stop_ /= stop 
    then Left $ "decodeB: bad stop: " ++ show stop_
  else if not $ all validSymbolId data_
    then Left $ "decodeB: invalid data: " ++ show data_
  else if checksum_ /= (computeChecksum start_ data_)
    then Left $ "decodeB: bad checksum: " ++ show checksum_
  else do 
    --unsafe lookup since symbolIds already screened
    let eitherBDecodings = map ((!) bDecodings) data_
    Right $ map getValidBValue eitherBDecodings 

decodeC :: TheCodes -> BC -> Either Error String  
decodeC TheCodes{startC = startC, stop = stop, cDecodings = cDecodings}
       (start_, data_, checksum_, stop_) =
  if start_ /= startC
    then Left $ "decodeC: bad start: " ++ show start_
  else if stop_ /= stop
    then Left $ "decodeC: bad stop: " ++ show stop_
  else if not $ all validSymbolId data_
    then Left $ "decodeC: invalid data: " ++ show data_
  else if checksum_ /= (computeChecksum start_ data_) 
    then Left $ "decodeC: bad checksum: " ++ show checksum_
  else do 
    --unsafe lookup since symbolIds already screened
    let eitherCDecodings = map ((!) cDecodings) data_
    Right $ concatMap getValidCValue eitherCDecodings

validSymbolId :: Int -> Bool  
validSymbolId symbolId = (0 <= symbolId) && (symbolId <= 102)

getValidBValue :: BValue -> Char  
getValidBValue (Left c) = c

getValidCValue :: CValue -> String   
getValidCValue (Left (c1, c2)) = [c1, c2]

--------------------------------------------------------------------------------
-- 4. Barcode String Manipulation

finalBar     = "11"
symbolLength =  11

instance Show BCString where
  show :: BCString -> String
  show (BCString symbols) = do
    let mashedSymbols = concat symbols
    (map moduleToBinary mashedSymbols) ++ finalBar

moduleToBinary :: Bool -> Char
moduleToBinary bool = if bool then '1' else '0'

instance Read BCString where
  readsPrec :: Int -> String -> [(BCString, String)]
  readsPrec _ str =
    case maybeReadBCString str of
      Just bcString -> [(bcString, "")]
      Nothing       -> []

maybeReadBCString :: String -> Maybe BCString
maybeReadBCString str = do
  if not $ all isBinary str then Nothing
  else if length str < 3 * symbolLength + 2 then Nothing
  else if (length str - 2) `mod` 11 /= 0 then Nothing 
  else do
    let (bcBinary, bar) = splitAt (length str - 2) str
    if bar /= "11" then Nothing
    else Just $ BCString $ binaryToSymbol bcBinary 

isBinary :: Char -> Bool
isBinary c = (c == '0') || (c == '1')

binaryToModule :: Char -> Bool
binaryToModule c
  | c == '1' = True
  | c == '0' = False

binaryToSymbol :: String -> [[Bool]]
binaryToSymbol [] = []
binaryToSymbol str = do
  let (symbol, rest) = splitAt symbolLength str
  (map binaryToModule symbol) : binaryToSymbol rest

--------------------------------------------------------------------------------

run :: (TheCodes -> a -> Either Error b) -> a -> IO (Either Error b)
run f a = do
  theCodes <- loadTheCodes
  pure $ f theCodes a


--------------------------------------------------------------------------------
-- User Interface

bcToBCString :: TheCodes -> BC -> BCString
bcToBCString theCodes (start, data_, checksum, stop) =
  let symbolIds = [start] ++ data_ ++ [checksum, stop] in
  BCString $ map (\i -> (idsToSymbols theCodes) ! i) symbolIds

bcStringToBC :: TheCodes -> BCString -> BC
bcStringToBC theCodes (BCString symbols) =
  (start, data_, checksum, stop)
    where
      list     = map (\symbol -> (symbolsToIds theCodes) ! symbol) symbols
      start    = head list
      stop     = head $ reverse list
      checksum = head $ tail $ reverse list
      data_    = reverse $ tail $ tail $ reverse $ tail list

encodeAndShow
  :: (TheCodes -> String -> Either Error BC) -> TheCodes -> String
  -> Either Error String
encodeAndShow f theCodes str =
  show . bcToBCString theCodes <$> f theCodes str

readAndDecode
  :: (TheCodes -> BC -> Either Error String) -> TheCodes -> String
  -> Either Error String
readAndDecode f theCodes str =
  -- Call `maybeReadBCString` instead of `read` because the latter may crash
  case maybeReadBCString str of
    Nothing       -> Left "no parse"
    Just bcString ->
      let barcode = bcStringToBC theCodes $ bcString in
      f theCodes barcode

runEncoder
  :: (TheCodes -> String -> Either Error BC) -> String
  -> IO ()
runEncoder f str = do
  theCodes <- loadTheCodes
  case encodeAndShow f theCodes str of
    Left error -> die error
    Right s    -> putStrLn s

runDecoder
  :: (TheCodes -> BC -> Either Error String) -> String
  -> IO ()
runDecoder f str = do
  theCodes <- loadTheCodes
  case readAndDecode f theCodes str of
    Left error -> die error
    Right str  -> putStrLn str

main = do
  theCodes <- loadTheCodes
  args <- getArgs
  case args of
    ["encodeB", str] -> runEncoder encodeB str
    ["decodeB", str] -> runDecoder decodeB str
    ["encodeC", str] -> runEncoder encodeC str
    ["decodeC", str] -> runDecoder decodeC str
    _                -> die "Usage: cabal run code128 -- {en,de}code{B,C} string"
                         -- "Usage: ./Code128 {en,de}code{B,C} string"