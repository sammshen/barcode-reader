module HipBarcodes where

import           Code128
import           Data.Char
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Graphics.Image (Image, Pixel(..), RGB, VU(VU))
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface (MArray)
import qualified Graphics.Image as Image
import           System.Environment
import           System.Exit


--------------------------------------------------------------------------------
-- NOTE: Defining these at top-level rather than changing Code128.TheCodes
-- to include them, like the start and stop symbols.

bToC, cToB :: SymbolId
bToC = 99
cToB = 100


--------------------------------------------------------------------------------
-- General Decoding

decode :: TheCodes -> BC -> Either Error String
decode theCodes (start_, data_, checksum_, stop_)
  | start_ /= startB theCodes && start_ /= startC theCodes
    = Left $ "decode: bad start: " ++ show start_
  | stop_ /= stop theCodes
    = Left $ "decode: bad stop: " ++ show stop_
  | not $ all validSymbolId data_ 
    = Left $ "decode: invalid data: " ++ show data_
  | checksum_ /= computeChecksum start_ data_
    = Left $ "decode: bad checksum: " ++ show checksum_
  | start_ == startB theCodes
    = Right $ decodeBUntil theCodes data_ []
  | start_ == startC theCodes
    = Right $ decodeCUntil theCodes data_ []

decodeBUntil :: TheCodes -> [Int] -> String -> String
decodeBUntil _ [] curr_str = curr_str
decodeBUntil theCodes (firstSymbol : remSymbols) curr_str = 
  if firstSymbol == bToC 
    then decodeCUntil theCodes remSymbols curr_str
  else do
    let b_char = getValidBValue ( (bDecodings theCodes) ! firstSymbol) 
    decodeBUntil theCodes remSymbols (curr_str ++ [b_char])

decodeCUntil :: TheCodes -> [Int] -> String -> String
decodeCUntil _ [] curr_str = curr_str
decodeCUntil theCodes (firstSymbol : remSymbols) curr_str = 
  if firstSymbol == cToB 
    then decodeBUntil theCodes remSymbols curr_str
  else do
    let c_str = getValidCValue ( (cDecodings theCodes) ! firstSymbol)
    decodeCUntil theCodes remSymbols (curr_str ++ c_str)

--------------------------------------------------------------------------------
-- Optimal Encodings (To-Do)

encode :: TheCodes -> String -> Either Error BC
encode theCodes str
  | not $ all isPrintable str = Left "encode: unsupported characters"
  | all isDigit str           = encodeC theCodes str
  | otherwise                 = encodeB theCodes str


--------------------------------------------------------------------------------
-- Making Barcodes

makeBarcode :: FilePath -> Int -> Int -> BCString -> IO ()
makeBarcode filePath imageHeight moduleWidth (BCString symbols) = do
  let imageWidth = moduleWidth * length symbols * 11 + 2 * moduleWidth
  let newBarcode :: Image VU Y Double = Image.makeImageR 
                                          VU 
                                          (imageHeight, imageWidth) 
                                          (\(i, j) -> 
                                            if j >= imageWidth - (2 * moduleWidth) 
                                              then 0
                                            else let j' = j `div` moduleWidth in
                                              PixelY $ 
                                                fromIntegral $ 
                                                  moduleToBW $ 
                                                    (symbols !! (j' `div` 11)) 
                                                      !! (j' `mod` 11)
                                                      )
  Image.writeImage filePath newBarcode

--0 - black
--1 - white
moduleToBW :: Bool -> Int
moduleToBW bool = if bool then 0 else 1

--------------------------------------------------------------------------------
-- Scanning Barcodes

scanBarcode :: FilePath -> IO BCString
scanBarcode filePath = do
  img <- Image.readImageY VU filePath
  return $ imageToBCString img

imageToBCString :: MArray arr Y Double => Image arr Y Double -> BCString
imageToBCString img = do
  let imageWidth = Image.cols img
  let globOfBools = Prelude.map (colIsBlack img) [0 .. (imageWidth-1)]
  let moduleWidth = getModuleWidth (reverse globOfBools) 0
  let unsymboledBools = takeEvery moduleWidth globOfBools
  BCString $ everyEleven unsymboledBools


colIsBlack :: MArray arr Y Double => Image arr Y Double -> Int -> Bool
colIsBlack img i =
  let PixelY bw = Image.index img (0, i) in
    (bw == 0)

getModuleWidth :: [Bool] -> Int -> Int
getModuleWidth (True:bs) final_bars_len = getModuleWidth bs (final_bars_len + 1)
getModuleWidth (False:bs) final_bars_len = final_bars_len `div` 2 

takeEvery :: Int -> [a] -> [a]
takeEvery n xs = 
  case drop (n-1) xs of
    []     -> []
    (y:ys) -> (y : takeEvery n ys)

everyEleven :: [Bool] -> [[Bool]]
everyEleven xs = do
  let (symbol, bs) = splitAt 11 xs
  if (length symbol) < 11 then []
  else symbol : everyEleven bs
--------------------------------------------------------------------------------
-- Scanning Designed Barcodes (To-Do)

scanDesignedBarcode :: FilePath -> IO BCString
scanDesignedBarcode filePath =
  undefined

--------------------------------------------------------------------------------
-- Main

runEncoder
  :: (TheCodes -> String -> Either Error BC) -> FilePath -> Int -> Int -> String
  -> IO ()
runEncoder f filePath height moduleWidth str = do
  theCodes <- loadTheCodes
  let result = bcToBCString theCodes <$> f theCodes str
  either (const (die "encoder failed")) printEncoding result
    where
      printEncoding bcString = do
        putStrLn $ "\nPayload:\n" ++ str
        putStrLn $ "\nEncoding:\n" ++ show bcString
        makeBarcode filePath height moduleWidth bcString

runDecoder
  :: (TheCodes -> BC -> Either Error String) -> String
  -> IO ()
runDecoder f filePath = do
  theCodes <- loadTheCodes
  bcString <- scanBarcode filePath
  let bc = bcStringToBC theCodes bcString
  either (const (die "decoder failed")) printDecoding (f theCodes bc)
    where
      printDecoding str = do
        putStrLn $ "\nDecoding:\n" ++ str
        putStrLn ""

main :: IO ()
main =
  getArgs >>= processArgs
  where
    processArgs ["encode", filePath, imageHeight, moduleWidth, str] =
      HipBarcodes.runEncoder
        encode filePath (read imageHeight) (read moduleWidth) str
    processArgs ["decode", file] =
      HipBarcodes.runDecoder decode file
    processArgs _ =
      die $ "\nUsage:\n\n"
        ++ "  cabal run hip-barcodes -- encode imageFilePath imageHeight moduleWidth string\n"
        ++ "  cabal run hip-barcodes -- decode imageFilePath\n"
