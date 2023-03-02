module Tester where

import Control.Monad
import Code128
import Data.List
import HipBarcodes

data Test a b = -- data, instead of type, for better error messages
  Test
    { funcName :: String
    , func     :: a -> Either Error b
    , input    :: a
    , output   :: Either () b -- () because we ignore the actual Error strings
    }

main :: IO ()
main = do
  theCodes <- loadTheCodes
  runTests $ tests_decode theCodes
  runTests $ tests_encode theCodes
  runScanTests tests_scanBarcode theCodes

runTests :: (Show a, Eq b) => [Test a b] -> IO ()
runTests tests = do
  putStrLn ""
  forM_ tests $ \(Test funcName func input output) ->
    printSuccess funcName input $
      case (func input, output) of
        (Left _, Left ())   -> True
        (Right b, Right b') -> b == b'
        _                   -> False

runScanTests :: [(FilePath, String)] -> TheCodes -> IO ()
runScanTests tests theCodes =
  forM_ tests $ \(filePath, output) -> do
    bcString <- scanBarcode filePath
    let result = decode theCodes $ bcStringToBC theCodes bcString
    printSuccess "scanBarcode" filePath (result == Right output)

printSuccess :: Show a => String -> a -> Bool -> IO ()
printSuccess funcName input b =
  let
    strInput =
      show input
    strInput'
      | length strInput < 40 = strInput
      | otherwise            = take 40 strInput ++ "..."
  in
    putStrLn $ displayColor (if b then Green else Red) $
      intercalate " " [if b then "PASS" else "FAIL", funcName, strInput']

--------------------------------------------------------------------------------

data Color = Red | Green deriving (Eq, Ord)

displayColor :: Color -> String -> String
displayColor color str =
  let
    -- https://en.wikipedia.org/wiki/ANSI_escape_code
    -- https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
    green  = "\ESC[32m"
    red    = "\ESC[31m"
    reset  = "\ESC[0m"
  in
    case color of
      Red   -> red ++ str ++ reset
      Green -> green ++ str ++ reset

--------------------------------------------------------------------------------

tests_decode :: TheCodes -> [Test BC String]
tests_decode theCodes =
  map (uncurry $ Test "decode theCodes" $ decode theCodes)

    -- Tests from previous assignment's starter code...
    [ ( (104,[40,65,83,75,69,76,76,1],1,106)
      , Right "Haskell!"
      )
    , ( (104,[19,17,20,17,21,25,18,22,21,19],88,106)
      , Right "3141592653"
      )
    , ( (104,[19,17,20,17,21,25,18,22,21,19],20,106) -- bad checksum
      , Left ()
      )
    , ( (105,[31,41,59,26,53],43,106)
      , Right "3141592653"
      )
    , ( (105,[31,41,59,26,53],42,106) -- bad checksum
      , Left ()
      )
    , ( (0, [], 0, 0)
      , Left ()
      )

    -- ... plus some new tests
    , ( (104,[17],18,106)
      , Right "1"
      )
    , ( (105,[12],14,106)
      , Right"12"
      )
    , ( (104,[17,18,19],8,106)
      , Right "123"
      )
    , ( (105,[12,34],82,106)
      , Right "1234"
      )
    , ( (104,[17,99,23,45],53,106)
      , Right "12345"
      )
    , ( (105,[12,34,100,21],54,106)  -- also optimal
      , Right "12345"
      )
    , ( (105,[12,34,100,88],13,106)
      , Right "1234x"
      )
    , ( (105,[12,34,100,21,88],82,106) 
      , Right "12345x"
      )
    , ( (104,[88,99,12,34],47,106)
      , Right "x1234"
      )
    , ( (104,[88,17,99,23,45],16,106)
      , Right "x12345"
      )
    , ( (104,[16,25,24,88,17,99,23,45,67,100,89,18,19],101,106)
      , Right "098x1234567y23"
      )
    , ( (104,[16,25,24,88,99,12,34,56,100,23,89,18,19],93,106) -- also optimal
      , Right "098x1234567y23"
      )
    , ( (104,[88,99,12,34,56,100,89,99,12,34,56,100,90],8,106)
      , Right "x123456y123456z"
      )
    ]

tests_encode theCodes =
  map (uncurry $ Test "encode theCodes" $ encode theCodes)

    -- Tests from previous assignment's starter code...
    [ ( "Haskell!"
      , Right (104,[40,65,83,75,69,76,76,1],1,106)
      )
    , ( "CMSC 22300? HOT Programming in Haskell!"
      , Right (104,[35,45,51,35,0,18,18,19,16,16,31,0,40,47,52,0,48,82,79,71,82,65,77,77,73,78,71,0,73,78,0,40,65,83,75,69,76,76,1],72,106)
      )
    , ( "Separation of ⛪ and 🏛"
      , Left ()
      )
    , ( "3141592653"
      , Right (105,[31,41,59,26,53],43,106)
      )

    -- ... plus some new tests
    , ( "1"
      , Right (104,[17],18,106)
      )
    , ( "12"
      , Right (105,[12],14,106)
      )
    , ( "123"
      , Right (104,[17,18,19],8,106)
      )
    , ( "1234"
      , Right (105,[12,34],82,106)
      )
    , ( "12345"
      , Right (104,[17,99,23,45],53,106)
      )
    , ( "1234x"
      , Right (105,[12,34,100,88],13,106)
      )
    , ( "12345x"
      , Right (105,[12,34,100,21,88],82,106) 
      )
    , ( "x1234"
      , Right (104,[88,99,12,34],47,106)
      )
    , ( "x12345"
      , Right (104,[88,17,99,23,45],16,106)
      )
    , ( "098x1234567y23"
      , Right (104,[16,25,24,88,17,99,23,45,67,100,89,18,19],101,106)
      )
    , ( "x123456y123456z"
      , Right (104,[88,99,12,34,56,100,89,99,12,34,56,100,90],8,106)
      )
    ]

tests_scanBarcode =
  [ ("images/barcode-0.png", "Hip, hip, hooray!")
  , ("images/barcode-1.png", "Haskell!")
  , ("images/barcode-2.png", "CMSC 22300? HOT Programming in Haskell!")
  , ("images/barcode-3.png", "3141592653")
  , ("images/barcode-4.png", "098x1234567y23")
  ]
