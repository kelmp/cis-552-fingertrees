import FingerTree
import Test.HUnit
import Test.QuickCheck

testAll :: IO ()
testAll = do
  _ <-
    runTestTT
      ( TestList
          [ tConstruct,
            tInsertHead,
            tInsertTail,
            tHead,
            tTail,
            tIsEmpty,
            tConcat,
            tSplit,
            tMap,
            tToList
          ]
      )
  ()

quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs {maxSuccess = n, maxSize = 100}

verboseCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
verboseCheckN n = verboseCheckWith $ stdArgs {maxSuccess = n, maxSize = 100}

tConstruct :: Test
tConstruct =
  TestList
    [ "Empty tree" ~: undefined,
      "Unit tree" ~: undefined,
      "Larger tree" ~: undefined
    ]

tInsertHead :: Test
tInsertHead = undefined
