module Contexht where
  
type SpecDescription = String

data Spec = It SpecDescription SpecResult
          | Context SpecDescription [Spec]
          | Pending SpecDescription [Spec]
          -- | DependentContext String SpecResult [Spec] -- "Dependent" because [Spec] may not run if SpecResult fails
          deriving (Show)

data SpecResult = PASS
                | FAIL String
                deriving (Eq, Show)

type NumPASS = Int
type NumFAIL = Int
type NumPEND = Int
type SpecStat = (NumPASS, NumFAIL, NumPEND)


-- Assertions
assert          :: Bool -> String -> SpecResult
assert True  msg = PASS
assert False msg = FAIL msg

assertEqual :: (Eq a) => a -> a -> String -> SpecResult
assertEqual x y msg = assert (x == y) msg


-- Basic stat collection
specStats                  :: Spec -> SpecStat
specStats (It _ PASS)       = (1, 0, 0)
specStats (It _ (FAIL _))   = (0, 1, 0)
specStats (Pending _ specs) = (0, 0, countPendingList specs)
specStats (Context _ specs) = foldl1 addStat $ map specStats specs
  where
    addStat (a1, b1, c1) (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)


-- Stat collection for Pending contexts:  every It nested under a Pending should be counted as a pending spec
countPendingList specs             = sum (map countPendingSpec specs)
countPendingSpec (It _ _)          = 1
countPendingSpec (Context _ specs) = countPendingList specs
countPendingSpec (Pending _ specs) = countPendingList specs


-- Stat summary display
specStatsDisplay     :: Spec -> String
specStatsDisplay spec = (specOrSpecs numSpecs) ++ " run.  " ++
                        (show numPass)         ++ " passed, " ++
                        (show numPend)         ++ " pending, " ++
                        (show numFail)         ++ " failed."
  where
    numSpecs                    = numPass + numFail + numPend
    (numPass, numFail, numPend) = specStats spec
    specOrSpecs :: Int -> String
    specOrSpecs 1 = "1 spec"
    specOrSpecs n = (show n) ++ " specs"


-- Packaging all the various displays together...
specResults     :: Spec -> String
specResults spec = "\n" ++ (specStatsDisplay spec)

-- ...and running the above
runSpecs = putStrLn . specResults
