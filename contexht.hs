module Contexht where
  
type SpecDescription = String

data Spec = Context SpecDescription [Spec]
          | Pending SpecDescription [Spec]
          | It SpecDescription SpecResult
          -- | DependentContext String SpecResult [Spec] -- "Dependent" because [Spec] may not run if SpecResult fails
          deriving (Show)

data SpecResult = PASS | FAIL
                deriving (Eq, Show)

type NumPASS = Int
type NumFAIL = Int
type NumPEND = Int
type SpecStat = (NumPASS, NumFAIL, NumPEND)


-- Assertions
assert      :: Bool -> SpecResult
assert True  = PASS
assert False = FAIL

assertEqual    :: (Eq a) => a -> a -> SpecResult
assertEqual x y = assert (x == y)


-- Basic stat collection
specStats                  :: Spec -> SpecStat
specStats (Context _ specs) = foldl1 addSpecStat $ map specStats specs
specStats (Pending _ specs) = (0, 0, countPendingList specs)
specStats (It _ FAIL)       = (0, 1, 0)
specStats (It _ PASS)       = (1, 0, 0)

addSpecStat (a1, b1, c1) (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)


-- Stat collection for Pending contexts:  every It nested under a Pending should be counted as a pending spec
countPendingList specs             = sum (map countPendingSpec specs)
countPendingSpec (Context _ specs) = countPendingList specs
countPendingSpec (Pending _ specs) = countPendingList specs
countPendingSpec (It _ _)          = 1


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


-- Red/green bar display
specBar                     :: Spec -> String
specBar (Context desc specs) = concat (map specBar specs)
specBar (Pending desc specs) = take (countPendingList specs) (repeat '?')
specBar (It desc PASS)       = "."
specBar (It desc FAIL)       = "X"


-- Spec checklist display, with prefix to call out failing specs
specChecklist n (Context desc specs)    = specLineContext n desc specs
specChecklist n (Pending desc specs)    = specLinePending n desc specs
specChecklist n (It desc PASS)          = specLineIt      n desc ""
specChecklist n (It desc FAIL)          = specLineIt      n desc "FAIL"
                                       
pendingChecklist n (Context desc specs) = specLineContext n desc specs
pendingChecklist n (Pending desc specs) = specLinePending n desc specs
pendingChecklist n (It desc _)          = specLineIt      n desc "PEND"

specLineContext n text specs = (prefix n) ++ text ++ (indentSublist n specChecklist specs)    where prefix n = cr ++ tab (n+1)
specLinePending n text specs = (prefix n) ++ text ++ (indentSublist n pendingChecklist specs) where prefix n = cr ++ tab (n+1)
specLineIt      n text alert = (prefix n) ++ text
  where
    prefix n = cr ++ (overlay alert $ tab (n+1)) ++ 
                     (overlay "-"   $ tab 1)


-- display helpers
cr = "\n"
tab n = take (n * tabSize) $ repeat ' '       where tabSize = 2

indentSublist n checklist specs = concat $ map (checklist (n+1)) specs

overlay (a:as) (b:bs) = a : (overlay as bs)
overlay as [] = as
overlay [] bs = bs


-- Packaging all the various displays together...
specResults     :: Spec -> String
specResults spec = cr ++ specBar spec ++
                   cr ++ (specChecklist 2 spec) ++ cr ++
                   cr ++ (specStatsDisplay spec)


-- ...and running the above
runSpecs = putStrLn
         . specResults


