{-
  Contexht:  A very small Behavio[u]r-Driven Development library for Haskell.
  Written by Sam Livingston-Gray and Jesse Wolfe.

  Design goals:  simplicity and readability.

  We test because the things we work on are difficult to understand.  The tools we
  use to test should help us manage *that* complexity, not add more of their own.
-}

module Contexht where
  
data Spec = Context      SpecDescription [Spec]
          | It           SpecDescription SpecResult
          | ItEventually SpecDescription SpecResult
          deriving (Show)

type SpecDescription = String
data SpecResult = PASS | FAIL deriving (Eq, Show)

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
specStats (ItEventually _ _) = (0, 0, 1)
specStats (It _ FAIL)        = (0, 1, 0)
specStats (It _ PASS)        = (1, 0, 0)

addSpecStat :: SpecStat -> SpecStat -> SpecStat
addSpecStat (a1, b1, c1) (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)


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
specBar (ItEventually desc _) = "?"
specBar (It desc PASS)        = "."
specBar (It desc FAIL)        = "X"


-- Spec checklist display, with prefix to call out failing specs
specChecklist n (Context desc specs)    = specLineContext n desc specs
specChecklist n (It desc PASS)          = specLineIt n desc ""
specChecklist n (It desc FAIL)          = specLineIt n desc "FAIL"
specChecklist n (ItEventually desc _)   = specLineIt n desc "PEND"
                                       
specLineContext n text specs = cr ++ tab (n+1) ++ text ++ (indentSublist n specChecklist specs)
specLineIt      n text alert = cr ++ (overlay alert $ tab (n+1)) ++ 
                                     (overlay "-"   $ tab 1)     ++ text


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


-- ...and running them
runSpecs = putStrLn
         . specResults


