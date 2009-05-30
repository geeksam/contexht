#!/usr/bin/env runhaskell

type SpecDescription = String

data Spec = It SpecDescription SpecResult
          | Context SpecDescription [Spec]
          -- | Pending
          -- | DependentContext String SpecAssertion [Spec] -- "Dependent" because [Spec] may not run if SpecAssertion fails
          deriving (Show)

data SpecResult = PASS
                | FAIL SpecDescription
                deriving (Show)

specs = Context "A Thingy" [
          Context "with a doohickey" [
            Context "and a frobnitz" [
              It "should do something" (if False then PASS else FAIL "but it didn't"),
              It "should do another thing" PASS
            ]
          ]
        ]


specResults     :: Spec -> String
specResults spec = specBar spec ++
                   "\n\n" ++ (specResultsIndented "" spec) ++
                   "\n" ++ (specStatsDisplay spec)

specStatsDisplay     :: Spec -> String
specStatsDisplay spec = (show numSpecs) ++ " specs run.  " ++
                        (show numPass)  ++ " passed, " ++
                        (show numFail)  ++ " failed."
  where
    numSpecs           = numPass + numFail
    (numPass, numFail) = specStats spec

type SpecStat = (Int, Int) -- (number passes, number fails)

specStats                  :: Spec -> SpecStat
specStats (It _ PASS)       = (0, 1)
specStats (It _ (FAIL _))   = (1, 0)
specStats (Context _ specs) = foldl1 addStat (map specStats specs)
  where
    addStat (a, b) (c, d) = (a+c, b+d)


specBar :: Spec -> String
specBar (It desc PASS)       = "."
specBar (It desc (FAIL msg)) = "X"
specBar (Context desc specs) = childResults
  where
    childResults = concat (map specBar specs)

specResultsIndented             :: String -> Spec -> String
specResultsIndented indent (It desc PASS)       = indent ++ desc ++ " PASS\n"
specResultsIndented indent (It desc (FAIL msg)) = indent ++ desc ++ " (FAIL: " ++ msg ++ ")\n"
specResultsIndented indent (Context desc specs) = indent ++ desc ++ "\n" ++ childResults
  where
    childResults = concat (map (specResultsIndented (indent ++ "  ")) specs)
                   -- (concatMap specResults) specs

runSpecs = putStrLn . specResults
main = runSpecs specs

{-
  runner
  assertion helpers
  syntax munging
-}


{-

    A Thingy
    - with a doohickey
      - and a frobnitz
        - should do something PASS
        - should do another thing FAIL: didn't actually do another thing

-}
