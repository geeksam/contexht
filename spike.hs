#!/usr/bin/env runhaskell

type SpecDescription = String

data Spec = It SpecDescription SpecResult
          | Context SpecDescription [Spec]
          | Pending SpecDescription [Spec]
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
            ],
            Pending "This hasn't been done yet" [
              It "should prove P=NP" undefined
            ]
          ]
        ]


specResults     :: Spec -> String
specResults spec = specBar spec ++
                   "\n\n" ++ (specResultsIndented "" spec) ++
                   "\n" ++ (specStatsDisplay spec)

specPendingIndented                            :: String -> Spec -> String
specPendingIndented indent (It desc _)          = indent ++ "? " ++ desc ++ " (PENDING)\n"
specPendingIndented indent (Pending desc specs) = indent ++ "? " ++ desc ++ "\n" ++ childResults
  where
    childResults = concat $ map (specPendingIndented (indent ++ "  ")) specs
specPendingIndented indent (Context desc specs) = indent ++ "  " ++ desc ++ "\n" ++ childResults
  where
    childResults = concat $ map (specPendingIndented (indent ++ "  ")) specs


specResultsIndented                            :: String -> Spec -> String
specResultsIndented indent (It desc PASS)       = indent ++ "+ " ++ desc ++ " PASS\n"
specResultsIndented indent (It desc (FAIL msg)) = indent ++ "! " ++ desc ++ " (FAIL: " ++ msg ++ ")\n"
specResultsIndented indent (Pending desc specs) = indent ++ "? " ++ desc ++ "\n" ++ childResults
  where
    childResults = concat $ map (specPendingIndented (indent ++ "  ")) specs
specResultsIndented indent (Context desc specs) = indent ++ "  " ++ desc ++ "\n" ++ childResults
  where
    childResults = concat $ map (specResultsIndented (indent ++ "  ")) specs


specStatsDisplay     :: Spec -> String
specStatsDisplay spec = (show numSpecs) ++ " specs run.  " ++
                        (show numPass)  ++ " passed, " ++
                        (show numPend)  ++ " pending, " ++
                        (show numFail)  ++ " failed."
  where
    numSpecs                    = numPass + numFail + numPend
    (numPass, numFail, numPend) = specStats spec


type SpecStat = (Int, Int, Int) -- (number passes, number fails, number pending)

countPendingList specs             = sum (map countPendingSpec specs)
countPendingSpec (It _ _)          = 1
countPendingSpec (Context _ specs) = countPendingList specs
countPendingSpec (Pending _ specs) = countPendingList specs


specStats                  :: Spec -> SpecStat
specStats (It _ PASS)       = (0, 1, 0)
specStats (It _ (FAIL _))   = (1, 0, 0)
specStats (Pending _ specs) = (0, 0, countPendingList specs)
specStats (Context _ specs) = foldl1 addStat $ map specStats specs
  where
    addStat (a1, b1, c1) (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)


specBar :: Spec -> String
specBar (It desc PASS)       = "."
specBar (It desc (FAIL msg)) = "X"
specBar (Pending desc specs) = take (countPendingList specs) (repeat '?')
specBar (Context desc specs) = childResults
  where
    childResults = concat (map specBar specs)

runSpecs = putStrLn . specResults
main = runSpecs specs
