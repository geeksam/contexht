#!/usr/bin/env runhaskell

import Contexht

specs = Context "A Thingy" [
          Context "with a doohickey" [
            Context "and a frobnitz" [
              It "should do something" $ assert (True == False) "but it didn't",
                                      -- ==> if False then PASS else FAIL "but it didn't"
              It "should do another thing" PASS
            ],
            Pending "This hasn't been done yet" [
              It "should prove P=NP" undefined
            ]
            -- DependentContext "if this works" (FAIL) [
            --   It "should solve the halting problem" undefined
            -- ]
          ]
        ]


specResults     :: Spec -> String
specResults spec = specBar spec ++
                   "\n\n" ++ (specResultsIndented "" spec) ++
                   "\n" ++ (specStatsDisplay spec)

indentChildren               :: String -> (String -> Spec -> String) -> [Spec] -> String
indentChildren indent f specs = concat $ map (f (indent ++ "  ")) specs


specPendingIndented                            :: String -> Spec -> String
specPendingIndented indent (It desc _)          = indent ++ "? " ++ desc ++ " (PENDING)\n"
specPendingIndented indent (Pending desc specs) = indent ++ "? " ++ desc ++ "\n" ++ (indentChildren indent specPendingIndented specs)
specPendingIndented indent (Context desc specs) = indent ++ "  " ++ desc ++ "\n" ++ (indentChildren indent specPendingIndented specs)


specResultsIndented                            :: String -> Spec -> String
specResultsIndented indent (It desc PASS)       = indent ++ "+ " ++ desc ++ " PASS\n"
specResultsIndented indent (It desc (FAIL msg)) = indent ++ "! " ++ desc ++ " (FAIL: " ++ msg ++ ")\n"
specResultsIndented indent (Pending desc specs) = indent ++ "? " ++ desc ++ "\n" ++ (indentChildren indent specPendingIndented specs)
specResultsIndented indent (Context desc specs) = indent ++ "  " ++ desc ++ "\n" ++ (indentChildren indent specResultsIndented specs)


specBar :: Spec -> String
specBar (It desc PASS)       = "."
specBar (It desc (FAIL msg)) = "X"
specBar (Pending desc specs) = take (countPendingList specs) (repeat '?')
specBar (Context desc specs) = childResults
  where
    childResults = concat (map specBar specs)

runSpecs = putStrLn . Main.specResults
main = Main.runSpecs specs
