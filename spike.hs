#!/usr/bin/env runhaskell

import Contexht hiding (specResults, runSpecs)

specs =
  Context "A Thingy" [
    Context "with a doohickey" [
      Context "and a frobnitz" [
        It "should do something" $ assert (True == False),
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
specResults spec = "\n" ++ specBar spec ++
                   "\n\n" ++ (specResultsIndented "      " spec) ++
                   "\n" ++ (specStatsDisplay spec)
  where
    overlay (a:as) (b:bs) = a : (overlay as bs)
    overlay as [] = as
    overlay [] bs = bs

    indentChildren               :: String -> (String -> Spec -> String) -> [Spec] -> String
    indentChildren indent f specs = concat $ map (f (indent ++ "  ")) specs

    specLineIt      indent text glyph prefix       = (overlay prefix indent) ++ (overlay glyph "  ") ++ text ++ "\n"
    specLineContext indent text specs = indent ++ "  " ++ text ++ "\n" ++ (indentChildren indent specResultsIndented specs)
    specLinePending indent text specs = indent ++ "  " ++ text ++ "\n" ++ (indentChildren indent pendingSpecResults specs)

    pendingSpecResults                            :: String -> Spec -> String
    pendingSpecResults indent (It desc _)          = specLineIt      indent desc "-" "PEND"
    pendingSpecResults indent (Context desc specs) = specLineContext indent desc specs
    pendingSpecResults indent (Pending desc specs) = specLinePending indent desc specs

    specResultsIndented                            :: String -> Spec -> String
    specResultsIndented indent (It desc PASS)       = specLineIt      indent desc "-" ""
    specResultsIndented indent (It desc FAIL)       = specLineIt      indent desc "-" "FAIL"
    specResultsIndented indent (Context desc specs) = specLineContext indent desc specs
    specResultsIndented indent (Pending desc specs) = specLinePending indent desc specs


runSpecs = putStrLn
         . specResults
main = runSpecs specs
