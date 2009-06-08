#!/usr/bin/env runhaskell

import Test.HUnit
import Contexht

{-
  This file exists as a bootstrap/sanity check for the algebraic data type definitions and for
  minimum functionality of the Contexht framework (i.e., things that need to be working before
  I can test *all* of Contexht's other features from within Contexht itself).
-}

context_with_one_passing_spec =
  Context "a context" [
    It "should do some stuff" PASS
  ]
context_with_one_failing_spec =
  Context "a context" [
    It "should do some stuff" FAIL
  ]
pending_spec = ItEventually "should prove P=NP" undefined

tests =
  test [
    "specStats" ~:
    test [
      "One passing" ~: (specStats context_with_one_passing_spec) ~?= (1, 0, 0)
    , "One failing" ~: (specStats context_with_one_failing_spec) ~?= (0, 1, 0)
    , "One pending" ~: (specStats pending_spec)                  ~?= (0, 0, 1)
    ],

    "assert" ~:
    test [
      "truth"   ~: Contexht.assert True  ~?= PASS
    , "falsity" ~: Contexht.assert False ~?= FAIL
    ],

    "assertEqual" ~:
    test [
      "truth"   ~: Contexht.assertEqual (2+2) 4 ~?= PASS
    , "falsity" ~: Contexht.assertEqual (2+2) 5 ~?= FAIL
    ]
  ]

main = runTestTT(tests)
