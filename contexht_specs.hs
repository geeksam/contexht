#!/usr/bin/env runhaskell

import Contexht

context_with_one_passing_spec =
  Context "a context" [
    It "should do some stuff" PASS
  ]
context_with_one_failing_spec =
  Context "a context" [
    It "should do some stuff" FAIL
  ]
pending_spec = ItEventually "should prove P=NP" undefined
context_with_one_pass_one_fail =
  Context "a context" [
    It "should do some stuff" PASS,
    It "should do some other stuff" FAIL
  ]
threeSpecs = Context "three specs" [context_with_one_passing_spec, context_with_one_failing_spec, pending_spec]


specs =
  Context "Contexht" [
    Context ".assert" [
      It "should return PASS if its argument is true"  (if (PASS == (assert True))  then PASS else FAIL),
      It "should return FAIL if its argument is false" (if (FAIL == (assert False)) then PASS else FAIL)
    ],

    Context ".assertEqual" [
      It "should pass if its two arguments are equal"     $ assert (PASS == (assertEqual (2+2) 4)),
      It "should fail if its two arguments are not equal" $ assert (FAIL == (assertEqual (2+2) 5)) -- for extremely large values of 2
    ],

    Context ".specStats" [
      It "should count a passing spec" $ assertEqual (1, 0, 0) (specStats context_with_one_passing_spec),
      It "should count a failing spec" $ assertEqual (0, 1, 0) (specStats context_with_one_failing_spec),
      It "should count a pending spec" $ assertEqual (0, 0, 1) (specStats pending_spec)
    ],

    Context ".specBar" [
      It  "should print a dot, an X, and a question mark" $
          assertEqual ".X?" (specBar threeSpecs)
    ],

    Context ".specStatsDisplay" [
      It  "should display a message for one passing spec" $
          assertEqual "1 spec run.  1 passed, 0 pending, 0 failed." (specStatsDisplay context_with_one_passing_spec),
      It  "should display a message for one pending spec" $
          assertEqual "1 spec run.  0 passed, 1 pending, 0 failed." (specStatsDisplay pending_spec),
      It  "should display a message for one failing spec" $
          assertEqual "1 spec run.  0 passed, 0 pending, 1 failed." (specStatsDisplay context_with_one_failing_spec)
    ],

    -- Not testing specChecklist; it's too verbose and I keep changing it!
    
    Context "helper methods" [
      Context ".overlay" [
        It  "should put the first string on top of the second string" $
            assertEqual (overlay "SPAM" "foo bar") "SPAMbar"
      ]
    ]
  ]

main = runSpecs specs
