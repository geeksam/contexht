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
pending_context = 
  Pending "This hasn't been done yet" [
    It "should prove P=NP" undefined
  ]
context_with_one_pass_one_fail = 
  Context "a context" [
    It "should do some stuff" PASS,
    It "should do some other stuff" FAIL
  ]

specs = 
  Context "Contexht" [
    Context ".specStatsDisplay" [
      It  "should display a message for one passing spec" $ 
          assertEqual "1 spec run.  1 passed, 0 pending, 0 failed." (specStatsDisplay context_with_one_passing_spec),
      It  "should display a message for one pending spec" $ 
          assertEqual "1 spec run.  0 passed, 1 pending, 0 failed." (specStatsDisplay pending_context),
      It  "should display a message for one failing spec" $ 
          assertEqual "1 spec run.  0 passed, 0 pending, 1 failed." (specStatsDisplay context_with_one_failing_spec)
    ],
    
    let threeSpecs = Context "three specs" [context_with_one_passing_spec, context_with_one_failing_spec, pending_context] in
    Context ".specBar" [
      It  "should print a dot, an X, and a question mark" $
          assertEqual ".X?" (specBar threeSpecs)
    ]
  ]

main = runSpecs specs
