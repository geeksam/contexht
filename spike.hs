#!/usr/bin/env runhaskell

import Contexht

specs =
  Context "A Thingy" [
    Context "with a doohickey" [
      Context "and a frobnitz" [
        It "should do something" $ assert (True == False),
                                -- ==> if False then PASS else FAIL "but it didn't"
        It "should do another thing" PASS
      ],
      Pending "This hasn't been done yet" [
        It "should prove P=NP" undefined-- ,
        --         Context "some other junk" [
        --           It "foo" undefined,
        --           It "bar" undefined
        --         ]
      ]
      -- DependentContext "if this works" (FAIL) [
      --   It "should solve the halting problem" undefined
      -- ]
    ]
  ]

main = runSpecs specs
