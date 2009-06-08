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
      Context "This hasn't been done yet" [
        ItEventually "should prove P=NP" FAIL,
        Context "some other junk" [
          It "nope" FAIL,
          It "yup" PASS
        ]
      ]
    ]
  ]

main = runSpecs specs
