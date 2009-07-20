#lang scheme/base

(require (planet schematics/schemeunit:3)
         "imrp-test.ss")

(define/provide-test-suite all-tests
  imrp-tests)