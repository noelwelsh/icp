#lang scheme/base

(require (planet schematics/schemeunit:3)
         "imrp-test.ss"
         "point-test.ss"
         "slsma-test.ss")

(define/provide-test-suite all-tests
  imrp-tests
  point-tests
  slsma-tests)