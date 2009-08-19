#lang scheme/base

(require (planet schematics/schemeunit:3)
         "imrp-test.ss"
         "icp-test.ss"
         "point-test.ss"
         "slsma-test.ss"
         "tangent-test.ss"
         "geometry-test.ss"
         "util-test.ss")

(define/provide-test-suite all-tests
  imrp-tests
  icp-tests
  point-tests
  slsma-tests
  tangent-tests
  geometry-tests
  util-tests)