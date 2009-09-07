#lang scheme/base

(require (planet schematics/schemeunit:3)
         "imrp-test.ss"
         "icp-test.ss"
         "idc-test.ss"
         "point-test.ss"
         "slsma-test.ss"
         "tangent-test.ss"
         "geometry-test.ss"
         "probability-model-test.ss"
         "error-cache-test.ss"
         "util-test.ss"
         "angle-test.ss")

(define/provide-test-suite all-tests
  imrp-tests
  icp-tests
  idc-tests
  point-tests
  slsma-tests
  tangent-tests
  probability-model-tests
  error-cache-tests
  geometry-tests
  util-tests
  angle-tests)