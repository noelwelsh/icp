#lang scheme/base

(require (planet schematics/schemeunit:3)
         "icp-base-test.ss"
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
  icp-base-tests
  imrp-tests
  icp-tests
  point-tests
  slsma-tests
  tangent-tests
  probability-model-tests
  geometry-tests
  util-tests
  angle-tests
  idc-tests
;  error-cache-tests)
  )