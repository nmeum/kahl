#!/usr/bin/env chibi-scheme

(import (scheme base) (kahl) (chibi test))

(define (test-parse expected parser bv)
  (define (%test-parse parser bv)
    (let* ((stream (bytevector->parse-stream bv))
           (result (parse parser stream)))
      result))

  (test expected (%test-parse parser bv)))

(include "tests/primitive.scm"
         "tests/aggregated.scm")
