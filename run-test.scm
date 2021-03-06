#!/usr/bin/env chibi-scheme

(import (scheme base) (kahl) (chibi test))

(define (test-parse expected parser bv)
  (define (%test-parse parser bv)
    (let* ((stream (bytevector->parse-stream bv))
           (result (parse parser stream)))
      result))

  (test expected (%test-parse parser bv)))

(include "tests/integer.scm"
         "tests/boolean.scm"
         "tests/string.scm"
         "tests/aggregated.scm")
