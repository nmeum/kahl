#!/usr/bin/env chibi-scheme

(import (scheme base) (kahl) (chibi test))

(define (test-parse parser bv)
  (let* ((stream (bytevector->parse-stream bv))
         (result (parse parser stream)))
    result))

(include "tests/integer.scm")
