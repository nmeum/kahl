#!/usr/bin/env chibi-scheme

(import (scheme base) (kahl) (chibi test))

(define (%test-parse parser bv)
  (define (parse-with-error parser stream)
    (call-with-parse parser stream 0
                     (lambda (r s i fk) r)
                     (lambda (s i reason) (error reason))))

  (let* ((stream (bytevector->parse-stream bv))
         (result (parse-with-error parser stream)))
    result))

(define (test-parse expected parser bv)
  (test expected (%test-parse parser bv)))

(define (test-parse-error expected parser bv)
  (let ((r (call-with-current-continuation
             (lambda (k)
               (with-exception-handler
                 (lambda (e) (k e))
                 (lambda ( ) (k (%test-parse parser bv))))))))
    (test expected
      (if (error-object? r)
        (error-object-message r))))) ;; (not (error-object? r)) → undefined

(test-group "primitive"
  (include "tests/primitive.scm"))
(test-group "aggregated"
  (include "tests/aggregated.scm"))
