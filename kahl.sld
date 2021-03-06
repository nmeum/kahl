(define-library (kahl)
  (import (scheme base) (scheme file) (scheme write) (srfi 1) (srfi 151))

  (export make-parse-stream bytevector->parse-stream parse)
  (export parse-var-uint)

  (include "lib/util.scm"
           "lib/parser.scm"
           "lib/bare.scm"))
