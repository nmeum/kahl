(define-library (kahl)
  (import (scheme base) (scheme file) (scheme write) (srfi 1) (srfi 151))

  (export make-parse-stream bytevector->parse-stream parse call-with-parse)
  (export parse-uint parse-int parse-u8 parse-u16
          parse-u32 parse-u64 parse-i8 parse-i16 parse-i32
          parse-i64 parse-bool parse-string parse-optional
          parse-mapping parse-list parse-union parse-struct
          parse-data)

  (include "lib/util.scm"
           "lib/convert.scm"
           "lib/parser.scm"
           "lib/bare.scm"))
