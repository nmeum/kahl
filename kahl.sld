;;> A parser combinator library for decoding \hyperlink["https://baremessages.org/"]{BARE} messages.
;;>
;;> The structure of this document is inspired by the BARE draft,
;;> it is highly recommended to at least briefly read Section 2 of
;;> \hyperlink[https://datatracker.ietf.org/doc/draft-devault-bare/]{draft-devault-bare-01}
;;> before continuing.

;;> The implemented parser combinators are based on a modified version of
;;> \hyperlink["https://synthcode.com/scheme/chibi/lib/chibi/parse.html"]{(chibi parse)}
;;> the documentation of which may also be consulted for more background information
;;> on individual procedures.
;;>
;;> Regarding error handling, this document makes a distinction between
;;> errors occurring during parser constructing (e.g. creating an empty
;;> struct) and errors occurring during parsing (e.g. parsing a tagged
;;> union containing an unsupported type). The former result in an
;;> R\superscript{7}RS exception, the latter result in the invocation of a
;;> specified failure continuation which defaults to a function ignoring
;;> the failure reason and returning false. Refer to the documentation
;;> of \scheme{call-with-parse} for more information.

(define-library (kahl)
  (import (scheme base) (scheme file) (scheme write) (srfi 151))

  (export make-parse-stream bytevector->parse-stream parse call-with-parse)
  (export parse-uint parse-int parse-u8 parse-u16
          parse-u32 parse-u64 parse-i8 parse-i16 parse-i32
          parse-i64 parse-bool parse-string parse-optional
          parse-map parse-list parse-union parse-struct
          parse-data parse-enum parse-void)

  (include "lib/util.scm"
           "lib/convert.scm"
           "lib/parser.scm"
           "lib/bare.scm"))
