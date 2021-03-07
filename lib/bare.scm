;;> \section{BARE Parsers}

;;> \subsection{Primitive Types}

;;> Parses an unsigned integer with a variable-length encoding. The
;;> maximum precision of such a number is 64-bits. The maximum length of
;;> an encoded uint is therefore 10 octets. While the length is checked,
;;> the precision is currently not.

(define parse-uint
  ;; TODO: Limit precision to 64-bit.
  (parse-map
    (parse-seq
      ;; Each octet has MSB set, except the last one.
      ;; A maximum of 10 octets can be passed, i.e. 9 with MSB set and one without.
      (parse-repeat (parse-pred msb-set?) 0 9)
      (parse-pred (lambda (x) (not (msb-set? x)))))
    (lambda (lst)
      (let* ((join (append (car lst) (cdr lst)))
             (bv   (apply bytevector join)))
        (bytevector->uint bv)))))

;;> Parses a signed integer with a variable-length encoding. The
;;> maximum precision of such a number is 64-bits. The maximum length of
;;> an encoded uint is therefore 10 octets. While the length is checked,
;;> the precision is currently not.

(define parse-int
  (parse-map
    parse-uint
    uint->number))

(define (parse-fixed-uint size)
  (parse-map
    (parse-bytevector size)
    (lambda (bv)
      (bytevector->number size bv))))

(define (parse-fixed-int size)
  (parse-map
    (parse-fixed-uint size)
    (lambda (n)
      (from-twocomp (* size 8) n))))

(define parse-u8  (parse-fixed-uint 1))
(define parse-u16 (parse-fixed-uint 2))
(define parse-u32 (parse-fixed-uint 3))

;;> Parses unsigned integers of a fixed precision, respectively
;;> 8, 16, 32, 64 bits. The result is converted to a Scheme
;;> \scheme{number} in host byte order.

(define parse-u64 (parse-fixed-uint 4))

(define parse-i8  (parse-fixed-int 1))
(define parse-i16 (parse-fixed-int 2))
(define parse-i32 (parse-fixed-int 3))

;;> Parses signed integers of a fixed precision, respectively
;;> 8, 16, 32, 64 bits. The result is converted to a Scheme
;;> \scheme{number} in host byte order.

(define parse-i64 (parse-fixed-int 4))

;;> Parses a boolean value, either \scheme{#t} or \scheme{#f}.
;;> If a value other than one or zero is found in the u8
;;> representation of the boolean value, a parsing error
;;> is raised.

(define parse-bool
  (parse-with-context
    parse-byte
    (lambda (byte)
      (lambda (source index sk fk)
        (cond
          ((zero? byte)  (sk #f source index fk))
          ((eqv? byte 1) (sk #t source index fk))
          (else (fk source index "invalid boolean value")))))))

;;> Parses an unsigned integer value from a set of possible values
;;> agreed upon in advance. The parsed value must be part of the
;;> \var{values} list, otherwise a parsing error is raised. The
;;> \var{values} list must be non-empty and each member of \var{values}
;;> must have a unique value otherwise a parser construction error
;;> is raised.

(define (parse-enum values)
  ;; TODO: Consider using SRFI 113 sets.
  (if (or (null? values) (not (lset-unique? values)))
    (error "enum must be a non-empty list of unique values")
    (parse-with-context
      parse-uint
      (lambda (v)
        (lambda (source index sk fk)
          ;; Comparing numbers → eqv? (memv) should suffice
          (if (memv v values)
            (sk v source index fk)
            (fk source index "enum value not part of given list")))))))

;;> Parses a string of text. If the data is found to contain invalid
;;> UTF-8 sequences, it should be considered invalid. However, this
;;> implementation currently uses \scheme{utf8->string} internally,
;;> does not necessarily raise an error on invalid UTF-8 sequences.

(define parse-string
  ;; TODO: Explicitly check for invalid UTF-8 sequences.
  (parse-with-context
    parse-uint
    (lambda (size)
      (parse-map
        (parse-bytevector size)
        utf8->string))))

;;> Parses arbitrary data with a fixed \var{length} in octets.
;;> The data must not be greater than 18,446,744,073,709,551,615
;;> octets in length (the maximum value of a u64). Presently, this
;;> is not ensured by the current implementation.

(define (parse-data . length)
  ;; TODO: Ensure that data is not greater than 18,446,744,073,709,551,615 octets in length.
  (if (null? length)
    (parse-with-context
      parse-uint
      (lambda (size)
        (parse-bytevector size)))
    (let ((l (car length)))
      (if (zero? l)
        (error "length of fixed-length data must be at least 1")
        (parse-bytevector l)))))

;;> Parses a type with zero length. The result is ignored on success.
;;> Inside \scheme{parse-struct} the result will not be included in the
;;> list of results.

(define parse-void
  ;; TODO: Understand how this is supposed to be used and adjust implementation.
  (parse-ignore parse-epsilon))

;;> \subsection{Aggregate Types}

;;> Parses a value of \var{type} which may or may not be present. An
;;> optional value whose initial value is set to a number other than
;;> zero or one is considered invalid and results in a parsing error.

(define (parse-optional type)
  (parse-with-context
    parse-u8
    (lambda (opt)
      (cond
        ((zero? opt)  parse-epsilon)
        ((eqv? opt 1) type)
        (else (parse-fail "invalid option value"))))))

;;> Parses a list of \var{size} values of \var{type}. If no \var{size}
;;> was specified, a variable-length list of values of \var{type} is
;;> parsed. In the latter case, a parser construction error is raised
;;> if the given length is zero.

(define (parse-list type . length)
  (if (null? length)
    (parse-with-context
      parse-uint
      (lambda (size)
        (parse-repeat
          type
          size size)))
    (let ((l (car length)))
      (if (zero? l)
        (error "length of fixed-length arrays must be at least 1")
        (parse-repeat type l l)))))

;;> Parses a mapping of values of type \var{val-type} keyed by
;;> values of type \var{val-type}. A message with repeated keys
;;> is considered invalid. Presently, this implementation does
;;> not raise a parsing error when encountering such a map. Instead,
;;> the last field is treated as authoritative.

(define (parse-mapping key-type val-type)
  ;; TODO: raise parsing error on duplicated keys
  (parse-map
    (parse-with-context
      parse-uint
      (lambda (size)
        (parse-repeat
          (parse-seq key-type val-type)
          size size)))
    reverse)) ;; To ensure that last field is authoritative with assoc

;;> Parses a tagged union whose value may be one of any type from a
;;> \var{type-vector} of types. Each combinators index in \var{type-vector}
;;> response to the numeric identifier for this type as encoded in the
;;> message. A union with a tag value that does not have a corresponding
;;> type assigned is considered invalid. A parsing error is raised when
;;> encountering such a message.

(define (parse-union type-vector)
  ;; TODO: Consider using SRFI 113 sets.
  (if (null? type-vector)
    (error "unions must have at least one type")
    (parse-with-context
      parse-uint
      (lambda (id)
        (if (>= id (vector-length type-vector))
          (parse-fail "unexpected tag in tagged union")
          (vector-ref type-vector id))))))

;;> Parses a set of values of arbitrary types, concatenated in the
;;> order given by \var{types}. The result is concatenated to a
;;> \scheme{vector}.

(define (parse-struct . types)
  (if (or (null? types)
          (null? (car types)))
    (error "structs must have at least one field")
    (parse-map
      (parse-seq-list types)
      list->vector)))
