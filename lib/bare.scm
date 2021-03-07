;;> Parses an unsigned integer with a variable-length encoding.
(define parse-uint
  (parse-map
    (parse-seq
      (parse-repeat (parse-pred msb-set?))
      parse-byte) ;; Each octet has MSB set, except the last one.
    (lambda (lst)
      (let* ((join (append (car lst) (cdr lst)))
             (bv   (apply bytevector join)))
        (bytevector->uint bv)))))

;;> Parses a signed integer with variable-length encoding.
(define parse-int
  (parse-map
    parse-uint
    uint->number))

;;> Parses a BARE boolean value.
(define parse-bool
  (parse-map
    parse-byte
    (lambda (x) (not (zero? x)))))

;;> Parses an unsigned integer of \var{size} bytes.
(define (parse-fixed-uint size)
  (parse-map
    (parse-bytevector size)
    (lambda (bv)
      (bytevector->number size bv))))

;;> Parses a signed integer of \var{size} bytes.
(define (parse-fixed-int size)
  (parse-map
    (parse-fixed-uint size)
    (lambda (n)
      (from-twocomp (* size 8) n))))

(define parse-u8  (parse-fixed-uint 1))
(define parse-u16 (parse-fixed-uint 2))
(define parse-u32 (parse-fixed-uint 3))
(define parse-u64 (parse-fixed-uint 4))

(define parse-i8  (parse-fixed-int 1))
(define parse-i16 (parse-fixed-int 2))
(define parse-i32 (parse-fixed-int 3))
(define parse-i64 (parse-fixed-int 4))

;;> Parses a BEAR string.
(define parse-string
  (parse-with-size-field
    parse-uint
    (lambda (size)
      (parse-map
        (parse-bytevector size)
        utf8->string))))

;;> Parses BARE data.
(define (parse-data . length)
  (if (null? length)
    (parse-with-size-field
      parse-uint
      (lambda (size)
        (parse-bytevector size)))
    (let ((l (car length)))
      (if (zero? l)
        (error "length of fixed-length data must be at least 1")
        (parse-bytevector l)))))

;;> Parses an optional value of the combinator.
(define (parse-optional type)
  (parse-with-size-field
    parse-u8
    (lambda (size)
      (if (zero? size)
        parse-epsilon
        type))))

;;> Parses a BARE map with keys of type \var{key-type} and
;;> values of type \var{val-type}.
(define (parse-mapping key-type val-type)
  (parse-map
    (parse-with-size-field
      parse-uint
      (lambda (size)
        (parse-repeat
          (parse-seq key-type val-type)
          size size)))
    reverse)) ;; To ensure that last field is authoritive with assoc

;;> Parses a list of \var{size} values of \var{type}. If no \var{size}
;;> was specified, this combinator parser a variable-length list of
;;> values.
(define (parse-list type . length)
  (if (null? length)
    (parse-with-size-field
      parse-uint
      (lambda (size)
        (parse-repeat
          type
          size size)))
    (let ((l (car length)))
      (if (zero? l)
        (error "length of fixed-length arrays must be at least 1")
        (parse-repeat type l l)))))

;;> Parses a tagged union. This combinator takes a \var{type-vector},
;;> that is a vector of combinators where each vector index reponse
;;> to the numeric identifier of a BARE type.
(define (parse-union type-vector)
  (parse-with-size-field
    parse-uint
    (lambda (id)
      (if (>= id (vector-length type-vector))
        (parse-fail "unexpected tag in tagged union")
        (vector-ref type-vector id)))))

;;> Parses a BARE struct. Each comibinator in \var{types} is invoked
;;> sequentially.
(define (parse-struct . o)
  (if (or (null? o)
          (null? (car o)))
    (error "structs must have at least one field")
    (parse-seq-list o)))
