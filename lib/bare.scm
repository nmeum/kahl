;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{Conversion Routines}

;;> Converts BARE variable-length unsigned integer.
(define (bytevector->uint bv)
  (if (> (bytevector-length bv) 10)
    (error "maximum length of encoded uint is 10 bytes")
    (list->bits
      (bytevector-fold-right
        (lambda (x ys)
          (let* ((lst    (bits->list x))
                 (no-msb (if (msb-set? x)
                           (drop-right lst 1)
                           lst)))
            (append no-msb ys)))
        '() bv))))

;;> Converts BARE variable-length signed integer.
(define (uint->number n)
  (let ((x (arithmetic-shift n -1)))
    (if (zero? (first-set-bit n))
      (bitwise-eqv x)
      x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{BARE Parsers}

;;> Parses an unsigned integer with a variable-length encoding.
(define parse-var-uint
  (parse-map
    (parse-seq
      (parse-repeat (parse-pred msb-set?))
      parse-byte) ;; Each octet has MSB set, except the last one.
    (lambda (lst)
      (let* ((join (append (car lst) (cdr lst)))
             (bv   (apply bytevector join)))
        (bytevector->uint bv)))))

;;> Parses a signed integer with variable-length encoding.
(define parse-var-int
  (parse-map
    parse-var-uint
    uint->number))

;;> Parses a BARE boolean value.
(define parse-bool
  (parse-map
    parse-byte
    (lambda (x) (not (zero? x)))))

;;> Parses an unsigned integer of \var{size} bytes.
(define (parse-uint size)
  (parse-map
    (parse-bytevector size)
    (lambda (bv)
      (bytevector->number size bv))))

;;> Parses a signed integer of \var{size} bytes.
(define (parse-int size)
  (parse-map
    (parse-uint size)
    (lambda (n)
      (from-twocomp (* size 8) n))))

(define parse-u8  (parse-uint 1))
(define parse-u16 (parse-uint 2))
(define parse-u32 (parse-uint 3))
(define parse-u64 (parse-uint 4))

(define parse-i8  (parse-int 1))
(define parse-i16 (parse-int 2))
(define parse-i32 (parse-int 3))
(define parse-i64 (parse-int 4))

;;> Parses a BEAR string.
(define parse-string
  (parse-with-size-field
    parse-var-uint
    (lambda (size)
      (parse-map
        (parse-bytevector size)
        utf8->string))))

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
      parse-var-uint
      (lambda (size)
        (parse-repeat
          (parse-seq key-type val-type)
          size size)))
    reverse)) ;; To ensure that last field is authoritive with assoc
