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
      parse-byte) ;; Each octet has MSB set, expect the last one.
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
(define (parse-fixed size)
  (parse-map
    (parse-bytevector size)
    (lambda (bv)
      (bytevector->number size bv))))

(define parse-u8  (parse-fixed 1))
(define parse-u16 (parse-fixed 2))
(define parse-u32 (parse-fixed 3))
(define parse-u64 (parse-fixed 4))
