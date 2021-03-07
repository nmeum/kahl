;;> Parses a number in two's complete representation.
(define (from-twocomp numbits input)
  (let ((mask (expt 2 (- numbits 1))))
    (+ (* -1 (bitwise-and input mask))
       (bitwise-and input (bitwise-not mask)))))

;;> Converts a little-endian number representable in \var{size} bits.
(define (bytevector->number size bv)
  (let ((shift-proc (lambda (idx) (* idx 8))))
    (apply bitwise-ior
           (map (lambda (index)
                   (arithmetic-shift
                     (bytevector-u8-ref bv index)
                     (shift-proc index)))
                (range size)))))

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
