(define (from-twocomp numbits input)
  (let ((mask (expt 2 (- numbits 1))))
    (+ (* -1 (bitwise-and input mask))
       (bitwise-and input (bitwise-not mask)))))

(define (bytevector->number size bv)
  (let ((shift-proc (lambda (idx) (* idx 8))))
    (apply bitwise-ior
           (map (lambda (index)
                   (arithmetic-shift
                     (bytevector-u8-ref bv index)
                     (shift-proc index)))
                (range size)))))

(define (bytevector->uint bv)
  (if (> (bytevector-length bv) 10)
    (error "maximum length of encoded uint is 10 bytes")
    (list->bits
      (bytevector-fold-right
        (lambda (x ys)
          (let* ((lst    (bits->list x))
                 (no-msb (if (msb-set? x)
                           (init lst)
                           lst)))
            (append no-msb ys)))
        '() bv))))

(define (uint->number n)
  (let ((x (arithmetic-shift n -1)))
    (if (not (zero? (bitwise-and n 1)))
      (bitwise-eqv x)
      x)))
