(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define msb 7) ;; most-significant bit
(define lsb 0) ;; least-significant bit

(define (lsb-set? i) (bit-set? lsb i))
(define (msb-set? i) (bit-set? msb i))

;;> Creates a list of the length given by \var{upto}.
(define (range upto)
  (if (zero? upto)
    '()
    (let ((i (dec upto)))
      (append (range i) (list i)))))

;;> Like \var{fold} from SRFI 1, but for bytevectors.
(define (bytevector-fold proc seed bv)
  (define (%bytevector-fold n)
    (if (zero? n)
      seed
      (let ((idx (dec n)))
        (proc (bytevector-u8-ref bv idx)
              (%bytevector-fold idx)))))

  (let ((len (bytevector-length bv)))
    (if (zero? len)
      seed
      (%bytevector-fold len))))

;;> Like \var{fold-right} from SRFI 1, but for bytevectors.
(define (bytevector-fold-right proc seed bv)
  (define (%bytevector-fold-right n)
    (if (>= n (bytevector-length bv))
      seed
      (proc (bytevector-u8-ref bv n)
            (%bytevector-fold-right (inc n)))))

  (if (zero? (bytevector-length bv))
    seed
    (%bytevector-fold-right 0)))

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
