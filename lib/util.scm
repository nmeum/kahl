(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define msb 7) ;; most-significant bit
(define lsb 0) ;; least-significant bit

(define (lsb-set? i) (bit-set? lsb i))
(define (msb-set? i) (bit-set? msb i))

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

;;> Converts BARE bytvector to scheme unsigned number.
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
