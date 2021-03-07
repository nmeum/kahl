(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define msb 7) ;; most-significant bit
(define lsb 0) ;; least-significant bit

(define (lsb-set? i) (bit-set? lsb i))
(define (msb-set? i) (bit-set? msb i))

(define (range upto)
  (if (zero? upto)
    '()
    (let ((i (dec upto)))
      (append (range i) (list i)))))

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

(define (bytevector-fold-right proc seed bv)
  (define (%bytevector-fold-right n)
    (if (>= n (bytevector-length bv))
      seed
      (proc (bytevector-u8-ref bv n)
            (%bytevector-fold-right (inc n)))))

  (if (zero? (bytevector-length bv))
    seed
    (%bytevector-fold-right 0)))

(define (lset-unique? lset)
  (call-with-current-continuation
    (lambda (k)
      (letrec ((proc (lambda (l)
                       (if (null? l)
                         #t
                         (if (memv (car l) (cdr l))
                           (k #f)
                           (proc (cdr l)))))))
        (k (proc lset))))))
