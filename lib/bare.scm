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
