;; This file has been manually generated from schema.bare

(define parse-public-key (parse-data 128))
(define parse-time parse-string)

(define parse-department
  (parse-enum
    (list 0 1 2 3 4 99)))

(define parse-address
  (parse-struct
    (parse-list parse-string 4)
    parse-string
    parse-string
    parse-string))

(define parse-customer
  (parse-struct
    parse-string
    parse-string
    parse-address
    (parse-list
      (parse-struct
        parse-i64
        parse-i32))
    (parse-mapping parse-string parse-data)))

(define parse-employee
  (parse-struct
    parse-string
    parse-string
    parse-address
    parse-department
    parse-time
    (parse-optional
      parse-public-key)
    (parse-mapping parse-string parse-data)))

(define parse-terminated-employee parse-void)

(define parse-person
  (parse-union
    (vector
      parse-customer
      parse-employee
      parse-terminated-employee)))
