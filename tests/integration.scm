(include "./tests/test-vectors/schema.scm")

(define (parse-file f fn)
  (let ((s (make-parse-stream fn)))
    (call-with-parse f s 0
                     (lambda (r s i fk) r)
                     (lambda (s i reason) (error reason)))))

(test-group "customer.bin"
  (define c (parse-file parse-person "./tests/test-vectors/customer.bin"))

  (test #t (vector? c))

  (test "James Smith" (vector-ref c 0))        ;; Name
  (test "jsmith@example.org" (vector-ref c 1)) ;; Email
  (test '#(("123 Main St" "" "" "")            ;; Address
           "Philadelphia"
           "PA"
           "United States")
        (vector-ref c 2))

  ;; Orders
  (let ((orders (vector-ref c 3)))
    (test #t (list? orders))
    (test 1  (length orders))
    (test '#(4242424242 5) (car orders)))

  ;; Metadata should be empty
  (test #t (null? (vector-ref c 4))))

(test-group "employee.bin"
  (define e (parse-file parse-person "./tests/test-vectors/employee.bin"))

  (test
    '#("Tiffany Doe"               ;; name
       "tiffanyd@acme.corp"        ;; email
       #(                          ;; address
         ("123 Main St" "" "" "")  ;; address
         "Philadelphia"            ;; city
         "PA"                      ;; state
         "United States"           ;; country
        )
       1                           ;; repartment
       "2020-06-21T21:18:05+00:00" ;; hireDate
       #t                          ;; publicKey
       ())                         ;; metadata
    e))
