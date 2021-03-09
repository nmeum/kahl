(include "./tests/test-vectors/schema.scm")

(define (parse-file f fn)
  (let* ((p (string-append "./tests/test-vectors/" fn))
         (s (make-parse-stream p)))
    (parse f s)))

(test "customer.bin"
  #("James Smith"              ;; name
    "jsmith@example.org"       ;; email
    #(
      ("123 Main St" "" "" "") ;; address
      "Philadelphia"           ;; city
      "PA"                     ;; state
      "United States"          ;; country
     )
    (#(4242424242 5))          ;; orders
    ())                        ;; metadata
  (parse-file parse-person "customer.bin"))

(test "employee.bin"
  '#("Tiffany Doe"               ;; name
     "tiffanyd@acme.corp"        ;; email
     #(
       ("123 Main St" "" "" "")  ;; address
       "Philadelphia"            ;; city
       "PA"                      ;; state
       "United States"           ;; country
      )
     1                           ;; department
     "2020-06-21T21:18:05+00:00" ;; hireDate
     #t                          ;; publicKey
     ())                         ;; metadata
  (parse-file parse-person "employee.bin"))

(test "terminated.bin"
      'void
      (parse-file parse-person "terminated.bin"))
