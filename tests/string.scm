(test-group "string"
  (test-parse "foo" parse-string
              (bytevector-append
                #u8(3)
                (string->utf8 "foo"))))
