(test-group "integer"
  (test-group "parse fixed-size uints"
    (test-parse 0      parse-u8 #u8(0))
    (test-parse 255    parse-u8 #u8(#xff))
    (test-parse 42     parse-u8 #u8(42))
    (test-parse #x4223 parse-u16 #u8(#x23 #x42))
    (test-parse 65535  parse-u16 #u8(#xff #xff)))

  (test-group "parse fixed-size ints"
    (test-parse 126    parse-i8  #u8(#x7e))
    (test-parse -128   parse-i8  #u8(#x80)))

  (test-group "parse variable length uints"
    (test-parse 0   parse-uint #u8(0))
    (test-parse 300 parse-uint #u8(#xac #x2)))

  (test-group "parse variable length ints"
    (test-parse 0  parse-int #u8(0))
    (test-parse -1 parse-int #u8(1))
    (test-parse 1  parse-int #u8(2))
    (test-parse -2 parse-int #u8(3))))

(test-group "parse boolean values"
  (test-parse #f parse-bool #u8(0))
  (test-parse #t parse-bool #u8(1))
  (test-parse #t parse-bool #u8(#xff)))

(test-group "string"
  (test-parse "foo" parse-string
              (bytevector-append
                #u8(3)
                (string->utf8 "foo"))))

(test-group "data<length>"
  (test-parse #u8(1 2 3 4 5) (parse-data 5) #u8(1 2 3 4 5))
  (test-parse #u8(1) (parse-data 1) #u8(1))

  ;; length of fixed-length data must be at least 1
  (test-error (parse-data 0)))

(test-group "data"
  (test-parse #u8(1 2 3 4 5) (parse-data) #u8(#x05 1 2 3 4 5))
  (test-parse #u8(#x23) (parse-data) #u8(1 #x23)))
