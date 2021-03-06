(test-group "uint"
  (test-parse 0   parse-uint #u8(0))
  (test-parse 300 parse-uint #u8(#xac #x2))

  (test-parse 12418871010253280812 parse-uint
              #u8(#xac #xac #xac #xac #xac #xac #xac #xac #xac #x01))

  ;; 64-bit precision exceeded.
  (test-parse-error "failed predicate" parse-uint
    #u8(#xff #xff #xff #xff #xff #xff #xff #xff #xff #x2c))

  ;; Too many octets.
  (test-parse-error "failed predicate" parse-uint
              #u8(#xac #xac #xac #xac #xac #xac #xac #xac #xac #xac #x2))

  (test-parse-error "failed predicate" parse-uint #u8(#xac))
  (test-parse-error "failed predicate" parse-uint #u8(#xac #xac)))

(test-group "int"
  (test-parse 0  parse-int #u8(0))
  (test-parse -1 parse-int #u8(1))
  (test-parse 1  parse-int #u8(2))
  (test-parse -2 parse-int #u8(3)))

(test-group "u8, u16, u32, u64"
  (test-parse 0      parse-u8 #u8(0))
  (test-parse 255    parse-u8 #u8(#xff))
  (test-parse 42     parse-u8 #u8(42))
  (test-parse #x4223 parse-u16 #u8(#x23 #x42))
  (test-parse 65535  parse-u16 #u8(#xff #xff))

  (test-parse-error "unexpected eof" parse-u16 #u8(1))
  (test-parse-error "unexpected eof" parse-u32 #u8(1 2)))

(test-group "i8, i16, i32, i64"
  (test-parse 126    parse-i8  #u8(#x7e))
  (test-parse -128   parse-i8  #u8(#x80))

  (test-parse 4242424242 parse-i64 #u8(#xb2 #x41 #xde #xfc #x00 #x00 #x00 #x00)))

(test-group "bool"
  (test-parse #f parse-bool #u8(0))
  (test-parse #t parse-bool #u8(1))

  (test-parse-error "invalid boolean value"
                    parse-bool #u8(#xff)))

(test-group "enum"
  (test-parse 42 (parse-enum '(23 42 1337)) #u8(42))
  (test-parse-error "enum value not part of given list"
                    (parse-enum '(42 23)) #u8(5))

  ;; enum value list must be non-empty
  (test-error (parse-enum '()))

  ;; enum values must be inuque
  (test-error (parse-enum '(23 23))))

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
