(test-group "parse optional<type>"
  (test-parse #t (parse-optional parse-u8) #u8(0))
  (test-parse 23 (parse-optional parse-u8) #u8(1 23))
  (test-parse 42 (parse-optional parse-u8) #u8(23 42)))

(test-group "parse map[type A]type B"
  (test-parse '((5 6) (35 2))
              (parse-mapping parse-u8 parse-u8)
              #u8(#x2 #x23 #x2 #x5 #x6))
  (test-parse '()
              (parse-mapping parse-u16 parse-u32)
              #u8(#x0)))
