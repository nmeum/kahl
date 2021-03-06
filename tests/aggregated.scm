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

(test-group "parse []type"
  (test-parse '() (parse-list parse-u32 0) #u8())
  (test-parse '(#x23 #x42)
              (parse-list parse-u8 2)
              #u8(#x23 #x42)))

(test-group "[]type"
  (test-parse '(#x05 #x15)
              (parse-list parse-u8)
              #u8(2 #x05 #x15))
  (test-parse '(#x4223)
              (parse-list parse-u16)
              #u8(#x1 #x23 #x42)))
