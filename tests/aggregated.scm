(test-group "parse optional<type>"
  (test-parse #t (parse-optional parse-u8) #u8(0))
  (test-parse 23 (parse-optional parse-u8) #u8(1 23))
  (test-parse 42 (parse-optional parse-u8) #u8(23 42)))
