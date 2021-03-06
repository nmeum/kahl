(test-group "integer"
  (test-group "parse fixed-size uints"
    (test-parse 0      parse-u8 #u8(0))
    (test-parse 255    parse-u8 #u8(#xff))
    (test-parse 42     parse-u8 #u8(42))
    (test-parse #x4223 parse-u16 #u8(#x23 #x42))
    (test-parse 65535  parse-u16 #u8(#xff #xff)))

  (test-group "parse variable length uints"
    (test-parse 0   parse-var-uint #u8(0))
    (test-parse 300 parse-var-uint #u8(#xac #x2)))

  (test-group "parse variable length ints"
    (test-parse 0  parse-var-int #u8(0))
    (test-parse -1 parse-var-int #u8(1))
    (test-parse 1  parse-var-int #u8(2))
    (test-parse -2 parse-var-int #u8(3))))
