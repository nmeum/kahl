(test-group "integer"
  (test-group "parse variable length uints"
    (test-parse 0   parse-var-uint #u8(0))
    (test-parse 300 parse-var-uint #u8(#xac #x2)))

  (test-group "parse variable length ints"
    (test-parse 0  parse-var-int #u8(0))
    (test-parse -1 parse-var-int #u8(1))
    (test-parse 1  parse-var-int #u8(2))
    (test-parse -2 parse-var-int #u8(3))))
