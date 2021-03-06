(test-group "integer"
  (test "parse variable length uint"
    300
    (test-parse parse-var-uint #u8(#xac #x2))))
