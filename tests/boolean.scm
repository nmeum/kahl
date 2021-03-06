(test-group "parse boolean values"
  (test-parse #f parse-bool #u8(0))
  (test-parse #t parse-bool #u8(1))
  (test-parse #t parse-bool #u8(#xff)))
