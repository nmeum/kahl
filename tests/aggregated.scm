(test-group "optional<type>"
  (test-parse 'nothing (parse-optional parse-u8) #u8(0))
  (test-parse 23 (parse-optional parse-u8) #u8(1 23))
  (test-parse-error "invalid option value"
                    (parse-optional parse-u8) #u8(23 42)))

(test-group "map[type A]type B"
  (test-parse '((5 6) (35 2))
              (parse-mapping parse-u8 parse-u8)
              #u8(#x2 #x23 #x2 #x5 #x6))
  (test-parse '()
              (parse-mapping parse-u16 parse-u32)
              #u8(#x0)))

(test-group "[length]type"
  (test-parse '(#x23 #x42)
              (parse-list parse-u8 2)
              #u8(#x23 #x42))

  ;; length of fixed-length array must be at least 1
  (test-error (parse-list parse-u8 0)))

(test-group "[]type"
  (test-parse '(#x05 #x15)
              (parse-list parse-u8)
              #u8(2 #x05 #x15))
  (test-parse '(#x4223)
              (parse-list parse-u16)
              #u8(#x1 #x23 #x42)))

(test-group "union"
  (test-parse #t
              (parse-union
                (vector
                  parse-bool ;; type id → 0
                  parse-u16  ;; type id → 1
                ))
              #u8(#x00 #x01))

  (test-parse #x1312
              (parse-union
                (vector
                  parse-bool ;; type id → 0
                  parse-u16  ;; type id → 1
                ))
              #u8(#x01 #x12 #x13))

  (test-parse-error
    "unexpected tag in tagged union"
    (parse-union
      (vector parse-u8 parse-u8))
    #u8(#x02 #x23 #x42))

  ;; unions must have at least one type
  (test-error (parse-union '())))

(test-group "struct"
  (test-parse '#(#t #xff #f)
              (parse-struct
                parse-bool
                parse-u8
                parse-bool)
              #u8(#x01 #xff #x00))

  (test-parse '#("" "")
              (parse-struct
                parse-string
                parse-string)
              #u8(#x0 #x0))

  ;; Structs MUST have at least one field.
  (test-error (parse-struct '()))
  (test-error (parse-struct)))
