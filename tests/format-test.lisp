(in-package #:binary-protocol/tests)

(deftest calcsize-known
  (ok (= 0 (calcsize "")))
  (ok (= 1 (calcsize "x")))
  (ok (= 4 (calcsize "4x")))
  (ok (= 2 (calcsize "<h")))
  (ok (= 4 (calcsize ">I")))
  (ok (= 8 (calcsize "!q")))
  (ok (= 8 (calcsize "<2hI")))
  (ok (= 10 (calcsize "10s")))
  (ok (= 0 (calcsize "0s")))
  (ok (= 8 (calcsize "@ff")))
  (ok (= 8 (calcsize "=d")))
  (ok (= 8 (calcsize "< 2h I"))))

(deftest unknown-code
  (ok (signals (calcsize "z") 'binary-format-error)))

(deftest count-without-code
  (ok (signals (calcsize "12") 'binary-format-error)))
