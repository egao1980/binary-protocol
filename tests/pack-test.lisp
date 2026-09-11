(in-package #:binary-protocol/tests)

(defun octets= (a b)
  (and (= (length a) (length b))
       (every #'= a b)))

(deftest pack-endian-ints
  (ok (octets= #(1 0 0 0) (pack "<I" 1)))
  (ok (octets= #(0 0 0 1) (pack ">I" 1)))
  (ok (octets= #(#xfe #xff) (pack "<h" -2)))
  (ok (octets= #(#xff #xfe) (pack ">h" -2)))
  (ok (octets= #(1 0 2 0) (pack "<2H" 1 2)))
  (ok (octets= #(0 1 0 2 0 0 0 3) (pack ">hhl" 1 2 3)))
  (ok (octets= #(#xff) (pack "<b" -1)))
  (ok (octets= #(0 0 0 0 0 0 0 1) (pack ">Q" 1))))

(deftest pack-pad-bool-char-string
  (ok (octets= #(0 0 0 0) (pack "4x")))
  (ok (octets= #(1) (pack "?" t)))
  (ok (octets= #(0) (pack "?" nil)))
  (ok (octets= #(65) (pack "c" #\A)))
  (ok (octets= #(97 98 0) (pack "3s" "ab")))
  (ok (octets= #(97 98 99) (pack "3s" "abcd"))))

(deftest pack-floats
  (ok (octets= #(0 0 #x80 #x3f) (pack "<f" 1.0)))
  (ok (octets= #(#x3f #x80 0 0) (pack ">f" 1.0)))
  (ok (octets= #(0 0 0 0 0 0 #xf0 #x3f) (pack "<d" 1.0d0))))

(deftest unpack-roundtrip
  (ok (equal '(1 2 3) (unpack ">hhl" (pack ">hhl" 1 2 3))))
  (ok (equal '(-2) (unpack "<h" (pack "<h" -2))))
  (ok (equal '(t) (unpack "?" (pack "?" t))))
  (ok (equal '(nil) (unpack "?" (pack "?" nil))))
  (ok (equal '(#\A) (unpack "c" (pack "c" #\A)))))

(deftest unpack-string
  (ok (octets= #(97 98 0) (first (unpack "3s" (pack "3s" "ab"))))))

(deftest float-roundtrip
  (ok (= 1.0s0 (first (unpack "<f" (pack "<f" 1.0)))))
  (ok (= -2.0s0 (first (unpack ">f" (pack ">f" -2.0)))))
  (ok (= 1.0d0 (first (unpack "<d" (pack "<d" 1.0d0))))))

(deftest pack-into-offset
  (let ((buf (make-array 6 :element-type '(unsigned-byte 8) :initial-contents '(9 9 9 9 9 9))))
    (pack-into buf "<H" '(7) :start 2)
    (ok (octets= #(9 9 7 0 9 9) buf))))

(deftest unpack-from-offset
  (ok (equal '(7) (unpack-from "<H" #(9 9 7 0 9 9) :offset 2))))

(deftest pack-overflow
  (ok (signals (pack "<B" 256) 'binary-pack-error))
  (ok (signals (pack "<h" 40000) 'binary-pack-error)))

(deftest pack-overflow-use-value
  (ok (octets= #(3)
               (handler-bind ((binary-pack-error
                               (lambda (c)
                                 (use-value 3 c))))
                 (pack "<B" 256)))))

(deftest arity-errors
  (ok (signals (pack "<HH" 1) 'binary-pack-error))
  (ok (signals (pack "<H" 1 2) 'binary-pack-error)))

(deftest unpack-short
  (ok (signals (unpack "<I" #(1 2)) 'binary-unpack-error)))

(deftest native-endian-roundtrip
  (ok (equal '(#x1122) (unpack "=H" (pack "=H" #x1122))))
  (ok (equal '(#x01020304) (unpack "=I" (pack "=I" #x01020304)))))
