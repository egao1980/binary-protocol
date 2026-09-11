(in-package #:binary-protocol)

(defun make-octets (n)
  (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

(defun write-int (octets offset width endian value)
  (loop for i from 0 below width
        for shift = (if (eq endian :little)
                        (* i 8)
                        (* (- width 1 i) 8))
        do (setf (aref octets (+ offset i))
                 (ldb (byte 8 shift) value))))

(defun read-int (octets offset width endian)
  (let ((acc 0))
    (loop for i from 0 below width
          for shift = (if (eq endian :little)
                          (* i 8)
                          (* (- width 1 i) 8))
          do (setf acc (dpb (aref octets (+ offset i)) (byte 8 shift) acc)))
    acc))

(defun signed-value (bits width)
  (if (logbitp (1- (* width 8)) bits)
      (- bits (ash 1 (* width 8)))
      bits))

(defun coerce-int (value width signed)
  (let* ((bits (* width 8))
         (min (if signed (- (ash 1 (1- bits))) 0))
         (max (1- (ash 1 (if signed (1- bits) bits)))))
    (unless (integerp value)
      (error 'binary-pack-error
             :message (format nil "expected integer, got ~S" value)))
    (unless (<= min value max)
      (restart-case
          (error 'binary-pack-error
                 :message (format nil "~D out of range [~D, ~D]" value min max))
        (use-value (replacement)
          :report "Use a replacement integer"
          (return-from coerce-int (coerce-int replacement width signed)))))
    (if (minusp value)
        (ldb (byte bits 0) value)
        value)))

(defun coerce-char-byte (value)
  (cond
    ((and (typep value '(unsigned-byte 8))) value)
    ((characterp value)
     (let ((c (char-code value)))
       (unless (<= 0 c 255)
         (error 'binary-pack-error :message (format nil "char ~S not a byte" value)))
       c))
    ((and (stringp value) (= (length value) 1))
     (coerce-char-byte (char value 0)))
    ((and (vectorp value) (= (length value) 1)
          (typep (aref value 0) '(unsigned-byte 8)))
     (aref value 0))
    (t
     (error 'binary-pack-error
            :message (format nil "expected 1-byte char, got ~S" value)))))

(defun coerce-string-octets (value n)
  (let ((src (cond
               ((stringp value)
                (map '(simple-array (unsigned-byte 8) (*))
                     (lambda (ch)
                       (let ((c (char-code ch)))
                         (unless (<= 0 c 255)
                           (error 'binary-pack-error
                                  :message (format nil "string ~S is not latin-1" value)))
                         c))
                     value))
               ((and (vectorp value)
                     (every (lambda (b) (typep b '(unsigned-byte 8))) value))
                value)
               (t
                (error 'binary-pack-error
                       :message (format nil "expected bytes/string, got ~S" value))))))
    (let ((out (make-octets n)))
      (replace out src :end2 (min n (length src)))
      out)))

(defun %pack-fields (octets start endian fields values)
  (let ((offset start)
        (vals values))
    (dolist (field fields)
      (let ((kind (field-kind field))
            (count (field-count field))
            (size (field-size field)))
        (ecase kind
          (:pad
           (incf offset (* count size)))
          (:string
           (when (null vals)
             (error 'binary-pack-error :message "not enough values"))
           (let ((bytes (coerce-string-octets (pop vals) count)))
             (replace octets bytes :start1 offset)
             (incf offset count)))
          ((:char :int :uint :bool :float)
           (dotimes (k count)
             (when (null vals)
               (error 'binary-pack-error :message "not enough values"))
             (let ((v (pop vals)))
               (ecase kind
                 (:char
                  (setf (aref octets offset) (coerce-char-byte v))
                  (incf offset 1))
                 (:bool
                  (setf (aref octets offset) (if v 1 0))
                  (incf offset 1))
                 (:int
                  (write-int octets offset size endian (coerce-int v size t))
                  (incf offset size))
                 (:uint
                  (write-int octets offset size endian (coerce-int v size nil))
                  (incf offset size))
                 (:float
                  (write-int octets offset size endian
                             (if (= size 4) (encode-float32 v) (encode-float64 v)))
                  (incf offset size)))))))))
    (when vals
      (error 'binary-pack-error :message "too many values"))
    octets))

(defun pack (fmt &rest values)
  (multiple-value-bind (endian fields) (parse-format fmt)
    (let ((octets (make-octets (reduce #'+ fields :key #'field-nbytes :initial-value 0))))
      (%pack-fields octets 0 endian fields values))))

(defun pack-into (octets fmt values &key (start 0))
  (check-type octets (vector (unsigned-byte 8)))
  (check-type start (integer 0))
  (multiple-value-bind (endian fields) (parse-format fmt)
    (let ((need (+ start (reduce #'+ fields :key #'field-nbytes :initial-value 0))))
      (unless (>= (length octets) need)
        (error 'binary-pack-error
               :offset start
               :message (cl:format nil "buffer too small: need ~D bytes from ~D" need start)))
      (%pack-fields octets start endian fields values)
      octets)))

(defun %unpack-fields (octets start endian fields)
  (let ((offset start)
        (end (length octets))
        (out '()))
    (dolist (field fields)
      (let ((kind (field-kind field))
            (count (field-count field))
            (size (field-size field)))
        (ecase kind
          (:pad
           (let ((n (* count size)))
             (unless (<= (+ offset n) end)
               (error 'binary-unpack-error
                      :offset offset
                      :message "buffer too short"))
             (incf offset n)))
          (:string
           (unless (<= (+ offset count) end)
             (error 'binary-unpack-error
                    :offset offset
                    :message "buffer too short"))
           (let ((slice (subseq octets offset (+ offset count))))
             (push slice out)
             (incf offset count)))
          ((:char :int :uint :bool :float)
           (dotimes (k count)
             (unless (<= (+ offset size) end)
               (error 'binary-unpack-error
                      :offset offset
                      :message "buffer too short"))
             (push
              (ecase kind
                (:char (code-char (aref octets offset)))
                (:bool (plusp (aref octets offset)))
                (:int (signed-value (read-int octets offset size endian) size))
                (:uint (read-int octets offset size endian))
                (:float (if (= size 4)
                            (decode-float32 (read-int octets offset size endian))
                            (decode-float64 (read-int octets offset size endian)))))
              out)
             (incf offset size))))))
    (nreverse out)))

(defun unpack (fmt octets &key (start 0))
  (unpack-from fmt octets :offset start))

(defun unpack-from (fmt octets &key (offset 0))
  (unless (and (vectorp octets)
               (every (lambda (b) (typep b '(unsigned-byte 8))) octets))
    (error 'binary-unpack-error :message "expected octet vector"))
  (check-type offset (integer 0))
  (multiple-value-bind (endian fields) (parse-format fmt)
    (%unpack-fields octets offset endian fields)))
