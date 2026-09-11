(in-package #:binary-protocol)

(defun native-endian ()
  (if (= 1 (ldb (byte 8 0) 1)) :little :big))

(defstruct field
  kind
  count
  size)

(defun %code-spec (code)
  (ecase code
    (#\x '(:pad 1))
    (#\c '(:char 1))
    (#\b '(:int 1))
    (#\B '(:uint 1))
    (#\? '(:bool 1))
    (#\h '(:int 2))
    (#\H '(:uint 2))
    (#\i '(:int 4))
    (#\I '(:uint 4))
    (#\l '(:int 4))
    (#\L '(:uint 4))
    (#\q '(:int 8))
    (#\Q '(:uint 8))
    (#\f '(:float 4))
    (#\d '(:float 8))
    (#\s '(:string 1))))

(defun parse-format (string)
  (check-type string string)
  (let ((n (length string))
        (i 0)
        (endian :native)
        (fields '()))
    (when (plusp n)
      (let ((first (char string 0)))
        (when (find first "@=<>!" :test #'char=)
          (setf endian (ecase first
                         ((#\@ #\=) :native)
                         (#\< :little)
                         ((#\> #\!) :big))
                i 1))))
    (loop while (< i n)
          do (let ((ch (char string i)))
               (cond
                 ((or (char= ch #\Space) (char= ch #\Tab) (char= ch #\Newline)
                      (char= ch #\Return))
                  (incf i))
                 ((digit-char-p ch)
                  (let ((count 0))
                    (loop while (and (< i n) (digit-char-p (char string i)))
                          do (setf count (+ (* count 10)
                                            (digit-char-p (char string i) 10)))
                             (incf i))
                    (when (>= i n)
                      (error 'binary-format-error
                             :format string
                             :message "count without type code"))
                    (let* ((code (char string i))
                           (spec (handler-case (%code-spec code)
                                   (error ()
                                     (error 'binary-format-error
                                            :format string
                                            :message (format nil "unknown code ~S" code))))))
                      (incf i)
                      (push (make-field :kind (first spec)
                                        :count count
                                        :size (second spec))
                            fields))))
                 (t
                  (let ((spec (handler-case (%code-spec ch)
                                (error ()
                                  (error 'binary-format-error
                                         :format string
                                         :message (format nil "unknown code ~S" ch))))))
                    (incf i)
                    (push (make-field :kind (first spec)
                                      :count 1
                                      :size (second spec))
                          fields))))))
    (when (eq endian :native)
      (setf endian (native-endian)))
    (values endian (nreverse fields))))

(defun field-nbytes (field)
  (* (field-count field) (field-size field)))

(defun field-nvalues (field)
  (ecase (field-kind field)
    ((:pad) 0)
    ((:string) 1)
    ((:char :int :uint :bool :float) (field-count field))))

(defun calcsize (format)
  (multiple-value-bind (endian fields) (parse-format format)
    (declare (ignore endian))
    (reduce #'+ fields :key #'field-nbytes :initial-value 0)))
