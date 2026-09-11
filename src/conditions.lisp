(in-package #:binary-protocol)

(define-condition binary-error (error)
  ((message :initarg :message :reader binary-error-message :initform nil)
   (format-string :initarg :format :reader binary-error-format :initform nil)
   (offset :initarg :offset :reader binary-error-offset :initform nil))
  (:report (lambda (c s)
             (format s "binary error~@[: ~A~]" (binary-error-message c)))))

(define-condition binary-format-error (binary-error) ())
(define-condition binary-pack-error (binary-error) ())
(define-condition binary-unpack-error (binary-error) ())
