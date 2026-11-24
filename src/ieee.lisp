(in-package #:binary-protocol)

(defun %nan-p (x)
  "Is X a float NaN? On SBCL without comparing it: (= NaN NaN) can trap."
  (and (floatp x)
       (progn
         #+sbcl (sb-ext:float-nan-p x)
         #-sbcl (not (= x x)))))

(defun %inf-p (x)
  "Is X a float infinity?"
  (and (floatp x)
       (progn
         #+sbcl (sb-ext:float-infinity-p x)
         #-sbcl (and (= x x)
                     (not (zerop x))
                     (= (abs x) (* 2 (abs x)))))))

(defun encode-ieee754 (value exponent-bits mantissa-bits)
  (let* ((width (+ 1 exponent-bits mantissa-bits))
         (bias (1- (ash 1 (1- exponent-bits))))
         (sign-shift (1- width))
         (exp-shift mantissa-bits)
         (max-exp (1- (ash 1 exponent-bits)))
         (mant-mask (1- (ash 1 mantissa-bits)))
         (proto (if (= mantissa-bits 23) 1.0s0 1.0d0))
         (f (if (floatp value) (float value proto) (float value proto))))
    (cond
     ((%nan-p f)
      ;; The quiet NaN Python's struct.pack writes (7ff8... / 7fc0...).
      (logior (ash max-exp exp-shift) (ash 1 (1- mantissa-bits))))
     ((%inf-p f)
      (logior (if (minusp (float-sign f)) (ash 1 sign-shift) 0)
              (ash max-exp exp-shift)))
     ((zerop f)
      (if (minusp (float-sign f)) (ash 1 sign-shift) 0))
     (t
      (multiple-value-bind (sig exp sign) (integer-decode-float f)
        (let ((need (1+ mantissa-bits))
              (len (integer-length sig)))
          (cond
           ((< len need)
            (setf sig (ash sig (- need len))
                  exp (- exp (- need len))))
           ((> len need)
            (setf sig (ash sig (- need len))
                  exp (+ exp (- len need)))))
          (let ((biased (+ exp mantissa-bits bias)))
            (cond
             ((>= biased max-exp)
              (logior (if (minusp sign) (ash 1 sign-shift) 0)
                      (ash max-exp exp-shift)))
             ((<= biased 0)
              (let ((sub (ash sig (1- biased))))
                (logior (if (minusp sign) (ash 1 sign-shift) 0)
                        (logand sub mant-mask))))
             (t
              (logior (if (minusp sign) (ash 1 sign-shift) 0)
                      (ash biased exp-shift)
                      (logand sig mant-mask)))))))))))

(defun %special-float (sign nan-p proto)
  "Infinity of SIGN, or a NaN (NAN-P), in PROTO's float format, as Python's
struct.unpack returns them. They are made by division with the float traps
masked: with the default traps that division is an ARITHMETIC-ERROR."
  (progn
    #+sbcl
    (sb-int:with-float-traps-masked (:divide-by-zero :invalid :overflow :inexact)
      (let ((infinity (/ (float sign proto) (float 0 proto))))
        (if nan-p (- infinity infinity) infinity)))
    #-sbcl
    (error 'binary-unpack-error
           :message (format nil "IEEE ~:[infinity~;NaN~] needs float traps masked, ~
                                 which binary-protocol does on SBCL only"
                            nan-p))))

(defun decode-ieee754 (bits exponent-bits mantissa-bits)
  (let* ((bias (1- (ash 1 (1- exponent-bits))))
         (exp-shift mantissa-bits)
         (max-exp (1- (ash 1 exponent-bits)))
         (mant-mask (1- (ash 1 mantissa-bits)))
         (sign (if (logbitp (+ exponent-bits mantissa-bits) bits) -1 1))
         (exp (ldb (byte exponent-bits exp-shift) bits))
         (mant (logand bits mant-mask))
         (proto (if (= mantissa-bits 23) 1.0s0 1.0d0)))
    (cond
     ((= exp max-exp)
      (%special-float sign (not (zerop mant)) proto))
     ((zerop exp)
      (if (zerop mant)
          (* sign (float 0 proto))
          (let ((scale (expt 2.0d0 (- 1 bias mantissa-bits))))
            (float (* sign mant scale) proto))))
     (t
      (let* ((frac (+ (ash 1 mantissa-bits) mant))
             (scale (expt 2.0d0 (- exp bias mantissa-bits))))
        (float (* sign frac scale) proto))))))

(defun encode-float32 (value)
  (encode-ieee754 value 8 23))

(defun encode-float64 (value)
  (encode-ieee754 value 11 52))

(defun decode-float32 (bits)
  (decode-ieee754 bits 8 23))

(defun decode-float64 (bits)
  (decode-ieee754 bits 11 52))
