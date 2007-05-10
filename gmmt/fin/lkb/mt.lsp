(in-package :mt)

;;;
;;; disable all post-transfer post-processing of MRSs; needed in Norwegian --
;;; English MT (due to mismatches in internal variable structure), but surely
;;; not when working with only one grammar.
;;;
(setf *transfer-filter-p* nil)

(setf %transfer-properties-accumulator% nil)

(setf %transfer-properties-defaults% nil)

;(setf %transfer-values-filter% nil)

(setf %transfer-properties-filter%
  (list
    ;(cons (mrs::vsym "PNG.GEND") nil)
   (cons (mrs::vsym "PSVTYPE") nil)
   (cons (mrs::vsym "GRIND") nil)
   (cons (mrs::vsym "DEF") nil)))

; Other transfer variables

(setf *transfer-edge-limit* 50)
(setf *transfer-show-output-p* nil)
