(in-package :tsdb)

(setf *mmt-languages*
  '(:hau :isl :zul))

(setf *mmt-transfer-grammars*
  '((:any :isl :isl-acm)
    (:any :hau :hau-acm)
    (:any :zul :identity)
    (:any :any :identity)))
