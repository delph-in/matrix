(in-package :tsdb)

(setf *mmt-languages*
  '(:arb :eng :epo :fas :fin :hai :hau :heb :hye :isl :ita :mal :spa :zul))

(setf *mmt-transfer-grammars*
  '((:any :isl :isl-acm)
    (:any :hau :hau-acm)
    (:any :zul :zul-acm)
    (:any :any :identity)))
