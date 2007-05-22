(in-package :tsdb)

(setf *mmt-languages*
  '(:arb :eng :epo :fas :fin :hai :hau :heb :hye :isl :ita :mal :spa :zul))

(setf *mmt-transfer-grammars*
  '((:any :eng :eng-acm)
    (:any :epo :identity)
    (:any :hau :hau-acm)
    (:any :isl :isl-acm)
    (:any :ita :ita-acm)
    (:any :zul :zul-acm)
    (:any :any :identity)))
