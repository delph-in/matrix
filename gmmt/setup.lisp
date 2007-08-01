(in-package :tsdb)

(setf *mmt-languages*
  '(:eng :epo :fas :fin :hai :hau :heb :hye :isl :ita :mal :zul))

(setf *mmt-transfer-grammars*
  '((:any :eng :eng-acm)
    (:any :epo :epo-acm)
    (:any :fas :fas-acm)
    (:any :fin :fin-acm)
    (:any :hai :hai-acm)
    (:any :hau :hau-acm)
    (:any :heb :heb-acm)
    (:any :hye :hye-acm)    
    (:any :isl :isl-acm)
    (:any :ita :ita-acm)
    (:any :mal :mal-acm)
    (:any :zul :zul-acm)
    (:any :any :identity)))
