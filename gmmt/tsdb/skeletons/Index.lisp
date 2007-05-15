;;;
;;; this file should be `Index.lisp' and reside in the directory containing the
;;; tsdb(1) test suite skeleton databases (typically a subdirectory `skeletons'
;;; in the tsdb(1) database root directory `*tsdb-home*').
;;;
;;; the file should contain a single un-quote()d Common-Lisp list enumerating
;;; the available skeletons, e.g.
;;;
;;;   (((:path . "english") (:content . "English TSNLP test suite"))
;;;    ((:path . "csli") (:content . "CSLI (ERGO) test suite"))
;;;    ((:path . "vm") (:content . "English VerbMobil data")))
;;;
;;; where the individual entries are assoc() lists with at least two elements:
;;;
;;;   - :path --- the (relative) directory name containing the skeleton;
;;;   - :content --- a descriptive comment.
;;;
;;; the order of entries is irrelevant as the `tsdb :skeletons' command sorts
;;; the list lexicographically before output.
;;;

((((:path . "isl_gmmt") (:content . "Icelandic GMMT test sentences"))
(:path . "arb_567") (:content . "Modern Standard Arabic test suite from Ling 567"))
((:path . "hau_567") (:content . "Hausa test suite from Ling 567"))
((:path . "isl_567") (:content . "Icelandic general test suite"))
((:path . "mal_567") (:content . "Malayalam test suite from Ling 567"))
((:path . "zul_567") (:content . "Zulu test suite from Ling 567")))

 
