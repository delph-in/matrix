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

(
 ((:path . "ute") (:content . "80proof test suite: Chemehuevi."))
 ((:path . "hau") (:content . "80proof test suite: Hausa."))
 ((:path . "jig") (:content . "80proof test suite: Jingulu."))
 ((:path . "mal") (:content . "80proof test suite: Malayalam."))
 ((:path . "nyn") (:content . "80proof test suite: Nkore-Kiga."))
 ((:path . "kal") (:content . "80proof test suite: West Greenlandic."))
 )
