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

(((:path . "epo") (:content . "Espertanto test suite from Ling 567"))
 ((:path . "fas") (:content . "Farsi test suite from Ling 471"))
 ((:path . "fin") (:content . "Finnish test suite from Ling 567"))
 ((:path . "hau") (:content . "Hausa test suite from Ling 567"))
 ((:path . "hye") (:content . "Armenian test suite from Ling 567"))
 ((:path . "isl") (:content . "Icelandic test suite from Ling 567"))
 ((:path . "ita") (:content . "Italian test suite from Ling 567"))
 ((:path . "zul") (:content . "Zulu test suite from Ling 567"))
 ((:path . "eng_mmt") (:content . "English MMT test sentences"))
 ((:path . "epo_mmt") (:content . "Esperanto MMT test sentences"))
 ((:path . "fas_mmt") (:content . "Farsi MMT test sentences"))
 ((:path . "fin_mmt") (:content . "Finnish MMT test sentences"))
 ((:path . "hau_mmt") (:content . "Hausa MMT test sentences"))
 ((:path . "heb_mmt") (:content . "Hebrew MMT test sentences"))
 ((:path . "hye_mmt") (:content . "Armenian MMT test sentences"))
 ((:path . "isl_mmt") (:content . "Icelandic MMT test sentences"))
 ((:path . "ita_mmt") (:content . "Italian MMT test sentences"))
 ((:path . "zul_mmt") (:content . "Zulu MMT test sentences"))
 )
