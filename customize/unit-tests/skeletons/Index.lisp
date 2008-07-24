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
 ;;; new-unit-test-here
((:path . "dir-inv-fore") (:content . "dir-inv-fore: Direct-inverse, pseudo-Fore"))
((:path . "dir-inv-algonquian") (:content . "dir-inv-algonquian: Direct-inverse, pseudo-Algonquian"))
((:path . "case-tripartite") (:content . "case-tripartite: Case, tripartite"))
((:path . "case-split-v") (:content . "case-split-v: Case, split-V"))
((:path . "case-split-s") (:content . "case-split-s: Case, split-S"))
((:path . "case-split-n") (:content . "case-split-n: Case, split-N"))
((:path . "case-none") (:content . "case-none: Case, none"))
((:path . "case-nom-acc-adp") (:content . "case-nom-acc-adp: Case, nominative-accusative w/ adpositions"))
((:path . "case-nom-acc") (:content . "case-nom-acc: Case, nominative-accusative"))
((:path . "case-focus") (:content . "case-focus: Case, focus"))
((:path . "case-fluid-s") (:content . "case-fluid-s: Case, fluid-S"))
((:path . "case-erg-abs") (:content . "case-erg-abs: Case, ergative-absolutive"))
((:path . "tiniest") (:content . "tiniest: A very basic grammar just to get the unit tests started.  SOV word order, no frills."))
 )
