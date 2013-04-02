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
((:path . "tiniest") (:content . "tiniest: A very basic grammar just to get the regression tests started.  SOV word order, no frills."))
 ;;; new-regression-test-here
((:path . "ger-wh-auxrule") (:content . "ger-wh-auxrule: Test checking semantics of wh-phrases (wer, wem, wen, wo, welche) using aux-rule"))
((:path . "ger-wh-argcomp") (:content . "ger-wh-argcomp: Small test verifying semantics of wh-phrases (welche, wer, wem, wen, wo)"))
((:path . "ger-copula-argcomp") (:content . "ger-copula-argcomp: Testing copula with adjective predicates and PP predicates (locatives) this test uses argcomp analysis"))
((:path . "ger-copula-auxrule") (:content . "ger-copula-auxrule: Testing behavior and semantics of copula verbs with locative prepositional phrases or adjectives as predicates"))
((:path . "ger-argcomp-adjectives") (:content . "ger-argcomp-adjectives: Tests correct behavior of adjectives including agreement with arg-comp analysis"))
((:path . "ger-auxrule-adjectives") (:content . "ger-auxrule-adjectives: Tests correct behavior of modifying adjectives, including agreement"))
((:path . "ger-raiscontr-auxrule") (:content . "ger-raiscontr-auxrule: Short test for object raising using auxrule analysis"))
((:path . "ger-raiscontr-argcomp") (:content . "ger-raiscontr-argcomp: Short test to see if object raising works"))
((:path . "German-auxrule") (:content . "German-auxrule: Regression test covering basic data from acl 2011 using auxrule analysis for German"))
((:path . "acl-basic-ger-argcomp") (:content . "acl-basic-ger-argcomp: l of German covering phenomena present for ACL 2011"))
((:path . "ger-subjcontr-auxrule") (:content . "ger-subjcontr-auxrule: Small test for subject control verbs with minor word order variations using auxrule analysis"))
((:path . "ger-subcontr-argcomp") (:content . "ger-subcontr-argcomp: Testing whether subject control works with arg-comp analysis and minor word order variations"))
((:path . "german-auxrule-compl") (:content . "german-auxrule-compl: German test suite covering ACL data using auxrule analysis"))
((:path . "german-argcomp-compl") (:content . "german-argcomp-compl: German test covering ACL2011 data using arg-comp analysis"))
((:path . "dutch-auxrule-compl") (:content . "dutch-auxrule-compl: Dutch regression test using auxrule analysis"))
((:path . "dutch-argcomp-compl") (:content . "dutch-argcomp-compl: ACL coverage test for Dutch using arg-comp analysis"))
 )