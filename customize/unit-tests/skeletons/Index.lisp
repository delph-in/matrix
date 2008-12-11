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
((:path . "tiniest") (:content . "tiniest: A very basic grammar just to get the unit tests started.  SOV word order, no frills."))
 ;;; new-unit-test-here
((:path . "vso-aux-before-v-no-cluster") (:content . "vso-aux-before-v-no-cluster: "test vso word order where aux precedes its v-complement and does not form a verbal cluster""))
((:path . "vso-aux-before-v-cluster") (:content . "vso-aux-before-v-cluster: "testing vso word order where the aux precedes its v-comp forming a verbal cluster""))
((:path . "vso-aux-after-v") (:content . "vso-aux-after-v: "testing vso word order where the auxiliary follows its v-complement""))
((:path . "v-initial-aux-before-v") (:content . "v-initial-aux-before-v: "testing v-initial word order with aux preceding its v-complement""))
((:path . "v-initial-aux-after-v") (:content . "v-initial-aux-after-v: "testing v-initial word order when the auxiliary follows its v-complement""))
((:path . "v-initial-aux-before-vp") (:content . "v-initial-aux-before-vp: "testing v-initial word order where the auxiliary precedes its vp complement""))
((:path . "v-initial-aux-after-vp") (:content . "v-initial-aux-after-vp: "testing v-initial word order with auxiliaries following their vp-complement""))
((:path . "svo-aux-before-v") (:content . "svo-aux-before-v: "testing svo word order with auxiliary preceding v-comp""))
((:path . "svo-aux-after-v") (:content . "svo-aux-after-v: "testing svo word order with aux following its v-complement""))
((:path . "sov-aux-before-v") (:content . "sov-aux-before-v: "testing sov word order with auxiliaries preceding their v-comp""))
((:path . "sov-aux-after-v") (:content . "sov-aux-after-v: "testing sov word order where auxiliaries precede v-comp""))
((:path . "free-aux-before-v-cluster") (:content . "free-aux-before-v-cluster: "testing free word order with preceding auxiliaries that take a v-comp and form a v-cluster""))
((:path . "free-aux-after-v-one-aux") (:content . "free-aux-after-v-one-aux: "testing free word order with v-comp and no cluster forming, maximum one auxiliary should be allowed no matter what the order is""))
((:path . "free-aux-after-v-cluster") (:content . "free-aux-after-v-cluster: testing free word order where the aux follows its vcomp and forms verbal clusters"))
((:path . "vso-aux-before-vp") (:content . "vso-aux-before-vp: testing vso word order where auxiliary precedes main-verb and takes vso complement style Irish and Welsh"))
((:path . "multi-featured-aux") (:content . "multi-featured-aux: very! short test of features on the auxiliary itself"))
((:path . "free-aux-before-vp") (:content . "free-aux-before-vp: testing free word order with auxiliary that needs to come before its verbal complements"))
((:path . "free-aux-after-vp") (:content . "free-aux-after-vp: testing word order with auxiliaries following their vp-compl, else free word order"))
((:path . "vos-aux-after-vp") (:content . "vos-aux-after-vp: testing word order for vos languages in which the auxiliary follows its vp complement"))
((:path . "vos-aux-before-vp") (:content . "vos-aux-before-vp: testing word order for vos languages with auxiliaries preceding their vp complement"))
((:path . "ovs-aux-before-vp") (:content . "ovs-aux-before-vp: tests word order ovs with auxiliaries that precede their vp complement"))
((:path . "ovs-aux-after-vp") (:content . "ovs-aux-after-vp: testing ovs word order with auxiliaries that follow their vp complement"))
((:path . "aux-assigns-subj-case-vp") (:content . "aux-assigns-subj-case-vp: grammar tests case restrictions imposed by auxiliary: here the auxiliary aentence"))
((:path . "vos-vp-aux-case-rais") (:content . "vos-vp-aux-case-rais: testing case constraints imposed by the auxiliary: here it raises the case value from its vcomp's subject"))
((:path . "v2-aux-eitherside-vp") (:content . "v2-aux-eitherside-vp: v2nd word order with one auxiliary that takes a vp complement on either side"))
((:path . "v2-aux-eitherside-v") (:content . "v2-aux-eitherside-v: v2nd word order with one auxiliary that takes a v (not vp) complement which can appear freely on either side ofthe aux"))
((:path . "multi-select-case") (:content . "multi-select-case: multi-select of feature values, namely case"))
((:path . "nf-twoforms-withtwoaux-vp-1008") (:content . "nf-twoforms-withtwoaux-vp-1008: testing-- two nonfinite forms"))
((:path . "nf-form-withaux-vp-1008") (:content . "nf-form-withaux-vp-1008: testing nonfinite form constraint for aux complement"))
((:path . "fin-forms-noaux-1008") (:content . "fin-forms-noaux-1008: testing finite/nonfinite distinction when there are no auxiliaries"))
((:path . "auxcomp-markfeature-vp-1015") (:content . "auxcomp-markfeature-vp-1015: testing KEYS.KEY mhat is being used to distinguish etre and avoir type auxiliary verb classes"))
((:path . "auxcomp-feature-engstative-vp-1013") (:content . "auxcomp-feature-engstative-vp-1013: testing verb class feature stative and aux constrained to nonstative"))
((:path . "noaux-toblig-aopt-onv") (:content . "noaux-toblig-aopt-onv: obligatory tense, optional aspect on v, no aux level 3 test"))
((:path . "auxten-vpcompnfasp-tafeat") (:content . "auxten-vpcompnfasp-tafeat: tensed auxilialevel 3 test"))
((:path . "nopaux-noinfl-vpcomp-f-formfeat") (:content . "nopaux-noinfl-vpcomp-f-formfeat: single uninflected aux with finite vp complement, level 2 test (inflection but no t&a features)"))
((:path . "aux-f-vpcomp-nfconst-formfeat") (:content . "aux-f-vpcomp-nfconst-formfeat: finite auxes with nf vp compleof the nf comp is constrained - level 2 test (inflection, no t&a features)"))
((:path . "aux-v-f-noinfl") (:content . "aux-v-f-noinfl: two aux, finite v vomp, no inflection, level 1 test"))
((:path . "aux-vp-f-noinfl") (:content . "aux-vp-f-noinfl: two aux, finite vp comp, no inflection, level 1 test"))
((:path . "aux-s-f-noinfl") (:content . "aux-s-f-noinfl: two aux, finite sentential compelement, no inflection, level1 test"))
((:path . "testingcompforms") (:content . "testingcompforms: small ts - known problems"))
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
 )
