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
((:path . "german-artificial-basic-compl") (:content . "german-artificial-basic-compl: acl submitted version basic analysis for german complete set"))
((:path . "dutch-auxrule-compl") (:content . "dutch-auxrule-compl: artificial dutch grammarwith auxrule complete"))
((:path . "mood-buildhierarchy") (:content . "mood-buildhierarchy: testing the build your own mood hierarchy"))
((:path . "asp-mood-contrast-options") (:content . "asp-mood-contrast-options: tests options to add simple imper/perf and subj/ind hierarchies"))
((:path . "subj-v-inv-obj-drop") (:content . "subj-v-inv-obj-drop: Testing interaction of subj-v inversion and object drop, as well as interaction of subj-v intersion and FORM and AUX features."))
((:path . "eng-qpart-inf") (:content . "eng-qpart-inf: Testing interaction of non-finite form and question particle.  Question particle should now only attach to finite sentences."))
((:path . "infl-q-nonfinal-suffix") (:content . "infl-q-nonfinal-suffix: Question affix as non-final in suffix string."))
((:path . "infl-q-final-opt-suffix") (:content . "infl-q-final-opt-suffix: Question inflection as optional suffix after one other suffix"))
((:path . "infl-q-aux-verb") (:content . "infl-q-aux-verb: Questions as inflection on aux verbs."))
((:path . "infl-q-main-verb-prefix") (:content . "infl-q-main-verb-prefix: Questions as prefix on main verbs."))
((:path . "infl-q-main-verb") (:content . "infl-q-main-verb: Question marking as inflection on the main verb, no other affixes"))
((:path . "subj-aux-inv-q") (:content . "subj-aux-inv-q: Questions marked by subj-aux inversion."))
((:path . "qpart-yes-no") (:content . "qpart-yes-no: Questions marked via sentence-initial question particle."))
((:path . "infl-neg-sole-suffix") (:content . "infl-neg-sole-suffix: Negation as the only suffix, optional"))
((:path . "infl-neg-nonfinal-suffix") (:content . "infl-neg-nonfinal-suffix: Inflectional negation as optional non-final suffix, optionality with check box"))
((:path . "infl-neg-final-opt-suffix") (:content . "infl-neg-final-opt-suffix: Negative affix as optional suffix at end of suffix string"))
((:path . "Sahaptin-short") (:content . "Sahaptin-short: Sahaptin, shorter version (sfd dissertation)"))
((:path . "Tagalog") (:content . "Tagalog: Tagalog (sfd dissertation)"))
((:path . "Cree") (:content . "Cree: Plains Cree (sfd dissertation)"))
((:path . "subj-drop") (:content . "All subjects caobjects can be dropped.  No markers for subject dropping."))
((:path . "auxfeatures-onlyformmarked") (:content . "auxfeatures-onlyformmarked: testing features on the auxiliaryFORM marked on the complement verb"))
((:path . "compfeatures-onlyformmarked") (:content . "compfeatures-onlyformmarked: testing features on auxiliary complement but only FORM is marked on the verb"))
((:path . "compfeatures") (:content . "compfeatures: testing features on the auxiliary complement"))
((:path . "auxfeatures") (:content . "auxfeatures: testing features on auxiliary verb"))
((:path . "case-optadp") (:content . "case-optadp: case with optional adpositional marking"))
((:path . "Zulu") (:content . "Zulu: Zulu test for morphotactics"))
((:path . "pre-vp-adv-neg") (:content . "pre-vp-adv-neg: Negation as independent pre-VP modifier"))
((:path . "mini-infl-neg") (:content . "mini-infl-neg: Negation as inflection, simple case"))
((:path . "test-stative") (:content . "test-stative: testing assignment of prog-asp to non-stative-ing comp, ignoring tense/person/number/spelling, only transitive"))
((:path . "view-situ-aspect-infl") (:content . "view-situ-aspect-infl: situation and viewpoint aspect as inflection"))
((:path . "view-inf-situ-inher") (:content . "view-inf-situ-inher: inflected viewpoint and inherent situation aspect"))
((:path . "multi-featured-aux") (:content . "multi-featured-aux: very! short test of features on the auxiliary itself"))
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
((:path . "dir-inv-fore") (:content . "dir-inv-fore: Direct-inverse, pseudo-Fore"))
((:path . "dir-inv-algonquian") (:content . "dir-inv-algonquian: Direct-inverse, pseudo-Algonquian"))
((:path . "case-split-s") (:content . "case-split-s: Case, split-S"))
((:path . "case-none") (:content . "case-none: Case, none"))
((:path . "case-nom-acc-adp") (:content . "case-nom-acc-adp: Case, nominative-accusative w/ adpositions"))
((:path . "case-focus") (:content . "case-focus: Case, focus"))
((:path . "case-fluid-s") (:content . "case-fluid-s: Case, fluid-S"))
