(in-package "MRS")

(defparameter *initial-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT"))
  "Following this path into a sign gets you to the MRS structure")

(defparameter *psoa-liszt-path* (list (vsym "RELS") (vsym "LIST"))
  "path to get a liszt from a psoa")

(defparameter *ignored-sem-features* (list (vsym "LABEL"))
  "A list of features which are ignored completely")

(defparameter *main-semantics-path* 
  (append *initial-semantics-path* (list (vsym "RELS") (vsym "LIST")))
  "the path into a lexical entry which gives the list of
   relations - typically (append *initial-semantics-path* '(LISZT LIST))")

(defparameter *construction-semantics-path* 
  (list (vsym "C-CONT") (vsym "LISZT") (vsym "LIST"))
  "the path into a rule/construction which gives the
   semantics specific to that construction")

(defparameter *top-semantics-type* (vsym "RELATION")
  "the highest type in the hierarchy under which all
   rels are found")

;;;
;;; types for variable naming in mrsoutput (copy from `src/mrs/mrsglobals.lsp'
;;; but here to remind us to adapt them, as appropriate).
;;;
(defparameter *event-type* (vsym "event"))
(defparameter *event_or_index-type* (vsym "event_or_index"))
(defparameter *non_expl-ind-type* (vsym "non_expl-ind"))
(defparameter *eventtime-type* (vsym "eventtime"))
(defparameter *handle-type* (vsym "handle"))
(defparameter *group_lab-type* (vsym "group_lab"))
(defparameter *hole-type* (vsym "hole"))
(defparameter *label-type* (vsym "label"))
(defparameter *ref-ind-type* (vsym "ref-ind"))
(defparameter *full_ref-ind-type* (vsym "full_ref-ind"))
(defparameter *deg-ind-type* (vsym "deg-ind"))
(defparameter *individual-type* (vsym "individual"))
(defparameter *difference-list-type* (vsym "*diff-list*"))
(defparameter *conj-ind-type* (vsym "conj-ind"))

