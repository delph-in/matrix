(in-package :lkb)

;;;
;;; identify characters that can form words; all other characters will create
;;; word boundaries and later be suppressed in tokenization.
;;;
(defun alphanumeric-or-extended-p (c)
  (and (graphic-char-p c) (not (member c *punctuation-characters*))))

;;;
;;; determine surface order of constituents in rule: returns list of paths into
;;; feature structure of rule, i.e. (nil (args first) (args rest first)) for a
;;; binary rule, where the first list element is the path to the mother node of
;;; the rule.
;;;
(defun establish-linear-precedence (rule)
  (let ((daughters
         (loop
             for args = (existing-dag-at-end-of rule '(args))
             then (existing-dag-at-end-of args *list-tail*)
             for daughter = (when args 
                              (get-value-at-end-of args *list-head*))
             for path = (list 'args) then (append path *list-tail*)
             while (and daughter (not (eq daughter 'no-way-through)))
             collect (append path *list-head*))))
    (if (null daughters)
      (cerror "Ignore it" "Rule without daughters")
      (cons nil daughters))))

;;;
;;; detect rules that have orthographemic variation associated to them; those
;;; who do should only be applied within the morphology system; for the time
;;; being use value of NEEDS-AFFIX feature, though it would be nicer to rely
;;; on a type distinction of lexical rules or re-entrancy of ORTH.
;;;
(defun spelling-change-rule-p (rule)
  (let ((affix (get-dag-value (tdfs-indef 
                               (rule-full-fs rule)) 'needs-affix)))
    (and affix (bool-value-true affix))))

;;;
;;; create feature structure representation of orthography value for insertion
;;; into the output structure of inflectional rules; somewhat more complicated
;;; than one might expect because of treatment for multi-word elements.
;;;
(defun make-orth-tdfs (orthography)
  (let* ((unifications
          (loop 
              for token in (split-into-words orthography)
              for path = *orth-path* then (append path *list-tail*)
              for opath = (create-path-from-feature-list 
                           (append path *list-head*))
              collect (make-unification :lhs opath                    
                                        :rhs (make-u-value :type token))))
         (indef (process-unifications unifications)))
    (when indef
      (make-tdfs :indef (create-wffs indef)))))

;;;
;;; assign priorities to parser tasks and lexical entries
;;;
(defun rule-priority (rule)
  (case (rule-id rule)
    (subj 1000)))

(defun gen-rule-priority (rule)
  (rule-priority rule))

(defun lex-priority (mrec)
  (declare (ignore mrec))
  800)

(defun gen-lex-priority (fs)
  (declare (ignore fs))
  800)

;;;
;;; determine path and file names for lexicon and leaf type cache files.
;;;
(defun set-temporary-lexicon-filenames nil
  (let* ((version (or (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user)
                      (and (find-package :lkb)
                           (find-symbol "*GRAMMAR-VERSION*" :lkb))))
         (prefix
          (if (and version (boundp version))
            (remove-if-not #'alphanumericp (symbol-value version))
            "lexicon")))
    (setf *psorts-temp-file* 
      (make-pathname :name prefix 
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *psorts-temp-index-file* 
      (make-pathname :name (concatenate 'string prefix ".idx") 
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *leaf-temp-file* 
      (make-pathname :name (concatenate 'string prefix ".lfs")
                     :directory (pathname-directory (lkb-tmp-dir))))))

(defun bool-value-true (fs)
  (and fs
       (let ((fs-type (type-of-fs fs)))
         (eql fs-type '+))))
  
(defun bool-value-false (fs)
  (and fs
       (let ((fs-type (type-of-fs fs)))
         (eql fs-type '-))))

;;; BELOW from user-fns-add.lsp for XFST interface

;;; ERB 2005-05-13
;;; The following definitions clobber existing LKB code to
;;; make use of David Goss-Grubb's tduce.lsp (running FSTs
;;; within the LKB). This is just a short term solution.  A
;;; general fix to LKB's morphological interface is forthcoming.

;;; Note the utility function cfw() (short for "check for words").
;;; This function takes a string as its sole argument and just
;;; checks whether all of the (lower-tape) words in the string
;;; can make it through the transducer.  Once you're running with
;;; the FST in place, if the FST doesn't recognize a word, it
;;; simply doesn't get to the LKB.  Thus cfw() is useful for
;;; debugging.

;;; Use tranducer loaded into the LKB for preprocessing
;;; Split string into words on white space, then run each
;;; word through the transducer to create a list of possibilities.
;;; Return a list of lists --- all possibilities for each word.
;;; It looks like parse() is already anticipating this possibility.

(defun preprocess-sentence-string (string)
  (let ((words (split-into-words string)))
    (loop for word in words
	collect (transduce nil word))))

;;; Utility function to test transducer for missing words.

(defun cfw (string)
  (let ((words (split-into-words string)))
    (loop for word in words
	do (format t "~a: ~a~%" word (transduce nil word)))))

;;; Try redefining add-morphs-to-morphs to handle input
;;; that encodes morphological ambiguity (i.e., a list of 
;;; lists of strings, instead of just a list of strings).

(defun add-morphs-to-morphs (preprocessed-input)
  (let ((current 0)
	(counter 0))

    ;;; preprocessed-input is a list of lists of strings.
    ;;; position is a list of strings representing the
    ;;; morphological analyses for that position.
    
    (dolist (position preprocessed-input)
      (let* ((cfrom current)
	     (cto (+ current 1)))
	(setf (aref *morphs* current)
	  (loop for token in position
	      collect (let* ((base-word token)
			     (word (string-upcase base-word))
			     (morph-poss 
			      (union
			       (filter-for-irregs
				(remove-duplicates
				 (morph-analyse word)
				 :test #'equalp))
			       ;; filter is in rules.lsp
			       (find-irregular-morphs word) :test #'equalp)))
			(unless #+:ltemplates (template-p word) #-:ltemplates nil
				(unless morph-poss 
				  (format t "~%Word `~A' is not in lexicon." word)
				  (when *unknown-word-types* 
				    (format t " Using unknown word mechanism.")))
				(setf counter (+ counter 1)))
			(when #-:null t #+:null (or morph-poss *unknown-word-types*)
			      (make-morph-edge :id counter :word base-word 
					       :morph-results 
					       (or morph-poss (list (list word)))
					       :cfrom cfrom
					       :cto cto))))))
      (incf current))))

;;; Need to redefine add-words-to-chart() as well, since current
;;; version in lkb assumes *morphs* to be an array storing one
;;; form per chart position.  Change this to an array of lists, with
;;; a list of forms per chart position.  Since the lkb assumes multiple
;;; possibilities for everything else about chart positions, this change
;;; appears to be enough.

(defun add-words-to-chart (f)
  (let ((current 0)
        (to-be-accounted-for (make-array (list *chart-limit*) 
                                          :initial-element nil)))
     ;; to-be-accounted for is needed because we cannot tell that a word is
     ;; impossible until after the whole sentence has been processed because
     ;; it may be part of a multi-word
     (loop
       (let ((morph-poss (aref *morphs* current)))
         (when (null morph-poss)
           (return nil))
         (incf current)
         
	 ;; *morphs* is an array of lists for this grammar
	 (loop for m-edge in morph-poss
	       do (multiple-value-bind (ind-results multi-strings)
		      (add-word (morph-edge-word m-edge)
				(morph-edge-morph-results m-edge) current
				f (morph-edge-cfrom m-edge)
				(morph-edge-cto m-edge))
		    (unless (or ind-results multi-strings)
		      (setf (aref to-be-accounted-for current)
			    (morph-edge-word m-edge)))
		    ;; record the fact we haven't analysed this word
		    (dolist (mstr multi-strings)
		      ;; wipe the record for multi-words which allow for it
		      (let ((words (split-into-words mstr)))
			(dotimes (x (length words))
			  (setf (aref to-be-accounted-for (- current x)) 
				nil)))))))
       (dotimes (y current)
	 (when (aref to-be-accounted-for y)
	   (format t "~%No sign can be constructed for `~(~a~)'" 
		   (aref to-be-accounted-for y)))))))
