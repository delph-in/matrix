;;; David Goss-Grubbs December 2004
;;; Read in a transducer output in prolog format by XFST.
;;; Run transducer to map surface <-> underlying forms

;;; types for a transducer

;;; A transducer is an Epsilon alphabet and a transition table.

(in-package :lkb)

(defstruct transducer
	epsilon				; a list of strings
	table				; a vector of states
)


;;; A fst-state contains an indicator of whether it is a final state,
;;; and a list of transitions.
;;; The fst-state itself doesn't have a label - it is implicit, its
;;; index in the transition table

(defstruct fst-state
	final				; t or nil
	trans				; a list of transitions
)


;;; a transition contains a label and a destination

(defstruct transition
  label					; a cons cell - car = upper symbol, 
                                        ;               cdr = lower symbol
					;  or a single symbol
  dest					; an integer index into the 
                                        ;  transition table
)


;;; 


(defvar *transducers* nil)		; An association list. The index is
					; a network label and the datum is
					; a transducer

(defvar *active-transducer*		; The active transducer. This defaults
    (make-transducer			; to the identity transducer.
     :epsilon nil 
     :table 
     (make-array 1 
		 :initial-element 
		 (make-fst-state :final t
			     :trans (list (make-transition :label "?"
							   :dest 0))))))


;;;
;;; Functions for reading in a .pro transducer network
;;;

;;; load-xducer-network
;;;
;;; Read through a network file, filling in the transducer information
;;;
;;; pathname - the pathname to the .pro file to read in

(defun load-xducer-network (pathname)
  (let (infile line-string instring entry-type)
    ;; create transducers of the appropriate size
  (init-transducers pathname)
  (setq infile (open pathname :direction :input))
  ;; read each line of the file
  (setq line-string (read-line infile nil 'eof))
  (do ()
      ((eq line-string 'eof)
       (close infile))
      (setq instring (make-string-input-stream line-string))
      (setq entry-type (read instring nil 'no-entry))

    ;; take action depending on whether the first expression on the line
    ;; is 'symbol' 'arc' or 'final'
      (cond ((eq entry-type 'symbol)
	     (load-xducer-symbol instring))
	    ((eq entry-type 'arc)
	     (load-xducer-arc instring))
	    ((eq entry-type 'final)
	     (load-xducer-final instring)))

      (close instring)
      (setq line-string (read-line infile nil 'eof)))
))

;;; init-transducers
;;;
;;; Find out how many states each transducer in the network will have, and 
;;; create transition tables of the resulting size.
;;;
;;; pathname - the pathname of the .pro file where the network lives
;;;
;;; side effect: the global variables *transducers* and *active-transducer*
;;;              are set

(defun init-transducers (pathname)
(setq *transducers* nil)
(mapcar
 #'(lambda (table-length)
     (let (table xducer)
       (setq table (make-array (+ (cdr table-length) 1) :element-type 'fst-state))
       (do ((i 0 (+ i 1)))
	   ((eq i (+ (cdr table-length) 1)))
	 (setf (aref table i) (make-fst-state :final nil :trans nil)))
       (setq xducer (make-transducer :epsilon nil :table table))
       ;;; EB: this line had just 'transducers' instead of *transducers* at the
       ;;; end, and lisp was complaining that it was unbound.
       (setq *transducers* (cons (cons (car table-length) xducer) *transducers*))
       (setq *active-transducer* (cdar *transducers*))))
 (read-table-sizes pathname)))

;;; read-table-sizes
;;;
;;; Read through a first pass, counting how many arcs there are for each
;;; network.
;;;
;;; pathname - the pathname of the .pro file that contains the network.
;;;
;;; Returns: an association list where the index is a network label and the
;;;          datum is the number of arcs in the transducer with that label.

(defun read-table-sizes (pathname)
(let (infile line-string instring entry-type networks xducer end-state pair)

  (setq infile (open pathname :direction :input))
  (setq networks nil)
  (setq line-string (read-line infile nil 'eof))
  ;; read each line in the file
  (do ()
      ((eq line-string 'eof)
       (close infile)
       networks)
      (setq instring (make-string-input-stream line-string))
      (setq entry-type (read instring nil 'no-entry))

    ;; if the line starts with 'network' add a new network to the
    ;; return value
      (cond ((eq entry-type 'network)
	     (read-char instring)
	     (setq networks (cons (cons (read instring) 0) networks)))
	    ;; if the line starts with 'arc', and its end-state is the highest
	    ;; one so far, make that the size of the network.
	    ((eq entry-type 'arc)
	     (read-char instring)
	     (setq xducer (read instring))
	     (read-char instring)
	     (read instring)
	     (read-char instring)
	     (setq end-state (read instring))
	     (cond ((> end-state (cdr (setq pair (assoc xducer networks))))
		    (rplacd pair end-state)))))

      (close instring)
      (setq line-string (read-line infile nil 'eof))
)))

;;; load-xducer-symbol
;;;
;;; Read in a "symbol" line and add the symbol to the transducer's
;;; epsilon alphabet.
;;;
;;; instring - an input stream representing the line being read

(defun load-xducer-symbol (instring)
(let (network symbol xducer)
  (read-char instring)
  (setq network (read instring))
  (read-char instring)
  (setq symbol (read instring))
  (setq xducer (cdr (assoc network *transducers*)))
  (add-to-epsilon xducer symbol)
))

;;; add-to-epsilon
;;;
;;; Add a symbol to the epsilon alphabet of a transducer, if it's not there
;;; already.
;;;
;;; xducer - the transducer
;;; symbol - the symbol to be added

(defun add-to-epsilon (xducer symbol)
  (cond ((and (not (equal symbol "?"))	; don't add a "?" from an arc label
	      (not (member symbol (transducer-epsilon xducer) :test #'equal)))
	 (setf (transducer-epsilon xducer) 
	   (cons symbol (transducer-epsilon xducer)))))
)

;;; load-xducer-arc
;;;
;;; Read in an 'arc' line from a .pro file, and add a corresponding arc
;;; to the appropriate transducer.
;;;
;;; instring - an input stream representing the line being read

(defun load-xducer-arc (instring)
(let (network start-state end-state label1 label2 colon)
  (read-char instring)
  (setq network (read instring))
  (read-char instring)
  (setq start-state (read instring))
  (read-char instring)
  (setq end-state (read instring))
  (read-char instring)
  (setq label1 (read instring))
  (setq colon (read-char instring))
  ;; the label may be a single symbol, or a pair separated by ":"
  (cond ((eq colon #\:)
         (setq label2 (read instring))
         (add-arc network start-state end-state (cons label1 label2)))
        (t
         (add-arc network start-state end-state label1)))
))

;;; add-arc
;;;
;;; Add an arc to a transducer.
;;;
;;; network - a label/transducer pair. The transducer will get the new arc
;;; source  - the source state of the arc
;;; goal    - the goal state of the arc
;;; label   - the label, which is either a single string, or a cons whose
;;;             car and cdr are each strings

(defun add-arc (network source goal label)
(let (xducer xtable state)
  (setq xducer (cdr (assoc network *transducers*)))
  (setq xtable (transducer-table xducer))
  (setq state (aref xtable source))
  ;; if we haven't seen this result state yet, create a new state struct
  (cond ((null state)
	 (setq state (make-fst-state :final nil :trans nil))
	 (setf (aref xtable source) state)))
  (setf (fst-state-trans state) 
    (cons (make-transition :label label :dest goal) (fst-state-trans state)))
  ;; add the label(s) to the epsilon alphabet
  (cond ((listp label)
	 (add-to-epsilon xducer (car label))
	 (add-to-epsilon xducer (cdr label)))
	(t
	 (add-to-epsilon xducer label)))
))

;;; load-xducer-final
;;;
;;; Read in a 'final' line from a .pro file, and mark that state as being final
;;;
;;; instring - an input stream representing the line being read

(defun load-xducer-final (instring)
(let (network state-number xducer xtable state)
  (read-char instring)
  (setq network (read instring))
  (read-char instring)
  (setq state-number (read instring))
  (setq xducer (cdr (assoc network *transducers*)))
  (setq xtable (transducer-table xducer))
  (setq state (aref xtable state-number))
  (setf (fst-state-final state) t)
))

;;; xducer-tokenize
;;;
;;; Separate a string into its component symbols, relative to an epsilon
;;; alphabet. Tokenization proceeds left-to-right, and always takes the
;;; longest symbol it can
;;;
;;; string  - the string to tokenize
;;; epsilon - the epsilon alphabet
;;;
;;; Returns: the list of symbols

(defun xducer-tokenize (string epsilon)
(let (start length ret done subseq)
  (setq start 0)
  (setq length (length string))
  (setq ret nil)
  
  ;; Loop through until the starting point is past the end of the string
  (do ()
      ((eq start length))
      ;; Loop backward from the end of the string until you find a match or
      ;; you reach a single character
      (setq done nil)
      (do ((end length (- end 1)))
          ((eq done t))
          
          (setq subseq (subseq string start end))
          (cond ((or (eq start (- end 1))
					 (member subseq epsilon :test 'equal))
				 (setq ret (append ret (list subseq)))
				 (setq done t)
				 (setq start end)))
	))
	ret
))


;;;
;;; Functions to transduce one string to another
;;;

;;; transduce
;;;
;;; Run a string through a transducer
;;;
;;; direction - 'down' means get a surface form from an underlying form.
;;;               otherwise, get an underlying form for a surface form.
;;; string    - the string to transduce
;;;
;;; Returns: the list of output strings

(defun transduce (direction string)
(let (xducer epsilon table agenda ret entry current-state new-entry)
  (setq xducer *active-transducer*)
  (setq epsilon (transducer-epsilon xducer))
  (setq table (transducer-table xducer))
  ;; initialize the agenda by starting at state 0, with nothing recognized so
  ;; far, and with the whole (tokenized) input string to go.
  (setq agenda (list (list 0 nil (xducer-tokenize string epsilon))))
  (setq ret nil)
  
  ;;; Loop until there is nothing left on the agenda
  (do ()
      ((null agenda))
      
    (setq entry (pop agenda))
    (setq current-state (aref table (car entry)))
     
      ;; If this is a success state, save it off
    (cond ((and (null (caddr entry))
		(fst-state-final current-state))
	   (push (apply #'concatenate (cons 'string (cadr entry))) ret)))
      
      ;; Try to follow arcs
    (mapcar #'(lambda (arc)
		(cond ((setq new-entry 
			 (xduce-explore-arc arc direction epsilon entry))
		       (push new-entry agenda))))
	    (fst-state-trans current-state)))
  ret
))

;;; xduce-explore-arc
;;;
;;; If we can follow the arc, make a new agenda entry
;;;
;;; arc       - the arc to try
;;; direction - 'down' for underlying-to-surface, anything else for
;;;               surface-to-underlying
;;; epsilon   - the epsilon alphabet of the transducer
;;; entry     - the current agenda entry
;;;
;;; Returns: the new agenda entry, or nil if one couldn't be made

(defun xduce-explore-arc (arc direction epsilon entry)
(let (label inlabel outlabel output-so-far input-left ret-entry)
  (setq ret-entry nil)
  (setq output-so-far (cadr entry))
  (setq input-left (caddr entry))
  (setq label (transition-label arc))
  ;; See if the label has two parts
  (cond ((consp label)
	 (cond ((eq direction 'down)
		(setq inlabel (car label))
		(setq outlabel (cdr label)))
               (t
		(setq inlabel (cdr label))
		(setq outlabel (car label))))
	 (cond ((xduce-label-match inlabel (car input-left) epsilon)
		;; An empty string on the output side
		(cond ((equal outlabel "0")
		       (setq ret-entry 
			 (list (transition-dest arc) 
			       output-so-far 
			       (cdr input-left))))
		      ;; A regular symbol match
		      (t
		       (setq ret-entry 
			 (list (transition-dest arc) 
			       (append output-so-far (list outlabel)) 
			       (cdr input-left))))))
	       ;; An empty string on the input side
	       ((equal inlabel "0")
		(setq ret-entry (list (transition-dest arc) 
			    (append output-so-far (list outlabel)) 
			    input-left)))))
	;; the label is a single symbol
	((xduce-label-match label (car input-left) epsilon)
	 (setq ret-entry (list (transition-dest arc) 
		     (append output-so-far (list (car input-left))) 
		     (cdr input-left)))))
  ret-entry
))

;;; xduce-label-match
;;;
;;; See if a symbol on the label of an arc matches an input symbol
;;;
;;; label        - the symbol on the label of the arc
;;; input-symbol - the input symbol
;;; epsilon      - the epsilon alphabet of the transducer

(defun xduce-label-match (label input-symbol epsilon)
  ;; A straight string match will work, unless the input symbol is "?" or "0"
  (or (and (equal label input-symbol)
	   (not (equal input-symbol "?"))
	   (not (equal input-symbol "0")))
      ;; If the label is "?", match anything (except the empty string) not in 
      ;; the epsilon alphabet
      (and (equal label "?")
	   input-symbol
	   (not (member input-symbol epsilon :test 'equal)))
      ;; If the label is "%0" match an input symbol "0"
      (and (equal label "%0")
	   (equal input-symbol "0"))
      ;; If the label is "%?" match an input symbol "?"
      (and (equal label "%?")
	   (equal input-symbol "?")))
)