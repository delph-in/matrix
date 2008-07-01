;;; ERB 2008-06-30

;;; Function for creating unit tests for
;;; matrix developers.  Usage:

;;; Make sure your .lkbrc defines the variable
;;; *customization-root* to point to the direcotry
;;; which contains the version of customize.py
;;; you wish to test, e.g.:

; (defparameter *customization-root* "~/lingo/grammars/matrix/customize/")

;;; Invoke this from within the *common-lisp* buffer
;;; while running the lkb, as follows:

;;; (create-matrix-unit-test "path-to-txt-suite" "path-to-choices")

;;; TODO: Also make one for running existing unit tests.

(in-package :lkb)

(defun create-matrix-unit-test (txt-suite choices)

  ;;; Check to make sure files are where they should be
  
  (check-unit-test-inputs txt-suite choices)
  
  ;;; Create grammar and store in unit-tests/grammars

  (let* ((lg-name (get-language-name choices))
	 (cmd (format nil "~a~a ~a ~a ~a~a~a" 
		      *customization-root* "unit-tests/call-customize"
		      *customization-root* 
		      choices *customization-root* "unit-tests/grammars/"
		      lg-name)))
    (excl:run-shell-command cmd))
  
  ;;; Load grammar into the LKB

  (let* ((lg-name (get-language-name choices))
	 (script (format nil "~a~a~a~a"
			*customization-root*
			"unit-tests/grammars/"
			lg-name
			"/matrix/lkb/script")))
    (read-script-file-aux script))
  
  ;;; Check that [incr tsdb()] is running, and if not
  ;;; start it up:
    
  (unless (find-package :tsdb)
    (load-system "tsdb"))
    
  ;;; Set *tsdb-home* to unit-tests/home/

  (let ((home (format nil "~a~a"
		       *customization-root*
		       "unit-tests/home/")))
	(setf tsdb::*tsdb-home* home))
  
  (tsdb::send-to-podium "tsdb_update all" :wait t)
  (tsdb::send-to-podium (format nil "set globals(home) ~a" tsdb::*tsdb-home*))
  
  ;;; Import test items

  ; directory name for profile (name is relative to *tsdb-home*)
  ; fix-me: this should have timestamnp in it.
  
  (let* ((lg-name (get-language-name choices))
	 (profile-directory (format nil "current/~a" lg-name)))
  
    ; add profile

    (tsdb::do-import-items txt-suite profile-directory :format :ascii)
  
  
    ;;; Process all items
  
    (tsdb::tsdb-do-process profile-directory))
  
  ;;; Update status.  Why does it sometimes seem to need both of these?

  (tsdb::send-to-podium "tsdb_update all" :wait t)
  (tsdb::send-to-podium (format nil "set globals(home) ~a" tsdb::*tsdb-home*) :wait t)
 
  ;;; Kick open browse-results window

  (tsdb::send-to-podium "tsdb_browse results" :wait t)
    
  ;;; It would be nice to also print out results of analyze
  ;;; coverage/overgeneration to lisp buffer.
)

(defun check-unit-test-inputs (txt-suite choices)

    ;;; Check that *customization-root* is defined.
    ;;; if only I knew how to do that.
    
    ;;; Check path names
    
    (if (fad:file-exists-p txt-suite)
	nil
      (error "No txt-suite at path ~S." txt-suite))
  
    (if (fad:file-exists-p choices)
	nil
      (error "No choices file at path ~S." choices))
    
    (let ((cust (make-pathname :directory *customization-root* :name "customize.py")))
      (if (fad:file-exists-p cust)
	  nil
	(error "Invalid customization root: ~S" *customization-root*)))
    
    ;;; Check language name from choices file and make sure
    ;;; that it isn't already in use among unit-tests and/or
    ;;; that there isn't already a grammar by that name in the
    ;;; grammars/ directory.
    
    (let* ((lg-name (get-language-name choices))
	   (choices-path (make-pathname 
		  :directory 
		  (pathname-directory 
		   (dir-append *customization-root* 
			       '(:relative "unit-tests" "choices"))) 
		  :name lg-name))
	   (grammar-path (make-pathname 
		  :directory 
		  (pathname-directory 
		   (dir-append *customization-root* 
			       '(:relative "unit-tests" "grammars"))) 
		  :name lg-name))
	   )
      (if (fad:file-exists-p choices-path)
	  (error "A unit test with name ~S already exists.  Create a new choices file with a different language name and start again.  Or, if you wanted to run that existing unit test, use run-unit-test." lg-name))
      (if (fad:file-exists-p grammar-path)
	  (error "A grammar with name ~S already exists in unit-tests/grammars. Please move it first before invoking this function." lg-name))))


(defun get-language-name (choices)

  ;;; Extract language name from choices file.
  ;;; Assume it's stored in a line that says language=name
  
  (let ((lg-name nil))
    (with-open-file (stream choices)
      (loop for line = (read-line stream nil)
	  until (eq line nil)
	  do (if (and (> (length line) 9)
		      (equal (subseq line 0 9) "language="))
		 (setf lg-name (subseq line 9)))))
    
    
    (if lg-name
	lg-name
      (error "Invalid choices file: No language name given."))))
