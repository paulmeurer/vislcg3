(in-package :vislcg3)

(cg3-init (fdopen 0 "r") (fdopen 1 "w") (fdopen 1 "w"))

#+test
(defparameter *grammar* (print (cg3-grammar-load "/Users/paul/lisp/projects/georgian-corpus/cg3/kat-dis.rle")))

;;input stream is in CG3 format
(defun cg3-disambiguate-string (grammar stream &key print-fn)
  (let* ((applicator (cg3-applicator-create grammar))
	 (sentence (cg3-sentence-new applicator))
	 (cohort nil))
    (unwind-protect
	 (progn
	   (u:with-stream-lines (stream line :end-line :eof)
	     (cond ((eq line :eof)
		    ;; add the last cohort
		    (when cohort (cg3-sentence-addcohort sentence cohort)))
		   ((string= line "")
		    nil)
		   ((char= (char line 0) #\")
		    (when cohort
		      ;; add the previous cohort
		      (cg3-sentence-addcohort sentence cohort))
		    (setf cohort (cg3-cohort-create sentence))
		    (let ((tag (cg3-tag-create-u8 applicator line)))
		      (cg3-cohort-setwordform cohort tag)))
		   ((null cohort)
		    nil)
		   (t
		    (let* ((reading (cg3-reading-create cohort))
			   (line (string-trim #(#\space #\tab) line))
			   (lemma-end (position #\" line :from-end t)) ;; lemma can be MWE
			   (lemma (subseq line 0 (1+ lemma-end))) 
			   (tags (u:split line #\space nil nil t nil (1+ lemma-end))))
		      ;;(print (list line lemma tags))
		      (let ((lemma (cg3-tag-create-u8 applicator lemma)))
			(cg3-reading-addtag reading lemma))
		      (dolist (tag tags)
			(let ((tag (cg3-tag-create-u8 applicator tag)))
			  (cg3-reading-addtag reading tag)))
		      (cg3-cohort-addreading cohort reading)))))
	   (cg3-sentence-runrules applicator sentence)
	   (dotimes (coh (cg3-sentence-numcohorts sentence))
	     (let* ((cohort (cg3-sentence-getcohort sentence coh))
		    (tag (cg3-cohort-getwordform cohort))
		    (word (cg3-tag-gettext-u8 tag)))
	       (cond (print-fn
		      (funcall print-fn
			       (cons (if (string= word ">>>")
					 :sentence
					 (subseq word 2 (- (length word) 2)))
				     (collecting
				       (dotimes (re (cg3-cohort-numreadings cohort))
					 (let* ((reading (cg3-cohort-getreading cohort re))
						(reading (collecting
							   (dotimes (ta (1- (cg3-reading-numtags reading)))
							     (let* ((tag (cg3-reading-gettag reading (1+ ta)))
								    (tag (cg3-tag-gettext-u8 tag)))
							       (cond ((string= tag "")
								      nil)
								     ((and (zerop ta) (>= (length tag) 2))
								      (collect (subseq tag 1 (1- (length tag)))))
								     (t
								      (collect tag))))))))
					   (when reading (collect reading))))))))
		     (t
		      (write-line word)
		      (dotimes (re (cg3-cohort-numreadings cohort))
			(write-char #\tab)
			(let ((reading (cg3-cohort-getreading cohort re)))
			  (dotimes (ta (1- (cg3-reading-numtags reading)))
			    (let* ((tag (cg3-reading-gettag reading (1+ ta)))
				   (tag (cg3-tag-gettext-u8 tag)))
			      (format t "~a " tag))))
			(terpri)))))))
      (cg3-sentence-free sentence)
      (cg3-applicator-free applicator))))

;; TODO: make all three a keyword
;; If print-fn is NIL output goes to *standard-output* in traditional CG format
(defun cg3-parse-text (grammar text tokenize-fn analyze-fn &key print-fn)
  (let* ((applicator (cg3-applicator-create grammar))
	 (sentence (cg3-sentence-new applicator)))
    (unwind-protect
	 (progn
	   (dolist (word (funcall tokenize-fn text))
	     (let* ((cohort (cg3-cohort-create sentence))
		    ;; used to include features coming from the tokenizer
		    (w (if (listp word) (car word) word))
		    (tag (cg3-tag-create-u8 applicator (format nil "\"<~a>\"" w)))
		    (t-f (when (listp word) (cadr word))))
	       (cg3-cohort-setwordform cohort tag)
	       ;; tags is (lemma . features)
	       (dolist (tags (funcall analyze-fn w))
		 (let ((reading (cg3-reading-create cohort)))
		   (let ((tag (cg3-tag-create-u8 applicator (format nil "\"~a\"" (car tags)))))
		     (cg3-reading-addtag reading tag))
		   (dolist (tag (cdr tags))
		     (let ((tag (cg3-tag-create-u8 applicator tag)))
		       (cg3-reading-addtag reading tag)))
		   (when t-f
		     (let ((tag (cg3-tag-create-u8 applicator t-f)))
		       (cg3-reading-addtag reading tag)))
		   (cg3-cohort-addreading cohort reading)))
	       (cg3-sentence-addcohort sentence cohort)))
	   (cg3-sentence-runrules applicator sentence)
	   (dotimes (coh (cg3-sentence-numcohorts sentence))
	     (let* ((cohort (cg3-sentence-getcohort sentence coh))
		    (tag (cg3-cohort-getwordform cohort))
		    (word (cg3-tag-gettext-u8 tag)))
	       (cond (print-fn
		      (funcall print-fn
			       (cons (if (string= word ">>>")
					 :sentence
					 (subseq word 2 (- (length word) 2)))
				     (collecting
				       (dotimes (re (cg3-cohort-numreadings cohort))
					 (let* ((reading (cg3-cohort-getreading cohort re))
						(reading (collecting
							   (dotimes (ta (1- (cg3-reading-numtags reading)))
							     (let* ((tag (cg3-reading-gettag reading (1+ ta)))
								    (tag (cg3-tag-gettext-u8 tag)))
							       (cond ((string= tag "")
								      nil)
								     ((and (zerop ta) (>= (length tag) 2))
								      (collect (subseq tag 1 (1- (length tag)))))
								     (t
								      (collect tag))))))))
					   (when reading (collect reading))))))))
		     (t
		      (write-line word)
		      (dotimes (re (cg3-cohort-numreadings cohort))
			(write-char #\tab)
			(let ((reading (cg3-cohort-getreading cohort re)))
			  (dotimes (ta (1- (cg3-reading-numtags reading)))
			    (let* ((tag (cg3-reading-gettag reading (1+ ta)))
				   (tag (cg3-tag-gettext-u8 tag)))
			      (format t "~a " tag))))
			(terpri)))))))
      (cg3-sentence-free sentence)
      (cg3-applicator-free applicator))))

;; parses one sentence and outputs as vertical file
(defun cg3-parse-text-vert (grammar tokens stream analyze-fn global-atts
			    &key (word-fn #'identity) ;; gives wordform when applied to token
			    (max-readings 6))
  (let* ((applicator (cg3-applicator-create grammar))
	 (sentence (cg3-sentence-new applicator)))
    (unwind-protect
	 (progn
	   (dolist (token tokens)
	     (let ((cohort (cg3-cohort-create sentence))
		   (tag (cg3-tag-create-u8 applicator (format nil "\"<~a>\"" (funcall word-fn token)))))
	       (cg3-cohort-setwordform cohort tag)
	       (dolist (tags (funcall analyze-fn token))
		 (let ((reading (cg3-reading-create cohort)))
		   (let ((tag (cg3-tag-create-u8 applicator (format nil "\"~a\"" (car tags)))))
		     (cg3-reading-addtag reading tag))
		   (dolist (tag (cdr tags))
		     (let ((tag (cg3-tag-create-u8 applicator tag)))
		       (cg3-reading-addtag reading tag)))
		   (cg3-cohort-addreading cohort reading)))
	       (cg3-sentence-addcohort sentence cohort)))
	   (cg3-sentence-runrules applicator sentence)
	   (dotimes (coh (cg3-sentence-numcohorts sentence))
	     (let* ((cohort (cg3-sentence-getcohort sentence coh))
		    (tag (cg3-cohort-getwordform cohort))
		    (word (cg3-tag-gettext-u8 tag)))
	       (unless (string= word ">>>")
		 ;; word
		 (write-string (subseq word 2 (- (length word) 2)) stream)
		 (write-char #\tab stream)
		 ;; lemma
		 (let ((lemmas ()))
		   (dotimes (re (cg3-cohort-numreadings cohort))
		     (let* ((reading (cg3-cohort-getreading cohort re))
			    (tag (cg3-reading-gettag reading 1))
			    (lemma (cg3-tag-gettext-u8 tag)))
		       (pushnew (subseq lemma 1 (1- (length lemma))) lemmas :test #'string=)))
		   (format stream "~{|~a~}|" lemmas))
		 (write-char #\tab stream)
		 ;; features
		 (let ((readings ()))
		   (dotimes (re (cg3-cohort-numreadings cohort))
		     (let* ((reading (cg3-cohort-getreading cohort re))
			    (lf (collecting
				  (dotimes (ta (- (cg3-reading-numtags reading) 2))
				    (let* ((tag (cg3-reading-gettag reading (+ ta 2)))
					   (tag (cg3-tag-gettext-u8 tag)))
				      (unless (or (and (char= (char tag 0) #\<)
						      (char= (char tag (1- (length tag))) #\>))
						  (find tag '("Base" "Passive" "Causative" "Full" "Reduced")
							:test #'string=))
					(collect tag)))))))
		       (pushnew (format nil " ~{~a ~}" lf) readings :test #'string=)))
		   (setf readings
			 (if (and max-readings (> (length readings) max-readings))
			     (last readings max-readings)
			     readings))
		   (format stream "~{|~a~}|" readings)
		   (format stream "~{	~a~}~%" global-atts))))))
      (cg3-sentence-free sentence)
      (cg3-applicator-free applicator))))

#+test
(cg3-cleanup)

:eof
