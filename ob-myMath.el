; ob-myMath.el --- Babel Functions for myMath                -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2018 Free Software Foundation, Inc.

;; Author: Eric Schulte
;;	Dan Davison
;; Keywords: literate programming, reproducible research, myMath, statistics
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating myMath code

;;; Code:

(require 'cl-lib)
(require 'ob)
(require 'wolfram-mode)
(require 'ess-myMath)
(load-file "/msu/scratch2/m1gsa00/git/pubOrgmode/ess-myMath.el")
(declare-function run-wolfram "ext:wolfram-mode")
(declare-function wolfram-send-region "ext:wolfram-mode")

(defun org-babel-execute:myMath (body params)
  "Execute a block of myMath code.
This function is called by `org-babel-execute-src-block'."
(message "what")
(message body)
  (save-excursion
    (let* ((result-params (cdr (assq :result-params params)))
	   (result-type (cdr (assq :result-type params)))
           (session (org-babel-myMath-initiate-session
		     (cdr (assq :session params)) params))
	   (graphics-file (and (member "graphics" (assq :result-params params))
			       (org-babel-graphical-output-file params)))
	   (colnames-p (unless graphics-file (cdr (assq :colnames params))))
	   (rownames-p (unless graphics-file (cdr (assq :rownames params))))
	   (full-body
	    (let ((inside
		   (list (org-babel-expand-body:myMath body params graphics-file))))
	      (mapconcat 'identity
			 (if graphics-file
			     (append
			      (list (org-babel-myMath-construct-graphics-device-call
				     graphics-file params))
			      inside
			      (list "},error=function(e){plot(x=-1:1, y=-1:1, type='n', xlab='', ylab='', axes=FALSE); text(x=0, y=0, labels=e$message, col='red'); paste('ERROR', e$message, sep=' : ')}); dev.off()"))
			   inside)
			 "\n")))
	   (result
	    (org-babel-myMath-evaluate
	     session full-body result-type result-params
	     (or (equal "yes" colnames-p)
		 (org-babel-pick-name
		  (cdr (assq :colname-names params)) colnames-p))
	     (or (equal "yes" rownames-p)
		 (org-babel-pick-name
		  (cdr (assq :rowname-names params)) rownames-p)))))
      (if graphics-file nil result))))


(defun org-babel-prep-session:myMath (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
(message "in prep session")
  (let* ((session (org-babel-myMath-initiate-session session params))
	 (var-lines (org-babel-variable-assignments:myMath params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))



(defun org-babel-myMath-initiate-session (session params)
(print session)
(print params))



(defvar ess-ask-for-ess-directory) ; dynamically scoped
;;try adapting matlab code from ob-octave.el
(defun org-babel-myMath-initiate-session (session params)
  "Create an myMath inferior process buffer.
If there is not a current inferior-process-buffer in SESSION then
create.  Return the initialized session."
(message "initiating a myMath session")
(message "before unless")
(print session)
  (unless (string= session "none")
    (let ((session (or session "*myMath*")))
(message "before if live")
(print session)
(print (org-babel-comint-buffer-livep session))
(message "just before if live")
(print (org-babel-comint-buffer-livep session))
      (if (org-babel-comint-buffer-livep session) session
	(save-window-excursion
;;(myMath)
(myMath)
;;(run-wolfram "/opt/mathematica/11.3/Executables/math \n")
;;(print "/opt/mathematica/11.3/Executables/math")
	  (rename-buffer (if (bufferp session) (buffer-name session)
			   (if (stringp session) session (buffer-name))))
	  (current-buffer))))))



(defun org-babel-expand-body:myMath (body params &optional _graphics-file)
  "Expand BODY according to PARAMS, return the expanded body."
  (mapconcat 'identity
	     (append
	      (when (cdr (assq :prologue params))
		(list (cdr (assq :prologue params))))
	      (org-babel-variable-assignments:myMath params)
	      (list body)
	      (when (cdr (assq :epilogue params))
		(list (cdr (assq :epilogue params)))))
	     "\n"))


(defun org-babel-variable-assignments:myMath (params)
  "Return list of myMath statements assigning the block's variables."
  (let ((vars (org-babel--get-vars params)))
    (mapcar
     (lambda (pair)
       (org-babel-myMath-assign-elisp
	(car pair) (cdr pair)
	(equal "yes" (cdr (assq :colnames params)))
	(equal "yes" (cdr (assq :rownames params)))))
     (mapcar
      (lambda (i)
	(cons (car (nth i vars))
	      (org-babel-reassemble-table
	       (cdr (nth i vars))
	       (cdr (nth i (cdr (assq :colname-names params))))
	       (cdr (nth i (cdr (assq :rowname-names params)))))))
      (number-sequence 0 (1- (length vars)))))))

(defun org-babel-myMath-evaluate
  (session body result-type result-params column-names-p row-names-p)
(message "myMath-evaluate")
(message body)
  "Evaluate myMath code in BODY."
  (if session
      (org-babel-myMath-evaluate-session
       session body result-type result-params column-names-p row-names-p)
    (org-babel-myMath-evaluate-external-process
     body result-type result-params column-names-p row-names-p)))

(setq org-babel-temp-file (make-temp-file "ob-myMath"))

(defconst org-babel-myMath-wrapper-method
"With[{theRes=%s,fnOne=\"%s\",fnTwo=\"%s\"},
If[Head[theRes]===List,Export[fnOne,theRes,\"CSV\"],
theFile=OpenWrite[fnOne];
If[Or[Head[theRes]===String,NumberQ[theRes]],WriteString[theFile,theRes],
WriteString[\"not sure how to format\"];
Close[theFile]]]]"
)

(defvar org-babel-myMath-eoe-indicator "Print[\"orgBabelEOE\"]")
(defvar org-babel-myMath-eoe-output "orgBabelEOELL")



;;(defvar org-babel-myMath-eoe-indicator "Print[\"org_babel_myMath_eoe\"]")
;;(defvar org-babel-myMath-eoe-output "org_babel_myMath_eoe")

(defvar org-babel-julia-write-object-command "writecsv(\"%s\",%s)")




(defun org-babel-myMath-evaluate-session
  (session body result-type result-params column-names-p row-names-p)
  "Evaluate BODY in SESSION.
If RESULT-TYPE equals 'output then return standard output as a
string.  If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
(message "this is the new code")
  (case result-type
    (value
     (with-temp-buffer
       (insert (org-babel-chomp body))
       (let ((ess-local-process-name
	      (process-name (get-buffer-process session)))
	     (ess-eval-visibly-p nil))
	 (ess-eval-buffer nil)))
     (let ((tmp-file (org-babel-temp-file "julia-")))
       (org-babel-comint-eval-invisibly-and-wait-for-file
	session tmp-file
(format org-babel-myMath-wrapper-method
			       (if row-names-p "TRUE" "FALSE")
			       (if column-names-p
				   (if row-names-p "NA" "TRUE")
				 "FALSE")
			       (format "Function[%s][]" body)
			       (org-babel-process-file-name tmp-file 'noquote))
)
       (org-babel-julia-process-value-result
	(org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (buffer-string))
	  (org-babel-import-elisp-from-file tmp-file '(4)))
	column-names-p)))
    (output
     (mapconcat
      #'org-babel-chomp
      (butlast
       (delq nil
	     (mapcar
	      (lambda (line) (when (> (length line) 0) line))
	      (mapcar
	       (lambda (line) ;; cleanup extra prompts left in output
		 (if (string-match
		      "^\\([ ]*[>+\\.][ ]?\\)+\\([[0-9]+\\|[ ]\\)" line)
		     (substring line (match-end 1))
		   line))
	       (org-babel-comint-with-output (session org-babel-myMath-eoe-output)
		 (insert (mapconcat #'org-babel-chomp
				    (list body org-babel-myMath-eoe-indicator)
				    "\n"))
		 (inferior-ess-send-input)))))) "\n"))))



(defun org-babel-myMath-evaluate-external-process
    (body result-type result-params column-names-p row-names-p)
(message "myMath-evaluate-external")
(message body)
  "Evaluate BODY in external myMath process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (cl-case result-type
    (value
     (let ((tmp-file (org-babel-temp-file "myMath-")))
(message "whatelse")
(message 
(format org-babel-myMath-wrapper-method
			       (if row-names-p "TRUE" "FALSE")
			       (if column-names-p
				   (if row-names-p "NA" "TRUE")
				 "FALSE")
			       (format "Function[%s][]" body)
			       (org-babel-process-file-name tmp-file 'noquote))
)
       (org-babel-eval org-babel-myMath-command
		       (format org-babel-myMath-write-object-command
			       (if row-names-p "TRUE" "FALSE")
			       (if column-names-p
				   (if row-names-p "NA" "TRUE")
				 "FALSE")
;			       (format "{function ()\n{\n%s\n}}()" body)
			       (format "Function[%s][]" body)
			       (org-babel-process-file-name tmp-file 'noquote)))
       (org-babel-myMath-process-value-result
	(org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (org-babel-chomp (buffer-string) "\n"))
	  (org-babel-import-elisp-from-file tmp-file '(16)))
	column-names-p)))
    (output (org-babel-eval org-babel-myMath-command body))))




(defcustom org-babel-myMath-command "/opt/mathematica/11.3/Executables/math"
  "Name of command to use for executing myMath code."
  :group 'org-babel
  :version "24.1"
  :type 'string)



(defconst org-babel-myMath-write-object-command 
"ToExpression[TemplateObject[{
\"Function[{},With[{str=OpenWrite[\\\"\",
TemplateSlot[4],
\"\\\"]},WriteString[str,\",
TemplateSlot[3],
\"];Close[str]]][]\"},InsertionFunction -> TextString, CombinerFunction -> StringJoin][\"%s\",\"%s\",\"%s\",\"%s\"]]"
)



(defun org-babel-myMath-process-value-result (result column-names-p)
  "myMath-specific processing of return value.
Insert hline if column names in output have been requested."
  (if column-names-p
      (cons (car result) (cons 'hline (cdr result)))
    result))




(provide 'ob-myMath)

