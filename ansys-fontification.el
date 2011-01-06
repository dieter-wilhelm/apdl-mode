;;; ansys-fontification.el-- building keywords and completions

;; Copyright (C) 2006 - 2011 H. Dieter Wilhelm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This lisp script is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;
;; Permission is granted to distribute copies of this lisp script
;; provided the copyright notice and this permission are preserved in
;; all copies.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;; help also for get-functions, elements, 

;; HINT: every line in the *.txt files starting with an # will be ignored
;; 1.) command parameter help: copy file
;;     ansys_inc/vXXX/ansys/docu/dynpromptXXX.ans -> `ansys_dynprompt.txt'
;;     done for v12,v13
;; 2.) elements: copy&pasted from the help documentation:
;;     table of contents -> `ansys_elements.txt'
;;     (ansys_inc/vXXX/commonfiles/help/en-us/help/ans_elem/toc.toc xml file)
;;     done: v12,v13
;; 3.) ansys-help:
;;     command reference: keywords:
;;     apdl * commands and regular Ansys commands
;;     copied from the ansys help ->`ansys_keywords.txt'
;;     kill /eof from the keywords (see: -font-lock-keywords) <- don't know why, seems to work now
;;     done: v12
;;     read_tags.py
;;     done: v13
;; 4.) parametric functions: seem to remain rather fixed (V13 same as in V12)
;;     ansys APDL guide chapter 3.8, trigonometric functions and their inverse functions must
;;     be separated by hand!!! The same applies to hyperbolic functions
;;     ->`ansys_parametric_functions.txt'
;; 5.) get functions: ansys 11.0 APDL Programmer's guide Appendix B.
;;     i) one has to comment (#) out some headlines!!!
;;     ii) Prepend some string function descriptions with their name e.g.
;;         StrOut = STRCAT(... with STRCAT(...)
;;     get function summary ->`ansys_get_functions.txt'

;;  The following is not (yet) implemented
;; (6.) _RETURN values from APDL guide chapter 4.6 (Ansys 11) 5.6 (Ansys 13)
;;     -> `ansys_return_values.txt'	       
;;     
;;; necessary variables:
;; 1.) `Ansys_undocumented_commands'
;; 2.) `Ansys_written_out_commands'
;; 3.) `Ansys_deprecated_elements_alist'
;; 4.) `Ansys_commands_without_arguments'

(setq Ansys-version "13.0")

;; 
(defconst Ansys_undocumented_commands	;or macros?
  '(
    "/WB"			   ; signify a WB generated input file
    "XMLO" "/XML" 
    "CNTR" ; ,print,1 ! print out contact info and also make no initial contact an error
    "EBLOCK" "CMBLOCK" "NBLOCK" "/TRACK" "CWZPLOT" "~EUI"
    "NELE"   ; predecessor of NSLE, NELE (WorkBench 10.0 output) still
	     ; working in Ansys 11.0 though
    "EALL"   ;esel,all? (WorkBench 10.0)
    "NALL"   ;nsel,all? (WB 10.0)
    "FLITEM" ;log command
    "/VERIFY"				;verification run?
    "/SSS"				;scale plot values /sss,3
    "~CFIN"				;Ansys 11.0
    "*EVAL"				;Ansys 11.0
    "*MOONEY"				;Ansys 11.0
   "/RUNSTAT"				; 13.0
   "ALPFILL"
   "ARCOLLAPSE"
   "ARDETACH"
   "ARFILL"
   "ARMERGE"
   "ARSPLIT"
   "GAPFINISH"
   "GAPLIST"
   "GAPMERGE"
   "GAPOPT"
   "GAPPLOT"
   "LNCOLLAPSE"
   "LNDETACH"
   "LNFILL"
   "LNMERGE"
   "LNSPLIT"
   "PCONV"
   "PLCONV"
   "PEMOPTS"
   "PEXCLUDE"
   "PINCLUDE"
   "PMETH"
   "/PMETH"
   "PMOPTS"
   "PPLOT"
   "PPRANGE"
   "PRCONV"
   "PRECISION"
   "RALL"
   "RFILSZ"
   "RITER"
   "RMEMRY"
   "RSPEED"
   "RSTAT"
   "RTIMST"
   "/RUNST"
   "RWFRNT"
   "SARPLOT"
   "SHSD"
   "SLPPLOT"
   "SLSPLOT"
   "VCVFILL"
   )			       
  "Ansys commands not documented in the manuals.
Seen mainly in Workbench output files and Ansys verification
models.")

(defconst Ansys_written_out_commands
  '(
    "*END" "*ENDIF"
    "*ELSEIF"				;allows arguments behind it
    "*ENDDO" "/POST1" "/POST26" "/PREP7"
    )
  "Commands which aren't allowed to be abbreviated and the solver
  won't allow characters appended to.")


(defconst Ansys_commands_without_arguments
  '(
    "*ENDIF" "*ENDDO" "*END" "*CFCLOS" "*CYCLE" "*ELSE" 
    "*EXIT" "/PMACRO" "*VSTAT" 
    "/AUX2" "/AUX3" "/AUX12" "/AUX15"
    "/POST1" "/POST26" "/PREP7"
    "SSUM"
    )
  "Ansys commands which aren't allowed with arguments")

;; deprecated element list is from Ansys version 12.0, 13.0
(defconst Ansys_deprecated_elements_alist
'(
  ("BEAM3"    . "BEAM188")
  ("BEAM4"    . "BEAM188")
  ("BEAM23"   . "BEAM188")
  ("BEAM24"   . "BEAM188")
  ("BEAM44"   . "BEAM188")
  ("BEAM54"   . "BEAM188")
  ("COMBIN7"  . "MPC184")
  ("CONTAC12" . "CONTA178")
  ("CONTAC52" . "CONTA178")
  ("LINK1"    . "LINK180")
  ("LINK8"    . "LINK180")
  ("LINK10"   . "LINK180")
  ("LINK32"   . "LINK33")
  ("PIPE16"   . "PIPE288")
  ("PIPE17"   . "PIPE288")
  ("PIPE18"   . "ELBOW290")
  ("PIPE20"   . "PIPE288")
  ("PIPE59"   . "PIPE288")
  ("PIPE60"   . "ELBOW290")
  ("PLANE42"  . "PLANE182")
  ("PLANE67"  . "PLANE223")
  ("PLANE82"  . "PLANE183")
  ("PLANE145" . "-")		       ;p-elements are out in 13.0
  ("PLANE146" . "-")
  ("SHELL43"  . "SHELL181")
  ("SHELL57"  . "SHELL131")
  ("SHELL63"  . "SHELL181")
  ("SHELL91"  . "SHELL281")
  ("SHELL93"  . "SHELL281")
  ("SHELL99"  . "SHELL281")
  ("SHELL150" . "-")		       ;p-elements are out in 13.0
  ("SOLID45"  . "SOLID185")
  ("SOLID46"  . "SOLID185")
  ("SOLID69"  . "SOLID226")
  ("SOLID92"  . "SOLID187")
  ("SOLID95"  . "SOLID186")
  ("SOLID191" . "SOLID186")
  ("SOLID127" . "-")		       ;p-elements are out in 13.0
  ("SOLID128" . "-")
  ("SOLID147" . "-")
  ("SOLID148" . "-")
  ("VISCO88"  . "PLANE183")
  ("VISCO89"  . "SOLID186")
  ("VISCO106" . "PLANE182")
  ("VISCO107" . "SOLID185")
  ("VISCO108" . "PLANE183")
)
"Association list of deprecated element types with their proposed
successor.")

;; (defun Ansys-elements (list)
;;       (mapcar
;;        '(lambda (name)
;; 	  (list
;; 	   (concat "\\<\\(" name "\\)\\>")
;; 	   1
;; 	   'font-lock-variable-name-face
;; 	   'keep			;do not overwrite comment-faces
;; 	   ))
;;        list))

(defun Ansys-get-functions (list)
      (mapcar
       '(lambda (name)
	  (list
	   (concat "\\(" name "\\)\\s-*(")
	   1
	   'font-lock-function-name-face
	   'keep))
       list))

(defun Ansys-parametric-functions (list)
      (mapcar
       '(lambda (name)
	  (list
	   (concat "\\(" name "\\)\\s-*(")
	   '(1 font-lock-function-name-face keep)))
       list))

(defun double_entry_p(strg commands)
  "Returns t if STRG occurs twice in COMMANDS."
  (let ( (n 0) (l (length strg)) (str))
    (dolist (M commands)
      (if (> (length M) l)
	  (setq str (substring M 0 l))
	(setq str M))
      (if (string= str strg)
	  (setq n (1+ n))))
    (if (> n 1)
      t)))

(defun make_unique( strg commands)
  "Return a sub-string of STRG which is unique in COMMANDS.
The length of STRG is greater than 4 characters."
  (let ((l (length strg)) (n 4) (str ""))
    (while (<= n l)
      (setq str (substring strg 0 n))
      (if (double_entry_p str commands)
	  (setq n (1+ n))
	(setq n (1+ l))))
    str))

(defun asterisk(String)
  (let ((str String))
    (if (= (elt String 0) ?*)
	(setq str (concat "\\" str)))
    str)
  )

(defun slice (string)
  (let* ((l (length string))
	 (s ""))
    (dolist (c (split-string string "" t))
      (setq s (concat s "\\(?:" c)))
    (dotimes (i l s)
      (setq s (concat s "\\)?")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; comments and strings are fontified for all levels
;; syntactical fontification is there

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; default level when font-lock-maximum-decoration is nil (just
;; minimum command length) overwritten by variable defs (=)

;; 1.) no regard for appended chars just minimum fontification lengths
;;     written_out_p && if l > 4 make_unique
;; 2.) no regard for condensed input lines, commands first in line 
;; 3.) undocumented commands are higlighted like documented

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; level 1 this is the mode default level (official names, FIXED in
;; length as suggested by command completion, no additional chars are
;; allowed even when disregarded by the solver) The rest is the same
;; as level 2 except no multiline stuff

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; level 2 (commands with variable fontification lengths)

;; a.) regexp for commands which do not allow characters behind it
;;     1.) written_out_p
;;     2.) length < 4  TODO: is this true? e. g. *if

;; TODO: check whether a.) and b.) is not congruent except *elseif
;; b.) regexp for commands which do not allow parameters behind it
;;     1.) no_parameter_p Ansys_commands_without_arguments
;;     2.) not written_out_p
;; TODO: what is with $ condensation?

;; c.) regexp for commands which allow characters and arguments behind it 
;;     1.) length >= 4 but not unique

(defun written_out_p(STRING)
  "Returns true if STRING is a member of
  \"Ansys_written_out_commands\""
  (member STRING Ansys_written_out_commands))

;; (written_out_p "*END")

(defun Prepare_list (List)
  "Simplified fontification v12.
Argument LIST is a list of ansys commands. Return a list of
command strings"
(let ( tmp tmp2 tmp_list l l2)
    (message "Preparing Ansys font lock strings, list-length: %d." (length List))
    (dolist (M List tmp_list)
      (setq l (length M))
      (cond ((written_out_p M)		
	     (setq tmp_list (cons M tmp_list)))
	    ((<= l 4)			;shorter stuff
	     (setq tmp_list (cons M tmp_list)))
	    (t ;;long command names
	     (setq tmp (make_unique M List)
		   tmp_list (cons tmp tmp_list)))))))

(defun Overlapping (strg l) 
"Return a list of a sequences of strings starting from L up to
the length of the original string STRG."
  (let (lisy
	(n1 (length strg)))
    (when (> l n1)
      (error "string length too small"))
    (dotimes (i (1+ (- n1 l)) lisy)
      (setq lisy (cons (substring strg 0 (- n1 i)) lisy)))))

;; TODO: we are still neglecting commands without parameters!
(defun Prepare_list_2 (List)
  "Return a list of 3 lists for level 2.
Written out commands and 2 lists of commands which the solver
allows characters appended behind."
  (let ( tmp tmp_list list_a list_b list_c l l2)
    (message "Preparing Ansys font lock 2 strings...")
    (dolist (M List)
      (setq l (length M))
      (cond  ;written out stuff
       ((written_out_p M)
	(setq list_a (cons M list_a)))
       ((< l 4)			;short stuff
	(setq list_a (cons M list_a)))
       ;; variable ending stuff
       ((= l 4)
	(setq tmp_list (cons M tmp_list)))
       (t ;;the rest, longer command names
	(setq tmp (make_unique M List)
	      l2 (length tmp)
	      tmp (Overlapping M l2)
	      tmp_list (append tmp tmp_list)))))

    (setq l2 (/ (length tmp_list) 2)
	  list_b (butlast tmp_list l2)
	  list_c (last tmp_list l2))
    (message "length_a: %d length_b: %d length_c: %d" (length list_a) (length list_b) (length list_c) )
    (list list_a list_b list_c)))

(defun Ansys-initialize-completions ()
  "Create a list for Ansys completions.
Function names are distinguished by `()'."
  (append
   Ansys_elements
   Ansys_commands
   (mapcar '(lambda (str) (concat str "()"))
	   Ansys_get_functions)
   (mapcar '(lambda (str) (concat str "()"))
	   Ansys_parametric_functions)))

;; ------------------------------------------------------------

(let ((buffer (find-file "ansys-keyword.el"))
      list
      commands
      commands-1
      undocumented-commands
      get-functions
      parameteric-functions
      elements
      deprecated_elements
      list1
      list2
      list3
      )
  (set-buffer buffer)
  (delete-region (point-min) (point-max))

  
  ;; ---------- undocumented commands ----------

  ;; getting another colour than regular commands
  (goto-char (point-min))
  (setq undocumented-commands Ansys_undocumented_commands)
  (insert "(defconst ansys-undocumented-command-regexp\n")
  (prin1 (regexp-opt Ansys_undocumented_commands) buffer)
  (insert "\n\"Regexp of commands not documented in the Ansys
manuals.  Seen mainly in Workbench output files and Ansys
verification models.\")\n\n")
  ;; (beginning-of-defun)
  ;; (fill-paragraph 0)
  (message "undocumented commands...done")
  
  ;; ---------- command parameter help ----------
  (setq list nil)
  ;; first fill list with documented commands
  (with-temp-buffer
    (insert-file-contents "ansys_keywords.txt")
    (insert-file-contents "ansys_dynprompt.txt")
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (setq sort-fold-case t)
    (sort-lines nil (point-min) (point-max))
    ;(write-file "keyw+promt.txt")
    (goto-char (point-min))
    (while (re-search-forward "^\\(.\\w*\\>\\).*\n\\1.*" nil t)
      (add-to-list 'list (match-string 0) 'append)))
  ;; now include the undocumented commands
  (setq list (append list
		     (mapcar '(lambda (m) (concat m " - Ansys undocumented command\n" m))
		       Ansys_undocumented_commands)))
;  (sort list 'string<)
  (set-buffer buffer)
  (goto-char (point-min))
  (insert "(defconst ansys-dynamic-prompt\n'")
  (setq print-length nil)		;nil: print all members of list
  (prin1 list buffer)
  (insert "\n\"Help strings for the parameters of Ansys keywords.\")\n\n")
  (message "command parameter help...done")
  ;; no formatting, it interferes with the output!

  ;; ---------- keywords ----------

  ;; ----- documented + written out + undocumented commands ---
  ;; 0.) unique, solver type
  ;; 1.) full Ansys name same as completed
  ;; 2.) from unique to full name


  (setq list ())			;initialise list
  (with-temp-buffer
    (insert-file-contents "ansys_keywords.txt")
;    (setq sort-fold-case t)
;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^.\\w*\\>" nil t)
      (add-to-list 'list (match-string 0) 'append)))
  (set-buffer buffer)
  (setq commands list)

  (message "starting minimal level.")
  (goto-char (point-min))
  (setq commands-1 (append commands Ansys_undocumented_commands))
;  (debug)
  (insert "(defconst ansys-command-regexp\n")
  (setq print-length nil)		;nil: print all members of list
  (prin1 (regexp-opt (Prepare_list commands-1))
		     buffer)
  (insert "\n\"Ansys short keyword name regexp\")\n\n")

  (message "starting level 1.")
  (goto-char (point-min))
  (insert "(defconst ansys-command-regexp-1\n")
  (prin1 (regexp-opt commands)
		     buffer)
  (insert "\n\"Ansys full keyword name regexp\")\n\n")

  (message "starting level 2.")
  (goto-char (point-min))
  ;;  regexp becoming too big for font-lock!!! when keywords are around 4000
  (setq list (Prepare_list_2 commands)
	list1 (car list)
	list2 (nth 1 list)
	list3 (nth 2 list)
	list1 (regexp-opt list1)	;written out stuff
	list2 (regexp-opt list2)
	list3 (regexp-opt list3))
  (insert "(defconst ansys-command-regexp-2a\n")
  (prin1 list1 buffer)
  (insert "\n\"Ansys keyword name regexp 2a\")\n\n")
  ;; 
  (insert "(defconst ansys-command-regexp-2b\n")
  (prin1 list2 buffer)
  (insert "\n\"Ansys keyword name regexp 2b\")\n\n")
  ;; 
  (insert "(defconst ansys-command-regexp-2c\n")
  (prin1 list3 buffer)
  (insert "\n\"Ansys keyword name regexp 2c\")\n\n")


  ;; ---------- ansys elements ----------

  ;; --- regular elements ---
  (setq list ())			;initialise list
  (with-temp-buffer
    (insert-file-contents "ansys_elements.txt")
;    (setq sort-fold-case t)
;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\w+\\>" nil t)
      (add-to-list 'list (match-string 0) 'append)))
  (setq elements list)		;needed for completion list
  (set-buffer buffer)
  (goto-char (point-min))
  (insert (concat
	   "(defconst ansys-element-regexp\n"))
  (setq print-length nil)		;nil: print all members of list
  (prin1 (regexp-opt list 'words) buffer)
  (insert "\n\"Ansys elements regexp.\")\n\n")
  (beginning-of-defun)
  (fill-paragraph 0)

  ;; --- deprecated elements ---
  (goto-char (point-min))
  (insert (concat
	   "(defconst ansys-deprecated-element-regexp\n"))
  (setq print-length nil)		;nil: print all members of list
  (setq deprecated-elements (mapcar 'car Ansys_deprecated_elements_alist))
  (prin1 (regexp-opt deprecated-elements 'words) buffer)
  (insert "\n\"Ansys deprecated elements regexp.\")\n\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  ;; -- write out the alist for a possible reference --
  (goto-char (point-min))
  (insert (concat
	   "(defconst ansys-deprecated-element-alist\n'"))
  (setq print-length nil)		;nil: print all members of list
  (setq deprecated-elements (mapcar 'car Ansys_deprecated_elements_alist))
  (prin1 Ansys_deprecated_elements_alist buffer)
  (insert "\n\"Association list for Ansys deprecated elements
  and their proposed replacements.\")\n\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  (message "elements...done")

  ;; ---------- get functions ----------

  (setq list ())			;initialise list
  (with-temp-buffer
    (insert-file-contents "ansys_get_functions.txt")
;    (setq sort-fold-case t)
;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\w+\\)(" nil t)
      (add-to-list 'list (match-string 1) 'append)))
  (setq get-functions list)	;we need this variable for the completions!
  (set-buffer buffer)
  (goto-char (point-min))
  (insert (concat
	   "(defconst ansys-get-function-regexp\n"))
  (setq print-length nil)		;nil: print all members of list
  (prin1 (regexp-opt get-functions) buffer)
  (insert "\n\"Ansys get function regexp.\")\n\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  (message "get-functions...done")
  
  ;; ---------- parametric functions ----------

  (setq list ())			;initialise list
  (with-temp-buffer
    (insert-file-contents "ansys_parametric_functions.txt")
;    (setq sort-fold-case t)
;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\w+\\)(" nil t)
      (add-to-list 'list (match-string 1) 'append)))
  (setq parametric-functions list) ;we need this later for completions!
  (set-buffer buffer)
  (goto-char (point-min))
  (insert (concat
	   "(defconst ansys-parametric-function-regexp\n"))
  (setq print-length nil)		;nil: print all members of list
  (prin1 (regexp-opt parametric-functions) buffer)
  (insert "\n\"Ansys parametric function regexp.\")\n\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  (message "parametric functions...done")

  ;; ---------- completions ----------

  (goto-char (point-min))
  ;; (setq functions
  ;; 	(map 'list '(lambda (s) (concat s "()"))
  ;; 	     parametric-functions get-functions))

  (setq get-functions
	(mapcar '(lambda (s) (concat s "()")) get-functions))
  (setq parametric-functions
	(mapcar '(lambda (s) (concat s "()")) parametric-functions))
  (insert "(defconst ansys-completions\n'")
  (setq print-length nil)		;nil: print all members of list
  (prin1 (append
	  elements
	  deprecated-elements
	  get-functions
	  parametric-functions
	  commands
	  undocumented-commands
	  ) buffer)
  ;; (prin1 commands buffer)
  ;;  buffer)
  (insert "\n\"Ansys symbols for completion in Ansys mode.
By default Ansys keywords, get-functions, parametric-function and elements
 (deprecated as well) are completed.\")\n\n")
  ;; (beginning-of-defun)
  ;; (fill-paragraph nil)
  (message "completions...done")

  ;; ---------- header ----------

  (goto-char (point-min))
  (insert ";; ansys-keyword.el -- Ansys mode completion and "
  "highlighting variables. \n" ";; This file was built by "
  "\"ansys-fontification.el\".\n\n"
  ";; Copyright (C) 2006 - 2011 H. Dieter Wilhelm.\n\n")
  (save-buffer)
  (message "ansys-keywords.el done.")
  ;; --- end of let
)  

;; (let* ((dir "")
;;        (file1 (concat dir "ansys_keywords.txt"))
;; p       (file2 (concat dir "ansys-dynprompt.txt"))
;;        (file3 (concat dir "ansys_elements.txt"))
;;        (file4 (concat dir "ansys_get_functions.txt"))
;;        (file5 (concat dir "ansys_parametric_functions.txt"))
;;        (buffer (find-file (concat dir "ansys-keyword.el")))
;;        list
;;        elements)

;;   ;; ---------- ansys elements ----------
;;   (setq list ())			;initialise list
;;   (with-temp-buffer
;;     (insert-file-contents file3)
;; ;    (setq sort-fold-case t)
;; ;    (sort-lines nil (point-min) (point-max))
;;     (delete-matching-lines "^#.*" (point-min) (point-max))
;;     (goto-char (point-min))
;;     (while (re-search-forward "^\\w+\\>" nil t)
;;       (add-to-list 'list (match-string 0) 'append)))
;;   (setq Ansys_elements list)		;needed for completion list
;;   (set-buffer buffer)
;;   (goto-char (point-min))
;;   (insert (concat
;; 	   "(defconst ansys-elements-regexp\n"))
;;   (setq print-length nil)		;nil: print all members of list
;;   (prin1 (regexp-opt list 'words) buffer)
;;   (insert "\n\"Ansys elements regexp.\"\n)\n")
;;   (beginning-of-defun)
;;   (fill-paragraph 0)
;;   ;; deprecated elements
;;   (goto-char (point-min))
;;   (insert (concat
;; 	   "(defconst ansys-deprecated-elements-regexp\n"))
;;   (setq print-length nil)		;nil: print all members of list
;;   (prin1 ansys_deprecated_elements_regexp buffer)
;;   (insert "\n\"Ansys deprecated elements regexp.\"\n)\n")
;;   (beginning-of-defun)
;;   (fill-paragraph 0)
;;   (message "elements...done")

;;   ;; ---------- get functions ----------
;;   (setq list ())			;initialise list
;;   (with-temp-buffer
;;     (insert-file-contents file4)
;; ;    (setq sort-fold-case t)
;; ;    (sort-lines nil (point-min) (point-max))
;;     (delete-matching-lines "^#.*" (point-min) (point-max))
;;     (goto-char (point-min))
;;     (while (re-search-forward "^\\(\\w+\\)(" nil t)
;;       (add-to-list 'list (match-string 1) 'append)))
;;   (set-buffer buffer)
;;   (goto-char (point-min))
;;   (insert (concat
;; 	   "(defconst ansys-get-functions\n'"))
;;   (setq print-length nil)		;nil: print all members of list
;;   (setq Ansys_get_functions list)	;we need this variable for the completions!
;;   (prin1 (Ansys-get-functions list) buffer)
;;   (insert "\n\"Ansys get functions.\"\n)\n")
;;   (beginning-of-defun)
;;   (fill-paragraph 0)
;;   (message "get-functions...done")

;;   ;; ---------- parametric functions ----------
;;   (setq list ())			;initialise list
;;   (with-temp-buffer
;;     (insert-file-contents file5)
;; ;    (setq sort-fold-case t)
;; ;    (sort-lines nil (point-min) (point-max))
;;     (delete-matching-lines "^#.*" (point-min) (point-max))
;;     (goto-char (point-min))
;;     (while (re-search-forward "^\\(\\w+\\)(" nil t)
;;       (add-to-list 'list (match-string 1) 'append)))
;;   (set-buffer buffer)
;;   (goto-char (point-min))
;;   (insert (concat
;; 	   "(defconst ansys-parametric-functions\n'"))
;;   (setq print-length nil)		;nil: print all members of list
;;   (setq Ansys_parametric_functions list) ;we need this later for completions!
;;   (prin1 (Ansys-parametric-functions list) buffer)
;;   (insert "\n\"Ansys parametric functions.\"\n)\n")
;;   (beginning-of-defun)
;;   (fill-paragraph 0)
;;   (message "parametric functions...done")

;;   ;; ---------- keywords ----------

;; ;;   (setq list ())			;initialise list
;; ;;   (with-temp-buffer
;; ;;     (insert-file-contents file1)
;; ;; ;    (setq sort-fold-case t)
;; ;; ;    (sort-lines nil (point-min) (point-max))
;; ;;     (delete-matching-lines "^#.*" (point-min) (point-max))
;; ;;     (goto-char (point-min))
;; ;;     (while (re-search-forward "^.\\w*\\>" nil t)
;; ;;       (add-to-list 'list (match-string 0) 'append)))
;; ;;   (set-buffer buffer)
;; ;;   (setq Ansys_commands list)
;; ;;   (goto-char (point-min))
;; ;; ;  (debug)
;; ;;   (insert "(defconst ansys-commands\n'")
;; ;;   (setq print-length nil)		;nil: print all members of list
;; ;;   (prin1 (Prepare_list3 list) buffer)
;; ;;   (insert "\n\"Ansys keywords\"\n)\n")
;; ;;   (beginning-of-defun)
;; ;;   (fill-paragraph 0)

;;   ;;  ---
;;   (set-buffer buffer)
;;   (goto-char (point-min))
;;   (insert "(defconst ansys-written-out-command-regexp\n")
;;   (prin1  ansys_written_out_commands_regexp buffer)
;;   (insert "\n\"Ansys command which have to be written out\"\n)\n")

;;   ;; --- building keywords level 2 ---
;;   (setq list ())			;initialise list
;;   (with-temp-buffer
;;     (insert-file-contents file1)
;; ;    (setq sort-fold-case t)
;; ;    (sort-lines nil (point-min) (point-max))
;;     (delete-matching-lines "^#.*" (point-min) (point-max))
;;     (goto-char (point-min))
;;     (while (re-search-forward "^.\\w*\\>" nil t)
;;       (add-to-list 'list (match-string 0) 'append)))
;;   (set-buffer buffer)
;;   (setq Ansys_commands list)
;;   (goto-char (point-min))
;;   ;; (insert "(defconst ansys-commands2\n'")
;;   ;; (insert (concat "^\\s-*" (regexp-opt Ansys_commands)))
;;   ;; (insert "\n\"Ansys keywords level 2\"\n)\n")

  ;; (message "keywords...done")
  

;  (message (car list))
;  (save-buffer)
  ;; )

;; ------------------------------------------------------------
;; ---------- repository, ok ok code cruft ----------
;; ------------------------------------------------------------

;; (define-skeleton ansys-skeleton-header	 ;from Holger Sparr
;;   "Insert header for an APDL script" nil ;;"Name of file: "
;;   "! 	$Id" ":$\n"
;;   "!"(insert (make-string 80 ?*))"\n"
;;   "!*"(insert (make-string (- 80 2) ? ))"*\n"
;;   "C*** " (buffer-name) (insert (make-string (- 80 5 (length (buffer-name))) ? ))"*\n"
;;   "!*"(insert (make-string (- 80 2) ? ))"*\n"
;;   "!*"(insert (make-string (- 80 2) ? ))"*\n"
;;   "!*"(insert (make-string (- 80 2) ? ))"*\n"
;;   "!*   Called by: " _ 
;;   (let ((called (read-file-name "Calling Script File: " "")))
;;     (insert called)
;;     (insert (make-string (- 80 2 (length "   Called by: ") (length called)) ? ))) "*\n"
;;   "!*"(insert (make-string (- 80 2) ? ))"*\n"
;;   "!*   Calling:"(insert (make-string (- 80 2 (length "   Calling:")) ? ))"*\n"
;;   "!*"(insert (make-string (- 80 2) ? ))"*\n"
;;   "!*   Macros: "
;;   (let ((mlib (read-file-name "Macro Library: " "")))
;;     (insert mlib " ()")
;;     (insert (make-string (- 80 2 (length "   Macros: ") (length mlib) 3) ? )))"*\n"
;;   "!*"(insert (make-string (- 80 2) ? ))"*\n"
;;   "!*"(insert (make-string (- 80 2) ? ))"*\n"
;;   "!"(insert (make-string 80 ?*))"\n")

;; (defsubst ansys-comment-or-empty-line-p () ;FIXME: not used
;;   "Return t if either in an comment or empty line, nil otherwise")

;; This file containes code from ansys-mod.el.
;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Author: Tim Read <Tim.Read@fp.co.nz> (Author does not respond)
;; Author: Geoff Foster <fosterg@fp.co.nz> (Address unreachable,
;;   Dieter Wilhelm 2006-03-08)

;; eob marking (not window-system)

;; (defun ansys-mark-eob ()		"code from Samuel Padgett.";FIXME
;;   (let ((existing-overlays (overlays-in (point-max) (point-max)))
;; 	(eob-mark (make-overlay (point-max) (point-max) nil t t))
;; 	(eob-text "~~~ end of file ~~~"))
;;     ;; Delete any previous EOB markers.  Necessary so that they don't
;;     ;; accumulate on calls to revert-buffer.
;;     (dolist (next-overlay existing-overlays)
;;       (if (overlay-get next-overlay 'eob-overlay)
;; 	  (delete-overlay next-overlay)))
;;     ;; Add a new EOB marker.
;;     (put-text-property 0 (length eob-text)
;; 		       'face '(foreground-color . "slate gray") eob-text)
;;     (overlay-put eob-mark 'eob-overlay t)
;;     (overlay-put eob-mark 'after-string eob-text)))

;; (add-hook 'find-file-hook 'ansys-mark-eob) ;FIXME: take it out, for
;; the next version provide choise between indicate-empty-lines and
;; this

;; (defun ansys_electric_comma ()		;FIXME
;;   ""
;;   (interactive)
;;   (if (ansys-not-in-string-or-comment-p)
;;       (insert "    ,")
;;     (insert ",")))

;;; Menu

;; FIXME:
;;    * The mode should specify how Imenu should find the definitions or
;;      sections of a buffer, by setting up a buffer-local value for the
;;      variable `imenu-generic-expression', for the two variables
;;      `imenu-prev-index-position-function' and
;;      `imenu-extract-index-name-function', or for the variable
;;      `imenu-create-index-function' (*note Imenu::).

;; what's about tool tips?

;; (defcustom ansys-completion-alist-flag t
;;   "When non nil build Ansys symbol completion list for \\[ansys-complete-symbol]."
;;   :type 'boolean
;;   :group 'Ansys)

;; ============================================================
;;;;; ---   Highlighting (fontification) --- FIXME:
;;
;; 0.) The Ansys solver does not distinguish between upper and lower
;;     case; the interpreter uses just the first 4 characters - when
;;     unique - (except shorter command names or some asterisk
;;     commands which must be written in full).  Undocumented commands
;;     are distinguished from documented ones.

;; 1.) /eof in warning face, :labels for *go branching

;; .) commands reading "raw" input /COM,/TIT,  messages in font-lock-doc-face

;; 2.) Ansys commands with arguments ^\\s-*\(COMMAND\)\\s-*,
;;     or ^\\s-*\(COMM\)\w+\>\\s-*,

;; 3.) Commands without arguments: ^\\s-*\(COMMAND\)\\s-$ FIXME

;; 4.) Slash (`/.*\>') and  asterisk (`\*.*\>') commands cannot be confused
;;     with (used as) variable names: ^\\s-*\(COMM\)\w*\\s-$ or
;;     ^\\s-*\(COMMAND\)\>

;; 5.) Get- and parametric functions): \(FUNCTION\)\\s-*(

;; The ampersand (&) is the *MSG continuation character (10
;; continuation lines of 72 chars respectively are possible).  See
;; below example input.

;; *MSG,UI,Vcoilrms,THTAv,Icoilrms,THTAi,Papprnt,Pelec,PF,indctnc
;; Coil RMS voltage, RMS current, apparent pwr, actual pwr, pwr factor: %/ &
;; Vcoil = %G V (electrical angle = %G DEG) %/ &
;; Icoil = %G A (electrical angle = %G DEG) %/ &
;; APPARENT POWER = %G W %/ &
;; ACTUAL POWER = %G W %/ &
;; Power factor: %G %/ &
;; Inductance = %G %/ &
;; VALUES ARE FOR ENTIRE COIL (NOT JUST THE MODELED SECTOR)
;; ============================================================

;; (setq ansys-commands-without-arguments
;;       (mapcar
;;        '(lambda (name)
;; 	  (list
;; 	   (concat "^\\s-*\\(" name "\\)\\s-*$")
;; 	   1
;; 	   'font-lock-type-face
;; 	   'prepend))
;;         ansys-commands-without-arguments))

;;`warning' is `keywords' a reserved word in lisp??

;; (defconst ansys-command-options
;;   '("EX" "DENS" "ALPX" "REFT" "NUXY" "PRXY" "GXY" "MU" "DAMP" "VISC" "SONC"
;;     "ex" "dens" "alpx" "reft" "nuxy" "prxy" "gxy" "mu" "damp" "visc" "sonc"
;;     "VOLU" "AREA" "LINE" "KP" "ELEM" "NODE"
;;     "volu" "area" "line" "kp" "elem" "node"
;;     "STATIC" "MODAL" "TRANS"
;;     "static" "modal" "trans"
;;     "FRONT" "PCG" "JCG" "SPARSE"
;;     "front" "pcg" "jcg" "sparse"
;;     "SUBSP" "FULL"
;;     "subsp" "full"
;;     "UX" "UY" "UZ" "ROTX" "ROTY" "ROTZ" "PRES"
;;     "ux" "uy" "uz" "rotx" "roty" "rotz" "pres"
;;     "ALL" "YES" "NO" "ON" "OFF" "AUTO"
;;     "all" "yes" "no" "on" "off" "auto"
;;     "THEN" "*EXIT" "CYCLE" "STOP"
;;     "then" "*exit" "cycle" "stop"
;;     )
;;   "Builtin Command options in Ansys.")

;; (defvar ansys-command-options
;;   '("EX" "DENS" "ALPX" "REFT" "NUXY" "PRXY" "GXY" "MU" "DAMP" "VISC" "SONC"
;;     "ex" "dens" "alpx" "reft" "nuxy" "prxy" "gxy" "mu" "damp" "visc" "sonc"
;;     "VOLU" "AREA" "LINE" "KP" "ELEM" "NODE"
;;     "volu" "area" "line" "kp" "elem" "node"
;;     "STATIC" "MODAL" "TRANS"
;;     "static" "modal" "trans"
;;     "FRONT" "PCG" "JCG" "SPARSE"
;;     "front" "pcg" "jcg" "sparse"
;;     "SUBSP" "FULL"
;;     "subsp" "full"
;;     "UX" "UY" "UZ" "ROTX" "ROTY" "ROTZ" "PRES"
;;     "ux" "uy" "uz" "rotx" "roty" "rotz" "pres"
;;     "ALL" "YES" "NO" "ON" "OFF" "AUTO"
;;     "all" "yes" "no" "on" "off" "auto"
;;     "THEN" "*EXIT" "CYCLE" "STOP"
;;     "then" "*exit" "cycle" "stop"
;;     )
;;   "Builtin Command options in Ansys.")

;; ------------------------------------------------------------
;; Startup message -- seems to be never used

;; ansys-mode-startup-message
;;   Nil means do not display the Ansys mode startup message.
;;   Default is t.

;; (defvar ansys-mode-startup-message t
;;   "*Nil means do not display the Ansys mode startup message.")


;; (setq ansys-commands-without-arguments
;;       (mapcar
;;        '(lambda (name)
;; 	  (list
;; 	   (concat "^\\s-*\\(" name "\\)\\s-*$")
;; 	   1
;; 	   'font-lock-type-face
;; 	   'prepend))
;;         ansys-commands-without-arguments))

;; (defconst ansys-command-options
;;   '("EX" "DENS" "ALPX" "REFT" "NUXY" "PRXY" "GXY" "MU" "DAMP" "VISC" "SONC"
;;     "ex" "dens" "alpx" "reft" "nuxy" "prxy" "gxy" "mu" "damp" "visc" "sonc"
;;     "VOLU" "AREA" "LINE" "KP" "ELEM" "NODE"
;;     "volu" "area" "line" "kp" "elem" "node"
;;     "STATIC" "MODAL" "TRANS"
;;     "static" "modal" "trans"
;;     "FRONT" "PCG" "JCG" "SPARSE"
;;     "front" "pcg" "jcg" "sparse"
;;     "SUBSP" "FULL"
;;     "subsp" "full"
;;     "UX" "UY" "UZ" "ROTX" "ROTY" "ROTZ" "PRES"
;;     "ux" "uy" "uz" "rotx" "roty" "rotz" "pres"
;;     "ALL" "YES" "NO" "ON" "OFF" "AUTO"
;;     "all" "yes" "no" "on" "off" "auto"
;;     "THEN" "*EXIT" "CYCLE" "STOP"
;;     "then" "*exit" "cycle" "stop"
;;     )
;;   "Builtin Command options in Ansys.")


;; (defcustom ansys-continuation-offset 3
;;   "Extra indentation applied to Ansys continuation lines."
;;   :type 'integer
;;   :group 'Ansys)


;; (setq Ansys-undocumented-commands
;;       (mapcar
;;        '(lambda (name)
;; 	  (list
;; 	   (if ( < (length name) 4)
;; 	       (concat "^\\s-*\\(" name "\\)\\>")
;; 	     (concat "^\\s-*\\(" name "\\)\\w*\\>"))
;; 	   1
;; 	   'font-lock-constant-face
;; 	   'prepend))
;;        Ansys_undocumented_commands))


;; (setq ansys_written_out_commands_regexp
;;      (concat "^\\s-*" (regexp-opt Ansys_written_out_commands) "\\>"))

;; (defun Prepare_list2 (List)
;;   "Bla.
;; Argument LIST is a list of ansys commands."
;;   (let ( tmp tmp2 tmp_list l l2)
;;     (message "Preparing Ansys font lock regexps...")
;;     (dolist (M List tmp_list)
;;       (setq l (length M))		;FIXME: cond is better
;;       (cond ((written_out_p M)		
;; 	     (setq tmp
;; 		   (list
;; 		    (concat "\\(^\\|\\$\\)\\s-*\\(" (asterisk M) "\\)\\>") ;/*~ aren't word characters!
;; 		    '(2 font-lock-type-face keep))
;; 		   tmp_list (cons tmp tmp_list)))
;; 	    ((< l 4)			;shorter stuff
;; 	     (setq tmp
;; 		   (list
;; 		    (concat "\\(^\\|\\$\\)\\s-*\\(" (asterisk M) "\\)\\>")	;condensed input line
;; 		    '(2 font-lock-type-face keep))
;; 		   tmp_list (cons tmp tmp_list)))
;; 	    ((= l 4)			;short stuff
;; 	     (setq tmp
;; 		   (list
;; 		    (concat "\\(^\\|\\$\\)\\s-*\\(" (asterisk M) "\\)\\(\\w*\\)")	;condensed input line
;; 		    '(2 font-lock-type-face keep)
;; 		    '(3 (quote shadow) keep))
;; 		   tmp_list (cons tmp tmp_list)))
;; 	    ((> l 4);;long stuff
;; 	     (setq
;; 	      tmp2 (make_unique M)
;; 	      l2 (length tmp2)
;; 	      tmp
;; 	      (list
;; 	       (concat "\\(^\\|\\$\\)\\s-*\\("
;; 		       (asterisk tmp2) "\\)\\("
;; 		       (slice (substring M l2 l)) "\\)\\(\\w*\\)"
;; 		       ) 
;; 	       '(2 font-lock-type-face keep)
;; 	       '(3 font-lock-constant-face keep)
;; 	       '(4 (quote shadow) keep))
;; 	      tmp_list (cons tmp tmp_list)))))))


;; Local Variables:
;; mode: outline-minor
;; time-stamp-active: t
;; End:
