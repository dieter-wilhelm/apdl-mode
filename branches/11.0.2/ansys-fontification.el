;;; ansys-fontification.el --- produces keywords for fontification

;; Copyright (C) 2006 - 2009 H. Dieter Wilhelm

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

;;; necessary files (best copied under Windows 8-/)

;; help also for get-functions, elements, 

;; HINT: every line starting with an # will be ignored
;; 1.) command parameter help: copy file
;;     ansys/docu/dynpromptXXX.ans -> `ansys-dynprompt.txt'
;;     done for v12
;; 2.) elements: copy&pasted from the help documentation:
;;     -> `ansys_elements.txt'
;;     done: v12
;; 3.) command keywords: apdl * commands and regular Ansys commands
;;     copied from the ansys help ->`ansys_keywords.txt'
;;     done: v12
;;     kill /eof from the keywords (see: -font-lock-keywords)
;; 4.) parametric functions:
;;     ansys help chapter 3.8, trigonometric functions and their inverse functions must
;;     be separated by hand!!! The same applies to hyperbolic functions
;;     ->`ansys_parametric_functions.txt'
;; 5.) get functions: ansys 11.0 APDL Programmer's guide Appendix B.
;;     i) one has to comment (#) out some headlines!!!
;;     ii) Prepend some string function descriptions with their name e.g.
;;         StrOut = STRCAT(... with STRCAT(...)
;;     get function summary ->`ansys_get_functions.txt'
;; 6.) _RETURN values from APDL guide chapter 4.6 (Ansys 11)
;;     -> `ansys_return_values.txt'	       
;;     
;;; necessary variables:
;; 1.) `ansys_undocumented_commands'
;; 2.) `ansys_written_out_commands'

(setq Ansys-version "12.0")

(defconst Ansys_undocumented_commands	;or macros?
  '(
    "XMLO" "/XML" 
    "CNTR" ; ,print,1 ! print out contact info and also make no initial contact an error
    "EBLOCK" "CMBLOCK" "NBLOCK" "/TRACK" "CWZPLOT" "~EUI"
    "NELE"   ; predecessor of NSLE, NELE (WorkBench 10.0 output) still
	     ; working though
    "EALL"				;esel,all? (WB 10.0)
    "NALL"				;nsel,all? (WB 10.0)
    "FLITEM"				;log command
    "/VERIFY"				;verification run?
    "/SSS"				;scale plot values /sss,3
    "~CFIN"				;Ansys 11
    "*EVAL"				;Ansys 11
    "*MOONEY"				;Ansys 11
    )
  "Ansys commands not documented in the manuals.
Seen mainly in Workbench output files and Ansys verification
models."
  )

(setq Ansys-undocumented-commands
      (mapcar
       '(lambda (name)
	  (list
	   (if ( < (length name) 4)
	       (concat "^\\s-*\\(" name "\\)\\>")
	     (concat "^\\s-*\\(" name "\\)\\w*\\>"))
	   1
	   'font-lock-constant-face
	   'prepend))
       Ansys_undocumented_commands))

(defconst Ansys_written_out_commands
  '(
    "*END"
    "*ENDIF" "*ELSEIF" "*ENDDO"
    "/POST1" "/POST26" "/PREP7"

    )
  "Commands which aren't allowed to be abbreviated and the solver
  won't allow to append to."  )

(defun Ansys-elements (list)
      (mapcar
       '(lambda (name)
	  (list
	   (concat "\\<\\(" name "\\)\\>")
	   1
	   'font-lock-variable-name-face
	   'keep			;do not overwrite comment-faces
	   ))
       list))

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

(defun written_out_p(STRING)
  "Returns true if STRING is a member of
  \"ansys_written_out_commands\""
  (let (v str (tmp Ansys_written_out_commands))
    (while tmp
      (setq str (car tmp))
      (if (string= str STRING)
	  (setq tmp nil v t)
	(setq tmp (cdr tmp))))
    v))

(defun double_entry_p(STRING)
  "Returns t if STRING occurs twice in \"ansys_commands\"."
  (let ( (n 0) (l (length STRING)) (str))
    (dolist (M Ansys_commands)
      (if (> (length M) l)
	  (setq str (substring M 0 l))
	(setq str M))
      (if (string= str STRING)
	  (setq n (1+ n))))
    (if (> n 1)
      t)))

(defun make_unique (STRING)
  "Return a sub-string of STRING which is unique in \"ansys_commands\".
STRING is more than 4 characters
  long."
  (let ((l (length STRING)) (n 4) (str ""))
    (while (<= n l)
      (setq str (substring STRING 0 n))
      (if (double_entry_p str)
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

;; 3 is for Ansys v12
(defun Prepare_list3 (List)
  "simplified fontification v12.
Argument LIST is a list of ansys commands."
  (let ( tmp tmp2 tmp_list l l2)
    (message "Preparing Ansys font lock regexps...")
    (dolist (M List tmp_list)
      (setq l (length M))		;FIXME: cond is better
      (cond ((written_out_p M)		
	     (setq tmp
		   (list
		    ;; we take into account $ command sequences
		    (concat "\\(^\\|\\$\\)\\s-*\\(" (asterisk M) "\\)\\>") ;/*~ aren't word characters!
		    '(2 font-lock-type-face keep))
		   tmp_list (cons tmp tmp_list)))
	    ((< l 4)			;shorter stuff
	     (setq tmp
		   (list
		    (concat "\\(^\\|\\$\\)\\s-*\\(" (asterisk M) "\\)\\>")	;condensed input line
		    '(2 font-lock-type-face keep))
		   tmp_list (cons tmp tmp_list)))
	    ((= l 4)			;short stuff
	     (setq tmp
		   (list
		    (concat "\\(^\\|\\$\\)\\s-*\\(" (asterisk M) "\\)")	;condensed input line
		    '(2 font-lock-type-face keep)) 
		   tmp_list (cons tmp tmp_list)))
	    ((> l 4) ;;long command names
	     (setq tmp2 (make_unique M)
		   l2 (length tmp2)
		   tmp (list
			(concat "\\(^\\|\\$\\)\\s-*\\(" (asterisk tmp2) "\\)")
				'(2 font-lock-type-face keep))
		   tmp_list (cons tmp tmp_list)))))))

(defun Ansys-initialize-completions ()
  "Create an alist for Ansys completions.
Function names are distinguished by `()'."
;  (when ansys-completion-alist-flag
  (mapcar '(lambda (var) (cons var var))
	  (append
	   Ansys_elements
	   Ansys_commands
	   (mapcar '(lambda (str)
		      (concat str "()"))
		   Ansys_get_functions)
	   (mapcar '(lambda (str)
		      (concat str "()"))
		   Ansys_parametric_functions))))

(let* ((dir "")
       (file1 (concat dir "ansys_keywords.txt"))
       (file2 (concat dir "ansys-dynprompt.txt"))
       (file3 (concat dir "ansys_elements.txt"))
       (file4 (concat dir "ansys_get_functions.txt"))
       (file5 (concat dir "ansys_parametric_functions.txt"))
       (buffer (find-file (concat dir "ansys-font-lock.el")))
       list
       elements)
  ;; ---------- command parameter help ----------
  (with-temp-buffer
    (insert-file-contents file1)
    (insert-file-contents file2)
    (setq sort-fold-case t)
    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\(.\\w*\\>\\).*\n\\1.*" nil t)
      (add-to-list 'list (match-string 0) 'append)))
  (set-buffer buffer)
  (delete-region (point-min) (point-max))
  ;; (goto-char (point-min))
  (insert (concat
	   "(defconst ansys-dynamic-prompt ;ansys/docu/dynprompt"
	   Ansys-version ".ans\n'"))
  (setq print-length nil)		;nil: print all members of list
  (prin1 list buffer)
  (insert "\n\"Help strings for the parameters of Ansys keywords.\"\n)\n")
  (message "command parameter help...done")
  ;; no formatting, it interferes with the output!

  ;; ---------- undocumented commands ----------
  (goto-char (point-min))
  (insert "(defconst ansys-undocumented-commands\n'")
  (prin1 Ansys_undocumented_commands buffer)
  (insert "\n\"Ansys commands not documented in the manuals.
Seen mainly in Workbench output files and Ansys verification
models.\"\n)\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  (message "undocumented commands...done")

  ;; ---------- ansys elements ----------
  (setq list ())			;initialise list
  (with-temp-buffer
    (insert-file-contents file3)
;    (setq sort-fold-case t)
;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\w+\\>" nil t)
      (add-to-list 'list (match-string 0) 'append)))
  ;; (setq Ansys_elements list)
  (set-buffer buffer)
  (goto-char (point-min))
  (insert (concat
	   "(defconst ansys-elements\n'"))
  (setq print-length nil)		;nil: print all members of list
 (prin1 (Ansys-elements list) buffer)
  (insert "\n\"Ansys element library.\"\n)\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  (message "elements...done")

  ;; ---------- get functions ----------
  (setq list ())			;initialise list
  (with-temp-buffer
    (insert-file-contents file4)
;    (setq sort-fold-case t)
;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\w+\\)(" nil t)
      (add-to-list 'list (match-string 1) 'append)))
  (set-buffer buffer)
  (goto-char (point-min))
  (insert (concat
	   "(defconst ansys-get-functions\n'"))
  (setq print-length nil)		;nil: print all members of list
  (setq Ansys_get_functions list)	;we need this variable for the completions!
  (prin1 (Ansys-get-functions list) buffer)
  (insert "\n\"Ansys get functions.\"\n)\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  (message "get-functions...done")

  ;; ---------- parametric functions ----------
  (setq list ())			;initialise list
  (with-temp-buffer
    (insert-file-contents file5)
;    (setq sort-fold-case t)
;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\w+\\)(" nil t)
      (add-to-list 'list (match-string 1) 'append)))
  (set-buffer buffer)
  (goto-char (point-min))
  (insert (concat
	   "(defconst ansys-parametric-functions\n'"))
  (setq print-length nil)		;nil: print all members of list
  (setq Ansys_parametric_functions list) ;we need this later for completions!
  (prin1 (Ansys-parametric-functions list) buffer)
  (insert "\n\"Ansys parametric functions.\"\n)\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  (message "parametric functions...done")

  ;; ---------- keywords ----------
  (setq list ())			;initialise list
  (with-temp-buffer
    (insert-file-contents file1)
;    (setq sort-fold-case t)
;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^.\\w*\\>" nil t)
      (add-to-list 'list (match-string 0) 'append)))
  (set-buffer buffer)
  (setq Ansys_commands list)
  (goto-char (point-min))
;  (debug)
  (insert "(defconst ansys-commands\n'")
  (setq print-length nil)		;nil: print all members of list
  (prin1 (Prepare_list3 list) buffer)
  (insert "\n\"Ansys keywords\"\n)\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  (message "keywords...done")
  
  ;; ---------- completions ----------
  (goto-char (point-min))
  (insert "(defvar ansys-completion-alist\n'")
  (setq print-length nil)		;nil: print all members of list
  (prin1 (Ansys-initialize-completions) buffer)
  (insert "\n\"Alist of Ansys symbols for completion in Ansys mode.
Each element looks like (VAR . VAR), where the car and cdr
are the same symbol (an Ansys command or variable name).  By
default keywords, get-functions, parametric-function and elements
are completed.\"\n)\n")
  (beginning-of-defun)
  (fill-paragraph nil)
  (message "completions...done\n Ansys font-lock formatting done.")
;  (message (car list))
;  (save-buffer)
  )

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

;; (defvar ansys_commands_without_arguments
;;   '(
;;     "*ENDIF" "*ENDDO" "*END" "*CFCLOS" "*CYCLE" "*ELSE" 
;;     "*EXIT" "/PMACRO" "*VSTAT" 
;;     "/AUX2" "/AUX3" "/AUX12" "/AUX15"
;;     "/POST1" "/POST26" "/PREP7"
;;     "SSUM"
;;     )
;;   "Ansys commands which aren't allowed with arguments"
;;   )


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


;; Local Variables:
;; mode: outline-minor
;; time-stamp-active: t
;; End:
