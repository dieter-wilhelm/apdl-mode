;;; fontification.el --- building keywords and completions

;; Copyright (C) 2006 - 2020 H. Dieter Wilhelm

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Version: 20.2.0
;; Package-Requires: ((emacs "25"))
;; Keywords: languages, convenience, tools, Ansys, APDL
;; URL: https://github.com/dieter-wilhelm/apdl-mode

;; Maintainer: H. Dieter Wilhelm

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

;;; Commentary:

;; at the moment it isn't working in batch mode but loading this file
;; does work...

;; HINT: every line in the *.txt files starting with an # will be ignored
;; 1.) command parameter help: copy file
;; (call-process "cp" nil "*tmp*" t "/appl/ansys_inc/162.0/v162/ansys/docu/dynprompt162.ans" "./apdl_dynprompt.txt")
;; (call-process "cp" nil "*tmp*" t "dynprompt193.ans" "./apdl_dynprompt.txt")
;;     ansys_inc/vXXX/ansys/docu/dynpromptXXX.ans -> `apdl_dynprompt.txt'
;;     done for v12,v13,v14,v145,v150,v162, v195

;; 2.) elements: copy within the ansys help browser from the
;;     Element reference from the table of contents the list -> `apdl_elements.txt'
;;     (/appl/ansys_inc/v162/commonfiles/help/en-us/help/ans_elem/toc.toc xml file)
;;     done: v12,v13,v14,v145,v150, v162, v201

;; 3.) command reference: keywords:
;;     apdl * commands and regular Ansys commands

;;     use: extract_tags.py
;; (call-process "cp" nil "*tmp*" t "/appl/ansys_inc/162.0/v162/commonfiles/help/en-us/help/ans_cmd/Hlp_C_CmdTOC.html" "/HOME/uidg1626/apdl-mode/trunk")
;; (call-process "cp" nil "*tmp*" t "Hlp_C_CmdTOC.html" "apdl_keywords.txt")
;;     cp Hlp_C_CmdTOC.html to ./ -> apdl_keywords.txt
;;     done: v13,14,145,150, v162, v201

;;     dated: previously did copy&pasted from the ansys help
;;       ->`apdl_keywords.txt' kill /eof from the keywords (see:
;;       -font-lock-keywords) <- don't know why, seems to work now
;;       done: v12 ----

;; 4.) parametric functions: seem to remain rather fixed (V13 same as
;;     in V12) ansys APDL guide chapter 3.8 using Parameters ->
;;     parametric functions, trigonometric functions and their inverse
;;     functions must be separated by hand!!! The same applies to
;;     hyperbolic functions there was a new one introduced in V14
;;     ->`apdl_parametric_functions.txt'

;; 5.) get functions: ansys 11.0 APDL Programmer's guide Appendix B.
;;     i) one has to comment (#) out some headlines!!!
;;     ii) Prepend some string function descriptions with their name e.g.
;;         StrOut = STRCAT(... with STRCAT(...)
;;     get function summary ->`apdl_get_functions.txt'

;; 6.) search index for the html help in /commonfiles/help/`apdl_Index.hlp'
;;      (call-process "cp" nil "*tmp*" t "/appl/ansys_inc/162.0/v162/commonfiles/help/ansys_Index.hlp" ".")
;;     the rest does code below
;;     done v145, v150, v162, v201
;;     cp ansys_Index.hlp??? and check variable apdl-help-index

;; _RETURN values are now documented in the -skeleton-information.
;; _RETURN values from APDL guide chapter 4.6 (Ansys 11) 5.6 (Ansys 13)

;;; necessary variables, to be maintained:
;; 1.) `apdl_undocumented_commands' from the release notes,
;;  v201/commonfiles/help/en-us/help/ai_rn/global_releasenotes.html
;;     done 145,150,v162, no undocumented in v201
;; 2.) `apdl_deprecated_elements_alist' APDL documentation ->
;;       Feature archive: legacy elements, v201: archived elements?
;;    done 145,150, v162, v201
;; 3.) `Apdl_written_out_commands'
;; 4.) `Apdl_commands_without_arguments'
;;

;;; Code:

(defconst Apdl_undocumented_commands    ;or macros?
  '(
    "/WB"                     ; signify a WB generated input file
    "XMLO"
    "/XML"
    "CNTR" ; ,print,1 ! print out contact info and also make no initial contact an error
    "EBLOCK" "CMBLOCK" "NBLOCK" "/TRACK" "CWZPLOT" "~EUI"
    "NELE"   ; predecessor of NSLE, NELE (WorkBench 10.0 output) still
;; working in Ansys 11.0 though
    "EALL"   ;esel,all? (WorkBench 10.0)
    "NALL"   ;nsel,all? (WB 10.0)
    "FLITEM" ;log command
    "LSLN"
    "PSOLVE"
    "ASLN"
    "/VERIFY"                           ;verification run?
    "/SSS"                              ;scale plot values /sss,3
    "~CFIN"                             ;Ansys 11.0
    "*EVAL"                             ;Ansys 11.0
    "*MOONEY"                           ;Ansys 11.0
    "/RUNSTAT"                          ; 13.0
    "ALPFILL"
    "ARCOLLAPSE"
    "ARDETACH"
    "ARFILL"
    "ARMERGE"
    "ARSPLIT"
    "CEWRITE" ;.dat: ansys.net: write out constraint equations into jobname.ce
    "FIPLOT"
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
    "/OPT"                              ;v14
    "OPEQN"
    "OPFACT"
    "OPFRST"
    "OPGRAD"
    "OPKEEP"
    "OPLOOP"
    "OPPRNT"
    "OPRAND"
    "OPSUBP"
    "OPSWEEP"
    "OPTYPE"
    "OPUSER"
    "OPVAR"
    "OPADD"
    "OPCLR"
    "OPDEL"
    "OPMAKE"
    "OPSEL"
    "OPANL"
    "OPDATA"
    "OPRESU"
    "OPSAVE"
    "OPEXE"
    "OPLFA"
    "OPLGR"
    "OPLIST"
    "OPLSW"
    "OPRFA"
    "OPRGR"
    "OPRSW"
    "PILECALC"
    "PILEDISPSET"
    "PILEGEN"
    "PILELOAD"
    "PILEMASS"
    "PILERUN"
    "PILESEL"
    "PILESTIF"
    "PLVAROPT"
    "PRVAROPT"
    "TOCOMP"
    "TODEF"
    "TOFREQ"
    "TOTYPE"
    "TOVAR"
    "TOEXE"
    "TOLOOP"
    "TOGRAPH"
    "TOLIST"
    "TOPLOT"
    "TOPRINT"
    "TOSTAT"
    "TZAMESH"
    "TZDELE"
    "TZEGEN"
    "XVAROPT"                    ;v14 end
    "PGSAVE"
    "SOLCONTROL"
    "TOTAL"
    "VTGEOM"
    "VTREAL"
    "VTSTAT"
    "PGRAPH"
    "/VT"
    "VTIN"
    "VTRFIL"
    "VTTEMP"
    "PGRSET"
    "VTCLR"
    "VTMETH"
    "VTRSLT"
    "VTVMOD"
    "PGSELE"
    "VTDISC"
    "VTMP"
    "VTSEC"
    "PGWRITE"
    "VTEVAL"
    "VTOP"
    "VTSFE"
    "POUTRES"
    "VTFREQ"
    "VTPOST"
    "VTSL"                              ;end of 14.5
    "FLDATA1-40"                        ;beginning of 15.0
    "HFPCSWP"
    "MSDATA"
    "MSVARY"
    "QFACT"
    "FLOCHECK"
    "HFPOWER"
    "MSMASS"
    "PERI"
    "SPADP"
    "FLREAD"
    "HFPORT"
    "MSMETH"
    "PLFSS"
    "SPARM"
    "FLOTRAN"
    "HFSCAT"
    "MSMIR"
    "PLSCH"
    "SPFSS"
    "HFADP"
    "ICE"
    "MSNOMF"
    "PLSYZ"
    "SPICE"
    "HFARRAY"
    "ICEDELE"
    "MSPROP"
    "PLTD"
    "SPSCAN"
    "HFDEEM"
    "ICELIST"
    "MSQUAD"
    "PLTLINE"
    "SPSWP"
    "HFEIGOPT"
    "ICVFRC"
    "MSRELAX"
    "PLVFRC"
    "HFEREFINE"
    "LPRT"
    "MSSOLU"
    "/PICE"
    "HFMODPRT"
    "MSADV"
    "MSSPEC"
    "PLWAVE"
    "HFPA"
    "MSCAP"
    "MSTERM"
    "PRSYZ"                             ;endo of v150
    )
  "APDL commands not documented in the manuals.
Either from dropped technologies or seen in Workbench output
files, old macros or old APDL verification models.")

(defconst Apdl_written_out_commands
  '(
    "*END" "*ENDIF"
    "*ELSEIF"                         ;allows arguments behind it
    "*ENDDO" "/POST1" "/POST26" "/PREP7"
    )
  "Commands which aren't allowed to be abbreviated.
And the solver won't allow characters appended to.")


(defconst Apdl_commands_without_arguments
  '(
    "*ENDIF" "*ENDDO" "*END" "*CFCLOS" "*CYCLE" "*ELSE"
    "*EXIT" "/PMACRO" "*VSTAT"
    "/AUX2" "/AUX3" "/AUX12" "/AUX15"
    "/POST1" "/POST26" "/PREP7"
    "SSUM"
    )
  "APDL commands which aren't allowed with arguments.")

;; deprecated element list is from Ansys version 12.0, 13.0, 14.0, 16.2, archived elements v201
(defconst Apdl_deprecated_elements_alist
  '(
    ("BEAM3"    . "BEAM188")
    ("BEAM4"    . "BEAM188")            ;legacy v162, v201
    ("BEAM23"   . "BEAM188")
    ("BEAM24"   . "BEAM188")
    ("BEAM44"   . "BEAM188")
    ("BEAM54"   . "BEAM188")
    ("CONTAC12". "CONTA178")           ;v150; v201
    ("CONTAC52". "CONTA178")           ;v162; v201
    ("CONTA171". "CONTA172")		;archived v201
    ("CONTA173". "CONTA174")		;archived v201
    ("CONTA176". "CONTA177")		;archived v201
    ("COMBIN7"  . "MPC184")
    ("FLUID79" . "Fluid29")             ;v150; v201
    ("FLUID80" . "Fluid29")             ;v150; v201
    ("FLUID81" . "Fluid29")             ;v150; v201
    ("FLUID141" . "CFX")                ;v145
    ("FLUID142" . "CFX")                ;v145
    ("INFIN9"   . "INFIN110")           ;v14
    ("INFIN47"  . "INFIN111")           ;v14
    ("PIPE16"   . "PIPE288")            ;legacy v162, v201
    ("PIPE18"   . "ELBOW290")           ;legacy v162, v201
    ("PLANE13"  . "PLANE223")           ;v14
    ("PLANE25"  . "PLANE272")           ;v14
    ("PLANE42"  . "PLANE182")		;v201
    ("PLANE53"  . "PLANE233")           ;v14
    ("PLANE67"  . "PLANE223")
    ("PLANE82"  . "PLANE183")		;v201
    ("PLANE83"  . "SOLID273")           ;v14
    ("PLANE145" . "-")                  ;p-elements are out in 13.0
    ("PLANE146" . "-")
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
    ("PIPE59"   . "PIPE288")		;archived v201
    ("PIPE60"   . "ELBOW290")
    ("SHELL41"  . "SHELL181")          ;v14
    ("SHELL43"  . "SHELL181")
    ("SHELL57"  . "SHELL131")
    ("SHELL63"  . "SHELL181")		;archived v201
    ("SHELL91"  . "SHELL281")
    ("SHELL93"  . "SHELL281")
    ("SHELL99"  . "SHELL281")
    ("SHELL150" . "-")                 ;p-elements are out in 13.0
    ("SOLID5"   . "SOLID226")            ;v14
    ("SOLID45"  . "SOLID185")		 ;archived v201
    ("SOLID46"  . "SOLID185")
    ("SOLID65" .  "SOLID185")          ;v145; v201
    ("SOLID69"  . "SOLID226")
    ("SOLID92"  . "SOLID187")		;archived v201
    ("SOLID95"  . "SOLID186")		;archived v201
    ("SOLID117" . "SOLID236")          ;v14
    ("SOLID127" . "-")                 ;p-elements are out in 13.0
    ("SOLID128" . "-")
    ("SOLID147" . "-")
    ("SOLID148" . "-")
    ("SOLID191" . "SOLID186")
    ("VISCO88"  . "PLANE183")
    ("VISCO89"  . "SOLID186")
    ("VISCO106" . "PLANE182")
    ("VISCO107" . "SOLID185")
    ("VISCO108" . "PLANE183")
    ("TRANS109" . "PLANE223")          ;v14
    )
  "Association list of deprecated element types with their proposed successor.")

;; (defun APDL-elements (list)
;;       (mapcar
;;        '(lambda (name)
;;        (list
;;         (concat "\\<\\(" name "\\)\\>")
;;         1
;;         'font-lock-variable-name-face
;;         'keep               ;do not overwrite comment-faces
;;         ))
;;        list))

(defun APDL-get-functions (list)
  (mapcar
   #'(lambda (name)
       (list
        (concat "\\(" name "\\)\\s-*(")
        1
        'font-lock-function-name-face
        'keep))
   list))

(defun APDL-parametric-functions (list)
  (mapcar
   #'(lambda (name)
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
;;     1.) no_parameter_p Apdl_commands_without_arguments
;;     2.) not written_out_p
;; TODO: what is with $ condensation?

;; c.) regexp for commands which allow characters and arguments behind it
;;     1.) length >= 4 but not unique

(defun written_out_p(STRING)
  "Returns true if STRING is a member of
  \"Apdl_written_out_commands\""
  (member STRING Apdl_written_out_commands))

;; (written_out_p "*END")

(defun Prepare_list (List)
  "Simplified fontification v12.
Argument LIST is a list of ansys commands. Return a list of
command strings"
  (let ( tmp tmp2 tmp_list l l2)
    (message "Preparing APDL font lock strings, list-length: %d." (length List))
    (dolist (M List tmp_list)
      (setq l (length M))
      (cond ((written_out_p M)
             (setq tmp_list (cons M tmp_list)))
            ((<= l 4)               ;shorter stuff
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
    (message "Preparing APDL font lock 2 strings...")
    (dolist (M List)
      (setq l (length M))
      (cond  ;written out stuff
       ((written_out_p M)
        (setq list_a (cons M list_a)))
       ((< l 4)               ;short stuff
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

(defun APDL-initialize-completions ()
  "Create a list for APDL completions.
Function names are distinguished by `()'."
  (append
   Apdl_elements
   Apdl_commands
   (mapcar #'(lambda (str) (concat str "()"))
           Apdl_get_functions)
   (mapcar #'(lambda (str) (concat str "()"))
           Apdl_parametric_functions)))

;; ------------------------------------------------------------

(let ((buffer (find-file "apdl-keyword.el"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; we are adding to the top!
;;

  (insert "(provide 'apdl-keyword)\n\n;;; apdl-keyword.el ends here\n")

;; ---------- undocumented commands ----------

;; getting another colour than regular commands
  (goto-char (point-min))
  (setq undocumented-commands Apdl_undocumented_commands)
  (insert "(defconst apdl-undocumented-command-regexp\n")
  (prin1 (regexp-opt Apdl_undocumented_commands) buffer)
  (insert "\n\"Regexp of commands not documented in the Ansys manuals.
Seen mainly in Workbench output files and Ansys verification models.\")\n\n")
;; (beginning-of-defun)
;; (fill-paragraph 0)
  (message "undocumented commands...done")

;; ---------- command parameter help ----------
  (setq list nil)
;; first fill list with documented commands
  (with-temp-buffer
    (insert-file-contents "apdl_keywords.txt")
    (insert-file-contents "apdl_dynprompt.txt")
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (setq sort-fold-case t)
    (sort-lines nil (point-min) (point-max))
                                        ;(write-file "keyw+promt.txt")
    (goto-char (point-min))
    (while (re-search-forward "^\\(.\\w*\\>\\).*\n\\1.*" nil t)
      (add-to-list 'list (match-string 0) 'append)))
;; now include the undocumented commands
  (setq list (append list
                     (mapcar #'(lambda (m) (concat m " - APDL undocumented command\n" m))
                             Apdl_undocumented_commands)))
;;  (sort list 'string<)
  (set-buffer buffer)
  (goto-char (point-min))
  (insert "(defconst apdl-dynamic-prompt\n'")
  (setq print-length nil)          ;nil: print all members of list
  (prin1 list buffer)
  (insert "\n\"Help strings for the parameters of APDL keywords.\")\n\n")
  (message "command parameter help...done")
;; no formatting, it interferes with the output!

;; ---------- keywords ----------

;; ----- documented + written out + undocumented commands ---
;; 0.) unique, solver type
;; 1.) full APDL name same as completed
;; 2.) from unique to full name


  (setq list ())               ;initialise list
  (with-temp-buffer
    (insert-file-contents "apdl_keywords.txt")
;;    (setq sort-fold-case t)
;;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^.\\w*\\>" nil t)
      (add-to-list 'list (match-string 0) 'append)))
  (set-buffer buffer)
  (setq commands list)

  (message "starting minimal level.")
  (goto-char (point-min))
  (setq commands-1 (append commands Apdl_undocumented_commands))
;;  (debug)
  (insert "(defconst apdl-command-regexp\n")
  (setq print-length nil)          ;nil: print all members of list
  (prin1 (regexp-opt (Prepare_list commands-1))
         buffer)
  (insert "\n\"APDL short keyword name regexp.\")\n\n")

  (message "starting level 1.")
  (goto-char (point-min))
  (insert "(defconst apdl-command-regexp-1\n")
  (prin1 (regexp-opt commands)
         buffer)
  (insert "\n\"APDL full keyword name regexp.\")\n\n")

  (message "starting level 2.")
  (goto-char (point-min))
;;  regexp becoming too big for font-lock!!! when keywords are around 4000
  (setq list (Prepare_list_2 commands)
        list1 (car list)
        list2 (nth 1 list)
        list3 (nth 2 list)
        list1 (regexp-opt list1)     ;written out stuff
        list2 (regexp-opt list2)
        list3 (regexp-opt list3))
  (insert "(defconst apdl-command-regexp-2a\n")
  (prin1 list1 buffer)
  (insert "\n\"APDL keyword name regexp 2a.\")\n\n")
;;
  (insert "(defconst apdl-command-regexp-2b\n")
  (prin1 list2 buffer)
  (insert "\n\"APDL keyword name regexp 2b.\")\n\n")
;;
  (insert "(defconst apdl-command-regexp-2c\n")
  (prin1 list3 buffer)
  (insert "\n\"APDL keyword name regexp 2c.\")\n\n")


;; ---------- ansys elements ----------

;; --- regular elements ---
  (setq list ())               ;initialise list
  (with-temp-buffer
    (insert-file-contents "apdl_elements.txt")
;;    (setq sort-fold-case t)
;;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\w+\\>" nil t)
      (add-to-list 'list (match-string 0) 'append)))
  (setq elements list)          ;needed for completion list
  (set-buffer buffer)
  (goto-char (point-min))
  (insert (concat
           "(defconst apdl-element-regexp\n"))
  (setq print-length nil)          ;nil: print all members of list
  (prin1 (regexp-opt list 'words) buffer)
  (insert "\n\"APDL elements regexp.\")\n\n")
  (beginning-of-defun)
  (fill-paragraph 0)

;; --- deprecated elements ---
  (goto-char (point-min))
  (insert (concat
           "(defconst apdl-deprecated-element-regexp\n"))
  (setq print-length nil)          ;nil: print all members of list
  (setq deprecated-elements (mapcar 'car Apdl_deprecated_elements_alist))
  (prin1 (regexp-opt deprecated-elements 'words) buffer)
  (insert "\n\"APDL deprecated elements regexp.\")\n\n")
  (beginning-of-defun)
  (fill-paragraph 0)
;; -- write out the alist for a possible reference --
  (goto-char (point-min))
  (insert (concat
           "(defconst apdl-deprecated-element-alist\n'"))
  (setq print-length nil)          ;nil: print all members of list
  (setq deprecated-elements (mapcar 'car Apdl_deprecated_elements_alist))
  (prin1 Apdl_deprecated_elements_alist buffer)
  (insert "\n\"Association list for APDL deprecated elements.
Together with their proposed replacements.\")\n\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  (message "elements...done")

;; ---------- get functions ----------

  (setq list ())               ;initialise list
  (with-temp-buffer
    (insert-file-contents "apdl_get_functions.txt")
;;    (setq sort-fold-case t)
;;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\w+\\)(" nil t)
      (add-to-list 'list (match-string 1) 'append)))
  (setq get-functions list)     ;we need this variable for the completions!
  (set-buffer buffer)
  (goto-char (point-min))
  (insert (concat
           "(defconst apdl-get-function-regexp\n"))
  (setq print-length nil)          ;nil: print all members of list
  (prin1 (regexp-opt get-functions) buffer)
  (insert "\n\"APDL get function regexp.\")\n\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  (message "get-functions...done")

;; ---------- parametric functions ----------

  (setq list ())               ;initialise list
  (with-temp-buffer
    (insert-file-contents "apdl_parametric_functions.txt")
;;    (setq sort-fold-case t)
;;    (sort-lines nil (point-min) (point-max))
    (delete-matching-lines "^#.*" (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\w+\\)(" nil t)
      (add-to-list 'list (match-string 1) 'append)))
  (setq parametric-functions list) ;we need this later for completions!
  (set-buffer buffer)
  (goto-char (point-min))
  (insert (concat
           "(defconst apdl-parametric-function-regexp\n"))
  (setq print-length nil)          ;nil: print all members of list
  (prin1 (regexp-opt parametric-functions) buffer)
  (insert "\n\"APDL parametric function regexp.\")\n\n")
  (beginning-of-defun)
  (fill-paragraph 0)
  (message "parametric functions...done")

;; ---------- completions ----------

  (goto-char (point-min))
;; (setq functions
;;      (map 'list '(lambda (s) (concat s "()"))
;;           parametric-functions get-functions))

  (setq get-functions
        (mapcar #'(lambda (s) (concat s "()")) get-functions))
  (setq parametric-functions
        (mapcar #'(lambda (s) (concat s "()")) parametric-functions))
  (insert "(defconst apdl-completions\n'")
  (setq print-length nil)          ;nil: print all members of list
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
  (insert "\n\"APDL symbols for completion in APDL-Mode.
By default APDL keywords, get-functions, parametric-function and elements
- deprecated as well - are completed.\")\n\n")
;; (beginning-of-defun)
;; (fill-paragraph nil)
  (message "completions...done")

;; ---------- help index ----------

  (message "starting help index.")
  (with-temp-buffer
    (setq list ())               ;initialise list
    (insert-file-contents "ansys_Index.hlp")
;; clean up redundant keywords
    (delete-matching-lines "^Hlp")
;; (goto-char (point-min))
;; (delete-matching-lines "^Hlp_E_")
;; (goto-char (point-min))
;; ;; shorten keywords
;; (while (re-search-forward "^Hlp_UI" nil t)
;;   (replace-match "UI" nil nil))
;; (goto-char (point-min))
;; (while (re-search-forward "^Hlp_G" nil t)
;;   (replace-match "G" nil nil))
;; (goto-char (point-min))
;; (while (re-search-forward "^Hlp_R" nil t) ;releas notes
;;   (replace-match "R" nil nil))
;; (goto-char (point-min))

    (while (re-search-forward "^SHELLS" nil t)
      (replace-match "\"SHELLS\"" nil nil))
    (goto-char (point-min))
    (while (re-search-forward "^PLANE " nil t)
      (replace-match "\"PLANES\"" nil nil))
    (goto-char (point-min))
    (while (re-search-forward "^SOLID " nil t)
      (replace-match "\"SOLIDS\"" nil nil))
    (goto-char (point-min))
;; Replace suffix ALL with "ALL"
    (while (re-search-forward "^ALL" nil t)
      (replace-match "\"all\"" nil nil))
    (goto-char (point-min))

;; skip the first line
    (next-line)
;; (dotimes (i 10) (re-search-forward
    (while (re-search-forward
            "^\\([^[:space:]]+\\)[[:space:]]+\\([^[:space:]]+\\)$" nil t)
      (add-to-list 'list (list (match-string 1) (match-string 2)) 'append)))
  (add-to-list 'list (list "\"RELEASE NOTES\"" "ai_rn/global_releasenotes.html") 'append)
  (add-to-list 'list (list "\"CONTACT TECHNOLOGY GUIDE\"" "ans_ctec/ctectoc.html") 'append)
  (add-to-list 'list (list "\"PARAMETRIC DESIGN LANGUAGE GUIDE\"" "ans_apdl/Hlp_P_APDLTOC.html") 'append)
  (add-to-list 'list (list "\"STRUCTURAL ANALYSIS GUIDE\"" "ans_str/Hlp_G_StrTOC.html") 'append)
  (add-to-list 'list (list "\"ADVANCED ANALYSIS TECHNIQUES GUIDE\"" "ans_str/Hlp_G_AdvTOC.html") 'append)
  (add-to-list 'list (list "\"MATERIAL MODELS\"" "ans_mat/ans_mat.html") 'append)
  (add-to-list 'list (list "\"THEORY REFERENCE\"" "ans_thry/ansys.theory.html") 'append)
  (message "adding help index...")
  (set-buffer buffer)
  (goto-char (point-min))
  (insert (concat
           "(defconst apdl-help-index\n'"))
  (setq print-length nil)          ;nil: print all members of list
  (prin1 list buffer)
  (insert "\n\"Ansys help index alist.\")\n\n")
  (beginning-of-defun)
;;  (fill-paragraph 0)
  (message "help index...done.")

;; ---------- header ----------

  (goto-char (point-min))
  (insert ";; apdl-keyword.el --- APDL-Mode completion and"
          "highlighting variables. -*- lexical-binding:t -*-\n" ";; This file was built by"
          "\"fontification.el\" release 20.2.0.\n\n"
          ";; Copyright (C) 2006 - 2020 H. Dieter Wilhelm.\n\n"
          ";; Version: 20.2.0\n"
          ";; URL: https://github.com/dieter-wilhelm/apdl-mode\n\n"
          ";;; Commentary:\n"
          ";;  Code for APDL command help and command completions.\n\n"
          ";;; Code:\n\n")
  (save-buffer)
  (message "apdl-keywords.el done.")
;; --- end of let
  )

;;; fontification.el ends here

;; Local Variables:
;; mode: outline-minor
;; time-stamp-active: t
;; End:
