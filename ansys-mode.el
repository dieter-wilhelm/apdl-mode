;;; ansys-.el --- Emacs support for working with Ansys FEA.

;; Time-stamp: "2009-11-25 17:04:54 uidg1626"

;; Copyright (C) 2006 - 2009  H. Dieter Wilhelm

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Maintainer: H. Dieter Wilhelm
;; Created: 2006-02
;; Version: 12.0.1
;; Keywords: Languages, Convenience

;; Parts of this mode were originally base on octave-mod.el: Copyright
;; (C) 1997 Free Software Foundation, Inc.  Author: Kurt Hornik
;; <Kurt.Hornik@wu-wien.ac.at> Author: John Eaton
;; <jwe@bevo.che.wisc.edu>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:

;; This Emacs Lisp package provides support for the FEA (Finite
;; Element Analysis) program Ansys (http://www.ansys.com) under
;; Windows and Unix systems.  It defines 'Ansys mode', a major mode
;; for viewing, writing and navigating in APDL (Ansys Parametric
;; Design Language) files as well as providing managing and
;; communication capabilities for an associated Ansys solver process.

;; The mode's capabilities are rather sophisticated but the
;; documentation is still targeted for Ansys users with little Emacs
;; experience.  Please consult the accompanying README file for
;; further, especially, installation information.

;;; History:

;; Please consult the accompanying README file.

;;; Code:

(defconst ansys_version "12.0"		;NEW_C
  "Ansys version on which Ansys mode is based.")

(defconst ansys_mode_version "1"	;NEW_C
  "Ansys mode minor version number.")

;; --- defcustoms ---

(require 'custom)

(defgroup Ansys nil			;NEW_C from Octave-Mod.el
  "Customisation group for the Ansys mode."
  :version "23.1"
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :link '(url-link "http://www.emacswiki.org")
  :link '(url-link "http://www.code.google.com/p/ansys-mode")
  :group 'Languages)

(defcustom ansys-dynamic-highlighting-flag nil ;NEW_C
  "Non-nil means that Ansys mode highlights user defined variables.
Warning: This option is computational expensive and--depending on
the file size and your system--it might make your editing
experience somewhat sluggish.  Currently dynamic highlighting of
user variables is only implemented for files with the extension
\".mac\" otherwise the fontification of variables is only static.
To take effect after setting this variable you have to recall
`ansys-mode'."
  :type 'boolean
  :group 'Ansys)

(defcustom ansys-program ""		;NEW_C
  "The Ansys executable name.
When the file is not in your search path, you have to funish the
complete path specification.  For example:
\"/ansys_inc/v120/ansys/bin/ansys120\"."
  :type 'string
  :group 'Ansys)

(defcustom ansys-help-file ""		;NEW_C
  "The Ansys \"Help System\" file name.
It is called with \\[ansys-start-ansys-help].  When the file is
not in your search path, you have to funish the complete path
specification.  For example:
\"/ansys_inc/v120/ansys/bin/anshelp120\" or with the windows OS
\"c:\\\\Program\ Files\\Ansys\ Inc\\v120\\CommonFiles\\HELP
\\en-us\\ansyshelp.chm\"."
  :type 'string
  :group 'Ansys)

(defcustom ansys-lmutil-program ""	;NEW_C
  "The FlexLM license manager utility executable name.
When the file is not in your search path, you have to furnish the
complete path.  For example:
\"/ansys_inc/shared_files/licensing/linop64/lmutil\" or in the
windows case \"c:\\\\Program Files\\Ansys Inc\\Shared\ Files
\\Licensing\\intel\\anslic_admin.exe.  Necessary for the
command `ansys-license-status'."
  :type 'string
  :group 'Ansys)

(defcustom ansys-license-file nil ;NEW_C
  "The FlexLM license file name or license server specification(s).
The license server specification(s) must include the port number
when it isn't 1055, i. e. the default port number:
port_number@server_name, use the colon for multiple servers, for
example \"27005@rbgs421x:27005@rbgs422x\".

System settings and order of precedence: 1. ANSYSLMD_LICENSE_FILE
environment variable, 2.)  The FLEXlm resource file: ~/.flexlmrc
on Unix or somewhere in the Windows registry. 3.) The
LM_LICENSE_FILE variable. 4.) The ansyslmd.ini file in the
licensing directory (This is what anslic_admin is doing in an
Ansys recommended installation).  5.) The license file itself."
  :type 'string
  :group 'Ansys)

(defcustom ansys-license-types		;NEW_C
  '("ansys" "struct" "ane3" "ansysds" "ane3fl" "preppost")
  "List of available license types to choose for a run.
This list should contain the license types you can choose from.  Below
are often used license types (as e.g. seen with the function
`ansys-license-status') and their corresponding WorkBench
terminology.

\"ansys\" - Mechanical U (without thermal capability)
\"struct\" - Structural U (with thermal capability)
\"ane3\" - Mechanical/Emag (Structural U with electromagnetics)
\"ansysds\" - Mechanical/LS-Dyna (Mechanical U with Ansys LS-Dyna inter-phase)
\"ane3fl\" - Multiphysics
\"preppost\" - PrepPost (no solver capabilities)"
  :group 'Ansys)

(defcustom ansys-license "struct"		;NEW_C
  "The License type with which the Ansys solver will be started.
See `ansys-license-types' for often used Ansys license types."
;  :options '("ansys" "struct" "ane3" "ane3fl" "ansysds" "preppost")
  :options ansys-license-types
  ;; options not available for strings (only hooks, alists, plists E22)
  :type 'string
  :group 'Ansys)

(defcustom ansys-indicate-empty-lines-flag nil ;NEW_C
  "Non-nil means indicate empty lines on window systems.
Do this visually at the end of an Ansys buffer in the left
fringe.  You have to reload function `ansys-mode' for this
variable to take effect."
  :type 'boolean
  :group 'Ansys)

(defcustom ansys-current-ansys-version ansys_version ;NEW_C
  "String describing the Ansys version installed by the user.
This variable is used by the `ansys-skeleton' skeleton."
  :type 'string
  :group 'Ansys)

(defcustom ansys-comment-padding " "	;NEW_C
  "Padding string that `comment-dwim' puts between comment chars and text.
Extra spacing between the comment character(s) and the comment
text makes the comment easier to read.  This padding is not
effective for code comments (comments behind code)."
  :type 'string
  :group 'Ansys)

(defcustom ansys-comment-add 1		;NEW_C
  "How many additional comment characters are inserted by \\[comment-dwim].
This determines the default value of the numeric argument of
`comment-dwim'.  It should generally stay 0, except for a few
modes like Lisp where it can be convenient to set it to 1 so that
regions are commented with two semi-colons."
  :type 'integer
  :group 'Ansys)

(defcustom ansys-code-comment-column 25 ;NEW_C
  "Column where Ansys code comments (behind code) are placed."
  :type 'integer
  :group 'Ansys)

(defcustom ansys-auto-indent-flag t	;NEW_C
  "Non-nil means indent line when typing the SPC key.
The space character is also inserted."
  :type 'boolean
					;  :options '(t nil) ; not necessary with booleans in Customise
  :group 'Ansys)

(defcustom ansys-indent-comment-suffix "" ;NEW_C
  "String placed after the Ansys comment char in an code comment.
See `ansys-indent-comment-string'."
  :type 'string
  :group 'Ansys)

(defcustom ansys-ruler-wide-flag nil	;NEW_C
  "Non-nil means show a 80 characters wide temporary ruler.
Nil means show a narrower temporary ruler with 50 characters."
  :type 'boolean
  :group 'Ansys)

(defcustom ansys-require-spaces-flag nil ;NEW_C
  "Non-nil means \\[insert-parentheses] inserts whitespace before ().
When there is a region marked function `insert-parentheses'
inserts the parentheses around the active region."
  :type 'boolean
  :group 'Ansys)

(defcustom ansys-blink-matching-block-flag t ;NEW_C
  "Non-nil means blinking of matching Ansys block keywords.
Skip temporary to the matching beginning of the block when
inserting a newline after an *ELSE or *END keyword."
  :type 'boolean
  :group 'Ansys)

(defcustom ansys-blink-matching-delay .7 ;NEW_C
  "Time in seconds for skipping to a matching block.
See also the variable `ansys-blink-matching-block-flag'."
  :type 'number
  :group 'Ansys)

(defcustom ansys-block-offset 2		;NEW_C
  "Indentation column(s) for statements in a block structure."
  :type 'integer
					;  :options only for types hook, plist and alist
  :group 'Ansys)

(defcustom ansys-outline-string "@" ;NEW_C string building outline-regexp
  "String specifying outline headings (see `outline-regexp')."
  :type 'string
  :group 'Ansys)

(defcustom ansys-mode-hook nil ;NEW_C
  "Normal hook run before entering Ansys mode.
A hook is a variable which holds a collection of functions."
  :type 'hook
  :options '(ansys-show-paren-mode ansys-font-lock-mode ansys-outline-minor-mode ansys-ruler-mode ansys-auto-insert-mode display-time)
  :group 'Ansys)

;; --- Variables ---

(defvar ansys-run-flag nil		;NEW_C
  "Non-nil means an Ansys job is already running.")

(defvar ansys-job nil			;NEW_C
  "Variable determines the Ansys job name.
See `ansys-abort-file' for a way of stopping a run in a
controlled way and `ansys-display-error-file' for viewing the
error file.")

(defvar ansys-user-variables () ;NEW_C
  "Variable containing the user variables and first occurance.
The list is used for the fontification of these variables.")

(defvar ansys-process-name "Ansys"		;NEW_C
  "Variable containing Emacs' name of a running ansys process.
Variable is only used internally in the mode.")

(defvar ansys-is-unix-system-flag nil	;NEW_C
  "Non-nil means computer runs a Unix system.")

;; --- constants ---

(defconst ansys-parameter-substitution-commands-regexp	;NEW
  "/TITLE\\|/STITLE\\|/COM\\|/AXLAB\\|/GCOLUMN\\|/TLABEL\\|/AN3D"
  "Regexp of command names which have a string behind them."
  )

(defconst ansys-string-commands-regexp	;NEW
  "C\\*\\*\\*\\|/TITLE\\|/STITLE\\|/COM\\|/AXLAB\\|/GCOLUMN\\|/TLABEL\\|\\*ABBR\\|/AN3D"
  "Regexp of command names which have a string behind them."
  )

(defconst ansys-variable-defining-commands ;NEW FIXME: correct unnecessary full names
  '("*do" "*get\\w*" "*dim" "*set.?" ;funny *SET works only with one
				     ;additional character
    "*ask" "path" "pdef" "*vget" "*vfun" "*mfun" "*vitrp"
    "*toper""*voper" "*moper" "*sread" "*vscfun" "/inq\\w*"
    "/fil\\w*")
  "Regexps for commands which define user variables.
(Excluded the \"=\" assignment.)")

(defconst ansys-use-variables		;NEW_C
  '("ARG[1-9]" "AR[1][0-9]")
  "Variable containing the Ansys *USE variables regexp.
ARG[1-9] and AR[1][0-9] are macro local variables and can be
passed to the *USE command.  AR[2-9][0-9] are also pure macro
local variables.")

(defconst ansys-format-commands-regexp	;New
  "\\*[mM][sS][gG]\\|\\*[vV][rR][eE]\\|\\*[vV][wW][rR]\\|\\*[mM][wW][rR]"
  "Regexp of command names which have one or more format lines.")

(defconst ansys-maintainer-address	;FROM_C: Octave-mod
  "Dieter Wilhelm <dieter@duenenhof-wilhelm.de>" ;bug-gnu-emacs@gnu.org
  "Address of current maintainer of the Ansys mode.")

(defconst ansys-comment-char ?!		;FROM_C: Octave-mod
  "The Ansys comment character.")

(defconst ansys-non-code-line-regexp "^\\s-*\\($\\|\\s<\\)" ;_C
  "Regexp indicating a pure comment or an empty line.
A \"pure comment\" line contrasting a \"code comment\" which
follows code to be analysed from the Ansys interpreter.")

(defconst ansys-condensed-input-line-regexp ".*\\$" ;NEW_C
  "Regexp indicating a condensed input line.")

(defconst ansys-indent-comment-string	;_C
  (concat (char-to-string ansys-comment-char) ansys-indent-comment-suffix)
  "String to insert when creating an Ansys code comment.")

(defconst ansys-comment-start-skip "\\S<+\\S-*" ;_C
  "Regexp to match the start of an Ansys comment up to its body.
Used for the variable `comment-start-skip'.")

(defvar ansys-format "mac"	     ;FIXME:This is for the ansys macro
  "String representing the ansys format.")

;;; --- Indentation ---

(defconst ansys-continuation-line-regexp ".*?&\\s-*$" ;_C
  "Regexp indicating a continuation line (of the *MSG command).")

(defconst ansys-begin-keywords		;NEW_C
  '("\\*[dD][oO]" "\\*[dD][oO][wW][hH][iI][lL][eE]"
    "\\*[iI][fF].*[tT][hH][eE][nN]" "\\*[cC][rR][eE][aA][tT][eE]")
  "Regexps describing Ansys block begin keywords.")

(defconst ansys-else-keywords		;NEW_C
  '("\\*[eE][lL][sS][eE][iI][fF]" "\\*[eE][lL][sS][eE]"
    "\\*[cC][yY][cC][lL][eE]")
  "Regexps describing Ansys block else keywords.")

(defconst ansys-end-keywords		;NEW_C
  '("\\*[eE][nN][dD][dD][oO]" "\\*[eE][nN][dD][iI][fF]"
    "\\*[eE][nN][dD]")
  "Regexps describing Ansys end keywords.")

(defconst ansys-number-line-regexp	;NEW_C
  "^\\s-*[(+-]?[[:digit:]]"
  "Regexp describing an Ansys number line.
Used for skipping pure number lines and CMBLOCK format strings")

(defconst ansys-block-begin-regexp	;_C
  (concat "\\("
	  (mapconcat 'identity ansys-begin-keywords "\\|")
	  "\\)\\>")
  "Regexp containing the Ansys begin keywords.")

(defconst ansys-block-else-regexp	;_C
  (concat "\\("
	  (mapconcat 'identity ansys-else-keywords "\\|")
	  "\\)\\>")
  "Regexp containing the Ansys else keywords.")

(defconst ansys-block-end-regexp	;_C
  (concat "\\("
	  (mapconcat 'identity ansys-end-keywords "\\|")
	  "\\)\\>")
  "Regexp containing the Ansys end keywords.")

(defconst ansys-block-begin-or-end-regexp ;_C
  (concat ansys-block-begin-regexp "\\|" ansys-block-end-regexp)
  "Regexp containing Ansys begin and end keywords.")

(defconst ansys-block-else-or-end-regexp ;C_
  (concat ansys-block-else-regexp "\\|" ansys-block-end-regexp)
  "Regexp containing the Ansys else or end keywords.")

(defconst ansys-block-match-alist	;_C
  '(("*IF" . ("THEN" "*ELSE" "*ELSEIF" "*ENDIF"))
    ("*DO" . ("*ENDDO"))
    ("*DOWHILE" . ("*ENDDO"))
    ("*CREATE" . ("*END")))
  "Alist with Ansys's matching block keywords.
It has Ansys's begin keywords as keys and a list of the
corresponding else or end keywords as associated values.")

(defvar ansys-mode-abbrev-table nil	;_C
  "Abbreviation definition table for the Ansys mode.
All Ansys abbrevs start with a grave accent \"`\".  \"`?\" lists
the currently defined abbreviations.")

(defconst ansys-column-ruler-wide	;_C
  (propertize
   (concat
    "0        10        20        30        40        50        60        70        80\n"
    "|    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |\n")
   'font-lock-face 'bold)
  "Contains the string for the wide ruler.
Ruler strings are displayed above the current line with
\\[ansys-column-ruler].")

(defconst ansys-column-ruler-narrow	;_C
  (propertize
   (concat
    "0        10        20        30        40        50\n"
    "|    |    |    |    |    |    |    |    |    |    |\n")
   'font-lock-face 'bold)
  "Narrow ruler string.
Ruler strings are displayed above the current line with \\[ansys-column-ruler].")

;;FIXME DEFSUBSTs with DEFUNs inside aren't particularly helpful?

;;; --- predicates ---

(defun ansys-in-asterisk-comment-p ()
  "Return t if the cursor is inside an Ansys asterisk comment."
  (save-excursion
    (let ((lbp (line-beginning-position)))
      (if (search-backward " *" lbp t)
	  t
	nil))))

(defun ansys-in-string-command-line-p () ;NEW
  "Return t if in an Ansys string command line."
  (save-excursion
   (back-to-indentation)
   (looking-at ansys-string-commands-regexp)))

(defun ansys-number-line-p ()		;NEW_C
  "Return t if in an Ansys number block."
  (save-excursion
    (beginning-of-line)
    (and (not (ansys-in-format-construct-p))
	 (looking-at ansys-number-line-regexp)))) ;"(" is for CMBLOCK format string

(defun ansys-default-command-p ()	;NEW_C
  "Return t if in an Ansys default command line."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*,")))

(defun ansys-in-indentation-p ()	;_C
  "Return t if in an indentation."
  (if (and (eolp) (bolp)) ; take care of empty lines
      nil
    (let ((p (point)))
      (save-excursion
	(back-to-indentation)
	(if (<= p (point)) t nil)))))

(defun ansys-first-line-p ()		;_C
  "Return t if at the first line."
  (save-excursion
    (beginning-of-line)
    (bobp)))

(defun ansys-last-line-p ()		;_C
  "Return t if at the last line."
  (save-excursion
    (end-of-line)
    (eobp)))

(defun ansys-continuation-line-p ()	;_C
  "Return t if in a continutation line of certain commands."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ansys-continuation-line-regexp) t nil)))

(defun ansys-in-format-command-line-p ()	;_C
  "Return t if in an Ansys format command line, nil otherwise.
See the constant variable `ansys-format-commands-regexp' which
includes the commands which need formatting lines."
  (save-excursion
    (beginning-of-line)
    (if (looking-at
	 (concat "^\\s-*\\(" ansys-format-commands-regexp "\\)")) t nil)))

(defun ansys-in-format-construct-p ()	;NEW_C
  "Return t if in an Ansys format construct.
Otherwise nil, i.e. return nil when in a format command line."
  (cond ((ansys-continuation-line-p) t)
	((ansys-first-line-p) nil)
	(t (save-excursion
	     (forward-line -1)
	     (if (or
		  (ansys-continuation-line-p)
		  (ansys-in-format-command-line-p)) t nil)))))

(defun ansys-condensed-input-line-p ()	;NEW_C
  "Return t if in an Ansys condensed (... $ ...) input line."
  (save-excursion
    (beginning-of-line)
    (if (ansys-in-format-construct-p)
	nil
      (if (looking-at ansys-condensed-input-line-regexp)
	  t
	nil))))

(defun ansys-code-line-p ()		;_C
  "Return t if in an Ansys code line, nil otherwise.
A code line is the complementary of the regexp `ansys-non-code-line-regexp'."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ansys-non-code-line-regexp) nil t)))

(defun ansys-at-end-of-text-p ()	;_C
  "Return t if the cusor is at the end of text in a line."
  (if (looking-at "\\s-*$") t nil))

(defun ansys-at-end-of-code-p ()	;_C
  "Return t if the cursor is at the end of code in a line.
This means at the end of code before whitespace or an Ansys
comment."
  (if (looking-at "\\s-*$\\|\\s-*!") t nil))

(defsubst ansys-in-comment-p ()		;_C
  "Return t if the cursor is inside an Ansys comment."
  (save-excursion
    (nth 4 (parse-partial-sexp (ansys-position 'bol) (point))))) ;nth -- nth element
					;of list

(defsubst ansys-in-string-p () ;_C FIXME:are there strings defined in ansys?
  "Return t if the cursor is inside an Ansys string."
  (save-excursion
    (nth 3 (parse-partial-sexp (ansys-position 'bol) (point)))))

(defsubst ansys-in-empty-line-p()	;_C
  "Return t if the cursor is in an empty (whitespace) line."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \n\t]*$")))

(defsubst ansys-not-in-string-or-comment-p() ;_C
  "Return t if the cursor is not inside a string or comment."
  (let ((pps (parse-partial-sexp (ansys-position 'bol) (point))))
    (not (or (nth 3 pps) (nth 4 pps)))))

(defsubst ansys-in-string-or-comment-p () ;_C
  "Return t if the cursor is not inside a string or comment."
  (let ((pps (parse-partial-sexp (ansys-position 'bol) (point))))
    (or (nth 3 pps) (nth 4 pps))))


(defconst ansys-mode-map		;_C
  (let ((map (make-sparse-keymap)))
    (define-key map "`" 'ansys-abbrev-start) ;``?' lists abbrevs
					;    (define-key map "\t" 'indent-according-to-mode)		  ;redundant
    ;;     standard behaviour of M-j is sufficient for me
    (define-key map "\e\t" 'ansys-complete-symbol) ;or M-C-i
    ;; --- changed standard Emacs keybindings ---
    (define-key map " " 'ansys-electric-space)
    (define-key map "\M-j" 'ansys-indent-format-line)
    (define-key map "\n" 'ansys-reindent-then-newline-and-indent)
    ;; end block indentation

    ;; --- especially interesting for continuation lines and condensed input
    (define-key map "\M-a" 'ansys-command-start)
    (define-key map "\M-e" 'ansys-command-end)
    ;; --- command movement --- (like defuns), skip comments and empty lines
    (define-key map "\M-p" 'ansys-previous-code-line)
    (define-key map "\M-n" 'ansys-next-code-line)
    ;; --- block movements ---
    (define-key map "\C-\M-f" 'ansys-next-block-end)
    (define-key map "\C-\M-b" 'ansys-previous-block-start-and-conditional)
    (define-key map "\C-\M-n" 'ansys-skip-block-forward)
    (define-key map "\C-\M-p" 'ansys-skip-block-backwards)
    (define-key map "\C-\M-d" 'ansys-down-block)
    (define-key map "\C-\M-u" 'ansys-up-block)
    (define-key map "\C-\M-h" 'ansys-mark-block) ;formerly mark defun
    ;; --- further block keys ---
    (define-key map "\C-c]" 'ansys-close-block)
    (define-key map "\C-c}" 'ansys-number-block-end)
    (define-key map "\C-c{" 'ansys-number-block-start)
    ;; --- pairs
    (define-key map [?\M-\"] 'insert-pair) ; needs insert-pair-alist in a
    (define-key map "\C-c[" 'insert-pair)
    (define-key map "\C-c'" 'insert-pair)
    (define-key map "\C-c%" 'insert-pair)
    (define-key map [?\C-c?\C-%] 'insert-pair)
    (define-key map [?\C-c?\C-[] 'insert-pair)
    (define-key map [?\C-c?\C-'] 'insert-pair)
    ;; --- miscellaneous ---
    (define-key map "\C-x4k" 'ansys-delete-other-window)
    (define-key map "\C-c\C-a" 'ansys-start-ansys-help)
    (define-key map "\C-c\C-b" 'ansys-submit-bug-report)
    (define-key map "\C-c\C-c" 'ansys-send-to-ansys)
    (define-key map "\C-c\C-d" 'ansys-do)
    (define-key map "\C-c\C-e" 'ansys-display-error-file)
    (define-key map "\C-c\C-f" 'ansys-fit)
    (define-key map "\C-c\C-g" 'ansys-start-graphics)
    (define-key map "\C-c\C-i" 'ansys-if)
    (define-key map "\C-c\C-j" 'ansys-job)
    (define-key map "\C-c\C-k" 'ansys-kill-ansys)
    (define-key map "\C-c\C-l" 'ansys-license-status)
    (define-key map "\C-c\C-m" 'ansys-start-ansys) ;this is also C-c RET
    (define-key map "\C-c\C-p" 'ansys-start-pzr-box) ;pan-zoom-rotate
    (define-key map "\C-c\C-q" 'ansys-query-ansys-command)
    (define-key map "\C-c\C-r" 'ansys-replot)
    (define-key map "\C-c\C-s" 'ansys-display-skeleton)
;      (define-key map "\C-c\C-t" 'ansys-if-then)
    (define-key map "\C-c\C-t" 'ansys-license)
    (define-key map "\C-c\C-u" 'ansys-copy-or-send-above)
    (define-key map "\C-c\C-v" 'ansys-display-variables)
    (define-key map "\M-?" 'ansys-show-command-parameters)
    (define-key map "\C-c?" 'ansys-show-command-parameters)
;    (define-key map [f1] 'describe-mode) ; [f1] reserved for user
;    (define-key map "\C-c\C-s" 'ansys-process-status) ;redundant with new mode line
       map)
    "Keymap for the Ansys mode.")

(defvar ansys-previous-major-mode ""	;NEW_C
  "The buffer's previous major mode (before Ansys mode).")

  ;; --- functions ---

(defun ansys-mode-version ()
  "Display the Ansys mode version numbering scheme."
  (interactive)
  (message "Ansys mode version: %s.%s (based on Ansys %s)" ansys_version
	   ansys_mode_version
	   ansys_version))


(defun ansys-toggle-mode nil ;NEW_C FIXME this toggles also all ansys minor-hooks?
  "Restore the buffer's previous major mode, if possible."
  (interactive)
  (if (string= ansys-previous-major-mode "ansys-mode")
      (error "Previous major mode was \"Ansys mode\"") ;buffers opended with auto-mode
    (funcall ansys-previous-major-mode)))

(load "ansys-keyword")

(defconst ansys-font-lock-keywords	;NEW_C
  (append
   ansys-commands			;command overwrite variables
   ansys-undocumented-commands
   '(("^\\s-*\\([[:alpha:]][[:alnum:]_]\\{0,7\\}\\)\\s-*=" 1 'default t)) ;remove fontification from variables (max. 8 chars long)
   ;; this is for level 2
   '(("^\\s-*\\(\\*[mM][sS][gG]\\|\\*[vV][rR][eE]\\|\\*[vV][wW][rR]\\|\\*[mM][wW][rR]\\).*\n\\(\\(.*&\\s-*\n\\)*.*\\)" ;format constructs
      2 'font-lock-doc-face prepend))
   '(("^\\s-*/[cC][oO][mM].?\\(.\\{0,75\\}\\)" 1 'font-lock-doc-face keep))
   					;highlight message of comment command /COM (no comment (!)
   					;is possible behind /COM), no separating comma necessary
   '(("^\\s-*\\([cC]\\*\\*\\*\\).?\\(.\\{1,75\\}\\)" ;c*** should get immediate fontification
      (1 'font-lock-type-face keep) (2 'font-lock-doc-face keep)))
   					;only 75 characters possible no separator necessary
   '(("^\\s-*\\(/TIT\\|/TITL\\|/TITLE\\)\\s-*,\\(.*\\)$" 2
      'font-lock-doc-face keep))	;the same for /title
   '(("^\\s-*/[sS][yY][sS]\\s-*,\\(.\\{1,75\\}\\)$" 1 'font-lock-doc-face keep))
   					;/SYS command sends string to OP, no parameter substitution!
   					;for variables with command names
   '(("\\( \\*[^[:alpha:]].*\\)$" 1 'font-lock-comment-face append)) ;deprecated Ansys comment!
   					;^[:alpha:] to avoid spurious asterisk command fontification
   ansys-elements
   ansys-get-functions
   ansys-parametric-functions
   '(("\\(\\$\\)" 1 'font-lock-warning-face keep)) ;condensed line continuation char
   '(("^[^ \t_]*\\(\\<\\w\\{33,\\}\\>\\)\\s-*=" 1 'font-lock-warning-face t))
   					; more than 32 character long variables are not allowed
   ;; this is for level 3
;   '(ansys-highlight) ;function searches user variables ; TODO BUG
   '(("\\(&\\)\\s-*$" 1 'font-lock-comment-face prepend)) ;format continuation char
   '(("\\(%\\)" 1 'font-lock-comment-face prepend))
   					;single % acts as a format specifier and pair %.% is an
   					;ansys parameter substitution
   '(("\\(:\\)" 1 'font-lock-warning-face keep))   ;colon loops
   '(("^\\s-*\\(:\\w\\{1,7\\}\\)" 1 'font-lock-warning-face t)) ;GOTO Labels, branching
   '(("\\(_\\w+\\>\\)" 1 'font-lock-warning-face)) ;reserved words
   ) "Regexp for the highlighting."  )

(defconst ansys-mode-syntax-table     ;FIXME check Ansys operators and
					;allowed variable characters
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\r " " table)
    (modify-syntax-entry ?\$ "." table)
    (modify-syntax-entry ?+ "."  table)
    (modify-syntax-entry ?- "."  table)
    (modify-syntax-entry ?= "."  table)
    (modify-syntax-entry ?> "."  table)
    (modify-syntax-entry ?< "."  table)
    (modify-syntax-entry ?. "."  table)
    (modify-syntax-entry ?\% "." table)
    (modify-syntax-entry ?| "."  table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\` "_" table) ;ansys-mode abbreviation specifier,
					;not an operator
    (modify-syntax-entry ?_ "_"  table) ;not an operator in Ansys
    (modify-syntax-entry ?: "_"  table) ;Ansys label specifier, not an operator
    (modify-syntax-entry ?* "_"  table) ;Ansys asterisk commands syntax clashing
					;with algebraic operators but blink-matching-
					;needs this
    ;;     (modify-syntax-entry ?/ "w"  table)	;Ansys slash commands
    (modify-syntax-entry ?\! "<" table) ;Ansys comment character
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "w" table) ;`"' is *not* a string delimeter for Ansys
    (modify-syntax-entry ?'  "\"" table); (modify-syntax-entry ?'  "." table)
					;Normally Ansys string delimiter, but might clash
					;with usages of genitives etc.!
    (modify-syntax-entry ?~ "_" table) ;Ansys connection commands, not an operator
    table)
  "Syntax table in use in `ansys-mode' buffers.")

;;;###autoload
(defun ansys-mode ()
"This is the Ansys mode help.
It is a major mode for reading, writing and navigating in
APDL (Ansys Parametric Design Language) files as well as
providing managing and communication capabilities for various
Ansys solver and license manager processes.

The mode provides sophisticated features for experienced Ansys
users but still the documentation is targeted for users with
little Emacs experience.

Documentation contents:
=======================

* Usage
* Keybindings
* Customisation
* Bugs and Problems

Usage
=====

* Ansys keyword completion (commands, elements, get- and
  parametric-functions)

Type the beginning of an Ansys command, function or element name
and use the key binding \"\\[ansys-complete-symbol]\" for
`ansys-complete-symbol' (\"M-TAB\" means holding down the \"ALT\"
key while pressing the \"TAB\" key), when your window manager
intercepts this key combination type \"C-M-i\"
  (typing \"CTRL\", \"ALT\" and \"i\" key simultaneously).

There are nearly 1900 Ansys symbols available for completion.

Undocumented Ansys commands are not completed (see the variable
`ansys-undocumented-commands', for inspecting the content: please
type \"C-h v\" and then the variable name at the prompt).  When
the character combination before the cursor (or point in Emacs
parlance) is not unambiguous: A completion list is shown,
selecting the suitable word from the list with either the mouse
or the cursor on the symbol and typing \"RET\" is completing the
symbol.  Hitting space removes the listing frame (in Emacs called
'buffer').

* Ansys command syntax help

Typing \"\\[ansys-show-command-parameters]\" displays at the
header line a command syntax help similar to (but often more
complete) than the dynamic prompt of the classical Ansys GUI.

* Auto-indentation of looping and conditional blocks

You can customise the indentation depth (Ansys Block Offset),
please have a look for it at 'Customise Ansys Mode' in the Ansys
menu entry.  The Emacs customisation facility saves your choices
automatically in your .emacs file for later sessions.

* Closing of open blocks with insertion of the appropriate end
  keyword

Typing \"\\[ansys-close-block]\" completes the current Ansys
block with the appropriate end keyword on a separate line.

* Code navigation, extended keyboard shortcuts: Code lines,
  number blocks, and *DO,*IF, DOWHILE, *CREATE blocks etc.

\\[ansys-next-code-line] -- `ansys-next-code-line'
\\[ansys-previous-code-line] -- `ansys-previous-code-line'

Are searching for and/or going the next/previous code line,
skipping comments and empty lines.

The block navigation is analogous to the list and sexp
navigiation in lisp mode, when you interchange list and sexp with
block.

\\[ansys-next-block-end] -- `ansys-next-block-end'
\\[ansys-previous-block-start-and-conditional] -- `ansys-previous-block-start-and-conditional'

Are searching for and skipping to the next/previous block keyword
regardless where you are in the block structure.
\\[ansys-previous-block-start-and-conditional] finds also *IF
commands without bases of the keyword THEN; furthermore the
*CYCLE and *EXIT looping controls.  They provide APDL constructs
but represent no block depth and are therefore not considered in
the following navigation commands.

\\[ansys-skip-block-forward] -- `ansys-skip-block-forward'
\\[ansys-skip-block-backwards] -- `ansys-skip-block-backwards'

Are searching for and going over a complete block (at the same
block depth, skipping the deaper block structures, if any).

\\[ansys-up-block] -- `ansys-up-block'
\\[ansys-down-block] -- `ansys-down-block'

Go searching for and skipping up/down a block level in the
current block structure.

\\[ansys-number-block-start] -- `ansys-number-block-start'
\\[ansys-number-block-end] -- `ansys-number-block-end'

Are searching for and skipping over pure number blocks e. g.
which are to be found in WorkBench input (.inp) files.

Moreover there are also keyboard shortcuts with whom you are able
to input pairs of corresponding characters like \"C-c %\" for the
input of '%%', the Ansys substitution operators.  Please have a
look at the Emacs function `insert-pair' and the keybinding
section below.

* Sophisticated highlighting (optionally also user variables)

The fontification tries to follow the idiosyncratic Ansys
interpreter logic as closely as possible. (` *' is still a valid
Ansys comment string although deprecated! See the Ansys manual
for the *LET command.)

The fontification distinguishes between Ansys keywords (or
commands), parametric-functions and element names.  For the Ansys
commands the unique stem (at least 4 characters) is slightly
distinguished from the rest of the complete command name.  In
case of arbitrary characters after the unique word stem, they are
shown in an unobtrusive font, since these characters are ignored
by the intepreter anyways.

Macro expressions beginning with an underscore might be Ansys
reserved variables and therefore are higlighted as warning in
red.  Another example is the Ansys the fontification of the
percent sign.  It reminds you that the use of such a pair around
a parameter name forces the parameter substitution, e. g. if I=5
then TEST$I$ becomes TEST5.  You can input various pairs with
keyboard shortcuts, e. g. apostrophies for Ansys character
parameters with \"C-c '\", please have a look wich bindings are
available with `describe-bindings' (for the function
\\[describe-bindings]).

The format strings of *MSG, *MWRITE, *VWRITE and *VREAD are also
highlighted.  Below is a summary of the C-format descriptors (no
parentheses needed in contrast to less powerful fortran
descriptors):

%I	Integer data
%F	Floating point format
%G	General numeric format
%E	Scientific notation
%C,%S	Character strings (up to 128 characters) or arrays;
%/	Line break
%%	Single percent sign
%wI	w is the column width. Integer is preceded by the number of blank characters needed to fill the column.
%0wI	Same as above except integer is padded by zeroes instead of spaces.
%0w.pI	Pad integer with zeros as necessary to obtain a minimum of p digits.
%w.pF	w is the column width. Floating point format to p decimal places.
%w.pG	General format with p significant digits.
%w.pE	Scientific notation to p decimal places.
%wC,
%wS	Character string with column width of w.
%-wC,
%-wS	Left justify character string (right justify is default).
%wX	Include w blank characters.

example:
*vwrite,B(1,1),B(2,1)
%E%/%E

Regarding the highlighting of user variables: see the menu
entry: ->Ansys ->Customise Ansys Mode or include

     (setq ansys-dynamic-highlighting-flag t)

in your .emacs file.  In general you can also scroll further down
to the == Customisation == section of this help and click on the
respective variable (here in this case:
`ansys-dynamic-highlighting-flag').  You will be presented with
an help buffer for this variable in which you are can click on
the underlined word 'customize'.  Then you also have the
convenient Emacs customisation functionality at hand.

The user variable highlighting is currently only implemented
for files with a '.mac' extension.

* Displays a summary for all definitions (*GET, *DIM, *SET, = and
  *DO) of APDL variables.

Typing \"\\[ansys-display-variables]\" shows all variable
definitions from your APDL file in a separate buffer.

When you place the cursor on the respecitve line number and type
\"C-u M-g M-g\" Emacs will place the cursor at the corresponding
defintion in the macro file.

* Use of the Emacs abbreviation facility for block templates

E.g. typing \"`dd\" and SPC in an Ansys buffer triggers the
insertion of an Emacs skeleton for an *Do loop.  (\"`d\" is a
shorter abbreviation version without user input.)  You can see
all the predefined abbreviations with the Emacs command \"M-x
list-abbrevs RET\" (You should apply the auto-completion of Emacs
commands: Just type the beginning and then \"TAB\"), or
alternatively just type a \"?\" after the \"`\" in your file
buffer.

* Outlining (hiding) of code sections

You must enable this by ticking on the option
`ansys-outline-minor-mode' in the `ansys-mode-hook' variable.
Either type \"M-x ansys-customise-ansys\" or use the menu
entries: ->Ansys ->Customise Ansys Mode.

Then you can hide certain sections of your code or navigate to
customisable outline headings.  Certain characters--by default
'!@' (see the variable `ansys_outline_string')--at the beginning
of a line in your code represent such headings.  '!@@' specifies
a subheading and so on (type \"M-x ansys-skeleton\" to insert an
example of this in the current file buffer).  The easiest way of
working with outline headings is to go to the ->Outline menu
entry (outline-minor-mode is not switched on by default in
`ansys-mode') and see what possibilities are at your convenience.

* Convenient comment handling, for example the commenting/un- of
  whole paragraphs

** \"\\[comment-dwim]\" or the menu entry calls
   `comment-dwim' (Do What I Mean 8-):

In a code line: Insert comment char `ansys-indent-comment-string'
at `ansys-code-comment-column' (if possible).  With a prefix
argument: Kill existing code comment.

With an active region: Commenting out (`comment-region') or
Uncommenting (`uncomment-region') the region.

In an empty line: Insert '!!\ ' at the line beginning.

** \"\\[indent-new-comment-line]\" (or \"M-j\" --
  `indent-new-comment-line' or the alias
  `comment-indent-new-line')

Inserts a single '!' (`ansys-comment-char')in a code line at
column `ansys-code-comment-column' (if possible).

In comment lines '!!\ ' (`ansys-indent-comment-string') at the
the line beginning.

In empty lines: Just insert a new line.

* Auto-insertion of code templates into new APDL files

See `ansys-skeleton' (type 'M-x ansys-skeleton' to insert this
skeleton).

Include the following lisp expressions in your .emacs file, when
you want to be ask for inclusion for every new .inp and .mac
file.

      (add-to-list 'auto-mode-alist '(\"\\.inp$\" . ansys-mode))
      (add-to-list 'auto-mode-alist '(\"\\.mac$\" . ansys-mode))

      (auto-insert-mode 1)
      (setq auto-insert-query t)
      (add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton]))

* Ansys process management

** Write an Ansys stop file (extension '.abt') in the current
   directory (you can use \"M-x cd\" for changing the current
   directory), for halting the calculation in an re-start-able
   fashion.

** Opening the Ansys error file ('.err' in the current
   directory).  The error file is opened in read only mode (see
   `toggle-read-only') and with the mode function
   `auto-revert-tail-mode', which scrolls the buffer
   automatically for keeping the growing Ansys output visible.

** Starting of the Ansys help
   system (\"\\[ansys-start-ansys-help]\").

** Display of the current license status in another Emacs
   buffer (\"\\[ansys-license-status]\").

For the last two capabilities you need to customise some
variables either with the Emacs customisation facility (menu
entries: ->Ansys '->Customise Ansys Mode' and look there for the
variables 'Ansys License File', 'Ansys Util Program' and 'Ansys
Help Program') or setting the variables directly like the
following example in your .emacs file.

      (setq ansys-license-file \"27005@rbgs421x:1055@p46054\")
      (cond
       ((string= system-type \"windows-nt\")
         (setq ansys-lmutil-program \"C:\\\\Program Files\\\\Ansys Inc\\\\Shared Files\\\\Licensing\\\\intel\\\\anslic_admin.exe\"
         (setq ansys-help-file \"C:\\\\Program Files\\\\Ansys Inc\\\\v120\\\\CommonFiles\\\\HELP\\\\en-us\\\\ansyshelp.chm\"))
        (t
         (setq ansys-lmutil-program \"/ansys_inc/shared_files/licensing/linop64/lmutil\")
         (setq ansys-help-file \"/ansys_inc/v110/ansys/bin/anshelp120\")))

* Ansys solver control and communication (mainly restricted to
  UNIX systems)

You likely have to specify the following variable: 'Ansys
Program' in the customisation menu or set it like the following
example in your .emacs file:

      (setq  ansys-program \"/ansys_inc/v110/ansys/bin/ansys110\")

With the Ansys mode command `ansys-start-ansys' it is possible to
start the Ansys solver as an asynchronous process and to send
interactively data from the current APDL file directly to the
solver.  With \\[ansys-send-to-ansys] you can send either the
current (single) code line or at once a whole region to the
running solver.  A selected region here means highlighted lines
of code.  When you are not familiar with Emacs you probably want
to select your part of interest with dragging the mouse pointer
while pressing the first mouse button.  Although it is often
faster to select sections in your macro with keyboard commands.
For example \\[ansys-mark-block] marks a whole block level,
\\[mark-paragraph] marks the current paragraph.  Please check the
code navigation commands which Ansys mode provides (type
\"\\[describe-bindings]\" to see which are available) these can
also be to create or to extend an existing selection.

After starting the solver you first have to send an empty line
because the Ansys solver in interactive mode requests a carriage
return for the required license agreement (in case there is a
lock file from a formerly crashed session: Send a single \"y\" to
the solver process to remove the lock (type
\\[ansys-send-to-ansys] in a line with a sole \"y\" in it).
Warning: the same applies when clearing the database, /CLEAR
requires an \"y\" to work, otherwise the command will just be
skipped.  The Ansys solver output can be observed at the same
time in a separate Emacs buffer.)

The interaction is like in the Ansys' solver interactive mode in
a text console, i. e. non-graphically, you have to type the
commands directyl but you can invoke--with the following APDL
commands--the Ansys graphics screen (without the whole graphical
user interphase).

       /show,x11 !or 3d
       /menu,grph

or alternatively call the function `ansys-start-graphics'.  Thus
you are able to check and debug your macro file content visually.
The graphics is in this state only changeable with APDL
commands (/view,1,1,1,1) and not through some mouse pointer.  If
you feel the need to turn, zoom, etc. the model with the
mouse (very likely) you can send the line

       /ui,view !start the Ansys pan zoom rotate dialog

to the solver process or alternatively call `ansys-start-pzr-box'
with \\[ansys-start-pzr-box] and a dialog box pops up.  This is
the Pan/Zoom/Rotate dialog for the graphics screen.  But beware:
before you are able to send further commands from Emacs to the
solver, you have to close dialog box first.  When the dialog is
closed the solver receives again commands via
\"\\[ansys-send-to-ansys]\".

See also the function
`ansys-show-command-parameters' (\"\\[ansys-show-command-parameters]\")
for getting quick help to Ansys commands within Emacs.

Ansys mode has his own command for invoking the Ansys help
system (\\[ansys-start-ansys-help], see above) because the
following APDL commands do not work when the complete GUI system
of Ansys is not active.

       !/ui,help  !is it not working in Ansys non-GUI modes
       !help, nsel !is not working in Ansys non-GUI modes

Some helpful commands when working interactively:
replotting: \\[ansys-replot]
refitting: \\[ansys-fit]

You can optimise the execution speed of Ansys mode with
byte-compilation of ansys-mode.el.  Just call the function
`byte-compile-file'.

Keybindings
===========

\\{ansys-mode-map}
Variables you can use to customize Ansys mode
=============================================

`ansys-dynamic-highlighting-flag': Controls experimental
highlighting of user defined variables.  Warning: This option is
computational expensive and depending on the file size and your
system it might make your editing experience somewhat sluggish.
Currently dynamic highlighting of user variables is only
implemented for files with the extension \".mac\".

`ansys-program': The Ansys executable with complete path
specification.

`ansys-lmutil-program': License manager utility program with
complete path specification.  Necessary for the command
`ansys-license-status'.

`ansys-license-file': License file or license server
specification.  The license server specification must include the
port number when it isn't 1055, the default port number:
port_number@server_name.

`ansys-help-file': The Ansys help system file with complete path
specification

`ansys-license-types':
Available license types to choose from.
Below are often used license types (as seen with the function
`ansys-license-status') and their corresponding WorkBench
terminologies.

  \"ansys\": - Mechanical U
  \"struct\" - Structural U (with thermal capability)
  \"ane3\" - Mechanical/Emag
  \"ansysds\" - Mechanical/LS-Dyna
  \"ane3fl\" - Multiphysics
  \"preppost\" - PrepPost (just pre- and post-processing)

`ansys-license': License type with which the Ansys solver will be
started.

`ansys-indicate-empty-lines-flag': When t indicate empty lines on
window systems.  Do this visually at the end of buffer in the
left fringe.

`ansys-current-ansys-version': String describing the Ansys
version installed by the user.  Variable is used by the
`ansys-skeleton' skeleton.

`ansys-comment-padding': Padding string that `comment-region'
puts between comment chars and text.  Extra spacing between the
comment character(s) and the comment text makes the comment
easier to read.  This padding is only active for comments at the
beginning of a line.

`ansys-comment-add': How many more comment chars should be
inserted by `comment-region'.  This determines the default value
of the numeric argument of `comment-region'.  This should
generally stay 0, except for a few modes like Lisp where it can
be convenient to set it to 1 so that regions are commented with
two semi-colons.

`ansys-code-comment-column': Column where Ansys code
comments (behind code) are placed.

`ansys-auto-indent-flag': Non-nil means indent line after a space
in Ansys mode.

`ansys-indent-comment-suffix': String placed after the comment
char in an indented (code) comment.  See
`ansys-indent-comment-string'.

`ansys-ruler-wide-flag': Non-nil means show a 80 characters wide
temporary ruler.  Nil means show a narrower temporary ruler with
50 characters.

`ansys-require-spaces-flag': If non-nil, `insert-parentheses'
inserts whitespace before ().

`ansys-blink-matching-block-flag': Control the blinking of
matching Ansys block keywords.  Non-nil means show matching begin
of block when inserting a newline after an else or end keyword.

`ansys-blink-matching-delay': Time in seconds for showing a
matching block.

`ansys-block-offset': Extra indentation applied to statements in
Ansys block structures.

`ansys-outline-string': Character specifying outline headings.

Turning on Ansys mode runs the hook `ansys-mode-hook'.

Bugs and Problems
=================

If you experience problems installing or running this mode you have
the following options:

* It might, at the first stage, be helpful for you to visit the
  Emacs Wiki at http://www.emacswiki.org/cgi-bin/wiki/AnsysMode
  for further instructions.  At the Wiki you can also leave some
  comments or wishes.

* Write an email to the mode maintainer (you can trigger a bug
  report from the menu--at least a useful template when you are
  not able to send emails via Emacs--or call the function
  `ansys-submit-bug-report' with \"\\[ansys-submit-bug-report]\").

* When you have already a (cost free) Google account you are able
  to issue a bug report at Google Code's hosted page
  http://code.google.com/p/ansys-mode/, where you can also
  download the latest developer version of this file.

==================== End of Ansys mode help ===================="
  (interactive)

  (unless (string= major-mode "ansys-mode")
    (set (make-local-variable 'ansys-previous-major-mode) major-mode))
  (put 'ansys-previous-major-mode 'permanent-local t)

  (kill-all-local-variables)		; convention
  (setq major-mode 'ansys-mode)
  (setq mode-name "Ansys")		; mode line string

  ;; only effective for window systems!
  (setq indicate-empty-lines ansys-indicate-empty-lines-flag)

  (setq completion-ignore-case t) ; keyword completion regardless of cases

  (use-local-map ansys-mode-map)
  (set-syntax-table ansys-mode-syntax-table)
  (setq local-abbrev-table ansys-mode-abbrev-table)

  (setq font-lock-multiline t)		;for *msg format strings

  (make-local-variable 'ansys-run-flag) ;FIXME: implement what exactly?

  (make-local-variable 'ansys-user-variables-regexp) ;for font-lock
  (setq ansys-user-variables-regexp nil)	     ;TODO

  (make-local-variable 'parens-require-spaces)
  (setq parens-require-spaces ansys-require-spaces-flag)

  (make-local-variable 'ansys-job)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ansys-indent-line-function)

  (make-local-variable 'comment-start)
  (setq comment-start ansys-indent-comment-string)

  (make-local-variable 'comment-padding)
  (setq comment-padding ansys-comment-padding)

  (make-local-variable 'comment-add)
  (setq comment-add ansys-comment-add)

  (make-local-variable 'comment-column)
  (setq comment-column ansys-code-comment-column)

  (make-local-variable 'kill-buffer-query-functions)

  ;; the following might become obselete with Emacs 23.2 (see TODO)
;;  (add-to-list 'kill-buffer-query-functions 'ansys-kill-buffer-query-function)

  ;; FIXME:
  ;;  (setq comment-fill-column 50)???
  ;;  comment-indent -> fill-column?? only when line-wrap mode t?

  ;; overlay for command-parameter-help
  (make-local-variable 'ansys-help-overlay)
  (setq ansys-help-overlay (make-overlay 1 1))

  ;; look at newcomment.el
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ansys-comment-start-skip)

  ;;  (make-local-variable 'parse-sexp-ignore-comments)
  ;;  (setq parse-sexp-ignore-comments t)

  ;;  (make-local-variable 'font-lock-defaults) is always local
  (setq font-lock-defaults '(ansys-font-lock-keywords nil 'case-ignore))
  ;; keywords
  ;; keywords-only -- nil: syntactic fontification
  ;; case-fold -- non nil: ignore case

  (make-local-variable 'outline-regexp)
  (setq outline-regexp (concat "^!\\(" ansys-outline-string "\\)+"))

  (setq ansys-is-unix-system-flag (ansys-is-unix-system-p))

  ;;   (make-local-variable 'ansys-column-ruler-wide) ;FIXEME
  ;;   (make-local-variable 'ansys-column-ruler-narrow)
  ;;   (make-local-variable 'ansys-ruler-wide-flag)
  ;;   (setq wide-ansys-ruler-mode nil)
  ;;	"set to  nil for narrow, t for wide."

  (make-local-variable 'ansys-format)
  (setq ansys-format (intern "mac"))	;FIXME: this is for the ansys-macro
					;? why intern?
  ;; menu
  (ansys-add-ansys-menu)

  ;; --- user variables ---
  (if ansys-dynamic-highlighting-flag
      (when (or
	     (> 1000000 (nth 7 (file-attributes (buffer-file-name))))
	     (yes-or-no-p
	      "File is bigger than 1MB, switch on user variable highlighting?"))
	(if (string= (file-name-extension (buffer-file-name)) "mac")
	    (progn (add-hook 'after-change-functions
			     'ansys-find-user-variables nil t)
		   (message "Experimental (dynamic) fontification of user variables activated."))
	  (message "Experimental (non-dynamic) fontification of variables activated."))
	(ansys-find-user-variables)))

  ;; --- hooks ---
  (run-hooks 'ansys-mode-hook))

(defun ansys-is-unix-system-p ()
  (not
   (or (string= system-type "darwin")
       (string= system-type "macos")
       (string= system-type "ms-dos")
       (string= system-type "windows-nt")
       (string= system-type "cygwin")
       (string= system-type "vax-vms")
       (string= system-type "axp-vms"))))

(defun ansys-show-paren-mode ()		;_C
  "Switch on minor mode function `show-paren-mode'.
Matching parenthesis are highlighted."
  (show-paren-mode 1))

(defun ansys-ruler-mode ()		;_C
  "Switch on minor mode function `ruler-mode'.
Display a ruler in the header line."
  (ruler-mode 1))

(defun ansys-font-lock-mode ()		;NEW_C
  "Switch on function `font-lock-mode'.
Font Lock is also known as \"syntax highlighting\"."
  (font-lock-mode 1))

(defun ansys-outline-minor-mode ()	;NEW_C
  "Switch on mode function `outline-minor-mode'.
Editing with selective display."
  (outline-minor-mode 1))

(defun ansys-auto-insert-mode ()	;_C
  "Switch on mode function `auto-insert-mode'.
Automatic template insertion is enabled"
  (auto-insert-mode 1))

(defun ansys-insert-pi ()		;NEW_C
  "Insert a variable assignment of Pi at point.
Together with a newline character and indentation of the assigment."
  (interactive)
  (insert "Pi=3.14159265359")
  (indent-according-to-mode)
  (newline-and-indent))

(defun ansys-column-ruler ()		;IMPROVED_C
  "Insert a temporary column ruler above the current line.
The insertion remains until the next keystroke.  The key typed is
inserted or evaluated unless it is the SPC key."
  (interactive)
  (save-excursion
    (momentary-string-display
     (if ansys-ruler-wide-flag
	 ansys-column-ruler-wide
       ansys-column-ruler-narrow)
     (line-beginning-position))))

(defun ansys-position (position) ;_C FIXME: with `line-beginning-position' etc.
					;redundant function
  "Return the value of point at certain POSITIONs."
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line)) ;line-beginning-position
     ((eq position 'eol)  (end-of-line))       ;line-end-position
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     (t (error "Unknown buffer position requested: %s" position)))
    (point)))

(defun ansys-close-block ()		;_C FIXME: choices for *IF
  "Complete an Ansys block command with the appropriate end keyword.
Insert the end keyword on a separate line.  An error is signaled
if no block to close is found.  For example the *IF command
represents only a proper block command when it is followed by a
THEN action label."
  (interactive "*")
  (let (bb-keyword str tmp)
    (condition-case err			;more pertinent error message
	(progn
	  (save-excursion
	    (ansys-up-block)
	    (setq bb-keyword (buffer-substring-no-properties
			      (match-beginning 1) (match-end 1)))
	    ;; for *IF we're getting more than one word (*IF,...,THEN)
	    (setq tmp (compare-strings bb-keyword 0 nil "*IF" 0 nil 'ignore-case))
	    (when (> tmp  2)
	      (setq bb-keyword
		    (substring-no-properties
		     (buffer-substring-no-properties
		      (match-beginning 1) (match-end 1)) 0 3))))
	  ;; prepare insertion of keyword
	  (setq str (car (reverse	;FIXME: uncomplete, why?
			  (assoc-string bb-keyword
					ansys-block-match-alist 1))))
	  ;; capitalise properly
	  (let ((case-fold-search nil))
	    (when (string-match
		   "\\([a-z].\\)\\|\\(\\*\\|/\\|~\\)[a-z]" bb-keyword)
	      (setq str (downcase str))))
	  (cond
	   ((ansys-in-empty-line-p)
	    ;(delete-blank-lines) deletes in 23.1 an isolated empty line
	    (insert str)
	    (indent-according-to-mode))
	   ((ansys-in-indentation-p)
	    (beginning-of-line)
	    (open-line 1)
	    (insert str)
	    (indent-according-to-mode)
	    (forward-line 1)
	    (indent-according-to-mode)
	    (forward-line -1)
	    (end-of-line))
	   ((ansys-in-string-or-comment-p)
	    (end-of-line)
	    (newline)
	    (insert str)
	    (indent-according-to-mode))
	   ((and (ansys-code-line-p)
		 (not (ansys-at-end-of-text-p)))
	    (indent-according-to-mode)
	    (newline 2)
	    (forward-line -1)
	    (insert str)
	    (indent-according-to-mode)
	    (forward-line 1)
	    (indent-according-to-mode)
	    (forward-line -1)
	    (end-of-line))
	   (t
	    (indent-according-to-mode)
	    (newline)
	    (insert str)
	    (indent-according-to-mode)))
	  (ansys-blink-matching-block)
	  t)
      (error (message "Cannot find a proper block command to close")))))

;;; --- Command parameters and command completions ----
(defun ansys-manage-overlay ( str)
  "Display or remove the command help overlay STR."
  (interactive)
  (let ((ho (overlay-start ansys-help-overlay))
	(lb (line-beginning-position))
	s)
    (delete-overlay ansys-help-overlay)
    (unless (eq lb ho)
      (move-overlay ansys-help-overlay lb lb)
      (setq s (propertize (concat str "\n") 'font-lock-face 'tooltip))
      (overlay-put ansys-help-overlay 'before-string s))))

(defun ansys-show-command-parameters (&optional ask) ;NEW FIXME: "ask" is an undocumented feature
  "Displays the Ansys command parameters help.
First it shows the parameters of the keyword and then a short
explanation.  This is done for the previous Ansys command
beginning, except when point is at the command beginning at the
indentation.  See also the function `ansys-command-start' how the
previous command is found."
  (interactive "P" )
  (let ((case-fold-search t)		;in case customised to nil
	str)
    ;; search for a valid command name
    (save-excursion
      (cond
       (ask
	(setq str (read-from-minibuffer "Type function or command name for help: "))
	(string-match ".*" str)
	(setq str (match-string-no-properties 0 str)))
       (t
	(unless (ansys-in-indentation-p) ;FIXME: take care of commented out commands
	  (ansys-command-start))	;go back to beginning of command
	(re-search-forward "[^[:space:]]\\w*\\>" nil t)
	(setq str (match-string-no-properties 0)))))
    ;; display help string
    (catch 'foo
      (dolist (s ansys-dynamic-prompt)
	(when (string-match (concat "^" str) s)
	  (ansys-manage-overlay s)
	  (throw 'foo nil)))
      (error "\"%s\" not found in keyword list" str))))

(defun ansys-complete-symbol ()
  "Perform completion on Ansys symbol preceding point.
Compare that symbol against Ansys's reserved words, functions and
element names."
  ;; This code taken from lisp-complete-symbol
  (interactive "*")
  (let ((window (get-buffer-window "*Completions*")))
    (if (and (eq last-command this-command)
	     window (window-live-p window) (window-buffer window)
	     (buffer-name (window-buffer window)))
	;; If this command was repeated, and
	;; there's a fresh completion window with a live buffer,
	;; and this command is repeated, scroll that window.
	(with-current-buffer (window-buffer window)
	  (if (pos-visible-in-window-p (point-max) window)
	      (set-window-start window (point-min))
	    (save-selected-window
	      (select-window window)
	      (scroll-up))))
      ;; Do completion.
      (let* ((end (point))
	     (beg (save-excursion (backward-sexp 1) (point)))
	     (string (buffer-substring-no-properties beg end))
	     (completion (try-completion string ansys-completion-alist)))
	(cond ((eq completion t))	;perfect match do nothing
	      ((null completion)	;completion is nil predicate
	       (message "Can't find completion for \"%s\"" string)
	       (ding))
	      ((not (string= string completion))
	       (delete-region beg end)
	       ;; Completion w/o capitalisation (Suggestion: Holger Sparr)
	       (let* ((case-fold-search nil)
		      (sc (string-match
			   "\\([a-z].\\)\\|\\(\\*\\|/\\|~\\)[a-z]" string)))
		 (if (or (null sc) (> sc 0))
		     (insert completion)
		   (insert (downcase completion)))))
	      (t
	       (let ((list
		      (all-completions string ansys-completion-alist))
		     (conf (current-window-configuration)))
		 ;; Taken from comint.el
		 (message "Making completion list...")
		 (with-output-to-temp-buffer "*Completions*"
		   (display-completion-list list string))
		 (message "Hit space to flush the completion buffer")
		 (let (key first)
		   (if (save-excursion
			 (set-buffer (get-buffer "*Completions*"))
			 (setq key (read-key-sequence nil)
			       first (aref key 0))
			 (and (consp first) (consp (event-start first))
			      (eq (window-buffer (posn-window (event-start
							       first)))
				  (get-buffer "*Completions*"))
			      (eq (key-binding key) 'mouse-choose-completion)))
		       (progn
			 (mouse-choose-completion first)
			 (set-window-configuration conf))
		     (if (eq first ?\ )
			 (set-window-configuration conf)
		       (setq unread-command-events
			     (listify-key-sequence key))))))))))))

;;;; Electric characters & friends

(defun ansys-reindent-then-newline-and-indent () ; (&ptional non-matching) ;FIXME: docu
  "Reindent current Ansys line, insert newline, and indent the new line.
If function `abbrev-mode' is on, expand the abbreviations first."
  (interactive "*") 			;* means signal error if read-only
  (expand-abbrev)
  (ansys-blink-matching-block)
  (save-excursion
    (delete-region
     (point)
     (progn (skip-chars-backward " \t")
	    (point)))			;clean trailing whitespace
    (indent-according-to-mode))
  (insert "\n")
  (indent-according-to-mode))

(defun ansys-electric-space ()
  "Insert a space in Ansys mode.
Maybe expand abbrevs and blink matching block open keywords.
Reindent the line if `ansys-auto-indent-flag' is non-nil."
  (interactive "*")
  (setq last-command-char ? )
  (if (and (ansys-not-in-string-or-comment-p)
	   (not (ansys-in-indentation-p))
	   (not (ansys-in-empty-line-p)))
      (progn
	(indent-according-to-mode)
	(self-insert-command 1)
	(expand-abbrev)
	(ansys-blink-matching-block)
	(if (and ansys-auto-indent-flag
		 (save-excursion
		   (skip-syntax-backward " ")
		   (not (bolp))))
	    (indent-according-to-mode)))
    (self-insert-command 1)))

(defconst ansys-mode-menu
  (list "Ansys"
	["Comment/Un~ Region"           comment-dwim :help "Comment out region or uncomment region, without a marked region start a code comment"]
	["Insert Pi"                    ansys-insert-pi :help "Insert variable definition \"Pi = 3.1415...\""]
	["Insert Parentheses"           insert-parentheses :help "Insert a pair of parentheses"]
	["Complete Expression"          ansys-complete-symbol :help "Complete an Ansys command"]
	["Close Block"                  ansys-close-block :help "Close an open control block with the corresponding end command"]
	["Preview Macro Template"        ansys-display-skeleton :help "Preview macro templates in another window"]
	"-"
	(list "Insert Template"
	      ["*IF ,Action *ENDIF"     ansys-if]
	      ["*IF ,THEN *ENDIF"	ansys-if-then]
	      ["*DO *ENDDO"	        ansys-do]
	      [" MP "	                ansys-mp]
	      ["Header"                 ansys-skeleton-header :help "File header template"]
	      ["Configuration"          ansys-skeleton-configuration help: "Configuration code template"]
	      ["View Settings"          ansys-skeleton-view-settings :help "View settings like focus point, magnification, ..."]
	      ["Coordinate Sys. Display"ansys-skeleton-coordinates :help "Template for creating and handling coordinate systems"]
	      ["Working Plane Operations"ansys-skeleton-working-plane :help "Template for creating and handling the working plane"]
	      ["Multiplot Commands"     ansys-skeleton-multi-plot :help "Graphic commands which show multiple model entities simultaneously"]
	      ["Numbering Controls"     ansys-skeleton-numbering-controls :help "Commands for numbering and colouring model entities"]
	      ["Geometry Import"        ansys-skeleton-import :help "Command for importing IGES models"]
	      ["Symmetry Expansions"    ansys-skeleton-expand :help "Commands for expanding the view of symmetric models to their full view"]
	      ["Element Definitions"    ansys-skeleton-element-def :help "2D, 3D, Element defintions and their keyoptions"]
	      ["Material Definitions"   ansys-skeleton-material-def :help "Various material definitions: Steel, alu, rubber, ..."]
	      ["Meshing Controls"       ansys-skeleton-meshing :help "Meshing control commands: Shapes, sizes, ..."]
	      ["Contact Pair Definition"           ansys-skeleton-contact-definition :help "Full definition of flexible-flexible contact pairs"]
	      ["Rigid Target"           ansys-skeleton-rigid-target :help "Full definition of rigid-flexible contact pairs"]
	      ["Boundary Conditions"    ansys-skeleton-bc :help "Commands for establishing boundary conditions"]
	      ["Buckling Analysis Type" ansys-skeleton-buckling :help "Commands for establishing a buckling analysis"]
	      ["Solve"                  ansys-skeleton-solve :help "Solver commands and options"]
	      ["Post1 Postprocessing"   ansys-skeleton-post1 :help "General postprocessor commands"]
	      ["Path plot operations"   ansys-skeleton-path-plot :help "Commands for establishing paths and plotting entities on paths"]
	      ["Output to file"         ansys-skeleton-output-to-file :help "Command for writing data to a file"]
	      ["Element Table Operations"ansys-skeleton-element-table :help "Commands for establishing and manipulation element tables"]
	      ["Post26 Postprocessing"  ansys-skeleton-post26 :help "Time history postprocessing commands"]
	      ["Template compilation" ansys-skeleton :help "Insert compilation of the most often used templates"])
	(list "Navigate Code Lines"
	      ["Previous Code Line"	ansys-previous-code-line :help "Goto previous apdl code line"]
	      ["Next Code Line"		ansys-next-code-line :help "Goto next code line"]
	      ["Beginning of (Continuation) Command" ansys-command-start :help "Go to the beginning of the current command"]
	      ["End of (Continuation) Command"	ansys-command-end :help "Go to the end of the current command"]
	      "-"
	      ["Split Format Line at Point"	ansys-indent-format-line :help "Split current line, if in a comment continue the comment, if in an Ansys format line insert the continuation character before splitting the line"]
	      )
	(list "Work with Logical Blocks"
	      ["Next Block End"		ansys-next-block-end :help "Go to the end of the current or next control block (*do, *if, ...)"]
	      ["Previous Block Start"   ansys-previous-block-start-and-conditional :help "Go to the beginning of the current or next control block (*do, *if, ...)"]
	      ["Down Block"		ansys-down-block :help "Move down one control block level"]
	      ["Up Block"		ansys-up-block :help "Move up one control block level"]
	      ["Skip Block Forward"     ansys-skip-block-forward :help "Skip to the end of the next control block"]
	      ["Skip Block Backwards"   ansys-skip-block-backwards :help "Skip to the beginning of previous control block"]
	      ["Beginning of N. Block" ansys-number-block-start :help "Go to the beginning of an Ansys number block (EBLOCK, NBLOCK)"]
	      ["End of Number Block"    ansys-number-block-end :help "Go to the end of an Ansys number block (EBLOCK, NBLOCK)"]
	      "-"
	      ["Close Block"                  ansys-close-block :help "Close the current Ansys control block with the respective closing command"]
	      ["Mark Block"              ansys-mark-block :help "Mark the current control block"]
	      )
	(list "Manage Ansys Processes"
	      ["Specify License Server or - File"   ansys-license-file :help "Insert or change the license server specification, either the license server or the actual license file"]
	      ["Specify License Utility" ansys-lmutil-program :help "Specify the Ansys license utility executable"]
	      ["Display License Status" ansys-license-status :help "Display a shortened license status from the license server"]
	      ["Start Ansys Help System" ansys-start-ansys-help :help "Start the Ansys help browser"]
	      "-"
	      ["Specify Ansys License Type" :help "Specify the license type for a solver run" ansys-license :active ansys-is-unix-system-flag]
	      ["Specify Job Name of Run" ansys-job :help "Specify the job name for a solver run" :active ansys-is-unix-system-flag]
	      ["Specify Ansys Executable " ansys-program :help "Specify the ansys executable for a solver run (with complete path if not in $PATH)" :active ansys-is-unix-system-flag]
	      ["Start Ansys Run" ansys-start-ansys :help "Start a solver run" :active ansys-is-unix-system-flag]
	      ["Display Ansys Run Status" ansys-process-status :help "Display the status of a possible solver run (nil if not active)" :active ansys-is-unix-system-flag]
	      "-"
	      ["Send Ansys Command Interactively" ansys-query-ansys-command :help "Send interactively an APDL command to a running solver process" :active ansys-is-unix-system-flag]
	      ["Send Code Line/Region to Ansys" ansys-send-to-ansys :help "Send the current line or active region to a running solver process" :active ansys-is-unix-system-flag]
	      ["Copy/Send above Code (to Ansys)" ansys-copy-or-send-above :help "Either copy the code up to the beginning of file or, when a run is active to the solver"]
	      ["Start Graphics Screen" ansys-start-graphics :help "Open the graphics screen of the Ansys GUI" :active ansys-is-unix-system-flag]
	      ["Start Pan/Zoom/Rot. Dialog" ansys-start-pzr-box :help "Open the Pan/Zoom/Rotate dialog of the Ansys GUI" :active ansys-is-unix-system-flag]
	      "-"
	      ["Display Emacs Processes" list-processes :help "Show all currently running Emacs Processes, like the Ansys help browser, etc."]
	      ["Display Ansys Run Status" ansys-process-status :active ansys-is-unix-system-flag]
	      ["Display Ansys Error File" ansys-display-error-file :help "Display in another window the current Ansys error file"]
	      ["Write Ansys Stop File" ansys-abort-file :help "Write a stop file for the solver (containing the word \"nonlinear\")"]
	      ["Exit Ansys Run" ansys-exit-ansys :help "Exit the active solver run" :active ansys-is-unix-system-flag]
	      "-"
	      ["Kill Ansys Run" ansys-kill-ansys :help "Kill the current run":active ansys-is-unix-system-flag]
	      )
	"-"
	["Start Ansys help system" ansys-start-ansys-help :help "Start the Ansys help browser"]
	["Display License Status" ansys-license-status :help "Show the license usage in another window or start the windows license manager"]
	["Display Ansys Command Help"      ansys-show-command-parameters :helpl "Display a short help for the Ansys command in the current line and its parameters"]
	["Display Variable Definitions" ansys-display-variables :help "Display all user variable definitions from the current file in another window"]
	["Insert Temporary Ruler"         ansys-column-ruler :help "Show a temporary ruler above the current line"]
	["Toggle Outline Mode"         outline-minor-mode :help "Switch on/off the outline mode for structuring the file with headlines"]
	["Show Ansys Mode version"  ansys-mode-version :help "Display the Ansys mode version in the mini buffer"]
	["Describe Ansys Mode"		describe-mode :help "Open a window with a description of Ansys mode"]
	["Customise Ansys Mode"         (customize-group "Ansys") :help "Open an Emacs customisation window for Ansys mode"]
	["Submit Bug Report"            ansys-submit-bug-report :help "Open a mail template for an Ansys mode bug report"]
	"-"
	["Return to previous mode"             ansys-toggle-mode :help "Switch to the previous major mode of the file"])
  "Menu items for the Ansys mode.")

(defun ansys-add-ansys-menu ()
  "Add an \"Ansys\" entry to the Emacs menu bar."
  (require 'easymenu)
  (easy-menu-define ansys-mode-menu-map ansys-mode-map
    "Menu keymap for Ansys mode." ansys-mode-menu)
  (easy-menu-add ansys-mode-menu-map ansys-mode-map))

(defun ansys-calculate-indent ()   ;FIXME: comment, fixed goal column,
  "Return appropriate indentation for current line as Ansys code.
Returns an integer (the column to indent to) unless the line is a
comment line with fixed goal column.  In that case, returns a list whose
car is the column to indent to, and whose cdr is the current indentation
level."
  (let ((column 0)			;column
	(keyword_c 0)			;for specified commands
	(comma_c nil)			;for default commands
	lep				;line end predicate
	lbp)				;line beginning pr.
    ;; --- first for the previous code line ---
    (save-excursion
      (when (zerop (ansys-previous-code-line)) ;otherwise at the first line
	(if (or (ansys-condensed-input-line-p)
		(and (ansys-in-indentation-p)
		     (not (ansys-default-command-p))))
	    (back-to-indentation)
	  (ansys-command-start)) ;skip to the beginning of an *msg and default command
	(setq keyword_c (current-column))
	(cond
	 ((looking-at ansys-block-begin-regexp)
					;	  (when (looking-at "\\*if.*,\\s-*then") ;*if base1 or base2
					;must be THEN for being a block keyword
	  (setq keyword_c (+ keyword_c ansys-block-offset)))
					;	 )
	 ((looking-at ansys-block-else-regexp)
	  (setq keyword_c (+ keyword_c ansys-block-offset)))
	 ((looking-at "[^*/\n,]") ;otherwise forbidden by default command stuff
	  (setq lep (line-end-position))
	  (setq comma_c (re-search-forward "\\w+\\s-*" lep 'noerror))
	  (when comma_c
	    (setq lbp (line-beginning-position))
	    (setq comma_c (- comma_c lbp))))
	 ((looking-at ",")
	  (setq lep (line-end-position))
	  (setq comma_c (1- (re-search-forward "," lep 'noerror))) ;excluding the comma
	  (when comma_c
	    (setq lbp (line-beginning-position))
	    (setq comma_c (- comma_c lbp)))))))
    ;; --- now for the current code line ---
    (save-excursion
      (back-to-indentation)
      (if (ansys-first-line-p)		;we are at the first code line
	  (setq column (current-column))
	(cond
	 ((and (looking-at ansys-block-else-regexp)
	       (ansys-not-in-string-or-comment-p))
	  (setq column (- keyword_c ansys-block-offset)))
	 ((and (looking-at ansys-block-end-regexp)
	       (ansys-not-in-string-or-comment-p))
	  (setq column (- keyword_c ansys-block-offset)))
	 ((and (looking-at ",")	   ;Ansys default command substitution
	       (ansys-not-in-string-or-comment-p)) ;FIXME:for *msg lines etc.?
	  (if comma_c
	      (setq column comma_c)
	    (setq column keyword_c)))
	 ((and (looking-at "\\s<\\w") ;FIXME:? this is for "code comments"
	       (not (looking-at
		     ( concat
		       "\\(\\s<\\s<\\s-\\S<\\)\\|\\(\\^\\s<"
		       ansys-outline-string "+\\)")))
	       (setq column comment-column)))
	 (t
	  (setq column keyword_c)))))
    (when (< column 0)
      (error "%s" "Can't deduce sensible column offset"))
    column))

(defun ansys-indent-line-function (&optional arg)
  "Indent current line in Ansys coding style.
With optional ARG, use this as offset unless this line is a
comment with fixed goal column."
  (interactive "*p")
  (unless arg (setq arg 1))
  (let ((icol (ansys-calculate-indent))
	(relpos (- (current-column) (current-indentation))))
    (if (listp icol)	     ;FIXME: -calculate-indent returns no list
	(setq icol (car icol))
      (setq icol (+ icol (1- arg))))
    (if (< icol 0)
	(error "Unmatched end keyword") ;FIXME: this is probably wrong
      (indent-line-to icol)
      (if (> relpos 0)
	  (move-to-column (truncate (+ icol relpos)))))))

;;;; Electric characters & friends

(defun ansys-abbrev-start ()
  "Start entering an Ansys abbreviation.
If Abbrev mode is turned on, typing ` (grave accent) followed by ? or
\\[help-command] lists all Ansys abbrevs.  Any other key combination is
executed normally.
Note that all Ansys mode abbrevs start with a grave accent."
  (interactive)
					;  (if (not abbrev-mode)			;FIXME: redundant with E22.?
					;      (self-insert-command 1)
  (let (c)
    (insert last-command-char)
    (if (or (eq (setq c (read-event)) ??)
	    (eq c help-char))
	(let ((abbrev-table-name-list '(ansys-mode-abbrev-table)))
	  (list-abbrevs))
      (setq unread-command-events (list c))))) ;)

(defun ansys-indent-format-line ()
  "Break Ansys line at point, continuing comment if within one.
If within code, insert the Ansys continuation character `&'
before breaking the line.  If within a string, signal an error.
The new line is properly indented."
  (interactive "*")
  (delete-horizontal-space)
  (cond
   ((ansys-in-comment-p)
    (indent-new-comment-line))
   ((ansys-in-string-p)	      ;FIXME: there are no strings defined yet
    (error "Cannot split a code line inside a string"))
   ((ansys-in-format-construct-p)
    (insert " &")
    (ansys-reindent-then-newline-and-indent))
   (t
    (ansys-reindent-then-newline-and-indent))))

;;; Motion

(defun ansys-default-command-end ()	;NEW
  "Move cursor to the end of an Ansys default command construct."
  (unless (ansys-default-command-p)
    (re-search-forward "^\\s-*,"))
  (while (ansys-default-command-p)
    (forward-line))
  (forward-line -1)
  (end-of-line))

(defun ansys-search-number-line(&optional dir) ;NEW
  "Search forward for a line existing purely of numbers.
If not If direction DIR is negativ search backwards.  The default
for DIR is 1. When already in a number line just go to the line
beginning when DIR is < 1 and when DIR is > to the end.  When no
futher number line is in the file signal an error."
  (interactive "p")
  (unless dir (setq dir 1))
  (let ((re ansys-number-line-regexp))
    (when (not (ansys-at-end-of-text-p))
      (beginning-of-line))
    (cond
     ((< dir 1)
      (while (progn
	       (unless (re-search-backward re nil t)
		 (error "Can't find preceding number line"))
	       (ansys-in-format-construct-p))))
     (t
      (while (progn
	       (unless (re-search-forward re nil t)
		 (error "Cant't find subsequent number line"))
	       (ansys-in-format-construct-p)))))))

(defun ansys-number-block-start()	;NEW
  "Move to the line beginning before a pure number block.
For example an Ansys NBLOCK or EBLOCK typically found in
WorkBench APDL files.  If there is no code before a number block,
signal an error."
  (interactive)
  (when (or (ansys-in-indentation-p)
	    (not (ansys-number-line-p)))
    (ansys-search-number-line -1))
  (while (ansys-number-line-p)
    (forward-line -1))
  (forward-line))

(defun ansys-number-block-end()		;NEW
  "Move to the end of a pure number block.
For example an Ansys NBLOCK or EBLOCK typically found in
WorkBench APDL files."
  (interactive)
  (when (or (ansys-at-end-of-text-p)
	    (not (ansys-number-line-p)))
    (ansys-search-number-line))
  (while (ansys-number-line-p)
    (forward-line))
  (forward-line -1)
  (end-of-line))

(defun ansys-next-code-line (&optional arg)
  "Move ARG lines of Ansys code forward, default for ARG is 1.
Skips past intermediate comment and empty lines."
  (interactive "p")
  (unless arg (setq arg 1))
  (unless (memq last-command '(next-line
			       previous-line
			       ansys-next-code-line
			       ansys-previous-code-line))
    (setq temporary-goal-column (current-column)))
  (when (eobp)
    (error "End of buffer"))
  (forward-line 1)
  (forward-comment (buffer-size))
  (move-to-column (truncate temporary-goal-column))
  (setq arg (1- arg))
  (when (and (not (ansys-last-line-p))
	     (/= arg 0))
    (ansys-next-code-line arg)))

(defun ansys-previous-code-line (&optional num)
  "Move NUM lines of Ansys code backward, default for NUM is 1.
Skips before all empty - and comment lines and return the
difference between NUM and actually moved code lines."
  (interactive "p")
  (unless num (setq num 1))
  (unless (memq last-command '(next-line
			       previous-line
			       ansys-next-code-line
			       ansys-previous-code-line))
    (setq temporary-goal-column (current-column)))
  (let ((p 0))
    (unless (ansys-first-line-p)	;in case we aren't at b-o-l
      (beginning-of-line)		;for forward-comment
      (forward-comment (-(buffer-size))) ;and in case we are in a comment line
      (move-to-column (truncate temporary-goal-column)) ;with Emacs 23.1
					;t-g-c might be a float!
      (setq num (1- num)
	    p num)
      (when (/= num 0)
	(setq p (ansys-previous-code-line num))))
    p))

(defun ansys-back-to-format-command ()	;NEW_C
  "Move cursor back to the beginning of a previous format command.
Signal an error when there is no format command."
  (interactive)
  (when (re-search-backward ansys-format-commands-regexp) ;signals error
    (back-to-indentation)))

(defun ansys-move-to-end-of-format-string () ;NEW_C
  "Move cursor to the end of an format command's format string."
  (when (ansys-in-format-command-line-p)
    (forward-line))
  (while (and (ansys-continuation-line-p)
	      (= (forward-line 1) 0))) ;in case of wrong format at eof
  (move-end-of-line 1))

(defun ansys-move-before-comment()	;NEW_C
  "Move cursor to the line's end of text (which is not commented out)."
  (beginning-of-line)
  (search-forward "!" (ansys-position 'eol) 1)
  (skip-chars-backward " \t!"))

(defun ansys-command-start (&optional num) ;_C
  "Move cursor to the beginning of the NUMth previous command or assignment.
Default for NUM is 1.  If in a comment or empty line, go to the
previous command or to the first line if no previous command is
there.  When on a condensed input line, go to previous `$'
statement or to the line's first command.  When in a format
command string move backward to the beginning of the respective
command.  When no Ansys command is to be found signal an error.
When NUM is 0 move to the current code line indentation."
  (interactive "p")
  (unless num (setq num 1))
  (while (> num 0)
    (cond
     ((ansys-in-format-construct-p)
      (ansys-back-to-format-command)
      (setq num (1- num)))
     ((ansys-number-line-p)
      (while (ansys-number-line-p)
	(forward-line -1))
      (end-of-line))
     ((ansys-default-command-p)
      (while (ansys-default-command-p)
	(forward-line -1))
      (end-of-line))
     ((and (not (ansys-code-line-p))	;in empty line or comment
	   (not (= num 0))
	   (not (ansys-first-line-p)))
      (beginning-of-line)
      (forward-comment (-(buffer-size))))
     ((ansys-in-indentation-p)
      (if (ansys-first-line-p)
	  (setq num -1)
	(forward-comment (-(buffer-size))))) ;skips also \n!
     ((ansys-condensed-input-line-p)
      (when (looking-back "\\$\\s-*")  ;we are already before a $ sign
	(skip-chars-backward " \t$"))	;skip at or before the $ char
      (if (re-search-backward "\\$\\s-*" (ansys-position 'bol) t)
	  (skip-chars-forward "$ \t")
	(back-to-indentation))
      (setq num (1- num)))
     (t
      (back-to-indentation)
      (setq num (1- num))))))

(defun ansys-command-end (&optional num) ;_C
  "Move to the end of the NUMth next Ansys command or assignment statement.
Default for NUM is 1.  If in a comment or empty line, go to the
next command or to the last line if no following command is
there.  When on a condensed input line, go to the end of the next
`$' statement or to the line's end.  Otherwise, when in a format
command string move forward to the end of the (possibly)
multi-line format string.  In a code comment skip back to the end
of the uncommented code, except when at the end of the code text,
then skip to the next code line's end."
  (interactive "p")
  (unless num (setq num 1))
  (while (> num 0)
    (cond
     ((ansys-at-end-of-code-p)
      (if (ansys-last-line-p)
	  (setq num -1)
	(forward-comment (buffer-size))))
     ((or (ansys-in-format-command-line-p)
	  (ansys-in-format-construct-p)) ;not the format command line
      (ansys-move-to-end-of-format-string)
      (setq num (1- num)))
     ((ansys-number-line-p)
      (ansys-number-block-end)
      (setq num (1- num)))
     ((ansys-default-command-p)
      (ansys-default-command-end)
      (setq num (1- num)))
     ((and (not (ansys-code-line-p))	;in empty line or comment
	   (not (= num 0))
	   (not (ansys-last-line-p)))
      (end-of-line)
      (forward-comment (-(buffer-size))))
     ((ansys-condensed-input-line-p)
      (when (looking-at "\\s-*\\$")    ;we are already before a $ sign
	(skip-chars-forward " \t$"))	;skip at or before the $ char
      (if (re-search-forward "\\s-*\\$" (ansys-position 'eol) t)
	  (skip-chars-backward " \t$")
	(end-of-line))
      (setq num (1- num)))
     (t
      (back-to-indentation)
      (while (not (ansys-at-end-of-code-p))
	(forward-char))
      (setq num (1- num))))))

(defun ansys-scan-blocks (count level-offset)
  "Scan from (point) COUNT balanced Ansys begin-end blocks.
Return the position thus found.  COUNT may be negative.

If LEVEL-OFFSET is nonzero, the block level gets an offset of
LEVEL-OFFSET."
  (let ((min-level-offset (if (> level-offset 0) 0 level-offset)) ;level-offset
					;can become large (we are going deeper
					;down block levels) but never
					;smaller than min-level-offset
	(inc (if (> count 0) 1 -1))
	(pt (point)))
    (save-excursion
      (while (/= count 0)
	(catch 'foo			;end the inner while loop
	  (while (or (re-search-forward ansys-block-begin-or-end-regexp nil t inc) ;FIXME:it's not working when
					;in a block regexp
		     (when (/= level-offset 0) (error "Can't reach specified block level")))
	    (unless (ansys-in-string-or-comment-p)
	      (cond
	       ((match-end 1) (setq level-offset (+ level-offset inc))) ;begin-block-keywords
	       ((match-end 2) (setq level-offset (- level-offset inc)))) ;end-block-keywords
	      (when (< level-offset min-level-offset)
		(if (< min-level-offset 0)
		    (error "Reached minimum block level: Can't go deeper")
		  (error "Reached maximum block level: Can't go further up")))
	      (when (= level-offset 0) (throw 'foo nil)))))
	(setq count (- count inc)))
      (if (= pt (point))
	  (error "No block keyword found")
	(point)))))

(defun ansys-mark-block ()     ;NEW FIXME: this is not consistent with
					;mark-paragraph, cursor below construct
  "Mark current block level.
Either inside of block structure or in the line of a block beginning
keyword."
  (interactive)
					;when we are in a line before a block beginning keyword
  (if (save-excursion
	(back-to-indentation)
	(looking-at ansys-block-begin-regexp))
      (progn
	(move-beginning-of-line nil)
	(set-mark-command nil))
    (progn
      (ansys-up-block)
      (move-beginning-of-line nil)
      (set-mark-command nil)))
  (ansys-skip-block-forward)
  (forward-line))

(defun ansys-skip-block-forward (&optional arg)
					;&optional: default arg always
					;nil when non interactively
					;called
  "Move forward across one balanced begin- and end-block keyword pair.
With argument, do it that many times.  Negative ARG means move
backward across |ARG| blocks."
  (interactive "p")	      ;"p" defaults to 1 only when interactive
  (unless arg (setq arg 1))
  (goto-char (or (ansys-scan-blocks arg 0)
		 (if (> arg 0)
		     (message "No %d block end(s) after cursor position" arg)
		   (message "No %d block start(s) before cursor position" arg)))))

(defun ansys-skip-block-backwards (&optional arg)
  "Move backward across one balanced Ansys begin-end block.
With argument, do it that many times.
Negative ARG means move forward across |ARG| blocks."
  (interactive "p")
  (unless arg (setq arg 1))
  (ansys-skip-block-forward (- arg)))


(defun ansys-next-block-end (&optional count)
  "Move forwards behind the next block end.
With arguement COUNT do that COUNT times.  With negative argument
move backards to the beginning of the |COUNT| previous block
end."
  (interactive "p")
  (unless count (setq count 1))
  (let ((c)
	(dir (if (< count 0 ) -1 1))
	(n (abs count)))
    (save-excursion
      (dotimes (i n)
	(while (progn
		 (setq c (re-search-forward ansys-block-end-regexp nil t dir))
		 (unless c
		   (if (< dir 0)
		       (error "No previous block end(s), %d is(are) missing"
			      (- n i))
		     (error "No further block end(s), %d is(are) missing"
			    (- n i))))
		 (ansys-in-string-or-comment-p)))))
    (goto-char c)))

(defun ansys-previous-block-start-and-conditional (&optional count)
  "Move backwards to the beginning the previous block start and conditionals.
This includes the conditional command *IF with bases other then
the keyword THEN; furthermore the looping controls *CYCLE and
*EXIT.  With argument COUNT do that COUNT times.  With negative
argument move forward to the end of the |COUNT| next block start
or conditional or looping construct."
  (interactive "p")
  (unless count (setq count 1))
  (let ((c)
	(dir (if (< count 0 ) -1 1))
	(n (abs count))
	(b-regexp
	 (concat
	  ansys-block-begin-regexp
	  "\\|\\*[iI][fF]\\>\\|\\*[cC][yY][cC]\\|\\*[eE][xX][iI]")))
    (save-excursion
      (dotimes (i n)
	(while (progn
		 (setq c (re-search-backward b-regexp nil t dir))
		 (unless c
		   (if (< dir 0)
		       (error "No further block start(s), %d is(are) missing"
			      (- n i))
		     (error "No previous block start(s), %d is(are) missing"
			    (- n i))))
		 (ansys-in-string-or-comment-p)))))
    (goto-char c)))

(defun ansys-down-block (&optional down-level)
  "Move forward down one begin-end block level of Ansys code.
Position cursor behind the beginning keyword of the respective
block.  With argument DOWN-LEVEL, do this for that many levels.
A negative argument means move backwards up DOWN-LEVEL
levels (see `ansys-up-block')."
  (interactive "p")
  (unless down-level (setq down-level 1))
  (let ((inc (if (> down-level 0) 1 -1)))
    ;; we have to take care whether cursor sits on a beginning keyword
    (while (/= down-level 0)
      (goto-char (ansys-scan-blocks inc (- inc)))
      (setq down-level (- down-level inc)))))

(defun ansys-up-block (&optional depth)
  "Move backwards up one begin-end block level of Ansys code.
Position cursor before the beginning keyword of the respective
block.  With argument DEPTH, do this for that many levels.  A
negative argument DEPTH means move forward down DEPTH levels (see
`ansys-down-block')."
  (interactive "p")
  (unless depth (setq depth 1))
  (ansys-down-block (- depth)))

(defun ansys-blink-matching-block ()
  "Blink the matching Ansys begin block keyword.
If point is right after an Ansys else or end type block keyword,
move cursor momentarily to the corresponding begin keyword.
Signal an error if the keywords are incompatible."
  (interactive)
  (when ansys-blink-matching-block-flag
    (let (bb-keyword bb-arg eb-keyword pos eol)
      (when
	  (and
	   (ansys-not-in-string-or-comment-p)
	   (looking-at "\\>")
	   (save-excursion
	     (skip-syntax-backward "w") ;FIXME: is * in word syntax?
	     (looking-at ansys-block-else-or-end-regexp))) ;FIXME: and otherwise?
	(save-excursion
	  (cond
	   ((match-end 1)		;else keyword
	    (setq eb-keyword
		  (buffer-substring-no-properties
		   (match-beginning 1) (match-end 1)))
	    (ansys-up-block))
	   ((match-end 2)		;end keyword
	    (setq eb-keyword
		  (buffer-substring-no-properties
		   (match-beginning 2) (match-end 2)))
	    (ansys-skip-block-backwards)))
	  (forward-word)
	  (setq pos (point)
		bb-keyword (buffer-substring-no-properties
			    (match-beginning 0) pos)
					;		pos (1+ pos);FIXME: bb-arg is eating commas
		eol (ansys-position 'eol)
		bb-arg (save-excursion
			 (save-restriction
			   (goto-char pos)
			   (while (and (skip-syntax-forward "^<" eol)
				       (ansys-in-string-p)
				       (not (forward-char 1))))
			   (skip-syntax-backward " ")
			   (buffer-substring-no-properties pos (point)))))
	  (if (member-ignore-case
	       eb-keyword (cdr (assoc-string bb-keyword ansys-block-match-alist 1)))
	      (progn
		(message "`%s' matches `%s%s'" eb-keyword bb-keyword bb-arg)
		(when (pos-visible-in-window-p)
		  (sit-for ansys-blink-matching-delay)))
	    (error "Block keywords `%s' and `%s' do not match"
		   bb-keyword eb-keyword)))))))

;;; Abbreviations
(unless ansys-mode-abbrev-table
  (let ((ac abbrevs-changed)) ;no offer to save unnecessary .abbrev_defs
    (define-abbrev-table 'ansys-mode-abbrev-table ())
    (define-abbrev ansys-mode-abbrev-table
      "`i" "*IF,i,LT,0,THEN\n\n!! *ELSEIF,i,GT,10\n!! *ELSE\n*ENDIF\n"
      '(lambda () (previous-line 4)))
    (define-abbrev ansys-mode-abbrev-table
      "`d" "*DO,i,1,10,1\n\n*CYCLE\n*ENDDO\n" '(lambda () (previous-line 3)))
    (define-abbrev ansys-mode-abbrev-table "`p" "" 'ansys-insert-pi)
    (define-abbrev ansys-mode-abbrev-table "`ii" "" 'ansys_if)
    (define-abbrev ansys-mode-abbrev-table "`dd" "" 'ansys_do)
    (define-abbrev ansys-mode-abbrev-table "`e" "/eof ------------------\n" '(lambda () (indent-according-to-mode)))
    (define-abbrev ansys-mode-abbrev-table "`c" "!! ====================\n" '(lambda () (indent-according-to-mode)))
    (setq abbrevs-changed ac))) ;reset `abbrevs-changed' to previous state

;;; Bug reporting

(defun ansys-submit-bug-report ()	;from Octave
  "Open an Emacs mail buffer with an Ansys mode bug report."
  (interactive)
  (require 'reporter)
  (let (salutation
	(reporter-prompt-for-summary-p t)) ;asks for summary goes into
					;subject line
    (when (y-or-n-p "Do you want to submit a bug report? ")
      (setq salutation
	    "Please describe briefly what your problem is and which actions
  triggered the bug.  A self contained, reproducible test case
  would be advantageous.")
      (reporter-submit-bug-report
       ansys-maintainer-address
       "Ansys mode"		  ;becomes prefix for the subject line
       (list
	'ansys_version
	'ansys_mode_version

	;; Ansys mode defcustoms are below
	'ansys-dynamic-highlighting-flag
	'ansys-program
	'ansys-help-file
	'ansys-lmutil-program
	'ansys-license-file
	'ansys-license
	'ansys-license-types
	'ansys-indicate-empty-lines-flag
	'ansys-current-ansys-version
	'ansys-comment-padding
	'ansys-comment-add
	'ansys-code-comment-column
	'ansys-auto-indent-flag
	'ansys-indent-comment-suffix
	'ansys-ruler-wide-flag
	'ansys-require-spaces-flag
	'ansys-blink-matching-block-flag
	'ansys-blink-matching-delay
	'ansys-block-offset
	'ansys-outline-string
	'ansys-mode-hook
	)
       nil
       nil
       salutation))))

(load "ansys-template")
(load "ansys-process")

;;; --- dynamic highlighting ---

;; ---- Restrictions ----
;; Ansys variables or parameters in Ansys parlance:
;; 1.) Begin with a letter
;; 2.) Contain only letters, numbers and the underscore '_'
;; 3.) Contain no more than 32 characters
;; 4.) Any variable ending with an underscore are *not* shown with the *STATUS command
;; 5.) The maximum number of parameter (< 5000) is retrieved by *GET,par,PARM,,MAX
;; 6.) (A<B) returns the value of A when A is less than B, B otherwise!

(defun ansys-asterisk-regexp(string)
  (when (= (elt string 0) ?*)
    (setq string (concat "\\" string)))
  string)

(defun ansys-string-length-predicate (s1 s2)
  (< (length s1) (length s2)))

(defun ansys-find-duplicate-p (entry list)
  (let ((l list) p)
    (while (and (not p) l)
      (setq p (assoc-string entry (car l) 'ignore-case))
      (pop l))
    p))

(defun ansys-find-user-variables (&optional a b c) ;NEW
					;pseudo arguments for after-change-functions
  "Find all user variables in the current buffer.
Pre-process the findings into the variable
`ansys-user-variables' for subsequent fontifications."
  (interactive)
  (save-excursion
    (save-match-data
      (let (res var)	; Start with Ansys *USE vars
	(setq ansys-user-variables ())
	(goto-char (point-min))
	(dolist (command ansys-variable-defining-commands)
	  ;; format line, comment, message, C***
	  (while (re-search-forward
		  (concat (ansys-asterisk-regexp command)
			  "\\s-*,\\s-*\\([[:alpha:]][[:alnum:]_]\\{0,31\\}\\)") nil t)
	    (setq var (match-string-no-properties 1))
	    (unless (or (ansys-in-string-or-comment-p)
			(ansys-in-string-command-line-p)
			(ansys-in-format-construct-p)
			(ansys-find-duplicate-p var ansys-user-variables))
	      (add-to-list 'ansys-user-variables (list var (match-beginning 1)))))
	  (goto-char (point-min)))

	;; Ansys = assignment
	(while (re-search-forward
		"[^_]\\(\\<[[:alpha:]][[:alnum:]_]\\{0,31\\}\\)\\s-*=" nil t)
	  (setq var (match-string-no-properties 1))
	  (unless
	      (or (ansys-in-string-or-comment-p)
		  (ansys-in-string-command-line-p)
		  (ansys-in-format-construct-p)
		  (ansys-find-duplicate-p var ansys-user-variables))
	    (add-to-list 'ansys-user-variables (list var (match-beginning 1))))))))
  ;; we must sort the variables to their length otherwise some of them will be
  ;; shadowed: the longer the earlier
  (setq ansys-user-variables
	(sort ansys-user-variables '(lambda (arg1 arg2)
				      (> (length (car arg1)) (length (car arg2)))))))

;; in comments: ok
;; in * comments: ansys-in-asterisk-comment-p
;; clashes with command names
;; in format strings without % chars
(defun ansys-search-variable (variable limit)
  (save-excursion
    (while (progn
	     (re-search-forward variable limit t)
	     (or (ansys-in-asterisk-comment-p)
		 (and (or (ansys-in-format-construct-p)
			  (ansys-in-string-command-line-p)
			  (not (looking-at "%")))))))))

(defun ansys-highlight (limit)		;NEW
  "Find user variables from (point) to position LIMIT."
  (let ((var-list ansys-user-variables)
	(p1 (point))
	(p2 limit)
	m-data entry)
    (setq m-data (match-data))
    (dolist (var var-list)
      (setq entry (concat "\\<" (car var) "\\>"))
      (ansys-search-variable entry limit)
      (setq p1 (match-end 0))
      (when (and (> p1 0)
		 (< p1 p2)
		 (<= (cadr var) p1)	;do not highlight before defintion
		 )
	(setq p2 p1)
	(set-match-data m-data)))
    (goto-char p2)))

(defun ansys-display-variables ()	;NEW
  "Displays APDL variable assignments in the current buffer.
Together with the corresponding line number N (type \\[goto-line]
N for skipping to line N or place the cursor over the number and
C-u \\[goto-line] takes the number automatically)."
  (interactive)
  (let* ((current-buffer (buffer-name))
	 (buffer-name "*Ansys-variables*")
	 (variable-buffer (get-buffer-create buffer-name))
	 s
	 r
	 tmp)
    (save-excursion
      (set-buffer variable-buffer)
      (toggle-read-only -1)
      (kill-region (point-min) (point-max))
      (insert
       (propertize
	(concat "-*- APDL variables of buffer " current-buffer " -*-\n")
	'face 'match))
      (set-buffer variable-buffer)
      (insert
       (propertize
	(concat"      ----------  =  assignments ----------\n")
	'face 'font-lock-warning-face))
      (set-buffer current-buffer)
      (goto-char (point-min))
      (setq r "^\\s-*[^!\n=]*\\<.+\\>\\s-*=\\s-*[^=\n]*")
      (while (re-search-forward r nil t)
	(unless (string-match "^\\s-*/com\\|^\\s-*c\\*\\*\\*" (match-string 0))
	  (setq s (concat
		   (propertize (format "%5d " (line-number-at-pos)) 'mouse-face 'highlight 'face 'bold))
		   (match-string 0)
		   "\n")))
	(set-buffer variable-buffer)
	(insert s)
	(set-buffer current-buffer))
      (dolist (tmp ansys-variable-defining-commands)
	(set-buffer variable-buffer)
	(insert
	 (propertize
	  (concat"      ---------- " tmp " assignments ----------\n")
	  'face 'font-lock-warning-face)
	(set-buffer current-buffer)
	(goto-char (point-min))
	(setq r (concat "^[^!\n]*" (ansys-asterisk-regexp tmp) ".*"))
	(while (re-search-forward r nil t)
	  (unless (string-match "^\\s-*/com\\|^\\s-*c\\*\\*\\*" (match-string 0))
	    (setq s (concat
		     (propertize (format "%5d " (line-number-at-pos)) 'mouse-face 'highlight 'face 'bold)
		     (match-string 0)
		     "\n")))
	  (set-buffer variable-buffer)
	  (insert s)
	  (set-buffer current-buffer)))
      (set-buffer variable-buffer)
      (goto-char (point-min))
      (toggle-read-only 1)
      (set-buffer current-buffer))
    (display-buffer buffer-name 'other-window)))

(defun ansys-customise-ansys ()		;NEW_C
  "Call the Emacs customisation facility for Ansys mode."
  (interactive)
  (customize-group "Ansys"))

(defun ansys-delete-other-window (&optional win) ;NEW_C
  "Delete the other, not selected Emacs window.
A window is in Emacs parlance a \"field\" where a buffer is
displayed in a window manager frame.  The command deletes only
the display of this file, not the data itself.  A frame can have
many windows (and Emacs can control many frames, by the way),
often the Emacs beginners confuse the term window with an Emacs
frame.  Optional prefix argument WIN is the WIN'th different
window in the current frame.  The default argument is 1."
  (interactive "p")
  (unless win (setq win 1))
  (other-window win)
  (delete-window))

(provide 'ansys-mode) ; this makes more sense when the file name is identical
					;to the feature name, what are subfeatures anyway?
					;needed for unload-feature

;; Local Variables:
;; mode: outline-minor
;; time-stamp-active: t
;; indicate-empty-lines: t
;; show-trailing-whitespace: t
;; word-wrap: t
;; End:

;;; ansys-mode.el ends here
