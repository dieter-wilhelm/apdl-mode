;;; ansys-mode.el -- Editor support for working with ANSYS FEA.

;; Copyright (C) 2006 - 2015  H. Dieter Wilhelm GPL V3

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Maintainer: H. Dieter Wilhelm
;; Created: 2006-02
;; Version: 16.1.1
;; Keywords: Languages, Convenience, ANSYS

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

;; Editor support for working with ANSYS FEA.

;; The ANSYS-Mode package provides support for the FEA (Finite Element
;; Analysis) program ANSYS (http://www.ansys.com) under Windows and
;; GNU-Linux systems.  It defines 'ANSYS-Mode', a major mode for
;; viewing, writing and navigating in APDL (Ansys Parametric Design
;; Language) files as well as providing managing and communication
;; capabilities for an associated ANSYS solver process.

;; The mode's capabilities are sophisticated but the documentation is
;; targeted for ANSYS users with little Emacs experience.  An online
;; version of its documention can be found at
;; http://dieter-wilhelm.github.io/ansys-mode/.

;;  The code is available on
;;  https://github.com/dieter-wilhelm/ansys-mode/. Regarding
;;  installation and further information please consult the
;;  accompanying README.org file.

;;; History:

;; Please consult the accompanying NEWS.org file.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- constants ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ansys_version "161"
  "ANSYS version on which ANSYS-Mode is based.")

(defconst ansys_mode_version "1"
  "ANSYS-Mode version number.")

(defconst ansys-parameter-substitution-commands-regexp
  "/TITLE\\|/STITLE\\|/COM\\|/AXLAB\\|/GCOLUMN\\|/TLABEL\\|/AN3D"
  "Regexp of command names which have a string behind them.")

(defconst ansys-string-commands-regexp
  "C\\*\\*\\*\\|/TITLE\\|/STITLE\\|/COM\\|/AXLAB\\|/GCOLUMN\\|/TLABEL\\|\\*ABBR\\|/AN3D"
  "Regexp of command names which have a string behind them.")

(defconst ansys-variable-defining-commands ;association list
  '(
    ("\\*ask\\w*" . "*ASK")
    ("\\<cm\\>" . "CM")
    ("\\<cmblock\\w*" . "CMBLOCK")
    ("\\*dim\\w*"."\\*DIM")
    ("/dir\\w*" . "/DIRECTORY")
    ("\\*do\\>" . "\\*DO")
    ("\\*dow\\w*" . "\\*DOWHILE")
    ("\\*get\\w*". "\\*GET")
    ("/inq\\w*"."/INQUIRE")
    ("\\*mfu\\w*"."*MFUN")
    ("\\*mop\\w*"."*MOPER")
    ("\\<path\\w*"."PATH")
    ("\\<page\\w*" "PAGET")
    ("\\<pdef\\w*"."PDEF")
    ("\\*sre\\w*"."*SREAD")
    ("\\*set.?"."*SET") ;ANSYS inconsistency *SET works only with one additional character
    ("\\*top\\*w"."*TOPER")
    ("\\*vge\\w*"."*VGET") ; Not true in 14.0 variable must be dimensiond with *dim
    ("\\*vfu\\w*"."*VFUN")
    ("\\*vit\\w*"."*VITRP")
    ("\\*vop\\w*"."*VOPER")
    ("\\*vsc\\w*"."*VSCFUN")
    ("\\*vfi\\w*"."*vfill")
    )
  "Alist for commands which define user variables.
In the form of (regexp . command_string), intentionally excluded
is the \"=\" assignment command.")

(defconst ansys-use-variables
  '("ARG[1-9]" "AR[1][0-9]")
  "Variable containing the ANSYS *USE variables regexp.
ARG[1-9] and AR[1][0-9] are macro local variables and can be
passed to the *USE command.  Additionally AR[2-9][0-9] are pure
macro local variables.")

(defconst ansys-format-commands-regexp
  "\\*[mM][sS][gG]\\|\\*[vV][rR][eE]\\|\\*[vV][wW][rR]\\|\\*[mM][wW][rR]"
  "Regexp of command names which have one or more format lines.")

(defconst ansys-maintainer-address
  "Dieter Wilhelm <dieter@duenenhof-wilhelm.de>" ;bug-gnu-emacs@gnu.org
  "Address of current maintainer of the ANSYS-Mode.")

(defconst ansys-comment-char ?!
  "The ANSYS comment character.")

(defconst ansys-non-code-line-regexp "^\\s-*\\($\\|\\s<\\|[+[:digit:]-]\\)"
  "Regexp indicating a comment -, number - or an empty line.
A comment line contrasting a \"code comment\" which follows code
to be analysed from the ANSYS solver/interpreter.  A \"number
line\" is a line beginning with a number e. g. from an element
block or with a `+' or `-' sign.")

(defconst ansys-condensed-input-line-regexp ".*\\$"
  "Regexp indicating a condensed input line.")

(defconst ansys-comment-start-skip "\\S<+\\S-*"
  "Regexp to match the start of an ANSYS comment up to its body.
Used for the variable `comment-start-skip'.")

;; --- defcustoms ---

(require 'custom)

(defgroup ANSYS nil
  "Customisation group for the ANSYS-Mode."
  :version "23.1"
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :link '(url-link :tag "EmacsWiki" "http://www.emacswiki.org")
  :link '(url-link :tag "GoogleCode" "http://www.code.google.com/p/ansys-mode")
  :group 'Languages)

(defcustom ansys-hide-region-before-string "![ ... hidden"
  "String to mark the beginning of an invisible region. This string is
not really placed in the text, it is just shown in an overlay"
  :type '(string)
  :group 'hide-region)

(defcustom ansys-hide-region-after-string " region ... ]"
  "String to mark the beginning of an invisible region. This string is
not really placed in the text, it is just shown in an overlay"
  :type '(string)
  :group 'hide-region)

(defcustom ansys-hide-region-propertize-markers t
  "If non-nil, add text properties (colour) to the region markers."
  :type 'boolean
  :group 'hide-region)

(defcustom ansys-highlighting-level 2
  "This variable sets the level of highlighting.
There are three levels available, 0 a minimalistic level
optimised for speed and working with very large files (like
solver input files from WorkBench), 1 and 2.  Level 0 highlights
only the minimum (unambiguous) length of ANSYS command names and
variable definitions with the '=' operator.  Level 1 highlights
complete command names, together with functions, elements,
deprecated elements, undocumented commands, strings from string
commands and the APDL operators.  Level 2 is the same as 1,
except that all defined user variables and unambiguous command
names (also interpreter-ignored characters behind them) are
highlighted as well.  The user variables are highlighted
\"statically\" only, newly defined variables are only taken into
account after `ansys-display-variables'
\(\\[ansys-display-variables]) is called, this updating is done
dynamically i. e. during editing when the variable
`ansys-dynamic-highlighting-flag' is set to t."
  :type 'integer
  :group 'ANSYS
  :link '(variable-link font-lock-maximum-decoration ))

(defcustom ansys-current-ansys-version ansys_version
  "String of the used ANSYS version.
This variable is used by the `ansys-skeleton-header' template and
for setting up variables defaults with ANSYS path specifications,
like in the variable `ansys-program'."
  :type 'string
  :group 'ANSYS)

(defcustom ansys-dynamic-highlighting-flag t
  "Non-nil means that ANSYS-Mode highlights user defined variables.
Warning: This option is computational expensive and --depending
on the file size and your hardware --it might make your editing
experience somewhat sluggish.  Currently dynamic highlighting of
user variables is only implemented for files with the extension
\".mac\" and in the highest highlighting level (please see the
variable `ansys-highlighting-level') otherwise the fontification
of variables is only static.  To take effect after setting this
variable you have to restart `ansys-mode'."
  :type 'boolean
  :group 'ANSYS)

(defcustom ansys-indicate-empty-lines-flag nil
  "Non-nil means indicate empty lines on window systems.
Do this visually at the end of an ANSYS buffer in the left
fringe.  You have to reload function `ansys-mode' for this
variable to take effect."
  :type 'boolean
  :group 'ANSYS)

(defcustom ansys-comment-padding " "
  "Padding string that `comment-dwim' puts between comment chars and text.
Extra spacing between the comment character(s) and the comment
text makes the comment easier to read.  This padding is not
effective for code comments (comments behind code)."
  :type 'string
  :group 'ANSYS)

(defcustom ansys-comment-add 1
  "How many additional comment characters are inserted by \\[comment-dwim].
This determines the default value of the numeric argument of
`comment-dwim'.  It should generally stay 0, except for a few
modes like Lisp where it can be convenient to set it to 1 so that
regions are commented with two semi-colons."
  :type 'integer
  :group 'ANSYS)

(defcustom ansys-code-comment-column 25
  "Column where ANSYS code comments (behind code) are placed."
  :type 'integer
  :group 'ANSYS)

(defcustom ansys-auto-indent-flag t
  "Non-nil means indent line when typing the SPC key.
The space character is also inserted."
  :type 'boolean
  ;;  :options '(t nil) ; not necessary with booleans in Customise
  :group 'ANSYS)

(defcustom ansys-indent-comment-suffix ""
  "String placed after the ANSYS comment char in an code comment.
See `ansys-indent-comment-string'."
  :type 'string
  :group 'ANSYS)

(defcustom ansys-ruler-wide-flag nil
  "Non-nil means show a 80 characters wide temporary ruler.
Nil means show a narrower temporary ruler with 50 characters."
  :type 'boolean
  :group 'ANSYS)

(defcustom ansys-require-spaces-flag nil
  "Non-nil means \\[insert-parentheses] inserts whitespace before ().
When there is a region marked function `insert-parentheses'
inserts the parentheses around the active region."
  :type 'boolean
  :group 'ANSYS)

(defcustom ansys-blink-matching-block-flag t
  "Non-nil means blinking of matching ANSYS block keywords.
Skip temporary to the matching beginning of the block when
inserting a newline after an *ELSE or *END keyword."
  :type 'boolean
  :group 'ANSYS)

(defcustom ansys-blink-matching-delay .7
  "Time in seconds for skipping to a matching block.
See also the variable `ansys-blink-matching-block-flag'."
  :type 'number
  :group 'ANSYS)

(defcustom ansys-block-offset 2
  "Indentation column(s) for statements in a block structure."
  :type 'integer
;; :options only for types hook, plist and alist
  :group 'ANSYS)

(defcustom ansys-outline-string "@"
  "String specifying outline headings (see `outline-regexp')."
  :type 'string
  :group 'ANSYS)

(defcustom ansys-mode-hook nil
  "Normal hook run before entering ANSYS-Mode.
A hook is a variable which holds a collection of functions."
  :type 'hook
  :options '(ansys-show-paren-mode ansys-outline-minor-mode ansys-ruler-mode ansys-auto-insert-mode)
  :group 'ANSYS)

(require 'align)

(defcustom ansys-align-rules-list
    '(
      (ansys-align-=
       (regexp   . "\\(\\s-*\\)=")
       (modes    . '(ansys-mode))
       (justify  . t)
       (tab-stop . nil))

      (ansys-align-text-column
       (regexp   . "=\\(\\s-*[0-9]+\\|\\s-*\\)")
       (modes    . '(ansys-mode))
       (justify . t)
       (tab-stop . nil))

      (ansys-align-comment
       (regexp   . "[0-9.)]+\\(\\s-*\\)\\!")
       (modes    . '(ansys-mode))
       (tab-stop . nil))
      )
    "Rules for aligning ANSYS variable definitions."
    :type align-rules-list-type
    :group 'ansys-mode
    )

;; (put 'my-align-rules-list 'risky-local-variable t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- variables ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ansys-overlay-str ""
  "variable to store previous overlay string.")

(defvar ansys-hide-region-overlays nil
  "Variable to store the regions we put an overlay on.")

(defvar ansys-help-overlay nil
  "Overlay for displaying the ansys parameter help.")

(defvar ansys-timer nil
  "Timer variable to set up a timer for overlay clearing.
  Please have a look at the function `ansys-manage-overlay'.")

(defvar ansys-indent-comment-string
  (concat (char-to-string ansys-comment-char) ansys-indent-comment-suffix)
  "String to insert when creating an ANSYS code comment.")

(defvar ansys-user-variables nil
  "Variable containing the user variables and line No of first occurance.
The list is used for the display of these
 variables (`ansys-display-variables').")

(defvar ansys-user-variable-regexp nil
  "Variable containing the user variables regexp.
The regexp is used for the
fontification (`ansys-highlight-variable') of these variables.")

(defvar ansys-is-unix-system-flag nil
  "Non-nil means computer runs a GNU-Linux system.")

(defvar ansys-previous-major-mode ""
  "The buffer's previous major mode (before ANSYS-Mode).")

(defvar ansys-mode-abbrev-table nil
  "Abbreviation definition table for the ANSYS-Mode.
All ANSYS abbrevs start with a grave accent \"`\".  \"`?\" lists
the currently defined abbreviations.")

(defvar ansys-parameter-help-position) ;for the compiler

(cond
 ((version< emacs-version "24")
  (defvar ansys-parameter-help-position 1
    "Cursor position in -show-command-parameters.")
  (make-local-variable 'ansys-parameter-help-position))
 (t
  (if (fboundp 'defvar-local)
      (defvar-local ansys-parameter-help-position 1
	"Cursor position in -show-command-parameters."))))

;;; --- constants ---

(defconst ansys-continuation-line-regexp ".*?&\\s-*$"
  "Regexp indicating a continuation line (of the *MSG command).")

(defconst ansys-begin-keywords
  '("\\*[dD][oO]" "\\*[dD][oO][wW][hH]?[iI]?[lL]?[eE]?"
    "\\*[iI][fF].*[tT][hH][eE][nN]" "\\*[cC][rR][eE][aA][tT][eE]")
  "Regexps describing ANSYS block begin keywords.")

(defconst ansys-else-keywords
  '("\\*[eE][lL][sS][eE][iI][fF]" "\\*[eE][lL][sS][eE]"
    "\\*[cC][yY][cC][lL][eE]")
  "Regexps describing ANSYS block else keywords.")

(defconst ansys-end-keywords
  '("\\*[eE][nN][dD][dD][oO]" "\\*[eE][nN][dD][iI][fF]"
    "\\*[eE][nN][dD]")
  "Regexps describing ANSYS end keywords.")

(defconst ansys-number-line-regexp
  "^\\s-*[(+-]?[[:digit:]]"
  "Regexp describing an ANSYS number line.
Used for skipping pure number lines and CMBLOCK format strings")

(defconst ansys-block-begin-regexp
  (concat "\\("
	  (mapconcat 'identity ansys-begin-keywords "\\|")
	  "\\)\\>")
  "Regexp containing the ANSYS begin keywords.")

(defconst ansys-block-else-regexp
  (concat "\\("
	  (mapconcat 'identity ansys-else-keywords "\\|")
	  "\\)\\>")
  "Regexp containing the ANSYS else keywords.")

(defconst ansys-block-end-regexp
  (concat "\\("
	  (mapconcat 'identity ansys-end-keywords "\\|")
	  "\\)\\>")
  "Regexp containing the ANSYS end keywords.")

(defconst ansys-block-begin-or-end-regexp
  (concat ansys-block-begin-regexp "\\|" ansys-block-end-regexp)
  "Regexp containing ANSYS begin and end keywords.")

(defconst ansys-block-else-or-end-regexp
  (concat ansys-block-else-regexp "\\|" ansys-block-end-regexp)
  "Regexp containing the ANSYS else or end keywords.")

(defconst ansys-block-match-alist
  '(("*IF" . ("THEN" "*ELSE" "*ELSEIF" "*ENDIF"))
    ("*DO" . ("*ENDDO"))
    ("*DOWHILE" . ("*ENDDO"))
    ("*CREATE" . ("*END")))
  "Alist with ANSYS's matching block keywords.
It has ANSYS's begin keywords as keys and a list of the
corresponding else or end keywords as associated values.")

(defconst ansys-column-ruler-wide
  (propertize
   (concat
    "0        10        20        30        40        50        60        70        80\n"
    "|    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |\n")
   'font-lock-face 'bold)
  "Contains the string for the wide ruler.
Ruler strings are displayed above the current line with
\\[ansys-column-ruler].")

(defconst ansys-column-ruler-narrow
  (propertize
   (concat
    "0        10        20        30        40        50\n"
    "|    |    |    |    |    |    |    |    |    |    |\n")
   'font-lock-face 'bold)
  "Narrow ruler string.
Ruler strings are displayed above the current line with \\[ansys-column-ruler].")

(when (> emacs-major-version 21)
  (add-to-list 'insert-pair-alist '(?\* ?\*))
  (add-to-list 'insert-pair-alist '(?\$ ?\$))
  (add-to-list 'insert-pair-alist '(?\% ?\%))
  )

(defconst ansys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "`" 'ansys-abbrev-start) ;``?' lists abbrevs
    ;; (define-key map "\t" 'indent-according-to-mode)		  ;redundant
    ;; standard behaviour of M-j is sufficient for me
    (define-key map "\e\t" 'ansys-complete-symbol) ;or M-C-i
    ;; --- changed standard Emacs keybindings ---
    (define-key map " " 'ansys-electric-space)
    (define-key map "\M-j" 'ansys-indent-format-line)
    (define-key map "\n" 'ansys-reindent-then-newline-and-indent)
    ;; end block indentation

    ;; --- especially interesting for continuation lines and condensed input
    (define-key map "\M-a" 'ansys-command-start)
    (define-key map "\M-e" 'ansys-command-end)
    ;; -- adaption of mark-paragraph
    (define-key map "\M-h" 'ansys-mark-paragraph)
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
    (define-key map [?\M-\"] 'insert-pair)
    (define-key map "\C-c[" 'insert-pair)
    (define-key map "\C-c'" 'insert-pair)
    (define-key map "\C-c%" 'insert-pair)

    ;; (define-key map [?\C-c?\C-%] 'insert-pair)
    ;; (define-key map [?\C-c?\C-[] 'insert-pair)
    ;; (define-key map [?\C-c?\C-'] 'insert-pair)
    ;; --- miscellaneous ---
    (define-key map [?\C-c?\C-+] 'ansys-zoom-in)
    (define-key map [?\C-c?\C--] 'ansys-zoom-out)
    (define-key map [?\C-c?\C-<] 'ansys-move-left)
    (define-key map [?\C-c?\C->] 'ansys-move-right)
    (define-key map [?\C-c?\C-^] 'ansys-move-up)
    (define-key map [?\C-c?\C-_] 'ansys-move-down)
    (define-key map "\C-x4k" 'ansys-delete-other-window)
    (define-key map "\C-c\C-a" 'ansys-align)
    (define-key map "\C-c\C-b" 'ansys-browse-ansys-help)
    (define-key map "\C-c\C-c" 'ansys-send-to-ansys)
    (define-key map "\C-c\C-d" 'ansys-do)
    (define-key map "\C-c\C-e" 'ansys-display-error-file)
    (define-key map "\C-c\C-f" 'ansys-fit)
    (define-key map "\C-c\C-g" 'ansys-start-graphics)
    (define-key map "\C-c\C-h" 'ansys-start-ansys-help)
;    (define-key map "\C-c\C-i" 'ansys-if)
    (define-key map "\C-c\C-i" 'ansys-iso-view)
    (define-key map "\C-c\C-j" 'ansys-send-to-ansys-and-proceed)
;    (define-key map "\C-c\C-j" (if (boundp 'ansys-job) 'ansys-job))
    (define-key map "\C-c\C-k" 'ansys-kill-ansys)
    (define-key map "\C-c\C-l" 'ansys-license-status)
    (define-key map "\C-c\C-m" 'ansys-start-ansys) ;this is also C-c RET
    (define-key map "\C-c\C-o" 'ansys-process-status)
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
       map)
    "Keymap for the ANSYS-Mode.")

(defun ansys-toggle-mode nil ;FIXME this toggles also all ansys minor-hooks?
  "Restore the buffer's previous major mode, if possible."
  (interactive)
  (if (or (string= ansys-previous-major-mode "ansys-mode")
	  (string= ansys-previous-major-mode ""))
      (error "There was no previous major mode except \"ANSYS-Mode\"") ;buffers opended with auto-mode
    (funcall ansys-previous-major-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- font locking stuff ---

(defvar ansys-command-regexp)
(defvar	ansys-command-regexp)
(defvar ansys-deprecated-element-regexp)
(defvar ansys-undocumented-command-regexp)
(defvar ansys-get-function-regexp)
(defvar ansys-command-regexp-1)
(defvar ansys-command-regexp-2a)
(defvar ansys-command-regexp-2b)
(defvar ansys-command-regexp-2c)
(defvar ansys-element-regexp)
(defvar ansys-parametric-function-regexp)
(defvar ansys-dynamic-prompt)
(defvar ansys-completions)

(load "ansys-keyword")

(defface ansys-arg-face
  '((((min-colors 88) (class color) (background light))
     :foreground "red1")
    (((class color) (background light))
     :foreground "red")
    (((min-colors 88) (class color) (background dark))
     :foreground "yellow1")
    (((class color) (background dark))
     :foreground "yellow")
    (t
     :weight bold))
  "Face for highlighting local variables AR(G), _return, ..."
  :group 'ansys-faces)

(defvar ansys-arg-face		'ansys-arg-face
  "Face name to use for local vars AR(G), _return, ...")

(let (;; = variable defs + reserved _names
      ;; wie need something behind the = otherwise it's a cleanup
      ;; variables + reserved _names (max. 32 chars long)
      (variable_r
       "^\\s-*\\([[:alpha:]_][[:alnum:]_]\\{0,31\\}\\)\\s-*=")
      ;; reserved vars consisting of a single "_" are valid in ANSYS 12.1
      (reserved_vars_r
      "\\_<\\(_[[:alnum:]_]\\{0,31\\}\\>\\)"))

;; font-lock-keyword-face is the default face
  (defconst ansys-font-lock-keywords
    `(
      (,variable_r 1 font-lock-variable-name-face); overwritting commands

      (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
		ansys-command-regexp
		"\\)") 1 font-lock-keyword-face)
      )
    )

  (defconst ansys-font-lock-keywords-1
  `(
    ;; /eof is special: it crashes ANSYS in interactive mode
    ;; TODO /eof is highlighted only first in line not behind $
    ("\\(?:^\\|\\$\\)\\s-*\\(/[eE][oO][fF].*\\)" 1 'trailing-whitespace t)

    ;; deprecated ansys * comment with 12.1
    ;; fini * bla : returns "* no longer valid as comment character - please use !"
    ;; * bla : returns a warning *bla is not a command
    ;; bla = 3 * 4 : returns still 3!
    ("[[:alnum:]_]+\\s-+\\(\\*.*$\\)" 1 font-lock-comment-face prepend)
    					;^[:alnum:] to avoid spurious
    					;asterisk command fontification
    ;; some string faces
    ("\\(?:^\\|\\$\\)\\s-*\\(?:/TIT\\|/TITL\\|/TITLE\\)\\s-*,\\(.*\\)$" 1
     font-lock-doc-face t) ;titles
    ("\\(?:^\\|\\$\\)\\s-*/[cC][oO][mM].?\\(.\\{0,75\\}\\)" 1 font-lock-doc-face t)
       ;highlight message of comment command /COM (no comment (!)
       ;is possible behind /COM), no separating comma necessary

    ;; multiline format constructs
;; ("^\\s-*\\(?:\\*[mM][sS][gG]\\|\\*[vV][rR][eE]\\|\\*[vV][wW][rR]\\|\\*[mM][wW][rR]\\).*\n\\(\\(?:.*&\\s-*\n\\)+.*\\)" ;format constructs
;;  (1 font-lock-doc-face t))


    ;; ("&\\s-*$" 0 font-lock-type-face t) ;format continuation char
    ;; ("%" 0 font-lock-type-face prepend) ;single % acts as a format
    		  ;specifier and pair %.% is a parameter substitution
(ansys-higlight-procent-and-ampersand (0 'font-lock-type-face t))
;("%\\|&\\s-*$" (0 'font-lock-type-face t))

      ;/SYS command sends string to OP,no parameter substitution!
    ("^\\s-*/[sS][yY][sS]\\s-*,\\(.\\{1,75\\}\\)$" 1
     font-lock-doc-face t)
    ;TODO: c*** should get fontification from command regexp
    ("^\\s-*\\([cC]\\*\\*\\*\\)[ ,]?\\(.\\{1,75\\}\\)"
      (1 font-lock-keyword-face t) (2 font-lock-doc-face t))
   	      ;only 75 characters possible no separator necessary

    ;; *use variables, local macro call arguments
;;   ("\\<\\(ARG[1-9]\\|AR[1][0-9]\\)\\>" . font-lock-warning-face)
   ("\\<\\(ARG[1-9]\\|AR[1][0-9]\\)\\>" . ansys-arg-face)

    ;; elements
    (,ansys-deprecated-element-regexp . font-lock-warning-face)
    (,ansys-element-regexp . font-lock-builtin-face)

    ;; reserved vars consisting of a single "_" are valid in A. 12.1
    (,reserved_vars_r 1 font-lock-warning-face)

    ("_RETURN" 0 ansys-arg-face append)

    ;; = variable defs (with reserved _names), overwritting commands
    (,variable_r 1
		 font-lock-variable-name-face) ; variables (max. 32 chars long)

    (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
	      ansys-undocumented-command-regexp
	      "\\)\\(\\w*\\)") . font-lock-constant-face)

    ;; get- and parametric-functions
    (,(concat "\\<\\("
	      ansys-get-function-regexp
	      "\\)(") 1 font-lock-function-name-face)
    (,(concat "\\<\\("
	      ansys-parametric-function-regexp
	      "\\)(") 1 font-lock-function-name-face)

    ;; command keywords first
    (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
	      ansys-command-regexp-1
	      "\\)\\>") 1 font-lock-keyword-face)

;; user variables
;(ansys-highlight-variable . font-lock-variable-name-face)

    ;; some operators
    ("\\$" . 'font-lock-type-face) ;condensed input line
    (":" . 'font-lock-type-face)   ;colon loops only

    ;; deprecated *go labels (max 8 chars including the colon) only at
;; the line beginning because they might clash with 'colon' loops
    ("^\\s-*:\\([[:alnum:]_]\\{1,7\\}\\)" 1 font-lock-type-face) ;GOTO Labels, branching
)
)

;; C-u C-x = -- describe char
;; order of execution

;; syntactic overwritting nothing fontification

  ;; strings and /eof overwritting syntactic fonts and command face
  ;; respectively

;; /eof warning: overwriting keyword face
;; * comments (must overwrite comments behind it)
;; strings in string commands
;; ?pairs of %VAR% with valid variable symbols
;; %X % with format specifier
;; *msg formatting
;; & only at line endings

;; c*** overwriting everything even %VAR%!
;; /sys command no parameter substitution

  ;; keep previous stuff

;; *use variables in warning face
;; = defs (overwritting commands)
;; : $ operators
;; elements
;; commands
;; experimental user variables

  (defconst ansys-font-lock-keywords-2
  `(
    ;; /eof is special: it crashes ANSYS in interactive mode
    ;; TODO /eof is highlighted only first in line not behind $
    ("\\(?:^\\|\\$\\)\\s-*\\(/[eE][oO][fF].*\\)" 1 'trailing-whitespace t)

    ;; deprecated ansys * comment with 12.1
    ;; fini * bla : returns "* no longer valid as comment character - please use !"
    ;; * bla : returns a warning *bla is not a command
    ;; bla = 3 * 4 : returns still 3!
    ("[[:alnum:]_]+\\s-+\\(\\*.*$\\)" 1 font-lock-comment-face prepend)
    					;^[:alnum:] to avoid spurious
    					;asterisk command fontification
    ;; some string faces
    ("\\(?:^\\|\\$\\)\\s-*\\(?:/TIT\\|/TITL\\|/TITLE\\)\\s-*,\\(.*\\)$" 1
     font-lock-doc-face t) ;titles
    ("\\(?:^\\|\\$\\)\\s-*/[cC][oO][mM].?\\(.\\{0,75\\}\\)" 1 font-lock-doc-face t)
       ;highlight message of comment command /COM (no comment (!)
       ;is possible behind /COM), no separating comma necessary

    ;; multiline format constructs
("^\\s-*\\(?:\\*[mM][sS][gG]\\|\\*[vV][rR][eE]\\|\\*[vV][wW][rR]\\|\\*[mM][wW][rR]\\).*\n\\(\\(?:.*&\\s-*\n\\)+.*\\)" ;format constructs
 (1 font-lock-doc-face t))


    ;; ("&\\s-*$" 0 font-lock-type-face t) ;format continuation char
    ;; ("%" 0 font-lock-type-face prepend) ;single % acts as a format
    		  ;specifier and pair %.% is a parameter substitution
(ansys-higlight-procent-and-ampersand (0 'font-lock-type-face t))
;("%\\|&\\s-*$" (0 'font-lock-type-face t))

      ;/SYS command sends string to OP,no parameter substitution!
    ("^\\s-*/[sS][yY][sS]\\s-*,\\(.\\{1,75\\}\\)$" 1
     font-lock-doc-face t)
    ;TODO: c*** should get fontification from command regexp
    ("^\\s-*\\([cC]\\*\\*\\*\\)[ ,]?\\(.\\{1,75\\}\\)"
      (1 font-lock-keyword-face t) (2 font-lock-doc-face t))
   	      ;only 75 characters possible no separator necessary

    ;; *use variables, local macro call arguments
;   ("\\<\\(ARG[1-9]\\|AR[1][0-9]\\)\\>" . font-lock-warning-face)
   ("\\<\\(ARG[1-9]\\|AR[1][0-9]\\)\\>" . ansys-arg-face)

    ;; elements
    (,ansys-deprecated-element-regexp . font-lock-warning-face)
    (,ansys-element-regexp . font-lock-builtin-face)

    ;; reserved vars consisting of a single "_" are valid in A. 12.1
    (,reserved_vars_r 1 font-lock-warning-face)

    ("_RETURN" 0 ansys-arg-face append)

    ;; = variable defs (with reserved _names), overwritting commands
    (,variable_r 1
		 font-lock-variable-name-face) ; variables (max. 32 chars long)

    (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
	      ansys-undocumented-command-regexp
	      "\\)\\(\\w*\\)") . font-lock-constant-face)

    ;; get- and parametric-functions
    (,(concat "\\<\\("
	      ansys-get-function-regexp
	      "\\)(") 1 font-lock-function-name-face)
    (,(concat "\\<\\("
	      ansys-parametric-function-regexp
	      "\\)(") 1 font-lock-function-name-face)

    ;; command keywords first -2a no characters appended
    (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
	      ansys-command-regexp-2a
	      "\\)\\>") 1 font-lock-keyword-face)
    (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
    	      ansys-command-regexp-2b
    	      "\\)\\(\\w*\\)") (1 font-lock-keyword-face) (2 'font-lock-constant-face))
    (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
    	      ansys-command-regexp-2c
    	      "\\)\\(\\w*\\)") (1 font-lock-keyword-face) (2 'font-lock-constant-face))

;; user variables
(ansys-highlight-variable . font-lock-variable-name-face)

    ;; some operators
    ("\\$" . 'font-lock-type-face) ;condensed input line
    (":" . 'font-lock-type-face)   ;colon loops only

    ;; deprecated *go labels (max 8 chars including the colon) only at
;; the line beginning because they might clash with 'colon' loops
    ("^\\s-*:\\([[:alnum:]_]\\{1,7\\}\\)" 1 font-lock-type-face) ;GOTO Labels, branching
))

;; testing
(defconst ansys-font-lock-keywords-3
  '(
    ("%" (0 font-lock-builtin-face keep))
    ("^/com.*" (0 font-lock-string-face prepend))
    ("bla" (0 font-lock-variable-name-face prepend))
    )
  )
)

(defconst ansys-font-lock-keyword-list
      '(
	ansys-font-lock-keywords
	ansys-font-lock-keywords-1
	ansys-font-lock-keywords-2
	;; testing
	ansys-font-lock-keywords-3
	  ))

(defconst ansys-mode-syntax-table     ;FIXME check ANSYS operators and
					;allowed variable characters
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\r " " table)
    (modify-syntax-entry ?\t " " table)
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
    (modify-syntax-entry ?\` "w" table) ;ansys-mode abbreviation specifier,
					;not an operator but "word".
    (modify-syntax-entry ?_ "_"  table) ;in ANSYS symbol component
    (modify-syntax-entry ?: "_"  table) ;ANSYS label specifier, not an operator
    (modify-syntax-entry ?* "_"  table) ;ANSYS asterisk commands syntax clashing
					;with algebraic operators but blink-matching-
					;needs this
    ;;     (modify-syntax-entry ?/ "w"  table)	;ANSYS slash commands
    (modify-syntax-entry ?\! "<" table) ;ANSYS comment character
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "w" table) ;`"' is *not* a string delimeter for ANSYS
    (modify-syntax-entry ?'  "\"" table); (modify-syntax-entry ?'  "." table)
					;Normally ANSYS string delimiter, but might clash
					;with usages of genitives etc.!
    (modify-syntax-entry ?~ "_" table) ;ANSYS connection commands, not an operator
    table)
  "Syntax table in use in `ansys-mode' buffers.")

(defconst ansys-mode-menu
  (list "ANSYS"
	["Comment/Un- Region"           comment-dwim :help "Comment out region or uncomment region, without a marked region start a code comment"]
	["Complete Symbol"              ansys-complete-symbol :help "Complete an ANSYS command, element or function name"]
	["Send/Copy Region or Paragraph"   ansys-send-to-ansys :label (if (ansys-process-running-p) "Send region or paragraph to ANSYS" "Copy code region or paragraph to the system clipboard") :help "In case of a running solver/interpreter send the marked region or by default the current paragraph to the interpreter, otherwise copy these lines to the system clipboard"]
	["Copy/Send above Code to ANSYS"ansys-copy-or-send-above :label (if (ansys-process-running-p) "Send above Code to ANSYS" "Copy above Code") :help "Either copy the code up to the beginning of file or, when a run is active, send it to the solver/interpreter"]
	["Close Logical Block"                  ansys-close-block :help "Close an open control block with the corresponding end command"]
	["Insert Parentheses"           insert-parentheses :help "Insert a pair of parentheses"]
	["Preview Macro Template"       ansys-display-skeleton :help "Preview macro templates in another window"]
	["Align region/section"       ansys-align :help "Align current region or section of ANSYS variable definitions"]
	"-"
	(list "Insert Template"
	      ["*IF ... *ENDIF"         ansys-if :help "Insert interactively an *if .. *endif construct"]
	      ["*DO ... *ENDDO"	        ansys-do :help "Insert interactively a *do .. *enddo loop"]
	      ["*IF ... *ELSEIF"	ansys-if-then :help "Insert interactively an *if,then .. (*elseif .. *else ..) *endif construct."]
	      ["MP"	                ansys-mp :help "Insert interactively an mp statement."]
	      ["Header"                 ansys-skeleton-header :help "Insert interactively the file header template"]
	      "-"
	      ["Insert Pi"              ansys-insert-pi :help "Insert the variable definition \"Pi = 3.1415...\""]
	      ["Configuration"          ansys-skeleton-configuration :help "Configuration code template"]
	      ["View Settings"          ansys-skeleton-view-settings :help "View settings like focus point, magnification, ..."]
	      ["Coordinate Sys. Display"ansys-skeleton-coordinates :help "Template for creating and handling coordinate systems"]
	      ["Working Plane Operations"ansys-skeleton-working-plane :help "Template for creating and handling the working plane"]
	      ["Multiplot Commands"     ansys-skeleton-multi-plot :help "Graphic commands which show multiple model entities simultaneously"]
	      ["Numbering Controls"     ansys-skeleton-numbering-controls :help "Commands for numbering and colouring model entities"]
	      ["Symbol Controls" ansys-skeleton-symbols :help "Graphic commands which show boundary conditions, surface loads and other symbols"]
	      ["Geometry Import"        ansys-skeleton-import :help "Command for importing IGES models"]
	      ["Control flow constructs"ansys-skeleton-looping :help "Commands for controlling loops (*do) and the program flow (*if)"]
	      ["Symmetry Expansions"    ansys-skeleton-expand :help "Commands for expanding the view of symmetric models to their full view"]
	      ["Element Definitions"    ansys-skeleton-element-definition :help "2D, 3D, Element defintions and their keyoptions"]
	      ["Material Definitions"   ansys-skeleton-material-defintion :help "Various material definitions: Steel, alu, rubber, ..."]
	      ["Modeling" ansys-skeleton-geometry :help "Operations for geometric modeling"]
	      ["Meshing Controls"       ansys-skeleton-meshing :help "Meshing control commands: Shapes, sizes, ..."]
	      ["Contact Pair Definition" ansys-skeleton-contact-definition :help "Full definition of flexible-flexible contact pairs"]
	      ["Rigid Contact"           ansys-skeleton-contact-rigid :help "Definition of the rigid target contact side"]
	      ["Contact Template"           ansys-skeleton-contact-template :help "Minimal working contact example"]
	      ["Boundary Conditions"    ansys-skeleton-bc :help "Commands for establishing boundary conditions"]
	      ["Buckling Analysis Type" ansys-skeleton-buckling :help "Commands for establishing a buckling analysis"]
	      ["Listings, Information, Statistics"ansys-skeleton-information :help "Parameter listings, graphic options, system information, run statistics"]
	      ["Solving"                ansys-skeleton-solve :help "ANSYS solver (/solu) commands and solver options"]
	      ["Post1 Postprocessing"   ansys-skeleton-post1 :help "General postprocessor (/post1) commands"]
	      ["Array Operations"       ansys-skeleton-array :help "Dimensioning, looping, changing array parameters"]
	      ["Path plot operations"   ansys-skeleton-path-plot :help "Commands for establishing paths and plotting entities on paths"]
	      ["Output to file"         ansys-skeleton-output-to-file :help "Commands for writing data to a file"]
	      ["Element Table Operations"ansys-skeleton-element-table :help "Commands for establishing and manipulation element tables"]
	      ["Post26 Postprocessing"  ansys-skeleton-post26 :help "Time history (/post26) postprocessing commands"]
	      ["Components"  ansys-skeleton-component :help "Components (Named selections in WorkBench) template"]
	      ["Selections"  ansys-skeleton-select :help "How to select stuff template"]
	      "-"
	      ["Outline template"          ansys-skeleton-outline-template :help "Empty skeleton of the structur of an APDL simulation, outlineing headers and sections"]
	      ["Beam template"          ansys-skeleton-beam-template :help "Insert a minimal template for a beam simulation"]
 	      ["Structural template"    ansys-skeleton-structural :help "Insert a minimal template for a structural simulation"]
	      ["Contact template"    ansys-skeleton-contact :help "Insert a minimal template for a structural contact simulation"]
	      ["Compilation of templates"   ansys-skeleton :help "Insert a compilation of selected templates"]
	      )
	(list "Navigate Code Lines"
	      ["Previous Code Line"	ansys-previous-code-line :help "Goto previous apdl code line"]
	      ["Next Code Line"		ansys-next-code-line :help "Goto next code line"]
	      ["Beginning of (Continuation) Command" ansys-command-start :help "Go to the beginning of the current command"]
	      ["End of (Continuation) Command"ansys-command-end :help "Go to the end of the current command"]
	      "-"
	      ["Split Format Line at Point"ansys-indent-format-line :help "Split current line, if in a comment continue the comment, if in an ANSYS format line insert the continuation character before splitting the line"]
	      )
	(list "Work with Logical Blocks"
	      ["Next Block End"		ansys-next-block-end :help "Go to the end of the current or next control block (*do, *if, ...)"]
	      ["Previous Block Start"   ansys-previous-block-start-and-conditional :help "Go to the beginning of the current or next control block (*do, *if, ...)"]
	      ["Down Block"		ansys-down-block :help "Move down one control block level"]
	      ["Up Block"		ansys-up-block :help "Move up one control block level"]
	      ["Skip Block Forward"     ansys-skip-block-forward :help "Skip to the end of the next control block"]
	      ["Skip Block Backwards"   ansys-skip-block-backwards :help "Skip to the beginning of previous control block"]
	      ["Hide Number Blocks"  ansys-hide-number-blocks :help "Hide all ANSYS number blocks (EBLOCK, NBLOCK, CMBLOCK)"]
	      ["Unhide Number Blocks"  ansys-unhide-number-blocks :help "Unhide all ANSYS number blocks (EBLOCK, NBLOCK, CMBLOCK)"]
	      ["Beginning of N. Block"  ansys-number-block-start :help "Go to the beginning of an ANSYS number blocks (EBLOCK, NBLOCK, CMBLOCK)"]
	      ["End of Number Block"    ansys-number-block-end :help "Go to the end of an ANSYS number blocks (EBLOCK, NBLOCK, CMBLOCK)"]
	      "-"
	      ["Close Block"            ansys-close-block :help "Close the current ANSYS control block with the respective closing command"]
	      ["Mark Block"             ansys-mark-block :help "Mark the current control block"]
	      ["Hide Region"  ansys-hide-region :help "Hide a marked region and display a hidden region message"]
	      ["Unhide Regions"  ansys-unhide-number-blocks :help "Unhide all hidden regions"]
	      )
	(list "Manage ANSYS Tasks"
	      ["Specify License Server or - File" ansys-license-file
	      :help "Change the license server specification (for an solver/interpreter run or the license status), either naming the license server machine (with port) or the actual license file" :active ansys-is-unix-system-flag]
	      ["Specify the License Interconnect Servers" ansys-ansysli-servers
	      :help "Change the interconnect server specification (for an solver/interpreter run)" :active ansys-is-unix-system-flag]
	      ["Specify License Utility" ansys-lmutil-program :help "Specify the ANSYS license utility executable"]
	      "-"
	      ["Specify ANSYS License Type" ansys-license :help "Specify the license type for an solver/interpreter run" :active ansys-is-unix-system-flag]
	      ["Specify Job Name of Run" ansys-job :help "Specify the job name for an solver/interpreter run"]
	      ["Specify ANSYS Executable or launcher" ansys-program :help "Specify the ANSYS solver/interpreter under GNU-Linux or the launcher (with complete path if not in $PATH)"]
	      ["Specify the Number of Processors" ansys-no-of-processors :help "Specify the number of processors to use for the ANSYS run definition." :active ansys-is-unix-system-flag]
	      [(concat (if ansys-is-unix-system-flag
		   "Start the APDL Solver/Interpreter"
		   "Start the Mechanical APDL Product Launcher"))   ansys-start-ansys :help "Start an APDL solver/interpreter run under GNU-Linux or the launcher under Windows" :active (not (ansys-process-running-p))]
	      ["License Status"              ansys-license-status :label (if ansys-is-unix-system-flag
               "Display License Status"
	        "Start License Utility") :help "Show the license usage in another window or start a license manager utility under Windows"]
	      ["Display ANSYS Run Status" ansys-process-status :help "Display the status of a possible ANSYS solver/interpreter run" :active (ansys-process-running-p)]
	      ["Exit ANSYS Run"         ansys-exit-ansys :help "Exit the active solver/interpreter run" :visible (ansys-process-running-p)]
	      "-"
	      ["Send ANSYS Command Interactively"ansys-query-ansys-command :help "Send interactively an APDL command to a running solver/interpreter process" :active (ansys-process-running-p)]
	      ["Start Graphics Screen"  ansys-start-graphics :help "Open the graphics screen of the ANSYS GUI" :active (ansys-process-running-p)]
	      ["Start Pan/Zoom/Rot. Dialog"ansys-start-pzr-box :help "Open the Pan/Zoom/Rotate dialog of the ANSYS GUI" :active (ansys-process-running-p)]
	      ["Replot"                 ansys-replot :help "Replot the ANSYS graphics window" :active (ansys-process-running-p)]
	      ["Fit Graphics into screen" ansys-fit :help "Fit the ANSYS graphics into the window" :active (ansys-process-running-p)]
	      ["Show Graphics in iso-view" ansys-iso-view :help "Show the current ANSYS graphic windows in isometric view" :active (ansys-process-running-p)]
	      ["Zoom In"                ansys-zoom-in :help "Zoom into the graphics" :active (ansys-process-running-p)]
	      ["Zoom Out"               ansys-zoom-out :help "Zoom out of the graphics" :active (ansys-process-running-p)]
	      ["Move Up"                ansys-move-up :help "Move graphics objects up" :active (ansys-process-running-p)]
	      ["Move Down"              ansys-move-down :help "Move graphics objects down" :active (ansys-process-running-p)]
	      ["Move Right"             ansys-move-right :help "Move graphics objects to the right" :active (ansys-process-running-p)]
	      ["Move Left"              ansys-move-left :help "Move graphics objects to the left" :active (ansys-process-running-p)]
	      "-"
	      ["Display all Emacs' Processes" list-processes :help "Show all active processes under Emacs, like the ANSYS help browser, etc."]
	      ["Display ANSYS Error File"ansys-display-error-file :help "Display in another window the ANSYS error file in the current directory"]
	      ["Write ANSYS Stop File" ansys-abort-file :help "Write a file (JOB.abt containing the word \"nonlinear\") for stopping a running solver/interpreter into the current directory"]
	      "-"
	      ["Kill ANSYS Run"        ansys-kill-ansys :help "Kill the current run":visible (ansys-process-running-p)]
	      )
	"-"
	["Show ANSYS Command Help"     ansys-show-command-parameters :help "Display a short help for the ANSYS command near the cursor with its parameters"]
	["Open APDL help in Browser"   ansys-browse-ansys-help :help "Open the original ANSYS help to a command or element name near the cursor in your default browser"]
	["Start ANSYS help system"     ansys-start-ansys-help :help "Start the ANSYS help browser"]
	["Display Variable Definitions"ansys-display-variables :help "Display all user variable definitions from the current file in another window"]
	["License Status"              ansys-license-status :label (if ansys-is-unix-system-flag
	     "Display License Status"
	   "Start License Utility") :help "Show the license usage in another window or start a license manager utility under Windows"]
;	["Insert Temporary Ruler"      ansys-column-ruler :help "Show a temporary ruler above the current line"]
	["Outline Minor Mode"         outline-minor-mode :style toggle :selected outline-minor-mode :help "Outline Mode is for hiding and selectively displaying headlines and their sublevel contents"]
	["Show Paren Mode"            show-paren-mode :style toggle :selected show-paren-mode :help "Show Paren Mode highlights matching parenthesis"]
	["Delete Selection Mode"      delete-selection-mode :style toggle :selected delete-selection-mode :help "Delete Selection Mode replaces the selection with typed text"]
	"-"
	["Show ANSYS Mode version"     ansys-mode-version :label (concat "ANSYS Mode Version: " ansys_version "."ansys_mode_version) :help "Display the ANSYS-Mode version in the mini buffer"]
	["List Expandable Abbreviations"     (list-abbrevs t) :help "Display a list of all abbreviation definitions for logical blocks"]
	["ANSYS Mode Help"	       describe-mode :help "Open a window with a description of ANSYS-Mode"]
	["Customise ANSYS Mode"        (customize-group "ANSYS") :help "Open a special customisation window for changing the values and inspecting the documentation of its customisation variables"]
	["ANSYS Mode Bug Report"       ansys-submit-bug-report :help "Open a mail template for an ANSYS-Mode bug report"]
	["Reload ANSYS Mode"           ansys-reload-ansys-mode :help "Loading the mode definitions anew and restarting ansys-mode"]
	"-"
	["Exit ANSYS Mode"             ansys-toggle-mode :help "Switch to the previous major mode of the file"])
  "Menu items for the ANSYS-Mode.")

;;; --- predicates ---

(defun ansys-in-asterisk-comment-p ()
  "Return t if the cursor is inside an ANSYS asterisk comment."
  (save-excursion
    (let ((lbp (line-beginning-position)))
      (if (search-backward " *" lbp t)
	  t
	nil))))

(defun ansys-in-string-command-line-p ()
  "Return t if in an ANSYS string command line."
  (save-excursion
   (back-to-indentation)
   (looking-at ansys-string-commands-regexp)))

(defun ansys-number-line-p ()
  "Return t if in an ANSYS number block."
  (save-excursion
    (beginning-of-line)
    (and (not (ansys-in-format-construct-p))
	 (looking-at ansys-number-line-regexp)))) ;"(" is for CMBLOCK format string

(defun ansys-default-command-p ()
  "Return t if in an ANSYS default command line."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*,")))

(defun ansys-in-indentation-p ()
  "Return t if in an indentation."
  (if (and (eolp) (bolp)) ; take care of empty lines
      nil
    (let ((p (point)))
      (save-excursion
	(back-to-indentation)
	(if (<= p (point)) t nil)))))

(defun ansys-first-line-p ()
  "Return t if at the first line."
  (save-excursion
    (beginning-of-line)
    (bobp)))

(defun ansys-last-line-p ()
  "Return t if at the last line."
  (save-excursion
    (end-of-line)
    (eobp)))

(defun ansys-continuation-line-p ()
  "Return t if in a continutation line of certain commands."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ansys-continuation-line-regexp) t nil)))

(defun ansys-in-format-command-line-p ()
  "Return t if in an ANSYS format command line, nil otherwise.
See the constant variable `ansys-format-commands-regexp' which
includes the commands which need formatting lines."
  (save-excursion
    (beginning-of-line)
    (if (looking-at
	 (concat "^\\s-*\\(" ansys-format-commands-regexp "\\)")) t nil)))

(defun ansys-in-format-construct-p ()
  "Return t if in an ANSYS format construct.
Otherwise nil, i.e. return nil when in a format command line."
  (cond ((ansys-continuation-line-p) t)
	((ansys-first-line-p) nil)
	(t (save-excursion
	     (forward-line -1)
	     (if (or
		  (ansys-continuation-line-p)
		  (ansys-in-format-command-line-p)) t nil)))))

(defun ansys-condensed-input-line-p ()
  "Return t if in an ANSYS condensed (... $ ...) input line."
  (save-excursion
    (beginning-of-line)
    (if (ansys-in-format-construct-p)
	nil
      (if (looking-at ansys-condensed-input-line-regexp)
	  t
	nil))))

(defun ansys-code-line-p ()
  "Return t if in an ANSYS code line, nil otherwise.
A code line is the complementary to the regexp
`ansys-non-code-line-regexp'."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ansys-non-code-line-regexp) nil t)))

(defun ansys-not-in-code-line-p ()
  "Return t if not in an ANSYS code line, nil otherwise.
A code line is the complementary to the regexp
`ansys-non-code-line-regexp'."
  (save-excursion
    (beginning-of-line)
    (looking-at ansys-non-code-line-regexp)))

(defun ansys-at-end-of-text-p ()
  "Return t if the cusor is at the end of text in a line."
  (if (looking-at "\\s-*$") t nil))

(defun ansys-at-end-of-code-p ()
  "Return t if the cursor is at the end of code in a line.
This means at the end of code before whitespace or an ANSYS
comment."
  (if (looking-at "\\s-*$\\|\\s-*!") t nil))

;; ANSYS will support in the future only GNU-Linux 64 and Windows 64
;; for the entire ANSYS platform, some support of legacy Unices (AIX
;; IBM, HP-UX HP, SGI, Solaris SUN) for standalone apps will be
;; provided so I don't restrict anys-mode to GNU-Linux
(defun ansys-is-unix-system-p ()
  "Return t when we are on a unix system.
gnu/linux, aix, berkeley-unix, hpux, irix, lynxos 3.0.1,
usg-unix-v."
  (not
   (or (string= system-type "gnu")	;gnu with the hurd kernel
       (string= system-type "darwin")	;mac
       (string= system-type "ms-dos")
       (string= system-type "windows-nt")
       (string= system-type "cygwin"))))

;;FIXME DEFSUBSTs with DEFUNs (ansys-position) inside aren't
;;particularly speedy, are they?

(defsubst ansys-in-comment-p ()
  "Return t if the cursor is inside an ANSYS comment.
The cursor is either in a code comment or comment line."
  (save-excursion
    (nth 4 (parse-partial-sexp (ansys-position 'bol) (point))))) ;nth -- nth element of list

(defsubst ansys-in-comment-line-p ()
  "Return t if the cursor is in a comment line."
  (save-excursion
    (back-to-indentation)
    (looking-at "!")))

(defsubst ansys-in-string-p () ;FIXME:are there strings defined in ansys?
  "Return t if the cursor is inside an ANSYS string."
  (save-excursion
    (nth 3 (parse-partial-sexp (ansys-position 'bol) (point)))))

(defsubst ansys-in-empty-line-p()
  "Return t if the cursor is in an empty (whitespace) line."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \n\t]*$")))

(defsubst ansys-not-in-string-or-comment-p()
  "Return t if the cursor is not inside a string or comment."
  (let ((pps (parse-partial-sexp (ansys-position 'bol) (point))))
    (not (or (nth 3 pps) (nth 4 pps)))))

(defsubst ansys-in-string-or-comment-p ()
  "Return t if the cursor is not inside a string or comment."
  (let ((pps (parse-partial-sexp (ansys-position 'bol) (point))))
    (or (nth 3 pps) (nth 4 pps))))

;; ======================================================================
;; --- functions ---

(defun ansys-align (p-min p-max)
  "Align current paragraph or selection of ANSYS variable definitions.
If a region is selected align it (with the region borders P-MIN
and P-MAX) otherwise align the current code paragraph."
  (interactive "r")
  (if mark-active
      (align p-min p-max)
    (align-current)))			;align-current needs a mark

;; (defvar solver-font-lock nil)

;; (defun ansys-solver-mode ()
;;   "Helper mode for the fontification of the solver output"
;;   (interactive)
;;   (kill-all-local-variables)
;;   (setq major-mode 'ansys-solver-mode)
;;   (setq mode-name "ANSYS-Solver")		; mode line string
;;   (setq solver-font-lock ;'("BEGIN:\\|PREP7:\\|SOLU_LS[0-9]+:\\|POST1:\\|POST26:\\|RUNSTAT:\\|AUX2:\\|AUX3:\\|AUX12:\\|AUX15:"
;; ; 1 font-lock-warning-face) )
;;  '("BEGIN:" "PREP7:" "SOLU_LS[0-9]+:" "POST1:" "POST26:" "RUNSTAT:" "AUX2:" "AUX3:" "AUX12:" "AUX15:")
;; 			      )
;;   (setq font-lock-defaults `(,solver-font-lock))
;;   )

;;;###autoload
(defun ansys-mode ()
  "Support for working with the ANSYS FEA suite.
The documentation is targeted at users with little Emacs
experience.  Sections dealing with features are indicated with
two asterisks (**) at the beginning.  Your input as a keyboard
sequence is indicated in quotation marks (\"), the actual keys
are quoted with <>.

== Contents ==

= Introduction to Emacs
= Usage of ANSYS-Mode
= Mode Keybindings
= Customisation
= Bugs and Problems

== Introduction to Emacs ==

In Emacs it is not only possible to run a certain command, let's
say `ansys-start-ansys-help', from entries in Emacs' ANSYS menu
bar or with keyboard shortcuts (here:
\"\\[ansys-start-ansys-help]\") but additionally from the so
called minibuffer.  This 'interactive' option remains the only
one if you have not yet activated ANSYS-Mode or you are currently
inspecting a file which is not intended for this mode.  Then
neither the ANSYS menu nor keyboard shortcuts for ANSYS-Mode
commands are available.

To run `ansys-start-ansys-help' by its function name, start with
\"M-x\", ('M-x' means holding down the <ALT> key while pressing
the <x> key (in case your window manager is intercepting this key
combination type <ESC> then <x> instead) the cursor will skip
below the status line, into the minibuffer, there type
\"ansys-start-ansys-help\", then terminate it with the <RET> key.
The 'auto-completion' feature of the minibuffer might save you
some typing: Just enter the first characters and then press the
<TAB> key.

You can always cancel minibuffer commands by typing
\"C-g\" (`keyboard-quit'), i. e. pressing the <CTRL> key and then
the <g> key at the same time.

All functions described in this help, regardless whether
possessing a keyboard shortcut or not, can be called in the
interactive way or they are to be found in the ANSYS menu.  (If
you prefer to run Emacs in a terminal you might access the menu
with <F10> key or \"ESC-`'\".)

Above described procedure has the same effect as typing
\"\\[ansys-start-ansys-help]\" in a file buffer under ANSYS
mode ('C-c C-h' means while holding down the <CTRL> key typing
the respective characters ('c' then 'h') for
`ansys-start-ansys-help').

A mouse click or typing the <RET> key, when the cursor is on the
underlined hyperlinks (you can also skip to these links with the
<TAB> key) will display their respective help strings (or typing
<RET> when the cursor is over these links).

In case something unintended happend to your code you are always
able to resort to the Emacs `undo' functionality from the menu or
with typing \"\\[undo]\".

== Usage of ANSYS-Mode ==

** ANSYS command syntax help **

Typing \"\\[ansys-show-command-parameters]\", the <CTRL> key
simultaneously with the <c> key and then <?>, the question
mark (for the command `ansys-show-command-parameters') displays
above a code line a brief description of the ANSYS command and
its syntax.  This command counts also the number of parameters
and visualises at which parameter position the cursor currently
is.  The command is looking for the next valid command near the
cursor or when using a prefix argument (`C-u' or `4') it inquires
an ANSYS command from you. The tooltip is switched off with an
argument of zero (`0').

** Browse the detailed ANSYS command (and element) html help **

Putting in \"\\[ansys-browse-ansys-help]\" will display the
original ANSYS help in your web browser for an APDL command or
element name (stating the element number is sufficient) near the
cursor or the code line's first APDL command.  When using a
prefix argument it inquires an keyword from you.  Keywords
beginning with a backslash `\"' describing general manual
sections, for example typing `\"SHELLS\"' will call the
collection of all shell elements in the ANSYS manual.

** ANSYS keyword completion (commands, elements, get- and
   parametric-functions) **

Type the first letter or letters of an ANSYS command, function or
element name and use the key binding
\"\\[ansys-complete-symbol]\" to let the function
`ansys-complete-symbol' do the (case sensitve) completion for
you.  Depending on the case of your letter or letters to be
completed, you will get a downcased, upcased or capitalised
completion.

There are around 2000 ANSYS symbols available for completion.
Undocumented ANSYS commands and deprecated element types are also
completed.  The former are identified as such with a different
highlighting and in their 'command syntax help'.  Please see also
the variable `ansys-deprecated-element-alist' it's a list with
the deprecated elements and their respective replacements (for
inspecting its content please click on above hyperlink or type
\"C-h v\" and then type above variable name).

Doing this using the <TAB> key might save you some typing in
auto-completing the name.  When the characters before the cursor
are not unambiguous a completion list is shown, select the
suitable word from the list, either by navigation the cursor over
the symbol and typing <RET> or clicking with the left (or middle)
mouse button is completing the symbol.  Hitting the <SPACE> key
removes the listing window.

** Alignment (formatting) of variable definitions **

Typing \"\\[ansys-align]\" to call the function `ansys-align'
will align marked sections or a paragraph of variable definitions
like the following lines

 xyz=30.381      !this is a variable
 x = 0.4!this is another variable

they will be aligned to

 xyz = 30.381 !this is a variable
 x   =  0.4   !this is another variable

** Auto-indentation of looping and conditional blocks **

You can customise the indentation depth (ANSYS Block Offset),
please have a look for the entry 'Customise ANSYS Mode' in the
ANSYS-Mode menu.  The Emacs customisation facility optionally
saves your choices automatically in your .emacs file for later
sessions.

** Closing of open control blocks (*do, *if, ...) with the
   insertion of appropriate end keywords **

Typing \"\\[ansys-close-block]\" for the function
`ansys-close-block' completes the current ANSYS block with the
insertion of a newline and an appropriate end keyword.

** Code navigation with extended keyboard shortcuts: Code lines,
   number blocks, and *DO,*IF, DOWHILE, *CREATE blocks etc. **

\"\\[ansys-next-code-line]\" -- `ansys-next-code-line' and
\"\\[ansys-previous-code-line]\" -- `ansys-previous-code-line'
are going to the next/previous code line, skipping intermediate
comments and empty lines.

The following block navigation commands are analogous to Emacs'
inbuilt list/sexp navigiation.

\"\\[ansys-next-block-end]\" -- `ansys-next-block-end'
\"\\[ansys-previous-block-start-and-conditional]\" --
`ansys-previous-block-start-and-conditional'
Above commands are skipping to the next/previous block end/start
keyword regardless where you are already in the block structure.
\"\\[ansys-previous-block-start-and-conditional]\" for the function
`ansys-previous-block-start-and-conditional' finds also *IF
commands without bases of the keyword 'THEN'; furthermore *CYCLE
and *EXIT looping controls.  These provide APDL constructs but
represent no block depth and therefore are not considered when
applying the following navigation commands.

\"\\[ansys-skip-block-forward]\" -- `ansys-skip-block-forward'
\"\\[ansys-skip-block-backwards]\" -- `ansys-skip-block-backwards'

Are looking for and skipping over a complete block (at the
current block level, skipping possibly over deeper block
structures).

\"\\[ansys-up-block]\" -- `ansys-up-block'
\"\\[ansys-down-block]\" -- `ansys-down-block'

Are searching for and skipping up/down a block structure from the
current block level.

\"\\[ansys-number-block-start]\" -- `ansys-number-block-start'
\"\\[ansys-number-block-end]\" -- `ansys-number-block-end'

Are searching for and skipping over 'pure' number blocks (the
nblock, eblocks and cmblocks), these are common (and often quite
large) in WorkBench solver input files (*.inp, *.dat).

You can also hide and unhide these - usually uninteresting -
blocks with `ansys-hide-number-blocks' and
`ansys-show-number-blocks' respectively or even a region of
you (un)liking with `ansys-hide-region'.  In files with the
suffix `.dat' number blocks are hidden by default.

Moreover there are keyboard shortcuts with which you are able to
input pairs of corresponding characters, like \"C-c %\" for '%%',
the ANSYS substitution operators.  The advantage is that the
cursor is place between the pair and you might give a numerical
argument to the call and enclose already existing words with the
pair, e. q. \"C-1 C-c %\".  Please have a look for `insert-pair'
and see below in the keybindings section.

** Sophisticated highlighting (optionally: User variables) **

The highlighting in the highest decoration level (please refer to
`ansys-highlighting-level') tries to follow the idiosyncratic
ANSYS solver/interpreter logic as closely as possible.  For
example: '* ', an asterisk with following whitespace(s), is still
a valid ANSYS comment operator (although deprecated, see the
ANSYS manual for the *LET command).

The fontification distinguishes between ANSYS commands,
undocumented commands, parametric- and get-functions, elements
and deprecated elements.  In case of arbitrary characters after
the command names, they are still highlighted, since these
characters are ignored by the ANSYS intepreter.

Macro variables beginning with an underscore might be ANSYS
reserved variables and therefore are higlighted in a warning
face.  Another example is the ANSYS the percent sign, its
highlighting reminds you that the use of such a pair around a
parameter name might force a parameter substitution, e. g. with
the assignment 'I=5' and '/com,TEST%I%', the /com command outputs
TEST5.

In the context of pairs of '%' characters, you can also input
various pairs with keyboard shortcuts, e. g. apostrophies for
ANSYS character parameters with \"C-c '\", please have a look
which bindings are available with \"\\[describe-bindings]\" (for
`describe-bindings').

The format strings of *MSG, *MWRITE, *VWRITE and *VREAD are also
highlighted (in decoration levels 2, again please refer to
`ansys-highlighting-level').  Below is a summary of the C-format
descriptors which can be used for above commands.  (with these
format descriptors there are no parentheses needed in contrast to
less general fortran ones):

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

example code:
*vwrite,B(1,1),B(2,1)
%E%/%E

Regarding the highlighting of user variables: The idea is to give
a visual hint whether variable names are spelled and used
correctly everywhere not only at the place of its definition.

For this to occur `ansys-highlighting-level' must be set to
2 (the maximum, which is also the default), please have a look at
the == customisation == section on how to change settings.

Newly edited variable definitions are taken into account only
when the variable `ansys-dynamic-highlighting-flag' is set (for
very large files this might slow Emacs and therefore the flag is
only effective for files ending in '.mac') or every times you
activating the variable display (with
\"\\[ansys-display-variables]\", see below) in the maximum
highlighting level (2).

** Compilation of all APDL variables definition and component
  names (*GET, *DIM, *SET, = and DO, ...) **

Typing \"\\[ansys-display-variables]\" (for `ansys-display-variables')
shows all definitions in your APDL file in a separate window.

You might remove '*ANSYS-variables*' window with \"\\[ansys-delete-other-window]\" (`ansys-delete-other-window').

When you place the cursor on the respective line number and type
\"C-u M-g g\", where 'C-u' is a 'prefix' argument to 'M-g
g' (`goto-line')).  Emacs will then skip to the corresponding
definition line in the macro file.

Additionally you are able with a prefix argument for
\"\\[ansys-display-variables]\" to receive the current value of
your variables at the cursor position.  For this functionality it
is necessary to have a running ANSYS process (GNU-Linux systems
only, please see below the chapter about the solver
communication).

** Use of the Emacs abbreviation facility for block templates **

E.g. typing \"`do\" (the backquote '`' then 'do') and the space
key <SPC> under ANSYS-Mode triggers an interactive code template
which inserts a *DO loop (`ansys-do').  \"`d\" (then <SPC>) is a
more immediate version of it without requesting user
input (`ansys_do').  You can see all the predefined abbreviations
with \"`?\", i. e. a question mark '?'  after the backquote '`'.
Alternatively you might use the menu entry or the command \"M-x
list-abbrevs <RET>\" to inspect all definitions which Emacs
knows.

** Outlining (hiding and expanding) code sections **

If you are using the pre-configured ANSYS-Mode then
`outline-minor-mode' is switched on by default.

With this mode you can hide certain sections of your code or
navigate to customisable outline headings.  Certain characters
--by default '!@' (see the variable `ansys_outline_string')-- at
the beginning of a line in your code represent such headings.
'!@@' specifies a subheading and so on (please call the function
`ansys-skeleton-outline-template' to insert a skeleton of outline
sections in your current file).  Check out the Outline menu
entries.

In case outlining is not activate you might call Outline Minor
Mode with \"M-x outline-minor-mode\" or you can enable this mode
for the current session by ticking on the respective option in
the menu or permanently by setting `ansys-outline-minor-mode' for
the `ansys-mode-hook' variable.  Please type \"M-x
ansys-customise-ansys <RET>\" or use the customisaton system from
the menu: ->ANSYS ->Customise ANSYS Mode.

** Convenient comment handling, commenting/un- of whole
   paragraphs **

- \"\\[comment-dwim]\" calls `comment-dwim' (Do What I Mean ;-):

In a code line: This command inserts comment char
`ansys-indent-comment-string' at `ansys-code-comment-column' (if
feasible, i. e. the code line is not too long).  With a prefix
argument: Kill existing code comment.

With an highlighted region: Commenting out (`comment-region') or
Uncommenting (`uncomment-region') that region.

In an empty line: Inserts '!! ' with the right indentation.

- \"\\[indent-new-comment-line]\" (or \"M-j\", calls
  `indent-new-comment-line').

Breaks a code comment and inserts a single exclamation mark
'!' (`ansys-comment-char') at column
`ansys-code-comment-column' (if possible).

In comment lines '!! ' with two comment
characters (`ansys-indent-comment-string') breaks the comment and
begins a the same comment style at the the current indentation.

In an empty line or a line without comment: Just inserts a new
line.

** Insertion of code template in an APDL file **

You are able to preview the code templates with
\"\\[ansys-display-skeleton]\" (for `ansys-display-skeleton'),
while doing this, you might type <TAB> to complete all available
skeleton names.

Check e. g. `ansys-skeleton-outline-template', type \"M-x
ansys-skeleton-outline-template <RET>\" to insert this skeleton
of APDL code with outline headings.  Alternatively you can use
the binding \"C-u C-c C-s\" for inserting templates (instead of
just previewing them).

** Auto-insertion of code templates into new APDL files **

Put the following section in your .emacs file, in case you want
to to include above skeleton (optionally) for every APDL file.

      (auto-insert-mode 1)
      (add-hook 'find-file-hook 'auto-insert)
      (setq auto-insert-query t)
      (add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton-outline-template]))

Please refere the configuration example `default.el'.

** ANSYS process management **

- ANSYS-Mode writes for you an ANSYS stop file in the current
  directory (the file name is compiled from the variable
  `job-name' and the extension '.abt'). You can do this with
  \"\\[ansys-write-abort-file]\" (`ansys-write-abort-file', you
  might previously use the Emacs command 'cd' (\"M-x cd\") to
  change the current directory).  This stop file is halting a
  running calculation in an orderly, re-startable fashion.

- You are able to view the ANSYS error file (a file consisting of
  the `job-name' and the suffix '.err' in the current directory)
  with \"\\[ansys-display-error-file]\" (this calls
  `ansys-display-error-file').  The error file is opened in read
  only mode (see `toggle-read-only') and with the minor mode
  `auto-revert-tail-mode', which scrolls the buffer automatically
  for keeping the current ANSYS output visible.

- You can start the ANSYS help browser directly from Emacs with
  \"\\[ansys-start-ansys-help]\" (for `ansys-start-ansys-help').

- You might also start the APDL product launcher from Emacs under
  windows or the APDL interpeter under GNU-Linux with \"\\[ansys-start-ansys]\" (for
  `ansys-start-ansys').

- For displaying the available licenses (in another Emacs window)
  please use \"\\[ansys-license-status]\" (for
  `ansys-license-status').

If you haven't installed ANSYS in the default locations and the
executables are not in your system search path or you are using a
different ANSYS version than '161' it is necessary for the last
two capabilities to customise some variables either calling the
Emacs customisation facility `ansys-customise-ansys' or from the
menu bar -> 'ANSYS' -> 'Customise ANSYS Mode' -> 'ANSYS-process'
and look there for the variables 'ANSYS License File', 'ANSYS
Util Program' and 'ANSYS Help Program' as well as 'ANSYS Help
Program Parameters') or set the variables directly in your .emacs
file.  Please have a look in the accompanying README and
default_el customisation file example.

** ANSYS solver/interpreter control and communication (mainly
  restricted to GNU-Linux systems) **

With the ANSYS-Mode keyboard shortcut
\"\\[ansys-start-ansys]\" (for the command `ansys-start-ansys')
you can start the APDL solver/interpreter under GNU-Linux as an
asynchronous process from Emacs.  After starting the run you will
see all interpreter output in a separate Emacs 'comint' (command
interpreter) window.  You are now able to interact with this
process in three ways, either by typing directly in the '*ANSYS*'
window or using \"\\[ansys-send-to-ansys]\" (for
`ansys-send-to-ansys').  With the latter you can send either the
current code line or a whole selected region to the running
solver.  (A selected region means highlighted lines of code.  If
there is no running solver the function copies the code to the
system clipboard.)  And lastly you are able to send interactively
ANSYS commands with
\"\\[ansys-query-ansys-command]\" (`ansys-query-ansys-command')
without switching to the '*ANSYS*' window. If you would like to
send your current code line in a slightly modified form, then
give a prefix argument to `ansys-query-ansys-command' and the
line will be the intial input for sending it to the interpreter.

Another very useful function in this context is
\"\\[ansys-copy-or-send-above]\" (`ansys-copy-or-send-above'),
which sends all code from the beginning up to the current line to
the solver/interpreter.  If there is no running interpreter the
function copies the code to the system clipboard.

The last two commands (`ansys-copy-or-send-above' and
`ansys-send-to-ansys') are skipping to the next code line (if
possible).  If you don't need this behaviour supply any prefix
argument to them and the cursor will remain in the current line
or in the last line of the previously highlighted region.

When you are not familiar with Emacs' keybindings you probably
want to select your part of interest with dragging the mouse
pointer while pressing the first mouse button.  Often it is
faster to select regions with specialised keyboard commands.  For
example \"\\[ansys-mark-block]\" (`ansys-mark-block') marks a
whole block level, \"\\[mark-paragraph] (`mark-paragraph') marks
the current paragraph, the last command can not only be used to
initialise a new selection but also to extend an existing one
when repeting the command.  Please check the code navigation
commands which ANSYS-Mode provides (type
\"\\[describe-bindings]\" (`describe-bindings') to see which are
available)

In this mode you are able to start an ANSYS graphics
screen (without the rest of graphical user interface) with
\\[ansys-start-graphics] (function `ansys-start-graphics').  Thus
you are able to check and debug your macro file content visually.
The graphics in this state is changeable with APDL commands (like
/view,1,1,1,1) but unfortunately not through mouse interactions!
If you want to turn, zoom, etc. the model it is best to call
`ansys-start-pzr-box' with \\[ansys-start-pzr-box] and a dialog
box will pop up.  This is the usual ANSYS Pan/Zoom/Rotate dialog
for the graphics screen.  But beware: Before you are able to send
further commands to the solver, you first have to close the PZR
dialog box.  There is also a family of interactive commands to
reposition the graphics, like
\\[ansys-zoom-in] (`ansys-zoom-in'), replotting works with
\\[ansys-replot] (`ansys-replot') and a fit to the screen with
\\[ansys-fit] (`ansys-fit'), of course, they are available from
the menu as well.

There is also a command for saving the data and ending the solver
run: `ansys-exit-ansys' and a command for an emergency kill in
case the solver is not stoppable any longer in an orderly way:
`ansys-kill-ansys'.

As already indicated ANSYS-Mode has its own command for invoking
the ANSYS help browser \"\\[ansys-start-ansys-help]\" because
unfortunately the following APDL commands do not work when the
complete GUI system of ANSYS is not active.

    /ui,help  !is it not working in ANSYS non-GUI modes
    help, COMMAND !is also not working in ANSYS non-GUI modes

So you are not able start the help browser for a *specific* ANSYS
command but must search within the ANSYS help browser or better
use \"\\[ansys-browse-ansys-help]\".

== Keybindings ==

\\{ansys-mode-map}

== ANSYS-Mode customisation ==

For a compilation (and respective documentation) of available
ANSYS-Mode customisations it's best to open the mode's
customisation buffer either with the command
`ansys-customise-ansys' or from the menu bar -> 'ANSYS' ->
'Customise ANSYS Mode' and check interesting options.

Another way getting to the customisation facility is to open the
specific documentation of respective variables.  Let's change for
example the highlighting level which is stored in the
customisation variable `ansys-highlighting-level'.  Click on the
hyperlink and you will be presented with its help buffer in which
you should click on the underlined word 'customize' at the
bottom.  Then you have the convenient customisation
functionalities for this particular variable at hand.  You can
set the value for the current session or add your choices
automatically in a .emacs file (the configuration file in your
home directory) for futur sessions as well.

Alternatively you might include the following Elisp code snippet

     (setq ansys-highlighting-level 2)
     (setq ansys-dynamic-highlighting-flag t)

directly into your .emacs file.  (Semicolon `;' is the comment
character.  Please check the configuration example `default.el'.)

For certain options to take effect without restarting Emacs, it's
necessary to reload ANSYS-Mode.  You can do this with the
interactive command `ansys-reload-ansys-mode' or with the
respective, toplevel ANSYS menu entry.

You can improve the loading and execution speed of ANSYS-Mode
with a byte-compilation of its lisp files (if they are not
already compiled, i. e. they have the suffix '.elc', please read
the section 'Byte Compilation' in the Emacs lisp reference, which
is availabe from the help menu).

== Bugs and Problems ==

Feedback is always welcome.  If you have issues while
installing/running this mode or simply would like to suggest some
improvements you have the following options:

- Write an email to the mode maintainer, please trigger a bug
  report form from the ANSYS menu or by calling the function
  `ansys-submit-bug-report' with \"\\[ansys-submit-bug-report]\".
  Even if you are not able to send emails directly via Emacs,
  this is, at least, a mail template with possibly valuable
  information like mode settings and Emacs internals.

- You might also issue a bug report at GitHub's ANSYS-Mode site
  https://github.com/dieter-wilhelm/ansys-mode/issues, where you
  will also find the latest versions of ANSYS-Mode.

- Or you can leave comments and hints at the ANSYS-Mode page of
  Emacs Wiki http://www.emacswiki.org/cgi-bin/wiki/AnsysMode.

====================== End of ANSYS-Mode help ===================="
  (interactive)

  (unless (string= major-mode "ansys-mode")
    (set (make-local-variable 'ansys-previous-major-mode) major-mode))
  (put 'ansys-previous-major-mode 'permanent-local t)

  (when (and (overlayp ansys-help-overlay)
	     (overlay-buffer ansys-help-overlay))
		    (delete-overlay ansys-help-overlay))

  (kill-all-local-variables)		; convention
  (setq major-mode 'ansys-mode)
  (setq mode-name "ANSYS")		; mode line string

  ;; only effective for window systems!
  (setq indicate-empty-lines ansys-indicate-empty-lines-flag)

  (setq completion-ignore-case t) ; keyword completion regardless of cases

  (use-local-map ansys-mode-map)
  (set-syntax-table ansys-mode-syntax-table)
  (setq local-abbrev-table ansys-mode-abbrev-table)

  (setq font-lock-maximum-decoration `((ansys-mode . ,ansys-highlighting-level) (t . t)))

  (setq align-mode-rules-list ansys-align-rules-list)
  ;; (when (> ansys-highlighting-level 1)
  ;;   (setq font-lock-multiline t)) ;for *msg, *vwrite,.. format strings

  (make-local-variable 'ansys-user-variable-regexp) ;for font-lock
  (setq ansys-user-variable-regexp nil)

  (make-local-variable 'parens-require-spaces)
  (setq parens-require-spaces ansys-require-spaces-flag)

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

  ;; FIXME:
  ;;  (setq comment-fill-column 50)???
  ;;  comment-indent -> fill-column?? only when line-wrap mode t?

  ;; overlay for command-parameter-help

  (make-local-variable 'ansys-timer)
  (make-local-variable 'ansys-help-overlay)
  (setq ansys-help-overlay (make-overlay 1 1))

  ;; look at newcomment.el
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ansys-comment-start-skip)

  ;;  (make-local-variable 'parse-sexp-ignore-comments)
  ;;  (setq parse-sexp-ignore-comments t)

  ;;  (make-local-variable 'font-lock-defaults) is always local
  (setq font-lock-defaults `(,ansys-font-lock-keyword-list nil 'case-ignore))
  ;; keywords
  ;; keywords-only -- nil: syntactic fontification
  ;; case-fold -- non nil: ignore case

  (make-local-variable 'outline-regexp)
  (make-local-variable 'ansys-hide-region-overlays)
;  outline searches only at the line beginning
  (setq outline-regexp (concat "!\\(" ansys-outline-string "\\)+"))

  ;; discrepancies from Emacs defaults
  (ansys-font-lock-mode)	;switch on font-lock when it's toggled
;  (delete-selection-mode t)
  (toggle-truncate-lines 1)
  (show-paren-mode t)
  (set (make-local-variable 'scroll-preserve-screen-position) nil)
  (defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
    (interactive (if mark-active
		     (list (region-beginning) (region-end))
		   (message "Copied line")
		     (list (line-beginning-position)
			   (line-beginning-position 2)))))

  (setq ansys-is-unix-system-flag (ansys-is-unix-system-p))

  ;; menu
  (ansys-add-ansys-menu)
  ;; avoid a possible error when calling align-current without a mark
  (set-mark (point))
  ; (deactivate-mark)
  ;; --- user variables ---
  (if (>= ansys-highlighting-level 2)
      (when (or
	     (unless buffer-file-name
	       t) ;skip rest is a buffer without a file
	     (> 1000000 (nth 7 (file-attributes (buffer-file-name))))
	     (y-or-n-p
	      "File is larger than 1MB, switch on user variable highlighting? "))
	(if (and buffer-file-name ;we have a file in the buffer
		 ansys-dynamic-highlighting-flag
		 (string= (file-name-extension (buffer-file-name) 'dot) ".mac"))
	    (progn (add-hook 'after-change-functions
			     'ansys-find-user-variables nil t)
		   (add-hook 'post-command-hook
			     'ansys-update-parameter-help nil t)
		   (message "Dynamic highlighting of user variables activated."))
	  (message "Non-dynamic highlighting of variables activated."))
	(ansys-find-user-variables)))

  ;; .dat WorkBench solver input files

  (when (and buffer-file-name ; a buffer with a file name
	 (string= (file-name-extension (buffer-file-name) t) ".dat"))
    (ansys-hide-number-blocks))
    ;; (when (y-or-n-p "Would you like to hide all blocks? This may take some time...")
    ;; 	    (hs-hide-all)))

  ;; --- hooks ---
  (run-hooks 'ansys-mode-hook)

  ;; ;;;;;;;;;; -- end of ansys-mode -- ;;;;;;;;;;;;;;;;;;;;
  )

(defun ansys-mark-paragraph (&optional arg allow-extend)
  "Put mark at beginning of this paragraph, point at end.
The paragraph marked is the one that contains point or follows
point.

With argument ARG, puts point at end of a following paragraph, so
that the number of paragraphs marked equals ARG.

If ARG is negative, point is put at the beginning of this
paragraph, mark is put at the end of this or a previous
paragraph.

Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG paragraphs after the ones already marked.

Arg ALLOW-EXTEND is in interactive calls the same as ARG."
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero paragraphs"))
  (cond ((and allow-extend		;we already called this function
	      (or (and (eq last-command this-command) (mark t))
		  (and transient-mark-mode mark-active)))
	 (forward-paragraph arg))
	((and (bolp) (eolp))		;we are in an empty line
	 (push-mark nil t t)
	 (forward-paragraph arg))
	(t				;we are within a paragraph
	 (backward-paragraph arg)
	 (push-mark nil t t)
	 (forward-paragraph arg))))

(defun ansys-mode-version ()
  "Display the ANSYS-Mode version numbering scheme."
  (interactive)
  (message "ANSYS-Mode version: %s.%s (based on ANSYS %s)" ansys_version
	   ansys_mode_version
	   ansys_version))

(defun ansys-reload-ansys-mode ()
  "Reload the ANSYS mayor mode.
Clear the mode definitions if active, load the necessary code and
call `ansys-mode'."
  (interactive)
  (progn
    (when (featurep 'ansys-mode)
      (unload-feature 'ansys-mode))
    (load "ansys-mode")
    (ansys-mode)
    (message "ANSYS-Mode reloaded.")))

(defun ansys-show-paren-mode ()		;_C
  "Switch on minor mode function `show-paren-mode'.
The Matching parenthesis is highlighted."
  (show-paren-mode 1))

(defun ansys-ruler-mode ()		;_C
  "Switch on minor mode function `ruler-mode'.
Display a ruler in the header line."
  (ruler-mode 1))

(defun ansys-font-lock-mode ()
  "Switch on function `font-lock-mode'.
Font Lock is also known as \"syntax highlighting\"."
  (unless font-lock-mode
    (font-lock-mode 1)))

(defun ansys-outline-minor-mode ()
  "Switch on mode function `outline-minor-mode'.
Editing with selective display."
  (outline-minor-mode 1))

(defun ansys-auto-insert-mode ()
  "Switch on mode function `auto-insert-mode'.
Automatic template insertion is enabled"
  (auto-insert-mode 1))

(defun ansys-insert-pi ()
  "Insert a variable assignment of Pi at point.
Together with a newline character and indentation of the assigment."
  (interactive)
  (insert "Pi=3.14159265359")
  (indent-according-to-mode)
  (newline-and-indent))

(defun ansys-column-ruler ()
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

(defun ansys-position (position) ;FIXME: with `line-beginning-position' etc.
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

(defun ansys-close-block ()		;FIXME: choices for *IF
  "Complete an ANSYS block command with the appropriate end keyword.
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
			  ;; RESTRICTED: asssoc-string Emacs 21.4
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
(defsubst ansys-count-commas ()
  "Return the number of commas in the line before point."
  (how-many "," (line-beginning-position) (point)))

(defun ansys-manage-overlay ( str)
  "Display or remove the command help overlay string STR.
Appying this function in the same line erases the help overlay.
The help overlay will be automatically removed after some time
interval.  The timer is sleeping, unless the buffer is the
current one."
  (interactive)
  (let ((ho (overlay-start ansys-help-overlay))
	(lb (line-beginning-position))
	s)
    (if ansys-timer
	(cancel-timer ansys-timer))
    (delete-overlay ansys-help-overlay)
      (setq ansys-help-overlay-str str)
      (move-overlay ansys-help-overlay lb lb)
      (overlay-put ansys-help-overlay 'before-string str)
      (setq ansys-timer (run-at-time "2 min" nil
        '(lambda () (when (and (overlayp ansys-help-overlay)
			       (overlay-buffer ansys-help-overlay))
		    (delete-overlay ansys-help-overlay)))))))

(defun ansys-search-comma (str count)
  "Return the index of the COUNT's occurance of a comma in STR.
Return nil otherwise."
  (let ((index 0)
	(c 0))
    (while (and (> count c) (not (null index)))
      (setq index (string-match "," str (1+ index))
	    c (1+ c)))
    index))

(defun ansys-update-parameter-help (&optional a b c)
  (let ((p (point))
	(lo (overlays-in (line-beginning-position) (1- (line-beginning-position)))))
    (when (and (not (equal p ansys-parameter-help-position))
	     (not (equal 1 p))		;TODO not working in the first line
	     (memq ansys-help-overlay lo))
    (setq ansys-parameter-help-position (point))
    (ansys-show-command-parameters 1))))

(defun ansys-show-command-parameters (&optional ask-or-toggle)
  "Display an ANSYS command parameters help for the command near the cursor.
Show the command name and its parameters (if any) and in a
further line a brief description.  Count the number of parameters
and visualise at which parameter position the cursor currently
is.  This is done for the previous ANSYS command beginning,
except when point is at the command beginning at the indentation.
See also the function `ansys-command-start' how the previous
command is found.  It displays also the parameters for commands
in a comment line.  With a prefix argument ASK-OR-TOGGLE of zero
switch off the command parameters highlighting, with an prefix
`C-u' or argument `4' (four) enquire a command name from the mini
buffer, which might be completed with <TAB>."
  (interactive "p" )
  (let ((case-fold-search t)		;in case customised to nil
	(count 0)
	substr
	tmpstr
	start
	end
	length
	str)
    ;; enquire or search for a valid command name
    (cond ((= ask-or-toggle 0))		;do nothing
	  ((= ask-or-toggle 4)
	   (setq str (completing-read
		      "Type function or command name for help: "
		      ansys-help-index))
	   ;;	(message "keyword %s" str)
	   )
	  ((ansys-in-comment-line-p)
	   (save-excursion
	     (back-to-indentation)
	     (skip-chars-forward " !")
	     (re-search-forward "[^[:space:]]\\w*\\>" nil t))
	   (setq str (match-string-no-properties 0)))
	  ((ansys-in-indentation-p) ; we are before a possible command
	   (save-excursion
	     (re-search-forward "[^[:space:]]\\w*\\>" nil t)
	     (setq str (match-string-no-properties 0)))
	   )
	  ((unless (ansys-in-indentation-p)
	     (save-excursion
	       (ansys-command-start)
	       (re-search-forward "[^[:space:]]\\w*\\>" nil t)
	       (setq str (match-string-no-properties 0))))))
    ;; search, amend and display help string in overlay
    (if (= ask-or-toggle 0)
	(delete-overlay ansys-help-overlay)
      (catch 'foo
	(dolist (s ansys-dynamic-prompt)
	  (when (string-match (concat "^" str) s)
	    (setq length (length s))
	    ;; creating additional row with comma counts
	    (setq start (string-match "\n" s)) ;looking for the first line break
	    (setq substr (substring s (1+ start)))
	    (setq tmpstr (mapconcat
			  (lambda (str)
			    (cond ((string-match "," (string str))
				   (setq count (1+ count))
				   (if (> count 9)
				       (setq count 0))
				   (format "%d" count))
				  (t "-")))
			  substr ""))
	    (when (> count 0)
		(setq s (concat s "\n" tmpstr)))
	    (setq s (propertize (concat s "\n") 'face 'highlight))
	    ;; show different face for current argument
	    (setq count (ansys-count-commas))
	    (setq start (ansys-search-comma s count))
	    (cond ((null start)
;		   (skip-chars-backward "[^,]")
		   (error "Too many commas, command has fewer arguments"))
		  ((= 0 start)
		   (setq start  (1+ (string-match "\n" s)))))
	    (setq end (ansys-search-comma s (1+ count)))
	    (unless end
		 (setq end length))
	    (add-text-properties start end '(face isearch-fail) s)
	    (ansys-manage-overlay s)
	    (throw 'foo nil)))
	(delete-overlay ansys-help-overlay)
	(error "\"%s\" not found in keyword list" str)))))

(defun ansys-check-capitalisation ( string)
"Check case of ANSYS keyword STRING.
Return symbols capitalise, upcase and downcase."
(interactive)
;; preferences: downcase, capitalize, upcase
(cond
 ((string= string (downcase string)) 'downcase)
 ((string= string (capitalize string)) 'capitalize)
 ((string= string (upcase string)) 'upcase)
 (t 'downcase)))

(defun ansys-complete-symbol ()
  "Perform a completion on ANSYS keywords preceding the cursor.
Complete the character(s) to ANSYS's reserved words, functions
and element names, otherwise throw an error.  When the keyword or
the completed character(s) represent a unique ANSYS keyword
indicate this fact with a message.  When the completion is not
unique or only partial show the other possible completions in a
temporary completion buffer, from which the completions might be
chosen with the mouse.  You might remove the *ANSYS completion*
buffer with the SPACE key."
  ;; This code taken from lisp-complete-symbol
  (interactive "*")
  (let* ((buffer-name "*ANSYS-completion*")
	(completion-buffer (get-buffer-create buffer-name))
	(completion-window (get-buffer-window completion-buffer))
	)
    (if (and (eq last-command this-command)
	     completion-window		;already window there?
	     (window-live-p completion-window)	;window is visible
;	     (window-buffer window)
	     ;; (buffer-name (window-buffer window))
	     )
	;; If this command was repeated, and
	;; there's a fresh completion window with a live buffer,
	;; and this command is repeated, scroll that window.
	(with-current-buffer (window-buffer completion-window)
	  (if (pos-visible-in-window-p (point-max) completion-window)
	      (set-window-start completion-window (point-min))
	    (save-selected-window
	      (select-window completion-window)
	      (scroll-up)))))
      ;; Do completion.
      (let* ((end (progn (skip-chars-backward " \t") (point)))
	     (beg (save-excursion (skip-chars-backward "()")
				  (backward-sexp 1) (point)))
	     (completion-string (buffer-substring-no-properties beg end))
	     (completion (try-completion
			  completion-string ansys-completions))
	     (completion-list (all-completions
			       completion-string ansys-completions))
	     (cc (ansys-check-capitalisation completion-string)))

	(cond
	 ;; completion not possible
	 ((null completion)
	  (message "\"%s\" can't be completed to an ANSYS symbol"
		   completion-string)
	  (if completion-window	;bury completion buffer
	      (save-selected-window
		(select-window completion-window)
		(bury-buffer)))
	  (ding))

	 ;; unique and upcased like in the -completions variable
	 ((equal completion t)
	  (message "\"%s\" is a unique ANSYS symbol."
		   completion-string)
	  (kill-buffer completion-buffer))

	 ;; unique or uniquely completable, case independent
	 ((= (length completion-list) 1) ;uniqe
	  (setq completion (funcall cc completion))
	  (unless (string= completion completion-string)
	    (setq completion (funcall cc completion))
	    (delete-region beg end)
	    (insert completion))
	  ;; possibly move back into parens
	  (skip-chars-backward ")" (1- (point)))
	  (kill-buffer completion-buffer)
	  (message "\"%s\" is a unique ANSYS symbol." completion))

	 ;;complete or not, but not unique anyway
	 (t
	  (setq completion (funcall cc completion))
	  (unless (string= completion completion-string)
	    (delete-region beg end)
	    (insert completion))
	  (with-output-to-temp-buffer buffer-name
	    (display-completion-list completion-list))
	  (if (= (apply 'min (mapcar 'length completion-list))
		 (length completion))
	      ;; already a complete, valid symbol but fragment is further
	      ;; completable
	      (message
;	       (concat "Complete ANSYS symbol.  Hit SPACE to remove the "
	       (concat "Complete but not unique ANSYS symbol.  Hit SPACE to remove the "
		       buffer-name " buffer."))
	    ;; not yet complete
	    (message
	     (concat "Incomplete ANSYS symbol.  Hit SPACE to remove the "
		     buffer-name " buffer.")))

	  ;; mouse selections in the completion buffer?
	  (let (key
		first)
	    (if (progn
		  (set-buffer (get-buffer completion-buffer))
		  ;; we are temporarily in the completion buffer
		  (setq key (read-key-sequence nil)
			first (aref key 0)) ;first key of key sequence
		  (and (consp first)	    ;is cons cell
		       (consp (event-start first))
		       (eq
			(window-buffer (posn-window (event-start first)))
			(get-buffer completion-buffer))
		;;        (eq (key-binding key) 'choose-completion)))
		;; (choose-completion first)
		       (eq (key-binding key) 'mouse-choose-completion))); <E23.2
		(mouse-choose-completion first)
	      (if (eq first ?\ )
		  (kill-buffer completion-buffer)
		(setq unread-command-events
		      (listify-key-sequence key))))))))))

;;;; Electric characters & friends

(defun ansys-reindent-then-newline-and-indent () ; (&ptional non-matching) ;FIXME: docu
  "Reindent current ANSYS line, insert newline, and indent the new line.
If function `abbrev-mode' is on, expand the abbreviations first."
  (interactive "*") 			;* means signal error if read-only
  (expand-abbrev)
  (ansys-blink-matching-block)
  (save-excursion
    (delete-region
     (point)
     (progn (skip-chars-backward " \t")
	    (point)))			;move trailing whitespace
    (indent-according-to-mode))
  (insert "\n")
  (indent-according-to-mode))

(defun ansys-electric-space ()
  "Insert a space in ANSYS-Mode.
Maybe expand abbrevs and blink matching block open keywords.
Reindent the line if `ansys-auto-indent-flag' is non-nil."
  (interactive "*")			;error if read only
  (setq last-command-event ? )
  (cond ((and mark-active transient-mark-mode delete-selection-mode)
	 (kill-region (point) (mark))
	 (self-insert-command 1))
	((and (ansys-not-in-string-or-comment-p)
	      (not (ansys-in-indentation-p))
	      (not (ansys-in-empty-line-p)))
	 (indent-according-to-mode)
	 (self-insert-command 1)
	 (expand-abbrev)
	 (ansys-blink-matching-block)
	 (if (and ansys-auto-indent-flag
		  (save-excursion
		    (skip-syntax-backward " ")
		    (not (bolp))))
	    (indent-according-to-mode)))
	(t
	 (self-insert-command 1))))

(defun ansys-add-ansys-menu ()
  "Add an \"ANSYS\" entry to the Emacs menu bar."
  (require 'easymenu)
  (easy-menu-define ansys-mode-menu-map ansys-mode-map
    "Menu keymap for ANSYS-Mode." ansys-mode-menu)
  (easy-menu-add ansys-mode-menu-map ansys-mode-map))

(defun ansys-calculate-indent ()   ;FIXME: comment, fixed goal column,
  "Return appropriate indentation for current line as ANSYS code.
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
	 ((looking-at "[^\n,]") ; */ are also valid default commands 12.1
	  (setq lep (line-end-position))
	  (setq comma_c (re-search-forward "\\w+\\s-*" lep 'noerror))
	  (when comma_c
	    (setq lbp (line-beginning-position))
	    (setq comma_c (- comma_c lbp))))
	 ((looking-at ",")		;TODO: shouldn't be possible
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
	 ((and (looking-at ",")	   ;ANSYS default command substitution
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
  "Indent current line in ANSYS coding style.
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
  "Start entering an ANSYS abbreviation.
If Abbrev mode is turned on, typing ` (grave accent) followed by ? or
\\[help-command] lists all ANSYS abbrevs.  Any other key combination is
executed normally.
Note that all ANSYS-Mode abbrevs start with a grave accent."
  (interactive)
					;  (if (not abbrev-mode)			;FIXME: redundant with E22.?
					;      (self-insert-command 1)
  (let (c)
    (insert last-command-event)
    (if (or (eq (setq c (read-event)) ??)
	    (eq c help-char))
	(list-abbrevs t)
      (setq unread-command-events (list c))))) ;)


;; redefine function because of bug in Emacs 23.2 squashed in 23.3
(defun prepare-abbrev-list-buffer (&optional local)
  "Temporary redefinition of internal Emacs function with the argument LOCAL."
  (let ((l-a-t-n  (abbrev-table-name local-abbrev-table)))
   (with-current-buffer (get-buffer-create "*Abbrevs*")
    (erase-buffer)
    (if local
        (insert-abbrev-table-description l-a-t-n t)
      (dolist (table abbrev-table-name-list)
        (insert-abbrev-table-description table t)))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (edit-abbrevs-mode)
    (current-buffer))))

(defun ansys-indent-format-line ()
  "Break ANSYS line at point, continuing comment if within one.
If within code, insert the ANSYS continuation character `&'
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; --- Cursor movement ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ansys-default-command-end ()
  "Move cursor to the end of an ANSYS default command construct."
  (unless (ansys-default-command-p)
    (re-search-forward "^\\s-*,"))
  (while (ansys-default-command-p)
    (forward-line))
  (forward-line -1)
  (end-of-line))

(defun ansys-search-number-line(&optional dir)
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

(defun ansys-number-block-start()
  "Move to the line beginning before a pure number block.
For example an ANSYS NBLOCK or EBLOCK typically found in
WorkBench APDL files.  If there is no code before a number block,
signal an error."
  (interactive)
  (when (or (ansys-in-indentation-p)
	    (not (ansys-number-line-p)))
    (ansys-search-number-line -1))
  (while (ansys-number-line-p)
    (forward-line -1))
  (forward-line))

(defun ansys-number-block-end()
  "Move to the end of a pure number block.
For example an ANSYS NBLOCK or EBLOCK typically found in
WorkBench APDL files."
  (interactive)
  (when (or (ansys-at-end-of-text-p)
	    (not (ansys-number-line-p)))
    (ansys-search-number-line))
  (while (ansys-number-line-p)
    (forward-line))
  (forward-line -1)
  (end-of-line))

(defun ansys-next-code-line (&optional num)
  "Move NUM lines of ANSYS code forward, default for NUM is 1.
Skip past intermediate comment and empty lines."
  (interactive "p")
  (unless num (setq num 1))
  (unless (memq last-command '(next-line
			       previous-line
			       ansys-next-code-line
			       ansys-previous-code-line))
    (setq temporary-goal-column (current-column)))
  (cond ((eobp)
	 (message "End of buffer"))
	(t
	 (forward-line 1)
	 (while (and (ansys-not-in-code-line-p)
		     (not (ansys-last-line-p)))
	     	 (forward-line 1))
	 ;(forward-comment (buffer-size))
	 ;; temporary-goal-column might be a cons cell since E23.2
	 (move-to-column  (if (integerp temporary-goal-column)
			      (truncate temporary-goal-column)
			    (truncate (car temporary-goal-column))))
	 (setq num (1- num))
	 (when (and (not (ansys-last-line-p))
		    (/= num 0))
	   (ansys-next-code-line num))
	 )))

(defun ansys-previous-code-line (&optional num)
  "Move NUM lines of ANSYS code backward, default for NUM is 1.
Skip before all empty - and comment lines and return the
difference between NUM and actually moved code lines."
  (interactive "p")
  (unless num (setq num 1))
  (unless (memq last-command '(next-line
			       previous-line
			       ansys-next-code-line
			       ansys-previous-code-line))
    (setq temporary-goal-column (current-column)))
  (let ((p 0))
    (forward-line -1)
    (while (and (ansys-not-in-code-line-p)
		(not (ansys-first-line-p)))
      (forward-line -1))
    ;; (unless (ansys-first-line-p)	;in case we aren't at b-o-l
    ;;   (beginning-of-line)		;for forward-comment
    ;;   (forward-comment (-(buffer-size))) ;and in case we are in a comment line
       ; starting with Emacs 23.1 t-g-c might be a cons cell
    (move-to-column   (if (integerp temporary-goal-column)
			  (truncate temporary-goal-column)
			(truncate (car temporary-goal-column))))
    (setq num (1- num)
	  p num)
    (when (and (ansys-number-line-p)
	       (/= num 0))
      (setq p (ansys-previous-code-line num)))
    p))

(defun ansys-back-to-format-command ()
  "Move cursor back to the beginning of a previous format command.
Signal an error when there is no format command."
  (interactive)
  (when (re-search-backward ansys-format-commands-regexp) ;signals error
    (back-to-indentation)))

(defun ansys-move-to-end-of-format-string ()
  "Move cursor to the end of an format command's format string."
  (when (ansys-in-format-command-line-p)
    (forward-line))
  (while (and (ansys-continuation-line-p)
	      (= (forward-line 1) 0))) ;in case of wrong format at eof
  (move-end-of-line 1))

(defun ansys-move-before-comment()
  "Move cursor to the line's end of text (which is not commented out)."
  (beginning-of-line)
  (search-forward "!" (ansys-position 'eol) 1)
  (skip-chars-backward " \t!"))

(defun ansys-command-start (&optional num)
  "Move cursor to the beginning of the NUMth previous command or assignment.
Default for NUM is 1.  If in a comment or empty line, go to the
previous command or to the first line if no previous command is
there.  When on a condensed input line, go to previous `$'
statement or to the line's first command.  When in a format
command string move backward to the beginning of the respective
command.  When no ANSYS command is to be found signal an error.
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

(defun ansys-command-end (&optional num)
  "Move to the end of the NUMth next ANSYS command or assignment statement.
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
  "Scan from (point) COUNT balanced ANSYS begin-end blocks.
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

(defun ansys-mark-block ()     ;FIXME: this is not consistent with
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
  "Move backward across one balanced ANSYS begin-end block.
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
  "Move forward down one begin-end block level of ANSYS code.
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
  "Move backwards up one begin-end block level of ANSYS code.
Position cursor before the beginning keyword of the respective
block.  With argument DEPTH, do this for that many levels.  A
negative argument DEPTH means move forward down DEPTH levels (see
`ansys-down-block')."
  (interactive "p")
  (unless depth (setq depth 1))
  (ansys-down-block (- depth)))

(defun ansys-blink-matching-block ()
  "Blink the matching ANSYS begin block keyword.
If point is right after an ANSYS else or end type block keyword,
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hiding regions heavily borrowed from M. Dahls hide-region.el

(defun ansys-hide-region ()
  "Hide a region by making an invisible overlay over it.
Put some markers (`ansys-hide-region-before-string',
`ansys-hide-region-after-string') around and save the overlay in
the `ansys-hide-region-overlays' \"overlay ring\"."
  (interactive)
  (let ((new-overlay (make-overlay (mark) (point))))
    (push new-overlay ansys-hide-region-overlays)
    (overlay-put new-overlay 'invisible t)
    (overlay-put new-overlay 'intangible t)
    (overlay-put new-overlay 'before-string
                 (if ansys-hide-region-propertize-markers
                     (propertize ansys-hide-region-before-string
                                 'font-lock-face 'region)
                   ansys-hide-region-before-string))
    (overlay-put new-overlay 'after-string
                 (if ansys-hide-region-propertize-markers
                     (propertize ansys-hide-region-after-string
                                 'font-lock-face 'region)
                   ansys-hide-region-after-string))
    )
  )

(defun ansys-hide-number-blocks ()
  "Hide all number blocks (nblock, eblocks, cmblocks) in file.
These constructs appear in WorkBench created solver input files."
  (interactive)
  (let (
	(p-orig (point))
	p1
	p2
	lines
	)
    (message "Hiding number blocks ...")
    (goto-char (point-min))
    (while (re-search-forward "nblock\\|eblock\\|cmblock" nil t)
      (setq p1 (point))
      (re-search-forward "^-1\\|^cmsel\\|^d" nil nil)
      (setq p2 (point)
	    lines (count-lines p1 p2)
	    )
      (when (> lines 5) ;only hide blocks if larger then 5 lines
	(goto-char p1)
	(forward-line 3) ; show one line of numbers before markers
	(set-mark (point))
	(goto-char p2)
	(forward-line -2) ; show one line of numbers after markers
	(end-of-line)
	(ansys-hide-region)
	)
      )
    (goto-char p-orig)
    )
  )

(defun ansys-unhide-number-blocks ()
  "Unhide all hidden regions in the current buffer."
  (interactive)
  (while ansys-hide-region-overlays
    (if (car ansys-hide-region-overlays)
        (progn
          (delete-overlay (car ansys-hide-region-overlays))
          (setq ansys-hide-region-overlays (cdr ansys-hide-region-overlays))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- Abbreviations ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless ansys-mode-abbrev-table
  (let ((ac abbrevs-changed)) ;inhibit offer to save .abbrev_defs
    (define-abbrev-table 'ansys-mode-abbrev-table ())
    (define-abbrev ansys-mode-abbrev-table "`1" "finish\n/clear\n!y\n"  ) ;the first 1 one
    (define-abbrev ansys-mode-abbrev-table "`i" ""      'ansys_if)        ;for if
    (define-abbrev ansys-mode-abbrev-table "`d" ""      'ansys_do)        ;for do
    (define-abbrev ansys-mode-abbrev-table "`p" "*dim,Dir,string,248 ! maximum of 248 characters!\nDir(1) = '/HOME/uidg1626/development/report/ej/95ks91leg0/'\n/syp,ls,Dir(1)\n") ; for path
;    (define-abbrev ansys-mode-abbrev-table "`p" "" 'ansys-insert-pi)
    (define-abbrev ansys-mode-abbrev-table "`if" "" 'ansys-if)
    (define-abbrev ansys-mode-abbrev-table "`ie" "" 'ansys-if-then)       ;for IfThen
    (define-abbrev ansys-mode-abbrev-table "`do" "" 'ansys-do)
    (define-abbrev ansys-mode-abbrev-table "`e" "/eof ----------------------------------------\n"
      '(lambda () (indent-according-to-mode)))
    (define-abbrev ansys-mode-abbrev-table "`c" "!! ========================================\n"
      '(lambda () (indent-according-to-mode)))
    (define-abbrev ansys-mode-abbrev-table "`t" "/title,"
      '(lambda () (indent-according-to-mode)))
    (setq abbrevs-changed ac))) ;reset `abbrevs-changed' to previous state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- Bug reporting ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ansys-submit-bug-report ()	;from Octave
  "Open an Emacs mail buffer with an ANSYS-Mode bug report."
  (interactive)
  (require 'reporter)
  (let (salutation
	(reporter-prompt-for-summary-p t)) ;asks for a summary which
					;goes in the subject line
    (when (y-or-n-p "Do you want to write a bug report? ")
      (setq salutation
	    "Please describe briefly what your problem is and which actions
  triggered the bug.  A self contained, reproducible test case
  would be advantageous.")
      (reporter-submit-bug-report
       ansys-maintainer-address
       "ANSYS-Mode"		  ;becomes prefix for the subject line
       (list
	;; constants
	'ansys_version
	'ansys_mode_version
	;; defcustoms
	'ansys-hide-region-before-string
	'ansys-hide-region-after-string
	'ansys-hide-region-propertize-markers
	'ansys-highlighting-level
	'ansys-current-ansys-version
	'ansys-dynamic-highlighting-flag
	'ansys-indicate-empty-lines-flag
	'ansys-blink-region-flag
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
	'ansys-align-rules-list
	'ansys-install-directory
	'ansys-job
	'ansys-program
	'ansys-help-program
	'ansys-help-path
	'ansys-help-program-parameters
	'ansys-lmutil-program
	'ansys-license-file
	'ansys-ansysli-servers
	'ansys-license-types
	'ansys-license
	'ansys-no-of-processors
	)
       nil
       nil
       salutation))))

(load "ansys-template")
(load "ansys-process")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- dynamic highlighting ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---- Restrictions ----
;; Variables or 'parameters' in ANSYS parlance:
;; 1.) Begin with a letter
;; 2.) Contain only letters, numbers and the underscore '_'
;; 3.) Have no more than 32 characters
;; 4.) Any variable ending with an underscore are *not* shown with the *STATUS command
;; 5.) The maximum number of parameter (< 5000) is retrieved by *GET,par,PARM,,MAX
;; 6.) (A<B) returns the value of A when A is less than B, B otherwise!

(defun ansys-asterisk-regexp(string)
  (when (= (elt string 0) ?*)
    (setq string (concat "\\" string)))
  string)

(defun ansys-string-length-predicate (s1 s2)
  "Return t when string S1 is larger then S2."
  (< (length s1) (length s2)))

(defun ansys-find-duplicate-p (entry list)
  "Return t when ENTRY is already a member of LIST."
  (let ((l list) p)
    (while (and (not p) l)
      (setq p (assoc-string entry (car l) 'ignore-case))
      (pop l))
    p))

;;with pseudo arguments a b c in case of usage as after-change-function
(defun ansys-find-user-variables (&optional a b c)
  "Find all user variables in the current buffer.
Pre-process the findings into the variables `ansys-user-variables'
and `ansys-user-variable-regexp' for subsequent fontifications.
Added pseudo arguments A B C."
  ;; RESTRICTED: line-number-at-pos was introduced after Emacs 21.4
  (interactive)
  (save-excursion
    (save-match-data
      (let (res var com)	; Start with ANSYS *USE vars
	(setq ansys-user-variables ())

	(dolist (command ansys-variable-defining-commands)
	  (setq com (car command))
	  (goto-char (point-min))

	  (while (re-search-forward
		  ;; take care of variables clashing with command names
		  (concat "\\(?:^\\|$\\)\\s-*" com
 "\\s-*,\\s-*\\([[:alpha:]][[:alnum:]_]\\{0,31\\}\\)") nil t)
	    (setq var (match-string-no-properties 1))
	  ;; format line, comment, message, C***
	    (unless (or (ansys-in-string-or-comment-p)
			(ansys-in-string-command-line-p)
			(ansys-in-format-construct-p)
			(ansys-find-duplicate-p var ansys-user-variables))
	      (add-to-list 'ansys-user-variables
;			   (match-beginning 1)
			   (list var (line-number-at-pos))))))

 	;; ANSYS = assignment
	(goto-char (point-min))
 	(while (re-search-forward
		;; search for reserved variables as well
		"\\_<\\([[:alpha:]_][[:alnum:]_]\\{0,31\\}\\)\\s-*="
		nil t)
 	  (setq var (match-string-no-properties 1))
 	  (unless
 	      (or (ansys-in-string-or-comment-p)
 		  (ansys-in-string-command-line-p)
 		  (ansys-in-format-construct-p)
 		  (ansys-find-duplicate-p var ansys-user-variables)
		  )
 	    (add-to-list 'ansys-user-variables
 			 (list var (line-number-at-pos)))))
	;; we must sort the variables according to their occurance
	;; for the display
	(setq ansys-user-variables
	      (sort ansys-user-variables
		    '(lambda (arg1 arg2)
		       (< (cadr arg1) (cadr arg2)))))
	;; make the regexp for fontification
	(setq res (mapcar 'car ansys-user-variables)
	      res (regexp-opt res 'symbols) ;words inhibits variables ending in _!
	      ansys-user-variable-regexp res)))))

;; in comments: ok
;; in * comments: ansys-in-asterisk-comment-p
;; clashes with command names
;; in format strings without % chars
(defun ansys-search-variable (variable limit)
  "Search for the variable VARIABLE up to limit LIMIT.
This function is used as a highlighting function."
  (save-excursion
    (while (progn
	     (re-search-forward variable limit t)
	     (or (ansys-in-asterisk-comment-p)
		 (and (or (ansys-in-format-construct-p)
			  (ansys-in-string-command-line-p)
			  (not (looking-at "%")))))))))

(defun ansys-highlight-variable (limit)
  "Find user variables from (point) to position LIMIT for highlighting.
Use variable `ansys-user-variable-regexp'."
  (let ((r ansys-user-variable-regexp))
    (re-search-forward r limit t)))

(defun ansys-higlight-procent-and-ampersand (limit)
  "Find procent and ampersand up to position LIMIT for highlighting."
  (let (res )
    (while
	(progn
	  (setq res (re-search-forward "%\\|&\\s-*$" limit t))
	  ;; don't highlight in comments
	  (and res (ansys-in-comment-p))))
    res))

(defun ansys-copy-buffer-line (buffer line-no)
  "Return from buffer BUFFER the line with LINE-NO as a string."
  (save-excursion
    (let (bol eol)
      (set-buffer buffer)
      (save-excursion
	(goto-char (point-min))
	(forward-line (- line-no 1))
	(back-to-indentation)
	(setq bol (point))
	(end-of-line)
	(setq eol (point))
	(buffer-substring bol eol)))))

(defun ansys-display-variables ( arg)
  "Displays APDL variable assignments in the current buffer.
Together with the corresponding line number N (type \\[goto-line]
N for skipping to line N or place the cursor over the number and
C-u \\[goto-line] takes the number automatically).  With a prefix
argument ARG, the function evaluates the variable at point."
  (interactive "P")
  (cond
   (arg
    (unless (ansys-process-running-p)
      (error "No ANSYS process is running"))
    (let* (
	   (pt (point))
	   (re "\s_[[:word:]]*")
	   (lbp (line-beginning-position))
	   (str (buffer-substring-no-properties
		 (save-excursion (+ pt (skip-chars-backward re lbp)))
		 (save-excursion (+ pt (skip-chars-forward re))))))
      (comint-send-string (get-process ansys-process-name)
			  (concat "*status," str "\n")
		      )
      (display-buffer "*ANSYS*" 'other-window)
      (message  (concat "Enquiring status for variable: " str))))
   (t
     (ansys-find-user-variables)
     (let* ((current-buffer (buffer-name))
	    (buffer-name "*ANSYS-variables*")
	    (variable-buffer (get-buffer-create buffer-name))
	    str old-num com
	    (num 0))
       (set-buffer variable-buffer)
       (if (version<  "24" emacs-version)
	   (when (fboundp 'read-only-mode)
	     (read-only-mode -1))
	 (toggle-read-only -1))
       (kill-region (point-min) (point-max))
       ;; insert header
       (insert
	(propertize
	 (concat "-*- APDL variables of buffer " current-buffer " -*-\n")
	 'face 'match))
       (insert (propertize "Line  | Definition\n" 'mouse-face
			   'highlight 'face 'bold))
       ;; insert variable lines
       (dolist (command ansys-user-variables)
	 (setq old-num num
	       num (cadr command)	;cadr same as nth 1
	       com (ansys-copy-buffer-line current-buffer num)
	       str (concat
		    (propertize (format "%5d | " num)
				'mouse-face 'highlight 'face 'bold)
		    com "\n"))
	 (unless (= num old-num)
	   (insert str)))
       (goto-char (point-min))
       (if (version< "24" emacs-version)
	   (when (fboundp 'read-only-mode)
	       (read-only-mode 1))
	 (toggle-read-only 1))
       (set-buffer current-buffer)
       (display-buffer buffer-name 'other-window)))))

(defun ansys-customise-ansys ()
  "Call the Emacs customisation facility for ANSYS-Mode."
  (interactive)
  (customize-group "ANSYS"))

(defun ansys-delete-other-window (&optional win)
  "Delete the other, not selected Emacs window.
A window is in Emacs parlance a \"field\" where data is displayed
and possibly others in a window manager frame.  The command
deletes only the window and not the buffer, i. e. not the data
itself.  A frame can have many windows (and Emacs can also
control multiple frames, by the way), often the Emacs beginners
confuse the term window with an Emacs frame.  Optional prefix
argument WIN is the WIN'th different window in the current frame.
The default argument is 1."
  (interactive "p")
  (unless win (setq win 1))
  (let ((swin (selected-window)))
    (other-window win)
    (delete-window)
    (select-window swin)))

(provide 'ansys-mode) ; this makes more sense when the file name is identical
					;to the feature name, what are subfeatures anyway?

;; Local Variables:
;; mode: outline-minor
;; indicate-empty-lines: t
;; show-trailing-whitespace: t
;; word-wrap: t
;; End:

;;; ansys-mode.el ends here
