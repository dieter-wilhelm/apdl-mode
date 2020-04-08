;;; apdl-mode.el --- Major mode for the scripting language APDL -*- lexical-binding: t -*-
;; Time-stamp: <2020-04-08>

;; Copyright (C) 2006 - 2020  H. Dieter Wilhelm GPL V3

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Version: 20.5.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages, convenience, tools, Ansys, APDL
;; URL: https://github.com/dieter-wilhelm/apdl-mode

;; Maintainer: H. Dieter Wilhelm
;; Created: 2006-02

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

;; Editor support for working with APDL code and Ansys FEA.

;; The APDL-Mode package provides support for the FEA (Finite Element
;; Analysis) program Ansys (https://www.ansys.com) under Windows and
;; GNU-Linux systems.  It defines 'APDL-Mode', a major mode for
;; viewing, writing and navigating in APDL (Ansys Parametric Design
;; Language) files as well as providing managing and communication
;; capabilities for an associated Ansys solver process.

;; The mode's capabilities are sophisticated but the documentation is
;; targeted for Ansys users with little Emacs experience.  An online
;; version of its documention can be found at
;; https://dieter-wilhelm.github.io/apdl-mode/.

;;  The code is available on
;;  https://github.com/dieter-wilhelm/apdl-mode/.  Regarding
;;  installation and further information please consult the
;;  accompanying README.org file.

;;; History:

;; Please consult the accompanying NEWS.org file.

;;; Code:

(require 'apdl-keyword)
(require 'apdl-initialise)
(require 'apdl-process)
(require 'apdl-template)
(require 'apdl-wb-template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- constants ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst apdl-parameter-substitution-commands-regexp
  "/TITLE\\|/STITLE\\|/COM\\|/AXLAB\\|/GCOLUMN\\|/TLABEL\\|/AN3D"
  "Regexp of command names which have a string behind them.")

(defconst apdl-string-commands-regexp
  "C\\*\\*\\*\\|/TITLE\\|/STITLE\\|/COM\\|/AXLAB\\|\
   /GCOLUMN\\|/TLABEL\\|\\*ABBR\\|/AN3D"
  "Regexp of command names which have a string behind them.")

(defconst apdl-variable-defining-commands ; association list
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
    ("\\*set.?"."*SET") ; Ansys inconsistency *SET works only with one
                                        ; additional character
    ("\\*top\\*w"."*TOPER")
    ("\\*vge\\w*"."*VGET")    ; Not true in 14.0 variable must be
                                        ; dimensiond with *dim
    ("\\*vfu\\w*"."*VFUN")
    ("\\*vit\\w*"."*VITRP")
    ("\\*vop\\w*"."*VOPER")
    ("\\*vsc\\w*"."*VSCFUN")
    ("\\*vfi\\w*"."*vfill"))
  "Alist for commands which define user variables.
In the form of (regexp . command_string), intentionally excluded
is the \"=\" assignment command.")

(defconst apdl-use-variables
  '("ARG[1-9]" "AR[1][0-9]")
  "Variable containing the APDL *USE variables regexp.
ARG[1-9] and AR[1][0-9] are macro local variables and can be
passed to the *USE command.  Additionally AR[2-9][0-9] are pure
macro local variables.")

(defconst apdl-format-commands-regexp
  "\\*[mM][sS][gG]\\|\\*[vV][rR][eE]\\|\\*[vV][wW][rR]\\|\\*[mM][wW][rR]"
  "Regexp of command names which have one or more format lines.")

(defconst apdl-maintainer-address
  "Dieter Wilhelm <dieter@duenenhof-wilhelm.de>" ; bug-gnu-emacs@gnu.org
  "Address of current maintainer of the APDL-Mode.")

(defconst apdl-comment-char ?!
  "The APDL comment character.")

;; \sCODE,code
;; -:whitespace
;; <:commentstart

;; whitespace +
;; $
;; comment
;; digits

;; "^\\s-*\\($\\|\\s<\\|[+[:digit:]-]\\)"


(defconst apdl-non-code-line-regexp "^\\s-*\\($\\|\\s<\\|[+[:digit:]-]\\)"
  "Regexp indicating a comment -, number - or an empty line.
A comment line contrasting a \"code comment\" which follows code
to be analysed from the Ansys solver/interpreter.  A \"number
line\" is a line beginning with a number e. g. from an element
block or with a `+' or `-' sign.")

(defconst apdl-condensed-input-line-regexp ".*\\$"
  "Regexp indicating a condensed input line.")

(defconst apdl-comment-start-skip "\\S<+\\S-*"
  "Regexp to match the start of an APDL comment up to its body.
Used for the variable `comment-start-skip'.")

;; --- defcustoms ---

(require 'custom)

(defgroup APDL nil
  "Customisation group for the APDL-Mode."
  :version "20.5.0"
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :link '(url-link :tag "Online documentation"
                   "https://dieter-wilhelm.github.io/apdl-mode ")
  :link '(url-link :tag "Code on GitHub"
                   "https://github.com/dieter-wilhelm/apdl-mode")
  :group 'Languages)

(defcustom apdl-parameter-help-duration "2 min"
  "Duration for showing the `apdl-show-command-parameters' overlay.
The value is a string expressing a relative time span like \"2
hours 35 minutes\" or a number of seconds from now (the
acceptable time formats are those recognised by the function
`timer-duration'."
  :type '(string number)
  :group 'APDL)

(defcustom apdl-hide-region-before-string "![ ... hidden"
  "String to mark the beginning of an invisible region.
This string is not really placed in the text, it is just shown in an
overlay"
  :type '(string)
  :group 'hide-region)

(defcustom apdl-hide-region-after-string " region ... ]"
  "String to mark the beginning of an invisible region.
This string is not really placed in the text, it is just shown in an overlay"
  :type '(string)
  :group 'hide-region)

(defcustom apdl-hide-region-propertize-markers t
  "If non-nil, add text properties (colour) to the region markers."
  :type 'boolean
  :group 'hide-region)

(defcustom apdl-highlighting-level 2
  "This variable sets the level of highlighting.
There are three levels available, 0 a minimalistic level
optimised for speed and working with very large files (like
solver input files from WorkBench), 1 and 2.  Level 0 highlights
only the minimum (unambiguous) length of APDL command names and
variable definitions with the '=' operator.  Level 1 highlights
complete command names, together with functions, elements,
deprecated elements, undocumented commands, strings from string
commands and the APDL operators.  Level 2 is the same as 1,
except that all defined user variables and unambiguous command
names (also interpreter-ignored characters behind them) are
highlighted as well.  The user variables are highlighted
\"statically\" only, newly defined variables are only taken into
account after `apdl-display-variables'
\(\\[apdl-display-variables]) is called, this updating is done
dynamically i. e. during editing when the variable
`apdl-dynamic-highlighting-flag' is set to t."
  :type 'integer
  :group 'APDL
  :link '(variable-link font-lock-maximum-decoration ))

(defcustom apdl-dynamic-highlighting-flag t
  "Non-nil means that APDL-Mode highlights user defined variables.
Warning: This option is computational expensive and --depending
on the file size and your hardware --it might make your editing
experience somewhat sluggish.  Currently dynamic highlighting of
user variables is only implemented for files with the extensions
either \".mac\" or \".ans\" and in the highest highlighting
level (please see the variable `apdl-highlighting-level')
otherwise the fontification of variables is only static.  To take
effect after setting this variable you have to restart
`apdl-mode'."
  :type 'boolean
  :group 'APDL)

(defcustom apdl-indicate-empty-lines-flag nil
  "Non-nil means indicate empty lines on window systems.
Do this visually at the end of an APDL buffer in the left
fringe.  You have to reload function `apdl-mode' for this
variable to take effect."
  :type 'boolean
  :group 'APDL)

(defcustom apdl-comment-padding " "
  "Padding string that `comment-dwim' puts between comment chars and text.
Extra spacing between the comment character(s) and the comment
text makes the comment easier to read.  This padding is not
effective for code comments (comments behind code)."
  :type 'string
  :group 'APDL)

(defcustom apdl-comment-add 1
  "How many additional comment characters are inserted by \\[comment-dwim].
This determines the default value of the numeric argument of
`comment-dwim'.  It should generally stay 0, except for a few
modes like Lisp where it can be convenient to set it to 1 so that
regions are commented with two semi-colons."
  :type 'integer
  :group 'APDL)

(defcustom apdl-code-comment-column 25
  "Column where APDL code comments (behind code) are placed."
  :type 'integer
  :group 'APDL)

(defcustom apdl-auto-indent-flag t
  "Non-nil means indent line when typing the SPC key.
The space character is also inserted."
  :type 'boolean
  ;;  :options '(t nil) ; not necessary with booleans in Customise
  :group 'APDL)

(defcustom apdl-indent-comment-suffix ""
  "String placed after the APDL comment char in an code comment.
See `apdl-indent-comment-string'."
  :type 'string
  :group 'APDL)

(defcustom apdl-ruler-wide-flag nil
  "Non-nil means show a 80 characters wide temporary ruler.
Nil means show a narrower temporary ruler with 50 characters."
  :type 'boolean
  :group 'APDL)

(defcustom apdl-require-spaces-flag nil
  "Non-nil means \\[insert-parentheses] inserts whitespace before ().
When there is a region marked then function `insert-parentheses'
inserts the parentheses around the active region."
  :type 'boolean
  :group 'APDL)

(defcustom apdl-blink-matching-block-flag t
  "Non-nil means blinking of matching APDL block keywords.
Skip temporary to the matching beginning of the block when
inserting a newline after an *ELSE or *END keyword."
  :type 'boolean
  :group 'APDL)

(defcustom apdl-blink-matching-delay .7
  "Time in seconds for skipping to a matching block.
See also the variable `apdl-blink-matching-block-flag'."
  :type 'number
  :group 'APDL)

(defcustom apdl-block-offset 2
  "Indentation column(s) for statements in a block structure."
  :type 'integer
  ;; :options only for types hook, plist and alist
  :group 'APDL)

(defcustom apdl-outline-string "@"
  "String specifying outline headings (see `outline-regexp')."
  :type 'string
  :group 'APDL)

(defcustom apdl-mode-hook nil
  "Normal hook run before entering APDL-Mode.
A hook is a variable which holds a collection of functions."
  :type 'hook
  :options '(apdl-show-paren-mode apdl-outline-minor-mode
                                  apdl-ruler-mode apdl-auto-insert-mode)
  :group 'APDL)

(require 'align)

(defcustom apdl-align-rules-list
  '(
    (apdl-align-=
     (regexp   . "\\(\\s-*\\)=")
     (modes    . '(apdl-mode))
     (justify  . t)
     (tab-stop . nil))

    (apdl-align-text-column
     (regexp   . "=\\(\\s-*[0-9]+\\|\\s-*\\)")
     (modes    . '(apdl-mode))
     (justify . t)
     (tab-stop . nil))

    (apdl-align-comment
     (regexp   . "[0-9.)]+\\(\\s-*\\)\\!")
     (modes    . '(apdl-mode))
     (tab-stop . nil)))
  "Rules for aligning APDL variable definitions."
  :type align-rules-list-type
  :group 'apdl-mode)

;; (put 'my-align-rules-list 'risky-local-variable t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- variables ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar apdl-overlay-str ""
  "Variable to store previous overlay string.")

(defvar apdl-hide-region-overlays nil
  "Variable to store the regions we put an overlay on.")

(defvar apdl-help-overlay nil
  "Overlay for displaying the ansys parameter help.")

(defvar apdl-timer nil
  "Timer variable to set up a timer for overlay clearing.
Please have a look at the function `apdl-manage-overlay'.")

(defvar apdl-indent-comment-string
  (concat (char-to-string apdl-comment-char) apdl-indent-comment-suffix)
  "String to insert when creating an APDL code comment.")

(defvar apdl-user-variables nil
  "Variable containing the user variables and line No of first occurance.
The list is used for the display of these
 variables (`apdl-display-variables').")

(defvar apdl-user-variable-regexp nil
  "Variable containing the user variables regexp.
The regexp is used for the
fontification (`apdl-highlight-variable') of these variables.")

(defvar apdl-is-unix-system-flag nil
  "Non-nil means the computer runs a Unix system.
Any of GNU-Linux, aix, berkeley-unix, hpux, irix, lynxos 3.0.1 or
usg-unix-v.")

(defvar apdl-previous-major-mode ""
  "The buffer's previous major mode (before APDL-Mode).")

(defvar apdl-mode-abbrev-table nil
  "Abbreviation definition table for the APDL-Mode.
All APDL abbrevs start with a grave accent \"`\".  \"`?\" lists
the currently defined abbreviations.")

(defvar apdl-parameter-help-position) ; for the compiler

(defvar-local apdl-parameter-help-position 1
  "Cursor position in -show-command-parameters.")

;;; --- constants ---

(defconst apdl-continuation-line-regexp ".*?&\\s-*$"
  "Regexp indicating a continuation line (of the *MSG command).")

(defconst apdl-else-keywords
  '("\\*[eE][lL][sS][eE][iI][fF]" "\\*[eE][lL][sS][eE]"
    "\\*[cC][yY][cC][lL][eE]")
  "Regexps describing APDL block else keywords.")

(defconst apdl-end-keywords
  '("\\*[eE][nN][dD][dD][oO]" "\\*[eE][nN][dD][iI][fF]"
    "\\*[eE][nN][dD]")
  "Regexps describing APDL end keywords.")

(defconst apdl-number-line-regexp
  "^\\s-*[(+-]?[[:digit:]]"
  "Regexp describing an APDL number line.
Used for skipping pure number lines and CMBLOCK format strings")

(defconst apdl-block-else-regexp
  (concat "\\("
          (mapconcat #'identity apdl-else-keywords "\\|")
          "\\)\\>")
  "Regexp containing the APDL else keywords.")

(defconst apdl-block-end-regexp
  (concat "\\("
          (mapconcat #'identity apdl-end-keywords "\\|")
          "\\)\\>")
  "Regexp containing the APDL end keywords.")

(defconst apdl-block-begin-or-end-regexp
  (concat apdl-block-begin-regexp "\\|" apdl-block-end-regexp)
  "Regexp containing APDL begin and end keywords.")

(defconst apdl-block-else-or-end-regexp
  (concat apdl-block-else-regexp "\\|" apdl-block-end-regexp)
  "Regexp containing the APDL else or end keywords.")

(defconst apdl-block-match-alist
  '(("*IF" . ("THEN" "*ELSE" "*ELSEIF" "*ENDIF"))
    ("*DO" . ("*ENDDO"))
    ("*DOWHILE" . ("*ENDDO"))
    ("*CREATE" . ("*END")))
  "Alist with APDL's matching block keywords.
It has APDL's begin keywords as keys and a list of the
corresponding else or end keywords as associated values.")

(defconst apdl-column-ruler-wide
  (propertize
   (concat
    "0        10        20        30        40        50        60        \
70        80\n"
    "|    |    |    |    |    |    |    |    |    |    |    |    |    |    \
|    |    |\n")
   'font-lock-face 'bold)
  "Contains the string for the wide ruler.
Ruler strings are displayed above the current line with
\\[apdl-column-ruler].")

(defconst apdl-column-ruler-narrow
  (propertize
   (concat
    "0        10        20        30        40        50\n"
    "|    |    |    |    |    |    |    |    |    |    |\n")
   'font-lock-face 'bold)
  "Narrow ruler string.
Ruler strings are displayed above the current line with \\[apdl-column-ruler].")

(when (> emacs-major-version 21)
  (add-to-list 'insert-pair-alist '(?\* ?\*))
  (add-to-list 'insert-pair-alist '(?\$ ?\$))
  (add-to-list 'insert-pair-alist '(?\% ?\%)))

(defconst apdl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "`" 'apdl-abbrev-start) ; ``?' lists abbrevs
    (define-key map "\M-?" 'apdl-show-command-parameters)
    (define-key map "\C-\M-i" 'apdl-complete-symbol)
    ;; --- changed standard Emacs keybindings ---
    (define-key map " " 'apdl-electric-space) ; needed for abbrevs
    (define-key map "\M-j" 'apdl-indent-format-line)
    (define-key map "\n" 'apdl-reindent-then-newline-and-indent)
    ;; end block indentation

    ;; --- especially interesting for continuation lines and condensed
    ;; --- input
    (define-key map "\M-a" 'apdl-command-start)
    (define-key map "\M-e" 'apdl-command-end)
    ;; -- adaption of mark-paragraph
    (define-key map "\M-h" 'apdl-mark-paragraph)
    ;; --- command movement --- (like defuns), skip comments and empty
    ;; --- lines
    (define-key map "\M-p" 'apdl-previous-code-line)
    (define-key map "\M-n" 'apdl-next-code-line)
    ;; --- block movements ---
    (define-key map "\C-\M-f" 'apdl-next-block-end)
    (define-key map "\C-\M-b" 'apdl-previous-block-start-and-conditional)
    (define-key map "\C-\M-n" 'apdl-skip-block-forward)
    (define-key map "\C-\M-p" 'apdl-skip-block-backwards)
    (define-key map "\C-\M-d" 'apdl-down-block)
    (define-key map "\C-\M-u" 'apdl-up-block)
    (define-key map "\C-\M-h" 'apdl-mark-block) ; formerly mark defun
    ;; --- further block keys ---
    (define-key map "\C-c]" 'apdl-close-block)
    (define-key map "\C-c}" 'apdl-number-block-end)
    (define-key map "\C-c{" 'apdl-number-block-start)
    ;; --- pairs
    (define-key map [?\M-\"] 'insert-pair)
    (define-key map "\C-c[" 'insert-pair)
    (define-key map "\C-c'" 'insert-pair)
    (define-key map "\C-c%" 'insert-pair)

    ;; (define-key map [?\C-c?\C-%] 'insert-pair)
    ;; (define-key map [?\C-c?\C-[] 'insert-pair)
    ;; (define-key map [?\C-c?\C-'] 'insert-pair)
    ;; --- miscellaneous ---
    (define-key map [?\C-c?\C-+] 'apdl-zoom-in)
    (define-key map [?\C-c?\C--] 'apdl-zoom-out)
    (define-key map [?\C-c?\C-<] 'apdl-move-left)
    (define-key map [?\C-c?\C->] 'apdl-move-right)
    (define-key map [?\C-c?\C-^] 'apdl-move-up)
    (define-key map [?\C-c?\C-_] 'apdl-move-down)
    (define-key map "\C-x4k" 'apdl-delete-other-window)
    (define-key map "\C-c\C-a" 'apdl-align)
    (define-key map "\C-c\C-b" 'apdl-browse-apdl-help)
    (define-key map "\C-c\C-c" 'apdl-send-to-ansys)
    (define-key map "\C-c\C-d" 'apdl-do)
    (define-key map "\C-c\C-e" 'apdl-display-error-file)
    (define-key map "\C-c\C-f" 'apdl-fit)
    ;;    (define-key map "\C-c\C-g" 'apdl-start-graphics) ; reserved
    (define-key map "\C-c\C-h" 'apdl-mode-help) ; reserved?
    ;;    (define-key map "\C-c\C-i" 'apdl-iso-view) ; reserved: C-tab
    ;;    (define-key map "\C-c\C-i" 'apdl-if)
    (define-key map "\C-c\C-j" 'apdl-send-to-apdl-and-proceed) ; same as ESS
    ;; was:   (define-key map "\C-c\C-j" (if (boundp 'apdl-job) 'apdl-job))
    (define-key map "\C-c\C-k" 'apdl-kill-ansys)
    (define-key map "\C-c\C-l" 'apdl-license-status)
    (define-key map "\C-c\C-m" 'apdl-start-ansys) ; interactively this
    ;; C-c C-n is also C-c RET
    (define-key map "\C-c\C-o" 'apdl-process-status)
    (define-key map "\C-c\C-p" 'apdl-start-pzr-box) ; pan-zoom-rotate
    (define-key map "\C-c\C-q" 'apdl-query-apdl-command)
    (define-key map "\C-c\C-r" 'apdl-replot)
    (define-key map "\C-c\C-s" 'apdl-display-skeleton)
    (define-key map "\C-c\C-t" 'apdl-license)
    (define-key map "\C-c\C-u" 'apdl-copy-or-send-above)
    (define-key map "\C-c\C-v" 'apdl-display-variables)
    (define-key map "\C-c\C-w" 'apdl-display-wb-skeleton) ; or aim:
							  ; runwb2--aim?
    (define-key map "\C-c\C-x" 'apdl-start-classics)
    (define-key map "\C-c\C-y" 'apdl-start-launcher)
    ;; (define-key map "\C-c\C-z" 'apdl-start-anslic_admin)
    ;; redundant with C-c C-l
    (define-key map "\C-c\C-z" 'apdl-user-license-status)
    ;; (define-key map "\C-c\C-z" 'apdl-start-aim)
    ;; (define-key map [f1] 'describe-mode) ; [f1] reserved for user
    map)
  "Keymap for the APDL-Mode.")

(defun apdl-toggle-mode nil ; -FIXME- this toggles also all ansys
			    ; minor-hooks?
  "Restore the buffer's previous major mode, if possible."
  (interactive)
  (if (or (string= apdl-previous-major-mode "apdl-mode")
          (string= apdl-previous-major-mode ""))
      (error "There was no previous major mode except \"APDL-Mode\"")
    ;; buffers opended with auto-mode
    (funcall apdl-previous-major-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- font locking stuff ---

(defvar apdl-command-regexp)
(defvar apdl-command-regexp)
(defvar apdl-deprecated-element-regexp)
(defvar apdl-undocumented-command-regexp)
(defvar apdl-get-function-regexp)
(defvar apdl-command-regexp-1)
(defvar apdl-command-regexp-2a)
(defvar apdl-command-regexp-2b)
(defvar apdl-command-regexp-2c)
(defvar apdl-element-regexp)
(defvar apdl-parametric-function-regexp)
(defvar apdl-dynamic-prompt)
(defvar apdl-completions)

(defface apdl-arg-face
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
  :group 'apdl-faces)

(defvar apdl-arg-face 'apdl-arg-face
  "Face name to use for local vars AR(G), _return, ...")

(let (;; = variable defs + reserved _names
      ;; wie need something behind the = otherwise it's a cleanup
      ;; variables + reserved _names (max. 32 chars long)
      (variable_r
       "^\\s-*\\([[:alpha:]_][[:alnum:]_]\\{0,31\\}\\)\\s-*=")
      ;; reserved vars consisting of a single "_" are valid in Ansys 12.1
      (reserved_vars_r
       "\\_<\\(_[[:alnum:]_]\\{0,31\\}\\>\\)"))

  ;; font-lock-keyword-face is the default face
  (defconst apdl-font-lock-keywords
    `(
      (,variable_r 1 font-lock-variable-name-face); overwritting commands
      (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
                apdl-command-regexp
                "\\)") 1 font-lock-keyword-face)))

  (defconst apdl-font-lock-keywords-1
    `(
      ;; /eof is special: it crashes Ansys in interactive mode
      ;; -TODO- /eof is highlighted only first in line not behind $
      ("\\(?:^\\|\\$\\)\\s-*\\(/[eE][oO][fF].*\\)" 1 'trailing-whitespace t)

      ;; deprecated ansys * comment with 12.1 fini * bla : returns "* no
      ;; longer valid as comment character - please use !"  * bla :
      ;; returns a warning *bla is not a command bla = 3 * 4 : returns
      ;; still 3!
      ("[[:alnum:]_]+\\s-+\\(\\*.*$\\)" 1 font-lock-comment-face prepend)
      ;; ^[:alnum:] to avoid spurious
      ;; asterisk command fontification
      ;; some string faces
      ("\\(?:^\\|\\$\\)\\s-*\\(?:/TIT\\|/TITL\\|/TITLE\\)\\s-*,\\(.*\\)$" 1
       font-lock-doc-face t) ; titles
      ("\\(?:^\\|\\$\\)\\s-*/[cC][oO][mM].?\\(.\\{0,75\\}\\)"
       1 font-lock-doc-face t)
      ;; highlight message of comment command /COM (no comment (!)
      ;; is possible behind /COM), no separating comma necessary

      (apdl-higlight-procent-and-ampersand (0 'font-lock-type-face t))

      ;; /SYS command sends string to OP,no parameter substitution!
      ("^\\s-*/[sS][yY][sS]\\s-*,\\(.\\{1,75\\}\\)$" 1
       font-lock-doc-face t)
      ;; TODO: c*** should get fontification from command regexp
      ("^\\s-*\\([cC]\\*\\*\\*\\)[ ,]?\\(.\\{1,75\\}\\)"
       (1 font-lock-keyword-face t) (2 font-lock-doc-face t))
      ;; only 75 characters possible no separator necessary

      ("\\<\\(ARG[1-9]\\|AR[1][0-9]\\)\\>" . apdl-arg-face)

      ;; elements
      (,apdl-deprecated-element-regexp . font-lock-warning-face)
      (,apdl-element-regexp . font-lock-builtin-face)

      ;; reserved vars consisting of a single "_" are valid in A. 12.1
      (,reserved_vars_r 1 font-lock-warning-face)

      ("_RETURN" 0 apdl-arg-face append)

      ;; = variable defs (with reserved _names), overwritting commands
      (,variable_r 1
                   font-lock-variable-name-face) ; variables (max. 32 chars long)

      (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
                apdl-undocumented-command-regexp
                "\\)\\(\\w*\\)") . font-lock-constant-face)

      ;; get- and parametric-functions
      (,(concat "\\<\\("
                apdl-get-function-regexp
                "\\)(") 1 font-lock-function-name-face)
      (,(concat "\\<\\("
                apdl-parametric-function-regexp
                "\\)(") 1 font-lock-function-name-face)

      ;; command keywords first
      (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
                apdl-command-regexp-1
                "\\)\\>") 1 font-lock-keyword-face)

      ;; some operators
      ("\\$" . 'font-lock-type-face) ; condensed input line
      (":" . 'font-lock-type-face)   ; colon loops only

      ;; deprecated *go labels (max 8 chars including the colon) only at
      ;; the line beginning because they might clash with 'colon' loops
      ;; GOTO Labels, branching
      ("^\\s-*:\\([[:alnum:]_]\\{1,7\\}\\)" 1 font-lock-type-face)))

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

  (defconst apdl-font-lock-keywords-2
    `(
      ;; /eof is special: it crashes Ansys in interactive mode
      ;; -TODO- /eof is highlighted only first in line not behind $
      ("\\(?:^\\|\\$\\)\\s-*\\(/[eE][oO][fF].*\\)" 1 'trailing-whitespace t)

      ;; deprecated ansys * comment with 12.1 fini * bla : returns "* no
      ;; longer valid as comment character - please use !"  * bla :
      ;; returns a warning *bla is not a command bla = 3 * 4 : returns
      ;; still 3!
      ("[[:alnum:]_]+\\s-+\\(\\*.*$\\)" 1 font-lock-comment-face prepend)
      ;; ^[:alnum:] to avoid spurious
      ;; asterisk command fontification
      ;; some string faces
      ("\\(?:^\\|\\$\\)\\s-*\\(?:/TIT\\|/TITL\\|/TITLE\\)\\s-*,\\(.*\\)$" 1
       font-lock-doc-face t) ; titles
      ("\\(?:^\\|\\$\\)\\s-*/[cC][oO][mM].?\\(.\\{0,75\\}\\)" 1
       font-lock-doc-face t)
      ;; highlight message of comment command /COM (no comment (!)
      ;; is possible behind /COM), no separating comma necessary

      ;; multiline format constructs
      ("^\\s-*\\(?:\\*[mM][sS][gG]\\|\\*[vV][rR][eE]\\|\
  \\*[vV][wW][rR]\\|\\*[mM][wW][rR]\\).*\n\\(\\(?:.*&\\s-*\n\\)+.*\\)" ; format
                                        ; constructs
       (1 font-lock-doc-face t))


      ;; ("&\\s-*$" 0 font-lock-type-face t) ; format continuation char
      ;; ("%" 0 font-lock-type-face prepend) ; single % acts as a format
      ;; specifier and pair %.% is a parameter substitution
      (apdl-higlight-procent-and-ampersand (0 'font-lock-type-face t))
      ;; ("%\\|&\\s-*$" (0 'font-lock-type-face t))

      ;; /SYS command sends string to OP,no parameter substitution!
      ("^\\s-*/[sS][yY][sS]\\s-*,\\(.\\{1,75\\}\\)$" 1
       font-lock-doc-face t)
      ;; -TODO-: c*** should get fontification from command regexp
      ("^\\s-*\\([cC]\\*\\*\\*\\)[ ,]?\\(.\\{1,75\\}\\)"
       (1 font-lock-keyword-face t) (2 font-lock-doc-face t))
      ;; only 75 characters possible no separator necessary

      ;; *use variables, local macro call arguments
      ;;   ("\\<\\(ARG[1-9]\\|AR[1][0-9]\\)\\>" . font-lock-warning-face)
      ("\\<\\(ARG[1-9]\\|AR[1][0-9]\\)\\>" . apdl-arg-face)

      ;; elements
      (,apdl-deprecated-element-regexp . font-lock-warning-face)
      (,apdl-element-regexp . font-lock-builtin-face)

      ;; reserved vars consisting of a single "_" are valid in A. 12.1
      (,reserved_vars_r 1 font-lock-warning-face)

      ("_RETURN" 0 apdl-arg-face append)

      ;; = variable defs (with reserved _names), overwritting commands
      (,variable_r 1
                   font-lock-variable-name-face) ; variables (max. 32 chars long)

      (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
                apdl-undocumented-command-regexp
                "\\)\\(\\w*\\)") . font-lock-constant-face)

      ;; get- and parametric-functions
      (,(concat "\\<\\("
                apdl-get-function-regexp
                "\\)(") 1 font-lock-function-name-face)
      (,(concat "\\<\\("
                apdl-parametric-function-regexp
                "\\)(") 1 font-lock-function-name-face)

      ;; command keywords first -2a no characters appended
      (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
                apdl-command-regexp-2a
                "\\)\\>") 1 font-lock-keyword-face)
      (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
                apdl-command-regexp-2b
                "\\)\\(\\w*\\)") (1 font-lock-keyword-face)
                (2 'font-lock-constant-face))
      (,(concat "\\(?:^\\|\\$\\)\\s-*\\("
                apdl-command-regexp-2c
                "\\)\\(\\w*\\)") (1 font-lock-keyword-face)
                (2 'font-lock-constant-face))

      ;; user variables
      (apdl-highlight-variable . font-lock-variable-name-face)

      ;; some operators
      ("\\$" . 'font-lock-type-face) ; condensed input line
      (":" . 'font-lock-type-face)   ; colon loops only

      ;; deprecated *go labels (max 8 chars including the colon) only at
      ;; the line beginning because they might clash with 'colon' loops
      ;; GOTO Labels, branching
      ("^\\s-*:\\([[:alnum:]_]\\{1,7\\}\\)" 1 font-lock-type-face)))

  ;; testing
  (defconst apdl-font-lock-keywords-3
    '(
      ("%" (0 font-lock-builtin-face keep))
      ("^/com.*" (0 font-lock-string-face prepend))
      ("bla" (0 font-lock-variable-name-face prepend)))))

(defconst apdl-font-lock-keyword-list
  '(
	apdl-font-lock-keywords
	apdl-font-lock-keywords-1
	apdl-font-lock-keywords-2
	;; testing
	apdl-font-lock-keywords-3))

(defconst apdl-mode-syntax-table     ; FIXME check APDL operators and
  ;; allowed variable characters
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
    (modify-syntax-entry ?\` "w" table) ; apdl-mode abbreviation specifier,
    ;; not an operator but "word".
    (modify-syntax-entry ?_ "_"  table) ; in APDL symbol component
    (modify-syntax-entry ?: "_"  table) ; APDL label specifier, not an operator
    (modify-syntax-entry ?* "_"  table) ; APDL asterisk commands syntax clashing
    ;; with algebraic operators but blink-matching- needs this
    ;; (modify-syntax-entry ?/ "w" table) ; APDL slash commands
    (modify-syntax-entry ?\! "<" table) ; APDL comment character
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "w" table) ; `"' is *not* a string
                                        ; delimeter for APDL
    (modify-syntax-entry ?'  "\"" table); (modify-syntax-entry ?'  "." table)
    ;; Normally APDL string delimiter, but might clash
    ;; with usages of genitives etc.!
    (modify-syntax-entry ?~ "_" table)  ; APDL connection commands,
                                        ; not an operator
    table)
  "Syntax table in use in `apdl-mode' buffers.")

(defconst apdl-mode-menu
  (list
   "APDL"
   ["Mark Paragraph" apdl-mark-paragraph
    :help "Mark a paragraph, make a region out of it."]
   ["Comment/Un- Region" comment-dwim
    :help "Comment out region or uncomment region, without a
marked region start or realign a code comment in the current
line."]
   ["Complete APDL Keyword" apdl-complete-symbol
    :help "Complete an APDL command, element or function name"]
   ["Send/Copy Region or Paragraph" apdl-send-to-ansys
    :label (if
               (or apdl-classics-flag (apdl-process-running-p))
               "Send region or paragraph to Ansys"
             "Copy region or paragraph to clipboard")
    :help "In case of a running solver/interpreter send marked
region or - by default - the current paragraph to the
interpreter, otherwise copy these lines to the system clipboard"]
   ["Copy/Send above Code to Ansys" apdl-copy-or-send-above
    :label (if
               (or apdl-classics-flag (apdl-process-running-p))
               "Send above Code to Ansys"
             "Copy above Code")
    :help "Either copy the code up to the beginning of file or,
when a run is active, send it to the solver/interpreter"]
   ["Close Logical Block" apdl-close-block
    :help "Close an open control block with the corresponding end
   command"]
;;    ["Insert Parentheses" insert-parentheses
;;     :help "Insert a pair of parentheses enclosing marked region
;; (insert-parentheses)"] ; -FIXME- redundant, necessary for Emacs-23.1
   ["Align region or paragraph" apdl-align
    :help "Align APDL variable definitions in a marked region or
the current paragraph (apdl-align)"]
   ["Display Variable Definitions" apdl-display-variables
    :help "Display all user variable definitions from the current
file in another window (apdl-display-variables)"]
   "--"
   ["Show the Short Command Help" apdl-show-command-parameters
    :help "Display a short help for the APDL command near the
cursor with its parameters (apdl-show-command-parameters)"]
   ["Browse the APDL Keyword Help" apdl-browse-apdl-help
    :help "Open the original APDL documentation for a command or
element name near the cursor in the default
browser (apdl-browse-apdl-help)"
    :active apdl-current-ansys-version]
   ["Interactively Browse Keywords" (apdl-browse-apdl-help t)
    :help "Choose and browse the original APDL documentation for
a command, element name or other
subjects (apdl-browse-apdl-help)."
    :active apdl-current-ansys-version]
   ["Browse the Ansys APDL Guide" apdl-browse-ansys-apdl-manual
    :help "Read the original Ansys Paramtetric Design Language
Guide in a browser (apdl-browse-ansys-apdl-manual)"
    ;; :active (file-readable-p apdl-ansys-help-path) ; now also online :-)
    ]
   "--"
   ["Preview Macro Template" apdl-display-skeleton
    :help "Preview an APDL code template in another window"]
   (list
    "Insert Macro Template"
    ["*IF ... *ENDIF" apdl-if
     :help "Insert interactively an *if .. *endif construct"]
    ["*DO ... *ENDDO" apdl-do
     :help "Insert interactively a *do .. *enddo loop"]
    ["*IF ... *ELSEIF" apdl-if-then
     :help "Insert interactively an *if,then .. \
(*elseif .. *else ..) *endif construct."]
    ["MP" apdl-mp
     :help "Insert interactively an mp statement."]
    ["Header" apdl-skeleton-header
     :help "Insert interactively the file header template"]
    ["Insert Pi" apdl-insert-pi
     :help "Insert the variable definition \"Pi = acos(-1) !
3.1415...\" at point and indent this line."]
    "--"
    ["Configuration" apdl-skeleton-configuration
     :help "Configuration code template"]
    ["Get- and Fortran functions" apdl-skeleton-function
     :help "Get- and Fortran functions template"]
    ["View Settings" apdl-skeleton-view-settings
     :help "View settings like focus point, magnification, ..."]
    ["Coordinate Sys. Display" apdl-skeleton-coordinates
     :help "Template for creating and handling coordinate
    systems"]
    ["Working Plane Operations"
    apdl-skeleton-working-plane
     :help "Template for creating and handling the working
    plane"]
    ["Multiplot Commands" apdl-skeleton-multi-plot
     :help "Graphic commands which show multiple entities
    simultaneously"]
    ["Numbering Controls"
    apdl-skeleton-numbering-controls
     :help "Commands for numbering and colouring model entities"]
    ["Symbol Controls" apdl-skeleton-symbols
     :help "Graphic commands which show boundary conditions,
surface loads and other symbols"]
    ["Geometry Import"
apdl-skeleton-import
     :help "Command for importing IGES models"]
    ["Control flow constructs" apdl-skeleton-looping
     :help "Commands for controlling loops (*do) and the program
    flow (*if)"]
    ["Symmetry Expansions" apdl-skeleton-expand
     :help "Commands for expanding the view of symmetric models
to their full view"]
    ["Element Definitions" apdl-skeleton-element-definition
     :help "2D, 3D, Element defintions and their keyoptions"]
    ["Material Definitions" apdl-skeleton-material-defintion
     :help "Various material definitions: Steel, alu, rubber, ..."]
    ["Modeling" apdl-skeleton-geometry
     :help "Operations for geometric modeling"]
    ["Meshing Controls" apdl-skeleton-meshing
     :help "Meshing control commands: Shapes, sizes, ..."]
    ["Contact Pair Definition" apdl-skeleton-contact-definition
     :help "Full definition of flexible-flexible contact pairs"]
    ["Rigid Contact" apdl-skeleton-contact-rigid
     :help "Definition of the rigid target contact side"]
    ["Contact Template" apdl-skeleton-contact-template
     :help "Minimal working contact example"]
    ["Boundary Conditions" apdl-skeleton-bc
     :help "Commands for establishing boundary conditions"]
    ["Buckling Analysis Type" apdl-skeleton-buckling
     :help "Commands for establishing a buckling analysis"]
    ["Listings, Information, Statistics"apdl-skeleton-information
     :help "Parameter listings, graphic options, system
information, run statistics"]
    ["Solving" apdl-skeleton-solve
     :help "Ansys solver (/solu) commands and solver options"]
    ["Post1 Postprocessing" apdl-skeleton-post1
     :help "General postprocessor (/post1) commands"]
    ["Array Operations" apdl-skeleton-array
     :help "Dimensioning, looping, changing array parameters"]
    ["Path plot operations" apdl-skeleton-path-plot
     :help "Commands for establishing paths and plotting entities
    on paths"]
    ["Output to file" apdl-skeleton-output-to-file
     :help "Commands for writing data to a file"]
    ["Element Table Operations" apdl-skeleton-element-table
     :help "Commands for establishing and manipulation element
    tables"]
    ["Post26 Postprocessing" apdl-skeleton-post26
     :help "Time history (/post26) postprocessing commands"]
    ["Components" apdl-skeleton-component
     :help "Components (Named selections in WorkBench) template"]
    ["Selections" apdl-skeleton-select
     :help "How to select stuff template"]
    "--"
    ["Outline template" apdl-skeleton-outline-template
     :help "Empty skeleton of the structur of an APDL simulation,
outlineing headers and sections"]
    ["Beam template" apdl-skeleton-beam-template
     :help "Insert a minimal template for a beam simulation"]
    ["Structural template" apdl-skeleton-structural
     :help "Insert a minimal template for a structural simulation"]
    ["Contact template" apdl-skeleton-contact
     :help "Insert a minimal template for a structural contact
    simulation"]
    ["Compilation of templates" apdl-skeleton
     :help "Insert a compilation of selected templates"])
   ["Preview WorkBench Template" apdl-display-wb-skeleton
    :help "Preview an WorkBench Command (APDL) template in
    another window"]
   (list
    "Insert WorkBench Template"
    ["*IF ... *ENDIF" apdl-if
     :help "Insert interactively an *if .. *endif construct"]
    ["*DO ... *ENDDO" apdl-do
     :help "Insert interactively a *do .. *enddo loop"]
    ["*IF ... *ELSEIF" apdl-if-then
     :help "Insert interactively an *if,then .. (*elseif .. *else
..)  *endif construct."]
    ["Do loop" apdl-wbt-do
     :help "Insert a do loop."]
    ["Header" apdl-wbt-if
     :help "Insert an if loop."]
    ["Insert Pi" apdl-insert-pi
     :help "Insert the variable definition \"Pi = acos(-1) !
3.1415...\" at point and indent this line."]
    "--"
    ["Post: Press-fit calcs" apdl-wbt-post-2d-press-fit_calcs
     :help "Post: Calculate the maximum torque and other
parameters from a plane stress press-fit simulation."]
    ["Post: /post26 harmonic results" apdl-wbt-harmonic-acceleration-result
     :help "Post: /post26 harmonic acceleration results.
Visualisation and file output of frequency and vector sum
aplitude."]
    ["Post: /post26 general results" apdl-wbt-post26-output
     :help "Post: /post26 output template"]
    )
   "--"
   (list
    "Navigate Code Lines"
    ["Previous Code Line" apdl-previous-code-line
     :help "Goto previous apdl code line"]
    ["Next Code Line" apdl-next-code-line
     :help "Goto next code line"]
    ["Beginning of (Continuation) Command" apdl-command-start
     :help "Go to the beginning of the current command"]
    ["End of (Continuation) Command" apdl-command-end
     :help "Go to the end of the current command"]
    "--"
    ["Split Format Line at Point" apdl-indent-format-line
     :help "Split current line, if in a comment continue the
comment, if in an APDL format line insert the continuation
character before splitting the line"])
   (list
    "Work with Logical Blocks"
    ["Next Block End" apdl-next-block-end
     :help "Go to the end of the current or next control block
(*do, *if, ...)"]
    ["Previous Block Start" apdl-previous-block-start-and-conditional
     :help "Go to the beginning of the current or next control
     block (*do, *if, ...)"]
    ["Down Block" apdl-down-block
     :help "Move down one control block level"]
    ["Up Block" apdl-up-block
     :help "Move up one control block level"]
    ["Skip Block Forward" apdl-skip-block-forward
     :help "Skip to the end of the next control block"]
    ["Skip Block Backwards" apdl-skip-block-backwards
     :help "Skip to the beginning of previous control block"]
    ["Hide Number Blocks" apdl-hide-number-blocks
     :help "Hide all APDL number blocks (EBLOCK, NBLOCK,
    CMBLOCK)"]
    ["Unhide Number Blocks" apdl-unhide-number-blocks
     :help "Unhide all APDL number blocks (EBLOCK, NBLOCK,
    CMBLOCK)"]
    ["Beginning of N. Block" apdl-number-block-start
     :help "Go to the beginning of an APDL number blocks
(EBLOCK, NBLOCK, CMBLOCK)"]
    ["End of Number Block"    apdl-number-block-end
     :help "Go to the end of an APDL number blocks (EBLOCK,
     NBLOCK, CMBLOCK)"]
    "--"
    ["Close Block" apdl-close-block
     :help "Close the current APDL control block with the
respective closing command"]
    ["Mark Block" apdl-mark-block
     :help "Mark the current control block"]
    ["Hide Region" apdl-hide-region
     :help "Hide a marked region and display a hidden region
    message"]
    ["Unhide Regions" apdl-unhide-number-blocks
     :help "Unhide all hidden regions"]
    ["Insert Temporary Ruler" apdl-column-ruler
     :help "Show a temporary ruler above the current line"])
   "--"
   (list
    "Helper Modes"
    ["Ruler Mode" ruler-mode
     :style toggle :selected ruler-mode
     :help "Toggle display of ruler in header line (Ruler mode)."]
    ["Outline Minor Mode" outline-minor-mode
     :style toggle :selected outline-minor-mode
     :help "Outline Mode is for hiding and selectively displaying
headlines and their sublevel contents"]
    ["Show Paren Mode" show-paren-mode :style toggle
     :selected show-paren-mode
     :help "Show Paren Mode highlights matching parenthesis"]
    ["Delete Selection Mode" delete-selection-mode
     :style toggle :selected delete-selection-mode
     :help
     "Delete Selection Mode replaces the selection with typed
    text"]
    ["Electric Pair Mode" electric-pair-mode
     :style toggle :selected electric-pair-mode
     :help
     "Electric Pair Mode insert corresponding closing delimeters"
     :visible (version< "24" emacs-version)])
   "--"
   ["APDL-Mode Documentation" apdl-mode-help
    :help "Display the APDL-Mode Documentation in Emacs' Info Viewer."]
   ["Help on APDL-Mode" describe-mode
    :help "Open an Emacs window describing APDL-Mode's usage"]
   ["Customise APDL-Mode"        (customize-group "APDL")
    :help "Open a special customisation window for changing the
values and inspecting the documentation of its customisation
variables"]
   ["List Mode Abbreviations" (list-abbrevs t)
    :help
    "Display a list of all abbreviation definitions for logical
   blocks"]
   ["Submit Bug Report" apdl-submit-bug-report
    :help "Open a mail template for an APDL-Mode bug report"]
   ["Reload APDL-Mode" apdl-reload-apdl-mode
    :help "Loading the mode definitions anew from files and
restarting apdl-mode"]
   "--"
   ["Exit APDL-Mode" apdl-toggle-mode
    :help "Switch to the previous major mode of the file"
    :label (concat "Exit APDL-Mode Version: " apdl-mode-version)])
  "APDL menu items for APDL-Mode.")

(defconst apdl-task-menu
  (list
   "MAPDL"
   ["Specify License Server or - File" apdl-license-file
    :label (if apdl-license-file
               "Change License Server or - File"
             "Specify License Server or - File")
    ;; :visible apdl-is-unix-system-flag
    :help "Change the license server specification (for an
solver/interpreter run or the license status), either naming the
license server machine (with port) or the actual license file"]
   ["Specify the License Interconnect Servers" apdl-ansysli-servers
    :label (if apdl-ansysli-servers
               "Change the License Interconnect Servers"
             "Specify the License Interconnect Servers")
    :visible apdl-is-unix-system-flag
    :help "Change the interconnect server specification (for an
solver/ interpreter run)"]
   ["Installation Directory" apdl-ansys-install-directory
    :label (if apdl-ansys-install-directory
               (concat "Change the Installation Directory ["
                       apdl-current-ansys-version "]")
             "Set the Ansys Installation Directory!")
    :help "For certain functionality you need to set the
installation directory of Ansys, the path up to the version
number vXXX (apdl-ansys-install-directory)"]
   ["Change MAPDL License Type" apdl-license
    :label (concat "Change License Type [" apdl-license "]")
    :help "Specify the license type for an solver/interpreter run
(apdl-license)"]
   ["Change Job Name of Run" apdl-job
    :label (concat "Change Job Name [" apdl-job "]")
    ;; :visible apdl-is-unix-system-flag
    :help "Specify the job name for an solver/interpreter
   run (apdl-job)"]
   ["Change the No of Processors" apdl-no-of-processors
    :label (format "Change the Number of Processors [%d]"
                   apdl-no-of-processors)
    ;; :visible apdl-is-unix-system-flag
    :help "Specify the number of processors to use for the Ansys
run definition (apdl-no-of-processors)"]
   "--"
   ["Start the Ansys Help Viewer" apdl-start-ansys-help
    :help "Start the Ansys Help Viewer
(apdl-start-ansys-help).  If there is no local help installed or
you configured online help you will be directed to the main
online help page."
    :active (file-executable-p apdl-ansys-help-program)]
   ["License Server Status" apdl-license-status
    :help "Show the license server status, the number of licenses
available and used (apdl-license-status)"
    :active (and apdl-lmutil-program (file-executable-p apdl-lmutil-program)
		 apdl-license-file apdl-username)]
   ["License User Status" apdl-user-license-status
    :help "Show the license user status, the licenses
used (apdl-user-license-status)"
    :active (and apdl-lmutil-program (file-executable-p apdl-lmutil-program)
		 apdl-license-file)]
   ["Start Ansys WorkBench" apdl-start-wb
    :active (and apdl-ansys-wb (file-executable-p apdl-ansys-wb))
    :help "Start the Ansys WorkBench (apdl-start-wb)"]
   ["Ansys MAPDL Product Launcher" apdl-start-launcher
    :active (and apdl-ansys-launcher (file-executable-p apdl-ansys-launcher))
    :help "Start the Ansys Mechanical APDL Product Launcher
(apdl-start-launcher)"]
   ["Ansys MAPDL Classics GUI" apdl-start-classics
    :active (and apdl-ansys-program (file-executable-p apdl-ansys-program))
    ;;    :visible apdl-is-unix-system-flag
    :help "Start the Ansys Classics MAPDL
   GUI (apdl-start-classics)"]
   ["Start Interactive Solver/Interpreter" apdl-start-ansys
    :help "Start an interactive MAPDL solver/interpreter run
    under Linux (apdl-start-ansys)"
    :active (and apdl-is-unix-system-flag
                 (file-executable-p apdl-ansys-program)
                 (not (apdl-process-running-p)))]
   "--"
;;   ;; not supported any longer 2020-03
;;    ["Connect to Classics" apdl-toggle-classics
;;     :label (if apdl-classics-flag
;;                "Switch off sending to Classics MAPDL"
;;              "Switch on sending to Classics MAPDL")
;;     :active (and apdl-is-unix-system-flag (not (apdl-process-running-p)))
;;     :help "Check whether an Ansys Classic is running and toogle sending output
;; to it (apdl-toggle-classics)"]
   ["Send/Copy Region or Paragraph" apdl-send-to-ansys
    :label (if
               (or apdl-classics-flag (apdl-process-running-p))
               "Send region or paragraph to MAPDL"
             "Copy region or paragraph to clipboard")
    :help "In case of a running solver/interpreter send the
marked region or by default the current paragraph to the
interpreter, otherwise copy these lines to the system
clipboard (apdl-send-to-ansys)"]
   ["Send/Copy Line or Region" apdl-send-to-apdl-and-proceed
    :label (if (or apdl-classics-flag (apdl-process-running-p))
               "Send line or region to MAPDL"
             "Copy line or region to clipboard")
    :help "In case of a running solver/interpreter send the
marked region or by default the current line to the interpreter,
otherwise copy these lines to the system
clipboard (apdl-send-to-apdl-and-proceed)"]
   ["Copy/Send above Code to Ansys" apdl-copy-or-send-above
    :label (if (or apdl-classics-flag (apdl-process-running-p))
               "Send above Code to MAPDL"
             "Copy above Code to clipboard")
    :help "Either copy the code up to the beginning of file or,
when a run is active, send it to the
solver/interpreter (apdl-copy-or-send-above)"]
   (list
    "Send Graphics Command"
    ["Start Graphics Screen" apdl-start-graphics
     :help "Open the graphics screen for the interactive MAPDL
     mode (apdl-start-graphics)"
     :active (apdl-process-running-p)]
    ["Start Pan/Zoom/Rot. Dialog" apdl-start-pzr-box
     :help "Open the Pan/Zoom/Rotate dialog of the Ansys GUI
(apdl-start-pzr-box)"
     :active (or apdl-classics-flag (apdl-process-running-p))]
    ["Replot" apdl-replot
     :help "Replot the Ansys graphics window (apdl-replot)"
     :active (or apdl-classics-flag (apdl-process-running-p))]
    ["Fit Graphics into screen" apdl-fit
     :help "Fit the Ansys graphics into the window (apdl-fit)"
     :active (or apdl-classics-flag (apdl-process-running-p))]
    ["Show Graphics in iso-view" apdl-iso-view
     :help "Show the current Ansys graphic windows in isometric
     view (apdl-iso-view)"
     :active (or apdl-classics-flag (apdl-process-running-p))]
    ["Zoom In" apdl-zoom-in
     :help "Zoom into the graphics (apdl-zoom-in)"
     :active (or apdl-classics-flag (apdl-process-running-p))]
    ["Zoom Out" apdl-zoom-out
     :help "Zoom out of the graphics (apdl-zoom-out)"
     :active (or apdl-classics-flag (apdl-process-running-p))]
    ["Move Up" apdl-move-up
     :help "Move graphics objects up (apdl-move-up)"
     :active (or apdl-classics-flag (apdl-process-running-p))]
    ["Move Down" apdl-move-down
     :help "Move graphics objects down (apdl-move-down)"
     :active (or apdl-classics-flag (apdl-process-running-p))]
    ["Move Right" apdl-move-right
     :help "Move graphics objects to the right (apdl-move-right)"
     :active (or apdl-classics-flag (apdl-process-running-p))]
    ["Move Left" apdl-move-left
     :help "Move graphics objects to the left (apdl-move-left)"
     :active (or apdl-classics-flag (apdl-process-running-p))])
   ["Send MAPDL Command Interactively" apdl-query-apdl-command
    :help "Send interactively an APDL command to a running MAPDL
solver interpreter process (apdl-query-apdl-command)"
    :active (or apdl-classics-flag (apdl-process-running-p))]
   "--"
   ["Display MAPDL Run Status" apdl-process-status
    :help "Display the status of the Ansys MAPDL
    solver/interpreter run (apdl-process-status)"
    :active (apdl-process-running-p)]
   ["Exit MAPDL Run" apdl-exit-ansys
    :help "Exit the active MAPDL solver/interpreter
    run (apdl-exit-ansys)"
    :visible (apdl-process-running-p)]
   ["Display MAPDL Error File" apdl-display-error-file
    :help "Display in another window in auto-revert-tail-mode the
MAPDL error file (job.err) in the current working
directory (apdl-display-error-file)"
    :active (file-readable-p (concat default-directory apdl-job ".err"))]
   ["Write MAPDL Stop File" apdl-abort-file
    :active  (file-readable-p (concat default-directory apdl-job ".lock"))
    :help "Write a file: JOB.abt containing the word
\"nonlinear\" for orderly stopping the solver in the current
working directory (apdl-abort-file)"]
   "--"
   ["Kill MAPDL Run" apdl-kill-ansys
    :help "Kill the current MAPDL run (apdl-kill-ansys)"
    :active (apdl-process-running-p)]
   ["List all Emacs' Processes" list-processes
    :help "Show all active processes under Emacs, like shells,
    etc. (list-processes)"]
   ["View Emacs' Messages" view-echo-area-messages
    :help "Display Emacs' latest messages for debugging and
    checking purposes"])
  "Ansys menu items for APDL-Mode.")

;;; --- predicates ---

(defun apdl-in-asterisk-comment-p ()
  "Return t if the cursor is inside an APDL asterisk comment."
  (save-excursion
    (let ((lbp (line-beginning-position)))
      (if (search-backward " *" lbp t)
          t
        nil))))

(defun apdl-in-string-command-line-p ()
  "Return t if in an APDL string command line."
  (save-excursion
    (back-to-indentation)
    (looking-at apdl-string-commands-regexp)))

(defun apdl-number-line-p ()
  "Return t if in an APDL number block."
  (save-excursion
    (beginning-of-line)
    (and (not (apdl-in-format-construct-p))
         (looking-at apdl-number-line-regexp)))) ; "(" is for CMBLOCK
						 ; format string

(defun apdl-default-command-p ()
  "Return t if in an APDL default command line.
The current code is reusing the previous APDL command with a line
beginning with a comma."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*,")))

(defun apdl-in-indentation-p ()
  "Return t if in an indentation."
  (if (and (eolp) (bolp)) ; take care of empty lines
      nil
    (let ((p (point)))
      (save-excursion
        (back-to-indentation)
        (if (<= p (point)) t nil)))))

(defun apdl-first-line-p ()
  "Return t if at the first line."
  (save-excursion
    (beginning-of-line)
    (bobp)))

(defun apdl-last-line-p ()
  "Return t if at the last line."
  (save-excursion
    (end-of-line)
    (eobp)))

(defun apdl-continuation-line-p ()
  "Return t if in a continutation line of certain commands."
  (save-excursion
    (beginning-of-line)
    (if (looking-at apdl-continuation-line-regexp) t nil)))

(defun apdl-in-format-command-line-p ()
  "Return t if in an APDL format command line, nil otherwise.
See the constant variable `apdl-format-commands-regexp' which
includes the commands which need formatting lines."
  (save-excursion
    (beginning-of-line)
    (if (looking-at
         (concat "^\\s-*\\(" apdl-format-commands-regexp
                 "\\)")) t nil)))

(defun apdl-in-format-construct-p ()
  "Return t if in an APDL format construct.
Otherwise nil, i.e. return nil when in a format command line."
  (cond ((apdl-continuation-line-p) t)
        ((apdl-first-line-p) nil)
        (t (save-excursion
             (forward-line -1)
             (if (or
                  (apdl-continuation-line-p)
                  (apdl-in-format-command-line-p)) t nil)))))

(defun apdl-condensed-input-line-p ()
  "Return t if in an APDL condensed (... $ ...) input line."
  (save-excursion
    (beginning-of-line)
    (if (apdl-in-format-construct-p)
        nil
      (if (looking-at apdl-condensed-input-line-regexp)
          t
        nil))))

(defun apdl-code-line-p ()
  "Return t if in an APDL code line, nil otherwise.
A code line is the complementary to the regexp
`apdl-non-code-line-regexp'."
  (save-excursion
    (beginning-of-line)
    (if (looking-at apdl-non-code-line-regexp) nil t)))

(defun apdl-not-in-code-line-p ()
  "Return t if not in an APDL code line, nil otherwise.
A code line is the complementary to the regexp
`apdl-non-code-line-regexp'."
  (save-excursion
    (beginning-of-line)
    (looking-at apdl-non-code-line-regexp)))

(defun apdl-at-end-of-text-p ()
  "Return t if the cusor is at the end of text in a line."
  (if (looking-at "\\s-*$") t nil))

(defun apdl-at-end-of-code-p ()
  "Return t if the cursor is at the end of code in a line.
This means at the end of code before whitespace or an APDL
comment."
  (if (looking-at "\\s-*$\\|\\s-*!") t nil))

(defun apdl-is-unix-system-p ()
  "Return t when we are on a Unix system.
gnu/linux, aix, berkeley-unix, hpux, irix, lynxos 3.0.1,
usg-unix-v.  Ansys supports only GNU-Linux 64 and Windows 64 for
the entire Ansys platform with some support of legacy Unices (AIX
IBM, HP-UX HP, SGI, Solaris SUN) for standalone apps will be
provided so I won't restrict some aspects of APDL-Mode to
GNU-Linux."
  (not
   (or (string= system-type "gnu")      ; gnu with the hurd kernel
       (string= system-type "darwin")   ; mac
       (string= system-type "ms-dos")
       (string= system-type "windows-nt")
       (string= system-type "cygwin"))))

;; FIXME DEFSUBSTs with DEFUNs (apdl-position) inside aren't
;; particularly speedy, are they?

(defsubst apdl-in-comment-p ()
  "Return t if the cursor is inside an APDL comment.
The cursor is either in a code comment or comment line."
  (save-excursion
    (nth 4 (parse-partial-sexp (apdl-position 'bol)
                               (point))))) ; nth -- nth element of list

(defsubst apdl-in-comment-line-p ()
  "Return t if the cursor is in a comment line."
  (save-excursion
    (back-to-indentation)
    (looking-at "!")))

(defsubst apdl-in-string-p () ; FIXME:are there strings defined in ansys?
  "Return t if the cursor is inside an APDL string."
  (save-excursion
    (nth 3 (parse-partial-sexp (apdl-position 'bol) (point)))))

(defsubst apdl-in-empty-line-p()
  "Return t if the cursor is in an empty (whitespace) line."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \n\t]*$")))

(defsubst apdl-not-in-string-or-comment-p()
  "Return t if the cursor is not inside a string or comment."
  (let ((pps (parse-partial-sexp (apdl-position 'bol) (point))))
    (not (or (nth 3 pps) (nth 4 pps)))))

(defsubst apdl-in-string-or-comment-p ()
  "Return t if the cursor is not inside a string or comment."
  (let ((pps (parse-partial-sexp (apdl-position 'bol) (point))))
    (or (nth 3 pps) (nth 4 pps))))

;; ======================================================================
;; --- interactive functions ---

(defun apdl-mode-help ()
  "Browse the APDL-Mode documentation in the Info Viewer."
  (interactive)
  (info "(apdl-mode)Top"))

(defun apdl-mode-browse-online ()
  "Browse the APDL-Mode online documentations."
  (interactive)
  (let ((url "https://dieter-wilhelm.github.io/apdl-mode"))
    (cond
     (apdl-is-unix-system-flag
      ;; use browse-url-default-browser!
      (if (fboundp 'browse-url-xdg-open)
          (browse-url-xdg-open url)
        ;; (browse-url-default-browser
        ;; (concat path file)) not working with E23.1 on RHEL
        (browse-url-firefox url)))
     ;; windows
     (t
      ;; wrapper of ShellExecute MS-Windows API
      ;;      (message "file:%s path:%s" file path)
      ;;      (w32-shell-execute "Open" (concat path file)))
      (browse-url-default-windows-browser url)))))

(defun apdl-align (p-min p-max)
  "Align current paragraph or selection of APDL variable definitions.
If a region is selected align it (with the region borders P-MIN
and P-MAX) otherwise align the current code paragraph."
  (interactive "r")
  (if mark-active
      (align p-min p-max)
    (align-current))) ; align-current needs a mark

;;  the autoload cookie is copying stuff to the -autoloads.el file
;;  check with (update-file-autoloads)

;;;###autoload (add-to-list 'auto-mode-alist '("\\.mac\\'" . apdl-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.dat\\'" . apdl-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.inp\\'" . apdl-mode))

;;;###autoload
(defun apdl-mode ()
  "Editor support for the APDL language and working with Ansys MAPDL.

APDL-Mode (formerly Ansys-Mode) is - in conjunction with the
GNU-Emacs editor - an advanced APDL environment with features
like, pin-pointing the APDL reference documentation, keyword
completion, code templates, dedicated highlighting, solver
communication (GNU-Linux only), license reporting, etc.  Over the
years it has accumulated lots of features for writing WorkBench /
Discovery AIM Command (APDL) objects and debugging complete FEA
models in APDL code.

The extensive documentation can be accessed from the APDL-Mode
menu or by calling the function `apdl-mode-help' with
\\[apdl-mode-help].

\\{apdl-mode-map}"
  (interactive)

  (unless (string= major-mode "apdl-mode")
    (set (make-local-variable 'apdl-previous-major-mode) major-mode))
  (put 'apdl-previous-major-mode 'permanent-local t)

  (when (and (overlayp apdl-help-overlay)
             (overlay-buffer apdl-help-overlay))
    (delete-overlay apdl-help-overlay))

  (kill-all-local-variables)            ; convention
  (setq major-mode 'apdl-mode)
  (setq mode-name "APDL")               ; mode line string

  ;; only effective for window systems!
  (setq indicate-empty-lines apdl-indicate-empty-lines-flag)

  (setq completion-ignore-case t) ; keyword completion regardless of cases

  (use-local-map apdl-mode-map)
  (set-syntax-table apdl-mode-syntax-table)
  (setq local-abbrev-table apdl-mode-abbrev-table)

  (setq font-lock-maximum-decoration
        `((apdl-mode . ,apdl-highlighting-level) (t . t)))

  (setq align-mode-rules-list apdl-align-rules-list)
  ;; (when (> apdl-highlighting-level 1)
  ;;   (setq font-lock-multiline t)) ; for *msg, *vwrite,.. format strings

  (make-local-variable 'apdl-user-variable-regexp) ; for font-lock
  (setq apdl-user-variable-regexp nil)

  (make-local-variable 'parens-require-spaces)
  (setq parens-require-spaces apdl-require-spaces-flag)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'apdl-indent-line-function)

  (make-local-variable 'comment-start)
  (setq comment-start apdl-indent-comment-string)

  (make-local-variable 'comment-padding)
  (setq comment-padding apdl-comment-padding)

  (make-local-variable 'comment-add)
  (setq comment-add apdl-comment-add)

  (make-local-variable 'comment-column)
  (setq comment-column apdl-code-comment-column)

  ;; FIXME:
  ;;  (setq comment-fill-column 50)???
  ;;  comment-indent -> fill-column?? only when line-wrap mode t?

  ;; overlay for command-parameter-help

  ;;  (make-local-variable 'apdl-timer)
  ;;  (make-local-variable 'apdl-help-overlay)
  (setq apdl-help-overlay (make-overlay 1 1))

  ;; look at newcomment.el
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip apdl-comment-start-skip)

  ;;  (make-local-variable 'parse-sexp-ignore-comments)
  ;;  (setq parse-sexp-ignore-comments t)

  ;;  (make-local-variable 'font-lock-defaults) is always local
  (setq font-lock-defaults `(,apdl-font-lock-keyword-list nil 'case-ignore))
  ;; keywords
  ;; keywords-only -- nil: syntactic fontification
  ;; case-fold -- non nil: ignore case

  (make-local-variable 'outline-regexp)
  (make-local-variable 'apdl-hide-region-overlays)
  ;;  outline searches only at the line beginning
  (setq outline-regexp (concat "!\\(" apdl-outline-string "\\)+"))

  ;; discrepancies from Emacs defaults
  (apdl-font-lock-mode)                 ; switch on font-lock when
					; it's toggled

  ;; It is impolite to enforce below behaviour over possible user
  ;; customisations

  ;; ; (delete-selection-mode t)
  ;; ; (toggle-truncate-lines 1)
  ;; ; (show-paren-mode t)
  ;; ; (set (make-local-variable 'scroll-preserve-screen-position) nil)

  (setq apdl-is-unix-system-flag (apdl-is-unix-system-p))

  ;; menu
  (apdl-add-apdl-menu)

  ;; --- user variables ---
  (message "Dealing with user variables.")
  ;; -highlighting-level >= 2 and apdl-dynamic-highlighting-flag
  (if (and (>= apdl-highlighting-level 2)
           apdl-dynamic-highlighting-flag
           (or
            ;; either *APDL code* buffer
            (string= (buffer-name) "*APDL code*")
            ;; or  .mac or .ans files and both smaller than 30 Mb
            (and (buffer-file-name)
                 (or (string= (file-name-extension (buffer-file-name)
                                                   'dot) ".ans")
                     (string= (file-name-extension (buffer-file-name)
                                                   'dot) ".mac"))
                 ;; 30 Mb bigger than file?
                 (when (file-attributes (buffer-file-name)) ; open an
							    ; existing
							    ; file
                   (if (> 30000000 (nth 7 (file-attributes (buffer-file-name))))
                       t
                     (y-or-n-p "File is larger than 30 MB, switch on \
user variable highlighting? "))))))
      (progn
	;; (message "before apdl-update-p.")
        (add-hook 'after-change-functions
                  #'apdl-find-user-variables nil t)
        (add-hook 'post-command-hook
                  #'apdl-update-parameter-help nil t)
        (message "Dynamic highlighting of user variables activated."))
    (message "Non-dynamic highlighting of variables activated."))

  (apdl-find-user-variables)

  ;;   (if (>= apdl-highlighting-level 2)
  ;;       (when (or
  ;; 	     (when (not buffer-file-name)
  ;; 	       t) ; skip below size query (buffer without a file)
  ;; 	     (> 30000000 (nth 7 (file-attributes (buffer-file-name))))
  ;; 	     (y-or-n-p
  ;; "File is larger than 30 MB, switch on user variable highlighting? "))
  ;; 	(message "before if.")
  ;; 	(if (and apdl-dynamic-highlighting-flag
  ;; 		 (or (string= (buffer-name) "*APDL code*")

  ;; ;		     (message "before ans.")
  ;; (string= (file-name-extension (buffer-file-name) 'dot) ".ans")
  ;; ;		   (message "before mac.")
  ;; (string= (file-name-extension (buffer-file-name) 'dot) ".mac")))
  ;; 	    (progn (message "before addhook.")
  ;; 		   (add-hook 'after-change-functions
  ;; 			     'apdl-find-user-variables nil t)
  ;; 		   (message "before apdl-update-p.")
  ;; 		   (add-hook 'post-command-hook
  ;; 			     'apdl-update-parameter-help nil t)
  ;; (message "Dynamic highlighting of user variables activated."))
  ;; 	  (message "Non-dynamic highlighting of variables activated."))
  ;; 	(apdl-find-user-variables)))

  ;; .dat WorkBench solver input files
  (when (and buffer-file-name ; a buffer with a file name
             (string= (file-name-extension (buffer-file-name) t) ".dat"))
    (apdl-hide-number-blocks))

  ;; a-align needs a mark to work for an unspecified region
  ;; (set-mark 0) -TODO-

  ;; initialise system dependent stuff
  (unless apdl-initialised-flag
    (apdl-initialise))
  ;; that is not friendly to enforce stuff on users
  ;; (outline-minor-mode t)
  ;; --- hooks ---
  (run-hooks 'apdl-mode-hook)
  )
;;  -- end of apdl-mode --
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun apdl ()
  "Open a new buffer in APDL-Mode.
You must save the buffer (connect it with a file-name), otherwise
possible edits are lost."
  (interactive)
  (let ((b "*APDL code*"))
    (get-buffer-create b)
    (switch-to-buffer b)
    (when (< (buffer-size) 1)
      (insert "!! This is an unnamed file under APDL-Mode.\n\
!! Please save it (C-x C-s) and start your APDL hacking...\n\n"))
    (apdl-mode)))

;; FIXME
;; (defun apdl-ansysli-servers-check ()
;;   "Return t if Ansys interconnect server information is found.
;; Checking whether the variable `apdl-ansysli-servers' is set or
;; otherwise the environment variable AnsysLI_SERVERS.  If neither
;; is set return nil"
;;   (interactive)
;;   (cond
;;    (apdl-ansysli-servers
;; ;    (setenv "AnsysLI_SERVERS" apdl-ansysli-servers)
;; ; (message "Set process environment variable AnsysLI_SERVERS
;; to apdl-ansysli-servers")
;;     t)
;;    ((getenv "AnsysLI_SERVERS")
;;     (setq apdl-ansysli-servers (getenv "AnsysLI_SERVERS"))
;;     (message "Read apdl-ansysli-servers from process environment
;;     variable AnsysLI_SERVERS") t)
;;    (t nil)))

;; (defun apdl-license-file-check ()
;;   "Return t if Ansys license file (server) information is found.
;; Checks whether the variable `apdl-license-file' is set, if not
;; sets its value to the environment variable AnsysLMD_LICENSE_FILE
;; or LM_LICENSE_FILE, in this order of precedence.  When the former
;; are not available return nil."
;;  (let ((lic1 (getenv "AnsysLMD_LICENSE_FILE"))
;;        (lic2 (getenv "LM_LICENSE_FILE"))
;;        )
;;      (cond
;;    (apdl-license-file
;; ;    (setenv "AnsysLMD_LICENSE_FILE" apdl-license-file)
;; ; (message "Set process environment variable AnsysLMD_LICENSE_FILE
;; to apdl-license-file")
;;     t)
;;    (lic1                 ; need this for -license-status
;;     (setq apdl-license-file lic1)
;;     (message "Set apdl-license-file from AnsysLMD_LICENSE_FILE")
;;     (message "apdl-license-file=%s" lic1)
;;     t)
;;    (lic2
;;     (setq apdl-license-file lic2)
;;     (message "Set apdl-license-file from MD_LICENSE_FILE")
;;     (message "apdl-license-file=%s" lic2)
;;     t)
;;    (t
;;     nil))))


(defun apdl-mark-paragraph (&optional arg allow-extend)
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
  (cond ((and allow-extend  ; we already called this function
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (forward-paragraph arg))
        ((and (bolp) (eolp))      ; we are in an empty line
         (push-mark nil t t)
         (forward-paragraph arg))
        (t    ; we are within a paragraph
         (backward-paragraph arg)
         (push-mark nil t t)
         (forward-paragraph arg))))

(defun apdl-mode-version ()
  "Display the APDL-Mode version numbering scheme.
Together with the APDL-Mode update date and the Ansys version on
which the keyword- and completion system is based upon."
  (interactive)
  (message "APDL-Mode version: %s (%s) based on Ansys %s"
           apdl-mode-version
	   apdl-mode-update
           apdl-ansys-version))

;;  ATTENTION: reloading seems to be critical, when there are multiple
;;  entries in `load-path' for APDL-Mode!
(defun apdl-reload-apdl-mode ()
  "Reload APDL mayor mode for debugging purposes.
Clear (unload) all mode definitions, if apdl-mode is active, load
the necessary code and call `apdl-mode'."
  (interactive)
  (progn
    (when (featurep 'apdl-mode)
      (unload-feature 'apdl-mode)
      (unload-feature 'apdl-initialise)
      (unload-feature 'apdl-keyword)
      (unload-feature 'apdl-process)
      (unload-feature 'apdl-template)
      (unload-feature 'apdl-wb-template))
;;    (load "apdl-config") ; don't rely on apdl-config!
    (load "apdl-mode") ; either load .elc or .el
    (apdl-mode)
    (message "APDL-Mode %s based on Ansys %s reloaded"
             apdl-mode-version apdl-ansys-version)))

(defun apdl-show-paren-mode ()          ; _C
  "Switch on minor mode function `show-paren-mode'.
The Matching parenthesis is highlighted."
  (show-paren-mode 1))

;; (defun apdl-ruler-mode ()               ; _C
;;   "Toggle display of ruler in header line.
;; This is the minor mode `ruler-mode'."
;;   (ruler-mode))

(defun apdl-font-lock-mode ()
  "Switch on function `font-lock-mode'.
Font Lock is also known as \"syntax highlighting\"."
  (unless font-lock-mode
    (font-lock-mode 1)))

(defun apdl-outline-minor-mode ()
  "Switch on mode function `outline-minor-mode'.
Editing with selective display."
  (outline-minor-mode 1))

(defun apdl-auto-insert-mode ()
  "Switch on mode function `auto-insert-mode'.
Automatic template insertion is enabled"
  (auto-insert-mode 1))

(defun apdl-insert-pi ()
  "Insert the code \"Pi = acos(-1)\" = 3.141.. at point.
Indent the current line according to the context."
  (interactive)
  (insert "Pi = acos(-1) ! 3.14159265359")
  (indent-according-to-mode)
  (newline-and-indent))

(defun apdl-column-ruler ()
  "Insert a temporary column ruler above the current line.
The insertion remains until the next keystroke.  The key typed is
inserted or evaluated unless it is the SPC key."
  (interactive)
  (save-excursion
    (momentary-string-display
     (if apdl-ruler-wide-flag
         apdl-column-ruler-wide
       apdl-column-ruler-narrow)
     (line-beginning-position))))

(defun apdl-position (position) ; FIXME: with `line-beginning-position' etc.
  ;; redundant function
  "Return the value of point at certain POSITIONs."
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line)) ; line-beginning-position
     ((eq position 'eol)  (end-of-line))       ; line-end-position
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     (t (error "Unknown buffer position requested: %s" position)))
    (point)))

(defun apdl-close-block () ; FIXME: choices for *IF
  "Complete an APDL block command with the appropriate end keyword.
Insert the end keyword on a separate line.  An error is signaled
if no block to close is found.  For example the *IF command
represents only a proper block command when it is followed by a
THEN action label."
  (interactive "*")
  (let (bb-keyword str tmp)
    (condition-case nil ; more pertinent error message -TODO-
        (progn
          (save-excursion
            (apdl-up-block)
            (setq bb-keyword (buffer-substring-no-properties
                              (match-beginning 1) (match-end 1)))
            ;; for *IF we're getting more than one word (*IF,...,THEN)
            (setq tmp (compare-strings bb-keyword 0 nil
                                       "*IF" 0 nil 'ignore-case))
            (when (> tmp  2)
              (setq bb-keyword
                    (substring-no-properties
                     (buffer-substring-no-properties
                      (match-beginning 1) (match-end 1)) 0 3))))
          ;; prepare insertion of keyword
          (setq str (car (reverse                 ; FIXME: uncomplete, why?
                          ;; RESTRICTED: asssoc-string Emacs 21.4
                          (assoc-string bb-keyword
                                        apdl-block-match-alist 1))))
          ;; capitalise properly
          (let ((case-fold-search nil))
            (when (string-match
                   "\\([a-z].\\)\\|\\(\\*\\|/\\|~\\)[a-z]" bb-keyword)
              (setq str (downcase str))))
          (cond
           ((apdl-in-empty-line-p)
            ;; (delete-blank-lines) deletes in 23.1 an isolated empty line
            (insert str)
            (indent-according-to-mode))
           ((apdl-in-indentation-p)
            (beginning-of-line)
            (open-line 1)
            (insert str)
            (indent-according-to-mode)
            (forward-line 1)
            (indent-according-to-mode)
            (forward-line -1)
            (end-of-line))
           ((apdl-in-string-or-comment-p)
            (end-of-line)
            (newline)
            (insert str)
            (indent-according-to-mode))
           ((and (apdl-code-line-p)
                 (not (apdl-at-end-of-text-p)))
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
          (apdl-blink-matching-block)
          t)
      (error (message "Cannot find a proper block command to close")))))

;;; --- Command parameters and command completions ----
(defsubst apdl-count-commas ()
  "Return the number of commas in the line before point."
  (how-many "," (line-beginning-position) (point)))

(defun apdl-manage-overlay ( str)
  "Display or remove the command help overlay string STR.
Appying this function in the same line erases the help overlay.
The help overlay will be automatically removed after some time
interval."
  (interactive)
  (let (; (ho (overlay-start apdl-help-overlay))
        (lb (line-beginning-position)))
    (if apdl-timer
        (cancel-timer apdl-timer))
    (delete-overlay apdl-help-overlay)
    ;;      (setq apdl-help-overlay-str str)
    (move-overlay apdl-help-overlay lb lb)
    (overlay-put apdl-help-overlay 'before-string str)
    (setq apdl-timer
          (run-at-time
           apdl-parameter-help-duration nil
           (lambda ()
	     (when (overlayp apdl-help-overlay)
	       (delete-overlay apdl-help-overlay)))))))

(defun apdl-search-comma (str count)
  "Return the index of the COUNT's occurance of a comma in STR.
Return nil otherwise."
  (let ((index 0)
        (c 0))
    (while (and (> count c) (not (null index)))
      (setq index (string-match "," str (1+ index))
            c (1+ c)))
    index))

;;  function is used as a post-command-hook
;; (defun apdl-update-parameter-help (&optional a b c)
(defun apdl-update-parameter-help ()
  "Update parameter help counting according to the cursor position.
Check if we are in a help overlay and if the cursor position
changed.  Then call `apdl-show-command-parameters'."
  (let ((p (point))
        (lo (overlays-in (line-beginning-position)
                         (1- (line-beginning-position)))))
    (when (and (not (equal p apdl-parameter-help-position))
               (not (equal 1 p))    ; -TODO- not working in the first line
               (memq apdl-help-overlay lo))
      (setq apdl-parameter-help-position (point))
      (apdl-show-command-parameters 1))))

;; spec: preserve post-command-hook! => no errors
;; edit command while help overlay!
;; remove overlay and warn when too many commas!
(defun apdl-show-command-parameters (&optional ask-or-toggle)
  "Display an APDL command parameters help for the command near the cursor.
Show the command name and its parameters (if any) and in a
further line a brief description.  Count the number of parameters
and visualise at which parameter position the cursor currently
is.  This is done for the previous APDL command beginning, except
when point is at the command beginning at the indentation.  See
also the function `apdl-command-start' how the previous command
is found.  It displays also the parameters for commands in a
comment line.  With a prefix argument ASK-OR-TOGGLE of zero
switch off the command parameters highlighting, with an prefix
`C-u' or argument `4' (four) enquire a command name from the mini
buffer, the beginning command characters can be completed with
<TAB>."
  (interactive "p" )
  (let ((case-fold-search t) ; in case customised to nil
        (count 0)
        substr
        tmpstr
        start
        end
        length
        str)
    ;; enquire or search for a valid command name
    (cond ((= ask-or-toggle 0))		; do nothing
          ((= ask-or-toggle 4)
           (setq str (completing-read
                      "Type APDL keyword to get its short help: "
                      apdl-help-index)))
          ((apdl-in-comment-line-p)
           (save-excursion
             (back-to-indentation)
             (skip-chars-forward " !")
             (re-search-forward "[^[:space:]]\\w*\\>" nil t))
           (setq str (match-string-no-properties 0)))
          ((apdl-in-indentation-p)  ; we are possibly before a command
           ;; -TODO- strange bug, when cursor is in a line with only
           ;; -commas and no command follows in lines below, then
           ;; -match-string-no-properties bails out!?
           (save-excursion
             (re-search-forward "[^[:space:]]\\w*\\>" nil t)
             (setq str (match-string-no-properties 0))))
          ((unless (apdl-in-indentation-p)
             (save-excursion
               (apdl-command-start)
               (re-search-forward "[^[:space:]]\\w*\\>" nil t)
               (setq str (match-string-no-properties 0))))))
    ;; search, amend and display help string in overlay
    (if (= ask-or-toggle 0)
        (delete-overlay apdl-help-overlay)
      (catch 'foo
        (dolist (s apdl-dynamic-prompt)
          (when (and (string-match (concat "^" str) s) (not (string= "" s)))
            (setq length (length s))
            ;; creating additional row with comma counts
            (setq start (string-match "\n" s)) ; looking for the first line break
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
            (setq count (apdl-count-commas))
            (setq start (apdl-search-comma s count))
            (cond ((null start)
                   ;; (skip-chars-backward "[^,]")
                   (delete-overlay apdl-help-overlay)
                   (message "Too many commas, command has fewer arguments")
                   (throw 'foo nil))
                  ((= 0 start)
                   (setq start  (1+ (string-match "\n" s)))))
            (setq end (apdl-search-comma s (1+ count)))
            (unless end
              (setq end length))
            (add-text-properties start end '(face isearch-fail) s)
            (apdl-manage-overlay s)
            ;; break dolist when str is found and skip over error
            (throw 'foo nil)))
        (delete-overlay apdl-help-overlay)
        (message "\"%s\" not found in keyword list" str)))))

(defun apdl-check-capitalisation ( string)
  "Check case of APDL keyword STRING.
Return symbols capitalise, upcase and downcase."
  (interactive)
  ;; preferences: downcase, capitalize, upcase
  (cond
   ((string= string (downcase string)) 'downcase)
   ((string= string (capitalize string)) 'capitalize)
   ((string= string (upcase string)) 'upcase)
   (t 'downcase)))

(defun apdl-complete-symbol ()
  "Perform a completion on APDL keywords preceding the cursor.
Complete the character(s) to APDL's reserved words, functions
and element names, otherwise throw an error.  When the keyword or
the completed character(s) represent a unique APDL keyword
indicate this fact with a message.  When the completion is not
unique or only partial show the other possible completions in a
temporary completion buffer, from which the completions might be
chosen with the mouse.  You might remove the *APDL completion*
buffer with the SPACE key."
  ;; This code taken from lisp-complete-symbol
  (interactive "*")
  (let* ((buffer-name "*APDL-completion*")
         (completion-buffer (get-buffer-create buffer-name))
         (completion-window (get-buffer-window completion-buffer)))
    (if (and (eq last-command this-command)
             completion-window ; already window there?
             ;; window is visible
             (window-live-p completion-window))
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
                        completion-string apdl-completions))
           (completion-list (all-completions
                             completion-string apdl-completions))
           (cc (apdl-check-capitalisation completion-string)))

      (cond
       ;; completion not possible
       ((null completion)
        (message "\"%s\" can't be completed to an APDL symbol"
                 completion-string)
        (if completion-window                 ; bury completion buffer
            (save-selected-window
              (select-window completion-window)
              (bury-buffer)))
        (ding))

       ;; unique and upcased like in the -completions variable
       ((equal completion t)
        (message "\"%s\" is a unique APDL symbol."
                 completion-string)
        (kill-buffer completion-buffer))

       ;; unique or uniquely completable, case independent
       ((= (length completion-list) 1) ; uniqe
        (setq completion (funcall cc completion))
        (unless (string= completion completion-string)
          (setq completion (funcall cc completion))
          (delete-region beg end)
          (insert completion))
        ;; possibly move back into parens
        (skip-chars-backward ")" (1- (point)))
        (kill-buffer completion-buffer)
        (message "\"%s\" is a unique APDL symbol." completion))

       ;; complete or not, but not unique anyway
       (t
        (setq completion (funcall cc completion))
        (unless (string= completion completion-string)
          (delete-region beg end)
          (insert completion))
        (with-output-to-temp-buffer buffer-name
          (display-completion-list completion-list))
        (if (= (apply #'min (mapcar #'length completion-list))
               (length completion))
            ;; already a complete, valid symbol but fragment is further
            ;; completable
            (message
             ;; (concat "Complete APDL symbol.  Hit SPACE to remove the "
             (concat "Complete but not unique APDL symbol.  \
Hit SPACE to remove the "
                     buffer-name " buffer."))
          ;; not yet complete
          (message
           (concat "Incomplete APDL symbol.  Hit SPACE to remove the "
                   buffer-name " buffer.")))

        ;; mouse selections in the completion buffer?
        (let (key
              first)
          (if (progn
                (set-buffer (get-buffer completion-buffer))
                ;; we are temporarily in the completion buffer
                (setq key (read-key-sequence nil)
                      first (aref key 0)) ; first key of key sequence
                (and (consp first)                     ; is cons cell
                     (consp (event-start first))
                     (eq
                      (window-buffer (posn-window (event-start first)))
                      (get-buffer completion-buffer))
                     ;;        (eq (key-binding key) 'choose-completion)))
                     ;; (choose-completion first)
                     (eq (key-binding key) 'mouse-choose-completion))); <E23.2
              (choose-completion first)
            ;; (mouse-choose-completion first) ; outdated function
            (if (eq first ?\ )
                (kill-buffer completion-buffer)
              (setq unread-command-events
                    (listify-key-sequence key))))))))))

;;;; Electric characters & friends

(defun apdl-reindent-then-newline-and-indent ()
  ;; (&ptional non-matching) ; FIXME: docu
  "Reindent current APDL line, insert newline, and indent the new line.
If function `abbrev-mode' is on, expand the abbreviations first."
  (interactive "*") ; * means signal error if read-only
  (expand-abbrev)
  (apdl-blink-matching-block)
  (save-excursion
    (delete-region
     (point)
     (progn (skip-chars-backward " \t")
            (point))) ; move trailing whitespace
    (indent-according-to-mode))
  (insert "\n")
  (indent-according-to-mode))

(defun apdl-electric-space ()
  "Insert a space in APDL-Mode.
Maybe expand abbrevs and blink matching block open keywords.
Reindent the line if `apdl-auto-indent-flag' is non-nil."
  (interactive "*") ; error if read only
  (setq last-command-event ? )
  (cond ((and mark-active transient-mark-mode delete-selection-mode)
         (kill-region (point) (mark))
         (self-insert-command 1))
        ((and (apdl-not-in-string-or-comment-p)
              (not (apdl-in-indentation-p))
              (not (apdl-in-empty-line-p)))
         (indent-according-to-mode)
         (self-insert-command 1)
         (expand-abbrev)
         (apdl-blink-matching-block)
         (if (and apdl-auto-indent-flag
                  (save-excursion
                    (skip-syntax-backward " ")
                    (not (bolp))))
             (indent-according-to-mode)))
        (t
         (self-insert-command 1))))

(defun apdl-add-apdl-menu ()
  "Add an \"APDL\" entry to the Emacs menu bar."
  (require 'easymenu)
  (easy-menu-define apdl-task-menu-map apdl-mode-map
    "Menu keymap for APDL Tasks." apdl-task-menu)
  (easy-menu-define apdl-mode-menu-map apdl-mode-map
    "Menu keymap for APDL-Mode." apdl-mode-menu)
  (easy-menu-add apdl-task-menu-map apdl-mode-map)
  (easy-menu-add apdl-mode-menu-map apdl-mode-map))

(defun apdl-calculate-indent ()   ; FIXME: comment, fixed goal column,
  "Return appropriate indentation for the current APDL code line.
Returns an integer (the column to indent to) unless the line is a
comment line with fixed goal column.  In that case, returns a
list whose car is the column to indent to, and whose cdr is the
current indentation level."
  (let ((column 0)    ; column
        (keyword_c 0) ; for specified commands
        (comma_c nil) ; for default commands
        lep           ; line end predicate
        lbp)          ; line beginning predicate
    ;; --- first for the previous code line ---
    (save-excursion
      (when (zerop (apdl-previous-code-line 1)) ; there is a previous
					      ; code line
        (if (or (apdl-condensed-input-line-p)
                (and (apdl-in-indentation-p)
                     (not (apdl-default-command-p))))
            (back-to-indentation)
          (apdl-command-start)) ; skip to the beginning of a *msg and
				; default command
        (setq keyword_c (current-column))
        (cond
         ((looking-at apdl-block-begin-regexp)
	  ;; checking for then keywords in if
;          (when (looking-at "\\*if.*,\\s-*then") ; *if base1 or base2
            ;; must be THEN for being a block keyword
            (setq keyword_c (+ keyword_c apdl-block-offset)));)
         ((looking-at apdl-block-else-regexp)
          (setq keyword_c (+ keyword_c apdl-block-offset)))
         ((looking-at "[^\n,]") ; */ are also valid default commands 12.1
          (setq lep (line-end-position))
          (setq comma_c (re-search-forward "\\w+\\s-*" lep 'noerror))
          (when comma_c
            (setq lbp (line-beginning-position))
            (setq comma_c (- comma_c lbp))))
         ((looking-at ",") ; -TODO-: shouldn't be possible
          (setq lep (line-end-position))
          (setq comma_c (1- (re-search-forward
                             "," lep 'noerror))) ; excluding the comma
          (when comma_c
            (setq lbp (line-beginning-position))
            (setq comma_c (- comma_c lbp)))))))
    ;; --- now for the current code line ---
    (save-excursion
      (back-to-indentation)
      (if (apdl-first-line-p) ; we are at the first code line
          (setq column (current-column))
        (cond
         ((and (looking-at apdl-block-else-regexp)
               (apdl-not-in-string-or-comment-p))
          (setq column (- keyword_c apdl-block-offset)))
         ((and (looking-at apdl-block-end-regexp)
               (apdl-not-in-string-or-comment-p))
          (setq column (- keyword_c apdl-block-offset)))
         ((and (looking-at ",") ; APDL default command substitution
               (apdl-not-in-string-or-comment-p)) ; FIXME:for *msg lines etc.?
          (if comma_c
              (setq column comma_c)
            (setq column keyword_c)))
         ((and (looking-at "\\s<\\w") ; FIXME:? this is for "code
				      ; comments"
               (not (looking-at
                     ( concat
                       "\\(\\s<\\s<\\s-\\S<\\)\\|\\(\\^\\s<"
                       apdl-outline-string "+\\)")))
               (setq column comment-column)))
         (t
          (setq column keyword_c)))))
    (when (< column 0)
      (error "%s" "Can't deduce sensible column offset"))
    column))

(defun apdl-indent-line-function (&optional arg)
  "Indent current line in APDL coding style.
With optional ARG, use this as offset unless this line is a
comment with fixed goal column.  This function is saved in
`indent-line-function'."
  (interactive "*p")
  (unless arg (setq arg 1))
  (let ((icol (apdl-calculate-indent))
        (relpos (- (current-column) (current-indentation))))
    (if (listp icol) ; FIXME: -calculate-indent returns no list
        (setq icol (car icol))
      (setq icol (+ icol (1- arg))))
    (if (< icol 0)
        (error "Unmatched end keyword") ; FIXME: this is probably wrong
      (indent-line-to icol)
      (if (> relpos 0)
          (move-to-column (truncate (+ icol relpos)))))))

;;;; Electric characters & friends

(defun apdl-abbrev-start ()
  "Start entering an APDL abbreviation.
If Abbrev mode is turned on, typing ` (grave accent) followed by ? or
\\[help-command] lists all APDL abbrevs.  Any other key combination is
executed normally.
Note that all APDL-Mode abbrevs start with a grave accent."
  (interactive)
  ;;  (if (not abbrev-mode) ; FIXME: redundant with E22.?
  ;;      (self-insert-command 1)
  (let (c)
    (insert last-command-event)
    (if (or (eq (setq c (read-event)) ??)
            (eq c help-char))
        (list-abbrevs t)
      (setq unread-command-events (list c))))) ; )

;; ;; redefine function because of bug in Emacs 23.2 squashed in 23.3
;; (defun prepare-abbrev-list-buffer (&optional local)
;; "Temporary redefinition of internal Emacs function with the argument LOCAL."
;;   (let ((l-a-t-n  (abbrev-table-name local-abbrev-table)))
;;    (with-current-buffer (get-buffer-create "*Abbrevs*")
;;     (erase-buffer)
;;     (if local
;;         (insert-abbrev-table-description l-a-t-n t)
;;       (dolist (table abbrev-table-name-list)
;;         (insert-abbrev-table-description table t)))
;;     (goto-char (point-min))
;;     (set-buffer-modified-p nil)
;;     (edit-abbrevs-mode)
;;     (current-buffer))))

(defun apdl-indent-format-line ()
  "Break APDL line at point, continuing comment if within one.
If within code, insert the APDL continuation character `&'
before breaking the line.  If within a string, signal an error.
The new line is properly indented."
  (interactive "*")
  (delete-horizontal-space)
  (cond
   ((apdl-in-comment-p)
    (indent-new-comment-line))
   ((apdl-in-string-p) ; FIXME: there are no strings defined yet
    (error "Cannot split a code line inside a string"))
   ((apdl-in-format-construct-p)
    (insert " &")
    (apdl-reindent-then-newline-and-indent))
   (t
    (apdl-reindent-then-newline-and-indent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; --- Cursor movement ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apdl-default-command-end ()
  "Move cursor to the end of an APDL default command construct."
  (unless (apdl-default-command-p)
    (re-search-forward "^\\s-*,"))
  (while (apdl-default-command-p)
    (forward-line))
  (forward-line -1)
  (end-of-line))

(defun apdl-search-number-line (&optional dir)
  "Search forward for a line existing purely of numbers.
If not If direction DIR is negativ search backwards.  The default
for DIR is 1. When already in a number line just go to the line
beginning when DIR is < 1 and when DIR is > to the end.  When no
futher number line is in the file signal an error."
  (interactive "p")
  (unless dir (setq dir 1))
  (let ((re apdl-number-line-regexp))
    (unless (apdl-at-end-of-text-p)
      (beginning-of-line))
    (cond
     ((< dir 1)
      (while (progn
               (unless (re-search-backward re nil t)
                 (error "Can't find preceding number line"))
               (apdl-in-format-construct-p))))
     (t
      (while (progn
               (unless (re-search-forward re nil t)
                 (error "Cant't find subsequent number line"))
               (apdl-in-format-construct-p)))))))

(defun apdl-number-block-start ()
  "Move to the line beginning before a pure number block.
For example an APDL NBLOCK or EBLOCK typically found in
WorkBench APDL files.  If there is no code before a number block,
signal an error."
  (interactive)
  (when (or (apdl-in-indentation-p)
            (not (apdl-number-line-p)))
    (apdl-search-number-line -1))
  (while (apdl-number-line-p)
    (forward-line -1))
  (forward-line))

(defun apdl-number-block-end ()
  "Move to the end of a pure number block.
For example an APDL NBLOCK or EBLOCK typically found in
WorkBench APDL files."
  (interactive)
  (when (or (apdl-at-end-of-text-p)
            (not (apdl-number-line-p)))
    (apdl-search-number-line))
  (while (apdl-number-line-p)
    (forward-line))
  (forward-line -1)
  (end-of-line))

(defun apdl-next-code-line (&optional num)
  "Move NUM lines of APDL code forward, default for NUM is 1.
Skip past intermediate comment and empty lines."
  (interactive "p")
  (unless num (setq num 1))
  (unless (memq last-command '(next-line
                               previous-line
                               apdl-next-code-line
                               apdl-previous-code-line))
    (setq temporary-goal-column (current-column)))
  (cond ((eobp)
         (message "End of buffer"))
        (t
         (forward-line 1)
         (while (and (apdl-not-in-code-line-p)
                     (not (apdl-last-line-p)))
           (forward-line 1))
         ;; (forward-comment (buffer-size))
         ;; temporary-goal-column might be a cons cell since E23.2
         (move-to-column  (if (integerp temporary-goal-column)
                              (truncate temporary-goal-column)
                            (truncate (car temporary-goal-column))))
         (setq num (1- num))
         (when (and (not (apdl-last-line-p))
                    (/= num 0))
           (apdl-next-code-line num)))))


(defun apdl-previous-code-line (&optional num)
  "Move NUM lines of APDL code backward, default for NUM is 1.
Skip all empty - and comment lines and return the difference
between NUM and actually moved code lines.  Check if there is
previous a code line before the cursor."
  (interactive "p")
  (unless num (setq num 1))
  (unless (memq last-command '(next-line
                               previous-line
                               apdl-next-code-line
                               apdl-previous-code-line))
    (setq temporary-goal-column (current-column)))
  (let ((Diff num)
	(Goal num))
    (cond
     ((= num 0)
      (message "%s" "do nothing and return 0")
      num)
     ((apdl-first-line-p)
      (message "Already at first line.")
      num)
     ((save-excursion			; here we are not at bob
	(while (progn
		 (forward-line -1)
		 (and (apdl-not-in-code-line-p)
		      (not (apdl-first-line-p)))))
	;; we are either at the first line or in a code line or both
	(if (apdl-not-in-code-line-p)
	    nil	       ; first line without code
	  t))               ; we found a previous code line
      (while (progn
	       (forward-line -1)
	       (apdl-not-in-code-line-p)))
      (setq Goal (1- Goal)	   ; moved to first previous code line
	    Diff Goal)
      ;; move to goal column
      (move-to-column (if (integerp temporary-goal-column)
			  (truncate temporary-goal-column)
			(truncate (car temporary-goal-column))))
      ;; recursion for possibly further code
      (unless (= Goal 0)		; = : numerically equal
	(setq Diff (apdl-previous-code-line Goal)))
      ;; (if (= 1 (- num Diff))
      ;; 	  (message "Moved %d code line" (- num Diff))
      ;; 	(message "Moved %d code lines" (- num Diff)))
      Diff)
     (t 				; found no code line
     ;; (message "No previous code line found.") ; in all templates
      num))))

(defun apdl-back-to-format-command ()
  "Move cursor back to the beginning of a previous format command.
Signal an error when there is no format command."
  (interactive)
  (when (re-search-backward apdl-format-commands-regexp) ; signals error
    (back-to-indentation)))

(defun apdl-move-to-end-of-format-string ()
  "Move cursor to the end of an format command's format string."
  (when (apdl-in-format-command-line-p)
    (forward-line))
  (while (and (apdl-continuation-line-p)
              (= (forward-line 1) 0))) ; in case of wrong format at eof
  (move-end-of-line 1))

(defun apdl-move-before-comment()
  "Move cursor to the line's end of text (which is not commented out)."
  (beginning-of-line)
  (search-forward "!" (apdl-position 'eol) 1)
  (skip-chars-backward " \t!"))

(defun apdl-command-start (&optional num)
  "Move cursor to the beginning of the NUMth previous command or assignment.
Default for NUM is 1.  If in a comment or empty line, go to the
previous command or to the first line if no previous command is
there.  When on a condensed input line, go to previous `$'
statement or to the line's first command.  When in a format
command string move backward to the beginning of the respective
command.  When no APDL command is to be found signal an error.
When NUM is 0 move to the current code line indentation."
  (interactive "p")
  (unless num (setq num 1))
  (while (> num 0)
    (cond
     ((apdl-in-format-construct-p)
      (apdl-back-to-format-command)
      (setq num (1- num)))
     ((apdl-number-line-p)
      (while (apdl-number-line-p)
        (forward-line -1))
      (end-of-line))
     ((apdl-default-command-p)
      (while (apdl-default-command-p)
        (forward-line -1))
      (end-of-line))
     ((and (not (apdl-code-line-p))            ; in empty line or comment
           (not (= num 0))
           (not (apdl-first-line-p)))
      (beginning-of-line)
      (forward-comment (-(buffer-size))))
     ((apdl-in-indentation-p)
      (if (apdl-first-line-p)
          (setq num -1)
        (forward-comment (-(buffer-size))))) ; skips also \n!
     ((apdl-condensed-input-line-p)
      (when (looking-back "\\$\\s-*" nil)  ; we are already before a $ sign
        ;; -TODO- speed things with LIMIT?
        (skip-chars-backward " \t$")) ; skip at or before the $ char
      (if (re-search-backward "\\$\\s-*" (apdl-position 'bol) t)
          (skip-chars-forward "$ \t")
        (back-to-indentation))
      (setq num (1- num)))
     (t
      (back-to-indentation)
      (setq num (1- num))))))

(defun apdl-command-end (&optional num)
  "Move to the end of the NUMth next APDL command or assignment statement.
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
     ((apdl-at-end-of-code-p)
      (if (apdl-last-line-p)
          (setq num -1)
        (forward-comment (buffer-size))))
     ((or (apdl-in-format-command-line-p)
          (apdl-in-format-construct-p)) ; not the format command line
      (apdl-move-to-end-of-format-string)
      (setq num (1- num)))
     ((apdl-number-line-p)
      (apdl-number-block-end)
      (setq num (1- num)))
     ((apdl-default-command-p)
      (apdl-default-command-end)
      (setq num (1- num)))
     ((and (not (apdl-code-line-p)) ; in empty line or comment
           (not (= num 0))
           (not (apdl-last-line-p)))
      (end-of-line)
      (forward-comment (-(buffer-size))))
     ((apdl-condensed-input-line-p)
      (when (looking-at "\\s-*\\$")    ; we are already before a $ sign
        (skip-chars-forward " \t$"))   ; skip at or before the $ char
      (if (re-search-forward "\\s-*\\$" (apdl-position 'eol) t)
          (skip-chars-backward " \t$")
        (end-of-line))
      (setq num (1- num)))
     (t
      (back-to-indentation)
      (while (not (apdl-at-end-of-code-p))
        (forward-char))
      (setq num (1- num))))))

(defun apdl-scan-blocks (count level-offset)
  "Scan from (point) COUNT balanced APDL begin-end blocks.
Return the position thus found.  COUNT may be negative.

If LEVEL-OFFSET is nonzero, the block level gets an offset of
LEVEL-OFFSET."
  (let ((min-level-offset (if (> level-offset 0) 0 level-offset)) ; level-offset
        ;; can become large (we are going deeper down block levels)
        ;; but never smaller than min-level-offset
        (inc (if (> count 0) 1 -1))
        (pt (point)))
    (save-excursion
      (while (/= count 0)
        (catch 'foo ; end the inner while loop
          (while (or (re-search-forward apdl-block-begin-or-end-regexp
                                        nil t inc) ; FIXME:it's not
						   ; working when
                     ;; in a block regexp
                     (when (/= level-offset 0)
                       (error "Can't reach specified block level")))
            (unless (apdl-in-string-or-comment-p)
              (cond
               ((match-end 1) (setq level-offset
                                    (+ level-offset inc))) ; begin-block-keywords
               ((match-end 2) (setq level-offset
                                    (- level-offset inc)))) ; end-block-keywords
              (when (< level-offset min-level-offset)
                (if (< min-level-offset 0)
                    (error "Reached minimum block level: Can't go deeper")
                  (error "Reached maximum block level: Can't go further up")))
              (when (= level-offset 0) (throw 'foo nil)))))
        (setq count (- count inc)))
      (if (= pt (point))
          (error "No block keyword found")
        (point)))))

(defun apdl-mark-block ()     ; FIXME: this is not consistent with
  ;; mark-paragraph, cursor below construct
  "Mark current block level.
Either inside of block structure or in the line of a block beginning
keyword."
  (interactive)
  ;; when we are in a line before a block beginning keyword
  (if (save-excursion
        (back-to-indentation)
        (looking-at apdl-block-begin-regexp))
      (progn
        (move-beginning-of-line nil)
        (set-mark-command nil))
    (progn
      (apdl-up-block)
      (move-beginning-of-line nil)
      (set-mark-command nil)))
  (apdl-skip-block-forward)
  (forward-line))

(defun apdl-skip-block-forward (&optional arg)
  ;; &optional: default arg always
  ;; nil when non interactively
  ;; called
  "Move forward across one balanced begin- and end-block keyword pair.
With argument, do it that many times.  Negative ARG means move
backward across |ARG| blocks."
  (interactive "p") ; "p" defaults to 1 only when interactive
  (unless arg (setq arg 1))
  (goto-char (or (apdl-scan-blocks arg 0)
                 (if (> arg 0)
                     (message "No %d block end(s) after cursor position" arg)
                   (message "No %d block start(s) before cursor position"
                            arg)))))

(defun apdl-skip-block-backwards (&optional arg)
  "Move backward across one balanced APDL begin-end block.
With argument, do it that many times.
Negative ARG means move forward across |ARG| blocks."
  (interactive "p")
  (unless arg (setq arg 1))
  (apdl-skip-block-forward (- arg)))

(defun apdl-next-block-end (&optional count)
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
                 (setq c (re-search-forward apdl-block-end-regexp nil t dir))
                 (unless c
                   (if (< dir 0)
                       (error "No previous block end(s), %d is(are) missing"
                              (- n i))
                     (error "No further block end(s), %d is(are) missing"
                            (- n i))))
                 (apdl-in-string-or-comment-p)))))
    (goto-char c)))

(defun apdl-previous-block-start-and-conditional (&optional count)
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
          apdl-block-begin-regexp
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
                 (apdl-in-string-or-comment-p)))))
    (goto-char c)))

(defun apdl-down-block (&optional down-level)
  "Move forward down one begin-end block level of APDL code.
Position cursor behind the beginning keyword of the respective
block.  With argument DOWN-LEVEL, do this for that many levels.
A negative argument means move backwards up DOWN-LEVEL
levels (see `apdl-up-block')."
  (interactive "p")
  (unless down-level (setq down-level 1))
  (let ((inc (if (> down-level 0) 1 -1)))
    ;; we have to take care whether cursor sits on a beginning keyword
    (while (/= down-level 0)
      (goto-char (apdl-scan-blocks inc (- inc)))
      (setq down-level (- down-level inc)))))

(defun apdl-up-block (&optional depth)
  "Move backwards up one begin-end block level of APDL code.
Position cursor before the beginning keyword of the respective
block.  With argument DEPTH, do this for that many levels.  A
negative argument DEPTH means move forward down DEPTH levels (see
`apdl-down-block')."
  (interactive "p")
  (unless depth (setq depth 1))
  (apdl-down-block (- depth)))

(defun apdl-blink-matching-block ()
  "Blink the matching APDL begin block keyword.
If point is right after an APDL else or end type block keyword,
move cursor momentarily to the corresponding begin keyword.
Signal an error if the keywords are incompatible."
  (interactive)
  (when apdl-blink-matching-block-flag
    (let (bb-keyword bb-arg eb-keyword pos eol)
      (when
          (and
           (apdl-not-in-string-or-comment-p)
           (looking-at "\\>")
           (save-excursion
             (skip-syntax-backward "w") ; FIXME: is * in word syntax?
             (looking-at apdl-block-else-or-end-regexp))) ; FIXME: and
							  ; otherwise?
        (save-excursion
          (cond
           ((match-end 1)                                 ; else keyword
            (setq eb-keyword
                  (buffer-substring-no-properties
                   (match-beginning 1) (match-end 1)))
            (apdl-up-block))
           ((match-end 2)                                 ; end keyword
            (setq eb-keyword
                  (buffer-substring-no-properties
                   (match-beginning 2) (match-end 2)))
            (apdl-skip-block-backwards)))
          (forward-word)
          (setq pos (point)
                bb-keyword (buffer-substring-no-properties
                            (match-beginning 0) pos)
                ;; pos (1+ pos); FIXME: bb-arg is eating commas
                eol (apdl-position 'eol)
                bb-arg (save-excursion
                         (save-restriction
                           (goto-char pos)
                           (while (and (skip-syntax-forward "^<" eol)
                                       (apdl-in-string-p)
                                       (not (forward-char 1))))
                           (skip-syntax-backward " ")
                           (buffer-substring-no-properties pos (point)))))
          (if (member-ignore-case
               eb-keyword (cdr (assoc-string
                                bb-keyword apdl-block-match-alist 1)))
              (progn
                (message "`%s' matches `%s%s'" eb-keyword bb-keyword bb-arg)
                (when (pos-visible-in-window-p)
                  (sit-for apdl-blink-matching-delay)))
            (error "Block keywords `%s' and `%s' do not match"
                   bb-keyword eb-keyword)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hiding regions heavily borrowed from M. Dahls hide-region.el

(defun apdl-hide-region ()
  "Hide a region by making an invisible overlay over it.
Put some markers (`apdl-hide-region-before-string',
`apdl-hide-region-after-string') around and save the overlay in
the `apdl-hide-region-overlays' \"overlay ring\"."
  (interactive)
  (let ((new-overlay (make-overlay (mark) (point))))
    (push new-overlay apdl-hide-region-overlays)
    (overlay-put new-overlay 'invisible t)
    (overlay-put new-overlay 'intangible t)
    (overlay-put new-overlay 'before-string
                 (if apdl-hide-region-propertize-markers
                     (propertize apdl-hide-region-before-string
                                 'font-lock-face 'region)
                   apdl-hide-region-before-string))
    (overlay-put new-overlay 'after-string
                 (if apdl-hide-region-propertize-markers
                     (propertize apdl-hide-region-after-string
                                 'font-lock-face 'region)
                   apdl-hide-region-after-string))))

(defun apdl-hide-number-blocks ()
  "Hide all number blocks (nblock, eblocks, cmblocks) in file.
These constructs appear in WorkBench created solver input files."
  (interactive)
  (let ((p-orig (point))
        p1
        p2
        lines)
    (message "Hiding number blocks ...")
    (goto-char (point-min))
    (while (re-search-forward "nblock\\|eblock\\|cmblock" nil t)
      (setq p1 (point))
      (re-search-forward "^-1\\|^cmsel\\|^d" nil nil)
      (setq p2 (point)
            lines (count-lines p1 p2))
      (when (> lines 5)  ; only hide blocks if larger then 5 lines
        (goto-char p1)
        (forward-line 3) ; show one line of numbers before markers
        (set-mark (point))
        (goto-char p2)
        (forward-line -2) ; show one line of numbers after markers
        (end-of-line)
        (apdl-hide-region)))
    (goto-char p-orig)))

(defun apdl-unhide-number-blocks ()
  "Unhide all hidden regions in the current buffer."
  (interactive)
  (while apdl-hide-region-overlays
    (if (car apdl-hide-region-overlays)
        (progn
          (delete-overlay (car apdl-hide-region-overlays))
          (setq apdl-hide-region-overlays (cdr apdl-hide-region-overlays))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- Abbreviations ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless apdl-mode-abbrev-table
  (let ((ac abbrevs-changed)) ; inhibit offer to save .abbrev_defs
    (define-abbrev-table 'apdl-mode-abbrev-table ())
    (define-abbrev apdl-mode-abbrev-table
      "`1" "finish\n/clear\n!y\n"  ) ; the first 1 one
    (define-abbrev apdl-mode-abbrev-table "`i" ""      'apdl_if)
    (define-abbrev apdl-mode-abbrev-table "`d" ""      'apdl_do)
    (define-abbrev apdl-mode-abbrev-table
      "`p" "*dim,Dir,string,248 ! maximum of 248 characters!\nDir(1) = \
 '/HOME/uidg1626/development/report/ej/95ks91leg0/'\n\
/syp,ls,Dir(1)\n") ; for path
    (define-abbrev apdl-mode-abbrev-table "`p" "" 'apdl-insert-pi)
    (define-abbrev apdl-mode-abbrev-table "`if" "" 'apdl-if)
    (define-abbrev apdl-mode-abbrev-table "`ie" "" 'apdl-if-then)
    (define-abbrev apdl-mode-abbrev-table "`do" "" 'apdl-do)
    (define-abbrev apdl-mode-abbrev-table "`e"
      "/eof ----------------------------------------\n"
      (lambda () (indent-according-to-mode)))
    (define-abbrev apdl-mode-abbrev-table
      "`c" "!! ========================================\n"
      (lambda () (indent-according-to-mode)))
    (define-abbrev apdl-mode-abbrev-table "`t" "/title,"
      (lambda () (indent-according-to-mode)))
    (setq abbrevs-changed ac))) ; reset `abbrevs-changed' to previous
				; state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- Bug reporting ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun apdl-submit-bug-report ()                 ; from Octave
  "Open an Emacs mail buffer with an APDL-Mode bug report."
  (interactive)
  (require 'reporter)
  (let ((salutation))
    (when (y-or-n-p "Do you want to write a bug report? ")
      (setq salutation
            "Please describe briefly what your problem is and
  which actions triggered the bug.  A self contained,
  reproducible test case would be advantageous.")
      (reporter-submit-bug-report
       apdl-maintainer-address
       "APDL-Mode" ; becomes prefix for the subject line
       (list
        ;; constants
        'apdl-mode-version
	'apdl-mode-update
	'apdl-mode-install-directory
	;; 'apdl-ansys-version ; is in -ansys-install-directory included
	;; variables
	'apdl-initialised-flag
	'apdl-current-ansys-version
        'apdl-is-unix-system-flag
        ;; defcustoms
	'apdl-parameter-help-duration
	'apdl-hide-region-before-string
	'apdl-hide-region-after-string
	'apdl-hide-region-propertize-markers
	'apdl-highlighting-level
	'apdl-dynamic-highlighting-flag
	'apdl-indicate-empty-lines-flag
	'apdl-comment-padding
	'apdl-comment-add
	'apdl-code-comment-column
	'apdl-auto-indent-flag
	'apdl-indent-comment-suffix
	'apdl-ruler-wide-flag
	'apdl-require-spaces-flag
	'apdl-blink-matching-block-flag
	'apdl-blink-matching-delay
	'apdl-block-offset
	'apdl-outline-string
	'apdl-mode-hook
	'apdl-align-rules-list
	'apdl-license-occur-regexp
	'apdl-job
	'apdl-license-categories
	'apdl-license
	'apdl-no-of-processors
	'apdl-blink-delay
	'apdl-blink-region-flag
	'apdl-username			; 20.4.0
	'apdl-ansys-install-directory
	'apdl-ansys-program
	'apdl-ansys-launcher
	'apdl-ansys-wb
	'apdl-ansys-help-program
	'apdl-ansys-help-path
	'apdl-lmutil-program
	'apdl-license-file
	'apdl-ansysli-servers
	'apdl-wb-custom-template-directory ; 20.4.0
	)
       nil
       nil
       salutation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- dynamic highlighting ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---- Restrictions ----
;; Variables or 'parameters' in APDL parlance:
;; 1.) Begin with a letter
;; 2.) Contain only letters, numbers and the underscore '_'
;; 3.) Have no more than 32 characters
;; 4.) Any variable ending with an underscore are *not* shown
;;     with the *STATUS command
;; 5.) The maximum number of parameter (< 5000) is retrieved by
;;     *GET,par,PARM,,MAX
;; 6.) (A<B) returns the value of A when A is less than B, B otherwise!

(defun apdl-asterisk-regexp(string)
  "Regexp STRING representing an asterix."
  (when (= (elt string 0) ?*)
    (setq string (concat "\\" string)))
  string)

(defun apdl-string-length-predicate (s1 s2)
  "Return t when string S1 is larger then S2."
  (< (length s1) (length s2)))

(defun apdl-find-duplicate-p (entry list)
  "Return t when ENTRY is already a member of LIST."
  (let ((l list) p)
    (while (and (not p) l)
      (setq p (assoc-string entry (car l) 'ignore-case))
      (pop l))
    p))

;; with pseudo arguments _a _b _c in case of usage as
;; after-change-function.  Underscore before variable removes compiler
;; warning about unused lexical variables.
(defun apdl-find-user-variables (&optional _a _b _c)
  ;; (defun apdl-find-user-variables ()
  ;; fontification is not working!? -TODO-
  "Find all user variables in the current buffer.
Pre-process the findings into the variables `apdl-user-variables'
and `apdl-user-variable-regexp' for subsequent fontifications.
Added pseudo arguments _A _B _C."
  ;; RESTRICTED: line-number-at-pos was introduced after Emacs 21.4
  (interactive)
  (save-excursion
    (save-match-data
      (let (res var com)                 ; Start with APDL *USE vars
        (setq apdl-user-variables ())

        (dolist (command apdl-variable-defining-commands)
          (setq com (car command))
          (goto-char (point-min))

          (while (re-search-forward
                  ;; take care of variables clashing with command names
                  (concat "\\(?:^\\|$\\)\\s-*" com
                          "\\s-*,\\s-*\\([[:alpha:]][[:alnum:]_]\\{0,31\\}\\)")
                  nil t)
            (setq var (match-string-no-properties 1))
            ;; format line, comment, message, C***
            (unless (or (apdl-in-string-or-comment-p)
                        (apdl-in-string-command-line-p)
                        (apdl-in-format-construct-p)
                        (apdl-find-duplicate-p var apdl-user-variables))
              (add-to-list 'apdl-user-variables
                           ;; (match-beginning 1)
                           (list var (line-number-at-pos))))))

        ;; APDL = assignment
        (goto-char (point-min))
        (while (re-search-forward
                ;; search for reserved variables as well
                "\\_<\\([[:alpha:]_][[:alnum:]_]\\{0,31\\}\\)\\s-*="
                nil t)
          (setq var (match-string-no-properties 1))
          (unless
              (or (apdl-in-string-or-comment-p)
                  (apdl-in-string-command-line-p)
                  (apdl-in-format-construct-p)
                  (apdl-find-duplicate-p var apdl-user-variables))
            (add-to-list 'apdl-user-variables
                         (list var (line-number-at-pos)))))
        ;; we must sort the variables according to their occurance
        ;; for the display
        (setq apdl-user-variables
              (sort apdl-user-variables
                    (if (version< "24" emacs-version)
                        (lambda (arg1 arg2)
			  (< (cadr arg1) (cadr arg2)))
                      (lambda (arg1 arg2)
			(< (cadr arg1) (cadr arg2))))))
        ;; make the regexp for fontification
        (setq res (mapcar #'car apdl-user-variables)
              res (regexp-opt res 'symbols) ; words inhibits variables
					; ending in _!
              apdl-user-variable-regexp res)))))

;; in comments: ok
;; in * comments: apdl-in-asterisk-comment-p
;; clashes with command names
;; in format strings without % chars
(defun apdl-search-variable (variable limit)
  "Search for the variable VARIABLE up to limit LIMIT.
This function is used as a highlighting function."
  (save-excursion
    (while (progn
             (re-search-forward variable limit t)
             (or (apdl-in-asterisk-comment-p)
                 (and (or (apdl-in-format-construct-p)
                          (apdl-in-string-command-line-p)
                          (not (looking-at "%")))))))))

(defun apdl-highlight-variable (limit)
  "Find user variables from (point) to position LIMIT for highlighting.
Use variable `apdl-user-variable-regexp'."
  (let ((r apdl-user-variable-regexp))
    (re-search-forward r limit t)))

(defun apdl-higlight-procent-and-ampersand (limit)
  "Find procent and ampersand up to position LIMIT for highlighting."
  (let (res )
    (while
        (progn
          (setq res (re-search-forward "%\\|&\\s-*$" limit t))
          ;; don't highlight in comments
          (and res (apdl-in-comment-p))))
    res))

(defun apdl-copy-buffer-line (buffer line-no)
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

(defun apdl-display-variables (arg)
  "Displays APDL variable assignments in the current buffer.
Together with the corresponding line number N (type \\[goto-line]
N for skipping to line N or place the cursor over the number and
`C-u' \\[goto-line] takes the number automatically).  With a prefix
argument ARG, the function evaluates the variable at point."
  (interactive "P")
  (cond
   (arg
    (unless (or (apdl-process-running-p) apdl-classics-flag)
      (error "No MAPDL process running"))
    (let* (
           (pt (point))
           (re "\s_[[:word:]]*")
           (lbp (line-beginning-position))
           (str (buffer-substring-no-properties
                 (save-excursion (+ pt (skip-chars-backward re lbp)))
                 (save-excursion (+ pt (skip-chars-forward re))))))
      (if apdl-classics-flag
          (progn
            (kill-new (concat "*status," str "\n"))
            (apdl-send-to-classics))
        (comint-send-string (get-process apdl-process-name)
                            (concat "*status," str "\n"))
        (display-buffer (concat "*" apdl-process-name "*") 'other-window))
      (message  (concat "Enquiring status for variable: " str))))
   (t
    (apdl-find-user-variables)
    (let* ((current-buffer (buffer-name))
           (buffer-name "*APDL-variables*")
           (variable-buffer (get-buffer-create buffer-name))
           str old-num com
           (num 0))
      (set-buffer variable-buffer)
      ;; make buffer writable
      (read-only-mode -1)
      (kill-region (point-min) (point-max))
      ;; insert header
      (insert
       (propertize
        (concat "-*- APDL variables of buffer " current-buffer " -*-\n")
        'face 'match))
      (insert (propertize "Line  | Definition\n" 'mouse-face
                          'highlight 'face 'bold))
      ;; insert variable lines
      (dolist (command apdl-user-variables)
        (setq old-num num
              num (cadr command)                 ; cadr same as nth 1
              com (apdl-copy-buffer-line current-buffer num)
              str (concat
                   (propertize (format "%5d | " num)
                               'mouse-face 'highlight 'face 'bold)
                   com "\n"))
        (unless (= num old-num)
          (insert str)))
      (goto-char (point-min))
      ;; make buffer read-only
      (read-only-mode 1)
      (set-buffer current-buffer)
      (display-buffer buffer-name 'other-window)))))

(defun apdl-customise-ansys ()
  "Call the Emacs customisation facility for APDL-Mode."
  (interactive)
  (customize-group "APDL"))

(defun apdl-delete-other-window (&optional win)
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

(provide 'apdl-mode)

;;; apdl-mode.el ends here

;; Local Variables:
;; minor-mode: flycheck
;; indicate-empty-lines: t
;; show-trailing-whitespace: t
;; time-stamp-format: "%:y-%02m-%02d"
;; time-stamp-active: t
;; End:
