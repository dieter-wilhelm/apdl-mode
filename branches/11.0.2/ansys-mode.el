;;; ansys-.el --- Emacs support for working with Ansys FEA.

;; Time-stamp: "2009-08-31 14:39:34 uidg1626"

;; Copyright (C) 2006 - 2009  H. Dieter Wilhelm

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Maintainer: H. Dieter Wilhelm
;; Created: 2006-02
;; Version: 11.0.2
;; Keywords: Languages, Convenience

;; This file contains code from a dated octave-mod.el:
;; Copyright (C) 1997 Free Software Foundation, Inc.  Author: Kurt
;; Hornik <Kurt.Hornik@wu-wien.ac.at> Author: John Eaton
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
;; Element Analysis) program Ansys (http://www.ansys.com) under Unix
;; and Window systems.  It defines 'Ansys mode', a major mode for
;; viewing, writing and navigating in APDL (Ansys Parametric Design
;; Language) files as well as providing managing and communication
;; capabilities for various Ansys solver processes.

;; The mode's capabilities are rather sophisticated but still the
;; documentation is targeted for Ansys users with little Emacs
;; experience.

;; == Documentation: ==

;; == Requirements ==
;; == Features ==
;; == Installation ==
;; == Usage ==
;; == History ==
;; == Resources ==
;; == Bugs and Problems ==
;; == ToDo ==

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; == Requirements ==

;; The code is based on Ansys version 11.0 and is written for GNU
;; Emacs 23.  It is tested with version 23.1 under XP and GNU/Linux.
;; The code won't run with Emacs 21.4 and is not (yet) consciously
;; targeted for XEmacs, there might be problems.  Please visit
;; ftp://ftp.gnu.org/pub/gnu/emacs/windows/ for an official,
;; precompiled Windows versions of GNU Emacs.  You can unpack Emacs in
;; any directory.  Optionally you can run the program addpm.exe
;; (located in the bin directory) to add an Emacs entry to the Windows
;; Start menu (please refer to the README.W32 file).

;; The Ansys solver communication capabilities are mainly restricted
;; to UNIX systems.

;; The experimental user variable highlighting is currently only
;; implemented for files with a '.mac' extension.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; == Features ==

;; * Process management (viewing error files, license status, etc.)

;; * Solver control and comunication (UNIX only)

;; * Command syntax help (similar but more verbose then the Ansys
;;   dynamic prompt)

;; * Keyword (case-sensitive) completion of commands, elements, get-
;;   and parametric-functions (nearly 1900 Ansys symbols).

;; * Auto-indentation of looping and conditional blocks

;; * Closing of open blocks with insertion of the appropriate end
;;   keyword (case-sensitive)

;; * Code navigation,extended keyboard shortcuts for code lines,
;; * number blocks, *DO,*IF, DOWHILE, *CREATE block etc.

;; * Sophisticated highlighting (optionally also for user variables)

;; * Displays summary for all definitions (*GET, *DIM, *SET, = and
;; * *DO) of APDL variables.

;; * Use of the Emacs abbreviation facility for block templates

;; * Convenient comment handling, commenting out of whole paragraphs

;; * Outlining (hiding and navigating) of code sections with Emacs'
;; * outline-minor-mode

;; * Auto-insertion of header and code templates into new APDL files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; == Installation ==

;; === Short instructions ===

;; put the following paragraph into your .emacs file.

;;     (autoload 'ansys-mode "ansys-mode" nil t)
;;        ;assuming ansys-mode.el directory is in your Emacs load-path!
;;     (add-to-list 'auto-mode-alist '("\\.mac\\'" . ansys-mode))
;;        ;assuming your APDL files have the extension .mac.
;;     (auto-insert-mode 1) ;needed for inserting Ansys skeleton
;;     (setq auto-insert-query t) ;insert skeleton only after request
;;     (add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton]))
;;     ;(setq ansys-dynamic-highlighting-flag t) ;helpful but experimental

;; === Verbose instructions ===

;; * The most direct way of using ansys-mode.el is storing the file
;;   somewhere on disk and loading the included definitions from there
;;   with the standard Emacs command `load-file' i. e.  type "M-x
;;   load-file RET", "M-x" means typing first the "Alt"-key and
;;   then the "x"-key simultaneously.  This gives you a prompt where
;;   you can type `load-file' followed by the "RET" key to conclude
;;   the command.  Then Emacs will prompt you for a file location.

;;   If you feel unsure about these concepts I urgently recommend to
;;   you strolling through the build-in tutorial of GNU Emacs (you'll
;;   find it in the help menu of Emacs), it doesn't take much time and
;;   the investment will also help you speeding up your general
;;   editing tasks.

;;   When the definitions are loaded into memory you must type "M-x
;;   ansys-mode RET" for the files of interest to activate the mode.

;; * When it becomes annoying loading the Lisp file 'ansys-mode.el'
;;   every time you are starting Emacs anew, you should specify the
;;   path for this file in your '~/.emacs' file (the configuration
;;   file '.emacs' of GNU Emacs in your home directory '~\') and
;;   auto-load the function `ansys-mode':

;;      (add-to-list 'load-path
;;                   "c:\\your\\directory\\where\\ansys-mode.el\\recides")
;;      (autoload 'ansys-mode "ansys-mode" "Activate Ansys mode." 'interactive)

;;   So far you only have to type "M-x ansys-mode RET" for every
;;   interesting APDL file.  With the following code you are also able
;;   to use the certain Ansys related functions without the need of
;;   previously having called Ansys mode.  When in your Emacs session
;;   there is already a file under Ansys mode, then they are available
;;   anyway.

;;      (autoload 'ansys-abort-file "ansys-mode" "Activate Ansys abort file function." 'interactive)
;;      (autoload 'ansys-display-error-file "ansys-mode" "Activate Ansys display error file function." 'interactive)
;;      (autoload 'ansys-start-ansys-help "ansys-mode" "Activate Ansys start help function." 'interactive)
;;      (autoload 'ansys-license-status "ansys-mode" "Activate Ansys license status function." 'interactive)

;; * When you intend to use the mode automatically, e.g. for all files
;;   you are opening with the extension '.mac' and '.inp' (WorkBench
;;   default solver input file suffix), add the following to your
;;   '.emacs' file:

;;      (add-to-list 'auto-mode-alist '("\\.mac$" . ansys-mode))
;;      (add-to-list 'auto-mode-alist '("\\.inp$" . ansys-mode))

;;   The suffix below belongs to the "anys neutral file" export format
;;   which contains also an APDL header and footer text

;;      (add-to-list 'auto-mode-alist '("\\.anf$" . ansys-mode))

;; * In case you also want to enjoy the auto insertion feature, which
;;   puts (optionally) some predefined body of Ansys commands to every
;;   new file--only those opened with `ansys-mode', of course--append
;;   the following to '.emacs':

;;      (setq auto-insert-mode 1)
;;      (setq auto-insert-query t) ;insert only after request
;;      (add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; == Usage ==

;; * When not the Ansys mode is not yet started, please invoke the
;;   mode function `ansys-mode' with typing "M-x" (the "Alt" key and
;;   the "x" key simultaneously) then you end up in the Emacs
;;   minibuffer prompt, type "ansys-mode" (you could try the "TAB" key
;;   for auto-completion) and conclude your input with the "RET" key.
;;   Then type "C-h m" i. e. the CTRL key together with the "h" key
;;   and then the "m" key, which gives you an Emacs buffer with basic
;;   usage guides.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; == Acknowledgements ==

;; (In no particular order):
;; rms,
;; Holger Sparr,
;; Eli Zaretzki,
;; Markus Triska,
;; Mathias Dahl,

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;: (the following line is for the checkdoc command)
;;; History:

;; == History: ==

;; * The version scheme is a mixture of the used Ansys version (11.0)
;;   and the version of the Ansys mode (2).

;; === Version 11.0.2 ===

;; cleared "buffer has no process" bug when killing an ansys-mode buffer

;; === Version 11.0.1 ===

;; * Feature freeze: 2009

;; * Submitting interactively Ansys commands (via minibuffer query not
;;   only written in a macro file) to the solver process,
;;   ansys-query-ansys-command (C-c C-q)

;; === ansys-mode.el version 11.0.1 in comparison to its predecessor
;;   ansys-mod.el: ===

;; * New: Provides Ansys command parameter- and syntax help.

;; * New: Offers Ansys process management: Acquiring license server
;;   information in a buffer Starting and stopping asynchronously
;;   Ansys runs.  Sending code lines to running Ansys process (sort of
;;   code debugging facility) and getting the output into a buffer.

;; * New: Experimental highlighting of user defined variables.
;;   Redefinition and clearing of variables is not yet taken into
;;   account

;; * New: Emacs customisation facility is available for the new Ansys
;;   mode group.

;; * New: Emacs outline-minor-mode is readily available in conjunction
;;   with this mode.

;; * Completions of Ansys commands are now case-sensitive, with
;;   additional completion of function and element names.

;; * Previously defined skeletons are fully functional now, new ones
;;   are added and enabled with the abbreviation and auto-load
;;   facilities of Emacs 22.

;; * Ansys' interpreter's disregard of any capitalisation is now fully
;;   taken into account in the highlighting.

;; * The apostrophe "'" is now assigned as the Ansys string and the
;;   value of character parameters delimiter and not wrongly """;
;;   the strings are fontified accordingly.

;; * The dollar sign "$" is now emphasised as the Ansys condensed
;;   input character (multiple Ansys commands in one line).

;; * The colon ":" is now emphasised as the Ansys colon do loop
;;   character ("(x:y:z)" means from x to y, in z steps, z is equal to
;;   one as default).  For example: "n,(1:6),(2:18:2)" runs 6 loops.
;;   Colon loops are working also with real values: k,,(2.5:3:0.1) and
;;   with array parameters: k,,A(1:100), but the latter is an
;;   undocumented feature. Since ansys 11.0 the colon looping is also
;;   working with *GET functions (example: A(1:5)=NX(1:5))). A ":"
;;   indicates also a beginning of a label for the *GO and *IF
;;   command.

;; * "%" is now distinguished as the Ansys parameter substitution
;;   and format specifier character.

;; * The ampersand "&" is now correctly highlighted as the only
;;   available Ansys continuation character applicable to the format
;;   commands (*MSG, *MWRITE, *VREAD and *VWRITE) command and the
;;   subsequent format strings of the command are fontified.

;; * New: " *" (SPC before *) is indicated as an (Ansys deprecated)
;;   comment sign e. g.: "a = 3 **4" results in "a" setting to 3,
;;   whereas "a = 3**4" sets "a" to 81!

;; * New: A line beginning with a comma is indented to the lenght of
;;   the last non slash or asterisk command as a reminder that the
;;   Ansys solver interprets this as a space holder for the last
;;   command keyword (the Ansys default command concept).

;; * Extended documentation, code cleaning and simplification of
;;   commands (e.g. comment handling) with the application of standard
;;   Emacs 22 facilities among other things.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; == Resources ==

;; http://www.emacswiki.org
;; http://www.ansys.net -- general Ansys repository
;; http://www.ansys.com
;; http://www.ansyssolutions.com -- The Ansys solutions magazine
;; http://www.xansys.org -- ansys online community

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; == Bugs and Problems ==

;; C*** does no parameter substitution, neither the /SYS command; this
;; is wrongly suggested in the highlighting of Ansys mode.
;; A clearing of variables (VAR= ) is not taken into account

;; Ansys format line (multi-line) highlighting is brittle, please use
;; M-o M-o to update the fontification in case the format line is not
;; correctly highlighted

;; The *END command is special: It needs 8 characters in all (+ 4
;; whitespaces) before a comment character behind it is possible (see
;; the Ansys 11.0 manual), this is not yet indicated in ansys-mode.

;; === Getting help ===

;; If you experience problems installing or running this mode you have
;; the following options:

;; * It might, at the first stage, be helpful for you to visit the
;;   Emacs Wiki (http://www.emacswiki.org/cgi-bin/wiki/AnsysMode) for
;;   further instructions.  At the Wiki you can also leave some
;;   comments or wishes.

;; * Write an email to the mode maintainer (you can trigger a bug
;;   report from the menu--at least a useful template when you are not
;;   in the position of sending emails via Emacs--or call the function
;;   `ansys-submit-bug-report').

;; * When you have already a (cost free) Google account you are able
;;   to issue a bug report at the Google Code hosted page
;;   http://code.google.com/p/ansys-mode/issues/list.  On this site
;;   you can also download the latest development version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(defconst ansys_version "11.0"		;NEW_C
  "Ansys version on which Ansys mode is based.")

(defconst ansys_mode_version "2"	;NEW_C
  "Ansys mode minor version number.")

;; --- defcustoms ---

(require 'custom)

(defgroup Ansys nil			;NEW_C from Octave-Mod.el
  "Customisation group for the Ansys mode."
  :version "22.1"
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
\"/ansys_inc/v110/ansys/bin/ansys110\"."
  :type 'string
  :group 'Ansys)

(defcustom ansys-help-file ""		;NEW_C
  "The Ansys \"Help System\" file name.
It is called with \\[ansys-start-ansys-help].  When the file is
not in your search path, you have to funish the complete path
specification.  For example:
\"/ansys_inc/v110/ansys/bin/anshelp110\" or with the windows OS
\"c:\\\\Program\ Files\\Ansys\ Inc\\v110\\CommonFiles\\HELP
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

(defcustom ansys-license-file ""	;NEW_C
  "The FlexLM license file name or license server specification(s).
The license server specification(s) must include the port number
when it isn't 1055, i. e. the default port number:
port_number@server_name, use the colon for multiple servers, for
example \"27005@rbgs421x:27005@rbgs422x\".

System settings and order of precedence: 1. ANSYSLMD_LICENSE_FILE
environment variable, 2.)  The FLEXlm resource file: ~/.flexlmrc
on Unix or the Windows registry. 3.) The LM_LICENSE_FILE
variable. 4.) The ansyslmd.ini file in the licensing
directory (This is what anslic_admin is doing in a recommended
installation).  5.) The license file itself."
  :type 'string
  :group 'Ansys)

(defcustom ansys-license-types		;NEW_C
  '("ansys" "struct" "ane3" "ansysds" "ane3fl" "preppost")
  "List of available license types to choose for a run.
This list should contain the license types you can choose from.  Below
are often used license types (as e.g. seen with the function
`ansys-license-status') and their corresponding WorkBench
terminologies.

\"ansys\" - Mechanical U (without thermal capability)
\"struct\" - Structural U (with thermal capability)
\"ane3\" - Mechanical/Emag (Structural U with electromagnetics)
\"ansysds\" - Mechanical/LS-Dyna (Mechanical U with Ansys LS-Dyna inter-phase)
\"ane3fl\" - Multiphysics
\"preppost\" - PrepPost (just pre- and post-processing)"
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

(defvar ansys-is-unix-system-flag nil	;NEW_C
  "Non-nil means computer runs a Unix system.")

(defvar ansys-user-variables () ;NEW_C
  "Variable containing the user variables and first occurance.
The list is used for the fontification of these variables.")

(defvar ansys-process "ansys"		;NEW_C
  "Variable containing Emacs' description of a running ansys process.
Variable is only used internally in the mode.")

(defvar ansys-completion-alist '(("LINK1" . "LINK1") ("BEAM3"
						      . "BEAM3") ("BEAM4" . "BEAM4") ("SOLID5" . "SOLID5") ("COMBIN7"
						      . "COMBIN7") ("LINK8" . "LINK8") ("INFIN9" . "INFIN9") ("LINK10"
						      . "LINK10") ("LINK11" . "LINK11") ("CONTAC12"
						      . "CONTAC12") ("PLANE13" . "PLANE13") ("COMBIN14"
						      . "COMBIN14") ("PIPE16" . "PIPE16") ("PIPE17"
						      . "PIPE17") ("PIPE18" . "PIPE18") ("PIPE20" . "PIPE20") ("MASS21"
						      . "MASS21") ("BEAM23" . "BEAM23") ("BEAM24"
						      . "BEAM24") ("PLANE25" . "PLANE25") ("MATRIX27"
						      . "MATRIX27") ("SHELL28" . "SHELL28") ("FLUID29"
						      . "FLUID29") ("FLUID30" . "FLUID30") ("LINK31"
						      . "LINK31") ("LINK32" . "LINK32") ("LINK33" . "LINK33") ("LINK34"
						      . "LINK34") ("PLANE35" . "PLANE35") ("SOURC36"
						      . "SOURC36") ("COMBIN37" . "COMBIN37") ("FLUID38"
						      . "FLUID38") ("COMBIN39" . "COMBIN39") ("COMBIN40"
						      . "COMBIN40") ("SHELL41" . "SHELL41") ("PLANE42"
						      . "PLANE42") ("SHELL43" . "SHELL43") ("BEAM44"
						      . "BEAM44") ("SOLID45" . "SOLID45") ("SOLID46"
						      . "SOLID46") ("INFIN47" . "INFIN47") ("MATRIX50"
						      . "MATRIX50") ("CONTAC52" . "CONTAC52") ("PLANE53"
						      . "PLANE53") ("BEAM54" . "BEAM54") ("PLANE55"
						      . "PLANE55") ("SHELL57" . "SHELL57") ("PIPE59"
						      . "PIPE59") ("PIPE60" . "PIPE60") ("SHELL61"
						      . "SHELL61") ("SOLID62" . "SOLID62") ("SHELL63"
						      . "SHELL63") ("SOLID65" . "SOLID65") ("PLANE67"
						      . "PLANE67") ("LINK68" . "LINK68") ("SOLID69"
						      . "SOLID69") ("SOLID70" . "SOLID70") ("MASS71"
						      . "MASS71") ("PLANE75" . "PLANE75") ("PLANE77"
						      . "PLANE77") ("PLANE78" . "PLANE78") ("FLUID79"
						      . "FLUID79") ("FLUID80" . "FLUID80") ("FLUID81"
						      . "FLUID81") ("PLANE82" . "PLANE82") ("PLANE83"
						      . "PLANE83") ("SOLID87" . "SOLID87") ("VISCO88"
						      . "VISCO88") ("VISCO89" . "VISCO89") ("SOLID90"
						      . "SOLID90") ("SHELL91" . "SHELL91") ("SOLID92"
						      . "SOLID92") ("SHELL93" . "SHELL93") ("CIRCU94"
						      . "CIRCU94") ("SOLID95" . "SOLID95") ("SOLID96"
						      . "SOLID96") ("SOLID97" . "SOLID97") ("SOLID98"
						      . "SOLID98") ("SHELL99" . "SHELL99") ("VISCO106"
						      . "VISCO106") ("VISCO107" . "VISCO107") ("VISCO108"
						      . "VISCO108") ("TRANS109" . "TRANS109") ("INFIN110"
						      . "INFIN110") ("INFIN111" . "INFIN111") ("INTER115"
						      . "INTER115") ("FLUID116" . "FLUID116") ("SOLID117"
						      . "SOLID117") ("HF118" . "HF118") ("HF119" . "HF119") ("HF120"
						      . "HF120") ("PLANE121" . "PLANE121") ("SOLID122"
						      . "SOLID122") ("SOLID123" . "SOLID123") ("CIRCU124"
						      . "CIRCU124") ("CIRCU125" . "CIRCU125") ("TRANS126"
						      . "TRANS126") ("SOLID127" . "SOLID127") ("SOLID128"
						      . "SOLID128") ("FLUID129" . "FLUID129") ("FLUID130"
						      . "FLUID130") ("SHELL131" . "SHELL131") ("SHELL132"
						      . "SHELL132") ("FLUID136" . "FLUID136") ("FLUID138"
						      . "FLUID138") ("FLUID139" . "FLUID139") ("FLUID141"
						      . "FLUID141") ("FLUID142" . "FLUID142") ("ROM144"
						      . "ROM144") ("PLANE145" . "PLANE145") ("PLANE146"
						      . "PLANE146") ("SOLID147" . "SOLID147") ("SOLID148"
						      . "SOLID148") ("SHELL150" . "SHELL150") ("SURF151"
						      . "SURF151") ("SURF152" . "SURF152") ("SURF153"
						      . "SURF153") ("SURF154" . "SURF154") ("SURF156"
						      . "SURF156") ("SHELL157" . "SHELL157") ("LINK160"
						      . "LINK160") ("BEAM161" . "BEAM161") ("PLANE162"
						      . "PLANE162") ("SHELL163" . "SHELL163") ("SOLID164"
						      . "SOLID164") ("COMBI165" . "COMBI165") ("MASS166"
						      . "MASS166") ("LINK167" . "LINK167") ("SOLID168"
						      . "SOLID168") ("TARGE169" . "TARGE169") ("TARGE170"
						      . "TARGE170") ("CONTA171" . "CONTA171") ("CONTA172"
						      . "CONTA172") ("CONTA173" . "CONTA173") ("CONTA174"
						      . "CONTA174") ("CONTA175" . "CONTA175") ("CONTA176"
						      . "CONTA176") ("CONTA177" . "CONTA177") ("CONTA178"
						      . "CONTA178") ("PRETS179" . "PRETS179") ("LINK180"
						      . "LINK180") ("SHELL181" . "SHELL181") ("PLANE182"
						      . "PLANE182") ("PLANE183" . "PLANE183") ("MPC184"
						      . "MPC184") ("SOLID185" . "SOLID185") ("SOLID186"
						      . "SOLID186") ("SOLID187" . "SOLID187") ("BEAM188"
						      . "BEAM188") ("BEAM189" . "BEAM189") ("SOLSH190"
						      . "SOLSH190") ("SOLID191" . "SOLID191") ("INTER192"
						      . "INTER192") ("INTER193" . "INTER193") ("INTER194"
						      . "INTER194") ("INTER195" . "INTER195") ("MESH200"
						      . "MESH200") ("FOLLW201" . "FOLLW201") ("INTER202"
						      . "INTER202") ("INTER203" . "INTER203") ("INTER204"
						      . "INTER204") ("INTER205" . "INTER205") ("SHELL208"
						      . "SHELL208") ("SHELL209" . "SHELL209") ("COMBI214"
						      . "COMBI214") ("PLANE223" . "PLANE223") ("SOLID226"
						      . "SOLID226") ("SOLID227" . "SOLID227") ("PLANE230"
						      . "PLANE230") ("SOLID231" . "SOLID231") ("SOLID232"
						      . "SOLID232") ("SURF251" . "SURF251") ("SURF252"
						      . "SURF252") ("REINF265" . "REINF265") ("SHELL281"
						      . "SHELL281") ("*ABBR" . "*ABBR") ("ABBRES" . "ABBRES") ("ABBSAV"
						      . "ABBSAV") ("*AFUN" . "*AFUN") ("*ASK" . "*ASK") ("*CFCLOS"
						      . "*CFCLOS") ("*CFOPEN" . "*CFOPEN") ("*CFWRITE"
						      . "*CFWRITE") ("*CREATE" . "*CREATE") ("*CYCLE"
						      . "*CYCLE") ("*DEL" . "*DEL") ("/DFLAB" . "/DFLAB") ("*DIM"
						      . "*DIM") ("/DIRECTORY" . "/DIRECTORY") ("*DO"
						      . "*DO") ("*DOWHILE" . "*DOWHILE") ("*ELSE" . "*ELSE") ("*ELSEIF"
						      . "*ELSEIF") ("*END" . "*END") ("*ENDDO" . "*ENDDO") ("*ENDIF"
						      . "*ENDIF") ("*EXIT" . "*EXIT") ("*GET" . "*GET") ("*GO"
						      . "*GO") ("*IF" . "*IF") ("/INQUIRE" . "/INQUIRE") ("/MAIL"
						      . "/MAIL") ("*MFOURI" . "*MFOURI") ("*MFUN" . "*MFUN") ("/MKDIR"
						      . "/MKDIR") ("*MOPER" . "*MOPER") ("*MSG" . "*MSG") ("*MWRITE"
						      . "*MWRITE") ("PARRES" . "PARRES") ("PARSAV"
						      . "PARSAV") ("/PMACRO" . "/PMACRO") ("/PSEARCH"
						      . "/PSEARCH") ("*REPEAT" . "*REPEAT") ("*RETURN"
						      . "*RETURN") ("/RMDIR" . "/RMDIR") ("*SET" . "*SET") ("*SREAD"
						      . "*SREAD") ("*STATUS" . "*STATUS") ("*TAXIS" . "*TAXIS") ("/TEE"
						      . "/TEE") ("*TOPER" . "*TOPER") ("*TREAD" . "*TREAD") ("/UCMD"
						      . "/UCMD") ("*ULIB" . "*ULIB") ("*USE" . "*USE") ("*VABS"
						      . "*VABS") ("*VCOL" . "*VCOL") ("*VCUM" . "*VCUM") ("*VEDIT"
						      . "*VEDIT") ("*VFACT" . "*VFACT") ("*VFILL" . "*VFILL") ("*VFUN"
						      . "*VFUN") ("*VGET" . "*VGET") ("*VITRP" . "*VITRP") ("*VLEN"
						      . "*VLEN") ("*VMASK" . "*VMASK") ("*VOPER" . "*VOPER") ("*VPLOT"
						      . "*VPLOT") ("*VPUT" . "*VPUT") ("*VREAD" . "*VREAD") ("*VSCFUN"
						      . "*VSCFUN") ("*VSTAT" . "*VSTAT") ("*VWRITE"
						      . "*VWRITE") ("/WAIT" . "/WAIT") ("~CAT5IN"
						      . "~CAT5IN") ("~CATIAIN" . "~CATIAIN") ("~PARAIN"
						      . "~PARAIN") ("~PROEIN" . "~PROEIN") ("~SATIN"
						      . "~SATIN") ("~UGIN" . "~UGIN") ("A" . "A") ("AADD"
						      . "AADD") ("AATT" . "AATT") ("ABEXTRACT" . "ABEXTRACT") ("ABS"
						      . "ABS") ("ACCAT" . "ACCAT") ("ACEL" . "ACEL") ("ACLEAR"
						      . "ACLEAR") ("ADAMS" . "ADAMS") ("ADAPT" . "ADAPT") ("ADD"
						      . "ADD") ("ADDAM" . "ADDAM") ("ADELE" . "ADELE") ("ADGL"
						      . "ADGL") ("ADRAG" . "ADRAG") ("AESIZE" . "AESIZE") ("AFILLT"
						      . "AFILLT") ("AFLIST" . "AFLIST") ("AFSURF" . "AFSURF") ("AGEN"
						      . "AGEN") ("AGLUE" . "AGLUE") ("AINA" . "AINA") ("AINP"
						      . "AINP") ("AINV" . "AINV") ("AL" . "AL") ("ALIST"
						      . "ALIST") ("ALLSEL" . "ALLSEL") ("ALPFILL"
						      . "ALPFILL") ("ALPHAD" . "ALPHAD") ("AMAP" . "AMAP") ("AMESH"
						      . "AMESH") ("/AN3D" . "/AN3D") ("ANCNTR" . "ANCNTR") ("ANCUT"
						      . "ANCUT") ("ANCYC" . "ANCYC") ("ANDATA" . "ANDATA") ("ANDSCL"
						      . "ANDSCL") ("ANDYNA" . "ANDYNA") ("/ANFILE"
						      . "/ANFILE") ("ANFLOW" . "ANFLOW") ("/ANGLE"
						      . "/ANGLE") ("ANHARM" . "ANHARM") ("ANIM" . "ANIM") ("ANISOS"
						      . "ANISOS") ("ANMODE" . "ANMODE") ("ANMRES" . "ANMRES") ("/ANNOT"
						      . "/ANNOT") ("ANORM" . "ANORM") ("ANSOL" . "ANSOL") ("ANSTOAQWA"
						      . "ANSTOAQWA") ("ANSTOASAS" . "ANSTOASAS") ("ANTIME"
						      . "ANTIME") ("ANTYPE" . "ANTYPE") ("/ANUM" . "/ANUM") ("AOFFST"
						      . "AOFFST") ("AOVLAP" . "AOVLAP") ("APLOT" . "APLOT") ("APPEND"
						      . "APPEND") ("APTN" . "APTN") ("ARCLEN" . "ARCLEN") ("ARCOLLAPSE"
						      . "ARCOLLAPSE") ("ARCTRM" . "ARCTRM") ("ARDETACH"
						      . "ARDETACH") ("AREAS" . "AREAS") ("AREFINE"
						      . "AREFINE") ("AREMESH" . "AREMESH") ("AREVERSE"
						      . "AREVERSE") ("ARFILL" . "ARFILL") ("ARMERGE"
						      . "ARMERGE") ("AROTAT" . "AROTAT") ("ARSCALE"
						      . "ARSCALE") ("ARSPLIT" . "ARSPLIT") ("ARSYM" . "ARSYM") ("ASBA"
						      . "ASBA") ("ASBL" . "ASBL") ("ASBV" . "ASBV") ("ASBW"
						      . "ASBW") ("ASEL" . "ASEL") ("ASKIN" . "ASKIN") ("ASLL"
						      . "ASLL") ("ASLV" . "ASLV") ("/ASSIGN" . "/ASSIGN") ("ASUB"
						      . "ASUB") ("ASUM" . "ASUM") ("ATAN" . "ATAN") ("ATRAN"
						      . "ATRAN") ("ATYPE" . "ATYPE") ("/AUTO" . "/AUTO") ("AUTOTS"
						      . "AUTOTS") ("/AUX2" . "/AUX2") ("/AUX3" . "/AUX3") ("/AUX12"
						      . "/AUX12") ("/AUX15" . "/AUX15") ("AVPRIN" . "AVPRIN") ("AVRES"
						      . "AVRES") ("/AXLAB" . "/AXLAB") ("/BATCH"
						      . "/BATCH") ("BCSOPTION" . "BCSOPTION") ("BELLOW"
						      . "BELLOW") ("BEND" . "BEND") ("BETAD" . "BETAD") ("BF"
						      . "BF") ("BFA" . "BFA") ("BFADELE" . "BFADELE") ("BFALIST"
						      . "BFALIST") ("BFCUM" . "BFCUM") ("BFDELE" . "BFDELE") ("BFE"
						      . "BFE") ("BFECUM" . "BFECUM") ("BFEDELE" . "BFEDELE") ("BFELIST"
						      . "BFELIST") ("BFESCAL" . "BFESCAL") ("BFINT" . "BFINT") ("BFK"
						      . "BFK") ("BFKDELE" . "BFKDELE") ("BFKLIST" . "BFKLIST") ("BFL"
						      . "BFL") ("BFLDELE" . "BFLDELE") ("BFLIST" . "BFLIST") ("BFLLIST"
						      . "BFLLIST") ("BFSCALE" . "BFSCALE") ("BFTRAN"
						      . "BFTRAN") ("BFUNIF" . "BFUNIF") ("BFV" . "BFV") ("BFVDELE"
						      . "BFVDELE") ("BFVLIST" . "BFVLIST") ("BIOOPT"
						      . "BIOOPT") ("BIOT" . "BIOT") ("BLC4" . "BLC4") ("BLC5"
						      . "BLC5") ("BLOCK" . "BLOCK") ("BOOL" . "BOOL") ("BOPTN"
						      . "BOPTN") ("BRANCH" . "BRANCH") ("BSAX" . "BSAX") ("BSMD"
						      . "BSMD") ("BSM1" . "BSM1") ("BSM2" . "BSM2") ("BSPLIN"
						      . "BSPLIN") ("BSS1" . "BSS1") ("BSS2" . "BSS2") ("BSTE"
						      . "BSTE") ("BSTQ" . "BSTQ") ("BTOL" . "BTOL") ("BUCOPT"
						      . "BUCOPT") ("CALC" . "CALC") ("CAMPBELL" . "CAMPBELL") ("CBDOF"
						      . "CBDOF") ("CDOPT" . "CDOPT") ("CDREAD" . "CDREAD") ("CDWRITE"
						      . "CDWRITE") ("CE" . "CE") ("CECHECK" . "CECHECK") ("CECMOD"
						      . "CECMOD") ("CECYC" . "CECYC") ("CEDELE" . "CEDELE") ("CEINTF"
						      . "CEINTF") ("CELIST" . "CELIST") ("CENTER" . "CENTER") ("CEQN"
						      . "CEQN") ("CERIG" . "CERIG") ("CESGEN" . "CESGEN") ("CFACT"
						      . "CFACT") ("/CFORMAT" . "/CFORMAT") ("CGLOC"
						      . "CGLOC") ("CGOMGA" . "CGOMGA") ("CHECK" . "CHECK") ("CHKMSH"
						      . "CHKMSH") ("CINT" . "CINT") ("CIRCLE" . "CIRCLE") ("CISOL"
						      . "CISOL") ("/CLABEL" . "/CLABEL") ("/CLEAR"
						      . "/CLEAR") ("CLOCAL" . "CLOCAL") ("CLOG" . "CLOG") ("/CLOG"
						      . "/CLOG") ("CLRMSHLN" . "CLRMSHLN") ("CM" . "CM") ("CMACEL"
						      . "CMACEL") ("/CMAP" . "/CMAP") ("CMATRIX" . "CMATRIX") ("CMDELE"
						      . "CMDELE") ("CMDOMEGA" . "CMDOMEGA") ("CMEDIT"
						      . "CMEDIT") ("CMGRP" . "CMGRP") ("CMLIST" . "CMLIST") ("CMMOD"
						      . "CMMOD") ("CMOMEGA" . "CMOMEGA") ("CMPLOT"
						      . "CMPLOT") ("CMROTATE" . "CMROTATE") ("CMSEL"
						      . "CMSEL") ("CMSFILE" . "CMSFILE") ("CMSOPT"
						      . "CMSOPT") ("CMWRITE" . "CMWRITE") ("CNCHECK"
						      . "CNCHECK") ("CNVTOL" . "CNVTOL") ("/COLOR" . "/COLOR") ("/COM"
						      . "/COM") ("COMPRESS" . "COMPRESS") ("CON4" . "CON4") ("CONE"
						      . "CONE") ("/CONFIG" . "/CONFIG") ("CONJUG"
						      . "CONJUG") ("/CONTOUR" . "/CONTOUR") ("/COPY"
						      . "/COPY") ("CORIOLIS" . "CORIOLIS") ("COUPLE"
						      . "COUPLE") ("COVAL" . "COVAL") ("CP" . "CP") ("CPCYC"
						      . "CPCYC") ("CPDELE" . "CPDELE") ("CPINTF" . "CPINTF") ("/CPLANE"
						      . "/CPLANE") ("CPLGEN" . "CPLGEN") ("CPLIST"
						      . "CPLIST") ("CPMERGE" . "CPMERGE") ("CPNGEN"
						      . "CPNGEN") ("CPSGEN" . "CPSGEN") ("CQC" . "CQC") ("CRPLIM"
						      . "CRPLIM") ("CS" . "CS") ("CSCIR" . "CSCIR") ("CSDELE"
						      . "CSDELE") ("CSKP" . "CSKP") ("CSLIST" . "CSLIST") ("CSWPLA"
						      . "CSWPLA") ("CSYS" . "CSYS") ("/CTYPE" . "/CTYPE") ("CURR2D"
						      . "CURR2D") ("CUTCONTROL" . "CUTCONTROL") ("/CVAL"
						      . "/CVAL") ("CVAR" . "CVAR") ("/CWD" . "/CWD") ("/CYCEXPAND"
						      . "/CYCEXPAND") ("CYCLIC" . "CYCLIC") ("CYCOPT"
						      . "CYCOPT") ("CYCPHASE" . "CYCPHASE") ("CYL4" . "CYL4") ("CYL5"
						      . "CYL5") ("CYLIND" . "CYLIND") ("CZDEL" . "CZDEL") ("CZMESH"
						      . "CZMESH") ("D" . "D") ("DA" . "DA") ("DADELE"
						      . "DADELE") ("DALIST" . "DALIST") ("DAMORPH" . "DAMORPH") ("DATA"
						      . "DATA") ("DATADEF" . "DATADEF") ("DCGOMG" . "DCGOMG") ("DCUM"
						      . "DCUM") ("DCVSWP" . "DCVSWP") ("DDELE" . "DDELE") ("DEACT"
						      . "DEACT") ("DECOMP" . "DECOMP") ("DEFINE" . "DEFINE") ("DELETE"
						      . "DELETE") ("/DELETE" . "/DELETE") ("DELTIM"
						      . "DELTIM") ("DEMORPH" . "DEMORPH") ("DERIV" . "DERIV") ("DESIZE"
						      . "DESIZE") ("DESOL" . "DESOL") ("DETAB" . "DETAB") ("/DEVDISP"
						      . "/DEVDISP") ("/DEVICE" . "/DEVICE") ("DIG" . "DIG") ("DIGIT"
						      . "DIGIT") ("DISPLAY" . "DISPLAY") ("/DIST" . "/DIST") ("DJ"
						      . "DJ") ("DJDELE" . "DJDELE") ("DJLIST" . "DJLIST") ("DK"
						      . "DK") ("DKDELE" . "DKDELE") ("DKLIST" . "DKLIST") ("DL"
						      . "DL") ("DLDELE" . "DLDELE") ("DLIST" . "DLIST") ("DLLIST"
						      . "DLLIST") ("DMOVE" . "DMOVE") ("DMPEXT" . "DMPEXT") ("DMPRAT"
						      . "DMPRAT") ("DNSOL" . "DNSOL") ("DOF" . "DOF") ("DOFSEL"
						      . "DOFSEL") ("DOMEGA" . "DOMEGA") ("DSCALE"
						      . "DSCALE") ("/DSCALE" . "/DSCALE") ("DSET"
						      . "DSET") ("DSPOPTION" . "DSPOPTION") ("DSUM" . "DSUM") ("DSURF"
						      . "DSURF") ("DSYM" . "DSYM") ("DSYS" . "DSYS") ("DTRAN"
						      . "DTRAN") ("DUMP" . "DUMP") ("/DV3D" . "/DV3D") ("DVMORPH"
						      . "DVMORPH") ("DYNOPT" . "DYNOPT") ("E" . "E") ("EALIVE"
						      . "EALIVE") ("EDADAPT" . "EDADAPT") ("EDALE" . "EDALE") ("EDASMP"
						      . "EDASMP") ("EDBOUND" . "EDBOUND") ("EDBX" . "EDBX") ("EDBVIS"
						      . "EDBVIS") ("EDCADAPT" . "EDCADAPT") ("EDCGEN"
						      . "EDCGEN") ("EDCLIST" . "EDCLIST") ("EDCMORE"
						      . "EDCMORE") ("EDCNSTR" . "EDCNSTR") ("EDCONTACT"
						      . "EDCONTACT") ("EDCPU" . "EDCPU") ("EDCRB" . "EDCRB") ("EDCSC"
						      . "EDCSC") ("EDCTS" . "EDCTS") ("EDCURVE" . "EDCURVE") ("EDDAMP"
						      . "EDDAMP") ("EDDBL" . "EDDBL") ("EDDC" . "EDDC") ("EDDRELAX"
						      . "EDDRELAX") ("EDDUMP" . "EDDUMP") ("EDELE"
						      . "EDELE") ("EDENERGY" . "EDENERGY") ("EDFPLOT"
						      . "EDFPLOT") ("EDGCALE" . "EDGCALE") ("/EDGE"
						      . "/EDGE") ("EDHGLS" . "EDHGLS") ("EDHIST" . "EDHIST") ("EDHTIME"
						      . "EDHTIME") ("EDINT" . "EDINT") ("EDIPART" . "EDIPART") ("EDIS"
						      . "EDIS") ("EDLCS" . "EDLCS") ("EDLOAD" . "EDLOAD") ("EDMP"
						      . "EDMP") ("EDNB" . "EDNB") ("EDNDTSD" . "EDNDTSD") ("EDNROT"
						      . "EDNROT") ("EDOPT" . "EDOPT") ("EDOUT" . "EDOUT") ("EDPART"
						      . "EDPART") ("EDPC" . "EDPC") ("EDPL" . "EDPL") ("EDPVEL"
						      . "EDPVEL") ("EDRC" . "EDRC") ("EDRD" . "EDRD") ("EDREAD"
						      . "EDREAD") ("EDRI" . "EDRI") ("EDRST" . "EDRST") ("EDRUN"
						      . "EDRUN") ("EDSHELL" . "EDSHELL") ("EDSOLV" . "EDSOLV") ("EDSP"
						      . "EDSP") ("EDSTART" . "EDSTART") ("EDTERM" . "EDTERM") ("EDTP"
						      . "EDTP") ("EDVEL" . "EDVEL") ("EDWELD" . "EDWELD") ("EDWRITE"
						      . "EDWRITE") ("/EFACET" . "/EFACET") ("EGEN" . "EGEN") ("EINTF"
						      . "EINTF") ("EKILL" . "EKILL") ("ELEM" . "ELEM") ("ELIST"
						      . "ELIST") ("EMAGERR" . "EMAGERR") ("EMATWRITE"
						      . "EMATWRITE") ("EMF" . "EMF") ("EMFT" . "EMFT") ("EMID"
						      . "EMID") ("EMIS" . "EMIS") ("EMODIF" . "EMODIF") ("EMORE"
						      . "EMORE") ("EMSYM" . "EMSYM") ("EMTGEN" . "EMTGEN") ("EMUNIT"
						      . "EMUNIT") ("EN" . "EN") ("ENDRELEASE"
						      . "ENDRELEASE") ("ENERSOL" . "ENERSOL") ("ENGEN"
						      . "ENGEN") ("ENORM" . "ENORM") ("ENSYM" . "ENSYM") ("/EOF"
						      . "/EOF") ("EORIENT" . "EORIENT") ("EPLOT" . "EPLOT") ("EQSLV"
						      . "EQSLV") ("ERASE" . "ERASE") ("/ERASE" . "/ERASE") ("EREAD"
						      . "EREAD") ("EREFINE" . "EREFINE") ("EREINF" . "EREINF") ("ERESX"
						      . "ERESX") ("ERNORM" . "ERNORM") ("ERRANG" . "ERRANG") ("ESCHECK"
						      . "ESCHECK") ("ESEL" . "ESEL") ("/ESHAPE" . "/ESHAPE") ("ESIZE"
						      . "ESIZE") ("ESLA" . "ESLA") ("ESLL" . "ESLL") ("ESLN"
						      . "ESLN") ("ESLV" . "ESLV") ("ESOL" . "ESOL") ("ESORT"
						      . "ESORT") ("ESSOLV" . "ESSOLV") ("ESTIF" . "ESTIF") ("ESURF"
						      . "ESURF") ("ESYM" . "ESYM") ("ESYS" . "ESYS") ("ET"
						      . "ET") ("ETABLE" . "ETABLE") ("ETCHG" . "ETCHG") ("ETCONTROL"
						      . "ETCONTROL") ("ETDELE" . "ETDELE") ("ETLIST"
						      . "ETLIST") ("ETYPE" . "ETYPE") ("EUSORT" . "EUSORT") ("EWRITE"
						      . "EWRITE") ("/EXIT" . "/EXIT") ("EXP" . "EXP") ("EXPAND"
						      . "EXPAND") ("/EXPAND" . "/EXPAND") ("EXPASS"
						      . "EXPASS") ("EXPROFILE" . "EXPROFILE") ("EXPSOL"
						      . "EXPSOL") ("EXTOPT" . "EXTOPT") ("EXTREM" . "EXTREM") ("EXUNIT"
						      . "EXUNIT") ("F" . "F") ("/FACET" . "/FACET") ("FATIGUE"
						      . "FATIGUE") ("FC" . "FC") ("FCCHECK" . "FCCHECK") ("FCDELE"
						      . "FCDELE") ("FCLIST" . "FCLIST") ("FCUM" . "FCUM") ("FDELE"
						      . "FDELE") ("/FDELE" . "/FDELE") ("FE" . "FE") ("FEBODY"
						      . "FEBODY") ("FECONS" . "FECONS") ("FEFOR" . "FEFOR") ("FELIST"
						      . "FELIST") ("FESURF" . "FESURF") ("FILE" . "FILE") ("FILEAUX2"
						      . "FILEAUX2") ("FILEAUX3" . "FILEAUX3") ("FILEDISP"
						      . "FILEDISP") ("FILL" . "FILL") ("FILLDATA"
						      . "FILLDATA") ("/FILNAME" . "/FILNAME") ("FINISH"
						      . "FINISH") ("FITEM" . "FITEM") ("FJ" . "FJ") ("FJDELE"
						      . "FJDELE") ("FJLIST" . "FJLIST") ("FK" . "FK") ("FKDELE"
						      . "FKDELE") ("FKLIST" . "FKLIST") ("FL" . "FL") ("FLANGE"
						      . "FLANGE") ("FLDATA" . "FLDATA") ("FLDATA1"
						      . "FLDATA1") ("FLDATA2" . "FLDATA2") ("FLDATA3"
						      . "FLDATA3") ("FLDATA4" . "FLDATA4") ("FLDATA4A"
						      . "FLDATA4A") ("FLDATA5" . "FLDATA5") ("FLDATA6"
						      . "FLDATA6") ("FLDATA7" . "FLDATA7") ("FLDATA8"
						      . "FLDATA8") ("FLDATA9" . "FLDATA9") ("FLDATA10"
						      . "FLDATA10") ("FLDATA11" . "FLDATA11") ("FLDATA12"
						      . "FLDATA12") ("FLDATA13" . "FLDATA13") ("FLDATA14"
						      . "FLDATA14") ("FLDATA15" . "FLDATA15") ("FLDATA16"
						      . "FLDATA16") ("FLDATA17" . "FLDATA17") ("FLDATA18"
						      . "FLDATA18") ("FLDATA19" . "FLDATA19") ("FLDATA20"
						      . "FLDATA20") ("FLDATA20A" . "FLDATA20A") ("FLDATA20B"
						      . "FLDATA20B") ("FLDATA21" . "FLDATA21") ("FLDATA22"
						      . "FLDATA22") ("FLDATA23" . "FLDATA23") ("FLDATA24"
						      . "FLDATA24") ("FLDATA24A" . "FLDATA24A") ("FLDATA24B"
						      . "FLDATA24B") ("FLDATA24C" . "FLDATA24C") ("FLDATA24D"
						      . "FLDATA24D") ("FLDATA24E" . "FLDATA24E") ("FLDATA24F"
						      . "FLDATA24F") ("FLDATA24G" . "FLDATA24G") ("FLDATA24H"
						      . "FLDATA24H") ("FLDATA25" . "FLDATA25") ("FLDATA26"
						      . "FLDATA26") ("FLDATA27" . "FLDATA27") ("FLDATA28"
						      . "FLDATA28") ("FLDATA29" . "FLDATA29") ("FLDATA30"
						      . "FLDATA30") ("FLDATA31" . "FLDATA31") ("FLDATA32"
						      . "FLDATA32") ("FLDATA33" . "FLDATA33") ("FLDATA34"
						      . "FLDATA34") ("FLDATA35" . "FLDATA35") ("FLDATA36"
						      . "FLDATA36") ("FLDATA37" . "FLDATA37") ("FLDATA38"
						      . "FLDATA38") ("FLDATA39" . "FLDATA39") ("FLDATA40"
						      . "FLDATA40") ("FLIST" . "FLIST") ("FLLIST"
						      . "FLLIST") ("FLOCHECK" . "FLOCHECK") ("FLOTRAN"
						      . "FLOTRAN") ("FLREAD" . "FLREAD") ("FLST" . "FLST") ("FLUXV"
						      . "FLUXV") ("FMAGBC" . "FMAGBC") ("FMAGSUM"
						      . "FMAGSUM") ("/FOCUS" . "/FOCUS") ("FOR2D" . "FOR2D") ("FORCE"
						      . "FORCE") ("FORM" . "FORM") ("/FORMAT" . "/FORMAT") ("FP"
						      . "FP") ("FPLIST" . "FPLIST") ("FREQ" . "FREQ") ("FRQSCL"
						      . "FRQSCL") ("FS" . "FS") ("FSCALE" . "FSCALE") ("FSDELE"
						      . "FSDELE") ("FSLIST" . "FSLIST") ("FSNODE" . "FSNODE") ("FSPLOT"
						      . "FSPLOT") ("FSSECT" . "FSSECT") ("FSSPARM" . "FSSPARM") ("FSUM"
						      . "FSUM") ("FTCALC" . "FTCALC") ("FTRAN" . "FTRAN") ("FTSIZE"
						      . "FTSIZE") ("FTWRITE" . "FTWRITE") ("FVMESH" . "FVMESH") ("GAP"
						      . "GAP") ("GAPF" . "GAPF") ("GAPFINISH" . "GAPFINISH") ("GAPLIST"
						      . "GAPLIST") ("GAPMERGE" . "GAPMERGE") ("GAPOPT"
						      . "GAPOPT") ("GAPPLOT" . "GAPPLOT") ("GAUGE" . "GAUGE") ("/GCMD"
						      . "/GCMD") ("/GCOLUMN" . "/GCOLUMN") ("GENOPT"
						      . "GENOPT") ("GEOM" . "GEOM") ("GEOMETRY" . "GEOMETRY") ("/GFILE"
						      . "/GFILE") ("/GFORMAT" . "/GFORMAT") ("/GLINE"
						      . "/GLINE") ("/GMARKER" . "/GMARKER") ("GMATRIX"
						      . "GMATRIX") ("GMFACE" . "GMFACE") ("/GO" . "/GO") ("/GOLIST"
						      . "/GOLIST") ("/GOPR" . "/GOPR") ("GP" . "GP") ("GPDELE"
						      . "GPDELE") ("GPLIST" . "GPLIST") ("GPLOT"
						      . "GPLOT") ("/GRAPHICS" . "/GRAPHICS") ("/GRESUME"
						      . "/GRESUME") ("/GRID" . "/GRID") ("/GROPT" . "/GROPT") ("GRP"
						      . "GRP") ("/GRTYP" . "/GRTYP") ("/GSAVE" . "/GSAVE") ("GSBDATA"
						      . "GSBDATA") ("GSGDATA" . "GSGDATA") ("GSLIST"
						      . "GSLIST") ("GSSOL" . "GSSOL") ("/GST" . "/GST") ("GSUM"
						      . "GSUM") ("/GTHK" . "/GTHK") ("/GTYPE" . "/GTYPE") ("HARFRQ"
						      . "HARFRQ") ("/HBC" . "/HBC") ("HBMAT" . "HBMAT") ("/HEADER"
						      . "/HEADER") ("HELP" . "HELP") ("HELPDISP"
						      . "HELPDISP") ("HEMIOPT" . "HEMIOPT") ("HFADP"
						      . "HFADP") ("HFANG" . "HFANG") ("HFARRAY" . "HFARRAY") ("HFDEEM"
						      . "HFDEEM") ("HFEIGOPT" . "HFEIGOPT") ("HFEREFINE"
						      . "HFEREFINE") ("HFMODPRT" . "HFMODPRT") ("HFNEAR"
						      . "HFNEAR") ("HFPA" . "HFPA") ("HFPCSWP" . "HFPCSWP") ("HFPOWER"
						      . "HFPOWER") ("HFPORT" . "HFPORT") ("HFSCAT"
						      . "HFSCAT") ("HFSWEEP" . "HFSWEEP") ("HFSYM"
						      . "HFSYM") ("HMAGSOLV" . "HMAGSOLV") ("HPGL"
						      . "HPGL") ("HPTCREATE" . "HPTCREATE") ("HPTDELETE"
						      . "HPTDELETE") ("HRCPLX" . "HRCPLX") ("HREXP" . "HREXP") ("HROPT"
						      . "HROPT") ("HROUT" . "HROUT") ("IC" . "IC") ("ICDELE"
						      . "ICDELE") ("ICE" . "ICE") ("ICEDELE" . "ICEDELE") ("ICELIST"
						      . "ICELIST") ("ICLIST" . "ICLIST") ("/ICLWID"
						      . "/ICLWID") ("/ICSCALE" . "/ICSCALE") ("ICVFRC"
						      . "ICVFRC") ("IGESIN" . "IGESIN") ("IGESOUT"
						      . "IGESOUT") ("/IMAGE" . "/IMAGE") ("IMAGIN" . "IMAGIN") ("IMESH"
						      . "IMESH") ("IMMED" . "IMMED") ("IMPD" . "IMPD") ("INISTATE"
						      . "INISTATE") ("/INPUT" . "/INPUT") ("INRES" . "INRES") ("INRTIA"
						      . "INRTIA") ("INT1" . "INT1") ("INTSRF" . "INTSRF") ("IOPTN"
						      . "IOPTN") ("IRLF" . "IRLF") ("IRLIST" . "IRLIST") ("ISFILE"
						      . "ISFILE") ("ISTRESS" . "ISTRESS") ("ISWRITE"
						      . "ISWRITE") ("JPEG" . "JPEG") ("JSOL" . "JSOL") ("K"
						      . "K") ("KATT" . "KATT") ("KBC" . "KBC") ("KBETW"
						      . "KBETW") ("KCALC" . "KCALC") ("KCENTER" . "KCENTER") ("KCLEAR"
						      . "KCLEAR") ("KDELE" . "KDELE") ("KDIST" . "KDIST") ("KEEP"
						      . "KEEP") ("KESIZE" . "KESIZE") ("KEYOPT" . "KEYOPT") ("KEYPTS"
						      . "KEYPTS") ("KEYW" . "KEYW") ("KFILL" . "KFILL") ("KGEN"
						      . "KGEN") ("KL" . "KL") ("KLIST" . "KLIST") ("KMESH"
						      . "KMESH") ("KMODIF" . "KMODIF") ("KMOVE" . "KMOVE") ("KNODE"
						      . "KNODE") ("KPLOT" . "KPLOT") ("KPSCALE" . "KPSCALE") ("KREFINE"
						      . "KREFINE") ("KSCALE" . "KSCALE") ("KSCON" . "KSCON") ("KSEL"
						      . "KSEL") ("KSLL" . "KSLL") ("KSLN" . "KSLN") ("KSUM"
						      . "KSUM") ("KSYMM" . "KSYMM") ("KTRAN" . "KTRAN") ("KUSE"
						      . "KUSE") ("KWPAVE" . "KWPAVE") ("KWPLAN" . "KWPLAN") ("L"
						      . "L") ("L2ANG" . "L2ANG") ("L2TAN" . "L2TAN") ("LANG"
						      . "LANG") ("LARC" . "LARC") ("/LARC" . "/LARC") ("LAREA"
						      . "LAREA") ("LARGE" . "LARGE") ("LATT" . "LATT") ("LAYER"
						      . "LAYER") ("LAYERP26" . "LAYERP26") ("LAYLIST"
						      . "LAYLIST") ("LAYPLOT" . "LAYPLOT") ("LCABS" . "LCABS") ("LCASE"
						      . "LCASE") ("LCCALC" . "LCCALC") ("LCCAT" . "LCCAT") ("LCDEF"
						      . "LCDEF") ("LCFACT" . "LCFACT") ("LCFILE" . "LCFILE") ("LCLEAR"
						      . "LCLEAR") ("LCOMB" . "LCOMB") ("LCOPER" . "LCOPER") ("LCSEL"
						      . "LCSEL") ("LCSL" . "LCSL") ("LCSUM" . "LCSUM") ("LCWRITE"
						      . "LCWRITE") ("LCZERO" . "LCZERO") ("LDELE" . "LDELE") ("LDIV"
						      . "LDIV") ("LDRAG" . "LDRAG") ("LDREAD" . "LDREAD") ("LESIZE"
						      . "LESIZE") ("LEXTND" . "LEXTND") ("LFILLT" . "LFILLT") ("LFSURF"
						      . "LFSURF") ("LGEN" . "LGEN") ("LGLUE" . "LGLUE") ("LGWRITE"
						      . "LGWRITE") ("/LIGHT" . "/LIGHT") ("LINA" . "LINA") ("LINE"
						      . "LINE") ("/LINE" . "/LINE") ("LINES" . "LINES") ("LINL"
						      . "LINL") ("LINP" . "LINP") ("LINV" . "LINV") ("LIST"
						      . "LIST") ("*LIST" . "*LIST") ("LLIST" . "LLIST") ("LMATRIX"
						      . "LMATRIX") ("LMESH" . "LMESH") ("LNCOLLAPSE"
						      . "LNCOLLAPSE") ("LNDETACH" . "LNDETACH") ("LNFILL"
						      . "LNFILL") ("LNMERGE" . "LNMERGE") ("LNSPLIT"
						      . "LNSPLIT") ("LNSRCH" . "LNSRCH") ("LOCAL" . "LOCAL") ("LOVLAP"
						      . "LOVLAP") ("LPLOT" . "LPLOT") ("LPTN" . "LPTN") ("LREFINE"
						      . "LREFINE") ("LREVERSE" . "LREVERSE") ("LROTAT"
						      . "LROTAT") ("LSBA" . "LSBA") ("LSBL" . "LSBL") ("LSBV"
						      . "LSBV") ("LSBW" . "LSBW") ("LSCLEAR" . "LSCLEAR") ("LSDELE"
						      . "LSDELE") ("LSEL" . "LSEL") ("LSLA" . "LSLA") ("LSLK"
						      . "LSLK") ("LSOPER" . "LSOPER") ("/LSPEC" . "/LSPEC") ("LSREAD"
						      . "LSREAD") ("LSSCALE" . "LSSCALE") ("LSSOLVE"
						      . "LSSOLVE") ("LSTR" . "LSTR") ("LSUM" . "LSUM") ("LSWRITE"
						      . "LSWRITE") ("/LSYMBOL" . "/LSYMBOL") ("LSYMM"
						      . "LSYMM") ("LTAN" . "LTAN") ("LTRAN" . "LTRAN") ("LUMPM"
						      . "LUMPM") ("LVSCALE" . "LVSCALE") ("LWPLAN" . "LWPLAN") ("M"
						      . "M") ("MADAPT" . "MADAPT") ("MAGOPT" . "MAGOPT") ("MAGSOLV"
						      . "MAGSOLV") ("MAPSOLVE" . "MAPSOLVE") ("MASTER"
						      . "MASTER") ("MAT" . "MAT") ("MATER" . "MATER") ("MCHECK"
						      . "MCHECK") ("MDAMP" . "MDAMP") ("MDELE" . "MDELE") ("MDPLOT"
						      . "MDPLOT") ("MEMM" . "MEMM") ("/MENU" . "/MENU") ("MESHING"
						      . "MESHING") ("MFANALYSIS" . "MFANALYSIS") ("MFBUCKET"
						      . "MFBUCKET") ("MFCALC" . "MFCALC") ("MFCI" . "MFCI") ("MFCLEAR"
						      . "MFCLEAR") ("MFCMMAND" . "MFCMMAND") ("MFCONV"
						      . "MFCONV") ("MFDTIME" . "MFDTIME") ("MFELEM" . "MFELEM") ("MFEM"
						      . "MFEM") ("MFEXTER" . "MFEXTER") ("MFFNAME" . "MFFNAME") ("MFFR"
						      . "MFFR") ("MFIMPORT" . "MFIMPORT") ("MFINTER"
						      . "MFINTER") ("MFITER" . "MFITER") ("MFLCOMM"
						      . "MFLCOMM") ("MFLIST" . "MFLIST") ("MFMAP" . "MFMAP") ("MFORDER"
						      . "MFORDER") ("MFOUTPUT" . "MFOUTPUT") ("MFPSIMUL"
						      . "MFPSIMUL") ("MFRELAX" . "MFRELAX") ("MFRSTART"
						      . "MFRSTART") ("MFSORDER" . "MFSORDER") ("MFSURFACE"
						      . "MFSURFACE") ("MFTIME" . "MFTIME") ("MFTOL"
						      . "MFTOL") ("MFVOLUME" . "MFVOLUME") ("MFWRITE"
						      . "MFWRITE") ("MGEN" . "MGEN") ("MIDTOL" . "MIDTOL") ("MITER"
						      . "MITER") ("MLIST" . "MLIST") ("MMF" . "MMF") ("MODE"
						      . "MODE") ("MODIFY" . "MODIFY") ("MODMSH" . "MODMSH") ("MODOPT"
						      . "MODOPT") ("MONITOR" . "MONITOR") ("MOPT" . "MOPT") ("MORPH"
						      . "MORPH") ("MOVE" . "MOVE") ("MP" . "MP") ("MPAMOD"
						      . "MPAMOD") ("MPCHG" . "MPCHG") ("MPCOPY" . "MPCOPY") ("MPDATA"
						      . "MPDATA") ("MPDELE" . "MPDELE") ("MPDRES" . "MPDRES") ("/MPLIB"
						      . "/MPLIB") ("MPLIST" . "MPLIST") ("MPPLOT" . "MPPLOT") ("MPREAD"
						      . "MPREAD") ("MPRINT" . "MPRINT") ("MPTEMP" . "MPTEMP") ("MPTGEN"
						      . "MPTGEN") ("MPTRES" . "MPTRES") ("MPWRITE"
						      . "MPWRITE") ("/MREP" . "/MREP") ("MSADV" . "MSADV") ("MSAVE"
						      . "MSAVE") ("MSCAP" . "MSCAP") ("MSDATA" . "MSDATA") ("MSHAPE"
						      . "MSHAPE") ("MSHCOPY" . "MSHCOPY") ("MSHKEY"
						      . "MSHKEY") ("MSHMID" . "MSHMID") ("MSHPATTERN"
						      . "MSHPATTERN") ("MSMASS" . "MSMASS") ("MSMETH"
						      . "MSMETH") ("MSMIR" . "MSMIR") ("MSNOMF" . "MSNOMF") ("MSPROP"
						      . "MSPROP") ("MSQUAD" . "MSQUAD") ("MSRELAX"
						      . "MSRELAX") ("MSSOLU" . "MSSOLU") ("MSSPEC"
						      . "MSSPEC") ("/MSTART" . "/MSTART") ("MSTERM"
						      . "MSTERM") ("MSVARY" . "MSVARY") ("MXPAND" . "MXPAND") ("N"
						      . "N") ("NANG" . "NANG") ("NCNV" . "NCNV") ("NDELE"
						      . "NDELE") ("NDIST" . "NDIST") ("NDSURF" . "NDSURF") ("NEQIT"
						      . "NEQIT") ("/NERR" . "/NERR") ("NFORCE" . "NFORCE") ("NGEN"
						      . "NGEN") ("NKPT" . "NKPT") ("NLDIAG" . "NLDIAG") ("NLDPOST"
						      . "NLDPOST") ("NLGEOM" . "NLGEOM") ("NLHIST" . "NLHIST") ("NLIST"
						      . "NLIST") ("NLOG" . "NLOG") ("NLOPT" . "NLOPT") ("NMODIF"
						      . "NMODIF") ("NOCOLOR" . "NOCOLOR") ("NODES"
						      . "NODES") ("/NOERASE" . "/NOERASE") ("/NOLIST"
						      . "/NOLIST") ("NOOFFSET" . "NOOFFSET") ("NOORDER"
						      . "NOORDER") ("/NOPR" . "/NOPR") ("NORA" . "NORA") ("NORL"
						      . "NORL") ("/NORMAL" . "/NORMAL") ("NPLOT" . "NPLOT") ("NPRINT"
						      . "NPRINT") ("NREAD" . "NREAD") ("NREFINE" . "NREFINE") ("NRLSUM"
						      . "NRLSUM") ("NROPT" . "NROPT") ("NROTAT" . "NROTAT") ("NRRANG"
						      . "NRRANG") ("NSCALE" . "NSCALE") ("NSEL" . "NSEL") ("NSLA"
						      . "NSLA") ("NSLE" . "NSLE") ("NSLK" . "NSLK") ("NSLL"
						      . "NSLL") ("NSLV" . "NSLV") ("NSMOOTH" . "NSMOOTH") ("NSOL"
						      . "NSOL") ("NSORT" . "NSORT") ("NSTORE" . "NSTORE") ("NSUBST"
						      . "NSUBST") ("NSVR" . "NSVR") ("NSYM" . "NSYM") ("/NUMBER"
						      . "/NUMBER") ("NUMCMP" . "NUMCMP") ("NUMEXP"
						      . "NUMEXP") ("NUMMRG" . "NUMMRG") ("NUMOFF" . "NUMOFF") ("NUMSTR"
						      . "NUMSTR") ("NUMVAR" . "NUMVAR") ("NUSORT" . "NUSORT") ("NWPAVE"
						      . "NWPAVE") ("NWPLAN" . "NWPLAN") ("NWRITE" . "NWRITE") ("OMEGA"
						      . "OMEGA") ("OPADD" . "OPADD") ("OPANL" . "OPANL") ("OPCLR"
						      . "OPCLR") ("OPDATA" . "OPDATA") ("OPDEL" . "OPDEL") ("OPEQN"
						      . "OPEQN") ("OPERATE" . "OPERATE") ("OPEXE" . "OPEXE") ("OPFACT"
						      . "OPFACT") ("OPFRST" . "OPFRST") ("OPGRAD" . "OPGRAD") ("OPKEEP"
						      . "OPKEEP") ("OPLFA" . "OPLFA") ("OPLGR" . "OPLGR") ("OPLIST"
						      . "OPLIST") ("OPLOOP" . "OPLOOP") ("OPLSW" . "OPLSW") ("OPMAKE"
						      . "OPMAKE") ("OPNCONTROL" . "OPNCONTROL") ("OPPRNT"
						      . "OPPRNT") ("OPRAND" . "OPRAND") ("OPRESU" . "OPRESU") ("OPRFA"
						      . "OPRFA") ("OPRGR" . "OPRGR") ("OPRSW" . "OPRSW") ("OPSAVE"
						      . "OPSAVE") ("OPSEL" . "OPSEL") ("OPSUBP" . "OPSUBP") ("OPSWEEP"
						      . "OPSWEEP") ("/OPT" . "/OPT") ("OPTYPE" . "OPTYPE") ("OPUSER"
						      . "OPUSER") ("OPVAR" . "OPVAR") ("OUTOPT" . "OUTOPT") ("OUTPR"
						      . "OUTPR") ("/OUTPUT" . "/OUTPUT") ("OUTRES"
						      . "OUTRES") ("PADELE" . "PADELE") ("/PAGE" . "/PAGE") ("PAGET"
						      . "PAGET") ("PAPUT" . "PAPUT") ("PARESU" . "PARESU") ("PARTSEL"
						      . "PARTSEL") ("PASAVE" . "PASAVE") ("PATH" . "PATH") ("/PBC"
						      . "/PBC") ("/PBF" . "/PBF") ("PCALC" . "PCALC") ("PCGOPT"
						      . "PCGOPT") ("PCIRC" . "PCIRC") ("/PCIRCLE"
						      . "/PCIRCLE") ("PCONV" . "PCONV") ("/PCOPY" . "/PCOPY") ("PCORRO"
						      . "PCORRO") ("PCROSS" . "PCROSS") ("PDANL" . "PDANL") ("PDCDF"
						      . "PDCDF") ("PDCFLD" . "PDCFLD") ("PDCLR" . "PDCLR") ("PDCMAT"
						      . "PDCMAT") ("PDCORR" . "PDCORR") ("PDDMCS" . "PDDMCS") ("PDDOEL"
						      . "PDDOEL") ("PDEF" . "PDEF") ("PDEXE" . "PDEXE") ("PDHIST"
						      . "PDHIST") ("PDINQR" . "PDINQR") ("PDLHS" . "PDLHS") ("PDMETH"
						      . "PDMETH") ("PDOT" . "PDOT") ("PDPINV" . "PDPINV") ("PDPLOT"
						      . "PDPLOT") ("PDPROB" . "PDPROB") ("PDRAG" . "PDRAG") ("PDRESU"
						      . "PDRESU") ("PDROPT" . "PDROPT") ("/PDS" . "/PDS") ("PDSAVE"
						      . "PDSAVE") ("PDSCAT" . "PDSCAT") ("PDSENS" . "PDSENS") ("PDSHIS"
						      . "PDSHIS") ("PDUSER" . "PDUSER") ("PDVAR" . "PDVAR") ("PDWRITE"
						      . "PDWRITE") ("PEMOPTS" . "PEMOPTS") ("PERBC2D"
						      . "PERBC2D") ("PERI" . "PERI") ("PEXCLUDE" . "PEXCLUDE") ("PFACT"
						      . "PFACT") ("PFLUID" . "PFLUID") ("PGAP" . "PGAP") ("PGRAPH"
						      . "PGRAPH") ("PGRSET" . "PGRSET") ("PGSAVE" . "PGSAVE") ("PGSELE"
						      . "PGSELE") ("PGWRITE" . "PGWRITE") ("PHYSICS"
						      . "PHYSICS") ("/PICE" . "/PICE") ("PINCLUDE"
						      . "PINCLUDE") ("PINSUL" . "PINSUL") ("PIPE" . "PIPE") ("PIVCHECK"
						      . "PIVCHECK") ("PLCAMP" . "PLCAMP") ("PLCINT"
						      . "PLCINT") ("PLCONV" . "PLCONV") ("PLCPLX"
						      . "PLCPLX") ("PLCRACK" . "PLCRACK") ("PLDISP"
						      . "PLDISP") ("PLESOL" . "PLESOL") ("PLETAB" . "PLETAB") ("PLF2D"
						      . "PLF2D") ("PLHFFAR" . "PLHFFAR") ("PLLS" . "PLLS") ("PLNSOL"
						      . "PLNSOL") ("/PLOPTS" . "/PLOPTS") ("PLORB" . "PLORB") ("PLOT"
						      . "PLOT") ("PLOTTING" . "PLOTTING") ("PLPAGM"
						      . "PLPAGM") ("PLPATH" . "PLPATH") ("PLSCH" . "PLSCH") ("PLSECT"
						      . "PLSECT") ("PLSYZ" . "PLSYZ") ("PLTD" . "PLTD") ("PLTIME"
						      . "PLTIME") ("PLTRAC" . "PLTRAC") ("PLVAR" . "PLVAR") ("PLVAROPT"
						      . "PLVAROPT") ("PLVECT" . "PLVECT") ("PLVFRC"
						      . "PLVFRC") ("PLWAVE" . "PLWAVE") ("PMAP" . "PMAP") ("PMETH"
						      . "PMETH") ("/PMETH" . "/PMETH") ("PMGTRAN"
						      . "PMGTRAN") ("PMLOPT" . "PMLOPT") ("PMLSIZE"
						      . "PMLSIZE") ("PMOPTS" . "PMOPTS") ("/PMORE" . "/PMORE") ("PNGR"
						      . "PNGR") ("/PNUM" . "/PNUM") ("POINT" . "POINT") ("POLY"
						      . "POLY") ("/POLYGON" . "/POLYGON") ("POPT" . "POPT") ("/POST1"
						      . "/POST1") ("/POST26" . "/POST26") ("POUTRES"
						      . "POUTRES") ("POWERH" . "POWERH") ("PPATH" . "PPATH") ("PPLOT"
						      . "PPLOT") ("PPRANGE" . "PPRANGE") ("PPRES" . "PPRES") ("PRANGE"
						      . "PRANGE") ("PRCAMP" . "PRCAMP") ("PRCINT" . "PRCINT") ("PRCONV"
						      . "PRCONV") ("PRCPLX" . "PRCPLX") ("PRECISION"
						      . "PRECISION") ("PRED" . "PRED") ("PRENERGY"
						      . "PRENERGY") ("/PREP7" . "/PREP7") ("PRERR" . "PRERR") ("PRESOL"
						      . "PRESOL") ("PRETAB" . "PRETAB") ("PRHFFAR" . "PRHFFAR") ("PRI2"
						      . "PRI2") ("PRIM" . "PRIM") ("PRINT" . "PRINT") ("PRISM"
						      . "PRISM") ("PRITER" . "PRITER") ("PRJSOL" . "PRJSOL") ("PRNLD"
						      . "PRNLD") ("PRNSOL" . "PRNSOL") ("PROD" . "PROD") ("PRORB"
						      . "PRORB") ("PRPATH" . "PRPATH") ("PRRFOR" . "PRRFOR") ("PRRSOL"
						      . "PRRSOL") ("PRSECT" . "PRSECT") ("PRSSOL" . "PRSSOL") ("PRSYZ"
						      . "PRSYZ") ("PRTIME" . "PRTIME") ("PRVAR" . "PRVAR") ("PRVAROPT"
						      . "PRVAROPT") ("PRVECT" . "PRVECT") ("PSCONTROL"
						      . "PSCONTROL") ("PSCR" . "PSCR") ("PSDCOM" . "PSDCOM") ("PSDFRQ"
						      . "PSDFRQ") ("PSDGRAPH" . "PSDGRAPH") ("PSDRES"
						      . "PSDRES") ("PSDSPL" . "PSDSPL") ("PSDUNIT"
						      . "PSDUNIT") ("PSDVAL" . "PSDVAL") ("PSDWAV" . "PSDWAV") ("PSEL"
						      . "PSEL") ("/PSF" . "/PSF") ("PSMESH" . "PSMESH") ("PSOLVE"
						      . "PSOLVE") ("PSPEC" . "PSPEC") ("/PSPEC" . "/PSPEC") ("PSPRNG"
						      . "PSPRNG") ("/PSTATUS" . "/PSTATUS") ("PSTRES"
						      . "PSTRES") ("/PSYMB" . "/PSYMB") ("PTEMP" . "PTEMP") ("PTXY"
						      . "PTXY") ("PUNIT" . "PUNIT") ("PVECT" . "PVECT") ("/PWEDGE"
						      . "/PWEDGE") ("QDVAL" . "QDVAL") ("QFACT" . "QFACT") ("QSOPT"
						      . "QSOPT") ("QUAD" . "QUAD") ("/QUIT" . "/QUIT") ("QUOT"
						      . "QUOT") ("R" . "R") ("RACE" . "RACE") ("RADOPT"
						      . "RADOPT") ("RALL" . "RALL") ("RAPPND" . "RAPPND") ("RATE"
						      . "RATE") ("/RATIO" . "/RATIO") ("RBE3" . "RBE3") ("RCON"
						      . "RCON") ("RDEC" . "RDEC") ("RDELE" . "RDELE") ("REAL"
						      . "REAL") ("REALVAR" . "REALVAR") ("RECTNG" . "RECTNG") ("REDUCE"
						      . "REDUCE") ("REFLCOEF" . "REFLCOEF") ("REMESH"
						      . "REMESH") ("/RENAME" . "/RENAME") ("REORDER"
						      . "REORDER") ("/REPLOT" . "/REPLOT") ("RESCONTROL"
						      . "RESCONTROL") ("RESET" . "RESET") ("/RESET" . "/RESET") ("RESP"
						      . "RESP") ("RESUME" . "RESUME") ("RESVEC" . "RESVEC") ("RESWRITE"
						      . "RESWRITE") ("REXPORT" . "REXPORT") ("REZONE"
						      . "REZONE") ("RFILSZ" . "RFILSZ") ("RFORCE" . "RFORCE") ("/RGB"
						      . "/RGB") ("RIGID" . "RIGID") ("RIMPORT" . "RIMPORT") ("RITER"
						      . "RITER") ("RLIST" . "RLIST") ("RMALIST" . "RMALIST") ("RMANL"
						      . "RMANL") ("RMASTER" . "RMASTER") ("RMCAP" . "RMCAP") ("RMCLIST"
						      . "RMCLIST") ("RMEMRY" . "RMEMRY") ("RMFLVEC"
						      . "RMFLVEC") ("RMLVSCALE" . "RMLVSCALE") ("RMMLIST"
						      . "RMMLIST") ("RMMRANGE" . "RMMRANGE") ("RMMSELECT"
						      . "RMMSELECT") ("RMNDISP" . "RMNDISP") ("RMNEVEC"
						      . "RMNEVEC") ("RMODIF" . "RMODIF") ("RMORE"
						      . "RMORE") ("RMPORDER" . "RMPORDER") ("RMRESUME"
						      . "RMRESUME") ("RMRGENERATE" . "RMRGENERATE") ("RMROPTIONS"
						      . "RMROPTIONS") ("RMRPLOT" . "RMRPLOT") ("RMRSTATUS"
						      . "RMRSTATUS") ("RMSAVE" . "RMSAVE") ("RMSMPLE"
						      . "RMSMPLE") ("RMUSE" . "RMUSE") ("RMXPORT" . "RMXPORT") ("ROCK"
						      . "ROCK") ("RPOLY" . "RPOLY") ("RPR4" . "RPR4") ("RPRISM"
						      . "RPRISM") ("RPSD" . "RPSD") ("RSFIT" . "RSFIT") ("RSOPT"
						      . "RSOPT") ("RSPEED" . "RSPEED") ("RSPLIT" . "RSPLIT") ("RSPLOT"
						      . "RSPLOT") ("RSPRNT" . "RSPRNT") ("RSSIMS" . "RSSIMS") ("RSTAT"
						      . "RSTAT") ("RSTOFF" . "RSTOFF") ("RSURF" . "RSURF") ("RSYMM"
						      . "RSYMM") ("RSYS" . "RSYS") ("RTHICK" . "RTHICK") ("RTIMST"
						      . "RTIMST") ("RUN" . "RUN") ("/RUNST" . "/RUNST") ("RWFRNT"
						      . "RWFRNT") ("SABS" . "SABS") ("SADD" . "SADD") ("SALLOW"
						      . "SALLOW") ("SARPLOT" . "SARPLOT") ("SAVE" . "SAVE") ("SBCLIST"
						      . "SBCLIST") ("SBCTRAN" . "SBCTRAN") ("SDELETE"
						      . "SDELETE") ("SE" . "SE") ("SECCONTROLS"
						      . "SECCONTROLS") ("SECDATA" . "SECDATA") ("SECFUNCTION"
						      . "SECFUNCTION") ("SECJOINT" . "SECJOINT") ("/SECLIB"
						      . "/SECLIB") ("SECLOCK" . "SECLOCK") ("SECMODIF"
						      . "SECMODIF") ("SECNUM" . "SECNUM") ("SECOFFSET"
						      . "SECOFFSET") ("SECPLOT" . "SECPLOT") ("SECREAD"
						      . "SECREAD") ("SECSTOP" . "SECSTOP") ("SECTYPE"
						      . "SECTYPE") ("SECWRITE" . "SECWRITE") ("SED" . "SED") ("SEDLIST"
						      . "SEDLIST") ("SEEXP" . "SEEXP") ("/SEG" . "/SEG") ("SEGEN"
						      . "SEGEN") ("SELIST" . "SELIST") ("SELM" . "SELM") ("SELTOL"
						      . "SELTOL") ("SENERGY" . "SENERGY") ("SEOPT" . "SEOPT") ("SESYMM"
						      . "SESYMM") ("SET" . "SET") ("SETFGAP" . "SETFGAP") ("SETRAN"
						      . "SETRAN") ("SEXP" . "SEXP") ("SF" . "SF") ("SFA"
						      . "SFA") ("SFACT" . "SFACT") ("SFADELE" . "SFADELE") ("SFALIST"
						      . "SFALIST") ("SFBEAM" . "SFBEAM") ("SFCALC" . "SFCALC") ("SFCUM"
						      . "SFCUM") ("SFDELE" . "SFDELE") ("SFE" . "SFE") ("SFEDELE"
						      . "SFEDELE") ("SFELIST" . "SFELIST") ("SFFUN"
						      . "SFFUN") ("SFGRAD" . "SFGRAD") ("SFL" . "SFL") ("SFLDELE"
						      . "SFLDELE") ("SFLIST" . "SFLIST") ("SFLLIST"
						      . "SFLLIST") ("SFSCALE" . "SFSCALE") ("SFTRAN"
						      . "SFTRAN") ("/SHADE" . "/SHADE") ("SHELL" . "SHELL") ("/SHOW"
						      . "/SHOW") ("/SHOWDISP" . "/SHOWDISP") ("SHPP"
						      . "SHPP") ("/SHRINK" . "/SHRINK") ("SHSD" . "SHSD") ("SLIST"
						      . "SLIST") ("SLOAD" . "SLOAD") ("SLPPLOT" . "SLPPLOT") ("SLSPLOT"
						      . "SLSPLOT") ("SMALL" . "SMALL") ("SMAX" . "SMAX") ("/SMBC"
						      . "/SMBC") ("SMBODY" . "SMBODY") ("SMCONS" . "SMCONS") ("SMFOR"
						      . "SMFOR") ("SMIN" . "SMIN") ("SMOOTH" . "SMOOTH") ("SMRTSIZE"
						      . "SMRTSIZE") ("SMSURF" . "SMSURF") ("SMULT"
						      . "SMULT") ("SOLCONTROL" . "SOLCONTROL") ("SOLU"
						      . "SOLU") ("/SOLU" . "/SOLU") ("SOLUOPT" . "SOLUOPT") ("SOLVE"
						      . "SOLVE") ("SORT" . "SORT") ("SOURCE" . "SOURCE") ("SPACE"
						      . "SPACE") ("SPADP" . "SPADP") ("SPARM" . "SPARM") ("SPCNOD"
						      . "SPCNOD") ("SPCTEMP" . "SPCTEMP") ("SPEC" . "SPEC") ("SPH4"
						      . "SPH4") ("SPH5" . "SPH5") ("SPHERE" . "SPHERE") ("SPICE"
						      . "SPICE") ("SPLINE" . "SPLINE") ("SPLOT" . "SPLOT") ("SPOINT"
						      . "SPOINT") ("SPOPT" . "SPOPT") ("SPREAD" . "SPREAD") ("SPSCAN"
						      . "SPSCAN") ("SPSWP" . "SPSWP") ("SPTOPT" . "SPTOPT") ("SQRT"
						      . "SQRT") ("SRSS" . "SRSS") ("SSBT" . "SSBT") ("/SSCALE"
						      . "/SSCALE") ("SSLN" . "SSLN") ("SSMT" . "SSMT") ("SSPA"
						      . "SSPA") ("SSPB" . "SSPB") ("SSPD" . "SSPD") ("SSPE"
						      . "SSPE") ("SSPM" . "SSPM") ("SSTIF" . "SSTIF") ("SSUM"
						      . "SSUM") ("STABILIZE" . "STABILIZE") ("STAOPT"
						      . "STAOPT") ("STAT" . "STAT") ("/STATUS" . "/STATUS") ("STEF"
						      . "STEF") ("/STITLE" . "/STITLE") ("STORE" . "STORE") ("SUBOPT"
						      . "SUBOPT") ("SUBSET" . "SUBSET") ("SUCALC" . "SUCALC") ("SUCR"
						      . "SUCR") ("SUDEL" . "SUDEL") ("SUEVAL" . "SUEVAL") ("SUGET"
						      . "SUGET") ("SUMAP" . "SUMAP") ("SUMTYPE" . "SUMTYPE") ("SUPL"
						      . "SUPL") ("SUPR" . "SUPR") ("SURESU" . "SURESU") ("SUSAVE"
						      . "SUSAVE") ("SUSEL" . "SUSEL") ("SUVECT" . "SUVECT") ("SV"
						      . "SV") ("SVTYP" . "SVTYP") ("SWADD" . "SWADD") ("SWDEL"
						      . "SWDEL") ("SWGEN" . "SWGEN") ("SWLIST" . "SWLIST") ("SYNCHRO"
						      . "SYNCHRO") ("/SYP" . "/SYP") ("/SYS" . "/SYS") ("TALLOW"
						      . "TALLOW") ("TB" . "TB") ("TBCOPY" . "TBCOPY") ("TBDATA"
						      . "TBDATA") ("TBDELE" . "TBDELE") ("TBFIELD" . "TBFIELD") ("TBFT"
						      . "TBFT") ("TBLE" . "TBLE") ("TBLIST" . "TBLIST") ("TBMODIF"
						      . "TBMODIF") ("TBPLOT" . "TBPLOT") ("TBPT" . "TBPT") ("TBTEMP"
						      . "TBTEMP") ("TCHG" . "TCHG") ("TEE" . "TEE") ("TERM"
						      . "TERM") ("THOPT" . "THOPT") ("TIFF" . "TIFF") ("TIME"
						      . "TIME") ("TIMERANGE" . "TIMERANGE") ("TIMINT"
						      . "TIMINT") ("TIMP" . "TIMP") ("TINTP" . "TINTP") ("/TITLE"
						      . "/TITLE") ("/TLABEL" . "/TLABEL") ("TOCOMP"
						      . "TOCOMP") ("TODEF" . "TODEF") ("TOEXE" . "TOEXE") ("TOFFST"
						      . "TOFFST") ("TOFREQ" . "TOFREQ") ("TOGRAPH"
						      . "TOGRAPH") ("TOLIST" . "TOLIST") ("TOLOOP"
						      . "TOLOOP") ("TOPLOT" . "TOPLOT") ("TOPRINT"
						      . "TOPRINT") ("TORQ2D" . "TORQ2D") ("TORQC2D"
						      . "TORQC2D") ("TORQSUM" . "TORQSUM") ("TORUS"
						      . "TORUS") ("TOSTAT" . "TOSTAT") ("TOTAL" . "TOTAL") ("TOTYPE"
						      . "TOTYPE") ("TOVAR" . "TOVAR") ("TRANS" . "TRANS") ("TRANSFER"
						      . "TRANSFER") ("TREF" . "TREF") ("/TRIAD" . "/TRIAD") ("/TRLCY"
						      . "/TRLCY") ("TRNOPT" . "TRNOPT") ("TRPDEL" . "TRPDEL") ("TRPLIS"
						      . "TRPLIS") ("TRPOIN" . "TRPOIN") ("TRTIME" . "TRTIME") ("TSHAP"
						      . "TSHAP") ("/TSPEC" . "/TSPEC") ("TSRES" . "TSRES") ("TUNIF"
						      . "TUNIF") ("TVAR" . "TVAR") ("/TXTRE" . "/TXTRE") ("/TYPE"
						      . "/TYPE") ("TYPE" . "TYPE") ("TZAMESH" . "TZAMESH") ("TZDELE"
						      . "TZDELE") ("TZEGEN" . "TZEGEN") ("/UDOC" . "/UDOC") ("/UI"
						      . "/UI") ("UIMP" . "UIMP") ("/UIS" . "/UIS") ("UNDELETE"
						      . "UNDELETE") ("UNDO" . "UNDO") ("/UNITS" . "/UNITS") ("UPCOORD"
						      . "UPCOORD") ("UPGEOM" . "UPGEOM") ("/USER" . "/USER") ("USRCAL"
						      . "USRCAL") ("USRDOF" . "USRDOF") ("USRELEM" . "USRELEM") ("V"
						      . "V") ("V2DOPT" . "V2DOPT") ("VA" . "VA") ("VADD"
						      . "VADD") ("VALVE" . "VALVE") ("VARDEL" . "VARDEL") ("VARNAM"
						      . "VARNAM") ("VATT" . "VATT") ("VCLEAR" . "VCLEAR") ("/VCONE"
						      . "/VCONE") ("VCROSS" . "VCROSS") ("VCVFILL"
						      . "VCVFILL") ("VDDAM" . "VDDAM") ("VDELE" . "VDELE") ("VDGL"
						      . "VDGL") ("VDOT" . "VDOT") ("VDRAG" . "VDRAG") ("VEORIENT"
						      . "VEORIENT") ("VEXT" . "VEXT") ("VFCALC" . "VFCALC") ("VFOPT"
						      . "VFOPT") ("VFQUERY" . "VFQUERY") ("VGEN" . "VGEN") ("VGET"
						      . "VGET") ("VGLUE" . "VGLUE") ("/VIEW" . "/VIEW") ("VIMP"
						      . "VIMP") ("VINP" . "VINP") ("VINV" . "VINV") ("VLIST"
						      . "VLIST") ("VLSCALE" . "VLSCALE") ("VMESH" . "VMESH") ("VOFFST"
						      . "VOFFST") ("VOLUMES" . "VOLUMES") ("VOVLAP"
						      . "VOVLAP") ("VPLOT" . "VPLOT") ("VPTN" . "VPTN") ("VPUT"
						      . "VPUT") ("VROTAT" . "VROTAT") ("VSBA" . "VSBA") ("VSBV"
						      . "VSBV") ("VSBW" . "VSBW") ("/VSCALE" . "/VSCALE") ("VSEL"
						      . "VSEL") ("VSLA" . "VSLA") ("VSUM" . "VSUM") ("VSWEEP"
						      . "VSWEEP") ("VSYMM" . "VSYMM") ("/VT" . "/VT") ("VTCLR"
						      . "VTCLR") ("VTDISC" . "VTDISC") ("VTEVAL" . "VTEVAL") ("VTFREQ"
						      . "VTFREQ") ("VTGEOM" . "VTGEOM") ("VTIN" . "VTIN") ("VTMETH"
						      . "VTMETH") ("VTMP" . "VTMP") ("VTOP" . "VTOP") ("VTPOST"
						      . "VTPOST") ("VTRAN" . "VTRAN") ("VTREAL" . "VTREAL") ("VTRFIL"
						      . "VTRFIL") ("VTRSLT" . "VTRSLT") ("VTSEC" . "VTSEC") ("VTSFE"
						      . "VTSFE") ("VTSL" . "VTSL") ("VTSTAT" . "VTSTAT") ("VTTEMP"
						      . "VTTEMP") ("VTVMOD" . "VTVMOD") ("VTYPE" . "VTYPE") ("/VUP"
						      . "/VUP") ("WAVES" . "WAVES") ("WERASE" . "WERASE") ("WFRONT"
						      . "WFRONT") ("/WINDOW" . "/WINDOW") ("WMID" . "WMID") ("WMORE"
						      . "WMORE") ("WPAVE" . "WPAVE") ("WPCSYS" . "WPCSYS") ("WPLANE"
						      . "WPLANE") ("WPOFFS" . "WPOFFS") ("WPROTA" . "WPROTA") ("WPSTYL"
						      . "WPSTYL") ("WRFULL" . "WRFULL") ("WRITE" . "WRITE") ("WSORT"
						      . "WSORT") ("WSPRINGS" . "WSPRINGS") ("WSTART"
						      . "WSTART") ("/XFRM" . "/XFRM") ("/XRANGE" . "/XRANGE") ("XVAR"
						      . "XVAR") ("XVAROPT" . "XVAROPT") ("/YRANGE"
						      . "/YRANGE") ("/ZOOM" . "/ZOOM") ("NSEL()" . "NSEL()") ("ESEL()"
						      . "ESEL()") ("KSEL()" . "KSEL()") ("LSEL()" . "LSEL()") ("ASEL()"
						      . "ASEL()") ("VSEL()" . "VSEL()") ("NDNEXT()"
						      . "NDNEXT()") ("ELNEXT()" . "ELNEXT()") ("KPNEXT()"
						      . "KPNEXT()") ("LSNEXT()" . "LSNEXT()") ("ARNEXT()"
						      . "ARNEXT()") ("VLNEXT()" . "VLNEXT()") ("CENTRX()"
						      . "CENTRX()") ("CENTRY()" . "CENTRY()") ("CENTRZ()"
						      . "CENTRZ()") ("NX()" . "NX()") ("NY()" . "NY()") ("NZ()"
						      . "NZ()") ("KX()" . "KX()") ("KY()" . "KY()") ("KZ()"
						      . "KZ()") ("LX()" . "LX()") ("LY()" . "LY()") ("LZ()"
						      . "LZ()") ("LSX()" . "LSX()") ("LSY()" . "LSY()") ("LSZ()"
						      . "LSZ()") ("NODE()" . "NODE()") ("KP()" . "KP()") ("DISTND()"
						      . "DISTND()") ("DISTKP()" . "DISTKP()") ("DISTEN()"
						      . "DISTEN()") ("ANGLEN()" . "ANGLEN()") ("ANGLEK()"
						      . "ANGLEK()") ("NNEAR()" . "NNEAR()") ("KNEAR()"
						      . "KNEAR()") ("ENEARN()" . "ENEARN()") ("AREAND()"
						      . "AREAND()") ("AREAKP()" . "AREAKP()") ("ARNODE()"
						      . "ARNODE()") ("NORMNX()" . "NORMNX()") ("NORMNY()"
						      . "NORMNY()") ("NORMNZ()" . "NORMNZ()") ("NORMKX()"
						      . "NORMKX()") ("NORMKY()" . "NORMKY()") ("NORMKZ()"
						      . "NORMKZ()") ("ENEXTN()" . "ENEXTN()") ("NELEM()"
						      . "NELEM()") ("NODEDOF()" . "NODEDOF()") ("ELADJ()"
						      . "ELADJ()") ("NDFACE()" . "NDFACE()") ("NMFACE()"
						      . "NMFACE()") ("ARFACE()" . "ARFACE()") ("UX()" . "UX()") ("UY()"
						      . "UY()") ("UZ()" . "UZ()") ("ROTX()" . "ROTX()") ("ROTY()"
						      . "ROTY()") ("ROTZ()" . "ROTZ()") ("TEMP()" . "TEMP()") ("PRES()"
						      . "PRES()") ("VX()" . "VX()") ("VY()" . "VY()") ("VZ()"
						      . "VZ()") ("ENKE()" . "ENKE()") ("ENDS()" . "ENDS()") ("VOLT()"
						      . "VOLT()") ("MAG()" . "MAG()") ("AX()" . "AX()") ("AY()"
						      . "AY()") ("AZ()" . "AZ()") ("VIRTINQR()"
						      . "VIRTINQR()") ("KWGET()" . "KWGET()") ("VALCHR()"
						      . "VALCHR()") ("VALHEX()" . "VALHEX()") ("CHRHEX()"
						      . "CHRHEX()") ("STRFILL()" . "STRFILL()") ("STRCOMP()"
						      . "STRCOMP()") ("STRPOS()" . "STRPOS()") ("STRLENG()"
						      . "STRLENG()") ("UPCASE()" . "UPCASE()") ("LWCASE()"
						      . "LWCASE()") ("SPLIT()" . "SPLIT()"))
  "Alist of Ansys symbols for completion in Ansys mode.
Each element looks like (VAR . VAR), where the car and cdr
are the same symbol (an Ansys command or variable name).  By
default keywords, get-functions, parametric-function and elements
are completed."
  )

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
  '("*do" "*get\\w*" "*dim" "*set.?"	   ;funny *SET works only with on add. char
    "*ask" "path" "pdef" "*vget" "*vfun" "*mfun" "*vitrp"
    "*toper""*voper" "*moper" "*sread" "*vscfun" "/inq\\w*"
    "/fil\\w*")
  "List of commands which define user variables.
Except the \"=\" assignment.")

(defconst ansys-use-variables		;NEW_C
  '("ARG[1-9]" "AR[1][0-9]")
  "Variable containing the Ansys *USE variables regexp.
ARG[1-9] and AR[1][0-9] are macro local variables and can be
passed to the *USE command.  AR[2-9][0-9] are also pure macro
local variables.")

(defconst ansys-format-commands-regexp	;New
  "\\*[mM][sS][gG]\\|\\*[vV][rR][eE]\\|\\*[vV][wW][rR]\\|\\*[mM][wW][rR]"
  "Regexp of command names which have one or more format lines.")

(defconst ansys-parametric-functions '(("\\(ABS\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(SIGN\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(EXP\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(LOG\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(LOG10\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(SQRT\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(NINT\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(MOD\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(RAND\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(GDIS\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(SIN\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(COS\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(TAN\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(SINH\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(COSH\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(TANH\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(ASIN\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(ACOS\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(ATAN\\)\\s-*(" (1
							   font-lock-function-name-face keep)) ("\\(ATAN2\\)\\s-*(" (1
							   font-lock-function-name-face keep)))
  "Ansys parametric functions."
  )

(defconst     ansys-get-functions     '(("\\(NSEL\\)\\s-*("     1
					 font-lock-function-name-face    keep)    ("\\(ESEL\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(KSEL\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(LSEL\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(ASEL\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(VSEL\\)\\s-*("    1
					 font-lock-function-name-face    keep)   ("\\(NDNEXT\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(ELNEXT\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(KPNEXT\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(LSNEXT\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(ARNEXT\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(VLNEXT\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(CENTRX\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(CENTRY\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(CENTRZ\\)\\s-*("   1
					 font-lock-function-name-face     keep)     ("\\(NX\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(NY\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(NZ\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(KX\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(KY\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(KZ\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(LX\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(LY\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(LZ\\)\\s-*("    1
					 font-lock-function-name-face     keep)    ("\\(LSX\\)\\s-*("    1
					 font-lock-function-name-face     keep)    ("\\(LSY\\)\\s-*("    1
					 font-lock-function-name-face     keep)    ("\\(LSZ\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(NODE\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(KP\\)\\s-*("    1
					 font-lock-function-name-face    keep)   ("\\(DISTND\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(DISTKP\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(DISTEN\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(ANGLEN\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(ANGLEK\\)\\s-*("   1
					 font-lock-function-name-face    keep)    ("\\(NNEAR\\)\\s-*("   1
					 font-lock-function-name-face    keep)    ("\\(KNEAR\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(ENEARN\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(AREAND\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(AREAKP\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(ARNODE\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(NORMNX\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(NORMNY\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(NORMNZ\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(NORMKX\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(NORMKY\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(NORMKZ\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(ENEXTN\\)\\s-*("   1
					 font-lock-function-name-face    keep)    ("\\(NELEM\\)\\s-*("   1
					 font-lock-function-name-face   keep)   ("\\(NODEDOF\\)\\s-*("   1
					 font-lock-function-name-face    keep)    ("\\(ELADJ\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(NDFACE\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(NMFACE\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(ARFACE\\)\\s-*("   1
					 font-lock-function-name-face     keep)     ("\\(UX\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(UY\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(UZ\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(ROTX\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(ROTY\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(ROTZ\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(TEMP\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(PRES\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(VX\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(VY\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(VZ\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(ENKE\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(ENDS\\)\\s-*("    1
					 font-lock-function-name-face    keep)    ("\\(VOLT\\)\\s-*("    1
					 font-lock-function-name-face     keep)    ("\\(MAG\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(AX\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(AY\\)\\s-*("    1
					 font-lock-function-name-face     keep)     ("\\(AZ\\)\\s-*("    1
					 font-lock-function-name-face   keep)   ("\\(VIRTINQR\\)\\s-*("  1
					 font-lock-function-name-face    keep)    ("\\(KWGET\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(VALCHR\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(VALHEX\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(CHRHEX\\)\\s-*("   1
					 font-lock-function-name-face   keep)   ("\\(STRFILL\\)\\s-*("   1
					 font-lock-function-name-face   keep)   ("\\(STRCOMP\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(STRPOS\\)\\s-*("   1
					 font-lock-function-name-face   keep)   ("\\(STRLENG\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(UPCASE\\)\\s-*("   1
					 font-lock-function-name-face    keep)   ("\\(LWCASE\\)\\s-*("   1
					 font-lock-function-name-face    keep)    ("\\(SPLIT\\)\\s-*("   1
					 font-lock-function-name-face keep))
  "Ansys get functions."
  )

(defconst                              ansys-undocumented-commands
  '(("^\\s-*\\(XMLO\\)\\w*\\>"       1      font-lock-constant-face
     prepend)   ("^\\s-*\\(/XML\\)\\w*\\>"  1  font-lock-constant-face
     prepend)   ("^\\s-*\\(CNTR\\)\\w*\\>"  1  font-lock-constant-face
     prepend)  ("^\\s-*\\(EBLOCK\\)\\w*\\>"  1 font-lock-constant-face
     prepend)  ("^\\s-*\\(CMBLOCK\\)\\w*\\>" 1 font-lock-constant-face
     prepend)  ("^\\s-*\\(NBLOCK\\)\\w*\\>"  1 font-lock-constant-face
     prepend)  ("^\\s-*\\(/TRACK\\)\\w*\\>"  1 font-lock-constant-face
     prepend)  ("^\\s-*\\(CWZPLOT\\)\\w*\\>" 1 font-lock-constant-face
     prepend)   ("^\\s-*\\(~EUI\\)\\w*\\>"  1  font-lock-constant-face
     prepend)   ("^\\s-*\\(NELE\\)\\w*\\>"  1  font-lock-constant-face
     prepend)   ("^\\s-*\\(EALL\\)\\w*\\>"  1  font-lock-constant-face
     prepend)   ("^\\s-*\\(NALL\\)\\w*\\>"  1  font-lock-constant-face
     prepend)  ("^\\s-*\\(FLITEM\\)\\w*\\>"  1 font-lock-constant-face
     prepend)  ("^\\s-*\\(/VERIFY\\)\\w*\\>" 1 font-lock-constant-face
     prepend)   ("^\\s-*\\(/SSS\\)\\w*\\>"  1  font-lock-constant-face
     prepend)  ("^\\s-*\\(~CFIN\\)\\w*\\>"  1  font-lock-constant-face
     prepend)  ("^\\s-*\\(*EVAL\\)\\w*\\>"  1  font-lock-constant-face
     prepend)  ("^\\s-*\\(*MOONEY\\)\\w*\\>" 1 font-lock-constant-face
     prepend))
  "Ansys commands not documented in the manuals.
Seen mainly in Workbench output files and Ansys verification
models."
  )

(defconst       ansys-elements      '(("\\<\\(LINK1\\)\\>"      1
				       font-lock-variable-name-face    keep)    ("\\<\\(BEAM3\\)\\>"   1
				       font-lock-variable-name-face    keep)    ("\\<\\(BEAM4\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(SOLID5\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(COMBIN7\\)\\>"   1
				       font-lock-variable-name-face    keep)    ("\\<\\(LINK8\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(INFIN9\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(LINK10\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(LINK11\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(CONTAC12\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE13\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(COMBIN14\\)\\>"  1
				       font-lock-variable-name-face    keep)   ("\\<\\(PIPE16\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(PIPE17\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(PIPE18\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(PIPE20\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(MASS21\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(BEAM23\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(BEAM24\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE25\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(MATRIX27\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL28\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID29\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID30\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(LINK31\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(LINK32\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(LINK33\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(LINK34\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE35\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOURC36\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(COMBIN37\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID38\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(COMBIN39\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(COMBIN40\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL41\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE42\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL43\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(BEAM44\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID45\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID46\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(INFIN47\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(MATRIX50\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(CONTAC52\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE53\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(BEAM54\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE55\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL57\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(PIPE59\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(PIPE60\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL61\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID62\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL63\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID65\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE67\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(LINK68\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID69\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID70\\)\\>"   1
				       font-lock-variable-name-face    keep)   ("\\<\\(MASS71\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE75\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE77\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE78\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID79\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID80\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID81\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE82\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE83\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID87\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(VISCO88\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(VISCO89\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID90\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL91\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID92\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL93\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(CIRCU94\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID95\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID96\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID97\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID98\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL99\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(VISCO106\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(VISCO107\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(VISCO108\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(TRANS109\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(INFIN110\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(INFIN111\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(INTER115\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID116\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID117\\)\\>"  1
				       font-lock-variable-name-face    keep)    ("\\<\\(HF118\\)\\>"   1
				       font-lock-variable-name-face    keep)    ("\\<\\(HF119\\)\\>"   1
				       font-lock-variable-name-face    keep)    ("\\<\\(HF120\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE121\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID122\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID123\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(CIRCU124\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(CIRCU125\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(TRANS126\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID127\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID128\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID129\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID130\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL131\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL132\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID136\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID138\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID139\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID141\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(FLUID142\\)\\>"  1
				       font-lock-variable-name-face    keep)   ("\\<\\(ROM144\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE145\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE146\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID147\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID148\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL150\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SURF151\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SURF152\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SURF153\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SURF154\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SURF156\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL157\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(LINK160\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(BEAM161\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE162\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL163\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID164\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(COMBI165\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(MASS166\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(LINK167\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID168\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(TARGE169\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(TARGE170\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(CONTA171\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(CONTA172\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(CONTA173\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(CONTA174\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(CONTA175\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(CONTA176\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(CONTA177\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(CONTA178\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(PRETS179\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(LINK180\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL181\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE182\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE183\\)\\>"  1
				       font-lock-variable-name-face    keep)   ("\\<\\(MPC184\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID185\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID186\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID187\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(BEAM188\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(BEAM189\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLSH190\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID191\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(INTER192\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(INTER193\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(INTER194\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(INTER195\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(MESH200\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(FOLLW201\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(INTER202\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(INTER203\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(INTER204\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(INTER205\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL208\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL209\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(COMBI214\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE223\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID226\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID227\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(PLANE230\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID231\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SOLID232\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SURF251\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(SURF252\\)\\>"   1
				       font-lock-variable-name-face   keep)   ("\\<\\(REINF265\\)\\>"  1
				       font-lock-variable-name-face   keep)   ("\\<\\(SHELL281\\)\\>"  1
				       font-lock-variable-name-face keep))
  "Ansys elements."
  )

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
  (let ((p (point)))
    (save-excursion
      (back-to-indentation)
      (if (<= p (point)) t nil))))

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

(defconst ansys-dynamic-prompt		;ansys/docu/dynprompt110.ans
  '("*ABBR
*ABBR, Abbr, String" "*AFUN
*AFUN, Lab" "*ASK
*ASK, Par, Query, DVAL" "*CFCLOS
*CFCLOS " "*CFOPEN
*CFOPEN, Fname, Ext, --, Loc" "*CFWRITE
*CFWRITE, Command" "*CREATE
*CREATE, Fname, Ext, --" "*CYCLE
*CYCLE " "*DEL
*DEL, Val1, Val2" "*DIM
*DIM, Par, Type, IMAX, JMAX, KMAX, Var1, Var2, Var3, CSYSID" "*DO
*DO, Par, IVAL, FVAL, INC" "*DOWHILE
*DOWHILE, Par" "*ELSE
*ELSE " "*ELSEIF
*ELSEIF, VAL1, Oper1, VAL2, Conj, VAL3, Oper2, VAL4" "*END
*END " "*ENDDO
*ENDDO " "*ENDIF
*ENDIF " "*EXIT
*EXIT " "*GET
*GET, Par, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM" "*GO
*GO, Base" "*IF
*IF, VAL1, Oper1, VAL2, Base1, VAL3, Oper2, VAL4, Base2" "*LIST - Displays the contents of an external, coded file.
*LIST, Fname, Ext, --" "*MFOURI
*MFOURI, Oper, COEFF, MODE, ISYM, THETA, CURVE" "*MFUN
*MFUN, ParR, Func, Par1" "*MOPER
*MOPER, ParR, Par1, Oper, Par2, Par3, kDim, --, kOut, LIMIT" "*MSG
*MSG, Lab, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8" "*MWRITE
*MWRITE, ParR, Fname, Ext, --, Label, n1, n2, n3" "*REPEAT
*REPEAT, NTOT, VINC1, VINC2, VINC3, VINC4, VINC5, VINC6, VINC7, VINC8, VINC9, VINC10, VINC11" "*RETURN
*RETURN, Level" "*SET
*SET, Par, VALUE, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10" "*SREAD
*SREAD, StrArray, Fname, Ext, --, nChar, nSkip, nRead" "*STATUS
*STATUS, Par, IMIN, IMAX, JMIN, JMAX, KMIN, KMAX, LMIN, LMAX, MMIN, MMAX, KPRI" "*TAXIS
*TAXIS, ParmLoc, nAxis, Val1, Val2, Val3, Val4, Val5, Val6, Val7, Val8, Val9, Val10" "*TOPER
*TOPER, ParR, Par1, Oper, Par2, FACT1, FACT2, CON1" "*TREAD
*TREAD, Par, Fname, Ext, --, NSKIP" "*ULIB
*ULIB, Fname, Ext, --" "*USE
*USE, Name, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7, ARG8, ARG9, AR10, AR11, AR12, AR13, AR14, AG15, AR16, AR17, AR18" "*VABS
*VABS, KABSR, KABS1, KABS2, KABS3" "*VCOL
*VCOL, NCOL1, NCOL2" "*VCUM
*VCUM, KEY" "*VEDIT
*VEDIT, Par" "*VFACT
*VFACT, FACTR, FACT1, FACT2, FACT3" "*VFILL
*VFILL, ParR, Func, CON1, CON2, CON3, CON4, CON5, CON6, CON7, CON8, CON9, CON10" "*VFUN
*VFUN, ParR, Func, Par1, CON1, CON2, CON3" "*VGET
*VGET, ParR, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM, KLOOP" "*VITRP
*VITRP, ParR, ParT, ParI, ParJ, ParK" "*VLEN
*VLEN, NROW, NINC" "*VMASK
*VMASK, Par" "*VOPER
*VOPER, ParR, Par1, Oper, Par2, CON1, CON2" "*VPLOT
*VPLOT, ParX, ParY, Y2, Y3, Y4, Y5, Y6, Y7, Y8" "*VPUT
*VPUT, ParR, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM, KLOOP" "*VREAD
*VREAD, ParR, Fname, Ext, --, Label, n1, n2, n3, NSKIP" "*VSCFUN
*VSCFUN, ParR, Func, Par1" "*VSTAT
*VSTAT " "*VWRITE
*VWRITE, Par1, Par2, Par3, Par4, Par5, Par6, Par7, Par8, Par9, Par10, Par11, Par12, Par13, Par14, Par15, Par16, Par17, Par18, Par19" "/AN3D - Specifies 3-D annotation functions
/AN3D, Kywrd, KEY" "/ANFILE - Saves or resumes an animation sequence to or from a file.
/ANFILE, LAB, Fname, Ext, --" "/ANGLE - Rotates the display about an axis.
/ANGLE, WN, THETA, Axis, KINCR" "/ANNOT - Activates graphics for annotating displays (GUI).
/ANNOT, Lab, VAL1, VAL2" "/ANUM - Specifies the annotation number, type, and hot spot (GUI).
/ANUM, NUM, TYPE, XHOT, YHOT" "/ASSIGN - Reassigns a file name to an ANSYS file identifier.
/ASSIGN, Ident, Fname, Ext, --" "/AUTO - Resets the focus and distance specifications to \"automatically calculated.\"
/AUTO, WN" "/AUX12
/AUX12 - Enters the radiation processor." "/AUX15
/AUX15 - Enters the IGES file transfer processor." "/AUX2
/AUX2 - Enters the binary file dumping processor." "/AUX3
/AUX3 - Enters the results file editing processor." "/AXLAB - Labels the X and Y axes on graph displays.
/AXLAB, Axis, Lab" "/BATCH - Sets the program mode to \"batch.\"
/BATCH, Lab" "/CFORMAT - Controls the graphical display of alphanumeric character strings for parameters, components, assemblies, and tables.
/CFORMAT, NFIRST, NLAST" "/CLABEL - Specifies contour labeling.
/CLABEL, WN, KEY" "/CLEAR - Clears the database.
/CLEAR, Read" "/CLOG - Copies the session log file to a named file.
/CLOG, Fname, Ext, --" "/CMAP - Changes an existing or creates a new color mapping table.
/CMAP, Fname, Ext, Dir, Kywrd, NCNTR" "/COLOR - Specifies the color mapping for various items.
/COLOR, Lab, Clab, N1, N2, NINC" "/COM - Places a comment in the output.
/COM, Comment" "/CONFIG - Assigns values to ANSYS configuration parameters.
/CONFIG, Lab, VALUE" "/CONTOUR - Specifies the uniform contour values on stress displays.
/CONTOUR, WN, NCONT, VMIN, VINC, VMAX" "/COPY - Copies a file.
/COPY, Fname1, Ext1, --, Fname2, Ext2, --" "/CPLANE - Specifies the cutting plane for section and capped displays.
/CPLANE, KEY" "/CTYPE - Specifies the type of contour display.
/CTYPE, KEY, DOTD, DOTS, DSHP, TLEN" "/CVAL - Specifies nonuniform contour values on stress displays.
/CVAL, WN, V1, V2, V3, V4, V5, V6, V7, V8" "/CWD - Changes the current working directory.
/CWD, DIRPATH" "/CYCEXPAND - Graphically expands displacements, stresses and strains of a cyclically symmetric model.
/CYCEXPAND, WN, OPTION, Value1, Value2" "/DELETE - Deletes a file.
/DELETE, Fname, Ext, --" "/DEVDISP - Controls graphics device options.
/DEVDISP, Label, KEY" "/DEVICE - Controls graphics device options.
/DEVICE, Label, KEY" "/DFLAB
/DFLAB, DOF, DispLab, ForceLab" "/DIRECTORY
/DIRECTORY, StrArray, FileName, Ext, Dir" "/DIST - Specifies the viewing distance for magnifications and perspective.
/DIST, WN, DVAL, KFACT" "/DSCALE - Sets the displacement multiplier for displacement displays.
/DSCALE, WN, DMULT" "/DV3D - Sets 3-D device option modes.
/DV3D, Lab, Key" "/EDGE - Displays only the \"edges\" of an object.
/EDGE, WN, KEY, ANGLE" "/EFACET - Specifies the number of facets per element edge for PowerGraphics displays.
/EFACET, NUM" "/EOF
/EOF - Exits the file being read." "/ERASE
/ERASE - Specifies that the screen is to be erased before each display." "/ESHAPE - Displays elements with shapes determined from the real constants or section definition.
/ESHAPE, SCALE, KEY" "/EXIT - Stops the run and returns control to the system.
/EXIT, Slab, Fname, Ext, --" "/EXPAND - Allows the creation of a larger graphic display than represented by the actual finite element analysis model.
/EXPAND, Nrepeat1, Type1, Method1, DX1, DY1, DZ1, Nrepeat2, Type2, Method2, DX2, DY2, DZ2, Nrepeat3, Type3, Method3, DX3, DY3, DZ3" "/FACET - Specifies the facet representation used to form solid model displays.
/FACET, Lab" "/FDELE - Deletes a binary file after it is used.
/FDELE, Ident, Stat" "/FILNAME - Changes the Jobname for the analysis.
/FILNAME, Fname, Key" "/FOCUS - Specifies the focus point (center of the window).
/FOCUS, WN, XF, YF, ZF, KTRANS" "/FORMAT - Specifies format controls for tables.
/FORMAT, NDIGIT, Ftype, NWIDTH, DSIGNF, LINE, CHAR" "/GCMD - Controls the type of element or graph display used for the GPLOT command.
/GCMD, WN, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6, Lab7, Lab8, Lab9, Lab10, Lab11, Lab12" "/GCOLUMN - Allows the user to apply a label to a specified curve.
/GCOLUMN, CURVE, STRING" "/GFILE - Specifies the pixel resolution on Z-buffered graphics files.
/GFILE, SIZE" "/GFORMAT - Specifies the format for the graphical display of numbers.
/GFORMAT, Ftype, NWIDTH, DSIGNF" "/GLINE - Specifies the element outline style.
/GLINE, WN, STYLE" "/GMARKER - Specifies the curve marking style.
/GMARKER, CURVE, KEY, INCR" "/GO
/GO - Reactivates suppressed printout." "/GOLIST
/GOLIST - Reactivates the suppressed data input listing." "/GOPR
/GOPR - Reactivates suppressed printout." "/GRAPHICS - Defines the type of graphics display.
/GRAPHICS, Key" "/GRESUME - Sets graphics settings to the settings on a file.
/GRESUME, Fname, Ext, --" "/GRID - Selects the type of grid on graph displays.
/GRID, KEY" "/GROPT - Sets various line graph display options.
/GROPT, Lab, KEY" "/GRTYP - Selects single or multiple Y-axes graph displays.
/GRTYP, KAXIS" "/GSAVE - Saves graphics settings to a file for later use.
/GSAVE, Fname, Ext, --" "/GST - Turns Graphical Solution Tracking (GST) on or off.
/GST, Lab, Lab2" "/GTHK - Sets line thicknesses for graph lines.
/GTHK, Label, THICK" "/GTYPE - Controls the entities that the GPLOT command displays.
/GTYPE, WN, LABEL, KEY" "/HBC - Determines how boundary condition symbols are displayed in a display window.
/HBC, WN, Key" "/HEADER - Sets page and table heading print controls.
/HEADER, Header, Stitle, Idstmp, Notes, Colhed, Minmax" "/ICLWID - Scales the line width of circuit builder icons.
/ICLWID, FACTOR" "/ICSCALE - Scales the icon size for elements supported in the circuit builder.
/ICSCALE, WN, FACTOR" "/IMAGE - Allows graphics data to be captured and saved.
/IMAGE, Label, Fname, Ext, --" "/INPUT - Switches the input file for the commands that follow.
/INPUT, Fname, Ext, --, LINE, LOG" "/INQUIRE
/INQUIRE, StrArray, FUNC" "/LARC - Creates annotation arcs (GUI).
/LARC, XCENTR, YCENTR, XLRAD, ANGLE1, ANGLE2" "/LIGHT - Specifies the light direction for the display window.
/LIGHT, WN, NUM, INT, XV, YV, ZV, REFL" "/LINE - Creates annotation lines (GUI).
/LINE, X1, Y1, X2, Y2" "/LSPEC - Specifies annotation line attributes (GUI).
/LSPEC, LCOLOR, LINSTL, XLNWID" "/LSYMBOL - Creates annotation symbols (GUI).
/LSYMBOL, X, Y, SYMANG, SYMTYP, SYMSIZ, KEYBMP" "/MAIL
/MAIL, --, Address, Fname, Ext" "/MENU - Activates the Graphical User Interface (GUI).
/MENU, Key" "/MKDIR
/MKDIR, Dir" "/MPLIB - Sets the default material library read and write paths.
/MPLIB, R-W_opt, PATH" "/MREP - Enables you to reissue the graphics command macro \"name\" during a replot or zoom operation.
/MREP, NAME, ARG1, ARG2, ARG3, . . . , ARG4, ARG5, ARG6, ARG7, ARG8, ARG9, ARG10, ARG11, ARG12, ARG13, ARG14, ARG15, ARG16, ARG17, ARG18" "/MSTART - Controls the initial GUI components.
/MSTART, Label, KEY" "/NERR - Limits the number of warning and error messages displayed.
/NERR, NMERR, NMABT, --, IFKEY, NUM" "/NOERASE
/NOERASE - Prevents the screen erase between displays." "/NOLIST
/NOLIST - Suppresses the data input listing." "/NOPR
/NOPR - Suppresses the expanded interpreted input data listing." "/NORMAL - Allows displaying area elements by top or bottom faces.
/NORMAL, WN, KEY" "/NUMBER - Specifies whether numbers, colors, or both are used for displays.
/NUMBER, NKEY" "/OPT
/OPT - Enters the design optimizer." "/OUTPUT - Redirects text output to a file or to the screen.
/OUTPUT, Fname, Ext, --, Loc" "/PAGE - Defines the printout and screen page size.
/PAGE, ILINE, ICHAR, BLINE, BCHAR" "/PBC - Shows boundary condition (BC) symbols and values on displays.
/PBC, Item, --, KEY, MIN, MAX, ABS" "/PBF - Shows magnitude of body force loads on displays.
/PBF, Item, --, KEY" "/PCIRCLE - Creates an annotation circle (GUI).
/PCIRCLE, XCENTR, YCENTR, XLRAD" "/PCOPY - Automatically generates hard copies for HP UNIX work stations.
/PCOPY, KEY" "/PDS
/PDS - Enters the probabilistic design system." "/PICE - Shows initial conditions on elements as contours on displays.
/PICE, Item, --, KEY" "/PLOPTS - Controls graphics options on subsequent displays.
/PLOPTS, Label, KEY" "/PMACRO
/PMACRO " "/PMETH - Activates the p-method solution options in the Graphical User Interface (GUI).
/PMETH, Key, OPTION" "/PMORE - Creates an annotation polygon (GUI).
/PMORE, --, X5, Y5, X6, Y6, X7, Y7, X8, Y8" "/PNUM - Controls entity numbering/coloring on plots.
/PNUM, Label, KEY" "/POLYGON - Creates annotation polygons (GUI).
/POLYGON, NVERT, X1, Y1, X2, Y2, X3, Y3, X4, Y4" "/POST1
/POST1 - Enters the database results postprocessor." "/POST26
/POST26 - Enters the time-history results postprocessor." "/PREP7
/PREP7 - Enters the model creation preprocessor." "/PSEARCH
/PSEARCH, Pname" "/PSF - Shows surface load symbols on model displays.
/PSF, Item, Comp, KEY, KSHELL, Color" "/PSPEC - Creates annotation polygon attributes (GUI).
/PSPEC, PCOLOR, KFILL, KBORDR" "/PSTATUS - Displays the global or window display specifications.
/PSTATUS, WN" "/PSYMB - Shows various symbols on displays.
/PSYMB, Label, KEY" "/PWEDGE - Creates an annotation wedge (GUI).
/PWEDGE, XCENTR, YCENTR, XLRAD, ANGLE1, ANGLE2" "/QUIT
/QUIT - Exits a processor." "/RATIO - Distorts the object geometry.
/RATIO, WN, RATOX, RATOY" "/RENAME - Renames a file.
/RENAME, Fname1, Ext1, --, Fname2, Ext2, --" "/REPLOT - Automatically reissues the last display command for convenience.
/REPLOT, Label" "/RESET
/RESET - Resets display specifications to their initial defaults. " "/RGB - Specifies the RGB color values for indices and contours.
/RGB, Kywrd, PRED, PGRN, PBLU, N1, N2, NINC, NCNTR" "/RMDIR
/RMDIR, Dir" "/RUNST
/RUNST - Enters the run statistics processor. " "/SECLIB - Sets the default section library path for the SECREAD command.
/SECLIB, Option, Path" "/SEG - Allows graphics data to be stored in the local terminal memory.
/SEG, Label, Aviname, DELAY" "/SHADE - Defines the type of surface shading used with Z-buffering.
/SHADE, WN, Type" "/SHOW - Specifies the device and other parameters for graphics displays.
/SHOW, Fname, Ext, VECT, NCPL" "/SHOWDISP - Defines the display driver name.
/SHOWDISP, Dname, --, --, NCPL" "/SHRINK - Shrinks elements, lines, areas, and volumes for display clarity.
/SHRINK, RATIO" "/SMBC - Controls the display of solid model boundary condition symbols and labels.
/SMBC, Mode" "/SOLU
/SOLU - Enters the solution processor." "/SSCALE - Sets the contour multiplier for topographic displays.
/SSCALE, WN, SMULT" "/STATUS - Lists the status of items for the run.
/STATUS, Lab" "/STITLE - Defines subtitles.
/STITLE, NLINE, Title" "/SYP - Passes a command string and arguments to the operating system.
/SYP, String, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7, ARG8" "/SYS - Passes a command string to the operating system.
/SYS, String" "/TEE
/TEE, Label, Fname, Ext, --" "/TITLE - Defines a main title.
/TITLE, Title" "/TLABEL - Creates annotation text (GUI).
/TLABEL, XLOC, YLOC, Text" "/TRIAD - Shows the global XYZ coordinate triad on displays.
/TRIAD, Lab" "/TRLCY - Specifies the level of translucency.
/TRLCY, Lab, TLEVEL, N1, N2, NINC" "/TSPEC - Creates annotation text attributes (GUI).
/TSPEC, TCOLOR, TSIZE, TXTHIC, PANGLE, IANGLE" "/TXTRE - Controls application of texture to selected items.
/TXTRE, Lab, NUM, N1, N2, NINC" "/TYPE - Defines the type of display.
/TYPE, WN, Type" "/UCMD
/UCMD, Cmd, SRNUM" "/UDOC - Determines position and content for the multi-legend options.
/UDOC, WIND, Class, Key," "/UI - Activates specified GUI dialog boxes.
/UI, Func, Type, Format, Screen, Color, Krev, Orient, Compress, Quality" "/UIS - Controls the GUI behavior.
/UIS, Label, VALUE" "/UNITS - Annotates the database with the system of units used.
/UNITS, Label, LENFACT, MASSFACT, TIMEFACT, TEMPFACT, TOFFSET, CHARGEFACT, FORCEFACT, HEATFACT" "/USER - Conveniently resets /FOCUS and /DIST to USER.
/USER, WN" "/VCONE - Defines the view cone angle for perspective displays.
/VCONE, WN, PHI" "/VIEW - Defines the viewing direction for the display.
/VIEW, WN, XV, YV, ZV" "/VSCALE - Scales the length of displayed vectors.
/VSCALE, WN, VRATIO, KEY" "/VT
/VT - Enters the Variational Technology preprocessor." "/VUP - Specifies the global Cartesian coordinate system reference orientation.
/VUP, WN, Label" "/WAIT
/WAIT, DTIME" "/WINDOW - Defines the window size on the screen.
/WINDOW, WN, XMIN, XMAX, YMIN, YMAX, NCOPY" "/XFRM - Controls the centroid or the axis of dynamic rotation.
/XFRM, LAB, X1, Y1, Z1, X2, Y2, Z2" "/XRANGE - Specifies a linear abscissa (X) scale range.
/XRANGE, XMIN, XMAX" "/YRANGE - Specifies a linear ordinate (Y) scale range.
/YRANGE, YMIN, YMAX, NUM" "/ZOOM - Zooms a region of a display window.
/ZOOM, WN, Lab, X1, Y1, X2, Y2" "A - Defines an area by connecting keypoints.
A, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18" "AADD - Adds separate areas to create a single area.
AADD, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "AATT - Associates element attributes with the selected, unmeshed areas.
AATT, MAT, REAL, TYPE, ESYS, SECN" "ABBRES
ABBRES, Lab, Fname, Ext, --" "ABBSAV
ABBSAV, Lab, Fname, Ext, --" "ABEXTRACT - Extracts the alpha-beta damping multipliers for Rayleigh damping.
ABEXTRACT, MODE1, MODE2" "ABS - Forms the absolute value of a variable.
ABS, IR, IA, --, --, Name, --, --, FACTA" "ACCAT - Concatenates multiple areas in preparation for mapped meshing.
ACCAT, NA1, NA2" "ACEL - Specifies the linear acceleration of the structure.
ACEL, ACEL_X, ACEL_Y, ACEL_Z" "ACLEAR - Deletes nodes and area elements associated with selected areas.
ACLEAR, NA1, NA2, NINC" "ADAMS - Performs solutions and writes flexible body information to a modal neutral file (Jobname.MNF) for use in an ADAMS analysis.
ADAMS, NMODES, KSTRESS, KSHELL" "ADAPT - Adaptively meshes and solves a model.
ADAPT, NSOLN, STARGT, TTARGT, FACMN, FACMX, KYKPS, KYMAC" "ADD - Adds variables.
ADD, IR, IA, IB, IC, Name, --, --, FACTA, FACTB, FACTC" "ADDAM - Specifies the acceleration spectrum computation constants for the analysis of shock resistance of shipboard structures.
ADDAM, AF, AA, AB, AC, AD, AMIN" "ADELE - Deletes unmeshed areas.
ADELE, NA1, NA2, NINC, KSWP" "ADGL - Lists keypoints of an area that lie on a parametric degeneracy.
ADGL, NA1, NA2, NINC" "ADRAG - Generates areas by dragging a line pattern along a path.
ADRAG, NL1, NL2, NL3, NL4, NL5, NL6, NLP1, NLP2, NLP3, NLP4, NLP5, NLP6" "AESIZE - Specifies the element size to be meshed onto areas.
AESIZE, ANUM, SIZE," "AFILLT - Generates a fillet at the intersection of two areas.
AFILLT, NA1, NA2, RAD" "AFLIST
AFLIST - Lists the current data in the database." "AFSURF - Generates surface elements overlaid on the surface of existing solid elements and assigns the extra node as the closest fluid element node.
AFSURF, SAREA, TLINE" "AGEN - Generates additional areas from a pattern of areas.
AGEN, ITIME, NA1, NA2, NINC, DX, DY, DZ, KINC, NOELEM, IMOVE" "AGLUE - Generates new areas by \"gluing\" areas.
AGLUE, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "AINA - Finds the intersection of areas.
AINA, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "AINP - Finds the pairwise intersection of areas.
AINP, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "AINV - Finds the intersection of an area with a volume.
AINV, NA, NV" "AL - Generates an area bounded by previously defined lines.
AL, L1, L2, L3, L4, L5, L6, L7, L8, L9, L10" "ALIST - Lists the defined areas.
ALIST, NA1, NA2, NINC, Lab" "ALLSEL - Selects all entities with a single command.
ALLSEL, LabT, Entity" "ALPFILL - Fills in an area loop within an existing 2-D area (for models imported from CAD files).
ALPFILL, LN1, LN2, LN3, LN4, LN5, LN6, LN7, LN8, LN9, LN10" "ALPHAD - Defines the mass matrix multiplier for damping.
ALPHAD, VALUE" "AMAP - Generates a 2-D mapped mesh based on specified area corners.
AMAP, AREA, KP1, KP2, KP3, KP4" "AMESH - Generates nodes and area elements within areas.
AMESH, NA1, NA2, NINC" "ANCNTR - Produces an animated sequence of a contoured deformed shape.
ANCNTR, NFRAM, DELAY, NCYCL" "ANCUT - Produces an animated sequence of Q-slices.
ANCUT, NFRAM, DELAY, NCYCL, QOFF, KTOP, TOPOFF, NODE1, NODE2, NODE3" "ANCYC - Applies a traveling wave animation to graphics data in a modal cyclic symmetry analysis.
ANCYC, NUMFRAMES, KCYCL, DELAY" "ANDATA - Produces a sequential contour animation over a range of results data.
ANDATA, DELAY, NCYCL, RSLTDAT, MIN, MAX, INCR, FRCLST, AUTOCONT, --, AUTOCNTR" "ANDSCL - Produces an animated sequence of a deformed shape.
ANDSCL, NFRAM, DELAY, NCYCL" "ANDYNA - Produces an animated sequence of contour values through substeps.
ANDYNA, DELAY, NCYCL, START, END, INC, AUTOCONTOURKEY" "ANFLOW - Produces an animated sequence of particle flow in a flowing fluid or a charged particle traveling in an electric or magnetic field.
ANFLOW, NFRAM, DELAY, NCYCL, TIME, SPACING, SIZE, LENGTH" "ANHARM - Produces a time-transient animated sequence of time-harmonic results or complex mode shapes.
ANHARM, NFRAM, DELAY, NCYCL" "ANIM - Displays graphics data in animated form.
ANIM, NCYCL, KCYCL, DELAY" "ANISOS - Produces an animated sequence of an isosurface.
ANISOS, NFRAM, DELAY, NCYCL" "ANMODE - Produces an animated sequence of a mode shape.
ANMODE, NFRAM, DELAY, NCYCL, KACCEL" "ANMRES - Performs animation of results over multiple results files in an explicit dynamic structural analysis or fluid flow analysis with remeshing.
ANMRES, DELAY, MIN, MAX, INC, AUTOCNTRKY, FREQ, EXT" "ANORM - Reorients area normals.
ANORM, ANUM, NOEFLIP" "ANSOL - Specifies averaged nodal data to be stored from the results file in the solution coordinate system.
ANSOL, NVAR, NODE, Item, Comp, Name, Mat, Real, Ename" "ANSTOAQWA - Creates an AQWA-LINE input file from the current ANSYS model.
ANSTOAQWA, Fname, VertAxis, Gc, Rho, HWL, DiffKey, SymxKey, SymyKey" "ANSTOASAS - Creates an ASAS input file from the current ANSYS model.
ANSTOASAS, Fname, KEY" "ANTIME - Produces a sequential contour animation over a range of time.
ANTIME, NFRAM, DELAY, NCYCL, AUTOCNTRKY, RSLTDAT, MIN, MAX" "ANTYPE - Specifies the analysis type and restart status.
ANTYPE, Antype, Status, LDSTEP, SUBSTEP, Action" "AOFFST - Generates an area, offset from a given area.
AOFFST, NAREA, DIST, KINC" "AOVLAP - Overlaps areas.
AOVLAP, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "APLOT - Displays the selected areas.
APLOT, NA1, NA2, NINC, DEGEN, SCALE" "APPEND - Reads data from the results file and appends it to the database.
APPEND, LSTEP, SBSTEP, FACT, KIMG, TIME, ANGLE, NSET" "APTN - Partitions areas.
APTN, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "ARCLEN - Activates the arc-length method.
ARCLEN, Key, MAXARC, MINARC" "ARCOLLAPSE - Collapses specified area to a specified line segment (for models imported from CAD files).
ARCOLLAPSE, AREA, LINE" "ARCTRM - Controls termination of the arc-length solution.
ARCTRM, Lab, VAL, NODE, DOF" "ARDETACH - Detaches areas from neighboring geometrical entities (for models imported from CAD files).
ARDETACH, AREA1, AREA2, AINC" "AREAS
AREAS - Specifies \"Areas\" as the subsequent status topic." "AREFINE - Refines the mesh around specified areas.
AREFINE, NA1, NA2, NINC, LEVEL, DEPTH, POST, RETAIN" "AREMESH - Generates an area in which to create a new mesh for rezoning.
AREMESH, LCOMB, ANGLE" "AREVERSE - Reverses the normal of an area, regardless of its connectivity or mesh status.
AREVERSE, ANUM, NOEFLIP" "ARFILL - Creates an area based on a set of singly-connected lines (for models imported from CAD files).
ARFILL, LN1, LN2, LN3, LN4, LN5, LN6, LN7, LN8, LN9, LN10" "ARMERGE - Merges two or more singly-connected adjacent areas (for models imported from CAD files).
ARMERGE, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10" "AROTAT - Generates cylindrical areas by rotating a line pattern about an axis.
AROTAT, NL1, NL2, NL3, NL4, NL5, NL6, PAX1, PAX2, ARC, NSEG" "ARSCALE - Generates a scaled set of areas from a pattern of areas.
ARSCALE, NA1, NA2, NINC, RX, RY, RZ, KINC, NOELEM, IMOVE" "ARSPLIT - Splits an area between two keypoints (for models imported from CAD files).
ARSPLIT, AREA, KP1, KP2, TOL, Factor" "ARSYM - Generates areas from an area pattern by symmetry reflection.
ARSYM, Ncomp, NA1, NA2, NINC, KINC, NOELEM, IMOVE" "ASBA - Subtracts areas from areas.
ASBA, NA1, NA2, SEPO, KEEP1, KEEP2" "ASBL - Subtracts lines from areas.
ASBL, NA, NL, --, KEEPA, KEEPL" "ASBV - Subtracts volumes from areas.
ASBV, NA, NV, SEPO, KEEPA, KEEPV" "ASBW - Subtracts the intersection of the working plane from areas (divides areas).
ASBW, NA, SEPO, KEEP" "ASEL - Selects a subset of areas.
ASEL, Type, Item, Comp, VMIN, VMAX, VINC, KSWP" "ASKIN - Generates an area by \"skinning\" a surface through guiding lines.
ASKIN, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "ASLL - Selects those areas containing the selected lines.
ASLL, Type, ARKEY" "ASLV - Selects those areas contained in the selected volumes.
ASLV, Type" "ASUB - Generates an area using the shape of an existing area.
ASUB, NA1, P1, P2, P3, P4" "ASUM - Calculates and prints geometry statistics of the selected areas.
ASUM, LAB" "ATAN - Forms the arctangent of a complex variable.
ATAN, IR, IA, --, --, Name, --, --, FACTA" "ATRAN - Transfers a pattern of areas to another coordinate system.
ATRAN, KCNTO, NA1, NA2, NINC, KINC, NOELEM, IMOVE" "ATYPE
ATYPE - Specifies \"Analysis types\" as the subsequent status topic." "AUTOTS - Specifies whether to use automatic time stepping or load stepping.
AUTOTS, Key" "AVPRIN - Specifies how principal and vector sums are to be calculated.
AVPRIN, KEY, EFFNU" "AVRES - Specifies how results data will be averaged when PowerGraphics is enabled.
AVRES, KEY, Opt" "BCSOPTION - Sets memory option for the sparse solver.
BCSOPTION, --, Memory_Option, Memory_Size, --, --, Solve_Info" "BELLOW - Defines a bellows in a piping run.
BELLOW, NLOC, LENG, STIFF, FLEX, ELEM" "BEND - Defines a bend in a piping run.
BEND, NEL1, NEL2, RAD, NDIV, ESTRT, EINC" "BETAD - Defines the stiffness matrix multiplier for damping.
BETAD, VALUE" "BF - Defines a nodal body force load.
BF, NODE, Lab, VAL1, VAL2, VAL3, VAL4" "BFA - Defines a body force load on an area.
BFA, AREA, Lab, VAL1, VAL2, VAL3, VAL4" "BFADELE - Deletes body force loads on an area.
BFADELE, AREA, Lab" "BFALIST - Lists the body force loads on an area.
BFALIST, AREA, Lab" "BFCUM - Specifies that nodal body force loads are to be accumulated.
BFCUM, Lab, Oper, FACT, TBASE" "BFDELE - Deletes nodal body force loads.
BFDELE, NODE, Lab" "BFE - Defines an element body force load.
BFE, ELEM, Lab, STLOC, VAL1, VAL2, VAL3, VAL4" "BFECUM - Specifies whether to ignore subsequent element body force loads.
BFECUM, Lab, Oper, FACT, TBASE" "BFEDELE - Deletes element body force loads.
BFEDELE, ELEM, Lab" "BFELIST - Lists the element body force loads.
BFELIST, ELEM, Lab" "BFESCAL - Scales element body force loads.
BFESCAL, Lab, FACT, TBASE" "BFINT - Activates the body force interpolation operation.
BFINT, Fname1, Ext1, --, Fname2, Ext2, --, KPOS, Clab, KSHS, TOLOUT, TOLHGT" "BFK - Defines a body force load at a keypoint.
BFK, KPOI, Lab, VAL1, VAL2, VAL3, PHASE" "BFKDELE - Deletes body force loads at a keypoint.
BFKDELE, KPOI, Lab" "BFKLIST - Lists the body force loads at keypoints.
BFKLIST, KPOI, Lab" "BFL - Defines a body force load on a line.
BFL, LINE, Lab, VAL1, VAL2, VAL3, VAL4" "BFLDELE - Deletes body force loads on a line.
BFLDELE, LINE, Lab" "BFLIST - Lists the body force loads on nodes.
BFLIST, NODE, Lab" "BFLLIST - Lists the body force loads on a line.
BFLLIST, LINE, Lab" "BFSCALE - Scales body force loads at nodes.
BFSCALE, Lab, FACT, TBASE" "BFTRAN
BFTRAN - Transfers solid model body force loads to the finite element model." "BFUNIF - Assigns a uniform body force load to all nodes.
BFUNIF, Lab, VALUE" "BFV - Defines a body force load on a volume.
BFV, VOLU, Lab, VAL1, VAL2, VAL3, PHASE" "BFVDELE - Deletes body force loads on a volume.
BFVDELE, VOLU, Lab" "BFVLIST - Lists the body force loads on a volume.
BFVLIST, VOLU, Lab" "BIOOPT
BIOOPT - Specifies \"Biot-Savart options\" as the subsequent status topic." "BIOT - Calculates the Biot-Savart source magnetic field intensity.
BIOT, Label" "BLC4 - Creates a rectangular area or block volume by corner points.
BLC4, XCORNER, YCORNER, WIDTH, HEIGHT, DEPTH" "BLC5 - Creates a rectangular area or block volume by center and corner points.
BLC5, XCENTER, YCENTER, WIDTH, HEIGHT, DEPTH" "BLOCK - Creates a block volume based on working plane coordinates.
BLOCK, X1, X2, Y1, Y2, Z1, Z2" "BOOL
BOOL - Specifies \"Booleans\" as the subsequent status topic." "BOPTN - Specifies Boolean operation options.
BOPTN, Lab, Value" "BRANCH - Defines the starting point for a piping branch.
BRANCH, NODE, X, Y, Z" "BSAX - Specifies the axial strain and axial force relationship for beam sections.
BSAX, VAL1, VAL2, T" "BSM1 - Specifies the bending curvature and moment relationship in plane XZ for beam sections.
BSM1, VAL1, VAL2, T" "BSM2 - Specifies the bending curvature and moment relationship in plane XY for beam sections.
BSM2, VAL1, VAL2, T" "BSMD - Specifies mass density for a nonlinear general beam section.
BSMD, DENS, T" "BSPLIN - Generates a single line from a spline fit to a series of keypoints.
BSPLIN, P1, P2, P3, P4, P5, P6, XV1, YV1, ZV1, XV6, YV6, ZV6" "BSS1 - Specifies the transverse shear strain and force relationship in plane XZ for beam sections.
BSS1, VAL1, VAL2, T" "BSS2 - Specifies the transverse shear strain and force relationship in plane XY for beam sections.
BSS2, VAL1, VAL2, T" "BSTE - Specifies a thermal expansion coefficient for a nonlinear general beam section.
BSTE, ALPHA, T" "BSTQ - Specifies the cross section twist and torque relationship for beam sections.
BSTQ, VAL1, VAL2, T" "BTOL - Specifies the Boolean operation tolerances.
BTOL, PTOL" "BUCOPT - Specifies buckling analysis options.
BUCOPT, Method, NMODE, SHIFT, LDMULTE" "C***, Comment
C*** Places a blank line and a  comment in the output"
"CALC
CALC - Specifies \"Calculation settings\" as the subsequent status topic." "CAMPBELL - Prepares the result file for a subsequent Campbell diagram of a prestressed structure.
CAMPBELL, Key" "CBDOF - Activates cut boundary interpolation (for submodeling).
CBDOF, Fname1, Ext1, --, Fname2, Ext2, --, KPOS, Clab, KSHS, TOLOUT, TOLHGT" "CDOPT - Specifies format to be used for archiving geometry.
CDOPT, Option" "CDREAD - Reads a file of solid model and database information into the database.
CDREAD, Option, Fname, Ext, --, Fnamei, Exti" "CDWRITE - Writes geometry and load database items to a file.
CDWRITE, Option, Fname, Ext, --, Fnamei, Exti, Fmat" "CE - Defines a constraint equation relating degrees of freedom.
CE, NEQN, CONST, NODE1, Lab1, C1, NODE2, Lab2, C2, NODE3, Lab3, C3" "CECHECK - Check constraint equations and couplings for rigid body motions.
CECHECK, ItemLab, Tolerance, DOF" "CECMOD - Modifies the constant term of a constraint equation during solution.
CECMOD, NEQN, CONST" "CECYC - Generates the constraint equations for a cyclic symmetry analysis
CECYC, Lowname, Highname, Nsector, HIndex, Tolerance, Kmove, Kpairs" "CEDELE - Deletes constraint equations.
CEDELE, NEQN1, NEQN2, NINC, Nsel" "CEINTF - Generates constraint equations at an interface.
CEINTF, TOLER, DOF1, DOF2, DOF3, DOF4, DOF5, DOF6, MoveTol" "CELIST - Lists the constraint equations.
CELIST, NEQN1, NEQN2, NINC, Option" "CENTER - Defines a node at the center of curvature of 2 or 3 nodes.
CENTER, NODE, NODE1, NODE2, NODE3, RADIUS" "CEQN
CEQN - Specifies \"Constraint equations\" as the subsequent status topic." "CERIG - Defines a rigid region.
CERIG, MASTE, SLAVE, Ldof, Ldof2, Ldof3, Ldof4, Ldof5" "CESGEN - Generates a set of constraint equations from existing sets.
CESGEN, ITIME, INC, NSET1, NSET2, NINC" "CFACT - Defines complex scaling factors to be used with operations.
CFACT, RFACTA, IFACTA, RFACTB, IFACTB, RFACTC, IFACTC" "CGLOC - Specifies the origin location of the acceleration coordinate system.
CGLOC, XLOC, YLOC, ZLOC" "CGOMGA - Specifies the rotational velocity of the global origin.
CGOMGA, CGOMX, CGOMY, CGOMZ" "CHECK - Checks current database items for completeness.
CHECK, Sele, Levl" "CHKMSH - Checks area and volume entities for previous meshes.
CHKMSH, Comp" "CINT - Defines parameters associated with contour integral calculations
CINT, Action, par1, par2, par3, par4, par5, par6, par7" "CIRCLE - Generates circular arc lines.
CIRCLE, PCENT, RAD, PAXIS, PZERO, ARC, NSEG" "CISOL - Stores J-integral information in a variable.
CISOL, n, ID, node, Cont" "CLOCAL - Defines a local coordinate system relative to the active coordinate system.
CLOCAL, KCN, KCS, XL, YL, ZL, THXY, THYZ, THZX, PAR1, PAR2" "CLOG - Forms the common log of a variable
CLOG, IR, IA, --, --, Name, --, --, FACTA, FACTB" "CLRMSHLN
CLRMSHLN - Clears meshed entities." "CM - Groups geometry items into a component.
CM, Cname, Entity" "CMACEL - Specifies the translational acceleration of an element component
CMACEL, CM_NAME, CMACEL_X, CMACEL_Y, CMACEL_Z" "CMATRIX - Performs electrostatic field solutions and calculates the self and mutual capacitances between multiple conductors.
CMATRIX, SYMFAC, Condname, NUMCOND, GRNDKEY, Capname" "CMDELE - Deletes a component or assembly definition.
CMDELE, Name" "CMDOMEGA - Specifies the rotational acceleration of an element component about a user-defined rotational axis.
CMDOMEGA, CM_NAME, DOMEGAX, DOMEGAY, DOMEGAZ, X1, Y1, Z1, X2, Y2, Z2" "CMEDIT - Edits an existing assembly.
CMEDIT, Aname, Oper, Cnam1, Cnam2, Cnam3, Cnam4, Cnam5, Cnam6, Cnam7" "CMGRP - Groups components and assemblies into an assembly.
CMGRP, Aname, Cnam1, Cnam2, Cnam3, Cnam4, Cnam5, Cnam6, Cnam7, Cnam8" "CMLIST - Lists the contents of a component or assembly.
CMLIST, Name, Key, Entity" "CMMOD - Modifies the specification of a component.
CMMOD, Cname, Keyword, Value" "CMOMEGA - Specifies the rotational velocity of an element component about a user-defined rotational axis.
CMOMEGA, CM_NAME, OMEGAX, OMEGAY, OMEGAZ, X1, Y1, Z1, X2, Y2, Z2, KSPIN" "CMPLOT - Plots the entities contained in a component or assembly.
CMPLOT, Label, Entity, Keyword" "CMROTATE - Specifies the rotational velocity of an element component about a user-defined rotational axis
CMROTATE, CM_NAME, ROTATX, ROTATY, ROTATZ, X1, Y1, Z1, X2, Y2, Z2" "CMSEL - Selects a subset of components and assemblies.
CMSEL, Type, Name, Entity" "CMSFILE - Specifies a list of component mode synthesis (CMS) results files for plotting results on the assembly.
CMSFILE, Option, Fname, Ext, CmsKey" "CMSOPT - Specifies component mode synthesis (CMS) analysis options.
CMSOPT, CMSMETH, NMODE, FREQB, FREQE, FBDDEF, FBDVAL" "CMWRITE - Writes components and assemblies to a file.
CMWRITE, Fname, Ext, --, --, Fmat" "CNCHECK - Provides and/or adjusts the initial status of contact pairs.
CNCHECK, Option, RID1, RID2, RINC" "CNVTOL - Sets convergence values for nonlinear analyses.
CNVTOL, Lab, VALUE, TOLER, NORM, MINREF" "COMPRESS
COMPRESS - Deletes all specified sets." "CON4 - Creates a conical volume anywhere on the working plane.
CON4, XCENTER, YCENTER, RAD1, RAD2, DEPTH" "CONE - Creates a conical volume centered about the working plane origin.
CONE, RBOT, RTOP, Z1, Z2, THETA1, THETA2" "CONJUG - Forms the complex conjugate of a variable.
CONJUG, IR, IA, --, --, Name, --, --, FACTA" "CORIOLIS - Applies the Coriolis effect to a rotating structure.
CORIOLIS, Option, --, --, RefFrame" "COUPLE
COUPLE - Specifies \"Node coupling\" as the subsequent status topic." "COVAL - Defines PSD cospectral values.
COVAL, TBLNO1, TBLNO2, SV1, SV2, SV3, SV4, SV5, SV6, SV7" "CP - Defines (or modifies) a set of coupled degrees of freedom.
CP, NSET, Lab, NODE1, NODE2, NODE3, NODE4, NODE5, NODE6, NODE7, NODE8, NODE9, NODE10, NODE11, NODE12, NODE13, NODE14, NODE15, NODE16, NODE17" "CPCYC - Couples the two side faces of a cyclically symmetric model for loadings that are the same on every segment.
CPCYC, Lab, TOLER, KCN, DX, DY, DZ, KNONROT" "CPDELE - Deletes coupled degree of freedom sets.
CPDELE, NSET1, NSET2, NINC, Nsel" "CPINTF - Defines coupled degrees of freedom at an interface.
CPINTF, Lab, TOLER" "CPLGEN - Generates sets of coupled nodes from an existing set.
CPLGEN, NSETF, Lab1, Lab2, Lab3, Lab4, Lab5" "CPLIST - Lists the coupled degree of freedom sets.
CPLIST, NSET1, NSET2, NINC, Nsel" "CPMERGE - Merges different couple sets with duplicate degrees of freedom into one couple set.
CPMERGE, Lab" "CPNGEN - Defines, modifies, or adds to a set of coupled degrees of freedom.
CPNGEN, NSET, Lab, NODE1, NODE2, NINC" "CPSGEN - Generates sets of coupled nodes from existing sets.
CPSGEN, ITIME, INC, NSET1, NSET2, NINC" "CQC - Specifies the complete quadratic mode combination method.
CQC, SIGNIF, Label" "CRPLIM - Specifies the creep criterion for automatic time stepping.
CRPLIM, CRCR, Option" "CS - Defines a local coordinate system by three node locations.
CS, KCN, KCS, NORIG, NXAX, NXYPL, PAR1, PAR2" "CSCIR - Locates the singularity for non-Cartesian local coordinate systems.
CSCIR, KCN, KTHET, KPHI" "CSDELE - Deletes local coordinate systems.
CSDELE, KCN1, KCN2, KCINC" "CSKP - Defines a local coordinate system by three keypoint locations.
CSKP, KCN, KCS, PORIG, PXAXS, PXYPL, PAR1, PAR2" "CSLIST - Lists coordinate systems.
CSLIST, KCN1, KCN2, KCINC" "CSWPLA - Defines a local coordinate system at the origin of the working plane.
CSWPLA, KCN, KCS, PAR1, PAR2" "CSYS - Activates a previously defined coordinate system.
CSYS, KCN" "CURR2D
CURR2D - Calculates current flow in a 2-D conductor." "CUTCONTROL - Controls time-step cutback during a nonlinear solution.
CUTCONTROL, Lab, VALUE, Option" "CVAR - Computes covariance between two quantities.
CVAR, IR, IA, IB, ITYPE, DATUM, Name" "CYCLIC - Specifies a cyclic symmetry analysis.
CYCLIC, NSECTOR, ANGLE, KCN, Name, USRCOMP" "CYCOPT - Specifies solution options for a cyclic symmetry analysis.
CYCOPT, OPTION, Value1, Value2, Value3, Value4, Value5, Value6, Value7" "CYCPHASE - Provides tools for determining minimum and maximum possible result values from frequency couplets produced in a modal cyclic symmetry analysis.
CYCPHASE, TYPE, OPTION" "CYL4 - Creates a circular area or cylindrical volume anywhere on the working plane.
CYL4, XCENTER, YCENTER, RAD1, THETA1, RAD2, THETA2, DEPTH" "CYL5 - Creates a circular area or cylindrical volume by end points.
CYL5, XEDGE1, YEDGE1, XEDGE2, YEDGE2, DEPTH" "CYLIND - Creates a cylindrical volume centered about the working plane origin.
CYLIND, RAD1, RAD2, Z1, Z2, THETA1, THETA2" "CZDEL - Edits or clears cohesive zone sections.
CZDEL, g1, g2, g3" "CZMESH - Create and mesh an interface area composed of cohesive zone elements.
CZMESH, ecomps1, ecomps2, KCN, KDIR, VALUE, CZTOL" "D - Defines DOF constraints at nodes.
D, NODE, Lab, VALUE, VALUE2, NEND, NINC, Lab2, Lab3, Lab4, Lab5, Lab6" "DA - Defines DOF constraints on areas.
DA, AREA, Lab, Value1, Value2" "DADELE - Deletes DOF constraints on an area.
DADELE, AREA, Lab" "DALIST - Lists the DOF constraints on an area.
DALIST, AREA" "DAMORPH - Move nodes in selected areas to conform to structural displacements.
DAMORPH, AREA, XLINE, RMSHKY" "DATA - Reads data records from a file into a variable.
DATA, IR, LSTRT, LSTOP, LINC, Name, KCPLX" "DATADEF
DATADEF - Specifies \"Directly defined data status\" as the subsequent status topic." "DCGOMG - Specifies the rotational acceleration of the global origin.
DCGOMG, DCGOX, DCGOY, DCGOZ" "DCUM - Specifies that DOF constraint values are to be accumulated.
DCUM, Oper, RFACT, IFACT, TBASE" "DCVSWP - Performs a DC voltage sweep on a ROM element.
DCVSWP, Option, Elem, Cnum, Vmax, Vinc1, Vinc2, Gap" "DDELE - Deletes degree of freedom constraints.
DDELE, NODE, Lab, NEND, NINC, Rkey" "DEACT
DEACT - Specifies \"Element birth and death\" as the subsequent status topic." "DECOMP - Decomposes the model into domains used by Distributed ANSYS
DECOMP, --, Ndomains" "DEFINE
DEFINE - Specifies \"Data definition settings\" as the subsequent status topic." "DELETE - Specifies sets in the results file to be deleted before postprocessing.
DELETE, SET, Nstart, Nend" "DELTIM - Specifies the time step sizes to be used for this load step.
DELTIM, DTIME, DTMIN, DTMAX, Carry" "DEMORPH - Move nodes in selected elements to conform to structural displacements.
DEMORPH, ELEM, DIMN, RMSHKY" "DERIV - Differentiates a variable.
DERIV, IR, IY, IX, --, Name, --, --, FACTA" "DESIZE - Controls default element sizes.
DESIZE, MINL, MINH, MXEL, ANGL, ANGH, EDGMN, EDGMX, ADJF, ADJM" "DESOL - Defines or modifies solution results at a node of an element.
DESOL, ELEM, NODE, Item, Comp, V1, V2, V3, V4, V5, V6" "DETAB - Modifies element table results in the database.
DETAB, ELEM, Lab, V1, V2, V3, V4, V5, V6" "DIG - Digitizes nodes to a surface.
DIG, NODE1, NODE2, NINC" "DIGIT
DIGIT - Specifies \"Node digitizing\" as the subsequent status topic." "DISPLAY
DISPLAY - Specifies \"Display settings\" as the subsequent status topic." "DJ - Specifies boundary conditions on the components of relative motion of a joint element.
DJ, ELEM, LABEL, VALUE" "DJDELE - Deletes boundary conditions on the components of relative motion of a joint element.
DJDELE, ELEM, LAB" "DJLIST - Lists boundary conditions applied to joint elements.
DJLIST, Elem" "DK - Defines DOF constraints at keypoints.
DK, KPOI, Lab, VALUE, VALUE2, KEXPND, Lab2, Lab3, Lab4, Lab5, Lab6" "DKDELE - Deletes DOF constraints at a keypoint.
DKDELE, KPOI, Lab" "DKLIST - Lists the DOF constraints at keypoints.
DKLIST, KPOI" "DL - Defines DOF constraints on lines.
DL, LINE, AREA, Lab, Value1, Value2" "DLDELE - Deletes DOF constraints on a line.
DLDELE, LINE, Lab" "DLIST - Lists DOF constraints.
DLIST, NODE1, NODE2, NINC" "DLLIST - Lists DOF constraints on a line.
DLLIST, LINE" "DMOVE - Digitizes nodes on surfaces and along intersections.
DMOVE, NODE1, NODE2, NINC" "DMPEXT - Extracts modal damping coefficients in a specified frequency range.
DMPEXT, SMODE, TMODE, Dmpname, Freqb, Freqe, NSTEPS" "DMPRAT - Sets a constant damping ratio.
DMPRAT, RATIO" "DNSOL - Defines or modifies solution results at a node.
DNSOL, NODE, Item, Comp, V1, V2, V3, V4, V5, V6" "DOF - Adds degrees of freedom to the current DOF set.
DOF, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6, Lab7, Lab8, Lab9, Lab10" "DOFSEL - Selects a DOF label set for reference by other commands.
DOFSEL, Type, Dof1, Dof2, Dof3, Dof4, Dof5, Dof6" "DOMEGA - Specifies the rotational acceleration of the structure.
DOMEGA, DOMGX, DOMGY, DOMGZ" "DSCALE - Scales DOF constraint values.
DSCALE, RFACT, IFACT, TBASE" "DSET - Sets the scale and drawing plane orientation for a digitizing tablet.
DSET, NODE1, NODE2, NODE3, DDEV" "DSPOPTION - Sets memory option for the distributed sparse solver.
DSPOPTION, Memory_Option" "DSUM - Specifies the double sum mode combination method.
DSUM, SIGNIF, Label, TD" "DSURF - Defines the surface upon which digitized nodes lie.
DSURF, KCN, XSURF, YSURF, ZSURF" "DSYM - Specifies symmetry or antisymmetry DOF constraints on nodes.
DSYM, Lab, Normal, KCN" "DSYS - Activates a display coordinate system for geometry listings and plots.
DSYS, KCN" "DTRAN
DTRAN - Transfers solid model DOF constraints to the finite element model." "DUMP - Dumps the contents of a binary file.
DUMP, NSTRT, NSTOP" "DVMORPH - Move nodes in selected volumes to conform to structural displacements.
DVMORPH, VOLU, XAREA, RMSHKY" "DYNOPT
DYNOPT - Specifies \"Dynamic analysis options\" as the subsequent status topic." "E - Defines an element by node connectivity.
E, I, J, K, L, M, N, O, P" "EALIVE - Reactivates an element (for the birth and death capability).
EALIVE, ELEM" "EDADAPT - Activates adaptive meshing in an explicit dynamic analysis.
EDADAPT, PART, Key" "EDALE - Assigns mesh smoothing to explicit dynamic elements that use the ALE formulation.
EDALE, Option, --, AFAC, BFAC, --, DFAC, EFAC, START, END" "EDASMP - Creates a part assembly to be used in an explicit dynamic analysis.
EDASMP, Option, ASMID, PART1, PART2, PART3, PART4, PART5, PART6, PART7, PART8, PART9, PART10, PART11, PART12, PART13, PART14, PART15, PART16" "EDBOUND - Defines a boundary plane for sliding or cyclic symmetry.
EDBOUND, Option, Lab, Cname, XC, YC, ZC, Cname2, COPT" "EDBVIS - Specifies global bulk viscosity coefficients for an explicit dynamics analysis.
EDBVIS, QVCO, LVCO" "EDBX - Creates a box shaped volume to be used in a contact definition for explicit dynamics.
EDBX, Option, BOXID, XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX" "EDCADAPT - Specifies adaptive meshing controls for an explicit dynamic analysis.
EDCADAPT, FREQ, TOL, OPT, MAXLVL, BTIME, DTIME, LCID, ADPSIZE, ADPASS, IREFLG, ADPENE, ADPTH, MAXEL" "EDCGEN - Specifies contact parameters for an explicit dynamics analysis.
EDCGEN, Option, Cont, Targ, FS, FD, DC, VC, VDC, V1, V2, V3, V4, BTIME, DTIME, BOXID1, BOXID2" "EDCLIST - Lists contact entity specifications in an explicit dynamics analysis.
EDCLIST, NUM" "EDCMORE - Specifies additional contact parameters for a given contact definition in an explicit dynamic analysis.
EDCMORE, Option, NUM, --, VAL1,VAL2" "EDCNSTR - Defines various types of constraints for an explicit dynamic analysis.
EDCNSTR, Option, Ctype, Comp1, Comp2, VAL1" "EDCONTACT - Specifies contact surface controls for an explicit dynamics analysis.
EDCONTACT, SFSI, RWPN, IPCK, SHTK, PENO, STCC, ORIE, CSPC, PENCHK" "EDCPU - Specifies CPU time limit for an explicit dynamics analysis.
EDCPU, CPUTIME" "EDCRB - Constrains two rigid bodies to act as one in an explicit dynamics analysis.
EDCRB, Option, NEQN, PARTM, PARTS" "EDCSC - Specifies whether to use subcycling in an explicit dynamics analysis.
EDCSC, Key" "EDCTS - Specifies mass scaling and scale factor of computed time step for an explicit dynamics analysis.
EDCTS, DTMS, TSSFAC" "EDCURVE - Specifies data curves for an explicit dynamic analysis.
EDCURVE, Option, LCID, Par1, Par2" "EDDAMP - Defines mass weighted (Alpha) or stiffness weighted (Beta) damping for an explicit dynamics model.
EDDAMP, PART, LCID, VALDMP" "EDDBL - Selects a numerical precision type of the explicit dynamics analysis.
EDDBL, KEY" "EDDC - Deletes or deactivates/reactivates contact surface specifications in an explicit dynamic analysis.
EDDC, Option, Ctype, Cont, Targ" "EDDRELAX - Activates initialization to a prescribed geometry or dynamic relaxation for the explicit analysis.
EDDRELAX, Option, NRCYCK, DRTOL, DFFCTR, DRTERM, TSSFDR, IRELAL, EDTTL" "EDDUMP - Specifies output frequency for the explicit dynamic restart file (d3dump).
EDDUMP, NUM, DT" "EDELE - Deletes selected elements from the model.
EDELE, IEL1, IEL2, INC" "EDENERGY - Specifies energy dissipation controls for an explicit dynamics analysis.
EDENERGY, HGEN, SWEN, SIEN, RLEN" "EDFPLOT - Allows plotting of explicit dynamics forces and other load symbols.
EDFPLOT, Key" "EDGCALE - Defines global ALE controls for an explicit dynamic analysis.
EDGCALE, NADV, METH" "EDHGLS - Specifies the hourglass coefficient for an explicit dynamics analysis.
EDHGLS, HGCO" "EDHIST - Specifies time-history output for an explicit dynamic analysis.
EDHIST, Comp" "EDHTIME - Specifies the time-history output interval for an explicit dynamics analysis.
EDHTIME, NSTEP, DT" "EDINT - Specifies number of integration points for explicit shell and beam output.
EDINT, SHELLIP, BEAMIP" "EDIPART - Defines inertia for rigid parts in an explicit dynamics analysis.
EDIPART, PART, Option, Cvect, TM, IRCS, Ivect, Vvect, CID" "EDIS - Specifies stress initialization in an explicit dynamic full restart analysis.
EDIS, Option, PIDN, PIDO" "EDLCS - Defines a local coordinate system for use in explicit dynamics analysis.
EDLCS, Option, CID, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3" "EDLOAD - Specifies loads for an explicit dynamics analysis.
EDLOAD, Option, Lab, KEY, Cname, Par1, Par2, PHASE, LCID, SCALE, BTIME, DTIME" "EDMP - Defines material properties for an explicit dynamics analysis.
EDMP, Lab, MAT, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6" "EDNB - Defines a nonreflecting boundary in an explicit dynamic analysis.
EDNB, Option, Cname, AD, AS" "EDNDTSD - Allows smoothing of noisy data for explicit dynamics analyses and provides a graphical representation of the data.
EDNDTSD, Vect1, Vect2, DATAP, FITPT, Vect3, Vect4, DISP" "EDNROT - Applies a rotated coordinate nodal constraint in an explicit dynamics analysis.
EDNROT, Option, CID, Cname, DOF1, DOF2, DOF3, DOF4, DOF5, DOF6" "EDOPT - Specifies the type of output for an explicit dynamics analysis.
EDOPT, Option, --, Value" "EDOUT - Specifies time-history output (ASCII format) for an explicit dynamics analysis.
EDOUT, Option" "EDPART - Configures parts for an explicit dynamics analysis.
EDPART, Option, PARTID, Cname" "EDPC - Selects and plots explicit dynamic contact entities.
EDPC, MIN, MAX, INC" "EDPL - Plots a time dependent load curve in an explicit dynamic analysis.
EDPL, LDNUM" "EDPVEL - Applies initial velocities to parts or part assemblies in an explicit dynamic analysis.
EDPVEL, Option, PID, VX, VY, VZ, OMEGAX, OMEGAY, OMEGAZ, XC, YC, ZC, ANGX, ANGY, ANGZ" "EDRC - Specifies rigid/deformable switch controls in an explicit dynamic analysis.
EDRC, Option, NRBF, NCSF, --, DTMAX" "EDRD - Switches a part from deformable to rigid or from rigid to deformable in an explicit dynamic analysis.
EDRD, Option, PART, MRB" "EDREAD - Reads explicit dynamics output into variables for time-history postprocessing.
EDREAD, NSTART, Label, NUM, STEP1, STEP2" "EDRI - Defines inertia properties for a new rigid body that is created when a deformable part is switched to rigid in an explicit dynamic analysis.
EDRI, Option, PART, XC, YC, ZC, TM, IXX, IYY, IZZ, IXY, IYZ, IXZ" "EDRST - Specifies the output interval for an explicit dynamic analysis.
EDRST, NSTEP, DT" "EDRUN - Specify LS-DYNA serial or parallel processing.
EDRUN, Option, Cons, Ncpu" "EDSHELL - Specifies shell computation controls for an explicit dynamics analysis.
EDSHELL, WPAN, SHNU, SHTC, WPBT, SHPL, ITRST" "EDSOLV
EDSOLV - Specifies \"explicit dynamics solution\" as the subsequent status topic." "EDSP - Specifies small penetration checking for contact entities in an explicit dynamic analysis.
EDSP, Option, MIN, MAX, INC" "EDSTART - Specifies status (new or restart) of an explicit dynamics analysis.
EDSTART, RESTART, MEMORY, FSIZE, Dumpfile" "EDTERM - Specifies termination criteria for an explicit dynamic analysis.
EDTERM, Option, Lab, NUM, STOP, MAXC, MINC" "EDTP - Plots explicit elements based on their time step size.
EDTP, OPTION, VALUE1, VALUE2" "EDVEL - Applies initial velocities to nodes or node components in an explicit dynamic analysis.
EDVEL, Option, Cname, VX, VY, VZ, OMEGAX, OMEGAY, OMEGAZ, XC, YC, ZC, ANGX, ANGY, ANGZ" "EDWELD - Defines a massless spotweld or generalized weld for use in an explicit dynamic analysis.
EDWELD, Option, NWELD, N1, N2, SN, SS, EXPN, EXPS, EPSF, TFAIL, NSW, CID" "EDWRITE - Writes explicit dynamics input to an LS-DYNA input file.
EDWRITE, Option, Fname, Ext, --" "EGEN - Generates elements from an existing pattern.
EGEN, ITIME, NINC, IEL1, IEL2, IEINC, MINC, TINC, RINC, CINC, SINC, DX, DY, DZ" "EINTF - Defines two-node elements between coincident or offset nodes.
EINTF, TOLER, K, TLAB, KCN, DX, DY, DZ, KNONROT" "EKILL - Deactivates an element (for the birth and death capability).
EKILL, ELEM" "ELEM
ELEM - Specifies \"Elements\" as the subsequent status topic." "ELIST - Lists the elements and their attributes.
ELIST, IEL1, IEL2, INC, NNKEY, RKEY, PTKEY" "EMAGERR
EMAGERR - Calculates the relative error in an electrostatic or electromagnetic field analysis." "EMATWRITE - Forces the writing of all the element matrices to File.EMAT.
EMATWRITE, Key" "EMF
EMF - Calculates the electromotive force (emf), or voltage drop along a predefined path." "EMFT
EMFT - Summarizes electromagnetic forces and torques." "EMID - Adds or removes midside nodes.
EMID, Key, Edges" "EMIS - Specifies emissivity as a material property for the Radiation Matrix method.
EMIS, MAT, EVALU" "EMODIF - Modifies a previously defined element.
EMODIF, IEL, STLOC, I1, I2, I3, I4, I5, I6, I7, I8" "EMORE - Adds more nodes to the just-defined element.
EMORE, Q, R, S, T, U, V, W, X" "EMSYM - Specifies circular symmetry for electromagnetic sources.
EMSYM, NSECT" "EMTGEN - Generates a set of TRANS126 elements.
EMTGEN, Ncomp, Ecomp, PNcomp, DOF, GAP, GAPMIN, FKN, PER0" "EMUNIT - Specifies the system of units for magnetic field problems.
EMUNIT, Lab, VALUE" "EN - Defines an element by its number and node connectivity.
EN, IEL, I, J, K, L, M, N, O, P" "ENDRELEASE - Specifies degrees of freedom to be decoupled for end release.
ENDRELEASE, --, TOLERANCE, Dof1, Dof2, Dof3, Dof4" "ENERSOL - Specifies the total energies to be stored.
ENERSOL, NVAR, Item, ---, Name" "ENGEN - Generates elements from an existing pattern.
ENGEN, IINC, ITIME, NINC, IEL1, IEL2, IEINC, MINC, TINC, RINC, CINC, SINC, DX, DY, DZ" "ENORM - Reorients shell element normals or line element node connectivity.
ENORM, ENUM" "ENSYM - Generates elements by symmetry reflection.
ENSYM, IINC, --, NINC, IEL1, IEL2, IEINC" "EORIENT - Reorients solid element normals.
EORIENT, Etype, Dir, TOLER" "EPLOT
EPLOT - Produces an element display." "EQSLV - Specifies the type of equation solver.
EQSLV, Lab, TOLER, MULT" "ERASE
ERASE - Explicitly erases the current display." "EREAD - Reads elements from a file.
EREAD, Fname, Ext, --" "EREFINE - Refines the mesh around specified elements.
EREFINE, NE1, NE2, NINC, LEVEL, DEPTH, POST, RETAIN" "EREINF
EREINF - Generates reinforcing elements from selected existing (base) elements." "ERESX - Specifies extrapolation of integration point results.
ERESX, Key" "ERNORM - Controls error estimation calculations.
ERNORM, Key" "ERRANG - Specifies the element range to be read from a file.
ERRANG, EMIN, EMAX, EINC" "ESCHECK - Perform element shape checking for a selected element set.
ESCHECK, Sele, Levl, Defkey" "ESEL - Selects a subset of elements.
ESEL, Type, Item, Comp, VMIN, VMAX, VINC, KABS" "ESIZE - Specifies the default number of line divisions.
ESIZE, SIZE, NDIV" "ESLA - Selects those elements associated with the selected areas.
ESLA, Type" "ESLL - Selects those elements associated with the selected lines.
ESLL, Type" "ESLN - Selects those elements attached to the selected nodes.
ESLN, Type, EKEY, NodeType" "ESLV - Selects elements associated with the selected volumes.
ESLV, Type" "ESOL - Specifies element data to be stored from the results file.
ESOL, NVAR, ELEM, NODE, Item, Comp, Name" "ESORT - Sorts the element table.
ESORT, Item, Lab, ORDER, KABS, NUMB" "ESSOLV - Performs a coupled electrostatic-structural analysis.
ESSOLV, Electit, Strutit, DIMN, MORPHOPT, Mcomp, Xcomp, ELECTOL, STRUTOL, MXLOOP, --, RUSEKY, RESTKY, EISCOMP" "ESTIF - Specifies the matrix multiplier for deactivated elements.
ESTIF, KMULT" "ESURF - Generates elements overlaid on the free faces of existing selected elements.
ESURF, XNODE, Tlab, Shape" "ESYM - Generates elements from a pattern by a symmetry reflection.
ESYM, --, NINC, IEL1, IEL2, IEINC" "ESYS - Sets the element coordinate system attribute pointer.
ESYS, KCN" "ET - Defines a local element type from the element library.
ET, ITYPE, Ename, KOP1, KOP2, KOP3, KOP4, KOP5, KOP6, INOPR" "ETABLE - Fills a table of element values for further processing.
ETABLE, Lab, Item, Comp" "ETCHG - Changes element types to their corresponding types.
ETCHG, Cnv" "ETCONTROL - Control the element technologies used in element formulation (for applicable elements).
ETCONTROL, Eltech, Eldegene" "ETDELE - Deletes element types.
ETDELE, ITYP1, ITYP2, INC" "ETLIST - Lists currently defined element types.
ETLIST, ITYP1, ITYP2, INC" "ETYPE
ETYPE - Specifies \"Element types\" as the subsequent status topic." "EUSORT
EUSORT - Restores original order of the element table." "EWRITE - Writes elements to a file.
EWRITE, Fname, Ext, --, KAPPND, Format" "EXP - Forms the exponential of a variable.
EXP, IR, IA, --, --, Name, --, --, FACTA, FACTB" "EXPAND - Displays the results of a modal cyclic symmetry analysis.
EXPAND, Nrepeat, MODAL, HIndex, Icsys, SctAng, --, Phase" "EXPASS - Specifies an expansion pass of an analysis.
EXPASS, Key" "EXPROFILE - Exports ANSYS interface loads to a CFX Profile file.
EXPROFILE, Ldtype, Load, VALUE, Pname, Fname, Fext, Fdir" "EXPSOL - Specifies the solution to be expanded for reduced analyses.
EXPSOL, LSTEP, SBSTEP, TIMFRQ, Elcalc" "EXTOPT - Controls options relating to the generation of volume elements from area elements.
EXTOPT, Lab, Val1, Val2, Val3" "EXTREM - Lists the extreme values for variables.
EXTREM, NVAR1, NVAR2, NINC" "EXUNIT - Indicates units assumed for an interface load for ANSYS to CFX transfer.
EXUNIT, Ldtype, Load, Untype, Name" "F - Specifies force loads at nodes.
F, NODE, Lab, VALUE, VALUE2, NEND, NINC" "FATIGUE
FATIGUE - Specifies \"Fatigue data status\" as the subsequent status topic." "FC - Provides failure criteria information and activates a data table to input temperature-dependent stress and strain limits.
FC, MAT, Lab1, Lab2, DATA1, DATA2, DATA3, DATA4, DATA5, DATA6" "FCCHECK
FCCHECK - Checks both the strain and stress input criteria for all materials." "FCDELE - Deletes previously defined failure criterion data for the given material.
FCDELE, MAT" "FCLIST - To list what the failure criteria is that you have input.
FCLIST, MAT, --, TEMP" "FCUM - Specifies that force loads are to be accumulated.
FCUM, Oper, RFACT, IFACT" "FDELE - Deletes force loads on nodes.
FDELE, NODE, Lab, NEND, NINC" "FE - Defines a set of fatigue event parameters.
FE, NEV, CYCLE, FACT, Title" "FEBODY
FEBODY - Specifies \"Body loads on elements\" as the subsequent status topic." "FECONS
FECONS - Specifies \"Constraints on nodes\" as the subsequent status topic." "FEFOR
FEFOR - Specifies \"Forces on nodes\" as the subsequent status topic." "FELIST - Lists the fatigue event parameters.
FELIST, NEV1, NEV2, NINC" "FESURF
FESURF - Specifies \"Surface loads on elements\" as the subsequent status topic." "FILE - Specifies the data file where results are to be found.
FILE, Fname, Ext, --" "FILEAUX2 - Specifies the binary file to be dumped.
FILEAUX2, Fname, Ident, --" "FILEAUX3 - Specifies the results file to be edited.
FILEAUX3, Fname, Ext, --" "FILEDISP - Specifies the file containing the graphics data.
FILEDISP, Fname, Ext, --" "FILL - Generates a line of nodes between two existing nodes.
FILL, NODE1, NODE2, NFILL, NSTRT, NINC, ITIME, INC, SPACE" "FILLDATA - Fills a variable by a ramp function.
FILLDATA, IR, LSTRT, LSTOP, LINC, VALUE, DVAL" "FINISH
FINISH - Exits normally from a processor." "FITEM - Identifies items chosen by a picking operation (GUI).
FITEM, NFIELD, ITEM, ITEMY, ITEMZ" "FJ - Specify forces or moments on the components of the relative motion of a joint element.
FJ, ELEM, LABEL, VALUE" "FJDELE - Deletes forces (or moments) on the components of the relative motion of a joint element.
FJDELE, ELEM, LAB" "FJLIST - Lists forces and moments applied on joint elements.
FJLIST, Elem" "FK - Defines force loads at keypoints.
FK, KPOI, Lab, VALUE, VALUE2" "FKDELE - Deletes force loads at a keypoint.
FKDELE, KPOI, Lab" "FKLIST - Lists the forces at keypoints.
FKLIST, KPOI, Lab" "FL - Defines a set of fatigue location parameters.
FL, NLOC, NODE, SCFX, SCFY, SCFZ, Title" "FLANGE - Defines a flange in a piping run.
FLANGE, NLOC, LENG, MASS, SIF, FLEX, ARINS, ELEM" "FLDATA - Sets up a FLOTRAN analysis.
FLDATA, Name, Label, Value" "FLDATA1 - Controls which features of the solution algorithm are activated.
FLDATA1, SOLU, Label, Value" "FLDATA10 - Specifies the COF2 coefficient of the fluid property equation.
FLDATA10, COF2, Label, Value" "FLDATA11 - Specifies the COF3 coefficient of the fluid property equation.
FLDATA11, COF3, Label, Value" "FLDATA12 - Sets the property update frequency flag.
FLDATA12, PROP, Label, Value" "FLDATA13 - Sets the property variation flag.
FLDATA13, VARY, Label, Value" "FLDATA14 - Specifies the reference temperature.
FLDATA14, TEMP, Label, Value" "FLDATA15 - Specifies the reference pressure.
FLDATA15, PRES, Label, Value" "FLDATA16 - Specifies the bulk modulus parameter.
FLDATA16, BULK, Label, Value" "FLDATA17 - Specifies the specific heat ratio.
FLDATA17, GAMM, Label, Value" "FLDATA18 - Selects the algebraic solver.
FLDATA18, METH, Label, Value" "FLDATA19 - Specifies the number of TDMA sweeps.
FLDATA19, TDMA, Label, Value" "FLDATA2 - Sets iteration and output controls for steady state analyses.
FLDATA2, ITER, Label, Value" "FLDATA20 - Specifies the number of conjugate direction search vectors.
FLDATA20, SRCH, Label, Value" "FLDATA20A - Specifies the amount of fill-in when preconditioning the coefficient matrix.
FLDATA20A, PGMR, Label, Value" "FLDATA20B - Specifies the number of fill-ins for the ILU preconditioner.
FLDATA20B, PBCGM, Label, Value" "FLDATA21 - Specifies the convergence criterion for FLOTRAN algebraic solvers.
FLDATA21, CONV, Label, Value" "FLDATA22 - Specifies the maximum number of semi-direct iterations.
FLDATA22, MAXI, Label, Value" "FLDATA23 - Specifies the solver minimum normalized rate of change.
FLDATA23, DELT, Label, Value" "FLDATA24 - Sets the turbulence model and the constants used in the Standard k-? Model and the Zero Equation Turbulence Model.
FLDATA24, TURB, Label, Value" "FLDATA24A - Sets constants for the Re-Normalized Group Turbulence Model (RNG).
FLDATA24A, RNGT, Label, Value" "FLDATA24B - Sets constants for the k-? Turbulence Model due to Shih (NKE).
FLDATA24B, NKET, Label, Value" "FLDATA24C - Sets constants for the Nonlinear Turbulence Model of Girimaji (GIR).
FLDATA24C, GIRT, Label, Value" "FLDATA24D - Sets constants for the Shih, Zhu, Lumley Turbulence Model (SZL).
FLDATA24D, SZLT, Label, Value" "FLDATA24E - Sets constants for the k-? turbulence model.
FLDATA24E, SKWT, Label, Value" "FLDATA24F - Sets the turbulent production clip factor for the Shear Stress Transport (SST) turbulence model.
FLDATA24F, SST1, Label, Value" "FLDATA24G - Sets constants in the k-? regime for the Shear Stress Transport (SST) turbulence model.
FLDATA24G, SST1, Label, Value" "FLDATA24H - Sets constants in the k-? regime for the Shear Stress Transport (SST) turbulence model.
FLDATA24H, SST2, Label, Value" "FLDATA25 - Sets solution and property relaxation factors.
FLDATA25, RELX, Label, Value" "FLDATA26 - Sets stability controls.
FLDATA26, STAB, Label, Value" "FLDATA27 - Controls dependent variable printing.
FLDATA27, PRIN, Label, Value" "FLDATA28 - Specifies that variable results are to be replaced.
FLDATA28, MODR, Label, Value" "FLDATA29 - Re-initializes a results variable.
FLDATA29, MODV, Label, Value" "FLDATA3 - Sets the convergence monitors for the degree of freedom set.
FLDATA3, TERM, Label, Value" "FLDATA30 - Controls the quadrature orders.
FLDATA30, QUAD, Label, Value" "FLDATA31 - Specifies dependent variable caps.
FLDATA31, CAPP, Label, Value" "FLDATA32 - Controls restart options.
FLDATA32, REST, Label, Value, Value2, Fname, Ext, --" "FLDATA33 - Specifies the approach to discretize the advection term.
FLDATA33, ADVM, Label, Value" "FLDATA34 - Sets modified inertial relaxation factors.
FLDATA34, MIR, Label, Value" "FLDATA35 - Specifies tolerances for the lower and upper bound of the volume fraction.
FLDATA35, VFTOL, Label, Value" "FLDATA36 - Specifies ambient reference values outside of the fluid for the volume of fluid (VOF) method.
FLDATA36, AMBV, Label, Value" "FLDATA37 - Specifies segregated solution or film coefficient algorithms.
FLDATA37, ALGR, Label, Value" "FLDATA38 - Specifies the mass type for a fluid transient analysis.
FLDATA38, MASS, Label, Value" "FLDATA39 - Specifies remeshing parameters for transient fluid flow and fluid-solid interaction analyses.
FLDATA39, REMESH, Label, Value" "FLDATA4 - Sets controls for transient analyses based on transient time and convergence monitors or sets time integration method.
FLDATA4, TIME, Label, Value" "FLDATA40 - Controls activation of thermal stabilization near walls.
FLDATA40, WADV, Label, Value" "FLDATA4A - Sets controls for transient analyses based on the number of time steps.
FLDATA4A, STEP, Label, Value" "FLDATA5 - Sets output and storage controls.
FLDATA5, OUTP, Label, Value" "FLDATA6 - Controls the output of the convergence monitor.
FLDATA6, CONV, Label, Value" "FLDATA7 - Specifies the type of fluid property.
FLDATA7, PROT, Label, Value" "FLDATA8 - Specifies the NOMI coefficient of the fluid property equation.
FLDATA8, NOMI, Label, Value" "FLDATA9 - Specifies the COF1 coefficient of the fluid property equation.
FLDATA9, COF1, Label, Value" "FLIST - Lists force loads on the nodes.
FLIST, NODE1, NODE2, NINC" "FLLIST - Lists the fatigue location parameters.
FLLIST, NLOC1, NLOC2, NINC" "FLOCHECK - Sets up and runs a zero-iteration FLOTRAN analysis.
FLOCHECK, Key" "FLOTRAN
FLOTRAN - Specifies \"FLOTRAN data settings\" as the subsequent status topic." "FLREAD - Reads the residual file written by the FLOTRAN CFD option.
FLREAD, Fname, Ext, --" "FLST - Specifies data required for a picking operation (GUI).
FLST, NFIELD, NARG, TYPE, Otype, LENG" "FLUXV
FLUXV - Calculates the flux passing through a closed contour." "FMAGBC - Applies force and torque boundary conditions to an element component.
FMAGBC, Cnam1, Cnam2, Cnam3, Cnam4, Cnam5, Cnam6, Cnam7, Cnam8, Cnam9" "FMAGSUM - Summarizes electromagnetic force calculations on element components.
FMAGSUM, Cnam1, Cnam2, Cnam3, Cnam4, Cnam5, Cnam6, Cnam7, Cnam8, Cnam9" "FOR2D
FOR2D - Calculates magnetic forces on a body." "FORCE - Selects the element nodal force type for output.
FORCE, Lab" "FORM - Specifies the format of the file dump.
FORM, Lab" "FP - Defines the fatigue S vs. N and Sm vs. T tables.
FP, STITM, C1, C2, C3, C4, C5, C6" "FPLIST
FPLIST - Lists the property table stored for fatigue evaluation." "FREQ - Defines the frequency points for the SV vs. FREQ tables.
FREQ, FREQ1, FREQ2, FREQ3, FREQ4, FREQ5, FREQ6, FREQ7, FREQ8, FREQ9" "FRQSCL - Turns on automatic scaling of the entire mass matrix and frequency range for modal analyses using the Block Lanczos or PCG Lanczos mode extraction method.
FRQSCL, Scaling" "FS - Stores fatigue stress components at a node.
FS, NODE, NEV, NLOD, STITM, C1, C2, C3, C4, C5, C6" "FSCALE - Scales force load values in the database.
FSCALE, RFACT, IFACT" "FSDELE - Deletes a stress condition for a fatigue location, event, and loading.
FSDELE, NLOC, NEV, NLOD" "FSLIST - Lists the stresses stored for fatigue evaluation.
FSLIST, NLOC1, NLOC2, NINC, NEV, NLOD" "FSNODE - Calculates and stores the stress components at a node for fatigue.
FSNODE, NODE, NEV, NLOD" "FSPLOT - Displays a fatigue stress item for a fatigue location and event.
FSPLOT, NLOC, NEV, ITEM" "FSSECT - Calculates and stores total linearized stress components.
FSSECT, RHO, NEV, NLOD, KBR" "FSSPARM - Calculates reflection and transmission properties of a frequency selective surface.
FSSPARM, PORT1, PORT2" "FSUM - Sums the nodal force and moment contributions of elements.
FSUM, LAB, ITEM" "FTCALC - Performs fatigue calculations for a particular node location.
FTCALC, NLOC, NODE" "FTRAN
FTRAN - Transfers solid model forces to the finite element model." "FTSIZE - Defines the fatigue data storage array.
FTSIZE, MXLOC, MXEV, MXLOD" "FTWRITE - Writes all currently stored fatigue data on a file.
FTWRITE, Fname, Ext, --" "FVMESH - Generates nodes and tetrahedral volume elements from detached exterior area elements (facets).
FVMESH, KEEP" "GAP
GAP - Specifies \"Reduced transient gap conditions\" as the subsequent status topic." "GAPF - Defines the gap force data to be stored in a variable.
GAPF, NVAR, NUM, Name" "GAPFINISH
GAPFINISH - Exits from the CAD import topology repair stage." "GAPLIST - Lists all joined or disjoined lines in a model (for models imported from CAD files).
GAPLIST, Lab" "GAPMERGE - Merges adjacent disjoined lines (for models imported from CAD files).
GAPMERGE, Lab, VAL1, VAL2, VAL3" "GAPOPT - Sets preferences for the CAD import repair commands.
GAPOPT, Lab, Value" "GAPPLOT - Plots all joined or disjoined lines (for models imported from CAD files).
GAPPLOT, Lab" "GAUGE - Gauges the problem domain for an edge-element formulation.
GAUGE, Opt" "GENOPT
GENOPT - Specifies \"General options\" as the subsequent status topic." "GEOM - Defines the geometry specifications for the radiation matrix calculation.
GEOM, K2D, NDIV" "GEOMETRY
GEOMETRY - Specifies \"Geometry\" as the subsequent status topic." "GMATRIX - Performs electric field solutions and calculates the self and mutual conductance between multiple conductors.
GMATRIX, SYMFAC, Condname, NUMCOND, --, Matrixname" "GMFACE - Specifies the facet representation used to form solid models.
GMFACE, Lab, N" "GP - Defines a gap condition for transient analyses.
GP, NODE1, NODE2, Lab, STIF, GAP, DAMP" "GPDELE - Deletes gap conditions.
GPDELE, GAP1, GAP2, GINC" "GPLIST - Lists the gap conditions.
GPLIST, GAP1, GAP2, GINC" "GPLOT
GPLOT - Controls general plotting." "GRP - Specifies the grouping mode combination method.
GRP, SIGNIF, Label" "GSBDATA - Specifies the constraints or applies the load at the ending point for generalized plane strain option.
GSBDATA, LabZ, VALUEZ, LabX, VALUEX, LabY, VALUEY" "GSGDATA - Specifies the reference point and defines the geometry in the fiber direction for the generalized plane strain element option.
GSGDATA, LFIBER, XREF, YREF, ROTX0, ROTY0" "GSLIST - When using generalized plane strain, lists the input data or solutions.
GSLIST, Lab" "GSSOL - Specifies which results to store from the results file when using generalized plane strain.
GSSOL, NVAR, Item, Comp, Name" "GSUM
GSUM - Calculates and prints geometry items." "HARFRQ - Defines the frequency range in the harmonic response analysis.
HARFRQ, FREQB, FREQE" "HBMAT - Writes an assembled global matrix in Harwell-Boeing format.
HBMAT, Fname, Ext, --, Form, Matrx, Rhs, Mapping" "HELP - Displays help information on ANSYS commands and element types.
HELP, Name" "HELPDISP - Displays help information on DISPLAY program commands.
HELPDISP, Commandname" "HEMIOPT - Specifies options for Hemicube view factor calculation.
HEMIOPT, HRES" "HFADP - Turns a high-frequency adaptive error calculation on or off.
HFADP, Lab" "HFANG - Defines or displays spatial angles of a spherical radiation surface for antenna parameter calculations.
HFANG, Lab, PHI1, PHI2, THETA1, THETA2" "HFARRAY - Defines phased array antenna characteristics.
HFARRAY, NUMX, NUMY, PX, PY, SKEW, PHASEX, PHASEY" "HFDEEM - Calibrates S-parameter phase shift.
HFDEEM, Filename, Snp, PORTNUM1, L1, Filename1, Ext1, PORTNUM2, L2, Filename2, Ext2" "HFEIGOPT - Specifies high frequency electromagnetic modal analysis options.
HFEIGOPT, Lab, Val1" "HFEREFINE - Automatically refines high-frequency tetrahedral elements (HF119) or lists high-frequency brick elements (HF120) with the largest error.
HFEREFINE, FACTOR, NUMLIST" "HFMODPRT - Calculates electromagnetic field distribution for a modal port.
HFMODPRT, FREQ" "HFNEAR - Calculates the electromagnetic field at points in the near zone exterior to the equivalent source surface (flagged with the Maxwell surface flag in the preprocessor).
HFNEAR, Lab, VAL, X, Y, Z, CS" "HFPA - Specifies a radiation scan angle for a phased array antenna analysis.
HFPA, Lab, Local, VAL1, VAL2" "HFPCSWP - Calculates the propagating constants of a transmission line or waveguide over a frequency range.
HFPCSWP, FREQB, FREQE, FREQINC, Nummode" "HFPORT - Specifies input data for waveguide, modal, lumped gap, or plane wave ports.
HFPORT, Portnum, Porttype, Local, Opt1, Opt2, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8" "HFPOWER - Calculates power terms of a multi-port network.
HFPOWER, ARG1, ARG2" "HFSCAT - Specifies a high-frequency scattering analysis.
HFSCAT, Lab" "HFSWEEP - Performs a harmonic response for a high-frequency electromagnetic wave guide analysis.
HFSWEEP, FREQB, FREQE, FREQINC, Portin, Port2, Port3, Port4, Pvolt, Pang, Pdist, Vpath, Ipath, Vsymm, Isymm" "HFSYM - Indicates the presence of symmetry planes for the computation of high-frequency electromagnetic fields in the near and far field domains (beyond the finite element region).
HFSYM, KCN, Xkey, Ykey, Zkey" "HMAGSOLV - Specifies 2-D or axisymmetric harmonic magnetic solution options and initiates the solution.
HMAGSOLV, FREQ, NRAMP, CNVA, CNVV, CNVC, CNVE, NEQIT" "HPGL - Specifies various HP options.
HPGL, Kywrd, Opt1, Opt2" "HPTCREATE - Defines a hard point.
HPTCREATE, TYPE, ENTITY, NHP, LABEL, VAL1, VAL2, VAL3" "HPTDELETE - Deletes selected hardpoints.
HPTDELETE, NP1, NP2, NINC" "HRCPLX - Computes and stores in the database the time-harmonic solution at a prescribed phase angle.
HRCPLX, LOADSTEP, SUBSTEP, OMEGAT, 1STLCASE, 2NDLCASE" "HREXP - Specifies the phase angle for the harmonic analysis expansion pass.
HREXP, ANGLE" "HROPT - Specifies harmonic analysis options.
HROPT, Method, MAXMODE, MINMODE, MCout, Damp" "HROUT - Specifies the harmonic analysis output options.
HROUT, Reimky, Clust, Mcont" "IC - Specifies initial conditions at nodes.
IC, NODE, Lab, VALUE, VALUE2, NEND, NINC" "ICDELE
ICDELE - Deletes initial conditions at nodes." "ICE - Specifies initial conditions on elements.
ICE, ELEM, Lab, VALUE" "ICEDELE - Deletes initial conditions on elements.
ICEDELE, ELEM, Lab" "ICELIST - Lists initial conditions on elements.
ICELIST, ELEM, Lab" "ICLIST - Lists the initial conditions.
ICLIST, NODE1, NODE2, NINC, Lab" "ICVFRC - Sets the initial volume fraction field for a geometry.
ICVFRC, Geom, VAL1, VAL2, VAL3, VAL4" "IGESIN - Transfers IGES data from a file into ANSYS.
IGESIN, Fname, Ext, --" "IGESOUT - Writes solid model data to a file in IGES Version 5.1 format.
IGESOUT, Fname, Ext, --, ATT" "IMAGIN - Forms an imaginary variable from a complex variable.
IMAGIN, IR, IA, --, --, Name, --, --, FACTA" "IMESH - Generates nodes and interface elements along lines or areas.
IMESH, LAKY, NSLA, NTLA, KCN, DX, DY, DZ, TOL" "IMMED - Allows immediate display of a model as it is generated.
IMMED, KEY" "IMPD - Calculates the impedance of a conductor at a reference plane.
IMPD, Vpath, Ipath, Vsymm, Isymm" "INISTATE - Defines initial state data and parameters.
INISTATE, Action, par1, par2, par3, par4, par5, par6, par7, par8, par9" "INRES - Identifies the data to be retrieved from the results file.
INRES, Item1, Item2, Item3, Item4, Item5, Item6, Item7, Item8" "INRTIA
INRTIA - Specifies Inertial loads as the subsequent status topic." "INT1 - Integrates a variable.
INT1, IR, IY, IX, --, Name, --, --, FACTA, FACTB, CONST" "INTSRF - Integrates nodal results on an exterior surface.
INTSRF, Lab" "IOPTN - Controls options relating to importing a model.
IOPTN, Lab, VAL1" "IRLF - Specifies that inertia relief calculations are to be performed.
IRLF, KEY" "IRLIST
IRLIST - Prints inertia relief summary table." "ISFILE - Reads an initial stress state from a file into ANSYS.
ISFILE, Option, Fname, Ext, --, LOC, MAT1, MAT2, MAT3, MAT4, MAT5, MAT6, MAT7, MAT8, MAT9, MAT10" "ISTRESS -
ISTRESS, Sx, Sy, Sz, Sxy, Syz, Sxz, MAT1, MAT2, MAT3, MAT4, MAT5, MAT6, MAT7, MAT8, MAT9, MAT10" "ISWRITE - Writes an ASCII file containing the initial stress values.
ISWRITE, Switch" "JPEG - Provides JPEG file export for ANSYS displays.
JPEG, Kywrd, OPT" "JSOL - Specifies result items to be stored for the joint element.
JSOL, NVAR, ELEM, ITEM, COMP, Name" "K - Defines a keypoint.
K, NPT, X, Y, Z" "KATT - Associates attributes with the selected, unmeshed keypoints.
KATT, MAT, REAL, TYPE, ESYS" "KBC - Specifies stepped or ramped loading within a load step.
KBC, KEY" "KBETW - Creates a keypoint between two existing keypoints.
KBETW, KP1, KP2, KPNEW, Type, VALUE" "KCALC - Calculates stress intensity factors in fracture mechanics analyses.
KCALC, KPLAN, MAT, KCSYM, KLOCPR" "KCENTER - Creates a keypoint at the center of a circular arc defined by three locations.
KCENTER, Type, VAL1, VAL2, VAL3, VAL4, KPNEW" "KCLEAR - Deletes nodes and point elements associated with selected keypoints.
KCLEAR, NP1, NP2, NINC" "KDELE - Deletes unmeshed keypoints.
KDELE, NP1, NP2, NINC" "KDIST - Calculates and lists the distance between two keypoints.
KDIST, KP1, KP2" "KEEP - Stores POST26 definitions and data during active session.
KEEP, Key" "KESIZE - Specifies the edge lengths of the elements nearest a keypoint.
KESIZE, NPT, SIZE, FACT1, FACT2" "KEYOPT - Sets element key options.
KEYOPT, ITYPE, KNUM, VALUE" "KEYPTS
KEYPTS - Specifies \"Keypoints\" as the subsequent status topic." "KEYW - Sets a keyword used by the GUI for context filtering (GUI).
KEYW, Keyword, KEY" "KFILL - Generates keypoints between two keypoints.
KFILL, NP1, NP2, NFILL, NSTRT, NINC, SPACE" "KGEN - Generates additional keypoints from a pattern of keypoints.
KGEN, ITIME, NP1, NP2, NINC, DX, DY, DZ, KINC, NOELEM, IMOVE" "KL - Generates a keypoint at a specified location on an existing line.
KL, NL1, RATIO, NK1" "KLIST - Lists the defined keypoints or hard points.
KLIST, NP1, NP2, NINC, Lab" "KMESH - Generates nodes and point elements at keypoints.
KMESH, NP1, NP2, NINC" "KMODIF - Modifies an existing keypoint.
KMODIF, NPT, X, Y, Z" "KMOVE - Calculates and moves a keypoint to an intersection.
KMOVE, NPT, KC1, X1, Y1, Z1, KC2, X2, Y2, Z2" "KNODE - Defines a keypoint at an existing node location.
KNODE, NPT, NODE" "KPLOT - Displays the selected keypoints.
KPLOT, NP1, NP2, NINC, Lab" "KPSCALE - Generates a scaled set of (meshed) keypoints from a pattern of keypoints.
KPSCALE, NP1, NP2, NINC, RX, RY, RZ, KINC, NOELEM, IMOVE" "KREFINE - Refines the mesh around specified keypoints.
KREFINE, NP1, NP2, NINC, LEVEL, DEPTH, POST, RETAIN" "KSCALE - Generates a scaled pattern of keypoints from a given keypoint pattern.
KSCALE, KINC, NP1, NP2, NINC, RX, RY, RZ" "KSCON - Specifies a keypoint about which an area mesh will be skewed.
KSCON, NPT, DELR, KCTIP, NTHET, RRAT" "KSEL - Selects a subset of keypoints or hard points.
KSEL, Type, Item, Comp, VMIN, VMAX, VINC, KABS" "KSLL - Selects those keypoints contained in the selected lines.
KSLL, Type" "KSLN - Selects those keypoints associated with the selected nodes.
KSLN, Type" "KSUM
KSUM - Calculates and prints geometry statistics of the selected keypoints." "KSYMM - Generates a reflected set of keypoints.
KSYMM, Ncomp, NP1, NP2, NINC, KINC, NOELEM, IMOVE" "KTRAN - Transfers a pattern of keypoints to another coordinate system.
KTRAN, KCNTO, NP1, NP2, NINC, KINC, NOELEM, IMOVE" "KUSE - Specifies whether or not to reuse the triangularized matrix.
KUSE, KEY" "KWPAVE - Moves the working plane origin to the average location of keypoints.
KWPAVE, P1, P2, P3, P4, P5, P6, P7, P8, P9" "KWPLAN - Defines the working plane using three keypoints.
KWPLAN, WN, KORIG, KXAX, KPLAN" "L - Defines a line between two keypoints.
L, P1, P2, NDIV, SPACE, XV1, YV1, ZV1, XV2, YV2, ZV2" "L2ANG - Generates a line at an angle with two existing lines.
L2ANG, NL1, NL2, ANG1, ANG2, PHIT1, PHIT2" "L2TAN - Generates a line tangent to two lines.
L2TAN, NL1, NL2" "LANG - Generates a straight line at an angle with a line.
LANG, NL1, P3, ANG, PHIT, LOCAT" "LARC - Defines a circular arc.
LARC, P1, P2, PC, RAD" "LAREA - Generates the shortest line between two keypoints on an area.
LAREA, P1, P2, NAREA" "LARGE - Finds the largest (the envelope) of three variables.
LARGE, IR, IA, IB, IC, Name, --, --, FACTA, FACTB, FACTC" "LATT - Associates element attributes with the selected, unmeshed lines.
LATT, MAT, REAL, TYPE, --, KB, KE, SECNUM" "LAYER - Specifies the element layer for which data are to be processed.
LAYER, NUM" "LAYERP26 - Specifies the element layer for which data are to be stored.
LAYERP26, NUM" "LAYLIST - Lists real constants material properties for layered elements.
LAYLIST, IEL, LAYR1, LAYR2, Mplab1, Mplab2" "LAYPLOT - Displays the layer stacking sequence for layered elements.
LAYPLOT, IEL, LAYR1, LAYR2" "LCABS - Specifies absolute values for load case operations.
LCABS, LCNO, KABS" "LCASE - Reads a load case into the database.
LCASE, LCNO" "LCCALC
LCCALC - Specifies \"Load case settings\" as the subsequent status topic." "LCCAT - Concatenates multiple lines into one line for mapped meshing.
LCCAT, NL1, NL2" "LCDEF - Creates a load case from a set of results on a results file.
LCDEF, LCNO, LSTEP, SBSTEP, KIMG" "LCFACT - Defines scale factors for load case operations.
LCFACT, LCNO, FACT" "LCFILE - Creates a load case from an existing load case file.
LCFILE, LCNO, Fname, Ext, --" "LCLEAR - Deletes nodes and line elements associated with selected lines.
LCLEAR, NL1, NL2, NINC" "LCOMB - Combines adjacent lines into one line.
LCOMB, NL1, NL2, KEEP" "LCOPER - Performs load case operations.
LCOPER, Oper, LCASE1, Oper2, LCASE2" "LCSEL - Selects a subset of load cases.
LCSEL, Type, LCMIN, LCMAX, LCINC" "LCSL - Divides intersecting lines at their point(s) of intersection.
LCSL, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LCSUM - Specifies whether to process non-summable items in load case operations.
LCSUM, Lab" "LCWRITE - Creates a load case by writing results to a load case file.
LCWRITE, LCNO, Fname, Ext, --" "LCZERO
LCZERO - Zeroes the results portion of the database." "LDELE - Deletes unmeshed lines.
LDELE, NL1, NL2, NINC, KSWP" "LDIV - Divides a single line into two or more lines.
LDIV, NL1, RATIO, PDIV, NDIV, KEEP" "LDRAG - Generates lines by sweeping a keypoint pattern along path.
LDRAG, NK1, NK2, NK3, NK4, NK5, NK6, NL1, NL2, NL3, NL4, NL5, NL6" "LDREAD - Reads results from the results file and applies them as loads.
LDREAD, Lab, LSTEP, SBSTEP, TIME, KIMG, Fname, Ext, --" "LESIZE - Specifies the divisions and spacing ratio on unmeshed lines.
LESIZE, NL1, SIZE, ANGSIZ, NDIV, SPACE, KFORC, LAYER1, LAYER2, KYNDIV" "LEXTND - Extends a line at one end by using its slope.
LEXTND, NL1, NK1, DIST, KEEP" "LFILLT - Generates a fillet line between two intersecting lines.
LFILLT, NL1, NL2, RAD, PCENT" "LFSURF - Generates surface elements overlaid on the edge of existing solid elements and assigns the extra node as the closest fluid element node.
LFSURF, SLINE, TLINE" "LGEN - Generates additional lines from a pattern of lines.
LGEN, ITIME, NL1, NL2, NINC, DX, DY, DZ, KINC, NOELEM, IMOVE" "LGLUE - Generates new lines by \"gluing\" lines.
LGLUE, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LGWRITE - Writes the database command log to a file.
LGWRITE, Fname, Ext, --, Kedit" "LINA - Finds the intersection of a line with an area.
LINA, NL, NA" "LINE
LINE - Specifies \"Lines\" as the subsequent status topic." "LINES - Specifies the length of a printed page.
LINES, N" "LINL - Finds the common intersection of lines.
LINL, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LINP - Finds the pairwise intersection of lines.
LINP, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LINV - Finds the intersection of a line with a volume.
LINV, NL, NV" "LIST - Lists out the sets in the results file.
LIST, LEVEL" "LLIST - Lists the defined lines.
LLIST, NL1, NL2, NINC, Lab" "LMATRIX - Calculates an inductance matrix and the total flux linkage for an N-winding coil system.
LMATRIX, SYMFAC, Coilname, Curname, Indname" "LMESH - Generates nodes and line elements along lines.
LMESH, NL1, NL2, NINC" "LNCOLLAPSE - Collapse a line segment to a keypoint (for models imported from CAD files).
LNCOLLAPSE, LINE, KEYPOINT" "LNDETACH - Detaches lines from neighboring geometric entity (for models imported from CAD files).
LNDETACH, LINE1, LINE2, LNINC" "LNFILL - Creates a straight line between two keypoints (for models imported from CAD files).
LNFILL, KP1, KP2" "LNMERGE - Merges two or more connected line segments (for models imported from CAD files).
LNMERGE, LN1, LN2, LN3, LN4, LN4, LN6, LN7, LN8, LN9, LN10" "LNSPLIT - Splits a line segment into two line segments (for models imported from CAD files).
LNSPLIT, LINE, PARAM" "LNSRCH - Activates a line search to be used with Newton-Raphson.
LNSRCH, Key" "LOCAL - Defines a local coordinate system by a location and orientation.
LOCAL, KCN, KCS, XC, YC, ZC, THXY, THYZ, THZX, PAR1, PAR2" "LOVLAP - Overlaps lines.
LOVLAP, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LPLOT - Displays the selected lines.
LPLOT, NL1, NL2, NINC" "LPTN - Partitions lines.
LPTN, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LREFINE - Refines the mesh around specified lines.
LREFINE, NL1, NL2, NINC, LEVEL, DEPTH, POST, RETAIN" "LREVERSE - Reverses the normal of a line, regardless of its connectivity or mesh status.
LREVERSE, LNUM, NOEFLIP" "LROTAT - Generates circular lines by rotating a keypoint pattern about an axis.
LROTAT, NK1, NK2, NK3, NK4, NK5, NK6, PAX1, PAX2, ARC, NSEG" "LSBA - Subtracts areas from lines.
LSBA, NL, NA, SEPO, KEEPL, KEEPA" "LSBL - Subtracts lines from lines.
LSBL, NL1, NL2, SEPO, KEEP1, KEEP2" "LSBV - Subtracts volumes from lines.
LSBV, NL, NV, SEPO, KEEPL, KEEPV" "LSBW - Subtracts the intersection of the working plane from lines (divides lines).
LSBW, NL, SEPO, KEEP" "LSCLEAR - Clears loads and load step options from the database.
LSCLEAR, Lab" "LSDELE - Deletes load step files.
LSDELE, LSMIN, LSMAX, LSINC" "LSEL - Selects a subset of lines.
LSEL, Type, Item, Comp, VMIN, VMAX, VINC, KSWP" "LSLA - Selects those lines contained in the selected areas.
LSLA, Type" "LSLK - Selects those lines containing the selected keypoints.
LSLK, Type, LSKEY" "LSOPER
LSOPER - Specifies \"Load step operations\" as the subsequent status topic." "LSREAD - Reads load and load step option data into the database.
LSREAD, LSNUM" "LSSCALE - Generates a scaled set of lines from a pattern of lines.
LSSCALE, NL1, NL2, NINC, RX, RY, RZ, KINC, NOELEM, IMOVE" "LSSOLVE - Reads and solves multiple load steps.
LSSOLVE, LSMIN, LSMAX, LSINC" "LSTR - Defines a straight line irrespective of the active coordinate system.
LSTR, P1, P2" "LSUM
LSUM - Calculates and prints geometry statistics of the selected lines." "LSWRITE - Writes load and load step option data to a file.
LSWRITE, LSNUM" "LSYMM - Generates lines from a line pattern by symmetry reflection.
LSYMM, Ncomp, NL1, NL2, NINC, KINC, NOELEM, IMOVE" "LTAN - Generates a line at the end of, and tangent to, an existing line.
LTAN, NL1, P3, XV3, YV3, ZV3" "LTRAN - Transfers a pattern of lines to another coordinate system.
LTRAN, KCNTO, NL1, NL2, NINC, KINC, NOELEM, IMOVE" "LUMPM - Specifies a lumped mass matrix formulation.
LUMPM, Key" "LVSCALE - Scales the load vector for mode superposition analyses.
LVSCALE, FACT" "LWPLAN - Defines the working plane normal to a location on a line.
LWPLAN, WN, NL1, RATIO" "M - Defines master degrees of freedom for reduced and superelement generation analyses.
M, NODE, Lab1, NEND, NINC, Lab2, Lab3, Lab4, Lab5, Lab6" "MADAPT - Adaptively meshes and solves an edge-based model.
MADAPT, ERRTARGT, NADAPT, NMAX, KPLT, Ksmooth, KLST, KCD, DEVICE" "MAGOPT - Specifies options for a 3-D magnetostatic field analysis.
MAGOPT, Value, Method" "MAGSOLV - Specifies magnetic solution options and initiates the solution.
MAGSOLV, OPT, NRAMP, CNVCSG, CNVFLUX, NEQIT, BIOT,CNVTOL" "MAPSOLVE - Maps solved node and element solutions from an original mesh to a new mesh.
MAPSOLVE, MAXSBSTEP" "MASTER
MASTER - Specifies \"Master DOF\" as the subsequent status topic." "MAT - Sets the element material attribute pointer.
MAT, MAT" "MATER
MATER - Specifies \"Material properties\" as the subsequent status topic." "MCHECK - Checks mesh connectivity.
MCHECK, Lab" "MDAMP - Defines the damping ratios as a function of mode.
MDAMP, STLOC, V1, V2, V3, V4, V5, V6" "MDELE - Deletes master degrees of freedom.
MDELE, NODE, Lab1, NEND, NINC, Lab2, Lab3, Lab4, Lab5, Lab6" "MDPLOT - Plots frequency-dependent modal damping coefficients calculated by DMPEXT.
MDPLOT, Function, Dmpname, Scale" "MEMM - Allows the current session to keep allocated memory
MEMM, Lab, Kywrd" "MESHING
MESHING - Specifies \"Meshing\" as the subsequent status topic." "MFANALYSIS - Turns an ANSYS Multi-field solver analysis on or off.
MFANALYSIS, Key" "MFBUCKET - Turns a bucket search on or off.
MFBUCKET, Key, Value" "MFCALC - Specifies a calculation frequency for a field in an ANSYS Multi-field solver analysis.
MFCALC, FNUMB, FREQ" "MFCI - Sets the control parameters used by the conservative (CPP) interpolation scheme.
MFCI, VAL1, VAL2" "MFCLEAR - Deletes ANSYS Multi-field solver analysis settings.
MFCLEAR, Option, Value" "MFCMMAND - Captures field solution options in a command file.
MFCMMAND, FNUMB, Fname, Ext" "MFCONV - Sets convergence values for an ANSYS Multi-field solver analysis.
MFCONV, Lab, VALUE" "MFDTIME - Sets time step sizes for an ANSYS Multi-field solver analysis.
MFDTIME, DTIME, DTMIN, DTMAX, Carry" "MFELEM - Defines a field by grouping element types.
MFELEM, FNUMB, ITYPE1, ITYPE2, ITYPE3, ITYPE4, ITYPE5, ITYPE6, ITYPE7, ITYPE8, ITYPE9, ITYPE10" "MFEM - Add more element types to a previously defined field number.
MFEM, FNUMB, ITYPE1, ITYPE2, ITYPE3, ITYPE4, ITYPE5, ITYPE6, ITYPE7, ITYPE8, ITYPE9, ITYPE10" "MFEXTER - Defines external fields for an ANSYS Multi-field solver analysis.
MFEXTER, FNUMB1, FNUMB2, FNUMB3, FNUMB4, FNUMB5, FNUMB6, FNUMB7, FNUMB8, FNUMB9, FNUMB10, FNUMB11, FNUMB12, FNUMB13, FNUMB14, FNUMB15, FNUMB16, FNUMB17, FNUMB18, FNUMB19, FNUMB20" "MFFNAME - Specifies a file name for a field in an ANSYS Multi-field solver analysis.
MFFNAME, FNUMB, Fname" "MFFR - Setup Multi-Field relaxation factors for field solutions.
MFFR, Fname, Lab, RFINI, RFMIN, RFMAX" "MFIMPORT - Imports a new field into a current ANSYS Multi-field solver analysis.
MFIMPORT, FNUMB, Option, Fname, Ext" "MFINTER - Specifies the interface load transfer interpolation option for an ANSYS Multi-field solver analysis.
MFINTER, Option" "MFITER - Sets the number of stagger iterations for an ANSYS Multi-field solver analysis.
MFITER, MAXITER, MINITER, TARGET" "MFLCOMM - Defines a load transfer for code coupling analyses.
MFLCOMM, Type, Fname1, Intname1, Label1, Fname2, Intname2, Label2, Option" "MFLIST - Lists the settings for an ANSYS Multi-field solver analysis.
MFLIST, Option, Value" "MFMAP - Calculates, saves, resumes, or deletes mapping data in an ANSYS Multi-field solver analysis.
MFMAP, Lab1, Lab2, Filename, Opt" "MFORDER - Specifies field solution order for an ANSYS Multi-field solver analysis.
MFORDER, FNUMB1, FNUMB2, FNUMB3, FNUMB4, FNUMB5, FNUMB6, FNUMB7, FNUMB8, FNUMB9, FNUMB10, FNUMB11, FNUMB12, FNUMB13, FNUMB14, FNUMB15, FNUMB16, FNUMB17, FNUMB18, FNUMB19, FNUMB20" "MFOUTPUT - Specifies results file output frequency for an ANSYS Multi-field solver analysis.
MFOUTPUT, FREQ" "MFPSIMUL - Sets up a field solver group to simultaneously process with code coupling analyses.
MFPSIMUL, gname, Fname1, Fname2" "MFRELAX - Sets relaxation values for an ANSYS Multi-field solver analysis.
MFRELAX, Lab, VALUE, Option" "MFRSTART - Specifies restart status for an ANSYS Multi-field solver analysis.
MFRSTART, TIME, Type" "MFSORDER - Sets up the solution sequence of simultaneous field solver groups for code coupling analyses.
MFSORDER, gname1, gname2" "MFSURFACE - Defines a surface load transfer for an ANSYS Multi-field solver analysis.
MFSURFACE, INUMB, FNUMB1, Label, FNUMB2" "MFTIME - Sets end time for an ANSYS Multi-field solver analysis.
MFTIME, TIME" "MFTOL - Turns normal distance checking on for surface mapping in an ANSYS Multi-field solver analysis.
MFTOL, Key, Value, Toler" "MFVOLUME - Defines a volume load transfer for an ANSYS Multi-field solver analysis.
MFVOLUME, INUMB, FNUMB1, Label, FNUMB2" "MFWRITE - Writes an ANSYS master input file for MFX multiple code coupling.
MFWRITE, Fname, Ext" "MGEN - Generates additional MDOF from a previously defined set.
MGEN, ITIME, INC, NODE1, NODE2, NINC" "MIDTOL - Sets midstep residual criterion values for structural transient analyses.
MIDTOL, KEY, TOLERB, RESFQ" "MITER - Defines a mitered bend in a piping run.
MITER, NEL1, NEL2, RAD, NDIV, ESTRT, EINC" "MLIST - Lists the MDOF of freedom.
MLIST, NODE1, NODE2, NINC" "MMF
MMF - Calculates the magnetomotive force along a path." "MODE - Specifies the harmonic loading term for this load step.
MODE, MODE, ISYM" "MODIFY - Changes the listed values of the data in a set.
MODIFY, SET, LSTEP, ITER, CUMIT, TIME, Ktitle" "MODMSH - Controls the relationship of the solid model and the FE model.
MODMSH, Lab" "MODOPT - Specifies modal analysis options.
MODOPT, Method, NMODE, FREQB, FREQE, Cpxmod/PRMODE, Nrmkey" "MONITOR - Controls contents of three variable fields in nonlinear solution monitor file.
MONITOR, VAR, Node, Lab" "MOPT - Specifies meshing options.
MOPT, Lab, Value" "MORPH - Specifies morphing and remeshing controls.
MORPH, Option, --, Remeshopt, ElemSet, ARMAX, VOCH, ARCH, STEP, TIME" "MOVE - Calculates and moves a node to an intersection.
MOVE, NODE, KC1, X1, Y1, Z1, KC2, X2, Y2, Z2" "MP - Defines a linear material property as a constant or a function of temperature.
MP, Lab, MAT, C0, C1, C2, C3, C4" "MPAMOD - Modifies temperature-dependent secant coefficients of thermal expansion.
MPAMOD, MAT, DEFTEMP" "MPCHG - Changes the material number attribute of an element.
MPCHG, MAT, ELEM" "MPCOPY - Copies linear material model data from one material reference number to another.
MPCOPY, --, MATF, MATT" "MPDATA - Defines property data to be associated with the temperature table.
MPDATA, Lab, MAT, STLOC, C1, C2, C3, C4, C5, C6" "MPDELE - Deletes linear material properties.
MPDELE, Lab, MAT1, MAT2, INC,--, LCHK" "MPDRES - Reassembles existing material data with the temperature table.
MPDRES, LabF, MATF, LabT, MATT" "MPLIST - Lists linear material properties.
MPLIST, MAT1, MAT2, INC, Lab, TEVL" "MPPLOT - Plots linear material properties as a function of temperature.
MPPLOT, Lab, MAT, TMIN, TMAX, PMIN, PMAX" "MPREAD - Reads a file containing material properties.
MPREAD, Fname, Ext, --, LIB" "MPRINT - Specifies that radiation matrices are to be printed.
MPRINT, KEY" "MPTEMP - Defines a temperature table for material properties.
MPTEMP, STLOC, T1, T2, T3, T4, T5, T6" "MPTGEN - Adds temperatures to the temperature table by generation.
MPTGEN, STLOC, NUM, TSTRT, TINC" "MPTRES - Restores a temperature table previously defined.
MPTRES, Lab, MAT" "MPWRITE - Writes linear material properties in the database to a file (if the LIB option is not specified) or writes both linear and nonlinear material properties (if LIB is specified) from the database to a file.
MPWRITE, Fname, Ext, --, LIB, MAT" "MSADV - Specifies the approach to discretize the advection term in a species transport equation.
MSADV, SPNUM, MTHA" "MSAVE - Sets the solver memory saving option. This option only applies to the PCG solver.
MSAVE, Key" "MSCAP - Activates and controls mass fraction capping for a species.
MSCAP, SPNUM, Capkey, UPPER, LOWER" "MSDATA - Defines multiple species data applicable to all species.
MSDATA, ALGEB, UGAS" "MSHAPE - For elements that support multiple shapes, specifies the element shape to be used for meshing.
MSHAPE, KEY, Dimension" "MSHCOPY - Simplifies the generation of meshes that have matching node element patterns on two different line groups (in 2-D) or area groups (3-D).
MSHCOPY, KEYLA, LAPTRN, LACOPY, KCN, DX, DY, DZ, TOL, LOW, HIGH" "MSHKEY - Specifies whether free meshing or mapped meshing should be used to mesh a model.
MSHKEY, KEY" "MSHMID - Specifies placement of midside nodes.
MSHMID, KEY" "MSHPATTERN - Specifies pattern to be used for mapped triangle meshing.
MSHPATTERN, KEY" "MSMASS - Specifies the mass type for a transient species analysis.
MSMASS, SPNUM, Value" "MSMETH - Specifies the method of solution of the species transport equations.
MSMETH, SPNUM, KEY" "MSMIR - Sets modified inertial relaxation factors for multiple species.
MSMIR, SPNUM, Value" "MSNOMF - Specifies the initial value of nominal mass fraction for a species.
MSNOMF, SPNUM, FRACTION" "MSPROP - Defines the fluid properties of a species.
MSPROP, SPNUM, Label, Type, NOMINAL, COF1, COF2, COF3" "MSQUAD - Specifies the quadrature order for multiple species elements.
MSQUAD, QDIF, QSRC" "MSRELAX - Specifies relaxation factors for a multiple species transport analysis.
MSRELAX, SPNUM, CONC, MDIF, EMDI, STAB" "MSSOLU - Specifies solution options for multiple species transport.
MSSOLU, SPNUM, NSWEEP, MAXI, NSRCH, CONV, DELMAX" "MSSPEC - Specifies the name, molecular weight, and Schmidt number of a species.
MSSPEC, SPNUM, Name, MOLWT, SCHMIDT" "MSTERM - Sets the convergence monitors for species.
MSTERM, SPNUM, STER, TTER" "MSVARY - Allows species properties to vary between global iterations.
MSVARY, SPNUM, Lab, Key" "MXPAND - Specifies the number of modes to expand and write for a modal or buckling analysis.
MXPAND, NMODE, FREQB, FREQE, Elcalc, SIGNIF" "N - Defines a node.
N, NODE, X, Y, Z, THXY, THYZ, THZX" "NANG - Rotates a nodal coordinate system by direction cosines.
NANG, NODE, X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3" "NCNV - Sets the key to terminate an analysis.
NCNV, KSTOP, DLIM, ITLIM, ETLIM, CPLIM" "NDELE - Deletes nodes.
NDELE, NODE1, NODE2, NINC" "NDIST - Calculates and lists the distance between two nodes.
NDIST, ND1, ND2" "NDSURF - Generates surface elements overlaid on the edge of existing elements and assigns the extra node as the closest fluid element node.
NDSURF, Snode, Telem, DIMN" "NEQIT - Specifies the maximum number of equilibrium iterations for nonlinear analyses.
NEQIT, NEQIT" "NFORCE - Sums the nodal forces and moments of elements attached to nodes.
NFORCE, ITEM" "NGEN - Generates additional nodes from a pattern of nodes.
NGEN, ITIME, INC, NODE1, NODE2, NINC, DX, DY, DZ, SPACE" "NKPT - Defines a node at an existing keypoint location.
NKPT, NODE, NPT" "NLDIAG - Sets nonlinear diagnostics functionality.
NLDIAG, Label, Key" "NLDPOST - Gets element component information from nonlinear diagnostic files.
NLDPOST, Label, Key, FileID, Prefix" "NLGEOM - Includes large-deflection effects in a static or full transient analysis.
NLGEOM, Key" "NLHIST - Specify result items to track during solution.
NLHIST, Key, Name, Item, Comp, NODE, ELEM, SHELL, LAYER" "NLIST - Lists nodes.
NLIST, NODE1, NODE2, NINC, Lcoord, SORT1, SORT2, SORT3" "NLOG - Forms the natural log of a variable.
NLOG, IR, IA, --, --, Name, --, --, FACTA, FACTB" "NLOPT
NLOPT - Specifies \"Nonlinear analysis options\" as the subsequent status topic." "NMODIF - Modifies an existing node.
NMODIF, NODE, X, Y, Z, THXY, THYZ, THZX" "NOCOLOR - Removes color from graphics displays.
NOCOLOR, KEY" "NODES
NODES - Specifies \"Nodes\" as the subsequent status topic." "NOOFFSET - Prevents the CDREAD command from offsetting specified data items
NOOFFSET, Label" "NOORDER - Re-establishes the original element ordering.
NOORDER, Lab" "NORA - Rotates nodal coordinate systems to surface normal
NORA, AREA, NDIR" "NORL - Rotates nodal coordinate systems perpendicular to line normal
NORL, LINE, AREA, NDIR" "NPLOT - Displays nodes.
NPLOT, KNUM" "NPRINT - Defines which time points stored are to be listed.
NPRINT, N" "NREAD - Reads nodes from a file.
NREAD, Fname, Ext, --" "NREFINE - Refines the mesh around specified nodes.
NREFINE, NN1, NN2, NINC, LEVEL, DEPTH, POST, RETAIN" "NRLSUM - Specifies the Naval Research Laboratory (NRL) sum mode combination method.
NRLSUM, SIGNIF, Label" "NROPT - Specifies the Newton-Raphson options in a static or full transient analysis.
NROPT, Option, --, Adptky" "NROTAT - Rotates nodal coordinate systems into the active system.
NROTAT, NODE1, NODE2, NINC" "NRRANG - Specifies the range of nodes to be read from the node file.
NRRANG, NMIN, NMAX, NINC" "NSCALE - Generates a scaled set of nodes from a pattern of nodes.
NSCALE, INC, NODE1, NODE2, NINC, RX, RY, RZ" "NSEL - Selects a subset of nodes.
NSEL, Type, Item, Comp, VMIN, VMAX, VINC, KABS" "NSLA - Selects those nodes associated with the selected areas.
NSLA, Type, NKEY" "NSLE - Selects those nodes attached to the selected elements.
NSLE, Type, NodeType, Num" "NSLK - Selects those nodes associated with the selected keypoints.
NSLK, Type" "NSLL - Selects those nodes associated with the selected lines.
NSLL, Type, NKEY" "NSLV - Selects those nodes associated with the selected volumes.
NSLV, Type, NKEY" "NSMOOTH - Smooths selected nodes among selected elements.
NSMOOTH, NPASS" "NSOL - Specifies nodal data to be stored from the results file.
NSOL, NVAR, NODE, Item, Comp, Name" "NSORT - Sorts nodal data.
NSORT, Item, Comp, ORDER, KABS, NUMB, SEL" "NSTORE - Defines which time points are to be stored.
NSTORE, TINC" "NSUBST - Specifies the number of substeps to be taken this load step.
NSUBST, NSBSTP, NSBMX, NSBMN, Carry" "NSVR - Defines the number of variables for user-programmable element options.
NSVR, ITYPE, NSTV" "NSYM - Generates a reflected set of nodes.
NSYM, Ncomp, INC, NODE1, NODE2, NINC" "NUMCMP - Compresses the numbering of defined items.
NUMCMP, Label" "NUMEXP - Specifies solutions to be expanded from reduced analyses.
NUMEXP, NUM, BEGRNG, ENDRNG, Elcalc" "NUMMRG - Merges coincident or equivalently defined items.
NUMMRG, Label, TOLER, GTOLER, Action, Switch" "NUMOFF - Adds a number offset to defined items.
NUMOFF, Label, VALUE" "NUMSTR - Establishes starting numbers for automatically numbered items.
NUMSTR, Label, VALUE" "NUMVAR - Specifies the number of variables allowed in POST26.
NUMVAR, NV" "NUSORT
NUSORT - Restores original order for nodal data." "NWPAVE - Moves the working plane origin to the average location of nodes.
NWPAVE, N1, N2, N3, N4, N5, N6, N7, N8, N9" "NWPLAN - Defines the working plane using three nodes.
NWPLAN, WN, NORIG, NXAX, NPLAN" "NWRITE - Writes nodes to a file.
NWRITE, Fname, Ext, --, KAPPND" "OMEGA - Specifies the rotational velocity of the structure.
OMEGA, OMEGX, OMEGY, OMEGZ, KSPIN" "OPADD - Forms a set of optimization parameters by adding two sets.
OPADD, NRES, NUM1, NUM2, C1, C2" "OPANL - Defines the analysis file to be used for optimization looping.
OPANL, Fname, Ext, --" "OPCLR
OPCLR - Clears the optimization database." "OPDATA - Identifies the file where optimization data is to be saved.
OPDATA, Fname, Ext, --" "OPDEL - Deletes optimization design sets.
OPDEL, NSET1, NSET2" "OPEQN - Controls curve fitting for the subproblem approximation method.
OPEQN, KFOBJ, KFSV, KWGHT, KOPPR, INOPT" "OPERATE
OPERATE - Specifies \"Operation data\" as the subsequent status topic." "OPEXE
OPEXE - Initiates optimization looping." "OPFACT - Defines the type of factorial evaluation to be performed.
OPFACT, Type" "OPFRST - Defines specifications for the first order optimization method.
OPFRST, NITR, SIZE, DELTA" "OPGRAD - Specifies which design set will be used for gradient evaluation.
OPGRAD, Dset, DELTA" "OPKEEP - Specifies whether to save the best-set results and database file.
OPKEEP, Key" "OPLFA - Displays the results of a factorial evaluation.
OPLFA, Name, Effect, EMIN, EMAX" "OPLGR - Graphs the results of a gradient evaluation.
OPLGR, Pname, Dvnam1, Dvnam2, Dvnam3, Dvnam4, Dvnam5, Dvnam6" "OPLIST - Displays the parameters for design sets.
OPLIST, SET1, SET2, LKEY" "OPLOOP - Specifies controls for optimization looping.
OPLOOP, Read, Dvar, Parms" "OPLSW - Graphs the results of a global sweep generation.
OPLSW, Pname, Dvnam1, Dvnam2, Dvnam3, Dvnam4, Dvnam5, Dvnam6" "OPMAKE
OPMAKE - Creates a design set using active scalar parameter values." "OPNCONTROL - Sets decision parameter for automatically increasing the time step interval.
OPNCONTROL, Lab, VALUE, NUMSTEP" "OPPRNT - Activates detailed optimization summary printout.
OPPRNT, Key" "OPRAND - Defines the number of iterations for a random optimization.
OPRAND, NITR, NFEAS" "OPRESU - Reads optimization data into the optimization database.
OPRESU, Fname, Ext, --" "OPRFA - Prints the results of a factorial evaluation.
OPRFA, Name" "OPRGR - Prints the results of a gradient evaluation.
OPRGR, Name" "OPRSW - Prints the results of a global sweep generation.
OPRSW, Name" "OPSAVE - Writes all optimization data to a file.
OPSAVE, Fname, Ext, --" "OPSEL - Selects design sets for subsequent optimization looping.
OPSEL, NSEL" "OPSUBP - Defines number of iterations for subproblem approximation method.
OPSUBP, NITR, NINFS" "OPSWEEP - Specifies the reference point and number of evaluation points for a sweep generation.
OPSWEEP, Dset, NSPS" "OPTYPE - Specifies the optimization method to be used.
OPTYPE, Mname" "OPUSER - Defines specifications for user-supplied external optimization.
OPUSER, NITR, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8" "OPVAR - Specifies the parameters to be treated as optimization variables.
OPVAR, Name, Type, MIN, MAX, TOLER" "OUTOPT
OUTOPT - Specifies \"Output options\" as the subsequent status topic." "OUTPR - Controls the solution printout.
OUTPR, Item, FREQ, Cname" "OUTRES - Controls the solution data written to the database.
OUTRES, Item, Freq, Cname" "PADELE - Deletes a defined path.
PADELE, DELOPT" "PAGET - Writes current path information into an array variable.
PAGET, PARRAY, POPT" "PAPUT - Retrieves path information from an array variable.
PAPUT, PARRAY, POPT" "PARESU - Restores previously saved paths from a file.
PARESU, Lab, Fname, Ext, --" "PARRES
PARRES, Lab, Fname, Ext, --" "PARSAV
PARSAV, Lab, Fname, Ext, --" "PARTSEL - Selects a subset of parts in an explicit dynamic analysis.
PARTSEL, Type, PMIN, PMAX, PINC" "PASAVE - Saves selected paths to an external file.
PASAVE, Lab, Fname, Ext, --" "PATH - Defines a path name and establishes parameters for the path.
PATH, NAME, nPts, nSets, nDiv" "PCALC - Forms additional labeled path items by operating on existing path items.
PCALC, Oper, LabR, Lab1, Lab2, FACT1, FACT2, CONST" "PCGOPT - Controls PCG solver options.
PCGOPT, Lev_Diff , --, ReduceIO, StrmCk, Wrtfull, Memory" "PCIRC - Creates a circular area centered about the working plane origin.
PCIRC, RAD1, RAD2, THETA1, THETA2" "PCONV - Sets convergence values for p-method solutions.
PCONV, TOLER, Item, Comp, NODE, Surf" "PCORRO - Specifies the allowable exterior corrosion thickness for a piping run.
PCORRO, CTK" "PCROSS - Calculates the cross product of two path vectors along the current path.
PCROSS, LabXR, LabYR, LabZR, LabX1, LabY1, LabZ1, LabX2, LabY2, LabZ2" "PDANL - Defines the analysis file to be used for probabilistic looping.
PDANL, Fname, Ext, --" "PDCDF - Plots the cumulative distribution function.
PDCDF, Rlab, Name, Type, CONF, NMAX" "PDCFLD - Calculates a correlation field and stores it into an ANSYS array.
PDCFLD, ParR, Entity, Ctype, CLENGTH" "PDCLR - Clears the probabilistic design database.
PDCLR, Type" "PDCMAT - Prints the correlation coefficient matrix.
PDCMAT, Rlab, Matrix, Name1, Name2, Corr, SLEVEL, Popt" "PDCORR - Specifies the correlation between two random input variables.
PDCORR, Name1, Name2, CORR" "PDDMCS - Specifies options for Monte Carlo Simulations using direct sampling.
PDDMCS, NSIM, --, Astop, ACCMEAN, ACCSTDEV, CHECK, Seed" "PDDOEL - Defines design of experiment levels for an individual random input variable.
PDDOEL, Name, Method, Vtype, Lopt, VAL1, VAL2, VAL3, VAL4, VAL5" "PDEF - Interpolates an item onto a path.
PDEF, Lab, Item, Comp, Avglab" "PDEXE - Executes the probabilistic analysis.
PDEXE, Slab, MRUN, NFAIL, FOPT, Fname" "PDHIST - Plots the frequency histogram.
PDHIST, Rlab, Name, NCL, Type" "PDINQR - Evaluates statistical characteristics of a random input variable.
PDINQR, Rpar, Name, Type, VAL" "PDLHS - Specifies options for Monte Carlo Simulations using Latin-Hypercube sampling.
PDLHS, NSIM, NREP, ISopt, --, Astop, ACCMEAN, ACCSTDV, CHECK, Seed" "PDMETH - Specifies the probabilistic analysis method.
PDMETH, Method, Samp" "PDOT - Calculates the dot product of two path vectors along the current path.
PDOT, LabR, LabX1, LabY1, LabZ1, LabX2, LabY2, LabZ2" "PDPINV - Prints the result of the inversion of a probability.
PDPINV, Rlab, Name, PROB, --, CONF" "PDPLOT - Plots the distribution curves of a defined random input variable.
PDPLOT, Name, PLOW, PUP" "PDPROB - Prints a probability result.
PDPROB, Rlab, Name, Relation, LIMIT, --, CONF" "PDRAG - Defines the external fluid drag loading for a piping run.
PDRAG, PX1, PY1, PZ1, H1, PX2, PY2, PZ2, H2, Kcord" "PDRESU - Reads the probabilistic model data and loads it into the database.
PDRESU, Fname, Ext, --" "PDROPT - Specifies the options for an HTML report.
PDROPT, RVAR, CORR, STAT, SHIS, HIST, CDF, SENS, CMAT, CONF" "PDSAVE - Writes the probabilistic model data to a file.
PDSAVE, Fname, Ext, --" "PDSCAT - Plots a scatter graph.
PDSCAT, Rlab, Name1, Name2, Type, ORDER, NMAX" "PDSENS - Plots the probabilistic sensitivities.
PDSENS, Rlab, Name, Chart, Type, SLEVEL" "PDSHIS - Plots the sample history values.
PDSHIS, Rlab, Name, Type, CONF" "PDUSER - Specifies options for user-specified sampling methods.
PDUSER, Fname, Ext, --" "PDVAR - Specifies the parameters to be treated as probabilistic design variables.
PDVAR, Name, Type, PAR1, PAR2, PAR3, PAR4" "PDWRITE - Generates an HTML report for the probabilistic analyses.
PDWRITE, File, Fnam, Lnam" "PEMOPTS - Defines percentage tolerance and error estimation method for electrostatic p-Method solution.
PEMOPTS, TOLER, Method" "PERBC2D - Generates periodic constraints for 2-D planar magnetic field analyses.
PERBC2D, LOC1, LOC2, LOCTOL, R1, R2, TOLR, OPT, PLNOPT" "PERI - Specifies periodic boundary conditions in an incompressible flow analysis.
PERI, DX, DY, DZ" "PEXCLUDE - Specifies elements to be excluded from p-level escalations.
PEXCLUDE, ELEM" "PFACT - Calculates participation factors for the PSD or multi-point response spectrum table.
PFACT, TBLNO, Excit, Parcor" "PFLUID - Defines the contained fluid density for a piping run.
PFLUID, DENS" "PGAP - Defines a spring-gap constraint in a piping run.
PGAP, NLOC, K, DX, DY, DZ, GAP, ELEM" "PGRAPH - Specifies the location from which graphics data will be retrieved for viewing.
PGRAPH, Option, Fname, Fext, --" "PGRSET - Defines the data set to be read from the PGR file.
PGRSET, Lstep, SBSTEP, --, KIMG, TIME, --, NSET" "PGSAVE - Creates a PowerGraphics (PGR) file from results data.
PGSAVE, Fname, Fext, --, DataType, InteriorKey, Append" "PGSELE - Select a subset of elements for display with the PGR viewer.
PGSELE, Type, Item, --, VMIN, VMAX, VINC" "PGWRITE - Writes selected solution data to the PGR file for faster post processing access.
PGWRITE, Label, Fname, Fext, --, DataType, InteriorKey, Append" "PHYSICS - Writes, reads, or lists all element information
PHYSICS, Option, Title, Fname, Ext, --" "PINCLUDE - Specifies elements to be included in p-level escalations.
PINCLUDE, ELEM" "PINSUL - Defines the external insulation constants in a piping run.
PINSUL, DENS, ITK" "PIPE
PIPE - Specifies \"Pipe modeling\" as the subsequent status topic." "PIVCHECK - Prevents a batch mode, linear static analysis from stopping when a negative or zero equation solver pivot value is encountered.
PIVCHECK, KEY, PRNTCNTRL" "PLCAMP - Plots Campbell diagram data for applications involving rotating structure dynamics.
PLCAMP, Option, SLOPE, UNIT, FREQB, Cname, STABVAL" "PLCINT - Plots the J-integral result data.
PLCINT, ACTION, ID, node, Cont" "PLCONV - Plots the convergence curve for specified items from a p-method solution.
PLCONV, Item, Comp, NODE, Surf" "PLCPLX - Specifies the part of a complex variable to display.
PLCPLX, KEY" "PLCRACK - Displays cracking and crushing locations in SOLID65 elements.
PLCRACK, LOC, NUM" "PLDISP - Displays the displaced structure.
PLDISP, KUND" "PLESOL - Displays the solution results as discontinuous element contours.
PLESOL, Item, Comp, KUND, Fact" "PLETAB - Displays element table items.
PLETAB, Itlab, Avglab" "PLF2D - Generates a contour line plot of equipotentials.
PLF2D, NCONT, OLAY, ANUM, WIN" "PLHFFAR - Displays electric far fields and far field parameters.
PLHFFAR, Opt, Lab, PHI1, PHI2, NPHI, THETA1, THETA2, NTHETA, RADZ" "PLLS - Displays element table items as contoured areas along elements.
PLLS, LabI, LabJ, Fact, KUND" "PLNSOL - Displays results as continuous contours.
PLNSOL, Item, Comp, KUND, Fact, FileID" "PLORB
PLORB - Displays the orbital motion of a rotating structure" "PLOT - Forms a display.
PLOT, NSTRT, NEND, NINC" "PLOTTING
PLOTTING - Specifies \"Plotting settings\" as the subsequent status topic." "PLPAGM - Displays path items along the path geometry.
PLPAGM, Item, Gscale, Nopt" "PLPATH - Displays path items on a graph.
PLPATH, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6" "PLSCH - Converts and plots scattering, admittance, or impedance parameters on a Smith chart.
PLSCH, Fname, Ext, Lab, Port" "PLSECT - Displays membrane and membrane-plus-bending linearized stresses.
PLSECT, Item, Comp, RHO, KBR" "PLSYZ - Converts and plots network parameters versus frequency or plots losses versus frequency.
PLSYZ, Fname, Ext, Lab, Opt, VAL_I1, VAL_J1, VAL_I2, VAL_J2, VAL_I3, VAL_J3, VAL_I4, VAL_J4" "PLTD - Displays TDR/TDT waveforms, an impedance profile, or a total waveform.
PLTD, Fname, Ext, Lab, Opt, Vkey, NFFT, TSTART, TRISE, PORTI1, PORTJ1, PORTI2, PORTJ2, PORTI3, PORTJ3, PORTI4, PORTJ4" "PLTIME - Defines the time range for which data are to be displayed.
PLTIME, TMIN, TMAX" "PLTRAC - Displays a particle flow or charged particle trace on an element display.
PLTRAC, Analopt, Item, Comp, TRPNum, Name, MXLOOP, TOLER, OPTION, ESCL, MSCL" "PLVAR - Displays up to ten variables in the form of a graph.
PLVAR, NVAR1, NVAR2, NVAR3, NVAR4, NVAR5, NVAR6, NVAR7, NVAR8, NVAR9, NVAR10" "PLVAROPT - Displays up to ten parameters in the form of a graph.
PLVAROPT, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6, Lab7, Lab8, Lab9, Lab10" "PLVECT - Displays results as vectors.
PLVECT, Item, Lab2, Lab3, LabP, Mode, Loc, Edge, KUND" "PLVFRC - Displays volume fractions in a volume of fluid (VOF) analysis.
PLVFRC, CONT" "PLWAVE - Specifies a free-space time-harmonic incident plane electromagnetic wave.
PLWAVE, Ex, Ey, Ez, AngX, AngZ" "PMAP - Creates mapping of the path geometry by defining path interpolation division points.
PMAP, FORM, DISCON" "PMETH
PMETH - Specifies \"p-Method\" as the subsequent status topic." "PMGTRAN - Summarizes electromagnetic results from a transient analysis.
PMGTRAN, Fname, FREQ, Fcnam1, Fcnam2, Pcnam1, Pcnam2, Ecnam1, Ccnam1" "PMLOPT - Defines perfectly matched layers (PMLs) for a high-frequency analysis.
PMLOPT, ESYS, Lab, Xminus, Xplus, Yminus, Yplus, Zminus, Zplus" "PMLSIZE - Determines number of PML layers.
PMLSIZE, FREQB, FREQE, DMIN, DMAX, THICK, ANGLE" "PMOPTS - Defines percentage tolerance for a p-Method solution.
PMOPTS, TOLER" "PNGR - Provides PNG file export for ANSYS displays.
PNGR, Kywrd, OPT, VAL" "POINT
POINT - Specifies \"Point flow tracing settings\" as the subsequent status topic." "POLY
POLY - Creates a polygonal area based on working plane coordinate pairs." "POPT - Selects the piping analysis standard for a piping run.
POPT, Lop1" "POUTRES - Controls the nodal DOF and computed element results graphics data that is written to the PGR file.
POUTRES, Item1, Item2, Item3,Â Item4,Â Item5,Â Item6,Â Item7,Â Item8,Â Item9,Â Item10,Â Item11,Â Item12, Item13,Â Item14,Â Item15,Â Item16,Â Item17,Â Item18,Â Item19" "POWERH
POWERH - Calculates the rms power loss in a conductor or lossy dielectric." "PPATH - Defines a path by picking or defining nodes, or locations on the currently active working plane, or by entering specific coordinate locations.
PPATH, POINT, NODE, X, Y, Z, CS" "PPLOT
PPLOT - Displays an element plot indicating each element's final p-level." "PPRANGE - Specifies a range of p-level values for use in a p-method solution.
PPRANGE, START, MAX" "PPRES - Defines the internal pressure for a piping run.
PPRES, PRESS" "PRANGE - Determines the path range.
PRANGE, LINC, VMIN, VMAX, XVAR" "PRCAMP - Prints Campbell diagram data for applications involving rotating structure dynamics.
PRCAMP, Option, SLOPE, UNIT, FREQB, Cname, STABVAL" "PRCINT - Lists the J-integral result data.
PRCINT, ID, node" "PRCONV
PRCONV - Lists convergence values versus characteristic p-level. " "PRCPLX - Defines the output form for complex variables.
PRCPLX, KEY" "PRECISION - Specifies machine precision for solvers (currently valid only for PCG solvers).
PRECISION, LABEL" "PRED - Activates a predictor in a nonlinear analysis.
PRED, Sskey, --, Lskey" "PRENERGY
PRENERGY - Prints the total energies of a model." "PRERR
PRERR - Prints SEPC and TEPC." "PRESOL - Prints the solution results for elements.
PRESOL, Item, Comp" "PRETAB - Prints the element table items.
PRETAB, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6, Lab7, Lab8, Lab9" "PRHFFAR - Prints electric far fields and far field parameters.
PRHFFAR, Opt, Lab, PHI1, PHI2, NPHI, THETA1, THETA2, NTHETA, RADZ" "PRI2 - Creates a polygonal area or a prism volume by vertices (GUI).
PRI2, P51X, Z1, Z2" "PRIM
PRIM - Specifies \"Solid model primitives\" as the subsequent status topic." "PRINT
PRINT - Specifies \"Print settings\" as the subsequent status topic." "PRISM - Creates a prism volume based on working plane coordinate pairs.
PRISM, Z1, Z2" "PRITER
PRITER - Prints solution summary data." "PRJSOL - Prints joint element output.
PRJSOL, Item, Comp" "PRNLD - Prints the summed element nodal loads.
PRNLD, Lab, TOL, Item" "PRNSOL - Prints the nodal solution results.
PRNSOL, Item, Comp" "PROD - Multiplies variables.
PROD, IR, IA, IB, IC, Name, --, --, FACTA, FACTB, FACTC" "PRORB
PRORB - Prints the orbital motion characteristics of a rotating structure" "PRPATH - Prints path items along a geometry path.
PRPATH, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6" "PRRFOR - Used with the FORCE command. Prints the constrained node reaction solution.
PRRFOR, Lab" "PRRSOL - Prints the constrained node reaction solution.
PRRSOL, Lab" "PRSECT - Calculates and prints linearized stresses along a section path.
PRSECT, RHO, KBR" "PRSSOL - Prints BEAM188 and BEAM189 section results.
PRSSOL, Item, Comp" "PRSYZ - Converts and lists scattering, admittance, or impedance parameters.
PRSYZ, Fname, Ext, Lab, Opt, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10, VAL11, VAL12, VAL13, VAL14, VAL15, VAL16" "PRTIME - Defines the time range for which data are to be listed.
PRTIME, TMIN, TMAX" "PRVAR - Lists variables vs. time (or frequency).
PRVAR, NVAR1, NVAR2, NVAR3, NVAR4, NVAR5, NVAR6" "PRVAROPT - Lists up to ten optimization parameters.
PRVAROPT, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6, Lab7, Lab8, Lab9, Lab10" "PRVECT - Prints results as vector magnitude and direction cosines.
PRVECT, Item, Lab2, Lab3, LabP" "PSCONTROL - Turn shared-memory parallel operations on or off during solution.
PSCONTROL, Option, Key" "PSCR - Specifies various PostScript options.
PSCR, Kywrd, KEY" "PSDCOM - Specifies the power spectral density mode combination method.
PSDCOM, SIGNIF, COMODE" "PSDFRQ - Defines the frequency points for the input spectrum vs. FREQ tables of PSD and multi-point spectrum analyses.
PSDFRQ, TBLNO1, TBLNO2, FREQ1, FREQ2, FREQ3, FREQ4, FREQ5, FREQ6, FREQ7" "PSDGRAPH - Displays input PSD curves
PSDGRAPH, TBLNO1, TBLNO2" "PSDRES - Controls solution output written to the results file from a PSD analysis.
PSDRES, Lab, RelKey" "PSDSPL - Defines a partially correlated excitation in a PSD analysis.
PSDSPL, TBLNO, RMIN, RMAX" "PSDUNIT - Defines the type of PSD or multi-point response spectrum.
PSDUNIT, TBLNO, Type, GVALUE" "PSDVAL - Defines PSD or multi-point response spectrum values.
PSDVAL, TBLNO, SV1, SV2, SV3, SV4, SV5, SV6, SV7" "PSDWAV - Defines a wave propagation excitation in a PSD analysis.
PSDWAV, TBLNO, VX, VY, VZ" "PSEL - Selects a path or paths.
PSEL, Type, Pname1, Pname2, Pname3, Pname4, Pname5, Pname6, Pname7, Pname8, Pname9, Pname10" "PSMESH - Create and mesh a pretension section
PSMESH, SECID, Name, P0, Egroup, NUM, KCN, KDIR, VALUE, NDPLANE, PSTOL, PSTYPE, ECOMP, NCOMP" "PSOLVE - Directs the program to perform a partial solution.
PSOLVE, Lab" "PSPEC - Defines pipe material and dimensions.
PSPEC, MAT, DNOM, SCHED, OD, TK" "PSPRNG - Defines a spring constraint in a piping run.
PSPRNG, NLOC, TYPE, K, DX, DY, DZ, ELEM" "PSTRES - Specifies whether prestress effects are calculated or included.
PSTRES, Key" "PTEMP - Defines the pipe wall temperatures in a piping run.
PTEMP, TOUT, TIN" "PTXY - Defines coordinate pairs for use in polygons and prisms.
PTXY, X1, Y1, X2, Y2, X3, Y3, X4, Y4" "PUNIT - Selects the system of length units to be used in a piping run.
PUNIT, KOPT" "PVECT - Interpolates a set of items onto a path.
PVECT, Oper, LabXR, LabYR, LabZR" "QDVAL - Defines PSD quadspectral values.
QDVAL, TBLNO1, TBLNO2, SV1, SV2, SV3, SV4, SV5, SV6, SV7" "QFACT
QFACT - Calculates the quality factor for high-frequency electromagnetic resonators." "QSOPT - Specifies quasi static radiation options.
QSOPT, Opt" "QUAD - Generates a quadratic line of nodes from three nodes.
QUAD, NODE1, NINTR, NODE2, NFILL, NSTRT, NINC, PKFAC" "QUOT - Divides two variables.
QUOT, IR, IA, IB, --, Name, --, --, FACTA, FACTB" "R - Defines the element real constants.
R, NSET, R1, R2, R3, R4, R5, R6" "RACE - Defines a \"racetrack\" current source.
RACE, XC, YC, RAD, TCUR, DY, DZ, --, --, Cname" "RADOPT - Specifies Gauss-Seidel Radiosity Solver options.
RADOPT, FLUXRELX, FLUXTOL, SOLVER, MAXITER, TOLER, OVERRLEX" "RALL
RALL - Calculates solver statistics and run time estimates. " "RAPPND - Appends results data from the database to the results file.
RAPPND, LSTEP, TIME" "RATE - Specifies whether the effect of creep strain rate will be used in the solution of a load step.
RATE, Option" "RBE3 - Distributes the force/moment applied at the master node to a set of slave nodes, taking into account the geometry of the slave nodes as well as weighting factors.
RBE3, Master, DOF, Slaves, Wtfact" "RCON
RCON - Specifies \"Real constants\" as the subsequent status topic. " "RDEC - Defines the decimation parameters.
RDEC, Option REDUC , --, Nplace" "RDELE - Deletes real constant sets.
RDELE, NSET1, NSET2, NINC,--, LCHK" "REAL - Sets the element real constant set attribute pointer.
REAL, NSET" "REALVAR - Forms a variable using only the real part of a complex variable.
REALVAR, IR, IA, --, --, Name, --, --, FACTA" "RECTNG - Creates a rectangular area anywhere on the working plane.
RECTNG, X1, X2, Y1, Y2" "REDUCE - Defines a reducer in a piping run.
REDUCE, NLOC, LENG, ELEM" "REFLCOEF - Calculates the voltage reflection coefficient (REFLC), standing wave ratio (VSWR), and return loss (RL) in a COAX fed device; at postprocessing of an HF electromagnetic analysis.
REFLCOEF, Portin, Pvolt, Pang, Pdist, Vpathy" "REMESH - Specifies the starting and ending remeshing points for rezoning.
REMESH, Action" "REORDER
REORDER - Specifies \"Model reordering\" as the subsequent status topic. " "RESCONTROL - Controls file writing for multiframe restarts.
RESCONTROL, Action, Ldstep, Frequency, MAXFILES" "RESET
RESET - Resets all POST1 or POST26 specifications to initial defaults. " "RESP - Generates a response spectrum.
RESP, IR, LFTAB, LDTAB, ITYPE, RATIO, DTIME, TMIN, TMAX" "RESUME - Resumes the database from the database file.
RESUME, Fname, Ext, --, NOPAR, KNOPLOT" "RESVEC - Calculates or includes residual vectors.
RESVEC, Key" "RESWRITE - Appends results data from the database to a results file.
RESWRITE, Fname" "REXPORT - Exports displacements from an implicit run to ANSYS LS-DYNA.
REXPORT, Target, --, --, LSTEP, SBSTEP, Fname, Ext, --" "REZONE - Initiates the rezoning process, sets rezoning options, and rebuilds the database.
REZONE, Option, LDSTEP, SBSTEP" "RFILSZ
RFILSZ - Estimates file sizes. " "RFORCE - Specifies the total reaction force data to be stored.
RFORCE, NVAR, NODE, Item, Comp, Name" "RIGID - Specifies known rigid body modes (if any) of the model.
RIGID, Dof1, Dof2, Dof3, Dof4, Dof5, Dof6" "RIMPORT - Imports initial stresses from an explicit dynamics run into ANSYS.
RIMPORT, Source, Type, Loc, LSTEP, SBSTEP, Fname, Ext, --, SPSCALE, MSCALE" "RITER - Supplies an estimate of the number of iterations for time estimates.
RITER, NITER" "RLIST - Lists the real constant sets.
RLIST, NSET1, NSET2, NINC" "RMALIST
RMALIST - Lists all defined master nodes for a ROM method." "RMANL - Assigns model database, dimensionality, and operating direction for the ROM method.
RMANL, Fname, Ext, --, Dimn, Oper" "RMASTER - Defines master nodes for the ROM method.
RMASTER, Node, Lab" "RMCAP - Defines lumped capacitance pairs between conductors C1 and C2 for a ROM method.
RMCAP, RefName, C1, C2" "RMCLIST
RMCLIST - Lists all lumped capacitance pairs defined." "RMEMRY
RMEMRY - Prints memory statistics for the current model. " "RMFLVEC
RMFLVEC - Writes eigenvectors of fluid nodes to a file for use in damping parameter extraction." "RMLVSCALE - Defines element load vector scaling for a ROM use pass.
RMLVSCALE, Nload, Fact1, Fact2, Fact3, Fact4, Fact5" "RMMLIST
RMMLIST - Lists all mode specifications for the ROM method." "RMMRANGE - Defines and edits various modal parameters for the ROM method.
RMMRANGE, Mode, Key, Min, Max, Nstep, Damp, Scale" "RMMSELECT - Selects modes for the ROM method.
RMMSELECT, Nmode, Method, Dmin, Dmax" "RMNDISP - Extracts neutral plane displacements from a test load or element load solution for the ROM method.
RMNDISP, LoadT, Loc" "RMNEVEC
RMNEVEC - Extracts neutral plane eigenvectors from a modal analysis for the ROM method." "RMODIF - Modifies real constant sets.
RMODIF, NSET, STLOC, V1, V2, V3, V4, V5, V6" "RMORE - Adds real constants to a set.
RMORE, R7, R8, R9, R10, R11, R12" "RMPORDER - Defines polynomial orders for ROM functions.
RMPORDER, Ord1, Ord2, Ord3, Ord4, Ord5, Ord6, Ord7, Ord8, Ord9" "RMRESUME - Resumes ROM data from a file.
RMRESUME, Fname, Ext, --" "RMRGENERATE
RMRGENERATE - Performs fitting procedure for all ROM functions to generate response surfaces." "RMROPTIONS - Defines options for ROM response surface fitting.
RMROPTIONS, RefName, Type, Invert" "RMRPLOT - Plots response surface of ROM function or its derivatives with respect to the dominant mode(s).
RMRPLOT, RefName, Type, Mode1, Mode2" "RMRSTATUS - Prints status of response surface for ROM function.
RMRSTATUS, RefName" "RMSAVE - Saves ROM data to file.
RMSAVE, Fname, Ext, --" "RMSMPLE - Runs finite element solutions and obtains sample points for the ROM method.
RMSMPLE, Nlgeom, Cap, Seqslv, Eeqslv" "RMUSE - Activates ROM use pass for ROM elements.
RMUSE, Option, Usefil" "RMXPORT
RMXPORT - Exports ROM model to external VHDL-AMS simulator." "ROCK - Specifies a rocking response spectrum.
ROCK, CGX, CGY, CGZ, OMX, OMY, OMZ" "RPOLY - Creates a regular polygonal area centered about the working plane origin.
RPOLY, NSIDES, LSIDE, MAJRAD, MINRAD" "RPR4 - Creates a regular polygonal area or prism volume anywhere on the working plane.
RPR4, NSIDES, XCENTER, YCENTER, RADIUS, THETA, DEPTH" "RPRISM - Creates a regular prism volume centered about the working plane origin.
RPRISM, Z1, Z2, NSIDES, LSIDE, MAJRAD, MINRAD" "RPSD - Computes response power spectral density (PSD).
RPSD, IR, IA, IB, ITYPE, DATUM, Name" "RSFIT - Fit a response surface for an output parameter in a solution set.
RSFIT, RSlab, Slab, Name, Rmod, Ytrans, Yval, Xfilt, CONF" "RSOPT - Creates or loads the radiosity mapping data file for SURF251 or SURF252 element types.
RSOPT, Opt, Filename, Ext, Dir" "RSPEED - Supplies system performance information for use in time estimates.
RSPEED, MIPS, SMFLOP, VMFLOP" "RSPLIT - Creates one or more results file(s) from the current results file based on subsets of elements.
RSPLIT, Option, Label, Name1, Name2, Name3, Name4, Name5, Name6, Name7, Name8, Name9, Name10, Name11, Name12, Name13, Name14, Name15, Name16" "RSPLOT - Plot a response surface.
RSPLOT, RSlab, YName, X1Name, X2Name, Type, NPTS, PLOW, PUP" "RSPRNT - Print a response surface.
RSPRNT, RSlab, YName, Xout" "RSSIMS - Performs Monte Carlo simulations on response surface(s).
RSSIMS, RSlab, NSIM, Seed" "RSTAT
RSTAT - Prints the FE model statistics of the model. " "RSTOFF - Offsets node or element IDs in the FE geometry record.
RSTOFF, Lab, OFFSET" "RSURF - Generates the radiosity surface elements (SURF251/SURF252) and stores them in the database.
RSURF, Options, Delopts, ETNUM" "RSYMM - Defines the plane of symmetry or center of rotation for the radiosity method.
RSYMM, Option, CS, Axis, NSECT, CONDVALUE" "RSYS - Activates a coordinate system for printout or display of element and nodal results.
RSYS, KCN" "RTHICK - Defines variable thickness at nodes for shell elements.
RTHICK, Par, ILOC, JLOC, KLOC, LLOC" "RTIMST
RTIMST - Prints runtime estimates. " "RUN - Defines a pipe run.
RUN, DX, DY, DZ, NDIV, NEND, ESTRT, EINC" "RWFRNT
RWFRNT - Generates wavefront statistics and memory requirements. " "SABS - Specifies absolute values for element table operations.
SABS, KEY" "SADD - Forms an element table item by adding two existing items.
SADD, LabR, Lab1, Lab2, FACT1, FACT2, CONST" "SALLOW - Defines the allowable stress table for safety factor calculations.
SALLOW, STRS1, STRS2, STRS3, STRS4, STRS5, STRS6" "SARPLOT - Displays areas smaller than a specified size (for models imported from CAD files).
SARPLOT, Prefer, VALUE" "SAVE - Saves all current database information.
SAVE, Fname, Ext, --, Slab" "SBCLIST
SBCLIST - Lists solid model boundary conditions." "SBCTRAN
SBCTRAN - Transfers solid model loads and boundary conditions to the FE model." "SDELETE - Deletes cross sections from the ANSYS database.
SDELETE, SFIRST, SLAST, SINC, KNOCLEAN,--, LCHK" "SE - Defines a superelement.
SE, File, --, --, TOLER" "SECCONTROLS - Overrides program calculated properties.
SECCONTROLS, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10, VAL11, VAL12" "SECDATA - Describes the geometry of a section.
SECDATA, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10" "SECFUNCTION - Specifies shell section thickness as a tabular function.
SECFUNCTION, TABLE" "SECJOINT - Defines local coordinate systems at joint element nodes or relative DOFs to be fixed for a general joint element.
SECJOINT, Kywrd, Val1, Val2, Val3, Val4, Val5, Val6" "SECLOCK - Specifies locks on the components of relative motion in a joint element.
SECLOCK, dof, MINVALUE, MAXVALUE, dof, MINVALUE, MAXVALUE, dof, MINVALUE, MAXVALUE" "SECMODIF - Modifies a pretension section
SECMODIF, SECID, Kywrd" "SECNUM - Sets the element section attribute pointer.
SECNUM, SECID" "SECOFFSET - Defines the section offset for cross sections.
SECOFFSET, Location, OFFSET1, OFFSET2, CG-Y, CG-Z, SH-Y, SH-Z" "SECPLOT - Plots the geometry of a beam or shell section to scale.
SECPLOT, SECID, VAL1, VAL2" "SECREAD - Reads a customized beam section library or a user-defined beam section mesh into ANSYS.
SECREAD, Fname, Ext, --, Option" "SECSTOP - Specifies stops on the components of relative motion in a joint element.
SECSTOP, dof, MINVALUE, MAXVALUE, dof, MINVALUE, MAXVALUE, dof, MINVALUE, MAXVALUE" "SECTYPE - Associates section type information with a section ID number.
SECTYPE, SECID, Type, Subtype, Name, REFINEKEY" "SECWRITE - Creates an ASCII file containing user mesh section information.
SECWRITE, Fname, Ext, --, ELEM_TYPE" "SED - Defines the excitation direction for a single-point response spectrum.
SED, SEDX, SEDY, SEDZ" "SEDLIST - Lists the DOF solution of a superelement after the use pass.
SEDLIST, Sename, KOPT" "SEEXP - Specifies options for the substructure expansion pass.
SEEXP, Sename, Usefil, Imagky, Expopt" "SEGEN - Automatically generate superelements.
SEGEN, Mode, nSuper, mDof, stopStage" "SELIST - Lists the contents of a superelement matrix file.
SELIST, Sename, KOPT" "SELM
SELM - Specifies \"Superelements\" as the subsequent status topic." "SELTOL - Sets the tolerance for subsequent select operations.
SELTOL, Toler" "SENERGY - Determines the stored magnetic energy or co-energy.
SENERGY, OPT, ANTYPE" "SEOPT - Specifies substructure analysis options.
SEOPT, Sename, SEMATR, SEPR, SESST, EXPMTH" "SESYMM - Performs a symmetry operation on a superelement within the use pass.
SESYMM, Sename, Ncomp, INC, File, Ext, --" "SET - Defines the data set to be read from the results file.
SET, Lstep, Sbstep, Fact, KIMG, TIME, ANGLE, NSET, ORDER" "SETFGAP - Updates or defines the real constant table for squeeze film elements.
SETFGAP, GAP, ROPT, --, PAMB, ACF1, ACF2, PREF, MFP" "SETRAN - Creates a superelement from an existing superelement.
SETRAN, Sename, KCNTO, INC, File, Ext, --, DX, DY, DZ, NOROT" "SEXP - Forms an element table item by exponentiating and multiplying.
SEXP, LabR, Lab1, Lab2, EXP1, EXP2" "SF - Specifies surface loads on nodes.
SF, Nlist, Lab, VALUE, VALUE2" "SFA - Specifies surface loads on the selected areas.
SFA, AREA, LKEY, Lab, VALUE, VALUE2" "SFACT - Allows safety factor or margin of safety calculations to be made.
SFACT, TYPE" "SFADELE - Deletes surface loads from areas.
SFADELE, AREA, LKEY, Lab" "SFALIST - Lists the surface loads for the specified area.
SFALIST, AREA, Lab" "SFBEAM - Specifies surface loads on beam elements.
SFBEAM, ELEM, LKEY, Lab, VALI, VALJ, VAL2I, VAL2J, IOFFST, JOFFST" "SFCALC - Calculates the safety factor or margin of safety.
SFCALC, LabR, LabS, LabT, TYPE" "SFCUM - Specifies that surface loads are to be accumulated.
SFCUM, Lab, Oper, FACT, FACT2" "SFDELE - Deletes surface loads.
SFDELE, Nlist, Lab" "SFE - Specifies surface loads on elements.
SFE, ELEM, LKEY, Lab, KVAL, VAL1, VAL2, VAL3, VAL4" "SFEDELE - Deletes surface loads from elements.
SFEDELE, ELEM, LKEY, Lab" "SFELIST - Lists the surface loads for elements.
SFELIST, ELEM, Lab" "SFFUN - Specifies a varying surface load.
SFFUN, Lab, Par, Par2" "SFGRAD - Specifies a gradient (slope) for surface loads.
SFGRAD, Lab, SLKCN, Sldir, SLZER, SLOPE" "SFL - Specifies surface loads on lines of an area.
SFL, LINE, Lab, VALI, VALJ, VAL2I, VAL2J" "SFLDELE - Deletes surface loads from lines.
SFLDELE, LINE, Lab" "SFLIST - Lists surface loads.
SFLIST, NODE, Lab" "SFLLIST - Lists the surface loads for lines.
SFLLIST, LINE, Lab" "SFSCALE - Scales surface loads on elements.
SFSCALE, Lab, FACT, FACT2" "SFTRAN
SFTRAN - Transfer the solid model surface loads to the finite element model." "SHELL - Selects a shell element or shell layer location for results output.
SHELL, Loc" "SHPP - Controls element shape checking.
SHPP, Lab, VALUE1, VALUE2" "SHSD - Creates or deletes shell-solid interface to be used in shell-to-solid assemblies.
SHSD, RID, Action" "SLIST - Summarizes the section properties for all defined sections in the current session of ANSYS.
SLIST, SFIRST, SLAST, SINC, Details, Type" "SLOAD - Load a pretension section.
SLOAD, SECID, PLNLAB, KINIT, KFD, FDVALUE, LSLOAD, LSLOCK" "SLPPLOT - Displays line loops smaller than a specified size (for models imported from CAD files).
SLPPLOT, Prefer, VALUE" "SLSPLOT - Displays line segments smaller than a specified size (for models imported from CAD files).
SLSPLOT, Prefer, VALUE" "SMALL - Finds the smallest of three variables.
SMALL, IR, IA, IB, IC, Name, --, --, FACTA, FACTB, FACTC" "SMAX - Forms an element table item from the maximum of two other items.
SMAX, LabR, Lab1, Lab2, FACT1, FACT2" "SMBODY
SMBODY - Specifies \"Body loads on the solid model\" as the subsequent status topic." "SMCONS
SMCONS - Specifies \"Constraints on the solid model\" as the subsequent status topic." "SMFOR
SMFOR - Specifies \"Forces on the solid model\" as the subsequent status topic." "SMIN - Forms an element table item from the minimum of two other items.
SMIN, LabR, Lab1, Lab2, FACT1, FACT2" "SMOOTH - Allows smoothing of noisy data and provides a graphical representation of the data.
SMOOTH, Vect1, Vect2, DATAP, FITPT, Vect3, Vect4, DISP" "SMRTSIZE - Specifies meshing parameters for automatic (smart) element sizing.
SMRTSIZE, SIZLVL, FAC, EXPND, TRANS, ANGL, ANGH, GRATIO, SMHLC, SMANC, MXITR, SPRX" "SMSURF
SMSURF - Specifies \"Surface loads on the solid model\" as the subsequent status topic." "SMULT - Forms an element table item by multiplying two other items.
SMULT, LabR, Lab1, Lab2, FACT1, FACT2" "SOLCONTROL - Specifies whether to use optimized nonlinear solution defaults and some enhanced internal solution algorithms.
SOLCONTROL, Key1, Key2, Key3, Vtol" "SOLU - Specifies solution summary data per substep to be stored.
SOLU, NVAR, Item, Comp, Name" "SOLUOPT
SOLUOPT - Specifies \"Solution options\" as the subsequent status topic." "SOLVE
SOLVE - Starts a solution." "SORT
SORT - Specifies \"Sort settings\" as the subsequent status topic." "SOURCE - Defines a default location for undefined nodes or keypoints.
SOURCE, X, Y, Z" "SPACE - Defines a space node for radiation using the Radiation Matrix method.
SPACE, NODE" "SPADP - Automatically refines a tetrahedral element mesh based on S-parameter convergence.
SPADP, FREQ, NUMADP, RMSSP, FACTB, FACTE, SLVOPT, SLVACC" "SPARM - Calculates scattering (S) parameters between ports of a network system.
SPARM, Porti, Portj" "SPCNOD - Defines a space node for radiation using the Radiosity method.
SPCNOD, ENCL, NODE" "SPCTEMP - Defines a free-space ambient temperature for radiation using the Radiosity method.
SPCTEMP, ENCL, TEMP" "SPEC
SPEC - Specifies \"Miscellaneous specifications\" as the subsequent status topic." "SPH4 - Creates a spherical volume anywhere on the working plane.
SPH4, XCENTER, YCENTER, RAD1, RAD2" "SPH5 - Creates a spherical volume by diameter end points.
SPH5, XEDGE1, YEDGE1, XEDGE2, YEDGE2" "SPHERE - Creates a spherical volume centered about the working plane origin.
SPHERE, RAD1, RAD2, THETA1, THETA2" "SPICE - Generates a SPICE subcircuit model using S-parameters from a Touchstone file.
SPICE, Fname, Ext, RMSERR, Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10, Z11, Z12, Z13, Z14, Z15, Z16" "SPLINE - Generates a segmented spline through a series of keypoints.
SPLINE, P1, P2, P3, P4, P5, P6, XV1, YV1, ZV1, XV6, YV6, ZV6" "SPLOT - Displays the selected areas and a faceted view of their underlying surfaces
SPLOT, NA1, NA2, NINC, MESH" "SPOINT - Defines a point for moment summations.
SPOINT, NODE, X, Y, Z" "SPOPT - Selects the spectrum type and other spectrum options.
SPOPT, Sptype, NMODE, Elcalc" "SPREAD - Turns on a dashed tolerance curve for the subsequent curve plots.
SPREAD, VALUE" "SPSCAN - Performs a harmonic analysis of a unit cell over a range of angles and extracts the S-parameter.
SPSCAN, FREQ, LOCAL, PHIB, PHIE, PHIINC, THETAB, THETAE, THETAINC, FILEOPT" "SPSWP - Computes S-parameters over a frequency range and writes them to a file.
SPSWP, FREQB, FREQE, FREQINC, SWPOPT, EFACC, OUTPUT, FILEOPT, SLVOPT, SLVACC" "SPTOPT
SPTOPT - Specifies \"Spectrum analysis options\" as the subsequent status topic." "SQRT - Forms the square root of a variable.
SQRT, IR, IA, --, --, Name, --, --, FACTA" "SRSS - Specifies the square root of sum of squares mode combination method.
SRSS, SIGNIF, Label" "SSBT - Specifies preintegrated bending thermal effects for shell sections.
SSBT, BT11, BT22, BT12, T" "SSLN - Selects and displays small lines in the model.
SSLN, FACT, SIZE" "SSMT - Specifies preintegrated membrane thermal effects for shell sections.
SSMT, MT11, MT22, MT12, T" "SSPA - Specifies a preintegrated membrane stiffness for shell sections.
SSPA, A11, A21, A31, A22, A32, A33, T" "SSPB - Specifies a preintegrated coupling stiffness for shell sections.
SSPB, B11, B21, B31, B22, B32, B33, T" "SSPD - Specifies a preintegrated bending stiffness for shell sections.
SSPD, D11, D21, D31, D22, D32, D33, T" "SSPE - Specifies a preintegrated transverse shear stiffness for shell sections.
SSPE, E11, E21, E22, T" "SSPM - Specifies mass density for a preintegrated shell section.
SSPM, DENS, T" "SSTIF - Activates stress stiffness effects in a nonlinear analysis.
SSTIF, Key" "SSUM
SSUM - Calculates and prints the sum of element table items." "STABILIZE - Activates stabilization for all elements that support nonlinear stabilization.
STABILIZE, Key, Method, VALUE, SubStpOpt" "STAOPT - Specifies static analysis options.
STAOPT, Method" "STAT
STAT - Displays the status of database settings." "STEF - Specifies Stefan-Boltzmann radiation constant.
STEF, VALUE" "STORE - Stores data in the database for the defined variables.
STORE, Lab, NPTS" "SUBOPT - Specifies options for subspace iteration eigenvalue extraction.
SUBOPT, SUBSIZ, NPAD, NPERBK, NUMSSI, NSHIFT, Strmck, JCGITR" "SUBSET - Reads results for the selected portions of the model.
SUBSET, Lstep, SBSTEP, FACT, KIMG, TIME, ANGLE, NSET" "SUCALC - Create new result data by operating on two existing result data sets on a given surface.
SUCALC, RSetName, lab1, Oper, lab2, fact1, fact2, const" "SUCR - Create a surface.
SUCR, SurfName, SurfType, nRefine, Radius, blank, blank, TolOut" "SUDEL - Delete geometry information as well as any mapped results for specified surface.
SUDEL, SurfName" "SUEVAL - Perform operations on a mapped item and store result in a scalar parameter.
SUEVAL, Parm, lab1, Oper" "SUGET - Moves surface geometry and mapped results to an array parameter.
SUGET, SurfName, RSetName, Parm, Geom" "SUMAP - Map results onto selected surface(s).
SUMAP, RSetName, Item, Comp" "SUMTYPE - Sets the type of summation to be used in the following load case operations.
SUMTYPE, Label" "SUPL - Plot result data on all selected surfaces or on a specified surface.
SUPL, SurfName, RSetName, KWIRE" "SUPR - Print global status, geometry information and/or result information.
SUPR, SurfName, RSetName" "SURESU - Read a set of surface definitions and result items from a file and make them the current set.
SURESU, --, Fname, Fext, Fdir" "SUSAVE - Saves surface definitions to a file.
SUSAVE, Lab, Fname, Fext, Fdir" "SUSEL - Selects a subset of surfaces
SUSEL, Type, Name1, Name2, Name3, Name4, Name5, Name6, Name7, Name8" "SUVECT - Create new result data by operating on two existing result vectors on a given surface.
SUVECT, RSetName, lab1, Oper, lab2, Offset" "SV - Defines spectrum values to be associated with frequency points.
SV, DAMP, SV1, SV2, SV3, SV4, SV5, SV6, SV7, SV8, SV9" "SVTYP - Defines the type of single-point response spectrum.
SVTYP, KSV, FACT" "SWADD - Adds more surfaces to an existing spot weld set.
SWADD, Ecomp, SHRD, NCM1, NCM2, NCM3, NCM4, NCM5, NCM6, NCM7, NCM8, NCM9" "SWDEL - Deletes spot weld sets.
SWDEL, Ecomp" "SWGEN - Creates a new spot weld set.
SWGEN, Ecomp, SWRD, NCM1, NCM2, SND1, SND2, SHRD, DIRX, DIRY, DIRZ, ITTY, ICTY" "SWLIST - Lists spot weld sets.
SWLIST, Ecomp" "SYNCHRO - Specifies whether the excitation frequency is synchronous or asynchronous with the rotational velocity of a structure.
SYNCHRO, RATIO, Cname" "TALLOW - Defines the temperature table for safety factor calculations.
TALLOW, TEMP1, TEMP2, TEMP3, TEMP4, TEMP5, TEMP6" "TB - Activates a data table for nonlinear material properties or special element input.
TB, Lab, MAT, NTEMP, NPTS, TBOPT, EOSOPT" "TBCOPY - Copies a data table from one material to another (see ).
TBCOPY, Lab, MATF, MATT" "TBDATA - Defines data for the data table.
TBDATA, STLOC, C1, C2, C3, C4, C5, C6" "TBDELE - Deletes previously defined data tables.
TBDELE, Lab, MAT1, MAT2, INC" "TBFIELD - Defines values of field variables for the material data tables.
TBFIELD, Type, Value" "TBFT - Performs material curve fitting operations.
TBFT, Oper, ID, Option1, Option2, Option3, Option4, Option5, Option6, Option7" "TBLE
TBLE - Specifies \"Data table properties\" as the subsequent status topic." "TBLIST - Lists the data tables.
TBLIST, Lab, MAT" "TBMODIF - Modifies data for the data table (GUI).
TBMODIF, ROW, COL, VALUE" "TBPLOT - Displays the data table.
TBPLOT, Lab, MAT, TBOPT, TEMP, SEGN" "TBPT - Defines a point on a nonlinear data curve.
TBPT, Oper, X, Y" "TBTEMP - Defines a temperature for the data table.
TBTEMP, TEMP, KMOD" "TCHG - Converts 20-node degenerate tetrahedral elements to their 10-node non-degenerate counterparts.
TCHG, ELEM1, ELEM2, ETYPE2" "TEE - Defines a tee in a piping run.
TEE, NCENT, TYPE, ELEM, EINC, L1, L2, L3" "TERM - Specifies various terminal driver options.
TERM, Kywrd, Opt1, Opt2, Opt3" "THOPT - Nonlinear transient thermal solution option.
THOPT, Refopt, REFORMTOL, NTABPOINTS, TEMPMIN, TEMPMAX" "TIFF - Provides TIFF file Export for ANSYS Displays.
TIFF, Kywrd, OPT" "TIME - Sets the time for a load step.
TIME, TIME" "TIMERANGE - Specifies the time range for which data are to be stored.
TIMERANGE, TMIN, TMAX" "TIMINT - Turns on transient effects.
TIMINT, Key, Lab" "TIMP - Improves the quality of tetrahedral elements that are not associated with a volume.
TIMP, ELEM, CHGBND, IMPLEVEL" "TINTP - Defines transient integration parameters.
TINTP, GAMMA, ALPHA, DELTA, THETA, OSLM, TOL, --, --, AVSMOOTH, ALPHAF, ALPHAM" "TOCOMP - Defines single or multiple compliance as the topological optimization function.
TOCOMP, Refname, Type, NUMLC, LCARR" "TODEF - Defines parameters for and initializes topological optimization.
TODEF, ACCUR" "TOEXE
TOEXE - Executes one topological optimization iteration." "TOFFST - Specifies the temperature offset from absolute zero to zero.
TOFFST, VALUE" "TOFREQ - Defines single or mean frequency formulation as the topological optimization function.
TOFREQ, Refname, Type, Nfreq, Frqarr, Targval" "TOGRAPH - Plots iteration solution of topological optimization.
TOGRAPH, Type, Refname" "TOLIST
TOLIST - Lists all topological optimization functions currently defined." "TOLOOP - Execute several topological optimization iterations.
TOLOOP, NITER, PLOT" "TOPLOT - Plot current topological density distribution.
TOPLOT, AVRG" "TOPRINT - Print iteration solution history of topological optimization.
TOPRINT, Type, Refname" "TORQ2D
TORQ2D - Calculates torque on a body in a magnetic field." "TORQC2D - Calculates torque on a body in a magnetic field based on a circular path.
TORQC2D, RAD, NUMN, LCSYS" "TORQSUM - Summarizes electromagnetic torque calculations on element components.
TORQSUM, Cnam1, Cnam2, Cnam3, Cnam4, Cnam5, Cnam6, Cnam7, Cnam8, Cnam9" "TORUS - Creates a toroidal volume.
TORUS, RAD1, RAD2, RAD3, THETA1, THETA2" "TOSTAT
TOSTAT - Displays topological optimization status and results information." "TOTAL - Specifies automatic MDOF generation.
TOTAL, NTOT, NRMDF" "TOTYPE - Specifies solution method for topological optimization.
TOTYPE, Type" "TOVAR - Specifies the objective and constraints for the topological optimization problem.
TOVAR, Refname, Type, LOWER, UPPER, Boundtype" "TRANS - Reformats File.GRPH for improved performance with plotters.
TRANS, Fname, Ext, --" "TRANSFER - Transfers a pattern of nodes to another coordinate system.
TRANSFER, KCNTO, INC, NODE1, NODE2, NINC" "TREF - Defines the reference temperature for the thermal strain calculations.
TREF, TREF" "TRNOPT - Specifies transient analysis options.
TRNOPT, Method, MAXMODE, Dmpkey, MINMODE, MCout, TINTOPT" "TRPDEL - Deletes particle flow or charged particle trace points.
TRPDEL, NTRP1, NTRP2, TRPINC" "TRPLIS - Lists the particle flow or charged particle trace points.
TRPLIS, NTRP1, NTRP2, TRPINC, Opt" "TRPOIN - Defines a point through which a particle flow or charged particle trace will travel.
TRPOIN, X, Y, Z, VX, VY, VZ, CHRG, MASS" "TRTIME - Defines the options used for the PLTRAC (particle flow or charged particle trace) command.
TRTIME, TIME, SPACING, OFFSET, SIZE, LENGTH" "TSHAP - Defines simple 2-D and 3-D geometric surfaces for target segment elements.
TSHAP, Shape" "TSRES - Defines an array of keytimes at which the time-stepping strategy changes.
TSRES, Array" "TUNIF - Assigns a uniform temperature to all nodes.
TUNIF, TEMP" "TVAR - Changes time to the cumulative iteration number.
TVAR, KEY" "TYPE - Sets the element type attribute pointer.
TYPE, ITYPE" "TZAMESH - Meshes the areas of a volume to create Trefftz nodes.
TZAMESH, Tvolu, SIZE, NDIV" "TZDELE
TZDELE - Deletes the Trefftz superelement, associated constraint equations and all supporting Trefftz files." "TZEGEN
TZEGEN - Generates a Trefftz domain substructure and defines a Trefftz superelement for use in electrostatic analysis." "UIMP - Defines constant material properties (GUI).
UIMP, MAT, Lab1, Lab2, Lab3, VAL1, VAL2, VAL3" "UNDELETE - Removes results sets from the group of sets selected for editing.
UNDELETE, Option, Nstart, Nend" "UNDO - Allows the user to modify or save commands issued since the last RESUME or SAVE command.
UNDO, Kywrd" "UPCOORD - Modifies the coordinates of the active set of nodes, based on the current displacements.
UPCOORD, FACTOR, Key" "UPGEOM - Adds displacements from a previous analysis and updates the geometry of the finite element model to the deformed configuration.
UPGEOM, FACTOR, LSTEP, SBSTEP, Fname, Ext, --" "USRCAL - Allows user-solution subroutines to be activated or deactivated.
USRCAL, Rnam1, Rnam2, Rnam3, Rnam4, Rnam5, Rnam6, Rnam7, Rnam8, Rnam9" "USRDOF - Specifies the degrees of freedom for the user-defined element USER300.
USRDOF, Action, DOF1, DOF2, DOF3, DOF4, DOF5, DOF6, DOF7, DOF8, DOF9, DOF10" "USRELEM - Specifies the characteristics of the user-defined element USER300.
USRELEM, NNODES, NDIM, KeyShape, NREAL, NSAVEVARS, NRSLTVAR, KEYANSMAT, NINTPNTS, KESTRESS, KEYSYM" "V - Defines a volume through keypoints.
V, P1, P2, P3, P4, P5, P6, P7, P8" "V2DOPT - Specifies 2-D/axisymmetric view factor calculation options.
V2DOPT, GEOM, NDIV, HIDOPT, NZONE" "VA - Generates a volume bounded by existing areas.
VA, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10" "VADD - Adds separate volumes to create a single volume.
VADD, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VALVE - Defines a valve in a piping run.
VALVE, NLOC, LENG, MASS, SIF, FLEX, ARINS, ELEM" "VARDEL - Deletes a variable (GUI).
VARDEL, NVAR" "VARNAM - Names (or renames) a variable.
VARNAM, IR, Name" "VATT - Associates element attributes with the selected, unmeshed volumes.
VATT, MAT, REAL, TYPE, ESYS" "VCLEAR - Deletes nodes and volume elements associated with selected volumes.
VCLEAR, NV1, NV2, NINC" "VCROSS - Forms element table items from the cross product of two vectors.
VCROSS, LabXR, LabYR, LabZR, LabX1, LabY1, LabZ1, LabX2, LabY2, LabZ2" "VCVFILL - Fills cavities and bosses in volumes (for models imported from CAD files).
VCVFILL, A1, A2, A3, A4, A5, A6, A7, A9, A9, A10" "VDDAM - Specifies the velocity spectrum computation constants for the analysis of shock resistance of shipboard structures.
VDDAM, VF, VA, VB, VC" "VDELE - Deletes unmeshed volumes.
VDELE, NV1, NV2, NINC, KSWP" "VDGL - Lists keypoints of a volume that lie on a parametric degeneracy.
VDGL, NV1, NV2, NINC" "VDOT - Forms an element table item from the dot product of two vectors.
VDOT, LabR, LabX1, LabY1, LabZ1, LabX2, LabY2, LabZ2" "VDRAG - Generates volumes by dragging an area pattern along a path.
VDRAG, NA1, NA2, NA3, NA4, NA5, NA6, NLP1, NLP2, NLP3, NLP4, NLP5, NLP6" "VEORIENT - Specifies brick element orientation for volume mapped (hexahedron) meshing.
VEORIENT, VNUM, Option, VALUE1, VALUE2" "VEXT - Generates additional volumes by extruding areas.
VEXT, NA1, NA2, NINC, DX, DY, DZ, RX, RY, RZ" "VFCALC - Computes and stores Hemicube view factors.
VFCALC, Fname, Ext, --" "VFOPT - Specifies options for view factor file.
VFOPT, Opt, Filename, Ext, Dir, Format" "VFQUERY - Queries and prints element Hemicube view factors and average view factor.
VFQUERY, SRCELEM, TARELEM" "VGEN - Generates additional volumes from a pattern of volumes.
VGEN, ITIME, NV1, NV2, NINC, DX, DY, DZ, KINC, NOELEM, IMOVE" "VGET - Moves a variable into an array parameter vector.
VGET, Par, IR, TSTRT, KCPLX" "VGLUE - Generates new volumes by \"gluing\" volumes.
VGLUE, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VIMP - Improves the quality of the tetrahedral elements in the selected volume(s).
VIMP, VOL, CHGBND, IMPLEVEL" "VINP - Finds the pairwise intersection of volumes.
VINP, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VINV - Finds the intersection of volumes.
VINV, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VLIST - Lists the defined volumes.
VLIST, NV1, NV2, NINC" "VLSCALE - Generates a scaled set of volumes from a pattern of volumes.
VLSCALE, NV1, NV2, NINC, RX, RY, RZ, KINC, NOELEM, IMOVE" "VMESH - Generates nodes and volume elements within volumes.
VMESH, NV1, NV2, NINC" "VOFFST - Generates a volume, offset from a given area.
VOFFST, NAREA, DIST, KINC" "VOLUMES
VOLUMES - Specifies \"Volumes\" as the subsequent status topic." "VOVLAP - Overlaps volumes.
VOVLAP, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VPLOT - Displays the selected volumes.
VPLOT, NV1, NV2, NINC, DEGEN, SCALE" "VPTN - Partitions volumes.
VPTN, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VPUT - Moves an array parameter vector into a variable.
VPUT, Par, IR, TSTRT, KCPLX, Name" "VROTAT - Generates cylindrical volumes by rotating an area pattern about an axis.
VROTAT, NA1, NA2, NA3, NA4, NA5, NA6, PAX1, PAX2, ARC, NSEG" "VSBA - Subtracts areas from volumes.
VSBA, NV, NA, SEPO, KEEPV, KEEPA" "VSBV - Subtracts volumes from volumes.
VSBV, NV1, NV2, SEPO, KEEP1, KEEP2" "VSBW - Subtracts intersection of the working plane from volumes (divides volumes).
VSBW, NV, SEPO, KEEP" "VSEL - Selects a subset of volumes.
VSEL, Type, Item, Comp, VMIN, VMAX, VINC, KSWP" "VSLA - Selects those volumes containing the selected areas.
VSLA, Type, VLKEY" "VSUM - Calculates and prints geometry statistics of the selected volumes.
VSUM, LAB" "VSWEEP - Fills an existing unmeshed volume with elements by sweeping the mesh from an adjacent area through the volume.
VSWEEP, VNUM, SRCA, TRGA, LSMO" "VSYMM - Generates volumes from a volume pattern by symmetry reflection.
VSYMM, Ncomp, NV1, NV2, NINC, KINC, NOELEM, IMOVE" "VTCLR - Clears the Variational Technology database.
VTCLR, Type" "VTDISC - Defines an element component as a discrete input variable for the DesignXplorer.
VTDISC, Name, ElComp" "VTEVAL - Triggers evaluation of generated results based on input variables specified via the VTVMOD command.
VTEVAL, --, Mode" "VTFREQ - Defines the frequency as input variable for the harmonic sweep functionality of VT Accelerator.
VTFREQ, Name, MIN, MAX, INC, RedOpt" "VTGEOM - Defines a geometry parameter created with ANSYS Mesh Morpher as a DesignXplorer input variable.
VTGEOM, Name, MIN, MAX, RedOpt, Order" "VTIN - Defines an inertial load as an input variable for DesignXplorer.
VTIN, Name, MIN, MAX, RedOpt, Lab, Comp, --, VarType, Order" "VTMETH - Defines the solution options for the DesignXplorer.
VTMETH, SoluType, ApprType, ModeTrack" "VTMP - Defines a material property as an input variable for DesignXplorer.
VTMP, Name, MIN, MAX, RedOpt, Lab, MAT, ElComp, VarType, Order" "VTOP - Defines options value for the DesignXplorer.
VTOP, Oper, Name, Value" "VTPOST
VTPOST - Launches the DesignXplorer postprocessing application." "VTRAN - Transfers a pattern of volumes to another coordinate system.
VTRAN, KCNTO, NV1, NV2, NINC, KINC, NOELEM, IMOVE" "VTREAL - Defines a real constant property as an input variable for the DesignXplorer.
VTREAL, Name, MIN, MAX, RedOpt, Lab, NSET, ElComp, VarType, Order" "VTRFIL - Specifies the file to which DesignXplorer results are written.
VTRFIL, Fname, Ext, Dir" "VTRSLT - Defines a result quantity for the DesignXplorer.
VTRSLT, Name, Entity, Type, Comp, ACC, CompName" "VTSEC - Defines a section property as an input variable for DesignXplorer.
VTSEC, Name, MIN, MAX, RedOpt, Lab, SECID, LAYERID, ElComp, VarType, Order" "VTSFE - Defines a surface load as an input variable for the DesignXplorer.
VTSFE, Name, MIN, MAX, RedOpt, Lab, LKEY, ElComp, VarType, Order" "VTSL - Selects a subset of elements associated with an VT input variable.
VTSL, Type, Varname" "VTSTAT - Print the status of the DesignXplorer definitions and settings into a separate window.
VTSTAT, EntyLis" "VTTEMP - Defines the temperature as input variable for the DesignXplorer.
VTTEMP,Name, MIN, MAX, RedOpt, --, VarType, Order" "VTVMOD - Modifies the status or current value of an input variable for the DesignXplorer.
VTVMOD, Name, Oper, Value" "VTYPE - Specifies the viewing procedure used to determine the form factors for the Radiation Matrix method.
VTYPE, NOHID, NZONE" "WAVES - Initiates reordering.
WAVES, Wopt, OLDMAX, OLDRMS" "WERASE
WERASE - Erases all reordering wave lists." "WFRONT - Estimates wavefront statistics.
WFRONT, KPRNT, KCALC" "WMID - Specifies reordering options for the WAVES command.
WMID, Key" "WMORE - Adds more nodes to the starting wave list.
WMORE, NODE1, NODE2, NINC, ITIME, INC" "WPAVE - Moves the working plane origin to the average of specified points.
WPAVE, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3" "WPCSYS - Defines the working plane location based on a coordinate system.
WPCSYS, WN, KCN" "WPLANE - Defines a working plane to assist in picking operations.
WPLANE, WN, XORIG, YORIG, ZORIG, XXAX, YXAX, ZXAX, XPLAN, YPLAN, ZPLAN" "WPOFFS - Offsets the working plane.
WPOFFS, XOFF, YOFF, ZOFF" "WPROTA - Rotates the working plane.
WPROTA, THXY, THYZ, THZX" "WPSTYL - Controls the display and style of the working plane.
WPSTYL, SNAP, GRSPAC, GRMIN, GRMAX, WPTOL, WPCTYP, GRTYPE, WPVIS, SNAPANG" "WRFULL - Stops solution after assembling global matrices.
WRFULL, Ldstep" "WRITE - Writes the radiation matrix file.
WRITE, Fname" "WSORT - Initiates element reordering based upon a geometric sort.
WSORT, Lab, KORD, --, Wopt, OLDMAX, OLDRMS" "WSPRINGS
WSPRINGS - Creates weak springs on corner nodes of a bounding box of the currently selected elements." "WSTART - Defines a starting wave list.
WSTART, NODE1, NODE2, NINC, ITIME, INC" "XVAR - Specifies the X variable to be displayed.
XVAR, N" "XVAROPT - Specifies the parameter to be used as the X-axis variable.
XVAROPT, Lab" "~CAT5IN - Transfers a .CATPart file into the ANSYS program.
~CAT5IN, Name, Extension, Path, Entity, FMT, NOCL, NOAN" "~CATIAIN - Transfers a CATIA model into the ANSYS program.
~CATIAIN, Name, Extension, Path, - -, - -, BLANK, - -" "~PARAIN - Transfers a Parasolid file into the ANSYS program.
~PARAIN, Name, Extension, Path, Entity, FMT, Scale" "~PROEIN - Transfers a Pro/ENGINEER part into the ANSYS program.
~PROEIN, Name, Extension, Path, Proecomm, FMT" "~SATIN - Transfers a .SAT file into the ANSYS program.
~SATIN, Name, Extension, Path, Entity, FMT, NOCL, NOAN" "~UGIN - Transfers a Unigraphics part into the ANSYS program.
~UGIN, Name, Extension, Path, Entity, LAYER, FMT")
  "Help strings for the parameters of Ansys keywords."
  )

(defconst                                          ansys-commands
  '(("\\(^\\|\\$\\)\\s-*\\(/ZOO\\)\\(\\(?:M\\)?\\)\\(\\w*\\)"     (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/YRA\\)\\(\\(?:N\\(?:G\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(XVARO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(XVAR\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/XRA\\)\\(\\(?:N\\(?:G\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/XFR\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WSTA\\)\\(\\(?:R\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WSPR\\)\\(\\(?:I\\(?:N\\(?:G\\(?:S\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WSOR\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WRIT\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WRFU\\)\\(\\(?:L\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WPST\\)\\(\\(?:Y\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WPRO\\)\\(\\(?:T\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WPOF\\)\\(\\(?:F\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WPLA\\)\\(\\(?:N\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WPCS\\)\\(\\(?:Y\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WPAV\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WMOR\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(WMID\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/WIN\\)\\(\\(?:D\\(?:O\\(?:W\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WFRO\\)\\(\\(?:N\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WERA\\)\\(\\(?:S\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(WAVE\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/VUP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTYP\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTVM\\)\\(\\(?:O\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTTE\\)\\(\\(?:M\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTST\\)\\(\\(?:A\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VTSL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTSF\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTSE\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTRS\\)\\(\\(?:L\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTRF\\)\\(\\(?:I\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTRE\\)\\(\\(?:A\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTRA\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTPO\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VTOP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VTMP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTME\\)\\(\\(?:T\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VTIN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTGE\\)\\(\\(?:O\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTFR\\)\\(\\(?:E\\(?:Q\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTEV\\)\\(\\(?:A\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTDI\\)\\(\\(?:S\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VTCL\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(/VT\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VSYM\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VSWE\\)\\(\\(?:E\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VSUM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VSLA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VSEL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/VSC\\)\\(\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VSBW\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VSBV\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VSBA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VROT\\)\\(\\(?:A\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VPUT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VPTN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VPLO\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VOVL\\)\\(\\(?:A\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VOLU\\)\\(\\(?:M\\(?:E\\(?:S\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VOFF\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VMES\\)\\(\\(?:H\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VLSC\\)\\(\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VLIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VINV\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VINP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VIMP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/VIE\\)\\(\\(?:W\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VGLU\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VGET\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VGEN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VFQU\\)\\(\\(?:E\\(?:R\\(?:Y\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VFOP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VFCA\\)\\(\\(?:L\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VEXT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VEOR\\)\\(\\(?:I\\(?:E\\(?:N\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VDRA\\)\\(\\(?:G\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VDOT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VDGL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VDEL\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VDDA\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VCVF\\)\\(\\(?:I\\(?:L\\(?:L\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VCRO\\)\\(\\(?:S\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/VCO\\)\\(\\(?:N\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VCLE\\)\\(\\(?:A\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VATT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VARN\\)\\(\\(?:A\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VARD\\)\\(\\(?:E\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(VALV\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(VADD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(VA\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(V2DO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep) (4 (quote shadow) keep)) ("\\(^\\|\\$\\)\\s-*\\(V\\)\\>" (2
								   font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(USRE\\)\\(\\(?:L\\(?:E\\(?:M\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(USRD\\)\\(\\(?:O\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(USRC\\)\\(\\(?:A\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/USE\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(UPGE\\)\\(\\(?:O\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(UPCO\\)\\(\\(?:O\\(?:R\\(?:D\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/UNI\\)\\(\\(?:T\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(UNDO\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(UNDE\\)\\(\\(?:L\\(?:E\\(?:T\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/UIS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(UIMP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(/UI\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/UDO\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TZEG\\)\\(\\(?:E\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TZDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TZAM\\)\\(\\(?:E\\(?:S\\(?:H\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(TYPE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/TYP\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/TXT\\)\\(\\(?:R\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(TVAR\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TUNI\\)\\(\\(?:F\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TSRE\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/TSP\\)\\(\\(?:E\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TSHA\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TRTI\\)\\(\\(?:M\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TRPO\\)\\(\\(?:I\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TRPL\\)\\(\\(?:I\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TRPD\\)\\(\\(?:E\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TRNO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/TRL\\)\\(\\(?:C\\(?:Y\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/TRI\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(TREF\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TRANSF\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))     ("\\(^\\|\\$\\)\\s-*\\(TRANS\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOVA\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOTY\\)\\(\\(?:P\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOTA\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOST\\)\\(\\(?:A\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TORU\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TORQS\\)\\(\\(?:U\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TORQC\\)\\(\\(?:2\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TORQ2\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOPR\\)\\(\\(?:I\\(?:N\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOPL\\)\\(\\(?:O\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOLO\\)\\(\\(?:O\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOGR\\)\\(\\(?:A\\(?:P\\(?:H\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOFR\\)\\(\\(?:E\\(?:Q\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOFF\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOEX\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TODE\\)\\(\\(?:F\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TOCO\\)\\(\\(?:M\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/TLA\\)\\(\\(?:B\\(?:E\\(?:L\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/TIT\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TINT\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(TIMP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TIMI\\)\\(\\(?:N\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TIMER\\)\\(\\(?:A\\(?:N\\(?:G\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(TIME\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(TIFF\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(THOP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(TERM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(TEE\\)\\>"  (2 font-lock-type-face
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(TCHG\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TBTE\\)\\(\\(?:M\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(TBPT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TBPL\\)\\(\\(?:O\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TBMO\\)\\(\\(?:D\\(?:I\\(?:F\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TBLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(TBLE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(TBFT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TBFI\\)\\(\\(?:E\\(?:L\\(?:D\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TBDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TBDA\\)\\(\\(?:T\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TBCO\\)\\(\\(?:P\\(?:Y\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(TB\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(TALL\\)\\(\\(?:O\\(?:W\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/SYS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/SYP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SYNC\\)\\(\\(?:H\\(?:R\\(?:O\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SWLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SWGE\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SWDE\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SWAD\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SVTY\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(SV\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SUVE\\)\\(\\(?:C\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SUSE\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SUSA\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SURE\\)\\(\\(?:S\\(?:U\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SUPR\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SUPL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SUMT\\)\\(\\(?:Y\\(?:P\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SUMA\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SUGE\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SUEV\\)\\(\\(?:A\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SUDE\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SUCR\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SUCA\\)\\(\\(?:L\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SUBS\\)\\(\\(?:E\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SUBO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(STOR\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/STI\\)\\(\\(?:T\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(STEF\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/STA\\)\\(\\(?:T\\(?:U\\(?:S\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(STAT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(STAO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(STAB\\)\\(\\(?:I\\(?:L\\(?:I\\(?:Z\\(?:E\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SSUM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SSTI\\)\\(\\(?:F\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SSPM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SSPE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SSPD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SSPB\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SSPA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SSMT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SSLN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/SSC\\)\\(\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SSBT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SRSS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SQRT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPTO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPSW\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPSC\\)\\(\\(?:A\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPRE\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPOP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPOI\\)\\(\\(?:N\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPLO\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPLI\\)\\(\\(?:N\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPIC\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPHE\\)\\(\\(?:R\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SPH5\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SPH4\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SPEC\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPCT\\)\\(\\(?:E\\(?:M\\(?:P\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPCN\\)\\(\\(?:O\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPAR\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPAD\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SPAC\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SOUR\\)\\(\\(?:C\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SORT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SOLV\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SOLUO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/SOL\\)\\(\\(?:U\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SOLU\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SOLC\\)\\(\\(?:O\\(?:N\\(?:T\\(?:R\\(?:O\\(?:L\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SMUL\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SMSU\\)\\(\\(?:R\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SMRT\\)\\(\\(?:S\\(?:I\\(?:Z\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SMOO\\)\\(\\(?:T\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SMIN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SMFO\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SMCO\\)\\(\\(?:N\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SMBO\\)\\(\\(?:D\\(?:Y\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/SMB\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SMAX\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SMAL\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SLSP\\)\\(\\(?:L\\(?:O\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SLPP\\)\\(\\(?:L\\(?:O\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SLOA\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SLIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SHSD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/SHR\\)\\(\\(?:I\\(?:N\\(?:K\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SHPP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/SHOWD\\)\\(\\(?:I\\(?:S\\(?:P\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))     ("\\(^\\|\\$\\)\\s-*\\(/SHOW\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SHEL\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/SHA\\)\\(\\(?:D\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFTR\\)\\(\\(?:A\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFSC\\)\\(\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFLL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFLD\\)\\(\\(?:E\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(SFL\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFGR\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFFU\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFEL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFED\\)\\(\\(?:E\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(SFE\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFCU\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFCA\\)\\(\\(?:L\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFBE\\)\\(\\(?:A\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFAL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFAD\\)\\(\\(?:E\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SFAC\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(SFA\\)\\>"  (2 font-lock-type-face
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(SF\\)\\>"  (2  font-lock-type-face
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SEXP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SETR\\)\\(\\(?:A\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SETF\\)\\(\\(?:G\\(?:A\\(?:P\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(SET\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SESY\\)\\(\\(?:M\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SEOP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SENE\\)\\(\\(?:R\\(?:G\\(?:Y\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SELT\\)\\(\\(?:O\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SELM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SELI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SEGE\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/SEG\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SEEX\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SEDL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(SED\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECW\\)\\(\\(?:R\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECT\\)\\(\\(?:Y\\(?:P\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECS\\)\\(\\(?:T\\(?:O\\(?:P\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECR\\)\\(\\(?:E\\(?:A\\(?:D\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECP\\)\\(\\(?:L\\(?:O\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECO\\)\\(\\(?:F\\(?:F\\(?:S\\(?:E\\(?:T\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECN\\)\\(\\(?:U\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECM\\)\\(\\(?:O\\(?:D\\(?:I\\(?:F\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECL\\)\\(\\(?:O\\(?:C\\(?:K\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/SEC\\)\\(\\(?:L\\(?:I\\(?:B\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECJ\\)\\(\\(?:O\\(?:I\\(?:N\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECF\\)\\(\\(?:U\\(?:N\\(?:C\\(?:T\\(?:I\\(?:O\\(?:N\\)?\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECD\\)\\(\\(?:A\\(?:T\\(?:A\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SECC\\)\\(\\(?:O\\(?:N\\(?:T\\(?:R\\(?:O\\(?:L\\(?:S\\)?\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(SE\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SDEL\\)\\(\\(?:E\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SBCT\\)\\(\\(?:R\\(?:A\\(?:N\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SBCL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SAVE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SARP\\)\\(\\(?:L\\(?:O\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(SALL\\)\\(\\(?:O\\(?:W\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SADD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(SABS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RWFR\\)\\(\\(?:N\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/RUN\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(RUN\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RTIM\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RTHI\\)\\(\\(?:C\\(?:K\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(RSYS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RSYM\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RSUR\\)\\(\\(?:F\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RSTO\\)\\(\\(?:F\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RSTA\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RSSI\\)\\(\\(?:M\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RSPR\\)\\(\\(?:N\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RSPLO\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RSPLI\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RSPE\\)\\(\\(?:E\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RSOP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RSFI\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(RPSD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RPRI\\)\\(\\(?:S\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(RPR4\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RPOL\\)\\(\\(?:Y\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ROCK\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMXP\\)\\(\\(?:O\\(?:R\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMUS\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMSM\\)\\(\\(?:P\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMSA\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMRS\\)\\(\\(?:T\\(?:A\\(?:T\\(?:U\\(?:S\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMRP\\)\\(\\(?:L\\(?:O\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMRO\\)\\(\\(?:P\\(?:T\\(?:I\\(?:O\\(?:N\\(?:S\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMRG\\)\\(\\(?:E\\(?:N\\(?:E\\(?:R\\(?:A\\(?:T\\(?:E\\)?\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMRE\\)\\(\\(?:S\\(?:U\\(?:M\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMPO\\)\\(\\(?:R\\(?:D\\(?:E\\(?:R\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMOR\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMOD\\)\\(\\(?:I\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMNE\\)\\(\\(?:V\\(?:E\\(?:C\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMND\\)\\(\\(?:I\\(?:S\\(?:P\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMMS\\)\\(\\(?:E\\(?:L\\(?:E\\(?:C\\(?:T\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMMR\\)\\(\\(?:A\\(?:N\\(?:G\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMML\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMLV\\)\\(\\(?:S\\(?:C\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMFL\\)\\(\\(?:V\\(?:E\\(?:C\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMEM\\)\\(\\(?:R\\(?:Y\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMCL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMCA\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMAS\\)\\(\\(?:T\\(?:E\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMAN\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RMAL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RLIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RITE\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RIMP\\)\\(\\(?:O\\(?:R\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RIGI\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/RGB\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RFOR\\)\\(\\(?:C\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RFIL\\)\\(\\(?:S\\(?:Z\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(REZO\\)\\(\\(?:N\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(REXP\\)\\(\\(?:O\\(?:R\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RESW\\)\\(\\(?:R\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RESV\\)\\(\\(?:E\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RESU\\)\\(\\(?:M\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(RESP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/RES\\)\\(\\(?:E\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RESE\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RESC\\)\\(\\(?:O\\(?:N\\(?:T\\(?:R\\(?:O\\(?:L\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/REP\\)\\(\\(?:L\\(?:O\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(REOR\\)\\(\\(?:D\\(?:E\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/REN\\)\\(\\(?:A\\(?:M\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(REME\\)\\(\\(?:S\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(REFL\\)\\(\\(?:C\\(?:O\\(?:E\\(?:F\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(REDU\\)\\(\\(?:C\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RECT\\)\\(\\(?:N\\(?:G\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(REALV\\)\\(\\(?:A\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(REAL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RDEL\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(RDEC\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(RCON\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(RBE3\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/RAT\\)\\(\\(?:I\\(?:O\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(RATE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RAPP\\)\\(\\(?:N\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(RALL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(RADO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(RACE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(R\\)\\>"  (2  font-lock-type-face
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(QUOT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/QUI\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(QUAD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(QSOP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(QFAC\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(QDVA\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PWE\\)\\(\\(?:D\\(?:G\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PVEC\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PUNI\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PTXY\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PTEM\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PSY\\)\\(\\(?:M\\(?:B\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSTR\\)\\(\\(?:E\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PST\\)\\(\\(?:A\\(?:T\\(?:U\\(?:S\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSPR\\)\\(\\(?:N\\(?:G\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PSP\\)\\(\\(?:E\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSPE\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSOL\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSME\\)\\(\\(?:S\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/PSF\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PSEL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSDW\\)\\(\\(?:A\\(?:V\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSDV\\)\\(\\(?:A\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSDU\\)\\(\\(?:N\\(?:I\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSDS\\)\\(\\(?:P\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSDR\\)\\(\\(?:E\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSDG\\)\\(\\(?:R\\(?:A\\(?:P\\(?:H\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSDF\\)\\(\\(?:R\\(?:Q\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSDC\\)\\(\\(?:O\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PSCR\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PSCO\\)\\(\\(?:N\\(?:T\\(?:R\\(?:O\\(?:L\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRVE\\)\\(\\(?:C\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRVARO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))     ("\\(^\\|\\$\\)\\s-*\\(PRVAR\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRTI\\)\\(\\(?:M\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRSY\\)\\(\\(?:Z\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRSS\\)\\(\\(?:O\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRSE\\)\\(\\(?:C\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRRS\\)\\(\\(?:O\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRRF\\)\\(\\(?:O\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRPA\\)\\(\\(?:T\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PROR\\)\\(\\(?:B\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PROD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRNS\\)\\(\\(?:O\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRNL\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRJS\\)\\(\\(?:O\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRIT\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRIS\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRIN\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PRIM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PRI2\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRHF\\)\\(\\(?:F\\(?:A\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRET\\)\\(\\(?:A\\(?:B\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRES\\)\\(\\(?:O\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRER\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))           ("\\(^\\|\\$\\)\\s-*\\(/PREP7\\)\\>"          (2
								   font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PREN\\)\\(\\(?:E\\(?:R\\(?:G\\(?:Y\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PRED\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PREC\\)\\(\\(?:I\\(?:S\\(?:I\\(?:O\\(?:N\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRCP\\)\\(\\(?:L\\(?:X\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRCO\\)\\(\\(?:N\\(?:V\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRCI\\)\\(\\(?:N\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRCA\\)\\(\\(?:M\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PRAN\\)\\(\\(?:G\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PPRE\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PPRA\\)\\(\\(?:N\\(?:G\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PPLO\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PPAT\\)\\(\\(?:H\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(POWE\\)\\(\\(?:R\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(POUT\\)\\(\\(?:R\\(?:E\\(?:S\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))          ("\\(^\\|\\$\\)\\s-*\\(/POST26\\)\\>"          (2
								   font-lock-type-face
								   keep))           ("\\(^\\|\\$\\)\\s-*\\(/POST1\\)\\>"          (2
								   font-lock-type-face
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(POPT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/POL\\)\\(\\(?:Y\\(?:G\\(?:O\\(?:N\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(POLY\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(POIN\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PNU\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PNGR\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PMO\\)\\(\\(?:R\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PMOP\\)\\(\\(?:T\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PMLS\\)\\(\\(?:I\\(?:Z\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PMLO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PMGT\\)\\(\\(?:R\\(?:A\\(?:N\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PME\\)\\(\\(?:T\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PMET\\)\\(\\(?:H\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PMAP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLWA\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLVF\\)\\(\\(?:R\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLVE\\)\\(\\(?:C\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLVARO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))     ("\\(^\\|\\$\\)\\s-*\\(PLVAR\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLTR\\)\\(\\(?:A\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLTI\\)\\(\\(?:M\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PLTD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLSY\\)\\(\\(?:Z\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLSE\\)\\(\\(?:C\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLSC\\)\\(\\(?:H\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLPAT\\)\\(\\(?:H\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLPAG\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLOTT\\)\\(\\(?:I\\(?:N\\(?:G\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PLOT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLOR\\)\\(\\(?:B\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PLO\\)\\(\\(?:P\\(?:T\\(?:S\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLNS\\)\\(\\(?:O\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PLLS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLHF\\)\\(\\(?:F\\(?:A\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLF2\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLET\\)\\(\\(?:A\\(?:B\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLES\\)\\(\\(?:O\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLDI\\)\\(\\(?:S\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLCR\\)\\(\\(?:A\\(?:C\\(?:K\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLCP\\)\\(\\(?:L\\(?:X\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLCO\\)\\(\\(?:N\\(?:V\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLCI\\)\\(\\(?:N\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PLCA\\)\\(\\(?:M\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PIVC\\)\\(\\(?:H\\(?:E\\(?:C\\(?:K\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PIPE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PINS\\)\\(\\(?:U\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PINC\\)\\(\\(?:L\\(?:U\\(?:D\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PIC\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PHYS\\)\\(\\(?:I\\(?:C\\(?:S\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PGWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PGSE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PGSA\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PGRS\\)\\(\\(?:E\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PGRA\\)\\(\\(?:P\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PGAP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PFLU\\)\\(\\(?:I\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PFAC\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PEXC\\)\\(\\(?:L\\(?:U\\(?:D\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PERI\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PERB\\)\\(\\(?:C\\(?:2\\(?:D\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PEMO\\)\\(\\(?:P\\(?:T\\(?:S\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDVA\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDUS\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDSH\\)\\(\\(?:I\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDSE\\)\\(\\(?:N\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDSC\\)\\(\\(?:A\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDSA\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/PDS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDRO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDRE\\)\\(\\(?:S\\(?:U\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDRA\\)\\(\\(?:G\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDPR\\)\\(\\(?:O\\(?:B\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDPL\\)\\(\\(?:O\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDPI\\)\\(\\(?:N\\(?:V\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PDOT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDME\\)\\(\\(?:T\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDLH\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDIN\\)\\(\\(?:Q\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDHI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDEX\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PDEF\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDDO\\)\\(\\(?:E\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDDM\\)\\(\\(?:C\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDCO\\)\\(\\(?:R\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDCM\\)\\(\\(?:A\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDCL\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDCF\\)\\(\\(?:L\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDCD\\)\\(\\(?:F\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PDAN\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PCRO\\)\\(\\(?:S\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PCOR\\)\\(\\(?:R\\(?:O\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PCO\\)\\(\\(?:P\\(?:Y\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PCON\\)\\(\\(?:V\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PCI\\)\\(\\(?:R\\(?:C\\(?:L\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PCIR\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PCGO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PCAL\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/PBF\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/PBC\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(PATH\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PASA\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PART\\)\\(\\(?:S\\(?:E\\(?:L\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PARE\\)\\(\\(?:S\\(?:U\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PAPU\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PAGE\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PAG\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PADE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OUTR\\)\\(\\(?:E\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/OUT\\)\\(\\(?:P\\(?:U\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OUTP\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OUTO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPVA\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPUS\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPTY\\)\\(\\(?:P\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/OPT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPSW\\)\\(\\(?:E\\(?:E\\(?:P\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPSU\\)\\(\\(?:B\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPSE\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPSA\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPRS\\)\\(\\(?:W\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPRG\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPRF\\)\\(\\(?:A\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPRE\\)\\(\\(?:S\\(?:U\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPRA\\)\\(\\(?:N\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPPR\\)\\(\\(?:N\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPNC\\)\\(\\(?:O\\(?:N\\(?:T\\(?:R\\(?:O\\(?:L\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPMA\\)\\(\\(?:K\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPLS\\)\\(\\(?:W\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPLO\\)\\(\\(?:O\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPLG\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPLF\\)\\(\\(?:A\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPKE\\)\\(\\(?:E\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPGR\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPFR\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPFA\\)\\(\\(?:C\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPEX\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPER\\)\\(\\(?:A\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPEQ\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPDE\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPDA\\)\\(\\(?:T\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPCL\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPAN\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OPAD\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(OMEG\\)\\(\\(?:A\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NWRI\\)\\(\\(?:T\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NWPL\\)\\(\\(?:A\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NWPA\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NUSO\\)\\(\\(?:R\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NUMV\\)\\(\\(?:A\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NUMS\\)\\(\\(?:T\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NUMO\\)\\(\\(?:F\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NUMM\\)\\(\\(?:R\\(?:G\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NUME\\)\\(\\(?:X\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NUMC\\)\\(\\(?:M\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/NUM\\)\\(\\(?:B\\(?:E\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NSYM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NSVR\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NSUB\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NSTO\\)\\(\\(?:R\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NSOR\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NSOL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NSMO\\)\\(\\(?:O\\(?:T\\(?:H\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NSLV\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NSLL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NSLK\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NSLE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NSLA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NSEL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NSCA\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NRRA\\)\\(\\(?:N\\(?:G\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NROT\\)\\(\\(?:A\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NROP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NRLS\\)\\(\\(?:U\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NREF\\)\\(\\(?:I\\(?:N\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NREA\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NPRI\\)\\(\\(?:N\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NPLO\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/NOR\\)\\(\\(?:M\\(?:A\\(?:L\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NORL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NORA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/NOP\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NOOR\\)\\(\\(?:D\\(?:E\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NOOF\\)\\(\\(?:F\\(?:S\\(?:E\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/NOL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/NOE\\)\\(\\(?:R\\(?:A\\(?:S\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NODE\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NOCO\\)\\(\\(?:L\\(?:O\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NMOD\\)\\(\\(?:I\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NLOP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NLOG\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NLIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NLHI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NLGE\\)\\(\\(?:O\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NLDP\\)\\(\\(?:O\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NLDI\\)\\(\\(?:A\\(?:G\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NKPT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NGEN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NFOR\\)\\(\\(?:C\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/NER\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NEQI\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NDSU\\)\\(\\(?:R\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NDIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(NDEL\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NCNV\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(NANG\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(N\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MXPA\\)\\(\\(?:N\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSVA\\)\\(\\(?:R\\(?:Y\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSTE\\)\\(\\(?:R\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/MST\\)\\(\\(?:A\\(?:R\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSSP\\)\\(\\(?:E\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSSO\\)\\(\\(?:L\\(?:U\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSRE\\)\\(\\(?:L\\(?:A\\(?:X\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSQU\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSPR\\)\\(\\(?:O\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSNO\\)\\(\\(?:M\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSMI\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSME\\)\\(\\(?:T\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSMA\\)\\(\\(?:S\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSHP\\)\\(\\(?:A\\(?:T\\(?:T\\(?:E\\(?:R\\(?:N\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSHM\\)\\(\\(?:I\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSHK\\)\\(\\(?:E\\(?:Y\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSHC\\)\\(\\(?:O\\(?:P\\(?:Y\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSHA\\)\\(\\(?:P\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSDA\\)\\(\\(?:T\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSCA\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSAV\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MSAD\\)\\(\\(?:V\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/MRE\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPTR\\)\\(\\(?:E\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPTG\\)\\(\\(?:E\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPTE\\)\\(\\(?:M\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPRI\\)\\(\\(?:N\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPRE\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPPL\\)\\(\\(?:O\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/MPL\\)\\(\\(?:I\\(?:B\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPDR\\)\\(\\(?:E\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPDA\\)\\(\\(?:T\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPCO\\)\\(\\(?:P\\(?:Y\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPCH\\)\\(\\(?:G\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MPAM\\)\\(\\(?:O\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(MP\\)\\>"  (2  font-lock-type-face
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(MOVE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MORP\\)\\(\\(?:H\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(MOPT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MONI\\)\\(\\(?:T\\(?:O\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MODO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MODM\\)\\(\\(?:S\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MODI\\)\\(\\(?:F\\(?:Y\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(MODE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(MMF\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MLIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MITE\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MIDT\\)\\(\\(?:O\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(MGEN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFVO\\)\\(\\(?:L\\(?:U\\(?:M\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFTO\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFTI\\)\\(\\(?:M\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFSU\\)\\(\\(?:R\\(?:F\\(?:A\\(?:C\\(?:E\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFSO\\)\\(\\(?:R\\(?:D\\(?:E\\(?:R\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFRS\\)\\(\\(?:T\\(?:A\\(?:R\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFRE\\)\\(\\(?:L\\(?:A\\(?:X\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFPS\\)\\(\\(?:I\\(?:M\\(?:U\\(?:L\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFOU\\)\\(\\(?:T\\(?:P\\(?:U\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFOR\\)\\(\\(?:D\\(?:E\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFMA\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFLC\\)\\(\\(?:O\\(?:M\\(?:M\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFIT\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFIN\\)\\(\\(?:T\\(?:E\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFIM\\)\\(\\(?:P\\(?:O\\(?:R\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(MFFR\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFFN\\)\\(\\(?:A\\(?:M\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFEX\\)\\(\\(?:T\\(?:E\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(MFEM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFEL\\)\\(\\(?:E\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFDT\\)\\(\\(?:I\\(?:M\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFCO\\)\\(\\(?:N\\(?:V\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFCM\\)\\(\\(?:M\\(?:A\\(?:N\\(?:D\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFCL\\)\\(\\(?:E\\(?:A\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(MFCI\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFCA\\)\\(\\(?:L\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFBU\\)\\(\\(?:C\\(?:K\\(?:E\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MFAN\\)\\(\\(?:A\\(?:L\\(?:Y\\(?:S\\(?:I\\(?:S\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MESH\\)\\(\\(?:I\\(?:N\\(?:G\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/MEN\\)\\(\\(?:U\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(MEMM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MDPL\\)\\(\\(?:O\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MDEL\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MDAM\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MCHE\\)\\(\\(?:C\\(?:K\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MATE\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(MAT\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MAST\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MAPS\\)\\(\\(?:O\\(?:L\\(?:V\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MAGS\\)\\(\\(?:O\\(?:L\\(?:V\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MAGO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(MADA\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep) (4 (quote shadow) keep)) ("\\(^\\|\\$\\)\\s-*\\(M\\)\\>" (2
								   font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LWPL\\)\\(\\(?:A\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LVSC\\)\\(\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LUMP\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LTRA\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LTAN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LSYM\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/LSY\\)\\(\\(?:M\\(?:B\\(?:O\\(?:L\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LSWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LSUM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LSTR\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LSSO\\)\\(\\(?:L\\(?:V\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LSSC\\)\\(\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LSRE\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/LSP\\)\\(\\(?:E\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LSOP\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LSLK\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LSLA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LSEL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LSDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LSCL\\)\\(\\(?:E\\(?:A\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LSBW\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LSBV\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LSBL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LSBA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LROT\\)\\(\\(?:A\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LREV\\)\\(\\(?:E\\(?:R\\(?:S\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LREF\\)\\(\\(?:I\\(?:N\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LPTN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LPLO\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LOVL\\)\\(\\(?:A\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LOCA\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LNSR\\)\\(\\(?:C\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LNSP\\)\\(\\(?:L\\(?:I\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LNME\\)\\(\\(?:R\\(?:G\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LNFI\\)\\(\\(?:L\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LNDE\\)\\(\\(?:T\\(?:A\\(?:C\\(?:H\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LNCO\\)\\(\\(?:L\\(?:L\\(?:A\\(?:P\\(?:S\\(?:E\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LMES\\)\\(\\(?:H\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LMAT\\)\\(\\(?:R\\(?:I\\(?:X\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LLIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*LIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LIST\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LINV\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LINP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LINL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))     ("\\(^\\|\\$\\)\\s-*\\(LINES\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/LIN\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LINE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LINA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/LIG\\)\\(\\(?:H\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LGWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LGLU\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LGEN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LFSU\\)\\(\\(?:R\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LFIL\\)\\(\\(?:L\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LEXT\\)\\(\\(?:N\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LESI\\)\\(\\(?:Z\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LDRE\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LDRA\\)\\(\\(?:G\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LDIV\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LDEL\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCZE\\)\\(\\(?:R\\(?:O\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCSU\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LCSL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCSE\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCOP\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCOM\\)\\(\\(?:B\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCLE\\)\\(\\(?:A\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCFI\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCFA\\)\\(\\(?:C\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCDE\\)\\(\\(?:F\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))     ("\\(^\\|\\$\\)\\s-*\\(LCCAT\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCCAL\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCAS\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LCAB\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LAYP\\)\\(\\(?:L\\(?:O\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LAYL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LAYERP\\)\\(\\(?:2\\(?:6\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))     ("\\(^\\|\\$\\)\\s-*\\(LAYER\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LATT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LARG\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(LARE\\)\\(\\(?:A\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/LAR\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LARC\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(LANG\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(L2TA\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(L2AN\\)\\(\\(?:G\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep) (4 (quote shadow) keep)) ("\\(^\\|\\$\\)\\s-*\\(L\\)\\>" (2
								   font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KWPL\\)\\(\\(?:A\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KWPA\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(KUSE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KTRA\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KSYM\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(KSUM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(KSLN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(KSLL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(KSEL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KSCO\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KSCA\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KREF\\)\\(\\(?:I\\(?:N\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KPSC\\)\\(\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KPLO\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KNOD\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KMOV\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KMOD\\)\\(\\(?:I\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KMES\\)\\(\\(?:H\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KLIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(KL\\)\\>"  (2  font-lock-type-face
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(KGEN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KFIL\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(KEYW\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KEYP\\)\\(\\(?:T\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KEYO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KESI\\)\\(\\(?:Z\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(KEEP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KDIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KDEL\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KCLE\\)\\(\\(?:A\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KCEN\\)\\(\\(?:T\\(?:E\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KCAL\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(KBET\\)\\(\\(?:W\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(KBC\\)\\>"  (2 font-lock-type-face
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(KATT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(K\\)\\>"  (2  font-lock-type-face
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(JSOL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(JPEG\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ISWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ISTR\\)\\(\\(?:E\\(?:S\\(?:S\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ISFI\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(IRLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(IRLF\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(IOPT\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(INTS\\)\\(\\(?:R\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(INT1\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(INRT\\)\\(\\(?:I\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(INRE\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/INP\\)\\(\\(?:U\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(INIS\\)\\(\\(?:T\\(?:A\\(?:T\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(IMPD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(IMME\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(IMES\\)\\(\\(?:H\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(IMAG\\)\\(\\(?:I\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/IMA\\)\\(\\(?:G\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(IGESO\\)\\(\\(?:U\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(IGESI\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ICVF\\)\\(\\(?:R\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/ICS\\)\\(\\(?:C\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/ICL\\)\\(\\(?:W\\(?:I\\(?:D\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ICLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ICEL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ICED\\)\\(\\(?:E\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(ICE\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ICDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(IC\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HROU\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HROP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HREX\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HRCP\\)\\(\\(?:L\\(?:X\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HPTD\\)\\(\\(?:E\\(?:L\\(?:E\\(?:T\\(?:E\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HPTC\\)\\(\\(?:R\\(?:E\\(?:A\\(?:T\\(?:E\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(HPGL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HMAG\\)\\(\\(?:S\\(?:O\\(?:L\\(?:V\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFSY\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFSW\\)\\(\\(?:E\\(?:E\\(?:P\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFSC\\)\\(\\(?:A\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFPOR\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFPOW\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFPC\\)\\(\\(?:S\\(?:W\\(?:P\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(HFPA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFNE\\)\\(\\(?:A\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFMO\\)\\(\\(?:D\\(?:P\\(?:R\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFER\\)\\(\\(?:E\\(?:F\\(?:I\\(?:N\\(?:E\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFEI\\)\\(\\(?:G\\(?:O\\(?:P\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFDE\\)\\(\\(?:E\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFAR\\)\\(\\(?:R\\(?:A\\(?:Y\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFAN\\)\\(\\(?:G\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HFAD\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HEMI\\)\\(\\(?:O\\(?:P\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HELPD\\)\\(\\(?:I\\(?:S\\(?:P\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(HELP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/HEA\\)\\(\\(?:D\\(?:E\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HBMA\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/HBC\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(HARF\\)\\(\\(?:R\\(?:Q\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GTY\\)\\(\\(?:P\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GTH\\)\\(\\(?:K\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(GSUM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/GST\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GSSO\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GSLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GSGD\\)\\(\\(?:A\\(?:T\\(?:A\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GSBD\\)\\(\\(?:A\\(?:T\\(?:A\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GSA\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GRT\\)\\(\\(?:Y\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(GRP\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GRO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GRI\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GRE\\)\\(\\(?:S\\(?:U\\(?:M\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GRA\\)\\(\\(?:P\\(?:H\\(?:I\\(?:C\\(?:S\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GPLO\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GPLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GPDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(GP\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GOP\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GOL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(/GO\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GMFA\\)\\(\\(?:C\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GMAT\\)\\(\\(?:R\\(?:I\\(?:X\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GMA\\)\\(\\(?:R\\(?:K\\(?:E\\(?:R\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GLI\\)\\(\\(?:N\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GFO\\)\\(\\(?:R\\(?:M\\(?:A\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GFI\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GEOME\\)\\(\\(?:T\\(?:R\\(?:Y\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(GEOM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GENO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GCO\\)\\(\\(?:L\\(?:U\\(?:M\\(?:N\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/GCM\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GAUG\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GAPP\\)\\(\\(?:L\\(?:O\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GAPO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GAPM\\)\\(\\(?:E\\(?:R\\(?:G\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GAPL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(GAPFI\\)\\(\\(?:N\\(?:I\\(?:S\\(?:H\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(GAPF\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(GAP\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FVME\\)\\(\\(?:S\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FTWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FTSI\\)\\(\\(?:Z\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FTRA\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FTCA\\)\\(\\(?:L\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(FSUM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FSSP\\)\\(\\(?:A\\(?:R\\(?:M\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FSSE\\)\\(\\(?:C\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FSPL\\)\\(\\(?:O\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FSNO\\)\\(\\(?:D\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FSLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FSDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FSCA\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(FS\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FRQS\\)\\(\\(?:C\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(FREQ\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FPLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(FP\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/FOR\\)\\(\\(?:M\\(?:A\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(FORM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FORC\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FOR2\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/FOC\\)\\(\\(?:U\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FMAGS\\)\\(\\(?:U\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FMAGB\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FLUX\\)\\(\\(?:V\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(FLST\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FLRE\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FLOT\\)\\(\\(?:R\\(?:A\\(?:N\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FLOC\\)\\(\\(?:H\\(?:E\\(?:C\\(?:K\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FLLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FLIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA40\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA39\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA38\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA37\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA36\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA35\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA34\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA33\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA32\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA31\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA30\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA29\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA28\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA27\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA26\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA25\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA24H\\)\\(\\)\\(\\w*\\)"  (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA24G\\)\\(\\)\\(\\w*\\)"  (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA24F\\)\\(\\)\\(\\w*\\)"  (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA24E\\)\\(\\)\\(\\w*\\)"  (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA24D\\)\\(\\)\\(\\w*\\)"  (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA24C\\)\\(\\)\\(\\w*\\)"  (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA24B\\)\\(\\)\\(\\w*\\)"  (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA24A\\)\\(\\)\\(\\w*\\)"  (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA24\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA23\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA22\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA21\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA20B\\)\\(\\)\\(\\w*\\)"  (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA20A\\)\\(\\)\\(\\w*\\)"  (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA20\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA19\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA18\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA17\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA16\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA15\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA14\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA13\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA12\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA11\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA10\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(FLDATA9\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(FLDATA8\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(FLDATA7\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(FLDATA6\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(FLDATA5\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FLDATA4A\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(FLDATA4\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(FLDATA3\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(FLDATA2\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(FLDATA1\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(FLDATA\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FLAN\\)\\(\\(?:G\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(FL\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FKLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FKDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(FK\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FJLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FJDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(FJ\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FITE\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FINI\\)\\(\\(?:S\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/FIL\\)\\(\\(?:N\\(?:A\\(?:M\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FILLD\\)\\(\\(?:A\\(?:T\\(?:A\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(FILL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FILED\\)\\(\\(?:I\\(?:S\\(?:P\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FILEAUX3\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(FILEAUX2\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(FILE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FESU\\)\\(\\(?:R\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FELI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FEFO\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FECO\\)\\(\\(?:N\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FEBO\\)\\(\\(?:D\\(?:Y\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(FE\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/FDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FDEL\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(FCUM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FCLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FCDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FCCH\\)\\(\\(?:E\\(?:C\\(?:K\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(FC\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(FATI\\)\\(\\(?:G\\(?:U\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/FAC\\)\\(\\(?:E\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep) (4 (quote shadow) keep)) ("\\(^\\|\\$\\)\\s-*\\(F\\)\\>" (2
								   font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EXUN\\)\\(\\(?:I\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EXTR\\)\\(\\(?:E\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EXTO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EXPS\\)\\(\\(?:O\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EXPR\\)\\(\\(?:O\\(?:F\\(?:I\\(?:L\\(?:E\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EXPAS\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/EXP\\)\\(\\(?:A\\(?:N\\(?:D\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EXPAN\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(EXP\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/EXI\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EWRI\\)\\(\\(?:T\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EUSO\\)\\(\\(?:R\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ETYP\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ETLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ETDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ETCO\\)\\(\\(?:N\\(?:T\\(?:R\\(?:O\\(?:L\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ETCH\\)\\(\\(?:G\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ETAB\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(ET\\)\\>"  (2  font-lock-type-face
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ESYS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ESYM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ESUR\\)\\(\\(?:F\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ESTI\\)\\(\\(?:F\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ESSO\\)\\(\\(?:L\\(?:V\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ESOR\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ESOL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ESLV\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ESLN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ESLL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ESLA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ESIZ\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/ESH\\)\\(\\(?:A\\(?:P\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ESEL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ESCH\\)\\(\\(?:E\\(?:C\\(?:K\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ERRA\\)\\(\\(?:N\\(?:G\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ERNO\\)\\(\\(?:R\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ERES\\)\\(\\(?:X\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EREI\\)\\(\\(?:N\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EREF\\)\\(\\(?:I\\(?:N\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EREA\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/ERA\\)\\(\\(?:S\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ERAS\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EQSL\\)\\(\\(?:V\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EPLO\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EORI\\)\\(\\(?:E\\(?:N\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/EOF\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ENSY\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ENOR\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ENGE\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ENER\\)\\(\\(?:S\\(?:O\\(?:L\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ENDR\\)\\(\\(?:E\\(?:L\\(?:E\\(?:A\\(?:S\\(?:E\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(EN\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EMUN\\)\\(\\(?:I\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EMTG\\)\\(\\(?:E\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EMSY\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EMOR\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EMOD\\)\\(\\(?:I\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EMIS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EMID\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EMFT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(EMF\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EMAT\\)\\(\\(?:W\\(?:R\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EMAG\\)\\(\\(?:E\\(?:R\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ELIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ELEM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EKIL\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EINT\\)\\(\\(?:F\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EGEN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/EFA\\)\\(\\(?:C\\(?:E\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDWE\\)\\(\\(?:L\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDVE\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDTP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDTE\\)\\(\\(?:R\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDST\\)\\(\\(?:A\\(?:R\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDSP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDSO\\)\\(\\(?:L\\(?:V\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDSH\\)\\(\\(?:E\\(?:L\\(?:L\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDRU\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDRS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDRI\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDRE\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDRD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDRC\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDPV\\)\\(\\(?:E\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDPL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDPC\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDPA\\)\\(\\(?:R\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDOU\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDOP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDNR\\)\\(\\(?:O\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDND\\)\\(\\(?:T\\(?:S\\(?:D\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDNB\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDMP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDLO\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDLC\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDIS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDIP\\)\\(\\(?:A\\(?:R\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDIN\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDHT\\)\\(\\(?:I\\(?:M\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDHI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDHG\\)\\(\\(?:L\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/EDG\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDGC\\)\\(\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDFP\\)\\(\\(?:L\\(?:O\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDEN\\)\\(\\(?:E\\(?:R\\(?:G\\(?:Y\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDEL\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDDU\\)\\(\\(?:M\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDDR\\)\\(\\(?:E\\(?:L\\(?:A\\(?:X\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDDC\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDDB\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDDA\\)\\(\\(?:M\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDCU\\)\\(\\(?:R\\(?:V\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDCT\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDCS\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDCR\\)\\(\\(?:B\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDCP\\)\\(\\(?:U\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDCO\\)\\(\\(?:N\\(?:T\\(?:A\\(?:C\\(?:T\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDCN\\)\\(\\(?:S\\(?:T\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDCM\\)\\(\\(?:O\\(?:R\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDCL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDCG\\)\\(\\(?:E\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDCA\\)\\(\\(?:D\\(?:A\\(?:P\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDBV\\)\\(\\(?:I\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(EDBX\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDBO\\)\\(\\(?:U\\(?:N\\(?:D\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDAS\\)\\(\\(?:M\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDAL\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EDAD\\)\\(\\(?:A\\(?:P\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(EALI\\)\\(\\(?:V\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep) (4 (quote shadow) keep)) ("\\(^\\|\\$\\)\\s-*\\(E\\)\\>" (2
								   font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DYNO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DVMO\\)\\(\\(?:R\\(?:P\\(?:H\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/DV3\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(DUMP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DTRA\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(DSYS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(DSYM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DSUR\\)\\(\\(?:F\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(DSUM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DSPO\\)\\(\\(?:P\\(?:T\\(?:I\\(?:O\\(?:N\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(DSET\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/DSC\\)\\(\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DSCA\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DOME\\)\\(\\(?:G\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DOFS\\)\\(\\(?:E\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(DOF\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DNSO\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DMPR\\)\\(\\(?:A\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DMPE\\)\\(\\(?:X\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DMOV\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DLLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DLIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DLDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(DL\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DKLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DKDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(DK\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DJLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DJDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(DJ\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/DIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DISP\\)\\(\\(?:L\\(?:A\\(?:Y\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DIGI\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(DIG\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/DEVI\\)\\(\\(?:C\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/DEVD\\)\\(\\(?:I\\(?:S\\(?:P\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DETA\\)\\(\\(?:B\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DESO\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DESI\\)\\(\\(?:Z\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DERI\\)\\(\\(?:V\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DEMO\\)\\(\\(?:R\\(?:P\\(?:H\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DELT\\)\\(\\(?:I\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/DEL\\)\\(\\(?:E\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DELE\\)\\(\\(?:T\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DEFI\\)\\(\\(?:N\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DECO\\)\\(\\(?:M\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DEAC\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DDEL\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DCVS\\)\\(\\(?:W\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(DCUM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DCGO\\)\\(\\(?:M\\(?:G\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DATAD\\)\\(\\(?:E\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(DATA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DAMO\\)\\(\\(?:R\\(?:P\\(?:H\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DALI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(DADE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(DA\\)\\>"  (2  font-lock-type-face
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(D\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CZME\\)\\(\\(?:S\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CZDE\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CYLI\\)\\(\\(?:N\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(CYL5\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(CYL4\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CYCP\\)\\(\\(?:H\\(?:A\\(?:S\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CYCO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CYCL\\)\\(\\(?:I\\(?:C\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/CYC\\)\\(\\(?:E\\(?:X\\(?:P\\(?:A\\(?:N\\(?:D\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/CWD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(CVAR\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/CVA\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CUTC\\)\\(\\(?:O\\(?:N\\(?:T\\(?:R\\(?:O\\(?:L\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CURR\\)\\(\\(?:2\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/CTY\\)\\(\\(?:P\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(CSYS\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CSWP\\)\\(\\(?:L\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CSLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(CSKP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CSDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CSCI\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(CS\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CRPL\\)\\(\\(?:I\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(CQC\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CPSG\\)\\(\\(?:E\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CPNG\\)\\(\\(?:E\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CPME\\)\\(\\(?:R\\(?:G\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CPLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CPLG\\)\\(\\(?:E\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/CPL\\)\\(\\(?:A\\(?:N\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CPIN\\)\\(\\(?:T\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CPDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CPCY\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(CP\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(COVA\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(COUP\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CORI\\)\\(\\(?:O\\(?:L\\(?:I\\(?:S\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/COP\\)\\(\\(?:Y\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/CONT\\)\\(\\(?:O\\(?:U\\(?:R\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CONJ\\)\\(\\(?:U\\(?:G\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/CONF\\)\\(\\(?:I\\(?:G\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(CONE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(CON4\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(COMP\\)\\(\\(?:R\\(?:E\\(?:S\\(?:S\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/COM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/COL\\)\\(\\(?:O\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CNVT\\)\\(\\(?:O\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CNCH\\)\\(\\(?:E\\(?:C\\(?:K\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMSO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMSF\\)\\(\\(?:I\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMSE\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMRO\\)\\(\\(?:T\\(?:A\\(?:T\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMPL\\)\\(\\(?:O\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMOM\\)\\(\\(?:E\\(?:G\\(?:A\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMMO\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMGR\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMED\\)\\(\\(?:I\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMDO\\)\\(\\(?:M\\(?:E\\(?:G\\(?:A\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMAT\\)\\(\\(?:R\\(?:I\\(?:X\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/CMA\\)\\(\\(?:P\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CMAC\\)\\(\\(?:E\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(CM\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CLRM\\)\\(\\(?:S\\(?:H\\(?:L\\(?:N\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/CLO\\)\\(\\(?:G\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(CLOG\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CLOC\\)\\(\\(?:A\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/CLE\\)\\(\\(?:A\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/CLA\\)\\(\\(?:B\\(?:E\\(?:L\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CISO\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CIRC\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(CINT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CHKM\\)\\(\\(?:S\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CHEC\\)\\(\\(?:K\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CGOM\\)\\(\\(?:G\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CGLO\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/CFO\\)\\(\\(?:R\\(?:M\\(?:A\\(?:T\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CFAC\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CESG\\)\\(\\(?:E\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CERI\\)\\(\\(?:G\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(CEQN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CENT\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CELI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CEIN\\)\\(\\(?:T\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CEDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CECY\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CECM\\)\\(\\(?:O\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CECH\\)\\(\\(?:E\\(?:C\\(?:K\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(CE\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CDWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CDRE\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CDOP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CBDO\\)\\(\\(?:F\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(CAMP\\)\\(\\(?:B\\(?:E\\(?:L\\(?:L\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(CALC\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BUCO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BTOL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BSTQ\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BSTE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BSS2\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BSS1\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BSPL\\)\\(\\(?:I\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BSM2\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BSM1\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BSMD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BSAX\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BRAN\\)\\(\\(?:C\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BOPT\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BOOL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BLOC\\)\\(\\(?:K\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BLC5\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BLC4\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BIOT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BIOO\\)\\(\\(?:P\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFVL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFVD\\)\\(\\(?:E\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(BFV\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFUN\\)\\(\\(?:I\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFTR\\)\\(\\(?:A\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFSC\\)\\(\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFLL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFLD\\)\\(\\(?:E\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(BFL\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFKL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFKD\\)\\(\\(?:E\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(BFK\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFIN\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFES\\)\\(\\(?:C\\(?:A\\(?:L\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFEL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFED\\)\\(\\(?:E\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFEC\\)\\(\\(?:U\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(BFE\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFDE\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFCU\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFAL\\)\\(\\(?:I\\(?:S\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BFAD\\)\\(\\(?:E\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(BFA\\)\\>"  (2 font-lock-type-face
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(BF\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BETA\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(BEND\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BELL\\)\\(\\(?:O\\(?:W\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(BCSO\\)\\(\\(?:P\\(?:T\\(?:I\\(?:O\\(?:N\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/BAT\\)\\(\\(?:C\\(?:H\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/AXL\\)\\(\\(?:A\\(?:B\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AVRE\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AVPR\\)\\(\\(?:I\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(/AUX15\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(/AUX12\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))     ("\\(^\\|\\$\\)\\s-*\\(/AUX3\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))     ("\\(^\\|\\$\\)\\s-*\\(/AUX2\\)\\(\\)\\(\\w*\\)"    (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AUTO\\)\\(\\(?:T\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/AUT\\)\\(\\(?:O\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ATYP\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ATRA\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ATAN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ASUM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ASUB\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/ASS\\)\\(\\(?:I\\(?:G\\(?:N\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ASLV\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ASLL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ASKI\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ASEL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ASBW\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ASBV\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ASBL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ASBA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ARSY\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ARSP\\)\\(\\(?:L\\(?:I\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ARSC\\)\\(\\(?:A\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AROT\\)\\(\\(?:A\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ARME\\)\\(\\(?:R\\(?:G\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ARFI\\)\\(\\(?:L\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AREV\\)\\(\\(?:E\\(?:R\\(?:S\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AREM\\)\\(\\(?:E\\(?:S\\(?:H\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AREF\\)\\(\\(?:I\\(?:N\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AREA\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ARDE\\)\\(\\(?:T\\(?:A\\(?:C\\(?:H\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ARCT\\)\\(\\(?:R\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ARCO\\)\\(\\(?:L\\(?:L\\(?:A\\(?:P\\(?:S\\(?:E\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ARCL\\)\\(\\(?:E\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(APTN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(APPE\\)\\(\\(?:N\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(APLO\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AOVL\\)\\(\\(?:A\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AOFF\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/ANU\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANTY\\)\\(\\(?:P\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANTI\\)\\(\\(?:M\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANSTOAS\\)\\(\\(?:A\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANSTOAQ\\)\\(\\(?:W\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANSO\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANOR\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/ANN\\)\\(\\(?:O\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANMR\\)\\(\\(?:E\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANMO\\)\\(\\(?:D\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANIS\\)\\(\\(?:O\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ANIM\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANHA\\)\\(\\(?:R\\(?:M\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/ANG\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANFL\\)\\(\\(?:O\\(?:W\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/ANF\\)\\(\\(?:I\\(?:L\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANDY\\)\\(\\(?:N\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANDS\\)\\(\\(?:C\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANDA\\)\\(\\(?:T\\(?:A\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANCY\\)\\(\\(?:C\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANCU\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ANCN\\)\\(\\(?:T\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/AN3\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AMES\\)\\(\\(?:H\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(AMAP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ALPH\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ALPF\\)\\(\\(?:I\\(?:L\\(?:L\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ALLS\\)\\(\\(?:E\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ALIS\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(AL\\)\\>"  (2  font-lock-type-face
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(AINV\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(AINP\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(AINA\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AGLU\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(AGEN\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AFSU\\)\\(\\(?:R\\(?:F\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AFLI\\)\\(\\(?:S\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AFIL\\)\\(\\(?:L\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(AESI\\)\\(\\(?:Z\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ADRA\\)\\(\\(?:G\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ADGL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ADEL\\)\\(\\(?:E\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ADDA\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(ADD\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ADAP\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ADAM\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ACLE\\)\\(\\(?:A\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(ACEL\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ACCA\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))  ("\\(^\\|\\$\\)\\s-*\\(ABS\\)\\>"  (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ABEX\\)\\(\\(?:T\\(?:R\\(?:A\\(?:C\\(?:T\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(AATT\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(AADD\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep))   ("\\(^\\|\\$\\)\\s-*\\(A\\)\\>"  (2  font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(~UGI\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(~SAT\\)\\(\\(?:I\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(~PRO\\)\\(\\(?:E\\(?:I\\(?:N\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(~PAR\\)\\(\\(?:A\\(?:I\\(?:N\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(~CATI\\)\\(\\(?:A\\(?:I\\(?:N\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(~CAT5\\)\\(\\(?:I\\(?:N\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/WAI\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VST\\)\\(\\(?:A\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VSC\\)\\(\\(?:F\\(?:U\\(?:N\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VRE\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VPU\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VPL\\)\\(\\(?:O\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VOP\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VMA\\)\\(\\(?:S\\(?:K\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VLE\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VIT\\)\\(\\(?:R\\(?:P\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VGE\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VFU\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VFI\\)\\(\\(?:L\\(?:L\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VFA\\)\\(\\(?:C\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VED\\)\\(\\(?:I\\(?:T\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VCU\\)\\(\\(?:M\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VCO\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*VAB\\)\\(\\(?:S\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))       ("\\(^\\|\\$\\)\\s-*\\(\\*USE\\)\\(\\w*\\)"       (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*ULI\\)\\(\\(?:B\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/UCM\\)\\(\\(?:D\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*TRE\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*TOP\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))        ("\\(^\\|\\$\\)\\s-*\\(/TEE\\)\\(\\w*\\)"        (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*TAX\\)\\(\\(?:I\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*STA\\)\\(\\(?:T\\(?:U\\(?:S\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*SRE\\)\\(\\(?:A\\(?:D\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))       ("\\(^\\|\\$\\)\\s-*\\(\\*SET\\)\\(\\w*\\)"       (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/RMD\\)\\(\\(?:I\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*RET\\)\\(\\(?:U\\(?:R\\(?:N\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*REP\\)\\(\\(?:E\\(?:A\\(?:T\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PSE\\)\\(\\(?:A\\(?:R\\(?:C\\(?:H\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/PMA\\)\\(\\(?:C\\(?:R\\(?:O\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PARS\\)\\(\\(?:A\\(?:V\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(PARR\\)\\(\\(?:E\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*MWR\\)\\(\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))       ("\\(^\\|\\$\\)\\s-*\\(\\*MSG\\)\\(\\w*\\)"       (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*MOP\\)\\(\\(?:E\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/MKD\\)\\(\\(?:I\\(?:R\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*MFU\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*MFO\\)\\(\\(?:U\\(?:R\\(?:I\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/MAI\\)\\(\\(?:L\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/INQ\\)\\(\\(?:U\\(?:I\\(?:R\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*IF\\)\\>" (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*GO\\)\\>" (2 font-lock-type-face
								   keep))       ("\\(^\\|\\$\\)\\s-*\\(\\*GET\\)\\(\\w*\\)"       (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*EXI\\)\\(\\(?:T\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))          ("\\(^\\|\\$\\)\\s-*\\(\\*ENDIF\\)\\>"         (2
								   font-lock-type-face
								   keep))          ("\\(^\\|\\$\\)\\s-*\\(\\*ENDDO\\)\\>"         (2
								   font-lock-type-face
								   keep))       ("\\(^\\|\\$\\)\\s-*\\(\\*END\\)\\>"       (2
								   font-lock-type-face       keep)      )         ("\\(^\\|\\$\\)\\s-*\\(\\*ELSEIF\\)\\>"         (2
								   font-lock-type-face
								   keep))    ("\\(^\\|\\$\\)\\s-*\\(\\*ELSE\\)\\(\\)\\(\\w*\\)"   (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*DOW\\)\\(\\(?:H\\(?:I\\(?:L\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*DO\\)\\>" (2 font-lock-type-face
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/DIR\\)\\(\\(?:E\\(?:C\\(?:T\\(?:O\\(?:R\\(?:Y\\)?\\)?\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))       ("\\(^\\|\\$\\)\\s-*\\(\\*DIM\\)\\(\\w*\\)"       (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(/DFL\\)\\(\\(?:A\\(?:B\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))       ("\\(^\\|\\$\\)\\s-*\\(\\*DEL\\)\\(\\w*\\)"       (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*CYC\\)\\(\\(?:L\\(?:E\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*CRE\\)\\(\\(?:A\\(?:T\\(?:E\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*CFW\\)\\(\\(?:R\\(?:I\\(?:T\\(?:E\\)?\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*CFO\\)\\(\\(?:P\\(?:E\\(?:N\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*CFC\\)\\(\\(?:L\\(?:O\\(?:S\\)?\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep))       ("\\(^\\|\\$\\)\\s-*\\(\\*ASK\\)\\(\\w*\\)"       (2
								   font-lock-type-face       keep)      (3       (quote      shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*AFU\\)\\(\\(?:N\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ABBS\\)\\(\\(?:A\\(?:V\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(ABBR\\)\\(\\(?:E\\(?:S\\)?\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep)               (4               (quote               shadow)
								   keep)) ("\\(^\\|\\$\\)\\s-*\\(\\*ABB\\)\\(\\(?:R\\)?\\)\\(\\w*\\)" (2
								   font-lock-type-face      keep)     (3     font-lock-constant-face
								   keep) (4 (quote shadow) keep)))
  "Ansys keywords."
  )

(defconst ansys-font-lock-keywords	;NEW_C
  (append
   ansys-get-functions
   ansys-parametric-functions
   ansys-commands			;command overwrite variables
;   '(ansys-highlight) ;function searches user variables ; TODO BUG
   ansys-undocumented-commands
   ansys-elements
   '(("\\(a\\)=" 1 nil t))
   '(("^\\s-*\\(\\*[mM][sS][gG]\\|\\*[vV][rR][eE]\\|\\*[vV][wW][rR]\\|\\*[mM][wW][rR]\\).*\n\\(\\(.*&\\s-*\n\\)*.*\\)" ;format constructs
      2 'font-lock-doc-face prepend))
   '(("\\(&\\)\\s-*$" 1 'font-lock-comment-face prepend)) ;format continuation char
   '(("\\(%\\)" 1 'font-lock-comment-face prepend))
					;single % acts as a format specifier and pair %.% is an
					;ansys parameter substitution
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
   '(("^[^ \t_]*\\(\\<\\w\\{33,\\}\\>\\)\\s-*=" 1 'font-lock-warning-face t))
					; more than 32 character long variables are not allowed
   '(("\\(\\$\\)" 1 'font-lock-warning-face keep)) ;condensed line continuation char
   '(("\\(:\\)" 1 'font-lock-warning-face keep))   ;colon loops
   '(("^\\s-*\\(:\\w\\{1,7\\}\\)" 1 'font-lock-warning-face t)) ;GOTO Labels, branching
   '(("\\(\\<_\\w+\\>\\)" 1 'font-lock-warning-face prepend)) ;reserved words
   )
  "Regexp for the highlighting."  )

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

;; needed for insert-pair in below key map
(add-to-list 'insert-pair-alist '(?% ?%))

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
;      (define-key map "\C-c\C-t" 'ansys-if-then)
    (define-key map "\C-c\C-t" 'ansys-exit-ansys)
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
         (setq ansys-help-file \"C:\\\\Program Files\\\\Ansys Inc\\\\v110\\\\CommonFiles\\\\HELP\\\\en-us\\\\ansyshelp.chm\"))
        (t
         (setq ansys-lmutil-program \"/ansys_inc/shared_files/licensing/linop64/lmutil\")
         (setq ansys-help-file \"/ansys_inc/v110/ansys/bin/anshelp110\")))

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

  (add-to-list 'kill-buffer-query-functions 'ansys-kill-buffer-query-function)

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

  ;;   (make-local-variable 'ansys-column-ruler-wide) ;FIXEME
  ;;   (make-local-variable 'ansys-column-ruler-narrow)
  ;;   (make-local-variable 'ansys-ruler-wide-flag)
  ;;   (setq wide-ansys-ruler-mode nil)
  ;;	"set to  nil for narrow, t for wide."

  (make-local-variable 'ansys-format)
  (setq ansys-format (intern "mac"))	;FIXME: this is for the ansys-macro
					;? why intern?
  ;; menu
  (or (string= system-type "darwin")
      (string= system-type "macos")
      (string= system-type "ms-dos")
      (string= system-type "windows-nt")
      (string= system-type "cygwin")
      (string= system-type "vax-vms")
      (string= system-type "axp-vms")
      (setq ansys-is-unix-system-flag t))
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
	    (delete-blank-lines)
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
	(unless (ansys-in-indentation-p)
	  (ansys-command-start))
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
  (interactive "*")
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
	["Comment/Un~ Region"           comment-dwim]
	["Insert Pi"                    ansys-insert-pi]
	["Insert Parentheses"           insert-parentheses]
	["Complete Expression"          ansys-complete-symbol]
	["Close Block"                  ansys-close-block]
	"-"
	(list "Insert Template"
	      ["*IF ,Action *ENDIF"    ansys-if]
	      ["*IF ,THEN *ENDIF"	ansys-if-then]
	      ["*DO *ENDDO"	        ansys-do]
	      [" MP "	                ansys-mp]
	      ["Header"                 ansys-skeleton-header]
	      ["Configuration"          ansys-skeleton-configuration]
	      ["View Settings"          ansys-skeleton-view-settings]
	      ["Coordinate Sys. Display"ansys-skeleton-display-coord]
	      ["Working Plane Operations"ansys-skeleton-working-plane]
	      ["Multiplot Commands"     ansys-skeleton-multi-plot]
	      ["Numbering Controls"     ansys-skeleton-numbering-controls]
	      ["Geometry Import"        ansys-skeleton-import]
	      ["Symmetry Expansions"    ansys-skeleton-expand]
	      ["Element Definitions"    ansys-skeleton-element-def]
	      ["Material Definitions"   ansys-skeleton-material-def]
	      ["Meshing Controls"       ansys-skeleton-meshing]
	      ["Contact Pair Definition"           ansys-skeleton-contact-definition]
	      ["Rigid Target"           ansys-skeleton-rigid-target]
	      ["Boundary Conditions"    ansys-skeleton-bc]
	      ["Buckling Analysis Type" ansys-skeleton-buckling]
	      ["Solve"                  ansys-skeleton-solve]
	      ["Post1 Postprocessing"   ansys-skeleton-post1]
	      ["Path plot operations"   ansys-skeleton-path-plot]
	      ["Output to file"         ansys-skeleton-file-output]
	      ["Post26 Postprocessing"  ansys-skeleton-post26]
	      ["Element Table Operations"ansys-skeleton-element-table]
	      ["Big Macro Skeleton" ansys-skeleton])
	(list "Navigate Code Lines"
	      ["Previous Code Line"	ansys-previous-code-line]
	      ["Next Code Line"		ansys-next-code-line]
	      ["Beginning of (Continuation) Command" ansys-command-start]
	      ["End of (Continuation) Command"	ansys-command-end]
	      "-"
	      ["Split Format Line at Point"	ansys-indent-format-line]
	      )
	(list "Work with Logical Blocks"
	      ["Next Block End"		ansys-next-block-end]
	      ["Previous Block Start"   ansys-previous-block-start-and-conditional]
	      ["Down Block"		ansys-down-block]
	      ["Up Block"		ansys-up-block]
	      ["Skip Block Forward"     ansys-skip-block-forward]
	      ["Skip Block Backwards"   ansys-skip-block-backwards]
	      ["Beginning of N. Block" ansys-number-block-start]
	      ["End of Number Block"    ansys-number-block-end]
	      "-"
	      ["Close Block"                  ansys-close-block]
	      ["Mark Block"              ansys-mark-block]
	      )
	(list "Manage Ansys Processes"
	      ["Specify License Server or - File"   ansys-license-file]
	      ["Specify License Utility" ansys-lmutil-program]
	      ["Display License Status" ansys-license-status]
	      ["Start Ansys Help System" ansys-start-ansys-help]
	      "-"
	      ["Specify Ansys License Type" ansys-license :active ansys-is-unix-system-flag]
	      ["Specify Job Name of Run" ansys-job :active ansys-is-unix-system-flag]
	      ["Specify Ansys Executable " ansys-program :active ansys-is-unix-system-flag]
	      ["Start Ansys Run" ansys-start-ansys :active ansys-is-unix-system-flag]
	      ["Display Run Status" ansys-process-status :active ansys-is-unix-system-flag]
	      "-"
	      ["Send Ansys Command Interactively" ansys-query-ansys-command :active ansys-is-unix-system-flag]
	      ["Send Code Line/Region to Ansys" ansys-send-to-ansys :active ansys-is-unix-system-flag]
	      ["Copy/Send above Code (to Ansys)" ansys-copy-or-send-above]
	      ["Start Graphics Screen" ansys-start-graphics :active ansys-is-unix-system-flag]
	      ["Start Pan/Zoom/Rot. Dialog" ansys-start-pzr-box :active ansys-is-unix-system-flag]
	      "-"
	      ["Display Running Processes" list-processes]
	      ["Display Error File" ansys-display-error-file]
	      ["Write Ansys Stop File" ansys-abort-file]
	      ["Exit Ansys Run" ansys-exit-ansys :active ansys-is-unix-system-flag]
	      "-"
	      ["Kill Ansys Run" ansys-kill-ansys :active ansys-is-unix-system-flag]
	      )
	"-"
	["Start Ansys help system" ansys-start-ansys-help]
	["Display License Status" ansys-license-status]
	["Display Command Help"      ansys-show-command-parameters]
	["Display User Variables" ansys-display-variables]
	["Insert Temporary Ruler"         ansys-column-ruler]
	["Show Ansys Mode version"  ansys-mode-version]
	["Describe Ansys Mode"		describe-mode]
	["Customise Ansys Mode"         (customize-group "Ansys")]
	["Submit Bug Report"            ansys-submit-bug-report]
	"-"
	["Return to previous mode"             ansys-toggle-mode])
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
	  (move-to-column (+ icol relpos))))))

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
Skips past all empty - and comment lines."
  (interactive "p")
  (unless arg (setq arg 1))
  (unless (memq last-command '(next-line
			       previous-line
			       ansys-next-code-line
			       ansys-previous-code-line))
    (setq temporary-goal-column (current-column)))
  (forward-line 1)
  (forward-comment (buffer-size))
  (move-to-column temporary-goal-column)
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
      (move-to-column temporary-goal-column)
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

;;; --- Macros and skeletons ---FIXME: redundant macros

(define-skeleton ansys_do		;NEW
  ""
  "Which *do counter [i]: "
  > "*do," str | "i" ",1," (skeleton-read "End condition: " "10") ",1\n"
  > _ \n
  "*cycle !bypass below commands in *do loop" > \n
  "*enddo" > \n
  )

(define-skeleton ansys_if		;NEW
  ""
  "First operant [i]: "
  "*if," str | "i" ","
  (skeleton-read "Operator (EQ,NE,GT,LE,GE,ABLT,ABGT): " "LT") ","
  (skeleton-read "Second operant: " "j") ","
  (skeleton-read "Action (:label,STOP,EXIT,CYCLE): " "THEN") > \n
  > _ \n
  "!! *elseif,i,GT,j" > \n
  "!! *else" > \n
  "*endif" >
  )

(define-skeleton ansys-skeleton-header	 ;NEW
  "Insert header for an APDL script" nil ;;"Name of file: "
;  "! 	$Id" ":$\n"
  "!"(insert (make-string 80 ?*))"\n"
  "!*"(insert (make-string (- 80 2) ? ))"*\n"
  "C*** " (buffer-name) (insert (make-string (- 80 5 (length
						      (buffer-name))) ? ))"*\n"
  "!*"(insert (make-string (- 80 2) ? ))"*\n"
  "!*"(insert (make-string (- 80 2) ? ))"*\n"
  "!*"(insert (make-string (- 80 2) ? ))"*\n"
  "!*   Called by: " _
  (let ((called (read-file-name "Calling Script File: " "")))
    (insert called)
    (insert (make-string (- 80 2 (length "   Called by: ") (length
							    called)) ? ))) "*\n"
  "!*"(insert (make-string (- 80 2) ? ))"*\n"
  "!*   Calling:"(insert (make-string (- 80 2 (length "   Calling:")) ?
				      ))"*\n"
  "!*"(insert (make-string (- 80 2) ? ))"*\n"
  "!*   Macros: "
  (let ((mlib (read-file-name "Macro Library: " "")))
    (insert mlib " ()")
    (insert (make-string (- 80 2 (length "   Macros: ") (length mlib) 3)
			 ? )))"*\n"
  "!*"(insert (make-string (- 80 2) ? ))"*\n"
  "!*"(insert (make-string (- 80 2) ? ))"*\n"
  "!"(insert (make-string 80 ?*))"\n")

(define-skeleton ansys-skeleton-configuration
  ""
  nil
  "!! --- configurations ---" \n
  "! *afun,deg ! trig. functions accept angle arguments" \n
  "*afun,rad" \n
  "True = 1"  \n
  "False = 0" \n
  \n
  "/title," _ \n
  "/plopts,wp,1 !display working plane" \n
  "/triad,rbot" \n
  \n)

(define-skeleton ansys-skeleton-view-settings
  ""
  nil
  "!! --- view settings ---" \n
  "!/view or /vup !viewing direction"_ \n
  "!/angle,1,10,xs,1!rotation {x,y,z}m global {x,y,z}s screen 1:cumulative 0: absolut" \n
  "!/dist !magnification" \n
  "!/focus !focus point" \n
  "!/zoom,1,off !off:refit" \n
  "!/auto !?" \n
  "!/user,all !automatic fit mode"
  \n)

(define-skeleton ansys-skeleton-import	;NEW
  "Import commands."
  nil
  "!! ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string " --- Cad Import --- " \n
  "!! ------------------------------" \n
  \n
  "/aux15" \n
  "ioptn,iges,nodefeat" \n
  "ioptn,merge,yes" \n
  "ioptn,solid,yes" \n
  "ioptn,small,yes" \n
  "ioptn,gtoler,defa" \n
  "igesin,'test','iges'"\n
  \n
  "/input,filename,anf ! for APDL based input" \n
  "/facet,norm" \n
  \n)

(define-skeleton ansys-skeleton-expand	;NEW
  "Symmetry expansion."
  nil
  "/expand,8,lpolar,half,,45 !local polar symmetry expansion" \n
  "/expand,18,axis,,,10 !axissymmetric expansion" \n
  "!! /expand !switch off expansion" \n
  \n)

(define-skeleton ansys-skeleton-contact-definition
  ""
  nil
  "!! --- Contact pair defintion ---" \n
  "Contact="_ \n
  "Target=Contact+1" \n
  "Mu = 0 !contact friction" \n
  "Fkn = .1 !contact stiffness (default 1, divided by 100 if plastic mat. < AnsysWB12)" \n
  "Ftoln = .1 !penetration tolerance [.1] for lagr. mult." \n
  "Icont = 0. !contact closure band size" \n
  "Cnof = 0 !contact offset (neg.: penetr.)" \n
  "Pinb = -1 !search radius, neg: absolut value (at least 1.1*CNOF)" \n
  "r,Contact" \n
  "et,Contact,conta174    !3d, 8 node" \n
  "!! et,Contact,conta173 !3d, 4 node" \n
  "!! et,Contact,conta172 !2d, 3 node" \n
  "!! et,Contact,conta171 !2d, 2 node" \n
  "!! et,Contact,conta175 !2/3d node to surf" \n
  "!! et,Contact,conta176 !3d line to line, 3 node" \n
  "!! et,Contact,conta177 !3d line to surf, 3 node" \n
  \n
  "et,Target,targe170 !3d area,line,(pilot-)node" \n
  "!! et,Target,targe169  !2d" \n
  \n
  "!! --- Contact Options --"\n
  "keyo,Contact,2,1 !ALGORITHM 0:augm. Lagrange,1:penalty,2:MPC,4:pure Lagrange" \n
  "!keyo,Contact,5,0 !AUTOMATED adjustment cnof (surface offset)/icont (node movement in a band),1:auto CNOF gap 2: pene CNOF 3: gap/pene. CNOF, 4: ICONT \"contact band\"" \n
  "keyo,Contact,9,4 !pene./gap,0:include,1:remove,2:include ramped, 3: remove gap/penetr., include offset, 4:remove initial gaps/penetr, incl. offeset ramped" \n
  "keyo,Contact,10,2 !Stiffness UPDATE,[0]:each LS,2:each NR iteration,1:each substep" \n
  "!keyo,Contact,11,1 !Shell thickness effect" \n
  "keyo,Contact,12,0 !BEHAVIOUR,[0]:frictional/-less,1:rough,2:no separation,3:bonded" \n
  "real,Contact" \n
  \n
  "!! --- Contact values ---"\n
  "rmod,Contact,3,Fkn !FKN:normal penalty stiffness factor (default:1) smaller: bigger penetration, easier convergence" \n
  "rmod,Contact,4,Ftoln !FTOLN penetration tolerance (augm. Lagrance! default:0.1) bigger: less chattering"
  "rmod,Contact,5,Icont !ICONT:amount of initial contact closure (positiv:penetration)" \n
  "!rmod,Contact,6,Pinb !PINB:pinball radius (negative: no scaling:absolute distance)" \n
  "!rmod,Contact,10,Cnof !CNOF (thickness effects):contact normal offset (e.g. beams)" \n
  "!rmod,Contact,11,-1 !FKOP contact damping must be neg." \n
  "!rmod,Contact,12,0. ! FKT:tangent stiffness factor,0:means 1 for Ansys!!!" \n
  "mp,mu,Contact,Mu !friction factor" \n
  "mat,Contact" \n
  \n
  "!! -- Contact generation --"
  "!! type,Contact" \n
  "!! real,Contact" \n
  "!! esurf !,,top ![default] beam element's top direction" \n
  "!! esurf !,,bottom ! for beam elements top direction" \n
  "!! esurf !,,reverse ! reverse dir. on existing elem." \n
  "!! enorm ! change the underlying elem."

  "!! -- check contact status --" \n
  "!save"\n
  "!cncheck,adjust !adjust the elements!"
  "!resume,file.db" \n
  "!/solu" \n
  "!cncheck,post" \n
  "!/post1" \n
  "!file,file,rcn" \n
  "!set,first" \n
  "!plnsol,cont,gap,0,1" \n
  "!esel,s,type,,Contact" \n
  "!etable,St,cont,stat	 !3-closed sticking" \n
  "			 !2-closed sliding" \n
  "			 !1-open near" \n
  "			 !0-open far" \n
  "!/efacet,2" \n
  "!plls,Pene,Pene !line element results" \n
  "!plls,st,st" \n
  \n)

(define-skeleton ansys-skeleton-rigid-target ;NEW
  ""
  nil
  "!! --- Rigid Target creation --- " \n
  "Contact="_ \n
  "Target=Contact+1" \n
  "real,Contact" \n
  "type,Target" \n
  "!!tshap,arc !clockwise arc" \n
  "!!tshap,cone" \n
  "!!tshap,quad" \n
  "!!tshap,sphere" \n
  "!!tshap,qua8" \n
  "tshap,line" \n
  "*get,Nmax,node,,num,max" \n
  "n,Nmax+1,1,1,0" \n
  " ,Nmax+2,1,2,0" \n
  "e,Nmax+1,Nmax+2" \n
  "tshap,pilo" \n
  "e,Nmax+1" \n
  \n)

(define-skeleton ansys-skeleton-display-coord
  ""
  nil
  "/plopts,wp,1 !display working plane" \n
  "/triad,rbot"_ \n
  \n)

(define-skeleton ansys-skeleton-working-plane
  ""
  nil
  "!wpcsys,1,0 !align wp in WIN with specified c-sys" \n
  "!wpoffs,,-100 !x,y,z offset" \n
  "!wprota,0,90,0 !z,x,y axis of rotation" \n
  "!wpstyl,,,,,,1 !type spec" \n
  "!csys,wp !change co to wp" \n
  \n)

;; PlotCtrls ->Multi-plot-Ctrls???
(define-skeleton ansys-skeleton-multi-plot
  ""
  nil
  "/gtype,all,node,0 !turn off nodes" \n
  "!/gcmd,1,u,sum"
  "gplot" \n
  \n)

;; PlotCtrls ->Numbering Controls
(define-skeleton ansys-skeleton-numbering-controls
  ""
  nil
  "!/pnum,kp,line,area,volu,node,elem,tabn,sval,on" \n
  "!/replot"
  \n)

;; PlotCtrls -> Symbols
(define-skeleton ansys-skeleton-symbols
  ""
  nil
  "!! --- symbols ---" \n
  "!! /pbc,all,,1 !bc symbols"\n
  "!! /psf !surface loads" \n
  "!! /pbf !body loads"
  "!! /psymb,esys,on !check element esys" \n
  "!! /psymb,ndir !only for rotated nodal co-ordinate systems!" \n
  \n)

(define-skeleton ansys-skeleton-element-table
  ""
  nil
    "!! --------- etables ----------" \n
  "!! etables don't take into account higher element order!"
  "!! ---- Mohr-Coulomb failure criterion" \n
  "Z1 = 60 !tensile strength" \n
  "Z3 = 160 !compressive strength" \n
  "etable,S1,s,1" \n
  "etable,S3,s,3" \n
  "sadd,R,S1,S3,1/Z1,-1/Z3" \n
  "pletab,R,avg !avg: average over nodes" \n
  "esel,s,type,,2" \n
  "etable,Pene,cont,pene" \n
  "!etable,chat,cont,cnos !chattering levels" \n
  "!etable,cpre,cont,pres"\n
  "!plls,Pene,Pene !line elem. results" \n
  "esort,etab,R" \n
  "*get,Mc,etab,sort,,max" \n
  "*msg,,Mc" \n
  "Mohr-Coulomb criterion (< 1): %G" \n
  \n)

(define-skeleton ansys-skeleton-element-def
 ""
 nil
 "! --- element definition ---" \n
 "ID=Steel" \n
 "et,ID,solid186 !3d, 20 node" \n
 "!! et,ID,solid185 !3d, 8 node" \n
 "!! et,ID,plane183,,,0 !2d, 8 node (3)0:plane stress, 1:axissymmetric" \n
 "!! et,ID,plane182 !2d, 4 node"\n
 "!! keyopt,ID,3,1 !(3)=0:plane stress,1:axissym,2:plain strain." \n
 "!! keyopt,ID,1,0 !(1)=0:reduced integr.2:enhanced strain for bending" \n
 "!! !!for most elements the radial direction is the x-axis" \n
 "!! --- magnetics ---" \n
 "!! et,ID,plane13 !2d, legacy coupled-field ->plane223" \n
 "!! keyopt,ID,3,1 !(3)=1:axissym." \n
 "!! et,ID,infin110 !2d semi infinit electromagnetic elem." \n
 "!! keyopt,ID,3,1 !(3)=1:axissym." \n
 "!! keyopt,ID,2,1 !(2)=1:8-node" \n
 "!! --- assign attributes ---" \n
 "!! aatt,ID ! associate mat. ID with selected areas" \n
 "!! /pnum,mat,1 ! display materials" \n
 \n
)

(define-skeleton ansys-skeleton-meshing
  ""
  nil
  "!! --- Meshing ---" \n
  \n
  "!! mat,Steel" \n
  "!! mshkey,1 !1: mapped meshing,2: mapped if possible" \n
  "!! mshape,0 !0: quads 1:tri (supported shapes)" \n
  "esize,1 ! element edge length" \n
  "!! lesize,all,,,3 ! line divisions"
  "vmesh,all" \n
  "!! amesh,all" \n
  "!! /pnum,mat,1" \n
  \n
  )

(define-skeleton ansys-skeleton-geometry
  ""
  nil
  "!! --- Geometry ---"\n
  "/prep7" \n
  "rectng,x1,x2,y1,y2 ! 2d rectangle" \n
  "cyl4,xc,yc,r1,alpha1,r2,alpha2,depth ! circular area or cylinder" \n
  "!! --- booleans ---" \n
  "!! aovlap,all ! overlap areas" \n
  "!! /pnum,area,1 $ aplot" \n
  \n)

(define-skeleton ansys-skeleton-material-def
  ""
  nil
  "!! --- Material definitions ---" \n
  "Steel=1" \n
  "mp,nuxy,Steel,0.3 ! Poisson No" \n
  "mp,ex,Steel,200000 ! Elastic modulus" \n
  "!! tb,biso,Steel,1 ! bilinear isotropic plasticity" \n
  "!! yield_stress=140" \n
  "!! tangent_modulus=1400" \n
  "!! tbdata,,yield_stress,tangent_modulus !biso" \n
  "/com, === Material %Steel% is steel. ===" \n
  "!! Alu=2" \n
  "!! mp,nuxy,Alu,0.3" \n
  "!! mp,ex,Alu,70000" \n
  "!! tb,biso,Alu,1" \n
  "!! !! tbdata,,yield_stress,tangent_modulus !biso" \n
  "!! /com, === Material %Alu% is Aluminium. ===" \n
  "!! --- hyperelastic mooney rivlin mat ---" \n
  "!! for 30 % compression 100 % tension strain" \n
  \n
  "!! --- Elastomers (hyperelastic) ---" \n
  "!! Rubber = 3" \n
  "!! tb,hyper,Rubber,,,MOONEY" \n
  "!! Shore = 60" \n
  "!! ShearModule = 0.086*1.045**Shore" \n
  "!! tbdata,1,3*ShearModule/6.6" \n
  "!! tbdata,2,.3*ShearModule/6.6" \n
  "!! -- check whether to drop elem. midside nodes and use u-p formulation" \n
  "!! keyopt,Rubber,6,1		 !(6)1: mixed u-p formulation" \n
  "!! ! ogden for high strain applic. (700 % strain)" \n
  "!! tb,hyper,Rubber,,,OGDEN" \n
  \n
  "!! --- Magnetic materials ---" \n
  "!! Air = 4" \n
  "!! mp,murx,Air,1 ! murx permeability" \n
  "!! Magnet = 5" \n
  "!! Hc = 28e4 ! coercive force in A/m" \n
  "!! mp,mgxx,Magnet,Hc " \n
  "!! Mu0 = 1.2566e-6 ! field constant in Vs/(Am)" \n
  "!! Br = .4 ! residual induction in Tesla" \n
  "!! mp,murx,Magnet,Br/(Mu0*Hc)" \n
  \n)

(define-skeleton ansys-skeleton-bc
  ""
  nil
  "! --- Boundary conditions --- " \n
  \n
  "!kbc,1: 1:stepped loading"
  "!nsel,s,loc,y,0" \n
  "!    ,a,loc,y,1" \n
  "!    ,r,loc,x,0" \n
  "d,all,all" \n
  "!dlist,all" \n
  "!f,all,fx,1" \n
  "nsel,s,loc,x,1" \n
  "cp,next,uy,all !couple dofs" \n
  "f,1,fx,1" \n
  "!flist" \n
  "allsel" \n
  "/pbc,all,on" \n
  "!gplot" \n
  "!! --- magnetics ---" \n
  "!! fmagbc,'Component' ! flag force calculation" \n
  "!! bfa,all,js, ! js current density" \n
  "!! dl,all,,asym ! flux parallel to lines" \n
  "!! nsel,s,ext ! select exterior nodes" \n
  "!! dsym,asym ! flux parallel to lines"
  \n)

(define-skeleton ansys-skeleton-buckling
  ""
  nil
  "!! --- buckling ---" \n
  \n
  "!! -- static --"
  "/solu" \n
  "allsel" \n
  "outres,all,all" \n
  "pstres,on" \n
  "solve" \n
  "" \n
  "!! -- buckling --" \n
  "" \n
  "fini $ /solu" \n
  "antype,buckle" \n
  "bucopt,lanb,3" \n
  "outres,all,all" \n
  "solve" \n
  "fini $ /solu		 !creazy" \n
  "expass,on" \n
  "mxpand,3" \n
  \n)

(define-skeleton ansys-skeleton-solve
  ""
  nil
  "! --- Solution --- " \n
  \n
  "finish" \n
  "!! /config,nres,2000 !No of substeps in result file [1000]" \n
  "/solu" \n
  \n
  "!! nlhist,on !nonlinear tracking in .nlh" \n
  "!! solcontrol,on! optimised nonlinear solution defaults" \n
  "!! cnvtol,u,,0.1! convergence [0.5 % solcontrol, on: 5 %] manipulation" \n
  "!! cnvtol,f,,0.05 !solcontol,on: [0.5% F,M; 5% U]" \n
  "!! nequit,30! No of equilibr. iterations"
  "!! nldiag,nrre,on! store residual file" \n
  "!! nldiag,maxf,2! maximum files written" \n
  "!! n1=20" \n
  "!! n2=n1*100" \n
  "!! n3=n1/4" \n
  "!! nsubst,n1,n2,n3"\n
  "!! outres,all,all"\n
  "!! !antype,,rest, !perform restart operation" \n
  "!! nlgeom,on" \n
  "!! autots,on" \n
  \n
  "!! rescontrol,,1,last !create restart file(s)" \n
  "!!           ,status" \n
  "!! eqslv,pcg,1e-4" \n
  "!! nropt,unsym !frictional contacts not converging?" \n
  "!! coupling of sliding and normal stiffness" \n
  "!! stabilize,constant,energy,1e-4" \n
  "!! !stabilize,off !reduce" \n
  "!! !arclen,on ! arclen stabilisation" \n
  \n
  "!! /runst !enter the run statistics processor" \n
  "!! rall !run statistics estimator" \n
  \n
  "!! rescontrol,file_summary !check restart files" \n
  "!! antyp,,rest,1,last"\n
  "!! time,1.2 !time at the end of load step" \n
  "!! --- magnetics ---" \n
  "!! magsolv" \n
  "solve" \n
  \n)

(define-skeleton ansys-skeleton-post1
  ""
  nil
  "!! --- post 1 ---" \n
  "/post1" \n
  \n
  "!! /dscale,,1 !do not scale (for nlgeom)" \n
  "!! /dscale,,auto !or 0:scale automatically" \n
  "!! !*get,Ds,graph,WN,dscale,dmult" \n
  "!! /contour,,ncont,min,inc,max" \n
  "!! /contour,,auto !switch off user contours" \n
  "!! /edge,,1 !1:display elements in contour plots" \n
  "!! /edge,,0 !0:switch off display of elements in contour plots" \n
  "!! /plopts,minm,off !switch off min-max symbols" \n
  "!! /plopts,minm,on" \n
  "!! /pbc,rfor,,1 !1:show reaction f. symbols" \n
  "!! /pbc,rfor,,0" \n
  "!! /dist,,1/2,1 !enlarge twice" \n
  "!! /noerase ! don't erase screen between plots" \n
  "!! erase"
  "!! /triad,rbot ! coordinate system to right bot" \n
  "!! /plopts,wp ! switch off working plane" \n
  "!! /plopts,minm ! switch off min max" \n
  \n
  "set,last" \n
  "/efacet,2" \n
  "!psdisp,0" \n
  "plnsol,u,sum,2 !0:deformed only, 1:with undef model 2:with undeformed edges" \n
  "!/graphics,full ! results averaging also from interior" \n
  "!pletab,Pene" \n
  "plls,Pene,Pene !line element results" \n
  "!!! --- magnetics ---" \n
  "!! plf2d,27 ! flux lines, equipotentials" \n
  "!! plvect,b,! induction vector plot" \n
  "!! fmagsum,'component_name'" \n
  \n
  "nldpost,nrre,stat !element information nonlinear" \n
  "plnsol,nrre,,,,001 !plot residual file .nr001 " \n
  "!! etable,Pene,cont,pene" \n
  "!! etable,chat,cont,cnos" \n
  "!! etable,cpre,cont,pres" \n
  "!! etable,Slid,cont,slide" \n
  "!! etable,St,cont,stat	 !3-closed sticking" \n
  "!! 			 !2-closed sliding" \n
  "!! 			 !1-open but near" \n
  "!! 			 !0-open and far, outside pinball" \n
  "set,list" \n
  "set,last!first" \n
  "plnsol,s,1" \n
  "!antime" \n
  "!andata" \n
  "!anmres !multiple result files" \n
  \n)

(define-skeleton ansys-skeleton-path-plot
  ""
  nil
  "!! -- path plot --" \n
  "path,axis,2 ! define active path "axis"" \n
  "ppath,1" \n
  "ppath,2,,,Rair" \n
  "!psel,s,axis,...    	 !select multiple paths" \n
  "pdef,By,b,y" \n
  "plpath,By		 !plot in graph" \n
  "plpagm,By,5		 !plot on geom." \n
  \n
)

(define-skeleton ansys-skeleton-post26
  ""
  nil
  "!! --- Time-History Postprocessing ---" \n
  \n
  "/post26" \n
  "!! numvar,200 !maximum No of variables"
  "!! esol,2,1,,u,z,'displ z'" \n
  "nsol,2,1,u,z" \n
  "rforce,3,1,f,z" \n
  "!! add,4,2,,,displ,,,-1" \n
  "/grid,1" \n
  "/gmarker,1,1 !curve marking: 1: triangles,2: squares" \n
  "!! /xrange,0,1" \n
  "!! /xrange,default" \n
  "!! /yrange,0,1" \n
  "!! /axlab,x,x" \n
  "!! /axlab,y,y" \n
  "!! timerange,0,1" \n
  "!! /title,bla" \n
  "!! /stitle,,blabla !subtitle line 1 (not shown in plot)" \n
  "!! /stitle,2,blabla !subtitle line 2" \n
  "!! /tlable,x,y,bla !annotation at (x,y)" \n
  "xvar,2" \n
  "!! invert background colour" \n
  "!/RGB,index,100,100,100,0" \n
  "!/RGB,index,0,0,0,15" \n
  "!/show,png !creates jobnameXXX.png files" \n
  "plvar,3" \n
  "!/show,close" \n
  "!!prvar,3" \n
  \n)

(define-skeleton ansys-skeleton		;NEW
  "Insert full framework of an Ansys APDL file."
  "Insert brief purpose of file: "
  "!" ansys-outline-string " ********* first line ***************\n"
  "!! FILENAME: " (buffer-file-name) \n
  "!! CREATION DATE: " (current-time-string) \n
  "!! ANSYS VERSION: " ansys-current-ansys-version \n
  "!! DESCRIPTION: " str \n
  "!! ------------------------------" \n
  "!! COMMENTARY: User parameters start in upper case." \n
  "!!  Ansys command names may be ommitted (defaulting to the" \n
  "!!  previous command, except slash and asterisk commands)." \n
  "!! WARNING: Variables starting with an underscore are reserved"  \n
  "!!  (for  components and Ansys furnished macros, like" \n
  "!!  the %_FIX% table name for current displacement values or" \n
  "!!  the _RETURN and _STATUS variable (_STATUS: 0 no error, 1" \n
  "!!  note, 2 warning, 3 error)!" \n \n
  "!! ==============================" \n
  "!" ansys-outline-string " --- Setup ---" \n
  "!! ==============================" \n
  \n
  "finish "\n
  "/clear" \n
  "y !necessary for /clear" \n
  \n
  "*get,Wallstrt,active,,time,wall" \n
  "c*** Configuring for 2 processors does not harm when only 1 is present" \n
  "/config,nproc,2" \n
  "/uis,msgpop,3 !3: No warning popup boxes" \n
  "*afun,deg !trig: funs accept degree args" \n
  "*afun,rad" \n
  "/title," _ \n
  "/plopts,wp,1 !display working plane" \n
  "/triad,rbot" \n
  "!! /output, !change solver output file" \n
  "!! /input," \n
  "/filname," (setq ansys-job (skeleton-read "Ansys jobname: " "file")) \n
  \n
  "!! ==============================" \n
  "!" ansys-outline-string " --- Preprocessing --- " \n
  "!! ==============================" \n
  \n
  "!! ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string " --- Cad Import --- " \n
  "!! ------------------------------" \n
  \n
  "!! /aux15" \n
  "!! ioptn,iges,nodefeat" \n
  "!! ioptn,merge,yes" \n
  "!! ioptn,solid,yes" \n
  "!! ioptn,small,yes" \n
  "!! ioptn,gtoler,defa" \n
  "!! igesin,'test','iges'"\n
  \n
  "!! /input,fname,anf" \n
  "!! /facet,norm" \n
  \n
  "!! ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string " --- General Preprocessing --- " \n
  "!! ------------------------------" \n
  \n
  "/prep7" \n
  "Pi=3.14159265359" \n
  \n
a  "!! /pnum,area,1"\n
  \n
  "!! --- Materials and element types ---" \n
  "Steel=1" \n
  "Alu=2" \n
  "!! Contact = 3" \n
  "!! Target = 4" \n
  "mp,nuxy,Steel,0.3" \n
  "mp,ex,Steel,200000" \n
  "!! tb,biso,Steel,1" \n
  "!! yield_stress=140" \n
  "!! tangent_modulus=1400" \n
  "!! tbdata,,yield_stress,tangent_modulus !biso" \n
  "/com, === Material %Steel% is steel. ===" \n
  "et,Steel,solid186 !3d, 20 node" \n
  "!! et,Steel,solid185 !3d, 8 node" \n
  "!! et,Steel,plane183,,,0 !2d, 8 node (3)0:plane stress, 1:axissymmetric" \n
  "!! et,Steel,plane182 !2d, 4 node"\n
  "!! keyopt,Steel,3,1 !keyopt(3)=1:axissym." \n
  "!!   for most elements the radial direction is the x-axis" \n
  \n
  "!! mp,nuxy,Alu,0.3" \n
  "!! mp,ex,Alu,70000" \n
  "!! tb,biso,Alu,1" \n
  "!! !! tbdata,,yield_stress,tangent_modulus !biso" \n
  "!! /com, === Material %Alu% is Aluminium. ===" \n
  "!! et,Alu,solid186 !3d" \n
  "!! !! et,Alu,plane183 !2d" \n
  "!! !! et,Alu,plane182 !2d 4 node" \n
  "!! !! keyopt,Alu,3,1 !0:plane stress, 1:axissym." \n
  \n
  "!! !! --- Contacts ---" \n
  "!! r,Contact" \n
  "!! et,Contact,conta174 !3d, 8 node" \n
  "!! !! et,Contact,conta173, !3d, 4 node" \n
  "!! !! et,Contact,conta172 !2d, 3 node" \n
  "!! !! et,Contact,conta171 !2d, 2 node" \n
  "!! et,Target,targe170 !3d" \n
  "!! !! et,Target,targe169 !2d, 2/3 node" \n
  "!! keyo,Contact,2,1 !algorithm 0:augm. Lagrange (default),1:penalty,2:MPC,4:pure Lagrange" \n
  "!! keyo,Contact,5,1 !initial contact closure,1:auto CNOF adjustment to close geometric gap only" \n
  "!! keyo,Contact,9,2 !initial penetration,1:ignore initial gaps/penetr 2:ramp" \n
  "!! keyo,Contact,10,2 !contact stiffness update,2:each NR iteration,1:each substep" \n
  "!! keyo,Contact,12,0 !contact behaviour,0:frictional/-less (default),1:rough" \n
  "!! real,Contact" \n
  "!! rmod,Contact,3,1. !FKN:normal penalty stiffness factor (default:1)" \n
  "!! rmod,Contact,5,0.0 !ICONT:amount of initial contact closure (positiv:penetration)" \n
  "!! rmod,Contact,6,-0.1 !PINB:pinball radius (negativ means no scaling:absolute distance)" \n
  "!! rmod,Contact,10,0. !CNOF:contact surface offset" \n
  "!! mp,mu,Contact,0.4 !friction factor" \n
  "!! rmod,Contact,12,0. ! FKT:tangent stiffness factor (corresp. to FKN),0:means 1 for Ansys!!!" \n
  \n
  "!" ansys-outline-string ansys-outline-string ansys-outline-string " --- Geometry ---" \n
  \n
  "bloc,0,1,0,1,0,1" \n
  "*get,A1,area,,num,max" \n
  "!! rectng,0,1,0,1 !x1,x2,y1,y2" \n
  "!! k,,1,0,0 & KN = _return !keypoint number" \n
  \n
  "!! /number,1 !0: colour and number,1: colour" \n
  "!! /pnum,line,1 !1: turn on numbering" \n
  "!! lplot" \n
  "!! lesize,1,,,3" \n
  \n
  "!! --- Meshing ---" \n
  \n
  "!! mat,Steel" \n
  "!! mshkey,1 !1: mapped meshing,2: mapped if possible" \n
  "!! mshape,0 !0: quadrilaterals" \n
  "esize,1" \n
  "vmesh,all" \n
  \n
  "!! amesh,all" \n
  \n
  "!! !! --- Rigid targets ---" \n
  \n
  "!! type,Target" \n
  "!! real,Contact" \n
  "!! tshap,line" \n
  "!! *get,Nmax,node,,num,max" \n
  "!! n,Nmax+1,1,1,0" \n
  "!!  ,Nmax+2,1,2,0" \n
  "!! e,Nmax+1,Nmax+2" \n
  "!! tshap,pilo" \n
  "!! e,Nmax+1" \n
  \n
  "!! !! --- Contacts --- " \n
  \n
  "!! type,Contact" \n
  "!! real,Contact" \n
  "!! esurf !,,reverse !also 2d" \n
  \n
  "!! /pcb !bc symbols"\n
  "!! /psf !surface loads" \n
  "!! /pbf !body loads"
  "!! /psymb,esys,on !check element esys" \n
  "!! /psymb,ndir !only for rotated nodal co-ordinate systems!" \n
  "!! cncheck !initial contact status" \n
  \n
  "!" ansys-outline-string ansys-outline-string ansys-outline-string " --- Boundary conditions --- " \n
  \n
  "nsel,s,loc,y,0" \n
  "    ,a,loc,y,1" \n
  "    ,r,loc,x,0" \n
  "d,all,all" \n
  "nsel,s,loc,x,1" \n
  "cp,next,uy,all !couple dofs" \n
  "f,1,fx,1" \n
  "allsel" \n
  "/pbc,all,on" \n
  \n
  "!! ==============================" \n
  "!" ansys-outline-string " --- Solution --- " \n
  "!! ==============================" \n
  \n
  "/solu" \n
  \n
  "!! solcontrol,,on, ! ,,check contact state,pressure load stiffness"
  \n
  "!! n1=20" \n
  "!! n2=n1*100" \n
  "!! n3=n1/4" \n
  "!! nsubst,n1,n2,n3"\n
  "!! outres,all,all"\n
  "!! nlgeom,on" \n
  "!! autots,on" \n
  \n
  "!! rescontrol,,all,1 !restart files" \n
  "!! eqslv,pcg,1e-4" \n
  "!! cnvtol,f,,0.05 !solcontol,on: [0.5% F,M; 5% U]" \n
  "!! nropt,unsym !frictional contacts not converging?" \n
  "!! coupling of sliding and normal stiffness"
  \n
  "/eof ------------------------------" \n
  \n
  "/runst !enter the run statistics processor" \n
  "rall !run statistics estimator" \n
  "/solu"  \n
  "*get,Wallasol,active,,time,wall" \n
  \n
  "solve" \n
  "y" \n
  "save $ finish" \n
  \n
  "*get,Wallbsol,active,,time,wall" \n
  \n
  "!! ==============================" \n
  "!" ansys-outline-string " --- Postprocessing ---" \n
  "!! ==============================" \n
  \n
  "!! ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string
  " --- General Postprocessing --- " \n
  "!! ------------------------------" \n
  \n
  "/post1" \n
  \n
  "!! /dscale,,1 !do not scale (for nlgeom)" \n
  "!! /dscale,,auto !or 0:scale automatically" \n
  "!! !*get,Ds,graph,WN,dscale,dmult" \n
  "!! /contour,,ncont,min,inc,max" \n
  "!! /contour,,auto !switch off user contours" \n
  "!! /edge,,1 !1:display elements in contour plots" \n
  "!! /edge,,0 !0:switch off display of elements in contour plots" \n
  "!! /plopts,minm,off !switch off min-max symbols" \n
  "!! /plopts,minm,on" \n
  "!! /pbc,rfor,,1 !1:show reaction f. symbols" \n
  "!! /pbc,rfor,,0" \n
  "!! /expand,8,lpolar,half,,45 !polar symmetry expansion" \n
  "!! /expand,8,lpolar,half,,45 !polar symmetry expansion" \n
  "!!  !half symmetry(mirrored) and then 8 x 45° offset!" \n
  "!! /expand,18,axis,,,10 !axis symmetry 180° expansion" \n
  "!! /expand !switch off expansion" \n
  "!! /dist,,1/2,1 !enlarge twice" \n
  "!! " \n
  \n
  "set,last" \n
  "/efacet,2" \n
  "plnsol,u,sum !,2 !2:overlay undeformed edges" \n
  \n
  "!! --- Birth & Death --- " \n
  "!! etable,strain,epto,1" \n
  "!! esel,s,etab,strain,0.02" \n
  "!! /solu" \n
  "!! antype,,rest"\n
  "!! ekill,all"\n
  "!! esel,all" \n
  \n
  "!! --- Reactions" \n
  "Ls=1" \n
  "set,Ls" \n
  "*get,Ns,active,,solu,ncmss !number of substeps" \n
  "*get,Dim,parm,Reaction,dim,x" \n
  "*if,Dim,le,1,then" \n
  "*dim,Reaction,array,Ns,1" \n
  "*endif" > \n
  "*do,I,1,Ns" \n
  "set,Ls,I" > \n
  "fsum" \n
  "*get,Fx,fsum,,item,fx" \n
  "Reaction(I)=Fx" \n
  "*enddo" > \n
  "/gcolumn,1,'Reaction'" \n
  "/axlab,x,Substep" \n
  "/axlab,y,Force in N" \n
  "*vplot,,Reaction" \n
  \n
  "!! --- Animations ---" \n
  "/seg,multi,process,0.15 !process.avi, delay .15" \n
  "Ls=1 !Loadstep 1" \n
  "!antime," \n
  "!andata," \n
  "set,LS" \n
  "*get,Ns,active,,solu,ncmss !number of substeps" \n
  "*do,I,1,Ns" \n
  "set,Ls,I" > \n
  "plnsol,s,eqv" \n
  "*enddo" > \n
  "/seg,off,process,.15" \n
  \n
  "!! --------- etables ----------" \n
  "!! etables don't take into account higher element order!"
  "!! ---- Mohr-Coulomb failure criterion" \n
  "Z1 = 60 !tensile strength" \n
  "Z3 = 160 !compressive strength" \n
  "etable,S1,s,1" \n
  "etable,S3,s,3" \n
  "sadd,R,S1,S3,1/Z1,-1/Z3" \n
  "pletab,R,avg !avg: average over nodes" \n
  "esort,etab,R" \n
  "*get,Mc,etab,sort,,max" \n
  "*msg,,Mc" \n
  "Mohr-Coulomb criterion (< 1): %G" \n
  \n
  "!! --- multiple graphics windows" \n
  "/window,1,rtop" \n
  "/window,2,ltop" > \n
  "/window,3,lbot" > \n
  "/window,4,rbot" > \n
  "/gtype,all,node,0 !switch off node display" > \n
  "/gcmd,1,pletab,s1" \n
  "/gcmd,2,pletab,s3" > \n
  "/gcmd,3,pletab,r,avg" > \n
  "/gcmd,4,plvect,s" > \n
  "gplot" > \n
  \n
  "/window,1,full" \n
  \n
  "!! --- cross section by working plane ---" \n
  "/cplane,1 !1:cutting plane is x-y-wp" \n
  "wpcsys,1,11 !align wp with specified c-sys" \n
  "wpoffs,,-100" \n
  "wprota,0,90,0 !z,x,y axis rotation" \n
  \n
  "/type,1,zcap ! z-buffered capping" \n
  "!! /type,1,zqsl ! z-bufferd capping with outlines" \n
  "!! /type,1,basic !switch off cross sections" \n
  \n
  "!! ------------------------------" \n
  "!" ansys-outline-string
  ansys-outline-string " --- Time-History Postprocessing ---" \n
  "!! ------------------------------" \n
  \n
  "/post26" \n
  "!! esol,2,1,,u,z,'displ z'" \n
  "nsol,2,1,u,z" \n
  "rforce,3,1,f,z" \n
  "!! add,4,2,,,displ,,,-1" \n
  "/grid,1" \n
  "/gmarker,1,1 !curve marking: 1: triangles,2: squares" \n
  "!! /xrange,0,1" \n
  "!! /xrange,default" \n
  "!! /yrange,0,1" \n
  "!! /axlab,x,x" \n
  "!! /axlab,y,y" \n
  "!! timerange,0,1" \n
  "!! /title,bla" \n
  "!! /stitle,,blabla !subtitle line 1" \n
  "!! /stitle,2,blabla !subtitle line 2" \n
  "!! /tlable,x,y,bla !annotation at (x,y)" \n
  "xvar,2" \n
  "!! invert background colour" \n
  "!/RGB,index,100,100,100,0" \n
  "!/RGB,index,0,0,0,15" \n
  "!/show,png !creates jobnameXXX.png files" \n
  "plvar,3" \n
  "!/show,close" \n
  "!!prvar,3" \n
  \n
  "!! ------------------------------" \n
  "!" ansys-outline-string
  ansys-outline-string " --- Time Stat Processing ---" \n
  "!! ------------------------------" \n
  \n
  "*get,Walldone,active,,time,wall" \n
  "Preptime=(Wallasol-Wallstrt)*60" \n
  "Solvtime=(Wallbsol-Wallasol)*60" \n
  "Totaltim=(Walldone-Wallstrt)" \n
  "*msg,ui,Preptime,Solvtime,Totaltim" \n
  "Time in min for preprocessing: %G %/ &" \n
  "Time in min for Solving: %G %/ &" \n
  "Total time in h: %G %/ &" \n
  "=== End of timing messages ===" \n
  \n)


(defmacro define-ansys-skeleton (command documentation &rest definitions) ;FIXME: documentation
  "Define COMMAND with an optional docstring DOCUMENTATION.
to insert statements as in DEFINITION ...  Prior
DEFINITIONS (e.g. from ~/.emacs) are maintained.  Each definition
is built up as (format PROMPT ELEMENT ...).  Alternately a
synonym definition can be (format . PREVIOUSLY-DEFINED-FORMAT).

For the meaning of (PROMPT ELEMENT ...) see `skeleton-insert'.
Each DEFINITION is actually stored as
	(put COMMAND format (PROMPT ELEMENT ...)), which you can
also do yourself."
  (unless (stringp documentation)
    (setq definitions (cons documentation definitions)
	  documentation ""))
  ;; The compiled version doesn't. FIXME: Doesn't what?
  (require 'backquote)
  (`(progn
      (let ((definitions '(, definitions)))
	(while definitions
	  ;; skeleton need not be loaded to define these
	  (or (get '(, command) (car (car definitions)))
	      (put '(, command) (car (car definitions))
		   (if (symbolp (cdr (car definitions)))
		       (get '(, command) (cdr (car definitions)))
		     (cdr (car definitions)))))
	  (setq definitions (cdr definitions))))
      (defun (, command) ()
	(, documentation)
	(interactive)
	(skeleton-insert
	 (or (get '(, command) ansys-format)
	     (error "%s statement syntax not defined for ansys format %s"
		    '(, command) ansys-format)))))))

(define-ansys-skeleton ansys-if
  "Insert an if statement in the current format's syntax."
  (format "Value/Parameter 1: "
	  "*IF," str ","
	  (read-string "Operation: (EQ,NE,LT,GT,LE,GE,ABLT,ABGT) ")
	  ","
	  (read-string "Value/Parameter 2: ")
	  ","
	  (read-string "Action: (:label,STOP,EXIT,CYCLE,THEN,otto) ")
	  \n >_
	  "*ENDIF"
	  \n)
  (mac . format))


(define-ansys-skeleton ansys-if-then
  "Insert an if statement in the current format's syntax."
  (format "Value/Parameter 1: "
	  "*IF," str ","
	  (read-string "Operation: (EQ,NE,LT,GT,LE,GE,ABLT,ABGT) ")
	  ","
	  (read-string "Value/Parameter 2: ")
	  ",THEN" \n
	  > _ \n
	  ("*ELSEIF? %s: "		;FIXME: here subskeleton!
	   > "*ELSEIF," str ","
	   (read-string "Operation: (EQ,NE,LT,GT,LE,GE,ABLT,ABGT) ")
	   ","
	   (read-string "Next Value/Parameter: ")
	   ",THEN" \n
	   > \n)
	  "*ELSE" > \n
	  > \n
	  "*ENDIF" > \n)
  (mac . format))

(define-ansys-skeleton ansys-do
  "Insert an if statement in the current format's syntax."
  (format "Parameter: "
	  "*DO," str ","
	  (read-string "Start Value/Parameter: ")
	  ","
	  (read-string "Finish Value/Parameter: ")
	  ","
	  (read-string "Increment Value/Parameter: ") \n
	  > _ \n
	  "*ENDDO" > \n)
  (mac . format))

(define-ansys-skeleton ansys-mp		;FIXME: skeleton a bit over the top
  "Insert an if statement in the current format's syntax."
  (format "Material Property: (EX,ALPX,PRXY,NUXY,GXY,DAMP,MU,DENS,KXX) "
	  "MP," str ","
	  (read-string "Material Number: ")
	  ","
	  (read-string "Constant Value: ")
	  ","
	  (read-string "Linear Coefficient? : ")
	  ","
	  (read-string "Quadratic Coefficient? : ")
	  ","
	  (read-string "Cubic Coefficient? : ")
	  ","
	  (read-string "Quartic Coefficient? : ")
	  \n)
  (mac . format))

;;; --- managing ansys runs ---

;;;###autoload
(defun ansys-abort-file (&optional arg) ;NEW
  "Writes an ansys abort file for terminating the current run.
Normally the Ansys jobname is taken from the variable `ansys-job'
you can change it with the equally named
function (or type \\[ansys-job]).  The file (JOBNAME.abt) in the default
directory contains the sole word \"nonlinear\".  The function
prompts for an appropriate job name when ARG is negative,
otherwise it uses a sensible default.  In case the default
directory is not of your liking, use: `M-x cd'."
  (interactive "p")
  (unless arg (setq arg 0))
					;  (debug)
  (let ((filename ansys-job)
	(default-job "file"))
    (cond
     ((< arg 0)				;ask for job-name
      (setq filename
	    (read-string
	     (concat "job name: [" default-job "] ") nil nil default-job))
      (setq filename (concat filename ".abt")))
     (filename				;filname is known
      (setq filename (concat filename ".abt")))
     (t					;search for /filn
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "/filn.*,\\(\\w+\\)" nil 'noerror)
	    (setq filename (concat (match-string 1) ".abt"))
	  (setq filename "file.abt")))))
    (if (yes-or-no-p (concat "Write \"" default-directory filename "\"? "))
	(ansys-write-abort-file filename)
      (message "Function \"ansys-abort-file\" canceled!"))))

(defun ansys-write-abort-file (filename) ;NEW
  "Open file FILENAME, clear it's contents and insert \"nonlinear\"."
  (find-file filename)
  (delete-region (point-min) (point-max))
  (insert "nonlinear")
  (save-buffer)
  (message (concat "Wrote \"%s\" into " default-directory ".") filename))

;;;###autoload
(defun ansys-display-error-file ()	;NEW
  "Open the current Ansys error file in the buffer default directory.
You can change the directory with \"M-x cd\".  When the variable
`ansys-job' is not specified use the Ansys default job name
\"file\" i. e. \"file.err\" as the error file.  You can change
the job name with \"\\[ansys-job]\"."
  (interactive)
  (let (file)
    (if ansys-job
	(setq file ansys-job)
      (setq file "file"))
    (setq file (concat file ".err"))
    (find-file-read-only-other-window file)
    (goto-char (point-max))
    (auto-revert-tail-mode 1)))

(defun ansys-copy-or-send-above	()	;NEW
  "Copy or send to Ansys above code - up to the cursor"
  (interactive)
  (kill-ring-save (point-min) (point))	;point-min is heeding narrowing
  ;; no-property stuff necessary?????

;;   (if (y-or-n-p
;;        (concat
;; 	"Start this Ansys run: (lic: " ansys-license ", job: " ansys-job ")? "))
;;       (message "Starting run...")
;;     (error "Run canceled"))
  (message "Copied from beginning of buffer to cursor."))

(defun ansys-send-to-ansys (beg end)	;NEW
  "Send code line or region to running Ansys process.
Argument BEG is the beginning of the region.
Argument END is the end of the region."
  (interactive "r")
  (unless (string= "run" (process-status ansys-process))
    (setq mode-line-process (format ":%s" (process-status ansys-process)))
    (force-mode-line-update)
    (error "No Ansys process is running"))
  (let (bol eol s)
    (cond ((and mark-active (not (= beg end)))
	   (when (< (point) (region-end))
	     (exchange-point-and-mark))
	   (setq s (buffer-substring-no-properties beg end))
	   (setq mark-active nil))
	  (t
	   (back-to-indentation)
	   (setq bol (point))
	   (end-of-line)
	   (setq eol (point))
	   (setq s (buffer-substring-no-properties bol eol))
	   (forward-line)))
					;(ansys-next-code-line) ;; not always desirable!
    (process-send-string ansys-process (concat s "\n"))
    ;;  (walk-windows
    ;;    (lambda (w)
    ;;      (when (string= (buffer-name (window-buffer w)) "*Ansys*")
    ;;        (with-selected-window w (goto-char (point-max))))))
    (setq mode-line-process (format ":%s" (process-status ansys-process)))
    (force-mode-line-update)
    (display-buffer "*Ansys*" 'other-window)))

(defun ansys-process-running-p ()
  (string= "run" (process-status ansys-process))) ;TODO

(defun ansys-update-mode-line ()
  (setq mode-line-process (format ":%s" (process-status ansys-process)))
  (force-mode-line-update))

(defun ansys-query-ansys-command ()	;NEW
  ""
  (interactive)
  (unless (string= "run" (process-status ansys-process))
    (setq mode-line-process (format ":%s" (process-status ansys-process)))
    (force-mode-line-update)
    (error "No Ansys process is running"))
  (let ((s (read-string "Ansys command: ")))
    (process-send-string ansys-process (concat s "\n"))
    ;;  (walk-windows
    ;;    (lambda (w)
    ;;      (when (string= (buffer-name (window-buffer w)) "*Ansys*")
    ;;        (with-selected-window w (goto-char (point-max))))))
    (setq mode-line-process (format ":%s" (process-status ansys-process)))
    (force-mode-line-update)
    (display-buffer "*Ansys*" 'other-window)))

(defun ansys-start-ansys ()		;NEW
  "Start an Ansys run (when no run is already active).
Ask for confirmation with some run particulars before actually
starting the process."
  (interactive)
  (cond ((string= ansys-license "")
	 (error "You must set the `ansys-license' variable"))
	((string= ansys-license-file "")
	 (error "You must set the `ansys-license-file' variable"))
	((string= ansys-program "")
	 (error "You must set the `ansys-program' variable")))
  (when (and ansys-process (string= "run" (process-status ansys-process)))
    (error "Ansys already running, won't start subsequent runs"))
  (ansys-license)
  (ansys-job)
  (if (y-or-n-p
       (concat
	"Start this Ansys run: (lic: " ansys-license ", job: " ansys-job ")? "))
      (message "Starting run...")
    (error "Run canceled"))
  (setenv "LM_LICENSE_FILE" ansys-license-file)
  (setenv "PATH" (concat "/appl/ansys/ansys110/bin:" (getenv "PATH")))
  (setq ansys-process
	(start-process
	 "ansys" "*Ansys*" ansys-program
	 (concat "-p " ansys-license " -j " ansys-job)))
  (display-buffer "*Ansys*" 'other-window)
  ;;  (process-send-string ansys-process "\n") Ansys idiosynncrasy, skip
  ;;the license agreement, normally harmless but not possible when
  ;;there is a lock-file from a crashed run!
  (message "Starting run...done, process status: %s, Id: %d"
	   (process-status ansys-process)
	   (process-id ansys-process))
  (setq mode-line-process (format ":%s" (process-status ansys-process)))
  (force-mode-line-update)
  (walk-windows				;HINT: Markus Triska
   (lambda (w)
     (when (string= (buffer-name (window-buffer w)) "*Ansys*")
       (with-selected-window w (goto-char (point-max)))))))

(defun ansys-kill-ansys ()		;NEW
  "Kill the current Ansys run under Emacs.
The function asks for confirmation before actually killing the
process.  Warning: Ansys writes a lock file (jobname.lock) if the
process is killed and not regularly exited.  You should prefere
the function `ansys-exit-ansys'."
  (interactive)
  (unless (ansys-process-running-p)
    (error "Error: No active Ansys process"))
  (if (yes-or-no-p
       "Do you want to kill the Ansys run?")
      (progn
;	(message "Killing run...")
	(delete-process ansys-process)
	(message "Killing run...done.")
	(setq mode-line-process (format ":%s" (process-status ansys-process)))
	(force-mode-line-update))
    (error "Killing of Ansys run canceled")))

(defun ansys-exit-ansys ()		;NEW
  "Exit normally the current Ansys run under Emacs.
The function asks for confirmation before exiting the process
with the Ansys /EXIT,all command which saves all model data."
  (interactive)
  (unless (ansys-process-running-p)
    (error "Error: No active Ansys process"))
  (if (yes-or-no-p
       "Do you want to exit the Ansys run?")
      (progn
	(message "Trying to exit run ...")
	(process-send-string ansys-process "finish $ /exit,all\n")
	(setq mode-line-process (format ":%s" (process-status ansys-process)))
	(force-mode-line-update))
    (error "Exiting of Ansys run canceled")))

;;;###autoload
(defun ansys-start-ansys-help ()       ;NEW_C
  "Start the Ansys help system.
Alternatively one can use the Ansys \"/SYS, anshelp110\" command
when running Ansys interactively and provided that anshelp110 (or
anshelp110.chm on Windows) is found within the PATH environment
variable."
  (interactive)
  (if (string= ansys-help-file "")
      (error "You must set the `ansys-help-file' variable")
    (progn
      (message "Starting the Ansys help system.")
      (cond
       (ansys-is-unix-system-flag
	(start-process "ansys-help-file" nil ansys-help-file))
       ((string= system-type "windows-nt")
	(w32-shell-execute "Open" ansys-help-file)))))) ;HINT: Eli Z., M. Dahl

(defun ansys-kill-buffer-query-function ()
  (when (or (string= (process-status ansys-process) "run")
	    (string= (process-status ansys-process) "stop"))
    (yes-or-no-p "Ansys process is active, quit buffer anyway? ")))

(defun ansys-process-status ()		;NEW
  "Show the process status in the Emacs command line (minibuffer).

    'run'
          for a process that is running.
    'stop'
          for a process that is stopped but continuable.
    'exit'
          for a process that has exited.
    'signal'
          for a process that has received a fatal signal.
    'open'
          for a network connection that is open.
    'closed'
          for a network connection that is closed.  Once a connection
          is closed, you cannot reopen it, though you might be able to
          open a new connection to the same place.
    'connect'
          for a non-blocking connection that is waiting to complete.
    'failed'
          for a non-blocking connection that has failed to complete.
    'listen'
          for a network server that is listening.
    'nil'
          if PROCESS-NAME is not the name of an existing process."
  (interactive)
  (message "Ansys process status: %s, process identification No: %d"
	   (process-status ansys-process)
	   (process-id ansys-process)))

;;;###autoload
(defun ansys-license-status ()		;NEW
  "Display the Ansys license status.
For Unix systems do this in a separate buffer, under Windows
start the anslic_admin.exe utility, which has a button for
displaying the license status."
  (interactive)
  (cond ((string= ansys-license-file "")
	 (error "You must set the `ansys-license-file' variable"))
	((string= ansys-lmutil-program "")
	 (error "You must set the `ansys-lmutil-program' variable")))
  (let ((current-b (buffer-name))
	(buffer (buffer-name (get-buffer-create "*LMutil*"))))
    (message "Retrieving license status information from %s." ansys-license-file)
    (cond
     (ansys-is-unix-system-flag
      (setenv "LM_LICENSE_FILE" ansys-license-file)
      (set-buffer buffer)
      (toggle-read-only -1)
      (insert "\n\n======================================================================\n\n\n")
      (start-process "lmutil" "*LMutil*" ansys-lmutil-program "lmstat" "-a")
      ;;       (while (string= "run" (process-status "lmutil"))
      ;; 	(sit-for 1))
      (toggle-read-only 1)
      (set-buffer current-b)
      (display-buffer "*LMutil*" 'other-window)
      (walk-windows
       (lambda (w)
	 (when (string= (buffer-name (window-buffer w)) "*LMutil*")
	   (with-selected-window w (goto-char (point-max)))))))
     ((string= system-type "windows-nt")
      (w32-shell-execute nil ansys-lmutil-program))))) ;nil for executable

(defun ansys-start-graphics ()		;NEW
  "Start the Ansys display in interactive mode."
  (interactive)
  (unless (string= "run" (process-status ansys-process))
    (error "No Ansys process is running"))
  (process-send-string ansys-process "/show,3d\n/menu,grph\n") ;valid in any processor
  (display-buffer "*Ansys*" 'other-window))

(defun ansys-start-pzr-box ()		;NEW PanZoomRotate box
  "Start the Ansys Pan/Zoom/Rotate dialog box in interactive mode."
  (interactive)
  (unless (string= "run" (process-status ansys-process))
    (error "No Ansys process is running"))
  (process-send-string ansys-process "/ui,view\n") ;valid in any processor
  (display-buffer "*Ansys*" 'other-window))

(defun ansys-replot ()			;NEW_C
  "Replot the Ansys interactive graphics screen."
  (interactive)
  (unless (string= "run" (process-status ansys-process))
    (error "No Ansys process is running"))
  (process-send-string ansys-process "/replot\n") ;valid in any processor
  (display-buffer "*Ansys*" 'other-window))

(defun ansys-fit ()			;NEW_C
  "Fit FEA entities to the Ansys interactive graphics screen."
  (interactive)
  (unless (string= "run" (process-status ansys-process))
    (error "No Ansys process is running"))
  (process-send-string ansys-process "/dist\n/replot\n") ;valid in any processor
  (display-buffer "*Ansys*" 'other-window))

(defun ansys-program ()			;NEW
  "Change the Ansys program name.
And specify it in the variable `ansys-program'."
  (interactive)
  (let (pr)
    (if (and ansys-program
	     (not (string= ansys-program "")))
	(setq pr ansys-program)
      (setq pr "/ansys_inc/v110/ansys/bin/ansys110"))
    (setq ansys-program
	  (read-file-name
	   (concat "Ansys program name [" pr "]: ") "" pr))
    (if (not (file-exists-p ansys-program))
	(error "Error: File %s does not exist" ansys-program))
    (message (concat "Ansys program is set to \"" ansys-program "\"."))))

(defun ansys-help-file ()			;NEW
  "Change the Ansys help file name.
And specify it in the variable `ansys-help-file'."
  (interactive)
  (let (pr)
    (if (and ansys-help-file
	     (not (string= ansys-help-file "")))
	(setq pr ansys-help-file)
      (if ansys-is-unix-system-flag
	  (setq pr "/ansys_inc/v110/ansys/bin/anshelp110")
	(setq pr "c:\\\\Program\ Files\\Ansys\ Inc\\v110\\CommonFiles\\HELP\\en-us\\ansyshelp.chm")))
    (setq ansys-program
	  (read-file-name
	   (concat "Ansys help file [" pr "]: ") "" pr))
    (message (concat "Ansys help file is set to \"" ansys-program "\"."))))


(defun ansys-lmutil-program ()		;NEW
  "Change the Ansys LMutil program name.
And specify it in the variable `ansys-lmutil-program'.  The
function inserts the string `default-directory' in the prompt
when the variable `insert-default-directory' is not nil."
  (interactive)
  (let (pr)
    (if (and ansys-lmutil-program
	     (not (string= ansys-lmutil-program "")))
	(setq pr ansys-lmutil-program)
      (if ansys-is-unix-system-flag
	  (setq pr "/ansys_inc/shared_files/licensing/linop64/lmutil")
	(setq pr "c:\\\\Program Files\\Ansys Inc\\Shared\\\
                   Files\\Licensing\\intel\\anslic_admin.exe")))
    (setq ansys-lmutil-program
	  (read-file-name
	   (concat "Ansys LMutil program name [" pr "]: ") "" pr))
    (message (concat "Ansys LMutil program is set to \""
		     ansys-lmutil-program "\"."))))

;;;###autoload
(defun ansys-job ()			;NEW
  "Change the Ansys job name.
And put it into the variable `ansys-job'."
  (interactive)
  (if ansys-job
      (setq ansys-job
	    (read-string "job name: " ansys-job))
    (setq ansys-job
	  (read-string "job name: " "file")))
  (message (concat "Job-name is set to \"" ansys-job "\".")))

(defun ansys-license-file ()		;NEW
  "Change the Ansys license file name.
And specify it in the variable `ansys-license-file' which can
either be the license file name or license server
specification(s).  The server specification must include the port
number when it isn't 1055, the default port number:
port_number@server_name, multiple server names are separated by a
colon, for example \"27005@rbgs421x:27005@rbgs422x:...\"."
  (interactive)
  (if ansys-license-file
      (setq ansys-license-file
	    (read-string "License file name or license server name(s): "
			 ansys-license-file))
    (setq ansys-license-file
	  (read-string "Ansys license file name or license server name(s): " "")))
  (message (concat "Set ansys-license-file to \"" ansys-license-file "\".")))

(defun ansys-license ()			;NEW
  "Change the Ansys license type.
And specify it in the variable `ansys-license'."
  (interactive)
  (let ((lic (if (not (string= ansys-license ""))
		 ansys-license
	       "struct")))
    (setq ansys-license
	  (completing-read (concat "License type [" lic "] (TAB for completion): ")
			   ansys-license-types
			   nil nil nil nil lic))
    (message (concat "Ansys license type is now set to \"" ansys-license "\"."))))

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
  "Displays APDL variable assignments in the current Buffer.
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
	'face 'bold))
      (set-buffer variable-buffer)
      (insert
       (propertize
	(concat"      ----------  =  assignments ----------\n")
	'face 'bold))
      (set-buffer current-buffer)
      (goto-char (point-min))
      (setq r "^\\s-*[^!\n=]*\\<.+\\>\\s-*=\\s-*[^=\n]*")
      (while (re-search-forward r nil t)
	(unless (string-match "^\\s-*/com\\|^\\s-*c\\*\\*\\*" (match-string 0))
	  (setq s (concat
		   (propertize (format "%5d " (line-number-at-pos)) 'mouse-face 'highlight 'face 'bold)
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
	  'face 'bold))
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
