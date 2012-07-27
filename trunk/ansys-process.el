;;; ansys-process.el -- Managing runs and processes for the ANSYS-Mode

;; Copyright (C) 2006 - 2012  H. Dieter Wilhelm GPL V3

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

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- customisation ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup ANSYS-process nil
  "Customisation 'process' subgroup for the ANSYS-Mode."
  :group 'ANSYS)

(defcustom ansys-install-directory
  (cond ((string= window-system "x")
	 ;; "/" the root dir is the default installation directory on Unix
	 "/")
	(t ;; the default is "C:\\Program Files\\" on Windows
	 "C:\\Program Files\\"))
  "This is the directory where ANSYS is installed."
  :type 'string
  :group 'ANSYS-process
  )

(defcustom ansys-job "file"			;NEW_C
  "Variable storing the ANSYS job name.
It is initialised to 'file' (which is also the ANSYS default job
name).  See `ansys-abort-file' for a way of stopping a solver run
in a controlled way and `ansys-display-error-file' for viewing
the respective error file."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-program
  (let ((version (if (boundp 'ansys-current-ansys-version)
		     ansys-current-ansys-version
		   "140")))
  (if (string= window-system "x")
      (concat ansys-install-directory "ansys_inc/v"
	      version "/ansys/bin/ansys" version)
    (concat ansys-install-directory "Ansys\ Inc\\v" version "\\bin\\ansys" version)))		;NEW_C
  "This variable stores the ANSYS executable name.
When the file is not in your search path, you have to specify the
full qualified file name and not only the name of the executable.
For example: \"/ansys_inc/v140/ansys/bin/ansys140\" and not only
\"ansys140\".  You might customise this variable or use the
function `ansys-program' to do this for the current session
only."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-help-program
  (if (string= window-system "x")
      (concat ansys-install-directory "ansys_inc/v"
      ansys-current-ansys-version "/ansys/bin/anshelp"
      ansys-current-ansys-version)
    (concat ansys-install-directory "Ansys\ Inc\\v" ansys-current-ansys-version "\\commonfiles\\jre\\intel\\bin\\Javaw.exe"))		;NEW_C
  "The ANSYS help executable.
It is called with
\\[ansys-start-ansys-help] (`ansys-start-ansys-help').  When the
executable is not in the search path, you have to complement the
executable with its complete path.  For example the default
locations are \"/ansys_inc/v140/ansys/bin/anshelp140\" on UNIX
and \"c:\\\\Program Files\\Ansys\
Inc\\v140\\commonfiles\\jre\\intel\\bin\\Javaw.exe\" on
Windows (XP).  Since V12.0 ANSYS uses a java interpreter."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-help-program-parameters (concat " -Xmx500000000 -cp \"c:\\Program Files\\Ansys Inc\\v"
ansys-current-ansys-version "\\commonfiles\\help\" HelpDocViewer")
  "Stores parameters for the variable `ansys-help-program' under Windows.
For example: ' -Xmx500000000 -cp \"c:\\Program Files\\Ansys
Inc\\v140\\commonfiles\\help\" HelpDocViewer' (the whitespace
before -X... is important)."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-lmutil-program ;NEW_C
  (if (string= window-system "x")
      (concat ansys-install-directory "ansys_inc/shared_files/licensing/linx64/lmutil")
    (concat ansys-install-directory "Ansys Inc\\Shared Files\\licensing\\winx64\\anslic_admin.exe"))
  "A FlexLM license manager executable.
For example: \"/ansys_inc/shared_files/licensing/linx64/lmutil\"
or in case of a Windows 32-bit OS \"c:\\\\Program Files\\Ansys
Inc\\Shared\ Files \\Licensing\\intel\\anslic_admin.exe.  This
variable is used for displaying the license status or starting
the ansli_admin tool under Windows with the function
`ansys-license-status'."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-license-file nil ;NEW_C
  "The FlexLM license file name or license server specification(s).
The license server specification(s) should include the port
number even if it's the default port 1055 because the lmutil tool
needs it in the following way: port_number@server_name, use the
colon for multiple servers, for example
\"27005@rbgs421x:27005@rbgs422x\".

Setting this variable skips the effect of previously set
environment variables, which have the following order of
precedence: 1. ANSYSLMD_LICENSE_FILE environment variable, 2.)
The FLEXlm resource file: ~/.flexlmrc on Unix or somewhere in the
Windows registry. 3.) The LM_LICENSE_FILE variable. 4.) The
ansyslmd.ini file in the licensing directory (This is what
anslic_admin is doing in an ANSYS recommended installation).  5.)
The license file itself."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-ansysli-servers nil ;NEW_C
  "Used to identify the server machine for the Licensing Interconnect.
Set it to port@host.  The default port is 2325."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-license-types		;NEW_C
  '("ansys" "struct" "ane3" "ansysds" "ane3fl" "preppost")
  "List of available license types to choose for an ANSYS run.
This list should contain the license types you can choose from.
Below are often used license types (as e.g. seen with the
function `ansys-license-status') and their corresponding
WorkBench terminology.

\"ansys\" - Mechanical U (without thermal capability)
\"struct\" - Structural U (with thermal capability)
\"ane3\" - Mechanical/Emag (Structural U with electromagnetics)
\"ansysds\" - Mechanical/LS-Dyna (Mechanical U with ANSYS LS-Dyna inter-phase)
\"ane3fl\" - Multiphysics
\"preppost\" - PrepPost (no solving capabilities)"
  :group 'ANSYS-process)

(defcustom ansys-license "struct"		;NEW_C
  "The License type with which the ANSYS interpreter will be started.
See `ansys-license-types' for often used ANSYS license types."
;  :options '("ansys" "struct" "ane3" "ane3fl" "ansysds" "preppost")
  :options ansys-license-types
  ;; options not available for strings (only hooks, alists, plists E22)
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-no-of-processors 2
  "No of processors to use for an ANSYS solver run.
If smaller then 3 the run does not require additonal HPC
licenses. 2 is the ANSYS default."
  :type 'integer
  :group 'ANSYS-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- constants ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ansys-process-name "ANSYS"		;NEW_C
  "Variable containing Emacs' name for an ANSYS process.
Variable is only used internally in the mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- functions ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ansys-write-abort-file (filename) ;NEW
  "Open file FILENAME, clear it's contents and insert \"nonlinear\"."
  (find-file filename)
  (delete-region (point-min) (point-max))
  (insert "nonlinear\n")
  (save-buffer)
  (message "Wrote \"%s\" into \"%s\"." filename default-directory))

;;;###autoload
(defun ansys-abort-file (&optional arg) ;NEW
  "Writes an ANSYS abort file for stopping the current run.
The abort file does not terminate the current session but
triggers the solver to stop solving in an orderly manner.  This
function prompts for a job name when ARG is negative.  Otherwise
it tries to guess it from the current file, if this fails the
jobname is taken from the variable `ansys-job', you can change
this variable by calling the equally named interactive
function (i. e. typing \\[ansys-job]) or setting it directly as
lisp expression (i. e.  typing \"\\[eval-expression] (setq
ansys-job \"jobname\")\", where jobname is a name of your liking
but must be enclosed with double quotes (\") to represent a lisp
string).  The file jobname.abt in the current directory contains
the sole word \"nonlinear\". In case the `default-directory' is
not the working directory of the interesting job, you might
change it with \"\\[cd]\"."
  (interactive "p")
  (let ((jobname ansys-job)
	filename)
    (cond
     ((< arg 0)				;ask for job-name
      (setq filename
	    (read-string
	     (concat "Job name: [" jobname "] ") nil nil
	     jobname))
      (setq filename (concat filename ".abt")))
     (t					;search for /filn
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "/filn.*,\\(\\w+\\)" nil 'noerror)
	    (setq filename (concat (match-string 1) ".abt"))
	  (setq filename (concat jobname ".abt"))))))
    (if (yes-or-no-p (concat "Write \"" default-directory filename "\"? "))
	(ansys-write-abort-file filename)
      (message "ansys-abort-file canceled!"))))

;;;###autoload
(defun ansys-display-error-file ()	;NEW
  "Open the current interpreter error file in the current directory.
You might change the directory with \"M-x `cd'\".  The error file
name consists of the current job name and the suffix '.err'.  For
the job name the variable `ansys-job' is used. You can change the
job name interactively either with the \"\\[ansys-job]\" or in
the customisation facility (by calling `ansys-customise-ansys')."
  (interactive)
  (let ((file ansys-job))
    (setq file (concat file ".err"))
    (find-file-read-only-other-window file)
    (goto-char (point-max))
    (auto-revert-tail-mode 1)))

(defun ansys-copy-or-send-above	()	;NEW
  "Copy or send all of above code - up from the cursor position."
  (interactive)
  (let ((process (get-process
		  (if (boundp 'ansys-process-name)
		      ansys-process-name
		    "ANSYS"))))
    ;; no-property stuff necessary?????
    ;;   (if (y-or-n-p
    ;;        (concat
    ;; 	"Start this ANSYS run: (lic: " ansys-license ", job: " ansys-job ")? "))
    ;;       (message "Starting run...")
    ;;     (error "Run canceled"))
    (cond ((ansys-process-running-p)
	   (comint-send-region process (point-min) (point))
	   (display-buffer (process-buffer process) 'other-window))
	  (t
	   (kill-ring-save (point-min) (point))	;point-min is heeding narrowing
	   (message "Copied from beginning of buffer to cursor.")))))

(defun ansys-send-to-ansys ( &optional stay)	;NEW
  "Send region (or code line) to the ANSYS interpreter, otherwise copy it.
Argument BEG may the beginning of the region.  If there is no
region active send/copy the complete code line, if the cursor is
in no code line (like a comment) go to the next code line and
indicate an error.  When there is no running ANSYS interpreter process
just copy the respective code (region or line) to the system
clipboard and skip to the subsequent code line.  With any prefix
argument STAY copy or send code but remain at the current cursor
position."
  (interactive "P")
  (let (code
	beg
	end
	(process (get-process
		  (if (boundp 'ansys-process-name) ansys-process-name)))
	(region (and transient-mark-mode mark-active)))
;    	(region (region-active-p))) ;this is for Emacs-23.1
    ;; make a valid region if possible, when region is not active:
    ;; "region" will be the whole code line (including \n)
    (if region
	(setq beg (region-beginning)
	      end (region-end))
      (unless (ansys-code-line-p)
	(unless stay
	  (ansys-next-code-line))
	(error "There was no active region or code line"))
      (save-excursion
	(setq beg (line-beginning-position))
	(forward-line 1)
	(setq end (point))))

    ;; move cursor to subsequent code line unless stay
    (unless stay
      (if (and region
	       (< (point) end))
	(exchange-point-and-mark))
      (ansys-next-code-line))

    ;; invalidate region
    (setq mark-active nil)

    ;; send or copy region or line
    (cond ((ansys-process-running-p)
	   (setq code (buffer-substring-no-properties beg end))
	   (comint-send-string process
			       (concat code ""); "\n"); why did I do \n?
			       )
	   (display-buffer "*ANSYS*" 'other-window))
	  (t
	   (kill-ring-save beg end)
	   (if region
	       (message "Copied region.")
	     (message "Copied code line."))))))

(defun ansys-process-running-p ()
  "Return nil if no ANSYS interpreter process is running."
  (let ((proc (get-process
	       (if (boundp 'ansys-process-name) ansys-process-name))))
    (if proc
	(string= "run" (process-status proc))
      nil)))

;; (defun ansys-update-mode-line ()
;;   (setq mode-line-process (format ":%s" (process-status ansys-process)))
;;   (force-mode-line-update))

(defun ansys-query-ansys-command ()	;NEW
  "Ask for a string which will be sent to the interpreter."
  (interactive)
  (unless (ansys-process-running-p)
;    (setq mode-line-process (format ":%s" (process-status ansys-process)))
;    (force-mode-line-update)
    (error "No ANSYS process is running"))
  (let ((s (read-string "Send to interpreter: ")))
    (comint-send-string (get-process
			 (if (boundp 'ansys-process-name)
			     ansys-process-name)) (concat s "\n"))
    ;;  (walk-windows
    ;;    (lambda (w)
    ;;      (when (string= (buffer-name (window-buffer w)) "*ANSYS*")
    ;;        (with-selected-window w (goto-char (point-max))))))
    ;; (setq mode-line-process (format ":%s" (process-status ansys-process)))
    ;; (force-mode-line-update)
    (display-buffer "*ANSYS*" 'other-window)))

(require 'comint)

(defun ansys-start-ansys ()		;NEW
  "Start the ANSYS interpreter process.
And display the run configuration. The specified No of cores is
not shown if they are chosen smaller then 3 (see
`ansys-number-of-processors')."
  (interactive)
  (let (ansys-process-buffer)
    (when (ansys-process-running-p)
      (error "An Interpreter is already running under Emacs"))
    (message "Preparing an ANSYS interpreter run...")
    ;; (setq comint-use-prompt-regexp t) TODO: ???
    (ansys-program "")		 ;take exec from -program var.
    (ansys-license-file "")	 ;
    (ansys-ansysli-servers "")	 ;
    ;(ansys-license "")		 ;

    (if (y-or-n-p
	 (concat
	  "Start run?  (license type: " ansys-license
	  ;; "Start run?  (license type: " (if (boundp
	  ;; 'ansys-license) ansys-license)
	  (if (>= ansys-no-of-processors 3)
	      (concat ", No of processors: " (number-to-string ansys-no-of-processors))
	    "")
	  ", job: " (if (boundp 'ansys-job) ansys-job)
	  " in " default-directory ", server: " ansys-license-file ")"))
	(message "Starting the ANSYS interpreter...")
      (error "Function ansys-start-ansys canceled"))
    (setq ansys-process-buffer
	  (make-comint ansys-process-name ansys-program nil
		       (if (>= ansys-no-of-processors 3)
			   (concat "-np " (number-to-string ansys-no-of-processors)
				   " -p " ansys-license " -j " ansys-job)
		       (concat "-p " ansys-license " -j " ansys-job))))
    ;;  (comint-send-string (get-process ansys-process-name) "\n")
    (display-buffer ansys-process-buffer 'other-window)
    ;;  (switch-to-buffer ansys-process-buffer)
    (other-window 1)
    (setq comint-prompt-regexp "BEGIN:\\|PREP7:\\|SOLU_LS[0-9]+:\\|POST1:\\|POST26:\\|RUNSTAT:\\|AUX2:\\|AUX3:\\|AUX12:\\|AUX15:")
    (font-lock-add-keywords nil (list comint-prompt-regexp))

	  ;; comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom comint-watch-for-password-prompt comint-truncate-buffer)
  ))

(defun ansys-kill-ansys ()		;NEW
  "Kill the current ANSYS run under Emacs.
The function asks for confirmation before actually killing the
process.  Warning: ANSYS writes a lock file (jobname.lock) if the
process is killed and not regularly exited.  You should prefere
the function `ansys-exit-ansys'."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No active ANSYS solver process"))
  (if (yes-or-no-p
       "Do you want to kill the running ANSYS solver?")
      (progn
;	(message "Killing run...")
	(delete-process (get-process ansys-process-name))
	(message "Killing ANSYS run...done.")
	(display-buffer "*ANSYS*" 'otherwindow)
	;; (setq mode-line-process (format ":%s" (process-status (get-process ansys-process-name))))
	;; (force-mode-line-update)
	)
    (message "Killing of ANSYS run canceled.")))

(defun ansys-exit-ansys ()		;NEW
  "Exit normally the current ANSYS run under Emacs.
The function asks for confirmation before exiting the process
with the ANSYS /EXIT,all command which saves all model data."
  (interactive)
  (unless (ansys-process-running-p)
    (error "Error: No active ANSYS process"))
  (if (yes-or-no-p
       "Do you want to exit the ANSYS run?")
      (progn
	(message "Trying to exit run ...")
	(process-send-string (get-process ansys-process-name) "finish $ /exit,all\n"))
	;; (setq mode-line-process (format ":%s" (process-status ansys-process)))
	;; (force-mode-line-update))
    (error "Exiting of ANSYS run canceled")))

;;;###autoload
(defun ansys-start-ansys-help ()       ;NEW_C
  "Start the ANSYS help system.
Alternatively under a Unix system, one can also use the ANSYS
command line \"/SYS, anshelp140\" when running ANSYS
interactively, provided that anshelp140 is found in the search
paths for executables (these are stored in the PATH environment
variable)."
  (interactive)
  (ansys-help-program "")		;checking
  (progn
    (message "Starting the ANSYS help browser...")
    (cond
     ((ansys-is-unix-system-p)
      (start-process "ANSYS-help-program" nil ansys-help-program))
     ((string= system-type "windows-nt")
      (if (fboundp 'w32-shell-execute)
	  (w32-shell-execute "Open" (concat "\"" ansys-help-program "\"")
			 ansys-help-program-parameters)))  ;HINT: Eli Z.,M. Dahl
     (t
      (error "Can only start the ANSYS help on Windows and UNIX systems")))))

(defun ansys-browse-ansys-help ( &optional arg)       ;NEW_C
  "Open the ANSYS help for a command or element name in the web browser.
The function is looking for a keyword before or at the cursor
location.  This is working also in a comment.  If argument ARG is
a prefix argument query for the command name.  You can browse
also predefined sections in the help manual, the use of
completions is advisable.  For example

All- keyword e.g. allbeams, allcontacts TODO:
Elements
########
ALLBEAMS
ALLCIRCUS
ALLCOMBINS
ALLCOMBIS
ALLCONTACS
ALLCONTAS
ALLCPTS
ALLFLUIDS
ALLFOLLW
ALLHFS
ALLHSFLDS
ALLINFINS
ALLINTERS
ALLLINKS
ALLMASS
ALLMATRIXS
ALLMESHS
ALLMPCS
ALLPIPES
ALLPLANES
ALLPRETS
ALLREINF
ALLROMS
ALLSEL
ALLSHELLS
ALLSOLIDS
ALLSOLSHS
ALLSOURCS
ALLSURFS
ALLTARGES
ALLTRANS
#### Materials
Hlp_AM_CH2anel
Hlp_AM_CH2anis
Hlp_AM_CH2bh
Hlp_AM_CH2biso
Hlp_AM_CH2bkin
Hlp_AM_CH2boyc
Hlp_AM_CH2chab
Hlp_AM_CH2cree
Hlp_AM_CH2crei
Hlp_AM_CH2dp
Hlp_AM_CH2dper
Hlp_AM_CH2edpcap
Hlp_AM_CH2evis
Hlp_AM_CH2hfmatl
Hlp_AM_CH2hill
Hlp_AM_CH2jntma
Hlp_AM_CH2mela
Hlp_AM_CH2miso
Hlp_AM_CH2mkin
Hlp_AM_CH2moon
Hlp_AM_CH2neo
Hlp_AM_CH2nlis
Hlp_AM_CH2ogde
Hlp_AM_CH2piez
Hlp_AM_CH2poly
Hlp_AM_CH2pzrs
Hlp_AM_CH2rate
Hlp_AM_CH2swel
Hlp_AM_CH2useh
Hlp_AM_CH2user

Hlp_G_ADVREZ  Hlp_G_ADVREZ.html
Hlp_G_ADVexad  Hlp_G_ADVexad.html
Hlp_G_BAS6  Hlp_G_BAS6.html
Hlp_G_BASPGRP  Hlp_G_BASPGRP.html
Hlp_G_BASrani  Hlp_G_BASrgenanim.html
Hlp_G_BASrassy  Hlp_G_BASrgenassm.html
Hlp_G_BASrcap  Hlp_G_BASrgenimg.html
Hlp_G_BASrdef  Hlp_G_BASrgenset.html
Hlp_G_BASrgen  Hlp_G_BASrgen.html
Hlp_G_BASrlist  Hlp_G_BASrgenlist.html
Hlp_G_BASrstart  Hlp_G_BASrgenstart.html
Hlp_G_BASrtable  Hlp_G_BASrgentable.html
Hlp_G_STRSHELL_4  Hlp_G_STRSHELL_3.html#abem0829010908
Hlp_G_STR_CMAN  Hlp_ctec_contman.html
Hlp_G_STR_CMAN  Hlp_ctec_contman.html
Hlp_Help_Rev  viewer_help.html
Hlp_O_WhatsNew  Hlp_R_RelNotes.html
Hlp_PDSctrl  Hlp_G_ADVPDS3.html#PDSctrl
Hlp_PDSfiles  Hlp_G_ADVPDS3.html#PDSfiles
Hlp_PDShost  Hlp_G_ADVPDS3.html#PDShost
Hlp_R_RelNotes  Hlp_R_RelNotes.html

#####
UI

Hlp_UI_ALE  Hlp_UI_ALE.html
Hlp_UI_ANIMRES  Hlp_UI_ANIMRES.html
Hlp_UI_ANSYSHelp  viewer_help.html
Hlp_UI_AREAHARD  Hlp_UI_selectent.html
Hlp_UI_ASEL_Acca  Hlp_UI_selectent.html
Hlp_UI_ASEL_Attd  Hlp_UI_selectatt.html
Hlp_UI_ASEL_Attr  Hlp_UI_SELbyatt.html
Hlp_UI_ASEL_Ext  Hlp_UI_selectent.html
Hlp_UI_ASEL_Hdpt  Hlp_UI_selectent.html
Hlp_UI_ASEL_Loca  Hlp_UI_selectloc.html
Hlp_UI_ASEL_Pick  Hlp_UI_selectent.html
Hlp_UI_An3D_Arro  Hlp_UI_Annot.html
Hlp_UI_An3D_Bitm  Hlp_UI_Annot.html
Hlp_UI_An3D_Cont  Hlp_UI_Annot.html
Hlp_UI_An3D_Line  Hlp_UI_Annot.html
Hlp_UI_An3D_Poly  Hlp_UI_Annot.html
Hlp_UI_An3D_Symb  Hlp_UI_Annot.html
Hlp_UI_An3D_TxtB  Hlp_UI_Annot.html
Hlp_UI_An3D_TxtS  Hlp_UI_Annot.html
Hlp_UI_Anim  Hlp_G_BAS15_animprog.html#AnimControl
Hlp_UI_Anno_Arcs  Hlp_UI_Annot.html
Hlp_UI_Anno_Arro  Hlp_UI_Annot.html
Hlp_UI_Anno_Circ  Hlp_UI_Annot.html
Hlp_UI_Anno_Cont  Hlp_UI_Annot.html
Hlp_UI_Anno_Dime  Hlp_UI_Annot.html
Hlp_UI_Anno_Line  Hlp_UI_Annot.html
Hlp_UI_Anno_Pies  Hlp_UI_Annot.html
Hlp_UI_Anno_Poly  Hlp_UI_Annot.html
Hlp_UI_Anno_Rect  Hlp_UI_Annot.html
Hlp_UI_Anno_Symb  Hlp_UI_Annot.html
Hlp_UI_Anno_TxtB  Hlp_UI_Annot.html
Hlp_UI_Anno_TxtS  Hlp_UI_Annot.html
Hlp_UI_Anno_Wedg  Hlp_UI_Annot.html
Hlp_UI_Ansys_Rev  ai_legal.html
Hlp_UI_Arry_Edit  Hlp_UI_Arry_Edit.html
Hlp_UI_Arry_Stat  Hlp_UI_Arry_Stat.html
Hlp_UI_Arry_Tabl  Hlp_UI_Arry_Table.html
Hlp_UI_Beam_ASEC  Hlp_UI_Beamtool.html
Hlp_UI_Beam_CHAN  Hlp_UI_Beamtool.html
Hlp_UI_Beam_CSOL  Hlp_UI_Beamtool.html
Hlp_UI_Beam_CTUB  Hlp_UI_Beamtool.html
Hlp_UI_Beam_HATS  Hlp_UI_Beamtool.html
Hlp_UI_Beam_HREC  Hlp_UI_Beamtool.html
Hlp_UI_Beam_I  Hlp_UI_Beamtool.html
Hlp_UI_Beam_L  Hlp_UI_Beamtool.html
Hlp_UI_Beam_QUAD  Hlp_UI_Beamtool.html
Hlp_UI_Beam_RECT  Hlp_UI_Beamtool.html
Hlp_UI_Beam_T  Hlp_UI_Beamtool.html
Hlp_UI_Beam_Z  Hlp_UI_Beamtool.html
Hlp_UI_CIMAGE  Hlp_UI_CIMAGE.html
Hlp_UI_CZ_MISC  Hlp_UI_contacset.html#wcontactsetmisc
Hlp_UI_CZ_RIGID  Hlp_UI_contacset.html#wcontactsetrigid
Hlp_UI_CZ_THERM  Hlp_UI_contacset.html#wcontactsetthermal
Hlp_UI_Cz_CNST  Hlp_UI_contacset.html#wcontactsetcnst
Hlp_UI_Cz_Done  Hlp_UI_contact.html#wfinishcontact
Hlp_UI_Cz_Flip  Hlp_UI_contact.html#wflipnormals
Hlp_UI_Cz_OCt  Hlp_UI_contacset.html#wcontactsetinitial
Hlp_UI_Cz_OID  Hlp_UI_contacset.html#wcontactsetid
Hlp_UI_Cz_OPar  Hlp_UI_contacset.html#wcontactsetbasic
Hlp_UI_Cz_OTg  Hlp_UI_contacset.html#wcontactsetfriction
Hlp_UI_Cz_PCt2D  Hlp_UI_contact.html#wselectcontact
Hlp_UI_Cz_PCt3D  Hlp_UI_contact.html#wselectcontact
Hlp_UI_Cz_PILO  Hlp_UI_contact.html#wsdefinepilot
Hlp_UI_Cz_PTg2D  Hlp_UI_contact.html#wselecttarget
Hlp_UI_Cz_PTg3D  Hlp_UI_contact.html#wselecttarget
Hlp_UI_Cz_Pair2D  Hlp_UI_contact.html#wsetparameters
Hlp_UI_Cz_Pair3D  Hlp_UI_contact.html#wsetparameters
Hlp_UI_Cz_PrCNST  Hlp_UI_contact.html#wsetparamsurf
Hlp_UI_Dummy  Hlp_UI_Dummy.html
Hlp_UI_ESEL_Adja  Hlp_UI_selelem.html
Hlp_UI_ESEL_Attd  Hlp_UI_selectatt.html
Hlp_UI_ESEL_Attr  Hlp_UI_SELbyatt.html
Hlp_UI_ESEL_Live  Hlp_UI_selectent.html
Hlp_UI_ESEL_Name  Hlp_UI_selelem.html
Hlp_UI_ESEL_Pick  Hlp_UI_selectent.html
Hlp_UI_ESEL_Resu  Hlp_UI_selectent.html
Hlp_UI_ElemType  Hlp_UI_Elem_Type.html
Hlp_UI_HardCopy  Hlp_UI_HardCopy.html
Hlp_UI_Help_Ins  installlic_set.html
Hlp_UI_Help_Rev  viewer_help.html
Hlp_UI_HomePage  Hlp_UI_HomePage.html
Hlp_UI_KEYHARD  Hlp_UI_selectent.html
Hlp_UI_KSEL_Attd  Hlp_UI_selectatt.html
Hlp_UI_KSEL_Attr  Hlp_UI_SELbyatt.html
Hlp_UI_KSEL_Ext  Hlp_UI_selectent.html
Hlp_UI_KSEL_Hdpt  Hlp_UI_selectent.html
Hlp_UI_KSEL_Loca  Hlp_UI_selectloc.html
Hlp_UI_KSEL_Pick  Hlp_UI_selectent.html
Hlp_UI_LECOPY  Hlp_UI_LECOPY.html
Hlp_UI_LEFLIP  Hlp_UI_LEFLIP.html
Hlp_UI_LINEHARD  Hlp_UI_selectent.html
Hlp_UI_LSEL_Attd  Hlp_UI_selectatt.html
Hlp_UI_LSEL_Attr  Hlp_UI_SELbyatt.html
Hlp_UI_LSEL_Ext  Hlp_UI_selectent.html
Hlp_UI_LSEL_Hdpt  Hlp_UI_selectent.html
Hlp_UI_LSEL_Lcca  Hlp_UI_selectent.html
Hlp_UI_LSEL_Lnrd  Hlp_UI_SELbylr.html
Hlp_UI_LSEL_Loca  Hlp_UI_selectloc.html
Hlp_UI_LSEL_Pick  Hlp_UI_selectent.html
Hlp_UI_MFXSolu  Hlp_UI_MFXSolu.html
Hlp_UI_MFXTCtrl  Hlp_UI_MFXTCtrl.html
Hlp_UI_MFXmap  Hlp_UI_MFXmap.html
Hlp_UI_MFdefine  Hlp_UI_MFdefine.html
Hlp_UI_MFsetup  Hlp_UI_MFsetup.html
Hlp_UI_MFsoln  Hlp_UI_MFsoln.html
Hlp_UI_MFtime  Hlp_UI_MFtime.html
Hlp_UI_MeshCtl  Hlp_UI_MeshCtl.html
Hlp_UI_ModlQP  Hlp_UI_ModlQP.html
Hlp_UI_NSEL_Attd  Hlp_UI_selectatt.html
Hlp_UI_NSEL_Attr  Hlp_UI_SELbyatt.html
Hlp_UI_NSEL_Ext  Hlp_UI_selectent.html
Hlp_UI_NSEL_Loca  Hlp_UI_selectloc.html
Hlp_UI_NSEL_Pick  Hlp_UI_selectent.html
Hlp_UI_NSEL_Resu  Hlp_UI_selectent.html
Hlp_UI_P1_Etable  Hlp_UI_P1_ETABLE.html
Hlp_UI_P26_Var  Hlp_UI_P26.html
Hlp_UI_PCHardCop  Hlp_UI_PC_HardCopy.html
Hlp_UI_PIMAGE  Hlp_UI_PIMAGE.html
Hlp_UI_PLNSOL  Hlp_UI_plnsol.html
Hlp_UI_PanZoom  Hlp_UI_PanZoom.html
Hlp_UI_QESOL  Hlp_UI_QUSOLU.html
Hlp_UI_QNSOL  Hlp_UI_QUSOLU.html
Hlp_UI_QSSOL  Hlp_UI_QUSOLU.html
Hlp_UI_QUALITY  Hlp_G_BAS11_3.html#bstQwidbox3atwr
Hlp_UI_QUERY  Hlp_UI_QUERY.html
Hlp_UI_RIMAGE  Hlp_UI_RIMAGE.html
Hlp_UI_RealCons  Hlp_UI_RealCons.html
Hlp_UI_SOLU_ADVN  Hlp_UI_SOLUCNTRL.html
Hlp_UI_SOLU_BASI  Hlp_UI_SOLUCNTRL.html
Hlp_UI_SOLU_NONL  Hlp_UI_SOLUCNTRL.html
Hlp_UI_SOLU_SOLN  Hlp_UI_SOLUCNTRL.html
Hlp_UI_SOLU_TRAN  Hlp_UI_SOLUCNTRL.html
Hlp_UI_SOL_CNVT  Hlp_UI_SOL_CNVT.html
Hlp_UI_Scal_Parm  Hlp_UI_Scal_Parm.html
Hlp_UI_Toolbar  Hlp_UI_Toolbar.html
Hlp_UI_Tutorials  tutpreface.html
Hlp_UI_VSEL_Attd  Hlp_UI_selectatt.html
Hlp_UI_VSEL_Attr  Hlp_UI_SELbyatt.html
Hlp_UI_VSEL_Loca  Hlp_UI_selectloc.html
Hlp_UI_VSEL_Pick  Hlp_UI_selectent.html
Hlp_UI_WIMAGE  Hlp_UI_WIMAGE.html
Hlp_UI_WP_Offset  Hlp_UI_WP_Offset.html
Hlp_UI_WP_Set  Hlp_UI_WP_Set.html
Hlp_UI_Web_Site  Hlp_UI_HomePage.html
Hlp_UI_Whats_New  Hlp_R_RelNotes.html
Hlp_UI_advopt  Hlp_UI_advopt.html
Hlp_UI_anopts0  Hlp_UI_anopts0.html
Hlp_UI_anopts2  Hlp_UI_anopts2.html
Hlp_UI_anopts3  Hlp_UI_anopts3.html
Hlp_UI_anopts3a2  Hlp_UI_anopts3a2.html
Hlp_UI_anopts4  Hlp_UI_anopts4.html
Hlp_UI_anopts4a1  Hlp_UI_anopts4a1.html
Hlp_UI_anopts4a2  Hlp_UI_anopts4a2.html
Hlp_UI_anopts7  Hlp_UI_anopts7.html
Hlp_UI_anopts8  Hlp_UI_anopts8.html
Hlp_UI_arcopts  Hlp_UI_arcopts.html
Hlp_UI_autofit  Hlp_UI_autofit.html
Hlp_UI_boolopts  Hlp_UI_boolopts.html
Hlp_UI_cfdprops  Hlp_UI_cfdprops.html
Hlp_UI_cirr  Hlp_UI_cirr.html
Hlp_UI_contstyle  Hlp_UI_contstyle.html
Hlp_UI_coriolis  Hlp_UI_coriolis.html
Hlp_UI_coval  Hlp_UI_coval.html
Hlp_UI_damping  Hlp_UI_damping.html
Hlp_UI_dataset  Hlp_UI_dataset.html
Hlp_UI_ddam  Hlp_UI_ddam.html
Hlp_UI_def_mat  Hlp_UI_def_mat.html
Hlp_UI_def_mid  Hlp_UI_def_mid.html
Hlp_UI_device  Hlp_UI_device.html
Hlp_UI_dscale  Hlp_UI_dscale.html
Hlp_UI_dump  Hlp_UI_dump.html
Hlp_UI_edge  Hlp_UI_edge.html
Hlp_UI_egsol  Hlp_UI_egsol.html
Hlp_UI_eread  Hlp_UI_eread.html
Hlp_UI_esol  Hlp_UI_esol.html
Hlp_UI_etableadd  Hlp_UI_etableadd.html
Hlp_UI_etadd  Hlp_UI_etadd.html
Hlp_UI_etchgwid  Hlp_UI_etchgwid.html
Hlp_UI_export  Hlp_UI_export.html
Hlp_UI_expsolhrx  Hlp_UI_expsolhrx.html
Hlp_UI_extopt  Hlp_UI_extopt.html
Hlp_UI_fileopt  Hlp_UI_fileopt.html
Hlp_UI_fileset  Hlp_UI_fileset.html
Hlp_UI_fscale  Hlp_UI_fscale.html
Hlp_UI_funcbld  Hlp_G_BASFUNCTOOL.html
Hlp_UI_funcld  Hlp_G_BASFUNCLOAD.html
Hlp_UI_grphset  Hlp_UI_grphset.html
Hlp_UI_harfrqetc  Hlp_UI_harfrqetc.html
Hlp_UI_hidden  Hlp_UI_hidden.html
Hlp_UI_import2  Hlp_UI_import2.html
Hlp_UI_lccreate  Hlp_UI_lccreate.html
Hlp_UI_listset  Hlp_UI_listset.html
Hlp_UI_maclib  Hlp_UI_maclib.html
Hlp_UI_mattpgen  Hlp_UI_mattpgen.html
Hlp_UI_mems  Hlp_UI_mems.html
Hlp_UI_meshatt  Hlp_UI_meshatt.html
Hlp_UI_modaxes  Hlp_UI_modaxes.html
Hlp_UI_modcomsp  Hlp_UI_modcomsp.html
Hlp_UI_modcurv  Hlp_UI_modcurv.html
Hlp_UI_modgrid  Hlp_UI_modgrid.html
Hlp_UI_mpanel  Hlp_UI_mpanel.html
Hlp_UI_mpdelete  Hlp_UI_mpdelete.html
Hlp_UI_mpdyn  Hlp_UI_mpdyn.html
Hlp_UI_mpliblist  Hlp_UI_mpliblist.html
Hlp_UI_mplibread  Hlp_UI_mplibread.html
Hlp_UI_mplin  Hlp_UI_mplin.html
Hlp_UI_msgctrls  Hlp_UI_msgctrls.html
Hlp_UI_mshropts  Hlp_UI_mshropts.html
Hlp_UI_multiplot  Hlp_UI_multiplot.html
Hlp_UI_multspec  Hlp_UI_multspec.html
Hlp_UI_nread  Hlp_UI_nread.html
Hlp_UI_number  Hlp_UI_number.html
Hlp_UI_numexphrx  Hlp_UI_numexphrx.html
Hlp_UI_opanl  Hlp_UI_opanl.html
Hlp_UI_opdel  Hlp_UI_opdel.html
Hlp_UI_opsubp  Hlp_UI_opsubp.html
Hlp_UI_optcntrls  Hlp_UI_optcntrls.html
Hlp_UI_optnoprm  Hlp_UI_optnoprm.html
Hlp_UI_optplvar  Hlp_UI_optplvar.html
Hlp_UI_orient  Hlp_UI_orient.html
Hlp_UI_outopts  Hlp_UI_outopts.html
Hlp_UI_outstat  Hlp_UI_outstat.html
Hlp_UI_plesol  Hlp_UI_plesol.html
Hlp_UI_plvect  Hlp_UI_plvect.html
Hlp_UI_presol  Hlp_UI_presol.html
Hlp_UI_prnsol  Hlp_UI_prnsol.html
Hlp_UI_prntctrl  Hlp_UI_prntctrl.html
Hlp_UI_props  Hlp_UI_props.html
Hlp_UI_pscr  Hlp_UI_pscr.html
Hlp_UI_psdvalmul  Hlp_UI_psdvalmul.html
Hlp_UI_qdval  Hlp_UI_qdval.html
Hlp_UI_radopts  Hlp_UI_radopts.html
Hlp_UI_rcon  Hlp_UI_rcon.html
Hlp_UI_readopts  Hlp_UI_readopts.html
Hlp_UI_refercond  Hlp_UI_refercond.html
Hlp_UI_rotcoord  Hlp_UI_rotcoord.html
Hlp_UI_setlstep  Hlp_UI_setlstep.html
Hlp_UI_setupaux  Hlp_UI_setupaux.html
Hlp_UI_sfarad  Hlp_UI_sfarad.html
Hlp_UI_sferad  Hlp_UI_sferad.html
Hlp_UI_sflrad  Hlp_UI_sflrad.html
Hlp_UI_size  Hlp_UI_size.html
Hlp_UI_slexit  Hlp_UI_slexit.html
Hlp_UI_slshow  Hlp_UI_slshow.html
Hlp_UI_spec  Hlp_UI_spec.html
Hlp_UI_specgen  Hlp_UI_specgen.html
Hlp_UI_steadctrl  Hlp_UI_steadctrl.html
Hlp_UI_stgetelem  Hlp_UI_stgetelem.html
Hlp_UI_stpctrl  Hlp_UI_stpctrl.html
Hlp_UI_stvread  Hlp_UI_stvread.html
Hlp_UI_stvset  Hlp_UI_stvset.html
Hlp_UI_stvwrite  Hlp_UI_stvwrite.html
Hlp_UI_svtypeetc  Hlp_UI_svtypeetc.html
Hlp_UI_symbols  Hlp_UI_symbols.html
Hlp_UI_tallow  Hlp_UI_tallow.html
Hlp_UI_tapersect  Hlp_UI_tapersect.html
Hlp_UI_timesub  Hlp_UI_timesub.html
Hlp_UI_timetime  Hlp_UI_timetime.html
Hlp_UI_timtint  Hlp_UI_timtint.html
Hlp_UI_topobas  Hlp_UI_topobas.html
Hlp_UI_topofunc  Hlp_UI_topofunc.html
Hlp_UI_toporun  Hlp_UI_toporun.html
Hlp_UI_transset  Hlp_UI_transset.html
Hlp_UI_uimp  Hlp_UI_uimp.html
Hlp_UI_usrbeam  Hlp_UI_usrbeam.html
Hlp_UI_uvsol0  Hlp_UI_uvsol0.html
Hlp_UI_varadd2  Hlp_UI_p26varadd2.html
Hlp_UI_viewopts  Hlp_UI_viewopts.html
Hlp_UI_windopts  Hlp_UI_windopts.html
Hlp_UI_writeaux  Hlp_UI_writeaux.html

#####
linear material?

Hlp_lanel  Hlp_L_matl3.html#lanel
Hlp_lbiso  Hlp_L_matl3.html#lbiso
Hlp_lbist  Hlp_L_matl3.html#lbist
Hlp_lbkin  Hlp_L_matl3.html#lbkin
Hlp_lblk  Hlp_L_matl3.html#lblk
Hlp_lcab  Hlp_L_matl3.html#lcab
Hlp_lcom  Hlp_L_matl3.html#lcom
Hlp_lcon  Hlp_L_matl3.html#lcon
Hlp_ldi0  Hlp_L_matl3.html#ldi0
Hlp_ldi1  Hlp_L_matl3.html#ldi1
Hlp_ldi2  Hlp_L_matl3.html#ldi2
Hlp_ldi3  Hlp_L_matl3.html#ldi3
Hlp_ldi4  Hlp_L_matl3.html#ldi4
Hlp_ldi5  Hlp_L_matl3.html#ldi5
Hlp_ldi6  Hlp_L_matl3.html#ldi6
Hlp_ldi7  Hlp_L_matl3.html#ldi7
Hlp_lelf  Hlp_L_matl3.html#lelf
Hlp_leo1  Hlp_L_matl3.html#leo1
Hlp_leo2  Hlp_L_matl3.html#leo2
Hlp_leo3  Hlp_L_matl3.html#leo3
Hlp_leo4  Hlp_L_matl3.html#leo4
Hlp_leo5  Hlp_L_matl3.html#leo5
Hlp_levi  Hlp_L_matl3.html#levi
Hlp_lfo1  Hlp_L_matl3.html#lfo1
Hlp_lfo2  Hlp_L_matl3.html#lfo2
Hlp_lfo3  Hlp_L_matl3.html#lfo3
Hlp_lfo4  Hlp_L_matl3.html#lfo4
Hlp_lgca  Hlp_L_matl3.html#lgca
Hlp_lhon  Hlp_L_matl3.html#lhon
Hlp_lise  Hlp_L_matl3.html#lise
Hlp_lmoon  Hlp_L_matl3.html#lmoon
Hlp_lore  Hlp_L_matl3.html#lore
Hlp_lpl1  Hlp_L_matl3.html#lpl1
Hlp_lpl10  Hlp_L_matl3.html#lpl10
Hlp_lpl11  Hlp_L_matl3.html#lpl11
Hlp_lpl12  Hlp_L_matl3.html#lpl12
Hlp_lpl2  Hlp_L_matl3.html#lpl2
Hlp_lpl3  Hlp_L_matl3.html#lpl3
Hlp_lpl4  Hlp_L_matl3.html#lpl4
Hlp_lpl5  Hlp_L_matl3.html#lpl5
Hlp_lpl6  Hlp_L_matl3.html#lpl6
Hlp_lpl7  Hlp_L_matl3.html#lpl7
Hlp_lpl8  Hlp_L_matl3.html#lpl8
Hlp_lpl9  Hlp_L_matl3.html#lpl9
Hlp_lrig  Hlp_L_matl3.html#lrig

### post26?
Hlp_lcom Hlp_smas, Hlp_yeoh MPC184 p26calc, p26export,p26plot"

  (interactive "P")
  ;; we must change the path for elements!
  (let (file path (command "aadd") prefix)
    (if arg
	(setq command (completing-read "Browse help for keyword: "
				       ansys-help-index))
      (save-excursion
	(backward-word)
	(search-forward-regexp "[[:word:]]+")
	(setq command (match-string 0))
	(assoc-string command ansys-help-index t))
    (message command)
    (setq file (nth 1 (assoc-string command ansys-help-index t)))
    (unless  file
      (error "Command %s not found in keyword list" command))
    (message "Help file: %s" file)
    (if (string-match "_C_" file)
	(setq prefix "ans_cmd/")
	(setq prefix "ans_elem/"))
    (cond
     ((ansys-is-unix-system-p)
      (setq path (concat ansys-install-directory
			 "ansys_inc/v" ansys-current-ansys-version
			 "/commonfiles/help/en-us/help/"))
      (start-process "help_browser" nil "chromium-browser" (concat path prefix file)))
     ((string= system-type "windows-nt")
      (if (fboundp 'w32-shell-execute)
	  (setq path (concat( ansys-install-directory
			      "Ansys Inc\\v" ansys-current-ansys-version
			      "\\commonfiles\\help\\en-us\\help\\")))
	;; wrapper of ShellExecute MS-Windows API
	(w32-shell-execute "Open" (concat path prefix file))
	(error "Emacs cannot find w23-shell-execute")))
     (t
      (error "Can only start the ANSYS help on Windows and UNIX systems")))))


;; ;; TODO: this function is supposedly obsolete with Emacs 23.2
;; (defun ansys-kill-buffer-query-function ()
;;   (if (or (string= (process-status (get-process ansys-process-name)) "run")
;; 	  (string= (process-status (get-process ansys-process-name)) "stop"))
;;       (yes-or-no-p "ANSYS process is active, quit buffer anyway? ")
;;     t))

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
  (let ((status (process-status ansys-process-name)))
    (if status
	(message "ANSYS process is in state \"%s\"" ;, process identification No: %d"
		 (symbol-name status))
      (message "No ANSYS interpreter process is running."))
	   ;; (process-id (get-process ansys-process-name))
    ))

(defun ansys-license-status ()		;NEW
  "Display the ANSYS license status or start the license tool.
For Unix systems show the status in a separate buffer, under
Windows start the anslic_admin.exe utility, which has a button
for displaying the license status."
  (interactive)
  (ansys-lmutil-program "")  ;check whether program is found on system
  (cond
   ((ansys-is-unix-system-p)
    (ansys-license-file-check)
;    (ansys-ansysli-servers-check)
    (message "Retrieving license status, please wait...")
    (with-current-buffer (get-buffer-create "*ANSYS-licenses*")
      (delete-region (point-min) (point-max)))
    ;; syncronous call
    (call-process ansys-lmutil-program nil "*ANSYS-licenses*" nil "lmstat" "-c "  ansys-license-file  "-a")
    (let (bol eol)
      (with-current-buffer "*ANSYS-licenses*"
	;; remove unintersting licenses
	;; (goto-char (point-min))
	;; (delete-matching-lines "\\<acfx\\|\\<ai\\|\\<wbunix\\|\\<rdacis\\>")

	(goto-char (point-min))
	(while (not (eobp))
	  (push-mark (point))
	  (search-forward-regexp "Users of " nil t)
	  (beginning-of-line)
	  (delete-region (mark) (point))
	  (forward-line 1))
	(goto-char (point-max))
	(push-mark (point))
	(search-backward-regexp "Users of " nil t)
	(forward-line 1)
	(delete-region (mark) (point))

	;; remove empty lines
	(goto-char (point-min))
	(delete-matching-lines "^$")

	;; shorten lines
	(goto-char (point-min))
	(while (re-search-forward "Total of \\|Users of " nil t)
	  (replace-match ""))

	;; sorting
	(sort-lines nil (point-min) (point-max))

	;; add some comments
	(goto-char (point-min))
	(insert (propertize
		 (concat " -*- License status from " ansys-license-file
		 " -*-\n") 'face 'match))

	(goto-char (point-max))
	(insert "\n")
	(insert (propertize (concat (current-time-string) "\n")
			    'face 'match))
	;; higlight current -license-type
	(goto-char (point-min))
	(search-forward-regexp (concat "\\<" ansys-license ":") nil t)
	(forward-line)
	(setq eol (point))
	(forward-line -1)
	(setq bol (point))
	(put-text-property bol eol 'face 'font-lock-warning-face)
	;; (set-window-point (get-buffer-window "*ANSYS-licenses*") (point))
	))
    (display-buffer "*ANSYS-licenses*" 'otherwindow)
    (message "Updated license status: %s." (current-time-string)))
   ((string= system-type "windows-nt")
    (if (fboundp 'w32-shell-execute)
	(let ((version (if (boundp 'ansys-current-ansys-version)
			   ansys-current-ansys-version
			 "140")))
	    (cond ((> (string-to-number version) 130)
	       (w32-shell-execute nil ansys-lmutil-program "-client140"))
	      ((< (string-to-number version) 120)
	       (w32-shell-execute nil ansys-lmutil-program))
	      (t
	       (w32-shell-execute nil ansys-lmutil-program "-client")))))
    (message "Loading the anslic_admin program..."))
   (t
    (error "No license status available on %s" system-type))))

;; starting in GUI mode (/menu,on) does inhibit the process intercommunication
;; => /menu,graph
;; env variable ANS_CONSEC=YES disables dialog boxes
(defun ansys-start-graphics ()		;NEW
  "Start the ANSYS display in interactive mode."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (progn (comint-send-string (get-process ansys-process-name)
		      ;; "/show,X11c\n/menu,grph\n"
		      "/show,3d\n/menu,grph\n"
		      )
	 (display-buffer "*ANSYS*" 'other-window)))

(defun ansys-start-pzr-box ()		;NEW PanZoomRotate box
  "Start the ANSYS Pan/Zoom/Rotate dialog box in interactive mode."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (comint-send-string (get-process ansys-process-name) "/ui,view\n")
  (display-buffer "*ANSYS*" 'other-window))

(defun ansys-iso-view (arg)
  "Show current display in isometric view (/view,,1,1,1)."
  (interactive "p")
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (comint-send-string (get-process ansys-process-name) "/view,,1,1,1\n/replot\n")
  (display-buffer "*ANSYS*" 'other-window))

(defun ansys-move-up (arg)
  "Move geometry up ARG steps in the graphics window.
A Negative ARG moves ARG steps down."
  (interactive "p")
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (comint-send-string (get-process ansys-process-name)
		      (format "/focus,,,-0.25*(%d),,1\n/replot\n" arg))
  (display-buffer "*ANSYS*" 'other-window))

(defun ansys-move-down (arg)
  "Move geometry down ARG steps in the graphics window.
A Negative ARG moves ARG steps up."
  (interactive "p")
  (ansys-move-up (- arg)))

(defun ansys-move-right (arg)
  "Move geometry right ARG steps in the graphics window.
A Negative ARG moves ARG steps left."
  (interactive "p")
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (comint-send-string (get-process ansys-process-name)
		      (format "/focus,,-0.25*(%d),,,1\n/replot\n" arg))
  (display-buffer "*ANSYS*" 'other-window))

(defun ansys-move-left (arg)
  "Move geometry left ARG steps in the graphics window.
A Negative ARG moves ARG steps right."
  (interactive "p")
  (ansys-move-right (- arg)))

(defun ansys-zoom-in ()
  "Zoom into the graphics window."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (comint-send-string (get-process ansys-process-name) "/dist,,.7,1\n/replot\n") ;valid in any processor
  (display-buffer "*ANSYS*" 'other-window)  )

(defun ansys-zoom-out ()
  "Zoom out of the graphics window."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (comint-send-string (get-process ansys-process-name) "/dist,,1.4,1\n/replot\n") ;valid in any processor
  (display-buffer "*ANSYS*" 'other-window)  )

(defun ansys-replot ()			;NEW_C
  "Replot the ANSYS interactive graphics screen."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (comint-send-string (get-process ansys-process-name) "/replot\n") ;valid in any processor
  (display-buffer "*ANSYS*" 'other-window))

(defun ansys-fit ()			;NEW_C
  "Fit FEA entities to the ANSYS interactive graphics screen."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (comint-send-string (get-process ansys-process-name) "/dist\n/replot\n") ;valid in any processor
  (display-buffer "*ANSYS*" 'other-window))

(defun ansys-program ( exec)			;NEW
  "Change the ANSYS executable name to EXEC.
And set the variable `ansys-program' accordingly if the for
executable EXEC can be found on the system's search path."
  (interactive "FANSYS interpreter executable: ")
  (when (string= exec "")
    (setq exec ansys-program))
  (setq ansys-program exec)
  (if (executable-find exec)
      (message "ansys-program is set to \"%s\"." ansys-program)
    (error "Cannot find ANSYS interpreter executable \"%s\" on the system" exec)))

(defun ansys-help-program ( exec)			;NEW
  "Change the ANSYS help executable to EXEC and check for its existence.
And store the value EXEC in the variable `ansys-help-program'."
  (interactive "FANSYS help executable: ")
  (when (string= exec "")
    (setq exec ansys-help-program))
  (setq ansys-help-program exec)
  (if (executable-find exec)
      (message "ansys-program is set to \"%s\"." exec)
    (error "Cannot find the ANSYS help executable \"%s\" on the system" exec)))

(defun ansys-lmutil-program ( exec)		;NEW
  "Change the ANSYS license management utility executable to EXEC.
And specify it in the variable `ansys-lmutil-program'.  The
function inserts the string `default-directory' in the prompt
when the variable `insert-default-directory' is not nil.  For
Lin64 it is the 'lmutil' executable
/ansys_inc/shared_files/licensing/linx64/lmutil.  For Windows the
anslic_admin utility: `C:\\Ansys Inc\\Shared
Files\\licensing\\win64\\anslic_admin.exe'"
  (interactive "FANSYS License Management Utility executable: ")
  (when (string= exec "")		;use default
    (setq exec ansys-lmutil-program))
  (setq ansys-lmutil-program exec)
  (if (executable-find exec)
      (message "ansys-lmutil-program is set to \"%s\"." ansys-lmutil-program)
  (error "Cannot find ANSYS LM Utility executable \"%s\" on the
    system" exec)))

;;;###autoload
(defun ansys-job ()			;NEW
  "Change the ANSYS job name.
And write it into the variable `ansys-job'."
  (interactive)
  (let ((job-name))
    (if ansys-job
	(setq job-name (read-string "job name: " ansys-job))
      (setq job-name (read-string "job name: ")))
    (if (string= job-name "")
	(error "job-name must not be the empty string")
      (message (concat "Job name is set to \"" ansys-job "\".")))
    (setq ansys-job job-name)))

(defun ansys-no-of-processors ()
  "Change the No of processors to use for an Anys run.
The number of processors will be put into the integer
`ansys-no-of-processors'.  If this number is below 3 the variable
won't affect the run definition since the default No of
processors (if available) for a structural analysis in ANSYS is
2."
  (interactive)
  (let ((no-string (number-to-string ansys-no-of-processors))
	no
	query
	s)
    (setq query (concat "Put in the No of processors to use [" no-string "]: ")
	  s (read-string query nil nil no-string)
	  no (string-to-number s))
    (if (integerp no)
	(setq ansys-no-of-processors no)
      (error "Specified number is not an integer"))
    (message "No of processors for the next run definiton is %d" ansys-no-of-processors)))

(defun ansys-license-file-check ()
  "Return t if ANSYS license file (server) information is found.
Checks whether the variable `ansys-license-file' is set, if not
sets its value to the environment variable ANSYSLMD_LICENSE_FILE
or LM_LICENSE_FILE, in this order of precedence.  When the former
are not available return nil."
  (cond
   (ansys-license-file
    (setenv "ANSYSLMD_LICENSE_FILE" ansys-license-file)
    (message "Set process environment variable ANSYSLMD_LICENSE_FILE to ansys-license-file")
    t)
   ((getenv "ANSYSLMD_LICENSE_FILE")	;need this for -license-status
    (setq ansys-license-file (getenv "ANSYSLMD_LICENSE_FILE"))
    (message "Set ansys-license-file from process environment variable ANSYSLMD_LICENSE_FILE")
    t)
   ((getenv "LM_LICENSE_FILE")
    (setq ansys-license-file (getenv "LM_LICENSE_FILE"))
    (message "Set ansys-license-file from process environment variable LM_LICENSE_FILE")
    t)
   (t
    nil)))

(defun ansys-ansysli-servers-check ()
  "Return t if ANSYS interconnect server information is found.
Checking whether the variable `ansys-ansysli-servers' is set or
otherwise the environment variable ANSYSLI_SERVERS.  If neither
is set return nil"
  (interactive)
  (cond
   (ansys-ansysli-servers
    (setenv "ANSYSLI_SERVERS" ansys-ansysli-servers)
    (message "Set process environment variable ANSYSLI_SERVERS to ansys-ansysli-servers")
    t)
   ((getenv "ANSYSLI_SERVERS")
    (setq ansys-ansysli-servers (getenv "ANSYSLI_SERVERS"))
    (message "Read ansys-ansysli-servers from process environment
    variable ANSYSLI_SERVERS") t)
   (t nil)))

(defun ansys-license-file ( file)		;NEW
  "Change the ANSYS license file name or license server(s).
And specify the string FILE in the variable `ansys-license-file'
which can either be the license file name or license server(s)
specification.  The server specification should include the port
number for the lmutil tool even when it's 1055, the default port
number: port_number@server_name, multiple server names are
separated by a colon, for example
\"27005@rbgs421x:27005@rbgs422x:...\"."
  (interactive "sLicense server or license file :")
  (cond ((string= file "")
	 (ansys-license-file-check))
	(t
	 (setq ansys-license-file file)
	 (message (concat "Set ansys-license-file to \""
			  ansys-license-file "\".")))))

;; (error "Please specify the license server information with
;;     the `ansys-license-file' function or either set
;;     ANSYSLMD_LICENSE_FILE or LM-LICENSE-FILE environment
;;     variable")

(defun ansys-ansysli-servers ( servers)		;NEW
  "Change the ANSYS interconnect servers to SERVERS.
And specify it in the variable `ansys-ansysli-servers'.  The
server specification must include the port number even when it is
2325, the default port number: port_number@server_name, multiple
server names are separated by a colon, for example
\"rbgs421x:rbgs422x:...\"."
  (interactive "sInterconnect license server(s) :")
  (cond ((string= servers "")
	 (ansys-ansysli-servers-check))
	(t
	 (setq ansys-ansysli-servers servers)
	 (message (concat "Set ansys-ansysli-servers to \""
			  ansys-ansysli-servers "\".")))))

;; (error "Please specify the license server information with
;;     the `ansys-license-file' function or either set
;;     ANSYSLMD_LICENSE_FILE or LM-LICENSE-FILE environment
;;     variable")

(defun ansys-license ()			;NEW
  "Change the ANSYS license type to LIC.
And store it in the variable `ansys-license'."
  (interactive)
  (let ((lic (if (not (string= ansys-license ""))
		 ansys-license
	       "struct")))
    (setq ansys-license
	  (completing-read (concat "License type [" lic "] (TAB for completion): ")
			   ansys-license-types
			   nil nil nil nil lic))
    (message (concat "ANSYS license type is now set to \"" ansys-license "\"."))))


(provide 'ansys-process)

;; Local Variables:
;; mode: outline-minor
;; indicate-empty-lines: t
;; show-trailing-whitespace: t
;; word-wrap: t
;; End:
