;;; ansys-process.el -- Managing runs and processes for ANSYS-Mode

;; Copyright (C) 2006 - 2015  H. Dieter Wilhelm GPL V3

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Maintainer: H. Dieter Wilhelm
;; Version: 16.1.1
;; Keywords: Languages, Convenience, ANSYS

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
	 ;; "/" the root dir is the default installation directory on GNU-Linux
	 "/")
	(t ;; the default is "C:\\Program Files\\" on Windows
	 "C:\\Program Files\\"))
  "This is the directory where ANSYS is installed."
  :type 'string
  :group 'ANSYS-process
  )

(defcustom ansys-job "file"
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
		   "161")))
  (if (string= window-system "x")
      (concat ansys-install-directory "ansys_inc/v"
	      version "/ansys/bin/ansys" version)
    (concat ansys-install-directory "Ansys Inc\\v" version "\\ansys\\bin\\winx64\\launcher" version ".exe")))
  "This variable stores the ANSYS executable name.
Under GNU-Linux this should be the solver, under Windows just the
launcher.  When the respective executable is not in your search
path, you have to specify the full qualified file name and not
only executable's name.  For example:
\"/ansys_inc/v145/ansys/bin/ansys145\" and not only \"ansys145\".
You might customise this variable or use the function
`ansys-program' to do this for the current session only."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-help-program
  (if (string= window-system "x")
      (concat ansys-install-directory "ansys_inc/v"
      ansys-current-ansys-version "/ansys/bin/anshelp"
      ansys-current-ansys-version)
    (concat ansys-install-directory "Ansys Inc\\v" ansys-current-ansys-version "\\commonfiles\\help\\HelpViewer\\ANSYSHelpViewer.exe"))
  "The ANSYS help executable.
It is called with
\\[ansys-start-ansys-help] (`ansys-start-ansys-help').  When the
executable is not in the search path, you have to complement the
executable with its complete path.  For example the default
locations are \"/ansys_inc/v161/ansys/bin/anshelp161\" on GNU-Linux
and \"c:\\\\Program Files\\Ansys\
Inc\\v161\\commonfiles\\help\\HelpViewer\\ANSYSHelpViewer.exe\" on
Windows (XP/7)."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-help-path
  (if (string= window-system "x")
      (concat ansys-install-directory "ansys_inc/v"
      ansys-current-ansys-version "/ansys/bin/anshelp"
      ansys-current-ansys-version)
    (concat ansys-install-directory "Ansys Inc\\v" ansys-current-ansys-version "\\commonfiles\\help\\HelpViewer\\ANSYSHelpViewer.exe"))
  "The ANSYS help executable.
It is called with
\\[ansys-start-ansys-help] (`ansys-start-ansys-help').  When the
executable is not in the search path, you have to complement the
executable with its complete path.  For example the default
locations are \"/ansys_inc/v161/ansys/bin/anshelp161\" on GNU-Linux
and \"c:\\\\Program Files\\Ansys\
Inc\\v161\\commonfiles\\help\\HelpViewer\\ANSYSHelpViewer.exe\" on
Windows (XP/7)."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-help-program-parameters ""
  "Stores parameters for the program `ansys-help-program' under Windows.
In ANSYS version 150 not longer necessary."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-lmutil-program
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

(defcustom ansys-license-file nil
  "The FlexLM license file name or license server specification(s).
The license server specification(s) should include the port
number even if it's the default port 1055 because the lmutil tool
needs it in the following way: port_number@server_name, use the
colon for multiple servers, for example
\"27005@rbgs421x:27005@rbgs422x\".

Setting this variable skips the effect of previously set
environment variables, which have the following order of
precedence: 1. ANSYSLMD_LICENSE_FILE environment variable, 2.)
The FLEXlm resource file: ~/.flexlmrc on GNU-Linux or somewhere in the
Windows registry. 3.) The LM_LICENSE_FILE variable. 4.) The
ansyslmd.ini file in the licensing directory (This is what
anslic_admin is doing in an ANSYS recommended installation).  5.)
The license file itself."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-ansysli-servers nil
  "Used to identify the server machine for the Licensing Interconnect.
Set it to port@host.  The default port is 2325."
  :type 'string
  :group 'ANSYS-process)

(defcustom ansys-license-types
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

(defcustom ansys-license "struct"
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

(defcustom ansys-blink-delay .3
  "Number of seconds to highlight the evaluated region."
  :group 'ANSYS-process
  :type 'number)

(defcustom ansys-blink-region-flag t
  "Non-nil means highlight the evaluated region."
  :group 'ANSYS-process
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- constants ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ansys-process-name "ANSYS"
  "Variable containing Emacs' name for an ANSYS process.
Variable is only used internally in the mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- functions ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ansys-start-classics ()
  "Start the Ansys Classics user interface."
  (interactive)
;    (ansys-program "")		 ;take exec from -program var.
;    (ansys-license-file "")	 ;
;    (ansys-ansysli-servers "")	 ;
    ;(ansys-license "")		 ;

    (if (y-or-n-p
	 (concat
	  "Start run?  version: "
	  ansys-current-ansys-version
	  ", license type: " ansys-license
	  ;; "Start run?  (license type: " (if (boundp
	  ;; 'ansys-license) ansys-license)
	  (if (>= ansys-no-of-processors 3)
	      (concat ", No of processors: " (number-to-string ansys-no-of-processors))
	    "")
	  ", job: " (if (boundp 'ansys-job) ansys-job)
	  " in " default-directory ", server: " ansys-license-file ")"))
	(message "Starting the ANSYS Classics GUI...")
      (error "Starting ANSYS Classics canceled"))
    ;; -d : device
    (start-process "GUI" "*ANSYS GUI*" "/appl/ansys_inc/v161/ansys/bin/ansys161" "-p ansys " "-d 3d " "-g")
    (process-running-child-p "*ANSYS GUI*")
    (process-status "*ANSYS GUI*")

    (process-send-string "*ANSYS GUI*" "/prep7")
    (process-send-string "*ANSYS GUI*" "y\n")
    (process-send-string "*ANSYS GUI*" "\r")
    (process-send-string "*ANSYS GUI*" "/exit")

    )

(defun ansys-write-abort-file ( filename)
  "Open file FILENAME, clear it's contents and insert \"nonlinear\"."
  (find-file filename)
  (delete-region (point-min) (point-max))
  (insert "nonlinear\n")
  (save-buffer)
  (message "Wrote \"%s\" into \"%s\"." filename default-directory))

;;;###autoload
(defun ansys-abort-file (&optional arg)
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
(defun ansys-display-error-file ()
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

(defun ansys-copy-or-send-above	()
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
	   (display-buffer-other-frame (process-buffer process)))
	  (t
	   (kill-ring-save (point-min) (point))	;point-min is heeding narrowing
	   (message "Copied from beginning of buffer to cursor.")))))

(defvar  ansys-current-region-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  'highlight)
    overlay)
  "The overlay for highlighting currently evaluated region or line.")

(defun ansys-blink-region (start end)
  "Let the region blink between START and END."
  (when ansys-blink-region-flag
    (move-overlay ansys-current-region-overlay start end)
    (run-with-timer ansys-blink-delay nil
                    (lambda ()
                      (delete-overlay ansys-current-region-overlay)))))

(defun ansys-send-to-ansys ( &optional move)
  "Send a region to the ANSYS interpreter,
if the interpreter is not active, just copy it.  If there is no
region marked, send (or copy) the current paragraph.  With a
prefix argument MOVE equal to \"4\" or \"C-u\" skip to the next
code line after this region (or paragraph)."
  (interactive "p")
  (let (code
	beg
	end
        (point (point))
	(process (get-process
		  (if (boundp 'ansys-process-name) ansys-process-name)))
	(region (and transient-mark-mode mark-active)))
;    	(region (region-active-p))) ;this is for Emacs-23.1
    ;; make a valid region if possible, when region is not active:
    ;; "region" will be the whole code line (including \n)
    (unless region
      (mark-paragraph))
    (setq beg (region-beginning)
	  end (region-end))
    ;; invalidate region
    (deactivate-mark)			;for Emacs 23.1 no arguments
    ;; (deactivate-mark nil)
    (ansys-blink-region beg end)
    ;; send or copy region or line
    (cond ((ansys-process-running-p)
	   (setq code (buffer-substring-no-properties beg end))
	   (comint-send-string process
			       (concat code ""); "\n"); why did I do \n?
			       )
	   (display-buffer-other-frame "*ANSYS*")
	   (message "Sent region to solver."))
	  (t
	   (kill-ring-save beg end)
	   (message "Copied region.")
	   ))
    (if (= move 4)
	(progn
	  (goto-char end)
	  (ansys-next-code-line))
      (goto-char point))))

(defun ansys-send-to-ansys-and-proceed ( &optional stay)
  "Send a region or code line to the ANSYS interpreter.
When there is no running ANSYS interpreter process just copy the
respective region or code line to the system clipboard and skip
to the subsequent code line.  With a prefix argument STAY of
\"4\" or \"C-u\" copy or send the code and remain at the current
cursor position. The command can be repeated by typing just the
final character \"j\" (or \"C-j\")."
  (interactive "p")
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
    (unless (= stay 4)
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
	   ;; Issue a hint to the user, if the echo area isn't in use.
	   (unless (current-message)
	     (message "(Type \"j\" or \"C-j\" to repeat function.)"))
	   (display-buffer-other-frame "*ANSYS*"))
	  (t
	   (kill-ring-save beg end)
	   (if region
	       (message "Copied region.")
	     (message "Copied code line."))))
    ;; TODO set-transient-map not defined in 23.1
    (if (fboundp 'set-transient-map)
	(set-transient-map
	 (let ((map (make-sparse-keymap)))
	   (define-key map "j"
	     'ansys-send-to-ansys-and-proceed)
	   (define-key map "\C-j"
	     'ansys-send-to-ansys-and-proceed)
	   map)))))

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

(defun ansys-query-ansys-command ( &optional arg)
  "Ask for a string which will be sent to the interpreter.
The string is completable to all current ANSYS commands and with
an optional prefix argument ARG the current command line is the
initial input."
  (interactive "P")
  (unless (ansys-process-running-p)
;    (setq mode-line-process (format ":%s" (process-status ansys-process)))
;    (force-mode-line-update)
    (error "No ANSYS process is running"))
  (let (s)
    (if arg
	(setq s (read-minibuffer "Send to interpreter: "
				 (buffer-substring-no-properties
				  (line-beginning-position)
				  (line-end-position))))
      (setq s (completing-read "Send to interpreter: "
	    ansys-help-index nil nil)))
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

;;;###autoload
(defun ansys-start-ansys ()
   "Start an ANSYS interpreter process under GNU-Linux or the launcher under Windows.
 For the interpreter process summarise the run's configuration
 first. The specified No of cores is not shown if they are chosen
 smaller than 3 (see `ansys-number-of-processors')."
   (interactive)
     (if (ansys-is-unix-system-p)
	 (let (ansys-process-buffer)
	   (when (ansys-process-running-p)
	     (error "An ANSYS interpreter is already running under Emacs"))
	   (message "Preparing an ANSYS interpreter run...")
	   ;; (setq comint-use-prompt-regexp t) TODO: ???
	   (ansys-program "")		 ;take exec from -program var.
	   (ansys-license-file "")	 ;
	   (ansys-ansysli-servers "")	 ;
					 ;(ansys-license "")		 ;

					 ; env variable: ANSYS161_WORKING_DIRECTORY or -dir command line string
					 ; (setenv "ANSYS161_WORKING_DIRECTORY" "/tmp")
					 ; (getenv "ANSYS161_WORKING_DIRECTORY")

	   (if (y-or-n-p
		(concat
		 "Start run?  (version: "
		 ansys-current-ansys-version
		 ", license type: " ansys-license
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
	   )
        ;; w32-shell-execute not know under RHEL Emacs 23.1
       (if (fboundp 'w32-shell-execute)
	   (w32-shell-execute "Open" ansys-program))))

(defun ansys-kill-ansys ()
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

(defun ansys-exit-ansys ()
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
(defun ansys-start-ansys-help ()
  "Start the ANSYS help system.
Alternatively under a GNU-Linux system, one can also use the ANSYS
command line \"/SYS, anshelp161\" when running ANSYS
interactively, provided that anshelp161 is found in the search
paths for executables (these are stored in the PATH environment
variable)."
  (interactive)
  (ansys-help-program "")		;checking
  (progn
    (cond
     ((ansys-is-unix-system-p)
      (start-process "ANSYS-help-program" nil ansys-help-program)
      (message "Started the ANSYS help browser..."))
     ((string= system-type "windows-nt")
      (if (fboundp 'w32-shell-execute)
	  (w32-shell-execute "Open" (concat "\"" ansys-help-program "\"")
			     ansys-help-program-parameters)  ;HINT: Eli Z.,M. Dahl
	(error "w32-shell-execute not bound"))
      (message "Started the ANSYS help browser..."))
     (t
      (error "Can only start the ANSYS help on Windows and GNU-Linux systems")))))

(defun ansys-search-keyword()
  "Search the code line for a valid the keyword from `ansys-help-index'.
"
  (interactive)
  (when (ansys-in-empty-line-p)
    (error "Cannot find a keyword in an empty line"))
  (let* (
	 (pt (point))
	 (re "~/*[:word:]")
	 (lbp (line-beginning-position))
	 (eolp (save-excursion (end-of-line) (point)))
	 (str (upcase (buffer-substring-no-properties
		       (save-excursion
			 (+ pt (skip-chars-backward re lbp)))
		       (save-excursion
			 (+ pt (skip-chars-forward re))))))
	 (cmpl (try-completion str ansys-help-index))
	 )
    (when (or (string= str "") (not cmpl))
      ;; we are surrounded by whities, or not on a valid keyword, try
      ;; the first command (possibly behind an comment char)
      (save-excursion
	(move-beginning-of-line 1)
	(skip-chars-forward " !")
	(setq pt (point)
	      str (upcase
		   (buffer-substring-no-properties pt
		       (+ pt (skip-chars-forward re))))))
      (setq cmpl (try-completion str ansys-help-index)))
    (cond ((stringp cmpl)		;not unique
	   cmpl)
	  ((equal cmpl nil)
	   (error "\"%s\" is not a valid keyword" str))
	  (t				;perfect match
	   str))))

(require 'browse-url)

(defun ansys-browse-ansys-help ( &optional arg)
  "Open the ANSYS help for APDL commands and element names in the default web browser.
The function is looking for the next keyword before or at the
cursor location.  If that fails the command is looking for the
keyword at the line beginning.  (This is working in a comment
line as well.)  If there is an optional argument ARG, query for a
manual search keyword input.  Besides the regular command and
element names you can also input predefined help sections or
element categories.  The use of completions is advisable, for
example: Type the character `\"' and the <TAB> key and you will
see completions of the following:

Help sections:

\"RELEASE NOTES\"
\"CONTACT TECHNOLOGY GUIDE\"
\"PARAMETRIC DESIGN LANGUAGE GUIDE\"
\"STRUCTURAL ANALYSIS GUIDE\"
\"ADVANCED ANALYSIS TECHNIQUES GUIDE\"
\"MATERIAL MODELS\"

Element categories:

\"SHELLS\" = \"ALL\"SHELLS
\"PLANES\" = \"ALL\"PLANES
\"SOLIDS\" = \"ALL\"SOLIDS

\"ALL\" -- Element categories

\"ALL\"BEAMS -- Beam elem.
\"ALL\"CIRCUS -- Electric/magnetic circuit elem.
\"ALL\"COMBINS -- Combination elem.
\"ALL\"COMBIS -- Combination elem.
\"ALL\"CONTACS -- Contact elem.
\"ALL\"CONTAS -- Contact elem.
\"ALL\"CPTS -- Coupled pore-pressure mechanical solid elem.
\"ALL\"FLUIDS -- Fluid elem.
\"ALL\"FOLLW -- Follower load elem.
\"ALL\"HFS -- High Frequency elem.
\"ALL\"HSFLDS -- Hydrostatic elem.
\"ALL\"INFINS -- Infinite Boundary/Solid elem.
\"ALL\"INTERS -- Interface magnetic/gasket/cohesive elem.
\"ALL\"LINKS -- Link radiation/conduction/convection/thermal-electric/spar/ elem.
\"ALL\"MASS -- Mass elem
\"ALL\"MATRIXS -- Matrix stiffness/damping/super elem
\"ALL\"MESHS -- Mesh facet elem.
\"ALL\"MPCS -- Structural multipoint constraint
\"ALL\"PIPES -- Pipe/Elbow elem.
\"ALL\"PLANES -- Plane elem.
\"ALL\"PRETS -- Pretension combination elem.
\"ALL\"REINF -- Reinforcing elem.
\"ALL\"ROMS -- Reduced order electrostatic-structural coupled-field elem.
\"ALL\"SHELLS -- Shell elem.
\"ALL\"SOLIDS -- Solid elem.
\"ALL\"SOLSHS -- Structural solid shell elem.
\"ALL\"SOURCS -- Magnetic Electric Current source elem.
\"ALL\"SURFS -- Surface elem.
\"ALL\"TARGES -- Target elem.
\"ALL\"TRANS -- Electromechanical solid/transducer elem.
"
  (interactive "P")
  (let (file path command)
    (if arg
	(setq command (completing-read "Browse help for keyword: "
				       ansys-help-index))
      (setq command (ansys-search-keyword)))
    (setq file (nth 1 (assoc-string command ansys-help-index t)))
    (unless  file
      (error "Keyword \"%s\" is not uniquely completable" command))
;    (message "Help file: %s" file)
    (cond
     ((ansys-is-unix-system-p)
      ;; we must adapt the path to various items!
      (cond ((string-match "_C_" file)
	     (setq file (concat "ans_cmd/" file)))
	    ((string-match "_E_" file)
	     (setq file (concat "ans_elem/" file)))
	    ((string-match "_P_APDL" file)
	     (setq file (concat "ans_apdl/" file)))
	    ((string-match "_G_AdvTOC" file)
	     (setq file (concat "ans_adv/" file)))
	    ((string-match "_G_StrTOC" file)
	     (setq file (concat "ans_str/" file)))
	    ((string-match "ans_mat.html" file)
	     (setq file (concat "ans_mat/" file)))
	    ((string-match "ctectoc.html" file)
	     (setq file (concat "ans_ctec/" file)))
	    ((string-match "ansysincrelease" file)
	     (setq file (concat "ai_rn/" file)))
	    ((string-match "ansys.theory" file)
	     (setq file (concat "ans_thry/" file)))
	    )
      (setq path ansys-help-path)
		 ;; (concat "file://" ansys-install-directory
		 ;; 	 "ansys_inc/v" ansys-current-ansys-version
		 ;; 	 "/commonfiles/help/en-us/help/"))
;;       (start-process "browser"
;; ;		     nil "chromium-browser" (concat path file)))
;; 		     nil "firefox" (concat path file)))
;; ;		     nil "xdg-open" (concat path file)))
      ;; TODO: w32-shell-execute not know under Emacs 23.1
      ;; use browse-url-default-browser!
      (if (fboundp 'browse-url-xdg-open)
	  (browse-url-xdg-open (concat path file))
	(browse-url-default-browser (concat path file))))
     ;; windows
     ((string= system-type "windows-nt")
;      (if (fboundp 'w32-shell-execute)
      (cond ((string-match "_C_" file)
	     (setq file (concat "ans_cmd\\" file)))
	    ((string-match "_E_" file)
	     (setq file (concat "ans_elem\\" file)))
	    ((string-match "_P_APDL" file)
	     (setq file (concat "ans_apdl\\" file)))
	    ((string-match "_G_AdvTOC" file)
	     (setq file (concat "ans_adv\\" file)))
	    ((string-match "_G_StrTOC" file)
	     (setq file (concat "ans_str\\" file)))
	    ((string-match "ans_mat.html" file)
	     (setq file (concat "ans_mat\\" file)))
	    ((string-match "ctectoc.html" file)
	     (setq file (concat "ans_ctec\\" file)))
	    ((string-match "ansysincrelease" file)
	     (setq file (concat "ai_rn\\" file)))
	    ((string-match "ansys.theory" file)
	     (setq file (concat "ans_thry\\" file)))
	    )
;	(error "Emacs cannot find w23-shell-execute"))
      (setq path (concat
		  "file://"
		  ansys-install-directory
			 "Ansys Inc\\v" ansys-current-ansys-version
			 "\\commonfiles\\help\\en-us\\help\\"))
      ;; wrapper of ShellExecute MS-Windows API
;      (message "file:%s path:%s" file path)
;      (w32-shell-execute "Open" (concat path file)))
      (browse-url-default-windows-browser (concat path file)))
     (t
      (error "Can only start the ANSYS help on Windows and GNU-Linux systems")))
    (message "Called html browser for keyword \"%s\"..." command)))


;; ;; TODO: this function is supposedly obsolete with Emacs 23.2
;; (defun ansys-kill-buffer-query-function ()
;;   (if (or (string= (process-status (get-process ansys-process-name)) "run")
;; 	  (string= (process-status (get-process ansys-process-name)) "stop"))
;;       (yes-or-no-p "ANSYS process is active, quit buffer anyway? ")
;;     t))

(defun ansys-process-status ()
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

(defun ansys-license-status ()
  "Display the ANSYS license status or start the license tool.
For GNU-Linux systems show the status in a separate buffer, under
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
			 "145")))
	    (cond ((> (string-to-number version) 130)
	       (w32-shell-execute nil ansys-lmutil-program "-client145"))
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
(defun ansys-start-graphics ()
  "Start the ANSYS display in interactive mode."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (progn (comint-send-string (get-process ansys-process-name)
		      ;; "/show,X11c\n/menu,grph\n"
		      "/show,3d\n/menu,grph\n"
		      )
	 (display-buffer "*ANSYS*" 'other-window)))

(defun ansys-start-pzr-box ()
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

(defun ansys-replot ()
  "Replot the ANSYS interactive graphics screen."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (comint-send-string (get-process ansys-process-name) "/replot\n") ;valid in any processor
  (display-buffer "*ANSYS*" 'other-window))

(defun ansys-fit ()
  "Fit FEA entities to the ANSYS interactive graphics screen."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No ANSYS process is running"))
  (comint-send-string (get-process ansys-process-name) "/dist\n/replot\n") ;valid in any processor
  (display-buffer "*ANSYS*" 'other-window))

(defun ansys-program ( exec)
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

(defun ansys-help-program ( exec)
  "Change the ANSYS help executable to EXEC and check for its existence.
And store the value EXEC in the variable `ansys-help-program'."
  (interactive "FANSYS help executable: ")
  (when (string= exec "")
    (setq exec ansys-help-program))
  (setq ansys-help-program exec)
  (if (executable-find exec)
      (message "ansys-program is set to \"%s\"." exec)
    (error "Cannot find the ANSYS help executable \"%s\" on the system" exec)))

(defun ansys-lmutil-program ( exec)
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
  (error "Cannot find ANSYS LM Utility executable \"%s\" on this system" exec)))

;;;###autoload
(defun ansys-job ()
  "Change the ANSYS job name.
And write it into the variable `ansys-job'."
  (interactive)
  (let ((job-name))
    (if ansys-job
	(setq job-name (read-string "job name: " ansys-job))
      (setq job-name (read-string "job name: ")))
    (if (string= job-name "")
	(error "job-name must not be the empty string")
      (message (concat "Job name is set to \"" job-name "\".")))
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
    (message "No of processors for the next run definition is %d" ansys-no-of-processors)))

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

(defun ansys-license-file ( file)
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

(defun ansys-ansysli-servers ( servers)
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

(defun ansys-license ()
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
