;;; ansys-process.el -- Managing runs and processes for the Ansys mode

;; Copyright (C) 2006 - 2011  H. Dieter Wilhelm GPL V3

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

(defgroup Ansys-process nil
  "Customisation 'process' subgroup for the Ansys mode."
  :group 'Ansys)

(defcustom ansys-job "file"			;NEW_C
  "Variable storing the Ansys job name.
It is initialised to 'file' (which is also the Ansys default job
name).  See `ansys-abort-file' for a way of stopping a solver run
in a controlled way and `ansys-display-error-file' for viewing
the respective error file."
  :type 'string
  :group 'Ansys-process)

(defcustom ansys-program (concat "ansys"
		ansys-current-ansys-version)		;NEW_C
  "This variable stores the Ansys executable name.
When the file is not in your search path, you have to specify the
full qualified file name and not only the name of the executable.
For example: \"/ansys_inc/v130/ansys/bin/ansys130\" and not only
\"ansys130\".  You might customise this variable or use the
function `ansys-program' to do this for the current session
only."
  :type 'string
  :group 'Ansys-process)

(defcustom ansys-help-program (concat "anshelp"
ansys-current-ansys-version)		;NEW_C
  "The Ansys help executable.
It is called with
\\[ansys-start-ansys-help] (`ansys-start-ansys-help').  When the
executable is not in the search path, you have to complement the
executable with its complete path.  For example the default
locations are \"/ansys_inc/v130/ansys/bin/anshelp130\" on UNIX
and \"c:\\\\Program Files\\Ansys\
Inc\\v130\\commonfiles\\jre\\intel\\bin\\Javaw.exe\" on Windows
XP.  Since Ansys version 12.0 it is a java interpreter."
  :type 'string
  :group 'Ansys-process)

(defcustom ansys-help-program-parameters (concat "-cp \"c:\\Program Files\\Ansys Inc\\"
ansys-current-ansys-version "\\commonfiles\\help\" HelpDocViewer")
  "Stores parameters for the variable `ansys-help-program' under Windows.
For example: '-cp \"c:\\Program Files\\Ansys
Inc\\v130\\commonfiles\\help\" HelpDocViewer'."
  :type 'string
  :group 'Ansys-process)

(defcustom ansys-lmutil-program "lmutil"	;NEW_C
  "The FlexLM license manager utility executable name.
When the file is not in your search path, you have to furnish the
complete path.  For example:
\"/ansys_inc/shared_files/licensing/linx64/lmutil\" or in the
case of a Windows OS \"c:\\\\Program Files\\Ansys Inc\\Shared\
Files \\Licensing\\intel\\anslic_admin.exe.  This variable is
used for displaying the license status with the function
`ansys-license-status'."
  :type 'string
  :group 'Ansys-process)

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
anslic_admin is doing in an Ansys recommended installation).  5.)
The license file itself."
  :type 'string
  :group 'Ansys-process)

(defcustom ansys-ansysli-servers nil ;NEW_C
  "Used to identify the server machine for the Licensing Interconnect.
Set it to port@host.  The default port is 2325."
  :type 'string
  :group 'Ansys-process)

(defcustom ansys-license-types		;NEW_C
  '("ansys" "struct" "ane3" "ansysds" "ane3fl" "preppost")
  "List of available license types to choose for an Ansys run.
This list should contain the license types you can choose from.
Below are often used license types (as e.g. seen with the
function `ansys-license-status') and their corresponding
WorkBench terminology.

\"ansys\" - Mechanical U (without thermal capability)
\"struct\" - Structural U (with thermal capability)
\"ane3\" - Mechanical/Emag (Structural U with electromagnetics)
\"ansysds\" - Mechanical/LS-Dyna (Mechanical U with Ansys LS-Dyna inter-phase)
\"ane3fl\" - Multiphysics
\"preppost\" - PrepPost (no solving capabilities)"
  :group 'Ansys-process)

(defcustom ansys-license "struct"		;NEW_C
  "The License type with which the Ansys interpreter will be started.
See `ansys-license-types' for often used Ansys license types."
;  :options '("ansys" "struct" "ane3" "ane3fl" "ansysds" "preppost")
  :options ansys-license-types
  ;; options not available for strings (only hooks, alists, plists E22)
  :type 'string
  :group 'Ansys-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- constants ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ansys-process-name "Ansys"		;NEW_C
  "Variable containing Emacs' name for an Ansys process.
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
  "Writes an Ansys abort file for stopping the current run.
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
		  (if (boundp' ansys-process-name)
		      ansys-process-name
		    "Ansys"))))
    ;; no-property stuff necessary?????
    ;;   (if (y-or-n-p
    ;;        (concat
    ;; 	"Start this Ansys run: (lic: " ansys-license ", job: " ansys-job ")? "))
    ;;       (message "Starting run...")
    ;;     (error "Run canceled"))
    (cond ((ansys-process-running-p)
	   (comint-send-region process (point-min) (point))
	   (display-buffer (process-buffer process) 'other-window))
	  (t
	   (kill-ring-save (point-min) (point))	;point-min is heeding narrowing
	   (message "Copied from beginning of buffer to cursor.")))))

(defun ansys-send-to-ansys ( &optional stay)	;NEW
  "Send region (or code line) to the Ansys interpreter, otherwise copy it.
Argument BEG may the beginning of the region.  If there is no
region active send/copy the complete code line, if the cursor is
in no code line (like a comment) go to the next code line and
indicate an error.  When there is no running Ansys interpreter process
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
			       (concat code "\n"))
	   (display-buffer "*Ansys*" 'other-window))
	  (t
	   (kill-ring-save beg end)
	   (if region
	       (message "Copied region.")
	     (message "Copied code line."))))))

(defun ansys-process-running-p ()
  "Return nil if no Ansys interpreter process is running."
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
    (error "No Ansys process is running"))
  (let ((s (read-string "Send to interpreter: ")))
    (comint-send-string (get-process
			 (if (boundp 'ansys-process-name)
			     ansys-process-name)) (concat s "\n"))
    ;;  (walk-windows
    ;;    (lambda (w)
    ;;      (when (string= (buffer-name (window-buffer w)) "*Ansys*")
    ;;        (with-selected-window w (goto-char (point-max))))))
    ;; (setq mode-line-process (format ":%s" (process-status ansys-process)))
    ;; (force-mode-line-update)
    (display-buffer "*Ansys*" 'other-window)))

(require 'comint)

(defun ansys-start-ansys ()		;NEW
  "Start the Ansys interpreter process."
  (interactive)
  (let (ansys-process-buffer)
    (when (ansys-process-running-p)
      (error "A Interpreter is already running under Emacs"))
    (message "Preparing an Ansys interpreter start...")
    ;; (setq comint-use-prompt-regexp t) TODO: ???
    (ansys-program "")			;take exec from -program var.
    (ansys-license-file "")		;take file from license-file or env.
    (if (y-or-n-p
	 (concat
	  "Start run?  (l-type: " (if (boundp 'ansys-license) ansys-license)
	  ", job: " (if (boundp 'ansys-job) ansys-job)
	  " in " default-directory ", server: " ansys-license-file ")"))
	(message "Starting the Ansys interpreter...")
      (error "Function ansys-start-ansys canceled"))
    (setq ansys-process-buffer
	  (make-comint ansys-process-name ansys-program nil
;		       (concat "-np 8 -p " ansys-license " -j " ansys-job)))
		       (concat "-p " ansys-license " -j " ansys-job)))
    ;;  (comint-send-string (get-process ansys-process-name) "\n")
    (display-buffer ansys-process-buffer 'other-window)
    ;;  (switch-to-buffer ansys-process-buffer)
    (other-window 1)
    (setq comint-prompt-regexp "BEGIN:\\|PREP7:\\|SOLU_LS[0-9]+:\\|POST1:\\|POST26:\\|RUNSTAT:\\|AUX2:\\|AUX3:\\|AUX12:\\|AUX15:")
    (font-lock-add-keywords nil (list comint-prompt-regexp))

	  ;; comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom comint-watch-for-password-prompt comint-truncate-buffer)
  ))

(defun ansys-kill-ansys ()		;NEW
  "Kill the current Ansys run under Emacs.
The function asks for confirmation before actually killing the
process.  Warning: Ansys writes a lock file (jobname.lock) if the
process is killed and not regularly exited.  You should prefere
the function `ansys-exit-ansys'."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No active Ansys solver process"))
  (if (yes-or-no-p
       "Do you want to kill the running Ansys solver?")
      (progn
;	(message "Killing run...")
	(delete-process (get-process ansys-process-name))
	(message "Killing Ansys run...done.")
	(display-buffer "*Ansys*" 'otherwindow)
	;; (setq mode-line-process (format ":%s" (process-status (get-process ansys-process-name))))
	;; (force-mode-line-update)
	)
    (message "Killing of Ansys run canceled.")))

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
	(process-send-string (get-process ansys-process-name) "finish $ /exit,all\n"))
	;; (setq mode-line-process (format ":%s" (process-status ansys-process)))
	;; (force-mode-line-update))
    (error "Exiting of Ansys run canceled")))

;;;###autoload
(defun ansys-start-ansys-help ()       ;NEW_C
  "Start the Ansys help system.
Alternatively under a Unix system, one can also use the Ansys
command line \"/SYS, anshelp130\" when running Ansys
interactively, provided that anshelp130 is found in the search
paths for executables (these are stored in the PATH environment
variable)."
  (interactive)
  (ansys-help-program "")
  (progn
    (message "Starting the Ansys help browser...")
    (cond
     ((ansys-is-unix-system-p)
      (start-process "Ansys-help-program" nil ansys-help-program))
     ((string= system-type "windows-nt")
      (if (fboundp 'w32-shell-execute)
	  (w32-shell-execute "Open" (concat "\"" ansys-help-program "\"")
			 ansys-help-program-parameters)))  ;HINT: Eli Z.,M. Dahl
     (t
      (error "Can only start the Ansys help on Windows and UNIX systems")))))

;; ;; TODO: this function is supposedly obsolete with Emacs 23.2
;; (defun ansys-kill-buffer-query-function ()
;;   (if (or (string= (process-status (get-process ansys-process-name)) "run")
;; 	  (string= (process-status (get-process ansys-process-name)) "stop"))
;;       (yes-or-no-p "Ansys process is active, quit buffer anyway? ")
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
	(message "Ansys process is in state \"%s\"" ;, process identification No: %d"
		 (symbol-name status))
      (message "No Ansys interpreter process is running."))
	   ;; (process-id (get-process ansys-process-name))
    ))

(defun ansys-license-status ()		;NEW
  "Display the Ansys license status or start the license tool.
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
    (with-current-buffer (get-buffer-create "*Ansys-licenses*")
      (delete-region (point-min) (point-max)))
    ;; syncronous call
    (call-process ansys-lmutil-program nil "*Ansys-licenses*" nil "lmstat" "-c "  ansys-license-file  "-a")
    (let (bol eol)
      (with-current-buffer "*Ansys-licenses*"
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
	;; (set-window-point (get-buffer-window "*Ansys-licenses*") (point))
	))
    (display-buffer "*Ansys-licenses*" 'otherwindow)
    (message "Updated license status: %s." (current-time-string)))
   ((string= system-type "windows-nt")
    (if (fboundp 'w32-shell-execute)
	(w32-shell-execute nil ansys-lmutil-program))
    (message "Loading the anslic_admin program..."))
   (t
    (error "No license status available on %s" system-type))))

;; starting in GUI mode (/menu,on) does inhibit the process intercommunication
;; => /menu,graph
;; env variable ANS_CONSEC=YES disables dialog boxes
(defun ansys-start-graphics ()		;NEW
  "Start the Ansys display in interactive mode."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No Ansys process is running"))
  (progn (comint-send-string (get-process ansys-process-name)
		      ;; "/show,X11c\n/menu,grph\n"
		      "/show,3d\n/menu,grph\n"
		      )
	 (display-buffer "*Ansys*" 'other-window)))

(defun ansys-start-pzr-box ()		;NEW PanZoomRotate box
  "Start the Ansys Pan/Zoom/Rotate dialog box in interactive mode."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No Ansys process is running"))
  (comint-send-string "*Ansys*";(get-process ansys-process-name)
		      "/ui,view\n") ;valid in any processor
  (display-buffer "*Ansys*" 'other-window))

(defun ansys-move-up (arg)
  "Move geometry up ARG steps in the graphics window.
A Negative ARG moves ARG steps down."
  (interactive "p")
  (unless (ansys-process-running-p)
    (error "No Ansys process is running"))
  (comint-send-string (get-process ansys-process-name)
		      (format "/focus,,,-0.25*(%d),,1\n/replot\n" arg))
  (display-buffer "*Ansys*" 'other-window))

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
    (error "No Ansys process is running"))
  (comint-send-string (get-process ansys-process-name)
		      (format "/focus,,-0.25*(%d),,,1\n/replot\n" arg))
  (display-buffer "*Ansys*" 'other-window))

(defun ansys-move-left (arg)
  "Move geometry left ARG steps in the graphics window.
A Negative ARG moves ARG steps right."
  (interactive "p")
  (ansys-move-right (- arg)))

(defun ansys-zoom-in ()
  "Zoom into the graphics window."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No Ansys process is running"))
  (comint-send-string (get-process ansys-process-name) "/dist,,.7,1\n/replot\n") ;valid in any processor
  (display-buffer "*Ansys*" 'other-window)  )

(defun ansys-zoom-out ()
  "Zoom out of the graphics window."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No Ansys process is running"))
  (comint-send-string (get-process ansys-process-name) "/dist,,1.4,1\n/replot\n") ;valid in any processor
  (display-buffer "*Ansys*" 'other-window)  )

(defun ansys-replot ()			;NEW_C
  "Replot the Ansys interactive graphics screen."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No Ansys process is running"))
  (comint-send-string (get-process ansys-process-name) "/replot\n") ;valid in any processor
  (display-buffer "*Ansys*" 'other-window))

(defun ansys-fit ()			;NEW_C
  "Fit FEA entities to the Ansys interactive graphics screen."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No Ansys process is running"))
  (comint-send-string (get-process ansys-process-name) "/dist\n/replot\n") ;valid in any processor
  (display-buffer "*Ansys*" 'other-window))

(defun ansys-program ( exec)			;NEW
  "Change the Ansys executable name to EXEC.
And set the variable `ansys-program' accordingly if the for
executable EXEC can be found on the system's search path."
  (interactive "FAnsys interpreter executable: ")
  (when (string= exec "")
    (setq exec ansys-program))
  (setq ansys-program exec)

  (if (executable-find exec)
      (message "ansys-program is set to \"%s\"." ansys-program)
    (error "Cannot find Ansys interpreter executable \"%s\" on the system" exec)))

(defun ansys-help-program ( exec)			;NEW
  "Change the Ansys help executable to EXEC and check for its existence.
And store the value EXEC in the variable `ansys-help-program'."
  (interactive "FAnsys help executable: ")
  (when (string= exec "")
    (setq exec ansys-help-program))
  (setq ansys-help-program exec)
  (if (executable-find exec)
      (message "ansys-program is set to \"%s\"." exec)
    (error "Cannot find the Ansys help executable \"%s\" on the system" exec)))

(defun ansys-lmutil-program ( exec)		;NEW
  "Change the Ansys license management utility executable to EXEC.
And specify it in the variable `ansys-lmutil-program'.  The
function inserts the string `default-directory' in the prompt
when the variable `insert-default-directory' is not nil.  For
Lin64 it is the 'lmutil' executable
/ansys_inc/shared_files/licensing/linx64/lmutil.  For Windows the
anslic_admin utility: `C:\\Ansys Inc\\Shared
Files\\licensing\\win32\\anslic_admin.exe'"
  (interactive "FAnsys License Management Utility executable: ")
  (when (string= exec "")		;use default
    (setq exec ansys-lmutil-program))
  (setq ansys-lmutil-program exec)

  (if (executable-find exec)
      (message "ansys-lmutil-program is set to \"%s\"." ansys-lmutil-program)
  (error "Cannot find Ansys LM Utility executable \"%s\" on the
    system" exec)))

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


(defun ansys-license-file-check ()
  "Return t if Ansys license file (server) information is found.
Checks whether the variable `ansys-license-file' is set, if not
sets its value to the environment variable ANSYSLMD_LICENSE_FILE
or LM_LICENSE_FILE, in this order of precedence.  When also these
are not available returns an error."
  (cond
   (ansys-license-file
    (setenv "ANSYSLMD_LICENSE_FILE" ansys-license-file)
    t)
   ((getenv "ANSYSLMD_LICENSE_FILE")
    (setq ansys-license-file (getenv "ANSYSLMD_LICENSE_FILE"))
    t)
   ((getenv "LM_LICENSE_FILE")
    (setq ansys-license-file (getenv "LM_LICENSE_FILE"))
    t)
   (t
    (error "Please specify the license server information in the
    `ansys-license-file' variable or set an environment variable
    either ANSYSLMD_LICENSE_FILE or LM-LICENSE-FILE"))))

(defun ansys-ansysli-servers-check ()
  "Return t if Ansys interconect server information is found.
Checks whether the variable `ansys-ansysli-servers' is set or
uses the environment variable ANSYSLI_SERVERS for it."
  (interactive)
  (cond
   (ansys-ansysli-servers
    (setenv "ANSYSLI_SERVERS" ansys-ansysli-servers)
    t)
   ((getenv "ANSYSLI_SERVERS")
    (setq ansys-ansysli-servers (getenv "ANSYSLI_SERVERS"))
    t)
   (t
    (error "Please specify the interconnect server information in
    the `ansys-ansysli-servers' variable or set the environment
    variable ANSYSLI_SERVERS"))))

(defun ansys-license-file ( file)		;NEW
  "Change the Ansys license file name or license server(s).
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

(defun ansys-ansysli-servers ( servers)		;NEW
  "Change the Ansys interconnect servers to SERVERS.
And specify it in the variable `ansys-ansysli-servers'.  The
server specification must include the port number when it isn't
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


(provide 'ansys-process)

;; Local Variables:
;; mode: outline-minor
;; indicate-empty-lines: t
;; show-trailing-whitespace: t
;; word-wrap: t
;; End:
