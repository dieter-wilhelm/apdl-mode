;;; ansys-process.el -- Managing runs and processes for ANSYS-Mode

;; Copyright (C) 2006 - 2015  H. Dieter Wilhelm GPL V3

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Maintainer: H. Dieter Wilhelm
;; Version: 162-1
;; Keywords: languages, convenience

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

(defcustom ansys-job "file"
  "Variable storing the ANSYS job name.
It is initialised to 'file' (which is also the ANSYS default job
name).  See `ansys-abort-file' for a way of stopping a solver run
in a controlled way and `ansys-display-error-file' for viewing
the respective error file."
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
;;; --- variables ---

(defvar ansys-emacs-window-id nil
  "Editing buffer's X11 window id.")

(defvar ansys-classics-window-id nil
  "The X11 window id of the ANSYS GUI or the command window.")

(defvar ansys-classics-flag nil
  "Flag dertermining whether a Classics GUI could be found.")

;;; --- constants ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ansys-process-name "MAPDL"
  "Variable containing the name of a possible MAPDL interactive process.
Variable is used internally only.")

(defconst ansys-classics-process "Classics"
  "Variable containing the name of a possible MAPDL GUI process.
Variable is used internally only.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- functions ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ansys-toggle-classics ()
  "Toogle sending output to ANSYS Classics.
Try to locate an ANSYS Classics GUI or the command dialog box and
switch output to it."
  (interactive)
  (if ansys-classics-flag
      (progn
	(setq ansys-classics-flag nil)
	(message "Disconnected from Classics."))
    (if (ansys-classics-p)
	(progn (setq ansys-classics-flag t)
	       (message "Connected to Classics."))
      (error "No ANSYS Classics window found"))))

(defun ansys-classics-p ()
  "Check whether ANSYS Classics is running.
Return nil if we can't find an MAPDL GUI."
  (let ((aID (replace-regexp-in-string
	      "\n" ""
	      (shell-command-to-string "~/a-m/X11/xGetClassicsWindow")))
	(eID (replace-regexp-in-string
	      "\n" ""
	      (shell-command-to-string "~/a-m/X11/xGetFocusWindow"))))
  (if (string= "" aID)
      ;(error "No ANSYS MAPDL window found")
      nil
    (setq ansys-emacs-window-id eID)
    (setq ansys-classics-window-id aID)
    aID)))

(defun ansys-start-classics ()
  "Start the Ansys Classics graphical user interface.
The output of the solver is captured in an Emacs buffer called
*Classics*."
  (interactive)
;    (ansys-program "")		 ;take exec from -program var.
;    (ansys-license-file "")	 ;
;    (ansys-ansysli-servers "")	 ;
    ;(ansys-license "")		 ;
    (let ((bname (concat "*"ansys-classics-process"*")))
      (when (file-readable-p (concat default-directory ansys-job ".lock"))
	(if (yes-or-no-p
	     (concat "Warning: There is a \""ansys-job".lock" "\" in " default-directory ". This might indicate that there is already a solver running.  Do you wish to kill the lock file? "))
	    (delete-file (concat ansys-job ".lock"))
	  (error "Starting the MAPDL GUI (ANSYS Classics) cancelled")))
      (if (y-or-n-p
	   (concat
	    "Start run of: "
	    ansys-program
	    ", license: " ansys-license
	    ;; "Start run?  (license type: " (if (boundp
	    ;; 'ansys-license) ansys-license)
	    (if (>= ansys-no-of-processors 3)
		(concat ", No of procs: " (number-to-string ansys-no-of-processors))
	      "")
	    ", job: " (if (boundp 'ansys-job) ansys-job)
	    " in " default-directory ", server: " ansys-license-file " "))
	  (message "Starting MAPDL in GUI mode (ANSYS Classics) ...")
	(error "Calling MAPDL solver (ANSYS Classics) cancelled"))
      ;; -d : device
      ;; -g : graphics mode
      ;; -p : license
      ;; -np: no of PROCs
      ;; -j : job
      (start-process ansys-classics-process
		     bname
		     ansys-program
		     (concat (concat " -p " ansys-license)
			     " -d 3d "
			     (concat " -j " ansys-job)
			     (concat " -np " (number-to-string ansys-no-of-processors))
			     " -g"))
    (display-buffer bname 'other-window)))

(defun ansys-start-launcher ()
  "Start the Ansys Launcher."
  (interactive)
;    (ansys-program "")		 ;take exec from -program var.
;    (ansys-license-file "")	 ;
;    (ansys-ansysli-servers "")	 ;
    ;(ansys-license "")		 ;
    (start-process "Launcher" nil ansys-launcher)
    (message "Started the ANSYS Launcher..."))

(defun ansys-start-wb ()
  "Start the Ansys WorkBench."
  (interactive)
;    (ansys-program "")		 ;take exec from -program var.
;    (ansys-license-file "")	 ;
;    (ansys-ansysli-servers "")	 ;
    ;(ansys-license "")		 ;
    (start-process "WorkBench" nil ansys-wb)
    (message "Started the ANSYS WorkBench..."))

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
it tries to guess it from the current file (for a /filname
command), if this fails the jobname is taken from the variable
`ansys-job', you can change this variable by calling the equally
named interactive function (i. e. typing \\[ansys-job]) or
setting it directly as lisp expression (i. e.  typing
\"\\[eval-expression] (setq ansys-job \"jobname\")\", where
jobname is a name of your liking but must be enclosed with double
quotes (\") to represent a lisp string).  The file jobname.abt in
the current directory contains the sole word \"nonlinear\". In
case the `default-directory' is not the working directory of your
respective job, you can change it with \"\\[cd]\"."
  (interactive "p")
  (let ((job ansys-job)
	file
	lfile
	name)
    (cond
     ((< arg 0)				;ask for job-name
      (setq name
	    (read-string
	     (concat "Job name [" job "]: ") nil nil job)))
     (t					;search for /filn
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "/filn.*,\\(\\w+\\)" nil 'noerror)
	    (setq name (match-string 1))
	  (setq name job)))))
    (setq lfile (concat name ".lock"))
    (unless (file-readable-p lfile)
      (error "No \"%s\" in %s" lfile default-directory))
    (setq file (concat name ".abt"))
    (if (yes-or-no-p (concat "Write \"" default-directory file "\"? "))
	(progn
	  (ansys-write-abort-file file)
	  (message "Wrote MAPDL stop file %s in %s." file
		   default-directory))
      (message "Writing MAPDL stop file canceled!"))))

;;;###autoload
(defun ansys-display-error-file ()
  "Open the current interpreter error file in the current working directory.
You might change the directory with \"M-x `cd'\".  The error file
name consists of the current job name and the suffix '.err'.  For
the job name the variable `ansys-job' is used. You can change the
job name interactively either with the \"\\[ansys-job]\" or in
the customisation facility (by calling `ansys-customise-ansys')."
  (interactive)
  (let ((file (concat ansys-job ".err")))
    (if (not (file-readable-p file))
	(error "ANSYS error file \"%s\" doesn't exist in %s" file (pwd))
      (find-file-read-only-other-window file)
      (goto-char (point-max))
      (auto-revert-tail-mode 1))))

(defun ansys-copy-or-send-above	()
  "Copy or send above file content to the current cursor position."
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
    (cond (ansys-classics-flag
	   (kill-ring-save (point-min) (point))
	   (ansys-send-to-classics)
	   (message "Send above file content to the Classics GUI" ))
	  ((ansys-process-running-p)
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

(defun ansys-send-to-classics ()
  "Sending clipboard content to the Classics GUI."
;  (let ((win (call-process "/home/uidg1626/script/ctrlv.sh")))
  (let ((win "otto"))
  ;  (message "return value: %s" win)
    (sleep-for .5)			;wait till user lifts CTRL!
    ;;(setq win (shell-command-to-string "/home/uidg1626/script/ctrlv.sh"))
    ;; (call-process "/home/uidg1626/script/ctrlv.sh" nil nil nil ansys-classics-window-id)
    (call-process (concat ansys-mode-install-directory
			  "X11/xPasteToWin") nil nil nil ansys-classics-window-id)
    (sleep-for .1)     ;seems to take 0.1 s for the clipboard to copy!
    ;; (call-process "/home/uidg1626/script/return.sh" nil nil nil ansys-emacs-window-id)
    (call-process (concat ansys-mode-install-directory
			  "X11/xSendReturn") nil nil nil
		  ansys-emacs-window-id ansys-classics-window-id)
    ))

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
    (cond (ansys-classics-flag
	   (kill-ring-save beg end)
	   (ansys-send-to-classics)
	   (message "Sent to Classics GUI"))
	  ((ansys-process-running-p)
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
	(region (and transient-mark-mode mark-active))
	(block (save-excursion
		 (back-to-indentation)
		 (looking-at ansys-block-begin-regexp)))
	(code (ansys-code-line-p))
	(column (current-column)))
    ;; (region (region-active-p))) ;this is for Emacs-23.1
    ;; make a valid region if possible, when region is not active:
    ;; "region" will be the whole code line (including \n)
    (message "column: %d" column)
    (cond
     (region
      (setq beg (region-beginning)
	    end (region-end)))
     (block
       (move-beginning-of-line 1)
       (setq beg (point))
       (ansys-skip-block-forward)
       (setq end (point))
       (setq region t))			;block considered a region
     (code
      (setq beg (line-beginning-position))
      (save-excursion
	(forward-line 1)
	(setq end (point))))
      (t
       (unless (= stay 4)
	 (ansys-next-code-line))
       (error "There was no active region or code line")))
    ;; move cursor to subsequent code line unless stay
    (unless (= stay 4)
      (if (and region
	       (< (point) end))
	  (exchange-point-and-mark))
      (move-to-column column)		;stay in the previous column
      (ansys-next-code-line))
    ;; invalidate region
    (setq mark-active nil)
    ;; set-transient-map since 24.4
    (when (fboundp 'set-transient-map)
	(set-transient-map
	 (let ((map (make-sparse-keymap)))
	   (define-key map "j"
	     'ansys-send-to-ansys-and-proceed)
	   (define-key map "\C-j"
	     'ansys-send-to-ansys-and-proceed)
	   map)))
    ;; send or copy region or line
    (cond (ansys-classics-flag
	   (kill-ring-save beg end)
	   (if (fboundp 'set-transient-map)
	       (if region
		   (message "Sent region, type \"j\" or \"C-j\" to sent next line or block.")
		 (message "Sent line, type \"j\" or \"C-j\" to sent next line or block."))
	     (if region
		 (message "Sent region.")
	       (message "Sent line.")))
	   (ansys-send-to-classics)
	   )
	  ((ansys-process-running-p)
	   (setq code (buffer-substring-no-properties beg end))
	   (comint-send-string process
			       (concat code ""); "\n"); why did I do \n?
			       )
	   (display-buffer-other-frame "*ANSYS*")
	   ;; Issue a hint to the user
	   (if (fboundp 'set-transient-map)
	       (if region
		   (message "Sent region, type \"j\" or \"C-j\" to sent next line or block.")
		 (message "Sent line, type \"j\" or \"C-j\" to sent next line or block."))
	     (if region
		 (message "Sent region.")
	       (message "Sent line."))))
	  (t
	   (kill-ring-save beg end)
	   (if (fboundp 'set-transient-map)
	       (if region
		   (message "Copied region, type \"j\" or \"C-j\" to copy next line or block.")
		 (message "Copied line, type \"j\" or \"C-j\" to copy next line or block."))
	     (if region
		 (message "Copied region.")
	       (message "Copied line.")))))))

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
  (unless (or ansys-classics-flag (ansys-process-running-p))
;    (setq mode-line-process (format ":%s" (process-status ansys-process)))
;    (force-mode-line-update)
    (error "No MAPDL process is running"))
  (let (s)
    (if arg
	(setq s (read-minibuffer "Send to interpreter: "
				 (buffer-substring-no-properties
				  (line-beginning-position)
				  (line-end-position))))
      (setq s (completing-read "Send to interpreter: "
	    ansys-help-index nil nil)))
    (cond
     (ansys-classics-flag
      (kill-new s)
      (ansys-send-to-classics))
     (t
      (comint-send-string (get-process
			   (if (boundp 'ansys-process-name)
			       ansys-process-name)) (concat s "\n"))
      ;;  (walk-windows
      ;;    (lambda (w)
      ;;      (when (string= (buffer-name (window-buffer w)) "*ANSYS*")
      ;;        (with-selected-window w (goto-char (point-max))))))
      ;; (setq mode-line-process (format ":%s" (process-status ansys-process)))
      ;; (force-mode-line-update)
      (display-buffer "*ANSYS*" 'other-window)))))


(require 'comint)

;;;###autoload
(defun ansys-start-ansys ()
   "Start an ANSYS interpreter process under GNU-Linux or the launcher under Windows.
 For the interpreter process summarise the run's configuration
 first. The specified No of cores is not shown if they are chosen
 smaller than 3 (see `ansys-number-of-processors')."
   (interactive)
   (let (ansys-process-buffer)
     (when (ansys-process-running-p)
       (error "An ANSYS interpreter is already running under Emacs"))
     (message "Preparing an ANSYS interpreter run...")
     ;; (setq comint-use-prompt-regexp t) TODO: ???
     ;; (ansys-program "")		 ;take exec from -program var.
     ;; (ansys-license-file "")	 ;
     ;; (ansys-ansysli-servers "")	 ;
					 ;(ansys-license "")		 ;

					 ; env variable: ANSYS162_WORKING_DIRECTORY or -dir command line string
					 ; (setenv "ANSYS162_WORKING_DIRECTORY" "/tmp")
					 ; (getenv "ANSYS162_WORKING_DIRECTORY")

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
	   " in " default-directory ", server: " ansys-license-file))
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

        ;; w32-shell-execute not know under RHEL Emacs 23.1
       ;; (if (fboundp 'w32-shell-execute)
       ;; 	   (w32-shell-execute "Open" ansys-program))
       ))

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
  "Start the ANSYS Help Viewer.
Alternatively under a GNU-Linux system, one can also use the ANSYS
command line \"/SYS, anshelp162\" when running ANSYS
interactively, provided that anshelp162 is found in the search
paths for executables (these are stored in the PATH environment
variable)."
  (interactive)
  (ansys-help-program "")		;checking
  (progn
    (cond
     (ansys-unix-system-flag
      (start-process "ANSYS-help-program" nil ansys-help-program)
      (message "Started the ANSYS Help Viewer..."))
     ((string= system-type "windows-nt")
      (if (fboundp 'w32-shell-execute)
	  (w32-shell-execute "Open" (concat "\"" ansys-help-program "\"")
			     ansys-help-program-parameters)  ;HINT: Eli Z.,M. Dahl
	(error "w32-shell-execute not bound"))
      (message "Started the ANSYS Help Viewer..."))
     (t
      (error "Can only start the ANSYS help on Windows and GNU-Linux systems")))))

(defun ansys-search-keyword()
  "Search the code line for a valid the keyword from `ansys-help-index'."
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

(defun ansys-browse-apdl-guide ()
  "Open the ANSYS APDL guide in a browser."
  (interactive)
  (let (file (path ansys-help-path) command)
    (cond
     (ansys-unix-system-flag
      (setq file "ans_apdl/Hlp_P_APDLTOC.html")
      ;; use browse-url-default-browser!
      (if (fboundp 'browse-url-xdg-open)
	  (browse-url-xdg-open (concat path file))
	;; (browse-url-default-browser (concat path file)) not working with E23.1 on RHEL
	(browse-url-firefox (concat path file))))
     ;; windows
     ((string= system-type "windows-nt")
      (setq file (concat "ans_apdl\\Hlp_P_APDLTOC.html" file))
      ;; wrapper of ShellExecute MS-Windows API
;      (message "file:%s path:%s" file path)
;      (w32-shell-execute "Open" (concat path file)))
      (browse-url-default-windows-browser (concat "file:" path file)))
     (t
      (error "Can only start the ANSYS help on Windows and Unix/GNU-Linux systems")))
    (message "Called HTML browser for keyword \"%s\"..." command)))

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
  (let (file
	(path ansys-help-path)
	command)
    (if arg
	(setq command (completing-read "Browse help for keyword: "
				       ansys-help-index))
      (setq command (ansys-search-keyword)))
    (setq file (nth 1 (assoc-string command ansys-help-index t)))
    (unless  file
      (error "Keyword \"%s\" is not uniquely completable" command))
    ;; we must adapt the path to various items!
    (cond
     ((string-match "_C_" file)
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
      (setq file (concat "ans_thry/" file))))
    ;; FIXME: remove
;;     (cond
;;      ((string= system-type "gnu/linux")
;;       (if (fboundp 'browse-url-xdg-open)
;; 	  (browse-url-xdg-open (concat path file))
;; ;	(browse-url-default-browser (concat path file)) not working with E23.1 on RHEL
;; 	(browse-url-firefox (concat path file) 'new-window)))
;;      ((string= system-type "windows-nt")
;;       ;; TODO: w32-shell-execute not know under Emacs 23.1
;;       ;; use browse-url-default-browser!
;;       (browse-url-default-windows-browser (concat path file)))
;;      (t
;;       (error "Can only start the ANSYS help on Windows and GNU-Linux systems")))
    (browse-url-default-browser (concat "file://" path file))
    ;; the following will be overwritten by browse-url-default-browser?
    ;; (message "Called HTML browser for keyword \"%s\"..." command)
    ))

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
Show the status in a separate buffer, the license
type (`ansys-license') determines a highlighting of the license
server summary rows."
  (interactive)
  (cond
   ((and ansys-lmutil-program ansys-license-file)
    ;; lmutil calls with many license server specified takes loooooonnnnggg
    (message "Retrieving license (%s) status, this may take some time..." ansys-license)
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
	(search-forward-regexp ansys-license nil t)
	(forward-line)
	(setq eol (point))
	(forward-line -1)
	(setq bol (point))
	(put-text-property bol eol 'face 'font-lock-warning-face)
	;;  on Windows the license stat buffer doesn't move to point without:
	(when (not ansys-unix-system-flag)
	  (set-window-point (get-buffer-window "*ANSYS-licenses*") (point)))))
    (display-buffer "*ANSYS-licenses*" 'otherwindow)
    (message "Updated license status: %s." (current-time-string)))
   (t
    (message "No license information or lmutil program found"))))

;; starting in GUI mode (/menu,on) does inhibit the process intercommunication
;; => /menu,graph
;; env variable ANS_CONSEC=YES disables dialog boxes

(defun ansys-start-graphics ()
  "Start - in interactive mode - the MAPDL display window."
  (interactive)
  (unless
      (ansys-process-running-p)
    (error "No interactive MAPDL process is running"))
  (progn (comint-send-string
	  (get-process ansys-process-name)
	  ;; "/show,X11c\n/menu,grph\n"
	  "/show,3d\n/menu,grph\n"
	  )
	 (display-buffer "*ANSYS*" 'other-window)))

(defun ansys-start-pzr-box ()
  "Start the ANSYS Pan/Zoom/Rotate dialog box."
  (interactive)
  (cond
   (ansys-classics-flag
    (kill-new "/ui,view\n")
    (ansys-send-to-classics))
   ((ansys-process-running-p)
    (comint-send-string (get-process ansys-process-name) "/ui,view\n")
    (display-buffer "*ANSYS*" 'other-window))
   (t
    (error "No interactive MAPDL process running or Classics GUI can be found"))))

(defun ansys-iso-view (arg)
  "Show current display in isometric view (/view,,1,1,1)."
  (interactive "p")
  (cond
   (ansys-classics-flag
    (kill-new "/view,,1,1,1\n/replot\n")
    (ansys-send-to-classics))
   ((ansys-process-running-p)
    (comint-send-string (get-process ansys-process-name) "/view,,1,1,1\n/replot\n")
    (display-buffer "*ANSYS*" 'other-window))
   (t
    (error "No interactive MAPDL process running or Classics GUI can be found"))))

(defun ansys-move-up (arg)
  "Move geometry up ARG steps in the graphics window.
A Negative ARG moves ARG steps down."
  (interactive "p")
  (cond
   (ansys-classics-flag
    (kill-new (format "/focus,,,-0.25*(%d),,1\n/replot\n" arg))
    (ansys-send-to-classics))
   ((ansys-process-running-p)
    (comint-send-string (get-process ansys-process-name)
			(format "/focus,,,-0.25*(%d),,1\n/replot\n" arg))
    (display-buffer "*ANSYS*" 'other-window))
   (t
     (error "No ANSYS process is running"))))

(defun ansys-move-down (arg)
  "Move geometry down ARG steps in the graphics window.
A Negative ARG moves ARG steps up."
  (interactive "p")
  (ansys-move-up (- arg)))

(defun ansys-move-right (arg)
  "Move geometry right ARG steps in the graphics window.
A Negative ARG moves ARG steps left."
  (interactive "p")
  (cond
   (ansys-classics-flag
    (kill-new (format "/focus,,-0.25*(%d),,,1\n/replot\n" arg))
    (ansys-send-to-classics))
   ((ansys-process-running-p)
    (comint-send-string (get-process ansys-process-name)
			(format "/focus,,-0.25*(%d),,,1\n/replot\n" arg))
    (display-buffer "*ANSYS*" 'other-window))
   (t
    (error "No ANSYS process is running"))))

(defun ansys-move-left (arg)
  "Move geometry left ARG steps in the graphics window.
A Negative ARG moves ARG steps right."
  (interactive "p")
  (ansys-move-right (- arg)))

(defun ansys-zoom-in ()
  "Zoom into the graphics window."
  (interactive)
  (cond
   (ansys-classics-flag
     (kill-new "/dist,,.7,1\n/replot\n")
     (ansys-send-to-classics))
   ((ansys-process-running-p)
    (comint-send-string (get-process ansys-process-name) "/dist,,.7,1\n/replot\n") ;valid in any processor
    (display-buffer "*ANSYS*" 'other-window))
   (t
    (error "No interactive MAPDL process running or Classics GUI can be found"))))

(defun ansys-zoom-out ()
  "Zoom out of the graphics window."
  (interactive)
  (cond
   (ansys-classics-flag
    (kill-new "/dist,,1.4,1\n/replot\n")
    (ansys-send-to-classics))
   ((ansys-process-running-p)
    (comint-send-string (get-process ansys-process-name) "/dist,,1.4,1\n/replot\n") ;valid in any processor
    (display-buffer "*ANSYS*" 'other-window))
    (t
     (error "No interactive MAPDL process running or Classics GUI can be found"))))

(defun ansys-replot ()
  "Replot the ANSYS graphics screen."
  (interactive)
  (cond
   (ansys-classics-flag
    (kill-new "/replot\n")
    (ansys-send-to-classics))
   ((ansys-process-running-p)
    (comint-send-string (get-process ansys-process-name) "/replot\n") ;valid in any processor
    (display-buffer "*ANSYS*" 'other-window)))
  (t
   (error "No interactive MAPDL process running or Classics GUI can be found")))

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

(defun ansys-license-file ( file)
  "Change the ANSYS license file name or license server(s).
And specify the string FILE in the variable `ansys-license-file'
which can either be the license file name or license server(s)
specification.  The server specification must include the port
number (default port 1055), multiple server names are separated
by colons `:' on Linux, semi-colons `;' on Windows , for example
\"27005@rbgs421x:27005@rbgs422x\". The license specification is
stored in the environment variable ANSYS-LICENSE-FILE."
  (interactive
   (list (read-string
	  (concat "License server or license file [" ansys-license-file "]: ")
	  nil nil ansys-license-file)))
  (cond ((null file)
	 buffer-name)
	(t
	 (setq ansys-license-file file)
	 (setenv "ANSYSLMD_LICENSE_FILE" file)
	 (message (concat "Set ansys-license-file to \""
			  ansys-license-file "\".")))))

(defun ansys-install-directory ()
  "Change the ANSYS installation directory.
This is the path before the directory `ansys_inc/' under Linux or
`ANSYS Inc\\' under Windows."
  (interactive)
  (let* ((idir ansys-install-directory)
	 path
	 (ndir
	  (expand-file-name	       ;in case it was written ~
	   (file-name-as-directory	;in case the slash is forgotten
	    (read-directory-name
	     (concat "Specify the ANSYS installation directory ["
		     idir "]:")
	     nil nil idir)))))
    (cond (ansys-unix-system-flag
	   (setq path (concat ndir "ansys_inc")))
	  (t
	   (setq path (concat ndir "ANSYS Inc"))))
    (if (file-readable-p path)
	(progn
	  (setq ansys-install-directory
		(file-name-as-directory ndir)) ;ensure final slash
	  (message
	   (concat
	    "Set ansys-install-directory to \"" ndir "\".")))
      (error "ANSYS directory \"%s\" is not readable" path))
    (ansys-initialise-defcustoms 'force)))

(defun ansys-ansysli-servers ( servers)
  "Change the ANSYS interconnect servers to SERVERS.
And specify it in the variable `ansys-ansysli-servers'.  The
server specification must include the port number even when it is
2325, the default port number: port_number@server_name, multiple
server names are separated by a colon, for example
\"rbgs421x:rbgs422x:...\"."
  (interactive
   (list (read-string (concat "Interconnect license server(s) [" ansys-ansysli-servers "]: ")
		      nil nil ansys-ansysli-servers)))
  (cond ((null servers)
	 (buffer-name))
	(t
	 (setq ansys-ansysli-servers servers)
	 (setenv "ANSYSLI_SERVERS" servers)
	 (message (concat "Set ansys-ansysli-servers to \""
			  ansys-ansysli-servers "\".")))))

;; FIXME:
;; (error "Please specify the license server information with
;;     the `ansys-license-file' function or either set
;;     ANSYSLMD_LICENSE_FILE or LM-LICENSE-FILE environment
;;     variable")

(defun ansys-license ()
  "Change the ANSYS license type.
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
