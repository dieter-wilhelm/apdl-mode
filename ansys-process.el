;;; ansys-process.el -- Managing Ansys runs and process

;; Copyright (C) 2006 - 2010  H. Dieter Wilhelm GPL V3

;;;###autoload
(defun ansys-abort-file (&optional arg) ;NEW
  "Writes an ansys abort file for terminating the current run.
Normally the Ansys jobname is taken from the variable `ansys-job'
you can change its value by calling the equally named
function (or typing \\[ansys-job]).  The file (JOBNAME.abt) in
the default directory contains the sole word \"nonlinear\".  The
function prompts for an appropriate job name when ARG is
negative, otherwise it uses a sensible default.  In case the
default directory is not of your liking, use: `M-x cd'."
  (interactive "p")
  (unless arg (setq arg 0))
					;  (debug)
  (let (filename)
    (cond
     ((< arg 0)				;ask for job-name
      (setq filename
	    (read-string
	     (concat "job name: [" ansys-job "] ") nil nil
	     ansys-job))
      (setq filename (concat filename ".abt")))
     (t					;search for /filn
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "/filn.*,\\(\\w+\\)" nil 'noerror)
	    (setq filename (concat (match-string 1) ".abt"))
	  (setq filename (concat ansys-job ".abt")))))
    (if (yes-or-no-p (concat "Write \"" default-directory filename "\"? "))
	(ansys-write-abort-file filename)
      (message "Function \"ansys-abort-file\" canceled!")))))

(defun ansys-write-abort-file (filename) ;NEW
  "Open file FILENAME, clear it's contents and insert \"nonlinear\"."
  (find-file filename)
  (delete-region (point-min) (point-max))
  (insert "nonlinear")
  (save-buffer)
  (message (concat "Wrote \"%s\" into " default-directory ".") filename))

;;;###autoload
(defun ansys-display-error-file ()	;NEW
  "Open the current solver error file in the current directory.
You might change the directory with \"M-x `cd'\".  The error file
name consists of the current job name and the suffix '.err'.  For
the job name the variable `ansys-job' is used (which is
initialised to \"file\", in accordance to the Ansys default job
name). You can change the job name interactively either with the
\"\\[ansys-job]\" or in the customisation facility (by calling
`ansys-customise-ansys')."
  (interactive)
  (let (file ansys-job)
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
  (if (ansys-process-running-p)
      (progn
	(comint-send-region (get-process ansys-process-name) (point-min) (point))
	(display-buffer (process-buffer (get-process ansys-process-name)) 'other-window))
    (message "Copied from beginning of buffer to cursor.")))

(defun ansys-send-to-ansys (beg end)	;NEW
  "Send code line or region to an Ansys process otherwise copy it.
Argument BEG is the beginning of the region.  Argument END is the
end of the region.  If there is no active region, either go to
the next code line or send the current one.  When there is no
running Ansys process just copy the respective code to the
clipboard."
  (interactive "r")
  ;; copy the region, current line or go to next! code line
  (let (eol bol s reg)
    (cond ((use-region-p)
	   (setq reg t)
	   (when (< (point) (region-end))
	     (exchange-point-and-mark))
	   (setq s (buffer-substring-no-properties beg end))
	   (kill-ring-save beg end))
	  ((not (ansys-code-line-p))
	   (ansys-next-code-line)
	   (error "There was no valid code line"))
	  (t
	   (save-excursion
	     (back-to-indentation)
	     (setq bol (point))
	     (end-of-line)
	     (setq eol (point)))
	   (setq s (buffer-substring-no-properties bol eol))
	   (kill-ring-save bol eol)
	   (ansys-next-code-line)))
    (cond ((ansys-process-running-p)
	   (comint-send-string (get-process ansys-process-name) (concat s "\n"))
	   (display-buffer "*Ansys*" 'other-window))
	  (t
	   (if reg
	       (message "Copied region.")
	     (message "Copied code line."))))))

(defun ansys-process-running-p ()
  (let ((proc (get-process ansys-process-name)))
    (if proc
	(string= "run" (process-status proc))
      nil)))

;; (defun ansys-update-mode-line ()
;;   (setq mode-line-process (format ":%s" (process-status ansys-process)))
;;   (force-mode-line-update))

(defun ansys-query-ansys-command ()	;NEW
  ""
  (interactive)
  (unless (ansys-process-running-p)
;    (setq mode-line-process (format ":%s" (process-status ansys-process)))
;    (force-mode-line-update)
    (error "No Ansys process is running"))
  (let ((s (read-string "Send to solver: ")))
    (comint-send-string (get-process ansys-process-name) (concat s "\n"))
    ;;  (walk-windows
    ;;    (lambda (w)
    ;;      (when (string= (buffer-name (window-buffer w)) "*Ansys*")
    ;;        (with-selected-window w (goto-char (point-max))))))
    ;; (setq mode-line-process (format ":%s" (process-status ansys-process)))
    ;; (force-mode-line-update)
    (display-buffer "*Ansys*" 'other-window)))

(require 'comint)
;; TODO defvar ansys-process-buffer??

(defun ansys-start-ansys ()		;NEW
  (interactive)
  (setq ansys-process-name "Ansys")
  ;; (setq comint-use-prompt-regexp t) TODO: ???
  (ansys-program "")			;take exec from -program var.
  (ansys-license-file "")		;take file from license-file or env.

  (when (ansys-process-running-p)
    (error "Ansys already running, won't start subsequent runs"))
  (if (y-or-n-p
       (concat
	"Start run: (l-type: " ansys-license ", job: " ansys-job ", server: " ansys-license-file ")? "))
      (message "Starting the Ansys solver...")
    (error "Ansys run canceled"))
  (setq ansys-process-buffer (make-comint ansys-process-name ansys-program nil (concat "-p " ansys-license " -j " ansys-job)))
;  (comint-send-string (get-process ansys-process-name) "\n")
  (display-buffer ansys-process-buffer 'other-window)
;  (switch-to-buffer ansys-process-buffer)
  (other-window 1)
  (setq comint-prompt-regexp "BEGIN:\\|PREP7:\\|SOLU_LS[0-9]+:\\|POST1:\\|POST26:\\|RUNSTAT:\\|AUX2:\\|AUX3:\\|AUX12:\\AUX15:")
  (font-lock-add-keywords nil (list comint-prompt-regexp))

	  ;; comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom comint-watch-for-password-prompt comint-truncate-buffer)
  )

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
	(delete-process (get-process ansys-process-name))
	(message "Killing run...done.")
	(setq mode-line-process (format ":%s" (process-status (get-process ansys-process-name))))
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
	(process-send-string (get-process ansys-process-name) "finish $ /exit,all\n"))
	;; (setq mode-line-process (format ":%s" (process-status ansys-process)))
	;; (force-mode-line-update))
    (error "Exiting of Ansys run canceled")))

;;;###autoload
(defun ansys-start-ansys-help ()       ;NEW_C
  "Start the Ansys help system.
Alternatively one can use the Ansys \"/SYS, anshelp120\" command
when running Ansys interactively and provided that anshelp120 (or
anshelp120.chm on Windows) is found e. g. through the PATH
environment variable."
  (interactive)
  (if (string= ansys-help-file "")
      (error "You must set the `ansys-help-file' variable")
    (progn
      (message "Starting the Ansys help browser...")
      (cond
       ((ansys-is-unix-system-p)
	(start-process "ansys-help-file" nil ansys-help-file))
       ((string= system-type "windows-nt")
	(w32-shell-execute "Open" ansys-help-file)))))) ;HINT: Eli Z., M. Dahl

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
  (message "Ansys process status: %s" ;, process identification No: %d"
	   (process-status ansys-process-name))
	   ;; (process-id (get-process ansys-process-name))
  )

(defun ansys-license-status ()		;NEW
  "Display the Ansys license status or starts a license tool.
For Unix systems do this in a separate buffer, under Windows
start the anslic_admin.exe utility, which has a button for
displaying the license status."
  (interactive)
  (cond
   ((ansys-is-unix-system-p)
    (ansys-license-file-check)
    (ansys-ansysli-servers-check)
    (message "Retrieving license status, please wait...")
    (with-current-buffer (get-buffer-create "*LM-Util*")
      (delete-region (point-min) (point-max)))
    ;; syncronous call
    (call-process ansys-lmutil-program nil "*LM-Util*" nil "lmstat" "-c "  ansys-license-file  "-a")
    (let (bol eol)
      (with-current-buffer "*LM-Util*"
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
		 " -*- License status -*-\n" 'face 'match))

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
	;; (set-window-point (get-buffer-window "*LM-Util*") (point))
	))
    (display-buffer "*LM-Util*" 'otherwindow)
    (message "Updated license status: %s." (current-time-string)))
   ((string= system-type "windows-nt")
    ;; TODO: check for -lmutil-program
    (if (string= ansys-lmutil-program "")
	(error "You must set the `ansys-lmutil-program' variable")
      (w32-shell-execute nil ansys-lmutil-program))
    (message "Loading lmutil helper program...")) ;nil for executable
   (t
    (error "No license status available on %s" system-type))))

(defun ansys-start-graphics ()		;NEW
  "Start the Ansys display in interactive mode."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No Ansys process is running"))
  (progn (comint-send-string (get-process ansys-process-name)
		      "/show,X11c\n/menu,grph\n") ;valid in any processor
	 (display-buffer "*Ansys*" 'other-window)))

(defun ansys-start-pzr-box ()		;NEW PanZoomRotate box
  "Start the Ansys Pan/Zoom/Rotate dialog box in interactive mode."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No Ansys process is running"))
  (comint-send-string "*Ansys*";(get-process ansys-process-name)
		      "/ui,view\n") ;valid in any processor
  (display-buffer "*Ansys*" 'other-window))

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
And set the variable `ansys-program' accordingly if the
executable EXEC can be found."
  (interactive "FAnsys solver executable: ")
  (when (string= exec "")
    (setq exec ansys-program))

  (unless (executable-find exec)
    (error "Cannot find Ansys solver executable \"%s\" on the system" exec))

  (setq ansys-program exec)
  (message "ansys-program is set to \"%s\"." ansys-program)

  ;; ;; check default name in exec-path
  ;; (let (pr)
  ;;   (if (and ansys-program
  ;; 	     (not (string= ansys-program "")))
  ;; 	(setq pr ansys-program)
  ;;     (setq pr "/ansys_inc/v120/ansys/bin/ansys120"))
  ;;   (setq ansys-program
  ;; 	  (read-file-name
  ;; 	   (concat "Ansys program name [" pr "]: ") "" pr))
  ;;   (if (not (file-exists-p ansys-program))
  ;; 	(error "Error: File %s does not exist" ansys-program))
  ;;   (message (concat "Ansys program is set to \"" ansys-program "\".")))
  )

(defun ansys-help-file ()			;NEW
  "Change the Ansys help file name.
And specify it in the variable `ansys-help-file'."
  (interactive)
  (let (pr)
    (if (and ansys-help-file
	     (not (string= ansys-help-file "")))
	(setq pr ansys-help-file)
      (if (ansys-is-unix-system-p)
	  (setq pr "/ansys_inc/v120/ansys/bin/anshelp120")
	(setq pr "c:\\\\Program\ Files\\Ansys\ Inc\\v120\\CommonFiles\\HELP\\en-us\\ansyshelp.chm")))
    (setq ansys-help-file
	  (read-file-name
	   (concat "Ansys help file [" pr "]: ") "" pr))
    (message (concat "Ansys help file is set to \"" ansys-help-file "\"."))))

(defun ansys-lmutil-program ( exec)		;NEW
  "Change the Ansys LMutil program name.
And specify it in the variable `ansys-lmutil-program'.  The
function inserts the string `default-directory' in the prompt
when the variable `insert-default-directory' is not nil."
  (interactive "FAnsys LMUtil executable: ")
  (when (string= exec "")
    (setq exec ansys-lmutil-program))

  (unless (executable-find exec)
    (error "Cannot find Ansys LMUtil executable \"%s\" on the system" exec))

  (setq ansys-lmutil-program exec)
  (message "ansys-lmutil-program is set to \"%s\"." ansys-lmutil-program))

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
  "Returns t if Ansys license file (server) information is found.
Checks whether `ansys-license-file' is set, if not sets its value
to the environment variable ANSYSLMD_LICENSE_FILE or
LM_LICENSE_FILE, in this order of precedence.  When also these
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
  "Returns t if Ansys interconect server information is found.
Checks whether `ansys-ansysli-servers' is set or uses the
environment variable ANSYSLI_SERVERS for it."
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
    variable ANSYSLI_SERVERS."))))

(defun ansys-license-file ( file)		;NEW
  "Change the Ansys license file name.
And specify it in the variable `ansys-license-file' which can
either be the license file name or license server
specification(s).  The server specification must include the port
number when it isn't 1055, the default port number:
port_number@server_name, multiple server names are separated by a
colon, for example \"27005@rbgs421x:27005@rbgs422x:...\"."
  (interactive "sLicense server or license file :")
  (cond ((string= file "")
	 (ansys-license-file-check))
	(t
	 (setq ansys-license-file file)
	 (message (concat "Set ansys-license-file to \""
			  ansys-license-file "\".")))))

(defun ansys-ansysli-servers ( servers)		;NEW
  "Change the Ansys interconnect servers.
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

