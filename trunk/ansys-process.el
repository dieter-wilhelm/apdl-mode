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
  (setq comint-use-prompt-regexp t)
  (cond ((string= ansys-license "")
	 (error "You must set the `ansys-license' variable"))
	((string= ansys-license-file "")
	 (error "You must set the `ansys-license-file' variable"))
	((string= ansys-program "")
	 (error "You must set the `ansys-program' variable")))
  (when (ansys-process-running-p)
    (error "Ansys already running, won't start subsequent runs"))
  (ansys-license)
  (ansys-job)
  (if (y-or-n-p
       (concat
	"Start this Ansys run: (license type: " ansys-license ", jobname: " ansys-job ")? "))
      (message "Starting the Ansys run...")
    (error "Ansys run canceled"))
  (setq ansys-process-buffer (make-comint ansys-process-name ansys-program nil (concat "-p " ansys-license " -j " ansys-job)))
;  (comint-send-string (get-process ansys-process-name) "\n")
  (display-buffer ansys-process-buffer 'other-window)
;  (switch-to-buffer ansys-process-buffer)
  (other-window 1)
  (setq comint-prompt-regexp "POST1:\\|PREP7:\\|SOLU_LS[0-9]+:\\|BEGIN:\\|POST26:")
  (font-lock-add-keywords nil (list comint-prompt-regexp))

	  ;; comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom comint-watch-for-password-prompt comint-truncate-buffer)
  )

;; (defun ansys-start-ansys ()		;NEW
;;   "Start an Ansys run (when no run is already active).
;; Ask for confirmation with some run particulars before actually
;; starting the process."
;;   (interactive)
;;   (cond ((string= ansys-license "")
;; 	 (error "You must set the `ansys-license' variable"))
;; 	((string= ansys-license-file "")
;; 	 (error "You must set the `ansys-license-file' variable"))
;; 	((string= ansys-program "")
;; 	 (error "You must set the `ansys-program' variable")))
;;   (when (and ansys-process (string= "run" (process-status ansys-process)))
;;     (error "Ansys already running, won't start subsequent runs"))
;;   (ansys-license)
;;   (ansys-job)
;;   (if (y-or-n-p
;;        (concat
;; 	"Start this Ansys run: (lic: " ansys-license ", job: " ansys-job ")? "))
;;       (message "Starting run...")
;;     (error "Run canceled"))
;;   (setenv "LM_LICENSE_FILE" ansys-license-file)
;;   (setenv "PATH" (concat "/appl/ansys/ansys110/bin:" (getenv "PATH")))
;;   (setq ansys-process
;; 	(start-process
;; 	 "ansys" "*Ansys*" ansys-program
;; 	 (concat "-p " ansys-license " -j " ansys-job)))
;;   (display-buffer "*Ansys*" 'other-window)
;;   ;;  (process-send-string ansys-process "\n") Ansys idiosynncrasy, skip
;;   ;;the license agreement, normally harmless but not possible when
;;   ;;there is a lock-file from a crashed run!
;;   (message "Starting run...done, process status: %s, Id: %d"
;; 	   (process-status ansys-process)
;; 	   (process-id ansys-process))
;;   (setq mode-line-process (format ":%s" (process-status ansys-process)))
;;   (force-mode-line-update)
;;   (walk-windows				;HINT: Markus Triska
;;    (lambda (w)
;;      (when (string= (buffer-name (window-buffer w)) "*Ansys*")
;;        (with-selected-window w (goto-char (point-max)))))))

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
Alternatively one can use the Ansys \"/SYS, anshelp110\" command
when running Ansys interactively and provided that anshelp110 (or
anshelp110.chm on Windows) is found e. g. through the PATH
environment variable."
  (interactive)
  (if (string= ansys-help-file "")
      (error "You must set the `ansys-help-file' variable")
    (progn
      (message "Starting the Ansys help system...")
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
;; (setq ansys-license-filter-keywords
;;       )

;; (defun ansys-license-filter (proc string)
;;   (display-buffer (process-buffer proc))
;;   (with-current-buffer (process-buffer proc)
;;     ;; (font-lock-mode t)
;;     (let ((keywords (list (concat "\\<" ansys-license "\\>") 0 font-lock-keyword-face t))
;; 	  )
;;   		;(= (point) (process-mark proc))))
;;       (setq font-lock-keywords keywords)
;;       ;; (save-excursion
;;   	;; Insert the text, advancing the process marker.
;;   	;; (set-marker (process-mark proc) (point))
;;   	;; (setq start (process-mark proc))
;;   	;; (goto-char start)
;; 	(goto-char (process-mark proc))
;;   	(insert string)
;;   	(insert "--- todo ---\n")
;; 	(set-marker (process-mark proc) (point))
;; 	;; (goto-char start)
;;   	;; (push-mark)
;;   	;; (search-forward "Users of" nil t)
;;   	;; (forward-line -1)
;;   	;; (delete-region (mark) (point))
;;   	;; (set-marker (process-mark proc) (point))
;;   	;)
;;       ;; (goto-char (point-max))
;;       (goto-char (process-mark proc))
;;       ))
;;   )



(defun ansys-license-file-check ()
  "Returns t if Ansys license file (server) information is found.
Checks whether `ansys-license-file' is preset in Emacs or uses
the environment variable ANSYSLMD_LICENSE_FILE or LM_LICENSE_FILE
for it, in the order of preference."
  (cond
   (ansys-license-file
    t)
   ((getenv "ANSYSLMD_LICENSE_FILE")
    (setq ansys-license-file (getenv "ANSYSLMD_LICENSE_FILE"))
    t)
   ((getenv "LM_LICENSE_FILE")
    (setq ansys-license-file (getenv "LM_LICENSE_FILE"))
    t)
   (t
    (error "Please specify the license server information in the `ansys-license-file' variable or use the environment variables ANSYSLMD_LICENSE_FILE or ANSYS-LICENSE-FILE"))))

(defun ansys-license-status ()		;NEW
  "Display the Ansys license status or starts a license tool.
For Unix systems do this in a separate buffer, under Windows
start the anslic_admin.exe utility, which has a button for
displaying the license status."
  (interactive)
  (cond
   ((ansys-is-unix-system-p)
    (message "Retrieving license status, please wait...")
    (ansys-license-file-check)
    (with-current-buffer (get-buffer-create "*LM-Util*")
      (delete-region (point-min) (point-max)))
    ;; syncronous call
    (call-process ansys-lmutil-program nil "*LM-Util*" nil "lmstat" "-c "  ansys-license-file  "-a")
    (let ((kw (list
	       (concat "\\<" ansys-license "\\>:.*")
	       0 font-lock-keyword-face t)))
      (with-current-buffer "*LM-Util*"
	;; (font-lock-mode 1)
	(setq font-lock-keywords kw)

	;; remove unintersting licenses
	(goto-char (point-min))
	(delete-matching-lines "\\<acfx\\|\\<ai\\|\\<wbunix\\|\\<rdacis\\>")

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

	;; some comments
	(goto-char (point-min))
	(insert (propertize
		 " -*- License status -*-\n" 'face 'match))

	(goto-char (point-max))
	(insert "\n")
	(insert (propertize (current-time-string)
			    'face 'match))
	;; (set-window-point (get-buffer-window "*LM-Util*") (point))
	))
    (display-buffer "*LM-Util*" 'otherwindow)
    (message "Updated license status: %s." (current-time-string)))
   ((string= system-type "windows-nt")
    ;; TODO: check for -lmutil-program
    (if (string= ansys-lmutil-program "")
	(error "You must set the `ansys-lmutil-program' variable")
      (w32-shell-execute nil ansys-lmutil-program))) ;nil for executable
   (t
    (error "No license status available on %s" system-type))))

;; ;;;###autoload
;; (defun ansys-license-status ()		;NEW
;;   "Display the Ansys license status.
;; For Unix systems do this in a separate buffer, under Windows
;; start the anslic_admin.exe utility, which has a button for
;; displaying the license status."
;;   (interactive)
;;   (cond ((string= ansys-license-file "")
;; 	 (error "You must set the `ansys-license-file' variable"))
;; 	((string= ansys-lmutil-program "")
;; 	 (error "You must set the `ansys-lmutil-program' variable")))
;;   (let ((current-b (buffer-name))
;; 	;; (buffer (buffer-name (get-buffer-create "*LMutil*")))
;; 	)
;;     (message "Retrieving license status information from %s." ansys-license-file)
;;     (cond
;;      (ansys-is-unix-system-flag
;;       (setenv "LM_LICENSE_FILE" ansys-license-file)
;;       ;; (get-buffer-create "*LMutil*")
;;       ;; (delete-process "lmutil")
;;       ;; (start-process "lmutil" "*LMutil*" ansys-lmutil-program "lmstat" "-a"); async
;;       (call-process "lmutil" "*LMutil*" ansys-lmutil-program "lmstat" "-a") ;syncronous call
;;       ;; (set-process-filter (get-process "lmutil") 'ordinary-insertion-filter); 'ansys-license-filter)
;;       ;;       (while (string= "run" (process-status "lmutil"))
;;       ;; 	(sit-for 1))
;;       ;; (toggle-read-only 1)
;;       ;; (set-buffer current-b)
;;       ;; (display-buffer "*LMutil*" 'other-window)
;;       ;; (walk-windows
;;       ;;  (lambda (w)
;;       ;; 	 (when (string= (buffer-name (window-buffer w)) "*LMutil*")
;;       ;; 	   (with-selected-window w (goto-char (point-max))))))
;;       )
;;      ((string= system-type "windows-nt")
;;       (w32-shell-execute nil ansys-lmutil-program))))) ;nil for executable

(defun ansys-start-graphics ()		;NEW
  "Start the Ansys display in interactive mode."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No Ansys process is running"))
  (progn (comint-send-string (get-process ansys-process-name)
		      "/show,X11c\n/menu,grph\n") ;valid in any processor
;  (display-buffer "*Ansys*" 'other-window)
  ))

(defun ansys-start-pzr-box ()		;NEW PanZoomRotate box
  "Start the Ansys Pan/Zoom/Rotate dialog box in interactive mode."
  (interactive)
  (unless (ansys-process-running-p)
    (error "No Ansys process is running"))
  (comint-send-string (get-process ansys-process-name) "/ui,view\n") ;valid in any processor
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
      (if (ansys-is-unix-system-p)
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
      (if (ansys-is-unix-system-p)
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

