;;; apdl-process.el --- Managing runs and processes for APDL-Mode -*- lexical-binding: t -*-
;; Time-stamp: <2020-03-30>

;; Copyright (C) 2006 - 2020  H. Dieter Wilhelm GPL V3

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Maintainer: H. Dieter Wilhelm
;; Version: 20.4.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages, convenience
;; URL: https://github.com/dieter-wilhelm/apdl-mode

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

;; Managing runs and processes for APDL-Mode

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external defvars

(defvar apdl-username)
(defvar apdl-help-index)
(defvar apdl-license-file)
(defvar apdl-license-categories)
(defvar apdl-ansys-program)
(defvar apdl-ansys-launcher)
(defvar apdl-ansys-wb)
(defvar apdl-ansys-help-path)
(defvar apdl-mode-install-directory)
(defvar apdl-current-ansys-version)
;; (defvar apdl-ansys-install-directory)
(defvar apdl-ansysli-servers)
(defvar apdl-ansys-help-program-parameters)
(defvar apdl-is-unix-system-flag)
(defvar apdl-lmutil-program)
(defvar apdl-ansys-help-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; declare-functions

(declare-function apdl-initialise "apdl-initialise")
(declare-function apdl-next-code-line "apdl-mode")
(declare-function apdl-code-line-p "apdl-mode")
(declare-function apdl-skip-block-forward "apdl-mode")
(declare-function apdl-in-empty-line-p "apdl-mode")

;; (declare-function buffer-name "") ; in-built
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; requires

(require 'comint)
(require 'url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- customisation ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup APDL-process nil
  "Customisation 'process' subgroup for the APDL-Mode."
  :group 'APDL)

(defcustom apdl-license-occur-regexp
  '(
    "electronics2d_gui"
    "electronics_desktop"
    "a_spaceclaim_dirmod"		; spaceclaim
;;    "agppi"				; agppi -- Design Modeler
    "disc"				; disc* -- discovery procucts
    "aim_mp"				; aim_mp -- Discovery Aim
					; standard
    "stba"				; stba -- structural solver
    "struct"				; struct -- structural
    "mpba"				; mpba -- multiphysics solver
    "ane3"				; ane3 -- magnetics
					; ane3fl -- multiphysics
    "^ansys"				; ansys -- structural
    "anshpc"				; anshpc -- HighPerformanceComputing
    "^preppost"				; preppost -- PrePost
					; processing
    "mech_"				; mech_1 -- mechanical pro
					; mech_2 -- mechanical premium
    "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")	; and time XX:XX:XX of status
					; request
  "List of strings of interesting licenses.
This list is concatenated to a regexp for the function
`apdl-occur'."
  :type 'list
  :group 'APDL-process)


(defcustom apdl-job "file"
  "String variable storing the Ansys job name.
It is initialised to 'file' (which is also the Ansys default job
name).  See `apdl-abort-file' for a way of stopping a solver run
in a controlled way and `apdl-display-error-file' for viewing
the respective error file."
  :type 'string
  :group 'APDL-process)

(defcustom apdl-license-categories
  '("ansys"
    "struct"
    "ane3"
    "ansysds"
    "ane3fl"
    "preppost")
  "List of available license types to choose for an Ansys run.
This list should contain the license types you can choose from.
Below are often used license types (as e.g. seen with the
function `apdl-license-status') and their corresponding WorkBench
terminology.

\"ansys\" - Mechanical U (without thermal capability)
\"struct\" - Structural U (with thermal capability)
\"ane3\" - Mechanical/Emag (Structural U with electromagnetics)
\"ansysds\" - Mechanical/LS-Dyna (Mechanical U with Ansys LS-Dyna inter-phase)
\"ane3fl\" - Multiphysics
\"preppost\" - PrepPost (no solving capabilities)"
  :type 'list
  :group 'APDL-process)

(defcustom apdl-license "ansys"
  "The License type with which the Ansys interpreter will be started.
See `apdl-license-categories' for often used Ansys license types."
  ;;  :options '("ansys" "struct" "ane3" "ane3fl" "ansysds" "preppost")
  :options apdl-license-categories
  ;; options not available for strings (only hooks, alists, plists E22)
  :type 'string
  :group 'APDL-process)

(defcustom apdl-no-of-processors 4
  "No of processors to use for an Ansys solver run.
If smaller then 4 the run does not require additonal HPC
licenses.  4 is the Ansys default."
  :type 'integer
  :group 'APDL-process)

(defcustom apdl-blink-delay .3
  "Number of seconds to highlight the evaluated region."
  :group 'APDL-process
  :type 'number)

(defcustom apdl-blink-region-flag t
  "Non-nil means highlight the evaluated region."
  :group 'APDL-process
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- variables ---

(defvar apdl-emacs-window-id nil
  "Editing buffer's X11 window id.")

(defvar apdl-classics-window-id nil
  "The X11 window id of the Ansys GUI or the command window.")

(defvar apdl-classics-flag nil
  "Non-nil means that a Classics GUI could be found.")

;;; --- constants ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst apdl-begin-keywords
  '("\\*[dD][oO]" "\\*[dD][oO][wW][hH]?[iI]?[lL]?[eE]?"
    "\\*[iI][fF].*[tT][hH][eE][nN]" "\\*[cC][rR][eE][aA][tT][eE]")
  "Regexps describing APDL block begin keywords.")

(defconst apdl-block-begin-regexp
  (concat "\\("
          (mapconcat 'identity apdl-begin-keywords "\\|")
          "\\)\\>")
  "Regexp containing the APDL begin keywords.")

(defconst apdl-process-name "MAPDL"
  "Variable containing the name of a possible MAPDL interactive process.
Variable is used internally only.")

(defconst apdl-classics-process "Classics"
  "Variable containing the name of a possible MAPDL GUI process.
Variable is used internally only.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- functions ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apdl-toggle-classics ()
  "Toogle sending output to Ansys Classics.
Try to locate an Ansys Classics GUI or the command dialog box and
switch output to it."
  (interactive)
  (if apdl-classics-flag
      (progn
        (setq apdl-classics-flag nil)
        (message "Disconnected from Classics."))
    (if (apdl-classics-p)
        (progn (setq apdl-classics-flag t)
               (message "Connected to Classics."))
      (error "No Ansys Classics window found"))))

(defun apdl-classics-p ()
  "Check whether Ansys Classics is running.
Return nil if we can't find an MAPDL GUI."
  (let ((aID (replace-regexp-in-string
              "\n" ""
              (shell-command-to-string "~/a-m/X11/xGetClassicsWindow")))
        (eID (replace-regexp-in-string
              "\n" ""
              (shell-command-to-string "~/a-m/X11/xGetFocusWindow"))))
    (if (string= "" aID)
        ;; (error "No Ansys MAPDL window found")
        nil
      (setq apdl-emacs-window-id eID)
      (setq apdl-classics-window-id aID)
      ;;    (setq x-select-enable-clipboard t)             ; for kill-new necessary
      (setq select-enable-clipboard t)                     ; for kill-new necessary
      aID)))

(defun apdl-start-classics ()
  "Start the Ansys MAPDL Classics graphical user interface.
The output of the solver is captured in an Emacs buffer called
*Classics*."
  (interactive)
  (let ((bname (concat "*"apdl-classics-process"*")))
    (when (file-readable-p (concat default-directory apdl-job ".lock"))
      (if (yes-or-no-p
           (concat "Warning: There is a \""apdl-job".lock" "\" in " default-directory ". This might indicate that there is already a solver running.  Do you wish to kill the lock file? "))
          (delete-file (concat apdl-job ".lock"))
        (error "Starting the MAPDL GUI (Ansys Classics) cancelled")))
    (if (y-or-n-p
         (concat
          "Start run of: "
          apdl-ansys-program
          ", license: " apdl-license
          ;; "Start run?  (license type: " (if (boundp
          ;; 'apdl-license) apdl-license)
          (if (> apdl-no-of-processors 4)
              (concat ", No of procs: " (number-to-string apdl-no-of-processors))
            "")
          ", job: " (if (boundp 'apdl-job) apdl-job)
          " in " default-directory ", lic server: " apdl-license-file " "))
        (message "Starting MAPDL in GUI mode (Ansys Classics) ...")
      (error "Calling MAPDL solver (Ansys Classics) cancelled"))
    ;; -d : device
    ;; -g : graphics mode
    ;; -p : license
    ;; -np: no of PROCs
    ;; -j : job
    ;; v195 new params?: -t -lch
    ;; -g -p ansys -np 2 -j "file" -d 3D
    (start-process apdl-classics-process
		   (if apdl-is-unix-system-flag
		       bname
		     nil)
                   apdl-ansys-program
                   (concat " -g"
			   (concat " -p " apdl-license)
                           (concat " -np " (number-to-string apdl-no-of-processors))
                           (concat " -j \"" apdl-job "\"")
                           " -d 3D" ; 3d device, win32
                           ))
    (display-buffer bname 'other-window)))

(defun apdl-start-launcher ()
  "Start the Ansys Launcher."
  (interactive)
  ;;    (apdl-ansys-program "")  ; take exec from -program var.
  ;;    (apdl-license-file "")
  ;;    (apdl-ansysli-servers "")
  ;; (apdl-license "")
  (start-process "Launcher" nil apdl-ansys-launcher)
  (message "Started the Ansys Launcher..."))

(defun apdl-start-wb ()
  "Start the Ansys WorkBench."
  (interactive)
  ;;    (apdl-ansys-program "") ; take exec from -program var.
  ;;    (apdl-license-file "")
  ;;    (apdl-ansysli-servers "")
  ;; (apdl-license "")
  (start-process "WorkBench" nil apdl-ansys-wb)
  (message "Started the Ansys WorkBench..."))

(defun apdl-write-abort-file ( filename)
  "Open file FILENAME, clear it's contents and insert \"nonlinear\"."
  (interactive)
  (find-file filename)
  (delete-region (point-min) (point-max))
  (insert "nonlinear\n")
  (save-buffer)
  (message "Wrote \"%s\" into \"%s\"." filename default-directory))

;;;###autoload
(defun apdl-abort-file (&optional arg)
  "Writes an Ansys abort file for stopping the current run.
The abort file does not terminate the current session but
triggers the solver to stop solving in an orderly manner.  This
function prompts for a job name when ARG is negative.  Otherwise
it tries to guess it from the current file (for a /filname
command), if this fails the jobname is taken from the variable
`apdl-job', you can change this variable by calling the equally
named interactive function (i. e. typing \\[apdl-job]) or
setting it directly as Lisp expression (i. e.  typing
\"\\[eval-expression] (setq apdl-job \"jobname\")\", where
jobname is a name of your liking but must be enclosed with double
quotes (\") to represent a lisp string).  The file jobname.abt in
the current directory contains the sole word \"nonlinear\". In
case the `default-directory' is not the working directory of your
respective job, you can change it with \"\\[cd]\"."
  (interactive "p")
  (let ((job apdl-job)
        file
        lfile
        name)
    (cond
     ((< arg 0)                         ; ask for job-name
      (setq name
            (read-string
             (concat "Job name [" job "]: ") nil nil job)))
     (t                                 ; search for /filn
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
          (apdl-write-abort-file file)
          (message "Wrote MAPDL stop file %s in %s." file
                   default-directory))
      (message "Writing MAPDL stop file canceled!"))))

;;;###autoload
(defun apdl-display-error-file ()
  "Open the current interpreter error file in the current working directory.
You might change the directory with `M-x cd <RET>'.  The error
file name consists of the current job name and the suffix '.err'.
For the job name the variable `apdl-job' is used.  You can change
the job name interactively either with the \"\\[apdl-job]\" or in
the customisation facility (by calling `apdl-customise-ansys')."
  (interactive)
  (let ((file (concat apdl-job ".err")))
    (if (not (file-readable-p file))
        (error "Ansys error file \"%s\" doesn't exist in %s" file (pwd))
      (find-file-read-only-other-window file)
      (goto-char (point-max))
      (auto-revert-tail-mode 1))))

(defun apdl-copy-or-send-above                     ()
  "Copy or send above file content to the current cursor position."
  (interactive)
  (let ((process (get-process
                  (if (boundp 'apdl-process-name)
                      apdl-process-name
                    "APDL"))))
    ;; no-property stuff necessary?????
    ;;   (if (y-or-n-p
    ;;        (concat
    ;;                      "Start this Ansys run: (lic: " apdl-license ", job: " apdl-job ")? "))
    ;;       (message "Starting run...")
    ;;     (error "Run canceled"))
    (cond (apdl-classics-flag
           (clipboard-kill-ring-save (point-min) (point))
           (apdl-send-to-classics)
           (message "Send above file content to the Classics GUI" ))
          ((apdl-process-running-p)
           (comint-send-region process (point-min) (point))
           (display-buffer-other-frame (process-buffer process)))
          (t
           (clipboard-kill-ring-save (point-min) (point)) ; point-min is heeding narrowing
           (message "Copied from beginning of buffer to cursor.")))))

(defvar  apdl-current-region-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  'highlight)
    overlay)
  "The overlay for highlighting currently evaluated region or line.")

(defun apdl-blink-region (start end)
  "Let the region blink between START and END."
  (when apdl-blink-region-flag
    (move-overlay apdl-current-region-overlay start end)
    (run-with-timer apdl-blink-delay nil
                    (lambda ()
                      (delete-overlay apdl-current-region-overlay)))))

(defun apdl-send-to-classics ()
  "Sending clipboard content to the Classics GUI."
  ;;  (let ((win (call-process "/home/uidg1626/script/ctrlv.sh")))
  (let ()
    (sleep-for .5)                    ; wait till user lifts CTRL!
    (call-process (concat apdl-mode-install-directory
                          "X11/xPasteToWin") nil nil nil apdl-classics-window-id)
    (sleep-for .1) ; seems to take at least 0.1 s for the clipboard to copy!
    (call-process (concat apdl-mode-install-directory
                          "X11/xSendReturn") nil nil nil
                          apdl-emacs-window-id apdl-classics-window-id)
    ;; repeating this as workaround... TODO
    (sleep-for .1) ; seems to take 0.1 s for the clipboard to copy!
    (call-process (concat apdl-mode-install-directory
                          "X11/xSendReturn") nil nil nil
                          apdl-emacs-window-id apdl-classics-window-id)))

(defun apdl-send-to-ansys ( &optional move)
  "Send a region to the Ansys MAPDL interpreter.
If the interpreter is not active, just copy it.  If there is no
region marked, send (or copy) the current paragraph.  With a
prefix argument MOVE equal to `4' or `C-u' skip to the next code
line after this region (or paragraph)."
  (interactive "p")
  (let (code
        beg
        end
        (point (point))
        (process (get-process
                  (if (boundp 'apdl-process-name) apdl-process-name)))
        (region (and transient-mark-mode mark-active)))
    ;;                         (region (region-active-p))) ; this is for Emacs-23.1
    ;; make a valid region if possible, when region is not active:
    ;; "region" will be the whole code line (including \n)
    (unless region
      (mark-paragraph))
    (setq beg (region-beginning)
          end (region-end))
    ;; invalidate region
    (deactivate-mark)                ; for Emacs 23.1 no arguments
    ;; (deactivate-mark nil)
    (apdl-blink-region beg end)
    ;; send or copy region or line
    (cond (apdl-classics-flag
           (clipboard-kill-ring-save beg end)
           (apdl-send-to-classics)
           (message "Sent to Classics GUI"))
          ((apdl-process-running-p)
           (setq code (buffer-substring-no-properties beg end))
           (comint-send-string process
                               ;; "\n"); why did I do \n?
                               (concat code ""))
           (display-buffer-other-frame (concat "*" apdl-process-name "*"))
           (message "Sent region to solver."))
          (t
           (clipboard-kill-ring-save beg end)
           (message "Copied region.")))
    (if (= move 4)
        (progn
          (goto-char end)
          (apdl-next-code-line))
      (goto-char point))))

(defun apdl-send-to-apdl-and-proceed ( &optional stay)
  "Send a region or code line to the Ansys MAPDL interpreter.
When there is no running Ansys interpreter process just copy the
respective region or code line to the system clipboard and skip
to the subsequent code line.  With a prefix argument STAY of `4'
or `C-u' copy or send the code and remain at the current cursor
position.  The command can be repeated by typing just the final
character `j' (or `C-j')."
  (interactive "p")
  (let (; code
        beg
        end
        (process (get-process
                  (if (boundp 'apdl-process-name) apdl-process-name)))
        (region (and transient-mark-mode mark-active))
        (block (save-excursion
                 (back-to-indentation)
                 (looking-at apdl-block-begin-regexp)))
        (code (apdl-code-line-p))
        (column (current-column)))
    ;; (region (region-active-p))) ; this is for Emacs-23.1
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
       (apdl-skip-block-forward)
       (setq end (point))
       (setq region t))                ; block considered a region
     (code
      (setq beg (line-beginning-position))
      (save-excursion
        (forward-line 1)
        (setq end (point))))
     (t
      (unless (= stay 4)
        (apdl-next-code-line))
      (error "There was no active region or code line")))
    ;; move cursor to subsequent code line unless stay
    (unless (= stay 4)
      (if (and region
               (< (point) end))
          (exchange-point-and-mark))
      (move-to-column column)        ; stay in the previous column
      (apdl-next-code-line))
    ;; invalidate region
    (setq mark-active nil)
    ;; set-transient-map since 24.4
    (when (fboundp 'set-transient-map)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map "j"
           'apdl-send-to-apdl-and-proceed)
         (define-key map "\C-j"
           'apdl-send-to-apdl-and-proceed)
         map)))
    ;; send or copy region or line
    (cond (apdl-classics-flag
           (clipboard-kill-ring-save beg end)
           (if (fboundp 'set-transient-map)
               (if region
                   (message "Sent region, type \"j\" or \"C-j\" to sent next line or block.")
                 (message "Sent line, type \"j\" or \"C-j\" to sent next line or block."))
             (if region
                 (message "Sent region.")
               (message "Sent line.")))
           (apdl-send-to-classics))
          ((apdl-process-running-p)
           (setq code (buffer-substring-no-properties beg end))
           (comint-send-string process
                               ;; "\n"); why did I do \n?
                               (concat code ""))
           (display-buffer-other-frame (concat "*" apdl-process-name "*"))
           ;; Issue a hint to the user
           (if (fboundp 'set-transient-map)
               (if region
                   (message "Sent region, type \"j\" or \"C-j\" to sent next line or block.")
                 (message "Sent line, type \"j\" or \"C-j\" to sent next line or block."))
             (if region
                 (message "Sent region.")
               (message "Sent line."))))
          (t
           (clipboard-kill-ring-save beg end)
           (if (fboundp 'set-transient-map)
               (if region
                   (message "Copied region, type \"j\" or \"C-j\" to copy next line or block.")
                 (message "Copied line, type \"j\" or \"C-j\" to copy next line or block."))
             (if region
                 (message "Copied region.")
               (message "Copied line.")))))))

(defun apdl-process-running-p ()
  "Return nil if no Ansys interpreter process is running."
  (let ((proc (get-process
               (if (boundp 'apdl-process-name) apdl-process-name))))
    (if proc
        (string= "run" (process-status proc))
      nil)))

(defun apdl-query-apdl-command ( &optional arg)
  "Ask for a string which will be sent to the interpreter.
The string is completable to all current APDL commands and with
an optional prefix argument ARG the current command line is the
initial input."
  (interactive "P")
  (unless (or apdl-classics-flag (apdl-process-running-p))
    (error "No MAPDL process is running"))
  (let (s)
    (if arg
        (setq s (read-minibuffer "Send to interpreter: "
                                 (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))))
      (setq s (completing-read "Send to interpreter: "
                               apdl-help-index nil 'confirm)))
    (cond
     (apdl-classics-flag
      (kill-new s)
      (apdl-send-to-classics))
     (t
      (comint-send-string (get-process
                           (if (boundp 'apdl-process-name)
                               apdl-process-name)) (concat s "\n"))
      (display-buffer (concat "*" apdl-process-name "*") 'other-window)))))


;;;###autoload
(defun apdl-start-ansys ()
  "Start the MAPDL interpreter under Linux or the launcher under Windows.
For the interpreter process summarise the run's configuration
first.  The specified No of cores is not shown if they are chosen
smaller than 3 (see `apdl-number-of-processors')."
  (interactive)
  (let (apdl-process-buffer)
    (when (apdl-process-running-p)
      (error "An Ansys interpreter is already running under Emacs"))
    (message "Preparing an Ansys interpreter run...")
    (if (y-or-n-p
         (concat
          "Start run?  (version: "
          apdl-current-ansys-version
          ", license type: " apdl-license
          (if (>= apdl-no-of-processors 3)
              (concat ", No of processors: " (number-to-string apdl-no-of-processors))
            "")
          ", job: " (if (boundp 'apdl-job) apdl-job)
          " in " default-directory ", server: " apdl-license-file))
        (message "Starting the Ansys interpreter...")
      (error "Function apdl-start-ansys canceled"))
    (setq apdl-process-buffer
          (make-comint apdl-process-name apdl-ansys-program nil
                       (if (>= apdl-no-of-processors 3)
                           (concat "-np " (number-to-string apdl-no-of-processors)
                                   " -p " apdl-license " -j " apdl-job)
                         (concat "-p " apdl-license " -j " apdl-job))))
    ;;  (comint-send-string (get-process apdl-process-name) "\n")
    (setq apdl-classics-flag nil)
    (display-buffer apdl-process-buffer 'other-window)
    ;;  (switch-to-buffer apdl-process-buffer)
    (other-window 1)
    (setq comint-prompt-regexp "BEGIN:\\|PREP7:\\|SOLU_LS[0-9]+:\\|POST1:\\|POST26:\\|RUNSTAT:\\|AUX2:\\|AUX3:\\|AUX12:\\|AUX15:")
    (font-lock-add-keywords nil (list comint-prompt-regexp))))

(defun apdl-kill-ansys ()
  "Kill the current Ansys run under Emacs.
The function asks for confirmation before actually killing the
process.  Warning: Ansys writes a lock file (jobname.lock) if the
process is killed and not regularly exited.  You should prefer
the function `apdl-exit-ansys'."
  (interactive)
  (unless (apdl-process-running-p)
    (error "No active Ansys solver process"))
  (if (yes-or-no-p
       "Do you want to kill the running MAPDL solver?")
      (progn
        ;;                     (message "Killing run...")
        (delete-process (get-process apdl-process-name))
        (message "Killing Mechanical APDL run...done.")
        (display-buffer (concat "*" apdl-process-name "*") 'otherwindow))
    (message "Killing of Ansys run canceled.")))

(defun apdl-exit-ansys ()
  "Exit normally the current Ansys run under Emacs.
The function asks for confirmation before exiting the process
with the APDL /EXIT,all command which saves all model data."
  (interactive)
  (unless (apdl-process-running-p)
    (error "Error: No active Ansys process"))
  (if (yes-or-no-p
       "Do you want to exit the Ansys run?")
      (progn
        (message "Trying to exit run ...")
        (process-send-string (get-process apdl-process-name) "finish $ /exit,all\n"))
    ;; (setq mode-line-process (format ":%s" (process-status apdl-process)))
    ;; (force-mode-line-update))
    (error "Exiting of Ansys run canceled")))

;;;###autoload
(defun apdl-start-ansys-help ()
  "Start the Ansys Help Viewer.
Alternatively under a GNU-Linux system, one can also use the APDL
command line \"/SYS, anshelp201\" when running Ansys
interactively, provided that anshelp162 is found in the search
paths for executables (these are stored in the PATH environment
variable)."
  (interactive)
  (apdl-ansys-help-program "")          ; checking
  (progn
    (cond
     (apdl-is-unix-system-flag
      (start-process "apdl-ansys-help-program" nil apdl-ansys-help-program)
      (message "Started the Ansys Help Viewer..."))
     ((string= system-type "windows-nt")
      (if (fboundp 'w32-shell-execute)
          (w32-shell-execute
	   "Open" (concat "\"" apdl-ansys-help-program "\"")
	   apdl-ansys-help-program-parameters)  ; HINT: Eli Z.,M. Dahl
        (error "Function w32-shell-execute not bound"))
      (message "Started the Ansys Help Viewer..."))
     (t
      (error "Can only start the Ansys help on Windows and GNU-Linux systems")))))

(defun apdl-search-keyword()
  "Search the code line for a valid the keyword from `apdl-help-index'."
  (interactive)
  (when (apdl-in-empty-line-p)
    (error "Cannot find a keyword in an empty line"))
  (let* (
         (pt (point))
         (re "~/*[:word:]")
         (lbp (line-beginning-position))
         ;;                      (eolp (save-excursion (end-of-line) (point)))
         (str (upcase (buffer-substring-no-properties
                       (save-excursion
                         (+ pt (skip-chars-backward re lbp)))
                       (save-excursion
                         (+ pt (skip-chars-forward re))))))
         (cmpl (try-completion str apdl-help-index)))
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
      (setq cmpl (try-completion str apdl-help-index)))
    (cond ((stringp cmpl)               ; not unique
           cmpl)
          ((not cmpl)
           (error "\"%s\" is not a valid keyword" str))
          (t                            ; perfect match
           str))))

(require 'browse-url)

(defun apdl-browse-ansys-apdl-manual ()
  "Open the Ansys Parametric Design Language Guide in a browser."
  (interactive)
  (let ((file "ans_apdl/Hlp_P_APDLTOC.html")
        (path apdl-ansys-help-path))
    (if path
        (browse-url-of-file (concat "file://" path file))
      (if (string= apdl-current-ansys-version "v201")
          (browse-url (concat "https://ansyshelp.ansys.com/account/secured?returnurl=/Views/Secured/corp/" apdl-current-ansys-version "/en/" file))
        (browse-url (concat "https://ansyshelp.ansys.com/account/secured?returnurl=/Views/Secured/corp/" apdl-current-ansys-version "/" file))))))

(defun apdl-browse-apdl-help (&optional arg)
  "Browse the Ansys help for APDL commands, elements and other topics.

ATTENTION: If you are using the Ansys online help - default since
V19 - then first you need to register at the Ansys help site!
Please start some help topic with an Ansys application first, the
you can use APDL-Mode for pin-pointing your topics.  For an even
faster access I recommend installing the local Ansys help.

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
\"ALL\"LINKS -- Link radiation/conduction/convection/thermal-electric/spar/\
elem.
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
\"ALL\"TRANS -- Electromechanical solid/transducer elem."
  (interactive "P")
  (let (file
        (path apdl-ansys-help-path)
        command)
    (if arg
        (setq command (completing-read "Browse help for keyword [TAB to complete]: "
                                       apdl-help-index))
      (setq command (apdl-search-keyword)))
    (setq file (nth 1 (assoc-string command apdl-help-index t)))
    (unless  file
      (error "Keyword \"%s\" is not uniquely completable" command))

    ;; ;; we must adapt the path to various items!
    ;; ;;  since 201 not any longer

    ;; (cond
    ;;  ((string-match "_C_" file)
    ;;   (setq file (concat "ans_cmd/" file)))
    ;;  ((string-match "_E_" file)
    ;;   (setq file (concat "ans_elem/" file)))
    ;;  ((string-match "_P_APDL" file)
    ;;   (setq file (concat "ans_apdl/" file)))
    ;;  ((string-match "_G_AdvTOC" file)
    ;;   (setq file (concat "ans_adv/" file)))
    ;;  ((string-match "_G_StrTOC" file)
    ;;   (setq file (concat "ans_str/" file)))
    ;;  ((string-match "ans_mat.html" file)
    ;;   (setq file (concat "ans_mat/" file)))
    ;;  ((string-match "ctectoc.html" file)
    ;;   (setq file (concat "ans_ctec/" file)))
    ;;  ((string-match "ansysincrelease" file)
    ;;   (setq file (concat "ai_rn/" file)))
    ;;  ((string-match "ansys.theory" file)
    ;;   (setq file (concat "ans_thry/" file))))
    (if path
	(progn
	  (when (eq browse-url-browser-function 'eww-browse-url)
	      (switch-to-buffer-other-window nil))
	  (browse-url-of-file (concat "file:///" path file)))
      (unless apdl-current-ansys-version
        (error "Please set `apdl-current-ansys-version'"))
      ;; since v201 changed the path to the online help:
      (if (string= apdl-current-ansys-version "v201")
	  (browse-url
	   (concat
	    "https://ansyshelp.ansys.com/account/secured?returnurl=/Views/Secured/corp/"
	    apdl-current-ansys-version "/en/" file))
        (browse-url
	 (concat
	  "https://ansyshelp.ansys.com/account/secured?returnurl=/Views/Secured/corp/"
	  apdl-current-ansys-version "/" file))))))

(defun apdl-process-status ()
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
  (let ((status (process-status apdl-process-name)))
    (if status
        (message "Ansys process is in state \"%s\"" ; process identification No: %d"
                 (symbol-name status))
      (message "No Ansys interpreter process is running."))))

(defun apdl-occur ()
  "Show selected licenses in an occur buffer.
Interesting licenses are compiled in the string
`apdl-license-occur-regexp' which is used in the function
`apdl-license-status'."
  (interactive)
  (occur
   (mapconcat 'identity apdl-license-occur-regexp "\\|")))

(defun apdl-user-license-status ()
  "Display the Ansys license user status.
Show the status for the user `apdl-username' in a separate
buffer, the license type variable `apdl-license' determines a
highlighting of the license server summary rows.  There are
additional keybindings for the license buffer:
- `g' for updating the license status
- `Q' for killing the Buffer
- `h' for this help and
- `l' for the general license status and
- `q' for burying the *User-licenses* buffer."
  (interactive)
  (cond
   ((and apdl-lmutil-program apdl-license-file)
    ;; lmutil calls with many license server specified takes loooooonnnnggg
    (message "Retrieving user licenses, this may take some time...")
    (with-current-buffer (get-buffer-create "*User-licenses*")
      (delete-region (point-min) (point-max)))
    ;; syncronous call
    (call-process apdl-lmutil-program nil "*User-licenses*" nil "lmstat" "-c "  apdl-license-file  "-a")
    (let ((user apdl-username))
      (with-current-buffer "*User-licenses*"
        ;; below key settings are only allowed in fundamental mode
        ;; otherwise it supposedly overwrites major modes keymaps!
        (local-set-key (kbd "Q") 'kill-this-buffer)
        (local-set-key (kbd "q") 'bury-buffer)
        (local-set-key (kbd "h") '(describe-function 'apdl-user-license-status))
        (local-set-key (kbd "l") 'apdl-license-status)
        (local-set-key (kbd "g") 'apdl-user-license-status)

        ;; remove empty lines
        (goto-char (point-min))
        (delete-matching-lines "^$")

        ;; remove lines with expiry: date
        (goto-char (point-min))
        (delete-matching-lines "expiry:")

        ;; shorten lines
        (goto-char (point-min))
        (while (re-search-forward "Total of \\|Users of " nil t)
          (replace-match ""))

	;; keep either lines with user or lines not beginning with
	;; whitespace
	(goto-char (point-min))
	(keep-lines (concat "^[^ ]\\|" user))

	;; keep double line only with user in 2nd line
	(goto-char (point-min))
	(keep-lines (concat "^[^ ]+.*\n[ ]+" user))

        ;; add some comments
        (goto-char (point-min))
        (insert (propertize
                 (concat " -*- User license status" ;" from " apdl-license-file
                         " type h for help -*-\n") 'face 'match))
        (goto-char (point-max))
        (insert "\n")
        (insert (propertize (concat (current-time-string) "\n")
                            'face 'match))

        ;;  on Windows the license stat buffer doesn't move to point without:
        (unless apdl-is-unix-system-flag
          (set-window-point (get-buffer-window "*User-licenses*") (point)))))
    (unless (equal (current-buffer) (get-buffer "*User-licenses*"))
      (display-buffer "*User-licenses*" 'otherwindow))
    (message "Updated user license status: %s." (current-time-string)))
   (t
    (message "No license information or lmutil program found"))))

(defun apdl-license-status ()
  "Display the Ansys license status or start the license tool.
Show the status in a separate buffer, the license type variable
`apdl-license' determines a highlighting of the license server
summary rows.  There are additional keybindings for the license
buffer:
- `g' for updating the license status,
- `o' for showing an occur buffer with the interesting licenses from
      `apdl-license-occur-regexp',
- `u' for displaying all the user license,
- 'h' for showing this help,
- `Q' for killing the Buffer and
- `q' for burying it below another buffer."
  (interactive)
  (cond
   ((and apdl-lmutil-program apdl-license-file)
    ;; lmutil calls with many license server specified takes loooooonnnnggg
    (message
     "Retrieving license (%s) status, this may take some time..." apdl-license)
    (with-current-buffer (get-buffer-create "*Licenses*")
      (delete-region (point-min) (point-max)))
    ;; syncronous call
    (call-process apdl-lmutil-program nil "*Licenses*" nil "lmstat" "-c "
		  apdl-license-file  "-a")
    (let (bol eol)
      (with-current-buffer "*Licenses*"
        ;; remove uninteresting licenses
        ;; (goto-char (point-min))
        ;; (delete-matching-lines "\\<acfx\\|\\<ai\\|\\<wbunix\\|\\<rdacis\\>")

        ;; below key settings are only allowed in fundamental mode
        ;; otherwise it supposedly overwrites major modes keymaps!
        (local-set-key (kbd "Q") 'kill-this-buffer)
        (local-set-key (kbd "q") 'bury-buffer)
        (local-set-key (kbd "g") 'apdl-license-status)
        (local-set-key (kbd "o") 'apdl-occur)
        (local-set-key (kbd "o") '(describe-function 'apdl-license-status))
        (local-set-key (kbd "u") 'apdl-user-license-status)

        ;; ;; remove users
        ;; (goto-char (point-min))
        ;; (while (not (eobp))
        ;;   (push-mark (point))
        ;;   (search-forward-regexp "Users of " nil t)
        ;;   (beginning-of-line)
        ;;   (delete-region (mark) (point))
        ;;   (forward-line 1))
        ;; (goto-char (point-max))
        ;; (push-mark (point))
        ;; (search-backward-regexp "Users of " nil t)
        ;; (forward-line 1)
        ;; (delete-region (mark) (point))

        ;; remove empty lines
        (goto-char (point-min))
        (delete-matching-lines "^$")

        ;; remove lines with expiry: date
        (goto-char (point-min))
        (delete-matching-lines "expiry:")

        ;; shorten lines
        (goto-char (point-min))
        (while (re-search-forward "Total of \\|Users of " nil t)
          (replace-match ""))

        ;; ;; sorting
        ;; (sort-lines nil (point-min) (point-max))

        ;; add some comments
        (goto-char (point-min))
        (insert (propertize
                 (concat " -*- License status" ; from " apdl-license-file
                         ", type h for help -*-\n") 'face 'match))
        (goto-char (point-max))
        (insert "\n")
        (insert (propertize (concat (current-time-string) "\n")
                            'face 'match))
        ;; higlight current -license-type
        (goto-char (point-min))
        (search-forward-regexp apdl-license nil t)
        (forward-line)
        (setq eol (point))
        (forward-line -1)
        (setq bol (point))
        (put-text-property bol eol 'face 'font-lock-warning-face)
        ;;  on Windows the license stat buffer doesn't move to point without:
        (unless apdl-is-unix-system-flag
          (set-window-point (get-buffer-window "*Licenses*") (point)))))
    (unless (equal (current-buffer) (get-buffer "*Licenses*"))
      (display-buffer "*Licenses*" 'otherwindow))
    (message "Updated license status: %s." (current-time-string)))
   (t
    (message "No license information or lmutil program found"))))

;; starting in GUI mode (/menu,on) does inhibit the process intercommunication
;; => /menu,graph
;; env variable ANS_CONSEC=YES disables dialog boxes

(defun apdl-start-graphics ()
  "Start - in interactive mode - the MAPDL display window."
  (interactive)
  (unless
      (apdl-process-running-p)
    (error "No interactive MAPDL process is running"))
  (progn (comint-send-string
          (get-process apdl-process-name)
          ;; "/show,X11c\n/menu,grph\n"
          "/show,3d\n/menu,grph\n")
         (display-buffer (concat "*" apdl-process-name "*") 'other-window)))

(defun apdl-start-pzr-box ()
  "Start the Ansys Pan/Zoom/Rotate dialog box."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/ui,view\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/ui,view\n")
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or Classics GUI can be found"))))

(defun apdl-iso-view ()
  "Show current display in isometric view (/view,,1,1,1)."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/view,,1,1,1\n/replot\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/view,,1,1,1\n/replot\n")
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or Classics GUI can be found"))))

(defun apdl-move-up (arg)
  "Move geometry up ARG steps in the graphics window.
A Negative ARG moves ARG steps down."
  (interactive "p")
  (cond
   (apdl-classics-flag
    (kill-new (format "/focus,,,-0.25*(%d),,1\n/replot\n" arg))
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name)
                        (format "/focus,,,-0.25*(%d),,1\n/replot\n" arg))
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No Ansys process is running"))))

(defun apdl-move-down (arg)
  "Move geometry down ARG steps in the graphics window.
A Negative ARG moves ARG steps up."
  (interactive "p")
  (apdl-move-up (- arg)))

(defun apdl-move-right (arg)
  "Move geometry right ARG steps in the graphics window.
A Negative ARG moves ARG steps left."
  (interactive "p")
  (cond
   (apdl-classics-flag
    (kill-new (format "/focus,,-0.25*(%d),,,1\n/replot\n" arg))
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name)
                        (format "/focus,,-0.25*(%d),,,1\n/replot\n" arg))
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No Ansys process is running"))))

(defun apdl-move-left (arg)
  "Move geometry left ARG steps in the graphics window.
A Negative ARG moves ARG steps right."
  (interactive "p")
  (apdl-move-right (- arg)))

(defun apdl-zoom-in ()
  "Zoom into the graphics window."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/dist,,.7,1\n/replot\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/dist,,.7,1\n/replot\n") ; valid in any processor
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or Classics GUI can be found"))))

(defun apdl-zoom-out ()
  "Zoom out of the graphics window."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/dist,,1.4,1\n/replot\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/dist,,1.4,1\n/replot\n") ; valid in any processor
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or no Classics GUI can be found"))))

(defun apdl-replot ()
  "Replot the Ansys graphics screen."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/replot\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/replot\n") ; valid in any processor
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or no Classics GUI can be found"))))

(defun apdl-fit ()
  "Fit FEA entities to the Ansys interactive graphics screen."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/dist\n/replot\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/dist\n/replot\n")
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or no Classics GUI can be found"))))

(defun apdl-ansys-program ( exec)
  "Change the Ansys executable name to EXEC.
And set the variable `apdl-ansys-program' accordingly if the for
executable EXEC can be found on the system's search path."
  (interactive "FAnsys interpreter executable: ")
  (when (string= exec "")
    (setq exec apdl-ansys-program))
  (setq apdl-ansys-program exec)
  (if (executable-find exec)
      (message "apdl-ansys-program is set to \"%s\"." apdl-ansys-program)
    (error "Cannot find Ansys interpreter executable \"%s\" on the system" exec)))

(defun apdl-ansys-help-program ( exec)
  "Change the Ansys help executable to EXEC and check for its existence.
And store the value EXEC in the variable `apdl-ansys-help-program'."
  (interactive "FAnsys help executable: ")
  (when (string= exec "")
    (setq exec apdl-ansys-help-program))
  (setq apdl-ansys-help-program exec)
  (if (executable-find exec)
      (message "apdl-ansys-program is set to \"%s\"." exec)
    (error "Cannot find the Ansys help executable \"%s\" on the system" exec)))

(defun apdl-lmutil-program ( exec)
  "Change the Ansys license management utility executable to EXEC.
And specify it in the variable `apdl-lmutil-program'.  The
function inserts the string `default-directory' in the prompt
when the variable `insert-default-directory' is not nil.  For
Lin64 it is the 'lmutil' executable
/ansys_inc/shared_files/licensing/linx64/lmutil.  For Windows the
anslic_admin utility: `C:\\Ansys Inc\\Shared
Files\\licensing\\win64\\anslic_admin.exe'"
  (interactive "FAnsys License Management Utility executable: ")
  (when (string= exec "")               ; use default
    (setq exec apdl-lmutil-program))
  (setq apdl-lmutil-program exec)
  (if (executable-find exec)
      (message "apdl-lmutil-program is set to \"%s\"." apdl-lmutil-program)
    (error "Cannot find Ansys LM Utility executable \"%s\" on this system" exec)))

;;;###autoload
(defun apdl-job ()
  "Change the Ansys job name.
And write it into the variable `apdl-job'."
  (interactive)
  (let ((job-name))
    (if apdl-job
        (setq job-name (read-string "job name: " apdl-job))
      (setq job-name (read-string "job name: ")))
    (if (string= job-name "")
        (error "Variable `job-name' must not be the empty string")
      (message (concat "Job name is set to \"" job-name "\".")))
    (setq apdl-job job-name)))

(defun apdl-no-of-processors ()
  "Change the No of processors to use for an Anys run.
The number of processors will be put into the integer variable
`apdl-no-of-processors'.  If this number is below 3 the variable
won't affect the run definition since the default No of
processors (if available) for a structural analysis in Ansys is
2."
  (interactive)
  (let ((no-string (number-to-string apdl-no-of-processors))
        no
        query
        s)
    (setq query (concat "Put in the No of processors to use [" no-string "]: ")
          s (read-string query nil nil no-string)
          no (string-to-number s))
    (if (integerp no)
        (setq apdl-no-of-processors no)
      (error "Specified number is not an integer"))
    (message "No of processors for the next run definition is %d" apdl-no-of-processors)))

(defun apdl-license-file (file)
  "Change the Ansys license file name or license server(s).
And specify the string FILE in the variable `apdl-license-file'
which can either be the license file name or license server(s)
specification.  The server specification must include the port
number (default port 1055), multiple server names are separated
by colons `:' on Linux, semi-colons `;' on Windows , for example
\"27005@rbgs421x:27005@rbgs422x\". The license specification is
stored in the environment variable APDL-LICENSE-FILE."
  (interactive
   (list (read-string
          (concat "License server or license file [" apdl-license-file "]: ")
          nil nil apdl-license-file)))
  (cond ((null file)
         (buffer-name))
        (t
         (setq apdl-license-file file)
         (setenv "AnsysLMD_LICENSE_FILE" file)
         (message (concat "Set apdl-license-file to \""
                          apdl-license-file "\".")))))

(defun apdl-ansysli-servers ( servers)
  "Change the Ansys interconnect servers to SERVERS.
And specify it in the variable `apdl-ansysli-servers'.  The
server specification must include the port number even when it is
2325, the default port number: port_number@server_name, multiple
server names are separated by a colon, for example
\"rbgs421x:rbgs422x:...\"."
  (interactive
   (list (read-string (concat "Interconnect license server(s) [" apdl-ansysli-servers "]: ")
                      nil nil apdl-ansysli-servers)))
  (cond ((null servers)
         (buffer-name))
        (t
         (setq apdl-ansysli-servers servers)
         (setenv "AnsysLI_SERVERS" servers)
         (message (concat "Set apdl-ansysli-servers to \""
                          apdl-ansysli-servers "\".")))))

;; FIXME:
;; (error "Please specify the license server information with
;;     the `apdl-license-file' function or either set
;;     AnsysLMD_LICENSE_FILE or LM-LICENSE-FILE environment
;;     variable")

(defun apdl-license ()
  "Change the Ansys license type.
And store it in the variable `apdl-license'."
  (interactive)
  (let ((lic (if (not (string= apdl-license ""))
                 apdl-license
               "struct")))
    (setq apdl-license
          (completing-read (concat "License type [" lic "] (TAB for completion): ")
                           apdl-license-categories
                           nil nil nil nil lic))
    (message (concat "Ansys license type is now set to \"" apdl-license "\"."))))

(provide 'apdl-process)

;;; apdl-process.el ends here

;; Local Variables:
;; mode: outline-minor
;; indicate-empty-lines: t
;; show-trailing-whitespace: t
;; word-wrap: t
;; time-stamp-active: t
;; time-stamp-format: "%:y-%02m-%02d"
;; End:
