;; Example customisation file for Ansys mode under Unix and Windows
;; `default_.el' can be used as a system wide startup file after
;; adjusting and renaming it to `default.el'.  And moving it e. g. to
;; `/usr/local/share/emacs/site-lisp' or `EMACS_INSTALLDIR/site-lisp'
;; Copyright (C) 2006 - 2010  H. Dieter Wilhelm GPL V3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-path

(add-to-list 'load-path "C:\\DIRECTORY-PATH\\WHERE\\THE\\ANSYS-MODE\\FILES\\RESIDE")
;for example: "c:\\emacs\\emacs-23.1" with Windows
;or "/usr/local/src/emacs-23.1/site-lisp" with Gnu/Linux 8-)

(setq ansys-current-ansys-version "120") ;which Ansys version is used 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fontification (highlighting) levels 0,1,2 are available

(setq ansys-highlighting-level 1)
;; experimental user variables highlighting only in level 2 available
;(setq ansys-dynamic-highlighting-flag t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoloading of command definitions

    (autoload 'ansys-mode "ansys-mode" nil t)
    (autoload 'ansys-customise-ansys "ansys-mode" "Activate the function for 
calling a special Ansys customisation buffer." 'interactive)
     (autoload 'ansys-abort-file "ansys-mode" "Activate the function for  aborting Ansys runs." 'interactive)
     (autoload 'ansys-display-error-file "ansys-mode" "Activate the function for inspecting the Ansys error file." 'interactive)
     (autoload 'ansys-start-ansys-help "ansys-mode" "Activate the function for starting the Ansys help browser." 'interactive)
; TODO: -license-file, -license-program variables
     (autoload 'ansys-license-status "ansys-mode" "Activate the function for displaying Ansys license status or starting a license utility." 'interactive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file suffixes for autoloading of ansys-mode

;; appropriate file suffixes for which Ansys mode is automatically
;; called for
(add-to-list 'auto-mode-alist '("\\.mac$" . ansys-mode))
;; .mac is the macro suffix of ansys i. e. these files can be called
;; directly from the command line (without the suffix)
(add-to-list 'auto-mode-alist '("\\.inp$" . ansys-mode))
;; this is the suffix of WorkBench solver input files
(add-to-list 'auto-mode-alist '("\\.anf$" . ansys-mode))
;; this is the suffix for "Ansys Neutral Files".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; processes stuff

(if (string= window-system "x")		;Unix
    (progn
      ;; The Ansys executable
      (setq  ansys-program (concat "/appl/ansys_inc/v" ansys-current-ansys-version "/ansys/bin/ansys" ansys-current-ansys-version)

      ;; Tool for the license status
      (setq ansys-lmutil-program "/appl/ansys_inc/shared_files/licensing/linx64/lmutil")

      ;; Ansys help browser executable
      (setq ansys-help-file (concat "/appl/ansys_inc/v" ansys-current-ansys-version "/ansys/bin/anshelp" ansys-current-ansys-version)))
  ;; windows
  (progn
    (setq ansys-lmutil-program "C:\\Program Files\\Ansys Inc\\Shared Files\\licensing\\intel\\anslic_admin.exe")
					;backslash '\"' on windows mandatory
    (setq ansys-help-file (concat "\"C:\\Program Files\\Ansys Inc\\v"  ansys-current-ansys-version "\\CommonFiles\\HELP\\en-us\\ansyshelp.chm\""))))

;; for starting the solver & ansys-license-status
(setq ansys-license "struct"	     ;which license to use for the run
      ansys-job "file"			;job name
      ansys-license-file "27005@lic-rbg1:27005@lic-rbg2" ;
					;license servers (or license file name)
					;specify even the default port for lmutil 
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto insertion stuff (in the moment of creating a new APDL file) ;;

(auto-insert-mode 1)
(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-query t)
(add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton-compilation]))
