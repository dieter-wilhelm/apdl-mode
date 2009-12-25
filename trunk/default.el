;; example `default.el' for Ansys mode under Unix and Windows
;; `default.el' can be used as a system wide startup file.  E. g. in
;; `/usr/local/share/emacs/site-lisp' or `EMACS_INSTALLDIR/site-lisp'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-path

(add-to-list 'load-path "C:\\DIRECTORY-PATH\\WHERE\\THE\\ANSYS-MODE\\FILES\\RESIDE")
;for example: "c:\\emacs\\emacs-23.1" with Windows
;or "/usr/local/src/emacs-23.1/site-lisp" with Gnu/Linux 8-)

(setq ansys-current-ansys-version "12.0") ;which Ansys version is used
					;used in a header template

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fontification (highlighting) levels 0,1,2 are available

(setq font-lock-maximum-decoration '((ansys-mode . 1) (t . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoloading of command definitions

(autoload 'ansys-mode "ansys-mode" "Activate Ansys mode." 'interactive)
(autoload 'ansys-customise-ansys "ansys-mode" "Activate the Ansys customisation buffer." 'interactive)
(autoload 'ansys-display-error-file "ansys-mode" "Activate Ansys display error file function." 'interactive)
(autoload 'ansys-start-ansys-help "ansys-mode" "Activate Ansys start help function." 'interactive)
(autoload 'ansys-license-status "ansys-mode" "Activate Ansys license status function." 'interactive)

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
      (setq  ansys-program "/appl/ansys_inc/v120/ansys/bin/ansys120")

      ;; Tool for the license status
      (setq ansys-lmutil-program "/appl/ansys_inc/shared_files/licensing/linop64/lmutil")

      ;; Ansys help browser executable
      (setq ansys-help-file "/appl/ansys_inc/v120/ansys/bin/anshelp120"))
  ;; windows
  (progn
    (setq ansys-lmutil-program "C:\\Program Files\\Ansys Inc\\Shared Files\\licensing\\intel\\anslic_admin.exe")
					;backslash '\"' on windows mandatory
    (setq ansys-help-file "\"C:\\Program Files\\Ansys Inc\\v120\\CommonFiles\\HELP\\en-us\\ansyshelp.chm\"")))

;; for starting the solver
(setq ansys-license "struct"	     ;which license to use for the run
      ansys-job "file"			;job name
      ansys-license-file "27005@lic-rbg1" ;license server (or file)
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto insertion stuff (in the moment of creating a new APDL file) ;;

(auto-insert-mode 1)
(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-query t)
(add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton-compilation]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; experimental user variables highlighting

;(setq ansys-dynamic-highlighting-flag t)
