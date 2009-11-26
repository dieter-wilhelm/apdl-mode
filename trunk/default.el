;; example `default.el' for Ansys mode under Unix and Windows
;; `default.el' can be used as a system wide startup file.  E. g. in
;; `/usr/local/share/emacs/site-lisp' or `EMACS_INSTALLDIR/site-lisp'

(add-to-list 'load-path "C:\\DIRECTORY-PATH\\WHERE\\THE\\ANSYS-MODE\\FILES\\RESIDE")
      ;assuming you extracted the files on drive "c:"
      ;for example: "c:\\emacs\\emacs-23.1"

(autoload 'ansys-mode "ansys-mode" "Activate Ansys mode." 'interactive)
(autoload 'ansys-display-error-file "ansys-mode" "Activate Ansys display error file function." 'interactive)
(autoload 'ansys-start-ansys-help "ansys-mode" "Activate Ansys start help function." 'interactive)
(autoload 'ansys-license-status "ansys-mode" "Activate Ansys license status function." 'interactive)

;; file suffixes for which Ansys mode is automatically called
(add-to-list 'auto-mode-alist '("\\.mac$" . ansys-mode))
(add-to-list 'auto-mode-alist '("\\.inp$" . ansys-mode))
(add-to-list 'auto-mode-alist '("\\.anf$" . ansys-mode))

;; processes stuff
(setq ansys-license-file "27005@lic-rbg1")
(if (string= window-system "x")
    (progn
      ;; The Ansys executable
      (setq  ansys-program "/appl/ansys_inc/v120/ansys/bin/ansys120")
      ;; Tool for the license status
      (setq ansys-lmutil-program "/appl/ansys_inc/shared_files/licensing/linop64/lmutil")
      ;; Ansys help browser executable
      (setq ansys-help-file "/appl/ansys_inc/v120/ansys/bin/anshelp120"))
  (progn
    (setq ansys-lmutil-program "C:\\Program Files\\Ansys Inc\\Shared Files\\licensing\\intel\\anslic_admin.exe")
					;backslash '\"' on windows mandatory
    (setq ansys-help-file "\"C:\\Program Files\\Ansys Inc\\v120\\CommonFiles\\HELP\\en-us\\ansyshelp.chm\"")))

(setq ansys-licence "struct")	     ;which license to use for the run

;; auto insertion stuff (when creating an APDL file)
(auto-insert-mode 1)
(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-query t)
(add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton-compilation]))

;; experimental user variables highlighting
;(setq ansys-dynamic-highlighting-flag t)

