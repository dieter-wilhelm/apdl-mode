;; Customisation file for ANSYS-Mode (GNU-Linux and Windows)

;; Copyright (C) 2006 - 2015 H. Dieter Wilhelm GPL V3

;; !!! `default.el' can be used as a configuration file (after moving
;; it e. g. to `/usr/share/emacs/site-lisp' or
;; `c:\\EMACS_INSTALLDIR\\site-lisp').  Yet this file is then loaded
;; AFTER Emacs' user configuration file `~/.emacs' (or `~/.emacs.el'
;; or `~/.emacs.d/init.el')!  If you intend to change the following
;; settings with Emacs' customisation system or changing them directly
;; in your configuration file, you must either set the variable
;; `inhibit-default-init' to `t' like in the following line

;; (setq inhibit-default-init t)

;; in your configuration file or remove or rename `default.el' (or at
;; least its corresponding sections) otherwise clashing settings in
;; your `.emacs' will be overwritten!!!

;;; CONVENTIONS

;; The comment sign is `;' (one semi-colon ;) Textual hints are
;; indicated with DOUBLE semi-colons `;;', optionally uncomment the
;; code lines with a SINGLE comment sign.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		      ;; IMPORTANT PREREQUISITES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !!! If the ANSYS-Mode files (*.el) are NOT placed in a default
;; Emacs load-path it is necessary to adjust the following
;; variable:!!!  Hint: The directory site-lisp/ in the Emacs
;; installation tree, for example, is in its default load-path.

;(add-to-list 'load-path "C:\\DIRECTORY-PATH\\WHERE\\THE\\ANSYS-MODE\\FILES\\RESIDE")

;; for example: "c:\\emacs\\ansys-mode" for a Windows system or
;; "/usr/local/src/emacs/ansys-mode" for a GNU-Linux system.


;; For reading the ANSYS help in your browser it should be sufficient
;; to set at most the ANSYS version and the installation directory of
;; ANSYS (if they are differing from the defaults and restart Emacs
;; ;-), the complete paths will be constructed with below information

;(setq ansys-current-ansys-version "150")
;(setq ansys-current-ansys-version "161") ; default in ANSYS-Mode 16.1.1

;; conditional: Linux or Windows

;(cond ((string= window-system "x")
;        ;; This is an example of an installation directory on GNU-Linux
;        (setq ansys-install-directory "/appl/")) ; default: "/"
;       (t
;        ;;This an example of an installation dir. on WINDOWS
;        (setq ansys-install-directory "D:\\Ansys\\")) ; default: "C:\\Program Files\\"
;							 )

;; If the paths of your ANSYS installation is completely differing
;; from the normal ANSYS structures, please see below.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			   ;; IMPORTANT END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   ;; File suffixes for ANSYS-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file suffixes for autoloading of ansys-mode, appropriate file
;; suffixes for which ANSYS mode is automatically called for

;; .mac is the macro suffix of ANSYS i. e. these macros can be called
;; in the ANSYS command prompt like a regular ANSYS function (without
;; the suffix .mac). See the file helper/example.mac
(add-to-list 'auto-mode-alist '("\\.mac$" . ansys-mode))

;; .dat and .inp are WorkBench's solver input file suffixes.
;; See the file helper/example.dat
(add-to-list 'auto-mode-alist '("\\.dat$" . ansys-mode))
(add-to-list 'auto-mode-alist '("\\.inp\\'" . ansys-mode))

;; .anf is the suffix for "ANSYS Neutral" files which include mostly
;;  gometric data but also some APDL snippets. See the file
;;  helper/example.anf.
(add-to-list 'auto-mode-alist '("\\.anf$" . ansys-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			  ;; Auto insertion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto insertion stuff (when creating a new APDL file)

(auto-insert-mode 1)		        ; insert automatically templates
(add-hook 'find-file-hook 'auto-insert) ; when opening new files
(setq auto-insert-query t)   ; aks for auto insertion of APDL template
(add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton-outline-template])) ;which
										   ;template

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		     ;; Autoloading of functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set of useful commands which are now interactively available (M-x
;; ...)  even when ANSYS Mode was not (yet) activated i.e. the lisp
;; files not loaded.

(autoload 'ansys-mode "ansys-mode" nil t)
(autoload 'ansys-customise-ansys "ansys-mode" "Activate the function for 
calling a special ANSYS customisation buffer." 'interactive)
(autoload 'ansys-abort-file "ansys-mode" "Activate the function for  aborting ANSYS runs." 'interactive)
(autoload 'ansys-display-error-file "ansys-mode" "Activate the function for inspecting the ANSYS error file." 'interactive)
(autoload 'ansys-start-ansys-help "ansys-mode" "Activate the function for starting the ANSYS help browser." 'interactive)
(autoload 'ansys-start-ansys "ansys-mode" "Activate the function for starting the APDL interpreter under GNU-Linux or product launcher under Windows." 'interactive)
(autoload 'ansys-license-status "ansys-mode" "Activate the function for displaying ANSYS license status or starting a license utility." 'interactive)
(autoload 'ansys-mode-version "ansys-mode" "Show ANSYS-Mode's version number." 'interactive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			     ;; Outlining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; activating outline minor mode for selectively hiding/unhiding
;; sections

(add-hook 'ansys-mode-hook 'ansys-outline-minor-mode) ;enable outlining

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		     ;; Highlighting/Colorisation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following toggles a dynamics change of the highlighting: While
;; you are editing your new variable definitions highlighted and the
;; cursor position is shown in the parameter help overlay

;; Uncommenting the following might slow the editing of large .mac
;; files (but only when ansys-highlighting-level is set to 2, see
;; below).

;(setq ansys-dynamic-highlighting-flag nil)
;(setq ansys-dynamic-highlighting-flag t) ;default

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fontification (highlighting) of user variables and decoration
;; levels (0,1,2 are available), user variables highlighting is only
;; in level 2 available (statical, if above flag is not set), the
;; default is 2

;(setq ansys-highlighting-level 1) ; default: 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		       ;; ANSYS version and paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Things you might have to configure if your ANSYS installation is
;; completely differing from default ANSYS installation paths, as in
;; the example below:

      ;  (setq ansys-help-program
      ; 	     "/appl/ansys_inc/16.1.0/v161/commonfiles/help/HelpViewer/ANSYSHelpViewer.exe")
      ;; normally it looks like this:
      ;; "/INSTALL_DIRECTORY/ansys_inc/v161/commonfiles/help/HelpViewer/ANSYSHelpViewer.exe"

      ; (setq ansys-help-path "/appl/ansys_inc/16.1.0/v161/commonfiles/help/en-us/help/")
      ; (setq ansys-program "/appl/ansys_inc/16.1.0/v161/ansys/bin/ansys161")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		       ;; ANSYS processes stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; license server configuration

      ;; GNU-Linux 64 bit

      ;; for starting the solver & ansys-license-status & ANSYS help 
;      (setq		     ;
;	;; license servers (or license file name)
;	;; specify even the default port for lmutil (since ANSYS V 12.0) on GNU-Linux
;	;; GNU-Linux: License servers separated by colons (":"), 1055 is the default port
;	ansys-license-file
;	"1055@frlifl01.auto.contiwan.com:1055@frlifl02.auto.contiwan.com"
;
;	;; since ANSYS 12.0 there is an intermediate server for
;	;; the communication between flexlm and ANSYS, 2325 is the default port
;	ansys-ansysli-servers
;	"2325@frlifl01.auto.contiwan.com:2325@frlifl02.auto.contiwan.com"
;	)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
		 ;; options when starting the solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Number of cores for the run, 2 does not require HPC licenses
;(setq ansys-no-of-processors 8) ;default: 2

;;  which license type to use for the solver
;(setq ansys-license "ansys") ;default: "struct"

;; ANSYS job name
;(setq ansys-job "otto"); default: "file"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			      ;; The End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
