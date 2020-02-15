;;; apdl-config.el -- Customisation file for APDL-Mode under GNU-Linux and Windows
;; This file was created from the file "apdl-config.org"!
;; Created: {{{date}}}

;; Copyright (C) 2016 - 20202 H. Dieter Wilhelm, GPL V3

;;; Comentary:

;; `apdl-config.el' can be used as a configuration file (after moving
;; it e. g. to `/usr/share/emacs/site-lisp' or
;; `c:\\EMACS_INSTALLDIR\\site-lisp').  Yet this file is then loaded
;; AFTER Emacs' user configuration file `~/.emacs' (or `~/.emacs.el'
;; or `~/.emacs.d/init.el')!  If you intend to change the following
;; settings with 1) Emacs' customisation system or 2) changing them
;; directly in your configuration file, you MUST either set the
;; variable `inhibit-default-init' to `t' "(setq inhibit-default-init
;; t)" in your configuration file or remove or rename `a-m.el' (or
;; at least its corresponding sections) otherwise clashing settings in
;; `.emacs' will be overwritten!!!

;;; Commentary:

;; The comment sign is `;' (one semi-colon ;) Textual hints are
;; indicated with DOUBLE semi-colons `;;', optionally uncomment the
;; code lines with a SINGLE comment sign.

;;; CODE:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       ;; IMPORTANT PREREQUISIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !!! If the APDL-Mode files (*.el) are NOT placed in a default
;; Emacs load-path it is necessary to adjust the following
;; variable:!!!  Hint: The directory site-lisp/ in the Emacs
;; installation tree, for example, is in its default load-path.

;(add-to-list 'load-path "C:\\DIRECTORY-PATH\\WHERE\\THE\\APDL-MODE\\FILES\\RESIDE")

;; for example: "c:\\emacs\\apdl-mode" for a Windows system or
;; "/usr/local/src/emacs/apdl-mode" for a GNU-Linux system.

;; For reading the ANSYS help in your browser it should be sufficient
;; to set at most the ANSYS version and the installation directory of
;; ANSYS (if they are differing from the defaults and restart Emacs
;; ;-), the complete paths will be constructed with below information

;(setq apdl-current-apdl-version "150") ; "162" default in APDL-Mode 162-1

;; if `apdl-ansys-install-directory' is not set APDL-Mode tries to
;; initialise it from the environment variable `AWP_ROOT162'.

;; conditional: Linux or Windows
;(cond ((string= window-system "x")
;        ;; This is an example of an installation directory on GNU-Linux
;        (setq apdl-ansys-install-directory "/appl")) ; default: "/"
;       (t
;        ;;This an example of an installation dir. on WINDOWS
;        (setq apdl-ansys-install-directory "D:\\Ansys")) ; default: "C:\\Program Files"
;                                                        )

;; If the paths of your ANSYS installation is completely differing
;; from the normal ANSYS structures, please see below.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                           ;; IMPORTANT END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file suffixes for autoloading of apdl-mode, appropriate file
;; suffixes for which ANSYS mode is automatically called for

;; .mac is the macro suffix of ANSYS i. e. these macros can be called
;; in the ANSYS command prompt like a regular ANSYS function (without
;; the suffix .mac). See the file helper/example.mac
(add-to-list 'auto-mode-alist '("\\.mac$" . apdl-mode))
(add-to-list 'auto-mode-alist '("\\.ans$" . apdl-mode))

;; .dat and .inp are WorkBench's solver input file suffixes
;; See the file helper/example.dat
(add-to-list 'auto-mode-alist '("\\.dat$" . apdl-mode))
(add-to-list 'auto-mode-alist '("\\.inp\\'" . apdl-mode))

;; .anf is the suffix for "ANSYS Neutral" files which include mostly
;;  gometric data but also some APDL snippets. See the file
;;  helper/example.anf.
(add-to-list 'auto-mode-alist '("\\.anf$" . apdl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			  ;; Auto insertion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto insertion stuff (when creating a new APDL file)

(auto-insert-mode 1)		        ; insert automatically templates
(add-hook 'find-file-hook 'auto-insert) ; when opening new files
(setq auto-insert-query t)   ; aks for auto insertion of APDL template
(add-to-list 'auto-insert-alist
  '(apdl-mode . [apdl-skeleton-outline-template])) ;which template to insert

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			      ;; Autoloading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set of useful commands which are interactively available (M-x ...)
;; even when APDL Mode was not (yet) activated i.e. the lisp files not
;; loaded.

  (autoload 'apdl "apdl-mode" "Opening an empty buffer in APDL-Mode" 'interactive)
  (autoload 'apdl-mode "apdl-mode" "Switch to APDL-Mode" 'interactive)
  (autoload 'apdl-customise-ansys "apdl-mode" "Activate the function for
  calling a special ANSYS customisation buffer." 'interactive)
  (autoload 'apdl-abort-file "apdl-mode" "Activate the function for  aborting ANSYS runs." 'interactive)
  (autoload 'apdl-display-error-file "apdl-mode" "Activate the function for inspecting the ANSYS error file." 'interactive)
  (autoload 'apdl-start-apdl-help "apdl-mode" "Activate the function for starting the ANSYS help browser." 'interactive)
  (autoload 'apdl-start-ansys "apdl-mode" "Activate the function for starting the APDL interpreter under GNU-Linux or product launcher under Windows." 'interactive)
  (autoload 'apdl-start-classics "apdl-mode" "Activate the function for starting the MAPDL in GUI Mode (APDL-Classics)." 'interactive)
  (autoload 'apdl-start-wb "apdl-mode" "Activate the function for starting Workbench." 'interactive)
  (autoload 'apdl-license-status "apdl-mode" "Activate the function for displaying ANSYS license status or starting a license utility." 'interactive)
  (autoload 'apdl-mode-version "apdl-mode" "Show APDL-Mode's version number." 'interactive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			     ;; Outlining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; activating outline minor mode for selectively hiding/unhiding
;; sections

(add-hook 'apdl-mode-hook 'apdl-outline-minor-mode) ;enable outlining

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		     ;; Highlighting/Colourisation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following toggles a dynamics change of the highlighting: While
;; you are editing your new variable definitions highlighted and the
;; cursor position is shown in the parameter help overlay

;; Uncommenting the following might slow the editing of large .mac
;; files (but only when apdl-highlighting-level is set to 2, see
;; below).

;(setq apdl-dynamic-highlighting-flag nil)
;(setq apdl-dynamic-highlighting-flag t) ;default

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fontification (highlighting) of user variables and decoration
;; levels (0,1,2 are available), user variables highlighting is only
;; in level 2 available (statical, if above flag is not set), the
;; default is 2

;(setq apdl-highlighting-level 1) ; default: 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       ;; ANSYS version and paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Things you might have to configure if your ANSYS installation is
;; completely differing from default ANSYS installation paths, as in
;; the example below:

; (setq apdl-help-program
;   "/appl/ansys_inc/16.2.0/v162/commonfiles/help/HelpViewer/ANSYSHelpViewer.exe")
; ;;the ANSYS path to the help viewer looks normally like this:
; ;; "/INSTALL_DIRECTORY/ansys_inc/v162/commonfiles/help/HelpViewer/ANSYSHelpViewer.exe"
;; ;; on windows systems (note the slash before c:!)
;; (setq apdl-ansys-help-path "/c:/Program Files/ANSYS Inc/16.2.0/v162/commonfiles/help/en-us/help/")

; (setq apdl-ansys-help-path "/appl/ansys_inc/16.2.0/v162/commonfiles/help/en-us/help/")
; (setq apdl-ansys-program "/appl/ansys_inc/16.2.0/v162/ansys/bin/ansys162")
; (setq apdl-lmutil-program "/appl/ansys_inc/16.2.0/shared_files/licensing/linx64/lmutil")

;; if you want to read the manual in GNU-Emacs' EWW browser
;(setq browse-url-browser-function 'eww-browse-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;; ANSYS processes stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; license server configuration


  ;; GNU-Linux 64 bit only !!! Warning specifiying many license server
  ;; takes a long time for displaying the license status!!!

   ;; for starting the solver & apdl-license-status & ANSYS help
;  (setq                 ;
   ;; license servers (or license file name)nn
   ;; specify even the default port for lmutil (since ANSYS V 12.0) on GNU-Linux
   ;; GNU-Linux: License servers separated by colons (":"), 1055 is the default port
;   apdl-license-file
;  "32002@ls_fr_ansyslmd_ww_1.conti.de"
;   "32002@ls_fr_ansyslmd_ww_1.conti.de:32002@ls_fr_ansyslmd_ww_2.conti.de:32002@ls_fr_ansyslmd_ww_4.conti.de:1055@frlifl01.auto.contiwan.com:1055@frlifl02.auto.contiwan.com"

   ;; since ANSYS 12.0 there is an intermediate server for
   ;; the communication between flexlm and ANSYS, 2325 is the default port
;   apdl-ansysli-servers
;  "2325@ls_fr_ansyslmd_ww_1.conti.de"
;  "2325@ls_fr_ansyslmd_ww_1.conti.de:2325@ls_fr_ansyslmd_ww_3.conti.de:2325@ls_fr_ansyslmd_ww_4.conti.de:2325@frlifl01.auto.contiwan.com:2325@frlifl02.auto.contiwan.com"
;   )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   ;; options when starting the solver
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Number of cores for the run, 2 does not require HPC licenses
  ;(setq apdl-no-of-processors 8) ;default: 2

  ;;  which license type to use for the solver
  ;(setq apdl-license "ansys") ;default: "struct"

  ;; ANSYS job name
  ;(setq apdl-job "otto"); default: "file"

;; adding the directory of this (loaded) file to the load-path
(add-to-list 'load-path (file-name-directory load-file-name))
;; setting the APDL-Mode install directory
(setq apdl-mode-install-directory (file-name-directory load-file-name))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                              ;; The End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; apdl-config.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
