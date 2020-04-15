;;; apdl-config.el --- Customisation example for APDL-Mode
;; This file was built from the file "apdl-config.org".

;; Copyright (C) 2016 - 2020 H. Dieter Wilhelm, GPL V3
;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Version: 20.5.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages, convenience, tools, Ansys, APDL
;; URL: https://github.com/dieter-wilhelm/apdl-mode

;; Maintainer: H. Dieter Wilhelm
;; Created: 2006-02

;; Parts of this mode were originally base on octave-mod.el: Copyright
;; (C) 1997 Free Software Foundation, Inc.  Author: Kurt Hornik
;; <Kurt.Hornik@wu-wien.ac.at> Author: John Eaton
;; <jwe@bevo.che.wisc.edu>

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
;; The customisations itself are written in `Emacs-Lisp'.
;; The comment is ~;~ (one semi-colon `;').  Textual hints
;; are indicated with DOUBLE semi-colons `;;', optionally uncomment
;; the code lines with a SINGLE comment sign.

;; Please add the interesting code lines into your GNU-Emacs init file or
;; load this file in the init file with `(load-file
;; "PATH/apdl-config.el")'.

;;; CODE:

(cond ((string= window-system "x")
	;; This is an example of an installation directory on GNU-Linux
	(setq apdl-ansys-install-directory "/appl/ansys_inc/v201/"))
	;; the default might look like "/ansys_inc/v201/"
       (t ;This an example of an installation directory on WINDOWS
	;; Emacs is using here forward slashes as under Unix and not
	;; the backslash "\"!
	(setq apdl-ansys-install-directory "D:/Ansys Inc/v201/")))
	;; default: "C:/Program Files/Ansys Inc/v201/"

(add-to-list 'auto-mode-alist '("\\.apdl$" . apdl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file suffixes for autoloading of APDL-Mode, appropriate file
;; suffixes for which Ansys mode is automatically called for

;; .mac is the macro suffix of Ansys i. e. these macros can be called
;; in the Ansys command prompt like a regular Ansys function (without
;; the suffix .mac).  See the file doc/example.mac

;; APDL-Mode has configured GNU-Emacs to open files with this suffix
;; under apdl-mode.

;; (add-to-list 'auto-mode-alist '("\\.mac$" . apdl-mode))
(add-to-list 'auto-mode-alist '("\\.ans$" . apdl-mode))

;; .dat and .inp are WorkBench's solver input file suffixes
;; See the file doc/example.dat

;; APDL-Mode has configured GNU-Emacs to open files with these suffixes
;; under apdl-mode.

;; (add-to-list 'auto-mode-alist '("\\.dat$" . apdl-mode))
;; (add-to-list 'auto-mode-alist '("\\.inp\\'" . apdl-mode))

;; .anf is the suffix for "Ansys Neutral" files which include mostly
;;  gometric data but also some APDL snippets. See the file
;;  doc/example.anf.
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
			  ;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The amount of time the help overlay is shown from
;; (`apdl-show-command-parameters').

; (setq apdl-parameter-help-duration "2 min") ; the default
(setq apdl-parameter-help-duration 30) ; 30 seconds

;; If you want to read the manual in GNU-Emacs' EWW browser.  This
;; might only work for locally installed help documents (a 1.7 GB
;; package v201) since v191 the online help is the default help
;; system.

(setq browse-url-browser-function 'eww-browse-url)

;; You might use this variable to create you own templates
;; in `apdl-wb-template.el'.

(setq apdl-wb-custom-template-directory "c:/my_macros/") ; new in 20.4.0

;;  APDL-Mode mode configures the following variable from the
;;  evironment to show your license usage in
;;  `apdl-user-license-status'.  It is the user ID you are registered
;;  for the  license server.

(setq apdl-username "userID") 		; new in 20.4.0

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

 (setq apdl-dynamic-highlighting-flag nil)
 (setq apdl-dynamic-highlighting-flag t) ; default

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fontification (highlighting) of user variables and decoration
;; levels (0,1,2 are available), user variables highlighting is only
;; in level 2 available (statical, if above flag is not set), the
;; default is 2

 (setq apdl-highlighting-level 1) ; default: 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			 ;; Ansys version and paths
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Things you might have to configure if your Ansys installation is
  ;; completely differing from default Ansys installation paths, as in
  ;; the example below, especially if you want to use mixed version
  ;; installations of Ansys:

(setq apdl-ansys-help-program
    "/appl/ansys_inc/20.0.1/v201/commonfiles/help/HelpViewer/AnsysHelpViewer.exe")
    ;; normally it looks like this:
    ;; "/ansys_inc/v201/commonfiles/help/HelpViewer/AnsysHelpViewer.exe"
;; On WINDOWS: slash before /d: is unnecessary, but possible?
(setq apdl-ansys-help-path "d:/Program Files/Ansys Inc/16.2.0/v201/commonfiles/help/en-us/help/")
(setq apdl-ansys-help-path "/appl/ansys_inc/16.2.0/v201/commonfiles/help/en-us/help/")
(setq apdl-ansys-launcher "/appl/ansys_inc/v201/ansys/bin/winx64/launcher")
(setq apdl-ansys-wb "/appl/ansys_inc/v201/Framework/bin/Linux64/runwb2")
(setq apdl-ansys-program "/appl/ansys_inc/19.3.0/ansys/bin/ansys195")
(setq apdl-lmutil-program "/appl/ansys_inc/19.3.0/shared_files/licensing/linx64/lmutil")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;; Ansys processes stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; license server configuration


  ;; GNU-Linux 64 bit only !!! Warning specifiying many license server
  ;; takes a long time for displaying the license status!!!

   ;; for starting the solver & apdl-license-status & Ansys help
  (setq                 ;
   ;; license servers (or license file name)
   ;; specify even the default port for lmutil (since Ansys V 12.0) on GNU-Linux
   ;; GNU-Linux: License servers separated by colons (":"), 1055 is the default port
   apdl-license-file
   "32002@ls_fr_ansyslmd_ww_1.conti.de"

   ;; since Ansys 12.0 there is an intermediate server for
   ;; the communication between flexlm and Ansys, 2325 is the default port
   apdl-ansysli-servers
   "2325@ls_fr_ansyslmd_ww_1.conti.de"
   )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   ;; options when starting the solver
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Number of cores for the run, 4 does not require HPC licenses
   (setq apdl-no-of-processors 8) ; default: 4

  ;;  which license type to use for the solver
   (setq apdl-license "struct") ; default: "ansys"

  ;; Ansys job name
   (setq apdl-job "harmonics1"); default: "file"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                              ;; The End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'apdl-config)

;;; apdl-config.el ends here

;; Local Variables:
;; no-byte-compile: t
;; show-trailing-whitespace: t
;; indicate-empty-lines: t
;; time-stamp-active: t
;; time-stamp-format: "%:y-%02m-%02d"
;; End:
