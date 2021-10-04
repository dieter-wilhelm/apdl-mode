;;; apdl-initialise.el --- Initialisation code for APDL-Mode -*- lexical-binding: t -*-
;; Time-stamp: <2021-10-01 22:58:02 dieter>

;; Copyright (C) 2016 - 2021  H. Dieter Wilhelm

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Version: 20.7.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages, convenience, extensions
;; URL: https://github.com/dieter-wilhelm/apdl-mode

;; Maintainer: H. Dieter Wilhelm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Initialisation code:

;; Read the Ansys installation dependent parameters mainly from
;; environment variables.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(defconst apdl-mode-version "20.7.0"
  "The APDL-Mode version string.")

(defconst apdl-mode-update "2021-10-04"
  "APDL-Mode packaging date string in yyyy-mm-dd format.
This is for the APDL-Mode development versions to check the time
of packaging.")

;; safer way to get the installation directory? -TODO-
;; (defconst superfrobnicator-base (file-name-directory load-file-name))
;; (defun superfrobnicator-fetch-image (file)
;;  (expand-file-name file superfrobnicator-base))
(defconst apdl-mode-install-directory
  (file-name-directory
   (or (locate-library "apdl-mode")
       buffer-file-name))		;use this file location
  "The installation directory string of APDL-Mode.
The string contains the directory where the APDL-Mode Elisp files
reside.")

(defconst apdl-ansys-version "v201"
  "Ansys version string on which APDL-Mode is based upon.
With respect to keywords and documentation, like deprecated
elements, command names, etc.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defcustoms

(defgroup APDL-initialise nil
  "Initialisation subgroup for APDL-Mode."
  :group 'APDL)

(defcustom apdl-username nil
  "Regex string representing the user name or user ID.
This variable is a regular expression used to display only the
user's license(s) from the license pool. Please see the function
`apdl-user-license-status'."
  :type 'string
  :group 'APDL-initialise)

;; -TODO-: are environment variables also set under GNU-Linux?
(defcustom apdl-ansys-install-directory nil
  "This is the path where the MAPDL solver (Ansys) has been installed.
Which is to say the path up to (and including) the Ansys version
number, for example \"/ansys_inc/v201/\".  The Ansys installation
routine sets for this path an environment variable, for the
former versioning example: \"AWP_ROOT201\".  With other words:
this customisation variable includes besides the installation
root directory also the information which Ansys version is
currently in use."
  :type 'directory
  :group 'APDL-initialise)

;; TODO: the following defcustoms can actually be variables, can't
;; they?

(defcustom apdl-ansys-program nil
  "This string variable stores the Ansys executable.
Under GNU-Linux this should be the solver, under Windows just the
launcher.  When the respective executable is not in your search
path, you have to specify the full qualified file name and not
only executable's name.  For example:
\"/ansys_inc/v201/ansys/bin/ansys195\" and not only \"ansys195\".
You might customise this variable or use the function
`apdl-ansys-program' to do this for the current session only."
  :type '(file :must-match t)
  :group 'APDL-initialise)

(defcustom apdl-ansys-launcher nil
  "This string variable stores the Ansys launcher executable.
When the respective executable is not in your search path, you
have to specify the full qualified file name and not only
executable's name.  For example:
\"/ansys_inc/v162/ansys/bin/launcher162\".  You might customise this
variable permanently or use the function `apdl-ansys-launcher' to do
this for the current session only."
  :type '(file :must-match t)
  :group 'APDL-initialise)

(defcustom apdl-ansys-wb nil
  "This string variable stores the Ansys WorkBench executable.
When the respective executable is not in your search path, you
have to specify the full qualified file name, not only
executable's name.  For example:
\"/ansys_inc/v201/Framework/bin/Linux64/runwb2\".  You might
customise this variable permanently or use the function
`apdl-ansys-wb' to do this for the current session only."
  :type '(file :must-match t)
  :group 'APDL-initialise)

(defcustom apdl-ansys-help-program nil
  "The Ansys help viewer executable.
It is called with
\\[apdl-start-ansys-help] (`apdl-start-ansys-help').  When the
executable is not in the search path, you have to complement the
executable with its complete path.  For example the default
locations are \"/ansys_inc/v162/ansys/bin/anshelp162\" on
GNU-Linux and \"c:/Program Files/Ansys
Inc/v162/commonfiles/help/HelpViewer/AnsysHelpViewer.exe\" on
Windows (Windows 10)."
  :type '(file :must-match t)
  :group 'APDL-initialise)

(defcustom apdl-ansys-help-path nil
  "The Ansys help path."
  :type 'directory
  :group 'APDL-initialise)

(defcustom apdl-lmutil-program nil
  "A FlexLM license manager executable.
For example: \"/ansys_inc/shared_files/licensing/linx64/lmutil\"
or in case of a Windows OS \"c:/Program Files/Ansys Inc/Shared
Files/Licensing/anslic_admin.exe\".  This variable is used for
displaying the license status or starting the anslic_admin tool
under Windows with the function `apdl-license-status'."
  :type '(file :must-match t)
  :group 'APDL-initialise)

(defcustom apdl-license-file nil
  "The FlexLM license file name or license server specification(s).
The license server specification(s) should include the port
number even if it's the default port 1055 because the lmutil tool
needs it in the following way: port_number@server_name, use the
colon for multiple servers, for example
\"27005@rbgs421x:27005@rbgs422x\".

Setting this variable skips the effect of previously set
environment variables, which have the following order of
precedence: 1. AnsysLMD_LICENSE_FILE environment variable, 2.)
The FLEXlm resource file: ~/.flexlmrc on GNU-Linux or somewhere in the
Windows registry. 3.) The LM_LICENSE_FILE variable. 4.) The
ansyslmd.ini file in the licensing directory (This is what
anslic_admin is doing in an Ansys recommended installation).  5.)
The license file itself."
  :type 'string
  :group 'APDL-initialise)

(defcustom apdl-ansysli-servers nil
  "Used to identify the server machine for the Licensing Interconnect.
Set it to port@host.  The default port is 2325."
  :type 'string
  :group 'APDL-initialise)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; declaring functions

(declare-function apdl-is-unix-system-p "apdl-mode")
(declare-function apdl-classics-p "apdl-process")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defining variables

(defvar apdl-initialised-flag nil
  "Non-nil means that APDL-Mode is already initialised.")

(defvar apdl-current-ansys-version nil
  "String of the currently used MAPDL solver version.
This variable is used by the `apdl-skeleton-header' template and
for setting up variables defaults with Ansys path specifications,
like in the variable `apdl-ansys-program'.  The content looks
like: \"v201\"")

;; from -mode.el
(defvar apdl-is-unix-system-flag)

;; from -process.el
(defvar apdl-classics-flag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defun apdl-read-ansyslmd-ini (type)
  "Read the Ansys license server configuration file for license TYPE.
If TYPE is nil return the license servers, if non-nil the
ansysli_servers.  When there are no license servers readable,
return nil."
  (let* ((idir
          (if apdl-ansys-install-directory
              (file-name-directory
               (directory-file-name apdl-ansys-install-directory))
            nil))
         ini)
    (if apdl-is-unix-system-flag
        (setq ini (concat idir "shared_files/licensing/ansyslmd.ini"))
      (setq ini (concat idir "Shared Files/Licensing/ansyslmd.ini")))
    (message "Checking license file: %s" ini)
    (if (file-readable-p ini)
        (with-temp-buffer
          (insert-file-contents ini)
          (if type 			;ansli_server or not
	      ;; I think word search doesn't distinct capitalisation
	      ;; word search is not working with "=" for Emacs-24!!
              (word-search-forward "AnsysLI_SERVERS=" nil t)
            (word-search-forward "SERVER=" nil t))
          (search-forward-regexp ".*" nil t)
          (match-string-no-properties 0)) ; TODO: there's no check
      ;; against empty ini!
      (message "File %s not readable" ini)
      nil)))

(defun apdl-find-path-environment-value ()
  "Find the latest AWP_ROOTXXX environment value.
Which is to say find the Ansys root path with the largest
installed versioning number and check the accessibility of the
content."
  (let ((dir
         (car
          (reverse
           (sort
            (remove nil
                    (mapcar (lambda (str)
                              (when
                                  (string-match
                                   "AWP_ROOT[0-9][0-9][0-9]=\\(.*\\)"
                                   str)
                                (match-string 1 str)))
                            process-environment))
            'string<)))))
    (if (null dir)
        (progn
          (message "No AWP_ROOTXXX environment variable")
          nil)
      (if (file-readable-p dir)
          (progn
            (message "Found Ansys root directory in environment: %s" dir)
            dir)
        (message "Environment AWP_ROOTXXX set but value is not readable")
        nil))))

;;;###autoload
(defun apdl-initialise ( &optional force)
  "Initialise the customisation variables.
When argument FORCE is non-nil overwrite some of the already set
customisation variables."
  (message "Initialising Ansys installation dependent parameters ...")
  ;; 0) -unix-system-flag, system environment
  (setq apdl-is-unix-system-flag (apdl-is-unix-system-p))

  (unless apdl-username
      (setq apdl-username (or (getenv "USERNAME")
      ;; centos 7.9
      (getenv "USER"))))

  ;; 1) -install-directory
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;            !!!! with Ansys version information!!!!
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; if apdl-ansys-install-directory is set we need a way to get
  ;; apdl-current-ansys-version!

  (when apdl-ansys-install-directory	;set by user
    (let((subdir
          (file-name-nondirectory
	   (directory-file-name apdl-ansys-install-directory))))
      (setq apdl-current-ansys-version subdir) ; (remove ?v subdir))
      (message "Current Ansys version: %s" apdl-current-ansys-version)))

  (unless apdl-ansys-install-directory
    (let* ((cdir "/appl/ansys_inc/")	; this is a corporate path
	   ;; from environment variable below, I think only under
	   ;; Windows!?
           (path (apdl-find-path-environment-value))
           (dir (if (null path)
                    nil
                  (file-name-as-directory path)))
           subdir)
      (cond
       (dir 				; from environment
        (setq apdl-ansys-install-directory dir)
        (message
         "apdl-ansys-install-directory set from environment variable
AWP_ROOTXXX")
        (message "apdl-ansys-install-directory = %s" dir)
        (setq subdir
              (file-name-nondirectory (directory-file-name dir)))
        (setq apdl-current-ansys-version subdir) ; (remove ?v subdir))
        (message "Current Ansys version: %s" apdl-current-ansys-version))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; from company Linux installation path
       ((file-readable-p cdir)
        (setq subdir
              (car
               (reverse
                (directory-files cdir nil "[0-9][0-9]\.[0-9]"))))
        (setq apdl-current-ansys-version (remove ?. (substring subdir 0 4)))
        (setq dir (concat cdir subdir apdl-current-ansys-version "/")))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; default installation path on Linux "/" or rather "/usr"
       ;; /ansys_i..nc is a symlink to /usr/ansys_inc!
       ((apdl-is-unix-system-p)
	(message "Checking the default path under a Unix system.")
        (setq cdir "/ansys_inc/")
        (when (file-readable-p cdir)
          (setq subdir
                (car
                 (reverse
                  (directory-files cdir nil "v[0-9][0-9][0-9]"))))
          (setq apdl-current-ansys-version (substring subdir 0 4))
          (message "Current Ansys version: %s" apdl-current-ansys-version)
          (setq dir (concat cdir subdir "/"))))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; default installation path on windows
       (t
        (setq cdir "C:/Program Files/Ansys Inc/")
        ;; search for the latest version
        (when (file-readable-p cdir)
          (setq subdir
                (car
                 (reverse
                  (directory-files cdir nil "v[0-9][0-9][0-9]" 'string<))))
          (setq apdl-current-ansys-version (substring subdir 0 4))
          ;; (remove ?v (substring subdir 0 4)))
          (message "Current Ansys version: %s" apdl-current-ansys-version)
          (setq dir (concat cdir subdir "/")))))
      (if dir
	  (setq apdl-ansys-install-directory dir)
	(message "No Ansys installation directory found"))))

    ;; ;; 1a) -classics-flag ; not supported any longer 2020-03
    ;; (let* ()
    ;;   (if (and apdl-is-unix-system-flag (apdl-classics-p))
    ;;       (setq apdl-classics-flag t)))

    ;; ;; 2) -current-apdl-version: is in the -ansys-install-directory
    ;; ;; included

  ;; 3) -ansys-program under Win10 seems to be case sensitive!  Since
  ;; at least v191 there is also an MAPDL.exe
  (when (and apdl-ansys-install-directory
	     (or (null apdl-ansys-program) force))
    (let* ((version1 (remove ?v apdl-current-ansys-version))
	   (idir (file-name-directory apdl-ansys-install-directory))
	   (exe (if apdl-is-unix-system-flag
		    (concat idir "ansys/bin/ansys" version1)
		  (concat idir "ansys/bin/winx64/MAPDL.exe"))))
      (if (file-executable-p exe)
	  (progn
	    (setq apdl-ansys-program exe)
	    (message (concat "apdl-ansys-program set to " apdl-ansys-program)))
	(message "Couldn't find an executable for apdl-ansys-program."))))

    ;; 4) -wb
    (when (and apdl-ansys-install-directory (or (null apdl-ansys-wb) force))
      (let* ((idir apdl-ansys-install-directory)
             (exe
              (if apdl-is-unix-system-flag
                  (concat idir "Framework/bin/Linux64/runwb2") ; 150, 161
                (concat idir "Framework/bin/Win64/RunWB2.exe" )))) ; 195
        (when (file-executable-p exe)
          (setq apdl-ansys-wb exe))
        (if apdl-ansys-wb
            (message (concat "apdl-ansys-wb set to " apdl-ansys-wb))
          (message "Couldn't find an executable for apdl-ansys-wb."))))

    ;; 5) -launcher
    (when (and apdl-ansys-install-directory (or (null apdl-ansys-launcher) force))
      (let* ( (idir (when  apdl-ansys-install-directory
                      (file-name-directory apdl-ansys-install-directory)))
              (exe
               ;; since v191 there is no launcher191.exe, only
               ;; launcher.exe...
               (if apdl-is-unix-system-flag
                   (concat idir "ansys/bin/launcher")
                 (concat idir  "ansys/bin/winx64/launcher.exe"))))
        (when (file-executable-p exe)
          (setq apdl-ansys-launcher exe))
        (if apdl-ansys-launcher
            (message "apdl-ansys-launcher is set to %s" apdl-ansys-launcher)
	  (message "Couldn't find an executable for apdl-ansys-launcher (%s)."
		   exe))))

    ;; 6) -help-path
    (when (and apdl-ansys-install-directory (or (null apdl-ansys-help-path)
                                                force))
      (let* ((idir apdl-ansys-install-directory)
             (path (concat idir "commonfiles/help/en-us/help/")))
        (if (file-readable-p path) ; path must be a string, not nil
            (progn
              (setq apdl-ansys-help-path path)
              (message "Set apdl-ansys-help-path to %s" path))
          (message "%s" "Couldn't find the apdl-ansys-help-path"))))

    ;; 7) -help-program, checked v201, linux as well
    (when (and apdl-ansys-install-directory (or (null apdl-ansys-help-program)
                                                force))
      (let* ((idir apdl-ansys-install-directory)
	     (version1 (remove ?v apdl-current-ansys-version))
             (exe
              (if apdl-is-unix-system-flag
                  (concat idir "ansys/bin/anshelp" version1)
		;; cygwin distincts capitalisation of execs (v201:
		;; ANSYSHelpViewer)
                (concat idir "commonfiles/help/HelpViewer/ANSYSHelpViewer.exe"))))
        (if (file-executable-p exe)
            (progn
              (message "apdl-ansys-help-program = %s" exe)
              (setq apdl-ansys-help-program exe))
          (message
           "%s"
           "Couldn't find an executable for apdl-ansys-help-program."))))

    ;; 8) -lmutil-program
    (when (and apdl-ansys-install-directory (or (null apdl-lmutil-program) force))
      (let* ((idir (file-name-directory
                    (directory-file-name
                     apdl-ansys-install-directory)))
             (exe
              (if apdl-is-unix-system-flag
                  (concat idir "shared_files/licensing/linx64/lmutil")
                (concat idir "Shared Files/Licensing/winx64/lmutil.exe"))))
        (if (file-executable-p exe)
            (progn
              (setq apdl-lmutil-program exe)
              (message "apdl-lmutil-program = %s" exe))
          (message "%s" "Couldn't find an executable for apdl-lmutil-program"))))

    ;; 9) -license-file
    (unless apdl-license-file
      (let* (
             (lfile "AnsysLMD_LICENSE_FILE")
             (lic (apdl-read-ansyslmd-ini nil))
             (lic1 (getenv lfile)) ; Ansys doesn't use LM_LICENSE_FILE
             ;; corporate stuff
             (lic2 (if (file-readable-p "/appl/ansys_inc")
                       "32002@ls_fr_ansyslmd_ww_1.conti.de")))
        (cond
         (lic
          (setq apdl-license-file lic)
          (message "%s" "Read content of ansyslmd.ini")
          (message "apdl-license-file=%s" lic))
         (lic1
          (setq apdl-license-file lic1)
          (message "Read environment variable %s" lfile)
          (message "apdl-license-file=%s" lic1))
         (lic2
          (setq apdl-license-file lic2)
          (message "Conti server: apdl-license-file=%s" lic2)
          (setenv lfile lic2))
         (t
          (message
           "%s"
           "Found no default apdl-license-file from environment or ini file")))))

    ;; 10) -ansysli-servers, the Interconnect license server(s)
    (unless apdl-ansysli-servers
      (let* (
             (lfile "AnsysLI_SERVERS")
             (lic (apdl-read-ansyslmd-ini t))
             (lic1 (getenv lfile))
             (lic2 (if (file-readable-p "/appl/ansys_inc")
                       "2325@ls_fr_ansyslmd_ww_1.conti.de")))
        (cond
         (lic
          (setq apdl-ansysli-servers lic)
          (message "%s" "Read content of ansyslmd.ini")
          (message "apdl-ansysli-servers=%s" lic))
         (lic1
          (setq apdl-ansysli-servers lic1)
          (message "Read environment variable %s" lfile)
          (message "apdl-ansysli-servers=%s" lic1))
         (lic2
          (setq apdl-ansysli-servers lic2)
          (message "Conti server: apdl-ansysli-servers=%s" lic2)
          (setenv lfile lic2))
         (apdl-license-file ; Ansys assumes the following as the last
			    ; resort as well
          ;; FIXME: but only in anslic_admin I think
          (setq apdl-ansysli-servers
                (replace-regexp-in-string "[0-9]*@" "2325@" apdl-license-file))
          (message
           "%s" "Assuming the same servers for Interconnect with default port")
          (message "apdl-ansysli-servers=%s" apdl-ansysli-servers))
         (t
          (message
           "%s" "Found no apdl-ansyslic-servers from environment or ini file")))))
    ;; ------------------------------------------------------------
    (setq apdl-initialised-flag t)
    (message "%s" "\nAPDL-Mode: Initialised system dependent variables."))
;; end of init function

(defun apdl-ansys-install-directory ()
  "Change the Ansys installation directory.
Which is to say the path up to (and including) the Ansys version
number, for example \"/ansys_inc/v201/\".  The path is stored in
the variable `apdl-ansys-install-directory'"
  (interactive)
  (let* ((idir apdl-ansys-install-directory)
         (ndir
          (expand-file-name                ; in case it was written ~
           (file-name-as-directory         ; in case the slash is forgotten
            (read-directory-name
             (concat "Specify the Ansys installation directory ["
                     idir "]:")
             idir idir))))
         (length (length ndir))
         (version (substring (directory-file-name ndir)
                             (- length 5) (- length 1))))
    (message "a-i-d: %s" ndir)
    (if (file-readable-p ndir)
        (progn
          (setq apdl-ansys-install-directory
                (file-name-as-directory ndir)) ; ensure final slash
          (message "Set apdl-ansys-install-directory to \"%s\"." ndir))
      (error "Ansys directory \"%s\" is not readable" ndir))
    (apdl-initialise 'force)
    (setq apdl-current-ansys-version version)))

(provide 'apdl-initialise)

;;; apdl-initialise.el ends here

;; Local Variables:
;; indicate-empty-lines: t
;; show-trailing-whitespace: t
;; time-stamp-format: "%:y-%02m-%02d"
;; End:
