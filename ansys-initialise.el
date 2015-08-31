;;; ansys-initialise.el --- initialisation code      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  H. Dieter Wilhelm

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Version: 162-1
;; Keywords: languages, convenience, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(defconst ansys-mode_version "1"
  "ANSYS-Mode version number.")

(defconst ansys-version_ "162"
  "ANSYS version on which ANSYS-Mode is based upon.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defcustoms

(defgroup ANSYS-initialise nil
  "Initialisation subgroup for ANSYS-Mode."
  :group 'ANSYS)

(defcustom ansys-mode-install-directory nil
  "The installation directory of ANSYS-Mode.
The directory where the Elisp files reside."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-install-directory nil
  "This is the path where the MAPDL solver (ANSYS) has been installed.
Which is to say the path up to the ANSYS version number, for
example \"v162\".  The ANSYS installation routine sets for this
path an environment variable, for the former versioning example:
AWP_ROOT162.  With other words: this customisation variable
includes besides the installation root directory also the
information which ANSYS version currently is in use."
  :type 'string
  :group 'ANSYS-initialise)

;; TODO: the following defcustoms can actually be variables, can't
;; they?

(defcustom ansys-program nil
  "This string variable stores the ANSYS executable.
Under GNU-Linux this should be the solver, under Windows just the
launcher.  When the respective executable is not in your search
path, you have to specify the full qualified file name and not
only executable's name.  For example:
\"/ansys_inc/v162/ansys/bin/ansys162\" and not only \"ansys162\".
You might customise this variable or use the function
`ansys-program' to do this for the current session only."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-launcher nil
  "This string variable stores the ANSYS launcher executable.
When the respective executable is not in your search path, you
have to specify the full qualified file name and not only
executable's name.  For example:
\"/ansys_inc/v162/ansys/bin/launcher162\".  You might customise this
variable permanently or use the function `ansys-launcher' to do
this for the current session only."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-wb nil
  "This string variable stores the ANSYS WorkBench executable.
When the respective executable is not in your search path, you
have to specify the full qualified file name and not only
executable's name.  For example:
\"/ansys_inc/v162/Framework/bin/Linux64/runwb2\".  You might
customise this variable permanently or use the function
`ansys-wb' to do this for the current session only."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-help-program nil
  "The ANSYS help executable.
It is called with
\\[ansys-start-ansys-help] (`ansys-start-ansys-help').  When the
executable is not in the search path, you have to complement the
executable with its complete path.  For example the default
locations are \"/ansys_inc/v162/ansys/bin/anshelp162\" on GNU-Linux
and \"c:\\\\Program Files\\Ansys\
Inc\\v162\\commonfiles\\help\\HelpViewer\\ANSYSHelpViewer.exe\" on
Windows (XP/7)."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-help-path nil
  "The ANSYS help path."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-help-program-parameters ""
  "Stores parameters for the program `ansys-help-program' under Windows.
Since ANSYS150 not longer necessary."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-lmutil-program nil
  "A FlexLM license manager executable.
For example: \"/ansys_inc/shared_files/licensing/linx64/lmutil\"
or in case of a Windows 32-bit OS \"c:\\\\Program Files\\Ansys
Inc\\Shared\ Files \\Licensing\\intel\\anslic_admin.exe.  This
variable is used for displaying the license status or starting
the ansli_admin tool under Windows with the function
`ansys-license-status'."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-license-file nil
  "The FlexLM license file name or license server specification(s).
The license server specification(s) should include the port
number even if it's the default port 1055 because the lmutil tool
needs it in the following way: port_number@server_name, use the
colon for multiple servers, for example
\"27005@rbgs421x:27005@rbgs422x\".

Setting this variable skips the effect of previously set
environment variables, which have the following order of
precedence: 1. ANSYSLMD_LICENSE_FILE environment variable, 2.)
The FLEXlm resource file: ~/.flexlmrc on GNU-Linux or somewhere in the
Windows registry. 3.) The LM_LICENSE_FILE variable. 4.) The
ansyslmd.ini file in the licensing directory (This is what
anslic_admin is doing in an ANSYS recommended installation).  5.)
The license file itself."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-ansysli-servers nil
  "Used to identify the server machine for the Licensing Interconnect.
Set it to port@host.  The default port is 2325."
  :type 'string
  :group 'ANSYS-initialise)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables

(defvar ansys-unix-system-flag nil
  "Indicate a Unix system with t.")

(defvar ansys-current-ansys-version nil
  "String of the currently used MAPDL solver version.
This variable is used by the `ansys-skeleton-header' template and
for setting up variables defaults with ANSYS path specifications,
like in the variable `ansys-program'.")

;; (defvar ansys-current-ansys-version-history '("160" "150" "145")
;;   "History list for the minibuffer input of
;;   `ansys-current-ansys-version'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defun ansys-read-ansyslmd-ini (type)
  "Read the ANSYS license server configuration file for license TYPE.
If TYPE is nil return the license servers, if non-nil the
ansysli_servers.  When there are no license servers readable,
return nil."
  (let* ((idir 
	  (if ansys-install-directory
	      (file-name-directory
	       (directory-file-name ansys-install-directory))
	    nil))
	 ini
	 )
    (if ansys-unix-system-flag
	(setq ini (concat idir "shared_files/licensing/ansyslmd.ini"))
      (setq ini (concat idir "Shared Files/Licensing/ansyslmd.ini")))
    (if (file-readable-p ini)
	(with-temp-buffer
	  (insert-file-contents ini)
	  (if type
	      (word-search-forward "ANSYSLI_SERVERS=" nil t)
	    (word-search-forward "SERVER=" nil t))
	  (search-forward-regexp ".*" nil t)
	  (match-string-no-properties 0)) ;TODO: there's no check
					  ;against empty ini!
      (message (concat "File "ini" not readable"))
      nil)))

(defun ansys-find-path-environment-value ()
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
			    process-environment)
		    )
	    'string<)))))
    (if (null dir)
	(progn
	  (message "No AWP_ROOTXXX environment variable")
	  nil)
      (if (file-readable-p dir)
	  dir
	(message "Environment AWP_ROOTXXX set but value is not readable")
	nil))))

(defun ansys-initialise ( &optional force)
  "Initialise the customisation variables.
When argument FORCE is non-nil overwrite already set
customisation variables"

  ;; 0) -unix-system-flag
  (setq ansys-unix-system-flag (ansys-is-unix-system-p))

  ;; 1) -install-directory (with versioning information)
  (when (null ansys-install-directory)
    (let* (
	   (cdir "/appl/ansys_inc/")
	   (path (ansys-find-path-environment-value))
	   (dir (if (null path)
		    nil
		  (file-name-as-directory path)))
	   subdir)
      (cond 
       ;; from environment variable
       (dir
	(message "ansys-install-directory set from environment variable AWP_ROOTXXX")
	(message "ansys-install-directory = %s" dir)
	(setq subdir 
	      (file-name-nondirectory (directory-file-name dir)))
	(setq ansys-current-ansys-version (remove ?v subdir))	
	(message "Current ANSYS version: %s" ansys-current-ansys-version))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; default company installation path
       ((file-readable-p cdir)
        (setq cdir "/appl/ansys_inc/") ;FIXME: remove
	(setq subdir 
	      (car 
	       (reverse
		(directory-files cdir nil "[0-9][0-9]\.[0-9]"))))
	(setq ansys-current-ansys-version (remove ?. (substring subdir 0 4)))
	(setq dir (concat cdir subdir "/v" ansys-current-ansys-version "/")))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; default installation path on Linux "/" or rather "/usr"
       ;; /ansys_inc is a symlink to /usr/ansys_inc
       ((string= window-system "x")
	(setq cdir "/ansys_inc/")
	(when (file-readable-p cdir)
	  (setq subdir 
		(car 
		 (reverse
		  (directory-files cdir nil "v[0-9][0-9][0-9]"))))
	  (setq ansys-current-ansys-version (remove ?v (substring subdir 0 4)))	       
	  (message "Current ANSYS version: %s" ansys-current-ansys-version)
	  (setq dir (concat cdir subdir "/"))))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; default installation path on windows
       (t
	(setq cdir "C:\\Program Files\\ANSYS Inc")
	;; search for the latest version
	(when (file-readable-p cdir)
	  (setq subdir 
		(car 
		 (reverse
		  (directory-files cdir nil "v[0-9][0-9][0-9]" 'string<))))
	  (setq ansys-current-ansys-version (remove ?v (substring subdir 0 4)))	       
	  (message "Current ANSYS version: %s" ansys-current-ansys-version)
	  (setq dir (concat cdir subdir "/")))))
      (if dir
	  (setq ansys-install-directory dir)
	(message "No ANSYS installation directory found"))))

  ;; 1a) -classics-flag
  (let* ()
    (if (and ansys-unix-system-flag (ansys-classics-p))
	(setq ansys-classics-flag t)))

  ;; 2) -current-ansys-version: 
  (when ansys-install-directory
    (let* ((idir (file-name-as-directory ansys-install-directory))
	   (version ansys-current-ansys-version))
      (when (or (and idir
		     (not version))
		force)
	(setq length (length idir))
	(setq version (substring (directory-file-name idir) (- length 4) (- length 1)))
	(setq ansys-current-ansys-version version)
	(message "ansys-current-ansys-version=%s" version))))
 
  ;; 3) -program
  (when (and ansys-install-directory (or (null ansys-program) force))
    (let* ((version ansys-current-ansys-version)
	   (idir (unless (null ansys-install-directory)
		   (file-name-directory ansys-install-directory)))
	   (exe (if ansys-unix-system-flag
		    (concat idir "ansys/bin/ansys" version)
		  (concat idir "ansys/bin/winx64/ansys"version".exe"))))
      (if (file-executable-p exe)
	  (progn
	    (setq ansys-program exe)
	    (message (concat "ansys-program set to " ansys-program)))
	(message "Couldn't find an executable for ansys-program."))))

  ;; 4) -wb
  (when (and ansys-install-directory (or (null ansys-wb) force))
    (let* ((version ansys-current-ansys-version)
	   (idir ansys-install-directory)
	   (exe 
	    (if ansys-unix-system-flag
		(concat idir "Framework/bin/Linux64/runwb2") ;150, 161
		 (concat idir "Framework/bin/Win64/RunWB2.exe" ))))
      (when (file-executable-p exe)
	(setq ansys-wb exe))
      (if ansys-wb
	  (message (concat "ansys-wb set to " ansys-wb))
	(message "Couldn't find an executable for ansys-wb."))))

;; 5) -launcher
  (when (and ansys-install-directory (or (null ansys-launcher) force))
    (let* ((version ansys-current-ansys-version)
	   (idir (unless (null ansys-install-directory)
		   (file-name-directory ansys-install-directory)))
	   (exe
	    (if ansys-unix-system-flag
		(concat idir "ansys/bin/launcher" version)
	      (concat idir  "ansys/bin/winx64/launcher" version ".exe"))))
      (when (file-executable-p exe)
	(setq ansys-launcher exe))
      (if ansys-launcher
	  (message (concat "ansys-launcher is set to " ansys-launcher))
	(message "Couldn't find an executable for ansys-launcher."
	exe))))

  ;; 6) -help-path
  (when (and ansys-install-directory (or (null ansys-help-path) force))
    (let* ((idir ansys-install-directory)
	   (path (concat idir "commonfiles/help/en-us/help/")))
      (if (file-readable-p path)	;path must be a string, not nil
	  (progn
	    (setq ansys-help-path path)
	    (message "Set ansys-help-path to %s" path))
	(message "Couldn't find the ansys-help-path"))))

  ;; 7) -help-program
  (when (and ansys-install-directory (or (null ansys-help-program) force))
    (let* ((idir ansys-install-directory)
	   (version ansys-current-ansys-version)
	   (exe
	    (if ansys-unix-system-flag
		(concat idir "ansys/bin/anshelp" version)
	      (concat idir "commonfiles/help/HelpViewer/ANSYSHelpViewer.exe"))))
      (if (file-executable-p exe)
	  (progn
	    (message "ansys-help-program = %s" exe)
	    (setq ansys-help-program exe))
	(message "Couldn'f find an executable for ansys-help-program."))))

  ;; 8) -lmutil-program
  (when (and ansys-install-directory (or (null ansys-lmutil-program) force))
    (let* ((idir (file-name-directory
		  (directory-file-name
		   ansys-install-directory)))
	   (version ansys-current-ansys-version)
	   (exe
	    (if ansys-unix-system-flag
		(concat idir "shared_files/licensing/linx64/lmutil")
	      (concat idir "Shared Files/Licensing/winx64/lmutil.exe"))))
      (if (file-executable-p exe)
	  (progn
	    (setq ansys-lmutil-program exe)
	    (message "ansys-lmutil-program = %s" exe))
	(message "Couldn't find an executable for ansys-lmutil-program"))))

  ;; 9) -license-file
  (when (null ansys-license-file)
    (let* (
	   (lfile "ANSYSLMD_LICENSE_FILE")
	   (lic (ansys-read-ansyslmd-ini nil))
	   (lic1 (getenv lfile)) ; ANSYS doesn't use LM_LICENSE_FILE
	   (lic2 (if (file-readable-p "/appl/ansys_inc")
		     "32002@ls_fr_ansyslmd_ww_1.conti.de")))
     (cond
      (lic
       (setq ansys-license-file lic)
       (message "Read content of ansyslmd.ini")
       (message "ansys-license-file=%s" lic))
      (lic1
       (setq ansys-license-file lic1)
       (message "Read environment variable %s" lfile)
       (message "ansys-license-file=%s" lic1))
      (lic2 
       (setq ansys-license-file lic2)
       (message "Conti server: ansys-license-file=%s" lic2)
       (setenv lfile lic2))
      (t
       (message "Found no default ansys-license-file from environment or ini file"))
      )))

    ;; 10) -ansysli-servers, the Interconnect license server(s)
   (when (null ansys-ansysli-servers)
     (let* (
	    (lfile "ANSYSLI_SERVERS")
	    (lic (ansys-read-ansyslmd-ini t))
	    (lic1 (getenv lfile))
	    (lic2 (if (file-readable-p "/appl/ansys_inc")
		      "2325@ls_fr_ansyslmd_ww_1.conti.de")))
       (cond
	(lic
	 (setq ansys-ansysli-servers lic)
	 (message "Read content of ansyslmd.ini")
	 (message "ansys-ansysli-servers=%s" lic))
	(lic1
	 (setq ansys-ansysli-servers lic1)
	 (message "Read environment variable %s" lfile)
	 (message "ansys-ansysli-servers=%s" lic1))
	(lic2
	 (setq ansys-ansysli-servers lic2)
	 (message "Conti server: ansys-ansysli-servers=%s" lic2)
	 (setenv lfile lic2))
	(ansys-license-file ;ANSYS assumes the following as the last resort as well
					;FIXME: but only in anslic_admin I think
	 (setq ansys-ansysli-servers
	       (replace-regexp-in-string "[0-9]*@" "2325@" ansys-license-file))
	 (message "Assuming the same servers for Interconnect with default port")
	 (message "ansys-ansysli-servers=%s" ansys-ansysli-servers))
	(t
	 (message "Found no ansys-ansyslic-servers from environment or ini file")))))
     ;; ------------------------------------------------------------
     (message "ANSYS-Mode: Initialised system dependened variables."))  ;; end of init function

(defun ansys-install-directory ()
  "Change the ANSYS installation directory.
Which is to say the path up to the ANSYS version number, for
example \"v162\".  The path is stored in the variable
`ansys-install-directory'"
  (interactive)
  (let* ((idir ansys-install-directory)
	 (ndir
	  (expand-file-name	       ;in case it was written ~
	   (file-name-as-directory	;in case the slash is forgotten
	    (read-directory-name
	     (concat "Specify the ANSYS installation directory ["
		     idir "]:")
	     idir idir))))
	 (length (length ndir))
	 (version (substring (directory-file-name ndir) (- length 4) (- length 1))))
    (message "a-i-d: %s" ndir)
    (if (file-readable-p ndir)
	(progn
	  (setq ansys-install-directory
		(file-name-as-directory ndir)) ;ensure final slash
	  (message
	   (concat
	    "Set ansys-install-directory to \"" ndir "\".")))
      (error "ANSYS directory \"%s\" is not readable" ndir))
    (ansys-initialise 'force)
    (setq ansys-current-ansys-version version)))

(provide 'ansys-initialise)
;;; ansys-initialise.el ends here
