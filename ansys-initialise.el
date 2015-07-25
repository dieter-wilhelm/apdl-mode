;;; ansys-initialise.el --- initialisation code      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  H. Dieter Wilhelm

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Version: 161-2
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

(defconst ansys-mode_version "2"
  "ANSYS-Mode version number.")

(defconst ansys-version_ "161"
  "ANSYS version on which ANSYS-Mode is based upon.")

;; Ansys calls their bug fixed/patched releases between major releases
;; "updates, they are not named in the default installation path, the
;; older update version files are normally overwritten."
(defconst ansys-update_version nil
  "Current ANSYS update version.
Used also internally to signal a no-default ANSYS installation
path, if not nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defcustoms
(defgroup ANSYS-initialise nil
  "Initialisation subgroup for ANSYS-Mode."
  :group 'ANSYS)

(defcustom ansys-install-directory nil
  "This is the directory path where ANSYS has been installed.
Which is to say the path before \"ansys_inc\" under Linux or
\"Ansys Inc\" under Windows."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-current-ansys-version ansys-version_
  "String of the used ANSYS version.
This variable is used by the `ansys-skeleton-header' template and
for setting up variables defaults with ANSYS path specifications,
like in the variable `ansys-program'."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-current-update-version ansys-update_version
  "String of the used ANSYS update version. ANSYS calls their bug
  fixed/patched releases \"updates\"."
  :type 'string
  :group 'ANSYS-initialise)

;; FIXME: the following should actually be variables...

(defcustom ansys-program nil
  "This string variable stores the ANSYS executable.
Under GNU-Linux this should be the solver, under Windows just the
launcher.  When the respective executable is not in your search
path, you have to specify the full qualified file name and not
only executable's name.  For example:
\"/ansys_inc/v161/ansys/bin/ansys161\" and not only \"ansys161\".
You might customise this variable or use the function
`ansys-program' to do this for the current session only."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-launcher nil
  "This string variable stores the ANSYS launcher executable.
When the respective executable is not in your search path, you
have to specify the full qualified file name and not only
executable's name.  For example:
\"/ansys_inc/v161/ansys/bin/launcher161\".  You might customise this
variable permanently or use the function `ansys-launcher' to do
this for the current session only."
  :type 'string
  :group 'ANSYS-initialise)

(defcustom ansys-wb nil
  "This string variable stores the ANSYS WorkBench executable.
When the respective executable is not in your search path, you
have to specify the full qualified file name and not only
executable's name.  For example:
\"/ansys_inc/v161/Framework/bin/Linux64/runwb2\".  You might
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
locations are \"/ansys_inc/v161/ansys/bin/anshelp161\" on GNU-Linux
and \"c:\\\\Program Files\\Ansys\
Inc\\v161\\commonfiles\\help\\HelpViewer\\ANSYSHelpViewer.exe\" on
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

(defvar ansys-current-ansys-version-history '("160" "150" "145")
  "History list for the minibuffer input of
  `ansys-current-ansys-version'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defun ansys-read-ansyslmd-ini (type)
  "Read the ANSYS license server configuration file for license TYPE.
If TYPE is nil return the license servers, if non-nil the
ansysli_servers.  When there are no license servers readable,
return nil."
  (let* ((idir ansys-install-directory)
	 ini
	 servers
	 ansysli)
    (if (ansys-is-unix-system-p)
	(setq ini (concat idir "/ansys_inc/shared_files/licensing/ansyslmd.ini"))
      (setq ini (concat idir "\\ANSYS Inc\\Shared Files\\Licensing\\ansyslmd.ini")))
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

(defun ansys-initialise ( &optional force)
  "Initialise the customisation variables.
When argument FORCE is non-nil overwrite already set
customisation variables"

  ;; 1) -current-ansys-version: In its definition 161

  ;; 2) -install-directory
  (when (null ansys-install-directory)
    (let* ((version  ansys-current-ansys-version)
	   ;; we have to remove `ansys_inc/v161' or `ANSYS Inc\v161' from AWP_ROOT161
	   (root (getenv (concat "AWP_ROOT" version)))
	   (dir (if root
		    (file-name-directory
		     (directory-file-name
		      (file-name-directory root)))
		  nil)))
      (cond (dir
	     (if (not (file-readable-p dir))
		 (message "Directory in AWP_ROOT not readable")
	       (message "Set ansys-install-directory from environment AWP_ROOT")
	       (message "ansys-install-directory = %s" dir)))
	    ((string= window-system "x")
	     (when (file-readable-p (concat "/ansys_inc/v" version))
	       ;; "/" is the ANSYS default installation directory on GNU-Linux
	       (setq dir "/")))
	    (t
	     (when (file-readable-p (concat "C:\\Program Files\\ANSYS Inc\\v" version))
	       ;; ANSYS default is "C:\\Program Files\\" on Windows
	       (setq dir "C:\\Program Files\\"))))
      (if dir
	  (setq ansys-install-directory dir)
	(message "No ANSYS default installation directory found"))))

  ;; 3) -dynamic-highlighting-flag: In its definition

  ;; 4) -job: in its definition

  ;; 5) -program
  (when (or (null ansys-program) force)
    (let* ((version ansys-current-ansys-version)
	   (update ansys-current-update-version)
	   (idir (unless (null ansys-install-directory)
		   (file-name-directory ansys-install-directory)))
	   (exe ""))
      (cond
       (update
	(setq exe
	      (concat "/appl/ansys_inc/"
		      major "." minor "." update
		      "/v" version
		      "/ansys/bin/ansys" version)))
       ((string= window-system "x")
	(setq exe
	      (concat
	       idir
	       ;; here follows the ANSYS default directory structure
	       "/ansys_inc/v" version "/ansys/bin/ansys" version)))
       (t
	(setq exe
	      (concat
	       idir
	       ;; here follow the ANSYS default directory structure
	       "\\ANSYS Inc\\v" version
	       "\\ansys\\bin\\winx64\\ansys" version ".exe"))))
      (if (file-executable-p exe)
	  (progn
	    (setq ansys-program exe)
	    (message (concat "ansys-program set to " ansys-program)))
	(message "Couldn't find default executable for ansys-program."))))

  ;; 6) -wb
  (when (or (null ansys-wb) force)
    (let* ((version ansys-current-ansys-version)
	   (update ansys-current-update-version)
	   (idir (unless (null ansys-install-directory)
		   (file-name-directory ansys-install-directory)))
	   exe)
      (cond
       (update
	(setq exe
	      (concat "/appl/ansys_inc/"
		      major "." minor "." update
		      "/v" version
		      "/Framework/bin/Linux64/runwb2")))
       ((string= window-system "x")
	(setq exe
	      (concat
	       idir
	       ;; here follows the ANSYS default directory structure
	       "/ansys_inc/v" version "/Framework/bin/Linux64/runwb2")))
       (t
	(setq exe
	      (concat
	       idir
	       ;; here follow the ANSYS default directory structure
	       "\\ANSYS Inc\\v" version
	       "\\Framework\\bin\\Win64\\RunWB2.exe"))))
      (when (file-executable-p exe)
	(setq ansys-wb exe))
      (if ansys-wb
	  (message (concat "ansys-wb set to " ansys-wb))
	(message "Couldn't find an executable for ansys-wb."))))

;; 7) -launcher
  (when (or (null ansys-launcher) force)
    (let* ((version ansys-current-ansys-version)
	   (update ansys-current-update-version)
	   (idir (unless (null ansys-install-directory)
		   (file-name-directory ansys-install-directory)))
	   exe)
      (cond
       (update
	(setq exe
	      (concat "/appl/ansys_inc/"
		      major "." minor "." update
		      "/v" version
		      "/ansys/bin/launcher" version)))
       ((string= window-system "x")
	(setq exe
	      (concat
	       idir
	       ;; here follows the ANSYS default directory structure
	       "/ansys_inc/v" version "/ansys/bin/launcher" version)))
       (t
	(setq exe
	      (concat
	       idir
	       ;; here follow the ANSYS default directory structure
	       "\\ANSYS Inc\\v" version
	       "\\ansys\\bin\\winx64\\launcher" version ".exe"))))
      (when (file-executable-p exe)
	(setq ansys-launcher exe))
      (if ansys-launcher
	  (message (concat "ansys-launcher is set to " ansys-launcher))
	(message "Couldn't find launcher, %s is not executable."
	exe))))

  ;; -dynamic-highlighting-flag: t in its definition

  ;; 8) -help-path
  (when (or (null ansys-help-path) force)
    (let ((idir ansys-install-directory)
	  (version ansys-current-ansys-version)
	  (update ansys-current-update-version)
	  path)
      (cond
       (update
	(setq path
	      (concat "/appl/ansys_inc/"
		      major "." minor "." update
		      "/v" version
		      "/commonfiles/help/en-us/help/")))
       ((string= window-system "x")
	(setq path
	      (concat idir "/ansys_inc/v" version
		      "/commonfiles/help/en-us/help/")))
       (t
	(setq path
	      (concat idir "\\ANSYS Inc\\v" version
		      "\\commonfiles\\help\\en-us\\help\\"))))
      (if (file-readable-p path)	;path must be a string, not nil
	(progn
	  (setq ansys-help-path path)
	  (message "Set ansys-help-path to %s" path))
	(message "Couldn't find an ansys-help-path"))))

  ;; 9) -help-program
  (when (or (null ansys-help-program) force)
    (let* ((idir ansys-install-directory)
	   (version ansys-current-ansys-version)
	   (update ansys-current-update-version)
	   exe)
      (cond
       (udate 
	(setq exe
	      (concat "/appl/ansys_inc/"
		      major "." minor "." update
		      "/v" version
		      "/ansys/bin/anshelp" version)))
       ((string= window-system "x")
	(setq ansys-help-program
	      (concat idir "ansys_inc/v" version
		      "/ansys/bin/anshelp" version)))
       (t
	(setq exe
	      (concat idir "ANSYS Inc\\v" version
		      "\\commonfiles\\help\\HelpViewer\\ANSYSHelpViewer.exe"))))
      (if (file-executable-p exe)
	  (progn
	    (message "ansys-help-program = %s" exe)
	    (setq ansys-help-program exe))
	(message "Found no ansys-help-program on your system"))))

  ;; 10) -lmutil-program
  (when (or (null ansys-lmutil-program) force)
    (let ((idir ansys-install-directory)
	  (version ansys-current-ansys-version)
	  (update ansys-current-update-version)
	  exe)
      (cond
       (udate
	(setq ansys-lmutil-program 
	      (concat "/appl/ansys_inc/"
		      major "." minor "." update
		      "/shared_files/licensing/linx64/lmutil")))
       ((string= window-system "x")
	(setq exe (concat idir
			  "ansys_inc/shared_files/licensing/linx64/lmutil")))
       (t
	(setq exe (concat idir
			  "ANSYS Inc\\Shared Files\\Licensing\\winx64\\lmutil.exe"))))
      (if (file-executable-p exe)
	  (progn
	    (setq ansys-lmutil-program exe)
	    (message "ansys-lmutil-program = %s" exe))
	(message "Found no ansys-lmutil-program on your system"))))

  ;; 11) -license-file
  (when (null ansys-license-file)
    (let* ((update ansys-current-update-version)
	   (lic (ansys-read-ansyslmd-ini nil))
	   (lic1 (getenv "ANSYSLMD_LICENSE_FILE")) ; ANSYS doesn't use LM_LICENSE_FILE
	   (lic2 (when update "32002@ls_fr_ansyslmd_ww_1.conti.de")))
     (cond
      (lic
       (setq ansys-license-file lic)
       (message "Read content of ansyslmd.ini")
       (message "ansys-license-file=%s" lic))
      (lic1
       (setq ansys-license-file lic1)
       (message "Read environment variable ANSYSLMD_LICENSE_FILE")
       (message "ansys-license-file=%s" lic1))
      ;; (lic2
      ;;  (setq ansys-license-file lic2)
      ;;  (message "Read environment variable MD_LICENSE_FILE")
      ;;  (message "ansys-license-file=%s" lic2))
      (lic2 
       (setq ansys-license-file lic2)
       (message "Conti server: ansys-license-file=%s" lic2))
      (t
       (message "Found no default ansys-license-file from environment or ini file"))
      )))

    ;; 12) -ansysli-servers, the Interconnect license server(s)
   (when (null ansys-ansysli-servers)
     (let* ((update ansys-current-update-version)
	    (lic (ansys-read-ansyslmd-ini t))
	    (lic1 (getenv "ANSYSLI_SERVERS"))
	    (lic2 (when update "2325@ls_fr_ansyslmd_ww_1.conti.de")))
       (cond
	(lic
	 (setq ansys-ansysli-servers lic)
	 (message "Read content of ansyslmd.ini")
	 (message "ansys-ansysli-servers=%s" lic))
	(lic1
	 (setq ansys-ansysli-servers lic1)
	 (message "Read environment variable ANSYSLI_SERVERS")
	 (message "ansys-ansysli-servers=%s" lic1))
	(lic2
	 (setq ansys-ansysli-servers lic2)
	 (message "Conti server: ansys-ansysli-servers=%s" lic2))
	)
	(ansys-license-file ;ANSYS assumes the following as the last resort as well
			    ;FIXME: but only in anslic_admin I think
	 (setq ansys-ansysli-servers
	       (replace-regexp-in-string "[0-9]*@" "2325@" ansys-license-file))
	 (message "Assuming the same servers for Interconnect with default port")
	 (message "ansys-ansysli-servers=%s" ansys-ansysli-servers))
	(t
	 (message "Found no ansys-ansyslic-servers from environment or ini file"))))
   (message "Initialised system dependened variables."))  ;; end of init function

(defun ansys-install-directory ()
  "Change the ANSYS installation directory.
This is the path before the directory `ansys_inc/' under Linux or
`ANSYS Inc\\' under Windows."
  (interactive)
  (let* ((idir ansys-install-directory)
	 path
	 (ndir
	  (expand-file-name	       ;in case it was written ~
	   (file-name-as-directory	;in case the slash is forgotten
	    (read-directory-name
	     (concat "Specify the ANSYS installation directory ["
		     idir "]:")
	     nil nil idir)))))
    (cond ((ansys-is-unix-system-p)
	   (setq path (concat ndir "ansys_inc")))
	  (t
	   (setq path (concat ndir "ANSYS Inc"))))
    (if (file-readable-p path)
	(progn
	  (setq ansys-install-directory
		(file-name-as-directory ndir)) ;ensure final slash
	  (message
	   (concat
	    "Set ansys-install-directory to \"" ndir "\".")))
      (error "ANSYS directory \"%s\" is not readable" path))
    (ansys-initialise 'force)))


(defun ansys-current-ansys-version ()
  "Specify the required (and installed) ANSYS version."
  (interactive)
  (let* ((dir ansys-install-directory)
	 path
	 (setq version ansys-current-ansys-version)
	 (setq major (substring version 0 2))
	 (setq minor (substring version 2))
	 (setq update ansys-current-update-version)
	 (update
	  (read-string
	   (concat "Specify the required ANSYS update version ["
	   update "]:") update
	   'ansys-current-update-version-history)))
    (setq path
	  (concat dir "/ansys_inc/"
		  major "." minor "." update
		  "/v" version))
    (if (file-readable-p path)
	(setq ansys-current-ansys-version update)
      (error "ANSYS update version \"%s\" not present, path %s not readable"
	     update path))
    (message "Set current ANSYS version to %s." update)
    (ansys-initialise 'force)))

(defun ansys-current-update-version ()
  "Specify the required (and installed) ANSYS update version.
This variable is only necessary for a special (company
dependened) non-standard ANSYS installation tree.  For example
\"7\" from \"16.1.0\" instead of \"0\"."
  (interactive)
  (let* ((dir ansys-install-directory)
	 path
	 (setq version ansys-current-ansys-version)
	 (setq major (substring version 0 2))
	 (setq minor (substring version 2))
	 (setq update ansys-current-update-version)
	 (update
	  (read-string
	   (concat "Specify the required ANSYS update version ["
	   update "]:") update
	   'ansys-current-update-version-history)))
    (setq path
	  (concat dir "/ansys_inc/"
		  major "." minor "." update
		  "/v" version))
    (if (file-readable-p path)
	(setq ansys-current-ansys-version update)
      (error "ANSYS update version \"%s\" not present, path %s not readable"
	     update path))
    (message "Set current ANSYS version to %s." update)
    (ansys-initialise 'force)))

(provide 'ansys-initialise)
;;; ansys-initialise.el ends here
