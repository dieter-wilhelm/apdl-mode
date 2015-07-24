;;; Conti.el --- Company configurations              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hans-Dieter Wilhelm

;; Author: Hans-Dieter Wilhelm <uidg1626@sbav106x.vs.de.conti.de>
;; Keywords: ANSYS

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

;; 

;;; Code:

(setq ansys-current-ansys-version "161")
(setq ansys-current-update-version "0")
;; needed for our non default `ansys_inc/16.1.0/v161/' ANSYS
;; installation path on Linux...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following is calculated with above please don't touch it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ansys-company-flag t)

(defvar ansys-company-flag nil
  "Flag to switch company dependened stuff.")

(defun ansys-current-update-version ()
  "Specify the required (and installed) ANSYS update version.
For example \"7\" from \"16.1.0\" instead of \"0\", which is the
current default."
  (interactive)
  (let* ((dir ansys-install-directory)
	 path
	 (setq version ansys-current-ansys-version)
	 (setq major (substring version 0 2))
	 (setq minor (substring version 2))
	 (setq update ansys-current-update-version)
	 (version
	  (read-string
	   (concat "Specify the required ANSYS version [" update "]:")
	   update
	   'ansys-current-update-version-history)))
    (cond
     ((ansys-is-unix-system-p)
      (setq path
	    (concat dir "/ansys_inc/"
		    major "." minor "." update
		    "/v" version)))
     (t
      (setq path (concat dir "\\ANSYS Inc\\"
		    major "." minor "." update
		    "\\v" version))))
    (if (file-readable-p path)
	(progn
	  (setq ansys-current-ansys-version version)
	  (message "Set current ANSYS version to %s." version)
	  (ansys-initialise-conti t))
      (error "ANSYS V%s not present: %s not readable" version path))))


(defun initialise-conti ()
  "Initialise company dependened."
  (let* (update
	 version
	 minor
	 major)
    (setq version ansys-current-ansys-version)
    (setq major (substring version 0 2))
    (setq minor (substring version 2))
    (setq update ansys-current-update-version)

    (setq ansys-program
	  (concat "/appl/ansys_inc/"
		  major "." minor "." update
		  "/v" version
		  "/ansys/bin/ansys" version))

    (setq ansys-launcher
	  (concat "/appl/ansys_inc/"
		  major "." minor "." update
		  "/v" version
		  "/ansys/bin/launcher" version))

    (setq ansys-wb
	  (concat "/appl/ansys_inc/"
		  major "." minor "." update
		  "/v" version
		  "/Framework/bin/Linux64/runwb2"))

    (setq ansys-help-path (concat "/appl/ansys_inc/"
				  major "." minor "." update
				  "/v" version
				  "/commonfiles/help/en-us/help/"))

    (setq ansys-help-program
	  (concat "/appl/ansys_inc/"
		  major "." minor "." update
		  "/v" version
		  "/ansys/bin/anshelp" version))

    (setq ansys-lmutil-program 
	  (concat "/appl/ansys_inc/"
		  major "." minor "." update
		  "/shared_files/licensing/linx64/lmutil"))

    (when (null ansys-license-file)
      (setq ansys-license-file "32002@ls_fr_ansyslmd_ww_1.conti.de"))

    (when (null ansys-ansysli-servers)
      (setq ansys-ansysli-servers "2325@ls_fr_ansyslmd_ww_1.conti.de"))))

(provide 'Conti)
;;; Conti.el ends here
