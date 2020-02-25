;;; apdl-wb-template.el --- APDL WorkBench templates for the APDL-Mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  H. Dieter Wilhelm GPL V3

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Maintainer: H. Dieter Wilhelm
;; Version: 20.2.0
;; Package-Requires: ((emacs "25"))
;; Keywords: languages, convenience, ANSYS, tools, APDL
;; URL: https://github.com/dieter-wilhelm/apdl-mode


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

;;; Comment:
;; Convention used for outlining
;; !@ is surrounded by 30 equal signs  ==============================
;; !@@ by 30 dashes ------------------------------
;; !@@@ by 30 dots ..............................
;; and empty lines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:

;; Collection of templates for WorkBench and AIM Command (APDL)
;; objects

;;; Code:

(declare-function apdl-mode "apdl-mode")

;; skeleton-insert
;; docu string
;; interactor string or nil
;; strings "here comes\n" str | "default" " again."

;; default values
(define-skeleton apdl-template-wb-test
  "Write greetings"
  "Type name of idiot: "
  "hello, " str | "Otto von Bernstein" "!\n"
  "Here it goes.\n")

;; skeleton in skeleton
;; _ interesting / cursor region
(define-skeleton apdl-template-wb-test2
  "Write greetings"
  "Type name of idiot: "
  ("Put a name in: " "hello, " str "!\n")
   "Here "_ "it goes.\n")

(provide 'apdl-wb-template)

;;; apdl-template.el ends here

;; Local Variables:
;; mode: outline-minor
;; indicate-empty-lines: t
;; show-trailing-whitespace: t
;; word-wrap: t
;; End:
