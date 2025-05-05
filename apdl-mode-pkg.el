;;; apdl-mode-pkg.el --- Defining the APDL-Mode package -*-lexical-binding:t -*-

;; Copyright (C) 2006 - 2025 H. Dieter Wilhelm

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Version: 20.7.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages, convenience, tools, Ansys, APDL
;; URL: https://github.com/dieter-wilhelm/apdl-mode

;; Maintainer: H. Dieter Wilhelm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
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

;;; Commentary:

;; APDL-Mode (formerly ANSYS-Mode) represents, in conjunction with
;; the GNU-Emacs editor, an advanced APDL environment.  Including
;; features features like immediate documentation access, keyword
;; completion, code templates, dedicated keybindings, solver
;; communication (GNU-Linux only), license reporting, etc.

;; ------------------------------
;; First usage:

;; Open an APDL-Mode buffer with `M-x apdl'.  From there you might
;; investigate the menu entries `APDL' and `Ansys'.

;; For the APDL-Mode help with a brief tutorial and further guidance
;; please type `C-h m' or type `C-h i' to read its documentation with
;; the Info reader."

;;; Code:

;; (require 'package)

(define-package "apdl-mode" "20.7.0" "Major mode for the APDL programming language."
  '((emacs "25.1"))
  :keywords
  '("APDL" "Ansys" "languages" "FEA" "convenience" "tools")
  :authors
  '(("H. Dieter Wilhelm" . "dieter@duenenhof-wilhelm.de"))
  :maintainer
  '("H. Dieter Wilhelm" . "dieter@duenenhof-wilhelm.de")
  :url "https://github.com/dieter-wilhelm/apdl-mode")

;; (provide 'apdl-mode-pkg)

;;; apdl-mode-pkg.el ends here

;; Local Variables:
;; indicate-empty-lines: t
;; no-byte-compile: t
;; show-trailing-whitespace: t
;; End:
