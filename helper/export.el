;;; export.el --- for org-latex and md export -*- lexical-binding: t -*-

;; Copyright (C) 2015-2021  H. Dieter Wilhelm

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Keywords: convenience

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

;; stuff for compiling latex to pdf

;;; Code:


;; (add-to-list 'load-path "/home/dieter/org/elisp/org-mode/lisp")
;(org-reload)
(require 'ox-md)
(require 'ox-beamer)
(add-to-list 'org-latex-classes '(
                                  "A-M" "\\documentclass{beamer}"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}") \
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             )

;;; export.el ends here
