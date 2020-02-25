;; adding info documentation
;; reporter
;; custom
;; align
;; easymenu
;; comint
;; browse-url

;;  "
;; APDL (Ansys Parametric Design Language) is the solver scripting
;; language of the FEA (Finite Element Analysis) suite ANSYS (ANalysis
;; SYStem, registered TM http://www.ansys.com).

;; APDL-Mode (formerly ANSYS-Mode) represents, in conjunction with
;; the GNU-Emacs editor, an advanced APDL environment.  Including
;; features features like immediate documentation access, keyword
;; completion, code templates, dedicated keybindings, solver
;; communication (GNU-Linux only), license reporting, etc.

;; ------------------------------
;; First usage:

;; Open an APDL-Mode buffer with `M-x apdl'.  From there you might
;; investigate the menu entries `APDL' and `ANSYS'.

;; For the APDL-Mode help with a brief tutorial and further guidance
;; please type `C-h m'."  

;; short doc string less than 36 chars!?

(define-package "apdl-mode" "20.2.0" "Major mode for the APDL programming language."
  '((emacs "25.1"))
  :keywords
  '("APDL" "ANSYS" "languages" "FEA" "convenience" "tools")
  :authors
  '(("H. Dieter Wilhelm" . "dieter@duenenhof-wilhelm.de"))
  :maintainer
  '("H. Dieter Wilhelm" . "dieter@duenenhof-wilhelm.de")
  :url "https://github.com/dieter-wilhelm/apdl-mode")

;; Local Variables:
;; no-byte-compile: t
;; End:
