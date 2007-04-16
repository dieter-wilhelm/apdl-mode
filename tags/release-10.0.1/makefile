VERSION := 100# this is the Ansys version

EL_FILES := ansys-mode$(VERSION).el ansys-fontification.el \
  ansys-process.el

FILES := makefile LICENSE README \
   ansys-process.mac ansys-dynprompt$(VERSION).txt ansys_return_values$(VERSION).txt \
   ansys_elements$(VERSION).txt ansys_keywords$(VERSION).txt \
   ansys_parametric_functions$(VERSION).txt ansys_get_functions$(VERSION).txt \
   TAGS ansys-mode.mac bug-report.sh Readme.txt padt.mac

ansys-mode$(VERSION).tar.gz : $(FILES) $(EL_FILES)
	@tar -czvf $@ $^
	@echo
	@echo $@...done
	@echo ------------------------------

TAGS : makefile $(EL_FILES)
	etags $(EL_FILES)

README : makefile ansys-mode$(VERSION).el
	@echo This is the $@ for ansys-mode$(VERSION).tar.gz > $@
	@date -I >> $@
	@echo You only need ansys-mode.el from the archive for Ansys mode in Emacs.  This >> $@
	@echo lisp package provides Emacs support for the Ansys command language. >> $@
	@echo "It defines Ansys mode, a major mode for editing APDL (Ansys Parametric" >> $@
	@echo "Design Language) files and managing Ansys processes." >> $@
	@sed -e "/== Documentation/,/;;; Code/!d" ansys-mode$(VERSION).el \
		| sed -e "1d" | sed -e "/;;; Code/d" \
		| sed -e "s/;;;*//" >> $@
	@echo file list: >> $@
	@echo >> $@
	@ls $(EL_FILES) $(FILES) | cat >> $@

#ansys-mode$(VERSION).elc: ansys-mode$(VERSION).el
#	/cygdrive/c/emacs/emacs-22.0.50/Emacs/bin/emacs.exe -batch \
	/appl/emacs/emacs/emacs-22.0.5src/emacs -batch \
	-f batch-byte-compile $<	