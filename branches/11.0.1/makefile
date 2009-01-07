ANSYS_VERSION := 11
ANSYS_MINOR := 0
MODE_VERSION := 1# this is the current mode version
VERSION := $(ANSYS_VERSION).$(ANSYS_MINOR)
PACKAGE := ansys-mode_$(VERSION).$(MODE_VERSION).tar.gz

EL_FILES := ansys-mode.el ansys-fontification.el \
  ansys-process.el default.el

FILES := makefile LICENSE README TODO \
  ansys-process.mac ansys-dynprompt.txt \
  ansys_return_values.txt ansys_elements.txt \
  ansys_keywords.txt ansys_parametric_functions.txt \
  ansys_get_functions.txt TAGS ansys-mode.mac \
  bug-report.sh padt.mac

.PHONEY : ALL
ALL : $(PACKAGE) ansys-mode.el.gz
	@cp -v $^ ~/tmp

$(PACKAGE) : $(FILES) $(EL_FILES)
	@tar -czvf $@ $^
	@echo
	@echo $@...done
	@echo ------------------------------

ansys-mode.el.gz : ansys-mode.el
	@gzip -c $^ > $@
	@echo $@...done
	@echo ------------------------------

TAGS : makefile $(EL_FILES)
	etags $(EL_FILES)

README : makefile ansys-mode.el
	@echo This is the $@ for $(PACKAGE) > $@
	@date -I >> $@
	@echo You only need ansys-mode.el for running Ansys mode in Emacs.  This >> $@
	@echo lisp package provides Emacs support for the Ansys command language. >> $@
	@echo "It defines Ansys mode, a major mode for editing APDL (Ansys Parametric" >> $@
	@echo "Design Language) files and managing Ansys processes." >> $@
	@sed -e "/== Documentation/,/;;; Code/!d" ansys-mode.el \
		| sed -e "1d" | sed -e "/;;; Code/d" \
		| sed -e "s/;;;*//" >> $@
	@echo file list: >> $@
	@echo >> $@
	@ls $(EL_FILES) $(FILES) | cat >> $@

#ansys-mode$(VERSION).elc: ansys-mode$(VERSION).el
#	/cygdrive/c/emacs/emacs-22.0.50/Emacs/bin/emacs.exe -batch \
	/appl/emacs/emacs/emacs-22.0.5src/emacs -batch \
	-f batch-byte-compile $<	