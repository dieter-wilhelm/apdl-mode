VERSION := 100# this is the Ansys version

EL_FILES := ansys-mode$(VERSION).el ansys-fontification.el \
  ansys-process.el octave-mod.el


FILES := makefile LICENSE README \
   ansys-process.mac ansys-dynprompt$(VERSION).txt ansys_return_values$(VERSION).txt \
   ansys_elements$(VERSION).txt ansys_keywords$(VERSION).txt \
   ansys_parametric_functions$(VERSION).txt ansys_get_functions$(VERSION).txt \
   TAGS ansys-mode.mac bug-report.sh Readme.txt

ansys-mode$(VERSION).tar.gz : $(FILES) $(EL_FILES)
	@tar -czvf $@ $^
	@echo
	@echo $@...done
	@echo ------------------------------

TAGS : makefile $(EL_FILES)
	etags $(EL_FILES)

README : makefile Readme.txt
	date -I > README
	@sed /^#/d Readme.txt | sed -e 's/\(ansys-mode\)/\1$(VERSION)/' >>README
	@echo >> README
	@ls $(EL_FILES) $(FILES) | cat >> README

ansys-mode$(VERSION).elc: ansys-mode$(VERSION).el
#	/cygdrive/c/emacs/emacs-22.0.50/Emacs/bin/emacs.exe -batch \
	/appl/emacs/emacs/emacs-22.0.5src/emacs -batch \
	-f batch-byte-compile $<	