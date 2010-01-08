ANSYS_MAJOR := 12
ANSYS_MINOR := 0

# this is the current ansys-mode version
MODE_VERSION := 1
VERSION := $(ANSYS_MAJOR).$(ANSYS_MINOR).$(MODE_VERSION)
PACKAGE := ansys-mode-$(VERSION).tgz

EL_FILES := ansys-mode.el ansys-keyword.el default_.el \
  ansys-template.el ansys-process.el

FILES := LICENSE README TODO fontification.mac

.PHONEY : ALL
ALL : $(PACKAGE) TAGS EMACS

$(PACKAGE) : $(FILES) $(EL_FILES) makefile
	@echo "Packaging $@ ..."
	@echo
	@tar -czvf $@ $(FILES) $(EL_FILES)
	@echo
	@echo "... $@ done."
	@echo "------------------------------"

EMACS : $(FILES)  $(EL_FILES) makefile
	@echo "Packaging Ansys mode with Emacs 23.1 ..."
	@cp $(FILES) $(EL_FILES) emacs-23.1/site-lisp
	@tar -czf "ansys-mode+emacs-23.1-win32.tgz" emacs-23.1
	@echo "... $@ done."

TAGS : makefile $(EL_FILES)
	etags $(EL_FILES)
