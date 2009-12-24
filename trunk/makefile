ANSYS_MAJOR := 12
ANSYS_MINOR := 0

# this is the current ansys-mode version
MODE_VERSION := 1
VERSION := $(ANSYS_MAJOR).$(ANSYS_MINOR).$(MODE_VERSION)
PACKAGE := ansys-mode-$(VERSION).tgz

EL_FILES := ansys-mode.el ansys-keyword.el default.el \
  ansys-template.el ansys-process.el

FILES := LICENSE README TODO fontification.mac

.PHONEY : ALL
ALL : $(PACKAGE) TAGS

$(PACKAGE) : $(FILES) $(EL_FILES) makefile
	@echo "Packaging $@ ..."
	@echo
	@tar -czvf $@ $(FILES) $(EL_FILES)
	@echo
	@echo "... $@ done."
	@echo "------------------------------"

TAGS : makefile $(EL_FILES)
	etags $(EL_FILES)
