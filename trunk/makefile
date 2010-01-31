ANSYS_MAJOR := 12
ANSYS_MINOR := 0

HOSTNAME := $(shell hostname)

ifeq ($(HOSTNAME),urmel)
 E_DIR := /usr/local/src
else
 E_DIR := /appl/emacs
endif

EMACS := $(E_DIR)/emacs-23.1/src/emacs

# this is the current ansys-mode version
MODE_VERSION := 1
VERSION := $(ANSYS_MAJOR).$(ANSYS_MINOR).$(MODE_VERSION)
PACKAGE := ansys-mode-$(VERSION).tgz

EL_FILES := ansys-mode.el ansys-keyword.el \
  ansys-template.el ansys-process.el

ELC_FILES := $(EL_FILES:.el=.elc)

FILES := LICENSE README TODO fontification.mac default_el

.PHONEY : MODE
MODE : $(PACKAGE) TAGS

.PHONEY : ALL
ALL : $(PACKAGE) TAGS EMACS

.PHONEY : CLEAN
CLEAN :
	rm $(ELC_FILES)

PACKAGE_FILES :=  $(FILES) $(EL_FILES)

$(PACKAGE) : $(PACKAGE_FILES) makefile
	@echo "Packaging $@ ..."
	@echo
	@tar -czvf $@ $(PACKAGE_FILES)
	@echo
	@echo "... $@ done."
	@echo "------------------------------"

ansys-keyword.el : ansys-fontification.el
	$(EMACS) --batch --load $<

%.elc : %.el
	$(EMACS) --batch -f batch-byte-compile $<

default.el : default_el
	@cp default_el emacs-23.1/site-lisp/default.el

.PHONEY : EMACS
EMACS : $(PACKAGE)  $(ELC_FILES) default.el
	@echo "Packaging Ansys mode with Emacs 23.1 ..."
	@cp $(FILES) $(EL_FILES) emacs-23.1/site-lisp
	mv $(ELC_FILES) emacs-23.1/site-lisp
	@tar -czf "ansys-mode+emacs-23.1-win32.tgz" emacs-23.1
	@echo "... $@ done."

TAGS : makefile $(EL_FILES) default_el ansys-fontification.el
	etags $(EL_FILES)
