# Targets:
##############################
# ALL = MODE + EMACS
# MODE = mode package + tags
# EMACS = Windoze version of emacs with pre-configured mode

ANSYS_MAJOR := 16
ANSYS_MINOR := 1

HOSTNAME := $(shell hostname)
DIR := $(shell pwd)

# use make in the "tags" release directories
#RELEASE := $(shell $$($(DIR)))

ifeq ($(HOSTNAME),urmel)
 EMACS_DIR := /usr/local/src/
else
 EMACS_DIR := /appl/
endif


EMACS_VERSION := emacs-24.5
EMACS_PACKAGE := $(EMACS_VERSION)-bin-i386.zip
# in the ftp.gnu.org/gnu folder is the gnu-keyring.gpg
EMACS_PACKAGE_SIG := $(EMACS_VERSION)-bin-i386.zip.sig
EMACS_SOURCE_PACKAGE := $(EMACS_VERSION).tar.gz
EMACS_SOURCE_PACKAGE_SIG := $(EMACS_VERSION).tar.gz.sig
ADDRESS := http://ftp.gnu.org/pub/gnu/emacs/windows/$(EMACS_PACKAGE)
SIG_ADDRESS := http://ftp.gnu.org/pub/gnu/emacs/windows/$(EMACS_PACKAGE_SIG)
SOURCE_ADDRESS := http://ftp.gnu.org/pub/gnu/emacs/$(EMACS_SOURCE_PACKAGE)
SIG_SOURCE_ADDRESS := http://ftp.gnu.org/pub/gnu/emacs/$(EMACS_SOURCE_PACKAGE_SIG)
# ftp://ftp.informatik.rwth-aachen.de/pub/gnu/
EMACS_EXE := $(EMACS_DIR)$(EMACS_VERSION)/src/emacs

.PHONEY : EMACS_SOURCE
EMACS_SOURCE : $(EMACS_SOURCE_PACKAGE) $(EMACS_SOURCE_PACKAGE_SIG)

.PHONEY : EXE
EXE : $(EMACS_EXE)

# this is the current ansys-mode version
MODE_VERSION := 1-beta.1
VERSION := $(ANSYS_MAJOR).$(ANSYS_MINOR).$(MODE_VERSION)
PACKAGE := ansys-mode-$(VERSION).tgz

EL_FILES := ansys-mode.el ansys-keyword.el \
  ansys-template.el ansys-process.el

TESTFILES := ../ansys-mode.el ../ansys-template.el ../ansys-process.el ../README ../NEWS ../TODO ansys-fontification.el extract_tags.py ../doc/A-M_APDL_reference.org ../doc/A-M_in-depth_tutorial.org ../doc/A-M_introductory_tutorial.org

ELC_FILES := $(EL_FILES:.el=.elc)

FILES := LICENSE README TODO NEWS fontification.mac default_el	\
 A-M_introductory_tutorial-$(VERSION).pdf			\
 A-M_in-depth_tutorial-$(VERSION).ansys				\
 A-M_in-depth_tutorial-$(VERSION).pdf				\
 A-M_APDL_reference-$(VERSION).pdf

PACKAGE_FILES :=  $(FILES) $(EL_FILES)

$(PACKAGE) : $(PACKAGE_FILES) makefile
	@echo "Packaging $@ ..."
	@echo
	@tar -czvf $@ $(PACKAGE_FILES)
	@echo
	@echo "... $@ done."
	@echo "------------------------------"

# this is the Ansys mode package
.PHONEY : MODE
MODE : $(PACKAGE) TAGS

.PHONEY : ALL
ALL : MODE EMACS

.PHONEY : CLEAN
CLEAN :
	rm $(ELC_FILES)

$(EMACS_SOURCE_PACKAGE_SIG) :
	wget $(SIG_SOURCE_ADDRESS)
	gpg $(EMACS_SOURCE_PACKAGE_SIG)

# $(EMACS_SOURCE_PACKAGE) : $(EMACS_SOURCE_PACKAGE_SIG)
# 	cd $(EMACS_DIR)
# 	wget $(SOURCE_ADDRESS)
# 	tar -xjvf $(EMACS_SOURCE_PACKAGE)

$(EMACS_EXE) : $(EMACS_SOURCE)
	cp $(EMACS_SOURCE_PACKAGE) $(EMACS_DIR)
	cd $(EMACS_DIR); tar -xzvf $(EMACS_SOURCE_PACKAGE)
	cd $(EMACS_DIR)$(EMACS_VERSION); ./configure  --with-gif=no &&	make

A-M_APDL_reference-$(VERSION).pdf : A-M_APDL_reference.org
	$(EMACS_EXE) --batch --file $< \
          --execute "(add-to-list 'load-path \"~/ansys-mode/trunk\")" \
	  --load "ansys-mode.el"  --execute "(org-export-as-pdf 3)"
	  cp A-M_APDL_reference.pdf A-M_APDL_reference-$(VERSION).pdf

A-M_in-depth_tutorial-$(VERSION).ansys : A-M_in-depth_tutorial.org
	$(EMACS_EXE) --batch --file $< \
	  --execute "(add-to-list 'load-path \"~/ansys-mode/trunk\")" \
	  --load "~/ansys-mode/trunk/ansys-mode.el" --execute "(org-babel-tangle)"
	cp A-M_in-depth_tutorial.ansys A-M_in-depth_tutorial-$(VERSION).ansys

# A-M_in-depth_tutorial-$(VERSION).ansys : A-M_in-depth_tutorial.org
# 	$(EMACS_EXE) --batch --file $<  --execute "(add-to-list 'load-path \"/home/dieter/ansys-mode/trunk\")(load-file \"ansys-mode.el\")" --execute "(org-babel-tangle)"
# 	cp A-M_in-depth_tutorial.ansys A-M_in-depth_tutorial-$(VERSION).ansys

A-M_in-depth_tutorial-$(VERSION).pdf : A-M_in-depth_tutorial.org
	$(EMACS_EXE) --batch  --file $< \
	  --execute "(add-to-list 'load-path \"~/ansys-mode/trunk\")" \
	  --load "ansys-mode.el" --execute "(org-export-as-pdf 3)"
	  cp A-M_in-depth_tutorial.pdf A-M_in-depth_tutorial-$(VERSION).pdf

# $(EMACS_EXE) --batch  --file $< --load "/home/dieter/ansys-mode/trunk/ansys-mode.el" --execute "(org-export-as-pdf 3)"
# htmlize does not yet work in batch mode
#	$(EMACS_EXE) --batch --load "/home/dieter/ansys-mode/trunk/ansys-mode.el" \
# 	--load "/home/dieter/ansys-modehtmlize.el\")(load-file \"ansys-mode.el\")" --file $< --execute "(org-export-as-html 1)"

# A-M_in-depth_tutorial-$(VERSION).pdf : A-M_in-depth_tutorial.org
# 	$(EMACS_EXE) --batch  --file $< --execute "(add-to-list 'load-path \"/home/dieter/ansys-mode/trunk\")(load-file \"ansys-mode.el\")" --execute "(org-export-as-pdf 3)"
# # htmlize does not yet work in batch mode
# 	$(EMACS_EXE) --batch --execute "(add-to-list 'load-path \"/home/dieter/ansys-mode/trunk\")(load-file \"htmlize.el\")(load-file \"ansys-mode.el\")" --file $< --execute "(org-export-as-html 1)"

A-M_introductory_tutorial-$(VERSION).pdf : A-M_introductory_tutorial.org
	$(EMACS_EXE) --batch --file $< --execute  "(org-export-as-pdf 3)"
	cp  A-M_introductory_tutorial.pdf A-M_introductory_tutorial-$(VERSION).pdf
	$(EMACS_EXE) --batch  --file $< --execute "(org-export-as-html 1)"

ansys-keyword.el : ansys-fontification.el ansys_dynprompt.txt ansys_elements.txt ansys_parametric_functions.txt ansys_get_functions.txt ansys_keywords.txt
	$(EMACS_EXE) --batch --load $<

%.elc : %.el
	$(EMACS_EXE) --batch -f batch-byte-compile $<

# default.el : default_el
# 	@cp default_el emacs-23.1/site-lisp/default.el

# This is Emacs for Windows packaged with Ansys mode
.PHONEY : EMACS
EMACS : $(EMACS_PACKAGE) $(PACKAGE)  $(ELC_FILES) default_el
	test -d $(EMACS_VERSION)/site-lisp || mkdir -p $(EMACS_VERSION)/site-lisp
	cp -uv $(FILES) $(EL_FILES) $(EMACS_VERSION)/site-lisp
	cp -uv $(ELC_FILES) $(EMACS_VERSION)/site-lisp
	cp -uv default_el $(EMACS_VERSION)/site-lisp/default.el
	cp -uv $(EMACS_PACKAGE) ansys-mode-$(VERSION)+$(EMACS_PACKAGE)
	zip -u ansys-mode-$(VERSION)+$(EMACS_PACKAGE) $(EMACS_VERSION)/site-lisp/*

#	rm -r $(EMACS_VERSION)

# get Emacs stuff
# emacs_package is the windows zip file

$(EMACS_PACKAGE) :
	wget $(ADDRESS) $(SIG_ADDRESS)
	gpg $(EMACS_PACKAGE_SIG)

##################################################
# getting the emacs source
##################################################
$(EMACS_SOURCE_PACKAGE) : 
	wget $(SOURCE_ADDRESS)

# need the org files for the versioning string
TAGS : makefile ../$(EL_FILES) ../default_el ansys-fontification.el ../README ../TODO ../NEWS A-M_APDL_reference.org A-M_in-depth_tutorial.org A-M_introductory_tutorial.org
	etags ../$(EL_FILES) ../default_el ansys-fontification.el ../README ../TODO ../NEWS A-M_APDL_reference.org A-M_in-depth_tutorial.org A-M_introductory_tutorial.org

TEST : 
	etags $(TESTFILES)

.PHONEY : TAG_RELEASE
TAG_RELEASE :
	svn copy https://ansys-mode.googlecode.com/svn/trunk https://ansys-mode.googlecode.com/svn/tags/release-14.5.2-beta.2 -m "tagging 14.5.2-beta.2"

#	svn copy https://ansys-mode.googlecode.com/svn/trunk https://ansys-mode.googlecode.com/svn/tags/release-14.5.2-beta.1 -m "tagging 14.5.2-beta.1"
