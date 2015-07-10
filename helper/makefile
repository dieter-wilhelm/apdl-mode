help : 
	@echo "This is the makefile of ANSYS-Mode $(VERSION) using $(EMACS_VERSION)"
	@echo "make MODE = A-M package files + tutorials"
	@echo "make EMACS_SRC = Emacs sources + A-M package files + tutorials"
	@echo "make EMACS = Windoze version of emacs with pre-configured ANSYS-Mode"
	@echo "make ALWIN = Archive for Alwin"
	@echo "make HTML = update the html documentation"
	@echo "make TAGS = make the goddam tags!"
	@echo "make ELC = compile the el files"
	@echo "make ALL = MODE + EMACS + EMACS_SRC + TAGS"
	@echo "make CLEAN = clean the elc files"
	@echo "make TAG_RELEASE = "
	@echo ""

# EMACS_VER needed for Emacs' windows installaion tree
EMACS_VER := 24.5
EMACS_VERSION := emacs-$(EMACS_VER)

# complete package names are comprised of
# ansys-mode-$(ANSYS_MAJOR).$(ANSYS_MINOR).$(MODE_VERSION)
# tutorials: A-M_xxx
ANSYS_MAJOR := 16
ANSYS_MINOR := 1

# this is the current ANSYS-Mode version suffix
MODE_VERSION := test2
# for consistency:
#MODE_VERSION := test1
#MODE_VERSION := beta1
#MODE_VERSION := freeze1
#MODE_VERSION := rc1
#MODE_VERSION := 1

##################################################

HOSTNAME := $(shell hostname)
DIR := $(shell pwd)

# use make in the "tags" release directories
#RELEASE := $(shell $$($(DIR)))

ifeq ($(HOSTNAME),urmel)
 EMACS_DIR := /usr/local/src/
else
 EMACS_DIR := /appl/emacs/
endif

VERSION := $(ANSYS_MAJOR).$(ANSYS_MINOR).$(MODE_VERSION)

EMACS_PACKAGE := $(EMACS_VERSION)-bin-i686-mingw32.zip
# in the ftp.gnu.org/gnu folder is the gnu-keyring.gpg
# but it is not up-to-date, use keyserver!
KEYRING := http://ftp.gnu.org/gnu/gnu-keyring.gpg
EMACS_PACKAGE_SIG := $(EMACS_PACKAGE).sig
EMACS_SOURCE_PACKAGE := $(EMACS_VERSION).tar.xz
A-M_EMACS_SOURCE_PACKAGE := ansys-mode-$(VERSION).$(EMACS_VERSION).tar.xz
EMACS_SOURCE_PACKAGE_SIG := $(EMACS_SOURCE_PACKAGE).sig
ADDRESS := http://ftp.gnu.org/gnu/emacs/windows/$(EMACS_PACKAGE)
SIG_ADDRESS := http://ftp.gnu.org/gnu/emacs/windows/$(EMACS_PACKAGE_SIG)
SOURCE_ADDRESS := http://ftp.gnu.org/gnu/emacs/$(EMACS_SOURCE_PACKAGE)
SIG_SOURCE_ADDRESS := http://ftp.gnu.org/gnu/emacs/$(EMACS_SOURCE_PACKAGE_SIG)
# ftp://ftp.informatik.rwth-aachen.de/pub/gnu/
EMACS_EXE := $(EMACS_DIR)$(EMACS_VERSION)/src/emacs

.PHONEY : EMACS_SOURCE
EMACS_SOURCE : $(EMACS_SOURCE_PACKAGE) $(EMACS_SOURCE_PACKAGE_SIG)

.PHONEY : EXE
EXE : $(EMACS_EXE)

PACKAGE := ansys-mode-$(VERSION).tar.xz

EL_FILES := ../A-M.el ../ansys-mode.el ../keyword.el \
	      ../template.el ../process.el
# fontification.el is not included since it is not necessary for running A-M

ELC_FILES := $(EL_FILES:.el=.elc)

# compiled files
.PHONEY : ELC
ELC : $(ELC_FILES)

FILES := ../LICENSE.org ../README.org ../TODO.org ../NEWS.org
HELPER_FILES := example.mac example.anf example.dat

#	 ../doc/APDL_tutorial.ans

# ../doc/APDL_tutorial.ans : ../doc/A-M_in-depth_tutorial.org
# 	$(EMACS_EXE) --batch --eval "(add-to-list 'load-path \"..\doc")"  \
# 	   $< -eval "(require 'ob-tangle)" -eval "(org-babel-tangle nil "APDL_tutorial.ans")"

WORK_FILES := ../LICENSE.org ../README.org ../TODO.org ../NEWS.org \
		 example.mac example.anf example.dat tools/ansys-mode/README

WORK_EL_FILES := ../A-M.el ../ansys-mode.el ../keyword.el \
  ../template.el ../process.el


TUTORIAL_SOURCES := ../doc/A-M_introductory_tutorial.org \
	            ../doc/A-M_in-depth_tutorial.org     \
	            ../doc/A-M_APDL_reference.org

TUTORIALS := ../doc/A-M_introductory_tutorial.pdf \
 ../doc/A-M_in-depth_tutorial.ansys 	          \
 ../doc/A-M_in-depth_tutorial.pdf                 \
 ../doc/A-M_APDL_reference.pdf

MATLIB := ../matlib/

PACKAGE_FILES :=  $(FILES) $(HELPER_FILES) $(EL_FILES) $(MATLIB) $(TUTORIALS) default.el

default.el : makefile
	@echo ";;; Automatically created, ANSYS-mode" > default.el
	@echo "(load-file \"ansys-mode-$(VERSION)\\\\A-M\")" >> default.el

$(PACKAGE) : $(PACKAGE_FILES) makefile
	@echo "Packaging $@ ..."
	@echo
	@test -d ansys-mode-$(VERSION) || \
	    mkdir ansys-mode-$(VERSION)
	@cp -a $(FILES) ansys-mode-$(VERSION)
	@cp -a $(EL_FILES) ansys-mode-$(VERSION)
	@test -d ansys-mode-$(VERSION)/doc || \
	    mkdir ansys-mode-$(VERSION)/doc
	@cp -a $(TUTORIALS) ansys-mode-$(VERSION)/doc
	@test -d ansys-mode-$(VERSION)/helper || \
	    mkdir ansys-mode-$(VERSION)/helper
	@cp -a $(HELPER_FILES) ansys-mode-$(VERSION)/helper
	@test -d ansys-mode-$(VERSION)/matlib || \
	    mkdir ansys-mode-$(VERSION)/matlib
	@cp -rf $(MATLIB) ansys-mode-$(VERSION)
	@tar -cJvf $@ ansys-mode-$(VERSION) default.el
	@echo
	@echo "... $@ done."
	@echo "------------------------------"

# MODE: this is the Ansys mode "installation" package
# the sources are built from GitHub automatically when tagged
.PHONEY : MODE
MODE : $(PACKAGE)

.PHONEY : ALWIN
ALWIN : A-M-$(VERSION).tar.xz

A-M-$(VERSION).tar.xz : $(PACKAGE) tools/ansys-mode/README tools/bin/ansys-mode.sh makefile
	rm -f tools/ansys-mode/ansys-mode-$(VERSION)/matlib/*
	cp -a $(WORK_EL_FILES) $(FILES) $(TUTORIALS) $(MATLIB) \
	      tools/ansys-mode/ansys-mode-$(VERSION)
	tar -cJvf $@ --group HPCUser_1 tools

$(A-M_EMACS_SOURCE_PACKAGE) : $(PACKAGE_FILES) $(EMACS_SOURCE_PACKAGE) makefile  $(A-M_EMACS_SOURCE_PACKAGE)
	cp $(EMACS_SOURCE_PACKAGE) $(A-M_EMACS_SOURCE_PACKAGE)
	cp -uvr $(PACKAGE_FILES) $(EMACS_VERSION)/site-lisp/ansys-mode-$(VERSION)
	tar -uJvf $(A-M_EMACS_SOURCE_PACKAGE) $(EMACS_VERSION)

.PHONEY : EMACS_SRC
EMACS_SRC : $(A-M_EMACS_SOURCE_PACKAGE)

HTMLs := ../index.html ../README.html ../NEWS.html ../TODO.html ../LICENSE.html ../A-M.html ../doc/A-M_APDL_reference.html ../doc/A-M_in-depth_tutorial.html ../doc/A-M_introductory_tutorial.html ../matlib/README.html

ORG_FILES := $(HTMLs:.html=.org)

%.html : %.org
	$(EMACS_EXE) --batch -eval "(add-to-list 'load-path \"..\")" -l ansys-mode.el -l htmlize.el $< -eval "(require 'ox-html)" -eval "(setq org-export-htmlize-output-type 'css)" -eval "(org-html-export-to-html)"

.PHONEY : HTML
HTML : html.tar.xz

# only archive members which are newer than the existingp archive
html.tar.xz : $(HTMLs) makefile
	tar cJvf $@ $(HTMLs)
	rm $(HTMLs)

.PHONEY : ALL
ALL : MODE EMACS EMACS_SRC HTML TAGS

.PHONEY : CLEAN
CLEAN :
	rm $(ELC_FILES)

$(EMACS_SOURCE_PACKAGE_SIG) :
	wget $(SIG_SOURCE_ADDRESS)
	gpg $(EMACS_SOURCE_PACKAGE_SIG)

$(EMACS_EXE) : $(EMACS_SOURCE)
	cp $(EMACS_SOURCE_PACKAGE) $(EMACS_DIR)
	cd $(EMACS_DIR); tar -xJvf $(EMACS_SOURCE_PACKAGE)
	cd $(EMACS_DIR)$(EMACS_VERSION); ./configure && make
#	cd $(EMACS_DIR)$(EMACS_VERSION); ./configure  --with-gif=no &&	make

../doc/A-M_APDL_reference.pdf : ../doc/A-M_APDL_reference.org
	$(EMACS_EXE) --batch --file $< \
          --execute "(add-to-list 'load-path \"~/a-m\")" \
	  --load "ansys-mode.el"  --execute "(org-latex-export-to-pdf)"

../doc/A-M_in-depth_tutorial.ansys : ../doc/A-M_in-depth_tutorial.org
	$(EMACS_EXE) --batch --file $< \
	  --execute "(add-to-list 'load-path \"~/a-m\")" \
	  --load "ansys-mode.el" --execute "(org-babel-tangle)"

../doc/A-M_in-depth_tutorial.pdf : ../doc/A-M_in-depth_tutorial.org
	$(EMACS_EXE) --batch  --file $< \
	  --execute "(add-to-list 'load-path \"~/a-m\")" \
	  --load "ansys-mode.el" --execute "(org-latex-export-to-pdf)"

../doc/A-M_introductory_tutorial.pdf : ../doc/A-M_introductory_tutorial.org
	$(EMACS_EXE) --batch --file $< \
	--load "~/a-m/helper/export.el" \
	--execute  "(org-beamer-export-to-pdf)"

keyword.el : fontification.el ansys_dynprompt.txt ansys_elements.txt ansys_parametric_functions.txt ansys_get_functions.txt ansys_keywords.txt
	$(EMACS_EXE) --batch --load $<

%.elc : %.el
	$(EMACS_EXE) --batch -f batch-byte-compile $<

# This is Emacs for Windows packaged with Ansys mode
.PHONEY : EMACS
EMACS : $(EMACS_PACKAGE) $(PACKAGE)
	 test -d share/emacs/$(EMACS_VER)/site-lisp/ansys-mode-$(VERSION) || \
	    mkdir -p share/emacs/$(EMACS_VER)/site-lisp/ansys-mode-$(VERSION)
	 cp -uv $(FILES) share/emacs/$(EMACS_VER)/site-lisp/ansys-mode-$(VERSION)
	 cp -uv $(EL_FILES) share/emacs/$(EMACS_VER)/site-lisp/ansys-mode-$(VERSION)
	 cp -uv default.el share/emacs/$(EMACS_VER)/site-lisp
	 cp -uv $(EMACS_PACKAGE) ansys-mode-$(VERSION).$(EMACS_PACKAGE)
	 zip -u ansys-mode-$(VERSION).$(EMACS_PACKAGE) \
	   share/emacs/$(EMACS_VER)/site-lisp/ansys-mode-$(VERSION)/*

##################################################
# getting the emacs source
##################################################

## !!! gnu-keyring is not current: use key-servers!!!!
# gnu-keyring.gpg : 
# 	wget $(KEYRING)
$(EMACS_SOURCE_PACKAGE) : 
	wget $(SOURCE_ADDRESS)

$(EMACS_PACKAGE) :  $(EMACS_PACKAGE_SIG)
	wget $(SIG_ADDRESS)
	wget $(ADDRESS)
	gpg $(EMACS_PACKAGE_SIG)

# need the org files for the versioning string
TAGS_FILES := makefile $(EL_FILES) $(FILES) $(TUTORIAL_SOURCES) fontification.el \
		default.el
TAGS : $(TAGS_FILES)
	etags $(TAGS_FILES)

.PHONEY : TAG_RELEASE
TAG_RELEASE :
	git tag release-$(VERSION)
# echo "git tag release-$(VERSION)"
