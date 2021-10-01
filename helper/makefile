help : 
	@echo "This is the makefile of APDL-Mode $(VERSION) using $(EMACS_VERSION)"
	@echo "make PACK = Emacs tar package"
	@echo "make GH = for gh-pages updates"
	@echo "make [help]  = this help"
	@echo "make CLEAN = clean the elc and html files"
	@echo "older stuff"
	@echo "make MODE = A-M package files + tutorials"
	@echo "make EMACS_SRC = Emacs sources + A-M package files + tutorials"
	@echo "make EMACS = Windoze version of emacs with pre-configured APDL-Mode"
	@echo "make CONTI = Archive for my company"
	@echo "make HTML = update the html documentation"
	@echo "make TAGS = make the etags"
	@echo "make ELC = compile the el files"
	@echo "make ALL = MODE + EMACS + EMACS_SRC + TAGS"
#	@echo "make TAG_RELEASE = "
	@echo "make -k = --keep-going, even if some targets can't be made.."

# Semantic versioning: MA.Mi.Pa, Ma = year
APDL_MAJOR := 20
APDL_MINOR := 7
APDL_PATCH := 0


# EMACS_VER needed for Emacs' windows installaion tree
EMACS_VER := 27.1
EMACS_VERSION := emacs-$(EMACS_VER)

# complete package names are comprised of

# apdl-mode-$(APDL_MAJOR).$(ANSYS_MINOR).$(MODE_VERSION)
# tutorials: A-M_xxx

##################################################

HOSTNAME := $(shell hostname)
DIR := $(shell pwd)
# the sources dir
ifeq ($(HOSTNAME),ping)
S_DIR := "/home/dieter/a-m/"
else
S_DIR := "/mnt/hgfs/D/apdl-mode/"
endif

# use make in the "tags" release directories
#RELEASE := $(shell $$($(DIR)))

ifeq ($(HOSTNAME),ping)
 EMACS_DIR := /usr/local/src/emacs/
else
 EMACS_DIR := /usr/local/src/
endif

VERSION := $(APDL_MAJOR).$(APDL_MINOR).$(APDL_PATCH)

EMACS_PACKAGE := $(EMACS_VERSION)-bin-i686-mingw32.zip
# in the ftp.gnu.org/gnu folder is the gnu-keyring.gpg
# but it is not up-to-date, use keyserver!
KEYRING := http://ftp.gnu.org/gnu/gnu-keyring.gpg
EMACS_PACKAGE_SIG := $(EMACS_PACKAGE).sig
EMACS_SOURCE_PACKAGE := $(EMACS_VERSION).tar.xz
A-M_EMACS_SOURCE_PACKAGE := apdl-mode-$(VERSION).$(EMACS_VERSION).tar.xz
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

X11_FILES := ../X11/xGetClassicsWindow.cpp ../X11/xPasteToWin.cpp ../X11/xGetFocusWindow.cpp ../X11/xSendReturn.cpp

X11_EXE := $(X11_FILES:.cpp=)

.PHONEY : TEST
TEST : 
	@echo $(VERSION)

.PHONEY : ../X11
X11 : $(X11_FILES)
	$(MAKE) -C X11

PACKAGE := apdl-mode-$(VERSION).tar

.PHONEY : EXE
EXE : $(EMACS_EXE)

X11_FILES := ../X11/xGetClassicsWindow.cpp ../X11/xPasteToWin.cpp ../X11/xGetFocusWindow.cpp ../X11/xSendReturn.cpp

X11_EXE := $(X11_FILES:.cpp=)

# necessary el files
EL_FILES := ../apdl-mode.el ../apdl-keyword.el ../apdl-mode-pkg.el \
	../apdl-initialise.el ../apdl-template.el ../apdl-wb-template.el \
	../apdl-process.el ../info/apdl-config.el

ELC_FILES := $(EL_FILES:.el=.elc)

# compile Elisp files
.PHONEY : ELC
ELC : $(ELC_FILES)

# interesting files besides the essential lisp files
FILES := ../info/LICENSE ../apdl-mode-readme.txt ../info/README.org \
	 ../info/TODO.org ../info/NEWS.org ../info/INSTALLATION.org \
	 ../info/apdl-config.org ../info/apdl-mode.org \
	 ../info/acknowledgements.org ../info/bugs.org ../info/first_steps.org \
	 ../info/introduction.org ../info/resources.org ../info/tutorial.org \
	 ../info/usage.org

# all org files which are referenced in README.org have to be exported
# to markdown .md
MD_FILES := ../info/README.md ../info/TODO.md ../info/resources.md \
		../info/NEWS.md ../info/apdl-config.md

# example files
EXAMPLE_FILES := ../doc/example.mac ../doc/example.anf ../doc/example.dat

INFO_FILES := ../info/introduction.org ../info/first_steps.org ../info/tutorial.org \
	../info/usage.org ../info/apdl-config.org ../doc/A-M_APDL_reference.org \
	../info/INSTALLATION.org ../info/bugs.org ../info/resources.org \
	../info/acknowledgements.org  ../info/GNU-FDL.txt ../info/TODO.org \
	../info/NEWS.org

# ../doc/APDL_tutorial.ans : ../doc/A-M_in-depth_tutorial.org
# 	$(EMACS_EXE) --batch --eval "(add-to-list 'load-path \"..\doc")"  \
# 	   $< -eval "(require 'ob-tangle)" -eval "(org-babel-tangle nil "APDL_tutorial.ans")"

# WORK_FILES := ../LICENSE ../README.org ../TODO.org ../NEWS.org \
# 		 example.mac example.anf example.dat # tools/ansys-mode/README.Conti
# WORK_EL_FILES := $(EL_FILES)

TUTORIAL_SOURCES := ../doc/A-M_introductory_tutorial.org \
	            ../doc/A-M_in-depth_tutorial.org     \
	            ../doc/A-M_APDL_reference.org

TUTORIALS := ../doc/A-M_introductory_tutorial.pdf \
 ../doc/A-M_in-depth_tutorial.ans 	          \
 ../doc/A-M_in-depth_tutorial.pdf                 \
 ../doc/A-M_APDL_reference.pdf

MATLIB := ../matlib/
TEMPLATE := ../template/

X11 := \
../X11/makefile \
../X11/xGetClassicsWindow \
../X11/xGetClassicsWindow.cpp \
../X11/xGetFocusWindow \
../X11/xGetFocusWindow.cpp \
../X11/xPasteToWin \
../X11/xPasteToWin.cpp \
../X11/xSendReturn \
../X11/xSendReturn.cpp \


# PACKAGE_FILES :=  $(X11) $(FILES) $(EXAMPLE_FILES) $(EL_FILES) $(MATLIB) $(TUTORIALS) default.el
PACKAGE_FILES := $(MD_FILES) $(FILES) $(EXAMPLE_FILES) $(EL_FILES) \
		$(MATLIB) $(TUTORIAL_SOURCES) ../info/apdl-mode.info $(TUTORIALS)
# default.el : makefile
# 	@echo ";;; Automatically created from APDL-Mode-$(VERSION)" > $@
# 	@echo "(let* ((dir (file-name-directory load-file-name))" >> $@
# 	@echo "       (file (concat dir \"/apdl-mode-$(VERSION)/apdl-config.el\")))" >> $@
# 	@echo "  (when (file-readable-p file) (load-file file)))" >> $@

../info/README.md : ../info/README.org $(FILES)
	$(EMACS_EXE) --batch --file $< \
	--load "$(DIR)/export.el" \
	--execute  "(org-md-export-to-markdown)"
	cp $@ ..

../info/TODO.md : ../info/TODO.org
	$(EMACS_EXE) --batch --file $< \
	--load "$(DIR)/export.el" \
	--execute  "(org-md-export-to-markdown)"

../info/resources.md : ../info/resources.org
	$(EMACS_EXE) --batch --file $< \
	--load "$(DIR)/export.el" \
	--execute  "(org-md-export-to-markdown)"

../info/NEWS.md : ../info/NEWS.org
	$(EMACS_EXE) --batch --file $< \
	--load "$(DIR)/export.el" \
	--execute  "(org-md-export-to-markdown)"

../info/apdl-config.md : ../info/apdl-config.org
	$(EMACS_EXE) --batch --file $< \
	--load "$(DIR)/export.el" \
	--execute  "(org-md-export-to-markdown)"

../info/apdl-mode.info : ../info/apdl-mode.org $(INFO_FILES)
#       update time in info/apdl-mode.org iso 8601 -I
	sed -i "s/Time-stamp: \".*\"/Time-stamp: \"`date -I`\"/" $<
	$(EMACS_EXE) --batch --file $< \
	--execute  "(org-texinfo-export-to-info)"
	cp $@ ..

P_DIR := apdl-mode-$(VERSION)
I_DIR := $(P_DIR)/info/

$(PACKAGE) : $(PACKAGE_FILES) makefile
	$(EMACS_EXE) --batch -L $(S_DIR) --eval \
		'(byte-recompile-directory $(S_DIR) 0)'
	@echo "Packaging GNU-Emacs package $@ ..."
	@echo
	@test -d $(P_DIR) || \
	    mkdir $(P_DIR)
	@cp -a $(FILES) $(P_DIR)
	@cp -a $(EL_FILES) $(P_DIR)
	@test -d $(P_DIR)/doc || \
	    mkdir $(P_DIR)/doc
#	@cp -a $(TUTORIALS) P_DIR/doc
	@cp -a $(EXAMPLE_FILES) $(P_DIR)/doc
	@test -d $(P_DIR)/helper || \
	    mkdir $(P_DIR)/helper
	@test -d $(P_DIR)/matlib || \
	    mkdir $(P_DIR)/matlib
# @test -d P_DIR/X11 || \
#     mkdir P_DIR/X11
	@test -d $(I_DIR) || mkdir $(I_DIR)
	@cp $(MD_FILES) $(I_DIR)
	@cp ../info/dir $(P_DIR)
	@cp ../info/apdl-mode.info $(P_DIR)
#	@cp ../info/apdl-mode.texi $(P_DIR)
#	@cp ../info/APDL-Mode.info $(P_DIR)
#	@cp ../info/APDL-Mode.texi $(P_DIR)
	@cp -rf $(MATLIB) $(P_DIR)
	@test -d $(P_DIR)/template || \
	    mkdir $(P_DIR)/template
	@cp -rf $(TEMPLATE) $(P_DIR)
	@tar -cf $@ $(P_DIR) # ../apdl-mode-readme.txt # default.el
	@rm -r $(P_DIR)
	@echo Time of packaging: `date -I`
#       update time in -initialise.el apdl-mode-update in iso 8601 -I
	@sed -i "s/(defconst apdl-mode-update \".*\"/(defconst apdl-mode-update \"`date -I`\"/" ../apdl-initialise.el
	@echo
	@echo "... $@ done."
	@echo "------------------------------"
#	@echo markdown files: $(MD_FILES)
#	@echo package files: $(PACKAGE_FILES)

# MODE: this is the Ansys mode "installation" package
# the sources are built from GitHub automatically when tagged
.PHONEY : PACK
PACK : CLEAN $(PACKAGE) makefile

.PHONEY : CONTI
CONTI : A-M-$(VERSION).tar.xz

A-M-$(VERSION).tar.xz : $(PACKAGE) \ # tools/apdl-mode/README.Conti
	tools/bin/A-M-$(VERSION).sh \
			makefile $(X11)
#	mv tools/apdl-mode/README.Conti tools/
	rm -fr tools/apdl-mode/*
#	mv tools/README.Conti tools/apdl-mode/
	@test -d tools/apdl-mode/apdl-mode-$(VERSION) || \
	    mkdir tools/apdl-mode/apdl-mode-$(VERSION)
	@echo "syncing.."
	cd ..;rsync -aR --exclude '*~' $(WORK_EL_FILES:../%=%) $(FILES:../%=%) \
	    $(TUTORIALS:../%=%) X11/ \
	    $(MATLIB:../%=%) $(EXAMPLE_FILES:../%=%) $(X11_EXE:../%=%)\
		helper/tools/apdl-mode/apdl-mode-$(VERSION)
	@pwd
	cd ./tools/apdl-mode; ln -sf apdl-mode-$(VERSION) current
	tar -cJvf $@ --group HPCUser_1 tools

tools/bin/A-M-$(VERSION).sh : makefile
	@test -d tools/bin || mkdir tools/bin
	@echo "#!/bin/sh " > $@
	@echo "# Automatically created from APDL-Mode-$(VERSION)" >> $@
	@echo "emacs -l /appl/tools/apdl-mode/apdl-mode-$(VERSION)/doc/apdl-config.el \$$*" >> $@
	chmod +x $@

.PHONEY : EMACS_SRC
EMACS_SRC : $(A-M_EMACS_SOURCE_PACKAGE)

$(A-M_EMACS_SOURCE_PACKAGE) : $(PACKAGE_FILES) $(EMACS_SOURCE_PACKAGE) makefile
	test -d $(EMACS_VERSION)/site-lisp/apdl-mode-$(VERSION) || \
	    mkdir -p $(EMACS_VERSION)/site-lisp/apdl-mode-$(VERSION)
	cp $(EMACS_SOURCE_PACKAGE) $(A-M_EMACS_SOURCE_PACKAGE)
	cp -uvr $(PACKAGE_FILES) $(EMACS_VERSION)/site-lisp/apdl-mode-$(VERSION)
	tar --append -Jvf $(A-M_EMACS_SOURCE_PACKAGE) $(EMACS_VERSION)

HTMLs := ../info/index.html ../info/README.html ../info/NEWS.html ../info/TODO.html \
	../doc/A-M_APDL_reference.html \
	../doc/A-M_in-depth_tutorial.html ../doc/A-M_introductory_tutorial.html \
	../matlib/README.html ../info/INSTALLATION.html

ORG_FILES := $(HTMLs:.html=.org)

%.html : %.org
	$(EMACS_EXE) --batch -eval "(add-to-list 'load-path \"..\")" \
  -load "/home/dieter/.emacs.d/elpa/htmlize-20210825.2150/htmlize-autoloads.el" \
	-l apdl-mode.el $< -eval "(require 'htmlize)" -eval "(require 'ox-html)" \
	-eval "(setq org-export-htmlize-output-type 'css)" \
	-eval "(org-html-export-to-html)"

.PHONEY : HTML
HTML : html.tar.xz

# for gh-pages documentation
.PHONEY : GH
GH : $(HTMLs) $(TUTORIALS)
	cp ../info/index.html ..
	cp ../info/README.html ..

# only archive members which are newer than the existingp archive
html.tar.xz : $(HTMLs) makefile
	tar cJvf $@ $(HTMLs)
	rm $(HTMLs)

.PHONEY : ALL
ALL : MODE EMACS EMACS_SRC HTML TAGS

.PHONEY : CLEAN
CLEAN :
	@rm -f $(ELC_FILES)
	@rm -f $(HTMLs)
	@rm -f ../index.html
	@echo cleansed ELCs and HTMLs.

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
	  --load "apdl-mode.el"  --execute "(org-latex-export-to-pdf)"

../doc/A-M_in-depth_tutorial.ans : ../doc/A-M_in-depth_tutorial.org
	$(EMACS_EXE) --batch --file $< \
	  --execute "(add-to-list 'load-path \"~/a-m\")" \
	  --load "apdl-mode.el" --execute "(org-babel-tangle)"
	mv ../doc/A-M_in-depth_tutorial.ansys ../doc/A-M_in-depth_tutorial.ans

../doc/A-M_in-depth_tutorial.pdf : ../doc/A-M_in-depth_tutorial.org
	$(EMACS_EXE) --batch  --file $< \
	  --execute "(add-to-list 'load-path \"~/a-m\")" \
	  --load "apdl-mode.el" --execute "(org-latex-export-to-pdf)"

../doc/A-M_introductory_tutorial.pdf : ../doc/A-M_introductory_tutorial.org
	$(EMACS_EXE) --batch --file $< \
	--load "$(DIR)/export.el" \
	--execute  "(org-beamer-export-to-pdf)"

../apdl-keyword.el : fontification.el apdl_dynprompt.txt apdl_elements.txt \
	apdl_parametric_functions.txt apdl_get_functions.txt apdl_keywords.txt \
	makefile
	$(EMACS_EXE) --batch --load $<
	cp apdl-keyword.el ..

../info/apdl-config.el : ../info/apdl-config.org
	$(EMACS_EXE) --batch --file $< \
	  --execute "(add-to-list 'load-path \"$(S_DIR)\")" \
	  --load "apdl-mode.el" --execute "(org-babel-tangle)"

# this is for compiling Elisp files, we need to set the load-path for a pristine Emacs
# -f means --funcall
# -L means --directory prepend to load-path
%.elc : %.el
	$(EMACS_EXE) --batch -L $(S_DIR) -f batch-byte-compile $<

# This is Emacs for Windows packaged with Ansys mode
.PHONEY : EMACS
EMACS : $(EMACS_PACKAGE) $(PACKAGE)
	 test -d share/emacs/$(EMACS_VER)/site-lisp/apdl-mode-$(VERSION) || \
	    mkdir -p share/emacs/$(EMACS_VER)/site-lisp/apdl-mode-$(VERSION)
	 cp -uv $(FILES) share/emacs/$(EMACS_VER)/site-lisp/apdl-mode-$(VERSION)
	 cp -uv $(EL_FILES) share/emacs/$(EMACS_VER)/site-lisp/apdl-mode-$(VERSION)
	 cp -rf $(MATLIB) share/emacs/$(EMACS_VER)/site-lisp/apdl-mode-$(VERSION)
	 test -d share/emacs/$(EMACS_VER)/site-lisp/apdl-mode-$(VERSION)/doc || \
	    mkdir -p share/emacs/$(EMACS_VER)/site-lisp/apdl-mode-$(VERSION)/doc
	 cp -uv $(TUTORIALS) share/emacs/$(EMACS_VER)/site-lisp/apdl-mode-$(VERSION)/doc
	 cp -uv share/emacs/$(EMACS_VER)/site-lisp
	 cp -uv $(EMACS_PACKAGE) apdl-mode-$(VERSION).$(EMACS_PACKAGE)
	 zip -urv apdl-mode-$(VERSION).$(EMACS_PACKAGE) \
	   share/emacs/$(EMACS_VER)/site-lisp/apdl-mode-$(VERSION)/*
	 zip -uv apdl-mode-$(VERSION).$(EMACS_PACKAGE) 

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
	../info/apdl-config.org

TAGS : $(TAGS_FILES)
	etags $(TAGS_FILES)

.PHONEY : TAG_RELEASE
TAG_RELEASE :
	git tag -a $(VERSION)
	echo "git tag -a $(VERSION)"
