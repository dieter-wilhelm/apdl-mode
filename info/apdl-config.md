
# Table of Contents

1.  [Conventions](#orgd1058e7)
2.  [Important Prerequisites](#orge550f74)
    1.  [Ansys Installation path with version information](#org8d6fd36)
3.  [File suffixes](#org7118306)
    1.  [Macro files](#orgca33071)
    2.  [WorkBench generated input files](#org9f2d911)
    3.  [The Ansys Neutral file format](#org7120fe0)
4.  [Auto insertion](#orgbb0dfa8)
5.  [Miscellaneous](#org38e6fd5)
6.  [Outlining](#org0c8283a)
7.  [Highlighting (Colourisation)](#org24f958b)
    1.  [Dynamic highlighting](#org6ca27d2)
    2.  [Decoration levels](#orgaec9374)
8.  [Installation dependent configurations](#orga584ee7)
9.  [Ansys processes](#org170d215)
    1.  [License server](#orgb70b74d)
    2.  [Solver options](#org0c91e3f)

Most functionality of APDL-Mode is working without additional
configurations.  APDL-Mode is intelligent enough to figure out Ansys
installation dependent paths.  For regular Ansys installations, it
chooses by default the highest installed Ansys version on your system.

APDL-Mode configures GNU-Emacs to open all files with the suffixes
".mac", ".dat" and ".inp" under apdl-mode.

The customisations itself are written in \`Emacs-Lisp'.  The comment
sign in this language is `;` (one semi-colon \`;').


<a id="orgd1058e7"></a>

# Conventions

used in *apdl-config.el*
Textual hints in *apdl-config.el* are indicated with TWO semi-colons
`;;`, please uncomment only the code lines with a SINGLE comment sign
and change them when appropriate.

Please add the interesting code lines into your GNU-Emacs init file or
load this file in the init file with \`(load-file
"PATH/apdl-config.el")'.


<a id="orge550f74"></a>

# Important Prerequisites


<a id="org8d6fd36"></a>

## Ansys Installation path with version information

If you have a non default installation APDL-Mode might not be able
to find the Ansys installation.  Please configure your installation
path up to and including the versioning number.

    (cond ((string= window-system "x")
    	;; This is an example of an installation directory on GNU-Linux
    	(setq apdl-ansys-install-directory "/appl/ansys_inc/v201/"))
    	;; the default might look like "/ansys_inc/v201/"
           (t ;This an example of an installation directory on WINDOWS
    	;; Emacs is using here forward slashes as under Unix and not
    	;; the backslash "\"!
    	(setq apdl-ansys-install-directory "D:/Ansys Inc/v201/"))
    	;; default: "C:/Program Files/Ansys Inc/v201/"

If your Ansys installation differs completely from the standard
Ansys directory structure, or you want to use a mixed version
system, then please consult the section \`INSTALLATION PATHS'
further below.


<a id="org7118306"></a>

# File suffixes

for auto loading APDL-Mode Configure your file suffixes for which
Ansys mode is automatically called for.


<a id="orgca33071"></a>

## Macro files

*.mac* is the macro suffix of Ansys i. e. these macro files can be
called in the Ansys command prompt or APDL like a regular Ansys
function (without the suffix *.mac*).

APDL-Mode has configured GNU-Emacs to open files with this suffix
under apdl-mode.

With the following setting

    (add-to-list 'auto-mode-alist '("\\.ans$" . apdl-mode))

files with the suffix *.ans* will be opended in Emacs under
APDL-Mode.  Please check the *example.mac* file in the *doc* folder.


<a id="org9f2d911"></a>

## WorkBench generated input files

*.dat* and *.inp* are WorkBench's solver input file suffixes.  See
the file *example.dat* in the *doc* folder.

APDL-Mode has configured GNU-Emacs to open files with these
suffixes under apdl-mode.


<a id="org7120fe0"></a>

## The Ansys Neutral file format

*.anf* is the suffix for "Ansys Neutral" files which include mostly
gometric data but also some APDL snippets. These files are used for
imports, see the file *example.anf* in the *doc* folder.

    (add-to-list 'auto-mode-alist '("\\.anf$" . apdl-mode))


<a id="orgbb0dfa8"></a>

# Auto insertion

of code With the following code block Emacs inserts (after query) an
outline of a code structure when creating a new file with any suffix
in the `auto-mode-alist` for `apdl-mode`, please see above section).

    (auto-insert-mode 1)         ; insert automatically templates
    (add-hook 'find-file-hook 'auto-insert) ; when opening new files
    (setq auto-insert-query t)   ; ask before insertion of an APDL
    			     ; template
    (add-to-list 'auto-insert-alist
       '(apdl-mode . [apdl-skeleton-outline-template])) ;which template
    						    ;to insert


<a id="org38e6fd5"></a>

# Miscellaneous

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    			  ;; Miscellaneous
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (setq apdl-parameter-help-duration "2 min")
    (setq apdl-parameter-help-duration 30) ; 30 seconds
    
    ;; If you want to read the manual in GNU-Emacs' EWW browser.  This
    ;; might only work for locally installed help documents (1.7 GB
    ;; package v201) since v191 the online help is the default.
    
    ; (setq browse-url-browser-function 'eww-browse-url)


<a id="org0c8283a"></a>

# Outlining

Activating outline minor mode for selectively hiding and unhiding
code sections:

    (add-hook 'apdl-mode-hook 'apdl-outline-minor-mode) ;enable outlining


<a id="org24f958b"></a>

# Highlighting (Colourisation)


<a id="org6ca27d2"></a>

## Dynamic highlighting

of variables.  The following toggles a dynamics change of the
highlighting: While you are editing your new variable definitions
highlighted and the cursor position is shown in the parameter help
overlay

Uncommenting the following might slow the editing of large .mac
files (but only when apdl-highlighting-level is set to 2, see
below).

    (setq apdl-dynamic-highlighting-flag nil)
    ; (setq apdl-dynamic-highlighting-flag t) ;default: t


<a id="orgaec9374"></a>

## Decoration levels

Decoration levels 0,1,2 are available.  User variable highlighting is
only in level 2 available (statical, if above flag is not set), the
current default is 2

    (setq apdl-highlighting-level 1) ; default: 2


<a id="orga584ee7"></a>

# Installation dependent configurations

Further things you possibly have to configure if your Ansys
installation is completely differing from a default Ansys installation
path, or if you want to mix various Ansys versions:

    (setq apdl-ansys-help-program
        "/appl/ansys_inc/20.0.1/v201/commonfiles/help/HelpViewer/AnsysHelpViewer.exe")
        ;; normally it looks like this:
        ;; "/ansys_inc/v201/commonfiles/help/HelpViewer/AnsysHelpViewer.exe"
    ;; On WINDOWS: slash before /d: is unnecessary, but possible?
    (setq apdl-ansys-help-path "d:/Program Files/Ansys Inc/16.2.0/v201/commonfiles/help/en-us/help/")
    (setq apdl-ansys-help-path "/appl/ansys_inc/16.2.0/v201/commonfiles/help/en-us/help/")
    (setq apdl-ansys-launcher "/appl/ansys_inc/v201/ansys/bin/winx64/launcher")
    (setq apdl-ansys-wb "/appl/ansys_inc/v201/Framework/bin/Linux64/runwb2")
    (setq apdl-ansys-program "/appl/ansys_inc/19.3.0/ansys/bin/ansys195")
    (setq apdl-lmutil-program "/appl/ansys_inc/19.3.0/shared_files/licensing/linx64/lmutil")


<a id="org170d215"></a>

# Ansys processes


<a id="orgb70b74d"></a>

## License server

or license file.

The more license servers are specified in `apdl-license-file` the
longer it takes to get the license status.

License server configuration: License servers (or license file
name) you have to specify also the port for (since Ansys V12.0).
On GNU-Linux GNU-Linux: License servers are separated by colons
(":"), on Windows with semicolon ";".  1055 is the default port.

the following variable APDL-Mode is checking the environment
variables AnsysLMD\_LICENSE\_FILE and MD\_LICENSE\_FILE.

    (setq apdl-license-file
      "1055@frlifl01.auto.contiwan.com:1055@frlifl02.auto.contiwan.com")

since Ansys 12.0 there is an intermediate server for the communication
between flexlm and Ansys WorkBench, 2325 is here the default port.
But the solver/interpreter is checking this server as well!?

    (setq apdl-ansysli-servers
       "2325@frlifl01.auto.contiwan.com:2325@frlifl02.auto.contiwan.com")


<a id="org0c91e3f"></a>

## Solver options

Number of cores for the run: 4 does not require HPC licenses

    (setq apdl-no-of-processors 8) ; default: 4

Which license type to use for the solver

    (setq apdl-license "struct") ; default: "ansys"

Ansys job name

    (setq apdl-job "harmonics1"); default: "file"

---

