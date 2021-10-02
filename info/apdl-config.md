
# Table of Contents

1.  [Conventions](#org89f3845)
2.  [Important Prerequisite](#org05d3fc5)
3.  [File suffixes](#orgd0c5d50)
4.  [Auto insertion](#orgf847457)
5.  [Miscellaneous](#orgf0e56d2)
6.  [Outlining](#org1417193)
7.  [Highlighting (Colourisation)](#orgcafc92b)
8.  [Installation dependent configurations](#orga558e72)
9.  [Ansys processes](#orgb22da1e)

Most functionality of APDL-Mode is working without additional
configurations.  APDL-Mode is intelligent enough to figure out Ansys
installation dependent paths.  For regular Ansys installations, it
chooses by default the highest installed Ansys version on your system.

APDL-Mode configures GNU-Emacs to open all files with the suffixes
".mac", ".dat" and ".inp" under apdl-mode.

You can change the APDL-Mode configurations permanently by 1. using
the Emacs customisation system

or 2. by directly manipulating the Emacs configuration file

1.  For an overview of available APDL-Mode customisation's variables
    it’s easiest to open the APDL-Mode’s customisation buffer either
    with the command ‘M-x apdl-customise-apdl’ or from the menu bar
    -> ’APDL’ -> ’Customise APDL Mode’ and search for interesting
    options.  Another way of using the customisation facility is to
    open the help of respective variables.  Let’s change for example
    the highlighting level which is stored in the customisation
    variable ‘apdl-highlighting-level’: Open its documentation with
    "C-h v" \`apdl-highlighting-level'.  In its help buffer you can
    click on the hyperlink \`customize’ at the bottom.  You might set
    a value for this particular variable only for the current session
    or you might add your choices automatically in the Emacs
    configuration file for future sessions.

2.  The following describes editing the Emacs configuration file (for
    example ~/.emacs or ~/.emacs.d/init.el).  These customisations
    are written in \`Emacs-Lisp'.  The comment sign in this language
    is `;` (one semi-colon \`;').


<a id="org89f3845"></a>

# Conventions

used in *apdl-config.el*, APDL-Mode's example configuration file.
Textual hints in *apdl-config.el* are indicated with TWO semi-colons
`;;`, please uncomment only the code lines with a SINGLE comment sign
and change them when appropriate.

Please add the interesting code lines into your GNU-Emacs init file
or load your adjustments of this file with \`(load-file
"PATH/apdl-config.el")' from your init file.


<a id="org05d3fc5"></a>

# Important Prerequisite

For using Ansys processes, like getting license information, the
Ansys Installation path with version information is necessary.
APDL-Mode is looking in different places and is using environment
variables to detect your Ansys installation but if you have a non
default installation it might not be able to find it.  In that case
please configure your installation path up to and including the
Ansys versioning number.

You can change the installation path for the current session with
the MAPDL menu entry "Change Installation Directory".

    (cond ((string= window-system "x")
    	;; This condition is an example of an installation directory on GNU-Linux
    	(setq apdl-ansys-install-directory "/appl/ansys_inc/v201/"))
    	;; the default might look like "/ansys_inc/v201/"
           (t ;; The default condition is an example of an installation directory on WINDOWS
    	;; Emacs is using here forward slashes as under Unix and not the backslash "\"!
    	(setq apdl-ansys-install-directory "D:/Ansys Inc/v201/")))
    	;; default: "C:/Program Files/Ansys Inc/v201/"

If your Ansys installation differs completely from the standard
Ansys directory structure, or if you want to use multiple versions of
Ansys, then please consult the section [Installation dependent configurations](#orga558e72).


<a id="orgd0c5d50"></a>

# File suffixes

For auto loading APDL-Mode please customise your file suffixes for
which Ansys mode is automatically called for.


## Macro files

*.mac* is the macro suffix of Ansys i. e. these macro files can be
called in the Ansys command prompt or APDL like a regular Ansys
function (without the suffix *.mac*).

APDL-Mode has configured GNU-Emacs to open files with this suffix
under apdl-mode.  Please check the *example.mac* file in the *doc*
folder.

With the following setting

    (add-to-list 'auto-mode-alist '("\\.apdl$" . apdl-mode))

files with the suffix *.apdl* will be opened under APDL-Mode.


## WorkBench generated input files

*.dat* and *.inp* are WorkBench's solver input file suffixes.  See
the file *example.dat* in the *doc* folder.

APDL-Mode has configured GNU-Emacs to open files with these
suffixes under apdl-mode.


## The Ansys Neutral file format

*.anf* is the suffix for "Ansys Neutral" files which include mostly
geometric data but also some APDL snippets. These files are used
for imports, see the file *example.anf* in the *doc* folder.

    (add-to-list 'auto-mode-alist '("\\.anf$" . apdl-mode))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; file suffixes for autoloading of APDL-Mode, appropriate file
    ;; suffixes for which Ansys mode is automatically called for
    
    ;; .mac is the macro suffix of Ansys i. e. these macros can be called
    ;; in the Ansys command prompt like a regular Ansys function (without
    ;; the suffix .mac).  See the file doc/example.mac
    
    ;; APDL-Mode has configured GNU-Emacs to open files with this suffix
    ;; under apdl-mode.
    
    ;; (add-to-list 'auto-mode-alist '("\\.mac$" . apdl-mode))
    (add-to-list 'auto-mode-alist '("\\.ans$" . apdl-mode))
    
    ;; .dat and .inp are WorkBench's solver input file suffixes
    ;; See the file doc/example.dat
    
    ;; APDL-Mode has configured GNU-Emacs to open files with these suffixes
    ;; under apdl-mode.
    
    ;; (add-to-list 'auto-mode-alist '("\\.dat$" . apdl-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.inp\\'" . apdl-mode))
    
    ;; .anf is the suffix for "Ansys Neutral" files which include mostly
    ;;  gometric data but also some APDL snippets. See the file
    ;;  doc/example.anf.
    (add-to-list 'auto-mode-alist '("\\.anf$" . apdl-mode))


<a id="orgf847457"></a>

# Auto insertion

of code.  With the following code block Emacs inserts (after query)
an outline of a code structure when creating a new file with any
suffix in the `auto-mode-alist` for `apdl-mode`, please see above
section).

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    			  ;; Auto insertion
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; auto insertion stuff (when creating a new APDL file)
    
     (auto-insert-mode 1)		        ; insert automatically templates
     (add-hook 'find-file-hook 'auto-insert) ; when opening new files
     (setq auto-insert-query t)   ; aks for auto insertion of APDL template
    
     (add-to-list 'auto-insert-alist
      '(apdl-mode . [apdl-skeleton-outline-template])) ;which template to insert


<a id="orgf0e56d2"></a>

# Miscellaneous

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    			   ;; Miscellaneous
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; use Emacs' EWW browser for locally installed help
    (setq browse-url-browser-function 'eww-browse-url)
    ;; use the default system browser
    ; (setq browse-url-browser-function 'browse-url-default-browser)
    
    ;; Some element collections, like "SHELLS" are implemented with `#'
    ;; anchors which are not standard URLs.  This leads to an error on
    ;; Windows with w32-shell-execute for locally installed help files.
    ;; Browsers don't mind the anchors, so in this case use either EWW
    ;; (see above) or specify an installed browser in your init file, see
    ;; below example for MS Edge:
    
      (defun browse-url-edge (url &optional new-window)
      (shell-command
       (concat "start msedge " url)))
    
      ; (setq browse-url-browser-function 'browse-url-edge)

    ;; The amount of time the help overlay is shown from
    ;; (`apdl-show-command-parameters').
    
    ; (setq apdl-parameter-help-duration 30) ; 30 seconds, the default
    (setq apdl-parameter-help-duration "2 min")
    
    ;; If you want to read the manual in GNU-Emacs' EWW browser.  This
    ;; might only work for locally installed help documents (a 1.7 GB
    ;; package v201) since v191 the online help is the default help
    ;; system.
    
    ;; You might use this variable to create you own templates
    ;; in `apdl-wb-template.el'.
    
    (setq apdl-wb-custom-template-directory "c:/my_macros/") ; new in 20.4.0
    
    ;;  APDL-Mode mode configures the following variable from the
    ;;  evironment to show your license usage in
    ;;  `apdl-user-license-status'.  It is the user ID you are registered
    ;;  for the license server.  This is only necessary if APDL-Mode can't
    ;;  find the environment variable USERNAME.
    
    (setq apdl-username "myUserID") 		; new in 20.4.0


<a id="org1417193"></a>

# Outlining

Activating outline minor mode for selectively hiding and revealing
code sections:

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    			     ;; Outlining
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; activating outline minor mode for selectively hiding/unhiding
    ;; sections
    
    (add-hook 'apdl-mode-hook 'apdl-outline-minor-mode) ;enable outlining


<a id="orgcafc92b"></a>

# Highlighting (Colourisation)


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


## Decoration levels

Decoration levels 0,1,2 are available.  User variable highlighting
is only in level 2 available (statically, if above flag is not
set), the current default is 2.


## Summary

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    		     ;; Highlighting/Colourisation
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; The following toggles a dynamics change of the highlighting: While
    ;; you are editing your new variable definitions highlighted and the
    ;; cursor position is shown in the parameter help overlay
    
    ;; Uncommenting the following might slow the editing of large .mac
    ;; files (but only when apdl-highlighting-level is set to 2, see
    ;; below).
    
     (setq apdl-dynamic-highlighting-flag nil)
     (setq apdl-dynamic-highlighting-flag t) ; default
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; fontification (highlighting) of user variables and decoration
    ;; levels (0,1,2 are available), user variables highlighting is only
    ;; in level 2 available (statical, if above flag is not set), the
    ;; default is 2
    
     (setq apdl-highlighting-level 1) ; default: 2


<a id="orga558e72"></a>

# Installation dependent configurations

Further things you possibly have to configure if your Ansys
installation is completely differing from a default Ansys installation
path, or if you want to mix various Ansys versions:

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    			 ;; Ansys version and paths
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
      ;; Things you might have to configure if your Ansys installation is
      ;; completely differing from default Ansys installation paths, as in
      ;; the example below, especially if you want to use mixed version
      ;; installations of Ansys:
    
    (setq apdl-ansys-help-program
        "/appl/ansys_inc/20.0.1/v201/commonfiles/help/HelpViewer/AnsysHelpViewer.exe")
        ;; the default under GNU-Linux looks like this:
        ;; "/ansys_inc/v201/commonfiles/help/HelpViewer/AnsysHelpViewer.exe"
    ;; On WINDOWS: slash before /d: is unnecessary, but possible?
    (setq apdl-ansys-help-path "d:/Program Files/Ansys Inc/16.2.0/v201/commonfiles/help/en-us/help/")
    (setq apdl-ansys-help-path "/appl/ansys_inc/16.2.0/v201/commonfiles/help/en-us/help/")
    (setq apdl-ansys-launcher "/appl/ansys_inc/v201/ansys/bin/winx64/launcher")
    (setq apdl-ansys-wb "/appl/ansys_inc/v201/Framework/bin/Linux64/runwb2")
    (setq apdl-ansys-program "/appl/ansys_inc/19.3.0/ansys/bin/ansys195")
    (setq apdl-lmutil-program "/appl/ansys_inc/19.3.0/shared_files/licensing/linx64/lmutil")


<a id="orgb22da1e"></a>

# Ansys processes


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


## Solver options

Number of cores for the run: 4 does not require HPC licenses

    (setq apdl-no-of-processors 8) ; default: 4

Which license type to use for the solver

    (setq apdl-license "preppost") ; default: "ansys"

The Ansys job name

    (setq apdl-job "harmonics1") ; default: "file"


## Summary

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    			 ;; Ansys processes stuff
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; license server configuration
    
    
      ;; GNU-Linux 64 bit only !!! Warning specifiying many license server
      ;; takes a long time for displaying the license status!!!
    
       ;; for starting the solver & apdl-license-status & Ansys help
      (setq                 ;
       ;; license servers (or license file name)
       ;; specify even the default port for lmutil (since Ansys V 12.0) on GNU-Linux
       ;; GNU-Linux: License servers separated by colons (":"), 1055 is the default port
       apdl-license-file
       "32002@ls_fr_ansyslmd_ww_1.conti.de"
    
       ;; since Ansys 12.0 there is an intermediate server for
       ;; the communication between flexlm and Ansys, 2325 is the default port
       apdl-ansysli-servers
       "2325@ls_fr_ansyslmd_ww_1.conti.de"
       )
    
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    		   ;; options when starting the solver
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
      ;; Number of cores for the run, 4 does not require HPC licenses
       (setq apdl-no-of-processors 8) ; default: 4
    
      ;;  which license type to use for the solver
       (setq apdl-license "preppost") ; default: "ansys"
    
      ;; Ansys job name
       (setq apdl-job "harmonics1"); default: "file"
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    			      ;; The End
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (provide 'apdl-config)
    
    ;;; apdl-config.el ends here, the rest are Emacs variables
    
    ;; Local Variables:
    ;; no-byte-compile: t
    ;; show-trailing-whitespace: t
    ;; indicate-empty-lines: t
    ;; time-stamp-active: t
    ;; time-stamp-format: "%:y-%02m-%02d"
    ;; End:

---

