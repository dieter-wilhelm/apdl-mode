#This file is included into the README
#The comments (^#) will be removed 
#The creation date will be included

README for ansys-mode.tar.gz
This file is created from Readme.txt

You only need ansys-mode.el for running Ansys mode in Emacs.  This
lisp package provides Emacs support for the Ansys command language.
It defines Ansys mode, a major mode for editing APDL (Ansys Parametric
Design Language) files and managing Ansys processes.

== Requirements ==

You need Gnu Emacs version 22.  Ansys mode does not run with older
versions of Emacs, see ftp://ftp.gnu.org/pub/gnu/emacs/windows/ for
windows versions.

== Installation ==

=== Short instructions ===

   (autoload 'ansys-mode "ansys-mode100" nil t)
            ;ansys-mode100.el directory is in load-path!
   (add-to-list 'auto-mode-alist '("\\.mac\\'" . ansys-mode))
       ;assuming your Ansys files are suffixed .mac.
   (setq auto-insert-query t) ;auto insert only after request
   (add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton]))

=== Verbose instructions ===

* The most direct way of using ansys-mode100.el is storing the file
  somewhere on disk and loading the included definitions from there
  with the standard Emacs command `load-file' i. e.  type `M-x
  load-file RET', M-x means typing first the Alt-key and then the
  x-key simultaneously.  This gives you a prompt where you can type
  `load-file' followed by the RET key to conclude the command.
  Then Emacs will prompt you for a file location. If you feel
  unsure about these concepts I urgently recommend to you strolling
  through the build-in tutorial of Gnu Emacs (please have a look at
  the help menu), it doesn't take much time and the investment will
  also help you enormously speeding up your general editing.  When
  the definitions are loaded into memory you must type `M-x
  ansys-mode RET' for the files of interest to activate the mode.

* When it becomes annoying to load the lisp file ansys-mode100.el
  every time you are starting Emacs anew you can specify the path for
  this file in your `~/.emacs' file (the configuration file `.emacs'
  of Emacs in your home directory `~') and auto-load `ansys-mode':

     (add-to-list 'load-path
                  "c:\\your\\directory\\where\\ansys-mode100.el\\recides")
     (autoload 'ansys-mode "ansys-mode" "Activate Ansys mode." 'interactive)

  So you only have to type `M-x ansys-mode RET' for every
  interesting APDL file.

* When you intend to use the mode automatically, e.g. for all
  `.mac' and `.inp' files you are invoking, add the following to
  your `.emacs' file:

     (add-to-list 'auto-mode-alist '("\\.inp$" . ansys-mode))
     (add-to-list 'auto-mode-alist '("\\.mac$" . ansys-mode))

* In case you also also want to enjoy the auto insertion feature,
  which puts some predefined body of Ansys commands optionally to
  every new file--only those opened with ansys-mode, of
  course--append the following to `.emacs':

    (setq auto-insert-query t) ;insert only after request
    (add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton]))

It might be helpful for you to visit the Emacs wiki
(http://www.emacswiki.org/cgi-bin/wiki/AnsysMode) for further
instructions.

== Usage ==

* Please invoke ansys-mode and type C-h m for basic usage hints.

It might be helpful for you to visit the Emacs wiki
(http://www.emacswiki.org/cgi-bin/wiki/AnsysMode) for further
instructions.

#Here'll be the file contents of ansys-mode.tar.gz appended
Files of ansys-mode.tar.gz:
