
# Table of Contents

1.  [Introduction](#org1457a48)
2.  [Some Highlights](#org952d117)
3.  [Installation](#orged248d6)
4.  [First Steps](#org744e6a5)
5.  [Configuration and Customisation](#orge1ac101)
6.  [Bugs and Problems](#org803b921)
7.  [News](#orge819d18)
8.  [Further Resources](#org7b477eb)
9.  [Acknowledgements](#org3981b11)
10. [Todos](#orgd2adab8)
11. [GNU GPL v3 License](#orge7e3a00)



## The GNU-Emacs major mode for the scripting language APDL

APDL-Mode version 20.6.0

[![img](https://melpa.org/packages/apdl-mode-badge.svg)](https://melpa.org/#/apdl-mode)
[![img](https://stable.melpa.org/packages/apdl-mode-badge.svg)](https://stable.melpa.org/#/apdl-mode)

Copyright (C) 2006 - 2021  H. Dieter Wilhelm, GPL V3

![img](doc/ansys+emacs2020-03.png)


<a id="org1457a48"></a>

# Introduction

APDL ([Ansys Parametric Design Language](https://de.wikipedia.org/wiki/ANSYS_Parametric_Design_Language)) is the solver scripting
language of the mechanical FEA (Finite Element Analysis) suite [Ansys](http://www.ansys.com)
(ANalysis SYStem, registered TM).

APDL-Mode (formerly Ansys-Mode) is - in conjunction with the
[GNU-Emacs](https://www.gnu.org/software/emacs/) editor - an advanced APDL environment with features like,
pin-pointing the APDL reference documentation, keyword completion,
code templates, dedicated highlighting, solver communication
(GNU-Linux only), license reporting, etc.  Over the years it has
accumulated lots of features for writing and debugging FEA complete
models in APDL code. Please convince yourself with the APDL-Mode
in-depth documentation.

With the advent of the modern Ansys GUIs - like \`WorkBench' or
\`Discovery AIM' - the usage of APDL as a wholesale modelling
language has diminished for non automated tasks.  But APDL is here
to stay: \`WorkBench' and \`AIM' operate exclusively the Ansys solver
with it!  They are producing and sending APDL input (.dat) files to
the solver.  For a true understanding of the GUIs' inner workings a
study of their APDL code is prerequisite!  Moreover, the GUIs are
not supporting all solver features.  So "Command (APDL)" objects
are used to enhance the GUIs' modelling capabilities.

Nowadays I find APDL-Mode mostly useful for studying solver (.dat)
files which were created by WorkBench.  And, especially, for writing
WorkBench \`Command' objects and inspecting longer command snippets
from other sources.  Accessing the Ansys APDL reference documentation
with the detour of searching the keywords alone is worth using
APDL-Mode!

When you "Export" (or "Import") such a WorkBench "Command (APDL)"
object it becomes associated with a file and is then editable with a
third party editor.  When you have modified the file then the "File
Status" in Workbench changes and you can pull-in the updated content
with the "Refresh" button.

![img](doc/connect_command_snippet_to_file.png)


<a id="org952d117"></a>

# Some Highlights


## In-built APDL command help with argument counter

Especially for commands with a large number of arguments it is
cumbersome to count the arguments, **M-?** facilitates this for you
and visualises dynamically at which parameter position your cursor
currently is.

![img](doc/parameter_help2.png)


## Pin-pointing the relevant Ansys documentation

You can browse (with **C-c C-b** on a code line) the relevant Ansys
manual entry directly without the detour of searching in the Ansys
online help.  This works not only for all APDL commands but also
for element names and more general manual topics!

Since Ansys v191 the online help is the default.  APDL-Mode is
using your default browser but to be able to access the online help
you must register or you must start the online help once from any
licensed Ansys product.  I recommend installing the local Ansys
documentation package, access time is much shorter.

The image below is showing a manual entry in GNU-Emacs' eww
browser.  You are able to consult the manuals side-by-side your
APDL code.

![img](doc/browse_manual.png)


## Command Object Templates and Code Highlighting Example

You are learning and also coding APDL best by example!  APDL-Mode
provides coding examples, here called templates, for the most often
used subjects.  Moreover you are able to compile your own WorkBench
/ Discovery AIM Command templates and have them all immediately
available in APDL-Mode for inspection and inclusion.

APDL-Mode is helping you with code colourisation to distinguish
valid commands, user variables, element names, comments, APDL
functions and further subjects.

Below image shows GNU-Emacs with a ripped off APDL-Mode menu, the
APDL variable buffer, the APDL file itself and an APDL template
preview.

![img](doc/ansys-mode.jpg)


## Code outlining for inspecting WorkBench solver (.dat) files

APDL-Mode hides the normally uninteresting but usually very large
number blocks.

![img](doc/hidden_blocks.png)

The image below shows the unhidden content.

![img](doc/unhidden_blocks.png)


<a id="orged248d6"></a>

# Installation

Copyright (C) 2006 - 2020  H. Dieter Wilhelm, GPL V3

Please install [GNU-Emacs](https://www.gnu.org/software/emacs/) first, you should install at least Emacs
version 25.1.  (If you are new to this editor please check the
tutorial in its \`Help' menu, please really do it ;-)


## Melpa

APDL-Mode is now available on the GNU-Emacs packages archive [Melpa](https://melpa.org/).

[![img](https://melpa.org/packages/apdl-mode-badge.svg)](https://melpa.org/#/apdl-mode)
[![img](https://stable.melpa.org/packages/apdl-mode-badge.svg)](https://stable.melpa.org/#/apdl-mode)

Please add

    (add-to-list 'package-archives
    	  '("melpa" . "https://melpa.org/packages/") t)

to your initialisation file.  Then type: \`M-x list-packages', find
\`apdl-mode', mark it with \`i' and install it with \`x'.

If you prefer the stable package archive instead of development
versions exchange above package source with

    (add-to-list 'package-archives
    '("melpa-stable" . "https://stable.melpa.org/packages/") t)


## Manual installation

If you are behind a corporate firewall and you are not able to
install APDL-Mode from Emacs' package menu, you can download and
install APDL-Mode manually:

-   Download the latest APDL-Mode's tar package from [Melpa](https://melpa.org/#/apdl-mode) or - for a
    released package - from the [Github](https://github.com/dieter-wilhelm/apdl-mode/releases/tag/20.1.0) release page.
-   Install the package within Emacs: Please type \`M-x
    package-install-file <RET>' and select your downloaded tar file.

That's it.

**Hint:** If you are getting an error message "package.el is not yet
initialized", you are using Emacs' packaging system for the very
first time.  It is necessary to initialise this machinery once,
please type: \`M-: (package-initialize) <RET>' (it's an 'ALT + :'
colon!)  and then apply \`M-x package-install-file <RET>' again.


## Development and source code installation

Please clone the APDL-Mode git archive in a directory of your
choice:

    git clone https://github.com/dieter-wilhelm/apdl-mode.git

or download the Github ZIP archive and add the following line

    (require 'apdl-mode)

to your initialisation file (the source directory must be set in
the \`load-path' variable as well).


<a id="org744e6a5"></a>

# First Steps

Please type \`M-x apdl' which opens a buffer in APDL-Mode and you can
inspect the menu bar's \`APDL' and \`MAPDL' entries.  For existing
APDL files please type \`M-x apdl-mode' if the mode is not activated
already, please see the following Configuration section for the
pre-configured file suffixes.

For further guidance please select the APDL menu \`APDL-Mode
Documentation' (or type \`C-c C-h') and \`Describe APDL-Mode' (or type
\`C-h m') for the list of its keybindings.  You might also check the
introductory APDL-Mode [tutorial](doc/A-M_introductory_tutorial.md).

APDL-Mode is tested with Ansys v193 and v201 under Windows 10, as
well as under Emacs-25 and 26 under GNU-Linux and Windows.


<a id="orge1ac101"></a>

# Configuration and Customisation

Most functionality is working without additional configurations.
APDL-Mode is intelligent enough to figure out Ansys installation
dependent paths.  For regular Ansys installations, it chooses by
default the highest installed Ansys version on your system.

APDL-Mode configures GNU-Emacs to open all files with the suffixes
".mac", ".dat" and ".inp" under apdl-mode.

Please read the [apdl-config](info/apdl-config.md) documentation for further options or
open the accompanying configuration [example-file](info/apdl-config.el).


<a id="org803b921"></a>

# Bugs and Problems

Feedback is always welcome.  If you have issues while installing and
running this mode or want to suggest improvements you have the
following options:

1.  Write an email to the [mode maintainer](mailto:dieter@duenenhof-wilhelm.de).  Please trigger a bug report
    form from the APDL-Mode menu or by calling the function
    \`apdl-submit-bug-report'.  Even if you have not configured Emacs to
    send emails, please copy the content of the mail template for the
    maintainer.

2.  You might issue bug reports or feature requests at APDL-Mode's
    [issues site](https://github.com/dieter-wilhelm/apdl-mode/issues)

3.  And you can leave comments and hints at the [APDL-Mode page](https://www.emacswiki.org/emacs/APDLMode) of the
    [Emacs Wiki](https://www.emacswiki.org).


<a id="orge819d18"></a>

# News

For further news please have a look into the [NEWS](info/NEWS.md) file.


<a id="org7b477eb"></a>

# Further Resources

If you want to read further details regarding the APDL scripting,
GNU-Emacs and other APDL editors please read the [RESOURCES](info/resources.md) file.


<a id="org3981b11"></a>

# Acknowledgements

My acknowledgements to Tim Read and Geoff Foster for their
ansys-mod.el from 1997 which triggered the idea in 2006 to start
APDL-Mode.

Parts of APDL-Mode were based on octave-mod.el: Copyright (C) 1997
Free Software Foundation, Inc.  Authors: [Kurt Hornik](mailto:Kurt.Hornik@wu-wien.ac.at) and [John Eaton](mailto:jwe@bevo.che.wisc.edu).

I received moreover support and feedback from many individuals, thank
you very much!


<a id="orgd2adab8"></a>

# Todos

Please check the [TODO](info/TODO.md) file.


<a id="orge7e3a00"></a>

# GNU GPL v3 License

The GNU General Public License version 3.  There are no costs and no
usage restrictions even in commercial application, please convince
yourself with the [LICENSE](info/LICENSE) file.

