
# Table of Contents

1.  [Version 20.5.0?](#orgf71e7a7)
2.  [Version 20.4.0, completion and help updated to 2020R1](#orga47265b)
    1.  [Todos, bugs](#org0635f1b)
    2.  [minor bugs](#org731f1f2)
    3.  [check](#org7d1b814)
    4.  [wishes](#orgd190bdc)
        1.  [templates](#org5964081)
        2.  [misc](#org3abb4c5)
    5.  [Done](#org9f34967)
    6.  [Release proceedures](#orgc487f7e)
3.  [20.3.0, Melpa](#orgb5a10d6)
4.  [20.2.0](#orgdafa79a)
5.  [20.1.1](#orgd917c2f)
6.  [162-2:](#orgc67c0ee)
    1.  [Bugs](#orged56d71)
    2.  [Freeze](#org6cc3d56)
    3.  [Wish-list](#org8edf6cf)
7.  [Deficiencies:](#org877161f)
8.  [Procedures](#orgdf3822c)
    1.  [Freeze proceedures](#orga0d6162)
    2.  [Release](#orgbfdce1c)
9.  [Ideas for further versions](#orgb67226d)
    1.  [Parameter help and documentation](#org6d4c3a9)
    2.  [Ansys process and interpreter buffer](#orgcdcc643)
    3.  [Skeletons, outline and abbrevs](#org443c8f2)
    4.  [Miscellaneous](#org878706c)
    5.  [Ansys syntax restrictions not (yet) accounted for](#org97472d9)
    6.  [Unknown Ansys stuff](#org8636548)



<a id="orgf71e7a7"></a>

# Version 20.5.0?


<a id="orga47265b"></a>

# TODO Version 20.4.0, completion and help updated to 2020R1


<a id="org0635f1b"></a>

## Todos, bugs

-   Update -config.org with the new defcustoms
-   work on the Info index


<a id="org731f1f2"></a>

## minor bugs

-   fmagsum in -template-post1 not fontified
-   APDL-Mode documentation is only shown in default browswer, not
    shown in eww
-   C-u C-c C-b in browser on Win10 for "All"stuff isn't skipping to
    the respective page section on Chrome and Edge!?
-   1/en, en is variable but fraction not fontified?


<a id="org7d1b814"></a>

## check

-   changed file size without warning to 30 MB what is Emacs-26
    using?


<a id="orgd190bdc"></a>

## wishes


<a id="org5964081"></a>

### templates

-   Add more WorkBench/ Discovery AIM templates
-   intelligent template selector with keywords in the line of helm
-   what is the advantage of ARG1-9? => parameter system, should I
    include ARG[1-9] optionally?


<a id="org3abb4c5"></a>

### misc

1.  derive A-M from prog-modes => branch derived
2.  Travis CI continuous integration!
3.  <https://github.com/marketplace/coveralls>
4.  Start optionally runbWB2 &#x2013;aim, Discovery AIM
5.  Ansys is often used synonymous for Ansys MAPDL solver /
    interpreter or Ansys Classics GUI
6.  Melpa README, better wait till emacs-27


<a id="org9f34967"></a>

## Done


<a id="orgc487f7e"></a>

## Release proceedures

-   NEWS.org
-   README.org News

gca = git commit -a = gau + gc
gta = git tag -a 20.4.0

Attention: in apdl<sub>keywords.txt</sub> there are many
trailing whitespaces (delete-trailing-whitespace)

whitespace-mode
package-lint
checkdoc, flycheck etc. : from makefile?

APDL<sub>MAJOR</sub> := 20
APDL<sub>MINOR</sub> := 5
APDL<sub>PATCH</sub> := 0

(tags-query-replace "20\\.4\\.0" "20.5.0")

(tags-query-replace "- 2020" "- 2021")

(tags-query-replace "v201" "v211")

(tags-query-replace "26\\.3" "27.1")
(tags-search "((emacs \\"25.1\\"))")


<a id="orgb5a10d6"></a>

# DONE 20.3.0, Melpa

<span class="timestamp-wrapper"><span class="timestamp">[2020-03-24 Di]</span></span>
feature freeze <span class="timestamp-wrapper"><span class="timestamp">[2020-03-20 Fr]</span></span>


<a id="orgdafa79a"></a>

# DONE 20.2.0

<span class="timestamp-wrapper"><span class="timestamp">[2020-03-10 Di]</span></span>


<a id="orgd917c2f"></a>

# DONE 20.1.1

-   are the material properties documented, matlib?  Matlib is used
    in -template.el, how about document this directory somewhere?
    Done in matlib/README.org

-   document the APDL Parameter Help Duration in M-?: Done in
    -config.org!
-   bug-report about package.el "NAME-readme.txt", done


<a id="orgc67c0ee"></a>

# 162-2:


<a id="orged56d71"></a>

## Bugs

-   C-c C-s first time visit: User variables file-name-extension:
    Wrong type argument: stringp, nilError during redisplay:
    (jit-lock-function 1) signaled (wrong-type-argument stringp nil)
-   /sho is not not font-locked
-   c\*\*\* without argument is not font-locked
-   jit-lock-function errors
-   RESCOMBINE is not fontified
-   M-? on an empty buffer returns **abbr** help instead of a warning
-   C-c C-j tries to send a next line in the LAST line and after an
    /exit command
-   -send-to-ansys, -send-above copy empty regions or only comments
-   importing template-header into empty file doesn't change file
    status to changed!
-   error for -skeleton-outline-template when called ansys-mode
    for an unnamed file without extension.
-   APDL + WB -> Window Name is &#x2026; - Mechanical APDL (Ansys
    Structural)


<a id="org6cc3d56"></a>

## Freeze

-   Documentation of installation, usage and configuration!
    -   C-c C-{w,y,x}
    -   sending to classics
-   Menu for checking availablity for the X11 helper functions
-   List of all new functions <- bug-report.sh


<a id="org8edf6cf"></a>

## Wish-list

-   C-c C-v show only the latest most current variable, if in a
    variable block
-   remove spurious defcustoms which are calculated, or leave them as
    last resort?
-   a-start-wb: start-process seemingly uses the current working
    directory, there might be a problem, when "pwd" of the current
    buffer was/is an unavailable network drive .
-   add bbdb files to emacs packages
-   -help-program-parameters obsolete
-   option to make eww default help browser
-   MinGWin: FindWindowFindWindow(CLASSNAME, WINDOWNAME)
    
    -   ActivateWin: SetFocus()  + SendKeys? SendKeys
    
    <https://www.transmissionzero.co.uk/computing/win32-apps-with-mingw/>
    <https://msdn.microsoft.com/en-us/library/aa383749.aspx>

-   C-c C-a does not take signs (e.g. +13.0, -8.3) befor numbers into account
-   Targeting Ansys Aim
-   customisation :version attribute is not clear and not defined for
    new stuff
-   Mouse completion is working with **middle** mouse button (mouse-2) but only
    on the second LMB or, in the case of C-c C-s, not at all.
-   \*mwrite
    \*cfclos
    M-? shows help for \*mwrite, when cursor behind \*cfclos and in the
    last line without \n
-   Make a usage.org, splice into a-mode.el
-   -wb function (doesn't adjust along the decimal point)? -> align-rules-list
-   Changing license type on the fly (with C-u) for -display-license-status
-   tool tip help-echo properties for keywords!
-   permanent saving option for -license-file, -lmutil, -program, &#x2026;?
-   Check for valid license types for the solver
-   license and template status line always in the first visible line
    of buffer (in-place-annotations?)
-   splash screen?
-   -license-status optional filters for output
-   C-c C-c checking blocks
-   include ../ansys/apdl macros?
-   ../apdl/start162.ans valuable?
-   add screw thread capability to WB translation
-   color scheme of \*msg command is only readable on terminals
-   abreviated input method for all graphics functions zoom, etc.
-   defaults for template MP
-   add adwords to gh-pages
-   -show-command-parameters is not dynamically updated in the first line
-   Correct the creep data together with young's modulus and total
    strain to creep strain!
-   variables behind / and \* without whitespace in between are not
    highlighted! try changing the character syntax???
-   implement -hide-region also for rotated nodes in WB .dat files
    (frictionless support)
-   started and commented out highlighting of solver output
-   Variables are not highlighted in -skeletons, it is specified in
    -mode only for files with .mac suffix; \*create file tmp,mac:
    macro call: tmp, is not highlighted
-   ansys-template.el:85:13:Warning: reference to free variable
    \`ansys-skeleton-overlay'
-   mouse-choose-completion is obsolete since E23.2
-   add timer customisation for -command-parameter-help variable 1
    min, 2 min, etc.
-   check \*vwrite with gui mode and interactive mode (graphics window?)
-   add element numbers to the general completion list, yes or no?
-   WikEmacs, Melpa, GNU ELPA and/or marmalade, Ansys-Mode debian
    .deb package
-   HTML APDL documentation
    here is the tanslation of names to html files:
    -   link to Ansys-Mode APDL reference:
    -   Chapters of structural APDL documentation:
-   TODOS: in fontification.org
    -   deletion of vectors \*del,Vector without request? ,,nopr?
    -   check character variables 32 Chars. and highlight accordingly
        string 128, char 8
    -   \*vscfun: mean, stdev, &#x2026;
-   \_RETURN (-mode) somewhat milder red, clashes with reseved vars!
-   test server specifications (menu!) make interconnect
    conditional of the Ansys version >= 120
-   explain fill/\*vfill under the "looping" commands: Generates a line
    of nodes between two existing nodes.
-   switch automatically to read only mode for \*.dat files?
-   force harmonic table (\*vfill) example in -skeleton-bc, make a
    ansys&#x2026;-template out of two-mass-harmonics.mac
-   -program, -license-file and -ansysli-servers should show the
    current selections
-   check -license-file var with -license-file-check
    -   Emacs var  :DONE:
    -   env vars  :DONE:
    -   activate -license-file function :TODO:
-   document Lagrange contact behaviour for contact/press-fit
    skeleton, critical to element differences!
-   -dynamic-highlighting is sluggish, highlighting somehow
    retarded!!! Still??
-   constraint equations in -ansys-template.el and get functions
-   enable hiding of geometry items in .anf files
-   fontification.el
    -   add the 4 Ansys neutral file (aux15) functions to the parameter-help,
        .anf files from DM anf export
          kpt &#x2013; Write keypoints or vertices into the database
          lcurv &#x2013; Write lines or edges into the database
          asurf &#x2013; Write the area or face information into the database
          vbody &#x2013; Define a B-rep solid.
    -   add Hlp<sub>G</sub><sub>OPE6</sub><sub>NeutralFile.html</sub> (aux15 utility functions)
        commands to the fontification and help stuff: KPT, LCURV,
        ASURF, VBODY. (functions in ANF files) from the operations
        guide \`ans<sub>ope.pdf</sub>'
    -   some functions in -skeleton-function are not highlighted and
        completable, whitespace problem befor paren&#x2026;
        and: "nele" undocumented command and function nelem()
-   inconsistencies in A-M<sub>introductory</sub><sub>tutorial.org</sub>
    -   the parameter help overlay is dated
    -   in variable.png symbols Ns and Ls are not highlighted as
        variables
    -   change sequence of alignment.png first not aligned, second aligned
    -   Too difficult: slide of extensibility, showing Emacs
        self-documenting help system for a template function


<a id="org877161f"></a>

# Deficiencies:

-   **Highlighting:** Experimental user variable highlighting
    does not take into account:
    -   clearing of variables and
    -   usage of variables before their definitions (uninitialised
        variables)
    -   the variable fontification might clash with Ansys specifiers
    -   string substitution of %variables% in strings does not highlight
        them in the proper variable face
-   **Highlighting:** A label :LABEL may appear behind the /input
    command and is not highlighted as label
-   **Highlighting:** An apostrophy clashes with the Ansys "char"
    specifier Keybinding: M-Tab is captured under
    Windows and some GNU-Linux desktops Workaround: Press
    instead of M-Tab (ALT + TAB) the ESC key and then
    the Tab key (ESC, TAB) or apply the command from
    the menu bar
-   **Keybinding:** M-j: When there is already a \`&' in a format command
    (like \*msg, \*vwrite, \*mwrite)
    ansys-indent-format-line inserts a redundant one
-   **Keybinding:** C-c C-j jjj not working with Emacs version < Emacs-24
-   **Completion:** Of parametric function parentheses
    completes redundant closing parentheses
-   **Completion:** A mouse selection from the **Ansys-completion** buffer
    is only inserted upcased.
-   **Completion:** Ansys command 'C\*\*\*' will not be completed
-   **Skeleton:** Mouse selection of -structural-template does not work
    (because of stipulated user text input from this skeleton)
-   **Navigation:** C-c { does not skip an overlay in a number-block
    (M-{ does it though))


<a id="orgdf3822c"></a>

# Procedures

-   **GNU-Linux:** instead of GNU/Linux as FSF suggesting ;-)
-   **Mode Help:** keyboard input is quoted in "", emphasizing in \`' and
    keys in <>
-   **APDL templates:** minimal working examples ending with -template
-   **Menu:** -> indicates the following sub menu entry ->
-   **M-x compile:** ALL ;-)
    
    GH-wiki is repo with write access for world, separate from the A-M repo!


<a id="orga0d6162"></a>

## Freeze proceedures

-   check whether all undocumented commands
    <(find-tag "Ansys_undocumented_commands")>
    are still working in V 162
-   (tags-search "-TODO-")
-   (tags-search "-FIXME-")
-   checkdoc, -ansys-template.el, -ansys-process.el, -mode.el done except
    \\\\<keymap> & \\\\[function]
-   README &#x2013; installation (Emacs Wiki format) and accompanying files,
    features, news, history
-   update the mode help, update version numbers, default specifiers
-   update defcustom list in ./bug-report.sh -> ansys-submit-bug-report
-   update/complete skeletons menu
    
    (tags-query-replace "161-2" "162-1")
    
    (tags-query-replace "16.2.0" "17.1.0")
    (tags-query-replace "16.1.0" "16.2.0")
    (tags-query-replace "Ansys 16" "Ansys 17")
    
    (tags-query-replace "24\\.5" "25.1")
    
    (tags-query-replace "161" "162")
    
    (tags-query-replace "20\\.1\\.0" "20.1.1")
    (tags-query-replace "\\"1.1\\"" "\\"1.2\\"")
    
    (tags-query-replace "- 2020" "- 2021")
    
    checkdoc then dry run: Emacs
    24.5 -Q testing: example.mac /\*commands and default command
    lines, every menu entry.  byte-compile-file then dry run
    profiling, major mode conventions: multiple loading of this mode?.
-   check Emacs versions on longterm OS systems compile with 24.X,
    then pretests, emacs-snapshots, clash with Emacs releases (yearly
    Emacs cycle)?


<a id="orgbfdce1c"></a>

## Release

-   Add the latest news from NEWS.org to the README.org
-   update Emacs wiki (README), home page, GitHub
-   publication emails (with tutorial and news):
    
    The project is hosted on
    
    <https://github.com/dieter-wilhelm/apdl-mode>
    
    Where you will find the latest development version.
    
    Stable versions and prebuild packages are on the releases page:
    
    <https://github.com/dieter-wilhelm/apdl-mode/releases>
    
    Dieter


<a id="orgb67226d"></a>

# Ideas for further versions


<a id="org6d4c3a9"></a>

## Parameter help and documentation

-   Enable a mouse button to unhide hidden regions, enable an
    interactive way to unhide regions, when in the region: Return
    opens hidden region, &#x2026;
-   dynamic completion and help of parameter options, depending on
    the contex like the one in bash
-   makeinfo documentation with org-mode ox exporter
-   make completion of templates with <mouse 1> button additionally
    to <mouse 2> as in -complete-symbol.
-   create a function showing deprecated elements and their
    replacement.  Inspirations from eldoc-mode, show replacements of
    deprecated elements?
-   create a reference card
-   show list of license products and their license feature names or
    translate it in the license status from the licensing guide
    (product variable table)
-   C-c C-v show (optionally) only variables defined up to current
    cursor line.  Make the line number display in
    \`ansys-display-variables' (hyper-)links to the corresponding code
    line in the respective APDL file.  (See \`occur' function.)  Or
    use the imenu mechanism for this and display the variables in the
    speedbar. Count the number of user variables when displaying them
-   refcard, etc; Emacs help guidelines
    GNU programming guideline: More requires?,
-   display alternatives to/swap deprecated element types
-   M-? Help: parametric functions are not explained with the help
    command
-   M-? Help: if there are ignored characters behind the keyword, the
    keyword is not found
-   M-? the command help does not work when there are solver ignored
    characters behind the unique command name, example: \*VWROOOOM.
-   M-?: following a variable allocation with \`='
-   M-?: In the last empty line displays overlay below command
    instead above
-   Provide Ansys command completion and command-help in comint
    buffer
-   Include all inquiry functions (see UPF documentation)
-   Include the \_RETURN value of the solid modelling commands into
    their help strings of parameter help.


<a id="orgcdcc643"></a>

## Ansys process and interpreter buffer

example: gnuplot-mode

-   call to start the Ansys solution results tracker for .nlh (xml
    see <file.nlh>, contact forces) and convergence .gst (binary?,
    coded? <file.gst>)
-   implement something like <nlhist.sh> for .cnd files (xml see
    <file.cnd>) or use the new libxml parser ;-)
-   make display-buffer "**Ansys**" optional when sending commands to
    the Ansys process
-   use Ansys **env variables** like AWP<sub>ROOT140</sub> for checking
    installation directories
-   check also the license status for hpc licenses if
    -no-of-processors is greater then 3
-   implement ANSWAIT variable
-   autoloading of: -license-file, -license-program functions
-   dbus support of workbench or integrating emacs in workbench?
-   insert skeleton with C-c C-s i, send line C-c C-c ccc
-   warn when C-c C-c tries to send a block command (complete to full
    block?)
-   take care when region isn't complete in -send-to-ansys (send whole line)
-   C-c C-c skips empty lines and comments this is not always desirable
    -> make this optional -> filter process input?
-   C-c C-q, C-c C-u and C-c C-c sequences are not shown (and stored)
    in the comint buffer
-   make filter of -license-status optional
-   optimise -start-run with query of y (start immediately),n
    (exit),e (change params),C-h (help)
-   Splice any input line behind the BEGIN: symbol in the **Ansys**
    buffer
-   Enable one solver run for every Ansys macro buffer
-   indicate with activation/inactivation of menu items that an
    asynchronous job is already running or not.
-   show/mark sent lines in apdl file C-c C-u, C-c C-c,
    -   with fringes
    -   with background
    -   maximum line
    -   reset highlighting, when?
-   provide Ansys \`y' request and carriage return? superflouous?  a
    single \`y' does the trick
-   Killing a buffer with a running process now asks a confirmation.
    You can remove this query in two ways: either removing
    \`process-kill-buffer-query-function' from
    \`kill-buffer-query-functions' or setting the appropriate process
    flag with \`set-process-query-on-exit-flag'.
    -   finding an Ansys /filnam command in current macro file and
        suggesting this as current job-name optionally kill old job when
        called again with working run warn and optionally remove the
        ansys lock file before starting a run
    -   search in -job for /filn arguments as default job name like in
        -display-error-file


<a id="org443c8f2"></a>

## Skeletons, outline and abbrevs

-   show a preview buffer with the outline headlines
-   -skeleton-select is a bit thin, not yet finished?
-   add a preview mode, with its own keymap for faster editing and
    copying, like dired
-   enhance abbrev definitions for \*create, \*dowhile logics:
-   use the Ansys sample input listing for template-examples
-   additional dialog boxes with:
    /ui,anno,ksel&#x2026;,wpse,help,query,copy
-   make outline string in skeletons configurable
-   spider beams, \_bolt.mac, screw.mac, Mohr-Coulomb criterion
-   rework concept with respect to the Ansys menu structure sort
    skeletons in menu. Concept: 1.) Ansys Workflow 2.) specialised
    macro library
-   Make skeleton-header properly working (info "(autotype)")
-   Optimise templates: completing-read, read-from-minibuffer
-   abbrev \`d does not indent properly in another block level
-   Implement choice when completing \*IF commands (\*ELSEIF or \*ENDIF
    ?THEN?).
-   Warn when including skeleton in read only file.
-   skeleton for numbering ansys-skeleton-numbering-controls
    (skeleton-insert docu)
-   suggestions with auto-completion of
    kp,line,area,volu,node,elem,tabn,sval
-   negation, what negation? TODO:
-   skeleton for skipping code \*if,then \*else\*endif if selection:
    wrap around and indenting \*go/\*if label is not fontified at the
    command line (restriction of 7 characters with out the colon.)
    must \*go:label be unambiguous?


<a id="org878706c"></a>

## Miscellaneous

-   hash or signature file for packages
-   show content of matlib/ folder
-   add to -display-variables a prefix argument for showing the value
    of the variable around point, or center the variables window&#x2026;
-   customisation option for a the web browser of -browse-ansys-help
-   ideas for preview also of a/the macro directory?
-   put graphics dlls in windows package
-   LSDYN support, see lsdyna.el
-   **embedded calc:** include in hacking.mac
-   **Alignment:** alignment "section" is not clearly defined in function
    -align
-   **alignment:** extend to vector definitions
    t(1,0) = 20,300,3094,
    t(1,1) =  3,  4,   9,
-   Utility for clearing files like .rst, &#x2026; or using dired, clear<sub>files.sh</sub>?
    with listing listing of file types
-   Configure the highlighting colours with black on white background
-   C-j, M-j are not skipping to \`,' whith default command (from second
    line onwards)!
-   supply command for clearing recursively not important process files:
    .rst, .log, &#x2026;, supply a customisaton variable
-   narrow, outline, transparent-font: eblocks, nblocks by default?
-   add notes for parameter help of undocumented commands (alist)
-   create filter variable for hiding not installed license types
    ("aiiges" "aihexa") or better regexp filter "ai.\*\\\\|acf.\*"
-   commands shorter than 4 chars are not allowed with additional chars
    adjust this to -highlighting-level 0
-   Make Ansys-Mode also working solely with ansys-mode.el??
-   make M-C-h more intelligent like M-h (if mark&#x2026;)
-   speed concern: replace -find-duplicate-p with function argument to
    add-to-list
-   For non-comint-mode usage: display .log file with tail mode
-   is outline-mode enabled? no! make default?, outline
-   take care of setup procedure: v110/ansys/apdl/start110.ans
    /MPLIB,READ,/ansys<sub>inc</sub>/v110/ansys/matlib
-   removing/renaming/checking abort file?
-   Are characters behind unique commands shorter than 4 characters
    possible? No /sol(u) sufficient? Yes condensed input line ($)
    behind commands without arguments possible? Yes have a look in
    -variable-defining-commands!  Warn when - unintentionally -
    arguments are used for Ansys commands which do not allow
    arguments.  Or implement some auto-newline feature?  But
    problematic in condensed command lines or when applying code
    comments!
-   undocumented ask parameter
-   mode-line-format nil);no mode line for this buffer Unambiguous
-   Emacs: old-style backquotes?  used in the old template macro system
-   completion of function names: cursor should end within parentheses
-   better hints for auto-insertion features
-   up/down-block when we are over a begin-keyword and in a block-end
    line
-   component names are fontified as variables! separate them cmlist?
-   format string for \*VWRITE: line(s) below, in parens, when FORTRAN
    format specifiers are used, keyword SEQU povides row numbers, up
    to 19 parameters are allowed
-   remove vestiges of ansys-mod.el for making ansys-mode.el GPL
    proof.  Check whether octave-mod.el really is GPL compliant, use
    -ctave-mod.el from 1997, kill octave-mod.el afterwards in
    makefile read every symbol docu string ->NEW<sub>C</sub> or \_C or OCTAVE<sub>C</sub>
-   replace/extend column-ruler with ruler-mode or ruler implemented as
    overlay in buffer
-   make everything completely customisable, eg auto-insert stuff
    customisable enable, Emacs customisation of auto-insert-query
-   Fontify **completion list** distinguishing elements: commands,
    functions and keywords.
-   provide a list of options for the -license function, set this
    function in the defcustom lmstat -a etc.
-   auto-indent-switch as defcustom?
-   inhibit the unnecessary blink-matching-block display when closing a
    block behind a block-end keyword
-   highlight matching block keywords (similar to show-paren-mode) when
    point is at keyword
-   Implement highlighting of bracket pairs with the correct level in
    Ansys GET- and parametric- functions.
-   highlighting of plot commands inside the /GCMD command
-   DEFSUBSTs with DEFUNs inside aren't particularly helpful?
-   Emphasise better implied (colon) loops n,(1:6),(2:12:2) => n,1,2
    $ n,2,4 $&#x2026; (little used, I know, but any ideas going beyond the
    colon?).
-   startup screen for Ansys mode: Mode help, Ansys version,
    supressing the startup screen 'ansys-mode-startup-message maybe
    as advice when sluggish -> compiliation
-   Enable choice for /show,3d or x11 (-start-graphics)
-   Provide a way to send commands to the tcl-tk Ansys gui (x11
    programming).


<a id="org97472d9"></a>

## Ansys syntax restrictions not (yet) accounted for

-   Parentheses can only be nested 4 levels deep and only up to 9
    operations (+,-,\*,&#x2026;) within these set of parentheses
-   PATH name is restricted to 8 chars
-   \*SET parameter strings may only be 32 character long!
-   Character parameters are restricted to only 8 characters.
-   \*MSG command can only have 9 additional continuation lines
-   Code line restriction of 640 characters
-   Block level restriction of 20 levels of nested \*DO loops (except
    with /INPUT and \*USE)
-   Block level restriction of 10 levels of nested \*IF blocks
-   Macro level restriction: 20 macros


<a id="org8636548"></a>

## Unknown Ansys stuff

-   what the heck is the \*UILIST command?
-   Is hyper56 a valid element?

---

