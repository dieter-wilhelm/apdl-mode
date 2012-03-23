;;; ansys-template.el -- APDL code templates for the Ansys mode

;; Copyright (C) 2006 - 20011  H. Dieter Wilhelm GPL V3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This lisp script is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;
;; Permission is granted to distribute copies of this lisp script
;; provided the copyright notice and this permission are preserved in
;; all copies.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- variables ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar ansys-last-skeleton nil
  "Variable containing the last previewed skeleton")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- functions ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ansys-display-skeleton (&optional arg)	;NEW
  "Display code templates in another buffer.
With an argument ARG insert the template into the current buffer
instead of previewing it in a separate window."
  (interactive "P")
  (let* (
	 (old-buffer (buffer-name))
	 (new-buffer-name "*Ansys-skeleton*")
	 (skeleton-buffer
	  (get-buffer-create new-buffer-name))
	 s
	 ;; if skeleton window is visible in selected frame
	 (visible  (get-buffer-window new-buffer-name nil))
	 (skel-string
	  (if (and arg ansys-last-skeleton visible)
	      ansys-last-skeleton
	    "ansys-skeleton-"))
	 (skel (completing-read
		       "Preview template: " obarray 'commandp t skel-string nil))
	 )
    (setq ansys-last-skeleton skel)
    (cond (arg
	   (funcall (intern-soft skel)))
	  (t
	   (switch-to-buffer-other-window skeleton-buffer)
	   (remove-overlays)
	   ;;(make-local-variable 'ansys-skeleton-overlay)
	   (setq ansys-skeleton-overlay (make-overlay 1 1))
	   (kill-region (point-min) (point-max))
	   (funcall (intern-soft skel))
	   ;;    (ansys-skeleton-numbering-controls)
	   ;;    (insert "bla\n")
	   (goto-char (point-min))
	   (unless  (eq major-mode 'ansys-mode)
	     (ansys-mode))
	   (setq s (propertize
		    (concat "-*- Ansys template: "
			    skel " -*-\n") 'face 'match))
	   (overlay-put ansys-skeleton-overlay 'before-string s)
	   (set-buffer-modified-p nil)
	   ;;    (toggle-read-only t)
	   (switch-to-buffer-other-window old-buffer)
	   ;; (display-buffer new-buffer-name 'other-window)
	   ))))

(define-skeleton ansys_do		;NEW
  "Insert a *do .. *enddo loop."
  nil
  "*do,I,1,10,1" > \n
  - \n
  "*cycle !bypass below commands in *do loop" > \n
  "*enddo" > \n
  )

(define-skeleton ansys_if		;NEW
  "Insert an *if .. *endif construct."
  nil
  "*if,I,eq,J,then" > \n
  > _ \n
  "!! *elseif,K,gt,L" > \n
  "!! *else" > \n
  "*endif" >
  )

(define-skeleton ansys-skeleton-looping
  "Control constructs"
  nil
  "\n!@@@ - branching, looping and control structures -"\n
  \n
  "! if controls" \n
  "*if,I,eq,1,then" \n
  "*elseif,I,le,10" > \n
  "*else" > \n
  "*endif" > \n
  \n
  "! *if,val1,oper1,val2,base1,val3,oper2,val4,base2" \n
  " ! oper: eq,ne,lt,gt,le,ge,ablt,abgt," \n
  " ! base: stop,exit,cycle,and,or,xor" \n
  \n
  "! implicit looping" \n
  "lfillt,(1:2),(3:4),5" \n
  \n
  "! command repetition" \n
  "e,1,2" \n
  "*repeat,5,0,1"\n
  \n
  "! do loops" \n
  "*do,I,1,6,2" \n
  "*cycle" > \n
  "*exit" > \n
  "*enddo" > \n
  \n
  "*dowhile,PAR" \n
  "*cycle" > \n
  "*exit" > \n
  "*enddo" > \n
  \n
  "! goto branching" \n
  "*go,:BRANCH" \n
  ":BRANCH" \n
  )

(define-skeleton ansys-skeleton-header	 ;NEW
  "Insert a file header for an APDL script.
Together with an Emacs Time-stamp string.  You might update the
time stamp with the Emacs command M-x `time-stamp'."
  "Brief description of the file: "
  "!" ansys-outline-string " --- file header ---" \n
  "!! ------------------------------" \n
  ;; "!! FILENAME: " (file-name-nondirectory (if (buffer-file-name)
  ;; 					      (buffer-file-name)
  ;; 					    (buffer-name))) \n
  "!! Time-stamp: <" (current-time-string) ">"\n
  "!! ANSYS VERSION: " ansys-current-ansys-version \n
  "!! UNITS: mm-t-s" \n
  "!! NOTE: " str \n
  "!! ------------------------------" \n
  "! fini" \n
  "! /clear" \n
  "! y" \n
  "/units,mpa !indicate mm-t-s unit system" \n
  \n
  )

(define-skeleton ansys-skeleton-information
  ""
  nil
  "\n!! ------------------------------" \n
  "!@@ -- informations --" \n
  "/inquire,Job_name,jobname!get string array jobname|directory|user|psearch" \n
  "*stat,Job_name(1,1,1)" \n
  "/inquire,param,date,file,ext !get date(size,lines) of file.ext"\n
  "!@@@ - stati -" \n
  "/status ![all], title,units,mem,db,config,global,solu,prod" \n
  "*list,file,ext ! list file content" \n
  "/runst ! enter run statistics processor" \n
  "/pstatus ! display window stats specifications" \n
  "list: k-,l-,a-,v-,n-,e-,ce-,cp-,mp-,f-,bf-,d-,da-,dk-,dl-,fk-,af-,sf-,sfl-,bfa-,bfe-,bfk-,bfl-,bfv-,ic-,r-,tb-,s-,m-,sw-" \n
  \n
  "*status ! parameters, arrays and abbreviations" \n
  "*status,_STATUS !return value: 0:no error, 1:note, 2:warning, 3:error" \n
  "*stat,argx !list all local ARGx values" \n
  "*status,_RETURN !some solid modelling commands return this parameter" \n
  "(see the _return value list below)"\n
  "*vstat !status on arry operations"\n
  \n
  "!@@@ - material info" \n
  "mplist,all" \n
  "tblist" \n
  "tbplot,biso,1" \n
  "!@@@ - solution info -" \n
  \n
  "set,list ! list a summary of each load step" \n
  "wpstyl,stat !working plane status" \n
  "status,solu" \n
  \n
  "!@@@ - \"stat\" database settings - "\n
  "/prep7" \n
  "etype" \n
  "stat" \n
  "rcon$stat"\n
  "!! prep7 stat topics" \n
  "ETYPE $ stat ! - Element types" \n
  "RCON $ stat ! - Real constants" \n
  "MATER $ stat ! - Material properties" \n
  "TBLE $ stat ! - Data table properties" \n
  "PRIM $ stat ! - Solid model primitives" \n
  "KEYPTS $ stat ! - Keypoints" \n
  "LINE $ stat ! - Lines" \n
  "AREAS $ stat ! - Areas" \n
  "VOLUMES $ stat ! - Volumes" \n
  "GEOMETRY $ stat ! - Solid model information" \n
  "MESHING $ stat ! - Meshing" \n
  "BOOL $ stat ! - Booleans" \n
  "NODES $ stat ! - Nodes" \n
  "ELEM $ stat ! - Elements" \n
  "SELM $ stat ! - Superelements" \n
;; Pipe is not supported any longer in v130  "PIPE $ stat ! - Pipe modeling" \n
  "DIGIT $ stat ! - Node digitizing" \n
  "COUPLE $ stat ! - Node coupling" \n
  "CEQN $ stat ! - Constraint equations" \n
  "REORDER $ stat ! - Model reordering" \n
  \n
  "!! solution stat topics" \n
  "ATYPE $ stat ! - Analysis types" \n
  "MASTER $ stat ! - Master DOF" \n
  "GAP $ stat ! - Reduced transient gap conditions" \n
  "DEACT $ stat ! - Element birth and death (deactivation)" \n
  "LSOPER $ stat ! - Load step operations" \n
  "FECONS $ stat ! - Constraints on nodes" \n
  "FEFOR $ stat ! - Forces on nodes" \n
  "FESURF $ stat ! - Surface loads on elements" \n
  "FEBODY $ stat ! - Body loads on elements" \n
  "SMCONS $ stat ! - Constraints on the solid model" \n
  "SMFOR $ stat ! - Forces on the solid model" \n
  "SMSURF $ stat ! - Surface loads on the solid model" \n
  "SMBODY $ stat ! - Body loads on the solid model" \n
  "INRTIA $ stat ! - Inertial loads" \n
  "GENOPT $ stat ! - General options" \n
  "DYNOPT $ stat ! - Dynamic analysis options" \n
  "NLOPT $ stat ! - Nonlinear analysis options" \n
  "OUTOPT $ stat ! - Output options" \n
  "BIOOPT $ stat ! - Biot-Savart options" \n
  "SPTOPT $ stat ! - Spectrum analysis options" \n
  "SOLUOPT $ stat ! - Solution options" \n
  "FLOTRAN $ stat ! - FLOTRAN data settings" \n
  \n
  "!! post1 stat topics" \n
  "DEFINE $ stat ! - Data definition settings" \n
  "SORT $ stat ! - Sort settings" \n
  "PRINT $ stat ! - Print settings" \n
  "DISPLAY $ stat ! - Display settings" \n
  "CALC $ stat ! - Calculation settings" \n
  "PATH $ stat ! - Path data settings" \n
  "LCCALC $ stat ! - Load case settings" \n
  "DATADEF $ stat ! - Directly defined data status" \n
  "FATIGUE $ stat ! - Fatigue data status" \n
  "POINT $ stat ! - Point flow tracing settings" \n
  "SPEC $ stat ! - Miscellaneous specifications" \n
  \n
  "!! post26 stat topics" \n
  "DEFINE $ stat ! - Data definition settings" \n
  "OPERATE $ stat ! - Operation data" \n
  "PRINT $ stat ! - Print settings" \n
  "PLOTTING $ stat ! - Plotting settings" \n
  \n
  "!@@@ - aux3 result file edit routine -" \n
  "/aux3" \n
  "list !result statistics" \n
  "!@@@ - *get -" \n
  "*get,bla,active,,mat![|csys|type|real|esys]" \n
  \n
  "*status,_RETURN ! command, documenation, _return value" \n
  "!Keypoints -" \n
  "K ! Defines a keypoint - keypoint number"\n
  "KL ! Keypoint on a line - Keypoint number"\n
  "KNODE ! Keypoint at node - Keypoint number"\n
  "KBETW ! Keypoint between two keypoints - KP number"\n
  "KCENTER ! Keypoint at center - KP number"\n
  "!Lines -" \n
  "BSPLIN ! Generate spline - Line number"\n
  "CIRCLE ! Generate circular arc lines - First line number"\n
  "L ! Line between two keypoints - Line number"\n
  "L2ANG ! Line at angle with two lines - Line number"\n
  "LANG ! Line tangent to two lines - Line number"\n
  "LARC ! Defines a circular arc - Line number"\n
  "LAREA ! Line between two keypoints - Line number"\n
  "LCOMB ! Combine two lines into one - Line number"\n
  "LDIV ! Divide line into two or more lines - First keypoint number"\n
  "LDRAG ! Line by keypoint sweep - First line number"\n
  "LFILLT ! Fillet line between two liens - Fillet line number"\n
  "LROTAT ! Arc by keypoint rotation - First line number"\n
  "LSTR ! Straight line - Line number"\n
  "LTAN ! Line at end and tangent - Line number"\n
  "SPLINE ! Segmented spline - First line number"\n
  "!Areas -" \n
  "A ! Area connecting keypoints - Area number"\n
  "ACCAT ! Concatenate two or more areas - Area number"\n
  "ADRAG ! Drag lines along path - First area number"\n
  "AFILLT ! Fillet at intersection of two areas - Fillet area number"\n
  "AL ! Area bounded by lines - Area number"\n
  "ALPFILL ! All loops - Area number"\n
  "AOFFST ! Area offset from given area - Area number"\n
  "AROTAT ! Rotate lines around axis - First area number"\n
  "ASKIN ! Skin surface through guiding lines - First area number"\n
  "ASUB ! Area using shape of existing area - Area number"\n
  "!Volumes - "\n
  "V ! Volume through keypoints - Volume number"\n
  "VA ! Volume bounded through areas - Volume number"\n
  "VDRAG ! Drag area pattern to create volume - First volume number"\n
  "VEXT ! Volume by extruding areas - First volume number"\n
  "VOFFST ! Volume offset from given area - Volume number"\n
  "VROTAT ! Volume by rotating areas - First volume number"\n
)

(define-skeleton ansys-skeleton-configuration
  ""
  nil
  "\n!! ------------------------------" \n
  "!@@ -- configurations --" \n
  \n
  "! *afun,deg ! trig. functions accept angle arguments" \n
  "*afun,rad !rad: default" \n
  "True = 1"  \n
  "False = 0" \n
  \n
  "/title," _ \n
  "/plopts,wp ! switch off working plane" \n
  "/plopts,wp,1 !display working plane" \n
  "/plopts,minm,0 !0: switch off min max" \n
  "/triad,rbot !off, orig, ltop, ..." \n
  "/cwd,DIR !changes working directory" \n
  \n
  "/cwd !changes working dir" \n
  "/filname !changes jobname" \n
  "resume! resume the database" \n
  "file !result file" \n
  )

(define-skeleton ansys-skeleton-view-settings
  ""
  nil
  "\n!! ------------------------------" \n
  "!@@ -- view settings --" \n
  \n
  "/view !viewing direction"_ \n
  "/angle,1,10,xs,1!rotation {x,y,z}m global {x,y,z}s screen 1:cumulative 0: absolut" \n
  "/dist,1,1/2.,1 $ /repl !distance (zoom) to object " \n
  "/focus,1 $ /repl !focus to csys,0" \n
  "/plopts,wp,1 !display working plane" \n
  "/plopts,minm ! switch off min max" \n
  "!/focus,1,,.5,,1 $ /repl !focus with screen coordinate multiplier" \n
  "/auto ! automatic fit mode" \n
  "/user ! keep last display scaling"\n
  "/pstatus ! display window stats specifications" \n
  "/dscale,all,10 !set displacment multiplier" \n
  )

(define-skeleton ansys-skeleton-import	;NEW
  "Import commands."
  nil
  "\n!! ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string " -- cad import -- " \n
  \n
  "/aux15" \n
  "ioptn,iges,nodefeat" \n
  "ioptn,merge,yes" \n
  "ioptn,solid,yes" \n
  "ioptn,small,yes" \n
  "ioptn,gtoler,defa" \n
  "igesin,'test','iges'"\n
  \n
  "/input,filename,anf ! for APDL based input" \n
  "/facet,norm" \n
  \n
  )

(define-skeleton ansys-skeleton-expand	;NEW
  "Symmetry expansion."
  nil
  "\n!@@@ - symmetry expansion -" \n
  \n
  "!/EXPAND, Nrepeat1, Type1, Method1, DX1, DY1, DZ1, Nrepeat2, Type2, Method2, DX2, DY2, DZ2, Nrepeat3, Type3, Method3, DX3, DY3, DZ3"\n
  "!DX1,DY1,DZ1,... 1.) normal vector of reflection plane 2.) increments between patterns"\n
  "! full: no tranlation<-small nonzero value, half: mirroring, increment is doubled" \n
  "/expand,2,(l)rect,half,,-1e-6,,2,rect,half,-1e-6 !(local) cartesian, half:mirror" \n
  "/expand,8,(l)polar,half,,45 !(local) polar expansion, full:normal exp." \n
  "/expand,18,axis,,,10 !axisymmetric (360 ° rot. around y-axis)" \n
  "/expand !switch off expansion" \n
  "!! -- cyclic expansion --" \n
  "cyclic,status" \n
  "/cycexpand ! expand graphics rep." \n
  \n
  )

(define-skeleton ansys-skeleton-contact-definition
  ""
  nil
  "\n!! ------------------------------" \n
  "!@@ -- contact pair defintion --" \n
  \n
  "Contact="_ \n
  "Target=Contact+1" \n
  "r,Contact !define a real set" \n
  "et,Contact,conta174    !3d, 8 node" \n
  "et,Contact,conta173 !3d, 4 node" \n
  "et,Contact,conta172 !2d, 3 node" \n
  "et,Contact,conta171 !2d, 2 node" \n
  "et,Contact,conta175 !2/3d node to surf" \n
  "et,Contact,conta176 !3d line to line, 3 node" \n
  "et,Contact,conta177 !3d line to surf, 3 node" \n
  \n
  "et,Target,targe170 !3d area,line,(pilot-)node" \n
  "et,Target,targe169  !2d" \n
  \n
  "!! --- contact options --"\n
  "keyo,Contact,2,1 !ALGORITHM [0]:augm. Lagrange,1:penalty,2:MPC,4:pure Lagrange" \n
  "!! " \n
  "Fkn = .1 !contact stiffness (default 1, divided by 100 if plastic mat. ONLY Ansys version < 12.0!)" \n
  "rmod,Contact,3,Fkn !FKN:normal penalty stiffness factor (default:1) smaller: bigger penetration, easier convergence" \n
  "rmod,Contact,12,0. !FKT:tangent stiffness factor,0:means 1 for Ansys!!!" \n
  \n
  "Ftoln = .1 !penetration tolerance [.1] for lagr. mult. & chattering control" \n
  "rmod,Contact,4,Ftoln !FTOLN penetration tolerance (augm. Lagrance! default:0.1) bigger: less chattering" \n
  \n
  "Pinb = -1 !search radius, neg: absolut value ( > CNOF!)" \n
  "rmod,Contact,6,Pinb !PINB:pinball radius (negative: no scaling:absolute distance)" \n
  \n
  "ICONT = -0.05 !initial contact closure [0] band size (neg. absolut)" \n
  "rmod,Contact,5,Icont !ICONT:amount of initial contact closure (positiv:penetration)" \n
  \n
  "CNOF = 0 !contact surface offset ([0], neg.: penetr.)" \n
  "rmod,Contact,10,Cnof !CNOF (thickness effects):contact normal offset (e.g. beams)" \n
  \n
  "keyo,Contact,4,3 !keyo(4): location of contact detection" \n
  "!! 0:Gauss points, 3(V13):surface projection method" \n
  "keyo,Contact,5,4 !EFFEKT of CNOF (surface offset) or ICONT (node movement in a band)" \n
  "    !! 0: no adjustm." \n
  "    !! 1: close gap with auto CNOF" \n
  "    !! 2: reduce penetr. w. auto CNOF" \n
  "    !! 3: close gap/red. pene. w. auto CNOF" \n
  "    !! 4: auto ICONT" \n
  "keyo,Contact,9,4 !HANDLING of initial penetration/gap and CNOF" \n
  "    !! 0: include everything" \n
  "    !! 1: remove everything" \n
  "    !! 2: include everyth. ramped" \n
  "    !! 3: include offset only" \n
  "    !! 4: incl. offset only, ramped" \n
  "keyo,Contact,10,2 !Stiffness UPDATE,[0]:each LS,2:each NR iteration,1:each substep" \n
  "keyo,Contact,11,1 !SHELL thickness effect" \n
  "keyo,Contact,12,0 !BEHAVIOUR,[0]:frictional/-less,1:rough,2:no separation,3:bonded" \n
  "real,Contact" \n
  \n
  "rmod,Contact,11,-1 !FKOP contact opening stiffness & contact damping, must be neg." \n
  \n
  "mp,mu,Contact,Mu !friction factor" \n
  "mat,Contact" \n
  \n
  "!@@ -- contact generation --" \n
  \n
  "type,Contact" \n
  "real,Contact" \n
  "esurf !,,top ![default] beam element's top direction" \n
  "esurf !,,bottom ! for beam elements top direction" \n
  "esurf !,,reverse ! reverse dir. on existing elem." \n
  "!! -- Target generation --" \n
  "type,Target" \n
  \n
  "!! check of contact normals" \n
  "esel,s,type,,Contact" \n
  "esel,a,type,,Target" \n
  "/psymb,esys,1" \n
  "eplot" \n
  \n
  "enorm ! change the underlying elem." \n
  \n
  "!@@@ - check contact status -" \n
  \n
  "cncheck !list contact pari properties" \n
  "cncheck,summary !list only open/closed status" \n
  "cncheck,adjust !adjust physically the elements!" \n
  "/solu" \n
  "cncheck,post !write contact config to jobname.rcn" \n
  \n
  "/post1" \n
  "/inquire,Job_name,jobname!get string array jobname|directory|user|psearch" \n
  "/inquire,param,date,file,ext !get date(size,lines) of file.ext"\n
  "save"\n
  "file,Job_name(1),rcn ! set result file to file.rcn" \n
  "set,first" \n
  "plnsol,cont,gap,0,1" \n
  "esel,s,type,,Contact" \n
  "etable,Stat,cont,stat	 !3-closed sticking" \n
  "			 !2-closed sliding" \n
  "			 !1-open near" \n
  "			 !0-open far" \n
  "!/efacet,2" \n
  "plls,stat,stat" \n
  "etable,Pene,cont,pene !pres|sfric|stot|slide|gap|flux|cnos|fprs" \n
  "plls,Pene,Pene !line element results" \n
  "resume,Job_name,db" \n
  \n
  )

(define-skeleton ansys-skeleton-rigid-target ;NEW
  ""
  nil
  "\n!! ------------------------------" \n
  "!@@ -- rigid target creation -- " \n
  \n
  "Contact="_ \n
  "Target=Contact+1" \n
  "real,Contact" \n
  "type,Target" \n
  "!!tshap,arc !clockwise arc" \n
  "!!tshap,cone" \n
  "!!tshap,quad" \n
  "!!tshap,sphere" \n
  "!!tshap,qua8" \n
  "tshap,line" \n
  "*get,Nmax,node,,num,max" \n
  "n,Nmax+1,1,1,0" \n
  " ,Nmax+2,1,2,0" \n
  "e,Nmax+1,Nmax+2" \n
  "tshap,pilo" \n
  "e,Nmax+1" \n
  \n
  )

(define-skeleton ansys-skeleton-coordinates
  ""
  nil
  "\n!@@@ - coordinate system display -" \n
  \n
  "!csys ![0]:cartesian, 1:cylindrical, 2:spherical, 3:toroidal, 4:wp" \n
  "!clocal,11,0 !define local coord. sys. from active" \n
  "!psymb,cs,1 ! display local coord."
  "/plopts,wp,1 !display working plane" \n
  "/plopts,wp,off !switch off wp" \n
  "/plopts,frame,off !switch off graphics frame" \n
  "/plopts,logo,off !switch off Ansys logo" \n
  "/triad,rbot"_ \n
  "/triad,off"
  \n
  )

(define-skeleton ansys-skeleton-working-plane
  "Settings for the working plane and related stuff."
  nil
  "\n!@@@ - working plane setup -" \n
  \n
  "/plopts,wp,1 !display working plane" \n
  "/repl" \n
  "wpcsys,1,0 !align wp in WIN with specified c-sys" \n
  "wpoffs,,-100 !x,y,z offset" \n
  "wprota,0,90,0 !z,x,y axis of rotation!" \n
  "/plopts,wp,off !switch off wp" \n
  "/triad,off !off: switch off co-ordinate triad, rbot, ltop, ..." \n
  "/plopts,frame,off !switch off graphics frame" \n
  "/plopts,logo,off !switch off Ansys logo" \n
  "wpstyl,,,,,,1 !type spec 0,1,2 cartesian,cylindrical,spherical" \n
  "wpstyl,,,,,,,0 !grid spec: 0 grid+triad,1 grid,2 [triad]" \n
  "!wpstyl,stat" \n
  "csys,wp ! or csys,4: change csys to wp" \n
  \n
  )

;; PlotCtrls ->Multi-plot-Ctrls???
(define-skeleton ansys-skeleton-multi-plot
  ""
  nil
  "\n!@@@ - multiplot controls -" \n
  \n
  "/gcmd,1,u,sum" \n
  "/gtype,all,node,0 !turn off nodes (elem,keyp,line,area)" \n
  "/gtype,,volu,1 !turn on volumens" \n
  "gplot" \n
  \n
  )

;; PlotCtrls ->Numbering Controls
(define-skeleton ansys-skeleton-numbering-controls
  ""
  nil
  "\n!@@@ - numbering controls -" \n
  \n
  "/pnum,kp,1 !line;area;volu;node;elem;mat;type;tabn;sval,on" \n
  "/number,1 ![0]: colour & number, 1:colour only, 2 number only" \n
  "/replot"\n
  \n
  )

;; PlotCtrls -> Symbols
(define-skeleton ansys-skeleton-symbols
  ""
  nil
  "\n!@@@ - symbol display -" \n
  \n
  "/pbc,all,,1 !bc symbols"\n
  "/psf,pres,,2 !2 arrows, surface loads" \n
  "/pbf !body loads" \n
  "/pice !element initial condition symbols" \n
  "/psymb,esys,1 ![0],1: display of element co-ordinate sys." \n
  "/psymb,ndir,1 !only for rotated nodal co-ordinate systems!" \n
  "/psymb,stat" \n
  "/repl" \n
  \n
  )

(define-skeleton ansys-skeleton-element-table
  ""
  nil
  "\n!! ------------------------------" \n
  "!@@ -- etables --" \n
  \n
  "!! etables don't take into account higher element order!" \n
  "!! they are averaged over the element" \n
  "!! ---- Mohr-Coulomb failure criterion" \n
  "Z1 = 60 !tensile strength" \n
  "Z3 = 160 !compressive strength" \n
  "etable,S1,s,1" \n
  "etable,S3,s,3" \n
  "sadd,R,S1,S3,1/Z1,-1/Z3" \n
  "sexp,X,S1,,-1 !warning: sexp uses modulus of S!!!!!" \n
  "!! constant element values are transfered to the nodes and optionally averaged" \n
  "pletab,R,avg !avg: average over nodes of neigbouring elements" \n
  "esel,s,type,,2" \n
  "etable,Pene,cont,pene" \n
  "!etable,chat,cont,cnos !chattering levels" \n
  "!etable,cpre,cont,pres"\n
  "!plls,Pene,Pene !line elem. results" \n
  "esort,etab,R" \n
  "etable,refl !refill all element tables for latest load set" \n
  "*get,Mc,etab,sort,,max" \n
  "*msg,,Mc" \n
  "Mohr-Coulomb criterion (< 1): %G" \n
  \n
  )

(define-skeleton ansys-skeleton-element-def
 ""
 nil
 "\n!! ------------------------------" \n
 "!@@ -- element definition --" \n
 \n
 "Steel = 1" \n
 "ID = Steel" \n
 "real = Steel" \n
 "et,ID,solid186 !3d, 20 node" \n
 "etlist !list defined elements" \n
 "et,ID,solid185 !3d, 8 node" \n
 "et,ID,plane183,,,3 !2d, 8 node (3)0:plane stress, 1:axissymmetric, 2:plane strain, 3:plane stress with thickness real constant" \n
 "r,ID,13 ! thickness" \n
 "et,ID,plane182 !2d, 4 node"\n
 "keyopt,ID,3,1 !(3)=0:plane stress,1:axissym,2:plain strain." \n
 "keyopt,ID,1,0 !(1)=0:reduced integr.2:enhanced strain for bending" \n
 "!!for most elements the radial direction is the x-axis" \n
 \n
 "!! --- shells and planes ---" \n
 "et,ID,shell181 !4 node structural shell" \n
 "et,ID,plane182 !2d 4 node structural solid" \n
 "et,ID,plane183 !2d 6 node structural solid" \n
 \n
 "!! --- thermal ---" \n
 "et,ID,solid90 !3D 20 nodes thermal solid" \n
 \n
 "!! --- magnetics ---" \n
 "et,ID,plane13 !2d, legacy coupled-field ->plane53" \n
 "keyopt,ID,3,1 !(3)=1:axissym." \n
 "keyopt,ID,5,2 !(5)=2:nodal magnetic field printout" \n
 "et,ID,infin110 !2d semi infinit electromagnetic elem." \n
 "!only 1 element layer, sized in the order of the problem domain" \n
 "keyopt,ID,3,1 !(3)=1:axissym." \n
 "keyopt,ID,2,1 !(2)=0:4-node,1:8-n" \n
 \n
 "!! --- assign attributes ---" \n
 "aatt,MAT,REAL,TYPE ! associate prop. with selected areas" \n
 \n
 "!! /pnum,type,1 $ eplot ! display materials" \n
 \n
)

(define-skeleton ansys-skeleton-meshing
  ""
  nil
  "\n!! ------------------------------" \n
  "!@@ -- meshing --" \n
  \n
  "!! mat,Steel" \n
  "mshkey,1 !1: mapped meshing,2: mapped if possible" \n
  "mshape,0,3d !0: quads/hex 1:tri/tets, dimensions: 2d/3d" \n
  "esize,1 ! element edge length" \n
  "aesize,ANUM,SIZE ! element SIZE on area ANUM (or ALL)" \n
  "lesize,all,,,3 ! SPACE neg: center to end division" \n
  \n
  "!! max and min element sizes for a given 'lesize' spacing ratio" \n
  "Length = 10		 !line segment length" \n
  "Ratio = 1/500.0		 !spacing ratio" \n
  "N = 10			 !number of line divisions" \n
  "!! lesize uses a geometric series" \n
  "Res = Length*(1-Ratio**(1/(N-1.0)))/(1-Ratio**(N/(N-1.0)))" \n
  "/go" \n
  "/com,first line segment length: %Res%" \n
  "/nopr" \n
  "Res = Res*Ratio" \n
  "/go" \n
  "/com,last line segment length: %Res%" \n
  \n
  "lcomb,all !combine adjacent lines" \n
  "ldiv,all,1 !divide a single line" \n
  "lccat,all !concatenate lines for meshing" \n
  \n
  "esys,12 !set element coordinates for a and v elements to 12" \n
  "shpp,off,,nowarn! control mesh shape checking" \n
  "vmesh,all" \n
  "amesh,all" \n
  "shrink,.8 !shrink elements,l,a,v"\n
  "lsel,s,lcca !select all concatenated lines" \n
  "ldele,all" \n
  "!! -- cyclic symmetric meshing --" \n
  "cyclic ! check sectors in case of cyclic sym." \n
  "*status ! look for CYCLIC_XREF" \n
  "cyclic,status" \n
  "/cycexpand ! expand graphics rep." \n
  \n
  "/pnum,mat,1 $ eplot" \n
  \n
  )

(define-skeleton ansys-skeleton-function
  "Standard FORTRAN functions"
  nil
  "A = ABS()"  \n
  "A = SIGN()  !sign(x,y) absolute value of x with sign of y)" \n
  "A = EXP()   !exp(x): e^x" \n
  "A = x**y    !exponentiation x**y: x^y" \n
  "A = LOG()"  \n
  "A = LOG10()"\n
  "A = SQRT()" \n
  "A = NINT()  !nearest integer" \n
  "A = abs(nint()) !round" \n
  "A = MOD()   !mod(x,y): modulo x/y" \n
  "A = RAND()  !rand(lower_bound,upper_bound)" \n
  "A = GDIS()  !gdis(mean,stdd): gaussian distribution" \n
  "A = SIN()"  \n
  "A = COS()"  \n
  "A = TAN()"  \n
  "A = SINH()" \n
  "A = COSH()" \n
  "A = TANH()" \n
  "A = ASIN()" \n
  "A = ACOS()" \n
  "A = ATAN()" \n
  "A = ATAN2() !atan2(x,y): arctangent of y/x"\n
  \n
  )

(define-skeleton ansys-skeleton-geometry
  ""
  nil
  "\n!! ------------------------------" \n
  "!@@ -- geometry --"\n
  \n
  "/prep7" \n
  "X1 = 0" \n
  "X2 = 1" \n
  "Y1 = X1" \n
  "Y2 = X2" \n
  "rectng,X1,X2,Y1,Y2 ! 2d rectangle" \n
  "Z1 = 0" \n
  "Z2 = 1" \n
  "block,X1,X2,Y1,Y2,Z1,Z2 !3d hexahedron (wp)" \n
  "!!arsym,y,all ! reflection of areas "
  "Xc  = 0 !centre x-coord." \n
  "Yc  = 0 !centre y-coord." \n
  "R1  = 4" \n
  "R2  = 20" \n
  "N   = 14." \n
  "Th1 = -360./(2*N)" \n
  "Th2 = +360./(2*N)" \n
  "Depth=30" \n
  "pcirc,R1,R2,Th1,Th2 ! circular area" \n
  "cyl4,Xc,Yc,R1,Th1,R2,Th2,Depth ! circular area or cylinder" \n
  "shpere,Rad1,Rad2,Th1,Th2 !spherical volume" \n
  "cylind,R1,R2,Z1,Z2,Th1,Th2 !cylinder V>0! " \n
  \n
  "!@@@ - operations -" \n
  "vdele,all,,,1 ! delete everything below" \n
  \n
  "!@@@ - booleans -" \n
  \n
  "aovlap,all ! overlap areas" \n
  "asba,A1,A2,SEPO,KEEP1,KEEP2 ! SEPO: seperate entities" \n
  "asbw, !substract by wp" \n
  "/pnum,area,1 $ aplot" \n
  "vglue,all" \n
  "vsbw,all,,delete !v substracted by wp" \n
  "vdele,all,,,1 !skwp 1:delete kp,l,a as well" \n
  \n
  )

(define-skeleton ansys-skeleton-material-def
  ""
  nil
  "\n!@@ -- material definitions --" \n
  \n
  "Steel=1" \n
  "mp,nuxy,Steel,0.3 ! Poisson No" \n
  "mp,ex,Steel,200000 ! Elastic modulus" \n
  "AlphaSteel = 12e-6 ! thermal expansion in 1/K" \n
  "mp,alpx,Steel,AlphaSteel !secant modulus of therm. exp.!" \n
  "!mp,ctex,Steel,12e-6 ! instantaneous coofficient of therm. exp." \n
  "KSteel = 60.5 !conductivity in W/(mK)" \n
  "mp,kxx,Steel,KSteel" \n
  "mplist,all" \n
  "mpplot,ex,Steel,100,500 !plots mat. vs temp." \n
  "tb,biso,Steel,1 ! bilinear isotropic plasticity" \n
  "Yield_stress = 160" \n
  "Tensile_strain = 0.3" \n
  "True_tensile_strain = log( 1+Tensile_strain)" \n
  "Tensile_stress = 260" \n
  "True_tensile_stress = Tensile_stress*(1+Tensile_strain)" \n
  "Tangent_modulus = (True_tensile_stress-Yield_stress) / True_tensile_strain" \n
  "tbdata,,Yield_stress,Tangent_modulus" \n
  "tblist !list data tables" \n
  "tbplot,biso,Steel" \n
  "/com, === Material %Steel% is steel. ===" \n
  "Alu=2" \n
  "mp,nuxy,Alu,0.3" \n
  "mp,ex,Alu,70000" \n
  "tb,biso,Alu,1" \n
  "tbdata,,Yield_stress,Tangent_modulus" \n
  "mptemp !erase temperature table"  \n
  "mptemp,,-100,0,100,200 !4 temperatures" \n
  "mpdata,kxx,Alu,,114,144,165,175 !conductivities in W/(mK)" \n
  "mptemp" \n
  "/com, === Material %Alu% is Aluminium. ===" \n
  "Air=3" \n
  "mptemp,,-150,0,100,200,300,500,1000" \n
  "mpdata,kxx,Air,,.012,.0243,.0314,.0386,.0454,.057,.0662" \n
  "mptemp" \n
  \n
  "!! -- orthotropic linear material --" \n
  "esel,s,mat,,bla" \n
  "emodif,all,esys,12 !modify esys" \n
  "mp, e{x,y,z}, Steel, VAL" \n
  "mp, alp{x,y,z}, Steel, VAL !in element co-ordinate system" \n
  \n
  "!! --- Elastomers (hyperelasticity) ---" \n
  \n
  "!! --- Neo Hook ---" \n
  "!! for 30 % strain" \n
  "Shore = 60" \n
  "ShearModule = 0.086*1.045**Shore !guestimate" \n
  "NeoHook = 1" \n
  "tb,hyper,NeoHook,,,neo" \n
  "BulkModulus = 2000" \n
  "tbdata,1,ShearModulus,1/(2*BulkModulus)" \n
  \n
  "!! --- Mooney-Rivlin ---" \n
  "!! for 30 % compression 100 % tension strain" \n
  "Mooney = 2" \n
  "tb,hyper,Mooney,,,MOONEY" \n
  "tbdata,1,3*ShearModule/6.6" \n
  "tbdata,2,.3*ShearModule/6.6" \n
  "!! -- check whether to drop elem. midside nodes and use u-p formulation" \n
  "!! -- u-p is not needed in plane stress configurations!" \n
  "keyopt,Mooney,6,1		 !(6)1: mixed u-p formulation" \n
  "!! --- Ogden for high strain applic. (700 % strain) ---" \n
  "Ogden = 3" \n
  "tb,hyper,Ogden,1,2,OGDEN !2nd order Ogden model" \n
  "tbdata,1,3.5809,1.05e-9,3.8485e5" \n
  "tbdata,4,-2.2e6,-.8778,0" \n
  \n
  "!! --- Magnetic materials ---" \n
  "Air = 4" \n
  "mp,murx,Air,1 ! murx permeability" \n
  "Magnet = 5" \n
  "Hc = 2.8e5 ! ferrit magnet coercive force in A/m" \n
  "mp,mgxx,Magnet,Hc " \n
  "Pi = acos(-1)" \n
  "Mu0 = .4*Pi*1e-6 ! field constant in Vs/(Am)" \n
  "Br = .4 ! residual induction in Tesla" \n
  "mp,murx,Magnet,Br/(Mu0*Hc)" \n
  \n
  "/pnum,mat,1 $ eplot"\n
  \n
  )

(define-skeleton ansys-skeleton-bc
  ""
  nil
  "\n!@@ -- boundary conditions --"\n
  \n
  "/prep7" \n
  \n
  "kbc,1 ![0](antype,static):ramped, 1:stepped loading" \n
  \n
  "!@@@ - DOF constraints -" \n
  \n
  "nsel,s,loc,y,0" \n
  ",a,loc,y,1" > \n
  ",r,loc,x,0" > \n
  "d,all,all!dk,dl,da" \n
  "dlist,all" \n
  \n
  "!@@@ - concentrated loads -" \n
  "f,all,fx,1 !fk" \n
  "flist,all !fklist" \n
  \n
  "!@@@ - surface loads -" \n
  "sf,all,pres,1 !surface loads on nodes" \n
  "sflist,all" \n
  "sfe,all,pres" \n
  "sfelist,all" \n
  "sfl,all,pres" \n
  "sfllist,all" \n
  "sfa,all,,pres," \n
  "sfalist,all" \n
  \n
  "!@@@ - body loads -" \n
  "tref,23 ![0] degree reference temperature" \n
  "tunif,30 !uniform temperature (default step applied!)" \n
  "bf,all,temp,30 !bfe,bfk,bfl,bfa,bfv" \n
  "bflist,all !list body loads" \n
  "!! e. g.: as harmonic acceleration load with amplitude steps" \n
  "*dim,mytab,table,5,1,,freq" \n
  "mytab(1,0)=   20,  199, 200, 999,1000" \n
  "mytab(1,1)=100e3,100e3,30e3,30e3,10e3" \n
  "acel,%mytab%,, !acceleration in global coordinates" \n
  \n
  "!@@@ - inertia relief -" \n
  \n
  "!! LOADS: nonlinearities aren't supported, fix all DOFs!" \n
  "irlf,1 !0: none,1:ir on,-1:printout masses" \n
  "nsel,s,loc,x,1" \n
  \n
  "!@@@ - corriolis effects -" \n
  \n
  "cgmga,x,y,z, ! rotational velocity about globla coord. sys." \n
  "dcgomg,x,y,z ! rotational acceleration about global coord. sys." \n
  \n
  "!@@@ - coupling -" \n
  "nsel,s,loc,x,1" \n
  "cp,next,uy,all !couple dofs" \n
  \n
  "allsel" \n
  "/pbc,all,on" \n
  "gplot" \n
  \n
  "!@@@ - magnetics -" \n
  \n
  "!! fmagbc,'Component' ! flag force calculation" \n
  "bfa,all,js, ! js current density" \n
  "bflist,all" \n
  "dl,all,,asym ! flux parallel to lines" \n
  "nsel,s,ext ! select exterior nodes" \n
  "dsym,asym ! flux parallel to lines" \n
  \n
  )

(define-skeleton ansys-skeleton-buckling
  ""
  nil
  "\n!! ------------------------------" \n
  "!@@ - buckling -" \n
  \n
  "!! -- static --"
  "/solu" \n
  "allsel" \n
  "outres,all,all" \n
  "pstres,on" \n
  "solve" \n
  "" \n
  "!! -- buckling --" \n
  "" \n
  "fini $ /solu" \n
  "antype,buckle" \n
  "bucopt,lanb,3" \n
  "outres,all,all" \n
  "solve" \n
  "fini $ /solu	 !switch to another loadstep?" \n
  "expass,on" \n
  "mxpand,3" \n
  \n
  )

(define-skeleton ansys-skeleton-solve
  ""
  nil
  "\n!! ------------------------------" \n
  "!@ --- solution --- " \n
  "!! ------------------------------" \n
  \n
  "/solu" \n
  "allsel" \n
  \n
  "solcontrol,on! optimised nonlinear solution defaults" \n
  "!! implies /kbc,0: ramped loading" \n
  "n1=20 ! No of substeps for the first one" \n
  "n2=n1*100 ! maximum No of substemps" \n
  "n3=n1/4 ! minimum No of substeps " \n
  "nsubst,n1,n2,n3"\n
  "outres,all,all"\n
  "antype!,,rest,LoadStep,SubStep !rest: perform restart operation" \n
  "nlgeom,on" \n
  "autots,on" \n
  \n
  "solve" \n
  \n
  "cnvtol,u,,0.1! convergence [0.5 % solcontrol, on: 5 %] manipulation" \n
  "cnvtol,f,,0.05 !solcontol,on: [0.5% F,M; 5% U]" \n
  "neqit,30! No of equilibr. iterations"
  "nldiag,nrre,on! store residual file" \n
  "nldiag,maxf,2! maximum files written" \n
  "rescontrol,,1,last !create restart file(s)" \n
  ",status" > \n
  "/config,nres,2000 !No of substeps in result file [1000]" \n
  "/solu" \n
  \n
  "nlhist,on !nonlinear tracking in .nlh" \n
  "eqslv,pcg,1e-4" \n
  "nropt,unsym !frictional contacts not converging?" \n
  "coupling of sliding and normal stiffness" \n
  "stabilize,constant,energy,1e-4 !constant over LS" \n
  "stabilize,reduce,energy,1e-4 !reduce to the end of LS" \n
  "stabilize !decactivate stabilisation" \n
  "arclen,on ! arclen stabilisation" \n
  \n
  "/runst !enter the run statistics processor" \n
  "rall !run statistics estimator" \n
  \n
  "rescontrol,file_summary !check restart files" \n
  "antyp,,rest,1,last"\n
  "time,1.2 !time at the end of load step" \n
  \n
  "!@@ -- magnetics --" \n
  \n
  "magsolv" \n
  \n
  "!@@ -- cyclic symmetry --" \n
  \n
  "cycopt,status" \n
  \n
  )

(define-skeleton ansys-skeleton-post1
  ""
  nil
  "\n!! ------------------------------" \n
  "!@ --- post 1 ---" \n
  "!! ------------------------------" \n
  \n
  "/post1" \n
  "!! --- theory reference: Nodal and centroidal data evaluation ---" \n
  "eresx!defa->elastic:extrapolate from integration points to nodes, nonlinear:copy, yes->extrapolate linear part, no->copy to nodes" \n
  "!! --- derived nodal data computation ---" \n
  "avprin !principal sums and vector sums" \n
  "avres !result averaging for PowerGraphics" \n
  "plnsol !continuous contours, averaged at neighbouring nodes" \n
  "plesol !no nodal averaging" \n
  "pletab !constant (centroidal) value per element" \n
  "pletab,avg !constant element value, averaged at neigbouring nodes" \n
  "/inquire,job_name,jobname" \n
  "!resume,job_name,db" \n
  "set,last" \n
  "pldisp,2 !display displaced structure" \n
  "plnsol,u,sum,2 !0:deformed only, 1:with undef model 2:with undeformed edges" \n
  "plnsol,s,eqv ! von Mises" \n
  "plnsol,s,1 ! maximum principle: Lamé" \n
  "plnsol,s,int ! stress intensity: Tresca" \n
  "prnsol,s,x !|presol components in global x-dir (except transformed:nrotat,rsys)"\n
  "plnsol,s,xy ! shear in xy-dir." \n
  "plnsol,epto,1!principal total mechanical strain (excluding thermal) (EPEL + EPPL + EPCR)," \n
  "plvect,u !display vector results"\n
  "!! reactions"\n
  "fsum !force sum from all selected nodes"\n
  "*get,Fy,fsum,,item,fy" \n
  "*get,T,active,,set,time" \n
  "nforce !list of all nodal forces" \n
  "/gcolumn,1,'Reaction'" \n
  "/axlab,x,Substep" \n
  "/axlab,y,Force in N" \n
  "/gropt,fill,1 ! fill curves" \n
  "*vplot,,Reaction" \n
  \n
  "/dscale,,1 !do not scale (for nlgeom)" \n
  "/dscale,,auto !or 0:scale automatically" \n
  "*get,Ds,graph,WN,dscale,dmult" \n
  "/contour,,ncont,min,inc,max" \n
  "/contour !,,auto !switch off user contours" \n
  "/cval,,10,20,30,40,50 !explicit contour values" \n
  "/edge,,1 !1:display elements in contour plots" \n
  "/edge,,0 !0:switch off display of elements in contour plots" \n
  "/plopts,minm,off !switch off min-max symbols" \n
  "/plopts,minm,on" \n
  "/pbc,rfor,,1 !1:show reaction f. symbols" \n
  "/pbc,rfor,,0" \n
  "/dist,,1/2,1 !enlarge twice" \n
  "/noerase ! don't erase screen between plots" \n
  "erase" \n
  "/triad,rbot ! coordinate system to right bot" \n
  "/plopts,wp ! switch off working plane" \n
  "/plopts,minm ! switch off min max" \n
  \n
  "/image,save,test !save XWindow Dump xwd (or bmp on Windows)" \n
  "/sys,convert test test.png" \n
  "!! -- graphics output & invert background colour --" \n
  "/RGB,index,100,100,100,0" \n
  "/RGB,index,0,0,0,15" \n
  "/show,png !creates jobnameXXX.png files" \n
  "pngr,stat !additional png options (orientation,compression,...)" \n
  "plvect,B" \n
  "/noerase" \n
  "lplot" \n
  "/show,close" \n
  "erase" \n
  \n
  "/efacet,2" \n
;  "psdisp,0" \n
  "/graphics,full ! results averaging also from interior" \n
  "pletab,Pene" \n
  "plls,Pene,Pene !line element results" \n
  "!@@@ - magnetics -" \n
  "plf2d,27 ! flux lines, equipotentials" \n
  "plvect,b,! induction vector plot" \n
  "fmagsum,'component_name'" \n
  \n
  "nldpost,nrre,stat !element information nonlinear" \n
  "plnsol,nrre,,,,001 !plot residual file .nr001 " \n
  "etable,Pene,cont,pene" \n
  "etable,chat,cont,cnos" \n
  "etable,cpre,cont,pres" \n
  "etable,Slid,cont,slide" \n
  "etable,St,cont,stat	 !3-closed sticking" \n
  "!! 			 !2-closed sliding" \n
  "!! 			 !1-open but near" \n
  "!! 			 !0-open and far, outside pinball" \n
  "set,list" \n
  "set,last!first,next,previous" \n
  "set,2,last ! set,last,last does not work!" \n
  "plnsol,s,1" \n
  "antime" \n
  "andata" \n
  "anmres !multiple result files" \n
  \n
  "!! cycexpand,on ! graphical expansion" \n
  \n
  )

(define-skeleton ansys-skeleton-output-to-file
  ""
  nil
  "\n!! ------------------------------" \n
  "!@@ -- output to file --" \n
  \n
  "! redirect ansys text output to file" \n
  "/output,test,txt,,append !append solver output to file" \n
  "/com,# dist from center | axial mag. induction" \n
  "*vwrite,B(1,1),B(1,2)" > \n
  "%G %G" \n
  "/output ! redirect to screen" \n
  \n
  "! macro file" \n
  "*create,test.txt ! macro file, no parameter substitution!" \n
  "*end !up to this command" > \n
  "!! can be used with the *use command to pass params into it" \n
  "!! /input does not allow parameters" \n
  \n
  "! create data file" \n
  "*cfopen,test,txt,,append ! appending to file" \n
  "*cfwrite,A=5 ! interpreted output" \n
  "*set strings are limited to 32 characters!!!" \n
  "Strg1='# cylindrical magnet: radius = %Rad%'" \n
  "Strg2=', lenght = %Len%'" \n
  "Strg3='# distance, magnetic induction'" \n
  "*vwrite,Strg1, Strg2, Strg3" \n
  "%S %S% %S" \n
  "*vwrite,B(1,1),B(1,2)" > \n
  "%E %E" > \n
  "*cfclos ! close file" \n
  "/input,test,txt,,:label ! read from label onwards"\n
  \n
  "!! graphical output" \n
  "/RGB,index,100,100,100,0" \n
  "/RGB,index,0,0,0,15" \n
  "/show,png !creates jobnameXXX.png files" \n
  "pngr !additional options" \n
  "plvect,B" \n
  "noerase" \n
  "lplot" \n
  "/show,close" \n
  \n
  )

;TODO: explain what's it for
(define-skeleton ansys-skeleton-select
  ""
  nil
  "\n!@@@ - select stuff -" \n
  \n
  "!! lowest face No of element E from selected nodes"
  "a=nmface(E) !plane elements:faces =^= el. sides" \n
  "node in POS of element E" \n
  "bla=nelem(E,POS) !pos. ijklmnop =^= [1:8]" \n
  "!unsel midnodes of 8-node 2d elem" \n
  "*get,En,elem,,count" \n
  "*get,E,elem,,num,min" \n
  "*do,I,1,En,1" \n
  "  Face = nmface(E)" \n
  "  Ntmp = ndface(E,Face,3)" \n
  "  nsel,u,,,Ntmp" \n
  "  *get,E,elem,E,nxth" \n
  "*enddo  " \n
  \n
  )

(define-skeleton ansys-skeleton-path-plot
  ""
  nil
  "\n!! ------------------------------" \n
  "!@@ -- path plot --" \n
  "!! avoid element borders for inaccuracies of the rounding algorithm." \n
  \n
  "path,Name,nPts[2],nSets[30],nDiv[20] ! define active path \"Name\"" \n
  "!ppath,P,N,x,y,z,CS coord. in global cartesian csys, but use CS for path" \n
  "ppath,1" \n
  "ppath,2,,,Rair" \n
  "psel,s,axis,...    	 !select multiple paths" \n
  "pdef,By,b,y" \n
  "pcalc,add,By2,By,,2 !multiply By by 2" \n
  "/axlab,x,distance !axis label" \n
  "plpath,By		 !plot in graph" \n
  "plpagm,By,5		 !plot on geom." \n
  "!write into table variable content: x,y,z,path length?,v1,v2,..." \n
  "paget,Path,table" \n
  "!path $ stat" \n
  \n
  )

(define-skeleton ansys-skeleton-post26
  ""
  nil
  "\n!! ------------------------------" \n
  "!@ --- time-history postprocessing ---" \n
  "!! ------------------------------" \n
  \n
  "/post26" \n
  "numvar,200 !maximum No of variables"
  "esol,2,1,,u,z,'displ z'" \n
  "nsol,2,1,u,z" \n
  "filldata,7,1,10,,20 !fills a variable by a ramp or constant"\n
  "rforce,3,1,f,z" \n
  "add,4,2,,,displ,,,-1" \n
  "/grid,1" \n
  "/gmarker,1,1 !curve marking: 1: triangles,2: squares" \n
  "/xrange,0,1" \n
  "/xrange,default" \n
  "/yrange,0,1" \n
  "/axlab,x,x" \n
  "/axlab,y,y" \n
  "/gthk,curve,3!set line thickness" \n
  "timerange,0,1" \n
  "/title,bla" \n
  "/stitle,,blabla !subtitle line 1 (not shown in plot)" \n
  "/stitle,2,blabla !subtitle line 2" \n
  "/tlable,x,y,bla !annotation at (x,y)" \n
  "xvar,2" \n
  "!! invert background colour" \n
  "/RGB,index,100,100,100,0" \n
  "/RGB,index,0,0,0,15" \n
  "/show,png !creates jobnameXXX.png files" \n
  "plvar,3" \n
  "/show,close" \n
  "!!prvar,3" \n
  \n
  )

;; TODO: complete
(define-skeleton ansys-skeleton-array
  "arrays"
  nil
  "\n!! ------------------------------" \n
  "!@@ -- table arrays --" \n
  "!! table arrays are *set with integers" \n
  "!! and accessed with real indices" \n
  "NSS=100" \n
  "*dim,F_y,table,NSS,3! three columns" \n
  "F_y(0,1) = 1,2,3 ! column 'index'" \n
  "*do,I,1,NSS" \n
  "set,1,I"> \n
  "fsum"> \n
  "*get,Tim,active,,set,time"> \n
  "Strain = Tim*100*Displ/Leng"> \n
  "F_y(I,0) = Strain ! row 'index'"> \n
  "*get,Forc,fsum,,item,fy"> \n
  "F_y(I,1) = Forc/(Width*Thick)"> \n
  "*enddo"> \n
  "!! e. g.: as harmonic acceleration load (with amplitude steps)" \n
  "*dim,mytab,table,5,1,,freq" \n
  "mytab(1,0)=   50,  199, 200, 999,1000" \n
  "mytab(1,1)=100e3,100e3,30e3,30e3,10e3" \n
  "acel,,,%mytab% !acceleration in global coordinates" \n
  \n
  "!@@ -- arrays --" \n
  \n
  "*dim,A,,10,1 ! type array is default, No of rows, No of columns" \n
  "B = !deleting before redimensioning necessary" \n
  "*dim,B,table,10,1,1,TIME !table type interpolating" \n
  "B(1,0,1) = 0." \n
  "B(2,0,1) = 1." \n
  "*get,A,x,,item,fx" \n
  \n
  "*get,Nn,node,,count" \n
  "*vget,PAR,node,,nlist! array of nodenumbers" \n
  "A = 1,2,3,4,5" \n
  \n
  "!! -- check dimensions --" \n
  "*get,Dim,parm,A,dim,x" \n
  "*if,Dim,le,1,then" \n
  "*dim,A,array,10,1" > \n
  "*endif" > \n
  "*do,I,1,Ns" \n
  "set,Ls,I" > \n
  "fsum" \n
  "Reaction(I)=Fx" \n
  "*enddo" > \n
  "!! -- plotting --" \n
  "! arrays are plotted as histograms,tables are plotted as curves"
  "/gcol,1,'curve1'"\n
  "/gropt,fill,1 !fill lines"
  "/axlab,x,'x-variable in mm'" \n
  "/xrange,0,10 !xrange of plot"\n
  "*vplot,time(1,1),A(1,2)!plot column 2 of A " \n
  "/gmarker,1,3,10" \n
  "/gcolumn,1,'Neo-Hook'" \n
  "/gcolumn,2,'Mooney-R'" \n
  "/gcolumn,3,'Ogden'" \n
  "/gthk,curve,4 !curve thickness" \n
  "*vplot,F_y(1,0),F_y(1,1),2.0"
  \n
  )

(define-skeleton ansys-skeleton-structural
  "Minimum working structural APDL template."
  nil					;no interactor needed
  '(ansys-skeleton-header)
  "!@ --- Preprocessing ---"\n
  "/prep7"\n
  "!@@ -- Elements --"\n
  "Steel = 1"\n
  "ID = Steel"\n
  "real = Steel"\n
  "et,ID,solid186 !3d, 20 node"\n
  "!@@ -- Material --"\n
  "mp,nuxy,Steel,0.3 ! Poisson No"\n
  "mp,ex,Steel,200000 ! Elastic modulus"\n
  "!@@ -- Modeling --"\n
  "block,0,1,0,1,0,1"\n
  "!@@ -- Meshing --"\n
  "vmesh,all"\n
  "!@@ -- BCs, Loads --"\n
  "nsel,s,loc,x,0"\n
  "d,all,all"\n
  "nsel,s,loc,x,1"\n
  "d,all,uy,-.1"\n
  "allsel"\n
  "save"\n
  "!@ --- Solving ---"\n
  "/solu"\n
  "solve"\n
  "!@ --- Postprocessing --"\n
  "/post1"\n
  "plnsol,u,sum"\n
  \n
  )

(define-skeleton ansys-skeleton-contact
  "Minimum working structural contact APDL template."
  nil					;no interactor needed
  '(ansys-skeleton-header)
  "!@ --- Preprocessing ---"\n
  "/prep7"\n
  "!@@ -- Elements --"\n
  "Steel = 1"\n
  "ID = Steel"\n
  "real = Steel"\n
  "et,ID,solid186 !3d, 20 node"\n
  "tid = 4" \n
  "cid = 3" \n
  "r,cid" \n
  "et,tid,170" \n
  "et,cid,174" \n
  "!@@ -- Material --"\n
  "mp,nuxy,Steel,0.3 ! Poisson No"\n
  "mp,ex,Steel,200000 ! Elastic modulus"\n
  "!@@ -- Modeling --"\n
  "block,0,1,0,1,0,1"\n
  "block,1,2,0,1,0,1"\n
  "!@@ -- Meshing --"\n
  "vmesh,all"\n
  "vsel,s,,,1" \n
  "eslv,s" \n
  "nsle,s" \n
  "nsel,r,loc,x,1" \n
  "type,cid" \n
  "real,cid" \n
  "esurf" \n
  "" \n
  "type,tid" \n
  "real,cid" \n
  "vsel,s,,,2" \n
  "eslv,s" \n
  "nsle,s" \n
  "nsel,r,loc,x,1" \n
  "esurf" \n
  "keyo,cid,12,5              ! bonded always" \n
  "keyo,cid,2,1               ! penalty function only" \n
  "keyo,cid,9,1               ! ignore initial gaps/penetration" \n
  "keyo,cid,7,0               ! No Prediction" \n
  "rmod,cid,3,10.	! FKN" \n
  "rmod,cid,5,0.	! ICONT" \n
  "rmod,cid,6,0.	! PINB" \n
  "rmod,cid,10,0.	! CNOF" \n
  "rmod,cid,12,0.	! FKT" \n
  "!@@ -- Loads --"\n
  "nsel,s,loc,x,0"\n
  "d,all,all"\n
  "nsel,s,loc,y,1"\n
  "esln,s" \n
  "sf,all,pres,1e3"\n
  "allsel"\n
  "save"\n
  "!@ --- Solving ---"\n
  "/solu"\n
  "nsubst,10"\n
  "solve"\n
  "!@ --- Postprocessing --"\n
  "/post1"\n
  "plnsol,u,sum"\n
  \n
  )

(defun ansys-skeleton-compilation ()
  "Collection of important code templates for an APDL file."
  (interactive)
  (ansys-skeleton-header)
  (goto-char (point-max))
  (ansys-skeleton-import)
  (goto-char (point-max))
  (ansys-skeleton-configuration)
  (goto-char (point-max))
  (ansys-skeleton-geometry)
  (goto-char (point-max))
  (ansys-skeleton-material-def)
  (goto-char (point-max))
  (ansys-skeleton-element-def)
  (goto-char (point-max))
  (ansys-skeleton-meshing)
  (goto-char (point-max))
  (ansys-skeleton-bc)
  (goto-char (point-max))
  (ansys-skeleton-solve)
  (goto-char (point-max))
  (ansys-skeleton-post1))

;; TODO: warning dated
(define-skeleton ansys-skeleton		;NEW
  "Insert full framework of an Ansys APDL file."
  "Insert brief purpose of file: "
  "!" ansys-outline-string " ********* first line ***************\n"
  "!! FILENAME: " (buffer-file-name) \n
  "!! CREATION DATE: " (current-time-string) \n
  "!! ANSYS VERSION: " ansys-current-ansys-version \n
  "!! DESCRIPTION: " str \n
  "!! ------------------------------" \n
  "!! COMMENTARY: User parameters start in upper case." \n
  "!!  Ansys command names may be ommitted (defaulting to the" \n
  "!!  previous command, except slash and asterisk commands)." \n
  "!! WARNING: Variables starting with an underscore are reserved"  \n
  "!!  (for  components and Ansys furnished macros, like" \n
  "!!  the %_FIX% table name for current displacement values or" \n
  "!!  the _RETURN and _STATUS variable (_STATUS: 0 no error, 1" \n
  "!!  note, 2 warning, 3 error)!" \n \n
  "!! ==============================" \n
  "!" ansys-outline-string " --- Setup ---" \n
  "!! ==============================" \n
  \n
  "finish "\n
  "/clear" \n
  "y !necessary for /clear" \n
  \n
  "*get,Wallstrt,active,,time,wall" \n
  "c*** Configuring for 2 processors does not harm when only 1 is present" \n
  "/config,nproc,2" \n
  "/uis,msgpop,3 !3: No warning popup boxes" \n
  "*afun,deg !trig: funs accept degree args" \n
  "*afun,rad" \n
  "/title," _ \n
  "/plopts,wp,1 !display working plane" \n
  "/triad,rbot" \n
  "!! /output, !change solver output file" \n
  "!! /input," \n
  "/filname," (setq ansys-job (skeleton-read "Ansys jobname: " "file")) \n
  \n
  "!! ==============================" \n
  "!" ansys-outline-string " --- Preprocessing --- " \n
  "!! ==============================" \n
  \n
  "!! ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string " --- Cad Import --- " \n
  "!! ------------------------------" \n
  \n
  "!! /aux15" \n
  "!! ioptn,iges,nodefeat" \n
  "!! ioptn,merge,yes" \n
  "!! ioptn,solid,yes" \n
  "!! ioptn,small,yes" \n
  "!! ioptn,gtoler,defa" \n
  "!! igesin,'test','iges'"\n
  \n
  "!! /input,fname,anf" \n
  "!! /facet,norm" \n
  \n
  "!! ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string " --- General Preprocessing --- " \n
  "!! ------------------------------" \n
  \n
  "/prep7" \n
  "Pi=3.14159265359" \n
  \n
  "!! /pnum,area,1"\n
  \n
  "!! --- Materials and element types ---" \n
  "Steel=1" \n
  "Alu=2" \n
  "!! Contact = 3" \n
  "!! Target = 4" \n
  "mp,nuxy,Steel,0.3" \n
  "mp,ex,Steel,200000" \n
  "!! tb,biso,Steel,1" \n
  "!! yield_stress=140" \n
  "!! tangent_modulus=1400" \n
  "!! tbdata,,yield_stress,tangent_modulus !biso" \n
  "/com, === Material %Steel% is steel. ===" \n
  "et,Steel,solid186 !3d, 20 node" \n
  "!! et,Steel,solid185 !3d, 8 node" \n
  "!! et,Steel,plane183,,,0 !2d, 8 node (3)0:plane stress, 1:axissymmetric" \n
  "!! et,Steel,plane182 !2d, 4 node"\n
  "!! keyopt,Steel,3,1 !keyopt(3)=1:axissym." \n
  "!!   for most elements the radial direction is the x-axis" \n
  \n
  "!! mp,nuxy,Alu,0.3" \n
  "!! mp,ex,Alu,70000" \n
  "!! tb,biso,Alu,1" \n
  "!! !! tbdata,,yield_stress,tangent_modulus !biso" \n
  "!! /com, === Material %Alu% is Aluminium. ===" \n
  "!! et,Alu,solid186 !3d" \n
  "!! !! et,Alu,plane183 !2d" \n
  "!! !! et,Alu,plane182 !2d 4 node" \n
  "!! !! keyopt,Alu,3,1 !0:plane stress, 1:axissym." \n
  \n
  "!! !! --- Contacts ---" \n
  "!! r,Contact" \n
  "!! et,Contact,conta174 !3d, 8 node" \n
  "!! !! et,Contact,conta173, !3d, 4 node" \n
  "!! !! et,Contact,conta172 !2d, 3 node" \n
  "!! !! et,Contact,conta171 !2d, 2 node" \n
  "!! et,Target,targe170 !3d" \n
  "!! !! et,Target,targe169 !2d, 2/3 node" \n
  "!! keyo,Contact,2,1 !algorithm 0:augm. Lagrange (default),1:penalty,2:MPC,4:pure Lagrange" \n
  "!! keyo,Contact,5,1 !initial contact closure,1:auto CNOF adjustment to close geometric gap only" \n
  "!! keyo,Contact,9,2 !initial penetration,1:ignore initial gaps/penetr 2:ramp" \n
  "!! keyo,Contact,10,2 !contact stiffness update,2:each NR iteration,1:each substep" \n
  "!! keyo,Contact,12,0 !contact behaviour,0:frictional/-less (default),1:rough" \n
  "!! real,Contact" \n
  "!! rmod,Contact,3,1. !FKN:normal penalty stiffness factor (default:1)" \n
  "!! rmod,Contact,5,0.0 !ICONT:amount of initial contact closure (positiv:penetration)" \n
  "!! rmod,Contact,6,-0.1 !PINB:pinball radius (negativ means no scaling:absolute distance)" \n
  "!! rmod,Contact,10,0. !CNOF:contact surface offset" \n
  "!! mp,mu,Contact,0.4 !friction factor" \n
  "!! rmod,Contact,12,0. ! FKT:tangent stiffness factor (corresp. to FKN),0:means 1 for Ansys!!!" \n
  \n
  "!" ansys-outline-string ansys-outline-string ansys-outline-string " --- Geometry ---" \n
  \n
  "bloc,0,1,0,1,0,1" \n
  "*get,A1,area,,num,max" \n
  "!! rectng,0,1,0,1 !x1,x2,y1,y2" \n
  "!! k,,1,0,0 & KN = _return !keypoint number" \n
  \n
  "!! /number,1 !0: colour and number,1: colour" \n
  "!! /pnum,line,1 !1: turn on numbering" \n
  "!! lplot" \n
  "!! lesize,1,,,3" \n
  \n
  "!! --- Meshing ---" \n
  \n
  "!! mat,Steel" \n
  "!! mshkey,1 !1: mapped meshing,2: mapped if possible" \n
  "!! mshape,0 !0: quadrilaterals" \n
  "esize,1" \n
  "vmesh,all" \n
  \n
  "!! amesh,all" \n
  \n
  "!! !! --- Rigid targets ---" \n
  \n
  "!! type,Target" \n
  "!! real,Contact" \n
  "!! tshap,line" \n
  "!! *get,Nmax,node,,num,max" \n
  "!! n,Nmax+1,1,1,0" \n
  "!!  ,Nmax+2,1,2,0" \n
  "!! e,Nmax+1,Nmax+2" \n
  "!! tshap,pilo" \n
  "!! e,Nmax+1" \n
  \n
  "!! !! --- Contacts --- " \n
  \n
  "!! type,Contact" \n
  "!! real,Contact" \n
  "!! esurf !,,reverse !also 2d" \n
  \n
  "!! /pcb !bc symbols"\n
  "!! /psf !surface loads" \n
  "!! /pbf !body loads"
  "!! /psymb,esys,on !check element esys" \n
  "!! /psymb,ndir !only for rotated nodal co-ordinate systems!" \n
  "!! cncheck !initial contact status" \n
  \n
  "!" ansys-outline-string ansys-outline-string ansys-outline-string " --- boundary conditions --- " \n
  \n
  "nsel,s,loc,y,0" \n
  "    ,a,loc,y,1" \n
  "    ,r,loc,x,0" \n
  "d,all,all" \n
  "nsel,s,loc,x,1" \n
  "cp,next,uy,all !couple dofs" \n
  "f,1,fx,1" \n
  "allsel" \n
  "/pbc,all,on" \n
  \n
  "!! ==============================" \n
  "!" ansys-outline-string " --- solution --- " \n
  "!! ==============================" \n
  \n
  "/solu" \n
  \n
  "!! solcontrol,,on, ! ,,check contact state,pressure load stiffness"
  \n
  "!! n1=20" \n
  "!! n2=n1*100" \n
  "!! n3=n1/4" \n
  "!! nsubst,n1,n2,n3"\n
  "!! outres,all,all"\n
  "!! nlgeom,on" \n
  "!! autots,on" \n
  \n
  "!! rescontrol,,all,1 !restart files" \n
  "!! eqslv,pcg,1e-4" \n
  "!! cnvtol,f,,0.05 !solcontol,on: [0.5% F,M; 5% U]" \n
  "!! nropt,unsym !frictional contacts not converging?" \n
  "!! coupling of sliding and normal stiffness"
  \n
  "/eof ------------------------------" \n
  \n
  "/runst !enter the run statistics processor" \n
  "rall !run statistics estimator" \n
  "/solu"  \n
  "*get,Wallasol,active,,time,wall" \n
  \n
  "solve" \n
  "y" \n
  "save $ finish" \n
  \n
  "*get,Wallbsol,active,,time,wall" \n
  \n
  "!! ==============================" \n
  "!" ansys-outline-string " --- Postprocessing ---" \n
  "!! ==============================" \n
  \n
  "!! ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string
  " --- General Postprocessing --- " \n
  "!! ------------------------------" \n
  \n
  "/post1" \n
  \n
  "!! /dscale,,1 !do not scale (for nlgeom)" \n
  "!! /dscale,,auto !or 0:scale automatically" \n
  "!! !*get,Ds,graph,WN,dscale,dmult" \n
  "!! /contour,,ncont,min,inc,max" \n
  "!! /contour,,auto !switch off user contours" \n
  "!! /edge,,1 !1:display elements in contour plots" \n
  "!! /edge,,0 !0:switch off display of elements in contour plots" \n
  "!! /plopts,minm,off !switch off min-max symbols" \n
  "!! /plopts,minm,on" \n
  "!! /pbc,rfor,,1 !1:show reaction f. symbols" \n
  "!! /pbc,rfor,,0" \n
  "!! /expand,8,lpolar,half,,45 !polar symmetry expansion" \n
  "!! /expand,8,lpolar,half,,45 !polar symmetry expansion" \n
  "!!  !half symmetry(mirrored) and then 8 x 45° offset!" \n
  "!! /expand,18,axis,,,10 !axis symmetry 180° expansion" \n
  "!! /expand !switch off expansion" \n
  "!! /dist,,1/2,1 !enlarge twice" \n
  "!! " \n
  \n
  "set,last" \n
  "/efacet,2" \n
  "plnsol,u,sum !,2 !2:overlay undeformed edges" \n
  \n
  "!! --- Birth & Death --- " \n
  "!! etable,strain,epto,1" \n
  "!! esel,s,etab,strain,0.02" \n
  "!! /solu" \n
  "!! antype,,rest"\n
  "!! ekill,all"\n
  "!! esel,all" \n
  \n
  "!! --- Reactions" \n
  "Ls=1" \n
  "set,Ls" \n
  "*get,Ns,active,,solu,ncmss !number of substeps" \n
  "*get,Dim,parm,Reaction,dim,x" \n
  "*if,Dim,le,1,then" \n
  "*dim,Reaction,array,Ns,1" \n
  "*endif" > \n
  "*do,I,1,Ns" \n
  "set,Ls,I" > \n
  "fsum" \n
  "*get,Fx,fsum,,item,fx" \n
  "Reaction(I)=Fx" \n
  "*enddo" > \n
  "/gcolumn,1,'Reaction'" \n
  "/axlab,x,Substep" \n
  "/axlab,y,Force in N" \n
  "/gropt,fill,1 ! fill curves" \n
  "*vplot,,Reaction" \n
  \n
  "!! --- Animations ---" \n
  "/seg,multi,process,0.15 !process.avi, delay .15" \n
  "Ls=1 !Loadstep 1" \n
  "!antime," \n
  "!andata," \n
  "set,LS" \n
  "*get,Ns,active,,solu,ncmss !number of substeps" \n
  "*do,I,1,Ns" \n
  "set,Ls,I" > \n
  "plnsol,s,eqv" \n
  "*enddo" > \n
  "/seg,off,process,.15" \n
  \n
  "!! --------- etables ----------" \n
  "!! etables don't take into account higher element order!"
  "!! ---- Mohr-Coulomb failure criterion" \n
  "Z1 = 60 !tensile strength" \n
  "Z3 = 160 !compressive strength" \n
  "etable,S1,s,1" \n
  "etable,S3,s,3" \n
  "sadd,R,S1,S3,1/Z1,-1/Z3" \n
  "pletab,R,avg !avg: average over nodes" \n
  "esort,etab,R" \n
  "*get,Mc,etab,sort,,max" \n
  "*msg,,Mc" \n
  "Mohr-Coulomb criterion (< 1): %G" \n
  \n
  "!! --- multiple graphics windows" \n
  "/window,1,rtop" \n
  "/window,2,ltop" > \n
  "/window,3,lbot" > \n
  "/window,4,rbot" > \n
  "/gtype,all,node,0 !switch off node display" > \n
  "/gcmd,1,pletab,s1" \n
  "/gcmd,2,pletab,s3" > \n
  "/gcmd,3,pletab,r,avg" > \n
  "/gcmd,4,plvect,s" > \n
  "gplot" > \n
  \n
  "/window,1,full" \n
  \n
  "!! --- cross section by working plane ---" \n
  "/cplane,1 !1:cutting plane is x-y-wp" \n
  "wpcsys,1,11 !align wp with specified c-sys" \n
  "wpoffs,,-100" \n
  "wprota,0,90,0 !z,x,y axis rotation" \n
  \n
  "/type,1,zcap ! z-buffered capping" \n
  "!! /type,1,zqsl ! z-bufferd capping with outlines" \n
  "!! /type,1,basic !switch off cross sections" \n
  \n
  "!! ------------------------------" \n
  "!" ansys-outline-string
  ansys-outline-string " --- Time-History Postprocessing ---" \n
  "!! ------------------------------" \n
  \n
  "/post26" \n
  "!! esol,2,1,,u,z,'displ z'" \n
  "nsol,2,1,u,z" \n
  "rforce,3,1,f,z" \n
  "!! add,4,2,,,displ,,,-1" \n
  "/grid,1" \n
  "/gmarker,1,1 !curve marking: 1: triangles,2: squares" \n
  "!! /xrange,0,1" \n
  "!! /xrange,default" \n
  "!! /yrange,0,1" \n
  "!! /axlab,x,x" \n
  "!! /axlab,y,y" \n
  "!! timerange,0,1" \n
  "!! /title,bla" \n
  "!! /stitle,,blabla !subtitle line 1" \n
  "!! /stitle,2,blabla !subtitle line 2" \n
  "!! /tlable,x,y,bla !annotation at (x,y)" \n
  "xvar,2" \n
  "!! invert background colour" \n
  "!/RGB,index,100,100,100,0" \n
  "!/RGB,index,0,0,0,15" \n
  "!/show,png !creates jobnameXXX.png files" \n
  "plvar,3" \n
  "!/show,close" \n
  "!!prvar,3" \n
  \n
  "!! ------------------------------" \n
  "!" ansys-outline-string
  ansys-outline-string " --- Time Stat Processing ---" \n
  "!! ------------------------------" \n
  \n
  "*get,Walldone,active,,time,wall" \n
  "Preptime=(Wallasol-Wallstrt)*60" \n
  "Solvtime=(Wallbsol-Wallasol)*60" \n
  "Totaltim=(Walldone-Wallstrt)" \n
  "*msg,ui,Preptime,Solvtime,Totaltim" \n
  "Time in min for preprocessing: %G %/ &" \n
  "Time in min for Solving: %G %/ &" \n
  "Total time in h: %G %/ &" \n
  "=== End of timing messages ===" \n
  \n)


;; (defmacro define-ansys-skeleton (command documentation &rest definitions) ;FIXME: documentation
;;   "Define COMMAND with an optional docstring DOCUMENTATION.
;; to insert statements as in DEFINITION ...  Prior
;; DEFINITIONS (e.g. from ~/.emacs) are maintained.  Each definition
;; is built up as (format PROMPT ELEMENT ...).  Alternately a
;; synonym definition can be (format . PREVIOUSLY-DEFINED-FORMAT).

;; For the meaning of (PROMPT ELEMENT ...) see `skeleton-insert'.
;; Each DEFINITION is actually stored as
;; 	(put COMMAND format (PROMPT ELEMENT ...)), which you can
;; also do yourself."
;;   (unless (stringp documentation)
;;     (setq definitions (cons documentation definitions)
;; 	  documentation ""))
;;   ;; The compiled version doesn't work.
;;   (require 'backquote)
;;   (`(progn
;;       (let ((definitions '(, definitions)))
;; 	(while definitions
;; 	  ;; skeleton need not be loaded to define these
;; 	  (or (get '(, command) (car (car definitions)))
;; 	      (put '(, command) (car (car definitions))
;; 		   (if (symbolp (cdr (car definitions)))
;; 		       (get '(, command) (cdr (car definitions)))
;; 		     (cdr (car definitions)))))
;; 	  (setq definitions (cdr definitions))))
;;       (defun (, command) ()
;; 	(, documentation)
;; 	(interactive)
;; 	(skeleton-insert
;; 	 (or (get '(, command) ansys-format)
;; 	     (error "%s statement syntax not defined for ansys format %s"
;; 		    '(, command) ansys-format)))))))

;; (define-ansys-skeleton ansys-if
;;   "Insert an if statement in the current format's syntax."
;;   (format "Value/Parameter 1: "
;; 	  "*IF," str ","
;; 	  (read-string "Operation: (EQ,NE,LT,GT,LE,GE,ABLT,ABGT) ")
;; 	  ","
;; 	  (read-string "Value/Parameter 2: ")
;; 	  ","
;; 	  (read-string "Action: (:label,STOP,EXIT,CYCLE,THEN) ")
;; 	  \n >_
;; 	  "*ENDIF"
;; 	  \n)
;;   (mac . format))


(define-skeleton ansys-if
  "Insert interactively an *if .. *endif construct."
  "Value/Parameter 1 [I]: "
  "*if," str | "I" ","
  (completing-read "Operand [eq] (use TAB to complete): "
		   '("eq" "ne" "lt" "gt" "le" "ge" "ablt" "abgt")
		   nil			;predicate
		   t			;require-match
		   nil			;inital input
		   nil			;history
		   "eq"			;default
		   )
  ","
  (skeleton-read "Value/Parameter 2 [J]: ") | "J"
  ","
  (completing-read "Action [then] (use TAB to complete): "
		   '(":label" "stop" "exit" "cycle" "then")
		   nil
		   t
		   nil
		   nil
		   "then") > \n
  - \n
  "*endif" > \n
  \n)


;; (define-ansys-skeleton ansys-if-then
;;   "Insert an if statement in the current format's syntax."
;;   (format "Value/Parameter 1: "
;; 	  "*IF," str ","
;; 	  (read-string "Operation: (EQ,NE,LT,GT,LE,GE,ABLT,ABGT) ")
;; 	  ","
;; 	  (read-string "Value/Parameter 2: ")
;; 	  ",THEN" \n
;; 	  > _ \n
;; 	  ("*ELSEIF? %s: "
;; 	   > "*ELSEIF," str ","
;; 	   (read-string "Operation: (EQ,NE,LT,GT,LE,GE,ABLT,ABGT) ")
;; 	   ","
;; 	   (read-string "Next Value/Parameter: ")
;; 	   ",THEN" \n
;; 	   > \n)
;; 	  "*ELSE" > \n
;; 	  > \n
;; 	  "*ENDIF" > \n)
;;   (mac . format))

(define-skeleton ansys-if-then
  "Insert an *if,then .. (*elseif .. *else ..) *endif construct."
  "Value/Parameter 1 [I]: "
  "*if," str | "I" ","
  (completing-read "Operand [eq] (use TAB to complete): "
		   '("eq" "ne" "lt" "gt" "le" "ge" "ablt" "abgt")
		   nil			;predicate
		   t			;require-match
		   nil			;inital input
		   nil			;history
		   "eq"			;default
		   )
  ","
  (skeleton-read "Value/Parameter 2 [J]: ") | "J"
  ","
  (completing-read "Action [then] (use TAB to complete): "
		   '(":label" "stop" "exit" "cycle" "then")
		   nil
		   t
		   nil
		   nil
		   "then") > \n
  - \n
  ;; * elsif subskeleton
  ("*elseif construct(s)? Value/Paramter 1: (%s) "
    > "*elseif," str ","
    (completing-read
     "Operand [eq] (ne, lt, gt, le, ge, ablt, abgt, use TAB to complete): "
     '("eq" "ne" "lt" "gt" "le" "ge" "ablt" "abgt")
     nil			;predicate
     t				;require-match
     nil			;inital input
     nil			;history
     "eq"			;default
     )
    ","
    (read-string "Next Value/Parameter 2: ")
   ",then" _ \n >
     \n) 			;-- e o subskeleton
  ;; else subskeleton
  '(if (y-or-n-p "*else construct? ")
      (insert "*else\n\n"))_ >
  "*endif" > \n)

;; (define-ansys-skeleton ansys-do
;;   "Insert an if statement in the current format's syntax."
;;   (format "Parameter: "
;; 	  "*DO," str ","
;; 	  (read-string "Start Value/Parameter: ")
;; 	  ","
;; 	  (read-string "Finish Value/Parameter: ")
;; 	  ","
;; 	  (read-string "Increment Value/Parameter: ") \n
;; 	  > _ \n
;; 	  "*ENDDO" > \n)
;;   (mac . format))

(define-skeleton ansys-do
  "Insert interactively a *do .. *enddo loop."
  "Loop parameter [I]: "
  "*do," str | "I" ","
  (read-string "Start Value/Parameter [1]: ") | "1"
  ","
  (read-string "Finish Value/Parameter: ")
  ","
  (read-string "Increment Value/Parameter [1]: ") | "1" \n
  > _ \n
  "*enddo" > \n
)

;; (define-ansys-skeleton ansys-mp		;FIXME: skeleton a bit over the top
;;   "Insert an if statement in the current format's syntax."
;;   (format "Material Property: (EX,ALPX,PRXY,NUXY,GXY,DAMP,MU,DENS,KXX) "
;; 	  "MP," str ","
;; 	  (read-string "Material Number: ")
;; 	  ","
;; 	  (read-string "Constant Value: ")
;; 	  ","
;; 	  (read-string "Linear Coefficient? : ")
;; 	  ","
;; 	  (read-string "Quadratic Coefficient? : ")
;; 	  ","
;; 	  (read-string "Cubic Coefficient? : ")
;; 	  ","
;; 	  (read-string "Quartic Coefficient? : ")
;; 	  \n)
;;   (mac . format))

(define-skeleton ansys-mp		;FIXME: skeleton a bit over the top
  "Insert interactively an mp statement."
  "Material Property: (EX,ALPX,PRXY,NUXY,GXY,DAMP,MU,DENS,KXX) "
  "MP," str ","
  (read-string "Material Number: ")
  ","
  (read-string "Constant Value: ")
  ","
  (read-string "Linear Coefficient? : ")
  ","
  (read-string "Quadratic Coefficient? : ")
  ","
  (read-string "Cubic Coefficient? : ")
  ","
  (read-string "Quartic Coefficient? : ")  \n
  \n
  )

;; Local Variables:
;; mode: outline-minor
;; indicate-empty-lines: t
;; show-trailing-whitespace: t
;; word-wrap: t
;; End: