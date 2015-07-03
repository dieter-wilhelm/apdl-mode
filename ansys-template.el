;;; ansys-template.el -- APDL code templates for the ANSYS-Mode

;; Copyright (C) 2006 - 20015  H. Dieter Wilhelm GPL V3

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Maintainer: H. Dieter Wilhelm
;; Version: 16.1.1
;; Keywords: Languages, Convenience, ANSYS

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

;;; Comment:
;; Convention used for outlining
;; !@ is surrounded by 30 equal signs  ==============================
;; !@@ by 30 dashes ------------------------------
;; !@@@ by 30 dots ..............................
;; and empty lines

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
  "Display or insert code templates.
With an argument ARG not equal to 1 insert the template into the
current buffer instead of previewing it in a separate window.
You might trigger a completion of templates with the <TAB> or <?>
key and choose with the mouse 2 button."
  (interactive "p")
  (let* (
	 (old-buffer (buffer-name))
	 (new-buffer-name "*ANSYS-skeleton*")
	 (skeleton-buffer
	  (get-buffer-create new-buffer-name))
	 s  ;yellow indicator line in the preview buffer above content
	 ;; if skeleton window is visible in selected frame
	 (visible  (get-buffer-window new-buffer-name nil))
	 (skel-string
	  ;; we might want to insert it while previewing...
	  (if (and (not (= arg 1)) ansys-last-skeleton visible)
	      ansys-last-skeleton
	    "ansys-skeleton-"))
	 (skel
	  (if (= arg 1)
	      (completing-read "Preview template: "
			       obarray 'commandp t skel-string nil)
	    (completing-read "Insert template: "
			     obarray 'commandp t skel-string nil)))
	 )
    (setq ansys-last-skeleton skel)
    (cond ((= arg 1)
	   (switch-to-buffer-other-window skeleton-buffer)
	   (setq buffer-read-only nil)
	   (remove-overlays)		;from beginnin and end of buffer
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
		    (concat "-*- ANSYS template: "
			    skel " -*-\n") 'face 'match))
	   (overlay-put ansys-skeleton-overlay 'before-string s)
	   (set-buffer-modified-p nil)
	   (setq buffer-read-only t)
	   (switch-to-buffer-other-window old-buffer)
	   )
	  (t
	   (funcall (intern-soft skel)))
	  )))

(define-skeleton ansys_do
  "Insert a *do .. *enddo loop."
  nil
  "*do,I,1,10,1" > \n
  - \n
  "*cycle !bypass below commands in *do loop" > \n
  "*enddo" > \n
  )

(define-skeleton ansys_if
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
  "!! .............................." \n
  "!@@@ - branching, looping and control structures -" \n
  "!! .............................." \n
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
  "*repeat,5,0,1" \n
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

(define-skeleton ansys-skeleton-header
  "Insert a file header for an APDL script.
Together with an Emacs Time-stamp string.  You might update the
time stamp with the Emacs command M-x `time-stamp'."
  "Brief description of the file: "
  "!@ ==============================" \n
  "!" ansys-outline-string " ---  Header ---" \n
  "!@ ==============================" \n
  ;; "!! FILENAME: " (file-name-nondirectory (if (buffer-file-name)
  ;; 					      (buffer-file-name)
  ;; 					    (buffer-name))) \n
  "!! Time-stamp: <" (current-time-string) ">" \n
  "!! ANSYS VERSION: " ansys-current-ansys-version \n
;;  "!! UNITS: mm-t-s" \n
  "!! NOTE: " str \n
  "!! ------------------------------" \n
  \n
  "! fini" \n
  "! /clear" \n
  "! y" \n
  "/units,mpa !indicate mm-t-s unit system" \n
  \n
  )

(define-skeleton ansys-skeleton-information
  "Information gathering with APDL commands."
  nil
  ;; "!@ ------------------------------" \n
  ;; "!@@ -- informations --" \n
  ;; "!! ------------------------------" \n
  "!! ------------------------------" \n
  "!@@@ --- measurements ---" \n
  "!! ------------------------------" \n
  \n
  "nx(NodeNo)|y|z ! x|y|z-coordinate of node NodeNo " \n
  "kx(KPoint)|y|z ! x|y|z-coordinate of KP KeyPoint " \n
  "distnd(N1,N2) ! distance between nodes" \n
  "distkp(k1,k2) ! distance between keypoints" \n
  "disten(e,n) ! distance between element centroid and node" \n
  \n
  "! -------------------------------" \n
  "!@@@ - Center of mass, mass, and mass moments of inertia -" \n
  "!! ------------------------------" \n
  \n
  "!! --- partial solution for mass calculation ---" \n
  "/solu !for psolve" \n
  "outpr,basic,all" \n
  "irlf,-1! inertia relief option" \n
  "/output,mass_output,txt" \n
  "psolve,elform !only partial solution" \n
  "psolve,elprep" \n
  "/output" \n
  "*list,mass_output,txt"\n
  "irlist ! print inertia relief summary" \n
  \n
  "/prep7 !for gsum" \n
  "gsum !for selected entities: combination of ksum, lsum, asum and vsum" \n
  "!mass, centroids, moments of inertia, length, area, volumen, ..." \n
  "*get,bla,area,0,imc,y !moment of inertia about y w.r.t. mass centroid" \n
  \n
  "!@ ------------------------------" \n
  "!@@@ - job items -" \n
  "!! ------------------------------" \n
  \n
  "/inquire,Job_name,jobname!get string array jobname|directory|user|psearch" \n
  "*stat,Job_name(1)" \n
  "/com,This is the jobname: \"%Job_name(1)%\"" \n
  "/inquire,Dirname,directory" \n
  "*stat,Dirname(1)" \n
  "/inquire,param,date,file,ext !get date(size,lines) of file.ext" \n
  \n
  "!! .............................." \n
  "!@@@ - statuses -" \n
  "!! .............................." \n
  \n
  "/status ![all],capabilities: title,units,mem,db,config,global,solu,prod" \n
  "*list,file,ext ! list file content" \n
  "/runst ! enter run statistics processor" \n
  "/pstatus ! display window stats specifications" \n
  "list: k-,l-,a-,v-,n-,e-,ce-,cp-,mp-,f-,bf-,d-,da-,dk-,dl-,fk-,af-,sf-,sfl-,bfa-,bfe-,bfk-,bfl-,bfv-,ic-,r-,tb-,s-,m-,sw-" \n
  \n
  "*status ! parameters, arrays and abbreviations" \n
  "*status,_STATUS !return value: 0:no error, 1:note, 2:warning, 3:error" \n
  "*stat,argx !list all local ARGx values" \n
  "*status,_RETURN !some solid modelling commands return this parameter" \n
  "!(see the _return value list below)" \n
  "*vstat !status on arry operations" \n
  \n
  "!! .............................." \n
  "!@@@ - material info" \n
  "!! .............................." \n
  \n
  "*get,Density,dens,ID,temperature !get the properties of material ID" \n
  "mplist,all" \n
  "tblist" \n
  "tbplot,biso,1" \n
  \n
  "!! .............................." \n
  "!@@@ - geom info" \n
  "!! .............................." \n
  \n
  "/prep7" \n
  "lsum !or gsum" \n
  "*get,LLen,line,,leng !combined length of all selected lines" \n
  "asum !or gsum" \n
  "*get,Area,area,,area !combined area of all selected areas" \n
  "vsum !or gsum" \n
  "*get,Volume,volu,,volu !combined volume of all selected volumes" \n
  \n
  "!! .............................." \n
  "!@@@ - solution info -" \n
  "!! .............................." \n
  \n
  "set,list ! list a summary of each load step" \n
  "wpstyl,stat !working plane status" \n
  "status,solu" \n
  \n
  "!! .............................." \n
  "!@@@ - postproc info -" \n
  "!! .............................." \n
  \n
  "*get,NS,active,,set,nset !No of load steps" \n
  "*get,T,active,,set,time !Time of current set" \n
  \n
  "!! .............................." \n
  "!@@@ - \"stat\" database settings - " \n
  "!! .............................." \n
  \n
  "/prep7" \n
  "etype" \n
  "stat ! load step options" \n
  "rcon$stat" \n
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
  "!! post26" \n
  "extrem,2 !post26 variable extrema listing" \n
  "*get,Extr,vari,2,extrem,vmax! get max extreme value" \n
  "*get,Extr,vari,2,extrem,vmin! get min extreme value" \n
  "!! post26 stat topics" \n
  "DEFINE $ stat ! - Data definition settings" \n
  "OPERATE $ stat ! - Operation data" \n
  "PRINT $ stat ! - Print settings" \n
  "PLOTTING $ stat ! - Plotting settings" \n
  \n
  "!! .............................." \n
  "!@@@ - aux3 result file edit routine -" \n
  "!! .............................." \n
  \n
  "/aux3" \n
  "list !result statistics" \n
  "!! .............................." \n
  "!@@@ - *get -" \n
  "!! .............................." \n
  \n
  "*get,bla,active,,mat![|csys|type|real|esys]" \n
  \n
  "*status,_RETURN ! command, documentation, _return value" \n
  "!Keypoints -" \n
  "K ! Defines a keypoint - keypoint number" \n
  "KL ! Keypoint on a line - Keypoint number" \n
  "KNODE ! Keypoint at node - Keypoint number" \n
  "KBETW ! Keypoint between two keypoints - KP number" \n
  "KCENTER ! Keypoint at center - KP number" \n
  "!Lines -" \n
  "BSPLIN ! Generate spline - Line number" \n
  "CIRCLE ! Generate circular arc lines - First line number" \n
  "L ! Line between two keypoints - Line number" \n
  "L2ANG ! Line at angle with two lines - Line number" \n
  "LANG ! Line tangent to two lines - Line number" \n
  "LARC ! Defines a circular arc - Line number" \n
  "LAREA ! Line between two keypoints - Line number" \n
  "LCOMB ! Combine two lines into one - Line number" \n
  "LDIV ! Divide line into two or more lines - First keypoint number" \n
  "LDRAG ! Line by keypoint sweep - First line number" \n
  "LFILLT ! Fillet line between two liens - Fillet line number" \n
  "LROTAT ! Arc by keypoint rotation - First line number" \n
  "LSTR ! Straight line - Line number" \n
  "LTAN ! Line at end and tangent - Line number" \n
  "SPLINE ! Segmented spline - First line number" \n
  "!Areas -" \n
  "A ! Area connecting keypoints - Area number" \n
  "ACCAT ! Concatenate two or more areas - Area number" \n
  "ADRAG ! Drag lines along path - First area number" \n
  "AFILLT ! Fillet at intersection of two areas - Fillet area number" \n
  "AL ! Area bounded by lines - Area number" \n
  "ALPFILL ! All loops - Area number" \n
  "AOFFST ! Area offset from given area - Area number" \n
  "AROTAT ! Rotate lines around axis - First area number" \n
  "ASKIN ! Skin surface through guiding lines - First area number" \n
  "ASUB ! Area using shape of existing area - Area number" \n
  "!Volumes - " \n
  "V ! Volume through keypoints - Volume number" \n
  "VA ! Volume bounded through areas - Volume number" \n
  "VDRAG ! Drag area pattern to create volume - First volume number" \n
  "VEXT ! Volume by extruding areas - First volume number" \n
  "VOFFST ! Volume offset from given area - Volume number" \n
  "VROTAT ! Volume by rotating areas - First volume number" \n
)

(define-skeleton ansys-skeleton-configuration
  "Configuration skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- configurations --" \n
  "!! ------------------------------" \n
  \n
  "*afun,rad ![rad],deg" \n
  "Pi = acos(-1)" \n
  "*afun,deg !deg: trig. functions accept and return angle arguments" \n
  "True = 1"  \n
  "False = 0" \n
  "/mplib,read,YourLibraryPath !define material library path" \n
  "/mplib,stat !stat defined material library paths" \n
  "/units,MPA  !I'm using mostly MPA (Tmms) (important only for material libs)" \n
  "mpread,steel_elastic,,,lib" \n
  \n
  "!! --- Directory and file names -- " \n
  "*dim,Dir,string,248 ! string array with maximum of 248 characters!" \n
  "Dir(1) = '/very/very/very/long/path/to/heaven/'" \n
  "/cwd,/tmp !change the current working dir." \n
  "filnam,bla !change the jobname" \n
  "resume! resume the database" \n
  "file !result file" \n
  "/title," _ \n
  "!! --- display options ---" \n
  "/plopts,wp ! switch off working plane" \n
  "/plopts,wp,1 !display working plane" \n
  "/plopts,minm,0 !0: switch off min max" \n
  "/triad,rbot !off, orig, ltop, ..." \n
  "!! --- graphics ---" \n
  "/gfile,1200 !set height resolution [800] to 1200, width=1.33*height" \n
  "/graphics,power" \n
  "/efacet,4" \n
  "/type,4 !better hidden line removal" \n
  \n
  )

(define-skeleton ansys-skeleton-view-settings
  "View settings skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- view settings --" \n
  "!! ------------------------------" \n
  \n
  "/color,wbak,whit !white background on plot window" \n
  "/RGB,index,100,100,100,0" \n
  "/RGB,index,0,0,0,15" \n
  "immed,1 !immediate display of generated geom. in /prep7" \n
  "/graphics,power" \n
  "/uis,replot,0 !suppress automatic replot" \n
  \n
  "!! -- views --" \n
  "/view,,1,1,1 !viewing direction vector [0,0,1]"_ \n
  "/view,,wp !view normal to working plane" \n
  "/focus,1 $ /repl !focus wn 1 to csys,0" \n
  "!/focus,1,,.5,,1 $ /repl !focus with screen coordinate multiplier" \n
  "/angle,1,10,xs,1!rotation {x,y,z}m global {x,y,z}s screen 1:cumulative 0: absolut" \n
  "/dist,1,1/2.,1 $ /repl !1/2:distance (zoom) to object <1 nearer/larger,1:use multiplier" \n
  "/dscale,all,10 !set displacment multiplier" \n
  "/zoom,1,off ! refit image to window" \n
  "/zoom,1,rect,0,0,1,1 ! fit image to rectangel X1,Y1 & X2,Y2" \n
  "/auto ! automatic fit mode" \n
  "/user ! keep last display scaling" \n
  \n
  "!! -- style options ---" \n
  "/device,text,1,140 ! enlarge 140 % the text size" \n
  "/plopts,minm !  switch off[on] (both!) min & max" \n
  "/plopts,info,off !switch off all descriptive text" \n
  "/plopts,wp,1 !display working plane" \n
  "/plopts,wp,off !switch off wp" \n
  "/plopts,frame,off !switch off graphics frame" \n
  "/plopts,logo,off !switch off ANSYS logo" \n
  "/plopts,date,1 !show only date and not time" \n
  "/plopts,title,off !switch of title display" \n
  "!! --- WB like legend display --- " \n
  "/plopt,leg3,on! contour section of legend" \n
  "/plopt,leg1,on! legend header" \n
  "/plopt,info,3! show legend info on the left side" \n
  "/udoc,,cntr,bottom !show legend on bottom" \n
  "/pstatus ! display window stats specifications" \n
  \n
  "!! -- wp & coordinate systems display"
  "/psymb,cs,1 ! display local coord." \n
  "/psymb,ndir,1 ! display (only) rotated nodal coordinates" \n
  "/plopts,wp,1 !display working plane" \n
  "/plopts,wp,off !switch off wp" \n
  "/triad,off !orig,lbot,rbot,ltop,rtop" \n
  "/triad,rbot" \n
  "/triad,off" \n
  \n
  "!! .............................." \n
  "!@@@ - translucency/transparency -" \n
  "!! .............................." \n
  \n
  "/trlcy,elem,.5,all !make all selected elements translucent" \n
  \n
  "!! .............................." \n
  "!@@@ - material display -" \n
  "!! .............................." \n
  \n
  "/number,1 !0:colours & numbers,1:colours,2:numbers" \n
  "/pnum,mat,1 !1: turn on numbering" \n
  "!NODE ELEM MAT TYPE REAL LOC SVAL ESYS KP LINE AREA VOLU STAT TABN  SEC DOMA DEFA" \n
  "/pnum,defa !1: turn off any numbering" \n
  \n
  "!! .............................." \n
  "!@@@ - element shape display -" \n
  "!! .............................." \n
  \n
  "/efacet,2! display 2 element facets (curvature) with power graphics" \n
  "/eshape,1 !1:use real constant def. for element shapes" \n
  "/graphics,power !for post1 results" \n
  \n
  "!! .............................." \n
  "!@@@ - multi window plots -" \n
  "!! .............................." \n
  "/window,2,dele" \n
  "/window,1,dele" \n
  "/erase" \n
  "/window,2,-1,-.5,.5,1 !from -1 to 1 is the full screen" \n
  "aplot" \n
  "/window,2,off" \n
  "/noerase" \n
  "!/window,1,rbot !from -1 to 1 is the full screen" \n
  "/window,1,full !from -1 to 1 is the full screen" \n
  "!/window,3,rbot !full,righ,top,bot,ltop,lbot,rtop,rbot" \n
  "eplot" \n
  "/window,2,on" \n
  "/erase" \n
  \n
  "!! .............................." \n
  "!@@@ - countours in legend -" \n
  "!! .............................." \n
  "!X11 or WIN32 and to 128 for X11c or WIN32C " \n
  "/contour,,!wn,ncont[9],vmin,vinc,vmax" \n
  "! 3-D device" \n
  "/dv3d,contr,off![on]" \n
  "/contour,,!wn,ncont[128],vmin,vinc,vmax" \n
  \n
  "!! .............................." \n
  "!@@@ - cutting planes and power graphics -" \n
  "!! .............................." \n
  \n
  "/graphics,power !power (surface) graphics" \n
  "/shade,,0 !bug in 14.5, shouldn't be necessary" \n
  "/type,,zcap !capped z-buffered" \n
  "/type,,zqsl !sliced z-buffered" \n
  "/gline,,1 !elem outlines [0] solid, 1 dashed, -1 no outl." \n
  \n
  "!! .............................." \n
  "!@@@ - mesh line display -" \n
  "!! .............................." \n
  \n
  "/edge,,1 !1:display elements in contour plots" \n
  "/edge,,0 !0:switch off display of elements in contour plots" \n
  \n
  "!! .............................." \n
  "!@@@ - coordinate system display -" \n
  "!! .............................." \n
  \n
  "csys ![0]:cartesian, 1:cylindrical, 2:spherical, 3:toroidal, 4:wp" \n
  "clocal,11,0 !define local coord. sys. from active" \n
  "/psymb,cs,1 ! display local coord." \n
  "/psymb,ndir,1 ! display (only) rotated nodal coord." \n
  "/plopts,wp,off !switch off wp" \n
  "/triad,rbot"_ \n
  "/triad,off" \n
  \n
  )

(define-skeleton ansys-skeleton-import
  "Import commands."
  nil
  "!! ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string " -- cad import -- " \n
  \n
  "/aux15" \n
  "ioptn,iges,nodefeat" \n
  "ioptn,merge,yes" \n
  "ioptn,solid,yes" \n
  "ioptn,small,yes" \n
  "ioptn,gtoler,defa" \n
  "igesin,'test','iges'" \n
  \n
  "/input,filename,anf ! for APDL based input" \n
  "/facet,fine" \n
  "/title,new title" \n
  \n
  "!! read array from file: Variable of *vread must be dimensioned!" \n
  "/inquire,Numlines,LINES,'%Dir(1)%ground_plate_temperature',csv" \n
  "*dim,bla,Numlines,1" \n
  "!! lines might be skipped with the SKIP parameter" \n
  "*vread,Bla,file,ext,,ijk,Numlines,2,,skip" \n
  "(E10.3)"  \n
  "*vplot,Bla(1,1),Bla(1,2) "
  "! works only with FORTRAN not C specifiers!" \n
  \n
  "!!read table from file: Variable of *tread must be dimensioned!" \n
  "!!*tread: tab-delimited, blank-delimited, or comma-delimited file format" \n
  "/inquire,Numlines,LINES,'%Dir(1)%ground_plate_temperature',csv" \n
  "*dim,Hans,table,Numlines,4 !column and row 0 must not be dimensioned!" \n
  "!! lines might be skipped with the SKIP parameter" \n
  "*tread,Hans,tmp,out,,skip !the value Hans(0,0) must be smaller then Hans(0,1) and Hans(1,0)!" \n
  "*vplot,Hans(1,0),Hans(1,1),2,3" \n
  \n
  )

(define-skeleton ansys-skeleton-expand
  "Symmetry expansion."
  nil
  "!! ------------------------------" \n
  "!@@@ - symmetry expansion -" \n
  "!! ------------------------------" \n
  \n
  "!/EXPAND, Nrepeat1, Type1, Method1, DX1, DY1, DZ1, Nrepeat2, Type2, Method2, DX2, DY2, DZ2, Nrepeat3, Type3, Method3, DX3, DY3, DZ3" \n
  "!DX1,DY1,DZ1,... 1.) normal vector of reflection plane 2.) increments between patterns" \n
  "! full: no tranlation<-small nonzero value, half: mirroring, increment is doubled" \n
  "/expand,2,(l)rect,half,,-1e-6,,2,rect,half,-1e-6 !(local) cartesian, half:mirror" \n
  "/expand,8,(l)polar,half,,45 !(local) polar expansion, full:normal exp." \n
  "/expand,18,axis,,,10 !axisymmetric (180 ° rot. around y-axis)" \n
  "/expand !switch off expansion" \n
  "!! -- cyclic expansion --" \n
  "cyclic,status" \n
  "/cycexpand ! expand graphics rep." \n
  \n
  )

(define-skeleton ansys-skeleton-contact-definition
  "Contact definitons skeleton."
  nil
  "!@ -------------------------------" \n
  "!@@ -- General contact definitions (since Ansys 16) --" \n
  "!! -------------------------------" \n
  \n
  "!! 2D and 3D elements are automatically selected!" \n
  "gcgen ! contacts on all exterior element faces" \n
  "gcgen,,,,,select! only contacts for selected elements!" \n
  "gcgen,,,,part! sectionIDs only for parts!" \n
  "gcdef, Option, SECT1, SECT2, MATID, REALID" \n
  "tb,inter,MatID,,,bonded !contact interaction" \n
  "mp,mu,MatID,.1 !friction " \n
  "!! rigid targets without real constants are allowed with general contact definitons!" \n
  "ET,100,170,,1" \n
  "TYPE,100" \n
  "SECNUM,101" \n
  "REAL,0"  \n
  "MAT,0" \n
  "keyopt,gcn,2,3! keyopt for ALL general contacts" \n
  "gcgen,select !select all general contacts and deselect others!" \n
  "elist !get sectionID and typeID" \n
  "/pnum,sect,on !display section type" \n
  \n
  "!@ -------------------------------" \n
  "!@@ -- Contact pair definitions --" \n
  "!! -------------------------------" \n
  \n
  "Contact=10"_ \n
  "Target=Contact+1" \n
  "Mu = 0.1 !friction factor" \n
  "r,Contact !define a real set" \n
  "et,Contact,conta174 !3d, 8 node" \n
  "et,Contact,conta173 !3d, 4 node" \n
  "et,Contact,conta172 !2d, 3 node" \n
  "et,Contact,conta171 !2d, 2 node" \n
  "et,Contact,conta175 !2/3d node to surf" \n
  "et,Contact,conta176 !3d line to line, 3 node" \n
  "et,Contact,conta177 !3d line to surf, 3 node" \n
  "!! --- targets ---" \n
  "et,Target,targe170  !3d area,line,(pilot-)node" \n
  "et,Target,targe169  !2d" \n
  \n
  "!! --- contact options --" \n
  "keyo,Contact,2,1 !ALGORITHM [0]:augm. Lagrange,1:penalty,2:MPC,4:pure Lagrange" \n
  "!! " \n
  "Fkn = .1 !contact stiffness (default 1, divided by 100 if plastic mat. ONLY ANSYS version < 12.0!)" \n
  "rmodif,Contact,3,Fkn !FKN:normal penalty stiffness factor (default:1) smaller: bigger penetration, easier convergence" \n
  "!rmod,Contact,12,0. !FKT:tangent stiffness factor,0:means 1 for ANSYS!!!" \n
  \n
  "!Ftoln = .1 !penetration tolerance [.1] for lagr. mult. & chattering control" \n
  "!rmod,Contact,4,Ftoln !FTOLN penetration tolerance (augm. Lagrance! default:0.1) bigger: less chattering" \n
  \n
  "!Pinb = -0.1 !search radius, neg: absolut value ( > CNOF!!!!)" \n
  "!rmod,Contact,6,Pinb !PINB:pinball radius (negative: no scaling:absolute distance)" \n
  \n
  "!ICONT = -0.05 !initial contact closure [0] relative band size (neg. absolut)" \n
  "!rmod,Contact,5,Icont !ICONT:amount of initial contact closure (positiv:penetration)" \n
  \n
  "!CNOF = 0 !contact surface offset (complete shift) ([0], neg.: penetr.)" \n
  "!rmod,Contact,10,Cnof !CNOF (thickness effects):contact normal offset (e.g. beams)" \n
  \n
  "!keyo,Contact,4,0 !keyo(4): location of contact detection" \n
  "    !! [0]:Gauss points, 3(V13):surface projection method" \n
  "!keyo,Contact,5,4 !EFFEKT of CNOF (surface offset) or ICONT (node movement in a band)" \n
  "    !! 0: no adjustm." \n
  "    !! 1: close gap with auto CNOF" \n
  "    !! 2: reduce penetr. w. auto CNOF" \n
  "    !! 3: close gap/red. pene. w. auto CNOF" \n
  "    !! 4: auto ICONT" \n
  "!keyo,Contact,9,1 ! corresponds to Adjust To Touch in WB" \n
  "keyo,Contact,9,4 !HANDLING of initial penetration/gap and CNOF" \n
  "    !! 0: include everything" \n
  "    !! 1: remove everything" \n
  "    !! 2: include everyth. ramped" \n
  "    !! 3: include offset only" \n
  "    !! 4: incl. offset only, ramped" \n
  "keyo,Contact,10,2 !Stiffness UPDATE,[0]:each LS,2:each NR iteration,1:each substep" \n
  "!keyo,Contact,11,1 !SHELL thickness effect" \n
  "keyo,Contact,12,0 !BEHAVIOUR,[0]:frictional/-less,1:rough,2:no separation,3:bonded" \n
  "real,Contact" \n
  \n
  "!rmod,Contact,11,-1 !FKOP contact opening stiffness & contact damping, must be neg." \n
  \n
  "Mu = 0.1 !Mu is the friction factor" \n
  "!Mu = 0 ! frictionless" \n
  "mp,mu,Contact,Mu" \n
  "mat,Contact" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- contact generation --" \n
  "!! ------------------------------" \n
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
  "!! .............................." \n
  "!@@@ - check contact status -" \n
  "!! .............................." \n
  \n
  "cncheck !list contact pari properties" \n
  "cncheck,summary !list only open/closed status" \n
  "cncheck,adjust !adjust physically the elements!" \n
  "/solu" \n
  "cncheck,post !write contact config to jobname.rcn" \n
  \n
  "/post1" \n
  "/inquire,Job_name,jobname!get string array jobname|directory|user|psearch" \n
  "/inquire,param,date,file,ext !get date(size,lines) of file.ext" \n
  "save" \n
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
  "ssum !sum of element table items" \n
  \n
  )

(define-skeleton ansys-skeleton-contact-rigid
  "Rigid contacts skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- rigid target creation -- " \n
  "!! ------------------------------" \n
  \n
  "Contact="_ \n
  "Target=Contact+1" \n
  "real,Contact" \n
  "type,Target" \n
  "tshap,line !2d/3d" \n
  "!!tshap,para !parabola 2d/3d" \n
  "!!tshap,arc !clockwise arc 2d (targe169)" \n
  "!!tshap,carc !counterclockwise arc 2d (targe169)" \n
  "!!tshap,circ !2d  (targe169)" \n
  "!!tshap,tria !3d" \n
  "!!tshap,tri6 !6 node triangle 3d" \n
  "!!tshap,quad !3d" \n
  "!!tshap,quad8 !8-node quadrilateral 3D" \n
  "!!tshap,cyli !3d" \n
  "!!tshap,cone" \n
  "!!tshap,sphe !sphere 3d" \n
  "!!tshap,pilo !2d/3d" \n
  "!!tshap,point !2d/3d" \n
  "*get,Nmax,node,,num,max" \n
  "n,Nmax+1,1,1,0" \n
  " ,Nmax+2,1,2,0" \n
  "e,Nmax+1,Nmax+2" \n
  "tshap,pilo !2d/3d" \n
  "e,Nmax+1" \n
  \n
  )

(define-skeleton ansys-skeleton-coordinates
  "Co-ordinate systems skeleton."
  nil
  "!! .............................." \n
  "!@@@ - coordinate system creations and modifications -" \n
  "!! .............................." \n
  \n
  "csys,4 ![0]:cartesian, 1:cylindrical, 2:spherical, 3:toroidal, 4:wp" \n
  "clocal,11,cylin !define local coord. sys. 11 from active, with type cylindrical" \n
  "wpcsys,12 !set working plane to coordinate system 12" \n
  "wprota,,,90 !rotate wp" \n
  "wpstyl,SNAP,GRSPAC,GRMIN,GRMAX,WPTOL,WPCTYP,GRTYPE,WPVIS,SNAPANG !style of wp" \n
  "!! .............................." \n
  "!@@@ - coordinate system display -" \n
  "!! .............................." \n
  \n
  "/psymb,cs,1 ! display local coord." \n
  "/psymb,ndir,1 ! display (only) rotated nodal coord." \n
  "/plopts,wp,1 !display working plane" \n
  "/plopts,wp,off !switch off wp" \n
  "/triad,rbot"_ \n
  "/triad,off"
  \n
  )

(define-skeleton ansys-skeleton-working-plane
  "Settings for the working plane and related stuff."
  nil
  "!! .............................." \n
  "!@@@ - working plane setup -" \n
  "!! .............................." \n
  \n
  "/plopts,wp,1 !display working plane" \n
  "/repl" \n
  "wpcsys,1,0 !align wp in WIN with specified c-sys" \n
  "wpoffs,,-100 !x,y,z offset" \n
  "wprota,0,90,0 !z,x,y axis of rotation!" \n
  "/plopts,wp,off !switch off wp" \n
  "/triad,off !off: switch off co-ordinate triad, rbot, ltop, ..." \n
  "/plopts,frame,off !switch off graphics frame" \n
  "/plopts,logo,off !switch off ANSYS logo" \n
  "wpstyl,,,,,,1 !type spec 0,1,2 cartesian,cylindrical,spherical" \n
  "wpstyl,,,,,,,0 !grid spec: 0 grid+triad,1 grid,2 [triad]" \n
  "!wpstyl,stat" \n
  "csys,wp ! or csys,4: change csys to wp" \n
  \n
  )

;; PlotCtrls ->Multi-plot-Ctrls???
(define-skeleton ansys-skeleton-multi-plot
  "Multi-plot skeleton"
  nil
  "!! .............................." \n
  "!@@@ - multiplot controls -" \n
  "!! .............................." \n
  \n
  "/gcmd,1,u,sum" \n
  "/gtype,all,node,0 !turn off nodes (elem,keyp,line,area)" \n
  "/gtype,,volu,1 !turn on volumens" \n
  "gplot" \n
  \n
  )

;; PlotCtrls ->Numbering Controls
(define-skeleton ansys-skeleton-numbering-controls
  "Numbering controls skeleton."
  nil
  "!! .............................." \n
  "!@@@ - numbering controls -" \n
  "!! .............................." \n
  \n
  "/pnum,kp,1 !line;area;volu;node;elem;mat;type;tabn;sval,on" \n
  "/number,2 ![0]: colour & number, 1:colour only, 2 number only" \n
  "/pnum,defa !1: turn off any numbering" \n
  "/replot" \n
  \n
  )

;; PlotCtrls -> Symbols
(define-skeleton ansys-skeleton-symbols
  "Symbols skeleton."
  nil
  "!! .............................." \n
  "!@@@ - symbol display -" \n
  "!! .............................." \n
  \n
  "/vscale,,10 !scale displayed vectors" \n
  "/psf,pres,,2 !2 arrows, surface loads" \n
  "/pbf !body loads" \n
  "/psf,conv,hcoef ! plot surface loads" \n
  "/pbf,hgen ! plot body force loads (temp,...) as contours" \n
  "/pbc,all,,1 !bc symbols" \n
  "/pbc,f,,1 !1:show applied force symbols" \n
  "/pbc,nfor,,1 ! show nodal forces"  \n
  "/pbc,rfor,,1 ![0],1:show reaction forces" \n
  "/pbc,defa !reset /pbc" \n
  "/pice !element initial condition symbols" \n
  "!! coordinates" \n
  "/psymb,esys,1 ![0],1: display of element co-ordinate sys." \n
  "/psymb,ndir,1 !only for rotated nodal co-ordinate systems!" \n
  "/psymb,stat" \n
  "/repl" \n
  \n
  )

(define-skeleton ansys-skeleton-element-table
  "Element tables."
  nil
  "!@ ------------------------------" \n
  "!@@ -- Etables, element tables --" \n
  "!! ------------------------------" \n
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
  "!etable,cpre,cont,pres" \n
  "!plls,Pene,Pene !line elem. results" \n
  "esort,etab,R" \n
  "etable,refl !refill all element tables for latest load set" \n
  "*get,Mc,etab,sort,,max" \n
  "*msg,,Mc" \n
  "Mohr-Coulomb criterion (< 1): %G" \n
  "ssum !Calculate and print the sum of element table items." \n
  "sabs,1 ! 1: absolut values, [0] algebraic values for table operations" \n
  "*get,My_magnet_force1,ssum,,item,S1 " \n
  \n
  )

(define-skeleton ansys-skeleton-beam-template
  "Minimal example of a modal analysis with beams"
  nil
  '(ansys-skeleton-header)
  "!fini" \n
  "!/clear" \n
  "!y" \n
  "/units,mpa !indicate mm-t-s unit system" \n
  "!@ ==============================" \n
  "!@ --- Preprocessing ---" \n
  "!@ ==============================" \n
  "/prep7" \n
  "!@@ -- Elements --" \n
  "Steel = 1" \n
  "ID = Steel" \n
  "et,ID,beam189 !189 3d 3node, 188 3d, 2 node beam" \n
  "" \n
  "!! real = ID+1" \n
  "!! et,ID+1,mass21,,,2!no rotary intertia" \n
  "!! R,ID+1,100*7850e-12" \n
  "!! !keyopt,ID+1,3,2" \n
  "" \n
  "!@@ -- Material --" \n
  "mp,nuxy,Steel,0.3 ! Poisson No" \n
  "mp,ex,Steel,200000 ! Elastic modulus" \n
  "mp,dens,Steel,7850e-12 !density in t/mm³" \n
  "" \n
  "!@@ -- Modeling --" \n
  "sectype,1,beam,rect" \n
  "secdata,1,1" \n
  "slist, 1, 1 !list section properties" \n
  "secplot,1 ! show beam section" \n
  "" \n
  "n,1,0" \n
  "*repeat,11,1,1" \n
  "" \n
  "type,1" \n
  "mat,Steel" \n
  "e,1,2" \n
  "*repeat,10,1,1" \n
  "" \n
  "!! type,ID+1" \n
  "!! real,ID+1" \n
  "!! e,11" \n
  "" \n
  "!@@ -- BCs, Loads --" \n
  "nsel,s,loc,x,0" \n
  "d,all,all" \n
  "" \n
  "!! nsel,s,loc,x,1" \n
  "!! d,all,uy,-.1" \n
  "!! allsel" \n
  "!! save" \n
  "!@ ==============================" \n
  "!@ --- Solving ---" \n
  "!@ ==============================" \n
  "/solu" \n
  "allsel" \n
  "antype,modal" \n
  "modopt,lanb,10,10,1e10!method,No of modes,freqB,freqE" \n
  "outres,all,all" \n
  "mxpand,,,,yes! write results to the results file" \n
  "solve" \n
  "save" \n
  "" \n
  "/solu" \n
  "solve" \n
  "!@ ==============================" \n
  "!@ --- Postprocessing ---" \n
  "!@ ==============================" \n
  "/post1" \n
  "/graphics,power !for 3d result views" \n
  "set,list" \n
  "set,1,1 !1st eigenfrequency" \n
  "prnsol,dof !print nodal solution results" \n
  "prrsol,node !reaction solution" \n
  "" \n
  "/eshape,1" \n
  "/graphics,power !for post1 results" \n
  "/view,,1,1,1" \n
  "pldisp" \n
  "plnsol,u,sum,2" \n
  "anmode" \n
  )

(define-skeleton ansys-skeleton-element-definition
  "Element definitions skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- element definitions --" \n
  "!! ------------------------------" \n
  \n
  "!! --- Solid elements ---" \n
  "*get,ET,etyp,,num,max !get maximum element type" \n
  "Solid = ET+1" \n
  "ID = Solid" \n
  "real,ID" \n
  "et,ID,solid186 !3d, 20 node" \n
  "etlist !list defined elements" \n
  \n
  "et,ID,solid185 !3d, 8 node" \n
  \n
  "!! --- A plane element type --- " \n
  "*get,ET,etyp,,num,max !get maximum element type" \n
  "Plane = ET+1" \n
  "ID = Plane" \n
  "real,ID" \n
  "et,ID,plane183,,,3 !2d, 8 node (3)0:plane stress, 1:axissymmetric, 2:plane strain, 3:plane stress with thickness real constant" \n
  "/eshape,1 !1:use real constant def. for element shapes" \n
  "/graphics,power !for post1 results" \n
  "r,ID,13 ! thickness" \n
  \n
  "et,ID,plane182 !2d, 4 node" \n
  "keyopt,ID,3,1 !(3)=0:plane stress,1:axissym,2:plain strain." \n
  "keyopt,ID,1,0 !(1)=0:reduced integr.2:enhanced strain for bending" \n
  "!!for most elements the radial direction is the x-axis" \n
  \n
  "!@ -------------------------------" \n
  "!@@ -- contact pair definitions --" \n
  "!! -------------------------------" \n
  \n
  "*get,ET,etyp,,num,max !get maximum element type" \n
  "Contact = ET+1" \n
  "ID = Contact" \n
  "Target=Contact+1" \n
  "r,Contact !define a real set" \n
  "et,Contact,conta174    !3d, 8 node" \n
  \n
  "et,Contact,conta173 !3d, 4 node" \n
  "et,Contact,conta172 !2d, 3 node" \n
  "et,Contact,conta171 !2d, 2 node" \n
  "et,Contact,conta175 !2/3d node to surf" \n
  "et,Contact,conta176 !3d line to line, 3 node" \n
  "et,Contact,conta177 !3d line to surf, 3 node" \n
  \n
  "et,Target,targe170 !3d area,line,(pilot-)node" \n
  \n
  "et,Target,targe169  !2d" \n
  \n

  "!! .............................." \n
  "!@@@ - structural shells, beams, masses and planes -" \n
  "!! .............................." \n
  \n
  "et,ID,shell181 !3d 4/3-node structural shell" \n
  "Shell = 1" \n
  "ID = Shell" \n
  "sectype,ID,shell" \n
  "Secdata,ShellThickness" \n
  "et,ID,shell281 !3d 8/6-node structural shell" \n
  \n
  "!! - beams -" \n
  "et,ID,beam188 !3d 2-node beam, shows only line results" \n
  "et,ID,beam189 !3d 3-node beam, shows 3d results with power graphics" \n
  "sectype,ID,beam,rect" \n
  "secdata,1,1 !rect: width,height" \n
  "secplot,1" \n
  "slist, 1, 1 !list section properties" \n
  "!! - mass -" \n
  "et,ID,mass21,,,2 !(3)2 no rotary inertia" \n
  "et,ID,mass21   !keyo(3)=0 3d mass with rotary inertia" \n
  "!keyopt,ID,1,1 !keyopt(1)=1: volume and rotary intertias/density" \n
  "r,ID,1,...,R6 ! mass or mass per density" \n
  "rmore,ID,R7,... R12" \n
  "mp,dens,ID,Density !if keyopt(1)=1" \n
  "!! - planes -" \n
  "et,ID,plane182 !2d 4/3-node structural solid" \n
  "et,ID,plane183 !2d 8/6-node structural solid" \n
  \n
  "!! .............................." \n
  "!@@@ - thermal -" \n
  "!! .............................." \n
  \n
  "et,ID,plane35 !2d 6-node triangular thermal solid" \n
  "et,ID,plane77,,,3 !2d 8-node thermal solid, keyopt(3)=3 thickness" \n
  "r,ID,length !plane77 has only 1 real const.: thk" \n
  "et,ID,solid90 !3D 20 nodes thermal solid" \n
  \n
  "!! .............................." \n
  "!@@@ - magnetics -" \n
  "!! .............................." \n
  \n
  "et,ID,plane13 !2d, legacy 4-node coupled-field ->plane233 8-node" \n
  "keyopt,ID,3,1 !(3)=1:axissym." \n
  "keyopt,ID,5,2 !(5)=2:nodal magnetic field printout" \n
  "et,ID,infin110 !2d semi infinit electromagnetic elem." \n
  "!only 1 element layer, sized in the order of the problem domain" \n
  "keyopt,ID,3,1 !(3)=1:axissym." \n
  "keyopt,ID,2,1 !(2)=0:4-node,1:8-n" \n
  \n
  "!! .............................." \n
  "!@@@ - assign attributes -" \n
  "!! .............................." \n
  \n
  "aatt,MAT,REAL,TYPE ! associate prop. with selected areas" \n
  \n
  "!! /pnum,type,1 $ eplot ! display materials" \n
  \n
  )

(define-skeleton ansys-skeleton-meshing
  "Meshing skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- meshing --" \n
  "!! ------------------------------" \n
  "shpp,off !switch off shape checking" \n
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
  "type !element type" \n
  "vmesh,all" \n
  "amesh,all" \n
  "/shrink,.5 ![0] 0 to max 0.5, shrink elem.,l,a,v" \n
  "eplot" \n
  "lsel,s,lcca !select all concatenated lines" \n
  "ldele,all" \n
  \n
  "!! -- extrusion --" \n
  "! ESIZE must be set before vext extrusion!" \n
  "esize,,1 ! here: one element per extrusion" \n
  "vext  !extrude areas by offset" \n
  "vdrag !extrude areas along lines" \n
  \n
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
  "Standard FORTRAN and get functions."
  nil
  "!! ==============================" \n
  "!! --- Parameteric functions ---" \n
  "!! ==============================" \n
  "X = 2 $ Y = 1/3 !Base X and (real) exponent Y for the following:" \n
  "A = X**Y    !Exponentiation x**y: x^y" \n
  "A = ABS()   !Absolute value"  \n
  "A = SIGN()  !sign(x,y) absolute value of x with sign of y" \n
  "A = CXABS() !cxabs(x,y) absolute value of complex number x+i*y" \n
  "A = EXP()   !Exponential,exp(x): e^x" \n
  "A = LOG()   !Natural log"  \n
  "A = LOG10() !Common log, Briggs' log" \n
  "A = SQRT()  !Square root" \n
  "A = NINT()  !Nearest integer, poor man's round(x,2)=nint(x*100)/100" \n
; bullshit  "A = abs(nint()) !round" \n
  "A = MOD()   !mod(x,y): modulo x/y" \n
  "A = RAND()  !rand(lower_bound,upper_bound):uniform dist." \n
  "A = GDIS()  !gdis(mean,stdd): gaussian distribution" \n
  "!! the default for the trig. is radians, change this with *afun" \n
  "A = SIN()   !Sine"  \n
  "A = COS()   !Cosine"  \n
  "A = TAN()   !Tangent"  \n
  "A = SINH()  !Hyperbolic sine" \n
  "A = COSH()  !Hyperbolic cosine" \n
  "A = TANH()  !Hyperbolic tangent" \n
  "A = ASIN()  !Arcsine,arg. between -1.0 and +1.0 " \n
  "A = ACOS()  !Arccosine,arg. between -1.0 and +1.0 " \n
  "A = ATAN()  !Arctangent" \n
  "A = ATAN2() !atan2(x,y): arctangent of y/x with the sign of each component considered" \n
  \n
  "!! ==============================" \n
  "!! --- get functions ---" \n
  "!! ==============================" \n
  "! -- selections -- " \n
  "NSEL(N) ! Status of node N: -1=unselected, 0=undefined, 1=selected." \n
  "ESEL(E) ! Status of element E: -1=unselected, 0=undefined, 1=selected." \n
  "KSEL(K) ! Status of keypoint K: -1=unselected, 0=undefined, 1=selected." \n
  "LSEL(L) ! Status of line L: -1=unselected, 0=undefined, 1=selected." \n
  "ASEL(A) ! Status of area A: -1=unselected, 0=undefined, 1=selected." \n
  "VSEL(V) ! Status of volume V: -1=unselected, 0=undefined, 1=selected." \n
  "! -- Next Selected Entity --" \n
  "NDNEXT(N) ! Next selected node having a node number greater than N." \n
  "ELNEXT(E) ! Next selected element having an element number greater than E." \n
  "KPNEXT(K) ! Next selected keypoint having a keypoint number greater than K." \n
  "LSNEXT(L) ! Next selected line having a line number greater than L." \n
  "ARNEXT(A) ! Next selected area having an area number greater than A." \n
  "VLNEXT(V) ! Next selected volume having a volume number greater than V." \n
  "! -- Locations --" \n
  "CENTRX(E) ! Centroid X-coordinate of element E in global Cartesian coordinate system. Centroid is determined from the selected nodes on the element." \n
  "CENTRY(E) ! Centroid Y-coordinate of element E in global Cartesian coordinate system. Centroid is determined from the selected nodes on the element." \n
  "CENTRZ(E) ! Centroid Z-coordinate of element E in global Cartesian coordinate system. Centroid is determined from the selected nodes on the element." \n
  "NX(N) ! X-coordinate of node N in the active coordinate system." \n
  "NY(N) ! Y-coordinate of node N in the active coordinate system." \n
  "NZ(N) ! Z-coordinate of node N in the active coordinate system." \n
  "KX(K) ! X-coordinate of keypoint K in the active coordinate system" \n
  "KY(K) ! Y-coordinate of keypoint K in the active coordinate system" \n
  "KZ(K) ! Z-coordinate of keypoint K in the active coordinate system" \n
  "LX(L,LFRAC) ! X-coordinate of line L at length fraction LFRAC (0.0 to 1.0)." \n
  "LY(L,LFRAC) ! Y-coordinate of line L at length fraction LFRAC (0.0 to 1.0)." \n
  "LZ(L,LFRAC) ! Z-coordinate of line L at length fraction LFRAC (0.0 to 1.0)." \n
  "LSX(L,LFRAC) ! X slope of line L at length fraction LFRAC (0.0 to 1.0)." \n
  "LSY(L,LFRAC) ! Y slope of line L at length fraction LFRAC (0.0 to 1.0)." \n
  "LSZ(L,LFRAC) ! Z slope of line L at length fraction LFRAC (0.0 to 1.0)." \n
  "! -- Nearest to Location --" \n
  "NODE(X,Y,Z) ! Number of the selected node nearest the X,Y,Z point (in the active coordinate system, lowest number for coincident nodes)." \n
  "KP(X,Y,Z) ! Number of the selected keypoint nearest the X,Y,Z point (in the active coordinate system, lowest number for coincident nodes)." \n
  "! -- Distances --" \n
  "DISTND(N1,N2) ! Distance between nodes N1 and N2." \n
  "DISTKP(K1,K2) ! Distance between keypoints K1 and K2." \n
  "DISTEN(E,N) ! Distance between the centroid of element E and node N. Centroid is determined from the selected nodes on the element." \n
  "! -- Angles (in radians by default -- see the *AFUN command) --" \n
  "ANGLEN(N1,N2,N3) ! Subtended angle between two lines (defined by three nodes where N1 is the vertex node). Default is in radians." \n
  "ANGLEK(K1,K2,K3) ! Subtended angle between two lines (defined by three keypoints where K1 is the vertex keypoint). Default is in radians." \n
  "! -- Nearest to Entity --" \n
  "NNEAR(N) ! Selected node nearest node N." \n
  "KNEAR(K) ! Selected keypoint nearest keypoint K." \n
  "ENEARN(N) ! Selected element nearest node N. The element position is calculated from the selected nodes." \n
  "! -- Areas --" \n
  "AREAND(N1,N2,N3) ! Area of the triangle with vertices at nodes N1, N2, and N3." \n
  "AREAKP(K1,K2,K3) ! Area of the triangle with vertices at keypoints K1, K2, and K3." \n
  "ARNODE(N) ! Area at node N apportioned from selected elements attached to node N. For 2-D planar solids, returns edge area associated with the node. For axisymmetric solids, returns edge surface area associated with the node. For 3-D volumetric solids, returns face area associated with the node. For 3?D, select all the nodes of the surface of interest before using ARNODE." \n
  "! -- Normals --" \n
  "NORMNX(N1,N2,N3) ! X-direction cosine of the normal to the plane containing nodes N1, N2, and N3." \n
  "NORMNY(N1,N2,N3) ! Y-direction cosine of the normal to the plane containing nodes N1, N2, and N3." \n
  "NORMNZ(N1,N2,N3) ! Z-direction cosine of the normal to the plane containing nodes N1, N2, and N3." \n
  "NORMKX(K1,K2,K3) ! X-direction cosine of the normal to the plane containing keypoints K1, K2, and K3." \n
  "NORMKY(K1,K2,K3) ! Y-direction cosine of the normal to the plane containing keypoints K1, K2, and K3." \n
  "NORMKZ(K1,K2,K3) ! Z-direction cosine of the normal to the plane containing keypoints K1, K2, and K3." \n
  "! -- Connectivity --" \n
  "ENEXTN(N,LOC) ! Element connected to node N. LOC is the position in the resulting list when many elements share the node. A zero is returned at the end of the list." \n
  "NELEM(E,NPOS) ! Node number in position NPOS (1--20) of element E." \n
  "NODEDOF(N) ! Returns the bit pattern for the active DOFs at the specified node.bit 0 is UX, bit 1 is UY,... bit 5 is ROTZ bits 6,7,8 are AX,AY,AZ bits 9,10,11 are VX,VY,VZ bit 18 is PRES, bit 19 is TEMP, bit 20 is VOLT, bit 21 is MAG bit 24 is EMF, bit 25 is CURR For a node with UX,UY,UZ the return value will be 7 (bits 0,1,2) For a node with UX,UY,UZ,ROTX,ROTY,ROTZ the return value will be 63 (bits 0,1,2,3,4,5)" \n
  "! -- Faces --" \n
  "ELADJ(E,FACE) ! For 2-D planar solids and 3-D volumetric solids, element adjacent to a face (FACE) of element E. The face number is the same as the surface load key number. Only elements of the same dimensionality and shape are considered. A -1 is returned if more than one is adjacent." \n
  "NDFACE(E,FACE,LOC) ! Node in position LOC of a face number FACE of element E. The face number is the same as the surface load key number. LOC is the nodal position on the face (for an IJLK face, LOC=1 is at node I, 2 is at node J, etc.)" \n
  "NMFACE(E) ! Face number of element E containing the selected nodes. The face number output is the surface load key. If multiple load keys occur on a face (such as for line and area elements) the lowest load key for that face is output." \n
  "ARFACE(E) ! For 2-D planar solids and 3-D volumetric solids, returns the area of the face of element E containing the selected nodes. For axisymmetric elements, the area is the full (360 degree) area." \n
  "! -- Degree of Freedom Results --" \n
  "UX(N) ! UX structural displacement at node N." \n
  "UY(N) ! UY structural displacement at node N." \n
  "UZ(N) ! UZ structural displacement at node N." \n
  "ROTX(N) ! ROTX structural rotation at node N." \n
  "ROTY(N) ! ROTY structural rotation at node N." \n
  "ROTZ(N) ! ROTZ structural rotation at node N." \n
  "TEMP(N) ! Temperature at node N. For SHELL131 and SHELL132 elements with KEYOPT(3) = 0 or 1, use TBOT(N), TE2(N), TE3(N), . . ., TTOP(N) instead of TEMP(N)." \n
  "PRES(N) ! Pressure at node N." \n
  "VX(N) ! VX fluid velocity at node N." \n
  "VY(N) ! VY fluid velocity at node N." \n
  "VZ(N) ! VZ fluid velocity at node N." \n
  "ENKE(N) ! Turbulent kinetic energy (FLOTRAN) at node N." \n
  "ENDS(N) ! Turbulent energy dissipation (FLOTRAN) at node N." \n
  "VOLT(N) ! Electric potential at node N." \n
  "MAG(N) ! Magnetic scalar potential at node N." \n
  "AX(N) ! AX magnetic vector potential at node N." \n
  "AY(N) ! AY magnetic vector potential at node N." \n
  "AZ(N) ! AZ magnetic vector potential at node N." \n
  "! -- Returns information about the data base manager --" \n
  "VIRTINQR(1) ! Number of pages in core." \n
  "VIRTINQR(4) ! Page size in integer words." \n
  "VIRTINQR(7) ! Maximum number of pages allowed on disk." \n
  "VIRTINQR(8) ! Number of read/write operations on page." \n
  "VIRTINQR(9) ! Maximum record number on page." \n
  "VIRTINQR(11) ! Maximum pages touched." \n
  "! -- Returns the current value of ANSYS filtering keywords. --" \n
  "KWGET(KEYWORD) ! Returns the current value the keyword specified by KEYWORD. See the ANSYS UIDL Programmer's Guide for a list of keywords and values." \n
  "! -- Character String Functions Strings must be dimensioned (see *DIM) as a character parameter or enclosed in single apostrophes ('char'). --" \n
  "! -- Functions which return a double precision value of a numeric character string. --" \n
  "VALCHR(a8) ! a8 is a decimal value expressed in a string." \n
  "VALOCT(a8) ! a8 is an octal value expressed in a string." \n
  "VALHEX(a8) ! a8 is a hex value expressed in a string." \n
  "! -- Functions which return an 8 character string of a numeric value. --" \n
  "CHRVAL(dp) ! dp is a double precision variable." \n
  "CHROCT(dp) ! dp is an integer value." \n
  "CHRHEX(dp) ! dp is an integer value." \n
  "! -- Functions which manipulate strings: StrOut is the output string (or character parameter) Str1 and Str2 are input strings. Strings are a maximum of 128 characters. (see *DIM) StrOut = STRSUB(Str1, nLoc,nChar)  Get the nChar substring starting at character nLoc in Str1. StrOut = STRCAT(Str1,Str2) Add Str2 at the end of Str1. --" \n
  "STRFILL(Str1,Str2,nLoc) ! StrOut = STRFILL(Str1,Str2,nLoc)  Add Str2 to Str1 starting at character nLoc." \n
  "STRCOMP(Str1) ! StrOut = STRCOMP(Str1) Remove all blanks from Str1" \n
  "STRCOMP(Str1) ! Left-justify Str1" \n
  "STRPOS(Str1,Str2) ! nLoc = STRPOS(Str1,Str2) Get starting location of Str2 in Str1." \n
  "STRLENG(Str1) ! nLoc = STRLENG(Str1) Location of last nonblank character" \n
  "UPCASE(Str1) ! StrOut = UPCASE(Str1) Upper case of Str1" \n
  "LWCASE(Str1) ! StrOut = LWCASE(Str1) Lower case of Str1" \n
  "! -- The following functions manipulate file names. --" \n
  "JOIN('directory','filename','extension') ! Path String = JOIN ('directory','filename','extension')  Produces a contiguous pathstring. e.g. directory/filename.ext" \n
  "JOIN('directory','filename') ! Path String = JOIN ('directory','filename') Produces a contiguous pathstring. e.g. directory/filename" \n
  "SPLIT('PathString', 'DIR') !  Produces a separate output of the directory from the pathstring." \n
  "SPLIT('PathString', 'FILE') !  Produces a separate output of the complete filename (with extension) from the pathstring." \n
  "SPLIT('PathString', 'NAME') ! Produces a separate output of the filename from the pathstring." \n
  "SPLIT('PathString', 'EXT') !  Produces a separate output of the file extension from the pathstring." \n
  )

(define-skeleton ansys-skeleton-geometry
  "Geometry definitons skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- geometry --" \n
  "!! ------------------------------" \n
  \n
  "/prep7" \n
  "X1 = 0" \n
  "X2 = 1" \n
  "Y1 = X1" \n
  "Y2 = X2" \n
  "Z1 = 0" \n
  "Z2 = 1" \n
  "!! --- volumes ---" \n
  "block,X1,X2,Y1,Y2,Z1,Z2 !3d hexahedron (wp)" \n
  "sphere,Rad1,Rad2,Th1,Th2 !spherical volume" \n
  "cylind,R1,R2,Z1,Z2,Th1,Th2 !cylinder V>0! " \n
  "!!arsym,y,all ! reflection of areas "
  "Xc  = 0 !centre x-coord." \n
  "Yc  = 0 !centre y-coord." \n
  "!! --- AREAS ---" \n
  "rectng,X1,X2,Y1,Y2 ! 2d rectangle" \n
  "wpcsys" \n
  "R1  = 4" \n
  "R2  = 20" \n
  "N   = 14." \n
  "Th1 = -360./(2*N)" \n
  "Th2 = +360./(2*N)" \n
  "Depth=30" \n
  "pcirc,R1,R2,Th1,Th2 ! circular area on WP" \n
  "rpr4,3,10,0,1.2,! polygonal area or prism volume"
  "rcon$stat !status of real constands" \n
  "*get,RN,rcon,,num,max	 !maximum real set no "
  "Cylinder = RN + 1 !new real set" \n
  "ID = Cylinder" \n
  "r,ID,Length" \n
  "!! --- VOLUMES  (or AREAS) -- "\n
  "cyl4,Xc,Yc,R1,Th1,R2,Th2,Depth=>0 !circular area or cylindrical volume" \n
  "!! --- KeyPoints ---"  \n
  "source,X,Y,Z !default undefined kp and node location" \n
  "kl,L1,Ratio,KPNo !keypoint on line" \n
  "!! --- LINES ---" \n
  "l,KP1,KP2, NDIV, SPACE, slope vector XV1, YV1, ZV1, XV2, YV2, ZV2 !line in the respective CS with opt. slope" \n
  "lstr,KP1,KP2 !straight line irrespective of current CS" \n
  "larc,Kp1,Kp2,Kpc,rad !if rad is blank, fit throught KPs" \n
  "circle,centreKp,radiusKp," \n
  \n
  "!! .............................." \n
  "!@@@ - checks -" \n
  "!! .............................." \n
  \n
  "/prep7" \n
  "gsum !geometry stats" \n
  "asum !area statistics, mass, inertia" \n
  "vsum !volume statistics" \n
  "*get,Par,volu,,volu !get volume from last vsum" \n
  \n
  "!! .............................." \n
  "!@@@ - operations -" \n
  "!! .............................." \n
  "pcirc,Diam/2,Diam/2+Thic,90-22.5,90" \n
  "Y=Diam/2+Thic"\n
  "source,Thic,Y-Interf,0 !make location unambiguous for kmove!!"\n
  "Interf = 0.01"\n
  "kmove,10,0,U,Y-Interf,0,1,Y,U,0! move KP"\n
  "move,10,0,U,Y-Interf,0,1,Y,U,0! move node"\n
  \n
  "nummrg,all! merge coincident items" \n
  "vglue !a-,l- glue items together" \n
  "boptn,stat !boolean operation options" \n
  "!! --- line ---" \n
  "lglue,all !glue lines, retaining area attributes" \n
  "ldiv,1,.5 !divide line 1 in ratio .5" \n
  "!! --- areas ---" \n
  "arsym,X|Y|Z, NA1, NA2, NINC, KINC, NOELEM, IMOVE !cartesian reflection normal to X,y,z"
  "agen,ITIME,NA1,NA2,NINC,DX,DY,DZ,KINC,NOELEM,IMOVE!Generate areas from a pattern of areas" \n
  "atran !Transfers a pattern of areas to another coord.-system." \n
  "arotat !areas from rotated lines" \n
  "!! --- volume ---" \n
  "vdele,all,,,1 ! delete everything below" \n
  "vrotat !volumes from areas !" \n
  \n
  "!! .............................." \n
  "!@@@ - booleans -" \n
  "!! .............................." \n
  \n
  "!! area" \n
  "asbl,all,all ! substract area by line" \n
  "aovlap,all ! overlap areas" \n
  "asba,A1,A2,SEPO,KEEP1,KEEP2 ! SEPO: seperate entities" \n
  "asbw, !substract by wp" \n
  "/pnum,area,1 $ aplot" \n
  \n
  "!! volume" \n
  "vglue,all" \n
  "vsbw,all,,delete !v substracted by wp" \n
  "vdele,all,,,1 !skwp 1:delete kp,l,a as well" \n
  "vsym! symmetry reflections: arsym,esym" \n
  \n
  )

(define-skeleton ansys-skeleton-material-definition
  "Material definitons skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- material definitions --" \n
  "!! ------------------------------" \n
  \n
  "!! --- material library ---" \n
  "!! convention: file suffix: .unitssystem_MPL" \n
  "!! unitsystems:" \n
  "!! USER — User-defined system (default)." \n
  "/units,mpa !indicate MPA system for material defs." \n
  "!! SI — International system (m, kg, s, K)." \n
  "!! MKS — MKS system (m, kg, s, °C)." \n
  "!! uMKS — μMKS system (μm, kg, s, °C)." \n
  "!! CGS — CGS system (cm, g, s, °C)." \n
  "!! MPA — MPA system (mm, Mg, s, °C)." \n
  "!! BFT — U. S. Customary system using feet (ft, slug, s, °F)." \n
  "!! BIN — U. S. Customary system using inches (in, lbf*s2/in, s, °F)." \n
  \n
  "!! --- material library --- " \n
  "/mplib,write,/HOME/uidg1626/a-m/matlib" \n
  "!! --- It is advisable to make the material files which are commented read only!" \n
  "/mplib,read,/HOME/uidg1626/a-m/matlib" \n
  "/mplib,stat !shows the read write directories" \n
  "!! which materials are in the mplib?" \n
  "/sys,ls ~/a-m/matlib" \n
  "/units,mpa !default extension for mpread is now MPA_MPL" \n
  "mat,Steel ! set the material number" \n
  "mpread,St37,,,lib" \n
  "! mpwrite" \n
  "mplist" \n
  \n
  "*get,MN,mat,,count,max !get maximum material no" \n
  "Steel=NM+1" \n
  "ID = Steel" \n
  "!! MP: constat properties or up to 4th order polynomials in temperature" \n
  "mp,nuxy,ID,0.3 ! Poisson No" \n
  "mp,ex,ID,200000 ! Elastic modulus" \n
  "mp,dens,ID,7850e-12 !density in t/mm³" \n
  "AlphaSteel = 12e-6 ! thermal expansion in 1/K" \n
  "mp,alpx,ID,AlphaSteel !secant modulus of therm. exp.!" \n
  "!mp,ctex,ID,12e-6 ! instantaneous coofficient of therm. exp." \n
  "KSteel = 60.5 !conductivity in W/(mK)" \n
  "mp,kxx,ID,KSteel" \n
  "mplist,ID,,,all !show all properties of mat. ID" \n
  "mpplot,ex,ID,100,500 !plots mat. vs temp." \n
  \n
  "!! --- Nonlinear materials ---" \n
  "tb,biso,ID,1 ! bilinear isotropic plasticity" \n
  "Yield_stress = 160" \n
  "Tensile_strain = 0.3" \n
  "True_tensile_strain = log( 1+Tensile_strain)" \n
  "Tensile_stress = 260" \n
  "True_tensile_stress = Tensile_stress*(1+Tensile_strain)" \n
  "Tangent_modulus = (True_tensile_stress-Yield_stress) / True_tensile_strain" \n
  "tbdata,,Yield_stress,Tangent_modulus" \n
  "tblist,all,ID !list properties" \n
  "tbplot,biso,ID" \n
  "/com, === Material %ID% is steel. ===" \n
  "Alu=2" \n
  "mp,nuxy,Alu,0.3" \n
  "mp,ex,Alu,70000" \n
  "tb,biso,Alu,1" \n
  "tbdata,,Yield_stress,Tangent_modulus" \n
  \n
  "!! - KINH: Multilinear kinematic hardening of 0.5 mm hardened C75S spring sheet steel - " \n
  "Steel = 1" \n
  "ID = Steel" \n
  "mp,nuxy,ID,0.3 ! Poisson No" \n
  "mp,ex,ID,200000 ! Elastic modulus" \n
  "mp,dens,ID,7850e-12 !density in t/mm³" \n
  "AlphaSteel = 12e-6 ! thermal expansion in 1/K" \n
  "mp,alpx,ID,AlphaSteel !secant modulus of therm. exp.!" \n
  "!mp,ctex,ID,12e-6 ! instantaneous coofficient of therm. exp." \n
  "KSteel = 60.5 !conductivity in W/(mK)" \n
  "mp,kxx,ID,KSteel" \n
  "tbdele,kinh,Steel !redefine material" \n
  "tb,kinh,Steel,1,8,0 !1 temperature, 8 data points,0:total strain" \n
  "tbtemp,20 !measurements @RT" \n
  "tbpt,,.6e-2,1300." \n
  "tbpt,,.75e-2,1430." \n
  "tbpt,,1.e-2,1500." \n
  "tbpt,,1.5e-2,1550." \n
  "tbpt,,2.e-2,1580." \n
  "tbpt,,4.e-2,1645." \n
  "tbpt,,5.e-2,1640." \n
  "tbpt,,5.4e-2,1600." \n
  "mp,ex,Steel,1300/.6e-2	      ! Elastic modulus" \n
  "!! -- mpwrite overwrites without warning existing files! " \n
  "mpwrite,C75S_hardened,MPA_MPL,,lib,Steel !write to library" \n
  \n
  "!! --- kinh (max 20 data points) defined with plastic strain -- " \n
  "PPS = 1" \n
  "tbpt,,ID = PPS" \n
  "mptemp ! erase temp table" \n
  "mptemp,1,23" \n
  "mptemp,2,70" \n
  "mpdata,nuxy,ID,,0.4,0.4" \n
  "mpdata,ex,ID,,13000,11568" \n
  "mplist" \n
  "tbdele,kinh,ID !redefine material" \n
  "tb,kinh,ID,1,10,4 !1 temperature, 9 data sets,4: plastic strain!" \n
  "tbtemp,70 ! 70 °C" \n
  "tbpt,, 0.000000e+00, 20.824" \n
  "tbpt,, 6.845947e-05, 40.856" \n
  "tbpt,, 3.125624e-04, 58.856" \n
  "tbpt,, 8.648098e-04, 74.448" \n
  "tbpt,, 1.530734e-03, 87.568" \n
  "tbpt,, 2.393046e-03, 98.416" \n
  "tbpt,, 3.430311e-03, 107.240" \n
  "tbpt,, 4.619708e-03, 114.304" \n
  "tbpt,, 5.946024e-03, 119.784" \n
  "tbpt,, 7.496811e-03, 123.824" \n
  \n
  "!! - specific heat -" \n
  "mptemp !erase temperature table"  \n
  "mpdata,c,Alu,, ! specific heat in W/(mK)" \n
  "!! - conductivity -" \n
  "mptemp !erase temperature table"  \n
  "mptemp,,-100,0,100,200 !4 temperatures" \n
  "mpdata,kxx,Alu,,114,144,165,175 !conductivities in W/(mK)" \n
  "mptemp" \n
  "/com, === Material %Alu% is Aluminium. ===" \n
  "Air=3" \n
  "mp,dens,Air,1.2e-12 !t/mm³" \n
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
  "*afun,rad" \n
  "Pi = acos(-1)" \n
  "Mu0 = .4*Pi*1e-6 ! field constant in Vs/(Am)" \n
  "Br = .4 ! residual induction in Tesla" \n
  "mp,murx,Magnet,Br/(Mu0*Hc)" \n
  \n
  "/pnum,mat,1 $ eplot" \n
  \n
  )

(define-skeleton ansys-skeleton-component
  "Component (Named Selections in WorkBench) skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- components --" \n
  "!! ------------------------------" \n
  \n
  "cm,cmName,volu !,,area;line;kp;elem;node" \n
  "cmsel,s,cmName !select components cmName" \n
  "cmsel,s,,volu !select all volume components" \n
  "cmsel,all !additional select all components" \n
  "cmdele,cmName !delete component cmName" \n
  "*GET, Parameter, COMP, 0, ncomp! Get the number of components" \n
  \n
)

(define-skeleton ansys-skeleton-bc
  "Boundary conditions skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- boundary conditions --" \n
  "!! ------------------------------" \n
  \n
  "/prep7" \n
  \n
  "kbc,1 ![0](antype,static):ramped, 1:stepped loading" \n
  \n
  "!! .............................." \n
  "!@@@ - DOF constraints -" \n
  "!! .............................." \n
  \n
  "nsel,s,loc,y,0" \n
  ",a,loc,y,1" > \n
  ",r,loc,x,0" > \n
  "d,all,ux,0,,,,uy,uz,rotx,roty,rotz ! apply to other labels" \n
  "d,all,all!dk,dl,da" \n
  "/pbc,u,,on !plot translational constraints" \n
  "/pbc,rot,,on !plot rotational constraints" \n
  "dlist,all" \n
  \n
  "!! .............................." \n
  "!@@@ - concentrated loads -" \n
  "!! .............................." \n
  \n
  "f,all,fx,1,1 !@nodes:real,imag" \n
  "f,all,fx,%table% !table values" \n
  "f,node(0,0,0),m,y !torque load" \n
  "/pbc,m,,on !show torque loads" \n
  "fk,all,fx,1,1 !@keypoints:real,imag" \n
  "/pbc,f,,on !show force loads" \n
  "flist,all !fklist" \n
  \n
  "!! .............................." \n
  "!@@@ - surface loads -" \n
  "!! .............................." \n
  \n
  "sf,all,pres,1 !surface loads on nodes" \n
  "sflist,all" \n
  "sfe,all,pres" \n
  "sfelist,all" \n
  "sfl,all,pres" \n
  "sfllist,all" \n
  "sfa,all,,pres," \n
  "sfalist,all" \n
  \n
  "!! .............................." \n
  "!@@@ - body loads -" \n
  "!! .............................." \n
  \n
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
  "!! .............................." \n
  "!@@@ - inertia relief and mass summary -" \n
  "!! .............................." \n
  \n
  "!! LOADS: nonlinearities aren't supported, fix all DOFs!" \n
  "/solu" \n
  "irlf,1 !0: none,1:ir on,-1:printout masses" \n
  "nsel,s,loc,x,1" \n
  "!! --- partial solution for mass calculation ---" \n
  "outpr,basic,all" \n
  "irlf,-1 " \n
  "/output,mass_output,txt" \n
  "psolve,elform !partial solution" \n
  "psolve,elprep" \n
  "/output" \n
  "*list,mass_output,txt"
  "irlist ! print masses and load summaries" \n
  \n
  "!! .............................." \n
  "!@@@ - corriolis effects -" \n
  "!! .............................." \n
  \n
  "cgomga,x,y,z, ! rotational velocity about globla coord. sys." \n
  "dcgomg,x,y,z ! rotational acceleration about global coord. sys." \n
  \n
  "!! .............................." \n
  "!@@@ - coupling -" \n
  "!! .............................." \n
  \n
  "nsel,s,loc,x,1" \n
  "cp,next,uy,all !couple dofs" \n
  \n
  "!! .............................." \n
  "!@@@ - constraint equations -" \n
  "!! .............................." \n
  \n
  "nsel,s,loc,x,1" \n
  "ce,next,uy,all !couple dofs" \n
  \n
  "allsel" \n
  "/pbc,all,on" \n
  "gplot" \n
  \n
  "!! .............................." \n
  "!@@@ - magnetics -" \n
  "!! .............................." \n
  \n
  "!! fmagbc,'Component' ! flag force calculation" \n
  "bfa,all,js, ! js current density" \n
  "bflist,all" \n
  "dl,all,,asym ! flux parallel to lines" \n
  "nsel,s,ext ! select exterior nodes" \n
  "dsym,asym ! flux parallel to lines" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- plot BCs --" \n
  "!! ------------------------------" \n
  \n
  "/psf,conv,hcoef ! plot surface loads" \n
  "/pbf,hgen ! plot body force loads (temp,...) as contours" \n
  "/pbc,f,,1 !1:show applied force symbols" \n
  "/pbc,nfor,,1 ! show nodal forces"  \n
  "/pbc,rfor,,1 ![0],1:show reaction forces" \n
  "/pbc,defa !reset /pbc" \n
  \n
  )

(define-skeleton ansys-skeleton-buckling
  "Buckling skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ - buckling -" \n
  "!! ------------------------------" \n
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
  "Solving /solu skeleton."
  nil
  "!@ ==============================" \n
  "!@ --- solution --- " \n
  "!! ==============================" \n
  \n
  "/solu" \n
  "allsel" \n
  \n
  "!! --- basics ---" \n
  "solcontrol,on! optimised nonlinear solution defaults" \n
  "!! implies /kbc,0: ramped loading" \n
  "N1=20 ! No of substeps for the first one" \n
  "N2=N1*100 ! maximum No of substemps" \n
  "N3=N1/4 ! minimum No of substeps " \n
  "nsubst,N1,N2,N3" \n
  "outres,all,all !,item,freq,cname" \n
  "antype,static !,rest,LoadStep,SubStep ![new]rest: perform restart operation" \n
  "nlgeom,on" \n
  "autots,on" \n
  \n
  "/gst,on !activate graphical solution tracking (convergence norms only?)" \n
  "solve" \n
  \n
  "!! --- advanced controls ---" \n
  "cnvtol,u,,0.1! convergence [0.5 % solcontrol, on: 5 %] manipulation" \n
  "cnvtol,f,,0.05 !solcontol,on: [0.5% F,M; 5% U]" \n
  "neqit,30! No of equilibr. iterations" \n
  "!! nonlinear tracking" \n
  "nlhist,on ! [off], on: track all nonlinear variables in .nlh" \n
  "nlhist,pair,ContArea,cont,carea,4 ! monitor contact area, of cid 4" \n
  "!! nonlinear diagnostics" \n
  "nldiag,maxf,2 !maximum  files nrXXX or ndXXX to be written" \n
  "nldiag,cont,iter !contact information file .cnd" \n
  "nldiag,cont,stat !status" \n
  "nldiag,nrre,on !store residual file .nrXXX" \n
  "!! plnsol,nrres" \n
  "rescontrol,,1,last !create restart file(s)" \n
  ",status" > \n
  "/config,nres,2000 !No of substeps in result file [1000]" \n
  "/solu" \n
  "!! --- birth & death ---" \n
  "ekill,all !deactivate elements" \n
  "ealive,all !reactivate elements"
  \n
  "eqslv,pcg,1e-4" \n
  "nropt,unsym !frictional contacts not converging?" \n
  "coupling of sliding and normal stiffness" \n
  "!! --- unstable structures: stabilisaton methods " \n
  "stabilize,constant,energy,1e-4 !constant over LS" \n
  "stabilize,reduce,energy,1e-4 !reduce to the end of LS" \n
  "stabilize !decactivate stabilisation" \n
  "antyp,,rest,1,next-to-last-converged substep!!!!!!! to calculate the stabilistation factors" \n
  "!! --- or arclength method ---" \n
  "arclen,on ! arclen stabilisation method" \n
  \n
  "/runst !enter the run statistics processor" \n
  "rall !run statistics estimator" \n
  \n
  "rescontrol,file_summary !check restart files" \n
  "antyp,,rest,1,last" \n
  "time,1.2 !time at the end of load step" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- inertia --" \n
  "!! ------------------------------" \n
  \n
  "omega,,,2*Pi*Rpm/60 !rotational ANGULAR velocity" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- modal --" \n
  "!! ------------------------------" \n
  \n
  "antype,modal" \n
  "modopt,lanb,10,10,1e10!method,No of modes,freqB,freqE" \n
  "mxpand" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- harmonic --" \n
  "!! ------------------------------" \n
  \n
  "antype,harmic" \n
  "dmprat,.02                ! constant damping ratio" \n
  "hropt,full                ! Full harmonic response" \n
  "hrout,off                 ! Print results as amplitudes and phase angles" \n
  "outpr,basic,1             ! solution item printout" \n
  "nsubst,30                 ! 30 Intervals within freq. range" \n
  "harfrq,,7.5               ! Frequency range from 0 to 7.5 HZ" \n
  "kbc,1                     ! Step boundary condition" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- linear buckling --" \n
  "!! ------------------------------" \n
  \n
  "!! static pstres solu" \n
  "/solu" \n
  "allsel" \n
  "pstres,on" \n
  "rescontrol,linear,all,1" \n
  "solve" \n
  "y" \n
  "finish" \n
  "/solu" \n
  "antype,buckle" \n
  "bucopt,lanb,3" \n
  "mxpand,3" \n
  "solve" \n
  "finish" \n
  \n
  "!! Nonlinar buckling with deformed shape" \n
  "/prep7" \n
  "upcoord,2e-1" \n
  "eplot" \n
  "!nwrite,nodes,dat" \n

  \n
  "!@ ------------------------------" \n
  "!@@ -- magnetics --" \n
  "!! ------------------------------" \n
  \n
  "magsolv" \n
  \n
  "solve" \n
  "!@ ------------------------------" \n
  "!@@ -- cyclic symmetry --" \n
  "!! ------------------------------" \n
  \n
  "cycopt,status" \n
  \n
  )

(define-skeleton ansys-skeleton-post1
  "Postprocessing /postXX skeleton."
  nil
  "!@ ==============================" \n
  "!@ --- post 1 ---" \n
  "!@ ==============================" \n
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
  "!! - contours -" \n
  "plnsol,u,sum,2 !0:deformed only, 1:with undef model 2:with undeformed edges" \n
  "plnsol,s,eqv ! von Mises" \n
  "shell,bot, ![top],mid select shell location for result ouput" \n
  "plnsol,s,1 ! maximum principle: Lamé" \n
  "plnsol,s,int ! stress intensity: Tresca" \n
  "prnsol,s,x !|presol components in global x-dir (except transformed:nrotat,rsys)" \n
  "plnsol,s,xy ! shear in xy-dir." \n
  "plnsol,epto,1!principal total mechanical strain (excluding thermal) (EPEL + EPPL + EPCR)," \n
  "plnsol,eptt,1!principal total mechanical strain + thermal strain" \n
  "!! - vectors -" \n
  "plvect,u !display vector results" \n
  "plvect,epto" \n
  "plvect,s" \n
  \n
  "!! .............................." \n
  "!@@@ - temperature -" \n
  "!! .............................." \n
  \n
  "plnsol,temp" \n
  "!! .............................." \n
  "!@@@ - reactions -" \n
  "!! .............................." \n
  \n
  "fsum !force sum from all selected elements" \n
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
  "/dist,,1/2,1 !enlarge twice" \n
  "/noerase ! don't erase screen between plots" \n
  "/erase" \n
  "/triad,rbot ! coordinate system to right bot" \n
  "/plopts,wp ! switch off working plane" \n
  "/plopts,minm ! switch off min max" \n
  \n
  "!/image is not possible in batch mode" \n
  "/image,save,test !save XWindow Dump xwd (or bmp on Windows)" \n
  "/sys,convert test test.png" \n
  "!! -- graphics output & invert background colour --" \n
  "/color,wbak,whit !white background" \n
  "!or" \n
  "/color,wbak,whit !white background or:" \n
  "/RGB,index,100,100,100,0" \n
  "/RGB,index,0,0,0,15" \n
  "/gfile,1200 !resolution height of /show 1000 [256,[800],2400], width is 1.33*height" \n
  "/show,png !creates jobnameXXX.png files quality not as good as with /image" \n
  "pngr,stat !additional png options (orientation,compression,...)" \n
  "plvect,B" \n
  "/noerase" \n
  "lplot" \n
  "/show,close" \n
  "/erase" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- plot BCs --" \n
  "!! ------------------------------" \n
  \n
  "/pbc,f,,1 !1:show applied force symbols" \n
  "/pbc,nfor,,1 ! show nodal forces"  \n
  "/pbc,rfor,,1 ![0],1:show reaction forces" \n
  "/pbc,defa !reset /pbc" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- acoustics --" \n
  "!! ------------------------------" \n
  \n
  "/view,,1,1,1" \n
  "/graphics,full" \n
  "/sscale,,1e5 !topographic display scaling" \n
  "plnsol,pres" \n
  "!! sound pressure level" \n
  "etable,spl,nmisc,4 !read SPL into table" \n
  "pletab,spl" \n
  "!! SPL in nodal display" \n
  \n
  "!! .............................." \n
  "!@@@ - multi window plots -" \n
  "!! .............................." \n
  "/window,2,dele" \n
  "/window,1,dele" \n
  "/erase" \n
  "/window,2,-1,-.5,.5,1 !from -1 to 1 is the full screen" \n
  "aplot" \n
  "/window,2,off" \n
  "/noerase" \n
  "!/window,1,rbot !from -1 to 1 is the full screen" \n
  "/window,1,full !from -1 to 1 is the full screen" \n
  "!/window,3,rbot !full,righ,top,bot,ltop,lbot,rtop,rbot" \n
  "eplot" \n
  "/window,2,on" \n
  "/erase" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- magnetics --" \n
  "!! ------------------------------" \n
  \n
  "/efacet,2" \n
;  "psdisp,0" \n
  "/graphics,full ! results averaging also from interior" \n
  "pletab,Pene" \n
  "plls,Pene,Pene !line element results" \n
  "plf2d,27 ! flux lines, equipotentials" \n
  "plvect,b,! induction vector plot" \n
  "fmagsum,'component_name'" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- modal --" \n
  "!! ------------------------------" \n
  \n
  "*get,Freq1,mode,1,freq! first eigenfrequency" \n
  "pldisp,2 !show deformed shape and undefomed (2) contours" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- harmonics --" \n
  "!! ------------------------------" \n
  \n
  "!! animation" \n
  "lcdef,1,,1		 !real" \n
  "lcdef,2,,1,1		 !complex" \n
  "/show,pscr,,,8! 8 colour planes" \n
  "*afun,deg" \n
  "N = 20" \n
  "I = 1" \n
  "*do,Theta,0,360-360/N,360/N" \n
  "/syp,rm -v file*.eps" \n
  "Real =  cos(Theta)" \n
  "Imag =  sin(Theta)" \n
  "lcfact,1,real" \n
  "lcfact,2,imag" \n
  "*if,Real,eq,0,then" \n
  "lcfact,1,1e-10" \n
  "*endif" > \n
  "*if,Imag,eq,0,then" \n
  "lcfact,2,1e-10" \n
  "*endif" > \n
  "lcase,1" \n
  "lcoper,add,2" \n
  "/title" \n
  "/replot                ! replot currently stored plot item" \n
  "File='-o=eigenmodes/anim%I%.pdf'" \n
  "I = I+1" \n
  "/syp,epstopdf file000.eps,File" \n
  "*enddo" > \n
  "!/seg,off" \n
  "!anim,5,1" \n
  "/show,close" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- contact status --" \n
  "!! ------------------------------" \n
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
  \n
  "!@ ------------------------------" \n
  "!@@ -- animations --" \n
  "!@ ------------------------------" \n
  \n
  "plnsol,s,1" \n
  "/anfile,save !save/resume animation to/from jobname.anim" \n
  "/anfile,save,cylind !save animation to cylind.anim" \n
  "anim,20,1,.3 !cycles:20[5],mode:1[0],forward-reverse 1:forward-forward,delay: .3[.1]" \n
  "anmode !mode shape animation" \n
  "anharm,40,.3,1 !40[12]frames,.3[.1] s delay,1[5] cycles,harmonics animation or complex mode shapes" \n
  "antime,20,.3,1,0,0,0,2!animate time data,20[5]: frames, .3[.1]: s delay, 1[5]: anim. cycles, [0]:no scaling,[0] current LS, 2:range,min tim, max time" \n
  "andata !contour animation over result data range" \n
  "anmres !multiple result files" \n
  \n
  "/show,spring_washer6,grph !ANSYS graphics format for the display utility" \n
  "! display is saveing the animation in the HOME folder on Windows" \n
  "*get,Nls,active,,set,nset" \n
  "set,first" \n
  "/user,! inhibit image size fit under large displacements" \n
  "*do,I,1,Nls" \n
  "  plnsol,epto,eqv" > \n
  "  set,next" > \n
  "*enddo" > \n
  "/show,close" \n
  \n
  "!! cycexpand,on ! graphical expansion" \n
  \n
  )

(define-skeleton ansys-skeleton-output-to-file
  "In/Output to file skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- output to file --" \n
  "!! ------------------------------" \n
  \n
  "! --- 1.) write macro file without parameter substitution" \n
  "! *vwrite works only in batch mode" \n
  "*create,tmp,mac ! macro file, no parameter substitution!" \n
  "/output,tmp,out !redirect output to tmp.out" \n
  "bla=otto" \n
  "*vwrite,bla,otto" \n
  "%G %G" \n
  "/output" \n
  "*end !writes into file up to this command" > \n
  "!! can be used with the *use command to pass params into it" \n
  "!! /input does not allow parameters" \n
  \n
  "*list,tmp,mac ! display external file" \n
  "/input,tmp,mac,,:LABEL ! read from label LABEL onwards" \n
  \n
  "! --- 2.) redirect ansys text output to file" \n
  "/output,%Dir(1)%%FileName%,txt,," \n
  "/com, #Time/s, WireTemp/°C InsulationTemp/°C YokeTemp/°C" \n
  "/output" \n
  "*do,I,1,NS" \n
  "set,,,,,,,I" > \n
  "!! etable,Temptab,temp" > \n
  "*get,Tim,active,,set,time" > \n
  "*vget,TempArray(1),node,,temp" > \n
  "*vmask,WireMask(1)" > \n
  "*vscfun,WireT,mean,TempArray(1)" > \n
  "*vmask,InsuMask(1)" > \n
  "*vscfun,InsuT,mean,TempArray(1)" > \n
  "*vmask,YokeMask(1)" > \n
  "*vscfun,YokeT,mean,TempArray(1)" > \n
  "/output,%Dir(1)%%FileName%,txt,,append" > \n
  "/com, %Tim% %WireT% %InsuT% %YokeT%" > \n
  "/output" > \n
  "*enddo" > \n
  \n
  "tmp ! read tmp.mac into the interpreter" \n
  \n
  "! --- 3.) create a 'command' file test.mac with parameter substitution" \n
  "*create,test,mac !write macro file" \n
  "*cfopen,test,txt,,append ! appending to file" > \n
  "*cfwrite,A=5 ! interpreted output" \n
  "! SET strings are limited to 32 characters!!!" \n
  "*cfwrite,Strg1='# cylindrical magnet: radius = %Rad%'" \n
  "*cfwrite,Strg2=', length = %Len%'" \n
  "*cfwrite,Strg3=', distance, magnetic induction'" \n
  "*cfwrite,*vwrite,Strg1, Strg2, Strg3" \n
  "*cfwrite,%S %S% %S" \n
  "*vwrite,B(1,1),B(1,2)" > \n
  "%E %E" > \n
  "*cfclos ! close file" \n
  "*end ! end macro file" > \n
  "/input,test,mac,,:LABEL ! read macro file from label LABEL onwards" \n
  \n
  "!! --- 4.) output includes responses from commands, notes, errors and warnings" \n
  "/output,bla,mac !write macro file, overwrite content,up to 75 characters only!" \n
  "!/output,bla,mac,,append !append to macro file" \n
  "/com,*mwrite,B(1),'bla','txt'" \n
  "/com,%G" \n
  "/output !redirect to standard ouput " \n
  "/input,bla,mac" \n
  \n
  "!! -- 5.) output from /post26" \n
  "*get,Size,VARI,,NSETS !No of result sets" \n
  "*dim,Accx,array,Size" \n
  "*dim,Tim,array,Size" \n
  "*dim,Accy,array,Size" \n
  "vget,Accx(1),5,,0 ! post26 variable 5, 0:real into array" \n
  "vget,Accy(1),7,,1 ! variable 7 complex value" \n
  "vget,Tim(1),1 ! time or frequency" \n
  "!! - export arrays -" \n
  "*create,tmp,mac" \n
  "*cfopen,sim,csv ! don't indent the output format strings" \n
  "Strg='T AX AY'" \n
  "*vwrite,Strg" \n
  "%S" \n
  "*vwrite,Tim(1), Accx(1), Accy(1)" \n
  "%G %G %G" \n
  "*cfclos"> \n
  "*end"> \n
  "*list,tmp,mac" \n
  \n
  "/input,tmp,mac" \n
  "*list,sim,csv" \n
  \n
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" \n
  "!! --- input from file ---" \n
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" \n
  "! read in table" \n
  \n
  "*dim,Rolf,,5,5" \n
  "*do,I,1,5,1" \n
  "*do,J,1,5,1"> \n
  "Rolf(I,J)=(I+1)*J"> \n
  "*enddo"> \n
  "*enddo"> \n
  \n
  "*create,tmp,mac" \n
  "/output,tmp,out" \n
  "*vwrite,Rolf(1,1),Rolf(1,2),Rolf(1,3),Rolf(1,4),Rolf(1,5)" \n
  "% G % G % G %G %G" \n
  "/output" \n
  "*end"> \n
  "*list,tmp,mac" \n
  \n
  "/input,tmp,mac" \n
  "*list,tmp,out" \n
  \n
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" \n
  "!! --- graphical output ---" \n
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" \n
  "!! --- 1.) device ouput ---" \n
  \n
  "!! --- PNG ---" \n
  "!/color,wbak,whit !white background or:" \n
  "/RGB,index,100,100,100,0 !white background" \n
  "/RGB,index,0,0,0,15" \n
  "/gfile,1200 !set height resolution [800] to 1200, width=1.33*height" \n
  "/sys,rm file*.png" \n
  "/show,png !creates jobname###.png files" \n
  "pngr !additional options" \n
  "pngr,stat" \n
  "plvect,B" \n
  "/noerase !prevent screen erase" \n
  "lplot" \n
  "/show,close" \n
  "/erase !erase screen" \n
  "/sys,display file*.png" \n
  \n
  "!! --- EPS ---" \n
  "!! -- eps output, default white background" \n
  "pscr,color,2 ! coloured output" \n
  "pscr,paper,a4,landscape" \n
  "pscr,hires,1 !1:high resolution" \n
  "pscr,LWID,5 ! line width factor [3] 1-99" \n
  "pscr,stat" \n
  "/device,text,1,140 ! enlarge 140 % the text size" \n
  "/plopts,info,off !switch off all descriptive text" \n
  "/triad,off" \n
  "/plopts,wp,1 !display working plane" \n
  "/plopts,wp,off !switch off wp" \n
  "/plopts,frame,off !switch off graphics frame" \n
  "/title" \n
  "/sys,rm file*.eps" \n
  "/show,pscr,,,8! 8 colour planes" \n
  "eplot" \n
  "/show,close" \n
  "/sys,epstopdf file000.eps" \n
  "/sys,display file000.pdf" \n
  "*dim,Dir,string,248 ! string array with maximum of 248 characters!" \n
  "Dir(1) = '/very/very/very/long/path/to/heaven/'" \n
  "*stat,Dir" \n
  "/com, %Dir(1)%bla.mac! bug (V15) in /com: substitution not directly behind comma" \n
  "/syp,ls, Dir(1)" \n
  "File = 'eptoeqv_at_shaft_press-fit'" \n
  "/syp,mv file000.png, '%Dir(1)%%File%.png'" \n
  \n
  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" \n
  "!! --- 2.) screen capture" \n
  "! /image does not work in batch mode" \n
  "File = 'blabla'" \n
  "/image,save,%File%,xwd !write in xwd bitmap format" \n
  "/syp,mogrify -format png, '%File%.xwd'" \n
  \n
  "/sys,rm file*.jpg file*.eps file*.tiff" \n
  "/ui,copy,save,jpeg,graph,color,norm,portrait,yes,100 !100 max qality " \n
  "/sys,display *.jpg" \n
  "!! eps not supported by org, emacs' docview" \n
  "/ui,copy,save,pscr,graph,color,norm,portrait,yes,100 !100 max qality" \n
  "/sys,convert file000.eps file000.png" \n
  "/ui,copy,save,tiff,graph,color,norm,portrait,yes,100 !100 max qality" \n
  "/sys,ls *.eps" \n
  "/sys,display *.eps" \n
  \n
  )

(define-skeleton ansys-skeleton-select
  "The selections skeleton.
Select or deselect various elements: Geometry, elements, nodes,
  ..."  nil
  "!! .............................." \n
  "!@@@ - select stuff -" \n
  "!! .............................." \n
  \n
  "asel,,item,comp,vmin,vmax,vinc,kswp ![s] select new set" \n
  "asel,r, !reselect new set" \n
  "asel,a, !additionally select new set" \n
  "asel,u, !unselect new set" \n
  "asel,all, !select all entities" \n
  "asel,inve, !invert current set" \n
  "N1="
  \n
  "esel,s,adj|elem|cent|type|ename|mat|real|esys|part(ls-dyna)|live|layer|sec|stra|sfe|bfe|path|etab"\n
  "esel,a,ename,172 !select additionally conta172 elements" \n
  \n
  "!! lowest face No of element E from selected nodes" \n
  "!! a=nmface(E) !plane elements:faces =^= el. sides" \n
  "!! node in POS of element E" \n
  "bla=nelem(E,POS) !pos. ijklmnop =^= [1:8]" \n
  "!unsel midnodes of 8-node 2d elem" \n
  "*get,En,elem,,count !No of elements" \n
  "*get,E,elem,,num,min !min node No" \n
  "*do,I,1,En,1" \n
  "Face = nmface(E)" > \n
  "Ntmp = ndface(E,Face,3)"> \n
  "nsel,u,,,Ntmp"> \n
  "*get,E,elem,E,nxth" \n
  "*enddo"> \n
  \n
  )

(define-skeleton ansys-skeleton-path-plot
  "Path plot skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- path plot --" \n
  "!! ------------------------------" \n
  \n
  "!! avoid element borders for inaccuracies of the rounding algorithm." \n
  \n
  "path,Name,nPts[2],nSets[30],nDiv[20] ! define active path \"Name\"" \n
  "!ppath,P,N,x,y,z,CS !coord. in global cartesian csys, but use CS for path interpolation" \n
  "ppath,1,N   !define path point by node No N" \n
  "ppath,2,,X,Y,Z ! path point by coordinates" \n
  "pdef,By,b,y" \n
  "pcalc,add,Res,By,Bx,2 !add Bx to 2*By" \n
  "pcalc,intg,Res,By,S !integrate By with respect to the path length" \n
  "pcalc,intg,L,1,S ! path length?" \n
  "/axlab,x,distance !axis label" \n
  "psel,s,axis,...    	 !select multiple paths" \n
  "plpath,By		 !plot in graph" \n
  "plpagm,By,5		 !plot on geom." \n
  "!write into table variable content: x,y,z,path length?,v1,v2,..." \n
  "paget,Path,table" \n
  "!path $ stat" \n
  \n
  )

(define-skeleton ansys-skeleton-post26
  "Time postprocessing /post26 skeleton."
  nil
  "!@ ------------------------------" \n
  "!@ --- time-history postprocessing ---" \n
  "!! ------------------------------" \n
  \n
  "/post26" \n
  "numvar,200 !maximum No of variables, 1 is always time"
  "esol,2,1,,u,z,'displ z'" \n
  "nsol,2,1,u,z" \n
  "deriv,3,2,1,,vz !time derivative of uz" \n
  "extrem,2 !list (only real parts!) extrema" \n
  "*get,Max,vari,2,extrem,vmax! get max extreme value" \n
  "*get,Min,vari,2,extrem,vmin! get min extreme value" \n
  "*get,Maxr,vari,4,rtime,2230 !real part at freq or time 2230" \n
  "*get,Maxi,vari,4,itime,2230 !imag part at freq 2230" \n
  "rforce,3,1,f,z ! reaction force" \n
  "filldata,7,1,10,,20 !fill a variable by a ramp or constant" \n
  "add,4,2,,,displ,,,-1 !sum variables" \n
  "prod,3,2,,,,,,-N*2 !product of variables" \n
  "/grid,1" \n
  "/gmarker,1,1 !curve marking: 1: triangles,2: squares" \n
  "xvar,2 !specify the variable for the x-axis" \n
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
  "/tlabel,x,y,bla !annotation at (x,y)" \n
  "!! --- graphical output --- " \n
  "/color,wbak,whit !white background" \n
  "!! invert background colour" \n
  "/color,wbak,whit !white background" \n
  "/RGB,index,100,100,100,0" \n
  "/RGB,index,0,0,0,15" \n
  "/show,png !creates jobnameXXX.png files" \n
  "plvar,3" \n
  "/show,close" \n
  "!! -- listing of vars ---" \n
  "nprint,5 !whicht time points are to be listed" \n
  "prtime,min,max !time range to be listed"  \n
  "prvar,3" \n
  "!! --- data output --- " \n
  "*get,Size,VARI,,NSETS !No of sets" \n
  "*dim,Accx,array,Size" \n
  "*dim,Tim,array,Size" \n
  "*dim,Accy,array,Size" \n
  "vget,Accx(1),5 ! post26 variable 5 into array" \n
  "vget,Accy(1),7" \n
  "vget,Tim(1),1" \n
  "!! - export arrays -" \n
  "*create,tmp,mac" \n
  "*cfopen,sim,txt" \n
  "Strg='T AX AY'" \n
  "*vwrite,Strg" \n
  "%S" \n
  "*vwrite,Tim(1), Accx(1), Accy(1)" \n
  "%G %G %G" \n
  "*cfclos"> \n
  "*end" > \n
  "*list,tmp,mac" \n
  "/input,tmp,mac" \n
  "*list,sim,txt" \n
  \n
  )

(define-skeleton ansys-skeleton-array
  "Fields and arrays skeleton."
  nil
  "!@ ------------------------------" \n
  "!@@ -- \"table\" arrays --" \n
  "!! ------------------------------" \n
  \n
  "!! table arrays interpolate linearly between their values" \n
  "!! and might be accessed with real indices" \n
  "!! the last value remains constant" \n
  "NSS=100" \n
  "*dim,F_y,table,NSS,3! three columns" \n
  "F_y(0,1) = 1,2,3 ! column 'index'" \n
  "*do,I,1,NSS" \n
  "set,1,I"> \n
  "fsum !sum nodal forces of selected elements"> \n
  "*get,Tim,active,,set,time"> \n
  "Strain = Tim*100*Displ/Leng"> \n
  "F_y(I,0) = Strain ! row 'index'"> \n
  "*get,Forc,fsum,,item,fy"> \n
  "F_y(I,1) = Forc/(Width*Thick)"> \n
  "*enddo"> \n
  "!! e. g.: as harmonic acceleration load (with amplitude steps)" \n
  "!! primary value: frequency" \n
  "*taxis ! specify index values, only 10" \n
  "*taxis,Test(1,0),.1,.2,.3,.4,.5,.6,.7,.8,.9,.10" \n
  "Test(1,0) = .1,.2,.3,.4,.5,.6,.7,.8,.9,.10" \n
  "*toper,R3,R1,add,R2,2 !add tables with mult. factors" \n
  "*dim,mytab,table,5,1,,freq" \n
  "mytab(1,0)=   50,  199, 200, 999,1000" \n
  "mytab(1,1)=100e3,100e3,30e3,30e3,10e3" \n
  "acel,,,%mytab% !acceleration in global coordinates" \n
  "!! apply factor to table/array" \n
  "Fact = 1e-3" \n
  "*vfact = Fact" \n
  "*vstat ! list current values" \n
  "*vfun,mytab,copy,mytab" \n
  \n
  "!@ ------------------------------" \n
  "!@@ -- arrays --" \n
  "!! ------------------------------" \n
   \n
  \n
  "!@ ------------------------------" \n
  "!@@@ -- string arrays --" \n
  "!! ------------------------------" \n
   \n
   "*dim,Dir,string,248 ! maximum of 248 characters!" \n
   "Dir(1) = '/HOME/uidg1626/development/report/ej/95ks91leg0/'" \n
   "*stat,Dir" \n
   "/com, %Dir(1)%bla.mac! bug (V15) in /com: substitution not directly behind comma" \n
   "/syp,ls, Dir(1)" \n
   "File = 'eptoeqv_at_shaft_press-fit'" \n
   "/syp,mv file000.png, '%Dir(1)%%File%.png'" \n
  \n
  "!@ ------------------------------" \n
  "!@@@ -- Fortran arrays --" \n
  "!! ------------------------------" \n
   \n
  "*dim,A,,10,1 ! type array is default, No of rows, No of columns" \n
  "*del,B,,nopr !undocumented feature: deleting without warning" \n
  "*dim,B,table,10,1,1,TIME !table type interpolating" \n
  "B(1,0,1) = 0." \n
  "B(2,0,1) = 1." \n
  "*get,A,x,,item,fx" \n
  \n
  "*get,Nn,node,,count" \n
  "*vget,PAR,node,,nlist! array of nodenumbers" \n
  "A = 1,2,3,4,5" \n
  "*voper,R3,R1,prod,R2,50! operate on arrys" \n
  \n
  "!! -- check dimensions --" \n
  "*get,Dim,parm,A,dim,x" \n
  "*if,Dim,le,1,then" \n
  "! or just deleting with warning: A =" > \n
  "! deleting before (re)dimensioning without warning: *del,A,,nopr" > \n
  "*dim,A,array,10,1" > \n
  "*endif" > \n
  "*do,I,1,Ns" \n
  "set,Ls,I" > \n
  "fsum ! sum nodal forces of selected elements" \n
  "Reaction(I)=Fx" \n
  "*enddo" > \n
  "!! -- plotting --" \n
  "! arrays are plotted as histograms,tables are plotted as curves"
  "/gcol,1,'curve1'" \n
  "/gropt,fill,1 !fill lines"
  "/axlab,x,'x-variable in mm'" \n
  "/xrange,0,10 !xrange of plot" \n
  "*vplot,time(1,1),A(1,2)!plot column 2 of A " \n
  "/gmarker,1,3,10" \n
  "/gcolumn,1,'Neo-Hook'" \n
  "/gcolumn,2,'Mooney-R'" \n
  "/gcolumn,3,'Ogden'" \n
  "/gthk,curve,4 !curve thickness" \n
  "*vplot,F_y(1,0),F_y(1,1),2.0"
  \n
  )

(define-skeleton ansys-skeleton-structural-template
  "Minimum working structural APDL template."
  nil					;no interactor needed
  '(ansys-skeleton-header)
  "!@ ==============================" \n
  "!@ --- Preprocessing ---" \n
  "!@ ==============================" \n
  "/prep7" \n
  "!@@ -- Elements --" \n
  "Steel = 1" \n
  "ID = Steel" \n
  "real = Steel" \n
  "et,ID,solid186 !3d, 20 node" \n
  "!@@ -- Material --" \n
  "mp,nuxy,Steel,0.3 ! Poisson No" \n
  "mp,ex,Steel,200000 ! Elastic modulus" \n
  "!@@ -- Modeling --" \n
  "block,0,1,0,1,0,1" \n
  "!@@ -- Meshing --" \n
  "vmesh,all" \n
  "!@@ -- BCs, Loads --" \n
  "nsel,s,loc,x,0" \n
  "d,all,all" \n
  "nsel,s,loc,x,1" \n
  "d,all,uy,-.1" \n
  "allsel" \n
  "save" \n
  "!@ ==============================" \n
  "!@ --- Solving ---" \n
  "!@ ==============================" \n
  "/solu" \n
  "solve" \n
  "!@ ==============================" \n
  "!@ --- Postprocessing ---" \n
  "!@ ==============================" \n
  "/post1" \n
  "/view,,1,1,1" \n
  "plnsol,u,sum,2" \n
  "nsel,s,loc,x,0" \n
  "fsum !sum nodal forces/moments of selected elements" \n
  \n
  )

(define-skeleton ansys-skeleton-contact-template
  "Minimum working structural contact APDL template."
  nil					;no interactor needed
  '(ansys-skeleton-header)
  "!@ ==============================" \n
  "!@ --- Preprocessing ---" \n
  "!@ ==============================" \n
  "/prep7" \n
  "!@@ -- Elements --" \n
  "Steel = 1" \n
  "ID = Steel" \n
  "real = Steel" \n
  "et,ID,solid186 !3d, 20 node" \n
  "tid = 4" \n
  "cid = 3" \n
  "r,cid" \n
  "et,tid,170" \n
  "et,cid,174" \n
  "!@@ -- Material --" \n
  "mp,nuxy,Steel,0.3 ! Poisson No" \n
  "mp,ex,Steel,200000 ! Elastic modulus" \n
  "!@@ -- Modeling --" \n
  "block,0,1,0,1,0,1" \n
  "block,1,2,0,1,0,1" \n
  "!@@ -- Meshing --" \n
  "vmesh,all" \n
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
  "!@@ -- Loads --" \n
  "nsel,s,loc,x,0" \n
  "d,all,all" \n
  "nsel,s,loc,y,1" \n
  "esln,s" \n
  "sf,all,pres,1e3" \n
  "allsel" \n
  "save"  \n
  "!@ ==============================" \n
  "!@ --- Solving ---" \n
  "!@ ==============================" \n
  "/solu" \n
  "nsubst,10" \n
  "solve" \n
  "!@ ==============================" \n
  "!@ --- Postprocessing ---" \n
  "!@ ==============================" \n
  "/post1" \n
  "plnsol,u,sum" \n
  \n
  )

(defun ansys-skeleton-compilation-template ()
  "Your collection of selected code templates."
  (interactive)
  (ansys-skeleton-header)
  (goto-char (point-max))
  (ansys-skeleton-configuration)
  (goto-char (point-max))
  (ansys-skeleton-import)
  (goto-char (point-max))
  (ansys-skeleton-geometry)
  (goto-char (point-max))
  (ansys-skeleton-material-definition)
  (goto-char (point-max))
  (ansys-skeleton-element-definition)
  (goto-char (point-max))
  (ansys-skeleton-meshing)
  (goto-char (point-max))
  (ansys-skeleton-bc)
  (goto-char (point-max))
  (ansys-skeleton-solve)
  (goto-char (point-max))
  (ansys-skeleton-post1))

(define-skeleton ansys-skeleton-outline-template
  "Insert outline framework into an ANSYS APDL file."
  "Insert brief purpose of file: "
  "!@ ==============================" \n
  "!" ansys-outline-string " --- Header ---" \n
  "!@ ==============================" \n
  \n
  "!! FILENAME: " (buffer-file-name) \n
  "!! CREATION DATE: " (current-time-string) \n
  "!! ANSYS VERSION: " ansys-current-ansys-version \n
  "!! DESCRIPTION: " str \n
  \n
  "!@ ==============================" \n
  "!" ansys-outline-string " --- Setup ---" \n
  "!@ ==============================" \n
  \n
  "finish " \n
  "!/clear" \n
  "!y" \n
  "*afun,rad ![rad],deg" \n
  "Pi = acos(-1)" \n
  "*afun,deg !Use degrees [rad] for input and output angular functions" \n
  "/title," \n
  \n
  "!@ ==============================" \n
  "!" ansys-outline-string " --- Preprocessing --- " \n
  "!@ ==============================" \n
  \n
  "!@ ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string " -- Cad Import -- " \n
  "!! ------------------------------" \n
  \n
  "/aux15 !Enter the IGES file transfer processor" \n
  "!ioptn, Lab, VAL1 !Control options relating to importing" \n
  "!igesin, Fname, Ext," \n
  "!/facet,fine" \n
  "/title,new title" \n
  "!!otherwise the title is defined with the iges import" \n
  \n
  "!@ ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string " -- General Preprocessing -- " \n
  "!! ------------------------------" \n
  \n
  "/prep7" \n
  \n
  "!! .............................." \n
  "!" ansys-outline-string ansys-outline-string ansys-outline-string " - Materials and element types -" \n
  "!! .............................." \n
  \n
  "!@@@ --- Materials ---" \n
  \n
  "!@@@ --- Solid elements ---" \n
  \n
  "!@@@ --- Contact elements ---" \n
  \n
  "!! .............................." \n
  "!" ansys-outline-string ansys-outline-string ansys-outline-string " - Geometry -" \n
  "!! .............................." \n
  \n
  "!! .............................." \n
  "!" ansys-outline-string ansys-outline-string ansys-outline-string " - Meshing -" \n
  "!! .............................." \n
  \n
  "!! .............................." \n
  "!" ansys-outline-string ansys-outline-string ansys-outline-string " - Boundary conditions -" \n
  "!! .............................." \n
  \n
  "!@ ==============================" \n
  "!" ansys-outline-string " --- Solution --- " \n
  "!@ ==============================" \n
  \n
  "/solu" \n
  "allsel" \n
  \n
  "!@ ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string
  " -- Solution controls -- " \n
  "!! ------------------------------" \n
  \n
  "!@ ==============================" \n
  "!" ansys-outline-string " --- Postprocessing ---" \n
  "!@ ==============================" \n
  \n
  "!@ ------------------------------" \n
  "!" ansys-outline-string ansys-outline-string
  " -- General Postprocessing -- " \n
  "!! ------------------------------" \n
  \n
  "/post1" \n
  \n
  "!@ ------------------------------" \n
  "!" ansys-outline-string
  ansys-outline-string " -- Time-History Postprocessing --" \n
  "!! ------------------------------" \n
  \n
  "/post26" \n
  \n
  "/eof ------------------------------" \n
  )


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
		   "then") > _ \n
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
   ",then" _ >
     \n) 			;-- e o subskeleton
  ;; else subskeleton
  '(if (y-or-n-p "*else construct? ")
      (insert "*else")) >
  "\n*endif" >
  "\n")

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
