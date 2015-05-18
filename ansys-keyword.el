;; ansys-keyword.el -- Ansys mode completion and highlighting variables. 
;; This file was built by "ansys-fontification.el" release 16.1.1.

;; Copyright (C) 2006 - 2015 H. Dieter Wilhelm.

(defconst ansys-help-index
'(("*ABBR" "Hlp_C_ABBR.html") ("*AFUN" "Hlp_C_AFUN.html") ("*ASK" "Hlp_C_ASK.html") ("*AXPY" "Hlp_C_AXPY.html") ("*CFCLOS" "Hlp_C_CFCLOS.html") ("*CFOPEN" "Hlp_C_CFOPEN.html") ("*CFWRITE" "Hlp_C_CFWRITE.html") ("*COMP" "Hlp_C_COMP.html") ("*CREATE" "Hlp_C_CREATE.html") ("*CYCLE" "Hlp_C_CYCLE.html") ("*DEL" "Hlp_C_DEL.html") ("*DIM" "Hlp_C_DIM.html") ("*DMAT" "Hlp_C_DMAT.html") ("*DO" "Hlp_C_DO.html") ("*DOT" "Hlp_C_DOT.html") ("*DOWHILE" "Hlp_C_DOWHILE.html") ("*EIGEN" "Hlp_C_EIGEN.html") ("*ELSE" "Hlp_C_ELSE.html") ("*ELSEIF" "Hlp_C_ELSEIF.html") ("*END" "Hlp_C_END.html") ("*ENDDO" "Hlp_C_ENDDO.html") ("*ENDIF" "Hlp_C_ENDIF.html") ("*EXIT" "Hlp_C_EXIT_st.html") ("*EXPORT" "Hlp_C_EXPORT.html") ("*FFT" "Hlp_C_FFT.html") ("*FREE" "Hlp_C_FREE.html") ("*GET" "Hlp_C_GET.html") ("*GO" "Hlp_C_GO_st.html") ("*IF" "Hlp_C_IF.html") ("*INIT" "Hlp_C_INIT.html") ("*ITENGINE" "Hlp_C_ITENGINE.html") ("*LIST" "Hlp_C_LIST_st.html") ("*LSBAC" "Hlp_C_LSBAC.html") ("*LSDUMP" "Hlp_C_LSDUMP.html") ("*LSENGINE" "Hlp_C_LSENGINE.html") ("*LSFACTOR" "Hlp_C_LSFACTOR.html") ("*LSRESTORE" "Hlp_C_LSRESTORE.html") ("*MFOURI" "Hlp_C_MFOURI.html") ("*MFUN" "Hlp_C_MFUN.html") ("*MOPER" "Hlp_C_MOPER.html") ("*MSG" "Hlp_C_MSG.html") ("*MULT" "Hlp_C_MULT.html") ("*MWRITE" "Hlp_C_MWRITE_st.html") ("*NRM" "Hlp_C_NRM.html") ("*PRINT" "Hlp_C_PRINT_a.html") ("*REPEAT" "Hlp_C_REPEAT.html") ("*RETURN" "Hlp_C_RETURN.html") ("*SET" "Hlp_C_SET_st.html") ("*SMAT" "Hlp_C_SMAT.html") ("*SREAD" "Hlp_C_SREAD_st.html") ("*STATUS" "Hlp_C_STATUS_st.html") ("*TAXIS" "Hlp_C_TAXIS.html") ("*TOPER" "Hlp_C_TOPER_st.html") ("*TREAD" "Hlp_C_TREAD.html") ("*ULIB" "Hlp_C_ULIB.html") ("*USE" "Hlp_C_USE.html") ("*VABS" "Hlp_C_VABS.html") ("*VCOL" "Hlp_C_VCOL.html") ("*VCUM" "Hlp_C_VCUM.html") ("*VEC" "Hlp_C_VEC.html") ("*VEDIT" "Hlp_C_VEDIT.html") ("*VFACT" "Hlp_C_VFACT.html") ("*VFILL" "Hlp_C_VFILL.html") ("*VFUN" "Hlp_C_VFUN.html") ("*VGET" "Hlp_C_VGET_st.html") ("*VITRP" "Hlp_C_VITRP.html") ("*VLEN" "Hlp_C_VLEN.html") ("*VMASK" "Hlp_C_VMASK.html") ("*VOPER" "Hlp_C_VOPER.html") ("*VPLOT" "Hlp_C_VPLOT_st.html") ("*VPUT" "Hlp_C_VPUT_st.html") ("*VREAD" "Hlp_C_VREAD.html") ("*VSCFUN" "Hlp_C_VSCFUN.html") ("*VSTAT" "Hlp_C_VSTAT.html") ("*VWRITE" "Hlp_C_VWRITE.html") ("*WRK" "Hlp_C_WRK.html") ("/AN3D" "Hlp_C_AN3D.html") ("/ANFILE" "Hlp_C_ANFILE.html") ("/ANGLE" "Hlp_C_ANGLE.html") ("/ANNOT" "Hlp_C_ANNOT.html") ("/ANUM" "Hlp_C_ANUM.html") ("/ASSIGN" "Hlp_C_ASSIGN.html") ("/AUTO" "Hlp_C_AUTO.html") ("/AUX12" "Hlp_C_AUX12.html") ("/AUX15" "Hlp_C_AUX15.html") ("/AUX2" "Hlp_C_AUX2.html") ("/AUX3" "Hlp_C_AUX3_sl.html") ("/AXLAB" "Hlp_C_AXLAB.html") ("/BATCH" "Hlp_C_BATCH.html") ("/CFORMAT" "Hlp_C_CFORMAT_sl.html") ("/CLABEL" "Hlp_C_CLABEL.html") ("/CLEAR" "Hlp_C_CLEAR.html") ("/CLOG" "Hlp_C_CLOG_sl.html") ("/CMAP" "Hlp_C_CMAP.html") ("/COLOR" "Hlp_C_COLOR.html") ("/COM" "Hlp_C_COM.html") ("/CONFIG" "Hlp_C_CONFIG.html") ("/CONTOUR" "Hlp_C_CONTOUR.html") ("/COPY" "Hlp_C_COPY.html") ("/CPLANE" "Hlp_C_CPLANE.html") ("/CTYPE" "Hlp_C_CTYPE.html") ("/CVAL" "Hlp_C_CVAL.html") ("/CWD" "Hlp_C_CWD_sl.html") ("/CYCEXPAND" "Hlp_C_CYCEXPAND_sl.html") ("/DELETE" "Hlp_C_DELETE_sl.html") ("/DEVDISP" "Hlp_C_DEVDISP.html") ("/DEVICE" "Hlp_C_DEVICE.html") ("/DFLAB" "Hlp_C_DFLAB.html") ("/DIRECTORY" "Hlp_C_DIRECTORY.html") ("/DIST" "Hlp_C_DIST.html") ("/DSCALE" "Hlp_C_DSCALE_sl.html") ("/DV3D" "Hlp_C_DV3D.html") ("/EDGE" "Hlp_C_EDGE.html") ("/EFACET" "Hlp_C_EFACET.html") ("/EOF" "Hlp_C_EOF.html") ("/ERASE" "Hlp_C_ERASE_sl.html") ("/ESHAPE" "Hlp_C_ESHAPE.html") ("/EXIT" "Hlp_C_EXIT.html") ("/EXPAND" "Hlp_C_EXPAND_sl.html") ("/FACET" "Hlp_C_FACET.html") ("/FDELE" "Hlp_C_FDELE_sl.html") ("/FILNAME" "Hlp_C_FILNAME.html") ("/FOCUS" "Hlp_C_FOCUS.html") ("/FORMAT" "Hlp_C_FORMAT.html") ("/GCMD" "Hlp_C_GCMD.html") ("/GCOLUMN" "Hlp_C_GCOLUMN.html") ("/GFILE" "Hlp_C_GFILE.html") ("/GFORMAT" "Hlp_C_GFORMAT.html") ("/GLINE" "Hlp_C_GLINE.html") ("/GMARKER" "Hlp_C_GMARKER.html") ("/GO" "Hlp_C_GO.html") ("/GOLIST" "Hlp_C_GOLIST.html") ("/GOPR" "Hlp_C_GOPR.html") ("/GRAPHICS" "Hlp_C_GRAPHICS.html") ("/GRESUME" "Hlp_C_GRESUME.html") ("/GRID" "Hlp_C_GRID.html") ("/GROPT" "Hlp_C_GROPT.html") ("/GRTYP" "Hlp_C_GRTYP.html") ("/GSAVE" "Hlp_C_GSAVE.html") ("/GST" "Hlp_C_GST.html") ("/GTHK" "Hlp_C_GTHK.html") ("/GTYPE" "Hlp_C_GTYPE.html") ("/HBC" "Hlp_C_HBC.html") ("/HEADER" "Hlp_C_HEADER.html") ("/ICLWID" "Hlp_C_ICLWID.html") ("/ICSCALE" "Hlp_C_ICSCALE.html") ("/IMAGE" "Hlp_C_IMAGE.html") ("/INPUT" "Hlp_C_INPUT.html") ("/INQUIRE" "Hlp_C_INQUIRE.html") ("/LARC" "Hlp_C_LARC_sl.html") ("/LIGHT" "Hlp_C_LIGHT.html") ("/LINE" "Hlp_C_LINE_sl.html") ("/LSPEC" "Hlp_C_LSPEC.html") ("/LSYMBOL" "Hlp_C_LSYMBOL.html") ("/MAIL" "Hlp_C_MAIL.html") ("/MAP" "Hlp_C_MAP_s.html") ("/MENU" "Hlp_C_MENU.html") ("/MKDIR" "Hlp_C_MKDIR.html") ("/MPLIB" "Hlp_C_MPLIB.html") ("/MREP" "Hlp_C_MREP.html") ("/MSTART" "Hlp_C_MSTART.html") ("/NERR" "Hlp_C_NERR.html") ("/NOERASE" "Hlp_C_NOERASE.html") ("/NOLIST" "Hlp_C_NOLIST.html") ("/NOPR" "Hlp_C_NOPR.html") ("/NORMAL" "Hlp_C_NORMAL.html") ("/NUMBER" "Hlp_C_NUMBER.html") ("/OUTPUT" "Hlp_C_OUTPUT.html") ("/PAGE" "Hlp_C_PAGE.html") ("/PBC" "Hlp_C_PBC.html") ("/PBF" "Hlp_C_PBF.html") ("/PCIRCLE" "Hlp_C_PCIRCLE.html") ("/PCOPY" "Hlp_C_PCOPY.html") ("/PDS" "Hlp_C_PDS_sl.html") ("/PLOPTS" "Hlp_C_PLOPTS.html") ("/PMACRO" "Hlp_C_PMACRO.html") ("/PMORE" "Hlp_C_PMORE.html") ("/PNUM" "Hlp_C_PNUM.html") ("/POLYGON" "Hlp_C_POLYGON.html") ("/POST1" "Hlp_C_POST1.html") ("/POST26" "Hlp_C_POST26.html") ("/PREP7" "Hlp_C_PREP7.html") ("/PSEARCH" "Hlp_C_PSEARCH.html") ("/PSF" "Hlp_C_PSF.html") ("/PSPEC" "Hlp_C_PSPEC_sl.html") ("/PSTATUS" "Hlp_C_PSTATUS.html") ("/PSYMB" "Hlp_C_PSYMB.html") ("/PWEDGE" "Hlp_C_PWEDGE.html") ("/QUIT" "Hlp_C_QUIT.html") ("/RATIO" "Hlp_C_RATIO.html") ("/RENAME" "Hlp_C_RENAME.html") ("/REPLOT" "Hlp_C_REPLOT.html") ("/RESET" "Hlp_C_RESET_sl.html") ("/RGB" "Hlp_C_RGB.html") ("/RMDIR" "Hlp_C_RMDIR.html") ("/SECLIB" "Hlp_C_SECLIB.html") ("/SEG" "Hlp_C_SEG.html") ("/SHADE" "Hlp_C_SHADE.html") ("/SHOW" "Hlp_C_SHOW.html") ("/SHOWDISP" "Hlp_C_SHOWDISP.html") ("/SHRINK" "Hlp_C_SHRINK.html") ("/SMBC" "Hlp_C_SMBC_sl.html") ("/SOLU" "Hlp_C_SOLU_sl.html") ("/SSCALE" "Hlp_C_SSCALE.html") ("/STATUS" "Hlp_C_STATUS.html") ("/STITLE" "Hlp_C_STITLE.html") ("/SYP" "Hlp_C_SYP.html") ("/SYS" "Hlp_C_SYS.html") ("/TEE" "Hlp_C_TEE_sl.html") ("/TITLE" "Hlp_C_TITLE.html") ("/TLABEL" "Hlp_C_TLABEL.html") ("/TRIAD" "Hlp_C_TRIAD.html") ("/TRLCY" "Hlp_C_TRLCY.html") ("/TSPEC" "Hlp_C_TSPEC.html") ("/TXTRE" "Hlp_C_TXTRE.html") ("/TYPE" "Hlp_C_TYPE_sl.html") ("/UCMD" "Hlp_C_UCMD.html") ("/UDOC" "Hlp_C_UDOC_sl.html") ("/UI" "Hlp_C_UI.html") ("/UIS" "Hlp_C_UIS.html") ("/UNITS" "Hlp_C_UNITS.html") ("/USER" "Hlp_C_USER.html") ("/VCONE" "Hlp_C_VCONE.html") ("/VIEW" "Hlp_C_VIEW.html") ("/VSCALE" "Hlp_C_VSCALE.html") ("/VUP" "Hlp_C_VUP.html") ("/WAIT" "Hlp_C_WAIT.html") ("/WINDOW" "Hlp_C_WINDOW.html") ("/XFRM" "Hlp_C_XFRM.html") ("/XRANGE" "Hlp_C_XRANGE.html") ("/YRANGE" "Hlp_C_YRANGE.html") ("/ZOOM" "Hlp_C_ZOOM.html") ("11" "Hlp_E_LINK11.html") ("110" "Hlp_E_INFIN110.html") ("111" "Hlp_E_INFIN111.html") ("116" "Hlp_E_FLUID116.html") ("12" "Hlp_E_CONTAC12.html") ("121" "Hlp_E_PLANE121.html") ("122" "Hlp_E_SOLID122.html") ("123" "Hlp_E_SOLID123.html") ("124" "Hlp_E_CIRCU124.html") ("125" "Hlp_E_CIRCU125.html") ("126" "Hlp_E_TRANS126.html") ("129" "Hlp_E_FLUID129.html") ("13" "Hlp_E_PLANE13.html") ("130" "Hlp_E_FLUID130.html") ("131" "Hlp_E_SHELL131.html") ("132" "Hlp_E_SHELL132.html") ("136" "Hlp_E_FLUID136.html") ("138" "Hlp_E_FLUID138.html") ("139" "Hlp_E_FLUID139.html") ("14" "Hlp_E_COMBIN14.html") ("144" "Hlp_E_ROM144.html") ("151" "Hlp_E_SURF151.html") ("152" "Hlp_E_SURF152.html") ("153" "Hlp_E_SURF153.html") ("154" "Hlp_E_SURF154.html") ("156" "Hlp_E_SURF156.html") ("157" "Hlp_E_SHELL157.html") ("159" "Hlp_E_SURF159.html") ("16" "Hlp_E_PIPE16.html") ("160" "Hlp_E_LINK160.html") ("161" "Hlp_E_BEAM161.html") ("162" "Hlp_E_PLANE162.html") ("163" "Hlp_E_SHELL163.html") ("164" "Hlp_E_SOLID164.html") ("165" "Hlp_E_COMBI165.html") ("166" "Hlp_E_MASS166.html") ("167" "Hlp_E_LINK167.html") ("168" "Hlp_E_SOLID168.html") ("169" "Hlp_E_TARGE169.html") ("170" "Hlp_E_TARGE170.html") ("171" "Hlp_E_CONTA171.html") ("172" "Hlp_E_CONTA172.html") ("173" "Hlp_E_CONTA173.html") ("174" "Hlp_E_CONTA174.html") ("175" "Hlp_E_CONTA175.html") ("176" "Hlp_E_CONTA176.html") ("177" "Hlp_E_CONTA177.html") ("178" "Hlp_E_CONTA178.html") ("179" "Hlp_E_PRETS179.html") ("18" "Hlp_E_PIPE18.html") ("180" "Hlp_E_LINK180.html") ("181" "Hlp_E_SHELL181.html") ("182" "Hlp_E_PLANE182.html") ("183" "Hlp_E_PLANE183.html") ("184" "Hlp_E_MPC184.html") ("184cyl" "Hlp_E_MPC184cyl.html") ("184gen" "Hlp_E_MPC184gen.html") ("184link" "Hlp_E_MPC184link.html") ("184orie" "Hlp_E_MPC184orie.html") ("184plan" "Hlp_E_MPC184plan.html") ("184poin" "Hlp_E_MPC184poin.html") ("184revo" "Hlp_E_MPC184revo.html") ("184scr" "Hlp_E_MPC184scr.html") ("184slid" "Hlp_E_MPC184slid.html") ("184slot" "Hlp_E_MPC184slot.html") ("184sphe" "Hlp_E_MPC184sphe.html") ("184tran" "Hlp_E_MPC184tran.html") ("184univ" "Hlp_E_MPC184univ.html") ("184weld" "Hlp_E_MPC184weld.html") ("185" "Hlp_E_SOLID185.html") ("186" "Hlp_E_SOLID186.html") ("187" "Hlp_E_SOLID187.html") ("188" "Hlp_E_BEAM188.html") ("189" "Hlp_E_BEAM189.html") ("190" "Hlp_E_SOLSH190.html") ("192" "Hlp_E_INTER192.html") ("193" "Hlp_E_INTER193.html") ("194" "Hlp_E_INTER194.html") ("195" "Hlp_E_INTER195.html") ("200" "Hlp_E_MESH200.html") ("201" "Hlp_E_FOLLW201.html") ("202" "Hlp_E_INTER202.html") ("203" "Hlp_E_INTER203.html") ("204" "Hlp_E_INTER204.html") ("205" "Hlp_E_INTER205.html") ("208" "Hlp_E_SHELL208.html") ("209" "Hlp_E_SHELL209.html") ("21" "Hlp_E_MASS21.html") ("212" "Hlp_E_CPT212.html") ("213" "Hlp_E_CPT213.html") ("214" "Hlp_E_COMBI214.html") ("215" "Hlp_E_CPT215.html") ("216" "Hlp_E_CPT216.html") ("217" "Hlp_E_CPT217.html") ("220" "Hlp_E_FLUID220.html") ("221" "Hlp_E_FLUID221.html") ("223" "Hlp_E_PLANE223.html") ("226" "Hlp_E_SOLID226.html") ("227" "Hlp_E_SOLID227.html") ("230" "Hlp_E_PLANE230.html") ("231" "Hlp_E_SOLID231.html") ("232" "Hlp_E_SOLID232.html") ("233" "Hlp_E_PLANE233.html") ("236" "Hlp_E_SOLID236.html") ("237" "Hlp_E_SOLID237.html") ("238" "Hlp_E_PLANE238.html") ("239" "Hlp_E_SOLID239.html") ("240" "Hlp_E_SOLID240.html") ("241" "Hlp_E_HSFLD241.html") ("242" "Hlp_E_HSFLD242.html") ("25" "Hlp_E_PLANE25.html") ("251" "Hlp_E_SURF251.html") ("252" "Hlp_E_SURF252.html") ("257" "Hlp_E_INFIN257.html") ("263" "Hlp_E_REINF263.html") ("264" "Hlp_E_REINF264.html") ("265" "Hlp_E_REINF265.html") ("27" "Hlp_E_MATRIX27.html") ("272" "Hlp_E_SOLID272.html") ("273" "Hlp_E_SOLID273.html") ("278" "Hlp_E_SOLID278.html") ("279" "Hlp_E_SOLID279.html") ("28" "Hlp_E_SHELL28.html") ("281" "Hlp_E_SHELL281.html") ("285" "Hlp_E_SOLID285.html") ("288" "Hlp_E_PIPE288.html") ("289" "Hlp_E_PIPE289.html") ("29" "Hlp_E_FLUID29.html") ("290" "Hlp_E_ELBOW290.html") ("30" "Hlp_E_FLUID30.html") ("300" "Hlp_E_USER300.html") ("31" "Hlp_E_LINK31.html") ("33" "Hlp_E_LINK33.html") ("34" "Hlp_E_LINK34.html") ("35" "Hlp_E_PLANE35.html") ("36" "Hlp_E_SOURC36.html") ("37" "Hlp_E_COMBIN37.html") ("38" "Hlp_E_FLUID38.html") ("39" "Hlp_E_COMBIN39.html") ("4" "Hlp_E_BEAM4.html") ("40" "Hlp_E_COMBIN40.html") ("41" "Hlp_E_SHELL41.html") ("42" "Hlp_E_PLANE42.html") ("45" "Hlp_E_SOLID45.html") ("47" "Hlp_E_INFIN47.html") ("5" "Hlp_E_SOLID5.html") ("50" "Hlp_E_MATRIX50.html") ("52" "Hlp_E_CONTAC52.html") ("53" "Hlp_E_PLANE53.html") ("55" "Hlp_E_PLANE55.html") ("59" "Hlp_E_PIPE59.html") ("61" "Hlp_E_SHELL61.html") ("63" "Hlp_E_SHELL63.html") ("65" "Hlp_E_SOLID65.html") ("68" "Hlp_E_LINK68.html") ("70" "Hlp_E_SOLID70.html") ("71" "Hlp_E_MASS71.html") ("75" "Hlp_E_PLANE75.html") ("77" "Hlp_E_PLANE77.html") ("78" "Hlp_E_PLANE78.html") ("79" "Hlp_E_FLUID79.html") ("80" "Hlp_E_FLUID80.html") ("81" "Hlp_E_FLUID81.html") ("82" "Hlp_E_PLANE82.html") ("83" "Hlp_E_PLANE83.html") ("87" "Hlp_E_SOLID87.html") ("9" "Hlp_E_INFIN9.html") ("90" "Hlp_E_SOLID90.html") ("92" "Hlp_E_SOLID92.html") ("94" "Hlp_E_CIRCU94.html") ("95" "Hlp_E_SOLID95.html") ("96" "Hlp_E_SOLID96.html") ("97" "Hlp_E_SOLID97.html") ("98" "Hlp_E_SOLID98.html") ("A" "Hlp_C_A.html") ("AADD" "Hlp_C_AADD.html") ("AATT" "Hlp_C_AATT.html") ("ABBRES" "Hlp_C_ABBRES.html") ("ABBSAV" "Hlp_C_ABBSAV.html") ("ABEXTRACT" "Hlp_C_ABEXTRACT.html") ("ABS" "Hlp_C_ABS.html") ("ACCAT" "Hlp_C_ACCAT.html") ("ACCOPTION" "Hlp_C_ACCOPTION.html") ("ACEL" "Hlp_C_ACEL.html") ("ACLEAR" "Hlp_C_ACLEAR.html") ("ADAMS" "Hlp_C_ADAMS.html") ("ADAPT" "Hlp_C_ADAPT.html") ("ADD" "Hlp_C_ADD.html") ("ADDAM" "Hlp_C_ADDAM.html") ("ADELE" "Hlp_C_ADELE.html") ("ADGL" "Hlp_C_ADGL.html") ("ADRAG" "Hlp_C_ADRAG.html") ("AESIZE" "Hlp_C_AESIZE.html") ("AFILLT" "Hlp_C_AFILLT.html") ("AFLIST" "Hlp_C_AFLIST.html") ("AFSURF" "Hlp_C_AFSURF.html") ("AGEN" "Hlp_C_AGEN.html") ("AGLUE" "Hlp_C_AGLUE.html") ("AINA" "Hlp_C_AINA.html") ("AINP" "Hlp_C_AINP.html") ("AINV" "Hlp_C_AINV.html") ("AL" "Hlp_C_AL.html") ("ALIST" "Hlp_C_ALIST.html") ("\"ALL\"BEAMS" "Hlp_E_CH3_2.html#allbeams") ("\"ALL\"CIRCUS" "Hlp_E_CH3_2.html#allcircu") ("\"ALL\"COMBINS" "Hlp_E_CH3_2.html#allcombin") ("\"ALL\"COMBIS" "Hlp_E_CH3_2.html#allcombin") ("\"ALL\"CONTACS" "Hlp_E_CH3_2.html#allcontac") ("\"ALL\"CONTAS" "Hlp_E_CH3_2.html#allcontac") ("\"ALL\"CPTS" "Hlp_E_CH3_2.html#allcpt") ("\"ALL\"FLUIDS" "Hlp_E_CH3_2.html#allfluid") ("\"ALL\"FOLLW" "Hlp_E_CH3_2.html#allfollw") ("\"ALL\"HSFLDS" "Hlp_E_CH3_2.html#allhsfld") ("\"ALL\"INFINS" "Hlp_E_CH3_2.html#allinfin") ("\"ALL\"INTERS" "Hlp_E_CH3_2.html#allinter") ("\"ALL\"LINKS" "Hlp_E_CH3_2.html#alllinks") ("\"ALL\"MASS" "Hlp_E_CH3_2.html#allmass") ("\"ALL\"MATRIXS" "Hlp_E_CH3_2.html#allmatrix") ("\"ALL\"MESHS" "Hlp_E_CH3_2.html#allmesh") ("\"ALL\"MPCS" "Hlp_E_CH3_2.html#allmpc") ("\"ALL\"PIPES" "Hlp_E_CH3_2.html#allpipes") ("\"ALL\"PLANES" "Hlp_E_CH3_2.html#allplanes") ("\"ALL\"PRETS" "Hlp_E_CH3_2.html#allprets") ("\"ALL\"REINF" "Hlp_E_CH3_2.html#allreinf") ("\"ALL\"ROMS" "Hlp_E_CH3_2.html#allrom") ("\"ALL\"SEL" "Hlp_C_ALLSEL.html") ("\"ALL\"SHELLS" "Hlp_E_CH3_2.html#allshells") ("\"ALL\"SOLIDS" "Hlp_E_CH3_2.html#allsolids") ("\"ALL\"SOLSHS" "Hlp_E_CH3_2.html#allsolsh") ("\"ALL\"SOURCS" "Hlp_E_CH3_2.html#allsourc") ("\"ALL\"SURFS" "Hlp_E_CH3_2.html#allsurf") ("\"ALL\"TARGES" "Hlp_E_CH3_2.html#alltarge") ("\"ALL\"TRANS" "Hlp_E_CH3_2.html#alltrans") ("ALPHAD" "Hlp_C_ALPHAD.html") ("AMAP" "Hlp_C_AMAP.html") ("AMESH" "Hlp_C_AMESH.html") ("ANCNTR" "Hlp_C_ANCNTR.html") ("ANCUT" "Hlp_C_ANCUT.html") ("ANCYC" "Hlp_C_ANCYC.html") ("ANDATA" "Hlp_C_ANDATA.html") ("ANDSCL" "Hlp_C_ANDSCL.html") ("ANDYNA" "Hlp_C_ANDYNA.html") ("ANFLOW" "Hlp_C_ANFLOW.html") ("ANHARM" "Hlp_C_ANHARM.html") ("ANIM" "Hlp_C_ANIM.html") ("ANISOS" "Hlp_C_ANISOS.html") ("ANMODE" "Hlp_C_ANMODE.html") ("ANMRES" "Hlp_C_ANMRES.html") ("ANORM" "Hlp_C_ANORM.html") ("ANPRES" "Hlp_C_ANPRES.html") ("ANSOL" "Hlp_C_ANSOL.html") ("ANSTOAQWA" "Hlp_C_ANSTOAQWA.html") ("ANSTOASAS" "Hlp_C_ANSTOASAS.html") ("ANTIME" "Hlp_C_ANTIME.html") ("ANTYPE" "Hlp_C_ANTYPE.html") ("AOFFST" "Hlp_C_AOFFST.html") ("AOVLAP" "Hlp_C_AOVLAP.html") ("APLOT" "Hlp_C_APLOT.html") ("APPEND" "Hlp_C_APPEND.html") ("APTN" "Hlp_C_APTN.html") ("ARCLEN" "Hlp_C_ARCLEN.html") ("ARCTRM" "Hlp_C_ARCTRM.html") ("AREAS" "Hlp_C_AREAS.html") ("AREFINE" "Hlp_C_AREFINE.html") ("AREMESH" "Hlp_C_AREMESH.html") ("AREVERSE" "Hlp_C_AREVERSE.html") ("AROTAT" "Hlp_C_AROTAT.html") ("ARSCALE" "Hlp_C_ARSCALE.html") ("ARSYM" "Hlp_C_ARSYM.html") ("ASBA" "Hlp_C_ASBA.html") ("ASBL" "Hlp_C_ASBL.html") ("ASBV" "Hlp_C_ASBV.html") ("ASBW" "Hlp_C_ASBW.html") ("ASCRES" "Hlp_C_ASCRES.html") ("ASEL" "Hlp_C_ASEL.html") ("ASIFILE" "Hlp_C_ASIFILE.html") ("ASKIN" "Hlp_C_ASKIN.html") ("ASLL" "Hlp_C_ASLL.html") ("ASLV" "Hlp_C_ASLV.html") ("ASOL" "Hlp_C_ASOL.html") ("ASUB" "Hlp_C_ASUB.html") ("ASUM" "Hlp_C_ASUM.html") ("ATAN" "Hlp_C_ATAN.html") ("ATRAN" "Hlp_C_ATRAN.html") ("ATYPE" "Hlp_C_ATYPE.html") ("AUTOTS" "Hlp_C_AUTOTS.html") ("AVPRIN" "Hlp_C_AVPRIN.html") ("AVRES" "Hlp_C_AVRES.html") ("AWAVE" "Hlp_C_AWAVE.html") ("BCSOPTION" "Hlp_C_BCSOPTION.html") ("BEAM" "Hlp_E_CH3_2.html#allbeams") ("BEAM161" "Hlp_E_BEAM161.html") ("BEAM188" "Hlp_E_BEAM188.html") ("BEAM189" "Hlp_E_BEAM189.html") ("BEAM4" "Hlp_E_BEAM4.html") ("BELLOW" "Hlp_C_BELLOW.html") ("BEND" "Hlp_C_BEND.html") ("BETAD" "Hlp_C_BETAD.html") ("BF" "Hlp_C_BF.html") ("BFA" "Hlp_C_BFA.html") ("BFADELE" "Hlp_C_BFADELE.html") ("BFALIST" "Hlp_C_BFALIST.html") ("BFCUM" "Hlp_C_BFCUM.html") ("BFDELE" "Hlp_C_BFDELE.html") ("BFE" "Hlp_C_BFE.html") ("BFECUM" "Hlp_C_BFECUM.html") ("BFEDELE" "Hlp_C_BFEDELE.html") ("BFELIST" "Hlp_C_BFELIST.html") ("BFESCAL" "Hlp_C_BFESCAL.html") ("BFINT" "Hlp_C_BFINT.html") ("BFK" "Hlp_C_BFK.html") ("BFKDELE" "Hlp_C_BFKDELE.html") ("BFKLIST" "Hlp_C_BFKLIST.html") ("BFL" "Hlp_C_BFL.html") ("BFLDELE" "Hlp_C_BFLDELE.html") ("BFLIST" "Hlp_C_BFLIST.html") ("BFLLIST" "Hlp_C_BFLLIST.html") ("BFSCALE" "Hlp_C_BFSCALE.html") ("BFTRAN" "Hlp_C_BFTRAN.html") ("BFUNIF" "Hlp_C_BFUNIF.html") ("BFV" "Hlp_C_BFV.html") ("BFVDELE" "Hlp_C_BFVDELE.html") ("BFVLIST" "Hlp_C_BFVLIST.html") ("BIOOPT" "Hlp_C_BIOOPT.html") ("BIOT" "Hlp_C_BIOT.html") ("BLC4" "Hlp_C_BLC4.html") ("BLC5" "Hlp_C_BLC5.html") ("BLOCK" "Hlp_C_BLOCK.html") ("BOOL" "Hlp_C_BOOL.html") ("BOPTN" "Hlp_C_BOPTN.html") ("BRANCH" "Hlp_C_BRANCH.html") ("BSAX" "Hlp_C_BSAX.html") ("BSM1" "Hlp_C_BSM1.html") ("BSM2" "Hlp_C_BSM2.html") ("BSMD" "Hlp_C_BSMD.html") ("BSPLIN" "Hlp_C_BSPLIN.html") ("BSS1" "Hlp_C_BSS1.html") ("BSS2" "Hlp_C_BSS2.html") ("BSTE" "Hlp_C_BSTE.html") ("BSTQ" "Hlp_C_BSTQ.html") ("BTOL" "Hlp_C_BTOL.html") ("BUCOPT" "Hlp_C_BUCOPT.html") ("C***" "Hlp_C_C.html") ("CALC" "Hlp_C_CALC.html") ("CAMPBELL" "Hlp_C_CAMPBELL.html") ("CAT5" "Hlp_C_CAT5IN.html") ("CBDOF" "Hlp_C_CBDOF.html") ("CBMD" "Hlp_C_CBMD.html") ("CBMX" "Hlp_C_CBMX.html") ("CBTE" "Hlp_C_CBTE.html") ("CBTMP" "Hlp_C_CBTMP.html") ("CDOPT" "Hlp_C_CDOPT.html") ("CDREAD" "Hlp_C_CDREAD.html") ("CDWRITE" "Hlp_C_CDWRITE.html") ("CE" "Hlp_C_CE.html") ("CECHECK" "Hlp_C_CECHECK.html") ("CECMOD" "Hlp_C_CECMOD.html") ("CECYC" "Hlp_C_CECYC.html") ("CEDELE" "Hlp_C_CEDELE.html") ("CEINTF" "Hlp_C_CEINTF.html") ("CELIST" "Hlp_C_CELIST.html") ("CENTER" "Hlp_C_CENTER.html") ("CEQN" "Hlp_C_CEQN.html") ("CERIG" "Hlp_C_CERIG.html") ("CESGEN" "Hlp_C_CESGEN.html") ("CFACT" "Hlp_C_CFACT.html") ("CGLOC" "Hlp_C_CGLOC.html") ("CGOMGA" "Hlp_C_CGOMGA.html") ("CGROW" "Hlp_C_CGROW.html") ("CH1TOC" "Hlp_C_CH1.html") ("CH2TOC" "Hlp_C_CH2.html") ("CH3TOC" "Hlp_C_CH3.html") ("CHECK" "Hlp_C_CHECK.html") ("CHKMSH" "Hlp_C_CHKMSH.html") ("CINT" "Hlp_C_CINT.html") ("CIRCLE" "Hlp_C_CIRCLE.html") ("CIRCU" "Hlp_E_CH3_2.html#allcircu") ("CIRCU124" "Hlp_E_CIRCU124.html") ("CIRCU125" "Hlp_E_CIRCU125.html") ("CIRCU94" "Hlp_E_CIRCU94.html") ("CISOL" "Hlp_C_CISOL.html") ("CLOCAL" "Hlp_C_CLOCAL.html") ("CLOG" "Hlp_C_CLOG.html") ("CLRMSHLN" "Hlp_C_CLRMSHLN.html") ("CM" "Hlp_C_CM.html") ("CMACEL" "Hlp_C_CMACEL.html") ("CMATRIX" "Hlp_C_CMATRIX.html") ("CMDELE" "Hlp_C_CMDELE.html") ("CMDOMEGA" "Hlp_C_CMDOMEGA.html") ("CMEDIT" "Hlp_C_CMEDIT.html") ("CMGRP" "Hlp_C_CMGRP.html") ("CMLIST" "Hlp_C_CMLIST.html") ("CMMOD" "Hlp_C_CMMOD.html") ("CMOMEGA" "Hlp_C_CMOMEGA.html") ("CMPLOT" "Hlp_C_CMPLOT.html") ("CMROTATE" "Hlp_C_CMROTATE.html") ("CMSEL" "Hlp_C_CMSEL.html") ("CMSFILE" "Hlp_C_CMSFILE.html") ("CMSOPT" "Hlp_C_CMSOPT.html") ("CMWRITE" "Hlp_C_CMWRITE.html") ("CNCHECK" "Hlp_C_CNCHECK.html") ("CNKMOD" "Hlp_C_CNKMOD.html") ("CNTR" "Hlp_C_CNTR.html") ("CNVTOL" "Hlp_C_CNVTOL.html") ("COMBI" "Hlp_E_CH3_2.html#allcombin") ("COMBI165" "Hlp_E_COMBI165.html") ("COMBI214" "Hlp_E_COMBI214.html") ("COMBIN" "Hlp_E_CH3_2.html#allcombin") ("COMBIN14" "Hlp_E_COMBIN14.html") ("COMBIN37" "Hlp_E_COMBIN37.html") ("COMBIN39" "Hlp_E_COMBIN39.html") ("COMBIN40" "Hlp_E_COMBIN40.html") ("COMBINE" "Hlp_C_COMBINE.html") ("COMPRESS" "Hlp_C_COMPRESS.html") ("CON4" "Hlp_C_CON4.html") ("CONE" "Hlp_C_CONE.html") ("CONJUG" "Hlp_C_CONJUG.html") ("CONTA" "Hlp_E_CH3_2.html#allcontac") ("CONTA171" "Hlp_E_CONTA171.html") ("CONTA172" "Hlp_E_CONTA172.html") ("CONTA173" "Hlp_E_CONTA173.html") ("CONTA174" "Hlp_E_CONTA174.html") ("CONTA175" "Hlp_E_CONTA175.html") ("CONTA176" "Hlp_E_CONTA176.html") ("CONTA177" "Hlp_E_CONTA177.html") ("CONTA178" "Hlp_E_CONTA178.html") ("CONTAC" "Hlp_E_CH3_2.html#allcontac") ("CONTAC12" "Hlp_E_CONTAC12.html") ("CONTAC52" "Hlp_E_CONTAC52.html") ("CORIOLIS" "Hlp_C_CORIOLIS.html") ("COUPLE" "Hlp_C_COUPLE.html") ("COVAL" "Hlp_C_COVAL.html") ("CP" "Hlp_C_CP.html") ("CPCYC" "Hlp_C_CPCYC.html") ("CPDELE" "Hlp_C_CPDELE.html") ("CPINTF" "Hlp_C_CPINTF.html") ("CPLGEN" "Hlp_C_CPLGEN.html") ("CPLIST" "Hlp_C_CPLIST.html") ("CPMERGE" "Hlp_C_CPMERGE.html") ("CPNGEN" "Hlp_C_CPNGEN.html") ("CPSGEN" "Hlp_C_CPSGEN.html") ("CPT" "Hlp_E_CH3_2.html#allcpt") ("CPT212" "Hlp_E_CPT212.html") ("CPT213" "Hlp_E_CPT213.html") ("CPT215" "Hlp_E_CPT215.html") ("CPT216" "Hlp_E_CPT216.html") ("CPT217" "Hlp_E_CPT217.html") ("CQC" "Hlp_C_CQC.html") ("CRPLIM" "Hlp_C_CRPLIM.html") ("CS" "Hlp_C_CS.html") ("CSCIR" "Hlp_C_CSCIR.html") ("CSDELE" "Hlp_C_CSDELE.html") ("CSKP" "Hlp_C_CSKP.html") ("CSLIST" "Hlp_C_CSLIST.html") ("CSWPLA" "Hlp_C_CSWPLA.html") ("CSYS" "Hlp_C_CSYS.html") ("CURR2D" "Hlp_C_CURR2D.html") ("CUTCONTROL" "Hlp_C_CUTCONTROL.html") ("CVAR" "Hlp_C_CVAR.html") ("CYCCALC" "Hlp_C_CYCCALC.html") ("CYCFILES" "Hlp_C_CYCFILES.html") ("CYCFREQ" "Hlp_C_CYCFREQ.html") ("CYCLIC" "Hlp_C_CYCLIC.html") ("CYCOPT" "Hlp_C_CYCOPT.html") ("CYCPHASE" "Hlp_C_CYCPHASE.html") ("CYCSPEC" "Hlp_C_CYCSPEC.html") ("CYL4" "Hlp_C_CYL4.html") ("CYL5" "Hlp_C_CYL5.html") ("CYLIND" "Hlp_C_CYLIND.html") ("CZDEL" "Hlp_C_CZDEL.html") ("CZMESH" "Hlp_C_CZMESH.html") ("Commands" "Hlp_C_CmdTOC.html") ("D" "Hlp_C_D.html") ("DA" "Hlp_C_DA.html") ("DADELE" "Hlp_C_DADELE.html") ("DALIST" "Hlp_C_DALIST.html") ("DAMORPH" "Hlp_C_DAMORPH.html") ("DATA" "Hlp_C_DATA.html") ("DATADEF" "Hlp_C_DATADEF.html") ("DCGOMG" "Hlp_C_DCGOMG.html") ("DCUM" "Hlp_C_DCUM.html") ("DCVSWP" "Hlp_C_DCVSWP.html") ("DDASPEC" "Hlp_C_DDASPEC.html") ("DDELE" "Hlp_C_DDELE.html") ("DDOPTION" "Hlp_C_DDOPTION.html") ("DEACT" "Hlp_C_DEACT.html") ("DEFINE" "Hlp_C_DEFINE.html") ("DELETE" "Hlp_C_DELETE.html") ("DELTIM" "Hlp_C_DELTIM.html") ("DEMORPH" "Hlp_C_DEMORPH.html") ("DERIV" "Hlp_C_DERIV.html") ("DESIZE" "Hlp_C_DESIZE.html") ("DESOL" "Hlp_C_DESOL.html") ("DETAB" "Hlp_C_DETAB.html") ("DFLX" "Hlp_C_DFLX.html") ("DFSWAVE" "Hlp_C_DFSWAVE.html") ("DIG" "Hlp_C_DIG.html") ("DIGIT" "Hlp_C_DIGIT.html") ("DISPLAY" "Hlp_C_DISPLAY.html") ("DJ" "Hlp_C_DJ.html") ("DJDELE" "Hlp_C_DJDELE.html") ("DJLIST" "Hlp_C_DJLIST.html") ("DK" "Hlp_C_DK.html") ("DKDELE" "Hlp_C_DKDELE.html") ("DKLIST" "Hlp_C_DKLIST.html") ("DL" "Hlp_C_DL.html") ("DLDELE" "Hlp_C_DLDELE.html") ("DLIST" "Hlp_C_DLIST.html") ("DLLIST" "Hlp_C_DLLIST.html") ("DMOVE" "Hlp_C_DMOVE.html") ("DMPEXT" "Hlp_C_DMPEXT.html") ("DMPOPTION" "Hlp_C_DMPOPTION.html") ("DMPRAT" "Hlp_C_DMPRAT.html") ("DMPSTR" "Hlp_C_DMPSTR.html") ("DNSOL" "Hlp_C_DNSOL.html") ("DOF" "Hlp_C_DOF.html") ("DOFSEL" "Hlp_C_DOFSEL.html") ("DOMEGA" "Hlp_C_DOMEGA.html") ("DSCALE" "Hlp_C_DSCALE.html") ("DSET" "Hlp_C_DSET.html") ("DSPOPTION" "Hlp_C_DSPOPTION.html") ("DSUM" "Hlp_C_DSUM.html") ("DSURF" "Hlp_C_DSURF.html") ("DSYM" "Hlp_C_DSYM.html") ("DSYS" "Hlp_C_DSYS.html") ("DTRAN" "Hlp_C_DTRAN.html") ("DUMP" "Hlp_C_DUMP.html") ("DVAL" "Hlp_C_DVAL.html") ("DVMORPH" "Hlp_C_DVMORPH.html") ("DYNOPT" "Hlp_C_DYNOPT.html") ("E" "Hlp_C_E.html") ("EALIVE" "Hlp_C_EALIVE.html") ("EDADAPT" "Hlp_C_EDADAPT.html") ("EDALE" "Hlp_C_EDALE.html") ("EDASMP" "Hlp_C_EDASMP.html") ("EDBOUND" "Hlp_C_EDBOUND.html") ("EDBVIS" "Hlp_C_EDBVIS.html") ("EDBX" "Hlp_C_EDBX.html") ("EDCADAPT" "Hlp_C_EDCADAPT.html") ("EDCGEN" "Hlp_C_EDCGEN.html") ("EDCLIST" "Hlp_C_EDCLIST.html") ("EDCMORE" "Hlp_C_EDCMORE.html") ("EDCNSTR" "Hlp_C_EDCNSTR.html") ("EDCONTACT" "Hlp_C_EDCONTACT.html") ("EDCPU" "Hlp_C_EDCPU.html") ("EDCRB" "Hlp_C_EDCRB.html") ("EDCSC" "Hlp_C_EDCSC.html") ("EDCTS" "Hlp_C_EDCTS.html") ("EDCURVE" "Hlp_C_EDCURVE.html") ("EDDAMP" "Hlp_C_EDDAMP.html") ("EDDBL" "Hlp_C_EDDBL.html") ("EDDC" "Hlp_C_EDDC.html") ("EDDRELAX" "Hlp_C_EDDRELAX.html") ("EDDUMP" "Hlp_C_EDDUMP.html") ("EDELE" "Hlp_C_EDELE.html") ("EDENERGY" "Hlp_C_EDENERGY.html") ("EDFPLOT" "Hlp_C_EDFPLOT.html") ("EDGCALE" "Hlp_C_EDGCALE.html") ("EDHGLS" "Hlp_C_EDHGLS.html") ("EDHIST" "Hlp_C_EDHIST.html") ("EDHTIME" "Hlp_C_EDHTIME.html") ("EDINT" "Hlp_C_EDINT.html") ("EDIPART" "Hlp_C_EDIPART.html") ("EDIS" "Hlp_C_EDIS.html") ("EDLCS" "Hlp_C_EDLCS.html") ("EDLOAD" "Hlp_C_EDLOAD.html") ("EDMP" "Hlp_C_EDMP.html") ("EDNB" "Hlp_C_EDNB.html") ("EDNDTSD" "Hlp_C_EDNDTSD.html") ("EDNROT" "Hlp_C_EDNROT.html") ("EDOPT" "Hlp_C_EDOPT.html") ("EDOUT" "Hlp_C_EDOUT.html") ("EDPART" "Hlp_C_EDPART.html") ("EDPC" "Hlp_C_EDPC.html") ("EDPL" "Hlp_C_EDPL.html") ("EDPVEL" "Hlp_C_EDPVEL.html") ("EDRC" "Hlp_C_EDRC.html") ("EDRD" "Hlp_C_EDRD.html") ("EDREAD" "Hlp_C_EDREAD.html") ("EDRI" "Hlp_C_EDRI.html") ("EDRST" "Hlp_C_EDRST.html") ("EDRUN" "Hlp_C_EDRUN.html") ("EDSHELL" "Hlp_C_EDSHELL.html") ("EDSOLV" "Hlp_C_EDSOLV.html") ("EDSP" "Hlp_C_EDSP.html") ("EDSTART" "Hlp_C_EDSTART.html") ("EDTERM" "Hlp_C_EDTERM.html") ("EDTP" "Hlp_C_EDTP.html") ("EDVEL" "Hlp_C_EDVEL.html") ("EDWELD" "Hlp_C_EDWELD.html") ("EDWRITE" "Hlp_C_EDWRITE.html") ("EEXTRUDE" "Hlp_C_EEXTRUDE.html") ("EGEN" "Hlp_C_EGEN.html") ("EINFIN" "Hlp_C_EINFIN.html") ("EINTF" "Hlp_C_EINTF.html") ("EKILL" "Hlp_C_EKILL.html") ("ELBOW" "Hlp_C_ELBOW.html") ("ELBOW290" "Hlp_E_ELBOW290.html") ("ELEM" "Hlp_C_ELEM.html") ("ELIST" "Hlp_C_ELIST.html") ("EMAGERR" "Hlp_C_EMAGERR.html") ("EMATWRITE" "Hlp_C_EMATWRITE.html") ("EMF" "Hlp_C_EMF.html") ("EMFT" "Hlp_C_EMFT.html") ("EMID" "Hlp_C_EMID.html") ("EMIS" "Hlp_C_EMIS.html") ("EMODIF" "Hlp_C_EMODIF.html") ("EMORE" "Hlp_C_EMORE.html") ("EMSYM" "Hlp_C_EMSYM.html") ("EMTGEN" "Hlp_C_EMTGEN.html") ("EMUNIT" "Hlp_C_EMUNIT.html") ("EN" "Hlp_C_EN.html") ("ENDRELEASE" "Hlp_C_ENDRELEASE.html") ("ENERSOL" "Hlp_C_ENERSOL.html") ("ENGEN" "Hlp_C_ENGEN.html") ("ENORM" "Hlp_C_ENORM.html") ("ENSYM" "Hlp_C_ENSYM.html") ("EORIENT" "Hlp_C_EORIENT.html") ("EPLOT" "Hlp_C_EPLOT.html") ("EQSLV" "Hlp_C_EQSLV.html") ("ERASE" "Hlp_C_ERASE.html") ("EREAD" "Hlp_C_EREAD.html") ("EREFINE" "Hlp_C_EREFINE.html") ("EREINF" "Hlp_C_EREINF.html") ("ERESX" "Hlp_C_ERESX.html") ("ERNORM" "Hlp_C_ERNORM.html") ("ERRANG" "Hlp_C_ERRANG.html") ("ESCHECK" "Hlp_C_ESCHECK.html") ("ESEL" "Hlp_C_ESEL.html") ("ESIZE" "Hlp_C_ESIZE.html") ("ESLA" "Hlp_C_ESLA.html") ("ESLL" "Hlp_C_ESLL.html") ("ESLN" "Hlp_C_ESLN.html") ("ESLV" "Hlp_C_ESLV.html") ("ESOL" "Hlp_C_ESOL.html") ("ESORT" "Hlp_C_ESORT.html") ("ESSOLV" "Hlp_C_ESSOLV.html") ("ESTIF" "Hlp_C_ESTIF.html") ("ESURF" "Hlp_C_ESURF.html") ("ESYM" "Hlp_C_ESYM.html") ("ESYS" "Hlp_C_ESYS.html") ("ET" "Hlp_C_ET.html") ("ETABLE" "Hlp_C_ETABLE.html") ("ETCHG" "Hlp_C_ETCHG.html") ("ETCONTROL" "Hlp_C_ETCONTROL.html") ("ETDELE" "Hlp_C_ETDELE.html") ("ETLIST" "Hlp_C_ETLIST.html") ("ETYPE" "Hlp_C_ETYPE.html") ("EUSORT" "Hlp_C_EUSORT.html") ("EWRITE" "Hlp_C_EWRITE.html") ("EXP" "Hlp_C_EXP.html") ("EXPAND" "Hlp_C_EXPAND.html") ("EXPASS" "Hlp_C_EXPASS.html") ("EXPROFILE" "Hlp_C_EXPROFILE.html") ("EXPSOL" "Hlp_C_EXPSOL.html") ("EXTOPT" "Hlp_C_EXTOPT.html") ("EXTREM" "Hlp_C_EXTREM.html") ("EXUNIT" "Hlp_C_EXUNIT.html") ("F" "Hlp_C_F.html") ("FATIGUE" "Hlp_C_FATIGUE.html") ("FC" "Hlp_C_FC.html") ("FCCHECK" "Hlp_C_FCCHECK.html") ("FCDELE" "Hlp_C_FCDELE.html") ("FCLIST" "Hlp_C_FCLIST.html") ("FCTYP" "Hlp_C_FCTYP.html") ("FCUM" "Hlp_C_FCUM.html") ("FDELE" "Hlp_C_FDELE.html") ("FE" "Hlp_C_FE.html") ("FEBODY" "Hlp_C_FEBODY.html") ("FECONS" "Hlp_C_FECONS.html") ("FEFOR" "Hlp_C_FEFOR.html") ("FELIST" "Hlp_C_FELIST.html") ("FESURF" "Hlp_C_FESURF.html") ("FILE" "Hlp_C_FILE.html") ("FILEAUX2" "Hlp_C_FILEAUX2.html") ("FILEAUX3" "Hlp_C_FILEAUX3.html") ("FILEDISP" "Hlp_C_FILEDISP.html") ("FILL" "Hlp_C_FILL.html") ("FILLDATA" "Hlp_C_FILLDATA.html") ("FINISH" "Hlp_C_FINISH.html") ("FITEM" "Hlp_C_FITEM.html") ("FJ" "Hlp_C_FJ.html") ("FJDELE" "Hlp_C_FJDELE.html") ("FJLIST" "Hlp_C_FJLIST.html") ("FK" "Hlp_C_FK.html") ("FKDELE" "Hlp_C_FKDELE.html") ("FKLIST" "Hlp_C_FKLIST.html") ("FL" "Hlp_C_FL.html") ("FLANGE" "Hlp_C_FLANGE.html") ("FLIST" "Hlp_C_FLIST.html") ("FLLIST" "Hlp_C_FLLIST.html") ("FLST" "Hlp_C_FLST.html") ("FLUID" "Hlp_E_CH3_2.html#allfluid") ("FLUID116" "Hlp_E_FLUID116.html") ("FLUID129" "Hlp_E_FLUID129.html") ("FLUID130" "Hlp_E_FLUID130.html") ("FLUID136" "Hlp_E_FLUID136.html") ("FLUID138" "Hlp_E_FLUID138.html") ("FLUID139" "Hlp_E_FLUID139.html") ("FLUID220" "Hlp_E_FLUID220.html") ("FLUID221" "Hlp_E_FLUID221.html") ("FLUID29" "Hlp_E_FLUID29.html") ("FLUID30" "Hlp_E_FLUID30.html") ("FLUID38" "Hlp_E_FLUID38.html") ("FLUID79" "Hlp_E_FLUID79.html") ("FLUID80" "Hlp_E_FLUID80.html") ("FLUID81" "Hlp_E_FLUID81.html") ("FLUREAD" "Hlp_C_FLUREAD.html") ("FLUXV" "Hlp_C_FLUXV.html") ("FMAGBC" "Hlp_C_FMAGBC.html") ("FMAGSUM" "Hlp_C_FMAGSUM.html") ("FOLLW" "Hlp_E_CH3_2.html#allfollw") ("FOLLW201" "Hlp_E_FOLLW201.html") ("FOR2D" "Hlp_C_FOR2D.html") ("FORCE" "Hlp_C_FORCE.html") ("FORM" "Hlp_C_FORM.html") ("FP" "Hlp_C_FP.html") ("FPLIST" "Hlp_C_FPLIST.html") ("FREQ" "Hlp_C_FREQ.html") ("FRQSCL" "Hlp_C_FRQSCL.html") ("FS" "Hlp_C_FS.html") ("FSCALE" "Hlp_C_FSCALE.html") ("FSDELE" "Hlp_C_FSDELE.html") ("FSLIST" "Hlp_C_FSLIST.html") ("FSNODE" "Hlp_C_FSNODE.html") ("FSPLOT" "Hlp_C_FSPLOT.html") ("FSSECT" "Hlp_C_FSSECT.html") ("FSSPARM" "Hlp_C_FSSPARM.html") ("FSUM" "Hlp_C_FSUM.html") ("FTCALC" "Hlp_C_FTCALC.html") ("FTRAN" "Hlp_C_FTRAN.html") ("FTSIZE" "Hlp_C_FTSIZE.html") ("FTWRITE" "Hlp_C_FTWRITE.html") ("FTYPE" "Hlp_C_FTYPE.html") ("FVMESH" "Hlp_C_FVMESH.html") ("GAP" "Hlp_C_GAP.html") ("GAPF" "Hlp_C_GAPF.html") ("GAUGE" "Hlp_C_GAUGE.html") ("GCDEF" "Hlp_C_GCDEF.html") ("GCGEN" "Hlp_C_GCGEN.html") ("GENOPT" "Hlp_C_GENOPT.html") ("GEOM" "Hlp_C_GEOM.html") ("GEOMETRY" "Hlp_C_GEOMETRY.html") ("GMATRIX" "Hlp_C_GMATRIX.html") ("GMFACE" "Hlp_C_GMFACE.html") ("GP" "Hlp_C_GP.html") ("GPDELE" "Hlp_C_GPDELE.html") ("GPLIST" "Hlp_C_GPLIST.html") ("GPLOT" "Hlp_C_GPLOT.html") ("GRP" "Hlp_C_GRP.html") ("GSBDATA" "Hlp_C_GSBDATA.html") ("GSGDATA" "Hlp_C_GSGDATA.html") ("GSLIST" "Hlp_C_GSLIST.html") ("GSSOL" "Hlp_C_GSSOL.html") ("GSUM" "Hlp_C_GSUM.html") ("HARFRQ" "Hlp_C_HARFRQ.html") ("HBMAT" "Hlp_C_HBMAT.html") ("HELP" "Hlp_C_HELP.html") ("HELPDISP" "Hlp_C_HELPDISP.html") ("HEMIOPT" "Hlp_C_HEMIOPT.html") ("HFANG" "Hlp_C_HFANG.html") ("HFSYM" "Hlp_C_HFSYM.html") ("HMAGSOLV" "Hlp_C_HMAGSOLV.html") ("HPGL" "Hlp_C_HPGL.html") ("HPTCREATE" "Hlp_C_HPTCREATE.html") ("HPTDELETE" "Hlp_C_HPTDELETE.html") ("HRCPLX" "Hlp_C_HRCPLX.html") ("HREXP" "Hlp_C_HREXP.html") ("HROCEAN" "Hlp_C_HROCEAN.html") ("HROPT" "Hlp_C_HROPT.html") ("HROUT" "Hlp_C_HROUT.html") ("HSFLD" "Hlp_E_CH3_2.html#allhsfld") ("HSFLD241" "Hlp_E_HSFLD241.html") ("HSFLD242" "Hlp_E_HSFLD242.html") ("IC" "Hlp_C_IC.html") ("ICDELE" "Hlp_C_ICDELE.html") ("ICLIST" "Hlp_C_ICLIST.html") ("IGESIN" "Hlp_C_IGESIN.html") ("IGESOUT" "Hlp_C_IGESOUT.html") ("IMAGIN" "Hlp_C_IMAGIN.html") ("IMESH" "Hlp_C_IMESH.html") ("IMMED" "Hlp_C_IMMED.html") ("IMPD" "Hlp_C_IMPD.html") ("INFIN" "Hlp_E_CH3_2.html#allinfin") ("INFIN110" "Hlp_E_INFIN110.html") ("INFIN111" "Hlp_E_INFIN111.html") ("INFIN257" "Hlp_E_INFIN257.html") ("INFIN47" "Hlp_E_INFIN47.html") ("INFIN9" "Hlp_E_INFIN9.html") ("INISTATE" "Hlp_C_INISTATE.html") ("INRES" "Hlp_C_INRES.html") ("INRTIA" "Hlp_C_INRTIA.html") ("INT1" "Hlp_C_INT1.html") ("INTER" "Hlp_E_CH3_2.html#allinter") ("INTER192" "Hlp_E_INTER192.html") ("INTER193" "Hlp_E_INTER193.html") ("INTER194" "Hlp_E_INTER194.html") ("INTER195" "Hlp_E_INTER195.html") ("INTER202" "Hlp_E_INTER202.html") ("INTER203" "Hlp_E_INTER203.html") ("INTER204" "Hlp_E_INTER204.html") ("INTER205" "Hlp_E_INTER205.html") ("INTSRF" "Hlp_C_INTSRF.html") ("IOPTN" "Hlp_C_IOPTN.html") ("IRLF" "Hlp_C_IRLF.html") ("IRLIST" "Hlp_C_IRLIST.html") ("Index" "ans_intro.html") ("JPEG" "Hlp_C_JPEG.html") ("JSOL" "Hlp_C_JSOL.html") ("K" "Hlp_C_K.html") ("KATT" "Hlp_C_KATT.html") ("KBC" "Hlp_C_KBC.html") ("KBETW" "Hlp_C_KBETW.html") ("KCALC" "Hlp_C_KCALC.html") ("KCENTER" "Hlp_C_KCENTER.html") ("KCLEAR" "Hlp_C_KCLEAR.html") ("KDELE" "Hlp_C_KDELE.html") ("KDIST" "Hlp_C_KDIST.html") ("KEEP" "Hlp_C_KEEP.html") ("KESIZE" "Hlp_C_KESIZE.html") ("KEYOPT" "Hlp_C_KEYOPT.html") ("KEYPTS" "Hlp_C_KEYPTS.html") ("KEYW" "Hlp_C_KEYW.html") ("KFILL" "Hlp_C_KFILL.html") ("KGEN" "Hlp_C_KGEN.html") ("KL" "Hlp_C_KL.html") ("KLIST" "Hlp_C_KLIST.html") ("KMESH" "Hlp_C_KMESH.html") ("KMODIF" "Hlp_C_KMODIF.html") ("KMOVE" "Hlp_C_KMOVE.html") ("KNODE" "Hlp_C_KNODE.html") ("KPLOT" "Hlp_C_KPLOT.html") ("KPSCALE" "Hlp_C_KPSCALE.html") ("KREFINE" "Hlp_C_KREFINE.html") ("KSCALE" "Hlp_C_KSCALE.html") ("KSCON" "Hlp_C_KSCON.html") ("KSEL" "Hlp_C_KSEL.html") ("KSLL" "Hlp_C_KSLL.html") ("KSLN" "Hlp_C_KSLN.html") ("KSUM" "Hlp_C_KSUM.html") ("KSYMM" "Hlp_C_KSYMM.html") ("KTRAN" "Hlp_C_KTRAN.html") ("KUSE" "Hlp_C_KUSE.html") ("KWPAVE" "Hlp_C_KWPAVE.html") ("KWPLAN" "Hlp_C_KWPLAN.html") ("L" "Hlp_C_L.html") ("L2ANG" "Hlp_C_L2ANG.html") ("L2TAN" "Hlp_C_L2TAN.html") ("LANG" "Hlp_C_LANG.html") ("LARC" "Hlp_C_LARC.html") ("LAREA" "Hlp_C_LAREA.html") ("LARGE" "Hlp_C_LARGE.html") ("LATT" "Hlp_C_LATT.html") ("LAYER" "Hlp_C_LAYER.html") ("LAYERP26" "Hlp_C_LAYERP26.html") ("LAYLIST" "Hlp_C_LAYLIST.html") ("LAYPLOT" "Hlp_C_LAYPLOT.html") ("LCABS" "Hlp_C_LCABS.html") ("LCASE" "Hlp_C_LCASE.html") ("LCCALC" "Hlp_C_LCCALC.html") ("LCCAT" "Hlp_C_LCCAT.html") ("LCDEF" "Hlp_C_LCDEF.html") ("LCFACT" "Hlp_C_LCFACT.html") ("LCFILE" "Hlp_C_LCFILE.html") ("LCLEAR" "Hlp_C_LCLEAR.html") ("LCOMB" "Hlp_C_LCOMB.html") ("LCOPER" "Hlp_C_LCOPER.html") ("LCSEL" "Hlp_C_LCSEL.html") ("LCSL" "Hlp_C_LCSL.html") ("LCSUM" "Hlp_C_LCSUM.html") ("LCWRITE" "Hlp_C_LCWRITE.html") ("LCZERO" "Hlp_C_LCZERO.html") ("LDELE" "Hlp_C_LDELE.html") ("LDIV" "Hlp_C_LDIV.html") ("LDRAG" "Hlp_C_LDRAG.html") ("LDREAD" "Hlp_C_LDREAD.html") ("LESIZE" "Hlp_C_LESIZE.html") ("LEXTND" "Hlp_C_LEXTND.html") ("LFILLT" "Hlp_C_LFILLT.html") ("LFSURF" "Hlp_C_LFSURF.html") ("LGEN" "Hlp_C_LGEN.html") ("LGLUE" "Hlp_C_LGLUE.html") ("LGWRITE" "Hlp_C_LGWRITE.html") ("LINA" "Hlp_C_LINA.html") ("LINE" "Hlp_C_LINE.html") ("LINES" "Hlp_C_LINES.html") ("LINK" "Hlp_E_CH3_2.html#alllinks") ("LINK11" "Hlp_E_LINK11.html") ("LINK160" "Hlp_E_LINK160.html") ("LINK167" "Hlp_E_LINK167.html") ("LINK180" "Hlp_E_LINK180.html") ("LINK31" "Hlp_E_LINK31.html") ("LINK33" "Hlp_E_LINK33.html") ("LINK34" "Hlp_E_LINK34.html") ("LINK68" "Hlp_E_LINK68.html") ("LINL" "Hlp_C_LINL.html") ("LINP" "Hlp_C_LINP.html") ("LINV" "Hlp_C_LINV.html") ("LIST" "Hlp_C_LIST.html") ("LLIST" "Hlp_C_LLIST.html") ("LMATRIX" "Hlp_C_LMATRIX.html") ("LMESH" "Hlp_C_LMESH.html") ("LNSRCH" "Hlp_C_LNSRCH.html") ("LOCAL" "Hlp_C_LOCAL.html") ("LOVLAP" "Hlp_C_LOVLAP.html") ("LPLOT" "Hlp_C_LPLOT.html") ("LPTN" "Hlp_C_LPTN.html") ("LREFINE" "Hlp_C_LREFINE.html") ("LREVERSE" "Hlp_C_LREVERSE.html") ("LROTAT" "Hlp_C_LROTAT.html") ("LSBA" "Hlp_C_LSBA.html") ("LSBL" "Hlp_C_LSBL.html") ("LSBV" "Hlp_C_LSBV.html") ("LSBW" "Hlp_C_LSBW.html") ("LSCLEAR" "Hlp_C_LSCLEAR.html") ("LSDELE" "Hlp_C_LSDELE.html") ("LSEL" "Hlp_C_LSEL.html") ("LSLA" "Hlp_C_LSLA.html") ("LSLK" "Hlp_C_LSLK.html") ("LSOPER" "Hlp_C_LSOPER.html") ("LSREAD" "Hlp_C_LSREAD.html") ("LSSCALE" "Hlp_C_LSSCALE.html") ("LSSOLVE" "Hlp_C_LSSOLVE.html") ("LSTR" "Hlp_C_LSTR.html") ("LSUM" "Hlp_C_LSUM.html") ("LSWRITE" "Hlp_C_LSWRITE.html") ("LSYMM" "Hlp_C_LSYMM.html") ("LTAN" "Hlp_C_LTAN.html") ("LTRAN" "Hlp_C_LTRAN.html") ("LUMPM" "Hlp_C_LUMPM.html") ("LVSCALE" "Hlp_C_LVSCALE.html") ("LWPLAN" "Hlp_C_LWPLAN.html") ("M" "Hlp_C_M.html") ("MADAPT" "Hlp_C_MADAPT.html") ("MAGOPT" "Hlp_C_MAGOPT.html") ("MAGSOLV" "Hlp_C_MAGSOLV.html") ("MAP" "Hlp_C_MAP.html") ("MAP2DTO3D" "Hlp_C_MAP2DTO3D.html") ("MAPSOLVE" "Hlp_C_MAPSOLVE.html") ("MAPVAR" "Hlp_C_MAPVAR.html") ("MASS" "Hlp_E_CH3_2.html#allmass") ("MASS166" "Hlp_E_MASS166.html") ("MASS21" "Hlp_E_MASS21.html") ("MASS71" "Hlp_E_MASS71.html") ("MASTER" "Hlp_C_MASTER.html") ("MAT" "Hlp_C_MAT.html") ("MATER" "Hlp_C_MATER.html") ("MATRIX" "Hlp_E_CH3_2.html#allmatrix") ("MATRIX27" "Hlp_E_MATRIX27.html") ("MATRIX50" "Hlp_E_MATRIX50.html") ("MCHECK" "Hlp_C_MCHECK.html") ("MDAMP" "Hlp_C_MDAMP.html") ("MDELE" "Hlp_C_MDELE.html") ("MDPLOT" "Hlp_C_MDPLOT.html") ("MEMM" "Hlp_C_MEMM.html") ("MESH" "Hlp_E_CH3_2.html#allmesh") ("MESH200" "Hlp_E_MESH200.html") ("MESHING" "Hlp_C_MESHING.html") ("MFANALYSIS" "Hlp_C_MFANALYSIS.html") ("MFBUCKET" "Hlp_C_MFBUCKET.html") ("MFCALC" "Hlp_C_MFCALC.html") ("MFCI" "Hlp_C_MFCI.html") ("MFCLEAR" "Hlp_C_MFCLEAR.html") ("MFCMMAND" "Hlp_C_MFCMMAND.html") ("MFCONV" "Hlp_C_MFCONV.html") ("MFDTIME" "Hlp_C_MFDTIME.html") ("MFELEM" "Hlp_C_MFELEM.html") ("MFEM" "Hlp_C_MFEM.html") ("MFEXTER" "Hlp_C_MFEXTER.html") ("MFFNAME" "Hlp_C_MFFNAME.html") ("MFFR" "Hlp_C_MFFR.html") ("MFIMPORT" "Hlp_C_MFIMPORT.html") ("MFINTER" "Hlp_C_MFINTER.html") ("MFITER" "Hlp_C_MFITER.html") ("MFLCOMM" "Hlp_C_MFLCOMM.html") ("MFLIST" "Hlp_C_MFLIST.html") ("MFMAP" "Hlp_C_MFMAP.html") ("MFORDER" "Hlp_C_MFORDER.html") ("MFOUTPUT" "Hlp_C_MFOUTPUT.html") ("MFPSIMUL" "Hlp_C_MFPSIMUL.html") ("MFRC" "Hlp_C_MFRC.html") ("MFRELAX" "Hlp_C_MFRELAX.html") ("MFRSTART" "Hlp_C_MFRSTART.html") ("MFSORDER" "Hlp_C_MFSORDER.html") ("MFSURFACE" "Hlp_C_MFSURFACE.html") ("MFTIME" "Hlp_C_MFTIME.html") ("MFTOL" "Hlp_C_MFTOL.html") ("MFVOLUME" "Hlp_C_MFVOLUME.html") ("MFWRITE" "Hlp_C_MFWRITE.html") ("MGEN" "Hlp_C_MGEN.html") ("MIDTOL" "Hlp_C_MIDTOL.html") ("MITER" "Hlp_C_MITER.html") ("MLIST" "Hlp_C_MLIST.html") ("MMASS" "Hlp_C_MMASS.html") ("MMF" "Hlp_C_MMF.html") ("MODCONT" "Hlp_C_MODCONT.html") ("MODE" "Hlp_C_MODE.html") ("MODIFY" "Hlp_C_MODIFY.html") ("MODMSH" "Hlp_C_MODMSH.html") ("MODOPT" "Hlp_C_MODOPT.html") ("MODSELOPTION" "Hlp_C_MODSELOPTION.html") ("MONITOR" "Hlp_C_MONITOR.html") ("MOPT" "Hlp_C_MOPT.html") ("MORPH" "Hlp_C_MORPH.html") ("MOVE" "Hlp_C_MOVE.html") ("MP" "Hlp_C_MP.html") ("MPAMOD" "Hlp_C_MPAMOD.html") ("MPC184" "Hlp_E_MPC184.html") ("MPC184-Cylin" "Hlp_E_MPC184cyl.html") ("MPC184-General" "Hlp_E_MPC184gen.html") ("MPC184-Link/Beam" "Hlp_E_MPC184link.html") ("MPC184-Orient" "Hlp_E_MPC184orie.html") ("MPC184-Planar" "Hlp_E_MPC184plan.html") ("MPC184-Point" "Hlp_E_MPC184poin.html") ("MPC184-Revolute" "Hlp_E_MPC184revo.html") ("MPC184-Screw" "Hlp_E_MPC184scr.html") ("MPC184-Slider" "Hlp_E_MPC184slid.html") ("MPC184-Slot" "Hlp_E_MPC184slot.html") ("MPC184-Spherical" "Hlp_E_MPC184sphe.html") ("MPC184-Trans" "Hlp_E_MPC184tran.html") ("MPC184-Universal" "Hlp_E_MPC184univ.html") ("MPC184-Weld" "Hlp_E_MPC184weld.html") ("MPCHG" "Hlp_C_MPCHG.html") ("MPCOPY" "Hlp_C_MPCOPY.html") ("MPDATA" "Hlp_C_MPDATA.html") ("MPDELE" "Hlp_C_MPDELE.html") ("MPDRES" "Hlp_C_MPDRES.html") ("MPLIST" "Hlp_C_MPLIST.html") ("MPPLOT" "Hlp_C_MPPLOT.html") ("MPREAD" "Hlp_C_MPREAD.html") ("MPRINT" "Hlp_C_MPRINT.html") ("MPTEMP" "Hlp_C_MPTEMP.html") ("MPTGEN" "Hlp_C_MPTGEN.html") ("MPTRES" "Hlp_C_MPTRES.html") ("MPWRITE" "Hlp_C_MPWRITE.html") ("MSAVE" "Hlp_C_MSAVE.html") ("MSHAPE" "Hlp_C_MSHAPE.html") ("MSHCOPY" "Hlp_C_MSHCOPY.html") ("MSHKEY" "Hlp_C_MSHKEY.html") ("MSHMID" "Hlp_C_MSHMID.html") ("MSHPATTERN" "Hlp_C_MSHPATTERN.html") ("MSOLVE" "Hlp_C_MSOLVE.html") ("MSTOLE" "Hlp_C_MSTOLE.html") ("MXPAND" "Hlp_C_MXPAND.html") ("N" "Hlp_C_N.html") ("NANG" "Hlp_C_NANG.html") ("NAXIS" "Hlp_C_NAXIS.html") ("NCNV" "Hlp_C_NCNV.html") ("NDELE" "Hlp_C_NDELE.html") ("NDIST" "Hlp_C_NDIST.html") ("NDSURF" "Hlp_C_NDSURF.html") ("NEQIT" "Hlp_C_NEQIT.html") ("NFORCE" "Hlp_C_NFORCE.html") ("NGEN" "Hlp_C_NGEN.html") ("NKPT" "Hlp_C_NKPT.html") ("NLADAPTIVE" "Hlp_C_NLADAPTIVE.html") ("NLDIAG" "Hlp_C_NLDIAG.html") ("NLDPOST" "Hlp_C_NLDPOST.html") ("NLGEOM" "Hlp_C_NLGEOM.html") ("NLHIST" "Hlp_C_NLHIST.html") ("NLIST" "Hlp_C_NLIST.html") ("NLMESH" "Hlp_C_NLMESH.html") ("NLOG" "Hlp_C_NLOG.html") ("NLOPT" "Hlp_C_NLOPT.html") ("NMODIF" "Hlp_C_NMODIF.html") ("NOCOLOR" "Hlp_C_NOCOLOR.html") ("NODES" "Hlp_C_NODES.html") ("NOOFFSET" "Hlp_C_NOOFFSET.html") ("NOORDER" "Hlp_C_NOORDER.html") ("NORA" "Hlp_C_NORA.html") ("NORL" "Hlp_C_NORL.html") ("NPLOT" "Hlp_C_NPLOT.html") ("NPRINT" "Hlp_C_NPRINT.html") ("NREAD" "Hlp_C_NREAD.html") ("NREFINE" "Hlp_C_NREFINE.html") ("NRLSUM" "Hlp_C_NRLSUM.html") ("NROPT" "Hlp_C_NROPT.html") ("NROTAT" "Hlp_C_NROTAT.html") ("NRRANG" "Hlp_C_NRRANG.html") ("NSCALE" "Hlp_C_NSCALE.html") ("NSEL" "Hlp_C_NSEL.html") ("NSLA" "Hlp_C_NSLA.html") ("NSLE" "Hlp_C_NSLE.html") ("NSLK" "Hlp_C_NSLK.html") ("NSLL" "Hlp_C_NSLL.html") ("NSLV" "Hlp_C_NSLV.html") ("NSMOOTH" "Hlp_C_NSMOOTH.html") ("NSOL" "Hlp_C_NSOL.html") ("NSORT" "Hlp_C_NSORT.html") ("NSTORE" "Hlp_C_NSTORE.html") ("NSUBST" "Hlp_C_NSUBST.html") ("NSVR" "Hlp_C_NSVR.html") ("NSYM" "Hlp_C_NSYM.html") ("NUMCMP" "Hlp_C_NUMCMP.html") ("NUMEXP" "Hlp_C_NUMEXP.html") ("NUMMRG" "Hlp_C_NUMMRG.html") ("NUMOFF" "Hlp_C_NUMOFF.html") ("NUMSTR" "Hlp_C_NUMSTR.html") ("NUMVAR" "Hlp_C_NUMVAR.html") ("NUSORT" "Hlp_C_NUSORT.html") ("NWPAVE" "Hlp_C_NWPAVE.html") ("NWPLAN" "Hlp_C_NWPLAN.html") ("NWRITE" "Hlp_C_NWRITE.html") ("OCDATA" "Hlp_C_OCDATA.html") ("OCDELETE" "Hlp_C_OCDELETE.html") ("OCLIST" "Hlp_C_OCLIST.html") ("OCREAD" "Hlp_C_OCREAD.html") ("OCTABLE" "Hlp_C_OCTABLE.html") ("OCTYPE" "Hlp_C_OCTYPE.html") ("OCZONE" "Hlp_C_OCZONE.html") ("OMEGA" "Hlp_C_OMEGA.html") ("OPERATE" "Hlp_C_OPERATE.html") ("OPNCONTROL" "Hlp_C_OPNCONTROL.html") ("OUTAERO" "OUTAERO.html") ("OUTOPT" "Hlp_C_OUTOPT.html") ("OUTPR" "Hlp_C_OUTPR.html") ("OUTRES" "Hlp_C_OUTRES.html") ("OVCHECK" "Hlp_C_OVCHECK.html") ("PADELE" "Hlp_C_PADELE.html") ("PAGET" "Hlp_C_PAGET.html") ("PAPUT" "Hlp_C_PAPUT.html") ("PARESU" "Hlp_C_PARESU.html") ("PARRES" "Hlp_C_PARRES.html") ("PARSAV" "Hlp_C_PARSAV.html") ("PARTSEL" "Hlp_C_PARTSEL.html") ("PASAVE" "Hlp_C_PASAVE.html") ("PATH" "Hlp_C_PATH.html") ("PAUSE" "Hlp_C_PAUSE.html") ("PCALC" "Hlp_C_PCALC.html") ("PCGOPT" "Hlp_C_PCGOPT.html") ("PCIRC" "Hlp_C_PCIRC.html") ("PCORRO" "Hlp_C_PCORRO.html") ("PCROSS" "Hlp_C_PCROSS.html") ("PDANL" "Hlp_C_PDANL.html") ("PDCDF" "Hlp_C_PDCDF.html") ("PDCFLD" "Hlp_C_PDCFLD.html") ("PDCLR" "Hlp_C_PDCLR.html") ("PDCMAT" "Hlp_C_PDCMAT.html") ("PDCORR" "Hlp_C_PDCORR.html") ("PDDMCS" "Hlp_C_PDDMCS.html") ("PDDOEL" "Hlp_C_PDDOEL.html") ("PDEF" "Hlp_C_PDEF.html") ("PDEXE" "Hlp_C_PDEXE.html") ("PDHIST" "Hlp_C_PDHIST.html") ("PDINQR" "Hlp_C_PDINQR.html") ("PDLHS" "Hlp_C_PDLHS.html") ("PDMETH" "Hlp_C_PDMETH.html") ("PDOT" "Hlp_C_PDOT.html") ("PDPINV" "Hlp_C_PDPINV.html") ("PDPLOT" "Hlp_C_PDPLOT.html") ("PDPROB" "Hlp_C_PDPROB.html") ("PDRAG" "Hlp_C_PDRAG.html") ("PDRESU" "Hlp_C_PDRESU.html") ("PDROPT" "Hlp_C_PDROPT.html") ("PDSAVE" "Hlp_C_PDSAVE.html") ("PDSCAT" "Hlp_C_PDSCAT.html") ("PDSENS" "Hlp_C_PDSENS.html") ("PDSHIS" "Hlp_C_PDSHIS.html") ("PDUSER" "Hlp_C_PDUSER.html") ("PDVAR" "Hlp_C_PDVAR.html") ("PDWRITE" "Hlp_C_PDWRITE.html") ("PERBC2D" "Hlp_C_PERBC2D.html") ("PERTURB" "Hlp_C_PERTURB.html") ("PFACT" "Hlp_C_PFACT.html") ("PFLUID" "Hlp_C_PFLUID.html") ("PGAP" "Hlp_C_PGAP.html") ("PHYSICS" "Hlp_C_PHYSICS.html") ("PINSUL" "Hlp_C_PINSUL.html") ("PIPE" "Hlp_C_PIPE.html") ("PIPE16" "Hlp_E_PIPE16.html") ("PIPE18" "Hlp_E_PIPE18.html") ("PIPE288" "Hlp_E_PIPE288.html") ("PIPE289" "Hlp_E_PIPE289.html") ("PIPE59" "Hlp_E_PIPE59.html") ("PIPES" "Hlp_E_CH3_2.html#allpipes") ("PIVCHECK" "Hlp_C_PIVCHECK.html") ("\"PLANES\"" "Hlp_E_CH3_2.html#allplanes") ("PLANE121" "Hlp_E_PLANE121.html") ("PLANE13" "Hlp_E_PLANE13.html") ("PLANE162" "Hlp_E_PLANE162.html") ("PLANE182" "Hlp_E_PLANE182.html") ("PLANE183" "Hlp_E_PLANE183.html") ("PLANE223" "Hlp_E_PLANE223.html") ("PLANE230" "Hlp_E_PLANE230.html") ("PLANE233" "Hlp_E_PLANE233.html") ("PLANE238" "Hlp_E_PLANE238.html") ("PLANE25" "Hlp_E_PLANE25.html") ("PLANE35" "Hlp_E_PLANE35.html") ("PLANE42" "Hlp_E_PLANE42.html") ("PLANE53" "Hlp_E_PLANE53.html") ("PLANE55" "Hlp_E_PLANE55.html") ("PLANE75" "Hlp_E_PLANE75.html") ("PLANE77" "Hlp_E_PLANE77.html") ("PLANE78" "Hlp_E_PLANE78.html") ("PLANE82" "Hlp_E_PLANE82.html") ("PLANE83" "Hlp_E_PLANE83.html") ("PLCAMP" "Hlp_C_PLCAMP.html") ("PLCFREQ" "Hlp_C_PLCFREQ.html") ("PLCHIST" "Hlp_C_PLCHIST.html") ("PLCINT" "Hlp_C_PLCINT.html") ("PLCPLX" "Hlp_C_PLCPLX.html") ("PLCRACK" "Hlp_C_PLCRACK.html") ("PLDISP" "Hlp_C_PLDISP.html") ("PLESOL" "Hlp_C_PLESOL.html") ("PLETAB" "Hlp_C_PLETAB.html") ("PLF2D" "Hlp_C_PLF2D.html") ("PLFAR" "Hlp_C_PLFAR.html") ("PLGEOM" "Hlp_C_PLGEOM.html") ("PLLS" "Hlp_C_PLLS.html") ("PLMAP" "Hlp_C_PLMAP.html") ("PLMC" "Hlp_C_PLMC.html") ("PLNEAR" "Hlp_C_PLNEAR.html") ("PLNSOL" "Hlp_C_PLNSOL.html") ("PLORB" "Hlp_C_PLORB.html") ("PLOT" "Hlp_C_PLOT.html") ("PLOTTING" "Hlp_C_PLOTTING.html") ("PLPAGM" "Hlp_C_PLPAGM.html") ("PLPATH" "Hlp_C_PLPATH.html") ("PLSECT" "Hlp_C_PLSECT.html") ("PLST" "Hlp_C_PLST.html") ("PLTIME" "Hlp_C_PLTIME.html") ("PLTRAC" "Hlp_C_PLTRAC.html") ("PLVAR" "Hlp_C_PLVAR.html") ("PLVECT" "Hlp_C_PLVECT.html") ("PLZZ" "Hlp_C_PLZZ.html") ("PMAP" "Hlp_C_PMAP.html") ("PMGTRAN" "Hlp_C_PMGTRAN.html") ("PMLOPT" "Hlp_C_PMLOPT.html") ("PMLSIZE" "Hlp_C_PMLSIZE.html") ("PNGR" "Hlp_C_PNGR.html") ("POINT" "Hlp_C_POINT.html") ("POLY" "Hlp_C_POLY.html") ("POPT" "Hlp_C_POPT.html") ("POWERH" "Hlp_C_POWERH.html") ("PPATH" "Hlp_C_PPATH.html") ("PPRES" "Hlp_C_PPRES.html") ("PRANGE" "Hlp_C_PRANGE.html") ("PRAS" "Hlp_C_PRAS.html") ("PRCAMP" "Hlp_C_PRCAMP.html") ("PRCINT" "Hlp_C_PRCINT.html") ("PRCPLX" "Hlp_C_PRCPLX.html") ("PRED" "Hlp_C_PRED.html") ("PRENERGY" "Hlp_C_PRENERGY.html") ("PRERR" "Hlp_C_PRERR.html") ("PRESOL" "Hlp_C_PRESOL.html") ("PRETAB" "Hlp_C_PRETAB.html") ("PRETS" "Hlp_E_CH3_2.html#allprets") ("PRETS179" "Hlp_E_PRETS179.html") ("PRFAR" "Hlp_C_PRFAR.html") ("PRI2" "Hlp_C_PRI2.html") ("PRIM" "Hlp_C_PRIM.html") ("PRINT" "Hlp_C_PRINT.html") ("PRISM" "Hlp_C_PRISM.html") ("PRITER" "Hlp_C_PRITER.html") ("PRJSOL" "Hlp_C_PRJSOL.html") ("PRNEAR" "Hlp_C_PRNEAR.html") ("PRNLD" "Hlp_C_PRNLD.html") ("PRNSOL" "Hlp_C_PRNSOL.html") ("PROD" "Hlp_C_PROD.html") ("PRORB" "Hlp_C_PRORB.html") ("PRPATH" "Hlp_C_PRPATH.html") ("PRRFOR" "Hlp_C_PRRFOR.html") ("PRRSOL" "Hlp_C_PRRSOL.html") ("PRSCONTROL" "Hlp_C_PRSCONTROL.html") ("PRSECT" "Hlp_C_PRSECT.html") ("PRTIME" "Hlp_C_PRTIME.html") ("PRVAR" "Hlp_C_PRVAR.html") ("PRVECT" "Hlp_C_PRVECT.html") ("PSCONTROL" "Hlp_C_PSCONTROL.html") ("PSCR" "Hlp_C_PSCR.html") ("PSDCOM" "Hlp_C_PSDCOM.html") ("PSDFRQ" "Hlp_C_PSDFRQ.html") ("PSDGRAPH" "Hlp_C_PSDGRAPH.html") ("PSDRES" "Hlp_C_PSDRES.html") ("PSDSPL" "Hlp_C_PSDSPL.html") ("PSDUNIT" "Hlp_C_PSDUNIT.html") ("PSDVAL" "Hlp_C_PSDVAL.html") ("PSDWAV" "Hlp_C_PSDWAV.html") ("PSEL" "Hlp_C_PSEL.html") ("PSMAT" "Hlp_C_PSMAT.html") ("PSMESH" "Hlp_C_PSMESH.html") ("PSOLVE" "Hlp_C_PSOLVE.html") ("PSPEC" "Hlp_C_PSPEC.html") ("PSPRNG" "Hlp_C_PSPRNG.html") ("PSTRES" "Hlp_C_PSTRES.html") ("PTEMP" "Hlp_C_PTEMP.html") ("PTR" "Hlp_C_PTR.html") ("PTXY" "Hlp_C_PTXY.html") ("PUNIT" "Hlp_C_PUNIT.html") ("PVECT" "Hlp_C_PVECT.html") ("QDVAL" "Hlp_C_QDVAL.html") ("QRDOPT" "Hlp_C_QRDOPT.html") ("QSOPT" "Hlp_C_QSOPT.html") ("QUAD" "Hlp_C_QUAD.html") ("QUOT" "Hlp_C_QUOT.html") ("R" "Hlp_C_R.html") ("RACE" "Hlp_C_RACE.html") ("RADOPT" "Hlp_C_RADOPT.html") ("RAPPND" "Hlp_C_RAPPND.html") ("RATE" "Hlp_C_RATE.html") ("RBE3" "Hlp_C_RBE3.html") ("RCON" "Hlp_C_RCON.html") ("RCYC" "Hlp_C_RCYC.html") ("RDEC" "Hlp_C_RDEC.html") ("RDELE" "Hlp_C_RDELE.html") ("READ" "Hlp_C_READ.html") ("REAL" "Hlp_C_REAL.html") ("REALVAR" "Hlp_C_REALVAR.html") ("RECTNG" "Hlp_C_RECTNG.html") ("REDUCE" "Hlp_C_REDUCE.html") ("REINF" "Hlp_E_CH3_2.html#allreinf") ("REINF263" "Hlp_E_REINF263.html") ("REINF264" "Hlp_E_REINF264.html") ("REINF265" "Hlp_E_REINF265.html") ("REMESH" "Hlp_C_REMESH.html") ("REORDER" "Hlp_C_REORDER.html") ("RESCOMBINE" "Hlp_C_RESCOMBINE.html") ("RESCONTROL" "Hlp_C_RESCONTROL.html") ("RESET" "Hlp_C_RESET.html") ("RESP" "Hlp_C_RESP.html") ("RESUME" "Hlp_C_RESUME.html") ("RESVEC" "Hlp_C_RESVEC.html") ("RESWRITE" "Hlp_C_RESWRITE.html") ("REXPORT" "Hlp_C_REXPORT.html") ("REZONE" "Hlp_C_REZONE.html") ("RFORCE" "Hlp_C_RFORCE.html") ("RIGID" "Hlp_C_RIGID.html") ("RIGRESP" "Hlp_C_RIGRESP.html") ("RIMPORT" "Hlp_C_RIMPORT.html") ("RLIST" "Hlp_C_RLIST.html") ("RMALIST" "Hlp_C_RMALIST.html") ("RMANL" "Hlp_C_RMANL.html") ("RMASTER" "Hlp_C_RMASTER.html") ("RMCAP" "Hlp_C_RMCAP.html") ("RMCLIST" "Hlp_C_RMCLIST.html") ("RMFLVEC" "Hlp_C_RMFLVEC.html") ("RMLVSCALE" "Hlp_C_RMLVSCALE.html") ("RMMLIST" "Hlp_C_RMMLIST.html") ("RMMRANGE" "Hlp_C_RMMRANGE.html") ("RMMSELECT" "Hlp_C_RMMSELECT.html") ("RMNDISP" "Hlp_C_RMNDISP.html") ("RMNEVEC" "Hlp_C_RMNEVEC.html") ("RMODIF" "Hlp_C_RMODIF.html") ("RMORE" "Hlp_C_RMORE.html") ("RMPORDER" "Hlp_C_RMPORDER.html") ("RMRESUME" "Hlp_C_RMRESUME.html") ("RMRGENERATE" "Hlp_C_RMRGENERATE.html") ("RMROPTIONS" "Hlp_C_RMROPTIONS.html") ("RMRPLOT" "Hlp_C_RMRPLOT.html") ("RMRSTATUS" "Hlp_C_RMRSTATUS.html") ("RMSAVE" "Hlp_C_RMSAVE.html") ("RMSMPLE" "Hlp_C_RMSMPLE.html") ("RMUSE" "Hlp_C_RMUSE.html") ("RMXPORT" "Hlp_C_RMXPORT.html") ("ROCK" "Hlp_C_ROCK.html") ("ROM" "Hlp_E_CH3_2.html#allrom") ("ROM144" "Hlp_E_ROM144.html") ("ROSE" "Hlp_C_ROSE.html") ("RPOLY" "Hlp_C_RPOLY.html") ("RPR4" "Hlp_C_RPR4.html") ("RPRISM" "Hlp_C_RPRISM.html") ("RPSD" "Hlp_C_RPSD.html") ("RSFIT" "Hlp_C_RSFIT.html") ("RSOPT" "Hlp_C_RSOPT.html") ("RSPLIT" "Hlp_C_RSPLIT.html") ("RSPLOT" "Hlp_C_RSPLOT.html") ("RSPRNT" "Hlp_C_RSPRNT.html") ("RSSIMS" "Hlp_C_RSSIMS.html") ("RSTMAC" "Hlp_C_RSTMAC.html") ("RSTOFF" "Hlp_C_RSTOFF.html") ("RSURF" "Hlp_C_RSURF.html") ("RSYMM" "Hlp_C_RSYMM.html") ("RSYS" "Hlp_C_RSYS.html") ("RTHICK" "Hlp_C_RTHICK.html") ("RUN" "Hlp_C_RUN.html") ("SABS" "Hlp_C_SABS.html") ("SADD" "Hlp_C_SADD.html") ("SALLOW" "Hlp_C_SALLOW.html") ("SAVE" "Hlp_C_SAVE.html") ("SBCLIST" "Hlp_C_SBCLIST.html") ("SBCTRAN" "Hlp_C_SBCTRAN.html") ("SDELETE" "Hlp_C_SDELETE.html") ("SE" "Hlp_C_SE.html") ("SECCONTROL" "Hlp_C_SECCONTROL.html") ("SECDATA" "Hlp_C_SECDATA.html") ("SECFUNCTION" "Hlp_C_SECFUNCTION.html") ("SECJOINT" "Hlp_C_SECJOINT.html") ("SECLOCK" "Hlp_C_SECLOCK.html") ("SECMODIF" "Hlp_C_SECMODIF.html") ("SECNUM" "Hlp_C_SECNUM.html") ("SECOFFSET" "Hlp_C_SECOFFSET.html") ("SECPLOT" "Hlp_C_SECPLOT.html") ("SECREAD" "Hlp_C_SECREAD.html") ("SECSTOP" "Hlp_C_SECSTOP.html") ("SECTYPE" "Hlp_C_SECTYPE.html") ("SECWRITE" "Hlp_C_SECWRITE.html") ("SED" "Hlp_C_SED.html") ("SEDLIST" "Hlp_C_SEDLIST.html") ("SEEXP" "Hlp_C_SEEXP.html") ("SEGEN" "Hlp_C_SEGEN.html") ("SELIST" "Hlp_C_SELIST.html") ("SELM" "Hlp_C_SELM.html") ("SELTOL" "Hlp_C_SELTOL.html") ("SENERGY" "Hlp_C_SENERGY.html") ("SEOPT" "Hlp_C_SEOPT.html") ("SESYMM" "Hlp_C_SESYMM.html") ("SET" "Hlp_C_SET.html") ("SETFGAP" "Hlp_C_SETFGAP.html") ("SETRAN" "Hlp_C_SETRAN.html") ("SEXP" "Hlp_C_SEXP.html") ("SF" "Hlp_C_SF.html") ("SFA" "Hlp_C_SFA.html") ("SFACT" "Hlp_C_SFACT.html") ("SFADELE" "Hlp_C_SFADELE.html") ("SFALIST" "Hlp_C_SFALIST.html") ("SFBEAM" "Hlp_C_SFBEAM.html") ("SFCALC" "Hlp_C_SFCALC.html") ("SFCUM" "Hlp_C_SFCUM.html") ("SFDELE" "Hlp_C_SFDELE.html") ("SFE" "Hlp_C_SFE.html") ("SFEDELE" "Hlp_C_SFEDELE.html") ("SFELIST" "Hlp_C_SFELIST.html") ("SFFUN" "Hlp_C_SFFUN.html") ("SFGRAD" "Hlp_C_SFGRAD.html") ("SFL" "Hlp_C_SFL.html") ("SFLDELE" "Hlp_C_SFLDELE.html") ("SFLEX" "Hlp_C_SFLEX.html") ("SFLIST" "Hlp_C_SFLIST.html") ("SFLLIST" "Hlp_C_SFLLIST.html") ("SFSCALE" "Hlp_C_SFSCALE.html") ("SFTRAN" "Hlp_C_SFTRAN.html") ("SHELL" "Hlp_C_SHELL.html") ("SHELL131" "Hlp_E_SHELL131.html") ("SHELL132" "Hlp_E_SHELL132.html") ("SHELL157" "Hlp_E_SHELL157.html") ("SHELL163" "Hlp_E_SHELL163.html") ("SHELL181" "Hlp_E_SHELL181.html") ("SHELL208" "Hlp_E_SHELL208.html") ("SHELL209" "Hlp_E_SHELL209.html") ("SHELL28" "Hlp_E_SHELL28.html") ("SHELL281" "Hlp_E_SHELL281.html") ("SHELL41" "Hlp_E_SHELL41.html") ("SHELL61" "Hlp_E_SHELL61.html") ("SHELL63" "Hlp_E_SHELL63.html") ("\"SHELLS\"" "Hlp_E_CH3_2.html#allshells") ("SHPP" "Hlp_C_SHPP.html") ("SLIST" "Hlp_C_SLIST.html") ("SLOAD" "Hlp_C_SLOAD.html") ("SMALL" "Hlp_C_SMALL.html") ("SMAX" "Hlp_C_SMAX.html") ("SMBODY" "Hlp_C_SMBODY.html") ("SMCONS" "Hlp_C_SMCONS.html") ("SMFOR" "Hlp_C_SMFOR.html") ("SMIN" "Hlp_C_SMIN.html") ("SMOOTH" "Hlp_C_SMOOTH.html") ("SMRTSIZE" "Hlp_C_SMRTSIZE.html") ("SMSURF" "Hlp_C_SMSURF.html") ("SMULT" "Hlp_C_SMULT.html") ("SNOPTION" "Hlp_C_SNOPTION.html") ("\"SOLIDS\"" "Hlp_E_CH3_2.html#allsolids") ("SOLID122" "Hlp_E_SOLID122.html") ("SOLID123" "Hlp_E_SOLID123.html") ("SOLID164" "Hlp_E_SOLID164.html") ("SOLID168" "Hlp_E_SOLID168.html") ("SOLID185" "Hlp_E_SOLID185.html") ("SOLID186" "Hlp_E_SOLID186.html") ("SOLID187" "Hlp_E_SOLID187.html") ("SOLID226" "Hlp_E_SOLID226.html") ("SOLID227" "Hlp_E_SOLID227.html") ("SOLID231" "Hlp_E_SOLID231.html") ("SOLID232" "Hlp_E_SOLID232.html") ("SOLID236" "Hlp_E_SOLID236.html") ("SOLID237" "Hlp_E_SOLID237.html") ("SOLID239" "Hlp_E_SOLID239.html") ("SOLID240" "Hlp_E_SOLID240.html") ("SOLID272" "Hlp_E_SOLID272.html") ("SOLID273" "Hlp_E_SOLID273.html") ("SOLID278" "Hlp_E_SOLID278.html") ("SOLID279" "Hlp_E_SOLID279.html") ("SOLID285" "Hlp_E_SOLID285.html") ("SOLID45" "Hlp_E_SOLID45.html") ("SOLID5" "Hlp_E_SOLID5.html") ("SOLID65" "Hlp_E_SOLID65.html") ("SOLID70" "Hlp_E_SOLID70.html") ("SOLID87" "Hlp_E_SOLID87.html") ("SOLID90" "Hlp_E_SOLID90.html") ("SOLID92" "Hlp_E_SOLID92.html") ("SOLID95" "Hlp_E_SOLID95.html") ("SOLID96" "Hlp_E_SOLID96.html") ("SOLID97" "Hlp_E_SOLID97.html") ("SOLID98" "Hlp_E_SOLID98.html") ("SOLSH" "Hlp_E_CH3_2.html#allsolsh") ("SOLSH190" "Hlp_E_SOLSH190.html") ("SOLU" "Hlp_C_SOLU.html") ("SOLUOPT" "Hlp_C_SOLUOPT.html") ("SOLVE" "Hlp_C_SOLVE.html") ("SORT" "Hlp_C_SORT.html") ("SOURC" "Hlp_E_CH3_2.html#allsourc") ("SOURC36" "Hlp_E_SOURC36.html") ("SOURCE" "Hlp_C_SOURCE.html") ("SPACE" "Hlp_C_SPACE.html") ("SPCNOD" "Hlp_C_SPCNOD.html") ("SPCTEMP" "Hlp_C_SPCTEMP.html") ("SPDAMP" "Hlp_C_SPDAMP.html") ("SPEC" "Hlp_C_SPEC.html") ("SPFREQ" "Hlp_C_SPFREQ.html") ("SPGRAPH" "Hlp_C_SPGRAPH.html") ("SPH4" "Hlp_C_SPH4.html") ("SPH5" "Hlp_C_SPH5.html") ("SPHERE" "Hlp_C_SPHERE.html") ("SPLINE" "Hlp_C_SPLINE.html") ("SPLOT" "Hlp_C_SPLOT.html") ("SPMWRITE" "Hlp_C_SPMWRITE.html") ("SPOINT" "Hlp_C_SPOINT.html") ("SPOPT" "Hlp_C_SPOPT.html") ("SPOWER" "Hlp_C_SPOWER.html") ("SPREAD" "Hlp_C_SPREAD.html") ("SPTOPT" "Hlp_C_SPTOPT.html") ("SPUNIT" "Hlp_C_SPUNIT.html") ("SPVAL" "Hlp_C_SPVAL.html") ("SQRT" "Hlp_C_SQRT.html") ("SRSS" "Hlp_C_SRSS.html") ("SSBT" "Hlp_C_SSBT.html") ("SSLN" "Hlp_C_SSLN.html") ("SSMT" "Hlp_C_SSMT.html") ("SSPA" "Hlp_C_SSPA.html") ("SSPB" "Hlp_C_SSPB.html") ("SSPD" "Hlp_C_SSPD.html") ("SSPE" "Hlp_C_SSPE.html") ("SSPM" "Hlp_C_SSPM.html") ("SSTATE" "Hlp_C_SSTATE.html") ("SSTIF" "Hlp_C_SSTIF.html") ("SSUM" "Hlp_C_SSUM.html") ("STABILIZE" "Hlp_C_STABILIZE.html") ("STAOPT" "Hlp_C_STAOPT.html") ("STAT" "Hlp_C_STAT.html") ("STEF" "Hlp_C_STEF.html") ("STORE" "Hlp_C_STORE.html") ("SUBOPT" "Hlp_C_SUBOPT.html") ("SUBSET" "Hlp_C_SUBSET.html") ("SUCALC" "Hlp_C_SUCALC.html") ("SUCR" "Hlp_C_SUCR.html") ("SUDEL" "Hlp_C_SUDEL.html") ("SUEVAL" "Hlp_C_SUEVAL.html") ("SUGET" "Hlp_C_SUGET.html") ("SUMAP" "Hlp_C_SUMAP.html") ("SUMTYPE" "Hlp_C_SUMTYPE.html") ("SUPL" "Hlp_C_SUPL.html") ("SUPR" "Hlp_C_SUPR.html") ("SURESU" "Hlp_C_SURESU.html") ("SURF" "Hlp_E_CH3_2.html#allsurf") ("SURF151" "Hlp_E_SURF151.html") ("SURF152" "Hlp_E_SURF152.html") ("SURF153" "Hlp_E_SURF153.html") ("SURF154" "Hlp_E_SURF154.html") ("SURF156" "Hlp_E_SURF156.html") ("SURF159" "Hlp_E_SURF159.html") ("SURF251" "Hlp_E_SURF251.html") ("SURF252" "Hlp_E_SURF252.html") ("SUSAVE" "Hlp_C_SUSAVE.html") ("SUSEL" "Hlp_C_SUSEL.html") ("SUVECT" "Hlp_C_SUVECT.html") ("SV" "Hlp_C_SV.html") ("SVPLOT" "Hlp_C_SVPLOT.html") ("SVTYP" "Hlp_C_SVTYP.html") ("SWADD" "Hlp_C_SWADD.html") ("SWDEL" "Hlp_C_SWDEL.html") ("SWGEN" "Hlp_C_SWGEN.html") ("SWLIST" "Hlp_C_SWLIST.html") ("SYNCHRO" "Hlp_C_SYNCHRO.html") ("TALLOW" "Hlp_C_TALLOW.html") ("TARGE" "Hlp_E_CH3_2.html#alltarge") ("TARGE169" "Hlp_E_TARGE169.html") ("TARGE170" "Hlp_E_TARGE170.html") ("TARGET" "Hlp_C_TARGET.html") ("TB" "Hlp_C_TB.html") ("TBCOPY" "Hlp_C_TBCOPY.html") ("TBDATA" "Hlp_C_TBDATA.html") ("TBDELE" "Hlp_C_TBDELE.html") ("TBEO" "Hlp_C_TBEO.html") ("TBFIELD" "Hlp_C_TBFIELD.html") ("TBFT" "Hlp_C_TBFT.html") ("TBIN" "Hlp_C_TBIN.html") ("TBLE" "Hlp_C_TBLE.html") ("TBLIST" "Hlp_C_TBLIST.html") ("TBMODIF" "Hlp_C_TBMODIF.html") ("TBPLOT" "Hlp_C_TBPLOT.html") ("TBPT" "Hlp_C_TBPT.html") ("TBTEMP" "Hlp_C_TBTEMP.html") ("TCHG" "Hlp_C_TCHG.html") ("TEE" "Hlp_C_TEE.html") ("TERM" "Hlp_C_TERM.html") ("THEXPAND" "Hlp_C_THEXPAND.html") ("THOPT" "Hlp_C_THOPT.html") ("TIFF" "Hlp_C_TIFF.html") ("TIME" "Hlp_C_TIME.html") ("TIMERANGE" "Hlp_C_TIMERANGE.html") ("TIMINT" "Hlp_C_TIMINT.html") ("TIMP" "Hlp_C_TIMP.html") ("TINTP" "Hlp_C_TINTP.html") ("TOFFST" "Hlp_C_TOFFST.html") ("TORQ2D" "Hlp_C_TORQ2D.html") ("TORQC2D" "Hlp_C_TORQC2D.html") ("TORQSUM" "Hlp_C_TORQSUM.html") ("TORUS" "Hlp_C_TORUS.html") ("TRANS" "Hlp_C_TRANS.html") ("TRANS126" "Hlp_E_TRANS126.html") ("TRANSFER" "Hlp_C_TRANSFER.html") ("TREF" "Hlp_C_TREF.html") ("TRNOPT" "Hlp_C_TRNOPT.html") ("TRPDEL" "Hlp_C_TRPDEL.html") ("TRPLIS" "Hlp_C_TRPLIS.html") ("TRPOIN" "Hlp_C_TRPOIN.html") ("TRTIME" "Hlp_C_TRTIME.html") ("TSHAP" "Hlp_C_TSHAP.html") ("TSRES" "Hlp_C_TSRES.html") ("TUNIF" "Hlp_C_TUNIF.html") ("TVAR" "Hlp_C_TVAR.html") ("TYPE" "Hlp_C_TYPE.html") ("UIMP" "Hlp_C_UIMP.html") ("UNDELETE" "Hlp_C_UNDELETE.html") ("UNDO" "Hlp_C_UNDO.html") ("UNPAUSE" "Hlp_C_UNPAUSE.html") ("UPCOORD" "Hlp_C_UPCOORD.html") ("UPGEOM" "Hlp_C_UPGEOM.html") ("USER300" "Hlp_E_USER300.html") ("USRCAL" "Hlp_C_USRCAL.html") ("USRDOF" "Hlp_C_USRDOF.html") ("USRELEM" "Hlp_C_USRELEM.html") ("V" "Hlp_C_V.html") ("V2DOPT" "Hlp_C_V2DOPT.html") ("VA" "Hlp_C_VA.html") ("VADD" "Hlp_C_VADD.html") ("VALVE" "Hlp_C_VALVE.html") ("VARDEL" "Hlp_C_VARDEL.html") ("VARNAM" "Hlp_C_VARNAM.html") ("VATT" "Hlp_C_VATT.html") ("VCLEAR" "Hlp_C_VCLEAR.html") ("VCROSS" "Hlp_C_VCROSS.html") ("VDDAM" "Hlp_C_VDDAM.html") ("VDELE" "Hlp_C_VDELE.html") ("VDGL" "Hlp_C_VDGL.html") ("VDOT" "Hlp_C_VDOT.html") ("VDRAG" "Hlp_C_VDRAG.html") ("VEORIENT" "Hlp_C_VEORIENT.html") ("VEXT" "Hlp_C_VEXT.html") ("VFOPT" "Hlp_C_VFOPT.html") ("VFQUERY" "Hlp_C_VFQUERY.html") ("VFSM" "Hlp_C_VFSM.html") ("VGEN" "Hlp_C_VGEN.html") ("VGET" "Hlp_C_VGET.html") ("VGLUE" "Hlp_C_VGLUE.html") ("VIMP" "Hlp_C_VIMP.html") ("VINP" "Hlp_C_VINP.html") ("VINV" "Hlp_C_VINV.html") ("VLIST" "Hlp_C_VLIST.html") ("VLSCALE" "Hlp_C_VLSCALE.html") ("VMESH" "Hlp_C_VMESH.html") ("VOFFST" "Hlp_C_VOFFST.html") ("VOLUMES" "Hlp_C_VOLUMES.html") ("VOVLAP" "Hlp_C_VOVLAP.html") ("VPLOT" "Hlp_C_VPLOT.html") ("VPTN" "Hlp_C_VPTN.html") ("VPUT" "Hlp_C_VPUT.html") ("VROTAT" "Hlp_C_VROTAT.html") ("VSBA" "Hlp_C_VSBA.html") ("VSBV" "Hlp_C_VSBV.html") ("VSBW" "Hlp_C_VSBW.html") ("VSEL" "Hlp_C_VSEL.html") ("VSLA" "Hlp_C_VSLA.html") ("VSUM" "Hlp_C_VSUM.html") ("VSWEEP" "Hlp_C_VSWEEP.html") ("VSYMM" "Hlp_C_VSYMM.html") ("VTRAN" "Hlp_C_VTRAN.html") ("VTYPE" "Hlp_C_VTYPE.html") ("WAVES" "Hlp_C_WAVES.html") ("WERASE" "Hlp_C_WERASE.html") ("WFRONT" "Hlp_C_WFRONT.html") ("WMID" "Hlp_C_WMID.html") ("WMORE" "Hlp_C_WMORE.html") ("WPAVE" "Hlp_C_WPAVE.html") ("WPCSYS" "Hlp_C_WPCSYS.html") ("WPLANE" "Hlp_C_WPLANE.html") ("WPOFFS" "Hlp_C_WPOFFS.html") ("WPROTA" "Hlp_C_WPROTA.html") ("WPSTYL" "Hlp_C_WPSTYL.html") ("WRFULL" "Hlp_C_WRFULL.html") ("WRITE" "Hlp_C_WRITE.html") ("WRITEMAP" "Hlp_C_WRITEMAP.html") ("WSORT" "Hlp_C_WSORT.html") ("WSPRINGS" "Hlp_C_WSPRINGS.html") ("WSTART" "Hlp_C_WSTART.html") ("WTBCREATE" "WTBCREATE.html") ("XFDATA" "Hlp_C_XFDATA.html") ("XFENRICH" "Hlp_C_XFENRICH.html") ("XFLIST" "Hlp_C_XFLIST.html") ("XVAR" "Hlp_C_XVAR.html") ("notfound" "notfound.html") ("p26calc" "Hlp_G_BASP26calc.html") ("p26export" "Hlp_G_BASP26export.html") ("p26import" "Hlp_G_BASP26import.html") ("p26list" "Hlp_G_BASP26graph.html#p26list") ("p26plot" "Hlp_G_BASP26graph.html#p26plot") ("p26vardefine" "Hlp_G_BASP26define.html") ("p26viewer" "Hlp_G_BASP26viewer.html") ("pdsser" "Hlp_G_ADVPDS3.html#pdsser") ("pdssra" "Hlp_G_ADVPDS5.html#pdsrsa") ("pdsstatpp" "Hlp_G_ADVPDS7.html#pdsstatpp") ("s-AUX3" "Hlp_C_AUX3_sl.html") ("ug_ins_pt_node" "ugdt_wf_nodes.html#ug_ins_pt_node") ("wb_rsm_slvmgrp" "wb_rsm_slvmgrp.html") ("~CAT5" "Hlp_C_CAT5IN.html") ("~CAT5IN" "Hlp_C_CAT5IN.html") ("~CATIAIN" "Hlp_C_CATIAIN.html") ("~PARAIN" "Hlp_C_PARAIN.html") ("~PROEIN" "Hlp_C_PROEIN.html") ("~SATIN" "Hlp_C_SATIN.html") ("~UGIN" "Hlp_C_UGIN.html") ("\"RELEASE NOTES\"" "ansysincreleasenotes.html") ("\"CONTACT TECHNOLOGY GUIDE\"" "ctectoc.html") ("\"PARAMETRIC DESIGN LANGUAGE GUIDE\"" "Hlp_P_APDLTOC.html") ("\"STRUCTURAL ANALYSIS GUIDE\"" "Hlp_G_StrTOC.html") ("\"ADVANCED ANALYSIS TECHNIQUES GUIDE\"" "Hlp_G_AdvTOC.html") ("\"MATERIAL MODELS\"" "ans_mat.html") ("\"THEORY REFERENCE\"" "ansys.theory.html"))
"ANSYS help index alist.")

(defconst ansys-completions
'("SOLID5" "INFIN9" "LINK11" "PLANE13" "COMBIN14" "MASS21" "PLANE25" "MATRIX27" "SHELL28" "FLUID29" "FLUID30" "LINK31" "LINK33" "LINK34" "PLANE35" "SOURC36" "COMBIN37" "FLUID38" "COMBIN39" "COMBIN40" "SHELL41" "INFIN47" "MATRIX50" "PLANE53" "PLANE55" "SHELL61" "SOLID65" "LINK68" "SOLID70" "MASS71" "PLANE75" "PLANE77" "PLANE78" "PLANE83" "SOLID87" "SOLID90" "CIRCU94" "SOLID96" "SOLID97" "SOLID98" "INFIN110" "INFIN111" "FLUID116" "PLANE121" "SOLID122" "SOLID123" "CIRCU124" "CIRCU125" "TRANS126" "FLUID129" "FLUID130" "SHELL131" "SHELL132" "FLUID136" "FLUID138" "FLUID139" "ROM144" "SURF151" "SURF152" "SURF153" "SURF154" "SURF156" "SHELL157" "SURF159" "LINK160" "BEAM161" "PLANE162" "SHELL163" "SOLID164" "COMBI165" "MASS166" "LINK167" "SOLID168" "TARGE169" "TARGE170" "CONTA171" "CONTA172" "CONTA173" "CONTA174" "CONTA175" "CONTA176" "CONTA177" "CONTA178" "PRETS179" "LINK180" "SHELL181" "PLANE182" "PLANE183" "MPC184" "SOLID185" "SOLID186" "SOLID187" "BEAM188" "BEAM189" "SOLSH190" "INTER192" "INTER193" "INTER194" "INTER195" "MESH200" "FOLLW201" "INTER202" "INTER203" "INTER204" "INTER205" "SHELL208" "SHELL209" "CPT212" "CPT213" "COMBI214" "CPT215" "CPT216" "CPT217" "FLUID220" "FLUID221" "PLANE223" "SOLID226" "SOLID227" "PLANE230" "SOLID231" "SOLID232" "PLANE233" "SOLID236" "SOLID237" "PLANE238" "SOLID239" "SOLID240" "HSFLD241" "HSFLD242" "SURF251" "SURF252" "REINF263" "REINF264" "REINF265" "SOLID272" "SOLID273" "SOLID278" "SOLID279" "SHELL281" "SOLID285" "PIPE288" "PIPE289" "ELBOW290" "USER300" "BEAM3" "BEAM4" "BEAM23" "BEAM24" "BEAM44" "BEAM54" "CONTAC12" "CONTAC52" "COMBIN7" "FLUID79" "FLUID80" "FLUID81" "FLUID141" "FLUID142" "INFIN9" "INFIN47" "PIPE16" "PIPE18" "PLANE13" "PLANE25" "PLANE42" "PLANE53" "PLANE67" "PLANE82" "PLANE83" "PLANE145" "PLANE146" "CONTAC12" "CONTAC52" "LINK1" "LINK8" "LINK10" "LINK32" "PIPE16" "PIPE17" "PIPE18" "PIPE20" "PIPE59" "PIPE60" "SHELL41" "SHELL43" "SHELL57" "SHELL63" "SHELL91" "SHELL93" "SHELL99" "SHELL150" "SOLID5" "SOLID45" "SOLID46" "SOLID65" "SOLID69" "SOLID92" "SOLID95" "SOLID117" "SOLID127" "SOLID128" "SOLID147" "SOLID148" "SOLID191" "VISCO88" "VISCO89" "VISCO106" "VISCO107" "VISCO108" "TRANS109" "NSEL()" "ESEL()" "KSEL()" "LSEL()" "ASEL()" "VSEL()" "NDNEXT()" "ELNEXT()" "KPNEXT()" "LSNEXT()" "ARNEXT()" "VLNEXT()" "CENTRX()" "CENTRY()" "CENTRZ()" "NX()" "NY()" "NZ()" "KX()" "KY()" "KZ()" "LX()" "LY()" "LZ()" "LSX()" "LSY()" "LSZ()" "NODE()" "KP()" "DISTND()" "DISTKP()" "DISTEN()" "ANGLEN()" "ANGLEK()" "NNEAR()" "KNEAR()" "ENEARN()" "AREAND()" "AREAKP()" "ARNODE()" "NORMNX()" "NORMNY()" "NORMNZ()" "NORMKX()" "NORMKY()" "NORMKZ()" "ENEXTN()" "NELEM()" "NODEDOF()" "ELADJ()" "NDFACE()" "NMFACE()" "ARFACE()" "UX()" "UY()" "UZ()" "ROTX()" "ROTY()" "ROTZ()" "TEMP()" "PRES()" "VX()" "VY()" "VZ()" "ENKE()" "ENDS()" "VOLT()" "MAG()" "AX()" "AY()" "AZ()" "VIRTINQR()" "KWGET()" "VALCHR()" "VALHEX()" "CHRHEX()" "STRFILL()" "STRCOMP()" "STRPOS()" "STRLENG()" "UPCASE()" "LWCASE()" "JOIN()" "SPLIT()" "ABS()" "SIGN()" "CXABS()" "EXP()" "LOG()" "LOG10()" "SQRT()" "NINT()" "MOD()" "RAND()" "GDIS()" "SIN()" "COS()" "TAN()" "SINH()" "COSH()" "TANH()" "ASIN()" "ACOS()" "ATAN()" "ATAN2()" "~CAT5IN" "~CATIAIN" "~PARAIN" "~PROEIN" "~SATIN" "~UGIN" "A" "AADD" "AATT" "ABEXTRACT" "*ABBR" "ABBRES" "ABBSAV" "ABS" "ACCAT" "ACCOPTION" "ACEL" "ACLEAR" "ADAMS" "ADAPT" "ADD" "ADDAM" "ADELE" "ADGL" "ADRAG" "AESIZE" "AFILLT" "AFLIST" "AFSURF" "*AFUN" "AGEN" "AGLUE" "AINA" "AINP" "AINV" "AL" "ALIST" "ALLSEL" "ALPHAD" "AMAP" "AMESH" "/AN3D" "ANCNTR" "ANCUT" "ANCYC" "ANDATA" "ANDSCL" "ANDYNA" "/ANFILE" "ANFLOW" "/ANGLE" "ANHARM" "ANIM" "ANISOS" "ANMODE" "ANMRES" "/ANNOT" "ANORM" "ANPRES" "ANSOL" "ANSTOAQWA" "ANSTOASAS" "ANTIME" "ANTYPE" "/ANUM" "AOFFST" "AOVLAP" "APLOT" "APPEND" "APTN" "ARCLEN" "ARCTRM" "AREAS" "AREFINE" "AREMESH" "AREVERSE" "AROTAT" "ARSCALE" "ARSYM" "ASBA" "ASBL" "ASBV" "ASBW" "ASCRES" "ASEL" "ASIFILE" "*ASK" "ASKIN" "ASLL" "ASLV" "ASOL" "/ASSIGN" "ASUB" "ASUM" "ATAN" "ATRAN" "ATYPE" "/AUTO" "AUTOTS" "/AUX2" "/AUX3" "/AUX12" "/AUX15" "AVPRIN" "AVRES" "AWAVE" "/AXLAB" "*AXPY" "/BATCH" "BCSOPTION" "BETAD" "BF" "BFA" "BFADELE" "BFALIST" "BFCUM" "BFDELE" "BFE" "BFECUM" "BFEDELE" "BFELIST" "BFESCAL" "BFINT" "BFK" "BFKDELE" "BFKLIST" "BFL" "BFLDELE" "BFLIST" "BFLLIST" "BFSCALE" "BFTRAN" "BFUNIF" "BFV" "BFVDELE" "BFVLIST" "BIOOPT" "BIOT" "BLC4" "BLC5" "BLOCK" "BOOL" "BOPTN" "BSAX" "BSMD" "BSM1" "BSM2" "BSPLIN" "BSS1" "BSS2" "BSTE" "BSTQ" "BTOL" "BUCOPT" "C" "CALC" "CAMPBELL" "CBDOF" "CBMD" "CBMX" "CBTE" "CBTMP" "CDOPT" "CDREAD" "CDWRITE" "CE" "CECHECK" "CECMOD" "CECYC" "CEDELE" "CEINTF" "CELIST" "CENTER" "CEQN" "CERIG" "CESGEN" "CFACT" "*CFCLOS" "*CFOPEN" "*CFWRITE" "/CFORMAT" "CGLOC" "CGOMGA" "CGROW" "CHECK" "CHKMSH" "CINT" "CIRCLE" "CISOL" "/CLABEL" "/CLEAR" "CLOCAL" "CLOG" "/CLOG" "CLRMSHLN" "CM" "CMACEL" "/CMAP" "CMATRIX" "CMDELE" "CMDOMEGA" "CMEDIT" "CMGRP" "CMLIST" "CMMOD" "CMOMEGA" "CMPLOT" "CMROTATE" "CMSEL" "CMSFILE" "CMSOPT" "CMWRITE" "CNCHECK" "CNKMOD" "CNTR" "CNVTOL" "/COLOR" "/COM" "*COMP" "COMBINE" "COMPRESS" "CON4" "CONE" "/CONFIG" "CONJUG" "/CONTOUR" "/COPY" "CORIOLIS" "COUPLE" "COVAL" "CP" "CPCYC" "CPDELE" "CPINTF" "/CPLANE" "CPLGEN" "CPLIST" "CPMERGE" "CPNGEN" "CPSGEN" "CQC" "*CREATE" "CRPLIM" "CS" "CSCIR" "CSDELE" "CSKP" "CSLIST" "CSWPLA" "CSYS" "/CTYPE" "CURR2D" "CUTCONTROL" "/CVAL" "CVAR" "/CWD" "CYCCALC" "/CYCEXPAND" "CYCFILES" "CYCFREQ" "*CYCLE" "CYCLIC" "CYCOPT" "CYCPHASE" "CYCSPEC" "CYL4" "CYL5" "CYLIND" "CZDEL" "CZMESH" "D" "DA" "DADELE" "DALIST" "DAMORPH" "DATA" "DATADEF" "DCGOMG" "DCUM" "DCVSWP" "DDASPEC" "DDELE" "DDOPTION" "DEACT" "DEFINE" "*DEL" "DELETE" "/DELETE" "DELTIM" "DEMORPH" "DERIV" "DESIZE" "DESOL" "DETAB" "/DEVDISP" "/DEVICE" "/DFLAB" "DFLX" "DFSWAVE" "DIG" "DIGIT" "*DIM" "/DIRECTORY" "DISPLAY" "/DIST" "DJ" "DJDELE" "DJLIST" "DK" "DKDELE" "DKLIST" "DL" "DLDELE" "DLIST" "DLLIST" "*DMAT" "DMOVE" "DMPEXT" "DMPOPTION" "DMPRAT" "DMPSTR" "DNSOL" "*DO" "DOF" "DOFSEL" "DOMEGA" "*DOT" "*DOWHILE" "DSCALE" "/DSCALE" "DSET" "DSPOPTION" "DSUM" "DSURF" "DSYM" "DSYS" "DTRAN" "DUMP" "/DV3D" "DVAL" "DVMORPH" "DYNOPT" "E" "EALIVE" "EDADAPT" "EDALE" "EDASMP" "EDBOUND" "EDBX" "EDBVIS" "EDCADAPT" "EDCGEN" "EDCLIST" "EDCMORE" "EDCNSTR" "EDCONTACT" "EDCPU" "EDCRB" "EDCSC" "EDCTS" "EDCURVE" "EDDAMP" "EDDBL" "EDDC" "EDDRELAX" "EDDUMP" "EDELE" "EDENERGY" "EDFPLOT" "EDGCALE" "/EDGE" "EDHGLS" "EDHIST" "EDHTIME" "EDINT" "EDIPART" "EDIS" "EDLCS" "EDLOAD" "EDMP" "EDNB" "EDNDTSD" "EDNROT" "EDOPT" "EDOUT" "EDPART" "EDPC" "EDPL" "EDPVEL" "EDRC" "EDRD" "EDREAD" "EDRI" "EDRST" "EDRUN" "EDSHELL" "EDSOLV" "EDSP" "EDSTART" "EDTERM" "EDTP" "EDVEL" "EDWELD" "EDWRITE" "EEXTRUDE" "/EFACET" "EGEN" "*EIGEN" "EINFIN" "EINTF" "EKILL" "ELBOW" "ELEM" "ELIST" "*ELSE" "*ELSEIF" "EMAGERR" "EMATWRITE" "EMF" "EMFT" "EMID" "EMIS" "EMODIF" "EMORE" "EMSYM" "EMTGEN" "EMUNIT" "EN" "*END" "*ENDDO" "*ENDIF" "ENDRELEASE" "ENERSOL" "ENGEN" "ENORM" "ENSYM" "/EOF" "EORIENT" "EPLOT" "EQSLV" "ERASE" "/ERASE" "EREAD" "EREFINE" "EREINF" "ERESX" "ERNORM" "ERRANG" "ESCHECK" "ESEL" "/ESHAPE" "ESIZE" "ESLA" "ESLL" "ESLN" "ESLV" "ESOL" "ESORT" "ESSOLV" "ESTIF" "ESURF" "ESYM" "ESYS" "ET" "ETABLE" "ETCHG" "ETCONTROL" "ETDELE" "ETLIST" "ETYPE" "EUSORT" "EWRITE" "*EXIT" "/EXIT" "EXP" "EXPAND" "/EXPAND" "EXPASS" "*EXPORT" "EXPROFILE" "EXPSOL" "EXTOPT" "EXTREM" "EXUNIT" "F" "/FACET" "FATIGUE" "FC" "FCCHECK" "FCDELE" "FCLIST" "FCUM" "FCTYP" "FDELE" "/FDELE" "FE" "FEBODY" "FECONS" "FEFOR" "FELIST" "FESURF" "*FFT" "FILE" "FILEAUX2" "FILEAUX3" "FILEDISP" "FILL" "FILLDATA" "/FILNAME" "FINISH" "FITEM" "FJ" "FJDELE" "FJLIST" "FK" "FKDELE" "FKLIST" "FL" "FLIST" "FLLIST" "FLST" "FLUXV" "FLUREAD" "FMAGBC" "FMAGSUM" "/FOCUS" "FOR2D" "FORCE" "FORM" "/FORMAT" "FP" "FPLIST" "*FREE" "FREQ" "FRQSCL" "FS" "FSCALE" "FSDELE" "FSLIST" "FSNODE" "FSPLOT" "FSSECT" "FSSPARM" "FSUM" "FTCALC" "FTRAN" "FTSIZE" "FTWRITE" "FTYPE" "FVMESH" "GAP" "GAPF" "GAUGE" "GCDEF" "GCGEN" "/GCMD" "/GCOLUMN" "GENOPT" "GEOM" "GEOMETRY" "*GET" "/GFILE" "/GFORMAT" "/GLINE" "/GMARKER" "GMATRIX" "GMFACE" "*GO" "/GO" "/GOLIST" "/GOPR" "GP" "GPDELE" "GPLIST" "GPLOT" "/GRAPHICS" "/GRESUME" "/GRID" "/GROPT" "GRP" "/GRTYP" "/GSAVE" "GSBDATA" "GSGDATA" "GSLIST" "GSSOL" "/GST" "GSUM" "/GTHK" "/GTYPE" "HARFRQ" "/HBC" "HBMAT" "/HEADER" "HELP" "HELPDISP" "HEMIOPT" "HFANG" "HFSYM" "HMAGSOLV" "HPGL" "HPTCREATE" "HPTDELETE" "HRCPLX" "HREXP" "HROPT" "HROCEAN" "HROUT" "IC" "ICDELE" "ICLIST" "/ICLWID" "/ICSCALE" "*IF" "IGESIN" "IGESOUT" "/IMAGE" "IMAGIN" "IMESH" "IMMED" "IMPD" "INISTATE" "*INIT" "/INPUT" "/INQUIRE" "INRES" "INRTIA" "INT1" "INTSRF" "IOPTN" "IRLF" "IRLIST" "*ITENGINE" "JPEG" "JSOL" "K" "KATT" "KBC" "KBETW" "KCALC" "KCENTER" "KCLEAR" "KDELE" "KDIST" "KEEP" "KESIZE" "KEYOPT" "KEYPTS" "KEYW" "KFILL" "KGEN" "KL" "KLIST" "KMESH" "KMODIF" "KMOVE" "KNODE" "KPLOT" "KPSCALE" "KREFINE" "KSCALE" "KSCON" "KSEL" "KSLL" "KSLN" "KSUM" "KSYMM" "KTRAN" "KUSE" "KWPAVE" "KWPLAN" "L" "L2ANG" "L2TAN" "LANG" "LARC" "/LARC" "LAREA" "LARGE" "LATT" "LAYER" "LAYERP26" "LAYLIST" "LAYPLOT" "LCABS" "LCASE" "LCCALC" "LCCAT" "LCDEF" "LCFACT" "LCFILE" "LCLEAR" "LCOMB" "LCOPER" "LCSEL" "LCSL" "LCSUM" "LCWRITE" "LCZERO" "LDELE" "LDIV" "LDRAG" "LDREAD" "LESIZE" "LEXTND" "LFILLT" "LFSURF" "LGEN" "LGLUE" "LGWRITE" "/LIGHT" "LINA" "LINE" "/LINE" "LINES" "LINL" "LINP" "LINV" "LIST" "*LIST" "LLIST" "LMATRIX" "LMESH" "LNSRCH" "LOCAL" "LOVLAP" "LPLOT" "LPTN" "LREFINE" "LREVERSE" "LROTAT" "LSBA" "*LSBAC" "LSBL" "LSBV" "LSBW" "LSCLEAR" "LSDELE" "*LSDUMP" "LSEL" "*LSENGINE" "*LSFACTOR" "LSLA" "LSLK" "LSOPER" "/LSPEC" "LSREAD" "*LSRESTORE" "LSSCALE" "LSSOLVE" "LSTR" "LSUM" "LSWRITE" "/LSYMBOL" "LSYMM" "LTAN" "LTRAN" "LUMPM" "LVSCALE" "LWPLAN" "M" "MADAPT" "MAGOPT" "MAGSOLV" "/MAIL" "MAP" "/MAP" "MAP2DTO3D" "MAPSOLVE" "MAPVAR" "MASTER" "MAT" "MATER" "MCHECK" "MDAMP" "MDELE" "MDPLOT" "MEMM" "/MENU" "MESHING" "MFANALYSIS" "MFBUCKET" "MFCALC" "MFCI" "MFCLEAR" "MFCMMAND" "MFCONV" "MFDTIME" "MFELEM" "MFEM" "MFEXTER" "MFFNAME" "MFFR" "MFIMPORT" "MFINTER" "MFITER" "MFLCOMM" "MFLIST" "MFMAP" "MFORDER" "MFOUTPUT" "*MFOURI" "MFPSIMUL" "MFRC" "MFRELAX" "MFRSTART" "MFSORDER" "MFSURFACE" "MFTIME" "MFTOL" "*MFUN" "MFVOLUME" "MFWRITE" "MGEN" "MIDTOL" "/MKDIR" "MLIST" "MMASS" "MMF" "MODCONT" "MODE" "MODIFY" "MODMSH" "MODSELOPTION" "MODOPT" "MONITOR" "*MOPER" "MOPT" "MORPH" "MOVE" "MP" "MPAMOD" "MPCHG" "MPCOPY" "MPDATA" "MPDELE" "MPDRES" "/MPLIB" "MPLIST" "MPPLOT" "MPREAD" "MPRINT" "MPTEMP" "MPTGEN" "MPTRES" "MPWRITE" "/MREP" "MSAVE" "*MSG" "MSHAPE" "MSHCOPY" "MSHKEY" "MSHMID" "MSHPATTERN" "MSOLVE" "/MSTART" "MSTOLE" "*MULT" "*MWRITE" "MXPAND" "N" "NANG" "NAXIS" "NCNV" "NDELE" "NDIST" "NDSURF" "NEQIT" "/NERR" "NFORCE" "NGEN" "NKPT" "NLADAPTIVE" "NLDIAG" "NLDPOST" "NLGEOM" "NLHIST" "NLIST" "NLMESH" "NLOG" "NLOPT" "NMODIF" "NOCOLOR" "NODES" "/NOERASE" "/NOLIST" "NOOFFSET" "NOORDER" "/NOPR" "NORA" "NORL" "/NORMAL" "NPLOT" "NPRINT" "NREAD" "NREFINE" "NRLSUM" "*NRM" "NROPT" "NROTAT" "NRRANG" "NSCALE" "NSEL" "NSLA" "NSLE" "NSLK" "NSLL" "NSLV" "NSMOOTH" "NSOL" "NSORT" "NSTORE" "NSUBST" "NSVR" "NSYM" "/NUMBER" "NUMCMP" "NUMEXP" "NUMMRG" "NUMOFF" "NUMSTR" "NUMVAR" "NUSORT" "NWPAVE" "NWPLAN" "NWRITE" "OCDATA" "OCDELETE" "OCLIST" "OCREAD" "OCTABLE" "OCTYPE" "OCZONE" "OMEGA" "OPERATE" "OPNCONTROL" "OUTAERO" "OUTOPT" "OUTPR" "/OUTPUT" "OUTRES" "OVCHECK" "PADELE" "/PAGE" "PAGET" "PAPUT" "PARESU" "PARTSEL" "PARRES" "PARSAV" "PASAVE" "PATH" "PAUSE" "/PBC" "/PBF" "PCALC" "PCGOPT" "PCIRC" "/PCIRCLE" "/PCOPY" "PCROSS" "PDANL" "PDCDF" "PDCFLD" "PDCLR" "PDCMAT" "PDCORR" "PDDMCS" "PDDOEL" "PDEF" "PDEXE" "PDHIST" "PDINQR" "PDLHS" "PDMETH" "PDOT" "PDPINV" "PDPLOT" "PDPROB" "PDRESU" "PDROPT" "/PDS" "PDSAVE" "PDSCAT" "PDSENS" "PDSHIS" "PDUSER" "PDVAR" "PDWRITE" "PERBC2D" "PERTURB" "PFACT" "PHYSICS" "PIVCHECK" "PLCAMP" "PLCFREQ" "PLCHIST" "PLCINT" "PLCPLX" "PLCRACK" "PLDISP" "PLESOL" "PLETAB" "PLFAR" "PLF2D" "PLGEOM" "PLLS" "PLMAP" "PLMC" "PLNEAR" "PLNSOL" "/PLOPTS" "PLORB" "PLOT" "PLOTTING" "PLPAGM" "PLPATH" "PLSECT" "PLST" "PLTIME" "PLTRAC" "PLVAR" "PLVECT" "PLZZ" "/PMACRO" "PMAP" "PMGTRAN" "PMLOPT" "PMLSIZE" "/PMORE" "PNGR" "/PNUM" "POINT" "POLY" "/POLYGON" "/POST1" "/POST26" "POWERH" "PPATH" "PRANGE" "PRAS" "PRCAMP" "PRCINT" "PRCPLX" "PRED" "PRENERGY" "/PREP7" "PRERR" "PRESOL" "PRETAB" "PRFAR" "PRI2" "PRIM" "PRINT" "*PRINT" "PRISM" "PRITER" "PRJSOL" "PRNEAR" "PRNLD" "PRNSOL" "PROD" "PRORB" "PRPATH" "PRRFOR" "PRRSOL" "PRSCONTROL" "PRSECT" "PRTIME" "PRVAR" "PRVECT" "PSCONTROL" "PSCR" "PSDCOM" "PSDFRQ" "PSDGRAPH" "PSDRES" "PSDSPL" "PSDUNIT" "PSDVAL" "PSDWAV" "/PSEARCH" "PSEL" "/PSF" "PSMAT" "PSMESH" "/PSPEC" "/PSTATUS" "PSTRES" "/PSYMB" "PTR" "PTXY" "PVECT" "/PWEDGE" "QDVAL" "QRDOPT" "QSOPT" "QUAD" "/QUIT" "QUOT" "R" "RACE" "RADOPT" "RAPPND" "RATE" "/RATIO" "RBE3" "RCON" "RCYC" "RDEC" "RDELE" "READ" "REAL" "REALVAR" "RECTNG" "REMESH" "/RENAME" "REORDER" "*REPEAT" "/REPLOT" "RESCOMBINE" "RESCONTROL" "RESET" "/RESET" "RESP" "RESUME" "RESVEC" "RESWRITE" "*RETURN" "REXPORT" "REZONE" "RFORCE" "/RGB" "RIGID" "RIGRESP" "RIMPORT" "RLIST" "RMALIST" "RMANL" "RMASTER" "RMCAP" "RMCLIST" "/RMDIR" "RMFLVEC" "RMLVSCALE" "RMMLIST" "RMMRANGE" "RMMSELECT" "RMNDISP" "RMNEVEC" "RMODIF" "RMORE" "RMPORDER" "RMRESUME" "RMRGENERATE" "RMROPTIONS" "RMRPLOT" "RMRSTATUS" "RMSAVE" "RMSMPLE" "RMUSE" "RMXPORT" "ROCK" "ROSE" "RPOLY" "RPR4" "RPRISM" "RPSD" "RSFIT" "RSOPT" "RSPLIT" "RSPLOT" "RSPRNT" "RSSIMS" "RSTMAC" "RSTOFF" "RSURF" "RSYMM" "RSYS" "RTHICK" "SABS" "SADD" "SALLOW" "SAVE" "SBCLIST" "SBCTRAN" "SDELETE" "SE" "SECCONTROL" "SECDATA" "SECFUNCTION" "SECJOINT" "/SECLIB" "SECLOCK" "SECMODIF" "SECNUM" "SECOFFSET" "SECPLOT" "SECREAD" "SECSTOP" "SECTYPE" "SECWRITE" "SED" "SEDLIST" "SEEXP" "/SEG" "SEGEN" "SELIST" "SELM" "SELTOL" "SENERGY" "SEOPT" "SESYMM" "*SET" "SET" "SETFGAP" "SETRAN" "SEXP" "SF" "SFA" "SFACT" "SFADELE" "SFALIST" "SFBEAM" "SFCALC" "SFCUM" "SFDELE" "SFE" "SFEDELE" "SFELIST" "SFFUN" "SFGRAD" "SFL" "SFLDELE" "SFLEX" "SFLIST" "SFLLIST" "SFSCALE" "SFTRAN" "/SHADE" "SHELL" "/SHOW" "/SHOWDISP" "SHPP" "/SHRINK" "SLIST" "SLOAD" "SMALL" "*SMAT" "SMAX" "/SMBC" "SMBODY" "SMCONS" "SMFOR" "SMIN" "SMOOTH" "SMRTSIZE" "SMSURF" "SMULT" "SNOPTION" "SOLU" "/SOLU" "SOLUOPT" "SOLVE" "SORT" "SOURCE" "SPACE" "SPCNOD" "SPCTEMP" "SPDAMP" "SPEC" "SPFREQ" "SPGRAPH" "SPH4" "SPH5" "SPHERE" "SPLINE" "SPLOT" "SPMWRITE" "SPOINT" "SPOPT" "SPREAD" "SPTOPT" "SPOWER" "SPUNIT" "SPVAL" "SQRT" "*SREAD" "SRSS" "SSBT" "/SSCALE" "SSLN" "SSMT" "SSPA" "SSPB" "SSPD" "SSPE" "SSPM" "SSUM" "SSTATE" "STABILIZE" "STAOPT" "STAT" "*STATUS" "/STATUS" "STEF" "/STITLE" "STORE" "SUBOPT" "SUBSET" "SUCALC" "SUCR" "SUDEL" "SUEVAL" "SUGET" "SUMAP" "SUMTYPE" "SUPL" "SUPR" "SURESU" "SUSAVE" "SUSEL" "SUVECT" "SV" "SVPLOT" "SVTYP" "SWADD" "SWDEL" "SWGEN" "SWLIST" "SYNCHRO" "/SYP" "/SYS" "TALLOW" "TARGET" "*TAXIS" "TB" "TBCOPY" "TBDATA" "TBDELE" "TBEO" "TBIN" "TBFIELD" "TBFT" "TBLE" "TBLIST" "TBMODIF" "TBPLOT" "TBPT" "TBTEMP" "TCHG" "/TEE" "TERM" "THEXPAND" "THOPT" "TIFF" "TIME" "TIMERANGE" "TIMINT" "TIMP" "TINTP" "/TITLE" "/TLABEL" "TOFFST" "*TOPER" "TORQ2D" "TORQC2D" "TORQSUM" "TORUS" "TRANS" "TRANSFER" "*TREAD" "TREF" "/TRIAD" "/TRLCY" "TRNOPT" "TRPDEL" "TRPLIS" "TRPOIN" "TRTIME" "TSHAP" "/TSPEC" "TSRES" "TUNIF" "TVAR" "/TXTRE" "/TYPE" "TYPE" "/UCMD" "/UDOC" "/UI" "UIMP" "/UIS" "*ULIB" "UNDELETE" "UNDO" "/UNITS" "UNPAUSE" "UPCOORD" "UPGEOM" "*USE" "/USER" "USRCAL" "USRDOF" "USRELEM" "V" "V2DOPT" "VA" "*VABS" "VADD" "VARDEL" "VARNAM" "VATT" "VCLEAR" "*VCOL" "/VCONE" "VCROSS" "*VCUM" "VDDAM" "VDELE" "VDGL" "VDOT" "VDRAG" "*VEC" "*VEDIT" "VEORIENT" "VEXT" "*VFACT" "*VFILL" "VFOPT" "VFQUERY" "VFSM" "*VFUN" "VGEN" "*VGET" "VGET" "VGLUE" "/VIEW" "VIMP" "VINP" "VINV" "*VITRP" "*VLEN" "VLIST" "VLSCALE" "*VMASK" "VMESH" "VOFFST" "VOLUMES" "*VOPER" "VOVLAP" "*VPLOT" "VPLOT" "VPTN" "*VPUT" "VPUT" "*VREAD" "VROTAT" "VSBA" "VSBV" "VSBW" "/VSCALE" "*VSCFUN" "VSEL" "VSLA" "*VSTAT" "VSUM" "VSWEEP" "VSYMM" "VTRAN" "VTYPE" "/VUP" "*VWRITE" "/WAIT" "WAVES" "WERASE" "WFRONT" "/WINDOW" "WMID" "WMORE" "WPAVE" "WPCSYS" "WPLANE" "WPOFFS" "WPROTA" "WPSTYL" "WRFULL" "WRITE" "WRITEMAP" "*WRK" "WSORT" "WSPRINGS" "WSTART" "WTBCREATE" "XFDATA" "XFENRICH" "XFLIST" "/XFRM" "/XRANGE" "XVAR" "/YRANGE" "/ZOOM" "/WB" "XMLO" "/XML" "CNTR" "EBLOCK" "CMBLOCK" "NBLOCK" "/TRACK" "CWZPLOT" "~EUI" "NELE" "EALL" "NALL" "FLITEM" "LSLN" "PSOLVE" "ASLN" "/VERIFY" "/SSS" "~CFIN" "*EVAL" "*MOONEY" "/RUNSTAT" "ALPFILL" "ARCOLLAPSE" "ARDETACH" "ARFILL" "ARMERGE" "ARSPLIT" "FIPLOT" "GAPFINISH" "GAPLIST" "GAPMERGE" "GAPOPT" "GAPPLOT" "LNCOLLAPSE" "LNDETACH" "LNFILL" "LNMERGE" "LNSPLIT" "PCONV" "PLCONV" "PEMOPTS" "PEXCLUDE" "PINCLUDE" "PMETH" "/PMETH" "PMOPTS" "PPLOT" "PPRANGE" "PRCONV" "PRECISION" "RALL" "RFILSZ" "RITER" "RMEMRY" "RSPEED" "RSTAT" "RTIMST" "/RUNST" "RWFRNT" "SARPLOT" "SHSD" "SLPPLOT" "SLSPLOT" "VCVFILL" "/OPT" "OPEQN" "OPFACT" "OPFRST" "OPGRAD" "OPKEEP" "OPLOOP" "OPPRNT" "OPRAND" "OPSUBP" "OPSWEEP" "OPTYPE" "OPUSER" "OPVAR" "OPADD" "OPCLR" "OPDEL" "OPMAKE" "OPSEL" "OPANL" "OPDATA" "OPRESU" "OPSAVE" "OPEXE" "OPLFA" "OPLGR" "OPLIST" "OPLSW" "OPRFA" "OPRGR" "OPRSW" "PILECALC" "PILEDISPSET" "PILEGEN" "PILELOAD" "PILEMASS" "PILERUN" "PILESEL" "PILESTIF" "PLVAROPT" "PRVAROPT" "TOCOMP" "TODEF" "TOFREQ" "TOTYPE" "TOVAR" "TOEXE" "TOLOOP" "TOGRAPH" "TOLIST" "TOPLOT" "TOPRINT" "TOSTAT" "TZAMESH" "TZDELE" "TZEGEN" "XVAROPT" "PGSAVE" "SOLCONTROL" "TOTAL" "VTGEOM" "VTREAL" "VTSTAT" "PGRAPH" "/VT" "VTIN" "VTRFIL" "VTTEMP" "PGRSET" "VTCLR" "VTMETH" "VTRSLT" "VTVMOD" "PGSELE" "VTDISC" "VTMP" "VTSEC" "PGWRITE" "VTEVAL" "VTOP" "VTSFE" "POUTRES" "VTFREQ" "VTPOST" "VTSL" "FLDATA1-40" "HFPCSWP" "MSDATA" "MSVARY" "QFACT" "FLOCHECK" "HFPOWER" "MSMASS" "PERI" "SPADP" "FLREAD" "HFPORT" "MSMETH" "PLFSS" "SPARM" "FLOTRAN" "HFSCAT" "MSMIR" "PLSCH" "SPFSS" "HFADP" "ICE" "MSNOMF" "PLSYZ" "SPICE" "HFARRAY" "ICEDELE" "MSPROP" "PLTD" "SPSCAN" "HFDEEM" "ICELIST" "MSQUAD" "PLTLINE" "SPSWP" "HFEIGOPT" "ICVFRC" "MSRELAX" "PLVFRC" "HFEREFINE" "LPRT" "MSSOLU" "/PICE" "HFMODPRT" "MSADV" "MSSPEC" "PLWAVE" "HFPA" "MSCAP" "MSTERM" "PRSYZ")
"Ansys symbols for completion in Ansys mode.
By default Ansys keywords, get-functions, parametric-function and elements
 (deprecated as well) are completed.")

(defconst ansys-parametric-function-regexp
"\\(?:A\\(?:BS\\|COS\\|SIN\\|TAN2?\\)\\|C\\(?:OSH?\\|XABS\\)\\|EXP\\|GDIS\\|LOG\\(?:10\\)?\\|MOD\\|NINT\\|RAND\\|S\\(?:I\\(?:GN\\|NH?\\)\\|QRT\\)\\|TANH?\\)"
"Ansys parametric function regexp.")

(defconst ansys-get-function-regexp
"\\(?:A\\(?:NGLE[KN]\\|R\\(?:EA\\(?:KP\\|ND\\)\\|FACE\\|N\\(?:EXT\\|ODE\\)\\)\\|SEL\\|[XYZ]\\)\\|C\\(?:ENTR[XYZ]\\|HRHEX\\)\\|DIST\\(?:EN\\|KP\\|ND\\)\\|E\\(?:L\\(?:ADJ\\|NEXT\\)\\|N\\(?:DS\\|E\\(?:\\(?:AR\\|XT\\)N\\)\\|KE\\)\\|SEL\\)\\|JOIN\\|K\\(?:NEAR\\|PNEXT\\|SEL\\|WGET\\|[PXYZ]\\)\\|L\\(?:S\\(?:EL\\|NEXT\\|[XYZ]\\)\\|WCASE\\|[XYZ]\\)\\|MAG\\|N\\(?:D\\(?:FACE\\|NEXT\\)\\|ELEM\\|MFACE\\|NEAR\\|O\\(?:DE\\(?:DOF\\)?\\|RM\\(?:K[XYZ]\\|N[XYZ]\\)\\)\\|SEL\\|[XYZ]\\)\\|PRES\\|ROT[XYZ]\\|S\\(?:PLIT\\|TR\\(?:COMP\\|FILL\\|LENG\\|POS\\)\\)\\|TEMP\\|U\\(?:PCASE\\|[XYZ]\\)\\|V\\(?:AL\\(?:CHR\\|HEX\\)\\|IRTINQR\\|LNEXT\\|OLT\\|SEL\\|[XYZ]\\)\\)"
"Ansys get function regexp.")

(defconst        ansys-deprecated-element-alist        '(("BEAM3"
.     "BEAM188")     ("BEAM4"      .     "BEAM188")     ("BEAM23"
.     "BEAM188")     ("BEAM24"     .     "BEAM188")     ("BEAM44"
.     "BEAM188")    ("BEAM54"     .    "BEAM188")     ("CONTAC12"
.    "Contac178")   ("CONTAC52"    .   "Contac178")    ("COMBIN7"
.     "MPC184")     ("FLUID79"    .     "Fluid29")     ("FLUID80"
.    "Fluid29")     ("FLUID81"    .     "Fluid29")    ("FLUID141"
. "CFX") ("FLUID142" .  "CFX") ("INFIN9" . "INFIN110") ("INFIN47"
.     "INFIN111")     ("PIPE16"    .     "PIPE288")     ("PIPE18"
.    "ELBOW290")    ("PLANE13"     .    "PLANE223")    ("PLANE25"
.    "PLANE272")    ("PLANE42"     .    "PLANE182")    ("PLANE53"
.    "PLANE233")    ("PLANE67"     .    "PLANE223")    ("PLANE82"
.    "PLANE183")    ("PLANE83"    .    "SOLID273")    ("PLANE145"
. "-")  ("PLANE146" . "-") ("CONTAC12"  . "CONTA178") ("CONTAC52"
.     "CONTA178")     ("LINK1"      .     "LINK180")     ("LINK8"
.     "LINK180")     ("LINK10"     .     "LINK180")     ("LINK32"
.     "LINK33")     ("PIPE16"      .     "PIPE288")     ("PIPE17"
.     "PIPE288")     ("PIPE18"    .     "ELBOW290")     ("PIPE20"
.     "PIPE288")     ("PIPE59"     .     "PIPE288")     ("PIPE60"
.    "ELBOW290")    ("SHELL41"     .    "SHELL181")    ("SHELL43"
.    "SHELL181")    ("SHELL57"     .    "SHELL131")    ("SHELL63"
.    "SHELL181")    ("SHELL91"     .    "SHELL281")    ("SHELL93"
.    "SHELL281")    ("SHELL99"    .    "SHELL281")    ("SHELL150"
.      "-")      ("SOLID5"     .      "SOLID226")      ("SOLID45"
.    "SOLID185")    ("SOLID46"     .    "SOLID185")    ("SOLID65"
.    "SOLID185")    ("SOLID69"     .    "SOLID226")    ("SOLID92"
.    "SOLID187")    ("SOLID95"    .    "SOLID186")    ("SOLID117"
. "SOLID236")  ("SOLID127" . "-") ("SOLID128"  . "-") ("SOLID147"
. "-")  ("SOLID148" .  "-") ("SOLID191" .  "SOLID186") ("VISCO88"
.    "PLANE183")    ("VISCO89"    .    "SOLID186")    ("VISCO106"
.    "PLANE182")    ("VISCO107"   .    "SOLID185")    ("VISCO108"
. "PLANE183") ("TRANS109" . "PLANE223"))
"Association list for Ansys deprecated elements
  and their proposed replacements.")

(defconst ansys-deprecated-element-regexp
"\\<\\(BEAM\\(?:2[34]\\|[45]4\\|[34]\\)\\|CO\\(?:MBIN7\\|NTAC\\(?:[15]2\\)\\)\\|FLUID\\(?:14[12]\\|79\\|8[01]\\)\\|INFIN\\(?:47\\|9\\)\\|LINK\\(?:10\\|32\\|[18]\\)\\|P\\(?:IPE\\(?:1[678]\\|20\\|59\\|60\\)\\|LANE\\(?:1\\(?:3\\|4[56]\\)\\|25\\|42\\|53\\|67\\|8[23]\\)\\)\\|S\\(?:HELL\\(?:150\\|4[13]\\|57\\|63\\|9[139]\\)\\|OLID\\(?:1\\(?:17\\|2[78]\\|4[78]\\|91\\)\\|4[56]\\|5\\|6[59]\\|9[25]\\)\\)\\|TRANS109\\|VISCO\\(?:10[678]\\|8[89]\\)\\)\\>"
"Ansys deprecated elements regexp.")

(defconst ansys-element-regexp
"\\<\\(BEAM1\\(?:61\\|8[89]\\)\\|C\\(?:IRCU\\(?:12[45]\\|94\\)\\|O\\(?:MBI\\(?:165\\|214\\|N\\(?:14\\|3[79]\\|40\\)\\)\\|NTA17[1-8]\\)\\|PT21[23567]\\)\\|ELBOW290\\|F\\(?:LUID\\(?:1\\(?:16\\|29\\|3[0689]\\)\\|2\\(?:2[01]\\|9\\)\\|3[08]\\)\\|OLLW201\\)\\|HSFLD24[12]\\|IN\\(?:FIN\\(?:11[01]\\|47\\|9\\)\\|TER\\(?:19[2-5]\\|20[2-5]\\)\\)\\|LINK\\(?:1\\(?:1\\|6[07]\\|80\\)\\|3[134]\\|68\\)\\|M\\(?:A\\(?:SS\\(?:166\\|[27]1\\)\\|TRIX\\(?:27\\|50\\)\\)\\|ESH200\\|PC184\\)\\|P\\(?:IPE28[89]\\|LANE\\(?:1\\(?:21\\|3\\|62\\|8[23]\\)\\|2\\(?:23\\|3[038]\\|5\\)\\|35\\|5[35]\\|7[578]\\|83\\)\\|RETS179\\)\\|R\\(?:EINF26[345]\\|OM144\\)\\|S\\(?:HELL\\(?:1\\(?:3[12]\\|57\\|63\\|81\\)\\|2\\(?:0[89]\\|81?\\)\\|[46]1\\)\\|O\\(?:L\\(?:ID\\(?:1\\(?:2[23]\\|6[48]\\|8[567]\\)\\|2\\(?:2[67]\\|3[12679]\\|40\\|7[2389]\\|85\\)\\|5\\|65\\|70\\|87\\|9[0678]\\)\\|SH190\\)\\|URC36\\)\\|URF\\(?:15[1-469]\\|25[12]\\)\\)\\|T\\(?:ARGE1\\(?:69\\|70\\)\\|RANS126\\)\\|USER300\\)\\>"
"Ansys elements regexp.")

(defconst ansys-command-regexp-2a
"\\(?:\\*\\(?:DO\\|E\\(?:LSEIF\\|ND\\(?:DO\\|IF\\)?\\)\\|GO\\|IF\\)\\|/\\(?:GO\\|P\\(?:OST\\(?:1\\|26\\)\\|REP7\\)\\|UI\\)\\|A\\(?:BS\\|DD\\|L\\)\\|BF[AEKLV]?\\|C\\(?:QC\\|[EMPS]\\)\\|D\\(?:IG\\|OF\\|[AJKL]\\)\\|E\\(?:MF\\|XP\\|[NT]\\)\\|F[CEJKLPS]\\|G\\(?:[AR]?P\\)\\|IC\\|K\\(?:BC\\|L\\)\\|M\\(?:A[PT]\\|MF\\|P\\)\\|PTR\\|S\\(?:E[DT]\\|F[AEL]\\|[EFV]\\)\\|TB\\|VA\\|[AC-FK-NRV]\\)"
"Ansys keyword name regexp 2a")

(defconst ansys-command-regexp-2b
"\\(?:\\*\\(?:M\\(?:F\\(?:OU\\(?:RI?\\)?\\|UN\\|[OU]\\)\\|OP\\(?:ER?\\)?\\|SG\\|ULT?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|NRM\\|PRI\\(?:NT?\\)?\\|RE\\(?:PE\\(?:AT?\\)?\\|TU\\(?:RN?\\)?\\|[PT]\\)\\|S\\(?:ET\\|MAT?\\|RE\\(?:AD?\\)?\\|TA\\(?:T\\(?:US?\\)?\\)?\\)\\|T\\(?:AX\\(?:IS?\\)?\\|OP\\(?:ER?\\)?\\|RE\\(?:AD?\\)?\\)\\|U\\(?:LIB?\\|SE\\)\\|V\\(?:ABS?\\|C\\(?:OL\\|UM\\|[OU]\\)\\|E\\(?:DIT?\\|[CD]\\)\\|F\\(?:ACT?\\|ILL?\\|UN\\|[AIU]\\)\\|GET?\\|IT\\(?:RP?\\)?\\|LEN?\\|MA\\(?:SK?\\)?\\|OP\\(?:ER?\\)?\\|P\\(?:LOT?\\|UT\\|[LU]\\)\\|RE\\(?:AD?\\)?\\|S\\(?:CF\\(?:UN?\\)?\\|TAT?\\|[CT]\\)\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|WRK\\)\\|/\\(?:M\\(?:KD\\(?:IR?\\)?\\|PL\\(?:IB?\\)?\\|REP?\\|ST\\(?:A\\(?:RT?\\)?\\)?\\)\\|N\\(?:ERR?\\|O\\(?:ER\\(?:A\\(?:SE?\\)?\\)?\\|LI\\(?:ST?\\)?\\|PR\\|RM\\(?:AL?\\)?\\|[ELPR]\\)\\|UM\\(?:B\\(?:ER?\\)?\\)?\\)\\|OUT\\(?:P\\(?:UT?\\)?\\)?\\|P\\(?:AGE?\\|B[CF]\\|C\\(?:IR\\(?:C\\(?:LE?\\)?\\)?\\|OPY?\\|[IO]\\)\\|DS\\|LO\\(?:P\\(?:TS?\\)?\\)?\\|M\\(?:AC\\(?:RO?\\)?\\|ORE?\\|[AO]\\)\\|NUM?\\|OL\\(?:Y\\(?:G\\(?:ON?\\)?\\)?\\)?\\|S\\(?:EA\\(?:R\\(?:CH?\\)?\\)?\\|PEC?\\|TA\\(?:T\\(?:US?\\)?\\)?\\|YMB?\\|[EFPTY]\\)\\|WE\\(?:D\\(?:GE?\\)?\\)?\\)\\|QUIT?\\|R\\(?:AT\\(?:IO?\\)?\\|E\\(?:NA\\(?:ME?\\)?\\|PL\\(?:OT?\\)?\\|SET?\\|[NPS]\\)\\|GB\\|MD\\(?:IR?\\)?\\)\\|S\\(?:E\\(?:CL\\(?:IB?\\)?\\|[CG]\\)\\|H\\(?:ADE?\\|OW\\(?:D\\(?:I\\(?:SP?\\)?\\)?\\)?\\|RI\\(?:NK?\\)?\\|[AR]\\)\\|MBC?\\|OLU?\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|T\\(?:AT\\(?:US?\\)?\\|IT\\(?:LE?\\)?\\|[AI]\\)\\|Y[PS]\\)\\|T\\(?:EE\\|IT\\(?:LE?\\)?\\|LA\\(?:B\\(?:EL?\\)?\\)?\\|R\\(?:IAD?\\|LCY?\\|[IL]\\)\\|SP\\(?:EC?\\)?\\|XT\\(?:RE?\\)?\\|YPE?\\)\\|U\\(?:CMD?\\|DOC?\\|IS\\|NI\\(?:TS?\\)?\\|SER?\\)\\|V\\(?:CO\\(?:NE?\\)?\\|IEW?\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|UP\\)\\|W\\(?:AIT?\\|IN\\(?:D\\(?:OW?\\)?\\)?\\)\\|X\\(?:FRM?\\|RA\\(?:N\\(?:GE?\\)?\\)?\\)\\|YRA\\(?:N\\(?:GE?\\)?\\)?\\|ZOOM?\\)\\|M\\(?:F\\(?:AN\\|BU\\(?:C\\(?:K\\(?:ET?\\)?\\)?\\)?\\|C\\(?:ALC?\\|LE\\(?:AR?\\)?\\|MM\\(?:A\\(?:ND?\\)?\\)?\\|ONV?\\|[AILMO]\\)\\|DT\\(?:I\\(?:ME?\\)?\\)?\\|E\\(?:LEM?\\|XT\\(?:ER?\\)?\\|[LMX]\\)\\|F\\(?:NA\\(?:ME?\\)?\\|[NR]\\)\\|I\\(?:MP\\(?:O\\(?:RT?\\)?\\)?\\|NT\\(?:ER?\\)?\\|TER?\\|[MNT]\\)\\|L\\(?:CO\\(?:MM?\\)?\\|IST?\\|[CI]\\)\\|MAP?\\|O\\(?:RD\\(?:ER?\\)?\\|UT\\(?:P\\(?:UT?\\)?\\)?\\|[RU]\\)\\|PS\\(?:I\\(?:M\\(?:UL?\\)?\\)?\\)?\\|R\\(?:EL\\(?:AX?\\)?\\|ST\\(?:A\\(?:RT?\\)?\\)?\\|[CES]\\)\\|S\\(?:OR\\(?:D\\(?:ER?\\)?\\)?\\|UR\\(?:F\\(?:A\\(?:CE?\\)?\\)?\\)?\\|[OU]\\)\\|T\\(?:IME?\\|OL\\|[IO]\\)\\|VO\\(?:L\\(?:U\\(?:ME?\\)?\\)?\\)?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|GEN\\|IDT\\(?:OL?\\)?\\|LIST?\\|MASS?\\|O\\(?:D\\(?:CO\\(?:NT?\\)?\\|IFY?\\|MSH?\\|OPT?\\|SE\\(?:L\\(?:O\\(?:P\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)?\\)?\\)?\\|[CEIMOS]\\)\\|NI\\(?:T\\(?:OR?\\)?\\)?\\|PT\\|RPH?\\|VE\\)\\|P\\(?:AM\\(?:OD?\\)?\\|C\\(?:HG\\|OPY?\\|[HO]\\)\\|D\\(?:ATA?\\|ELE?\\|RES?\\|[AER]\\)\\|LI\\(?:ST?\\)?\\|PL\\(?:OT?\\)?\\|R\\(?:EAD?\\|INT?\\|[EI]\\)\\|T\\(?:EMP?\\|GEN?\\|RES?\\|[EGR]\\)\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|S\\(?:AVE?\\|H\\(?:APE?\\|CO\\(?:PY?\\)?\\|KEY?\\|MID?\\|PA\\(?:T\\(?:T\\(?:E\\(?:RN?\\)?\\)?\\)?\\)?\\|[ACKMP]\\)\\|OL\\(?:VE?\\)?\\|TO\\(?:LE?\\)?\\)\\|XPA\\(?:ND?\\)?\\)\\|N\\(?:A\\(?:NG\\|XIS?\\)\\|CNV\\|D\\(?:ELE?\\|IST?\\|SU\\(?:RF?\\)?\\)\\|EQIT?\\|FOR\\(?:CE?\\)?\\|GEN\\|KPT\\|L\\(?:AD\\(?:A\\(?:P\\(?:T\\(?:I\\(?:VE?\\)?\\)?\\)?\\)?\\)?\\|D\\(?:IAG?\\|PO\\(?:ST?\\)?\\|[IP]\\)\\|GE\\(?:OM?\\)?\\|HI\\(?:ST?\\)?\\|IST?\\|ME\\(?:SH?\\)?\\|O\\(?:PT\\|[GP]\\)\\)\\|MOD\\(?:IF?\\)?\\|O\\(?:CO\\(?:L\\(?:OR?\\)?\\)?\\|DES?\\|O\\(?:FF\\(?:S\\(?:ET?\\)?\\)?\\|RD\\(?:ER?\\)?\\|[FR]\\)\\|R[AL]\\)\\|P\\(?:LOT?\\|RI\\(?:NT?\\)?\\)\\|R\\(?:E\\(?:AD\\|FI\\(?:NE?\\)?\\|[AF]\\)\\|LS\\(?:UM?\\)?\\|O\\(?:PT\\|TAT?\\|[PT]\\)\\|RA\\(?:NG?\\)?\\)\\|S\\(?:CA\\(?:LE?\\)?\\|EL\\|L[AEKLV]\\|MO\\(?:O\\(?:TH?\\)?\\)?\\|O\\(?:RT\\|[LR]\\)\\|TO\\(?:RE?\\)?\\|UB\\(?:ST?\\)?\\|VR\\|YM\\)\\|U\\(?:M\\(?:CMP?\\|EXP?\\|MRG?\\|OFF?\\|STR?\\|VAR?\\|[CEMOSV]\\)\\|SO\\(?:RT?\\)?\\)\\|W\\(?:P\\(?:AVE?\\|LAN?\\|[AL]\\)\\|RI\\(?:TE?\\)?\\)\\)\\|O\\(?:C\\(?:D\\(?:ATA?\\|EL\\(?:E\\(?:TE?\\)?\\)?\\|[AE]\\)\\|LI\\(?:ST?\\)?\\|RE\\(?:AD?\\)?\\|T\\(?:AB\\(?:LE?\\)?\\|YPE?\\|[AY]\\)\\|ZO\\(?:NE?\\)?\\)\\|MEGA?\\|P\\(?:ER\\(?:A\\(?:TE?\\)?\\)?\\|NC\\(?:O\\(?:N\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\)?\\)?\\)\\|UT\\(?:AE\\(?:RO?\\)?\\|OPT?\\|PR\\|RES?\\|[AOPR]\\)\\|VCH\\(?:E\\(?:CK?\\)?\\)?\\)\\|P\\(?:A\\(?:DE\\(?:LE?\\)?\\|GET?\\|PUT?\\|R\\(?:ESU?\\|RES?\\|SAV?\\|TS\\(?:EL?\\)?\\|[ERST]\\)\\|SA\\(?:VE?\\)?\\|TH\\|USE?\\)\\|C\\(?:ALC?\\|GO\\(?:PT?\\)?\\|IRC?\\|RO\\(?:SS?\\)?\\)\\|D\\(?:ANL?\\|C\\(?:DF\\|FLD?\\|LR\\|MAT?\\|ORR?\\|[DFLMO]\\)\\|D\\(?:MCS?\\|OEL?\\|[MO]\\)\\|E\\(?:XE\\|[FX]\\)\\|HI\\(?:ST?\\)?\\|IN\\(?:QR?\\)?\\|LHS?\\|ME\\(?:TH?\\)?\\|OT\\|P\\(?:INV?\\|LOT?\\|ROB?\\|[ILR]\\)\\|R\\(?:ESU?\\|OPT?\\|[EO]\\)\\|S\\(?:AVE?\\|CAT?\\|ENS?\\|HIS?\\|[ACEH]\\)\\|US\\(?:ER?\\)?\\|VAR?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|ER\\(?:BC\\(?:2D?\\)?\\|TU\\(?:RB?\\)?\\|[BT]\\)\\|FACT?\\|HYS\\(?:I\\(?:CS?\\)?\\)?\\|IVC\\(?:H\\(?:E\\(?:CK?\\)?\\)?\\)?\\|L\\(?:C\\(?:AMP?\\|FR\\(?:EQ?\\)?\\|HI\\(?:ST?\\)?\\|INT?\\|PLX?\\|RA\\(?:CK?\\)?\\|[AFHIPR]\\)\\|DI\\(?:SP?\\)?\\|E\\(?:SOL?\\|TAB?\\|[ST]\\)\\|F\\(?:2D\\|AR\\|[2A]\\)\\|GE\\(?:OM?\\)?\\|LS\\|M\\(?:AP\\|[AC]\\)\\|N\\(?:EAR?\\|SOL?\\|[ES]\\)\\|O\\(?:RB\\|TT\\(?:I\\(?:NG?\\)?\\)?\\|[RT]\\)\\|PA\\(?:GM\\|TH\\|[GT]\\)\\|S\\(?:ECT?\\|[ET]\\)\\|T\\(?:IME?\\|RAC?\\|[IR]\\)\\|V\\(?:AR\\|ECT?\\|[AE]\\)\\|ZZ\\)\\|M\\(?:AP\\|GT\\(?:R\\(?:AN?\\)?\\)?\\|L\\(?:OPT?\\|SI\\(?:ZE?\\)?\\|[OS]\\)\\)\\|NGR\\|O\\(?:INT?\\|LY\\|WE\\(?:RH?\\)?\\)\\|PATH?\\|R\\(?:A\\(?:NGE?\\|[NS]\\)\\|C\\(?:AMP?\\|INT?\\|PLX?\\|[AIP]\\)\\|E\\(?:NE\\(?:R\\(?:GY?\\)?\\)?\\|RR\\|SOL?\\|TAB?\\|[DNRST]\\)\\|FAR?\\|I\\(?:NT\\|SM\\|TER?\\|[2MNST]\\)\\|JS\\(?:OL?\\)?\\|N\\(?:EAR?\\|LD\\|SOL?\\|[ELS]\\)\\|O\\(?:RB\\|[DR]\\)\\|PA\\(?:TH?\\)?\\|R\\(?:FOR?\\|SOL?\\|[FS]\\)\\|S\\(?:CO\\(?:N\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\)?\\|ECT?\\|[CE]\\)\\|TI\\(?:ME?\\)?\\|V\\(?:AR\\|ECT?\\|[AE]\\)\\)\\|S\\(?:C\\(?:ON\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\|[OR]\\)\\|D\\(?:COM?\\|FRQ?\\|GR\\(?:A\\(?:PH?\\)?\\)?\\|RES?\\|SPL?\\|UN\\(?:IT?\\)?\\|VAL?\\|WAV?\\|[CFGRSUVW]\\)\\|EL\\|M\\(?:AT\\|ESH?\\|[AE]\\)\\|TR\\(?:ES?\\)?\\)\\|TXY\\|VECT?\\)\\|Q\\(?:DVAL?\\|RDO\\(?:PT?\\)?\\|SOPT?\\|U\\(?:AD\\|OT\\)\\)\\|R\\(?:A\\(?:CE\\|DO\\(?:PT?\\)?\\|PP\\(?:ND?\\)?\\|TE\\)\\|BE3\\|C\\(?:ON\\|YC\\)\\|DE\\(?:LE\\|[CL]\\)\\|E\\(?:A\\(?:LV\\(?:AR?\\)?\\|[DL]\\)\\|CT\\(?:NG?\\)?\\|ME\\(?:SH?\\)?\\|OR\\(?:D\\(?:ER?\\)?\\)?\\|S\\(?:CO\\(?:MB\\(?:I\\(?:NE?\\)?\\)?\\|NT\\(?:R\\(?:OL?\\)?\\)?\\|[MN]\\)\\|ET\\|UME?\\|VEC?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\|[EPUVW]\\)\\|XP\\(?:O\\(?:RT?\\)?\\)?\\|ZO\\(?:NE?\\)?\\)\\|FOR\\(?:CE?\\)?\\|I\\(?:G\\(?:ID\\|RE\\(?:SP?\\)?\\|[IR]\\)\\|MP\\(?:O\\(?:RT?\\)?\\)?\\)\\|LIST?\\|M\\(?:A\\(?:LI\\(?:ST?\\)?\\|NL\\|ST\\(?:ER?\\)?\\|[LNS]\\)\\|C\\(?:AP\\|LI\\(?:ST?\\)?\\|[AL]\\)\\|FL\\(?:V\\(?:EC?\\)?\\)?\\|LV\\(?:S\\(?:C\\(?:A\\(?:LE?\\)?\\)?\\)?\\)?\\|M\\(?:LI\\(?:ST?\\)?\\|RA\\(?:N\\(?:GE?\\)?\\)?\\|SE\\(?:L\\(?:E\\(?:CT?\\)?\\)?\\)?\\|[LRS]\\)\\|N\\(?:DI\\(?:SP?\\)?\\|EV\\(?:EC?\\)?\\|[DE]\\)\\|O\\(?:DIF?\\|RE\\|[DR]\\)\\|PO\\(?:R\\(?:D\\(?:ER?\\)?\\)?\\)?\\|R\\(?:ES\\(?:U\\(?:ME?\\)?\\)?\\|GE\\(?:N\\(?:E\\(?:R\\(?:A\\(?:TE?\\)?\\)?\\)?\\)?\\)?\\|OP\\(?:T\\(?:I\\(?:O\\(?:NS?\\)?\\)?\\)?\\)?\\|PL\\(?:OT?\\)?\\|ST\\(?:A\\(?:T\\(?:US?\\)?\\)?\\)?\\|[EGOPS]\\)\\|S\\(?:AVE?\\|MP\\(?:LE?\\)?\\|[AM]\\)\\|USE?\\|XP\\(?:O\\(?:RT?\\)?\\)?\\)\\|O\\(?:CK\\|SE\\)\\|P\\(?:OLY?\\|R\\(?:ISM?\\|[4I]\\)\\|SD\\)\\|S\\(?:FIT?\\|OPT?\\|P\\(?:L\\(?:[IO]T\\|[IO]\\)\\|R\\(?:NT?\\)?\\)\\|SI\\(?:MS?\\)?\\|T\\(?:MAC?\\|OFF?\\|[MO]\\)\\|URF?\\|Y\\(?:MM\\|[MS]\\)\\)\\|THI\\(?:CK?\\)?\\)\\|S\\(?:A\\(?:BS\\|DD\\|LL\\(?:OW?\\)?\\|VE\\)\\|BC\\(?:LI\\(?:ST?\\)?\\|TR\\(?:AN?\\)?\\|[LT]\\)\\|DEL\\(?:E\\(?:TE?\\)?\\)?\\|E\\(?:C\\(?:CO\\(?:N\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\)?\\|DA\\(?:TA?\\)?\\|FU\\(?:N\\(?:C\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)?\\)?\\|JO\\(?:I\\(?:NT?\\)?\\)?\\|LO\\(?:CK?\\)?\\|MO\\(?:D\\(?:IF?\\)?\\)?\\|NUM?\\|OF\\(?:F\\(?:S\\(?:ET?\\)?\\)?\\)?\\|PL\\(?:OT?\\)?\\|RE\\(?:AD?\\)?\\|ST\\(?:OP?\\)?\\|TY\\(?:PE?\\)?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\|[CDFJL-PRSTW]\\)\\|DL\\(?:I\\(?:ST?\\)?\\)?\\|EXP?\\|GEN?\\|L\\(?:IST?\\|TOL?\\|[IMT]\\)\\|NE\\(?:R\\(?:GY?\\)?\\)?\\|OPT?\\|SY\\(?:MM?\\)?\\|T\\(?:FG\\(?:AP?\\)?\\|RAN?\\|[FR]\\)\\|XP\\)\\|F\\(?:A\\(?:CT\\|DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|[CDL]\\)\\|BE\\(?:AM?\\)?\\|C\\(?:ALC?\\|UM\\|[AU]\\)\\|DE\\(?:LE?\\)?\\|E\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|[DL]\\)\\|FUN?\\|GR\\(?:AD?\\)?\\|L\\(?:DE\\(?:LE?\\)?\\|EX\\|IST?\\|LI\\(?:ST?\\)?\\|[DEIL]\\)\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|TR\\(?:AN?\\)?\\)\\|H\\(?:ELL?\\|PP\\)\\|L\\(?:IST?\\|OAD?\\)\\|M\\(?:A\\(?:LL\\|[LX]\\)\\|BO\\(?:DY?\\)?\\|CO\\(?:NS?\\)?\\|FOR?\\|IN\\|OO\\(?:TH?\\)?\\|RT\\(?:S\\(?:I\\(?:ZE?\\)?\\)?\\)?\\|SU\\(?:RF?\\)?\\|ULT?\\)\\|NOP\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\|O\\(?:L\\(?:UO\\(?:PT?\\)?\\|VE\\|[UV]\\)\\|RT\\|UR\\(?:CE?\\)?\\)\\|P\\(?:ACE?\\|C\\(?:NOD?\\|TE\\(?:MP?\\)?\\|[NT]\\)\\|DA\\(?:MP?\\)?\\|EC\\|FR\\(?:EQ?\\)?\\|GR\\(?:A\\(?:PH?\\)?\\)?\\|H\\(?:ERE?\\|[45E]\\)\\|L\\(?:INE?\\|OT\\|[IO]\\)\\|MW\\(?:R\\(?:I\\(?:TE?\\)?\\)?\\)?\\|O\\(?:INT?\\|PT\\|WER?\\|[IPW]\\)\\|RE\\(?:AD?\\)?\\|TO\\(?:PT?\\)?\\|UN\\(?:IT?\\)?\\|VAL?\\)\\|QRT\\|RSS\\|S\\(?:BT\\|LN\\|MT\\|P[ABDEM]\\|TA\\(?:TE?\\)?\\|UM\\)\\|T\\(?:A\\(?:BI\\(?:L\\(?:I\\(?:ZE?\\)?\\)?\\)?\\|OPT?\\|[BOT]\\)\\|EF\\|ORE?\\)\\|U\\(?:B\\(?:OPT?\\|SET?\\|[OS]\\)\\|C\\(?:ALC?\\|[AR]\\)\\|DEL?\\|EV\\(?:AL?\\)?\\|GET?\\|M\\(?:AP\\|TY\\(?:PE?\\)?\\|[AT]\\)\\|P[LR]\\|RE\\(?:SU?\\)?\\|S\\(?:AVE?\\|EL\\|[AE]\\)\\|VE\\(?:CT?\\)?\\)\\|V\\(?:PL\\(?:OT?\\)?\\|TYP?\\)\\|W\\(?:ADD?\\|DEL?\\|GEN?\\|LI\\(?:ST?\\)?\\)\\|YNC\\(?:H\\(?:RO?\\)?\\)?\\)\\|T\\(?:A\\(?:LL\\(?:OW?\\)?\\|RG\\(?:ET?\\)?\\)\\|B\\(?:CO\\(?:PY?\\)?\\|D\\(?:ATA?\\|ELE?\\|[AE]\\)\\|EO\\|F\\(?:IE\\(?:LD?\\)?\\|[IT]\\)\\|IN\\|L\\(?:IST?\\|[EI]\\)\\|MO\\(?:D\\(?:IF?\\)?\\)?\\|P\\(?:LOT?\\|[LT]\\)\\|TE\\(?:MP?\\)?\\)\\|CHG\\|ERM\\|H\\(?:EX\\(?:P\\(?:A\\(?:ND?\\)?\\)?\\)?\\|OPT?\\)\\|I\\(?:FF\\|M\\(?:ER\\(?:A\\(?:N\\(?:GE?\\)?\\)?\\)?\\|INT?\\|[EIP]\\)\\|NTP?\\)\\|O\\(?:FF\\(?:ST?\\)?\\|R\\(?:Q\\(?:2D\\|C2D?\\|SUM?\\|[2CS]\\)\\|US?\\)\\)\\|R\\(?:ANS\\(?:F\\(?:ER?\\)?\\)?\\|EF\\|NO\\(?:PT?\\)?\\|P\\(?:DEL?\\|LIS?\\|OIN?\\|[DLO]\\)\\|TI\\(?:ME?\\)?\\)\\|S\\(?:HAP?\\|RES?\\)\\|UNIF?\\|VAR\\|YPE\\)\\|U\\(?:IMP\\|N\\(?:D\\(?:EL\\(?:E\\(?:TE?\\)?\\)?\\|[EO]\\)\\|PA\\(?:U\\(?:SE?\\)?\\)?\\)\\|P\\(?:CO\\(?:O\\(?:RD?\\)?\\)?\\|GE\\(?:OM?\\)?\\)\\|SR\\(?:CAL?\\|DOF?\\|EL\\(?:EM?\\)?\\|[CDE]\\)\\)\\|V\\(?:2DO\\(?:PT?\\)?\\|A\\(?:DD\\|R\\(?:DEL?\\|NAM?\\|[DN]\\)\\|TT\\)\\|C\\(?:LE\\(?:AR?\\)?\\|RO\\(?:SS?\\)?\\)\\|D\\(?:DAM?\\|ELE?\\|GL\\|OT\\|RAG?\\)\\|E\\(?:OR\\(?:I\\(?:E\\(?:NT?\\)?\\)?\\)?\\|XT\\)\\|F\\(?:OPT?\\|QU\\(?:E\\(?:RY?\\)?\\)?\\|SM\\)\\|G\\(?:E[NT]\\|LUE?\\)\\|I\\(?:MP\\|N[PV]\\)\\|L\\(?:IST?\\|SC\\(?:A\\(?:LE?\\)?\\)?\\)\\|MESH?\\|O\\(?:FF\\(?:ST?\\)?\\|LU\\(?:M\\(?:ES?\\)?\\)?\\|VL\\(?:AP?\\)?\\)\\|P\\(?:LOT?\\|TN\\|UT\\)\\|ROT\\(?:AT?\\)?\\|S\\(?:B[AVW]\\|EL\\|LA\\|UM\\|WE\\(?:EP?\\)?\\|YMM?\\)\\|T\\(?:RAN?\\|YPE?\\)\\)\\|W\\(?:AVES?\\|ERA\\(?:SE?\\)?\\|FRO\\(?:NT?\\)?\\|M\\(?:ID\\|ORE?\\)\\|P\\(?:AVE?\\|CS\\(?:YS?\\)?\\|LA\\(?:NE?\\)?\\|OF\\(?:FS?\\)?\\|RO\\(?:TA?\\)?\\|ST\\(?:YL?\\)?\\)\\|R\\(?:FU\\(?:LL?\\)?\\|ITE\\(?:M\\(?:AP?\\)?\\)?\\)\\|S\\(?:ORT?\\|PR\\(?:I\\(?:N\\(?:GS?\\)?\\)?\\)?\\|TA\\(?:RT?\\)?\\)\\|TBC\\(?:R\\(?:E\\(?:A\\(?:TE?\\)?\\)?\\)?\\)?\\)\\|X\\(?:F\\(?:DA\\(?:TA?\\)?\\|EN\\(?:R\\(?:I\\(?:CH?\\)?\\)?\\)?\\|LI\\(?:ST?\\)?\\)\\|VAR\\)\\)"
"Ansys keyword name regexp 2b")

(defconst ansys-command-regexp-2c
"\\(?:\\*\\(?:A\\(?:BBR?\\|FUN?\\|SK\\|XPY?\\)\\|C\\(?:F\\(?:CL\\(?:OS?\\)?\\|OP\\(?:EN?\\)?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\|[COW]\\)\\|OMP?\\|RE\\(?:A\\(?:TE?\\)?\\)?\\|YC\\(?:LE?\\)?\\)\\|D\\(?:EL\\|IM\\|MAT?\\|O\\(?:WH\\(?:I\\(?:LE?\\)?\\)?\\|[TW]\\)\\)\\|E\\(?:IG\\(?:EN?\\)?\\|LSE\\|X\\(?:IT\\|PO\\(?:RT?\\)?\\|[IP]\\)\\)\\|F\\(?:FT\\|REE?\\)\\|GET\\|I\\(?:NIT?\\|TE\\(?:N\\(?:G\\(?:I\\(?:NE?\\)?\\)?\\)?\\)?\\)\\|L\\(?:IST?\\|S\\(?:BAC?\\|DU\\(?:MP?\\)?\\|EN\\(?:G\\(?:I\\(?:NE?\\)?\\)?\\)?\\|FA\\(?:C\\(?:T\\(?:OR?\\)?\\)?\\)?\\|RE\\(?:S\\(?:T\\(?:O\\(?:RE?\\)?\\)?\\)?\\)?\\|[BDEFR]\\)\\)\\)\\|/\\(?:A\\(?:N\\(?:3D\\|FI\\(?:LE?\\)?\\|GLE?\\|NOT?\\|UM\\|[3FGNU]\\)\\|SS\\(?:I\\(?:GN?\\)?\\)?\\|U\\(?:TO?\\|X\\(?:1[25]\\|[23]\\)\\)\\|XL\\(?:AB?\\)?\\)\\|BAT\\(?:CH?\\)?\\|C\\(?:FO\\(?:R\\(?:M\\(?:AT?\\)?\\)?\\)?\\|L\\(?:AB\\(?:EL?\\)?\\|EAR?\\|OG\\|[AEO]\\)\\|MAP?\\|O\\(?:LOR?\\|N\\(?:FIG?\\|TO\\(?:UR?\\)?\\|[FT]\\)\\|PY\\|[LMP]\\)\\|PL\\(?:A\\(?:NE?\\)?\\)?\\|TY\\(?:PE?\\)?\\|VAL?\\|WD\\|YC\\(?:E\\(?:X\\(?:P\\(?:A\\(?:ND?\\)?\\)?\\)?\\)?\\)?\\)\\|D\\(?:E\\(?:L\\(?:E\\(?:TE?\\)?\\)?\\|V\\(?:DI\\(?:SP?\\)?\\|ICE?\\|[DI]\\)\\)\\|FL\\(?:AB?\\)?\\|I\\(?:RE\\(?:C\\(?:T\\(?:O\\(?:RY?\\)?\\)?\\)?\\)?\\|ST\\|[RS]\\)\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|V3D?\\)\\|E\\(?:DGE?\\|FA\\(?:C\\(?:ET?\\)?\\)?\\|OF\\|RA\\(?:SE?\\)?\\|SH\\(?:A\\(?:PE?\\)?\\)?\\|X\\(?:IT\\|PA\\(?:ND?\\)?\\|[IP]\\)\\)\\|F\\(?:AC\\(?:ET?\\)?\\|DE\\(?:LE?\\)?\\|IL\\(?:N\\(?:A\\(?:ME?\\)?\\)?\\)?\\|O\\(?:CUS?\\|RM\\(?:AT?\\)?\\|[CR]\\)\\)\\|G\\(?:C\\(?:MD\\|OL\\(?:U\\(?:MN?\\)?\\)?\\|[MO]\\)\\|F\\(?:ILE?\\|OR\\(?:M\\(?:AT?\\)?\\)?\\|[IO]\\)\\|LI\\(?:NE?\\)?\\|MA\\(?:R\\(?:K\\(?:ER?\\)?\\)?\\)?\\|O\\(?:LI\\(?:ST?\\)?\\|PR\\|[LP]\\)\\|R\\(?:AP\\(?:H\\(?:I\\(?:CS?\\)?\\)?\\)?\\|ES\\(?:U\\(?:ME?\\)?\\)?\\|ID\\|OPT?\\|TYP?\\|[AEIOT]\\)\\|S\\(?:AVE?\\|[AT]\\)\\|T\\(?:HK\\|YPE?\\|[HY]\\)\\)\\|H\\(?:BC\\|EA\\(?:D\\(?:ER?\\)?\\)?\\)\\|I\\(?:C\\(?:LW\\(?:ID?\\)?\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|[LS]\\)\\|MA\\(?:GE?\\)?\\|N\\(?:PUT?\\|QU\\(?:I\\(?:RE?\\)?\\)?\\|[PQ]\\)\\)\\|L\\(?:ARC?\\|I\\(?:GHT?\\|NE\\|[GN]\\)\\|S\\(?:PEC?\\|YM\\(?:B\\(?:OL?\\)?\\)?\\|[PY]\\)\\)\\|M\\(?:A\\(?:IL\\|[IP]\\)\\|ENU?\\)\\)\\|A\\(?:A\\(?:DD\\|TT\\)\\|B\\(?:B\\(?:RES?\\|SAV?\\|[RS]\\)\\|EX\\(?:T\\(?:R\\(?:A\\(?:CT?\\)?\\)?\\)?\\)?\\)\\|C\\(?:C\\(?:AT\\|OP\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\|[AO]\\)\\|EL\\|LE\\(?:AR?\\)?\\)\\|D\\(?:A\\(?:MS\\|PT\\|[MP]\\)\\|DAM?\\|ELE?\\|GL\\|RAG?\\)\\|ESI\\(?:ZE?\\)?\\|F\\(?:IL\\(?:LT?\\)?\\|LI\\(?:ST?\\)?\\|SU\\(?:RF?\\)?\\)\\|G\\(?:EN\\|LUE?\\)\\|IN[APV]\\|L\\(?:IST?\\|LS\\(?:EL?\\)?\\|PH\\(?:AD?\\)?\\)\\|M\\(?:AP\\|ESH?\\)\\|N\\(?:C\\(?:NTR?\\|UT\\|YC\\|[NUY]\\)\\|D\\(?:ATA?\\|SCL?\\|YNA?\\|[ASY]\\)\\|FL\\(?:OW?\\)?\\|HA\\(?:RM?\\)?\\|I\\(?:SOS?\\|[MS]\\)\\|M\\(?:ODE?\\|RES?\\|[OR]\\)\\|ORM?\\|PR\\(?:ES?\\)?\\|S\\(?:OL?\\|TOA\\(?:QWA?\\|SAS?\\|[QS]\\)\\)\\|T\\(?:IME?\\|YPE?\\|[IY]\\)\\)\\|O\\(?:FF\\(?:ST?\\)?\\|VL\\(?:AP?\\)?\\)\\|P\\(?:LOT?\\|PE\\(?:ND?\\)?\\|TN\\)\\|R\\(?:C\\(?:LEN?\\|TRM?\\|[LT]\\)\\|E\\(?:AS\\|FI\\(?:NE?\\)?\\|ME\\(?:SH?\\)?\\|VE\\(?:R\\(?:SE?\\)?\\)?\\|[AFMV]\\)\\|OT\\(?:AT?\\)?\\|S\\(?:CA\\(?:LE?\\)?\\|YM\\|[CY]\\)\\)\\|S\\(?:B[ALVW]\\|CR\\(?:ES?\\)?\\|EL\\|IF\\(?:I\\(?:LE?\\)?\\)?\\|KIN?\\|L[LV]\\|OL\\|U[BM]\\)\\|T\\(?:AN\\|RAN?\\|YPE?\\)\\|UTO\\(?:TS?\\)?\\|V\\(?:PR\\(?:IN?\\)?\\|RES?\\)\\|WAVE?\\)\\|B\\(?:CSO\\(?:P\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)?\\|ETAD?\\|F\\(?:A\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|[DL]\\)\\|CUM?\\|DE\\(?:LE?\\)?\\|E\\(?:CUM?\\|DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|SC\\(?:AL?\\)?\\|[CDLS]\\)\\|INT?\\|K\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|[DL]\\)\\|L\\(?:DE\\(?:LE?\\)?\\|IST?\\|LI\\(?:ST?\\)?\\|[DIL]\\)\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|TR\\(?:AN?\\)?\\|UN\\(?:IF?\\)?\\|V\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|[DL]\\)\\)\\|IO\\(?:OPT?\\|[OT]\\)\\|L\\(?:C[45]\\|OCK?\\)\\|O\\(?:OL\\|PTN?\\)\\|S\\(?:AX\\|M[12D]\\|PL\\(?:IN?\\)?\\|S[12]\\|T[EQ]\\)\\|TOL\\|UCO\\(?:PT?\\)?\\)\\|C\\(?:A\\(?:LC\\|MP\\(?:B\\(?:E\\(?:LL?\\)?\\)?\\)?\\)\\|B\\(?:DOF?\\|M[DX]\\|T\\(?:MP\\|[EM]\\)\\)\\|D\\(?:OPT?\\|RE\\(?:AD?\\)?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|E\\(?:C\\(?:HE\\(?:CK?\\)?\\|MOD?\\|YC\\|[HMY]\\)\\|DE\\(?:LE?\\)?\\|IN\\(?:TF?\\)?\\|LI\\(?:ST?\\)?\\|NT\\(?:ER?\\)?\\|QN\\|RIG?\\|SG\\(?:EN?\\)?\\)\\|FACT?\\|G\\(?:LOC?\\|OM\\(?:GA?\\)?\\|ROW?\\)\\|H\\(?:ECK?\\|KM\\(?:SH?\\)?\\)\\|I\\(?:NT\\|RC\\(?:LE?\\)?\\|SOL?\\)\\|L\\(?:O\\(?:CAL?\\|[CG]\\)\\|RM\\(?:S\\(?:H\\(?:LN?\\)?\\)?\\)?\\)\\|M\\(?:A\\(?:CEL?\\|TR\\(?:IX?\\)?\\|[CT]\\)\\|D\\(?:ELE?\\|OM\\(?:E\\(?:GA?\\)?\\)?\\|[EO]\\)\\|ED\\(?:IT?\\)?\\|GRP?\\|LI\\(?:ST?\\)?\\|MOD?\\|OM\\(?:E\\(?:GA?\\)?\\)?\\|PL\\(?:OT?\\)?\\|RO\\(?:T\\(?:A\\(?:TE?\\)?\\)?\\)?\\|S\\(?:EL\\|FI\\(?:LE?\\)?\\|OPT?\\|[EFO]\\)\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|N\\(?:CH\\(?:E\\(?:CK?\\)?\\)?\\|KM\\(?:OD?\\)?\\|TR\\|VT\\(?:OL?\\)?\\)\\|O\\(?:M\\(?:BI\\(?:NE?\\)?\\|PR\\(?:E\\(?:SS?\\)?\\)?\\|[BP]\\)\\|N\\(?:JUG?\\|[4EJ]\\)\\|RI\\(?:O\\(?:L\\(?:IS?\\)?\\)?\\)?\\|UP\\(?:LE?\\)?\\|VAL?\\)\\|P\\(?:CYC?\\|DE\\(?:LE?\\)?\\|IN\\(?:TF?\\)?\\|L\\(?:GEN?\\|IST?\\|[GI]\\)\\|ME\\(?:R\\(?:GE?\\)?\\)?\\|NG\\(?:EN?\\)?\\|SG\\(?:EN?\\)?\\)\\|RPL\\(?:IM?\\)?\\|S\\(?:CIR?\\|DE\\(?:LE?\\)?\\|KP\\|LI\\(?:ST?\\)?\\|WP\\(?:LA?\\)?\\|YS\\)\\|U\\(?:RR\\(?:2D?\\)?\\|TC\\(?:O\\(?:N\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\)?\\)?\\)\\|VAR\\|Y\\(?:C\\(?:CA\\(?:LC?\\)?\\|F\\(?:IL\\(?:ES?\\)?\\|REQ?\\|[IR]\\)\\|LIC?\\|OPT?\\|PH\\(?:A\\(?:SE?\\)?\\)?\\|SP\\(?:EC?\\)?\\|[CLOPS]\\)\\|L\\(?:IND?\\|[45I]\\)\\)\\|Z\\(?:DEL?\\|ME\\(?:SH?\\)?\\)\\)\\|D\\(?:A\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|MO\\(?:R\\(?:PH?\\)?\\)?\\|TA\\(?:D\\(?:EF?\\)?\\)?\\)\\|C\\(?:GO\\(?:MG?\\)?\\|UM\\|VS\\(?:WP?\\)?\\)\\|D\\(?:AS\\(?:P\\(?:EC?\\)?\\)?\\|ELE?\\|OP\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)\\|E\\(?:ACT?\\|FI\\(?:NE?\\)?\\|L\\(?:ETE?\\|TIM?\\|[ET]\\)\\|MO\\(?:R\\(?:PH?\\)?\\)?\\|RIV?\\|S\\(?:IZE?\\|OL\\|[IO]\\)\\|TAB?\\)\\|F\\(?:LX\\|SW\\(?:A\\(?:VE?\\)?\\)?\\)\\|I\\(?:GIT?\\|SP\\(?:L\\(?:AY?\\)?\\)?\\)\\|J\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\)\\|K\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\)\\|L\\(?:DE\\(?:LE?\\)?\\|IST?\\|LI\\(?:ST?\\)?\\)\\|M\\(?:OVE?\\|P\\(?:EXT?\\|OP\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\|RAT?\\|STR?\\|[EORS]\\)\\)\\|NSOL?\\|O\\(?:FS\\(?:EL?\\)?\\|ME\\(?:GA?\\)?\\)\\|S\\(?:CA\\(?:LE?\\)?\\|ET\\|PO\\(?:P\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)?\\|U\\(?:RF\\|[MR]\\)\\|Y[MS]\\)\\|TRAN?\\|UMP\\|V\\(?:AL\\|MO\\(?:R\\(?:PH?\\)?\\)?\\)\\|YNO\\(?:PT?\\)?\\)\\|E\\(?:ALI\\(?:VE?\\)?\\|D\\(?:A\\(?:DA\\(?:PT?\\)?\\|LE\\|SMP?\\|[DLS]\\)\\|B\\(?:OU\\(?:ND?\\)?\\|VIS?\\|[OVX]\\)\\|C\\(?:AD\\(?:A\\(?:PT?\\)?\\)?\\|GEN?\\|LI\\(?:ST?\\)?\\|MO\\(?:RE?\\)?\\|NS\\(?:TR?\\)?\\|ON\\(?:T\\(?:A\\(?:CT?\\)?\\)?\\)?\\|PU\\|RB\\|SC\\|TS\\|UR\\(?:VE?\\)?\\|[AGL-PR-U]\\)\\|D\\(?:AMP?\\|BL\\|RE\\(?:L\\(?:AX?\\)?\\)?\\|UMP?\\|[ABCRU]\\)\\|E\\(?:LE\\|NE\\(?:R\\(?:GY?\\)?\\)?\\|[LN]\\)\\|FP\\(?:L\\(?:OT?\\)?\\)?\\|GC\\(?:A\\(?:LE?\\)?\\)?\\|H\\(?:GLS?\\|IST?\\|TI\\(?:ME?\\)?\\|[GIT]\\)\\|I\\(?:NT\\|PA\\(?:RT?\\)?\\|[NPS]\\)\\|L\\(?:CS\\|OAD?\\|[CO]\\)\\|MP\\|N\\(?:DT\\(?:SD?\\)?\\|ROT?\\|[BDR]\\)\\|O\\(?:[PU]T\\|[PU]\\)\\|P\\(?:ART?\\|VEL?\\|[ACLV]\\)\\|R\\(?:EAD?\\|ST\\|UN\\|[CDEISU]\\)\\|S\\(?:HE\\(?:LL?\\)?\\|OLV?\\|TA\\(?:RT?\\)?\\|[HOPT]\\)\\|T\\(?:ERM?\\|[EP]\\)\\|VEL?\\|W\\(?:ELD?\\|RI\\(?:TE?\\)?\\|[ER]\\)\\)\\|EXT\\(?:R\\(?:U\\(?:DE?\\)?\\)?\\)?\\|GEN\\|IN\\(?:FIN?\\|TF\\|[FT]\\)\\|KILL?\\|L\\(?:BOW?\\|EM\\|IST?\\)\\|M\\(?:A\\(?:GE\\(?:RR?\\)?\\|TW\\(?:R\\(?:I\\(?:TE?\\)?\\)?\\)?\\|[GT]\\)\\|FT\\|I[DS]\\|O\\(?:DIF?\\|RE\\|[DR]\\)\\|SYM?\\|TG\\(?:EN?\\)?\\|UN\\(?:IT?\\)?\\)\\|N\\(?:DR\\(?:E\\(?:L\\(?:E\\(?:A\\(?:SE?\\)?\\)?\\)?\\)?\\)?\\|ER\\(?:S\\(?:OL?\\)?\\)?\\|GEN?\\|ORM?\\|SYM?\\)\\|ORI\\(?:E\\(?:NT?\\)?\\)?\\|PLOT?\\|QSLV?\\|R\\(?:ASE?\\|E\\(?:AD\\|FI\\(?:NE?\\)?\\|INF?\\|SX\\|[AFIS]\\)\\|NO\\(?:RM?\\)?\\|RA\\(?:NG?\\)?\\)\\|S\\(?:CH\\(?:E\\(?:CK?\\)?\\)?\\|EL\\|IZE?\\|L[ALNV]\\|O\\(?:RT\\|[LR]\\)\\|SO\\(?:LV?\\)?\\|TIF?\\|URF?\\|Y[MS]\\)\\|T\\(?:AB\\(?:LE?\\)?\\|C\\(?:HG\\|ON\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\|[HO]\\)\\|DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|YPE?\\)\\|USO\\(?:RT?\\)?\\|WRI\\(?:TE?\\)?\\|X\\(?:P\\(?:A\\(?:ND\\|SS\\|[NS]\\)\\|RO\\(?:F\\(?:I\\(?:LE?\\)?\\)?\\)?\\|SOL?\\|[RS]\\)\\|T\\(?:OPT?\\|REM?\\|[OR]\\)\\|UN\\(?:IT?\\)?\\)\\)\\|F\\(?:ATI\\(?:G\\(?:UE?\\)?\\)?\\|C\\(?:CH\\(?:E\\(?:CK?\\)?\\)?\\|DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|TYP?\\|UM\\)\\|DELE?\\|E\\(?:BO\\(?:DY?\\)?\\|CO\\(?:NS?\\)?\\|FOR?\\|LI\\(?:ST?\\)?\\|SU\\(?:RF?\\)?\\)\\|I\\(?:L\\(?:E\\(?:AUX[23]\\|D\\(?:I\\(?:SP?\\)?\\)?\\)\\|LD\\(?:A\\(?:TA?\\)?\\)?\\|[EL]\\)\\|NI\\(?:SH?\\)?\\|TEM?\\)\\|J\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\)\\|K\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\)\\|L\\(?:IST?\\|LI\\(?:ST?\\)?\\|ST\\|U\\(?:RE\\(?:AD?\\)?\\|XV\\|[RX]\\)\\)\\|MAG\\(?:BC\\|SUM?\\|[BS]\\)\\|OR\\(?:2D\\|CE\\|[2CM]\\)\\|PLI\\(?:ST?\\)?\\|R\\(?:EQ\\|QS\\(?:CL?\\)?\\)\\|S\\(?:CA\\(?:LE?\\)?\\|DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|NO\\(?:DE?\\)?\\|PL\\(?:OT?\\)?\\|S\\(?:ECT?\\|PA\\(?:RM?\\)?\\|[EP]\\)\\|UM\\)\\|T\\(?:CA\\(?:LC?\\)?\\|RAN?\\|SI\\(?:ZE?\\)?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\|YPE?\\)\\|VME\\(?:SH?\\)?\\)\\|G\\(?:A\\(?:PF\\|UGE?\\)\\|C\\(?:DEF?\\|GEN?\\)\\|E\\(?:NO\\(?:PT?\\)?\\|OM\\(?:E\\(?:T\\(?:RY?\\)?\\)?\\)?\\)\\|M\\(?:AT\\(?:R\\(?:IX?\\)?\\)?\\|FA\\(?:CE?\\)?\\)\\|P\\(?:DE\\(?:LE?\\)?\\|L\\(?:IST?\\|OT\\|[IO]\\)\\)\\|S\\(?:BD\\(?:A\\(?:TA?\\)?\\)?\\|GD\\(?:A\\(?:TA?\\)?\\)?\\|LI\\(?:ST?\\)?\\|SOL?\\|UM\\)\\)\\|H\\(?:ARF\\(?:RQ?\\)?\\|BMAT?\\|E\\(?:LP\\(?:D\\(?:I\\(?:SP?\\)?\\)?\\)?\\|MI\\(?:O\\(?:PT?\\)?\\)?\\)\\|F\\(?:ANG?\\|SYM?\\)\\|MAG\\(?:S\\(?:O\\(?:LV?\\)?\\)?\\)?\\|P\\(?:GL\\|T\\(?:CR\\(?:E\\(?:A\\(?:TE?\\)?\\)?\\)?\\|DE\\(?:L\\(?:E\\(?:TE?\\)?\\)?\\)?\\|[CD]\\)\\)\\|R\\(?:CP\\(?:LX?\\)?\\|EXP?\\|O\\(?:CE\\(?:AN?\\)?\\|[PU]T\\|[CPU]\\)\\)\\)\\|I\\(?:C\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\)\\|GES\\(?:IN\\|OUT?\\|[IO]\\)\\|M\\(?:AG\\(?:IN?\\)?\\|ESH?\\|MED?\\|PD\\)\\|N\\(?:IS\\(?:T\\(?:A\\(?:TE?\\)?\\)?\\)?\\|R\\(?:ES\\|TIA?\\|[ET]\\)\\|T\\(?:SRF?\\|[1S]\\)\\)\\|OPTN?\\|RL\\(?:IST?\\|[FI]\\)\\)\\|J\\(?:PEG\\|SOL\\)\\|K\\(?:ATT\\|BETW?\\|C\\(?:ALC?\\|EN\\(?:T\\(?:ER?\\)?\\)?\\|LE\\(?:AR?\\)?\\)\\|D\\(?:ELE?\\|IST?\\)\\|E\\(?:EP\\|SI\\(?:ZE?\\)?\\|Y\\(?:OPT?\\|PTS?\\|[OPW]\\)\\)\\|FILL?\\|GEN\\|LIST?\\|M\\(?:ESH?\\|O\\(?:DIF?\\|VE\\|[DV]\\)\\)\\|NODE?\\|P\\(?:LOT?\\|SC\\(?:A\\(?:LE?\\)?\\)?\\)\\|REF\\(?:I\\(?:NE?\\)?\\)?\\|S\\(?:C\\(?:ALE?\\|ON\\|[AO]\\)\\|EL\\|L[LN]\\|\\(?:YM\\|[UY]\\)M\\)\\|TRAN?\\|USE\\|WP\\(?:AVE?\\|LAN?\\|[AL]\\)\\)\\|L\\(?:2\\(?:ANG?\\|TAN?\\)\\|A\\(?:NG\\|R\\(?:EA\\|GE\\|[CEG]\\)\\|TT\\|Y\\(?:ER\\(?:P\\(?:26?\\)?\\)?\\|LI\\(?:ST?\\)?\\|PL\\(?:OT?\\)?\\|[LP]\\)\\)\\|C\\(?:A\\(?:BS\\|SE\\|[BS]\\)\\|CA\\(?:LC\\|[LT]\\)\\|DEF?\\|F\\(?:ACT?\\|ILE?\\|[AI]\\)\\|LE\\(?:AR?\\)?\\|O\\(?:MB\\|PER?\\|[MP]\\)\\|S\\(?:EL\\|UM\\|[ELU]\\)\\|WR\\(?:I\\(?:TE?\\)?\\)?\\|ZE\\(?:RO?\\)?\\)\\|D\\(?:ELE?\\|IV\\|R\\(?:AG\\|EAD?\\|[AE]\\)\\)\\|E\\(?:SI\\(?:ZE?\\)?\\|XT\\(?:ND?\\)?\\)\\|F\\(?:IL\\(?:LT?\\)?\\|SU\\(?:RF?\\)?\\)\\|G\\(?:EN\\|LUE?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|I\\(?:N\\(?:ES\\|[AELPV]\\)\\|ST\\)\\|LIST?\\|M\\(?:AT\\(?:R\\(?:IX?\\)?\\)?\\|ESH?\\)\\|NSR\\(?:CH?\\)?\\|O\\(?:CAL?\\|VL\\(?:AP?\\)?\\)\\|P\\(?:LOT?\\|TN\\)\\|R\\(?:E\\(?:FI\\(?:NE?\\)?\\|VE\\(?:R\\(?:SE?\\)?\\)?\\|[FV]\\)\\|OT\\(?:AT?\\)?\\)\\|S\\(?:B[ALVW]\\|CL\\(?:E\\(?:AR?\\)?\\)?\\|DE\\(?:LE?\\)?\\|EL\\|L[AK]\\|OP\\(?:ER?\\)?\\|RE\\(?:AD?\\)?\\|S\\(?:CA\\(?:LE?\\)?\\|OL\\(?:VE?\\)?\\|[CO]\\)\\|TR\\|UM\\|WR\\(?:I\\(?:TE?\\)?\\)?\\|YMM?\\)\\|T\\(?:AN\\|RAN?\\)\\|UMPM?\\|VSC\\(?:A\\(?:LE?\\)?\\)?\\|WPL\\(?:AN?\\)?\\)\\|M\\(?:A\\(?:DA\\(?:PT?\\)?\\|G\\(?:OPT?\\|SO\\(?:LV?\\)?\\|[OS]\\)\\|P\\(?:2D\\(?:T\\(?:O\\(?:3D?\\)?\\)?\\)?\\|SO\\(?:L\\(?:VE?\\)?\\)?\\|VAR?\\|[2SV]\\)\\|ST\\(?:ER?\\)?\\|TER?\\)\\|CHE\\(?:CK?\\)?\\|D\\(?:AMP?\\|ELE?\\|PL\\(?:OT?\\)?\\)\\|E\\(?:MM\\|SH\\(?:I\\(?:NG?\\)?\\)?\\)\\|FANA\\(?:L\\(?:Y\\(?:S\\(?:IS?\\)?\\)?\\)?\\)?\\)\\|~\\(?:CAT\\(?:5IN?\\|IA\\(?:IN?\\)?\\|[5I]\\)\\|P\\(?:AR\\(?:A\\(?:IN?\\)?\\)?\\|RO\\(?:E\\(?:IN?\\)?\\)?\\)\\|SAT\\(?:IN?\\)?\\|UGIN?\\)\\)"
"Ansys keyword name regexp 2c")

(defconst ansys-command-regexp-1
"\\(?:\\*\\(?:A\\(?:BBR\\|FUN\\|SK\\|XPY\\)\\|C\\(?:F\\(?:CLOS\\|OPEN\\|WRITE\\)\\|OMP\\|\\(?:REAT\\|YCL\\)E\\)\\|D\\(?:EL\\|IM\\|MAT\\|O\\(?:T\\|WHILE\\)?\\)\\|E\\(?:IGEN\\|LSE\\(?:IF\\)?\\|ND\\(?:DO\\|IF\\)?\\|X\\(?:\\(?:I\\|POR\\)T\\)\\)\\|F\\(?:FT\\|REE\\)\\|G\\(?:ET\\|O\\)\\|I\\(?:F\\|NIT\\|TENGINE\\)\\|L\\(?:IST\\|S\\(?:BAC\\|DUMP\\|ENGINE\\|FACTOR\\|RESTORE\\)\\)\\|M\\(?:F\\(?:OURI\\|UN\\)\\|OPER\\|SG\\|ULT\\|WRITE\\)\\|NRM\\|PRINT\\|RE\\(?:PEAT\\|TURN\\)\\|S\\(?:ET\\|MAT\\|READ\\|TATUS\\)\\|T\\(?:AXIS\\|OPER\\|READ\\)\\|U\\(?:LIB\\|SE\\)\\|V\\(?:ABS\\|C\\(?:OL\\|UM\\)\\|E\\(?:C\\|DIT\\)\\|F\\(?:ACT\\|ILL\\|UN\\)\\|GET\\|ITRP\\|LEN\\|MASK\\|OPER\\|P\\(?:\\(?:LO\\|U\\)T\\)\\|READ\\|S\\(?:CFUN\\|TAT\\)\\|WRITE\\)\\|WRK\\)\\|/\\(?:A\\(?:N\\(?:3D\\|FILE\\|GLE\\|NOT\\|UM\\)\\|SSIGN\\|U\\(?:TO\\|X\\(?:1[25]\\|[23]\\)\\)\\|XLAB\\)\\|BATCH\\|C\\(?:FORMAT\\|L\\(?:ABEL\\|EAR\\|OG\\)\\|MAP\\|O\\(?:LOR\\|M\\|N\\(?:FIG\\|TOUR\\)\\|PY\\)\\|PLANE\\|TYPE\\|VAL\\|\\(?:W\\|YCEXPAN\\)D\\)\\|D\\(?:E\\(?:LETE\\|V\\(?:DISP\\|ICE\\)\\)\\|FLAB\\|I\\(?:RECTORY\\|ST\\)\\|SCALE\\|V3D\\)\\|E\\(?:DGE\\|FACET\\|OF\\|RASE\\|SHAPE\\|X\\(?:IT\\|PAND\\)\\)\\|F\\(?:ACET\\|DELE\\|ILNAME\\|O\\(?:CUS\\|RMAT\\)\\)\\|G\\(?:C\\(?:MD\\|OLUMN\\)\\|F\\(?:ILE\\|ORMAT\\)\\|LINE\\|MARKER\\|O\\(?:LIST\\|PR\\)?\\|R\\(?:APHICS\\|ESUME\\|ID\\|OPT\\|TYP\\)\\|S\\(?:AVE\\|T\\)\\|T\\(?:HK\\|YPE\\)\\)\\|H\\(?:BC\\|EADER\\)\\|I\\(?:C\\(?:LWID\\|SCALE\\)\\|MAGE\\|N\\(?:PUT\\|QUIRE\\)\\)\\|L\\(?:ARC\\|I\\(?:GHT\\|NE\\)\\|S\\(?:PEC\\|YMBOL\\)\\)\\|M\\(?:A\\(?:IL\\|P\\)\\|ENU\\|KDIR\\|PLIB\\|REP\\|START\\)\\|N\\(?:ERR\\|O\\(?:ERASE\\|LIST\\|PR\\|RMAL\\)\\|UMBER\\)\\|OUTPUT\\|P\\(?:AGE\\|B[CF]\\|C\\(?:IRCLE\\|OPY\\)\\|DS\\|LOPTS\\|M\\(?:ACRO\\|ORE\\)\\|NUM\\|O\\(?:LYGON\\|ST\\(?:1\\|26\\)\\)\\|REP7\\|S\\(?:EARCH\\|F\\|PEC\\|TATUS\\|YMB\\)\\|WEDGE\\)\\|QUIT\\|R\\(?:ATIO\\|E\\(?:NAME\\|\\(?:PLO\\|SE\\)T\\)\\|GB\\|MDIR\\)\\|S\\(?:E\\(?:CLIB\\|G\\)\\|H\\(?:ADE\\|OW\\(?:DISP\\)?\\|RINK\\)\\|MBC\\|OLU\\|SCALE\\|T\\(?:ATUS\\|ITLE\\)\\|Y[PS]\\)\\|T\\(?:EE\\|ITLE\\|LABEL\\|R\\(?:IAD\\|LCY\\)\\|SPEC\\|\\(?:XTR\\|YP\\)E\\)\\|U\\(?:CMD\\|DOC\\|IS?\\|NITS\\|SER\\)\\|V\\(?:CONE\\|IEW\\|SCALE\\|UP\\)\\|W\\(?:AIT\\|INDOW\\)\\|X\\(?:FRM\\|RANGE\\)\\|YRANGE\\|ZOOM\\)\\|A\\(?:A\\(?:DD\\|TT\\)\\|B\\(?:B\\(?:RES\\|SAV\\)\\|EXTRACT\\|S\\)\\|C\\(?:C\\(?:AT\\|OPTION\\)\\|EL\\|LEAR\\)\\|D\\(?:A\\(?:MS\\|PT\\)\\|D\\(?:AM\\)?\\|ELE\\|GL\\|RAG\\)\\|ESIZE\\|F\\(?:ILLT\\|LIST\\|SURF\\)\\|G\\(?:EN\\|LUE\\)\\|IN[APV]\\|L\\(?:IST\\|LSEL\\|PHAD\\)?\\|M\\(?:AP\\|ESH\\)\\|N\\(?:C\\(?:NTR\\|UT\\|YC\\)\\|D\\(?:ATA\\|SCL\\|YNA\\)\\|FLOW\\|HARM\\|I\\(?:M\\|SOS\\)\\|M\\(?:ODE\\|RES\\)\\|ORM\\|PRES\\|S\\(?:OL\\|TOA\\(?:QWA\\|SAS\\)\\)\\|T\\(?:\\(?:IM\\|YP\\)E\\)\\)\\|O\\(?:FFST\\|VLAP\\)\\|P\\(?:LOT\\|PEND\\|TN\\)\\|R\\(?:C\\(?:LEN\\|TRM\\)\\|E\\(?:AS\\|FINE\\|MESH\\|VERSE\\)\\|OTAT\\|S\\(?:CALE\\|YM\\)\\)\\|S\\(?:B[ALVW]\\|CRES\\|EL\\|IFILE\\|KIN\\|L[LV]\\|OL\\|U[BM]\\)\\|T\\(?:AN\\|RAN\\|YPE\\)\\|UTOTS\\|V\\(?:PRIN\\|RES\\)\\|WAVE\\)\\|B\\(?:CSOPTION\\|ETAD\\|F\\(?:A\\(?:DELE\\|LIST\\)\\|CUM\\|DELE\\|E\\(?:CUM\\|DELE\\|LIST\\|SCAL\\)\\|INT\\|K\\(?:DELE\\|LIST\\)\\|L\\(?:DELE\\|L?IST\\)\\|SCALE\\|TRAN\\|UNIF\\|V\\(?:DELE\\|LIST\\)\\|[AEKLV]\\)?\\|IO\\(?:\\(?:OP\\)?T\\)\\|L\\(?:C[45]\\|OCK\\)\\|O\\(?:OL\\|PTN\\)\\|S\\(?:AX\\|M[12D]\\|PLIN\\|S[12]\\|T[EQ]\\)\\|TOL\\|UCOPT\\)\\|C\\(?:A\\(?:LC\\|MPBELL\\)\\|B\\(?:DOF\\|M[DX]\\|T\\(?:E\\|MP\\)\\)\\|D\\(?:OPT\\|READ\\|WRITE\\)\\|E\\(?:C\\(?:HECK\\|MOD\\|YC\\)\\|DELE\\|INTF\\|LIST\\|NTER\\|QN\\|RIG\\|SGEN\\)\\|FACT\\|G\\(?:LOC\\|OMGA\\|ROW\\)\\|H\\(?:ECK\\|KMSH\\)\\|I\\(?:NT\\|RCLE\\|SOL\\)\\|L\\(?:O\\(?:CAL\\|G\\)\\|RMSHLN\\)\\|M\\(?:A\\(?:CEL\\|TRIX\\)\\|D\\(?:ELE\\|OMEGA\\)\\|EDIT\\|GRP\\|LIST\\|MOD\\|OMEGA\\|PLOT\\|ROTATE\\|S\\(?:EL\\|FILE\\|OPT\\)\\|WRITE\\)\\|N\\(?:CHECK\\|KMOD\\|TR\\|VTOL\\)\\|O\\(?:M\\(?:BINE\\|PRESS\\)\\|N\\(?:JUG\\|[4E]\\)\\|RIOLIS\\|UPLE\\|VAL\\)\\|P\\(?:CYC\\|DELE\\|INTF\\|L\\(?:GEN\\|IST\\)\\|MERGE\\|[NS]GEN\\)\\|QC\\|RPLIM\\|S\\(?:CIR\\|DELE\\|KP\\|LIST\\|WPLA\\|YS\\)\\|U\\(?:RR2D\\|TCONTROL\\)\\|VAR\\|Y\\(?:C\\(?:CALC\\|F\\(?:ILES\\|REQ\\)\\|LIC\\|OPT\\|PHASE\\|SPEC\\)\\|L\\(?:IND\\|[45]\\)\\)\\|Z\\(?:DEL\\|MESH\\)\\|[EMPS]\\)\\|D\\(?:A\\(?:DELE\\|LIST\\|MORPH\\|TA\\(?:DEF\\)?\\)\\|C\\(?:GOMG\\|UM\\|VSWP\\)\\|D\\(?:ASPEC\\|ELE\\|OPTION\\)\\|E\\(?:ACT\\|FINE\\|L\\(?:ETE\\|TIM\\)\\|MORPH\\|RIV\\|S\\(?:IZE\\|OL\\)\\|TAB\\)\\|F\\(?:LX\\|SWAVE\\)\\|I\\(?:G\\(?:IT\\)?\\|SPLAY\\)\\|J\\(?:DELE\\|LIST\\)\\|K\\(?:DELE\\|LIST\\)\\|L\\(?:DELE\\|L?IST\\)\\|M\\(?:OVE\\|P\\(?:EXT\\|OPTION\\|RAT\\|STR\\)\\)\\|NSOL\\|O\\(?:F\\(?:SEL\\)?\\|MEGA\\)\\|S\\(?:CALE\\|ET\\|POPTION\\|U\\(?:M\\|RF\\)\\|Y[MS]\\)\\|TRAN\\|UMP\\|V\\(?:AL\\|MORPH\\)\\|YNOPT\\|[AJKL]\\)\\|E\\(?:ALIVE\\|D\\(?:A\\(?:DAPT\\|LE\\|SMP\\)\\|B\\(?:OUND\\|VIS\\|X\\)\\|C\\(?:ADAPT\\|GEN\\|LIST\\|MORE\\|NSTR\\|ONTACT\\|PU\\|RB\\|SC\\|TS\\|URVE\\)\\|D\\(?:AMP\\|BL\\|C\\|RELAX\\|UMP\\)\\|E\\(?:LE\\|NERGY\\)\\|FPLOT\\|GCALE\\|H\\(?:GLS\\|IST\\|TIME\\)\\|I\\(?:NT\\|PART\\|S\\)\\|L\\(?:CS\\|OAD\\)\\|MP\\|N\\(?:B\\|DTSD\\|ROT\\)\\|O\\(?:[PU]T\\)\\|P\\(?:ART\\|VEL\\|[CL]\\)\\|R\\(?:EAD\\|ST\\|UN\\|[CDI]\\)\\|S\\(?:HELL\\|OLV\\|P\\|TART\\)\\|T\\(?:ERM\\|P\\)\\|VEL\\|W\\(?:ELD\\|RITE\\)\\)\\|EXTRUDE\\|GEN\\|IN\\(?:FIN\\|TF\\)\\|KILL\\|L\\(?:BOW\\|EM\\|IST\\)\\|M\\(?:A\\(?:GERR\\|TWRITE\\)\\|FT?\\|I[DS]\\|O\\(?:DIF\\|RE\\)\\|SYM\\|TGEN\\|UNIT\\)\\|N\\(?:DRELEASE\\|ERSOL\\|GEN\\|\\(?:OR\\|SY\\)M\\)\\|ORIENT\\|PLOT\\|QSLV\\|R\\(?:ASE\\|E\\(?:AD\\|FINE\\|INF\\|SX\\)\\|NORM\\|RANG\\)\\|S\\(?:CHECK\\|EL\\|IZE\\|L[ALNV]\\|O\\(?:L\\|RT\\)\\|SOLV\\|TIF\\|URF\\|Y[MS]\\)\\|T\\(?:ABLE\\|C\\(?:HG\\|ONTROL\\)\\|DELE\\|LIST\\|YPE\\)\\|USORT\\|WRITE\\|X\\(?:P\\(?:A\\(?:ND\\|SS\\)\\|ROFILE\\|SOL\\)?\\|T\\(?:OPT\\|REM\\)\\|UNIT\\)\\|[NT]\\)\\|F\\(?:ATIGUE\\|C\\(?:CHECK\\|DELE\\|LIST\\|TYP\\|UM\\)\\|DELE\\|E\\(?:BODY\\|CONS\\|FOR\\|LIST\\|SURF\\)\\|I\\(?:L\\(?:E\\(?:AUX[23]\\|DISP\\)\\|LDATA\\|[EL]\\)\\|NISH\\|TEM\\)\\|J\\(?:DELE\\|LIST\\)\\|K\\(?:DELE\\|LIST\\)\\|L\\(?:IST\\|LIST\\|ST\\|U\\(?:READ\\|XV\\)\\)\\|MAG\\(?:BC\\|SUM\\)\\|OR\\(?:2D\\|CE\\|M\\)\\|PLIST\\|R\\(?:EQ\\|QSCL\\)\\|S\\(?:CALE\\|DELE\\|LIST\\|NODE\\|PLOT\\|S\\(?:ECT\\|PARM\\)\\|UM\\)\\|T\\(?:CALC\\|RAN\\|\\(?:SIZ\\|WRIT\\|YP\\)E\\)\\|VMESH\\|[CEJKLPS]\\)\\|G\\(?:A\\(?:PF?\\|UGE\\)\\|C\\(?:DEF\\|GEN\\)\\|E\\(?:NOPT\\|OM\\(?:ETRY\\)?\\)\\|M\\(?:ATRIX\\|FACE\\)\\|P\\(?:DELE\\|L\\(?:\\(?:IS\\|O\\)T\\)\\)?\\|RP\\|S\\(?:BDATA\\|GDATA\\|LIST\\|SOL\\|UM\\)\\)\\|H\\(?:ARFRQ\\|BMAT\\|E\\(?:LP\\(?:DISP\\)?\\|MIOPT\\)\\|F\\(?:ANG\\|SYM\\)\\|MAGSOLV\\|P\\(?:GL\\|T\\(?:\\(?:CREA\\|DELE\\)TE\\)\\)\\|R\\(?:CPLX\\|EXP\\|O\\(?:CEAN\\|[PU]T\\)\\)\\)\\|I\\(?:C\\(?:DELE\\|LIST\\)?\\|GES\\(?:IN\\|OUT\\)\\|M\\(?:AGIN\\|ESH\\|\\(?:ME\\|P\\)D\\)\\|N\\(?:ISTATE\\|R\\(?:ES\\|TIA\\)\\|T\\(?:1\\|SRF\\)\\)\\|OPTN\\|RL\\(?:F\\|IST\\)\\)\\|J\\(?:PEG\\|SOL\\)\\|K\\(?:ATT\\|B\\(?:C\\|ETW\\)\\|C\\(?:ALC\\|\\(?:ENTE\\|LEA\\)R\\)\\|D\\(?:ELE\\|IST\\)\\|E\\(?:EP\\|SIZE\\|Y\\(?:OPT\\|PTS\\|W\\)\\)\\|FILL\\|GEN\\|L\\(?:IST\\)?\\|M\\(?:ESH\\|O\\(?:DIF\\|VE\\)\\)\\|NODE\\|P\\(?:LOT\\|SCALE\\)\\|REFINE\\|S\\(?:C\\(?:ALE\\|ON\\)\\|EL\\|L[LN]\\|\\(?:U\\|YM\\)M\\)\\|TRAN\\|USE\\|WP\\(?:AVE\\|LAN\\)\\)\\|L\\(?:2\\(?:ANG\\|TAN\\)\\|A\\(?:NG\\|R\\(?:C\\|EA\\|GE\\)\\|TT\\|Y\\(?:ER\\(?:P26\\)?\\|\\(?:LIS\\|PLO\\)T\\)\\)\\|C\\(?:A\\(?:BS\\|SE\\)\\|CA\\(?:LC\\|T\\)\\|DEF\\|F\\(?:ACT\\|ILE\\)\\|LEAR\\|O\\(?:MB\\|PER\\)\\|S\\(?:EL\\|L\\|UM\\)\\|WRITE\\|ZERO\\)\\|D\\(?:ELE\\|IV\\|R\\(?:AG\\|EAD\\)\\)\\|E\\(?:SIZE\\|XTND\\)\\|F\\(?:ILLT\\|SURF\\)\\|G\\(?:EN\\|\\(?:LU\\|WRIT\\)E\\)\\|I\\(?:N\\(?:ES\\|[AELPV]\\)\\|ST\\)\\|LIST\\|M\\(?:ATRIX\\|ESH\\)\\|NSRCH\\|O\\(?:CAL\\|VLAP\\)\\|P\\(?:LOT\\|TN\\)\\|R\\(?:E\\(?:\\(?:FIN\\|VERS\\)E\\)\\|OTAT\\)\\|S\\(?:B[ALVW]\\|CLEAR\\|DELE\\|EL\\|L[AK]\\|OPER\\|READ\\|S\\(?:\\(?:CAL\\|OLV\\)E\\)\\|TR\\|UM\\|WRITE\\|YMM\\)\\|T\\(?:R?AN\\)\\|UMPM\\|VSCALE\\|WPLAN\\)\\|M\\(?:A\\(?:DAPT\\|G\\(?:OPT\\|SOLV\\)\\|P\\(?:2DTO3D\\|SOLVE\\|VAR\\)\\|S?TER\\|[PT]\\)\\|CHECK\\|D\\(?:AMP\\|ELE\\|PLOT\\)\\|E\\(?:MM\\|SHING\\)\\|F\\(?:ANALYSIS\\|BUCKET\\|C\\(?:ALC\\|I\\|LEAR\\|MMAND\\|ONV\\)\\|DTIME\\|E\\(?:LEM\\|M\\|XTER\\)\\|F\\(?:NAME\\|R\\)\\|I\\(?:MPORT\\|N?TER\\)\\|L\\(?:COMM\\|IST\\)\\|MAP\\|O\\(?:RDER\\|UTPUT\\)\\|PSIMUL\\|R\\(?:C\\|ELAX\\|START\\)\\|S\\(?:ORDER\\|URFACE\\)\\|T\\(?:IME\\|OL\\)\\|\\(?:VOLUM\\|WRIT\\)E\\)\\|GEN\\|IDTOL\\|LIST\\|M\\(?:ASS\\|F\\)\\|O\\(?:D\\(?:CONT\\|E\\|IFY\\|MSH\\|OPT\\|SELOPTION\\)\\|NITOR\\|PT\\|RPH\\|VE\\)\\|P\\(?:AMOD\\|C\\(?:HG\\|OPY\\)\\|D\\(?:ATA\\|ELE\\|RES\\)\\|LIST\\|PLOT\\|R\\(?:EAD\\|INT\\)\\|T\\(?:EMP\\|GEN\\|RES\\)\\|WRITE\\)?\\|S\\(?:AVE\\|H\\(?:APE\\|COPY\\|KEY\\|MID\\|PATTERN\\)\\|\\(?:OLV\\|TOL\\)E\\)\\|XPAND\\)\\|N\\(?:A\\(?:NG\\|XIS\\)\\|CNV\\|D\\(?:ELE\\|IST\\|SURF\\)\\|EQIT\\|FORCE\\|GEN\\|KPT\\|L\\(?:ADAPTIVE\\|D\\(?:IAG\\|POST\\)\\|GEOM\\|HIST\\|IST\\|MESH\\|O\\(?:G\\|PT\\)\\)\\|MODIF\\|O\\(?:COLOR\\|DES\\|O\\(?:FFSET\\|RDER\\)\\|R[AL]\\)\\|P\\(?:\\(?:LO\\|RIN\\)T\\)\\|R\\(?:E\\(?:AD\\|FINE\\)\\|LSUM\\|O\\(?:\\(?:P\\|TA\\)T\\)\\|RANG\\)\\|S\\(?:CALE\\|EL\\|L[AEKLV]\\|MOOTH\\|O\\(?:L\\|RT\\)\\|TORE\\|UBST\\|VR\\|YM\\)\\|U\\(?:M\\(?:CMP\\|EXP\\|MRG\\|OFF\\|\\(?:ST\\|VA\\)R\\)\\|SORT\\)\\|W\\(?:P\\(?:AVE\\|LAN\\)\\|RITE\\)\\)\\|O\\(?:C\\(?:D\\(?:ATA\\|ELETE\\)\\|LIST\\|READ\\|\\(?:T\\(?:ABL\\|YP\\)\\|ZON\\)E\\)\\|MEGA\\|P\\(?:ERATE\\|NCONTROL\\)\\|UT\\(?:AERO\\|OPT\\|PR\\|RES\\)\\|VCHECK\\)\\|P\\(?:A\\(?:DELE\\|GET\\|PUT\\|R\\(?:ESU\\|RES\\|SAV\\|TSEL\\)\\|SAVE\\|TH\\|USE\\)\\|C\\(?:ALC\\|GOPT\\|IRC\\|ROSS\\)\\|D\\(?:ANL\\|C\\(?:DF\\|FLD\\|LR\\|MAT\\|ORR\\)\\|D\\(?:MCS\\|OEL\\)\\|E\\(?:F\\|XE\\)\\|HIST\\|INQR\\|LHS\\|METH\\|OT\\|P\\(?:INV\\|LOT\\|ROB\\)\\|R\\(?:ESU\\|OPT\\)\\|S\\(?:AVE\\|CAT\\|\\(?:EN\\|HI\\)S\\)\\|USER\\|VAR\\|WRITE\\)\\|ER\\(?:BC2D\\|TURB\\)\\|FACT\\|HYSICS\\|IVCHECK\\|L\\(?:C\\(?:AMP\\|FREQ\\|HIST\\|INT\\|PLX\\|RACK\\)\\|DISP\\|E\\(?:SOL\\|TAB\\)\\|F\\(?:2D\\|AR\\)\\|GEOM\\|LS\\|M\\(?:AP\\|C\\)\\|N\\(?:EAR\\|SOL\\)\\|O\\(?:RB\\|T\\(?:TING\\)?\\)\\|PA\\(?:GM\\|TH\\)\\|S\\(?:\\(?:EC\\)?T\\)\\|T\\(?:IME\\|RAC\\)\\|V\\(?:AR\\|ECT\\)\\|ZZ\\)\\|M\\(?:AP\\|GTRAN\\|L\\(?:OPT\\|SIZE\\)\\)\\|NGR\\|O\\(?:INT\\|LY\\|WERH\\)\\|PATH\\|R\\(?:A\\(?:NGE\\|S\\)\\|C\\(?:AMP\\|INT\\|PLX\\)\\|E\\(?:D\\|NERGY\\|RR\\|SOL\\|TAB\\)\\|FAR\\|I\\(?:NT\\|SM\\|TER\\|[2M]\\)\\|JSOL\\|N\\(?:EAR\\|LD\\|SOL\\)\\|O\\(?:D\\|RB\\)\\|PATH\\|R\\(?:FOR\\|SOL\\)\\|S\\(?:CONTROL\\|ECT\\)\\|TIME\\|V\\(?:AR\\|ECT\\)\\)\\|S\\(?:C\\(?:ONTROL\\|R\\)\\|D\\(?:COM\\|FRQ\\|GRAPH\\|RES\\|SPL\\|UNIT\\|VAL\\|WAV\\)\\|EL\\|M\\(?:AT\\|ESH\\)\\|TRES\\)\\|T\\(?:R\\|XY\\)\\|VECT\\)\\|Q\\(?:DVAL\\|RDOPT\\|SOPT\\|U\\(?:AD\\|OT\\)\\)\\|R\\(?:A\\(?:CE\\|DOPT\\|PPND\\|TE\\)\\|BE3\\|C\\(?:ON\\|YC\\)\\|DE\\(?:C\\|LE\\)\\|E\\(?:A\\(?:LVAR\\|[DL]\\)\\|CTNG\\|MESH\\|ORDER\\|S\\(?:CO\\(?:MBINE\\|NTROL\\)\\|ET\\|P\\|UME\\|VEC\\|WRITE\\)\\|XPORT\\|ZONE\\)\\|FORCE\\|I\\(?:G\\(?:ID\\|RESP\\)\\|MPORT\\)\\|LIST\\|M\\(?:A\\(?:LIST\\|NL\\|STER\\)\\|C\\(?:AP\\|LIST\\)\\|FLVEC\\|LVSCALE\\|M\\(?:LIST\\|RANGE\\|SELECT\\)\\|N\\(?:DISP\\|EVEC\\)\\|O\\(?:DIF\\|RE\\)\\|PORDER\\|R\\(?:ESUME\\|GENERATE\\|OPTIONS\\|PLOT\\|STATUS\\)\\|S\\(?:\\(?:AV\\|MPL\\)E\\)\\|USE\\|XPORT\\)\\|O\\(?:CK\\|SE\\)\\|P\\(?:OLY\\|R\\(?:4\\|ISM\\)\\|SD\\)\\|S\\(?:FIT\\|OPT\\|P\\(?:\\(?:L[IO]\\|RN\\)T\\)\\|SIMS\\|T\\(?:MAC\\|OFF\\)\\|URF\\|Y\\(?:MM\\|S\\)\\)\\|THICK\\)\\|S\\(?:A\\(?:BS\\|DD\\|LLOW\\|VE\\)\\|BC\\(?:LIST\\|TRAN\\)\\|DELETE\\|E\\(?:C\\(?:CONTROL\\|DATA\\|FUNCTION\\|JOINT\\|LOCK\\|MODIF\\|NUM\\|OFFSET\\|PLOT\\|READ\\|STOP\\|\\(?:TYP\\|WRIT\\)E\\)\\|DLIST\\|EXP\\|GEN\\|L\\(?:IST\\|M\\|TOL\\)\\|NERGY\\|OPT\\|SYMM\\|T\\(?:FGAP\\|RAN\\)\\|XP\\|[DT]\\)\\|F\\(?:A\\(?:CT\\|DELE\\|LIST\\)\\|BEAM\\|C\\(?:ALC\\|UM\\)\\|DELE\\|E\\(?:DELE\\|LIST\\)\\|FUN\\|GRAD\\|L\\(?:DELE\\|EX\\|L?IST\\)\\|SCALE\\|TRAN\\|[AEL]\\)\\|H\\(?:ELL\\|PP\\)\\|L\\(?:IST\\|OAD\\)\\|M\\(?:A\\(?:LL\\|X\\)\\|BODY\\|CONS\\|FOR\\|IN\\|OOTH\\|RTSIZE\\|SURF\\|ULT\\)\\|NOPTION\\|O\\(?:L\\(?:U\\(?:OPT\\)?\\|VE\\)\\|RT\\|URCE\\)\\|P\\(?:ACE\\|C\\(?:NOD\\|TEMP\\)\\|DAMP\\|EC\\|FREQ\\|GRAPH\\|H\\(?:ERE\\|[45]\\)\\|L\\(?:INE\\|OT\\)\\|MWRITE\\|O\\(?:INT\\|PT\\|WER\\)\\|READ\\|TOPT\\|UNIT\\|VAL\\)\\|QRT\\|RSS\\|S\\(?:BT\\|LN\\|MT\\|P[ABDEM]\\|TATE\\|UM\\)\\|T\\(?:A\\(?:BILIZE\\|\\(?:OP\\)?T\\)\\|EF\\|ORE\\)\\|U\\(?:B\\(?:\\(?:OP\\|SE\\)T\\)\\|C\\(?:ALC\\|R\\)\\|DEL\\|EVAL\\|GET\\|M\\(?:AP\\|TYPE\\)\\|P[LR]\\|RESU\\|S\\(?:AVE\\|EL\\)\\|VECT\\)\\|V\\(?:PLOT\\|TYP\\)\\|W\\(?:ADD\\|DEL\\|GEN\\|LIST\\)\\|YNCHRO\\|[EFV]\\)\\|T\\(?:A\\(?:LLOW\\|RGET\\)\\|B\\(?:COPY\\|D\\(?:ATA\\|ELE\\)\\|EO\\|F\\(?:IELD\\|T\\)\\|IN\\|L\\(?:E\\|IST\\)\\|MODIF\\|P\\(?:\\(?:LO\\)?T\\)\\|TEMP\\)?\\|CHG\\|ERM\\|H\\(?:EXPAND\\|OPT\\)\\|I\\(?:FF\\|M\\(?:ERANGE\\|INT\\|[EP]\\)\\|NTP\\)\\|O\\(?:FFST\\|R\\(?:Q\\(?:2D\\|C2D\\|SUM\\)\\|US\\)\\)\\|R\\(?:ANS\\(?:FER\\)?\\|EF\\|NOPT\\|P\\(?:DEL\\|LIS\\|OIN\\)\\|TIME\\)\\|S\\(?:HAP\\|RES\\)\\|UNIF\\|VAR\\|YPE\\)\\|U\\(?:IMP\\|N\\(?:D\\(?:ELETE\\|O\\)\\|PAUSE\\)\\|P\\(?:COORD\\|GEOM\\)\\|SR\\(?:CAL\\|DOF\\|ELEM\\)\\)\\|V\\(?:2DOPT\\|A\\(?:DD\\|R\\(?:DEL\\|NAM\\)\\|TT\\)?\\|C\\(?:LEAR\\|ROSS\\)\\|D\\(?:DAM\\|ELE\\|GL\\|OT\\|RAG\\)\\|E\\(?:\\(?:ORIEN\\|X\\)T\\)\\|F\\(?:OPT\\|QUERY\\|SM\\)\\|G\\(?:E[NT]\\|LUE\\)\\|I\\(?:MP\\|N[PV]\\)\\|L\\(?:IST\\|SCALE\\)\\|MESH\\|O\\(?:FFST\\|LUMES\\|VLAP\\)\\|P\\(?:LOT\\|TN\\|UT\\)\\|ROTAT\\|S\\(?:B[AVW]\\|EL\\|LA\\|UM\\|WEEP\\|YMM\\)\\|T\\(?:RAN\\|YPE\\)\\)\\|W\\(?:AVES\\|ERASE\\|FRONT\\|M\\(?:ID\\|ORE\\)\\|P\\(?:AVE\\|CSYS\\|LANE\\|OFFS\\|ROTA\\|STYL\\)\\|R\\(?:FULL\\|ITE\\(?:MAP\\)?\\)\\|S\\(?:ORT\\|PRINGS\\|TART\\)\\|TBCREATE\\)\\|X\\(?:F\\(?:DATA\\|ENRICH\\|LIST\\)\\|VAR\\)\\|~\\(?:\\(?:CAT\\(?:5\\|IA\\)\\|P\\(?:ARA\\|ROE\\)\\|SAT\\|UG\\)IN\\)\\|[AC-FK-NRV]\\)"
"Ansys full keyword name regexp")

(defconst ansys-command-regexp
"\\(?:\\*\\(?:A\\(?:BB\\|FU\\|SK\\|XP\\)\\|C\\(?:F[COW]\\|OM\\|RE\\|YC\\)\\|D\\(?:EL\\|IM\\|MA\\|O[TW]?\\)\\|E\\(?:IG\\|LSE\\(?:IF\\)?\\|ND\\(?:DO\\|IF\\)?\\|VA\\|X[IP]\\)\\|F\\(?:FT\\|RE\\)\\|G\\(?:ET\\|O\\)\\|I\\(?:F\\|NI\\|TE\\)\\|L\\(?:IS\\|S[BDEFR]\\)\\|M\\(?:F[OU]\\|O[OP]\\|SG\\|UL\\|WR\\)\\|NRM\\|PRI\\|RE[PT]\\|S\\(?:ET\\|MA\\|RE\\|TA\\)\\|T\\(?:AX\\|OP\\|RE\\)\\|U\\(?:LI\\|SE\\)\\|V\\(?:AB\\|C[OU]\\|E[CD]\\|F[AIU]\\|GE\\|IT\\|LE\\|MA\\|OP\\|P[LU]\\|RE\\|S[CT]\\|WR\\)\\|WRK\\)\\|/\\(?:A\\(?:N[3FGNU]\\|SS\\|U\\(?:T\\|X\\(?:1[25]\\|[23]\\)\\)\\|XL\\)\\|BAT\\|C\\(?:FO\\|L[AEO]\\|MA\\|O\\(?:N[FT]\\|[LMP]\\)\\|PL\\|TY\\|VA\\|WD\\|YC\\)\\|D\\(?:E\\(?:L\\|V[DI]\\)\\|FL\\|I[RS]\\|SC\\|V3\\)\\|E\\(?:DG\\|FA\\|OF\\|RA\\|SH\\|X[IP]\\)\\|F\\(?:AC\\|DE\\|IL\\|O[CR]\\)\\|G\\(?:C[MO]\\|F[IO]\\|LI\\|MA\\|O[LP]?\\|R[AEIOT]\\|S[AT]\\|T[HY]\\)\\|H\\(?:BC\\|EA\\)\\|I\\(?:C[LS]\\|MA\\|N[PQ]\\)\\|L\\(?:AR\\|I[GN]\\|S[PY]\\)\\|M\\(?:A[IP]\\|EN\\|KD\\|PL\\|RE\\|ST\\)\\|N\\(?:ER\\|O[ELPR]\\|UM\\)\\|O\\(?:[PU]T\\)\\|P\\(?:AG\\|B[CF]\\|C[IO]\\|DS\\|IC\\|LO\\|M[AEO]\\|NU\\|O\\(?:L\\|ST\\(?:1\\|26\\)\\)\\|REP7\\|S[EFPTY]\\|WE\\)\\|QUI\\|R\\(?:AT\\|E[NPS]\\|GB\\|MD\\|UNSTA?\\)\\|S\\(?:E[CG]\\|H\\(?:OWD?\\|[AR]\\)\\|MB\\|OL\\|S[CS]\\|T[AI]\\|Y[PS]\\)\\|T\\(?:EE\\|IT\\|LA\\|R[AIL]\\|SP\\|XT\\|YP\\)\\|U\\(?:CM\\|DO\\|IS?\\|NI\\|SE\\)\\|V\\(?:CO\\|ER\\|IE\\|SC\\|T\\|UP\\)\\|W\\(?:AI\\|B\\|IN\\)\\|X\\(?:FR\\|ML\\|RA\\)\\|YRA\\|ZOO\\)\\|A\\(?:A\\(?:DD\\|TT\\)\\|B\\(?:B[RS]\\|EX\\|S\\)\\|C\\(?:C[AO]\\|EL\\|LE\\)\\|D\\(?:A[MP]\\|DA?\\|EL\\|GL\\|RA\\)\\|ESI\\|F\\(?:IL\\|LI\\|SU\\)\\|G\\(?:EN\\|LU\\)\\|IN[APV]\\|L\\(?:IS\\|LS\\|P[FH]\\)?\\|M\\(?:AP\\|ES\\)\\|N\\(?:C[NUY]\\|D[ASY]\\|FL\\|HA\\|I[MS]\\|M[OR]\\|OR\\|PR\\|S\\(?:O\\|TOA[QS]\\)\\|T[IY]\\)\\|O\\(?:FF\\|VL\\)\\|P\\(?:LO\\|PE\\|TN\\)\\|R\\(?:C[LOT]\\|DE\\|E[AFMV]\\|FI\\|ME\\|OT\\|S[CPY]\\)\\|S\\(?:B[ALVW]\\|CR\\|EL\\|IF\\|KI\\|L[LNV]\\|OL\\|U[BM]\\)\\|T\\(?:AN\\|RA\\|YP\\)\\|UTO\\|V\\(?:PR\\|RE\\)\\|WAV\\)\\|B\\(?:CSO\\|ETA\\|F\\(?:A[DL]\\|CU\\|DE\\|E[CDLS]\\|IN\\|K[DL]\\|L[DIL]\\|SC\\|TR\\|UN\\|V[DL]\\|[AEKLV]\\)?\\|IO[OT]\\|L\\(?:C[45]\\|OC\\)\\|O\\(?:OL\\|PT\\)\\|S\\(?:AX\\|M[12D]\\|PL\\|S[12]\\|T[EQ]\\)\\|TOL\\|UCO\\)\\|C\\(?:A\\(?:LC\\|MP\\)\\|B\\(?:DO\\|M[DX]\\|T[EM]\\)\\|D\\(?:OP\\|RE\\|WR\\)\\|E\\(?:C[HMY]\\|DE\\|IN\\|LI\\|NT\\|QN\\|RI\\|SG\\)\\|FAC\\|G\\(?:LO\\|OM\\|RO\\)\\|H\\(?:EC\\|KM\\)\\|I\\(?:NT\\|RC\\|SO\\)\\|L\\(?:O[CG]\\|RM\\)\\|M\\(?:A[CT]\\|BL\\|D[EO]\\|ED\\|GR\\|LI\\|MO\\|OM\\|PL\\|RO\\|S[EFO]\\|WR\\)\\|N\\(?:CH\\|KM\\|TR\\|VT\\)\\|O\\(?:M[BP]\\|N[4EJ]\\|RI\\|UP\\|VA\\)\\|P\\(?:CY\\|DE\\|IN\\|L[GI]\\|ME\\|[NS]G\\)\\|QC\\|RPL\\|S\\(?:CI\\|DE\\|KP\\|LI\\|WP\\|YS\\)\\|U\\(?:RR\\|TC\\)\\|VAR\\|WZP\\|Y\\(?:C\\(?:F[IR]\\|[CLOPS]\\)\\|L[45I]\\)\\|Z\\(?:[DM]E\\)\\|[EMPS]\\)\\|D\\(?:A\\(?:DE\\|LI\\|MO\\|TAD?\\)\\|C\\(?:GO\\|UM\\|VS\\)\\|D\\(?:AS\\|EL\\|OP\\)\\|E\\(?:AC\\|FI\\|L[ET]\\|MO\\|RI\\|S[IO]\\|TA\\)\\|F\\(?:LX\\|SW\\)\\|I\\(?:GI?\\|SP\\)\\|J\\(?:DE\\|LI\\)\\|K\\(?:DE\\|LI\\)\\|L\\(?:DE\\|IS\\|LI\\)\\|M\\(?:OV\\|P[EORS]\\)\\|NSO\\|O\\(?:FS?\\|ME\\)\\|S\\(?:CA\\|ET\\|PO\\|U[MR]\\|Y[MS]\\)\\|TRA\\|UMP\\|V\\(?:AL\\|MO\\)\\|YNO\\|[AJKL]\\)\\|E\\(?:AL[IL]\\|BLO\\|D\\(?:A[DLS]\\|B[OVX]\\|C[AGL-PR-U]\\|D[ABCRU]\\|E[LN]\\|FP\\|GC\\|H[GIT]\\|I[NPS]\\|L[CO]\\|MP\\|N[BDR]\\|O[PU]\\|P[ACLV]\\|R[CDEISU]\\|S[HOPT]\\|T[EP]\\|VE\\|W[ER]\\)\\|EXT\\|GEN\\|IN[FT]\\|KIL\\|L\\(?:BO\\|EM\\|IS\\)\\|M\\(?:A[GT]\\|FT?\\|I[DS]\\|O[DR]\\|SY\\|TG\\|UN\\)\\|N\\(?:DR\\|ER\\|GE\\|OR\\|SY\\)\\|ORI\\|PLO\\|QSL\\|R\\(?:AS\\|E[AFIS]\\|NO\\|RA\\)\\|S\\(?:CH\\|EL\\|IZ\\|L[ALNV]\\|O[LR]\\|SO\\|TI\\|UR\\|Y[MS]\\)\\|T\\(?:AB\\|C[HO]\\|DE\\|LI\\|YP\\)\\|USO\\|WRI\\|X\\(?:P\\(?:A[NS]\\|[RS]\\)?\\|T[OR]\\|UN\\)\\|[NT]\\)\\|F\\(?:ATI\\|C\\(?:CH\\|DE\\|LI\\|TY\\|UM\\)\\|DEL\\|E\\(?:BO\\|CO\\|FO\\|LI\\|SU\\)\\|I\\(?:L\\(?:E\\(?:AUX[23]\\|D\\)\\|LD\\|[EL]\\)\\|NI\\|PL\\|TE\\)\\|J\\(?:DE\\|LI\\)\\|K\\(?:DE\\|LI\\)\\|L\\(?:DA\\|I[ST]\\|LI\\|O[CT]\\|RE\\|ST\\|U[RX]\\)\\|MAG[BS]\\|OR[2CM]\\|PLI\\|R\\(?:EQ\\|QS\\)\\|S\\(?:CA\\|DE\\|LI\\|NO\\|PL\\|S[EP]\\|UM\\)\\|T\\(?:CA\\|RA\\|SI\\|WR\\|YP\\)\\|VME\\|[CEJKLPS]\\)\\|G\\(?:A\\(?:P\\(?:FI\\|[FLMOP]\\)?\\|UG\\)\\|C\\(?:[DG]E\\)\\|E\\(?:NO\\|OME?\\)\\|M\\(?:AT\\|FA\\)\\|P\\(?:DE\\|L[IO]\\)?\\|RP\\|S\\(?:BD\\|GD\\|LI\\|SO\\|UM\\)\\)\\|H\\(?:ARF\\|BMA\\|E\\(?:LPD?\\|MI\\)\\|F\\(?:A[DNR]\\|DE\\|E[IR]\\|MO\\|P\\(?:O[RW]\\|[AC]\\)\\|S[CY]\\)\\|MAG\\|P\\(?:GL\\|T[CD]\\)\\|R\\(?:CP\\|EX\\|O[CPU]\\)\\)\\|I\\(?:C\\(?:DE\\|E[DL]?\\|LI\\|VF\\)?\\|GES[IO]\\|M\\(?:AG\\|ES\\|ME\\|PD\\)\\|N\\(?:IS\\|R[ET]\\|T[1S]\\)\\|OPT\\|RL[FI]\\)\\|J\\(?:PEG\\|SOL\\)\\|K\\(?:ATT\\|B\\(?:C\\|ET\\)\\|C\\(?:AL\\|EN\\|LE\\)\\|D\\(?:EL\\|IS\\)\\|E\\(?:EP\\|SI\\|Y[OPW]\\)\\|FIL\\|GEN\\|L\\(?:IS\\)?\\|M\\(?:ES\\|O[DV]\\)\\|NOD\\|P\\(?:LO\\|SC\\)\\|REF\\|S\\(?:C[AO]\\|EL\\|L[LN]\\|[UY]M\\)\\|TRA\\|USE\\|WP[AL]\\)\\|L\\(?:2\\(?:AN\\|TA\\)\\|A\\(?:NG\\|R[CEG]\\|TT\\|Y\\(?:ERP?\\|[LP]\\)\\)\\|C\\(?:A[BS]\\|CA[LT]\\|DE\\|F[AI]\\|LE\\|O[MP]\\|S[ELU]\\|WR\\|ZE\\)\\|D\\(?:EL\\|IV\\|R[AE]\\)\\|E\\(?:SI\\|XT\\)\\|F\\(?:IL\\|SU\\)\\|G\\(?:EN\\|LU\\|WR\\)\\|I\\(?:N\\(?:ES\\|[AELPV]\\)\\|ST\\)\\|LIS\\|M\\(?:AT\\|ES\\)\\|N\\(?:CO\\|DE\\|FI\\|ME\\|S[PR]\\)\\|O\\(?:CA\\|VL\\)\\|P\\(?:LO\\|RT\\|TN\\)\\|R\\(?:E[FV]\\|OT\\)\\|S\\(?:B[ALVW]\\|CL\\|DE\\|EL\\|L[AKN]\\|OP\\|RE\\|S[CO]\\|TR\\|UM\\|WR\\|YM\\)\\|T\\(?:AN\\|RA\\)\\|UMP\\|VSC\\|WPL\\)\\|M\\(?:A\\(?:DA\\|G[OS]\\|P[2SV]\\|ST\\|TE\\|[PT]\\)\\|CHE\\|D\\(?:AM\\|[EP]L\\)\\|E\\(?:MM\\|SH\\)\\|F\\(?:AN\\|BU\\|C[AILMO]\\|DT\\|E[LMX]\\|F[NR]\\|I[MNT]\\|L[CI]\\|MA\\|O[RU]\\|PS\\|R[CES]\\|S[OU]\\|T[IO]\\|VO\\|WR\\)\\|GEN\\|IDT\\|LIS\\|M\\(?:AS\\|F\\)\\|O\\(?:D[CEIMOS]\\|NI\\|PT\\|RP\\|VE\\)\\|P\\(?:AM\\|C[HO]\\|D[AER]\\|LI\\|PL\\|R[EI]\\|T[EGR]\\|WR\\)?\\|S\\(?:A[DV]\\|CA\\|DA\\|H[ACKMP]\\|M[AEI]\\|NO\\|OL\\|PR\\|QU\\|RE\\|S[OP]\\|T[EO]\\|VA\\)\\|XPA\\)\\|N\\(?:A\\(?:LL\\|NG\\|XI\\)\\|BLO\\|CNV\\|D\\(?:EL\\|IS\\|SU\\)\\|E\\(?:LE\\|QI\\)\\|FOR\\|GEN\\|KPT\\|L\\(?:AD\\|D[IP]\\|GE\\|HI\\|IS\\|ME\\|O[GP]\\)\\|MOD\\|O\\(?:CO\\|DE\\|O[FR]\\|R[AL]\\)\\|P\\(?:LO\\|RI\\)\\|R\\(?:E[AF]\\|LS\\|O[PT]\\|RA\\)\\|S\\(?:CA\\|EL\\|L[AEKLV]\\|MO\\|O[LR]\\|TO\\|UB\\|VR\\|YM\\)\\|U\\(?:M[CEMOSV]\\|SO\\)\\|W\\(?:P[AL]\\|RI\\)\\)\\|O\\(?:C\\(?:D[AE]\\|LI\\|RE\\|T[AY]\\|ZO\\)\\|MEG\\|P\\(?:A[DN]\\|CL\\|D[AE]\\|E[QRX]\\|F[AR]\\|GR\\|KE\\|L[FGIOS]\\|MA\\|NC\\|PR\\|R[AEFGS]\\|S[AEUW]\\|TY\\|US\\|VA\\)\\|UT[AOPR]\\|VCH\\)\\|P\\(?:A\\(?:DE\\|GE\\|PU\\|R[ERST]\\|SA\\|TH\\|US\\)\\|C\\(?:AL\\|GO\\|IR\\|ON\\|RO\\)\\|D\\(?:AN\\|C[DFLMO]\\|D[MO]\\|E[FX]\\|HI\\|IN\\|LH\\|ME\\|OT\\|P[ILR]\\|R[EO]\\|S[ACEH]\\|US\\|VA\\|WR\\)\\|E\\(?:MO\\|R[BIT]\\|XC\\)\\|FAC\\|G\\(?:R[AS]\\|S[AE]\\|WR\\)\\|HYS\\|I\\(?:LE\\(?:S[ET]\\|[CDGLMR]\\)\\|[NV]C\\)\\|L\\(?:C[AFHIOPR]\\|DI\\|E[ST]\\|F[2AS]\\|GE\\|LS\\|M[AC]\\|N[ES]\\|O\\(?:TT\\|[RT]\\)\\|PA[GT]\\|S[CETY]\\|T[DILR]\\|V\\(?:ARO?\\|[EF]\\)\\|WA\\|ZZ\\)\\|M\\(?:AP\\|ET\\|GT\\|L[OS]\\|OP\\)\\|NGR\\|O\\(?:IN\\|LY\\|UT\\|WE\\)\\|P\\(?:AT\\|LO\\|RA\\)\\|R\\(?:A[NS]\\|C[AIOP]\\|E[CDNRST]\\|FA\\|I[2MNST]\\|JS\\|N[ELS]\\|O[DR]\\|PA\\|R[FS]\\|S[CEY]\\|TI\\|V\\(?:ARO?\\|E\\)\\)\\|S\\(?:C[OR]\\|D[CFGRSUVW]\\|EL\\|M[AE]\\|OL\\|TR\\)\\|T\\(?:R\\|XY\\)\\|VEC\\)\\|Q\\(?:DVA\\|FAC\\|RDO\\|SOP\\|U\\(?:AD\\|OT\\)\\)\\|R\\(?:A\\(?:CE\\|DO\\|LL\\|PP\\|TE\\)\\|BE3\\|C\\(?:ON\\|YC\\)\\|DE[CL]\\|E\\(?:A\\(?:LV\\|[DL]\\)\\|CT\\|ME\\|OR\\|S\\(?:CO[MN]\\|[EPUVW]\\)\\|XP\\|ZO\\)\\|F\\(?:IL\\|OR\\)\\|I\\(?:G[IR]\\|MP\\|TE\\)\\|LIS\\|M\\(?:A[LNS]\\|C[AL]\\|EM\\|FL\\|LV\\|M[LRS]\\|N[DE]\\|O[DR]\\|PO\\|R[EGOPS]\\|S[AM]\\|US\\|XP\\)\\|O\\(?:CK\\|SE\\)\\|P\\(?:OL\\|R[4I]\\|SD\\)\\|S\\(?:FI\\|OP\\|P\\(?:L[IO]\\|[ER]\\)\\|SI\\|T[AMO]\\|UR\\|Y[MS]\\)\\|T\\(?:HI\\|IM\\)\\|WFR\\)\\|S\\(?:A\\(?:BS\\|DD\\|LL\\|RP\\|VE\\)\\|BC[LT]\\|DEL\\|E\\(?:C[CDFJL-PRSTW]\\|DL\\|EX\\|GE\\|L[IMT]\\|NE\\|OP\\|SY\\|T[FR]\\|XP\\|[DT]\\)\\|F\\(?:A[CDL]\\|BE\\|C[AU]\\|DE\\|E[DL]\\|FU\\|GR\\|L[DEIL]\\|SC\\|TR\\|[AEL]\\)\\|H\\(?:EL\\|PP\\|SD\\)\\|L\\(?:IS\\|OA\\|[PS]P\\)\\|M\\(?:A[LX]\\|BO\\|CO\\|FO\\|IN\\|OO\\|RT\\|SU\\|UL\\)\\|NOP\\|O\\(?:L\\(?:UO\\|[CUV]\\)\\|RT\\|UR\\)\\|P\\(?:A[CDR]\\|C[NT]\\|DA\\|EC\\|F[RS]\\|GR\\|H[45E]\\|IC\\|L[IO]\\|MW\\|O[IPW]\\|RE\\|S[CW]\\|TO\\|UN\\|VA\\)\\|QRT\\|RSS\\|S\\(?:BT\\|LN\\|MT\\|P[ABDEM]\\|TA\\|UM\\)\\|T\\(?:A[BOT]\\|EF\\|OR\\)\\|U\\(?:B[OS]\\|C[AR]\\|DE\\|EV\\|GE\\|M[AT]\\|P[LR]\\|RE\\|S[AE]\\|VE\\)\\|V\\(?:PL\\|TY\\)\\|W\\(?:AD\\|DE\\|GE\\|LI\\)\\|YNC\\|[EFV]\\)\\|T\\(?:A\\(?:LL\\|RG\\)\\|B\\(?:CO\\|D[AE]\\|EO\\|F[IT]\\|IN\\|L[EI]\\|MO\\|P[LT]\\|TE\\)?\\|CHG\\|ERM\\|H\\(?:EX\\|OP\\)\\|I\\(?:FF\\|M\\(?:ER\\|[EIP]\\)\\|NT\\)\\|O\\(?:CO\\|DE\\|EX\\|F[FR]\\|GR\\|L[IO]\\|P[LR]\\|R\\(?:Q[2CS]\\|U\\)\\|ST\\|T[AY]\\|VA\\)\\|R\\(?:ANSF?\\|EF\\|NO\\|P[DLO]\\|TI\\)\\|S\\(?:HA\\|RE\\)\\|UNI\\|VAR\\|YPE\\|Z\\(?:AM\\|DE\\|EG\\)\\)\\|U\\(?:IMP\\|N\\(?:D[EO]\\|PA\\)\\|P\\(?:CO\\|GE\\)\\|SR[CDE]\\)\\|V\\(?:2DO\\|A\\(?:DD\\|R[DN]\\|TT\\)?\\|C\\(?:LE\\|RO\\|VF\\)\\|D\\(?:DA\\|EL\\|GL\\|OT\\|RA\\)\\|E\\(?:OR\\|XT\\)\\|F\\(?:OP\\|QU\\|SM\\)\\|G\\(?:E[NT]\\|LU\\)\\|I\\(?:MP\\|N[PV]\\)\\|L\\(?:IS\\|SC\\)\\|MES\\|O\\(?:FF\\|LU\\|VL\\)\\|P\\(?:LO\\|TN\\|UT\\)\\|ROT\\|S\\(?:B[AVW]\\|EL\\|LA\\|UM\\|WE\\|YM\\)\\|T\\(?:CL\\|DI\\|EV\\|FR\\|GE\\|IN\\|M[EP]\\|OP\\|PO\\|R[AEFS]\\|S[EFLT]\\|TE\\|VM\\|YP\\)\\)\\|W\\(?:AVE\\|ERA\\|FRO\\|M\\(?:ID\\|OR\\)\\|P\\(?:AV\\|CS\\|LA\\|OF\\|RO\\|ST\\)\\|R\\(?:FU\\|ITEM?\\)\\|S\\(?:OR\\|PR\\|TA\\)\\|TBC\\)\\|X\\(?:F\\(?:DA\\|EN\\|LI\\)\\|MLO\\|VARO?\\)\\|~\\(?:C\\(?:AT[5I]\\|FI\\)\\|EUI\\|P\\(?:AR\\|RO\\)\\|SAT\\|UGI\\)\\|[AC-FK-NRV]\\)"
"Ansys short keyword name regexp")

(defconst ansys-dynamic-prompt
'("*ABBR - Defines an abbreviation.
*ABBR, Abbr, String" "*AFUN - Specifies units for angular functions in parameter expressions.
*AFUN, Lab" "*ASK - Prompts the user to input a parameter value.
*ASK, Par, Query, DVAL" "*AXPY - Performs the matrix operation M2= v*M1 + w*M2.
*AXPY, vr, vi, M1, wr, wi, M2" "*CFCLOS
*CFCLOS - Closes the \"command\" file." "*CFOPEN - Opens a \"command\" file.
*CFOPEN, Fname, Ext, --, Loc" "*CFWRITE - Writes an ANSYS command (or similar string) to a \"command\" file.
*CFWRITE, Command" "*COMP - Compresses the columns of a matrix using a specified algorithm.
*COMP, Matrix, Algorithm, Threshold" "*CREATE - Opens (creates) a macro file.
*CREATE, Fname, Ext, --" "*CYCLE
*CYCLE - Bypasses commands within a do-loop." "*DEL - Deletes a parameter or parameters (GUI).
*DEL, Val1, Val2" "*DIM - Defines an array parameter and its dimensions.
*DIM, Par, Type, IMAX, JMAX, KMAX, Var1, Var2, Var3, CSYSID" "*DMAT - Creates a dense matrix.
*DMAT, Matrix, Type, Method, Val1, Val2, Val3, Val4, Val5" "*DO - Defines the beginning of a do-loop.
*DO, Par, IVAL, FVAL, INC" "*DOT - Computes the dot (or inner) product of two vectors.
*DOT, Vector1, Vector2, Par_Real, Par_Imag" "*DOWHILE - Loops repeatedly through the next *ENDDO command.
*DOWHILE, Par" "*EIGEN - Performs a modal solution with unsymmetric or damping matrices.
*EIGEN, Kmatrix, Mmatrix, Cmatrix, Evals, Evects" "*ELSE
*ELSE - Separates the final if-then-else block." "*ELSEIF - Separates an intermediate if-then-else block.
*ELSEIF, VAL1, Oper1, VAL2, Conj, VAL3, Oper2, VAL4" "*END
*END - Closes a macro file." "*ENDDO
*ENDDO - Ends a do-loop and starts the looping action." "*ENDIF
*ENDIF - Ends an if-then-else." "*EXIT
*EXIT - Exits a do-loop." "*EXPORT - Exports a matrix to a file in the specified format.
*EXPORT, Matrix, Format, Fname, Val1, Val2, Val3" "*FFT - Computes the fast Fourier transformation of a specified matrix or vector.
*FFT, Type, InputData, OutputData, DIM1, DIM2, ResultFormat" "*FREE - Deletes a matrix or a solver object and frees its memory allocation.
*FREE, Name," "*GET - Retrieves a value and stores it as a scalar parameter or part of an array parameter.
*GET, Par, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM" "*GO - Causes a specified line on the input file to be read next.
*GO, Base" "*IF - Conditionally causes commands to be read.
*IF, VAL1, Oper1, VAL2, Base1, VAL3, Oper2, VAL4, Base2" "*INIT - Initializes a vector or dense matrix.
*INIT, Name, Method, Val1, Val2, Val3" "*ITENGINE - Performs a solution using an iterative solver.
*ITENGINE, Type, EngineName, PrecondName, Matrix, RhsVector, SolVector, MaxIter, Toler" "*LIST - Displays the contents of an external, coded file.
*LIST, Fname, Ext, --" "*LSBAC - Performs the solve (forward/backward substitution) of a factorized linear system. 
*LSBAC, EngineName, RhsVector, SolVector" "*LSDUMP - Dumps a linear solver engine to a binary File.
*LSDUMP, EngineName, FileName" "*LSENGINE - Creates a linear solver engine.
*LSENGINE, Type, EngineName, Matrix, Option" "*LSFACTOR - Performs the numerical factorization of a linear solver system.
*LSFACTOR, EngineName, Option" "*LSRESTORE - Restores a linear solver engine from a binary file.
*LSRESTORE, EngineName, FileName" "*MFOURI - Calculates the coefficients for, or evaluates, a Fourier series.
*MFOURI, Oper, COEFF, MODE, ISYM, THETA, CURVE" "*MFUN - Copies or transposes an array parameter matrix.
*MFUN, ParR, Func, Par1" "*MOPER -  Performs matrix operations on array parameter matrices.
*MOPER, ParR, Par1, Oper, Par2, Par3, kDim, --, kOut, LIMIT" "*MSG - Writes an output message via the ANSYS message subroutine.
*MSG, Lab, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8" "*MULT - Performs the matrix multiplication M3 = M1(T1)*M2(T2).
*MULT, M1, T1, M2, T2, M3" "*MWRITE - Writes a matrix to a file in a formatted sequence. 
*MWRITE, ParR, Fname, Ext, --, Label, n1, n2, n3" "*NRM - Computes the norm of the specified matrix or vector.
*NRM, Name, NormType, ParR, Normalize" "*PRINT - Prints the matrix values to a file.
*PRINT, Matrix, Fname" "*REPEAT - Repeats the previous command. 
*REPEAT, NTOT, VINC1, VINC2, VINC3, VINC4, VINC5, VINC6, VINC7, VINC8, VINC9, VINC10, VINC11" "*RETURN - Returns input stream to a higher level.
*RETURN, Level" "*SET - Assigns values to user-named parameters.
*SET, Par, VALUE, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10" "*SMAT - Creates a sparse matrix.
*SMAT, Matrix, Type, Method, Val1, Val2, Val3" "*SREAD - Reads a file into a string array parameter.
*SREAD, StrArray, Fname, Ext, --, nChar, nSkip, nRead" "*STATUS - Lists the current parameters and abbreviations.
*STATUS, Par, IMIN, IMAX, JMIN, JMAX, KMIN, KMAX, LMIN, LMAX, MMIN, MMAX, KPRI" "*TAXIS - Defines table index numbers.
*TAXIS, ParmLoc, nAxis, Val1, Val2, Val3, Val4, Val5, Val6, Val7, Val8, Val9, Val10" "*TOPER - Operates on table parameters.
*TOPER, ParR, Par1, Oper, Par2, FACT1, FACT2, CON1" "*TREAD - Reads data from an external file into a table array parameter.
*TREAD, Par, Fname, Ext, --, NSKIP" "*ULIB - Identifies a macro library file.
*ULIB, Fname, Ext, --" "*USE - Executes a macro file.
*USE, Name, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7, ARG8, ARG9, AR10, AR11, AR12, AR13, AR14, AG15, AR16, AR17, AR18" "*VABS - Applies the absolute value function to array parameters.
*VABS, KABSR, KABS1, KABS2, KABS3" "*VCOL - Specifies the number of columns in matrix operations.
*VCOL, NCOL1, NCOL2" "*VCUM - Allows array parameter results to add to existing results.
*VCUM, KEY" "*VEC - Creates a vector.
*VEC, Vector, Type, Method, Val1, Val2, Val3, Val4" "*VEDIT - Allows numerical array parameters to be graphically edited.
*VEDIT, Par" "*VFACT - Applies a scale factor to array parameters.
*VFACT, FACTR, FACT1, FACT2, FACT3" "*VFILL - Fills an array parameter.
*VFILL, ParR, Func, CON1, CON2, CON3, CON4, CON5, CON6, CON7, CON8, CON9, CON10" "*VFUN - Performs a function on a single array parameter.
*VFUN, ParR, Func, Par1, CON1, CON2, CON3" "*VGET - Retrieves values and stores them into an array parameter.
*VGET, ParR, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM, KLOOP" "*VITRP - Forms an array parameter by interpolation of a table.
*VITRP, ParR, ParT, ParI, ParJ, ParK" "*VLEN - Specifies the number of rows to be used in array parameter operations.
*VLEN, NROW, NINC" "*VMASK - Specifies an array parameter as a masking vector.
*VMASK, Par" "*VOPER - Operates on two array parameters.
*VOPER, ParR, Par1, Oper, Par2, CON1, CON2" "*VPLOT - Graphs columns (vectors) of array parameters.
*VPLOT, ParX, ParY, Y2, Y3, Y4, Y5, Y6, Y7, Y8" "*VPUT -  Restores array parameter values into the ANSYS database.
*VPUT, ParR, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM, KLOOP" "*VREAD - Reads data and produces an array parameter vector or matrix.
*VREAD, ParR, Fname, Ext, --, Label, n1, n2, n3, NSKIP" "*VSCFUN - Determines properties of an array parameter.
*VSCFUN, ParR, Func, Par1" "*VSTAT
*VSTAT - Lists the current specifications for the array parameters." "*VWRITE - Writes data to a file in a formatted sequence.
*VWRITE, Par1, Par2, Par3, Par4, Par5, Par6, Par7, Par8, Par9, Par10, Par11, Par12, Par13, Par14, Par15, Par16, Par17, Par18, Par19" "*WRK - Sets the active workspace number.
*WRK, Num" "/AN3D - Specifies 3-D annotation functions
/AN3D, Kywrd, KEY" "/ANFILE - Saves or resumes an animation sequence to or from a file.
/ANFILE, LAB, Fname, Ext, --" "/ANGLE - Rotates the display about an axis.
/ANGLE, WN, THETA, Axis, KINCR" "/ANNOT - Activates graphics for annotating displays (GUI).
/ANNOT, Lab, VAL1, VAL2" "/ANUM - Specifies the annotation number, type, and hot spot (GUI).
/ANUM, NUM, TYPE, XHOT, YHOT" "/ASSIGN - Reassigns a file name to an ANSYS file identifier.
/ASSIGN, Ident, Fname, Ext, --, LGkey" "/AUTO - Resets the focus and distance specifications to \"automatically calculated.\"
/AUTO, WN" "/AUX12
/AUX12 - Enters the radiation processor." "/AUX15
/AUX15 - Enters the IGES file transfer processor." "/AUX2
/AUX2 - Enters the binary file dumping processor." "/AUX3
/AUX3 - Enters the results file editing processor." "/AXLAB - Labels the X and Y axes on graph displays.
/AXLAB, Axis, Lab" "/BATCH - Sets the program mode to \"batch.\"
/BATCH, Lab" "/CFORMAT - Controls the graphical display of alphanumeric character strings for parameters, components, assemblies, and tables.
/CFORMAT, NFIRST, NLAST" "/CLABEL - Specifies contour labeling.
/CLABEL, WN, KEY" "/CLEAR - Clears the database.
/CLEAR, Read" "/CLOG - Copies the session log file to a named file.
/CLOG, Fname, Ext, --" "/CMAP - Changes an existing or creates a new color mapping table.
/CMAP, Fname, Ext, --, Kywrd, NCNTR" "/COLOR - Specifies the color mapping for various items.
/COLOR, Lab, Clab, N1, N2, NINC" "/COM - Places a comment in the output.
/COM, Comment" "/CONFIG - Assigns values to ANSYS configuration parameters.
/CONFIG, Lab, VALUE" "/CONTOUR - Specifies the uniform contour values on stress displays.
/CONTOUR, WN, NCONT, VMIN, VINC, VMAX" "/COPY - Copies a file.
/COPY, Fname1, Ext1, --, Fname2, Ext2, --, DistKey" "/CPLANE - Specifies the cutting plane for section and capped displays.
/CPLANE, KEY" "/CTYPE - Specifies the type of contour display.
/CTYPE, KEY, DOTD, DOTS, DSHP, TLEN" "/CVAL - Specifies nonuniform contour values on stress displays.
/CVAL, WN, V1, V2, V3, V4, V5, V6, V7, V8" "/CWD - Changes the current working directory.
/CWD, DIRPATH" "/CYCEXPAND - Graphically expands displacements, stresses and strains of a cyclically symmetric model. 
/CYCEXPAND, WN, OPTION, Value1, Value2" "/DELETE - Deletes a file.
/DELETE, Fname, Ext, --, DistKey" "/DEVDISP - Controls graphics device options.
/DEVDISP, Label, KEY" "/DEVICE - Controls graphics device options.
/DEVICE, Label, KEY" "/DFLAB - Changes degree-of-freedom labels for user custom elements.
/DFLAB, DOF, DispLab, ForceLab" "/DIRECTORY - Put the file names in the current directory into a string parameter array.
/DIRECTORY, StrArray, FileName, Ext, Dir" "/DIST - Specifies the viewing distance for magnifications and perspective.
/DIST, WN, DVAL, KFACT" "/DSCALE - Sets the displacement multiplier for displacement displays.
/DSCALE, WN, DMULT" "/DV3D - Sets 3-D device option modes.
/DV3D, Lab, Key" "/EDGE - Displays only the common lines (&#8220;edges&#8221;) of an object.
/EDGE, WN, KEY, ANGLE" "/EFACET - Specifies the number of facets per element edge for PowerGraphics displays.
/EFACET, NUM" "/EOF
/EOF - Exits the file being read." "/ERASE
/ERASE - Specifies that the screen is to be erased before each display." "/ESHAPE - Displays elements with shapes determined from the real constants or section definition.
/ESHAPE, SCALE, KEY" "/EXIT - Stops the run and returns control to the system.
/EXIT, Slab, Fname, Ext, --" "/EXPAND - Allows the creation of a larger graphic display than represented by the actual finite element analysis model.
/EXPAND, Nrepeat1, Type1, Method1, DX1, DY1, DZ1, Nrepeat2, Type2, Method2, DX2, DY2, DZ2, Nrepeat3, Type3, Method3, DX3, DY3, DZ3" "/FACET - Specifies the facet representation used to form solid model displays.
/FACET, Lab" "/FDELE - Deletes a binary file after it is used.
/FDELE, Ident, Stat" "/FILNAME - Changes the Jobname for the analysis.
/FILNAME, Fname, Key" "/FOCUS - Specifies the focus point (center of the window).
/FOCUS, WN, XF, YF, ZF, KTRANS" "/FORMAT - Specifies format controls for tables.
/FORMAT, NDIGIT, Ftype, NWIDTH, DSIGNF, LINE, CHAR" "/GCMD - Controls the type of element or graph display used for the GPLOT command.
/GCMD, WN, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6, Lab7, Lab8, Lab9, Lab10, Lab11, Lab12" "/GCOLUMN - Allows the user to apply a label to a specified curve.
/GCOLUMN, CURVE, STRING" "/GFILE - Specifies the pixel resolution on Z-buffered graphics files.
/GFILE, SIZE" "/GFORMAT - Specifies the format for the graphical display of numbers.
/GFORMAT, Ftype, NWIDTH, DSIGNF" "/GLINE - Specifies the element outline style.
/GLINE, WN, STYLE" "/GMARKER - Specifies the curve marking style.
/GMARKER, CURVE, KEY, INCR" "/GO
/GO - Reactivates suppressed printout." "/GOLIST
/GOLIST - Reactivates the suppressed data input listing." "/GOPR
/GOPR - Reactivates suppressed printout." "/GRAPHICS - Defines the type of graphics display.
/GRAPHICS, Key" "/GRESUME - Sets graphics settings to the settings on a file.
/GRESUME, Fname, Ext, --" "/GRID - Selects the type of grid on graph displays.
/GRID, KEY" "/GROPT - Sets various line graph display options.
/GROPT, Lab, KEY" "/GRTYP - Selects single or multiple Y-axes graph displays.
/GRTYP, KAXIS" "/GSAVE - Saves graphics settings to a file for later use.
/GSAVE, Fname, Ext, --" "/GST - Turns Graphical Solution Tracking (GST) on or off.
/GST, Lab, Lab2" "/GTHK - Sets line thicknesses for graph lines.
/GTHK, Label, THICK" "/GTYPE - Controls the entities that the GPLOT command displays.
/GTYPE, WN, LABEL, KEY" "/HBC - Determines how boundary condition symbols are displayed in a display window.
/HBC, WN, Key" "/HEADER - Sets page and table heading print controls.
/HEADER, Header, Stitle, Idstmp, Notes, Colhed, Minmax" "/ICLWID - Scales the line width of circuit builder icons.
/ICLWID, FACTOR" "/ICSCALE - Scales the icon size for elements supported in the circuit builder.
/ICSCALE, WN, FACTOR" "/IMAGE - Allows graphics data to be captured and saved. 
/IMAGE, Label, Fname, Ext, --" "/INPUT - Switches the input file for the commands that follow.
/INPUT, Fname, Ext, Dir, LINE, LOG" "/INQUIRE - Returns system information to a parameter.
/INQUIRE, StrArray, FUNC" "/LARC - Creates annotation arcs (GUI).
/LARC, XCENTR, YCENTR, XLRAD, ANGLE1, ANGLE2" "/LIGHT - Specifies the light direction for the display window.
/LIGHT, WN, NUM, INT, XV, YV, ZV, REFL" "/LINE - Creates annotation lines (GUI).
/LINE, X1, Y1, X2, Y2" "/LSPEC - Specifies annotation line attributes (GUI).
/LSPEC, LCOLOR, LINSTL, XLNWID" "/LSYMBOL - Creates annotation symbols (GUI).
/LSYMBOL, X, Y, SYMANG, SYMTYP, SYMSIZ, KEYBMP" "/MAIL - Mails file to the specified address.
/MAIL, --, Address, Fname, Ext" "/MAP
/MAP - Enters the mapping processor." "/MENU - Activates the Graphical User Interface (GUI).
/MENU, Key" "/MKDIR - Creates a directory.
/MKDIR, Dir" "/MPLIB - Sets the default material library read and write paths.
/MPLIB, R-W_opt, PATH" "/MREP - Enables you to reissue the graphics command macro \"name\" during a replot or zoom operation.
/MREP, NAME, ARG1, ARG2, ARG3, . . . , ARG4, ARG5, ARG6, ARG7, ARG8, ARG9, ARG10, ARG11, ARG12, ARG13, ARG14, ARG15, ARG16, ARG17, ARG18" "/MSTART - Controls the initial GUI components.
/MSTART, Label, KEY" "/NERR - Limits the number of warning and error messages displayed.
/NERR, NMERR, NMABT, --, IFKEY, NUM" "/NOERASE
/NOERASE - Prevents the screen erase between displays." "/NOLIST
/NOLIST - Suppresses the data input listing." "/NOPR
/NOPR - Suppresses the expanded interpreted input data listing." "/NORMAL - Allows displaying area elements by top or bottom faces.
/NORMAL, WN, KEY" "/NUMBER - Specifies whether numbers, colors, or both are used for displays.
/NUMBER, NKEY" "/OUTPUT - Redirects text output to a file or to the screen.
/OUTPUT, Fname, Ext, --, Loc" "/PAGE - Defines the printout and screen page size.
/PAGE, ILINE, ICHAR, BLINE, BCHAR, COMMA" "/PBC - Shows boundary condition (BC) symbols and values on displays.
/PBC, Item, --, KEY, MIN, MAX, ABS" "/PBF - Shows magnitude of body force loads on displays.
/PBF, Item, --, KEY" "/PCIRCLE - Creates an annotation circle (GUI).
/PCIRCLE, XCENTR, YCENTR, XLRAD" "/PCOPY - Automatically generates hard copies for HP UNIX work stations.
/PCOPY, KEY" "/PDS
/PDS - Enters the probabilistic design system." "/PLOPTS - Controls graphics options on subsequent displays.
/PLOPTS, Label, KEY" "/PMACRO
/PMACRO - Specifies that macro contents be written to the session log file." "/PMORE - Creates an annotation polygon (GUI).
/PMORE, --, X5, Y5, X6, Y6, X7, Y7, X8, Y8" "/PNUM - Controls entity numbering/coloring on plots.
/PNUM, Label, KEY" "/POLYGON - Creates annotation polygons (GUI).
/POLYGON, NVERT, X1, Y1, X2, Y2, X3, Y3, X4, Y4" "/POST1
/POST1 - Enters the database results postprocessor." "/POST26
/POST26 - Enters the time-history results postprocessor." "/PREP7
/PREP7 - Enters the model creation preprocessor." "/PSEARCH - Specifies a directory to be searched for \"unknown command\" macro files.
/PSEARCH, Pname" "/PSF - Shows surface load symbols on model displays.
/PSF, Item, Comp, KEY, KSHELL, Color" "/PSPEC - Creates annotation polygon attributes (GUI).
/PSPEC, PCOLOR, KFILL, KBORDR" "/PSTATUS - Displays the global or window display specifications.
/PSTATUS, WN" "/PSYMB - Shows various symbols on displays.
/PSYMB, Label, KEY" "/PWEDGE - Creates an annotation wedge (GUI).
/PWEDGE, XCENTR, YCENTR, XLRAD, ANGLE1, ANGLE2" "/QUIT
/QUIT - Exits a processor." "/RATIO - Distorts the object geometry. 
/RATIO, WN, RATOX, RATOY" "/RENAME - Renames a file. 
/RENAME, Fname1, Ext1, --, Fname2, Ext2, --, DistKey" "/REPLOT - Automatically reissues the last display command for convenience. 
/REPLOT, Label" "/RESET
/RESET - Resets display specifications to their initial defaults. " "/RGB - Specifies the RGB color values for indices and contours. 
/RGB, Kywrd, PRED, PGRN, PBLU, N1, N2, NINC, NCNTR" "/RMDIR - Removes (deletes) a directory.
/RMDIR, Dir" "/SECLIB - Sets the default section library path for the SECREAD command.
/SECLIB, Option, Path" "/SEG - Allows graphics data to be stored in the local terminal memory.
/SEG, Label, Aviname, DELAY" "/SHADE - Defines the type of surface shading used with Z-buffering.
/SHADE, WN, Type" "/SHOW - Specifies the device and other parameters for graphics displays.
/SHOW, Fname, Option, VECT, NCPL" "/SHOWDISP - Defines the display driver name.
/SHOWDISP, Dname, --, --, NCPL" "/SHRINK - Shrinks elements, lines, areas, and volumes for display clarity.
/SHRINK, RATIO" "/SMBC - Controls the display of solid model boundary condition symbols and labels.
/SMBC, Mode" "/SOLU
/SOLU - Enters the solution processor." "/SSCALE - Sets the contour multiplier for topographic displays.
/SSCALE, WN, SMULT" "/STATUS - Lists the status of items for the run.
/STATUS, Lab" "/STITLE - Defines subtitles.
/STITLE, NLINE, Title" "/SYP - Passes a command string and arguments to the operating system.
/SYP, String, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7, ARG8" "/SYS - Passes a command string to the operating system.
/SYS, String" "/TEE - Writes a list of commands to a specified file at the same time that the commands are being executed.
/TEE, Label, Fname, Ext, --" "/TITLE - Defines a main title.
/TITLE, Title" "/TLABEL - Creates annotation text (GUI).
/TLABEL, XLOC, YLOC, Text" "/TRIAD - Shows the global XYZ coordinate triad on displays.
/TRIAD, Lab" "/TRLCY - Specifies the level of translucency.
/TRLCY, Lab, TLEVEL, N1, N2, NINC" "/TSPEC - Creates annotation text attributes (GUI).
/TSPEC, TCOLOR, TSIZE, TXTHIC, PANGLE, IANGLE" "/TXTRE - Controls application of texture to selected items. 
/TXTRE, Lab, NUM, N1, N2, NINC" "/TYPE - Defines the type of display.
/TYPE, WN, Type" "/UCMD - Assigns a user-defined command name.
/UCMD, Cmd, SRNUM" "/UDOC - Determines position and content for the multi-legend options. 
/UDOC, WIND, Class, Key," "/UI - Activates specified GUI dialog boxes.
/UI, Func, Type, Format, Screen, Color, Krev, Orient, Compress, Quality" "/UIS - Controls the GUI behavior.
/UIS, Label, VALUE" "/UNITS - Annotates the database with the system of units used.
/UNITS, Label, LENFACT, MASSFACT, TIMEFACT, TEMPFACT, TOFFSET, CHARGEFACT, FORCEFACT, HEATFACT" "/USER - Conveniently resets /FOCUS and /DIST to USER.
/USER, WN" "/VCONE - Defines the view cone angle for perspective displays.
/VCONE, WN, PHI" "/VIEW - Defines the viewing direction for the display.
/VIEW, WN, XV, YV, ZV" "/VSCALE - Scales the length of displayed vectors.
/VSCALE, WN, VRATIO, KEY" "/VUP - Specifies the global Cartesian coordinate system reference orientation.
/VUP, WN, Label" "/WAIT - Causes a delay before the reading of the next command.
/WAIT, DTIME" "/WINDOW - Defines the window size on the screen.
/WINDOW, WN, XMIN, XMAX, YMIN, YMAX, NCOPY" "/XFRM - Controls the centroid or the axis of dynamic rotation.
/XFRM, LAB, X1, Y1, Z1, X2, Y2, Z2" "/XRANGE - Specifies a linear abscissa (X) scale range.
/XRANGE, XMIN, XMAX" "/YRANGE - Specifies a linear ordinate (Y) scale range.
/YRANGE, YMIN, YMAX, NUM" "/ZOOM - Zooms a region of a display window.
/ZOOM, WN, Lab, X1, Y1, X2, Y2" "A - Defines an area by connecting keypoints.
A, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18" "AADD - Adds separate areas to create a single area.
AADD, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "AATT - Associates element attributes with the selected, unmeshed areas.
AATT, MAT, REAL, TYPE, ESYS, SECN" "ABBRES - Reads abbreviations from a coded file.
ABBRES, Lab, Fname, Ext, --" "ABBSAV - Writes the current abbreviation set to a coded file.
ABBSAV, Lab, Fname, Ext, --" "ABEXTRACT - Extracts the alpha-beta damping multipliers for Rayleigh damping.
ABEXTRACT, MODE1, MODE2" "ABS - Forms the absolute value of a variable.
ABS, IR, IA, --, --, Name, --, --, FACTA" "ACCAT - Concatenates multiple areas in preparation for mapped meshing.
ACCAT, NA1, NA2" "ACCOPTION - Specifies GPU accelerator capability options.
ACCOPTION, Activate" "ACEL - Specifies the linear acceleration of the global Cartesian reference frame for the analysis.
ACEL, ACEL_X, ACEL_Y, ACEL_Z" "ACLEAR - Deletes nodes and area elements associated with selected areas.
ACLEAR, NA1, NA2, NINC" "ADAMS - Performs solutions and writes flexible body information to a modal neutral file (Jobname.MNF) for use in an ADAMS analysis.
ADAMS, NMODES, KSTRESS, KSHELL" "ADAPT - Adaptively meshes and solves a model.
ADAPT, NSOLN, STARGT, TTARGT, FACMN, FACMX, KYKPS, KYMAC" "ADD - Adds (sums) variables.
ADD, IR, IA, IB, IC, Name, --, --, FACTA, FACTB, FACTC" "ADDAM - Specifies the acceleration spectrum computation constants for the analysis of shock resistance of shipboard structures.
ADDAM, AF, AA, AB, AC, AD, AMIN" "ADELE - Deletes unmeshed areas.
ADELE, NA1, NA2, NINC, KSWP" "ADGL - Lists keypoints of an area that lie on a parametric degeneracy.
ADGL, NA1, NA2, NINC" "ADRAG - Generates areas by dragging a line pattern along a path.
ADRAG, NL1, NL2, NL3, NL4, NL5, NL6, NLP1, NLP2, NLP3, NLP4, NLP5, NLP6" "AESIZE - Specifies the element size to be meshed onto areas.
AESIZE, ANUM, SIZE," "AFILLT - Generates a fillet at the intersection of two areas.
AFILLT, NA1, NA2, RAD" "AFLIST
AFLIST - Lists the current data in the database." "AFSURF - Generates surface elements overlaid on the surface of existing solid elements and assigns the extra node as the closest fluid element node.
AFSURF, SAREA, TLINE" "AGEN - Generates additional areas from a pattern of areas.
AGEN, ITIME, NA1, NA2, NINC, DX, DY, DZ, KINC, NOELEM, IMOVE" "AGLUE - Generates new areas by \"gluing\" areas.
AGLUE, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "AINA - Finds the intersection of areas.
AINA, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "AINP - Finds the pairwise intersection of areas.
AINP, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "AINV - Finds the intersection of an area with a volume.
AINV, NA, NV" "AL - Generates an area bounded by previously defined lines.
AL, L1, L2, L3, L4, L5, L6, L7, L8, L9, L10" "ALIST - Lists the defined areas.
ALIST, NA1, NA2, NINC, Lab" "ALLSEL - Selects all entities with a single command.
ALLSEL, LabT, Entity" "ALPHAD - Defines the mass matrix multiplier for damping.
ALPHAD, VALUE" "AMAP - Generates a 2-D mapped mesh based on specified area corners.
AMAP, AREA, KP1, KP2, KP3, KP4" "AMESH - Generates nodes and area elements within areas.
AMESH, NA1, NA2, NINC" "ANCNTR - Produces an animated sequence of a contoured deformed shape.
ANCNTR, NFRAM, DELAY, NCYCL" "ANCUT - Produces an animated sequence of Q-slices.
ANCUT, NFRAM, DELAY, NCYCL, QOFF, KTOP, TOPOFF, NODE1, NODE2, NODE3" "ANCYC - Applies a traveling wave animation to graphics data in a modal cyclic symmetry analysis.
ANCYC, NUMFRAMES, KCYCL, DELAY" "ANDATA - Displays animated graphics data for nonlinear problems.
ANDATA, DELAY, NCYCL, RSLTDAT, MIN, MAX, INCR, FRCLST, AUTOCONT, --, AUTOCNTR" "ANDSCL - Produces an animated sequence of a deformed shape.
ANDSCL, NFRAM, DELAY, NCYCL" "ANDYNA - Produces an animated sequence of contour values through substeps.
ANDYNA, DELAY, NCYCL, START, END, INC, AUTOCONTOURKEY" "ANFLOW - Produces an animated sequence of particle flow in a flowing fluid or a charged particle traveling in an electric or magnetic field.
ANFLOW, NFRAM, DELAY, NCYCL, TIME, SPACING, SIZE, LENGTH" "ANHARM - Produces an animated sequence of time-harmonic results or complex mode shapes.
ANHARM, NFRAM, DELAY, NCYCL, NPERIOD, CMS_ANTYPE, CMS_MODOPT" "ANIM - Displays animated graphics data for linear problems.
ANIM, NCYCL, KCYCL, DELAY" "ANISOS - Produces an animated sequence of an isosurface.
ANISOS, NFRAM, DELAY, NCYCL" "ANMODE - Produces an animated sequence of a mode shape.
ANMODE, NFRAM, DELAY, NCYCL, KACCEL" "ANMRES - Performs animation of results over multiple results files in an explicit dynamic structural analysis or fluid flow analysis with remeshing.
ANMRES, DELAY, MIN, MAX, INC, AUTOCNTRKY, FREQ, EXT" "ANORM - Reorients area normals.
ANORM, ANUM, NOEFLIP" "ANPRES - Produces an animated sequence of the time-harmonic pressure variation of an engine-order excitation in a cyclic harmonic analysis.
ANPRES, NFRAM, DELAY, NCYCL, RefFrame" "ANSOL - Specifies averaged nodal data to be stored from the results file in the solution coordinate system. 
ANSOL, NVAR, NODE, Item, Comp, Name, Mat, Real, Ename" "ANSTOAQWA - Creates an AQWA-LINE input file from the current ANSYS model.
ANSTOAQWA, Fname, VertAxis, Gc, Rho, HWL, DiffKey, SymxKey, SymyKey" "ANSTOASAS - Creates an ASAS input file from the current ANSYS model.
ANSTOASAS, Fname, KEY" "ANTIME - Produces a  sequential contour animation over a range of time.
ANTIME, NFRAM, DELAY, NCYCL, AUTOCNTRKY, RSLTDAT, MIN, MAX" "ANTYPE - Specifies the analysis type and restart status.
ANTYPE, Antype, Status, LDSTEP, SUBSTEP, Action" "AOFFST - Generates an area, offset from a given area.
AOFFST, NAREA, DIST, KINC" "AOVLAP - Overlaps areas.
AOVLAP, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "APLOT - Displays the selected areas.
APLOT, NA1, NA2, NINC, DEGEN, SCALE" "APPEND - Reads data from the results file and appends it to the database.
APPEND, LSTEP, SBSTEP, FACT, KIMG, TIME, ANGLE, NSET" "APTN - Partitions areas.
APTN, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "ARCLEN - Activates the arc-length method.
ARCLEN, Key, MAXARC, MINARC" "ARCTRM - Controls termination of the solution when the arc-length method is used.
ARCTRM, Lab, VAL, NODE, DOF" "AREAS
AREAS - Specifies \"Areas\" as the subsequent status topic." "AREFINE - Refines the mesh around specified areas.
AREFINE, NA1, NA2, NINC, LEVEL, DEPTH, POST, RETAIN" "AREMESH - Generates an area in which to create a new mesh for rezoning. 
AREMESH, LCOMB, ANGLE" "AREVERSE - Reverses the normal of an area, regardless of its connectivity or mesh status.
AREVERSE, ANUM, NOEFLIP" "AROTAT - Generates cylindrical areas by rotating a line pattern about an axis.
AROTAT, NL1, NL2, NL3, NL4, NL5, NL6, PAX1, PAX2, ARC, NSEG" "ARSCALE - Generates a scaled set of areas from a pattern of areas.
ARSCALE, NA1, NA2, NINC, RX, RY, RZ, KINC, NOELEM, IMOVE" "ARSYM - Generates areas from an area pattern by symmetry reflection.
ARSYM, Ncomp, NA1, NA2, NINC, KINC, NOELEM, IMOVE" "ASBA - Subtracts areas from areas.
ASBA, NA1, NA2, SEPO, KEEP1, KEEP2" "ASBL - Subtracts lines from areas.
ASBL, NA, NL, --, KEEPA, KEEPL" "ASBV - Subtracts volumes from areas.
ASBV, NA, NV, SEPO, KEEPA, KEEPV" "ASBW - Subtracts the intersection of the working plane from areas (divides areas).
ASBW, NA, SEPO, KEEP" "ASCRES - Specifies the output type for an acoustic scattering analysis.
ASCRES, Opt" "ASEL - Selects a subset of areas.
ASEL, Type, Item, Comp, VMIN, VMAX, VINC, KSWP" "ASIFILE - Writes or reads one-way acoustic-structural coupling data.
ASIFILE, Opt, Fname, Ext, Oper, kDim, kOut, LIMIT" "ASKIN - Generates an area by \"skinning\" a surface through guiding lines.
ASKIN, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "ASLL - Selects those areas containing the selected lines.
ASLL, Type, ARKEY" "ASLV - Selects those areas contained in the selected volumes.
ASLV, Type" "ASOL - Specifies the output type of an acoustic scattering analysis.
ASOL, Lab, Opt" "ASUB - Generates an area using the shape of an existing area.
ASUB, NA1, P1, P2, P3, P4" "ASUM - Calculates and prints geometry statistics of the selected areas.
ASUM, LAB" "ATAN - Forms the arctangent of a complex variable.
ATAN, IR, IA, --, --, Name, --, --, FACTA" "ATRAN - Transfers a pattern of areas to another coordinate system.
ATRAN, KCNTO, NA1, NA2, NINC, KINC, NOELEM, IMOVE" "ATYPE
ATYPE - Specifies \"Analysis types\" as the subsequent status topic." "AUTOTS - Specifies whether to use automatic time stepping or load stepping.
AUTOTS, Key" "AVPRIN - Specifies how principal and vector sums are to be calculated.
AVPRIN, KEY, EFFNU" "AVRES - Specifies how results data will be averaged when PowerGraphics is enabled.
AVRES, KEY, Opt" "AWAVE - Specifies input data for an acoustic incident wave.
AWAVE, Wavenum, Wavetype, Opt1, Opt2, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10, VAL11, VAL12, VAL13" "BCSOPTION -  Sets memory option for the sparse solver.
BCSOPTION, --, Memory_Option, Memory_Size, --, --, Solve_Info" "BETAD - Defines the stiffness matrix multiplier for damping.
BETAD, VALUE" "BF - Defines a nodal body force load.
BF, Node, Lab, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6" "BFA - Defines a body force load on an area.
BFA, Area, Lab, VAL1, VAL2, VAL3, VAL4" "BFADELE - Deletes body force loads on an area.
BFADELE, AREA, Lab" "BFALIST - Lists the body force loads on an area.
BFALIST, AREA, Lab" "BFCUM - Specifies that nodal body force loads are to be accumulated.
BFCUM, Lab, Oper, FACT, TBASE" "BFDELE - Deletes nodal body force loads.
BFDELE, NODE, Lab" "BFE - Defines an element body force load.
BFE, Elem, Lab, STLOC, VAL1, VAL2, VAL3, VAL4" "BFECUM - Specifies whether to ignore subsequent element body force loads.
BFECUM, Lab, Oper, FACT, TBASE" "BFEDELE - Deletes element body force loads.
BFEDELE, ELEM, Lab" "BFELIST - Lists the element body force loads.
BFELIST, ELEM, Lab" "BFESCAL - Scales element body force loads.
BFESCAL, Lab, FACT, TBASE" "BFINT - Activates the body force interpolation operation.
BFINT, Fname1, Ext1, --, Fname2, Ext2, --, KPOS, Clab, KSHS, TOLOUT, TOLHGT" "BFK - Defines a body force load at a keypoint.
BFK, Kpoi, Lab, VAL1, VAL2, VAL3, PHASE" "BFKDELE - Deletes body force loads at a keypoint.
BFKDELE, KPOI, Lab" "BFKLIST - Lists the body force loads at keypoints.
BFKLIST, KPOI, Lab" "BFL - Defines a body force load on a line.
BFL, Line, Lab, VAL1, VAL2, VAL3, VAL4" "BFLDELE - Deletes body force loads on a line.
BFLDELE, LINE, Lab" "BFLIST - Lists the body force loads on nodes.
BFLIST, NODE, Lab" "BFLLIST - Lists the body force loads on a line.
BFLLIST, LINE, Lab" "BFSCALE - Scales body force loads at nodes.
BFSCALE, Lab, FACT, TBASE" "BFTRAN
BFTRAN - Transfers solid model body force loads to the finite element model." "BFUNIF - Assigns a uniform body force load to all nodes.
BFUNIF, Lab, VALUE" "BFV - Defines a body force load on a volume.
BFV, Volu, Lab, VAL1, VAL2, VAL3, PHASE" "BFVDELE - Deletes body force loads on a volume.
BFVDELE, VOLU, Lab" "BFVLIST - Lists the body force loads on a volume.
BFVLIST, VOLU, Lab" "BIOOPT
BIOOPT - Specifies \"Biot-Savart options\" as the subsequent status topic." "BIOT - Calculates the Biot-Savart source magnetic field intensity.
BIOT, Label" "BLC4 - Creates a rectangular area or block volume by corner points.
BLC4, XCORNER, YCORNER, WIDTH, HEIGHT, DEPTH" "BLC5 - Creates a rectangular area or block volume by center and corner points.
BLC5, XCENTER, YCENTER, WIDTH, HEIGHT, DEPTH" "BLOCK - Creates a block volume based on working plane coordinates.
BLOCK, X1, X2, Y1, Y2, Z1, Z2" "BOOL
BOOL - Specifies \"Booleans\" as the subsequent status topic." "BOPTN - Specifies Boolean operation options.
BOPTN, Lab, Value" "BSAX - Specifies the axial strain and axial force relationship for beam sections.
BSAX, VAL1, VAL2, T" "BSM1 - Specifies the bending curvature and moment relationship in plane XZ for beam sections.
BSM1, VAL1, VAL2, T" "BSM2 - Specifies the bending curvature and moment relationship in plane XY for beam sections.
BSM2, VAL1, VAL2, T" "BSMD - Specifies mass per unit length for a nonlinear general beam section.
BSMD, DENS" "BSPLIN - Generates a single line from a spline fit to a series of keypoints.
BSPLIN, P1, P2, P3, P4, P5, P6, XV1, YV1, ZV1, XV6, YV6, ZV6" "BSS1 - Specifies the transverse shear strain and force relationship in plane XZ for beam sections.
BSS1, VAL1, VAL2, T" "BSS2 - Specifies the transverse shear strain and force relationship in plane XY for beam sections.
BSS2, VAL1, VAL2, T" "BSTE - Specifies a thermal expansion coefficient for a nonlinear general beam section.
BSTE, ALPHA" "BSTQ - Specifies the cross section twist and torque relationship for beam sections.
BSTQ, VAL1, VAL2, T" "BTOL - Specifies the Boolean operation tolerances.
BTOL, PTOL" "BUCOPT - Specifies buckling analysis options.
BUCOPT, Method, NMODE, SHIFT, LDMULTE, RangeKey" "C*** - Places a comment in the output.
C***, Comment" "CALC
CALC - Specifies \"Calculation settings\" as the subsequent status topic." "CAMPBELL - Prepares the result file for a subsequent Campbell diagram of a prestressed structure. 
CAMPBELL, Action" "CBDOF - Activates cut-boundary interpolation (for submodeling).
CBDOF, Fname1, Ext1, --, Fname2, Ext2, --, KPOS, Clab, KSHS, TOLOUT, TOLHGT, TOLTHK" "CBMD - Specifies preintegrated section mass matrix for composite-beam sections.
CBMD, ROW, C(R)(R), C(R)(R+1), C(R)(R+2), C(R)(R+3), C(R)(R+4), C(R)(R+5)" "CBMX - Specifies preintegrated cross-section stiffness for composite beam sections.
CBMX, ROW, S(R)(R), S(R)(R+1), S(R)(R+2), S(R)(R+3), S(R)(R+4), S(R)(R+5), S(R)(R+6)" "CBTE - Specifies a thermal expansion coefficient for a composite beam section. 
CBTE, ALPHA" "CBTMP - Specifies a temperature for composite-beam input. 
CBTMP, TEMP" "CDOPT - Specifies format to be used for archiving geometry.
CDOPT, Option" "CDREAD - Reads a file of solid model and database information into the database.
CDREAD, Option, Fname, Ext, --, Fnamei, Exti" "CDWRITE - Writes geometry and load database items to a file.
CDWRITE, Option, Fname, Ext, --, Fnamei, Exti, Fmat" "CE - Defines a constraint equation relating degrees of freedom.
CE, NEQN, CONST, NODE1, Lab1, C1, NODE2, Lab2, C2, NODE3, Lab3, C3" "CECHECK - Check constraint equations and couplings for rigid body motions.
CECHECK, ItemLab, Tolerance, DOF" "CECMOD - Modifies the constant term of a constraint equation during solution.
CECMOD, NEQN, CONST" "CECYC - Generates the constraint equations for a cyclic symmetry analysis
CECYC, Lowname, Highname, Nsector, HIndex, Tolerance, Kmove, Kpairs" "CEDELE - Deletes constraint equations.
CEDELE, NEQN1, NEQN2, NINC, Nsel" "CEINTF - Generates constraint equations at an interface.
CEINTF, TOLER, DOF1, DOF2, DOF3, DOF4, DOF5, DOF6, MoveTol" "CELIST - Lists the constraint equations.
CELIST, NEQN1, NEQN2, NINC, Option" "CENTER - Defines a node at the center of curvature of 2 or 3 nodes.
CENTER, NODE, NODE1, NODE2, NODE3, RADIUS" "CEQN
CEQN - Specifies \"Constraint equations\" as the subsequent status topic." "CERIG - Defines a rigid region.
CERIG, MASTE, SLAVE, Ldof, Ldof2, Ldof3, Ldof4, Ldof5" "CESGEN - Generates a set of constraint equations from existing sets.
CESGEN, ITIME, INC, NSET1, NSET2, NINC" "CFACT - Defines complex scaling factors to be used with operations.
CFACT, RFACTA, IFACTA, RFACTB, IFACTB, RFACTC, IFACTC" "CGLOC - Specifies the origin location of the acceleration coordinate system.
CGLOC, XLOC, YLOC, ZLOC" "CGOMGA - Specifies the rotational velocity of the global origin.
CGOMGA, CGOMX, CGOMY, CGOMZ" "CGROW - Defines crack-growth information
CGROW, Action, Par1, Par2" "CHECK - Checks current database items for completeness.
CHECK, Sele, Levl" "CHKMSH - Checks area and volume entities for previous meshes.
CHKMSH, Comp" "CINT - Defines parameters associated with fracture parameter calculations
CINT, Action, Par1, Par2, Par3, Par4, Par5, Par6, Par7" "CIRCLE - Generates circular arc lines.
CIRCLE, PCENT, RAD, PAXIS, PZERO, ARC, NSEG" "CISOL - Stores fracture parameter information in a variable.
CISOL, n, ID, node, Cont, Dtype" "CLOCAL - Defines a local coordinate system relative to the active coordinate system.
CLOCAL, KCN, KCS, XL, YL, ZL, THXY, THYZ, THZX, PAR1, PAR2" "CLOG - Forms the common log of a variable
CLOG, IR, IA, --, --, Name, --, --, FACTA, FACTB" "CLRMSHLN
CLRMSHLN - Clears meshed entities." "CM - Groups geometry items into a component.
CM, Cname, Entity" "CMACEL - Specifies the translational acceleration of an element component
CMACEL, CM_NAME, CMACEL_X, CMACEL_Y, CMACEL_Z" "CMATRIX - Performs electrostatic field solutions and calculates the self and mutual capacitances between multiple conductors.
CMATRIX, SYMFAC, Condname, NUMCOND, GRNDKEY, Capname" "CMDELE - Deletes a component or assembly definition.
CMDELE, Name" "CMDOMEGA - Specifies the rotational acceleration of an element component about a user-defined rotational axis.
CMDOMEGA, CM_NAME, DOMEGAX, DOMEGAY, DOMEGAZ, X1, Y1, Z1, X2, Y2, Z2" "CMEDIT - Edits an existing assembly.
CMEDIT, Aname, Oper, Cnam1, Cnam2, Cnam3, Cnam4, Cnam5, Cnam6, Cnam7" "CMGRP - Groups components and assemblies into an assembly.
CMGRP, Aname, Cnam1, Cnam2, Cnam3, Cnam4, Cnam5, Cnam6, Cnam7, Cnam8" "CMLIST - Lists the contents of a component or assembly.
CMLIST, Name, Key, Entity" "CMMOD - Modifies the specification of a component.
CMMOD, Cname, Keyword, Value" "CMOMEGA - Specifies the rotational velocity of an element component about a user-defined rotational axis.
CMOMEGA, CM_NAME, OMEGAX, OMEGAY, OMEGAZ, X1, Y1, Z1, X2, Y2, Z2" "CMPLOT - Plots the entities contained in a component or assembly.
CMPLOT, Label, Entity, Keyword" "CMROTATE - Specifies the rotational velocity of an element component in a brake squeal analysis.
CMROTATE, CM_Name, ROTATX, ROTATY, ROTATZ, X1, Y1, Z1, X2, Y2, Z2" "CMSEL - Selects a subset of components and assemblies.
CMSEL, Type, Name, Entity" "CMSFILE - Specifies a list of component mode synthesis (CMS) results files for plotting results on the assembly.
CMSFILE, Option, Fname, Ext, CmsKey" "CMSOPT - Specifies component mode synthesis (CMS) analysis options.
CMSOPT, CMSMETH, NMODE, FREQB, FREQE, FBDDEF, FBDVAL, IOKEY" "CMWRITE - Writes node and element components and assemblies to a file.
CMWRITE, Fname, Ext, --, --, Fmat" "CNCHECK - Provides and/or adjusts the initial status of contact pairs.
CNCHECK, Option, RID1, RID2, RINC, InterType, TRlevel, CGAP, CPEN, IOFF" "CNKMOD - Modifies contact element key options.
CNKMOD, ITYPE, KNUM, VALUE" "CNTR - Redirects contact pair output quantities to a text file.
CNTR, Option, Key" "CNVTOL - Sets convergence values for nonlinear analyses.
CNVTOL, Lab, VALUE, TOLER, NORM, MINREF" "COMBINE - Combines distributed memory parallel (Distributed ANSYS) files.
COMBINE, FileType" "COMPRESS
COMPRESS - Deletes all specified sets." "CON4 - Creates a conical volume anywhere on the working plane.
CON4, XCENTER, YCENTER, RAD1, RAD2, DEPTH" "CONE - Creates a conical volume centered about the working plane origin.
CONE, RBOT, RTOP, Z1, Z2, THETA1, THETA2" "CONJUG - Forms the complex conjugate of a variable.
CONJUG, IR, IA, --, --, Name, --, --, FACTA" "CORIOLIS - Applies the Coriolis effect to a rotating structure.
CORIOLIS, Option, --, --, RefFrame, RotDamp" "COUPLE
COUPLE - Specifies \"Node coupling\" as the subsequent status topic." "COVAL - Defines PSD cospectral values.
COVAL, TBLNO1, TBLNO2, SV1, SV2, SV3, SV4, SV5, SV6, SV7" "CP - Defines (or modifies) a set of coupled degrees of freedom.
CP, NSET, Lab, NODE1, NODE2, NODE3, NODE4, NODE5, NODE6, NODE7, NODE8, NODE9, NODE10, NODE11, NODE12, NODE13, NODE14, NODE15, NODE16, NODE17" "CPCYC - Couples the two side faces of a cyclically symmetric model for loadings that are the same on every segment.
CPCYC, Lab, TOLER, KCN, DX, DY, DZ, KNONROT" "CPDELE - Deletes coupled degree of freedom sets.
CPDELE, NSET1, NSET2, NINC, Nsel" "CPINTF - Defines coupled degrees of freedom at an interface.
CPINTF, Lab, TOLER" "CPLGEN - Generates sets of coupled nodes from an existing set.
CPLGEN, NSETF, Lab1, Lab2, Lab3, Lab4, Lab5" "CPLIST - Lists the coupled degree of freedom sets.
CPLIST, NSET1, NSET2, NINC, Nsel" "CPMERGE - Merges different couple sets with duplicate degrees of freedom into one couple set.
CPMERGE, Lab" "CPNGEN - Defines, modifies, or adds to a set of coupled degrees of freedom.
CPNGEN, NSET, Lab, NODE1, NODE2, NINC" "CPSGEN - Generates sets of coupled nodes from existing sets.
CPSGEN, ITIME, INC, NSET1, NSET2, NINC" "CQC - Specifies the complete quadratic mode combination method.
CQC, SIGNIF, Label, , ForceType" "CRPLIM - Specifies the creep criterion for automatic time stepping.
CRPLIM, CRCR, Option" "CS - Defines a local coordinate system by three node locations.
CS, KCN, KCS, NORIG, NXAX, NXYPL, PAR1, PAR2" "CSCIR - Locates the singularity for non-Cartesian local coordinate systems.
CSCIR, KCN, KTHET, KPHI" "CSDELE - Deletes local coordinate systems.
CSDELE, KCN1, KCN2, KCINC" "CSKP - Defines a local coordinate system by three keypoint locations.
CSKP, KCN, KCS, PORIG, PXAXS, PXYPL, PAR1, PAR2" "CSLIST - Lists coordinate systems.
CSLIST, KCN1, KCN2, KCINC" "CSWPLA - Defines a local coordinate system at the origin of the working plane.
CSWPLA, KCN, KCS, PAR1, PAR2" "CSYS - Activates a previously defined coordinate system.
CSYS, KCN" "CURR2D
CURR2D - Calculates current flow in a 2-D conductor." "CUTCONTROL - Controls time-step cutback during a nonlinear solution.
CUTCONTROL, Lab, VALUE, Option" "CVAR - Computes covariance between two quantities.
CVAR, IR, IA, IB, ITYPE, DATUM, Name" "CYCCALC - Calculates results from a cyclic harmonic mode-superposition analysis using the specifications defined by CYCSPEC.
CYCCALC, FilePrefix, FileFormat" "CYCFILES - Specifies the data files where results are to be found for a cyclic symmetry mode-superposition harmonic analysis.
CYCFILES, FnameRst, ExtRst, FnameRfrq, ExtRfrq" "CYCFREQ - Specifies solution options for a cyclic symmetry mode-superposition harmonic analysis.
CYCFREQ, Option, Value1, Value2, Value3, Value4, Value5" "CYCLIC - Specifies a cyclic symmetry analysis.
CYCLIC, NSECTOR, ANGLE, KCN, Name, USRCOMP, USRNMAP" "CYCOPT - Specifies solution options for a cyclic symmetry analysis.
CYCOPT, OPTION, Value1, Value2, Value3, Value4, Value5, Value6, Value7" "CYCPHASE - Provides tools for determining minimum and maximum possible result values from frequency couplets produced in a modal cyclic symmetry analysis.
CYCPHASE, TYPE, OPTION" "CYCSPEC - Defines the set of result items for a subsequent CYCCALC command in postprocessing a cyclic harmonic mode-superposition analysis.
CYCSPEC, LABEL, Node, Item, Comp" "CYL4 - Creates a circular area or cylindrical volume anywhere on the working plane.
CYL4, XCENTER, YCENTER, RAD1, THETA1, RAD2, THETA2, DEPTH" "CYL5 - Creates a circular area or cylindrical volume by end points.
CYL5, XEDGE1, YEDGE1, XEDGE2, YEDGE2, DEPTH" "CYLIND - Creates a cylindrical volume centered about the working plane origin.
CYLIND, RAD1, RAD2, Z1, Z2, THETA1, THETA2" "CZDEL - Edits or clears cohesive zone sections.
CZDEL, grp1, grp2, grp3" "CZMESH - Create and mesh an interface area composed of cohesive zone elements.
CZMESH, ecomps1, ecomps2, KCN, KDIR, VALUE, CZTOL" "D - Defines degree-of-freedom constraints at nodes.
D, Node, Lab, VALUE, VALUE2, NEND, NINC, Lab2, Lab3, Lab4, Lab5, Lab6" "DA - Defines degree-of-freedom constraints on areas.
DA, AREA, Lab, Value1, Value2" "DADELE - Deletes degree-of-freedom constraints on an area.
DADELE, AREA, Lab" "DALIST - Lists the DOF constraints on an area.
DALIST, AREA" "DAMORPH - Move nodes in selected areas to conform to structural displacements.
DAMORPH, AREA, XLINE, RMSHKY" "DATA - Reads data records from a file into a variable.
DATA, IR, LSTRT, LSTOP, LINC, Name, KCPLX" "DATADEF
DATADEF - Specifies \"Directly defined data status\" as the subsequent status topic." "DCGOMG - Specifies the rotational acceleration of the global origin.
DCGOMG, DCGOX, DCGOY, DCGOZ" "DCUM - Specifies that DOF constraint values are to be accumulated.
DCUM, Oper, RFACT, IFACT, TBASE" "DCVSWP - Performs a DC voltage sweep on a ROM element.
DCVSWP, Option, Elem, Cnum, Vmax, Vinc1, Vinc2, Gap" "DDASPEC - Specifies the shock spectrum computation constants for DDAM analysis.
DDASPEC, KeyRef, Shptyp, MountLoc, Deftyp, Amin" "DDELE - Deletes degree-of-freedom constraints.
DDELE, NODE, Lab, NEND, NINC, Rkey" "DDOPTION -  Sets domain decomposer option for Distributed ANSYS.
DDOPTION, Decomp" "DEACT
DEACT - Specifies \"Element birth and death\" as the subsequent status topic." "DEFINE
DEFINE - Specifies \"Data definition settings\" as the subsequent status topic." "DELETE - Specifies sets in the results file to be deleted before postprocessing.
DELETE, SET, Nstart, Nend" "DELTIM - Specifies the time step sizes to be used for the current load step.
DELTIM, DTIME, DTMIN, DTMAX, Carry" "DEMORPH - Move nodes in selected elements to conform to structural displacements.
DEMORPH, ELEM, DIMN, RMSHKY" "DERIV - Differentiates a variable.
DERIV, IR, IY, IX, --, Name, --, --, FACTA" "DESIZE - Controls default element sizes.
DESIZE, MINL, MINH, MXEL, ANGL, ANGH, EDGMN, EDGMX, ADJF, ADJM" "DESOL - Defines or modifies solution results at a node of an element.
DESOL, ELEM, NODE, Item, Comp, V1, V2, V3, V4, V5, V6" "DETAB - Modifies element table results in the database.
DETAB, ELEM, Lab, V1, V2, V3, V4, V5, V6" "DFLX - Imposes a uniform magnetic flux B on an edge-element electromagnetic model.
DFLX, NODE, BX, BY, BZ, BX2, BY2, BZ2" "DFSWAVE - Specifies the incident planar waves with random phases for a diffuse sound field.
DFSWAVE, KCN, RADIUS, PSDREF, DENS, SONIC, INCANG, NPARA, SampOpt" "DIG - Digitizes nodes to a surface.
DIG, NODE1, NODE2, NINC" "DIGIT
DIGIT - Specifies \"Node digitizing\" as the subsequent status topic." "DISPLAY
DISPLAY - Specifies \"Display settings\" as the subsequent status topic." "DJ - Specifies boundary conditions on the components of relative motion of a joint element.
DJ, ELEM, LABEL, VALUE" "DJDELE - Deletes boundary conditions on the components of relative motion of a joint element.
DJDELE, ELEM, LAB" "DJLIST - Lists boundary conditions applied to joint elements.
DJLIST, Elem" "DK - Defines DOF constraints at keypoints.
DK, KPOI, Lab, VALUE, VALUE2, KEXPND, Lab2, Lab3, Lab4, Lab5, Lab6" "DKDELE - Deletes DOF constraints at a keypoint.
DKDELE, KPOI, Lab" "DKLIST - Lists the DOF constraints at keypoints.
DKLIST, KPOI" "DL - Defines DOF constraints on lines.
DL, LINE, AREA, Lab, Value1, Value2" "DLDELE - Deletes DOF constraints on a line.
DLDELE, LINE, Lab" "DLIST - Lists DOF constraints.
DLIST, NODE1, NODE2, NINC" "DLLIST - Lists DOF constraints on a line.
DLLIST, LINE" "DMOVE - Digitizes nodes on surfaces and along intersections.
DMOVE, NODE1, NODE2, NINC" "DMPEXT - Extracts modal damping coefficients in a specified frequency range.
DMPEXT, SMODE, TMODE, Dmpname, Freqb, Freqe, NSTEPS" "DMPOPTION - Specifies distributed memory parallel (Distributed ANSYS) file combination options.
DMPOPTION, FileType, Combine" "DMPRAT - Sets a constant modal damping ratio.
DMPRAT, RATIO" "DMPSTR - Sets a constant structural damping coefficient.
DMPSTR, COEFF" "DNSOL - Defines or modifies solution results at a node.
DNSOL, NODE, Item, Comp, V1, V2, V3, V4, V5, V6" "DOF - Adds degrees of freedom to the current DOF set.
DOF, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6, Lab7, Lab8, Lab9, Lab10" "DOFSEL - Selects a DOF label set for reference by other commands.
DOFSEL, Type, Dof1, Dof2, Dof3, Dof4, Dof5, Dof6" "DOMEGA - Specifies the rotational acceleration of the structure.
DOMEGA, DOMGX, DOMGY, DOMGZ" "DSCALE - Scales DOF constraint values.
DSCALE, RFACT, IFACT, TBASE" "DSET - Sets the scale and drawing plane orientation for a digitizing tablet.
DSET, NODE1, NODE2, NODE3, DDEV" "DSPOPTION -  Sets memory option for the distributed sparse solver.
DSPOPTION, Reord_Option, Memory_Option, Memory_Size, --, --, Solve_Info" "DSUM - Specifies the double sum mode combination method.
DSUM, SIGNIF, Label, TD, ForceType" "DSURF - Defines the surface upon which digitized nodes lie.
DSURF, KCN, XSURF, YSURF, ZSURF" "DSYM - Specifies symmetry or antisymmetry degree-of-freedom constraints on nodes.
DSYM, Lab, Normal, KCN" "DSYS - Activates a display coordinate system for geometry listings and plots.
DSYS, KCN" "DTRAN
DTRAN - Transfers solid model DOF constraints to the finite element model." "DUMP - Dumps the contents of a binary file.
DUMP, NSTRT, NSTOP" "DVAL - Defines values at enforced motion base.
DVAL, BaseID, Lab, VALUE, VALUE2, KeyCal" "DVMORPH - Move nodes in selected volumes to conform to structural displacements.
DVMORPH, VOLU, XAREA, RMSHKY" "DYNOPT
DYNOPT - Specifies \"Dynamic analysis options\" as the subsequent status topic." "E - Defines an element by node connectivity.
E, I, J, K, L, M, N, O, P" "EALIVE - Reactivates an element (for the birth and death capability).
EALIVE, ELEM" "EDADAPT - Activates adaptive meshing in an explicit dynamic analysis.
EDADAPT, PART, Key" "EDALE - Assigns mesh smoothing to explicit dynamic elements that use the ALE formulation.
EDALE, Option, --, AFAC, BFAC, --, DFAC, EFAC, START, END" "EDASMP - Creates a part assembly to be used in an explicit dynamic analysis.
EDASMP, Option, ASMID, PART1, PART2, PART3, PART4, PART5, PART6, PART7, PART8, PART9, PART10, PART11, PART12, PART13, PART14, PART15, PART16" "EDBOUND - Defines a boundary plane for sliding or cyclic symmetry.
EDBOUND, Option, Lab, Cname, XC, YC, ZC, Cname2, COPT" "EDBVIS - Specifies global bulk viscosity coefficients for an explicit dynamics analysis.
EDBVIS, QVCO, LVCO" "EDBX - Creates a box shaped volume to be used in a contact definition for explicit dynamics.
EDBX, Option, BOXID, XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX" "EDCADAPT - Specifies adaptive meshing controls for an explicit dynamic analysis.
EDCADAPT, FREQ, TOL, OPT, MAXLVL, BTIME, DTIME, LCID, ADPSIZE, ADPASS, IREFLG, ADPENE, ADPTH, MAXEL" "EDCGEN - Specifies contact parameters for an explicit dynamics analysis.
EDCGEN, Option, Cont, Targ, FS, FD, DC, VC, VDC, V1, V2, V3, V4, BTIME, DTIME, BOXID1, BOXID2" "EDCLIST - Lists contact entity specifications in an explicit dynamics analysis.
EDCLIST, NUM" "EDCMORE - Specifies additional contact parameters for a given contact definition in an explicit dynamic analysis.
EDCMORE, Option, NUM, --, VAL1,VAL2" "EDCNSTR - Defines various types of constraints for an explicit dynamic analysis.
EDCNSTR, Option, Ctype, Comp1, Comp2, VAL1" "EDCONTACT - Specifies contact surface controls for an explicit dynamics analysis.
EDCONTACT, SFSI, RWPN, IPCK, SHTK, PENO, STCC, ORIE, CSPC, PENCHK" "EDCPU - Specifies CPU time limit for an explicit dynamics analysis.
EDCPU, CPUTIME" "EDCRB - Constrains two rigid bodies to act as one in an explicit dynamics analysis.
EDCRB, Option, NEQN, PARTM, PARTS" "EDCSC - Specifies whether to use subcycling in an explicit dynamics analysis.
EDCSC, Key" "EDCTS - Specifies mass scaling and scale factor of computed time step for an explicit dynamics analysis.
EDCTS, DTMS, TSSFAC" "EDCURVE - Specifies data curves for an explicit dynamic analysis.
EDCURVE, Option, LCID, Par1, Par2" "EDDAMP - Defines mass weighted (Alpha) or stiffness weighted (Beta) damping for an explicit dynamics model.
EDDAMP, PART, LCID, VALDMP" "EDDBL -  Selects a numerical precision type of the explicit dynamics analysis.
EDDBL, KEY" "EDDC - Deletes or deactivates/reactivates contact surface specifications in an explicit dynamic analysis.
EDDC, Option, Ctype, Cont, Targ" "EDDRELAX - Activates initialization to a prescribed geometry or dynamic relaxation for the explicit analysis.
EDDRELAX, Option, NRCYCK, DRTOL, DFFCTR, DRTERM, TSSFDR, IRELAL, EDTTL" "EDDUMP - Specifies output frequency for the explicit dynamic restart file (d3dump).
EDDUMP, NUM, DT" "EDELE - Deletes selected elements from the model.
EDELE, IEL1, IEL2, INC" "EDENERGY - Specifies energy dissipation controls for an explicit dynamics analysis.
EDENERGY, HGEN, SWEN, SIEN, RLEN" "EDFPLOT - Allows plotting of explicit dynamics forces and other load symbols.
EDFPLOT, Key" "EDGCALE - Defines global ALE controls for an explicit dynamic analysis.
EDGCALE, NADV, METH" "EDHGLS - Specifies the hourglass coefficient for an explicit dynamics analysis.
EDHGLS, HGCO" "EDHIST - Specifies time-history output for an explicit dynamic analysis.
EDHIST, Comp" "EDHTIME - Specifies the time-history output interval for an explicit dynamics analysis.
EDHTIME, NSTEP, DT" "EDINT - Specifies number of integration points for explicit shell and beam output.
EDINT, SHELLIP, BEAMIP" "EDIPART - Defines inertia for rigid parts in an explicit dynamics analysis.
EDIPART, PART, Option, Cvect, TM, IRCS, Ivect, Vvect, CID" "EDIS - Specifies stress initialization in an explicit dynamic full restart analysis.
EDIS, Option, PIDN, PIDO" "EDLCS - Defines a local coordinate system for use in explicit dynamics analysis.
EDLCS, Option, CID, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3" "EDLOAD - Specifies loads for an explicit dynamics analysis.
EDLOAD, Option, Lab, KEY, Cname, Par1, Par2, PHASE, LCID, SCALE, BTIME, DTIME" "EDMP - Defines material properties for an explicit dynamics analysis.
EDMP, Lab, MAT, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6" "EDNB - Defines a nonreflecting boundary in an explicit dynamic analysis.
EDNB, Option, Cname, AD, AS" "EDNDTSD - Allows smoothing of noisy data for explicit dynamics analyses and provides a graphical representation of the data.
EDNDTSD, Vect1, Vect2, DATAP, FITPT, Vect3, Vect4, DISP" "EDNROT - Applies a rotated coordinate nodal constraint in an explicit dynamics analysis.
EDNROT, Option, CID, Cname, DOF1, DOF2, DOF3, DOF4, DOF5, DOF6" "EDOPT - Specifies the type of output for an explicit dynamics analysis.
EDOPT, Option, --, Value" "EDOUT - Specifies time-history output (ASCII format) for an explicit dynamics analysis.
EDOUT, Option" "EDPART - Configures parts for an explicit dynamics analysis.
EDPART, Option, PARTID, Cname" "EDPC - Selects and plots explicit dynamic contact entities.
EDPC, MIN, MAX, INC" "EDPL - Plots a time dependent load curve in an explicit dynamic analysis.
EDPL, LDNUM" "EDPVEL - Applies initial velocities to parts or part assemblies in an explicit dynamic analysis.
EDPVEL, Option, PID, VX, VY, VZ, OMEGAX, OMEGAY, OMEGAZ, XC, YC, ZC, ANGX, ANGY, ANGZ" "EDRC - Specifies rigid/deformable switch controls in an explicit dynamic analysis.
EDRC, Option, NRBF, NCSF, --, DTMAX" "EDRD - Switches a part from deformable to rigid or from rigid to deformable in an explicit dynamic analysis.
EDRD, Option, PART, MRB" "EDREAD - Reads explicit dynamics output into variables for time-history postprocessing.
EDREAD, NSTART, Label, NUM, STEP1, STEP2" "EDRI - Defines inertia properties for a new rigid body that is created when a deformable part is switched to rigid in an explicit dynamic analysis.
EDRI, Option, PART, XC, YC, ZC, TM, IXX, IYY, IZZ, IXY, IYZ, IXZ" "EDRST - Specifies the output interval for an explicit dynamic analysis.
EDRST, NSTEP, DT" "EDRUN - Specify LS-DYNA serial or parallel processing.
EDRUN, Option, Cons, Ncpu" "EDSHELL - Specifies shell computation controls for an explicit dynamics analysis.
EDSHELL, WPAN, SHNU, SHTC, WPBT, SHPL, ITRST" "EDSOLV
EDSOLV - Specifies \"explicit dynamics solution\" as the subsequent status topic." "EDSP - Specifies small penetration checking for contact entities in an explicit dynamic analysis.
EDSP, Option, MIN, MAX, INC" "EDSTART - Specifies status (new or restart) of an explicit dynamics analysis.
EDSTART, RESTART, MEMORY, FSIZE, Dumpfile" "EDTERM - Specifies termination criteria for an explicit dynamic analysis.
EDTERM, Option, Lab, NUM, STOP, MAXC, MINC" "EDTP - Plots explicit elements based on their time step size.
EDTP, OPTION, VALUE1, VALUE2" "EDVEL - Applies initial velocities to nodes or node components in an explicit dynamic analysis.
EDVEL, Option, Cname, VX, VY, VZ, OMEGAX, OMEGAY, OMEGAZ, XC, YC, ZC, ANGX, ANGY, ANGZ" "EDWELD - Defines a massless spotweld or generalized weld for use in an explicit dynamic analysis.
EDWELD, Option, NWELD, N1, N2, SN, SS, EXPN, EXPS, EPSF, TFAIL, NSW, CID" "EDWRITE - Writes explicit dynamics input to an LS-DYNA input file.
EDWRITE, Option, Fname, Ext, --" "EEXTRUDE - Extrudes 2-D plane elements into 3-D solids.
EEXTRUDE, Action, NELEM, SPACE, DIST, THETA, TFACT" "EGEN - Generates elements from an existing pattern.
EGEN, ITIME, NINC, IEL1, IEL2, IEINC, MINC, TINC, RINC, CINC, SINC, DX, DY, DZ" "EINFIN - Generates structural infinite elements from selected nodes.
EINFIN, CompName, PNODE" "EINTF - Defines two-node elements between coincident or offset nodes.
EINTF, TOLER, K, TLAB, KCN, DX, DY, DZ, KNONROT" "EKILL - Deactivates an element (for the birth and death capability).
EKILL, ELEM" "ELBOW - Specifies degrees of freedom to be coupled for end release and applies section constraints to elbow elements.
ELBOW, Transkey, TOL, Dof, Cons1, Cons2, Cons3, Cons4" "ELEM
ELEM - Specifies \"Elements\" as the subsequent status topic." "ELIST - Lists the elements and their attributes.
ELIST, IEL1, IEL2, INC, NNKEY, RKEY, PTKEY" "EMAGERR
EMAGERR - Calculates the relative error in an electrostatic or electromagnetic field analysis." "EMATWRITE - Forces the writing of all the element matrices to File.EMAT.
EMATWRITE, Key" "EMF
EMF - Calculates the electromotive force (emf), or voltage drop along a predefined path." "EMFT
EMFT - Summarizes electromagnetic forces and torques." "EMID - Adds or removes midside nodes.
EMID, Key, Edges" "EMIS - Specifies emissivity as a material property for the Radiation Matrix method.
EMIS, MAT, EVALU" "EMODIF - Modifies a previously defined element.
EMODIF, IEL, STLOC, I1, I2, I3, I4, I5, I6, I7, I8" "EMORE - Adds more nodes to the just-defined element.
EMORE, Q, R, S, T, U, V, W, X" "EMSYM - Specifies circular symmetry for electromagnetic sources.
EMSYM, NSECT" "EMTGEN - Generates a set of TRANS126 elements.
EMTGEN, Ncomp, Ecomp, PNcomp, DOF, GAP, GAPMIN, FKN, EPZERO" "EMUNIT - Specifies the system of units for magnetic field problems.
EMUNIT, Lab, VALUE" "EN - Defines an element by its number and node connectivity.
EN, IEL, I, J, K, L, M, N, O, P" "ENDRELEASE - Specifies degrees of freedom to be decoupled for end release.
ENDRELEASE, --, TOLERANCE, Dof1, Dof2, Dof3, Dof4" "ENERSOL - Specifies the total energies to be stored.
ENERSOL, NVAR, Item, --, Name" "ENGEN - Generates elements from an existing pattern.
ENGEN, IINC, ITIME, NINC, IEL1, IEL2, IEINC, MINC, TINC, RINC, CINC, SINC, DX, DY, DZ" "ENORM - Reorients shell element normals or line element node connectivity.
ENORM, ENUM" "ENSYM - Generates elements by symmetry reflection.
ENSYM, IINC, --, NINC, IEL1, IEL2, IEINC" "EORIENT - Reorients solid element normals.
EORIENT, Etype, Dir, TOLER" "EPLOT
EPLOT - Produces an element display." "EQSLV - Specifies the type of equation solver.
EQSLV, Lab, TOLER, MULT, --, KeepFile" "ERASE
ERASE - Explicitly erases the current display." "EREAD - Reads elements from a file.
EREAD, Fname, Ext, --" "EREFINE - Refines the mesh around specified elements.
EREFINE, NE1, NE2, NINC, LEVEL, DEPTH, POST, RETAIN" "EREINF
EREINF - Generates reinforcing elements from selected existing (base) elements." "ERESX - Specifies extrapolation of integration point results.
ERESX, Key" "ERNORM - Controls error estimation calculations.
ERNORM, Key" "ERRANG - Specifies the element range to be read from a file.
ERRANG, EMIN, EMAX, EINC" "ESCHECK - Perform element shape checking for a selected element set.
ESCHECK, Sele, Levl, Defkey" "ESEL - Selects a subset of elements.
ESEL, Type, Item, Comp, VMIN, VMAX, VINC, KABS" "ESIZE - Specifies the default number of line divisions.
ESIZE, SIZE, NDIV" "ESLA - Selects those elements associated with the selected areas.
ESLA, Type" "ESLL - Selects those elements associated with the selected lines.
ESLL, Type" "ESLN - Selects those elements attached to the selected nodes.
ESLN, Type, EKEY, NodeType" "ESLV - Selects elements associated with the selected volumes.
ESLV, Type" "ESOL - Specifies element data to be stored from the results file.
ESOL, NVAR, ELEM, NODE, Item, Comp, Name" "ESORT - Sorts the element table.
ESORT, Item, Lab, ORDER, KABS, NUMB" "ESSOLV - Performs a coupled electrostatic-structural analysis.
ESSOLV, Electit, Strutit, DIMN, MORPHOPT, Mcomp, Xcomp, ELECTOL, STRUTOL, MXLOOP, --, RUSEKY, RESTKY, EISCOMP" "ESTIF - Specifies the matrix multiplier for deactivated elements.
ESTIF, KMULT" "ESURF - Generates elements overlaid on the free faces of selected nodes.
ESURF, XNODE, Tlab, Shape" "ESYM - Generates elements from a pattern by a symmetry reflection.
ESYM, --, NINC, IEL1, IEL2, IEINC" "ESYS - Sets the element coordinate system attribute pointer.
ESYS, KCN" "ET - Defines a local element type from the element library.
ET, ITYPE, Ename, KOP1, KOP2, KOP3, KOP4, KOP5, KOP6, INOPR" "ETABLE - Fills a table of element values for further processing.
ETABLE, Lab, Item, Comp, Option" "ETCHG - Changes element types to their corresponding types.
ETCHG, Cnv" "ETCONTROL - Control the element technologies used in element formulation (for applicable elements). 
ETCONTROL, Eltech, Eldegene" "ETDELE - Deletes element types.
ETDELE, ITYP1, ITYP2, INC" "ETLIST - Lists currently defined element types.
ETLIST, ITYP1, ITYP2, INC" "ETYPE
ETYPE - Specifies \"Element types\" as the subsequent status topic." "EUSORT
EUSORT - Restores original order of the element table." "EWRITE - Writes elements to a file.
EWRITE, Fname, Ext, --, KAPPND, Format" "EXP - Forms the exponential of a variable.
EXP, IR, IA, --, --, Name, --, --, FACTA, FACTB" "EXPAND - Displays the results of a modal cyclic symmetry analysis.
EXPAND, Nrepeat, MODAL, HIndex, Icsys, SctAng, --, Phase" "EXPASS - Specifies an expansion pass of an analysis.
EXPASS, Key" "EXPROFILE - Exports interface loads or loads on selected nodes to an ANSYS CFX Profile file.
EXPROFILE, Ldtype, Load, VALUE, Pname, Fname, Fext, Fdir" "EXPSOL - Specifies the solution to be expanded for mode-superposition analyses or substructure analyses.
EXPSOL, LSTEP, SBSTEP, TIMFRQ, Elcalc" "EXTOPT - Controls options relating to the generation of volume elements from area elements.
EXTOPT, Lab, Val1, Val2, Val3, Val4" "EXTREM - Lists the extreme values for variables.
EXTREM, NVAR1, NVAR2, NINC" "EXUNIT - Specifies the interface load unit labels to be written to the export file for ANSYS to CFX transfer.
EXUNIT, Ldtype, Load, Untype, Name" "F - Specifies force loads at nodes.
F, NODE, Lab, VALUE, VALUE2, NEND, NINC" "FATIGUE
FATIGUE - Specifies \"Fatigue data status\" as the subsequent status topic." "FC - Provides failure criteria information and activates a data table to input temperature-dependent stress and strain limits.
FC, MAT, Lab1, Lab2, DATA1, DATA2, DATA3, DATA4, DATA5, DATA6" "FCCHECK
FCCHECK -  Checks both the strain and stress input criteria for all materials." "FCDELE - Deletes previously defined failure criterion data for the given material.
FCDELE, MAT" "FCLIST - To list what the failure criteria is that you have input.
FCLIST, MAT, --, TEMP" "FCTYP - Activates or removes failure-criteria types for postprocessing. 
FCTYP, Oper, Lab" "FCUM - Specifies that force loads are to be accumulated.
FCUM, Oper, RFACT, IFACT" "FDELE - Deletes force loads on nodes.
FDELE, NODE, Lab, NEND, NINC, Lkey" "FE - Defines a set of fatigue event parameters.
FE, NEV, CYCLE, FACT, Title" "FEBODY
FEBODY - Specifies \"Body loads on elements\" as the subsequent status topic." "FECONS
FECONS - Specifies \"Constraints on nodes\" as the subsequent status topic." "FEFOR
FEFOR - Specifies \"Forces on nodes\" as the subsequent status topic." "FELIST - Lists the fatigue event parameters.
FELIST, NEV1, NEV2, NINC" "FESURF
FESURF - Specifies \"Surface loads on elements\" as the subsequent status topic." "FILE - Specifies the data file where results are to be found.
FILE, Fname, Ext, --" "FILEAUX2 - Specifies the binary file to be dumped.
FILEAUX2, Fname, Ident, --" "FILEAUX3 - Specifies the results file to be edited.
FILEAUX3, Fname, Ext, --" "FILEDISP - Specifies the file containing the graphics data.
FILEDISP, Fname, Ext" "FILL - Generates a line of nodes between two existing nodes.
FILL, NODE1, NODE2, NFILL, NSTRT, NINC, ITIME, INC, SPACE" "FILLDATA - Fills a variable by a ramp function.
FILLDATA, IR, LSTRT, LSTOP, LINC, VALUE, DVAL" "FINISH
FINISH - Exits normally from a processor." "FITEM - Identifies items chosen by a picking operation (GUI).
FITEM, NFIELD, ITEM, ITEMY, ITEMZ" "FJ - Specify forces or moments on the components of the relative motion of a joint element.
FJ, ELEM, LABEL, VALUE" "FJDELE - Deletes forces (or moments) on the components of the relative motion of a joint element.
FJDELE, ELEM, LAB" "FJLIST - Lists forces and moments applied on joint elements.
FJLIST, Elem" "FK - Defines force loads at keypoints.
FK, KPOI, Lab, VALUE, VALUE2" "FKDELE - Deletes force loads at a keypoint.
FKDELE, KPOI, Lab" "FKLIST - Lists the forces at keypoints.
FKLIST, KPOI, Lab" "FL - Defines a set of fatigue location parameters.
FL, NLOC, NODE, SCFX, SCFY, SCFZ, Title" "FLIST - Lists force loads on the nodes.
FLIST, NODE1, NODE2, NINC" "FLLIST - Lists the fatigue location parameters.
FLLIST, NLOC1, NLOC2, NINC" "FLST - Specifies data required for a picking operation (GUI).
FLST, NFIELD, NARG, TYPE, Otype, LENG" "FLUREAD - Reads one-way Fluent-to-Mechanical APDL coupling data via a .cgns file with one-side fast Fourier transformation complex pressure peak value.
FLUREAD, --, Fname, Ext, KDIM, KOUT, LIMIT, ListOpt" "FLUXV
FLUXV - Calculates the flux passing through a closed contour." "FMAGBC - Applies force and torque boundary conditions to an element component.
FMAGBC, Cnam1, Cnam2, Cnam3, Cnam4, Cnam5, Cnam6, Cnam7, Cnam8, Cnam9" "FMAGSUM - Summarizes electromagnetic force calculations on element components.
FMAGSUM, Cnam1, Cnam2, Cnam3, Cnam4, Cnam5, Cnam6, Cnam7, Cnam8, Cnam9" "FOR2D
FOR2D - Calculates magnetic forces on a body." "FORCE - Selects the element nodal force type for output.
FORCE, Lab" "FORM - Specifies the format of the file dump.
FORM, Lab" "FP - Defines the fatigue S vs. N and Sm vs. T tables.
FP, STITM, C1, C2, C3, C4, C5, C6" "FPLIST
FPLIST - Lists the property table stored for fatigue evaluation." "FREQ - Defines the frequency points for the SV vs. FREQ tables.
FREQ, FREQ1, FREQ2, FREQ3, FREQ4, FREQ5, FREQ6, FREQ7, FREQ8, FREQ9" "FRQSCL - Turns on automatic scaling of the entire mass matrix and frequency range for modal analyses using the Block Lanczos, PCG Lanczos, or Supernode mode extraction method.
FRQSCL, Scaling" "FS - Stores fatigue stress components at a node.
FS, NODE, NEV, NLOD, STITM, C1, C2, C3, C4, C5, C6" "FSCALE - Scales force load values in the database.
FSCALE, RFACT, IFACT" "FSDELE - Deletes a stress condition for a fatigue location, event, and loading.
FSDELE, NLOC, NEV, NLOD" "FSLIST - Lists the stresses stored for fatigue evaluation.
FSLIST, NLOC1, NLOC2, NINC, NEV, NLOD" "FSNODE - Calculates and stores the stress components at a node for fatigue.
FSNODE, NODE, NEV, NLOD" "FSPLOT - Displays a fatigue stress item for a fatigue location and event.
FSPLOT, NLOC, NEV, ITEM" "FSSECT - Calculates and stores total linearized stress components.
FSSECT, RHO, NEV, NLOD, KBR" "FSSPARM - Calculates reflection and transmission properties of a frequency selective surface.
FSSPARM, PORT1, PORT2" "FSUM - Sums the nodal force and moment contributions of elements.
FSUM, LAB, ITEM" "FTCALC - Performs fatigue calculations for a particular node location.
FTCALC, NLOC, NODE" "FTRAN
FTRAN - Transfers solid model forces to the finite element model." "FTSIZE - Defines the fatigue data storage array.
FTSIZE, MXLOC, MXEV, MXLOD" "FTWRITE - Writes all currently stored fatigue data on a file.
FTWRITE, Fname, Ext, --" "FTYPE - Specifies the file type and pressure type for the subsequent import of source points and pressures.
FTYPE, FileType, PresType" "FVMESH - Generates nodes and tetrahedral volume elements from detached exterior area elements (facets).
FVMESH, KEEP" "GAP
GAP - Specifies \"mode-superposition transient gap conditions\" as the subsequent status topic." "GAPF - Defines the gap force data to be stored in a variable.
GAPF, NVAR, NUM, Name" "GAUGE - Gauges the problem domain for a magnetic edge-element formulation.
GAUGE, Opt, FREQ" "GCDEF - Defines interface interactions between general contact surfaces.
GCDEF, Option, SECT1, SECT2, MATID, REALID" "GCGEN - Creates contact elements for general contact.
GCGEN, Option, FeatureANGLE, EdgeKEY, SplitKey, SelOpt" "GENOPT
GENOPT - Specifies \"General options\" as the subsequent status topic." "GEOM - Defines the geometry specifications for the radiation matrix calculation.
GEOM, K2D, NDIV" "GEOMETRY
GEOMETRY - Specifies \"Geometry\" as the subsequent status topic." "GMATRIX - Performs electric field solutions and calculates the self and mutual conductance between multiple conductors.
GMATRIX, SYMFAC, Condname, NUMCOND, --, Matrixname" "GMFACE - Specifies the facet representation used to form solid models.
GMFACE, Lab, N" "GP - Defines a gap condition for transient analyses.
GP, NODE1, NODE2, Lab, STIF, GAP, DAMP" "GPDELE - Deletes gap conditions.
GPDELE, GAP1, GAP2, GINC" "GPLIST - Lists the gap conditions.
GPLIST, GAP1, GAP2, GINC" "GPLOT
GPLOT - Controls general plotting." "GRP - Specifies the grouping mode combination method.
GRP, SIGNIF, Label, , ForceType" "GSBDATA - Specifies the constraints or applies the load at the ending point for generalized plane strain option.
GSBDATA, LabZ, VALUEZ, LabX, VALUEX, LabY, VALUEY" "GSGDATA - Specifies the reference point and defines the geometry in the fiber direction for the generalized plane strain element option.
GSGDATA, LFIBER, XREF, YREF, ROTX0, ROTY0" "GSLIST - When using generalized plane strain, lists the input data or solutions.
GSLIST, Lab" "GSSOL - Specifies which results to store from the results file when using generalized plane strain.
GSSOL, NVAR, Item, Comp, Name" "GSUM
GSUM - Calculates and prints geometry items." "HARFRQ - Defines the frequency range in a harmonic analysis.
HARFRQ, FREQB, FREQE, --, LogOpt, FREQARR, Toler" "HBMAT - Writes an assembled global matrix in Harwell-Boeing format.
HBMAT, Fname, Ext, --, Form, Matrx, Rhs, Mapping" "HELP - Displays help information on ANSYS commands and element types.
HELP, Name" "HELPDISP - Displays help information on DISPLAY program commands.
HELPDISP, Commandname" "HEMIOPT - Specifies options for Hemicube view factor calculation.
HEMIOPT, HRES" "HFANG - Defines or displays spatial angles of a spherical radiation surface for sound radiation parameter calculations.
HFANG, Lab, PHI1, PHI2, THETA1, THETA2" "HFSYM - Indicates the presence of symmetry planes for the computation of acoustic fields in the near and far field domains (beyond the finite element region).
HFSYM, KCN, Xkey, Ykey, Zkey" "HMAGSOLV - Specifies 2-D or axisymmetric harmonic magnetic solution options and initiates the solution.
HMAGSOLV, FREQ, NRAMP, CNVA, CNVV, CNVC, CNVE, NEQIT" "HPGL - Specifies various HP options.
HPGL, Kywrd, Opt1, Opt2" "HPTCREATE - Defines a hard point.
HPTCREATE, TYPE, ENTITY, NHP, LABEL, VAL1, VAL2, VAL3" "HPTDELETE - Deletes selected hardpoints.
HPTDELETE, NP1, NP2, NINC" "HRCPLX - Computes and stores in the database the time-harmonic solution at a prescribed phase angle.
HRCPLX, LOADSTEP, SUBSTEP, OMEGAT, 1STLCASE, 2NDLCASE" "HREXP - Specifies the phase angle for the harmonic analysis expansion pass.
HREXP, ANGLE" "HROCEAN - Perform the harmonic ocean wave procedure (HOWP).
HROCEAN, Type, NPHASE" "HROPT - Specifies harmonic analysis options.
HROPT, Method, MAXMODE, MINMODE, MCout, Damp" "HROUT - Specifies the harmonic analysis output options.
HROUT, Reimky, Clust, Mcont" "IC - Specifies initial conditions at nodes.
IC, NODE, Lab, VALUE, VALUE2, NEND, NINC" "ICDELE
ICDELE - Deletes initial conditions at nodes." "ICLIST - Lists the initial conditions.
ICLIST, NODE1, NODE2, NINC, Lab" "IGESIN - Transfers IGES data from a file into ANSYS.
IGESIN, Fname, Ext, --" "IGESOUT - Writes solid model data to a file in IGES Version 5.1 format.
IGESOUT, Fname, Ext, --, ATT" "IMAGIN - Forms an imaginary variable from a complex variable.
IMAGIN, IR, IA, --, --, Name, --, --, FACTA" "IMESH - Generates nodes and interface elements along lines or areas.
IMESH, LAKY, NSLA, NTLA, KCN, DX, DY, DZ, TOL" "IMMED - Allows immediate display of a model as it is generated.
IMMED, KEY" "IMPD - Calculates the impedance of a conductor at a reference plane.
IMPD, Vpath, Ipath, Vsymm, Isymm" "INISTATE - Defines initial state data and parameters.
INISTATE, Action, Val1, Val2, Val3, Val4, Val5, Val6, Val7, Val8, Val9" "INRES - Identifies the data to be retrieved from the results file.
INRES, Item1, Item2, Item3, Item4, Item5, Item6, Item7, Item8" "INRTIA
INRTIA - Specifies \"Inertial loads\" as the subsequent status topic." "INT1 - Integrates a variable.
INT1, IR, IY, IX, --, Name, --, --, FACTA, FACTB, CONST" "INTSRF - Integrates nodal results on an exterior surface.
INTSRF, Lab" "IOPTN - Controls options relating to importing a model.
IOPTN, Lab, VAL1" "IRLF - Specifies that inertia relief calculations are to be performed.
IRLF, KEY" "IRLIST
IRLIST - Prints inertia relief summary table." "JPEG - Provides JPEG file export for ANSYS displays. 
JPEG, Kywrd, OPT" "JSOL - Specifies result items to be stored for the joint element.
JSOL, NVAR, ELEM, ITEM, COMP, Name" "K - Defines a keypoint.
K, NPT, X, Y, Z" "KATT - Associates attributes with the selected, unmeshed keypoints.
KATT, MAT, REAL, TYPE, ESYS" "KBC - Specifies ramped or stepped loading within a load step.
KBC, KEY" "KBETW - Creates a keypoint between two existing keypoints.
KBETW, KP1, KP2, KPNEW, Type, VALUE" "KCALC - Calculates stress intensity factors in fracture mechanics analyses.
KCALC, KPLAN, MAT, KCSYM, KLOCPR" "KCENTER - Creates a keypoint at the center of a circular arc defined by three locations.
KCENTER, Type, VAL1, VAL2, VAL3, VAL4, KPNEW" "KCLEAR - Deletes nodes and point elements associated with selected keypoints.
KCLEAR, NP1, NP2, NINC" "KDELE - Deletes unmeshed keypoints.
KDELE, NP1, NP2, NINC" "KDIST - Calculates and lists the distance between two keypoints.
KDIST, KP1, KP2" "KEEP - Stores POST26 definitions and data during active session.
KEEP, Key" "KESIZE - Specifies the edge lengths of the elements nearest a keypoint.
KESIZE, NPT, SIZE, FACT1, FACT2" "KEYOPT - Sets element key options.
KEYOPT, ITYPE, KNUM, VALUE" "KEYPTS
KEYPTS - Specifies \"Keypoints\" as the subsequent status topic." "KEYW - Sets a keyword used by the GUI for context filtering (GUI).
KEYW, Keyword, KEY" "KFILL - Generates keypoints between two keypoints.
KFILL, NP1, NP2, NFILL, NSTRT, NINC, SPACE" "KGEN - Generates additional keypoints from a pattern of keypoints.
KGEN, ITIME, NP1, NP2, NINC, DX, DY, DZ, KINC, NOELEM, IMOVE" "KL - Generates a keypoint at a specified location on an existing line.
KL, NL1, RATIO, NK1" "KLIST - Lists the defined keypoints or hard points.
KLIST, NP1, NP2, NINC, Lab" "KMESH - Generates nodes and point elements at keypoints.
KMESH, NP1, NP2, NINC" "KMODIF - Modifies an existing keypoint.
KMODIF, NPT, X, Y, Z" "KMOVE - Calculates and moves a keypoint to an intersection.
KMOVE, NPT, KC1, X1, Y1, Z1, KC2, X2, Y2, Z2" "KNODE - Defines a keypoint at an existing node location.
KNODE, NPT, NODE" "KPLOT - Displays the selected keypoints.
KPLOT, NP1, NP2, NINC, Lab" "KPSCALE - Generates a scaled set of (meshed) keypoints from a pattern of keypoints.
KPSCALE, NP1, NP2, NINC, RX, RY, RZ, KINC, NOELEM, IMOVE" "KREFINE - Refines the mesh around specified keypoints.
KREFINE, NP1, NP2, NINC, LEVEL, DEPTH, POST, RETAIN" "KSCALE - Generates a scaled pattern of keypoints from a given keypoint pattern.
KSCALE, KINC, NP1, NP2, NINC, RX, RY, RZ" "KSCON - Specifies a keypoint about which an area mesh will be skewed.
KSCON, NPT, DELR, KCTIP, NTHET, RRAT" "KSEL - Selects a subset of keypoints or hard points.
KSEL, Type, Item, Comp, VMIN, VMAX, VINC, KABS" "KSLL - Selects those keypoints contained in the selected lines.
KSLL, Type" "KSLN - Selects those keypoints associated with the selected nodes.
KSLN, Type" "KSUM
KSUM - Calculates and prints geometry statistics of the selected keypoints." "KSYMM - Generates a reflected set of keypoints.
KSYMM, Ncomp, NP1, NP2, NINC, KINC, NOELEM, IMOVE" "KTRAN - Transfers a pattern of keypoints to another coordinate system.
KTRAN, KCNTO, NP1, NP2, NINC, KINC, NOELEM, IMOVE" "KUSE - Specifies whether or not to reuse the factorized matrix.
KUSE, KEY" "KWPAVE - Moves the working plane origin to the average location of keypoints.
KWPAVE, P1, P2, P3, P4, P5, P6, P7, P8, P9" "KWPLAN - Defines the working plane using three keypoints.
KWPLAN, WN, KORIG, KXAX, KPLAN" "L - Defines a line between two keypoints.
L, P1, P2, NDIV, SPACE, XV1, YV1, ZV1, XV2, YV2, ZV2" "L2ANG - Generates a line at an angle with two existing lines.
L2ANG, NL1, NL2, ANG1, ANG2, PHIT1, PHIT2" "L2TAN - Generates a line tangent to two lines.
L2TAN, NL1, NL2" "LANG - Generates a straight line at an angle with a line.
LANG, NL1, P3, ANG, PHIT, LOCAT" "LARC - Defines a circular arc.
LARC, P1, P2, PC, RAD" "LAREA - Generates the shortest line between two keypoints on an area.
LAREA, P1, P2, NAREA" "LARGE - Finds the largest (the envelope) of three variables.
LARGE, IR, IA, IB, IC, Name, --, --, FACTA, FACTB, FACTC" "LATT - Associates element attributes with the selected, unmeshed lines.
LATT, MAT, REAL, TYPE, --, KB, KE, SECNUM" "LAYER - Specifies the element layer for which data are to be processed.
LAYER, NUM" "LAYERP26 - Specifies the element layer for which data are to be stored.
LAYERP26, NUM" "LAYLIST - Lists real constants material properties for layered elements.
LAYLIST, IEL, LAYR1, LAYR2, Mplab1, Mplab2" "LAYPLOT - Displays the layer stacking sequence for layered elements.
LAYPLOT, IEL, LAYR1, LAYR2" "LCABS - Specifies absolute values for load case operations.
LCABS, LCNO, KABS" "LCASE - Reads a load case into the database.
LCASE, LCNO" "LCCALC
LCCALC - Specifies \"Load case settings\" as the subsequent status topic." "LCCAT - Concatenates multiple lines into one line for mapped meshing.
LCCAT, NL1, NL2" "LCDEF - Creates a load case from a set of results on a results file.
LCDEF, LCNO, LSTEP, SBSTEP, KIMG" "LCFACT - Defines scale factors for load case operations.
LCFACT, LCNO, FACT" "LCFILE - Creates a load case from an existing load case file.
LCFILE, LCNO, Fname, Ext, --" "LCLEAR - Deletes nodes and line elements associated with selected lines.
LCLEAR, NL1, NL2, NINC" "LCOMB - Combines adjacent lines into one line.
LCOMB, NL1, NL2, KEEP" "LCOPER - Performs load case operations.
LCOPER, Oper, LCASE1, Oper2, LCASE2" "LCSEL - Selects a subset of load cases.
LCSEL, Type, LCMIN, LCMAX, LCINC" "LCSL - Divides intersecting lines at their point(s) of intersection.
LCSL, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LCSUM - Specifies whether to process non-summable items in load case operations.
LCSUM, Lab" "LCWRITE - Creates a load case by writing results to a load case file.
LCWRITE, LCNO, Fname, Ext, --" "LCZERO
LCZERO - Zeroes the results portion of the database." "LDELE - Deletes unmeshed lines.
LDELE, NL1, NL2, NINC, KSWP" "LDIV - Divides a single line into two or more lines.
LDIV, NL1, RATIO, PDIV, NDIV, KEEP" "LDRAG - Generates lines by sweeping a keypoint pattern along  path.
LDRAG, NK1, NK2, NK3, NK4, NK5, NK6, NL1, NL2, NL3, NL4, NL5, NL6" "LDREAD - Reads results from the results file and applies them as loads.
LDREAD, Lab, LSTEP, SBSTEP, TIME, KIMG, Fname, Ext, --" "LESIZE - Specifies the divisions and spacing ratio on unmeshed lines.
LESIZE, NL1, SIZE, ANGSIZ, NDIV, SPACE, KFORC, LAYER1, LAYER2, KYNDIV" "LEXTND - Extends a line at one end by using its slope.
LEXTND, NL1, NK1, DIST, KEEP" "LFILLT - Generates a fillet line between two intersecting lines.
LFILLT, NL1, NL2, RAD, PCENT" "LFSURF - Generates surface elements overlaid on the edge of existing solid elements and assigns the extra node as the closest fluid element node.
LFSURF, SLINE, TLINE" "LGEN - Generates additional lines from a pattern of lines.
LGEN, ITIME, NL1, NL2, NINC, DX, DY, DZ, KINC, NOELEM, IMOVE" "LGLUE - Generates new lines by \"gluing\" lines.
LGLUE, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LGWRITE - Writes the database command log to a file.
LGWRITE, Fname, Ext, --, Kedit" "LINA - Finds the intersection of a line with an area.
LINA, NL, NA" "LINE
LINE - Specifies \"Lines\" as the subsequent status topic." "LINES - Specifies the length of a printed page.
LINES, N" "LINL - Finds the common intersection of lines.
LINL, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LINP - Finds the pairwise intersection of lines.
LINP, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LINV - Finds the intersection of a line with a volume.
LINV, NL, NV" "LIST - Lists out the sets in the results file.
LIST, LEVEL" "LLIST - Lists the defined lines.
LLIST, NL1, NL2, NINC, Lab" "LMATRIX - Calculates an inductance matrix and the total flux linkage for an N-winding coil system.
LMATRIX, SYMFAC, Coilname, Curname, Indname" "LMESH - Generates nodes and line elements along lines.
LMESH, NL1, NL2, NINC" "LNSRCH - Activates a line search to be used with Newton-Raphson.
LNSRCH, Key" "LOCAL - Defines a local coordinate system by a location and orientation.
LOCAL, KCN, KCS, XC, YC, ZC, THXY, THYZ, THZX, PAR1, PAR2" "LOVLAP - Overlaps lines.
LOVLAP, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LPLOT - Displays the selected lines.
LPLOT, NL1, NL2, NINC" "LPTN - Partitions lines.
LPTN, NL1, NL2, NL3, NL4, NL5, NL6, NL7, NL8, NL9" "LREFINE - Refines the mesh around specified lines.
LREFINE, NL1, NL2, NINC, LEVEL, DEPTH, POST, RETAIN" "LREVERSE - Reverses the normal of a line, regardless of its connectivity or mesh status.
LREVERSE, LNUM, NOEFLIP" "LROTAT - Generates circular lines by rotating a keypoint pattern about an axis.
LROTAT, NK1, NK2, NK3, NK4, NK5, NK6, PAX1, PAX2, ARC, NSEG" "LSBA - Subtracts areas from lines.
LSBA, NL, NA, SEPO, KEEPL, KEEPA" "LSBL - Subtracts lines from lines.
LSBL, NL1, NL2, SEPO, KEEP1, KEEP2" "LSBV - Subtracts volumes from lines.
LSBV, NL, NV, SEPO, KEEPL, KEEPV" "LSBW - Subtracts the intersection of the working plane from lines (divides lines).
LSBW, NL, SEPO, KEEP" "LSCLEAR - Clears loads and load step options from the database.
LSCLEAR, Lab" "LSDELE - Deletes load step files.
LSDELE, LSMIN, LSMAX, LSINC" "LSEL - Selects a subset of lines.
LSEL, Type, Item, Comp, VMIN, VMAX, VINC, KSWP" "LSLA - Selects those lines contained in the selected areas.
LSLA, Type" "LSLK - Selects those lines containing the selected keypoints.
LSLK, Type, LSKEY" "LSOPER
LSOPER - Specifies \"Load step operations\" as the subsequent status topic." "LSREAD - Reads load and load step option data into the database.
LSREAD, LSNUM" "LSSCALE - Generates a scaled set of lines from a pattern of lines.
LSSCALE, NL1, NL2, NINC, RX, RY, RZ, KINC, NOELEM, IMOVE" "LSSOLVE - Reads and solves multiple load steps.
LSSOLVE, LSMIN, LSMAX, LSINC" "LSTR - Defines a straight line irrespective of the active coordinate system.
LSTR, P1, P2" "LSUM
LSUM - Calculates and prints geometry statistics of the selected lines." "LSWRITE - Writes load and load step option data to a file.
LSWRITE, LSNUM" "LSYMM - Generates lines from a line pattern by symmetry reflection.
LSYMM, Ncomp, NL1, NL2, NINC, KINC, NOELEM, IMOVE" "LTAN - Generates a line at the end of, and tangent to, an existing line.
LTAN, NL1, P3, XV3, YV3, ZV3" "LTRAN - Transfers a pattern of lines to another coordinate system.
LTRAN, KCNTO, NL1, NL2, NINC, KINC, NOELEM, IMOVE" "LUMPM - Specifies a lumped mass matrix formulation.
LUMPM, Key" "LVSCALE - Scales the load vector for mode-superposition analyses.
LVSCALE, FACT, LDSTEP" "LWPLAN - Defines the working plane normal to a location on a line.
LWPLAN, WN, NL1, RATIO" "M - Defines master degrees of freedom for superelement generation analyses.
M, NODE, Lab1, NEND, NINC, Lab2, Lab3, Lab4, Lab5, Lab6" "MADAPT - Adaptively meshes and solves an edge-based model.
MADAPT, ERRTARGT, NADAPT, NMAX, KPLT, Ksmooth, KLST, KCD, DEVICE" "MAGOPT - Specifies options for a 3-D magnetostatic field analysis.
MAGOPT, Value" "MAGSOLV - Specifies magnetic solution options and initiates the solution.
MAGSOLV, OPT, NRAMP, CNVCSG, CNVFLUX, NEQIT, BIOT,CNVTOL" "MAP - Maps pressures from source points to target surface elements.
MAP, --, kDIM, --, kOUT, LIMIT" "MAP2DTO3D - Initiates a 2-D to 3-D analysis and maps variables.
MAP2DTO3D, Action, LDSTEP, SBSTEP, OPTION" "MAPSOLVE - Maps solved node and element solutions from an original mesh to a new mesh.
MAPSOLVE, MAXSBSTEP" "MAPVAR - Defines tensors and vectors in user-defined state variables for rezoning and in 2-D to 3-D analyses.
MAPVAR, Option, MatId, IstrtStress, nTenStress, IstrtStrain, nTenStrain, , IstrtVect, nVect" "MASTER
MASTER - Specifies \"Master DOF\" as the subsequent status topic." "MAT - Sets the element material attribute pointer.
MAT, MAT" "MATER
MATER - Specifies \"Material properties\" as the subsequent status topic." "MCHECK - Checks mesh connectivity.
MCHECK, Lab" "MDAMP - Defines the damping ratios as a function of mode.
MDAMP, STLOC, V1, V2, V3, V4, V5, V6" "MDELE - Deletes master degrees of freedom.
MDELE, NODE, Lab1, NEND, NINC, Lab2, Lab3, Lab4, Lab5, Lab6" "MDPLOT - Plots frequency-dependent modal damping coefficients calculated by DMPEXT.
MDPLOT, Function, Dmpname, Scale" "MEMM - Allows the current session to keep allocated memory 
MEMM, Lab, Kywrd" "MESHING
MESHING - Specifies \"Meshing\" as the subsequent status topic." "MFANALYSIS - Activates or deactivates an ANSYS Multi-field solver analysis.
MFANALYSIS, Key" "MFBUCKET - Turns a bucket search on or off.
MFBUCKET, Key, Value" "MFCALC - Specifies a calculation frequency for a field in an ANSYS Multi-field solver analysis.
MFCALC, FNUMB, FREQ" "MFCI - Sets the control parameters used by the conservative (CPP) interpolation scheme.
MFCI, VAL1, VAL2" "MFCLEAR - Deletes ANSYS Multi-field solver analysis settings.
MFCLEAR, Option, Value" "MFCMMAND - Captures field solution options in a command file.
MFCMMAND, FNUMB, Fname, Ext" "MFCONV - Sets convergence values for an ANSYS Multi-field solver analysis.
MFCONV, Lab, TOLER, --, MINREF" "MFDTIME - Sets time step sizes for an ANSYS Multi-field solver analysis.
MFDTIME, DTIME, DTMIN, DTMAX, Carry" "MFELEM - Defines a field by grouping element types.
MFELEM, FNUMB, ITYPE1, ITYPE2, ITYPE3, ITYPE4, ITYPE5, ITYPE6, ITYPE7, ITYPE8, ITYPE9, ITYPE10" "MFEM - Add more element types to a previously defined field number.
MFEM, FNUMB, ITYPE1, ITYPE2, ITYPE3, ITYPE4, ITYPE5, ITYPE6, ITYPE7, ITYPE8, ITYPE9, ITYPE10" "MFEXTER - Defines external fields for an ANSYS Multi-field solver analysis. 
MFEXTER, FNUMB1, FNUMB2, FNUMB3, FNUMB4, FNUMB5, FNUMB6, FNUMB7, FNUMB8, FNUMB9, FNUMB10, FNUMB11, FNUMB12, FNUMB13, FNUMB14, FNUMB15, FNUMB16, FNUMB17, FNUMB18, FNUMB19, FNUMB20" "MFFNAME - Specifies a file name for a field in an ANSYS Multi-field solver analysis.
MFFNAME, FNUMB, Fname" "MFFR - Setup Multi-Field relaxation factors for field solutions.
MFFR, Fname, Lab, RFINI, RFMIN, RFMAX" "MFIMPORT - Imports a new field into a current ANSYS Multi-field solver analysis.
MFIMPORT, FNUMB, Option, Fname, Ext" "MFINTER - Specifies the interface load transfer interpolation option for an ANSYS Multi-field solver analysis.
MFINTER, Option" "MFITER - Sets the number of stagger iterations for an ANSYS Multi-field solver analysis.
MFITER, MAXITER, MINITER, TARGET" "MFLCOMM - Defines a load transfer for code coupling analyses.
MFLCOMM, Type, Fname1, Intname1, Label1, Fname2, Intname2, Label2, Option" "MFLIST - Lists the settings for an ANSYS Multi-field solver analysis.
MFLIST, Option, Value" "MFMAP - Calculates, saves, resumes, or deletes mapping data in an ANSYS Multi-field solver analysis.
MFMAP, Lab1, Lab2, Filename, Opt" "MFORDER - Specifies field solution order for an ANSYS Multi-field solver analysis.
MFORDER, FNUMB1, FNUMB2, FNUMB3, FNUMB4, FNUMB5, FNUMB6, FNUMB7, FNUMB8, FNUMB9, FNUMB10, FNUMB11, FNUMB12, FNUMB13, FNUMB14, FNUMB15, FNUMB16, FNUMB17, FNUMB18, FNUMB19, FNUMB20" "MFOUTPUT - Specifies results file output frequency for an ANSYS Multi-field solver analysis.
MFOUTPUT, FREQ" "MFPSIMUL - Sets up a field solver group to simultaneously process with code coupling analyses.
MFPSIMUL, gname, Fname1, Fname2" "MFRC - Controls file writing for multiframe restarts for the ANSYS Multi-field solver.
MFRC, FREQ, MAXFILES" "MFRELAX - Sets relaxation values for an ANSYS Multi-field solver analysis.
MFRELAX, Lab, VALUE, Option" "MFRSTART - Specifies restart status for an ANSYS Multi-field solver analysis.
MFRSTART, TIME" "MFSORDER - Sets up the solution sequence of simultaneous field solver groups for code coupling analyses.
MFSORDER, gname1, gname2" "MFSURFACE - Defines a surface load transfer for an ANSYS Multi-field solver analysis.
MFSURFACE, INUMB, FNUMB1, Label, FNUMB2" "MFTIME - Sets end time for an ANSYS Multi-field solver analysis.
MFTIME, TIME" "MFTOL - Activates or deactivates normal distance checking for surface mapping in an ANSYS Multi-field solver analysis.
MFTOL, Key, Value, Toler" "MFVOLUME - Defines a volume load transfer for an ANSYS Multi-field solver analysis.
MFVOLUME, INUMB, FNUMB1, Label, FNUMB2" "MFWRITE - Writes an ANSYS master input file for MFX multiple code coupling. 
MFWRITE, Fname, Ext" "MGEN - Generates additional MDOF from a previously defined set.
MGEN, ITIME, INC, NODE1, NODE2, NINC" "MIDTOL - Sets midstep residual criterion values for structural transient analyses.
MIDTOL, KEY, TOLERB, RESFQ" "MLIST - Lists the MDOF of freedom.
MLIST, NODE1, NODE2, NINC" "MMASS - Specifies the missing mass response calculation.
MMASS, Option, ZPA" "MMF
MMF - Calculates the magnetomotive force along a path." "MODCONT - Specify additional modal analysis options. 
MODCONT, MLSkey, EnforcedKey" "MODE - Specifies the harmonic loading term for this load step.
MODE, MODE, ISYM" "MODIFY - Changes the listed values of the data in a set.
MODIFY, SET, LSTEP, ITER, CUMIT, TIME, Ktitle" "MODMSH - Controls the relationship of the solid model and the FE model.
MODMSH, Lab" "MODOPT - Specifies modal analysis options.
MODOPT, Method, NMODE, FREQB, FREQE, Cpxmod, Nrmkey, ModType, BlockSize, --, --, Scalekey" "MODSELOPTION - Specifies the criteria for selecting the modes to be expanded.
MODSELOPTION, dir1, dir2, dir3, dir4, dir5, dir6" "MONITOR - Controls contents of three variable fields in nonlinear solution monitor file.
MONITOR, VAR, Node, Lab" "MOPT - Specifies meshing options.
MOPT, Lab, Value" "MORPH - Specifies morphing and remeshing controls.
MORPH, Option, --, Remeshopt, ElemSet, ARMAX, VOCH, ARCH, STEP, TIME, StrOpt" "MOVE - Calculates and moves a node to an intersection.
MOVE, NODE, KC1, X1, Y1, Z1, KC2, X2, Y2, Z2" "MP - Defines a linear material property as a constant or a function of temperature.
MP, Lab, MAT, C0, C1, C2, C3, C4" "MPAMOD - Modifies temperature-dependent secant coefficients of thermal expansion.
MPAMOD, MAT, DEFTEMP" "MPCHG - Changes the material number attribute of an element.
MPCHG, MAT, ELEM" "MPCOPY - Copies linear material model data from one material reference number to another.
MPCOPY, --, MATF, MATT" "MPDATA - Defines property data to be associated with the temperature table.
MPDATA, Lab, MAT, SLOC, C1, C2, C3, C4, C5, C6" "MPDELE - Deletes linear material properties.
MPDELE, Lab, MAT1, MAT2, INC, LCHK" "MPDRES - Reassembles existing material data with the temperature table.
MPDRES, LabF, MATF, LabT, MATT" "MPLIST - Lists linear material properties.
MPLIST, MAT1, MAT2, INC, Lab, TEVL" "MPPLOT - Plots linear material properties as a function of temperature.
MPPLOT, Lab, MAT, TMIN, TMAX, PMIN, PMAX" "MPREAD - Reads a file containing material properties.
MPREAD, Fname, Ext, --, LIB" "MPRINT - Specifies that radiation matrices are to be printed.
MPRINT, KEY" "MPTEMP - Defines a temperature table for material properties.
MPTEMP, SLOC, T1, T2, T3, T4, T5, T6" "MPTGEN - Adds temperatures to the temperature table by generation.
MPTGEN, STLOC, NUM, TSTRT, TINC" "MPTRES - Restores a temperature table previously defined.
MPTRES, Lab, MAT" "MPWRITE - Writes linear material properties in the database to a file (if the LIB option is not specified) or writes both linear and nonlinear material properties (if LIB is specified) from the database to a file.
MPWRITE, Fname, Ext, --, LIB, MAT" "MSAVE - Sets the solver memory saving option. This option only applies to the PCG solver (including PCG Lanczos).
MSAVE, Key" "MSHAPE - For elements that support multiple shapes, specifies the element shape to be used for meshing.
MSHAPE, KEY, Dimension" "MSHCOPY - Simplifies the generation of meshes that have matching node element patterns on two different line groups (in 2-D) or area groups (3-D).
MSHCOPY, KEYLA, LAPTRN, LACOPY, KCN, DX, DY, DZ, TOL, LOW, HIGH" "MSHKEY - Specifies whether free meshing or mapped meshing should be used to mesh a model.
MSHKEY, KEY" "MSHMID - Specifies placement of midside nodes.
MSHMID, KEY" "MSHPATTERN - Specifies pattern to be used for mapped triangle meshing.
MSHPATTERN, KEY" "MSOLVE - Starts multiple solutions for random acoustics analysis with diffuse sound field.
MSOLVE, NUMSLV, NRMTOL, NRMCHKINC" "MSTOLE - Adds two extra nodes from FLUID116 elements to SURF151 or SURF152 elements for convection analyses.
MSTOLE, METHOD, Namesurf, Namefluid" "MXPAND - Specifies the number of modes to expand and write for a modal or buckling analysis.
MXPAND, NMODE, FREQB, FREQE, Elcalc, SIGNIF, MSUPkey, ModeSelMethod" "N - Defines a node.
N, NODE, X, Y, Z, THXY, THYZ, THZX" "NANG - Rotates a nodal coordinate system by direction cosines.
NANG, NODE, X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3" "NAXIS - Generates nodes for general axisymmetric element sections.
NAXIS, Action, Val" "NCNV - Sets the key to terminate an analysis.
NCNV, KSTOP, DLIM, ITLIM, ETLIM, CPLIM" "NDELE - Deletes nodes.
NDELE, NODE1, NODE2, NINC" "NDIST - Calculates and lists the distance between two nodes.
NDIST, ND1, ND2" "NDSURF - Generates surface elements overlaid on the edge of existing elements and assigns the extra node as the closest fluid element node.
NDSURF, Snode, Telem, DIMN" "NEQIT - Specifies the maximum number of equilibrium iterations for nonlinear analyses.
NEQIT, NEQIT, FORCEkey" "NFORCE - Sums the nodal forces and moments of elements attached to nodes.
NFORCE, ITEM" "NGEN - Generates additional nodes from a pattern of nodes.
NGEN, ITIME, INC, NODE1, NODE2, NINC, DX, DY, DZ, SPACE" "NKPT - Defines a node at an existing keypoint location.
NKPT, NODE, NPT" "NLADAPTIVE - Defines the criteria under which the mesh is refined or modified during a nonlinear solution.
NLADAPTIVE, Component, Action, Criterion, Option, VAL1, VAL2, VAL3" "NLDIAG - Sets nonlinear diagnostics functionality.
NLDIAG, Label, Key, MAXFILE" "NLDPOST - Gets element component information from nonlinear diagnostic files.
NLDPOST, Label, Key, FileID, Prefix" "NLGEOM - Includes large-deflection effects in a static or full transient analysis.
NLGEOM, Key" "NLHIST - Specify result items to track during solution.
NLHIST, Key, Name, Item, Comp, NODE, ELEM, SHELL, LAYER, STOP_VALUE, STOP_COND" "NLIST - Lists nodes.
NLIST, NODE1, NODE2, NINC, Lcoord, SORT1, SORT2, SORT3, KINTERNAL" "NLMESH - Controls remeshing in nonlinear adaptivity.
NLMESH, Control, VAL1, VAL2, VAL3, VAL4" "NLOG - Forms the natural log of a variable.
NLOG, IR, IA, --, --, Name, --, --, FACTA, FACTB" "NLOPT
NLOPT - Specifies \"Nonlinear analysis options\" as the subsequent status topic." "NMODIF - Modifies an existing node.
NMODIF, NODE, X, Y, Z, THXY, THYZ, THZX" "NOCOLOR - Removes color from graphics displays.
NOCOLOR, KEY" "NODES
NODES - Specifies \"Nodes\" as the subsequent status topic." "NOOFFSET - Prevents the CDREAD command from offsetting specified data items
NOOFFSET, Label" "NOORDER - Re-establishes the original element ordering.
NOORDER, Lab" "NORA - Rotates nodal coordinate systems to surface normal
NORA, AREA, NDIR" "NORL - Rotates nodal coordinate systems perpendicular to line normal
NORL, LINE, AREA, NDIR" "NPLOT - Displays nodes.
NPLOT, KNUM" "NPRINT - Defines which time points stored are to be listed.
NPRINT, N" "NREAD - Reads nodes from a file.
NREAD, Fname, Ext, --" "NREFINE - Refines the mesh around specified nodes.
NREFINE, NN1, NN2, NINC, LEVEL, DEPTH, POST, RETAIN" "NRLSUM - Specifies the Naval Research Laboratory (NRL) sum mode combination method.
NRLSUM, SIGNIF, Label, LabelCSM, ForceType" "NROPT - Specifies the Newton-Raphson options in a static or full transient analysis.
NROPT, Option1, Option2, Optval" "NROTAT - Rotates nodal coordinate systems into the active system.
NROTAT, NODE1, NODE2, NINC" "NRRANG - Specifies the range of nodes to be read from the node file.
NRRANG, NMIN, NMAX, NINC" "NSCALE - Generates a scaled set of nodes from a pattern of nodes.
NSCALE, INC, NODE1, NODE2, NINC, RX, RY, RZ" "NSEL - Selects a subset of nodes.
NSEL, Type, Item, Comp, VMIN, VMAX, VINC, KABS" "NSLA - Selects those nodes associated with the selected areas.
NSLA, Type, NKEY" "NSLE - Selects those nodes attached to the selected elements.
NSLE, Type, NodeType, Num" "NSLK - Selects those nodes associated with the selected keypoints.
NSLK, Type" "NSLL - Selects those nodes associated with the selected lines.
NSLL, Type, NKEY" "NSLV - Selects those nodes associated with the selected volumes.
NSLV, Type, NKEY" "NSMOOTH - Smooths selected nodes among selected elements.
NSMOOTH, NPASS" "NSOL - Specifies nodal data to be stored from the results file.
NSOL, NVAR, NODE, Item, Comp, Name, SECTOR" "NSORT - Sorts nodal data.
NSORT, Item, Comp, ORDER, KABS, NUMB, SEL" "NSTORE - Defines which time points are to be stored.
NSTORE, TINC" "NSUBST - Specifies the number of substeps to be taken this load step.
NSUBST, NSBSTP, NSBMX, NSBMN, Carry" "NSVR - Defines the number of variables for user-programmable element options.
NSVR, ITYPE, NSTV" "NSYM - Generates a reflected set of nodes.
NSYM, Ncomp, INC, NODE1, NODE2, NINC" "NUMCMP - Compresses the numbering of defined items.
NUMCMP, Label" "NUMEXP - Specifies solutions to be expanded from mode-superposition analyses or substructure analyses.
NUMEXP, NUM, BEGRNG, ENDRNG, Elcalc" "NUMMRG - Merges coincident or equivalently defined items.
NUMMRG, Label, TOLER, GTOLER, Action, Switch" "NUMOFF - Adds a number offset to defined items.
NUMOFF, Label, VALUE" "NUMSTR - Establishes starting numbers for automatically numbered items.
NUMSTR, Label, VALUE" "NUMVAR - Specifies the number of variables allowed in POST26.
NUMVAR, NV" "NUSORT
NUSORT - Restores original order for nodal data." "NWPAVE - Moves the working plane origin to the average location of nodes.
NWPAVE, N1, N2, N3, N4, N5, N6, N7, N8, N9" "NWPLAN - Defines the working plane using three nodes.
NWPLAN, WN, NORIG, NXAX, NPLAN" "NWRITE - Writes nodes to a file.
NWRITE, Fname, Ext, --, KAPPND" "OCDATA - Defines an ocean load using non-table data.
OCDATA, VAL1, VAL2, VAL3, . . . , VAL14" "OCDELETE - Deletes a previously defined ocean load.
OCDELETE, DataType, ZoneName" "OCLIST - Summarizes all currently defined ocean loads.
OCLIST, DataType, ZoneName" "OCREAD - Reads externally defined ocean data.
OCREAD, Fname, Ext, --, Option" "OCTABLE - Defines an ocean load using table data.
OCTABLE, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7" "OCTYPE - Specifies the type of ocean load data to follow.
OCTYPE, DataType, Name" "OCZONE - Specifies the type of ocean zone data to follow.
OCZONE, ZoneType, ZoneName, CompNameInt, CompNameExt" "OMEGA - Specifies the rotational velocity of the structure.
OMEGA, OMEGX, OMEGY, OMEGZ" "OPERATE
OPERATE - Specifies \"Operation data\" as the subsequent status topic." "OPNCONTROL - Sets decision parameter for automatically increasing the time step interval.
OPNCONTROL, Lab, VALUE, NUMSTEP" "OUTAERO - Outputs the superelement matrices and load vectors to formatted files for aeroelastic analysis.
OUTAERO, SENAME, TIMEB, DTIME" "OUTOPT
OUTOPT - Specifies \"Output options\" as the subsequent status topic." "OUTPR - Controls the solution printout.
OUTPR, Item, Freq, Cname" "OUTRES - Controls the solution data written to the database.
OUTRES, Item, Freq, Cname, -- , NSVAR, DSUBres" "OVCHECK - Checks for overconstraint among constraint equations and Lagrange multipliers.
OVCHECK, Method, Frequency, Set" "PADELE - Deletes a defined path.
PADELE, DELOPT" "PAGET - Writes current path information into an array variable.
PAGET, PARRAY, POPT" "PAPUT - Retrieves path information from an array variable.
PAPUT, PARRAY, POPT" "PARESU - Restores previously saved paths from a file.
PARESU, Lab, Fname, Ext, --" "PARRES - Reads parameters from a file.
PARRES, Lab, Fname, Ext, --" "PARSAV - Writes parameters to a file.
PARSAV, Lab, Fname, Ext, --" "PARTSEL - Selects a subset of parts in an explicit dynamic analysis.
PARTSEL, Type, PMIN, PMAX, PINC" "PASAVE - Saves selected paths to an external file.
PASAVE, Lab, Fname, Ext, --" "PATH - Defines a path name and establishes parameters for the path.
PATH, NAME, nPts, nSets, nDiv" "PAUSE
PAUSE - Temporarily releases the current product license." "PCALC - Forms additional labeled path items by operating on existing path items.
PCALC, Oper, LabR, Lab1, Lab2, FACT1, FACT2, CONST" "PCGOPT - Controls PCG solver options.
PCGOPT, Lev_Diff , --, ReduceIO, StrmCk, Wrtfull, Memory, LM_Key" "PCIRC - Creates a circular area centered about the working plane origin.
PCIRC, RAD1, RAD2, THETA1, THETA2" "PCROSS - Calculates the cross product of two path vectors along the current path.
PCROSS, LabXR, LabYR, LabZR, LabX1, LabY1, LabZ1, LabX2, LabY2, LabZ2" "PDANL - Defines the analysis file to be used for probabilistic looping.
PDANL, Fname, Ext, --" "PDCDF - Plots the cumulative distribution function.
PDCDF, Rlab, Name, Type, CONF, NMAX" "PDCFLD - Calculates a correlation field and stores it into an ANSYS array.
PDCFLD, ParR, Entity, Ctype, CLENGTH" "PDCLR - Clears the probabilistic design database.
PDCLR, Type" "PDCMAT - Prints the correlation coefficient matrix.
PDCMAT, Rlab, Matrix, Name1, Name2, Corr, SLEVEL, Popt" "PDCORR - Specifies the correlation between two random input variables.
PDCORR, Name1, Name2, CORR" "PDDMCS - Specifies options for Monte Carlo Simulations using direct sampling.
PDDMCS, NSIM, --, Astop, ACCMEAN, ACCSTDEV, CHECK, Seed" "PDDOEL - Defines design of experiment levels for an individual random input variable.
PDDOEL, Name, Method, Vtype, Lopt, VAL1, VAL2, VAL3, VAL4, VAL5" "PDEF - Interpolates an item onto a path.
PDEF, Lab, Item, Comp, Avglab" "PDEXE - Executes the probabilistic analysis.
PDEXE, Slab, MRUN, NFAIL, FOPT, Fname" "PDHIST - Plots the frequency histogram.
PDHIST, Rlab, Name, NCL, Type" "PDINQR - Evaluates statistical characteristics of a random input variable.
PDINQR, Rpar, Name, Type, VAL" "PDLHS - Specifies options for Monte Carlo Simulations using Latin-Hypercube sampling.
PDLHS, NSIM, NREP, ISopt, --, Astop, ACCMEAN, ACCSTDV, CHECK, Seed" "PDMETH - Specifies the probabilistic analysis method.
PDMETH, Method, Samp" "PDOT - Calculates the dot product of two path vectors along the current path.
PDOT, LabR, LabX1, LabY1, LabZ1, LabX2, LabY2, LabZ2" "PDPINV - Prints the result of the inversion of a probability.
PDPINV, Rlab, Name, PROB, --, CONF" "PDPLOT - Plots the distribution curves of a defined random input variable.
PDPLOT, Name, PLOW, PUP" "PDPROB - Prints a probability result.
PDPROB, Rlab, Name, Relation, LIMIT, --, CONF" "PDRESU - Reads the probabilistic model data and loads it into the database.
PDRESU, Fname, Ext, --" "PDROPT - Specifies the options for an HTML report.
PDROPT, RVAR, CORR, STAT, SHIS, HIST, CDF, SENS, CMAT, CONF" "PDSAVE - Writes the probabilistic model data to a file.
PDSAVE, Fname, Ext, --" "PDSCAT - Plots a scatter graph.
PDSCAT, Rlab, Name1, Name2, Type, ORDER, NMAX" "PDSENS - Plots the probabilistic sensitivities.
PDSENS, Rlab, Name, Chart, Type, SLEVEL" "PDSHIS - Plots the sample history values.
PDSHIS, Rlab, Name, Type, CONF" "PDUSER - Specifies options for user-specified sampling methods.
PDUSER, Fname, Ext, --" "PDVAR - Specifies the parameters to be treated as probabilistic design variables.
PDVAR, Name, Type, PAR1, PAR2, PAR3, PAR4" "PDWRITE - Generates an HTML report for the probabilistic analyses.
PDWRITE, File, Fnam, Lnam" "PERBC2D - Generates periodic constraints for 2-D planar magnetic field analyses.
PERBC2D, LOC1, LOC2, LOCTOL, R1, R2, TOLR, OPT, PLNOPT" "PERTURB - Sets linear perturbation analysis options.
PERTURB, Type, MatKey, ContKey, LoadControl" "PFACT - Calculates participation factors for the PSD or multi-point response spectrum table.
PFACT, TBLNO, Excit, Parcor" "PHYSICS - Writes, reads, or lists all element information
PHYSICS, Option, Title, Fname, Ext, --" "PIVCHECK - Controls the behavior of an analysis when a negative or zero equation solver pivot value is encountered.
PIVCHECK, KEY, PRNTCNTRL" "PLCAMP - Plots Campbell diagram data for applications involving rotating structure dynamics.
PLCAMP, Option, SLOPE, UNIT, FREQB, Cname, STABVAL, KeyAllFreq, KeyNegFreq" "PLCFREQ - Plots the frequency response for the given CYCSPEC specification.
PLCFREQ, SPEC, SECTbeg, SECTend" "PLCHIST - Plots a histogram of the frequency response of each sector for the given CYCSPEC specification.
PLCHIST, SPEC, FREQpt" "PLCINT - Plots the fracture parameter (CINT) result data.
PLCINT, ACTION, ID, Node, Cont, Dtype" "PLCPLX - Specifies the part of a complex variable to display.
PLCPLX, KEY" "PLCRACK - Displays cracking and crushing locations in SOLID65 elements.
PLCRACK, LOC, NUM" "PLDISP - Displays the displaced structure.
PLDISP, KUND" "PLESOL - Displays the solution results as discontinuous element contours.
PLESOL, Item, Comp, KUND, Fact" "PLETAB - Displays element table items.
PLETAB, Itlab, Avglab" "PLF2D - Generates a contour line plot of equipotentials.
PLF2D, NCONT, OLAY, ANUM, WIN" "PLFAR - Plots pressure far fields and far field parameters. 
PLFAR, Lab, Option, PHI1, PHI2, NPH1, THETA1, THETA2, NTHETA, VAL1, VAL2, VAL3" "PLGEOM - Plots target and source geometries.
PLGEOM, Item, NODEkey" "PLLS - Displays element table items as contoured areas along elements.
PLLS, LabI, LabJ, Fact, KUND,ViewUP" "PLMAP - Plots target and source pressures.
PLMAP, Item, --, NODEkey, ImagKey" "PLMC - Plots the modal coordinates from a mode-superposition solution.
PLMC, LSTEP, SBSTEP, TIMFRQ, KIMG, HIbeg, HIend" "PLNEAR - Plots the pressure in the near zone exterior to the equivalent source surface.
PLNEAR, Lab, Opt, KCN, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8,VAL9" "PLNSOL - Displays results as continuous contours.
PLNSOL, Item, Comp, KUND, Fact, FileID" "PLORB
PLORB - Displays the orbital motion of a rotating structure" "PLOT - Forms a display.
PLOT, NSTRT, NEND, NINC" "PLOTTING
PLOTTING - Specifies \"Plotting settings\" as the subsequent status topic." "PLPAGM - Displays path items along the path geometry.
PLPAGM, Item, Gscale, Nopt" "PLPATH - Displays path items on a graph.
PLPATH, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6" "PLSECT - Displays membrane and membrane-plus-bending linearized stresses.
PLSECT, Item, Comp, RHO, KBR" "PLST - Plots sound power parameters vs. frequency, or postprocesses results for a random acoustics analysis with diffuse sound field.
PLST, Fname, Ext, ParmPlot, MSLVSTEP" "PLTIME - Defines the time range for which data are to be displayed.
PLTIME, TMIN, TMAX" "PLTRAC - Displays a particle flow or charged particle trace on an element display.
PLTRAC, Analopt, Item, Comp, TRPNum, Name, MXLOOP, TOLER, OPTION, ESCL, MSCL" "PLVAR - Displays up to ten variables in the form of a graph.
PLVAR, NVAR1, NVAR2, NVAR3, NVAR4, NVAR5, NVAR6, NVAR7, NVAR8, NVAR9, NVAR10" "PLVECT - Displays results as vectors.
PLVECT, Item, Lab2, Lab3, LabP, Mode, Loc, Edge, KUND" "PLZZ - Plots the interference diagram from a cyclic modal analysis.
PLZZ, RotVel, DeltaRotVel" "PMAP - Creates mapping of the path geometry by defining path interpolation division points.
PMAP, FORM, DISCON" "PMGTRAN - Summarizes electromagnetic results from a transient analysis.
PMGTRAN, Fname, FREQ, Fcnam1, Fcnam2, Pcnam1, Pcnam2, Ecnam1, Ccnam1" "PMLOPT - Defines perfectly matched layers (PMLs) for acoustic and structural analyses.
PMLOPT, ESYS, Lab, Xminus, Xplus, Yminus, Yplus, Zminus, Zplus" "PMLSIZE - Determines number of PML layers.
PMLSIZE, FREQB, FREQE, DMIN, DMAX, THICK, ANGLE" "PNGR - Provides PNG file export for ANSYS displays. 
PNGR, Kywrd, OPT, VAL" "POINT
POINT - Specifies \"Point flow tracing settings\" as the subsequent status topic." "POLY
POLY - Creates a polygonal area based on working plane coordinate pairs." "POWERH
POWERH - Calculates the rms power loss in a conductor or lossy dielectric." "PPATH - Defines a path by picking or defining nodes, or locations on the currently active working plane, or by entering specific coordinate locations.
PPATH, POINT, NODE, X, Y, Z, CS" "PRANGE - Determines the path range.
PRANGE, LINC, VMIN, VMAX, XVAR" "PRAS - Calculates a specified acoustic quantity on the selected exterior surface or the frequency-band sound pressure level (SPL).
PRAS, Quantity, LOADSTEP, SUBSTEP" "PRCAMP - Prints Campbell diagram data for applications involving rotating structure dynamics.
PRCAMP, Option, SLOPE, UNIT, FREQB, Cname, STABVAL, KeyALLFreq, KeyNegFreq" "PRCINT - Lists the fracture parameter (CINT) results data.
PRCINT, ID, Node, Dtype" "PRCPLX - Defines the output form for complex variables.
PRCPLX, KEY" "PRED - Activates a predictor in a nonlinear analysis.
PRED, Sskey, --, Lskey" "PRENERGY - Prints the total energies of a model or the energies of the specified components. 
PRENERGY, EnergyType, Cname1, Cname2, Cname3, Cname4, Cname5, Cname6" "PRERR
PRERR - Prints SEPC and TEPC." "PRESOL - Prints the solution results for elements.
PRESOL, Item, Comp" "PRETAB - Prints the element table items.
PRETAB, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6, Lab7, Lab8, Lab9" "PRFAR - Prints pressure far fields and far field parameters. 
PRFAR, Lab, Option, PHI1, PHI2, NPH1, THETA1, THETA2, NTHETA, VAL1, VAL2, VAL3" "PRI2 - Creates a polygonal area or a prism volume by vertices (GUI).
PRI2, P51X, Z1, Z2" "PRIM
PRIM - Specifies \"Solid model primitives\" as the subsequent status topic." "PRINT
PRINT - Specifies \"Print settings\" as the subsequent status topic." "PRISM - Creates a prism volume based on working plane coordinate pairs.
PRISM, Z1, Z2" "PRITER
PRITER - Prints solution summary data." "PRJSOL - Prints joint element output.
PRJSOL, Item, Comp" "PRNEAR - Prints the pressure in the near zone exterior to the equivalent source surface.
PRNEAR, Lab, Opt, KCN, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8,VAL9" "PRNLD - Prints the summed element nodal loads.
PRNLD, Lab, TOL, Item" "PRNSOL - Prints nodal solution results.
PRNSOL, Item, Comp" "PROD - Multiplies variables.
PROD, IR, IA, IB, IC, Name, --, --, FACTA, FACTB, FACTC" "PRORB
PRORB - Prints the orbital motion characteristics of a rotating structure" "PRPATH - Prints path items along a geometry path.
PRPATH, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6" "PRRFOR - Prints the constrained node reaction solution. Used with the FORCE command.
PRRFOR, Lab" "PRRSOL - Prints the constrained node reaction solution.
PRRSOL, Lab" "PRSCONTROL - Specifies whether to include pressure load stiffness in the element stiffness formation.
PRSCONTROL, Key" "PRSECT - Calculates and prints linearized stresses along a section path.
PRSECT, RHO, KBR" "PRTIME - Defines the time range for which data are to be listed.
PRTIME, TMIN, TMAX" "PRVAR - Lists variables vs. time (or frequency).
PRVAR, NVAR1, NVAR2, NVAR3, NVAR4, NVAR5, NVAR6" "PRVECT - Prints results as vector magnitude and direction cosines.
PRVECT, Item, Lab2, Lab3, LabP" "PSCONTROL - Enables or disables shared-memory parallel operations.
PSCONTROL, Option, Key" "PSCR - Specifies various PostScript options.
PSCR, Kywrd, KEY" "PSDCOM - Specifies the power spectral density mode combination method.
PSDCOM, SIGNIF, COMODE, , ForceType" "PSDFRQ - Defines the frequency points for the input spectrum tables PSDVAL vs. PSDFRQ for PSD analysis.
PSDFRQ, TBLNO1, TBLNO2, FREQ1, FREQ2, FREQ3, FREQ4, FREQ5, FREQ6, FREQ7" "PSDGRAPH - Displays input PSD curves
PSDGRAPH, TBLNO1, TBLNO2" "PSDRES - Controls solution output written to the results file from a PSD analysis.
PSDRES, Lab, RelKey" "PSDSPL - Defines a partially correlated excitation in a PSD analysis.
PSDSPL, TBLNO, RMIN, RMAX" "PSDUNIT - Defines the type of input PSD.
PSDUNIT, TBLNO, Type, GVALUE" "PSDVAL - Defines PSD values.
PSDVAL, TBLNO, SV1, SV2, SV3, SV4, SV5, SV6, SV7" "PSDWAV - Defines a wave propagation excitation in a PSD analysis.
PSDWAV, TBLNO, VX, VY, VZ" "PSEL - Selects a path or paths.
PSEL, Type, Pname1, Pname2, Pname3, Pname4, Pname5, Pname6, Pname7, Pname8, Pname9, Pname10" "PSMAT - Writes an assembled global matrix to a postscript format that graphically displays nonzero matrix values.
PSMAT, Fname, Ext, Matrix, Color" "PSMESH - Create and mesh a pretension section
PSMESH, SECID, Name, P0, Egroup, NUM, KCN, KDIR, VALUE, NDPLANE, PSTOL, PSTYPE, ECOMP, NCOMP" "PSTRES - Specifies whether prestress effects are calculated or included.
PSTRES, Key" "PTR - Dumps the record of a binary file.
PTR, LOC, BASE" "PTXY - Defines coordinate pairs for use in polygons and prisms.
PTXY, X1, Y1, X2, Y2, X3, Y3, X4, Y4" "PVECT - Interpolates a set of items onto a path.
PVECT, Oper, LabXR, LabYR, LabZR" "QDVAL - Defines PSD quadspectral values.
QDVAL, TBLNO1, TBLNO2, SV1, SV2, SV3, SV4, SV5, SV6, SV7" "QRDOPT - Specifies additional QRDAMP modal analysis options.
QRDOPT, ReuseKey,--,--,SymMeth,CMCCoutKey" "QSOPT - Specifies quasi static radiation options.
QSOPT, Opt" "QUAD - Generates a quadratic line of nodes from three nodes.
QUAD, NODE1, NINTR, NODE2, NFILL, NSTRT, NINC, PKFAC" "QUOT - Divides two variables.
QUOT, IR, IA, IB, --, Name, --, --, FACTA, FACTB" "R - Defines the element real constants.
R, NSET, R1, R2, R3, R4, R5, R6" "RACE - Defines a \"racetrack\" current source. 
RACE, XC, YC, RAD, TCUR, DY, DZ, --, --, Cname" "RADOPT - Specifies Radiosity Solver options.
RADOPT, --, FLUXTOL, SOLVER, MAXITER, TOLER, OVERRLEX, --, --, --, --, MAXFLUXITER" "RAPPND - Appends results data from the database to the results file. 
RAPPND, LSTEP, TIME" "RATE - Specifies whether the effect of creep strain rate will be used in the solution of a load step.
RATE, Option" "RBE3 - Distributes the force/moment applied at the master node to a set  of slave nodes, taking into account the geometry of the slave nodes as well as weighting factors. 
RBE3, Master, DOF, Slaves, Wtfact" "RCON
RCON - Specifies \"Real constants\" as the subsequent status topic. " "RCYC - Calculates cyclic results for a mode-superposition harmonic solution.
RCYC, IR, IA, SECTOR, Name" "RDEC - Defines the decimation parameters.
RDEC, Option REDUC , --, Nplace" "RDELE - Deletes real constant sets. 
RDELE, NSET1, NSET2, NINC, LCHK" "READ - Reads coordinate and pressure data from a file.
READ, Fname, NSKIP, Format, Xfield, Yfield, Zfield, pRfield, pIfield" "REAL - Sets the element real constant set attribute pointer. 
REAL, NSET" "REALVAR - Forms a variable using only the real part of a complex variable. 
REALVAR, IR, IA, --, --, Name, --, --, FACTA" "RECTNG - Creates a rectangular area anywhere on the working plane. 
RECTNG, X1, X2, Y1, Y2" "REMESH - Specifies the starting and ending remeshing points, and other options, for rezoning.
REMESH, Action, Filename, Ext, --, Opt1, Opt2" "REORDER
REORDER - Specifies \"Model reordering\" as the subsequent status topic. " "RESCOMBINE - Reads results from local results files into the database after a distributed memory parallel (Distributed ANSYS) solution.
RESCOMBINE, NUMFILES, Fname, Ext, Lstep, Sbstep, Fact, KIMG, TIME, ANGLE, NSET, ORDER" "RESCONTROL - Controls file writing for multiframe restarts.
RESCONTROL, Action, Ldstep, Frequency, MAXFILES" "RESET
RESET - Resets all POST1 or POST26 specifications to initial defaults. " "RESP - Generates a response spectrum. 
RESP, IR, LFTAB, LDTAB, specType, dampRatio, DTIME, TMIN, TMAX, inputType" "RESUME - Resumes the database from the database file.
RESUME, Fname, Ext, --, NOPAR, KNOPLOT" "RESVEC - Calculates or includes residual vectors.
RESVEC, Key" "RESWRITE - Appends results data from the database to a results file.
RESWRITE, Fname, --, --, --, cFlag" "REXPORT - Exports displacements from an implicit run to ANSYS LS-DYNA. 
REXPORT, Target, --, --, LSTEP, SBSTEP, Fname, Ext, --" "REZONE - Initiates the rezoning process, sets rezoning options, and rebuilds the database.
REZONE, Option, LDSTEP, SBSTEP" "RFORCE - Specifies the total reaction force data to be stored. 
RFORCE, NVAR, NODE, Item, Comp, Name" "RIGID - Specifies known rigid body modes (if any) of the model. 
RIGID, Dof1, Dof2, Dof3, Dof4, Dof5, Dof6" "RIGRESP - Specifies the rigid response calculation.
RIGRESP, Option, Method, Val1, Val2" "RIMPORT - Imports initial stresses from an explicit dynamics run into ANSYS. 
RIMPORT, Source, Type, Loc, LSTEP, SBSTEP, Fname, Ext, --, SPSCALE, MSCALE" "RLIST - Lists the real constant sets. 
RLIST, NSET1, NSET2, NINC" "RMALIST
RMALIST - Lists all defined master nodes for a ROM method." "RMANL - Assigns model database, dimensionality, and operating direction for the ROM method.
RMANL, Fname, Ext, --, Dimn, Oper" "RMASTER - Defines master nodes for the ROM method.
RMASTER, Node, Lab" "RMCAP - Defines lumped capacitance pairs between conductors C1 and C2 for a ROM method.
RMCAP, RefName, C1, C2" "RMCLIST
RMCLIST - Lists all lumped capacitance pairs defined." "RMFLVEC
RMFLVEC - Writes eigenvectors of fluid nodes to a file for use in damping parameter extraction." "RMLVSCALE - Defines element load vector scaling for a ROM use pass.
RMLVSCALE, Nload, Fact1, Fact2, Fact3, Fact4, Fact5" "RMMLIST
RMMLIST - Lists all mode specifications for the ROM method." "RMMRANGE - Defines and edits various modal parameters for the ROM method.
RMMRANGE, Mode, Key, Min, Max, Nstep, Damp, Scale" "RMMSELECT - Selects modes for the ROM method.
RMMSELECT, Nmode, Method, Dmin, Dmax" "RMNDISP - Extracts neutral plane displacements from a test load or element load solution for the ROM method.
RMNDISP, LoadT, Loc" "RMNEVEC
RMNEVEC - Extracts neutral plane eigenvectors from a modal analysis for the ROM method." "RMODIF - Modifies real constant sets.
RMODIF, NSET, STLOC, V1, V2, V3, V4, V5, V6" "RMORE - Adds real constants to a set. 
RMORE, R7, R8, R9, R10, R11, R12" "RMPORDER - Defines polynomial orders for ROM functions.
RMPORDER, Ord1, Ord2, Ord3, Ord4, Ord5, Ord6, Ord7, Ord8, Ord9" "RMRESUME - Resumes ROM data from a file.
RMRESUME, Fname, Ext, --" "RMRGENERATE
RMRGENERATE - Performs fitting procedure for all ROM functions to generate response surfaces." "RMROPTIONS - Defines options for ROM response surface fitting.
RMROPTIONS, RefName, Type, Invert" "RMRPLOT - Plots response surface of ROM function or its derivatives with respect to the dominant mode(s).
RMRPLOT, RefName, Type, Mode1, Mode2" "RMRSTATUS - Prints status of response surface for ROM function.
RMRSTATUS, RefName" "RMSAVE - Saves ROM data to file.
RMSAVE, Fname, Ext, --" "RMSMPLE - Runs finite element solutions and obtains sample points for the ROM method.
RMSMPLE, Nlgeom, Cap, Seqslv, Eeqslv" "RMUSE - Activates ROM use pass for ROM elements.
RMUSE, Option, Usefil" "RMXPORT
RMXPORT - Exports ROM model to external VHDL-AMS simulator." "ROCK - Specifies a rocking response spectrum. 
ROCK, CGX, CGY, CGZ, OMX, OMY, OMZ" "ROSE - Specifies the Rosenblueth mode combination method.
ROSE, SIGNIF, Label, TD, ForceType" "RPOLY - Creates a regular polygonal area centered about the working plane origin. 
RPOLY, NSIDES, LSIDE, MAJRAD, MINRAD" "RPR4 - Creates a regular polygonal area or prism volume anywhere on the working plane. 
RPR4, NSIDES, XCENTER, YCENTER, RADIUS, THETA, DEPTH" "RPRISM - Creates a regular prism volume centered about the working plane origin. 
RPRISM, Z1, Z2, NSIDES, LSIDE, MAJRAD, MINRAD" "RPSD - Calculates response power spectral density (PSD). 
RPSD, IR, IA, IB, ITYPE, DATUM, Name, --, SIGNIF" "RSFIT - Fit a response surface for an output parameter in a solution set.
RSFIT, RSlab, Slab, Name, Rmod, Ytrans, Yval, Xfilt, CONF" "RSOPT - Creates or loads the radiosity mapping data file for SURF251 or SURF252 element types.
RSOPT, Opt, Filename, Ext, Dir" "RSPLIT - Creates one or more results file(s) from the current results file based on subsets of elements.
RSPLIT, Option, Label, Name1, Name2, Name3, Name4, Name5, Name6, Name7, Name8, Name9, Name10, Name11, Name12, Name13, Name14, Name15, Name16" "RSPLOT - Plot a response surface.
RSPLOT, RSlab, YName, X1Name, X2Name, Type, NPTS, PLOW, PUP" "RSPRNT - Print a response surface.
RSPRNT, RSlab, YName, Xout" "RSSIMS - Performs Monte Carlo simulations on response surface(s).
RSSIMS, RSlab, NSIM, Seed" "RSTMAC - Calculates modal assurance criterion (MAC) and matches nodal solutions from two results files or from one results file and one universal format file.
RSTMAC, File1, Lstep1, Sbstep1, File2, Lstep2, Sbstep2, TolerN, MacLim, Cname, KeyPrint, UNVscale, KeyMass" "RSTOFF - Offsets node or element IDs in the FE geometry record.
RSTOFF, Lab, OFFSET" "RSURF - Generates the radiosity surface elements (SURF251/SURF252) and stores them in the database.
RSURF, Options, Delopts, ETNUM" "RSYMM - Defines the plane of symmetry or center of rotation for the radiosity method.
RSYMM, Option, CS, Axis, NSECT, CONDVALUE" "RSYS - Activates a coordinate system for printout or display of element and nodal results. 
RSYS, KCN" "RTHICK - Defines variable thickness at nodes for shell elements.
RTHICK, Par, ILOC, JLOC, KLOC, LLOC" "SABS - Specifies absolute values for element table operations.
SABS, KEY" "SADD - Forms an element table item by adding two existing items.
SADD, LabR, Lab1, Lab2, FACT1, FACT2, CONST" "SALLOW - Defines the allowable stress table for safety factor calculations.
SALLOW, STRS1, STRS2, STRS3, STRS4, STRS5, STRS6" "SAVE - Saves all current database information.
SAVE, Fname, Ext, --, Slab" "SBCLIST
SBCLIST - Lists solid model boundary conditions." "SBCTRAN
SBCTRAN - Transfers solid model loads and boundary conditions to the FE model." "SDELETE - Deletes sections from the database.
SDELETE, SFIRST, SLAST, SINC, KNOCLEAN, LCHK" "SE - Defines a superelement.
SE, File, --, --, TOLER" "SECCONTROL - Supplements or overrides default section properties.
SECCONTROL, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10, VAL11, VAL12, VAL13" "SECDATA - Describes the geometry of a section.
SECDATA, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10, VAL11, VAL12" "SECFUNCTION - Specifies shell section thickness as a tabular function.
SECFUNCTION, TABLE, KCN" "SECJOINT - Defines local coordinate systems at joint element nodes and other data for joint elements.
SECJOINT, Kywrd, Val1, Val2, Val3, Val4, Val5, Val6" "SECLOCK - Specifies locks on the components of relative motion in a joint element.
SECLOCK, dof, MINVALUE, MAXVALUE, dof, MINVALUE, MAXVALUE, dof, MINVALUE, MAXVALUE" "SECMODIF - Modifies a pretension section
SECMODIF, SECID, Kywrd" "SECNUM - Sets the element section attribute pointer.
SECNUM, SECID" "SECOFFSET - Defines the section offset for cross sections.
SECOFFSET, Location, OFFSET1, OFFSET2, CG-Y, CG-Z, SH-Y, SH-Z" "SECPLOT - Plots the geometry of a beam, pipe, shell, or reinforcing section to scale.
SECPLOT, SECID, VAL1, VAL2, VAL3" "SECREAD - Reads a custom section library or a user-defined section mesh into ANSYS.
SECREAD, Fname, Ext, --, Option" "SECSTOP - Specifies stops on the components of relative motion in a joint element.
SECSTOP, dof, MINVALUE, MAXVALUE, dof, MINVALUE, MAXVALUE, dof, MINVALUE, MAXVALUE" "SECTYPE - Associates section type information with a section ID number.
SECTYPE, SECID, Type, Subtype, Name, REFINEKEY" "SECWRITE - Creates an ASCII file containing user mesh section information.
SECWRITE, Fname, Ext, --, ELEM_TYPE" "SED - Defines the excitation direction for response spectrum and PSD analyses.
SED, SEDX, SEDY, SEDZ, Cname" "SEDLIST - Lists the DOF solution of a superelement after the use pass.
SEDLIST, Sename, KOPT" "SEEXP - Specifies options for the substructure expansion pass.
SEEXP, Sename, Usefil, Imagky, Expopt" "SEGEN - Automatically generate superelements.
SEGEN, Mode, nSuper, mDof, stopStage" "SELIST - Lists the contents of a superelement matrix file.
SELIST, Sename, KOPT, KINT" "SELM
SELM - Specifies \"Superelements\" as the subsequent status topic." "SELTOL - Sets the tolerance for subsequent select operations.
SELTOL, Toler" "SENERGY - Determines the stored magnetic energy or co-energy.
SENERGY, OPT, ANTYPE" "SEOPT - Specifies substructure analysis options.
SEOPT, Sename, SEMATR, SEPR, SESST, ExpMth, SeOcLvL" "SESYMM - Performs a symmetry operation on a superelement within the use pass.
SESYMM, Sename, Ncomp, INC, File, Ext, --" "SET - Defines the data set to be read from the results file.
SET, Lstep, Sbstep, Fact, KIMG, TIME, ANGLE, NSET, ORDER" "SETFGAP - Updates or defines the real constant table for squeeze film elements.
SETFGAP, GAP, ROPT, --, PAMB, ACF1, ACF2, PREF, MFP" "SETRAN - Creates a superelement from an existing superelement.
SETRAN, Sename, KCNTO, INC, File, Ext, --, DX, DY, DZ, NOROT" "SEXP - Forms an element table item by exponentiating and multiplying.
SEXP, LabR, Lab1, Lab2, EXP1, EXP2" "SF - Specifies surface loads on nodes.
SF, Nlist, Lab, VALUE, VALUE2" "SFA - Specifies surface loads on the selected areas.
SFA, Area, LKEY, Lab, VALUE, VALUE2" "SFACT - Allows safety factor or margin of safety calculations to be made.
SFACT, TYPE" "SFADELE - Deletes surface loads from areas.
SFADELE, AREA, LKEY, Lab" "SFALIST - Lists the surface loads for the specified area.
SFALIST, AREA, Lab" "SFBEAM - Specifies surface loads on beam and pipe elements.
SFBEAM, Elem, LKEY, Lab, VALI, VALJ, VAL2I, VAL2J, IOFFST, JOFFST, LENRAT" "SFCALC - Calculates the safety factor or margin of safety.
SFCALC, LabR, LabS, LabT, TYPE" "SFCUM - Specifies that surface loads are to be accumulated.
SFCUM, Lab, Oper, FACT, FACT2" "SFDELE - Deletes surface loads.
SFDELE, Nlist, Lab" "SFE - Specifies surface loads on elements.
SFE, Elem, LKEY, Lab, KVAL, VAL1, VAL2, VAL3, VAL4" "SFEDELE - Deletes surface loads from elements.
SFEDELE, ELEM, LKEY, Lab" "SFELIST - Lists the surface loads for elements.
SFELIST, ELEM, Lab" "SFFUN - Specifies a varying surface load.
SFFUN, Lab, Par, Par2" "SFGRAD - Specifies a gradient (slope) for surface loads.
SFGRAD, Lab, SLKCN, Sldir, SLZER, SLOPE" "SFL - Specifies surface loads on lines of an area.
SFL, Line, Lab, VALI, VALJ, VAL2I, VAL2J" "SFLDELE - Deletes surface loads from lines.
SFLDELE, LINE, Lab" "SFLEX - Sets flexibility factors for the currently defined pipe element section.
SFLEX, FFAX, FFBY, FFBZ, FFTO, FFTSY, FFTSZ" "SFLIST - Lists surface loads.
SFLIST, NODE, Lab" "SFLLIST - Lists the surface loads for lines.
SFLLIST, LINE, Lab" "SFSCALE - Scales surface loads on elements.
SFSCALE, Lab, FACT, FACT2" "SFTRAN
SFTRAN - Transfer the solid model surface loads to the finite element model." "SHELL - Selects a shell element or shell layer location for results output.
SHELL, Loc" "SHPP - Controls element shape checking.
SHPP, Lab, VALUE1, VALUE2" "SLIST - Summarizes the section properties for all defined sections in the current session.
SLIST, SFIRST, SLAST, SINC, Details, Type" "SLOAD - Load a pretension section.
SLOAD, SECID, PLNLAB, KINIT, KFD, FDVALUE, LSLOAD, LSLOCK" "SMALL - Finds the smallest of three variables.
SMALL, IR, IA, IB, IC, Name, --, --, FACTA, FACTB, FACTC" "SMAX - Forms an element table item from the maximum of two other items.
SMAX, LabR, Lab1, Lab2, FACT1, FACT2" "SMBODY
SMBODY - Specifies \"Body loads on the solid model\" as the subsequent status topic." "SMCONS
SMCONS - Specifies \"Constraints on the solid model\" as the subsequent status topic." "SMFOR
SMFOR - Specifies \"Forces on the solid model\" as the subsequent status topic." "SMIN - Forms an element table item from the minimum of two other items.
SMIN, LabR, Lab1, Lab2, FACT1, FACT2" "SMOOTH - Allows smoothing of noisy data and provides a graphical representation of the data.
SMOOTH, Vect1, Vect2, DATAP, FITPT, Vect3, Vect4, DISP" "SMRTSIZE - Specifies meshing parameters for automatic (smart) element sizing.
SMRTSIZE, SIZLVL, FAC, EXPND, TRANS, ANGL, ANGH, GRATIO, SMHLC, SMANC, MXITR, SPRX" "SMSURF
SMSURF - Specifies \"Surface loads on the solid model\" as the subsequent status topic." "SMULT - Forms an element table item by multiplying two other items.
SMULT, LabR, Lab1, Lab2, FACT1, FACT2" "SNOPTION -  Specifies Supernode (SNODE) eigensolver options.
SNOPTION, RangeFact, BlockSize, RobustLev, Compute, --, Solve_Info" "SOLU - Specifies solution summary data per substep to be stored.
SOLU, NVAR, Item, Comp, Name" "SOLUOPT
SOLUOPT - Specifies \"Solution options\" as the subsequent status topic." "SOLVE - Starts a solution.
SOLVE, Action" "SORT
SORT - Specifies \"Sort settings\" as the subsequent status topic." "SOURCE - Defines a default location for undefined nodes or keypoints.
SOURCE, X, Y, Z" "SPACE - Defines a space node for radiation using the Radiation Matrix method.
SPACE, NODE" "SPCNOD - Defines a space node for radiation using the Radiosity method.
SPCNOD, ENCL, NODE" "SPCTEMP - Defines a free-space ambient temperature for radiation using the 	Radiosity method.
SPCTEMP, ENCL, TEMP" "SPDAMP - Defines input spectrum damping in a multi-point response spectrum analysis.
SPDAMP, TBLNO, CURVNO, DampRatio" "SPEC
SPEC - Specifies \"Miscellaneous specifications\" as the subsequent status topic." "SPFREQ - Defines the frequency points for the input spectrum tables SPVAL vs. SPFREQ for multi-point spectrum analysis.
SPFREQ, TBLNO, FREQ1, FREQ2, FREQ3, FREQ4, FREQ5, FREQ6, FREQ7" "SPGRAPH - Displays input spectrum curves for MPRS analysis.
SPGRAPH, TBLNO, CURVNO, CURVNOBeg" "SPH4 - Creates a spherical volume anywhere on the working plane.
SPH4, XCENTER, YCENTER, RAD1, RAD2" "SPH5 - Creates a spherical volume by diameter end points.
SPH5, XEDGE1, YEDGE1, XEDGE2, YEDGE2" "SPHERE - Creates a spherical volume centered about the working plane origin.
SPHERE, RAD1, RAD2, THETA1, THETA2" "SPLINE - Generates a segmented spline through a series of keypoints.
SPLINE, P1, P2, P3, P4, P5, P6, XV1, YV1, ZV1, XV6, YV6, ZV6" "SPLOT - Displays the selected areas and a faceted view of their underlying surfaces 
SPLOT, NA1, NA2, NINC, MESH" "SPMWRITE - Calculates the state-space matrices and writes them to the SPM file. 
SPMWRITE, Method, NMODE, Inputs, InputLabels, Outputs, OutputLabels, NIC, VelAccKey, FileFormat" "SPOINT - Defines a point for moment summations.
SPOINT, NODE, X, Y, Z" "SPOPT - Selects the spectrum type and other spectrum options.
SPOPT, Sptype, NMODE, Elcalc, modeReuseKey" "SPOWER - Calculates sound power parameters.
SPOWER, INLETPORT, OUTLETPORT" "SPREAD - Turns on a dashed tolerance curve for the subsequent curve plots.
SPREAD, VALUE" "SPTOPT
SPTOPT - Specifies \"Spectrum analysis options\" as the subsequent status topic." "SPUNIT - Defines the type of multi-point response spectrum.
SPUNIT, TBLNO, Type, GVALUE, KeyInterp" "SPVAL - Defines multi-point response spectrum values.
SPVAL, TBLNO, CURVNO, SV1, SV2, SV3, SV4, SV5, SV6, SV7" "SQRT - Forms the square root of a variable.
SQRT, IR, IA, --, --, Name, --, --, FACTA" "SRSS - Specifies the square root of sum of squares mode combination method.
SRSS, SIGNIF, Label, AbsSumKey, ForceType" "SSBT - Specifies preintegrated bending thermal effects for shell sections.
SSBT, BT11, BT22, BT12, T" "SSLN - Selects and displays small lines in the model.
SSLN, FACT, SIZE" "SSMT - Specifies preintegrated membrane thermal effects for shell sections.
SSMT, MT11, MT22, MT12, T" "SSPA - Specifies a preintegrated membrane stiffness for shell sections.
SSPA, A11, A21, A31, A22, A32, A33, T" "SSPB - Specifies a preintegrated coupling stiffness for shell sections.
SSPB, B11, B21, B31, B22, B32, B33, T, B12, B13, B23" "SSPD - Specifies a preintegrated bending stiffness for shell sections.
SSPD, D11, D21, D31, D22, D32, D33, T" "SSPE - Specifies a preintegrated transverse shear stiffness for shell sections.
SSPE, E11, E21, E22, T" "SSPM - Specifies mass density for a preintegrated shell section.
SSPM, DENS, T" "SSTATE - Defines a steady-state analysis.
SSTATE, Action, CM_Name, Val1, Val2, Val3, Val4, Val5, Val6, Val7, Val8, Val9" "SSUM
SSUM - Calculates and prints the sum of element table items." "STABILIZE - Activates stabilization for all elements that support nonlinear stabilization.
STABILIZE, Key, Method, VALUE, SubStpOpt, FORCELIMIT" "STAOPT - Specifies static analysis options.
STAOPT, Method" "STAT
STAT - Displays the status of database settings." "STEF - Specifies Stefan-Boltzmann radiation constant.
STEF, VALUE" "STORE - Stores data in the database for the defined variables.
STORE, Lab, NPTS" "SUBOPT - Specifies Subspace (SUBSP) eigensolver options.
SUBOPT, Option, Value1" "SUBSET - Reads results for the selected portions of the model.
SUBSET, Lstep, SBSTEP, FACT, KIMG, TIME, ANGLE, NSET" "SUCALC - Create new result data by operating on two existing result data sets on a given surface.
SUCALC, RSetName, lab1, Oper, lab2, fact1, fact2, const" "SUCR - Create a surface.
SUCR, SurfName, SurfType, nRefine, Radius, blank, blank, TolOut" "SUDEL - Delete geometry information as well as any mapped results for specified surface.
SUDEL, SurfName" "SUEVAL - Perform operations on a mapped item and store result in a scalar parameter.
SUEVAL, Parm, lab1, Oper" "SUGET - Moves surface geometry and mapped results to an array parameter.
SUGET, SurfName, RSetName, Parm, Geom" "SUMAP - Map results onto selected surface(s).
SUMAP, RSetName, Item, Comp" "SUMTYPE - Sets the type of summation to be used in the following load case operations.
SUMTYPE, Label" "SUPL - Plot result data on all selected surfaces or on a specified surface.
SUPL, SurfName, RSetName, KWIRE" "SUPR - Print global status, geometry information and/or result information. 
SUPR, SurfName, RSetName" "SURESU - Read a set of surface definitions and result items from a file and make them the current set. 
SURESU, --, Fname, Fext, Fdir" "SUSAVE - Saves surface definitions to a file.
SUSAVE, Lab, Fname, Fext, Fdir" "SUSEL - Selects a subset of surfaces
SUSEL, Type, Name1, Name2, Name3, Name4, Name5, Name6, Name7, Name8" "SUVECT - Create new result data by operating on two existing result vectors on a given surface. 
SUVECT, RSetName, lab1, Oper, lab2, Offset" "SV - Defines spectrum values to be associated with frequency points.
SV, DAMP, SV1, SV2, SV3, SV4, SV5, SV6, SV7, SV8, SV9" "SVPLOT - Displays input spectrum curves.
SVPLOT, OptionScale, damp1, damp2, damp3, damp4" "SVTYP - Defines the type of single-point response spectrum.
SVTYP, KSV, FACT, KeyInterp" "SWADD - Adds more surfaces to an existing spot weld set.
SWADD, Ecomp, SHRD, NCM1, NCM2, NCM3, NCM4, NCM5, NCM6, NCM7, NCM8, NCM9" "SWDEL - Deletes spot weld sets.
SWDEL, Ecomp" "SWGEN - Creates a new spot weld set.
SWGEN, Ecomp, SWRD, NCM1, NCM2, SND1, SND2, SHRD, DIRX, DIRY, DIRZ, ITTY, ICTY" "SWLIST - Lists spot weld sets.
SWLIST, Ecomp" "SYNCHRO - Specifies whether the excitation frequency is synchronous or asynchronous with the rotational velocity of a structure.
SYNCHRO, RATIO, Cname" "TALLOW - Defines the temperature table for safety factor calculations.
TALLOW, TEMP1, TEMP2, TEMP3, TEMP4, TEMP5, TEMP6" "TARGET - Specifies the target nodes for mapping pressures onto surface effect elements.
TARGET, Nlist" "TB - Activates a data table for material properties or special element input.
TB, Lab, MAT, NTEMP, NPTS, TBOPT, EOSOPT, FuncName" "TBCOPY - Copies a data table from one material to another.
TBCOPY, Lab, MATF, MATT" "TBDATA - Defines data for the material data table.
TBDATA, STLOC, C1, C2, C3, C4, C5, C6" "TBDELE - Deletes previously defined material data tables.
TBDELE, Lab, MAT1, MAT2, INC" "TBEO - Sets special options or parameters for material data tables.
TBEO, Par, Value" "TBFIELD - Defines values of field variables for material data tables.
TBFIELD, Type, Value" "TBFT - Performs material curve-fitting operations.
TBFT, Oper, ID, Option1, Option2, Option3, Option4, Option5, Option6, Option7" "TBIN - Sets parameters used for interpolation of the material data tables.
TBIN, Oper, Par1, Par2, Par3, Par4" "TBLE
TBLE - Specifies \"Data table properties\" as the subsequent status topic." "TBLIST - Lists the material data tables.
TBLIST, Lab, MAT" "TBMODIF - Modifies data for the material data table (GUI).
TBMODIF, ROW, COL, VALUE" "TBPLOT - Displays the material data table.
TBPLOT, Lab, MAT, TBOPT, TEMP, SEGN" "TBPT - Defines a point on a nonlinear data curve.
TBPT, Oper, X1, X2, X3, ..., XN" "TBTEMP - Defines a temperature for a material data table.
TBTEMP, TEMP, KMOD" "TCHG - Converts 20-node degenerate tetrahedral elements to their 10-node non-degenerate counterparts.
TCHG, ENAME1, ENAME2, ETYPE2" "TERM - Specifies various terminal driver options.
TERM, Kywrd, Opt1, Opt2, Opt3" "THEXPAND - Enables or disables thermal loading
THEXPAND, KEY" "THOPT - Specifies nonlinear transient thermal solution options.
THOPT, Refopt, REFORMTOL, NTABPOINTS, TEMPMIN, TEMPMAX, --, ALGO" "TIFF - Provides TIFF file Export for ANSYS Displays.
TIFF, Kywrd, OPT" "TIME - Sets the time for a load step.
TIME, TIME" "TIMERANGE - Specifies the time range for which data are to be stored.
TIMERANGE, TMIN, TMAX" "TIMINT - Turns on transient effects.
TIMINT, Key, Lab" "TIMP - Improves the quality of tetrahedral elements that are not associated with a volume.
TIMP, ELEM, CHGBND, IMPLEVEL" "TINTP - Defines transient integration parameters.
TINTP, GAMMA, ALPHA, DELTA, THETA, OSLM, TOL, --, --, AVSMOOTH, ALPHAF, ALPHAM" "TOFFST - Specifies the temperature offset from absolute zero to zero.
TOFFST, VALUE" "TORQ2D
TORQ2D - Calculates torque on a body in a magnetic field." "TORQC2D - Calculates torque on a body in a magnetic field based on a circular path.
TORQC2D, RAD, NUMN, LCSYS" "TORQSUM - Summarizes electromagnetic torque calculations on element components.
TORQSUM, Cnam1, Cnam2, Cnam3, Cnam4, Cnam5, Cnam6, Cnam7, Cnam8, Cnam9" "TORUS - Creates a toroidal volume.
TORUS, RAD1, RAD2, RAD3, THETA1, THETA2" "TRANS - Reformats File.GRPH for improved performance with plotters.
TRANS, Fname, Ext, --" "TRANSFER - Transfers a pattern of nodes to another coordinate system.
TRANSFER, KCNTO, INC, NODE1, NODE2, NINC" "TREF - Defines the reference temperature for the thermal strain calculations.
TREF, TREF" "TRNOPT - Specifies transient analysis options.
TRNOPT, Method, MAXMODE, --, MINMODE, MCout, TINTOPT, VAout" "TRPDEL - Deletes particle flow or charged particle trace points.
TRPDEL, NTRP1, NTRP2, TRPINC" "TRPLIS - Lists the particle flow or charged particle trace points.
TRPLIS, NTRP1, NTRP2, TRPINC, Opt" "TRPOIN - Defines a point through which a particle flow or charged particle trace will travel.
TRPOIN, X, Y, Z, VX, VY, VZ, CHRG, MASS" "TRTIME - Defines the options used for the PLTRAC (particle flow or charged particle trace) command.
TRTIME, TIME, SPACING, OFFSET, SIZE, LENGTH" "TSHAP - Defines simple 2-D and 3-D geometric surfaces for target segment elements.
TSHAP, Shape" "TSRES - Defines an array of key times at which the time-stepping strategy changes.
TSRES, Array" "TUNIF - Assigns a uniform temperature to all nodes.
TUNIF, TEMP" "TVAR - Changes time to the cumulative iteration number.
TVAR, KEY" "TYPE - Sets the element type attribute pointer.
TYPE, ITYPE" "UIMP - Defines constant material properties (GUI).
UIMP, MAT, Lab1, Lab2, Lab3, VAL1, VAL2, VAL3" "UNDELETE - Removes results sets from the group of sets selected for editing.
UNDELETE, Option, Nstart, Nend" "UNDO - Allows the user to modify or save commands issued since the last RESUME or SAVE command. 
UNDO, Kywrd" "UNPAUSE
UNPAUSE - Restores use of a temporarily released product license." "UPCOORD - Modifies the coordinates of the active set of nodes, based on the current displacements.
UPCOORD, FACTOR, Key" "UPGEOM - Adds displacements from a previous analysis and updates the geometry of the finite element model to the deformed configuration.
UPGEOM, FACTOR, LSTEP, SBSTEP, Fname, Ext, --" "USRCAL - Allows user-solution subroutines to be activated or deactivated.
USRCAL, Rnam1, Rnam2, Rnam3, Rnam4, Rnam5, Rnam6, Rnam7, Rnam8, Rnam9" "USRDOF - Specifies the degrees of freedom for the user-defined element USER300.
USRDOF, Action, DOF1, DOF2, DOF3, DOF4, DOF5, DOF6, DOF7, DOF8, DOF9, DOF10" "USRELEM - Specifies the characteristics of the user-defined element USER300.
USRELEM, NNODES, NDIM, KeyShape, NREAL, NSAVEVARS, NRSLTVAR, KEYANSMAT, NINTPNTS, KESTRESS, KEYSYM" "V - Defines a volume through keypoints.
V, P1, P2, P3, P4, P5, P6, P7, P8" "V2DOPT - Specifies 2-D/axisymmetric view factor calculation options.
V2DOPT, GEOM, NDIV, HIDOPT, NZONE" "VA - Generates a volume bounded by existing areas.
VA, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10" "VADD - Adds separate volumes to create a single volume.
VADD, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VARDEL - Deletes a variable (GUI).
VARDEL, NVAR" "VARNAM - Names (or renames) a variable.
VARNAM, IR, Name" "VATT - Associates element attributes with the selected, unmeshed volumes.
VATT, MAT, REAL, TYPE, ESYS, SECNUM" "VCLEAR - Deletes nodes and volume elements associated with selected volumes.
VCLEAR, NV1, NV2, NINC" "VCROSS - Forms element table items from the cross product of two vectors.
VCROSS, LabXR, LabYR, LabZR, LabX1, LabY1, LabZ1, LabX2, LabY2, LabZ2" "VDDAM - Specifies the velocity spectrum computation constants for the analysis of shock resistance of shipboard structures.
VDDAM, VF, VA, VB, VC" "VDELE - Deletes unmeshed volumes.
VDELE, NV1, NV2, NINC, KSWP" "VDGL - Lists keypoints of a volume that lie on a parametric degeneracy.
VDGL, NV1, NV2, NINC" "VDOT - Forms an element table item from the dot product of two vectors.
VDOT, LabR, LabX1, LabY1, LabZ1, LabX2, LabY2, LabZ2" "VDRAG - Generates volumes by dragging an area pattern along a path.
VDRAG, NA1, NA2, NA3, NA4, NA5, NA6, NLP1, NLP2, NLP3, NLP4, NLP5, NLP6" "VEORIENT - Specifies brick element orientation for volume mapped (hexahedron) meshing.
VEORIENT, VNUM, Option, VALUE1, VALUE2" "VEXT - Generates additional volumes by extruding areas.
VEXT, NA1, NA2, NINC, DX, DY, DZ, RX, RY, RZ" "VFOPT - Specifies options for the view factor file and calculates view factors.
VFOPT, Opt, Filename, Ext, Dir, Filetype, Fileformat" "VFQUERY - Queries and prints element Hemicube view factors and average view factor.
VFQUERY, SRCELEM, TARELEM" "VFSM - Adjusts view factor matrix to satisfy reciprocity and/or row sum properties.
VFSM, Action, ENCL, OPT, MAXITER, CONV" "VGEN - Generates additional volumes from a pattern of volumes.
VGEN, ITIME, NV1, NV2, NINC, DX, DY, DZ, KINC, NOELEM, IMOVE" "VGET - Moves a variable into an array parameter vector.
VGET, Par, IR, TSTRT, KCPLX" "VGLUE - Generates new volumes by \"gluing\" volumes.
VGLUE, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VIMP - Improves the quality of the tetrahedral elements in the selected volume(s).
VIMP, VOL, CHGBND, IMPLEVEL" "VINP - Finds the pairwise intersection of volumes.
VINP, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VINV - Finds the intersection of volumes.
VINV, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VLIST - Lists the defined volumes.
VLIST, NV1, NV2, NINC" "VLSCALE - Generates a scaled set of volumes from a pattern of volumes.
VLSCALE, NV1, NV2, NINC, RX, RY, RZ, KINC, NOELEM, IMOVE" "VMESH - Generates nodes and volume elements within volumes.
VMESH, NV1, NV2, NINC" "VOFFST - Generates a volume, offset from a given area.
VOFFST, NAREA, DIST, KINC" "VOLUMES
VOLUMES - Specifies \"Volumes\" as the subsequent status topic." "VOVLAP - Overlaps volumes.
VOVLAP, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VPLOT - Displays the selected volumes.
VPLOT, NV1, NV2, NINC, DEGEN, SCALE" "VPTN - Partitions volumes.
VPTN, NV1, NV2, NV3, NV4, NV5, NV6, NV7, NV8, NV9" "VPUT - Moves an array parameter vector into a variable.
VPUT, Par, IR, TSTRT, KCPLX, Name" "VROTAT - Generates cylindrical volumes by rotating an area pattern about an axis.
VROTAT, NA1, NA2, NA3, NA4, NA5, NA6, PAX1, PAX2, ARC, NSEG" "VSBA - Subtracts areas from volumes.
VSBA, NV, NA, SEPO, KEEPV, KEEPA" "VSBV - Subtracts volumes from volumes.
VSBV, NV1, NV2, SEPO, KEEP1, KEEP2" "VSBW - Subtracts intersection of the working plane from volumes (divides volumes).
VSBW, NV, SEPO, KEEP" "VSEL - Selects a subset of volumes.
VSEL, Type, Item, Comp, VMIN, VMAX, VINC, KSWP" "VSLA - Selects those volumes containing the selected areas.
VSLA, Type, VLKEY" "VSUM - Calculates and prints geometry statistics of the selected volumes.
VSUM, LAB" "VSWEEP - Fills an existing unmeshed volume with elements by sweeping the mesh from an adjacent area through the volume.
VSWEEP, VNUM, SRCA, TRGA, LSMO" "VSYMM - Generates volumes from a volume pattern by symmetry reflection.
VSYMM, Ncomp, NV1, NV2, NINC, KINC, NOELEM, IMOVE" "VTRAN - Transfers a pattern of volumes to another coordinate system.
VTRAN, KCNTO, NV1, NV2, NINC, KINC, NOELEM, IMOVE" "VTYPE - Specifies the viewing procedure used to determine the form factors for the Radiation Matrix method.
VTYPE, NOHID, NZONE" "WAVES - Initiates reordering.
WAVES, Wopt, OLDMAX, OLDRMS" "WERASE
WERASE - Erases all reordering wave lists." "WFRONT - Estimates wavefront statistics.
WFRONT, KPRNT, KCALC" "WMID - Specifies reordering options for the WAVES command.
WMID, Key" "WMORE - Adds more nodes to the starting wave list.
WMORE, NODE1, NODE2, NINC, ITIME, INC" "WPAVE - Moves the working plane origin to the average of specified points.
WPAVE, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3" "WPCSYS - Defines the working plane location based on a coordinate system.
WPCSYS, WN, KCN" "WPLANE - Defines a working plane to assist in picking operations.
WPLANE, WN, XORIG, YORIG, ZORIG, XXAX, YXAX, ZXAX, XPLAN, YPLAN, ZPLAN" "WPOFFS - Offsets the working plane.
WPOFFS, XOFF, YOFF, ZOFF" "WPROTA - Rotates the working plane.
WPROTA, THXY, THYZ, THZX" "WPSTYL - Controls the display and style of the working plane.
WPSTYL, SNAP, GRSPAC, GRMIN, GRMAX, WPTOL, WPCTYP, GRTYPE, WPVIS, SNAPANG" "WRFULL - Stops solution after assembling global matrices.
WRFULL, Ldstep" "WRITE - Writes the radiation matrix file.
WRITE, Fname" "WRITEMAP - Writes interpolated pressure data to a file.
WRITEMAP, Fname" "WSORT - Initiates element reordering based upon a geometric sort.
WSORT, Lab, KORD, --, Wopt, OLDMAX, OLDRMS" "WSPRINGS
WSPRINGS - Creates weak springs on corner nodes of a bounding box of the currently selected elements." "WSTART - Defines a starting wave list.
WSTART, NODE1, NODE2, NINC, ITIME, INC" "WTBCREATE - Creates a USER300 element to model the turbine for full aeroelastic coupling analysis and specifies relevant settings for the analysis.
WTBCREATE, IEL, NODE, DAMP" "XFDATA - Defines a crack in the model by specifying nodal level set values
XFDATA, EnrichmentID, LSM or -- , ELEMNUM, NODENUM, PHI" "XFENRICH - Defines parameters associated with crack propagation using XFEM
XFENRICH, EnrichmentID, CompName, MatID" "XFLIST - Lists enrichment details and associated crack information
XFLIST, EnrichmentID" "XVAR - Specifies the X variable to be displayed.
XVAR, N" "~CAT5IN - Transfers a .CATPart file into the ANSYS program.
~CAT5IN, Name, Extension, Path, Entity, FMT, NOCL, NOAN" "~CATIAIN - Transfers a CATIA model into the ANSYS program.
~CATIAIN, Name, Extension, Path, - -, - -, BLANK, - -" "~PARAIN - Transfers a Parasolid file into the ANSYS program.
~PARAIN, Name, Extension, Path, Entity, FMT, Scale" "~PROEIN - Transfers a Creo Parametric part into the ANSYS program.
~PROEIN, Name, Extension, Path, Proecomm" "~SATIN - Transfers a .SAT file into the ANSYS program.
~SATIN, Name, Extension, Path, Entity, FMT, NOCL, NOAN" "~UGIN - Transfers an NX part into the ANSYS program.
~UGIN, Name, Extension, Path, Entity, LAYER, FMT" "/WB - Ansys undocumented command
/WB" "XMLO - Ansys undocumented command
XMLO" "/XML - Ansys undocumented command
/XML" "CNTR - Ansys undocumented command
CNTR" "EBLOCK - Ansys undocumented command
EBLOCK" "CMBLOCK - Ansys undocumented command
CMBLOCK" "NBLOCK - Ansys undocumented command
NBLOCK" "/TRACK - Ansys undocumented command
/TRACK" "CWZPLOT - Ansys undocumented command
CWZPLOT" "~EUI - Ansys undocumented command
~EUI" "NELE - Ansys undocumented command
NELE" "EALL - Ansys undocumented command
EALL" "NALL - Ansys undocumented command
NALL" "FLITEM - Ansys undocumented command
FLITEM" "LSLN - Ansys undocumented command
LSLN" "PSOLVE - Ansys undocumented command
PSOLVE" "ASLN - Ansys undocumented command
ASLN" "/VERIFY - Ansys undocumented command
/VERIFY" "/SSS - Ansys undocumented command
/SSS" "~CFIN - Ansys undocumented command
~CFIN" "*EVAL - Ansys undocumented command
*EVAL" "*MOONEY - Ansys undocumented command
*MOONEY" "/RUNSTAT - Ansys undocumented command
/RUNSTAT" "ALPFILL - Ansys undocumented command
ALPFILL" "ARCOLLAPSE - Ansys undocumented command
ARCOLLAPSE" "ARDETACH - Ansys undocumented command
ARDETACH" "ARFILL - Ansys undocumented command
ARFILL" "ARMERGE - Ansys undocumented command
ARMERGE" "ARSPLIT - Ansys undocumented command
ARSPLIT" "FIPLOT - Ansys undocumented command
FIPLOT" "GAPFINISH - Ansys undocumented command
GAPFINISH" "GAPLIST - Ansys undocumented command
GAPLIST" "GAPMERGE - Ansys undocumented command
GAPMERGE" "GAPOPT - Ansys undocumented command
GAPOPT" "GAPPLOT - Ansys undocumented command
GAPPLOT" "LNCOLLAPSE - Ansys undocumented command
LNCOLLAPSE" "LNDETACH - Ansys undocumented command
LNDETACH" "LNFILL - Ansys undocumented command
LNFILL" "LNMERGE - Ansys undocumented command
LNMERGE" "LNSPLIT - Ansys undocumented command
LNSPLIT" "PCONV - Ansys undocumented command
PCONV" "PLCONV - Ansys undocumented command
PLCONV" "PEMOPTS - Ansys undocumented command
PEMOPTS" "PEXCLUDE - Ansys undocumented command
PEXCLUDE" "PINCLUDE - Ansys undocumented command
PINCLUDE" "PMETH - Ansys undocumented command
PMETH" "/PMETH - Ansys undocumented command
/PMETH" "PMOPTS - Ansys undocumented command
PMOPTS" "PPLOT - Ansys undocumented command
PPLOT" "PPRANGE - Ansys undocumented command
PPRANGE" "PRCONV - Ansys undocumented command
PRCONV" "PRECISION - Ansys undocumented command
PRECISION" "RALL - Ansys undocumented command
RALL" "RFILSZ - Ansys undocumented command
RFILSZ" "RITER - Ansys undocumented command
RITER" "RMEMRY - Ansys undocumented command
RMEMRY" "RSPEED - Ansys undocumented command
RSPEED" "RSTAT - Ansys undocumented command
RSTAT" "RTIMST - Ansys undocumented command
RTIMST" "/RUNST - Ansys undocumented command
/RUNST" "RWFRNT - Ansys undocumented command
RWFRNT" "SARPLOT - Ansys undocumented command
SARPLOT" "SHSD - Ansys undocumented command
SHSD" "SLPPLOT - Ansys undocumented command
SLPPLOT" "SLSPLOT - Ansys undocumented command
SLSPLOT" "VCVFILL - Ansys undocumented command
VCVFILL" "/OPT - Ansys undocumented command
/OPT" "OPEQN - Ansys undocumented command
OPEQN" "OPFACT - Ansys undocumented command
OPFACT" "OPFRST - Ansys undocumented command
OPFRST" "OPGRAD - Ansys undocumented command
OPGRAD" "OPKEEP - Ansys undocumented command
OPKEEP" "OPLOOP - Ansys undocumented command
OPLOOP" "OPPRNT - Ansys undocumented command
OPPRNT" "OPRAND - Ansys undocumented command
OPRAND" "OPSUBP - Ansys undocumented command
OPSUBP" "OPSWEEP - Ansys undocumented command
OPSWEEP" "OPTYPE - Ansys undocumented command
OPTYPE" "OPUSER - Ansys undocumented command
OPUSER" "OPVAR - Ansys undocumented command
OPVAR" "OPADD - Ansys undocumented command
OPADD" "OPCLR - Ansys undocumented command
OPCLR" "OPDEL - Ansys undocumented command
OPDEL" "OPMAKE - Ansys undocumented command
OPMAKE" "OPSEL - Ansys undocumented command
OPSEL" "OPANL - Ansys undocumented command
OPANL" "OPDATA - Ansys undocumented command
OPDATA" "OPRESU - Ansys undocumented command
OPRESU" "OPSAVE - Ansys undocumented command
OPSAVE" "OPEXE - Ansys undocumented command
OPEXE" "OPLFA - Ansys undocumented command
OPLFA" "OPLGR - Ansys undocumented command
OPLGR" "OPLIST - Ansys undocumented command
OPLIST" "OPLSW - Ansys undocumented command
OPLSW" "OPRFA - Ansys undocumented command
OPRFA" "OPRGR - Ansys undocumented command
OPRGR" "OPRSW - Ansys undocumented command
OPRSW" "PILECALC - Ansys undocumented command
PILECALC" "PILEDISPSET - Ansys undocumented command
PILEDISPSET" "PILEGEN - Ansys undocumented command
PILEGEN" "PILELOAD - Ansys undocumented command
PILELOAD" "PILEMASS - Ansys undocumented command
PILEMASS" "PILERUN - Ansys undocumented command
PILERUN" "PILESEL - Ansys undocumented command
PILESEL" "PILESTIF - Ansys undocumented command
PILESTIF" "PLVAROPT - Ansys undocumented command
PLVAROPT" "PRVAROPT - Ansys undocumented command
PRVAROPT" "TOCOMP - Ansys undocumented command
TOCOMP" "TODEF - Ansys undocumented command
TODEF" "TOFREQ - Ansys undocumented command
TOFREQ" "TOTYPE - Ansys undocumented command
TOTYPE" "TOVAR - Ansys undocumented command
TOVAR" "TOEXE - Ansys undocumented command
TOEXE" "TOLOOP - Ansys undocumented command
TOLOOP" "TOGRAPH - Ansys undocumented command
TOGRAPH" "TOLIST - Ansys undocumented command
TOLIST" "TOPLOT - Ansys undocumented command
TOPLOT" "TOPRINT - Ansys undocumented command
TOPRINT" "TOSTAT - Ansys undocumented command
TOSTAT" "TZAMESH - Ansys undocumented command
TZAMESH" "TZDELE - Ansys undocumented command
TZDELE" "TZEGEN - Ansys undocumented command
TZEGEN" "XVAROPT - Ansys undocumented command
XVAROPT" "PGSAVE - Ansys undocumented command
PGSAVE" "SOLCONTROL - Ansys undocumented command
SOLCONTROL" "TOTAL - Ansys undocumented command
TOTAL" "VTGEOM - Ansys undocumented command
VTGEOM" "VTREAL - Ansys undocumented command
VTREAL" "VTSTAT - Ansys undocumented command
VTSTAT" "PGRAPH - Ansys undocumented command
PGRAPH" "/VT - Ansys undocumented command
/VT" "VTIN - Ansys undocumented command
VTIN" "VTRFIL - Ansys undocumented command
VTRFIL" "VTTEMP - Ansys undocumented command
VTTEMP" "PGRSET - Ansys undocumented command
PGRSET" "VTCLR - Ansys undocumented command
VTCLR" "VTMETH - Ansys undocumented command
VTMETH" "VTRSLT - Ansys undocumented command
VTRSLT" "VTVMOD - Ansys undocumented command
VTVMOD" "PGSELE - Ansys undocumented command
PGSELE" "VTDISC - Ansys undocumented command
VTDISC" "VTMP - Ansys undocumented command
VTMP" "VTSEC - Ansys undocumented command
VTSEC" "PGWRITE - Ansys undocumented command
PGWRITE" "VTEVAL - Ansys undocumented command
VTEVAL" "VTOP - Ansys undocumented command
VTOP" "VTSFE - Ansys undocumented command
VTSFE" "POUTRES - Ansys undocumented command
POUTRES" "VTFREQ - Ansys undocumented command
VTFREQ" "VTPOST - Ansys undocumented command
VTPOST" "VTSL - Ansys undocumented command
VTSL" "FLDATA1-40 - Ansys undocumented command
FLDATA1-40" "HFPCSWP - Ansys undocumented command
HFPCSWP" "MSDATA - Ansys undocumented command
MSDATA" "MSVARY - Ansys undocumented command
MSVARY" "QFACT - Ansys undocumented command
QFACT" "FLOCHECK - Ansys undocumented command
FLOCHECK" "HFPOWER - Ansys undocumented command
HFPOWER" "MSMASS - Ansys undocumented command
MSMASS" "PERI - Ansys undocumented command
PERI" "SPADP - Ansys undocumented command
SPADP" "FLREAD - Ansys undocumented command
FLREAD" "HFPORT - Ansys undocumented command
HFPORT" "MSMETH - Ansys undocumented command
MSMETH" "PLFSS - Ansys undocumented command
PLFSS" "SPARM - Ansys undocumented command
SPARM" "FLOTRAN - Ansys undocumented command
FLOTRAN" "HFSCAT - Ansys undocumented command
HFSCAT" "MSMIR - Ansys undocumented command
MSMIR" "PLSCH - Ansys undocumented command
PLSCH" "SPFSS - Ansys undocumented command
SPFSS" "HFADP - Ansys undocumented command
HFADP" "ICE - Ansys undocumented command
ICE" "MSNOMF - Ansys undocumented command
MSNOMF" "PLSYZ - Ansys undocumented command
PLSYZ" "SPICE - Ansys undocumented command
SPICE" "HFARRAY - Ansys undocumented command
HFARRAY" "ICEDELE - Ansys undocumented command
ICEDELE" "MSPROP - Ansys undocumented command
MSPROP" "PLTD - Ansys undocumented command
PLTD" "SPSCAN - Ansys undocumented command
SPSCAN" "HFDEEM - Ansys undocumented command
HFDEEM" "ICELIST - Ansys undocumented command
ICELIST" "MSQUAD - Ansys undocumented command
MSQUAD" "PLTLINE - Ansys undocumented command
PLTLINE" "SPSWP - Ansys undocumented command
SPSWP" "HFEIGOPT - Ansys undocumented command
HFEIGOPT" "ICVFRC - Ansys undocumented command
ICVFRC" "MSRELAX - Ansys undocumented command
MSRELAX" "PLVFRC - Ansys undocumented command
PLVFRC" "HFEREFINE - Ansys undocumented command
HFEREFINE" "LPRT - Ansys undocumented command
LPRT" "MSSOLU - Ansys undocumented command
MSSOLU" "/PICE - Ansys undocumented command
/PICE" "HFMODPRT - Ansys undocumented command
HFMODPRT" "MSADV - Ansys undocumented command
MSADV" "MSSPEC - Ansys undocumented command
MSSPEC" "PLWAVE - Ansys undocumented command
PLWAVE" "HFPA - Ansys undocumented command
HFPA" "MSCAP - Ansys undocumented command
MSCAP" "MSTERM - Ansys undocumented command
MSTERM" "PRSYZ - Ansys undocumented command
PRSYZ")
"Help strings for the parameters of Ansys keywords.")

(defconst ansys-undocumented-command-regexp
"\\(?:\\*\\(?:EVAL\\|MOONEY\\)\\|/\\(?:OPT\\|P\\(?:ICE\\|METH\\)\\|RUNST\\(?:AT\\)?\\|SSS\\|TRACK\\|V\\(?:ERIFY\\|T\\)\\|WB\\|XML\\)\\|A\\(?:LPFILL\\|R\\(?:COLLAPSE\\|DETACH\\|FILL\\|MERGE\\|SPLIT\\)\\|SLN\\)\\|C\\(?:MBLOCK\\|NTR\\|WZPLOT\\)\\|E\\(?:ALL\\|BLOCK\\)\\|F\\(?:IPLOT\\|L\\(?:DATA1-40\\|ITEM\\|O\\(?:CHECK\\|TRAN\\)\\|READ\\)\\)\\|GAP\\(?:FINISH\\|LIST\\|MERGE\\|\\(?:OP\\|PLO\\)T\\)\\|HF\\(?:A\\(?:DP\\|RRAY\\)\\|DEEM\\|E\\(?:IGOPT\\|REFINE\\)\\|MODPRT\\|P\\(?:A\\|CSWP\\|O\\(?:RT\\|WER\\)\\)\\|SCAT\\)\\|IC\\(?:E\\(?:DELE\\|LIST\\)?\\|VFRC\\)\\|L\\(?:N\\(?:COLLAPSE\\|DETACH\\|FILL\\|MERGE\\|SPLIT\\)\\|PRT\\|SLN\\)\\|MS\\(?:ADV\\|CAP\\|DATA\\|M\\(?:ASS\\|ETH\\|IR\\)\\|NOMF\\|PROP\\|QUAD\\|RELAX\\|S\\(?:OLU\\|PEC\\)\\|TERM\\|VARY\\)\\|N\\(?:ALL\\|BLOCK\\|ELE\\)\\|OP\\(?:A\\(?:DD\\|NL\\)\\|CLR\\|D\\(?:ATA\\|EL\\)\\|E\\(?:QN\\|XE\\)\\|F\\(?:\\(?:AC\\|RS\\)T\\)\\|GRAD\\|KEEP\\|L\\(?:FA\\|GR\\|IST\\|OOP\\|SW\\)\\|MAKE\\|PRNT\\|R\\(?:AND\\|ESU\\|FA\\|GR\\|SW\\)\\|S\\(?:AVE\\|EL\\|\\(?:UB\\|WEE\\)P\\)\\|TYPE\\|\\(?:USE\\|VA\\)R\\)\\|P\\(?:CONV\\|E\\(?:MOPTS\\|RI\\|XCLUDE\\)\\|G\\(?:R\\(?:APH\\|SET\\)\\|\\(?:S\\(?:AV\\|EL\\)\\|WRIT\\)E\\)\\|I\\(?:LE\\(?:CALC\\|DISPSET\\|GEN\\|LOAD\\|MASS\\|RUN\\|S\\(?:EL\\|TIF\\)\\)\\|NCLUDE\\)\\|L\\(?:CONV\\|FSS\\|S\\(?:CH\\|YZ\\)\\|T\\(?:D\\|LINE\\)\\|V\\(?:AROPT\\|FRC\\)\\|WAVE\\)\\|M\\(?:ETH\\|OPTS\\)\\|OUTRES\\|P\\(?:LOT\\|RANGE\\)\\|R\\(?:CONV\\|ECISION\\|SYZ\\|VAROPT\\)\\|SOLVE\\)\\|QFACT\\|R\\(?:ALL\\|FILSZ\\|ITER\\|MEMRY\\|S\\(?:PEED\\|TAT\\)\\|\\(?:TIMS\\|WFRN\\)T\\)\\|S\\(?:ARPLOT\\|HSD\\|L\\(?:[PS]PLOT\\)\\|OLCONTROL\\|P\\(?:A\\(?:DP\\|RM\\)\\|FSS\\|ICE\\|S\\(?:CAN\\|WP\\)\\)\\)\\|T\\(?:O\\(?:COMP\\|DEF\\|EXE\\|FREQ\\|GRAPH\\|L\\(?:IST\\|OOP\\)\\|P\\(?:\\(?:LO\\|RIN\\)T\\)\\|STAT\\|T\\(?:AL\\|YPE\\)\\|VAR\\)\\|Z\\(?:AMESH\\|DELE\\|EGEN\\)\\)\\|V\\(?:CVFILL\\|T\\(?:CLR\\|DISC\\|EVAL\\|FREQ\\|GEOM\\|IN\\|M\\(?:ETH\\|P\\)\\|OP\\|POST\\|R\\(?:EAL\\|FIL\\|SLT\\)\\|S\\(?:EC\\|FE\\|L\\|TAT\\)\\|TEMP\\|VMOD\\)\\)\\|X\\(?:MLO\\|VAROPT\\)\\|~\\(?:CFIN\\|EUI\\)\\)"
"Regexp of commands not documented in the Ansys
manuals.  Seen mainly in Workbench output files and Ansys
verification models.")

