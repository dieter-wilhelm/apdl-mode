;;; apdl-keyword.el --- APDL-Mode completion and highlighting variables -*- lexical-binding:t -*-

;; Copyright (C) 2006 - 2020 H. Dieter Wilhelm

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Version: 20.2.0
;; Package-Requires: ((emacs "25"))
;; Keywords: languages, convenience, tools, Ansys, APDL
;; URL: https://github.com/dieter-wilhelm/apdl-mode

;; Maintainer: H. Dieter Wilhelm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This lisp script is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;; Permission is granted to distribute copies of this lisp script
;; provided the copyright notice and this permission are preserved in
;; all copies.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:
;; This file was built by fontification.el!

;;; Code:

(defconst apdl-help-index
'(("*ABBR" "ans_cmd/Hlp_C_ABBR.html") ("*AFUN" "ans_cmd/Hlp_C_AFUN.html") ("*ASK" "ans_cmd/Hlp_C_ASK.html") ("*AXPY" "ans_cmd/Hlp_C_AXPY.html") ("*CFCLOS" "ans_cmd/Hlp_C_CFCLOS.html") ("*CFOPEN" "ans_cmd/Hlp_C_CFOPEN.html") ("*CFWRITE" "ans_cmd/Hlp_C_CFWRITE.html") ("*COMP" "ans_cmd/Hlp_C_COMP.html") ("*CREATE" "ans_cmd/Hlp_C_CREATE.html") ("*CYCLE" "ans_cmd/Hlp_C_CYCLE.html") ("*DEL" "ans_cmd/Hlp_C_DEL.html") ("*DIM" "ans_cmd/Hlp_C_DIM.html") ("*DMAT" "ans_cmd/Hlp_C_DMAT.html") ("*DO" "ans_cmd/Hlp_C_DO.html") ("*DOT" "ans_cmd/Hlp_C_DOT.html") ("*DOWHILE" "ans_cmd/Hlp_C_DOWHILE.html") ("*EIGEN" "ans_cmd/Hlp_C_EIGEN.html") ("*ELSE" "ans_cmd/Hlp_C_ELSE.html") ("*ELSEIF" "ans_cmd/Hlp_C_ELSEIF.html") ("*END" "ans_cmd/Hlp_C_END.html") ("*ENDDO" "ans_cmd/Hlp_C_ENDDO.html") ("*ENDIF" "ans_cmd/Hlp_C_ENDIF.html") ("*EXIT" "ans_cmd/Hlp_C_EXIT_st.html") ("*EXPORT" "ans_cmd/Hlp_C_EXPORT.html") ("*FFT" "ans_cmd/Hlp_C_FFT.html") ("*FREE" "ans_cmd/Hlp_C_FREE.html") ("*GET" "ans_cmd/Hlp_C_GET.html") ("*GO" "ans_cmd/Hlp_C_GO_st.html") ("*IF" "ans_cmd/Hlp_C_IF.html") ("*INIT" "ans_cmd/Hlp_C_INIT.html") ("*ITENGINE" "ans_cmd/Hlp_C_ITENGINE.html") ("*LIST" "ans_cmd/Hlp_C_LIST_st.html") ("*LSBAC" "ans_cmd/Hlp_C_LSBAC.html") ("*LSDUMP" "ans_cmd/Hlp_C_LSDUMP.html") ("*LSENGINE" "ans_cmd/Hlp_C_LSENGINE.html") ("*LSFACTOR" "ans_cmd/Hlp_C_LSFACTOR.html") ("*LSRESTORE" "ans_cmd/Hlp_C_LSRESTORE.html") ("*MERGE" "ans_cmd/Hlp_C_MERGE.html") ("*MFOURI" "ans_cmd/Hlp_C_MFOURI.html") ("*MFUN" "ans_cmd/Hlp_C_MFUN.html") ("*MOPER" "ans_cmd/Hlp_C_MOPER.html") ("*MSG" "ans_cmd/Hlp_C_MSG.html") ("*MULT" "ans_cmd/Hlp_C_MULT.html") ("*MWRITE" "ans_cmd/Hlp_C_MWRITE_st.html") ("*NRM" "ans_cmd/Hlp_C_NRM.html") ("*PRINT" "ans_cmd/Hlp_C_PRINT_a.html") ("*REMOVE" "ans_cmd/Hlp_C_REMOVE.html") ("*RENAME" "ans_cmd/Hlp_C_RENAME_a.html") ("*REPEAT" "ans_cmd/Hlp_C_REPEAT.html") ("*RETURN" "ans_cmd/Hlp_C_RETURN.html") ("*SCAL" "ans_cmd/Hlp_C_SCAL.html") ("*SET" "ans_cmd/Hlp_C_SET_st.html") ("*SMAT" "ans_cmd/Hlp_C_SMAT.html") ("*SORT" "ans_cmd/Hlp_C_SORT_st.html") ("*SREAD" "ans_cmd/Hlp_C_SREAD_st.html") ("*STATUS" "ans_cmd/Hlp_C_STATUS_st.html") ("*TAXIS" "ans_cmd/Hlp_C_TAXIS.html") ("*TOPER" "ans_cmd/Hlp_C_TOPER_st.html") ("*TREAD" "ans_cmd/Hlp_C_TREAD.html") ("*ULIB" "ans_cmd/Hlp_C_ULIB.html") ("*USE" "ans_cmd/Hlp_C_USE.html") ("*VABS" "ans_cmd/Hlp_C_VABS.html") ("*VCOL" "ans_cmd/Hlp_C_VCOL.html") ("*VCUM" "ans_cmd/Hlp_C_VCUM.html") ("*VEC" "ans_cmd/Hlp_C_VEC.html") ("*VEDIT" "ans_cmd/Hlp_C_VEDIT.html") ("*VFACT" "ans_cmd/Hlp_C_VFACT.html") ("*VFILL" "ans_cmd/Hlp_C_VFILL.html") ("*VFUN" "ans_cmd/Hlp_C_VFUN.html") ("*VGET" "ans_cmd/Hlp_C_VGET_st.html") ("*VITRP" "ans_cmd/Hlp_C_VITRP.html") ("*VLEN" "ans_cmd/Hlp_C_VLEN.html") ("*VMASK" "ans_cmd/Hlp_C_VMASK.html") ("*VOPER" "ans_cmd/Hlp_C_VOPER.html") ("*VPLOT" "ans_cmd/Hlp_C_VPLOT_st.html") ("*VPUT" "ans_cmd/Hlp_C_VPUT_st.html") ("*VREAD" "ans_cmd/Hlp_C_VREAD.html") ("*VSCFUN" "ans_cmd/Hlp_C_VSCFUN.html") ("*VSTAT" "ans_cmd/Hlp_C_VSTAT.html") ("*VWRITE" "ans_cmd/Hlp_C_VWRITE.html") ("*WRK" "ans_cmd/Hlp_C_WRK.html") ("*XPL" "ans_cmd/Hlp_C_XPL.html") ("/AN3D" "ans_cmd/Hlp_C_AN3D.html") ("/ANFILE" "ans_cmd/Hlp_C_ANFILE.html") ("/ANGLE" "ans_cmd/Hlp_C_ANGLE.html") ("/ANNOT" "ans_cmd/Hlp_C_ANNOT.html") ("/ANUM" "ans_cmd/Hlp_C_ANUM.html") ("/ASSIGN" "ans_cmd/Hlp_C_ASSIGN.html") ("/AUTO" "ans_cmd/Hlp_C_AUTO.html") ("/AUX12" "ans_cmd/Hlp_C_AUX12.html") ("/AUX15" "ans_cmd/Hlp_C_AUX15.html") ("/AUX2" "ans_cmd/Hlp_C_AUX2.html") ("/AUX3" "ans_cmd/Hlp_C_AUX3_sl.html") ("/AXLAB" "ans_cmd/Hlp_C_AXLAB.html") ("/BATCH" "ans_cmd/Hlp_C_BATCH.html") ("/CFORMAT" "ans_cmd/Hlp_C_CFORMAT_sl.html") ("/CLABEL" "ans_cmd/Hlp_C_CLABEL.html") ("/CLEAR" "ans_cmd/Hlp_C_CLEAR.html") ("/CLOG" "ans_cmd/Hlp_C_CLOG_sl.html") ("/CMAP" "ans_cmd/Hlp_C_CMAP.html") ("/COLOR" "ans_cmd/Hlp_C_COLOR.html") ("/COM" "ans_cmd/Hlp_C_COM.html") ("/CONFIG" "ans_cmd/Hlp_C_CONFIG.html") ("/CONTOUR" "ans_cmd/Hlp_C_CONTOUR.html") ("/COPY" "ans_cmd/Hlp_C_COPY.html") ("/CPLANE" "ans_cmd/Hlp_C_CPLANE.html") ("/CTYPE" "ans_cmd/Hlp_C_CTYPE.html") ("/CVAL" "ans_cmd/Hlp_C_CVAL.html") ("/CWD" "ans_cmd/Hlp_C_CWD_sl.html") ("/CYCEXPAND" "ans_cmd/Hlp_C_CYCEXPAND_sl.html") ("/DELETE" "ans_cmd/Hlp_C_DELETE_sl.html") ("/DEVICE" "ans_cmd/Hlp_C_DEVICE.html") ("/DFLAB" "ans_cmd/Hlp_C_DFLAB.html") ("/DIRECTORY" "ans_cmd/Hlp_C_DIRECTORY.html") ("/DIST" "ans_cmd/Hlp_C_DIST.html") ("/DSCALE" "ans_cmd/Hlp_C_DSCALE_sl.html") ("/DV3D" "ans_cmd/Hlp_C_DV3D.html") ("/EDGE" "ans_cmd/Hlp_C_EDGE.html") ("/EFACET" "ans_cmd/Hlp_C_EFACET.html") ("/EOF" "ans_cmd/Hlp_C_EOF.html") ("/ERASE" "ans_cmd/Hlp_C_ERASE_sl.html") ("/ESHAPE" "ans_cmd/Hlp_C_ESHAPE.html") ("/EXIT" "ans_cmd/Hlp_C_EXIT.html") ("/EXPAND" "ans_cmd/Hlp_C_EXPAND_sl.html") ("/FACET" "ans_cmd/Hlp_C_FACET.html") ("/FCOMP" "ans_cmd/Hlp_C_FCOMP_sl.html") ("/FDELE" "ans_cmd/Hlp_C_FDELE_sl.html") ("/FILNAME" "ans_cmd/Hlp_C_FILNAME.html") ("/FOCUS" "ans_cmd/Hlp_C_FOCUS.html") ("/FORMAT" "ans_cmd/Hlp_C_FORMAT.html") ("/GCMD" "ans_cmd/Hlp_C_GCMD.html") ("/GCOLUMN" "ans_cmd/Hlp_C_GCOLUMN.html") ("/GFILE" "ans_cmd/Hlp_C_GFILE.html") ("/GFORMAT" "ans_cmd/Hlp_C_GFORMAT.html") ("/GLINE" "ans_cmd/Hlp_C_GLINE.html") ("/GMARKER" "ans_cmd/Hlp_C_GMARKER.html") ("/GO" "ans_cmd/Hlp_C_GO.html") ("/GOLIST" "ans_cmd/Hlp_C_GOLIST.html") ("/GOPR" "ans_cmd/Hlp_C_GOPR.html") ("/GRAPHICS" "ans_cmd/Hlp_C_GRAPHICS.html") ("/GRESUME" "ans_cmd/Hlp_C_GRESUME.html") ("/GRID" "ans_cmd/Hlp_C_GRID.html") ("/GROPT" "ans_cmd/Hlp_C_GROPT.html") ("/GRTYP" "ans_cmd/Hlp_C_GRTYP.html") ("/GSAVE" "ans_cmd/Hlp_C_GSAVE.html") ("/GST" "ans_cmd/Hlp_C_GST.html") ("/GTHK" "ans_cmd/Hlp_C_GTHK.html") ("/GTYPE" "ans_cmd/Hlp_C_GTYPE.html") ("/HBC" "ans_cmd/Hlp_C_HBC.html") ("/HEADER" "ans_cmd/Hlp_C_HEADER.html") ("/ICLWID" "ans_cmd/Hlp_C_ICLWID.html") ("/ICSCALE" "ans_cmd/Hlp_C_ICSCALE.html") ("/IMAGE" "ans_cmd/Hlp_C_IMAGE.html") ("/INPUT" "ans_cmd/Hlp_C_INPUT.html") ("/INQUIRE" "ans_cmd/Hlp_C_INQUIRE.html") ("/LARC" "ans_cmd/Hlp_C_LARC_sl.html") ("/LIGHT" "ans_cmd/Hlp_C_LIGHT.html") ("/LINE" "ans_cmd/Hlp_C_LINE_sl.html") ("/LSPEC" "ans_cmd/Hlp_C_LSPEC.html") ("/LSYMBOL" "ans_cmd/Hlp_C_LSYMBOL.html") ("/MAP" "ans_cmd/Hlp_C_MAP_s.html") ("/MENU" "ans_cmd/Hlp_C_MENU.html") ("/MKDIR" "ans_cmd/Hlp_C_MKDIR.html") ("/MPLIB" "ans_cmd/Hlp_C_MPLIB.html") ("/MREP" "ans_cmd/Hlp_C_MREP.html") ("/MSTART" "ans_cmd/Hlp_C_MSTART.html") ("/NERR" "ans_cmd/Hlp_C_NERR.html") ("/NOERASE" "ans_cmd/Hlp_C_NOERASE.html") ("/NOLIST" "ans_cmd/Hlp_C_NOLIST.html") ("/NOPR" "ans_cmd/Hlp_C_NOPR.html") ("/NORMAL" "ans_cmd/Hlp_C_NORMAL.html") ("/NUMBER" "ans_cmd/Hlp_C_NUMBER.html") ("/OUTPUT" "ans_cmd/Hlp_C_OUTPUT.html") ("/PAGE" "ans_cmd/Hlp_C_PAGE.html") ("/PBC" "ans_cmd/Hlp_C_PBC.html") ("/PBF" "ans_cmd/Hlp_C_PBF.html") ("/PCIRCLE" "ans_cmd/Hlp_C_PCIRCLE.html") ("/PLOPTS" "ans_cmd/Hlp_C_PLOPTS.html") ("/PMACRO" "ans_cmd/Hlp_C_PMACRO.html") ("/PMORE" "ans_cmd/Hlp_C_PMORE.html") ("/PNUM" "ans_cmd/Hlp_C_PNUM.html") ("/POLYGON" "ans_cmd/Hlp_C_POLYGON.html") ("/POST1" "ans_cmd/Hlp_C_POST1.html") ("/POST26" "ans_cmd/Hlp_C_POST26.html") ("/PREP7" "ans_cmd/Hlp_C_PREP7.html") ("/PSEARCH" "ans_cmd/Hlp_C_PSEARCH.html") ("/PSF" "ans_cmd/Hlp_C_PSF.html") ("/PSPEC" "ans_cmd/Hlp_C_PSPEC_sl.html") ("/PSTATUS" "ans_cmd/Hlp_C_PSTATUS.html") ("/PSYMB" "ans_cmd/Hlp_C_PSYMB.html") ("/PWEDGE" "ans_cmd/Hlp_C_PWEDGE.html") ("/QUIT" "ans_cmd/Hlp_C_QUIT.html") ("/RATIO" "ans_cmd/Hlp_C_RATIO.html") ("/RENAME" "ans_cmd/Hlp_C_RENAME.html") ("/REPLOT" "ans_cmd/Hlp_C_REPLOT.html") ("/RESET" "ans_cmd/Hlp_C_RESET_sl.html") ("/RGB" "ans_cmd/Hlp_C_RGB.html") ("/RMDIR" "ans_cmd/Hlp_C_RMDIR.html") ("/SECLIB" "ans_cmd/Hlp_C_SECLIB.html") ("/SEG" "ans_cmd/Hlp_C_SEG.html") ("/SHADE" "ans_cmd/Hlp_C_SHADE.html") ("/SHOW" "ans_cmd/Hlp_C_SHOW.html") ("/SHRINK" "ans_cmd/Hlp_C_SHRINK.html") ("/SMBC" "ans_cmd/Hlp_C_SMBC_sl.html") ("/SOLU" "ans_cmd/Hlp_C_SOLU_sl.html") ("/SSCALE" "ans_cmd/Hlp_C_SSCALE.html") ("/STATUS" "ans_cmd/Hlp_C_STATUS.html") ("/STITLE" "ans_cmd/Hlp_C_STITLE.html") ("/SYP" "ans_cmd/Hlp_C_SYP.html") ("/SYS" "ans_cmd/Hlp_C_SYS.html") ("/TEE" "ans_cmd/Hlp_C_TEE_sl.html") ("/TITLE" "ans_cmd/Hlp_C_TITLE.html") ("/TLABEL" "ans_cmd/Hlp_C_TLABEL.html") ("/TRIAD" "ans_cmd/Hlp_C_TRIAD.html") ("/TRLCY" "ans_cmd/Hlp_C_TRLCY.html") ("/TSPEC" "ans_cmd/Hlp_C_TSPEC.html") ("/TXTRE" "ans_cmd/Hlp_C_TXTRE.html") ("/TYPE" "ans_cmd/Hlp_C_TYPE_sl.html") ("/UCMD" "ans_cmd/Hlp_C_UCMD.html") ("/UDOC" "ans_cmd/Hlp_C_UDOC_sl.html") ("/UI" "ans_cmd/Hlp_C_UI.html") ("/UIS" "ans_cmd/Hlp_C_UIS.html") ("/UNITS" "ans_cmd/Hlp_C_UNITS.html") ("/USER" "ans_cmd/Hlp_C_USER.html") ("/VCONE" "ans_cmd/Hlp_C_VCONE.html") ("/VIEW" "ans_cmd/Hlp_C_VIEW.html") ("/VSCALE" "ans_cmd/Hlp_C_VSCALE.html") ("/VUP" "ans_cmd/Hlp_C_VUP.html") ("/WAIT" "ans_cmd/Hlp_C_WAIT.html") ("/WINDOW" "ans_cmd/Hlp_C_WINDOW.html") ("/XFRM" "ans_cmd/Hlp_C_XFRM.html") ("/XRANGE" "ans_cmd/Hlp_C_XRANGE.html") ("/YRANGE" "ans_cmd/Hlp_C_YRANGE.html") ("/ZOOM" "ans_cmd/Hlp_C_ZOOM.html") ("11" "ans_elem/Hlp_E_LINK11.html") ("110" "ans_elem/Hlp_E_INFIN110.html") ("111" "ans_elem/Hlp_E_INFIN111.html") ("116" "ans_elem/Hlp_E_FLUID116.html") ("12" "ans_arch/Hlp_E_CONTAC12.html") ("121" "ans_elem/Hlp_E_PLANE121.html") ("122" "ans_elem/Hlp_E_SOLID122.html") ("123" "ans_elem/Hlp_E_SOLID123.html") ("124" "ans_elem/Hlp_E_CIRCU124.html") ("125" "ans_elem/Hlp_E_CIRCU125.html") ("126" "ans_elem/Hlp_E_TRANS126.html") ("129" "ans_elem/Hlp_E_FLUID129.html") ("13" "ans_elem/Hlp_E_PLANE13.html") ("130" "ans_elem/Hlp_E_FLUID130.html") ("131" "ans_elem/Hlp_E_SHELL131.html") ("132" "ans_elem/Hlp_E_SHELL132.html") ("136" "ans_elem/Hlp_E_FLUID136.html") ("138" "ans_elem/Hlp_E_FLUID138.html") ("139" "ans_elem/Hlp_E_FLUID139.html") ("14" "ans_elem/Hlp_E_COMBIN14.html") ("144" "ans_elem/Hlp_E_ROM144.html") ("151" "ans_elem/Hlp_E_SURF151.html") ("152" "ans_elem/Hlp_E_SURF152.html") ("153" "ans_elem/Hlp_E_SURF153.html") ("154" "ans_elem/Hlp_E_SURF154.html") ("155" "ans_elem/Hlp_E_SURF155.html") ("156" "ans_elem/Hlp_E_SURF156.html") ("157" "ans_elem/Hlp_E_SHELL157.html") ("159" "ans_elem/Hlp_E_SURF159.html") ("16" "ans_arch/Hlp_E_PIPE16.html") ("169" "ans_elem/Hlp_E_TARGE169.html") ("170" "ans_elem/Hlp_E_TARGE170.html") ("171" "ans_arch/Hlp_E_CONTA171.html") ("172" "ans_elem/Hlp_E_CONTA172.html") ("173" "ans_arch/Hlp_E_CONTA173.html") ("174" "ans_elem/Hlp_E_CONTA174.html") ("175" "ans_elem/Hlp_E_CONTA175.html") ("176" "ans_arch/Hlp_E_CONTA176.html") ("177" "ans_elem/Hlp_E_CONTA177.html") ("178" "ans_elem/Hlp_E_CONTA178.html") ("179" "ans_elem/Hlp_E_PRETS179.html") ("18" "ans_arch/Hlp_E_PIPE18.html") ("180" "ans_elem/Hlp_E_LINK180.html") ("181" "ans_elem/Hlp_E_SHELL181.html") ("182" "ans_elem/Hlp_E_PLANE182.html") ("183" "ans_elem/Hlp_E_PLANE183.html") ("184" "ans_elem/Hlp_E_MPC184.html") ("184cyl" "ans_elem/Hlp_E_MPC184cyl.html") ("184gen" "ans_elem/Hlp_E_MPC184gen.html") ("184link" "ans_elem/Hlp_E_MPC184link.html") ("184orie" "ans_elem/Hlp_E_MPC184orie.html") ("184plan" "ans_elem/Hlp_E_MPC184plan.html") ("184poin" "ans_elem/Hlp_E_MPC184poin.html") ("184revo" "ans_elem/Hlp_E_MPC184revo.html") ("184scr" "ans_elem/Hlp_E_MPC184scr.html") ("184slid" "ans_elem/Hlp_E_MPC184slid.html") ("184slot" "ans_elem/Hlp_E_MPC184slot.html") ("184sphe" "ans_elem/Hlp_E_MPC184sphe.html") ("184tran" "ans_elem/Hlp_E_MPC184tran.html") ("184univ" "ans_elem/Hlp_E_MPC184univ.html") ("184weld" "ans_elem/Hlp_E_MPC184weld.html") ("185" "ans_elem/Hlp_E_SOLID185.html") ("186" "ans_elem/Hlp_E_SOLID186.html") ("187" "ans_elem/Hlp_E_SOLID187.html") ("188" "ans_elem/Hlp_E_BEAM188.html") ("189" "ans_elem/Hlp_E_BEAM189.html") ("190" "ans_elem/Hlp_E_SOLSH190.html") ("192" "ans_elem/Hlp_E_INTER192.html") ("193" "ans_elem/Hlp_E_INTER193.html") ("194" "ans_elem/Hlp_E_INTER194.html") ("195" "ans_elem/Hlp_E_INTER195.html") ("200" "ans_elem/Hlp_E_MESH200.html") ("201" "ans_elem/Hlp_E_FOLLW201.html") ("202" "ans_elem/Hlp_E_INTER202.html") ("203" "ans_elem/Hlp_E_INTER203.html") ("204" "ans_elem/Hlp_E_INTER204.html") ("205" "ans_elem/Hlp_E_INTER205.html") ("208" "ans_elem/Hlp_E_SHELL208.html") ("209" "ans_elem/Hlp_E_SHELL209.html") ("21" "ans_elem/Hlp_E_MASS21.html") ("212" "ans_elem/Hlp_E_CPT212.html") ("213" "ans_elem/Hlp_E_CPT213.html") ("214" "ans_elem/Hlp_E_COMBI214.html") ("215" "ans_elem/Hlp_E_CPT215.html") ("216" "ans_elem/Hlp_E_CPT216.html") ("217" "ans_elem/Hlp_E_CPT217.html") ("218" "ans_elem/Hlp_E_FLUID218.html") ("220" "ans_elem/Hlp_E_FLUID220.html") ("221" "ans_elem/Hlp_E_FLUID221.html") ("222" "ans_elem/Hlp_E_PLANE222.html") ("223" "ans_elem/Hlp_E_PLANE223.html") ("226" "ans_elem/Hlp_E_SOLID226.html") ("227" "ans_elem/Hlp_E_SOLID227.html") ("230" "ans_elem/Hlp_E_PLANE230.html") ("231" "ans_elem/Hlp_E_SOLID231.html") ("232" "ans_elem/Hlp_E_SOLID232.html") ("233" "ans_elem/Hlp_E_PLANE233.html") ("236" "ans_elem/Hlp_E_SOLID236.html") ("237" "ans_elem/Hlp_E_SOLID237.html") ("238" "ans_elem/Hlp_E_PLANE238.html") ("239" "ans_elem/Hlp_E_SOLID239.html") ("240" "ans_elem/Hlp_E_SOLID240.html") ("241" "ans_elem/Hlp_E_HSFLD241.html") ("242" "ans_elem/Hlp_E_HSFLD242.html") ("25" "ans_elem/Hlp_E_PLANE25.html") ("250" "ans_elem/Hlp_E_COMBI250.html") ("251" "ans_elem/Hlp_E_SURF251.html") ("252" "ans_elem/Hlp_E_SURF252.html") ("257" "ans_elem/Hlp_E_INFIN257.html") ("263" "ans_elem/Hlp_E_REINF263.html") ("264" "ans_elem/Hlp_E_REINF264.html") ("265" "ans_elem/Hlp_E_REINF265.html") ("27" "ans_elem/Hlp_E_MATRIX27.html") ("272" "ans_elem/Hlp_E_SOLID272.html") ("273" "ans_elem/Hlp_E_SOLID273.html") ("278" "ans_elem/Hlp_E_SOLID278.html") ("279" "ans_elem/Hlp_E_SOLID279.html") ("280" "ans_elem/Hlp_E_CABLE280.html") ("281" "ans_elem/Hlp_E_SHELL281.html") ("285" "ans_elem/Hlp_E_SOLID285.html") ("288" "ans_elem/Hlp_E_PIPE288.html") ("289" "ans_elem/Hlp_E_PIPE289.html") ("29" "ans_elem/Hlp_E_FLUID29.html") ("290" "ans_elem/Hlp_E_ELBOW290.html") ("291" "ans_elem/Hlp_E_SOLID291.html") ("292" "ans_elem/Hlp_E_PLANE292.html") ("293" "ans_elem/Hlp_E_PLANE293.html") ("30" "ans_elem/Hlp_E_FLUID30.html") ("300" "ans_elem/Hlp_E_USER300.html") ("31" "ans_elem/Hlp_E_LINK31.html") ("33" "ans_elem/Hlp_E_LINK33.html") ("34" "ans_elem/Hlp_E_LINK34.html") ("35" "ans_elem/Hlp_E_PLANE35.html") ("36" "ans_elem/Hlp_E_SOURC36.html") ("37" "ans_elem/Hlp_E_COMBIN37.html") ("38" "ans_elem/Hlp_E_FLUID38.html") ("39" "ans_elem/Hlp_E_COMBIN39.html") ("4" "ans_arch/Hlp_E_BEAM4.html") ("40" "ans_elem/Hlp_E_COMBIN40.html") ("42" "ans_arch/Hlp_E_PLANE42.html") ("45" "ans_arch/Hlp_E_SOLID45.html") ("47" "ans_elem/Hlp_E_INFIN47.html") ("5" "ans_elem/Hlp_E_SOLID5.html") ("50" "ans_elem/Hlp_E_MATRIX50.html") ("52" "ans_arch/Hlp_E_CONTAC52.html") ("55" "ans_elem/Hlp_E_PLANE55.html") ("59" "ans_arch/Hlp_E_PIPE59.html") ("61" "ans_elem/Hlp_E_SHELL61.html") ("63" "ans_arch/Hlp_E_SHELL63.html") ("65" "ans_arch/Hlp_E_SOLID65.html") ("68" "ans_elem/Hlp_E_LINK68.html") ("70" "ans_elem/Hlp_E_SOLID70.html") ("71" "ans_elem/Hlp_E_MASS71.html") ("75" "ans_elem/Hlp_E_PLANE75.html") ("77" "ans_elem/Hlp_E_PLANE77.html") ("78" "ans_elem/Hlp_E_PLANE78.html") ("79" "ans_arch/Hlp_E_FLUID79.html") ("80" "ans_arch/Hlp_E_FLUID80.html") ("81" "ans_arch/Hlp_E_FLUID81.html") ("82" "ans_arch/Hlp_E_PLANE82.html") ("83" "ans_elem/Hlp_E_PLANE83.html") ("87" "ans_elem/Hlp_E_SOLID87.html") ("90" "ans_elem/Hlp_E_SOLID90.html") ("92" "ans_arch/Hlp_E_SOLID92.html") ("94" "ans_elem/Hlp_E_CIRCU94.html") ("95" "ans_arch/Hlp_E_SOLID95.html") ("96" "ans_elem/Hlp_E_SOLID96.html") ("98" "ans_elem/Hlp_E_SOLID98.html") ("A" "ans_cmd/Hlp_C_A.html") ("AADD" "ans_cmd/Hlp_C_AADD.html") ("AATT" "ans_cmd/Hlp_C_AATT.html") ("ABBRES" "ans_cmd/Hlp_C_ABBRES.html") ("ABBSAV" "ans_cmd/Hlp_C_ABBSAV.html") ("ABEXTRACT" "ans_cmd/Hlp_C_ABEXTRACT.html") ("ABS" "ans_cmd/Hlp_C_ABS.html") ("ACCAT" "ans_cmd/Hlp_C_ACCAT.html") ("ACCOPTION" "ans_cmd/Hlp_C_ACCOPTION.html") ("ACEL" "ans_cmd/Hlp_C_ACEL.html") ("ACLEAR" "ans_cmd/Hlp_C_ACLEAR.html") ("ADAMS" "ans_cmd/Hlp_C_ADAMS.html") ("ADD" "ans_cmd/Hlp_C_ADD.html") ("ADDAM" "ans_cmd/Hlp_C_ADDAM.html") ("ADELE" "ans_cmd/Hlp_C_ADELE.html") ("ADGL" "ans_cmd/Hlp_C_ADGL.html") ("ADRAG" "ans_cmd/Hlp_C_ADRAG.html") ("AEROCOEFF" "ans_cmd/Hlp_C_AEROCOEFF.html") ("AESIZE" "ans_cmd/Hlp_C_AESIZE.html") ("AFILLT" "ans_cmd/Hlp_C_AFILLT.html") ("AFLIST" "ans_cmd/Hlp_C_AFLIST.html") ("AFSURF" "ans_cmd/Hlp_C_AFSURF.html") ("AGEN" "ans_cmd/Hlp_C_AGEN.html") ("AGLUE" "ans_cmd/Hlp_C_AGLUE.html") ("AINA" "ans_cmd/Hlp_C_AINA.html") ("AINP" "ans_cmd/Hlp_C_AINP.html") ("AINV" "ans_cmd/Hlp_C_AINV.html") ("AL" "ans_cmd/Hlp_C_AL.html") ("ALIST" "ans_cmd/Hlp_C_ALIST.html") ("\"ALL\"BEAMS" "ans_elem/Hlp_E_CH3_2.html#allbeams") ("\"ALL\"CABLES" "ans_elem/Hlp_E_CH3_2.html#allcables") ("\"ALL\"CIRCUS" "ans_elem/Hlp_E_CH3_2.html#allcircu") ("\"ALL\"COMBINS" "ans_elem/Hlp_E_CH3_2.html#allcombin") ("\"ALL\"COMBIS" "ans_elem/Hlp_E_CH3_2.html#allcombin") ("\"ALL\"CONTACS" "ans_elem/Hlp_E_CH3_2.html#allcontac") ("\"ALL\"CONTAS" "ans_elem/Hlp_E_CH3_2.html#allcontac") ("\"ALL\"CPTS" "ans_elem/Hlp_E_CH3_2.html#allcpt") ("\"ALL\"FLUIDS" "ans_elem/Hlp_E_CH3_2.html#allfluid") ("\"ALL\"FOLLW" "ans_elem/Hlp_E_CH3_2.html#allfollw") ("\"ALL\"HSFLDS" "ans_elem/Hlp_E_CH3_2.html#allhsfld") ("\"ALL\"INFINS" "ans_elem/Hlp_E_CH3_2.html#allinfin") ("\"ALL\"INTERS" "ans_elem/Hlp_E_CH3_2.html#allinter") ("\"ALL\"LINKS" "ans_elem/Hlp_E_CH3_2.html#alllinks") ("\"ALL\"MASS" "ans_elem/Hlp_E_CH3_2.html#allmass") ("\"ALL\"MATRIXS" "ans_elem/Hlp_E_CH3_2.html#allmatrix") ("\"ALL\"MESHS" "ans_elem/Hlp_E_CH3_2.html#allmesh") ("\"ALL\"MPCS" "ans_elem/Hlp_E_CH3_2.html#allmpc") ("\"ALL\"PIPES" "ans_elem/Hlp_E_CH3_2.html#allpipes") ("\"ALL\"PLANES" "ans_elem/Hlp_E_CH3_2.html#allplanes") ("\"ALL\"PRETS" "ans_elem/Hlp_E_CH3_2.html#allprets") ("\"ALL\"REINF" "ans_elem/Hlp_E_CH3_2.html#allreinf") ("\"ALL\"ROMS" "ans_elem/Hlp_E_CH3_2.html#allrom") ("\"ALL\"SEL" "ans_cmd/Hlp_C_ALLSEL.html") ("\"ALL\"SHELLS" "ans_elem/Hlp_E_CH3_2.html#allshells") ("\"ALL\"SOLIDS" "ans_elem/Hlp_E_CH3_2.html#allsolids") ("\"ALL\"SOLSHS" "ans_elem/Hlp_E_CH3_2.html#allsolsh") ("\"ALL\"SOURCS" "ans_elem/Hlp_E_CH3_2.html#allsourc") ("\"ALL\"SURFS" "ans_elem/Hlp_E_CH3_2.html#allsurf") ("\"ALL\"TARGES" "ans_elem/Hlp_E_CH3_2.html#alltarge") ("\"ALL\"TRANS" "ans_elem/Hlp_E_CH3_2.html#alltrans") ("ALPHAD" "ans_cmd/Hlp_C_ALPHAD.html") ("AMAP" "ans_cmd/Hlp_C_AMAP.html") ("AMBEAM" "ans_cmd/Hlp_C_AMBEAM.html") ("AMBUILD" "ans_cmd/Hlp_C_AMBUILD.html") ("AMENV" "ans_cmd/Hlp_C_AMENV.html") ("AMESH" "ans_cmd/Hlp_C_AMESH.html") ("AMMAT" "ans_cmd/Hlp_C_AMMAT.html") ("AMPOWDER" "ans_cmd/Hlp_C_AMPOWDER.html") ("AMSTEP" "ans_cmd/Hlp_C_AMSTEP.html") ("AMSUPPORTS" "ans_cmd/Hlp_C_AMSUPPORTS.html") ("AMTYPE" "ans_cmd/Hlp_C_AMTYPE.html") ("ANCNTR" "ans_cmd/Hlp_C_ANCNTR.html") ("ANCUT" "ans_cmd/Hlp_C_ANCUT.html") ("ANCYC" "ans_cmd/Hlp_C_ANCYC.html") ("ANDATA" "ans_cmd/Hlp_C_ANDATA.html") ("ANDSCL" "ans_cmd/Hlp_C_ANDSCL.html") ("ANDYNA" "ans_cmd/Hlp_C_ANDYNA.html") ("ANFLOW" "ans_cmd/Hlp_C_ANFLOW.html") ("ANHARM" "ans_cmd/Hlp_C_ANHARM.html") ("ANIM" "ans_cmd/Hlp_C_ANIM.html") ("ANISOS" "ans_cmd/Hlp_C_ANISOS.html") ("ANMODE" "ans_cmd/Hlp_C_ANMODE.html") ("ANORM" "ans_cmd/Hlp_C_ANORM.html") ("ANPRES" "ans_cmd/Hlp_C_ANPRES.html") ("ANSOL" "ans_cmd/Hlp_C_ANSOL.html") ("ANSTOAQWA" "ans_cmd/Hlp_C_ANSTOAQWA.html") ("ANSTOASAS" "ans_cmd/Hlp_C_ANSTOASAS.html") ("ANTIME" "ans_cmd/Hlp_C_ANTIME.html") ("ANTYPE" "ans_cmd/Hlp_C_ANTYPE.html") ("AOFFST" "ans_cmd/Hlp_C_AOFFST.html") ("AOVLAP" "ans_cmd/Hlp_C_AOVLAP.html") ("APLOT" "ans_cmd/Hlp_C_APLOT.html") ("APORT" "ans_cmd/Hlp_C_aport.html") ("APPEND" "ans_cmd/Hlp_C_APPEND.html") ("APTN" "ans_cmd/Hlp_C_APTN.html") ("ARCLEN" "ans_cmd/Hlp_C_ARCLEN.html") ("ARCTRM" "ans_cmd/Hlp_C_ARCTRM.html") ("AREAS" "ans_cmd/Hlp_C_AREAS.html") ("AREFINE" "ans_cmd/Hlp_C_AREFINE.html") ("AREMESH" "ans_cmd/Hlp_C_AREMESH.html") ("AREVERSE" "ans_cmd/Hlp_C_AREVERSE.html") ("AROTAT" "ans_cmd/Hlp_C_AROTAT.html") ("ARSCALE" "ans_cmd/Hlp_C_ARSCALE.html") ("ARSYM" "ans_cmd/Hlp_C_ARSYM.html") ("ASBA" "ans_cmd/Hlp_C_ASBA.html") ("ASBL" "ans_cmd/Hlp_C_ASBL.html") ("ASBV" "ans_cmd/Hlp_C_ASBV.html") ("ASBW" "ans_cmd/Hlp_C_ASBW.html") ("ASCRES" "ans_cmd/Hlp_C_ASCRES.html") ("ASEL" "ans_cmd/Hlp_C_ASEL.html") ("ASIFILE" "ans_cmd/Hlp_C_ASIFILE.html") ("ASKIN" "ans_cmd/Hlp_C_ASKIN.html") ("ASLL" "ans_cmd/Hlp_C_ASLL.html") ("ASLV" "ans_cmd/Hlp_C_ASLV.html") ("ASOL" "ans_cmd/Hlp_C_ASOL.html") ("ASUB" "ans_cmd/Hlp_C_ASUB.html") ("ASUM" "ans_cmd/Hlp_C_ASUM.html") ("ATAN" "ans_cmd/Hlp_C_ATAN.html") ("ATRAN" "ans_cmd/Hlp_C_ATRAN.html") ("ATYPE" "ans_cmd/Hlp_C_ATYPE.html") ("AUTOTS" "ans_cmd/Hlp_C_AUTOTS.html") ("AVPRIN" "ans_cmd/Hlp_C_AVPRIN.html") ("AVRES" "ans_cmd/Hlp_C_AVRES.html") ("AWAVE" "ans_cmd/Hlp_C_AWAVE.html") ("BCSOPTION" "ans_cmd/Hlp_C_BCSOPTION.html") ("BEAM" "ans_elem/Hlp_E_CH3_2.html#allbeams") ("BEAM188" "ans_elem/Hlp_E_BEAM188.html") ("BEAM189" "ans_elem/Hlp_E_BEAM189.html") ("BEAM4" "ans_arch/Hlp_E_BEAM4.html") ("BELLOW" "ans_arch/Hlp_C_BELLOW.html") ("BEND" "ans_arch/Hlp_C_BEND.html") ("BETAD" "ans_cmd/Hlp_C_BETAD.html") ("BF" "ans_cmd/Hlp_C_BF.html") ("BFA" "ans_cmd/Hlp_C_BFA.html") ("BFADELE" "ans_cmd/Hlp_C_BFADELE.html") ("BFALIST" "ans_cmd/Hlp_C_BFALIST.html") ("BFCUM" "ans_cmd/Hlp_C_BFCUM.html") ("BFDELE" "ans_cmd/Hlp_C_BFDELE.html") ("BFE" "ans_cmd/Hlp_C_BFE.html") ("BFECUM" "ans_cmd/Hlp_C_BFECUM.html") ("BFEDELE" "ans_cmd/Hlp_C_BFEDELE.html") ("BFELIST" "ans_cmd/Hlp_C_BFELIST.html") ("BFESCAL" "ans_cmd/Hlp_C_BFESCAL.html") ("BFINT" "ans_cmd/Hlp_C_BFINT.html") ("BFK" "ans_cmd/Hlp_C_BFK.html") ("BFKDELE" "ans_cmd/Hlp_C_BFKDELE.html") ("BFKLIST" "ans_cmd/Hlp_C_BFKLIST.html") ("BFL" "ans_cmd/Hlp_C_BFL.html") ("BFLDELE" "ans_cmd/Hlp_C_BFLDELE.html") ("BFLIST" "ans_cmd/Hlp_C_BFLIST.html") ("BFLLIST" "ans_cmd/Hlp_C_BFLLIST.html") ("BFSCALE" "ans_cmd/Hlp_C_BFSCALE.html") ("BFTRAN" "ans_cmd/Hlp_C_BFTRAN.html") ("BFUNIF" "ans_cmd/Hlp_C_BFUNIF.html") ("BFV" "ans_cmd/Hlp_C_BFV.html") ("BFVDELE" "ans_cmd/Hlp_C_BFVDELE.html") ("BFVLIST" "ans_cmd/Hlp_C_BFVLIST.html") ("BIOOPT" "ans_cmd/Hlp_C_BIOOPT.html") ("BIOT" "ans_cmd/Hlp_C_BIOT.html") ("BLC4" "ans_cmd/Hlp_C_BLC4.html") ("BLC5" "ans_cmd/Hlp_C_BLC5.html") ("BLOCK" "ans_cmd/Hlp_C_BLOCK.html") ("BOOL" "ans_cmd/Hlp_C_BOOL.html") ("BOPTN" "ans_cmd/Hlp_C_BOPTN.html") ("BRANCH" "ans_arch/Hlp_C_BRANCH.html") ("BSAX" "ans_cmd/Hlp_C_BSAX.html") ("BSM1" "ans_cmd/Hlp_C_BSM1.html") ("BSM2" "ans_cmd/Hlp_C_BSM2.html") ("BSMD" "ans_cmd/Hlp_C_BSMD.html") ("BSPLIN" "ans_cmd/Hlp_C_BSPLIN.html") ("BSS1" "ans_cmd/Hlp_C_BSS1.html") ("BSS2" "ans_cmd/Hlp_C_BSS2.html") ("BSTE" "ans_cmd/Hlp_C_BSTE.html") ("BSTQ" "ans_cmd/Hlp_C_BSTQ.html") ("BTOL" "ans_cmd/Hlp_C_BTOL.html") ("BUCOPT" "ans_cmd/Hlp_C_BUCOPT.html") ("C***" "ans_cmd/Hlp_C_C.html") ("CABLE" "ans_elem/Hlp_E_CH3_2.html#allcables") ("CABLE280" "ans_elem/Hlp_E_CABLE280.html") ("CALC" "ans_cmd/Hlp_C_CALC.html") ("CAMPBELL" "ans_cmd/Hlp_C_CAMPBELL.html") ("CAT5" "ans_cmd/Hlp_C_CAT5IN.html") ("CBDOF" "ans_cmd/Hlp_C_CBDOF.html") ("CBMD" "ans_cmd/Hlp_C_CBMD.html") ("CBMX" "ans_cmd/Hlp_C_CBMX.html") ("CBTE" "ans_cmd/Hlp_C_CBTE.html") ("CBTMP" "ans_cmd/Hlp_C_CBTMP.html") ("CDOPT" "ans_cmd/Hlp_C_CDOPT.html") ("CDREAD" "ans_cmd/Hlp_C_CDREAD.html") ("CDWRITE" "ans_cmd/Hlp_C_CDWRITE.html") ("CE" "ans_cmd/Hlp_C_CE.html") ("CECHECK" "ans_cmd/Hlp_C_CECHECK.html") ("CECMOD" "ans_cmd/Hlp_C_CECMOD.html") ("CECYC" "ans_cmd/Hlp_C_CECYC.html") ("CEDELE" "ans_cmd/Hlp_C_CEDELE.html") ("CEINTF" "ans_cmd/Hlp_C_CEINTF.html") ("CELIST" "ans_cmd/Hlp_C_CELIST.html") ("CENTER" "ans_cmd/Hlp_C_CENTER.html") ("CEQN" "ans_cmd/Hlp_C_CEQN.html") ("CERIG" "ans_cmd/Hlp_C_CERIG.html") ("CESGEN" "ans_cmd/Hlp_C_CESGEN.html") ("CFACT" "ans_cmd/Hlp_C_CFACT.html") ("CGLOC" "ans_cmd/Hlp_C_CGLOC.html") ("CGOMGA" "ans_cmd/Hlp_C_CGOMGA.html") ("CGROW" "ans_cmd/Hlp_C_CGROW.html") ("CH1TOC" "ans_cmd/Hlp_C_CH1.html") ("CH2TOC" "ans_cmd/aV39Sq344ctg.html") ("CH3TOC" "ans_cmd/Hlp_C_CH3.html") ("CHECK" "ans_cmd/Hlp_C_CHECK.html") ("CHKMSH" "ans_cmd/Hlp_C_CHKMSH.html") ("CINT" "ans_cmd/Hlp_C_CINT.html") ("CIRCLE" "ans_cmd/Hlp_C_CIRCLE.html") ("CIRCU" "ans_elem/Hlp_E_CH3_2.html#allcircu") ("CIRCU124" "ans_elem/Hlp_E_CIRCU124.html") ("CIRCU125" "ans_elem/Hlp_E_CIRCU125.html") ("CIRCU94" "ans_elem/Hlp_E_CIRCU94.html") ("CISOL" "ans_cmd/Hlp_C_CISOL.html") ("CLOCAL" "ans_cmd/Hlp_C_CLOCAL.html") ("CLOG" "ans_cmd/Hlp_C_CLOG.html") ("CLRMSHLN" "ans_cmd/Hlp_C_CLRMSHLN.html") ("CM" "ans_cmd/Hlp_C_CM.html") ("CMACEL" "ans_cmd/Hlp_C_CMACEL.html") ("CMATRIX" "ans_cmd/Hlp_C_CMATRIX.html") ("CMDELE" "ans_cmd/Hlp_C_CMDELE.html") ("CMDOMEGA" "ans_cmd/Hlp_C_CMDOMEGA.html") ("CMEDIT" "ans_cmd/Hlp_C_CMEDIT.html") ("CMGRP" "ans_cmd/Hlp_C_CMGRP.html") ("CMLIST" "ans_cmd/Hlp_C_CMLIST.html") ("CMMOD" "ans_cmd/Hlp_C_CMMOD.html") ("CMOMEGA" "ans_cmd/Hlp_C_CMOMEGA.html") ("CMPLOT" "ans_cmd/Hlp_C_CMPLOT.html") ("CMROTATE" "ans_cmd/Hlp_C_CMROTATE.html") ("CMSEL" "ans_cmd/Hlp_C_CMSEL.html") ("CMSFILE" "ans_cmd/Hlp_C_CMSFILE.html") ("CMSOPT" "ans_cmd/Hlp_C_CMSOPT.html") ("CMWRITE" "ans_cmd/Hlp_C_CMWRITE.html") ("CNCHECK" "ans_cmd/Hlp_C_CNCHECK.html") ("CNKMOD" "ans_cmd/Hlp_C_CNKMOD.html") ("CNTR" "ans_cmd/Hlp_C_CNTR.html") ("CNVTOL" "ans_cmd/Hlp_C_CNVTOL.html") ("COMBI" "ans_elem/Hlp_E_CH3_2.html#allcombin") ("COMBI214" "ans_elem/Hlp_E_COMBI214.html") ("COMBI250" "ans_elem/Hlp_E_COMBI250.html") ("COMBIN" "ans_elem/Hlp_E_CH3_2.html#allcombin") ("COMBIN14" "ans_elem/Hlp_E_COMBIN14.html") ("COMBIN37" "ans_elem/Hlp_E_COMBIN37.html") ("COMBIN39" "ans_elem/Hlp_E_COMBIN39.html") ("COMBIN40" "ans_elem/Hlp_E_COMBIN40.html") ("COMBINE" "ans_cmd/Hlp_C_COMBINE.html") ("COMPRESS" "ans_cmd/Hlp_C_COMPRESS.html") ("CON4" "ans_cmd/Hlp_C_CON4.html") ("CONE" "ans_cmd/Hlp_C_CONE.html") ("CONJUG" "ans_cmd/Hlp_C_CONJUG.html") ("CONTA" "ans_elem/Hlp_E_CH3_2.html#allcontac") ("CONTA171" "ans_arch/Hlp_E_CONTA171.html") ("CONTA172" "ans_elem/Hlp_E_CONTA172.html") ("CONTA173" "ans_arch/Hlp_E_CONTA173.html") ("CONTA174" "ans_elem/Hlp_E_CONTA174.html") ("CONTA175" "ans_elem/Hlp_E_CONTA175.html") ("CONTA176" "ans_arch/Hlp_E_CONTA176.html") ("CONTA177" "ans_elem/Hlp_E_CONTA177.html") ("CONTA178" "ans_elem/Hlp_E_CONTA178.html") ("CONTAC" "ans_elem/Hlp_E_CH3_2.html#allcontac") ("CONTAC12" "ans_arch/Hlp_E_CONTAC12.html") ("CONTAC52" "ans_arch/Hlp_E_CONTAC52.html") ("CORIOLIS" "ans_cmd/Hlp_C_CORIOLIS.html") ("COUPLE" "ans_cmd/Hlp_C_COUPLE.html") ("COVAL" "ans_cmd/Hlp_C_COVAL.html") ("CP" "ans_cmd/Hlp_C_CP.html") ("CPCYC" "ans_cmd/Hlp_C_CPCYC.html") ("CPDELE" "ans_cmd/Hlp_C_CPDELE.html") ("CPINTF" "ans_cmd/Hlp_C_CPINTF.html") ("CPLGEN" "ans_cmd/Hlp_C_CPLGEN.html") ("CPLIST" "ans_cmd/Hlp_C_CPLIST.html") ("CPMERGE" "ans_cmd/Hlp_C_CPMERGE.html") ("CPNGEN" "ans_cmd/Hlp_C_CPNGEN.html") ("CPSGEN" "ans_cmd/Hlp_C_CPSGEN.html") ("CPT" "ans_elem/Hlp_E_CH3_2.html#allcpt") ("CPT212" "ans_elem/Hlp_E_CPT212.html") ("CPT213" "ans_elem/Hlp_E_CPT213.html") ("CPT215" "ans_elem/Hlp_E_CPT215.html") ("CPT216" "ans_elem/Hlp_E_CPT216.html") ("CPT217" "ans_elem/Hlp_E_CPT217.html") ("CQC" "ans_cmd/Hlp_C_CQC.html") ("CRPLIM" "ans_arch/Hlp_C_CRPLIM.html") ("CS" "ans_cmd/Hlp_C_CS.html") ("CSCIR" "ans_cmd/Hlp_C_CSCIR.html") ("CSDELE" "ans_cmd/Hlp_C_CSDELE.html") ("CSKP" "ans_cmd/Hlp_C_CSKP.html") ("CSLIST" "ans_cmd/Hlp_C_CSLIST.html") ("CSWPLA" "ans_cmd/Hlp_C_CSWPLA.html") ("CSYS" "ans_cmd/Hlp_C_CSYS.html") ("CURR2D" "ans_cmd/Hlp_C_CURR2D.html") ("CUTCONTROL" "ans_cmd/Hlp_C_CUTCONTROL.html") ("CVAR" "ans_cmd/Hlp_C_CVAR.html") ("CYCCALC" "ans_cmd/Hlp_C_CYCCALC.html") ("CYCFILES" "ans_cmd/Hlp_C_CYCFILES.html") ("CYCFREQ" "ans_cmd/Hlp_C_CYCFREQ.html") ("CYCLIC" "ans_cmd/Hlp_C_CYCLIC.html") ("CYCOPT" "ans_cmd/Hlp_C_CYCOPT.html") ("CYCPHASE" "ans_cmd/Hlp_C_CYCPHASE.html") ("CYCSPEC" "ans_cmd/Hlp_C_CYCSPEC.html") ("CYL4" "ans_cmd/Hlp_C_CYL4.html") ("CYL5" "ans_cmd/Hlp_C_CYL5.html") ("CYLIND" "ans_cmd/Hlp_C_CYLIND.html") ("CZDEL" "ans_cmd/Hlp_C_CZDEL.html") ("CZMESH" "ans_cmd/Hlp_C_CZMESH.html") ("Commands" "ans_cmd/Hlp_C_CmdTOC.html") ("D" "ans_cmd/Hlp_C_D.html") ("DA" "ans_cmd/Hlp_C_DA.html") ("DADELE" "ans_cmd/Hlp_C_DADELE.html") ("DALIST" "ans_cmd/Hlp_C_DALIST.html") ("DAMORPH" "ans_cmd/Hlp_C_DAMORPH.html") ("DATA" "ans_cmd/Hlp_C_DATA.html") ("DATADEF" "ans_cmd/Hlp_C_DATADEF.html") ("DCGOMG" "ans_cmd/Hlp_C_DCGOMG.html") ("DCUM" "ans_cmd/Hlp_C_DCUM.html") ("DCVSWP" "ans_cmd/Hlp_C_DCVSWP.html") ("DDASPEC" "ans_cmd/Hlp_C_DDASPEC.html") ("DDELE" "ans_cmd/Hlp_C_DDELE.html") ("DDOPTION" "ans_cmd/Hlp_C_DDOPTION.html") ("DEACT" "ans_cmd/Hlp_C_DEACT.html") ("DEFINE" "ans_cmd/Hlp_C_DEFINE.html") ("DELETE" "ans_cmd/Hlp_C_DELETE.html") ("DELTIM" "ans_cmd/Hlp_C_DELTIM.html") ("DEMORPH" "ans_cmd/Hlp_C_DEMORPH.html") ("DERIV" "ans_cmd/Hlp_C_DERIV.html") ("DESIZE" "ans_cmd/Hlp_C_DESIZE.html") ("DESOL" "ans_cmd/Hlp_C_DESOL.html") ("DETAB" "ans_cmd/Hlp_C_DETAB.html") ("DFLX" "ans_cmd/Hlp_C_DFLX.html") ("DFSWAVE" "ans_cmd/Hlp_C_DFSWAVE.html") ("DIG" "ans_cmd/Hlp_C_DIG.html") ("DIGIT" "ans_cmd/Hlp_C_DIGIT.html") ("DISPLAY" "ans_cmd/Hlp_C_DISPLAY.html") ("DJ" "ans_cmd/Hlp_C_DJ.html") ("DJDELE" "ans_cmd/Hlp_C_DJDELE.html") ("DJLIST" "ans_cmd/Hlp_C_DJLIST.html") ("DK" "ans_cmd/Hlp_C_DK.html") ("DKDELE" "ans_cmd/Hlp_C_DKDELE.html") ("DKLIST" "ans_cmd/Hlp_C_DKLIST.html") ("DL" "ans_cmd/Hlp_C_DL.html") ("DLDELE" "ans_cmd/Hlp_C_DLDELE.html") ("DLIST" "ans_cmd/Hlp_C_DLIST.html") ("DLLIST" "ans_cmd/Hlp_C_DLLIST.html") ("DMOVE" "ans_cmd/Hlp_C_DMOVE.html") ("DMPEXT" "ans_cmd/Hlp_C_DMPEXT.html") ("DMPOPTION" "ans_cmd/Hlp_C_DMPOPTION.html") ("DMPRAT" "ans_cmd/Hlp_C_DMPRAT.html") ("DMPSTR" "ans_cmd/Hlp_C_DMPSTR.html") ("DNSOL" "ans_cmd/Hlp_C_DNSOL.html") ("DOF" "ans_cmd/Hlp_C_DOF.html") ("DOFSEL" "ans_cmd/Hlp_C_DOFSEL.html") ("DOMEGA" "ans_cmd/Hlp_C_DOMEGA.html") ("DSCALE" "ans_cmd/Hlp_C_DSCALE.html") ("DSET" "ans_cmd/Hlp_C_DSET.html") ("DSPOPTION" "ans_cmd/Hlp_C_DSPOPTION.html") ("DSUM" "ans_cmd/Hlp_C_DSUM.html") ("DSURF" "ans_cmd/Hlp_C_DSURF.html") ("DSYM" "ans_cmd/Hlp_C_DSYM.html") ("DSYS" "ans_cmd/Hlp_C_DSYS.html") ("DTRAN" "ans_cmd/Hlp_C_DTRAN.html") ("DUMP" "ans_cmd/Hlp_C_DUMP.html") ("DVAL" "ans_cmd/Hlp_C_DVAL.html") ("DVMORPH" "ans_cmd/Hlp_C_DVMORPH.html") ("DYNOPT" "ans_cmd/Hlp_C_DYNOPT.html") ("E" "ans_cmd/Hlp_C_E.html") ("EALIVE" "ans_cmd/Hlp_C_EALIVE.html") ("ECPCHG" "ans_cmd/Hlp_C_ECPCHG.html") ("EDELE" "ans_cmd/Hlp_C_EDELE.html") ("EEXTRUDE" "ans_cmd/Hlp_C_EEXTRUDE.html") ("EGEN" "ans_cmd/Hlp_C_EGEN.html") ("EGID" "ans_cmd/Hlp_C_EGID.html") ("EINFIN" "ans_cmd/Hlp_C_EINFIN.html") ("EINTF" "ans_cmd/Hlp_C_EINTF.html") ("EKILL" "ans_cmd/Hlp_C_EKILL.html") ("ELBOW" "ans_cmd/Hlp_C_ELBOW.html") ("ELBOW290" "ans_elem/Hlp_E_ELBOW290.html") ("ELEM" "ans_cmd/Hlp_C_ELEM.html") ("ELIST" "ans_cmd/Hlp_C_ELIST.html") ("EMAGERR" "ans_cmd/Hlp_C_EMAGERR.html") ("EMATWRITE" "ans_cmd/Hlp_C_EMATWRITE.html") ("EMF" "ans_cmd/Hlp_C_EMF.html") ("EMFT" "ans_cmd/Hlp_C_EMFT.html") ("EMID" "ans_cmd/Hlp_C_EMID.html") ("EMIS" "ans_cmd/Hlp_C_EMIS.html") ("EMODIF" "ans_cmd/Hlp_C_EMODIF.html") ("EMORE" "ans_cmd/Hlp_C_EMORE.html") ("EMSEL" "ans_cmd/Hlp_C_EMSEL.html") ("EMSYM" "ans_cmd/Hlp_C_EMSYM.html") ("EMTGEN" "ans_cmd/Hlp_C_EMTGEN.html") ("EMUNIT" "ans_cmd/Hlp_C_EMUNIT.html") ("EN" "ans_cmd/Hlp_C_EN.html") ("ENDRELEASE" "ans_cmd/Hlp_C_ENDRELEASE.html") ("ENERSOL" "ans_cmd/Hlp_C_ENERSOL.html") ("ENGEN" "ans_cmd/Hlp_C_ENGEN.html") ("ENORM" "ans_cmd/Hlp_C_ENORM.html") ("ENSYM" "ans_cmd/Hlp_C_ENSYM.html") ("EORIENT" "ans_cmd/Hlp_C_EORIENT.html") ("EPLOT" "ans_cmd/Hlp_C_EPLOT.html") ("EQSLV" "ans_cmd/Hlp_C_EQSLV.html") ("ERASE" "ans_cmd/Hlp_C_ERASE.html") ("EREAD" "ans_cmd/Hlp_C_EREAD.html") ("EREFINE" "ans_cmd/Hlp_C_EREFINE.html") ("EREINF" "ans_cmd/Hlp_C_EREINF.html") ("ERESX" "ans_cmd/Hlp_C_ERESX.html") ("ERNORM" "ans_cmd/Hlp_C_ERNORM.html") ("ERRANG" "ans_cmd/Hlp_C_ERRANG.html") ("ESCHECK" "ans_cmd/Hlp_C_ESCHECK.html") ("ESEL" "ans_cmd/Hlp_C_ESEL.html") ("ESIZE" "ans_cmd/Hlp_C_ESIZE.html") ("ESLA" "ans_cmd/Hlp_C_ESLA.html") ("ESLL" "ans_cmd/Hlp_C_ESLL.html") ("ESLN" "ans_cmd/Hlp_C_ESLN.html") ("ESLV" "ans_cmd/Hlp_C_ESLV.html") ("ESOL" "ans_cmd/Hlp_C_ESOL.html") ("ESORT" "ans_cmd/Hlp_C_ESORT.html") ("ESSOLV" "ans_cmd/Hlp_C_ESSOLV.html") ("ESTIF" "ans_cmd/Hlp_C_ESTIF.html") ("ESURF" "ans_cmd/Hlp_C_ESURF.html") ("ESYM" "ans_cmd/Hlp_C_ESYM.html") ("ESYS" "ans_cmd/Hlp_C_ESYS.html") ("ET" "ans_cmd/Hlp_C_ET.html") ("ETABLE" "ans_cmd/Hlp_C_ETABLE.html") ("ETCHG" "ans_cmd/Hlp_C_ETCHG.html") ("ETCONTROL" "ans_cmd/Hlp_C_ETCONTROL.html") ("ETDELE" "ans_cmd/Hlp_C_ETDELE.html") ("ETLIST" "ans_cmd/Hlp_C_ETLIST.html") ("ETYPE" "ans_cmd/Hlp_C_ETYPE.html") ("EUSORT" "ans_cmd/Hlp_C_EUSORT.html") ("EWRITE" "ans_cmd/Hlp_C_EWRITE.html") ("EXBOPT" "ans_cmd/Hlp_C_EXBOPT.html") ("EXOPTION" "ans_cmd/Hlp_C_EXOPTION.html") ("EXP" "ans_cmd/Hlp_C_EXP.html") ("EXPAND" "ans_cmd/Hlp_C_EXPAND.html") ("EXPASS" "ans_cmd/Hlp_C_EXPASS.html") ("EXPROFILE" "ans_cmd/Hlp_C_EXPROFILE.html") ("EXPSOL" "ans_cmd/Hlp_C_EXPSOL.html") ("EXTOPT" "ans_cmd/Hlp_C_EXTOPT.html") ("EXTREM" "ans_cmd/Hlp_C_EXTREM.html") ("EXUNIT" "ans_cmd/Hlp_C_EXUNIT.html") ("F" "ans_cmd/Hlp_C_F.html") ("FATIGUE" "ans_arch/Hlp_C_FATIGUE.html") ("FC" "ans_cmd/Hlp_C_FC.html") ("FCCHECK" "ans_cmd/Hlp_C_FCCHECK.html") ("FCDELE" "ans_cmd/Hlp_C_FCDELE.html") ("FCLIST" "ans_cmd/Hlp_C_FCLIST.html") ("FCTYP" "ans_cmd/Hlp_C_FCTYP.html") ("FCUM" "ans_cmd/Hlp_C_FCUM.html") ("FDELE" "ans_cmd/Hlp_C_FDELE.html") ("FE" "ans_arch/Hlp_C_FE.html") ("FEBODY" "ans_cmd/Hlp_C_FEBODY.html") ("FECONS" "ans_cmd/Hlp_C_FECONS.html") ("FEFOR" "ans_cmd/Hlp_C_FEFOR.html") ("FELIST" "ans_arch/Hlp_C_FELIST.html") ("FESURF" "ans_cmd/Hlp_C_FESURF.html") ("FILE" "ans_cmd/Hlp_C_FILE.html") ("FILEAUX2" "ans_cmd/Hlp_C_FILEAUX2.html") ("FILEAUX3" "ans_cmd/Hlp_C_FILEAUX3.html") ("FILL" "ans_cmd/Hlp_C_FILL.html") ("FILLDATA" "ans_cmd/Hlp_C_FILLDATA.html") ("FINISH" "ans_cmd/Hlp_C_FINISH.html") ("FITEM" "ans_cmd/Hlp_C_FITEM.html") ("FJ" "ans_cmd/Hlp_C_FJ.html") ("FJDELE" "ans_cmd/Hlp_C_FJDELE.html") ("FJLIST" "ans_cmd/Hlp_C_FJLIST.html") ("FK" "ans_cmd/Hlp_C_FK.html") ("FKDELE" "ans_cmd/Hlp_C_FKDELE.html") ("FKLIST" "ans_cmd/Hlp_C_FKLIST.html") ("FL" "ans_arch/Hlp_C_FL.html") ("FLANGE" "ans_arch/Hlp_C_FLANGE.html") ("FLIST" "ans_cmd/Hlp_C_FLIST.html") ("FLLIST" "ans_arch/Hlp_C_FLLIST.html") ("FLST" "ans_cmd/Hlp_C_FLST.html") ("FLUID" "ans_elem/Hlp_E_CH3_2.html#allfluid") ("FLUID116" "ans_elem/Hlp_E_FLUID116.html") ("FLUID129" "ans_elem/Hlp_E_FLUID129.html") ("FLUID130" "ans_elem/Hlp_E_FLUID130.html") ("FLUID136" "ans_elem/Hlp_E_FLUID136.html") ("FLUID138" "ans_elem/Hlp_E_FLUID138.html") ("FLUID139" "ans_elem/Hlp_E_FLUID139.html") ("FLUID218" "ans_elem/Hlp_E_FLUID218.html") ("FLUID220" "ans_elem/Hlp_E_FLUID220.html") ("FLUID221" "ans_elem/Hlp_E_FLUID221.html") ("FLUID29" "ans_elem/Hlp_E_FLUID29.html") ("FLUID30" "ans_elem/Hlp_E_FLUID30.html") ("FLUID38" "ans_elem/Hlp_E_FLUID38.html") ("FLUID79" "ans_arch/Hlp_E_FLUID79.html") ("FLUID80" "ans_arch/Hlp_E_FLUID80.html") ("FLUID81" "ans_arch/Hlp_E_FLUID81.html") ("FLUREAD" "ans_cmd/Hlp_C_FLUREAD.html") ("FLUXV" "ans_cmd/Hlp_C_FLUXV.html") ("FOLLW" "ans_elem/Hlp_E_CH3_2.html#allfollw") ("FOLLW201" "ans_elem/Hlp_E_FOLLW201.html") ("FORCE" "ans_cmd/Hlp_C_FORCE.html") ("FORM" "ans_cmd/Hlp_C_FORM.html") ("FP" "ans_arch/Hlp_C_FP.html") ("FPLIST" "ans_arch/Hlp_C_FPLIST.html") ("FREQ" "ans_cmd/Hlp_C_FREQ.html") ("FRQSCL" "ans_cmd/Hlp_C_FRQSCL.html") ("FS" "ans_arch/Hlp_C_FS.html") ("FSCALE" "ans_cmd/Hlp_C_FSCALE.html") ("FSDELE" "ans_arch/Hlp_C_FSDELE.html") ("FSLIST" "ans_arch/Hlp_C_FSLIST.html") ("FSNODE" "ans_arch/Hlp_C_FSNODE.html") ("FSPLOT" "ans_arch/Hlp_C_FSPLOT.html") ("FSSECT" "ans_cmd/Hlp_C_FSSECT.html") ("FSSPARM" "ans_cmd/Hlp_C_FSSPARM.html") ("FSUM" "ans_cmd/Hlp_C_FSUM.html") ("FTCALC" "ans_arch/Hlp_C_FTCALC.html") ("FTRAN" "ans_cmd/Hlp_C_FTRAN.html") ("FTSIZE" "ans_arch/Hlp_C_FTSIZE.html") ("FTWRITE" "ans_arch/Hlp_C_FTWRITE.html") ("FTYPE" "ans_cmd/Hlp_C_FTYPE.html") ("FVMESH" "ans_cmd/Hlp_C_FVMESH.html") ("GAP" "ans_cmd/Hlp_C_GAP.html") ("GAPF" "ans_cmd/Hlp_C_GAPF.html") ("GAUGE" "ans_cmd/Hlp_C_GAUGE.html") ("GCDEF" "ans_cmd/Hlp_C_GCDEF.html") ("GCGEN" "ans_cmd/Hlp_C_GCGEN.html") ("GENOPT" "ans_cmd/Hlp_C_GENOPT.html") ("GEOM" "ans_cmd/Hlp_C_GEOM.html") ("GEOMETRY" "ans_cmd/Hlp_C_GEOMETRY.html") ("GMATRIX" "ans_cmd/Hlp_C_GMATRIX.html") ("GMFACE" "ans_cmd/Hlp_C_GMFACE.html") ("GP" "ans_cmd/Hlp_C_GP.html") ("GPDELE" "ans_cmd/Hlp_C_GPDELE.html") ("GPLIST" "ans_cmd/Hlp_C_GPLIST.html") ("GPLOT" "ans_cmd/Hlp_C_GPLOT.html") ("GRP" "ans_cmd/Hlp_C_GRP.html") ("GSBDATA" "ans_cmd/Hlp_C_GSBDATA.html") ("GSGDATA" "ans_cmd/Hlp_C_GSGDATA.html") ("GSLIST" "ans_cmd/Hlp_C_GSLIST.html") ("GSSOL" "ans_cmd/Hlp_C_GSSOL.html") ("GSUM" "ans_cmd/Hlp_C_GSUM.html") ("HARFRQ" "ans_cmd/Hlp_C_HARFRQ.html") ("HBMAT" "ans_cmd/Hlp_C_HBMAT.html") ("HELP" "ans_cmd/Hlp_C_HELP.html") ("HEMIOPT" "ans_cmd/Hlp_C_HEMIOPT.html") ("HFANG" "ans_cmd/Hlp_C_HFANG.html") ("HFSYM" "ans_cmd/Hlp_C_HFSYM.html") ("HPTCREATE" "ans_cmd/Hlp_C_HPTCREATE.html") ("HPTDELETE" "ans_cmd/Hlp_C_HPTDELETE.html") ("HRCPLX" "ans_cmd/Hlp_C_HRCPLX.html") ("HREXP" "ans_cmd/Hlp_C_HREXP.html") ("HROCEAN" "ans_cmd/Hlp_C_HROCEAN.html") ("HROPT" "ans_cmd/Hlp_C_HROPT.html") ("HROUT" "ans_cmd/Hlp_C_HROUT.html") ("HSFLD" "ans_elem/Hlp_E_CH3_2.html#allhsfld") ("HSFLD241" "ans_elem/Hlp_E_HSFLD241.html") ("HSFLD242" "ans_elem/Hlp_E_HSFLD242.html") ("IC" "ans_cmd/Hlp_C_IC.html") ("ICDELE" "ans_cmd/Hlp_C_ICDELE.html") ("ICLIST" "ans_cmd/Hlp_C_ICLIST.html") ("ICROTATE" "ans_cmd/Hlp_C_ICROTATE.html") ("IGESIN" "ans_cmd/Hlp_C_IGESIN.html") ("IGESOUT" "ans_cmd/Hlp_C_IGESOUT.html") ("IMAGIN" "ans_cmd/Hlp_C_IMAGIN.html") ("IMESH" "ans_cmd/Hlp_C_IMESH.html") ("IMMED" "ans_cmd/Hlp_C_IMMED.html") ("INFIN" "ans_elem/Hlp_E_CH3_2.html#allinfin") ("INFIN110" "ans_elem/Hlp_E_INFIN110.html") ("INFIN111" "ans_elem/Hlp_E_INFIN111.html") ("INFIN257" "ans_elem/Hlp_E_INFIN257.html") ("INFIN47" "ans_elem/Hlp_E_INFIN47.html") ("INISTATE" "ans_cmd/Hlp_C_INISTATE.html") ("INRES" "ans_cmd/Hlp_C_INRES.html") ("INRTIA" "ans_cmd/Hlp_C_INRTIA.html") ("INT1" "ans_cmd/Hlp_C_INT1.html") ("INTER" "ans_elem/Hlp_E_CH3_2.html#allinter") ("INTER192" "ans_elem/Hlp_E_INTER192.html") ("INTER193" "ans_elem/Hlp_E_INTER193.html") ("INTER194" "ans_elem/Hlp_E_INTER194.html") ("INTER195" "ans_elem/Hlp_E_INTER195.html") ("INTER202" "ans_elem/Hlp_E_INTER202.html") ("INTER203" "ans_elem/Hlp_E_INTER203.html") ("INTER204" "ans_elem/Hlp_E_INTER204.html") ("INTER205" "ans_elem/Hlp_E_INTER205.html") ("INVOPT" "ans_cmd/Hlp_C_INVOPT.html") ("IOPTN" "ans_cmd/Hlp_C_IOPTN.html") ("IRLF" "ans_cmd/Hlp_C_IRLF.html") ("IRLIST" "ans_cmd/Hlp_C_IRLIST.html") ("Index" "ai_sinfo/ans_intro.html") ("JPEG" "ans_cmd/Hlp_C_JPEG.html") ("JSOL" "ans_cmd/Hlp_C_JSOL.html") ("K" "ans_cmd/Hlp_C_K.html") ("KATT" "ans_cmd/Hlp_C_KATT.html") ("KBC" "ans_cmd/Hlp_C_KBC.html") ("KBETW" "ans_cmd/Hlp_C_KBETW.html") ("KCALC" "ans_arch/Hlp_C_KCALC.html") ("KCENTER" "ans_cmd/Hlp_C_KCENTER.html") ("KCLEAR" "ans_cmd/Hlp_C_KCLEAR.html") ("KDELE" "ans_cmd/Hlp_C_KDELE.html") ("KDIST" "ans_cmd/Hlp_C_KDIST.html") ("KEEP" "ans_cmd/Hlp_C_KEEP.html") ("KESIZE" "ans_cmd/Hlp_C_KESIZE.html") ("KEYOPT" "ans_cmd/Hlp_C_KEYOPT.html") ("KEYPTS" "ans_cmd/Hlp_C_KEYPTS.html") ("KEYW" "ans_cmd/Hlp_C_KEYW.html") ("KFILL" "ans_cmd/Hlp_C_KFILL.html") ("KGEN" "ans_cmd/Hlp_C_KGEN.html") ("KL" "ans_cmd/Hlp_C_KL.html") ("KLIST" "ans_cmd/Hlp_C_KLIST.html") ("KMESH" "ans_cmd/Hlp_C_KMESH.html") ("KMODIF" "ans_cmd/Hlp_C_KMODIF.html") ("KMOVE" "ans_cmd/Hlp_C_KMOVE.html") ("KNODE" "ans_cmd/Hlp_C_KNODE.html") ("KPLOT" "ans_cmd/Hlp_C_KPLOT.html") ("KPSCALE" "ans_cmd/Hlp_C_KPSCALE.html") ("KREFINE" "ans_cmd/Hlp_C_KREFINE.html") ("KSCALE" "ans_cmd/Hlp_C_KSCALE.html") ("KSCON" "ans_cmd/Hlp_C_KSCON.html") ("KSEL" "ans_cmd/Hlp_C_KSEL.html") ("KSLL" "ans_cmd/Hlp_C_KSLL.html") ("KSLN" "ans_cmd/Hlp_C_KSLN.html") ("KSUM" "ans_cmd/Hlp_C_KSUM.html") ("KSYMM" "ans_cmd/Hlp_C_KSYMM.html") ("KTRAN" "ans_cmd/Hlp_C_KTRAN.html") ("KUSE" "ans_cmd/Hlp_C_KUSE.html") ("KWPAVE" "ans_cmd/Hlp_C_KWPAVE.html") ("KWPLAN" "ans_cmd/Hlp_C_KWPLAN.html") ("L" "ans_cmd/Hlp_C_L.html") ("L2ANG" "ans_cmd/Hlp_C_L2ANG.html") ("L2TAN" "ans_cmd/Hlp_C_L2TAN.html") ("LANBOPTION" "ans_cmd/Hlp_C_LANBOPTION.html") ("LANG" "ans_cmd/Hlp_C_LANG.html") ("LARC" "ans_cmd/Hlp_C_LARC.html") ("LAREA" "ans_cmd/Hlp_C_LAREA.html") ("LARGE" "ans_cmd/Hlp_C_LARGE.html") ("LATT" "ans_cmd/Hlp_C_LATT.html") ("LAYER" "ans_cmd/Hlp_C_LAYER.html") ("LAYERP26" "ans_cmd/Hlp_C_LAYERP26.html") ("LAYLIST" "ans_cmd/Hlp_C_LAYLIST.html") ("LAYPLOT" "ans_cmd/Hlp_C_LAYPLOT.html") ("LCABS" "ans_cmd/Hlp_C_LCABS.html") ("LCASE" "ans_cmd/Hlp_C_LCASE.html") ("LCCALC" "ans_cmd/Hlp_C_LCCALC.html") ("LCCAT" "ans_cmd/Hlp_C_LCCAT.html") ("LCDEF" "ans_cmd/Hlp_C_LCDEF.html") ("LCFACT" "ans_cmd/Hlp_C_LCFACT.html") ("LCFILE" "ans_cmd/Hlp_C_LCFILE.html") ("LCLEAR" "ans_cmd/Hlp_C_LCLEAR.html") ("LCOMB" "ans_cmd/Hlp_C_LCOMB.html") ("LCOPER" "ans_cmd/Hlp_C_LCOPER.html") ("LCSEL" "ans_cmd/Hlp_C_LCSEL.html") ("LCSL" "ans_cmd/Hlp_C_LCSL.html") ("LCSUM" "ans_cmd/Hlp_C_LCSUM.html") ("LCWRITE" "ans_cmd/Hlp_C_LCWRITE.html") ("LCZERO" "ans_cmd/Hlp_C_LCZERO.html") ("LDELE" "ans_cmd/Hlp_C_LDELE.html") ("LDIV" "ans_cmd/Hlp_C_LDIV.html") ("LDRAG" "ans_cmd/Hlp_C_LDRAG.html") ("LDREAD" "ans_cmd/Hlp_C_LDREAD.html") ("LESIZE" "ans_cmd/Hlp_C_LESIZE.html") ("LEXTND" "ans_cmd/Hlp_C_LEXTND.html") ("LFILLT" "ans_cmd/Hlp_C_LFILLT.html") ("LFSURF" "ans_cmd/Hlp_C_LFSURF.html") ("LGEN" "ans_cmd/Hlp_C_LGEN.html") ("LGLUE" "ans_cmd/Hlp_C_LGLUE.html") ("LGWRITE" "ans_cmd/Hlp_C_LGWRITE.html") ("LINA" "ans_cmd/Hlp_C_LINA.html") ("LINE" "ans_cmd/Hlp_C_LINE.html") ("LINES" "ans_cmd/Hlp_C_LINES.html") ("LINK" "ans_elem/Hlp_E_CH3_2.html#alllinks") ("LINK11" "ans_elem/Hlp_E_LINK11.html") ("LINK180" "ans_elem/Hlp_E_LINK180.html") ("LINK31" "ans_elem/Hlp_E_LINK31.html") ("LINK33" "ans_elem/Hlp_E_LINK33.html") ("LINK34" "ans_elem/Hlp_E_LINK34.html") ("LINK68" "ans_elem/Hlp_E_LINK68.html") ("LINL" "ans_cmd/Hlp_C_LINL.html") ("LINP" "ans_cmd/Hlp_C_LINP.html") ("LINV" "ans_cmd/Hlp_C_LINV.html") ("LIST" "ans_cmd/Hlp_C_LIST.html") ("LLIST" "ans_cmd/Hlp_C_LLIST.html") ("LMESH" "ans_cmd/Hlp_C_LMESH.html") ("LNSRCH" "ans_cmd/Hlp_C_LNSRCH.html") ("LOCAL" "ans_cmd/Hlp_C_LOCAL.html") ("LOVLAP" "ans_cmd/Hlp_C_LOVLAP.html") ("LPLOT" "ans_cmd/Hlp_C_LPLOT.html") ("LPTN" "ans_cmd/Hlp_C_LPTN.html") ("LREFINE" "ans_cmd/Hlp_C_LREFINE.html") ("LREVERSE" "ans_cmd/Hlp_C_LREVERSE.html") ("LROTAT" "ans_cmd/Hlp_C_LROTAT.html") ("LSBA" "ans_cmd/Hlp_C_LSBA.html") ("LSBL" "ans_cmd/Hlp_C_LSBL.html") ("LSBV" "ans_cmd/Hlp_C_LSBV.html") ("LSBW" "ans_cmd/Hlp_C_LSBW.html") ("LSCLEAR" "ans_cmd/Hlp_C_LSCLEAR.html") ("LSDELE" "ans_cmd/Hlp_C_LSDELE.html") ("LSEL" "ans_cmd/Hlp_C_LSEL.html") ("LSLA" "ans_cmd/Hlp_C_LSLA.html") ("LSLK" "ans_cmd/Hlp_C_LSLK.html") ("LSOPER" "ans_cmd/Hlp_C_LSOPER.html") ("LSREAD" "ans_cmd/Hlp_C_LSREAD.html") ("LSSCALE" "ans_cmd/Hlp_C_LSSCALE.html") ("LSSOLVE" "ans_cmd/Hlp_C_LSSOLVE.html") ("LSTR" "ans_cmd/Hlp_C_LSTR.html") ("LSUM" "ans_cmd/Hlp_C_LSUM.html") ("LSWRITE" "ans_cmd/Hlp_C_LSWRITE.html") ("LSYMM" "ans_cmd/Hlp_C_LSYMM.html") ("LTAN" "ans_cmd/Hlp_C_LTAN.html") ("LTRAN" "ans_cmd/Hlp_C_LTRAN.html") ("LUMPM" "ans_cmd/Hlp_C_LUMPM.html") ("LVSCALE" "ans_cmd/Hlp_C_LVSCALE.html") ("LWPLAN" "ans_cmd/Hlp_C_LWPLAN.html") ("M" "ans_cmd/Hlp_C_M.html") ("MACOPT" "ans_cmd/Hlp_C_MACOPT.html") ("MAGOPT" "ans_cmd/Hlp_C_MAGOPT.html") ("MAGSOLV" "ans_cmd/Hlp_C_MAGSOLV.html") ("MAP" "ans_cmd/Hlp_C_MAP.html") ("MAP2DTO3D" "ans_cmd/Hlp_C_MAP2DTO3D.html") ("MAPSOLVE" "ans_cmd/Hlp_C_MAPSOLVE.html") ("MAPVAR" "ans_cmd/Hlp_C_MAPVAR.html") ("MASCALE" "ans_cmd/Hlp_C_MASCALE.html") ("MASS" "ans_elem/Hlp_E_CH3_2.html#allmass") ("MASS21" "ans_elem/Hlp_E_MASS21.html") ("MASS71" "ans_elem/Hlp_E_MASS71.html") ("MASTER" "ans_cmd/Hlp_C_MASTER.html") ("MAT" "ans_cmd/Hlp_C_MAT.html") ("MATER" "ans_cmd/Hlp_C_MATER.html") ("MATRIX" "ans_elem/Hlp_E_CH3_2.html#allmatrix") ("MATRIX27" "ans_elem/Hlp_E_MATRIX27.html") ("MATRIX50" "ans_elem/Hlp_E_MATRIX50.html") ("MCHECK" "ans_cmd/Hlp_C_MCHECK.html") ("MDAMP" "ans_cmd/Hlp_C_MDAMP.html") ("MDELE" "ans_cmd/Hlp_C_MDELE.html") ("MDPLOT" "ans_cmd/Hlp_C_MDPLOT.html") ("MEMM" "ans_cmd/Hlp_C_MEMM.html") ("MESH" "ans_elem/Hlp_E_CH3_2.html#allmesh") ("MESH200" "ans_elem/Hlp_E_MESH200.html") ("MESHING" "ans_cmd/Hlp_C_MESHING.html") ("MGEN" "ans_cmd/Hlp_C_MGEN.html") ("MIDTOL" "ans_cmd/Hlp_C_MIDTOL.html") ("MITER" "ans_arch/Hlp_C_MITER.html") ("MLIST" "ans_cmd/Hlp_C_MLIST.html") ("MMASS" "ans_cmd/Hlp_C_MMASS.html") ("MMF" "ans_cmd/Hlp_C_MMF.html") ("MODCONT" "ans_cmd/Hlp_C_MODCONT.html") ("MODDIR" "ans_cmd/Hlp_C_MODDIR.html") ("MODE" "ans_cmd/Hlp_C_MODE.html") ("MODIFY" "ans_cmd/Hlp_C_MODIFY.html") ("MODMSH" "ans_cmd/Hlp_C_MODMSH.html") ("MODOPT" "ans_cmd/Hlp_C_MODOPT.html") ("MODSELOPTION" "ans_cmd/Hlp_C_MODSELOPTION.html") ("MONITOR" "ans_cmd/Hlp_C_MONITOR.html") ("MOPT" "ans_cmd/Hlp_C_MOPT.html") ("MORPH" "ans_cmd/Hlp_C_MORPH.html") ("MOVE" "ans_cmd/Hlp_C_MOVE.html") ("MP" "ans_cmd/Hlp_C_MP.html") ("MPAMOD" "ans_cmd/Hlp_C_MPAMOD.html") ("MPC184" "ans_elem/Hlp_E_MPC184.html") ("MPC184-Cylin" "ans_elem/Hlp_E_MPC184cyl.html") ("MPC184-General" "ans_elem/Hlp_E_MPC184gen.html") ("MPC184-Link/Beam" "ans_elem/Hlp_E_MPC184link.html") ("MPC184-Orient" "ans_elem/Hlp_E_MPC184orie.html") ("MPC184-Planar" "ans_elem/Hlp_E_MPC184plan.html") ("MPC184-Point" "ans_elem/Hlp_E_MPC184poin.html") ("MPC184-Revolute" "ans_elem/Hlp_E_MPC184revo.html") ("MPC184-Screw" "ans_elem/Hlp_E_MPC184scr.html") ("MPC184-Slider" "ans_elem/Hlp_E_MPC184slid.html") ("MPC184-Slot" "ans_elem/Hlp_E_MPC184slot.html") ("MPC184-Spherical" "ans_elem/Hlp_E_MPC184sphe.html") ("MPC184-Trans" "ans_elem/Hlp_E_MPC184tran.html") ("MPC184-Universal" "ans_elem/Hlp_E_MPC184univ.html") ("MPC184-Weld" "ans_elem/Hlp_E_MPC184weld.html") ("MPCHG" "ans_cmd/Hlp_C_MPCHG.html") ("MPCOPY" "ans_cmd/Hlp_C_MPCOPY.html") ("MPDATA" "ans_cmd/Hlp_C_MPDATA.html") ("MPDELE" "ans_cmd/Hlp_C_MPDELE.html") ("MPDRES" "ans_cmd/Hlp_C_MPDRES.html") ("MPLIST" "ans_cmd/Hlp_C_MPLIST.html") ("MPPLOT" "ans_cmd/Hlp_C_MPPLOT.html") ("MPREAD" "ans_cmd/Hlp_C_MPREAD.html") ("MPRINT" "ans_cmd/Hlp_C_MPRINT.html") ("MPTEMP" "ans_cmd/Hlp_C_MPTEMP.html") ("MPTGEN" "ans_cmd/Hlp_C_MPTGEN.html") ("MPTRES" "ans_cmd/Hlp_C_MPTRES.html") ("MPWRITE" "ans_cmd/Hlp_C_MPWRITE.html") ("MRPM" "ans_cmd/Hlp_C_MRPM.html") ("MSAVE" "ans_cmd/Hlp_C_MSAVE.html") ("MSHAPE" "ans_cmd/Hlp_C_MSHAPE.html") ("MSHCOPY" "ans_cmd/Hlp_C_MSHCOPY.html") ("MSHKEY" "ans_cmd/Hlp_C_MSHKEY.html") ("MSHMID" "ans_cmd/Hlp_C_MSHMID.html") ("MSHPATTERN" "ans_cmd/Hlp_C_MSHPATTERN.html") ("MSOLVE" "ans_cmd/Hlp_C_MSOLVE.html") ("MSTOLE" "ans_cmd/Hlp_C_MSTOLE.html") ("MXPAND" "ans_cmd/Hlp_C_MXPAND.html") ("N" "ans_cmd/Hlp_C_N.html") ("NANG" "ans_cmd/Hlp_C_NANG.html") ("NAXIS" "ans_cmd/Hlp_C_NAXIS.html") ("NCNV" "ans_cmd/Hlp_C_NCNV.html") ("NDELE" "ans_cmd/Hlp_C_NDELE.html") ("NDIST" "ans_cmd/Hlp_C_NDIST.html") ("NDSURF" "ans_cmd/Hlp_C_NDSURF.html") ("NEQIT" "ans_cmd/Hlp_C_NEQIT.html") ("NFORCE" "ans_cmd/Hlp_C_NFORCE.html") ("NGEN" "ans_cmd/Hlp_C_NGEN.html") ("NKPT" "ans_cmd/Hlp_C_NKPT.html") ("NLADAPTIVE" "ans_cmd/Hlp_C_NLADAPTIVE.html") ("NLDIAG" "ans_cmd/Hlp_C_NLDIAG.html") ("NLDPOST" "ans_cmd/Hlp_C_NLDPOST.html") ("NLGEOM" "ans_cmd/Hlp_C_NLGEOM.html") ("NLHIST" "ans_cmd/Hlp_C_NLHIST.html") ("NLIST" "ans_cmd/Hlp_C_NLIST.html") ("NLMESH" "ans_cmd/Hlp_C_NLMESH.html") ("NLOG" "ans_cmd/Hlp_C_NLOG.html") ("NLOPT" "ans_cmd/Hlp_C_NLOPT.html") ("NMODIF" "ans_cmd/Hlp_C_NMODIF.html") ("NODES" "ans_cmd/Hlp_C_NODES.html") ("NOOFFSET" "ans_cmd/Hlp_C_NOOFFSET.html") ("NORA" "ans_cmd/Hlp_C_NORA.html") ("NORL" "ans_cmd/Hlp_C_NORL.html") ("NPLOT" "ans_cmd/Hlp_C_NPLOT.html") ("NPRINT" "ans_cmd/Hlp_C_NPRINT.html") ("NREAD" "ans_cmd/Hlp_C_NREAD.html") ("NREFINE" "ans_cmd/Hlp_C_NREFINE.html") ("NRLSUM" "ans_cmd/Hlp_C_NRLSUM.html") ("NROPT" "ans_cmd/Hlp_C_NROPT.html") ("NROTAT" "ans_cmd/Hlp_C_NROTAT.html") ("NRRANG" "ans_cmd/Hlp_C_NRRANG.html") ("NSCALE" "ans_cmd/Hlp_C_NSCALE.html") ("NSEL" "ans_cmd/Hlp_C_NSEL.html") ("NSLA" "ans_cmd/Hlp_C_NSLA.html") ("NSLE" "ans_cmd/Hlp_C_NSLE.html") ("NSLK" "ans_cmd/Hlp_C_NSLK.html") ("NSLL" "ans_cmd/Hlp_C_NSLL.html") ("NSLV" "ans_cmd/Hlp_C_NSLV.html") ("NSMOOTH" "ans_cmd/Hlp_C_NSMOOTH.html") ("NSOL" "ans_cmd/Hlp_C_NSOL.html") ("NSORT" "ans_cmd/Hlp_C_NSORT.html") ("NSTORE" "ans_cmd/Hlp_C_NSTORE.html") ("NSUBST" "ans_cmd/Hlp_C_NSUBST.html") ("NSVR" "ans_cmd/Hlp_C_NSVR.html") ("NSYM" "ans_cmd/Hlp_C_NSYM.html") ("NUMCMP" "ans_cmd/Hlp_C_NUMCMP.html") ("NUMEXP" "ans_cmd/Hlp_C_NUMEXP.html") ("NUMMRG" "ans_cmd/Hlp_C_NUMMRG.html") ("NUMOFF" "ans_cmd/Hlp_C_NUMOFF.html") ("NUMSTR" "ans_cmd/Hlp_C_NUMSTR.html") ("NUMVAR" "ans_cmd/Hlp_C_NUMVAR.html") ("NUSORT" "ans_cmd/Hlp_C_NUSORT.html") ("NWPAVE" "ans_cmd/Hlp_C_NWPAVE.html") ("NWPLAN" "ans_cmd/Hlp_C_NWPLAN.html") ("NWRITE" "ans_cmd/Hlp_C_NWRITE.html") ("OCDATA" "ans_cmd/Hlp_C_OCDATA.html") ("OCDELETE" "ans_cmd/Hlp_C_OCDELETE.html") ("OCLIST" "ans_cmd/Hlp_C_OCLIST.html") ("OCREAD" "ans_cmd/Hlp_C_OCREAD.html") ("OCTABLE" "ans_cmd/Hlp_C_OCTABLE.html") ("OCTYPE" "ans_cmd/Hlp_C_OCTYPE.html") ("OCZONE" "ans_cmd/Hlp_C_OCZONE.html") ("OMEGA" "ans_cmd/Hlp_C_OMEGA.html") ("OPERATE" "ans_cmd/Hlp_C_OPERATE.html") ("OPNCONTROL" "ans_cmd/Hlp_C_OPNCONTROL.html") ("OUTAERO" "ans_cmd/OUTAERO.html") ("OUTGEOM" "ans_cmd/Hlp_C_OUTGEOM.html") ("OUTOPT" "ans_cmd/Hlp_C_OUTOPT.html") ("OUTPR" "ans_cmd/Hlp_C_OUTPR.html") ("OUTRES" "ans_cmd/Hlp_C_OUTturRES.html") ("OVCHECK" "ans_cmd/Hlp_C_OVCHECK.html") ("PADELE" "ans_cmd/Hlp_C_PADELE.html") ("PAGET" "ans_cmd/Hlp_C_PAGET.html") ("PAPUT" "ans_cmd/Hlp_C_PAPUT.html") ("PARESU" "ans_cmd/Hlp_C_PARESU.html") ("PARRES" "ans_cmd/Hlp_C_PARRES.html") ("PARSAV" "ans_cmd/Hlp_C_PARSAV.html") ("PASAVE" "ans_cmd/Hlp_C_PASAVE.html") ("PATH" "ans_cmd/Hlp_C_PATH.html") ("PAUSE" "ans_cmd/Hlp_C_PAUSE.html") ("PCALC" "ans_cmd/Hlp_C_PCALC.html") ("PCGOPT" "ans_cmd/Hlp_C_PCGOPT.html") ("PCIRC" "ans_cmd/Hlp_C_PCIRC.html") ("PCORRO" "ans_arch/Hlp_C_PCORRO.html") ("PCROSS" "ans_cmd/Hlp_C_PCROSS.html") ("PDEF" "ans_cmd/Hlp_C_PDEF.html") ("PDOT" "ans_cmd/Hlp_C_PDOT.html") ("PDRAG" "ans_arch/Hlp_C_PDRAG.html") ("PERBC2D" "ans_cmd/Hlp_C_PERBC2D.html") ("PERTURB" "ans_cmd/Hlp_C_PERTURB.html") ("PFACT" "ans_cmd/Hlp_C_PFACT.html") ("PFLUID" "ans_arch/Hlp_C_PFLUID.html") ("PGAP" "ans_arch/Hlp_C_PGAP.html") ("PHYSICS" "ans_cmd/Hlp_C_PHYSICS.html") ("PINSUL" "ans_arch/Hlp_C_PINSUL.html") ("PIPE" "ans_arch/Hlp_C_PIPE.html") ("PIPE16" "ans_arch/Hlp_E_PIPE16.html") ("PIPE18" "ans_arch/Hlp_E_PIPE18.html") ("PIPE288" "ans_elem/Hlp_E_PIPE288.html") ("PIPE289" "ans_elem/Hlp_E_PIPE289.html") ("PIPE59" "ans_arch/Hlp_E_PIPE59.html") ("PIPES" "ans_elem/Hlp_E_CH3_2.html#allpipes") ("PIVCHECK" "ans_cmd/Hlp_C_PIVCHECK.html") ("\"PLANES\"" "ans_elem/Hlp_E_CH3_2.html#allplanes") ("PLANE121" "ans_elem/Hlp_E_PLANE121.html") ("PLANE13" "ans_elem/Hlp_E_PLANE13.html") ("PLANE182" "ans_elem/Hlp_E_PLANE182.html") ("PLANE183" "ans_elem/Hlp_E_PLANE183.html") ("PLANE222" "ans_elem/Hlp_E_PLANE222.html") ("PLANE223" "ans_elem/Hlp_E_PLANE223.html") ("PLANE230" "ans_elem/Hlp_E_PLANE230.html") ("PLANE233" "ans_elem/Hlp_E_PLANE233.html") ("PLANE238" "ans_elem/Hlp_E_PLANE238.html") ("PLANE25" "ans_elem/Hlp_E_PLANE25.html") ("PLANE292" "ans_elem/Hlp_E_PLANE292.html") ("PLANE293" "ans_elem/Hlp_E_PLANE293.html") ("PLANE35" "ans_elem/Hlp_E_PLANE35.html") ("PLANE42" "ans_arch/Hlp_E_PLANE42.html") ("PLANE55" "ans_elem/Hlp_E_PLANE55.html") ("PLANE75" "ans_elem/Hlp_E_PLANE75.html") ("PLANE77" "ans_elem/Hlp_E_PLANE77.html") ("PLANE78" "ans_elem/Hlp_E_PLANE78.html") ("PLANE82" "ans_arch/Hlp_E_PLANE82.html") ("PLANE83" "ans_elem/Hlp_E_PLANE83.html") ("PLAS" "ans_cmd/Hlp_C_PLAS.html") ("PLCAMP" "ans_cmd/Hlp_C_PLCAMP.html") ("PLCFREQ" "ans_cmd/Hlp_C_PLCFREQ.html") ("PLCHIST" "ans_cmd/Hlp_C_PLCHIST.html") ("PLCINT" "ans_cmd/Hlp_C_PLCINT.html") ("PLCKSURF" "ans_cmd/Hlp_C_PLCKSURF.html") ("PLCPLX" "ans_cmd/Hlp_C_PLCPLX.html") ("PLCRACK" "ans_arch/Hlp_C_PLCRACK.html") ("PLDISP" "ans_cmd/Hlp_C_PLDISP.html") ("PLESOL" "ans_cmd/Hlp_C_PLESOL.html") ("PLETAB" "ans_cmd/Hlp_C_PLETAB.html") ("PLF2D" "ans_cmd/Hlp_C_PLF2D.html") ("PLFAR" "ans_cmd/Hlp_C_PLFAR.html") ("PLGEOM" "ans_cmd/Hlp_C_PLGEOM.html") ("PLLS" "ans_cmd/Hlp_C_PLLS.html") ("PLMAP" "ans_cmd/Hlp_C_PLMAP.html") ("PLMC" "ans_cmd/Hlp_C_PLMC.html") ("PLNEAR" "ans_cmd/Hlp_C_PLNEAR.html") ("PLNSOL" "ans_cmd/Hlp_C_PLNSOL.html") ("PLORB" "ans_cmd/Hlp_C_PLORB.html") ("PLOTTING" "ans_cmd/Hlp_C_PLOTTING.html") ("PLPAGM" "ans_cmd/Hlp_C_PLPAGM.html") ("PLPATH" "ans_cmd/Hlp_C_PLPATH.html") ("PLSECT" "ans_cmd/Hlp_C_PLSECT.html") ("PLTIME" "ans_cmd/Hlp_C_PLTIME.html") ("PLTRAC" "ans_cmd/Hlp_C_PLTRAC.html") ("PLVAR" "ans_cmd/Hlp_C_PLVAR.html") ("PLVECT" "ans_cmd/Hlp_C_PLVECT.html") ("PLZZ" "ans_cmd/Hlp_C_PLZZ.html") ("PMAP" "ans_cmd/Hlp_C_PMAP.html") ("PMGTRAN" "ans_cmd/Hlp_C_PMGTRAN.html") ("PMLOPT" "ans_cmd/Hlp_C_PMLOPT.html") ("PMLSIZE" "ans_cmd/Hlp_C_PMLSIZE.html") ("PNGR" "ans_cmd/Hlp_C_PNGR.html") ("POINT" "ans_cmd/Hlp_C_POINT.html") ("POLY" "ans_cmd/Hlp_C_POLY.html") ("POPT" "ans_arch/Hlp_C_POPT.html") ("POWERH" "ans_cmd/Hlp_C_POWERH.html") ("PPATH" "ans_cmd/Hlp_C_PPATH.html") ("PPRES" "ans_arch/Hlp_C_PPRES.html") ("PRANGE" "ans_cmd/Hlp_C_PRANGE.html") ("PRAS" "ans_cmd/Hlp_C_PRAS.html") ("PRCAMP" "ans_cmd/Hlp_C_PRCAMP.html") ("PRCINT" "ans_cmd/Hlp_C_PRCINT.html") ("PRCPLX" "ans_cmd/Hlp_C_PRCPLX.html") ("PRED" "ans_cmd/Hlp_C_PRED.html") ("PRENERGY" "ans_cmd/Hlp_C_PRENERGY.html") ("PRERR" "ans_cmd/Hlp_C_PRERR.html") ("PRESOL" "ans_cmd/Hlp_C_PRESOL.html") ("PRETAB" "ans_cmd/Hlp_C_PRETAB.html") ("PRETS" "ans_elem/Hlp_E_CH3_2.html#allprets") ("PRETS179" "ans_elem/Hlp_E_PRETS179.html") ("PRFAR" "ans_cmd/Hlp_C_PRFAR.html") ("PRI2" "ans_cmd/Hlp_C_PRI2.html") ("PRIM" "ans_cmd/Hlp_C_PRIM.html") ("PRINT" "ans_cmd/Hlp_C_PRINT.html") ("PRISM" "ans_cmd/Hlp_C_PRISM.html") ("PRITER" "ans_cmd/Hlp_C_PRITER.html") ("PRJSOL" "ans_cmd/Hlp_C_PRJSOL.html") ("PRMC" "ans_cmd/Hlp_C_PRMC.html") ("PRNEAR" "ans_cmd/Hlp_C_PRNEAR.html") ("PRNLD" "ans_cmd/Hlp_C_PRNLD.html") ("PRNSOL" "ans_cmd/Hlp_C_PRNSOL.html") ("PROD" "ans_cmd/Hlp_C_PROD.html") ("PRORB" "ans_cmd/Hlp_C_PRORB.html") ("PRPATH" "ans_cmd/Hlp_C_PRPATH.html") ("PRRFOR" "ans_cmd/Hlp_C_PRRFOR.html") ("PRRSOL" "ans_cmd/Hlp_C_PRRSOL.html") ("PRSCONTROL" "ans_cmd/Hlp_C_PRSCONTROL.html") ("PRSECT" "ans_cmd/Hlp_C_PRSECT.html") ("PRTIME" "ans_cmd/Hlp_C_PRTIME.html") ("PRVAR" "ans_cmd/Hlp_C_PRVAR.html") ("PRVECT" "ans_cmd/Hlp_C_PRVECT.html") ("PSCONTROL" "ans_cmd/Hlp_C_PSCONTROL.html") ("PSDCOM" "ans_cmd/Hlp_C_PSDCOM.html") ("PSDFRQ" "ans_cmd/Hlp_C_PSDFRQ.html") ("PSDGRAPH" "ans_cmd/Hlp_C_PSDGRAPH.html") ("PSDRES" "ans_cmd/Hlp_C_PSDRES.html") ("PSDSPL" "ans_cmd/Hlp_C_PSDSPL.html") ("PSDUNIT" "ans_cmd/Hlp_C_PSDUNIT.html") ("PSDVAL" "ans_cmd/Hlp_C_PSDVAL.html") ("PSDWAV" "ans_cmd/Hlp_C_PSDWAV.html") ("PSEL" "ans_cmd/Hlp_C_PSEL.html") ("PSMAT" "ans_cmd/Hlp_C_PSMAT.html") ("PSMESH" "ans_cmd/Hlp_C_PSMESH.html") ("PSOLVE" "ans_arch/Hlp_C_PSOLVE.html") ("PSPEC" "ans_arch/Hlp_C_PSPEC.html") ("PSPRNG" "ans_arch/Hlp_C_PSPRNG.html") ("PSTRES" "ans_cmd/Hlp_C_PSTRES.html") ("PSYS" "ans_cmd/Hlp_C_PSYS.html") ("PTEMP" "ans_arch/Hlp_C_PTEMP.html") ("PTR" "ans_cmd/Hlp_C_PTR.html") ("PTXY" "ans_cmd/Hlp_C_PTXY.html") ("PUNIT" "ans_arch/Hlp_C_PUNIT.html") ("PVECT" "ans_cmd/Hlp_C_PVECT.html") ("QDVAL" "ans_cmd/Hlp_C_QDVAL.html") ("QRDOPT" "ans_cmd/Hlp_C_QRDOPT.html") ("QSOPT" "ans_cmd/Hlp_C_QSOPT.html") ("QUAD" "ans_cmd/Hlp_C_QUAD.html") ("QUOT" "ans_cmd/Hlp_C_QUOT.html") ("R" "ans_cmd/Hlp_C_R.html") ("RACE" "ans_cmd/Hlp_C_RACE.html") ("RADOPT" "ans_cmd/Hlp_C_RADOPT.html") ("RAPPND" "ans_cmd/Hlp_C_RAPPND.html") ("RATE" "ans_cmd/Hlp_C_RATE.html") ("RBE3" "ans_cmd/Hlp_C_RBE3.html") ("RCON" "ans_cmd/Hlp_C_RCON.html") ("RCYC" "ans_cmd/Hlp_C_RCYC.html") ("RDEC" "ans_cmd/Hlp_C_RDEC.html") ("RDELE" "ans_cmd/Hlp_C_RDELE.html") ("READ" "ans_cmd/Hlp_C_READ.html") ("REAL" "ans_cmd/Hlp_C_REAL.html") ("REALVAR" "ans_cmd/Hlp_C_REALVAR.html") ("RECTNG" "ans_cmd/Hlp_C_RECTNG.html") ("REDUCE" "ans_arch/Hlp_C_REDUCE.html") ("REINF" "ans_elem/Hlp_E_CH3_2.html#allreinf") ("REINF263" "ans_elem/Hlp_E_REINF263.html") ("REINF264" "ans_elem/Hlp_E_REINF264.html") ("REINF265" "ans_elem/Hlp_E_REINF265.html") ("REMESH" "ans_cmd/Hlp_C_REMESH.html") ("RESCOMBINE" "ans_cmd/Hlp_C_RESCOMBINE.html") ("RESCONTROL" "ans_cmd/Hlp_C_RESCONTROL.html") ("RESET" "ans_cmd/Hlp_C_RESET.html") ("RESP" "ans_cmd/Hlp_C_RESP.html") ("RESUME" "ans_cmd/Hlp_C_RESUME.html") ("RESVEC" "ans_cmd/Hlp_C_RESVEC.html") ("RESWRITE" "ans_cmd/Hlp_C_RESWRITE.html") ("REZONE" "ans_cmd/Hlp_C_REZONE.html") ("RFORCE" "ans_cmd/Hlp_C_RFORCE.html") ("RIGID" "ans_cmd/Hlp_C_RIGID.html") ("RIGRESP" "ans_cmd/Hlp_C_RIGRESP.html") ("RLIST" "ans_cmd/Hlp_C_RLIST.html") ("RMALIST" "ans_cmd/Hlp_C_RMALIST.html") ("RMANL" "ans_cmd/Hlp_C_RMANL.html") ("RMASTER" "ans_cmd/Hlp_C_RMASTER.html") ("RMCAP" "ans_cmd/Hlp_C_RMCAP.html") ("RMCLIST" "ans_cmd/Hlp_C_RMCLIST.html") ("RMFLVEC" "ans_cmd/Hlp_C_RMFLVEC.html") ("RMLVSCALE" "ans_cmd/Hlp_C_RMLVSCALE.html") ("RMMLIST" "ans_cmd/Hlp_C_RMMLIST.html") ("RMMRANGE" "ans_cmd/Hlp_C_RMMRANGE.html") ("RMMSELECT" "ans_cmd/Hlp_C_RMMSELECT.html") ("RMNDISP" "ans_cmd/Hlp_C_RMNDISP.html") ("RMNEVEC" "ans_cmd/Hlp_C_RMNEVEC.html") ("RMODIF" "ans_cmd/Hlp_C_RMODIF.html") ("RMORE" "ans_cmd/Hlp_C_RMORE.html") ("RMPORDER" "ans_cmd/Hlp_C_RMPORDER.html") ("RMRESUME" "ans_cmd/Hlp_C_RMRESUME.html") ("RMRGENERATE" "ans_cmd/Hlp_C_RMRGENERATE.html") ("RMROPTIONS" "ans_cmd/Hlp_C_RMROPTIONS.html") ("RMRPLOT" "ans_cmd/Hlp_C_RMRPLOT.html") ("RMRSTATUS" "ans_cmd/Hlp_C_RMRSTATUS.html") ("RMSAVE" "ans_cmd/Hlp_C_RMSAVE.html") ("RMSMPLE" "ans_cmd/Hlp_C_RMSMPLE.html") ("RMUSE" "ans_cmd/Hlp_C_RMUSE.html") ("RMXPORT" "ans_cmd/Hlp_C_RMXPORT.html") ("ROCK" "ans_cmd/Hlp_C_ROCK.html") ("ROM" "ans_elem/Hlp_E_CH3_2.html#allrom") ("ROM144" "ans_elem/Hlp_E_ROM144.html") ("ROSE" "ans_cmd/Hlp_C_ROSE.html") ("RPOLY" "ans_cmd/Hlp_C_RPOLY.html") ("RPR4" "ans_cmd/Hlp_C_RPR4.html") ("RPRISM" "ans_cmd/Hlp_C_RPRISM.html") ("RPSD" "ans_cmd/Hlp_C_RPSD.html") ("RSMESH" "ans_cmd/Hlp_C_RSMESH.html") ("RSOPT" "ans_cmd/Hlp_C_RSOPT.html") ("RSPLIT" "ans_cmd/Hlp_C_RSPLIT.html") ("RSTMAC" "ans_cmd/Hlp_C_RSTMAC.html") ("RSTOFF" "ans_cmd/Hlp_C_RSTOFF.html") ("RSURF" "ans_cmd/Hlp_C_RSURF.html") ("RSYMM" "ans_cmd/Hlp_C_RSYMM.html") ("RSYS" "ans_cmd/Hlp_C_RSYS.html") ("RUN" "ans_arch/Hlp_C_RUN.html") ("SABS" "ans_cmd/Hlp_C_SABS.html") ("SADD" "ans_cmd/Hlp_C_SADD.html") ("SALLOW" "ans_cmd/Hlp_C_SALLOW.html") ("SAVE" "ans_cmd/Hlp_C_SAVE.html") ("SBCLIST" "ans_cmd/Hlp_C_SBCLIST.html") ("SBCTRAN" "ans_cmd/Hlp_C_SBCTRAN.html") ("SCOPT" "ans_cmd/Hlp_C_SCOPT.html") ("SDELETE" "ans_cmd/Hlp_C_SDELETE.html") ("SE" "ans_cmd/Hlp_C_SE.html") ("SECCONTROL" "ans_cmd/Hlp_C_SECCONTROL.html") ("SECDATA" "ans_cmd/Hlp_C_SECDATA.html") ("SECFUNCTION" "ans_cmd/Hlp_C_SECFUNCTION.html") ("SECJOINT" "ans_cmd/Hlp_C_SECJOINT.html") ("SECLOCK" "ans_cmd/Hlp_C_SECLOCK.html") ("SECMODIF" "ans_cmd/Hlp_C_SECMODIF.html") ("SECNUM" "ans_cmd/Hlp_C_SECNUM.html") ("SECOFFSET" "ans_cmd/Hlp_C_SECOFFSET.html") ("SECPLOT" "ans_cmd/Hlp_C_SECPLOT.html") ("SECREAD" "ans_cmd/Hlp_C_SECREAD.html") ("SECSTOP" "ans_cmd/Hlp_C_SECSTOP.html") ("SECTYPE" "ans_cmd/Hlp_C_SECTYPE.html") ("SECWRITE" "ans_cmd/Hlp_C_SECWRITE.html") ("SED" "ans_cmd/Hlp_C_SED.html") ("SEDLIST" "ans_cmd/Hlp_C_SEDLIST.html") ("SEEXP" "ans_cmd/Hlp_C_SEEXP.html") ("SELIST" "ans_cmd/Hlp_C_SELIST.html") ("SELM" "ans_cmd/Hlp_C_SELM.html") ("SELTOL" "ans_cmd/Hlp_C_SELTOL.html") ("SEMIIMPLICIT" "ans_cmd/Hlp_C_SEMI.html") ("SENERGY" "ans_cmd/Hlp_C_SENERGY.html") ("SEOPT" "ans_cmd/Hlp_C_SEOPT.html") ("SESYMM" "ans_cmd/Hlp_C_SESYMM.html") ("SET" "ans_cmd/Hlp_C_SET.html") ("SETFGAP" "ans_cmd/Hlp_C_SETFGAP.html") ("SETRAN" "ans_cmd/Hlp_C_SETRAN.html") ("SEXP" "ans_cmd/Hlp_C_SEXP.html") ("SF" "ans_cmd/Hlp_C_SF.html") ("SFA" "ans_cmd/Hlp_C_SFA.html") ("SFACT" "ans_cmd/Hlp_C_SFACT.html") ("SFADELE" "ans_cmd/Hlp_C_SFADELE.html") ("SFALIST" "ans_cmd/Hlp_C_SFALIST.html") ("SFBEAM" "ans_cmd/Hlp_C_SFBEAM.html") ("SFCALC" "ans_cmd/Hlp_C_SFCALC.html") ("SFCONTROL" "ans_cmd/Hlp_C_SFCONTROL.html") ("SFCUM" "ans_cmd/Hlp_C_SFCUM.html") ("SFDELE" "ans_cmd/Hlp_C_SFDELE.html") ("SFE" "ans_cmd/Hlp_C_SFE.html") ("SFEDELE" "ans_cmd/Hlp_C_SFEDELE.html") ("SFELIST" "ans_cmd/Hlp_C_SFELIST.html") ("SFFUN" "ans_cmd/Hlp_C_SFFUN.html") ("SFGRAD" "ans_cmd/Hlp_C_SFGRAD.html") ("SFL" "ans_cmd/Hlp_C_SFL.html") ("SFLDELE" "ans_cmd/Hlp_C_SFLDELE.html") ("SFLEX" "ans_cmd/Hlp_C_SFLEX.html") ("SFLIST" "ans_cmd/Hlp_C_SFLIST.html") ("SFLLIST" "ans_cmd/Hlp_C_SFLLIST.html") ("SFSCALE" "ans_cmd/Hlp_C_SFSCALE.html") ("SFTRAN" "ans_cmd/Hlp_C_SFTRAN.html") ("SHELL" "ans_cmd/Hlp_C_SHELL.html") ("SHELL131" "ans_elem/Hlp_E_SHELL131.html") ("SHELL132" "ans_elem/Hlp_E_SHELL132.html") ("SHELL157" "ans_elem/Hlp_E_SHELL157.html") ("SHELL181" "ans_elem/Hlp_E_SHELL181.html") ("SHELL208" "ans_elem/Hlp_E_SHELL208.html") ("SHELL209" "ans_elem/Hlp_E_SHELL209.html") ("SHELL281" "ans_elem/Hlp_E_SHELL281.html") ("SHELL61" "ans_elem/Hlp_E_SHELL61.html") ("SHELL63" "ans_arch/Hlp_E_SHELL63.html") ("\"SHELLS\"" "ans_elem/Hlp_E_CH3_2.html#allshells") ("SHPP" "ans_cmd/Hlp_C_SHPP.html") ("SHSD" "ans_cmd/Hlp_C_SHSD.html") ("SLIST" "ans_cmd/Hlp_C_SLIST.html") ("SLOAD" "ans_cmd/Hlp_C_SLOAD.html") ("SMALL" "ans_cmd/Hlp_C_SMALL.html") ("SMAX" "ans_cmd/Hlp_C_SMAX.html") ("SMBODY" "ans_cmd/Hlp_C_SMBODY.html") ("SMCONS" "ans_cmd/Hlp_C_SMCONS.html") ("SMFOR" "ans_cmd/Hlp_C_SMFOR.html") ("SMIN" "ans_cmd/Hlp_C_SMIN.html") ("SMOOTH" "ans_cmd/Hlp_C_SMOOTH.html") ("SMRTSIZE" "ans_cmd/Hlp_C_SMRTSIZE.html") ("SMSURF" "ans_cmd/Hlp_C_SMSURF.html") ("SMULT" "ans_cmd/Hlp_C_SMULT.html") ("SNOPTION" "ans_cmd/Hlp_C_SNOPTION.html") ("\"SOLIDS\"" "ans_elem/Hlp_E_CH3_2.html#allsolids") ("SOLID122" "ans_elem/Hlp_E_SOLID122.html") ("SOLID123" "ans_elem/Hlp_E_SOLID123.html") ("SOLID185" "ans_elem/Hlp_E_SOLID185.html") ("SOLID186" "ans_elem/Hlp_E_SOLID186.html") ("SOLID187" "ans_elem/Hlp_E_SOLID187.html") ("SOLID226" "ans_elem/Hlp_E_SOLID226.html") ("SOLID227" "ans_elem/Hlp_E_SOLID227.html") ("SOLID231" "ans_elem/Hlp_E_SOLID231.html") ("SOLID232" "ans_elem/Hlp_E_SOLID232.html") ("SOLID236" "ans_elem/Hlp_E_SOLID236.html") ("SOLID237" "ans_elem/Hlp_E_SOLID237.html") ("SOLID239" "ans_elem/Hlp_E_SOLID239.html") ("SOLID240" "ans_elem/Hlp_E_SOLID240.html") ("SOLID272" "ans_elem/Hlp_E_SOLID272.html") ("SOLID273" "ans_elem/Hlp_E_SOLID273.html") ("SOLID278" "ans_elem/Hlp_E_SOLID278.html") ("SOLID279" "ans_elem/Hlp_E_SOLID279.html") ("SOLID285" "ans_elem/Hlp_E_SOLID285.html") ("SOLID291" "ans_elem/Hlp_E_SOLID291.html") ("SOLID45" "ans_arch/Hlp_E_SOLID45.html") ("SOLID5" "ans_elem/Hlp_E_SOLID5.html") ("SOLID65" "ans_arch/Hlp_E_SOLID65.html") ("SOLID70" "ans_elem/Hlp_E_SOLID70.html") ("SOLID87" "ans_elem/Hlp_E_SOLID87.html") ("SOLID90" "ans_elem/Hlp_E_SOLID90.html") ("SOLID92" "ans_arch/Hlp_E_SOLID92.html") ("SOLID95" "ans_arch/Hlp_E_SOLID95.html") ("SOLID96" "ans_elem/Hlp_E_SOLID96.html") ("SOLID98" "ans_elem/Hlp_E_SOLID98.html") ("SOLSH" "ans_elem/Hlp_E_CH3_2.html#allsolsh") ("SOLSH190" "ans_elem/Hlp_E_SOLSH190.html") ("SOLU" "ans_cmd/Hlp_C_SOLU.html") ("SOLUOPT" "ans_cmd/Hlp_C_SOLUOPT.html") ("SOLVE" "ans_cmd/Hlp_C_SOLVE.html") ("SORT" "ans_cmd/Hlp_C_SORT.html") ("SOURC" "ans_elem/Hlp_E_CH3_2.html#allsourc") ("SOURC36" "ans_elem/Hlp_E_SOURC36.html") ("SOURCE" "ans_cmd/Hlp_C_SOURCE.html") ("SPACE" "ans_cmd/Hlp_C_SPACE.html") ("SPCNOD" "ans_cmd/Hlp_C_SPCNOD.html") ("SPCTEMP" "ans_cmd/Hlp_C_SPCTEMP.html") ("SPDAMP" "ans_cmd/Hlp_C_SPDAMP.html") ("SPEC" "ans_cmd/Hlp_C_SPEC.html") ("SPFREQ" "ans_cmd/Hlp_C_SPFREQ.html") ("SPGRAPH" "ans_cmd/Hlp_C_SPGRAPH.html") ("SPH4" "ans_cmd/Hlp_C_SPH4.html") ("SPH5" "ans_cmd/Hlp_C_SPH5.html") ("SPHERE" "ans_cmd/Hlp_C_SPHERE.html") ("SPLINE" "ans_cmd/Hlp_C_SPLINE.html") ("SPLOT" "ans_cmd/Hlp_C_SPLOT.html") ("SPMWRITE" "ans_cmd/Hlp_C_SPMWRITE.html") ("SPOINT" "ans_cmd/Hlp_C_SPOINT.html") ("SPOPT" "ans_cmd/Hlp_C_SPOPT.html") ("SPREAD" "ans_cmd/Hlp_C_SPREAD.html") ("SPTOPT" "ans_cmd/Hlp_C_SPTOPT.html") ("SPUNIT" "ans_cmd/Hlp_C_SPUNIT.html") ("SPVAL" "ans_cmd/Hlp_C_SPVAL.html") ("SQRT" "ans_cmd/Hlp_C_SQRT.html") ("SRSS" "ans_cmd/Hlp_C_SRSS.html") ("SSBT" "ans_cmd/Hlp_C_SSBT.html") ("SSLN" "ans_cmd/Hlp_C_SSLN.html") ("SSMT" "ans_cmd/Hlp_C_SSMT.html") ("SSOPT" "ans_cmd/Hlp_C_SSOPT.html") ("SSPA" "ans_cmd/Hlp_C_SSPA.html") ("SSPB" "ans_cmd/Hlp_C_SSPB.html") ("SSPD" "ans_cmd/Hlp_C_SSPD.html") ("SSPE" "ans_cmd/Hlp_C_SSPE.html") ("SSPM" "ans_cmd/Hlp_C_SSPM.html") ("SSTATE" "ans_cmd/Hlp_C_SSTATE.html") ("SSTIF" "ans_arch/Hlp_C_SSTIF.html") ("SSUM" "ans_cmd/Hlp_C_SSUM.html") ("STABILIZE" "ans_cmd/Hlp_C_STABILIZE.html") ("STAT" "ans_cmd/Hlp_C_STAT.html") ("STEF" "ans_cmd/Hlp_C_STEF.html") ("STORE" "ans_cmd/Hlp_C_STORE.html") ("SUBOPT" "ans_cmd/Hlp_C_SUBOPT.html") ("SUBSET" "ans_cmd/Hlp_C_SUBSET.html") ("SUCALC" "ans_cmd/Hlp_C_SUCALC.html") ("SUCR" "ans_cmd/Hlp_C_SUCR.html") ("SUDEL" "ans_cmd/Hlp_C_SUDEL.html") ("SUEVAL" "ans_cmd/Hlp_C_SUEVAL.html") ("SUGET" "ans_cmd/Hlp_C_SUGET.html") ("SUMAP" "ans_cmd/Hlp_C_SUMAP.html") ("SUMTYPE" "ans_cmd/Hlp_C_SUMTYPE.html") ("SUPL" "ans_cmd/Hlp_C_SUPL.html") ("SUPR" "ans_cmd/Hlp_C_SUPR.html") ("SURESU" "ans_cmd/Hlp_C_SURESU.html") ("SURF" "ans_elem/Hlp_E_CH3_2.html#allsurf") ("SURF151" "ans_elem/Hlp_E_SURF151.html") ("SURF152" "ans_elem/Hlp_E_SURF152.html") ("SURF153" "ans_elem/Hlp_E_SURF153.html") ("SURF154" "ans_elem/Hlp_E_SURF154.html") ("SURF155" "ans_elem/Hlp_E_SURF155.html") ("SURF156" "ans_elem/Hlp_E_SURF156.html") ("SURF159" "ans_elem/Hlp_E_SURF159.html") ("SURF251" "ans_elem/Hlp_E_SURF251.html") ("SURF252" "ans_elem/Hlp_E_SURF252.html") ("SUSAVE" "ans_cmd/Hlp_C_SUSAVE.html") ("SUSEL" "ans_cmd/Hlp_C_SUSEL.html") ("SUVECT" "ans_cmd/Hlp_C_SUVECT.html") ("SV" "ans_cmd/Hlp_C_SV.html") ("SVPLOT" "ans_cmd/Hlp_C_SVPLOT.html") ("SVTYP" "ans_cmd/Hlp_C_SVTYP.html") ("SWADD" "ans_cmd/Hlp_C_SWADD.html") ("SWDEL" "ans_cmd/Hlp_C_SWDEL.html") ("SWGEN" "ans_cmd/Hlp_C_SWGEN.html") ("SWLIST" "ans_cmd/Hlp_C_SWLIST.html") ("SYNCHRO" "ans_cmd/Hlp_C_SYNCHRO.html") ("TALLOW" "ans_cmd/Hlp_C_TALLOW.html") ("TARGE" "ans_elem/Hlp_E_CH3_2.html#alltarge") ("TARGE169" "ans_elem/Hlp_E_TARGE169.html") ("TARGE170" "ans_elem/Hlp_E_TARGE170.html") ("TARGET" "ans_cmd/Hlp_C_TARGET.html") ("TB" "ans_cmd/Hlp_C_TB.html") ("TBCOPY" "ans_cmd/Hlp_C_TBCOPY.html") ("TBDATA" "ans_cmd/Hlp_C_TBDATA.html") ("TBDELE" "ans_cmd/Hlp_C_TBDELE.html") ("TBEO" "ans_cmd/Hlp_C_TBEO.html") ("TBFIELD" "ans_cmd/Hlp_C_TBFIELD.html") ("TBFT" "ans_cmd/Hlp_C_TBFT.html") ("TBIN" "ans_cmd/Hlp_C_TBIN.html") ("TBLE" "ans_cmd/Hlp_C_TBLE.html") ("TBLIST" "ans_cmd/Hlp_C_TBLIST.html") ("TBMODIF" "ans_cmd/Hlp_C_TBMODIF.html") ("TBPLOT" "ans_cmd/Hlp_C_TBPLOT.html") ("TBPT" "ans_cmd/Hlp_C_TBPT.html") ("TBTEMP" "ans_cmd/Hlp_C_TBTEMP.html") ("TCHG" "ans_cmd/Hlp_C_TCHG.html") ("TEE" "ans_arch/Hlp_C_TEE.html") ("THEXPAND" "ans_cmd/Hlp_C_THEXPAND.html") ("THOPT" "ans_cmd/Hlp_C_THOPT.html") ("TIFF" "ans_cmd/Hlp_C_TIFF.html") ("TIME" "ans_cmd/Hlp_C_TIME.html") ("TIMERANGE" "ans_cmd/Hlp_C_TIMERANGE.html") ("TIMINT" "ans_cmd/Hlp_C_TIMINT.html") ("TIMP" "ans_cmd/Hlp_C_TIMP.html") ("TINTP" "ans_cmd/Hlp_C_TINTP.html") ("TOFFST" "ans_cmd/Hlp_C_TOFFST.html") ("TORUS" "ans_cmd/Hlp_C_TORUS.html") ("TRANS126" "ans_elem/Hlp_E_TRANS126.html") ("TRANSFER" "ans_cmd/Hlp_C_TRANSFER.html") ("TREF" "ans_cmd/Hlp_C_TREF.html") ("TRNOPT" "ans_cmd/Hlp_C_TRNOPT.html") ("TRPDEL" "ans_cmd/Hlp_C_TRPDEL.html") ("TRPLIS" "ans_cmd/Hlp_C_TRPLIS.html") ("TRPOIN" "ans_cmd/Hlp_C_TRPOIN.html") ("TRTIME" "ans_cmd/Hlp_C_TRTIME.html") ("TSHAP" "ans_cmd/Hlp_C_TSHAP.html") ("TSRES" "ans_cmd/Hlp_C_TSRES.html") ("TUNIF" "ans_cmd/Hlp_C_TUNIF.html") ("TVAR" "ans_cmd/Hlp_C_TVAR.html") ("TYPE" "ans_cmd/Hlp_C_TYPE.html") ("UIMP" "ans_cmd/Hlp_C_UIMP.html") ("UNDELETE" "ans_cmd/Hlp_C_UNDELETE.html") ("UNDO" "ans_cmd/Hlp_C_UNDO.html") ("UNPAUSE" "ans_cmd/Hlp_C_UNPAUSE.html") ("UPCOORD" "ans_cmd/Hlp_C_UPCOORD.html") ("UPGEOM" "ans_cmd/Hlp_C_UPGEOM.html") ("USER300" "ans_elem/Hlp_E_USER300.html") ("USRCAL" "ans_cmd/Hlp_C_USRCAL.html") ("USRDOF" "ans_cmd/Hlp_C_USRDOF.html") ("USRELEM" "ans_cmd/Hlp_C_USRELEM.html") ("V" "ans_cmd/Hlp_C_V.html") ("V2DOPT" "ans_cmd/Hlp_C_V2DOPT.html") ("VA" "ans_cmd/Hlp_C_VA.html") ("VADD" "ans_cmd/Hlp_C_VADD.html") ("VALVE" "ans_arch/Hlp_C_VALVE.html") ("VARDEL" "ans_cmd/Hlp_C_VARDEL.html") ("VARNAM" "ans_cmd/Hlp_C_VARNAM.html") ("VATT" "ans_cmd/Hlp_C_VATT.html") ("VCLEAR" "ans_cmd/Hlp_C_VCLEAR.html") ("VCROSS" "ans_cmd/Hlp_C_VCROSS.html") ("VDDAM" "ans_cmd/Hlp_C_VDDAM.html") ("VDELE" "ans_cmd/Hlp_C_VDELE.html") ("VDGL" "ans_cmd/Hlp_C_VDGL.html") ("VDOT" "ans_cmd/Hlp_C_VDOT.html") ("VDRAG" "ans_cmd/Hlp_C_VDRAG.html") ("VEORIENT" "ans_cmd/Hlp_C_VEORIENT.html") ("VEXT" "ans_cmd/Hlp_C_VEXT.html") ("VFOPT" "ans_cmd/Hlp_C_VFOPT.html") ("VFQUERY" "ans_cmd/Hlp_C_VFQUERY.html") ("VFSM" "ans_cmd/Hlp_C_VFSM.html") ("VGEN" "ans_cmd/Hlp_C_VGEN.html") ("VGET" "ans_cmd/Hlp_C_VGET.html") ("VGLUE" "ans_cmd/Hlp_C_VGLUE.html") ("VIMP" "ans_cmd/Hlp_C_VIMP.html") ("VINP" "ans_cmd/Hlp_C_VINP.html") ("VINV" "ans_cmd/Hlp_C_VINV.html") ("VLIST" "ans_cmd/Hlp_C_VLIST.html") ("VLSCALE" "ans_cmd/Hlp_C_VLSCALE.html") ("VMESH" "ans_cmd/Hlp_C_VMESH.html") ("VOFFST" "ans_cmd/Hlp_C_VOFFST.html") ("VOLUMES" "ans_cmd/Hlp_C_VOLUMES.html") ("VOVLAP" "ans_cmd/Hlp_C_VOVLAP.html") ("VPLOT" "ans_cmd/Hlp_C_VPLOT.html") ("VPTN" "ans_cmd/Hlp_C_VPTN.html") ("VPUT" "ans_cmd/Hlp_C_VPUT.html") ("VROTAT" "ans_cmd/Hlp_C_VROTAT.html") ("VSBA" "ans_cmd/Hlp_C_VSBA.html") ("VSBV" "ans_cmd/Hlp_C_VSBV.html") ("VSBW" "ans_cmd/Hlp_C_VSBW.html") ("VSEL" "ans_cmd/Hlp_C_VSEL.html") ("VSLA" "ans_cmd/Hlp_C_VSLA.html") ("VSUM" "ans_cmd/Hlp_C_VSUM.html") ("VSWEEP" "ans_cmd/Hlp_C_VSWEEP.html") ("VSYMM" "ans_cmd/Hlp_C_VSYMM.html") ("VTRAN" "ans_cmd/Hlp_C_VTRAN.html") ("VTYPE" "ans_cmd/Hlp_C_VTYPE.html") ("WPAVE" "ans_cmd/Hlp_C_WPAVE.html") ("WPCSYS" "ans_cmd/Hlp_C_WPCSYS.html") ("WPLANE" "ans_cmd/Hlp_C_WPLANE.html") ("WPOFFS" "ans_cmd/Hlp_C_WPOFFS.html") ("WPROTA" "ans_cmd/Hlp_C_WPROTA.html") ("WPSTYL" "ans_cmd/Hlp_C_WPSTYL.html") ("WRFULL" "ans_cmd/Hlp_C_WRFULL.html") ("WRITE" "ans_cmd/Hlp_C_WRITE.html") ("WRITEMAP" "ans_cmd/Hlp_C_WRITEMAP.html") ("WSPRINGS" "ans_cmd/Hlp_C_WSPRINGS.html") ("WTBCREATE" "ans_cmd/WTBCREATE.html") ("XFCRKMESH" "ans_cmd/Hlp_C_XFCRKMESH.html") ("XFDATA" "ans_cmd/Hlp_C_XFDATA.html") ("XFENRICH" "ans_cmd/Hlp_C_XFENRICH.html") ("XFLIST" "ans_cmd/Hlp_C_XFLIST.html") ("XVAR" "ans_cmd/Hlp_C_XVAR.html") ("notfound" "ans_wid/notfound.html") ("p26calc" "ans_bas/Hlp_G_BASP26calc.html") ("p26export" "ans_bas/Hlp_G_BASP26export.html") ("p26import" "ans_bas/Hlp_G_BASP26import.html") ("p26list" "ans_bas/Hlp_G_BASP26graph.html#p26list") ("p26plot" "ans_bas/Hlp_G_BASP26graph.html#p26plot") ("p26vardefine" "ans_bas/Hlp_G_BASP26define.html") ("p26viewer" "ans_bas/Hlp_G_BASP26viewer.html") ("s-AUX3" "ans_cmd/Hlp_C_AUX3_sl.html") ("~CAT5" "ans_cmd/Hlp_C_CAT5IN.html") ("~CAT5IN" "ans_cmd/Hlp_C_CAT5IN.html") ("~CATIAIN" "ans_cmd/Hlp_C_CATIAIN.html") ("~PARAIN" "ans_cmd/Hlp_C_PARAIN.html") ("~PROEIN" "ans_cmd/Hlp_C_PROEIN.html") ("~SATIN" "ans_cmd/Hlp_C_SATIN.html") ("~UGIN" "ans_cmd/Hlp_C_UGIN.html") ("\"RELEASE NOTES\"" "ai_rn/global_releasenotes.html") ("\"CONTACT TECHNOLOGY GUIDE\"" "ans_ctec/ctectoc.html") ("\"PARAMETRIC DESIGN LANGUAGE GUIDE\"" "ans_apdl/Hlp_P_APDLTOC.html") ("\"STRUCTURAL ANALYSIS GUIDE\"" "ans_str/Hlp_G_StrTOC.html") ("\"ADVANCED ANALYSIS TECHNIQUES GUIDE\"" "ans_str/Hlp_G_AdvTOC.html") ("\"MATERIAL MODELS\"" "ans_mat/ans_mat.html") ("\"THEORY REFERENCE\"" "ans_thry/ansys.theory.html"))
"Ansys help index alist.")

(defconst apdl-completions
'("SOLID5" "LINK11" "PLANE13" "COMBIN14" "MASS21" "PLANE25" "MATRIX27" "FLUID29" "FLUID30" "LINK31" "LINK33" "LINK34" "PLANE35" "SOURC36" "COMBIN37" "FLUID38" "COMBIN39" "COMBIN40" "INFIN47" "MATRIX50" "PLANE55" "SHELL61" "LINK68" "SOLID70" "MASS71" "PLANE75" "PLANE77" "PLANE78" "PLANE83" "SOLID87" "SOLID90" "CIRCU94" "SOLID96" "SOLID98" "INFIN110" "INFIN111" "FLUID116" "PLANE121" "SOLID122" "SOLID123" "CIRCU124" "CIRCU125" "TRANS126" "FLUID129" "FLUID130" "SHELL131" "SHELL132" "FLUID136" "FLUID138" "FLUID139" "ROM144" "SURF151" "SURF152" "SURF153" "SURF154" "SURF155" "SURF156" "SHELL157" "SURF159" "TARGE169" "TARGE170" "CONTA172" "CONTA174" "CONTA175" "CONTA177" "CONTA178" "PRETS179" "LINK180" "SHELL181" "PLANE182" "PLANE183" "MPC184" "SOLID185" "SOLID186" "SOLID187" "BEAM188" "BEAM189" "SOLSH190" "INTER192" "INTER193" "INTER194" "INTER195" "MESH200" "FOLLW201" "INTER202" "INTER203" "INTER204" "INTER205" "SHELL208" "SHELL209" "CPT212" "CPT213" "COMBI214" "CPT215" "CPT216" "CPT217" "FLUID218" "FLUID220" "FLUID221" "PLANE222" "PLANE223" "SOLID226" "SOLID227" "PLANE230" "SOLID231" "SOLID232" "PLANE233" "SOLID236" "SOLID237" "PLANE238" "SOLID239" "SOLID240" "HSFLD241" "HSFLD242" "COMBI250" "SURF251" "SURF252" "INFIN257" "REINF263" "REINF264" "REINF265" "SOLID272" "SOLID273" "SOLID278" "SOLID279" "CABLE280" "SHELL281" "SOLID285" "PIPE288" "PIPE289" "ELBOW290" "SOLID291" "PLANE292" "PLANE293" "USER300" "BEAM3" "BEAM4" "BEAM23" "BEAM24" "BEAM44" "BEAM54" "CONTAC12" "CONTAC52" "CONTA171" "CONTA173" "CONTA176" "COMBIN7" "FLUID79" "FLUID80" "FLUID81" "FLUID141" "FLUID142" "INFIN9" "INFIN47" "PIPE16" "PIPE18" "PLANE13" "PLANE25" "PLANE42" "PLANE53" "PLANE67" "PLANE82" "PLANE83" "PLANE145" "PLANE146" "CONTAC12" "CONTAC52" "LINK1" "LINK8" "LINK10" "LINK32" "PIPE16" "PIPE17" "PIPE18" "PIPE20" "PIPE59" "PIPE60" "SHELL41" "SHELL43" "SHELL57" "SHELL63" "SHELL91" "SHELL93" "SHELL99" "SHELL150" "SOLID5" "SOLID45" "SOLID46" "SOLID65" "SOLID69" "SOLID92" "SOLID95" "SOLID117" "SOLID127" "SOLID128" "SOLID147" "SOLID148" "SOLID191" "VISCO88" "VISCO89" "VISCO106" "VISCO107" "VISCO108" "TRANS109" "NSEL()" "ESEL()" "KSEL()" "LSEL()" "ASEL()" "VSEL()" "NDNEXT()" "ELNEXT()" "KPNEXT()" "LSNEXT()" "ARNEXT()" "VLNEXT()" "CENTRX()" "CENTRY()" "CENTRZ()" "NX()" "NY()" "NZ()" "KX()" "KY()" "KZ()" "LX()" "LY()" "LZ()" "LSX()" "LSY()" "LSZ()" "NODE()" "KP()" "DISTND()" "DISTKP()" "DISTEN()" "ANGLEN()" "ANGLEK()" "NNEAR()" "KNEAR()" "ENEARN()" "AREAND()" "AREAKP()" "ARNODE()" "NORMNX()" "NORMNY()" "NORMNZ()" "NORMKX()" "NORMKY()" "NORMKZ()" "ENEXTN()" "NELEM()" "NODEDOF()" "ELADJ()" "NDFACE()" "NMFACE()" "ARFACE()" "UX()" "UY()" "UZ()" "ROTX()" "ROTY()" "ROTZ()" "TEMP()" "PRES()" "VX()" "VY()" "VZ()" "ENKE()" "ENDS()" "VOLT()" "MAG()" "AX()" "AY()" "AZ()" "VIRTINQR()" "KWGET()" "VALCHR()" "VALHEX()" "CHRHEX()" "STRFILL()" "STRCOMP()" "STRPOS()" "STRLENG()" "UPCASE()" "LWCASE()" "JOIN()" "SPLIT()" "ABS()" "SIGN()" "CXABS()" "EXP()" "LOG()" "LOG10()" "SQRT()" "NINT()" "MOD()" "RAND()" "GDIS()" "SIN()" "COS()" "TAN()" "SINH()" "COSH()" "TANH()" "ASIN()" "ACOS()" "ATAN()" "ATAN2()" "~CAT5IN" "~CATIAIN" "~PARAIN" "~PROEIN" "~SATIN" "~UGIN" "A" "*ABBR" "AADD" "*AFUN" "*ASK" "AATT" "*AXPY" "ABBRES" "ABBSAV" "ABEXTRACT" "ABS" "ACCAT" "ACCOPTION" "ACEL" "ACLEAR" "ADAMS" "ADD" "ADDAM" "ADELE" "ADGL" "ADRAG" "AEROCOEFF" "AESIZE" "AFILLT" "AFLIST" "AFSURF" "AGEN" "AGLUE" "AINA" "AINP" "AINV" "AL" "ALIST" "ALLSEL" "ALPHAD" "AMAP" "AMBEAM" "AMBUILD" "AMENV" "AMESH" "AMMAT" "AMPOWDER" "AMSUPPORTS" "AMSTEP" "AMTYPE" "/AN3D" "ANCNTR" "ANCUT" "ANCYC" "ANDATA" "ANDSCL" "ANDYNA" "/ANFILE" "ANFLOW" "/ANGLE" "ANHARM" "ANIM" "ANISOS" "ANMODE" "/ANNOT" "ANORM" "ANPRES" "ANSOL" "ANSTOAQWA" "ANSTOASAS" "ANTIME" "ANTYPE" "/ANUM" "AOFFST" "AOVLAP" "APLOT" "APORT" "APPEND" "APTN" "ARCLEN" "ARCTRM" "AREAS" "AREFINE" "AREMESH" "AREVERSE" "AROTAT" "ARSCALE" "ARSYM" "ASBA" "ASBL" "ASBV" "ASBW" "ASCRES" "ASEL" "ASIFILE" "ASKIN" "ASLL" "ASLV" "ASOL" "/ASSIGN" "ASUB" "ASUM" "ATAN" "ATRAN" "ATYPE" "/AUTO" "AUTOTS" "/AUX2" "/AUX3" "/AUX12" "/AUX15" "AVPRIN" "AVRES" "AWAVE" "/AXLAB" "/BATCH" "BCSOPTION" "BETAD" "BF" "BFA" "BFADELE" "BFALIST" "BFCUM" "BFDELE" "BFE" "BFECUM" "BFEDELE" "BFELIST" "BFESCAL" "BFINT" "BFK" "BFKDELE" "BFKLIST" "BFL" "BFLDELE" "BFLIST" "BFLLIST" "BFSCALE" "BFTRAN" "BFUNIF" "BFV" "BFVDELE" "BFVLIST" "BIOOPT" "BIOT" "BLC4" "BLC5" "BLOCK" "BOOL" "BOPTN" "BSAX" "BSMD" "BSM1" "BSM2" "BSPLIN" "BSS1" "BSS2" "BSTE" "BSTQ" "BTOL" "BUCOPT" "C" "CALC" "CAMPBELL" "CBDOF" "CBMD" "CBMX" "CBTE" "CBTMP" "CDOPT" "CDREAD" "CDWRITE" "CE" "CECHECK" "CECMOD" "CECYC" "CEDELE" "CEINTF" "CELIST" "CENTER" "CEQN" "CERIG" "CESGEN" "CFACT" "*CFCLOS" "*CFOPEN" "*CFWRITE" "/CFORMAT" "CGLOC" "CGOMGA" "CGROW" "CHECK" "CHKMSH" "CINT" "CIRCLE" "CISOL" "/CLABEL" "/CLEAR" "CLOCAL" "CLOG" "/CLOG" "CLRMSHLN" "CM" "CMACEL" "/CMAP" "CMATRIX" "CMDELE" "CMDOMEGA" "CMEDIT" "CMGRP" "CMLIST" "CMMOD" "CMOMEGA" "CMPLOT" "CMROTATE" "CMSEL" "CMSFILE" "CMSOPT" "CMWRITE" "CNCHECK" "CNKMOD" "CNTR" "CNVTOL" "/COLOR" "/COM" "*COMP" "COMBINE" "COMPRESS" "CON4" "CONE" "/CONFIG" "CONJUG" "/CONTOUR" "/COPY" "CORIOLIS" "COUPLE" "COVAL" "CP" "CPCYC" "CPDELE" "CPINTF" "/CPLANE" "CPLGEN" "CPLIST" "CPMERGE" "CPNGEN" "CPSGEN" "CQC" "*CREATE" "CS" "CSCIR" "CSDELE" "CSKP" "CSLIST" "CSWPLA" "CSYS" "/CTYPE" "CURR2D" "CUTCONTROL" "/CVAL" "CVAR" "/CWD" "CYCCALC" "/CYCEXPAND" "CYCFILES" "CYCFREQ" "*CYCLE" "CYCLIC" "CYCOPT" "CYCPHASE" "CYCSPEC" "CYL4" "CYL5" "CYLIND" "CZDEL" "CZMESH" "D" "DA" "DADELE" "DALIST" "DAMORPH" "DATA" "DATADEF" "DCGOMG" "DCUM" "DCVSWP" "DDASPEC" "DDELE" "DDOPTION" "DEACT" "DEFINE" "*DEL" "DELETE" "/DELETE" "DELTIM" "DEMORPH" "DERIV" "DESIZE" "DESOL" "DETAB" "/DEVICE" "/DFLAB" "DFLX" "DFSWAVE" "DIG" "DIGIT" "*DIM" "/DIRECTORY" "DISPLAY" "/DIST" "DJ" "DJDELE" "DJLIST" "DK" "DKDELE" "DKLIST" "DL" "DLDELE" "DLIST" "DLLIST" "*DMAT" "DMOVE" "DMPEXT" "DMPOPTION" "DMPRAT" "DMPSTR" "DNSOL" "*DO" "DOF" "DOFSEL" "DOMEGA" "*DOT" "*DOWHILE" "DSCALE" "/DSCALE" "DSET" "DSPOPTION" "DSUM" "DSURF" "DSYM" "DSYS" "DTRAN" "DUMP" "/DV3D" "DVAL" "DVMORPH" "DYNOPT" "E" "EALIVE" "ECPCHG" "EDELE" "/EDGE" "EEXTRUDE" "/EFACET" "EGEN" "EGID" "*EIGEN" "EINFIN" "EINTF" "EKILL" "ELBOW" "ELEM" "ELIST" "*ELSE" "*ELSEIF" "EMAGERR" "EMATWRITE" "EMF" "EMFT" "EMID" "EMIS" "EMODIF" "EMORE" "EMSEL" "EMSYM" "EMTGEN" "EMUNIT" "EN" "*END" "*ENDDO" "*ENDIF" "ENDRELEASE" "ENERSOL" "ENGEN" "ENORM" "ENSYM" "/EOF" "EORIENT" "EPLOT" "EQSLV" "ERASE" "/ERASE" "EREAD" "EREFINE" "EREINF" "ERESX" "ERNORM" "ERRANG" "ESCHECK" "ESEL" "/ESHAPE" "ESIZE" "ESLA" "ESLL" "ESLN" "ESLV" "ESOL" "ESORT" "ESSOLV" "ESTIF" "ESURF" "ESYM" "ESYS" "ET" "ETABLE" "ETCHG" "ETCONTROL" "ETDELE" "ETLIST" "ETYPE" "EUSORT" "EWRITE" "EXBOPT" "*EXIT" "/EXIT" "EXOPTION" "EXP" "EXPAND" "/EXPAND" "EXPASS" "*EXPORT" "EXPROFILE" "EXPSOL" "EXTOPT" "EXTREM" "EXUNIT" "F" "/FACET" "FC" "FCCHECK" "FCDELE" "FCLIST" "/FCOMP" "FCUM" "FCTYP" "FDELE" "/FDELE" "FEBODY" "FECONS" "FEFOR" "FESURF" "*FFT" "FILE" "FILEAUX2" "FILEAUX3" "FILL" "FILLDATA" "/FILNAME" "FINISH" "FITEM" "FJ" "FJDELE" "FJLIST" "FK" "FKDELE" "FKLIST" "FLIST" "FLST" "FLUXV" "FLUREAD" "/FOCUS" "FORCE" "FORM" "/FORMAT" "*FREE" "FREQ" "FRQSCL" "FSCALE" "FSSECT" "FSSPARM" "FSUM" "FTRAN" "FTYPE" "FVMESH" "GAP" "GAPF" "GAUGE" "GCDEF" "GCGEN" "/GCMD" "/GCOLUMN" "GENOPT" "GEOM" "GEOMETRY" "*GET" "/GFILE" "/GFORMAT" "/GLINE" "/GMARKER" "GMATRIX" "GMFACE" "*GO" "/GO" "/GOLIST" "/GOPR" "GP" "GPDELE" "GPLIST" "GPLOT" "/GRAPHICS" "/GRESUME" "/GRID" "/GROPT" "GRP" "/GRTYP" "/GSAVE" "GSBDATA" "GSGDATA" "GSLIST" "GSSOL" "/GST" "GSUM" "/GTHK" "/GTYPE" "HARFRQ" "/HBC" "HBMAT" "/HEADER" "HELP" "HEMIOPT" "HFANG" "HFSYM" "HPTCREATE" "HPTDELETE" "HRCPLX" "HREXP" "HROPT" "HROCEAN" "HROUT" "IC" "ICDELE" "ICLIST" "/ICLWID" "ICROTATE" "/ICSCALE" "*IF" "IGESIN" "IGESOUT" "/IMAGE" "IMAGIN" "IMESH" "IMMED" "INISTATE" "*INIT" "/INPUT" "/INQUIRE" "INRES" "INRTIA" "INT1" "INVOPT" "IOPTN" "IRLF" "IRLIST" "*ITENGINE" "JPEG" "JSOL" "K" "KATT" "KBC" "KBETW" "KCENTER" "KCLEAR" "KDELE" "KDIST" "KEEP" "KESIZE" "KEYOPT" "KEYPTS" "KEYW" "KFILL" "KGEN" "KL" "KLIST" "KMESH" "KMODIF" "KMOVE" "KNODE" "KPLOT" "KPSCALE" "KREFINE" "KSCALE" "KSCON" "KSEL" "KSLL" "KSLN" "KSUM" "KSYMM" "KTRAN" "KUSE" "KWPAVE" "KWPLAN" "L" "L2ANG" "L2TAN" "LANBOPTION" "LANG" "LARC" "/LARC" "LAREA" "LARGE" "LATT" "LAYER" "LAYERP26" "LAYLIST" "LAYPLOT" "LCABS" "LCASE" "LCCALC" "LCCAT" "LCDEF" "LCFACT" "LCFILE" "LCLEAR" "LCOMB" "LCOPER" "LCSEL" "LCSL" "LCSUM" "LCWRITE" "LCZERO" "LDELE" "LDIV" "LDRAG" "LDREAD" "LESIZE" "LEXTND" "LFILLT" "LFSURF" "LGEN" "LGLUE" "LGWRITE" "/LIGHT" "LINA" "LINE" "/LINE" "LINES" "LINL" "LINP" "LINV" "LIST" "*LIST" "LLIST" "LMESH" "LNSRCH" "LOCAL" "LOVLAP" "LPLOT" "LPTN" "LREFINE" "LREVERSE" "LROTAT" "LSBA" "*LSBAC" "LSBL" "LSBV" "LSBW" "LSCLEAR" "LSDELE" "*LSDUMP" "LSEL" "*LSENGINE" "*LSFACTOR" "LSLA" "LSLK" "LSOPER" "/LSPEC" "LSREAD" "*LSRESTORE" "LSSCALE" "LSSOLVE" "LSTR" "LSUM" "LSWRITE" "/LSYMBOL" "LSYMM" "LTAN" "LTRAN" "LUMPM" "LVSCALE" "LWPLAN" "M" "MACOPT" "MAGOPT" "MAGSOLV" "MAP" "/MAP" "MAP2DTO3D" "MAPSOLVE" "MAPVAR" "MASCALE" "MASTER" "MAT" "MATER" "MCHECK" "MDAMP" "MDELE" "MDPLOT" "MEMM" "/MENU" "*MERGE" "MESHING" "*MFOURI" "*MFUN" "MGEN" "MIDTOL" "/MKDIR" "MLIST" "MMASS" "MMF" "MODCONT" "MODDIR" "MODE" "MODIFY" "MODMSH" "MODSELOPTION" "MODOPT" "MONITOR" "*MOPER" "MOPT" "MORPH" "MOVE" "MP" "MPAMOD" "MPCHG" "MPCOPY" "MPDATA" "MPDELE" "MPDRES" "/MPLIB" "MPLIST" "MPPLOT" "MPREAD" "MPRINT" "MPTEMP" "MPTGEN" "MPTRES" "MPWRITE" "/MREP" "MRPM" "MSAVE" "*MSG" "MSHAPE" "MSHCOPY" "MSHKEY" "MSHMID" "MSHPATTERN" "MSOLVE" "/MSTART" "MSTOLE" "*MULT" "*MWRITE" "MXPAND" "N" "NANG" "NAXIS" "NCNV" "NDELE" "NDIST" "NDSURF" "NEQIT" "/NERR" "NFORCE" "NGEN" "NKPT" "NLADAPTIVE" "NLDIAG" "NLDPOST" "NLGEOM" "NLHIST" "NLIST" "NLMESH" "NLOG" "NLOPT" "NMODIF" "NODES" "/NOERASE" "/NOLIST" "NOOFFSET" "/NOPR" "NORA" "NORL" "/NORMAL" "NPLOT" "NPRINT" "NREAD" "NREFINE" "NRLSUM" "*NRM" "NROPT" "NROTAT" "NRRANG" "NSCALE" "NSEL" "NSLA" "NSLE" "NSLK" "NSLL" "NSLV" "NSMOOTH" "NSOL" "NSORT" "NSTORE" "NSUBST" "NSVR" "NSYM" "/NUMBER" "NUMCMP" "NUMEXP" "NUMMRG" "NUMOFF" "NUMSTR" "NUMVAR" "NUSORT" "NWPAVE" "NWPLAN" "NWRITE" "OCDATA" "OCDELETE" "OCLIST" "OCREAD" "OCTABLE" "OCTYPE" "OCZONE" "OMEGA" "OPERATE" "OPNCONTROL" "OUTAERO" "OUTGEOM" "OUTOPT" "OUTPR" "/OUTPUT" "OUTRES" "OVCHECK" "PADELE" "/PAGE" "PAGET" "PAPUT" "PARESU" "PARRES" "PARSAV" "PASAVE" "PATH" "PAUSE" "/PBC" "/PBF" "PCALC" "PCGOPT" "PCIRC" "/PCIRCLE" "PCROSS" "PDEF" "PDOT" "PERBC2D" "PERTURB" "PFACT" "PHYSICS" "PIVCHECK" "PLAS" "PLCAMP" "PLCFREQ" "PLCHIST" "PLCINT" "PLCKSURF" "PLCPLX" "PLDISP" "PLESOL" "PLETAB" "PLFAR" "PLF2D" "PLGEOM" "PLLS" "PLMAP" "PLMC" "PLNEAR" "PLNSOL" "/PLOPTS" "PLORB" "PLOTTING" "PLPAGM" "PLPATH" "PLSECT" "PLTIME" "PLTRAC" "PLVAR" "PLVECT" "PLZZ" "/PMACRO" "PMAP" "PMGTRAN" "PMLOPT" "PMLSIZE" "/PMORE" "PNGR" "/PNUM" "POINT" "POLY" "/POLYGON" "/POST1" "/POST26" "POWERH" "PPATH" "PRANGE" "PRAS" "PRCAMP" "PRCINT" "PRCPLX" "PRED" "PRENERGY" "/PREP7" "PRERR" "PRESOL" "PRETAB" "PRFAR" "PRI2" "PRIM" "PRINT" "*PRINT" "PRISM" "PRITER" "PRJSOL" "PRMC" "PRNEAR" "PRNLD" "PRNSOL" "PROD" "PRORB" "PRPATH" "PRRFOR" "PRRSOL" "PRSCONTROL" "PRSECT" "PRTIME" "PRVAR" "PRVECT" "PSCONTROL" "PSDCOM" "PSDFRQ" "PSDGRAPH" "PSDRES" "PSDSPL" "PSDUNIT" "PSDVAL" "PSDWAV" "/PSEARCH" "PSEL" "/PSF" "PSMAT" "PSMESH" "/PSPEC" "/PSTATUS" "PSTRES" "/PSYMB" "PSYS" "PTR" "PTXY" "PVECT" "/PWEDGE" "QDVAL" "QRDOPT" "QSOPT" "QUAD" "/QUIT" "QUOT" "R" "RACE" "RADOPT" "RAPPND" "RATE" "/RATIO" "RBE3" "RCON" "RCYC" "RDEC" "RDELE" "READ" "REAL" "REALVAR" "RECTNG" "REMESH" "*REMOVE" "*RENAME" "/RENAME" "*REPEAT" "/REPLOT" "RESCOMBINE" "RESCONTROL" "RESET" "/RESET" "RESP" "RESUME" "RESVEC" "RESWRITE" "*RETURN" "REZONE" "RFORCE" "/RGB" "RIGID" "RIGRESP" "RLIST" "RMALIST" "RMANL" "RMASTER" "RMCAP" "RMCLIST" "/RMDIR" "RMFLVEC" "RMLVSCALE" "RMMLIST" "RMMRANGE" "RMMSELECT" "RMNDISP" "RMNEVEC" "RMODIF" "RMORE" "RMPORDER" "RMRESUME" "RMRGENERATE" "RMROPTIONS" "RMRPLOT" "RMRSTATUS" "RMSAVE" "RMSMPLE" "RMUSE" "RMXPORT" "ROCK" "ROSE" "RPOLY" "RPR4" "RPRISM" "RPSD" "RSMESH" "RSOPT" "RSPLIT" "RSTMAC" "RSTOFF" "RSURF" "RSYMM" "RSYS" "SABS" "SADD" "SALLOW" "SAVE" "SBCLIST" "SBCTRAN" "*SCAL" "SCOPT" "SDELETE" "SE" "SECCONTROL" "SECDATA" "SECFUNCTION" "SECJOINT" "/SECLIB" "SECLOCK" "SECMODIF" "SECNUM" "SECOFFSET" "SECPLOT" "SECREAD" "SECSTOP" "SECTYPE" "SECWRITE" "SED" "SEDLIST" "SEEXP" "/SEG" "SELIST" "SELM" "SELTOL" "SEMIIMPLICIT" "SENERGY" "SEOPT" "SESYMM" "*SET" "SET" "SETFGAP" "SETRAN" "SEXP" "SF" "SFA" "SFACT" "SFADELE" "SFALIST" "SFBEAM" "SFCALC" "SFCONTROL" "SFCUM" "SFDELE" "SFE" "SFEDELE" "SFELIST" "SFFUN" "SFGRAD" "SFL" "SFLDELE" "SFLEX" "SFLIST" "SFLLIST" "SFSCALE" "SFTRAN" "/SHADE" "SHELL" "/SHOW" "SHPP" "SHSD" "/SHRINK" "SLIST" "SLOAD" "SMALL" "*SMAT" "SMAX" "/SMBC" "SMBODY" "SMCONS" "SMFOR" "SMIN" "SMOOTH" "SMRTSIZE" "SMSURF" "SMULT" "SNOPTION" "SOLU" "/SOLU" "SOLUOPT" "SOLVE" "*SORT" "SORT" "SOURCE" "SPACE" "SPCNOD" "SPCTEMP" "SPDAMP" "SPEC" "SPFREQ" "SPGRAPH" "SPH4" "SPH5" "SPHERE" "SPLINE" "SPLOT" "SPMWRITE" "SPOINT" "SPOPT" "SPREAD" "SPTOPT" "SPUNIT" "SPVAL" "SQRT" "*SREAD" "SRSS" "SSBT" "/SSCALE" "SSLN" "SSMT" "SSOPT" "SSPA" "SSPB" "SSPD" "SSPE" "SSPM" "SSUM" "SSTATE" "STABILIZE" "STAT" "*STATUS" "/STATUS" "STEF" "/STITLE" "STORE" "SUBOPT" "SUBSET" "SUCALC" "SUCR" "SUDEL" "SUEVAL" "SUGET" "SUMAP" "SUMTYPE" "SUPL" "SUPR" "SURESU" "SUSAVE" "SUSEL" "SUVECT" "SV" "SVPLOT" "SVTYP" "SWADD" "SWDEL" "SWGEN" "SWLIST" "SYNCHRO" "/SYP" "/SYS" "TALLOW" "TARGET" "*TAXIS" "TB" "TBCOPY" "TBDATA" "TBDELE" "TBEO" "TBIN" "TBFIELD" "TBFT" "TBLE" "TBLIST" "TBMODIF" "TBPLOT" "TBPT" "TBTEMP" "TCHG" "/TEE" "THEXPAND" "THOPT" "TIFF" "TIME" "TIMERANGE" "TIMINT" "TIMP" "TINTP" "/TITLE" "/TLABEL" "TOFFST" "*TOPER" "TORUS" "TRANSFER" "*TREAD" "TREF" "/TRIAD" "/TRLCY" "TRNOPT" "TRPDEL" "TRPLIS" "TRPOIN" "TRTIME" "TSHAP" "/TSPEC" "TSRES" "TUNIF" "TVAR" "/TXTRE" "/TYPE" "TYPE" "/UCMD" "/UDOC" "/UI" "UIMP" "/UIS" "*ULIB" "UNDELETE" "UNDO" "/UNITS" "UNPAUSE" "UPCOORD" "UPGEOM" "*USE" "/USER" "USRCAL" "USRDOF" "USRELEM" "V" "V2DOPT" "VA" "*VABS" "VADD" "VARDEL" "VARNAM" "VATT" "VCLEAR" "*VCOL" "/VCONE" "VCROSS" "*VCUM" "VDDAM" "VDELE" "VDGL" "VDOT" "VDRAG" "*VEC" "*VEDIT" "VEORIENT" "VEXT" "*VFACT" "*VFILL" "VFOPT" "VFQUERY" "VFSM" "*VFUN" "VGEN" "*VGET" "VGET" "VGLUE" "/VIEW" "VIMP" "VINP" "VINV" "*VITRP" "*VLEN" "VLIST" "VLSCALE" "*VMASK" "VMESH" "VOFFST" "VOLUMES" "*VOPER" "VOVLAP" "*VPLOT" "VPLOT" "VPTN" "*VPUT" "VPUT" "*VREAD" "VROTAT" "VSBA" "VSBV" "VSBW" "/VSCALE" "*VSCFUN" "VSEL" "VSLA" "*VSTAT" "VSUM" "VSWEEP" "VSYMM" "VTRAN" "VTYPE" "/VUP" "*VWRITE" "/WAIT" "/WINDOW" "WPAVE" "WPCSYS" "WPLANE" "WPOFFS" "WPROTA" "WPSTYL" "WRFULL" "WRITE" "WRITEMAP" "*WRK" "WSPRINGS" "WTBCREATE" "XFCRKMESH" "XFDATA" "XFENRICH" "XFLIST" "/XFRM" "*XPL" "/XRANGE" "XVAR" "/YRANGE" "/ZOOM" "/WB" "XMLO" "/XML" "CNTR" "EBLOCK" "CMBLOCK" "NBLOCK" "/TRACK" "CWZPLOT" "~EUI" "NELE" "EALL" "NALL" "FLITEM" "LSLN" "PSOLVE" "ASLN" "/VERIFY" "/SSS" "~CFIN" "*EVAL" "*MOONEY" "/RUNSTAT" "ALPFILL" "ARCOLLAPSE" "ARDETACH" "ARFILL" "ARMERGE" "ARSPLIT" "CEWRITE" "FIPLOT" "GAPFINISH" "GAPLIST" "GAPMERGE" "GAPOPT" "GAPPLOT" "LNCOLLAPSE" "LNDETACH" "LNFILL" "LNMERGE" "LNSPLIT" "PCONV" "PLCONV" "PEMOPTS" "PEXCLUDE" "PINCLUDE" "PMETH" "/PMETH" "PMOPTS" "PPLOT" "PPRANGE" "PRCONV" "PRECISION" "RALL" "RFILSZ" "RITER" "RMEMRY" "RSPEED" "RSTAT" "RTIMST" "/RUNST" "RWFRNT" "SARPLOT" "SHSD" "SLPPLOT" "SLSPLOT" "VCVFILL" "/OPT" "OPEQN" "OPFACT" "OPFRST" "OPGRAD" "OPKEEP" "OPLOOP" "OPPRNT" "OPRAND" "OPSUBP" "OPSWEEP" "OPTYPE" "OPUSER" "OPVAR" "OPADD" "OPCLR" "OPDEL" "OPMAKE" "OPSEL" "OPANL" "OPDATA" "OPRESU" "OPSAVE" "OPEXE" "OPLFA" "OPLGR" "OPLIST" "OPLSW" "OPRFA" "OPRGR" "OPRSW" "PILECALC" "PILEDISPSET" "PILEGEN" "PILELOAD" "PILEMASS" "PILERUN" "PILESEL" "PILESTIF" "PLVAROPT" "PRVAROPT" "TOCOMP" "TODEF" "TOFREQ" "TOTYPE" "TOVAR" "TOEXE" "TOLOOP" "TOGRAPH" "TOLIST" "TOPLOT" "TOPRINT" "TOSTAT" "TZAMESH" "TZDELE" "TZEGEN" "XVAROPT" "PGSAVE" "SOLCONTROL" "TOTAL" "VTGEOM" "VTREAL" "VTSTAT" "PGRAPH" "/VT" "VTIN" "VTRFIL" "VTTEMP" "PGRSET" "VTCLR" "VTMETH" "VTRSLT" "VTVMOD" "PGSELE" "VTDISC" "VTMP" "VTSEC" "PGWRITE" "VTEVAL" "VTOP" "VTSFE" "POUTRES" "VTFREQ" "VTPOST" "VTSL" "FLDATA1-40" "HFPCSWP" "MSDATA" "MSVARY" "QFACT" "FLOCHECK" "HFPOWER" "MSMASS" "PERI" "SPADP" "FLREAD" "HFPORT" "MSMETH" "PLFSS" "SPARM" "FLOTRAN" "HFSCAT" "MSMIR" "PLSCH" "SPFSS" "HFADP" "ICE" "MSNOMF" "PLSYZ" "SPICE" "HFARRAY" "ICEDELE" "MSPROP" "PLTD" "SPSCAN" "HFDEEM" "ICELIST" "MSQUAD" "PLTLINE" "SPSWP" "HFEIGOPT" "ICVFRC" "MSRELAX" "PLVFRC" "HFEREFINE" "LPRT" "MSSOLU" "/PICE" "HFMODPRT" "MSADV" "MSSPEC" "PLWAVE" "HFPA" "MSCAP" "MSTERM" "PRSYZ")
"APDL symbols for completion in APDL-Mode.
By default APDL keywords, get-functions, parametric-function and elements
- deprecated as well - are completed.")

(defconst apdl-parametric-function-regexp
"\\(?:A\\(?:BS\\|COS\\|SIN\\|TAN2?\\)\\|C\\(?:OSH?\\|XABS\\)\\|EXP\\|GDIS\\|LOG\\(?:10\\)?\\|MOD\\|NINT\\|RAND\\|S\\(?:I\\(?:GN\\|NH?\\)\\|QRT\\)\\|TANH?\\)"
"APDL parametric function regexp.")

(defconst apdl-get-function-regexp
"\\(?:A\\(?:NGLE[KN]\\|R\\(?:EA\\(?:KP\\|ND\\)\\|FACE\\|N\\(?:EXT\\|ODE\\)\\)\\|SEL\\|[XYZ]\\)\\|C\\(?:ENTR[XYZ]\\|HRHEX\\)\\|DIST\\(?:EN\\|KP\\|ND\\)\\|E\\(?:L\\(?:ADJ\\|NEXT\\)\\|N\\(?:DS\\|E\\(?:\\(?:AR\\|XT\\)N\\)\\|KE\\)\\|SEL\\)\\|JOIN\\|K\\(?:NEAR\\|PNEXT\\|SEL\\|WGET\\|[PXYZ]\\)\\|L\\(?:S\\(?:EL\\|NEXT\\|[XYZ]\\)\\|WCASE\\|[XYZ]\\)\\|MAG\\|N\\(?:D\\(?:FACE\\|NEXT\\)\\|ELEM\\|MFACE\\|NEAR\\|O\\(?:DE\\(?:DOF\\)?\\|RM\\(?:K[XYZ]\\|N[XYZ]\\)\\)\\|SEL\\|[XYZ]\\)\\|PRES\\|ROT[XYZ]\\|S\\(?:PLIT\\|TR\\(?:COMP\\|FILL\\|LENG\\|POS\\)\\)\\|TEMP\\|U\\(?:PCASE\\|[XYZ]\\)\\|V\\(?:AL\\(?:CHR\\|HEX\\)\\|IRTINQR\\|LNEXT\\|OLT\\|SEL\\|[XYZ]\\)\\)"
"APDL get function regexp.")

(defconst         apdl-deprecated-element-alist        '(("BEAM3"
.     "BEAM188")     ("BEAM4"      .     "BEAM188")     ("BEAM23"
.     "BEAM188")     ("BEAM24"     .     "BEAM188")     ("BEAM44"
.     "BEAM188")    ("BEAM54"     .    "BEAM188")     ("CONTAC12"
.    "CONTA178")    ("CONTAC52"   .    "CONTA178")    ("CONTA171"
.    "CONTA172")    ("CONTA173"   .    "CONTA174")    ("CONTA176"
.     "CONTA177")    ("COMBIN7"     .    "MPC184")     ("FLUID79"
.     "Fluid29")    ("FLUID80"     .    "Fluid29")     ("FLUID81"
. "Fluid29") ("FLUID141" .  "CFX") ("FLUID142" . "CFX") ("INFIN9"
.    "INFIN110")     ("INFIN47"    .     "INFIN111")    ("PIPE16"
.     "PIPE288")    ("PIPE18"     .    "ELBOW290")     ("PLANE13"
.    "PLANE223")    ("PLANE25"     .    "PLANE272")    ("PLANE42"
.    "PLANE182")    ("PLANE53"     .    "PLANE233")    ("PLANE67"
.    "PLANE223")    ("PLANE82"     .    "PLANE183")    ("PLANE83"
. "SOLID273")  ("PLANE145" . "-") ("PLANE146"  . "-") ("CONTAC12"
.    "CONTA178")     ("CONTAC52"    .     "CONTA178")    ("LINK1"
.     "LINK180")     ("LINK8"      .     "LINK180")     ("LINK10"
.     "LINK180")     ("LINK32"      .     "LINK33")     ("PIPE16"
.     "PIPE288")     ("PIPE17"     .     "PIPE288")     ("PIPE18"
.     "ELBOW290")     ("PIPE20"    .     "PIPE288")     ("PIPE59"
.     "PIPE288")    ("PIPE60"     .    "ELBOW290")     ("SHELL41"
.    "SHELL181")    ("SHELL43"     .    "SHELL181")    ("SHELL57"
.    "SHELL131")    ("SHELL63"     .    "SHELL181")    ("SHELL91"
.    "SHELL281")    ("SHELL93"     .    "SHELL281")    ("SHELL99"
.      "SHELL281")     ("SHELL150"      .     "-")      ("SOLID5"
.    "SOLID226")    ("SOLID45"     .    "SOLID185")    ("SOLID46"
.    "SOLID185")    ("SOLID65"     .    "SOLID185")    ("SOLID69"
.    "SOLID226")    ("SOLID92"     .    "SOLID187")    ("SOLID95"
.    "SOLID186")    ("SOLID117"   .    "SOLID236")    ("SOLID127"
.  "-")  ("SOLID128"  .   "-")  ("SOLID147"  .  "-")  ("SOLID148"
.     "-")      ("SOLID191"     .      "SOLID186")     ("VISCO88"
.    "PLANE183")    ("VISCO89"    .    "SOLID186")    ("VISCO106"
.    "PLANE182")    ("VISCO107"   .    "SOLID185")    ("VISCO108"
. "PLANE183") ("TRANS109" . "PLANE223"))
"Association list for APDL deprecated elements.
Together with their proposed replacements.")

(defconst apdl-deprecated-element-regexp
"\\<\\(BEAM\\(?:2[34]\\|[45]4\\|[34]\\)\\|CO\\(?:MBIN7\\|NTA\\(?:17[136]\\|C\\(?:[15]2\\)\\)\\)\\|FLUID\\(?:14[12]\\|79\\|8[01]\\)\\|INFIN\\(?:47\\|9\\)\\|LINK\\(?:10\\|32\\|[18]\\)\\|P\\(?:IPE\\(?:1[678]\\|20\\|59\\|60\\)\\|LANE\\(?:1\\(?:3\\|4[56]\\)\\|25\\|42\\|53\\|67\\|8[23]\\)\\)\\|S\\(?:HELL\\(?:150\\|4[13]\\|57\\|63\\|9[139]\\)\\|OLID\\(?:1\\(?:17\\|2[78]\\|4[78]\\|91\\)\\|4[56]\\|5\\|6[59]\\|9[25]\\)\\)\\|TRANS109\\|VISCO\\(?:10[678]\\|8[89]\\)\\)\\>"
"APDL deprecated elements regexp.")

(defconst apdl-element-regexp
"\\<\\(BEAM18[89]\\|C\\(?:ABLE280\\|IRCU\\(?:12[45]\\|94\\)\\|O\\(?:MBI\\(?:2\\(?:14\\|50\\)\\|N\\(?:14\\|3[79]\\|40\\)\\)\\|NTA17[24578]\\)\\|PT21[23567]\\)\\|ELBOW290\\|F\\(?:LUID\\(?:1\\(?:16\\|29\\|3[0689]\\)\\|2\\(?:18\\|2[01]\\|9\\)\\|3[08]\\)\\|OLLW201\\)\\|HSFLD24[12]\\|IN\\(?:FIN\\(?:11[01]\\|\\(?:25\\|4\\)7\\)\\|TER\\(?:19[2-5]\\|20[2-5]\\)\\)\\|LINK\\(?:1\\(?:1\\|80\\)\\|3[134]\\|68\\)\\|M\\(?:A\\(?:SS\\(?:[27]1\\)\\|TRIX\\(?:27\\|50\\)\\)\\|ESH200\\|PC184\\)\\|P\\(?:IPE28[89]\\|LANE\\(?:1\\(?:21\\|3\\|8[23]\\)\\|2\\(?:2[23]\\|3[038]\\|5\\|9[23]\\)\\|35\\|55\\|7[578]\\|83\\)\\|RETS179\\)\\|R\\(?:EINF26[345]\\|OM144\\)\\|S\\(?:HELL\\(?:1\\(?:3[12]\\|57\\|81\\)\\|2\\(?:0[89]\\|81\\)\\|61\\)\\|O\\(?:L\\(?:ID\\(?:1\\(?:2[23]\\|8[567]\\)\\|2\\(?:2[67]\\|3[12679]\\|40\\|7[2389]\\|85\\|91\\)\\|5\\|70\\|87\\|9[068]\\)\\|SH190\\)\\|URC36\\)\\|URF\\(?:15[1-69]\\|25[12]\\)\\)\\|T\\(?:ARGE1\\(?:69\\|70\\)\\|RANS126\\)\\|USER300\\)\\>"
"APDL elements regexp.")

(defconst apdl-command-regexp-2a
"\\(?:\\*\\(?:DO\\|E\\(?:LSEIF\\|ND\\(?:DO\\|IF\\)?\\)\\|GO\\|IF\\)\\|/\\(?:GO\\|P\\(?:OST\\(?:1\\|26\\)\\|REP7\\)\\|UI\\)\\|A\\(?:BS\\|DD\\|L\\)\\|BF[AEKLV]?\\|C\\(?:QC\\|[EMPS]\\)\\|D\\(?:IG\\|OF\\|[AJKL]\\)\\|E\\(?:MF\\|XP\\|[NT]\\)\\|F[CJK]\\|G\\(?:[AR]?P\\)\\|IC\\|K\\(?:BC\\|L\\)\\|M\\(?:A[PT]\\|MF\\|P\\)\\|PTR\\|S\\(?:E[DT]\\|F[AEL]\\|[EFV]\\)\\|TB\\|VA\\|[AC-FK-NRV]\\)"
"APDL keyword name regexp 2a.")

(defconst apdl-command-regexp-2b
"\\(?:\\*\\(?:M\\(?:ER\\(?:GE?\\)?\\|F\\(?:OU\\(?:RI?\\)?\\|UN\\|[OU]\\)\\|OP\\(?:ER?\\)?\\|SG\\|ULT?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|NRM\\|PRI\\(?:NT?\\)?\\|RE\\(?:MO\\(?:VE?\\)?\\|NA\\(?:ME?\\)?\\|PE\\(?:AT?\\)?\\|TU\\(?:RN?\\)?\\|[MNPT]\\)\\|S\\(?:CAL?\\|ET\\|MAT?\\|ORT?\\|RE\\(?:AD?\\)?\\|TA\\(?:T\\(?:US?\\)?\\)?\\)\\|T\\(?:AX\\(?:IS?\\)?\\|OP\\(?:ER?\\)?\\|RE\\(?:AD?\\)?\\)\\|U\\(?:LIB?\\|SE\\)\\|V\\(?:ABS?\\|C\\(?:OL\\|UM\\|[OU]\\)\\|E\\(?:DIT?\\|[CD]\\)\\|F\\(?:ACT?\\|ILL?\\|UN\\|[AIU]\\)\\|GET?\\|IT\\(?:RP?\\)?\\|LEN?\\|MA\\(?:SK?\\)?\\|OP\\(?:ER?\\)?\\|P\\(?:LOT?\\|UT\\|[LU]\\)\\|RE\\(?:AD?\\)?\\|S\\(?:CF\\(?:UN?\\)?\\|TAT?\\|[CT]\\)\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|WRK\\|XPL\\)\\|/\\(?:M\\(?:ENU?\\|KD\\(?:IR?\\)?\\|PL\\(?:IB?\\)?\\|REP?\\|ST\\(?:A\\(?:RT?\\)?\\)?\\)\\|N\\(?:ERR?\\|O\\(?:ER\\(?:A\\(?:SE?\\)?\\)?\\|LI\\(?:ST?\\)?\\|PR\\|RM\\(?:AL?\\)?\\|[ELPR]\\)\\|UM\\(?:B\\(?:ER?\\)?\\)?\\)\\|OUT\\(?:P\\(?:UT?\\)?\\)?\\|P\\(?:AGE?\\|B[CF]\\|CI\\(?:R\\(?:C\\(?:LE?\\)?\\)?\\)?\\|LO\\(?:P\\(?:TS?\\)?\\)?\\|M\\(?:AC\\(?:RO?\\)?\\|ORE?\\|[AO]\\)\\|NUM?\\|OL\\(?:Y\\(?:G\\(?:ON?\\)?\\)?\\)?\\|S\\(?:EA\\(?:R\\(?:CH?\\)?\\)?\\|PEC?\\|TA\\(?:T\\(?:US?\\)?\\)?\\|YMB?\\|[EFPTY]\\)\\|WE\\(?:D\\(?:GE?\\)?\\)?\\)\\|QUIT?\\|R\\(?:AT\\(?:IO?\\)?\\|E\\(?:NA\\(?:ME?\\)?\\|PL\\(?:OT?\\)?\\|SET?\\|[NPS]\\)\\|GB\\|MD\\(?:IR?\\)?\\)\\|S\\(?:E\\(?:CL\\(?:IB?\\)?\\|[CG]\\)\\|H\\(?:ADE?\\|OW\\|RI\\(?:NK?\\)?\\|[AOR]\\)\\|MBC?\\|OLU?\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|T\\(?:AT\\(?:US?\\)?\\|IT\\(?:LE?\\)?\\|[AI]\\)\\|Y[PS]\\)\\|T\\(?:EE\\|IT\\(?:LE?\\)?\\|LA\\(?:B\\(?:EL?\\)?\\)?\\|R\\(?:IAD?\\|LCY?\\|[IL]\\)\\|SP\\(?:EC?\\)?\\|XT\\(?:RE?\\)?\\|YPE?\\)\\|U\\(?:CMD?\\|DOC?\\|IS\\|NI\\(?:TS?\\)?\\|SER?\\)\\|V\\(?:CO\\(?:NE?\\)?\\|IEW?\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|UP\\)\\|W\\(?:AIT?\\|IN\\(?:D\\(?:OW?\\)?\\)?\\)\\|X\\(?:FRM?\\|RA\\(?:N\\(?:GE?\\)?\\)?\\)\\|YRA\\(?:N\\(?:GE?\\)?\\)?\\|ZOOM?\\)\\|M\\(?:D\\(?:AMP?\\|ELE?\\|PL\\(?:OT?\\)?\\)\\|E\\(?:MM\\|SH\\(?:I\\(?:NG?\\)?\\)?\\)\\|GEN\\|IDT\\(?:OL?\\)?\\|LIST?\\|MASS?\\|O\\(?:D\\(?:CO\\(?:NT?\\)?\\|DIR?\\|IFY?\\|MSH?\\|OPT?\\|SE\\(?:L\\(?:O\\(?:P\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)?\\)?\\)?\\|[CDEIMOS]\\)\\|NI\\(?:T\\(?:OR?\\)?\\)?\\|PT\\|RPH?\\|VE\\)\\|P\\(?:AM\\(?:OD?\\)?\\|C\\(?:HG\\|OPY?\\|[HO]\\)\\|D\\(?:ATA?\\|ELE?\\|RES?\\|[AER]\\)\\|LI\\(?:ST?\\)?\\|PL\\(?:OT?\\)?\\|R\\(?:EAD?\\|INT?\\|[EI]\\)\\|T\\(?:EMP?\\|GEN?\\|RES?\\|[EGR]\\)\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|RPM\\|S\\(?:AVE?\\|H\\(?:APE?\\|CO\\(?:PY?\\)?\\|KEY?\\|MID?\\|PA\\(?:T\\(?:T\\(?:E\\(?:RN?\\)?\\)?\\)?\\)?\\|[ACKMP]\\)\\|OL\\(?:VE?\\)?\\|TO\\(?:LE?\\)?\\)\\|XPA\\(?:ND?\\)?\\)\\|N\\(?:A\\(?:NG\\|XIS?\\)\\|CNV\\|D\\(?:ELE?\\|IST?\\|SU\\(?:RF?\\)?\\)\\|EQIT?\\|FOR\\(?:CE?\\)?\\|GEN\\|KPT\\|L\\(?:AD\\(?:A\\(?:P\\(?:T\\(?:I\\(?:VE?\\)?\\)?\\)?\\)?\\)?\\|D\\(?:IAG?\\|PO\\(?:ST?\\)?\\|[IP]\\)\\|GE\\(?:OM?\\)?\\|HI\\(?:ST?\\)?\\|IST?\\|ME\\(?:SH?\\)?\\|O\\(?:PT\\|[GP]\\)\\)\\|MOD\\(?:IF?\\)?\\|O\\(?:DES?\\|OF\\(?:F\\(?:S\\(?:ET?\\)?\\)?\\)?\\|R[AL]\\)\\|P\\(?:LOT?\\|RI\\(?:NT?\\)?\\)\\|R\\(?:E\\(?:AD\\|FI\\(?:NE?\\)?\\|[AF]\\)\\|LS\\(?:UM?\\)?\\|O\\(?:PT\\|TAT?\\|[PT]\\)\\|RA\\(?:NG?\\)?\\)\\|S\\(?:CA\\(?:LE?\\)?\\|EL\\|L[AEKLV]\\|MO\\(?:O\\(?:TH?\\)?\\)?\\|O\\(?:RT\\|[LR]\\)\\|TO\\(?:RE?\\)?\\|UB\\(?:ST?\\)?\\|VR\\|YM\\)\\|U\\(?:M\\(?:CMP?\\|EXP?\\|MRG?\\|OFF?\\|STR?\\|VAR?\\|[CEMOSV]\\)\\|SO\\(?:RT?\\)?\\)\\|W\\(?:P\\(?:AVE?\\|LAN?\\|[AL]\\)\\|RI\\(?:TE?\\)?\\)\\)\\|O\\(?:C\\(?:D\\(?:ATA?\\|EL\\(?:E\\(?:TE?\\)?\\)?\\|[AE]\\)\\|LI\\(?:ST?\\)?\\|RE\\(?:AD?\\)?\\|T\\(?:AB\\(?:LE?\\)?\\|YPE?\\|[AY]\\)\\|ZO\\(?:NE?\\)?\\)\\|MEGA?\\|P\\(?:ER\\(?:A\\(?:TE?\\)?\\)?\\|NC\\(?:O\\(?:N\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\)?\\)?\\)\\|UT\\(?:AE\\(?:RO?\\)?\\|GE\\(?:OM?\\)?\\|OPT?\\|PR\\|RES?\\|[AGOPR]\\)\\|VCH\\(?:E\\(?:CK?\\)?\\)?\\)\\|P\\(?:A\\(?:DE\\(?:LE?\\)?\\|GET?\\|PUT?\\|R\\(?:ESU?\\|RES?\\|SAV?\\|[ERS]\\)\\|SA\\(?:VE?\\)?\\|TH\\|USE?\\)\\|C\\(?:ALC?\\|GO\\(?:PT?\\)?\\|IRC?\\|RO\\(?:SS?\\)?\\)\\|D\\(?:EF\\|OT\\)\\|ER\\(?:BC\\(?:2D?\\)?\\|TU\\(?:RB?\\)?\\|[BT]\\)\\|FACT?\\|HYS\\(?:I\\(?:CS?\\)?\\)?\\|IVC\\(?:H\\(?:E\\(?:CK?\\)?\\)?\\)?\\|L\\(?:AS\\|C\\(?:AMP?\\|FR\\(?:EQ?\\)?\\|HI\\(?:ST?\\)?\\|INT?\\|KS\\(?:U\\(?:RF?\\)?\\)?\\|PLX?\\|[AFHIKP]\\)\\|DI\\(?:SP?\\)?\\|E\\(?:SOL?\\|TAB?\\|[ST]\\)\\|F\\(?:2D\\|AR\\|[2A]\\)\\|GE\\(?:OM?\\)?\\|LS\\|M\\(?:AP\\|[AC]\\)\\|N\\(?:EAR?\\|SOL?\\|[ES]\\)\\|O\\(?:RB\\|TT\\(?:I\\(?:NG?\\)?\\)?\\|[RT]\\)\\|PA\\(?:GM\\|TH\\|[GT]\\)\\|SE\\(?:CT?\\)?\\|T\\(?:IME?\\|RAC?\\|[IR]\\)\\|V\\(?:AR\\|ECT?\\|[AE]\\)\\|ZZ\\)\\|M\\(?:AP\\|GT\\(?:R\\(?:AN?\\)?\\)?\\|L\\(?:OPT?\\|SI\\(?:ZE?\\)?\\|[OS]\\)\\)\\|NGR\\|O\\(?:INT?\\|LY\\|WE\\(?:RH?\\)?\\)\\|PATH?\\|R\\(?:A\\(?:NGE?\\|[NS]\\)\\|C\\(?:AMP?\\|INT?\\|PLX?\\|[AIP]\\)\\|E\\(?:NE\\(?:R\\(?:GY?\\)?\\)?\\|RR\\|SOL?\\|TAB?\\|[DNRST]\\)\\|FAR?\\|I\\(?:NT\\|SM\\|TER?\\|[2MNST]\\)\\|JS\\(?:OL?\\)?\\|MC\\|N\\(?:EAR?\\|LD\\|SOL?\\|[ELS]\\)\\|O\\(?:RB\\|[DR]\\)\\|PA\\(?:TH?\\)?\\|R\\(?:FOR?\\|SOL?\\|[FS]\\)\\|S\\(?:CO\\(?:N\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\)?\\|ECT?\\|[CE]\\)\\|TI\\(?:ME?\\)?\\|V\\(?:AR\\|ECT?\\|[AE]\\)\\)\\|S\\(?:CO\\(?:N\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\)?\\|D\\(?:COM?\\|FRQ?\\|GR\\(?:A\\(?:PH?\\)?\\)?\\|RES?\\|SPL?\\|UN\\(?:IT?\\)?\\|VAL?\\|WAV?\\|[CFGRSUVW]\\)\\|EL\\|M\\(?:AT\\|ESH?\\|[AE]\\)\\|TR\\(?:ES?\\)?\\|YS\\)\\|TXY\\|VECT?\\)\\|Q\\(?:DVAL?\\|RDO\\(?:PT?\\)?\\|SOPT?\\|U\\(?:AD\\|OT\\)\\)\\|R\\(?:A\\(?:CE\\|DO\\(?:PT?\\)?\\|PP\\(?:ND?\\)?\\|TE\\)\\|BE3\\|C\\(?:ON\\|YC\\)\\|DE\\(?:LE\\|[CL]\\)\\|E\\(?:A\\(?:LV\\(?:AR?\\)?\\|[DL]\\)\\|CT\\(?:NG?\\)?\\|ME\\(?:SH?\\)?\\|S\\(?:CO\\(?:MB\\(?:I\\(?:NE?\\)?\\)?\\|NT\\(?:R\\(?:OL?\\)?\\)?\\|[MN]\\)\\|ET\\|UME?\\|VEC?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\|[EPUVW]\\)\\|ZO\\(?:NE?\\)?\\)\\|FOR\\(?:CE?\\)?\\|IG\\(?:ID\\|RE\\(?:SP?\\)?\\|[IR]\\)\\|LIST?\\|M\\(?:A\\(?:LI\\(?:ST?\\)?\\|NL\\|ST\\(?:ER?\\)?\\|[LNS]\\)\\|C\\(?:AP\\|LI\\(?:ST?\\)?\\|[AL]\\)\\|FL\\(?:V\\(?:EC?\\)?\\)?\\|LV\\(?:S\\(?:C\\(?:A\\(?:LE?\\)?\\)?\\)?\\)?\\|M\\(?:LI\\(?:ST?\\)?\\|RA\\(?:N\\(?:GE?\\)?\\)?\\|SE\\(?:L\\(?:E\\(?:CT?\\)?\\)?\\)?\\|[LRS]\\)\\|N\\(?:DI\\(?:SP?\\)?\\|EV\\(?:EC?\\)?\\|[DE]\\)\\|O\\(?:DIF?\\|RE\\|[DR]\\)\\|PO\\(?:R\\(?:D\\(?:ER?\\)?\\)?\\)?\\|R\\(?:ES\\(?:U\\(?:ME?\\)?\\)?\\|GE\\(?:N\\(?:E\\(?:R\\(?:A\\(?:TE?\\)?\\)?\\)?\\)?\\)?\\|OP\\(?:T\\(?:I\\(?:O\\(?:NS?\\)?\\)?\\)?\\)?\\|PL\\(?:OT?\\)?\\|ST\\(?:A\\(?:T\\(?:US?\\)?\\)?\\)?\\|[EGOPS]\\)\\|S\\(?:AVE?\\|MP\\(?:LE?\\)?\\|[AM]\\)\\|USE?\\|XP\\(?:O\\(?:RT?\\)?\\)?\\)\\|O\\(?:CK\\|SE\\)\\|P\\(?:OLY?\\|R\\(?:ISM?\\|[4I]\\)\\|SD\\)\\|S\\(?:ME\\(?:SH?\\)?\\|OPT?\\|PL\\(?:IT?\\)?\\|T\\(?:MAC?\\|OFF?\\|[MO]\\)\\|URF?\\|Y\\(?:MM\\|[MS]\\)\\)\\)\\|S\\(?:A\\(?:BS\\|DD\\|LL\\(?:OW?\\)?\\|VE\\)\\|BC\\(?:LI\\(?:ST?\\)?\\|TR\\(?:AN?\\)?\\|[LT]\\)\\|COPT?\\|DEL\\(?:E\\(?:TE?\\)?\\)?\\|E\\(?:C\\(?:CO\\(?:N\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\)?\\|DA\\(?:TA?\\)?\\|FU\\(?:N\\(?:C\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)?\\)?\\|JO\\(?:I\\(?:NT?\\)?\\)?\\|LO\\(?:CK?\\)?\\|MO\\(?:D\\(?:IF?\\)?\\)?\\|NUM?\\|OF\\(?:F\\(?:S\\(?:ET?\\)?\\)?\\)?\\|PL\\(?:OT?\\)?\\|RE\\(?:AD?\\)?\\|ST\\(?:OP?\\)?\\|TY\\(?:PE?\\)?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\|[CDFJL-PRSTW]\\)\\|DL\\(?:I\\(?:ST?\\)?\\)?\\|EXP?\\|L\\(?:IST?\\|TOL?\\|[IMT]\\)\\|MI\\(?:I\\(?:M\\(?:P\\(?:L\\(?:I\\(?:C\\(?:IT?\\)?\\)?\\)?\\)?\\)?\\)?\\)?\\|NE\\(?:R\\(?:GY?\\)?\\)?\\|OPT?\\|SY\\(?:MM?\\)?\\|T\\(?:FG\\(?:AP?\\)?\\|RAN?\\|[FR]\\)\\|XP\\)\\|F\\(?:A\\(?:CT\\|DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|[CDL]\\)\\|BE\\(?:AM?\\)?\\|C\\(?:ALC?\\|ON\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\|UM\\|[AOU]\\)\\|DE\\(?:LE?\\)?\\|E\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|[DL]\\)\\|FUN?\\|GR\\(?:AD?\\)?\\|L\\(?:DE\\(?:LE?\\)?\\|EX\\|IST?\\|LI\\(?:ST?\\)?\\|[DEIL]\\)\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|TR\\(?:AN?\\)?\\)\\|H\\(?:ELL?\\|PP\\|SD\\)\\|L\\(?:IST?\\|OAD?\\)\\|M\\(?:A\\(?:LL\\|[LX]\\)\\|BO\\(?:DY?\\)?\\|CO\\(?:NS?\\)?\\|FOR?\\|IN\\|OO\\(?:TH?\\)?\\|RT\\(?:S\\(?:I\\(?:ZE?\\)?\\)?\\)?\\|SU\\(?:RF?\\)?\\|ULT?\\)\\|NOP\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\|O\\(?:L\\(?:UO\\(?:PT?\\)?\\|VE\\|[UV]\\)\\|RT\\|UR\\(?:CE?\\)?\\)\\|P\\(?:ACE?\\|C\\(?:NOD?\\|TE\\(?:MP?\\)?\\|[NT]\\)\\|DA\\(?:MP?\\)?\\|EC\\|FR\\(?:EQ?\\)?\\|GR\\(?:A\\(?:PH?\\)?\\)?\\|H\\(?:ERE?\\|[45E]\\)\\|L\\(?:INE?\\|OT\\|[IO]\\)\\|MW\\(?:R\\(?:I\\(?:TE?\\)?\\)?\\)?\\|O\\(?:INT?\\|PT\\|[IP]\\)\\|RE\\(?:AD?\\)?\\|TO\\(?:PT?\\)?\\|UN\\(?:IT?\\)?\\|VAL?\\)\\|QRT\\|RSS\\|S\\(?:BT\\|LN\\|MT\\|OPT?\\|P[ABDEM]\\|TA\\(?:TE?\\)?\\|UM\\)\\|T\\(?:A\\(?:BI\\(?:L\\(?:I\\(?:ZE?\\)?\\)?\\)?\\|[BT]\\)\\|EF\\|ORE?\\)\\|U\\(?:B\\(?:OPT?\\|SET?\\|[OS]\\)\\|C\\(?:ALC?\\|[AR]\\)\\|DEL?\\|EV\\(?:AL?\\)?\\|GET?\\|M\\(?:AP\\|TY\\(?:PE?\\)?\\|[AT]\\)\\|P[LR]\\|RE\\(?:SU?\\)?\\|S\\(?:AVE?\\|EL\\|[AE]\\)\\|VE\\(?:CT?\\)?\\)\\|V\\(?:PL\\(?:OT?\\)?\\|TYP?\\)\\|W\\(?:ADD?\\|DEL?\\|GEN?\\|LI\\(?:ST?\\)?\\)\\|YNC\\(?:H\\(?:RO?\\)?\\)?\\)\\|T\\(?:A\\(?:LL\\(?:OW?\\)?\\|RG\\(?:ET?\\)?\\)\\|B\\(?:CO\\(?:PY?\\)?\\|D\\(?:ATA?\\|ELE?\\|[AE]\\)\\|EO\\|F\\(?:IE\\(?:LD?\\)?\\|[IT]\\)\\|IN\\|L\\(?:IST?\\|[EI]\\)\\|MO\\(?:D\\(?:IF?\\)?\\)?\\|P\\(?:LOT?\\|[LT]\\)\\|TE\\(?:MP?\\)?\\)\\|CHG\\|H\\(?:EX\\(?:P\\(?:A\\(?:ND?\\)?\\)?\\)?\\|OPT?\\)\\|I\\(?:FF\\|M\\(?:ER\\(?:A\\(?:N\\(?:GE?\\)?\\)?\\)?\\|INT?\\|[EIP]\\)\\|NTP?\\)\\|O\\(?:FF\\(?:ST?\\)?\\|RUS?\\)\\|R\\(?:AN\\(?:S\\(?:F\\(?:ER?\\)?\\)?\\)?\\|EF\\|NO\\(?:PT?\\)?\\|P\\(?:DEL?\\|LIS?\\|OIN?\\|[DLO]\\)\\|TI\\(?:ME?\\)?\\)\\|S\\(?:HAP?\\|RES?\\)\\|UNIF?\\|VAR\\|YPE\\)\\|U\\(?:IMP\\|N\\(?:D\\(?:EL\\(?:E\\(?:TE?\\)?\\)?\\|[EO]\\)\\|PA\\(?:U\\(?:SE?\\)?\\)?\\)\\|P\\(?:CO\\(?:O\\(?:RD?\\)?\\)?\\|GE\\(?:OM?\\)?\\)\\|SR\\(?:CAL?\\|DOF?\\|EL\\(?:EM?\\)?\\|[CDE]\\)\\)\\|V\\(?:2DO\\(?:PT?\\)?\\|A\\(?:DD\\|R\\(?:DEL?\\|NAM?\\|[DN]\\)\\|TT\\)\\|C\\(?:LE\\(?:AR?\\)?\\|RO\\(?:SS?\\)?\\)\\|D\\(?:DAM?\\|ELE?\\|GL\\|OT\\|RAG?\\)\\|E\\(?:OR\\(?:I\\(?:E\\(?:NT?\\)?\\)?\\)?\\|XT\\)\\|F\\(?:OPT?\\|QU\\(?:E\\(?:RY?\\)?\\)?\\|SM\\)\\|G\\(?:E[NT]\\|LUE?\\)\\|I\\(?:MP\\|N[PV]\\)\\|L\\(?:IST?\\|SC\\(?:A\\(?:LE?\\)?\\)?\\)\\|MESH?\\|O\\(?:FF\\(?:ST?\\)?\\|LU\\(?:M\\(?:ES?\\)?\\)?\\|VL\\(?:AP?\\)?\\)\\|P\\(?:LOT?\\|TN\\|UT\\)\\|ROT\\(?:AT?\\)?\\|S\\(?:B[AVW]\\|EL\\|LA\\|UM\\|WE\\(?:EP?\\)?\\|YMM?\\)\\|T\\(?:RAN?\\|YPE?\\)\\)\\|W\\(?:P\\(?:AVE?\\|CS\\(?:YS?\\)?\\|LA\\(?:NE?\\)?\\|OF\\(?:FS?\\)?\\|RO\\(?:TA?\\)?\\|ST\\(?:YL?\\)?\\)\\|R\\(?:FU\\(?:LL?\\)?\\|ITE\\(?:M\\(?:AP?\\)?\\)?\\)\\|SPR\\(?:I\\(?:N\\(?:GS?\\)?\\)?\\)?\\|TBC\\(?:R\\(?:E\\(?:A\\(?:TE?\\)?\\)?\\)?\\)?\\)\\|X\\(?:F\\(?:CR\\(?:K\\(?:M\\(?:E\\(?:SH?\\)?\\)?\\)?\\)?\\|DA\\(?:TA?\\)?\\|EN\\(?:R\\(?:I\\(?:CH?\\)?\\)?\\)?\\|LI\\(?:ST?\\)?\\)\\|VAR\\)\\)"
"APDL keyword name regexp 2b.")

(defconst apdl-command-regexp-2c
"\\(?:\\*\\(?:A\\(?:BBR?\\|FUN?\\|SK\\|XPY?\\)\\|C\\(?:F\\(?:CL\\(?:OS?\\)?\\|OP\\(?:EN?\\)?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\|[COW]\\)\\|OMP?\\|RE\\(?:A\\(?:TE?\\)?\\)?\\|YC\\(?:LE?\\)?\\)\\|D\\(?:EL\\|IM\\|MAT?\\|O\\(?:WH\\(?:I\\(?:LE?\\)?\\)?\\|[TW]\\)\\)\\|E\\(?:IG\\(?:EN?\\)?\\|LSE\\|X\\(?:IT\\|PO\\(?:RT?\\)?\\|[IP]\\)\\)\\|F\\(?:FT\\|REE?\\)\\|GET\\|I\\(?:NIT?\\|TE\\(?:N\\(?:G\\(?:I\\(?:NE?\\)?\\)?\\)?\\)?\\)\\|L\\(?:IST?\\|S\\(?:BAC?\\|DU\\(?:MP?\\)?\\|EN\\(?:G\\(?:I\\(?:NE?\\)?\\)?\\)?\\|FA\\(?:C\\(?:T\\(?:OR?\\)?\\)?\\)?\\|RE\\(?:S\\(?:T\\(?:O\\(?:RE?\\)?\\)?\\)?\\)?\\|[BDEFR]\\)\\)\\)\\|/\\(?:A\\(?:N\\(?:3D\\|FI\\(?:LE?\\)?\\|GLE?\\|NOT?\\|UM\\|[3FGNU]\\)\\|SS\\(?:I\\(?:GN?\\)?\\)?\\|U\\(?:TO?\\|X\\(?:1[25]\\|[23]\\)\\)\\|XL\\(?:AB?\\)?\\)\\|BAT\\(?:CH?\\)?\\|C\\(?:FO\\(?:R\\(?:M\\(?:AT?\\)?\\)?\\)?\\|L\\(?:AB\\(?:EL?\\)?\\|EAR?\\|OG\\|[AEO]\\)\\|MAP?\\|O\\(?:LOR?\\|N\\(?:FIG?\\|TO\\(?:UR?\\)?\\|[FT]\\)\\|PY\\|[LMP]\\)\\|PL\\(?:A\\(?:NE?\\)?\\)?\\|TY\\(?:PE?\\)?\\|VAL?\\|WD\\|YC\\(?:E\\(?:X\\(?:P\\(?:A\\(?:ND?\\)?\\)?\\)?\\)?\\)?\\)\\|D\\(?:E\\(?:LE\\(?:TE?\\)?\\|VI\\(?:CE?\\)?\\|[LV]\\)\\|FL\\(?:AB?\\)?\\|I\\(?:RE\\(?:C\\(?:T\\(?:O\\(?:RY?\\)?\\)?\\)?\\)?\\|ST\\|[RS]\\)\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|V3D?\\)\\|E\\(?:DGE?\\|FA\\(?:C\\(?:ET?\\)?\\)?\\|OF\\|RA\\(?:SE?\\)?\\|SH\\(?:A\\(?:PE?\\)?\\)?\\|X\\(?:IT\\|PA\\(?:ND?\\)?\\|[IP]\\)\\)\\|F\\(?:AC\\(?:ET?\\)?\\|CO\\(?:MP?\\)?\\|DE\\(?:LE?\\)?\\|IL\\(?:N\\(?:A\\(?:ME?\\)?\\)?\\)?\\|O\\(?:CUS?\\|RM\\(?:AT?\\)?\\|[CR]\\)\\)\\|G\\(?:C\\(?:MD\\|OL\\(?:U\\(?:MN?\\)?\\)?\\|[MO]\\)\\|F\\(?:ILE?\\|OR\\(?:M\\(?:AT?\\)?\\)?\\|[IO]\\)\\|LI\\(?:NE?\\)?\\|MA\\(?:R\\(?:K\\(?:ER?\\)?\\)?\\)?\\|O\\(?:LI\\(?:ST?\\)?\\|PR\\|[LP]\\)\\|R\\(?:AP\\(?:H\\(?:I\\(?:CS?\\)?\\)?\\)?\\|ES\\(?:U\\(?:ME?\\)?\\)?\\|ID\\|OPT?\\|TYP?\\|[AEIOT]\\)\\|S\\(?:AVE?\\|[AT]\\)\\|T\\(?:HK\\|YPE?\\|[HY]\\)\\)\\|H\\(?:BC\\|EA\\(?:D\\(?:ER?\\)?\\)?\\)\\|I\\(?:C\\(?:LW\\(?:ID?\\)?\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|[LS]\\)\\|MA\\(?:GE?\\)?\\|N\\(?:PUT?\\|QU\\(?:I\\(?:RE?\\)?\\)?\\|[PQ]\\)\\)\\|L\\(?:ARC?\\|I\\(?:GHT?\\|NE\\|[GN]\\)\\|S\\(?:PEC?\\|YM\\(?:B\\(?:OL?\\)?\\)?\\|[PY]\\)\\)\\|MAP\\)\\|A\\(?:A\\(?:DD\\|TT\\)\\|B\\(?:B\\(?:RES?\\|SAV?\\|[RS]\\)\\|EX\\(?:T\\(?:R\\(?:A\\(?:CT?\\)?\\)?\\)?\\)?\\)\\|C\\(?:C\\(?:AT\\|OP\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\|[AO]\\)\\|EL\\|LE\\(?:AR?\\)?\\)\\|D\\(?:AMS?\\|DAM?\\|ELE?\\|GL\\|RAG?\\)\\|E\\(?:RO\\(?:C\\(?:O\\(?:E\\(?:FF?\\)?\\)?\\)?\\)?\\|SI\\(?:ZE?\\)?\\)\\|F\\(?:IL\\(?:LT?\\)?\\|LI\\(?:ST?\\)?\\|SU\\(?:RF?\\)?\\)\\|G\\(?:EN\\|LUE?\\)\\|IN[APV]\\|L\\(?:IST?\\|LS\\(?:EL?\\)?\\|PH\\(?:AD?\\)?\\)\\|M\\(?:AP\\|B\\(?:EAM?\\|UI\\(?:LD?\\)?\\|[EU]\\)\\|E\\(?:NV\\|SH\\|[NS]\\)\\|MAT?\\|PO\\(?:W\\(?:D\\(?:ER?\\)?\\)?\\)?\\|S\\(?:TEP?\\|UP\\(?:P\\(?:O\\(?:R\\(?:TS?\\)?\\)?\\)?\\)?\\|[TU]\\)\\|TY\\(?:PE?\\)?\\)\\|N\\(?:C\\(?:NTR?\\|UT\\|YC\\|[NUY]\\)\\|D\\(?:ATA?\\|SCL?\\|YNA?\\|[ASY]\\)\\|FL\\(?:OW?\\)?\\|HA\\(?:RM?\\)?\\|I\\(?:SOS?\\|[MS]\\)\\|MO\\(?:DE?\\)?\\|ORM?\\|PR\\(?:ES?\\)?\\|S\\(?:OL?\\|TOA\\(?:QWA?\\|SAS?\\|[QS]\\)\\)\\|T\\(?:IME?\\|YPE?\\|[IY]\\)\\)\\|O\\(?:FF\\(?:ST?\\)?\\|VL\\(?:AP?\\)?\\)\\|P\\(?:LOT?\\|ORT?\\|PE\\(?:ND?\\)?\\|TN\\)\\|R\\(?:C\\(?:LEN?\\|TRM?\\|[LT]\\)\\|E\\(?:AS\\|FI\\(?:NE?\\)?\\|ME\\(?:SH?\\)?\\|VE\\(?:R\\(?:SE?\\)?\\)?\\|[AFMV]\\)\\|OT\\(?:AT?\\)?\\|S\\(?:CA\\(?:LE?\\)?\\|YM\\|[CY]\\)\\)\\|S\\(?:B[ALVW]\\|CR\\(?:ES?\\)?\\|EL\\|IF\\(?:I\\(?:LE?\\)?\\)?\\|KIN?\\|L[LV]\\|OL\\|U[BM]\\)\\|T\\(?:AN\\|RAN?\\|YPE?\\)\\|UTO\\(?:TS?\\)?\\|V\\(?:PR\\(?:IN?\\)?\\|RES?\\)\\|WAVE?\\)\\|B\\(?:CSO\\(?:P\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)?\\|ETAD?\\|F\\(?:A\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|[DL]\\)\\|CUM?\\|DE\\(?:LE?\\)?\\|E\\(?:CUM?\\|DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|SC\\(?:AL?\\)?\\|[CDLS]\\)\\|INT?\\|K\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|[DL]\\)\\|L\\(?:DE\\(?:LE?\\)?\\|IST?\\|LI\\(?:ST?\\)?\\|[DIL]\\)\\|SC\\(?:A\\(?:LE?\\)?\\)?\\|TR\\(?:AN?\\)?\\|UN\\(?:IF?\\)?\\|V\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|[DL]\\)\\)\\|IO\\(?:OPT?\\|[OT]\\)\\|L\\(?:C[45]\\|OCK?\\)\\|O\\(?:OL\\|PTN?\\)\\|S\\(?:AX\\|M[12D]\\|PL\\(?:IN?\\)?\\|S[12]\\|T[EQ]\\)\\|TOL\\|UCO\\(?:PT?\\)?\\)\\|C\\(?:A\\(?:LC\\|MP\\(?:B\\(?:E\\(?:LL?\\)?\\)?\\)?\\)\\|B\\(?:DOF?\\|M[DX]\\|T\\(?:MP\\|[EM]\\)\\)\\|D\\(?:OPT?\\|RE\\(?:AD?\\)?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|E\\(?:C\\(?:HE\\(?:CK?\\)?\\|MOD?\\|YC\\|[HMY]\\)\\|DE\\(?:LE?\\)?\\|IN\\(?:TF?\\)?\\|LI\\(?:ST?\\)?\\|NT\\(?:ER?\\)?\\|QN\\|RIG?\\|SG\\(?:EN?\\)?\\)\\|FACT?\\|G\\(?:LOC?\\|OM\\(?:GA?\\)?\\|ROW?\\)\\|H\\(?:ECK?\\|KM\\(?:SH?\\)?\\)\\|I\\(?:NT\\|RC\\(?:LE?\\)?\\|SOL?\\)\\|L\\(?:O\\(?:CAL?\\|[CG]\\)\\|RM\\(?:S\\(?:H\\(?:LN?\\)?\\)?\\)?\\)\\|M\\(?:A\\(?:CEL?\\|TR\\(?:IX?\\)?\\|[CT]\\)\\|D\\(?:ELE?\\|OM\\(?:E\\(?:GA?\\)?\\)?\\|[EO]\\)\\|ED\\(?:IT?\\)?\\|GRP?\\|LI\\(?:ST?\\)?\\|MOD?\\|OM\\(?:E\\(?:GA?\\)?\\)?\\|PL\\(?:OT?\\)?\\|RO\\(?:T\\(?:A\\(?:TE?\\)?\\)?\\)?\\|S\\(?:EL\\|FI\\(?:LE?\\)?\\|OPT?\\|[EFO]\\)\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|N\\(?:CH\\(?:E\\(?:CK?\\)?\\)?\\|KM\\(?:OD?\\)?\\|TR\\|VT\\(?:OL?\\)?\\)\\|O\\(?:M\\(?:BI\\(?:NE?\\)?\\|PR\\(?:E\\(?:SS?\\)?\\)?\\|[BP]\\)\\|N\\(?:JUG?\\|[4EJ]\\)\\|RI\\(?:O\\(?:L\\(?:IS?\\)?\\)?\\)?\\|UP\\(?:LE?\\)?\\|VAL?\\)\\|P\\(?:CYC?\\|DE\\(?:LE?\\)?\\|IN\\(?:TF?\\)?\\|L\\(?:GEN?\\|IST?\\|[GI]\\)\\|ME\\(?:R\\(?:GE?\\)?\\)?\\|NG\\(?:EN?\\)?\\|SG\\(?:EN?\\)?\\)\\|S\\(?:CIR?\\|DE\\(?:LE?\\)?\\|KP\\|LI\\(?:ST?\\)?\\|WP\\(?:LA?\\)?\\|YS\\)\\|U\\(?:RR\\(?:2D?\\)?\\|TC\\(?:O\\(?:N\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\)?\\)?\\)\\|VAR\\|Y\\(?:C\\(?:CA\\(?:LC?\\)?\\|F\\(?:IL\\(?:ES?\\)?\\|REQ?\\|[IR]\\)\\|LIC?\\|OPT?\\|PH\\(?:A\\(?:SE?\\)?\\)?\\|SP\\(?:EC?\\)?\\|[CLOPS]\\)\\|L\\(?:IND?\\|[45I]\\)\\)\\|Z\\(?:DEL?\\|ME\\(?:SH?\\)?\\)\\)\\|D\\(?:A\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|MO\\(?:R\\(?:PH?\\)?\\)?\\|TA\\(?:D\\(?:EF?\\)?\\)?\\)\\|C\\(?:GO\\(?:MG?\\)?\\|UM\\|VS\\(?:WP?\\)?\\)\\|D\\(?:AS\\(?:P\\(?:EC?\\)?\\)?\\|ELE?\\|OP\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)\\|E\\(?:ACT?\\|FI\\(?:NE?\\)?\\|L\\(?:ETE?\\|TIM?\\|[ET]\\)\\|MO\\(?:R\\(?:PH?\\)?\\)?\\|RIV?\\|S\\(?:IZE?\\|OL\\|[IO]\\)\\|TAB?\\)\\|F\\(?:LX\\|SW\\(?:A\\(?:VE?\\)?\\)?\\)\\|I\\(?:GIT?\\|SP\\(?:L\\(?:AY?\\)?\\)?\\)\\|J\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\)\\|K\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\)\\|L\\(?:DE\\(?:LE?\\)?\\|IST?\\|LI\\(?:ST?\\)?\\)\\|M\\(?:OVE?\\|P\\(?:EXT?\\|OP\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\|RAT?\\|STR?\\|[EORS]\\)\\)\\|NSOL?\\|O\\(?:FS\\(?:EL?\\)?\\|ME\\(?:GA?\\)?\\)\\|S\\(?:CA\\(?:LE?\\)?\\|ET\\|PO\\(?:P\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)?\\|U\\(?:RF\\|[MR]\\)\\|Y[MS]\\)\\|TRAN?\\|UMP\\|V\\(?:AL\\|MO\\(?:R\\(?:PH?\\)?\\)?\\)\\|YNO\\(?:PT?\\)?\\)\\|E\\(?:ALI\\(?:VE?\\)?\\|CPC\\(?:HG?\\)?\\|DELE?\\|EXT\\(?:R\\(?:U\\(?:DE?\\)?\\)?\\)?\\|G\\(?:EN\\|ID\\)\\|IN\\(?:FIN?\\|TF\\|[FT]\\)\\|KILL?\\|L\\(?:BOW?\\|EM\\|IST?\\)\\|M\\(?:A\\(?:GE\\(?:RR?\\)?\\|TW\\(?:R\\(?:I\\(?:TE?\\)?\\)?\\)?\\|[GT]\\)\\|FT\\|I[DS]\\|O\\(?:DIF?\\|RE\\|[DR]\\)\\|S\\(?:EL\\|YM\\|[EY]\\)\\|TG\\(?:EN?\\)?\\|UN\\(?:IT?\\)?\\)\\|N\\(?:DR\\(?:E\\(?:L\\(?:E\\(?:A\\(?:SE?\\)?\\)?\\)?\\)?\\)?\\|ER\\(?:S\\(?:OL?\\)?\\)?\\|GEN?\\|ORM?\\|SYM?\\)\\|ORI\\(?:E\\(?:NT?\\)?\\)?\\|PLOT?\\|QSLV?\\|R\\(?:ASE?\\|E\\(?:AD\\|FI\\(?:NE?\\)?\\|INF?\\|SX\\|[AFIS]\\)\\|NO\\(?:RM?\\)?\\|RA\\(?:NG?\\)?\\)\\|S\\(?:CH\\(?:E\\(?:CK?\\)?\\)?\\|EL\\|IZE?\\|L[ALNV]\\|O\\(?:RT\\|[LR]\\)\\|SO\\(?:LV?\\)?\\|TIF?\\|URF?\\|Y[MS]\\)\\|T\\(?:AB\\(?:LE?\\)?\\|C\\(?:HG\\|ON\\(?:T\\(?:R\\(?:OL?\\)?\\)?\\)?\\|[HO]\\)\\|DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|YPE?\\)\\|USO\\(?:RT?\\)?\\|WRI\\(?:TE?\\)?\\|X\\(?:BO\\(?:PT?\\)?\\|OP\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\|P\\(?:A\\(?:ND\\|SS\\|[NS]\\)\\|RO\\(?:F\\(?:I\\(?:LE?\\)?\\)?\\)?\\|SOL?\\|[RS]\\)\\|T\\(?:OPT?\\|REM?\\|[OR]\\)\\|UN\\(?:IT?\\)?\\)\\)\\|F\\(?:C\\(?:CH\\(?:E\\(?:CK?\\)?\\)?\\|DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|TYP?\\|UM\\)\\|DELE?\\|E\\(?:BO\\(?:DY?\\)?\\|CO\\(?:NS?\\)?\\|FOR?\\|SU\\(?:RF?\\)?\\)\\|I\\(?:L\\(?:EAUX[23]\\|LD\\(?:A\\(?:TA?\\)?\\)?\\|[EL]\\)\\|NI\\(?:SH?\\)?\\|TEM?\\)\\|J\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\)\\|K\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\)\\|L\\(?:IST?\\|ST\\|U\\(?:RE\\(?:AD?\\)?\\|XV\\|[RX]\\)\\)\\|OR\\(?:CE\\|[CM]\\)\\|R\\(?:EQ\\|QS\\(?:CL?\\)?\\)\\|S\\(?:CA\\(?:LE?\\)?\\|S\\(?:ECT?\\|PA\\(?:RM?\\)?\\|[EP]\\)\\|UM\\)\\|T\\(?:RAN?\\|YPE?\\)\\|VME\\(?:SH?\\)?\\)\\|G\\(?:A\\(?:PF\\|UGE?\\)\\|C\\(?:DEF?\\|GEN?\\)\\|E\\(?:NO\\(?:PT?\\)?\\|OM\\(?:E\\(?:T\\(?:RY?\\)?\\)?\\)?\\)\\|M\\(?:AT\\(?:R\\(?:IX?\\)?\\)?\\|FA\\(?:CE?\\)?\\)\\|P\\(?:DE\\(?:LE?\\)?\\|L\\(?:IST?\\|OT\\|[IO]\\)\\)\\|S\\(?:BD\\(?:A\\(?:TA?\\)?\\)?\\|GD\\(?:A\\(?:TA?\\)?\\)?\\|LI\\(?:ST?\\)?\\|SOL?\\|UM\\)\\)\\|H\\(?:ARF\\(?:RQ?\\)?\\|BMAT?\\|E\\(?:LP\\|MI\\(?:O\\(?:PT?\\)?\\)?\\)\\|F\\(?:ANG?\\|SYM?\\)\\|PT\\(?:CR\\(?:E\\(?:A\\(?:TE?\\)?\\)?\\)?\\|DE\\(?:L\\(?:E\\(?:TE?\\)?\\)?\\)?\\|[CD]\\)\\|R\\(?:CP\\(?:LX?\\)?\\|EXP?\\|O\\(?:CE\\(?:AN?\\)?\\|[PU]T\\|[CPU]\\)\\)\\)\\|I\\(?:C\\(?:DE\\(?:LE?\\)?\\|LI\\(?:ST?\\)?\\|RO\\(?:T\\(?:A\\(?:TE?\\)?\\)?\\)?\\)\\|GES\\(?:IN\\|OUT?\\|[IO]\\)\\|M\\(?:AG\\(?:IN?\\)?\\|ESH?\\|MED?\\)\\|N\\(?:IS\\(?:T\\(?:A\\(?:TE?\\)?\\)?\\)?\\|R\\(?:ES\\|TIA?\\|[ET]\\)\\|T1\\|VO\\(?:PT?\\)?\\)\\|OPTN?\\|RL\\(?:IST?\\|[FI]\\)\\)\\|J\\(?:PEG\\|SOL\\)\\|K\\(?:ATT\\|BETW?\\|C\\(?:EN\\(?:T\\(?:ER?\\)?\\)?\\|LE\\(?:AR?\\)?\\)\\|D\\(?:ELE?\\|IST?\\)\\|E\\(?:EP\\|SI\\(?:ZE?\\)?\\|Y\\(?:OPT?\\|PTS?\\|[OPW]\\)\\)\\|FILL?\\|GEN\\|LIST?\\|M\\(?:ESH?\\|O\\(?:DIF?\\|VE\\|[DV]\\)\\)\\|NODE?\\|P\\(?:LOT?\\|SC\\(?:A\\(?:LE?\\)?\\)?\\)\\|REF\\(?:I\\(?:NE?\\)?\\)?\\|S\\(?:C\\(?:ALE?\\|ON\\|[AO]\\)\\|EL\\|L[LN]\\|\\(?:YM\\|[UY]\\)M\\)\\|TRAN?\\|USE\\|WP\\(?:AVE?\\|LAN?\\|[AL]\\)\\)\\|L\\(?:2\\(?:ANG?\\|TAN?\\)\\|A\\(?:N\\(?:BO\\(?:P\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)?\\|[BG]\\)\\|R\\(?:EA\\|GE\\|[CEG]\\)\\|TT\\|Y\\(?:ER\\(?:P\\(?:26?\\)?\\)?\\|LI\\(?:ST?\\)?\\|PL\\(?:OT?\\)?\\|[LP]\\)\\)\\|C\\(?:A\\(?:BS\\|SE\\|[BS]\\)\\|CA\\(?:LC\\|[LT]\\)\\|DEF?\\|F\\(?:ACT?\\|ILE?\\|[AI]\\)\\|LE\\(?:AR?\\)?\\|O\\(?:MB\\|PER?\\|[MP]\\)\\|S\\(?:EL\\|UM\\|[ELU]\\)\\|WR\\(?:I\\(?:TE?\\)?\\)?\\|ZE\\(?:RO?\\)?\\)\\|D\\(?:ELE?\\|IV\\|R\\(?:AG\\|EAD?\\|[AE]\\)\\)\\|E\\(?:SI\\(?:ZE?\\)?\\|XT\\(?:ND?\\)?\\)\\|F\\(?:IL\\(?:LT?\\)?\\|SU\\(?:RF?\\)?\\)\\|G\\(?:EN\\|LUE?\\|WR\\(?:I\\(?:TE?\\)?\\)?\\)\\|I\\(?:N\\(?:ES\\|[AELPV]\\)\\|ST\\)\\|LIST?\\|MESH?\\|NSR\\(?:CH?\\)?\\|O\\(?:CAL?\\|VL\\(?:AP?\\)?\\)\\|P\\(?:LOT?\\|TN\\)\\|R\\(?:E\\(?:FI\\(?:NE?\\)?\\|VE\\(?:R\\(?:SE?\\)?\\)?\\|[FV]\\)\\|OT\\(?:AT?\\)?\\)\\|S\\(?:B[ALVW]\\|CL\\(?:E\\(?:AR?\\)?\\)?\\|DE\\(?:LE?\\)?\\|EL\\|L[AK]\\|OP\\(?:ER?\\)?\\|RE\\(?:AD?\\)?\\|S\\(?:CA\\(?:LE?\\)?\\|OL\\(?:VE?\\)?\\|[CO]\\)\\|TR\\|UM\\|WR\\(?:I\\(?:TE?\\)?\\)?\\|YMM?\\)\\|T\\(?:AN\\|RAN?\\)\\|UMPM?\\|VSC\\(?:A\\(?:LE?\\)?\\)?\\|WPL\\(?:AN?\\)?\\)\\|M\\(?:A\\(?:CO\\(?:PT?\\)?\\|G\\(?:OPT?\\|SO\\(?:LV?\\)?\\|[OS]\\)\\|P\\(?:2D\\(?:T\\(?:O\\(?:3D?\\)?\\)?\\)?\\|SO\\(?:L\\(?:VE?\\)?\\)?\\|VAR?\\|[2SV]\\)\\|S\\(?:CA\\(?:LE?\\)?\\|TER?\\|[CT]\\)\\|TER?\\)\\|CHE\\(?:CK?\\)?\\)\\|~\\(?:CAT\\(?:5IN?\\|IA\\(?:IN?\\)?\\|[5I]\\)\\|P\\(?:AR\\(?:A\\(?:IN?\\)?\\)?\\|RO\\(?:E\\(?:IN?\\)?\\)?\\)\\|SAT\\(?:IN?\\)?\\|UGIN?\\)\\)"
"APDL keyword name regexp 2c.")

(defconst apdl-command-regexp-1
"\\(?:\\*\\(?:A\\(?:BBR\\|FUN\\|SK\\|XPY\\)\\|C\\(?:F\\(?:CLOS\\|OPEN\\|WRITE\\)\\|OMP\\|\\(?:REAT\\|YCL\\)E\\)\\|D\\(?:EL\\|IM\\|MAT\\|O\\(?:T\\|WHILE\\)?\\)\\|E\\(?:IGEN\\|LSE\\(?:IF\\)?\\|ND\\(?:DO\\|IF\\)?\\|X\\(?:\\(?:I\\|POR\\)T\\)\\)\\|F\\(?:FT\\|REE\\)\\|G\\(?:ET\\|O\\)\\|I\\(?:F\\|NIT\\|TENGINE\\)\\|L\\(?:IST\\|S\\(?:BAC\\|DUMP\\|ENGINE\\|FACTOR\\|RESTORE\\)\\)\\|M\\(?:ERGE\\|F\\(?:OURI\\|UN\\)\\|OPER\\|SG\\|ULT\\|WRITE\\)\\|NRM\\|PRINT\\|RE\\(?:MOVE\\|NAME\\|PEAT\\|TURN\\)\\|S\\(?:CAL\\|ET\\|MAT\\|ORT\\|READ\\|TATUS\\)\\|T\\(?:AXIS\\|OPER\\|READ\\)\\|U\\(?:LIB\\|SE\\)\\|V\\(?:ABS\\|C\\(?:OL\\|UM\\)\\|E\\(?:C\\|DIT\\)\\|F\\(?:ACT\\|ILL\\|UN\\)\\|GET\\|ITRP\\|LEN\\|MASK\\|OPER\\|P\\(?:\\(?:LO\\|U\\)T\\)\\|READ\\|S\\(?:CFUN\\|TAT\\)\\|WRITE\\)\\|WRK\\|XPL\\)\\|/\\(?:A\\(?:N\\(?:3D\\|FILE\\|GLE\\|NOT\\|UM\\)\\|SSIGN\\|U\\(?:TO\\|X\\(?:1[25]\\|[23]\\)\\)\\|XLAB\\)\\|BATCH\\|C\\(?:FORMAT\\|L\\(?:ABEL\\|EAR\\|OG\\)\\|MAP\\|O\\(?:LOR\\|M\\|N\\(?:FIG\\|TOUR\\)\\|PY\\)\\|PLANE\\|TYPE\\|VAL\\|\\(?:W\\|YCEXPAN\\)D\\)\\|D\\(?:E\\(?:\\(?:LET\\|VIC\\)E\\)\\|FLAB\\|I\\(?:RECTORY\\|ST\\)\\|SCALE\\|V3D\\)\\|E\\(?:DGE\\|FACET\\|OF\\|RASE\\|SHAPE\\|X\\(?:IT\\|PAND\\)\\)\\|F\\(?:ACET\\|COMP\\|DELE\\|ILNAME\\|O\\(?:CUS\\|RMAT\\)\\)\\|G\\(?:C\\(?:MD\\|OLUMN\\)\\|F\\(?:ILE\\|ORMAT\\)\\|LINE\\|MARKER\\|O\\(?:LIST\\|PR\\)?\\|R\\(?:APHICS\\|ESUME\\|ID\\|OPT\\|TYP\\)\\|S\\(?:AVE\\|T\\)\\|T\\(?:HK\\|YPE\\)\\)\\|H\\(?:BC\\|EADER\\)\\|I\\(?:C\\(?:LWID\\|SCALE\\)\\|MAGE\\|N\\(?:PUT\\|QUIRE\\)\\)\\|L\\(?:ARC\\|I\\(?:GHT\\|NE\\)\\|S\\(?:PEC\\|YMBOL\\)\\)\\|M\\(?:AP\\|ENU\\|KDIR\\|PLIB\\|REP\\|START\\)\\|N\\(?:ERR\\|O\\(?:ERASE\\|LIST\\|PR\\|RMAL\\)\\|UMBER\\)\\|OUTPUT\\|P\\(?:AGE\\|B[CF]\\|CIRCLE\\|LOPTS\\|M\\(?:ACRO\\|ORE\\)\\|NUM\\|O\\(?:LYGON\\|ST\\(?:1\\|26\\)\\)\\|REP7\\|S\\(?:EARCH\\|F\\|PEC\\|TATUS\\|YMB\\)\\|WEDGE\\)\\|QUIT\\|R\\(?:ATIO\\|E\\(?:NAME\\|\\(?:PLO\\|SE\\)T\\)\\|GB\\|MDIR\\)\\|S\\(?:E\\(?:CLIB\\|G\\)\\|H\\(?:ADE\\|OW\\|RINK\\)\\|MBC\\|OLU\\|SCALE\\|T\\(?:ATUS\\|ITLE\\)\\|Y[PS]\\)\\|T\\(?:EE\\|ITLE\\|LABEL\\|R\\(?:IAD\\|LCY\\)\\|SPEC\\|\\(?:XTR\\|YP\\)E\\)\\|U\\(?:CMD\\|DOC\\|IS?\\|NITS\\|SER\\)\\|V\\(?:CONE\\|IEW\\|SCALE\\|UP\\)\\|W\\(?:AIT\\|INDOW\\)\\|X\\(?:FRM\\|RANGE\\)\\|YRANGE\\|ZOOM\\)\\|A\\(?:A\\(?:DD\\|TT\\)\\|B\\(?:B\\(?:RES\\|SAV\\)\\|EXTRACT\\|S\\)\\|C\\(?:C\\(?:AT\\|OPTION\\)\\|EL\\|LEAR\\)\\|D\\(?:AMS\\|D\\(?:AM\\)?\\|ELE\\|GL\\|RAG\\)\\|E\\(?:ROCOEFF\\|SIZE\\)\\|F\\(?:ILLT\\|LIST\\|SURF\\)\\|G\\(?:EN\\|LUE\\)\\|IN[APV]\\|L\\(?:IST\\|LSEL\\|PHAD\\)?\\|M\\(?:AP\\|B\\(?:EAM\\|UILD\\)\\|E\\(?:NV\\|SH\\)\\|MAT\\|POWDER\\|S\\(?:TEP\\|UPPORTS\\)\\|TYPE\\)\\|N\\(?:C\\(?:NTR\\|UT\\|YC\\)\\|D\\(?:ATA\\|SCL\\|YNA\\)\\|FLOW\\|HARM\\|I\\(?:M\\|SOS\\)\\|MODE\\|ORM\\|PRES\\|S\\(?:OL\\|TOA\\(?:QWA\\|SAS\\)\\)\\|T\\(?:\\(?:IM\\|YP\\)E\\)\\)\\|O\\(?:FFST\\|VLAP\\)\\|P\\(?:LOT\\|ORT\\|PEND\\|TN\\)\\|R\\(?:C\\(?:LEN\\|TRM\\)\\|E\\(?:AS\\|FINE\\|MESH\\|VERSE\\)\\|OTAT\\|S\\(?:CALE\\|YM\\)\\)\\|S\\(?:B[ALVW]\\|CRES\\|EL\\|IFILE\\|KIN\\|L[LV]\\|OL\\|U[BM]\\)\\|T\\(?:AN\\|RAN\\|YPE\\)\\|UTOTS\\|V\\(?:PRIN\\|RES\\)\\|WAVE\\)\\|B\\(?:CSOPTION\\|ETAD\\|F\\(?:A\\(?:DELE\\|LIST\\)\\|CUM\\|DELE\\|E\\(?:CUM\\|DELE\\|LIST\\|SCAL\\)\\|INT\\|K\\(?:DELE\\|LIST\\)\\|L\\(?:DELE\\|L?IST\\)\\|SCALE\\|TRAN\\|UNIF\\|V\\(?:DELE\\|LIST\\)\\|[AEKLV]\\)?\\|IO\\(?:\\(?:OP\\)?T\\)\\|L\\(?:C[45]\\|OCK\\)\\|O\\(?:OL\\|PTN\\)\\|S\\(?:AX\\|M[12D]\\|PLIN\\|S[12]\\|T[EQ]\\)\\|TOL\\|UCOPT\\)\\|C\\(?:A\\(?:LC\\|MPBELL\\)\\|B\\(?:DOF\\|M[DX]\\|T\\(?:E\\|MP\\)\\)\\|D\\(?:OPT\\|READ\\|WRITE\\)\\|E\\(?:C\\(?:HECK\\|MOD\\|YC\\)\\|DELE\\|INTF\\|LIST\\|NTER\\|QN\\|RIG\\|SGEN\\)\\|FACT\\|G\\(?:LOC\\|OMGA\\|ROW\\)\\|H\\(?:ECK\\|KMSH\\)\\|I\\(?:NT\\|RCLE\\|SOL\\)\\|L\\(?:O\\(?:CAL\\|G\\)\\|RMSHLN\\)\\|M\\(?:A\\(?:CEL\\|TRIX\\)\\|D\\(?:ELE\\|OMEGA\\)\\|EDIT\\|GRP\\|LIST\\|MOD\\|OMEGA\\|PLOT\\|ROTATE\\|S\\(?:EL\\|FILE\\|OPT\\)\\|WRITE\\)\\|N\\(?:CHECK\\|KMOD\\|TR\\|VTOL\\)\\|O\\(?:M\\(?:BINE\\|PRESS\\)\\|N\\(?:JUG\\|[4E]\\)\\|RIOLIS\\|UPLE\\|VAL\\)\\|P\\(?:CYC\\|DELE\\|INTF\\|L\\(?:GEN\\|IST\\)\\|MERGE\\|[NS]GEN\\)\\|QC\\|S\\(?:CIR\\|DELE\\|KP\\|LIST\\|WPLA\\|YS\\)\\|U\\(?:RR2D\\|TCONTROL\\)\\|VAR\\|Y\\(?:C\\(?:CALC\\|F\\(?:ILES\\|REQ\\)\\|LIC\\|OPT\\|PHASE\\|SPEC\\)\\|L\\(?:IND\\|[45]\\)\\)\\|Z\\(?:DEL\\|MESH\\)\\|[EMPS]\\)\\|D\\(?:A\\(?:DELE\\|LIST\\|MORPH\\|TA\\(?:DEF\\)?\\)\\|C\\(?:GOMG\\|UM\\|VSWP\\)\\|D\\(?:ASPEC\\|ELE\\|OPTION\\)\\|E\\(?:ACT\\|FINE\\|L\\(?:ETE\\|TIM\\)\\|MORPH\\|RIV\\|S\\(?:IZE\\|OL\\)\\|TAB\\)\\|F\\(?:LX\\|SWAVE\\)\\|I\\(?:G\\(?:IT\\)?\\|SPLAY\\)\\|J\\(?:DELE\\|LIST\\)\\|K\\(?:DELE\\|LIST\\)\\|L\\(?:DELE\\|L?IST\\)\\|M\\(?:OVE\\|P\\(?:EXT\\|OPTION\\|RAT\\|STR\\)\\)\\|NSOL\\|O\\(?:F\\(?:SEL\\)?\\|MEGA\\)\\|S\\(?:CALE\\|ET\\|POPTION\\|U\\(?:M\\|RF\\)\\|Y[MS]\\)\\|TRAN\\|UMP\\|V\\(?:AL\\|MORPH\\)\\|YNOPT\\|[AJKL]\\)\\|E\\(?:ALIVE\\|CPCHG\\|DELE\\|EXTRUDE\\|G\\(?:EN\\|ID\\)\\|IN\\(?:FIN\\|TF\\)\\|KILL\\|L\\(?:BOW\\|EM\\|IST\\)\\|M\\(?:A\\(?:GERR\\|TWRITE\\)\\|FT?\\|I[DS]\\|O\\(?:DIF\\|RE\\)\\|S\\(?:EL\\|YM\\)\\|TGEN\\|UNIT\\)\\|N\\(?:DRELEASE\\|ERSOL\\|GEN\\|\\(?:OR\\|SY\\)M\\)\\|ORIENT\\|PLOT\\|QSLV\\|R\\(?:ASE\\|E\\(?:AD\\|FINE\\|INF\\|SX\\)\\|NORM\\|RANG\\)\\|S\\(?:CHECK\\|EL\\|IZE\\|L[ALNV]\\|O\\(?:L\\|RT\\)\\|SOLV\\|TIF\\|URF\\|Y[MS]\\)\\|T\\(?:ABLE\\|C\\(?:HG\\|ONTROL\\)\\|DELE\\|LIST\\|YPE\\)\\|USORT\\|WRITE\\|X\\(?:BOPT\\|OPTION\\|P\\(?:A\\(?:ND\\|SS\\)\\|ROFILE\\|SOL\\)?\\|T\\(?:OPT\\|REM\\)\\|UNIT\\)\\|[NT]\\)\\|F\\(?:C\\(?:CHECK\\|DELE\\|LIST\\|TYP\\|UM\\)\\|DELE\\|E\\(?:BODY\\|CONS\\|FOR\\|SURF\\)\\|I\\(?:L\\(?:EAUX[23]\\|LDATA\\|[EL]\\)\\|NISH\\|TEM\\)\\|J\\(?:DELE\\|LIST\\)\\|K\\(?:DELE\\|LIST\\)\\|L\\(?:IST\\|ST\\|U\\(?:READ\\|XV\\)\\)\\|OR\\(?:CE\\|M\\)\\|R\\(?:EQ\\|QSCL\\)\\|S\\(?:CALE\\|S\\(?:ECT\\|PARM\\)\\|UM\\)\\|T\\(?:RAN\\|YPE\\)\\|VMESH\\|[CJK]\\)\\|G\\(?:A\\(?:PF?\\|UGE\\)\\|C\\(?:DEF\\|GEN\\)\\|E\\(?:NOPT\\|OM\\(?:ETRY\\)?\\)\\|M\\(?:ATRIX\\|FACE\\)\\|P\\(?:DELE\\|L\\(?:\\(?:IS\\|O\\)T\\)\\)?\\|RP\\|S\\(?:BDATA\\|GDATA\\|LIST\\|SOL\\|UM\\)\\)\\|H\\(?:ARFRQ\\|BMAT\\|E\\(?:LP\\|MIOPT\\)\\|F\\(?:ANG\\|SYM\\)\\|PT\\(?:\\(?:CREA\\|DELE\\)TE\\)\\|R\\(?:CPLX\\|EXP\\|O\\(?:CEAN\\|[PU]T\\)\\)\\)\\|I\\(?:C\\(?:DELE\\|LIST\\|ROTATE\\)?\\|GES\\(?:IN\\|OUT\\)\\|M\\(?:AGIN\\|ESH\\|MED\\)\\|N\\(?:ISTATE\\|R\\(?:ES\\|TIA\\)\\|T1\\|VOPT\\)\\|OPTN\\|RL\\(?:F\\|IST\\)\\)\\|J\\(?:PEG\\|SOL\\)\\|K\\(?:ATT\\|B\\(?:C\\|ETW\\)\\|C\\(?:\\(?:ENTE\\|LEA\\)R\\)\\|D\\(?:ELE\\|IST\\)\\|E\\(?:EP\\|SIZE\\|Y\\(?:OPT\\|PTS\\|W\\)\\)\\|FILL\\|GEN\\|L\\(?:IST\\)?\\|M\\(?:ESH\\|O\\(?:DIF\\|VE\\)\\)\\|NODE\\|P\\(?:LOT\\|SCALE\\)\\|REFINE\\|S\\(?:C\\(?:ALE\\|ON\\)\\|EL\\|L[LN]\\|\\(?:U\\|YM\\)M\\)\\|TRAN\\|USE\\|WP\\(?:AVE\\|LAN\\)\\)\\|L\\(?:2\\(?:ANG\\|TAN\\)\\|A\\(?:N\\(?:BOPTION\\|G\\)\\|R\\(?:C\\|EA\\|GE\\)\\|TT\\|Y\\(?:ER\\(?:P26\\)?\\|\\(?:LIS\\|PLO\\)T\\)\\)\\|C\\(?:A\\(?:BS\\|SE\\)\\|CA\\(?:LC\\|T\\)\\|DEF\\|F\\(?:ACT\\|ILE\\)\\|LEAR\\|O\\(?:MB\\|PER\\)\\|S\\(?:EL\\|L\\|UM\\)\\|WRITE\\|ZERO\\)\\|D\\(?:ELE\\|IV\\|R\\(?:AG\\|EAD\\)\\)\\|E\\(?:SIZE\\|XTND\\)\\|F\\(?:ILLT\\|SURF\\)\\|G\\(?:EN\\|\\(?:LU\\|WRIT\\)E\\)\\|I\\(?:N\\(?:ES\\|[AELPV]\\)\\|ST\\)\\|LIST\\|MESH\\|NSRCH\\|O\\(?:CAL\\|VLAP\\)\\|P\\(?:LOT\\|TN\\)\\|R\\(?:E\\(?:\\(?:FIN\\|VERS\\)E\\)\\|OTAT\\)\\|S\\(?:B[ALVW]\\|CLEAR\\|DELE\\|EL\\|L[AK]\\|OPER\\|READ\\|S\\(?:\\(?:CAL\\|OLV\\)E\\)\\|TR\\|UM\\|WRITE\\|YMM\\)\\|T\\(?:R?AN\\)\\|UMPM\\|VSCALE\\|WPLAN\\)\\|M\\(?:A\\(?:COPT\\|G\\(?:OPT\\|SOLV\\)\\|P\\(?:2DTO3D\\|SOLVE\\|VAR\\)\\|S\\(?:CALE\\|TER\\)\\|TER\\|[PT]\\)\\|CHECK\\|D\\(?:AMP\\|ELE\\|PLOT\\)\\|E\\(?:MM\\|SHING\\)\\|GEN\\|IDTOL\\|LIST\\|M\\(?:ASS\\|F\\)\\|O\\(?:D\\(?:CONT\\|DIR\\|E\\|IFY\\|MSH\\|OPT\\|SELOPTION\\)\\|NITOR\\|PT\\|RPH\\|VE\\)\\|P\\(?:AMOD\\|C\\(?:HG\\|OPY\\)\\|D\\(?:ATA\\|ELE\\|RES\\)\\|LIST\\|PLOT\\|R\\(?:EAD\\|INT\\)\\|T\\(?:EMP\\|GEN\\|RES\\)\\|WRITE\\)?\\|RPM\\|S\\(?:AVE\\|H\\(?:APE\\|COPY\\|KEY\\|MID\\|PATTERN\\)\\|\\(?:OLV\\|TOL\\)E\\)\\|XPAND\\)\\|N\\(?:A\\(?:NG\\|XIS\\)\\|CNV\\|D\\(?:ELE\\|IST\\|SURF\\)\\|EQIT\\|FORCE\\|GEN\\|KPT\\|L\\(?:ADAPTIVE\\|D\\(?:IAG\\|POST\\)\\|GEOM\\|HIST\\|IST\\|MESH\\|O\\(?:G\\|PT\\)\\)\\|MODIF\\|O\\(?:DES\\|OFFSET\\|R[AL]\\)\\|P\\(?:\\(?:LO\\|RIN\\)T\\)\\|R\\(?:E\\(?:AD\\|FINE\\)\\|LSUM\\|O\\(?:\\(?:P\\|TA\\)T\\)\\|RANG\\)\\|S\\(?:CALE\\|EL\\|L[AEKLV]\\|MOOTH\\|O\\(?:L\\|RT\\)\\|TORE\\|UBST\\|VR\\|YM\\)\\|U\\(?:M\\(?:CMP\\|EXP\\|MRG\\|OFF\\|\\(?:ST\\|VA\\)R\\)\\|SORT\\)\\|W\\(?:P\\(?:AVE\\|LAN\\)\\|RITE\\)\\)\\|O\\(?:C\\(?:D\\(?:ATA\\|ELETE\\)\\|LIST\\|READ\\|\\(?:T\\(?:ABL\\|YP\\)\\|ZON\\)E\\)\\|MEGA\\|P\\(?:ERATE\\|NCONTROL\\)\\|UT\\(?:AERO\\|GEOM\\|OPT\\|PR\\|RES\\)\\|VCHECK\\)\\|P\\(?:A\\(?:DELE\\|GET\\|PUT\\|R\\(?:ESU\\|RES\\|SAV\\)\\|SAVE\\|TH\\|USE\\)\\|C\\(?:ALC\\|GOPT\\|IRC\\|ROSS\\)\\|D\\(?:EF\\|OT\\)\\|ER\\(?:BC2D\\|TURB\\)\\|FACT\\|HYSICS\\|IVCHECK\\|L\\(?:AS\\|C\\(?:AMP\\|FREQ\\|HIST\\|INT\\|KSURF\\|PLX\\)\\|DISP\\|E\\(?:SOL\\|TAB\\)\\|F\\(?:2D\\|AR\\)\\|GEOM\\|LS\\|M\\(?:AP\\|C\\)\\|N\\(?:EAR\\|SOL\\)\\|O\\(?:RB\\|TTING\\)\\|PA\\(?:GM\\|TH\\)\\|SECT\\|T\\(?:IME\\|RAC\\)\\|V\\(?:AR\\|ECT\\)\\|ZZ\\)\\|M\\(?:AP\\|GTRAN\\|L\\(?:OPT\\|SIZE\\)\\)\\|NGR\\|O\\(?:INT\\|LY\\|WERH\\)\\|PATH\\|R\\(?:A\\(?:NGE\\|S\\)\\|C\\(?:AMP\\|INT\\|PLX\\)\\|E\\(?:D\\|NERGY\\|RR\\|SOL\\|TAB\\)\\|FAR\\|I\\(?:NT\\|SM\\|TER\\|[2M]\\)\\|JSOL\\|MC\\|N\\(?:EAR\\|LD\\|SOL\\)\\|O\\(?:D\\|RB\\)\\|PATH\\|R\\(?:FOR\\|SOL\\)\\|S\\(?:CONTROL\\|ECT\\)\\|TIME\\|V\\(?:AR\\|ECT\\)\\)\\|S\\(?:CONTROL\\|D\\(?:COM\\|FRQ\\|GRAPH\\|RES\\|SPL\\|UNIT\\|VAL\\|WAV\\)\\|EL\\|M\\(?:AT\\|ESH\\)\\|\\(?:TRE\\|Y\\)S\\)\\|T\\(?:R\\|XY\\)\\|VECT\\)\\|Q\\(?:DVAL\\|RDOPT\\|SOPT\\|U\\(?:AD\\|OT\\)\\)\\|R\\(?:A\\(?:CE\\|DOPT\\|PPND\\|TE\\)\\|BE3\\|C\\(?:ON\\|YC\\)\\|DE\\(?:C\\|LE\\)\\|E\\(?:A\\(?:LVAR\\|[DL]\\)\\|CTNG\\|MESH\\|S\\(?:CO\\(?:MBINE\\|NTROL\\)\\|ET\\|P\\|UME\\|VEC\\|WRITE\\)\\|ZONE\\)\\|FORCE\\|IG\\(?:ID\\|RESP\\)\\|LIST\\|M\\(?:A\\(?:LIST\\|NL\\|STER\\)\\|C\\(?:AP\\|LIST\\)\\|FLVEC\\|LVSCALE\\|M\\(?:LIST\\|RANGE\\|SELECT\\)\\|N\\(?:DISP\\|EVEC\\)\\|O\\(?:DIF\\|RE\\)\\|PORDER\\|R\\(?:ESUME\\|GENERATE\\|OPTIONS\\|PLOT\\|STATUS\\)\\|S\\(?:\\(?:AV\\|MPL\\)E\\)\\|USE\\|XPORT\\)\\|O\\(?:CK\\|SE\\)\\|P\\(?:OLY\\|R\\(?:4\\|ISM\\)\\|SD\\)\\|S\\(?:MESH\\|OPT\\|PLIT\\|T\\(?:MAC\\|OFF\\)\\|URF\\|Y\\(?:MM\\|S\\)\\)\\)\\|S\\(?:A\\(?:BS\\|DD\\|LLOW\\|VE\\)\\|BC\\(?:LIST\\|TRAN\\)\\|COPT\\|DELETE\\|E\\(?:C\\(?:CONTROL\\|DATA\\|FUNCTION\\|JOINT\\|LOCK\\|MODIF\\|NUM\\|OFFSET\\|PLOT\\|READ\\|STOP\\|\\(?:TYP\\|WRIT\\)E\\)\\|DLIST\\|EXP\\|L\\(?:IST\\|M\\|TOL\\)\\|MIIMPLICIT\\|NERGY\\|OPT\\|SYMM\\|T\\(?:FGAP\\|RAN\\)\\|XP\\|[DT]\\)\\|F\\(?:A\\(?:CT\\|DELE\\|LIST\\)\\|BEAM\\|C\\(?:ALC\\|ONTROL\\|UM\\)\\|DELE\\|E\\(?:DELE\\|LIST\\)\\|FUN\\|GRAD\\|L\\(?:DELE\\|EX\\|L?IST\\)\\|SCALE\\|TRAN\\|[AEL]\\)\\|H\\(?:ELL\\|PP\\|SD\\)\\|L\\(?:IST\\|OAD\\)\\|M\\(?:A\\(?:LL\\|X\\)\\|BODY\\|CONS\\|FOR\\|IN\\|OOTH\\|RTSIZE\\|SURF\\|ULT\\)\\|NOPTION\\|O\\(?:L\\(?:U\\(?:OPT\\)?\\|VE\\)\\|RT\\|URCE\\)\\|P\\(?:ACE\\|C\\(?:NOD\\|TEMP\\)\\|DAMP\\|EC\\|FREQ\\|GRAPH\\|H\\(?:ERE\\|[45]\\)\\|L\\(?:INE\\|OT\\)\\|MWRITE\\|O\\(?:\\(?:IN\\|P\\)T\\)\\|READ\\|TOPT\\|UNIT\\|VAL\\)\\|QRT\\|RSS\\|S\\(?:BT\\|LN\\|MT\\|OPT\\|P[ABDEM]\\|TATE\\|UM\\)\\|T\\(?:A\\(?:BILIZE\\|T\\)\\|EF\\|ORE\\)\\|U\\(?:B\\(?:\\(?:OP\\|SE\\)T\\)\\|C\\(?:ALC\\|R\\)\\|DEL\\|EVAL\\|GET\\|M\\(?:AP\\|TYPE\\)\\|P[LR]\\|RESU\\|S\\(?:AVE\\|EL\\)\\|VECT\\)\\|V\\(?:PLOT\\|TYP\\)\\|W\\(?:ADD\\|DEL\\|GEN\\|LIST\\)\\|YNCHRO\\|[EFV]\\)\\|T\\(?:A\\(?:LLOW\\|RGET\\)\\|B\\(?:COPY\\|D\\(?:ATA\\|ELE\\)\\|EO\\|F\\(?:IELD\\|T\\)\\|IN\\|L\\(?:E\\|IST\\)\\|MODIF\\|P\\(?:\\(?:LO\\)?T\\)\\|TEMP\\)?\\|CHG\\|H\\(?:EXPAND\\|OPT\\)\\|I\\(?:FF\\|M\\(?:ERANGE\\|INT\\|[EP]\\)\\|NTP\\)\\|O\\(?:FFST\\|RUS\\)\\|R\\(?:ANSFER\\|EF\\|NOPT\\|P\\(?:DEL\\|LIS\\|OIN\\)\\|TIME\\)\\|S\\(?:HAP\\|RES\\)\\|UNIF\\|VAR\\|YPE\\)\\|U\\(?:IMP\\|N\\(?:D\\(?:ELETE\\|O\\)\\|PAUSE\\)\\|P\\(?:COORD\\|GEOM\\)\\|SR\\(?:CAL\\|DOF\\|ELEM\\)\\)\\|V\\(?:2DOPT\\|A\\(?:DD\\|R\\(?:DEL\\|NAM\\)\\|TT\\)?\\|C\\(?:LEAR\\|ROSS\\)\\|D\\(?:DAM\\|ELE\\|GL\\|OT\\|RAG\\)\\|E\\(?:\\(?:ORIEN\\|X\\)T\\)\\|F\\(?:OPT\\|QUERY\\|SM\\)\\|G\\(?:E[NT]\\|LUE\\)\\|I\\(?:MP\\|N[PV]\\)\\|L\\(?:IST\\|SCALE\\)\\|MESH\\|O\\(?:FFST\\|LUMES\\|VLAP\\)\\|P\\(?:LOT\\|TN\\|UT\\)\\|ROTAT\\|S\\(?:B[AVW]\\|EL\\|LA\\|UM\\|WEEP\\|YMM\\)\\|T\\(?:RAN\\|YPE\\)\\)\\|W\\(?:P\\(?:AVE\\|CSYS\\|LANE\\|OFFS\\|ROTA\\|STYL\\)\\|R\\(?:FULL\\|ITE\\(?:MAP\\)?\\)\\|SPRINGS\\|TBCREATE\\)\\|X\\(?:F\\(?:CRKMESH\\|DATA\\|ENRICH\\|LIST\\)\\|VAR\\)\\|~\\(?:\\(?:CAT\\(?:5\\|IA\\)\\|P\\(?:ARA\\|ROE\\)\\|SAT\\|UG\\)IN\\)\\|[AC-FK-NRV]\\)"
"APDL full keyword name regexp.")

(defconst apdl-command-regexp
"\\(?:\\*\\(?:A\\(?:BB\\|FU\\|SK\\|XP\\)\\|C\\(?:F[COW]\\|OM\\|RE\\|YC\\)\\|D\\(?:EL\\|IM\\|MA\\|O[TW]?\\)\\|E\\(?:IG\\|LSE\\(?:IF\\)?\\|ND\\(?:DO\\|IF\\)?\\|VA\\|X[IP]\\)\\|F\\(?:FT\\|RE\\)\\|G\\(?:ET\\|O\\)\\|I\\(?:F\\|NI\\|TE\\)\\|L\\(?:IS\\|S[BDEFR]\\)\\|M\\(?:ER\\|F[OU]\\|O[OP]\\|SG\\|UL\\|WR\\)\\|NRM\\|PRI\\|RE[MNPT]\\|S\\(?:CA\\|ET\\|MA\\|OR\\|RE\\|TA\\)\\|T\\(?:AX\\|OP\\|RE\\)\\|U\\(?:LI\\|SE\\)\\|V\\(?:AB\\|C[OU]\\|E[CD]\\|F[AIU]\\|GE\\|IT\\|LE\\|MA\\|OP\\|P[LU]\\|RE\\|S[CT]\\|WR\\)\\|WRK\\|XPL\\)\\|/\\(?:A\\(?:N[3FGNU]\\|SS\\|U\\(?:T\\|X\\(?:1[25]\\|[23]\\)\\)\\|XL\\)\\|BAT\\|C\\(?:FO\\|L[AEO]\\|MA\\|O\\(?:N[FT]\\|[LMP]\\)\\|PL\\|TY\\|VA\\|WD\\|YC\\)\\|D\\(?:E[LV]\\|FL\\|I[RS]\\|SC\\|V3\\)\\|E\\(?:DG\\|FA\\|OF\\|RA\\|SH\\|X[IP]\\)\\|F\\(?:AC\\|CO\\|DE\\|IL\\|O[CR]\\)\\|G\\(?:C[MO]\\|F[IO]\\|LI\\|MA\\|O[LP]?\\|R[AEIOT]\\|S[AT]\\|T[HY]\\)\\|H\\(?:BC\\|EA\\)\\|I\\(?:C[LS]\\|MA\\|N[PQ]\\)\\|L\\(?:AR\\|I[GN]\\|S[PY]\\)\\|M\\(?:AP\\|EN\\|KD\\|PL\\|RE\\|ST\\)\\|N\\(?:ER\\|O[ELPR]\\|UM\\)\\|O\\(?:[PU]T\\)\\|P\\(?:AG\\|B[CF]\\|CI\\|IC\\|LO\\|M[AEO]\\|NU\\|O\\(?:L\\|ST\\(?:1\\|26\\)\\)\\|REP7\\|S[EFPTY]\\|WE\\)\\|QUI\\|R\\(?:AT\\|E[NPS]\\|GB\\|MD\\|UNSTA?\\)\\|S\\(?:E[CG]\\|H[AOR]\\|MB\\|OL\\|S[CS]\\|T[AI]\\|Y[PS]\\)\\|T\\(?:EE\\|IT\\|LA\\|R[AIL]\\|SP\\|XT\\|YP\\)\\|U\\(?:CM\\|DO\\|IS?\\|NI\\|SE\\)\\|V\\(?:CO\\|ER\\|IE\\|SC\\|T\\|UP\\)\\|W\\(?:AI\\|B\\|IN\\)\\|X\\(?:FR\\|ML\\|RA\\)\\|YRA\\|ZOO\\)\\|A\\(?:A\\(?:DD\\|TT\\)\\|B\\(?:B[RS]\\|EX\\|S\\)\\|C\\(?:C[AO]\\|EL\\|LE\\)\\|D\\(?:AM\\|DA?\\|EL\\|GL\\|RA\\)\\|E\\(?:RO\\|SI\\)\\|F\\(?:IL\\|LI\\|SU\\)\\|G\\(?:EN\\|LU\\)\\|IN[APV]\\|L\\(?:IS\\|LS\\|P[FH]\\)?\\|M\\(?:AP\\|B[EU]\\|E[NS]\\|MA\\|PO\\|S[TU]\\|TY\\)\\|N\\(?:C[NUY]\\|D[ASY]\\|FL\\|HA\\|I[MS]\\|MO\\|OR\\|PR\\|S\\(?:O\\|TOA[QS]\\)\\|T[IY]\\)\\|O\\(?:FF\\|VL\\)\\|P\\(?:LO\\|OR\\|PE\\|TN\\)\\|R\\(?:C[LOT]\\|DE\\|E[AFMV]\\|FI\\|ME\\|OT\\|S[CPY]\\)\\|S\\(?:B[ALVW]\\|CR\\|EL\\|IF\\|KI\\|L[LNV]\\|OL\\|U[BM]\\)\\|T\\(?:AN\\|RA\\|YP\\)\\|UTO\\|V\\(?:PR\\|RE\\)\\|WAV\\)\\|B\\(?:CSO\\|ETA\\|F\\(?:A[DL]\\|CU\\|DE\\|E[CDLS]\\|IN\\|K[DL]\\|L[DIL]\\|SC\\|TR\\|UN\\|V[DL]\\|[AEKLV]\\)?\\|IO[OT]\\|L\\(?:C[45]\\|OC\\)\\|O\\(?:OL\\|PT\\)\\|S\\(?:AX\\|M[12D]\\|PL\\|S[12]\\|T[EQ]\\)\\|TOL\\|UCO\\)\\|C\\(?:A\\(?:LC\\|MP\\)\\|B\\(?:DO\\|M[DX]\\|T[EM]\\)\\|D\\(?:OP\\|RE\\|WR\\)\\|E\\(?:C[HMY]\\|DE\\|IN\\|LI\\|NT\\|QN\\|RI\\|SG\\|WR\\)\\|FAC\\|G\\(?:LO\\|OM\\|RO\\)\\|H\\(?:EC\\|KM\\)\\|I\\(?:NT\\|RC\\|SO\\)\\|L\\(?:O[CG]\\|RM\\)\\|M\\(?:A[CT]\\|BL\\|D[EO]\\|ED\\|GR\\|LI\\|MO\\|OM\\|PL\\|RO\\|S[EFO]\\|WR\\)\\|N\\(?:CH\\|KM\\|TR\\|VT\\)\\|O\\(?:M[BP]\\|N[4EJ]\\|RI\\|UP\\|VA\\)\\|P\\(?:CY\\|DE\\|IN\\|L[GI]\\|ME\\|[NS]G\\)\\|QC\\|S\\(?:CI\\|DE\\|KP\\|LI\\|WP\\|YS\\)\\|U\\(?:RR\\|TC\\)\\|VAR\\|WZP\\|Y\\(?:C\\(?:F[IR]\\|[CLOPS]\\)\\|L[45I]\\)\\|Z\\(?:[DM]E\\)\\|[EMPS]\\)\\|D\\(?:A\\(?:DE\\|LI\\|MO\\|TAD?\\)\\|C\\(?:GO\\|UM\\|VS\\)\\|D\\(?:AS\\|EL\\|OP\\)\\|E\\(?:AC\\|FI\\|L[ET]\\|MO\\|RI\\|S[IO]\\|TA\\)\\|F\\(?:LX\\|SW\\)\\|I\\(?:GI?\\|SP\\)\\|J\\(?:DE\\|LI\\)\\|K\\(?:DE\\|LI\\)\\|L\\(?:DE\\|IS\\|LI\\)\\|M\\(?:OV\\|P[EORS]\\)\\|NSO\\|O\\(?:FS?\\|ME\\)\\|S\\(?:CA\\|ET\\|PO\\|U[MR]\\|Y[MS]\\)\\|TRA\\|UMP\\|V\\(?:AL\\|MO\\)\\|YNO\\|[AJKL]\\)\\|E\\(?:AL[IL]\\|BLO\\|CPC\\|DEL\\|EXT\\|G\\(?:EN\\|ID\\)\\|IN[FT]\\|KIL\\|L\\(?:BO\\|EM\\|IS\\)\\|M\\(?:A[GT]\\|FT?\\|I[DS]\\|O[DR]\\|S[EY]\\|TG\\|UN\\)\\|N\\(?:DR\\|ER\\|GE\\|OR\\|SY\\)\\|ORI\\|PLO\\|QSL\\|R\\(?:AS\\|E[AFIS]\\|NO\\|RA\\)\\|S\\(?:CH\\|EL\\|IZ\\|L[ALNV]\\|O[LR]\\|SO\\|TI\\|UR\\|Y[MS]\\)\\|T\\(?:AB\\|C[HO]\\|DE\\|LI\\|YP\\)\\|USO\\|WRI\\|X\\(?:BO\\|OP\\|P\\(?:A[NS]\\|[RS]\\)?\\|T[OR]\\|UN\\)\\|[NT]\\)\\|F\\(?:C\\(?:CH\\|DE\\|LI\\|TY\\|UM\\)\\|DEL\\|E\\(?:BO\\|CO\\|FO\\|SU\\)\\|I\\(?:L\\(?:EAUX[23]\\|LD\\|[EL]\\)\\|NI\\|PL\\|TE\\)\\|J\\(?:DE\\|LI\\)\\|K\\(?:DE\\|LI\\)\\|L\\(?:DA\\|I[ST]\\|O[CT]\\|RE\\|ST\\|U[RX]\\)\\|OR[CM]\\|R\\(?:EQ\\|QS\\)\\|S\\(?:CA\\|S[EP]\\|UM\\)\\|T\\(?:RA\\|YP\\)\\|VME\\|[CJK]\\)\\|G\\(?:A\\(?:P\\(?:FI\\|[FLMOP]\\)?\\|UG\\)\\|C\\(?:[DG]E\\)\\|E\\(?:NO\\|OME?\\)\\|M\\(?:AT\\|FA\\)\\|P\\(?:DE\\|L[IO]\\)?\\|RP\\|S\\(?:BD\\|GD\\|LI\\|SO\\|UM\\)\\)\\|H\\(?:ARF\\|BMA\\|E\\(?:LP\\|MI\\)\\|F\\(?:A[DNR]\\|DE\\|E[IR]\\|MO\\|P\\(?:O[RW]\\|[AC]\\)\\|S[CY]\\)\\|PT[CD]\\|R\\(?:CP\\|EX\\|O[CPU]\\)\\)\\|I\\(?:C\\(?:DE\\|E[DL]?\\|LI\\|RO\\|VF\\)?\\|GES[IO]\\|M\\(?:AG\\|ES\\|ME\\)\\|N\\(?:IS\\|R[ET]\\|T1\\|VO\\)\\|OPT\\|RL[FI]\\)\\|J\\(?:PEG\\|SOL\\)\\|K\\(?:ATT\\|B\\(?:C\\|ET\\)\\|C\\(?:EN\\|LE\\)\\|D\\(?:EL\\|IS\\)\\|E\\(?:EP\\|SI\\|Y[OPW]\\)\\|FIL\\|GEN\\|L\\(?:IS\\)?\\|M\\(?:ES\\|O[DV]\\)\\|NOD\\|P\\(?:LO\\|SC\\)\\|REF\\|S\\(?:C[AO]\\|EL\\|L[LN]\\|[UY]M\\)\\|TRA\\|USE\\|WP[AL]\\)\\|L\\(?:2\\(?:AN\\|TA\\)\\|A\\(?:N[BG]\\|R[CEG]\\|TT\\|Y\\(?:ERP?\\|[LP]\\)\\)\\|C\\(?:A[BS]\\|CA[LT]\\|DE\\|F[AI]\\|LE\\|O[MP]\\|S[ELU]\\|WR\\|ZE\\)\\|D\\(?:EL\\|IV\\|R[AE]\\)\\|E\\(?:SI\\|XT\\)\\|F\\(?:IL\\|SU\\)\\|G\\(?:EN\\|LU\\|WR\\)\\|I\\(?:N\\(?:ES\\|[AELPV]\\)\\|ST\\)\\|LIS\\|MES\\|N\\(?:CO\\|DE\\|FI\\|ME\\|S[PR]\\)\\|O\\(?:CA\\|VL\\)\\|P\\(?:LO\\|RT\\|TN\\)\\|R\\(?:E[FV]\\|OT\\)\\|S\\(?:B[ALVW]\\|CL\\|DE\\|EL\\|L[AKN]\\|OP\\|RE\\|S[CO]\\|TR\\|UM\\|WR\\|YM\\)\\|T\\(?:AN\\|RA\\)\\|UMP\\|VSC\\|WPL\\)\\|M\\(?:A\\(?:CO\\|G[OS]\\|P[2SV]\\|S[CT]\\|TE\\|[PT]\\)\\|CHE\\|D\\(?:AM\\|[EP]L\\)\\|E\\(?:MM\\|SH\\)\\|GEN\\|IDT\\|LIS\\|M\\(?:AS\\|F\\)\\|O\\(?:D[CDEIMOS]\\|NI\\|PT\\|RP\\|VE\\)\\|P\\(?:AM\\|C[HO]\\|D[AER]\\|LI\\|PL\\|R[EI]\\|T[EGR]\\|WR\\)?\\|RPM\\|S\\(?:A[DV]\\|CA\\|DA\\|H[ACKMP]\\|M[AEI]\\|NO\\|OL\\|PR\\|QU\\|RE\\|S[OP]\\|T[EO]\\|VA\\)\\|XPA\\)\\|N\\(?:A\\(?:LL\\|NG\\|XI\\)\\|BLO\\|CNV\\|D\\(?:EL\\|IS\\|SU\\)\\|E\\(?:LE\\|QI\\)\\|FOR\\|GEN\\|KPT\\|L\\(?:AD\\|D[IP]\\|GE\\|HI\\|IS\\|ME\\|O[GP]\\)\\|MOD\\|O\\(?:DE\\|OF\\|R[AL]\\)\\|P\\(?:LO\\|RI\\)\\|R\\(?:E[AF]\\|LS\\|O[PT]\\|RA\\)\\|S\\(?:CA\\|EL\\|L[AEKLV]\\|MO\\|O[LR]\\|TO\\|UB\\|VR\\|YM\\)\\|U\\(?:M[CEMOSV]\\|SO\\)\\|W\\(?:P[AL]\\|RI\\)\\)\\|O\\(?:C\\(?:D[AE]\\|LI\\|RE\\|T[AY]\\|ZO\\)\\|MEG\\|P\\(?:A[DN]\\|CL\\|D[AE]\\|E[QRX]\\|F[AR]\\|GR\\|KE\\|L[FGIOS]\\|MA\\|NC\\|PR\\|R[AEFGS]\\|S[AEUW]\\|TY\\|US\\|VA\\)\\|UT[AGOPR]\\|VCH\\)\\|P\\(?:A\\(?:DE\\|GE\\|PU\\|R[ERS]\\|SA\\|TH\\|US\\)\\|C\\(?:AL\\|GO\\|IR\\|ON\\|RO\\)\\|D\\(?:EF\\|OT\\)\\|E\\(?:MO\\|R[BIT]\\|XC\\)\\|FAC\\|G\\(?:R[AS]\\|S[AE]\\|WR\\)\\|HYS\\|I\\(?:LE\\(?:S[ET]\\|[CDGLMR]\\)\\|[NV]C\\)\\|L\\(?:AS\\|C[AFHIKOP]\\|DI\\|E[ST]\\|F[2AS]\\|GE\\|LS\\|M[AC]\\|N[ES]\\|O[RT]\\|PA[GT]\\|S[CEY]\\|T[DILR]\\|V\\(?:ARO?\\|[EF]\\)\\|WA\\|ZZ\\)\\|M\\(?:AP\\|ET\\|GT\\|L[OS]\\|OP\\)\\|NGR\\|O\\(?:IN\\|LY\\|UT\\|WE\\)\\|P\\(?:AT\\|LO\\|RA\\)\\|R\\(?:A[NS]\\|C[AIOP]\\|E[CDNRST]\\|FA\\|I[2MNST]\\|JS\\|MC\\|N[ELS]\\|O[DR]\\|PA\\|R[FS]\\|S[CEY]\\|TI\\|V\\(?:ARO?\\|E\\)\\)\\|S\\(?:CO\\|D[CFGRSUVW]\\|EL\\|M[AE]\\|OL\\|TR\\|YS\\)\\|T\\(?:R\\|XY\\)\\|VEC\\)\\|Q\\(?:DVA\\|FAC\\|RDO\\|SOP\\|U\\(?:AD\\|OT\\)\\)\\|R\\(?:A\\(?:CE\\|DO\\|LL\\|PP\\|TE\\)\\|BE3\\|C\\(?:ON\\|YC\\)\\|DE[CL]\\|E\\(?:A\\(?:LV\\|[DL]\\)\\|CT\\|ME\\|S\\(?:CO[MN]\\|[EPUVW]\\)\\|ZO\\)\\|F\\(?:IL\\|OR\\)\\|I\\(?:G[IR]\\|TE\\)\\|LIS\\|M\\(?:A[LNS]\\|C[AL]\\|EM\\|FL\\|LV\\|M[LRS]\\|N[DE]\\|O[DR]\\|PO\\|R[EGOPS]\\|S[AM]\\|US\\|XP\\)\\|O\\(?:CK\\|SE\\)\\|P\\(?:OL\\|R[4I]\\|SD\\)\\|S\\(?:ME\\|OP\\|P[EL]\\|T[AMO]\\|UR\\|Y[MS]\\)\\|TIM\\|WFR\\)\\|S\\(?:A\\(?:BS\\|DD\\|LL\\|RP\\|VE\\)\\|BC[LT]\\|COP\\|DEL\\|E\\(?:C[CDFJL-PRSTW]\\|DL\\|EX\\|L[IMT]\\|MI\\|NE\\|OP\\|SY\\|T[FR]\\|XP\\|[DT]\\)\\|F\\(?:A[CDL]\\|BE\\|C[AOU]\\|DE\\|E[DL]\\|FU\\|GR\\|L[DEIL]\\|SC\\|TR\\|[AEL]\\)\\|H\\(?:EL\\|PP\\|SD\\)\\|L\\(?:IS\\|OA\\|[PS]P\\)\\|M\\(?:A[LX]\\|BO\\|CO\\|FO\\|IN\\|OO\\|RT\\|SU\\|UL\\)\\|NOP\\|O\\(?:L\\(?:UO\\|[CUV]\\)\\|RT\\|UR\\)\\|P\\(?:A[CDR]\\|C[NT]\\|DA\\|EC\\|F[RS]\\|GR\\|H[45E]\\|IC\\|L[IO]\\|MW\\|O[IP]\\|RE\\|S[CW]\\|TO\\|UN\\|VA\\)\\|QRT\\|RSS\\|S\\(?:BT\\|LN\\|MT\\|OP\\|P[ABDEM]\\|TA\\|UM\\)\\|T\\(?:A[BT]\\|EF\\|OR\\)\\|U\\(?:B[OS]\\|C[AR]\\|DE\\|EV\\|GE\\|M[AT]\\|P[LR]\\|RE\\|S[AE]\\|VE\\)\\|V\\(?:PL\\|TY\\)\\|W\\(?:AD\\|DE\\|GE\\|LI\\)\\|YNC\\|[EFV]\\)\\|T\\(?:A\\(?:LL\\|RG\\)\\|B\\(?:CO\\|D[AE]\\|EO\\|F[IT]\\|IN\\|L[EI]\\|MO\\|P[LT]\\|TE\\)?\\|CHG\\|H\\(?:EX\\|OP\\)\\|I\\(?:FF\\|M\\(?:ER\\|[EIP]\\)\\|NT\\)\\|O\\(?:CO\\|DE\\|EX\\|F[FR]\\|GR\\|L[IO]\\|P[LR]\\|RU\\|ST\\|T[AY]\\|VA\\)\\|R\\(?:AN\\|EF\\|NO\\|P[DLO]\\|TI\\)\\|S\\(?:HA\\|RE\\)\\|UNI\\|VAR\\|YPE\\|Z\\(?:AM\\|DE\\|EG\\)\\)\\|U\\(?:IMP\\|N\\(?:D[EO]\\|PA\\)\\|P\\(?:CO\\|GE\\)\\|SR[CDE]\\)\\|V\\(?:2DO\\|A\\(?:DD\\|R[DN]\\|TT\\)?\\|C\\(?:LE\\|RO\\|VF\\)\\|D\\(?:DA\\|EL\\|GL\\|OT\\|RA\\)\\|E\\(?:OR\\|XT\\)\\|F\\(?:OP\\|QU\\|SM\\)\\|G\\(?:E[NT]\\|LU\\)\\|I\\(?:MP\\|N[PV]\\)\\|L\\(?:IS\\|SC\\)\\|MES\\|O\\(?:FF\\|LU\\|VL\\)\\|P\\(?:LO\\|TN\\|UT\\)\\|ROT\\|S\\(?:B[AVW]\\|EL\\|LA\\|UM\\|WE\\|YM\\)\\|T\\(?:CL\\|DI\\|EV\\|FR\\|GE\\|IN\\|M[EP]\\|OP\\|PO\\|R[AEFS]\\|S[EFLT]\\|TE\\|VM\\|YP\\)\\)\\|W\\(?:P\\(?:AV\\|CS\\|LA\\|OF\\|RO\\|ST\\)\\|R\\(?:FU\\|ITEM?\\)\\|SPR\\|TBC\\)\\|X\\(?:F\\(?:CR\\|DA\\|EN\\|LI\\)\\|MLO\\|VARO?\\)\\|~\\(?:C\\(?:AT[5I]\\|FI\\)\\|EUI\\|P\\(?:AR\\|RO\\)\\|SAT\\|UGI\\)\\|[AC-FK-NRV]\\)"
"APDL short keyword name regexp.")

(defconst apdl-dynamic-prompt
'("*ABBR - Defines an abbreviation.
*ABBR, Abbr, String" "*AFUN - Specifies units for angular functions in parameter expressions.
*AFUN, Lab" "*ASK - Prompts the user to input a parameter value.
*ASK, Par, Query, DVAL" "*AXPY - Performs the matrix operation M2= v*M1 + w*M2.
*AXPY, vr, vi, M1, wr, wi, M2" "*CFCLOS
*CFCLOS - Closes the \"command\" file." "*CFOPEN - Opens a \"command\" file.
*CFOPEN, Fname, Ext, --, Loc" "*CFWRITE - Writes a Mechanical APDL command (or similar string) to a \"command\" file.
*CFWRITE, Command" "*COMP - Compresses a matrix using a specified algorithm.
*COMP, Matrix, Algorithm, THRESHOLD, Val1, Val2" "*CREATE - Opens (creates) a macro file.
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
*FREE, Name, VAL1" "*GET - Retrieves a value and stores it as a scalar parameter or part of an array parameter.
*GET, Par, Entity, ENTNUM, Item1, IT1NUM, Item2, IT2NUM" "*GO - Causes a specified line on the input file to be read next.
*GO, Base" "*IF - Conditionally causes commands to be read.
*IF, VAL1, Oper1, VAL2, Base1, VAL3, Oper2, VAL4, Base2" "*INIT - Initializes a vector or matrix.
*INIT, Name, Method, Val1, Val2, Val3" "*ITENGINE - Performs a solution using an iterative solver.
*ITENGINE, Type, EngineName, PrecondName, Matrix, RhsVector, SolVector, MaxIter, Toler" "*LIST - Displays the contents of an external, coded file.
*LIST, Fname, Ext, --" "*LSBAC - Performs the solve (forward/backward substitution) of a factorized linear system. 
*LSBAC, EngineName, RhsVector, SolVector, TransKey" "*LSDUMP - Dumps a linear solver engine to a binary File.
*LSDUMP, EngineName, FileName" "*LSENGINE - Creates a linear solver engine.
*LSENGINE, Type, EngineName, Matrix, Option" "*LSFACTOR - Performs the numerical factorization of a linear solver system.
*LSFACTOR, EngineName, Option" "*LSRESTORE - Restores a linear solver engine from a binary file.
*LSRESTORE, EngineName, FileName" "*MERGE - Merges two dense matrices or vectors into one.
*MERGE, Name1, Name2, Val1, Val2" "*MFOURI - Calculates the coefficients for, or evaluates, a Fourier series.
*MFOURI, Oper, COEFF, MODE, ISYM, THETA, CURVE" "*MFUN - Copies or transposes an array parameter matrix.
*MFUN, ParR, Func, Par1" "*MOPER -  Performs matrix operations on array parameter matrices.
*MOPER, ParR, Par1, Oper, Val1, Val2, Val3, Val4, Val5, Val6" "*MSG - Writes an output message via the ANSYS message subroutine.
*MSG, Lab, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8" "*MULT - Performs the matrix multiplication M3 =     M1(T1)*M2(T2).
*MULT, M1, T1, M2, T2, M3" "*MWRITE - Writes a matrix to a file in a formatted sequence. 
*MWRITE, ParR, Fname, Ext, --, Label, n1, n2, n3" "*NRM - Computes the norm of the specified matrix or vector.
*NRM, Name, NormType, ParR, Normalize" "*PRINT - Prints the matrix values to a file.
*PRINT, Matrix, Fname" "*REMOVE - Suppresses rows or columns of a dense matrix or a vector.
*REMOVE, Name, Val1, Val2, Val3" "*RENAME - Renames an existing vector or matrix. 
*RENAME, OldName, NewName," "*REPEAT - Repeats the previous command. 
*REPEAT, NTOT, VINC1, VINC2, VINC3, VINC4, VINC5, VINC6, VINC7, VINC8, VINC9, VINC10, VINC11" "*RETURN - Returns input stream to a higher level.
*RETURN, Level" "*SCAL - Scales a vector or matrix by a constant.
*SCAL, Name, VAL1, VAL2" "*SET - Assigns values to user-named parameters.
*SET, Par, VALUE, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10" "*SMAT - Creates a sparse matrix.
*SMAT, Matrix, Type, Method, Val1, Val2, Val3, Val4 , Val5" "*SORT - Sorts the values of the specified vector.
*SORT, Name, SortType, Val1, Val2" "*SREAD - Reads a file into a string array parameter.
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
*WRK, Num" "*XPL - Accesses content of a Mechanical APDL file.
*XPL, Action, Val1, Val2" "/AN3D - Specifies 3-D annotation functions
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
/COM, Comment" "/CONFIG - Assigns values to Mechanical APDL configuration parameters.
/CONFIG, Lab, VAL" "/CONTOUR - Specifies the uniform contour values on stress displays.
/CONTOUR, WN, NCONT, VMIN, VINC, VMAX" "/COPY - Copies a file.
/COPY, Fname1, Ext1, --, Fname2, Ext2, --, DistKey" "/CPLANE - Specifies the cutting plane for section and capped displays.
/CPLANE, KEY" "/CTYPE - Specifies the type of contour display.
/CTYPE, KEY, DOTD, DOTS, DSHP, TLEN" "/CVAL - Specifies nonuniform contour values on stress displays.
/CVAL, WN, V1, V2, V3, V4, V5, V6, V7, V8" "/CWD - Changes the current working directory.
/CWD, DIRPATH" "/CYCEXPAND - Graphically expands displacements, stresses and strains of a cyclically symmetric model. 
/CYCEXPAND, WN, OPTION, Value1, Value2" "/DELETE - Deletes a file.
/DELETE, Fname, Ext, --, DistKey" "/DEVICE - Controls graphics device options.
/DEVICE, Label, KEY" "/DFLAB - Changes degree-of-freedom labels for user custom elements.
/DFLAB, DOF, DispLab, ForceLab" "/DIRECTORY - Put the file names in the current directory into a string parameter array.
/DIRECTORY, StrArray, FileName, Ext, Dir" "/DIST - Specifies the viewing distance for magnifications and perspective.
/DIST, WN, DVAL, KFACT" "/DSCALE - Sets the displacement multiplier for displacement displays.
/DSCALE, WN, DMULT" "/DV3D - Sets 3-D device option modes.
/DV3D, Lab, Key" "/EDGE - Displays only the common lines (&#8220;edges&#8221;) of an object.
/EDGE, WN, KEY, ANGLE" "/EFACET - Specifies the number of facets per element edge for PowerGraphics displays.
/EFACET, NUM" "/EOF
/EOF - Exits the file being read." "/ERASE
/ERASE - Specifies that the screen is to be erased before each display." "/ESHAPE - Displays elements with shapes determined from the real constants,       section definition, or other inputs.
/ESHAPE, SCALE, KEY" "/EXIT - Stops the run and returns control to the system.
/EXIT, Slab, Fname, Ext, --" "/EXPAND - Allows the creation of a larger graphic display than represented by the actual finite element analysis model.
/EXPAND, Nrepeat1, Type1, Method1, DX1, DY1, DZ1, Nrepeat2, Type2, Method2, DX2, DY2, DZ2, Nrepeat3, Type3, Method3, DX3, DY3, DZ3" "/FACET - Specifies the facet representation used to form solid model displays.
/FACET, Lab" "/FCOMP - Specifies file-compression options.
/FCOMP, Ident, LEVEL" "/FDELE - Deletes a binary file after it is used.
/FDELE, Ident, Stat" "/FILNAME - Changes the Jobname for the analysis.
/FILNAME, Fname, Key" "/FOCUS - Specifies the focus point (center of the window).
/FOCUS, WN, XF, YF, ZF, KTRANS" "/FORMAT - Specifies format controls for tables.
/FORMAT, NDIGIT, Ftype, NWIDTH, DSIGNF, LINE, CHAR, EXPTYPE" "/GCMD - Controls the type of element or graph display used for the GPLOT command.
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
/GST, Lab" "/GTHK - Sets line thicknesses for graph lines.
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
/LSYMBOL, X, Y, SYMANG, SYMTYP, SYMSIZ, KEYBMP" "/MAP
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
/PCIRCLE, XCENTR, YCENTR, XLRAD" "/PLOPTS - Controls graphics options on subsequent displays.
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
/SHOW, Fname, Option, VECT, NCPL" "/SHRINK - Shrinks elements, lines, areas, and volumes for display clarity.
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
ADAMS, NMODES, KSTRESS, KSHELL" "ADD - Adds (sums) variables.
ADD, IR, IA, IB, IC, Name, --, --, FACTA, FACTB, FACTC" "ADDAM - Specifies the acceleration spectrum computation constants for the analysis of shock resistance of shipboard structures.
ADDAM, AF, AA, AB, AC, AD, AMIN" "ADELE - Deletes unmeshed areas.
ADELE, NA1, NA2, NINC, KSWP" "ADGL - Lists keypoints of an area that lie on a parametric degeneracy.
ADGL, NA1, NA2, NINC" "ADRAG - Generates areas by dragging a line pattern along a path.
ADRAG, NL1, NL2, NL3, NL4, NL5, NL6, NLP1, NLP2, NLP3, NLP4, NLP5, NLP6" "AEROCOEFF - Computes the aero-damping and stiffness coefficients and writes them to an APDL       array.
AEROCOEFF, AeroModeType, AeroMappedFileNames, AeroSpecs, AeroScalar, nBlades, AutoFileRead" "AESIZE - Specifies the element size to be meshed onto areas.
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
AMAP, AREA, KP1, KP2, KP3, KP4" "AMBEAM - For multiple-beam printers, specifies the number of beams in an additive manufacturing analysis.
AMBEAM, NUMBEAMS" "AMBUILD - Specifies printer parameters for the build and other options in an additive manufacturing analysis.
AMBUILD, Option, VAL1, VAL2" "AMENV - Specifies the build-environment thermal boundary conditions in an additive manufacturing analysis.
AMENV, TGAS, HGAS" "AMESH - Generates nodes and area elements within areas.
AMESH, NA1, NA2, NINC" "AMMAT - Specifies the melting and relaxation temperatures of the build material in an additive manufacturing analysis.
AMMAT, MATPART, TMELT, TRELAX" "AMPOWDER - Specifies the thermal conditions of the powder in an additive manufacturing analysis.
AMPOWDER, TPOWDER, HPOWDER, MATFACTOR" "AMSTEP - Specifies the build-sequence steps in an additive     manufacturing analysis.
AMSTEP, Sequence, VAL1, VAL2, VAL3, VAL4" "AMSUPPORTS - Specifies information about the supports in an additive     manufacturing analysis.
AMSUPPORTS, NSUPPORTS, CompName, SectArray" "AMTYPE - Specifies the printing process in an additive     manufacturing analysis.
AMTYPE, Process" "ANCNTR - Produces an animated sequence of a contoured deformed shape.
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
ANMODE, NFRAM, DELAY, NCYCL, KACCEL" "ANORM - Reorients area normals.
ANORM, ANUM, NOEFLIP" "ANPRES - Produces an animated sequence of the time-harmonic pressure variation of an engine-order excitation in a cyclic harmonic analysis.
ANPRES, NFRAM, DELAY, NCYCL, RefFrame" "ANSOL - Specifies averaged nodal data to be stored from the results file in the solution coordinate system. 
ANSOL, NVAR, NODE, Item, Comp, Name, Mat, Real, Ename" "ANSTOAQWA - Creates an AQWA-LINE input file from the current Mechanical APDL model.
ANSTOAQWA, Fname, VertAxis, Gc, Rho, HWL, DiffKey, SymxKey, SymyKey" "ANSTOASAS - Creates an ASAS input file from the current ANSYS model.
ANSTOASAS, Fname, KEY" "ANTIME - Generates a sequential contour animation over a range of time.
ANTIME, NFRAM, DELAY, NCYCL, AUTOCNTRKY, RSLTDAT, MIN, MAX" "ANTYPE - Specifies the analysis type and restart status.
ANTYPE, Antype, Status, LDSTEP, SUBSTEP, Action, --, PRELP" "AOFFST - Generates an area, offset from a given area.
AOFFST, NAREA, DIST, KINC" "AOVLAP - Overlaps areas.
AOVLAP, NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9" "APLOT - Displays the selected areas.
APLOT, NA1, NA2, NINC, DEGEN, SCALE" "APORT - Specifies input data for plane wave and acoustic duct ports.
APORT, PortNum, Label, KCN, PRES, PHASE, --, VAL1, VAL2, VAL3, VAL4" "APPEND - Reads data from the results file and appends it to the database.
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
ASLV, Type" "ASOL - Specifies the acoustic solver with scattered field    formulation.
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
BFDELE, NODE, Lab" "BFE - Defines an element body-force load.
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
CBMD, ROW, C(R)(R) , C(R)(R+1) , C(R)(R+2) , C(R)(R+3) , C(R)(R+4) , C(R)(R+5)" "CBMX - Specifies preintegrated cross-section stiffness for composite beam sections.
CBMX, ROW, S(R)(R) , S(R)(R+1) , S(R)(R+2) , S(R)(R+3) , S(R)(R+4) , S(R)(R+5) , S(R)(R+6)" "CBTE - Specifies a thermal expansion coefficient for a composite beam section. 
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
CGROW, Action, Par1, Par2, Par3" "CHECK - Checks current database items for completeness.
CHECK, Sele, Levl" "CHKMSH - Checks area and volume entities for previous meshes.
CHKMSH, Comp" "CINT - Defines parameters associated with fracture-parameter calculations
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
CMPLOT, Label, Entity, Keyword" "CMROTATE - Specifies the rotational velocity of an element component in a brake-squeal       analysis.
CMROTATE, CM_Name, ROTATX, ROTATY, ROTATZ, X1, Y1, Z1, X2, Y2, Z2" "CMSEL - Selects a subset of components and assemblies.
CMSEL, Type, Name, Entity" "CMSFILE - Specifies a list of component mode synthesis (CMS) results files for plotting results on the assembly.
CMSFILE, Option, Fname, Ext, CmsKey" "CMSOPT - Specifies component mode synthesis (CMS) analysis options.
CMSOPT, CMSMETH, NMODE, FREQB, FREQE, FBDDEF, FBDVAL, IOKEY, --, --, ELCALC, --, EIGMETH, nStartVN" "CMWRITE - Writes node and element components and assemblies to a file.
CMWRITE, Fname, Ext, --, --, Fmat" "CNCHECK - Provides and/or adjusts the initial status of contact pairs.
CNCHECK, Option, RID1, RID2, RINC, InterType, TRlevel, CGAP, CPEN, IOFF" "CNKMOD - Modifies contact element key options.
CNKMOD, ITYPE, KNUM, VALUE" "CNTR - Redirects contact pair output quantities to a text file.
CNTR, Option, Key" "CNVTOL - Sets convergence values for nonlinear analyses.
CNVTOL, Lab, VALUE, TOLER, NORM, MINREF" "COMBINE - Combines distributed memory parallel (Distributed ANSYS) files.
COMBINE, FileType, NUM" "COMPRESS
COMPRESS - Deletes all specified sets." "CON4 - Creates a conical volume anywhere on the working plane.
CON4, XCENTER, YCENTER, RAD1, RAD2, DEPTH" "CONE - Creates a conical volume centered about the working plane origin.
CONE, RBOT, RTOP, Z1, Z2, THETA1, THETA2" "CONJUG - Forms the complex conjugate of a variable.
CONJUG, IR, IA, --, --, Name, --, --, FACTA" "CORIOLIS - Applies the Coriolis effect to a rotating structure.
CORIOLIS, Option, --, --, RefFrame, RotDamp, RotMass" "COUPLE
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
CQC, SIGNIF, Label, , ForceType" "CS - Defines a local coordinate system by three node locations.
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
CYCCALC, FilePrefix, FileFormat, Separator" "CYCFILES - Specifies the data files where results are to be found for a cyclic symmetry mode-superposition harmonic analysis.
CYCFILES, FnameRst, ExtRst, FnameRfrq, ExtRfrq" "CYCFREQ - Specifies solution options for a cyclic symmetry mode-superposition harmonic analysis.
CYCFREQ, Option, Value1, Value2, Value3, Value4, Value5" "CYCLIC - Specifies a cyclic symmetry analysis.
CYCLIC, NSECTOR, ANGLE, KCN , Name, USRCOMP, USRNMAP" "CYCOPT - Specifies solution options for a cyclic symmetry analysis.
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
DDOPTION, Decomp, NProcPerSol" "DEACT
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
DMPEXT, SMODE, TMODE, Dmpname, Freqb, Freqe, NSTEPS" "DMPOPTION - Specifies distributed memory parallel (Distributed ANSYS) file combination             options.
DMPOPTION, FileType, Combine, ResCombFreq" "DMPRAT - Sets a modal damping ratio.
DMPRAT, RATIO" "DMPSTR - Sets constant structural damping data.
DMPSTR, COEFF, DMPSFreqTab" "DNSOL - Defines or modifies solution results at a node.
DNSOL, NODE, Item, Comp, V1, V2, V3, V4, V5, V6" "DOF - Adds degrees of freedom to the current DOF set.
DOF, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6, Lab7, Lab8, Lab9, Lab10" "DOFSEL - Selects a DOF label set for reference by other commands.
DOFSEL, Type, Dof1, Dof2, Dof3, Dof4, Dof5, Dof6" "DOMEGA - Specifies the rotational acceleration of the structure.
DOMEGA, DOMGX, DOMGY, DOMGZ" "DSCALE - Scales DOF constraint values.
DSCALE, RFACT, IFACT, TBASE" "DSET - Sets the scale and drawing plane orientation for a digitizing tablet.
DSET, NODE1, NODE2, NODE3, DDEV" "DSPOPTION - Sets memory option for the sparse solver.
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
EALIVE, ELEM" "ECPCHG
ECPCHG - Optimizes degree-of-freedom usage in a coupled acoustic model." "EDELE - Deletes selected elements from the model.
EDELE, IEL1, IEL2, INC" "EEXTRUDE - Extrudes 2-D plane elements into 3-D solids during a 2-D to 3-D    analysis.
EEXTRUDE, Action, NELEM, SPACE, DIST, THETA, TFACT , –, BCKEY" "EGEN - Generates elements from an existing pattern.
EGEN, ITIME, NINC, IEL1, IEL2, IEINC, MINC, TINC, RINC, CINC, SINC, DX, DY, DZ" "EGID - Specifies a global identifier for a set of MESH200    elements.
EGID, VAL" "EINFIN - Generates structural infinite elements from selected nodes.
EINFIN, CompName, PNODE(NREF1), NREF2, NREF3, MATID" "EINTF - Defines two-node elements between coincident or offset nodes.
EINTF, TOLER, K, TLAB, KCN, DX, DY, DZ, KNONROT" "EKILL - Deactivates an element (for the birth and death capability).
EKILL, ELEM" "ELBOW - Specifies degrees of freedom to be coupled for end release and applies section constraints to elbow elements.
ELBOW, Transkey, TOL, Dof, Cons1, Cons2, Cons3, Cons4" "ELEM
ELEM - Specifies \"Elements\" as the subsequent status topic." "ELIST - Lists the elements and their attributes.
ELIST, IEL1, IEL2, INC, NNKEY, RKEY" "EMAGERR
EMAGERR - Calculates the relative error in an electrostatic or electromagnetic field analysis." "EMATWRITE - Forces the writing of all the element matrices to File.EMAT.
EMATWRITE, Key" "EMF
EMF - Calculates the electromotive force (emf), or voltage drop along a predefined path." "EMFT
EMFT - Summarizes electromagnetic forces and torques." "EMID - Adds or removes midside nodes.
EMID, Key, Edges" "EMIS - Specifies emissivity as a material property for the Radiation Matrix method.
EMIS, MAT, EVALU" "EMODIF - Modifies a previously defined element.
EMODIF, IEL, STLOC, I1, I2, I3, I4, I5, I6, I7, I8" "EMORE - Adds more nodes to the just-defined element.
EMORE, Q, R, S, T, U, V, W, X" "EMSEL - Selects a group of reinforcing members via a    predefined global identifier.
EMSEL, Type,--, --, VMIN, VMAX, VINC" "EMSYM - Specifies circular symmetry for electromagnetic sources.
EMSYM, NSECT" "EMTGEN - Generates a set of TRANS126 elements.
EMTGEN, Ncomp, Ecomp, PNcomp, DOF, GAP, GAPMIN, FKN, EPZERO, --, Smethod" "EMUNIT - Specifies the system of units for magnetic field problems.
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
EREFINE, NE1, NE2, NINC, LEVEL, DEPTH, POST, RETAIN" "EREINF - Generates reinforcing elements from selected existing (base) elements.
EREINF, KOffALim" "ERESX - Specifies extrapolation of integration point results.
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
EWRITE, Fname, Ext, --, KAPPND, Format" "EXBOPT - Specifies .EXB file output options in a CMS generation    pass.
EXBOPT, OUTINV2, OUTTCMS, OUTSUB, OUTCMS, OUTCOMP, OUTRM, NOINV, OUTELE" "EXOPTION - Specifies the EXPROFILE options for the Mechanical APDL to       ANSYS CFX profile file transfer.
EXOPTION, Ldtype, Option, VALUE" "EXP - Forms the exponential of a variable.
EXP, IR, IA, --, --, Name, --, --, FACTA, FACTB" "EXPAND - Displays the results of a modal cyclic symmetry analysis.
EXPAND, Nrepeat, MODAL, HIndex, Icsys, SctAng, --, Phase" "EXPASS - Specifies an expansion pass of an analysis.
EXPASS, Key, --, --, KeyStat" "EXPROFILE - Exports Mechanical APDL interface data on selected nodes to an ANSYS CFX Profile       file.
EXPROFILE, Ldtype, Load, VALUE, Pname, Fname, Fext, Fdir" "EXPSOL - Specifies the solution to be expanded for mode-superposition analyses or substructure analyses.
EXPSOL, LSTEP, SBSTEP, TIMFRQ, Elcalc" "EXTOPT - Controls options relating to the generation of volume elements from area elements.
EXTOPT, Lab, Val1, Val2, Val3, Val4" "EXTREM - Lists the extreme values for variables.
EXTREM, NVAR1, NVAR2, NINC" "EXUNIT - Specifies the interface data unit labels to be written to the profile file from    Mechanical APDL to ANSYS CFX transfer.
EXUNIT, Ldtype, Load, Untype, Name" "F - Specifies force loads at nodes.
F, NODE, Lab, VALUE, VALUE2, NEND, NINC" "FC - Provides failure criteria information and activates a data table to input temperature-dependent stress and strain limits.
FC, MAT, Lab1, Lab2, DATA1, DATA2, DATA3, DATA4, DATA5, DATA6" "FCCHECK
FCCHECK -  Checks both the strain and stress input criteria for all materials." "FCDELE - Deletes previously defined failure criterion data for the given material.
FCDELE, MAT" "FCLIST - To list what the failure criteria is that you have input.
FCLIST, MAT, --, TEMP" "FCTYP - Activates or removes failure-criteria types for postprocessing. 
FCTYP, Oper, Lab" "FCUM - Specifies that force loads are to be accumulated.
FCUM, Oper, RFACT, IFACT" "FDELE - Deletes force loads on nodes.
FDELE, NODE, Lab, NEND, NINC, Lkey" "FEBODY
FEBODY - Specifies \"Body loads on elements\" as the subsequent status topic." "FECONS
FECONS - Specifies \"Constraints on nodes\" as the subsequent status topic." "FEFOR
FEFOR - Specifies \"Forces on nodes\" as the subsequent status topic." "FESURF
FESURF - Specifies \"Surface loads on elements\" as the subsequent status topic." "FILE - Specifies the data file where results are to be found.
FILE, Fname, Ext, --" "FILEAUX2 - Specifies the binary file to be dumped.
FILEAUX2, Fname, Ident, --" "FILEAUX3 - Specifies the results file to be edited.
FILEAUX3, Fname, Ext, --" "FILL - Generates a line of nodes between two existing nodes.
FILL, NODE1, NODE2, NFILL, NSTRT, NINC, ITIME, INC, SPACE" "FILLDATA - Fills a variable by a ramp function.
FILLDATA, IR, LSTRT, LSTOP, LINC, VALUE, DVAL" "FINISH
FINISH - Exits normally from a processor." "FITEM - Identifies items chosen by a picking operation (GUI).
FITEM, NFIELD, ITEM, ITEMY, ITEMZ" "FJ - Specify forces or moments on the components of the relative motion of a joint element.
FJ, ELEM, LABEL, VALUE" "FJDELE - Deletes forces (or moments) on the components of the relative motion of a joint element.
FJDELE, ELEM, LAB" "FJLIST - Lists forces and moments applied on joint elements.
FJLIST, Elem" "FK - Defines force loads at keypoints.
FK, KPOI, Lab, VALUE, VALUE2" "FKDELE - Deletes force loads at a keypoint.
FKDELE, KPOI, Lab" "FKLIST - Lists the forces at keypoints.
FKLIST, KPOI, Lab" "FLIST - Lists force loads on the nodes.
FLIST, NODE1, NODE2, NINC" "FLST - Specifies data required for a picking operation (GUI).
FLST, NFIELD, NARG, TYPE, Otype, LENG" "FLUREAD - Reads one-way Fluent-to-Mechanical APDL coupling data via a .cgns file with one-side fast Fourier transformation complex pressure peak value.
FLUREAD, --, Fname, Ext, KDIM, KOUT, LIMIT, ListOpt" "FLUXV
FLUXV - Calculates the flux passing through a closed contour." "FORCE - Selects the element nodal force type for output.
FORCE, Lab" "FORM - Specifies the format of the file dump.
FORM, Lab" "FREQ - Defines the frequency points for the SV vs. FREQ tables.
FREQ, FREQ1, FREQ2, FREQ3, FREQ4, FREQ5, FREQ6, FREQ7, FREQ8, FREQ9" "FRQSCL - Turns on automatic scaling of the entire mass matrix and    frequency range for modal analyses.
FRQSCL, Scaling" "FSCALE - Scales force load values in the database.
FSCALE, RFACT, IFACT" "FSSECT - Calculates and stores total linearized stress components.
FSSECT, RHO, NEV, NLOD, KBR" "FSSPARM - Calculates reflection and transmission properties of a frequency selective surface.
FSSPARM, PORT1, PORT2" "FSUM - Sums the nodal force and moment contributions of elements.
FSUM, LAB, ITEM" "FTRAN
FTRAN - Transfers solid model forces to the finite element model." "FTYPE - Specifies the file type and pressure type for the subsequent import of source points and pressures.
FTYPE, FileType, PresType" "FVMESH - Generates nodes and tetrahedral volume elements from detached exterior area elements (facets).
FVMESH, KEEP" "GAP
GAP - Specifies \"mode-superposition transient gap conditions\" as the subsequent status topic." "GAPF - Defines the gap force data to be stored in a variable.
GAPF, NVAR, NUM, Name" "GAUGE - Gauges the problem domain for a magnetic edge-element formulation.
GAUGE, Opt, FREQ" "GCDEF - Defines interface interactions between general contact surfaces.
GCDEF, Option, SECT1, SECT2, MATID, REALID, SECT1END, SECT2END" "GCGEN - Creates contact elements for general contact.
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
HELP, Name" "HEMIOPT - Specifies options for Hemicube view factor calculation.
HEMIOPT, HRES" "HFANG - Defines or displays spatial angles of a spherical radiation surface for sound radiation parameter calculations.
HFANG, Lab, PHI1, PHI2, THETA1, THETA2" "HFSYM - Indicates the presence of symmetry planes for the computation of acoustic fields in the near and far field domains (beyond the finite element region).
HFSYM, KCN, Xkey, Ykey, Zkey" "HPTCREATE - Defines a hard point.
HPTCREATE, TYPE, ENTITY, NHP, LABEL, VAL1, VAL2, VAL3" "HPTDELETE - Deletes selected hardpoints.
HPTDELETE, NP1, NP2, NINC" "HRCPLX - Computes and stores in the database the time-harmonic solution at a prescribed phase angle.
HRCPLX, LOADSTEP, SUBSTEP, OMEGAT, 1STLCASE, 2NDLCASE" "HREXP - Specifies the phase angle for the harmonic analysis expansion pass.
HREXP, ANGLE" "HROCEAN - Perform the harmonic ocean wave procedure (HOWP).
HROCEAN, Type, NPHASE" "HROPT - Specifies harmonic analysis options.
HROPT, Method, MAXMODE, MINMODE, MCout, Damp" "HROUT - Specifies the harmonic analysis output options.
HROUT, Reimky, Clust, Mcont, EngCalc" "IC - Specifies initial conditions at nodes.
IC, NODE, Lab, VALUE, VALUE2,NEND, NINC" "ICDELE
ICDELE - Deletes initial conditions at nodes." "ICLIST - Lists the initial conditions.
ICLIST, NODE1, NODE2, NINC, Lab" "ICROTATE - Specifies initial velocity at nodes as a sum of rotation about an axis and             translation.
ICROTATE, NODE, OMEGA, X1, Y1, Z1, X2, Y2, Z2, Vx, Vy, Vz, ACCEL" "IGESIN - Transfers IGES data from a file into ANSYS.
IGESIN, Fname, Ext, --" "IGESOUT - Writes solid model data to a file in IGES Version 5.1 format.
IGESOUT, Fname, Ext, --, ATT" "IMAGIN - Forms an imaginary variable from a complex variable.
IMAGIN, IR, IA, --, --, Name, --, --, FACTA" "IMESH - Generates nodes and interface elements along lines or areas.
IMESH, LAKY, NSLA, NTLA, KCN, DX, DY, DZ, TOL" "IMMED - Allows immediate display of a model as it is generated.
IMMED, KEY" "INISTATE - Defines initial-state data and parameters.
INISTATE, Action, Val1, Val2, Val3, Val4, Val5, Val6, Val7, Val8, Val9" "INRES - Identifies the data to be retrieved from the results file.
INRES, Item1, Item2, Item3, Item4, Item5, Item6, Item7, Item8" "INRTIA
INRTIA - Specifies \"Inertial loads\" as the subsequent status topic." "INT1 - Integrates a variable.
INT1, IR, IY, IX, --, Name, --, --, FACTA, FACTB, CONST" "INVOPT - Enables or disables inverse solving for    the current load step.
INVOPT, Option" "IOPTN - Controls options relating to importing a model.
IOPTN, Lab, VAL1" "IRLF - Specifies that inertia relief calculations are to be performed.
IRLF, KEY" "IRLIST
IRLIST - Prints inertia relief summary table." "JPEG - Provides JPEG file export for ANSYS displays. 
JPEG, Kywrd, OPT" "JSOL - Specifies result items to be stored for the joint element.
JSOL, NVAR, ELEM, ITEM, COMP, Name" "K - Defines a keypoint.
K, NPT, X, Y, Z" "KATT - Associates attributes with the selected, unmeshed keypoints.
KATT, MAT, REAL, TYPE, ESYS" "KBC - Specifies ramped or stepped loading within a load step.
KBC, KEY, OMGSQRDKEY" "KBETW - Creates a keypoint between two existing keypoints.
KBETW, KP1, KP2, KPNEW, Type, VALUE" "KCENTER - Creates a keypoint at the center of a circular arc defined by three locations.
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
KTRAN, KCNTO, NP1, NP2, NINC, KINC, NOELEM, IMOVE" "KUSE - Specifies whether or not to reuse factorized          matrices.
KUSE, KEY" "KWPAVE - Moves the working plane origin to the average location of keypoints.
KWPAVE, P1, P2, P3, P4, P5, P6, P7, P8, P9" "KWPLAN - Defines the working plane using three keypoints.
KWPLAN, WN, KORIG, KXAX, KPLAN" "L - Defines a line between two keypoints.
L, P1, P2, NDIV, SPACE, XV1, YV1, ZV1, XV2, YV2, ZV2" "L2ANG - Generates a line at an angle with two existing lines.
L2ANG, NL1, NL2, ANG1, ANG2, PHIT1, PHIT2" "L2TAN - Generates a line tangent to two lines.
L2TAN, NL1, NL2" "LANBOPTION - Specifies Block Lanczos eigensolver options.
LANBOPTION, StrmCk" "LANG - Generates a straight line at an angle with a line.
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
LCOPER, Oper, LCASE1, Oper2, LCASE2,SweepANG" "LCSEL - Selects a subset of load cases.
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
LLIST, NL1, NL2, NINC, Lab" "LMESH - Generates nodes and line elements along lines.
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
LUMPM, Key, --, KeyElt" "LVSCALE - Scales the load vector for mode-superposition analyses.
LVSCALE, FACT, LDSTEP" "LWPLAN - Defines the working plane normal to a location on a line.
LWPLAN, WN, NL1, RATIO" "M - Defines master degrees of freedom for superelement generation analyses.
M, NODE, Lab1, NEND, NINC, Lab2, Lab3, Lab4, Lab5, Lab6" "MACOPT - Specifies modal assurance criterion (MAC) calculation options for     RSTMAC.
MACOPT, Option, Value1" "MAGOPT - Specifies options for a 3-D magnetostatic field analysis.
MAGOPT, Value" "MAGSOLV - Specifies magnetic solution options and initiates the solution.
MAGSOLV, OPT, NRAMP, CNVCSG, CNVFLUX, NEQIT, BIOT,CNVTOL" "MAP - Maps pressures from source points to target surface elements.
MAP, --, kDIM, --, kOUT, LIMIT" "MAP2DTO3D - Initiates a 2-D to 3-D analysis and maps variables.
MAP2DTO3D, Action, VALUE1, VALUE2" "MAPSOLVE - Maps solved node and element solutions from an original mesh to a new mesh.
MAPSOLVE, MAXSBSTEP" "MAPVAR - Defines tensors and vectors in user-defined state variables for rezoning and in 2-D to 3-D analyses.
MAPVAR, Option, MatId, IstrtStress, nTenStress, IstrtStrain, nTenStrain, , IstrtVect, nVect" "MASCALE - Activates scaling of the entire system matrix.
MASCALE, massFact" "MASTER
MASTER - Specifies \"Master DOF\" as the subsequent status topic." "MAT - Sets the element material attribute pointer.
MAT, MAT" "MATER
MATER - Specifies \"Material properties\" as the subsequent status topic." "MCHECK - Checks mesh connectivity.
MCHECK, Lab" "MDAMP - Defines the damping ratios as a function of mode.
MDAMP, STLOC, V1, V2, V3, V4, V5, V6" "MDELE - Deletes master degrees of freedom.
MDELE, NODE, Lab1, NEND, NINC, Lab2, Lab3, Lab4, Lab5, Lab6" "MDPLOT - Plots frequency-dependent modal damping coefficients calculated by DMPEXT.
MDPLOT, Function, Dmpname, Scale" "MEMM - Allows the current session to keep allocated memory 
MEMM, Lab, Kywrd" "MESHING
MESHING - Specifies \"Meshing\" as the subsequent status topic." "MGEN - Generates additional MDOF from a previously defined set.
MGEN, ITIME, INC, NODE1, NODE2, NINC" "MIDTOL - Sets midstep residual criterion values for structural transient analyses.
MIDTOL, KEY, TOLERB, RESFQ" "MLIST - Lists the MDOF of freedom.
MLIST, NODE1, NODE2, NINC" "MMASS - Specifies the missing mass response calculation.
MMASS, Option, ZPA" "MMF
MMF - Calculates the magnetomotive force along a path." "MODCONT - Specify additional modal analysis options. 
MODCONT, MLSkey, EnforcedKey, --, FastLV" "MODDIR - Enables remote read-only usage of modal analysis files or             substructuring analysis files.
MODDIR, Key, Directory, Fname" "MODE - Specifies the harmonic loading term for this load step.
MODE, MODE, ISYM" "MODIFY - Changes the listed values of the data in a set.
MODIFY, SET, LSTEP, ITER, CUMIT, TIME, Ktitle" "MODMSH - Controls the relationship of the solid model and the FE model.
MODMSH, Lab" "MODOPT - Specifies modal analysis options.
MODOPT, Method, NMODE, FREQB, FREQE, Cpxmod, Nrmkey, ModType, BlockSize, --, --, --, FREQMOD" "MODSELOPTION - Specifies the criteria for selecting the modes to be expanded.
MODSELOPTION, dir1, dir2, dir3, dir4, dir5, dir6" "MONITOR - Controls contents of variable fields in the nonlinear solution       monitor file.
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
MPWRITE, Fname, Ext, --, LIB, MAT" "MRPM - Defines the revolutions per minute (RPM) for a machine rotation.
MRPM, VAL1" "MSAVE - Sets the solver memory saving option. This option only applies to the PCG solver (including PCG Lanczos).
MSAVE, Key" "MSHAPE - For elements that support multiple shapes, specifies the element shape to be used for meshing.
MSHAPE, KEY, Dimension" "MSHCOPY - Simplifies the generation of meshes that have matching node element patterns on two different line groups (in 2-D) or area groups (3-D).
MSHCOPY, KEYLA, LAPTRN, LACOPY, KCN, DX, DY, DZ, TOL, LOW, HIGH" "MSHKEY - Specifies whether free meshing or mapped meshing should be used to mesh a model.
MSHKEY, KEY" "MSHMID - Specifies placement of midside nodes.
MSHMID, KEY" "MSHPATTERN - Specifies pattern to be used for mapped triangle meshing.
MSHPATTERN, KEY" "MSOLVE - Starts multiple solutions for an acoustic analysis.
MSOLVE, NUMSLV, VAL1, VAL2, Lab, ANGFIX" "MSTOLE - Adds two extra nodes from FLUID116 elements to SURF151 or SURF152 elements for convection analyses.
MSTOLE, METHOD, Namesurf, Namefluid" "MXPAND - Specifies modal or buckling analysis expansion                   options.
MXPAND, NMODE, FREQB, FREQE, Elcalc, SIGNIF, MSUPkey, ModeSelMethod, EngCalc" "N - Defines a node.
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
NLGEOM, Key" "NLHIST - Specify results items to track during solution.
NLHIST, Key, Name, Item, Comp, NODE, ELEM, SHELL, LAYER, STOP_VALUE, STOP_COND" "NLIST - Lists nodes.
NLIST, NODE1, NODE2, NINC, Lcoord, SORT1, SORT2, SORT3, KINTERNAL" "NLMESH - Controls remeshing in nonlinear adaptivity.
NLMESH, Control, VAL1, VAL2" "NLOG - Forms the natural log of a variable.
NLOG, IR, IA, --, --, Name, --, --, FACTA, FACTB" "NLOPT
NLOPT - Specifies \"Nonlinear analysis options\" as the subsequent status topic." "NMODIF - Modifies an existing node.
NMODIF, NODE, X, Y, Z, THXY, THYZ, THZX" "NODES
NODES - Specifies \"Nodes\" as the subsequent status topic." "NOOFFSET - Prevents the CDREAD command from offsetting specified data items
NOOFFSET, Label" "NORA - Rotates nodal coordinate systems to surface normal
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
NUMOFF, Label, VALUE, KREF" "NUMSTR - Establishes starting numbers for automatically numbered items.
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
OUTPR, Item, Freq, Cname" "OUTRES - Controls the solution-result data written to the database.
OUTRES, Item, Freq, Cname, -- , NSVAR, DSUBres" "OVCHECK - Checks for overconstraint among constraint equations and Lagrange multipliers.
OVCHECK, Method, Frequency, Set" "PADELE - Deletes a defined path.
PADELE, DELOPT" "PAGET - Writes current path information into an array variable.
PAGET, PARRAY, POPT" "PAPUT - Retrieves path information from an array variable.
PAPUT, PARRAY, POPT" "PARESU - Restores previously saved paths from a file.
PARESU, Lab, Fname, Ext, --" "PARRES - Reads parameters from a file.
PARRES, Lab, Fname, Ext, --" "PARSAV - Writes parameters to a file.
PARSAV, Lab, Fname, Ext, --" "PASAVE - Saves selected paths to an external file.
PASAVE, Lab, Fname, Ext, --" "PATH - Defines a path name and establishes parameters for the path.
PATH, NAME, nPts, nSets, nDiv" "PAUSE
PAUSE - Temporarily releases the current product license." "PCALC - Forms additional labeled path items by operating on existing path items.
PCALC, Oper, LabR, Lab1, Lab2, FACT1, FACT2, CONST" "PCGOPT - Controls PCG solver options.
PCGOPT, Lev_Diff , --, ReduceIO, StrmCk, Wrtfull, Memory, LM_Key" "PCIRC - Creates a circular area centered about the working plane origin.
PCIRC, RAD1, RAD2, THETA1, THETA2" "PCROSS - Calculates the cross product of two path vectors along the current path.
PCROSS, LabXR, LabYR, LabZR, LabX1, LabY1, LabZ1, LabX2, LabY2, LabZ2" "PDEF - Interpolates an item onto a path.
PDEF, Lab, Item, Comp, Avglab" "PDOT - Calculates the dot product of two path vectors along the current path.
PDOT, LabR, LabX1, LabY1, LabZ1, LabX2, LabY2, LabZ2" "PERBC2D - Generates periodic constraints for 2-D planar magnetic field analyses.
PERBC2D, LOC1, LOC2, LOCTOL, R1, R2, TOLR, OPT, PLNOPT" "PERTURB - Sets linear perturbation analysis options.
PERTURB, Type, MatKey, ContKey, LoadControl" "PFACT - Calculates participation factors for the PSD or multi-point response spectrum table.
PFACT, TBLNO, Excit, Parcor" "PHYSICS - Writes, reads, or lists all element information
PHYSICS, Option, Title, Fname, Ext, --" "PIVCHECK - Controls the behavior of an analysis when a negative or zero equation solver pivot value is encountered.
PIVCHECK, KEY, PRNTCNTRL" "PLAS - Plots a specified acoustic quantity during postprocessing of an acoustic    analysis.
PLAS, Lab, LDSTEP, SUBSTEP, FREQB, FREQE, LogOpt, PlotType, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6" "PLCAMP - Plots Campbell diagram data for applications involving rotating structure dynamics.
PLCAMP, Option, SLOPE, UNIT, FREQB, Cname, STABVAL, KeyAllFreq, KeyNegFreq" "PLCFREQ - Plots the frequency response for the given CYCSPEC specification.
PLCFREQ, SPEC, SECTbeg, SECTend" "PLCHIST - Plots a histogram of the frequency response of each sector for the given CYCSPEC specification.
PLCHIST, SPEC, FREQpt" "PLCINT - Plots the fracture parameter (CINT) result data.
PLCINT, ACTION, ID, Node, Cont, Dtype" "PLCKSURF - Plots the &#934; = 0 level set surface in an XFEM-based crack analysis
PLCKSURF, MODELDISPLAY" "PLCPLX - Specifies the part of a complex variable to display.
PLCPLX, KEY" "PLDISP - Displays the displaced structure.
PLDISP, KUND" "PLESOL - Displays solution results as discontinuous element contours.
PLESOL, Item, Comp, KUND, Fact" "PLETAB - Displays element table items.
PLETAB, Itlab, Avglab" "PLF2D - Generates a contour line plot of equipotentials.
PLF2D, NCONT, OLAY, ANUM, WIN" "PLFAR - Plots pressure far fields and far field parameters. 
PLFAR, Lab, Option, PHI1, PHI2, NPH1, THETA1, THETA2, NTHETA, VAL1, VAL2, VAL3, VAL4, VAL5, LDSTEP, SUBSTEP, FREQB, FREQE, PlotType, LogOpt" "PLGEOM - Plots target and source geometries.
PLGEOM, Item, NODEkey" "PLLS - Displays element table items as contoured areas along elements.
PLLS, LabI, LabJ, Fact, KUND,ViewUP" "PLMAP - Plots target and source pressures.
PLMAP, Item, --, NODEkey, ImagKey" "PLMC - Plots the modal coordinates from a mode-superposition solution.
PLMC, LSTEP, SBSTEP, TIMFRQ, KIMG, HIbeg, HIend" "PLNEAR - Plots the pressure in the near zone exterior to the equivalent source surface.
PLNEAR, Lab, Opt, KCN, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8,VAL9" "PLNSOL - Displays solution results as continuous element contours.
PLNSOL, Item, Comp, KUND, Fact, FileID, AVG" "PLORB
PLORB - Displays the orbital motion of a rotating structure" "PLOT, NSTRT, NEND, NINC
PLOTTING" "PLPAGM - Displays path items along the path geometry.
PLPAGM, Item, Gscale, Nopt" "PLPATH - Displays path items on a graph.
PLPATH, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6" "PLSECT - Displays membrane and membrane-plus-bending linearized stresses.
PLSECT, Item, Comp, RHO, KBR, KBR3D" "PLTIME - Defines the time range for which data are to be displayed.
PLTIME, TMIN, TMAX" "PLTRAC - Displays a charged particle trace on an element    display.
PLTRAC, Analopt, Item, Comp, TRPNum, Name, MXLOOP, TOLER, OPTION, ESCL, MSCL" "PLVAR - Displays up to ten variables in the form of a graph.
PLVAR, NVAR1, NVAR2, NVAR3, NVAR4, NVAR5, NVAR6, NVAR7, NVAR8, NVAR9, NVAR10" "PLVECT - Displays results as vectors.
PLVECT, Item, Lab2, Lab3, LabP, Mode, Loc, Edge, KUND" "PLZZ - Plots the interference diagram from a cyclic modal analysis.
PLZZ, RotVel, DeltaRotVel" "PMAP - Creates mapping of the path geometry by defining path interpolation division points.
PMAP, FORM, DISCON" "PMGTRAN - Summarizes electromagnetic results from a transient analysis.
PMGTRAN, Fname, FREQ, Fcnam1, Fcnam2, Pcnam1, Pcnam2, Ecnam1, Ccnam1" "PMLOPT - Defines perfectly matched layers (PMLs) or irregular perfectly    matched layers (IPML).
PMLOPT, PSYS, Lab, Xminus, Xplus, Yminus, Yplus, Zminus, Zplus, MixOpt" "PMLSIZE - Determines number of PML or IPML layers.
PMLSIZE, FREQB, FREQE, DMIN, DMAX, THICK, ANGLE, WAVESPEED" "PNGR - Provides PNG file export for ANSYS displays. 
PNGR, Kywrd, OPT, VAL" "POINT
POINT - Specifies \"Point flow tracing settings\" as the subsequent status topic." "POLY
POLY - Creates a polygonal area based on working plane coordinate pairs." "POWERH
POWERH - Calculates the rms power loss in a conductor or lossy dielectric." "PPATH - Defines a path by picking or defining nodes, or locations on the currently active working plane, or by entering specific coordinate locations.
PPATH, POINT, NODE, X, Y, Z, CS" "PRANGE - Determines the path range.
PRANGE, LINC, VMIN, VMAX, XVAR" "PRAS - Prints a specified acoustic quantity during postprocessing of an    acoustic analysis.
PRAS, Lab, LDSTEP, SUBSTEP, FREQB, FREQE, LogOpt, --, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6" "PRCAMP - Prints Campbell diagram data for applications involving rotating structure dynamics.
PRCAMP, Option, SLOPE, UNIT, FREQB, Cname, STABVAL, KeyALLFreq, KeyNegFreq, KeyWhirl" "PRCINT - Lists the fracture parameter (CINT) results data.
PRCINT, ID, Node, Dtype" "PRCPLX - Defines the output form for complex variables.
PRCPLX, KEY" "PRED - Activates a predictor in a nonlinear analysis.
PRED, Sskey, --, Lskey" "PRENERGY - Prints the total energies of a model or the energies of the specified components. 
PRENERGY, EnergyType, Cname1, Cname2, Cname3, Cname4, Cname5, Cname6" "PRERR
PRERR - Prints SEPC and TEPC." "PRESOL - Prints the solution results for elements.
PRESOL, Item, Comp" "PRETAB - Prints the element table items.
PRETAB, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6, Lab7, Lab8, Lab9" "PRFAR - Prints acoustic far field parameters. 
PRFAR, Lab, Option, PHI1, PHI2, NPH1, THETA1, THETA2, NTHETA, VAL1, VAL2, VAL3, VAL4, VAL5, LDSTEP, SUBSTEP, FREQB, FREQE, --, LogOpt" "PRI2 - Creates a polygonal area or a prism volume by vertices (GUI).
PRI2, P51X, Z1, Z2" "PRIM
PRIM - Specifies \"Solid model primitives\" as the subsequent status topic." "PRINT
PRINT - Specifies \"Print settings\" as the subsequent status topic." "PRISM - Creates a prism volume based on working plane coordinate pairs.
PRISM, Z1, Z2" "PRITER
PRITER - Prints solution summary data." "PRJSOL - Prints joint element output.
PRJSOL, Item, Comp" "PRNEAR - Prints the pressure in the near zone exterior to the equivalent source surface.
PRNEAR, Lab, Opt, KCN, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8,VAL9" "PRNLD - Prints the summed element nodal loads.
PRNLD, Lab, TOL, Item" "PRNSOL - Prints nodal solution results.
PRNSOL, Item, Comp, --, --, --, AVG" "PROD - Multiplies variables.
PROD, IR, IA, IB, IC, Name, --, --, FACTA, FACTB, FACTC" "PRORB - Prints the orbital motion characteristics of a rotating structure
PRORB, WhrlNodKey" "PRPATH - Prints path items along a geometry path.
PRPATH, Lab1, Lab2, Lab3, Lab4, Lab5, Lab6" "PRRFOR - Prints the constrained node reaction solution. Used with the FORCE command.
PRRFOR, Lab" "PRRSOL - Prints the constrained node reaction solution.
PRRSOL, Lab" "PRSCONTROL - Specifies whether to include pressure load stiffness in the element stiffness formation.
PRSCONTROL, Key" "PRSECT - Calculates and prints linearized stresses along a section path.
PRSECT, RHO, KBR, KBR3D" "PRTIME - Defines the time range for which data are to be listed.
PRTIME, TMIN, TMAX" "PRVAR - Lists variables vs. time (or frequency).
PRVAR, NVAR1, NVAR2, NVAR3, NVAR4, NVAR5, NVAR6" "PRVECT - Prints results as vector magnitude and direction cosines.
PRVECT, Item, Lab2, Lab3, LabP" "PSCONTROL - Enables or disables shared-memory parallel operations.
PSCONTROL, Option, Key" "PSDCOM - Specifies the power spectral density mode combination method.
PSDCOM, SIGNIF, COMODE, , ForceType" "PSDFRQ - Defines the frequency points for the input spectrum tables PSDVAL vs. PSDFRQ for PSD analysis.
PSDFRQ, TBLNO1, TBLNO2, FREQ1, FREQ2, FREQ3, FREQ4, FREQ5, FREQ6, FREQ7" "PSDGRAPH - Displays input PSD curves.
PSDGRAPH, TBLNO1, TBLNO2, DisplayKey" "PSDRES - Controls solution output written to the results file from a PSD analysis.
PSDRES, Lab, RelKey" "PSDSPL - Defines a partially correlated excitation in a PSD analysis.
PSDSPL, TBLNO, RMIN, RMAX" "PSDUNIT - Defines the type of input PSD.
PSDUNIT, TBLNO, Type, GVALUE" "PSDVAL - Defines PSD values.
PSDVAL, TBLNO, SV1, SV2, SV3, SV4, SV5, SV6, SV7" "PSDWAV - Defines a wave propagation excitation in a PSD analysis.
PSDWAV, TBLNO, VX, VY, VZ" "PSEL - Selects a path or paths.
PSEL, Type, Pname1, Pname2, Pname3, Pname4, Pname5, Pname6, Pname7, Pname8, Pname9, Pname10" "PSMAT - Writes an assembled global matrix to a postscript format that graphically displays nonzero matrix values.
PSMAT, Fname, Ext, Matrix, Color" "PSMESH - Creates and meshes a pretension section     (PRETS179) or a preload section    (MPC184).
PSMESH, SECID, Name, P0, Egroup, NUM, KCN, KDIR, VALUE, NDPLANE, PSTOL, PSTYPE, ECOMP, NCOMP" "PSTRES - Specifies whether prestress effects are calculated or included.
PSTRES, Key" "PSYS - Sets the PML element coordinate system attribute pointer.
PSYS, KCN" "PTR - Dumps the record of a binary file.
PTR, LOC, BASE, LOCH, BASEH" "PTXY - Defines coordinate pairs for use in polygons and prisms.
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
RCYC, IR, IA, SECTOR, Name" "RDEC - Defines the decimation parameters used by the radiosity solver       method.
RDEC, Option, REDUC, --, Nplace" "RDELE - Deletes real constant sets. 
RDELE, NSET1, NSET2, NINC, LCHK" "READ - Reads coordinate and pressure data from a file.
READ, Fname, NSKIP, Format, Xfield, Yfield, Zfield, pRfield, pIfield" "REAL - Sets the element real constant set attribute pointer. 
REAL, NSET" "REALVAR - Forms a variable using only the real part of a complex variable. 
REALVAR, IR, IA, --, --, Name, --, --, FACTA" "RECTNG - Creates a rectangular area anywhere on the working plane. 
RECTNG, X1, X2, Y1, Y2" "REMESH - Specifies the starting and ending remeshing points, and other options, for rezoning.
REMESH, Action, Filename, Ext, --, Opt1, Opt2" "RESCOMBINE - Reads results from local results files into the database after a distributed memory parallel (Distributed ANSYS) solution.
RESCOMBINE, NUMFILES, Fname, Ext, Lstep, Sbstep, Fact, KIMG, TIME, ANGLE, NSET, ORDER" "RESCONTROL - Controls file writing for multiframe restarts.
RESCONTROL, Action, Ldstep, Frequency, MAXFILES, --, MAXTotalFiles, Filetype" "RESET
RESET - Resets all POST1 or POST26 specifications to initial defaults. " "RESP - Generates a response spectrum. 
RESP, IR, LFTAB, LDTAB, specType, dampRatio, DTIME, TMIN, TMAX, inputType" "RESUME - Resumes the database from the database file.
RESUME, Fname, Ext, --, NOPAR, KNOPLOT" "RESVEC - Calculates or includes residual vectors or residual responses
RESVEC, KeyVect, -, -, -, KeyResp" "RESWRITE - Appends results data from the database to a results file.
RESWRITE, Fname, --, --, --, cFlag" "REZONE - Initiates the rezoning process, sets rezoning options, and rebuilds the database.
REZONE, Option, LDSTEP, SBSTEP" "RFORCE - Specifies the total reaction force data to be stored. 
RFORCE, NVAR, NODE, Item, Comp, Name" "RIGID - Specifies known rigid body modes (if any) of the model. 
RIGID, Dof1, Dof2, Dof3, Dof4, Dof5, Dof6" "RIGRESP - Specifies the rigid response calculation.
RIGRESP, Option, Method, Val1, Val2" "RLIST - Lists the real constant sets. 
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
RPSD, IR, IA, IB, ITYPE, DATUM, Name, --, SIGNIF" "RSMESH - Generates a result section. 
RSMESH, P0, RID, KCN, Kdir, VALUE, NDPLANE, PSTOL, Ecomp" "RSOPT - Creates or loads the radiosity mapping data file for SURF251 or SURF252 element types.
RSOPT, Opt, Filename, Ext, Dir" "RSPLIT - Creates one or more results file(s) from the current results file based on subsets of elements.
RSPLIT, Option, Label, Name1, Name2, Name3, Name4, Name5, Name6, Name7, Name8, Name9, Name10, Name11, Name12, Name13, Name14, Name15, Name16" "RSTMAC - Calculates modal assurance criterion (MAC) and matches nodal solutions from two results files or from one results file and one universal format file.
RSTMAC, File1, Lstep1, Sbstep1, File2, Lstep2, Sbstep2, --, MacLim, Cname, KeyPrint" "RSTOFF - Offsets node or element IDs in the FE geometry record.
RSTOFF, Lab, OFFSET" "RSURF - Generates the radiosity surface elements and stores them in the database.
RSURF, Options, Delopts, ETNUM" "RSYMM - Defines symmetry, rotation, or extrusion parameters for the    radiosity method.
RSYMM, Option, CS, Axis, NSECT, CONDVALUE, SVAL, EVAL" "RSYS - Activates a coordinate system for printout or display of element and nodal results. 
RSYS, KCN" "SABS - Specifies absolute values for element table operations.
SABS, KEY" "SADD - Forms an element table item by adding two existing items.
SADD, LabR, Lab1, Lab2, FACT1, FACT2, CONST" "SALLOW - Defines the allowable stress table for safety factor calculations.
SALLOW, STRS1, STRS2, STRS3, STRS4, STRS5, STRS6" "SAVE - Saves all current database information.
SAVE, Fname, Ext, --, Slab" "SBCLIST
SBCLIST - Lists solid model boundary conditions." "SBCTRAN
SBCTRAN - Transfers solid model loads and boundary conditions to the FE model." "SCOPT - Specifies System Coupling options.
SCOPT, TempDepKey" "SDELETE - Deletes sections from the database.
SDELETE, SFIRST, SLAST, SINC, KNOCLEAN, LCHK" "SE - Defines a superelement.
SE, File, --, --, TOLER, nStartVN" "SECCONTROL - Supplements or overrides default section properties.
SECCONTROL, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10, VAL11, VAL12, VAL13" "SECDATA - Describes the geometry of a section.
SECDATA, VAL1, VAL2, VAL3, VAL4, VAL5, VAL6, VAL7, VAL8, VAL9, VAL10, VAL11, VAL12" "SECFUNCTION - Specifies shell section thickness as a tabular function.
SECFUNCTION, TABLE, PATTERN" "SECJOINT - Defines local coordinate systems at joint element nodes and other data for joint elements.
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
SEEXP, Sename, Usefil, Imagky, Expopt" "SELIST - Lists the contents of a superelement matrix file.
SELIST, Sename, KOPT, KINT" "SELM
SELM - Specifies \"Superelements\" as the subsequent status topic." "SELTOL - Sets the tolerance for subsequent select operations.
SELTOL, Toler" "SEMIIMPLICIT - Specifies parameters for a semi-implicit solution.
SEMIIMPLICIT, Option, Type, Value" "SENERGY - Determines the stored magnetic energy or co-energy.
SENERGY, OPT, ANTYPE" "SEOPT - Specifies substructure analysis options.
SEOPT, Sename, SEMATR, SEPR, SESST, ExpMth, SeOcLvL" "SESYMM - Performs a symmetry operation on a superelement within the use pass.
SESYMM, Sename, Ncomp, INC, File, Ext, --" "SET - Defines the data set to be read from the results file.
SET, Lstep, Sbstep, Fact, KIMG, TIME, ANGLE, NSET, ORDER" "SETFGAP - Updates or defines the real constant table for squeeze film elements.
SETFGAP, GAP, ROPT, --, PAMB, ACF1, ACF2, PREF, MFP" "SETRAN - Creates a superelement from an existing superelement.
SETRAN, Sename, KCNTO, INC, File, Ext, --, DX, DY, DZ, NOROT" "SEXP - Forms an element table item by exponentiating and multiplying.
SEXP, LabR, Lab1, Lab2, EXP1, EXP2" "SF - Defines surface loads on nodes.
SF, Nlist, Lab, VALUE, VALUE2" "SFA - Specifies surface loads on the selected areas.
SFA, Area, LKEY, Lab, VALUE, VALUE2" "SFACT - Allows safety factor or margin of safety calculations to be made.
SFACT, TYPE" "SFADELE - Deletes surface loads from areas.
SFADELE, AREA, LKEY, Lab" "SFALIST - Lists the surface loads for the specified area.
SFALIST, AREA, Lab" "SFBEAM - Specifies surface loads on beam and pipe elements.
SFBEAM, Elem, LKEY, Lab, VALI, VALJ, VAL2I, VAL2J, IOFFST, JOFFST, LENRAT" "SFCALC - Calculates the safety factor or margin of safety.
SFCALC, LabR, LabS, LabT, TYPE" "SFCUM - Specifies that surface loads are to be accumulated.
SFCUM, Lab, Oper, FACT, FACT2" "SFDELE - Deletes surface loads.
SFDELE, Nlist, Lab" "SFE - Defines surface loads on elements.
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
SHPP, Lab, VALUE1, VALUE2" "SHSD - Creates or deletes a shell-solid interface to be used in shell-to-solid    assemblies.
SHSD, RID, Action" "SLIST - Summarizes the section properties for all defined sections in the current session.
SLIST, SFIRST, SLAST, SINC, Details, Type" "SLOAD - Loads a pretension section.
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
SPMWRITE, Method, NMODE, Inputs, InputLabels, Outputs, OutputLabels, NIC, VelAccKey, FileFormat" "SPOINT - Defines a point for force/moment summations or inertia calculation
SPOINT, NODE, X, Y, Z, InertiaKey" "SPOPT - Selects the spectrum type and other spectrum options.
SPOPT, Sptype, NMODE, Elcalc, modeReuseKey" "SPREAD - Turns on a dashed tolerance curve for the subsequent curve plots.
SPREAD, VALUE" "SPTOPT
SPTOPT - Specifies \"Spectrum analysis options\" as the subsequent status topic." "SPUNIT - Defines the type of multi-point response spectrum.
SPUNIT, TBLNO, Type, GVALUE, KeyInterp" "SPVAL - Defines multi-point response spectrum values.
SPVAL, TBLNO, CURVNO, SV1, SV2, SV3, SV4, SV5, SV6, SV7" "SQRT - Forms the square root of a variable.
SQRT, IR, IA, --, --, Name, --, --, FACTA" "SRSS - Specifies the square root of sum of squares mode combination method.
SRSS, SIGNIF, Label, AbsSumKey, ForceType" "SSBT - Specifies preintegrated bending thermal effects for shell sections.
SSBT, BT 11 , BT 22 , BT 12 , T" "SSLN - Selects and displays small lines in the model.
SSLN, FACT, SIZE" "SSMT - Specifies preintegrated membrane thermal effects for shell sections.
SSMT, MT 11 , MT 22 , MT 12 , T" "SSOPT - Defines a solution option for soil analysis.
SSOPT, Option, Par1, Par2, Par3, Par4, Par5" "SSPA - Specifies a preintegrated membrane stiffness for shell sections.
SSPA, A11 , A21 , A31 , A22 , A32 , A33 , T" "SSPB - Specifies a preintegrated coupling stiffness for shell sections.
SSPB, B11 , B21 , B31 , B22 , B32 , B33 , T, B12 , B13 , B23" "SSPD - Specifies a preintegrated bending stiffness for shell sections.
SSPD, D11 , D21 , D31 , D22 , D32 , D33 , T" "SSPE - Specifies a preintegrated transverse shear stiffness for shell sections.
SSPE, E11 , E21 , E22 , T" "SSPM - Specifies mass density for a preintegrated shell section.
SSPM, DENS, T" "SSTATE - Defines a steady-state rolling analysis.
SSTATE, Action, CM_Name, Val1, Val2, Val3, Val4, Val5, Val6, Val7, Val8, Val9" "SSUM
SSUM - Calculates and prints the sum of element table items." "STABILIZE - Activates stabilization for all elements that support nonlinear             stabilization.
STABILIZE, Key, Method, VALUE, SubStpOpt, FORCELIMIT" "STAT
STAT - Displays the status of database settings." "STEF - Specifies Stefan-Boltzmann radiation constant.
STEF, VALUE" "STORE - Stores data in the database for the defined variables.
STORE, Lab, NPTS, --, FREQ, Toler" "SUBOPT - Specifies Subspace (SUBSP) eigensolver options.
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
TARGET, Nlist" "TB - Activates a data table for material       properties or special element input.
TB, Lab, MATID, NTEMP, NPTS, TBOPT, --, FuncName" "TBCOPY - Copies a data table from one material to another.
TBCOPY, Lab, MATF, MATT" "TBDATA - Defines data for the material data table.
TBDATA, STLOC, C1, C2, C3, C4, C5, C6" "TBDELE - Deletes previously defined material data tables.
TBDELE, Lab, MAT1, MAT2, INC" "TBEO - Sets special options or parameters for material data tables.
TBEO, Par, Value" "TBFIELD - Defines values of field variables for material data tables.
TBFIELD, Type, Value" "TBFT - Performs material curve-fitting operations.
TBFT, Oper, ID, Option1, Option2, Option3, Option4, Option5, Option6, Option7, –, Option9" "TBIN - Sets parameters used for interpolation of    the material data    tables.
TBIN, Oper, Par1, Par2, Par3, Par4" "TBLE
TBLE - Specifies \"Data table properties\" as the subsequent status topic." "TBLIST - Lists the material data tables.
TBLIST, Lab, MAT" "TBMODIF - Modifies data for the material data table (GUI).
TBMODIF, ROW, COL, VALUE" "TBPLOT - Displays the material data table.
TBPLOT, Lab, MAT, TBOPT, TEMP, SEGN" "TBPT - Defines a point on a nonlinear data curve.
TBPT, Oper, X1, X2, X3, ..., XN" "TBTEMP - Defines a temperature for a material data table.
TBTEMP, TEMP, KMOD" "TCHG - Converts 20-node degenerate tetrahedral elements to their 10-node non-degenerate counterparts.
TCHG, ENAME1, ENAME2, ETYPE2" "THEXPAND - Enables or disables thermal loading
THEXPAND, KEY" "THOPT - Specifies nonlinear transient thermal solution options.
THOPT, Refopt, REFORMTOL, NTABPOINTS, TEMPMIN, TEMPMAX, --, ALGO" "TIFF - Provides TIFF file Export for ANSYS Displays.
TIFF, Kywrd, OPT" "TIME - Sets the time for a load step.
TIME, TIME" "TIMERANGE - Specifies the time range for which data are to be stored.
TIMERANGE, TMIN, TMAX" "TIMINT - Turns on transient effects.
TIMINT, Key, Lab" "TIMP - Improves the quality of tetrahedral elements that are not associated with a volume.
TIMP, ELEM, CHGBND, IMPLEVEL" "TINTP - Defines transient integration parameters.
TINTP, GAMMA, ALPHA, DELTA, THETA, OSLM, TOL, --, --, AVSMOOTH, ALPHAF, ALPHAM" "TOFFST - Specifies the temperature offset from absolute zero to zero.
TOFFST, VALUE" "TORUS - Creates a toroidal volume.
TORUS, RAD1, RAD2, RAD3, THETA1, THETA2" "TRANS, Fname, Ext, --
TRANSFER - Transfers a pattern of nodes to another coordinate system." "TREF - Defines the reference temperature for thermal strain calculations.
TREF, TREF" "TRNOPT - Specifies transient analysis options.
TRNOPT, Method, MAXMODE, --, MINMODE, MCout, TINTOPT, VAout, DMPSFreq, EngCalc" "TRPDEL - Deletes particle flow or charged particle trace points.
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
VTYPE, NOHID, NZONE" "WPAVE - Moves the working plane origin to the average of specified points.
WPAVE, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3" "WPCSYS - Defines the working plane location based on a coordinate system.
WPCSYS, WN, KCN" "WPLANE - Defines a working plane to assist in picking operations.
WPLANE, WN, XORIG, YORIG, ZORIG, XXAX, YXAX, ZXAX, XPLAN, YPLAN, ZPLAN" "WPOFFS - Offsets the working plane.
WPOFFS, XOFF, YOFF, ZOFF" "WPROTA - Rotates the working plane.
WPROTA, THXY, THYZ, THZX" "WPSTYL - Controls the display and style of the working plane.
WPSTYL, SNAP, GRSPAC, GRMIN, GRMAX, WPTOL, WPCTYP, GRTYPE, WPVIS, SNAPANG" "WRFULL - Stops solution after assembling global matrices.
WRFULL, Ldstep" "WRITE - Writes the radiation matrix file.
WRITE, Fname" "WRITEMAP - Writes interpolated pressure data to a file.
WRITEMAP, Fname" "WSPRINGS
WSPRINGS - Creates weak springs on corner nodes of a bounding box of the currently selected elements." "WTBCREATE - Creates a USER300 element to model the turbine for full aeroelastic coupling analysis and specifies relevant settings for the analysis.
WTBCREATE, IEL, NODE, DAMP" "XFCRKMESH - Defines a crack in the model when the crack surface is discretized by     MESH200 elements
XFCRKMESH, EnrichmentID, ElemComp, NodeComp" "XFDATA - Defines a crack in the model by specifying nodal level set values
XFDATA, EnrichmentID, LSM or -- , ELEMNUM, NODENUM, PHI, PSI" "XFENRICH - Defines parameters associated with crack propagation using XFEM
XFENRICH, EnrichmentID, CompName, MAT_ID,Method, RADIUS, SNAPTOLER" "XFLIST - Lists enrichment details and associated crack information
XFLIST, EnrichmentID" "XVAR - Specifies the X variable to be displayed.
XVAR, N" "~CAT5IN - Transfers a .CATPart file into the ANSYS program.
~CAT5IN, Name, Extension, Path, Entity, FMT, NOCL, NOAN" "~CATIAIN - Transfers a CATIA model into the ANSYS program.
~CATIAIN, Name, Extension, Path, - -, - -, BLANK, - -" "~PARAIN - Transfers a Parasolid file into the ANSYS program.
~PARAIN, Name, Extension, Path, Entity, FMT, Scale" "~PROEIN - Transfers a Creo Parametric part into the ANSYS program.
~PROEIN, Name, Extension, Path, Proecomm" "~SATIN - Transfers a .SAT file into the ANSYS program.
~SATIN, Name, Extension, Path, Entity, FMT, NOCL, NOAN" "~UGIN - Transfers an NX part into the ANSYS program.
~UGIN, Name, Extension, Path, Entity, LAYER, FMT" "/WB - APDL undocumented command
/WB" "XMLO - APDL undocumented command
XMLO" "/XML - APDL undocumented command
/XML" "CNTR - APDL undocumented command
CNTR" "EBLOCK - APDL undocumented command
EBLOCK" "CMBLOCK - APDL undocumented command
CMBLOCK" "NBLOCK - APDL undocumented command
NBLOCK" "/TRACK - APDL undocumented command
/TRACK" "CWZPLOT - APDL undocumented command
CWZPLOT" "~EUI - APDL undocumented command
~EUI" "NELE - APDL undocumented command
NELE" "EALL - APDL undocumented command
EALL" "NALL - APDL undocumented command
NALL" "FLITEM - APDL undocumented command
FLITEM" "LSLN - APDL undocumented command
LSLN" "PSOLVE - APDL undocumented command
PSOLVE" "ASLN - APDL undocumented command
ASLN" "/VERIFY - APDL undocumented command
/VERIFY" "/SSS - APDL undocumented command
/SSS" "~CFIN - APDL undocumented command
~CFIN" "*EVAL - APDL undocumented command
*EVAL" "*MOONEY - APDL undocumented command
*MOONEY" "/RUNSTAT - APDL undocumented command
/RUNSTAT" "ALPFILL - APDL undocumented command
ALPFILL" "ARCOLLAPSE - APDL undocumented command
ARCOLLAPSE" "ARDETACH - APDL undocumented command
ARDETACH" "ARFILL - APDL undocumented command
ARFILL" "ARMERGE - APDL undocumented command
ARMERGE" "ARSPLIT - APDL undocumented command
ARSPLIT" "CEWRITE - APDL undocumented command
CEWRITE" "FIPLOT - APDL undocumented command
FIPLOT" "GAPFINISH - APDL undocumented command
GAPFINISH" "GAPLIST - APDL undocumented command
GAPLIST" "GAPMERGE - APDL undocumented command
GAPMERGE" "GAPOPT - APDL undocumented command
GAPOPT" "GAPPLOT - APDL undocumented command
GAPPLOT" "LNCOLLAPSE - APDL undocumented command
LNCOLLAPSE" "LNDETACH - APDL undocumented command
LNDETACH" "LNFILL - APDL undocumented command
LNFILL" "LNMERGE - APDL undocumented command
LNMERGE" "LNSPLIT - APDL undocumented command
LNSPLIT" "PCONV - APDL undocumented command
PCONV" "PLCONV - APDL undocumented command
PLCONV" "PEMOPTS - APDL undocumented command
PEMOPTS" "PEXCLUDE - APDL undocumented command
PEXCLUDE" "PINCLUDE - APDL undocumented command
PINCLUDE" "PMETH - APDL undocumented command
PMETH" "/PMETH - APDL undocumented command
/PMETH" "PMOPTS - APDL undocumented command
PMOPTS" "PPLOT - APDL undocumented command
PPLOT" "PPRANGE - APDL undocumented command
PPRANGE" "PRCONV - APDL undocumented command
PRCONV" "PRECISION - APDL undocumented command
PRECISION" "RALL - APDL undocumented command
RALL" "RFILSZ - APDL undocumented command
RFILSZ" "RITER - APDL undocumented command
RITER" "RMEMRY - APDL undocumented command
RMEMRY" "RSPEED - APDL undocumented command
RSPEED" "RSTAT - APDL undocumented command
RSTAT" "RTIMST - APDL undocumented command
RTIMST" "/RUNST - APDL undocumented command
/RUNST" "RWFRNT - APDL undocumented command
RWFRNT" "SARPLOT - APDL undocumented command
SARPLOT" "SHSD - APDL undocumented command
SHSD" "SLPPLOT - APDL undocumented command
SLPPLOT" "SLSPLOT - APDL undocumented command
SLSPLOT" "VCVFILL - APDL undocumented command
VCVFILL" "/OPT - APDL undocumented command
/OPT" "OPEQN - APDL undocumented command
OPEQN" "OPFACT - APDL undocumented command
OPFACT" "OPFRST - APDL undocumented command
OPFRST" "OPGRAD - APDL undocumented command
OPGRAD" "OPKEEP - APDL undocumented command
OPKEEP" "OPLOOP - APDL undocumented command
OPLOOP" "OPPRNT - APDL undocumented command
OPPRNT" "OPRAND - APDL undocumented command
OPRAND" "OPSUBP - APDL undocumented command
OPSUBP" "OPSWEEP - APDL undocumented command
OPSWEEP" "OPTYPE - APDL undocumented command
OPTYPE" "OPUSER - APDL undocumented command
OPUSER" "OPVAR - APDL undocumented command
OPVAR" "OPADD - APDL undocumented command
OPADD" "OPCLR - APDL undocumented command
OPCLR" "OPDEL - APDL undocumented command
OPDEL" "OPMAKE - APDL undocumented command
OPMAKE" "OPSEL - APDL undocumented command
OPSEL" "OPANL - APDL undocumented command
OPANL" "OPDATA - APDL undocumented command
OPDATA" "OPRESU - APDL undocumented command
OPRESU" "OPSAVE - APDL undocumented command
OPSAVE" "OPEXE - APDL undocumented command
OPEXE" "OPLFA - APDL undocumented command
OPLFA" "OPLGR - APDL undocumented command
OPLGR" "OPLIST - APDL undocumented command
OPLIST" "OPLSW - APDL undocumented command
OPLSW" "OPRFA - APDL undocumented command
OPRFA" "OPRGR - APDL undocumented command
OPRGR" "OPRSW - APDL undocumented command
OPRSW" "PILECALC - APDL undocumented command
PILECALC" "PILEDISPSET - APDL undocumented command
PILEDISPSET" "PILEGEN - APDL undocumented command
PILEGEN" "PILELOAD - APDL undocumented command
PILELOAD" "PILEMASS - APDL undocumented command
PILEMASS" "PILERUN - APDL undocumented command
PILERUN" "PILESEL - APDL undocumented command
PILESEL" "PILESTIF - APDL undocumented command
PILESTIF" "PLVAROPT - APDL undocumented command
PLVAROPT" "PRVAROPT - APDL undocumented command
PRVAROPT" "TOCOMP - APDL undocumented command
TOCOMP" "TODEF - APDL undocumented command
TODEF" "TOFREQ - APDL undocumented command
TOFREQ" "TOTYPE - APDL undocumented command
TOTYPE" "TOVAR - APDL undocumented command
TOVAR" "TOEXE - APDL undocumented command
TOEXE" "TOLOOP - APDL undocumented command
TOLOOP" "TOGRAPH - APDL undocumented command
TOGRAPH" "TOLIST - APDL undocumented command
TOLIST" "TOPLOT - APDL undocumented command
TOPLOT" "TOPRINT - APDL undocumented command
TOPRINT" "TOSTAT - APDL undocumented command
TOSTAT" "TZAMESH - APDL undocumented command
TZAMESH" "TZDELE - APDL undocumented command
TZDELE" "TZEGEN - APDL undocumented command
TZEGEN" "XVAROPT - APDL undocumented command
XVAROPT" "PGSAVE - APDL undocumented command
PGSAVE" "SOLCONTROL - APDL undocumented command
SOLCONTROL" "TOTAL - APDL undocumented command
TOTAL" "VTGEOM - APDL undocumented command
VTGEOM" "VTREAL - APDL undocumented command
VTREAL" "VTSTAT - APDL undocumented command
VTSTAT" "PGRAPH - APDL undocumented command
PGRAPH" "/VT - APDL undocumented command
/VT" "VTIN - APDL undocumented command
VTIN" "VTRFIL - APDL undocumented command
VTRFIL" "VTTEMP - APDL undocumented command
VTTEMP" "PGRSET - APDL undocumented command
PGRSET" "VTCLR - APDL undocumented command
VTCLR" "VTMETH - APDL undocumented command
VTMETH" "VTRSLT - APDL undocumented command
VTRSLT" "VTVMOD - APDL undocumented command
VTVMOD" "PGSELE - APDL undocumented command
PGSELE" "VTDISC - APDL undocumented command
VTDISC" "VTMP - APDL undocumented command
VTMP" "VTSEC - APDL undocumented command
VTSEC" "PGWRITE - APDL undocumented command
PGWRITE" "VTEVAL - APDL undocumented command
VTEVAL" "VTOP - APDL undocumented command
VTOP" "VTSFE - APDL undocumented command
VTSFE" "POUTRES - APDL undocumented command
POUTRES" "VTFREQ - APDL undocumented command
VTFREQ" "VTPOST - APDL undocumented command
VTPOST" "VTSL - APDL undocumented command
VTSL" "FLDATA1-40 - APDL undocumented command
FLDATA1-40" "HFPCSWP - APDL undocumented command
HFPCSWP" "MSDATA - APDL undocumented command
MSDATA" "MSVARY - APDL undocumented command
MSVARY" "QFACT - APDL undocumented command
QFACT" "FLOCHECK - APDL undocumented command
FLOCHECK" "HFPOWER - APDL undocumented command
HFPOWER" "MSMASS - APDL undocumented command
MSMASS" "PERI - APDL undocumented command
PERI" "SPADP - APDL undocumented command
SPADP" "FLREAD - APDL undocumented command
FLREAD" "HFPORT - APDL undocumented command
HFPORT" "MSMETH - APDL undocumented command
MSMETH" "PLFSS - APDL undocumented command
PLFSS" "SPARM - APDL undocumented command
SPARM" "FLOTRAN - APDL undocumented command
FLOTRAN" "HFSCAT - APDL undocumented command
HFSCAT" "MSMIR - APDL undocumented command
MSMIR" "PLSCH - APDL undocumented command
PLSCH" "SPFSS - APDL undocumented command
SPFSS" "HFADP - APDL undocumented command
HFADP" "ICE - APDL undocumented command
ICE" "MSNOMF - APDL undocumented command
MSNOMF" "PLSYZ - APDL undocumented command
PLSYZ" "SPICE - APDL undocumented command
SPICE" "HFARRAY - APDL undocumented command
HFARRAY" "ICEDELE - APDL undocumented command
ICEDELE" "MSPROP - APDL undocumented command
MSPROP" "PLTD - APDL undocumented command
PLTD" "SPSCAN - APDL undocumented command
SPSCAN" "HFDEEM - APDL undocumented command
HFDEEM" "ICELIST - APDL undocumented command
ICELIST" "MSQUAD - APDL undocumented command
MSQUAD" "PLTLINE - APDL undocumented command
PLTLINE" "SPSWP - APDL undocumented command
SPSWP" "HFEIGOPT - APDL undocumented command
HFEIGOPT" "ICVFRC - APDL undocumented command
ICVFRC" "MSRELAX - APDL undocumented command
MSRELAX" "PLVFRC - APDL undocumented command
PLVFRC" "HFEREFINE - APDL undocumented command
HFEREFINE" "LPRT - APDL undocumented command
LPRT" "MSSOLU - APDL undocumented command
MSSOLU" "/PICE - APDL undocumented command
/PICE" "HFMODPRT - APDL undocumented command
HFMODPRT" "MSADV - APDL undocumented command
MSADV" "MSSPEC - APDL undocumented command
MSSPEC" "PLWAVE - APDL undocumented command
PLWAVE" "HFPA - APDL undocumented command
HFPA" "MSCAP - APDL undocumented command
MSCAP" "MSTERM - APDL undocumented command
MSTERM" "PRSYZ - APDL undocumented command
PRSYZ")
"Help strings for the parameters of APDL keywords.")

(defconst apdl-undocumented-command-regexp
"\\(?:\\*\\(?:EVAL\\|MOONEY\\)\\|/\\(?:OPT\\|P\\(?:ICE\\|METH\\)\\|RUNST\\(?:AT\\)?\\|SSS\\|TRACK\\|V\\(?:ERIFY\\|T\\)\\|WB\\|XML\\)\\|A\\(?:LPFILL\\|R\\(?:COLLAPSE\\|DETACH\\|FILL\\|MERGE\\|SPLIT\\)\\|SLN\\)\\|C\\(?:EWRITE\\|MBLOCK\\|NTR\\|WZPLOT\\)\\|E\\(?:ALL\\|BLOCK\\)\\|F\\(?:IPLOT\\|L\\(?:DATA1-40\\|ITEM\\|O\\(?:CHECK\\|TRAN\\)\\|READ\\)\\)\\|GAP\\(?:FINISH\\|LIST\\|MERGE\\|\\(?:OP\\|PLO\\)T\\)\\|HF\\(?:A\\(?:DP\\|RRAY\\)\\|DEEM\\|E\\(?:IGOPT\\|REFINE\\)\\|MODPRT\\|P\\(?:A\\|CSWP\\|O\\(?:RT\\|WER\\)\\)\\|SCAT\\)\\|IC\\(?:E\\(?:DELE\\|LIST\\)?\\|VFRC\\)\\|L\\(?:N\\(?:COLLAPSE\\|DETACH\\|FILL\\|MERGE\\|SPLIT\\)\\|PRT\\|SLN\\)\\|MS\\(?:ADV\\|CAP\\|DATA\\|M\\(?:ASS\\|ETH\\|IR\\)\\|NOMF\\|PROP\\|QUAD\\|RELAX\\|S\\(?:OLU\\|PEC\\)\\|TERM\\|VARY\\)\\|N\\(?:ALL\\|BLOCK\\|ELE\\)\\|OP\\(?:A\\(?:DD\\|NL\\)\\|CLR\\|D\\(?:ATA\\|EL\\)\\|E\\(?:QN\\|XE\\)\\|F\\(?:\\(?:AC\\|RS\\)T\\)\\|GRAD\\|KEEP\\|L\\(?:FA\\|GR\\|IST\\|OOP\\|SW\\)\\|MAKE\\|PRNT\\|R\\(?:AND\\|ESU\\|FA\\|GR\\|SW\\)\\|S\\(?:AVE\\|EL\\|\\(?:UB\\|WEE\\)P\\)\\|TYPE\\|\\(?:USE\\|VA\\)R\\)\\|P\\(?:CONV\\|E\\(?:MOPTS\\|RI\\|XCLUDE\\)\\|G\\(?:R\\(?:APH\\|SET\\)\\|\\(?:S\\(?:AV\\|EL\\)\\|WRIT\\)E\\)\\|I\\(?:LE\\(?:CALC\\|DISPSET\\|GEN\\|LOAD\\|MASS\\|RUN\\|S\\(?:EL\\|TIF\\)\\)\\|NCLUDE\\)\\|L\\(?:CONV\\|FSS\\|S\\(?:CH\\|YZ\\)\\|T\\(?:D\\|LINE\\)\\|V\\(?:AROPT\\|FRC\\)\\|WAVE\\)\\|M\\(?:ETH\\|OPTS\\)\\|OUTRES\\|P\\(?:LOT\\|RANGE\\)\\|R\\(?:CONV\\|ECISION\\|SYZ\\|VAROPT\\)\\|SOLVE\\)\\|QFACT\\|R\\(?:ALL\\|FILSZ\\|ITER\\|MEMRY\\|S\\(?:PEED\\|TAT\\)\\|\\(?:TIMS\\|WFRN\\)T\\)\\|S\\(?:ARPLOT\\|HSD\\|L\\(?:[PS]PLOT\\)\\|OLCONTROL\\|P\\(?:A\\(?:DP\\|RM\\)\\|FSS\\|ICE\\|S\\(?:CAN\\|WP\\)\\)\\)\\|T\\(?:O\\(?:COMP\\|DEF\\|EXE\\|FREQ\\|GRAPH\\|L\\(?:IST\\|OOP\\)\\|P\\(?:\\(?:LO\\|RIN\\)T\\)\\|STAT\\|T\\(?:AL\\|YPE\\)\\|VAR\\)\\|Z\\(?:AMESH\\|DELE\\|EGEN\\)\\)\\|V\\(?:CVFILL\\|T\\(?:CLR\\|DISC\\|EVAL\\|FREQ\\|GEOM\\|IN\\|M\\(?:ETH\\|P\\)\\|OP\\|POST\\|R\\(?:EAL\\|FIL\\|SLT\\)\\|S\\(?:EC\\|FE\\|L\\|TAT\\)\\|TEMP\\|VMOD\\)\\)\\|X\\(?:MLO\\|VAROPT\\)\\|~\\(?:CFIN\\|EUI\\)\\)"
"Regexp of commands not documented in the Ansys manuals.
Seen mainly in Workbench output files and Ansys verification models.")

(provide 'apdl-keyword)

;;; apdl-keyword.el ends here
