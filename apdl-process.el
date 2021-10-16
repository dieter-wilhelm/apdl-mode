;;; apdl-process.el --- Managing runs and processes for APDL-Mode -*- lexical-binding: t -*-
;; Time-stamp: <2021-10-14>

;; Copyright (C) 2006 - 2021  H. Dieter Wilhelm GPL V3

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Maintainer: H. Dieter Wilhelm
;; Version: 20.7.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages, convenience
;; URL: https://github.com/dieter-wilhelm/apdl-mode

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

;;; Commentary:

;; Managing runs and processes for APDL-Mode

;; Ansys MAPDL error codes:
;; Code Explanation
;; 0	Normal Exit
;; 1	Stack Error
;; 2	Stack Error
;; 3	Stack Error
;; 4	Stack Error
;; 5	Command Line Argument Error
;; 6	Accounting File Error
;; 7	Auth File Verification Error
;; 8	Error in Mechanical APDL or End-of-run
;; 11	User Routine Error
;; 12	Macro STOP Command
;; 14	XOX Error
;; 15	Fatal Error
;; 16	Possible Full Disk
;; 17	Possible Corrupted or Missing File
;; 18	Possible Corrupted DB File
;; 21	Authorized Code Section Entered
;; 25	Unable to Open X11 Server
;; 30	Quit Signal
;; 31	Failure to Get Signal
;; >32	System-dependent Error

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; external defvars

(defvar apdl-username)
(defvar apdl-help-index)
(defvar apdl-license-file)
(defvar apdl-license-categories)
(defvar apdl-ansys-program)
(defvar apdl-ansys-launcher)
(defvar apdl-ansys-wb)
(defvar apdl-ansys-help-path)
(defvar apdl-mode-install-directory)
(defvar apdl-current-ansys-version)
;; (defvar apdl-ansys-install-directory)
(defvar apdl-ansysli-servers)
;;(defvar apdl-is-unix-system-flag)
(defvar apdl-lmutil-program)
(defvar apdl-ansys-help-program)
(defvar apdl-initialised-flag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; declare-functions

(declare-function apdl-initialise "apdl-initialise")
(declare-function apdl-next-code-line "apdl-mode")
(declare-function apdl-code-line-p "apdl-mode")
(declare-function apdl-skip-block-forward "apdl-mode")
(declare-function apdl-in-empty-line-p "apdl-mode")

;; (declare-function buffer-name "") ; in-built
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; requires

(require 'comint)
(require 'url)
;;(require 'apdl-mode) recursive loading error!
(require 'apdl-initialise)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- customisation ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup APDL-process nil
  "Customisation 'process' subgroup for the APDL-Mode."
  :group 'APDL)

(defcustom apdl-license-occur-regexp
  '(
    "granta"				; material stuff
    "electronics"			; electronics desktop
    "spaceclaim"			; spaceclaim
    ;;    "agppi"				; agppi -- Design Modeler
    "cfd"			        ; Computational Fluid Mechanics
    "disc"				; disc* -- discovery procucts
    "aim_mp"				; aim_mp -- Discovery Aim
					; standard
    "stba"				; stba -- structural solver
    "struct"				; struct -- structural
    "mpba"				; mpba -- multiphysics solver
    "ane3"				; ane3 -- magnetics
					; ane3fl -- multiphysics
    "^ansys"				; ansys -- mechanical
    "anshpc"				; anshpc -- HighPerformanceComputing
    "^preppost"				; preppost -- PrePost
					; processing no solve
    "mech_"				; mech_1 -- mechanical pro
					; mech_2 -- mechanical premium
    "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")	; and time XX:XX:XX of status
					; request
  "List of regular expression strings of interesting licenses.
This list is concatenated to a regexp for the function
`apdl-occur'."
  :type 'list
  :group 'APDL-process)


(defcustom apdl-job "file"
  "String variable storing the Ansys job name.
It is initialised to 'file' (which is also the Ansys default job
name).  See `apdl-abort-file' for a way of stopping a solver run
in a controlled way and `apdl-display-error-file' for viewing
the respective error file."
  :type 'string
  :group 'APDL-process)

(defcustom apdl-license-categories
  '("ansys"
    "struct"
    "ane3"
    "ansysds"
    "ane3fl"
    "preppost")
  "List of available license types to choose for an Ansys run.
This list should contain the license types you can choose from.
Below are often used license types (as e.g. seen with the
function `apdl-license-status') and their corresponding WorkBench
terminology.

\"ansys\" - Mechanical U (without thermal capability)
\"struct\" - Structural U (with thermal capability)
\"ane3\" - Mechanical/Emag (Structural U with electromagnetics)
\"ansysds\" - Mechanical/LS-Dyna (Mechanical U with Ansys LS-Dyna inter-phase)
\"ane3fl\" - Multiphysics
\"preppost\" - PrepPost (no solving capabilities)"
  :type 'list
  :group 'APDL-process)

;; under a "prepost" license - propably since V19 - you can issue a
;; solve command, and MAPDL is solving while catching a "meba" license
;; :-)
(defcustom apdl-license "preppost" 	; changed from "ansys" 2021-09
  "The License cagegory with which the MAPDL interpreter will be started.
It is also used for displaying the current license usage in
`apdl-license-status'.  See the custom variable
`apdl-license-categories' for often used Ansys license types."
  ;;  :options '("ansys" "struct" "ane3" "ane3fl" "ansysds" "preppost")
  :options apdl-license-categories
  ;; options not available for strings (only hooks, alists, plists E22)
  :type 'string
  :group 'APDL-process)

(defcustom apdl-batch-license "meba"
  "The License type for an MAPDL batch run."
  :options '("ansys" "struct" "ane3" "ane3fl" "ansysds" "meba" "mech_1" "mech_2")
  ;; :options apdl-license-categories
  ;; options not available for strings (only hooks, alists, plists E22)
  :type 'string
  :group 'APDL-process)

(defcustom apdl-no-of-processors 3
  "No of processors to use for an Ansys MAPDL run.
This value is reccomended to N - 1, with N the total number of
cores in the computer.  If smaller then 5 the run does not
require additonal HPC licenses.  2 is the Ansys default for SMP
parallelisation."
  :type 'integer
  :group 'APDL-process)

(defcustom apdl-blink-delay .3
  "Number of seconds to highlight the evaluated region."
  :group 'APDL-process
  :type 'number)

(defcustom apdl-blink-region-flag t
  "Non-nil means highlight the evaluated region."
  :group 'APDL-process
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- variables ---

(defvar apdl-is-unix-system-flag nil
  "Non-nil means the computer runs a Unix system.
Any of GNU-Linux, aix, berkeley-unix, hpux, irix, lynxos 3.0.1 or
usg-unix-v.")

(defvar apdl-emacs-window-id nil
  "Editing buffer's X11 window id.")

(defvar apdl-classics-window-id nil
  "The X11 window id of the Ansys GUI or the command window.")

(defvar apdl-classics-flag nil
  "Non-nil means that a Classics GUI could be found.")

;;; --- constants ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst apdl-license-description
  '((1spacdes . "Geometry Interface for Creo Elements/Direct Modeling")
    (a_catv6_reader . "ANSYS CATIA V6 Reader")
    (a_dynamics_4_scdm . "ANSYS Dynamics for Spaceclaim by Algoryx")
    (a_geometry . "ANSYS Geometry Interfaces")
    (a_jt_reader . "Geometry Interface for JT")
    (a_lmat . "ANSYS Composite Cure Simulation")
    (a_spaceclaim_3dpdf . "3D PDF Reader for SpaceClaim")
    (a_spaceclaim_catv5 . "ANSYS SpaceClaim CATIA V5 Interface")
    (a_spaceclaim_dirmod . "ANSYS SpaceClaim Direct Modeler")
    (a_spaceclaim_faceteddata . "Faceted Data Toolkit for SpaceClaim")
    (a_spaceclaim_jt . "JT Open Reader for SpaceClaim")
    (aa_a . "ANSYS Academic Associate")
    (aa_a_cfd . "ANSYS Academic Associate CFD")
    (aa_a_hpc . "ANSYS Academic Associate HPC")
    (aa_ds . "ANSYS Academic Teaching DesignSpace")
    (aa_dy_p . "ANSYS Academic LS-DYNA Parallel")
    (aa_fcell . "ANSYS Academic Fuel Cell Tools")
    (aa_mcad . "ANSYS Academic MCAD")
    (aa_mesh . "ANSYS Academic Meshing Tools")
    (aa_r . "ANSYS Academic Research")
    (aa_r_cfd . "ANSYS Academic Research CFD")
    (aa_r_dy . "ANSYS Academic Research LS-DYNA")
    (aa_r_et . "ANSYS Academic Research Electronics Thermal")
    (aa_r_hpc . "ANSYS Academic Research HPC")
    (aa_r_me . "ANSYS Academic Research Mechanical")
    (aa_r_pf . "ANSYS Academic Research POLYFLOW")
    (aa_s_aim . "ANSYS AIM Student")
    (aa_t_a . "ANSYS Academic Teaching Advanced")
    (aa_t_cfd . "ANSYS Academic Teaching CFD")
    (aa_t_i . "ANSYS Academic Teaching Introductory")
    (aa_t_me . "ANSYS Academic Teaching Mechanical")
    (acdi_ad3dfull . "ANSYS AUTODYN-3D")
    (acdi_adprepost . "ANSYS AUTODYN PrepPost")
    (acdi_aqwadrft . "ANSYS AQWA - AQWADRFT")
    (acdi_aqwafer . "ANSYS AQWA - AQWAFER")
    (acdi_aqwags . "ANSYS AQWA - AQWAGS")
    (acdi_aqwalbrm . "ANSYS AQWA - AQWALBRM")
    (acdi_aqwaline . "ANSYS AQWA - AQWALINE")
    (acdi_aqwanaut . "ANSYS AQWA - AQWANAUT")
    (acdi_aqwawave . "ANSYS AQWA - AQWAWAVE")
    (acdi_asas . "ANSYS ASAS - ASAS")
    (acdi_asaslink . "ANSYS ASAS - ASASLINK")
    (acdi_asasnl . "ANSYS ASAS - ASASNL")
    (acdi_asas-vis . "ANSYS ASAS - ASAS-VIS")
    (acdi_beamst . "ANSYS ASAS - BEAMST")
    (acdi_cd-ags . "ANSYS AQWA - CD-AGS")
    (acdi_cd-drift . "ANSYS AQWA - CD-DRIFT")
    (acdi_cd-fer . "ANSYS AQWA - CD-FER")
    (acdi_cd-libr . "ANSYS AQWA - CD-LIBR")
    (acdi_cd-naut . "ANSYS AQWA - CD-NAUT")
    (acdi_comped . "ANSYS ASAS - COMPED")
    (acdi_explprof . "ANSYS Explicit STR")
    (acdi_fatjack . "ANSYS ASAS - FATJACK")
    (acdi_hydrodiff . "ANSYS AQWA - HYDRO-DIFFRACT")
    (acdi_loco . "ANSYS ASAS - LOCO")
    (acdi_mass . "ANSYS ASAS - MASS")
    (acdi_maxmin . "ANSYS ASAS - MAXMIN")
    (acdi_post . "ANSYS ASAS - POST")
    (acdi_postnl . "ANSYS ASAS - POSTNL")
    (acdi_prebeam . "ANSYS ASAS - PREBEAM")
    (acdi_prenl . "ANSYS ASAS - PRENL")
    (acdi_response . "ANSYS ASAS - RESPONSE")
    (acdi_splinter . "ANSYS ASAS - SPLINTER")
    (acdi_wave . "ANSYS ASAS - WAVE")
    (acdi_windspec . "ANSYS ASAS - WINDSPEC")
    (acdi_xtract . "ANSYS ASAS - XTRACT")
    (acfd_2 . "ANSYS CFD Premium")
    (acfd_3 . "ANSYS CFD Enterprise")
    (acfd_fcell . "Fuel Cell Module")
    (acfd_ffc . "FfC V5")
    (acfd_ffc_pre . "FfC V5 Pre")
    (acfd_fluent_solver . "ANSYS FLUENT Solver")
    (acfd_ib . "Immersed Boundary Module")
    (acfd_mhd . "ANSYS CFD MHD")
    (acfd_polyflow_blowmolding . "ANSYS POLYFLOW BlowMolding")
    (acfd_polyflow_extrusion . "ANSYS POLYFLOW Extrusion")
    (acfd_preppost . "ANSYS CFD PrepPost")
    (acfd_rif . "CFX-RIF Flamelet Library Generator")
    (acfd_v2f . "V2f Module")
    (acfd_vista_tf . "ANSYS Vista TF")
    (acfd_vki . "ANSYS VKI library")
    (acfdsol2 . "ANSYS CFD Premium Solver")
    (acfdsol3 . "ANSYS CFD Enterprise Solver")
    (acfx_advanced_turbulence . "ANSYS CFX-5 Advanced Turbulence Models")
    (acfx_bldmdlr . "ANSYS BladeModeler")
    (acfx_combustion . "ANSYS CFX-5 Reacting and Combusting Species")
    (acfx_mfr . "ANSYS CFX-5 Multiple Frames of Reference")
    (acfx_multiphase . "ANSYS CFX-5 Multi-Phase Flows")
    (acfx_post . "ANSYS CFX-Post")
    (acfx_pre . "ANSYS CFX-5 Pre")
    (acfx_radiation . "ANSYS CFX-5 Radiation Models")
    (acfx_turbogrid . "ANSYS CFX-TurboGrid")
    (acfx_turbulence_transition . "ANSYS CFX-5 Turbulence Transition")
    (acpreppost . "ANSYS Composite PrepPost")
    (act_cadfem_gominterface . "TPA GOM Interface CADFEM")
    (act_demsol_edemforansys . "TPA EDEM for ANSYS DEM Solutions")
    (act_edrmed_ASMEFat . "TPA ASME Fatigue EDR Medeso")
    (act_edrmed_ASMELF . "TPA ASME Local Failure EDR Medeso")
    (act_edrmed_bolttoolkit . "TPA Bolt Toolkit EDR Medeso")
    (act_edrmed_reportgenerator . "TPA Report Generator EDR Medeso")
    (act_edrmed_weldfatigue . "TPA Weld Fatigue EDR Medeso")
    (act_edrmed_weldstrength . "TPA Weld Strength EDR Medeso")
    (act_es_multiplerun . "TPA MultipleRun EnginSoft SPA")
    (act_FBay_MBDforANSYSmodeler . "TPA MBD for ANSYS Modeler FunctionBay")
    (act_ges_utilizationplot . "TPA Utilization Plot General Engineering Solutions")
    (act_greact_ntoplatticeworkflow . "TPA nTopology Lattice Custom Workflow Groupe Reaction")
    (act_infini_exportresults . "TPA Export Results Infinite Simulation Systems")
    (act_mminc_multimech . "TPA MultiMech MultiMechanics")
    (act_oei_simnotebook . "TPA Simulation Notebook Ozen Eng")
    (act_PERA_PCBTraceImageImport . "TPA PCBTraceImageFileImport Pera Global")
    (act_phimec_exporteverything . "TPA Export Everything PHI-MECA Eng")
    (act_prosol_rfi . "TPA RFlex Generator for RecurDyn Pro-Lambda Solutions")
    (act_rbf_rbfmorph . "TPA - RBF Morph ACT Extension for Mechanical - RBF Morph")
    (act_sorvim_designbooster . "TPA SORVI Design Booster Sorvimo Optimointipalvelut Oy")
    (advanced_meshing . "Advanced Meshing")
    (afsp_fensapice_cfd . "ANSYS FENSAP-ICE")
    (afsp_gui . "ANSYS FENSAP-ICE GUI")
    (afsp_optigrid . "ANSYS FENSAP-ICE OptiGrid")
    (afsp_viewmerical . "ANSYS FENSAP-ICE Viewmerical")
    (agppi . "ANSYS DesignModeler")
    (aiacis . "ANSYS ICEM CFD ACIS to Tetin Converter")
    (aibfcart . "ANSYS ICEM CFD BF-Cart")
    (aice_mesher . "ANSYS IcePak Mesher")
    (aice_pak . "ANSYS IcePak")
    (aice_solv . "ANSYS IcePak Solver")
    (aidxf . "ANSYS ICEM CFD DXF to Tetin Converter")
    (aihexa . "ANSYS ICEM CFD Hexa Add-on")
    (aiiges . "ANSYS ICEM CFD IGES to Tetin Converter")
    (aim_mp1 . "ANSYS AIM Standard")
    (aimed . "ANSYS ICEM CFD Mesh Editor")
    (aimshcrt . "ANSYS ICEM CFD Cart3D Mesher")
    (aioutcfd . "ANSYS ICEM CFD Translators for CFD Codes")
    (aioutput . "ANSYS ICEM CFD Output Interfaces")
    (aiprism . "ANSYS ICEM CFD Prism Mesher")
    (aiquad . "ANSYS ICEM CFD Quad Mesher")
    (aitetra . "ANSYS ICEM CFD Tetra Mesher")
    (al4allegro . "ALinks Cadence Allegro integration")
    (al4ansoft . "Import ANF Neutral Files")
    (al4apd . "ALinks Cadence APD integration")
    (al4boardstation . "ALinks Mentor Boardstation Integretion")
    (al4cadvance . "ALinks for cadvance")
    (al4cds . "ALinks for CDS")
    (al4encore . "ALinks Synopsis Encore Integration")
    (al4expedition . "ALinks Mentor Expedition Integration")
    (al4first . "ALinks for First")
    (al4gem . "ALinks for GEM Design Technologies")
    (al4generic . "Import from 3rd Party exported data")
    (al4odb++ . "ALinks ODB++ integration")
    (al4powerpcb . "ALinks PowerPCB integration")
    (al4virtuoso . "ALinks Virtuoso integration")
    (al4zuken . "ALinks Zuken integration")
    (algoryx_momentum . "ANSYS Discovery Algoryx Momentum")
    (alinks_gui . "ALinks GUI")
    (am_module . "ANSYS Additive Manufacturing Module")
    (am_prep . "ANSYS Additive Prep")
    (am_print . "ANSYS Additive Print")
    (amesh . "ANSYS Mesh")
    (amesh_extended . "ANSYS Extended Meshing")
    (ancfx . "ANSYS Mechanical/CFX-Flo")
    (ancode_acc_testing . "ANSYS nCode Accelerated Testing")
    (ancode_composite . "ANSYS nCode DesignLife Composites")
    (ancode_hpc . "ANSYS nCode Parallelization")
    (ancode_standard . "ANSYS nCode Standard")
    (ancode_thermo_mech . "ANSYS nCode DesignLife Thermo-Mechanical Module")
    (ancode_vibration . "ANSYS nCode Vibration")
    (ancode_welds . "ANSYS nCode Welds")
    (ane3 . "ANSYS Mechanical/Emag")
    (ane3fl . "ANSYS Multiphysics")
    (ans_act . "ANSYS Customization Module")
    (ans_dp_pack . "ANSYS HPC Parametric Pack")
    (anshpc . "ANSYS HPC")
    (anshpc_pack . "ANSYS HPC Pack")
    (ansoft_distrib_engine . "ANSYS DSO")
    (ansrom . "ANSYS ROM Builder")
    (ansys . "ANSYS Mechanical")
    (aqwa_pre . "AQWA Pre")
    (aqwa_solve . "AQWA Solve")
    (aspeos_solver . "ANSYS SPEOS Solver")
    (caewbpl3 . "ANSYS DesignSpace")
    (capricatv5 . "CADNexus/CAPRI CAE Gateway for CATIA V5")
    (cfd_base . "CFD Base")
    (cfd_polyflow_ui . "Polyflow UI")
    (cfd_preppost . "CFD PrepPost")
    (cfd_solve_level1 . "CFD Solver - Level 1")
    (cfd_solve_level2 . "CFD Solver - Level 2")
    (cfd_solve_level3 . "CFD Solver - Level 3")
    (deba . "ANSYS DesignSpace Batch")
    (designer_hspice . "ANSYS Designer HSPICE")
    (dfatigue . "ANSYS Fatigue Module")
    (disc_ess . "ANSYS Discovery Essentials")
    (disc_sta . "ANSYS Discovery Standard")
    (disc_ult . "ANSYS Discovery Ultimate")
    (disc_ult_cpuext . "ANSYS Discovery Ultimate CPU Core Extension")
    (disco_level1 . "Discovery - Level 1")
    (disco_level2 . "Discovery - Level 2")
    (disco_level3 . "Discovery - Level 3")
    (discovery_geom . "ANSYS Discovery Ultimate Geometry Interface Bundle")
    (discovery_geom_catia . "ANSYS Discovery Ultimate Geometry Interface Bundle for CATIA")
    (dsdxm . "ANSYS DesignXplorer")
    (dspi . "DesignSpace PlugIn")
    (dyna . "ANSYS LS-DYNA")
    (dynamics . "ANSYS Dynamics")
    (dynapp . "ANSYS LS-DYNA PrepPost")
    (dynardo_osl . "ANSYS optiSLang")
    (dynardo_osp . "ANSYS optiSLang Post")
    (dynardo_sig . "dynardo_sig")
    (dysmp . "ANSYS LS-DYNA Parallel")
    (electronics_desktop . "ANSYS Electronics Desktop")
    (electronics2d_gui . "ANSYS Electronics 2D GUI")
    (electronics3d_gui . "ANSYS Electronics 3D GUI")
    (electronicsckt_gui . "ANSYS Electronics Circuit GUI")
    (emag . "ANSYS Emag")
    (emit_legacy_gui . "Emit Legacy GUI")
    (emit_solve . "Emit Solve")
    (ensemble_25_sim . "Planar EM 2.5D solver")
    (ensight . "ANSYS EnSight")
    (ensight_enterprise . "ANSYS EnSight Enterprise")
    (ensight_jt_exporter . "ANSYS EnSight JT Exporter")
    (ensight_mr . "ANSYS EnSight MR")
    (ensight_vr . "ANSYS EnSight VR")
    (envision_pro . "ANSYS EnVision Pro")
    (explicit_advanced . "Explicit - Advanced")
    (explicit_basic . "Explicit - Basic")
    (filter_synthesis . "ANSYS filter_synthesis")
    (forte_3 . "ANSYS Chemkin Enterprise Forte")
    (granta_mi_pro . "ANSYS Granta MI Pro")
    (granta_selector . "Granta CES Selector")
    (granta_sim_mat_data . "Granta Simulation Materials Data")
    (granta_spl_mat_bundle_1 . "Granta Complete Materials Reference Data Bundle #1")
    (granta_spl_mat_bundle_2 . "Granta Complete Materials Reference Data Bundle #2")
    (granta_spl_mat_bundle_3 . "Granta Complete Materials Reference Data Bundle #3")
    (hfss_solve . "HFSS solver")
    (hfss_transient_solve . "HFSS Transient")
    (hfsssbr_solve . "HFSS SBR+ Solve")
    (keyshot_spaceclaim_hd . "ANSYS Discovery KeyShot HD")
    (keyshot_spaceclaim_pro . "ANSYS Discovery KeyShot Pro")
    (kinemat . "ANSYS Kinematics")
    (linux_catiav5 . "ANSYS Geometry Interface for Catia V5 Linux")
    (m2dfs_qs_solve . "Maxwell 2D Quasistatic Solver")
    (m2dfs_solve . "Maxwell 2D Solver")
    (m3dfs_qs_solve . "Maxwell 3D Quasistatic Solver")
    (m3dfs_solve . "Maxwell 3D solver")
    (mat_designer . "Material Designer")
    (meba . "ANSYS Mechanical Batch")
    (mech_1 . "ANSYS Mechanical Pro")
    (mech_2 . "ANSYS Mechanical Premium")
    (MeshViewer . "Helic products (legacy) - mesh viewer")
    (mpba . "ANSYS Multiphysics Batch")
    (nexxim_ami . "NEXXIM AMI")
    (nexxim_dc . "NEXXIM DC")
    (nexxim_eye . "NEXXIM Eye")
    (nexxim_hb . "NEXXIM HB")
    (nexxim_osc . "NEXXIM OSC")
    (nexxim_tran . "NEXXIM Tran")
    (nexxim_tvnoise . "NEXXIM TVNOISE")
    (optimetrics . "Optimetrics")
    (paramesh . "ANSYS MeshMorpher")
    (pdmiman . "ANSYS Interface for Team Center Engineering")
    (pemag . "PEmag Modeling Tool")
    (pexprt . "PExprt Design Tool")
    (piautoin . "Geometry Interface for Autodesk Inventor")
    (picatv5 . "Geometry Interface for CATIA V5")
    (pimedesk . "Geometry Interface for Mechanical Desktop")
    (piproe . "Geometry Interface for Pro/ENGINEER")
    (pisoledg . "Geometry Interface for SolidEdge")
    (pisolwor . "Geometry Interface for SolidWorks")
    (piug . "Geometry Interface for NX")
    (preppost . "ANSYS Mechanical PrepPost")
    (prf . "ANSYS Professional NLT")
    (prfnls . "ANSYS Professional NLS")
    (RaptorX . "RaptorX - enable GUI")
    (rbfmorph . "ANSYS RBF Morph Module")
    (rd_ckcompute . "ANSYS RD CHEMKIN HPC")
    (rd_ckgraph . "ANSYS RD CHEMKIN GRAPH")
    (rd_ckpost . "ANSYS RD CHEMKIN POST")
    (rd_ckpreproc . "ANSYS RD CHEMKIN PREPROC")
    (rd_ckui . "ANSYS RD CHEMKIN UI")
    (rd_energico . "ANSYS RD ENERGICO")
    (rd_fortejob . "ANSYS RD FORTE JOB")
    (rd_fortemesh . "ANSYS RD FORTE MESH")
    (rd_forteui . "ANSYS RD FORTE UI")
    (rd_forteviz . "ANSYS RD FORTE VIZ")
    (rd_kineticsapi . "ANSYS RD KINETICS")
    (rd_mechforte . "ANSYS RD MECH FORTE")
    (rd_mechmfc . "ANSYS RD Model Fuel")
    (rd_mechsoot . "ANSYS RD Soot")
    (rd_mechwb . "ANSYS RD Workbench")
    (rd_reactionwbui . "ANSYS RD Workbench UI")
    (rdacis . "Geometry Interface for SAT")
    (rdpara . "Geometry Interface for Parasolid")
    (rmxprt_bcm . "RMxprt Brush Commutator Machine")
    (rmxprt_ecm . "RMxprt Electronic Commutator Machine")
    (rmxprt_im . "RMxprt Induction Machine")
    (rmxprt_sym . "RMxprt Synchronus+E298 Machine")
    (RXBackAnnotation . "RaptorX (legacy) - schematic backannotation module")
    (savant_legacy_gui . "Savant Legacy GUI")
    (sherlock . "ANSYS Sherlock")
    (si2d_solve . "Q3D Extractor 2D solver")
    (si3d_solve . "Q3D Extractor 3D solver")
    (simplorer_advanced . "Simplorer Advanced")
    (simplorer_control . "Simplorer Control")
    (simplorer_CProgrInterface . "Simplorer C Programming Interface")
    (simplorer_desktop . "Simplorer Desktop")
    (simplorer_gui . "Simplorer GUI")
    (simplorer_LibSMPS . "Simplorer LibSMPS")
    (simplorer_model_export . "Simplorer Model Export")
    (simplorer_modelica . "Simplorer Modelica")
    (simplorer_modelon_base . "Modelon Base Library")
    (simplorer_modelon_hl . "Hydraulics Library")
    (simplorer_modelon_pl . "Pneumatics Library")
    (simplorer_sim . "Simplorer Simulator")
    (simplorer_sim_entry . "ANSYS Simplorer Entry")
    (simplorer_twin_models . "ANSYS Twin Builder")
    (simplorer_vhdlams . "Simplorer VHDLAMS")
    (SIwave_gui . "SIwave GUI")
    (SIwave_level1 . "SIwave Solver - Level 1 functionality")
    (SIwave_level2 . "SIwave Solver - Level 2 functionality")
    (SIwave_level3 . "SIwave Solver - Level 3 functionality")
    (SIwave_psi_ac_solve . "SIwave PSI Solver")
    (stba . "ANSYS Structural Batch")
    (stl_prep . "ANSYS Discovery Essentials STL Prep for 3D Printng")
    (struct . "ANSYS Structural")
    (symphony_dt_sim . "Discrete Time Domain System Engine Solver")
    (symphony_fd_sim . "Frequency Domain System Engine Solver")
    (TransformerWizard . "VeloceRF (legacy) - transformer synthesis module")
    (twin_builder_dynamic_rom . "twin_builder_dynamic_rom")
    (twin_builder_rapidprototype . "twin_builder_rapidprototype")
    (twin_builder_rapidprototypesim . "twin_builder_rapidprototypesim")
    (twin_builder_runtime_export . "twin_builder_runtime_export")
    (VeloceAdaptiveCompaction . "Helic products (legacy) - compact model extraction")
    (VelocePolygonEngine . "Helic products (legacy) - mesh generation module")
    (VeloceRaptor . "Helic products (legacy) - extraction engine")
    (VeloceRF . "VeloceRF - enable GUI")
    (vmotion . "ANSYS Motion")
    (vmotion_car . "ANSYS Motion Car Toolkit")
    (vmotion_catiai . "ANSYS Motion CATIA Import")
    (vmotion_drtrain . "ANSYS Motion Drivetrain Toolkit")
    (vmotion_links . "ANSYS Motion Links Toolkit")
    (vmotion_mesh . "ANSYS Motion Physics Meshing Toolkit")
    (vmotion_para . "ANSYS Motion Parasolid Translator")
    (vmotion_step . "ANSYS Motion STEP Translator"))
  "Association list: Feature name . feature description.
Status: Ansys v201 from about the beginning of 2020.")

(defconst apdl-begin-keywords
  '("\\*[dD][oO]" "\\*[dD][oO][wW][hH]?[iI]?[lL]?[eE]?"
    "\\*[iI][fF].*[tT][hH][eE][nN]" "\\*[cC][rR][eE][aA][tT][eE]")
  "Regexps describing APDL block begin keywords.")

(defconst apdl-block-begin-regexp
  (concat "\\("
          (mapconcat 'identity apdl-begin-keywords "\\|")
          "\\)\\>")
  "Regexp containing the APDL begin keywords.")

(defconst apdl-process-name "MAPDL"
  "Variable containing the name of an MAPDL interactive process.
Variable is used internally only.")

(defconst apdl-classics-process "Classics"
  "Variable containing the name of an MAPDL GUI process.
Variable is used internally only.")

(defconst apdl-batch-process "MAPDL-Batch"
  "Variable containing the name of an MAPDL batch process.
Variable is used internally only.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- functions ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apdl-is-unix-system-p ()
  "Return t when we are on a Unix system.
gnu/linux, aix, berkeley-unix, hpux, irix, lynxos 3.0.1,
usg-unix-v.  Ansys supports only GNU-Linux 64 and Windows 64 for
the entire Ansys platform with some support of legacy Unices (AIX
IBM, HP-UX HP, SGI, Solaris SUN) for standalone apps will be
provided so I won't restrict some aspects of APDL-Mode to
GNU-Linux."
  (not
   (or (string= system-type "gnu")      ; gnu with the hurd kernel
       (string= system-type "darwin")   ; mac
       (string= system-type "ms-dos")
       (string= system-type "windows-nt")
       (string= system-type "cygwin"))))

(defun apdl-toggle-classics ()
  "Toogle sending output to Ansys Classics.
Try to locate an Ansys Classics GUI or the command dialog box and
switch output to it."
  (interactive)
  (if apdl-classics-flag
      (progn
        (setq apdl-classics-flag nil)
        (message "Disconnected from Classics."))
    (if (apdl-classics-p)
        (progn (setq apdl-classics-flag t)
               (message "Connected to Classics."))
      (error "No Ansys Classics window found"))))

(defun apdl-classics-p ()
  "Check whether Ansys Classics is running.
Return nil if we can't find an MAPDL GUI."
  (let ((aID (replace-regexp-in-string
              "\n" ""
              (shell-command-to-string "~/a-m/X11/xGetClassicsWindow")))
        (eID (replace-regexp-in-string
              "\n" ""
              (shell-command-to-string "~/a-m/X11/xGetFocusWindow"))))
    (if (string= "" aID)
        ;; (error "No Ansys MAPDL window found")
        nil
      (setq apdl-emacs-window-id eID)
      (setq apdl-classics-window-id aID)
      ;;    (setq x-select-enable-clipboard t)             ; for kill-new necessary
      (setq select-enable-clipboard t)                     ; for kill-new necessary
      aID)))

;;;###autoload
(defun apdl-start-classics ()		; C-c C-x
  "Start the Ansys MAPDL Classics graphical user interface.
The output of the solver is captured in an Emacs buffer called
*Classics* under GNU-Linux.  Under Windows it is not possible to
capture the output here, only the exit code.  Please
see `apdl-start-batch-run' for the documentation of these codes.

MAPDL command line options:
-aas : implies -b
-b : implies -i and -o

-lch : undocumented, command line built from the Ansys Launcher
-t : undocumented, maximum solver time -t 10:30:00

V2020R2:

-aas : Enables server mode. When enabling server mode, a custom
  name for the keyfile can be specified using the -iorFile
  option. For more information, see Mechanical APDL as a Server
  User's Guide.

-acc device : Enables the use of GPU hardware to accelerate the
  analysis. See GPU Accelerator Capability in the Parallel
  Processing Guide for more information.

-amfg : Enables the additive manufacturing capability (requires
  an additive manufacturing license). For general information
  about this feature, see AM Process Simulation in Ansys
  Workbench.

-ansexe : In the Ansys Workbench environment, activates a custom
  Mechanical APDL executable.

-b list or nolist : Activates the Mechanical APDL program in
  batch mode. The options -b list or -b by itself cause the input
  listing to be included in the output. The -b nolist option
  causes the input listing not to be included. For more
  information about running Mechanical APDL in batch mode, see
  Batch Mode.

-custom : Calls a custom Mechanical APDL executable. See Running
  Your Custom Executable in the Programmer's Reference for more
  information.

-d device : Specifies the type of graphics device. This option
  applies only to interactive mode. For Linux systems, graphics
  device choices are X11, X11C, or 3D. For Windows systems,
  graphics device options are WIN32 or WIN32C, or 3D.

-db value : Defines the portion of workspace (memory) to be used
  as the initial allocation for the database. The default is 1024
  MB. Specify a negative number (-value) to force a fixed size
  throughout the run; useful on small memory systems.

-dir : Defines the initial working directory. Using the -dir
  option overrides the ANSYS212_WORKING_DIRECTORY environment
  variable.

-dis : Enables Distributed Ansys. See the Parallel Processing
  Guide for more information.

-dvt : Enables Ansys DesignXplorer advanced task (add-on).

-g : Launches the Mechanical APDL program with the Graphical User
  Interface (GUI) on. If you select this option, an X11 graphics
  device is assumed for Linux unless the -d option specifies a
  different device. This option is not used on Windows
  systems. To activate the GUI after Mechanical APDL has started,
  enter two commands in the input window: /SHOW to define the
  graphics device, and /MENU,ON to activate the GUI. The -g
  option is valid only for interactive mode.  Note: If you start
  Mechanical APDL via the -g option, the program ignores any
  /SHOW command in the start.ans file and displays a splash
  screen briefly before opening the GUI windows.

-i inputname : Specifies the name of the file to read input into
  Mechanical APDL for batch processing. On Linux, the preferred
  method to indicate an input file is <.

-iorFile keyfile_name : Specifies the name of the server keyfile
  when enabling server mode. If this option is not supplied, the
  default name of the keyfile is aas_MapdlID.txt. For more
  information, see Mechanical APDL as a Server Keyfile in the
  Mechanical APDL as a Server User's Guide.

-j Jobname : Specifies the initial jobname, a name assigned to
  all files generated by the program for a specific model. If you
  omit the -j option, the jobname is assumed to be file.

-l language : Specifies a language file to use other than US
  English. This option is valid only if you have a translated
  message file in an appropriately named subdirectory in
  /ansys_inc/v212/ansys/docu (or Program Files\\ANSYS
  Inc\\V212\\ANSYS\\docu on Windows systems).

-m workspace : Specifies the total size of the workspace (memory)
  in megabytes used for the initial allocation. If you omit the
  -m option, the default is 2 GB (2048 MB). Specify a negative
  number (-value) to force a fixed size throughout the run.

-machines : Specifies the machines on which to run a Distributed
  Ansys analysis. See Starting Distributed Ansys in the Parallel
  Processing Guide for more information.

-mpi : Specifies the type of MPI to use. See the Parallel
  Processing Guide for more information.

-mpifile : Specifies an existing MPI file (appfile) to be used in
   a Distributed Ansys run. See Using MPI Files in the Parallel
   Processing Guide for more information.

-na : Specifies the number of GPU accelerator devices per machine
  or compute node when running with the GPU accelerator
  feature. See GPU Accelerator Capability in the Parallel
  Processing Guide for more information.

-name value : Defines Mechanical APDL parameters at program
  start-up. The parameter name must be at least two characters
  long. For details about parameters, see the Ansys Parametric
  Design Language Guide.

-np : Specifies the number of processors to use when running
  Distributed Ansys or Shared-memory Ansys. See the Parallel
  Processing Guide for more information.

-o outputname : Specifies the name of the file to store the
  output from a batch execution of Mechanical APDL. On Linux, the
  preferred method to indicate an output file is >.

-p productname : Defines which Ansys product will run during the
  session. For more detailed information about the -p option, see
  Selecting an Ansys Product via the Command Line.

-ppf license feature name : Specifies which HPC license to use
  during a parallel processing run. See HPC Licensing in the
  Parallel Processing Guide for more information.

-rcopy : On a Linux cluster, specifies the full path to the
  program used to perform remote copy of files. The default value
  is /usr/bin/scp.

-s read or noread : Specifies whether the program reads the
  start.ans file at start-up. If you omit the -s option,
  Mechanical APDL reads the start.ans file in interactive mode
  and not in batch mode.

-schost host name : Specifies the host machine on which the
  coupling service is running (to which the co-simulation
  participant/solver must connect) in a System Coupling analysis.

-scid value : Specifies the licensing ID of the System Coupling
  analysis.

-sclic port@host : Specifies the licensing port@host to use for
  the System Coupling analysis.

-scname name of the solver : Specifies the unique name used by
  the co-simulation participant to identify itself to the
  coupling service in a System Coupling analysis. For Linux
  systems, you need to quote the name to have the name recognized
  if it contains a space: ansys212 -scname \"Solution 1\"

-scport port number : Specifies the port on the host machine upon
  which the coupling service is listening for connections from
  co-simulation participants in a System Coupling analysis.

-smp : Enables shared-memory parallelism. See the Parallel
  Processing Guide for more information.

-usersh : Directs the MPI software (used by Distributed Ansys) to
  use the remote shell (rsh) protocol instead of the default
  secure shell (ssh) protocol. See Configuring Distributed Ansys
  in the Parallel Processing Guide for more information.

-v : Returns the Mechanical APDL release number, update number,
  copyright date, customer number, and license manager version
  number.
"
  (interactive)
  ;; initialise system dependent stuff in case this command was
  ;; invoked before APDL-Mode.
  (unless apdl-initialised-flag
    (apdl-initialise))

  (let ((bname (concat "*"apdl-classics-process"*")))
    ;; check against .lock file
    (when (file-readable-p (concat default-directory apdl-job ".lock"))
      (if (yes-or-no-p
           (concat "Warning: There is a \""apdl-job".lock" "\" in "
		   default-directory ". This might indicate that there \
is already a solver running.  Do you wish to kill the lock file? "))
          (delete-file (concat apdl-job ".lock"))
        (error "Starting the MAPDL GUI (Ansys Classics) canceled")))
    (if (y-or-n-p
         (concat
          "Start MAPDL GUI: "
          apdl-ansys-program
          ", license: " apdl-license
          ;; "Start run?  (license type: " (if (boundp
          ;; 'apdl-license) apdl-license)
          (if (> apdl-no-of-processors 4)
              (concat ", No of procs: "
		      (number-to-string apdl-no-of-processors))
            "")
          ", job: " (if (boundp 'apdl-job) apdl-job)
          " in " default-directory ", lic server: "
	  apdl-license-file " "))
        (message "Starting MAPDL in GUI mode (Ansys Classics) ...")
      (error "Starting MAPDL GUI (Ansys Classics) canceled"))
    ;; -d : device
    ;; -g : graphics mode
    ;; -p : license
    ;; -np: no of PROCs, Ansys default 2, >4 HPC licenses req.
    ;; -j : jobname
    ;; v195 new params?: -t -lch
    ;; -g -p ansys -np 2 -j "file" -d 3D
    (start-process apdl-classics-process
		   (if apdl-is-unix-system-flag
		       bname
		     nil)		;nil process not associated with a
					;buffer
                   apdl-ansys-program
                   "-g"
		   "-p" apdl-license
		   "-lch"
		   "-np" (number-to-string apdl-no-of-processors)
		   "-j" apdl-job
		   "-s read"
		   "-l en-us"
		   "-t"
		   "-d 3D" ; 3d device, win32
		   )
    ;; (with-temp-message "bla bla for 10 s"
    ;;   (run-with-timer 10 nil '(lambda())))
    (if apdl-is-unix-system-flag
	(display-buffer bname 'other-window)
      )))

(defun apdl-start-batch-run ()
  "Start an Ansys MAPDL batch run locally on the current script.
The output of the process is captured in an Emacs buffer called
*APDL-Batch*. You should finish your script with a \"finish\"
command, otherwise you'll get an error code 8.

Here are the Ansys MAPDL error codes:

Code    Explanation
-------------------
0	Normal Exit
1	Stack Error
2	Stack Error
3	Stack Error
4	Stack Error
5	Command Line Argument Error
6	Accounting File Error
7	Auth File Verification Error
8	Error in Mechanical APDL or End-of-run
11	User Routine Error
12	Macro STOP Command
14	XOX Error
15	Fatal Error
16	Possible Full Disk
17	Possible Corrupted or Missing File
18	Possible Corrupted DB File
21	Authorized Code Section Entered
25	Unable to Open X11 Server
30	Quit Signal
31	Failure to Get Signal
>32	System-dependent Error
"
  (interactive)
  ;; initialise system dependent stuff in case this command was
  ;; invoked before APDL-Mode.
  (unless apdl-initialised-flag
    (apdl-initialise))

  (let ((bname (concat "*"apdl-batch-process"*")))
    ;; check against .lock file
    (when (buffer-modified-p (current-buffer))
      (if (y-or-n-p
	   (concat "Warning: Buffer \""
		   (file-name-nondirectory buffer-file-name)
		   "\" is modified, do you want to save it?"))
	  (save-buffer)
	(message "APDL file not saved")))
    (when (file-readable-p (concat default-directory apdl-job ".lock"))
      (if (yes-or-no-p
           (concat "Warning: There is a \"" apdl-job ".lock" "\" in "
		   default-directory ". This might indicate that there \
is already a solver running.  Do you wish to kill the lock file? "))
          (delete-file (concat apdl-job ".lock"))
        (error "MAPDL batch run canceled")))
    (if (y-or-n-p
         (concat
          "Start batch run: "
          apdl-ansys-program
	  ", input file: " (buffer-file-name)
          ", license: " apdl-batch-license
          ;; "Start run?  (license type: " (if (boundp
          ;; 'apdl-license) apdl-license)
          (if (> apdl-no-of-processors 4)
              (concat ", No of procs: "
		      (number-to-string apdl-no-of-processors))
            "")
          ", job: " (if (boundp 'apdl-job) apdl-job)
          " in " default-directory ", lic server: "
	  apdl-license-file " "))
        (message "Starting MAPDL batch run ...")
      (error "MAPDL batch run canceled"))
    ;; -d : device
    ;; -g : graphics mode
    ;; -p : license
    ;; -np: no of PROCs
    ;; -j : job
    ;; v195 new params?: -t -lch
    ;; -g -p ansys -np 2 -j "file" -d 3D
    (start-process apdl-batch-process bname
		   ;; (if apdl-is-unix-system-flag
		   ;;     bname
		   ;;   nil)		;nil process not associated with a
		   ;; 			;buffer
                   apdl-ansys-program
                   ;;"-g"
		   "-p" apdl-batch-license
		   "-lch" default-directory
		   "-smp"		;smp: shared memory run
		   "-np" (number-to-string apdl-no-of-processors)
		   "-j" apdl-job	;job name
		   "-s" "noread"	;don't read startup script
		   "-l en-us"		;language
		   "-b"			;batch
		   "-i" (buffer-file-name)
		   "-o" (concat apdl-job ".out")
		   ;; "-t"
		   ;;"-d 3D" ; 3d device, win32
		   )
    ;;(if apdl-is-unix-system-flag
    (display-buffer bname 'other-window)
    ;; )
    ))

;;;###autoload
(defun apdl-start-launcher ()
  "Start the Ansys Launcher."
  (interactive)
  ;; initialise system dependent stuff in case this command was
  ;; invoked before APDL-Mode.
  (unless apdl-initialised-flag
    (apdl-initialise))

  (start-process "Launcher" nil apdl-ansys-launcher)
  (message "Started the Ansys Launcher..."))

(defun apdl-start-wb ()
  "Start the Ansys WorkBench."
  (interactive)
  ;; initialise system dependent stuff in case this command was
  ;; invoked before APDL-Mode.
  (unless apdl-initialised-flag
    (apdl-initialise))
  (start-process "WorkBench" nil apdl-ansys-wb)
  (message "Started the Ansys WorkBench..."))

(defun apdl-write-abort-file ( filename)
  "Open file FILENAME, clear it's contents and insert \"nonlinear\"."
  (interactive)
  (find-file filename)
  (delete-region (point-min) (point-max))
  (insert "nonlinear\n")
  (save-buffer)
  (message "Wrote \"%s\" into \"%s\"." filename default-directory))

(defun apdl-abort-file (&optional arg)
  "Writes an Ansys abort file for stopping the current run.
The abort file does not terminate the current session but
triggers the solver to stop solving in an orderly manner.  This
function prompts for a job name when ARG is negative.  Otherwise
it tries to guess it from the current file (for a /filname
command), if this fails the jobname is taken from the variable
`apdl-job', you can change this variable by calling the equally
named interactive function (i. e. typing \\[apdl-job]) or
setting it directly as Lisp expression (i. e.  typing
\"\\[eval-expression] (setq apdl-job \"jobname\")\", where
jobname is a name of your liking but must be enclosed with double
quotes (\") to represent a lisp string).  The file jobname.abt in
the current directory contains the sole word \"nonlinear\". In
case the `default-directory' is not the working directory of your
respective job, you can change it with \"\\[cd]\"."
  (interactive "p")
  (let ((job apdl-job)
        file
        ;;lfile ; lock file mechanism, might be switched off
        name)
    (cond
     ((< arg 0)                         ; ask for job-name
      (setq name
            (read-string
             (concat "Job name [" job "]: ") nil nil job)))
     (t                                 ; search for /filn
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "/filn.*,\\(\\w+\\)" nil 'noerror)
            (setq name (match-string 1))
          (setq name job)))))
    ;; we might have circumvented the locking with environment
    ;; variable ANSYS_LOCK=OFF, this is interesting if you are using
    ;; Classics as a "viewer" with prepost license (on Windows
    ;; removing the .lock file would be blocked), moreover below
    ;; condition was already interceped already in the apdl-mode menu,

    ;; (setq lfile (concat name ".lock"))
    ;; (unless (file-readable-p lfile)
    ;;   (error "No \"%s\" in %s" lfile default-directory))
    (setq file (concat name ".abt"))
    (if (yes-or-no-p (concat "Write stop file \"" default-directory file "\"? "))
        (progn
          (apdl-write-abort-file file)
          (message "Wrote MAPDL stop file %s in %s." file
                   default-directory))
      (message "Writing MAPDL stop file canceled!"))))

(defun apdl-file-list( regex)
  "List of files matching REGEX in current working directory.
The list is sorted according to their modification times and
might be nil if there is no file matching."
					;  (interactive)
  (let* ((File-name buffer-file-name)
	 (CWD (if File-name ;in buffer with filename?
		  (file-name-directory File-name)
		default-directory))
	 (File-list (directory-files CWD nil regex))
	 (Latest-file-list (sort File-list #'file-newer-than-file-p)))
    Latest-file-list))

(defun apdl-display-error-file (&optional arg)
  "Open the latest \"*.err\" file in the current working directory.
There might be multiple error files when using multiple
processors.   With a prefix argument you can choose between the
list of all \"*.err\" files.

This file will be opened in \"auto-revert-mode\" so that the
current output at the end of the file is always visible.

The error file names consists of the current job name and the
suffix '.out'.  For the job name the variable `apdl-job' is used.
You can change the job name interactively either with the
\"\\[apdl-job]\" or in the customisation facility (by calling
`apdl-customise-apdl').

You might also change the working directory with `M-x cd <RET>'."
  (interactive "P")
  (let* ((Regex "\\.err$")
	 (File-list (apdl-file-list Regex))
	 (Latest-file (car File-list))
	 (File))
    (if arg
	(setq File
	      (completing-read
	       "Choose an \".err\" file (<TAB> to complete): " File-list))
      (setq File Latest-file))
    (when (null File)
      (error "No file is matching \"%s\" in the current directory" Regex))
    (find-file-read-only-other-window File)
    (goto-char (point-max))
    (auto-revert-tail-mode 1)))

(defun apdl-display-out-file (&optional arg)
  "Open the latest \".out\" file in the current working directory.
There might be multiple out files when using multiple processors.
With a prefix argument you can choose between the list of all
\".out\" files.

This file will be opened in \"auto-revert-mode\" so that the
current output at the end of the file is always visible.

The regular out file name consists of the current job name and
the suffix '.out'. For the job name the variable
`apdl-job' is used.  You can change the job name interactively
either with the \"\\[apdl-job]\" or in the customisation
facility (by calling `apdl-customise-apdl').

But you might specify your own output file with the MAPDL
\"/out\" command.  And you might change Emacs' working directory
with `M-x cd <RET>'."
  (interactive "P")
  (let* ((Regex "\\.out$")
	 (File-list (apdl-file-list Regex))
	 (Latest-file (car File-list))
	 (File))
    (if arg
	(setq File
	      (completing-read
	       "Choose an \".out\" file (<TAB> to complete): " File-list))
      (setq File Latest-file))
    (when (null File)
      (error "No file is matching \"%s\" in the current directory" Regex))
    (find-file-read-only-other-window File)
    (goto-char (point-max))
    (auto-revert-tail-mode 1)))

(defun apdl-copy-or-send-above()
  "Copy or send above file content to the current cursor position."
  (interactive)
  (let ((process (get-process
                  (if (boundp 'apdl-process-name)
                      apdl-process-name
                    "APDL"))))
    ;; no-property stuff necessary?????
    ;;   (if (y-or-n-p
    ;;        (concat
    ;;                      "Start this Ansys run: (lic: " apdl-license ", job: " apdl-job ")? "))
    ;;       (message "Starting run...")
    ;;     (error "Run canceled"))
    (cond (apdl-classics-flag
           (clipboard-kill-ring-save (point-min) (point))
           (apdl-send-to-classics)
           (message "Send above file content to the Classics GUI" ))
          ((apdl-process-running-p)
           (comint-send-region process (point-min) (point))
           (display-buffer-other-frame (process-buffer process)))
          (t
           (clipboard-kill-ring-save (point-min) (point)) ; point-min is heeding narrowing
           (message "Copied from beginning of buffer to cursor.")))))

(defvar  apdl-current-region-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  'highlight)
    overlay)
  "The overlay for highlighting currently evaluated region or line.")

(defun apdl-blink-region (start end)
  "Let the region blink between START and END."
  (when apdl-blink-region-flag
    (move-overlay apdl-current-region-overlay start end)
    (run-with-timer apdl-blink-delay nil
                    (lambda ()
                      (delete-overlay apdl-current-region-overlay)))))

(defun apdl-send-to-classics ()
  "Sending clipboard content to the Classics GUI."
  ;;  (let ((win (call-process "/home/uidg1626/script/ctrlv.sh")))
  (let ()
    (sleep-for .5)                    ; wait till user lifts CTRL!
    (call-process (concat apdl-mode-install-directory
                          "X11/xPasteToWin") nil nil nil apdl-classics-window-id)
    (sleep-for .1) ; seems to take at least 0.1 s for the clipboard to copy!
    (call-process (concat apdl-mode-install-directory
                          "X11/xSendReturn") nil nil nil
                          apdl-emacs-window-id apdl-classics-window-id)
    ;; repeating this as workaround... TODO
    (sleep-for .1) ; seems to take 0.1 s for the clipboard to copy!
    (call-process (concat apdl-mode-install-directory
                          "X11/xSendReturn") nil nil nil
                          apdl-emacs-window-id apdl-classics-window-id)))

(defun apdl-send-to-ansys ( &optional move)
  "Send a region to the Ansys MAPDL interpreter.
If the interpreter is not active, just copy it.  If there is no
region marked, send (or copy) the current paragraph.  With a
prefix argument MOVE equal to `4' or `C-u' skip to the next code
line after this region (or paragraph)."
  (interactive "p")
  (let (code
        beg
        end
        (point (point))
        (process (get-process
                  (if (boundp 'apdl-process-name) apdl-process-name)))
        (region (and transient-mark-mode mark-active)))
    ;;                         (region (region-active-p))) ; this is for Emacs-23.1
    ;; make a valid region if possible, when region is not active:
    ;; "region" will be the whole code line (including \n)
    (unless region
      (mark-paragraph))
    (setq beg (region-beginning)
          end (region-end))
    ;; invalidate region
    (deactivate-mark)                ; for Emacs 23.1 no arguments
    ;; (deactivate-mark nil)
    (apdl-blink-region beg end)
    ;; send or copy region or line
    (cond (apdl-classics-flag
           (clipboard-kill-ring-save beg end)
           (apdl-send-to-classics)
           (message "Sent to Classics GUI"))
          ((apdl-process-running-p)
           (setq code (buffer-substring-no-properties beg end))
           (comint-send-string process
                               ;; "\n"); why did I do \n?
                               (concat code ""))
           (display-buffer-other-frame (concat "*" apdl-process-name "*"))
           (message "Sent region to solver."))
          (t
           (clipboard-kill-ring-save beg end)
           (message "Copied region.")))
    (if (= move 4)
        (progn
          (goto-char end)
          (apdl-next-code-line))
      (goto-char point))))

(defun apdl-send-to-apdl-and-proceed ( &optional stay)
  "Send a region or code line to the Ansys MAPDL interpreter.
When there is no running Ansys interpreter process just copy the
respective region or code line to the system clipboard and skip
to the subsequent code line.  With a prefix argument STAY of `4'
or `C-u' copy or send the code and remain at the current cursor
position.  The command can be repeated by typing just the final
character `j' (or `C-j')."
  (interactive "p")
  (let (; code
        beg
        end
        (process (get-process
                  (if (boundp 'apdl-process-name) apdl-process-name)))
        (region (and transient-mark-mode mark-active))
        (block (save-excursion
                 (back-to-indentation)
                 (looking-at apdl-block-begin-regexp)))
        (code (apdl-code-line-p))
        (column (current-column)))
    ;; (region (region-active-p))) ; this is for Emacs-23.1
    ;; make a valid region if possible, when region is not active:
    ;; "region" will be the whole code line (including \n)
    (message "column: %d" column)
    (cond
     (region
      (setq beg (region-beginning)
            end (region-end)))
     (block
         (move-beginning-of-line 1)
       (setq beg (point))
       (apdl-skip-block-forward)
       (setq end (point))
       (setq region t))                ; block considered a region
     (code
      (setq beg (line-beginning-position))
      (save-excursion
        (forward-line 1)
        (setq end (point))))
     (t
      (unless (= stay 4)
        (apdl-next-code-line))
      (error "There was no active region or code line")))
    ;; move cursor to subsequent code line unless stay
    (unless (= stay 4)
      (if (and region
               (< (point) end))
          (exchange-point-and-mark))
      (move-to-column column)        ; stay in the previous column
      (apdl-next-code-line))
    ;; invalidate region
    (setq mark-active nil)
    ;; set-transient-map since 24.4
    (when (fboundp 'set-transient-map)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map "j"
           'apdl-send-to-apdl-and-proceed)
         (define-key map "\C-j"
           'apdl-send-to-apdl-and-proceed)
         map)))
    ;; send or copy region or line
    (cond (apdl-classics-flag
           (clipboard-kill-ring-save beg end)
           (if (fboundp 'set-transient-map)
               (if region
                   (message "Sent region, type \"j\" or \"C-j\" to sent next line or block.")
                 (message "Sent line, type \"j\" or \"C-j\" to sent next line or block."))
             (if region
                 (message "Sent region.")
               (message "Sent line.")))
           (apdl-send-to-classics))
          ((apdl-process-running-p)
           (setq code (buffer-substring-no-properties beg end))
           (comint-send-string process
                               ;; "\n"); why did I do \n?
                               (concat code ""))
           (display-buffer-other-frame (concat "*" apdl-process-name "*"))
           ;; Issue a hint to the user
           (if (fboundp 'set-transient-map)
               (if region
                   (message "Sent region, type \"j\" or \"C-j\" to sent next line or block.")
                 (message "Sent line, type \"j\" or \"C-j\" to sent next line or block."))
             (if region
                 (message "Sent region.")
               (message "Sent line."))))
          (t
           (clipboard-kill-ring-save beg end)
           (if (fboundp 'set-transient-map)
               (if region
                   (message "Copied region, type \"j\" or \"C-j\" to copy next line or block.")
                 (message "Copied line, type \"j\" or \"C-j\" to copy next line or block."))
             (if region
                 (message "Copied region.")
               (message "Copied line.")))))))

(defun apdl-process-running-p ()
  "Return nil if no Ansys interpreter process is running."
  (let ((proc (get-process
               (if (boundp 'apdl-process-name) apdl-process-name))))
    (if proc
        (string= "run" (process-status proc))
      nil)))

(defun apdl-query-apdl-command ( &optional arg)
  "Ask for a string which will be sent to the interpreter.
The string is completable to all current APDL commands and with
an optional prefix argument ARG the current command line is the
initial input."
  (interactive "P")
  (unless (or apdl-classics-flag (apdl-process-running-p))
    (error "No MAPDL process is running"))
  (let (s)
    (if arg
        (setq s (read-minibuffer "Send to interpreter: "
                                 (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))))
      (setq s (completing-read "Send to interpreter: "
                               apdl-help-index nil 'confirm)))
    (cond
     (apdl-classics-flag
      (kill-new s)
      (apdl-send-to-classics))
     (t
      (comint-send-string (get-process
                           (if (boundp 'apdl-process-name)
                               apdl-process-name)) (concat s "\n"))
      (display-buffer (concat "*" apdl-process-name "*") 'other-window)))))

;; from product launcher: Tools ->
;; "C:\CAx\App\ANSYS Inc\v201\ansys\bin\winx64\MAPDL.exe"  -g -p ansys -lch -dir "H:\" -j "file" -s read -l en-us -t -d 3D
;; Test function
;; (defun apdl-start-mapdl ()
;;   "Start the MAPDL interpreter (Ansys Classics) under Windows."
;;   (interactive)
;;   ;(setq default-directory "c:/CAx/")
;;   (start-process   ; asynchronous process
;;    "MAPDL" "*MAPDL*" ; "c:/CAx/App/ANSYS Inc/v201/ansys/bin/winx64/MAPDL.exe"
;;    apdl-ansys-program
;;       "-g" "-p" apdl-license "-lch" (concat "-j " apdl-job) "-s read" "-l en-us" "-t" "-d 3D"
;; ;;      (concat "-g -p " apdl-license " -j " apdl-job "-d 3D")
;; ;;      (concat " -g -p " apdl-license )
;;       ;; (concat "-dir " default-directory)
;;    ))

(defun apdl-start-ansys () 		; C-c C-m
  "Start the MAPDL interpreter under Linux or the launcher under Windows.
For the interpreter process summarise the run's configuration
first.  The specified No of cores is not shown if they are chosen
smaller than 3 (see `apdl-number-of-processors')."
  (interactive)
  (let (apdl-process-buffer)
    (when (apdl-process-running-p)
      (error "An Ansys interpreter is already running under Emacs"))
    (message "Preparing an Ansys interpreter run...")
    (if (y-or-n-p
         (concat
          "Start run?  (version: "
          apdl-current-ansys-version
          ", license type: " apdl-license
          (if (>= apdl-no-of-processors 3)
              (concat ", No of processors: " (number-to-string apdl-no-of-processors))
            "")
          ", job: " (if (boundp 'apdl-job) apdl-job)
          " in " default-directory ", server: " apdl-license-file))
        (message "Starting the Ansys interpreter...")
      (error "Function apdl-start-ansys canceled"))
    (setq apdl-process-buffer
          (make-comint apdl-process-name apdl-ansys-program nil
                       (if (>= apdl-no-of-processors 3)
                           (concat "-np " (number-to-string apdl-no-of-processors)
                                   " -p " apdl-license " -j " apdl-job)
                         (concat "-p " apdl-license " -j " apdl-job))))
    ;;  (comint-send-string (get-process apdl-process-name) "\n")
    (setq apdl-classics-flag nil)
    (display-buffer apdl-process-buffer 'other-window)
    ;;  (switch-to-buffer apdl-process-buffer)
    (other-window 1)
    (setq comint-prompt-regexp "BEGIN:\\|PREP7:\\|SOLU_LS[0-9]+:\\|POST1:\\|POST26:\\|RUNSTAT:\\|AUX2:\\|AUX3:\\|AUX12:\\|AUX15:")
    (font-lock-add-keywords nil (list comint-prompt-regexp))))

(defun apdl-kill-ansys ()
  "Kill the current Ansys run under Emacs.
The function asks for confirmation before actually killing the
process.  Warning: Ansys writes a lock file (jobname.lock) if the
process is killed and not regularly exited.  You should prefer
the function `apdl-exit-ansys'."
  (interactive)
  (unless (apdl-process-running-p)
    (error "No active Ansys solver process"))
  (if (yes-or-no-p
       "Do you want to kill the running MAPDL solver?")
      (progn
        ;;                     (message "Killing run...")
        (delete-process (get-process apdl-process-name))
        (message "Killing Mechanical APDL run...done.")
        (display-buffer (concat "*" apdl-process-name "*") 'otherwindow))
    (message "Killing of Ansys run canceled.")))

(defun apdl-exit-ansys ()
  "Exit normally the current Ansys run under Emacs.
The function asks for confirmation before exiting the process
with the APDL /EXIT,all command which saves all model data."
  (interactive)
  (unless (apdl-process-running-p)
    (error "Error: No active Ansys process"))
  (if (yes-or-no-p
       "Do you want to exit the Ansys run?")
      (progn
        (message "Trying to exit run ...")
        (process-send-string (get-process apdl-process-name)
			     "finish $ /exit,all\n"))
    ;; (setq mode-line-process (format ":%s" (process-status apdl-process)))
    ;; (force-mode-line-update))
    (error "Exiting of Ansys run canceled")))

;; Unfortunately there's no html TOC for the locally installed help
;; pages yet (v201) so it remains only the online help
;; https://ansyshelp.ansys.com/account/secured?returnurl=/Views/Secured/main_page.html?lang=en
(defun apdl-start-ansys-help-page ()
  "Start the Ansys main online help page."
  (interactive)
  (let ((file "main_page.html"))
    (browse-url (concat "https://ansyshelp.ansys.com/"
     			"account/secured?returnurl=/Views/Secured/"
     			file))
    ;; (if (string> apdl-current-ansys-version "v200")
    ;; 	(browse-url (concat "https://ansyshelp.ansys.com/"
    ;; 			    "account/secured?returnurl=/Views/Secured/corp/"
    ;; 			    apdl-current-ansys-version "/en/" file))
    ;;   (browse-url (concat "https://ansyshelp.ansys.com"
    ;; 			  "/account/secured?returnurl=/Views/Secured/corp/"
    ;; 			  apdl-current-ansys-version "/" file)))
    ))

;; 2021-10-14
(defun apdl-start-ansys-customer-portal-site ()
  "Start the Ansys Customer Portal site in a browser.
You need to register or have an account."
  (interactive)
  (browse-url "https://support.ansys.com/Home/HomePage"))

(defun apdl-start-ansys-help ()
  "Start the Ansys Help Viewer.
When there is no local help installed or online help is
configured you will be redirected to the main Ansys online help
page.

Alternatively, you can use the APDL command line \"/SYS,
anshelp201\" for a Unix system (AnsysHelpViewer.exe for a Windows
operating system) when running Ansys MAPDL interactively.
Provided that 'anshelp201' is found in the search paths for
executables (these are stored in the PATH environment variable on
both systems)."
  (interactive)
  (unless apdl-ansys-help-program
    (error "Help executable `apdl-ansys-help-program' not set"))
  (progn
    (cond
     (apdl-is-unix-system-flag
      (start-process "apdl-ansys-help-program" nil apdl-ansys-help-program)
      (message "Started the Ansys Help Viewer..."))
     ((string= system-type "windows-nt")
      (if (fboundp 'w32-shell-execute)
          (w32-shell-execute
	   "Open" (concat "\"" apdl-ansys-help-program "\"") )
        (error "Function w32-shell-execute not bound"))
      (message "Started the Ansys Help Viewer..."))
     (t
      (error "Can only start the Ansys help on Windows and GNU-Linux systems")))))

;; thing-at-point command, get- or parametric function
;; ~ and * are symbol components under a-m mode, paren ( is not!

;; 1. we are on a keyword, first functions, then element or command
;; 2. we are behind a keyword, first function, then e. or c.
;; 3. we are before a keyword (for example in indentation)
;; 4. we are in a default command with only commas

;; defun is used in apdl-browse-apdl-help searching in apdl-help-index
(defun apdl-search-keyword()
  "Search and return a valid keyword string from the current line.
Signal an error if the cursor is in an empty line or no valid
keyword from `apdl-help-index' is found."
  (when (apdl-in-empty-line-p)
    (error "Cannot find a keyword in an empty line"))
  (let* ((pt (point))
         (re "~/*[:word:]") 		; keyword components
         (lbp (line-beginning-position))
         ;; (eolp (save-excursion (end-of-line) (point)))
	 ;; 1. we are on a keyword
         (str (upcase (buffer-substring-no-properties
                       (save-excursion
                         (+ pt (skip-chars-backward re lbp)))
                       (save-excursion
                         (+ pt (skip-chars-forward re)
			    (skip-chars-forward "("))))))
         (cmpl (try-completion str apdl-help-index)))
    ;; (message "keyword: %s" str)

    ;; 2. possibly behind a function keyword
    (when (or (string= str "") (not cmpl))
      (save-excursion
	(skip-chars-backward "^(")
	(setq pt (point)
	      str (upcase
		   (buffer-substring-no-properties
		    (+ pt (skip-chars-backward "(" lbp)
		       (skip-chars-backward re lbp))
		    pt))
	      cmpl (try-completion str apdl-help-index))))

    ;; 3. try command keyword at indentation
    (when (or (string= str "") (not cmpl))
      ;; we are surrounded by whities, or not on a valid keyword, try
      ;; the first command (possibly behind an comment char)
      (save-excursion
        (move-beginning-of-line 1)
        (skip-chars-forward " !")
        (setq pt (point)
              str (upcase
                   (buffer-substring-no-properties pt
                                                   (+ pt (skip-chars-forward re))))))
      (setq cmpl (try-completion str apdl-help-index)))

    ;; output
    (cond ((stringp cmpl)               ; not unique
           cmpl)
          ((not cmpl)
           (error "\"%s\" is not a valid keyword" str))
          (t                            ; perfect match
           str))))

(require 'browse-url)

(defun apdl-browse-ansys-apdl-manual ()
  "Open the Ansys Parametric Design Language Guide in a browser."
  (interactive)
  (let ((file "ans_apdl/Hlp_P_APDLTOC.html")
        (path apdl-ansys-help-path))
    (if path
	;; file:/// is working since at least Emacs-27 (windows and
	;; linux) 2021-08, see -browse-apdl-help!!!
        (browse-url-of-file (concat "file:///" path file))
      (if (string> apdl-current-ansys-version "v200")
          (browse-url
	   (concat "https://ansyshelp.ansys.com/"
		   "account/secured?returnurl=/Views/Secured/corp/"
		   apdl-current-ansys-version "/en/" file))
        (browse-url (concat "https://ansyshelp.ansys.com/"
			    "account/secured?returnurl=/Views/Secured/corp/"
			    apdl-current-ansys-version "/" file))))))

(defun apdl-browse-apdl-help (&optional arg)
  "Browse the Ansys help for APDL commands, elements and other topics.

ATTENTION: If you are using the Ansys online help - default since
V19 - then first you need to register at the Ansys help site!
Please start some help topic with an Ansys application first,
then you can use APDL-Mode for pin-pointing your topics.  For
faster access I recommend installing the local Ansys help.

The function is looking for the next keyword before or at the
cursor location.  If that fails the command is looking for the
keyword at the line beginning.  (This is working in a comment
line as well.)  If there is an optional argument ARG, query for a
manual search keyword input.  Besides the regular command and
element names you can also input predefined help sections or
element categories.  The use of completions is advisable, for
example: Type the character `\"' and the <TAB> key and you will
see completions of the following:

Help sections:

\"RELEASE NOTES\"
\"CONTACT TECHNOLOGY GUIDE\"
\"PARAMETRIC DESIGN LANGUAGE GUIDE\"
\"STRUCTURAL ANALYSIS GUIDE\"
\"ADVANCED ANALYSIS TECHNIQUES GUIDE\"
\"MATERIAL MODELS\"

Element categories:

\"SHELLS\" = \"ALL\"SHELLS
\"PLANES\" = \"ALL\"PLANES
\"SOLIDS\" = \"ALL\"SOLIDS

\"ALL\" -- Element categories

\"ALL\"BEAMS -- Beam elem.
\"ALL\"CIRCUS -- Electric/magnetic circuit elem.
\"ALL\"COMBINS -- Combination elem.
\"ALL\"COMBIS -- Combination elem.
\"ALL\"CONTACS -- Contact elem.
\"ALL\"CONTAS -- Contact elem.
\"ALL\"CPTS -- Coupled pore-pressure mechanical solid elem.
\"ALL\"FLUIDS -- Fluid elem.
\"ALL\"FOLLW -- Follower load elem.
\"ALL\"HFS -- High Frequency elem.
\"ALL\"HSFLDS -- Hydrostatic elem.
\"ALL\"INFINS -- Infinite Boundary/Solid elem.
\"ALL\"INTERS -- Interface magnetic/gasket/cohesive elem.
\"ALL\"LINKS -- Link radiation/conduction/convection/thermal-electric/spar/\
elem.
\"ALL\"MASS -- Mass elem
\"ALL\"MATRIXS -- Matrix stiffness/damping/super elem
\"ALL\"MESHS -- Mesh facet elem.
\"ALL\"MPCS -- Structural multipoint constraint
\"ALL\"PIPES -- Pipe/Elbow elem.
\"ALL\"PLANES -- Plane elem.
\"ALL\"PRETS -- Pretension combination elem.
\"ALL\"REINF -- Reinforcing elem.
\"ALL\"ROMS -- Reduced order electrostatic-structural coupled-field elem.
\"ALL\"SHELLS -- Shell elem.
\"ALL\"SOLIDS -- Solid elem.
\"ALL\"SOLSHS -- Structural solid shell elem.
\"ALL\"SOURCS -- Magnetic Electric Current source elem.
\"ALL\"SURFS -- Surface elem.
\"ALL\"TARGES -- Target elem.
\"ALL\"TRANS -- Electromechanical solid/transducer elem."
  (interactive "P")
  (let (file
        (path apdl-ansys-help-path)
        command)
    (if arg
        (setq command (completing-read "Browse help for keyword [TAB to complete]: "
                                       apdl-help-index))
      (setq command (apdl-search-keyword)))
    (setq file (nth 1 (assoc-string command apdl-help-index t)))
    (unless  file
      (error "Keyword \"%s\" is not uniquely completable" command))

    ;; ;; we must adapt the path to various items!
    ;; ;;  since 201 not any longer

    ;; (cond
    ;;  ((string-match "_C_" file)
    ;;   (setq file (concat "ans_cmd/" file)))
    ;;  ((string-match "_E_" file)
    ;;   (setq file (concat "ans_elem/" file)))
    ;;  ((string-match "_P_APDL" file)
    ;;   (setq file (concat "ans_apdl/" file)))
    ;;  ((string-match "_G_AdvTOC" file)
    ;;   (setq file (concat "ans_adv/" file)))
    ;;  ((string-match "_G_StrTOC" file)
    ;;   (setq file (concat "ans_str/" file)))
    ;;  ((string-match "ans_mat.html" file)
    ;;   (setq file (concat "ans_mat/" file)))
    ;;  ((string-match "ctectoc.html" file)
    ;;   (setq file (concat "ans_ctec/" file)))
    ;;  ((string-match "ansysincrelease" file)
    ;;   (setq file (concat "ai_rn/" file)))
    ;;  ((string-match "ansys.theory" file)
    ;;   (setq file (concat "ans_thry/" file))))
    (if path
	(progn
	  (when (eq browse-url-browser-function 'eww-browse-url)
	    (switch-to-buffer-other-window nil))
	  ;; file:/// is not working with tramp remotely 2020-04-03
	  ;; (browse-url-of-file (concat "file:/" path file)))

	  ;; Emacs 27.1. file:/ is working on windows, but not for eww
	  ;; with svg images!  And file:/ is not working under linux
	  ;; for eww, reverting to file:/// 2021-08-28
	  (browse-url-of-file (concat "file:///" path file)))
      (unless apdl-current-ansys-version
        (error "Please set `apdl-current-ansys-version'"))
      ;; since v201: Changed the path to the online help!
      (if (string> apdl-current-ansys-version "v200")
	  (browse-url
	   (concat
	    "https://ansyshelp.ansys.com/"
	    "account/secured?returnurl=/Views/Secured/corp/"
	    apdl-current-ansys-version "/en/" file))
        (browse-url
	 (concat
	  "https://ansyshelp.ansys.com/"
	  "account/secured?returnurl=/Views/Secured/corp/"
	  apdl-current-ansys-version "/" file))))))

(defun apdl-process-status ()
  "Show the process status in the Emacs command line (minibuffer).

    'run'
          for a process that is running.
    'stop'
          for a process that is stopped but continuable.
    'exit'
          for a process that has exited.
    'signal'
          for a process that has received a fatal signal.
    'open'
          for a network connection that is open.
    'closed'
          for a network connection that is closed.  Once a connection
          is closed, you cannot reopen it, though you might be able to
          open a new connection to the same place.
    'connect'
          for a non-blocking connection that is waiting to complete.
    'failed'
          for a non-blocking connection that has failed to complete.
    'listen'
          for a network server that is listening.
    'nil'
          if PROCESS-NAME is not the name of an existing process."
  (interactive)
  (let ((status (process-status apdl-process-name)))
    (if status
        (message "Ansys process is in state \"%s\"" ; process identification No: %d"
                 (symbol-name status))
      (message "No Ansys interpreter process is running."))))

(defun apdl-batch-process-status ()
  "Show the process status in the Emacs command line (minibuffer).

    'run'
          for a process that is running.
    'stop'
          for a process that is stopped but continuable.
    'exit'
          for a process that has exited.
    'signal'
          for a process that has received a fatal signal.
    'open'
          for a network connection that is open.
    'closed'
          for a network connection that is closed.  Once a connection
          is closed, you cannot reopen it, though you might be able to
          open a new connection to the same place.
    'connect'
          for a non-blocking connection that is waiting to complete.
    'failed'
          for a non-blocking connection that has failed to complete.
    'listen'
          for a network server that is listening.
    'nil'
          if PROCESS-NAME is not the name of an existing process."
  (interactive)
  (let ((status (process-status apdl-batch-process)))
    (if status
        (message "Ansys batch process is in state \"%s\"" ; process identification No: %d"
                 (symbol-name status))
      (message "No Ansys batch process is running."))))


(defun apdl-occur ()
  "Show selected licenses in an occur buffer.
Interesting licenses are compiled in the string
`apdl-license-occur-regexp' which is used in the function
`apdl-license-status'."
  (interactive)
  (occur
   (mapconcat 'identity apdl-license-occur-regexp "\\|")))

;;;###autoload
(defun apdl-user-license-status ()
  "Display only licenses which are used by the user.
Show the status for the user `apdl-username' in a separate buffer
*User-Licenses*.  The license type variable `apdl-license'
determines a highlighting of the license server summary rows.
There are additional keybindings for the license buffer
*User-licenses*:

- `d' for a license description of all available features
- `g' for updating the license status
- `?' and `h' for showing this help,
- `l' for the general license status and
- `q' for burying the *User-licenses* buffer
- `Q' for killing the Buffer"
  (interactive)
  (require 'apdl-mode)
  (unless apdl-initialised-flag
    (apdl-initialise))
  (cond
   ((and apdl-lmutil-program apdl-license-file)
    ;; lmutil calls with many license server specified takes loooooonnnnggg
    (message "Retrieving user licenses, this may take some time...")
    (with-current-buffer (get-buffer-create "*User-licenses*")
      (delete-region (point-min) (point-max)))
    ;; syncronous call
    (call-process apdl-lmutil-program nil "*User-licenses*" nil "lmstat" "-c "  apdl-license-file  "-a")
    (let ((user apdl-username))
      (with-current-buffer "*User-licenses*"
        ;; below key settings are only allowed in fundamental mode
        ;; otherwise it supposedly overwrites major modes keymaps!
        (local-set-key (kbd "Q") 'kill-this-buffer)
        (local-set-key (kbd "q") 'bury-buffer)
        (local-set-key (kbd "d") (lambda ()
				   (interactive)
				   (apdl-license-status 1)))
        (local-set-key (kbd "h") (lambda ()
				   (interactive)
				   (describe-function
				    'apdl-user-license-status)))
        (local-set-key (kbd "?") (lambda ()
				   (interactive)
				   (describe-function
				    'apdl-user-license-status)))
        (local-set-key (kbd "l") 'apdl-license-status)
        (local-set-key (kbd "g") 'apdl-user-license-status)

        ;; remove empty lines
        (goto-char (point-min))
        (delete-matching-lines "^$")

        ;; remove lines with expiry: date
        (goto-char (point-min))
        (delete-matching-lines "expiry:")

        ;; shorten lines
        (goto-char (point-min))
        (while (re-search-forward "Total of \\|Users of " nil t)
          (replace-match ""))

	;; keep either lines with user or lines not beginning with
	;; whitespace
	(goto-char (point-min))
	(keep-lines (concat "^[^ ]\\|" user))

	;; keep double line only with user in 2nd line
	(goto-char (point-min))
	(keep-lines (concat "^[^ ]+.*\n[ ]+" user))

        ;; add some comments
        (goto-char (point-min))
        (insert (propertize
                 (concat " -*- User license status" ;" from " apdl-license-file
                         " type h or ? for help -*-\n") 'face 'match))
        (goto-char (point-max))
        (insert "\n")
        (insert (propertize (concat (current-time-string) "\n")
                            'face 'match))

        ;;  on Windows the license stat buffer doesn't move to point without:
        (unless apdl-is-unix-system-flag
          (set-window-point (get-buffer-window "*User-licenses*") (point)))))
    (unless (equal (current-buffer) (get-buffer "*User-licenses*"))
      (display-buffer "*User-licenses*" 'otherwindow))
    (message "Updated user license status: %s." (current-time-string)))
   (t
    (message "No license information or lmutil program found"))))

;;;###autoload
(defun apdl-license-status (&optional features)
  "Display the lmutil license status.
With the optional argument FEATURES non nil summarise all license
features with the Ansys license feature description.  Show the
status and summary in a separate buffer, the license type
variable `apdl-license' determines a highlighting of the license
server summary rows.  There are additional keybindings for the
license buffer *APDL-licenses*:

- `g' updating the license status,
- `d' updating the license status with feature descriptions
- `o' for showing an occur buffer with the interesting licenses from
      `apdl-license-occur-regexp',
- `u' for displaying all the user license,
- `?' and `h' for showing this help,
- `Q' for killing the Buffer and
- `q' for burying it below another buffer."
  (interactive "P")
  (require 'apdl-mode)
  (let (bol eol match desc (lic-buffer "*APDL-licenses*"))
    (unless apdl-initialised-flag (apdl-initialise))
    (cond
     ((and apdl-lmutil-program apdl-license-file)
      ;; lmutil calls with many license servers specified takes loooooonnnnggg
      (if features
	  (message
	   "Retrieving summary license (%s) status, please wait." apdl-license)
	(message
	 "Retrieving lmutil license (%s) status, please wait..." apdl-license))
      (with-current-buffer (get-buffer-create lic-buffer)
	(delete-region (point-min) (point-max)))
      ;; syncronous call
      (call-process apdl-lmutil-program nil lic-buffer nil "lmstat" "-c "
		    apdl-license-file  "-a")
      (with-current-buffer lic-buffer
	(setq-local truncate-lines t)

        ;; remove uninteresting licenses

        ;; (goto-char (point-min))
        ;; (delete-matching-lines "\\<acfx\\|\\<ai\\|\\<wbunix\\|\\<rdacis\\>")

        ;; below key settings are only allowed in fundamental mode
        ;; otherwise it supposedly overwrites major modes keymaps!
        (local-set-key (kbd "Q") 'kill-this-buffer)
        (local-set-key (kbd "q") 'bury-buffer)
        (local-set-key (kbd "d") (lambda ()
				   (interactive)
				   (apdl-license-status 1)))
        (local-set-key (kbd "g") 'apdl-license-status)
        (local-set-key (kbd "o") 'apdl-occur)
        (local-set-key (kbd "h") (lambda ()
				   (interactive)
				   (describe-function
				    'apdl-license-status)))
        (local-set-key (kbd "?") (lambda ()
				   (interactive)
				   (describe-function
				    'apdl-license-status)))
        (local-set-key (kbd "u") 'apdl-user-license-status)

        ;; remove empty lines
        (goto-char (point-min))
        (delete-matching-lines "^$")

        ;; remove lines with expiry: date
        (goto-char (point-min))
        (delete-matching-lines "expiry:")

        ;; ;; sorting
        ;; (sort-lines nil (point-min) (point-max))
	(when features
	  ;; remove user parts
	  (goto-char (point-min))
	  (while (not (eobp))
	    (push-mark (point))
	    (search-forward-regexp "Users of " nil t)
	    (beginning-of-line)
	    (delete-region (mark) (point))
	    (forward-line 1))
	  (goto-char (point-max))
	  (push-mark (point))
	  (search-backward-regexp "Users of " nil t)
	  (forward-line 1)
	  (delete-region (mark) (point))
	  ;; shorten lines
	  (goto-char (point-min))
	  (while (re-search-forward "Total of \\|Users of " nil t)
	    (replace-match ""))
	  ;; insert explanations
	  (goto-char (point-min))
	  (while (re-search-forward "^\\(.+\\):" nil t)
	    (setq match (match-string 1))
	    (message "match string: %s" match)
	    (setq desc
		  (cdr
		   (assoc
		    (intern match)
		    apdl-license-description)))
	    (message "assoc match: %s" desc)
	    (when match
	      (replace-match (concat match ":" desc)))))

        ;; shorten lines
        (goto-char (point-min))
        (while (re-search-forward "Total of \\|Users of " nil t)
          (replace-match ""))

        ;; add some comments
        (goto-char (point-min))
        (insert (propertize
                 (concat " -*- License status" ; from " apdl-license-file
                         ", type h or ? for help -*-\n") 'face 'match))
        (goto-char (point-max))
        (insert "\n")
        (insert (propertize (concat (current-time-string) "\n")
                            'face 'match))

        ;; higlight current -license
        (goto-char (point-min))
	;; issue with the apdl-license-categories "ansys" and the
	;; apdl-license-file server name.  LMUTIL puts a colon after
	;; the license types, used to make the search unique and "^"
	;; suppresses composed license names e.g. "cfdpreppost"
        (search-forward-regexp (concat "^" apdl-license ":") nil t)
        (forward-line)
        (setq eol (point))
        (forward-line -1)
        (setq bol (point))
        (put-text-property bol eol 'face 'font-lock-warning-face)
        ;;  on Windows the license stat buffer doesn't move to point without:
        (unless apdl-is-unix-system-flag
          (set-window-point (get-buffer-window lic-buffer) (point))))
      (unless (equal (current-buffer) (get-buffer lic-buffer))
	(display-buffer lic-buffer 'otherwindow))
      (message "Updated license status: %s." (current-time-string)))
     (t
      (message "No license information or lmutil program found")))))

;; starting in GUI mode (/menu,on) does inhibit the process intercommunication
;; => /menu,graph
;; env variable ANS_CONSEC=YES disables dialog boxes

(defun apdl-start-graphics ()
  "Start - in interactive mode - the MAPDL display window."
  (interactive)
  (unless
      (apdl-process-running-p)
    (error "No interactive MAPDL process is running"))
  (progn (comint-send-string
          (get-process apdl-process-name)
          ;; "/show,X11c\n/menu,grph\n"
          "/show,3d\n/menu,grph\n")
         (display-buffer (concat "*" apdl-process-name "*") 'other-window)))

(defun apdl-start-pzr-box ()
  "Start the Ansys Pan/Zoom/Rotate dialog box."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/ui,view\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/ui,view\n")
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or Classics GUI can be found"))))

(defun apdl-iso-view ()
  "Show current display in isometric view (/view,,1,1,1)."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/view,,1,1,1\n/replot\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/view,,1,1,1\n/replot\n")
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or Classics GUI can be found"))))

(defun apdl-move-up (arg)
  "Move geometry up ARG steps in the graphics window.
A Negative ARG moves ARG steps down."
  (interactive "p")
  (cond
   (apdl-classics-flag
    (kill-new (format "/focus,,,-0.25*(%d),,1\n/replot\n" arg))
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name)
                        (format "/focus,,,-0.25*(%d),,1\n/replot\n" arg))
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No Ansys process is running"))))

(defun apdl-move-down (arg)
  "Move geometry down ARG steps in the graphics window.
A Negative ARG moves ARG steps up."
  (interactive "p")
  (apdl-move-up (- arg)))

(defun apdl-move-right (arg)
  "Move geometry right ARG steps in the graphics window.
A Negative ARG moves ARG steps left."
  (interactive "p")
  (cond
   (apdl-classics-flag
    (kill-new (format "/focus,,-0.25*(%d),,,1\n/replot\n" arg))
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name)
                        (format "/focus,,-0.25*(%d),,,1\n/replot\n" arg))
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No Ansys process is running"))))

(defun apdl-move-left (arg)
  "Move geometry left ARG steps in the graphics window.
A Negative ARG moves ARG steps right."
  (interactive "p")
  (apdl-move-right (- arg)))

(defun apdl-zoom-in ()
  "Zoom into the graphics window."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/dist,,.7,1\n/replot\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/dist,,.7,1\n/replot\n") ; valid in any processor
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or Classics GUI can be found"))))

(defun apdl-zoom-out ()
  "Zoom out of the graphics window."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/dist,,1.4,1\n/replot\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/dist,,1.4,1\n/replot\n") ; valid in any processor
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or no Classics GUI can be found"))))

(defun apdl-replot ()
  "Replot the Ansys graphics screen."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/replot\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/replot\n") ; valid in any processor
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or no Classics GUI can be found"))))

(defun apdl-fit ()
  "Fit FEA entities to the Ansys interactive graphics screen."
  (interactive)
  (cond
   (apdl-classics-flag
    (kill-new "/dist\n/replot\n")
    (apdl-send-to-classics))
   ((apdl-process-running-p)
    (comint-send-string (get-process apdl-process-name) "/dist\n/replot\n")
    (display-buffer (concat "*" apdl-process-name "*") 'other-window))
   (t
    (error "No interactive MAPDL process running or no Classics GUI can be found"))))

(defun apdl-ansys-program ( exec)
  "Change the Ansys executable name to EXEC.
And set the variable `apdl-ansys-program' accordingly if the for
executable EXEC can be found on the system's search path."
  (interactive "FAnsys interpreter executable: ")
  (when (string= exec "")
    (setq exec apdl-ansys-program))
  (setq apdl-ansys-program exec)
  (if (executable-find exec)
      (message "apdl-ansys-program is set to \"%s\"." apdl-ansys-program)
    (error "Cannot find Ansys interpreter executable \"%s\" on the system" exec)))

(defun apdl-ansys-help-program ( exec)
  "Change the Ansys help executable to EXEC and check for its existence.
And store the value EXEC in the variable `apdl-ansys-help-program'."
  (interactive "FAnsys help executable: ")
  (when (string= exec "")
    (setq exec apdl-ansys-help-program))
  (setq apdl-ansys-help-program exec)
  (if (executable-find exec)
      (message "apdl-ansys-program is set to \"%s\"." exec)
    (error "Cannot find the Ansys help executable \"%s\" on the system" exec)))

(defun apdl-lmutil-program ( exec)
  "Change the Ansys license management utility executable to EXEC.
And specify it in the variable `apdl-lmutil-program'.  The
function inserts the string `default-directory' in the prompt
when the variable `insert-default-directory' is not nil.  For
Lin64 it is the 'lmutil' executable
/ansys_inc/shared_files/licensing/linx64/lmutil.  For Windows the
anslic_admin utility: `C:\\Ansys Inc\\Shared
Files\\licensing\\win64\\anslic_admin.exe'"
  (interactive "FAnsys License Management Utility executable: ")
  (when (string= exec "")               ; use default
    (setq exec apdl-lmutil-program))
  (setq apdl-lmutil-program exec)
  (if (executable-find exec)
      (message "apdl-lmutil-program is set to \"%s\"." apdl-lmutil-program)
    (error "Cannot find Ansys LM Utility executable \"%s\" on this system" exec)))

(defun apdl-job ()
  "Change the Ansys job name.
And write it into the variable `apdl-job'."
  (interactive)
  (let ((job-name))
    (if apdl-job
        (setq job-name (read-string "job name: " apdl-job))
      (setq job-name (read-string "job name: ")))
    (if (string= job-name "")
        (error "Variable `job-name' must not be the empty string")
      (message (concat "Job name is set to \"" job-name "\".")))
    (setq apdl-job job-name)))

(defun apdl-no-of-processors ()
  "Change the No of processors to use for an Anys run.
The number of processors will be put into the integer variable
`apdl-no-of-processors'.  If this number is below 3 the variable
won't affect the run definition since the default No of
processors (if available) for a structural analysis in Ansys is
2."
  (interactive)
  (let ((no-string (number-to-string apdl-no-of-processors))
        no
        query
        s)
    (setq query (concat "Put in the No of processors to use [" no-string "]: ")
          s (read-string query nil nil no-string)
          no (string-to-number s))
    (if (integerp no)
        (setq apdl-no-of-processors no)
      (error "Specified number is not an integer"))
    (message "No of processors for the next run definition is %d" apdl-no-of-processors)))

(defun apdl-license-file (file)
  "Change the Ansys license file name or license server(s).
And specify the string FILE in the variable `apdl-license-file'
which can either be the license file name or license server(s)
specification.  The server specification must include the port
number (default port 1055), multiple server names are separated
by colons `:' on Linux, semi-colons `;' on Windows , for example
\"27005@rbgs421x:27005@rbgs422x\". The license specification is
stored in the environment variable APDL-LICENSE-FILE."
  (interactive
   (list (read-string
          (concat "License server or license file [" apdl-license-file "]: ")
          nil nil apdl-license-file)))
  (cond ((null file)
         (buffer-name))
        (t
         (setq apdl-license-file file)
         (setenv "AnsysLMD_LICENSE_FILE" file)
         (message (concat "Set apdl-license-file to \""
                          apdl-license-file "\".")))))

(defun apdl-ansysli-servers ( servers)
  "Change the Ansys interconnect servers to SERVERS.
And specify it in the variable `apdl-ansysli-servers'.  The
server specification must include the port number even when it is
2325, the default port number: port_number@server_name, multiple
server names are separated by a colon, for example
\"rbgs421x:rbgs422x:...\"."
  (interactive
   (list (read-string (concat "Interconnect license server(s) [" apdl-ansysli-servers "]: ")
                      nil nil apdl-ansysli-servers)))
  (cond ((null servers)
         (buffer-name))
        (t
         (setq apdl-ansysli-servers servers)
         (setenv "AnsysLI_SERVERS" servers)
         (message (concat "Set apdl-ansysli-servers to \""
                          apdl-ansysli-servers "\".")))))

;; FIXME:
;; (error "Please specify the license server information with
;;     the `apdl-license-file' function or either set
;;     AnsysLMD_LICENSE_FILE or LM-LICENSE-FILE environment
;;     variable")

;;;###autoload
(defun apdl-license ()
  "Change the Ansys license type.
And store it in the variable `apdl-license'."
  (interactive)
  (let ((lic (if (not (string= apdl-license ""))
                 apdl-license
               "struct")))
    (setq apdl-license
          (completing-read
	   (concat "License type [" lic "] (TAB for completion): ")
           apdl-license-categories
           nil nil nil nil lic))
    (message
     (concat "Ansys license type is now set to \"" apdl-license "\"."))))

(provide 'apdl-process)

;;; apdl-process.el ends here

;; Local Variables:
;; mode: outline-minor
;; indicate-empty-lines: t
;; show-trailing-whitespace: t
;; word-wrap: t
;; time-stamp-active: t
;; time-stamp-format: "%:y-%02m-%02d"
;; End:
