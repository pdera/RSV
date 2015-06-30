pro RSV
;resolve_all

COMMON Indices, HKLs, UB
common oadet,oad
COMMON tricl, WID_tricl, WID_BUTTON_tri_proceed, WID_BUTTON_tri_cancel
COMMON alternative, AWID_BUTTON_choose, AWID_BUTTON_Close, AWID_LIST_0
COMMON Hello_controls, WID_BUTTON_Hello_start
common Rot,Rotations
COMMON peaktable_objects, optable1,optable2, optable3, optable0
COMMON settings, ws
COMMON pat, path, cellnowpath

COMMON  ind_sol, solu, solus, soluno
common params, inpar
COMMON CLASS_crystal_reference, cryst
COMMON CLASS_crystal_objects, ocrystal
COMMON Tabsrefs, Tprojects, Tcrystal, Timage, Tpeaks, Tsettings
COMMON Diffs,diffsN, diffsV, diffsL, diffNO
COMMON Indices, HKLs, UB
Common local, peakno1
COMMON out_angs, omega, chi, energy
COMMON out_XYZ, XYZ
COMMON sol,solutions, solno

COMMON status, nopeaktable
nopeaktable=1

 rotations=[[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]]
 ws={window_settings, xy:intarr(2), scl : 0.0, control:0, rotation:0, colors:[0,0,0,0], circ:0}
 cellnowpath = 'C:/Users/harold/Projects/GSE_ADA_save/cell_now/'
 ; graphic window settings

 ws.xy      = [0,0]
 ws.scl     = 100.0
 ws.control = 1
 ws.colors  = [0, 255, 100, 255]
 ws.circ=2

 UB=fltarr(3,3)

 class_adetector
 oad=obj_new('adetector_class')

 soluno=0
 solu={solut, bvecs:intarr(3), UB:fltarr(3,3), UT:fltarr(3,3), lparams:fltarr(6), fraind: 0L, fom:0.0, sym:0}

 solus=replicate(solu, 200)

 dva={dv_par, eqth : 0.0,$
              coli : 0.0,$
              lmin : 0.0,$
              lmax : 0.0,$
              amin : 0.0,$
              amax : 0.0,$
              mfre : 0.0}

 ina={in_par, fract: 0.0,$
              thre : 0.0}

 inpar={index_param, dv:dva, in:ina}

 inpar.dv.eqth= 0.01
 inpar.dv.coli= 0.01
 inpar.dv.lmin= 3.0
 inpar.dv.lmax= 20.0
 inpar.dv.amin= 40.0
 inpar.dv.amax= 140.0
 inpar.dv.mfre= 2.0
 inpar.in.fract=0.5
 inpar.in.thre= 0.2


 CLASS_crystal

 Tcrystal=cryst

 ocrystal=Obj_new('crystal_class')

 CLASS_peaktable_definition
 optable1=obj_new('CLASS_peaktable')
 optable2=obj_new('CLASS_peaktable')
 optable3=obj_new('CLASS_peaktable')
 WIDGET_RSV

 initialize_indexing_params, inpar

 ;Widget_Hello
 ;hello_set_uval

end

;----------------------------------------------------
