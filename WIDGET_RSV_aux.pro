function get_bravis_type
COMMON BAse_bra, WID_BUTTON_P, WID_BUTTON_R, WID_BUTTON_I, WID_BUTTON_F, WID_BUTTON_A, WID_BUTTON_B, WID_BUTTON_C
res_p=widget_info(WID_BUTTON_P, /button_set)
res_r=widget_info(WID_BUTTON_R, /button_set)
res_i=widget_info(WID_BUTTON_I, /button_set)
res_f=widget_info(WID_BUTTON_F, /button_set)
res_a=widget_info(WID_BUTTON_A, /button_set)
res_b=widget_info(WID_BUTTON_B, /button_set)
res_c=widget_info(WID_BUTTON_C, /button_set)
if res_p eq 1 then re='P'
if res_r eq 1 then re='R'
if res_i eq 1 then re='I'
if res_f eq 1 then re='F'
if res_a eq 1 then re='A'
if res_b eq 1 then re='B'
if res_c eq 1 then re='C'
return, re
end


function read_dv_list_selection

@Main_Component_Common

COMMON DC_limits, WID_TEXT_lim11, WID_TEXT_lim12, WID_TEXT_lim21, WID_TEXT_lim22, WID_BUTTON_dc_thpo, WID_BUTTON_dc_thne, WID_TEXT_lim31, WID_TEXT_lim32, WID_TEXT_lim33
   res=WIDGET_INFO(WID_LIST_1, /LIST_SELECT)
   return,res
end

;---------------------------------------------------------------------

function check_rotation
 COMMON main_components
 r1=WIDGET_INFO(WID_BUTTON_13, /BUTTON_SET)
 r2=WIDGET_INFO(WID_BUTTON_14, /BUTTON_SET)
 r3=WIDGET_INFO(WID_BUTTON_15, /BUTTON_SET)
 if r1 eq 1 then ss=1
 if r2 eq 1 then ss=2
 if r3 eq 1 then ss=3
 return, ss
end
;--------------------------------------------------------------------------


pro save_UB, ub
COMMON pat, path
  fname=dialog_pickfile(FILTER='*.ub', /WRITE, PATH=path,  DEFAULT_EXTENSION='ub')
  free_lun, 2
  if fname ne '' then $
  begin
   openw, 2, fname
   printf, 2, ub[0,0], ub[1,0], ub[2,0], format='(F12.6,F12.6,F12.6)'
   printf, 2, ub[0,1], ub[1,1], ub[2,1], format='(F12.6,F12.6,F12.6)'
   printf, 2, ub[0,2], ub[1,2], ub[2,2], format='(F12.6,F12.6,F12.6)'
   close,2
  endif
  free_lun, 2
end


function read_DC_generation_settings


  COMMON DC_controls, $
  WID_BUTTON_dc_generate, $
  WID_BUTTON_dc_th, $
  WID_BUTTON_dc_en, $
  WID_TEXT_dc_th, $
  WID_TEXT_dc_en, $
  WID_BUTTON_RecalcUB,$
  WID_BUTTON_rec_xyz, $
  WID_BUTTON_RefBagainstd, $
  WID_BUTTON_42a, $ ;
  WID_BUTTON_30, $ ; apply transform button
  WID_BUTTON_30a, $ ; apply transform button
  WID_TEXT_t00, $
  WID_TEXT_t01, $
  WID_TEXT_t02, $
  WID_TEXT_t10, $
  WID_TEXT_t11, $
  WID_TEXT_t12, $
  WID_TEXT_t20, $
  WID_TEXT_t21, $
  WID_TEXT_t22, $
  WID_BUTTON_38, $ ; invert
  WID_BUTTON_s0, $
  WID_BUTTON_s11, $
  WID_BUTTON_s12, $
  WID_BUTTON_s13, $
  WID_BUTTON_s2, $
  WID_BUTTON_s3, $
  WID_BUTTON_s4, $
  WID_BUTTON_s5, $
  WID_LIST_2, $ ; manual indexing block
  WID_DROPLIST_2, $
  WID_DROPLIST_3, $
  WID_BUTTON_32, $
  WID_BUTTON_33, $
  WID_BUTTON_35, $
  WID_BUTTON_36, $
  WID_BUTTON_40, $
  WID_TEXT_al, $
  WID_TEXT_be, $
  WID_TEXT_ga, $
  WID_LIST_5, $
  WID_BUTTON_cpn, $
  WID_BUTTON_cdv, $
  WID_TEXT_bravis_angs, $
  WID_TEXT_bravis_lengs, $
  WID_BUTTON_unitmtx, $
  WID_BUTTON_refineom0, $
  WID_TEXT_om0, $
  WID_BUTTON_change_angles,$
  WID_BUTTON_41, $
  WID_BUTTON_refineiomega, $
  WID_BUTTON_import_p4p, $
  WID_TEXT_1, $
  WID_BUTTON_import, $
  WID_BUTTON_import_UNI, $
  WID_BUTTON_Import_ps, $
  WID_BUTTON_50, $
  WID_BUTTON_51, $
  WID_BUTTON_52, $
  WID_BUTTON_calibr_energy, $
  WID_BUTTON_Recalcangs

  COMMON DC_limits

  res=widget_info(WID_BUTTON_dc_th, /BUTTON_SET)
  if res eq 1 then ent=2 else ent=1

  WIDGET_CONTROL, WID_TEXT_dc_th, GET_VALUE=th
  WIDGET_CONTROL, WID_TEXT_dc_en, GET_VALUE=en



tths=widget_info(WID_BUTTON_dc_thpo, /BUTTON_SET)

; 1 if positive tth


WIDGET_CONTROL, WID_TEXT_lim31,GET_VALUE=hkl1
WIDGET_CONTROL, WID_TEXT_lim32,GET_VALUE=hkl2
WIDGET_CONTROL, WID_TEXT_lim33,GET_VALUE=hkl3
hkl1=long(hkl1)
hkl2=long(hkl2)
hkl3=long(hkl3)

  th=float(th)
  en=float(en)

WIDGET_CONTROL, WID_TEXT_lim11, GET_VALUE=tth1
WIDGET_CONTROL, WID_TEXT_lim12, GET_VALUE=tth2
WIDGET_CONTROL, WID_TEXT_lim21, GET_VALUE=om1
WIDGET_CONTROL, WID_TEXT_lim22, GET_VALUE=om2
tth1=float(tth1)
tth2=float(tth2)
om1=float(om1)
om2=float(om2)

  case ent of
  1: fi=[ent, en, tth1,tth2,om1,om2,tths,hkl1,hkl2,hkl3]
  2: fi=[ent, th, tth1,tth2,om1,om2,tths,hkl1,hkl2,hkl3]
  endcase
  return, fi

end


function UBM
;UB=[[0.33436401,    0.0212972,    0.0031367],$
;[-0.0669251,   -0.2032664,   -0.0093395],$
;[-0.0163090,   -0.0180880,    0.1037325]]

;UB=[[ 0.3130728,   -0.01982525,   -0.00891795],$
;    [-0.131073,    -0.20370365, 0.007717],$
;    [ 0.0365700,    0.01433500,  0.103524]]

 UB=[[   -0.017447, -0.0071514, 0.1411422],$
[-0.208782, 0.011477,-0.179236],$
[0.020313,  0.105757,    -0.037972]]

return, UB
end

;---------------------------------------------------------------
function read_ddd
@Main_Component_Common
  WIDGET_CONTROL, WID_TEXT_dvd1,GET_VALUE=a1
  WIDGET_CONTROL, WID_TEXT_dvd2,GET_VALUE=a2
  return, [float(a1), float(a2)]
end
;---------------------------------------------------------------
function read_dddI
@Main_Component_Common
  WIDGET_CONTROL, WID_TEXT_dvda2,GET_VALUE=a
  return, float(a)
end
;---------------------------------------------------------------
function read_dddF
@Main_Component_Common
  WIDGET_CONTROL, WID_TEXT_dvdb2,GET_VALUE=a
  return, float(a)
end

function which_x_and_y
@Main_Component_Common
  rx1=widget_info(WID_BUTTON_XF1, /BUTTON_SET)
  rx2=widget_info(WID_BUTTON_XF2, /BUTTON_SET)
  rx3=widget_info(WID_BUTTON_XF3, /BUTTON_SET)
  rx4=widget_info(WID_BUTTON_XF4, /BUTTON_SET)

  ry1=widget_info(WID_BUTTON_YF1, /BUTTON_SET)
  ry2=widget_info(WID_BUTTON_YF2, /BUTTON_SET)
  ry3=widget_info(WID_BUTTON_YF3, /BUTTON_SET)

  x=0
  y=0

  if rx1 eq 1 then x=1 else $
  if rx2 eq 1 then x=2 else $
  if rx3 eq 1 then x=3 else $
  if rx4 eq 1 then x=4

  if ry1 eq 1 then y=1 else $
  if ry2 eq 1 then y=2 else $
  if ry3 eq 1 then y=3 else $
  if ry4 eq 1 then y=4

  return, [x, y]
end


function read_pn_vs_dv
; OUTPUT:
;    1 - peak numberss
;    2 - difference vectors

COMMON DC_controls
  res_pn=WIDGET_INFO(WID_BUTTON_cpn, /BUTTON_SET)
  res_dv=WIDGET_INFO(WID_BUTTON_cdv, /BUTTON_SET)
  if res_pn eq 1 then re=1 else re=2
  return, re
end

;-------------------------------------

function read_transform_mtx
COMMON DC_controls
 t=fltarr(3,3)
 WIDGET_CONTROL, WID_TEXT_t00, gET_VALUE=t00
 WIDGET_CONTROL, WID_TEXT_t01, gET_VALUE=t01
 WIDGET_CONTROL, WID_TEXT_t02, gET_VALUE=t02
 WIDGET_CONTROL, WID_TEXT_t10, gET_VALUE=t10
 WIDGET_CONTROL, WID_TEXT_t11, gET_VALUE=t11
 WIDGET_CONTROL, WID_TEXT_t12, gET_VALUE=t12
 WIDGET_CONTROL, WID_TEXT_t20, gET_VALUE=t20
 WIDGET_CONTROL, WID_TEXT_t21, gET_VALUE=t21
 WIDGET_CONTROL, WID_TEXT_t22, gET_VALUE=t22
 t[0,0]=t00
 t[0,1]=t01
 t[0,2]=t02
 t[1,0]=t10
 t[1,1]=t11
 t[1,2]=t12
 t[2,0]=t20
 t[2,1]=t21
 t[2,2]=t22
 return, transpose(t)
end

;--------------------------------------------------------

pro  write_transform_mtx, t0
COMMON DC_controls
 t=transpose(t0)
 WIDGET_CONTROL, WID_TEXT_t00, sET_VALUE=string(t[0,0])
 WIDGET_CONTROL, WID_TEXT_t01, sET_VALUE=string(t[0,1])
 WIDGET_CONTROL, WID_TEXT_t02, sET_VALUE=string(t[0,2])
 WIDGET_CONTROL, WID_TEXT_t10, sET_VALUE=string(t[1,0])
 WIDGET_CONTROL, WID_TEXT_t11, sET_VALUE=string(t[1,1])
 WIDGET_CONTROL, WID_TEXT_t12, sET_VALUE=string(t[1,2])
 WIDGET_CONTROL, WID_TEXT_t20, sET_VALUE=string(t[2,0])
 WIDGET_CONTROL, WID_TEXT_t21, sET_VALUE=string(t[2,1])
 WIDGET_CONTROL, WID_TEXT_t22, sET_VALUE=string(t[2,2])
end


;--------------------------------------------------------


function make_peak_list, pn
if pn gt 0 then $
begin
 pl=strarr(pn)
 for i=0, pn-1 do pl[i]=strcompress(string(i), /remove_all)
 return, pl
end else return, ''
end

;--------------------------------------------------------


pro print_UB_and_lp, ub,lp,wid, optable1
  fom=optable1->indexing_FOM(UB, 0)
  omdif=optable1->aver_ang_error_mono(UB)
  list=['----------------------------------']
  list=[list,'UB matrix:']
  list=[list,string(ub[0,0])+'  '+string(ub[1,0])+'  '+string(ub[2,0])]
  list=[list,string(ub[0,1])+'  '+string(ub[1,1])+'  '+string(ub[2,1])]
  list=[list,string(ub[0,2])+'  '+string(ub[1,2])+'  '+string(ub[2,2])]
  list=[list,'----------------------------------']
  list=[list,'Unit cell parameters:']
  list=[list,string(lp[0])+'  '+string(lp[1])+'  '+string(lp[2])]
  list=[list,string(lp[3])+'  '+string(lp[4])+'  '+string(lp[5])]
  list=[list,'----------------------------------']
  sz=size(lp)
  sz=sz[1]
  if sz ge 12 then $
  begin
   list=[list,'Estimated standatd deviations:']
   tex=''
   tex1=''
   for i=6, 8 do tex=tex+string(lp[i])+'  '
   for i=9, 11 do tex1=tex1+string(lp[i])+'  '
   list=[list,tex,tex1]
  endif


   list=[list,'----------------------------------']
   a=''
   if n_elements(lp) eq 13 then a='('+string(deltaV_calculation(lp[0:5], lp[6:11]),format='(F6.2)')+')'
   list=[list,'Unit cell volume='+string(V_from_ub(ub),format='(F10.3)')+a]

   list=[list,'----------------------------------']
   list=[list,'Chi^2='+string(fom)]
   if n_elements(lp) eq 13 then $
   begin
    list=[list,'----------------------------------']
    list=[list,'Number of peaks used in refinement:']
    list=[list,'PN='+string(long(lp[12]))]
    end
   list=[list,'----------------------------------']
   list=[list,'Average omega error:']
   list=[list,'om_dif='+string(omdif[0])+'  '+string(omdif[1])]
   hklDAC=invert(UB) ## [1.,0.,0.]
   list=[list,'Vector alog DAC axis:'+string(hklDAC[0])+'  '+string(hklDAC[1])+'  '+string(hklDAC[2])]

  WIDGET_CONTROL, wid, SET_VALUE=list
end

;------------------------------------------------------------------

function read_bravis_settings
COMMON DC_controls

 WIDGET_CONTROL, WID_TEXT_bravis_angs,gET_VALUE=brav_a
 WIDGET_CONTROL, WID_TEXT_bravis_lengs, gET_VALUE=brav_l
 brav_a=float(brav_a)
 brav_l=float(brav_l)
 return, [brav_a, brav_l]
end





;-------------------------------------------
function active_PT
COMMON main_components

 re=WIDGET_info(WID_BUTTON_pt1,/button_set)
 if re eq 1 then return, 1 else return, 2
end

;-------------------------------------------
function read_scale
COMMON main_components

 WIDGET_CONTROL, WID_TEXT_scale,gET_VALUE=v
 return,reform(float(v))
end

;-------------------------------------------

function omega_zero_refine, optable1,om0

           om0=optable1->refine_om0(0.2,400, UB, om0, 0.0)
           om0=optable1->refine_om0(0.2,400, UB, om0, 0.0)
           om0=optable1->refine_om0(0.1,400, UB, om0, 0.0)
           om0=optable1->refine_om0(0.1,400, UB, om0, 0.0)
           om0=optable1->refine_om0(0.1,400, UB, om0, 0.0)
           om0=optable1->refine_om0(0.1,400, UB, om0, 0.0)
 return, om0
end

;--------------------------------------------

pro set_T_to_E
COMMON DC_controls

 WIDGET_CONTROL, WID_TEXT_t00, SET_VALUE='1'
 WIDGET_CONTROL, WID_TEXT_t01, SET_VALUE='0'
 WIDGET_CONTROL, WID_TEXT_t02, SET_VALUE='0'
 WIDGET_CONTROL, WID_TEXT_t10, SET_VALUE='0'
 WIDGET_CONTROL, WID_TEXT_t11, SET_VALUE='1'
 WIDGET_CONTROL, WID_TEXT_t12, SET_VALUE='0'
 WIDGET_CONTROL, WID_TEXT_t20, SET_VALUE='0'
 WIDGET_CONTROL, WID_TEXT_t21, SET_VALUE='0'
 WIDGET_CONTROL, WID_TEXT_t22, SET_VALUE='1'
end

;--------------------------------------------

function read_symmetry_constraint
COMMON DC_controls
  res0=WIDGET_INFO(WID_BUTTON_s0, /BUTTON_SET)
  res11=WIDGET_INFO(WID_BUTTON_s11, /BUTTON_SET)
  res12=WIDGET_INFO(WID_BUTTON_s12, /BUTTON_SET)
  res13=WIDGET_INFO(WID_BUTTON_s13, /BUTTON_SET)
  res2=WIDGET_INFO(WID_BUTTON_s2, /BUTTON_SET)
  res3=WIDGET_INFO(WID_BUTTON_s3, /BUTTON_SET)
  res4=WIDGET_INFO(WID_BUTTON_s4, /BUTTON_SET)
  res5=WIDGET_INFO(WID_BUTTON_s5, /BUTTON_SET)
  if res0 eq 1 then sym=0
  if res11 eq 1 then sym=11
  if res12 eq 1 then sym=12
  if res13 eq 1 then sym=13
  if res2 eq 1 then sym=2
  if res3 eq 1 then sym=3
  if res4 eq 1 then sym=4
  if res5 eq 1 then sym=5
  return, sym
end

;----------------------------------------------------

function read_vector_choice
  COMMON DC_controls
  resa=WIDGET_INFO(WID_BUTTON_35, /BUTTON_SET)
  resb=WIDGET_INFO(WID_BUTTON_36, /BUTTON_SET)
  resc=WIDGET_INFO(WID_BUTTON_40, /BUTTON_SET)
  if resa eq 1 then sym=1
  if resb eq 1 then sym=2
  if resc eq 1 then sym=3
  return, sym
end

;----------------------------------------------------

function read_two_peaks
  COMMON DC_controls
  res1=WIDGET_INFO(WID_DROPLIST_2, /DROPLIST_SELECT)
  res2=WIDGET_INFO(WID_DROPLIST_3, /DROPLIST_SELECT)
  res=[res1,res2]
  return, res
end

;----------------------------------------------------

pro WIDGET_RSV_event, ev


common Rot,Rotations
COMMON peaktable_objects, optable1,optable2, optable3, optable0
COMMON settings, ws
COMMON main_components
common params, inpar
COMMON Tabsrefs, Tprojects, Tcrystal, Timage, Tpeaks, Tsettings

COMMON CLASS_crystal_reference, cryst
 COMMON CLASS_crystal_objects, ocrystal
 COMMON Tabsrefs, Tprojects, Tcrystal, Timage, Tpeaks, Tsettings
COMMON DC_limits

COMMON Diffs,diffsN, diffsV, diffsL, diffNO
  COMMON Indices, HKLs, UB
  Common local, peakno1
  COMMON out_angs, omega, chi, energy
  COMMON out_XYZ, XYZ
  COMMON sol,solutions, solno
COMMON alternative, AWID_BUTTON_choose, AWID_BUTTON_Close, AWID_LIST_0
COMMON  ind_sol, solu, solus, soluno
COMMON ind_alt, altsols1, altsolno1
COMMON DC_controls
COMMON pat, path
COMMON status, nopeaktable
COMMON ind_tol, WID_TEXT_indthr, WID_TEXT_PD1

 WIDGET_CONTROL, ev.id,  GET_UVALUE=uval

 case uval of

'Calculate T':$
begin
 if n_elements(ub) gt 0 then $
 if total(UB) ne 0 then $
 begin
    UB1=open_UB()
    T=UB1#invert(UB)
    write_transform_mtx, T
 endif
end




'Select with lp':$
;'Select with UB' : $
   begin
        lp=[5.2,5.2,5.2,90.0,90.0,90.0]
        optable1->select_indexable_lp, lp, 0.01
        pl=optable1->peak_list(ub)
        WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
        draw_rp, optable1, WID_DRAW_0, ws
        WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
    end


'Select with UB' : $
    begin
        WIDGET_CONTROL, WID_TEXT_PD1,GET_VALUE=tol
        tol=float(tol)
        optable1->select_indexable, ub, tol
        pl=optable1->peak_list(ub)
        WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
        draw_rp, optable1, WID_DRAW_0, ws
        WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
    end

 ;'Ex redundant peaks':$
 ;   begin
 ;       optable1->filter_unique
 ;       pl=optable1->peak_list(ub)
 ;       WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
 ;       draw_rp, optable1, WID_DRAW_0, ws
 ;       WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
 ;   end
 'Inport ps':$
    begin
       fname_p4p=dialog_pickfile(FILTER='*.txt', /READ, PATH=path, GET_PATH=p)
       if p ne '' then path =p
       if fname_p4p ne '' then $
       begin
        nopeaktable=0
        optable1->import_ps,fname_p4p
        pl=optable1->peak_list(ub)
        WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
        draw_rp, optable1, WID_DRAW_0, ws
        WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
       endif

     end

 'Inport UNI':$
    begin
       fname_p4p=dialog_pickfile(FILTER='*.txt', /READ, PATH=path, GET_PATH=p)
       optable1->import_DetXYom, fname_p4p ;
       goto, ll
       if p ne '' then path =p
       if fname_p4p ne '' then $
       begin
        nopeaktable=0
        optable1->import_UNI,fname_p4p
        pl=optable1->peak_list(ub)
        WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
        draw_rp, optable1, WID_DRAW_0, ws
        WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
        ll:
       endif

     end

 'Inport ASCII':$
    begin
       fname_p4p=dialog_pickfile(FILTER='*.txt', /READ, PATH=path, GET_PATH=p)
       if p ne '' then path =p
       if fname_p4p ne '' then $
       begin
        nopeaktable=0
        optable1->import_ASCII,fname_p4p
        pl=optable1->peak_list(ub)
        WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
        draw_rp, optable1, WID_DRAW_0, ws
        WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
       endif

     end

 'Import p4p':$
     begin
       fname_p4p=dialog_pickfile(FILTER='*.p4p', /READ, PATH=path, GET_PATH=p)
       if p ne '' then path =p
       if fname_p4p ne '' then $
       begin
        nopeaktable=0
        optable1->Import_p4p,fname_p4p
        pl=optable1->peak_list(ub)
        WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
        draw_rp, optable1, WID_DRAW_0, ws
        WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
       endif

     end
 ;'Recalculate UB': $
 'Refine UB against XYZ':$
     begin
       if optable1->peakno() gt 0 then $
       begin
        iR=invert(rotations)
        aply_rotation, optable1, iR
        sym=read_symmetry_constraint()
        ds=optable1->BUILD_d_list()
        hkls=optable1->BUILD_hkls()
        xyzs=optable1->BUILD_xyzs()
        lp=lp_from_ub(ub)
        lp1=automatic_ub_refinement1(ub, XYZs, hkls, sym, lp)
        lp=lp_from_ub(ub)
        ;change ub matrix
        b1=b_from_lp(lp1)
        b0=b_from_lp(lp)
        u0=ub ## invert(b0)
        ub=u0 ## b1
        lp1[0:5]=lp_from_ub(ub)
        optable1->index, UB
        pl=optable1->peak_list(ub)
        WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
        aply_rotation, optable1, rotations
        print_UB_and_lp, ub,lp1,WID_LIST_5, optable1
       endif
     end

    'Reindex and calculate UB':$ ;------ this is the real routine that refines UB against xyz
    begin
               sym=read_symmetry_constraint()
               iR=invert(rotations)
               Ro=rotations
               aply_rotation, optable1, iR
               optable1->index, UB
               UB=optable1->recomp_UB()

               XYZs=optable1->build_XYZs()
               HKLs=optable1->build_HKLs()
               U=U_from_ub(ub)
               ub1=automatic_ub_refinement_against_xyz1(ub, xyzs, hkls, sym)

               lp=[ub1[0:5],ub1[9:14],ub1[18]]
               rotan=ub1[6:8]

               common Rota, Mtx

               GenerateR, 3, rotan[0]
               om=mtx
               GenerateR, 1, rotan[1]
               ch=mtx
               GenerateR, 3, rotan[2]
               ph=mtx
               RR=Om # Ch # Ph
               B=B_from_lp(lp)
               UB0=U ## B
               UB=RR ## UB0

               pl=optable1->peak_list(ub)
               WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
               print_UB_and_lp, ub,lp,WID_LIST_5, optable1
               aply_rotation, optable1, rotations

    end

 'Refine B against d':$
     begin
       iR=invert(rotations)
       aply_rotation, optable1, iR
       sym=read_symmetry_constraint()
       ds=optable1->BUILD_d_list()
       hkls=optable1->BUILD_hkls()
       lp=lp_from_ub(ub)
       lp1=automatic_lp_refinement3(lp, ds, hkls, sym)
       lp=lp_from_ub(ub)
       ;change ub matrix
       b1=b_from_lp(lp1)
       b0=b_from_lp(lp)
       u0=ub ## invert(b0)
       ub=u0 ## b1
       lp1[0:5]=lp_from_ub(ub)
       ;optable1->index, UB
       pl=optable1->peak_list(ub)
       WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
       aply_rotation, optable1, rotations
       print_UB_and_lp, ub,lp1,WID_LIST_5, optable1





       ddd=read_ddd()
       lp=lp_from_ub(ub)
       wset, WID_DRAW_F
       device, decomposed=1
       pt=optable1->get_object()
       N=pt.peakno
       ds=fltarr(N)
       dsc=fltarr(N)
       a=fltarr(N)
       ss=optable1->calculate_Ddd(lp)
       sel1=where(ss le ddd[0])
       sel2=where(ss ge ddd[1])

       if sel1[0] ne -1 and sel2[0] ne -1 then sel = [sel1, sel2] else $
       if sel1[0] ne -1 and sel2[0] eq -1 then sel = sel1 else $
       if sel1[0] eq -1 and sel2[0] ne -1 then sel = sel2 else $
       sel=-1

       if sel[0] ne -1 then optable1->select_peaks, sel
       ;pt=optable1->get_object()

       for i=0, n-1 do $
       begin
        a[i]=ang_between_vecs(pt.peaks[i].xyz, [1,0,0])
       endfor
       f2=sel
        plot, a, ss,  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='000000'xl
       if f2[0] ne -1 then $
       begin
         if f2[0] ne -1 then oplot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, color='0000FF'xl
       endif
       draw_rp, optable1, WID_DRAW_0, ws
     end
 'Refine omega':$
     begin
       optable1->refine_omega, ub
       lp=lp_from_ub(ub)
       optable1->index, UB
       pl=optable1->peak_list(ub)
       WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
       aply_rotation, optable1, rotations
       print_UB_and_lp, ub,lp,WID_LIST_5, optable1
     end
 'Save_UB':save_ub, ub

 'Open_UB':$
   begin
    ub=open_UB()
   ; ub1=open_UB()
    lp=lp_from_ub(UB)
    print_UB_and_lp, ub,lp,WID_LIST_5, optable1
    draw_cell,WID_DRAW_0,UB[0,0:2],UB[1,0:2],UB[2,0:2],ws
   end

'Recalculate xyz': $
      begin
          optable1->calculate_all_XYZ_from_EDD, oadetector,[0.0,0.0,0.0]
          pl=optable1->peak_list(ub)
          draw_rp, optable1, WID_DRAW_0, ws
          WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
          WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
      end
'Recalculate UB': $
    ; recomputes ub from miller indices in pt and xyz coordinates
        begin
         if optable1->peakno() gt 0 then $
         begin
           verify_rot, rotations
           iR=invert(rotations)
           aply_rotation, optable1, iR
           UB=optable1->recomp_UB()
           lp=lp_from_ub(UB)
           pl=optable1->peak_list(ub)
           WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl

           print_UB_and_lp, ub,lp,WID_LIST_5, optable1
           aply_rotation, optable1, rotations
         endif
        end

 'Generate DC':$
        begin
          ent=0
          the=0.0
          v=read_DC_generation_settings()
          ent=v[0]
          the=v[1]
          bra=get_bravis_type()
          print, bra, '-lattice type'
          case ent of
          2: optable1->generate_DC_peaklist_tth, UB, the, v[2:9],bra
          1: optable1->generate_DC_peaklist_en,  UB, the, v[2:9],bra
          endcase
          optable1->sort_angle, 3
          pl=optable1->peak_list(ub)
          draw_rp, optable1, WID_DRAW_0, ws
          WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
          WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)

        end
 'Ex improper profiles':$
        begin
           optable1->check_profiles

           draw_rp, optable1, WID_DRAW_0, ws
           pl=optable1->peak_list(ub)
           WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
           WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
        end
 'Draw': print, 'draw'
 'OpenPT': $
        begin
           fname=dialog_pickfile(FILTER='*.pks', /READ, PATH=path, GET_PATH=p)
           if p ne '' then path =p
           if fname ne '' then $
           begin
            nopeaktable=0
            optable1->read_object_from_file, fname, read_scale()
            if active_PT() eq 1 then optable1->copy,optable2 else optable1->copy,optable3
          ;  optable1->calculate_all_XYZ_from_EDD, oadetector,[0.0,0.0,0.0]
            ;WIDGET_CONTROL, W_RSV,  BASE_SET_TITLE='RSV ver. 2.5.1 : '+fname
            lp=Strlen(p)
            lf=Strlen(fname)
            WIDGET_CONTROL, wid_text_ptfilename, set_value=strmid(fname,lp,lf)
            WIDGET_CONTROL, WID_TEXT_workdir, set_value=p
            draw_rp, optable1, WID_DRAW_0, ws
            pl=make_peak_list(optable1->peakno())
            WIDGET_CONTROL, WID_DROPLIST_2, SET_VALUE=pl
            WIDGET_CONTROL, WID_DROPLIST_3, SET_VALUE=pl
            pl=optable1->peak_list(ub)
            WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
            WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
           endif
        end

 'Save as': $
        begin
           fname=dialog_pickfile(FILTER='*.pks', /write, PATH=path,  DEFAULT_EXTENSION='pks')
           if fname ne '' then $
           optable1->write_object_to_file, fname
           WIDGET_CONTROL, W_RSV,  BASE_SET_TITLE='RSV: '+fname
        end
 'Export Unitcell':$
 begin
       re=dialog_message('This feaure is temporarily disabled')
           ;indi, optable1
           ;fn=optable1->save_unitcell()
 end
 'Export p4p':$
 begin
           fn=optable1->save_p4p()
 end
 'Export ASCII':$
  begin
           if optable1->peakno() gt 0 then $
           fn=optable1->save_ascii1(path) else re=dialog_message('Peak table is empty')
  end
 'Close':  WIDGET_CONTROL, ev.top , /DESTROY
 'New' : $
        begin
           nopeaktable=1
           optable1->initialize
           draw_rp, optable1, WID_DRAW_0, ws
           WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)
           WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
           pl=''
           pl=optable1->peak_list(ub)
           WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl

        end
 'Slider': $
        begin
           case ws.control of
           1: $
              begin
                 WIDGET_CONTROL, WID_SLIDER_0, GET_VALUE=val
                 ws.scl=float(val)
              end
           2: $
              begin
                 WIDGET_CONTROL, WID_SLIDER_0, GET_VALUE=val
                 ws.xy[0]=float(val)
              end
           3: $
              begin
                 WIDGET_CONTROL, WID_SLIDER_0, GET_VALUE=val
                 ws.xy[1]=float(val)
              end
           endcase
              draw_rp, optable1, WID_DRAW_0, ws
        end
 'Zoom':$
        begin
           ws.control=1
           WIDGET_CONTROL, WID_SLIDER_0, SET_SLIDER_MIN=30
           WIDGET_CONTROL, WID_SLIDER_0, SET_SLIDER_MAX=300
           WIDGET_CONTROL, WID_SLIDER_0, SET_VALUE=fix(ws.scl)
        end
 'X':$
        begin
           ws.control=2
           WIDGET_CONTROL, WID_SLIDER_0, SET_SLIDER_MIN=-300
           WIDGET_CONTROL, WID_SLIDER_0, SET_SLIDER_MAX= 300
           WIDGET_CONTROL, WID_SLIDER_0, SET_VALUE=fix(ws.xy[0])
        end
 'Y':$
        begin
           ws.control=3
           WIDGET_CONTROL, WID_SLIDER_0, SET_SLIDER_MIN=-300
           WIDGET_CONTROL, WID_SLIDER_0, SET_SLIDER_MAX= 300
           WIDGET_CONTROL, WID_SLIDER_0, SET_VALUE=fix(ws.xy[1])
        end

 'RX':$
        begin
              ws.rotation=1
        end
 'RY':$
        begin
              ws.rotation=2
        end
 'RZ':$
        begin
              ws.rotation=3
        end
 '<':$
        begin
              ax=check_rotation()
              WIDGET_CONTROL, WID_TEXT_1,GET_VALUE=an
              an=float(an)
              rotate_peaks, optable1, ax, -an, 1
              draw_rp, optable1, WID_DRAW_0, ws
        end
 '>':$
        begin
              ax=check_rotation()
              WIDGET_CONTROL, WID_TEXT_1,GET_VALUE=an
              an=float(an)
              rotate_peaks, optable1, ax,  an, 1
              draw_rp, optable1, WID_DRAW_0, ws
        end

 'Zero rotation':$
        begin
              iR=invert(rotations)
              aply_rotation, optable1, iR
              draw_rp, optable1, WID_DRAW_0, ws
              pl=optable1->peak_list(ub)
              WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl

        end

 'Hand rotation': begin
           rotated=0
           readmouse=1
           wset, WID_DRAW_0
           x0=0
           y0=0
           x00=0
           y00=0
           doRotate=0
           WIDGET_CONTROL, WID_TEXT_Status, SET_VALUE='Pick reference'

           while (readmouse eq 1) do begin
             cursor, x, y, /nowait, /device
             whil:
             if not(x eq x0 and y eq y0) and not(x eq -1 and y eq -1) then $
             begin
               x0=x
               y0=y
             if DoRotate eq 1 then $
             begin
                stepy=y-y00
                stepx=x-x00
                if stepx lt 0 then $
                begin
                 rotate_peaks,optable1, 3, -2.0, 1
                 rotated=1
                endif
                if stepx gt 0 then $
                begin
                  rotate_peaks,optable1, 3,  2.0, 1
                  rotated=1
                endif
                if stepy lt 0 then $
                begin
                  rotate_peaks,optable1, 2, -2.0, 1
                  rotated=1
                endif
                if stepy gt 0 then $
                begin
                  rotate_peaks,optable1, 2,  2.0, 1
                  rotated=1
                endif
                draw_rp, optable1, WID_DRAW_0, ws
                Result1=WIDGET_INFO(WID_BUTTON_8, /BUTTON_SET)
                if Result1 eq 1 then $
                draw_cell,WID_DRAW_0,UB[0,0:2],UB[1,0:2],UB[2,0:2],ws
                x00=x
                y00=y
             endif
             endif

          if !MOUSE.button eq 1 then $ ;read reference point
             begin
               x00=x
               y00=y
               if DoRotate eq 0 then $
               begin
                  DoRotate=1
                  WIDGET_CONTROL, WID_TEXT_Status, SET_VALUE='Rotation'
;                  rotated=0
                  goto, whil
               endif else if rotated eq 1 then $
               begin
                  DoRotate=0
                  WIDGET_CONTROL, WID_TEXT_Status, SET_VALUE='Pick reference'
                  rotated = 0
                  goto, whil
               endif
             end
             if !MOUSE.button eq 4 then $
             begin
               readmouse=0
               WIDGET_CONTROL, WID_TEXT_Status, SET_VALUE=''
             endif
             endwhile

end
 'Select box' : BEGIN
           center=[241,241]

           SELECTED00=0
           SELECTED11=0
           box00x=0.0
           box00y=0.0
           box11x=0.0
           box11y=0.0
           readmouse=1
           wset, WID_DRAW_0
           while (readmouse eq 1) do begin
             cursor, x, y, /device, /NOWAIT
             if selected00 eq 1 then $
             begin
                if not(xo eq x and yo eq y) and not(x eq -1 and y eq -1) then $
                begin
                 xo=x
                 yo=y
                 draw_rp, optable1, WID_DRAW_0, ws
                 draw_box, box00x, box00y, x, y, ws, WID_DRAW_0
                endif
             endif
             if  !MOUSE.button eq 1 and not(x eq -1 and y eq -1) then begin
               if selected00 eq 0 then $ ; First left click
               begin
                 xo=x
                 yo=y
                box00x=(x-ws.xy[0]-center[0])/ws.scl
                box00y=(y-ws.xy[1]-center[1])/ws.scl
                selected00=1
               endif else if not(x eq -1 and y eq -1) then $ ; Second left click
               begin
                box11x=(x-ws.xy[0]-center[0])/ws.scl
                box11y=(y-ws.xy[1]-center[1])/ws.scl
                if box11x lt box00x then $ ; invert box x
                begin
                 aa=box11x
                 box11x=box00x
                 box00x=aa
                endif
                if box11y lt box00y then $ ; invert box y
                begin
                 aa=box11y
                 box11y=box00y
                 box00y=aa
                endif
                if box11x ne box00x and box00y ne box11y then $ ; Do the selection
                begin
                  selected11=1
                  readmouse=0

                   optable1->select_rpeaks, box00x,box00y, box11x, box11y
                  draw_rp, optable1, WID_DRAW_0, ws
                endif
               endif
             endif else begin
              if !MOUSE.button eq 4 then readmouse=0
             endelse
           endwhile
           WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)
           pl=optable1->peak_list(ub)
           WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl

      END

'Unselect box':BEGIN
           center=[241,241]

           SELECTED00=0
           SELECTED11=0
           box00x=0.0
           box00y=0.0
           box11x=0.0
           box11y=0.0
           readmouse=1
           wset, WID_DRAW_0
           while (readmouse eq 1) do begin
             cursor, x, y, /device, /NOWAIT
             if selected00 eq 1 then $
             begin
                if not(xo eq x and yo eq y) and not(x eq -1 and y eq -1) then $
                begin
                 xo=x
                 yo=y
                 draw_rp, optable1, WID_DRAW_0, ws
                 draw_box, box00x, box00y, x, y, ws, WID_DRAW_0
                endif
             endif
             if  !MOUSE.button eq 1 and not(x eq -1 and y eq -1) then begin
               if selected00 eq 0 then $ ; First left click
               begin
                 xo=x
                 yo=y
                box00x=(x-ws.xy[0]-center[0])/ws.scl
                box00y=(y-ws.xy[1]-center[1])/ws.scl
                selected00=1
               endif else if not(x eq -1 and y eq -1) then $ ; Second left click
               begin
                box11x=(x-ws.xy[0]-center[0])/ws.scl
                box11y=(y-ws.xy[1]-center[1])/ws.scl
                if box11x lt box00x then $ ; invert box x
                begin
                 aa=box11x
                 box11x=box00x
                 box00x=aa
                endif
                if box11y lt box00y then $ ; invert box y
                begin
                 aa=box11y
                 box11y=box00y
                 box00y=aa
                endif
                if box11x ne box00x and box00y ne box11y then $ ; Do the selection
                begin
                  selected11=1
                  readmouse=0
                  optable1->unselect_rpeaks, box00x,box00y, box11x, box11y
                  draw_rp, optable1, WID_DRAW_0, ws
                endif
               endif
             endif else begin
              if !MOUSE.button eq 4 then readmouse=0
             endelse
           endwhile
           WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)
           pl=optable1->peak_list(ub)
           WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl

      END


'Vector symbol size': $
               begin
                  WIDGET_CONTROL, WID_SLIDER_VecSS, GET_VALUE=val
                  ws.circ=fix(val)
                  draw_rp, optable1, WID_DRAW_0, ws

               end

'Delete selected'  :$
               begin
                   optable1->delete_selected
                   draw_rp, optable1, WID_DRAW_0, ws
                   WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)
                   WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
                   pl=optable1->peak_list(ub)
                   WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
                   pl=make_peak_list(optable1->peakno())
                   WIDGET_CONTROL, WID_DROPLIST_2, SET_VALUE=pl
                   WIDGET_CONTROL, WID_DROPLIST_3, SET_VALUE=pl

               end
'Unselect all'     :$
               begin
                   optable1->unselect_all
                   draw_rp, optable1, WID_DRAW_0, ws
                   WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)
                   pl=optable1->peak_list(ub)
                   WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
               end
'Invert selection' :$
               begin
                   optable1->invert_selection
                   draw_rp, optable1, WID_DRAW_0, ws
                   WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)
                   pl=optable1->peak_list(ub)
                   WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl

               end
 'Save graphics':$
               begin
                 wset, WID_DRAW_0
                 Result = TVRD(true=1)
                 fname=dialog_pickfile(FILTER='*.jpg', /WRITE,  DEFAULT_EXTENSION='jpg')
                 if STRMID(fname, 3,4, /REVERSE_OFFSET) ne '.jpg' then fname=fname+'.jpg'
                 WRITE_JPEG, fname, Result, true=1, quality=100

               end
 'Color Background': $
               begin
                  wset, WID_DRAW_0
                  temp=!P.COLOR
                  !P.COLOR=ws.colors[0]
                  ;xpalette, /block
                  xpcolor, group=W_RSV
                  ;!P.BACKGROUND=!P.COLOR
                  ws.colors[0]=!P.COLOR
                  ;!P.COLOR=temp
                  draw_rp, optable1, WID_DRAW_0, ws
               end
 'Color Peaks': $
               begin
                  wset, WID_DRAW_0
                  temp=!P.COLOR
                  !P.COLOR=ws.colors[1]
                  temp2=!P.BACKGROUND
                  xpcolor, group=W_RSV
                  ws.colors[1]=!P.COLOR
                  !P.BACKGROUND=temp2
                  draw_rp, optable1, WID_DRAW_0, ws
               end
 'Color Selected': $
               begin
                  wset, WID_DRAW_0
                  temp1=!P.COLOR
                  !P.COLOR=ws.colors[2]
                  temp2=!P.BACKGROUND
                  xpcolor, group=W_RSV
                  ws.colors[2]=!P.COLOR
                  !P.COLOR=temp1
                  !P.BACKGROUND=temp2
                  draw_rp, optable1, WID_DRAW_0, ws
               end
 'Color Box': $
               begin
                  wset, WID_DRAW_0
                  temp1=!P.COLOR
                  !P.COLOR=ws.colors[3]
                  temp2=!P.BACKGROUND
                  xpcolor, group=W_RSV
                  ws.colors[3]=!P.COLOR
                  !P.COLOR=temp1
                  !P.BACKGROUND=temp2
                  draw_rp, optable1, WID_DRAW_0, ws
               end
  'Index' : $
               begin
                 COMMON Diffs,diffsN, diffsV, diffsL, diffNO
                 COMMON setting_choice, sc

                 iR=invert(rotations)
                 Ro=rotations
                 aply_rotation, optable1, iR

                 get_indexing_params, inpar
                 calculate_differences, optable1,inpar
                 sort_differences_L
                 filter_differences_N, inpar.dv.mfre
                 print_difference_vectors, WID_LIST_1
                 IF diffNO lt 16 then $
                 begin
                 pick_rbasis_and_idex, inpar, optable1, solus, soluno,WID_LIST_1
                 ; make sure there are some solutions !!!!

                 WIDGET_CONTROL, WID_BUTTON_7, /SET_BUTTON
                 WIDGET_CONTROL, WID_LIST_1, SET_LIST_SELECT=0
                 result2=reind(solus[0].bvecs[0],solus[0].bvecs[1],solus[0].bvecs[2], inpar.in.thre, inpar.in.fract, fom,0, optable1)
                 ;al=Bravis(Tcrystal.UB_matrix, optable1)
                 ;st=unconstrainedlsq(optable1, Tcrystal)
                 al=''
                 Print, 'Crystal system: '+al;, st
                 vec1=diffsV[solus[0].bvecs[0]]
                 vec2=diffsV[solus[0].bvecs[1]]
                 vec3=diffsV[solus[0].bvecs[2]]
                 rotations=Ro
                 aply_rotation, optable1, Ro
                 draw_rp, optable1, WID_DRAW_0, ws
                 draw_cell,WID_DRAW_0,vec1.vec,vec2.vec,vec3.vec,ws
                 WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)
                 lp=lp_from_ub(UB)
                 print_UB_and_lp, ub,lp,WID_LIST_5, optable1
                 endif else re=dialog_message('It is not recommended to run indexing with more than 15 DV. Please, adjust parameters')
              end
  'Difference Vectors':$
               begin
                 get_indexing_params, inpar
                 calculate_differences, optable1,inpar
                 sort_differences_L
                 filter_differences_N, inpar.dv.mfre
                 print_difference_vectors,WID_LIST_1

               end
  'Reindex': $
        ; uses UB to calculate new hkl of all the peak in pt
               begin
                   iR=invert(rotations)
                   Ro=rotations
                   aply_rotation, optable1, iR

                   optable1->index, UB
                   pl=optable1->peak_list(ub)
                   WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
                   aply_rotation, optable1, rotations


               end

  'Draw DV' : $
               begin
                  print_difference_vectors, WID_LIST_1
                  WIDGET_CONTROL, WID_LIST_1, SET_LIST_SELECT=0
                  begin
                     vec=diffsV[0]
                     draw_rp, optable1, WID_DRAW_0, ws
                     draw_vector,WID_DRAW_0,vec.vec, ws
                  end
               end
  'Draw Solution' : $
               begin
                     Result1=WIDGET_INFO(WID_BUTTON_8, /BUTTON_SET)
                     WIDGET_CONTROL, WID_LIST_1, SET_LIST_SELECT=0
                     print_indexing_solutions, solus, soluno, WID_LIST_1
                     fom=0
                     result2=reind(solus[1].bvecs[0],solus[1].bvecs[1],solus[1].bvecs[2], inpar.in.thre, inpar.in.fract, fom,0, optable1)
                     ub=Tcrystal.UB_matrix
              ;       al=Bravis(ub, optable1)
                     Tcrystal.UB_matrix=ub
                     ;st=unconstrainedlsq(optable1, Tcrystal)
                     al=''
                     print, 'Crystal system: '+al;, st
                     draw_rp, optable1, WID_DRAW_0, ws
                     if Result1 eq 1 then $
                     begin
                         vec1=diffsV[solus[1].bvecs[0]]
                         vec2=diffsV[solus[1].bvecs[1]]
                         vec3=diffsV[solus[1].bvecs[2]]
                         draw_rp, optable1, WID_DRAW_0, ws
                         draw_cell,WID_DRAW_0,vec1.vec,vec2.vec,vec3.vec,ws

                     endif
               end

  'Bravis': $
               begin
                  ; I am changing this to work with the UB rather than with solution list 12/22/06
                  COMMON ind_alt, sols1, solno1
                  brav=read_bravis_settings()
                 ; WIDGET_CONTROL, WID_TEXT_AMAX, GET_VALUE=aa
                  fom=0
                  al=Bravis(ub, optable1, brav)
                  Tcrystal.UB_matrix=ub
                  ;st=unconstrainedlsq(optable1, Tcrystal)
                  WIDGET_Alternative_settings
                  alt_set_uval
                  i=1
                  COMMON ind_alt, altsols1, altsolno1
                  st=string(altsols1[i].lparams[0])+string(altsols1[i].lparams[1])+string(altsols1[i].lparams[2])+string(altsols1[i].lparams[3])+string(altsols1[i].lparams[4])+string(altsols1[i].lparams[5])
                  for i=2, solno1 do st=[st,string(altsols1[i].lparams[0])+string(altsols1[i].lparams[1])+string(altsols1[i].lparams[2])+string(altsols1[i].lparams[3])+string(altsols1[i].lparams[4])+string(altsols1[i].lparams[5])]
                  WIDGET_CONTROL, AWID_LIST_0, SET_VALUE=st


               end
  'Solution list':$
               begin
                ;  ub=fltarr(3,3)
                  Result0=WIDGET_INFO(WID_BUTTON_6, /BUTTON_SET)
                  Result1=WIDGET_INFO(WID_BUTTON_8, /BUTTON_SET)
                  Result=WIDGET_INFO(WID_LIST_1, /LIST_SELECT)
                  ;WIDGET_CONTROL, WID_TEXT_AMAX, GET_VALUE=aa
                  aa=1
                  if Result ne -1 and Result1 eq 1 and Result0 eq 1 then $
                  begin ;------------ draw difference vectors
                     vec=diffsV[Result[0]]
                     common Rot,Rotations
                     common Rota, Mtx


                     iR=invert(rotations)
                     aply_rotation, optable1, iR
                    ; draw_rp, optable1, WID_DRAW_0, ws
                    ; pl=optable1->peak_list(ub)


                     ;---------------------------
                     ;  calculate azimutal R1 totation to vertical up
                     ch=atan(vec.vec[1], vec.vec[2]) * !radeg

                     GenerateR, 1, -ch
                     v1=Mtx ## vec.vec

                     om=atan(v1[2], v1[0]) * !radeg

                     GenerateR, 2, -om
                     v2=Mtx ## v1
                     print, v2

                     print, 'Azimuthal rotation:', ch
                     rotate_peaks, optable1, 1, -ch, 0
                     rotate_peaks, optable1, 2, -om, 1
                     ;  calculate out of plane rotation R2 to vertical up


                     mark_proper_overlaps, optable1, WID_DRAW_0, ws, 1./vlength(vec.vec)

                     draw_rp, optable1, WID_DRAW_0, ws
                     draw_vector,WID_DRAW_0,float(aa)##vec.vec, ws

                  endif $
                  else if Result ne -1 and Result0 eq 0 then $
                  begin
                     fom=0
                     result2=reind(solus[result].bvecs[0],solus[result].bvecs[1],solus[result].bvecs[2], inpar.in.thre, inpar.in.fract, fom,0, optable1)
                     ub=Tcrystal.UB_matrix
                     Tcrystal.UB_matrix=ub
                     al=''
                     Print, 'Crystal system: '+al;, st
                     draw_rp, optable1, WID_DRAW_0, ws
                     if Result1 eq 1 then $
                     begin
                         vec1=diffsV[solus[result].bvecs[0]]
                         vec2=diffsV[solus[result].bvecs[1]]
                         vec3=diffsV[solus[result].bvecs[2]]
                         draw_rp, optable1, WID_DRAW_0, ws
                         draw_cell,WID_DRAW_0,vec1.vec,vec2.vec,vec3.vec,ws
                     endif
                     lp=lp_from_ub(UB)
                     print_UB_and_lp, ub,lp,WID_LIST_5, optable1
                  endif
                  WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)

               end


'Delete_peak'   : $
              begin
                 if nopeaktable eq 0 then $
                 begin

                  Result=WIDGET_INFO(WID_LIST_0, /LIST_SELECT)
                  si=size(Result)
                  if Result[0] ne -1 then $
                  begin
                    if si[0] gt 0 then for i=si[1]-1, 0, -1 do optable1->delete_peak, fix(Result[i]) else $
                    optable1->delete_peak, fix(Result[0])
                    draw_rp, optable1, WID_DRAW_0, ws
                    pl=optable1->peak_list(ub)
                    WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
                    WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
                    WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)
                  end
                  pl=make_peak_list(optable1->peakno())
                  WIDGET_CONTROL, WID_DROPLIST_2, SET_VALUE=pl
                  WIDGET_CONTROL, WID_DROPLIST_3, SET_VALUE=pl
                  end
              end

'Unselect_peak' : $
              begin
                 if nopeaktable eq 0 then $
                 begin
                  Result=WIDGET_INFO(WID_LIST_0, /LIST_SELECT)
                  si=size(Result)
                  if Result[0] ne -1 then $
                  begin
                    if si[0] gt 0 then for i=si[1]-1, 0, -1 do optable1->unselect_peak, fix(Result[i]) else $
                    optable1->unselect_peak, fix(Result[0])
                    draw_rp, optable1, WID_DRAW_0, ws
                    pl=optable1->peak_list(ub)
                    WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
                    WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
                    WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)
                  end
                  end
              end

  'Select_peak':$
              begin
                 if nopeaktable eq 0 then $
                 begin

                  Result=WIDGET_INFO(WID_LIST_0, /LIST_SELECT)
                  si=size(Result)
                  if Result[0] ne -1 then $
                  begin
                    if si[0] gt 0 then for i=si[1]-1, 0, -1 do optable1->select_peak, fix(Result[i]) else $
                    optable1->select_peak, fix(Result[0])
                    draw_rp, optable1, WID_DRAW_0, ws
                    pl=optable1->peak_list(ub)
                    WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
                    WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
                    WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)
                  end
                 end
              end
'Draw on': draw_rp, optable1, WID_DRAW_0, ws
 'Save results':$
              begin
                  WIDGET_CONTROL, WID_TEXT_3, GET_VALUE=st
                  fname=dialog_pickfile(FILTER='*.txt', /WRITE)
                  free_lun,2
                  openw, 2, fname
                  a=size(st)
                  for i=0, a[1]-1 do $
                  printf, 2, st[i]
                  close, 2
                  free_lun,2
              end

'Save_crystal':$
              begin
                  WIDGET_CONTROL, WID_TEXT_3, GET_VALUE=st
                  fname=dialog_pickfile(FILTER='*.txt', /WRITE)
                  free_lun,2
                  openw, 2, fname
                  a=size(st)
                  for i=0, a[1]-1 do $
                  printf, 2, st[i]
                  close, 2
                  free_lun,2
               end
'Open_crystal':$
              begin
                  fname=dialog_pickfile(FILTER='*.txt', /READ)
                  free_lun,2
                  st=''
                  openw, 2, fname
                  readf, 2, st
                  sti=[st]
                  while not eof(2) do $
                  begin
                    readf, 2, st
                    sti=[sti,st]
                  endwhile
                  close, 2
                  free_lun,2
                  WIDGET_CONTROL, WID_TEXT_3, SET_VALUE=sti

              end

'Apply UB transformation':$
    begin
       t=read_transform_mtx()
       ub=t # ub
       lp=lp_from_ub(UB)
       print_UB_and_lp, ub,lp,WID_LIST_5, optable1
       draw_cell,WID_DRAW_0,UB[0,0:2],UB[1,0:2],UB[2,0:2],ws
    end
 'Invert transformation mtx':$
    begin
       t=read_transform_mtx()
       t=invert(t)
       write_transform_mtx, t
    end

  'define vector':$
  begin

     choice=read_pn_vs_dv()
     vc=read_vector_choice()
     Res='Yes'
     if not (UB[vc-1, 0] eq 0 and UB[vc-1, 1] eq 0 and UB[vc-1, 2] eq 0) then $
     Res=Dialog_message('Another principal vector already defined. Overwrite?', /question)
     if Res eq 'Yes' then $
     begin

       case choice of
       1: $
          begin
            pt=optable1->get_object()
            ps=read_two_peaks()
            UB[vc-1, 0:2]=pt.peaks[ps[0]].xyz-pt.peaks[ps[1]].xyz
            print, ub
            print, 1/vlength(UB[vc-1, 0:2])
          end
       2: $ ; difference vectors
          begin
            sel=read_dv_list_selection()
            UB[vc-1, 0:2]=diffsV[sel].vec
            print, ub
            print, 1/vlength(UB[vc-1, 0:2])
          end
       else:
       endcase
       lp=lp_from_ub(UB)
       print_UB_and_lp, ub,lp,WID_LIST_5, optable1

     endif
  end
  'delete vector':
  'UB=B':$
  begin
    if n_elements(UB) gt 0 then $
      if finite(total(UB)) and (total(UB) ne 0) then $
      begin
     ub0=ub
     print, 'Determinant'
     print, determ(U_from_Ub(UB0))
     print, '---------------'
     ROT=invert(U_from_Ub(UB))
     UB=B_FROM_UB(UB)
     lp=lp_from_ub(UB)
     aply_rotation, optable1, ROT
     draw_rp, optable1, WID_DRAW_0, ws
     pl=optable1->peak_list(ub)
     WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
     print_UB_and_lp, ub,lp,WID_LIST_5, optable1
   end
  end
  'Change T to E': set_T_to_E
  'Refine om0':$
      begin
        WIDGET_CONTROL, WID_TEXT_om0,GET_VALUE=om0
        om0=float(om0)
        om0=omega_zero_refine(optable1, om0)
        WIDGET_CONTROL, WID_TEXT_om0,SET_VALUE=string(om0)
        pl=optable1->peak_list(ub)
        WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl

      end
   'Change angles':$
      begin
        WIDGET_CONTROL, WID_TEXT_om0,GET_VALUE=om0
        om0=float(om0)
        optable1->apply_om0,om0
        pl=optable1->peak_list(ub)
        WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
        WIDGET_CONTROL, WID_TEXT_om0,SET_VALUE='0.0'

      end

    'Refine ind. omega':$
         begin
            optable1->refine_ind_om,2.0,ub
         end

    'Sort': $
         begin
            optable1->sort
            pl=optable1->peak_list(ub)
            WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
         end



  'Activate PT1':$
  begin
            if active_PT() eq 1 then $
            begin
            r=rotations
            iR=invert(rotations)
            aply_rotation, optable1, iR
            optable1->copy,optable3 ; copy current to pt2
            optable2->copy,optable1  ; copy pt1 to current
            aply_rotation, optable1, r

            draw_rp, optable1, WID_DRAW_0, ws
            pl=make_peak_list(optable1->peakno())
            WIDGET_CONTROL, WID_DROPLIST_2, SET_VALUE=pl
            WIDGET_CONTROL, WID_DROPLIST_3, SET_VALUE=pl
            pl=optable1->peak_list(ub)
            WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
            WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
            endif
  end
  'Activate PT2':$
  begin
  	        if active_PT() eq 2 then $
  	        begin
            r=rotations
            iR=invert(rotations)
            aply_rotation, optable1, iR
	        optable1->copy,optable2
            optable3->copy,optable1
            aply_rotation, optable1, r
            draw_rp, optable1, WID_DRAW_0, ws
            pl=make_peak_list(optable1->peakno())
            WIDGET_CONTROL, WID_DROPLIST_2, SET_VALUE=pl
            WIDGET_CONTROL, WID_DROPLIST_3, SET_VALUE=pl
            pl=optable1->peak_list(ub)
            WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
            WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
            endif
  end
  'Show PT1':
  'Show PT2':
  'Move selected':$
  begin
    if active_PT() eq 1 then $
    begin
      r=rotations
      iR=invert(rotations)
      aply_rotation, optable1, iR
      optable1->move_selected, optable3
      aply_rotation, optable1, r
    endif else $
    begin
      r=rotations
      iR=invert(rotations)
      aply_rotation, optable1, iR
      optable1->move_selected, optable2
      aply_rotation, optable1, r
    endelse
      draw_rp, optable1, WID_DRAW_0, ws
      pl=make_peak_list(optable1->peakno())
      WIDGET_CONTROL, WID_DROPLIST_2, SET_VALUE=pl
      WIDGET_CONTROL, WID_DROPLIST_3, SET_VALUE=pl
      pl=optable1->peak_list(ub)
      WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
      WIDGET_CONTROL, WID_LABEL_Total,  SET_Value=STRCOMPRESS(string(optable1->peakno()), /REMOVE_ALL)
  end

'ReadUBfrom_p4p':$
 begin

    res=Dialog_pickfile(/read, filter='*.p4p')
	if res ne '' then $
	begin
 		FREE_LUN,2
 		OPENR, 2, res
 		str='       '
 		str1=''
 		str2=''
 		str3=''
 		; search for beginning of reflection block
 		while str ne 'CELLSD' and not eof(2) do $
   			readf, 2, str, format='(A6)'
 		if not eof(2) then $
 		begin
   			readf, 2, str1
			readf, 2, str2
			readf, 2, str3
     		CLOSE, 2
    		FREE_LUN,2
    		ub[0,0]=float(strmid(str1, 7,16))
    		ub[1,0]=float(strmid(str1, 23,16))
    		ub[2,0]=float(strmid(str1, 39,16))

    		ub[0,1]=float(strmid(str2, 7,16))
    		ub[1,1]=float(strmid(str2, 23,16))
    		ub[2,1]=float(strmid(str2, 39,16))

    		ub[0,2]=float(strmid(str3, 7,16))
    		ub[1,2]=float(strmid(str3, 23,16))
    		ub[2,2]=float(strmid(str3, 39,16))

            lp=lp_from_ub(UB)
            print_UB_and_lp, ub,lp,WID_LIST_5, optable1
            draw_cell,WID_DRAW_0,UB[0,0:2],UB[1,0:2],UB[2,0:2],ws

 		endif
 		endif
end

   ;-------------------


  'Select w/ I':$
  begin
       ddd=read_dddI()
       lp=lp_from_ub(ub)
       xy=which_x_and_y()
       wset, WID_DRAW_F
       device, decomposed=1
       pt=optable1->get_object()
       N=pt.peakno
       ds=fltarr(N)
       dsc=fltarr(N)
       a=fltarr(N)
       ss=pt.peaks[*].intAD[0]
       sel1=where(ss ge ddd[0])
       if sel1[0] ne -1 then optable1->select_peaks, sel1
       pt=optable1->get_object()
       for i=0, n-1 do $
       begin
        case xy[0] of
        1: a[i]=1.0/vlength(pt.peaks[i].xyz) 					; d
        2: a[i]=pt.peaks[i].gonio[3]         					; omega
        3: a[i]=pt.peaks[i].intad[0]         					; int
        else: a[i]=ang_between_vecs(pt.peaks[i].xyz, [1,0,0]) 	; dac axis
        endcase
       endfor
       f2=sel1;where(pt.peaks[*].selected[0] eq 1)
       if f2[0] ne -1 then $
       begin
         widget_control, WID_BUTTON_YF2, set_button=1
         plot, a, ss,  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='000000'xl
         if f2[0] ne -1 then oplot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, color='0000FF'xl
       endif ;else if f2[0] ne -1 then plot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='0000FF'xl
    end
   ;-------------------

  'Histogram filter':$
  begin
  kll:
       ds=optable1->calculate_Ds()
       h=histogram(ds, nbins=300, LOCATIONS=Loc)
       d_Range=max(ds)-min(ds)
       d_step=loc[1]-loc[0]
       wset, WID_DRAW_F
       plot, h
       w=where(h gt 50)
       if w[0] ne -1 then for i=0, n_elements(w)-1 do $
       begin
         w1=where(abs(ds-loc[w[i]]) lt d_step)
         if w1[0] ne -1 then optable1->select_peaks, w1
       endfor
       draw_rp, optable1, WID_DRAW_0, ws


  end
  'Select w/ FWHM':$
  begin
   ; goto, kll
       ddd=read_dddF()
       lp=lp_from_ub(ub)
       xy=which_x_and_y()

       wset, WID_DRAW_F
       device, decomposed=1
       pt=optable1->get_object()
       optable1->unselect_all
       N=pt.peakno
       ds=fltarr(N)
       dsc=fltarr(N)
       a=fltarr(N)
       ss=fltarr(N)
       ;ss=optable1->calculate_Ddd(lp)
       for i=0, n-1 do ss[i]= max(pt.peaks[i].energies[2:3])
       sel1=where(ss ge ddd[0])
       if sel1[0] ne -1 then optable1->select_peaks, sel1
       ;pt=optable1->get_object()
       for i=0, n-1 do $
       begin
       case xy[0] of
        1: a[i]=1.0/vlength(pt.peaks[i].xyz) 					; d
        2: a[i]=pt.peaks[i].gonio[3]         					; omega
        3: a[i]=pt.peaks[i].intad[0]         					; int
        else: a[i]=ang_between_vecs(pt.peaks[i].xyz, [1,0,0]) 	; dac axis
        endcase
       endfor
       f2=sel1
       if f2[0] ne -1 then $
       begin
         widget_control, WID_BUTTON_YF3, set_button=1
         plot, a, ss,  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='000000'xl
         if f2[0] ne -1 then oplot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, color='0000FF'xl
       endif
    end
   ;-------------------

    'Separate omega':$
    begin
      pt=optable1->get_object()
      b1=where(pt.peaks[*].gonio[3] lt -90.)
      b2=where(pt.peaks[*].gonio[3] gt  90.)

      if b1[0] ne -1 then $
      begin
       pt.peaks[b1].selected[0]=1
       pt.peaks[b1].gonio[3]=pt.peaks[b1].gonio[3]+360.
      end

      if b2[0] ne -1 then pt.peaks[b2].selected[0]=1

      optable1->set_object, pt

      draw_rp, optable1, WID_DRAW_0, ws
    end

    'Dd/d':$
    begin
     if total(ub) ne 0 and optable1->peakno() gt 0 then $
     begin
       ddd=read_ddd()
       lp=lp_from_ub(ub)
       xy=which_x_and_y()
       wset, WID_DRAW_F
       device, decomposed=1
       optable1->unselect_all
       pt=optable1->get_object()
       N=pt.peakno
       ds=fltarr(N)
       dsc=fltarr(N)
       a=fltarr(N)
       b=fltarr(N)
       ss=optable1->calculate_Ddd(lp)
       sel1=where(ss le ddd[0])
       sel2=where(ss ge ddd[1])
       if sel1[0] ne -1 and sel2[0] ne -1 then sel = [sel1, sel2] else $
       if sel1[0] ne -1 and sel2[0] eq -1 then sel = sel1 else $
       if sel1[0] eq -1 and sel2[0] ne -1 then sel = sel2 else $
       sel=-1


       if sel[0] ne -1 then optable1->select_peaks, sel

       pt=optable1->get_object()
       for i=0, n-1 do $
       begin
        case xy[0] of
        1: b[i]=1.0/vlength(pt.peaks[i].xyz) 					; d
        2: b[i]=pt.peaks[i].gonio[3]         					; omega
        3: b[i]=pt.peaks[i].intad[0]         					; int
        else: b[i]=ang_between_vecs(pt.peaks[i].xyz, [1,0,0]) 	; dac axis
        endcase
       endfor
       widget_control, WID_BUTTON_YF1, set_button=1
       f2=sel
       if f2[0] ne -1 then $
       begin
         plot, b, ss,  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='000000'xl
         if f2[0] ne -1 then oplot, b[f2], ss[f2],  thick=0, symsize=0.5, psym=6, color='0000FF'xl
       endif ;else if f2[0] ne -1 then plot, b[f2], ss[f2],  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='0000FF'xl
     endif
    end

;------------
    'YF1':$ ; dd/d
    begin
        xy=which_x_and_y()
    end

;------------

    'YF':$ ; intensity
    begin
        xy=which_x_and_y()
        ddd=read_ddd()
        lp=lp_from_ub(ub)
        wset, WID_DRAW_F
        device, decomposed=1
        pt=optable1->get_object()
        N=pt.peakno
        ds=fltarr(N)
        dsc=fltarr(N)
        a=fltarr(N)
        b=fltarr(N)
        ss=optable1->calculate_Ddd(lp)
        sel1=where(ss le ddd[0])
        sel2=where(ss ge ddd[1])
        if sel1[0] ne -1 then optable1->select_peaks, sel1
        if sel2[0] ne -1 then optable1->select_peaks, sel2
        pt=optable1->get_object()
        for i=0, n-1 do $
        begin
          case xy[0] of
           1: a[i]=1.0/vlength(pt.peaks[i].xyz)
           2: a[i]=pt.peaks[i].gonio[3]
           3: a[i]=pt.peaks[i].intad[0]
           4: a[i]=ang_between_vecs(pt.peaks[i].xyz, [1,0,0])
          endcase
          case xy[1] of
           1: b[i]=ss[i]
           2: b[i]=pt.peaks[i].intAD[0]
           3: b[i]=max([pt.peaks[i].energies[2],pt.peaks[i].energies[3]])
          endcase
        endfor
        f1=where(pt.peaks[*].selected[0] eq 0)
        f2=where(pt.peaks[*].selected[0] eq 1)
        if f1[0] ne -1 then $
        begin
          plot, a, b,  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='000000'xl
          if f2[0] ne -1 then oplot, a[f2], b[f2],  thick=0, symsize=0.5, psym=6, color='0000FF'xl
        endif else if f2[0] ne -1 then plot, a[f2], b[f2],  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='0000FF'xl
     end

;------------

    'YF3':$ ; Fwhm
    begin
    end

;------------
;------------

    'XF1':$ ; d-spacing
    begin
       ddd=read_ddd()
       lp=lp_from_ub(ub)
       wset, WID_DRAW_F
       device, decomposed=1
       pt=optable1->get_object()
       N=pt.peakno
       ds=fltarr(N)
       dsc=fltarr(N)
       a=fltarr(N)
       ss=optable1->calculate_Ddd(lp)
       sel1=where(ss le ddd[0])
       sel2=where(ss ge ddd[1])
       if sel1[0] ne -1 then optable1->select_peaks, sel1
       if sel2[0] ne -1 then optable1->select_peaks, sel2
       pt=optable1->get_object()
       for i=0, n-1 do $
       begin
        a[i]=1.0/vlength(pt.peaks[i].xyz)
       endfor
       f1=where(pt.peaks[*].selected[0] eq 0)
       f2=where(pt.peaks[*].selected[0] eq 1)
       if f1[0] ne -1 then $
       begin
         plot, a, ss,  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='000000'xl
         if f2[0] ne -1 then oplot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, color='0000FF'xl
       endif else if f2[0] ne -1 then plot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='0000FF'xl
    end

    'XF2':$ ; omega
    begin
         ddd=read_ddd()
       lp=lp_from_ub(ub)
       wset, WID_DRAW_F
       device, decomposed=1
       pt=optable1->get_object()
       N=pt.peakno
       ds=fltarr(N)
       dsc=fltarr(N)
       a=fltarr(N)
       ss=optable1->calculate_Ddd(lp)
       sel1=where(ss le ddd[0])
       sel2=where(ss ge ddd[1])
       if sel1[0] ne -1 then optable1->select_peaks, sel1
       if sel2[0] ne -1 then optable1->select_peaks, sel2
       pt=optable1->get_object()
       for i=0, n-1 do $
       begin
        a[i]=pt.peaks[i].gonio[3]
       endfor
       f1=where(pt.peaks[*].selected[0] eq 0)
       f2=where(pt.peaks[*].selected[0] eq 1)
       if f1[0] ne -1 then $
       begin
         plot, a, ss,  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='000000'xl
         if f2[0] ne -1 then oplot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, color='0000FF'xl
       endif else if f2[0] ne -1 then plot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='0000FF'xl
    end

    'XF3':$ ; Intensity
    begin

         ddd=read_ddd()
       lp=lp_from_ub(ub)
       wset, WID_DRAW_F
       device, decomposed=1
       pt=optable1->get_object()
       N=pt.peakno
       ds=fltarr(N)
       dsc=fltarr(N)
       a=fltarr(N)
       ss=optable1->calculate_Ddd(lp)
       sel1=where(ss le ddd[0])
       sel2=where(ss ge ddd[1])
       if sel1[0] ne -1 then optable1->select_peaks, sel1
       if sel2[0] ne -1 then optable1->select_peaks, sel2
       pt=optable1->get_object()
       for i=0, n-1 do $
       begin
        a[i]=pt.peaks[i].intad[0]
       endfor
       f1=where(pt.peaks[*].selected[0] eq 0)
       f2=where(pt.peaks[*].selected[0] eq 1)
       if f1[0] ne -1 then $
       begin
         plot, a, ss,  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='000000'xl
         if f2[0] ne -1 then oplot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, color='0000FF'xl
       endif else if f2[0] ne -1 then plot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='0000FF'xl
    end


    'XF4':$ ; DAC axis
    begin

         ddd=read_ddd()
       lp=lp_from_ub(ub)
       wset, WID_DRAW_F
       device, decomposed=1
       pt=optable1->get_object()
       N=pt.peakno
       ds=fltarr(N)
       dsc=fltarr(N)
       a=fltarr(N)
       ss=optable1->calculate_Ddd(lp)
       sel1=where(ss le ddd[0])
       sel2=where(ss ge ddd[1])
       if sel1[0] ne -1 then optable1->select_peaks, sel1
       if sel2[0] ne -1 then optable1->select_peaks, sel2
       pt=optable1->get_object()
       for i=0, n-1 do $
       begin
          a[i]=ang_between_vecs(pt.peaks[i].xyz, [1,0,0])
       endfor
       f1=where(pt.peaks[*].selected[0] eq 0)
       f2=where(pt.peaks[*].selected[0] eq 1)
       if f1[0] ne -1 then $
       begin
         plot, a, ss,  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='000000'xl
         if f2[0] ne -1 then oplot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, color='0000FF'xl
       endif else if f2[0] ne -1 then plot, a[f2], ss[f2],  thick=0, symsize=0.5, psym=6, background='FFFFFF'xl, color='0000FF'xl
    end

    'CellNow':$
    begin
      fn=optable1->save_p4p('xxx.p4p')
      Re=dialog_message('Current peak table has been saved in file xxx.p4p',/information)
      re=file_info('cell_now.exe')
      if re.exists eq 1 then $
      begin
          ; export current peak table as p4p file
          ; advise about the name and location of this file
          cellnow='cell_now.exe'
          ; offer to read ub matrix from result p4p file
          ; louch a browser to pick up that result file
          spawn, cellnow, /noshell
      ;endif else re=dialog_message('You need to copy cell_now.exe to RSV program directory')
      endif else begin
      	Re = dialog_message('Cell_now not found, Please select the cell_now executable',/information)
      	cellNowLoc = dialog_pickfile (/READ,TITLE='Please select the cell_now executable')
      endelse
    end

    'Launch XPREP':$
    begin
      re=file_info('bigxprep.exe')
      renp=file_info('notepad.exe')
      if re.exists eq 1 then $
      begin
      if  optable1->peakno() gt 0 then $
      begin
      fn=optable1->save_ascii1(path, 'rsv.hkl')
      lp=lp_from_ub(ub)
      free_lun, 2
      openw, 2, 'rsv.p4p'
      printf, 2, 'CELL  '+string(lp[0], format='(F11.4)')+string(lp[1], format='(F11.4)'), string(lp[2], format='(F11.4)')+string(lp[3], format='(F10.4)')+string(lp[4], format='(F10.4)')+string(lp[5], format='(F10.4)'),string(200.0, format='(F10.4)')
      printf, 2, 'CELLSD    0.0001    0.0001    0.0001    0.0001    0.0100    0.0100     0.1'
      close, 2
      xprepfile='bigxprep.exe rsv.hkl'
      free_lun,2
      spawn, xprepfile, /noshell, unit=un
      re=dialog_message('Open prp file in notepad?', /question)
      if re eq 'Yes' then spawn, 'notepad.exe rsv.prp'
      endif
      end else re=dialog_message('You need to copy bigxprep.exe to RSV program directory')
    end


    'Callibrate energy':$
         begin
           lp=[4.7608,4.7608,12.9957, 90.0,90.0,120.0]
           cal=fltarr(1000,2)
           pt=optable1->get_object()
           for i=0, optable1->peakno()-1 do $
           begin
             cal[i,0]=pt.peaks[i].energies[0]
             hkl=pt.peaks[i].hkl
             ttheta=pt.peaks[i].gonioss[1]
             d=d_from_lp_and_hkl(lp, hkl)
             cal[i,1]=en_from_tth_and_d(ttheta, d)
           endfor
           plot, cal[0:optable1->peakno()-1,0],cal[0:optable1->peakno()-1,0]
           Result = POLY_FIT( cal[0:optable1->peakno()-1,0], cal[0:optable1->peakno()-1,1], 1, $
           CHISQ=chisq, SIGMA=sig, YFIT=yfit)
           print, result
           print, sig
           for i=0, optable1->peakno()-1 do $
           begin
             en=pt.peaks[i].energies[0]
             ttheta=pt.peaks[i].gonioss[1]
             xyz=pt.peaks[i].xyz
             en1=-en*result[1]-result[0]
             pt.peaks[i].energies[0]=en1
             d=d_from_tth_and_en(ttheta, en1)
             pt.peaks[i].xyz=xyz/vlength(xyz)/d
           endfor
           optable1->set_object, pt
         end
      'Recalc. angs': $
      begin
               for i=0, optable1->peakno()-1 do $
               begin
                 pe=optable1->get_element(i)
                 pe.XYZ = UB ## pe.HKL
                 d=1.0/vlength(pe.XYZ)
                 tth=tth_from_en_and_d(pe.energies[0], d)
                 pe.GONIOSS=calculate_EDDangles_from_xyz(pe.XYZ, [0.0,0.0], tth)
                 optable1->set_element, i, pe
               endfor
               pl=optable1->peak_list(ub)
               WIDGET_CONTROL, WID_LIST_0,  SET_Value=pl
      end


    else:
 endcase


end

;-----------------------------------------

pro WIDGET_Alternative_settings_event, ev

COMMON alternative
COMMON ind_alt, sols1, solno1
COMMON main_components
COMMON Tabsrefs
COMMON sol
common params
COMMON peaktable_objects
COMMON settings
COMMON CLASS_crystal_objects
COMMON  ind_sol

COMMON ind_alt, altsols1, altsolno1
COMMON Indices, HKLs, UB


 WIDGET_CONTROL, ev.id,  GET_UVALUE=uval
 case uval of
 'Alt choose' : $
              begin
                  Result=WIDGET_INFO(WID_LIST_1, /LIST_SELECT)
                  Result1=WIDGET_INFO(AWID_LIST_0, /LIST_SELECT)
                  WIDGET_CONTROL, ev.top , /DESTROY

                  Tcrystal.UB_matrix=altsols1[result1+1].UT # altsols1[result1+1].UB
                  OCRYSTAL->set_object,Tcrystal

                  vec1=Tcrystal.UB_matrix[0,0:2]
                  vec2=Tcrystal.UB_matrix[1,0:2]
                  vec3=Tcrystal.UB_matrix[2,0:2]
                  fom=0
                  result2=reind(vec1,vec2,vec3, inpar.in.thre, inpar.in.fract, fom,2, optable1)
                  ;an=optable1->reindex(ocrystal, 0.1)
                  Tcrystal=ocrystal->get_object()
                  ;st=unconstrainedlsq(optable1, Tcrystal)
                  al=crystal_system(Tcrystal.cell_parameters, 0.3, 0.1)
                  Print, 'Crystal system: '+al;, st
                  draw_rp, optable1, WID_DRAW_0, ws
                  WIDGET_CONTROL, WID_LABEL_Selected,  SET_Value=STRCOMPRESS(string(optable1->selectedno()), /REMOVE_ALL)
                  UB=Tcrystal.UB_matrix
              end

 'Alt close'  : WIDGET_CONTROL, ev.top , /DESTROY

 'Alt list'   : $
              begin
              end
 else:
 endcase

end

;------------------


pro initialize_indexing_params, inpar

COMMON main_components

 WIDGET_CONTROL, WID_TEXT_dveqthresh,  SET_VALUE=string(inpar.dv.eqth)
 WIDGET_CONTROL, WID_TEXT_dvlengthmin, SET_VALUE=string(inpar.dv.lmin)
 WIDGET_CONTROL, WID_TEXT_dvlengthmax, SET_VALUE=string(inpar.dv.lmax)
 WIDGET_CONTROL, WID_TEXT_dvangmin,    SET_VALUE=string(inpar.dv.amin)
 WIDGET_CONTROL, WID_TEXT_dvangmax,    SET_VALUE=string(inpar.dv.amax)
 WIDGET_CONTROL, WID_TEXT_dvminfreq,   SET_VALUE=string(inpar.dv.mfre)
 WIDGET_CONTROL, WID_TEXT_fractionreq, SET_VALUE=string(inpar.in.fract)
 WIDGET_CONTROL, WID_TEXT_indthresh,   SET_VALUE=string(inpar.in.thre)


end

pro alt_set_uval

COMMON alternative

WIDGET_CONTROL,AWID_BUTTON_choose, SET_UVALUE='Alt choose'
 WIDGET_CONTROL,AWID_BUTTON_Close, SET_UVALUE='Alt close'
 WIDGET_CONTROL,AWID_LIST_0, SET_UVALUE='Alt list'

end


pro hello_set_uval
COMMON Hello_controls, WID_BUTTON_Hello_start
WIDGET_CONTROL,WID_BUTTON_Hello_start, SET_UVALUE='Hello start'
end



pro Widget_Hello_event, ev

 WIDGET_CONTROL, ev.id,  GET_UVALUE=uval
 case uval of
'Hello start': WIDGET_CONTROL, ev.top , /DESTROY
 else:
endcase
end

pro tricl_set_uval

 COMMON tricl, WID_tricl, WID_BUTTON_tri_proceed, WID_BUTTON_tri_cancel
 WIDGET_CONTROL,WID_BUTTON_tri_proceed, SET_UVALUE='tri_proceed'
 WIDGET_CONTROL,WID_BUTTON_tri_cancel, SET_UVALUE='tri_cancel'

end


pro WIDGET_triclinic_warning_event, ev

COMMON request, proceed
 proceed=0
 WIDGET_CONTROL, ev.id,  GET_UVALUE=uval
 case uval of

 'tri_proceed': $
    begin
        proceed=1
        WIDGET_CONTROL, ev.top , /DESTROY
    end
 'tri_cancel': $
    begin
        proceed=0
        WIDGET_CONTROL, ev.top , /DESTROY
    end
 else:
 endcase

 end

;--------------------------------------------------------------------

pro get_indexing_params, inpar

COMMON main_components

 WIDGET_CONTROL, WID_TEXT_dveqthresh,  GET_VALUE=st
 inpar.dv.eqth=float(st)
 WIDGET_CONTROL, WID_TEXT_dvlengthmin, GET_VALUE=st
 inpar.dv.lmin=float(st)
 WIDGET_CONTROL, WID_TEXT_dvlengthmax, GET_VALUE=st
 inpar.dv.lmax=float(st)
 WIDGET_CONTROL, WID_TEXT_dvangmin,    GET_VALUE=st
 inpar.dv.amin=float(st)
 WIDGET_CONTROL, WID_TEXT_dvangmax,    GET_VALUE=st
 inpar.dv.amax=float(st)
 WIDGET_CONTROL, WID_TEXT_dvminfreq,   GET_VALUE=st
 inpar.dv.mfre=float(st)
 WIDGET_CONTROL, WID_TEXT_fractionreq, GET_VALUE=st
 inpar.in.fract=float(st)
 WIDGET_CONTROL, WID_TEXT_indthresh,   GET_VALUE=st
 inpar.in.thre=float(st)
 WIDGET_CONTROL, WID_TEXT_indthresh,   GET_VALUE=st
 inpar.in.thre=float(st)

end

;--------------------------------------------------------------------

pro WIDGET_RSV_aux

COMMON ind_tol, WID_TEXT_indthr, WID_TEXT_PD1
COMMON BAse_bra, WID_BUTTON_P, WID_BUTTON_R, WID_BUTTON_I, WID_BUTTON_F, WID_BUTTON_A, WID_BUTTON_B, WID_BUTTON_C
COMMON main_components
COMMON alternative
COMMON DC_controls
COMMON DC_limits


;WIDGET_CONTROL, WID_TEXT_lim11, SET_UVALUE=''
;WIDGET_CONTROL, WID_TEXT_lim12, SET_UVALUE=''
;WIDGET_CONTROL, WID_TEXT_lim21, SET_UVALUE=''
;WIDGET_CONTROL, WID_TEXT_lim22, SET_UVALUE=''


;WIDGET_CONTROL, WID_BUTTON_dc_thpo,SET_UVALUE=''
;WIDGET_CONTROL, WID_BUTTON_dc_thpo,SET_BUTTON=1
;WIDGET_CONTROL, WID_BUTTON_dc_thne, SET_UVALUE=''
;WIDGET_CONTROL, WID_TEXT_lim31, SET_UVALUE=''

;WIDGET_CONTROL, WID_TEXT_ptfilename,  SET_UVALUE=''
;WIDGET_CONTROL, WID_TEXT_lim32, SET_UVALUE=''
;WIDGET_CONTROL, WID_TEXT_lim33, SET_UVALUE=''
;WIDGET_CONTROL, WID_TEXT_lim31, SET_VALUE='13'
;WIDGET_CONTROL, WID_TEXT_lim32, SET_VALUE='13'
;WIDGET_CONTROL, WID_TEXT_lim33, SET_VALUE='13'
;WIDGET_CONTROL, WID_TEXT_lim31, /EDITABLE
;WIDGET_CONTROL, WID_TEXT_lim32, /EDITABLE
;WIDGET_CONTROL, WID_TEXT_lim33, /EDITABLE
;WIDGET_CONTROL, WID_BUTTON_calibr_energy, SET_UVALUE='Callibrate energy'

;WIDGET_CONTROL, WID_TEXT_lim11, SET_VALUE='-20.0'
;WIDGET_CONTROL, WID_TEXT_lim12, SET_VALUE='-5.0'
;WIDGET_CONTROL, WID_TEXT_lim21, SET_VALUE='-13.0'
;WIDGET_CONTROL, WID_TEXT_lim22, SET_VALUE='13.0'

  WIDGET_CONTROL, WID_BUTTON_import_p4p, SET_UVALUE='Import p4p'

  WIDGET_CONTROL, WID_BUTTON_RecalcUB, SET_UVALUE='Recalculate UB'
  WIDGET_CONTROL, WID_BUTTON_RefBagainstd, SET_UVALUE='Refine B against d'
  WIDGET_CONTROL, WID_BUTTON_42a, SET_UVALUE='Refine omega'

  ;WIDGET_CONTROL, WID_BUTTON_P, SET_UVALUE='P'
  ;WIDGET_CONTROL, WID_BUTTON_P,       SET_BUTTON=1
  ;WIDGET_CONTROL, WID_BUTTON_R, SET_UVALUE='R'
  ;WIDGET_CONTROL, WID_BUTTON_I, SET_UVALUE='I'
  ;WIDGET_CONTROL, WID_BUTTON_F, SET_UVALUE='F'
  ;WIDGET_CONTROL, WID_BUTTON_A, SET_UVALUE='A'
  ;WIDGET_CONTROL, WID_BUTTON_B, SET_UVALUE='B'
  ;WIDGET_CONTROL, WID_BUTTON_C, SET_UVALUE='C'

  ;WIDGET_CONTROL, WID_BUTTON_dc_generate, SET_UVALUE='Generate DC'
  ;WIDGET_CONTROL, WID_BUTTON_dc_th,       SET_UVALUE=''
  ;WIDGET_CONTROL, WID_BUTTON_dc_th,       SET_BUTTON=1
  ;WIDGET_CONTROL, WID_BUTTON_dc_en,       SET_UVALUE=''
  ;WIDGET_CONTROL, WID_TEXT_dc_th,         SET_UVALUE=''
  ;WIDGET_CONTROL, WID_TEXT_dc_en,         SET_UVALUE=''
  ;WIDGET_CONTROL, WID_TEXT_dc_th,         EDITABLE=1
  ;WIDGET_CONTROL, WID_TEXT_dc_th,         SET_VALUE='-10.0'
  ;WIDGET_CONTROL, WID_TEXT_dc_en,         EDITABLE=1
  ;WIDGET_CONTROL, WID_TEXT_dc_en,         SET_VALUE='50.0'

  WIDGET_CONTROL, WID_BUTTON_31, SET_UVALUE='Export ASCII'
;  WIDGET_CONTROL, WID_BUTTON_rec_xyz, SET_UVALUE='Recalculate xyz'

  WIDGET_CONTROL, WID_DRAW_0,         SET_UVALUE='Draw'
  WIDGET_CONTROL, WID_DRAW_0,         GET_VALUE=WID_DRAW_0

  WIDGET_CONTROL, WID_DRAW_F,         SET_UVALUE='DrawF'
  WIDGET_CONTROL, WID_DRAW_F,         GET_VALUE=WID_DRAW_F


  WIDGET_CONTROL, WID_BUTTON_OPENPT,  SET_UVALUE='OpenPT'
  WIDGET_CONTROL, WID_BUTTON_4,       SET_UVALUE='Save as'
  WIDGET_CONTROL, WID_BUTTON_CLOSE,   SET_UVALUE='Close'
  WIDGET_CONTROL, WID_BUTTON_NEW,     SET_UVALUE='New'
  WIDGET_CONTROL, WID_SLIDER_0,       SET_UVALUE='Slider'
  WIDGET_CONTROL, WID_TAB_0,          SET_UVALUE=''
  WIDGET_CONTROL, WID_TAB_1,          SET_UVALUE=''

  WIDGET_CONTROL, WID_BUTTON_0,          SET_UVALUE='Zoom'
  WIDGET_CONTROL, WID_BUTTON_0,          SET_BUTTON=1
  WIDGET_CONTROL, WID_BUTTON_1,          SET_UVALUE='X'
  WIDGET_CONTROL, WID_BUTTON_3,          SET_UVALUE='Y'

  WIDGET_CONTROL, WID_BUTTON_13,          SET_UVALUE='RX'
  WIDGET_CONTROL, WID_BUTTON_13,          SET_BUTTON=1
  WIDGET_CONTROL, WID_BUTTON_14,          SET_UVALUE='RY'
  WIDGET_CONTROL, WID_BUTTON_15,          SET_UVALUE='RZ'

  WIDGET_CONTROL, WID_BUTTON_16,          SET_UVALUE='<'
  WIDGET_CONTROL, WID_BUTTON_17,          SET_UVALUE='>'

  WIDGET_CONTROL, WID_BUTTON_18,          SET_UVALUE='Hand rotation'
  WIDGET_CONTROL, WID_BUTTON_19,          SET_UVALUE='Select box'
  WIDGET_CONTROL, WID_BUTTON_23,          SET_UVALUE='Unselect box'

  WIDGET_CONTROL, WID_BUTTON_20,          SET_UVALUE='Delete selected'
  WIDGET_CONTROL, WID_BUTTON_21,          SET_UVALUE='Unselect all'
  WIDGET_CONTROL, WID_BUTTON_22,          SET_UVALUE='Invert selection'
  WIDGET_CONTROL, WID_BUTTON_50,          SET_UVALUE='Select with UB'
  WIDGET_CONTROL, WID_BUTTON_31a,          SET_UVALUE='Export Unitcell'
  WIDGET_CONTROL, WID_BUTTON_31b,          SET_UVALUE='Export p4p'

  WIDGET_CONTROL, WID_LIST_0,          SET_UVALUE='Peak List'

  WIDGET_CONTROL, WID_BUTTON_5,          SET_UVALUE='Save graphics'

  WIDGET_CONTROL, WID_BUTTON_28,           SET_UVALUE='Color Background'
  WIDGET_CONTROL, WID_BUTTON_27,          SET_UVALUE='Color Peaks'
  WIDGET_CONTROL, WID_BUTTON_9,          SET_UVALUE='Color Selected'
  WIDGET_CONTROL, WID_BUTTON_29,          SET_UVALUE='Color Box'

  WIDGET_CONTROL, WID_TEXT_bravis_angs,SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_bravis_lengs, SET_UVALUE=''

  WIDGET_CONTROL, WID_TEXT_bravis_angs,SET_VALUE='1.0'
  WIDGET_CONTROL, WID_TEXT_bravis_lengs, SET_VALUE='0.1'

  WIDGET_CONTROL, WID_BUTTON_30, SET_UVALUE='Apply UB transformation'
  WIDGET_CONTROL, WID_BUTTON_30a, SET_UVALUE='Launch XPREP'

  WIDGET_CONTROL, WID_TEXT_t00, SET_VALUE='1'
  WIDGET_CONTROL, WID_TEXT_t11, SET_VALUE='1'
  WIDGET_CONTROL, WID_TEXT_t22, SET_VALUE='1'

  WIDGET_CONTROL, WID_TEXT_t00, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_t01, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_t02, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_t10, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_t11, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_t12, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_t20, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_t21, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_t22, SET_UVALUE=''

  ;WIDGET_CONTROL, WID_BUTTON_refineom0, SET_UVALUE='Refine om0'

  WIDGET_CONTROL, WID_TEXT_dveqthresh, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_dvlengthmin, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_dvlengthmax, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_fractionreq, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_dvangmin, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_dvangmax, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_dvminfreq, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_indthresh, SET_UVALUE=''
  WIDGET_CONTROL,   WID_LIST_0, SET_UVALUE=''
  WIDGET_CONTROL, WID_LIST_1, SET_UVALUE=''

  WIDGET_CONTROL, WID_BUTTON_Index, SET_UVALUE='Index'
  WIDGET_CONTROL, WID_BUTTON_Reindex, SET_UVALUE='Reindex'
  WIDGET_CONTROL, WID_BUTTON_DV, SET_UVALUE='Difference Vectors'
  WIDGET_CONTROL, WID_BUTTON_6, /SET_BUTTON
  WIDGET_CONTROL, WID_BUTTON_8,  /SET_BUTTON
  WIDGET_CONTROL, WID_LIST_1, SET_UVALUE='Solution list'
  WIDGET_CONTROL,WID_BUTTON_38,  SET_UVALUE='Invert transformation mtx'

  WIDGET_CONTROL,WID_BUTTON_cpn, SET_UVALUE=''
  WIDGET_CONTROL,WID_BUTTON_cpn, /SET_BUTTON
  WIDGET_CONTROL,WID_BUTTON_cdv, SET_UVALUE=''

  WIDGET_CONTROL, WID_BUTTON_41, SET_UVALUE='Reindex and calculate UB'
  WIDGET_CONTROL, WID_DROPLIST_2, SET_UVALUE=''
  WIDGET_CONTROL, WID_DROPLIST_3, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_32, SET_UVALUE='define vector'
  WIDGET_CONTROL, WID_BUTTON_35, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_35, /SET_BUTTON
  WIDGET_CONTROL, WID_BUTTON_36, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_40, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_51, SET_UVALUE='Delete0'
  WIDGET_CONTROL, WID_BUTTON_52, SET_UVALUE='Select0'
  WIDGET_CONTROL, WID_BUTTON_33, SET_UVALUE='Unselect0'
  WIDGET_CONTROL, WID_TEXT_al, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_be, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_ga,SET_UVALUE=''

  ;WIDGET_CONTROL, WID_BUTTON_refineiomega,SET_UVALUE='Refine ind. omega'

  ;WIDGET_CONTROL, WID_TEXT_om0,SET_UVALUE=''
  ;WIDGET_CONTROL, WID_TEXT_om0,SET_VALUE='0.0'
  WIDGET_CONTROL, WID_BUTTON_unitmtx, SET_UVALUE='Change T to E'
  ;WIDGET_CONTROL, WID_BUTTON_change_angles, SET_UVALUE='Change angles'

  WIDGET_CONTROL, WID_BUTTON_6, SET_UVALUE='Draw DV'
  WIDGET_CONTROL, WID_BUTTON_7, SET_UVALUE='Draw Solution'
  WIDGET_CONTROL, WID_BUTTON_8, SET_UVALUE='Draw on'
  WIDGET_CONTROL, WID_SLIDER_VecSS, SET_UVALUE='Vector symbol size'
  WIDGET_CONTROL, WID_BUTTON_10,    SET_UVALUE='Delete_peak'
  WIDGET_CONTROL, WID_BUTTON_11,    SET_UVALUE='Unselect_peak'
  WIDGET_CONTROL, WID_BUTTON_12,    SET_UVALUE='Select_peak'

  WIDGET_CONTROL, WID_BUTTON_pt1, SET_UVALUE='Activate PT1'
  WIDGET_CONTROL, WID_BUTTON_pt2, SET_UVALUE='Activate PT2'
  ;WIDGET_CONTROL, WID_BUTTON_ptv1, SET_UVALUE='Show PT1'
  ;WIDGET_CONTROL, WID_BUTTON_ptv2, SET_UVALUE='Show PT2'
  WIDGET_CONTROL, WID_BUTTON_ptmv, SET_UVALUE='Move selected'

  WIDGET_CONTROL, WID_BUTTON_pt1, SET_BUTTON=1
;  WIDGET_CONTROL, WID_BUTTON_ptv1, SET_BUTTON=1


  ;WIDGET_CONTROL, WID_DROPLIST_sort_field, SET_UVALUE='Sort_field'
  ;WIDGET_CONTROL, WID_BUTTON_Sort_asc, SET_UVALUE='Sort ascending'
  ;WIDGET_CONTROL, WID_BUTTON_sort_desc, SET_UVALUE='Sort descending'
  ;WIDGET_CONTROL, WID_BUTTON_34, SET_UVALUE='Sort'
  ;WIDGET_CONTROL, WID_DROPLIST_sort_field, SET_VALUE=['Intensity','Energy','d-spacing']
  ;WIDGET_CONTROL, WID_BUTTON_Sort_asc, /SET_BUTTON
  WIDGET_CONTROL, WID_BUTTON_44, SET_UVALUE='Bravis'

  WIDGET_CONTROL, WID_BUTTON_Recalcangs, SET_UVALUE='UB=B'

  WIDGET_CONTROL, WID_BUTTON_39, SET_UVALUE='Save_UB'
  WIDGET_CONTROL, WID_BUTTON_43, SET_UVALUE='Open_UB'

  WIDGET_CONTROL, WID_BUTTON_37, SET_UVALUE='Zero rotation'

  ;WIDGET_CONTROL, WID_BUTTON_execute_profiles, SET_UVALUE='Ex improper profiles'
  ;WIDGET_CONTROL, WID_BUTTON_execute_unique, SET_UVALUE='Ex redundant peaks'
  ;WIDGET_CONTROL, WID_BUTTON_impr_prof, SET_UVALUE='Chk improper profiles'
  ;WIDGET_CONTROL, WID_BUTTON_red_peaks, SET_UVALUE='Chk redundant peaks'
  WIDGET_CONTROL, WID_BUTTON_import, SET_UVALUE='Inport ASCII'
  WIDGET_CONTROL, WID_BUTTON_import_UNI, SET_UVALUE='Inport UNI'
  WIDGET_CONTROL, WID_BUTTON_Import_ps, SET_UVALUE='Inport ps'

  WIDGET_CONTROL, WID_TEXT_PD1, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_s0, SET_BUTTON=1
  WIDGET_CONTROL, WID_LIST_5, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_s0, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_s11,SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_s12, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_s13, SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_scale, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_s2, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_s3, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_s4, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_s5,SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_1,SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_dvd,SET_UVALUE='Dd/d'
  WIDGET_CONTROL, WID_BUTTON_dvd0,SET_UVALUE='Separate omega'
  WIDGET_CONTROL, WID_TEXT_dvd1,SET_UVALUE=''
  WIDGET_CONTROL, WID_TEXT_dvd2,SET_UVALUE=''

  WIDGET_CONTROL,WID_BUTTON_XF1, SET_UVALUE='YF'
  WIDGET_CONTROL,WID_BUTTON_XF2, SET_UVALUE='YF'
  WIDGET_CONTROL,WID_BUTTON_XF3, SET_UVALUE='YF'
  WIDGET_CONTROL,WID_BUTTON_XF4, SET_UVALUE='YF'

  WIDGET_CONTROL, WID_BUTTON_YF1, SET_UVALUE='YF'
  WIDGET_CONTROL, WID_BUTTON_YF2, SET_UVALUE='YF'
  WIDGET_CONTROL, WID_BUTTON_YF3, SET_UVALUE='YF'

  WIDGET_CONTROL, WID_BUTTON_dvda, SET_UVALUE='Select w/ I'
  WIDGET_CONTROL, WID_TEXT_dvda2, SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_dvdb, SET_UVALUE='Select w/ FWHM'
  WIDGET_CONTROL, WID_TEXT_dvdb2,  SET_UVALUE=''
  WIDGET_CONTROL, WID_BUTTON_calcT,  SET_UVALUE='Calculate T'


  WIDGET_CONTROL,WID_BUTTON_XF1, SET_BUTTON=1
  WIDGET_CONTROL,WID_BUTTON_YF1, SET_BUTTON=1

  WIDGET_CONTROL, WID_TEXT_1,SET_VALUE='1.0'

  WIDGET_CONTROL,  WID_BUTTON_Cellnow,SET_UVALUE='CellNow'
  WIDGET_CONTROL,  WID_BUTTON_read_from_p4p,SET_UVALUE='ReadUBfrom_p4p'

  wset,WID_DRAW_0
  loadct, 2
  read_jpeg, 'start.jpg', pic, true=3
  tvscl, congrid(pic[*,*,0:2], 482, 482, 3), true=3

end


;--------------------------------------------------------------------


