;----------------------------------

function check_ab, a,b,lo,hi

     COMMON Diffs,diffsN, diffsV, diffsL, diffNO

     ra=diffsV[a].vec
     rb=diffsV[b].vec
     ga=ang_between_vecs(ra, rb)
     if ga gt lo and  ga lt hi then result=0 else result=1
     return, result

end
; -------------------------------------------------------------------------
; -----------------------  Calculates point coordinates along circle  -----------
; -------------------------------------------------------------------------

   FUNCTION CIRCLE, xcenter, ycenter, radius
    points = (2.0 * !PI / 99.0) * FINDGEN(100)
    x = xcenter + radius * COS(points )
    y = ycenter + radius * SIN(points )
    RETURN, TRANSPOSE([[x],[y]])
   END


; -------------------------------------------------------------------------

function check_overlaps, pt, i, ds
 ovl=0
 for j=0, pt.peakno-1 do $
 begin
   dst=1.0/vlength(pt.peaks[i].xyz-pt.peaks[j].xyz)
   if (i ne j) and abs(pt.peaks[i].xyz[1]-pt.peaks[j].xyz[1]) lt 0.01 and abs(pt.peaks[i].xyz[2]-pt.peaks[j].xyz[2]) lt 0.01 and (round(ds/dst)-ds/dst lt 0.1)  then $
   begin
      ovl=1
      return, 1
   endif
 endfor
 return, ovl
end
; -------------------------------------------------------------------------

pro mark_proper_overlaps, optable, WID_DRAW_0, ws, ds

   rtp=optable->get_object()

   wset, WID_DRAW_0
   ERASE
   center=[241,241]
   if rtp.peakno gt 0 then for i=1, rtp.peakno do $
   begin
       if check_overlaps(rtp, i, ds) eq 0 then rtp.peaks[i].selected[0]=1
       ;    if check_overlaps(rtp, i, ds) eq 0 then col=ws.colors[1] else col=ws.colors[2]
       ;    POLYFILL, CIRCLE(center[0]+ws.xy[0]+rtp.peaks[i-1].xyz[1]*ws.scl, center[1]+ws.xy[1]+rtp.peaks[i-1].xyz[2]*ws.scl, ws.circ), /Device, color=col

   endfor
   optable->set_object, rtp
end

; -------------------------------------------------------------------------
; -----------------------  Draws rpeaks in drwindow             -----------
; -------------------------------------------------------------------------

pro draw_rp, optable, WID_DRAW_0, ws

   rtp=optable->get_object()

   wset, WID_DRAW_0
   ERASE
   center=[241,241]
   if rtp.peakno gt 0 then for i=1, rtp.peakno do $
   begin
           if rtp.peaks[i-1].selected[0] eq 0 then col=ws.colors[1] else col=ws.colors[2]
           POLYFILL, CIRCLE(center[0]+ws.xy[0]+rtp.peaks[i-1].xyz[1]*ws.scl, center[1]+ws.xy[1]+rtp.peaks[i-1].xyz[2]*ws.scl, ws.circ), /Device, color=col

   endfor
end

;---------------------------------------

pro rotate_peaks, opt, ax, angle, show
   pt=opt->get_object()

   common Rot,Rotations
   common Rota, Mtx

   GenerateR, ax, angle
   Rotations=Mtx##Rotations
   for i=1, pt.peakno do $
   begin
     pt.peaks[i-1].xyz=Mtx##pt.peaks[i-1].xyz
   endfor
   opt->set_object, pt
end

;---------------------------------------

pro aply_rotation, opt, rot
common Rot,Rotations

   pt=opt->get_object()
   for i=1, pt.peakno do $
   begin
     pt.peaks[i-1].xyz=rot##pt.peaks[i-1].xyz
   endfor
   opt->set_object, pt
   rotations=rot ## rotations

end

;---------------------------------------

pro draw_box, box00x, box00y, x, y, ws, WID_DRAW_0
                 center=[241,241]
                 wset, WID_DRAW_0
                 PLOTS, box00x*ws.scl+center[0]+ws.xy[0], box00y*ws.scl+center[1]+ws.xy[1], /DEVICE, color=ws.colors[3]
                 PLOTS, box00x*ws.scl+center[0]+ws.xy[0], y,      /CONTINUE, /DEVICE, color=ws.colors[3]
                 PLOTS, x, y,           /CONTINUE, /DEVICE, color=ws.colors[3]
                 PLOTS, x, box00y*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]
                 PLOTS, box00x*ws.scl+center[0]+ws.xy[0], box00y*ws.scl+center[1]+ws.xy[1], /CONTINUE, /DEVICE, color=ws.colors[3]
end

pro draw_vector,WID_DRAW_0,vec,ws
                 common Rot,Rotations
                 center=[241,241]
                 vec1=Rotations ## vec
                 wset, WID_DRAW_0
                 PLOTS, center[0]+ws.xy[0], center[1]+ws.xy[1], /DEVICE, color=ws.colors[3]
                 PLOTS, vec1[1]*ws.scl+center[0]+ws.xy[0], vec1[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]
end


pro draw_cell,WID_DRAW_0,vec1a,vec2a,vec3a,ws
                 common Rot,Rotations

                 vec1=Rotations ## (vec1a)
                 vec2=Rotations ## (vec2a)
                 vec3=Rotations ## (vec3a)
                 vec12=vec1+vec2
                 vec31=vec3+vec1
                 vec32=vec3+vec2
                 vec321=vec3+vec2+vec1

                 center=[241,241]
                 wset, WID_DRAW_0
                 PLOTS, center[0]+ws.xy[0], center[1]+ws.xy[1], /DEVICE, color=ws.colors[3]
                 PLOTS, vec1[1]*ws.scl+center[0]+ws.xy[0], vec1[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]
                 PLOTS, vec12[1]*ws.scl+center[0]+ws.xy[0], vec12[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]
                 PLOTS, vec2[1]*ws.scl+center[0]+ws.xy[0], vec2[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]
                 PLOTS, center[0]+ws.xy[0], center[1]+ws.xy[1], /DEVICE, color=ws.colors[3], /CONTINUE
                 PLOTS, vec3[1]*ws.scl+center[0]+ws.xy[0], vec3[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]
                 PLOTS, vec31[1]*ws.scl+center[0]+ws.xy[0], vec31[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]
                 PLOTS, vec321[1]*ws.scl+center[0]+ws.xy[0], vec321[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]
                 PLOTS, vec32[1]*ws.scl+center[0]+ws.xy[0], vec32[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]
                 PLOTS, vec3[1]*ws.scl+center[0]+ws.xy[0], vec3[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]

                 PLOTS, vec2[1]*ws.scl+center[0]+ws.xy[0], vec2[2]*ws.scl+center[1]+ws.xy[1],      /DEVICE, color=ws.colors[3]
                 PLOTS, vec32[1]*ws.scl+center[0]+ws.xy[0], vec32[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]

                 PLOTS, vec1[1]*ws.scl+center[0]+ws.xy[0], vec1[2]*ws.scl+center[1]+ws.xy[1],      /DEVICE, color=ws.colors[3]
                 PLOTS, vec31[1]*ws.scl+center[0]+ws.xy[0], vec31[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]

                 PLOTS, vec12[1]*ws.scl+center[0]+ws.xy[0], vec12[2]*ws.scl+center[1]+ws.xy[1],      /DEVICE, color=ws.colors[3]
                 PLOTS, vec321[1]*ws.scl+center[0]+ws.xy[0], vec321[2]*ws.scl+center[1]+ws.xy[1],      /CONTINUE, /DEVICE, color=ws.colors[3]

end


;-------------------------------------------------
;------------------------------------------------------------------
;              Calculation of difference vectors
;                uses XYZ from peaktable.XYZ
;------------------------------------------------------------------

pro calculate_differences, optb, inpar

ptb=optb->get_object()


  COMMON Diffs,diffsN, diffsV, diffsL, diffNO

  mindif=999.0                 ; minimum difference vector length
  diffsN=intarr(10000)         ; Difference vector frequency
  difa={vedi, vec: fltarr(3)}  ; Difference vector coordinates
  diffsV=replicate(difa, 10000)
  diffsL= fltarr(10000)          ; Difference vector lengths


  vecd=[0.0,0.0,0.0]
  vec1=[0.0,0.0,0.0]
  diffno=0L                    ; Number of difference vectors
  ang=0.0
  inpar.dv.coli=3.0
  for i=1, ptb.peakno-1 do $
  begin
    for j=i+1, ptb.peakno do $
    begin
     vecd=ptb.peaks[i-1].XYZ - ptb.peaks[j-1].XYZ
     leng=vlength(vecd)
     if 1.0/leng gt inpar.dv.lmin and 1.0/leng lt inpar.dv.lmax then $
     begin ; is of proper length, i.e. direct 4-40 A
       match=0
       k=0L
       if diffno ne 0 then $
       while k le diffno and match eq 0 do $
       begin
          vec1=diffsV[k].vec
          ang=ANG_BETWEEN_VECS(vec1, vecd)
          if abs(ang) lt inpar.dv.coli or abs(ang-180.) lt inpar.dv.coli then $ ; COLINEAR
          begin
            rlp=vlength(vecd)/vlength(vec1)
            ilp=round(rlp)
            if abs(ang) lt inpar.dv.coli then slp=1 else slp=-1
            if abs(ang) lt inpar.dv.coli then slp1=1 else slp1=-1
            rlp1=vlength(vec1)/vlength(vecD)
            ilp1=Round(rlp)
            if abs(rlp-ilp) lt inpar.dv.eqth and ilp ne 0 then $ ; exact match
            begin
              match=1
              vec1=diffsN[k]*diffsV[k].vec
              diffsV[k].vec=(vec1+slp*vecd)/(diffsN[k]+ilp)
              diffsL[k]=vlength(diffsV[k].vec)
              diffsN[k]=diffsN[k]+1
            endif
            if abs(rlp1-ilp1) lt inpar.dv.eqth and ilp1 ne 0 then $ ; exact match
            begin
              match=1
              vec1=diffsN[k]*(diffsV[k].vec/ilp1)
              diffsV[k].vec=(SLP*vec1+vecd)/(diffsN[k]+1)
              diffsL[k]=vlength(diffsV[k].vec)
              diffsN[k]=diffsN[k]+1
              ;print, diffsL[k]
            endif

          endif
          k=k+1
       endwhile
       if (match eq 0 or diffno eq 0) and diffno lt 9999 then $
       begin ; new difference vector
         diffsN[diffno]=1
         diffsV[diffno].vec=vecd
         diffsL[diffno]=vlength(vecd)
         diffno=diffno+1
       end
     endif ; proper length
    endfor ;j
  endfor ;i
end


;------------------------------------------------------------------
;              Sort difference vectors according to length
;------------------------------------------------------------------

pro sort_differences_L

  COMMON Diffs


  vecV=[0.0,0.0,0.0]
  vecN=0L
  vecL=0.0
  l=0L


  for i=1, diffNO-1 do $
  begin
      ind=0L
      while diffsL[i] gt diffsL[ind] and ind lt i do ind=ind+1
      if ind lt i then $
      begin
       vecV=diffsV[i]
       vecN=diffsN[i]
       vecL=diffsL[i]
       for j=0, i-ind-1 do $
       begin
        k=i-j
        diffsV[k]=diffsV[k-1]
        diffsN[k]=diffsN[k-1]
        diffsL[k]=diffsL[k-1]
       endfor
       diffsV[ind]=vecV
       diffsN[ind]=vecN
       diffsL[ind]=vecL
      endif
  endfor
end


;------------------------------------------------------------------
;              Filters difference vectors according to frequency
;------------------------------------------------------------------

pro filter_differences_N, minN

  COMMON Diffs,diffsN, diffsV, diffsL, diffNO

  diffsN2=intarr(10000)         ; Difference vector frequency
  difa={vedi, vec: fltarr(3)}  ; Difference vector coordinates
  diffsV2=replicate(difa, 10000)
  diffsL2= fltarr(10000)          ; Difference vector lengths

  ind=0L
  for i=1, diffNO do $
  begin
      if diffsN[i-1] ge  minN then $
      begin
        diffsN2[ind]=diffsN[i-1]
        diffsL2[ind]=diffsL[i-1]
        diffsV2[ind].vec=diffsV[i-1].vec
        ind=ind+1
      endif
  endfor
  diffNO=ind
  diffsN=diffsN2
  diffsL=diffsL2
  diffsV=diffsV2
end

;================================================================
;=======   Sorts reciprocal vectors according to frequency   ====
;================================================================


pro sort_differences_N

  COMMON Diffs,diffsN, diffsV, diffsL, diffNO


  vecV=[0.0,0.0,0.0]
  vecN=0L
  vecL=0.0
  l=0L


  for i=1, diffNO-1 do $
  begin
      ind=0L
      while diffsN[i] lt diffsN[ind] and ind lt i do ind=ind+1
      if ind lt i then $
      begin
       vecV=diffsV[i]
       vecN=diffsN[i]
       vecL=diffsL[i]
       for j=0, i-ind-1 do $
       begin
        k=i-j
        diffsV[k]=diffsV[k-1]
        diffsN[k]=diffsN[k-1]
        diffsL[k]=diffsL[k-1]
       endfor
       diffsV[ind]=vecV
       diffsN[ind]=vecN
       diffsL[ind]=vecL
      endif
  endfor
end

function reind, a,b,c, limit, fra, fom, mode, opt
; mode = 0, define ub using diffsv
; mode = 2, define ub using a, b and c

  pt=opt->get_object()


  COMMON CLASS_crystal_objects, ocrystal
  COMMON Tabsrefs, Tprojects, Tcrystal, Timage, Tpeaks, Tsettings
  COMMON Diffs,diffsN, diffsV, diffsL, diffNO
  COMMON Indices, HKLs, UB
  Common local, peakno1
  COMMON out_angs, omega, chi, energy
  COMMON out_XYZ, XYZ
 COMMON CLASS_crystal_objects, ocrystal


    UB =fltarr(3,3)
    iUB=fltarr(3,3)

  if mode eq 0 then $
  begin
    ra=diffsV[a].vec
    rb=diffsV[b].vec
    rc=diffsV[c].vec
    for j=0, 2 do UB[0,J]=ra[j]
    for j=0, 2 do UB[1,J]=rb[j]
    for j=0, 2 do UB[2,J]=rc[j]
    Tcrystal.UB_matrix=UB
  endif else $
  if mode eq 2 then $
  begin
    for j=0, 2 do $
    begin
        UB[0,J]=a[j]
        UB[1,J]=b[j]
        UB[2,J]=c[j]
    endfor
    Tcrystal.UB_matrix=UB
  endif else $
  begin
    UB=Tcrystal.UB_matrix
  endelse

 opt2=obj_new('CLASS_peaktable')

For laps=1, 1 do $
begin
  iUB=Invert(UB)
  rhkl=[0.0,0.0,0.0]

 opt2->set_object, pt
 pt2=opt2->get_object()

  ; Indexing

  cr=ocrystal->get_object()

  cr.UB_matrix=UB
  ocrystal->set_object, cr
  pn=opt2->reindex(ocrystal, limit)

  pt2=opt2->get_object()
  if pn lt pt.peakno*fra  then $
  begin
    result=1
    goto, finit
  endif else $
  begin
    fit_UB_Matrix, pt2
    iUB=Invert(UB)

    opt2->set_object, pt
    cr=ocrystal->get_object()
    cr.UB_matrix=UB
    ocrystal->set_object, cr
    pn=opt2->reindex(ocrystal, limit)

   ; prepare_indexed_ptable, limit, opt2, aa
    pt2=opt2->get_object()

    result=0
  endelse
endfor

  ; prepare final indices

    finit:
  opt->set_object, pt2
  fom=opt->calculate_indexing_fom(ocrystal, limit)
  obj_destroy, opt2
  return, pn

end

;----------------------------------------------------




;================================================================
;=======   choses rbasis and indexes peaks                   ====
;================================================================


pro pick_rbasis_and_idex, inpar, opt, solus, soluno,WID_LIST_1

  COMMON Diffs,diffsN, diffsV, diffsL, diffNO
  COMMON Indices, HKLs, UB
  Common local, peakno1
  COMMON out_angs, omega, chi, energy
  COMMON out_XYZ, XYZ
  COMMON sol,solutions, solno
  COMMON setting_choice, sc

  limit=inpar.in.thre
  fra=inpar.in.fract

; Difference vectors are sorted according to decreasing length and filtered with
; respect to their frequency

  opt2=obj_new('CLASS_peaktable')

  pt=opt->get_object()
  opt2->set_object, pt

  tpeakno=pt.peakno
  soluno=0
  hkls=FLTARR(1000,3)

  amin=inpar.dv.amin
  amax=inpar.dv.amax


  UB=fltarr(3,3) ; local copy of the UB matrix
  iUB=fltarr(3,3) ; inverse of the UB matrix
;  limit=0.1
  ;======================================================================
  indA=0
  print_difference_vectors, WID_LIST_1
  for indA=0, diffno-3 do $
  begin
    for indB=indA+1, diffno-2 do $
    begin
      result=check_ab(indA,indB,amin,amax)
      if result eq 1 then goto, nextB
      for indC=indB+1, diffno-1 do $
      begin
        result=check_ab(indA,indC,amin,amax)
        if result eq 1 then goto, nextC
        result=check_ab(indB,indC,amin,amax)
        if result eq 1 then goto, nextC
        fom=0
        opt2->set_object, pt
        result=reind(inda,indb,indc, limit, fra, fom,0, opt2)
        pt2=opt2->get_object()
        ;print, result, inda,indb,indc, pt2.peakno
        if result lt pt.peakno*fra then goto, nextC else $
        begin
            ra=diffsV[inda].vec
            rb=diffsV[indb].vec
            rc=diffsV[indc].vec
            solus[soluno].bvecs=[inda, indb, indc]
            solus[soluno].UB[0:2,0]=diffsV[inda].vec
            solus[soluno].UB[0:2,1]=diffsV[indb].vec
            solus[soluno].UB[0:2,2]=diffsV[indc].vec
            solus[soluno].UT=[[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]]
            solus[soluno].fraind=result
            solus[soluno].fom=fom
            solus[soluno].lparams=lp_from_ub(solus[soluno].UB)
            soluno=soluno+1
            ;Print, 'Vectors:', inda, indb, indc, '  Total/indexed:', pt2.peakno, '/', tpeakno,'   fom=',fom/pt2.peakno
        endelse
      nextC:
      endfor ; indc
    nextB:
    endfor ;indb
  nextA:
  endfor ;inda
  ;====================================================
  ;print,solutions.indno
  if soluno gt 0 then $
  begin
    bestsolution=max(solus.fraind,i)
    result=reind(solus[i].bvecs[0],solus[i].bvecs[1],solus[i].bvecs[2], limit, fra, fom,0, opt2)
    print_indexing_solutions, solus, soluno, WID_LIST_1
  endif
  opt->set_object, pt
  obj_destroy, opt2
  sc=0
 end





;==================================================================
; selects from ptable the peaks that have integer indices
;==================================================================


pro prepare_indexed_ptable, limit, opt, HKLs

  ;COMMON Indices
  common local

  pt=opt->get_object()
  K=1
  for i=1, pt.peakno do $
  begin
     if (abs(HKLs[K-1,0]-round(HKLs[K-1,0])) gt limit) or $
        (abs(HKLs[K-1,1]-round(HKLs[K-1,1])) gt limit) or $
        (abs(HKLs[K-1,2]-round(HKLs[K-1,2])) gt limit) then $
        BEGIN
         opt->delete_peak, k-1         ;remove_rpeak, K
         K=K-1
        ENDIF
        ;else $
        ;begin
        ;  HKLs[K-1,0]=Round(HKLs[K-1,0])
        ;  HKLs[K-1,1]=Round(HKLs[K-1,1])
        ;  HKLs[K-1,2]=Round(HKLs[K-1,2])
        ;endelse
        K=K+1
  endfor
end

; ==================================================================

; leat squares fit of the unit cell parameters
; requires HKLs with indices and peaktableXYZ with rvector coordinates
; updates the components of the UB

; ==================================================================


pro print_difference_vectors,WID_LIST_1

COMMON diflist, rlist
COMMON Diffs,diffsN, diffsV, diffsL, diffNO

  list=''
  list0=''
  for i=0, diffno-1 do $
  begin
   list=string(i)+ string(diffsn[i])+string(1/diffsl[i])
   if i eq 0 then list0=list else list0=[list0,list]
  endfor
  widget_control,WID_LIST_1 , set_value=list0

end

;----------------------------------------------------


;-------------------------------------------------

pro print_indexing_solutions, solus, soluno, WID_LIST_1

  list=''
  list0=''
  for i=0, soluno-1 do $
  begin
   list=string(i)+ string(solus[i].bvecs[0])+ string(solus[i].bvecs[1])+ string(solus[i].bvecs[2])+string(solus[i].fraind)+string(solus[i].fom)
   if i eq 0 then list0=list else list0=[list0,list]
  endfor
  widget_control, WID_LIST_1, set_value=list0

end

;------------------------------------------------------------

pro lattice_params_from_UB,  UB, Tcrystal
; Calculate lattice parameters from UB matrix

 gstar=transpose(UB)##UB
 g=invert(gstar)

 det=determ(gstar, /check)
 if det ne 0.0 then V=sqrt(1/det) else goto, erro
 pi=acos(-1.0)

 Tcrystal.cell_volume=V

 Tcrystal.cell_parameters[0]=sqrt(g[0,0])
 Tcrystal.cell_parameters[1]=sqrt(g[1,1])
 Tcrystal.cell_parameters[2]=sqrt(g[2,2])

 Tcrystal.cell_parameters[3]=acos(g[2,1]/(Tcrystal.cell_parameters[1]*Tcrystal.cell_parameters[2]))*180.0/pi
 Tcrystal.cell_parameters[4]=acos(g[2,0]/(Tcrystal.cell_parameters[0]*Tcrystal.cell_parameters[2]))*180.0/pi
 Tcrystal.cell_parameters[5]=acos(g[1,0]/(Tcrystal.cell_parameters[0]*Tcrystal.cell_parameters[1]))*180.0/pi
 erro:
end



;=============================================

function display_cell, ocrystal
 Tcrystal=ocrystal->get_object()
 st=''
 st=st + '   a = '+STRCOMPRESS(string(Tcrystal.cell_parameters[0]), /REMOVE_ALL)
 st=st + '   b = '+STRCOMPRESS(string(Tcrystal.cell_parameters[1]), /REMOVE_ALL)
 st=st + '   c = '+STRCOMPRESS(string(Tcrystal.cell_parameters[2]), /REMOVE_ALL)
 st1=''
 st1=st1 + '  al = '+STRCOMPRESS(string(Tcrystal.cell_parameters[3]), /REMOVE_ALL)
 st1=st1 + '  be = '+STRCOMPRESS(string(Tcrystal.cell_parameters[4]), /REMOVE_ALL)
 st1=st1 + '  ga = '+STRCOMPRESS(string(Tcrystal.cell_parameters[5]), /REMOVE_ALL)
 st31=string(Tcrystal.UB_matrix[0,0])+string(Tcrystal.UB_matrix[0,1])+string(Tcrystal.UB_matrix[0,2])
 st32=string(Tcrystal.UB_matrix[1,0])+string(Tcrystal.UB_matrix[1,1])+string(Tcrystal.UB_matrix[1,2])
 st33=string(Tcrystal.UB_matrix[2,0])+string(Tcrystal.UB_matrix[2,1])+string(Tcrystal.UB_matrix[2,2])
 st2=['Unit cell parameters:', st,st1, 'Orientation matrix: ', st31, st32, st33]
 return, st2
end

pro fit_UB_Matrix, pt

@Main_Component_Common

COMMON Indices
COMMON local
COMMON Tabsrefs, Tprojects, Tcrystal, Timage, Tpeaks, Tsettings

x1=fltarr(pt.peakno-pt.selectedno)
x2=fltarr(pt.peakno-pt.selectedno)
x3=fltarr(pt.peakno-pt.selectedno)
y=fltarr(pt.peakno-pt.selectedno)
count=0
for i=1, pt.peakno-1 do $
if pt.peaks[i-1].selected[0] and $
not (pt.peaks[i-1].HKL[0] eq 0 and $
     pt.peaks[i-1].HKL[1] eq 0 and $
     pt.peaks[i-1].HKL[2] eq 0)  $
then $
begin
  X1[count]=pt.peaks[i-1].HKL[0]
  X2[count]=pt.peaks[i-1].HKL[1]
  X3[count]=pt.peaks[i-1].HKL[2]
  count=count+1
end
end
x=fltarr(3,pt.peakno-pt.selectedno)
X = [TRANSPOSE(X1), TRANSPOSE(X2), TRANSPOSE(X3)]
for j=0, 2 do $
begin
   count=0
   for i=1, pt.peakno-1 do $
   if pt.peaks[i-1].selected[0] eq 0 then $
   begin
      Y[count]= pt.peaks[i-1].XYZ[j]
      count=count+1
   endif
   measure_errors = REPLICATE(0.01, N_ELEMENTS(Y))
   result = REGRESS(X, Y, SIGMA=sigma, CONST=const, $
   MEASURE_ERRORS=measure_errors, CHISQ=chq)
   UB[0,j]=RESULT[0]
   UB[1,j]=RESULT[1]
   UB[2,j]=RESULT[2]
endfor
 vecra=[UB[0,0],UB[0,1],UB[0,2]]
 vecrb=[UB[1,0],UB[1,1],UB[1,2]]
 vecrc=[UB[2,0],UB[2,1],UB[2,2]]

; Calculate lattice parameters from UB matrix

 lattice_params_from_UB,  UB, Tcrystal

 Tcrystal.UB_matrix=UB

 COMMON CLASS_crystal_reference, cryst
 COMMON CLASS_crystal_objects, ocrystal

 ocrystal->set_object, Tcrystal

 a=display_cell(ocrystal)
 WIDGET_CONTROL, WID_TEXT_3, SET_VALUE=a

end ;==================================================================

