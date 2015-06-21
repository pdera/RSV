
function get_trans, TMS, i
  return, transpose(TMS[i,0:2,0:2])
end



function crystal_system, cell_parameters, ang, edg

orthogonal=0

if abs(cell_parameters[3]-90.0) lt ang and $
   abs(cell_parameters[4]-90.0) lt ang and $
   abs(cell_parameters[5]-90.0) lt ang then orthogonal=1

if orthogonal eq 1 then $
begin
if abs(cell_parameters[0]-cell_parameters[1]) lt edg and $
   abs(cell_parameters[0]-cell_parameters[2]) lt edg and $
   abs(cell_parameters[1]-cell_parameters[2]) lt edg then $
   begin
     result='cubic'
     goto, fin
   endif else $
   if abs(cell_parameters[0]-cell_parameters[1]) lt edg or $
      abs(cell_parameters[0]-cell_parameters[2]) lt edg or $
      abs(cell_parameters[1]-cell_parameters[2]) lt edg then $
   begin
      result='tetragonal'
      goto, fin
   endif else $
   begin
      result='orthorhombic'
      goto, fin
   endelse
endif else $  ;  othogonal
begin
  if (abs(cell_parameters[3]-120.0) lt ang and abs(cell_parameters[4]-  90.0) lt ang and abs(cell_parameters[5]- 90.0) lt ang and abs(cell_parameters[1]-cell_parameters[2]) lt edg) or $
     (abs(cell_parameters[3]- 90.0) lt ang and abs(cell_parameters[4]- 120.0) lt ang and abs(cell_parameters[5]- 90.0) lt ang and abs(cell_parameters[0]-cell_parameters[2]) lt edg) or $
     (abs(cell_parameters[3]- 90.0) lt ang and abs(cell_parameters[4]-  90.0) lt ang and abs(cell_parameters[5]-120.0) lt ang and abs(cell_parameters[0]-cell_parameters[1]) lt edg) then $
  begin
      result='hexagonal'
      goto, fin
  endif else $
  if (abs(cell_parameters[3]- 90.0) lt ang and abs(cell_parameters[4]-  90.0) lt ang )  or $
     (abs(cell_parameters[3]- 90.0) lt ang and abs(cell_parameters[5]-  90.0) lt ang )  or $
     (abs(cell_parameters[4]- 90.0) lt ang and abs(cell_parameters[5]-  90.0) lt ang )  then $
  begin
      result='monoclinic'
      goto, fin
  endif else $
  if (abs(cell_parameters[3]- cell_parameters[4]) lt ang)  and $
     (abs(cell_parameters[3]- cell_parameters[5]) lt ang)  and $
     (abs(cell_parameters[4]- cell_parameters[5]) lt ang)  and $
     abs(cell_parameters[0]-cell_parameters[1]) lt edg and $
     abs(cell_parameters[0]-cell_parameters[2]) lt edg and $
     abs(cell_parameters[1]-cell_parameters[2]) lt edg and $
     not (cell_parameters[0] eq 0.0 or cell_parameters[3] eq 0.0) $
     then $
  begin
      result='rhombohedral'
      goto, fin
  endif else $
  begin
      result='triclinic'
      goto, fin
  endelse
endelse ; non orthogonal
fin:
return, result
end

;---------------------------------------

function rate_crystal_system, sys
case sys of
'triclinic'   : s=1
'monoclinic'  : s=2
'orthorhombic': s=3
'rhombohedral': s=4
'hexagonal'   : s=5
'tetragonal'  : s=6
'cubic'       : s=7
else : s=0
endcase
return, s
end

;---------------------------

function unique_axis, a, symmetry, angl, lengl
res=-1
case symmetry of
'monoclinic': $
 begin
   mon=max([abs(a[3]-90.0), abs(a[4]-90.0), abs(a[5]-90.0)], i)
   case i of
   0: if abs(a[4]-90.0) lt angl and abs(a[5]-90.0) lt angl then res=0
   1: if abs(a[3]-90.0) lt angl and abs(a[5]-90.0) lt angl then res=1
   2: if abs(a[3]-90.0) lt angl and abs(a[4]-90.0) lt angl then res=2
   endcase
 end
'hexagonal': $
 begin
   mon=max([abs(a[3]-90.0), abs(a[4]-90.0), abs(a[5]-90.0)], i)
   case i of
   0: if abs(a[1]-a[2]) lt lengl then res=0
   1: if abs(a[0]-a[2]) lt lengl then res=1
   2: if abs(a[0]-a[1]) lt lengl then res=2
   endcase
 end
'tetragonal': $
 begin
   mon=min([abs(a[0]-a[1]), abs(a[0]-a[2]), abs(a[2]-a[1])], i)
   case i of
   0: if mon lt lengl then res=2
   1: if mon lt lengl then res=1
   2: if mon lt lengl then res=0
   endcase
 end
 else:
endcase
return, res
;-1=not monoclinic
; 0=a-unique
; 1=b-unique
; 2=c-unique
end

;================================================================

function Bravis, ub, optable1, brav

; This function determines the crystal system and checks for all possible settings that
; do not change the cell volume
; The results = alternative settings are available through COMMON ind_alt

COMMON  ind_sol, solu, solus, soluno
COMMON CLASS_crystal_objects, ocrystal
COMMON CLASS_crystal_reference, cryst
COMMON ind_alt, altsols1, altsolno1

altsols=replicate(solu, 2500)
altsolno=0

altsols1=replicate(solu, 2500)
altsolno1=0

;systems
;
;1- triclinic
;2- monoclinic
;3- orthorhombic
;4- rhombohedral
;5- hexagonal
;6- tetragonal
;7- cubic
;

; -- generate all possible transformation matrices ----

TM=fltarr(3,3)
TMS=fltarr(2500,3,3)
pos=0
for i11= 0, 1 do $
 for i12=-1, 1 do $
  for i13=-1, 1 do $
   for i21=-1, 1 do $
    for i22=-1, 1 do $
     for i23=-1, 1 do $
      for i31=-1, 1 do $
       for i32=-1, 1 do $
        for i33=-1, 1 do $
        begin
          TM[0,0]=float(i11)
          TM[0,1]=float(i12)
          TM[0,2]=float(i13)
          TM[1,0]=float(i21)
          TM[1,1]=float(i22)
          TM[1,2]=float(i23)
          TM[2,0]=float(i31)
          TM[2,1]=float(i32)
          TM[2,2]=float(i33)
          if determ(TM,/CHECK) eq 1.0 then $
          begin
            TMS[pos,0:2,0:2]=TM
            pos=pos+1
          end;if
        endfor

        ;--- determine proper crystal system ---

        maxsys=0
        maxsol=0
        altsolno=0

        for i=0, pos-1 do $
        begin
           t=get_trans(TMS, i)
           UBm=t # UB
           ;at=[[1.0,-1.0,0.0],[1.0,0.0,-1.0],[0.0,1.0,-1.0]]
           ;UBm=at##UBM
           cryst.cell_parameters=lp_from_ub(UBm)
           a=crystal_system(cryst.cell_parameters, brav[0], brav[1])
           b=rate_crystal_system(a)
           if b gt maxsys then $
           begin
              maxsys=b
              maxsol=i
              altsolno=0
              altsols[altsolno].UT=t
              altsols[altsolno].UB=UB
              altsols[altsolno].lparams=cryst.cell_parameters
              altsols[altsolno].sym=b
           endif $
           else if b eq maxsys then $
           begin
              altsolno=altsolno+1
              altsols[altsolno].UT=t
              altsols[altsolno].UB=UB
              altsols[altsolno].lparams =cryst.cell_parameters
              altsols[altsolno].sym=b
           end
        endfor


        ;oc=ocrystal->get_object()
        ;oc.UB_matrix=altsols[maxsol].UT # altsols[maxsol].UB
        ;ocrystal->set_object, oc
        ;an=optable1->reindex(ocrystal, 0.1)
        ;cryst.cell_parameters=lp_from_ub(oc.UB_matrix)

        ;--- leave only unique solutions ----


         UB=UBm
         altsols1=altsols
         altsolno1=0


         COMMON request, proceed

         if maxsys eq 1 then $
           begin
              WIDGET_triclinic_warning
              ;tricl_set_uval

           endif else proceed=1

         if proceed eq 1 then begin
         for i=0, altsolno-1 do if altsols[i].sym eq maxsys then $
         begin
              redund=0
              for j=1,i-1 do if altsols[i].sym eq maxsys then $
              begin
                ; monoclinic case
                an=crystal_system(altsols[i].lparams, 0.3, 0.1)
                case an of
                'monoclinic': $
                begin
                  ai=unique_axis(altsols[i].lparams, an, brav[0], brav[1])
                  aj=unique_axis(altsols[j].lparams, an, brav[0], brav[1])
                  if (abs(altsols[i].lparams[3+ai]-altsols[j].lparams[3+aj]) lt brav[0] and $
                      abs(altsols[i].lparams[ai]-altsols[j].lparams[aj]) lt brav[1]) then goto, next
                end
                'hexagonal': $
                begin
                  ai=unique_axis(altsols[i].lparams, an, brav[0], brav[1])
                  aj=unique_axis(altsols[j].lparams, an, brav[0], brav[1])
                  if abs(altsols[i].lparams[ai]-altsols[j].lparams[aj]) lt brav[1] and $
                     abs(altsols[i].lparams[(ai+1) mod 3]-altsols[j].lparams[(aj+1) mod 3]) lt brav[1] and $
                     abs(altsols[i].lparams[(ai+2) mod 3]-altsols[j].lparams[(aj+2) mod 3]) lt brav[1] then goto, next
                end
                'tetragonal': $
                begin
                  ai=unique_axis(altsols[i].lparams, an, brav[0], brav[1])
                  aj=unique_axis(altsols[j].lparams, an, brav[0], brav[1])
                  if abs(altsols[i].lparams[ai]-altsols[j].lparams[aj]) lt brav[1] and $
                     abs(altsols[i].lparams[(ai+1) mod 3]-altsols[j].lparams[(aj+1) mod 3]) lt brav[1] and $
                     abs(altsols[i].lparams[(ai+2) mod 3]-altsols[j].lparams[(aj+2) mod 3]) lt brav[1] then goto, next
                end
                'triclinic':$
                begin
                  if (abs(altsols[i].lparams[0]-altsols[j].lparams[1]) lt brav[1] and $
                      abs(altsols[i].lparams[3]-altsols[j].lparams[4]) lt brav[1] and $
                      abs(altsols[i].lparams[1]-altsols[j].lparams[2]) lt brav[1] and $
                      abs(altsols[i].lparams[4]-altsols[j].lparams[5]) lt brav[1] and $
                      abs(altsols[i].lparams[2]-altsols[j].lparams[0]) lt brav[1] and $
                      abs(altsols[i].lparams[5]-altsols[j].lparams[3]) lt brav[1]) or $

                     (abs(altsols[i].lparams[0]-altsols[j].lparams[2]) lt brav[1] and $
                      abs(altsols[i].lparams[3]-altsols[j].lparams[5]) lt brav[1] and $
                      abs(altsols[i].lparams[1]-altsols[j].lparams[0]) lt brav[1] and $
                      abs(altsols[i].lparams[4]-altsols[j].lparams[3]) lt brav[1] and $
                      abs(altsols[i].lparams[2]-altsols[j].lparams[1]) lt brav[1] and $
                      abs(altsols[i].lparams[5]-altsols[j].lparams[4]) lt brav[1]) or $

                     (abs(altsols[i].lparams[0]-altsols[j].lparams[0]) lt brav[1] and $
                      abs(altsols[i].lparams[3]-altsols[j].lparams[3]) lt brav[1] and $
                      abs(altsols[i].lparams[1]-altsols[j].lparams[1]) lt brav[1] and $
                      abs(altsols[i].lparams[4]-altsols[j].lparams[2]) lt brav[1] and $
                      abs(altsols[i].lparams[2]-altsols[j].lparams[2]) lt brav[1] and $
                      abs(altsols[i].lparams[5]-altsols[j].lparams[5]) lt brav[1]) then goto, next
                end
                else:
                endcase
              endif ; for j
              if redund eq 0 then $
              begin
                   altsolno1=altsolno1+1
                   altsols1[altsolno1]=altsols[i]
              endif
         next:
         endif ; for i
         endif else $
         begin
            altsolno1=0
            altsols1[1]=altsols[0]
            altsols1[1].UT=[[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]]
            altsols1[1].lparams=lp_from_ub(altsols1[1].UT # altsols1[1].UB)
         endelse

         fin:


        Return, crystal_system(cryst.cell_parameters, 0.3, 0.1)

end

;=============================================================================


;lattice_params_from_UB,  UB, Tcrystal

;----------------------------------------------------------
