

;Program to refine cell parameters from a list of single-crystal diffraction data
;  input is the symmetry, and then an n-dimensional array of hkl values with
;     an associated array of cartesian diffractometer space coordinates
;     input through a file: cellrefinement.dat with n+1 lines, h x free formatted,
;     with spaces between
;e.g.
;monoclinic2
;    7      -1       3    0.0212225    -0.956911  -0.00410196
;    6      -1       1     0.182962    -0.661939   0.00510531
;   12      -2       2     0.366124     -1.32461    0.0102162
;    6      -1       2    0.0803503    -0.763056    0.0112133

;  Written by Bob Downs, Dec 1, 2005

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

pro  CellRefinement

    htemp=fltarr(3)
    xtemp=fltarr(3)
    angl=fltarr(4)
    ub=fltarr(3,3)
    cell=fltarr(6)
    symmetry='           '

    datafile='C:\Documents and Settings\Przemek Dera\Desktop\CellRefinement\Debug\cellrefinement.dat'
    outfile='C:\Documents and Settings\Przemek Dera\Desktop\CellRefinement\Debug\cellrefinement1.out'

    free_lun, 2
    free_lun, 3
    openr, 2 , datafile
    openw, 3 , outfile
    readf, 2, Symmetry

;  determine symmetry constraints
    n=0
    while not eof(2) do $
    begin
      n=n+1
      readf, 2 , htemp, xtemp
    endwhile

    if n lt 7 then $
    begin
     print, 'Not enough data'
     printf, 3, 'Not enough data'
     return
    endif
    close, 2
    free_lun, 2
    openr, 2 , datafile
    readf, 2, Symmetry
    h=fltarr(3,n)
    x=fltarr(3,n)
    for i=0, n-1 do $
    begin
      readf, 2 , htemp, xtemp
      h[0:2,i]=htemp
      x[0:2,i]=xtemp
    endfor
    close, 2
    unconstrainedlsq, n,h,x,UB,cell
    ;if (Symmetry ne 'triclinic') then constrainedlsq, n,h,x,Symmetry,UB,cell
    close, 3
    print, Symmetry
    print, n
end

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*

function unconstrainedlsq, optable1, Tcrystal
;n,h,x,UB,cell

;  this routine computes the unconstrained cell parameters
;  UB h = x
;  h is 3xn
;  UB = x h^t (h h^t)^-1

COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak


n=optable1->peakno()
cell=Tcrystal.cell_parameters
UB=Tcrystal.UB_matrix
h1=intarr(3,n)
x1=fltarr(3,n)
k=0
for i=0, n-1 do $
begin
  ref_peak=optable1->get_element(i)
  if not (ref_peak.hkl[0] eq 0 and ref_peak.hkl[1] eq 0 and ref_peak.hkl[2] eq 0) then $
  begin
    for j=0, 2 do $
    begin
      h1[j,k]=ref_peak.hkl[j]
      x1[j,k]=ref_peak.xyz[j]
    endfor
    k=k+1
  endif
endfor

if k gt 6 then $
begin

h=intarr(3,k)
x=fltarr(3,k)
n=k
h=h1[0:2,0:k-1]
x=x1[0:2,0:k-1]

hht=fltarr(3,3)
xht=fltarr(3,3)
hhti=fltarr(3,3)
UB=fltarr(3,3)
GI=fltarr(3,3)
G=fltarr(3,3)
z2=fltarr(3,3)

da=fltarr(6)
db=fltarr(6)
z3=fltarr(3,3)
z33=fltarr(3,3)
cell=fltarr(6)
sigcell=fltarr(6)
verr=fltarr(3)
xc=fltarr(3)

rad=180.0/acos(-1.0)
hht=h # transpose(h)
xht=x # transpose(h)

hhtI = invert(hht)

IF(determ(hhti, /check) LT 1.0E-15)then $
begin
    print, 'Cannot invert matrix'
    return, ''
endif
UB=xht # hhtI

print, 'Orientation Matrix:'
print, UB
GI=transpose(UB) # UB

G=invert(gi)

IF(determ(g) LT 1.0E-15) then  $
begin
    print, 'Cannot invert gi'
    return, ''
endif
vol2=1/determ(g)
vol=1.0/sqrt(vol2)
CELL[0]=sqrt(g[0,0])
cell[1]=sqrt(g[1,1])
cell[2]=sqrt(g[2,2])
cell[3]=acos(g[1,2]/(cell[1]*cell[2]))*rad ; in deg
cell[4]=acos(g[0,2]/(cell[0]*cell[2]))*rad
cell[5]=acos(g[0,1]/(cell[0]*cell[1]))*rad
st=''
st='Unconstrained cell parameters:'
st1=[string(cell[0])+string(cell[1])+string(cell[2]),string(cell[3])+string(cell[4])+string(cell[5])+string(vol)]
st=[st,st1]

;  compute errors
;  get sum of errors-squared in vector coordinates

verr=fltarr(3)
for  i=0,n-1 do $
begin
   xc=UB # h[0:2,i]
   verr[0]=verr[0]+(xc[0]-x[0,i]) ^ 2
   verr[1]=verr[1]+(xc[1]-x[1,i]) ^ 2
   verr[2]=verr[2]+(xc[2]-x[2,i]) ^ 2
endfor
;  z2 are small shifts
for  i=0,2 do $
begin
   for  j=0,2 do $
   begin
      z2[j,i]=sqrt(hhti[i,i]/(n-3)*verr[j])
   endfor
endfor

;db=0.0
p1=0.0
for i=0,2 do $
begin
   for j=0,2 do $
   begin
      ub[j,i]=ub[j,i]+z2[j,i]       ;add a bit to an element of UB
      z3=transpose(UB) # UB        ;new value of G* in Z3
      z33=invert(z3)
      det=determ(z33, /check)

;     calculate direct space volume and lattice constants

      dvol=sqrt(det)
      for k=0,2 do da[k]=sqrt(z33[k,k])
      da[3]=acos(z33[2,1]/(da[2]*da[1]))*rad
      da[4]=acos(z33[2,0]/(da[2]*da[0]))*rad
      da[5]=acos(z33[0,1]/(da[0]*da[1]))*rad
      for k=0,5 do db[k]=db[k]+(da[k]-cell[k]) ^ 2
      p1=p1+(dvol-vol) ^ 2

;     restore UB element

      ub[j,i]=ub[j,i]-z2[j,i]
   endfor
endfor
for i=0,5 do $
begin
   db[i]=sqrt(db[i])
   sigcell[i]=db[i]
endfor
for i=3,5 do sigcell[i]=sin(cell[i]/rad)*sigcell[i]/rad
p1=sqrt(p1)
st=[st,'Estimated errors: ']
st1=[string(db[0])+string(db[1])+string(db[2]),string(db[3])+string(db[4])+string(db[5])+string(p1)]
st=[st, st1]

 st31=string(Tcrystal.UB_matrix[0,0])+string(Tcrystal.UB_matrix[0,1])+string(Tcrystal.UB_matrix[0,2])
 st32=string(Tcrystal.UB_matrix[1,0])+string(Tcrystal.UB_matrix[1,1])+string(Tcrystal.UB_matrix[1,2])
 st33=string(Tcrystal.UB_matrix[2,0])+string(Tcrystal.UB_matrix[2,1])+string(Tcrystal.UB_matrix[2,2])
 st2=[st,'Orientation matrix: ', st31, st32, st33]
endif else  st2=''

return, st2

end

;------------------------------------------------

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*
pro CALCH, ANGL, THPHI

;     CALCULATE VECTOR COMPONENTS FROM ANGLES - NOTE: OMEGA MUST BE DEVIATION
;     FROM BISECTING POSITION

   parity=fltarr(4)
   parity=[1.0,1.0,1.0,1.0]
   waveL=0.70927
   rad=180.0/acos(-1.0)

;     GET SINES AND COSINES OF EULERIAN ANGLES

   SINO=SIN(angl[1]/RAD)*PARITY[1]
   SINX=SIN(ANGL[2]/RAD)*PARITY[2]
   SINP=SIN(ANGL[3]/RAD)*PARITY[3]
   COSO=COS(angl[1]/RAD)
   COSX=COS(ANGL[2]/RAD)

   COSP=COS(ANGL[3]/RAD)
   SINT=SIN(ANGL[0]*0.5/RAD)    ;CALCULATE SIN(THETA)
;     FORM TERMS OF H-PHI
   THPHI[0]=2.0*SINT*(COSO*COSX*COSP-SINO*SINP)/WAVEL
   THPHI[1]=2.0*SINT*(COSO*COSX*SINP+SINO*COSP)/WAVEL
   THPHI[2]=2.0*SINT*(COSO*SINX)/WAVEL

   END

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*


;  routine is based on Schoemaker and Bassi (1970), Acta Cryst A26, 97-101
;  modified after Finger and Ralph (1981)
pro constrainedlsq, n,h,x,Symmetry,UB,cell

rcell=fltarr(6)
h=fltarr(3,n)
x=fltarr(3,n)
B=fltarr(3,3)
BI=fltarr(3,3)
U=fltarr(3,3)
a=fltarr(3,3)
yb=fltarr(3,3)
xa=fltarr(15,16)
xb=fltarr(12)
a2=fltarr(6)
V0=fltarr(3,3)
V0I=fltarr(3,3)
d=fltarr(3,3)
a1=fltarr(9)
dcov=fltarr(9,9)
tmp1=fltarr(3)
tmp2=fltarr(3)
aa1=fltarr(6)
as1=fltarr(6)
derivs=fltarr(7,9)
atemp=fltarr(7,9)
sigs=fltarr(7)

text=''
sym=['monoclinic2','monoclinic1','orthorhombic','tetragonal','hexagonal','cubic']
rad=180.0/acos(-1.0)
ffact=.0005

a=h # transpose(h)

xa[1:3,1:3]=a[1:3,1:3]
xa[4:6,4:6]=a[1:3,1:3]
xa[7:9,7:9]=a[1:3,1:3]

;  constrained least squares refinement of cell edges
print, ' Constrained cell parameters'
print, ' Symmetry constraint is: ',symmetry

PARAMS, UB,a1,vol1,a2,vol2
BCALC, a1,a2,B ; compute B-matrix, B[h]_D* = [h]_C

BI=invert(B)
det=determ(BI)

U = UB # BI

;  a2 represents the reciprocal cell parameters and cosines of the angles,
;     but has a zero value if the particular cell parameter is to be constrained

case Symmetry of

'monoclinic2': $    ;     MONOCLINIC - B UNIQUE
begin
   a2[3]=0.0
   A2[5]=0.0
end
'monoclinic1': $    ;     MONOCLINIC - C UNIQUE
begin
   a2[3]=0.0
   A2[4]=0.0
end
'orthorhombic':  $
begin
   a2[3]=0.0
   A2[4]=0.0
   A2[5]=0.0
end
'tetragonal':$
begin
   A2[0]=SQRT(A2[0]*A2[1])
   A2[1]=A2[0]
   a2[3]=0.0
   A2[4]=0.0
   A2[5]=0.0
end
'hexagonal':$
begin
   A2[0]=SQRT(A2[0]*A2[1])
   A2[1]=A2[0]
   a2[3]=0.0
   A2[4]=0.0
   A2[5]=0.5
end
'cubic': $
begin
   A2[0]=(A2[0]*A2[1]*A2[2])^(1.0/3.0)
   A2[1]=A2[0]
   A2[2]=A2[0]
   a2[3]=0.0
   A2[4]=0.0
   A2[5]=0.0
end
else : $
begin
   print, 'Symmetry character string is not recognized'
   return
end
endcase

;  CONSTRUCT CONSTRAINED B
;  first compute direct cell from reciprocal cell

 RECIP, A2,VOL2,A1,VOL1   ; A2, vol2 are reciprocal space, A1, Vol1 are direct space
 BCALC, A1,A2,B ; compute B-matrix from constrained cell parameters

;  CONSTRUCT CONSTRAINED ORIENTATION MATRIX = V0

V0 = U # B

;  INVERT V0

V0I = invert(V0)
det=determ(V0I)

if(abs(det) lt 0.00001) then $
begin
   print, 'Normal Equations Matrix is Singular.'
   return
endif

;  CALCULATE "OBSERVED" INDICES HH

hh=fltarr(3,n)
hh=V0I # x

;  SET UP EXPANDED VECTOR XB

for K=0,N-1 do $
begin
   for I=0,2 do $
   begin
      XB[I]=XB[I]+H[I,K]*HH[0,K]
      XB[I+3]=XB[I+3]+H[I,K]*HH[1,K]
      XB[I+6]=XB[I+6]+H[I,K]*HH[2,K]
   endfor
endfor

;  TREAT CONSTRAINED PROBLEM

CONSTR, Symmetry,IERR,NPARM,a2,xa,xb
IF(ierr ne 0) then $
begin
   Print, 'Normal Equations Matrix is Singular.'
   return
endif
for I=0,2 do $
begin
   D[0,I]=XB[I]
   D[1,I]=XB[I+3]
   D[2,I]=XB[I+6]
endfor

;  EXTRACT ORIENTATION MATRIXUB AND CELL PARAMETERS FROM D

UB=v0 # d
PARAMS, UB,a1,vol1,a2,vol2

;  BACK CALCULATE "OBSERVED" INDICES AND EVALUATE VARIANCE-COVARIANCE MATRIX
;  DCOV FOR D

FACT=0.0
for K=0,n-1 do $
begin
   for I=0,2 do $
   begin
      TMP1[I]=0.0
      TMP2[I]=-HH[I,K]
      for JJ=0,2 do $
      begin
         TMP1[I]=TMP1[I]+UB[I,JJ]*H[JJ,K]
         TMP2[I]=TMP2[I]+D[I,JJ]*H[JJ,K]
      endfor
   endfor
   TEMP=0.0
   for JJ=0,2 do $
   begin
      TMP1[JJ]=x[JJ,K]-TMP1[JJ]
      TEMP=TEMP+TMP1[JJ]^2
   endfor
   FACT=FACT+TMP2[1]^2+TMP2[2]^2+TMP2[3]^2
   TEMP=SQRT(TEMP)
endfor
TEMP=FACT
FACT=FACT/(3*N-NPARM)
for J=0,8  do $
begin
   for I=0,8 do $
   begin
      DCOV[I,J]=FACT*XA[I,J]
   endfor
endfor

print,  A1, VOL1

;  PERTURB D ONE ELEMENT AT A TIME, PROPAGATE RESULTS TO ORIENTATION MATRIX
;     V, AND ACCUMULATE PARTIAL DERIVATIVES FOR EACH D.L. PARAMETER IN MATRIX DERIVS
aa1[0:5]=a1[0:5]
VOLL1=VOL1
for J=0,2 do $
begin
   for I=0,2 do $
   begin
      D[I,J]=D[I,J]+FFACT
      ub=V0 # D
      PARAMS, UB,a1,vol1,a2,vol2
      as1=0.0
      L=J+3*(I-1)
      for K=0,5 do DERIVS[K,L]=(A1[K]-AA1[K])/FFACT
      DERIVS[7,L]=(VOL1-VOLL1)/FFACT
      D[I,J]=D[I,J]-FFACT
   endfor
endfor

;  CALCULATE AND PRINT STANDARD DEVIATIONS

atemp= derivs # dcov
for K=0,6 do $
begin
   for I=0,6 do $
   begin
      DCOV[K,I]=0.0
      for J=0,8 do DCOV[K,I]=DCOV[K,I]+ATEMP[K,J]*DERIVS[I,J]
   endfor
endfor
;  CLEAR NON-REFINED parts of DCOV
dcov[3,3]=0.0
IF(Symmetry eq 'monoclinic1')then $
   dcov[4,4]=0.0 $
else if(Symmetry eq 'monoclinic2')then $
   dcov[5,5]=0.0 $
else $
begin
   dcov[3,3]=0.0
   dcov[5,5]=0.0
endelse
for K=0,6 do $
begin
   FACT=max(DCOV[K,K],1.0E-24)
   SIGS[K]=SQRT(FACT)
endfor
for K=3,5 do SIGS[K]=RAD*SIGS[K]*SQRT(1.0-AA1[K]^2)   ; convert to degrees
print, sigs

end

;*********************************************************************




; CALCULATES MATRIX B FROM direct AND RECIPROCAL cell parameters,
;  where B[h]_D* = [h]_C

pro BCALC, a1,a2,B

B[0,0]= a2[0]
B[0,1]= a2[1]*a2[5]
B[0,2]= a2[2]*a2[4]
B[1,0]= 0.0
B[1,1]= a2[1]*SQRT(1.0-a2[5]*a2[5])
B[1,2]=-a2[2]*SQRT(1.0-a2[4]*a2[4])*a1[3]
B[2,0]= 0.0
B[2,1]= 0.0
B[2,2]= 1.0/a1[2]
RETURN
END

;*********************************************************************

pro RECIP, A2,VOL2,A1,VOL1

;     ROUTINE TO ACCEPT CELL PARAMETERS AND COSINES IN A2, CALCULATE VOLUME
;        THEN CALCULATE RECIPROCAL CELL AND VOLUME

SINES=fltarr(3)

VOL2=1.0-A2[3]*A2[3]-A2[4]*A2[4]-A2[5]*A2[5]+2.*A2[3]*A2[4]*A2[5]
VOL2=A2[0]*A2[1]*A2[2]*SQRT(VOL2)
;     WORK DIRECT LATTICE FROM RECIPROCAL LATTICE
VOL1=1./VOL2
for I=0,2 do $
   SINES[I]=SQRT(1.0-A2[I+3]*A2[I+3])
A1[0]=A2[1]*A2[2]*SINES[0]/VOL2
A1[1]=A2[2]*A2[0]*SINES[1]/VOL2
A1[2]=A2[0]*A2[1]*SINES[2]/VOL2
A1[3]=(A2[4]*A2[4]-A2[3])/(SINES[1]*SINES[2])
A1[4]=(A2[5]*A2[3]-A2[4])/(SINES[2]*SINES[0])
A1[5]=(A2[3]*A2[4]-A2[5])/(SINES[0]*SINES[1])
RETURN
END


;*************************************************************************

pro CONSTR, symmetry,IERR,NPARM,a2,xa,xb

;     SUBROUTINE TO APPLY CONSTRAINTS FOR CRYSTAL symmetry TO NORMAL
;     EQUATIONS MATRIX XA, SOLVE SYSTEM AND RETURN XA INVERSE IN
;     XA, SOLUTION IN XB. METHOD OF LAGRANGIAN MULTIPLIERS USED
;     FOR CONSTRAINTS. IERR = 0 IF NO ERROR, NON-ZERO MEANS ERROR.

NCNST=intarr(6)
CNSTYP=intarr(5,6)
NCOEF=intarr(10)

ROWTP=intarr(3,10)
COEFTP=intarr(3,10)
NTEMP=intarr(14)

COEF=fltarr(15)
xa=fltarr(15,16)
xb=fltarr(12)
a2=fltarr(6)

CNSTYP=[[9,10,0,0,0],[7,8,0,0,0],[1,2,3,0,0],[4,1,2,3,0],[6,1,7,8,0],[4,5,1,2,3]]
NCOEF=[2,2,2,2,2,3,3,3,3,3]
ROWTP=[[2,4,0],[3,7,0],[6,8,0],[1,5,0],[1,9,0],[1,4,5],[7,3,6],[7,8,6],[6,8,2],[6,4,2]]
COEFTP=[[1,2,15],[3,4,15],[5,6,15],[7,8,15],[7,8,15],[7,7,8],[4,3,9],[10,6,11],[5,6,12],[13,2,14]]
;     EVALUATE MULTIPLIER COEFFICIENTS
COEF[1]=A2[1]/A2[2]
COEF[2]=A2[2]/A2[1]
COEF[3]=A2[1]/A2[3]
COEF[4]=A2[3]/A2[1]
COEF[5]=A2[2]/A2[3]
COEF[6]=A2[3]/A2[2]
COEF[7]=1.0
COEF[8]=-1.0
COEF[9]=A2[2]*A2[6]/A2[3]
COEF[10]=-A2[3]*A2[6]/A2[1]
COEF[11]=A2[2]*(1.0-A2[6]*A2[6])/A2[3]
COEF[12]=A2[1]*A2[5]/A2[2]
COEF[13]=-A2[2]*A2[5]/A2[3]
COEF[14]=A2[1]*(1.0-A2[5]*A2[5])/A2[2]
COEF[15]=0.0

;  CLEAR CONSTRAINT PORTION OF MATRIX XA
XA(1:9,10:16)=0.0
xa(10:15,1:16)=0.0
;  GET NUMBER OF CONSTRAINT EQUATIONS = neq
case Symmetry of
'monoclinic2':$      ;     MONOCLINIC - B UNIQUE
begin
   neq=2
   CRYSTP=1
end
'monoclinic1':$  ;     MONOCLINIC - C UNIQUE
begin
   neq=2
   CRYSTP=2
end
'orthorhombic':$
begin
   neq=3
   CRYSTP=3
end
'tetragonal':$
begin
   neq=4
   CRYSTP=4
end
'hexagonal':$
begin
   neq=4
   CRYSTP=5
end
'cubic':$
begin
   neq=5
   CRYSTP=6
end
else:$
begin
   print, 'Symmetry character string is not recognized'
   return
end
endcase

NPARM=9-NEQ
;  LOOP THROUGH CONSTRAINTS
for I=0,NEQ-1 do $
begin
;  GET CONSTRAINT TYPE AND NUMBER OF COEFFICIENTS
   ITYP=CNSTYP[I,CRYSTP]
   NUMB=NCOEF[ITYP]
;  LOOP THROUGH TERMS IN CONSTRAINT EQUATION
   for J=0,NUMB-1 do $
   begin
;     GET ROW AND COLUMN OF TERM TO MODIFY AND COEFFICIENT OF MODIFICATION
      IROW=ROWTP[J,ITYP]
      NCF=COEFTP[J,ITYP]
      XA[IROW,9+I]=COEF[NCF]
      XA[9+I,IROW]=COEF[NCF]
   endfor
endfor
NRANK=9+NEQ
;  COPY VECTOR TO AUGMENTED POSITION, INVERT MATRIX AND SOLVE EQUATIONS
XA[1:9,NRANK+1]=XB[1:9]
MXLNEQ, XA,NRANK,15,DET,JRANK,1.0E-6,NTEMP,1
IF(JRANK NE NRANK)then $
begin
   print, 'Matrix is singular'
   return
endif
xb[1:9]=xa[1:9,nrank+1]
ierr=0
RETURN
END

