;------------- List of methods of the peak class


; function CLASS_peak::get_object
;        outputs the properties of self
; function CLASS_peak::distance, ref_peak
;        calculates a dinstance in pixels between self and ref_peak
; pro CLASS_peak::put_onepeak3_into_peak, onepeak3
; pro CLASS_peak::set_object, ref_peak
;        modifies self according to ref_peak


;------------- List of methods of the peaktable clas

; function CLASS_peaktable::calculate_Ddd, lp
; function CLASS_peaktable::indexing_FOM, UB, pr
; function CLASS_peaktable::recomp_UB
; function CLASS_peaktable::build_HKLs
; function CLASS_peaktable::build_XYZs
; function CLASS_peaktable::indexing_FOM, UB, pr
; function CLASS_peaktable::save_ascii
; function CLASS_peaktable::save_ascii1, path, fn
; function CLASS_peaktable::save_unitcell
; function CLASS_peaktable::save_p4p, fname
; function CLASS_peaktable::get_object
; function CLASS_peaktable::get_element, n
; function CLASS_peaktable::find_closest_peak, ref_peak
; function CLASS_peaktable::aver_ang_error_mono, ub
;           this one looks suspicious
; function CLASS_peaktable::reindex, ocrystal, limit
; function CLASS_peaktable::BUILD_d_list
; function CLASS_peaktable::peak_list, ub
; function CLASS_peaktable::peakno
; function CLASS_peaktable::selectedno

; pro CLASS_peaktable::calculate_all_edd_from_xyz, th
; pro CLASS_peaktable::import_p4p, fname
; pro CLASS_peaktable::import_ASCII, fname
; pro CLASS_peaktable::import_UNI, fname
; pro CLASS_peaktable::copy, opt
; pro CLASS_peaktable::exchange, opt
; pro CLASS_peaktable::index, UB
; pro CLASS_peaktable::calculate_all_xyz_fromhkl, UB
; pro CLASS_peaktable::check_profiles
; pro CLASS_peaktable::select_rpeaks, box00x,box00y, box11x, box11y
; pro CLASS_peaktable::select_rpeaks_det, box00x,box00y, box11x, box11y
; pro CLASS_peaktable::unselect_rpeaks, box00x,box00y, box11x, box11y
; pro CLASS_peaktable::invert_selection
; pro CLASS_peaktable::unselect_all
; pro CLASS_peaktable::select_all
; pro CLASS_peaktable::delete_selected
; pro CLASS_peaktable::move_selected, opt
; pro CLASS_peaktable::set_object,ref_peaktable
; pro CLASS_peaktable::set_element, n, ref_peak
; pro CLASS_peaktable::replacepeak, n, ref_peak
; pro CLASS_peaktable::appendpeak, ref_peak
; pro CLASS_peaktable::select_peak, k
; pro CLASS_peaktable::select_peaks, k
; pro CLASS_peaktable::unselect_peak, k
; pro CLASS_peaktable::delete_peak, sel
; pro CLASS_peaktable::insert_peak, sel, ref_peak
; pro CLASS_peaktable::initialize
; pro CLASS_peaktable::rotate_vectors, ran
; pro CLASS_peaktable::read_object_from_file, fname, sc
;                           sc is intensity scaling factor
; pro CLASS_peaktable::APPEND_object_from_file, fname
; pro CLASS_peaktable::write_object_to_file, fname
; pro CLASS_peaktable::calculate_all_xyz_from_pix, oadetector
; pro CLASS_peaktable::apply_rotation, angles
; pro CLASS_peaktable::select_indexable, ub, limit
; pro CLASS_peaktable::select_indexable_lp, lp, limit
; pro CLASS_peaktable::refine_omega, ub
; pro CLASS_peaktable::apply_transform, UT



;==============================================================
;        Peak class methods
;==============================================================

function CLASS_peak::get_object

COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

                ref_peak.Stat     = self.Stat
                ref_peak.HKL      = self.HKL
                ref_peak.XYZ      = self.XYZ
                ref_peak.selected = self.selected
                ref_peak.DetXY    = self.DetXY
                ref_peak.Gonio    = self.Gonio
                ref_peak.GonioSS  = self.GonioSS
                ref_peak.nen      = self.nen
                ref_peak.energies = self.energies
                ref_peak.IntAD    = self.IntAD
                ref_peak.position = self.position
                ref_peak.IntSSD   = self.IntSSD
                ref_peak.adp      = self.adp

                return, ref_peak

end

;--------------------------------------------------

pro CLASS_peak::set_object, ref_peak

                self.Stat     = ref_peak.Stat
                self.HKL      = ref_peak.HKL
                self.XYZ      = ref_peak.XYZ
                self.selected = ref_peak.selected
                self.DetXY    = ref_peak.DetXY
                self.Gonio    = ref_peak.Gonio
                self.GonioSS  = ref_peak.GonioSS
                self.nen      = ref_peak.nen
                self.energies = ref_peak.energies
                self.IntAD    = ref_peak.IntAD
                self.position = ref_peak.position
                self.IntSSD   = ref_peak.IntSSD
                self.adp      = ref_peak.adp

end

;--------------------------------------------------

function CLASS_peak::distance, ref_peak

 x1=self.DetXY[0]
 y1=self.DetXY[1]
 x2=ref_peak.DetXY[0]
 y2=ref_peak.DetXY[1]

 return, sqrt((x1-x2)^2+(y1-y2)^2)

end

;----------------------------------------------------

function CLASS_peaktable::calculate_misindex, ocrystal, limit, i
  cry=ocrystal->get_object()
  UB=cry.UB_matrix
  iUB=invert(UB)
  vec1=iUB##self.peaks[i].XYZ
  if self.peaks[i].selected[0] eq 0 then return, vlength(vec1-self.peaks[i].HKL) else return, -1.0
end

;----------------------------------------------------

function CLASS_peaktable::calculate_indexing_fom,  ocrystal, limit

  cry=ocrystal->get_object()
  UB=cry.UB_matrix
  iUB=invert(UB)

  fom=0.0
  count=0

  for i=0, self.peakno-1 do $
     if self.peaks[i].selected[0] eq 0 then $
          begin
           fom=fom+self->calculate_misindex(ocrystal, limit, i)
           count=count+1
          end
  ;print, 'fom= ', fom/count
  if count gt 0 then return, fom/count $
     else return, -1
end
;----------------------------------------------------

pro CLASS_peak::put_onepeak3_into_peak, onepeak3

                self.Stat     = onepeak3.Stat
                self.HKL      = onepeak3.HKL
                self.XYZ      = onepeak3.XYZ
                self.selected = onepeak3.selected
                self.DetXY    = onepeak3.DetXY
                self.Gonio    = onepeak3.Gonio
                self.GonioSS  = onepeak3.GonioSS
                self.nen      = onepeak3.nen
                self.energies = onepeak3.energies
                self.IntAD    = onepeak3.IntAD
                self.position = onepeak3.position
                self.IntSSD   = onepeak3.IntSSD

end


;==============================================================
;        Peaktable class methods
;==============================================================


function CLASS_peaktable::calculate_Ddd, lp
; calculates difference between observed (from xyz) and calculated (from lp) d-spacings
       N=self.peakno
       ds=fltarr(N)
       dsc=fltarr(N)
       a=fltarr(N)
       for i=0, n-1 do $
       begin
        ds[i]=1.0/vlength(self.peaks[i].xyz)
        dsc[i]= d_from_lp_and_hkl(lp, self.peaks[i].hkl)
       endfor
       return,(ds-dsc)/dsc
end


function CLASS_peaktable::calculate_Ds
; calculates difference between observed (from xyz) and calculated (from lp) d-spacings
       N=self.peakno
       ds=fltarr(N)
       dsc=fltarr(N)
       a=fltarr(N)
       for i=0, n-1 do $
       begin
        ds[i]=1.0/vlength(self.peaks[i].xyz)
       endfor
       return,ds
end

;-------------------------------------------------

pro CLASS_peaktable::import_p4p, fname
COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

; returns peak number
res=file_info(fname)
xyz=fltarr(3)
if res.exists eq 1 then $
begin
 FREE_LUN,2
 OPENR, 2, fname
 str='     '
 ; search for beginning of reflection block
 while str ne 'MOSA' and not eof(2) do $
   readf, 2, str, format='(A4)'
 readf, 2, str
 if strmid(str,0,5) eq 'REF1K' then $
 begin
  x=float(strmid(str,86,10))
  y=float(strmid(str,96,10))
  z=float(strmid(str,106,10))
  xyz=[x,y,z]
  ref_peak.xyz=xyz
  self->appendpeak,ref_peak
  while strmid(str,0,5) eq 'REF1K' and not eof(2) do $
  begin
    readf, 2, str
    x=float(strmid(str,86,10))
    y=float(strmid(str,96,10))
    z=float(strmid(str,106,10))
    xyz=[x,y,z]
    ref_peak.xyz=xyz
    self->appendpeak,ref_peak
  endwhile
 endif
 CLOSE, 2
 FREE_LUN,2
endif
end

;--------------------------------------------------------------------

;-------------------------------------------------

pro CLASS_peaktable::import_DetXYom, fname
COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

; returns peak number
res=file_info(fname)
xyz=fltarr(3)
if res.exists eq 1 then $
begin
 FREE_LUN,2
 OPENR, 2, fname
 a1=0.0
 a2=0.0
 a3=0.0
 i=0
 while not eof(2) do $
 begin
    readf, 2, a1,a2,a3
    ref_peak.detXY[*]=[a2,a3]
    ref_peak.gonio[3]=-a1
    self->appendpeak,ref_peak
    i=i+1
  endwhile
 endif
 print, i
 CLOSE, 2
 FREE_LUN,2

end

;--------------------------------------------------------------------


pro CLASS_peaktable::import_ASCII, fname
COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

; returns peak number
res=file_info(fname)
x=0.0
y=0.0
z=0.0
if res.exists eq 1 then $
begin
 FREE_LUN,2
 OPENR, 2, fname
 while not eof(2) do $
  begin
    readf, 2, x,y,z
    ref_peak.xyz=[float(x), float(y),float(z)]
    self->appendpeak,ref_peak
  endwhile
 CLOSE, 2
 FREE_LUN,2
endif
end

;--------------------------------------------------------------------

pro CLASS_peaktable::import_UNI, fname
COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

; returns peak number
res=file_info(fname)
a=''
b=0L
x=0.0
y=0.0
z=0.0
en=0.0
if res.exists eq 1 then $
begin
 FREE_LUN,2
 OPENR, 2, fname
 readf, 2, a
 while not eof(2) do $
  begin
    readf, 2, b,b,b,b, x,y,z, en
    ref_peak.xyz=[float(x), float(y),float(z)]
    ref_peak.energies[0]=float(en)
    self->appendpeak,ref_peak
  endwhile
 CLOSE, 2
 FREE_LUN,2
endif
end

;------------------------------------------------

pro CLASS_peaktable::copy, opt
;copy self to target
  pt=self->get_object()
  opt->set_object, pt

end
;-------------------------------------------------

pro CLASS_peaktable::exchange, opt

  pt1=self->get_object()
  pt2=opt->get_object()
  self->set_object, pt2
  opt->set_object, pt1

end
;-------------------------------------------------

function CLASS_peaktable::indexing_FOM, UB, pr
  ;UB=self->recomp_UB()
  fom=0.0
  iUB=invert(UB)
  no=0
  for i=1, self.peakno do $
  begin
    hkl=iUB ## self.peaks[i-1].xyz
    hkl1=[round(hkl[0]),round(hkl[1]),round(hkl[2])]
    self.peaks[i-1].hkl=hkl1
    fo=vlength(hkl1-hkl)
    ;print, fo
    if fo lt 0.1 then $
    begin
      fom=fom+fo
      no=no+1
    end ;else print, i
  end
  return, fom/no
end


;---------------
function CLASS_peaktable::recomp_UB
  HKLs=self->build_HKLs()
  XYZs=self->build_XYZs()
  UB=transpose(XYZs # transpose(HKLs) # invert(HKLs # transpose(HKLS)))
  return, UB
end


;--------------
function CLASS_peaktable::build_HKLs
HKLs=fltarr(3,self.peakno)
  for i=1, self.peakno do $
   HKLs[0:2,i-1]=self.peaks[i-1].hkl
   return,HKLs
end
;----------
function CLASS_peaktable::build_XYZs
XYZs=fltarr(3,self.peakno)
  for i=1, self.peakno do $
   XYZs[0:2,i-1]=self.peaks[i-1].xyz
   return, XYZs
end
;-------------------------------------------------


pro CLASS_peaktable::index, UB
; Claculates Miller indices of all reflections in the pt from UB matrix
; and xyzs
  for i=1, self.peakno do $
  begin
    hkl=hkl_from_ub_and_xyz(ub, self.peaks[i-1].xyz)
    self.peaks[i-1].hkl=[round(hkl[0]),round(hkl[1]),round(hkl[2])]
  end
end

;-------------------------------------------------


pro CLASS_peaktable::calculate_all_xyz_fromhkl, UB
  ;iUB=invert(UB)
  ;print, '--------------------------'
  for i=1, self.peakno do $
  begin
    self.peaks[i-1].xyz=UB ## self.peaks[i-1].hkl
    ;self.peaks[i-1].hkl=[round(hkl[0]),round(hkl[1]),round(hkl[2])]
    ;print,i, transpose(hkl)
  end
;print, '--------------------------'

end

;-------------------------------------------------
function CLASS_peaktable::save_ascii

          fname=dialog_pickfile(FILTER='*.txt', /WRITE)
          free_lun, 4
          if fname ne '' then $
          begin
        ;  pn=self.peakno
        ;  m=max(self.peaks[0:pn-1].intAD[0])
        ;  self.peaks[0:pn-1].intAD[0]=self.peaks[0:pn-1].intAD[0]*9999./m
        ;  self.peaks[0:pn-1].intAD[1]=self.peaks[0:pn-1].intAD[1]*9999./m
          OPENW, 4, fname
          for i=1, self.peakno do $
          begin
           if not(self.peaks[i-1].hkl[0] eq 0 and self.peaks[i-1].hkl[1] eq 0 and self.peaks[i-1].hkl[2] eq 0) then $
           begin
            st = string(self.peaks[i-1].hkl[0], format='(I4)')+$
                 string(self.peaks[i-1].hkl[1], format='(I4)')+$
                 string(self.peaks[i-1].hkl[2], format='(I4)')+$
                 string(self.peaks[i-1].intAD[0], format='(F8.2)')+$
                 string(self.peaks[i-1].intAD[1], format='(F8.2)')
            PRINTF, 4, st
           end
          endfor
          CLOSE, 4
          free_lun, 4
          endif
          return, fname
end
;-------------------------------------------------
function CLASS_peaktable::save_ascii1, path, fn
COMMON Indices, HKLs, UB




		lp=lp_from_ub(ub)
          if n_params() le 1 then fname=dialog_pickfile(FILTER='*.hkl', /WRITE, DEFAULT_EXTENSION='hkl', path=path) else fname=fn
          free_lun, 4
          if fname ne '' then $
          begin
          m=max(self.peaks[0:self.peakno-1].intAD[0])
          OPENW, 4, fname
          for i=1, self.peakno do $
          begin

          hkl1 = self.peaks[i-1].hkl
            st = string(self.peaks[i-1].hkl[0], format='(I4)')+$
                 string(self.peaks[i-1].hkl[1], format='(I4)')+$
                 string(self.peaks[i-1].hkl[2], format='(I4)')+$
                 string((self.peaks[i-1].intAD[0])*9999./m, format='(F8.2)')+$
                 string((self.peaks[i-1].intAD[1])*9999./m, format='(F8.2)')


            PRINTF, 4, st
;            PRINTF, 4, st2
;            PRINTF, 4, st3
          endfor
          CLOSE, 4
          free_lun, 4
          endif
          return, fname
end
;-------------------------------------------------
function CLASS_peaktable::save_ascii, path, fn
COMMON Indices, HKLs, UB


         T21=[[   0.500,   -1.500,    0.000],$
  			  [  -0.500,   -0.500,    0.000],$
			  [   0.000,    0.000,   -1.000]]

         T31=[[  -0.500,   -1.500,    0.000],$
  			  [  -0.500,    0.500,    0.000],$
			  [   0.000,    0.000,   -1.000]]

		lp=lp_from_ub(ub)
          if n_params() le 1 then fname=dialog_pickfile(FILTER='*.hkl', /WRITE, DEFAULT_EXTENSION='hkl', path=path) else fname=fn
          free_lun, 4
          if fname ne '' then $
          begin
          m=max(self.peaks[0:self.peakno-1].intAD[0])
          OPENW, 4, fname
          for i=1, self.peakno do $
          begin

          hkl1 = self.peaks[i-1].hkl
            st = string(self.peaks[i-1].hkl[0], format='(I4)')+$
                 string(self.peaks[i-1].hkl[1], format='(I4)')+$
                 string(self.peaks[i-1].hkl[2], format='(I4)')+$
                 string((self.peaks[i-1].intAD[0])*9999./m, format='(F8.2)')+$
                 string((self.peaks[i-1].intAD[1])*9999./m, format='(F8.2)')+$
                 '  -2'

            hkl2 = T21 ## self.peaks[i-1].hkl
            st2 = string(hkl2[0], format='(I4)')+$
                 string(hkl2[1], format='(I4)')+$
                 string(hkl2[2], format='(I4)')+$
                 string((self.peaks[i-1].intAD[0])*9999./m, format='(F8.2)')+$
                 string((self.peaks[i-1].intAD[1])*9999./m, format='(F8.2)')+$
                 '  -3'

            hkl3 = T31 ## self.peaks[i-1].hkl
            st3 = string(hkl3[0], format='(I4)')+$
                 string(hkl3[1], format='(I4)')+$
                 string(hkl3[2], format='(I4)')+$
                 string((self.peaks[i-1].intAD[0])*9999./m, format='(F8.2)')+$
                 string((self.peaks[i-1].intAD[1])*9999./m, format='(F8.2)')+$
                 '   1'

            PRINTF, 4, st
            PRINTF, 4, st2
            PRINTF, 4, st3
          endfor
          CLOSE, 4
          free_lun, 4
          endif
          return, fname
end
;-------------------------------------------------
function CLASS_peaktable::save_unitcell

          fname=dialog_pickfile(FILTER='*.dat', /WRITE, DEFAULT_EXTENSION='dat')
          free_lun, 4
          if fname ne '' then $
          begin
          OPENW, 4, fname
          PRINTF, 4, 'name'
          for i=1, self.peakno do $
          begin
            st = string(self.peaks[i-1].hkl[0], format='(I4)')+$
                 string(self.peaks[i-1].hkl[1], format='(I4)')+$
                 string(self.peaks[i-1].hkl[2], format='(I4)')+$
                 string(1.0/vlength(self.peaks[i-1].xyz), format='(F8.4)')
            PRINTF, 4, st
          endfor
          PRINTF, 4, '0 0 0'
          CLOSE, 4
          free_lun, 4
          endif
          return, fname
end
;-------------------------------------------------
;-------------------------------------------------
function CLASS_peaktable::save_p4p, fname

          if n_params()  eq 0 then fname=dialog_pickfile(FILTER='*.p4p', /WRITE, DEFAULT_EXTENSION='p4p')
          free_lun, 4
          if fname ne '' then $
          begin
          OPENW, 4, fname


            printf, 4, 'FILEID Seattle      ?             4.00        08/20/08 14:27:16 C8H8Se3'
			printf, 4, 'SITEID ?                                  ?'
			printf, 4, 'TITLE  ?'
			printf, 4, 'CHEM   C8 H8 Se3'
			printf, 4, 'CELL      4.9159   11.1888   16.6681   90.0000   90.0000   90.0000    916.791'
			printf, 4, 'CELLSD    0.0011    0.0028    0.0091    0.0000    0.0000    0.0000     0.229'
			printf, 4, 'ORT1    -4.4774786e-002  2.9954357e-002  5.4960791e-002'
			printf, 4, 'ORT2    -4.7662384e-002  7.9853413e-002 -2.2988310e-002'
			printf, 4, 'ORT3    -1.9262548e-001 -2.6721303e-002 -7.0872242e-003'
			printf, 4, 'ZEROS   0.0000000  0.0000000  0.0000000    0.0000    0.0000    0.0000'
			printf, 4, 'SOURCE ?      0.41328   0.41328   0.41328   1.00000    0.00    0.00'
			printf, 4, 'LIMITS    0.00    0.00    0.00    0.00    0.00    0.00    0.00    0.00    0.00    0.00'
			printf, 4, 'MORPH  ?'
			printf, 4, 'DNSMET ?'
			printf, 4, 'CCOLOR ?'
			printf, 4, 'CSIZE  ?            ?            ?            ?            ?'
			printf, 4, 'ADPAR      512.0000    512.0000      5.0000    1024'
			printf, 4, 'ADCOR       20.1957      9.0120     -0.0658      0.0000      0.0000      0.0000'
			printf, 4, 'BRAVAIS Orthorhombic          P'
       		printf, 4, 'MOSAIC  0.44 0.84'

          for i=1, self.peakno do $
          begin
            st = 'REF1K H      -1  -4   2 -30.000 180.000   0.150  54.736   15.40  166.45 1691.26    158'+$
                 string(self.peaks[i-1].xyz[0], format='(F11.6)')+$
                 string(self.peaks[i-1].xyz[1], format='(F11.6)')+$
                 string(self.peaks[i-1].xyz[2], format='(F11.6)')+$
                 '   0.000   0.000   0.000   0.000 '

            PRINTF, 4, st
          endfor
          PRINTF, 4,'DATA  SPATIAL linear          linear          3.0  512.00  512.00   17.000   3136 1024'
          CLOSE, 4
          free_lun, 4
          endif
          return, fname
end
;-------------------------------------------------
pro CLASS_peaktable::check_profiles
 pt=obj_new('CLASS_peaktable')
 count=1
 for i=self.peakno-1, 1, -1  do $
 begin
   if abs (self.peaks[i].xyz[0]-self.peaks[i-1].xyz[0]) lt 0.01 and $
      abs (self.peaks[i].xyz[1]-self.peaks[i-1].xyz[1]) lt 0.01 and $
      abs (self.peaks[i].xyz[2]-self.peaks[i-1].xyz[2]) lt 0.01 then $
      begin
        count=count+1
      endif else $
      begin
          if count eq 1 then self->delete_peak, i
          if count gt 1 then $
          begin
            pos=max(self.peaks[i:i+count-1].intAD[0], kl)
            pt->appendpeak,self.peaks[i+kl]
            count=1
          endif
      endelse
 endfor
 al=pt->get_object()
 self->set_object, al
 obj_destroy, pt
 end


;-------------------------------------------------

pro CLASS_peaktable::select_rpeaks, box00x,box00y, box11x, box11y

   for i=1, self.peakno do $
   begin
     if self.peaks[i-1].xyz[1] gt box00x and $
        self.peaks[i-1].xyz[1] lt box11x and $
        self.peaks[i-1].xyz[2] gt box00y and $
        self.peaks[i-1].xyz[2] lt box11y then $
        begin
          self.peaks[i-1].selected[0]=1
          self.selectedno=self.selectedno+1
        end
   endfor
end

;-------------------------------------------------

pro CLASS_peaktable::select_rpeaks_det, box00x,box00y, box11x, box11y

   for i=1, self.peakno do $
   begin
     if self.peaks[i-1].DetXY[0] gt box00x and $
        self.peaks[i-1].DetXY[0] lt box11x and $
        self.peaks[i-1].DetXY[1] gt box00y and $
        self.peaks[i-1].DetXY[1] lt box11y then $
        begin
          self.peaks[i-1].selected[0]=1
          self.selectedno=self.selectedno+1
        end
   endfor
end

;-------------------------------------------------

pro CLASS_peaktable::unselect_rpeaks, box00x,box00y, box11x, box11y

   for i=1, self.peakno do $
   begin
     if self.peaks[i-1].xyz[1] gt box00x and $
        self.peaks[i-1].xyz[1] lt box11x and $
        self.peaks[i-1].xyz[2] gt box00y and $
        self.peaks[i-1].xyz[2] lt box11y then $
        begin
          self.peaks[i-1].selected[0]=0
          self.selectedno=self.selectedno-1
        end
   endfor
end


;-------------------------------------------------

pro CLASS_peaktable::invert_selection

   for i=1, self.peakno do $
     if self.peaks[i-1].selected[0] eq 1 then $
     begin
        self.peaks[i-1].selected[0]=0
        self.selectedno=self.selectedno-1
      endif else $
      begin
        self.peaks[i-1].selected[0]=1
        self.selectedno=self.selectedno+1
      endelse
end

;-------------------------------------------------

pro CLASS_peaktable::unselect_all

   for i=1, self.peakno do $
     self.peaks[i-1].selected[0]=0
     self.selectedno=0
end
;-------------------------------------------------
pro CLASS_peaktable::select_all

   for i=1, self.peakno do $
     self.peaks[i-1].selected[0]=1
     self.selectedno=self.peakno
end
;-------------------------------------------------

pro CLASS_peaktable::delete_selected

   for i=self.peakno, 0, -1 do $
     if self.peaks[i].selected[0] eq 1 then self->delete_peak, i
   self.selectedno=0
end

;-------------------------------------------------
pro CLASS_peaktable::move_selected, opt
; moves selected peaks from self to peaktable opt. New peaks are placed at the end of the file
   pt=opt->get_object()
   a=self.peaks[0:self.peakno-1].selected[0]
   loc=where(a eq 1)
   if loc[0]eq -1 then pn=0 else pn=n_elements(loc)
   if pn gt 0 then $
   begin
    pt.peaks[pt.peakno:pt.peakno+pn-1]=self.peaks[loc]
    pt.peakno=pt.peakno+pn
    opt->set_object, pt
    self->delete_selected
   end
end

;-------------------------------------------------


function CLASS_peaktable::get_object
COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

  ref_peaktable.peakno = self.peakno
  ref_peaktable.selectedno = self.selectedno
  ref_peaktable.peaks  = self.peaks

  return, ref_peaktable

end

;--------------------------------------------------------------

pro CLASS_peaktable::set_object,ref_peaktable

  self.peakno = ref_peaktable.peakno
  self.selectedno = ref_peaktable.selectedno
  self.peaks  = ref_peaktable.peaks

end

;--------------------------------------------------------------

function CLASS_peaktable::get_element, n
common errors, er

           er=0
           if n lt self.peakno then return, self.peaks[n] else er=1

end

;--------------------------------------------------------------

pro CLASS_peaktable::set_element, n, ref_peak
common errors, er

           er=0
           if n lt self.peakno then self.peaks[n]=ref_peak else er=1
end

;---------------------------------------------------------------

pro CLASS_peaktable::replacepeak, n, ref_peak
common errors, er

           er=0
           if n lt self.peakno then self->set_element, n, ref_peak else er=1
end

;---------------------------------------------------------------

pro CLASS_peaktable::appendpeak, ref_peak
            if self.peakno lt 10000 then begin
             self.peakno=self.peakno+1
             self.peaks[self.peakno-1]=ref_peak
            endif
end

;---------------------------------------------------------------

pro CLASS_peaktable::select_peak, k
            if k gt 0 and k lt self.peakno  then begin
             self.peaks[k].selected[0]=1
            endif
end

pro CLASS_peaktable::select_peaks, k
            if k[0] ne -1 then  begin
             self.peaks[k].selected[0]=1
            endif
end

;---------------------------------------------------------------

pro CLASS_peaktable::unselect_peak, k
            if k gt 0 and k lt self.peakno  then begin
             self.peaks[k].selected[0]=0
            endif
end

;---------------------------------------------------------------

pro CLASS_peaktable::delete_peak, sel
common errors, er

           er=0
           if sel lt self.peakno then $
           begin
            if self.peaks[sel].selected[0] eq 1 then self.selectedno=self.selectedno-1

            for k=sel, self.peakno-2 do $
              self.peaks[k]=self.peaks[k+1]
            self.peakno=self.peakno-1
           endif else er=1

end

;---------------------------------------------------------------

pro CLASS_peaktable::insert_peak, sel, ref_peak
common errors, er

           er=0
           if sel le self.peakno then $
           begin
            for k=self.peakno, sel+1, -1 do $
            self.peaks[k]=self.peaks[k-1]
            self.peaks[sel]=ref_peak
            self.peakno=self.peakno+1
           endif else er=1
end

;---------------------------------------------------------------

pro CLASS_peaktable::initialize

           self.peakno=0
           self.selectedno=0
end

;---------------------------------------------------------------

pro CLASS_peaktable::rotate_vectors, ran
common Rota, Mtx
pi=acos(-1.0)
xyz=[0.0,0.0,0.0]

          GenerateR, 3, -ran[0]
          Om=Mtx
          GenerateR, 1, -ran[1]
          Ch=Mtx
          GenerateR, 2, -ran[2]
          Ph=Mtx

      for i=0,self.peakno-1 do $
      begin
         self.peaks[i].XYZ=Om ## Ch ## Ph ## self.peaks[i].XYZ
      endfor

end


;---------------------------------------------------------------


function CLASS_peaktable::find_closest_peak, ref_peak

pk=obj_new('CLASS_peak')

tab=[1000.0,9999]

for i=0, self.peakno-1 do $
begin
  pk->set_object, self.peaks[i]
  disti=pk->distance(ref_peak)
  if disti lt tab[0] then $
  begin
    tab[0]=disti
    tab[1]=i
  endif
end
;print,'mindist= ', distmin
obj_destroy, pk
return, tab

end

;----------------------------------------------------------

pro CLASS_peaktable::read_object_from_file, fname, sc

  common peakread, prchoice
  common odet, oadetector
  COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak
  COMMON wavelength, wv

  if n_params() eq 1 then sc=1.0


  gonio=[0.0,0.0,0.0,0.0,0.0,0.0]
            prchoice=1
            if self.peakno ne 0 then peak_table_not_empty
            case prchoice of
            2: $ ; overwrite
            begin
              self->initialize
              free_lun, 4
              OPENR, 4, fname
              A=assoc(4, ref_peak)
              i=0
              while (not eof(4)) do begin
                ref_peak=A[i]
                ref_peak.intAD[0]=sc*ref_peak.intAD[0]
                ref_peak.intAD[1]=sc*ref_peak.intAD[1]
                if not (ref_peak.xyz[0] eq 0 and ref_peak.xyz[1] eq 0 and ref_peak.xyz[2] eq 0) and not  (ref_peak.detxy[0] eq 0 and ref_peak.detxy[1] eq 0) then $
                begin
                 self->appendpeak, ref_peak
                 i=i+1
                end
              endwhile
              CLOSE, 4
              free_lun, 4
            end ; of overwrite
            1: $
            begin
              free_lun, 4
              OPENR, 4, fname
              A=assoc(4, ref_peak)
              i=0
              while (not eof(4)) do begin
                ref_peak=A[i]
                ref_peak.intAD[0]=sc*ref_peak.intAD[0]
                ref_peak.intAD[1]=sc*ref_peak.intAD[1]
                self->appendpeak, ref_peak
                i=i+1
              endwhile
              CLOSE, 4
              free_lun, 4
            end ; of append
            else:
            endcase
 END
;----------------------------------------------------------------------------

pro CLASS_peaktable::APPEND_object_from_file, fname
common odet, oadetector
COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak
COMMON wavelength, wv

              free_lun, 4
              OPENR, 4, fname
              A=assoc(4, ref_peak)
              i=0
              while (not eof(4)) do begin
                ref_peak=A[i]
                ref_peak.xyz=oadetector->calculate_XYZ_from_pixels_mono(ref_peak.DetXY, ref_peak.gonio, wv)
                self->appendpeak, ref_peak
                i=i+1
              endwhile
              CLOSE, 4
              free_lun, 4
             ; Update_peak_list_info, self
 END
;----------------------------------------------------------------------------

pro CLASS_peaktable::write_object_to_file, fname

            COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

            free_lun, 4
            OPENW, 4, fname
            A=assoc(4, ref_peak)
            i=self.peakno
            for i=0, self.peakno-1 do $
            begin
               ref_peak=self->get_element(i)
               A[i]=ref_peak
            end
            CLOSE, 4
            free_lun, 4
 END

;------------------------------------------------------

pro CLASS_peaktable::calculate_all_xyz_from_pix, oadetector

  for i=0, self.peakno-1 do $
  begin
  ; pix, gonio, lambda
    pix=[self.peaks[i].detxy[0], self.peaks[i].detxy[1]]
    gonio=self.peaks[i].gonio
     xyz=oadetector->calculate_XYZ_from_pixels_mono(pix, gonio, 12.39842/15.0)
     self.peaks[i].xyz=xyz
  endfor
  ;draw_rpeaks, self
end

;------------------------------------------------------

pro CLASS_peaktable::apply_rotation, angles

common odet, oadetector

common Rota, Mtx
pi=acos(-1.0)
xyz=[0.0,0.0,0.0]

          GenerateR, 3, -angles[0]
          Om=Mtx

          GenerateR, 1,  angles[1]
          Ch=Mtx
          iCh=invert(Ch)

          GenerateR, 2,  angles[2]
          Chy=Mtx
          iChy=invert(Chy)


          gonio=[0.0,0.0,0.0,-angles[0],angles[1],0.0]


  for i=0, self.peakno-1 do $
  begin

     ;xyz1=Om##Chy##Ch##iChy##self.peaks[i].xyz
     DXYZ=oadetector->calculate_pixels_from_xyz(self.peaks[i].xyz, gonio)
     ;self.peaks[i].xyz=xyz1
     self.peaks[i].DetXY=DXYZ
  endfor
end


;------------------------------------------------------

pro CLASS_peaktable::select_indexable, ub, limit

; reindexes peaktable with a limit

  iUB=invert(UB)
  ind=0
  self->unselect_all
  for i=0, self.peakno-1 do $
  begin
     vec=iUB##self.peaks[i].XYZ
     hkla=[round(vec[0]),round(vec[1]),round(vec[2])]
     dhkl=vlength(hkla-vec)
     if dhkl gt limit or $
        (abs(vec[0]) lt limit and abs(vec[1]) lt limit and abs(vec[2]) lt limit) then $
     begin
        self.peaks[i].HKL=[0,0,0]
        self.peaks[i].selected[0]=1
        self.selectedno=self.selectedno+1
     endif else $
     begin
        self.peaks[i].HKL=hkla
        self.peaks[i].selected[0]=0
        ind=ind+1
     endelse
  endfor

end
;---------------------------------------------
;------------------------------------------------------

pro CLASS_peaktable::select_indexable_lp, lp, limit

; reindexes peaktable with a limit
  ind=0
  self->unselect_all
  for i=0, self.peakno-1 do $
  begin
     d=1/vlength(self.peaks[i].XYZ)
     hkl=[10,10,10]
     clo=find_closest_d(lp, d, hkl)
     if clo[1] gt limit then $
     begin
        self.peaks[i].HKL=[0,0,0]
        self.peaks[i].selected[0]=1
        self.selectedno=self.selectedno+1
     endif else $
     begin
        self.peaks[i].HKL=clo[3]
        self.peaks[i].selected[0]=0
        ind=ind+1
     endelse
  endfor

end
;---------------------------------------------

pro CLASS_peaktable::refine_omega, ub
  common Rota, Mtx
  omdif=0.0
  dif=0.0
  maxdif=0.0
  dif1=0.0
  dif2=0.0
  if self.peaks[0].energies[0] eq 0 then re=dialog_message('Peak energies are not set in peak table. Use the latest version of GSE_ADA.') else $
  begin
   for i=0, self.peakno-1 do $
   begin
     xyz=UB ## self.peaks[i].hkl
     om=get_omega(self.peaks[i].energies[0] , xyz)
     om0=self.peaks[i].gonio[3]
     dif1=abs(om[0]-self.peaks[i].gonio[3])
     dif2=abs(om[1]-self.peaks[i].gonio[3])
     dif3=abs(om[0]-360.-self.peaks[i].gonio[3])
     dif4=abs(om[1]-360.-self.peaks[i].gonio[3])
     g=min([dif1, dif2, dif3, dif4],ll)
     case ll of
     0: self.peaks[i].gonio[3]=om[0]
     1: self.peaks[i].gonio[3]=om[1]
     2: self.peaks[i].gonio[3]=om[0]-360.
     3: self.peaks[i].gonio[3]=om[1]-360.
     endcase
     omdif=om0-self.peaks[i].gonio[3]
     GenerateR, 3, omdif
     xyz=mtx # self.peaks[i].xyz
     self.peaks[i].xyz=xyz
   endfor
  endelse
end
;---------------------------------------------

function CLASS_peaktable::aver_ang_error_mono, ub
  omdif=0.0
  dif=0.0
  maxdif=0.0
  dif1=0.0
  dif2=0.0
  en=self.peaks.energies[0]
  en=37.0766
  for i=0, self.peakno-1 do $
  begin
     xyz=UB ## self.peaks[i].hkl
     om=get_omega(en , xyz)
     ;angs=calculate_EDDangles_from_xyz(xyz, [0.0,0.0,0.0], self.peaks[i].gonioss[1])
     dif1=abs(om[0]-self.peaks[i].gonio[3])
     dif2=abs(om[1]-self.peaks[i].gonio[3])
     dif=min([dif1,dif2])
     omdif=omdif+dif
     if dif gt maxdif then maxdif=dif
  endfor
  return, [omdif/self.peakno, maxdif]
end
;---------------------------------------------

function CLASS_peaktable::reindex, ocrystal, limit

; reindexes peaktable with a limit

  cry=ocrystal->get_object()
  UB=cry.UB_matrix
  iUB=invert(UB)
  ind=0
  self->unselect_all
  for i=0, self.peakno-1 do $
  begin

     vec=iUB##self.peaks[i].XYZ
     hkla=[round(vec[0]),round(vec[1]),round(vec[2])]
     dhkl=vlength(hkla-vec)
     if dhkl gt limit or $
        (abs(vec[0]) lt 0.01 and abs(vec[1]) lt 0.01 and abs(vec[2]) lt 0.01) then $
     begin
        self.peaks[i].HKL=[0,0,0]
        self.peaks[i].selected[0]=1
        self.selectedno=self.selectedno+1
     endif else $
     begin
        self.peaks[i].HKL=hkla
        self.peaks[i].selected[0]=0
        ind=ind+1
     endelse
  endfor
  return, ind
end
;---------------------------------------------

function CLASS_peaktable::BUILD_d_list
; creates a vector of d-spacings
; requires correct xyz lengths
 dlist=fltarr(self.peakno)
 for i=1, self.peakno do dlist[i-1]=1/vlength(self.peaks[i-1].XYZ)
 return, dlist
end

;---------------------------------------------------------
function CLASS_peaktable::peak_list, ub
; creates a text array containing information about all the peaks

       list2=''
       list=''
       iub=invert(ub)
       for i=1, self.peakno do begin
            hkl=hkl_from_ub_and_xyz(ub, self.peaks[i-1].xyz)
            dhkl=vlength(hkl-self.peaks[i-1].HKL)
            list2=string(self.peaks[i-1].selected[0])
            list2=list2+string(i-1)
            list2=list2+string(self.peaks[i-1].HKL[0])
            list2=list2+string(self.peaks[i-1].HKL[1])
            list2=list2+string(self.peaks[i-1].HKL[2])
            list2=list2+string(1.0/vlength(self.peaks[i-1].XYZ)) ; d-spc
            list2=list2+string(self.peaks[i-1].gonio[3])    ;   omega
            list2=list2+string(self.peaks[i-1].intAD[0])    ;  intensity
            hkla=iub ## self.peaks[i-1].xyz
            hkl=[round(hkla[0]),round(hkla[1]),round(hkla[2])]
            dhkl=vlength(hkla-hkl)
            list2=list2+string(dhkl)
            if i eq 1 then list=list2 else list=[list,list2]
        endfor
        return, list
end

;----------------------------------------------------------------------------

function CLASS_peaktable::peakno
    return, self.peakno
end

;----------------------------------------------------------------------------
function CLASS_peaktable::selectedno
    return, self.selectedno
end

;----------------------------------------------------------------------------


pro CLASS_peaktable::apply_transform, UT

  for i=0, self.peakno do $
    self.peaks[i].hkl=transpose(UT) ## self.peaks[i].hkl
end



;-------------------------------------------------------------------
;-------------------------------------------------------------------


pro CLASS_peaktable_definition

Vector_Math
Crystallography


COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak
COMMON CLASS_Area_detector_parameters_reference, ref_adp

CLASS_Area_detector_parameters

 ref_peak = {CLASS_peak, $
                Stat     : 0,           $   ; reflection status
                HKL      : INTARR(3),   $   ; Miller indices
                XYZ      : FLTARR (3),  $   ; coordinates of the reciprocal sp. vector
                selected : INTARR(2),   $   ; 0-selcted, 1-visible
                DetXY    : FLTARR(2),   $   ; area detector pixel coordinates
                Gonio    : FLTARR(6),   $   ; goniometer settings for the Area detector
                GonioSS  : FLTARR(6),   $   ; setting angles for solid state detector
                nen      : 0,           $   ; number of different energy components
                energies : FLTARR(10),  $   ; energies
                IntAD    : FLTARR(2),   $   ; Intensity from area detector with e.s.d
                position : FLTARR(3),   $   ; Intensity from area detector with e.s.d
                IntSSD   : FLTARR(2),   $   ;
                ; rota   : 0L, $            ; rotation axis
                ; rota_range   : 0L, $      ; rotation range
                ; image_name  : '', $       ; name of the image
                Adp      : ref_adp}         ; Area detector parameters

 ref_peaktable={CLASS_peaktable, $
                 peakno : 0L,$
                 selectedno : 0L,$
                 peaks : REPLICATE(ref_peak, 10000)}

end
