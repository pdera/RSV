;----------------------------------------------------------------
; from class_peaktable.pro

function OLA_accessible_range, pn, om0, ch0

; P.Dera, 04/05/2006
;
; determines for a given peak, the range of omega and chi rotation that keeps the
; peak inside the active region of the detector
; uses oadetector through common block
;
; arguments: pix - pixel coordinates of the peak at 0 goniometer position
;            om0 - proposed range of omega rotation - [start, range, offset]
;            ch0 - proposed range of omega rotation - [start, range, offset]
;
; results:   res - omega start and end, chi start and end, number of steps (every x deg)
;

vis=fltarr(400,3)

res={rs, om:[0.0,0.0,0.0],ch:[0.0,0.0,0.0], leng:0L}

common Rota, Mtx
common odet, oadetector

         om0=[float(om0[0]),float(om0[1]),float(om0[2])]
         ch0=[float(ch0[0]),float(ch0[1]),float(ch0[2])]
         om1=fltarr(2)
         ch1=fltarr(2)
         omo=float(om0[2])
         cho=float(ch0[2])

         data=measure_OLA_curve(pn, 400, [om0,omo],[ch0,cho], points)

         pix0=[0.0,0.0]

         COMMON peaktable_objects, optable1,optable2, optable3, optable0

         pt=optable1->get_object()

         pix0=pt.peaks[long(pn)].detxy

         for i=0, 399 do $
         begin
           pix=[points[i,0],points[i,1]]
           vis[i,0]=data[i,0]
           vis[i,1]=data[i,32]
           if beam_stop(pix) eq 1 or detector_edge(pix) eq 1 or same_side_of_the_beam(pix, pix0) eq 0 then  $
                    vis[i,2]=0 else $
                    vis[i,2]=1
         endfor
         i=0
         ki=[0,0]

         ; determine starting angles

         om1[0]=vis[0,0]
         ch1[0]=vis[0,1]

         if (vis[0,2] eq 0) then $ ; beginning not visible
         begin
           while (vis[i,2] eq 0) and (i lt 399) do $
           begin
             om1[0]=vis[i,0]
             ch1[0]=vis[i,1]
             ki[0]=i
             i=i+1
           endwhile
           if i eq 399 then $ ; no visibility at all
           begin
             om1[1]=-om1[0]+vis[i,0]
             ch1[1]=-ch1[0]+vis[i,1]
             pix_dif=[0,0]
           endif else $ ; visibility found
           begin
             while vis[i,2] eq 1 and i lt 399 do $
             begin
               om1[1]=-om1[0]+vis[i,0]
               ch1[1]=-ch1[0]+vis[i,1]
               ki[1]=i
               i=i+1
             endwhile
             pix_0=[points[ki[0],0],points[ki[0],1]]
             pix_1=[points[ki[1],0],points[ki[1],1]]
             pix_dif=pix_1-pix_0
          endelse
        endif else $ ; beginning visible
        begin
           om1[0]=vis[0,0]
           ch1[0]=vis[0,1]
           ki[0]=0
           while (vis[i,2] eq 1) and (i lt 399) do $
           begin
             om1[1]=-om1[0]+vis[i,0]
             ch1[1]=-ch1[0]+vis[i,1]
             ki[1]=i
             i=i+1
           endwhile
             pix_0=[points[ki[0],0],points[ki[0],1]]
             pix_1=[points[ki[1],0],points[ki[1],1]]
             pix_dif=pix_1-pix_0
        endelse

           res.leng=sqrt(pix_dif[0]*pix_dif[0]+pix_dif[1]*pix_dif[1])
           res.om=[om1,omo]
           res.ch=[ch1,cho]
           return, res
end

;----------------------------------------------------------------
; from class_peaktable.pro

; this function is used in EDX calculations
function CLASS_peaktable::aver_ang_error, ub
  omdif=0.0
  maxdif=0.0
  for i=0, self.peakno-1 do $
  begin
     xyz=UB ## self.peaks[i].hkl
     angs=calculate_EDDangles_from_xyz(xyz, [0.0,0.0,0.0], self.peaks[i].gonioss[1])
     dif=abs(angs[3]-self.peaks[i].gonioss[3])
     omdif=omdif+dif
     if dif gt maxdif then maxdif=dif
  endfor
  return, [omdif/self.peakno, maxdif]
end

;----------------------------------------------------------------
; from class_peaktable.pro

pro CLASS_peaktable::calculate_all_xyz_from_edd, oadetector,zeros
  pi=acos(-1)
  for i=0, self.peakno-1 do $
  begin
     xyz=calculate_xyz_from_EDD(self.peaks[i].gonioSS, zeros)
     xyz=xyz/vlength(xyz)
     xyz[0]=-xyz[0]
     lambda = 12.39842/self.peaks[i].energies[0]
     leng=lambda/(2.0*sin(self.peaks[i].gonioSS[1]*pi/360.0))
     self.peaks[i].xyz=xyz/leng
  endfor
end



;----------------------------------------------------------------
; from class_peaktable.pro


pro CLASS_peaktable::calculate_SSD_from_pixels, gonio
common odet, oadetector
outgonio=fltarr(2,3)

 for i=0, self.peakno do $
 begin
   outgonio=oadetector->calculate_EDDangles_from_pixels(self.peaks[i].detxy[0], self.peaks[i].detxy[1], gonio)
   if abs(outgonio[1,1]) lt abs(outgonio[0,1]) then $
   self.peaks[i].gonioSS=[0.0,outgonio[1,0], 0.0,outgonio[1,1], outgonio[1,2],0.0] else $
   self.peaks[i].gonioSS=[0.0,outgonio[0,0], 0.0,outgonio[0,1], outgonio[0,2],0.0]
 endfor

end
;----------------------------------------------------------------
; from class_peaktable.pro


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

;----------------------------------------------------------------
; from class_peaktable.pro

function CLASS_peaktable::calculate_misindex, ocrystal, limit, i
  cry=ocrystal->get_object()
  UB=cry.UB_matrix
  iUB=invert(UB)
  vec1=iUB##self.peaks[i].XYZ
  if self.peaks[i].selected[0] eq 0 then return, vlength(vec1-self.peaks[i].HKL) else return, -1.0
end

;----------------------------------------------------------------
; from class_peaktable.pro

function CLASS_peaktable::refineX0, oadetector, ocrystal,wav, v_limit, v_range, v_var, v_old

  ad=oadetector->get_object()
  foms=fltarr(21,2)
  step=v_range/21
  wv=wav
  limit=v_limit
  v_old=ad.beamx

  print, self->reindex(ocrystal, limit)

  for i=-10, 10 do $
  begin
    ad.beamx=v_old+i*step
    oadetector->set_object, ad
    self->recalculate_all_xyz, oadetector, wv
    foms[i+10,1]=self->calculate_indexing_fom(ocrystal, limit)
    print, i, self->reindex(ocrystal, limit)
    foms[i+10,0]=ad.beamx
  end

  ad.beamx=v_old
  oadetector->set_object, ad
  self->recalculate_all_xyz, oadetector, wv
  plot, foms[0:20,0], foms[0:20,1]
  a=min(foms[0:20,1],i)
  if a gt -1.0 then return, foms[i,0] else return, v_old
end
;----------------------------------------------------------------
; from class_peaktable.pro

function CLASS_peaktable::refineY0, oadetector, ocrystal,wav, v_limit, v_range, v_var, v_old

  ad=oadetector->get_object()
  foms=fltarr(21,2)
  step=v_range/21
  wv=wav
  limit=v_limit
  v_old=ad.beamy

  pn=self->reindex(ocrystal, limit)

  for i=-10, 10 do $
  begin
    ad.beamy=v_old+i*step
    oadetector->set_object, ad
    self->recalculate_all_xyz, oadetector, wv
    foms[i+10,1]=self->calculate_indexing_fom(ocrystal, limit)
    foms[i+10,0]=ad.beamy
  end

  ad.beamy=v_old
  oadetector->set_object, ad
  self->recalculate_all_xyz, oadetector, wv
  plot, foms[0:20,0], foms[0:20,1]
  a=min(foms[0:20,1],i)
  if a gt -1.0 then return, foms[i,0] else return, v_old
end

;----------------------------------------------------------------
; from class_peaktable.pro

function CLASS_peaktable::refineD, oadetector, ocrystal,wav, v_limit, v_range, v_var, v_old

  ad=oadetector->get_object()
  foms=fltarr(21,2)
  step=v_range/21
  wv=wav
  limit=v_limit
  v_old=ad.dist

  pn=self->reindex(ocrystal, limit)

  for i=-10, 10 do $
  begin
    ad.dist=v_old+i*step
    oadetector->set_object, ad
    self->recalculate_all_xyz, oadetector, wv
    foms[i+10,1]=self->calculate_indexing_fom(ocrystal, limit)
    foms[i+10,0]=ad.dist
  end

  ad.dist=v_old
  oadetector->set_object, ad
  self->recalculate_all_xyz, oadetector, wv
  plot, foms[0:20,0], foms[0:20,1]
  a=min(foms[0:20,1],i)
  if a gt -1.0 then return, foms[i,0] else return, v_old
end

;----------------------------------------------------------------
; from class_peaktable.pro

function CLASS_peaktable::refineTth, oadetector, ocrystal,wav, v_limit, v_range, v_var, v_old

  ad=oadetector->get_object()
  foms=fltarr(21,2)
  step=v_range/21
  wv=wav
  limit=v_limit

  v_old=ad.ttheta0

  pn=self->reindex(ocrystal, limit)

  for i=-10, 10 do $
  begin
    ad.ttheta0=v_old+i*step
    oadetector->set_object, ad
    self->recalculate_all_xyz, oadetector, wv
    foms[i+10,1]=self->calculate_indexing_fom(ocrystal, limit)
    foms[i+10,0]=ad.ttheta0
  end

  ad.ttheta0=v_old
  oadetector->set_object, ad
  self->recalculate_all_xyz, oadetector, wv
  plot, foms[0:20,0], foms[0:20,1]
  a=min(foms[0:20,1],i)
  if a gt -1.0 then return, foms[i,0] else return, v_old
end

;----------------------------------------------------------------
; from class_peaktable.pro

function CLASS_peaktable::refineLa, oadetector, ocrystal,wav, v_limit, v_range, v_var, v_old

  ad=oadetector->get_object()
  foms=fltarr(21,2)
  step=v_range/21
  wv=wav
  limit=v_limit
  v_old=wav

  pn=self->reindex(ocrystal, limit)

  for i=-10, 10 do $
  begin
    wv=v_old+i*step
    oadetector->set_object, ad
    self->recalculate_all_xyz, oadetector, wv
    foms[i+10,1]=self->calculate_indexing_fom(ocrystal, limit)
    foms[i+10,0]=wv
  end

  wav=v_old
  self->recalculate_all_xyz, oadetector, wav
  plot, foms[0:20,0], foms[0:20,1]
  a=min(foms[0:20,1],i)
  if a gt -1.0 then return, foms[i,0] else return, v_old
end

;----------------------------------------------------------------
; from class_peaktable.pro

pro CLASS_peaktable::refine_rotations, oadetector, ocrystal,wav, v_limit

  ad=oadetector->get_object()
  foms=fltarr(21,2)
  step=2.0/21
  wv=wav
  limit=v_limit
  sum=0.0
  count=0
  for j=0, self.peakno-1 do $
  if self.peaks[j].selected[0] eq 0 then $
  begin
    v_old=self.peaks[j].gonio[5]
    aa=self->calculate_misindex(ocrystal, limit, j)
    for i=-10, 10 do $
    begin
      self.peaks[j].gonio[5]=v_old+i*step
      self->recalculate_xyz, oadetector, wv, j
      foms[i+10,1]=self->calculate_misindex(ocrystal, limit, j)
      foms[i+10,0]=self.peaks[j].gonio[5]
    endfor
  a=min(foms[0:20,1],k)
  self.peaks[j].gonio[5]=foms[k,0]
  self->recalculate_xyz, oadetector, wv, j
  sum=sum+a
  count=count+1
  endif
  print, 'final fom=', sum/count
end
;----------------------------------------------------------------
; from class_peaktable.pro

pro CLASS_peaktable::calculate_all_rprime_OLA, oadetector

      for i=0,self.peakno-1 do $
      begin
                vec1=oadetector->calculate_XYZ_from_pixels(self.peaks[i].DetXY[0],self.peaks[i].DetXY[1], [0.0,0.0,0.0,0.0,0.0,0.0])
                self.peaks[i].xyz=(1/VLength(vec1))#vec1
      endfor

end

;---------------------------------------------------------------

; from class_peaktable.pro

pro CLASS_peaktable::calculate_all_pixels_OLA, oadetector
      for i=0,self.peakno-1 do $
      begin
                self.peaks[i].DetXY=oadetector->calculate_pixels_from_XYZ(self.peaks[i].XYZ,  [0.0,0.0,0.0,0.0,0.0,0.0])
      endfor
end

;---------------------------------------------------------------

; from class_peaktable.pro

pro CLASS_peaktable::sort_angle, an
  pt=self.peaks
  angs=self.peaks[0:self->peakno()-1].gonioss[an]
  sub=sort(angs)
  for i=0, self->peakno()-1 do $
  begin
   self.peaks[i]=pt[sub[i]]
  end
end

;---------------------------------------------------------------

; from class_peaktable.pro

pro CLASS_peaktable::generate_DC_peaklist_tth, UB, tth, omb, exti
; generates peaktable of peaks accessible within the omb
; window, at a constant 2th angle
COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

   for h=-omb[5],omb[5] do $
     for k=-omb[6],omb[6] do $
       for l=-omb[7],omb[7] do $
       begin
          hkl=[h,k,l]

          if  exti eq 'P' or $
             (exti eq 'R' and (long((h-k+l)/3.0) eq (h-k+l)/3.0)) or $
             (exti eq 'F' and (long((h+k)/2.0) eq (h+k)/2.0) and  (long((k+l)/2.0) eq (k+l)/2.0) and  (long((h+l)/2.0) eq (h+l)/2.0)) or $
             (exti eq 'I' and (long((h+k+l)/2.0) eq (h+k+l)/2.0)) or $
             (exti eq 'A' and (long((k+l)/2.0) eq (k+l)/2.0)) or $
             (exti eq 'B' and (long((h+l)/2.0) eq (h+l)/2.0)) or $
             (exti eq 'C' and (long((h+k)/2.0) eq (h+k)/2.0)) $
          then $
          begin
          xyz=UB ## hkl
          d=1.0/vlength(xyz)
          en=calculate_energy(d,abs(tth))

          if omb[4] eq 0 then tth=-abs(tth)
          angs=calculate_EDDangles_from_xyz(xyz, [0.0,0.0], tth)

          if angs[3] gt omb[2] and angs[3] lt omb[3] and $
          en gt 15.0 and en lt 75.0 then $
          begin
            ref_peak.gonioss=angs
            ref_peak.gonioss[0]=tth
            ref_peak.gonioss[3]=ref_peak.gonioss[3]
            ref_peak.xyz=xyz
            ref_peak.hkl=hkl
            ref_peak.energies[0]=en
            self->appendpeak, ref_peak
          endif
          endif ; syst abs
       endfor
end

;---------------------------------------------------------------
;---------------------------------------------------------------

; from class_peaktable.pro

pro CLASS_peaktable::generate_DC_peaklist_en, UB, en, omb, exti
; generates peaktable of peaks accessible within the omb
; window, at a constant energy
; exti is systematic absence code
; 'R'
COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

   for h=-omb[5],omb[5] do $
     for k=-omb[6],omb[6] do $
       for l=-omb[7],omb[7] do $
       begin
          hkl=[h,k,l]

          if  exti eq 'P' or $
             (exti eq 'R' and (long((h-k+l)/3.0) eq (h-k+l)/3.0)) or $
             (exti eq 'F' and (long((h+k)/2.0) eq (h+k)/2.0) and  (long((k+l)/2.0) eq (k+l)/2.0) and  (long((h+l)/2.0) eq (h+l)/2.0)) or $
             (exti eq 'I' and (long((h+k+l)/2.0) eq (h+k+l)/2.0)) or $
             (exti eq 'A' and (long((k+l)/2.0) eq (k+l)/2.0)) or $
             (exti eq 'B' and (long((h+l)/2.0) eq (h+l)/2.0)) or $
             (exti eq 'C' and (long((h+k)/2.0) eq (h+k)/2.0)) $
          then $
          begin
          xyz=UB ## hkl
          d=1.0/vlength(xyz)
          tth=calculate_tth(d,en)

          if omb[4] eq 0 then tth=-abs(tth)
          angs=calculate_EDDangles_from_xyz(xyz, [0.0,0.0], tth)

          if angs[3] gt omb[2] and angs[3] lt omb[3] and $
          tth gt omb[0] and tth lt omb[1] then $
          begin
            ref_peak.gonioss=angs
            ref_peak.gonioss[3]=ref_peak.gonioss[3]
            ref_peak.gonioss[0]=tth
            ref_peak.xyz=xyz
            ref_peak.hkl=hkl
            ref_peak.energies[0]=en
            self->appendpeak, ref_peak
          endif
          endif ; syst abs
       end
end

;---------------------------------------------------------------

; from class_peaktable.pro

pro CLASS_peaktable::sort

COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

           pt=obj_new('CLASS_peaktable')
           ref_peak1=ref_peak
           ref_peak=self->get_element(0)
           pt->appendpeak, ref_peak
           l=1
           for k=l, self.peakno-1 do $
           begin
             for m=0, k-1 do $
             begin
               ref_peak=pt->get_element(m)
               if self.peaks[k].gonioss[1] lt ref_peak.gonioss[1] then $
               begin
                    ref_peak=self->get_element(k)
                    pt->insert_peak, m, ref_peak
                    l=l+1
                    goto, nex
               endif
             endfor ;m
             ref_peak=self->get_element(k)
             pt->appendpeak, ref_peak
             l=l+1
           nex:
           endfor ;k
           lg=pt->get_object()
           self->set_object, lg
           obj_destroy, pt
end

;---------------------------------------------------------------

; from class_peaktable.pro

function CLASS_peaktable::refine_om0, o1a, nst, UB, om0a, ch0
common oadet,oad
  om0s=fltarr(nst)
  fomis=fltarr(nst)
  self->calculate_all_xyz_from_edd, oad,[0.0,om0a,ch0]
  UB=self->recomp_UB()
  fo=self->indexing_FOM(UB, 0)
;  print, '*********************'
  print, 'Initial om0=', om0a, fo
  ;print, '*********************'
  o1=om0a-o1a
  o2=om0a+o1a

  od=(o2-o1)/nst
  for i=0, nst-1 do $
  begin
    om0=o1+i*od
    om0s[i]=om0
    self->calculate_all_xyz_from_edd, oad,[0.0,om0, ch0]
    a=self->indexing_FOM(UB, 0)
    fomis[i]=a[0]
  endfor
  plot, om0s[0:nst-1], fomis[0:nst-1]
  f0=min(fomis[0:nst-1], i)
  Result = POLY_FIT( om0s[0:nst-1], fomis[0:nst-1], 2, YFIT=YF)
  oplot, om0s[0:nst-1], yf[0:nst-1], color=130
  om0=-result[1]/(2.0*result[2])
  self->calculate_all_xyz_from_edd, oad,[0.0,om0, ch0]
  fomis[0]=self->indexing_FOM(UB, 0)
  UB=self->recomp_UB()
  fomis[1]=self->indexing_FOM(UB, 0)
  ;print, '*********************'
  print, 'Final om0=', om0, fomis[0];, fomis[1]
  ;print, '*********************'

  ;print,UB
  self->calculate_all_xyz_from_edd, oad,[0.0,0.0,0.0]
  return, om0


end



;-------------------------------------------------

; from class_peaktable.pro

pro CLASS_peaktable:: refine_ind_om,om_r,ub
common oadet,oad
foms=fltarr(101,2)
om_s=om_r/100.0
zeros=[0.0,0.0,0.0]
iub=invert(ub)
for j=0, self.peakno-1 do $
begin
 om0=self.peaks[j].gonioss[3]-om_r/2.0
 for i=0, 100 do $
 begin
  om=om0+i*om_s
  gon=self.peaks[j].gonioss
  gon[3]=om
  xyz=calculate_xyz_from_EDD(gon, zeros)
  d=d_from_tth_and_en(gon[1], self.peaks[j].energies[0])
  xyz[0]=-xyz[0]
  xyz=xyz/vlength(xyz)
  xyz=xyz/d
  hkl= iub ## xyz
  dhkl=vlength(self.peaks[j].hkl-hkl)
  foms[i,0]=om
  foms[i,1]=dhkl
 endfor
 a=min(foms[0:100,1],l)
 plot, foms[0:100,0], foms[0:100,1]
 self.peaks[j].gonioss[3]=foms[l,0]
endfor
self->calculate_all_xyz_from_edd, oad,[0.0,0.0,0.0]
end
;-------------------------------------------------

; from class_peaktable.pro

pro CLASS_peaktable::apply_om0, om0
common oadet,oad
  for i=1, self.peakno do $
    self.peaks[i-1].gonioSS[3]=self.peaks[i-1].gonioSS[3]+om0
  self->calculate_all_XYZ_from_EDD, oad,[0.0,0.0,0.0]
end

;-------------------------------------------------


; from class_peaktable.pro

function CLASS_peaktable::refine_ch0, o1a, nst, UB, om0, ch0a
common oadet,oad
  om0s=fltarr(nst)
  fomis=fltarr(nst)
  self->calculate_all_xyz_from_edd, oad,[0.0,om0,ch0a]
  ;UB=self->recomp_UB()
  fo=self->indexing_FOM(UB, 0)
;  print, '*********************'
  print, 'Initial ch0=', ch0a, fo
  ;print, '*********************'
  o1=ch0a-o1a
  o2=ch0a+o1a

  od=(o2-o1)/nst
  for i=0, nst-1 do $
  begin
    ch0=o1+i*od
    om0s[i]=ch0
    self->calculate_all_xyz_from_edd, oad,[0.0,om0, ch0]
    a=self->indexing_FOM(UB, 0)
    fomis[i]=a[0]
  endfor
  plot, om0s[0:nst-1], fomis[0:nst-1]
  f0=min(fomis[0:nst-1], i)
  Result = POLY_FIT( om0s[0:nst-1], fomis[0:nst-1], 2, YFIT=YF)
  oplot, om0s[0:nst-1], yf[0:nst-1], color=130
  ch0=-result[1]/(2.0*result[2])
  self->calculate_all_xyz_from_edd, oad,[0.0,om0, ch0]
  fomis[0]=self->indexing_FOM(UB, 0)
  UB=self->recomp_UB()
  fomis[1]=self->indexing_FOM(UB, 0)
  ;print, '*********************'
  print, 'Final ch0=', ch0, fomis[0];, fomis[1]
  ;print, '*********************'

  ;print,UB

  return, ch0


end


;-------------------------------------------------

; from class_peaktable.pro

function CLASS_peaktable::read_intensities

          COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

          hkl0=0.0
          hkl1=0.0
          hkl2=0.0

          fname=dialog_pickfile(FILTER='*.txt', /READ)
          free_lun, 4
          if fname ne '' then $
          begin
          OPENR, 4, fname
          i=0
          x_int=0
          x_flo1=0.0
          x_flo2=0.0
          x_flo3=0.0
          x_flo4=0.0
          inte=0.0
          en=0.0
          q1=0.0
          q2=0.0
          q3=0.0
          d=-0.0
          st=''
          READF, 4,st
          count=0
          while (not eof(4)) do begin
            READF, 4, x_flo1, x_flo2, inte, en, q1,q2,q3,d
            ref_peak.xyz=[q1/d/10.0, q2/d/10.0, q3/d/10.0]
            ref_peak.hkl=[0,0,0]
            ref_peak.IntAD[0]=inte
            ref_peak.energies[0]=en
            ref_peak.selected[0]=0
            self->appendpeak,ref_peak
            i=i+1
          endwhile
          peakno=i
          CLOSE, 4
          free_lun, 4
          endif
          return, fname
end

;-------------------------------------------------

; from class_peaktable.pro

pro CLASS_peaktable::filter_unique
  profile=fltarr(2,100)
  COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak
  for i=0, self.peakno-2 do $
  begin
    nred=0
    profile[0, nred]=i
    profile[1, nred]=self.peaks[i].IntSSD[0]
    for j=i+1, self.peakno-1 do $
    begin
       if vlength(self.peaks[i].xyz-self.peaks[j].xyz) lt 0.05 then $
       begin
          nred=nred+1
          profile[0, nred]=j
          profile[1, nred]=self.peaks[j].IntSSD[0]
       end
    endfor
    if nred gt 0 then $
    begin
     ; removing redundant
     m=max(profile[1,0:nred],k)
     self->replacepeak, i, self.peaks[profile[0,k]]
     for l=nred, 1, -1 do self->delete_peak, profile[0,l]
    end
  endfor
end

;-------------------------------------------------


; from class_peaktable.pro

pro CLASS_peaktable::import_ps, fname

 COMMON CLASS_peaktable_reference, ref_peaktable, ref_peak

          XYZ=[0.0,0.0,0.0]
          peakno=0
          if fname ne '' then $
          begin
          free_lun, 4
          OPENR, 4, fname
          i=0
          inte0=0
          inte1=0.0
          inte2=0.0
          inte3=0.0
          inte4=0.0
          inte5=0.0
          inte6=0.0
          inte7=0.0
          inte8=0.0
          gonio=fltarr(6)
          while (not eof(4)) do begin
            READF, 4, inte0, inte1, inte2,inte3, inte4, inte5, inte6, inte7, inte8
            gonio[1]=inte4
            gonio[3]=inte5
            gonio[4]=inte6
            ref_peak.gonioSS=gonio
            ;ref_peak.xyz=[inte1,inte2,inte3]
            ref_peak.energies[0]=inte7
            ref_peak.selected[0]=0
            ref_peak.intSSD[0]=inte8
            self->appendpeak,ref_peak
            i=i+1
          endwhile
          peakno=i
          CLOSE, 4
          free_lun, 4
          endif
        END
;----------------------------------------------------------------
; from class_peaktable.pro

function detector_edge, pix

; P.Dera, 04/05/2006
;
; checks is a given pixel is at the edge of the active area of detector
; size of the edge is now set to 3.0 pixels
; arguments: pix - pixel coordinates

  cen=[1024.0,1024.0]
  vec=pix - cen
  rad=sqrt(vec[0]*vec[0]+vec[1]*vec[1])
  if abs(rad-1024.0) lt 10.0 or rad gt 1014 then edge=1 else edge=0
  return, edge
end


;------------------------------------------------------------------
; from class_peaktable.pro

pro CLASS_peaktable::calculate_all_edd_from_xyz, th
; tested and works 10/11/2006

  pi=acos(-1.0)
  for i=0, self.peakno-1 do $
  begin
     ang=calculate_EDDangles_from_xyz(self.peaks[i].xyz, [0.0,0.0], th)
     self.peaks[i].gonioss=ang
     d=1.0/vlength(self.peaks[i].xyz)
     self.peaks[i].energies[0]=calculate_energy(d,abs(th))
     ;print, ang
  endfor
end


;------------------------------------------------------------------

;------------------------------------------------------------------

; from class_peaktable.pro

function same_side_of_the_beam, pix, pix0
; P.Dera, 05/25/06
; checks is pix is on the same side of the beam center as pix0

   common odet, oadetector
   ad=oadetector->get_object()

   cen=[ad.beamx,ad.beamy]

   d1=pix-cen
   d2=pix0-cen
   if sign(d1[0]) eq sign(d2[0]) and sign(d1[1]) eq sign(d2[1]) then res=1 else res=0

   return, res
end

;------------------------------------------------------------------
; from class_peaktable.pro

function beam_stop, pix

; P.Dera, 04/05/2006
;
; checks is a given pixel is in the region obscured by the beamstop
; uses oadetector through common block
; radius of the obscured region is now set to 50.0 pixels
; arguments: pix - piuxel coordinates


  common odet, oadetector
  ad=oadetector->get_object()

  cen=[ad.beamx,ad.beamy]
  vec=pix - cen
  rad=sqrt(vec[0]*vec[0]+vec[1]*vec[1])
  if rad lt 200.0 then edge=1 else edge=0
  return, edge
end
;-----------------------------
; from class_peaktable.pro

function calculate_EDDangles_from_xyz, hla, zeros, tth0
;fixed and checked on 10/10/2006

     hl=hla
     Pi=acos(-1.0)
     s1=[0.0,1.0,0.0]
     ns1=[0.0,-1.0,0.0]
     outgonio=fltarr(6)
;     if hl[1] lt 0.0 and tth0 gt 0.0 then hl=-hla
;     if hl[1] gt 0.0 and tth0 lt 0.0 then hl=-hla

     outgonio[4]=(180.0/pi)*(atan(hl[2],hl[1]))-180.0    ; chi 1
     if outgonio[4] lt 0.0 then outgonio[4]=outgonio[4]+360.0
     if outgonio[4] gt 360.0 then outgonio[4]=outgonio[4]-360.0

     chi1=outgonio[4]

     common Rota, Mtx
     GenerateR, 1, chi1
     c1=Mtx


     hl1=c1##hl

     x=hl1[0]
     al1=ang_between_vecs(s1,hl1)

     ; else al1=ang_between_vecs(ns1,hl1)


     if tth0 gt 0.0 and x lt 0.0 then al2= (180.0-al1)
     if tth0 gt 0.0 and x ge 0.0 then al2=-(180.0-al1)
     if tth0 le 0.0 and x lt 0.0 then al2= -al1
     if tth0 le 0.0 and x ge 0.0 then al2=  al1

     outgonio[3]=tth0/2.0+al2-zeros[1]
     ;if outgonio[3] gt   90.0 and outgonio[3] lt  180.0 then outgonio[3]=180.0-outgonio[3]
     ;if outgonio[3] lt  -90.0 and outgonio[3] gt -180.0 then outgonio[3]=-180.0-outgonio[3]
     ;if outgonio[3] gt  180.0 then outgonio[3]=outgonio[3]-180.0
     ;if outgonio[3] lt -180.0 then outgonio[3]=outgonio[3]+180.0

     outgonio[1]=tth0-zeros[0]
     outgonio[3]=outgonio[3]

     return, outgonio
 end

;-----------------------------
pro obsolete
end