; --------------------------------------------------------------------------------
;
; Library re-organized on 02/22/2013
;
; --------------------------------------------------------------------------------


; pro class_adetector
; pro adetector_class::set_values
; pro adetector_class::set_object, ad
; pro adetector_class::write_object_to_disk, filename
; pro adetector_class::read_object_from_disk, filename
; function adetector_class::get_object
; function adetector_class::calculate_sd_from_pixels, pix, gonio
; function adetector_class::calculate_pixels_from_sd, sd, gonio
; function adetector_class::calculate_XYZ_from_pixels_mono, pix, gonio, lambda
; function adetector_class::calculate_pixels_from_XYZ_mono, XYZ, gonio, lambda
; function adetector_class::calculate_pixels_from_xyz, xyz, gonio
; function adetector_class::calculate_pixels_from_xyz90, xyz, gonio
; function adetector_class::calculate_XYZ_from_pixels, xpix, ypix, gonio

; function calculate_sd_from_hl, hl
; function calculate_hl_from_sd, sd
; function calculate_theta_from_xyz, xyz

; function calculate_sd_from_EDD, gonio, zeros
; function adetector_class::calculate_EDDangles_from_pixels, xpix, ypix, gonio
; function calculate_xyz_from_EDD, gonioEDD, zeros
; function calculate_EDDangles_from_xyz, hl, zeros, tth0
; function adetector_class::calculate_pixels_from_EDDangles, gonioEDD, gonio

pro class_adetector

COMMON class_adetector_reference, ad

ad={adetector_class, $
  dist   : 0.0,  $
  beamx  : 0.0,  $
  beamy  : 0.0,  $
  psizex : 0.0,     $
  psizey : 0.0,     $
  nopixx : 0,       $
  nopixY : 0,       $
  angle  : 0.0,     $
  omega0 : 0.0,     $
  ttheta0: 0.0,     $
  tiltom : 0,       $
  tiltch : 0}

end

;--------------------------------------------------------------------

pro adetector_class::set_values

;COMMON ADPV, dist,beamx,beamy,psizex,psizey,nopixx,nopixY,angle,omega0,ttheta0,tiltom,tiltch

  self.dist   = dist
  self.beamx  = beamx
  self.beamy  = beamy
  self.omega0  = omega0
  self.ttheta0 = ttheta0
  self.psizex = psizex
  self.psizey = psizey
  self.nopixx = nopixx
  self.nopixy = nopixy
  self.angle  = angle
  self.tiltom = tiltom
  self.tiltch = tiltch

end
;--------------------------------------------------------------------

pro adetector_class::set_object, ad

  self.dist   = ad.dist
  self.beamx  = ad.beamx
  self.beamy  = ad.beamy
  self.omega0 = ad.omega0
  self.ttheta0 = ad.ttheta0
  self.psizex = ad.psizex
  self.psizey = ad.psizey
  self.nopixx = ad.nopixx
  self.nopixy = ad.nopixy
  self.angle  = ad.angle
  self.tiltom = ad.tiltom
  self.tiltch = ad.tiltch

end

;--------------------------------------------------------------------

function adetector_class::get_object

COMMON class_adetector_reference, ad

  ad.dist   = self.dist
  ad.beamx  = self.beamx
  ad.beamy  = self.beamy
  ad.omega0 = self.omega0
  ad.ttheta0 = self.ttheta0
  ad.psizex = self.psizex
  ad.psizey = self.psizey
  ad.nopixx = self.nopixx
  ad.nopixy = self.nopixy
  ad.angle  = self.angle
  ad.tiltom = self.tiltom
  ad.tiltch = self.tiltch

  return, ad

end

;----------------------------------
pro adetector_class::write_object_to_disk, filename

COMMON class_adetector_reference, ad

     print, 'Writing detector parameters to:', filename

     ad=self->get_object()

     FREE_LUN, 2
     OPENW, 2, filename
     writeu, 2, ad
     CLOSE, 2
     FREE_LUN, 2

end

;----------------------------------

pro adetector_class::read_object_from_disk, filename

COMMON class_adetector_reference, ad

    print, 'Reading detector parameters from:', filename

     FREE_LUN, 2
     OPENR, 2, filename
     readu, 2, ad
     CLOSE, 2
     FREE_LUN, 2
     self->set_object, ad

end

;--------------------------------------------------------------

function adetector_class::calculate_sd_from_pixels, pix, gonio

; Calculates the coordinates of the diffracted beam
; versor from pixel coordinates

; modified for GSECARS

     xpix=pix[0]
     ypix=pix[1]

     vec1=[0.0,0.0,0.0]
     vec2=vec1
     vec3=vec1

     ;--- calclate relative coordinares

     xrel=(xpix-self.beamX)*self.psizeX
     yrel=(ypix-self.beamY)*self.psizeY

     ;--- create sd at 2theta 0

     vec1[0]=0.0
     vec1[1]=xrel
     vec1[2]=yrel
     vec2[0]=self.dist
     vec2[1]=0.0
     vec2[2]=0.0
     vec3=vec2+vec1

     ;--- include detector roatations ---

     common Rota, Mtx

;     GenerateR, 3, gonio[1] ; 2theta rotation
;     vec2=Mtx##vec3

;     GenerateR, 2, gonio[0] ; Nu rotation
;     vec1=Mtx##vec2

     ;--- normalize ----

     sd=vec3/vlength(vec3)
     return, sd

end

;----------------------------------------------------------------------

function adetector_class::calculate_pixels_from_sd, sd, gonio

; calculates pixel coordinates from diffracted beam versor coordinates

; modified for GSECARS 11/30/2005

     Pi=acos(-1.0)
     vec1=[1.0,0.0,0.0]
     vec2=vec1
     vec3=vec1
     sd2=vec1
     pix=[0.0,0.0]
     sd1=sd/vlength(sd)

     ; apply detector rotation to the e1 vector (incident beam)

     common Rota, Mtx

     ;GenerateR, 3, gonio[1] ; 2theta rotation
     ;vec3=vec1

;     GenerateR, 2, gonio[0] ; Nu rotation
;     vec3=Mtx##vec2

     tth=ang_between_vecs(sd1,vec3)

     sdL=self.dist/cos(tth*pi/180.0)

     sd2=sd1*sdL
     vec2=self.dist*vec3
     dv=sd2-vec2


;     GenerateR, 3, -gonio[1] ; 2theta rotation
;     vec1=Mtx##dv

;     GenerateR, 2, -gonio[0] ; Nu rotation
;     dv=Mtx##vec1

     pix[1]=dv[2]/self.psizeY+self.beamY
     pix[0]=dv[1]/self.psizeX+self.beamX

     return, pix

end


;--------------------------------------------------------------------

; ---------------------------------------
; Caculates the diffracted beam versor coordinates
; from the reciprocal vector coordinates
; Checked 03/04/05
;----------------------------------------
function calculate_sd_from_hl, hl


 hl=hl/vlength(hl)
 Pi=acos(-1.0)
 s0=[1.0,0.0,0.0]
 al= ang_between_vecs(hl,s0)
 if al lt 90.0 then $
 begin
   hl=-hl
   al= ang_between_vecs(hl,s0)
   om=90.0-al
 endif
  om=al-90
 ;if hl[1] lt 0 then tth=om*2 else tth=-om*2
 tth=om*2
 ;hle=2*sin(tth*pi/180.0);/cos(om*pi/180.0)
 hle=sin(tth*pi/180.0)/cos(om*pi/180.0)
 h=hl*hle
 sd=s0+h

 return, sd

end

;----------------------------------------------------------


; ---------------------------------------
; Caculates the rec. vector coordinates
; from diffracted beam versor (hl) coordinates
; Checked 03/04/05
;----------------------------------------
function calculate_hl_from_sd, sd

 sd=sd/vlength(sd)
 s0=[1.0,0.0,0.0]
 hl=sd-s0

 return, hl

 end

;----------------------------------------------------------

; Calculates cartesian coordinates of reciprocal vector from pixel coordinates
; given goniometer position and wavelength
; p.Dera 11/01/2005

function adetector_class::calculate_XYZ_from_pixels_mono, pix, gonio, lambda

COMMON goniometer_objects, ogonio

     vec1=[0.0,0.0,0.0]
     vec2=vec1
     vec3=vec1
     sd=vec1
     hl=vec1
     pi=acos(-1.0)

    ; pix=[xpix, ypix]

     gonio[1]=gonio[1]+self.ttheta0

     gonio[0]=0.0
     gonio[1]=0.0
     gonio[2]=0.0
     ;gonio[3]=0.0
     ;gonio[4]=0.0
     gonio[5]=0.0

     sd=self->calculate_sd_from_pixels(pix, gonio)

     al=ang_between_vecs(sd,[1,0,0])
     hl=calculate_hl_from_sd(sd)
     hl=hl/vlength(hl)

     ; apply goniometer rotation


     vec2=ogonio->rotate_gonio(hl, [-gonio[2],-gonio[3],-gonio[4],-gonio[5]])

     ;th=calculate_theta_from_xyz(vec2)
     d=lambda/(2.0*sin(al*pi/180.0))
     vec3[0]=vec2[0]/float(d)
     vec3[1]=vec2[1]/float(d)
     vec3[2]=vec2[2]/float(d)

     return, vec3

 end

;----------------------------------------------------------

; calculates pixel coordinates for a given reciprocal vector described by
; cartesian coordinates XYZ, at a given goniometer position, at given wavelength.
; Since Bragg condition is not satisfied at just any gonio position, variable "Bragg"
; return the difference between the actual and requited theta.
; P.Dera 11/30/2005

function adetector_class::calculate_pixels_from_XYZ_mono, XYZ, gonio, lambda

COMMON goniometer_objects, ogonio

     vec1=[0.0,0.0,0.0]
     pix3=[0.0,0.0,0.0]
     pix=[0.0,0.0]

     vec2=vec1
     sd=vec1
     hl=vec1
     pi=acos(-1)

     ; apply goniometer rotation

     vec1=ogonio->rotate_gonio(XYZ, [gonio[2],gonio[3],gonio[4],gonio[5]])

     vec2=vec1/vlength(vec1)
     al=ang_between_vecs(vec2,[1,0,0])
     th=calculate_theta_from_xyz(vec2)

     pix3[0] = (90-al)-th

     sd=calculate_sd_from_hl(vec2)

     pix=self->calculate_pixels_from_sd(sd, gonio)
     pix3[1]=pix[0]
     pix3[2]=pix[1]

     return, pix3

 end

;----------------------------------------------------------

function calculate_theta_from_xyz, xyz

  vec1=[1.0,0.0,0.0]
  xyz1=xyz/vlength(xyz)
  ang=ang_between_vecs(vec1, xyz1)
  return, (abs(ang)-90.0)

end

;-----------------------------------------------------------




;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;--------------------------------------------------------------------
; Calculates pixel coordinates from rec. vector coordinates, allows to apply further
; goniometer rotation gonio
; it is assumed that the goniometer rotation is relative to the original position of xyz
;--------------------------------------------------------------------

function adetector_class::calculate_pixels_from_xyz, xyz, gonio

     sd=[0.0,0.0,0.0]
     pix=[0.0,0.0]
     xyz1=xyz/vlength(xyz)
     common Rota, Mtx
     GenerateR, 1, gonio[4]
     vec1=Mtx##xyz1
     GenerateR, 3, -gonio[3]-self.omega0 ; 03/24/05
     hl=Mtx##vec1
     sd=calculate_sd_from_hl(hl)

     pix=self->calculate_pixels_from_sd(sd, gonio)

     return, pix

 end

;--------------------------------------------------------------------
function adetector_class::calculate_pixels_from_xyz90, xyz, gonio

     sd=[0.0,0.0,0.0]
     pix=[0.0,0.0]
     xyz1=xyz/vlength(xyz)
     common Rota, Mtx
     GenerateR, 1, gonio[4]
     vec1=Mtx##xyz1
     GenerateR, 3, -gonio[3]-self.omega0 ; 03/24/05
     hl=Mtx##vec1
     sd=calculate_sd_from_hl(hl)
     pix=self->calculate_pixels_from_sd90(sd)
     return, pix

 end


;--------------------------------------------------------------------
; Calculates recip. versor coordinates from pixel coordinates
; (including non zero goinio position)
; Back rotation is applied, so the final vector coordinates are at zero
; goniometer position
;---------------------------------------------------------------------

;--------------------------------------------------------------------
function adetector_class::calculate_XYZ_from_pixels, xpix, ypix, gonio

     vec1=[0.0,0.0,0.0]
     vec2=vec1
     sd=vec1
     hl=vec1


     sd=self->calculate_sd_from_pixels([xpix, ypix], gonio)

     hl=calculate_hl_from_sd(sd)
     hl=hl/vlength(hl)
     common Rota, Mtx
     GenerateR, 3,gonio[3]+self.omega0
     vec1=Mtx##hl
     GenerateR, 1, -gonio[4]
     vec2=Mtx##vec1

     return, vec2

 end

;----------------------------------------------------------

; Calculates setting angles for EDD from pixel coordinates taken at gonio position
; gonio[0] is the EDD position
; Two solutions are calculated, that differ by 180 deg rotation on chi

function adetector_class::calculate_EDDangles_from_pixels, xpix, ypix, gonio
     zeros=[self.ttheta0,self.omega0,0.0]
     hl=self->calculate_XYZ_from_pixels(xpix, ypix, gonio)
     outgonio=calculate_EDDangles_from_xyz(hl, zeros, gonio[1])
     return, outgonio

 end

;--------------------------------------------------------------------


; Calculates setting angles for EDD from pixel coordinates taken at gonio position
; gonio[0] is the EDD position
; Two solutions are calculated, that differ by 180 deg rotation on chi

function adetector_class::calculate_pixels_from_EDDangles, gonioEDD, gonio

     zeros=[self.ttheta0,self.omega0,0.0]
     hl2=calculate_xyz_from_EDD(gonioEDD, zeros)
     pix=self->calculate_pixels_from_xyz(hl2, gonio)
     return, pix

 end

;--------------------------------------------------------------------

; ---------------------------------------
; Caculates the rec. vector coordinates
; from diffracted beam versor (hl) coordinates
; Checked 03/04/05
;----------------------------------------

function calculate_sd_from_EDD, gonio, zeros

 Pi=acos(-1.0)
 s0=[1.0,0.0,0.0]

 common Rota, Mtx
 GenerateR, 3, -gonio[1]-zeros[0]
 sd=Mtx##s0

 return, sd

end

;------------------------------------------

function calculate_xyz_from_EDD, gonioEDD, zeros

     common Rota, Mtx

     Pi=acos(-1.0)
     hl=[0.0,0.0,0.0]
     s0=[1.0,0.0,0.0]
     pix=[0.0,0.0]
     sd=s0
     tth0=0
     sd=calculate_sd_from_EDD(gonioEDD,zeros)
     hl=calculate_hl_from_sd(sd)

     GenerateR, 3, (zeros[1])
     Mom0 =Mtx
     iMom0=invert(Mtx)

     GenerateR, 2, (zeros[2])
     Mch0 =Mtx
     iMch0=invert(Mtx)

     GenerateR, 3, (gonioEDD[3])
     Mom =Mtx

     GenerateR, 1, -gonioEDD[4]
     Mch=Mtx

     ; hl2=  Mch0 ## Mom0 ##Mch ## iMom0 ## iMch0 ## Mom ## hl
     hl2= Mch0 # Mom # iMch0 # Mom0 # Mch # iMom0 ## hl

     ;if hl2[0] gt 0 then hl2=-hl2
     return, hl2
end

;-------------------------------------------------------

function calculate_EDDangles_from_xyz, hl, zeros, tth0

     Pi=acos(-1.0)
     s0=[1.0,0.0,0.0]
     outgonio=fltarr(2,3)

     outgonio[0,2]=(180/pi)*(atan(hl[2],hl[1]))    ; chi 1
     outgonio[1,2]=(180/pi)*(atan(hl[2],hl[1])+pi) ; chi 2

    ; have to check on which side the

     common Rota, Mtx
     GenerateR, 1, outgonio[0,2]
     hl1=Mtx##hl

     GenerateR, 1, outgonio[1,2]
     hl2=Mtx##hl
     al1=ang_between_vecs(s0,hl1)

     if al1 lt 90.0 then om1=90.0-al1 else om1=al1-90
     if hl1[1] gt 0 then om1=-om1

     tth1=(tth0-2*om1+zeros[0])
     outgonio[0,1]=((tth1)/2-zeros[1])
     al2=ang_between_vecs(s0,hl2)
     if al2 lt 90.0 then om2=90.0-al2 else om2=al2-90
     if hl2[1] gt 0 then om2=-om2
     tth2=(tth0-2*om2+zeros[0])
     outgonio[1,1]=((tth2)/2-zeros[1])

     outgonio[0,0]=tth0
     outgonio[1,0]=tth0
     return, outgonio
 end

;----------------------------------------------------------

