
; pro prepopulate_widget, wid
; pro save_cal, fn, oad, wv
; pro load_cal, fn, oad, wv
; function Read_calibration_from_widget, wid
; pro Write_calibration_to_widget, wid
; WID_calibration_aux
; function get_wv_from_widget, wid

;-----------------------------------
;-----------------------------------
;--
function get_ID_WID_BASE_calibration
 COMMON guis, WID_BASE_calibration
 return, WID_BASE_calibration
end

function get_wv_from_widget, wid
  id_07=widget_info(wid, FIND_BY_UNAME='WID_TEXT_wavelength')
  widget_control, id_07, get_value=val
  return, float(val)
end
;---------------------------------

pro prepopulate_widget, wid

  id_00=widget_info(wid, FIND_BY_UNAME='WID_TEXT_twist')
  id_01=widget_info(wid, FIND_BY_UNAME='WID_TEXT_psizeX')
  id_02=widget_info(wid, FIND_BY_UNAME='WID_TEXT_psizeY')
  id_03=widget_info(wid, FIND_BY_UNAME='WID_TEXT_distance')
  id_04=widget_info(wid, FIND_BY_UNAME='WID_TEXT_beamx')
  id_05=widget_info(wid, FIND_BY_UNAME='WID_TEXT_beamy')
  id_06=widget_info(wid, FIND_BY_UNAME='WID_TEXT_rotation')
  id_07=widget_info(wid, FIND_BY_UNAME='WID_TEXT_wavelength')
  id_08=widget_info(wid, FIND_BY_UNAME='WID_TEXT_tilt')
  id_09=widget_info(wid, FIND_BY_UNAME='WID_TEXT_phi')
  id_10=widget_info(wid, FIND_BY_UNAME='WID_TEXT_alpha')
  id_11=widget_info(wid, FIND_BY_UNAME='WID_TEXT_2theta')
  id_12=widget_info(wid, FIND_BY_UNAME='WID_TEXT_kappa')
  id_13=widget_info(wid, FIND_BY_UNAME='WID_TEXT_omega')
  id_14=widget_info(wid, FIND_BY_UNAME='WID_TEXT_nopixX')
  id_15=widget_info(wid, FIND_BY_UNAME='WID_TEXT_nopixY')


  widget_control, id_07, set_value='0.3344' ; wavelength
  widget_control, id_14, set_value='2048' ;   nopixX
  widget_control, id_15, set_value='2048' ;   nopixY

end

;--------------------------------

pro save_cal, fn, oad, wv
if fn ne '' then $
begin
 ad=oad->get_object()
 free_lun,2
 openw, 2, fn
 PRINTF, 2,  ad.psizex
 PRINTF, 2,  ad.psizey
 PRINTF, 2,  ad.dist
 PRINTF, 2,  wv
 PRINTF, 2,  ad.beamx
 PRINTF, 2,  ad.beamy
 PRINTF, 2,  ad.tiltom
 PRINTF, 2,  ad.tiltch

 PRINTF, 2,  ad.angle ; twist
 PRINTF, 2,  ad.alpha ; alpha


 close, 2
endif
end

;----------------------

pro load_cal, fn, oad, wv
if fn ne '' then $
begin
 on_ioerror, err
 valid=0
 ad=oad->get_object()
 a=0.0
 free_lun,2
 openr, 2, fn
 readF, 2,aa
 ad.psizex=aa
 readF, 2, aa
 ad.psizey=aa
 readF, 2,  aa
 ad.dist=aa
 readF, 2,  aa
 wv=aa
 readF, 2,  aa
 ad.beamx=aa
 readF, 2,  aa
 ad.beamy=aa
 readF, 2,  aa
 ad.tiltom=aa
 readF, 2,  aa
 ad.tiltch=aa

 if not eof(2) then $ ; format including twist and alpha
 begin
  readF, 2,  aa
  ad.angle=aa
  readF, 2,  aa
  ad.alpha=aa
 endif else $
 begin
  ad.angle=0.000
  ad.alpha=0.000
 endelse

 close, 2
 oad->set_object, ad
 valid=1
 endif
 err: IF ~ valid THEN re=dialog_message('Improper file format')
end

;----------------------



function Read_calibration_from_widget, wid

common oadet,oad, oad1, oad2

  id_00=widget_info(wid, FIND_BY_UNAME='WID_TEXT_twist')
  id_01=widget_info(wid, FIND_BY_UNAME='WID_TEXT_psizeX')
  id_02=widget_info(wid, FIND_BY_UNAME='WID_TEXT_psizeY')
  id_03=widget_info(wid, FIND_BY_UNAME='WID_TEXT_distance')
  id_04=widget_info(wid, FIND_BY_UNAME='WID_TEXT_beamx')
  id_05=widget_info(wid, FIND_BY_UNAME='WID_TEXT_beamy')
  id_06=widget_info(wid, FIND_BY_UNAME='WID_TEXT_rotation')
  id_07=widget_info(wid, FIND_BY_UNAME='WID_TEXT_wavelength')
  id_08=widget_info(wid, FIND_BY_UNAME='WID_TEXT_tilt')
  id_09=widget_info(wid, FIND_BY_UNAME='WID_TEXT_phi')
  id_10=widget_info(wid, FIND_BY_UNAME='WID_TEXT_alpha')
  id_11=widget_info(wid, FIND_BY_UNAME='WID_TEXT_2theta')
  id_12=widget_info(wid, FIND_BY_UNAME='WID_TEXT_kappa')
  id_13=widget_info(wid, FIND_BY_UNAME='WID_TEXT_omega')
  id_14=widget_info(wid, FIND_BY_UNAME='WID_TEXT_nopixX')
  id_15=widget_info(wid, FIND_BY_UNAME='WID_TEXT_nopixY')

  ad=oad->get_object()

  widget_control, id_03, get_value=val
  ad.dist   =float(val)

  widget_control, id_04, get_value=val
  ad.beamx  =float(val)

  widget_control, id_05, get_value=val
  ad.beamy  =float(val)

  widget_control, id_01, get_value=val
  ad.psizex =float(val)

  widget_control, id_02, get_value=val
  ad.psizey =float(val)

  widget_control, id_14, get_value=val
  ad.nopixx =Long(val)

  widget_control, id_15, get_value=val
  ad.nopixY =Long(val)

  widget_control, id_00, get_value=val
  ad.angle  =float(val)

  widget_control, id_13, get_value=val
  ad.omega0 =float(val)

  widget_control, id_11, get_value=val
  ad.ttheta0=float(val)

  widget_control, id_06, get_value=val
  ad.tiltom =float(val)

  widget_control, id_08, get_value=val
  ad.tiltch =float(val)

  oad->set_object, ad

  return, oad

end
;--------------------------------------------
pro Write_calibration_to_widget

wid=get_ID_WID_BASE_calibration()

common oadet

  id_00=widget_info(wid, FIND_BY_UNAME='WID_TEXT_twist')
  id_01=widget_info(wid, FIND_BY_UNAME='WID_TEXT_psizeX')
  id_02=widget_info(wid, FIND_BY_UNAME='WID_TEXT_psizeY')
  id_03=widget_info(wid, FIND_BY_UNAME='WID_TEXT_distance')
  id_04=widget_info(wid, FIND_BY_UNAME='WID_TEXT_beamx')
  id_05=widget_info(wid, FIND_BY_UNAME='WID_TEXT_beamy')
  id_06=widget_info(wid, FIND_BY_UNAME='WID_TEXT_rotation')
  id_07=widget_info(wid, FIND_BY_UNAME='WID_TEXT_wavelength')
  id_08=widget_info(wid, FIND_BY_UNAME='WID_TEXT_tilt')
  id_09=widget_info(wid, FIND_BY_UNAME='WID_TEXT_phi')
  id_10=widget_info(wid, FIND_BY_UNAME='WID_TEXT_alpha')
  id_11=widget_info(wid, FIND_BY_UNAME='WID_TEXT_2theta')
  id_12=widget_info(wid, FIND_BY_UNAME='WID_TEXT_kappa')
  id_13=widget_info(wid, FIND_BY_UNAME='WID_TEXT_omega')
  id_14=widget_info(wid, FIND_BY_UNAME='WID_TEXT_nopixX')
  id_15=widget_info(wid, FIND_BY_UNAME='WID_TEXT_nopixY')

 ad=oad->get_object()

  widget_control, id_03, set_value=string(ad.dist)
  widget_control, id_04, set_value=string(ad.beamx)
  widget_control, id_05, set_value=string(ad.beamy)
  widget_control, id_01, set_value=string(ad.psizex)
  widget_control, id_02, set_value=string(ad.psizey)
 ; widget_control, id_14, set_value=string(ad.nopixx)
 ; widget_control, id_15, set_value=string(ad.nopixY)
  widget_control, id_00, set_value=string(ad.angle)
  widget_control, id_13, set_value=string(ad.omega0)
  widget_control, id_11, set_value=string(ad.ttheta0)
  widget_control, id_06, set_value=string(ad.tiltom)
  widget_control, id_08, set_value=string(ad.tiltch)

end
;--------------------------------------------

pro WID_calibration_aux
end