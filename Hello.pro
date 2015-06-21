
pro Widget_Hello, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_


COMMON Hello_controls, WID_BUTTON_Hello_start


@Main_Component_Common

  Resolve_Routine, 'Hello_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  WID_BASE_0 = Widget_Base( GROUP_LEADER=W_RSV, UNAME='WID_BASE_0'  $
      ,XOFFSET=5 ,YOFFSET=5 ,SCR_XSIZE=533 ,SCR_YSIZE=328  $
      ,TITLE='RSV' ,SPACE=3 ,XPAD=3 ,YPAD=3,/FLOATING)


  WID_LABEL_0 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_0'  $
      ,XOFFSET=35 ,YOFFSET=41 ,/ALIGN_LEFT ,VALUE='RSV: Reciprocal'+ $
      ' Space Viewer   Ver 2.2a, March 27, 2007')


  WID_LABEL_1 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_1'  $
      ,XOFFSET=97 ,YOFFSET=80 ,SCR_XSIZE=333 ,SCR_YSIZE=15  $
      ,/ALIGN_LEFT ,VALUE='Program for visualization and indexing of'+ $
      ' the reciprocal space vectors')


  WID_LABEL_2 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_2'  $
      ,XOFFSET=97 ,YOFFSET=99 ,SCR_XSIZE=333 ,SCR_YSIZE=15  $
      ,/ALIGN_LEFT ,VALUE='from single-crystal x-ray diffraction'+ $
      ' data.')


  WID_LABEL_3 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_3'  $
      ,XOFFSET=102 ,YOFFSET=151 ,/ALIGN_LEFT ,VALUE='Developed by:')


  WID_LABEL_4 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_4'  $
      ,XOFFSET=102 ,YOFFSET=171 ,/ALIGN_LEFT ,VALUE='Przemek Dera'+ $
      ' (pdera@gl.ciw.edu)')


  WID_LABEL_5 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_5'  $
      ,XOFFSET=101 ,YOFFSET=187 ,/ALIGN_LEFT ,VALUE='Geophysical Lab,'+ $
      ' CIW')


  WID_LABEL_6 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_6'  $
      ,XOFFSET=8 ,YOFFSET=240 ,/ALIGN_LEFT ,VALUE='Sponsoring:')


  WID_LABEL_7 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_7'  $
      ,XOFFSET=8 ,YOFFSET=258 ,/ALIGN_LEFT ,VALUE='Development of'+ $
      ' this program has been funded by NSF DMR')


  WID_LABEL_8 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_8'  $
      ,XOFFSET=7 ,YOFFSET=275 ,/ALIGN_LEFT ,VALUE='Major Research'+ $
      ' Instrumentation  program under grant no.  DMR0521179')


  WID_BUTTON_Hello_start = Widget_Button(WID_BASE_0, UNAME='WID_BUTTON_0'  $
      ,XOFFSET=329 ,YOFFSET=141 ,SCR_XSIZE=154 ,SCR_YSIZE=78  $
      ,/ALIGN_CENTER ,VALUE='Start')

  Widget_Control, /REALIZE, WID_BASE_0

  XManager, 'Widget_Hello', WID_BASE_0, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro Hello, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  Widget_Hello, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
