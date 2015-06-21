
pro WIDGET_triclinic_warning


  COMMON tricl, WID_tricl, WID_BUTTON_tri_proceed, WID_BUTTON_tri_cancel

;  Resolve_Routine, 'WIDGET_triclinic_warning_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  WID_tricl = Widget_Base( UNAME='WID_tricl'  $
      ,XOFFSET=5 ,YOFFSET=5 ,SCR_XSIZE=446 ,SCR_YSIZE=199  $
      ,TITLE='Warning' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LABEL_0 = Widget_Label(WID_tricl, UNAME='WID_LABEL_0'  $
      ,XOFFSET=116 ,YOFFSET=21 ,/ALIGN_LEFT ,VALUE='Triclinic crystal'+ $
      ' system detected!')


  WID_LABEL_1 = Widget_Label(WID_tricl, UNAME='WID_LABEL_1'  $
      ,XOFFSET=21 ,YOFFSET=58 ,/ALIGN_LEFT ,VALUE='Computation of'+ $
      ' alternative setings in triclinic system may take as long as 2 '+ $
      'min!')


  WID_BUTTON_tri_proceed = Widget_Button(WID_tricl, UNAME='WID_BUTTON_0'  $
      ,XOFFSET=70 ,YOFFSET=95 ,SCR_XSIZE=120 ,SCR_YSIZE=46  $
      ,/ALIGN_CENTER ,VALUE='Proceed')


  WID_BUTTON_tri_cancel = Widget_Button(WID_tricl, UNAME='WID_BUTTON_1'  $
      ,XOFFSET=196 ,YOFFSET=95 ,SCR_XSIZE=120 ,SCR_YSIZE=46  $
      ,/ALIGN_CENTER ,VALUE='Cancel')

  Widget_Control, /REALIZE, WID_tricl
  tricl_set_uval

  XManager, 'WIDGET_triclinic_warning', WID_tricl;  /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
;pro WIDGET_triclinic_warning, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
;  WID_tricl, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
;end
