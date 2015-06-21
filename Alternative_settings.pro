;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on: 02/21/2006 11:22.51
;
pro WIDGET_Alternative_settings_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

    else:
  endcase

end

pro WIDGET_Alternative_settings, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

COMMON alternative, AWID_BUTTON_choose, AWID_BUTTON_Close, AWID_LIST_0


  Resolve_Routine, 'Alternative_settings_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  WIDGET_Alternative_settings = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='WIDGET_Alternative_settings' ,XOFFSET=5 ,YOFFSET=5  $
      ,SCR_XSIZE=531 ,SCR_YSIZE=349 ,TITLE='Alternative settings'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)


  AWID_LIST_0 = Widget_List(WIDGET_Alternative_settings,  $
      UNAME='AWID_LIST_0' ,XOFFSET=13 ,YOFFSET=14 ,SCR_XSIZE=422  $
      ,SCR_YSIZE=295 ,XSIZE=11 ,YSIZE=2)


  AWID_BUTTON_choose = Widget_Button(WIDGET_Alternative_settings,  $
      UNAME='AWID_BUTTON_choose' ,XOFFSET=445 ,YOFFSET=232  $
      ,SCR_XSIZE=73 ,SCR_YSIZE=37 ,/ALIGN_CENTER ,VALUE='Choose')


  AWID_BUTTON_Close = Widget_Button(WIDGET_Alternative_settings,  $
      UNAME='AWID_BUTTON_Close' ,XOFFSET=444 ,YOFFSET=272  $
      ,SCR_XSIZE=73 ,SCR_YSIZE=37 ,/ALIGN_CENTER ,VALUE='Close')

  Widget_Control, /REALIZE, WIDGET_Alternative_settings

  XManager, 'WIDGET_Alternative_settings', WIDGET_Alternative_settings, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro Alternative_settings, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  WIDGET_Alternative_settings, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
