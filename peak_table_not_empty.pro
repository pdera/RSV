pro peak_table_not_empty

common peakread, prchoice
COMMON Main_window, WIDGET_OLA_analyzer_0

  WID_BASE_0 = Widget_Base(UNAME='WID_BASE_0'  $
      ,XOFFSET=400 ,YOFFSET=400 ,SCR_XSIZE=300 ,SCR_YSIZE=200  $
      ,TITLE='Message' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LABEL_0 = Widget_Label(WID_BASE_0, UNAME='WID_LABEL_0'  $
      ,XOFFSET=82 ,YOFFSET=52 ,/ALIGN_LEFT ,VALUE='Peak table is not'+ $
      ' empty!')


  WID_append = Widget_Button(WID_BASE_0, UNAME='WID_BUTTON_0'  $
      ,XOFFSET=20 ,YOFFSET=110 ,SCR_XSIZE=77 ,SCR_YSIZE=38  $
      ,/ALIGN_CENTER ,VALUE='Append')


  WID_cancel = Widget_Button(WID_BASE_0, UNAME='WID_BUTTON_1'  $
      ,XOFFSET=195 ,YOFFSET=108 ,SCR_XSIZE=77 ,SCR_YSIZE=38  $
      ,/ALIGN_CENTER ,VALUE='Cancel')


  WID_overwrite = Widget_Button(WID_BASE_0, UNAME='WID_BUTTON_2'  $
      ,XOFFSET=109 ,YOFFSET=109 ,SCR_XSIZE=77 ,SCR_YSIZE=38  $
      ,/ALIGN_CENTER ,VALUE='Overwrite')

  Widget_Control, /REALIZE, WID_BASE_0

  XManager, 'peak_table_not_empty', WID_BASE_0

end

pro peak_table_not_empty_event, ev

  common peakread, prchoice

  WIDGET_CONTROL, ev.top, GET_UVALUE=textwid
  WIDGET_CONTROL, ev.id, GET_VALUE=uval
  case uval of
  'Append': begin
              prchoice=1
              WIDGET_CONTROL, ev.top , /DESTROY
            end
  'Cancel': begin
              prchoice=3
              WIDGET_CONTROL, ev.top , /DESTROY
            end
  'Overwrite':begin
              prchoice=2
              WIDGET_CONTROL, ev.top , /DESTROY
            end
  endcase
end