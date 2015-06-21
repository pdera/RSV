
pro CLASS_Area_detector_DC_settings

 ad={c_area_detector_DC_settings, $
        EPICS_name    : '' , $ ;
        File_path     : '' , $ ;
        Base_filename : '' , $ ;
        Next_file     : 0L , $ ;
        Exposure_time : 0.0, $ ;
        BinningX      : 0  , $ ;
        BinningY      : 0   $ ;
        }
end

pro CLASS_Area_detector
 ccd={c_area_detector, $
        parameters   : area_detector_parameters_CLASS, $
        DCsettings   : c_area_detector_DC_settings }
end

pro c_area_detector_settings::setEPICSname, name
 self.EPICS_name=name
end

function c_area_detector_settings::getEPICSname
 return, self.EPICS_name
end

function initialize_CCD, EIPCSname
  self.parameters=obj_new('c_area_detector_parameters')
  self.DCsettings=obj_new('c_area_detector_DC_settings')
  CCD_detector=obj_new('c_area_detector_settings')
  CCD_detector->setEPICSname, '13ccd:det1:'
end

pro CLASS_Area_detector::expose_CCD
  aname=self.DCsettings->get_EPICSname()
  aname=aname+'AcquireCLBK'
  t = caput(aname, 1)
end

