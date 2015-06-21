pro CLASS_crystal

  COMMON CLASS_crystal_reference, cryst

  cryst={crystal_CLASS, $
          ;         name               : 'AAAAA',          $ ; crystal name
          ;         chemical_name      : 'AAAAA',          $ ; chemical compound name
          ;         chemical_formula   : 'AAAAA',          $ ; chemical formula
                  crystal_size       : FLTARR(3),   $ ; crystal size in mm
                  cell_parameters    : FLTARR(6),   $ ; unit cell parameters in A
                  cell_esds          : FLTARR(6),   $ ; cell parametrs esd in A
                  UB_matrix          : FLTARR(3,3), $ ; orientation matrix
                  position           : FLTARR(3),   $ ; crystal position with respect to gasket hole center
                  pressure           : 0.0,         $ ; pressure of data collection in GPa
                  temperature        : 0.0,         $ ; temperature for data collection in K
                  cell_volume      : 0.0 $
          }

end

;-------------------------------------

function crystal_CLASS::get_object

COMMON CLASS_crystal_reference, cryst

                  cryst.crystal_size    = self.crystal_size
                  cryst.cell_parameters = self.cell_parameters
                  cryst.cell_esds       = self.cell_esds
                  cryst.UB_matrix       = self.UB_matrix
                  cryst.position        = self.position
                  cryst.pressure        = self.pressure
                  cryst.temperature     = self.temperature
                  cryst.cell_volume     = self.cell_volume

return, cryst

end

;-------------------------------------

pro crystal_CLASS::set_object, cryst

                  self.crystal_size    = cryst.crystal_size
                  self.cell_parameters = cryst.cell_parameters
                  self.cell_esds       = cryst.cell_esds
                  self.UB_matrix       = cryst.UB_matrix
                  self.position        = cryst.position
                  self.pressure        = cryst.pressure
                  self.temperature     = cryst.temperature
                  self.cell_volume     = cryst.cell_volume

end

;-------------------------------------

pro crystal_CLASS::write_object_to_disk, fname


     COMMON CLASS_crystal_reference, cryst

     cryst=self->get_object()
     FREE_LUN, 2
     OPENW, 2, fname
     writeu, 2, cryst
     CLOSE, 2
     FREE_LUN, 2

end

;-------------------------------------

pro crystal_CLASS::read_object_from_disk, fname


     COMMON CLASS_crystal_reference, cryst

     cryst=self->get_object()
     FREE_LUN, 2
     OPENR, 2, fname
     readu, 2, cryst
     CLOSE, 2
     FREE_LUN, 2
     self->set_object, cryst
     a=display_cell(self)
end

;-------------------------------------
