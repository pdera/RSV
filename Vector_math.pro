 ; -------------------------------------------------------------------------
; -----------------------  Generates rotation matrix            -----------
; -------------------------------------------------------------------------

; Rotation matrix about axisnumber (1=x, 2=y, 3=z) by angle, result passed in
; Rota, Mtx

; -------------------------------------------------------------------------

function distance, x1,y1,x2,y2
return, sqrt((x1-x2)^2+(y1-y2)^2)
end

;----------------------------------

pro verify_rot, rot
  if (n_elements(rot) ne 9) or $
     (finite(total(rot)) ne 1) or $
     ((total(rot)) eq 0) then rot=[[1,0,0],[0,1,0],[0,0,1]]
end

;------------------------

pro GenerateR, axisnumber, angle
common Rota, Mtx
Mtx=FLTARR(3,3)
       pi=acos(-1)
       s=angle*Pi/180.0;
       case axisnumber of $
       1: begin
            Mtx[0,0]=1.0;
            Mtx[0,1]=0.0;
            Mtx[0,2]=0.0;
            Mtx[1,0]=0.0;
            Mtx[1,1]=cos(s);
            Mtx[1,2]=-sin(s);
            Mtx[2,0]=0.0;
            Mtx[2,1]=sin(s);
            Mtx[2,2]=cos(s);
          end;
       2: begin
            Mtx[0,0]=cos(s);
            Mtx[0,1]=0.0;
            Mtx[0,2]=sin(s);
            Mtx[1,0]=0.0;
            Mtx[1,1]=1.0;
            Mtx[1,2]=0.0;
            Mtx[2,0]=-sin(s);
            Mtx[2,1]=0.0;
            Mtx[2,2]=cos(s);
          end;
       3: begin
            Mtx[0,0]=cos(s);
            Mtx[0,1]=sin(s);
            Mtx[0,2]=0.0
            Mtx[1,0]=-sin(s);
            Mtx[1,1]=cos(s);
            Mtx[1,2]=0.0
            Mtx[2,0]=0.0;
            Mtx[2,1]=0.0;
            Mtx[2,2]=1.0
          end
       endcase
end

; -------------------------------------------------------------------------
; -----------------------  Calculates vector length             -----------
; -------------------------------------------------------------------------

FUNCTION vlength, vec
 a=sqrt(vec[0]*vec[0]+vec[1]*vec[1]+vec[2]*vec[2])
 return, a
end

; -------------------------------------------------------------------------
; -----------------------  Calculates scalar product            -----------
; -------------------------------------------------------------------------

FUNCTION dotprod, vec1, vec2
 a=(vec1[0]*vec2[0]+vec1[1]*vec2[1]+vec1[2]*vec2[2])
 return, a
end


;================================================================
;   ---------------   Sign function -----------------------
;================================================================

function sign, x
  a=0
  if x lt 0 then a=-1 $
  else if  x gt 0 then a=1 $
  else a=0
  return, a
end

;================================================================
;   ---------------   Angle between vectors function -----------------------
;================================================================

function ang_between_vecs, vec1, vec2
  Pi=acos(-1.0)
  a=0.0
  a=dotprod(vec1, vec2)/(vlength(vec1)*vlength(vec2))
  a=abs(acos(a)*180.0/Pi)
  return, a
end



pro Vector_math
end