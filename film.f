SUBROUTINE FILM(H,SINK,TEMP,KSTEP,KINC,TIME,NOEL,NPT, COORDS,JLTYP,FIELD,NFIELD,SNAME,NODE,AREA)

INCLUDE 'ABA_PARAM.INC'

real(8), dimension(2) :: H, TIME
real(8) :: SINK, AREA, TEMP 
real(8), dimension(3) :: COORDS
integer :: NFIELD, KSTEP, KINC, JLTYP, NODE, NOEL, NPT
real(8), dimension(NFIELD) :: FIELD
character(80) :: SNAME


H(1) = COORDS(1) + COORDS(2) + COORDS(3)  !  三个坐标的函数 现在的意思是对流换热系数是 x+y+z改成你要的



END SUBROUTINE FILM