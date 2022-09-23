SUBROUTINE DFLUX(FLUX, SOL, KSTEP, KINC, TIME, NOEL, NPT, COORDS, JLTYP, TEMP, PRESS, SNAME)
    INCLUDE 'ABA_PARAM.INC'

    real(kind=8), dimension(2) :: FLUX
    real(kind=8), dimension(2) :: TIME
    real(kind=8), dimension(3) :: COORDS
    character(len = 80) :: SNAME
    ! 这里需要设置
    real(kind=8) :: START_POINT_X, START_POINT_Y, START_POINT_Z, END_POINT_X, END_POINT_Y, END_POINT_Z, TOTAL_TIME, r0 ! 起点 终点 总时间 热源半径


!    real(kind=8) :: v_x, v_y, distance_x, distance_y, r2, current_position_x,current_position_y
    real(kind = 8), dimension(3) :: v, distance, current_position, START_POINT, END_POINT
    real(kind=8) :: rr  ! 当前积分点与热源位置之间的距离的平方
    ! -----------------设置参数
    r0 = 5.0
    START_POINT_X = 0.0
    START_POINT_Y = 50.0
    START_POINT_Z = 4  ! todo 我那个是0 吗 要看看

    END_POINT_X = 200.0
    END_POINT_Y = 50.0
    END_POINT_Z = 4
    TOTAL_TIME = 48
    ! ------------- 参数设置完毕
    START_POINT(1) = START_POINT_X
    START_POINT(2) = START_POINT_Y
    START_POINT(3) = START_POINT_Z
    END_POINT(1) = END_POINT_X
    END_POINT(2) = END_POINT_Y
    END_POINT(3) = END_POINT_Z

    distance = END_POINT - START_POINT ! 就是起点到终点的向量

    v = distance / TOTAL_TIME  ! 热源速度向量 mm/s

    current_position = v * TIME(1) + START_POINT ! 当前时间的热源坐标
    rr = dot_product(COORDS - current_position, COORDS - current_position)
!    r2 = (COORDS(1) - current_position_x)**2 + (COORDS(2) - current_position_y)**2
    ! print *, COORDS(1), COORDS(2), r2, TIME(1), TIME(2)
    if (rr <= (r0 ** 2)) then
        FLUX(1) = 3000 * 0.75 * 15 * 80 / (3.14159 * 5 * 5) * exp(-3 * rr / (r0 ** 2))
    end if
    ! user coding to define FLUX(1) and FLUX(2)

END SUBROUTINE