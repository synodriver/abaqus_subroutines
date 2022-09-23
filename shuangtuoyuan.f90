SUBROUTINE DFLUX(FLUX, SOL, KSTEP, KINC, TIME, NOEL, NPT, COORDS, JLTYP, TEMP, PRESS, SNAME)
    INCLUDE 'ABA_PARAM.INC'
    ! https://www.bilibili.com/video/BV19S4y1U73T/?spm_id_from=333.788&vd_source=5ea7f682263b12f4a66659089a2aee38
    real(kind = 8), dimension(2) :: FLUX
    real(kind = 8), dimension(2) :: TIME
    real(kind = 8), dimension(3) :: COORDS
    character(len = 80) :: SNAME
    real(kind = 8), parameter :: pi = 3.1415927


    ! 这些需要设置 todo 好像还要一个焊接面的法向量
    real(kind = 8) :: a1, b, c, a2 ! 热源模型的四个参数  w/m3
    real(kind = 8) :: START_POINT_X, START_POINT_Y, START_POINT_Z, END_POINT_X, END_POINT_Y, END_POINT_Z, TOTAL_TIME ! 起点 终点 总时间
    real(kind = 8), dimension(3) :: faxiangliang  ! 焊接面的法向量

    real(kind = 8), dimension(3) :: v, distance, current_position, START_POINT, END_POINT, y_axis
    real(kind = 8) :: rr ! 积分点与热源距离的平方
    real(kind = 8) :: cosx, sinx, yuzjiajiao, yuyjiajiao! 积分点到热源这个路径 与 热源路径夹角的cos  sin  与z轴夹角的cos  与y轴夹角的cos
    real(kind = 8) :: x, y, z  !距离热源中心，焊接方向上的距离 融宽方向 熔深方向
    ! -----------------设置参数
    a1 = 1.0
    b = 1.0
    c = 1.0
    a2 = 1.0
    START_POINT_X = 0.0
    START_POINT_Y = 0.0
    START_POINT_Z = 0.0
    END_POINT_X = 1.0
    END_POINT_Y = 1.0
    END_POINT_Z = 1.0
    TOTAL_TIME = 10.0
    faxiangliang(1) = 0.0
    faxiangliang(2) = 0.0
    faxiangliang(3) = 1.0

    ! ------------- 参数设置完毕
    START_POINT(1) = START_POINT_X
    START_POINT(2) = START_POINT_Y
    START_POINT(3) = START_POINT_Z
    END_POINT(1) = END_POINT_X
    END_POINT(2) = END_POINT_Y
    END_POINT(3) = END_POINT_Z
    distance = END_POINT - START_POINT ! 就是起点到终点的向量
    !    distance(1) = END_POINT_X - START_POINT_X ! X距离
    !    distance(2) = END_POINT_Y - START_POINT_Y ! Y距离
    !    distance(3) = END_POINT_Z - START_POINT_Z ! z距离  (distance_x, distance_y, distance_z) 就是起点到终点的向量

    v = distance / TOTAL_TIME  ! 热源速度向量 mm/s
    current_position = v * TIME(1) + START_POINT ! 当前时间的热源坐标

    cosx = dot_product(COORDS - current_position, distance) / (sqrt(dot_product(COORDS - current_position, &
            COORDS - current_position)) * sqrt(dot_product(distance, distance)))
    sinx = sqrt(1 - cosx**2)

    rr = dot_product(COORDS - current_position, COORDS - current_position)
    x = sqrt(rr) * cosx ! 距离热源中心，焊接方向上的距离

    yuzjiajiao = dot_product(COORDS - current_position, faxiangliang) / &
            (sqrt(dot_product(COORDS - current_position, COORDS - current_position)) * &
                    sqrt(dot_product(faxiangliang, faxiangliang)))
    z = sqrt(rr) * yuzjiajiao

    y_axis(1) = faxiangliang(2) * distance(3) - faxiangliang(3) * distance(2)
    y_axis(2) = faxiangliang(3) * distance(1) - faxiangliang(1) * distance(3)
    y_axis(3) = faxiangliang(1) * distance(2) - faxiangliang(2) * distance(1)

    yuyjiajiao = dot_product(COORDS - current_position, y_axis) / &
            (sqrt(dot_product(COORDS - current_position, COORDS - current_position)) * &
                    sqrt(dot_product(y_axis, y_axis)))
    ! y轴方向就是faxiangliang和distance的向量积了
    y = sqrt(rr) * yuyjiajiao
    ! todo 求y和z的值 其实就是一个坐标系变换
    if (x>0) then
        if ((x**2 / a1**2 + y**2 / b**2 + z**2 / c**2) <=1) then  ! 确保在椭球范围内
            FLUX(1) = 6000 * 0.75 * 15 * 80 * sqrt(3.0) / a1 / b / c / pi / &
                    sqrt(pi) * exp(-3 * (x**2 / a1**2 + y**2 / b**2 + z**2 / c**2))
        end if
    else
        if ((x**2 / a2**2 + y**2 / b**2 + z**2 / c**2) <=1) then  ! 确保在椭球范围内
            FLUX(1) = 6000 * 0.75 * 15 * 80 * sqrt(3.0) / a2 / b / c / pi / &
                    sqrt(pi) * exp(-3 * (x**2 / a2**2 + y**2 / b**2 + z**2 / c**2))
        end if
    end if
END SUBROUTINE DFLUX