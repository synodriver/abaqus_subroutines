! Created by LAB on 2022/12/13.

subroutine UMESHMOTION(UREF, ULOCAL, NODE, NNDOF, LNODETYPE, ALOCAL, NDIM, TIME, DTIME, PNEWDT, KSTEP, KINC, KMESHSWEEP, JMATYP, JGVBLOCK, LSMOOTH)
    INCLUDE 'ABA_PARAM.INC'
    real(8) :: UREF, DTIME, PNEWDT
    integer :: NDIM, NODE, NNDOF, LNODETYPE, KSTEP, KINC, KMESHSWEEP, LSMOOTH
    real(8), dimension(NDIM) :: ULOCAL
    real(8), dimension(2) :: TIME
    real(8), dimension(NDIM, *) :: ALOCAL
    real(8), dimension(*) :: JMATYP, JGVBLOCK
    ! ----------------------我自己的参数--------------------------
    real(8) :: k, peeq
    integer :: NELEMS = 100 ! 一次返回100个节点
    integer :: JRCD ! 错误代码 0ok 1出错
    integer :: JTYP = 0
    integer, dimension(100) :: JELEMLIST, JELEMTYPE ! 存放返回值
    real(8), dimension(15) :: ARRAY

    call GETNODETOELEMCONN(NODE, NELEMS, JELEMLIST, JELEMTYPE, JRCD, JGVBLOCK)
    if (JRCD==1) then
        print *, "call GETNODETOELEMCONN wrong"
    end if
    call GETVRMAVGATNODE(NODE, JTYP, 'PE', ARRAY, JRCD, JELEMLIST, NELEMS, JMATYP, JGVBLOCK)
    if (JRCD==1) then
        print *, "call GETVRMAVGATNODE wrong"
    end if
    if (ARRAY(7)>0.0) then
        ULOCAL(NDIM) = -ARRAY(7)

    end if

    return
end subroutine UMESHMOTION
