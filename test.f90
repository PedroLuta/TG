program test

implicit none

    integer :: i
    real, parameter :: pi = 4*atan(1.)

    ! do i = 0, 360, 30
    !     write(*,*) sin(i)
    ! end do

        write(*,*) sin(30.*pi/180)
        write(*,*) sin(60.*pi/180)
        write(*,*) sin(90.*pi/180)
end program test