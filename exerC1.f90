module constants

    implicit none
    real(8), parameter :: pi = 3.14159265359d0
    real(8), parameter :: g = 9.81d0

end module constants

program exerC

    use constants
    implicit none

    integer :: i
    real(8) :: tempo, dt, m, l, theta, omega,t

    read(*,*) tempo, dt, m, l, theta
    theta = theta*pi/180
    omega = 0
    t = 0

    open(unit=100, file='exerC1_out.dat', status="new", action="write")

    do i = 0, int(tempo/dt), 1
        write(unit=100, fmt=*) t, theta
        call eu_cr(t,dt,l,theta,omega)
    enddo

end program exerC

subroutine eu_cr(t,dt,l,theta,omega)

    use constants
    real(8), INTENT(INOUT) :: theta, omega,t
    real(8), INTENT(IN) :: dt, l
    real(8) :: omega_i

    if (theta > pi) then
        theta = theta - 2*pi
    else if(theta < - pi) then
        theta = theta + 2*pi
    endif

    omega_i = omega
    omega = omega - g*theta*dt/l
    theta = theta + omega_i*dt

    if (theta > pi) then
        theta = theta - 2*pi
    else if(theta < - pi) then
        theta = theta + 2*pi
    endif

    t = t + dt

end subroutine eu_cr
