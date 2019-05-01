program exerA

    implicit none
    real(8) :: v, dt, t, tempo
    integer :: i
    read(*,*) v,dt,tempo
    t = 0

    open(unit=100, file='velA_out.dat', status="new", action="write")

    do i = 1, 1 + int(tempo/dt), 1
        write(100,*) t, v
        call vt(v, t, dt)
    end do

end program exerA

subroutine vt(v,t,dt)

    real(8), intent(inout) :: v,t
    real(8), intent(in) :: dt
    integer :: m, p

    m = 70
    p = 400

    v = v + (p*dt)/(m*v)
    t = t+dt

end subroutine vt
