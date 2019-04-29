program exerA

    implicit none
    real(8) :: v,v_1,dt,t,tempo,a,s,s_T,v_T,vm,t_term
    integer :: i, i_T
    read(*,*) v   !le a partir do terminal a v inicial
    read(*,*) dt  !le a partir do terminal o intervalo dt entre os passos
    read(*,*) tempo   !le o tempo final a partir do terminal
    read(*,*) A       !le a area de contato a partir do terminal
    i = 0             !inicia o contador
    v_1 = 0           !inicia a velocidade do passo -1
    s = 0             !inicia o espaço percorrido como sendo 0
    do while(i<=int(tempo/dt) .or. v_1 /= v)

        if (i <= int(tempo/dt)) then
            s = s + v*dt
            vm = vm + v
        endif

        if (i == int(tempo/dt)) then
            s_T = s
            v_T = v
            i_T = i
        endif

        if (v_1 < v) then
            t_term = t + dt
        endif

        v_1 = v

        call vt(v, t, dt, A)

        i = i+1

    enddo
    vm = vm/i_T

    print*, "O ciclista se curva para realizar a curva &
             &com a finalidade de compensar o torque gerado &
             &pela variação de momento angular com outro gerado &
             &pela gravidade. É vantajoso que um corredor cole &
             &atrás de outro ao invés de ultrapassá-lo diretamente &
             &pois o vácuo gerado pelo ciclista da frente diminui &
             &a resistencia do ar, permitindo um aumento na &
             &velocidade antes da ultrapassagem."
    print*, "A velocidade terminal é alcançada em t=", t_term, "s"
    print*, "O espaço percorrido após o tempo T é", s_T, "m"
    print*, "A velocidade final do ciclista após o tempo T é", v_T, "m/s"
    print*, "A velocidade média em T é", vm, "m/s"

end program exerA

subroutine vt(v,t,dt,A)

    real(8), intent(inout) :: v,t
    real(8), intent(in) :: dt,A
    integer :: m, p
    real(8) :: rho

    m = 70
    p = 400
    rho = 1.2

    v = v + (p*dt)/(m*v) - rho*A*v*v*dt/(2*m)
    t = t+dt

end subroutine vt
