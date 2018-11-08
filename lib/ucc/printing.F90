subroutine print_header(io)
    integer, intent(in) :: io
    character(len=255) :: hostname
    character(len=255) :: cmd
    integer(kind=4) :: stat

    write(io,'(a)') 'UCCSD building test'
    write(io,'(a/)') '==================='

    write(io,'(a)') 'Compilation information'
    write(io,'(a)') '-----------------------'

#if defined (COMP_TIME) && defined (COMP_HOST)
    write(io,'(2x,a20,1x,a)') 'Hostname', &
     COMP_HOST

    write(io,'(2x,a20,1x,a)') 'Date', &
     COMP_TIME
#endif

#ifdef __VERSION__
    write(io,'(2x,a20,1x,2a)') &
     'Compiler', 'gfortran ', __VERSION__
#endif

#ifdef __INTEL_COMPILER
    write(io,'(2x,a20,1x,a,i6)') &
     'Compiler', 'ifort ', __INTEL_COMPILER
#endif

#ifdef FLAGS
    write(io,'(2x,a20,1x,a)') &
     'Flags', FLAGS
#endif

#ifdef VERSION
    write(io,'(2x,a20,1x,a)') 'Git SHA', &
     VERSION
#endif

    call flush(io)

end subroutine print_header

subroutine print_calc_params(io, froz, occ_a, occ_b, total, &
        shift, itol, eref, diis_space, maxiter, restart)

    integer, intent(in) :: io
    integer, intent(in) :: froz, occ_a, occ_b, total
    integer, intent(in) :: itol, diis_space
    logical, intent(in) :: restart
    real(kind=8), intent(in) :: eref

    write(io,'(a)') 'System information'
    write(io,'(a)') '------------------'
    write(io,'(2x,a27,2x,i16)') 'Frozen orbitals', froz
    write(io,'(2x,a27,2x,i16)') 'Occupied orbitals (alpha)',occ_a
    write(io,'(2x,a27,2x,i16)') 'Occupied orbitals (beta)', occ_b
    write(io,'(2x,a27,2x,i16/)') 'Total orbitals', total

    !      write(io,'(2x,a27,2x,i16)')
    !    &  'Occupied active (guess)', m3
    !      write(io,'(2x,a27,2x,i16)')
    !    &  'Unoccupied active (guess)', m4
    !      write(io,'(2x,a27,2x,i16)')
    !    &  'Occupied active', m1
    !      write(io,'(2x,a27,2x,i16)')
    !    &  'Unoccupied active', m2

    write(io,'(/a)') 'CC settings'
    write(io,'(a)')  '-----------'
    !      write(io,'(2x,a27,2x,i16)') 'Number of excited states', nroot
    write(io,'(2x,a27,2x,es16.2)') 'Convergence tolerance', 1.0d1 ** (-itol)
    write(io,'(2x,a27,2x,i16)') 'Max. iterations', maxiter
    write(io,'(2x,a27,2x,i16)') 'DIIS space',diis_space
    write(io,'(2x,a27,2x,l16)') 'Restart', restart
    write(io,'(2x,a27,2x,es16.4)') 'Shift energy', shift

    write(io,'(/a)') 'Starting energies (Eh)'
    write(io,'(a)')  '----------------------'
    write(io,'(2x,a27,2x,f16.10/)') 'Reference (HF)', eref

    call flush(io)

end subroutine print_calc_params

subroutine print_summary(io, e_hf, ecor, ccpq_energy)
    integer, intent(in) :: io
    real(kind=8), intent(in) :: e_hf, ecor
    real(kind=8), intent(in) :: ccpq_energy(4)

    write(io,'(/a)') 'CC(P;3) Calculation Summary (Eh)'
    write(io,'(a/)') '--------------------------------'
    write(io,'(5x,a12,2a25)') 'Method', 'Correlation Energy (Eh)', 'Total Energy (Eh)'
    write(io,'(5x,62("-"))')
    write(io,'(5x,a12,7x,f18.12,7x,f18.12)') 'CCSDt', ecor,  e_hf + ecor
    write(io,'(5x,a12,7x,f18.12,7x,f18.12)') 'CC(t;3),A', ecor + ccpq_energy(1),  e_hf + ecor + ccpq_energy(1)
    write(io,'(5x,a12,7x,f18.12,7x,f18.12)') 'CC(t;3)', ecor + ccpq_energy(4),  e_hf + ecor + ccpq_energy(4)

    call flush(io)

end subroutine print_summary


subroutine print_date(io, note)
      implicit none
      integer, intent(in) :: io
      character(len=*), intent(in) :: note
      character(len=30) :: date

      call fdate(date)
      write (io,'(/a)') trim(note)//' '//trim(date)

      call flush(io)

end subroutine print_date

subroutine print_iter_head(io)
    integer, intent(in) :: io
    write(io,'(/2x,a4,3(a15),a16)') 'It.',  'E (Corr)', 'dE', 'Residuum', 'CPU Time'
    write(io,'(2x,65("-"))')
    call flush(io)
end subroutine print_iter_head

subroutine print_iteration(io, iter, ecor, energy_diff, res, prev_time)
    integer, intent(in) :: io
    integer, intent(in) :: iter
    real(kind=8), intent(in) :: ecor
    real(kind=8), intent(in) :: energy_diff
    real(kind=8), intent(in) :: res
    real(kind=8), intent(in) :: prev_time
    real(kind=8) :: cputime

    real(kind=8) :: nsec
    integer :: nmin

    call cpu_time(cputime)
    nsec=cputime - prev_time
    nmin=int(nsec) / 60
    nsec=nsec-real(nmin, kind=8)*60.0d0

    write(io,'(2x,i4,3(f15.10),i5,'' min'',f5.1,'' s'')') iter,ecor,energy_diff, res,nmin,nsec

    call flush(io)
end subroutine print_iteration

subroutine print_help()

    ! Print the help dialog

    print '(a)', 'Usage: CC [OPTIONS] FILE'
    print '(a/)', 'Coupled-cluster program'

    print '(a)', 'Options'
    !print '(a24,4x,a)', '-d,', 'write singles and doubles as well as triples and quadruples.'

    call exit()
end subroutine print_help

subroutine abort_cc(msg)
    use, intrinsic :: iso_fortran_env, only: error_unit
    character(len=*), intent(in) :: msg

    write(error_unit, '(a)') msg
    call exit(1)

end subroutine abort_cc

subroutine print_amps(iR,io,N0,N1,N2,N3,KKK,PP)

    integer, intent(in) :: n0, n1, n2, n3

    real(kind=8) :: PP
    real(kind=8), allocatable :: r(:)
    integer :: i, j, k, a, b, c, d

    !      PP=1.0d-1
    allocate(r(KKK))
    r = 0.0d0
    rewind(ir)
    read(iR) r
    i0=0
    !1A
    do i=N0+1,N1
        do a=N1+1,N3
            i0=i0+1
            if(dabs(r(i0)).lt.PP)cycle
            write(io,'(i4,a,i4,a,e19.12)')i,'A  --> ',a,'A  :',r(i0)
        enddo
    enddo
    !1B
    do i=N0+1,N2
        do a=N2+1,N3
            i0=i0+1
            if(dabs(r(i0)).lt.PP)cycle
            write(io,'(i4,a,i4,a,e19.12)')i,'B  --> ',a,'B  :',r(i0)
        enddo
    enddo
    !2A
    do i=N0+1,N1
        do j=N0+1,N1
            do a=N1+1,N3
                do b=N1+1,N3
                    i0=i0+1
                    if(dabs(r(i0)).lt.PP)cycle
                    write(io,'(i4,a,i4,a,i4,a,i4,a,e19.12)') i,'A  ',j,'A  --> ',a,'A  ',b,'A  :',r(i0)
                enddo
            enddo
        enddo
    enddo
    !2B
    do i=N0+1,N1
        do j=N0+1,N2
            do a=N1+1,N3
                do b=N2+1,N3
                    i0=i0+1
                    if(dabs(r(i0)).lt.PP)cycle
                    write(io,'(i4,a,i4,a,i4,a,i4,a,e19.12)') i,'A  ',j,'B  --> ',a,'A  ',b,'B  :',r(i0)
                enddo
            enddo
        enddo
    enddo
    !2C
    do i=N0+1,N2
        do j=N0+1,N2
            do a=N2+1,N3
                do b=N2+1,N3
                    i0=i0+1
                    if(dabs(r(i0)).lt.PP)cycle
                    write(io,'(i4,a,i4,a,i4,a,i4,a,e19.12)') i,'B  ',j,'B  --> ',a,'B  ',b,'B  :',r(i0)
                enddo
            enddo
        enddo
    enddo

end subroutine print_amps
