module cc_msu

    use const
    use errors, only: stop_all

    implicit none
    integer, parameter :: hmat_max_size = 5000000

    type cc_hmat_t
        integer(i0), allocatable :: ff(:,:) ! (string_len, string_len, hmat_max_size)
        real(p), allocatable :: hmat_r(:) ! (hmat_max_size)
        integer :: str_len
        integer :: hmat_size
    end type cc_hmat_t

    type p_mask_t
        integer(kind=8), allocatable :: a(:,:,:,:,:,:)
        integer(kind=8), allocatable :: b(:,:,:,:,:,:)
        integer(kind=8), allocatable :: c(:,:,:,:,:,:)
        integer(kind=8), allocatable :: d(:,:,:,:,:,:)
    end type p_mask_t

contains

    subroutine init_hmat(sys, hmat)

        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(cc_hmat_t), intent(inout) :: hmat


        if (.not. allocated(hmat%ff) .and. .not. allocated(hmat%hmat_r)) then
            allocate(hmat%ff(2*sys%basis%tot_string_len, hmat_max_size))
            allocate(hmat%hmat_r(hmat_max_size))
            hmat%str_len = sys%basis%tot_string_len
        else
            call stop_all('init_hmat', 'H matrix elements already allocated')
        end if

    end subroutine init_hmat

    subroutine store_hmat(sys, hmat, hmatel, f1, f2)

        use hamiltonian_data, only: hmatel_t
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(cc_hmat_t), intent(inout) :: hmat
        !type(hmatel_t), intent(in) :: hmatel
        real(p), intent(in) :: hmatel
        integer(i0), intent(in) :: f1(sys%basis%tot_string_len)
        integer(i0), intent(in) :: f2(sys%basis%tot_string_len)

        integer(i0) :: f_test(2*sys%basis%tot_string_len)
        integer :: i
        logical :: seen

        f_test(1:sys%basis%tot_string_len) = f1
        f_test(1+sys%basis%tot_string_len:2*sys%basis%tot_string_len) = f2

        seen = .false.
        do i=1, hmat%hmat_size

            if (all(f_test == hmat%ff(:,i))) then
                seen = .true.
                return
            end if
        enddo

        hmat%hmat_size = hmat%hmat_size + 1
        if (hmat%hmat_size > hmat_max_size) call stop_all("store_hmat", "H matrix is too large. Try increasing hmat_max_size.")
        hmat%ff(:,hmat%hmat_size) = f_test
        hmat%hmat_r(hmat%hmat_size) = hmatel

    end subroutine store_hmat

    subroutine test_list(hmat, psip_list)
        use qmc_data, only: particle_t

        type(cc_hmat_t), intent(in) :: hmat
        type(particle_t), intent(in) :: psip_list

        integer :: idet, ihmat
        integer :: seen_right, seen_left

        seen_right = 0
        seen_left = 0
        do idet=1, psip_list%nstates
            do ihmat=1, hmat%hmat_size
                if (all(psip_list%states(:,idet) == hmat%ff(1:hmat%str_len,ihmat))) seen_right = seen_right + 1
                if (all(psip_list%states(:,idet) == hmat%ff(hmat%str_len+1:2*hmat%str_len,ihmat))) seen_left = seen_left + 1
            enddo
        enddo

        print *, seen_left, seen_right
        call stop_all('test_list', 'Checking')

    end subroutine test_list

    subroutine write_dets(sys, psip_list, ireport)

        use determinants, only: det_info_t, alloc_det_info_t, dealloc_det_info_t
        use qmc_data, only: particle_t
        use system, only: sys_t
        use proc_pointers

        type(sys_t), intent(in) :: sys
        type(particle_t), intent(in), target :: psip_list
        integer, intent(in) :: ireport

        type(det_info_t) :: cdet

        integer :: idet, iocc
        character(len=200) :: output
        character(len=30) :: det_fmt

        call alloc_det_info_t(sys, cdet, .false.)
        write(output, '(a,i0.6)') 'walks-', ireport
        write(det_fmt, '(a,i0,a)') '(10x,', sys%nel, 'i4)'
        open(865,file=trim(output), status='unknown')

        do idet=1, psip_list%nstates
            cdet%f => psip_list%states(:,idet)
            cdet%data => psip_list%dat(:,idet)

            call decoder_ptr(sys, cdet%f, cdet)

            write(865, '(i10,i10)', advance='no') idet, psip_list%pops(1, idet)
            write(865, det_fmt) (cdet%occ_list(iocc), iocc=1, sys%nel)
        enddo
        close(865)
        call dealloc_det_info_t(cdet, .false.)

    end subroutine write_dets

    subroutine gen_p_space(sys, ref, hmat, psip_list, p_space, p_space_size)

        use bit_utils
        use excitations, only: get_excitation, excit_t
        use qmc_data, only: particle_t
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(cc_hmat_t), intent(in) :: hmat
        type(particle_t), intent(in) :: psip_list
        type(p_mask_t), intent(inout) :: p_space

        integer(i0), intent(in) :: ref(sys%basis%tot_string_len)
        integer, intent(inout) :: p_space_size

        integer(i0) :: f_ex(sys%basis%tot_string_len)

        integer :: idet, ihmat
        integer :: seen_right, seen_left

        !type(excit_t) :: excitation
        integer :: excitation

        ! [TODO] make this open-shell
        integer :: i, j, iexcit1, iexcit2, perm, iel1, iel2, shift, nset_bits
        logical :: test_f1, test_f2
        integer :: from_orb(3), to_orb(3)
        integer :: alpha_from(3), alpha_to(3)
        integer :: spin_case
        integer :: i_spin, tmp_orb

        ! All alpha
        p_space%a = 0
        ! All beta
        p_space%b = 0
        ! alpha, beta, alpha
        p_space%c = 0
        ! beta, beta, alpha
        p_space%d = 0

        p_space_size = 0

        do idet=1, psip_list%nstates
            f_ex = psip_list%states(:,idet)

            excitation = sum(count_set_bits(ieor(ref,f_ex)))/2
            if (excitation == 3) then

                alpha_from = 0
                alpha_to = 0

                spin_case = 0

                iexcit1 = 0
                iexcit2 = 0
                iel1 = 0
                iel2 = 0
                perm = 0

                do i = 1, sys%basis%tot_string_len
                    ! Bonus optimisation: We can skip most of the following for
                    ! this element of the bit strings if they are equal, but
                    ! may have to update iel1 and iel2 first...
                    if (ref(i) == f_ex(i)) then
                        ! If iexcit1-excit2 is even then we don't need to
                        ! update iel1 and iel2, since any error introduced into
                        ! perm by not doing so will be included an even number
                        ! of times, and so won't alter the parity.
                        if (modulo(iexcit1-iexcit2,2) == 1) then
                            nset_bits = count_set_bits(ref(i))
                            iel1 = iel1 + nset_bits
                            iel2 = iel2 + nset_bits
                        end if
                        cycle
                    end if

                    do j = 0, i0_end

                        test_f1 = btest(ref(i),j)
                        test_f2 = btest(f_ex(i),j)

                        if (test_f2) iel2 = iel2 + 1

                        if (test_f1) then
                            iel1 = iel1 + 1
                            if (.not.test_f2) then
                                ! occupied in f1 but not in f2
                                iexcit1 = iexcit1 + 1
                                if (mod(sys%basis%basis_lookup(j,i),2) == 1) then
                                    alpha_from(iexcit1) = iexcit1
                                    spin_case = spin_case + 1
                                endif
                                from_orb(iexcit1) = (sys%basis%basis_lookup(j,i) - 1) / 2 + 1
                            end if
                        else
                            if (test_f2) then
                                ! occupied in f1 but not in f2
                                iexcit2 = iexcit2 + 1
                                if (mod(sys%basis%basis_lookup(j,i),2) == 1) then
                                    alpha_to(iexcit2) = iexcit2
                                endif
                                to_orb(iexcit2) = (sys%basis%basis_lookup(j,i) - 1) / 2 + 1
                            end if
                        end if

                    end do
                end do

                select case (spin_case)
                case (0)
                    call antisymmetrize_p_space(to_orb(3), to_orb(2), to_orb(1), from_orb(3), from_orb(2), from_orb(1), p_space%a)

                case (1)
                    do i_spin=1, 3
                        if (alpha_from(i_spin) /= 0) then
                            tmp_orb = from_orb(1)
                            from_orb(1) = from_orb(i_spin)
                            if (i_spin == 3) then
                                from_orb(3) = from_orb(2)
                                from_orb(2) = tmp_orb
                            else
                                from_orb(i_spin) = tmp_orb
                            endif
                        endif

                        if (alpha_to(i_spin) /= 0) then
                            tmp_orb = to_orb(1)
                            to_orb(1) = to_orb(i_spin)
                            if (i_spin == 3) then
                                to_orb(3) = to_orb(2)
                                to_orb(2) = tmp_orb
                            else
                                to_orb(i_spin) = tmp_orb
                            endif
                        endif
                    enddo
                    call antisymmetrize_p_space(to_orb(3), to_orb(2), to_orb(1), from_orb(3), from_orb(2), from_orb(1), p_space%a)

                case (2)
                    do i_spin=1, 3
                        if (alpha_from(i_spin) == 0) then
                            tmp_orb = from_orb(2)
                            from_orb(2) = from_orb(i_spin)
                            from_orb(i_spin) = tmp_orb
                        endif

                        if (alpha_to(i_spin) == 0) then
                            tmp_orb = to_orb(2)
                            to_orb(2) = to_orb(i_spin)
                            to_orb(i_spin) = tmp_orb
                        endif
                    enddo
                    call antisymmetrize_p_space(to_orb(3), to_orb(2), to_orb(1), from_orb(3), from_orb(2), from_orb(1), p_space%a)

                case (3)
                    call antisymmetrize_p_space(to_orb(3), to_orb(2), to_orb(1), from_orb(3), from_orb(2), from_orb(1), p_space%a)

                end select

                p_space_size = p_space_size + 1

            endif

        enddo

    end subroutine gen_p_space

    subroutine cc_p_driver(sys, ref, psip_list, ireport)

        use qmc_data, only: particle_t
        use molecular_integrals, only: get_one_body_int_mol_real, get_two_body_int_mol_real
        use reference_determinant, only: reference_t
        use system, only: sys_t


        type(sys_t), intent(in) :: sys
        type(reference_t), intent(in) :: ref
        type(particle_t), intent(in) :: psip_list
        type(cc_hmat_t) :: hmat

        integer, intent(in) :: ireport
        real(kind=8) :: epst,r1

        real(kind=8), allocatable :: z(:,:), f_a(:,:), f_b(:,:)
        real(kind=8), allocatable :: v_aa(:,:,:,:), v_ab(:,:,:,:), v_bb(:,:,:,:)

        real(kind=8) :: e_hf, e_cor, x
        integer :: i, j, k, l, indx

        type(p_mask_t) :: p_space
        integer :: p_space_size

        ! [TODO] use HANDE's unit generation system
        integer(kind=8), parameter :: diis_space = 8, inf = 400, io = 401

        integer(kind=8) :: occ_a, occ_b
        integer(kind=8) :: orbs
        integer(kind=8),allocatable::icoe(:)

        integer :: ios

        character(len=200) :: cc_log

        ! Testing vars
        integer :: a, b, c
        logical :: restart = .false.

        orbs = ceiling(real(sys%basis%nbasis / 2))
        occ_a = sys%nel / 2 + sys%Ms
        occ_b = sys%nel / 2 - sys%Ms

        allocate(p_space%a(occ_a+1:orbs, occ_a+1:orbs, occ_a+1:orbs, occ_a, occ_a, occ_a))
        allocate(p_space%b(occ_b+1:orbs, occ_b+1:orbs, occ_b+1:orbs, occ_b, occ_b, occ_b))
        allocate(p_space%c(occ_a+1:orbs, occ_b+1:orbs, occ_a+1:orbs, occ_a, occ_b, occ_a))
        allocate(p_space%d(occ_b+1:orbs, occ_b+1:orbs, occ_a+1:orbs, occ_b, occ_b, occ_a))

        call gen_p_space(sys, ref%f0, hmat, psip_list, p_space, p_space_size)

        allocate(z(orbs, orbs))
        allocate(v_aa(orbs, orbs, orbs, orbs))
        allocate(v_ab(orbs, orbs, orbs, orbs))
        allocate(v_bb(orbs, orbs, orbs, orbs))

        associate(one_e_ints=>sys%read_in%one_e_h_integrals, coulomb_ints=>sys%read_in%coulomb_integrals)

            do i=1, orbs
                do j=1, orbs
                    z(i,j) = get_one_body_int_mol_real(one_e_ints, 2*i, 2*j, sys)
                enddo
            enddo

            open(998, file='twobody_test', status='unknown')
            do i=1, orbs
                do j=1, orbs
                    do k=1, orbs
                        do l=1, orbs
                            v_ab(i,j,k,l) = get_two_body_int_mol_real(coulomb_ints, 2*i, 2*j, 2*k, 2*l, sys)
                            write(998, '(4i4,f15.8)') i,j,k,l, v_ab(i,j,k,l)
                        enddo
                    enddo
                enddo
            enddo
            close(998)

        end associate

        call write_p_space_to_file(occ_a, orbs, ireport, p_space%a)

        allocate(f_a(orbs, orbs))
        allocate(f_b(orbs, orbs))

        f_a = z
        f_b = z

        deallocate(z)

        ! Anti-symmetrize two-body integrals
        do i=1, orbs
            do j=1, orbs
                do k=1, orbs
                    do l=1, orbs
                        v_aa(l,k,j,i) = v_ab(l,k,j,i) - v_ab(l,k,i,j)
                        v_bb(l,k,j,i) = v_ab(l,k,j,i) - v_ab(l,k,i,j)
                    enddo
                enddo
            enddo
        enddo

        ! Generate fock operator
        call gen_fock(occ_a, occ_b, orbs, v_ab, f_a, f_b, e_hf)

        write(cc_log, '(a,i0.6,a)') 'cc_p', ireport, '.out'
        open(io, file=trim(cc_log), status='unknown')

        write(io, '(a,i8)') 'CC for report ', ireport
        write(io, '(a/)')    '----------------------'

        write(io, '(a,i8)') 'P space size ', p_space_size

        call solve_ccpq(io, occ_a, occ_b, orbs, int(0,kind=8), &
            e_cor, ref%H00, &
            0.0d0, int(10,kind=8), &
            f_a, f_b, v_aa, v_bb, v_ab, &
            diis_space, restart, p_space%a)

        close(io)

        deallocate(f_a)
        deallocate(f_b)
        deallocate(v_aa)
        deallocate(v_ab)
        deallocate(v_bb)
        deallocate(p_space%a)
        deallocate(p_space%b)
        deallocate(p_space%c)
        deallocate(p_space%d)

    end subroutine cc_p_driver

    subroutine gen_fock(occ_a, occ_b, orbs, v_ab, f_a, f_b, e_hf)

        integer(kind=8), intent(in) :: occ_a, occ_b, orbs

        real(kind=8), intent(inout) :: f_a(orbs, orbs), f_b(orbs, orbs)
        real(kind=8), intent(in) :: v_ab(orbs, orbs, orbs, orbs)

        real(kind=8), intent(inout) :: e_hf

        integer :: i, j, k


        e_hf = 0.0d0
        do i=1, occ_a
            e_hf = e_hf + f_a(i,i)
        enddo

        do i=1, occ_b
            e_hf = e_hf + f_b(i,i)
        enddo

        do i=1, occ_a
            do j=1, occ_b
                e_hf = e_hf + v_ab(i,j,i,j)
            enddo
        enddo

        do i=1, occ_a
            do j=1, occ_a
                e_hf = e_hf + 0.5d0*(v_ab(i,j,i,j) - v_ab(i,j,j,i))
            enddo
        enddo

        do i=1, occ_b
            do j=1, occ_b
                e_hf = e_hf + 0.5d0*(v_ab(i,j,i,j) - v_ab(i,j,j,i))
            enddo
        enddo

        do i=1,orbs
            do j=1,i
                do k=1,occ_b
                    f_a(i,j) = f_a(i,j) + v_ab(i,k,j,k)
                enddo

                do k=1,occ_a
                    f_a(i,j) = f_a(i,j) + v_ab(i,k,j,k) - v_ab(i,k,k,j)
                enddo

                f_a(j,i) = f_a(i,j)

                do k=1,occ_a
                    f_b(i,j) = f_b(i,j) + v_ab(k,i,k,j)
                enddo

                do k=1,occ_b
                    f_b(i,j) = f_b(i,j) + v_ab(i,k,j,k) - v_ab(i,k,k,j)
                enddo
                f_b(j,i) = f_b(i,j)
            enddo
        enddo

    end subroutine gen_fock

    subroutine antisymmetrize_p_space(c, b, a, k, j, i, p_space)

        integer, intent(in) :: a, b, c, i, j, k
        integer(kind=8), allocatable, intent(inout) :: p_space(:,:,:,:,:,:)


        p_space(c,b,a,k,j,i) = 1
        p_space(c,b,a,k,i,j) = 1
        p_space(c,b,a,i,j,k) = 1
        p_space(c,b,a,i,k,j) = 1
        p_space(c,b,a,j,k,i) = 1
        p_space(c,b,a,j,i,k) = 1
        p_space(c,a,b,k,j,i) = 1
        p_space(c,a,b,k,i,j) = 1
        p_space(c,a,b,i,j,k) = 1
        p_space(c,a,b,i,k,j) = 1
        p_space(c,a,b,j,k,i) = 1
        p_space(c,a,b,j,i,k) = 1
        p_space(a,b,c,k,j,i) = 1
        p_space(a,b,c,k,i,j) = 1
        p_space(a,b,c,i,j,k) = 1
        p_space(a,b,c,i,k,j) = 1
        p_space(a,b,c,j,k,i) = 1
        p_space(a,b,c,j,i,k) = 1
        p_space(a,c,b,k,j,i) = 1
        p_space(a,c,b,k,i,j) = 1
        p_space(a,c,b,i,j,k) = 1
        p_space(a,c,b,i,k,j) = 1
        p_space(a,c,b,j,k,i) = 1
        p_space(a,c,b,j,i,k) = 1
        p_space(b,c,a,k,j,i) = 1
        p_space(b,c,a,k,i,j) = 1
        p_space(b,c,a,i,j,k) = 1
        p_space(b,c,a,i,k,j) = 1
        p_space(b,c,a,j,k,i) = 1
        p_space(b,c,a,j,i,k) = 1
        p_space(b,a,c,k,j,i) = 1
        p_space(b,a,c,k,i,j) = 1
        p_space(b,a,c,i,j,k) = 1
        p_space(b,a,c,i,k,j) = 1
        p_space(b,a,c,j,k,i) = 1
        p_space(b,a,c,j,i,k) = 1

    end subroutine antisymmetrize_p_space

    subroutine write_p_space_to_file(occ_a, orbs, ireport, p_space)

        integer(kind=8), intent(in) :: occ_a, orbs
        integer, intent(in) :: ireport
        integer(kind=8), allocatable, intent(in) :: p_space(:,:,:,:,:,:)
        character(len=100) :: cc_log

        integer :: i, j, k, a, b, c

        write(cc_log, '(a,i0.6,a)') 'p_space', ireport, '.txt'
        open(834, file=trim(cc_log), status='unknown')

        do i=1, occ_a
            do j=1, occ_a
                do k=1, occ_a
                    do a=occ_a+1, orbs
                        do b=occ_a+1, orbs
                            do c=occ_a+1, orbs
                                if (p_space(c,b,a,k,j,i) == 1) then
                                    write(834, '(6i3)') c,b,a,k,j,i
                                endif
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

        close(834)

    end subroutine write_p_space_to_file

end module cc_msu
