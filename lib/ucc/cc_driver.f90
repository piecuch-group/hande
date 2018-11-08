subroutine solve_ccpq(io, occ_a, occ_b, orbs, froz, &
        e_cor, e_ref, &
        shift, itol, &
        f_a, f_b, v_aa, v_bb, v_ab, &
        diis_space, restart, p_space)

    use cc
    use hbar
    use lcc
    use ccp3
    !use utils, only: antisymmetrize_p_space

    implicit none

    integer, intent(in) :: io

    ! Molecular system
    integer, intent(in) :: froz, occ_a, occ_b, orbs

    real(kind=8), intent(inout) :: e_cor, e_ref

    real(kind=8), intent(in) :: f_a(orbs, orbs), f_b(orbs, orbs)
    real(kind=8), intent(in) :: v_aa(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)
    real(kind=8), intent(in) :: v_bb(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)
    real(kind=8), intent(in) :: v_ab(froz+1:orbs,froz+1:orbs,froz+1:orbs,froz+1:orbs)

    integer :: p_space(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a)

    ! CC variables
    integer :: t2_size
    real(kind=8) :: ccpq_energy(4)

    real(kind=8), allocatable :: t2(:)

    ! Calculation parameters
    !integer, intent(in) :: maxiter
    integer :: maxiter = 200
    integer, intent(in) :: itol
    integer, intent(in) :: diis_space
    real(kind=8), intent(in) :: shift
    logical, intent(in) :: restart

    logical :: is_rhf = .false.

    ! File management
    ! [TODO] make a module with all units
    integer, parameter :: hbar_unit = 327

    !call antisymmetrize_p_space(froz, occ_a, occ_b, orbs, p_space)

    call print_calc_params(io, froz, occ_a, occ_b, orbs, &
        shift, itol, e_ref, diis_space, maxiter, restart)

    ! Solve CCSDt
    ! [TODO] remove icoe? idiis?
    call solve_cc(io, froz, occ_a, occ_b, orbs, &
        shift, itol, &
        f_a, f_b, v_aa, v_bb, v_ab, &
        is_rhf, diis_space, restart, p_space, &
        e_cor)

    ! Start HBar
    call gen_hbar(io, froz, occ_a, occ_b, orbs, &
        f_a, f_b, v_aa, v_bb, v_ab)

    ! Start L-CCSD
    call solve_lcc(io, froz, occ_a, occ_b, orbs, &
        f_a, f_b, v_aa, v_bb, v_ab, &
        e_cor, itol, maxiter, diis_space, p_space)

    ! Start moments correction
    call correct_cc(io, froz, occ_a, occ_b, orbs, &
        f_a, f_b, v_aa, v_bb, v_ab, &
        ccpq_energy, p_space)


    call print_summary(io, e_ref, e_cor, ccpq_energy)

    close(hbar_unit)

end subroutine solve_ccpq
