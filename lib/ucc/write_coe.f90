subroutine write_coe(ic,KKK0,t0,ncoe)
    implicit none
    integer, intent(in)          :: ic, KKK0
    integer, intent(out)         :: ncoe
    real(kind=8), intent(in)     :: t0(KKK0)
    ! Local variables
    integer                      :: i, i1, i2, j, k, a, b, c
    integer                      :: N, KKK
    if (ic.eq.0) then
        return
    endif
    N=10*1024*1024
    ncoe=(KKK0-1)/N+1
    rewind(ic)
    do i=1,ncoe
        KKK=N
        i1=(i-1)*N+1
        i2=i*N
        if (i .eq. ncoe) then
            KKK=KKK0-N*(i-1);i2=KKK0
        endif
        write(ic)KKK
        write(ic)(t0(k),k=i1,i2)
    enddo
end subroutine
