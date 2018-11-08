module diis
    implicit none

    integer :: chunks = 0
    integer :: chunk_size = 0
    integer, parameter :: max_len = (2**30/8)

    contains

        subroutine calc_diis(t_vecs_unit, diis_space, t_size, t)

            ! Input vars
            integer, intent(in) :: t_vecs_unit
            integer, intent(in) :: diis_space
            integer, intent(in) :: t_size

            real(kind=8), allocatable, intent(inout) :: t(:)

            ! Local vars
            integer, allocatable :: ipiv(:)
            integer :: info
            integer :: indx, indy

            real(kind=8), allocatable :: t_aux_1(:), t_aux_2(:), t_aux_3(:)
            real(kind=8), allocatable :: B(:,:), C(:)

            real(kind=8) :: accum
            real(kind=8) ddot, axpy

            ! Allocate
            allocate(B(diis_space+1,diis_space+1))
            B=0.0d0
            allocate(t_aux_1(t_size))
            allocate(t_aux_2(t_size))
            allocate(t_aux_3(t_size))

            do indx = 1, diis_space

                ! Generate difference vector
                !read(t_vecs_unit, rec=indx) t_aux_1
                !read(t_vecs_unit, rec=indx+1) t_aux_2
                call read_t_vecs(t_vecs_unit, indx,  t_aux_1)
                call read_t_vecs(t_vecs_unit, indx+1,  t_aux_2)

                t_aux_3 = t_aux_2 - t_aux_1

                do indy = 1, diis_space

                    ! Generate difference vector
                    !read(t_vecs_unit, rec=indy) t_aux_1
                    !read(t_vecs_unit, rec=indy+1) t_aux_2
                    call read_t_vecs(t_vecs_unit, indy,  t_aux_1)
                    call read_t_vecs(t_vecs_unit, indy+1,  t_aux_2)

                    t_aux_2 = t_aux_2 - t_aux_1

                    accum = 0.0d0
                    accum = ddot(t_size, t_aux_3, 1, t_aux_2, 1)

                    B(indx,indy) = B(indx,indy) + accum
                enddo
            enddo

            deallocate(t_aux_2, t_aux_3)

            ! Set DIIS matrix boundaries
            b(:,diis_space+1) = -1.0d0
            b(diis_space+1,:) = -1.0d0

            !allocate(L(iDIIS+1),C(iDIIS+1))
            allocate(ipiv(diis_space+1))
            allocate(c(diis_space+1))

            c=0.0d0
            c(diis_space+1) = -1.0d0

            ! Solve system of equations
            call dgesv(diis_space+1, 1, B, diis_space+1, ipiv, c, diis_space+1, info)

            if (info /= 0) call abort_cc('DIIS error.')

            t = 0.0d0
            do indx = 1, diis_space
                !read(t_vecs_unit, rec=indx) t_aux_1
                call read_t_vecs(t_vecs_unit, indx, t_aux_1)

                call daxpy(t_size, C(indx), t_aux_1, 1, t, 1)

            enddo

            deallocate(t_aux_1)
            deallocate(ipiv, c, b)

        end subroutine calc_diis

        subroutine write_t_vecs(t_vecs_unit, iter, diis_space, t)

            integer, intent(in) :: t_vecs_unit
            integer, intent(in) :: iter
            integer, intent(in) :: diis_space

            real(kind=8), allocatable, intent(in) :: t(:)

            integer :: indx_rec, i_chunk, i

            indx_rec = mod(iter, diis_space + 1)
            if (indx_rec == 0) indx_rec = diis_space + 1

            if (chunks == 0) then
                write(t_vecs_unit, rec=indx_rec) t
            else
                do i_chunk=0, chunks
                    if (i_chunk < chunks) then
                        write(t_vecs_unit, rec=((indx_rec - 1) * chunks) + i_chunk + 1) &
                            t(i_chunk*max_len+1:(i_chunk+1)*max_len)
                    else
                        write(t_vecs_unit, rec=((indx_rec - 1) * chunks) + i_chunk + 1) &
                            t(i_chunk*max_len+1:i_chunk*max_len + chunk_size)
                    endif

                enddo
            endif


        end subroutine write_t_vecs

        subroutine read_t_vecs(t_vecs_unit, indx,  t)

            integer, intent(in) :: t_vecs_unit
            integer, intent(in) :: indx

            real(kind=8), allocatable, intent(inout) :: t(:)

            integer :: i_chunk


            if (chunks == 0) then
                read(t_vecs_unit, rec=indx) t
            else
                do i_chunk=0, chunks
                    if (i_chunk < chunks) then
                        read(t_vecs_unit, rec=((indx - 1) * chunks) + i_chunk + 1) &
                            t(i_chunk*max_len+1:(i_chunk+1)*max_len)
                    else
                        read(t_vecs_unit, rec=((indx - 1) * chunks) + i_chunk + 1) &
                            t(i_chunk*max_len+1:i_chunk*max_len + chunk_size)
                    endif
                enddo
            endif

        end subroutine read_t_vecs

        subroutine init_t_vecs(t_vecs_unit, t_size)
            integer, intent(in) :: t_vecs_unit, t_size

            chunks = int(t_size / max_len)
            chunk_size = t_size - max_len * chunks

            if (chunks == 0) then
                open(t_vecs_unit, file='t_vecs.bin', form='unformatted', recl=t_size*8, access='direct')
            else
                open(t_vecs_unit, file='t_vecs.bin', form='unformatted', recl=max_len*8, access='direct')
            endif


        end subroutine init_t_vecs


    end module diis
