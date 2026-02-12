/* Copyright (C) 1991-2024 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
!
! © 2025. Triad National Security, LLC. All rights reserved.
!
! This program was produced under U.S. Government contract 89233218CNA000001
! for Los Alamos National Laboratory (LANL), which is operated by
! Triad National Security, LLC for the U.S. Department of Energy/National Nuclear
! Security Administration. All rights in the program are reserved by
! Triad National Security, LLC, and the U.S. Department of Energy/National
! Nuclear Security Administration. The Government is granted for itself and
! others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide
! license in this material to reproduce, prepare. derivative works,
! distribute copies to the public, perform publicly and display publicly,
! and to permit others to do so.
!
! Author:
! Kai Gao, kaigao@lanl.gov
!
module libflit_mpicomm_group
    use mpi_f08
    use libflit_array
    use libflit_domain_decomposition
    use libflit_error
    use libflit_array_operation
    use libflit_mpicomm
    use iso_fortran_env
    implicit none
    ! The module is a copy-and-paste of module_mpicomm
    ! with slight modifications to group domain
    interface bcast_array_group
        module procedure :: bcast_array_1d_group_string
        module procedure :: bcast_array_1d_group_int
        module procedure :: bcast_array_2d_group_int
        module procedure :: bcast_array_3d_group_int
        module procedure :: bcast_array_1d_group_double
        module procedure :: bcast_array_2d_group_double
        module procedure :: bcast_array_3d_group_double
        module procedure :: bcast_array_1d_group_float
        module procedure :: bcast_array_2d_group_float
        module procedure :: bcast_array_3d_group_float
        module procedure :: bcast_array_1d_group_complex
        module procedure :: bcast_array_2d_group_complex
        module procedure :: bcast_array_3d_group_complex
    end interface
    interface commute_array_group
        module procedure :: commute_array_1d_group_int
        module procedure :: commute_array_2d_group_int
        module procedure :: commute_array_3d_group_int
        module procedure :: commute_array_1d_group_double
        module procedure :: commute_array_2d_group_double
        module procedure :: commute_array_3d_group_double
        module procedure :: commute_array_1d_group_float
        module procedure :: commute_array_2d_group_float
        module procedure :: commute_array_3d_group_float
        module procedure :: commute_array_1d_group_complex
        module procedure :: commute_array_2d_group_complex
        module procedure :: commute_array_3d_group_complex
    end interface
    interface allreduce_group
        ! Scalar
        module procedure :: gather_distribute_group_int
        module procedure :: gather_distribute_group_double
        module procedure :: gather_distribute_group_float
        module procedure :: gather_distribute_group_complex
        module procedure :: gather_distribute_group_dcomplex
    end interface
    interface allreduce_array_group
        ! Array
        module procedure :: gather_distribute_array_1d_group_int
        module procedure :: gather_distribute_array_2d_group_int
        module procedure :: gather_distribute_array_3d_group_int
        module procedure :: gather_distribute_array_1d_group_double
        module procedure :: gather_distribute_array_2d_group_double
        module procedure :: gather_distribute_array_3d_group_double
        module procedure :: gather_distribute_array_1d_group_float
        module procedure :: gather_distribute_array_2d_group_float
        module procedure :: gather_distribute_array_3d_group_float
        module procedure :: gather_distribute_array_1d_group_complex
        module procedure :: gather_distribute_array_2d_group_complex
        module procedure :: gather_distribute_array_3d_group_complex
        module procedure :: gather_distribute_array_1d_group_dcomplex
        module procedure :: gather_distribute_array_2d_group_dcomplex
        module procedure :: gather_distribute_array_3d_group_dcomplex
        ! For array with size = int8
        module procedure :: gather_distribute_large_array_1d_group_int
        module procedure :: gather_distribute_large_array_1d_group_double
        module procedure :: gather_distribute_large_array_1d_group_float
        module procedure :: gather_distribute_large_array_1d_group_complex
        module procedure :: gather_distribute_large_array_1d_group_dcomplex
    end interface
    interface reduce_group
        module procedure :: gather_group_int
        module procedure :: gather_group_double
        module procedure :: gather_group_float
        module procedure :: gather_group_complex
        module procedure :: gather_group_dcomplex
    end interface
    interface reduce_array_group
        module procedure :: gather_array_1d_group_int
        module procedure :: gather_array_2d_group_int
        module procedure :: gather_array_3d_group_int
        module procedure :: gather_array_1d_group_double
        module procedure :: gather_array_2d_group_double
        module procedure :: gather_array_3d_group_double
        module procedure :: gather_array_1d_group_float
        module procedure :: gather_array_2d_group_float
        module procedure :: gather_array_3d_group_float
        module procedure :: gather_array_1d_group_complex
        module procedure :: gather_array_2d_group_complex
        module procedure :: gather_array_3d_group_complex
        module procedure :: gather_array_1d_group_dcomplex
        module procedure :: gather_array_2d_group_dcomplex
        module procedure :: gather_array_3d_group_dcomplex
    end interface
    interface domain_decomp_regular_group
        module procedure :: domain_decomp_regular_2d_group
        module procedure :: domain_decomp_regular_3d_group
    end interface
    interface group_min
        module procedure :: mpi_group_min_int
        module procedure :: mpi_group_min_float
        module procedure :: mpi_group_min_double
    end interface
    interface group_max
        module procedure :: mpi_group_max_int
        module procedure :: mpi_group_max_float
        module procedure :: mpi_group_max_double
    end interface
    interface group_and
        module procedure :: mpi_group_and
    end interface
    interface group_or
        module procedure :: mpi_group_or
    end interface
    integer, public :: ngroup = 1
    integer, public :: groupid
    type(mpi_comm), public :: mpi_group_comm
    integer, public :: rank1_group, rank2_group, rank3_group
    integer, public :: block_x1left_group, block_x1right_group
    integer, public :: block_x2left_group, block_x2right_group
    integer, public :: block_x3left_group, block_x3right_group
    integer, public :: rankid_group
    integer, public :: blockid_group
    integer, public :: nrank_group
    type(mpi_status), public :: mpi_stats_group
    integer, public :: mpi_ierr_group
    public :: mpistart_group, mpiend_group, mpibarrier_group, mpistop_group
    public :: bcast_array_group
    public :: reduce_group
    public :: reduce_array_group
    public :: allreduce_group
    public :: allreduce_array_group
    public :: commute_array_group
    public :: domain_decomp_regular_group
    public :: group_min
    public :: group_max
    public :: group_and
contains
    subroutine mpistart_group
        integer, allocatable, dimension(:, :) :: r
        integer :: i
        call assert(ngroup >= 1 .and. ngroup <= nrank, ' <mpistart_group> Error: ngroup must >= 1 and <= nrank')
        call alloc_array(r, [0, ngroup - 1, 1, 2])
        call cut(0, nrank - 1, ngroup, r)
        do i = 0, ngroup - 1
            if (rankid >= r(i, 1) .and. rankid <= r(i, 2)) then
                groupid = i
                exit
            end if
        end do
        call mpi_comm_split(mpi_comm_world, groupid, nrank, mpi_group_comm, mpi_ierr)
        call mpi_comm_size(mpi_group_comm, nrank_group, mpi_ierr_group)
        call mpi_comm_rank(mpi_group_comm, rankid_group, mpi_ierr_group)
        ! write(error_unit, '(a, i6, a, i6, a, i6)') ' @ group ', groupid, ' with group id ', rankid_group, '/', nrank_group
    end subroutine mpistart_group
    subroutine mpiend_group
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_comm_free(mpi_group_comm, mpi_ierr_group)
    end subroutine mpiend_group
    subroutine mpibarrier_group
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end subroutine mpibarrier_group
    subroutine mpistop_group
        call mpi_abort(mpi_group_comm, mpi_err_other, mpi_ierr_group)
    end subroutine mpistop_group
    !
    !> Decompose a 2D domain into MPI blocks
    !
    subroutine domain_decomp_regular_2d_group(n1, n2, n1beg, n1end, n2beg, n2end, weights1, weights2)
        integer, intent(in) :: n1, n2
        integer, intent(out) :: n1beg, n1end, n2beg, n2end
        real, dimension(:), intent(in), optional :: weights1, weights2
        integer, allocatable, dimension(:, :) :: blk1, blk2
        integer :: i, j
        call mpi_comm_rank(mpi_group_comm, rankid_group, mpi_ierr_group)
        if (present(weights1) .and. present(weights2)) then
            ! If the domain is padded with special layers (like PML) where number of FLOP is higher per point
            call assert(size(weights1) == n1, ' <domain_decomp_regular_2d_group> Error: size(weights1) must = n1. ')
            call assert(size(weights2) == n2, ' <domain_decomp_regular_2d_group> Error: size(weights2) must = n2. ')
            blk1 = zeros(rank1_group, 2)
            blk2 = zeros(rank2_group, 2)
            call divide_domain(weights1, rank1_group, blk1)
            call divide_domain(weights2, rank2_group, blk2)
        else
            ! Otherwise, use simple cut where FLOP is same for all points
            call cut(1, n1, rank1_group, blk1)
            call cut(1, n2, rank2_group, blk2)
        end if
        if (rankid_group > rank1_group*rank2_group - 1) then
            n1beg = 1
            n1end = -1
            n2beg = 1
            n2end = -1
        else
            do j = 0, rank2_group - 1
                do i = 0, rank1_group - 1
                    if (rankid_group == j*rank1_group + i) then
                        n1beg = blk1(i + 1, 1)
                        n1end = blk1(i + 1, 2)
                        n2beg = blk2(j + 1, 1)
                        n2end = blk2(j + 1, 2)
                    end if
                end do
            end do
            ! Find adjacent blocks for each group
            do j = 0, rank2_group - 1
                do i = 0, rank1_group - 1
                    blockid_group = j*rank1_group + i
                    if (blockid_group == rankid_group) then
                        block_x1left_group = rankid_group - 1
                        block_x1right_group = rankid_group + 1
                        block_x2left_group = rankid_group - rank1_group
                        block_x2right_group = rankid_group + rank1_group
                        if (i == 0) then
                            block_x1left_group = mpi_proc_null
                        end if
                        if (i == rank1_group - 1) then
                            block_x1right_group = mpi_proc_null
                        end if
                        if (j == 0) then
                            block_x2left_group = mpi_proc_null
                        end if
                        if (j == rank2_group - 1) then
                            block_x2right_group = mpi_proc_null
                        end if
                    end if
                end do
            end do
        end if
    end subroutine domain_decomp_regular_2d_group
    !
    !> Decompose a 3D domain into MPI blocks
    !
    subroutine domain_decomp_regular_3d_group(n1, n2, n3, n1beg, n1end, n2beg, n2end, n3beg, n3end, weights1, weights2, weights3)
        integer, intent(in) :: n1, n2, n3
        integer, intent(out) :: n1beg, n1end, n2beg, n2end, n3beg, n3end
        real, dimension(:),intent(in), optional :: weights1, weights2, weights3
        integer, allocatable, dimension(:, :) :: blk1, blk2, blk3
        integer :: i, j, k
        call mpi_comm_rank(mpi_group_comm, rankid_group, mpi_ierr_group)
        if (present(weights1) .and. present(weights2)) then
            ! If the domain is padded with special layers (like PML) where number of FLOP is higher per point
            call assert(size(weights1) == n1, ' <domain_decomp_regular_3d_group> Error: size(weights1) must = n1. ')
            call assert(size(weights2) == n2, ' <domain_decomp_regular_3d_group> Error: size(weights2) must = n2. ')
            call assert(size(weights3) == n3, ' <domain_decomp_regular_3d_group> Error: size(weights3) must = n3. ')
            blk1 = zeros(rank1_group, 2)
            blk2 = zeros(rank2_group, 2)
            blk3 = zeros(rank3_group, 2)
            call divide_domain(weights1, rank1_group, blk1)
            call divide_domain(weights2, rank2_group, blk2)
            call divide_domain(weights3, rank3_group, blk3)
        else
            ! Otherwise, use simple cut where FLOP is same for all points
            call cut(1, n1, rank1_group, blk1)
            call cut(1, n2, rank2_group, blk2)
            call cut(1, n3, rank3_group, blk3)
        end if
        if (rankid_group > rank1_group*rank2_group*rank3_group - 1) then
            n1beg = 1
            n1end = -1
            n2beg = 1
            n2end = -1
            n3beg = 1
            n3end = -1
        else
            do k = 0, rank3_group - 1
                do j = 0, rank2_group - 1
                    do i = 0, rank1_group - 1
                        if (rankid_group == k*rank1_group*rank2_group + j*rank1_group + i) then
                            n1beg = blk1(i + 1, 1)
                            n1end = blk1(i + 1, 2)
                            n2beg = blk2(j + 1, 1)
                            n2end = blk2(j + 1, 2)
                            n3beg = blk3(k + 1, 1)
                            n3end = blk3(k + 1, 2)
                        end if
                    end do
                end do
            end do
            ! Find adjacent blocks for each group
            do k = 0, rank3_group - 1
                do j = 0, rank2_group - 1
                    do i = 0, rank1_group - 1
                        blockid_group = k*rank2_group*rank1_group + j*rank1_group + i
                        if (blockid_group == rankid_group) then
                            block_x1left_group = rankid_group - 1
                            block_x1right_group = rankid_group + 1
                            block_x2left_group = rankid_group - rank1_group
                            block_x2right_group = rankid_group + rank1_group
                            block_x3left_group = rankid_group - rank1_group*rank2_group
                            block_x3right_group = rankid_group + rank1_group*rank2_group
                            if (i == 0) then
                                block_x1left_group = mpi_proc_null
                            end if
                            if (i == rank1_group - 1) then
                                block_x1right_group = mpi_proc_null
                            end if
                            if (j == 0) then
                                block_x2left_group = mpi_proc_null
                            end if
                            if (j == rank2_group - 1) then
                                block_x2right_group = mpi_proc_null
                            end if
                            if (k == 0) then
                                block_x3left_group = mpi_proc_null
                            end if
                            if (k == rank3_group - 1) then
                                block_x3right_group = mpi_proc_null
                            end if
                        end if
                    end do
                end do
            end do
        end if
    end subroutine domain_decomp_regular_3d_group
    ! MPI array operations
!
! © 2025. Triad National Security, LLC. All rights reserved.
!
! This program was produced under U.S. Government contract 89233218CNA000001
! for Los Alamos National Laboratory (LANL), which is operated by
! Triad National Security, LLC for the U.S. Department of Energy/National Nuclear
! Security Administration. All rights in the program are reserved by
! Triad National Security, LLC, and the U.S. Department of Energy/National
! Nuclear Security Administration. The Government is granted for itself and
! others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide
! license in this material to reproduce, prepare. derivative works,
! distribute copies to the public, perform publicly and display publicly,
! and to permit others to do so.
!
! Author:
! Kai Gao, kaigao@lanl.gov
!
! The following is a copy-and-paste of template_mpicomm
! with slight modifications to group domain
!
! Broadcast
!
subroutine bcast_array_1d_group_int(w, source)
    integer, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_integer, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_1d_group_int
subroutine bcast_array_2d_group_int(w, source)
    integer, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_integer, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_2d_group_int
subroutine bcast_array_3d_group_int(w, source)
    integer, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_integer, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_3d_group_int
!
! Reduce
!
subroutine gather_group_int(w, target)
    integer, intent(inout) :: w
    integer, intent(in), optional :: target
    integer :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, 1, mpi_integer, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_group_int
subroutine gather_array_1d_group_int(w, target)
    integer, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: target
    integer, allocatable, dimension(:) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_integer, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_1d_group_int
subroutine gather_array_2d_group_int(w, target)
    integer, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: target
    integer, allocatable, dimension(:, :) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_integer, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_2d_group_int
subroutine gather_array_3d_group_int(w, target)
    integer, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: target
    integer, allocatable, dimension(:, :, :) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_integer, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_3d_group_int
!
! Send-recv
!
subroutine commute_array_1d_group_int(w, nl)
    integer, allocatable, dimension(:), intent(inout) :: w
    integer, intent(in) :: nl
    integer :: blks1, n1l, n1u
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    if (rank1_group > 1) then
        blks1 = nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1)), blks1, mpi_integer, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl), blks1, mpi_integer, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u), blks1, mpi_integer, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1), blks1, mpi_integer, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_1d_group_int
subroutine commute_array_2d_group_int(w, nl, dim)
    integer, allocatable, dimension(:, :), intent(inout) :: w
    integer, intent(in) :: nl
    integer, intent(in), optional :: dim
    integer :: blks1, blks2, n1l, n1u, n2l, n2u
    integer :: axis
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    n2l = lbound(w, 2) + nl
    n2u = ubound(w, 2) - nl
    if (present(dim)) then
        axis = dim
    else
        axis = 0
    end if
    if (rank1_group > 1 .and. (axis == 0 .or. axis == 1)) then
        blks1 = (n2u - n2l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1), n2l - nl:n2u + nl), blks1, mpi_integer, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl, n2l - nl:n2u + nl), blks1, mpi_integer, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u, n2l - nl:n2u + nl), blks1, mpi_integer, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1, n2l - nl:n2u + nl), blks1, mpi_integer, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank2_group > 1 .and. (axis == 0 .or. axis == 2)) then
        blks2 = (n1u - n1l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l:n2l + (nl - 1)), blks2, mpi_integer, block_x2left_group, 3, &
            w(n1l - nl:n1u + nl, n2u + 1:n2u + nl), blks2, mpi_integer, block_x2right_group, 3, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2u - (nl - 1):n2u), blks2, mpi_integer, block_x2right_group, 4, &
            w(n1l - nl:n1u + nl, n2l - nl:n2l - 1), blks2, mpi_integer, block_x2left_group, 4, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_2d_group_int
subroutine commute_array_3d_group_int(w, nl, dim)
    integer, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, intent(in) :: nl
    integer, intent(in), optional :: dim
    integer :: blks1, blks2, blks3, n1l, n1u, n2l, n2u, n3l, n3u
    integer :: axis
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    n2l = lbound(w, 2) + nl
    n2u = ubound(w, 2) - nl
    n3l = lbound(w, 3) + nl
    n3u = ubound(w, 3) - nl
    if (present(dim)) then
        axis = dim
    else
        axis = 0
    end if
    if (rank1_group > 1 .and. (axis == 0 .or. axis == 1)) then
        blks1 = (n2u - n2l + 1 + 2*nl)*(n3u - n3l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1), n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_integer, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_integer, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_integer, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_integer, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank2_group > 1 .and. (axis == 0 .or. axis == 2)) then
        blks2 = (n1u - n1l + 1 + 2*nl)*(n3u - n3l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l:n2l + (nl - 1), n3l - nl:n3u + nl), blks2, mpi_integer, block_x2left_group, 3, &
            w(n1l - nl:n1u + nl, n2u + 1:n2u + nl, n3l - nl:n3u + nl), blks2, mpi_integer, block_x2right_group, 3, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2u - (nl - 1):n2u, n3l - nl:n3u + nl), blks2, mpi_integer, block_x2right_group, 4, &
            w(n1l - nl:n1u + nl, n2l - nl:n2l - 1, n3l - nl:n3u + nl), blks2, mpi_integer, block_x2left_group, 4, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank3_group > 1 .and. (axis == 0 .or. axis == 3)) then
        blks3 = (n2u - n2l + 1 + 2*nl)*(n1u - n1l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3l:n3l + (nl - 1)), blks3, mpi_integer, block_x3left_group, 5, &
            w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3u + 1:n3u + nl), blks3, mpi_integer, block_x3right_group, 5, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3u - (nl - 1):n3u), blks3, mpi_integer, block_x3right_group, 6, &
            w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3l - nl:n3l - 1), blks3, mpi_integer, block_x3left_group, 6, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_3d_group_int
!
! Allreduce
!
subroutine gather_distribute_group_int(w)
    integer, intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, 1, mpi_integer, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_group_int
subroutine gather_distribute_array_1d_group_int(w)
    integer, dimension(:), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_integer, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_1d_group_int
subroutine gather_distribute_large_array_1d_group_int(w, nblock)
    integer, dimension(:), intent(inout) :: w
    integer, intent(in) :: nblock
    integer(kind=8) :: n1
    integer(kind=8), allocatable, dimension(:, :) :: blkrange
    integer :: i
    allocate (blkrange(0:nblock - 1, 1:2))
    n1 = size(w, 1)
    call cut(int(1, kind=8), n1, int(nblock, kind=8), blkrange)
    do i = 0, nblock - 1
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(mpi_in_place, w(blkrange(i, 1):blkrange(i, 2)), int(blkrange(i, 2) - blkrange(i, 1) + 1, kind=4), &
            mpi_integer, mpi_sum, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end do
end subroutine gather_distribute_large_array_1d_group_int
subroutine gather_distribute_array_2d_group_int(w)
    integer, dimension(:, :), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_integer, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_2d_group_int
subroutine gather_distribute_array_3d_group_int(w)
    integer, dimension(:, :, :), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_integer, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_3d_group_int
!
! © 2025. Triad National Security, LLC. All rights reserved.
!
! This program was produced under U.S. Government contract 89233218CNA000001
! for Los Alamos National Laboratory (LANL), which is operated by
! Triad National Security, LLC for the U.S. Department of Energy/National Nuclear
! Security Administration. All rights in the program are reserved by
! Triad National Security, LLC, and the U.S. Department of Energy/National
! Nuclear Security Administration. The Government is granted for itself and
! others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide
! license in this material to reproduce, prepare. derivative works,
! distribute copies to the public, perform publicly and display publicly,
! and to permit others to do so.
!
! Author:
! Kai Gao, kaigao@lanl.gov
!
! The following is a copy-and-paste of template_mpicomm
! with slight modifications to group domain
!
! Broadcast
!
subroutine bcast_array_1d_group_float(w, source)
    real, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_real, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_1d_group_float
subroutine bcast_array_2d_group_float(w, source)
    real, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_real, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_2d_group_float
subroutine bcast_array_3d_group_float(w, source)
    real, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_real, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_3d_group_float
!
! Reduce
!
subroutine gather_group_float(w, target)
    real, intent(inout) :: w
    integer, intent(in), optional :: target
    real :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, 1, mpi_real, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_group_float
subroutine gather_array_1d_group_float(w, target)
    real, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: target
    real, allocatable, dimension(:) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_real, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_1d_group_float
subroutine gather_array_2d_group_float(w, target)
    real, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: target
    real, allocatable, dimension(:, :) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_real, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_2d_group_float
subroutine gather_array_3d_group_float(w, target)
    real, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: target
    real, allocatable, dimension(:, :, :) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_real, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_3d_group_float
!
! Send-recv
!
subroutine commute_array_1d_group_float(w, nl)
    real, allocatable, dimension(:), intent(inout) :: w
    integer, intent(in) :: nl
    integer :: blks1, n1l, n1u
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    if (rank1_group > 1) then
        blks1 = nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1)), blks1, mpi_real, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl), blks1, mpi_real, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u), blks1, mpi_real, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1), blks1, mpi_real, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_1d_group_float
subroutine commute_array_2d_group_float(w, nl, dim)
    real, allocatable, dimension(:, :), intent(inout) :: w
    integer, intent(in) :: nl
    integer, intent(in), optional :: dim
    integer :: blks1, blks2, n1l, n1u, n2l, n2u
    integer :: axis
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    n2l = lbound(w, 2) + nl
    n2u = ubound(w, 2) - nl
    if (present(dim)) then
        axis = dim
    else
        axis = 0
    end if
    if (rank1_group > 1 .and. (axis == 0 .or. axis == 1)) then
        blks1 = (n2u - n2l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1), n2l - nl:n2u + nl), blks1, mpi_real, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl, n2l - nl:n2u + nl), blks1, mpi_real, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u, n2l - nl:n2u + nl), blks1, mpi_real, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1, n2l - nl:n2u + nl), blks1, mpi_real, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank2_group > 1 .and. (axis == 0 .or. axis == 2)) then
        blks2 = (n1u - n1l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l:n2l + (nl - 1)), blks2, mpi_real, block_x2left_group, 3, &
            w(n1l - nl:n1u + nl, n2u + 1:n2u + nl), blks2, mpi_real, block_x2right_group, 3, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2u - (nl - 1):n2u), blks2, mpi_real, block_x2right_group, 4, &
            w(n1l - nl:n1u + nl, n2l - nl:n2l - 1), blks2, mpi_real, block_x2left_group, 4, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_2d_group_float
subroutine commute_array_3d_group_float(w, nl, dim)
    real, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, intent(in) :: nl
    integer, intent(in), optional :: dim
    integer :: blks1, blks2, blks3, n1l, n1u, n2l, n2u, n3l, n3u
    integer :: axis
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    n2l = lbound(w, 2) + nl
    n2u = ubound(w, 2) - nl
    n3l = lbound(w, 3) + nl
    n3u = ubound(w, 3) - nl
    if (present(dim)) then
        axis = dim
    else
        axis = 0
    end if
    if (rank1_group > 1 .and. (axis == 0 .or. axis == 1)) then
        blks1 = (n2u - n2l + 1 + 2*nl)*(n3u - n3l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1), n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_real, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_real, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_real, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_real, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank2_group > 1 .and. (axis == 0 .or. axis == 2)) then
        blks2 = (n1u - n1l + 1 + 2*nl)*(n3u - n3l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l:n2l + (nl - 1), n3l - nl:n3u + nl), blks2, mpi_real, block_x2left_group, 3, &
            w(n1l - nl:n1u + nl, n2u + 1:n2u + nl, n3l - nl:n3u + nl), blks2, mpi_real, block_x2right_group, 3, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2u - (nl - 1):n2u, n3l - nl:n3u + nl), blks2, mpi_real, block_x2right_group, 4, &
            w(n1l - nl:n1u + nl, n2l - nl:n2l - 1, n3l - nl:n3u + nl), blks2, mpi_real, block_x2left_group, 4, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank3_group > 1 .and. (axis == 0 .or. axis == 3)) then
        blks3 = (n2u - n2l + 1 + 2*nl)*(n1u - n1l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3l:n3l + (nl - 1)), blks3, mpi_real, block_x3left_group, 5, &
            w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3u + 1:n3u + nl), blks3, mpi_real, block_x3right_group, 5, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3u - (nl - 1):n3u), blks3, mpi_real, block_x3right_group, 6, &
            w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3l - nl:n3l - 1), blks3, mpi_real, block_x3left_group, 6, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_3d_group_float
!
! Allreduce
!
subroutine gather_distribute_group_float(w)
    real, intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, 1, mpi_real, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_group_float
subroutine gather_distribute_array_1d_group_float(w)
    real, dimension(:), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_real, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_1d_group_float
subroutine gather_distribute_large_array_1d_group_float(w, nblock)
    real, dimension(:), intent(inout) :: w
    integer, intent(in) :: nblock
    integer(kind=8) :: n1
    integer(kind=8), allocatable, dimension(:, :) :: blkrange
    integer :: i
    allocate (blkrange(0:nblock - 1, 1:2))
    n1 = size(w, 1)
    call cut(int(1, kind=8), n1, int(nblock, kind=8), blkrange)
    do i = 0, nblock - 1
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(mpi_in_place, w(blkrange(i, 1):blkrange(i, 2)), int(blkrange(i, 2) - blkrange(i, 1) + 1, kind=4), &
            mpi_real, mpi_sum, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end do
end subroutine gather_distribute_large_array_1d_group_float
subroutine gather_distribute_array_2d_group_float(w)
    real, dimension(:, :), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_real, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_2d_group_float
subroutine gather_distribute_array_3d_group_float(w)
    real, dimension(:, :, :), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_real, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_3d_group_float
!
! © 2025. Triad National Security, LLC. All rights reserved.
!
! This program was produced under U.S. Government contract 89233218CNA000001
! for Los Alamos National Laboratory (LANL), which is operated by
! Triad National Security, LLC for the U.S. Department of Energy/National Nuclear
! Security Administration. All rights in the program are reserved by
! Triad National Security, LLC, and the U.S. Department of Energy/National
! Nuclear Security Administration. The Government is granted for itself and
! others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide
! license in this material to reproduce, prepare. derivative works,
! distribute copies to the public, perform publicly and display publicly,
! and to permit others to do so.
!
! Author:
! Kai Gao, kaigao@lanl.gov
!
! The following is a copy-and-paste of template_mpicomm
! with slight modifications to group domain
!
! Broadcast
!
subroutine bcast_array_1d_group_double(w, source)
    double precision, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_double_precision, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_1d_group_double
subroutine bcast_array_2d_group_double(w, source)
    double precision, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_double_precision, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_2d_group_double
subroutine bcast_array_3d_group_double(w, source)
    double precision, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_double_precision, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_3d_group_double
!
! Reduce
!
subroutine gather_group_double(w, target)
    double precision, intent(inout) :: w
    integer, intent(in), optional :: target
    double precision :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, 1, mpi_double_precision, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_group_double
subroutine gather_array_1d_group_double(w, target)
    double precision, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: target
    double precision, allocatable, dimension(:) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_double_precision, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_1d_group_double
subroutine gather_array_2d_group_double(w, target)
    double precision, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: target
    double precision, allocatable, dimension(:, :) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_double_precision, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_2d_group_double
subroutine gather_array_3d_group_double(w, target)
    double precision, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: target
    double precision, allocatable, dimension(:, :, :) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_double_precision, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_3d_group_double
!
! Send-recv
!
subroutine commute_array_1d_group_double(w, nl)
    double precision, allocatable, dimension(:), intent(inout) :: w
    integer, intent(in) :: nl
    integer :: blks1, n1l, n1u
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    if (rank1_group > 1) then
        blks1 = nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1)), blks1, mpi_double_precision, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl), blks1, mpi_double_precision, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u), blks1, mpi_double_precision, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1), blks1, mpi_double_precision, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_1d_group_double
subroutine commute_array_2d_group_double(w, nl, dim)
    double precision, allocatable, dimension(:, :), intent(inout) :: w
    integer, intent(in) :: nl
    integer, intent(in), optional :: dim
    integer :: blks1, blks2, n1l, n1u, n2l, n2u
    integer :: axis
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    n2l = lbound(w, 2) + nl
    n2u = ubound(w, 2) - nl
    if (present(dim)) then
        axis = dim
    else
        axis = 0
    end if
    if (rank1_group > 1 .and. (axis == 0 .or. axis == 1)) then
        blks1 = (n2u - n2l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1), n2l - nl:n2u + nl), blks1, mpi_double_precision, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl, n2l - nl:n2u + nl), blks1, mpi_double_precision, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u, n2l - nl:n2u + nl), blks1, mpi_double_precision, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1, n2l - nl:n2u + nl), blks1, mpi_double_precision, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank2_group > 1 .and. (axis == 0 .or. axis == 2)) then
        blks2 = (n1u - n1l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l:n2l + (nl - 1)), blks2, mpi_double_precision, block_x2left_group, 3, &
            w(n1l - nl:n1u + nl, n2u + 1:n2u + nl), blks2, mpi_double_precision, block_x2right_group, 3, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2u - (nl - 1):n2u), blks2, mpi_double_precision, block_x2right_group, 4, &
            w(n1l - nl:n1u + nl, n2l - nl:n2l - 1), blks2, mpi_double_precision, block_x2left_group, 4, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_2d_group_double
subroutine commute_array_3d_group_double(w, nl, dim)
    double precision, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, intent(in) :: nl
    integer, intent(in), optional :: dim
    integer :: blks1, blks2, blks3, n1l, n1u, n2l, n2u, n3l, n3u
    integer :: axis
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    n2l = lbound(w, 2) + nl
    n2u = ubound(w, 2) - nl
    n3l = lbound(w, 3) + nl
    n3u = ubound(w, 3) - nl
    if (present(dim)) then
        axis = dim
    else
        axis = 0
    end if
    if (rank1_group > 1 .and. (axis == 0 .or. axis == 1)) then
        blks1 = (n2u - n2l + 1 + 2*nl)*(n3u - n3l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1), n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_double_precision, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_double_precision, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_double_precision, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_double_precision, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank2_group > 1 .and. (axis == 0 .or. axis == 2)) then
        blks2 = (n1u - n1l + 1 + 2*nl)*(n3u - n3l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l:n2l + (nl - 1), n3l - nl:n3u + nl), blks2, mpi_double_precision, block_x2left_group, 3, &
            w(n1l - nl:n1u + nl, n2u + 1:n2u + nl, n3l - nl:n3u + nl), blks2, mpi_double_precision, block_x2right_group, 3, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2u - (nl - 1):n2u, n3l - nl:n3u + nl), blks2, mpi_double_precision, block_x2right_group, 4, &
            w(n1l - nl:n1u + nl, n2l - nl:n2l - 1, n3l - nl:n3u + nl), blks2, mpi_double_precision, block_x2left_group, 4, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank3_group > 1 .and. (axis == 0 .or. axis == 3)) then
        blks3 = (n2u - n2l + 1 + 2*nl)*(n1u - n1l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3l:n3l + (nl - 1)), blks3, mpi_double_precision, block_x3left_group, 5, &
            w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3u + 1:n3u + nl), blks3, mpi_double_precision, block_x3right_group, 5, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3u - (nl - 1):n3u), blks3, mpi_double_precision, block_x3right_group, 6, &
            w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3l - nl:n3l - 1), blks3, mpi_double_precision, block_x3left_group, 6, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_3d_group_double
!
! Allreduce
!
subroutine gather_distribute_group_double(w)
    double precision, intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, 1, mpi_double_precision, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_group_double
subroutine gather_distribute_array_1d_group_double(w)
    double precision, dimension(:), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_double_precision, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_1d_group_double
subroutine gather_distribute_large_array_1d_group_double(w, nblock)
    double precision, dimension(:), intent(inout) :: w
    integer, intent(in) :: nblock
    integer(kind=8) :: n1
    integer(kind=8), allocatable, dimension(:, :) :: blkrange
    integer :: i
    allocate (blkrange(0:nblock - 1, 1:2))
    n1 = size(w, 1)
    call cut(int(1, kind=8), n1, int(nblock, kind=8), blkrange)
    do i = 0, nblock - 1
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(mpi_in_place, w(blkrange(i, 1):blkrange(i, 2)), int(blkrange(i, 2) - blkrange(i, 1) + 1, kind=4), &
            mpi_double_precision, mpi_sum, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end do
end subroutine gather_distribute_large_array_1d_group_double
subroutine gather_distribute_array_2d_group_double(w)
    double precision, dimension(:, :), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_double_precision, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_2d_group_double
subroutine gather_distribute_array_3d_group_double(w)
    double precision, dimension(:, :, :), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_double_precision, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_3d_group_double
!
! © 2025. Triad National Security, LLC. All rights reserved.
!
! This program was produced under U.S. Government contract 89233218CNA000001
! for Los Alamos National Laboratory (LANL), which is operated by
! Triad National Security, LLC for the U.S. Department of Energy/National Nuclear
! Security Administration. All rights in the program are reserved by
! Triad National Security, LLC, and the U.S. Department of Energy/National
! Nuclear Security Administration. The Government is granted for itself and
! others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide
! license in this material to reproduce, prepare. derivative works,
! distribute copies to the public, perform publicly and display publicly,
! and to permit others to do so.
!
! Author:
! Kai Gao, kaigao@lanl.gov
!
! The following is a copy-and-paste of template_mpicomm
! with slight modifications to group domain
!
! Broadcast
!
subroutine bcast_array_1d_group_complex(w, source)
    complex, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_complex, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_1d_group_complex
subroutine bcast_array_2d_group_complex(w, source)
    complex, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_complex, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_2d_group_complex
subroutine bcast_array_3d_group_complex(w, source)
    complex, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_complex, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_3d_group_complex
!
! Reduce
!
subroutine gather_group_complex(w, target)
    complex, intent(inout) :: w
    integer, intent(in), optional :: target
    complex :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, 1, mpi_complex, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_group_complex
subroutine gather_array_1d_group_complex(w, target)
    complex, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: target
    complex, allocatable, dimension(:) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_complex, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_1d_group_complex
subroutine gather_array_2d_group_complex(w, target)
    complex, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: target
    complex, allocatable, dimension(:, :) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_complex, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_2d_group_complex
subroutine gather_array_3d_group_complex(w, target)
    complex, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: target
    complex, allocatable, dimension(:, :, :) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_complex, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_3d_group_complex
!
! Send-recv
!
subroutine commute_array_1d_group_complex(w, nl)
    complex, allocatable, dimension(:), intent(inout) :: w
    integer, intent(in) :: nl
    integer :: blks1, n1l, n1u
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    if (rank1_group > 1) then
        blks1 = nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1)), blks1, mpi_complex, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl), blks1, mpi_complex, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u), blks1, mpi_complex, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1), blks1, mpi_complex, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_1d_group_complex
subroutine commute_array_2d_group_complex(w, nl, dim)
    complex, allocatable, dimension(:, :), intent(inout) :: w
    integer, intent(in) :: nl
    integer, intent(in), optional :: dim
    integer :: blks1, blks2, n1l, n1u, n2l, n2u
    integer :: axis
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    n2l = lbound(w, 2) + nl
    n2u = ubound(w, 2) - nl
    if (present(dim)) then
        axis = dim
    else
        axis = 0
    end if
    if (rank1_group > 1 .and. (axis == 0 .or. axis == 1)) then
        blks1 = (n2u - n2l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1), n2l - nl:n2u + nl), blks1, mpi_complex, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl, n2l - nl:n2u + nl), blks1, mpi_complex, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u, n2l - nl:n2u + nl), blks1, mpi_complex, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1, n2l - nl:n2u + nl), blks1, mpi_complex, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank2_group > 1 .and. (axis == 0 .or. axis == 2)) then
        blks2 = (n1u - n1l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l:n2l + (nl - 1)), blks2, mpi_complex, block_x2left_group, 3, &
            w(n1l - nl:n1u + nl, n2u + 1:n2u + nl), blks2, mpi_complex, block_x2right_group, 3, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2u - (nl - 1):n2u), blks2, mpi_complex, block_x2right_group, 4, &
            w(n1l - nl:n1u + nl, n2l - nl:n2l - 1), blks2, mpi_complex, block_x2left_group, 4, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_2d_group_complex
subroutine commute_array_3d_group_complex(w, nl, dim)
    complex, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, intent(in) :: nl
    integer, intent(in), optional :: dim
    integer :: blks1, blks2, blks3, n1l, n1u, n2l, n2u, n3l, n3u
    integer :: axis
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    n2l = lbound(w, 2) + nl
    n2u = ubound(w, 2) - nl
    n3l = lbound(w, 3) + nl
    n3u = ubound(w, 3) - nl
    if (present(dim)) then
        axis = dim
    else
        axis = 0
    end if
    if (rank1_group > 1 .and. (axis == 0 .or. axis == 1)) then
        blks1 = (n2u - n2l + 1 + 2*nl)*(n3u - n3l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1), n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_complex, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_complex, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_complex, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_complex, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank2_group > 1 .and. (axis == 0 .or. axis == 2)) then
        blks2 = (n1u - n1l + 1 + 2*nl)*(n3u - n3l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l:n2l + (nl - 1), n3l - nl:n3u + nl), blks2, mpi_complex, block_x2left_group, 3, &
            w(n1l - nl:n1u + nl, n2u + 1:n2u + nl, n3l - nl:n3u + nl), blks2, mpi_complex, block_x2right_group, 3, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2u - (nl - 1):n2u, n3l - nl:n3u + nl), blks2, mpi_complex, block_x2right_group, 4, &
            w(n1l - nl:n1u + nl, n2l - nl:n2l - 1, n3l - nl:n3u + nl), blks2, mpi_complex, block_x2left_group, 4, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank3_group > 1 .and. (axis == 0 .or. axis == 3)) then
        blks3 = (n2u - n2l + 1 + 2*nl)*(n1u - n1l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3l:n3l + (nl - 1)), blks3, mpi_complex, block_x3left_group, 5, &
            w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3u + 1:n3u + nl), blks3, mpi_complex, block_x3right_group, 5, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3u - (nl - 1):n3u), blks3, mpi_complex, block_x3right_group, 6, &
            w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3l - nl:n3l - 1), blks3, mpi_complex, block_x3left_group, 6, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_3d_group_complex
!
! Allreduce
!
subroutine gather_distribute_group_complex(w)
    complex, intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, 1, mpi_complex, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_group_complex
subroutine gather_distribute_array_1d_group_complex(w)
    complex, dimension(:), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_complex, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_1d_group_complex
subroutine gather_distribute_large_array_1d_group_complex(w, nblock)
    complex, dimension(:), intent(inout) :: w
    integer, intent(in) :: nblock
    integer(kind=8) :: n1
    integer(kind=8), allocatable, dimension(:, :) :: blkrange
    integer :: i
    allocate (blkrange(0:nblock - 1, 1:2))
    n1 = size(w, 1)
    call cut(int(1, kind=8), n1, int(nblock, kind=8), blkrange)
    do i = 0, nblock - 1
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(mpi_in_place, w(blkrange(i, 1):blkrange(i, 2)), int(blkrange(i, 2) - blkrange(i, 1) + 1, kind=4), &
            mpi_complex, mpi_sum, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end do
end subroutine gather_distribute_large_array_1d_group_complex
subroutine gather_distribute_array_2d_group_complex(w)
    complex, dimension(:, :), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_complex, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_2d_group_complex
subroutine gather_distribute_array_3d_group_complex(w)
    complex, dimension(:, :, :), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_complex, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_3d_group_complex
!
! © 2025. Triad National Security, LLC. All rights reserved.
!
! This program was produced under U.S. Government contract 89233218CNA000001
! for Los Alamos National Laboratory (LANL), which is operated by
! Triad National Security, LLC for the U.S. Department of Energy/National Nuclear
! Security Administration. All rights in the program are reserved by
! Triad National Security, LLC, and the U.S. Department of Energy/National
! Nuclear Security Administration. The Government is granted for itself and
! others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide
! license in this material to reproduce, prepare. derivative works,
! distribute copies to the public, perform publicly and display publicly,
! and to permit others to do so.
!
! Author:
! Kai Gao, kaigao@lanl.gov
!
! The following is a copy-and-paste of template_mpicomm
! with slight modifications to group domain
!
! Broadcast
!
subroutine bcast_array_1d_group_dcomplex(w, source)
    double complex, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_double_complex, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_1d_group_dcomplex
subroutine bcast_array_2d_group_dcomplex(w, source)
    double complex, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_double_complex, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_2d_group_dcomplex
subroutine bcast_array_3d_group_dcomplex(w, source)
    double complex, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: source
    integer :: rid
    if (present(source)) then
        rid = source
    else
        rid = 0
    end if
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_bcast(w, size(w), mpi_double_complex, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine bcast_array_3d_group_dcomplex
!
! Reduce
!
subroutine gather_group_dcomplex(w, target)
    double complex, intent(inout) :: w
    integer, intent(in), optional :: target
    double complex :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, 1, mpi_double_complex, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_group_dcomplex
subroutine gather_array_1d_group_dcomplex(w, target)
    double complex, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: target
    double complex, allocatable, dimension(:) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_double_complex, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_1d_group_dcomplex
subroutine gather_array_2d_group_dcomplex(w, target)
    double complex, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: target
    double complex, allocatable, dimension(:, :) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_double_complex, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_2d_group_dcomplex
subroutine gather_array_3d_group_dcomplex(w, target)
    double complex, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: target
    double complex, allocatable, dimension(:, :, :) :: wlocal
    integer :: rid
    if (present(target)) then
        rid = target
    else
        rid = 0
    end if
    wlocal = w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_reduce(w, wlocal, size(w), mpi_double_complex, mpi_sum, rid, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    w = wlocal
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_array_3d_group_dcomplex
!
! Send-recv
!
subroutine commute_array_1d_group_dcomplex(w, nl)
    double complex, allocatable, dimension(:), intent(inout) :: w
    integer, intent(in) :: nl
    integer :: blks1, n1l, n1u
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    if (rank1_group > 1) then
        blks1 = nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1)), blks1, mpi_double_complex, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl), blks1, mpi_double_complex, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u), blks1, mpi_double_complex, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1), blks1, mpi_double_complex, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_1d_group_dcomplex
subroutine commute_array_2d_group_dcomplex(w, nl, dim)
    double complex, allocatable, dimension(:, :), intent(inout) :: w
    integer, intent(in) :: nl
    integer, intent(in), optional :: dim
    integer :: blks1, blks2, n1l, n1u, n2l, n2u
    integer :: axis
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    n2l = lbound(w, 2) + nl
    n2u = ubound(w, 2) - nl
    if (present(dim)) then
        axis = dim
    else
        axis = 0
    end if
    if (rank1_group > 1 .and. (axis == 0 .or. axis == 1)) then
        blks1 = (n2u - n2l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1), n2l - nl:n2u + nl), blks1, mpi_double_complex, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl, n2l - nl:n2u + nl), blks1, mpi_double_complex, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u, n2l - nl:n2u + nl), blks1, mpi_double_complex, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1, n2l - nl:n2u + nl), blks1, mpi_double_complex, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank2_group > 1 .and. (axis == 0 .or. axis == 2)) then
        blks2 = (n1u - n1l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l:n2l + (nl - 1)), blks2, mpi_double_complex, block_x2left_group, 3, &
            w(n1l - nl:n1u + nl, n2u + 1:n2u + nl), blks2, mpi_double_complex, block_x2right_group, 3, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2u - (nl - 1):n2u), blks2, mpi_double_complex, block_x2right_group, 4, &
            w(n1l - nl:n1u + nl, n2l - nl:n2l - 1), blks2, mpi_double_complex, block_x2left_group, 4, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_2d_group_dcomplex
subroutine commute_array_3d_group_dcomplex(w, nl, dim)
    double complex, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, intent(in) :: nl
    integer, intent(in), optional :: dim
    integer :: blks1, blks2, blks3, n1l, n1u, n2l, n2u, n3l, n3u
    integer :: axis
    n1l = lbound(w, 1) + nl
    n1u = ubound(w, 1) - nl
    n2l = lbound(w, 2) + nl
    n2u = ubound(w, 2) - nl
    n3l = lbound(w, 3) + nl
    n3u = ubound(w, 3) - nl
    if (present(dim)) then
        axis = dim
    else
        axis = 0
    end if
    if (rank1_group > 1 .and. (axis == 0 .or. axis == 1)) then
        blks1 = (n2u - n2l + 1 + 2*nl)*(n3u - n3l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l:n1l + (nl - 1), n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_double_complex, block_x1left_group, 1, &
            w(n1u + 1:n1u + nl, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_double_complex, block_x1right_group, 1, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1u - (nl - 1):n1u, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_double_complex, block_x1right_group, 2, &
            w(n1l - nl:n1l - 1, n2l - nl:n2u + nl, n3l - nl:n3u + nl), blks1, mpi_double_complex, block_x1left_group, 2, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank2_group > 1 .and. (axis == 0 .or. axis == 2)) then
        blks2 = (n1u - n1l + 1 + 2*nl)*(n3u - n3l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l:n2l + (nl - 1), n3l - nl:n3u + nl), blks2, mpi_double_complex, block_x2left_group, 3, &
            w(n1l - nl:n1u + nl, n2u + 1:n2u + nl, n3l - nl:n3u + nl), blks2, mpi_double_complex, block_x2right_group, 3, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2u - (nl - 1):n2u, n3l - nl:n3u + nl), blks2, mpi_double_complex, block_x2right_group, 4, &
            w(n1l - nl:n1u + nl, n2l - nl:n2l - 1, n3l - nl:n3u + nl), blks2, mpi_double_complex, block_x2left_group, 4, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
    if (rank3_group > 1 .and. (axis == 0 .or. axis == 3)) then
        blks3 = (n2u - n2l + 1 + 2*nl)*(n1u - n1l + 1 + 2*nl)*nl
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3l:n3l + (nl - 1)), blks3, mpi_double_complex, block_x3left_group, 5, &
            w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3u + 1:n3u + nl), blks3, mpi_double_complex, block_x3right_group, 5, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
        call mpi_sendrecv(w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3u - (nl - 1):n3u), blks3, mpi_double_complex, block_x3right_group, 6, &
            w(n1l - nl:n1u + nl, n2l - nl:n2u + nl, n3l - nl:n3l - 1), blks3, mpi_double_complex, block_x3left_group, 6, mpi_group_comm, mpi_stats_group, mpi_ierr_group)
    end if
end subroutine commute_array_3d_group_dcomplex
!
! Allreduce
!
subroutine gather_distribute_group_dcomplex(w)
    double complex, intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, 1, mpi_double_complex, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_group_dcomplex
subroutine gather_distribute_array_1d_group_dcomplex(w)
    double complex, dimension(:), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_double_complex, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_1d_group_dcomplex
subroutine gather_distribute_large_array_1d_group_dcomplex(w, nblock)
    double complex, dimension(:), intent(inout) :: w
    integer, intent(in) :: nblock
    integer(kind=8) :: n1
    integer(kind=8), allocatable, dimension(:, :) :: blkrange
    integer :: i
    allocate (blkrange(0:nblock - 1, 1:2))
    n1 = size(w, 1)
    call cut(int(1, kind=8), n1, int(nblock, kind=8), blkrange)
    do i = 0, nblock - 1
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(mpi_in_place, w(blkrange(i, 1):blkrange(i, 2)), int(blkrange(i, 2) - blkrange(i, 1) + 1, kind=4), &
            mpi_double_complex, mpi_sum, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end do
end subroutine gather_distribute_large_array_1d_group_dcomplex
subroutine gather_distribute_array_2d_group_dcomplex(w)
    double complex, dimension(:, :), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_double_complex, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_2d_group_dcomplex
subroutine gather_distribute_array_3d_group_dcomplex(w)
    double complex, dimension(:, :, :), intent(inout) :: w
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    call mpi_allreduce(mpi_in_place, w, size(w), mpi_double_complex, mpi_sum, mpi_group_comm, mpi_ierr_group)
    call mpi_barrier(mpi_group_comm, mpi_ierr_group)
end subroutine gather_distribute_array_3d_group_dcomplex
    ! For string
    subroutine bcast_array_1d_group_string(w, rankid)
        character(len=*), intent(inout) :: w
        integer, intent(in), optional :: rankid
        integer :: rid
        if (present(rankid)) then
            rid = rankid
        else
            rid = 0
        end if
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_bcast(w, len(w), mpi_character, rid, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end subroutine
    ! Min max
    function mpi_group_min_int(w) result(m)
        integer :: w
        integer :: m
        integer :: local_min
        local_min = w
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(local_min, m, 1, mpi_integer, mpi_min, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end function
    function mpi_group_min_float(w) result(m)
        real :: w
        real :: m
        real :: local_min
        local_min = w
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(local_min, m, 1, mpi_real, mpi_min, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end function
    function mpi_group_min_double(w) result(m)
        double precision :: w
        double precision :: m
        double precision :: local_min
        local_min = w
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(local_min, m, 1, mpi_double_precision, mpi_min, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end function
    ! Min max
    function mpi_group_max_int(w) result(m)
        integer :: w
        integer :: m
        integer :: local_max
        local_max = w
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(local_max, m, 1, mpi_integer, mpi_max, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end function
    function mpi_group_max_float(w) result(m)
        real :: w
        real :: m
        real :: local_max
        local_max = w
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(local_max, m, 1, mpi_real, mpi_max, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end function
    function mpi_group_max_double(w) result(m)
        double precision :: w
        double precision :: m
        double precision :: local_max
        local_max = w
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(local_max, m, 1, mpi_double_precision, mpi_max, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end function
    ! Logical
    function mpi_group_and(w) result(m)
        logical :: w
        logical :: m
        logical :: local_and
        local_and = w
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(local_and, m, 1, mpi_logical, mpi_land, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end function
    function mpi_group_or(w) result(m)
        logical :: w
        logical :: m
        logical :: local_or
        local_or = w
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
        call mpi_allreduce(local_or, m, 1, mpi_logical, mpi_lor, mpi_group_comm, mpi_ierr_group)
        call mpi_barrier(mpi_group_comm, mpi_ierr_group)
    end function
end module libflit_mpicomm_group
