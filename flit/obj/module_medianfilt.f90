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
! © 2024. Triad National Security, LLC. All rights reserved.
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
module libflit_medianfilt
    use libflit_array
    use libflit_statistics
    use libflit_error
    use libflit_array_operation
    implicit none
    interface median_filt
        module procedure :: median_filt_1d_float
        module procedure :: median_filt_2d_float
        module procedure :: median_filt_3d_float
        module procedure :: median_filt_1d_double
        module procedure :: median_filt_2d_double
        module procedure :: median_filt_3d_double
    end interface
    private
    public :: median_filt
contains
!
! © 2024. Triad National Security, LLC. All rights reserved.
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
function median_filt_1d_float(w, nw) result(wr)
    real, dimension(:), intent(in) :: w
    integer, intent(in), optional :: nw
    integer :: i, n1, nw1
    real, allocatable, dimension(:) :: wr
    n1 = size(w)
    if (present(nw)) then
        nw1 = nw
    else
        nw1 = 1
    end if
    allocate(wr(1:n1))
    do i = 1, n1
        wr(i) = median(w(max(1, i - nw1):min(n1, i + nw1)))
    end do
end function median_filt_1d_float
function median_filt_2d_float(w, nw) result(wr)
    real, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in), optional :: nw
    integer :: i, j, n1, n2, nw1, nw2
    real, allocatable, dimension(:, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    if (present(nw)) then
        nw1 = nw(1)
        nw2 = nw(2)
    else
        nw1 = 1
        nw2 = 1
    end if
    allocate(wr(1:n1, 1:n2))
    !$omp parallel do private(i, j)
    do j = 1, n2
        do i = 1, n1
            wr(i, j) = median(w(max(1, i - nw1):min(n1, i + nw1), &
                max(1, j - nw2):min(n2, j + nw2)))
        end do
    end do
    !$omp end parallel do
end function median_filt_2d_float
function median_filt_3d_float(w, nw) result(wr)
    real, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in), optional :: nw
    integer :: i, j, k, n1, n2, n3, nw1, nw2, nw3
    real, allocatable, dimension(:, :, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    if (present(nw)) then
        nw1 = nw(1)
        nw2 = nw(2)
        nw3 = nw(3)
    else
        nw1 = 1
        nw2 = 1
        nw3 = 1
    end if
    allocate (wr(1:n1, 1:n2, 1:n3))
    !$omp parallel do private(i, j, k)
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                wr(i, j, k) = median(w( &
                    max(1, i - nw1):min(n1, i + nw1), &
                    max(1, j - nw2):min(n2, j + nw2), &
                    max(1, k - nw3):min(n3, k + nw3)))
            end do
        end do
    end do
    !$omp end parallel do
end function median_filt_3d_float
!
! © 2024. Triad National Security, LLC. All rights reserved.
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
function median_filt_1d_double(w, nw) result(wr)
    double precision, dimension(:), intent(in) :: w
    integer, intent(in), optional :: nw
    integer :: i, n1, nw1
    double precision, allocatable, dimension(:) :: wr
    n1 = size(w)
    if (present(nw)) then
        nw1 = nw
    else
        nw1 = 1
    end if
    allocate(wr(1:n1))
    do i = 1, n1
        wr(i) = median(w(max(1, i - nw1):min(n1, i + nw1)))
    end do
end function median_filt_1d_double
function median_filt_2d_double(w, nw) result(wr)
    double precision, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in), optional :: nw
    integer :: i, j, n1, n2, nw1, nw2
    double precision, allocatable, dimension(:, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    if (present(nw)) then
        nw1 = nw(1)
        nw2 = nw(2)
    else
        nw1 = 1
        nw2 = 1
    end if
    allocate(wr(1:n1, 1:n2))
    !$omp parallel do private(i, j)
    do j = 1, n2
        do i = 1, n1
            wr(i, j) = median(w(max(1, i - nw1):min(n1, i + nw1), &
                max(1, j - nw2):min(n2, j + nw2)))
        end do
    end do
    !$omp end parallel do
end function median_filt_2d_double
function median_filt_3d_double(w, nw) result(wr)
    double precision, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in), optional :: nw
    integer :: i, j, k, n1, n2, n3, nw1, nw2, nw3
    double precision, allocatable, dimension(:, :, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    if (present(nw)) then
        nw1 = nw(1)
        nw2 = nw(2)
        nw3 = nw(3)
    else
        nw1 = 1
        nw2 = 1
        nw3 = 1
    end if
    allocate (wr(1:n1, 1:n2, 1:n3))
    !$omp parallel do private(i, j, k)
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                wr(i, j, k) = median(w( &
                    max(1, i - nw1):min(n1, i + nw1), &
                    max(1, j - nw2):min(n2, j + nw2), &
                    max(1, k - nw3):min(n3, k + nw3)))
            end do
        end do
    end do
    !$omp end parallel do
end function median_filt_3d_double
end module libflit_medianfilt
