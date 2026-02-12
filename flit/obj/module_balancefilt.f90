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
module libflit_balancefilt
    use libflit_array
    use libflit_utility
    use libflit_constants
    use libflit_array_operation
    implicit none
    !
    !> Windowed RMS balance
    !
    interface balance_filt
        module procedure :: balance_filt_1d_float
        module procedure :: balance_filt_2d_float
        module procedure :: balance_filt_3d_float
        module procedure :: balance_filt_1d_double
        module procedure :: balance_filt_2d_double
        module procedure :: balance_filt_3d_double
        module procedure :: balance_filt_1d_complex
        module procedure :: balance_filt_2d_complex
        module procedure :: balance_filt_3d_complex
        module procedure :: balance_filt_1d_dcomplex
        module procedure :: balance_filt_2d_dcomplex
        module procedure :: balance_filt_3d_dcomplex
    end interface balance_filt
    private
    public :: balance_filt
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
!
!> 1D moving balance by dividing energy
!
function balance_filt_1d_float(w, radius, eps) result(wp)
    real, dimension(:) :: w
    integer :: radius
    real, optional :: eps
    real, allocatable, dimension(:) :: wp
    integer :: n1, i, od
    real, allocatable, dimension(:) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w)
    ww = zeros(n1)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius, radius], ['symm', 'symm'])
    call alloc_array(wr, [-radius, radius])
    do i = 1, n1
        wr = wp(i - radius:i + radius)
        scalar = epslevel*maxval(abs(wr))
        if (scalar /= 0) then
            ww(i) = sum(wr) + scalar
        else
            ww(i) = float_tiny*(radius + 1)*2
        end if
    end do
    ww = sqrt(ww/(2*radius + 1))
    wp = real((w/10.0d0**od)/ww)
end function balance_filt_1d_float
!
!> 2D moving balance by dividing energy
!
function balance_filt_2d_float(w, radius, eps) result(wp)
    real, dimension(:, :) :: w
    integer, dimension(:) :: radius
    real, optional :: eps
    real, allocatable, dimension(:, :) :: wp
    integer :: n1, n2, i, j, od
    real, allocatable, dimension(:, :) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w, 1)
    n2 = size(w, 2)
    ww = zeros(n1, n2)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius(1), radius(1), radius(2), radius(2)], &
        ['symm', 'symm', 'symm', 'symm'])
    call alloc_array(wr, [-radius(1), radius(1), -radius(2), radius(2)])
    !$omp parallel do private(i, j, wr, scalar) collapse(2)
    do j = 1, n2
        do i = 1, n1
            wr = wp(i - radius(1):i + radius(1), j - radius(2):j + radius(2))
            scalar = epslevel*maxval(abs(wr))
            if (scalar /= 0) then
                ww(i, j) = sum(wr) + scalar
            else
                ww(i, j) = float_tiny*product(radius + 1)*4
            end if
        end do
    end do
    !$omp end parallel do
    ww = sqrt(ww/(2*radius(1) + 1)/(2*radius(2) + 1))
    wp = real((w/10.0d0**od)/ww)
end function balance_filt_2d_float
!
!> 3D moving balance by dividing energy
!
function balance_filt_3d_float(w, radius, eps) result(wp)
    real, dimension(:, :, :) :: w
    integer, dimension(:) :: radius
    real, optional :: eps
    real, allocatable, dimension(:, :, :) :: wp
    integer :: n1, n2, n3, i, j, k, od
    real, allocatable, dimension(:, :, :) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ww = zeros(n1, n2, n3)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius(1), radius(1), radius(2), radius(2), radius(3), radius(3)], &
        ['symm', 'symm', 'symm', 'symm', 'symm', 'symm'])
    call alloc_array(wr, [-radius(1), radius(1), -radius(2), radius(2), -radius(3), radius(3)])
    !$omp parallel do private(i, j, k, wr, scalar) collapse(3)
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                wr = wp(i - radius(1):i + radius(1), &
                    j - radius(2):j + radius(2), &
                    k - radius(3):k + radius(3))
                scalar = epslevel*maxval(abs(wr))
                if (scalar /= 0) then
                    ww(i, j, k) = sum(wr) + scalar
                else
                    ww(i, j, k) = float_tiny*product(radius + 1)*8
                end if
            end do
        end do
    end do
    !$omp end parallel do
    ww = sqrt(ww/(2*radius(1) + 1)/(2*radius(2) + 1)/(2*radius(3) + 1))
    wp = real((w/10.0d0**od)/ww)
end function balance_filt_3d_float
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
!
!> 1D moving balance by dividing energy
!
function balance_filt_1d_double(w, radius, eps) result(wp)
    double precision, dimension(:) :: w
    integer :: radius
    real, optional :: eps
    double precision, allocatable, dimension(:) :: wp
    integer :: n1, i, od
    double precision, allocatable, dimension(:) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w)
    ww = zeros(n1)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius, radius], ['symm', 'symm'])
    call alloc_array(wr, [-radius, radius])
    do i = 1, n1
        wr = wp(i - radius:i + radius)
        scalar = epslevel*maxval(abs(wr))
        if (scalar /= 0) then
            ww(i) = sum(wr) + scalar
        else
            ww(i) = float_tiny*(radius + 1)*2
        end if
    end do
    ww = sqrt(ww/(2*radius + 1))
    wp = dble((w/10.0d0**od)/ww)
end function balance_filt_1d_double
!
!> 2D moving balance by dividing energy
!
function balance_filt_2d_double(w, radius, eps) result(wp)
    double precision, dimension(:, :) :: w
    integer, dimension(:) :: radius
    real, optional :: eps
    double precision, allocatable, dimension(:, :) :: wp
    integer :: n1, n2, i, j, od
    double precision, allocatable, dimension(:, :) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w, 1)
    n2 = size(w, 2)
    ww = zeros(n1, n2)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius(1), radius(1), radius(2), radius(2)], &
        ['symm', 'symm', 'symm', 'symm'])
    call alloc_array(wr, [-radius(1), radius(1), -radius(2), radius(2)])
    !$omp parallel do private(i, j, wr, scalar) collapse(2)
    do j = 1, n2
        do i = 1, n1
            wr = wp(i - radius(1):i + radius(1), j - radius(2):j + radius(2))
            scalar = epslevel*maxval(abs(wr))
            if (scalar /= 0) then
                ww(i, j) = sum(wr) + scalar
            else
                ww(i, j) = float_tiny*product(radius + 1)*4
            end if
        end do
    end do
    !$omp end parallel do
    ww = sqrt(ww/(2*radius(1) + 1)/(2*radius(2) + 1))
    wp = dble((w/10.0d0**od)/ww)
end function balance_filt_2d_double
!
!> 3D moving balance by dividing energy
!
function balance_filt_3d_double(w, radius, eps) result(wp)
    double precision, dimension(:, :, :) :: w
    integer, dimension(:) :: radius
    real, optional :: eps
    double precision, allocatable, dimension(:, :, :) :: wp
    integer :: n1, n2, n3, i, j, k, od
    double precision, allocatable, dimension(:, :, :) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ww = zeros(n1, n2, n3)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius(1), radius(1), radius(2), radius(2), radius(3), radius(3)], &
        ['symm', 'symm', 'symm', 'symm', 'symm', 'symm'])
    call alloc_array(wr, [-radius(1), radius(1), -radius(2), radius(2), -radius(3), radius(3)])
    !$omp parallel do private(i, j, k, wr, scalar) collapse(3)
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                wr = wp(i - radius(1):i + radius(1), &
                    j - radius(2):j + radius(2), &
                    k - radius(3):k + radius(3))
                scalar = epslevel*maxval(abs(wr))
                if (scalar /= 0) then
                    ww(i, j, k) = sum(wr) + scalar
                else
                    ww(i, j, k) = float_tiny*product(radius + 1)*8
                end if
            end do
        end do
    end do
    !$omp end parallel do
    ww = sqrt(ww/(2*radius(1) + 1)/(2*radius(2) + 1)/(2*radius(3) + 1))
    wp = dble((w/10.0d0**od)/ww)
end function balance_filt_3d_double
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
!
!> 1D moving balance by dividing energy
!
function balance_filt_1d_complex(w, radius, eps) result(wp)
    complex, dimension(:) :: w
    integer :: radius
    real, optional :: eps
    complex, allocatable, dimension(:) :: wp
    integer :: n1, i, od
    complex, allocatable, dimension(:) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w)
    ww = zeros(n1)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius, radius], ['symm', 'symm'])
    call alloc_array(wr, [-radius, radius])
    do i = 1, n1
        wr = wp(i - radius:i + radius)
        scalar = epslevel*maxval(abs(wr))
        if (scalar /= 0) then
            ww(i) = sum(wr) + scalar
        else
            ww(i) = float_tiny*(radius + 1)*2
        end if
    end do
    ww = sqrt(ww/(2*radius + 1))
    wp = cmplx((w/10.0d0**od)/ww)
end function balance_filt_1d_complex
!
!> 2D moving balance by dividing energy
!
function balance_filt_2d_complex(w, radius, eps) result(wp)
    complex, dimension(:, :) :: w
    integer, dimension(:) :: radius
    real, optional :: eps
    complex, allocatable, dimension(:, :) :: wp
    integer :: n1, n2, i, j, od
    complex, allocatable, dimension(:, :) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w, 1)
    n2 = size(w, 2)
    ww = zeros(n1, n2)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius(1), radius(1), radius(2), radius(2)], &
        ['symm', 'symm', 'symm', 'symm'])
    call alloc_array(wr, [-radius(1), radius(1), -radius(2), radius(2)])
    !$omp parallel do private(i, j, wr, scalar) collapse(2)
    do j = 1, n2
        do i = 1, n1
            wr = wp(i - radius(1):i + radius(1), j - radius(2):j + radius(2))
            scalar = epslevel*maxval(abs(wr))
            if (scalar /= 0) then
                ww(i, j) = sum(wr) + scalar
            else
                ww(i, j) = float_tiny*product(radius + 1)*4
            end if
        end do
    end do
    !$omp end parallel do
    ww = sqrt(ww/(2*radius(1) + 1)/(2*radius(2) + 1))
    wp = cmplx((w/10.0d0**od)/ww)
end function balance_filt_2d_complex
!
!> 3D moving balance by dividing energy
!
function balance_filt_3d_complex(w, radius, eps) result(wp)
    complex, dimension(:, :, :) :: w
    integer, dimension(:) :: radius
    real, optional :: eps
    complex, allocatable, dimension(:, :, :) :: wp
    integer :: n1, n2, n3, i, j, k, od
    complex, allocatable, dimension(:, :, :) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ww = zeros(n1, n2, n3)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius(1), radius(1), radius(2), radius(2), radius(3), radius(3)], &
        ['symm', 'symm', 'symm', 'symm', 'symm', 'symm'])
    call alloc_array(wr, [-radius(1), radius(1), -radius(2), radius(2), -radius(3), radius(3)])
    !$omp parallel do private(i, j, k, wr, scalar) collapse(3)
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                wr = wp(i - radius(1):i + radius(1), &
                    j - radius(2):j + radius(2), &
                    k - radius(3):k + radius(3))
                scalar = epslevel*maxval(abs(wr))
                if (scalar /= 0) then
                    ww(i, j, k) = sum(wr) + scalar
                else
                    ww(i, j, k) = float_tiny*product(radius + 1)*8
                end if
            end do
        end do
    end do
    !$omp end parallel do
    ww = sqrt(ww/(2*radius(1) + 1)/(2*radius(2) + 1)/(2*radius(3) + 1))
    wp = cmplx((w/10.0d0**od)/ww)
end function balance_filt_3d_complex
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
!
!> 1D moving balance by dividing energy
!
function balance_filt_1d_dcomplex(w, radius, eps) result(wp)
    double complex, dimension(:) :: w
    integer :: radius
    real, optional :: eps
    double complex, allocatable, dimension(:) :: wp
    integer :: n1, i, od
    double complex, allocatable, dimension(:) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w)
    ww = zeros(n1)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius, radius], ['symm', 'symm'])
    call alloc_array(wr, [-radius, radius])
    do i = 1, n1
        wr = wp(i - radius:i + radius)
        scalar = epslevel*maxval(abs(wr))
        if (scalar /= 0) then
            ww(i) = sum(wr) + scalar
        else
            ww(i) = float_tiny*(radius + 1)*2
        end if
    end do
    ww = sqrt(ww/(2*radius + 1))
    wp = dcmplx((w/10.0d0**od)/ww)
end function balance_filt_1d_dcomplex
!
!> 2D moving balance by dividing energy
!
function balance_filt_2d_dcomplex(w, radius, eps) result(wp)
    double complex, dimension(:, :) :: w
    integer, dimension(:) :: radius
    real, optional :: eps
    double complex, allocatable, dimension(:, :) :: wp
    integer :: n1, n2, i, j, od
    double complex, allocatable, dimension(:, :) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w, 1)
    n2 = size(w, 2)
    ww = zeros(n1, n2)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius(1), radius(1), radius(2), radius(2)], &
        ['symm', 'symm', 'symm', 'symm'])
    call alloc_array(wr, [-radius(1), radius(1), -radius(2), radius(2)])
    !$omp parallel do private(i, j, wr, scalar) collapse(2)
    do j = 1, n2
        do i = 1, n1
            wr = wp(i - radius(1):i + radius(1), j - radius(2):j + radius(2))
            scalar = epslevel*maxval(abs(wr))
            if (scalar /= 0) then
                ww(i, j) = sum(wr) + scalar
            else
                ww(i, j) = float_tiny*product(radius + 1)*4
            end if
        end do
    end do
    !$omp end parallel do
    ww = sqrt(ww/(2*radius(1) + 1)/(2*radius(2) + 1))
    wp = dcmplx((w/10.0d0**od)/ww)
end function balance_filt_2d_dcomplex
!
!> 3D moving balance by dividing energy
!
function balance_filt_3d_dcomplex(w, radius, eps) result(wp)
    double complex, dimension(:, :, :) :: w
    integer, dimension(:) :: radius
    real, optional :: eps
    double complex, allocatable, dimension(:, :, :) :: wp
    integer :: n1, n2, n3, i, j, k, od
    double complex, allocatable, dimension(:, :, :) :: ww, wr
    real :: epslevel, scalar
    if (present(eps)) then
        epslevel = eps
    else
        epslevel = 0.01
    end if
    od = order_of_magnitude(maxval(abs(w)))
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ww = zeros(n1, n2, n3)
    wp = (w/10.0d0**od)**2
    if (maxval(abs(w)) == 0) then
        wp = 0.0
        return
    end if
    call pad_array(wp, [radius(1), radius(1), radius(2), radius(2), radius(3), radius(3)], &
        ['symm', 'symm', 'symm', 'symm', 'symm', 'symm'])
    call alloc_array(wr, [-radius(1), radius(1), -radius(2), radius(2), -radius(3), radius(3)])
    !$omp parallel do private(i, j, k, wr, scalar) collapse(3)
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                wr = wp(i - radius(1):i + radius(1), &
                    j - radius(2):j + radius(2), &
                    k - radius(3):k + radius(3))
                scalar = epslevel*maxval(abs(wr))
                if (scalar /= 0) then
                    ww(i, j, k) = sum(wr) + scalar
                else
                    ww(i, j, k) = float_tiny*product(radius + 1)*8
                end if
            end do
        end do
    end do
    !$omp end parallel do
    ww = sqrt(ww/(2*radius(1) + 1)/(2*radius(2) + 1)/(2*radius(3) + 1))
    wp = dcmplx((w/10.0d0**od)/ww)
end function balance_filt_3d_dcomplex
end module libflit_balancefilt
