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
!
!> The module implements LOWESS -- locally weighted scatterplot smoothing
!> LOWESS is in fact a curve-fitting method, but could be considered equivalently as
!> a smoothing/filtering technique.
!
module libflit_lowessfilt
    use libflit_array
    use libflit_error
    use libflit_array_operation
    use libflit_linear_algebra
    use libflit_utility
    use libflit_statistics
    implicit none
    interface lowess_filt
        module procedure :: lowess_1d_float
        module procedure :: lowess_2d_float
        module procedure :: lowess_3d_float
        module procedure :: lowess_1d_double
        module procedure :: lowess_2d_double
        module procedure :: lowess_3d_double
    end interface lowess_filt
    private
    public :: lowess_filt
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
recursive function lowess_1d_float(x, y, xx, order, window, kernel, robust) result(yy)
    real, dimension(:) :: x, y, xx
    integer, optional :: order
    real, optional :: window
    character(len=*), optional :: kernel
    logical, optional :: robust
    real, allocatable, dimension(:) :: yy
    real, allocatable, dimension(:, :) :: b, btwb, btw
    real, allocatable, dimension(:) :: w, beta, bb
    integer :: n, nn, i, j
    integer :: lowess_order
    real :: lowess_window
    real, allocatable, dimension(:) :: rw, y_est, residual
    logical :: lowess_robust
    character(len=64) :: lowess_kernel
    if (present(order)) then
        lowess_order = order
    else
        lowess_order = 1
    end if
    if (present(window)) then
        lowess_window = window
    else
        lowess_window = 0.1
    end if
    call assert(lowess_window > 0, ' <lowess_1d> Error: window must > 0.')
    if (present(kernel)) then
        lowess_kernel = kernel
    else
        lowess_kernel = 'tri-cube'
    end if
    if (present(robust)) then
        lowess_robust = robust
    else
        lowess_robust = .false.
    end if
    n = size(x)
    nn = size(xx)
    lowess_window = lowess_window*rov(x)
    b = zeros(n, lowess_order + 1)
    b(:, 1) = 1.0
    do i = 1, lowess_order
        b(:, i + 1) = x**i
    end do
    w = zeros(n)
    yy = zeros(nn)
    if (lowess_robust) then
        rw = zeros(n)
        y_est = lowess_1d_float(x, y, x, window=lowess_window, kernel='epanechnikov', robust=.false.)
        residual = y_est - y
        residual = residual/(6*median(abs(return_normal(residual))))
        rw = return_normal(kernel_bisquare(residual))
    end if
    do i = 1, nn
        w = abs(xx(i) - x)/lowess_window
        select case(lowess_kernel)
            case('tri-cube')
                w = kernel_tricube(w)
            case('bi-square')
                w = kernel_bisquare(w)
            case('epanechnikov')
                w = kernel_epanechnikov(w)
        end select
        if (lowess_robust) then
            w = w*rw
        end if
        btw = xdiag(transpose(b), w)
        btwb = matx(btw, b)
        beta = matx(solve(btwb, btw), y)
        bb = [1.0]
        do j = 1, lowess_order
            bb = [bb, xx(i)**j]
        end do
        yy(i) = dot_product(bb, beta)
    end do
end function lowess_1d_float
recursive function lowess_2d_float(x, y, z, xx, yy, order, window, kernel, robust) result(zz)
    real, dimension(:) :: x, y, z, xx, yy
    integer, optional :: order
    real, dimension(1:2), optional :: window
    character(len=*), optional :: kernel
    logical, optional :: robust
    real, allocatable, dimension(:) :: zz
    real, allocatable, dimension(:, :) :: b, btwb, btw
    real, allocatable, dimension(:) :: w, beta, bb
    integer :: n, nn, i, j
    integer :: lowess_order
    real, dimension(1:2) :: lowess_window
    logical :: lowess_robust
    character(len=64) :: lowess_kernel
    real, allocatable, dimension(:) :: rw, z_est, residual
    if (present(order)) then
        lowess_order = order
    else
        lowess_order = 1
    end if
    if (present(window)) then
        lowess_window = window
    else
        lowess_window = [0.1, 0.1]
    end if
    call assert(all(lowess_window > 0), ' <lowess_2d> Error: window must > 0.')
    if (present(kernel)) then
        lowess_kernel = kernel
    else
        lowess_kernel = 'tri-cube'
    end if
    if (present(robust)) then
        lowess_robust = robust
    else
        lowess_robust = .false.
    end if
    n = size(x)
    nn = size(xx)
    lowess_window = lowess_window*[rov(x), rov(y)]
    b = zeros(n, 2*lowess_order + 1)
    b(:, 1) = 1.0
    do i = 1, lowess_order
        b(:, 1 + 2*(i - 1) + 1) = x**i
        b(:, 1 + 2*i) = y**i
    end do
    w = zeros(n)
    zz = zeros(nn)
    if (lowess_robust) then
        rw = zeros(n)
        z_est = lowess_2d_float(x, y, z, x, y, lowess_order, lowess_window, kernel='epanechnikov', robust=.false.)
        residual = z_est - z
        residual = residual/(6*median(abs(return_normal(residual))))
        rw = return_normal(kernel_bisquare(residual))
    end if
    do i = 1, nn
        w = sqrt(((xx(i) - x)/lowess_window(1))**2 + ((yy(i) - y)/lowess_window(2))**2)
        select case(lowess_kernel)
            case('tri-cube')
                w = kernel_tricube(w)
            case('bi-square')
                w = kernel_bisquare(w)
            case('epanechnikov')
                w = kernel_epanechnikov(w)
        end select
        if (lowess_robust) then
            w = w*rw
        end if
        btw = xdiag(transpose(b), w)
        btwb = matx(btw, b)
        beta = matx(solve(btwb, btw), z)
        bb = [1.0]
        do j = 1, lowess_order
            bb = [bb, xx(i)**j, yy(i)**j]
        end do
        zz(i) = dot_product(bb, beta)
    end do
end function lowess_2d_float
recursive function lowess_3d_float(x, y, z, v, xx, yy, zz, order, window, kernel, robust) result(vv)
    real, dimension(:) :: x, y, z, v, xx, yy, zz
    integer, optional :: order
    real, dimension(1:3), optional :: window
    character(len=*), optional :: kernel
    logical, optional :: robust
    real, allocatable, dimension(:) :: vv
    real, allocatable, dimension(:, :) :: b, btwb, btw
    real, allocatable, dimension(:) :: w, beta, bb
    integer :: n, nn, i, j
    integer :: lowess_order
    real, dimension(1:3) :: lowess_window
    logical :: lowess_robust
    character(len=64) :: lowess_kernel
    real, allocatable, dimension(:) :: rw, v_est, residual
    if (present(order)) then
        lowess_order = order
    else
        lowess_order = 1
    end if
    if (present(window)) then
        lowess_window = window
    else
        lowess_window = [0.1, 0.1, 0.1]
    end if
    call assert(all(lowess_window > 0), ' <lowess_3d> Error: window must > 0.')
    if (present(kernel)) then
        lowess_kernel = kernel
    else
        lowess_kernel = 'tri-cube'
    end if
    if (present(robust)) then
        lowess_robust = robust
    else
        lowess_robust = .false.
    end if
    n = size(x)
    nn = size(xx)
    lowess_window = lowess_window*[rov(x), rov(y), rov(z)]
    b = zeros(n, 3*lowess_order + 1)
    b(:, 1) = 1.0
    do i = 1, lowess_order
        b(:, 1 + 3*(i - 1) + 1) = x**i
        b(:, 1 + 3*(i - 1) + 2) = y**i
        b(:, 1 + 3*i) = z**i
    end do
    w = zeros(n)
    vv = zeros(nn)
    if (lowess_robust) then
        rw = zeros(n)
        v_est = lowess_3d_float(x, y, z, v, x, y, z, lowess_order, lowess_window, kernel='epanechnikov', robust=.false.)
        residual = v_est - v
        residual = residual/(6*median(abs(return_normal(residual))))
        rw = return_normal(kernel_bisquare(residual))
    end if
    do i = 1, nn
        w = sqrt(((xx(i) - x)/lowess_window(1))**2 + ((yy(i) - y)/lowess_window(2))**2 + ((zz(i) - z)/lowess_window(3))**2)
        select case(lowess_kernel)
            case('tri-cube')
                w = kernel_tricube(w)
            case('bi-square')
                w = kernel_bisquare(w)
            case('epanechnikov')
                w = kernel_epanechnikov(w)
        end select
        if (lowess_robust) then
            w = w*rw
        end if
        btw = xdiag(transpose(b), w)
        btwb = matx(btw, b)
        beta = matx(solve(btwb, btw), v)
        bb = [1.0]
        do j = 1, lowess_order
            bb = [bb, xx(i)**j, yy(i)**j, zz(i)**j]
        end do
        vv(i) = dot_product(bb, beta)
    end do
end function lowess_3d_float
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
recursive function lowess_1d_double(x, y, xx, order, window, kernel, robust) result(yy)
    double precision, dimension(:) :: x, y, xx
    integer, optional :: order
    double precision, optional :: window
    character(len=*), optional :: kernel
    logical, optional :: robust
    double precision, allocatable, dimension(:) :: yy
    double precision, allocatable, dimension(:, :) :: b, btwb, btw
    double precision, allocatable, dimension(:) :: w, beta, bb
    integer :: n, nn, i, j
    integer :: lowess_order
    double precision :: lowess_window
    double precision, allocatable, dimension(:) :: rw, y_est, residual
    logical :: lowess_robust
    character(len=64) :: lowess_kernel
    if (present(order)) then
        lowess_order = order
    else
        lowess_order = 1
    end if
    if (present(window)) then
        lowess_window = window
    else
        lowess_window = 0.1
    end if
    call assert(lowess_window > 0, ' <lowess_1d> Error: window must > 0.')
    if (present(kernel)) then
        lowess_kernel = kernel
    else
        lowess_kernel = 'tri-cube'
    end if
    if (present(robust)) then
        lowess_robust = robust
    else
        lowess_robust = .false.
    end if
    n = size(x)
    nn = size(xx)
    lowess_window = lowess_window*rov(x)
    b = zeros(n, lowess_order + 1)
    b(:, 1) = 1.0
    do i = 1, lowess_order
        b(:, i + 1) = x**i
    end do
    w = zeros(n)
    yy = zeros(nn)
    if (lowess_robust) then
        rw = zeros(n)
        y_est = lowess_1d_double(x, y, x, window=lowess_window, kernel='epanechnikov', robust=.false.)
        residual = y_est - y
        residual = residual/(6*median(abs(return_normal(residual))))
        rw = return_normal(kernel_bisquare(residual))
    end if
    do i = 1, nn
        w = abs(xx(i) - x)/lowess_window
        select case(lowess_kernel)
            case('tri-cube')
                w = kernel_tricube(w)
            case('bi-square')
                w = kernel_bisquare(w)
            case('epanechnikov')
                w = kernel_epanechnikov(w)
        end select
        if (lowess_robust) then
            w = w*rw
        end if
        btw = xdiag(transpose(b), w)
        btwb = matx(btw, b)
        beta = matx(solve(btwb, btw), y)
        bb = [1.0]
        do j = 1, lowess_order
            bb = [bb, xx(i)**j]
        end do
        yy(i) = dot_product(bb, beta)
    end do
end function lowess_1d_double
recursive function lowess_2d_double(x, y, z, xx, yy, order, window, kernel, robust) result(zz)
    double precision, dimension(:) :: x, y, z, xx, yy
    integer, optional :: order
    double precision, dimension(1:2), optional :: window
    character(len=*), optional :: kernel
    logical, optional :: robust
    double precision, allocatable, dimension(:) :: zz
    double precision, allocatable, dimension(:, :) :: b, btwb, btw
    double precision, allocatable, dimension(:) :: w, beta, bb
    integer :: n, nn, i, j
    integer :: lowess_order
    double precision, dimension(1:2) :: lowess_window
    logical :: lowess_robust
    character(len=64) :: lowess_kernel
    double precision, allocatable, dimension(:) :: rw, z_est, residual
    if (present(order)) then
        lowess_order = order
    else
        lowess_order = 1
    end if
    if (present(window)) then
        lowess_window = window
    else
        lowess_window = [0.1, 0.1]
    end if
    call assert(all(lowess_window > 0), ' <lowess_2d> Error: window must > 0.')
    if (present(kernel)) then
        lowess_kernel = kernel
    else
        lowess_kernel = 'tri-cube'
    end if
    if (present(robust)) then
        lowess_robust = robust
    else
        lowess_robust = .false.
    end if
    n = size(x)
    nn = size(xx)
    lowess_window = lowess_window*[rov(x), rov(y)]
    b = zeros(n, 2*lowess_order + 1)
    b(:, 1) = 1.0
    do i = 1, lowess_order
        b(:, 1 + 2*(i - 1) + 1) = x**i
        b(:, 1 + 2*i) = y**i
    end do
    w = zeros(n)
    zz = zeros(nn)
    if (lowess_robust) then
        rw = zeros(n)
        z_est = lowess_2d_double(x, y, z, x, y, lowess_order, lowess_window, kernel='epanechnikov', robust=.false.)
        residual = z_est - z
        residual = residual/(6*median(abs(return_normal(residual))))
        rw = return_normal(kernel_bisquare(residual))
    end if
    do i = 1, nn
        w = sqrt(((xx(i) - x)/lowess_window(1))**2 + ((yy(i) - y)/lowess_window(2))**2)
        select case(lowess_kernel)
            case('tri-cube')
                w = kernel_tricube(w)
            case('bi-square')
                w = kernel_bisquare(w)
            case('epanechnikov')
                w = kernel_epanechnikov(w)
        end select
        if (lowess_robust) then
            w = w*rw
        end if
        btw = xdiag(transpose(b), w)
        btwb = matx(btw, b)
        beta = matx(solve(btwb, btw), z)
        bb = [1.0]
        do j = 1, lowess_order
            bb = [bb, xx(i)**j, yy(i)**j]
        end do
        zz(i) = dot_product(bb, beta)
    end do
end function lowess_2d_double
recursive function lowess_3d_double(x, y, z, v, xx, yy, zz, order, window, kernel, robust) result(vv)
    double precision, dimension(:) :: x, y, z, v, xx, yy, zz
    integer, optional :: order
    double precision, dimension(1:3), optional :: window
    character(len=*), optional :: kernel
    logical, optional :: robust
    double precision, allocatable, dimension(:) :: vv
    double precision, allocatable, dimension(:, :) :: b, btwb, btw
    double precision, allocatable, dimension(:) :: w, beta, bb
    integer :: n, nn, i, j
    integer :: lowess_order
    double precision, dimension(1:3) :: lowess_window
    logical :: lowess_robust
    character(len=64) :: lowess_kernel
    double precision, allocatable, dimension(:) :: rw, v_est, residual
    if (present(order)) then
        lowess_order = order
    else
        lowess_order = 1
    end if
    if (present(window)) then
        lowess_window = window
    else
        lowess_window = [0.1, 0.1, 0.1]
    end if
    call assert(all(lowess_window > 0), ' <lowess_3d> Error: window must > 0.')
    if (present(kernel)) then
        lowess_kernel = kernel
    else
        lowess_kernel = 'tri-cube'
    end if
    if (present(robust)) then
        lowess_robust = robust
    else
        lowess_robust = .false.
    end if
    n = size(x)
    nn = size(xx)
    lowess_window = lowess_window*[rov(x), rov(y), rov(z)]
    b = zeros(n, 3*lowess_order + 1)
    b(:, 1) = 1.0
    do i = 1, lowess_order
        b(:, 1 + 3*(i - 1) + 1) = x**i
        b(:, 1 + 3*(i - 1) + 2) = y**i
        b(:, 1 + 3*i) = z**i
    end do
    w = zeros(n)
    vv = zeros(nn)
    if (lowess_robust) then
        rw = zeros(n)
        v_est = lowess_3d_double(x, y, z, v, x, y, z, lowess_order, lowess_window, kernel='epanechnikov', robust=.false.)
        residual = v_est - v
        residual = residual/(6*median(abs(return_normal(residual))))
        rw = return_normal(kernel_bisquare(residual))
    end if
    do i = 1, nn
        w = sqrt(((xx(i) - x)/lowess_window(1))**2 + ((yy(i) - y)/lowess_window(2))**2 + ((zz(i) - z)/lowess_window(3))**2)
        select case(lowess_kernel)
            case('tri-cube')
                w = kernel_tricube(w)
            case('bi-square')
                w = kernel_bisquare(w)
            case('epanechnikov')
                w = kernel_epanechnikov(w)
        end select
        if (lowess_robust) then
            w = w*rw
        end if
        btw = xdiag(transpose(b), w)
        btwb = matx(btw, b)
        beta = matx(solve(btwb, btw), v)
        bb = [1.0]
        do j = 1, lowess_order
            bb = [bb, xx(i)**j, yy(i)**j, zz(i)**j]
        end do
        vv(i) = dot_product(bb, beta)
    end do
end function lowess_3d_double
end module libflit_lowessfilt
