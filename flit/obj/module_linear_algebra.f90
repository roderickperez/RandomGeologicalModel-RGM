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
module libflit_linear_algebra
    use libflit_constants
    use libflit_sort
    use libflit_array
    use libflit_error
    use libflit_sort
    use libflit_random
    use libflit_utility
    use blas95
    use lapack95
    use libflit_array_operation
    implicit none
    interface trace
        module procedure :: trace_float
        module procedure :: trace_double
        module procedure :: trace_complex
        module procedure :: trace_dcomplex
    end interface trace
    interface det
        module procedure :: det_nxn_float
        module procedure :: det_nxn_double
        module procedure :: det_nxn_complex
        module procedure :: det_nxn_dcomplex
    end interface det
    interface inv
        module procedure :: inv_nxn_float
        module procedure :: inv_nxn_double
        module procedure :: inv_nxn_complex
        module procedure :: inv_nxn_dcomplex
    end interface
    interface solve
        module procedure :: solve_single_rhs_float
        module procedure :: solve_multiple_rhs_float
        module procedure :: solve_single_rhs_double
        module procedure :: solve_multiple_rhs_double
        module procedure :: solve_single_rhs_complex
        module procedure :: solve_multiple_rhs_complex
        module procedure :: solve_single_rhs_dcomplex
        module procedure :: solve_multiple_rhs_dcomplex
    end interface solve
    interface solve_band
        module procedure :: solve_band_single_rhs_float
        module procedure :: solve_band_single_rhs_double
        module procedure :: solve_band_single_rhs_complex
        module procedure :: solve_band_single_rhs_dcomplex
    end interface solve_band
    interface lsqsolve
        module procedure :: least_squares_solve_single_rhs_float
        module procedure :: least_squares_solve_multiple_rhs_float
        module procedure :: least_squares_solve_single_rhs_double
        module procedure :: least_squares_solve_multiple_rhs_double
        module procedure :: least_squares_solve_single_rhs_complex
        module procedure :: least_squares_solve_multiple_rhs_complex
        module procedure :: least_squares_solve_single_rhs_dcomplex
        module procedure :: least_squares_solve_multiple_rhs_dcomplex
    end interface lsqsolve
    interface svd
        module procedure :: svd_float
        module procedure :: svd_double
        module procedure :: svd_complex
        module procedure :: svd_dcomplex
    end interface svd
    interface eigen
        module procedure :: eigen_simple_symmetric_l_or_r_float
        module procedure :: eigen_simple_symmetric_l_or_r_double
        module procedure :: eigen_simple_l_or_r_float
        module procedure :: eigen_simple_l_or_r_double
        module procedure :: eigen_simple_l_or_r_complex
        module procedure :: eigen_simple_l_or_r_dcomplex
        module procedure :: eigen_general_l_or_r_float
        module procedure :: eigen_general_l_or_r_double
        module procedure :: eigen_general_l_or_r_complex
        module procedure :: eigen_general_l_or_r_dcomplex
        module procedure :: eigen_simple_l_and_r_float
        module procedure :: eigen_simple_l_and_r_double
        module procedure :: eigen_simple_l_and_r_complex
        module procedure :: eigen_simple_l_and_r_dcomplex
        module procedure :: eigen_general_l_and_r_float
        module procedure :: eigen_general_l_and_r_double
        module procedure :: eigen_general_l_and_r_complex
        module procedure :: eigen_general_l_and_r_dcomplex
    end interface eigen
    interface matx
        module procedure :: matx_mat_vec_float
        module procedure :: matx_mat_vec_double
        module procedure :: matx_mat_vec_complex
        module procedure :: matx_mat_vec_dcomplex
        module procedure :: matx_mat_mat_float
        module procedure :: matx_mat_mat_double
        module procedure :: matx_mat_mat_complex
        module procedure :: matx_mat_mat_dcomplex
    end interface matx
    interface diagx
        module procedure :: diagx_mat_mat_float
        module procedure :: diagx_mat_mat_double
        module procedure :: diagx_mat_mat_complex
        module procedure :: diagx_mat_mat_dcomplex
    end interface diagx
    interface xdiag
        module procedure :: xdiag_mat_mat_float
        module procedure :: xdiag_mat_mat_double
        module procedure :: xdiag_mat_mat_complex
        module procedure :: xdiag_mat_mat_dcomplex
    end interface xdiag
    interface toeplitz_matrix
        module procedure :: toeplitz_float
        module procedure :: toeplitz_double
        module procedure :: toeplitz_complex
        module procedure :: toeplitz_dcomplex
        module procedure :: circulant_float
        module procedure :: circulant_complex
        module procedure :: circulant_double
        module procedure :: circulant_dcomplex
    end interface toeplitz_matrix
    interface hankel_matrix
        module procedure :: hankel_float
        module procedure :: hankel_double
        module procedure :: hankel_complex
        module procedure :: hankel_dcomplex
    end interface hankel_matrix
    interface vandermonde_matrix
        module procedure :: vandermonde_float
        module procedure :: vandermonde_double
        module procedure :: vandermonde_complex
        module procedure :: vandermonde_dcomplex
    end interface vandermonde_matrix
    interface spectral_radius
        module procedure :: spectral_radius_float
        module procedure :: spectral_radius_double
    end interface spectral_radius
    interface eigen_symm2x2
        module procedure :: eigen_symm2x2_float
        module procedure :: eigen_symm2x2_double
    end interface eigen_symm2x2
    interface eigen_symm3x3
        module procedure :: eigen_symm3x3_float
        module procedure :: eigen_symm3x3_double
    end interface eigen_symm3x3
    private
    public :: trace
    public :: det
    public :: inv
    public :: solve
    public :: lsqsolve
    public :: svd
    public :: eigen
    public :: matx
    public :: diagx
    public :: xdiag
    public :: solve_band
    public :: spectral_radius
    public :: eigen_symm2x2
    public :: eigen_symm3x3
    public :: toeplitz_matrix
    public :: hankel_matrix
    public :: vandermonde_matrix
contains
    ! Some linear algebra functions
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
!!!#define matx_mat_vec_float matx_mat_vec_float_float_float
function trace_float(m) result(tr)
    real, dimension(:, :), intent(in) :: m
    real :: tr
    integer :: i
    tr = 0.0
    do i = 1, min(size(m, 1), size(m, 2))
        tr = tr + m(i, i)
    end do
end function trace_float
function circulant_float(c, nrow) result(t)
    real, dimension(:) :: c
    integer, intent(in), optional :: nrow
    real, allocatable, dimension(:, :) :: t
    integer :: n, i
    integer :: n2
    n = size(c)
    if (present(nrow)) then
        n2 = nrow
    else
        n2 = n
    end if
    call alloc_array(t, [1, n, 1, n2])
    t(:, 1) = c(:)
    do i = 2, n2
        t(:, i) = cshift(t(:, i - 1), -1)
    end do
end function circulant_float
function toeplitz_float(c, r) result(t)
    real, dimension(:) :: c, r
    real, allocatable, dimension(:, :) :: t
    integer :: n1, n2, i, j
    call assert(c(1) == r(1), ' Error: c(1) /= r(1) ')
    n1 = size(c)
    n2 = size(r)
    call alloc_array(t, [1, n1, 1, n2])
    do i = 1, n1
        t(i, i:n2) = r(1:n2 - i + 1)
    end do
    do j = 1, n2
        t(j:n1, j) = c(1:n1 - j + 1)
    end do
end function toeplitz_float
function hankel_float(x) result(h)
    real, dimension(:), intent(in) :: x
    real, allocatable, dimension(:, :) :: h
    integer :: nx, m, n, i
    nx = size(x)
    if (mod(nx, 2) == 0) then
        n = nx/2
        m = nx/2 + 1
    else
        n = (nx + 1)/2
        m = n
    end if
    call alloc_array(h, [1, n, 1, n])
    do i = 1, n
        h(:, i) = x(i:i + m - 1)
    end do
end function hankel_float
function vandermonde_float(x, n) result(h)
    real, dimension(:) :: x
    integer :: n
    real, allocatable, dimension(:, :) :: h
    integer :: i
    call alloc_array(h, [1, size(x), 1, n])
    do i = 1, n
        h(:, i) = x**(i - 1.0)
    end do
end function vandermonde_float
function det_nxn_float(w) result(wdet)
    real, dimension(:, :), intent(in) :: w
    real :: wdet, sgn
    integer :: n1, n2, i
    integer, allocatable, dimension(:) :: ipiv
    real, allocatable, dimension(:, :) :: wt
    n1 = size(w, 1)
    n2 = size(w, 2)
    if (n1 == 2 .and. n2 == 2) then
        ! 2x2 matrix
        wdet = w(1, 1)*w(2, 2) - w(1, 2)*w(2, 1)
    else if (n1 == 3 .and. n2 == 3) then
        ! 3x3 matrix
        wdet = &
            -w(1, 3)*w(2, 2)*w(3, 1) &
            + w(1, 2)*w(2, 3)*w(3, 1) &
            + w(1, 3)*w(2, 1)*w(3, 2) &
            - w(1, 1)*w(2, 3)*w(3, 2) &
            - w(1, 2)*w(2, 1)*w(3, 3) &
            + w(1, 1)*w(2, 2)*w(3, 3)
    else
        ! nxn matrix
        allocate (wt(1:n1, 1:n2), source=w)
        allocate (ipiv(1:n1))
        ipiv = 0
        call getrf(wt, ipiv)
        wdet = 1.0
        do i = 1, n1
            wdet = wdet*wt(i, i)
        end do
        sgn = 1.0
        do i = 1, n1
            if (ipiv(i) /= i) then
                sgn = -sgn
            end if
        end do
        wdet = sgn*wdet
    end if
end function det_nxn_float
function inv_nxn_float(w) result(winv)
    real, dimension(:, :), intent(in) :: w
    integer :: n1, n2
    real, allocatable, dimension(:, :) :: winv
    integer, allocatable, dimension(:) :: ipiv
    real :: wdet
    n1 = size(w, 1)
    n2 = size(w, 2)
    if (n1 == 2 .and. n2 == 2) then
        ! 2x2 matrix
        allocate (winv(1:n1, 1:n2))
        wdet = det_nxn_float(w)
        winv(1, 1) = w(2, 2)
        winv(1, 2) = -w(1, 2)
        winv(2, 1) = -w(2, 1)
        winv(2, 2) = w(1, 1)
        winv = winv/wdet
    else if (n1 == 3 .and. n2 == 3) then
        ! 3x3 matrix
        allocate (winv(1:n1, 1:n2))
        wdet = det_nxn_float(w)
        winv(1, 1) = (-w(2, 3)*w(3, 2) + w(2, 2)*w(3, 3))/wdet
        winv(1, 2) = (w(1, 3)*w(3, 2) - w(1, 2)*w(3, 3))/wdet
        winv(1, 3) = (-w(1, 3)*w(2, 2) + w(1, 2)*w(2, 3))/wdet
        winv(2, 1) = (w(2, 3)*w(3, 1) - w(2, 1)*w(3, 3))/wdet
        winv(2, 2) = (-w(1, 3)*w(3, 1) + w(1, 1)*w(3, 3))/wdet
        winv(2, 3) = (w(1, 3)*w(2, 1) - w(1, 1)*w(2, 3))/wdet
        winv(3, 1) = (-w(2, 2)*w(3, 1) + w(2, 1)*w(3, 2))/wdet
        winv(3, 2) = (w(1, 2)*w(3, 1) - w(1, 1)*w(3, 2))/wdet
        winv(3, 3) = (-w(1, 2)*w(2, 1) + w(1, 1)*w(2, 2))/wdet
    else
        ! nxn matrix
        allocate (winv(1:n1, 1:n2), source=w)
        allocate (ipiv(1:n1))
        ipiv = 0
        call getrf(winv, ipiv)
        call getri(winv, ipiv)
    end if
end function inv_nxn_float
subroutine svd_float(a, s, u, vt)
    real, dimension(:, :), intent(in) :: a
    real, allocatable, dimension(:), intent(out) :: s
    real, allocatable, dimension(:, :), intent(out) :: u, vt
    integer :: m, n
    real, allocatable, dimension(:, :) :: w
    m = size(a, 1)
    n = size(a, 2)
    call alloc_array(w, [1, m, 1, n], source=a)
    call alloc_array(s, [1, min(m, n)])
    call alloc_array(u, [1, m, 1, m])
    call alloc_array(vt, [1, n, 1, n])
    call gesvd(w, s, u, vt)
end subroutine svd_float
function solve_single_rhs_float(a, b) result(x)
    real, dimension(:, :) :: a
    real, dimension(:) :: b
    real, allocatable, dimension(:) :: x
    real, allocatable, dimension(:, :) :: ac
    ac = a
    x = b
    call gesv(ac, x)
end function solve_single_rhs_float
function solve_multiple_rhs_float(a, b) result(x)
    real, dimension(:, :) :: a
    real, dimension(:, :) :: b
    real, allocatable, dimension(:, :) :: x
    real, allocatable, dimension(:, :) :: ac
    ac = a
    x = b
    call gesv(ac, x)
end function solve_multiple_rhs_float
function least_squares_solve_single_rhs_float(a, b) result(x)
    real, dimension(:, :) :: a
    real, dimension(:) :: b
    real, allocatable, dimension(:) :: x, xt
    integer :: m, n
    real, allocatable, dimension(:, :) :: ac
    m = size(a, 1)
    n = size(a, 2)
    ac = a
    if (size(b) /= m) then
        print *, ' Error: size(a) and size(b) mismatch. '
        stop
    end if
    xt = zeros(max(m, n))
    xt(1:m) = b
    call gels(ac, xt)
    x = xt(1:n)
end function least_squares_solve_single_rhs_float
function least_squares_solve_multiple_rhs_float(a, b) result(x)
    real, dimension(:, :) :: a
    real, dimension(:, :) :: b
    real, allocatable, dimension(:, :) :: x, xt
    integer :: m, n, nrhs
    real, allocatable, dimension(:, :) :: ac
    m = size(a, 1)
    n = size(a, 2)
    nrhs = size(b, 2)
    ! allocate (ac(1:m, 1:n), source=a)
    ac = a
    if (size(b, 1) /= m) then
        print *, ' Error: size(a) and size(b) mismatch. '
        stop
    end if
    xt = zeros(max(m, n), nrhs)
    xt(1:m, 1:nrhs) = b
    call gels(ac, xt)
    x = xt(1:n, 1:nrhs)
end function least_squares_solve_multiple_rhs_float
function solve_band_single_rhs_float(a, nu, nl, b) result(x)
    real, dimension(:, :), intent(in) :: a
    integer, intent(in) :: nu, nl
    real, dimension(:), intent(in) :: b
    real, allocatable, dimension(:) :: x
    real, allocatable, dimension(:, :) :: ac
    integer :: m, n, i
    ! band storage: https://www.netlib.org/lapack/lug/node124.html
    ! https://www.intel.com/content/www/us/en/develop/documentation/onemkl-developer-reference-fortran/top/lapack-routines/lapack-linear-equation-routines/lapack-linear-equation-driver-routines/gbsv.html
    ! http://www.netlib.org/lapack/explore-html/dc/db2/group__real_g_bsolve_ga3656935309a19ed624052103572a4a47.html#ga3656935309a19ed624052103572a4a47
    m = size(a, 1)
    call assert(m == nu + nl + 1, &
        '<solve_band_single_rhs> Error: Dimension of a is inconsistent.')
    n = size(a, 2)
    do i = 1, nu
        call assert(all(a(nu + 1 - i, 1:i) == 0), &
            '<solve_band_single_rhs> Error: Upper half is not consistent.')
    end do
    do i = 1, nl
        call assert(all(a(nu + 1 + i, n - i + 1:n) == 0), &
            '<solve_band_single_rhs> Error: Lower half is not consistent.')
    end do
    ! dim1 of input ac = m + nl, where the first nl rows are zeros, and nl + 1: rows are a
    ac = zeros(m + nl, n)
    ac(nl + 1:m + nl, :) = a
    x = b
    call gbsv(ac, x, nl)
end function solve_band_single_rhs_float
function matx_mat_vec_float(a, x) result(y)
    real, dimension(:, :) :: a
    real, dimension(:) :: x
    real, allocatable, dimension(:) :: y
    integer :: n
    n = size(a, 1)
    call alloc_array(y, [1, n])
    call gemv(a, x, y)
end function matx_mat_vec_float
function matx_mat_mat_float(a, x) result(y)
    real, dimension(:, :) :: a
    real, dimension(:, :) :: x
    real, allocatable, dimension(:, :) :: y
    integer :: n1, n2
    n1 = size(a, 1)
    n2 = size(x, 2)
    call alloc_array(y, [1, n1, 1, n2])
    call gemm(a, x, y)
end function matx_mat_mat_float
function diagx_mat_mat_float(a, x) result(y)
    real, dimension(:) :: a
    real, dimension(:, :) :: x
    real, allocatable, dimension(:, :) :: y
    integer :: n1, n2, i
    n1 = size(a)
    n2 = size(x, 2)
    call alloc_array(y, [1, n1, 1, n2])
    do i = 1, n1
        y(i, :) = x(i, :)*a(i)
    end do
end function diagx_mat_mat_float
function xdiag_mat_mat_float(x, a) result(y)
    real, dimension(:, :) :: x
    real, dimension(:) :: a
    real, allocatable, dimension(:, :) :: y
    integer :: n1, n2, i
    n1 = size(x, 1)
    n2 = size(a)
    call alloc_array(y, [1, n1, 1, n2])
    do i = 1, n2
        y(:, i) = x(:, i)*a(i)
    end do
end function xdiag_mat_mat_float
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
!!!#define matx_mat_vec_double matx_mat_vec_double_double_double
function trace_double(m) result(tr)
    double precision, dimension(:, :), intent(in) :: m
    double precision :: tr
    integer :: i
    tr = 0.0
    do i = 1, min(size(m, 1), size(m, 2))
        tr = tr + m(i, i)
    end do
end function trace_double
function circulant_double(c, nrow) result(t)
    double precision, dimension(:) :: c
    integer, intent(in), optional :: nrow
    double precision, allocatable, dimension(:, :) :: t
    integer :: n, i
    integer :: n2
    n = size(c)
    if (present(nrow)) then
        n2 = nrow
    else
        n2 = n
    end if
    call alloc_array(t, [1, n, 1, n2])
    t(:, 1) = c(:)
    do i = 2, n2
        t(:, i) = cshift(t(:, i - 1), -1)
    end do
end function circulant_double
function toeplitz_double(c, r) result(t)
    double precision, dimension(:) :: c, r
    double precision, allocatable, dimension(:, :) :: t
    integer :: n1, n2, i, j
    call assert(c(1) == r(1), ' Error: c(1) /= r(1) ')
    n1 = size(c)
    n2 = size(r)
    call alloc_array(t, [1, n1, 1, n2])
    do i = 1, n1
        t(i, i:n2) = r(1:n2 - i + 1)
    end do
    do j = 1, n2
        t(j:n1, j) = c(1:n1 - j + 1)
    end do
end function toeplitz_double
function hankel_double(x) result(h)
    double precision, dimension(:), intent(in) :: x
    double precision, allocatable, dimension(:, :) :: h
    integer :: nx, m, n, i
    nx = size(x)
    if (mod(nx, 2) == 0) then
        n = nx/2
        m = nx/2 + 1
    else
        n = (nx + 1)/2
        m = n
    end if
    call alloc_array(h, [1, n, 1, n])
    do i = 1, n
        h(:, i) = x(i:i + m - 1)
    end do
end function hankel_double
function vandermonde_double(x, n) result(h)
    double precision, dimension(:) :: x
    integer :: n
    double precision, allocatable, dimension(:, :) :: h
    integer :: i
    call alloc_array(h, [1, size(x), 1, n])
    do i = 1, n
        h(:, i) = x**(i - 1.0)
    end do
end function vandermonde_double
function det_nxn_double(w) result(wdet)
    double precision, dimension(:, :), intent(in) :: w
    real :: wdet, sgn
    integer :: n1, n2, i
    integer, allocatable, dimension(:) :: ipiv
    double precision, allocatable, dimension(:, :) :: wt
    n1 = size(w, 1)
    n2 = size(w, 2)
    if (n1 == 2 .and. n2 == 2) then
        ! 2x2 matrix
        wdet = w(1, 1)*w(2, 2) - w(1, 2)*w(2, 1)
    else if (n1 == 3 .and. n2 == 3) then
        ! 3x3 matrix
        wdet = &
            -w(1, 3)*w(2, 2)*w(3, 1) &
            + w(1, 2)*w(2, 3)*w(3, 1) &
            + w(1, 3)*w(2, 1)*w(3, 2) &
            - w(1, 1)*w(2, 3)*w(3, 2) &
            - w(1, 2)*w(2, 1)*w(3, 3) &
            + w(1, 1)*w(2, 2)*w(3, 3)
    else
        ! nxn matrix
        allocate (wt(1:n1, 1:n2), source=w)
        allocate (ipiv(1:n1))
        ipiv = 0
        call getrf(wt, ipiv)
        wdet = 1.0
        do i = 1, n1
            wdet = wdet*wt(i, i)
        end do
        sgn = 1.0
        do i = 1, n1
            if (ipiv(i) /= i) then
                sgn = -sgn
            end if
        end do
        wdet = sgn*wdet
    end if
end function det_nxn_double
function inv_nxn_double(w) result(winv)
    double precision, dimension(:, :), intent(in) :: w
    integer :: n1, n2
    double precision, allocatable, dimension(:, :) :: winv
    integer, allocatable, dimension(:) :: ipiv
    double precision :: wdet
    n1 = size(w, 1)
    n2 = size(w, 2)
    if (n1 == 2 .and. n2 == 2) then
        ! 2x2 matrix
        allocate (winv(1:n1, 1:n2))
        wdet = det_nxn_double(w)
        winv(1, 1) = w(2, 2)
        winv(1, 2) = -w(1, 2)
        winv(2, 1) = -w(2, 1)
        winv(2, 2) = w(1, 1)
        winv = winv/wdet
    else if (n1 == 3 .and. n2 == 3) then
        ! 3x3 matrix
        allocate (winv(1:n1, 1:n2))
        wdet = det_nxn_double(w)
        winv(1, 1) = (-w(2, 3)*w(3, 2) + w(2, 2)*w(3, 3))/wdet
        winv(1, 2) = (w(1, 3)*w(3, 2) - w(1, 2)*w(3, 3))/wdet
        winv(1, 3) = (-w(1, 3)*w(2, 2) + w(1, 2)*w(2, 3))/wdet
        winv(2, 1) = (w(2, 3)*w(3, 1) - w(2, 1)*w(3, 3))/wdet
        winv(2, 2) = (-w(1, 3)*w(3, 1) + w(1, 1)*w(3, 3))/wdet
        winv(2, 3) = (w(1, 3)*w(2, 1) - w(1, 1)*w(2, 3))/wdet
        winv(3, 1) = (-w(2, 2)*w(3, 1) + w(2, 1)*w(3, 2))/wdet
        winv(3, 2) = (w(1, 2)*w(3, 1) - w(1, 1)*w(3, 2))/wdet
        winv(3, 3) = (-w(1, 2)*w(2, 1) + w(1, 1)*w(2, 2))/wdet
    else
        ! nxn matrix
        allocate (winv(1:n1, 1:n2), source=w)
        allocate (ipiv(1:n1))
        ipiv = 0
        call getrf(winv, ipiv)
        call getri(winv, ipiv)
    end if
end function inv_nxn_double
subroutine svd_double(a, s, u, vt)
    double precision, dimension(:, :), intent(in) :: a
    double precision, allocatable, dimension(:), intent(out) :: s
    double precision, allocatable, dimension(:, :), intent(out) :: u, vt
    integer :: m, n
    double precision, allocatable, dimension(:, :) :: w
    m = size(a, 1)
    n = size(a, 2)
    call alloc_array(w, [1, m, 1, n], source=a)
    call alloc_array(s, [1, min(m, n)])
    call alloc_array(u, [1, m, 1, m])
    call alloc_array(vt, [1, n, 1, n])
    call gesvd(w, s, u, vt)
end subroutine svd_double
function solve_single_rhs_double(a, b) result(x)
    double precision, dimension(:, :) :: a
    double precision, dimension(:) :: b
    double precision, allocatable, dimension(:) :: x
    double precision, allocatable, dimension(:, :) :: ac
    ac = a
    x = b
    call gesv(ac, x)
end function solve_single_rhs_double
function solve_multiple_rhs_double(a, b) result(x)
    double precision, dimension(:, :) :: a
    double precision, dimension(:, :) :: b
    double precision, allocatable, dimension(:, :) :: x
    double precision, allocatable, dimension(:, :) :: ac
    ac = a
    x = b
    call gesv(ac, x)
end function solve_multiple_rhs_double
function least_squares_solve_single_rhs_double(a, b) result(x)
    double precision, dimension(:, :) :: a
    double precision, dimension(:) :: b
    double precision, allocatable, dimension(:) :: x, xt
    integer :: m, n
    double precision, allocatable, dimension(:, :) :: ac
    m = size(a, 1)
    n = size(a, 2)
    ac = a
    if (size(b) /= m) then
        print *, ' Error: size(a) and size(b) mismatch. '
        stop
    end if
    xt = zeros(max(m, n))
    xt(1:m) = b
    call gels(ac, xt)
    x = xt(1:n)
end function least_squares_solve_single_rhs_double
function least_squares_solve_multiple_rhs_double(a, b) result(x)
    double precision, dimension(:, :) :: a
    double precision, dimension(:, :) :: b
    double precision, allocatable, dimension(:, :) :: x, xt
    integer :: m, n, nrhs
    double precision, allocatable, dimension(:, :) :: ac
    m = size(a, 1)
    n = size(a, 2)
    nrhs = size(b, 2)
    ! allocate (ac(1:m, 1:n), source=a)
    ac = a
    if (size(b, 1) /= m) then
        print *, ' Error: size(a) and size(b) mismatch. '
        stop
    end if
    xt = zeros(max(m, n), nrhs)
    xt(1:m, 1:nrhs) = b
    call gels(ac, xt)
    x = xt(1:n, 1:nrhs)
end function least_squares_solve_multiple_rhs_double
function solve_band_single_rhs_double(a, nu, nl, b) result(x)
    double precision, dimension(:, :), intent(in) :: a
    integer, intent(in) :: nu, nl
    double precision, dimension(:), intent(in) :: b
    double precision, allocatable, dimension(:) :: x
    double precision, allocatable, dimension(:, :) :: ac
    integer :: m, n, i
    ! band storage: https://www.netlib.org/lapack/lug/node124.html
    ! https://www.intel.com/content/www/us/en/develop/documentation/onemkl-developer-reference-fortran/top/lapack-routines/lapack-linear-equation-routines/lapack-linear-equation-driver-routines/gbsv.html
    ! http://www.netlib.org/lapack/explore-html/dc/db2/group__real_g_bsolve_ga3656935309a19ed624052103572a4a47.html#ga3656935309a19ed624052103572a4a47
    m = size(a, 1)
    call assert(m == nu + nl + 1, &
        '<solve_band_single_rhs> Error: Dimension of a is inconsistent.')
    n = size(a, 2)
    do i = 1, nu
        call assert(all(a(nu + 1 - i, 1:i) == 0), &
            '<solve_band_single_rhs> Error: Upper half is not consistent.')
    end do
    do i = 1, nl
        call assert(all(a(nu + 1 + i, n - i + 1:n) == 0), &
            '<solve_band_single_rhs> Error: Lower half is not consistent.')
    end do
    ! dim1 of input ac = m + nl, where the first nl rows are zeros, and nl + 1: rows are a
    ac = zeros(m + nl, n)
    ac(nl + 1:m + nl, :) = a
    x = b
    call gbsv(ac, x, nl)
end function solve_band_single_rhs_double
function matx_mat_vec_double(a, x) result(y)
    double precision, dimension(:, :) :: a
    double precision, dimension(:) :: x
    double precision, allocatable, dimension(:) :: y
    integer :: n
    n = size(a, 1)
    call alloc_array(y, [1, n])
    call gemv(a, x, y)
end function matx_mat_vec_double
function matx_mat_mat_double(a, x) result(y)
    double precision, dimension(:, :) :: a
    double precision, dimension(:, :) :: x
    double precision, allocatable, dimension(:, :) :: y
    integer :: n1, n2
    n1 = size(a, 1)
    n2 = size(x, 2)
    call alloc_array(y, [1, n1, 1, n2])
    call gemm(a, x, y)
end function matx_mat_mat_double
function diagx_mat_mat_double(a, x) result(y)
    double precision, dimension(:) :: a
    double precision, dimension(:, :) :: x
    double precision, allocatable, dimension(:, :) :: y
    integer :: n1, n2, i
    n1 = size(a)
    n2 = size(x, 2)
    call alloc_array(y, [1, n1, 1, n2])
    do i = 1, n1
        y(i, :) = x(i, :)*a(i)
    end do
end function diagx_mat_mat_double
function xdiag_mat_mat_double(x, a) result(y)
    double precision, dimension(:, :) :: x
    double precision, dimension(:) :: a
    double precision, allocatable, dimension(:, :) :: y
    integer :: n1, n2, i
    n1 = size(x, 1)
    n2 = size(a)
    call alloc_array(y, [1, n1, 1, n2])
    do i = 1, n2
        y(:, i) = x(:, i)*a(i)
    end do
end function xdiag_mat_mat_double
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
!!!#define matx_mat_vec_complex matx_mat_vec_complex_complex_complex
function trace_complex(m) result(tr)
    complex, dimension(:, :), intent(in) :: m
    complex :: tr
    integer :: i
    tr = 0.0
    do i = 1, min(size(m, 1), size(m, 2))
        tr = tr + m(i, i)
    end do
end function trace_complex
function circulant_complex(c, nrow) result(t)
    complex, dimension(:) :: c
    integer, intent(in), optional :: nrow
    complex, allocatable, dimension(:, :) :: t
    integer :: n, i
    integer :: n2
    n = size(c)
    if (present(nrow)) then
        n2 = nrow
    else
        n2 = n
    end if
    call alloc_array(t, [1, n, 1, n2])
    t(:, 1) = c(:)
    do i = 2, n2
        t(:, i) = cshift(t(:, i - 1), -1)
    end do
end function circulant_complex
function toeplitz_complex(c, r) result(t)
    complex, dimension(:) :: c, r
    complex, allocatable, dimension(:, :) :: t
    integer :: n1, n2, i, j
    call assert(c(1) == r(1), ' Error: c(1) /= r(1) ')
    n1 = size(c)
    n2 = size(r)
    call alloc_array(t, [1, n1, 1, n2])
    do i = 1, n1
        t(i, i:n2) = r(1:n2 - i + 1)
    end do
    do j = 1, n2
        t(j:n1, j) = c(1:n1 - j + 1)
    end do
end function toeplitz_complex
function hankel_complex(x) result(h)
    complex, dimension(:), intent(in) :: x
    complex, allocatable, dimension(:, :) :: h
    integer :: nx, m, n, i
    nx = size(x)
    if (mod(nx, 2) == 0) then
        n = nx/2
        m = nx/2 + 1
    else
        n = (nx + 1)/2
        m = n
    end if
    call alloc_array(h, [1, n, 1, n])
    do i = 1, n
        h(:, i) = x(i:i + m - 1)
    end do
end function hankel_complex
function vandermonde_complex(x, n) result(h)
    complex, dimension(:) :: x
    integer :: n
    complex, allocatable, dimension(:, :) :: h
    integer :: i
    call alloc_array(h, [1, size(x), 1, n])
    do i = 1, n
        h(:, i) = x**(i - 1.0)
    end do
end function vandermonde_complex
function det_nxn_complex(w) result(wdet)
    complex, dimension(:, :), intent(in) :: w
    real :: wdet, sgn
    integer :: n1, n2, i
    integer, allocatable, dimension(:) :: ipiv
    complex, allocatable, dimension(:, :) :: wt
    n1 = size(w, 1)
    n2 = size(w, 2)
    if (n1 == 2 .and. n2 == 2) then
        ! 2x2 matrix
        wdet = w(1, 1)*w(2, 2) - w(1, 2)*w(2, 1)
    else if (n1 == 3 .and. n2 == 3) then
        ! 3x3 matrix
        wdet = &
            -w(1, 3)*w(2, 2)*w(3, 1) &
            + w(1, 2)*w(2, 3)*w(3, 1) &
            + w(1, 3)*w(2, 1)*w(3, 2) &
            - w(1, 1)*w(2, 3)*w(3, 2) &
            - w(1, 2)*w(2, 1)*w(3, 3) &
            + w(1, 1)*w(2, 2)*w(3, 3)
    else
        ! nxn matrix
        allocate (wt(1:n1, 1:n2), source=w)
        allocate (ipiv(1:n1))
        ipiv = 0
        call getrf(wt, ipiv)
        wdet = 1.0
        do i = 1, n1
            wdet = wdet*wt(i, i)
        end do
        sgn = 1.0
        do i = 1, n1
            if (ipiv(i) /= i) then
                sgn = -sgn
            end if
        end do
        wdet = sgn*wdet
    end if
end function det_nxn_complex
function inv_nxn_complex(w) result(winv)
    complex, dimension(:, :), intent(in) :: w
    integer :: n1, n2
    complex, allocatable, dimension(:, :) :: winv
    integer, allocatable, dimension(:) :: ipiv
    complex :: wdet
    n1 = size(w, 1)
    n2 = size(w, 2)
    if (n1 == 2 .and. n2 == 2) then
        ! 2x2 matrix
        allocate (winv(1:n1, 1:n2))
        wdet = det_nxn_complex(w)
        winv(1, 1) = w(2, 2)
        winv(1, 2) = -w(1, 2)
        winv(2, 1) = -w(2, 1)
        winv(2, 2) = w(1, 1)
        winv = winv/wdet
    else if (n1 == 3 .and. n2 == 3) then
        ! 3x3 matrix
        allocate (winv(1:n1, 1:n2))
        wdet = det_nxn_complex(w)
        winv(1, 1) = (-w(2, 3)*w(3, 2) + w(2, 2)*w(3, 3))/wdet
        winv(1, 2) = (w(1, 3)*w(3, 2) - w(1, 2)*w(3, 3))/wdet
        winv(1, 3) = (-w(1, 3)*w(2, 2) + w(1, 2)*w(2, 3))/wdet
        winv(2, 1) = (w(2, 3)*w(3, 1) - w(2, 1)*w(3, 3))/wdet
        winv(2, 2) = (-w(1, 3)*w(3, 1) + w(1, 1)*w(3, 3))/wdet
        winv(2, 3) = (w(1, 3)*w(2, 1) - w(1, 1)*w(2, 3))/wdet
        winv(3, 1) = (-w(2, 2)*w(3, 1) + w(2, 1)*w(3, 2))/wdet
        winv(3, 2) = (w(1, 2)*w(3, 1) - w(1, 1)*w(3, 2))/wdet
        winv(3, 3) = (-w(1, 2)*w(2, 1) + w(1, 1)*w(2, 2))/wdet
    else
        ! nxn matrix
        allocate (winv(1:n1, 1:n2), source=w)
        allocate (ipiv(1:n1))
        ipiv = 0
        call getrf(winv, ipiv)
        call getri(winv, ipiv)
    end if
end function inv_nxn_complex
subroutine svd_complex(a, s, u, vt)
    complex, dimension(:, :), intent(in) :: a
    real, allocatable, dimension(:), intent(out) :: s
    complex, allocatable, dimension(:, :), intent(out) :: u, vt
    integer :: m, n
    complex, allocatable, dimension(:, :) :: w
    m = size(a, 1)
    n = size(a, 2)
    call alloc_array(w, [1, m, 1, n], source=a)
    call alloc_array(s, [1, min(m, n)])
    call alloc_array(u, [1, m, 1, m])
    call alloc_array(vt, [1, n, 1, n])
    call gesvd(w, s, u, vt)
end subroutine svd_complex
function solve_single_rhs_complex(a, b) result(x)
    complex, dimension(:, :) :: a
    complex, dimension(:) :: b
    complex, allocatable, dimension(:) :: x
    complex, allocatable, dimension(:, :) :: ac
    ac = a
    x = b
    call gesv(ac, x)
end function solve_single_rhs_complex
function solve_multiple_rhs_complex(a, b) result(x)
    complex, dimension(:, :) :: a
    complex, dimension(:, :) :: b
    complex, allocatable, dimension(:, :) :: x
    complex, allocatable, dimension(:, :) :: ac
    ac = a
    x = b
    call gesv(ac, x)
end function solve_multiple_rhs_complex
function least_squares_solve_single_rhs_complex(a, b) result(x)
    complex, dimension(:, :) :: a
    complex, dimension(:) :: b
    complex, allocatable, dimension(:) :: x, xt
    integer :: m, n
    complex, allocatable, dimension(:, :) :: ac
    m = size(a, 1)
    n = size(a, 2)
    ac = a
    if (size(b) /= m) then
        print *, ' Error: size(a) and size(b) mismatch. '
        stop
    end if
    xt = zeros(max(m, n))
    xt(1:m) = b
    call gels(ac, xt)
    x = xt(1:n)
end function least_squares_solve_single_rhs_complex
function least_squares_solve_multiple_rhs_complex(a, b) result(x)
    complex, dimension(:, :) :: a
    complex, dimension(:, :) :: b
    complex, allocatable, dimension(:, :) :: x, xt
    integer :: m, n, nrhs
    complex, allocatable, dimension(:, :) :: ac
    m = size(a, 1)
    n = size(a, 2)
    nrhs = size(b, 2)
    ! allocate (ac(1:m, 1:n), source=a)
    ac = a
    if (size(b, 1) /= m) then
        print *, ' Error: size(a) and size(b) mismatch. '
        stop
    end if
    xt = zeros(max(m, n), nrhs)
    xt(1:m, 1:nrhs) = b
    call gels(ac, xt)
    x = xt(1:n, 1:nrhs)
end function least_squares_solve_multiple_rhs_complex
function solve_band_single_rhs_complex(a, nu, nl, b) result(x)
    complex, dimension(:, :), intent(in) :: a
    integer, intent(in) :: nu, nl
    complex, dimension(:), intent(in) :: b
    complex, allocatable, dimension(:) :: x
    complex, allocatable, dimension(:, :) :: ac
    integer :: m, n, i
    ! band storage: https://www.netlib.org/lapack/lug/node124.html
    ! https://www.intel.com/content/www/us/en/develop/documentation/onemkl-developer-reference-fortran/top/lapack-routines/lapack-linear-equation-routines/lapack-linear-equation-driver-routines/gbsv.html
    ! http://www.netlib.org/lapack/explore-html/dc/db2/group__real_g_bsolve_ga3656935309a19ed624052103572a4a47.html#ga3656935309a19ed624052103572a4a47
    m = size(a, 1)
    call assert(m == nu + nl + 1, &
        '<solve_band_single_rhs> Error: Dimension of a is inconsistent.')
    n = size(a, 2)
    do i = 1, nu
        call assert(all(a(nu + 1 - i, 1:i) == 0), &
            '<solve_band_single_rhs> Error: Upper half is not consistent.')
    end do
    do i = 1, nl
        call assert(all(a(nu + 1 + i, n - i + 1:n) == 0), &
            '<solve_band_single_rhs> Error: Lower half is not consistent.')
    end do
    ! dim1 of input ac = m + nl, where the first nl rows are zeros, and nl + 1: rows are a
    ac = zeros(m + nl, n)
    ac(nl + 1:m + nl, :) = a
    x = b
    call gbsv(ac, x, nl)
end function solve_band_single_rhs_complex
function matx_mat_vec_complex(a, x) result(y)
    complex, dimension(:, :) :: a
    complex, dimension(:) :: x
    complex, allocatable, dimension(:) :: y
    integer :: n
    n = size(a, 1)
    call alloc_array(y, [1, n])
    call gemv(a, x, y)
end function matx_mat_vec_complex
function matx_mat_mat_complex(a, x) result(y)
    complex, dimension(:, :) :: a
    complex, dimension(:, :) :: x
    complex, allocatable, dimension(:, :) :: y
    integer :: n1, n2
    n1 = size(a, 1)
    n2 = size(x, 2)
    call alloc_array(y, [1, n1, 1, n2])
    call gemm(a, x, y)
end function matx_mat_mat_complex
function diagx_mat_mat_complex(a, x) result(y)
    complex, dimension(:) :: a
    complex, dimension(:, :) :: x
    complex, allocatable, dimension(:, :) :: y
    integer :: n1, n2, i
    n1 = size(a)
    n2 = size(x, 2)
    call alloc_array(y, [1, n1, 1, n2])
    do i = 1, n1
        y(i, :) = x(i, :)*a(i)
    end do
end function diagx_mat_mat_complex
function xdiag_mat_mat_complex(x, a) result(y)
    complex, dimension(:, :) :: x
    complex, dimension(:) :: a
    complex, allocatable, dimension(:, :) :: y
    integer :: n1, n2, i
    n1 = size(x, 1)
    n2 = size(a)
    call alloc_array(y, [1, n1, 1, n2])
    do i = 1, n2
        y(:, i) = x(:, i)*a(i)
    end do
end function xdiag_mat_mat_complex
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
!!!#define matx_mat_vec_dcomplex matx_mat_vec_dcomplex_dcomplex_dcomplex
function trace_dcomplex(m) result(tr)
    double complex, dimension(:, :), intent(in) :: m
    double complex :: tr
    integer :: i
    tr = 0.0
    do i = 1, min(size(m, 1), size(m, 2))
        tr = tr + m(i, i)
    end do
end function trace_dcomplex
function circulant_dcomplex(c, nrow) result(t)
    double complex, dimension(:) :: c
    integer, intent(in), optional :: nrow
    double complex, allocatable, dimension(:, :) :: t
    integer :: n, i
    integer :: n2
    n = size(c)
    if (present(nrow)) then
        n2 = nrow
    else
        n2 = n
    end if
    call alloc_array(t, [1, n, 1, n2])
    t(:, 1) = c(:)
    do i = 2, n2
        t(:, i) = cshift(t(:, i - 1), -1)
    end do
end function circulant_dcomplex
function toeplitz_dcomplex(c, r) result(t)
    double complex, dimension(:) :: c, r
    double complex, allocatable, dimension(:, :) :: t
    integer :: n1, n2, i, j
    call assert(c(1) == r(1), ' Error: c(1) /= r(1) ')
    n1 = size(c)
    n2 = size(r)
    call alloc_array(t, [1, n1, 1, n2])
    do i = 1, n1
        t(i, i:n2) = r(1:n2 - i + 1)
    end do
    do j = 1, n2
        t(j:n1, j) = c(1:n1 - j + 1)
    end do
end function toeplitz_dcomplex
function hankel_dcomplex(x) result(h)
    double complex, dimension(:), intent(in) :: x
    double complex, allocatable, dimension(:, :) :: h
    integer :: nx, m, n, i
    nx = size(x)
    if (mod(nx, 2) == 0) then
        n = nx/2
        m = nx/2 + 1
    else
        n = (nx + 1)/2
        m = n
    end if
    call alloc_array(h, [1, n, 1, n])
    do i = 1, n
        h(:, i) = x(i:i + m - 1)
    end do
end function hankel_dcomplex
function vandermonde_dcomplex(x, n) result(h)
    double complex, dimension(:) :: x
    integer :: n
    double complex, allocatable, dimension(:, :) :: h
    integer :: i
    call alloc_array(h, [1, size(x), 1, n])
    do i = 1, n
        h(:, i) = x**(i - 1.0)
    end do
end function vandermonde_dcomplex
function det_nxn_dcomplex(w) result(wdet)
    double complex, dimension(:, :), intent(in) :: w
    real :: wdet, sgn
    integer :: n1, n2, i
    integer, allocatable, dimension(:) :: ipiv
    double complex, allocatable, dimension(:, :) :: wt
    n1 = size(w, 1)
    n2 = size(w, 2)
    if (n1 == 2 .and. n2 == 2) then
        ! 2x2 matrix
        wdet = w(1, 1)*w(2, 2) - w(1, 2)*w(2, 1)
    else if (n1 == 3 .and. n2 == 3) then
        ! 3x3 matrix
        wdet = &
            -w(1, 3)*w(2, 2)*w(3, 1) &
            + w(1, 2)*w(2, 3)*w(3, 1) &
            + w(1, 3)*w(2, 1)*w(3, 2) &
            - w(1, 1)*w(2, 3)*w(3, 2) &
            - w(1, 2)*w(2, 1)*w(3, 3) &
            + w(1, 1)*w(2, 2)*w(3, 3)
    else
        ! nxn matrix
        allocate (wt(1:n1, 1:n2), source=w)
        allocate (ipiv(1:n1))
        ipiv = 0
        call getrf(wt, ipiv)
        wdet = 1.0
        do i = 1, n1
            wdet = wdet*wt(i, i)
        end do
        sgn = 1.0
        do i = 1, n1
            if (ipiv(i) /= i) then
                sgn = -sgn
            end if
        end do
        wdet = sgn*wdet
    end if
end function det_nxn_dcomplex
function inv_nxn_dcomplex(w) result(winv)
    double complex, dimension(:, :), intent(in) :: w
    integer :: n1, n2
    double complex, allocatable, dimension(:, :) :: winv
    integer, allocatable, dimension(:) :: ipiv
    double complex :: wdet
    n1 = size(w, 1)
    n2 = size(w, 2)
    if (n1 == 2 .and. n2 == 2) then
        ! 2x2 matrix
        allocate (winv(1:n1, 1:n2))
        wdet = det_nxn_dcomplex(w)
        winv(1, 1) = w(2, 2)
        winv(1, 2) = -w(1, 2)
        winv(2, 1) = -w(2, 1)
        winv(2, 2) = w(1, 1)
        winv = winv/wdet
    else if (n1 == 3 .and. n2 == 3) then
        ! 3x3 matrix
        allocate (winv(1:n1, 1:n2))
        wdet = det_nxn_dcomplex(w)
        winv(1, 1) = (-w(2, 3)*w(3, 2) + w(2, 2)*w(3, 3))/wdet
        winv(1, 2) = (w(1, 3)*w(3, 2) - w(1, 2)*w(3, 3))/wdet
        winv(1, 3) = (-w(1, 3)*w(2, 2) + w(1, 2)*w(2, 3))/wdet
        winv(2, 1) = (w(2, 3)*w(3, 1) - w(2, 1)*w(3, 3))/wdet
        winv(2, 2) = (-w(1, 3)*w(3, 1) + w(1, 1)*w(3, 3))/wdet
        winv(2, 3) = (w(1, 3)*w(2, 1) - w(1, 1)*w(2, 3))/wdet
        winv(3, 1) = (-w(2, 2)*w(3, 1) + w(2, 1)*w(3, 2))/wdet
        winv(3, 2) = (w(1, 2)*w(3, 1) - w(1, 1)*w(3, 2))/wdet
        winv(3, 3) = (-w(1, 2)*w(2, 1) + w(1, 1)*w(2, 2))/wdet
    else
        ! nxn matrix
        allocate (winv(1:n1, 1:n2), source=w)
        allocate (ipiv(1:n1))
        ipiv = 0
        call getrf(winv, ipiv)
        call getri(winv, ipiv)
    end if
end function inv_nxn_dcomplex
subroutine svd_dcomplex(a, s, u, vt)
    double complex, dimension(:, :), intent(in) :: a
    double precision, allocatable, dimension(:), intent(out) :: s
    double complex, allocatable, dimension(:, :), intent(out) :: u, vt
    integer :: m, n
    double complex, allocatable, dimension(:, :) :: w
    m = size(a, 1)
    n = size(a, 2)
    call alloc_array(w, [1, m, 1, n], source=a)
    call alloc_array(s, [1, min(m, n)])
    call alloc_array(u, [1, m, 1, m])
    call alloc_array(vt, [1, n, 1, n])
    call gesvd(w, s, u, vt)
end subroutine svd_dcomplex
function solve_single_rhs_dcomplex(a, b) result(x)
    double complex, dimension(:, :) :: a
    double complex, dimension(:) :: b
    double complex, allocatable, dimension(:) :: x
    double complex, allocatable, dimension(:, :) :: ac
    ac = a
    x = b
    call gesv(ac, x)
end function solve_single_rhs_dcomplex
function solve_multiple_rhs_dcomplex(a, b) result(x)
    double complex, dimension(:, :) :: a
    double complex, dimension(:, :) :: b
    double complex, allocatable, dimension(:, :) :: x
    double complex, allocatable, dimension(:, :) :: ac
    ac = a
    x = b
    call gesv(ac, x)
end function solve_multiple_rhs_dcomplex
function least_squares_solve_single_rhs_dcomplex(a, b) result(x)
    double complex, dimension(:, :) :: a
    double complex, dimension(:) :: b
    double complex, allocatable, dimension(:) :: x, xt
    integer :: m, n
    double complex, allocatable, dimension(:, :) :: ac
    m = size(a, 1)
    n = size(a, 2)
    ac = a
    if (size(b) /= m) then
        print *, ' Error: size(a) and size(b) mismatch. '
        stop
    end if
    xt = zeros(max(m, n))
    xt(1:m) = b
    call gels(ac, xt)
    x = xt(1:n)
end function least_squares_solve_single_rhs_dcomplex
function least_squares_solve_multiple_rhs_dcomplex(a, b) result(x)
    double complex, dimension(:, :) :: a
    double complex, dimension(:, :) :: b
    double complex, allocatable, dimension(:, :) :: x, xt
    integer :: m, n, nrhs
    double complex, allocatable, dimension(:, :) :: ac
    m = size(a, 1)
    n = size(a, 2)
    nrhs = size(b, 2)
    ! allocate (ac(1:m, 1:n), source=a)
    ac = a
    if (size(b, 1) /= m) then
        print *, ' Error: size(a) and size(b) mismatch. '
        stop
    end if
    xt = zeros(max(m, n), nrhs)
    xt(1:m, 1:nrhs) = b
    call gels(ac, xt)
    x = xt(1:n, 1:nrhs)
end function least_squares_solve_multiple_rhs_dcomplex
function solve_band_single_rhs_dcomplex(a, nu, nl, b) result(x)
    double complex, dimension(:, :), intent(in) :: a
    integer, intent(in) :: nu, nl
    double complex, dimension(:), intent(in) :: b
    double complex, allocatable, dimension(:) :: x
    double complex, allocatable, dimension(:, :) :: ac
    integer :: m, n, i
    ! band storage: https://www.netlib.org/lapack/lug/node124.html
    ! https://www.intel.com/content/www/us/en/develop/documentation/onemkl-developer-reference-fortran/top/lapack-routines/lapack-linear-equation-routines/lapack-linear-equation-driver-routines/gbsv.html
    ! http://www.netlib.org/lapack/explore-html/dc/db2/group__real_g_bsolve_ga3656935309a19ed624052103572a4a47.html#ga3656935309a19ed624052103572a4a47
    m = size(a, 1)
    call assert(m == nu + nl + 1, &
        '<solve_band_single_rhs> Error: Dimension of a is inconsistent.')
    n = size(a, 2)
    do i = 1, nu
        call assert(all(a(nu + 1 - i, 1:i) == 0), &
            '<solve_band_single_rhs> Error: Upper half is not consistent.')
    end do
    do i = 1, nl
        call assert(all(a(nu + 1 + i, n - i + 1:n) == 0), &
            '<solve_band_single_rhs> Error: Lower half is not consistent.')
    end do
    ! dim1 of input ac = m + nl, where the first nl rows are zeros, and nl + 1: rows are a
    ac = zeros(m + nl, n)
    ac(nl + 1:m + nl, :) = a
    x = b
    call gbsv(ac, x, nl)
end function solve_band_single_rhs_dcomplex
function matx_mat_vec_dcomplex(a, x) result(y)
    double complex, dimension(:, :) :: a
    double complex, dimension(:) :: x
    double complex, allocatable, dimension(:) :: y
    integer :: n
    n = size(a, 1)
    call alloc_array(y, [1, n])
    call gemv(a, x, y)
end function matx_mat_vec_dcomplex
function matx_mat_mat_dcomplex(a, x) result(y)
    double complex, dimension(:, :) :: a
    double complex, dimension(:, :) :: x
    double complex, allocatable, dimension(:, :) :: y
    integer :: n1, n2
    n1 = size(a, 1)
    n2 = size(x, 2)
    call alloc_array(y, [1, n1, 1, n2])
    call gemm(a, x, y)
end function matx_mat_mat_dcomplex
function diagx_mat_mat_dcomplex(a, x) result(y)
    double complex, dimension(:) :: a
    double complex, dimension(:, :) :: x
    double complex, allocatable, dimension(:, :) :: y
    integer :: n1, n2, i
    n1 = size(a)
    n2 = size(x, 2)
    call alloc_array(y, [1, n1, 1, n2])
    do i = 1, n1
        y(i, :) = x(i, :)*a(i)
    end do
end function diagx_mat_mat_dcomplex
function xdiag_mat_mat_dcomplex(x, a) result(y)
    double complex, dimension(:, :) :: x
    double complex, dimension(:) :: a
    double complex, allocatable, dimension(:, :) :: y
    integer :: n1, n2, i
    n1 = size(x, 1)
    n2 = size(a)
    call alloc_array(y, [1, n1, 1, n2])
    do i = 1, n2
        y(:, i) = x(:, i)*a(i)
    end do
end function xdiag_mat_mat_dcomplex
    ! Eigensystem solvers
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
subroutine spectral_radius_float(a, v, b, niter)
    real, dimension(:, :), intent(in) :: a
    real, intent(inout) :: v
    real, allocatable, dimension(:), intent(inout) :: b
    integer, intent(in), optional :: niter
    real, allocatable, dimension(:) :: b_k, b_k1
    integer :: n, i, spectral_radius_niter
    n = size(a, 2)
    b_k = random(n)
    b_k1 = zeros(n)
    if (present(niter)) then
        spectral_radius_niter = niter
    else
        spectral_radius_niter = 10
    end if
    i = 1
    do while (i <= spectral_radius_niter)
        b_k1 = matx(a, b_k)
        b_k = b_k1/norm2(b_k1)
        i = i + 1
    end do
    v = sum(b_k1*b_k)/sum(b_k**2)
    b = b_k
end subroutine spectral_radius_float
!
!> Eigen solver for 2x2 symmetric matrix
!
subroutine eigen_symm2x2_float(a, d, v, order)
    real, dimension(:, :), intent(in) :: a
    real, allocatable, dimension(:), intent(inout) :: d
    real, allocatable, dimension(:, :), intent(inout) :: v
    integer, intent(in), optional :: order
    real, parameter :: flt_epsilon = 1.0e-6
    real :: a00, a01, a11
    real :: v00, v01, v10, v11
    real :: c, r, s, t, u, vpr, vqr, edt
    real, allocatable :: vt(:)
    real :: vtiny
    integer :: eigenval_sort
    if (present(order)) then
        eigenval_sort = order
    else
        eigenval_sort = -1
    end if
    ! Initialize
    d = zeros(2)
    v = zeros(2, 2)
    vt = zeros(2)
    ! Copy matrix to local variables.
    a00 = a(1, 1)
    a01 = a(1, 2)
    a11 = a(2, 2)
    ! Initial eigenvectors.
    v00 = 1.0d0
    v01 = 0.0d0
    v10 = 0.0d0
    v11 = 1.0d0
    ! If off-diagonal element is non-zero, zero it with a Jacobi rotation.
    if (a01 /= 0.0d0) then
        ! avoid overflow in r*r below
        vtiny = 0.1d0*sqrt(flt_epsilon)
        u = a11 - a00
        if (abs(a01) < vtiny*abs(u)) then
            t = a01/u
        else
            r = 0.5d0*u/a01
            if (r >= 0.0d0) then
                t = 1.0d0/(r + sqrt(1.0d0 + r**2))
            else
                t = 1.0d0/(r - sqrt(1.0d0 + r**2))
            end if
        end if
        c = 1.0d0/sqrt(1.0d0 + t**2)
        s = t*c
        u = s/(1.0d0 + c)
        r = t*a01
        a00 = a00 - r
        a11 = a11 + r
        vpr = v00
        vqr = v10
        v00 = vpr - s*(vqr + vpr*u)
        v10 = vqr + s*(vpr - vqr*u)
        vpr = v01
        vqr = v11
        v01 = vpr - s*(vqr + vpr*u)
        v11 = vqr + s*(vpr - vqr*u)
    end if
    ! Copy eigenvalues and eigenvectors to output arrays.
    d(1) = a00
    d(2) = a11
    v(1, 1) = v00
    v(1, 2) = v01
    v(2, 1) = v10
    v(2, 2) = v11
    select case (eigenval_sort)
        case (1)
            ! Sort eigenvalues (and eigenvectors) in descending order.
            if (d(1) > d(2)) then
                edt = d(2)
                d(2) = d(1)
                d(1) = edt
                vt(1) = v(2, 1)
                vt(2) = v(2, 2)
                v(2, 1) = v(1, 1)
                v(2, 2) = v(1, 2)
                v(1, 1) = vt(1)
                v(1, 2) = vt(2)
            end if
        case (-1)
            ! Sort eigenvalues (and eigenvectors) in descending order.
            if (d(1) < d(2)) then
                edt = d(2)
                d(2) = d(1)
                d(1) = edt
                vt(1) = v(2, 1)
                vt(2) = v(2, 2)
                v(2, 1) = v(1, 1)
                v(2, 2) = v(1, 2)
                v(1, 1) = vt(1)
                v(1, 2) = vt(2)
            end if
    end select
    ! Transpose to make each colum an eigenvector
    v = transpose(v)
end subroutine eigen_symm2x2_float
!
!> Eigen solver for 3x3 symmetric matrix
!
!> Efficient numerical diagonalization of Hermitian 3x3 matrices, Int. J. Mod. Phys. C 19 (2008) 523-548
!> https://www.mpi-hd.mpg.de/personalhomes/globes/3x3/
!
subroutine eigen_symm3x3_float(ar, w, q, order)
    real, dimension(:, :), intent(in) :: ar
    real, allocatable, dimension(:), intent(inout) :: w
    real, allocatable, dimension(:, :), intent(inout) :: q
    integer, optional :: order
    real :: sd, so, s, c, t, g, h, z, theta, thresh
    integer :: i, x, y, r, j
    real, allocatable :: wd(:), a(:, :), qd(:, :)
    integer :: n, eigenval_sort
    if (present(order)) then
        eigenval_sort = order
    else
        eigenval_sort = -1
    end if
    ! Initialize
    w = zeros(3)
    q = zeros(3, 3)
    wd = zeros(3)
    qd = zeros(3, 3)
    n = 3
    a = ar
    ! initialize q to the identitity matrix
    ! --- this loop can be omitted if only the eigenvalues are desired ---
    q = 0.0d0
    do x = 1, n
        q(x, x) = 1.0d0
    end do
    ! initialize w to diag(a)
    do x = 1, n
        w(x) = a(x, x)
    end do
    ! calculate sqr(tr(a))
    sd = sum(abs(w))**2
    ! main iteration loop
    do i = 1, 50
        ! test for convergence
        so = 0.0d0
        do x = 1, n
            do y = x + 1, n
                so = so + abs(a(x, y))
            end do
        end do
        if (so == 0) then
            exit
        end if
        if (i < 4) then
            thresh = 0.2d0*so/n**2
        else
            thresh = 0.0d0
        end if
        ! do sweep
        do x = 1, n
            do y = x + 1, n
                g = 100.0d0*(abs(a(x, y)))
                if ((i > 4) .and. (abs(w(x)) + g == abs(w(x))) .and. (abs(w(y)) + g == abs(w(y)))) then
                    a(x, y) = 0.0d0
                else if (abs(a(x, y)) > thresh) then
                    ! calculate jacobi transformation
                    h = w(y) - w(x)
                    if (abs(h) + g == abs(h)) then
                        t = a(x, y)/h
                    else
                        theta = 0.5d0*h/a(x, y)
                        if (theta < 0.0d0) then
                            t = -1.0d0/(sqrt(1.0d0 + theta**2) - theta)
                        else
                            t = 1.0d0/(sqrt(1.0d0 + theta**2) + theta)
                        end if
                    end if
                    c = 1.0d0/sqrt(1.0d0 + t**2)
                    s = t*c
                    z = t*a(x, y)
                    ! apply jacobi transformation
                    a(x, y) = 0.0d0
                    w(x) = w(x) - z
                    w(y) = w(y) + z
                    do r = 1, x - 1
                        t = a(r, x)
                        a(r, x) = c*t - s*a(r, y)
                        a(r, y) = s*t + c*a(r, y)
                    end do
                    do r = x + 1, y - 1
                        t = a(x, r)
                        a(x, r) = c*t - s*a(r, y)
                        a(r, y) = s*t + c*a(r, y)
                    end do
                    do r = y + 1, n
                        t = a(x, r)
                        a(x, r) = c*t - s*a(y, r)
                        a(y, r) = s*t + c*a(y, r)
                    end do
                    ! update eigenvectors
                    ! --- this loop can be omitted if only the eigenvalues are desired ---
                    do r = 1, n
                        t = q(r, x)
                        q(r, x) = c*t - s*q(r, y)
                        q(r, y) = s*t + c*q(r, y)
                    end do
                end if
            end do
        end do
    end do
    ! Sort eigenvalues (and eigenvectors) to descending order
    wd = sort(w, eigenval_sort)
    do i = 1, 3
        do j = 1, 3
            if (wd(i) == w(j)) then
                qd(:, i) = q(:, j)
                exit
            end if
        end do
    end do
    ! Copy to output
    w = wd
    q = qd
end subroutine eigen_symm3x3_float
!
! A X = lambada X with A being real, symmetric
! Returns eigenvalues and left or right eigenvectors
!
subroutine eigen_simple_symmetric_l_or_r_float(a, w, v, lr, order)
    real, dimension(:, :), intent(in) :: a
    real, allocatable, dimension(:), intent(inout) :: w
    real, allocatable, dimension(:, :), intent(inout), optional :: v
    character(len=*), intent(in), optional :: lr
    integer, intent(in), optional :: order
    character(len=12) :: left_or_right
    integer :: n
    real, allocatable :: aa(:, :), wr(:), wi(:), vl(:, :), vr(:, :)
    integer, allocatable, dimension(:) :: windex
    integer :: eigenval_sort
    call assert(size(a, 1) == size(a, 2), 'Error: A must be square. ')
    call assert(all(a == transpose(a)), 'Error: A must be symmetric. ')
    if (present(lr)) then
        left_or_right = lr
    else
        left_or_right = 'right'
    end if
    if (present(order)) then
        eigenval_sort = order
    else
        eigenval_sort = -1
    end if
    n = size(a, 1)
    aa = a
    wr = zeros(n)
    wi = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    v = zeros(n, n)
    call geev(aa, wr, wi, vl, vr)
    w = wr
    ! Sort the eigenvalues descendingly
    allocate (windex(1:n))
    call sort_index(w, windex, eigenval_sort)
    if (present(v)) then
        select case (left_or_right)
            case ('right')
                v = vr
            case ('left')
                v = vl
        end select
        ! Arrange the eigenvectors accordingly
        v = v(:, windex)
    end if
end subroutine eigen_simple_symmetric_l_or_r_float
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
subroutine spectral_radius_double(a, v, b, niter)
    double precision, dimension(:, :), intent(in) :: a
    double precision, intent(inout) :: v
    double precision, allocatable, dimension(:), intent(inout) :: b
    integer, intent(in), optional :: niter
    double precision, allocatable, dimension(:) :: b_k, b_k1
    integer :: n, i, spectral_radius_niter
    n = size(a, 2)
    b_k = random(n)
    b_k1 = zeros(n)
    if (present(niter)) then
        spectral_radius_niter = niter
    else
        spectral_radius_niter = 10
    end if
    i = 1
    do while (i <= spectral_radius_niter)
        b_k1 = matx(a, b_k)
        b_k = b_k1/norm2(b_k1)
        i = i + 1
    end do
    v = sum(b_k1*b_k)/sum(b_k**2)
    b = b_k
end subroutine spectral_radius_double
!
!> Eigen solver for 2x2 symmetric matrix
!
subroutine eigen_symm2x2_double(a, d, v, order)
    double precision, dimension(:, :), intent(in) :: a
    double precision, allocatable, dimension(:), intent(inout) :: d
    double precision, allocatable, dimension(:, :), intent(inout) :: v
    integer, intent(in), optional :: order
    double precision, parameter :: flt_epsilon = 1.0e-6
    double precision :: a00, a01, a11
    double precision :: v00, v01, v10, v11
    double precision :: c, r, s, t, u, vpr, vqr, edt
    double precision, allocatable :: vt(:)
    double precision :: vtiny
    integer :: eigenval_sort
    if (present(order)) then
        eigenval_sort = order
    else
        eigenval_sort = -1
    end if
    ! Initialize
    d = zeros(2)
    v = zeros(2, 2)
    vt = zeros(2)
    ! Copy matrix to local variables.
    a00 = a(1, 1)
    a01 = a(1, 2)
    a11 = a(2, 2)
    ! Initial eigenvectors.
    v00 = 1.0d0
    v01 = 0.0d0
    v10 = 0.0d0
    v11 = 1.0d0
    ! If off-diagonal element is non-zero, zero it with a Jacobi rotation.
    if (a01 /= 0.0d0) then
        ! avoid overflow in r*r below
        vtiny = 0.1d0*sqrt(flt_epsilon)
        u = a11 - a00
        if (abs(a01) < vtiny*abs(u)) then
            t = a01/u
        else
            r = 0.5d0*u/a01
            if (r >= 0.0d0) then
                t = 1.0d0/(r + sqrt(1.0d0 + r**2))
            else
                t = 1.0d0/(r - sqrt(1.0d0 + r**2))
            end if
        end if
        c = 1.0d0/sqrt(1.0d0 + t**2)
        s = t*c
        u = s/(1.0d0 + c)
        r = t*a01
        a00 = a00 - r
        a11 = a11 + r
        vpr = v00
        vqr = v10
        v00 = vpr - s*(vqr + vpr*u)
        v10 = vqr + s*(vpr - vqr*u)
        vpr = v01
        vqr = v11
        v01 = vpr - s*(vqr + vpr*u)
        v11 = vqr + s*(vpr - vqr*u)
    end if
    ! Copy eigenvalues and eigenvectors to output arrays.
    d(1) = a00
    d(2) = a11
    v(1, 1) = v00
    v(1, 2) = v01
    v(2, 1) = v10
    v(2, 2) = v11
    select case (eigenval_sort)
        case (1)
            ! Sort eigenvalues (and eigenvectors) in descending order.
            if (d(1) > d(2)) then
                edt = d(2)
                d(2) = d(1)
                d(1) = edt
                vt(1) = v(2, 1)
                vt(2) = v(2, 2)
                v(2, 1) = v(1, 1)
                v(2, 2) = v(1, 2)
                v(1, 1) = vt(1)
                v(1, 2) = vt(2)
            end if
        case (-1)
            ! Sort eigenvalues (and eigenvectors) in descending order.
            if (d(1) < d(2)) then
                edt = d(2)
                d(2) = d(1)
                d(1) = edt
                vt(1) = v(2, 1)
                vt(2) = v(2, 2)
                v(2, 1) = v(1, 1)
                v(2, 2) = v(1, 2)
                v(1, 1) = vt(1)
                v(1, 2) = vt(2)
            end if
    end select
    ! Transpose to make each colum an eigenvector
    v = transpose(v)
end subroutine eigen_symm2x2_double
!
!> Eigen solver for 3x3 symmetric matrix
!
!> Efficient numerical diagonalization of Hermitian 3x3 matrices, Int. J. Mod. Phys. C 19 (2008) 523-548
!> https://www.mpi-hd.mpg.de/personalhomes/globes/3x3/
!
subroutine eigen_symm3x3_double(ar, w, q, order)
    double precision, dimension(:, :), intent(in) :: ar
    double precision, allocatable, dimension(:), intent(inout) :: w
    double precision, allocatable, dimension(:, :), intent(inout) :: q
    integer, optional :: order
    double precision :: sd, so, s, c, t, g, h, z, theta, thresh
    integer :: i, x, y, r, j
    double precision, allocatable :: wd(:), a(:, :), qd(:, :)
    integer :: n, eigenval_sort
    if (present(order)) then
        eigenval_sort = order
    else
        eigenval_sort = -1
    end if
    ! Initialize
    w = zeros(3)
    q = zeros(3, 3)
    wd = zeros(3)
    qd = zeros(3, 3)
    n = 3
    a = ar
    ! initialize q to the identitity matrix
    ! --- this loop can be omitted if only the eigenvalues are desired ---
    q = 0.0d0
    do x = 1, n
        q(x, x) = 1.0d0
    end do
    ! initialize w to diag(a)
    do x = 1, n
        w(x) = a(x, x)
    end do
    ! calculate sqr(tr(a))
    sd = sum(abs(w))**2
    ! main iteration loop
    do i = 1, 50
        ! test for convergence
        so = 0.0d0
        do x = 1, n
            do y = x + 1, n
                so = so + abs(a(x, y))
            end do
        end do
        if (so == 0) then
            exit
        end if
        if (i < 4) then
            thresh = 0.2d0*so/n**2
        else
            thresh = 0.0d0
        end if
        ! do sweep
        do x = 1, n
            do y = x + 1, n
                g = 100.0d0*(abs(a(x, y)))
                if ((i > 4) .and. (abs(w(x)) + g == abs(w(x))) .and. (abs(w(y)) + g == abs(w(y)))) then
                    a(x, y) = 0.0d0
                else if (abs(a(x, y)) > thresh) then
                    ! calculate jacobi transformation
                    h = w(y) - w(x)
                    if (abs(h) + g == abs(h)) then
                        t = a(x, y)/h
                    else
                        theta = 0.5d0*h/a(x, y)
                        if (theta < 0.0d0) then
                            t = -1.0d0/(sqrt(1.0d0 + theta**2) - theta)
                        else
                            t = 1.0d0/(sqrt(1.0d0 + theta**2) + theta)
                        end if
                    end if
                    c = 1.0d0/sqrt(1.0d0 + t**2)
                    s = t*c
                    z = t*a(x, y)
                    ! apply jacobi transformation
                    a(x, y) = 0.0d0
                    w(x) = w(x) - z
                    w(y) = w(y) + z
                    do r = 1, x - 1
                        t = a(r, x)
                        a(r, x) = c*t - s*a(r, y)
                        a(r, y) = s*t + c*a(r, y)
                    end do
                    do r = x + 1, y - 1
                        t = a(x, r)
                        a(x, r) = c*t - s*a(r, y)
                        a(r, y) = s*t + c*a(r, y)
                    end do
                    do r = y + 1, n
                        t = a(x, r)
                        a(x, r) = c*t - s*a(y, r)
                        a(y, r) = s*t + c*a(y, r)
                    end do
                    ! update eigenvectors
                    ! --- this loop can be omitted if only the eigenvalues are desired ---
                    do r = 1, n
                        t = q(r, x)
                        q(r, x) = c*t - s*q(r, y)
                        q(r, y) = s*t + c*q(r, y)
                    end do
                end if
            end do
        end do
    end do
    ! Sort eigenvalues (and eigenvectors) to descending order
    wd = sort(w, eigenval_sort)
    do i = 1, 3
        do j = 1, 3
            if (wd(i) == w(j)) then
                qd(:, i) = q(:, j)
                exit
            end if
        end do
    end do
    ! Copy to output
    w = wd
    q = qd
end subroutine eigen_symm3x3_double
!
! A X = lambada X with A being real, symmetric
! Returns eigenvalues and left or right eigenvectors
!
subroutine eigen_simple_symmetric_l_or_r_double(a, w, v, lr, order)
    double precision, dimension(:, :), intent(in) :: a
    double precision, allocatable, dimension(:), intent(inout) :: w
    double precision, allocatable, dimension(:, :), intent(inout), optional :: v
    character(len=*), intent(in), optional :: lr
    integer, intent(in), optional :: order
    character(len=12) :: left_or_right
    integer :: n
    double precision, allocatable :: aa(:, :), wr(:), wi(:), vl(:, :), vr(:, :)
    integer, allocatable, dimension(:) :: windex
    integer :: eigenval_sort
    call assert(size(a, 1) == size(a, 2), 'Error: A must be square. ')
    call assert(all(a == transpose(a)), 'Error: A must be symmetric. ')
    if (present(lr)) then
        left_or_right = lr
    else
        left_or_right = 'right'
    end if
    if (present(order)) then
        eigenval_sort = order
    else
        eigenval_sort = -1
    end if
    n = size(a, 1)
    aa = a
    wr = zeros(n)
    wi = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    v = zeros(n, n)
    call geev(aa, wr, wi, vl, vr)
    w = wr
    ! Sort the eigenvalues descendingly
    allocate (windex(1:n))
    call sort_index(w, windex, eigenval_sort)
    if (present(v)) then
        select case (left_or_right)
            case ('right')
                v = vr
            case ('left')
                v = vl
        end select
        ! Arrange the eigenvectors accordingly
        v = v(:, windex)
    end if
end subroutine eigen_simple_symmetric_l_or_r_double
    ! General eigensystem solvers
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
! ==============================================================
! A X = lambada X
! Returns eigenvalues and left or right eigenvectors
!
subroutine eigen_simple_l_or_r_float(a, w, v, lr)
    real, dimension(:, :), intent(in) :: a
    complex, allocatable, dimension(:), intent(inout) :: w
    complex, allocatable, dimension(:, :), intent(inout), optional :: v
    character(len=*), intent(in), optional :: lr
    character(len=12) :: left_or_right
    integer :: n
    complex, allocatable :: ac(:, :), vl(:, :), vr(:, :)
    call assert(size(a, 1) /= size(a, 2), &
        ' <eigen_general_l_or_r> Error: a must be square. ')
    if (present(lr)) then
        left_or_right = lr
    else
        left_or_right = 'right'
    end if
    n = size(a, 1)
    ac = cmplx(a)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call geev(ac, w, vl, vr)
    if (present(v)) then
        select case (left_or_right)
            case ('right')
                v = vr
            case ('left')
                v = vl
        end select
    end if
end subroutine eigen_simple_l_or_r_float
! ==============================================================
! A X = lambada B X
! Returns eigenvalues and left or right eigenvectors
!
subroutine eigen_general_l_or_r_float(a, b, w, v, lr)
    real, dimension(:, :), intent(in) :: a, b
    complex, allocatable, dimension(:), intent(inout) :: w
    complex, allocatable, dimension(:, :), intent(inout), optional :: v
    character(len=*), intent(in), optional :: lr
    character(len=12) :: left_or_right
    integer :: n
    complex, allocatable :: ac(:, :), bc(:, :), ww(:), vl(:, :), vr(:, :)
    call assert(size(a, 1) /= size(a, 2) .and. size(b, 1) /= size(b, 2), &
        ' <eigen_general_l_or_r> Error: both a and b must be square. ')
    if (present(lr)) then
        left_or_right = lr
    else
        left_or_right = 'right'
    end if
    n = size(a, 1)
    ac = cmplx(a)
    bc = cmplx(b)
    w = zeros(n)
    ww = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call ggev(ac, bc, w, ww, vl, vr)
    where (abs(ww) /= 0)
        w = w/ww
    end where
    if (present(v)) then
        select case (left_or_right)
            case ('right')
                v = vr
            case ('left')
                v = vl
        end select
    end if
end subroutine eigen_general_l_or_r_float
! ==============================================================
! A X = lambda X
! Returns eigenvalues and both left and right eigenvectors
!
subroutine eigen_simple_l_and_r_float(a, w, vl, vr)
    real, dimension(:, :), intent(in) :: a
    complex, allocatable, dimension(:), intent(inout) :: w
    complex, allocatable, dimension(:, :), intent(inout) :: vl, vr
    integer :: n
    complex, allocatable, dimension(:, :) :: ac
    call assert(size(a, 1) == size(a, 2), &
        ' <eigen_simple_l_and_r> Error: A must be square.')
    n = size(a, 1)
    ac = cmplx(a)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call geev(ac, w, vl, vr)
end subroutine eigen_simple_l_and_r_float
! ==============================================================
! A X = lambda B X
! Returns eigenvalues and both left and right eigenvectors
!
subroutine eigen_general_l_and_r_float(a, b, w, vl, vr)
    real, dimension(:, :), intent(in) :: a, b
    complex, allocatable, dimension(:), intent(inout) :: w
    complex, allocatable, dimension(:, :), intent(inout) :: vl, vr
    integer :: n
    complex, allocatable, dimension(:, :) :: ac, bc
    complex, allocatable, dimension(:) :: cc
    call assert(size(a, 1) == size(a, 2) .and. size(b, 1) == size(b, 2), &
        ' <eigen_general_l_and_r> Error: Both A and B must be square.')
    n = size(a, 1)
    ac = cmplx(a)
    bc = cmplx(b)
    cc = zeros(n)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call ggev(ac, bc, w, cc, vl, vr)
    where (abs(cc) /= 0)
        w = w/cc
    end where
end subroutine eigen_general_l_and_r_float
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
! ==============================================================
! A X = lambada X
! Returns eigenvalues and left or right eigenvectors
!
subroutine eigen_simple_l_or_r_double(a, w, v, lr)
    double precision, dimension(:, :), intent(in) :: a
    double complex, allocatable, dimension(:), intent(inout) :: w
    double complex, allocatable, dimension(:, :), intent(inout), optional :: v
    character(len=*), intent(in), optional :: lr
    character(len=12) :: left_or_right
    integer :: n
    double complex, allocatable :: ac(:, :), vl(:, :), vr(:, :)
    call assert(size(a, 1) /= size(a, 2), &
        ' <eigen_general_l_or_r> Error: a must be square. ')
    if (present(lr)) then
        left_or_right = lr
    else
        left_or_right = 'right'
    end if
    n = size(a, 1)
    ac = dcmplx(a)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call geev(ac, w, vl, vr)
    if (present(v)) then
        select case (left_or_right)
            case ('right')
                v = vr
            case ('left')
                v = vl
        end select
    end if
end subroutine eigen_simple_l_or_r_double
! ==============================================================
! A X = lambada B X
! Returns eigenvalues and left or right eigenvectors
!
subroutine eigen_general_l_or_r_double(a, b, w, v, lr)
    double precision, dimension(:, :), intent(in) :: a, b
    double complex, allocatable, dimension(:), intent(inout) :: w
    double complex, allocatable, dimension(:, :), intent(inout), optional :: v
    character(len=*), intent(in), optional :: lr
    character(len=12) :: left_or_right
    integer :: n
    double complex, allocatable :: ac(:, :), bc(:, :), ww(:), vl(:, :), vr(:, :)
    call assert(size(a, 1) /= size(a, 2) .and. size(b, 1) /= size(b, 2), &
        ' <eigen_general_l_or_r> Error: both a and b must be square. ')
    if (present(lr)) then
        left_or_right = lr
    else
        left_or_right = 'right'
    end if
    n = size(a, 1)
    ac = dcmplx(a)
    bc = dcmplx(b)
    w = zeros(n)
    ww = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call ggev(ac, bc, w, ww, vl, vr)
    where (abs(ww) /= 0)
        w = w/ww
    end where
    if (present(v)) then
        select case (left_or_right)
            case ('right')
                v = vr
            case ('left')
                v = vl
        end select
    end if
end subroutine eigen_general_l_or_r_double
! ==============================================================
! A X = lambda X
! Returns eigenvalues and both left and right eigenvectors
!
subroutine eigen_simple_l_and_r_double(a, w, vl, vr)
    double precision, dimension(:, :), intent(in) :: a
    double complex, allocatable, dimension(:), intent(inout) :: w
    double complex, allocatable, dimension(:, :), intent(inout) :: vl, vr
    integer :: n
    double complex, allocatable, dimension(:, :) :: ac
    call assert(size(a, 1) == size(a, 2), &
        ' <eigen_simple_l_and_r> Error: A must be square.')
    n = size(a, 1)
    ac = dcmplx(a)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call geev(ac, w, vl, vr)
end subroutine eigen_simple_l_and_r_double
! ==============================================================
! A X = lambda B X
! Returns eigenvalues and both left and right eigenvectors
!
subroutine eigen_general_l_and_r_double(a, b, w, vl, vr)
    double precision, dimension(:, :), intent(in) :: a, b
    double complex, allocatable, dimension(:), intent(inout) :: w
    double complex, allocatable, dimension(:, :), intent(inout) :: vl, vr
    integer :: n
    double complex, allocatable, dimension(:, :) :: ac, bc
    double complex, allocatable, dimension(:) :: cc
    call assert(size(a, 1) == size(a, 2) .and. size(b, 1) == size(b, 2), &
        ' <eigen_general_l_and_r> Error: Both A and B must be square.')
    n = size(a, 1)
    ac = dcmplx(a)
    bc = dcmplx(b)
    cc = zeros(n)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call ggev(ac, bc, w, cc, vl, vr)
    where (abs(cc) /= 0)
        w = w/cc
    end where
end subroutine eigen_general_l_and_r_double
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
! ==============================================================
! A X = lambada X
! Returns eigenvalues and left or right eigenvectors
!
subroutine eigen_simple_l_or_r_complex(a, w, v, lr)
    complex, dimension(:, :), intent(in) :: a
    complex, allocatable, dimension(:), intent(inout) :: w
    complex, allocatable, dimension(:, :), intent(inout), optional :: v
    character(len=*), intent(in), optional :: lr
    character(len=12) :: left_or_right
    integer :: n
    complex, allocatable :: ac(:, :), vl(:, :), vr(:, :)
    call assert(size(a, 1) /= size(a, 2), &
        ' <eigen_general_l_or_r> Error: a must be square. ')
    if (present(lr)) then
        left_or_right = lr
    else
        left_or_right = 'right'
    end if
    n = size(a, 1)
    ac = (a)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call geev(ac, w, vl, vr)
    if (present(v)) then
        select case (left_or_right)
            case ('right')
                v = vr
            case ('left')
                v = vl
        end select
    end if
end subroutine eigen_simple_l_or_r_complex
! ==============================================================
! A X = lambada B X
! Returns eigenvalues and left or right eigenvectors
!
subroutine eigen_general_l_or_r_complex(a, b, w, v, lr)
    complex, dimension(:, :), intent(in) :: a, b
    complex, allocatable, dimension(:), intent(inout) :: w
    complex, allocatable, dimension(:, :), intent(inout), optional :: v
    character(len=*), intent(in), optional :: lr
    character(len=12) :: left_or_right
    integer :: n
    complex, allocatable :: ac(:, :), bc(:, :), ww(:), vl(:, :), vr(:, :)
    call assert(size(a, 1) /= size(a, 2) .and. size(b, 1) /= size(b, 2), &
        ' <eigen_general_l_or_r> Error: both a and b must be square. ')
    if (present(lr)) then
        left_or_right = lr
    else
        left_or_right = 'right'
    end if
    n = size(a, 1)
    ac = (a)
    bc = (b)
    w = zeros(n)
    ww = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call ggev(ac, bc, w, ww, vl, vr)
    where (abs(ww) /= 0)
        w = w/ww
    end where
    if (present(v)) then
        select case (left_or_right)
            case ('right')
                v = vr
            case ('left')
                v = vl
        end select
    end if
end subroutine eigen_general_l_or_r_complex
! ==============================================================
! A X = lambda X
! Returns eigenvalues and both left and right eigenvectors
!
subroutine eigen_simple_l_and_r_complex(a, w, vl, vr)
    complex, dimension(:, :), intent(in) :: a
    complex, allocatable, dimension(:), intent(inout) :: w
    complex, allocatable, dimension(:, :), intent(inout) :: vl, vr
    integer :: n
    complex, allocatable, dimension(:, :) :: ac
    call assert(size(a, 1) == size(a, 2), &
        ' <eigen_simple_l_and_r> Error: A must be square.')
    n = size(a, 1)
    ac = (a)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call geev(ac, w, vl, vr)
end subroutine eigen_simple_l_and_r_complex
! ==============================================================
! A X = lambda B X
! Returns eigenvalues and both left and right eigenvectors
!
subroutine eigen_general_l_and_r_complex(a, b, w, vl, vr)
    complex, dimension(:, :), intent(in) :: a, b
    complex, allocatable, dimension(:), intent(inout) :: w
    complex, allocatable, dimension(:, :), intent(inout) :: vl, vr
    integer :: n
    complex, allocatable, dimension(:, :) :: ac, bc
    complex, allocatable, dimension(:) :: cc
    call assert(size(a, 1) == size(a, 2) .and. size(b, 1) == size(b, 2), &
        ' <eigen_general_l_and_r> Error: Both A and B must be square.')
    n = size(a, 1)
    ac = (a)
    bc = (b)
    cc = zeros(n)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call ggev(ac, bc, w, cc, vl, vr)
    where (abs(cc) /= 0)
        w = w/cc
    end where
end subroutine eigen_general_l_and_r_complex
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
! ==============================================================
! A X = lambada X
! Returns eigenvalues and left or right eigenvectors
!
subroutine eigen_simple_l_or_r_dcomplex(a, w, v, lr)
    double complex, dimension(:, :), intent(in) :: a
    double complex, allocatable, dimension(:), intent(inout) :: w
    double complex, allocatable, dimension(:, :), intent(inout), optional :: v
    character(len=*), intent(in), optional :: lr
    character(len=12) :: left_or_right
    integer :: n
    double complex, allocatable :: ac(:, :), vl(:, :), vr(:, :)
    call assert(size(a, 1) /= size(a, 2), &
        ' <eigen_general_l_or_r> Error: a must be square. ')
    if (present(lr)) then
        left_or_right = lr
    else
        left_or_right = 'right'
    end if
    n = size(a, 1)
    ac = (a)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call geev(ac, w, vl, vr)
    if (present(v)) then
        select case (left_or_right)
            case ('right')
                v = vr
            case ('left')
                v = vl
        end select
    end if
end subroutine eigen_simple_l_or_r_dcomplex
! ==============================================================
! A X = lambada B X
! Returns eigenvalues and left or right eigenvectors
!
subroutine eigen_general_l_or_r_dcomplex(a, b, w, v, lr)
    double complex, dimension(:, :), intent(in) :: a, b
    double complex, allocatable, dimension(:), intent(inout) :: w
    double complex, allocatable, dimension(:, :), intent(inout), optional :: v
    character(len=*), intent(in), optional :: lr
    character(len=12) :: left_or_right
    integer :: n
    double complex, allocatable :: ac(:, :), bc(:, :), ww(:), vl(:, :), vr(:, :)
    call assert(size(a, 1) /= size(a, 2) .and. size(b, 1) /= size(b, 2), &
        ' <eigen_general_l_or_r> Error: both a and b must be square. ')
    if (present(lr)) then
        left_or_right = lr
    else
        left_or_right = 'right'
    end if
    n = size(a, 1)
    ac = (a)
    bc = (b)
    w = zeros(n)
    ww = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call ggev(ac, bc, w, ww, vl, vr)
    where (abs(ww) /= 0)
        w = w/ww
    end where
    if (present(v)) then
        select case (left_or_right)
            case ('right')
                v = vr
            case ('left')
                v = vl
        end select
    end if
end subroutine eigen_general_l_or_r_dcomplex
! ==============================================================
! A X = lambda X
! Returns eigenvalues and both left and right eigenvectors
!
subroutine eigen_simple_l_and_r_dcomplex(a, w, vl, vr)
    double complex, dimension(:, :), intent(in) :: a
    double complex, allocatable, dimension(:), intent(inout) :: w
    double complex, allocatable, dimension(:, :), intent(inout) :: vl, vr
    integer :: n
    double complex, allocatable, dimension(:, :) :: ac
    call assert(size(a, 1) == size(a, 2), &
        ' <eigen_simple_l_and_r> Error: A must be square.')
    n = size(a, 1)
    ac = (a)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call geev(ac, w, vl, vr)
end subroutine eigen_simple_l_and_r_dcomplex
! ==============================================================
! A X = lambda B X
! Returns eigenvalues and both left and right eigenvectors
!
subroutine eigen_general_l_and_r_dcomplex(a, b, w, vl, vr)
    double complex, dimension(:, :), intent(in) :: a, b
    double complex, allocatable, dimension(:), intent(inout) :: w
    double complex, allocatable, dimension(:, :), intent(inout) :: vl, vr
    integer :: n
    double complex, allocatable, dimension(:, :) :: ac, bc
    double complex, allocatable, dimension(:) :: cc
    call assert(size(a, 1) == size(a, 2) .and. size(b, 1) == size(b, 2), &
        ' <eigen_general_l_and_r> Error: Both A and B must be square.')
    n = size(a, 1)
    ac = (a)
    bc = (b)
    cc = zeros(n)
    w = zeros(n)
    vl = zeros(n, n)
    vr = zeros(n, n)
    call ggev(ac, bc, w, cc, vl, vr)
    where (abs(cc) /= 0)
        w = w/cc
    end where
end subroutine eigen_general_l_and_r_dcomplex
end module libflit_linear_algebra
