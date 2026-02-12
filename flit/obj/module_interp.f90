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
module libflit_interp
    use libflit_specialfunc
    use libflit_constants
    use libflit_array
    use libflit_array_operation
    use libflit_unique
    use libflit_linear_algebra
    use libflit_error
    use libflit_string
    use libflit_utility
    use libflit_calculus
    use libflit_taper
    implicit none
    ! External interfaces
    interface
        subroutine interp_cspline_1d_float(n, x, y, nn, xx, yy) bind(c, name='cspline1d')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, nn
            real, dimension(*), intent(in) :: x, y, xx
            real, dimension(*), intent(out) :: yy
        end subroutine interp_cspline_1d_float
        subroutine interp_pchip_1d_float(n, x, y, nn, xx, yy) bind(c, name='pchip1d')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, nn
            real, dimension(*), intent(in) :: x, y, xx
            real, dimension(*), intent(out) :: yy
        end subroutine interp_pchip_1d_float
        subroutine interp_quintic_1d_float(n, x, y, nn, xx, yy) bind(c, name='quintic1d')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, nn
            real, dimension(*), intent(in) :: x, y, xx
            real, dimension(*), intent(out) :: yy
        end subroutine interp_quintic_1d_float
        subroutine interp_cspline_1d_double(n, x, y, nn, xx, yy) bind(c, name='cspline1d_double')
            use iso_c_binding, only: c_int, c_double
            integer(kind=c_int), value :: n, nn
            double precision, dimension(*), intent(in) :: x, y, xx
            double precision, dimension(*), intent(out) :: yy
        end subroutine interp_cspline_1d_double
        subroutine interp_pchip_1d_double(n, x, y, nn, xx, yy) bind(c, name='pchip1d_double')
            use iso_c_binding, only: c_int, c_double
            integer(kind=c_int), value :: n, nn
            double precision, dimension(*), intent(in) :: x, y, xx
            double precision, dimension(*), intent(out) :: yy
        end subroutine interp_pchip_1d_double
        subroutine interp_quintic_1d_double(n, x, y, nn, xx, yy) bind(c, name='quintic1d_double')
            use iso_c_binding, only: c_int, c_double
            integer(kind=c_int), value :: n, nn
            double precision, dimension(*), intent(in) :: x, y, xx
            double precision, dimension(*), intent(out) :: yy
        end subroutine interp_quintic_1d_double
        subroutine interp_mba_1d_float(n, x, y, nn, xx, yy) bind(c, name='mba1')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, nn
            real(kind=c_float), dimension(*), intent(in) :: x, y, xx
            real(kind=c_float), dimension(*), intent(out) :: yy
        end subroutine interp_mba_1d_float
        subroutine interp_mba_2d_float(n, x, y, z, nn, xx, yy, zz) bind(c, name='mba2')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, nn
            real(kind=c_float), dimension(*), intent(in) :: x, y, z, xx, yy
            real(kind=c_float), dimension(*), intent(out) :: zz
        end subroutine interp_mba_2d_float
        subroutine interp_mba_3d_float(n, x, y, z, v, nn, xx, yy, zz, vv) bind(c, name='mba3')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, nn
            real(kind=c_float), dimension(*), intent(in) :: x, y, z, v, xx, yy, zz
            real(kind=c_float), dimension(*), intent(out) :: vv
        end subroutine interp_mba_3d_float
        subroutine interp_mba_1d_double(n, x, y, nn, xx, yy) bind(c, name='mba1_double')
            use iso_c_binding, only: c_int, c_double
            integer(kind=c_int), value :: n, nn
            real(kind=c_double), dimension(*), intent(in) :: x, y, xx
            real(kind=c_double), dimension(*), intent(out) :: yy
        end subroutine interp_mba_1d_double
        subroutine interp_mba_2d_double(n, x, y, z, nn, xx, yy, zz) bind(c, name='mba2_double')
            use iso_c_binding, only: c_int, c_double
            integer(kind=c_int), value :: n, nn
            real(kind=c_double), dimension(*), intent(in) :: x, y, z, xx, yy
            real(kind=c_double), dimension(*), intent(out) :: zz
        end subroutine interp_mba_2d_double
        subroutine interp_mba_3d_double(n, x, y, z, v, nn, xx, yy, zz, vv) bind(c, name='mba3_double')
            use iso_c_binding, only: c_int, c_double
            integer(kind=c_int), value :: n, nn
            real(kind=c_double), dimension(*), intent(in) :: x, y, z, v, xx, yy, zz
            real(kind=c_double), dimension(*), intent(out) :: vv
        end subroutine interp_mba_3d_double
    end interface
    ! Internal interfaces
    interface interp
        module procedure :: reg_to_reg_interp_1d_float
        module procedure :: reg_to_reg_interp_1d_double
        module procedure :: reg_to_reg_interp_2d_float
        module procedure :: reg_to_reg_interp_2d_double
        module procedure :: reg_to_reg_interp_3d_float
        module procedure :: reg_to_reg_interp_3d_double
    end interface interp
    interface resample
        module procedure :: resample_1d_float
        module procedure :: resample_1d_double
        module procedure :: resample_2d_float
        module procedure :: resample_2d_double
        module procedure :: resample_3d_float
        module procedure :: resample_3d_double
    end interface resample
    interface interp_to
        module procedure :: interp_to_1d_float
        module procedure :: interp_to_1d_double
        module procedure :: interp_to_2d_float
        module procedure :: interp_to_2d_double
        module procedure :: interp_to_3d_float
        module procedure :: interp_to_3d_double
    end interface interp_to
    interface interp_like
        module procedure :: interp_like_1d_float
        module procedure :: interp_like_1d_double
        module procedure :: interp_like_2d_float
        module procedure :: interp_like_2d_double
        module procedure :: interp_like_3d_float
        module procedure :: interp_like_3d_double
    end interface interp_like
    interface ginterp
        module procedure :: irreg_to_irreg_interp_1d_float
        module procedure :: irreg_to_irreg_interp_1d_double
        module procedure :: irreg_to_irreg_interp_2d_float
        module procedure :: irreg_to_irreg_interp_2d_double
        module procedure :: irreg_to_irreg_interp_3d_float
        module procedure :: irreg_to_irreg_interp_3d_double
        module procedure :: irreg_to_reg_interp_1d_float
        module procedure :: irreg_to_reg_interp_1d_double
        module procedure :: irreg_to_reg_interp_2d_float
        module procedure :: irreg_to_reg_interp_2d_double
        module procedure :: irreg_to_reg_interp_3d_float
        module procedure :: irreg_to_reg_interp_3d_double
    end interface ginterp
    interface inpaint
        module procedure :: inpaint_1d_float
        module procedure :: inpaint_1d_double
        module procedure :: inpaint_2d_float
        module procedure :: inpaint_2d_double
        module procedure :: inpaint_3d_float
        module procedure :: inpaint_3d_double
    end interface inpaint
    interface meshgrid
        module procedure :: meshgrid_float
        module procedure :: meshgrid_double
        module procedure :: meshgrid_1d_float
        module procedure :: meshgrid_1d_double
        module procedure :: meshgrid_2d_float
        module procedure :: meshgrid_2d_double
        module procedure :: meshgrid_3d_float
        module procedure :: meshgrid_3d_double
    end interface meshgrid
    interface point_interp_linear
        module procedure :: point_interp_linear_1d_float
        module procedure :: point_interp_linear_2d_float
        module procedure :: point_interp_linear_3d_float
        module procedure :: point_interp_linear_1d_double
        module procedure :: point_interp_linear_2d_double
        module procedure :: point_interp_linear_3d_double
    end interface point_interp_linear
    interface point_interp_barycentric
        module procedure :: point_interp_barycentric_2d_float
        module procedure :: point_interp_barycentric_3d_float
        module procedure :: point_interp_barycentric_2d_double
        module procedure :: point_interp_barycentric_3d_double
    end interface point_interp_barycentric
    private
    public :: interp
    public :: resample
    public :: interp_to
    public :: interp_like
    public :: ginterp
    public :: meshgrid
    public :: inpaint
    public :: point_interp_linear
    public :: point_interp_barycentric
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
! external
!
!> Nearest neighbour interpolation for 1D data
!
subroutine interp_nearest_1d_float(n, x, y, nn, xx, yy)
    integer, intent(in) :: n, nn
    real, dimension(:), intent(in) :: x, xx
    real, dimension(:), intent(in) :: y
    real, dimension(:), intent(out) :: yy
    integer :: i
    call assert(size(x) == size(y), &
        ' <interp_nearest_1d> Error: size(x) /= size(y). ')
    i = n
    ! A brute-foce nearest neighbour implementation; could be slow for large data
    do i = 1, nn
        yy(i) = y(minloc(abs(xx(i) - x), dim=1))
    end do
end subroutine interp_nearest_1d_float
subroutine interp_nearest_2d_float(n, x, y, z, nn, xx, yy, zz)
    integer, intent(in) :: n, nn
    real, dimension(:), intent(in) :: x, y, xx, yy
    real, dimension(:), intent(in) :: z
    real, dimension(:), intent(out) :: zz
    integer :: i
    call assert(size(x) == size(y) .and. size(y) == size(z), &
        ' <interp_nearest_3d> Error: Sizes of x, y, z are inconsistent. ')
    i = n
    ! A brute-foce nearest neighbour implementation; could be slow for large data
    !$omp parallel do private(i)
    do i = 1, nn
        zz(i) = z(minloc((xx(i) - x)**2 + (yy(i) - y)**2, dim=1))
    end do
    !$omp end parallel do
end subroutine interp_nearest_2d_float
subroutine interp_nearest_3d_float(n, x, y, z, v, nn, xx, yy, zz, vv)
    integer, intent(in) :: n, nn
    real, dimension(:), intent(in) :: x, y, z, xx, yy, zz
    real, dimension(:), intent(in) :: v
    real, dimension(:), intent(out) :: vv
    integer :: i
    call assert(size(x) == size(y) .and. size(y) == size(z) .and. size(z) == size(v), &
        ' <interp_nearest_3d> Error: Sizes of x, y, z, v are inconsistent. ')
    i = n
    ! A brute-foce nearest neighbour implementation; could be slow for large data
    !$omp parallel do private(i)
    do i = 1, nn
        vv(i) = v(minloc((xx(i) - x)**2 + (yy(i) - y)**2 + (zz(i) - z)**2, dim=1))
    end do
    !$omp end parallel do
end subroutine interp_nearest_3d_float
!
!> Linear interpolation for 1D data
!
subroutine interp_linear_1d_float(n, x, y, nn, xx, yy)
    integer, intent(in) :: n, nn
    real, dimension(:), intent(in) :: x, xx
    real, dimension(:), intent(in) :: y
    real, dimension(:), intent(out) :: yy
    integer :: i, k
    real :: t
    call assert(size(x) == size(y), ' <interp_linear_1d> Error: size(x) /= size(y). ')
    yy = 0.0d0
    if (n == 1) then
        yy(1:nn) = y(1)
        return
    end if
    do i = 1, nn
        if (xx(i) <= x(1)) then
            t = (xx(i) - x(1))/(x(2) - x(1))
            yy(i) = (1.0d0 - t)*y(1) + t*y(2)
        else if (x(n) <= xx(i)) then
            t = (xx(i) - x(n - 1))/(x(n) - x(n - 1))
            yy(i) = (1.0 - t)*y(n - 1) + t*y(n)
        else
            do k = 2, n
                if (xx(i) >= x(k - 1) .and. xx(i) < x(k)) then
                    t = (xx(i) - x(k - 1))/(x(k) - x(k - 1))
                    yy(i) = (1.0d0 - t)*y(k - 1) + t*y(k)
                    exit
                end if
            end do
        end if
    end do
end subroutine interp_linear_1d_float
!
!> Cubic spline interpolation for 1D data; contains three methods:
!> - c2: cubic spline, a c2-continuous spline
!> - hermite: Hermite spline, where each piece is a third-degree polynomial
!> specified in Hermite form, that is, by its values and
!> first derivatives at the end points of the corresponding domain interval.
!> - monotonic: cubic monotonic Hermite spline, a.k.a. PCHIP,
!> is a variant of cubic interpolation
!> that preserves monotonicity of the data set being interpolated.
!> Here I implemented the algorithm described in
!> https://en.wikipedia.org/wiki/Monotone_cubic_interpolation,
!> with modifications (slope selection and boundary points handling).
!
subroutine interp_cubic_spline_1d_float(n, x, y, nn, xx, yy, method)
    integer, intent(in) :: n, nn
    real, dimension(:), intent(in) :: x, xx
    real, dimension(:), intent(in) :: y
    character(len=*), intent(in) :: method
    real, dimension(:), intent(out) :: yy
    real, allocatable, dimension(:) :: a, b, c, d, h, r
    real, allocatable, dimension(:, :) :: m
    integer :: i, j
    real :: dist
    real :: f1, f2
    real, allocatable, dimension(:) :: delta, mm, alpha, beta, tau
    real :: h00, h01, h10, h11
    real :: z(1:3)
    call assert(size(x) == size(y), ' <interp_cubic_spline_1d> Error: size(x) /= size(y). ')
    call assert(size(x) >= 3, ' <interp_cubic_spline_1d> Error: size(x) must >= 3. ')
    a = y
    b = zeros(n)
    d = zeros(n)
    h = zeros(n)
    do i = 1, n - 1
        h(i) = x(i + 1) - x(i)
    end do
    h(n) = h(n - 1)
    select case (method)
        case ('c2')
            ! Cubic spline
            m = zeros(3, n - 2)
            r = zeros(n - 2)
            ! Super-diagonal elements
            do i = 2, n - 2
                m(1, i) = h(i)/3.0
            end do
            ! Diagonal elements
            do i = 2, n - 1
                m(2, i - 1) = (h(i - 1) + h(i))*2.0/3.0
            end do
            ! Sub-diagonal elements
            do i = 2, n - 2
                m(3, i - 1) = h(i - 1)/3.0
            end do
            ! RHS
            do i = 2, n - 1
                r(i - 1) = (y(i + 1) - y(i))/h(i) - (y(i) - y(i - 1))/h(i - 1)
            end do
            c = [real(0.0/2.0), solve_band(m, 1, 1, r), real(0.0/2.0)]
            do i = 1, n - 1
                d(i) = (c(i + 1) - c(i))/(3.0*h(i))
                b(i) = (y(i + 1) - y(i))/h(i) - (2*c(i) + c(i + 1))*h(i)/3.0
            end do
            d(n) = 0.0
            b(n) = 3*d(n - 1)*h(n - 1)**2 + 2*c(n - 1)*h(n - 1) + b(n - 1)
            do i = 1, nn
                if (xx(i) < x(1)) then
                    dist = xx(i) - x(1)
                    yy(i) = a(1) + b(1)*dist + c(1)*dist**2
                else if (xx(i) >= x(n)) then
                    dist = xx(i) - x(n)
                    yy(i) = a(n) + b(n)*dist + c(n)*dist**2
                else
                    do j = 1, n - 1
                        if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
                            dist = xx(i) - x(j)
                            yy(i) = a(j) + b(j)*dist + c(j)*dist**2 + d(j)*dist**3
                            exit
                        end if
                    end do
                end if
            end do
        case ('hermite')
            ! Cubic Hermite spline
            c = zeros(n)
            do i = 2, n - 1
                f1 = (y(i) - y(i - 1))/h(i - 1)
                f2 = (y(i + 1) - y(i))/h(i)
                b(i) = h(i - 1)/(h(i - 1) + h(i))*f2 + h(i)/(h(i) + h(i - 1))*f1
            end do
            b(1) = 0.5*(-b(2) + 3*(y(2) - y(1))/h(1))
            b(n) = 0.5*(-b(n - 1) + 3*(y(n) - y(n - 1))/h(n - 1))
            do i = 1, n - 1
                c(i) = -(2*b(i) + b(i + 1))/h(i) + 3*(a(i + 1) - a(i))/h(i)**2
                d(i) = -2*c(i)/(3*h(i)) + (b(i + 1) - b(i))/(3*h(i)**2)
            end do
            do i = 1, nn
                if (xx(i) < x(1)) then
                    dist = xx(i) - x(1)
                    yy(i) = a(1) + b(1)*dist + c(1)*dist**2
                else if (xx(i) >= x(n)) then
                    dist = xx(i) - x(n)
                    yy(i) = a(n) + b(n)*dist + c(n)*dist**2
                else
                    do j = 1, n - 1
                        if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
                            dist = xx(i) - x(j)
                            yy(i) = a(j) + b(j)*dist + c(j)*dist**2 + d(j)*dist**3
                            exit
                        end if
                    end do
                end if
            end do
        case ('monotonic')
            ! PCHIP spline
            ! For handling boundary points
            c = zeros(n)
            do i = 2, n - 1
                f1 = (y(i) - y(i - 1))/h(i - 1)
                f2 = (y(i + 1) - y(i))/h(i)
                b(i) = h(i - 1)/(h(i - 1) + h(i))*f2 + h(i)/(h(i) + h(i - 1))*f1
            end do
            b(1) = 0.5*(-b(2) + 3*(y(2) - y(1))/h(1))
            b(n) = 0.5*(-b(n - 1) + 3*(y(n) - y(n - 1))/h(n - 1))
            do i = n - 1, n - 1
                c(i) = -(2*b(i) + b(i + 1))/h(i) + 3*(a(i + 1) - a(i))/h(i)**2
            end do
            c(n) = c(n - 1)
            ! Monotonicity-perserving cubic splines
            delta = zeros(n)
            do i = 1, n - 1
                delta(i) = (y(i + 1) - y(i))/(x(i + 1) - x(i))
            end do
            delta(n) = delta(n - 1)
            mm = zeros(n)
            do i = 2, n - 1
                ! Choosing the min slope in L1 sense
                z = [0.5*(delta(i - 1) + delta(i)), delta(i - 1), delta(i)]
                mm(i) = z(minloc(abs(z), dim=1))
                if (delta(i - 1)*delta(i) < 0) then
                    mm(i) = 0
                end if
            end do
            mm(1) = delta(1)
            mm(n) = delta(n - 1)
            do i = 1, n - 1
                if (delta(i) == 0) then
                    mm(i:i + 1) = 0
                end if
            end do
            alpha = zeros(n)
            beta = zeros(n)
            do i = 1, n - 1
                if (mm(i) /= 0) then
                    alpha(i) = mm(i)/delta(i)
                    beta(i) = mm(i + 1)/delta(i)
                end if
            end do
            do i = 1, n - 1
                if (alpha(i) < 0) then
                    mm(i) = 0
                end if
                if (beta(i) < 0) then
                    mm(i + 1) = 0
                end if
            end do
            tau = zeros(n)
            do i = 1, n - 1
                if (alpha(i)**2 + beta(i)**2 > 9) then
                    tau(i) = 9/sqrt(alpha(i)**2 + beta(i)**2)
                end if
            end do
            do i = 1, n - 1
                if (alpha(i)**2 + beta(i)**2 > 9 .and. mm(i) /= 0) then
                    mm(i) = tau(i)*alpha(i)*delta(i)
                    mm(i + 1) = tau(i)*beta(i)*delta(i)
                end if
            end do
            yy = 0
            do i = 1, nn
                if (xx(i) < x(1)) then
                    dist = xx(i) - x(1)
                    yy(i) = a(1) + b(1)*dist + c(1)*dist**2
                else if (xx(i) >= x(n)) then
                    dist = xx(i) - x(n)
                    yy(i) = a(n) + b(n)*dist + c(n)*dist**2
                else
                    do j = 1, n - 1
                        if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
                            dist = (xx(i) - x(j))/h(j)
                            h00 = (1 + 2*dist)*(1 - dist)**2
                            h10 = dist*(1 - dist)**2
                            h01 = dist**2*(3 - 2*dist)
                            h11 = dist**2*(dist - 1)
                            yy(i) = h00*y(j) + h10*h(j)*mm(j) + h01*y(j + 1) + h11*h(j)*mm(j + 1)
                            exit
                        end if
                    end do
                end if
            end do
    end select
    ! yy = 0
    ! select case (order)
    ! case (0)
    ! ! The function itself
    ! do i = 1, nn
    ! if (xx(i) < x(1)) then
    ! dist = xx(i) - x(1)
    ! yy(i) = a(1) + b(1)*dist + c(1)*dist**2
    !
    ! else if (xx(i) >= x(n)) then
    ! dist = xx(i) - x(n)
    ! yy(i) = a(n) + b(n)*dist + c(n)*dist**2
    ! else
    ! do j = 1, n - 1
    ! if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
    ! dist = xx(i) - x(j)
    ! yy(i) = a(j) + b(j)*dist + c(j)*dist**2 + d(j)*dist**3
    ! exit
    ! end if
    ! end do
    ! end if
    ! end do
    ! case (1)
    ! ! First-order derivative
    ! do i = 1, nn
    ! if (xx(i) < x(1)) then
    ! dist = xx(i) - x(1)
    ! yy(i) = b(1) + 2*c(1)*dist
    ! else if (xx(i) >= x(n)) then
    ! dist = xx(i) - x(n)
    ! yy(i) = b(n) + 2*c(n)*dist
    ! else
    ! do j = 1, n - 1
    ! if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
    ! dist = xx(i) - x(j)
    ! yy(i) = b(j) + 2*c(j)*dist + 3*d(j)*dist**2
    ! exit
    ! end if
    ! end do
    ! end if
    ! end do
    ! case (2)
    ! ! Second-order derivative
    ! do i = 1, nn
    ! if (xx(i) < x(1)) then
    ! yy(i) = 2*c(1)
    ! else if (xx(i) >= x(n)) then
    ! yy(i) = 2*c(n)
    ! else
    ! do j = 1, n - 1
    ! if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
    ! dist = xx(i) - x(j)
    ! yy(i) = 2*c(j) + 6*d(j)*dist
    ! exit
    ! end if
    ! end do
    ! end if
    ! end do
    ! end select
end subroutine interp_cubic_spline_1d_float
!
!> Biharmonic interpolation; could be very slow and also memory-intensive
!> because needs to solve a large linear system;
!> https://doi.org/10.1029/GL014i002p00139
!
subroutine interp_biharmonic_1d_float(n_, x, y, nn, xx, yy)
    integer, intent(in) :: n_, nn
    real, dimension(:), intent(in) :: x, xx
    real, dimension(:), intent(in) :: y
    real, dimension(:), intent(out) :: yy
    integer :: i, j, n
    real, allocatable, dimension(:, :) :: dist
    real, allocatable, dimension(:) :: w
    real, allocatable, dimension(:) :: xd
    real, allocatable, dimension(:) :: yd
    real, allocatable, dimension(:, :) :: xyd
    call assert(size(x) == size(y), ' <interp_biharmonic_1d> Error: size(x) /= size(y). ')
    ! First find unique pairs of original scatter points
    ! Otherwise the linear system is not solvable
    xyd = unique(reshape([real(x), y], [n_, 2]), cols=[1])
    n = size(xyd, 1)
    xd = xyd(:, 1)
    yd = xyd(:, 2)
    ! Compute distance and Green's function
    dist = zeros(n, n)
    do j = 1, n
        do i = 1, n
            if (i /= j) then
                dist(i, j) = abs(xd(i) - xd(j))
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            else
                dist(i, j) = 0.0
            end if
        end do
    end do
    ! Compute weights
    w = solve(dist, yd)
    ! Comptue interpolation values
    dist = zeros(nn, n)
    do j = 1, n
        do i = 1, nn
            dist(i, j) = abs(xx(i) - xd(j))
            if (abs(dist(i, j)) /= 0) then
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            end if
        end do
    end do
    yy = matx(dist, w)
end subroutine interp_biharmonic_1d_float
subroutine interp_biharmonic_2d_float(n_, x, y, z, nn, xx, yy, zz)
    integer, intent(in) :: n_, nn
    real, dimension(:), intent(in) :: x, y, xx, yy
    real, dimension(:), intent(in) :: z
    real, dimension(:), intent(out) :: zz
    integer :: i, j, n
    real, allocatable, dimension(:, :) :: dist
    real, allocatable, dimension(:) :: w
    real, allocatable, dimension(:) :: xd, yd
    real, allocatable, dimension(:) :: zd
    real, allocatable, dimension(:, :) :: xyzd
    call assert(size(x) == size(y) .and. size(y) == size(z), &
        ' <interp_biharmonic_2d> Error: Sizes of x, y, and z are inconsistent. ')
    ! First find unique pairs of original scatter points
    ! Otherwise the linear system is not solvable
    xyzd = unique(reshape([real(x), real(y), z], [n_, 3]), cols=[1, 2])
    n = size(xyzd, 1)
    xd = xyzd(:, 1)
    yd = xyzd(:, 2)
    zd = xyzd(:, 3)
    dist = zeros(n, n)
    ! Compute distance and Green's function
    !$omp parallel do private(i, j)
    do j = 1, n
        do i = 1, n
            dist(i, j) = sqrt((xd(i) - xd(j))**2 + (yd(i) - yd(j))**2)
            if (i /= j) then
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            else
                dist(i, j) = 0.0
            end if
        end do
    end do
    !$omp end parallel do
    ! Compute weights
    w = solve(dist, zd)
    ! Comptue interpolation values
    dist = zeros(nn, n)
    !$omp parallel do private(i, j)
    do j = 1, n
        do i = 1, nn
            dist(i, j) = sqrt((xx(i) - xd(j))**2 + (yy(i) - yd(j))**2)
            if (abs(dist(i, j)) /= 0) then
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            end if
        end do
    end do
    !$omp end parallel do
    zz = matx(dist, w)
end subroutine interp_biharmonic_2d_float
subroutine interp_biharmonic_3d_float(n_, x, y, z, v, nn, xx, yy, zz, vv)
    integer, intent(in) :: n_, nn
    real, dimension(:), intent(in) :: x, y, z, xx, yy, zz
    real, dimension(:), intent(in) :: v
    real, dimension(:), intent(out) :: vv
    integer :: i, j, n
    real, allocatable, dimension(:, :) :: dist
    real, allocatable, dimension(:) :: w
    real, allocatable, dimension(:) :: xd, yd, zd
    real, allocatable, dimension(:) :: vd
    real, allocatable, dimension(:, :) :: xyzvd
    call assert(size(x) == size(y) .and. size(y) == size(z) .and. size(z) == size(v), &
        ' <interp_biharmonic_3d> Error: Sizes of x, y, z, and v are inconsistent. ')
    ! First find unique pairs of original scatter points
    ! Otherwise the linear system is not solvable
    xyzvd = unique(reshape([real(x), real(y), real(z), v], [n_, 4]), cols=[1, 2, 3])
    n = size(xyzvd, 1)
    xd = xyzvd(:, 1)
    yd = xyzvd(:, 2)
    zd = xyzvd(:, 3)
    vd = xyzvd(:, 4)
    ! Compute distance and Green's function
    dist = zeros(n, n)
    !$omp parallel do private(i, j)
    do j = 1, n
        do i = 1, n
            dist(i, j) = sqrt((xd(i) - xd(j))**2 + (yd(i) - yd(j))**2 &
                + (zd(i) - zd(j))**2)
            if (i /= j) then
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            else
                dist(i, j) = 0.0
            end if
        end do
    end do
    !$omp end parallel do
    ! Compute weights
    w = solve(dist, vd)
    ! Comptue interpolation values
    dist = zeros(nn, n)
    !$omp parallel do private(i, j)
    do j = 1, n
        do i = 1, nn
            dist(i, j) = sqrt((xx(i) - xd(j))**2 + (yy(i) - yd(j))**2 &
                + (zz(i) - zd(j))**2)
            if (abs(dist(i, j)) /= 0) then
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            end if
        end do
    end do
    !$omp end parallel do
    vv = matx(dist, w)
end subroutine interp_biharmonic_3d_float
!
!> Windowed sinc interpolation for 1D data
!
subroutine interp_sinc_1d_float(n, x, y, nn, xx, yy)
    integer, intent(in) :: n, nn
    real, dimension(:), intent(in) :: x, xx
    real, dimension(:), intent(in) :: y
    real, dimension(:), intent(out) :: yy
    integer :: i, h, nk1, nk2
    real :: d, dist
    real :: ya, yb
    real, allocatable, dimension(:) :: ksinc, xd
    real, allocatable, dimension(:) :: yd
    integer :: nkw
    real :: b0
    nkw = 8
    b0 = 3.0d0
    xd = x
    yd = y
    d = x(2) - x(1)
    if (xx(1) < x(1)) then
        nk1 = nkw + ceiling(abs(xx(1) - x(1))/d)
    else
        nk1 = nkw
    end if
    if (xx(nn) > x(n)) then
        nk2 = nkw + ceiling(abs(xx(nn) - x(n))/d)
    else
        nk2 = nkw
    end if
    call pad_array(xd, [nk1, nk2])
    call pad_array(yd, [nk1, nk2])
    ya = y(2) - y(1)
    yb = y(n) - y(n - 1)
    do i = 1, nk1
        xd(1 - i) = xd(1) - i*d
        yd(1 - i) = yd(1) - i*ya
    end do
    do i = 1, nk2
        xd(n + i) = xd(n) + i*d
        yd(n + i) = yd(n) + i*yb
    end do
    do i = 1, nn
        h = nint((xx(i) - xd(1))/d) + 1
        dist = (xx(i) - xd(h))/d
        ksinc = dist - regspace(-nkw, 1, nkw)
        ksinc = sinc(ksinc*const_pi)*window_function(ksinc/nkw + 0.5d0, method='kaiser', alpha=b0 + 0.0d0)
        yy(i) = sum(ksinc*yd(h - nkw:h + nkw))
    end do
end subroutine interp_sinc_1d_float
!==================================================================================================
!
!> Interpolate regularly sampled data to regularly sampled data
!
function reg_to_reg_interp_1d_float(f, n1, d1, o1, nn1, dd1, oo1, method) result(ff)
    real, dimension(:) :: f
    integer :: n1, nn1
    real :: d1, dd1, o1, oo1
    character(len=*), optional :: method
    real, allocatable, dimension(:) :: ff
    real, allocatable, dimension(:) :: x, xx
    character(len=24) :: interp_method
    integer :: i
    call assert(d1 > 0, ' <reg_to_reg_interp_1d> Error: The original interval must be > 0')
    call assert(dd1 > 0, ' <reg_to_reg_interp_1d> Error: The resampling interval must be > 0')
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'linear'
    end if
    allocate (ff(1:nn1))
    if (n1 == nn1 .and. d1 == dd1 .and. o1 == oo1) then
        ! Trivial case
        ff = f
    else
        ! Do interpolation
        ! Point coordinates before and after interpolation
        allocate (x(1:n1))
        allocate (xx(1:nn1))
        do i = 1, n1
            x(i) = o1 + (i - 1)*d1
        end do
        do i = 1, nn1
            xx(i) = oo1 + (i - 1)*dd1
        end do
        select case (interp_method)
            case ('nearest')
                call interp_nearest_1d_float(n1, x, f, nn1, xx, ff)
            case ('linear')
                call interp_linear_1d_float(n1, x, f, nn1, xx, ff)
            case ('sinc')
                call interp_sinc_1d_float(n1, x, f, nn1, xx, ff)
            case ('cubic')
                call interp_cspline_1d_float(n1, x, f, nn1, xx, ff)
            case ('pchip')
                call interp_pchip_1d_float(n1, x, f, nn1, xx, ff)
            case ('quintic')
                call interp_quintic_1d_float(n1, x, f, nn1, xx, ff)
            case ('mba')
                call interp_mba_1d_float(n1, x, f, nn1, xx, ff)
            case ('biharmonic')
                call interp_biharmonic_1d_float(n1, x, f, nn1, xx, ff)
            case ('cubic_spline')
                call interp_cubic_spline_1d_float(n1, x, f, nn1, xx, ff, method='c2')
            case ('hermite_spline')
                call interp_cubic_spline_1d_float(n1, x, f, nn1, xx, ff, method='hermite')
            case ('monotonic_spline')
                call interp_cubic_spline_1d_float(n1, x, f, nn1, xx, ff, method='monotonic')
            case default
                call interp_linear_1d_float(n1, x, f, nn1, xx, ff)
        end select
    end if
end function reg_to_reg_interp_1d_float
function reg_to_reg_interp_2d_float(f, n, d, o, nn, dd, oo, method) result(ff)
    real, dimension(:, :) :: f
    integer, dimension(:) :: n, nn
    real, dimension(1:2) :: d, dd, o, oo
    character(len=*), dimension(1:2), optional :: method
    real, allocatable, dimension(:, :) :: ff
    character(len=24), dimension(1:2) :: interp_method
    integer :: i, j
    real, allocatable, dimension(:, :) :: w
    call assert(all(d > 0), ' <reg_to_reg_interp_2d_> Error: All original intervals must be > 0')
    call assert(all(dd > 0), ' <reg_to_reg_interp_2d_> Error: All resampling intervals must be > 0')
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear']
    end if
    allocate (w(1:nn(1), 1:n(2)))
    allocate (ff(1:nn(1), 1:nn(2)))
    ! Interpolate along the 1st dimension
    !$omp parallel do private(j)
    do j = 1, n(2)
        w(:, j) = reg_to_reg_interp_1d_float(f(:, j), &
            n(1), d(1), o(1), nn(1), dd(1), oo(1), interp_method(1))
    end do
    !$omp end parallel do
    ! Interpolate along the 2nd dimension
    !$omp parallel do private(i)
    do i = 1, nn(1)
        ff(i, :) = reg_to_reg_interp_1d_float(w(i, :), &
            n(2), d(2), o(2), nn(2), dd(2), oo(2), interp_method(2))
    end do
    !$omp end parallel do
end function reg_to_reg_interp_2d_float
function reg_to_reg_interp_3d_float(f, n, d, o, nn, dd, oo, method) result(ff)
    real, dimension(:, :, :) :: f
    integer, dimension(:) :: n, nn
    real, dimension(1:3) :: d, dd, o, oo
    character(len=*), dimension(:), optional :: method
    real, allocatable, dimension(:, :, :) :: ff
    character(len=24), dimension(1:3) :: interp_method
    integer :: i, j, k
    real, allocatable, dimension(:, :, :) :: w1, w2
    call assert(all(d > 0), ' <reg_to_reg_interp_3d_> Error: All original intervals must be > 0')
    call assert(all(dd > 0), ' <reg_to_reg_interp_3d_> Error: All resampling intervals must be > 0')
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear', 'linear']
    end if
    allocate (w1(1:nn(1), 1:n(2), 1:n(3)))
    allocate (w2(1:nn(1), 1:nn(2), 1:n(3)))
    allocate (ff(1:nn(1), 1:nn(2), 1:nn(3)))
    ! Interpolate along the 1st dimension
    !$omp parallel do private(j, k) collapse(2)
    do k = 1, n(3)
        do j = 1, n(2)
            w1(:, j, k) = reg_to_reg_interp_1d_float(f(:, j, k), &
                n(1), d(1), o(1), nn(1), dd(1), oo(1), interp_method(1))
        end do
    end do
    !$omp end parallel do
    ! Interpolate along the 2nd dimension
    !$omp parallel do private(i, k) collapse(2)
    do k = 1, n(3)
        do i = 1, nn(1)
            w2(i, :, k) = reg_to_reg_interp_1d_float(w1(i, :, k), &
                n(2), d(2), o(2), nn(2), dd(2), oo(2), interp_method(2))
        end do
    end do
    !$omp end parallel do
    ! Interpolate along the 3rd dimension
    !$omp parallel do private(i, j) collapse(2)
    do j = 1, nn(2)
        do i = 1, nn(1)
            ff(i, j, :) = reg_to_reg_interp_1d_float(w2(i, j, :), &
                n(3), d(3), o(3), nn(3), dd(3), oo(3), interp_method(3))
        end do
    end do
    !$omp end parallel do
end function reg_to_reg_interp_3d_float
!
!> Interpolate a regularly sampled data using ratios; the new dimensions will be old dimensions / ratios
!
function resample_1d_float(f, r, method) result(ff)
    real, dimension(:), intent(in) :: f
    real, intent(in) :: r
    character(len=*), intent(in), optional :: method
    real, allocatable, dimension(:) :: ff
    character(len=24) :: interp_method
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'linear'
    end if
    call assert(r > 0, ' <resample_1d> Error: Resampling ratio must > 0.')
    ff = reg_to_reg_interp_1d_float(f, size(f), real(1.0), real(0.0), &
        nint(r*(size(f) - 1) + 1), real(1.0/r), real(0.0), interp_method)
end function resample_1d_float
function resample_2d_float(f, r, method) result(ff)
    real, dimension(:, :), intent(in) :: f
    real, dimension(:), intent(in) :: r
    character(len=*), dimension(:), intent(in), optional :: method
    real, allocatable, dimension(:, :) :: ff
    character(len=24), dimension(1:2) :: interp_method
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear']
    end if
    call assert(all(r > 0), ' <resample_2d> Error: Resampling ratio must > 0.')
    ff = reg_to_reg_interp_2d_float(f, shape(f), real([1.0, 1.0]), real([0.0, 0.0]), &
        nint(r*(shape(f) - 1) + 1), real(1.0/r), real([0.0, 0.0]), interp_method)
end function resample_2d_float
function resample_3d_float(f, r, method) result(ff)
    real, dimension(:, :, :), intent(in) :: f
    real, dimension(:), intent(in) :: r
    character(len=*), dimension(:), intent(in), optional :: method
    real, allocatable, dimension(:, :, :) :: ff
    character(len=24), dimension(1:3) :: interp_method
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear', 'linear']
    end if
    call assert(all(r > 0), ' <resample_3d> Error: Resampling ratio must > 0.')
    ff = reg_to_reg_interp_3d_float(f, shape(f), real([1.0, 1.0, 1.0]), real([0.0, 0.0, 0.0]), &
        nint(r*(shape(f) - 1) + 1), real(1.0/r), real([0.0, 0.0, 0.0]), interp_method)
end function resample_3d_float
!
!> Interpolate a regularly sampled data like another array
!
function interp_like_1d_float(f, ff, method) result(g)
    real, dimension(:), intent(in) :: f, ff
    character(len=*), intent(in), optional :: method
    real, allocatable, dimension(:) :: g
    character(len=24) :: interp_method
    integer :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'linear'
    end if
    n = size(f)
    nn = size(ff)
    g = reg_to_reg_interp_1d_float(f, n, real(1.0), real(0.0), &
        nn, real((n - 1.0)/(nn - 1.0)), real(0.0), interp_method)
end function interp_like_1d_float
function interp_like_2d_float(f, ff, method) result(g)
    real, dimension(:, :), intent(in) :: f, ff
    character(len=*), dimension(:), intent(in), optional :: method
    real, allocatable, dimension(:, :) :: g
    character(len=24), dimension(1:2) :: interp_method
    integer, dimension(1:2) :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear']
    end if
    n = shape(f)
    nn = shape(ff)
    g = reg_to_reg_interp_2d_float(f, n, real([1.0, 1.0]), real([0.0, 0.0]), &
        nn, real((n - 1.0)/(nn - 1.0)), real([0.0, 0.0]), interp_method)
end function interp_like_2d_float
function interp_like_3d_float(f, ff, method) result(g)
    real, dimension(:, :, :), intent(in) :: f, ff
    character(len=*), dimension(:), intent(in), optional :: method
    real, allocatable, dimension(:, :, :) :: g
    character(len=24), dimension(1:3) :: interp_method
    integer, dimension(1:3) :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear', 'linear']
    end if
    n = shape(f)
    nn = shape(ff)
    g = reg_to_reg_interp_3d_float(f, n, real([1.0, 1.0, 1.0]), real([0.0, 0.0, 0.0]), &
        nn, real((n - 1.0)/(nn - 1.0)), real([0.0, 0.0, 0.0]), interp_method)
end function interp_like_3d_float
!
!> Interpolate from regularly sampled data based on target dimensions
!
function interp_to_1d_float(f, nn, method) result(g)
    real, dimension(:), intent(in) :: f
    integer, intent(in) :: nn
    character(len=*), intent(in), optional :: method
    real, allocatable, dimension(:) :: g
    character(len=24) :: interp_method
    integer :: n
    real :: dd
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'linear'
    end if
    n = size(f)
    if (n == 1 .or. nn == 1) then
        dd = real(1.0)
    else
        dd = real((n - 1.0)/(nn - 1.0))
    end if
    g = reg_to_reg_interp_1d_float(f, n, real(1.0), real(0.0), &
        nn, dd, real(0.0), interp_method)
end function interp_to_1d_float
function interp_to_2d_float(f, nn, method) result(g)
    real, dimension(:, :), intent(in) :: f
    integer, dimension(1:2), intent(in) :: nn
    character(len=*), dimension(:), intent(in), optional :: method
    real, allocatable, dimension(:, :) :: g
    character(len=24), dimension(1:2) :: interp_method
    integer, dimension(1:2) :: n
    real, dimension(1:2) :: dd
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear']
    end if
    n = shape(f)
    dd = real((n - 1.0)/(nn - 1.0))
    where (n == 1 .or. nn == 1)
        dd = real(1.0)
    end where
    g = reg_to_reg_interp_2d_float(f, n, real([1.0, 1.0]), real([0.0, 0.0]), &
        nn, dd, real([0.0, 0.0]), interp_method)
end function interp_to_2d_float
function interp_to_3d_float(f, nn, method) result(g)
    real, dimension(:, :, :), intent(in) :: f
    integer, dimension(1:3), intent(in) :: nn
    character(len=*), dimension(:), intent(in), optional :: method
    real, allocatable, dimension(:, :, :) :: g
    character(len=24), dimension(1:3) :: interp_method
    integer, dimension(1:3) :: n
    real, dimension(1:3) :: dd
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear', 'linear']
    end if
    n = shape(f)
    dd = real((n - 1.0)/(nn - 1.0))
    where (n == 1 .or. nn == 1)
        dd = real(1.0)
    end where
    g = reg_to_reg_interp_3d_float(f, n, real([1.0, 1.0, 1.0]), real([0.0, 0.0, 0.0]), &
        nn, dd, real([0.0, 0.0, 0.0]), interp_method)
end function interp_to_3d_float
!
!> Interpolate from irregularly sampled data to irregularly sampled data
!
function irreg_to_irreg_interp_1d_float(x, f, xx, method) result(ff)
    real, dimension(:), intent(in) :: x, xx
    real, dimension(:), intent(in) :: f
    real, allocatable, dimension(:) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n = size(x)
    nn = size(xx)
    allocate (ff(1:nn))
    select case (interp_method)
        case ('nearest')
            call interp_nearest_1d_float(n, x, f, nn, xx, ff)
        case ('linear')
            call interp_linear_1d_float(n, x, f, nn, xx, ff)
        case ('cubic')
            call interp_cspline_1d_float(n, x, f, nn, xx, ff)
        case ('pchip')
            call interp_pchip_1d_float(n, x, f, nn, xx, ff)
        case ('quintic')
            call interp_quintic_1d_float(n, x, f, nn, xx, ff)
        case ('mba')
            call interp_mba_1d_float(n, x, f, nn, xx, ff)
        case ('biharmonic')
            call interp_biharmonic_1d_float(n, x, f, nn, xx, ff)
        case ('cubic_spline')
            call interp_cubic_spline_1d_float(n, x, f, nn, xx, ff, method='c2')
        case ('hermite_spline')
            call interp_cubic_spline_1d_float(n, x, f, nn, xx, ff, method='hermite')
        case ('monotonic_spline')
            call interp_cubic_spline_1d_float(n, x, f, nn, xx, ff, method='monotonic')
        case default
            call interp_linear_1d_float(n, x, f, nn, xx, ff)
    end select
end function irreg_to_irreg_interp_1d_float
function irreg_to_irreg_interp_2d_float(x, y, f, xx, yy, method) result(ff)
    real, dimension(:), intent(in) :: x, y, xx, yy
    real, dimension(:), intent(in) :: f
    real, allocatable, dimension(:) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n = size(x)
    nn = size(xx)
    allocate (ff(1:nn))
    select case (interp_method)
        case ('nearest')
            call interp_nearest_2d_float(n, x, y, f, nn, xx, yy, ff)
        case ('mba')
            call interp_mba_2d_float(n, x, y, f, nn, xx, yy, ff)
        case ('biharmonic')
            call interp_biharmonic_2d_float(n, x, y, f, nn, xx, yy, ff)
    end select
end function irreg_to_irreg_interp_2d_float
function irreg_to_irreg_interp_3d_float(x, y, z, f, xx, yy, zz, method) result(ff)
    real, dimension(:) :: x, y, z, xx, yy, zz
    real, dimension(:) :: f
    real, allocatable, dimension(:) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n = size(x)
    nn = size(xx)
    allocate (ff(1:nn))
    select case (interp_method)
        case ('nearest')
            call interp_nearest_3d_float(n, x, y, z, f, nn, xx, yy, zz, ff)
        case ('mba')
            call interp_mba_3d_float(n, x, y, z, f, nn, xx, yy, zz, ff)
        case ('biharmonic')
            call interp_biharmonic_3d_float(n, x, y, z, f, nn, xx, yy, zz, ff)
    end select
end function irreg_to_irreg_interp_3d_float
!
!> Generate mesh grids
!
function meshgrid_float(n, d, o, dim) result(g)
    integer, intent(in) :: n(:)
    real, intent(in) :: o(:), d(:)
    integer, intent(in) :: dim
    real, allocatable, dimension(:) :: g
    integer :: i, j, k, l, n1, n2, n3
    real :: o1, o2, o3, d1, d2, d3
    select case (size(n))
        case (1)
            g = zeros(n(1))
            do i = 1, n(1)
                g(i) = o(1) + (i - 1)*d(1)
            end do
        case (2)
            n1 = n(1)
            n2 = n(2)
            o1 = o(1)
            o2 = o(2)
            d1 = d(1)
            d2 = d(2)
            allocate (g(1:n1*n2))
            select case (dim)
                case (1)
                    l = 1
                    do j = 1, n2
                        do i = 1, n1
                            g(l) = o1 + (i - 1)*d1
                            l = l + 1
                        end do
                    end do
                case (2)
                    l = 1
                    do j = 1, n2
                        do i = 1, n1
                            g(l) = o2 + (j - 1)*d2
                            l = l + 1
                        end do
                    end do
            end select
        case (3)
            n1 = n(1)
            n2 = n(2)
            n3 = n(3)
            o1 = o(1)
            o2 = o(2)
            o3 = o(3)
            d1 = d(1)
            d2 = d(2)
            d3 = d(3)
            allocate (g(1:n1*n2*n3))
            select case (dim)
                case (1)
                    l = 1
                    do k = 1, n3
                        do j = 1, n2
                            do i = 1, n1
                                g(l) = o1 + (i - 1)*d1
                                l = l + 1
                            end do
                        end do
                    end do
                case (2)
                    l = 1
                    do k = 1, n3
                        do j = 1, n2
                            do i = 1, n1
                                g(l) = o2 + (j - 1)*d2
                                l = l + 1
                            end do
                        end do
                    end do
                case (3)
                    l = 1
                    do k = 1, n3
                        do j = 1, n2
                            do i = 1, n1
                                g(l) = o3 + (k - 1)*d3
                                l = l + 1
                            end do
                        end do
                    end do
            end select
    end select
end function meshgrid_float
!
!> Generate mesh grids from vectors; the vectors may be irregularly sampled
!
function meshgrid_1d_float(v) result(g)
    real, dimension(:) :: v
    real, allocatable, dimension(:) :: g
    g = v
end function meshgrid_1d_float
function meshgrid_2d_float(v1, v2) result(g)
    real, dimension(:) :: v1, v2
    real, allocatable, dimension(:, :) :: g
    integer :: n1, n2, l, i, j
    n1 = size(v1)
    n2 = size(v2)
    g = zeros(n1*n2, 2)
    l = 1
    do j = 1, n2
        do i = 1, n1
            g(l, :) = [v1(i), v2(j)]
            l = l + 1
        end do
    end do
end function meshgrid_2d_float
function meshgrid_3d_float(v1, v2, v3) result(g)
    real, dimension(:) :: v1, v2, v3
    real, allocatable, dimension(:, :) :: g
    integer :: n1, n2, n3, l, i, j, k
    n1 = size(v1)
    n2 = size(v2)
    n3 = size(v3)
    g = zeros(n1*n2*n3, 3)
    l = 1
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                g(l, :) = [v1(i), v2(j), v3(k)]
                l = l + 1
            end do
        end do
    end do
end function meshgrid_3d_float
!
!> Interpolate from irregularly sampled data to regularly sampled data
!
function irreg_to_reg_interp_1d_float(x, f, n, d, o, method) result(ff)
    real, dimension(:) :: x
    real, dimension(:) :: f
    integer :: n
    real :: d, o
    real, allocatable, dimension(:) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: l
    real, allocatable, dimension(:) :: xx
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    l = size(x)
    call assert(n > 0 .and. d > 0, ' <irreg_to_reg_interp_1d> Error: n and d must > 0. ')
    xx = regspace(o, d, o + (n - 1)*d)
    ff = zeros(n)
    select case (interp_method)
        case ('nearest')
            call interp_nearest_1d_float(l, x, f, n, xx, ff)
        case ('linear')
            call interp_linear_1d_float(l, x, f, n, xx, ff)
        case ('cubic')
            call interp_cspline_1d_float(l, x, f, n, xx, ff)
        case ('pchip')
            call interp_pchip_1d_float(l, x, f, n, xx, ff)
        case ('quintic')
            call interp_quintic_1d_float(l, x, f, n, xx, ff)
        case ('mba')
            call interp_mba_1d_float(l, x, f, n, xx, ff)
        case ('biharmonic')
            call interp_biharmonic_1d_float(l, x, f, n, xx, ff)
        case ('cubic_spline')
            call interp_cubic_spline_1d_float(l, x, f, n, xx, ff, method='c2')
        case ('hermite_spline')
            call interp_cubic_spline_1d_float(l, x, f, n, xx, ff, method='hermite')
        case ('monotonic_spline')
            call interp_cubic_spline_1d_float(l, x, f, n, xx, ff, method='monotonic')
        case default
            call interp_linear_1d_float(l, x, f, n, xx, ff)
    end select
end function irreg_to_reg_interp_1d_float
function irreg_to_reg_interp_2d_float(x, y, f, n, d, o, method) result(ff)
    real, dimension(:) :: x, y
    real, dimension(:) :: f
    integer, dimension(1:2) :: n
    real, dimension(1:2) :: d, o
    real, allocatable, dimension(:, :) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: nn, l
    real, allocatable, dimension(:) :: xx, yy
    real, allocatable, dimension(:) :: tf
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    l = size(x)
    call assert(all(n > 0) .and. all(d > 0), ' <irreg_to_reg_interp_2d> Error: All n and d must > 0. ')
    xx = meshgrid(n=n, d=d, o=o, dim=1)
    yy = meshgrid(n=n, d=d, o=o, dim=2)
    nn = product(n)
    tf = zeros(nn)
    select case (interp_method)
        case ('nearest')
            call interp_nearest_2d_float(l, x, y, f, nn, xx, yy, tf)
        case ('mba')
            call interp_mba_2d_float(l, x, y, f, nn, xx, yy, tf)
        case ('biharmonic')
            call interp_biharmonic_2d_float(l, x, y, f, nn, xx, yy, tf)
    end select
    ff = reshape(tf, n)
end function irreg_to_reg_interp_2d_float
function irreg_to_reg_interp_3d_float(x, y, z, f, n, d, o, method) result(ff)
    real, dimension(:) :: x, y, z
    real, dimension(:) :: f
    integer, dimension(1:3) :: n
    real, dimension(1:3) :: d, o
    real, allocatable, dimension(:, :, :) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: nn, l
    real, allocatable, dimension(:) :: xx, yy, zz
    real, allocatable, dimension(:) :: tf
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    l = size(x)
    call assert(all(n > 0) .and. all(d > 0), ' <irreg_to_reg_interp_3d> Error: All n and d must > 0. ')
    xx = meshgrid(n=n, d=d, o=o, dim=1)
    yy = meshgrid(n=n, d=d, o=o, dim=2)
    zz = meshgrid(n=n, d=d, o=o, dim=3)
    nn = product(n)
    tf = zeros(nn)
    select case (interp_method)
        case ('nearest')
            call interp_nearest_3d_float(l, x, y, z, f, nn, xx, yy, zz, tf)
        case ('mba')
            call interp_mba_3d_float(l, x, y, z, f, nn, xx, yy, zz, tf)
        case ('biharmonic')
            call interp_biharmonic_3d_float(l, x, y, z, f, nn, xx, yy, zz, tf)
    end select
    ff = reshape(tf, n)
end function irreg_to_reg_interp_3d_float
!
!> Remove NaN values from regularly sampled data
!
function inpaint_1d_float(w, method) result(ww)
    real, dimension(:), intent(in) :: w
    character(len=*), optional :: method
    real, allocatable, dimension(:) :: ww
    character(len=24) :: interp_method
    integer :: n1, n, nn
    integer :: i, l, h
    real, allocatable, dimension(:) :: x1, xx1
    real, allocatable, dimension(:) :: f, ff
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n1 = size(w)
    ! Find NaN pixels
    n = count(.not. isnan(w))
    nn = n1
    x1 = zeros(n)
    f = zeros(n)
    xx1 = zeros(nn)
    ff = zeros(nn)
    l = 1
    h = 1
    do i = 1, n1
        if (.not. isnan(w(i))) then
            x1(l) = i - 1.0
            f(l) = w(i)
            l = l + 1
        end if
        xx1(h) = i - 1.0
        h = h + 1
    end do
    select case (interp_method)
        case ('nearest')
            call interp_nearest_1d_float(n, x1, f, nn, xx1, ff)
        case ('linear')
            call interp_linear_1d_float(n, x1, f, nn, xx1, ff)
        case ('sinc')
            call interp_sinc_1d_float(n, x1, f, nn, xx1, ff)
        case ('cubic')
            call interp_cspline_1d_float(n, x1, f, nn, xx1, ff)
        case ('pchip')
            call interp_pchip_1d_float(n, x1, f, nn, xx1, ff)
        case ('quintic')
            call interp_quintic_1d_float(n, x1, f, nn, xx1, ff)
        case ('mba')
            call interp_mba_1d_float(n, x1, f, nn, xx1, ff)
        case ('biharmonic')
            call interp_biharmonic_1d_float(n, x1, f, nn, xx1, ff)
        case ('cubic_spline')
            call interp_cubic_spline_1d_float(n, x1, f, nn, xx1, ff, method='c2')
        case ('hermite_spline')
            call interp_cubic_spline_1d_float(n, x1, f, nn, xx1, ff, method='hermite')
        case ('monotonic_spline')
            call interp_cubic_spline_1d_float(n, x1, f, nn, xx1, ff, method='monotonic')
        case default
            call interp_linear_1d_float(n, x1, f, nn, xx1, ff)
    end select
    ww = ff
end function inpaint_1d_float
function inpaint_2d_float(w, method) result(ww)
    real, dimension(:, :), intent(in) :: w
    character(len=*), optional :: method
    real, allocatable, dimension(:, :) :: ww
    character(len=24) :: interp_method
    integer :: n1, n2, n, nn
    integer :: i, j, l, h
    real, allocatable, dimension(:) :: x1, xx1, x2, xx2
    real, allocatable, dimension(:) :: f, ff
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Find NaN pixels
    n = count(.not. isnan(w))
    nn = size(w)
    x1 = zeros(n)
    x2 = zeros(n)
    f = zeros(n)
    xx1 = zeros(nn)
    xx2 = zeros(nn)
    ff = zeros(nn)
    l = 1
    h = 1
    do j = 1, n2
        do i = 1, n1
            if (.not. isnan(w(i, j))) then
                x1(l) = i - 1.0
                x2(l) = j - 1.0
                f(l) = w(i, j)
                l = l + 1
            end if
            xx1(h) = i - 1.0
            xx2(h) = j - 1.0
            h = h + 1
        end do
    end do
    select case (interp_method)
        case ('nearest')
            call interp_nearest_2d_float(n, x1, x2, f, nn, xx1, xx2, ff)
        case ('mba')
            call interp_mba_2d_float(n, x1, x2, f, nn, xx1, xx2, ff)
        case ('biharmonic')
            call interp_biharmonic_2d_float(n, x1, x2, f, nn, xx1, xx2, ff)
    end select
    ww = reshape(ff, [n1, n2])
end function inpaint_2d_float
function inpaint_3d_float(w, method) result(ww)
    real, dimension(:, :, :), intent(in) :: w
    character(len=*), optional :: method
    real, allocatable, dimension(:, :, :) :: ww
    character(len=24) :: interp_method
    integer :: n1, n2, n3, n, nn
    integer :: i, j, k, l, h
    real, allocatable, dimension(:) :: x1, xx1, x2, xx2, x3, xx3
    real, allocatable, dimension(:) :: f, ff
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Find NaN pixels
    n = count(.not. isnan(w))
    nn = size(w)
    x1 = zeros(n)
    x2 = zeros(n)
    x3 = zeros(n)
    f = zeros(n)
    xx1 = zeros(nn)
    xx2 = zeros(nn)
    xx3 = zeros(nn)
    ff = zeros(nn)
    l = 1
    h = 1
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                if (.not. isnan(w(i, j, k))) then
                    x1(l) = i - 1.0
                    x2(l) = j - 1.0
                    x3(l) = k - 1.0
                    f(l) = w(i, j, k)
                    l = l + 1
                end if
                xx1(h) = i - 1.0
                xx2(h) = j - 1.0
                xx3(h) = k - 1.0
                h = h + 1
            end do
        end do
    end do
    select case (interp_method)
        case ('nearest')
            call interp_nearest_3d_float(n, x1, x2, x3, f, nn, xx1, xx2, xx3, ff)
        case ('mba')
            call interp_mba_3d_float(n, x1, x2, x3, f, nn, xx1, xx2, xx3, ff)
        case ('biharmonic')
            call interp_biharmonic_3d_float(n, x1, x2, x3, f, nn, xx1, xx2, xx3, ff)
    end select
    ww = reshape(ff, [n1, n2, n3])
end function inpaint_3d_float
function point_interp_linear_1d_float(x, f, xx) result(ff)
    real, dimension(:) :: x
    real, dimension(:) :: f
    real :: xx, ff
    real, dimension(1:2) :: w
    integer :: i
    do i = 1, 2
        w(i) = abs(x(3 - i) - xx)/(x(2) - x(1))
    end do
    ff = sum(w*f)
end function point_interp_linear_1d_float
function point_interp_linear_2d_float(x, y, f, xx, yy) result(ff)
    real, dimension(:) :: x, y
    real, dimension(:, :) :: f
    real :: xx, yy, ff
    real, dimension(1:2, 1:2) :: w
    integer :: i, j
    do i = 1, 2
        do j = 1, 2
            w(i, j) = abs((x(3 - i) - xx)*(y(3 - j) - yy))/((x(2) - x(1))*(y(2) - y(1)))
        end do
    end do
    ff = sum(w*f)
end function point_interp_linear_2d_float
function point_interp_linear_3d_float(x, y, z, f, xx, yy, zz) result(ff)
    real, dimension(:) :: x, y, z
    real, dimension(:, :, :) :: f
    real :: xx, yy, zz, ff
    real, dimension(1:2, 1:2, 1:2) :: w
    integer :: i, j, k
    do i = 1, 2
        do j = 1, 2
            do k = 1, 2
                w(i, j, k) = abs((x(3 - i) - xx)*(y(3 - j) - yy)*(z(3 - k) - zz)) &
                    /((x(2) - x(1))*(y(2) - y(1))*(z(2) - z(1)))
            end do
        end do
    end do
    ff = sum(w*f)
end function point_interp_linear_3d_float
function point_interp_barycentric_2d_float(v1, v2, v3, f, p) result(ff)
    real, dimension(:) :: v1, v2, v3, f, p
    real :: ff
    real, dimension(1:3) :: w
    real :: a123
    a123 = area(v1, v2, v3)
    if (abs(a123) < 1e-6) then
        print *, ' <point_interp_barycentric_2d> Error: The points form a degenerate triangle! '
        stop
    end if
    ! Note the circulant order
    w(1) = area(p, v2, v3)/a123
    w(2) = area(v1, p, v3)/a123
    w(3) = area(v1, v2, p)/a123
    if (any(w < 0)) then
        ! print *, ' <point_interp_barycentric_2d> Warning: The point is outside of the triangle! '
        ff = 0
        return
    end if
    ff = sum(w*f)
contains
    function area(v1, v2, v3) result(a)
        real, dimension(:) :: v1, v2, v3
        real :: a
        a = det(transpose(reshape([v2 - v1, v3 - v1], [2, 2])))
    end function area
end function point_interp_barycentric_2d_float
function point_interp_barycentric_3d_float(v1, v2, v3, v4, f, p) result(ff)
    real, dimension(:) :: v1, v2, v3, v4, f, p
    real :: ff
    real, dimension(1:4) :: w
    real :: vol1234
    ! Calculate the determinant for the tetrahedron volume
    vol1234 = vol(v1, v2, v3, v4)
    if (abs(vol1234) < 1e-6) then
        print *, ' <point_interp_barycentric_3d> Error: The points form a degenerate tetrahedron. '
        stop
    end if
    ! Calculate the determinants for sub-tetrahedra
    ! Note the circulant order
    w(1) = vol(p, v2, v3, v4)/vol1234
    w(2) = vol(v1, p, v3, v4)/vol1234
    w(3) = vol(v1, v2, p, v4)/vol1234
    w(4) = vol(v1, v2, v3, p)/vol1234
    if (any(w < 0)) then
        ! print *, ' <point_interp_barycentric_3d> Warning: The point is outside of the tetrahedron! '
        ff = 0
        return
    end if
    ff = sum(w*f)
contains
    function vol(v1, v2, v3, v4) result(v)
        real, dimension(:) :: v1, v2, v3, v4
        real :: v
        v = det(transpose(reshape([v2 - v1, v3 - v1, v4 - v1], [3, 3])))
    end function vol
end function point_interp_barycentric_3d_float
! external
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
! external
!
!> Nearest neighbour interpolation for 1D data
!
subroutine interp_nearest_1d_double(n, x, y, nn, xx, yy)
    integer, intent(in) :: n, nn
    double precision, dimension(:), intent(in) :: x, xx
    double precision, dimension(:), intent(in) :: y
    double precision, dimension(:), intent(out) :: yy
    integer :: i
    call assert(size(x) == size(y), &
        ' <interp_nearest_1d> Error: size(x) /= size(y). ')
    i = n
    ! A brute-foce nearest neighbour implementation; could be slow for large data
    do i = 1, nn
        yy(i) = y(minloc(abs(xx(i) - x), dim=1))
    end do
end subroutine interp_nearest_1d_double
subroutine interp_nearest_2d_double(n, x, y, z, nn, xx, yy, zz)
    integer, intent(in) :: n, nn
    double precision, dimension(:), intent(in) :: x, y, xx, yy
    double precision, dimension(:), intent(in) :: z
    double precision, dimension(:), intent(out) :: zz
    integer :: i
    call assert(size(x) == size(y) .and. size(y) == size(z), &
        ' <interp_nearest_3d> Error: Sizes of x, y, z are inconsistent. ')
    i = n
    ! A brute-foce nearest neighbour implementation; could be slow for large data
    !$omp parallel do private(i)
    do i = 1, nn
        zz(i) = z(minloc((xx(i) - x)**2 + (yy(i) - y)**2, dim=1))
    end do
    !$omp end parallel do
end subroutine interp_nearest_2d_double
subroutine interp_nearest_3d_double(n, x, y, z, v, nn, xx, yy, zz, vv)
    integer, intent(in) :: n, nn
    double precision, dimension(:), intent(in) :: x, y, z, xx, yy, zz
    double precision, dimension(:), intent(in) :: v
    double precision, dimension(:), intent(out) :: vv
    integer :: i
    call assert(size(x) == size(y) .and. size(y) == size(z) .and. size(z) == size(v), &
        ' <interp_nearest_3d> Error: Sizes of x, y, z, v are inconsistent. ')
    i = n
    ! A brute-foce nearest neighbour implementation; could be slow for large data
    !$omp parallel do private(i)
    do i = 1, nn
        vv(i) = v(minloc((xx(i) - x)**2 + (yy(i) - y)**2 + (zz(i) - z)**2, dim=1))
    end do
    !$omp end parallel do
end subroutine interp_nearest_3d_double
!
!> Linear interpolation for 1D data
!
subroutine interp_linear_1d_double(n, x, y, nn, xx, yy)
    integer, intent(in) :: n, nn
    double precision, dimension(:), intent(in) :: x, xx
    double precision, dimension(:), intent(in) :: y
    double precision, dimension(:), intent(out) :: yy
    integer :: i, k
    double precision :: t
    call assert(size(x) == size(y), ' <interp_linear_1d> Error: size(x) /= size(y). ')
    yy = 0.0d0
    if (n == 1) then
        yy(1:nn) = y(1)
        return
    end if
    do i = 1, nn
        if (xx(i) <= x(1)) then
            t = (xx(i) - x(1))/(x(2) - x(1))
            yy(i) = (1.0d0 - t)*y(1) + t*y(2)
        else if (x(n) <= xx(i)) then
            t = (xx(i) - x(n - 1))/(x(n) - x(n - 1))
            yy(i) = (1.0 - t)*y(n - 1) + t*y(n)
        else
            do k = 2, n
                if (xx(i) >= x(k - 1) .and. xx(i) < x(k)) then
                    t = (xx(i) - x(k - 1))/(x(k) - x(k - 1))
                    yy(i) = (1.0d0 - t)*y(k - 1) + t*y(k)
                    exit
                end if
            end do
        end if
    end do
end subroutine interp_linear_1d_double
!
!> Cubic spline interpolation for 1D data; contains three methods:
!> - c2: cubic spline, a c2-continuous spline
!> - hermite: Hermite spline, where each piece is a third-degree polynomial
!> specified in Hermite form, that is, by its values and
!> first derivatives at the end points of the corresponding domain interval.
!> - monotonic: cubic monotonic Hermite spline, a.k.a. PCHIP,
!> is a variant of cubic interpolation
!> that preserves monotonicity of the data set being interpolated.
!> Here I implemented the algorithm described in
!> https://en.wikipedia.org/wiki/Monotone_cubic_interpolation,
!> with modifications (slope selection and boundary points handling).
!
subroutine interp_cubic_spline_1d_double(n, x, y, nn, xx, yy, method)
    integer, intent(in) :: n, nn
    double precision, dimension(:), intent(in) :: x, xx
    double precision, dimension(:), intent(in) :: y
    character(len=*), intent(in) :: method
    double precision, dimension(:), intent(out) :: yy
    double precision, allocatable, dimension(:) :: a, b, c, d, h, r
    double precision, allocatable, dimension(:, :) :: m
    integer :: i, j
    double precision :: dist
    double precision :: f1, f2
    double precision, allocatable, dimension(:) :: delta, mm, alpha, beta, tau
    double precision :: h00, h01, h10, h11
    double precision :: z(1:3)
    call assert(size(x) == size(y), ' <interp_cubic_spline_1d> Error: size(x) /= size(y). ')
    call assert(size(x) >= 3, ' <interp_cubic_spline_1d> Error: size(x) must >= 3. ')
    a = y
    b = zeros(n)
    d = zeros(n)
    h = zeros(n)
    do i = 1, n - 1
        h(i) = x(i + 1) - x(i)
    end do
    h(n) = h(n - 1)
    select case (method)
        case ('c2')
            ! Cubic spline
            m = zeros(3, n - 2)
            r = zeros(n - 2)
            ! Super-diagonal elements
            do i = 2, n - 2
                m(1, i) = h(i)/3.0
            end do
            ! Diagonal elements
            do i = 2, n - 1
                m(2, i - 1) = (h(i - 1) + h(i))*2.0/3.0
            end do
            ! Sub-diagonal elements
            do i = 2, n - 2
                m(3, i - 1) = h(i - 1)/3.0
            end do
            ! RHS
            do i = 2, n - 1
                r(i - 1) = (y(i + 1) - y(i))/h(i) - (y(i) - y(i - 1))/h(i - 1)
            end do
            c = [dble(0.0/2.0), solve_band(m, 1, 1, r), dble(0.0/2.0)]
            do i = 1, n - 1
                d(i) = (c(i + 1) - c(i))/(3.0*h(i))
                b(i) = (y(i + 1) - y(i))/h(i) - (2*c(i) + c(i + 1))*h(i)/3.0
            end do
            d(n) = 0.0
            b(n) = 3*d(n - 1)*h(n - 1)**2 + 2*c(n - 1)*h(n - 1) + b(n - 1)
            do i = 1, nn
                if (xx(i) < x(1)) then
                    dist = xx(i) - x(1)
                    yy(i) = a(1) + b(1)*dist + c(1)*dist**2
                else if (xx(i) >= x(n)) then
                    dist = xx(i) - x(n)
                    yy(i) = a(n) + b(n)*dist + c(n)*dist**2
                else
                    do j = 1, n - 1
                        if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
                            dist = xx(i) - x(j)
                            yy(i) = a(j) + b(j)*dist + c(j)*dist**2 + d(j)*dist**3
                            exit
                        end if
                    end do
                end if
            end do
        case ('hermite')
            ! Cubic Hermite spline
            c = zeros(n)
            do i = 2, n - 1
                f1 = (y(i) - y(i - 1))/h(i - 1)
                f2 = (y(i + 1) - y(i))/h(i)
                b(i) = h(i - 1)/(h(i - 1) + h(i))*f2 + h(i)/(h(i) + h(i - 1))*f1
            end do
            b(1) = 0.5*(-b(2) + 3*(y(2) - y(1))/h(1))
            b(n) = 0.5*(-b(n - 1) + 3*(y(n) - y(n - 1))/h(n - 1))
            do i = 1, n - 1
                c(i) = -(2*b(i) + b(i + 1))/h(i) + 3*(a(i + 1) - a(i))/h(i)**2
                d(i) = -2*c(i)/(3*h(i)) + (b(i + 1) - b(i))/(3*h(i)**2)
            end do
            do i = 1, nn
                if (xx(i) < x(1)) then
                    dist = xx(i) - x(1)
                    yy(i) = a(1) + b(1)*dist + c(1)*dist**2
                else if (xx(i) >= x(n)) then
                    dist = xx(i) - x(n)
                    yy(i) = a(n) + b(n)*dist + c(n)*dist**2
                else
                    do j = 1, n - 1
                        if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
                            dist = xx(i) - x(j)
                            yy(i) = a(j) + b(j)*dist + c(j)*dist**2 + d(j)*dist**3
                            exit
                        end if
                    end do
                end if
            end do
        case ('monotonic')
            ! PCHIP spline
            ! For handling boundary points
            c = zeros(n)
            do i = 2, n - 1
                f1 = (y(i) - y(i - 1))/h(i - 1)
                f2 = (y(i + 1) - y(i))/h(i)
                b(i) = h(i - 1)/(h(i - 1) + h(i))*f2 + h(i)/(h(i) + h(i - 1))*f1
            end do
            b(1) = 0.5*(-b(2) + 3*(y(2) - y(1))/h(1))
            b(n) = 0.5*(-b(n - 1) + 3*(y(n) - y(n - 1))/h(n - 1))
            do i = n - 1, n - 1
                c(i) = -(2*b(i) + b(i + 1))/h(i) + 3*(a(i + 1) - a(i))/h(i)**2
            end do
            c(n) = c(n - 1)
            ! Monotonicity-perserving cubic splines
            delta = zeros(n)
            do i = 1, n - 1
                delta(i) = (y(i + 1) - y(i))/(x(i + 1) - x(i))
            end do
            delta(n) = delta(n - 1)
            mm = zeros(n)
            do i = 2, n - 1
                ! Choosing the min slope in L1 sense
                z = [0.5*(delta(i - 1) + delta(i)), delta(i - 1), delta(i)]
                mm(i) = z(minloc(abs(z), dim=1))
                if (delta(i - 1)*delta(i) < 0) then
                    mm(i) = 0
                end if
            end do
            mm(1) = delta(1)
            mm(n) = delta(n - 1)
            do i = 1, n - 1
                if (delta(i) == 0) then
                    mm(i:i + 1) = 0
                end if
            end do
            alpha = zeros(n)
            beta = zeros(n)
            do i = 1, n - 1
                if (mm(i) /= 0) then
                    alpha(i) = mm(i)/delta(i)
                    beta(i) = mm(i + 1)/delta(i)
                end if
            end do
            do i = 1, n - 1
                if (alpha(i) < 0) then
                    mm(i) = 0
                end if
                if (beta(i) < 0) then
                    mm(i + 1) = 0
                end if
            end do
            tau = zeros(n)
            do i = 1, n - 1
                if (alpha(i)**2 + beta(i)**2 > 9) then
                    tau(i) = 9/sqrt(alpha(i)**2 + beta(i)**2)
                end if
            end do
            do i = 1, n - 1
                if (alpha(i)**2 + beta(i)**2 > 9 .and. mm(i) /= 0) then
                    mm(i) = tau(i)*alpha(i)*delta(i)
                    mm(i + 1) = tau(i)*beta(i)*delta(i)
                end if
            end do
            yy = 0
            do i = 1, nn
                if (xx(i) < x(1)) then
                    dist = xx(i) - x(1)
                    yy(i) = a(1) + b(1)*dist + c(1)*dist**2
                else if (xx(i) >= x(n)) then
                    dist = xx(i) - x(n)
                    yy(i) = a(n) + b(n)*dist + c(n)*dist**2
                else
                    do j = 1, n - 1
                        if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
                            dist = (xx(i) - x(j))/h(j)
                            h00 = (1 + 2*dist)*(1 - dist)**2
                            h10 = dist*(1 - dist)**2
                            h01 = dist**2*(3 - 2*dist)
                            h11 = dist**2*(dist - 1)
                            yy(i) = h00*y(j) + h10*h(j)*mm(j) + h01*y(j + 1) + h11*h(j)*mm(j + 1)
                            exit
                        end if
                    end do
                end if
            end do
    end select
    ! yy = 0
    ! select case (order)
    ! case (0)
    ! ! The function itself
    ! do i = 1, nn
    ! if (xx(i) < x(1)) then
    ! dist = xx(i) - x(1)
    ! yy(i) = a(1) + b(1)*dist + c(1)*dist**2
    !
    ! else if (xx(i) >= x(n)) then
    ! dist = xx(i) - x(n)
    ! yy(i) = a(n) + b(n)*dist + c(n)*dist**2
    ! else
    ! do j = 1, n - 1
    ! if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
    ! dist = xx(i) - x(j)
    ! yy(i) = a(j) + b(j)*dist + c(j)*dist**2 + d(j)*dist**3
    ! exit
    ! end if
    ! end do
    ! end if
    ! end do
    ! case (1)
    ! ! First-order derivative
    ! do i = 1, nn
    ! if (xx(i) < x(1)) then
    ! dist = xx(i) - x(1)
    ! yy(i) = b(1) + 2*c(1)*dist
    ! else if (xx(i) >= x(n)) then
    ! dist = xx(i) - x(n)
    ! yy(i) = b(n) + 2*c(n)*dist
    ! else
    ! do j = 1, n - 1
    ! if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
    ! dist = xx(i) - x(j)
    ! yy(i) = b(j) + 2*c(j)*dist + 3*d(j)*dist**2
    ! exit
    ! end if
    ! end do
    ! end if
    ! end do
    ! case (2)
    ! ! Second-order derivative
    ! do i = 1, nn
    ! if (xx(i) < x(1)) then
    ! yy(i) = 2*c(1)
    ! else if (xx(i) >= x(n)) then
    ! yy(i) = 2*c(n)
    ! else
    ! do j = 1, n - 1
    ! if (xx(i) >= x(j) .and. xx(i) < x(j + 1)) then
    ! dist = xx(i) - x(j)
    ! yy(i) = 2*c(j) + 6*d(j)*dist
    ! exit
    ! end if
    ! end do
    ! end if
    ! end do
    ! end select
end subroutine interp_cubic_spline_1d_double
!
!> Biharmonic interpolation; could be very slow and also memory-intensive
!> because needs to solve a large linear system;
!> https://doi.org/10.1029/GL014i002p00139
!
subroutine interp_biharmonic_1d_double(n_, x, y, nn, xx, yy)
    integer, intent(in) :: n_, nn
    double precision, dimension(:), intent(in) :: x, xx
    double precision, dimension(:), intent(in) :: y
    double precision, dimension(:), intent(out) :: yy
    integer :: i, j, n
    double precision, allocatable, dimension(:, :) :: dist
    double precision, allocatable, dimension(:) :: w
    double precision, allocatable, dimension(:) :: xd
    double precision, allocatable, dimension(:) :: yd
    double precision, allocatable, dimension(:, :) :: xyd
    call assert(size(x) == size(y), ' <interp_biharmonic_1d> Error: size(x) /= size(y). ')
    ! First find unique pairs of original scatter points
    ! Otherwise the linear system is not solvable
    xyd = unique(reshape([dble(x), y], [n_, 2]), cols=[1])
    n = size(xyd, 1)
    xd = xyd(:, 1)
    yd = xyd(:, 2)
    ! Compute distance and Green's function
    dist = zeros(n, n)
    do j = 1, n
        do i = 1, n
            if (i /= j) then
                dist(i, j) = abs(xd(i) - xd(j))
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            else
                dist(i, j) = 0.0
            end if
        end do
    end do
    ! Compute weights
    w = solve(dist, yd)
    ! Comptue interpolation values
    dist = zeros(nn, n)
    do j = 1, n
        do i = 1, nn
            dist(i, j) = abs(xx(i) - xd(j))
            if (abs(dist(i, j)) /= 0) then
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            end if
        end do
    end do
    yy = matx(dist, w)
end subroutine interp_biharmonic_1d_double
subroutine interp_biharmonic_2d_double(n_, x, y, z, nn, xx, yy, zz)
    integer, intent(in) :: n_, nn
    double precision, dimension(:), intent(in) :: x, y, xx, yy
    double precision, dimension(:), intent(in) :: z
    double precision, dimension(:), intent(out) :: zz
    integer :: i, j, n
    double precision, allocatable, dimension(:, :) :: dist
    double precision, allocatable, dimension(:) :: w
    double precision, allocatable, dimension(:) :: xd, yd
    double precision, allocatable, dimension(:) :: zd
    double precision, allocatable, dimension(:, :) :: xyzd
    call assert(size(x) == size(y) .and. size(y) == size(z), &
        ' <interp_biharmonic_2d> Error: Sizes of x, y, and z are inconsistent. ')
    ! First find unique pairs of original scatter points
    ! Otherwise the linear system is not solvable
    xyzd = unique(reshape([dble(x), dble(y), z], [n_, 3]), cols=[1, 2])
    n = size(xyzd, 1)
    xd = xyzd(:, 1)
    yd = xyzd(:, 2)
    zd = xyzd(:, 3)
    dist = zeros(n, n)
    ! Compute distance and Green's function
    !$omp parallel do private(i, j)
    do j = 1, n
        do i = 1, n
            dist(i, j) = sqrt((xd(i) - xd(j))**2 + (yd(i) - yd(j))**2)
            if (i /= j) then
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            else
                dist(i, j) = 0.0
            end if
        end do
    end do
    !$omp end parallel do
    ! Compute weights
    w = solve(dist, zd)
    ! Comptue interpolation values
    dist = zeros(nn, n)
    !$omp parallel do private(i, j)
    do j = 1, n
        do i = 1, nn
            dist(i, j) = sqrt((xx(i) - xd(j))**2 + (yy(i) - yd(j))**2)
            if (abs(dist(i, j)) /= 0) then
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            end if
        end do
    end do
    !$omp end parallel do
    zz = matx(dist, w)
end subroutine interp_biharmonic_2d_double
subroutine interp_biharmonic_3d_double(n_, x, y, z, v, nn, xx, yy, zz, vv)
    integer, intent(in) :: n_, nn
    double precision, dimension(:), intent(in) :: x, y, z, xx, yy, zz
    double precision, dimension(:), intent(in) :: v
    double precision, dimension(:), intent(out) :: vv
    integer :: i, j, n
    double precision, allocatable, dimension(:, :) :: dist
    double precision, allocatable, dimension(:) :: w
    double precision, allocatable, dimension(:) :: xd, yd, zd
    double precision, allocatable, dimension(:) :: vd
    double precision, allocatable, dimension(:, :) :: xyzvd
    call assert(size(x) == size(y) .and. size(y) == size(z) .and. size(z) == size(v), &
        ' <interp_biharmonic_3d> Error: Sizes of x, y, z, and v are inconsistent. ')
    ! First find unique pairs of original scatter points
    ! Otherwise the linear system is not solvable
    xyzvd = unique(reshape([dble(x), dble(y), dble(z), v], [n_, 4]), cols=[1, 2, 3])
    n = size(xyzvd, 1)
    xd = xyzvd(:, 1)
    yd = xyzvd(:, 2)
    zd = xyzvd(:, 3)
    vd = xyzvd(:, 4)
    ! Compute distance and Green's function
    dist = zeros(n, n)
    !$omp parallel do private(i, j)
    do j = 1, n
        do i = 1, n
            dist(i, j) = sqrt((xd(i) - xd(j))**2 + (yd(i) - yd(j))**2 &
                + (zd(i) - zd(j))**2)
            if (i /= j) then
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            else
                dist(i, j) = 0.0
            end if
        end do
    end do
    !$omp end parallel do
    ! Compute weights
    w = solve(dist, vd)
    ! Comptue interpolation values
    dist = zeros(nn, n)
    !$omp parallel do private(i, j)
    do j = 1, n
        do i = 1, nn
            dist(i, j) = sqrt((xx(i) - xd(j))**2 + (yy(i) - yd(j))**2 &
                + (zz(i) - zd(j))**2)
            if (abs(dist(i, j)) /= 0) then
                dist(i, j) = dist(i, j)**2*(log(dist(i, j)) - 1.0)
            end if
        end do
    end do
    !$omp end parallel do
    vv = matx(dist, w)
end subroutine interp_biharmonic_3d_double
!
!> Windowed sinc interpolation for 1D data
!
subroutine interp_sinc_1d_double(n, x, y, nn, xx, yy)
    integer, intent(in) :: n, nn
    double precision, dimension(:), intent(in) :: x, xx
    double precision, dimension(:), intent(in) :: y
    double precision, dimension(:), intent(out) :: yy
    integer :: i, h, nk1, nk2
    double precision :: d, dist
    double precision :: ya, yb
    double precision, allocatable, dimension(:) :: ksinc, xd
    double precision, allocatable, dimension(:) :: yd
    integer :: nkw
    double precision :: b0
    nkw = 8
    b0 = 3.0d0
    xd = x
    yd = y
    d = x(2) - x(1)
    if (xx(1) < x(1)) then
        nk1 = nkw + ceiling(abs(xx(1) - x(1))/d)
    else
        nk1 = nkw
    end if
    if (xx(nn) > x(n)) then
        nk2 = nkw + ceiling(abs(xx(nn) - x(n))/d)
    else
        nk2 = nkw
    end if
    call pad_array(xd, [nk1, nk2])
    call pad_array(yd, [nk1, nk2])
    ya = y(2) - y(1)
    yb = y(n) - y(n - 1)
    do i = 1, nk1
        xd(1 - i) = xd(1) - i*d
        yd(1 - i) = yd(1) - i*ya
    end do
    do i = 1, nk2
        xd(n + i) = xd(n) + i*d
        yd(n + i) = yd(n) + i*yb
    end do
    do i = 1, nn
        h = nint((xx(i) - xd(1))/d) + 1
        dist = (xx(i) - xd(h))/d
        ksinc = dist - regspace(-nkw, 1, nkw)
        ksinc = sinc(ksinc*const_pi)*window_function(ksinc/nkw + 0.5d0, method='kaiser', alpha=b0 + 0.0d0)
        yy(i) = sum(ksinc*yd(h - nkw:h + nkw))
    end do
end subroutine interp_sinc_1d_double
!==================================================================================================
!
!> Interpolate regularly sampled data to regularly sampled data
!
function reg_to_reg_interp_1d_double(f, n1, d1, o1, nn1, dd1, oo1, method) result(ff)
    double precision, dimension(:) :: f
    integer :: n1, nn1
    double precision :: d1, dd1, o1, oo1
    character(len=*), optional :: method
    double precision, allocatable, dimension(:) :: ff
    double precision, allocatable, dimension(:) :: x, xx
    character(len=24) :: interp_method
    integer :: i
    call assert(d1 > 0, ' <reg_to_reg_interp_1d> Error: The original interval must be > 0')
    call assert(dd1 > 0, ' <reg_to_reg_interp_1d> Error: The resampling interval must be > 0')
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'linear'
    end if
    allocate (ff(1:nn1))
    if (n1 == nn1 .and. d1 == dd1 .and. o1 == oo1) then
        ! Trivial case
        ff = f
    else
        ! Do interpolation
        ! Point coordinates before and after interpolation
        allocate (x(1:n1))
        allocate (xx(1:nn1))
        do i = 1, n1
            x(i) = o1 + (i - 1)*d1
        end do
        do i = 1, nn1
            xx(i) = oo1 + (i - 1)*dd1
        end do
        select case (interp_method)
            case ('nearest')
                call interp_nearest_1d_double(n1, x, f, nn1, xx, ff)
            case ('linear')
                call interp_linear_1d_double(n1, x, f, nn1, xx, ff)
            case ('sinc')
                call interp_sinc_1d_double(n1, x, f, nn1, xx, ff)
            case ('cubic')
                call interp_cspline_1d_double(n1, x, f, nn1, xx, ff)
            case ('pchip')
                call interp_pchip_1d_double(n1, x, f, nn1, xx, ff)
            case ('quintic')
                call interp_quintic_1d_double(n1, x, f, nn1, xx, ff)
            case ('mba')
                call interp_mba_1d_double(n1, x, f, nn1, xx, ff)
            case ('biharmonic')
                call interp_biharmonic_1d_double(n1, x, f, nn1, xx, ff)
            case ('cubic_spline')
                call interp_cubic_spline_1d_double(n1, x, f, nn1, xx, ff, method='c2')
            case ('hermite_spline')
                call interp_cubic_spline_1d_double(n1, x, f, nn1, xx, ff, method='hermite')
            case ('monotonic_spline')
                call interp_cubic_spline_1d_double(n1, x, f, nn1, xx, ff, method='monotonic')
            case default
                call interp_linear_1d_double(n1, x, f, nn1, xx, ff)
        end select
    end if
end function reg_to_reg_interp_1d_double
function reg_to_reg_interp_2d_double(f, n, d, o, nn, dd, oo, method) result(ff)
    double precision, dimension(:, :) :: f
    integer, dimension(:) :: n, nn
    double precision, dimension(1:2) :: d, dd, o, oo
    character(len=*), dimension(1:2), optional :: method
    double precision, allocatable, dimension(:, :) :: ff
    character(len=24), dimension(1:2) :: interp_method
    integer :: i, j
    double precision, allocatable, dimension(:, :) :: w
    call assert(all(d > 0), ' <reg_to_reg_interp_2d_> Error: All original intervals must be > 0')
    call assert(all(dd > 0), ' <reg_to_reg_interp_2d_> Error: All resampling intervals must be > 0')
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear']
    end if
    allocate (w(1:nn(1), 1:n(2)))
    allocate (ff(1:nn(1), 1:nn(2)))
    ! Interpolate along the 1st dimension
    !$omp parallel do private(j)
    do j = 1, n(2)
        w(:, j) = reg_to_reg_interp_1d_double(f(:, j), &
            n(1), d(1), o(1), nn(1), dd(1), oo(1), interp_method(1))
    end do
    !$omp end parallel do
    ! Interpolate along the 2nd dimension
    !$omp parallel do private(i)
    do i = 1, nn(1)
        ff(i, :) = reg_to_reg_interp_1d_double(w(i, :), &
            n(2), d(2), o(2), nn(2), dd(2), oo(2), interp_method(2))
    end do
    !$omp end parallel do
end function reg_to_reg_interp_2d_double
function reg_to_reg_interp_3d_double(f, n, d, o, nn, dd, oo, method) result(ff)
    double precision, dimension(:, :, :) :: f
    integer, dimension(:) :: n, nn
    double precision, dimension(1:3) :: d, dd, o, oo
    character(len=*), dimension(:), optional :: method
    double precision, allocatable, dimension(:, :, :) :: ff
    character(len=24), dimension(1:3) :: interp_method
    integer :: i, j, k
    double precision, allocatable, dimension(:, :, :) :: w1, w2
    call assert(all(d > 0), ' <reg_to_reg_interp_3d_> Error: All original intervals must be > 0')
    call assert(all(dd > 0), ' <reg_to_reg_interp_3d_> Error: All resampling intervals must be > 0')
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear', 'linear']
    end if
    allocate (w1(1:nn(1), 1:n(2), 1:n(3)))
    allocate (w2(1:nn(1), 1:nn(2), 1:n(3)))
    allocate (ff(1:nn(1), 1:nn(2), 1:nn(3)))
    ! Interpolate along the 1st dimension
    !$omp parallel do private(j, k) collapse(2)
    do k = 1, n(3)
        do j = 1, n(2)
            w1(:, j, k) = reg_to_reg_interp_1d_double(f(:, j, k), &
                n(1), d(1), o(1), nn(1), dd(1), oo(1), interp_method(1))
        end do
    end do
    !$omp end parallel do
    ! Interpolate along the 2nd dimension
    !$omp parallel do private(i, k) collapse(2)
    do k = 1, n(3)
        do i = 1, nn(1)
            w2(i, :, k) = reg_to_reg_interp_1d_double(w1(i, :, k), &
                n(2), d(2), o(2), nn(2), dd(2), oo(2), interp_method(2))
        end do
    end do
    !$omp end parallel do
    ! Interpolate along the 3rd dimension
    !$omp parallel do private(i, j) collapse(2)
    do j = 1, nn(2)
        do i = 1, nn(1)
            ff(i, j, :) = reg_to_reg_interp_1d_double(w2(i, j, :), &
                n(3), d(3), o(3), nn(3), dd(3), oo(3), interp_method(3))
        end do
    end do
    !$omp end parallel do
end function reg_to_reg_interp_3d_double
!
!> Interpolate a regularly sampled data using ratios; the new dimensions will be old dimensions / ratios
!
function resample_1d_double(f, r, method) result(ff)
    double precision, dimension(:), intent(in) :: f
    real, intent(in) :: r
    character(len=*), intent(in), optional :: method
    double precision, allocatable, dimension(:) :: ff
    character(len=24) :: interp_method
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'linear'
    end if
    call assert(r > 0, ' <resample_1d> Error: Resampling ratio must > 0.')
    ff = reg_to_reg_interp_1d_double(f, size(f), dble(1.0), dble(0.0), &
        nint(r*(size(f) - 1) + 1), dble(1.0/r), dble(0.0), interp_method)
end function resample_1d_double
function resample_2d_double(f, r, method) result(ff)
    double precision, dimension(:, :), intent(in) :: f
    real, dimension(:), intent(in) :: r
    character(len=*), dimension(:), intent(in), optional :: method
    double precision, allocatable, dimension(:, :) :: ff
    character(len=24), dimension(1:2) :: interp_method
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear']
    end if
    call assert(all(r > 0), ' <resample_2d> Error: Resampling ratio must > 0.')
    ff = reg_to_reg_interp_2d_double(f, shape(f), dble([1.0, 1.0]), dble([0.0, 0.0]), &
        nint(r*(shape(f) - 1) + 1), dble(1.0/r), dble([0.0, 0.0]), interp_method)
end function resample_2d_double
function resample_3d_double(f, r, method) result(ff)
    double precision, dimension(:, :, :), intent(in) :: f
    real, dimension(:), intent(in) :: r
    character(len=*), dimension(:), intent(in), optional :: method
    double precision, allocatable, dimension(:, :, :) :: ff
    character(len=24), dimension(1:3) :: interp_method
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear', 'linear']
    end if
    call assert(all(r > 0), ' <resample_3d> Error: Resampling ratio must > 0.')
    ff = reg_to_reg_interp_3d_double(f, shape(f), dble([1.0, 1.0, 1.0]), dble([0.0, 0.0, 0.0]), &
        nint(r*(shape(f) - 1) + 1), dble(1.0/r), dble([0.0, 0.0, 0.0]), interp_method)
end function resample_3d_double
!
!> Interpolate a regularly sampled data like another array
!
function interp_like_1d_double(f, ff, method) result(g)
    double precision, dimension(:), intent(in) :: f, ff
    character(len=*), intent(in), optional :: method
    double precision, allocatable, dimension(:) :: g
    character(len=24) :: interp_method
    integer :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'linear'
    end if
    n = size(f)
    nn = size(ff)
    g = reg_to_reg_interp_1d_double(f, n, dble(1.0), dble(0.0), &
        nn, dble((n - 1.0)/(nn - 1.0)), dble(0.0), interp_method)
end function interp_like_1d_double
function interp_like_2d_double(f, ff, method) result(g)
    double precision, dimension(:, :), intent(in) :: f, ff
    character(len=*), dimension(:), intent(in), optional :: method
    double precision, allocatable, dimension(:, :) :: g
    character(len=24), dimension(1:2) :: interp_method
    integer, dimension(1:2) :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear']
    end if
    n = shape(f)
    nn = shape(ff)
    g = reg_to_reg_interp_2d_double(f, n, dble([1.0, 1.0]), dble([0.0, 0.0]), &
        nn, dble((n - 1.0)/(nn - 1.0)), dble([0.0, 0.0]), interp_method)
end function interp_like_2d_double
function interp_like_3d_double(f, ff, method) result(g)
    double precision, dimension(:, :, :), intent(in) :: f, ff
    character(len=*), dimension(:), intent(in), optional :: method
    double precision, allocatable, dimension(:, :, :) :: g
    character(len=24), dimension(1:3) :: interp_method
    integer, dimension(1:3) :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear', 'linear']
    end if
    n = shape(f)
    nn = shape(ff)
    g = reg_to_reg_interp_3d_double(f, n, dble([1.0, 1.0, 1.0]), dble([0.0, 0.0, 0.0]), &
        nn, dble((n - 1.0)/(nn - 1.0)), dble([0.0, 0.0, 0.0]), interp_method)
end function interp_like_3d_double
!
!> Interpolate from regularly sampled data based on target dimensions
!
function interp_to_1d_double(f, nn, method) result(g)
    double precision, dimension(:), intent(in) :: f
    integer, intent(in) :: nn
    character(len=*), intent(in), optional :: method
    double precision, allocatable, dimension(:) :: g
    character(len=24) :: interp_method
    integer :: n
    double precision :: dd
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'linear'
    end if
    n = size(f)
    if (n == 1 .or. nn == 1) then
        dd = dble(1.0)
    else
        dd = dble((n - 1.0)/(nn - 1.0))
    end if
    g = reg_to_reg_interp_1d_double(f, n, dble(1.0), dble(0.0), &
        nn, dd, dble(0.0), interp_method)
end function interp_to_1d_double
function interp_to_2d_double(f, nn, method) result(g)
    double precision, dimension(:, :), intent(in) :: f
    integer, dimension(1:2), intent(in) :: nn
    character(len=*), dimension(:), intent(in), optional :: method
    double precision, allocatable, dimension(:, :) :: g
    character(len=24), dimension(1:2) :: interp_method
    integer, dimension(1:2) :: n
    double precision, dimension(1:2) :: dd
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear']
    end if
    n = shape(f)
    dd = dble((n - 1.0)/(nn - 1.0))
    where (n == 1 .or. nn == 1)
        dd = dble(1.0)
    end where
    g = reg_to_reg_interp_2d_double(f, n, dble([1.0, 1.0]), dble([0.0, 0.0]), &
        nn, dd, dble([0.0, 0.0]), interp_method)
end function interp_to_2d_double
function interp_to_3d_double(f, nn, method) result(g)
    double precision, dimension(:, :, :), intent(in) :: f
    integer, dimension(1:3), intent(in) :: nn
    character(len=*), dimension(:), intent(in), optional :: method
    double precision, allocatable, dimension(:, :, :) :: g
    character(len=24), dimension(1:3) :: interp_method
    integer, dimension(1:3) :: n
    double precision, dimension(1:3) :: dd
    if (present(method)) then
        interp_method = method
    else
        interp_method = ['linear', 'linear', 'linear']
    end if
    n = shape(f)
    dd = dble((n - 1.0)/(nn - 1.0))
    where (n == 1 .or. nn == 1)
        dd = dble(1.0)
    end where
    g = reg_to_reg_interp_3d_double(f, n, dble([1.0, 1.0, 1.0]), dble([0.0, 0.0, 0.0]), &
        nn, dd, dble([0.0, 0.0, 0.0]), interp_method)
end function interp_to_3d_double
!
!> Interpolate from irregularly sampled data to irregularly sampled data
!
function irreg_to_irreg_interp_1d_double(x, f, xx, method) result(ff)
    double precision, dimension(:), intent(in) :: x, xx
    double precision, dimension(:), intent(in) :: f
    double precision, allocatable, dimension(:) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n = size(x)
    nn = size(xx)
    allocate (ff(1:nn))
    select case (interp_method)
        case ('nearest')
            call interp_nearest_1d_double(n, x, f, nn, xx, ff)
        case ('linear')
            call interp_linear_1d_double(n, x, f, nn, xx, ff)
        case ('cubic')
            call interp_cspline_1d_double(n, x, f, nn, xx, ff)
        case ('pchip')
            call interp_pchip_1d_double(n, x, f, nn, xx, ff)
        case ('quintic')
            call interp_quintic_1d_double(n, x, f, nn, xx, ff)
        case ('mba')
            call interp_mba_1d_double(n, x, f, nn, xx, ff)
        case ('biharmonic')
            call interp_biharmonic_1d_double(n, x, f, nn, xx, ff)
        case ('cubic_spline')
            call interp_cubic_spline_1d_double(n, x, f, nn, xx, ff, method='c2')
        case ('hermite_spline')
            call interp_cubic_spline_1d_double(n, x, f, nn, xx, ff, method='hermite')
        case ('monotonic_spline')
            call interp_cubic_spline_1d_double(n, x, f, nn, xx, ff, method='monotonic')
        case default
            call interp_linear_1d_double(n, x, f, nn, xx, ff)
    end select
end function irreg_to_irreg_interp_1d_double
function irreg_to_irreg_interp_2d_double(x, y, f, xx, yy, method) result(ff)
    double precision, dimension(:), intent(in) :: x, y, xx, yy
    double precision, dimension(:), intent(in) :: f
    double precision, allocatable, dimension(:) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n = size(x)
    nn = size(xx)
    allocate (ff(1:nn))
    select case (interp_method)
        case ('nearest')
            call interp_nearest_2d_double(n, x, y, f, nn, xx, yy, ff)
        case ('mba')
            call interp_mba_2d_double(n, x, y, f, nn, xx, yy, ff)
        case ('biharmonic')
            call interp_biharmonic_2d_double(n, x, y, f, nn, xx, yy, ff)
    end select
end function irreg_to_irreg_interp_2d_double
function irreg_to_irreg_interp_3d_double(x, y, z, f, xx, yy, zz, method) result(ff)
    double precision, dimension(:) :: x, y, z, xx, yy, zz
    double precision, dimension(:) :: f
    double precision, allocatable, dimension(:) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: n, nn
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n = size(x)
    nn = size(xx)
    allocate (ff(1:nn))
    select case (interp_method)
        case ('nearest')
            call interp_nearest_3d_double(n, x, y, z, f, nn, xx, yy, zz, ff)
        case ('mba')
            call interp_mba_3d_double(n, x, y, z, f, nn, xx, yy, zz, ff)
        case ('biharmonic')
            call interp_biharmonic_3d_double(n, x, y, z, f, nn, xx, yy, zz, ff)
    end select
end function irreg_to_irreg_interp_3d_double
!
!> Generate mesh grids
!
function meshgrid_double(n, d, o, dim) result(g)
    integer, intent(in) :: n(:)
    double precision, intent(in) :: o(:), d(:)
    integer, intent(in) :: dim
    double precision, allocatable, dimension(:) :: g
    integer :: i, j, k, l, n1, n2, n3
    double precision :: o1, o2, o3, d1, d2, d3
    select case (size(n))
        case (1)
            g = zeros(n(1))
            do i = 1, n(1)
                g(i) = o(1) + (i - 1)*d(1)
            end do
        case (2)
            n1 = n(1)
            n2 = n(2)
            o1 = o(1)
            o2 = o(2)
            d1 = d(1)
            d2 = d(2)
            allocate (g(1:n1*n2))
            select case (dim)
                case (1)
                    l = 1
                    do j = 1, n2
                        do i = 1, n1
                            g(l) = o1 + (i - 1)*d1
                            l = l + 1
                        end do
                    end do
                case (2)
                    l = 1
                    do j = 1, n2
                        do i = 1, n1
                            g(l) = o2 + (j - 1)*d2
                            l = l + 1
                        end do
                    end do
            end select
        case (3)
            n1 = n(1)
            n2 = n(2)
            n3 = n(3)
            o1 = o(1)
            o2 = o(2)
            o3 = o(3)
            d1 = d(1)
            d2 = d(2)
            d3 = d(3)
            allocate (g(1:n1*n2*n3))
            select case (dim)
                case (1)
                    l = 1
                    do k = 1, n3
                        do j = 1, n2
                            do i = 1, n1
                                g(l) = o1 + (i - 1)*d1
                                l = l + 1
                            end do
                        end do
                    end do
                case (2)
                    l = 1
                    do k = 1, n3
                        do j = 1, n2
                            do i = 1, n1
                                g(l) = o2 + (j - 1)*d2
                                l = l + 1
                            end do
                        end do
                    end do
                case (3)
                    l = 1
                    do k = 1, n3
                        do j = 1, n2
                            do i = 1, n1
                                g(l) = o3 + (k - 1)*d3
                                l = l + 1
                            end do
                        end do
                    end do
            end select
    end select
end function meshgrid_double
!
!> Generate mesh grids from vectors; the vectors may be irregularly sampled
!
function meshgrid_1d_double(v) result(g)
    double precision, dimension(:) :: v
    double precision, allocatable, dimension(:) :: g
    g = v
end function meshgrid_1d_double
function meshgrid_2d_double(v1, v2) result(g)
    double precision, dimension(:) :: v1, v2
    double precision, allocatable, dimension(:, :) :: g
    integer :: n1, n2, l, i, j
    n1 = size(v1)
    n2 = size(v2)
    g = zeros(n1*n2, 2)
    l = 1
    do j = 1, n2
        do i = 1, n1
            g(l, :) = [v1(i), v2(j)]
            l = l + 1
        end do
    end do
end function meshgrid_2d_double
function meshgrid_3d_double(v1, v2, v3) result(g)
    double precision, dimension(:) :: v1, v2, v3
    double precision, allocatable, dimension(:, :) :: g
    integer :: n1, n2, n3, l, i, j, k
    n1 = size(v1)
    n2 = size(v2)
    n3 = size(v3)
    g = zeros(n1*n2*n3, 3)
    l = 1
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                g(l, :) = [v1(i), v2(j), v3(k)]
                l = l + 1
            end do
        end do
    end do
end function meshgrid_3d_double
!
!> Interpolate from irregularly sampled data to regularly sampled data
!
function irreg_to_reg_interp_1d_double(x, f, n, d, o, method) result(ff)
    double precision, dimension(:) :: x
    double precision, dimension(:) :: f
    integer :: n
    double precision :: d, o
    double precision, allocatable, dimension(:) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: l
    double precision, allocatable, dimension(:) :: xx
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    l = size(x)
    call assert(n > 0 .and. d > 0, ' <irreg_to_reg_interp_1d> Error: n and d must > 0. ')
    xx = regspace(o, d, o + (n - 1)*d)
    ff = zeros(n)
    select case (interp_method)
        case ('nearest')
            call interp_nearest_1d_double(l, x, f, n, xx, ff)
        case ('linear')
            call interp_linear_1d_double(l, x, f, n, xx, ff)
        case ('cubic')
            call interp_cspline_1d_double(l, x, f, n, xx, ff)
        case ('pchip')
            call interp_pchip_1d_double(l, x, f, n, xx, ff)
        case ('quintic')
            call interp_quintic_1d_double(l, x, f, n, xx, ff)
        case ('mba')
            call interp_mba_1d_double(l, x, f, n, xx, ff)
        case ('biharmonic')
            call interp_biharmonic_1d_double(l, x, f, n, xx, ff)
        case ('cubic_spline')
            call interp_cubic_spline_1d_double(l, x, f, n, xx, ff, method='c2')
        case ('hermite_spline')
            call interp_cubic_spline_1d_double(l, x, f, n, xx, ff, method='hermite')
        case ('monotonic_spline')
            call interp_cubic_spline_1d_double(l, x, f, n, xx, ff, method='monotonic')
        case default
            call interp_linear_1d_double(l, x, f, n, xx, ff)
    end select
end function irreg_to_reg_interp_1d_double
function irreg_to_reg_interp_2d_double(x, y, f, n, d, o, method) result(ff)
    double precision, dimension(:) :: x, y
    double precision, dimension(:) :: f
    integer, dimension(1:2) :: n
    double precision, dimension(1:2) :: d, o
    double precision, allocatable, dimension(:, :) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: nn, l
    double precision, allocatable, dimension(:) :: xx, yy
    double precision, allocatable, dimension(:) :: tf
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    l = size(x)
    call assert(all(n > 0) .and. all(d > 0), ' <irreg_to_reg_interp_2d> Error: All n and d must > 0. ')
    xx = meshgrid(n=n, d=d, o=o, dim=1)
    yy = meshgrid(n=n, d=d, o=o, dim=2)
    nn = product(n)
    tf = zeros(nn)
    select case (interp_method)
        case ('nearest')
            call interp_nearest_2d_double(l, x, y, f, nn, xx, yy, tf)
        case ('mba')
            call interp_mba_2d_double(l, x, y, f, nn, xx, yy, tf)
        case ('biharmonic')
            call interp_biharmonic_2d_double(l, x, y, f, nn, xx, yy, tf)
    end select
    ff = reshape(tf, n)
end function irreg_to_reg_interp_2d_double
function irreg_to_reg_interp_3d_double(x, y, z, f, n, d, o, method) result(ff)
    double precision, dimension(:) :: x, y, z
    double precision, dimension(:) :: f
    integer, dimension(1:3) :: n
    double precision, dimension(1:3) :: d, o
    double precision, allocatable, dimension(:, :, :) :: ff
    character(len=*), optional :: method
    character(len=24) :: interp_method
    integer :: nn, l
    double precision, allocatable, dimension(:) :: xx, yy, zz
    double precision, allocatable, dimension(:) :: tf
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    l = size(x)
    call assert(all(n > 0) .and. all(d > 0), ' <irreg_to_reg_interp_3d> Error: All n and d must > 0. ')
    xx = meshgrid(n=n, d=d, o=o, dim=1)
    yy = meshgrid(n=n, d=d, o=o, dim=2)
    zz = meshgrid(n=n, d=d, o=o, dim=3)
    nn = product(n)
    tf = zeros(nn)
    select case (interp_method)
        case ('nearest')
            call interp_nearest_3d_double(l, x, y, z, f, nn, xx, yy, zz, tf)
        case ('mba')
            call interp_mba_3d_double(l, x, y, z, f, nn, xx, yy, zz, tf)
        case ('biharmonic')
            call interp_biharmonic_3d_double(l, x, y, z, f, nn, xx, yy, zz, tf)
    end select
    ff = reshape(tf, n)
end function irreg_to_reg_interp_3d_double
!
!> Remove NaN values from regularly sampled data
!
function inpaint_1d_double(w, method) result(ww)
    double precision, dimension(:), intent(in) :: w
    character(len=*), optional :: method
    double precision, allocatable, dimension(:) :: ww
    character(len=24) :: interp_method
    integer :: n1, n, nn
    integer :: i, l, h
    double precision, allocatable, dimension(:) :: x1, xx1
    double precision, allocatable, dimension(:) :: f, ff
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n1 = size(w)
    ! Find NaN pixels
    n = count(.not. isnan(w))
    nn = n1
    x1 = zeros(n)
    f = zeros(n)
    xx1 = zeros(nn)
    ff = zeros(nn)
    l = 1
    h = 1
    do i = 1, n1
        if (.not. isnan(w(i))) then
            x1(l) = i - 1.0
            f(l) = w(i)
            l = l + 1
        end if
        xx1(h) = i - 1.0
        h = h + 1
    end do
    select case (interp_method)
        case ('nearest')
            call interp_nearest_1d_double(n, x1, f, nn, xx1, ff)
        case ('linear')
            call interp_linear_1d_double(n, x1, f, nn, xx1, ff)
        case ('sinc')
            call interp_sinc_1d_double(n, x1, f, nn, xx1, ff)
        case ('cubic')
            call interp_cspline_1d_double(n, x1, f, nn, xx1, ff)
        case ('pchip')
            call interp_pchip_1d_double(n, x1, f, nn, xx1, ff)
        case ('quintic')
            call interp_quintic_1d_double(n, x1, f, nn, xx1, ff)
        case ('mba')
            call interp_mba_1d_double(n, x1, f, nn, xx1, ff)
        case ('biharmonic')
            call interp_biharmonic_1d_double(n, x1, f, nn, xx1, ff)
        case ('cubic_spline')
            call interp_cubic_spline_1d_double(n, x1, f, nn, xx1, ff, method='c2')
        case ('hermite_spline')
            call interp_cubic_spline_1d_double(n, x1, f, nn, xx1, ff, method='hermite')
        case ('monotonic_spline')
            call interp_cubic_spline_1d_double(n, x1, f, nn, xx1, ff, method='monotonic')
        case default
            call interp_linear_1d_double(n, x1, f, nn, xx1, ff)
    end select
    ww = ff
end function inpaint_1d_double
function inpaint_2d_double(w, method) result(ww)
    double precision, dimension(:, :), intent(in) :: w
    character(len=*), optional :: method
    double precision, allocatable, dimension(:, :) :: ww
    character(len=24) :: interp_method
    integer :: n1, n2, n, nn
    integer :: i, j, l, h
    double precision, allocatable, dimension(:) :: x1, xx1, x2, xx2
    double precision, allocatable, dimension(:) :: f, ff
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Find NaN pixels
    n = count(.not. isnan(w))
    nn = size(w)
    x1 = zeros(n)
    x2 = zeros(n)
    f = zeros(n)
    xx1 = zeros(nn)
    xx2 = zeros(nn)
    ff = zeros(nn)
    l = 1
    h = 1
    do j = 1, n2
        do i = 1, n1
            if (.not. isnan(w(i, j))) then
                x1(l) = i - 1.0
                x2(l) = j - 1.0
                f(l) = w(i, j)
                l = l + 1
            end if
            xx1(h) = i - 1.0
            xx2(h) = j - 1.0
            h = h + 1
        end do
    end do
    select case (interp_method)
        case ('nearest')
            call interp_nearest_2d_double(n, x1, x2, f, nn, xx1, xx2, ff)
        case ('mba')
            call interp_mba_2d_double(n, x1, x2, f, nn, xx1, xx2, ff)
        case ('biharmonic')
            call interp_biharmonic_2d_double(n, x1, x2, f, nn, xx1, xx2, ff)
    end select
    ww = reshape(ff, [n1, n2])
end function inpaint_2d_double
function inpaint_3d_double(w, method) result(ww)
    double precision, dimension(:, :, :), intent(in) :: w
    character(len=*), optional :: method
    double precision, allocatable, dimension(:, :, :) :: ww
    character(len=24) :: interp_method
    integer :: n1, n2, n3, n, nn
    integer :: i, j, k, l, h
    double precision, allocatable, dimension(:) :: x1, xx1, x2, xx2, x3, xx3
    double precision, allocatable, dimension(:) :: f, ff
    if (present(method)) then
        interp_method = method
    else
        interp_method = 'mba'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Find NaN pixels
    n = count(.not. isnan(w))
    nn = size(w)
    x1 = zeros(n)
    x2 = zeros(n)
    x3 = zeros(n)
    f = zeros(n)
    xx1 = zeros(nn)
    xx2 = zeros(nn)
    xx3 = zeros(nn)
    ff = zeros(nn)
    l = 1
    h = 1
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                if (.not. isnan(w(i, j, k))) then
                    x1(l) = i - 1.0
                    x2(l) = j - 1.0
                    x3(l) = k - 1.0
                    f(l) = w(i, j, k)
                    l = l + 1
                end if
                xx1(h) = i - 1.0
                xx2(h) = j - 1.0
                xx3(h) = k - 1.0
                h = h + 1
            end do
        end do
    end do
    select case (interp_method)
        case ('nearest')
            call interp_nearest_3d_double(n, x1, x2, x3, f, nn, xx1, xx2, xx3, ff)
        case ('mba')
            call interp_mba_3d_double(n, x1, x2, x3, f, nn, xx1, xx2, xx3, ff)
        case ('biharmonic')
            call interp_biharmonic_3d_double(n, x1, x2, x3, f, nn, xx1, xx2, xx3, ff)
    end select
    ww = reshape(ff, [n1, n2, n3])
end function inpaint_3d_double
function point_interp_linear_1d_double(x, f, xx) result(ff)
    double precision, dimension(:) :: x
    double precision, dimension(:) :: f
    double precision :: xx, ff
    double precision, dimension(1:2) :: w
    integer :: i
    do i = 1, 2
        w(i) = abs(x(3 - i) - xx)/(x(2) - x(1))
    end do
    ff = sum(w*f)
end function point_interp_linear_1d_double
function point_interp_linear_2d_double(x, y, f, xx, yy) result(ff)
    double precision, dimension(:) :: x, y
    double precision, dimension(:, :) :: f
    double precision :: xx, yy, ff
    double precision, dimension(1:2, 1:2) :: w
    integer :: i, j
    do i = 1, 2
        do j = 1, 2
            w(i, j) = abs((x(3 - i) - xx)*(y(3 - j) - yy))/((x(2) - x(1))*(y(2) - y(1)))
        end do
    end do
    ff = sum(w*f)
end function point_interp_linear_2d_double
function point_interp_linear_3d_double(x, y, z, f, xx, yy, zz) result(ff)
    double precision, dimension(:) :: x, y, z
    double precision, dimension(:, :, :) :: f
    double precision :: xx, yy, zz, ff
    double precision, dimension(1:2, 1:2, 1:2) :: w
    integer :: i, j, k
    do i = 1, 2
        do j = 1, 2
            do k = 1, 2
                w(i, j, k) = abs((x(3 - i) - xx)*(y(3 - j) - yy)*(z(3 - k) - zz)) &
                    /((x(2) - x(1))*(y(2) - y(1))*(z(2) - z(1)))
            end do
        end do
    end do
    ff = sum(w*f)
end function point_interp_linear_3d_double
function point_interp_barycentric_2d_double(v1, v2, v3, f, p) result(ff)
    double precision, dimension(:) :: v1, v2, v3, f, p
    double precision :: ff
    double precision, dimension(1:3) :: w
    double precision :: a123
    a123 = area(v1, v2, v3)
    if (abs(a123) < 1e-6) then
        print *, ' <point_interp_barycentric_2d> Error: The points form a degenerate triangle! '
        stop
    end if
    ! Note the circulant order
    w(1) = area(p, v2, v3)/a123
    w(2) = area(v1, p, v3)/a123
    w(3) = area(v1, v2, p)/a123
    if (any(w < 0)) then
        ! print *, ' <point_interp_barycentric_2d> Warning: The point is outside of the triangle! '
        ff = 0
        return
    end if
    ff = sum(w*f)
contains
    function area(v1, v2, v3) result(a)
        double precision, dimension(:) :: v1, v2, v3
        double precision :: a
        a = det(transpose(reshape([v2 - v1, v3 - v1], [2, 2])))
    end function area
end function point_interp_barycentric_2d_double
function point_interp_barycentric_3d_double(v1, v2, v3, v4, f, p) result(ff)
    double precision, dimension(:) :: v1, v2, v3, v4, f, p
    double precision :: ff
    double precision, dimension(1:4) :: w
    double precision :: vol1234
    ! Calculate the determinant for the tetrahedron volume
    vol1234 = vol(v1, v2, v3, v4)
    if (abs(vol1234) < 1e-6) then
        print *, ' <point_interp_barycentric_3d> Error: The points form a degenerate tetrahedron. '
        stop
    end if
    ! Calculate the determinants for sub-tetrahedra
    ! Note the circulant order
    w(1) = vol(p, v2, v3, v4)/vol1234
    w(2) = vol(v1, p, v3, v4)/vol1234
    w(3) = vol(v1, v2, p, v4)/vol1234
    w(4) = vol(v1, v2, v3, p)/vol1234
    if (any(w < 0)) then
        ! print *, ' <point_interp_barycentric_3d> Warning: The point is outside of the tetrahedron! '
        ff = 0
        return
    end if
    ff = sum(w*f)
contains
    function vol(v1, v2, v3, v4) result(v)
        double precision, dimension(:) :: v1, v2, v3, v4
        double precision :: v
        v = det(transpose(reshape([v2 - v1, v3 - v1, v4 - v1], [3, 3])))
    end function vol
end function point_interp_barycentric_3d_double
! external
end module libflit_interp
