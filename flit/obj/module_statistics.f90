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
module libflit_statistics
    use libflit_array
    use libflit_sort
    use libflit_constants
    use libflit_linear_algebra
    use libflit_transform
    use libflit_error
    use iso_fortran_env
    use libflit_array_operation
    implicit none
    ! Cross- and auto-correlation
    interface xcorrd
        module procedure :: discrete_xcorr_1d_float
        module procedure :: discrete_xcorr_2d_float
        module procedure :: discrete_xcorr_3d_float
        module procedure :: discrete_xcorr_1d_double
        module procedure :: discrete_xcorr_2d_double
        module procedure :: discrete_xcorr_3d_double
    end interface xcorrd
    interface acorrd
        module procedure :: discrete_acorr_1d_float
        module procedure :: discrete_acorr_2d_float
        module procedure :: discrete_acorr_3d_float
        module procedure :: discrete_acorr_1d_double
        module procedure :: discrete_acorr_2d_double
        module procedure :: discrete_acorr_3d_double
    end interface acorrd
    interface xcorr
        module procedure :: xcorr_1d_float
        module procedure :: xcorr_2d_float
        module procedure :: xcorr_3d_float
        module procedure :: xcorr_1d_double
        module procedure :: xcorr_2d_double
        module procedure :: xcorr_3d_double
    end interface xcorr
    interface acorr
        module procedure :: acorr_1d_float
        module procedure :: acorr_2d_float
        module procedure :: acorr_3d_float
        module procedure :: acorr_1d_double
        module procedure :: acorr_2d_double
        module procedure :: acorr_3d_double
    end interface acorr
    interface lxcorr
        module procedure :: local_xcorr_1d_float
        module procedure :: local_xcorr_1d_double
    end interface lxcorr
    interface xcorr_coef
        module procedure :: xcorr_coef_1d_float
        module procedure :: xcorr_coef_2d_float
        module procedure :: xcorr_coef_3d_float
        module procedure :: xcorr_coef_1d_double
        module procedure :: xcorr_coef_2d_double
        module procedure :: xcorr_coef_3d_double
    end interface xcorr_coef
    ! Mean
    interface mean
        module procedure :: mean_1d_float
        module procedure :: mean_2d_float
        module procedure :: mean_3d_float
        module procedure :: mean_1d_double
        module procedure :: mean_2d_double
        module procedure :: mean_3d_double
        module procedure :: mean_1d_complex
        module procedure :: mean_2d_complex
        module procedure :: mean_3d_complex
        module procedure :: mean_1d_dcomplex
        module procedure :: mean_2d_dcomplex
        module procedure :: mean_3d_dcomplex
    end interface mean
    ! Median
    interface median
        module procedure :: median_1d_float
        module procedure :: median_2d_float
        module procedure :: median_3d_float
        module procedure :: median_1d_double
        module procedure :: median_2d_double
        module procedure :: median_3d_double
    end interface median
    ! Standard deviation
    interface std
        module procedure :: standard_deviation_1d_float
        module procedure :: standard_deviation_2d_float
        module procedure :: standard_deviation_3d_float
        module procedure :: standard_deviation_1d_double
        module procedure :: standard_deviation_2d_double
        module procedure :: standard_deviation_3d_double
    end interface std
    ! Covariance
    interface covar
        module procedure :: covariance_1d_float
        module procedure :: covariance_2d_float
        module procedure :: covariance_3d_float
        module procedure :: covariance_1d_double
        module procedure :: covariance_2d_double
        module procedure :: covariance_3d_double
    end interface covar
    ! Compute histogram
    interface histogram
        module procedure :: histogram_1d_float
        module procedure :: histogram_2d_float
        module procedure :: histogram_3d_float
        module procedure :: histogram_1d_double
        module procedure :: histogram_2d_double
        module procedure :: histogram_3d_double
    end interface histogram
    ! Plot histogram
    interface plot_histogram
        module procedure :: plot_histogram_1d_float
        module procedure :: plot_histogram_2d_float
        module procedure :: plot_histogram_3d_float
        module procedure :: plot_histogram_1d_double
        module procedure :: plot_histogram_2d_double
        module procedure :: plot_histogram_3d_double
    end interface plot_histogram
    ! Gaussian pdf
    interface gaussian
        module procedure :: gaussian_1d_float
        module procedure :: gaussian_2d_float
        module procedure :: gaussian_3d_float
        module procedure :: gaussian_1d_double
        module procedure :: gaussian_2d_double
        module procedure :: gaussian_3d_double
    end interface gaussian
    interface cauchy
        module procedure :: cauchy_1d_float
        module procedure :: cauchy_2d_float
        module procedure :: cauchy_3d_float
        module procedure :: cauchy_1d_double
        module procedure :: cauchy_2d_double
        module procedure :: cauchy_3d_double
    end interface cauchy
    ! Covariance matrix
    interface covar_matrix
        module procedure :: covariance_matrix_1d_float
        module procedure :: covariance_matrix_2d_float
        module procedure :: covariance_matrix_3d_float
        module procedure :: cross_covariance_matrix_1d_float
        module procedure :: cross_covariance_matrix_2d_float
        module procedure :: cross_covariance_matrix_3d_float
        module procedure :: row_covariance_matrix_2d_float
        module procedure :: row_cross_covariance_matrix_2d_float
        module procedure :: covariance_matrix_1d_double
        module procedure :: covariance_matrix_2d_double
        module procedure :: covariance_matrix_3d_double
        module procedure :: cross_covariance_matrix_1d_double
        module procedure :: cross_covariance_matrix_2d_double
        module procedure :: cross_covariance_matrix_3d_double
        module procedure :: row_covariance_matrix_2d_double
        module procedure :: row_cross_covariance_matrix_2d_double
    end interface covar_matrix
    ! Variance
    interface var
        module procedure :: variance_1d_float
        module procedure :: variance_2d_float
        module procedure :: variance_3d_float
        module procedure :: variance_1d_double
        module procedure :: variance_2d_double
        module procedure :: variance_3d_double
    end interface var
    ! Kernels
    interface kernel_triangular
        module procedure :: kernel_triangular_float
        module procedure :: kernel_triangular_double
    end interface kernel_triangular
    interface kernel_bisquare
        module procedure :: kernel_bisquare_float
        module procedure :: kernel_bisquare_double
    end interface kernel_bisquare
    interface kernel_trisquare
        module procedure :: kernel_trisquare_float
        module procedure :: kernel_trisquare_double
    end interface kernel_trisquare
    interface kernel_tricube
        module procedure :: kernel_tricube_float
        module procedure :: kernel_tricube_double
    end interface kernel_tricube
    interface kernel_epanechnikov
        module procedure :: kernel_epanechnikov_float
        module procedure :: kernel_epanechnikov_double
    end interface kernel_epanechnikov
    interface kernel_cosine
        module procedure :: kernel_cosine_float
        module procedure :: kernel_cosine_double
    end interface kernel_cosine
    private
    public :: xcorrd, acorrd
    public :: xcorr, acorr
    public :: mean
    public :: median
    public :: var, covar, covar_matrix
    public :: std
    public :: histogram, plot_histogram
    public :: lxcorr
    public :: xcorr_coef
    public :: gaussian
    public :: cauchy
    public :: kernel_triangular
    public :: kernel_bisquare
    public :: kernel_trisquare
    public :: kernel_tricube
    public :: kernel_epanechnikov
    public :: kernel_cosine
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
elemental function kernel_triangular_float(u) result(w)
    real, intent(in) :: u
    real :: w
    w = abs(u)
    if (w < 1) then
        w = 1 - w
    else
        w = 0
    end if
end function kernel_triangular_float
elemental function kernel_epanechnikov_float(u) result(w)
    real, intent(in) :: u
    real :: w
    w = abs(u)
    if (w < 1) then
        w = 0.75*(1 - w**2)
    else
        w = 0
    end if
end function kernel_epanechnikov_float
elemental function kernel_bisquare_float(u) result(w)
    real, intent(in) :: u
    real :: w
    w = abs(u)
    if (w < 1) then
        w = (1 - w**2)**2
    else
        w = 0
    end if
end function kernel_bisquare_float
elemental function kernel_trisquare_float(u) result(w)
    real, intent(in) :: u
    real :: w
    w = abs(u)
    if (w < 1) then
        w = (1 - w**2)**3
    else
        w = 0
    end if
end function kernel_trisquare_float
elemental function kernel_tricube_float(u) result(w)
    real, intent(in) :: u
    real :: w
    w = abs(u)
    if (w < 1) then
        w = (1 - w**3)**3
    else
        w = 0
    end if
end function kernel_tricube_float
elemental function kernel_cosine_float(u) result(w)
    real, intent(in) :: u
    real :: w
    w = abs(u)
    if (w < 1) then
        w = const_pi/4.0*cos(const_pi/2.0*w)
    else
        w = 0
    end if
end function kernel_cosine_float
!
! @brief Variance-covariance matrix of a vector x
!
function covariance_matrix_1d_float(x) result(c)
    real, dimension(:), intent(in) :: x
    real, allocatable, dimension(:, :) :: c
    c = ones(2, 2)*var(x)
end function covariance_matrix_1d_float
function covariance_matrix_2d_float(x) result(c)
    real, dimension(:, :), intent(in) :: x
    real, allocatable, dimension(:, :) :: c
    c = ones(2, 2)*var(x)
end function covariance_matrix_2d_float
function covariance_matrix_3d_float(x) result(c)
    real, dimension(:, :, :), intent(in) :: x
    real, allocatable, dimension(:, :) :: c
    c = ones(2, 2)*var(x)
end function covariance_matrix_3d_float
!
! @brief Variance-covariance matrix of n sets variants
! Each column of x is an observation/realization
!
function row_covariance_matrix_2d_float(x, rowvar) result(c)
    real, dimension(:, :), intent(in) :: x
    logical, intent(in) :: rowvar
    real, allocatable, dimension(:, :) :: c
    integer :: n, i, j
    if (rowvar) then
        n = size(x, 1)
        c = zeros(n, n)
        do j = 1, n
            do i = 1, n
                if (j >= i) then
                    c(i, j) = covar(x(i, :), x(j, :))
                end if
            end do
        end do
    else
        n = size(x, 2)
        c = zeros(n, n)
        do j = 1, n
            do i = 1, n
                if (j >= i) then
                    c(i, j) = covar(x(:, i), x(:, j))
                end if
            end do
        end do
    end if
    do j = 1, n
        do i = 1, n
            if (j < i) then
                c(i, j) = c(j, i)
            end if
        end do
    end do
end function row_covariance_matrix_2d_float
function cross_covariance_matrix_1d_float(x, y) result(c)
    real, dimension(:), intent(in) :: x, y
    real, allocatable, dimension(:, :) :: c
    c = zeros(2, 2)
    c(1, :) = [var(x), covar(x, y)]
    c(2, :) = [covar(y, x), var(y)]
end function cross_covariance_matrix_1d_float
function cross_covariance_matrix_2d_float(x, y) result(c)
    real, dimension(:, :), intent(in) :: x, y
    real, allocatable, dimension(:, :) :: c
    c = zeros(2, 2)
    c(1, :) = [var(x), covar(x, y)]
    c(2, :) = [covar(y, x), var(y)]
end function cross_covariance_matrix_2d_float
function cross_covariance_matrix_3d_float(x, y) result(c)
    real, dimension(:, :, :), intent(in) :: x, y
    real, allocatable, dimension(:, :) :: c
    c = zeros(2, 2)
    c(1, :) = [var(x), covar(x, y)]
    c(2, :) = [covar(y, x), var(y)]
end function cross_covariance_matrix_3d_float
function row_cross_covariance_matrix_2d_float(x, y, rowvar) result(c)
    real, dimension(:, :), intent(in) :: x, y
    logical, intent(in) :: rowvar
    real, allocatable, dimension(:, :) :: c
    integer :: n, i, j
    call assert(size(x, 1) == size(y, 1) .and. size(x, 2) == size(y, 2), &
        ' <cross_covariance_matrix_2d_> Error: x and y must have same shape.')
    if (rowvar) then
        n = size(x, 1)
        c = zeros(n, n)
        do j = 1, n
            do i = 1, n
                if (j >= i) then
                    c(i, j) = covar(x(i, :), y(j, :))
                end if
            end do
        end do
    else
        n = size(x, 2)
        c = zeros(n, n)
        do j = 1, n
            do i = 1, n
                if (j >= i) then
                    c(i, j) = covar(x(:, i), y(:, j))
                end if
            end do
        end do
    end if
    do j = 1, n
        do i = 1, n
            if (j < i) then
                c(i, j) = c(j, i)
            end if
        end do
    end do
end function row_cross_covariance_matrix_2d_float
!
!> Compute the variance of a 1D array
!
function variance_1d_float(w) result(s)
    real, dimension(:), intent(in) :: w
    real :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = s/(size(w) - 1)
    end if
end function variance_1d_float
!
!> Compute the variance of a 2D array
!
function variance_2d_float(w) result(s)
    real, dimension(:, :), intent(in) :: w
    real :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = s/(size(w) - 1)
    end if
end function variance_2d_float
!
!> Compute the variance of a 3D array
!
function variance_3d_float(w) result(s)
    real, dimension(:, :, :), intent(in) :: w
    real :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = s/(size(w) - 1)
    end if
end function variance_3d_float
!
!> 1D Gaussian
!
function gaussian_1d_float(x, mu, sigma) result(f)
    real, intent(in) :: mu, sigma
    real, dimension(:), intent(in) :: x
    real, allocatable, dimension(:) :: f
    ! The normalization factor is sigma*sqrt(2*const_pi)
    f = exp(-0.5*((x - mu)/sigma)**2)
end function gaussian_1d_float
!
!> 2D Gaussian
!
function gaussian_2d_float(x, y, mu, sigma, theta) result(f)
    real, dimension(:), intent(in) :: x, y
    real, dimension(1:2), intent(in) :: mu, sigma
    real, intent(in), optional :: theta
    real, allocatable, dimension(:, :) :: f
    integer :: i, j
    integer :: n1, n2
    real, allocatable, dimension(:, :) :: rt
    real :: g1, g2
    logical :: rotate
    n1 = size(x)
    n2 = size(y)
    if (present(theta)) then
        rt = zeros(2, 2)
        rt(1, :) = [cos(theta), -sin(theta)]
        rt(2, :) = [sin(theta), cos(theta)]
        rotate = .true.
    else
        rotate = .false.
    end if
    ! The normalization factor is product(sigma)*(2*const_pi)
    f = zeros(n1, n2)
    if (rotate) then
        !$omp parallel do private(i, j, g1, g2)
        do j = 1, n2
            do i = 1, n1
                g1 = (x(i) - mu(1))*rt(1, 1) + (y(j) - mu(2))*rt(1, 2)
                g2 = (x(i) - mu(1))*rt(2, 1) + (y(j) - mu(2))*rt(2, 2)
                f(i, j) = exp(-0.5*((g1/sigma(1))**2 + (g2/sigma(2))**2))
            end do
        end do
        !$omp end parallel do
    else
        !$omp parallel do private(i, j)
        do j = 1, n2
            do i = 1, n1
                f(i, j) = exp(-0.5*((x(i)/sigma(1))**2 + (y(j)/sigma(2))**2))
            end do
        end do
        !$omp end parallel do
    end if
end function gaussian_2d_float
!
!> 3D Gaussian
!
function gaussian_3d_float(x, y, z, mu, sigma, theta) result(f)
    real, dimension(:), intent(in) :: x, y, z
    real, dimension(1:3), intent(in) :: mu, sigma
    real, dimension(1:3), intent(in), optional :: theta
    real, allocatable, dimension(:, :, :) :: f
    integer :: i, j, k
    integer :: n1, n2, n3
    real, allocatable, dimension(:, :) :: rt1, rt2, rt3, rt
    real :: g1, g2, g3
    real :: zero
    logical :: rotate
    n1 = size(x)
    n2 = size(y)
    n3 = size(z)
    zero = 0.0
    if (present(theta)) then
        rt1 = zeros(3, 3)
        rt2 = zeros(3, 3)
        rt3 = zeros(3, 3)
        rt1(1, :) = [ 1.0, 0.0, 0.0 ]
        rt1(2, :) = [ zero, cos(theta(1)), -sin(theta(1))]
        rt1(3, :) = [ zero, sin(theta(1)), cos(theta(1))]
        rt2(1, :) = [ cos(theta(2)), zero, sin(theta(2))]
        rt2(2, :) = [ 0.0, 1.0, 0.0]
        rt2(3, :) = [-sin(theta(2)), zero, cos(theta(2))]
        rt3(1, :) = [ cos(theta(3)), -sin(theta(3)), zero ]
        rt3(2, :) = [ sin(theta(3)), cos(theta(3)), zero ]
        rt3(3, :) = [ 0.0, 0.0, 1.0 ]
        rt = matx(rt3, matx(rt2, rt1))
        rotate = .true.
    else
        rotate = .false.
    end if
    ! The normalization fator is h = product(sigma)*(2*const_pi)**1.5d0
    f = zeros(n1, n2, n3)
    if (rotate) then
        !$omp parallel do private(i, j, k, g1, g2, g3)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    g1 = (x(i) - mu(1))*rt(1, 1) + (y(j) - mu(2))*rt(1, 2) + (z(k) - mu(3))*rt(1, 3)
                    g2 = (x(i) - mu(1))*rt(2, 1) + (y(j) - mu(2))*rt(2, 2) + (z(k) - mu(3))*rt(2, 3)
                    g3 = (x(i) - mu(1))*rt(3, 1) + (y(j) - mu(2))*rt(3, 2) + (z(k) - mu(3))*rt(3, 3)
                    f(i, j, k) = exp(-0.5*((g1/sigma(1))**2 + (g2/sigma(2))**2 + (g3/sigma(3))**2))
                end do
            end do
        end do
        !$omp end parallel do
    else
        !$omp parallel do private(i, j, k)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    f(i, j, k) = exp(-0.5*((x(i)/sigma(1))**2 + (y(j)/sigma(2))**2 + (z(k)/sigma(3))**2))
                end do
            end do
        end do
        !$omp end parallel do
    end if
end function gaussian_3d_float
!
!>
!> Multidimensional Cauchy distribution follows:
!> f(x) = Gamma((1 + d)/2)/(Gamma(1/2)*pi^(d/2)*product(gamma))*(1 + sum_{i=1}^d ((xi - mu_i)/gamma_i)^2)^(-(1 + d)/2)
!> Therefore,
!> 1D: f(x1) = 1/(pi*gamma_1)*(1 + ((x_1 - mu_1)/gamma_1)^2)^(-1)
!> 2D: f(x1, x2) = 1/(2*pi*gamma_1*gamma_2)*(1 + ((x_1 - mu_1)/gamma_1)^2
!> + ((x_2 - mu_2)/gamma_2)^2)^(-3/2)
!> 3D: f(x1, x2, x3) = 1/(pi^2*gamma_1*gamma_2*gamma_3)*(1 + ((x_1 - mu_1)/gamma_1)^2
!> + ((x_2 - mu_2)/gamma_2)^2
!> + ((x_3 - mu_3)/gamma_3)^2)^(-2)
!>
!
!> 1D Cauchy
!
function cauchy_1d_float(x, mu, sigma) result(f)
    real, intent(in) :: mu, sigma
    real, dimension(:), intent(in) :: x
    real, allocatable, dimension(:) :: f
    ! The normalization factor is const_pi*sigma
    f = (1.0 + ((x - mu)/sigma)**2)**(-1.0d0)
end function cauchy_1d_float
!
!> 2D Cauchy
!
function cauchy_2d_float(x, y, mu, sigma, theta) result(f)
    real, dimension(:), intent(in) :: x, y
    real, dimension(1:2), intent(in) :: mu, sigma
    real, intent(in), optional :: theta
    real, allocatable, dimension(:, :) :: f
    integer :: i, j
    integer :: n1, n2
    real, allocatable, dimension(:, :) :: rt
    real :: g1, g2
    logical :: rotate
    n1 = size(x)
    n2 = size(y)
    if (present(theta)) then
        rt = zeros(2, 2)
        rt(1, :) = [cos(theta), -sin(theta)]
        rt(2, :) = [sin(theta), cos(theta)]
        rotate = .true.
    else
        rotate = .false.
    end if
    ! The normalization factoris h = product(sigma)*(2*const_pi)
    f = zeros(n1, n2)
    if (rotate) then
        !$omp parallel do private(i, j, g1, g2)
        do j = 1, n2
            do i = 1, n1
                g1 = (x(i) - mu(1))*rt(1, 1) + (y(j) - mu(2))*rt(1, 2)
                g2 = (x(i) - mu(1))*rt(2, 1) + (y(j) - mu(2))*rt(2, 2)
                f(i, j) = (1.0 + ((g1/sigma(1))**2 + (g2/sigma(2))**2))**(-1.5d0)
            end do
        end do
        !$omp end parallel do
    else
        !$omp parallel do private(i, j)
        do j = 1, n2
            do i = 1, n1
                f(i, j) = (1.0 + ((x(i)/sigma(1))**2 + (y(j)/sigma(2))**2))**(-1.5d0)
            end do
        end do
        !$omp end parallel do
    end if
end function cauchy_2d_float
!
!> 3D Cauchy
!
function cauchy_3d_float(x, y, z, mu, sigma, theta) result(f)
    real, dimension(:), intent(in) :: x, y, z
    real, dimension(1:3), intent(in) :: mu, sigma
    real, dimension(1:3), intent(in), optional :: theta
    real, allocatable, dimension(:, :, :) :: f
    integer :: i, j, k
    integer :: n1, n2, n3
    real, allocatable, dimension(:, :) :: rt1, rt2, rt3, rt
    real :: g1, g2, g3
    real :: zero
    logical :: rotate
    n1 = size(x)
    n2 = size(y)
    n3 = size(z)
    zero = 0.0
    if (present(theta)) then
        rt1 = zeros(3, 3)
        rt2 = zeros(3, 3)
        rt3 = zeros(3, 3)
        rt1(1, :) = [ 1.0, 0.0, 0.0 ]
        rt1(2, :) = [ zero, cos(theta(1)), -sin(theta(1))]
        rt1(3, :) = [ zero, sin(theta(1)), cos(theta(1))]
        rt2(1, :) = [ cos(theta(2)), zero, sin(theta(2))]
        rt2(2, :) = [ 0.0, 1.0, 0.0]
        rt2(3, :) = [-sin(theta(2)), zero, cos(theta(2))]
        rt3(1, :) = [ cos(theta(3)), -sin(theta(3)), zero ]
        rt3(2, :) = [ sin(theta(3)), cos(theta(3)), zero ]
        rt3(3, :) = [ 0.0, 0.0, 1.0 ]
        rt = matx(rt3, matx(rt2, rt1))
        rotate = .true.
    else
        rotate = .false.
    end if
    ! The normalization factor is h = product(sigma)*const_pi**2
    f = zeros(n1, n2, n3)
    if (rotate) then
        !$omp parallel do private(i, j, k, g1, g2, g3)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    g1 = (x(i) - mu(1))*rt(1, 1) + (y(j) - mu(2))*rt(1, 2) + (z(k) - mu(3))*rt(1, 3)
                    g2 = (x(i) - mu(1))*rt(2, 1) + (y(j) - mu(2))*rt(2, 2) + (z(k) - mu(3))*rt(2, 3)
                    g3 = (x(i) - mu(1))*rt(3, 1) + (y(j) - mu(2))*rt(3, 2) + (z(k) - mu(3))*rt(3, 3)
                    f(i, j, k) = (1.0 + ((g1/sigma(1))**2 + (g2/sigma(2))**2 + (g3/sigma(3))**2))**(-2.0d0)
                end do
            end do
        end do
        !$omp end parallel do
    else
        !$omp parallel do private(i, j, k)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    f(i, j, k) = (1.0 + ((x(i)/sigma(1))**2 + (y(j)/sigma(2))**2 + (z(k)/sigma(3))**2))**(-2.0d0)
                end do
            end do
        end do
        !$omp end parallel do
    end if
end function cauchy_3d_float
function covariance_1d_float(x, y) result(c)
    real, dimension(:), intent(in) :: x, y
    real :: c
    real :: mx, my
    integer :: nx, ny
    nx = size(x)
    ny = size(y)
    call assert(nx == ny, ' <covariance_1d_> Error: size(x) /= size(y)')
    mx = mean(x)
    my = mean(y)
    c = sum((x - mx)*(y - my))
    if (nx > 1) then
        c = c/(nx - 1.0)
    end if
end function covariance_1d_float
function covariance_2d_float(x, y) result(c)
    real, dimension(:, :), intent(in) :: x, y
    real :: c
    real :: mx, my
    integer :: nx1, ny1, nx2, ny2
    nx1 = size(x, 1)
    ny1 = size(y, 1)
    call assert(nx1 == ny1, ' <covariance_2d_> Error: size(x, 1) /= size(y, 1)')
    nx2 = size(x, 2)
    ny2 = size(y, 2)
    call assert(nx2 == ny2, ' <covariance_2d_> Error: size(x, 2) /= size(y, 2)')
    mx = mean(x)
    my = mean(y)
    c = sum((x - mx)*(y - my))
    if (nx1*nx2 > 1) then
        c = c/(nx1*nx2 - 1.0)
    end if
end function covariance_2d_float
function covariance_3d_float(x, y) result(c)
    real, dimension(:, :, :), intent(in) :: x, y
    real :: c
    real :: mx, my
    integer :: nx1, ny1, nx2, ny2, nx3, ny3
    nx1 = size(x, 1)
    ny1 = size(y, 1)
    call assert(nx1 == ny1, ' <covariance_3d_> Error: size(x, 1) /= size(y, 1)')
    nx2 = size(x, 2)
    ny2 = size(y, 2)
    call assert(nx2 == ny2, ' <covariance_3d_> Error: size(x, 2) /= size(y, 2)')
    nx3 = size(x, 3)
    ny3 = size(y, 3)
    call assert(nx3 == ny3, ' <covariance_3d_> Error: size(x, 3) /= size(y, 3)')
    mx = mean(x)
    my = mean(y)
    c = sum((x - mx)*(y - my))
    if (nx1*nx2*nx3 > 1) then
        c = c/(nx1*nx2*nx3 - 1.0)
    end if
end function covariance_3d_float
!
!> Compute the unbiased standard deviation of a 1D array
!
function standard_deviation_1d_float(w) result(s)
    real, dimension(:), intent(in) :: w
    real :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = sqrt(s/(size(w) - 1))
    end if
end function standard_deviation_1d_float
!
!> Compute the unbiased standard deviation of a 2D array
!
function standard_deviation_2d_float(w) result(s)
    real, dimension(:, :), intent(in) :: w
    real :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = sqrt(s/(size(w) - 1))
    end if
end function standard_deviation_2d_float
!
!> Compute the unbiased standard deviation of a 3D array
!
function standard_deviation_3d_float(w) result(s)
    real, dimension(:, :, :), intent(in) :: w
    real :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = sqrt(s/(size(w) - 1))
    end if
end function standard_deviation_3d_float
!
!> Compute the histogram of a 1D array
!
subroutine histogram_1d_float(w, hist, valmin, valmax, binsize)
    real, dimension(:), intent(in) :: w
    real, allocatable, dimension(:, :), intent(inout) :: hist
    real, intent(in), optional :: valmin, valmax, binsize
    real :: histmin, histmax, histbin
    integer :: nbin, i
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! allocate hist
    call alloc_array(hist, [1, nbin, 1, 4])
    ! compute hist
    !$omp parallel do private(i)
    do i = 1, nbin
        if (i == 1) then
            hist(i, 1) = histmin
        else
            hist(i, 1) = histmin + (i - 1)*histbin
        end if
        if (i == nbin) then
            hist(i, 2) = histmax
        else
            hist(i, 2) = histmin + i*histbin
        end if
        if (i < nbin) then
            hist(i, 3) = count(w >= hist(i, 1) .and. w < hist(i, 2), kind=8)
        else
            hist(i, 3) = count(w >= hist(i, 1) .and. w <= hist(i, 2), kind=8)
        end if
    end do
    !$omp end parallel do
    ! normalize
    hist(:, 4) = hist(:, 3)/size(w, kind=8)
end subroutine histogram_1d_float
!
!> Compute the histogram of a 2D array
!
subroutine histogram_2d_float(w, hist, valmin, valmax, binsize)
    real, dimension(:, :), intent(in) :: w
    real, allocatable, dimension(:, :), intent(inout) :: hist
    real, intent(in), optional :: valmin, valmax, binsize
    real :: histmin, histmax, histbin
    integer :: nbin, i
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! allocate hist
    call alloc_array(hist, [1, nbin, 1, 4])
    ! compute hist
    !$omp parallel do private(i)
    do i = 1, nbin
        if (i == 1) then
            hist(i, 1) = histmin
        else
            hist(i, 1) = histmin + (i - 1)*histbin
        end if
        if (i == nbin) then
            hist(i, 2) = histmax
        else
            hist(i, 2) = histmin + i*histbin
        end if
        if (i < nbin) then
            hist(i, 3) = count(w >= hist(i, 1) .and. w < hist(i, 2))
        else
            hist(i, 3) = count(w >= hist(i, 1) .and. w <= hist(i, 2))
        end if
    end do
    !$omp end parallel do
    ! normalize
    hist(:, 4) = hist(:, 3)/size(w, 1)/size(w, 2)
end subroutine histogram_2d_float
!
!> Compute the histogram of a 3D array
!
subroutine histogram_3d_float(w, hist, valmin, valmax, binsize)
    real, dimension(:, :, :), intent(in) :: w
    real, allocatable, dimension(:, :), intent(inout) :: hist
    real, intent(in), optional :: valmin, valmax, binsize
    real :: histmin, histmax, histbin
    integer :: nbin, i
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! allocate hist
    call alloc_array(hist, [1, nbin, 1, 4])
    ! compute hist
    !$omp parallel do private(i)
    do i = 1, nbin
        if (i == 1) then
            hist(i, 1) = histmin
        else
            hist(i, 1) = histmin + (i - 1)*histbin
        end if
        if (i == nbin) then
            hist(i, 2) = histmax
        else
            hist(i, 2) = histmin + i*histbin
        end if
        if (i < nbin) then
            hist(i, 3) = count(w >= hist(i, 1) .and. w < hist(i, 2))
        else
            hist(i, 3) = count(w >= hist(i, 1) .and. w <= hist(i, 2))
        end if
    end do
    !$omp end parallel do
    ! normalize
    hist(:, 4) = hist(:, 3)/size(w, 1)/size(w, 2)/size(w, 3)
end subroutine histogram_3d_float
!
!> Plot the histogram of a 1D array
!
subroutine plot_histogram_1d_float(w, valmin, valmax, binsize, label)
    real, dimension(:), intent(in) :: w
    real, intent(in), optional :: valmin, valmax, binsize
    character(len=*), intent(in), optional :: label
    real :: histmin, histmax, histbin
    integer :: nbin, i, j
    real, allocatable, dimension(:, :) :: hist
    character(len=20) :: bar
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! compute history
    call histogram_1d_float(w, hist, histmin, histmax, histbin)
    nbin = size(hist, 1)
    ! compute hist
    if (present(label)) then
        write (error_unit, '(x,a)') label
    end if
    do i = 1, nbin
        bar = ''
        do j = 1, nint(hist(i, 4)/maxval(hist(:, 4))*len(bar))
            bar(j:j) = '*'
        end do
        write (error_unit, '(x,es12.5,x,a,x,es12.5,x,x,a,x,i12,x,es12.5)') &
            hist(i, 1), '~', hist(i, 2), bar, int(hist(i, 3), kind=8), hist(i, 4)
    end do
end subroutine plot_histogram_1d_float
!
!> Plot the histogram of a 2D array
!
subroutine plot_histogram_2d_float(w, valmin, valmax, binsize, label)
    real, dimension(:, :), intent(in) :: w
    real, intent(in), optional :: valmin, valmax, binsize
    character(len=*), intent(in), optional :: label
    real :: histmin, histmax, histbin
    integer :: nbin, i, j
    real, allocatable, dimension(:, :) :: hist
    character(len=20) :: bar
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! compute history
    call histogram_2d_float(w, hist, histmin, histmax, histbin)
    nbin = size(hist, 1)
    ! compute hist
    if (present(label)) then
        write (error_unit, '(x,a)') label
    end if
    do i = 1, nbin
        bar = ''
        do j = 1, nint(hist(i, 4)/maxval(hist(:, 4))*len(bar))
            bar(j:j) = '*'
        end do
        write (error_unit, '(x,es12.5,x,a,x,es12.5,x,x,a,x,i12,x,es12.5)') &
            hist(i, 1), '~', hist(i, 2), bar, int(hist(i, 3), kind=8), hist(i, 4)
    end do
end subroutine plot_histogram_2d_float
!
!> Plot the histogram of a 3D array
!
subroutine plot_histogram_3d_float(w, valmin, valmax, binsize, label)
    real, dimension(:, :, :), intent(in) :: w
    real, intent(in), optional :: valmin, valmax, binsize
    character(len=*), intent(in), optional :: label
    real :: histmin, histmax, histbin
    integer :: nbin, i, j
    real, allocatable, dimension(:, :) :: hist
    character(len=20) :: bar
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! compute history
    call histogram_3d_float(w, hist, histmin, histmax, histbin)
    nbin = size(hist, 1)
    ! compute hist
    if (present(label)) then
        write (error_unit, '(a)') label
    end if
    do i = 1, nbin
        bar = ''
        do j = 1, nint(hist(i, 4)/maxval(hist(:, 4))*len(bar))
            bar(j:j) = '*'
        end do
        write (error_unit, '(x,es12.5,x,a,x,es12.5,x,x,a,x,i12,x,es12.5)') &
            hist(i, 1), '~', hist(i, 2), bar, int(hist(i, 3), kind=8), hist(i, 4)
    end do
end subroutine plot_histogram_3d_float
function median_1d_float(w) result(m)
    real, dimension(:), intent(in) :: w
    real :: m
    real, allocatable, dimension(:) :: wt
    integer(kind=8) :: n
    n = size(w, kind=8)
    ! wt = w !call alloc_array(wt, [1, n], source=w)
    wt = sort(w) !wt)
    if (mod(n, 2) == 0) then
        m = 0.5*(wt(n/2) + wt(n/2 + 1))
    else
        m = wt((n + 1)/2)
    end if
end function median_1d_float
function median_2d_float(w) result(m)
    real, dimension(:, :), intent(in) :: w
    real :: m
    real, allocatable, dimension(:) :: wt
    integer(kind=8) :: n
    n = size(w, kind=8) !size(w, 1)*size(w, 2)
    ! wt = flatten(w) !call alloc_array(wt, [1, n], source=reshape(w, [n]))
    wt = sort(flatten(w))
    if (mod(n, 2) == 0) then
        m = 0.5*(wt(n/2) + wt(n/2 + 1))
    else
        m = wt((n + 1)/2)
    end if
end function median_2d_float
function median_3d_float(w) result(m)
    real, dimension(:, :, :), intent(in) :: w
    real :: m
    real, allocatable, dimension(:) :: wt
    integer(kind=8) :: n
    n = size(w, kind=8) ! size(w, 1)*size(w, 2)*size(w, 3)
    ! call alloc_array(wt, [1, n], source=reshape(w, [n]))
    wt = sort(flatten(w)) !wt)
    if (mod(n, 2) == 0) then
        m = 0.5*(wt(n/2) + wt(n/2 + 1))
    else
        m = wt((n + 1)/2)
    end if
end function median_3d_float
!
!> 1D discrete crosscorrelation
!
subroutine discrete_xcorr_1d_float(x, y, z, kx, ky, kz)
    real, dimension(:), intent(in) :: x, y
    real, dimension(:), intent(inout) :: z
    integer, intent(in) :: kx, ky, kz
    integer :: lx, ly, lz
    integer :: kxr, i
    real, allocatable, dimension(:) :: xr
    lx = size(x, 1)
    ly = size(y, 1)
    lz = size(z, 1)
    allocate (xr(1:lx))
    do i = 1, lx
        xr(i) = x(lx - i + 1)
    end do
    kxr = 1 - (kx - 1) - lx
    call convd(xr, y, z, &
        kxr, &
        ky, &
        kz)
    deallocate (xr)
end subroutine discrete_xcorr_1d_float
!
!> 2D discrete crosscorrelation
!
subroutine discrete_xcorr_2d_float(x, y, z, &
        kx1, kx2, &
        ky1, ky2, &
        kz1, kz2)
    real, dimension(:, :), intent(in) :: x, y
    real, dimension(:, :), intent(inout) :: z
    integer, intent(in) :: kx1, ky1, kz1
    integer, intent(in) :: kx2, ky2, kz2
    integer :: lx1, ly1, lz1
    integer :: lx2, ly2, lz2
    real, allocatable, dimension(:, :) :: xr
    integer :: i, j, kx1r, kx2r
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    allocate (xr(1:lx1, 1:lx2))
    do j = 1, lx2
        do i = 1, lx1
            xr(i, j) = x(lx1 - i + 1, lx2 - j + 1)
        end do
    end do
    kx1r = 1 - (kx1 - 1) - lx1
    kx2r = 1 - (kx2 - 1) - lx2
    call convd(xr, y, z, &
        kx1r, kx2r, &
        ky1, ky2, &
        kz1, kz2)
    deallocate (xr)
end subroutine discrete_xcorr_2d_float
!
!> 3D discrete crosscorrelation
!
subroutine discrete_xcorr_3d_float(x, y, z, &
        kx1, kx2, kx3, &
        ky1, ky2, ky3, &
        kz1, kz2, kz3)
    integer, intent(in) :: kx1, ky1, kz1
    integer, intent(in) :: kx2, ky2, kz2
    integer, intent(in) :: kx3, ky3, kz3
    real, dimension(:, :, :), intent(in) :: x, y
    real, dimension(:, :, :), intent(inout) :: z
    integer :: lx1, ly1, lz1
    integer :: lx2, ly2, lz2
    integer :: lx3, ly3, lz3
    real, allocatable, dimension(:, :, :) :: xr
    integer :: i, j, k, kx1r, kx2r, kx3r
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    lx3 = size(x, 3)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    ly3 = size(y, 3)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    lz3 = size(z, 3)
    allocate (xr(1:lx1, 1:lx2, 1:lx3))
    do k = 1, lx3
        do j = 1, lx2
            do i = 1, lx1
                xr(i, j, k) = x(lx1 - i + 1, lx2 - j + 1, lx3 - k + 1)
            end do
        end do
    end do
    kx1r = 1 - (kx1 - 1) - lx1
    kx2r = 1 - (kx2 - 1) - lx2
    kx3r = 1 - (kx3 - 1) - lx3
    call convd(xr, y, z, &
        kx1r, kx2r, kx3r, &
        ky1, ky2, ky3, &
        kz1, kz2, kz3)
    deallocate (xr)
end subroutine discrete_xcorr_3d_float
!
!> 1D FFT-based cross-correlation
!
function xcorr_1d_float(x, y, maxlag) result(z)
    real, dimension(:), intent(in) :: x, y
    integer, intent(in), optional :: maxlag
    integer :: nx, ny, n, nlag
    real, allocatable, dimension(:) :: z
    ! Dimensions
    nx = size(x)
    ny = size(y)
    if (present(maxlag)) then
        nlag = min(maxlag, max(nx, ny))
    else
        nlag = max(nx, ny) - 1
    end if
    ! Pad to next power 235
    n = next_power_235(max(nx, ny) + nlag)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    !
    ! Cross-correlation has different definitions in different literature
    ! The definition adopted here is
    ! C_xy(m) = sum [x(n + m) * y^*(n)]
    ! Some literature defines
    ! C_xy(m) = sum [x^*(n) * y(n + m)]
    ! which gives reversed result compared with the first definition
    ! The first definition is adopted in MATLAB and numpy, and is therefore
    ! adopted here as well.
    !
    allocate (z(1:n))
    z = fftshift(ifft( &
        fft(pad(x, [0, n - nx], ['const', 'const']))* &
        conjg(fft(pad(y, [0, n - ny], ['const', 'const']))), real=.true.))
    call alloc_array(z, [-nlag, nlag], &
        source=z(nint((n + 1)/2.0) - nlag:nint((n + 1)/2.0) + nlag))
end function xcorr_1d_float
!
!> 2D FFT-based cross-correlation
!
function xcorr_2d_float(x, y, maxlag) result(z)
    real, dimension(:, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    integer :: nx1, ny1, nx2, ny2, n1, n2, nlag1, nlag2
    real, allocatable, dimension(:, :) :: z
    ! Dimensions
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(nx1, ny1))
        nlag2 = min(maxlag(2), max(nx2, ny2))
    else
        nlag1 = max(nx1, ny1) - 1
        nlag2 = max(nx2, ny2) - 1
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(nx1, ny1) + nlag1)
    n2 = next_power_235(max(nx2, ny2) + nlag2)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    allocate (z(1:n1, 1:n2))
    z = fftshift(ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2], ['const', 'const', 'const', 'const']))* &
        conjg(fft(pad(y, [0, n1 - ny1, 0, n2 - ny2], ['const', 'const', 'const', 'const']))), real=.true.))
    call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2], &
        source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
        nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2))
end function xcorr_2d_float
!
!> 3D FFT-based cross-correlation
!
function xcorr_3d_float(x, y, maxlag) result(z)
    real, dimension(:, :, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    integer :: nx1, ny1, nx2, ny2, nx3, ny3, n1, n2, n3, nlag1, nlag2, nlag3
    real, allocatable, dimension(:, :, :) :: z
    ! Dimensions
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    nx3 = size(x, 3)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    ny3 = size(y, 3)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(nx1, ny1))
        nlag2 = min(maxlag(2), max(nx2, ny2))
        nlag3 = min(maxlag(3), max(nx3, ny3))
    else
        nlag1 = max(nx1, ny1) - 1
        nlag2 = max(nx2, ny2) - 1
        nlag3 = max(nx3, ny3) - 1
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(nx1, ny1) + nlag1)
    n2 = next_power_235(max(nx2, ny2) + nlag2)
    n3 = next_power_235(max(nx3, ny3) + nlag3)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    allocate (z(1:n1, 1:n2, 1:n3))
    z = fftshift(ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2, 0, n3 - nx3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))* &
        conjg(fft(pad(y, [0, n1 - ny1, 0, n2 - ny2, 0, n3 - ny3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))), real=.true.))
    call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2, -nlag3, nlag3], &
        source=z( &
        nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
        nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2, &
        nint((n3 + 1)/2.0) - nlag3:nint((n3 + 1)/2.0) + nlag3))
end function xcorr_3d_float
!
!> 1D discrete autocorrelation
!
subroutine discrete_acorr_1d_float(x, z, kx, kz)
    real, dimension(:), intent(in) :: x
    real, dimension(:), intent(inout) :: z
    integer, intent(in) :: kx, kz
    integer :: lx, lz
    integer :: kxr, i
    real, allocatable, dimension(:) :: xr
    lx = size(x, 1)
    lz = size(z, 1)
    allocate (xr(1:lx))
    do i = 1, lx
        xr(i) = x(lx - i + 1)
    end do
    kxr = 1 - (kx - 1) - lx
    call convd(xr, x, z, &
        kxr, &
        kx, &
        kz)
    deallocate (xr)
end subroutine discrete_acorr_1d_float
!
!> 2D discrete autocorrelation
!
subroutine discrete_acorr_2d_float(x, z, &
        kx1, kx2, &
        kz1, kz2)
    real, dimension(:, :), intent(in) :: x
    real, dimension(:, :), intent(inout) :: z
    integer, intent(in) :: kx1, kz1
    integer, intent(in) :: kx2, kz2
    integer :: lx1, lz1
    integer :: lx2, lz2
    real, allocatable, dimension(:, :) :: xr
    integer :: i, j, kx1r, kx2r
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    allocate (xr(1:lx1, 1:lx2))
    do j = 1, lx2
        do i = 1, lx1
            xr(i, j) = x(lx1 - i + 1, lx2 - j + 1)
        end do
    end do
    kx1r = 1 - (kx1 - 1) - lx1
    kx2r = 1 - (kx2 - 1) - lx2
    call convd(xr, x, z, &
        kx1r, kx2r, &
        kx1, kx2, &
        kz1, kz2)
    deallocate (xr)
end subroutine discrete_acorr_2d_float
!
!> 3D discrete autocorrelation
!
subroutine discrete_acorr_3d_float(x, z, &
        kx1, kx2, kx3, &
        kz1, kz2, kz3)
    integer, intent(in) :: kx1, kz1
    integer, intent(in) :: kx2, kz2
    integer, intent(in) :: kx3, kz3
    real, dimension(:, :, :), intent(in) :: x
    real, dimension(:, :, :), intent(inout) :: z
    integer :: lx1, lz1
    integer :: lx2, lz2
    integer :: lx3, lz3
    real, allocatable, dimension(:, :, :) :: xr
    integer :: i, j, k, kx1r, kx2r, kx3r
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    lx3 = size(x, 3)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    lz3 = size(z, 3)
    allocate (xr(1:lx1, 1:lx2, 1:lx3))
    do k = 1, lx3
        do j = 1, lx2
            do i = 1, lx1
                xr(i, j, k) = x(lx1 - i + 1, lx2 - j + 1, lx3 - k + 1)
            end do
        end do
    end do
    kx1r = 1 - (kx1 - 1) - lx1
    kx2r = 1 - (kx2 - 1) - lx2
    kx3r = 1 - (kx3 - 1) - lx3
    call convd(xr, x, z, &
        kx1r, kx2r, kx3r, &
        kx1, kx2, kx3, &
        kz1, kz2, kz3)
    deallocate (xr)
end subroutine discrete_acorr_3d_float
!
!> 1D FFT-based auto-correlation
!
function acorr_1d_float(x, maxlag) result(z)
    real, dimension(:), intent(in) :: x
    integer, intent(in), optional :: maxlag
    integer :: nx, n, nlag
    real, allocatable, dimension(:) :: z
    ! Dimensions
    nx = size(x)
    if (present(maxlag)) then
        nlag = min(maxlag, nx)
    else
        nlag = nx - 1
    end if
    ! Pad to next power 235
    n = next_power_235(nx + nlag)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    allocate (z(1:n))
    z = fftshift(ifft( &
        cmplx(fft(pad(x, [0, n - nx], ['const', 'const']))**2), real=.true.))
    call alloc_array(z, [-nlag, nlag], &
        source=z(nint((n + 1)/2.0) - nlag:nint((n + 1)/2.0) + nlag))
end function acorr_1d_float
!
!> 2D FFT-based auto-correlation
!
function acorr_2d_float(x, maxlag) result(z)
    real, dimension(:, :), intent(in) :: x
    integer, dimension(:), intent(in), optional :: maxlag
    integer :: nx1, nx2, n1, n2, nlag1, nlag2
    real, allocatable, dimension(:, :) :: z
    ! Dimensions
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), nx1)
        nlag2 = min(maxlag(2), nx2)
    else
        nlag1 = nx1 - 1
        nlag2 = nx2 - 1
    end if
    ! Pad to next power 235
    n1 = next_power_235(nx1 + nlag1)
    n2 = next_power_235(nx2 + nlag2)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    allocate (z(1:n1, 1:n2))
    z = fftshift(ifft( &
        cmplx(fft(pad(x, [0, n1 - nx1, 0, n2 - nx2], &
        ['const', 'const', 'const', 'const']))**2), real=.true.))
    call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2], &
        source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
        nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2))
end function acorr_2d_float
!
!> 3D FFT-based auto-correlation
!
function acorr_3d_float(x, maxlag) result(z)
    real, dimension(:, :, :), intent(in) :: x
    integer, dimension(:), intent(in), optional :: maxlag
    integer :: nx1, nx2, nx3, n1, n2, n3, nlag1, nlag2, nlag3
    real, allocatable, dimension(:, :, :) :: z
    ! Dimensions
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    nx3 = size(x, 3)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), nx1)
        nlag2 = min(maxlag(2), nx2)
        nlag3 = min(maxlag(3), nx3)
    else
        nlag1 = nx1 - 1
        nlag2 = nx2 - 1
        nlag3 = nx3 - 1
    end if
    ! Pad to next power 235
    n1 = next_power_235(nx1 + nlag1)
    n2 = next_power_235(nx2 + nlag2)
    n3 = next_power_235(nx3 + nlag3)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    allocate (z(1:n1, 1:n2, 1:n3))
    z = fftshift(ifft( &
        cmplx(fft(pad(x, [0, n1 - nx1, 0, n2 - nx2, 0, n3 - nx3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))**2), real=.true.))
    call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2, -nlag3, nlag3], &
        source=z( &
        nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
        nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2, &
        nint((n3 + 1)/2.0) - nlag3:nint((n3 + 1)/2.0) + nlag3))
end function acorr_3d_float
!
!> 1D discrete cross-correlation
!
subroutine local_xcorr_1d_float(tr1, tr2, wt, ccoef, tshift)
    real, dimension(:), intent(in) :: tr1, tr2
    real, dimension(:), intent(inout) :: ccoef, tshift
    integer, intent(in) :: wt
    integer :: nt, i, ns
    real, allocatable, dimension(:) :: ptr1, ptr2, pw1, pw2, pc
    real :: norm
    ! number of samples
    nt = size(tr1, 1)
    if (maxval(abs(tr1)) == 0 .or. maxval(abs(tr2)) == 0) then
        ccoef = 1.0
        tshift = 0.0
    else
        ns = floor(wt/3.0)
        call alloc_array(ptr1, [-wt + 1, nt + wt])
        call alloc_array(ptr2, [-wt + 1, nt + wt])
        ptr1(1:nt) = tr1
        ptr2(1:nt) = tr2
        call alloc_array(pw1, [-wt, wt])
        call alloc_array(pw1, [-wt, wt])
        call alloc_array(pc, [-ns, ns])
        do i = 1, nt
            pw1 = ptr1(i - wt:i + wt)
            pw2 = ptr2(i - wt:i + wt)
            call discrete_xcorr_1d_float(pw1, pw2, pc, 1, 1, -ns)
            norm = norm2(pw1)*norm2(pw2)
            if (norm == 0) then
                ccoef(i) = 0.0
                tshift(i) = 0.0
            else
                ccoef(i) = maxval(pc)/norm
                tshift(i) = maxloc(pc, 1) - ns - 1.0
            end if
        end do
    end if
end subroutine local_xcorr_1d_float
!
!> Zero-lag cross-correlation coefficient
!
function xcorr_coef_1d_float(a, b) result(r)
    real, dimension(:), intent(in) :: a
    real, dimension(:), intent(in) :: b
    real :: r
    real :: ma, mb
    call assert(size(a) == size(b), 'Error: size(a) /= size(b)')
    ma = mean(a)
    mb = mean(b)
    r = sum((a - ma)*(b - mb))/sqrt(sum((a - ma)**2))/sqrt(sum((b - mb)**2))
end function xcorr_coef_1d_float
function xcorr_coef_2d_float(a, b) result(r)
    real, dimension(:, :), intent(in) :: a
    real, dimension(:, :), intent(in) :: b
    real :: r
    real :: ma, mb
    call assert(size(a, 1) == size(b, 1), 'Error: size(a, 1) /= size(b, 1)')
    call assert(size(a, 2) == size(b, 2), 'Error: size(a, 2) /= size(b, 2)')
    ma = mean(a)
    mb = mean(b)
    r = sum((a - ma)*(b - mb))/sqrt(sum((a - ma)**2))/sqrt(sum((b - mb)**2))
end function xcorr_coef_2d_float
function xcorr_coef_3d_float(a, b) result(r)
    real, dimension(:, :, :), intent(in) :: a
    real, dimension(:, :, :), intent(in) :: b
    real :: r
    real :: ma, mb
    call assert(size(a, 1) == size(b, 1), 'Error: size(a, 1) /= size(b, 1)')
    call assert(size(a, 2) == size(b, 2), 'Error: size(a, 2) /= size(b, 2)')
    call assert(size(a, 3) == size(b, 3), 'Error: size(a, 3) /= size(b, 3)')
    ma = mean(a)
    mb = mean(b)
    r = sum((a - ma)*(b - mb))/sqrt(sum((a - ma)**2))/sqrt(sum((b - mb)**2))
end function xcorr_coef_3d_float
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
elemental function kernel_triangular_double(u) result(w)
    double precision, intent(in) :: u
    double precision :: w
    w = abs(u)
    if (w < 1) then
        w = 1 - w
    else
        w = 0
    end if
end function kernel_triangular_double
elemental function kernel_epanechnikov_double(u) result(w)
    double precision, intent(in) :: u
    double precision :: w
    w = abs(u)
    if (w < 1) then
        w = 0.75*(1 - w**2)
    else
        w = 0
    end if
end function kernel_epanechnikov_double
elemental function kernel_bisquare_double(u) result(w)
    double precision, intent(in) :: u
    double precision :: w
    w = abs(u)
    if (w < 1) then
        w = (1 - w**2)**2
    else
        w = 0
    end if
end function kernel_bisquare_double
elemental function kernel_trisquare_double(u) result(w)
    double precision, intent(in) :: u
    double precision :: w
    w = abs(u)
    if (w < 1) then
        w = (1 - w**2)**3
    else
        w = 0
    end if
end function kernel_trisquare_double
elemental function kernel_tricube_double(u) result(w)
    double precision, intent(in) :: u
    double precision :: w
    w = abs(u)
    if (w < 1) then
        w = (1 - w**3)**3
    else
        w = 0
    end if
end function kernel_tricube_double
elemental function kernel_cosine_double(u) result(w)
    double precision, intent(in) :: u
    double precision :: w
    w = abs(u)
    if (w < 1) then
        w = const_pi/4.0*cos(const_pi/2.0*w)
    else
        w = 0
    end if
end function kernel_cosine_double
!
! @brief Variance-covariance matrix of a vector x
!
function covariance_matrix_1d_double(x) result(c)
    double precision, dimension(:), intent(in) :: x
    double precision, allocatable, dimension(:, :) :: c
    c = ones(2, 2)*var(x)
end function covariance_matrix_1d_double
function covariance_matrix_2d_double(x) result(c)
    double precision, dimension(:, :), intent(in) :: x
    double precision, allocatable, dimension(:, :) :: c
    c = ones(2, 2)*var(x)
end function covariance_matrix_2d_double
function covariance_matrix_3d_double(x) result(c)
    double precision, dimension(:, :, :), intent(in) :: x
    double precision, allocatable, dimension(:, :) :: c
    c = ones(2, 2)*var(x)
end function covariance_matrix_3d_double
!
! @brief Variance-covariance matrix of n sets variants
! Each column of x is an observation/realization
!
function row_covariance_matrix_2d_double(x, rowvar) result(c)
    double precision, dimension(:, :), intent(in) :: x
    logical, intent(in) :: rowvar
    double precision, allocatable, dimension(:, :) :: c
    integer :: n, i, j
    if (rowvar) then
        n = size(x, 1)
        c = zeros(n, n)
        do j = 1, n
            do i = 1, n
                if (j >= i) then
                    c(i, j) = covar(x(i, :), x(j, :))
                end if
            end do
        end do
    else
        n = size(x, 2)
        c = zeros(n, n)
        do j = 1, n
            do i = 1, n
                if (j >= i) then
                    c(i, j) = covar(x(:, i), x(:, j))
                end if
            end do
        end do
    end if
    do j = 1, n
        do i = 1, n
            if (j < i) then
                c(i, j) = c(j, i)
            end if
        end do
    end do
end function row_covariance_matrix_2d_double
function cross_covariance_matrix_1d_double(x, y) result(c)
    double precision, dimension(:), intent(in) :: x, y
    double precision, allocatable, dimension(:, :) :: c
    c = zeros(2, 2)
    c(1, :) = [var(x), covar(x, y)]
    c(2, :) = [covar(y, x), var(y)]
end function cross_covariance_matrix_1d_double
function cross_covariance_matrix_2d_double(x, y) result(c)
    double precision, dimension(:, :), intent(in) :: x, y
    double precision, allocatable, dimension(:, :) :: c
    c = zeros(2, 2)
    c(1, :) = [var(x), covar(x, y)]
    c(2, :) = [covar(y, x), var(y)]
end function cross_covariance_matrix_2d_double
function cross_covariance_matrix_3d_double(x, y) result(c)
    double precision, dimension(:, :, :), intent(in) :: x, y
    double precision, allocatable, dimension(:, :) :: c
    c = zeros(2, 2)
    c(1, :) = [var(x), covar(x, y)]
    c(2, :) = [covar(y, x), var(y)]
end function cross_covariance_matrix_3d_double
function row_cross_covariance_matrix_2d_double(x, y, rowvar) result(c)
    double precision, dimension(:, :), intent(in) :: x, y
    logical, intent(in) :: rowvar
    double precision, allocatable, dimension(:, :) :: c
    integer :: n, i, j
    call assert(size(x, 1) == size(y, 1) .and. size(x, 2) == size(y, 2), &
        ' <cross_covariance_matrix_2d_> Error: x and y must have same shape.')
    if (rowvar) then
        n = size(x, 1)
        c = zeros(n, n)
        do j = 1, n
            do i = 1, n
                if (j >= i) then
                    c(i, j) = covar(x(i, :), y(j, :))
                end if
            end do
        end do
    else
        n = size(x, 2)
        c = zeros(n, n)
        do j = 1, n
            do i = 1, n
                if (j >= i) then
                    c(i, j) = covar(x(:, i), y(:, j))
                end if
            end do
        end do
    end if
    do j = 1, n
        do i = 1, n
            if (j < i) then
                c(i, j) = c(j, i)
            end if
        end do
    end do
end function row_cross_covariance_matrix_2d_double
!
!> Compute the variance of a 1D array
!
function variance_1d_double(w) result(s)
    double precision, dimension(:), intent(in) :: w
    double precision :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = s/(size(w) - 1)
    end if
end function variance_1d_double
!
!> Compute the variance of a 2D array
!
function variance_2d_double(w) result(s)
    double precision, dimension(:, :), intent(in) :: w
    double precision :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = s/(size(w) - 1)
    end if
end function variance_2d_double
!
!> Compute the variance of a 3D array
!
function variance_3d_double(w) result(s)
    double precision, dimension(:, :, :), intent(in) :: w
    double precision :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = s/(size(w) - 1)
    end if
end function variance_3d_double
!
!> 1D Gaussian
!
function gaussian_1d_double(x, mu, sigma) result(f)
    double precision, intent(in) :: mu, sigma
    double precision, dimension(:), intent(in) :: x
    double precision, allocatable, dimension(:) :: f
    ! The normalization factor is sigma*sqrt(2*const_pi)
    f = exp(-0.5*((x - mu)/sigma)**2)
end function gaussian_1d_double
!
!> 2D Gaussian
!
function gaussian_2d_double(x, y, mu, sigma, theta) result(f)
    double precision, dimension(:), intent(in) :: x, y
    double precision, dimension(1:2), intent(in) :: mu, sigma
    double precision, intent(in), optional :: theta
    double precision, allocatable, dimension(:, :) :: f
    integer :: i, j
    integer :: n1, n2
    double precision, allocatable, dimension(:, :) :: rt
    double precision :: g1, g2
    logical :: rotate
    n1 = size(x)
    n2 = size(y)
    if (present(theta)) then
        rt = zeros(2, 2)
        rt(1, :) = [cos(theta), -sin(theta)]
        rt(2, :) = [sin(theta), cos(theta)]
        rotate = .true.
    else
        rotate = .false.
    end if
    ! The normalization factor is product(sigma)*(2*const_pi)
    f = zeros(n1, n2)
    if (rotate) then
        !$omp parallel do private(i, j, g1, g2)
        do j = 1, n2
            do i = 1, n1
                g1 = (x(i) - mu(1))*rt(1, 1) + (y(j) - mu(2))*rt(1, 2)
                g2 = (x(i) - mu(1))*rt(2, 1) + (y(j) - mu(2))*rt(2, 2)
                f(i, j) = exp(-0.5*((g1/sigma(1))**2 + (g2/sigma(2))**2))
            end do
        end do
        !$omp end parallel do
    else
        !$omp parallel do private(i, j)
        do j = 1, n2
            do i = 1, n1
                f(i, j) = exp(-0.5*((x(i)/sigma(1))**2 + (y(j)/sigma(2))**2))
            end do
        end do
        !$omp end parallel do
    end if
end function gaussian_2d_double
!
!> 3D Gaussian
!
function gaussian_3d_double(x, y, z, mu, sigma, theta) result(f)
    double precision, dimension(:), intent(in) :: x, y, z
    double precision, dimension(1:3), intent(in) :: mu, sigma
    double precision, dimension(1:3), intent(in), optional :: theta
    double precision, allocatable, dimension(:, :, :) :: f
    integer :: i, j, k
    integer :: n1, n2, n3
    double precision, allocatable, dimension(:, :) :: rt1, rt2, rt3, rt
    double precision :: g1, g2, g3
    double precision :: zero
    logical :: rotate
    n1 = size(x)
    n2 = size(y)
    n3 = size(z)
    zero = 0.0
    if (present(theta)) then
        rt1 = zeros(3, 3)
        rt2 = zeros(3, 3)
        rt3 = zeros(3, 3)
        rt1(1, :) = [ 1.0, 0.0, 0.0 ]
        rt1(2, :) = [ zero, cos(theta(1)), -sin(theta(1))]
        rt1(3, :) = [ zero, sin(theta(1)), cos(theta(1))]
        rt2(1, :) = [ cos(theta(2)), zero, sin(theta(2))]
        rt2(2, :) = [ 0.0, 1.0, 0.0]
        rt2(3, :) = [-sin(theta(2)), zero, cos(theta(2))]
        rt3(1, :) = [ cos(theta(3)), -sin(theta(3)), zero ]
        rt3(2, :) = [ sin(theta(3)), cos(theta(3)), zero ]
        rt3(3, :) = [ 0.0, 0.0, 1.0 ]
        rt = matx(rt3, matx(rt2, rt1))
        rotate = .true.
    else
        rotate = .false.
    end if
    ! The normalization fator is h = product(sigma)*(2*const_pi)**1.5d0
    f = zeros(n1, n2, n3)
    if (rotate) then
        !$omp parallel do private(i, j, k, g1, g2, g3)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    g1 = (x(i) - mu(1))*rt(1, 1) + (y(j) - mu(2))*rt(1, 2) + (z(k) - mu(3))*rt(1, 3)
                    g2 = (x(i) - mu(1))*rt(2, 1) + (y(j) - mu(2))*rt(2, 2) + (z(k) - mu(3))*rt(2, 3)
                    g3 = (x(i) - mu(1))*rt(3, 1) + (y(j) - mu(2))*rt(3, 2) + (z(k) - mu(3))*rt(3, 3)
                    f(i, j, k) = exp(-0.5*((g1/sigma(1))**2 + (g2/sigma(2))**2 + (g3/sigma(3))**2))
                end do
            end do
        end do
        !$omp end parallel do
    else
        !$omp parallel do private(i, j, k)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    f(i, j, k) = exp(-0.5*((x(i)/sigma(1))**2 + (y(j)/sigma(2))**2 + (z(k)/sigma(3))**2))
                end do
            end do
        end do
        !$omp end parallel do
    end if
end function gaussian_3d_double
!
!>
!> Multidimensional Cauchy distribution follows:
!> f(x) = Gamma((1 + d)/2)/(Gamma(1/2)*pi^(d/2)*product(gamma))*(1 + sum_{i=1}^d ((xi - mu_i)/gamma_i)^2)^(-(1 + d)/2)
!> Therefore,
!> 1D: f(x1) = 1/(pi*gamma_1)*(1 + ((x_1 - mu_1)/gamma_1)^2)^(-1)
!> 2D: f(x1, x2) = 1/(2*pi*gamma_1*gamma_2)*(1 + ((x_1 - mu_1)/gamma_1)^2
!> + ((x_2 - mu_2)/gamma_2)^2)^(-3/2)
!> 3D: f(x1, x2, x3) = 1/(pi^2*gamma_1*gamma_2*gamma_3)*(1 + ((x_1 - mu_1)/gamma_1)^2
!> + ((x_2 - mu_2)/gamma_2)^2
!> + ((x_3 - mu_3)/gamma_3)^2)^(-2)
!>
!
!> 1D Cauchy
!
function cauchy_1d_double(x, mu, sigma) result(f)
    double precision, intent(in) :: mu, sigma
    double precision, dimension(:), intent(in) :: x
    double precision, allocatable, dimension(:) :: f
    ! The normalization factor is const_pi*sigma
    f = (1.0 + ((x - mu)/sigma)**2)**(-1.0d0)
end function cauchy_1d_double
!
!> 2D Cauchy
!
function cauchy_2d_double(x, y, mu, sigma, theta) result(f)
    double precision, dimension(:), intent(in) :: x, y
    double precision, dimension(1:2), intent(in) :: mu, sigma
    double precision, intent(in), optional :: theta
    double precision, allocatable, dimension(:, :) :: f
    integer :: i, j
    integer :: n1, n2
    double precision, allocatable, dimension(:, :) :: rt
    double precision :: g1, g2
    logical :: rotate
    n1 = size(x)
    n2 = size(y)
    if (present(theta)) then
        rt = zeros(2, 2)
        rt(1, :) = [cos(theta), -sin(theta)]
        rt(2, :) = [sin(theta), cos(theta)]
        rotate = .true.
    else
        rotate = .false.
    end if
    ! The normalization factoris h = product(sigma)*(2*const_pi)
    f = zeros(n1, n2)
    if (rotate) then
        !$omp parallel do private(i, j, g1, g2)
        do j = 1, n2
            do i = 1, n1
                g1 = (x(i) - mu(1))*rt(1, 1) + (y(j) - mu(2))*rt(1, 2)
                g2 = (x(i) - mu(1))*rt(2, 1) + (y(j) - mu(2))*rt(2, 2)
                f(i, j) = (1.0 + ((g1/sigma(1))**2 + (g2/sigma(2))**2))**(-1.5d0)
            end do
        end do
        !$omp end parallel do
    else
        !$omp parallel do private(i, j)
        do j = 1, n2
            do i = 1, n1
                f(i, j) = (1.0 + ((x(i)/sigma(1))**2 + (y(j)/sigma(2))**2))**(-1.5d0)
            end do
        end do
        !$omp end parallel do
    end if
end function cauchy_2d_double
!
!> 3D Cauchy
!
function cauchy_3d_double(x, y, z, mu, sigma, theta) result(f)
    double precision, dimension(:), intent(in) :: x, y, z
    double precision, dimension(1:3), intent(in) :: mu, sigma
    double precision, dimension(1:3), intent(in), optional :: theta
    double precision, allocatable, dimension(:, :, :) :: f
    integer :: i, j, k
    integer :: n1, n2, n3
    double precision, allocatable, dimension(:, :) :: rt1, rt2, rt3, rt
    double precision :: g1, g2, g3
    double precision :: zero
    logical :: rotate
    n1 = size(x)
    n2 = size(y)
    n3 = size(z)
    zero = 0.0
    if (present(theta)) then
        rt1 = zeros(3, 3)
        rt2 = zeros(3, 3)
        rt3 = zeros(3, 3)
        rt1(1, :) = [ 1.0, 0.0, 0.0 ]
        rt1(2, :) = [ zero, cos(theta(1)), -sin(theta(1))]
        rt1(3, :) = [ zero, sin(theta(1)), cos(theta(1))]
        rt2(1, :) = [ cos(theta(2)), zero, sin(theta(2))]
        rt2(2, :) = [ 0.0, 1.0, 0.0]
        rt2(3, :) = [-sin(theta(2)), zero, cos(theta(2))]
        rt3(1, :) = [ cos(theta(3)), -sin(theta(3)), zero ]
        rt3(2, :) = [ sin(theta(3)), cos(theta(3)), zero ]
        rt3(3, :) = [ 0.0, 0.0, 1.0 ]
        rt = matx(rt3, matx(rt2, rt1))
        rotate = .true.
    else
        rotate = .false.
    end if
    ! The normalization factor is h = product(sigma)*const_pi**2
    f = zeros(n1, n2, n3)
    if (rotate) then
        !$omp parallel do private(i, j, k, g1, g2, g3)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    g1 = (x(i) - mu(1))*rt(1, 1) + (y(j) - mu(2))*rt(1, 2) + (z(k) - mu(3))*rt(1, 3)
                    g2 = (x(i) - mu(1))*rt(2, 1) + (y(j) - mu(2))*rt(2, 2) + (z(k) - mu(3))*rt(2, 3)
                    g3 = (x(i) - mu(1))*rt(3, 1) + (y(j) - mu(2))*rt(3, 2) + (z(k) - mu(3))*rt(3, 3)
                    f(i, j, k) = (1.0 + ((g1/sigma(1))**2 + (g2/sigma(2))**2 + (g3/sigma(3))**2))**(-2.0d0)
                end do
            end do
        end do
        !$omp end parallel do
    else
        !$omp parallel do private(i, j, k)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    f(i, j, k) = (1.0 + ((x(i)/sigma(1))**2 + (y(j)/sigma(2))**2 + (z(k)/sigma(3))**2))**(-2.0d0)
                end do
            end do
        end do
        !$omp end parallel do
    end if
end function cauchy_3d_double
function covariance_1d_double(x, y) result(c)
    double precision, dimension(:), intent(in) :: x, y
    double precision :: c
    double precision :: mx, my
    integer :: nx, ny
    nx = size(x)
    ny = size(y)
    call assert(nx == ny, ' <covariance_1d_> Error: size(x) /= size(y)')
    mx = mean(x)
    my = mean(y)
    c = sum((x - mx)*(y - my))
    if (nx > 1) then
        c = c/(nx - 1.0)
    end if
end function covariance_1d_double
function covariance_2d_double(x, y) result(c)
    double precision, dimension(:, :), intent(in) :: x, y
    double precision :: c
    double precision :: mx, my
    integer :: nx1, ny1, nx2, ny2
    nx1 = size(x, 1)
    ny1 = size(y, 1)
    call assert(nx1 == ny1, ' <covariance_2d_> Error: size(x, 1) /= size(y, 1)')
    nx2 = size(x, 2)
    ny2 = size(y, 2)
    call assert(nx2 == ny2, ' <covariance_2d_> Error: size(x, 2) /= size(y, 2)')
    mx = mean(x)
    my = mean(y)
    c = sum((x - mx)*(y - my))
    if (nx1*nx2 > 1) then
        c = c/(nx1*nx2 - 1.0)
    end if
end function covariance_2d_double
function covariance_3d_double(x, y) result(c)
    double precision, dimension(:, :, :), intent(in) :: x, y
    double precision :: c
    double precision :: mx, my
    integer :: nx1, ny1, nx2, ny2, nx3, ny3
    nx1 = size(x, 1)
    ny1 = size(y, 1)
    call assert(nx1 == ny1, ' <covariance_3d_> Error: size(x, 1) /= size(y, 1)')
    nx2 = size(x, 2)
    ny2 = size(y, 2)
    call assert(nx2 == ny2, ' <covariance_3d_> Error: size(x, 2) /= size(y, 2)')
    nx3 = size(x, 3)
    ny3 = size(y, 3)
    call assert(nx3 == ny3, ' <covariance_3d_> Error: size(x, 3) /= size(y, 3)')
    mx = mean(x)
    my = mean(y)
    c = sum((x - mx)*(y - my))
    if (nx1*nx2*nx3 > 1) then
        c = c/(nx1*nx2*nx3 - 1.0)
    end if
end function covariance_3d_double
!
!> Compute the unbiased standard deviation of a 1D array
!
function standard_deviation_1d_double(w) result(s)
    double precision, dimension(:), intent(in) :: w
    double precision :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = sqrt(s/(size(w) - 1))
    end if
end function standard_deviation_1d_double
!
!> Compute the unbiased standard deviation of a 2D array
!
function standard_deviation_2d_double(w) result(s)
    double precision, dimension(:, :), intent(in) :: w
    double precision :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = sqrt(s/(size(w) - 1))
    end if
end function standard_deviation_2d_double
!
!> Compute the unbiased standard deviation of a 3D array
!
function standard_deviation_3d_double(w) result(s)
    double precision, dimension(:, :, :), intent(in) :: w
    double precision :: s
    s = sum((w - mean(w))**2)
    if (size(w) > 1) then
        s = sqrt(s/(size(w) - 1))
    end if
end function standard_deviation_3d_double
!
!> Compute the histogram of a 1D array
!
subroutine histogram_1d_double(w, hist, valmin, valmax, binsize)
    double precision, dimension(:), intent(in) :: w
    double precision, allocatable, dimension(:, :), intent(inout) :: hist
    double precision, intent(in), optional :: valmin, valmax, binsize
    double precision :: histmin, histmax, histbin
    integer :: nbin, i
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! allocate hist
    call alloc_array(hist, [1, nbin, 1, 4])
    ! compute hist
    !$omp parallel do private(i)
    do i = 1, nbin
        if (i == 1) then
            hist(i, 1) = histmin
        else
            hist(i, 1) = histmin + (i - 1)*histbin
        end if
        if (i == nbin) then
            hist(i, 2) = histmax
        else
            hist(i, 2) = histmin + i*histbin
        end if
        if (i < nbin) then
            hist(i, 3) = count(w >= hist(i, 1) .and. w < hist(i, 2), kind=8)
        else
            hist(i, 3) = count(w >= hist(i, 1) .and. w <= hist(i, 2), kind=8)
        end if
    end do
    !$omp end parallel do
    ! normalize
    hist(:, 4) = hist(:, 3)/size(w, kind=8)
end subroutine histogram_1d_double
!
!> Compute the histogram of a 2D array
!
subroutine histogram_2d_double(w, hist, valmin, valmax, binsize)
    double precision, dimension(:, :), intent(in) :: w
    double precision, allocatable, dimension(:, :), intent(inout) :: hist
    double precision, intent(in), optional :: valmin, valmax, binsize
    double precision :: histmin, histmax, histbin
    integer :: nbin, i
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! allocate hist
    call alloc_array(hist, [1, nbin, 1, 4])
    ! compute hist
    !$omp parallel do private(i)
    do i = 1, nbin
        if (i == 1) then
            hist(i, 1) = histmin
        else
            hist(i, 1) = histmin + (i - 1)*histbin
        end if
        if (i == nbin) then
            hist(i, 2) = histmax
        else
            hist(i, 2) = histmin + i*histbin
        end if
        if (i < nbin) then
            hist(i, 3) = count(w >= hist(i, 1) .and. w < hist(i, 2))
        else
            hist(i, 3) = count(w >= hist(i, 1) .and. w <= hist(i, 2))
        end if
    end do
    !$omp end parallel do
    ! normalize
    hist(:, 4) = hist(:, 3)/size(w, 1)/size(w, 2)
end subroutine histogram_2d_double
!
!> Compute the histogram of a 3D array
!
subroutine histogram_3d_double(w, hist, valmin, valmax, binsize)
    double precision, dimension(:, :, :), intent(in) :: w
    double precision, allocatable, dimension(:, :), intent(inout) :: hist
    double precision, intent(in), optional :: valmin, valmax, binsize
    double precision :: histmin, histmax, histbin
    integer :: nbin, i
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! allocate hist
    call alloc_array(hist, [1, nbin, 1, 4])
    ! compute hist
    !$omp parallel do private(i)
    do i = 1, nbin
        if (i == 1) then
            hist(i, 1) = histmin
        else
            hist(i, 1) = histmin + (i - 1)*histbin
        end if
        if (i == nbin) then
            hist(i, 2) = histmax
        else
            hist(i, 2) = histmin + i*histbin
        end if
        if (i < nbin) then
            hist(i, 3) = count(w >= hist(i, 1) .and. w < hist(i, 2))
        else
            hist(i, 3) = count(w >= hist(i, 1) .and. w <= hist(i, 2))
        end if
    end do
    !$omp end parallel do
    ! normalize
    hist(:, 4) = hist(:, 3)/size(w, 1)/size(w, 2)/size(w, 3)
end subroutine histogram_3d_double
!
!> Plot the histogram of a 1D array
!
subroutine plot_histogram_1d_double(w, valmin, valmax, binsize, label)
    double precision, dimension(:), intent(in) :: w
    double precision, intent(in), optional :: valmin, valmax, binsize
    character(len=*), intent(in), optional :: label
    double precision :: histmin, histmax, histbin
    integer :: nbin, i, j
    double precision, allocatable, dimension(:, :) :: hist
    character(len=20) :: bar
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! compute history
    call histogram_1d_double(w, hist, histmin, histmax, histbin)
    nbin = size(hist, 1)
    ! compute hist
    if (present(label)) then
        write (error_unit, '(x,a)') label
    end if
    do i = 1, nbin
        bar = ''
        do j = 1, nint(hist(i, 4)/maxval(hist(:, 4))*len(bar))
            bar(j:j) = '*'
        end do
        write (error_unit, '(x,es12.5,x,a,x,es12.5,x,x,a,x,i12,x,es12.5)') &
            hist(i, 1), '~', hist(i, 2), bar, int(hist(i, 3), kind=8), hist(i, 4)
    end do
end subroutine plot_histogram_1d_double
!
!> Plot the histogram of a 2D array
!
subroutine plot_histogram_2d_double(w, valmin, valmax, binsize, label)
    double precision, dimension(:, :), intent(in) :: w
    double precision, intent(in), optional :: valmin, valmax, binsize
    character(len=*), intent(in), optional :: label
    double precision :: histmin, histmax, histbin
    integer :: nbin, i, j
    double precision, allocatable, dimension(:, :) :: hist
    character(len=20) :: bar
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! compute history
    call histogram_2d_double(w, hist, histmin, histmax, histbin)
    nbin = size(hist, 1)
    ! compute hist
    if (present(label)) then
        write (error_unit, '(x,a)') label
    end if
    do i = 1, nbin
        bar = ''
        do j = 1, nint(hist(i, 4)/maxval(hist(:, 4))*len(bar))
            bar(j:j) = '*'
        end do
        write (error_unit, '(x,es12.5,x,a,x,es12.5,x,x,a,x,i12,x,es12.5)') &
            hist(i, 1), '~', hist(i, 2), bar, int(hist(i, 3), kind=8), hist(i, 4)
    end do
end subroutine plot_histogram_2d_double
!
!> Plot the histogram of a 3D array
!
subroutine plot_histogram_3d_double(w, valmin, valmax, binsize, label)
    double precision, dimension(:, :, :), intent(in) :: w
    double precision, intent(in), optional :: valmin, valmax, binsize
    character(len=*), intent(in), optional :: label
    double precision :: histmin, histmax, histbin
    integer :: nbin, i, j
    double precision, allocatable, dimension(:, :) :: hist
    character(len=20) :: bar
    if (present(valmin)) then
        histmin = valmin
    else
        histmin = minval(w)
    end if
    if (present(valmax)) then
        histmax = valmax
    else
        histmax = maxval(w)
    end if
    if (present(binsize)) then
        histbin = binsize
        nbin = ceiling((histmax - histmin)/histbin)
    else
        nbin = 9
        histbin = (histmax - histmin)/nbin
    end if
    if (histbin == 0) then
        nbin = 1
    end if
    ! compute history
    call histogram_3d_double(w, hist, histmin, histmax, histbin)
    nbin = size(hist, 1)
    ! compute hist
    if (present(label)) then
        write (error_unit, '(a)') label
    end if
    do i = 1, nbin
        bar = ''
        do j = 1, nint(hist(i, 4)/maxval(hist(:, 4))*len(bar))
            bar(j:j) = '*'
        end do
        write (error_unit, '(x,es12.5,x,a,x,es12.5,x,x,a,x,i12,x,es12.5)') &
            hist(i, 1), '~', hist(i, 2), bar, int(hist(i, 3), kind=8), hist(i, 4)
    end do
end subroutine plot_histogram_3d_double
function median_1d_double(w) result(m)
    double precision, dimension(:), intent(in) :: w
    double precision :: m
    double precision, allocatable, dimension(:) :: wt
    integer(kind=8) :: n
    n = size(w, kind=8)
    ! wt = w !call alloc_array(wt, [1, n], source=w)
    wt = sort(w) !wt)
    if (mod(n, 2) == 0) then
        m = 0.5*(wt(n/2) + wt(n/2 + 1))
    else
        m = wt((n + 1)/2)
    end if
end function median_1d_double
function median_2d_double(w) result(m)
    double precision, dimension(:, :), intent(in) :: w
    double precision :: m
    double precision, allocatable, dimension(:) :: wt
    integer(kind=8) :: n
    n = size(w, kind=8) !size(w, 1)*size(w, 2)
    ! wt = flatten(w) !call alloc_array(wt, [1, n], source=reshape(w, [n]))
    wt = sort(flatten(w))
    if (mod(n, 2) == 0) then
        m = 0.5*(wt(n/2) + wt(n/2 + 1))
    else
        m = wt((n + 1)/2)
    end if
end function median_2d_double
function median_3d_double(w) result(m)
    double precision, dimension(:, :, :), intent(in) :: w
    double precision :: m
    double precision, allocatable, dimension(:) :: wt
    integer(kind=8) :: n
    n = size(w, kind=8) ! size(w, 1)*size(w, 2)*size(w, 3)
    ! call alloc_array(wt, [1, n], source=reshape(w, [n]))
    wt = sort(flatten(w)) !wt)
    if (mod(n, 2) == 0) then
        m = 0.5*(wt(n/2) + wt(n/2 + 1))
    else
        m = wt((n + 1)/2)
    end if
end function median_3d_double
!
!> 1D discrete crosscorrelation
!
subroutine discrete_xcorr_1d_double(x, y, z, kx, ky, kz)
    double precision, dimension(:), intent(in) :: x, y
    double precision, dimension(:), intent(inout) :: z
    integer, intent(in) :: kx, ky, kz
    integer :: lx, ly, lz
    integer :: kxr, i
    double precision, allocatable, dimension(:) :: xr
    lx = size(x, 1)
    ly = size(y, 1)
    lz = size(z, 1)
    allocate (xr(1:lx))
    do i = 1, lx
        xr(i) = x(lx - i + 1)
    end do
    kxr = 1 - (kx - 1) - lx
    call convd(xr, y, z, &
        kxr, &
        ky, &
        kz)
    deallocate (xr)
end subroutine discrete_xcorr_1d_double
!
!> 2D discrete crosscorrelation
!
subroutine discrete_xcorr_2d_double(x, y, z, &
        kx1, kx2, &
        ky1, ky2, &
        kz1, kz2)
    double precision, dimension(:, :), intent(in) :: x, y
    double precision, dimension(:, :), intent(inout) :: z
    integer, intent(in) :: kx1, ky1, kz1
    integer, intent(in) :: kx2, ky2, kz2
    integer :: lx1, ly1, lz1
    integer :: lx2, ly2, lz2
    double precision, allocatable, dimension(:, :) :: xr
    integer :: i, j, kx1r, kx2r
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    allocate (xr(1:lx1, 1:lx2))
    do j = 1, lx2
        do i = 1, lx1
            xr(i, j) = x(lx1 - i + 1, lx2 - j + 1)
        end do
    end do
    kx1r = 1 - (kx1 - 1) - lx1
    kx2r = 1 - (kx2 - 1) - lx2
    call convd(xr, y, z, &
        kx1r, kx2r, &
        ky1, ky2, &
        kz1, kz2)
    deallocate (xr)
end subroutine discrete_xcorr_2d_double
!
!> 3D discrete crosscorrelation
!
subroutine discrete_xcorr_3d_double(x, y, z, &
        kx1, kx2, kx3, &
        ky1, ky2, ky3, &
        kz1, kz2, kz3)
    integer, intent(in) :: kx1, ky1, kz1
    integer, intent(in) :: kx2, ky2, kz2
    integer, intent(in) :: kx3, ky3, kz3
    double precision, dimension(:, :, :), intent(in) :: x, y
    double precision, dimension(:, :, :), intent(inout) :: z
    integer :: lx1, ly1, lz1
    integer :: lx2, ly2, lz2
    integer :: lx3, ly3, lz3
    double precision, allocatable, dimension(:, :, :) :: xr
    integer :: i, j, k, kx1r, kx2r, kx3r
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    lx3 = size(x, 3)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    ly3 = size(y, 3)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    lz3 = size(z, 3)
    allocate (xr(1:lx1, 1:lx2, 1:lx3))
    do k = 1, lx3
        do j = 1, lx2
            do i = 1, lx1
                xr(i, j, k) = x(lx1 - i + 1, lx2 - j + 1, lx3 - k + 1)
            end do
        end do
    end do
    kx1r = 1 - (kx1 - 1) - lx1
    kx2r = 1 - (kx2 - 1) - lx2
    kx3r = 1 - (kx3 - 1) - lx3
    call convd(xr, y, z, &
        kx1r, kx2r, kx3r, &
        ky1, ky2, ky3, &
        kz1, kz2, kz3)
    deallocate (xr)
end subroutine discrete_xcorr_3d_double
!
!> 1D FFT-based cross-correlation
!
function xcorr_1d_double(x, y, maxlag) result(z)
    double precision, dimension(:), intent(in) :: x, y
    integer, intent(in), optional :: maxlag
    integer :: nx, ny, n, nlag
    double precision, allocatable, dimension(:) :: z
    ! Dimensions
    nx = size(x)
    ny = size(y)
    if (present(maxlag)) then
        nlag = min(maxlag, max(nx, ny))
    else
        nlag = max(nx, ny) - 1
    end if
    ! Pad to next power 235
    n = next_power_235(max(nx, ny) + nlag)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    !
    ! Cross-correlation has different definitions in different literature
    ! The definition adopted here is
    ! C_xy(m) = sum [x(n + m) * y^*(n)]
    ! Some literature defines
    ! C_xy(m) = sum [x^*(n) * y(n + m)]
    ! which gives reversed result compared with the first definition
    ! The first definition is adopted in MATLAB and numpy, and is therefore
    ! adopted here as well.
    !
    allocate (z(1:n))
    z = fftshift(ifft( &
        fft(pad(x, [0, n - nx], ['const', 'const']))* &
        conjg(fft(pad(y, [0, n - ny], ['const', 'const']))), real=.true.))
    call alloc_array(z, [-nlag, nlag], &
        source=z(nint((n + 1)/2.0) - nlag:nint((n + 1)/2.0) + nlag))
end function xcorr_1d_double
!
!> 2D FFT-based cross-correlation
!
function xcorr_2d_double(x, y, maxlag) result(z)
    double precision, dimension(:, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    integer :: nx1, ny1, nx2, ny2, n1, n2, nlag1, nlag2
    double precision, allocatable, dimension(:, :) :: z
    ! Dimensions
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(nx1, ny1))
        nlag2 = min(maxlag(2), max(nx2, ny2))
    else
        nlag1 = max(nx1, ny1) - 1
        nlag2 = max(nx2, ny2) - 1
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(nx1, ny1) + nlag1)
    n2 = next_power_235(max(nx2, ny2) + nlag2)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    allocate (z(1:n1, 1:n2))
    z = fftshift(ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2], ['const', 'const', 'const', 'const']))* &
        conjg(fft(pad(y, [0, n1 - ny1, 0, n2 - ny2], ['const', 'const', 'const', 'const']))), real=.true.))
    call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2], &
        source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
        nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2))
end function xcorr_2d_double
!
!> 3D FFT-based cross-correlation
!
function xcorr_3d_double(x, y, maxlag) result(z)
    double precision, dimension(:, :, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    integer :: nx1, ny1, nx2, ny2, nx3, ny3, n1, n2, n3, nlag1, nlag2, nlag3
    double precision, allocatable, dimension(:, :, :) :: z
    ! Dimensions
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    nx3 = size(x, 3)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    ny3 = size(y, 3)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(nx1, ny1))
        nlag2 = min(maxlag(2), max(nx2, ny2))
        nlag3 = min(maxlag(3), max(nx3, ny3))
    else
        nlag1 = max(nx1, ny1) - 1
        nlag2 = max(nx2, ny2) - 1
        nlag3 = max(nx3, ny3) - 1
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(nx1, ny1) + nlag1)
    n2 = next_power_235(max(nx2, ny2) + nlag2)
    n3 = next_power_235(max(nx3, ny3) + nlag3)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    allocate (z(1:n1, 1:n2, 1:n3))
    z = fftshift(ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2, 0, n3 - nx3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))* &
        conjg(fft(pad(y, [0, n1 - ny1, 0, n2 - ny2, 0, n3 - ny3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))), real=.true.))
    call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2, -nlag3, nlag3], &
        source=z( &
        nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
        nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2, &
        nint((n3 + 1)/2.0) - nlag3:nint((n3 + 1)/2.0) + nlag3))
end function xcorr_3d_double
!
!> 1D discrete autocorrelation
!
subroutine discrete_acorr_1d_double(x, z, kx, kz)
    double precision, dimension(:), intent(in) :: x
    double precision, dimension(:), intent(inout) :: z
    integer, intent(in) :: kx, kz
    integer :: lx, lz
    integer :: kxr, i
    double precision, allocatable, dimension(:) :: xr
    lx = size(x, 1)
    lz = size(z, 1)
    allocate (xr(1:lx))
    do i = 1, lx
        xr(i) = x(lx - i + 1)
    end do
    kxr = 1 - (kx - 1) - lx
    call convd(xr, x, z, &
        kxr, &
        kx, &
        kz)
    deallocate (xr)
end subroutine discrete_acorr_1d_double
!
!> 2D discrete autocorrelation
!
subroutine discrete_acorr_2d_double(x, z, &
        kx1, kx2, &
        kz1, kz2)
    double precision, dimension(:, :), intent(in) :: x
    double precision, dimension(:, :), intent(inout) :: z
    integer, intent(in) :: kx1, kz1
    integer, intent(in) :: kx2, kz2
    integer :: lx1, lz1
    integer :: lx2, lz2
    double precision, allocatable, dimension(:, :) :: xr
    integer :: i, j, kx1r, kx2r
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    allocate (xr(1:lx1, 1:lx2))
    do j = 1, lx2
        do i = 1, lx1
            xr(i, j) = x(lx1 - i + 1, lx2 - j + 1)
        end do
    end do
    kx1r = 1 - (kx1 - 1) - lx1
    kx2r = 1 - (kx2 - 1) - lx2
    call convd(xr, x, z, &
        kx1r, kx2r, &
        kx1, kx2, &
        kz1, kz2)
    deallocate (xr)
end subroutine discrete_acorr_2d_double
!
!> 3D discrete autocorrelation
!
subroutine discrete_acorr_3d_double(x, z, &
        kx1, kx2, kx3, &
        kz1, kz2, kz3)
    integer, intent(in) :: kx1, kz1
    integer, intent(in) :: kx2, kz2
    integer, intent(in) :: kx3, kz3
    double precision, dimension(:, :, :), intent(in) :: x
    double precision, dimension(:, :, :), intent(inout) :: z
    integer :: lx1, lz1
    integer :: lx2, lz2
    integer :: lx3, lz3
    double precision, allocatable, dimension(:, :, :) :: xr
    integer :: i, j, k, kx1r, kx2r, kx3r
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    lx3 = size(x, 3)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    lz3 = size(z, 3)
    allocate (xr(1:lx1, 1:lx2, 1:lx3))
    do k = 1, lx3
        do j = 1, lx2
            do i = 1, lx1
                xr(i, j, k) = x(lx1 - i + 1, lx2 - j + 1, lx3 - k + 1)
            end do
        end do
    end do
    kx1r = 1 - (kx1 - 1) - lx1
    kx2r = 1 - (kx2 - 1) - lx2
    kx3r = 1 - (kx3 - 1) - lx3
    call convd(xr, x, z, &
        kx1r, kx2r, kx3r, &
        kx1, kx2, kx3, &
        kz1, kz2, kz3)
    deallocate (xr)
end subroutine discrete_acorr_3d_double
!
!> 1D FFT-based auto-correlation
!
function acorr_1d_double(x, maxlag) result(z)
    double precision, dimension(:), intent(in) :: x
    integer, intent(in), optional :: maxlag
    integer :: nx, n, nlag
    double precision, allocatable, dimension(:) :: z
    ! Dimensions
    nx = size(x)
    if (present(maxlag)) then
        nlag = min(maxlag, nx)
    else
        nlag = nx - 1
    end if
    ! Pad to next power 235
    n = next_power_235(nx + nlag)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    allocate (z(1:n))
    z = fftshift(ifft( &
        dcmplx(fft(pad(x, [0, n - nx], ['const', 'const']))**2), real=.true.))
    call alloc_array(z, [-nlag, nlag], &
        source=z(nint((n + 1)/2.0) - nlag:nint((n + 1)/2.0) + nlag))
end function acorr_1d_double
!
!> 2D FFT-based auto-correlation
!
function acorr_2d_double(x, maxlag) result(z)
    double precision, dimension(:, :), intent(in) :: x
    integer, dimension(:), intent(in), optional :: maxlag
    integer :: nx1, nx2, n1, n2, nlag1, nlag2
    double precision, allocatable, dimension(:, :) :: z
    ! Dimensions
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), nx1)
        nlag2 = min(maxlag(2), nx2)
    else
        nlag1 = nx1 - 1
        nlag2 = nx2 - 1
    end if
    ! Pad to next power 235
    n1 = next_power_235(nx1 + nlag1)
    n2 = next_power_235(nx2 + nlag2)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    allocate (z(1:n1, 1:n2))
    z = fftshift(ifft( &
        dcmplx(fft(pad(x, [0, n1 - nx1, 0, n2 - nx2], &
        ['const', 'const', 'const', 'const']))**2), real=.true.))
    call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2], &
        source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
        nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2))
end function acorr_2d_double
!
!> 3D FFT-based auto-correlation
!
function acorr_3d_double(x, maxlag) result(z)
    double precision, dimension(:, :, :), intent(in) :: x
    integer, dimension(:), intent(in), optional :: maxlag
    integer :: nx1, nx2, nx3, n1, n2, n3, nlag1, nlag2, nlag3
    double precision, allocatable, dimension(:, :, :) :: z
    ! Dimensions
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    nx3 = size(x, 3)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), nx1)
        nlag2 = min(maxlag(2), nx2)
        nlag3 = min(maxlag(3), nx3)
    else
        nlag1 = nx1 - 1
        nlag2 = nx2 - 1
        nlag3 = nx3 - 1
    end if
    ! Pad to next power 235
    n1 = next_power_235(nx1 + nlag1)
    n2 = next_power_235(nx2 + nlag2)
    n3 = next_power_235(nx3 + nlag3)
    ! Do cross-correlation using FFT
    ! What FFT does is circular cross-correlation, therefore the
    ! result must be shifted and selected
    allocate (z(1:n1, 1:n2, 1:n3))
    z = fftshift(ifft( &
        dcmplx(fft(pad(x, [0, n1 - nx1, 0, n2 - nx2, 0, n3 - nx3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))**2), real=.true.))
    call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2, -nlag3, nlag3], &
        source=z( &
        nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
        nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2, &
        nint((n3 + 1)/2.0) - nlag3:nint((n3 + 1)/2.0) + nlag3))
end function acorr_3d_double
!
!> 1D discrete cross-correlation
!
subroutine local_xcorr_1d_double(tr1, tr2, wt, ccoef, tshift)
    double precision, dimension(:), intent(in) :: tr1, tr2
    double precision, dimension(:), intent(inout) :: ccoef, tshift
    integer, intent(in) :: wt
    integer :: nt, i, ns
    double precision, allocatable, dimension(:) :: ptr1, ptr2, pw1, pw2, pc
    double precision :: norm
    ! number of samples
    nt = size(tr1, 1)
    if (maxval(abs(tr1)) == 0 .or. maxval(abs(tr2)) == 0) then
        ccoef = 1.0
        tshift = 0.0
    else
        ns = floor(wt/3.0)
        call alloc_array(ptr1, [-wt + 1, nt + wt])
        call alloc_array(ptr2, [-wt + 1, nt + wt])
        ptr1(1:nt) = tr1
        ptr2(1:nt) = tr2
        call alloc_array(pw1, [-wt, wt])
        call alloc_array(pw1, [-wt, wt])
        call alloc_array(pc, [-ns, ns])
        do i = 1, nt
            pw1 = ptr1(i - wt:i + wt)
            pw2 = ptr2(i - wt:i + wt)
            call discrete_xcorr_1d_double(pw1, pw2, pc, 1, 1, -ns)
            norm = norm2(pw1)*norm2(pw2)
            if (norm == 0) then
                ccoef(i) = 0.0
                tshift(i) = 0.0
            else
                ccoef(i) = maxval(pc)/norm
                tshift(i) = maxloc(pc, 1) - ns - 1.0
            end if
        end do
    end if
end subroutine local_xcorr_1d_double
!
!> Zero-lag cross-correlation coefficient
!
function xcorr_coef_1d_double(a, b) result(r)
    double precision, dimension(:), intent(in) :: a
    double precision, dimension(:), intent(in) :: b
    double precision :: r
    double precision :: ma, mb
    call assert(size(a) == size(b), 'Error: size(a) /= size(b)')
    ma = mean(a)
    mb = mean(b)
    r = sum((a - ma)*(b - mb))/sqrt(sum((a - ma)**2))/sqrt(sum((b - mb)**2))
end function xcorr_coef_1d_double
function xcorr_coef_2d_double(a, b) result(r)
    double precision, dimension(:, :), intent(in) :: a
    double precision, dimension(:, :), intent(in) :: b
    double precision :: r
    double precision :: ma, mb
    call assert(size(a, 1) == size(b, 1), 'Error: size(a, 1) /= size(b, 1)')
    call assert(size(a, 2) == size(b, 2), 'Error: size(a, 2) /= size(b, 2)')
    ma = mean(a)
    mb = mean(b)
    r = sum((a - ma)*(b - mb))/sqrt(sum((a - ma)**2))/sqrt(sum((b - mb)**2))
end function xcorr_coef_2d_double
function xcorr_coef_3d_double(a, b) result(r)
    double precision, dimension(:, :, :), intent(in) :: a
    double precision, dimension(:, :, :), intent(in) :: b
    double precision :: r
    double precision :: ma, mb
    call assert(size(a, 1) == size(b, 1), 'Error: size(a, 1) /= size(b, 1)')
    call assert(size(a, 2) == size(b, 2), 'Error: size(a, 2) /= size(b, 2)')
    call assert(size(a, 3) == size(b, 3), 'Error: size(a, 3) /= size(b, 3)')
    ma = mean(a)
    mb = mean(b)
    r = sum((a - ma)*(b - mb))/sqrt(sum((a - ma)**2))/sqrt(sum((b - mb)**2))
end function xcorr_coef_3d_double
    ! Some can be extended to complex domain
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
!> Compute the mean of a 1D array
!
function mean_1d_float(w, power) result(m)
    real, dimension(:), intent(in) :: w
    integer, intent(in), optional :: power
    real :: m
    integer :: p
    if (present(power)) then
        p = power
        m = real((sum(dble(w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = real(sum(dble(w))/size(w))
    end if
end function mean_1d_float
!
!> Compute the mean of a 2D array
!
function mean_2d_float(w, power) result(m)
    real, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: power
    real :: m
    integer :: p
    if (present(power)) then
        p = power
        m = real((sum(dble(w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = real(sum(dble(w))/size(w))
    end if
end function mean_2d_float
!
!> Compute the mean of a 3D array
!
function mean_3d_float(w, power) result(m)
    real, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: power
    real :: m
    integer :: p
    if (present(power)) then
        p = power
        m = real((sum(dble(w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = real(sum(dble(w))/size(w))
    end if
end function mean_3d_float
!
!> Compute the mean of a 4D array
!
function mean_4d_float(w, power) result(m)
    real, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in), optional :: power
    real :: m
    integer :: p
    if (present(power)) then
        p = power
        m = real((sum(dble(w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = real(sum(dble(w))/size(w))
    end if
end function mean_4d_float
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
!> Compute the mean of a 1D array
!
function mean_1d_double(w, power) result(m)
    double precision, dimension(:), intent(in) :: w
    integer, intent(in), optional :: power
    double precision :: m
    integer :: p
    if (present(power)) then
        p = power
        m = ((sum((w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = (sum((w))/size(w))
    end if
end function mean_1d_double
!
!> Compute the mean of a 2D array
!
function mean_2d_double(w, power) result(m)
    double precision, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: power
    double precision :: m
    integer :: p
    if (present(power)) then
        p = power
        m = ((sum((w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = (sum((w))/size(w))
    end if
end function mean_2d_double
!
!> Compute the mean of a 3D array
!
function mean_3d_double(w, power) result(m)
    double precision, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: power
    double precision :: m
    integer :: p
    if (present(power)) then
        p = power
        m = ((sum((w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = (sum((w))/size(w))
    end if
end function mean_3d_double
!
!> Compute the mean of a 4D array
!
function mean_4d_double(w, power) result(m)
    double precision, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in), optional :: power
    double precision :: m
    integer :: p
    if (present(power)) then
        p = power
        m = ((sum((w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = (sum((w))/size(w))
    end if
end function mean_4d_double
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
!> Compute the mean of a 1D array
!
function mean_1d_complex(w, power) result(m)
    complex, dimension(:), intent(in) :: w
    integer, intent(in), optional :: power
    complex :: m
    integer :: p
    if (present(power)) then
        p = power
        m = cmplx((sum(dcmplx(w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = cmplx(sum(dcmplx(w))/size(w))
    end if
end function mean_1d_complex
!
!> Compute the mean of a 2D array
!
function mean_2d_complex(w, power) result(m)
    complex, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: power
    complex :: m
    integer :: p
    if (present(power)) then
        p = power
        m = cmplx((sum(dcmplx(w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = cmplx(sum(dcmplx(w))/size(w))
    end if
end function mean_2d_complex
!
!> Compute the mean of a 3D array
!
function mean_3d_complex(w, power) result(m)
    complex, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: power
    complex :: m
    integer :: p
    if (present(power)) then
        p = power
        m = cmplx((sum(dcmplx(w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = cmplx(sum(dcmplx(w))/size(w))
    end if
end function mean_3d_complex
!
!> Compute the mean of a 4D array
!
function mean_4d_complex(w, power) result(m)
    complex, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in), optional :: power
    complex :: m
    integer :: p
    if (present(power)) then
        p = power
        m = cmplx((sum(dcmplx(w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = cmplx(sum(dcmplx(w))/size(w))
    end if
end function mean_4d_complex
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
!> Compute the mean of a 1D array
!
function mean_1d_dcomplex(w, power) result(m)
    double complex, dimension(:), intent(in) :: w
    integer, intent(in), optional :: power
    double complex :: m
    integer :: p
    if (present(power)) then
        p = power
        m = ((sum((w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = (sum((w))/size(w))
    end if
end function mean_1d_dcomplex
!
!> Compute the mean of a 2D array
!
function mean_2d_dcomplex(w, power) result(m)
    double complex, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: power
    double complex :: m
    integer :: p
    if (present(power)) then
        p = power
        m = ((sum((w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = (sum((w))/size(w))
    end if
end function mean_2d_dcomplex
!
!> Compute the mean of a 3D array
!
function mean_3d_dcomplex(w, power) result(m)
    double complex, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: power
    double complex :: m
    integer :: p
    if (present(power)) then
        p = power
        m = ((sum((w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = (sum((w))/size(w))
    end if
end function mean_3d_dcomplex
!
!> Compute the mean of a 4D array
!
function mean_4d_dcomplex(w, power) result(m)
    double complex, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in), optional :: power
    double complex :: m
    integer :: p
    if (present(power)) then
        p = power
        m = ((sum((w)**p, mask=(abs(w) /= 0))/size(w))**(1.0d0/p))
    else
        m = (sum((w))/size(w))
    end if
end function mean_4d_dcomplex
end module libflit_statistics
