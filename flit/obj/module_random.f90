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
module libflit_random
    use libflit_array
    use libflit_utility
    use libflit_error
    use libflit_array_operation
    use libflit_sort
    implicit none
    ! C++ random number generators using the PCG random engine
    interface
        ! random number following uniform distribution
        subroutine uniform_rand_int(w, nw, lb, ub) bind(c, name='uniform_rand')
            use iso_c_binding, only: c_int, c_float
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: lb, ub
        end subroutine uniform_rand_int
        ! random number following uniform distribution, given seed
        subroutine uniform_rand_seed_int(w, nw, lb, ub, seed) bind(c, name='uniform_rand_seed')
            use iso_c_binding, only: c_int, c_float
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_int), value :: lb, ub
            integer(kind=c_float), value :: seed
        end subroutine uniform_rand_seed_int
        ! random number following normal distribution
        subroutine normal_rand_int(w, nw, mu, sigma) bind(c, name='normal_rand')
            use iso_c_binding, only: c_int, c_float
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: mu, sigma
        end subroutine normal_rand_int
        ! random number following normal distribution, given seed
        subroutine normal_rand_seed_int(w, nw, mu, sigma, seed) bind(c, name='normal_rand_seed')
            use iso_c_binding, only: c_int, c_float
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: mu, sigma
            integer(kind=c_int), value :: seed
        end subroutine normal_rand_seed_int
        ! random number following Cauchy distribution
        subroutine cauchy_rand_int(w, nw, a, b) bind(c, name='cauchy_rand')
            use iso_c_binding, only: c_int, c_float
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: a, b
        end subroutine cauchy_rand_int
        ! random number following Cauchy distribution, given seed
        subroutine cauchy_rand_seed_int(w, nw, a, b, seed) bind(c, name='cauchy_rand_seed')
            use iso_c_binding, only: c_int, c_float
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: a, b
            integer(kind=c_int), value :: seed
        end subroutine cauchy_rand_seed_int
        ! random number following Poisson distribution
        subroutine poisson_rand_int(w, nw, mean) bind(c, name='poisson_rand')
            use iso_c_binding, only: c_int, c_float
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: mean
        end subroutine poisson_rand_int
        ! random number following Poisson distribution, given seed
        subroutine poisson_rand_seed_int(w, nw, mean, seed) bind(c, name='poisson_rand_seed')
            use iso_c_binding, only: c_int, c_float
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: mean
            integer(kind=c_int), value :: seed
        end subroutine poisson_rand_seed_int
        ! random number following exponential distribution
        subroutine exponential_rand_int(w, nw, lambda) bind(c, name='exponential_rand')
            use iso_c_binding, only: c_int, c_float
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: lambda
        end subroutine exponential_rand_int
        ! random number following exponential distribution, given seed
        subroutine exponential_rand_seed_int(w, nw, lambda, seed) bind(c, name='exponential_rand_seed')
            use iso_c_binding, only: c_int, c_float
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: lambda
            integer(kind=c_int), value :: seed
        end subroutine exponential_rand_seed_int
        ! random number following uniform distribution
        subroutine uniform_rand_float(w, nw, lb, ub) bind(c, name='uniform_rand')
            use iso_c_binding, only: c_float, c_int
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: lb, ub
        end subroutine uniform_rand_float
        ! random number following uniform distribution, given seed
        subroutine uniform_rand_seed_float(w, nw, lb, ub, seed) bind(c, name='uniform_rand_seed')
            use iso_c_binding, only: c_float, c_int
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: lb, ub
            integer(kind=c_int), value :: seed
        end subroutine uniform_rand_seed_float
        ! random number following normal distribution
        subroutine normal_rand_float(w, nw, mu, sigma) bind(c, name='normal_rand')
            use iso_c_binding, only: c_float, c_int
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: mu, sigma
        end subroutine normal_rand_float
        ! random number following normal distribution, given seed
        subroutine normal_rand_seed_float(w, nw, mu, sigma, seed) bind(c, name='normal_rand_seed')
            use iso_c_binding, only: c_float, c_int
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: mu, sigma
            integer(kind=c_int), value :: seed
        end subroutine normal_rand_seed_float
        ! random number following Cauchy distribution
        subroutine cauchy_rand_float(w, nw, a, b) bind(c, name='cauchy_rand')
            use iso_c_binding, only: c_float, c_int
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: a, b
        end subroutine cauchy_rand_float
        ! random number following Cauchy distribution, given seed
        subroutine cauchy_rand_seed_float(w, nw, a, b, seed) bind(c, name='cauchy_rand_seed')
            use iso_c_binding, only: c_float, c_int
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: a, b
            integer(kind=c_int), value :: seed
        end subroutine cauchy_rand_seed_float
        ! random number following Poisson distribution
        subroutine poisson_rand_float(w, nw, mean) bind(c, name='poisson_rand')
            use iso_c_binding, only: c_float, c_int
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: mean
        end subroutine poisson_rand_float
        ! random number following Poisson distribution, given seed
        subroutine poisson_rand_seed_float(w, nw, mean, seed) bind(c, name='poisson_rand_seed')
            use iso_c_binding, only: c_float, c_int
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: mean
            integer(kind=c_int), value :: seed
        end subroutine poisson_rand_seed_float
        ! random number following exponential distribution
        subroutine exponential_rand_float(w, nw, lambda) bind(c, name='exponential_rand')
            use iso_c_binding, only: c_float, c_int
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: lambda
        end subroutine exponential_rand_float
        ! random number following exponential distribution, given seed
        subroutine exponential_rand_seed_float(w, nw, lambda, seed) bind(c, name='exponential_rand_seed')
            use iso_c_binding, only: c_float, c_int
            real(kind=c_float), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_float), value :: lambda
            integer(kind=c_int), value :: seed
        end subroutine exponential_rand_seed_float
        ! random number following uniform distribution
        subroutine uniform_rand_double(w, nw, lb, ub) bind(c, name='uniform_rand_double')
            use iso_c_binding, only: c_double, c_int
            real(kind=c_double), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_double), value :: lb, ub
        end subroutine uniform_rand_double
        ! random number following uniform distribution, given seed
        subroutine uniform_rand_seed_double(w, nw, lb, ub, seed) bind(c, name='uniform_rand_seed_double')
            use iso_c_binding, only: c_double, c_int
            real(kind=c_double), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_double), value :: lb, ub
            integer(kind=c_int), value :: seed
        end subroutine uniform_rand_seed_double
        ! random number following normal distribution
        subroutine normal_rand_double(w, nw, mu, sigma) bind(c, name='normal_rand_double')
            use iso_c_binding, only: c_double, c_int
            real(kind=c_double), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_double), value :: mu, sigma
        end subroutine normal_rand_double
        ! random number following normal distribution, given seed
        subroutine normal_rand_seed_double(w, nw, mu, sigma, seed) bind(c, name='normal_rand_seed_double')
            use iso_c_binding, only: c_double, c_int
            real(kind=c_double), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_double), value :: mu, sigma
            integer(kind=c_int), value :: seed
        end subroutine normal_rand_seed_double
        ! random number following Cauchy distribution
        subroutine cauchy_rand_double(w, nw, a, b) bind(c, name='cauchy_rand_double')
            use iso_c_binding, only: c_double, c_int
            real(kind=c_double), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_double), value :: a, b
        end subroutine cauchy_rand_double
        ! random number following Cauchy distribution, given seed
        subroutine cauchy_rand_seed_double(w, nw, a, b, seed) bind(c, name='cauchy_rand_seed_double')
            use iso_c_binding, only: c_double, c_int
            real(kind=c_double), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_double), value :: a, b
            integer(kind=c_int), value :: seed
        end subroutine cauchy_rand_seed_double
        ! random number following Poisson distribution
        subroutine poisson_rand_double(w, nw, mean) bind(c, name='poisson_rand_double')
            use iso_c_binding, only: c_double, c_int
            real(kind=c_double), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_double), value :: mean
        end subroutine poisson_rand_double
        ! random number following Poisson distribution, given seed
        subroutine poisson_rand_seed_double(w, nw, mean, seed) bind(c, name='poisson_rand_seed_double')
            use iso_c_binding, only: c_double, c_int
            real(kind=c_double), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_double), value :: mean
            integer(kind=c_int), value :: seed
        end subroutine poisson_rand_seed_double
        ! random number following exponential distribution
        subroutine exponential_rand_double(w, nw, lambda) bind(c, name='exponential_rand_double')
            use iso_c_binding, only: c_double, c_int
            real(kind=c_double), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_double), value :: lambda
        end subroutine exponential_rand_double
        ! random number following exponential distribution, given seed
        subroutine exponential_rand_seed_double(w, nw, lambda, seed) bind(c, name='exponential_rand_seed_double')
            use iso_c_binding, only: c_double, c_int
            real(kind=c_double), dimension(*) :: w
            integer(kind=c_int), value :: nw
            real(kind=c_double), value :: lambda
            integer(kind=c_int), value :: seed
        end subroutine exponential_rand_seed_double
    end interface
    interface irand
        module procedure :: rand_int
    end interface irand
    interface rand
        module procedure :: rand_float
    end interface
    interface drand
        module procedure :: rand_double
    end interface
    interface irandom
        module procedure :: rand_int
        module procedure :: rand_array_1d_int
        module procedure :: rand_array_2d_int
        module procedure :: rand_array_3d_int
    end interface irandom
    interface random
        module procedure :: rand_float
        module procedure :: rand_array_1d_float
        module procedure :: rand_array_2d_float
        module procedure :: rand_array_3d_float
    end interface random
    interface drandom
        module procedure :: rand_double
        module procedure :: rand_array_1d_double
        module procedure :: rand_array_2d_double
        module procedure :: rand_array_3d_double
    end interface drandom
    interface random_permute
        module procedure :: random_permute_int
        module procedure :: random_permute_float
        module procedure :: random_permute_double
        module procedure :: random_permute_complex
        module procedure :: random_permute_dcomplex
        module procedure :: random_permute_logical
    end interface random_permute
    interface random_mask
        module procedure :: random_mask_1d_float
        module procedure :: random_mask_2d_float
        module procedure :: random_mask_3d_float
    end interface random_mask
    private
    public :: irand, rand, drand
    public :: irandom, random, drandom
    public :: random_order
    public :: random_string
    public :: random_permute
    public :: random_mask
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
!> Return a random number
!
function rand_int(dist, seed, range, mu, sigma, a, b, mean, lambda) result(r)
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    integer, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    real, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    real, optional :: a, b
    ! Possion distribution -- mean
    real, optional :: mean
    ! Exponential distribution -- lambda
    real, optional :: lambda
    ! Output
    integer :: r
    real, dimension(1:1) :: random_value
    real, dimension(1:2) :: value_range
    real :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    real :: exponential_lambda
    character(len=32) :: distribution
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0, 1.0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    random_value = rand_array_1d_int(1, distribution, rs, nint(value_range), normal_mu, normal_sigma, &
        cauchy_a, cauchy_b, poisson_mean, exponential_lambda)
    r = nint(random_value(1))
end function rand_int
!
!> Return a 1D array of random numbers
!
function rand_array_1d_int(n1, dist, seed, range, mu, sigma, a, b, mean, lambda, spacing) result(r)
    integer :: n1
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    integer, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    real, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    real, optional :: a, b
    ! Possion distribution -- mean
    real, optional :: mean
    ! Exponential distribution -- lambda
    real, optional :: lambda
    ! Minimum distance
    integer, optional :: spacing
    ! Output
    integer, allocatable, dimension(:) :: r
    real, allocatable, dimension(:) :: random_value
    real, dimension(1:2) :: value_range
    real :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    real :: exponential_lambda
    character(len=32) :: distribution
    integer :: n
    real :: random_spacing
    real :: empty_space
    integer, allocatable, dimension(:) :: index
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0, 1.0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0
    end if
    if (present(spacing)) then
        random_spacing = spacing
    else
        random_spacing = 0.0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    n = n1
    if (random_spacing > 0) then
        empty_space = value_range(2) - value_range(1) - (n - 1.0)*random_spacing
        call assert(empty_space >= 0.0, ' <rand_array_1d> Error: The expected minimum spacing is too big. ')
    end if
    allocate (random_value(1:n))
    if (rs >= 0) then
        select case (distribution)
            case ('uniform')
                if (random_spacing > 0) then
                    call uniform_rand_seed_int(random_value, n, 0.0, 1.0, rs)
                    call sort_index(random_value, index)
                    random_value = empty_space*random_value + value_range(1) + random_spacing*linspace(0.0, n - 1.0, n)
                    random_value = random_value(index)
                else
                    call uniform_rand_seed_int(random_value, n, value_range(1), value_range(2), rs)
                end if
            case ('normal', 'gaussian', 'Gaussian')
                call normal_rand_seed_int(random_value, n, normal_mu, normal_sigma, rs)
            case ('cauchy', 'Cauchy')
                call cauchy_rand_seed_int(random_value, n, cauchy_a, cauchy_b, rs)
            case ('poisson', 'Poisson')
                call poisson_rand_seed_int(random_value, n, poisson_mean, rs)
            case ('exp', 'exponential')
                call exponential_rand_seed_int(random_value, n, exponential_lambda, rs)
        end select
    else
        select case (distribution)
            case ('uniform')
                if (random_spacing > 0) then
                    call uniform_rand_int(random_value, n, 0.0, 1.0)
                    call sort_index(random_value, index)
                    random_value = empty_space*random_value + value_range(1) + random_spacing*linspace(0.0, n - 1.0, n)
                    random_value = random_value(index)
                else
                    call uniform_rand_int(random_value, n, value_range(1), value_range(2))
                end if
            case ('normal', 'gaussian', 'Gaussian')
                call normal_rand_int(random_value, n, normal_mu, normal_sigma)
            case ('cauchy', 'Cauchy')
                call cauchy_rand_int(random_value, n, cauchy_a, cauchy_b)
            case ('poisson', 'Poisson')
                call poisson_rand_int(random_value, n, poisson_mean)
            case ('exp', 'exponential')
                call exponential_rand_int(random_value, n, exponential_lambda)
        end select
    end if
    r = nint(random_value)
end function rand_array_1d_int
!
!> Return a 2D array of random numbers
!
function rand_array_2d_int(n1, n2, dist, seed, range, mu, sigma, a, b, mean, lambda, spacing) result(r)
    integer :: n1, n2
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    integer, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    real, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    real, optional :: a, b
    ! Possion distribution -- mean
    real, optional :: mean
    ! Exponential distribution -- lambda
    real, optional :: lambda
    ! Minimum distance
    integer, optional :: spacing
    ! Output
    integer, allocatable, dimension(:, :) :: r
    real, allocatable, dimension(:) :: random_value
    real, dimension(1:2) :: value_range
    real :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    real :: exponential_lambda
    character(len=32) :: distribution
    real :: random_spacing
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0, 1.0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0
    end if
    if (present(spacing)) then
        random_spacing = spacing
    else
        random_spacing = 0.0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    random_value = rand_array_1d_int(n1*n2, distribution, rs, nint(value_range), normal_mu, normal_sigma, &
        cauchy_a, cauchy_b, poisson_mean, exponential_lambda, nint(random_spacing))
    r = nint(reshape(random_value, [n1, n2]))
end function rand_array_2d_int
!
!> Return a 3D array of random numbers
!
function rand_array_3d_int(n1, n2, n3, dist, seed, range, mu, sigma, a, b, mean, lambda, spacing) result(r)
    integer :: n1, n2, n3
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    integer, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    real, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    real, optional :: a, b
    ! Possion distribution -- mean
    real, optional :: mean
    ! Exponential distribution -- lambda
    real, optional :: lambda
    ! Minimum distance
    integer, optional :: spacing
    ! Output
    integer, allocatable, dimension(:, :, :) :: r
    real, allocatable, dimension(:) :: random_value
    real, dimension(1:2) :: value_range
    real :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    real :: exponential_lambda
    character(len=32) :: distribution
    real :: random_spacing
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0, 1.0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0
    end if
    if (present(spacing)) then
        random_spacing = spacing
    else
        random_spacing = 0.0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    random_value = rand_array_1d_int(n1*n2*n3, distribution, rs, nint(value_range), normal_mu, normal_sigma, &
        cauchy_a, cauchy_b, poisson_mean, exponential_lambda, nint(random_spacing))
    r = nint(reshape(random_value, [n1, n2, n3]))
end function rand_array_3d_int
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
!> Return a random number
!
function rand_float(dist, seed, range, mu, sigma, a, b, mean, lambda) result(r)
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    real, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    real, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    real, optional :: a, b
    ! Possion distribution -- mean
    real, optional :: mean
    ! Exponential distribution -- lambda
    real, optional :: lambda
    ! Output
    real :: r
    real, dimension(1:1) :: random_value
    real, dimension(1:2) :: value_range
    real :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    real :: exponential_lambda
    character(len=32) :: distribution
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0, 1.0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    random_value = rand_array_1d_float(1, distribution, rs, (value_range), normal_mu, normal_sigma, &
        cauchy_a, cauchy_b, poisson_mean, exponential_lambda)
    r = (random_value(1))
end function rand_float
!
!> Return a 1D array of random numbers
!
function rand_array_1d_float(n1, dist, seed, range, mu, sigma, a, b, mean, lambda, spacing) result(r)
    integer :: n1
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    real, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    real, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    real, optional :: a, b
    ! Possion distribution -- mean
    real, optional :: mean
    ! Exponential distribution -- lambda
    real, optional :: lambda
    ! Minimum distance
    real, optional :: spacing
    ! Output
    real, allocatable, dimension(:) :: r
    real, allocatable, dimension(:) :: random_value
    real, dimension(1:2) :: value_range
    real :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    real :: exponential_lambda
    character(len=32) :: distribution
    integer :: n
    real :: random_spacing
    real :: empty_space
    integer, allocatable, dimension(:) :: index
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0, 1.0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0
    end if
    if (present(spacing)) then
        random_spacing = spacing
    else
        random_spacing = 0.0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    n = n1
    if (random_spacing > 0) then
        empty_space = value_range(2) - value_range(1) - (n - 1.0)*random_spacing
        call assert(empty_space >= 0.0, ' <rand_array_1d> Error: The expected minimum spacing is too big. ')
    end if
    allocate (random_value(1:n))
    if (rs >= 0) then
        select case (distribution)
            case ('uniform')
                if (random_spacing > 0) then
                    call uniform_rand_seed_float(random_value, n, 0.0, 1.0, rs)
                    call sort_index(random_value, index)
                    random_value = empty_space*random_value + value_range(1) + random_spacing*linspace(0.0, n - 1.0, n)
                    random_value = random_value(index)
                else
                    call uniform_rand_seed_float(random_value, n, value_range(1), value_range(2), rs)
                end if
            case ('normal', 'gaussian', 'Gaussian')
                call normal_rand_seed_float(random_value, n, normal_mu, normal_sigma, rs)
            case ('cauchy', 'Cauchy')
                call cauchy_rand_seed_float(random_value, n, cauchy_a, cauchy_b, rs)
            case ('poisson', 'Poisson')
                call poisson_rand_seed_float(random_value, n, poisson_mean, rs)
            case ('exp', 'exponential')
                call exponential_rand_seed_float(random_value, n, exponential_lambda, rs)
        end select
    else
        select case (distribution)
            case ('uniform')
                if (random_spacing > 0) then
                    call uniform_rand_float(random_value, n, 0.0, 1.0)
                    call sort_index(random_value, index)
                    random_value = empty_space*random_value + value_range(1) + random_spacing*linspace(0.0, n - 1.0, n)
                    random_value = random_value(index)
                else
                    call uniform_rand_float(random_value, n, value_range(1), value_range(2))
                end if
            case ('normal', 'gaussian', 'Gaussian')
                call normal_rand_float(random_value, n, normal_mu, normal_sigma)
            case ('cauchy', 'Cauchy')
                call cauchy_rand_float(random_value, n, cauchy_a, cauchy_b)
            case ('poisson', 'Poisson')
                call poisson_rand_float(random_value, n, poisson_mean)
            case ('exp', 'exponential')
                call exponential_rand_float(random_value, n, exponential_lambda)
        end select
    end if
    r = (random_value)
end function rand_array_1d_float
!
!> Return a 2D array of random numbers
!
function rand_array_2d_float(n1, n2, dist, seed, range, mu, sigma, a, b, mean, lambda, spacing) result(r)
    integer :: n1, n2
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    real, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    real, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    real, optional :: a, b
    ! Possion distribution -- mean
    real, optional :: mean
    ! Exponential distribution -- lambda
    real, optional :: lambda
    ! Minimum distance
    real, optional :: spacing
    ! Output
    real, allocatable, dimension(:, :) :: r
    real, allocatable, dimension(:) :: random_value
    real, dimension(1:2) :: value_range
    real :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    real :: exponential_lambda
    character(len=32) :: distribution
    real :: random_spacing
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0, 1.0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0
    end if
    if (present(spacing)) then
        random_spacing = spacing
    else
        random_spacing = 0.0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    random_value = rand_array_1d_float(n1*n2, distribution, rs, (value_range), normal_mu, normal_sigma, &
        cauchy_a, cauchy_b, poisson_mean, exponential_lambda, (random_spacing))
    r = (reshape(random_value, [n1, n2]))
end function rand_array_2d_float
!
!> Return a 3D array of random numbers
!
function rand_array_3d_float(n1, n2, n3, dist, seed, range, mu, sigma, a, b, mean, lambda, spacing) result(r)
    integer :: n1, n2, n3
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    real, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    real, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    real, optional :: a, b
    ! Possion distribution -- mean
    real, optional :: mean
    ! Exponential distribution -- lambda
    real, optional :: lambda
    ! Minimum distance
    real, optional :: spacing
    ! Output
    real, allocatable, dimension(:, :, :) :: r
    real, allocatable, dimension(:) :: random_value
    real, dimension(1:2) :: value_range
    real :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    real :: exponential_lambda
    character(len=32) :: distribution
    real :: random_spacing
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0, 1.0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0
    end if
    if (present(spacing)) then
        random_spacing = spacing
    else
        random_spacing = 0.0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    random_value = rand_array_1d_float(n1*n2*n3, distribution, rs, (value_range), normal_mu, normal_sigma, &
        cauchy_a, cauchy_b, poisson_mean, exponential_lambda, (random_spacing))
    r = (reshape(random_value, [n1, n2, n3]))
end function rand_array_3d_float
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
!> Return a random number
!
function rand_double(dist, seed, range, mu, sigma, a, b, mean, lambda) result(r)
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    double precision, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    double precision, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    double precision, optional :: a, b
    ! Possion distribution -- mean
    double precision, optional :: mean
    ! Exponential distribution -- lambda
    double precision, optional :: lambda
    ! Output
    double precision :: r
    double precision, dimension(1:1) :: random_value
    double precision, dimension(1:2) :: value_range
    double precision :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    double precision :: exponential_lambda
    character(len=32) :: distribution
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0d0, 1.0d0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0d0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0d0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0d0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0d0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0d0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0d0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    random_value = rand_array_1d_double(1, distribution, rs, dble(value_range), normal_mu, normal_sigma, &
        cauchy_a, cauchy_b, poisson_mean, exponential_lambda)
    r = dble(random_value(1))
end function rand_double
!
!> Return a 1D array of random numbers
!
function rand_array_1d_double(n1, dist, seed, range, mu, sigma, a, b, mean, lambda, spacing) result(r)
    integer :: n1
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    double precision, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    double precision, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    double precision, optional :: a, b
    ! Possion distribution -- mean
    double precision, optional :: mean
    ! Exponential distribution -- lambda
    double precision, optional :: lambda
    ! Minimum distance
    double precision, optional :: spacing
    ! Output
    double precision, allocatable, dimension(:) :: r
    double precision, allocatable, dimension(:) :: random_value
    double precision, dimension(1:2) :: value_range
    double precision :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    double precision :: exponential_lambda
    character(len=32) :: distribution
    integer :: n
    double precision :: random_spacing
    double precision :: empty_space
    integer, allocatable, dimension(:) :: index
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0, 1.0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0
    end if
    if (present(spacing)) then
        random_spacing = spacing
    else
        random_spacing = 0.0d0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    n = n1
    if (random_spacing > 0) then
        empty_space = value_range(2) - value_range(1) - (n - 1.0d0)*random_spacing
        call assert(empty_space >= 0.0d0, ' <rand_array_1d> Error: The expected minimum spacing is too big. ')
    end if
    allocate (random_value(1:n))
    if (rs >= 0) then
        select case (distribution)
            case ('uniform')
                if (random_spacing > 0) then
                    call uniform_rand_seed_double(random_value, n, 0.0d0, 1.0d0, rs)
                    call sort_index(random_value, index)
                    random_value = empty_space*random_value + value_range(1) + random_spacing*linspace(0.0d0, n - 1.0d0, n)
                    random_value = random_value(index)
                else
                    call uniform_rand_seed_double(random_value, n, value_range(1), value_range(2), rs)
                end if
            case ('normal', 'gaussian', 'Gaussian')
                call normal_rand_seed_double(random_value, n, normal_mu, normal_sigma, rs)
            case ('cauchy', 'Cauchy')
                call cauchy_rand_seed_double(random_value, n, cauchy_a, cauchy_b, rs)
            case ('poisson', 'Poisson')
                call poisson_rand_seed_double(random_value, n, poisson_mean, rs)
            case ('exp', 'exponential')
                call exponential_rand_seed_double(random_value, n, exponential_lambda, rs)
        end select
    else
        select case (distribution)
            case ('uniform')
                if (random_spacing > 0) then
                    call uniform_rand_double(random_value, n, 0.0d0, 1.0d0)
                    call sort_index(random_value, index)
                    random_value = empty_space*random_value + value_range(1) + random_spacing*linspace(0.0d0, n - 1.0d0, n)
                    random_value = random_value(index)
                else
                    call uniform_rand_double(random_value, n, value_range(1), value_range(2))
                end if
            case ('normal', 'gaussian', 'Gaussian')
                call normal_rand_double(random_value, n, normal_mu, normal_sigma)
            case ('cauchy', 'Cauchy')
                call cauchy_rand_double(random_value, n, cauchy_a, cauchy_b)
            case ('poisson', 'Poisson')
                call poisson_rand_double(random_value, n, poisson_mean)
            case ('exp', 'exponential')
                call exponential_rand_double(random_value, n, exponential_lambda)
        end select
    end if
    r = dble(random_value)
end function rand_array_1d_double
!
!> Return a 2D array of random numbers
!
function rand_array_2d_double(n1, n2, dist, seed, range, mu, sigma, a, b, mean, lambda, spacing) result(r)
    integer :: n1, n2
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    double precision, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    double precision, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    double precision, optional :: a, b
    ! Possion distribution -- mean
    double precision, optional :: mean
    ! Exponential distribution -- lambda
    double precision, optional :: lambda
    ! Minimum distance
    double precision, optional :: spacing
    ! Output
    double precision, allocatable, dimension(:, :) :: r
    double precision, allocatable, dimension(:) :: random_value
    double precision, dimension(1:2) :: value_range
    double precision :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    double precision :: exponential_lambda
    character(len=32) :: distribution
    double precision :: random_spacing
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0, 1.0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0
    end if
    if (present(spacing)) then
        random_spacing = spacing
    else
        random_spacing = 0.0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    random_value = rand_array_1d_double(n1*n2, distribution, rs, dble(value_range), normal_mu, normal_sigma, &
        cauchy_a, cauchy_b, poisson_mean, exponential_lambda, dble(random_spacing))
    r = dble(reshape(random_value, [n1, n2]))
end function rand_array_2d_double
!
!> Return a 3D array of random numbers
!
function rand_array_3d_double(n1, n2, n3, dist, seed, range, mu, sigma, a, b, mean, lambda, spacing) result(r)
    integer :: n1, n2, n3
    ! Given seed or not
    integer, optional :: seed
    ! Distribution
    character(len=*), optional :: dist
    ! Uniform distribution -- value range
    double precision, dimension(:), optional :: range
    ! Gaussian distribution -- mean and standard deviation
    double precision, optional :: mu, sigma
    ! Cauchy distribution -- parameters a and b
    double precision, optional :: a, b
    ! Possion distribution -- mean
    double precision, optional :: mean
    ! Exponential distribution -- lambda
    double precision, optional :: lambda
    ! Minimum distance
    double precision, optional :: spacing
    ! Output
    double precision, allocatable, dimension(:, :, :) :: r
    double precision, allocatable, dimension(:) :: random_value
    double precision, dimension(1:2) :: value_range
    double precision :: normal_mu, normal_sigma, cauchy_a, cauchy_b, poisson_mean
    double precision :: exponential_lambda
    character(len=32) :: distribution
    double precision :: random_spacing
    integer :: rs
    if (present(dist)) then
        distribution = trim(adjustl(dist))
    else
        distribution = 'uniform'
    end if
    if (present(range)) then
        value_range = range
    else
        value_range = [0.0, 1.0]
    end if
    if (present(mu)) then
        normal_mu = mu
    else
        normal_mu = 0.0
    end if
    if (present(sigma)) then
        normal_sigma = sigma
    else
        normal_sigma = 1.0
    end if
    if (present(a)) then
        cauchy_a = a
    else
        cauchy_a = 0.0
    end if
    if (present(b)) then
        cauchy_b = b
    else
        cauchy_b = 1.0
    end if
    if (present(mean)) then
        poisson_mean = mean
    else
        poisson_mean = 0.0
    end if
    if (present(lambda)) then
        exponential_lambda = lambda
    else
        exponential_lambda = 1.0
    end if
    if (present(spacing)) then
        random_spacing = spacing
    else
        random_spacing = 0.0
    end if
    if (present(seed)) then
        rs = seed
    else
        rs = -1
    end if
    random_value = rand_array_1d_double(n1*n2*n3, distribution, rs, dble(value_range), normal_mu, normal_sigma, &
        cauchy_a, cauchy_b, poisson_mean, exponential_lambda, dble(random_spacing))
    r = dble(reshape(random_value, [n1, n2, n3]))
end function rand_array_3d_double
    !
    !> Generate a random ordering of the integers 1 ... n.
    !
    function random_order(n, seed) result(order)
        integer :: n
        integer, optional :: seed
        integer, allocatable, dimension(:) :: order
        integer :: i, j, k
        real, allocatable, dimension(:) :: wkr
        real :: wk
        order = regspace(1, 1, n)
        if (present(seed)) then
            wkr = random(n, seed=seed)
        else
            wkr = random(n)
        end if
        ! Starting at the end, swap the current last indicator with one
        ! randomly chosen from those preceeding it.
        if (present(seed)) then
            do i = n, 2, -1
                wk = wkr(i) !rand(range=[0.0, 1.0], seed=seed*i)
                j = 1 + i*wk
                if (j < i) then
                    k = order(i)
                    order(i) = order(j)
                    order(j) = k
                end if
            end do
        else
            do i = n, 2, -1
                wk = wkr(i) !rand(range=[0.0, 1.0])
                j = 1 + i*wk
                if (j < i) then
                    k = order(i)
                    order(i) = order(j)
                    order(j) = k
                end if
            end do
        end if
    end function random_order
    !
    !> Generate random string of A-Z, a-z, 0-9
    !
    function random_string(nc, seed) result(str)
        integer, optional :: nc, seed
        integer :: nchar, i
        character(len=1), dimension(1:72) :: pool
        character(len=:), allocatable :: str
        integer, allocatable, dimension(:) :: ri
        if (present(nc)) then
            nchar = max(1, nc)
        else
            nchar = 10
        end if
        pool = [ &
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', &
            'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', &
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', &
            'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', &
            '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', &
            '0', '9', '8', '7', '6', '5', '4', '3', '2', '1' &
            ]
        allocate (character(len=nchar)::str)
        if (present(seed)) then
            ri = irandom(nchar, range=[1, 72], seed=seed)
        else
            ri = irandom(nchar, range=[1, 72])
        end if
        do i = 1, nchar
            str(i:i) = pool(ri(i))
        end do
    end function random_string
    !
    !> Generate random mask
    !
    function random_mask_1d_float(m, n, seed) result(mm)
        real, dimension(:), intent(in) :: m
        integer, intent(in) :: n
        integer, intent(in), optional :: seed
        real, allocatable, dimension(:) :: mm
        integer, allocatable, dimension(:) :: i
        integer :: l
        mm = m
        call assert(n <= size(m), ' <random_mask_1d_float> Error: n must <= size(m)')
        if (present(seed)) then
            i = random_order(size(m), seed=seed)
        else
            i = random_order(size(m))
        end if
        do l = 1, n
            mm(i(l)) = 0.0
        end do
    end function random_mask_1d_float
    function random_mask_2d_float(m, n, seed) result(mm)
        real, dimension(:, :), intent(in) :: m
        integer, intent(in) :: n
        integer, intent(in), optional :: seed
        real, allocatable, dimension(:, :) :: mm
        integer, allocatable, dimension(:) :: ij
        integer, allocatable, dimension(:, :) :: index
        integer :: i, j, l
        mm = m
        call assert(n <= size(m), ' <random_mask_2d_float> Error: n must <= size(m)')
        if (present(seed)) then
            ij = random_order(size(m), seed=seed)
        else
            ij = random_order(size(m))
        end if
        index = zeros(size(m), 3)
        l = 1
        do i = 1, size(m, 1)
            do j = 1, size(m, 2)
                index(l, :) = [l, i, j]
                l = l + 1
            end do
        end do
        do l = 1, n
            mm(index(ij(l), 2), index(ij(l), 3)) = 0.0
        end do
    end function random_mask_2d_float
    function random_mask_3d_float(m, n, seed) result(mm)
        real, dimension(:, :, :), intent(in) :: m
        integer, intent(in) :: n
        integer, intent(in), optional :: seed
        real, allocatable, dimension(:, :, :) :: mm
        integer, allocatable, dimension(:) :: ijk
        integer, allocatable, dimension(:, :) :: index
        integer :: i, j, k, l
        mm = m
        call assert(n <= size(m), ' <random_mask_3d_float> Error: n must <= size(m)')
        if (present(seed)) then
            ijk = random_order(size(m), seed=seed)
        else
            ijk = random_order(size(m))
        end if
        index = zeros(size(m), 4)
        l = 1
        do i = 1, size(m, 1)
            do j = 1, size(m, 2)
                do k = 1, size(m, 3)
                    index(l, :) = [l, i, j, k]
                    l = l + 1
                end do
            end do
        end do
        do l = 1, n
            mm(index(ijk(l), 2), index(ijk(l), 3), index(ijk(l), 4)) = 0.0
        end do
    end function random_mask_3d_float
    !
    !> Random permute an array
    !
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
function random_permute_int(w, seed) result(wr)
    integer, dimension(:) :: w
    integer, optional :: seed
    integer, allocatable, dimension(:) :: wr
    integer :: n, i, j
    n = size(w)
    wr = w
    if (n == 1) then
        return
    end if
    if (present(seed)) then
        do i = 1, n - 1
            j = i + irand(range=[0, n - i], seed=seed*i)
            call swap(wr(i), wr(j))
        end do
    else
        do i = 1, n - 1
            j = i + irand(range=[0, n - i])
            call swap(wr(i), wr(j))
        end do
    end if
end function random_permute_int
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
function random_permute_float(w, seed) result(wr)
    real, dimension(:) :: w
    integer, optional :: seed
    real, allocatable, dimension(:) :: wr
    integer :: n, i, j
    n = size(w)
    wr = w
    if (n == 1) then
        return
    end if
    if (present(seed)) then
        do i = 1, n - 1
            j = i + irand(range=[0, n - i], seed=seed*i)
            call swap(wr(i), wr(j))
        end do
    else
        do i = 1, n - 1
            j = i + irand(range=[0, n - i])
            call swap(wr(i), wr(j))
        end do
    end if
end function random_permute_float
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
function random_permute_double(w, seed) result(wr)
    double precision, dimension(:) :: w
    integer, optional :: seed
    double precision, allocatable, dimension(:) :: wr
    integer :: n, i, j
    n = size(w)
    wr = w
    if (n == 1) then
        return
    end if
    if (present(seed)) then
        do i = 1, n - 1
            j = i + irand(range=[0, n - i], seed=seed*i)
            call swap(wr(i), wr(j))
        end do
    else
        do i = 1, n - 1
            j = i + irand(range=[0, n - i])
            call swap(wr(i), wr(j))
        end do
    end if
end function random_permute_double
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
function random_permute_complex(w, seed) result(wr)
    complex, dimension(:) :: w
    integer, optional :: seed
    complex, allocatable, dimension(:) :: wr
    integer :: n, i, j
    n = size(w)
    wr = w
    if (n == 1) then
        return
    end if
    if (present(seed)) then
        do i = 1, n - 1
            j = i + irand(range=[0, n - i], seed=seed*i)
            call swap(wr(i), wr(j))
        end do
    else
        do i = 1, n - 1
            j = i + irand(range=[0, n - i])
            call swap(wr(i), wr(j))
        end do
    end if
end function random_permute_complex
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
function random_permute_dcomplex(w, seed) result(wr)
    double complex, dimension(:) :: w
    integer, optional :: seed
    double complex, allocatable, dimension(:) :: wr
    integer :: n, i, j
    n = size(w)
    wr = w
    if (n == 1) then
        return
    end if
    if (present(seed)) then
        do i = 1, n - 1
            j = i + irand(range=[0, n - i], seed=seed*i)
            call swap(wr(i), wr(j))
        end do
    else
        do i = 1, n - 1
            j = i + irand(range=[0, n - i])
            call swap(wr(i), wr(j))
        end do
    end if
end function random_permute_dcomplex
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
function random_permute_logical(w, seed) result(wr)
    logical, dimension(:) :: w
    integer, optional :: seed
    logical, allocatable, dimension(:) :: wr
    integer :: n, i, j
    n = size(w)
    wr = w
    if (n == 1) then
        return
    end if
    if (present(seed)) then
        do i = 1, n - 1
            j = i + irand(range=[0, n - i], seed=seed*i)
            call swap(wr(i), wr(j))
        end do
    else
        do i = 1, n - 1
            j = i + irand(range=[0, n - i])
            call swap(wr(i), wr(j))
        end do
    end if
end function random_permute_logical
end module libflit_random
