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
module libflit_tvfilt
    use libflit_array
    use libflit_string
    use libflit_date_time
    use libflit_mpicomm
    use libflit_utility
    use libflit_constants
    use libflit_array_operation
    implicit none
    private
    !
    !> Isotropic TV filtering, implementing the algorithm in
    !>
    !> Goldstein, Osher, 2009, The Split Bregman Method for L1-Regularized Problems,
    !> SIAM Journal of Imaging Sciences, doi: 10.1137/080725891
    !
    interface tv_filt
        module procedure :: tv_iso_filt_1d_float
        module procedure :: tv_iso_filt_2d_float
        module procedure :: tv_iso_filt_3d_float
        module procedure :: tv_iso_filt_1d_double
        module procedure :: tv_iso_filt_2d_double
        module procedure :: tv_iso_filt_3d_double
    end interface tv_filt
    !
    !> Total generalized p-variation filtering, implementing the algorithm in
    !>
    !> Knoll et al., 2011, Second order total generalized variation (TGV) for MRI,
    !> DOI 10.1002/mrm.22595
    !>
    !> Gao, Huang, 2019, Acoustic- and elastic-waveform inversion with
    !> total generalized p-variation regularization
    !> doi: 10.1093/gji/ggz203
    !
    interface tgpv_filt
        module procedure :: tgpv_filt_1d_float
        module procedure :: tgpv_filt_2d_float
        module procedure :: tgpv_filt_3d_float
        module procedure :: tgpv_filt_1d_double
        module procedure :: tgpv_filt_2d_double
        module procedure :: tgpv_filt_3d_double
    end interface tgpv_filt
    !
    !> MPI version of 2D and 3D TGpV filtering
    !
    interface tgpv_filt_mpi
        module procedure :: tgpv_filt_2d_mpi_float
        module procedure :: tgpv_filt_3d_mpi_float
        module procedure :: tgpv_filt_2d_mpi_double
        module procedure :: tgpv_filt_3d_mpi_double
    end interface tgpv_filt_mpi
    !
    !> Isotropic TV + sparsity filtering, implementing the algorithm in
    !>
    !> Gao et al., 2022, SREMI: Super-resolution electromagnetic imaging with
    !> single-channel ground-penetrating radar, Journal of Applied Geophysics,
    !> doi: 10.1016/j.jappgeo.2022.104777
    !
    interface sparse_tv_filt
        module procedure :: sparse_tv_filt_1d_float
        module procedure :: sparse_tv_filt_2d_float
        module procedure :: sparse_tv_filt_3d_float
        module procedure :: sparse_tv_filt_1d_double
        module procedure :: sparse_tv_filt_2d_double
        module procedure :: sparse_tv_filt_3d_double
    end interface
    !
    !> Soft shrinkage, implementing Goldstein, Osher (2009)
    !
    interface soft_shrinkage
        module procedure :: soft_shrinkage_1d_float
        module procedure :: soft_shrinkage_2d_float
        module procedure :: soft_shrinkage_3d_float
        module procedure :: soft_shrinkage_1d_double
        module procedure :: soft_shrinkage_2d_double
        module procedure :: soft_shrinkage_3d_double
    end interface soft_shrinkage
    public :: tv_filt
    public :: sparse_tv_filt
    public :: tgpv_filt
    public :: tgpv_filt_mpi
    public :: soft_shrinkage
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
!> 1D isotropic TV denoising
!
function tv_iso_filt_1d_float(f, mu, niter, verbose) result(u)
    real, dimension(:), intent(in) :: f
    real, intent(in) :: mu
    integer, intent(in) :: niter
    logical, optional :: verbose
    real, allocatable, dimension(:) :: u
    integer :: i, iter
    real :: tmpx, sumu
    real, allocatable, dimension(:) :: d1, d1t
    real, allocatable, dimension(:) :: pu
    integer :: n1
    integer :: i1, i2
    integer :: ii1, ii2
    real :: lambda, sk
    logical :: tv_verbose
    n1 = size(f)
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1)
    d1t = zeros(n1)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda = 2.0*mu
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        ! minimization u
        do i = 1, n1
            if (i == 1) then
                i1 = 0
                ii1 = i
            else
                i1 = 1
                ii1 = i - 1
            end if
            if (i == n1) then
                i2 = 0
                ii2 = i
            else
                i2 = 1
                ii2 = i + 1
            end if
            sumu = lambda*( &
                i2*u(ii2) + i1*u(ii1)) &
                + lambda*( &
                (i1*d1(ii1) - i2*d1(i)) &
                - (i1*d1t(ii1) - i2*d1t(i)) &
                ) &
                + mu*f(i)
            u(i) = sumu/(mu + (2.0 - (1 - i1) - (1 - i2))*lambda)
        end do
        ! update multiplietgpv_filt_2d_rs
        ! minimization d
        !$omp parallel do private(i, i1, i2, tmpx, sk)
        do i = 1, n1
            if (i == n1) then
                i1 = i - 1
                i2 = i
            else
                i1 = i
                i2 = i + 1
            end if
            tmpx = (u(i2) - u(i1)) + d1t(i)
            sk = abs(tmpx)
            d1(i) = max(sk - 1.0/lambda, 0.0)*tmpx/(sk + float_tiny)
        end do
        !$omp end parallel do
        ! d
        !$omp parallel do private(i, i1, i2)
        do i = 1, n1
            if (i == n1) then
                i1 = i - 1
                i2 = i
            else
                i1 = i
                i2 = i + 1
            end if
            d1t(i) = d1t(i) + (u(i2) - u(i1) - d1(i))
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Isotropic TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function tv_iso_filt_1d_float
!
!> 2D isotropic TV denoising
!
function tv_iso_filt_2d_float(f, mu, niter, verbose) result(u)
    real, dimension(:, :), intent(in) :: f
    real, intent(in) :: mu
    integer, intent(in) :: niter
    logical, optional :: verbose
    real, allocatable, dimension(:, :) :: u
    integer :: i, j, iter
    real :: tmpx, tmpz, sumu
    real, allocatable, dimension(:, :) :: d1, d2, d1t, d2t
    real, allocatable, dimension(:, :) :: pu
    integer :: n1, n2
    integer :: i1, i2, j1, j2
    integer :: ii1, ii2, jj1, jj2
    real :: lambda, sk
    logical :: tv_verbose
    n1 = size(f, 1)
    n2 = size(f, 2)
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1, n2)
    d2 = zeros(n1, n2)
    d1t = zeros(n1, n2)
    d2t = zeros(n1, n2)
    ! read noisy image
    u = f
    pu = zeros_like(u)
    ! lambdas for u and w subproblems
    lambda = 2.0*mu
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        ! minimization u
        do j = 1, n2
            do i = 1, n1
                if (i == 1) then
                    i1 = 0
                    ii1 = i
                else
                    i1 = 1
                    ii1 = i - 1
                end if
                if (i == n1) then
                    i2 = 0
                    ii2 = i
                else
                    i2 = 1
                    ii2 = i + 1
                end if
                if (j == 1) then
                    j1 = 0
                    jj1 = j
                else
                    j1 = 1
                    jj1 = j - 1
                end if
                if (j == n2) then
                    j2 = 0
                    jj2 = j
                else
                    j2 = 1
                    jj2 = j + 1
                end if
                sumu = lambda*( &
                    i2*u(ii2, j) + i1*u(ii1, j) &
                    + j2*u(i, jj2) + j1*u(i, jj1)) &
                    + lambda*( &
                    (i1*d1(ii1, j) - i2*d1(i, j)) &
                    + (j1*d2(i, jj1) - j2*d2(i, j)) &
                    - (i1*d1t(ii1, j) - i2*d1t(i, j)) &
                    - (j1*d2t(i, jj1) - j2*d2t(i, j)) &
                    ) &
                    + mu*f(i, j)
                u(i, j) = sumu/(mu + (4.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2))*lambda)
            end do
        end do
        ! update multiplietgpv_filt_2d_rs
        ! minimization d
        !$omp parallel do private(i, j, i1, i2, j1, j2, tmpx, tmpz, sk)
        do j = 1, n2
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                tmpx = (u(i2, j) - u(i1, j)) + d1t(i, j)
                if (j == n2) then
                    j1 = j - 1
                    j2 = j
                else
                    j1 = j
                    j2 = j + 1
                end if
                tmpz = (u(i, j2) - u(i, j1)) + d2t(i, j)
                sk = sqrt(tmpx**2 + tmpz**2)
                d1(i, j) = max(sk - 1.0/lambda, 0.0)*tmpx/(sk + float_tiny)
                d2(i, j) = max(sk - 1.0/lambda, 0.0)*tmpz/(sk + float_tiny)
            end do
        end do
        !$omp end parallel do
        ! d
        !$omp parallel do private(i, j, i1, i2, j1, j2)
        do j = 1, n2
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                d1t(i, j) = d1t(i, j) + (u(i2, j) - u(i1, j) - d1(i, j))
                if (j == n2) then
                    j1 = j - 1
                    j2 = j
                else
                    j1 = j
                    j2 = j + 1
                end if
                d2t(i, j) = d2t(i, j) + (u(i, j2) - u(i, j1) - d2(i, j))
            end do
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Isotropic TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function tv_iso_filt_2d_float
!
!> 3D isotropic TV denoising
!
function tv_iso_filt_3d_float(f, mu, niter, verbose) result(u)
    real, dimension(:, :, :), intent(in) :: f
    real, intent(in) :: mu
    integer, intent(in) :: niter
    logical, optional :: verbose
    real, allocatable, dimension(:, :, :) :: u
    integer :: i, j, k, iter
    real :: tmpx, tmpy, tmpz, sumu
    real, allocatable, dimension(:, :, :) :: d1, d2, d3, d1t, d2t, d3t
    real, allocatable, dimension(:, :, :) :: pu
    integer :: n1, n2, n3
    integer :: i1, i2, j1, j2, k1, k2
    integer :: ii1, ii2, jj1, jj2, kk1, kk2
    real :: lambda, sk
    logical :: tv_verbose
    n1 = size(f, 1)
    n2 = size(f, 2)
    n3 = size(f, 3)
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1, n2, n3)
    d2 = zeros(n1, n2, n3)
    d3 = zeros(n1, n2, n3)
    d1t = zeros(n1, n2, n3)
    d2t = zeros(n1, n2, n3)
    d3t = zeros(n1, n2, n3)
    ! read noisy image
    u = f
    pu = zeros_like(u)
    ! lambdas for u and w subproblems
    lambda = 2.0*mu
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        ! minimization u
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    if (i == 1) then
                        i1 = 0
                        ii1 = i
                    else
                        i1 = 1
                        ii1 = i - 1
                    end if
                    if (i == n1) then
                        i2 = 0
                        ii2 = i
                    else
                        i2 = 1
                        ii2 = i + 1
                    end if
                    if (j == 1) then
                        j1 = 0
                        jj1 = j
                    else
                        j1 = 1
                        jj1 = j - 1
                    end if
                    if (j == n2) then
                        j2 = 0
                        jj2 = j
                    else
                        j2 = 1
                        jj2 = j + 1
                    end if
                    if (k == 1) then
                        k1 = 0
                        kk1 = k
                    else
                        k1 = 1
                        kk1 = k - 1
                    end if
                    if (k == n3) then
                        k2 = 0
                        kk2 = k
                    else
                        k2 = 1
                        kk2 = k + 1
                    end if
                    sumu = lambda*( &
                        +i2*u(ii2, j, k) + i1*u(ii1, j, k) &
                        + j2*u(i, jj2, k) + j1*u(i, jj1, k) &
                        + k2*u(i, j, kk2) + k1*u(i, j, kk1)) &
                        + lambda*( &
                        +(i1*d1(ii1, j, k) - i2*d1(i, j, k)) &
                        + (j1*d2(i, jj1, k) - j2*d2(i, j, k)) &
                        + (k1*d3(i, j, kk1) - k2*d3(i, j, k)) &
                        - (i1*d1t(ii1, j, k) - i2*d1t(i, j, k)) &
                        - (j1*d2t(i, jj1, k) - j2*d2t(i, j, k)) &
                        - (k1*d3t(i, j, kk1) - k2*d3t(i, j, k))) &
                        + mu*f(i, j, k)
                    u(i, j, k) = sumu/(mu + (6.0 - &
                        (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2) - (1 - k1) - (1 - k2))*lambda)
                end do
            end do
        end do
        ! update multiplietgpv_filt_2d_rs
        ! minimization d
        !$omp parallel do private(i, j, k, i1, i2, j1, j2, k1, k2, tmpx, tmpy, tmpz, sk)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    tmpx = (u(i2, j, k) - u(i1, j, k)) + d1t(i, j, k)
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    tmpy = (u(i, j2, k) - u(i, j1, k)) + d2t(i, j, k)
                    if (k == n3) then
                        k1 = k - 1
                        k2 = k
                    else
                        k1 = k
                        k2 = k + 1
                    end if
                    tmpz = (u(i, j, k2) - u(i, j, k1)) + d3t(i, j, k)
                    sk = sqrt(tmpx**2 + tmpy**2 + tmpz**2)
                    d1(i, j, k) = max(sk - 1.0/lambda, 0.0)*tmpx/(sk + float_tiny)
                    d2(i, j, k) = max(sk - 1.0/lambda, 0.0)*tmpy/(sk + float_tiny)
                    d3(i, j, k) = max(sk - 1.0/lambda, 0.0)*tmpz/(sk + float_tiny)
                end do
            end do
        end do
        !$omp end parallel do
        ! d
        !$omp parallel do private(i, j, k, i1, i2, j1, j2, k1, k2)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    d1t(i, j, k) = d1t(i, j, k) + (u(i2, j, k) - u(i1, j, k) - d1(i, j, k))
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    d2t(i, j, k) = d2t(i, j, k) + (u(i, j2, k) - u(i, j1, k) - d2(i, j, k))
                    if (k == n3) then
                        k1 = k - 1
                        k2 = k
                    else
                        k1 = k
                        k2 = k + 1
                    end if
                    d3t(i, j, k) = d3t(i, j, k) + (u(i, j, k2) - u(i, j, k1) - d3(i, j, k))
                end do
            end do
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Isotropic TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function tv_iso_filt_3d_float
!
!> 1D TGpV denoising
!
function tgpv_filt_1d_float(f, mu, alpha0, alpha1, niter, p, verbose) result(u)
    real, dimension(:), intent(in) :: f
    real, intent(in) :: mu, alpha0, alpha1, p
    integer, intent(in) :: niter
    logical, optional :: verbose
    real, allocatable, dimension(:) :: u
    integer :: i, iter
    real :: tmp, sumu, sumw1
    real, allocatable, dimension(:) :: d1, d1t
    real, allocatable, dimension(:) :: s11
    real, allocatable, dimension(:) :: s11t
    real, allocatable, dimension(:) :: w1
    real, allocatable, dimension(:) :: pu
    integer :: n1
    integer :: inner, ninner
    integer :: i1, i2
    integer :: ii1, ii2
    real :: lambda0, lambda1
    logical :: tv_verbose
    n1 = size(f)
    ninner = 2
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1)
    d1t = zeros(n1)
    s11 = zeros(n1)
    s11t = zeros(n1)
    w1 = zeros(n1)
    pu = zeros(n1)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda0 = 2.0*mu
    lambda1 = 2.0*mu*(alpha1/alpha0)
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! minimization u
            do i = 1, n1
                if (i == 1) then
                    i1 = 0
                    ii1 = i
                else
                    i1 = 1
                    ii1 = i - 1
                end if
                if (i == n1) then
                    i2 = 0
                    ii2 = i
                else
                    i2 = 1
                    ii2 = i + 1
                end if
                sumu = lambda0*( &
                    +i2*u(ii2) + i1*u(ii1)) &
                    + lambda0*( &
                    +(i1*d1(ii1) - i2*d1(i)) &
                    - (i1*d1t(ii1) - i2*d1t(i)) &
                    + (i1*w1(ii1) - i2*w1(i)) &
                    ) &
                    + mu*f(i)
                u(i) = sumu/(mu + (2.0 - (1 - i1) - (1 - i2))*lambda0)
            end do
            if (alpha1 /= 0) then
                ! minimization of w
                do i = 1, n1
                    if (i == 1) then
                        i1 = 0
                        ii1 = i
                    else
                        i1 = 1
                        ii1 = i - 1
                    end if
                    if (i == n1) then
                        i2 = 0
                        ii2 = i
                    else
                        i2 = 1
                        ii2 = i + 1
                    end if
                    ! w1
                    sumw1 = &
                        +lambda1*(i2*w1(ii2) + i1*w1(ii1)) &
                        - lambda0*(d1(i) - d1t(i) - (i2*u(ii2) - u(i))) &
                        + lambda1*(i1*s11(ii1) - i2*s11(i) - (i1*s11t(ii1) - i2*s11t(i)))
                    w1(i) = sumw1/(lambda0 + (2.0 - (1 - i1) - (1 - i2))*lambda1)
                end do
            end if
            ! update multipliers
            ! minimization d
            !$omp parallel do private(i, i1, i2, tmp)
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                tmp = (u(i2) - u(i1)) - w1(i) + d1t(i)
                d1(i) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
            end do
            !$omp end parallel do
            if (alpha1 /= 0) then
                !$omp parallel do private(i, i1, i2, tmp)
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    tmp = w1(i2) - w1(i1) + s11t(i)
                    s11(i) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                end do
                !$omp end parallel do
            end if
        end do
        ! d
        !$omp parallel do private(i, i1, i2)
        do i = 1, n1
            if (i == n1) then
                i1 = i - 1
                i2 = i
            else
                i1 = i
                i2 = i + 1
            end if
            d1t(i) = d1t(i) + (u(i2) - u(i1) - w1(i) - d1(i))
        end do
        !$omp end parallel do
        if (alpha1 /= 0) then
            !$omp parallel do private(i, i1, i2)
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                s11t(i) = s11t(i) + (w1(i2) - w1(i1)) - s11(i)
            end do
            !$omp end parallel do
        end if
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> TGpV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function tgpv_filt_1d_float
!
!> 2D TGpV denoising
!
function tgpv_filt_2d_float(f, mu, alpha0, alpha1, niter, p, verbose) result(u)
    real, dimension(:, :), intent(in) :: f
    real, intent(in) :: mu, alpha0, alpha1, p
    integer, intent(in) :: niter
    logical, optional :: verbose
    real, allocatable, dimension(:, :) :: u
    integer :: i, j, iter
    real :: tmp, sumu, sumw1, sumw2
    real, allocatable, dimension(:, :) :: d1, d2, d1t, d2t
    real, allocatable, dimension(:, :) :: s11, s12, s22
    real, allocatable, dimension(:, :) :: s11t, s12t, s22t
    real, allocatable, dimension(:, :) :: w1, w2
    real, allocatable, dimension(:, :) :: tmpd1w2
    real, allocatable, dimension(:, :) :: tmpd2w1
    real, allocatable, dimension(:, :) :: pu
    integer :: n1, n2
    integer :: inner, ninner
    integer :: i1, i2, j1, j2
    integer :: ii1, ii2, jj1, jj2
    real :: lambda0, lambda1
    logical :: tv_verbose
    n1 = size(f, 1)
    n2 = size(f, 2)
    ninner = 2
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1, n2)
    d2 = zeros(n1, n2)
    d1t = zeros(n1, n2)
    d2t = zeros(n1, n2)
    s11 = zeros(n1, n2)
    s12 = zeros(n1, n2)
    s22 = zeros(n1, n2)
    s11t = zeros(n1, n2)
    s12t = zeros(n1, n2)
    s22t = zeros(n1, n2)
    w1 = zeros(n1, n2)
    w2 = zeros(n1, n2)
    tmpd1w2 = zeros(n1, n2)
    tmpd2w1 = zeros(n1, n2)
    pu = zeros(n1, n2)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda0 = 2.0*mu
    lambda1 = 2.0*mu*(alpha1/alpha0)
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! minimization u
            do j = 1, n2
                do i = 1, n1
                    if (i == 1) then
                        i1 = 0
                        ii1 = i
                    else
                        i1 = 1
                        ii1 = i - 1
                    end if
                    if (i == n1) then
                        i2 = 0
                        ii2 = i
                    else
                        i2 = 1
                        ii2 = i + 1
                    end if
                    if (j == 1) then
                        j1 = 0
                        jj1 = j
                    else
                        j1 = 1
                        jj1 = j - 1
                    end if
                    if (j == n2) then
                        j2 = 0
                        jj2 = j
                    else
                        j2 = 1
                        jj2 = j + 1
                    end if
                    sumu = lambda0*( &
                        +i2*u(ii2, j) + i1*u(ii1, j) &
                        + j2*u(i, jj2) + j1*u(i, jj1)) &
                        + lambda0*( &
                        +(i1*d1(ii1, j) - i2*d1(i, j)) &
                        + (j1*d2(i, jj1) - j2*d2(i, j)) &
                        - (i1*d1t(ii1, j) - i2*d1t(i, j)) &
                        - (j1*d2t(i, jj1) - j2*d2t(i, j)) &
                        + (i1*w1(ii1, j) - i2*w1(i, j)) &
                        + (j1*w2(i, jj1) - j2*w2(i, j)) &
                        ) &
                        + mu*f(i, j)
                    u(i, j) = sumu/(mu + (4.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2))*lambda0)
                end do
            end do
            if (alpha1 /= 0) then
                ! minimization w
                !$omp parallel do private(i, j, i1, i2, j1, j2)
                do j = 1, n2
                    do i = 1, n1
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmpd1w2(i, j) = 0.5*(w2(i2, j) - w2(i1, j))
                        tmpd2w1(i, j) = 0.5*(w1(i, j2) - w1(i, j1))
                    end do
                end do
                !$omp end parallel do
                ! minimization of w
                do j = 1, n2
                    do i = 1, n1
                        if (i == 1) then
                            i1 = 0
                            ii1 = i
                        else
                            i1 = 1
                            ii1 = i - 1
                        end if
                        if (i == n1) then
                            i2 = 0
                            ii2 = i
                        else
                            i2 = 1
                            ii2 = i + 1
                        end if
                        if (j == 1) then
                            j1 = 0
                            jj1 = j
                        else
                            j1 = 1
                            jj1 = j - 1
                        end if
                        if (j == n2) then
                            j2 = 0
                            jj2 = j
                        else
                            j2 = 1
                            jj2 = j + 1
                        end if
                        ! w1
                        sumw1 = &
                            +lambda1*(i2*w1(ii2, j) + i1*w1(ii1, j)) &
                            + 0.5*lambda1*(j2*w1(i, jj2) + j1*w1(i, jj1)) &
                            - lambda0*(d1(i, j) - d1t(i, j) - (i2*u(ii2, j) - u(i, j))) &
                            + lambda1*(i1*s11(ii1, j) - i2*s11(i, j) - (i1*s11t(ii1, j) - i2*s11t(i, j))) &
                            + lambda1*(j1*s12(i, jj1) - j2*s12(i, j) - (j1*s12t(i, jj1) - j2*s12t(i, j)) - (j1*tmpd1w2(i, jj1) - j2*tmpd1w2(i, j)))
                        w1(i, j) = sumw1/(lambda0 + (2.0 - (1 - i1) - (1 - i2))*lambda1 + (2.0 - (1 - j1) - (1 - j2))*0.5*lambda1)
                        ! w2
                        sumw2 = &
                            +0.5*lambda1*(i2*w2(ii2, j) + i1*w2(ii1, j)) &
                            + lambda1*(j2*w2(i, jj2) + j1*w2(i, jj1)) &
                            - lambda0*(d2(i, j) - d2t(i, j) - (j2*u(i, jj2) - u(i, j))) &
                            + lambda1*(i1*s12(ii1, j) - i2*s12(i, j) - (i1*s12t(ii1, j) - i2*s12t(i, j)) - (i1*tmpd2w1(ii1, j) - i2*tmpd2w1(i, j))) &
                            + lambda1*(j1*s22(i, jj1) - j2*s22(i, j) - (j1*s22t(i, jj1) - j2*s22t(i, j)))
                        w2(i, j) = sumw2/(lambda0 + (2.0 - (1 - i1) - (1 - i2))*0.5*lambda1 + (2.0 - (1 - j1) - (1 - j2))*lambda1)
                    end do
                end do
            end if
            ! update multipliers
            ! minimization d
            !$omp parallel do private(i, j, i1, i2, j1, j2, tmp)
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    tmp = (u(i2, j) - u(i1, j)) - w1(i, j) + d1t(i, j)
                    d1(i, j) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    tmp = (u(i, j2) - u(i, j1)) - w2(i, j) + d2t(i, j)
                    d2(i, j) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                end do
            end do
            !$omp end parallel do
            if (alpha1 /= 0) then
                !$omp parallel do private(i, j, i1, i2, j1, j2, tmp)
                do j = 1, n2
                    do i = 1, n1
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmp = w1(i2, j) - w1(i1, j) + s11t(i, j)
                        s11(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        tmp = w2(i, j2) - w2(i, j1) + s22t(i, j)
                        s22(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        tmp = 0.5*(w2(i2, j) - w2(i1, j) + w1(i, j2) - w1(i, j1)) + s12t(i, j)
                        s12(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                    end do
                end do
                !$omp end parallel do
            end if
        end do
        ! d
        !$omp parallel do private(i, j, i1, i2, j1, j2)
        do j = 1, n2
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                d1t(i, j) = d1t(i, j) + (u(i2, j) - u(i1, j) - w1(i, j) - d1(i, j))
                if (j == n2) then
                    j1 = j - 1
                    j2 = j
                else
                    j1 = j
                    j2 = j + 1
                end if
                d2t(i, j) = d2t(i, j) + (u(i, j2) - u(i, j1) - w2(i, j) - d2(i, j))
            end do
        end do
        !$omp end parallel do
        if (alpha1 /= 0) then
            !$omp parallel do private(i, j, i1, i2, j1, j2, tmp)
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    s11t(i, j) = s11t(i, j) + (w1(i2, j) - w1(i1, j)) - s11(i, j)
                    s22t(i, j) = s22t(i, j) + (w2(i, j2) - w2(i, j1)) - s22(i, j)
                    s12t(i, j) = s12t(i, j) + 0.5*(w1(i, j2) - w1(i, j1) + w2(i2, j) - w2(i1, j)) - s12(i, j)
                end do
            end do
            !$omp end parallel do
        end if
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> TGpV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function tgpv_filt_2d_float
!
!> 3D TGpV denoising
!
function tgpv_filt_3d_float(f, mu, alpha0, alpha1, niter, p) result(u)
    real, dimension(:, :, :), intent(in) :: f
    real, intent(in) :: mu, alpha0, alpha1, p
    integer, intent(in) :: niter
    real, allocatable, dimension(:, :, :) :: u
    integer :: i, j, k
    real :: tmp, sumu, sumw1, sumw2, sumw3
    real, allocatable, dimension(:, :, :) :: d1, d2, d3, d1t, d2t, d3t
    real, allocatable, dimension(:, :, :) :: s11, s12, s13, s22, s23, s33
    real, allocatable, dimension(:, :, :) :: s11t, s12t, s13t, s22t, s23t, s33t
    real, allocatable, dimension(:, :, :) :: w1, w2, w3
    real, allocatable, dimension(:, :, :) :: tmpd1w2, tmpd1w3
    real, allocatable, dimension(:, :, :) :: tmpd2w1, tmpd2w3
    real, allocatable, dimension(:, :, :) :: tmpd3w1, tmpd3w2
    real, allocatable, dimension(:, :, :) :: pu
    integer :: n1, n2, n3
    integer :: iter, inner, ninner
    integer :: i1, i2, j1, j2, k1, k2
    integer :: ii1, ii2, jj1, jj2, kk1, kk2
    real :: lambda0, lambda1
    real :: bsum
    ! number of inner iteration
    ninner = 2
    ! lambdas for u and w subproblems
    lambda0 = 2.0*mu
    lambda1 = 2.0*mu*(alpha1/alpha0)
    n1 = size(f, 1)
    n2 = size(f, 2)
    n3 = size(f, 3)
    ! memory
    d1 = zeros(n1, n2, n3)
    d2 = zeros(n1, n2, n3)
    d3 = zeros(n1, n2, n3)
    d1t = zeros(n1, n2, n3)
    d2t = zeros(n1, n2, n3)
    d3t = zeros(n1, n2, n3)
    s11 = zeros(n1, n2, n3)
    s12 = zeros(n1, n2, n3)
    s13 = zeros(n1, n2, n3)
    s22 = zeros(n1, n2, n3)
    s23 = zeros(n1, n2, n3)
    s33 = zeros(n1, n2, n3)
    s11t = zeros(n1, n2, n3)
    s12t = zeros(n1, n2, n3)
    s13t = zeros(n1, n2, n3)
    s22t = zeros(n1, n2, n3)
    s23t = zeros(n1, n2, n3)
    s33t = zeros(n1, n2, n3)
    w1 = zeros(n1, n2, n3)
    w2 = zeros(n1, n2, n3)
    w3 = zeros(n1, n2, n3)
    u = zeros(n1, n2, n3)
    tmpd1w2 = zeros(n1, n2, n3)
    tmpd1w3 = zeros(n1, n2, n3)
    tmpd2w1 = zeros(n1, n2, n3)
    tmpd2w3 = zeros(n1, n2, n3)
    tmpd3w1 = zeros(n1, n2, n3)
    tmpd3w2 = zeros(n1, n2, n3)
    pu = zeros(n1, n2, n3)
    ! noisy image
    u = f
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! Minimization of u
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        if (i == 1) then
                            i1 = 0
                            ii1 = i
                        else
                            i1 = 1
                            ii1 = i - 1
                        end if
                        if (i == n1) then
                            i2 = 0
                            ii2 = i
                        else
                            i2 = 1
                            ii2 = i + 1
                        end if
                        if (j == 1) then
                            j1 = 0
                            jj1 = j
                        else
                            j1 = 1
                            jj1 = j - 1
                        end if
                        if (j == n2) then
                            j2 = 0
                            jj2 = j
                        else
                            j2 = 1
                            jj2 = j + 1
                        end if
                        if (k == 1) then
                            k1 = 0
                            kk1 = k
                        else
                            k1 = 1
                            kk1 = k - 1
                        end if
                        if (k == n3) then
                            k2 = 0
                            kk2 = k
                        else
                            k2 = 1
                            kk2 = k + 1
                        end if
                        sumu = lambda0*( &
                            +i2*u(ii2, j, k) + i1*u(ii1, j, k) &
                            + j2*u(i, jj2, k) + j1*u(i, jj1, k) &
                            + k2*u(i, j, kk2) + k1*u(i, j, kk1)) &
                            + lambda0*( &
                            +(i1*d1(ii1, j, k) - i2*d1(i, j, k)) &
                            + (j1*d2(i, jj1, k) - j2*d2(i, j, k)) &
                            + (k1*d3(i, j, kk1) - k2*d3(i, j, k)) &
                            - (i1*d1t(ii1, j, k) - i2*d1t(i, j, k)) &
                            - (j1*d2t(i, jj1, k) - j2*d2t(i, j, k)) &
                            - (k1*d3t(i, j, kk1) - k2*d3t(i, j, k)) &
                            + (i1*w1(ii1, j, k) - i2*w1(i, j, k)) &
                            + (j1*w2(i, jj1, k) - j2*w2(i, j, k)) &
                            + (k1*w3(i, j, kk1) - k2*w3(i, j, k)) &
                            ) &
                            + mu*f(i, j, k)
                        u(i, j, k) = sumu/(mu + (6.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2) - (1 - k1) - (1 - k2))*lambda0)
                    end do
                end do
            end do
            if (alpha1 /= 0) then
                ! minimization w
                do k = 1, n3
                    do j = 1, n2
                        do i = 1, n1
                            if (i == n1) then
                                i1 = i - 1
                                i2 = i
                            else
                                i1 = i
                                i2 = i + 1
                            end if
                            if (j == n2) then
                                j1 = j - 1
                                j2 = j
                            else
                                j1 = j
                                j2 = j + 1
                            end if
                            if (k == n3) then
                                k1 = k - 1
                                k2 = k
                            else
                                k1 = k
                                k2 = k + 1
                            end if
                            tmpd1w2(i, j, k) = 0.5*(w2(i2, j, k) - w2(i1, j, k))
                            tmpd1w3(i, j, k) = 0.5*(w3(i2, j, k) - w3(i1, j, k))
                            tmpd2w1(i, j, k) = 0.5*(w1(i, j2, k) - w1(i, j1, k))
                            tmpd2w3(i, j, k) = 0.5*(w3(i, j2, k) - w3(i, j1, k))
                            tmpd3w1(i, j, k) = 0.5*(w1(i, j, k2) - w1(i, j, k1))
                            tmpd3w2(i, j, k) = 0.5*(w2(i, j, k2) - w2(i, j, k1))
                        end do
                    end do
                end do
                ! minimization of w
                do k = 1, n3
                    do j = 1, n2
                        do i = 1, n1
                            if (i == 1) then
                                i1 = 0
                                ii1 = i
                            else
                                i1 = 1
                                ii1 = i - 1
                            end if
                            if (i == n1) then
                                i2 = 0
                                ii2 = i
                            else
                                i2 = 1
                                ii2 = i + 1
                            end if
                            if (j == 1) then
                                j1 = 0
                                jj1 = j
                            else
                                j1 = 1
                                jj1 = j - 1
                            end if
                            if (j == n2) then
                                j2 = 0
                                jj2 = j
                            else
                                j2 = 1
                                jj2 = j + 1
                            end if
                            if (k == 1) then
                                k1 = 0
                                kk1 = k
                            else
                                k1 = 1
                                kk1 = k - 1
                            end if
                            if (k == n3) then
                                k2 = 0
                                kk2 = k
                            else
                                k2 = 1
                                kk2 = k + 1
                            end if
                            ! w1
                            sumw1 = &
                                +lambda1*(i2*w1(ii2, j, k) + i1*w1(ii1, j, k)) &
                                + 0.5*lambda1*(j2*w1(i, jj2, k) + j1*w1(i, jj1, k)) &
                                + 0.5*lambda1*(k2*w1(i, j, kk2) + k1*w1(i, j, kk1)) &
                                - lambda0*(d1(i, j, k) - d1t(i, j, k) - (i2*u(ii2, j, k) - u(i, j, k))) &
                                + lambda1*(i1*s11(ii1, j, k) - i2*s11(i, j, k) - (i1*s11t(ii1, j, k) - i2*s11t(i, j, k))) &
                                + lambda1*(j1*s12(i, jj1, k) - j2*s12(i, j, k) - (j1*s12t(i, jj1, k) - j2*s12t(i, j, k)) - (j1*tmpd1w2(i, jj1, k) - j2*tmpd1w2(i, j, k))) &
                                + lambda1*(k1*s13(i, j, kk1) - k2*s13(i, j, k) - (k1*s13t(i, j, kk1) - k2*s13t(i, j, k)) - (k1*tmpd1w3(i, j, kk1) - k2*tmpd1w3(i, j, k)))
                            w1(i, j, k) = sumw1/(lambda0 + (2.0 - (1-i1) - (1-i2))*lambda1 + (2.0 - (1-j1) - (1-j2))*0.5*lambda1 + (2.0 - (1-k1) - (1-k2))*0.5*lambda1)
                            ! w2
                            sumw2 = &
                                +0.5*lambda1*(i2*w2(ii2, j, k) + i1*w2(ii1, j, k)) &
                                + lambda1*(j2*w2(i, jj2, k) + j1*w2(i, jj1, k)) &
                                + 0.5*lambda1*(k2*w2(i, j, kk2) + k1*w2(i, j, kk1)) &
                                - lambda0*(d2(i, j, k) - d2t(i, j, k) - (j2*u(i, jj2, k) - u(i, j, k))) &
                                + lambda1*(i1*s12(ii1, j, k) - i2*s12(i, j, k) - (i1*s12t(ii1, j, k) - i2*s12t(i, j, k)) - (i1*tmpd2w1(ii1, j, k) - i2*tmpd2w1(i, j, k))) &
                                + lambda1*(j1*s22(i, jj1, k) - j2*s22(i, j, k) - (j1*s22t(i, jj1, k) - j2*s22t(i, j, k))) &
                                + lambda1*(k1*s23(i, j, kk1) - k2*s23(i, j, k) - (k1*s23t(i, j, kk1) - k2*s23t(i, j, k)) - (k1*tmpd2w3(i, j, kk1) - k2*tmpd2w3(i, j, k)))
                            w2(i, j, k) = sumw2/(lambda0 + (2.0 - (1-i1) - (1-i2))*0.5*lambda1 + (2.0 - (1-j1) - (1-j2))*lambda1 + (2.0 - (1-k1) - (1-k2))*0.5*lambda1)
                            ! w3
                            sumw3 = &
                                +0.5*lambda1*(i2*w3(ii2, j, k) + i1*w3(ii1, j, k)) &
                                + 0.5*lambda1*(j2*w3(i, jj2, k) + j1*w3(i, jj1, k)) &
                                + lambda1*(k2*w3(i, j, kk2) + k1*w3(i, j, kk1)) &
                                - lambda0*(d3(i, j, k) - d3t(i, j, k) - (k2*u(i, j, kk2) - u(i, j, k))) &
                                + lambda1*(i1*s13(ii1, j, k) - i2*s13(i, j, k) - (i1*s13t(ii1, j, k) - i2*s13t(i, j, k)) - (i1*tmpd3w1(ii1, j, k) - i2*tmpd3w1(i, j, k))) &
                                + lambda1*(j1*s23(i, jj1, k) - j2*s23(i, j, k) - (j1*s23t(i, jj1, k) - j2*s23t(i, j, k)) - (j1*tmpd3w2(i, jj1, k) - j2*tmpd3w2(i, j, k))) &
                                + lambda1*(k1*s33(i, j, kk1) - k2*s33(i, j, k) - (k1*s33t(i, j, kk1) - k2*s33t(i, j, k)))
                            w3(i, j, k) = sumw3/(lambda0 + (2.0 - (1-i1) - (1-i2))*0.5*lambda1 + (2.0 - (1-j1) - (1-j2))*0.5*lambda1 + (2.0 - (1-k1) - (1-k2))*lambda1)
                        end do
                    end do
                end do
            end if
            ! update multipliers
            ! minimization d
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        tmp = (u(i2, j, k) - u(i1, j, k)) - w1(i, j, k) + d1t(i, j, k)
                        d1(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmp = (u(i, j2, k) - u(i, j1, k)) - w2(i, j, k) + d2t(i, j, k)
                        d2(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                        if (k == n3) then
                            k1 = k - 1
                            k2 = k
                        else
                            k1 = k
                            k2 = k + 1
                        end if
                        tmp = (u(i, j, k2) - u(i, j, k1)) - w3(i, j, k) + d3t(i, j, k)
                        d3(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                    end do
                end do
            end do
            if (alpha1 /= 0) then
                do k = 1, n3
                    do j = 1, n2
                        do i = 1, n1
                            if (i == n1) then
                                i1 = i - 1
                                i2 = i
                            else
                                i1 = i
                                i2 = i + 1
                            end if
                            if (j == n2) then
                                j1 = j - 1
                                j2 = j
                            else
                                j1 = j
                                j2 = j + 1
                            end if
                            if (k == n3) then
                                k1 = k - 1
                                k2 = k
                            else
                                k1 = k
                                k2 = k + 1
                            end if
                            tmp = w1(i2, j, k) - w1(i1, j, k) + s11t(i, j, k)
                            s11(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = w2(i, j2, k) - w2(i, j1, k) + s22t(i, j, k)
                            s22(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = w3(i, j, k2) - w3(i, j, k1) + s33t(i, j, k)
                            s33(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w2(i2, j, k) - w2(i1, j, k) + w1(i, j2, k) - w1(i, j1, k)) + s12t(i, j, k)
                            s12(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w3(i2, j, k) - w3(i1, j, k) + w1(i, j, k2) - w1(i, j, k1)) + s13t(i, j, k)
                            s13(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w3(i, j2, k) - w3(i, j1, k) + w2(i, j, k2) - w2(i, j, k1)) + s23t(i, j, k)
                            s23(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        end do
                    end do
                end do
            end if
        end do
        ! d
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    d1t(i, j, k) = d1t(i, j, k) + (u(i2, j, k) - u(i1, j, k) - w1(i, j, k) - d1(i, j, k))
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    d2t(i, j, k) = d2t(i, j, k) + (u(i, j2, k) - u(i, j1, k) - w2(i, j, k) - d2(i, j, k))
                    if (k == n3) then
                        k1 = k - 1
                        k2 = k
                    else
                        k1 = k
                        k2 = k + 1
                    end if
                    d3t(i, j, k) = d3t(i, j, k) + (u(i, j, k2) - u(i, j, k1) - w3(i, j, k) - d3(i, j, k))
                end do
            end do
        end do
        if (alpha1 /= 0) then
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        if (k == n3) then
                            k1 = k - 1
                            k2 = k
                        else
                            k1 = k
                            k2 = k + 1
                        end if
                        s11t(i, j, k) = s11t(i, j, k) + (w1(i2, j, k) - w1(i1, j, k)) - s11(i, j, k)
                        s22t(i, j, k) = s22t(i, j, k) + (w2(i, j2, k) - w2(i, j1, k)) - s22(i, j, k)
                        s33t(i, j, k) = s33t(i, j, k) + (w3(i, j, k2) - w3(i, j, k1)) - s33(i, j, k)
                        s12t(i, j, k) = s12t(i, j, k) + 0.5*(w1(i, j2, k) - w1(i, j1, k) + w2(i2, j, k) - w2(i1, j, k)) - s12(i, j, k)
                        s13t(i, j, k) = s13t(i, j, k) + 0.5*(w1(i, j, k2) - w1(i, j, k1) + w3(i2, j, k) - w3(i1, j, k)) - s13(i, j, k)
                        s23t(i, j, k) = s23t(i, j, k) + 0.5*(w2(i, j, k2) - w2(i, j, k1) + w3(i, j2, k) - w3(i, j1, k)) - s23(i, j, k)
                    end do
                end do
            end do
        end if
        ! progress
        if (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1) then
            bsum = sum((u - pu)**2)
            call warn(' >> TGpV iteration '//num2str(iter, '(i)') &
                //' of '//num2str(niter, '(i)')//' relative l2-norm difference = '//num2str(sqrt(bsum), '(es)'))
        end if
    end do
end function tgpv_filt_3d_float
!
!> 2D TGpV denoising MPI version
!
function tgpv_filt_2d_mpi_float(f0, mu, alpha0, alpha1, niter, p) result(u0)
    real, dimension(:, :), intent(in) :: f0
    real, intent(in) :: mu, alpha0, alpha1, p
    integer, intent(in) :: niter
    real, allocatable, dimension(:, :) :: u0
    integer :: i, j
    real :: tmp, sumu, sumw1, sumw2
    real, allocatable, dimension(:, :) :: d1, d2, d1t, d2t
    real, allocatable, dimension(:, :) :: s11, s12, s22
    real, allocatable, dimension(:, :) :: s11t, s12t, s22t
    real, allocatable, dimension(:, :) :: w1, w2, u, f
    real, allocatable, dimension(:, :) :: tmpd1w2
    real, allocatable, dimension(:, :) :: tmpd2w1
    real, allocatable, dimension(:, :) :: pu
    integer :: n1, n2
    integer :: iter, inner, ninner
    integer :: i1, i2, j1, j2
    integer :: ii1, ii2, jj1, jj2
    real :: lambda0, lambda1
    integer :: n1beg, n1end, n2beg, n2end
    real :: bsum
    logical :: nonempty
    ! number of inner iteration
    ninner = 2
    ! lambdas for u and w subproblems
    lambda0 = 2.0*mu
    lambda1 = 2.0*mu*(alpha1/alpha0)
    n1 = size(f0, 1)
    n2 = size(f0, 2)
    call domain_decomp_regular(n1, n2, n1beg, n1end, n2beg, n2end)
    if (rankid <= rank1*rank2 - 1) then
        nonempty = .true.
    else
        nonempty = .false.
    end if
    ! memory
    call alloc_array(d1, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(d2, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(d1t, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(d2t, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s11, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s12, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s22, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s11t, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s12t, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s22t, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(w1, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(w2, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(u, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(f, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(tmpd1w2, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(tmpd2w1, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(pu, [n1beg, n1end, n2beg, n2end], pad=1)
    ! noisy image
    f(n1beg:n1end, n2beg:n2end) = f0(n1beg:n1end, n2beg:n2end)
    u(n1beg:n1end, n2beg:n2end) = f0(n1beg:n1end, n2beg:n2end)
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            if (nonempty) then
                call commute_array(u, 1)
                call commute_array(d1, 1)
                call commute_array(d2, 1)
                call commute_array(d1t, 1)
                call commute_array(d2t, 1)
                call commute_array(w1, 1)
                call commute_array(w2, 1)
            end if
            ! minimization u
            do j = n2beg, n2end
                do i = n1beg, n1end
                    if (i == 1) then
                        i1 = 0
                        ii1 = i
                    else
                        i1 = 1
                        ii1 = i - 1
                    end if
                    if (i == n1) then
                        i2 = 0
                        ii2 = i
                    else
                        i2 = 1
                        ii2 = i + 1
                    end if
                    if (j == 1) then
                        j1 = 0
                        jj1 = j
                    else
                        j1 = 1
                        jj1 = j - 1
                    end if
                    if (j == n2) then
                        j2 = 0
                        jj2 = j
                    else
                        j2 = 1
                        jj2 = j + 1
                    end if
                    sumu = lambda0*( &
                        +i2*u(ii2, j) + i1*u(ii1, j) &
                        + j2*u(i, jj2) + j1*u(i, jj1)) &
                        + lambda0*( &
                        +(i1*d1(ii1, j) - i2*d1(i, j)) &
                        + (j1*d2(i, jj1) - j2*d2(i, j)) &
                        - (i1*d1t(ii1, j) - i2*d1t(i, j)) &
                        - (j1*d2t(i, jj1) - j2*d2t(i, j)) &
                        + (i1*w1(ii1, j) - i2*w1(i, j)) &
                        + (j1*w2(i, jj1) - j2*w2(i, j)) &
                        ) &
                        + mu*f(i, j)
                    u(i, j) = sumu/(mu + (4.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2))*lambda0)
                end do
            end do
            if (alpha1 /= 0) then
                ! minimization w
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmpd1w2(i, j) = 0.5*(w2(i2, j) - w2(i1, j))
                        tmpd2w1(i, j) = 0.5*(w1(i, j2) - w1(i, j1))
                    end do
                end do
                if (nonempty) then
                    call commute_array(u, 1)
                    call commute_array(tmpd1w2, 1)
                    call commute_array(tmpd2w1, 1)
                    call commute_array(s11, 1)
                    call commute_array(s12, 1)
                    call commute_array(s22, 1)
                    call commute_array(s11t, 1)
                    call commute_array(s12t, 1)
                    call commute_array(s22t, 1)
                end if
                ! minimization of w
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == 1) then
                            i1 = 0
                            ii1 = i
                        else
                            i1 = 1
                            ii1 = i - 1
                        end if
                        if (i == n1) then
                            i2 = 0
                            ii2 = i
                        else
                            i2 = 1
                            ii2 = i + 1
                        end if
                        if (j == 1) then
                            j1 = 0
                            jj1 = j
                        else
                            j1 = 1
                            jj1 = j - 1
                        end if
                        if (j == n2) then
                            j2 = 0
                            jj2 = j
                        else
                            j2 = 1
                            jj2 = j + 1
                        end if
                        ! w1
                        sumw1 = &
                            +lambda1*(i2*w1(ii2, j) + i1*w1(ii1, j)) &
                            + 0.5*lambda1*(j2*w1(i, jj2) + j1*w1(i, jj1)) &
                            - lambda0*(d1(i, j) - d1t(i, j) - (i2*u(ii2, j) - u(i, j))) &
                            + lambda1*(i1*s11(ii1, j) - i2*s11(i, j) - (i1*s11t(ii1, j) - i2*s11t(i, j))) &
                            + lambda1*(j1*s12(i, jj1) - j2*s12(i, j) - (j1*s12t(i, jj1) - j2*s12t(i, j)) - (j1*tmpd1w2(i, jj1) - j2*tmpd1w2(i, j)))
                        w1(i, j) = sumw1/(lambda0 + (2.0 - (1 - i1) - (1 - i2))*lambda1 + (2.0 - (1 - j1) - (1 - j2))*0.5*lambda1)
                        ! w2
                        sumw2 = &
                            +0.5*lambda1*(i2*w2(ii2, j) + i1*w2(ii1, j)) &
                            + lambda1*(j2*w2(i, jj2) + j1*w2(i, jj1)) &
                            - lambda0*(d2(i, j) - d2t(i, j) - (j2*u(i, jj2) - u(i, j))) &
                            + lambda1*(i1*s12(ii1, j) - i2*s12(i, j) - (i1*s12t(ii1, j) - i2*s12t(i, j)) - (i1*tmpd2w1(ii1, j) - i2*tmpd2w1(i, j))) &
                            + lambda1*(j1*s22(i, jj1) - j2*s22(i, j) - (j1*s22t(i, jj1) - j2*s22t(i, j)))
                        w2(i, j) = sumw2/(lambda0 + (2.0 - (1 - i1) - (1 - i2))*0.5*lambda1 + (2.0 - (1 - j1) - (1 - j2))*lambda1)
                    end do
                end do
            end if
            ! update multipliers
            ! minimization d
            do j = n2beg, n2end
                do i = n1beg, n1end
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    tmp = (u(i2, j) - u(i1, j)) - w1(i, j) + d1t(i, j)
                    d1(i, j) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    tmp = (u(i, j2) - u(i, j1)) - w2(i, j) + d2t(i, j)
                    d2(i, j) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                end do
            end do
            if (alpha1 /= 0) then
                if (nonempty) then
                    call commute_array(w1, 1)
                    call commute_array(w2, 1)
                end if
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmp = w1(i2, j) - w1(i1, j) + s11t(i, j)
                        s11(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        tmp = w2(i, j2) - w2(i, j1) + s22t(i, j)
                        s22(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        tmp = 0.5*(w2(i2, j) - w2(i1, j) + w1(i, j2) - w1(i, j1)) + s12t(i, j)
                        s12(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                    end do
                end do
            end if
            call mpi_barrier(mpi_comm_world, mpi_ierr)
        end do
        ! d
        do j = n2beg, n2end
            do i = n1beg, n1end
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                d1t(i, j) = d1t(i, j) + (u(i2, j) - u(i1, j) - w1(i, j) - d1(i, j))
                if (j == n2) then
                    j1 = j - 1
                    j2 = j
                else
                    j1 = j
                    j2 = j + 1
                end if
                d2t(i, j) = d2t(i, j) + (u(i, j2) - u(i, j1) - w2(i, j) - d2(i, j))
            end do
        end do
        if (alpha1 /= 0) then
            do j = n2beg, n2end
                do i = n1beg, n1end
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    s11t(i, j) = s11t(i, j) + (w1(i2, j) - w1(i1, j)) - s11(i, j)
                    s22t(i, j) = s22t(i, j) + (w2(i, j2) - w2(i, j1)) - s22(i, j)
                    s12t(i, j) = s12t(i, j) + 0.5*(w1(i, j2) - w1(i, j1) + w2(i2, j) - w2(i1, j)) - s12(i, j)
                end do
            end do
        end if
        call mpi_barrier(mpi_comm_world, mpi_ierr)
        ! progress
        if (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1) then
            if (nonempty) then
                bsum = sum((u - pu)**2)
            else
                bsum = 0.0
            end if
            call allreduce(bsum)
            if (rankid == 0) then
                call warn(' >> TGpV iteration '//num2str(iter, '(i)') &
                    //' of '//num2str(niter, '(i)')//' relative l2-norm difference = '//num2str(sqrt(bsum), '(es)'))
            end if
        end if
    end do
    call mpi_barrier(mpi_comm_world, mpi_ierr)
    call alloc_array(u0, [1, n1, 1, n2])
    if (nonempty) then
        u0(n1beg:n1end, n2beg:n2end) = u(n1beg:n1end, n2beg:n2end)
    end if
    call allreduce_array(u0)
    call mpi_barrier(mpi_comm_world, mpi_ierr)
end function tgpv_filt_2d_mpi_float
!
!> 3D TGpV denoising MPI version
!
function tgpv_filt_3d_mpi_float(f0, mu, alpha0, alpha1, niter, p) result(u0)
    real, dimension(:, :, :), intent(in) :: f0
    real, intent(in) :: mu, alpha0, alpha1, p
    integer, intent(in) :: niter
    real, allocatable, dimension(:, :, :) :: u0
    integer :: i, j, k
    real :: tmp, sumu, sumw1, sumw2, sumw3
    real, allocatable, dimension(:, :, :) :: d1, d2, d3, d1t, d2t, d3t
    real, allocatable, dimension(:, :, :) :: s11, s12, s13, s22, s23, s33
    real, allocatable, dimension(:, :, :) :: s11t, s12t, s13t, s22t, s23t, s33t
    real, allocatable, dimension(:, :, :) :: w1, w2, w3, u, f
    real, allocatable, dimension(:, :, :) :: tmpd1w2, tmpd1w3
    real, allocatable, dimension(:, :, :) :: tmpd2w1, tmpd2w3
    real, allocatable, dimension(:, :, :) :: tmpd3w1, tmpd3w2
    real, allocatable, dimension(:, :, :) :: pu
    integer :: n1, n2, n3
    integer :: iter, inner, ninner
    integer :: i1, i2, j1, j2, k1, k2
    integer :: ii1, ii2, jj1, jj2, kk1, kk2
    real :: lambda0, lambda1
    integer :: n1beg, n1end, n2beg, n2end, n3beg, n3end
    real :: bsum
    logical :: nonempty
    ! number of inner iteration
    ninner = 2
    ! lambdas for u and w subproblems
    lambda0 = 2.0*mu
    lambda1 = 2.0*mu*(alpha1/alpha0)
    n1 = size(f0, 1)
    n2 = size(f0, 2)
    n3 = size(f0, 3)
    call domain_decomp_regular(n1, n2, n3, n1beg, n1end, n2beg, n2end, n3beg, n3end)
    if (rankid <= rank1*rank2*rank3 - 1) then
        nonempty = .true.
    else
        nonempty = .false.
    end if
    ! memory
    call alloc_array(d1, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(d2, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(d3, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(d1t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(d2t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(d3t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s11, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s12, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s13, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s22, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s23, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s33, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s11t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s12t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s13t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s22t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s23t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s33t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(w1, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(w2, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(w3, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(u, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(f, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd1w2, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd1w3, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd2w1, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd2w3, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd3w1, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd3w2, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(pu, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    ! noisy image
    f(n1beg:n1end, n2beg:n2end, n3beg:n3end) = f0(n1beg:n1end, n2beg:n2end, n3beg:n3end)
    u(n1beg:n1end, n2beg:n2end, n3beg:n3end) = f0(n1beg:n1end, n2beg:n2end, n3beg:n3end)
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            if (nonempty) then
                call commute_array(u, 1)
                call commute_array(d1, 1)
                call commute_array(d2, 1)
                call commute_array(d3, 1)
                call commute_array(d1t, 1)
                call commute_array(d2t, 1)
                call commute_array(d3t, 1)
                call commute_array(w1, 1)
                call commute_array(w2, 1)
                call commute_array(w3, 1)
            end if
            ! minimization u
            do k = n3beg, n3end
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == 1) then
                            i1 = 0
                            ii1 = i
                        else
                            i1 = 1
                            ii1 = i - 1
                        end if
                        if (i == n1) then
                            i2 = 0
                            ii2 = i
                        else
                            i2 = 1
                            ii2 = i + 1
                        end if
                        if (j == 1) then
                            j1 = 0
                            jj1 = j
                        else
                            j1 = 1
                            jj1 = j - 1
                        end if
                        if (j == n2) then
                            j2 = 0
                            jj2 = j
                        else
                            j2 = 1
                            jj2 = j + 1
                        end if
                        if (k == 1) then
                            k1 = 0
                            kk1 = k
                        else
                            k1 = 1
                            kk1 = k - 1
                        end if
                        if (k == n3) then
                            k2 = 0
                            kk2 = k
                        else
                            k2 = 1
                            kk2 = k + 1
                        end if
                        sumu = lambda0*( &
                            +i2*u(ii2, j, k) + i1*u(ii1, j, k) &
                            + j2*u(i, jj2, k) + j1*u(i, jj1, k) &
                            + k2*u(i, j, kk2) + k1*u(i, j, kk1)) &
                            + lambda0*( &
                            +(i1*d1(ii1, j, k) - i2*d1(i, j, k)) &
                            + (j1*d2(i, jj1, k) - j2*d2(i, j, k)) &
                            + (k1*d3(i, j, kk1) - k2*d3(i, j, k)) &
                            - (i1*d1t(ii1, j, k) - i2*d1t(i, j, k)) &
                            - (j1*d2t(i, jj1, k) - j2*d2t(i, j, k)) &
                            - (k1*d3t(i, j, kk1) - k2*d3t(i, j, k)) &
                            + (i1*w1(ii1, j, k) - i2*w1(i, j, k)) &
                            + (j1*w2(i, jj1, k) - j2*w2(i, j, k)) &
                            + (k1*w3(i, j, kk1) - k2*w3(i, j, k)) &
                            ) &
                            + mu*f(i, j, k)
                        u(i, j, k) = sumu/(mu + (6.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2) - (1 - k1) - (1 - k2))*lambda0)
                    end do
                end do
            end do
            if (alpha1 /= 0) then
                ! minimization w
                do k = n3beg, n3end
                    do j = n2beg, n2end
                        do i = n1beg, n1end
                            if (i == n1) then
                                i1 = i - 1
                                i2 = i
                            else
                                i1 = i
                                i2 = i + 1
                            end if
                            if (j == n2) then
                                j1 = j - 1
                                j2 = j
                            else
                                j1 = j
                                j2 = j + 1
                            end if
                            if (k == n3) then
                                k1 = k - 1
                                k2 = k
                            else
                                k1 = k
                                k2 = k + 1
                            end if
                            tmpd1w2(i, j, k) = 0.5*(w2(i2, j, k) - w2(i1, j, k))
                            tmpd1w3(i, j, k) = 0.5*(w3(i2, j, k) - w3(i1, j, k))
                            tmpd2w1(i, j, k) = 0.5*(w1(i, j2, k) - w1(i, j1, k))
                            tmpd2w3(i, j, k) = 0.5*(w3(i, j2, k) - w3(i, j1, k))
                            tmpd3w1(i, j, k) = 0.5*(w1(i, j, k2) - w1(i, j, k1))
                            tmpd3w2(i, j, k) = 0.5*(w2(i, j, k2) - w2(i, j, k1))
                        end do
                    end do
                end do
                if (nonempty) then
                    call commute_array(u, 1)
                    call commute_array(tmpd1w2, 1)
                    call commute_array(tmpd1w3, 1)
                    call commute_array(tmpd2w1, 1)
                    call commute_array(tmpd2w3, 1)
                    call commute_array(tmpd3w1, 1)
                    call commute_array(tmpd3w2, 1)
                    call commute_array(s11, 1)
                    call commute_array(s12, 1)
                    call commute_array(s13, 1)
                    call commute_array(s22, 1)
                    call commute_array(s23, 1)
                    call commute_array(s33, 1)
                    call commute_array(s11t, 1)
                    call commute_array(s12t, 1)
                    call commute_array(s13t, 1)
                    call commute_array(s22t, 1)
                    call commute_array(s23t, 1)
                    call commute_array(s33t, 1)
                end if
                ! minimization of w
                do k = n3beg, n3end
                    do j = n2beg, n2end
                        do i = n1beg, n1end
                            if (i == 1) then
                                i1 = 0
                                ii1 = i
                            else
                                i1 = 1
                                ii1 = i - 1
                            end if
                            if (i == n1) then
                                i2 = 0
                                ii2 = i
                            else
                                i2 = 1
                                ii2 = i + 1
                            end if
                            if (j == 1) then
                                j1 = 0
                                jj1 = j
                            else
                                j1 = 1
                                jj1 = j - 1
                            end if
                            if (j == n2) then
                                j2 = 0
                                jj2 = j
                            else
                                j2 = 1
                                jj2 = j + 1
                            end if
                            if (k == 1) then
                                k1 = 0
                                kk1 = k
                            else
                                k1 = 1
                                kk1 = k - 1
                            end if
                            if (k == n3) then
                                k2 = 0
                                kk2 = k
                            else
                                k2 = 1
                                kk2 = k + 1
                            end if
                            ! w1
                            sumw1 = &
                                +lambda1*(i2*w1(ii2, j, k) + i1*w1(ii1, j, k)) &
                                + 0.5*lambda1*(j2*w1(i, jj2, k) + j1*w1(i, jj1, k)) &
                                + 0.5*lambda1*(k2*w1(i, j, kk2) + k1*w1(i, j, kk1)) &
                                - lambda0*(d1(i, j, k) - d1t(i, j, k) - (i2*u(ii2, j, k) - u(i, j, k))) &
                                + lambda1*(i1*s11(ii1, j, k) - i2*s11(i, j, k) - (i1*s11t(ii1, j, k) - i2*s11t(i, j, k))) &
                                + lambda1*(j1*s12(i, jj1, k) - j2*s12(i, j, k) - (j1*s12t(i, jj1, k) - j2*s12t(i, j, k)) - (j1*tmpd1w2(i, jj1, k) - j2*tmpd1w2(i, j, k))) &
                                + lambda1*(k1*s13(i, j, kk1) - k2*s13(i, j, k) - (k1*s13t(i, j, kk1) - k2*s13t(i, j, k)) - (k1*tmpd1w3(i, j, kk1) - k2*tmpd1w3(i, j, k)))
                            w1(i, j, k) = sumw1/(lambda0 + (2.0 - (1-i1) - (1-i2))*lambda1 + (2.0 - (1-j1) - (1-j2))*0.5*lambda1 + (2.0 - (1-k1) - (1-k2))*0.5*lambda1)
                            ! w2
                            sumw2 = &
                                +0.5*lambda1*(i2*w2(ii2, j, k) + i1*w2(ii1, j, k)) &
                                + lambda1*(j2*w2(i, jj2, k) + j1*w2(i, jj1, k)) &
                                + 0.5*lambda1*(k2*w2(i, j, kk2) + k1*w2(i, j, kk1)) &
                                - lambda0*(d2(i, j, k) - d2t(i, j, k) - (j2*u(i, jj2, k) - u(i, j, k))) &
                                + lambda1*(i1*s12(ii1, j, k) - i2*s12(i, j, k) - (i1*s12t(ii1, j, k) - i2*s12t(i, j, k)) - (i1*tmpd2w1(ii1, j, k) - i2*tmpd2w1(i, j, k))) &
                                + lambda1*(j1*s22(i, jj1, k) - j2*s22(i, j, k) - (j1*s22t(i, jj1, k) - j2*s22t(i, j, k))) &
                                + lambda1*(k1*s23(i, j, kk1) - k2*s23(i, j, k) - (k1*s23t(i, j, kk1) - k2*s23t(i, j, k)) - (k1*tmpd2w3(i, j, kk1) - k2*tmpd2w3(i, j, k)))
                            w2(i, j, k) = sumw2/(lambda0 + (2.0 - (1-i1) - (1-i2))*0.5*lambda1 + (2.0 - (1-j1) - (1-j2))*lambda1 + (2.0 - (1-k1) - (1-k2))*0.5*lambda1)
                            ! w3
                            sumw3 = &
                                +0.5*lambda1*(i2*w3(ii2, j, k) + i1*w3(ii1, j, k)) &
                                + 0.5*lambda1*(j2*w3(i, jj2, k) + j1*w3(i, jj1, k)) &
                                + lambda1*(k2*w3(i, j, kk2) + k1*w3(i, j, kk1)) &
                                - lambda0*(d3(i, j, k) - d3t(i, j, k) - (k2*u(i, j, kk2) - u(i, j, k))) &
                                + lambda1*(i1*s13(ii1, j, k) - i2*s13(i, j, k) - (i1*s13t(ii1, j, k) - i2*s13t(i, j, k)) - (i1*tmpd3w1(ii1, j, k) - i2*tmpd3w1(i, j, k))) &
                                + lambda1*(j1*s23(i, jj1, k) - j2*s23(i, j, k) - (j1*s23t(i, jj1, k) - j2*s23t(i, j, k)) - (j1*tmpd3w2(i, jj1, k) - j2*tmpd3w2(i, j, k))) &
                                + lambda1*(k1*s33(i, j, kk1) - k2*s33(i, j, k) - (k1*s33t(i, j, kk1) - k2*s33t(i, j, k)))
                            w3(i, j, k) = sumw3/(lambda0 + (2.0 - (1-i1) - (1-i2))*0.5*lambda1 + (2.0 - (1-j1) - (1-j2))*0.5*lambda1 + (2.0 - (1-k1) - (1-k2))*lambda1)
                        end do
                    end do
                end do
            end if
            ! update multipliers
            ! minimization d
            do k = n3beg, n3end
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        tmp = (u(i2, j, k) - u(i1, j, k)) - w1(i, j, k) + d1t(i, j, k)
                        d1(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmp = (u(i, j2, k) - u(i, j1, k)) - w2(i, j, k) + d2t(i, j, k)
                        d2(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                        if (k == n3) then
                            k1 = k - 1
                            k2 = k
                        else
                            k1 = k
                            k2 = k + 1
                        end if
                        tmp = (u(i, j, k2) - u(i, j, k1)) - w3(i, j, k) + d3t(i, j, k)
                        d3(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                    end do
                end do
            end do
            if (alpha1 /= 0) then
                if (nonempty) then
                    call commute_array(w1, 1)
                    call commute_array(w2, 1)
                    call commute_array(w3, 1)
                end if
                do k = n3beg, n3end
                    do j = n2beg, n2end
                        do i = n1beg, n1end
                            if (i == n1) then
                                i1 = i - 1
                                i2 = i
                            else
                                i1 = i
                                i2 = i + 1
                            end if
                            if (j == n2) then
                                j1 = j - 1
                                j2 = j
                            else
                                j1 = j
                                j2 = j + 1
                            end if
                            if (k == n3) then
                                k1 = k - 1
                                k2 = k
                            else
                                k1 = k
                                k2 = k + 1
                            end if
                            tmp = w1(i2, j, k) - w1(i1, j, k) + s11t(i, j, k)
                            s11(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = w2(i, j2, k) - w2(i, j1, k) + s22t(i, j, k)
                            s22(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = w3(i, j, k2) - w3(i, j, k1) + s33t(i, j, k)
                            s33(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w2(i2, j, k) - w2(i1, j, k) + w1(i, j2, k) - w1(i, j1, k)) + s12t(i, j, k)
                            s12(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w3(i2, j, k) - w3(i1, j, k) + w1(i, j, k2) - w1(i, j, k1)) + s13t(i, j, k)
                            s13(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w3(i, j2, k) - w3(i, j1, k) + w2(i, j, k2) - w2(i, j, k1)) + s23t(i, j, k)
                            s23(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        end do
                    end do
                end do
            end if
            call mpi_barrier(mpi_comm_world, mpi_ierr)
        end do
        ! d
        do k = n3beg, n3end
            do j = n2beg, n2end
                do i = n1beg, n1end
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    d1t(i, j, k) = d1t(i, j, k) + (u(i2, j, k) - u(i1, j, k) - w1(i, j, k) - d1(i, j, k))
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    d2t(i, j, k) = d2t(i, j, k) + (u(i, j2, k) - u(i, j1, k) - w2(i, j, k) - d2(i, j, k))
                    if (k == n3) then
                        k1 = k - 1
                        k2 = k
                    else
                        k1 = k
                        k2 = k + 1
                    end if
                    d3t(i, j, k) = d3t(i, j, k) + (u(i, j, k2) - u(i, j, k1) - w3(i, j, k) - d3(i, j, k))
                end do
            end do
        end do
        if (alpha1 /= 0) then
            do k = n3beg, n3end
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        if (k == n3) then
                            k1 = k - 1
                            k2 = k
                        else
                            k1 = k
                            k2 = k + 1
                        end if
                        s11t(i, j, k) = s11t(i, j, k) + (w1(i2, j, k) - w1(i1, j, k)) - s11(i, j, k)
                        s22t(i, j, k) = s22t(i, j, k) + (w2(i, j2, k) - w2(i, j1, k)) - s22(i, j, k)
                        s33t(i, j, k) = s33t(i, j, k) + (w3(i, j, k2) - w3(i, j, k1)) - s33(i, j, k)
                        s12t(i, j, k) = s12t(i, j, k) + 0.5*(w1(i, j2, k) - w1(i, j1, k) + w2(i2, j, k) - w2(i1, j, k)) - s12(i, j, k)
                        s13t(i, j, k) = s13t(i, j, k) + 0.5*(w1(i, j, k2) - w1(i, j, k1) + w3(i2, j, k) - w3(i1, j, k)) - s13(i, j, k)
                        s23t(i, j, k) = s23t(i, j, k) + 0.5*(w2(i, j, k2) - w2(i, j, k1) + w3(i, j2, k) - w3(i, j1, k)) - s23(i, j, k)
                    end do
                end do
            end do
        end if
        call mpi_barrier(mpi_comm_world, mpi_ierr)
        ! progress
        if (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1) then
            if (nonempty) then
                bsum = sum((u - pu)**2)
            else
                bsum = 0.0
            end if
            call allreduce(bsum)
            if (rankid == 0) then
                call warn(' >> TGpV iteration '//num2str(iter, '(i)') &
                    //' of '//num2str(niter, '(i)')//' relative l2-norm difference = '//num2str(sqrt(bsum), '(es)'))
            end if
        end if
    end do
    call mpi_barrier(mpi_comm_world, mpi_ierr)
    call alloc_array(u0, [1, n1, 1, n2, 1, n3])
    if (nonempty) then
        u0(n1beg:n1end, n2beg:n2end, n3beg:n3end) = u(n1beg:n1end, n2beg:n2end, n3beg:n3end)
    end if
    call allreduce_array(u0)
    call mpi_barrier(mpi_comm_world, mpi_ierr)
end function tgpv_filt_3d_mpi_float
!
!> 2D TV + sparsity denoising
!
function sparse_tv_filt_1d_float(f, mu, l_sparse, l_tv, niter, verbose) result(u)
    real, dimension(:) :: f
    real :: mu, l_sparse, l_tv
    integer :: niter
    logical, optional :: verbose
    real, allocatable, dimension(:) :: u
    integer :: i, iter
    real :: tmp, tmpx, sk, sumu
    real, allocatable, dimension(:) :: d1, d1t, s, st, pu
    integer :: n1
    real :: alpha0, alpha1, p
    integer :: inner, ninner
    integer :: i1, i2
    integer :: ii1, ii2
    real :: lambda0, lambda1
    logical :: tv_verbose
    ! read parameter
    n1 = size(f)
    alpha0 = l_tv
    alpha1 = l_sparse
    call assert(alpha0 <= 1, ' <sparse_tv_filt_2d_> Error: l_tv must >= 0 and <= 1')
    call assert(alpha1 <= 1, ' <sparse_tv_filt_2d_> Error: l_sparse must >= 0 and <= 1')
    p = 1.0
    ninner = 2
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1)
    d1t = zeros(n1)
    s = zeros(n1)
    st = zeros(n1)
    u = zeros(n1)
    pu = zeros(n1)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda0 = 2*mu*alpha0
    lambda1 = 2*mu*alpha1
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! minimization u
            do i = 1, n1
                if (i == 1) then
                    i1 = 0
                    ii1 = i
                else
                    i1 = 1
                    ii1 = i - 1
                end if
                if (i == n1) then
                    i2 = 0
                    ii2 = i
                else
                    i2 = 1
                    ii2 = i + 1
                end if
                sumu = lambda0*( &
                    +i2*u(ii2) + i1*u(ii1)) &
                    + lambda1*( &
                    +s(i) - st(i)) &
                    + lambda0*( &
                    +(i1*d1(ii1) - i2*d1(i)) &
                    - (i1*d1t(ii1) - i2*d1t(i)) &
                    ) &
                    + mu*f(i)
                u(i) = sumu/(mu + lambda1 + (2.0 - (1 - i1) - (1 - i2))*lambda0)
            end do
            ! update multipliers
            !$omp parallel do private(i, i1, i2, tmpx, sk)
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                tmpx = (u(i2) - u(i1)) + d1t(i)
                sk = abs(tmpx)
                d1(i) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpx/(sk + float_tiny)
            end do
            !$omp end parallel do
            !$omp parallel do private(i, tmp, sk)
            do i = 1, n1
                tmp = u(i) + st(i)
                sk = abs(tmp)
                s(i) = max(sk - 1.0/(lambda1/alpha1), 0.0)*tmp/(sk + float_tiny)
            end do
            !$omp end parallel do
        end do
        ! update split-Bregman variables
        !$omp parallel do private(i, i1, i2)
        do i = 1, n1
            if (i == n1) then
                i1 = i - 1
                i2 = i
            else
                i1 = i
                i2 = i + 1
            end if
            d1t(i) = d1t(i) + (u(i2) - u(i1) - d1(i))
        end do
        !$omp end parallel do
        !$omp parallel do private(i)
        do i = 1, n1
            st(i) = st(i) + u(i) - s(i)
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Sparse TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function sparse_tv_filt_1d_float
!
!> 2D TV + sparsity denoising
!
function sparse_tv_filt_2d_float(f, mu, l_sparse, l_tv, niter, verbose) result(u)
    real, dimension(:, :) :: f
    real :: mu, l_sparse, l_tv
    integer :: niter
    logical, optional :: verbose
    real, allocatable, dimension(:, :) :: u
    integer :: i, j, iter
    real :: tmp, tmpx, tmpz, sk, sumu
    real, allocatable, dimension(:, :) :: d1, d2, d1t, d2t, s, st, pu
    integer :: n1, n2
    real :: alpha0, alpha1, p
    integer :: inner, ninner
    integer :: i1, i2, j1, j2
    integer :: ii1, ii2, jj1, jj2
    real :: lambda0, lambda1
    logical :: tv_verbose
    ! read parameter
    n1 = size(f, 1)
    n2 = size(f, 2)
    alpha0 = l_tv
    alpha1 = l_sparse
    call assert(alpha0 <= 1, ' <sparse_tv_filt_2d_> Error: l_tv must >= 0 and <= 1')
    call assert(alpha1 <= 1, ' <sparse_tv_filt_2d_> Error: l_sparse must >= 0 and <= 1')
    p = 1.0
    ninner = 2
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1, n2)
    d2 = zeros(n1, n2)
    d1t = zeros(n1, n2)
    d2t = zeros(n1, n2)
    s = zeros(n1, n2)
    st = zeros(n1, n2)
    u = zeros(n1, n2)
    pu = zeros(n1, n2)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda0 = 2*mu*alpha0
    lambda1 = 2*mu*alpha1
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! minimization u
            do j = 1, n2
                do i = 1, n1
                    if (i == 1) then
                        i1 = 0
                        ii1 = i
                    else
                        i1 = 1
                        ii1 = i - 1
                    end if
                    if (i == n1) then
                        i2 = 0
                        ii2 = i
                    else
                        i2 = 1
                        ii2 = i + 1
                    end if
                    if (j == 1) then
                        j1 = 0
                        jj1 = j
                    else
                        j1 = 1
                        jj1 = j - 1
                    end if
                    if (j == n2) then
                        j2 = 0
                        jj2 = j
                    else
                        j2 = 1
                        jj2 = j + 1
                    end if
                    sumu = lambda0*( &
                        +i2*u(ii2, j) + i1*u(ii1, j) &
                        + j2*u(i, jj2) + j1*u(i, jj1)) &
                        + lambda1*( &
                        +s(i, j) - st(i, j)) &
                        + lambda0*( &
                        +(i1*d1(ii1, j) - i2*d1(i, j)) &
                        + (j1*d2(i, jj1) - j2*d2(i, j)) &
                        - (i1*d1t(ii1, j) - i2*d1t(i, j)) &
                        - (j1*d2t(i, jj1) - j2*d2t(i, j)) &
                        ) &
                        + mu*f(i, j)
                    u(i, j) = sumu/(mu + lambda1 + (4.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2))*lambda0)
                end do
            end do
            ! update multipliers
            !$omp parallel do private(i, j, i1, i2, j1, j2, tmpx, tmpz, sk)
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    tmpx = (u(i2, j) - u(i1, j)) + d1t(i, j)
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    tmpz = (u(i, j2) - u(i, j1)) + d2t(i, j)
                    sk = sqrt(tmpx**2 + tmpz**2)
                    d1(i, j) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpx/(sk + float_tiny)
                    d2(i, j) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpz/(sk + float_tiny)
                end do
            end do
            !$omp end parallel do
            !$omp parallel do private(i, j, tmp, sk)
            do j = 1, n2
                do i = 1, n1
                    tmp = u(i, j) + st(i, j)
                    sk = abs(tmp)
                    s(i, j) = max(sk - 1.0/(lambda1/alpha1), 0.0)*tmp/(sk + float_tiny)
                end do
            end do
            !$omp end parallel do
        end do
        ! update split-Bregman variables
        !$omp parallel do private(i, j, i1, i2, j1, j2)
        do j = 1, n2
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                d1t(i, j) = d1t(i, j) + (u(i2, j) - u(i1, j) - d1(i, j))
                if (j == n2) then
                    j1 = j - 1
                    j2 = j
                else
                    j1 = j
                    j2 = j + 1
                end if
                d2t(i, j) = d2t(i, j) + (u(i, j2) - u(i, j1) - d2(i, j))
            end do
        end do
        !$omp end parallel do
        !$omp parallel do private(i, j)
        do j = 1, n2
            do i = 1, n1
                st(i, j) = st(i, j) + u(i, j) - s(i, j)
            end do
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Sparse TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function sparse_tv_filt_2d_float
!
!> 3D TV + sparsity denoising
!
function sparse_tv_filt_3d_float(f, mu, l_sparse, l_tv, niter, verbose) result(u)
    real, dimension(:, :, :) :: f
    real :: mu, l_sparse, l_tv
    integer :: niter
    logical, optional :: verbose
    real, allocatable, dimension(:, :, :) :: u
    integer :: i, j, k, iter
    real :: tmp, tmpx, tmpy, tmpz, sk, sumu
    real, allocatable, dimension(:, :, :) :: d1, d2, d3, d1t, d2t, d3t, s, st, pu
    integer :: n1, n2, n3
    real :: alpha0, alpha1, p
    integer :: inner, ninner
    integer :: i1, i2, j1, j2, k1, k2
    integer :: ii1, ii2, jj1, jj2, kk1, kk2
    real :: lambda0, lambda1
    logical :: tv_verbose
    ! read parameter
    n1 = size(f, 1)
    n2 = size(f, 2)
    n3 = size(f, 3)
    alpha0 = l_tv
    alpha1 = l_sparse
    call assert(alpha0 <= 1, ' <sparse_tv_filt_3d_> Error: l_tv must >= 0 and <= 1')
    call assert(alpha1 <= 1, ' <sparse_tv_filt_3d_> Error: l_sparse must >= 0 and <= 1')
    p = 1.0
    ninner = 2
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1, n2, n3)
    d2 = zeros(n1, n2, n3)
    d3 = zeros(n1, n2, n3)
    d1t = zeros(n1, n2, n3)
    d2t = zeros(n1, n2, n3)
    d3t = zeros(n1, n2, n3)
    s = zeros(n1, n2, n3)
    st = zeros(n1, n2, n3)
    u = zeros(n1, n2, n3)
    pu = zeros(n1, n2, n3)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda0 = 2*mu*alpha0
    lambda1 = 2*mu*alpha1
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! minimization u
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        if (i == 1) then
                            i1 = 0
                            ii1 = i
                        else
                            i1 = 1
                            ii1 = i - 1
                        end if
                        if (i == n1) then
                            i2 = 0
                            ii2 = i
                        else
                            i2 = 1
                            ii2 = i + 1
                        end if
                        if (j == 1) then
                            j1 = 0
                            jj1 = j
                        else
                            j1 = 1
                            jj1 = j - 1
                        end if
                        if (j == n2) then
                            j2 = 0
                            jj2 = j
                        else
                            j2 = 1
                            jj2 = j + 1
                        end if
                        if (k == 1) then
                            k1 = 0
                            kk1 = k
                        else
                            k1 = 1
                            kk1 = k - 1
                        end if
                        if (k == n3) then
                            k2 = 0
                            kk2 = k
                        else
                            k2 = 1
                            kk2 = k + 1
                        end if
                        sumu = lambda0*( &
                            +i2*u(ii2, j, k) + i1*u(ii1, j, k) &
                            + j2*u(i, jj2, k) + j1*u(i, jj1, k) &
                            + k2*u(i, j, kk2) + k1*u(i, j, kk1)) &
                            + lambda1*( &
                            +s(i, j, k) - st(i, j, k)) &
                            + lambda0*( &
                            +(i1*d1(ii1, j, k) - i2*d1(i, j, k)) &
                            + (j1*d2(i, jj1, k) - j2*d2(i, j, k)) &
                            + (k1*d3(i, j, kk1) - k2*d3(i, j, k)) &
                            - (i1*d1t(ii1, j, k) - i2*d1t(i, j, k)) &
                            - (j1*d2t(i, jj1, k) - j2*d2t(i, j, k)) &
                            - (k1*d3t(i, j, kk1) - k2*d3t(i, j, k))) &
                            + mu*f(i, j, k)
                        u(i, j, k) = sumu/(mu + lambda1 + (6.0 - &
                            (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2) - (1 - k1) - (1 - k2))*lambda0)
                    end do
                end do
            end do
            ! update multipliers
            !$omp parallel do private(i, j, k, i1, i2, j1, j2, k1, k2, tmpx, tmpy, tmpz, sk)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        tmpx = (u(i2, j, k) - u(i1, j, k)) + d1t(i, j, k)
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmpy = (u(i, j2, k) - u(i, j1, k)) + d2t(i, j, k)
                        if (k == n3) then
                            k1 = k - 1
                            k2 = k
                        else
                            k1 = k
                            k2 = k + 1
                        end if
                        tmpz = (u(i, j, k2) - u(i, j, k1)) + d3t(i, j, k)
                        sk = sqrt(tmpx**2 + tmpy**2 + tmpz**2)
                        d1(i, j, k) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpx/(sk + float_tiny)
                        d2(i, j, k) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpy/(sk + float_tiny)
                        d3(i, j, k) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpz/(sk + float_tiny)
                    end do
                end do
            end do
            !$omp end parallel do
            !$omp parallel do private(i, j, k, tmp, sk)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        tmp = u(i, j, k) + st(i, j, k)
                        sk = abs(tmp)
                        s(i, j, k) = max(sk - 1.0/(lambda1/alpha1), 0.0)*tmp/(sk + float_tiny)
                    end do
                end do
            end do
            !$omp end parallel do
        end do
        ! update split-Bregman variables
        !$omp parallel do private(i, j, k, i1, i2, j1, j2, k1, k2)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    d1t(i, j, k) = d1t(i, j, k) + (u(i2, j, k) - u(i1, j, k) - d1(i, j, k))
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    d2t(i, j, k) = d2t(i, j, k) + (u(i, j2, k) - u(i, j1, k) - d2(i, j, k))
                    if (k == n3) then
                        k1 = k - 1
                        k2 = k
                    else
                        k1 = k
                        k2 = k + 1
                    end if
                    d3t(i, j, k) = d3t(i, j, k) + (u(i, j, k2) - u(i, j, k1) - d3(i, j, k))
                end do
            end do
        end do
        !$omp end parallel do
        !$omp parallel do private(i, j, k)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    st(i, j, k) = st(i, j, k) + u(i, j, k) - s(i, j, k)
                end do
            end do
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Sparse TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function sparse_tv_filt_3d_float
!
!> 1D soft thresholding
!
function soft_shrinkage_1d_float(x, b) result(y)
    real, dimension(:), intent(in) :: x
    real, intent(in) :: b
    real, allocatable, dimension(:) :: y
    y = x*0
    where (abs(x) >= b)
        y = x*(1.0 - b/abs(x))
    end where
end function soft_shrinkage_1d_float
!
!> 2D soft thresholding
!
function soft_shrinkage_2d_float(x, b) result(y)
    real, dimension(:, :), intent(in) :: x
    real, intent(in) :: b
    real, allocatable, dimension(:, :) :: y
    y = x*0
    where (abs(x) >= b)
        y = x*(1.0 - b/abs(x))
    end where
end function soft_shrinkage_2d_float
!
!> 3D soft thresholding
!
function soft_shrinkage_3d_float(x, b) result(y)
    real, dimension(:, :, :), intent(in) :: x
    real, intent(in) :: b
    real, allocatable, dimension(:, :, :) :: y
    y = x*0
    where (abs(x) >= b)
        y = x*(1.0 - b/abs(x))
    end where
end function soft_shrinkage_3d_float
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
!> 1D isotropic TV denoising
!
function tv_iso_filt_1d_double(f, mu, niter, verbose) result(u)
    double precision, dimension(:), intent(in) :: f
    double precision, intent(in) :: mu
    integer, intent(in) :: niter
    logical, optional :: verbose
    double precision, allocatable, dimension(:) :: u
    integer :: i, iter
    double precision :: tmpx, sumu
    double precision, allocatable, dimension(:) :: d1, d1t
    double precision, allocatable, dimension(:) :: pu
    integer :: n1
    integer :: i1, i2
    integer :: ii1, ii2
    double precision :: lambda, sk
    logical :: tv_verbose
    n1 = size(f)
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1)
    d1t = zeros(n1)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda = 2.0*mu
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        ! minimization u
        do i = 1, n1
            if (i == 1) then
                i1 = 0
                ii1 = i
            else
                i1 = 1
                ii1 = i - 1
            end if
            if (i == n1) then
                i2 = 0
                ii2 = i
            else
                i2 = 1
                ii2 = i + 1
            end if
            sumu = lambda*( &
                i2*u(ii2) + i1*u(ii1)) &
                + lambda*( &
                (i1*d1(ii1) - i2*d1(i)) &
                - (i1*d1t(ii1) - i2*d1t(i)) &
                ) &
                + mu*f(i)
            u(i) = sumu/(mu + (2.0 - (1 - i1) - (1 - i2))*lambda)
        end do
        ! update multiplietgpv_filt_2d_rs
        ! minimization d
        !$omp parallel do private(i, i1, i2, tmpx, sk)
        do i = 1, n1
            if (i == n1) then
                i1 = i - 1
                i2 = i
            else
                i1 = i
                i2 = i + 1
            end if
            tmpx = (u(i2) - u(i1)) + d1t(i)
            sk = abs(tmpx)
            d1(i) = max(sk - 1.0/lambda, 0.0)*tmpx/(sk + float_tiny)
        end do
        !$omp end parallel do
        ! d
        !$omp parallel do private(i, i1, i2)
        do i = 1, n1
            if (i == n1) then
                i1 = i - 1
                i2 = i
            else
                i1 = i
                i2 = i + 1
            end if
            d1t(i) = d1t(i) + (u(i2) - u(i1) - d1(i))
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Isotropic TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function tv_iso_filt_1d_double
!
!> 2D isotropic TV denoising
!
function tv_iso_filt_2d_double(f, mu, niter, verbose) result(u)
    double precision, dimension(:, :), intent(in) :: f
    double precision, intent(in) :: mu
    integer, intent(in) :: niter
    logical, optional :: verbose
    double precision, allocatable, dimension(:, :) :: u
    integer :: i, j, iter
    double precision :: tmpx, tmpz, sumu
    double precision, allocatable, dimension(:, :) :: d1, d2, d1t, d2t
    double precision, allocatable, dimension(:, :) :: pu
    integer :: n1, n2
    integer :: i1, i2, j1, j2
    integer :: ii1, ii2, jj1, jj2
    double precision :: lambda, sk
    logical :: tv_verbose
    n1 = size(f, 1)
    n2 = size(f, 2)
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1, n2)
    d2 = zeros(n1, n2)
    d1t = zeros(n1, n2)
    d2t = zeros(n1, n2)
    ! read noisy image
    u = f
    pu = zeros_like(u)
    ! lambdas for u and w subproblems
    lambda = 2.0*mu
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        ! minimization u
        do j = 1, n2
            do i = 1, n1
                if (i == 1) then
                    i1 = 0
                    ii1 = i
                else
                    i1 = 1
                    ii1 = i - 1
                end if
                if (i == n1) then
                    i2 = 0
                    ii2 = i
                else
                    i2 = 1
                    ii2 = i + 1
                end if
                if (j == 1) then
                    j1 = 0
                    jj1 = j
                else
                    j1 = 1
                    jj1 = j - 1
                end if
                if (j == n2) then
                    j2 = 0
                    jj2 = j
                else
                    j2 = 1
                    jj2 = j + 1
                end if
                sumu = lambda*( &
                    i2*u(ii2, j) + i1*u(ii1, j) &
                    + j2*u(i, jj2) + j1*u(i, jj1)) &
                    + lambda*( &
                    (i1*d1(ii1, j) - i2*d1(i, j)) &
                    + (j1*d2(i, jj1) - j2*d2(i, j)) &
                    - (i1*d1t(ii1, j) - i2*d1t(i, j)) &
                    - (j1*d2t(i, jj1) - j2*d2t(i, j)) &
                    ) &
                    + mu*f(i, j)
                u(i, j) = sumu/(mu + (4.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2))*lambda)
            end do
        end do
        ! update multiplietgpv_filt_2d_rs
        ! minimization d
        !$omp parallel do private(i, j, i1, i2, j1, j2, tmpx, tmpz, sk)
        do j = 1, n2
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                tmpx = (u(i2, j) - u(i1, j)) + d1t(i, j)
                if (j == n2) then
                    j1 = j - 1
                    j2 = j
                else
                    j1 = j
                    j2 = j + 1
                end if
                tmpz = (u(i, j2) - u(i, j1)) + d2t(i, j)
                sk = sqrt(tmpx**2 + tmpz**2)
                d1(i, j) = max(sk - 1.0/lambda, 0.0)*tmpx/(sk + float_tiny)
                d2(i, j) = max(sk - 1.0/lambda, 0.0)*tmpz/(sk + float_tiny)
            end do
        end do
        !$omp end parallel do
        ! d
        !$omp parallel do private(i, j, i1, i2, j1, j2)
        do j = 1, n2
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                d1t(i, j) = d1t(i, j) + (u(i2, j) - u(i1, j) - d1(i, j))
                if (j == n2) then
                    j1 = j - 1
                    j2 = j
                else
                    j1 = j
                    j2 = j + 1
                end if
                d2t(i, j) = d2t(i, j) + (u(i, j2) - u(i, j1) - d2(i, j))
            end do
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Isotropic TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function tv_iso_filt_2d_double
!
!> 3D isotropic TV denoising
!
function tv_iso_filt_3d_double(f, mu, niter, verbose) result(u)
    double precision, dimension(:, :, :), intent(in) :: f
    double precision, intent(in) :: mu
    integer, intent(in) :: niter
    logical, optional :: verbose
    double precision, allocatable, dimension(:, :, :) :: u
    integer :: i, j, k, iter
    double precision :: tmpx, tmpy, tmpz, sumu
    double precision, allocatable, dimension(:, :, :) :: d1, d2, d3, d1t, d2t, d3t
    double precision, allocatable, dimension(:, :, :) :: pu
    integer :: n1, n2, n3
    integer :: i1, i2, j1, j2, k1, k2
    integer :: ii1, ii2, jj1, jj2, kk1, kk2
    double precision :: lambda, sk
    logical :: tv_verbose
    n1 = size(f, 1)
    n2 = size(f, 2)
    n3 = size(f, 3)
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1, n2, n3)
    d2 = zeros(n1, n2, n3)
    d3 = zeros(n1, n2, n3)
    d1t = zeros(n1, n2, n3)
    d2t = zeros(n1, n2, n3)
    d3t = zeros(n1, n2, n3)
    ! read noisy image
    u = f
    pu = zeros_like(u)
    ! lambdas for u and w subproblems
    lambda = 2.0*mu
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        ! minimization u
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    if (i == 1) then
                        i1 = 0
                        ii1 = i
                    else
                        i1 = 1
                        ii1 = i - 1
                    end if
                    if (i == n1) then
                        i2 = 0
                        ii2 = i
                    else
                        i2 = 1
                        ii2 = i + 1
                    end if
                    if (j == 1) then
                        j1 = 0
                        jj1 = j
                    else
                        j1 = 1
                        jj1 = j - 1
                    end if
                    if (j == n2) then
                        j2 = 0
                        jj2 = j
                    else
                        j2 = 1
                        jj2 = j + 1
                    end if
                    if (k == 1) then
                        k1 = 0
                        kk1 = k
                    else
                        k1 = 1
                        kk1 = k - 1
                    end if
                    if (k == n3) then
                        k2 = 0
                        kk2 = k
                    else
                        k2 = 1
                        kk2 = k + 1
                    end if
                    sumu = lambda*( &
                        +i2*u(ii2, j, k) + i1*u(ii1, j, k) &
                        + j2*u(i, jj2, k) + j1*u(i, jj1, k) &
                        + k2*u(i, j, kk2) + k1*u(i, j, kk1)) &
                        + lambda*( &
                        +(i1*d1(ii1, j, k) - i2*d1(i, j, k)) &
                        + (j1*d2(i, jj1, k) - j2*d2(i, j, k)) &
                        + (k1*d3(i, j, kk1) - k2*d3(i, j, k)) &
                        - (i1*d1t(ii1, j, k) - i2*d1t(i, j, k)) &
                        - (j1*d2t(i, jj1, k) - j2*d2t(i, j, k)) &
                        - (k1*d3t(i, j, kk1) - k2*d3t(i, j, k))) &
                        + mu*f(i, j, k)
                    u(i, j, k) = sumu/(mu + (6.0 - &
                        (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2) - (1 - k1) - (1 - k2))*lambda)
                end do
            end do
        end do
        ! update multiplietgpv_filt_2d_rs
        ! minimization d
        !$omp parallel do private(i, j, k, i1, i2, j1, j2, k1, k2, tmpx, tmpy, tmpz, sk)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    tmpx = (u(i2, j, k) - u(i1, j, k)) + d1t(i, j, k)
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    tmpy = (u(i, j2, k) - u(i, j1, k)) + d2t(i, j, k)
                    if (k == n3) then
                        k1 = k - 1
                        k2 = k
                    else
                        k1 = k
                        k2 = k + 1
                    end if
                    tmpz = (u(i, j, k2) - u(i, j, k1)) + d3t(i, j, k)
                    sk = sqrt(tmpx**2 + tmpy**2 + tmpz**2)
                    d1(i, j, k) = max(sk - 1.0/lambda, 0.0)*tmpx/(sk + float_tiny)
                    d2(i, j, k) = max(sk - 1.0/lambda, 0.0)*tmpy/(sk + float_tiny)
                    d3(i, j, k) = max(sk - 1.0/lambda, 0.0)*tmpz/(sk + float_tiny)
                end do
            end do
        end do
        !$omp end parallel do
        ! d
        !$omp parallel do private(i, j, k, i1, i2, j1, j2, k1, k2)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    d1t(i, j, k) = d1t(i, j, k) + (u(i2, j, k) - u(i1, j, k) - d1(i, j, k))
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    d2t(i, j, k) = d2t(i, j, k) + (u(i, j2, k) - u(i, j1, k) - d2(i, j, k))
                    if (k == n3) then
                        k1 = k - 1
                        k2 = k
                    else
                        k1 = k
                        k2 = k + 1
                    end if
                    d3t(i, j, k) = d3t(i, j, k) + (u(i, j, k2) - u(i, j, k1) - d3(i, j, k))
                end do
            end do
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Isotropic TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function tv_iso_filt_3d_double
!
!> 1D TGpV denoising
!
function tgpv_filt_1d_double(f, mu, alpha0, alpha1, niter, p, verbose) result(u)
    double precision, dimension(:), intent(in) :: f
    double precision, intent(in) :: mu, alpha0, alpha1, p
    integer, intent(in) :: niter
    logical, optional :: verbose
    double precision, allocatable, dimension(:) :: u
    integer :: i, iter
    double precision :: tmp, sumu, sumw1
    double precision, allocatable, dimension(:) :: d1, d1t
    double precision, allocatable, dimension(:) :: s11
    double precision, allocatable, dimension(:) :: s11t
    double precision, allocatable, dimension(:) :: w1
    double precision, allocatable, dimension(:) :: pu
    integer :: n1
    integer :: inner, ninner
    integer :: i1, i2
    integer :: ii1, ii2
    double precision :: lambda0, lambda1
    logical :: tv_verbose
    n1 = size(f)
    ninner = 2
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1)
    d1t = zeros(n1)
    s11 = zeros(n1)
    s11t = zeros(n1)
    w1 = zeros(n1)
    pu = zeros(n1)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda0 = 2.0*mu
    lambda1 = 2.0*mu*(alpha1/alpha0)
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! minimization u
            do i = 1, n1
                if (i == 1) then
                    i1 = 0
                    ii1 = i
                else
                    i1 = 1
                    ii1 = i - 1
                end if
                if (i == n1) then
                    i2 = 0
                    ii2 = i
                else
                    i2 = 1
                    ii2 = i + 1
                end if
                sumu = lambda0*( &
                    +i2*u(ii2) + i1*u(ii1)) &
                    + lambda0*( &
                    +(i1*d1(ii1) - i2*d1(i)) &
                    - (i1*d1t(ii1) - i2*d1t(i)) &
                    + (i1*w1(ii1) - i2*w1(i)) &
                    ) &
                    + mu*f(i)
                u(i) = sumu/(mu + (2.0 - (1 - i1) - (1 - i2))*lambda0)
            end do
            if (alpha1 /= 0) then
                ! minimization of w
                do i = 1, n1
                    if (i == 1) then
                        i1 = 0
                        ii1 = i
                    else
                        i1 = 1
                        ii1 = i - 1
                    end if
                    if (i == n1) then
                        i2 = 0
                        ii2 = i
                    else
                        i2 = 1
                        ii2 = i + 1
                    end if
                    ! w1
                    sumw1 = &
                        +lambda1*(i2*w1(ii2) + i1*w1(ii1)) &
                        - lambda0*(d1(i) - d1t(i) - (i2*u(ii2) - u(i))) &
                        + lambda1*(i1*s11(ii1) - i2*s11(i) - (i1*s11t(ii1) - i2*s11t(i)))
                    w1(i) = sumw1/(lambda0 + (2.0 - (1 - i1) - (1 - i2))*lambda1)
                end do
            end if
            ! update multipliers
            ! minimization d
            !$omp parallel do private(i, i1, i2, tmp)
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                tmp = (u(i2) - u(i1)) - w1(i) + d1t(i)
                d1(i) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
            end do
            !$omp end parallel do
            if (alpha1 /= 0) then
                !$omp parallel do private(i, i1, i2, tmp)
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    tmp = w1(i2) - w1(i1) + s11t(i)
                    s11(i) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                end do
                !$omp end parallel do
            end if
        end do
        ! d
        !$omp parallel do private(i, i1, i2)
        do i = 1, n1
            if (i == n1) then
                i1 = i - 1
                i2 = i
            else
                i1 = i
                i2 = i + 1
            end if
            d1t(i) = d1t(i) + (u(i2) - u(i1) - w1(i) - d1(i))
        end do
        !$omp end parallel do
        if (alpha1 /= 0) then
            !$omp parallel do private(i, i1, i2)
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                s11t(i) = s11t(i) + (w1(i2) - w1(i1)) - s11(i)
            end do
            !$omp end parallel do
        end if
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> TGpV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function tgpv_filt_1d_double
!
!> 2D TGpV denoising
!
function tgpv_filt_2d_double(f, mu, alpha0, alpha1, niter, p, verbose) result(u)
    double precision, dimension(:, :), intent(in) :: f
    double precision, intent(in) :: mu, alpha0, alpha1, p
    integer, intent(in) :: niter
    logical, optional :: verbose
    double precision, allocatable, dimension(:, :) :: u
    integer :: i, j, iter
    double precision :: tmp, sumu, sumw1, sumw2
    double precision, allocatable, dimension(:, :) :: d1, d2, d1t, d2t
    double precision, allocatable, dimension(:, :) :: s11, s12, s22
    double precision, allocatable, dimension(:, :) :: s11t, s12t, s22t
    double precision, allocatable, dimension(:, :) :: w1, w2
    double precision, allocatable, dimension(:, :) :: tmpd1w2
    double precision, allocatable, dimension(:, :) :: tmpd2w1
    double precision, allocatable, dimension(:, :) :: pu
    integer :: n1, n2
    integer :: inner, ninner
    integer :: i1, i2, j1, j2
    integer :: ii1, ii2, jj1, jj2
    double precision :: lambda0, lambda1
    logical :: tv_verbose
    n1 = size(f, 1)
    n2 = size(f, 2)
    ninner = 2
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1, n2)
    d2 = zeros(n1, n2)
    d1t = zeros(n1, n2)
    d2t = zeros(n1, n2)
    s11 = zeros(n1, n2)
    s12 = zeros(n1, n2)
    s22 = zeros(n1, n2)
    s11t = zeros(n1, n2)
    s12t = zeros(n1, n2)
    s22t = zeros(n1, n2)
    w1 = zeros(n1, n2)
    w2 = zeros(n1, n2)
    tmpd1w2 = zeros(n1, n2)
    tmpd2w1 = zeros(n1, n2)
    pu = zeros(n1, n2)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda0 = 2.0*mu
    lambda1 = 2.0*mu*(alpha1/alpha0)
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! minimization u
            do j = 1, n2
                do i = 1, n1
                    if (i == 1) then
                        i1 = 0
                        ii1 = i
                    else
                        i1 = 1
                        ii1 = i - 1
                    end if
                    if (i == n1) then
                        i2 = 0
                        ii2 = i
                    else
                        i2 = 1
                        ii2 = i + 1
                    end if
                    if (j == 1) then
                        j1 = 0
                        jj1 = j
                    else
                        j1 = 1
                        jj1 = j - 1
                    end if
                    if (j == n2) then
                        j2 = 0
                        jj2 = j
                    else
                        j2 = 1
                        jj2 = j + 1
                    end if
                    sumu = lambda0*( &
                        +i2*u(ii2, j) + i1*u(ii1, j) &
                        + j2*u(i, jj2) + j1*u(i, jj1)) &
                        + lambda0*( &
                        +(i1*d1(ii1, j) - i2*d1(i, j)) &
                        + (j1*d2(i, jj1) - j2*d2(i, j)) &
                        - (i1*d1t(ii1, j) - i2*d1t(i, j)) &
                        - (j1*d2t(i, jj1) - j2*d2t(i, j)) &
                        + (i1*w1(ii1, j) - i2*w1(i, j)) &
                        + (j1*w2(i, jj1) - j2*w2(i, j)) &
                        ) &
                        + mu*f(i, j)
                    u(i, j) = sumu/(mu + (4.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2))*lambda0)
                end do
            end do
            if (alpha1 /= 0) then
                ! minimization w
                !$omp parallel do private(i, j, i1, i2, j1, j2)
                do j = 1, n2
                    do i = 1, n1
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmpd1w2(i, j) = 0.5*(w2(i2, j) - w2(i1, j))
                        tmpd2w1(i, j) = 0.5*(w1(i, j2) - w1(i, j1))
                    end do
                end do
                !$omp end parallel do
                ! minimization of w
                do j = 1, n2
                    do i = 1, n1
                        if (i == 1) then
                            i1 = 0
                            ii1 = i
                        else
                            i1 = 1
                            ii1 = i - 1
                        end if
                        if (i == n1) then
                            i2 = 0
                            ii2 = i
                        else
                            i2 = 1
                            ii2 = i + 1
                        end if
                        if (j == 1) then
                            j1 = 0
                            jj1 = j
                        else
                            j1 = 1
                            jj1 = j - 1
                        end if
                        if (j == n2) then
                            j2 = 0
                            jj2 = j
                        else
                            j2 = 1
                            jj2 = j + 1
                        end if
                        ! w1
                        sumw1 = &
                            +lambda1*(i2*w1(ii2, j) + i1*w1(ii1, j)) &
                            + 0.5*lambda1*(j2*w1(i, jj2) + j1*w1(i, jj1)) &
                            - lambda0*(d1(i, j) - d1t(i, j) - (i2*u(ii2, j) - u(i, j))) &
                            + lambda1*(i1*s11(ii1, j) - i2*s11(i, j) - (i1*s11t(ii1, j) - i2*s11t(i, j))) &
                            + lambda1*(j1*s12(i, jj1) - j2*s12(i, j) - (j1*s12t(i, jj1) - j2*s12t(i, j)) - (j1*tmpd1w2(i, jj1) - j2*tmpd1w2(i, j)))
                        w1(i, j) = sumw1/(lambda0 + (2.0 - (1 - i1) - (1 - i2))*lambda1 + (2.0 - (1 - j1) - (1 - j2))*0.5*lambda1)
                        ! w2
                        sumw2 = &
                            +0.5*lambda1*(i2*w2(ii2, j) + i1*w2(ii1, j)) &
                            + lambda1*(j2*w2(i, jj2) + j1*w2(i, jj1)) &
                            - lambda0*(d2(i, j) - d2t(i, j) - (j2*u(i, jj2) - u(i, j))) &
                            + lambda1*(i1*s12(ii1, j) - i2*s12(i, j) - (i1*s12t(ii1, j) - i2*s12t(i, j)) - (i1*tmpd2w1(ii1, j) - i2*tmpd2w1(i, j))) &
                            + lambda1*(j1*s22(i, jj1) - j2*s22(i, j) - (j1*s22t(i, jj1) - j2*s22t(i, j)))
                        w2(i, j) = sumw2/(lambda0 + (2.0 - (1 - i1) - (1 - i2))*0.5*lambda1 + (2.0 - (1 - j1) - (1 - j2))*lambda1)
                    end do
                end do
            end if
            ! update multipliers
            ! minimization d
            !$omp parallel do private(i, j, i1, i2, j1, j2, tmp)
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    tmp = (u(i2, j) - u(i1, j)) - w1(i, j) + d1t(i, j)
                    d1(i, j) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    tmp = (u(i, j2) - u(i, j1)) - w2(i, j) + d2t(i, j)
                    d2(i, j) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                end do
            end do
            !$omp end parallel do
            if (alpha1 /= 0) then
                !$omp parallel do private(i, j, i1, i2, j1, j2, tmp)
                do j = 1, n2
                    do i = 1, n1
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmp = w1(i2, j) - w1(i1, j) + s11t(i, j)
                        s11(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        tmp = w2(i, j2) - w2(i, j1) + s22t(i, j)
                        s22(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        tmp = 0.5*(w2(i2, j) - w2(i1, j) + w1(i, j2) - w1(i, j1)) + s12t(i, j)
                        s12(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                    end do
                end do
                !$omp end parallel do
            end if
        end do
        ! d
        !$omp parallel do private(i, j, i1, i2, j1, j2)
        do j = 1, n2
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                d1t(i, j) = d1t(i, j) + (u(i2, j) - u(i1, j) - w1(i, j) - d1(i, j))
                if (j == n2) then
                    j1 = j - 1
                    j2 = j
                else
                    j1 = j
                    j2 = j + 1
                end if
                d2t(i, j) = d2t(i, j) + (u(i, j2) - u(i, j1) - w2(i, j) - d2(i, j))
            end do
        end do
        !$omp end parallel do
        if (alpha1 /= 0) then
            !$omp parallel do private(i, j, i1, i2, j1, j2, tmp)
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    s11t(i, j) = s11t(i, j) + (w1(i2, j) - w1(i1, j)) - s11(i, j)
                    s22t(i, j) = s22t(i, j) + (w2(i, j2) - w2(i, j1)) - s22(i, j)
                    s12t(i, j) = s12t(i, j) + 0.5*(w1(i, j2) - w1(i, j1) + w2(i2, j) - w2(i1, j)) - s12(i, j)
                end do
            end do
            !$omp end parallel do
        end if
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> TGpV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function tgpv_filt_2d_double
!
!> 3D TGpV denoising
!
function tgpv_filt_3d_double(f, mu, alpha0, alpha1, niter, p) result(u)
    double precision, dimension(:, :, :), intent(in) :: f
    double precision, intent(in) :: mu, alpha0, alpha1, p
    integer, intent(in) :: niter
    double precision, allocatable, dimension(:, :, :) :: u
    integer :: i, j, k
    double precision :: tmp, sumu, sumw1, sumw2, sumw3
    double precision, allocatable, dimension(:, :, :) :: d1, d2, d3, d1t, d2t, d3t
    double precision, allocatable, dimension(:, :, :) :: s11, s12, s13, s22, s23, s33
    double precision, allocatable, dimension(:, :, :) :: s11t, s12t, s13t, s22t, s23t, s33t
    double precision, allocatable, dimension(:, :, :) :: w1, w2, w3
    double precision, allocatable, dimension(:, :, :) :: tmpd1w2, tmpd1w3
    double precision, allocatable, dimension(:, :, :) :: tmpd2w1, tmpd2w3
    double precision, allocatable, dimension(:, :, :) :: tmpd3w1, tmpd3w2
    double precision, allocatable, dimension(:, :, :) :: pu
    integer :: n1, n2, n3
    integer :: iter, inner, ninner
    integer :: i1, i2, j1, j2, k1, k2
    integer :: ii1, ii2, jj1, jj2, kk1, kk2
    double precision :: lambda0, lambda1
    double precision :: bsum
    ! number of inner iteration
    ninner = 2
    ! lambdas for u and w subproblems
    lambda0 = 2.0*mu
    lambda1 = 2.0*mu*(alpha1/alpha0)
    n1 = size(f, 1)
    n2 = size(f, 2)
    n3 = size(f, 3)
    ! memory
    d1 = zeros(n1, n2, n3)
    d2 = zeros(n1, n2, n3)
    d3 = zeros(n1, n2, n3)
    d1t = zeros(n1, n2, n3)
    d2t = zeros(n1, n2, n3)
    d3t = zeros(n1, n2, n3)
    s11 = zeros(n1, n2, n3)
    s12 = zeros(n1, n2, n3)
    s13 = zeros(n1, n2, n3)
    s22 = zeros(n1, n2, n3)
    s23 = zeros(n1, n2, n3)
    s33 = zeros(n1, n2, n3)
    s11t = zeros(n1, n2, n3)
    s12t = zeros(n1, n2, n3)
    s13t = zeros(n1, n2, n3)
    s22t = zeros(n1, n2, n3)
    s23t = zeros(n1, n2, n3)
    s33t = zeros(n1, n2, n3)
    w1 = zeros(n1, n2, n3)
    w2 = zeros(n1, n2, n3)
    w3 = zeros(n1, n2, n3)
    u = zeros(n1, n2, n3)
    tmpd1w2 = zeros(n1, n2, n3)
    tmpd1w3 = zeros(n1, n2, n3)
    tmpd2w1 = zeros(n1, n2, n3)
    tmpd2w3 = zeros(n1, n2, n3)
    tmpd3w1 = zeros(n1, n2, n3)
    tmpd3w2 = zeros(n1, n2, n3)
    pu = zeros(n1, n2, n3)
    ! noisy image
    u = f
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! Minimization of u
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        if (i == 1) then
                            i1 = 0
                            ii1 = i
                        else
                            i1 = 1
                            ii1 = i - 1
                        end if
                        if (i == n1) then
                            i2 = 0
                            ii2 = i
                        else
                            i2 = 1
                            ii2 = i + 1
                        end if
                        if (j == 1) then
                            j1 = 0
                            jj1 = j
                        else
                            j1 = 1
                            jj1 = j - 1
                        end if
                        if (j == n2) then
                            j2 = 0
                            jj2 = j
                        else
                            j2 = 1
                            jj2 = j + 1
                        end if
                        if (k == 1) then
                            k1 = 0
                            kk1 = k
                        else
                            k1 = 1
                            kk1 = k - 1
                        end if
                        if (k == n3) then
                            k2 = 0
                            kk2 = k
                        else
                            k2 = 1
                            kk2 = k + 1
                        end if
                        sumu = lambda0*( &
                            +i2*u(ii2, j, k) + i1*u(ii1, j, k) &
                            + j2*u(i, jj2, k) + j1*u(i, jj1, k) &
                            + k2*u(i, j, kk2) + k1*u(i, j, kk1)) &
                            + lambda0*( &
                            +(i1*d1(ii1, j, k) - i2*d1(i, j, k)) &
                            + (j1*d2(i, jj1, k) - j2*d2(i, j, k)) &
                            + (k1*d3(i, j, kk1) - k2*d3(i, j, k)) &
                            - (i1*d1t(ii1, j, k) - i2*d1t(i, j, k)) &
                            - (j1*d2t(i, jj1, k) - j2*d2t(i, j, k)) &
                            - (k1*d3t(i, j, kk1) - k2*d3t(i, j, k)) &
                            + (i1*w1(ii1, j, k) - i2*w1(i, j, k)) &
                            + (j1*w2(i, jj1, k) - j2*w2(i, j, k)) &
                            + (k1*w3(i, j, kk1) - k2*w3(i, j, k)) &
                            ) &
                            + mu*f(i, j, k)
                        u(i, j, k) = sumu/(mu + (6.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2) - (1 - k1) - (1 - k2))*lambda0)
                    end do
                end do
            end do
            if (alpha1 /= 0) then
                ! minimization w
                do k = 1, n3
                    do j = 1, n2
                        do i = 1, n1
                            if (i == n1) then
                                i1 = i - 1
                                i2 = i
                            else
                                i1 = i
                                i2 = i + 1
                            end if
                            if (j == n2) then
                                j1 = j - 1
                                j2 = j
                            else
                                j1 = j
                                j2 = j + 1
                            end if
                            if (k == n3) then
                                k1 = k - 1
                                k2 = k
                            else
                                k1 = k
                                k2 = k + 1
                            end if
                            tmpd1w2(i, j, k) = 0.5*(w2(i2, j, k) - w2(i1, j, k))
                            tmpd1w3(i, j, k) = 0.5*(w3(i2, j, k) - w3(i1, j, k))
                            tmpd2w1(i, j, k) = 0.5*(w1(i, j2, k) - w1(i, j1, k))
                            tmpd2w3(i, j, k) = 0.5*(w3(i, j2, k) - w3(i, j1, k))
                            tmpd3w1(i, j, k) = 0.5*(w1(i, j, k2) - w1(i, j, k1))
                            tmpd3w2(i, j, k) = 0.5*(w2(i, j, k2) - w2(i, j, k1))
                        end do
                    end do
                end do
                ! minimization of w
                do k = 1, n3
                    do j = 1, n2
                        do i = 1, n1
                            if (i == 1) then
                                i1 = 0
                                ii1 = i
                            else
                                i1 = 1
                                ii1 = i - 1
                            end if
                            if (i == n1) then
                                i2 = 0
                                ii2 = i
                            else
                                i2 = 1
                                ii2 = i + 1
                            end if
                            if (j == 1) then
                                j1 = 0
                                jj1 = j
                            else
                                j1 = 1
                                jj1 = j - 1
                            end if
                            if (j == n2) then
                                j2 = 0
                                jj2 = j
                            else
                                j2 = 1
                                jj2 = j + 1
                            end if
                            if (k == 1) then
                                k1 = 0
                                kk1 = k
                            else
                                k1 = 1
                                kk1 = k - 1
                            end if
                            if (k == n3) then
                                k2 = 0
                                kk2 = k
                            else
                                k2 = 1
                                kk2 = k + 1
                            end if
                            ! w1
                            sumw1 = &
                                +lambda1*(i2*w1(ii2, j, k) + i1*w1(ii1, j, k)) &
                                + 0.5*lambda1*(j2*w1(i, jj2, k) + j1*w1(i, jj1, k)) &
                                + 0.5*lambda1*(k2*w1(i, j, kk2) + k1*w1(i, j, kk1)) &
                                - lambda0*(d1(i, j, k) - d1t(i, j, k) - (i2*u(ii2, j, k) - u(i, j, k))) &
                                + lambda1*(i1*s11(ii1, j, k) - i2*s11(i, j, k) - (i1*s11t(ii1, j, k) - i2*s11t(i, j, k))) &
                                + lambda1*(j1*s12(i, jj1, k) - j2*s12(i, j, k) - (j1*s12t(i, jj1, k) - j2*s12t(i, j, k)) - (j1*tmpd1w2(i, jj1, k) - j2*tmpd1w2(i, j, k))) &
                                + lambda1*(k1*s13(i, j, kk1) - k2*s13(i, j, k) - (k1*s13t(i, j, kk1) - k2*s13t(i, j, k)) - (k1*tmpd1w3(i, j, kk1) - k2*tmpd1w3(i, j, k)))
                            w1(i, j, k) = sumw1/(lambda0 + (2.0 - (1-i1) - (1-i2))*lambda1 + (2.0 - (1-j1) - (1-j2))*0.5*lambda1 + (2.0 - (1-k1) - (1-k2))*0.5*lambda1)
                            ! w2
                            sumw2 = &
                                +0.5*lambda1*(i2*w2(ii2, j, k) + i1*w2(ii1, j, k)) &
                                + lambda1*(j2*w2(i, jj2, k) + j1*w2(i, jj1, k)) &
                                + 0.5*lambda1*(k2*w2(i, j, kk2) + k1*w2(i, j, kk1)) &
                                - lambda0*(d2(i, j, k) - d2t(i, j, k) - (j2*u(i, jj2, k) - u(i, j, k))) &
                                + lambda1*(i1*s12(ii1, j, k) - i2*s12(i, j, k) - (i1*s12t(ii1, j, k) - i2*s12t(i, j, k)) - (i1*tmpd2w1(ii1, j, k) - i2*tmpd2w1(i, j, k))) &
                                + lambda1*(j1*s22(i, jj1, k) - j2*s22(i, j, k) - (j1*s22t(i, jj1, k) - j2*s22t(i, j, k))) &
                                + lambda1*(k1*s23(i, j, kk1) - k2*s23(i, j, k) - (k1*s23t(i, j, kk1) - k2*s23t(i, j, k)) - (k1*tmpd2w3(i, j, kk1) - k2*tmpd2w3(i, j, k)))
                            w2(i, j, k) = sumw2/(lambda0 + (2.0 - (1-i1) - (1-i2))*0.5*lambda1 + (2.0 - (1-j1) - (1-j2))*lambda1 + (2.0 - (1-k1) - (1-k2))*0.5*lambda1)
                            ! w3
                            sumw3 = &
                                +0.5*lambda1*(i2*w3(ii2, j, k) + i1*w3(ii1, j, k)) &
                                + 0.5*lambda1*(j2*w3(i, jj2, k) + j1*w3(i, jj1, k)) &
                                + lambda1*(k2*w3(i, j, kk2) + k1*w3(i, j, kk1)) &
                                - lambda0*(d3(i, j, k) - d3t(i, j, k) - (k2*u(i, j, kk2) - u(i, j, k))) &
                                + lambda1*(i1*s13(ii1, j, k) - i2*s13(i, j, k) - (i1*s13t(ii1, j, k) - i2*s13t(i, j, k)) - (i1*tmpd3w1(ii1, j, k) - i2*tmpd3w1(i, j, k))) &
                                + lambda1*(j1*s23(i, jj1, k) - j2*s23(i, j, k) - (j1*s23t(i, jj1, k) - j2*s23t(i, j, k)) - (j1*tmpd3w2(i, jj1, k) - j2*tmpd3w2(i, j, k))) &
                                + lambda1*(k1*s33(i, j, kk1) - k2*s33(i, j, k) - (k1*s33t(i, j, kk1) - k2*s33t(i, j, k)))
                            w3(i, j, k) = sumw3/(lambda0 + (2.0 - (1-i1) - (1-i2))*0.5*lambda1 + (2.0 - (1-j1) - (1-j2))*0.5*lambda1 + (2.0 - (1-k1) - (1-k2))*lambda1)
                        end do
                    end do
                end do
            end if
            ! update multipliers
            ! minimization d
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        tmp = (u(i2, j, k) - u(i1, j, k)) - w1(i, j, k) + d1t(i, j, k)
                        d1(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmp = (u(i, j2, k) - u(i, j1, k)) - w2(i, j, k) + d2t(i, j, k)
                        d2(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                        if (k == n3) then
                            k1 = k - 1
                            k2 = k
                        else
                            k1 = k
                            k2 = k + 1
                        end if
                        tmp = (u(i, j, k2) - u(i, j, k1)) - w3(i, j, k) + d3t(i, j, k)
                        d3(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                    end do
                end do
            end do
            if (alpha1 /= 0) then
                do k = 1, n3
                    do j = 1, n2
                        do i = 1, n1
                            if (i == n1) then
                                i1 = i - 1
                                i2 = i
                            else
                                i1 = i
                                i2 = i + 1
                            end if
                            if (j == n2) then
                                j1 = j - 1
                                j2 = j
                            else
                                j1 = j
                                j2 = j + 1
                            end if
                            if (k == n3) then
                                k1 = k - 1
                                k2 = k
                            else
                                k1 = k
                                k2 = k + 1
                            end if
                            tmp = w1(i2, j, k) - w1(i1, j, k) + s11t(i, j, k)
                            s11(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = w2(i, j2, k) - w2(i, j1, k) + s22t(i, j, k)
                            s22(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = w3(i, j, k2) - w3(i, j, k1) + s33t(i, j, k)
                            s33(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w2(i2, j, k) - w2(i1, j, k) + w1(i, j2, k) - w1(i, j1, k)) + s12t(i, j, k)
                            s12(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w3(i2, j, k) - w3(i1, j, k) + w1(i, j, k2) - w1(i, j, k1)) + s13t(i, j, k)
                            s13(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w3(i, j2, k) - w3(i, j1, k) + w2(i, j, k2) - w2(i, j, k1)) + s23t(i, j, k)
                            s23(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        end do
                    end do
                end do
            end if
        end do
        ! d
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    d1t(i, j, k) = d1t(i, j, k) + (u(i2, j, k) - u(i1, j, k) - w1(i, j, k) - d1(i, j, k))
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    d2t(i, j, k) = d2t(i, j, k) + (u(i, j2, k) - u(i, j1, k) - w2(i, j, k) - d2(i, j, k))
                    if (k == n3) then
                        k1 = k - 1
                        k2 = k
                    else
                        k1 = k
                        k2 = k + 1
                    end if
                    d3t(i, j, k) = d3t(i, j, k) + (u(i, j, k2) - u(i, j, k1) - w3(i, j, k) - d3(i, j, k))
                end do
            end do
        end do
        if (alpha1 /= 0) then
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        if (k == n3) then
                            k1 = k - 1
                            k2 = k
                        else
                            k1 = k
                            k2 = k + 1
                        end if
                        s11t(i, j, k) = s11t(i, j, k) + (w1(i2, j, k) - w1(i1, j, k)) - s11(i, j, k)
                        s22t(i, j, k) = s22t(i, j, k) + (w2(i, j2, k) - w2(i, j1, k)) - s22(i, j, k)
                        s33t(i, j, k) = s33t(i, j, k) + (w3(i, j, k2) - w3(i, j, k1)) - s33(i, j, k)
                        s12t(i, j, k) = s12t(i, j, k) + 0.5*(w1(i, j2, k) - w1(i, j1, k) + w2(i2, j, k) - w2(i1, j, k)) - s12(i, j, k)
                        s13t(i, j, k) = s13t(i, j, k) + 0.5*(w1(i, j, k2) - w1(i, j, k1) + w3(i2, j, k) - w3(i1, j, k)) - s13(i, j, k)
                        s23t(i, j, k) = s23t(i, j, k) + 0.5*(w2(i, j, k2) - w2(i, j, k1) + w3(i, j2, k) - w3(i, j1, k)) - s23(i, j, k)
                    end do
                end do
            end do
        end if
        ! progress
        if (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1) then
            bsum = sum((u - pu)**2)
            call warn(' >> TGpV iteration '//num2str(iter, '(i)') &
                //' of '//num2str(niter, '(i)')//' relative l2-norm difference = '//num2str(sqrt(bsum), '(es)'))
        end if
    end do
end function tgpv_filt_3d_double
!
!> 2D TGpV denoising MPI version
!
function tgpv_filt_2d_mpi_double(f0, mu, alpha0, alpha1, niter, p) result(u0)
    double precision, dimension(:, :), intent(in) :: f0
    double precision, intent(in) :: mu, alpha0, alpha1, p
    integer, intent(in) :: niter
    double precision, allocatable, dimension(:, :) :: u0
    integer :: i, j
    double precision :: tmp, sumu, sumw1, sumw2
    double precision, allocatable, dimension(:, :) :: d1, d2, d1t, d2t
    double precision, allocatable, dimension(:, :) :: s11, s12, s22
    double precision, allocatable, dimension(:, :) :: s11t, s12t, s22t
    double precision, allocatable, dimension(:, :) :: w1, w2, u, f
    double precision, allocatable, dimension(:, :) :: tmpd1w2
    double precision, allocatable, dimension(:, :) :: tmpd2w1
    double precision, allocatable, dimension(:, :) :: pu
    integer :: n1, n2
    integer :: iter, inner, ninner
    integer :: i1, i2, j1, j2
    integer :: ii1, ii2, jj1, jj2
    double precision :: lambda0, lambda1
    integer :: n1beg, n1end, n2beg, n2end
    double precision :: bsum
    logical :: nonempty
    ! number of inner iteration
    ninner = 2
    ! lambdas for u and w subproblems
    lambda0 = 2.0*mu
    lambda1 = 2.0*mu*(alpha1/alpha0)
    n1 = size(f0, 1)
    n2 = size(f0, 2)
    call domain_decomp_regular(n1, n2, n1beg, n1end, n2beg, n2end)
    if (rankid <= rank1*rank2 - 1) then
        nonempty = .true.
    else
        nonempty = .false.
    end if
    ! memory
    call alloc_array(d1, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(d2, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(d1t, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(d2t, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s11, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s12, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s22, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s11t, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s12t, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(s22t, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(w1, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(w2, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(u, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(f, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(tmpd1w2, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(tmpd2w1, [n1beg, n1end, n2beg, n2end], pad=1)
    call alloc_array(pu, [n1beg, n1end, n2beg, n2end], pad=1)
    ! noisy image
    f(n1beg:n1end, n2beg:n2end) = f0(n1beg:n1end, n2beg:n2end)
    u(n1beg:n1end, n2beg:n2end) = f0(n1beg:n1end, n2beg:n2end)
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            if (nonempty) then
                call commute_array(u, 1)
                call commute_array(d1, 1)
                call commute_array(d2, 1)
                call commute_array(d1t, 1)
                call commute_array(d2t, 1)
                call commute_array(w1, 1)
                call commute_array(w2, 1)
            end if
            ! minimization u
            do j = n2beg, n2end
                do i = n1beg, n1end
                    if (i == 1) then
                        i1 = 0
                        ii1 = i
                    else
                        i1 = 1
                        ii1 = i - 1
                    end if
                    if (i == n1) then
                        i2 = 0
                        ii2 = i
                    else
                        i2 = 1
                        ii2 = i + 1
                    end if
                    if (j == 1) then
                        j1 = 0
                        jj1 = j
                    else
                        j1 = 1
                        jj1 = j - 1
                    end if
                    if (j == n2) then
                        j2 = 0
                        jj2 = j
                    else
                        j2 = 1
                        jj2 = j + 1
                    end if
                    sumu = lambda0*( &
                        +i2*u(ii2, j) + i1*u(ii1, j) &
                        + j2*u(i, jj2) + j1*u(i, jj1)) &
                        + lambda0*( &
                        +(i1*d1(ii1, j) - i2*d1(i, j)) &
                        + (j1*d2(i, jj1) - j2*d2(i, j)) &
                        - (i1*d1t(ii1, j) - i2*d1t(i, j)) &
                        - (j1*d2t(i, jj1) - j2*d2t(i, j)) &
                        + (i1*w1(ii1, j) - i2*w1(i, j)) &
                        + (j1*w2(i, jj1) - j2*w2(i, j)) &
                        ) &
                        + mu*f(i, j)
                    u(i, j) = sumu/(mu + (4.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2))*lambda0)
                end do
            end do
            if (alpha1 /= 0) then
                ! minimization w
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmpd1w2(i, j) = 0.5*(w2(i2, j) - w2(i1, j))
                        tmpd2w1(i, j) = 0.5*(w1(i, j2) - w1(i, j1))
                    end do
                end do
                if (nonempty) then
                    call commute_array(u, 1)
                    call commute_array(tmpd1w2, 1)
                    call commute_array(tmpd2w1, 1)
                    call commute_array(s11, 1)
                    call commute_array(s12, 1)
                    call commute_array(s22, 1)
                    call commute_array(s11t, 1)
                    call commute_array(s12t, 1)
                    call commute_array(s22t, 1)
                end if
                ! minimization of w
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == 1) then
                            i1 = 0
                            ii1 = i
                        else
                            i1 = 1
                            ii1 = i - 1
                        end if
                        if (i == n1) then
                            i2 = 0
                            ii2 = i
                        else
                            i2 = 1
                            ii2 = i + 1
                        end if
                        if (j == 1) then
                            j1 = 0
                            jj1 = j
                        else
                            j1 = 1
                            jj1 = j - 1
                        end if
                        if (j == n2) then
                            j2 = 0
                            jj2 = j
                        else
                            j2 = 1
                            jj2 = j + 1
                        end if
                        ! w1
                        sumw1 = &
                            +lambda1*(i2*w1(ii2, j) + i1*w1(ii1, j)) &
                            + 0.5*lambda1*(j2*w1(i, jj2) + j1*w1(i, jj1)) &
                            - lambda0*(d1(i, j) - d1t(i, j) - (i2*u(ii2, j) - u(i, j))) &
                            + lambda1*(i1*s11(ii1, j) - i2*s11(i, j) - (i1*s11t(ii1, j) - i2*s11t(i, j))) &
                            + lambda1*(j1*s12(i, jj1) - j2*s12(i, j) - (j1*s12t(i, jj1) - j2*s12t(i, j)) - (j1*tmpd1w2(i, jj1) - j2*tmpd1w2(i, j)))
                        w1(i, j) = sumw1/(lambda0 + (2.0 - (1 - i1) - (1 - i2))*lambda1 + (2.0 - (1 - j1) - (1 - j2))*0.5*lambda1)
                        ! w2
                        sumw2 = &
                            +0.5*lambda1*(i2*w2(ii2, j) + i1*w2(ii1, j)) &
                            + lambda1*(j2*w2(i, jj2) + j1*w2(i, jj1)) &
                            - lambda0*(d2(i, j) - d2t(i, j) - (j2*u(i, jj2) - u(i, j))) &
                            + lambda1*(i1*s12(ii1, j) - i2*s12(i, j) - (i1*s12t(ii1, j) - i2*s12t(i, j)) - (i1*tmpd2w1(ii1, j) - i2*tmpd2w1(i, j))) &
                            + lambda1*(j1*s22(i, jj1) - j2*s22(i, j) - (j1*s22t(i, jj1) - j2*s22t(i, j)))
                        w2(i, j) = sumw2/(lambda0 + (2.0 - (1 - i1) - (1 - i2))*0.5*lambda1 + (2.0 - (1 - j1) - (1 - j2))*lambda1)
                    end do
                end do
            end if
            ! update multipliers
            ! minimization d
            do j = n2beg, n2end
                do i = n1beg, n1end
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    tmp = (u(i2, j) - u(i1, j)) - w1(i, j) + d1t(i, j)
                    d1(i, j) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    tmp = (u(i, j2) - u(i, j1)) - w2(i, j) + d2t(i, j)
                    d2(i, j) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                end do
            end do
            if (alpha1 /= 0) then
                if (nonempty) then
                    call commute_array(w1, 1)
                    call commute_array(w2, 1)
                end if
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmp = w1(i2, j) - w1(i1, j) + s11t(i, j)
                        s11(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        tmp = w2(i, j2) - w2(i, j1) + s22t(i, j)
                        s22(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        tmp = 0.5*(w2(i2, j) - w2(i1, j) + w1(i, j2) - w1(i, j1)) + s12t(i, j)
                        s12(i, j) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                    end do
                end do
            end if
            call mpi_barrier(mpi_comm_world, mpi_ierr)
        end do
        ! d
        do j = n2beg, n2end
            do i = n1beg, n1end
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                d1t(i, j) = d1t(i, j) + (u(i2, j) - u(i1, j) - w1(i, j) - d1(i, j))
                if (j == n2) then
                    j1 = j - 1
                    j2 = j
                else
                    j1 = j
                    j2 = j + 1
                end if
                d2t(i, j) = d2t(i, j) + (u(i, j2) - u(i, j1) - w2(i, j) - d2(i, j))
            end do
        end do
        if (alpha1 /= 0) then
            do j = n2beg, n2end
                do i = n1beg, n1end
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    s11t(i, j) = s11t(i, j) + (w1(i2, j) - w1(i1, j)) - s11(i, j)
                    s22t(i, j) = s22t(i, j) + (w2(i, j2) - w2(i, j1)) - s22(i, j)
                    s12t(i, j) = s12t(i, j) + 0.5*(w1(i, j2) - w1(i, j1) + w2(i2, j) - w2(i1, j)) - s12(i, j)
                end do
            end do
        end if
        call mpi_barrier(mpi_comm_world, mpi_ierr)
        ! progress
        if (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1) then
            if (nonempty) then
                bsum = sum((u - pu)**2)
            else
                bsum = 0.0
            end if
            call allreduce(bsum)
            if (rankid == 0) then
                call warn(' >> TGpV iteration '//num2str(iter, '(i)') &
                    //' of '//num2str(niter, '(i)')//' relative l2-norm difference = '//num2str(sqrt(bsum), '(es)'))
            end if
        end if
    end do
    call mpi_barrier(mpi_comm_world, mpi_ierr)
    call alloc_array(u0, [1, n1, 1, n2])
    if (nonempty) then
        u0(n1beg:n1end, n2beg:n2end) = u(n1beg:n1end, n2beg:n2end)
    end if
    call allreduce_array(u0)
    call mpi_barrier(mpi_comm_world, mpi_ierr)
end function tgpv_filt_2d_mpi_double
!
!> 3D TGpV denoising MPI version
!
function tgpv_filt_3d_mpi_double(f0, mu, alpha0, alpha1, niter, p) result(u0)
    double precision, dimension(:, :, :), intent(in) :: f0
    double precision, intent(in) :: mu, alpha0, alpha1, p
    integer, intent(in) :: niter
    double precision, allocatable, dimension(:, :, :) :: u0
    integer :: i, j, k
    double precision :: tmp, sumu, sumw1, sumw2, sumw3
    double precision, allocatable, dimension(:, :, :) :: d1, d2, d3, d1t, d2t, d3t
    double precision, allocatable, dimension(:, :, :) :: s11, s12, s13, s22, s23, s33
    double precision, allocatable, dimension(:, :, :) :: s11t, s12t, s13t, s22t, s23t, s33t
    double precision, allocatable, dimension(:, :, :) :: w1, w2, w3, u, f
    double precision, allocatable, dimension(:, :, :) :: tmpd1w2, tmpd1w3
    double precision, allocatable, dimension(:, :, :) :: tmpd2w1, tmpd2w3
    double precision, allocatable, dimension(:, :, :) :: tmpd3w1, tmpd3w2
    double precision, allocatable, dimension(:, :, :) :: pu
    integer :: n1, n2, n3
    integer :: iter, inner, ninner
    integer :: i1, i2, j1, j2, k1, k2
    integer :: ii1, ii2, jj1, jj2, kk1, kk2
    double precision :: lambda0, lambda1
    integer :: n1beg, n1end, n2beg, n2end, n3beg, n3end
    double precision :: bsum
    logical :: nonempty
    ! number of inner iteration
    ninner = 2
    ! lambdas for u and w subproblems
    lambda0 = 2.0*mu
    lambda1 = 2.0*mu*(alpha1/alpha0)
    n1 = size(f0, 1)
    n2 = size(f0, 2)
    n3 = size(f0, 3)
    call domain_decomp_regular(n1, n2, n3, n1beg, n1end, n2beg, n2end, n3beg, n3end)
    if (rankid <= rank1*rank2*rank3 - 1) then
        nonempty = .true.
    else
        nonempty = .false.
    end if
    ! memory
    call alloc_array(d1, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(d2, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(d3, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(d1t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(d2t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(d3t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s11, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s12, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s13, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s22, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s23, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s33, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s11t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s12t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s13t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s22t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s23t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(s33t, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(w1, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(w2, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(w3, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(u, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(f, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd1w2, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd1w3, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd2w1, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd2w3, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd3w1, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(tmpd3w2, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    call alloc_array(pu, [n1beg, n1end, n2beg, n2end, n3beg, n3end], pad=1)
    ! noisy image
    f(n1beg:n1end, n2beg:n2end, n3beg:n3end) = f0(n1beg:n1end, n2beg:n2end, n3beg:n3end)
    u(n1beg:n1end, n2beg:n2end, n3beg:n3end) = f0(n1beg:n1end, n2beg:n2end, n3beg:n3end)
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            if (nonempty) then
                call commute_array(u, 1)
                call commute_array(d1, 1)
                call commute_array(d2, 1)
                call commute_array(d3, 1)
                call commute_array(d1t, 1)
                call commute_array(d2t, 1)
                call commute_array(d3t, 1)
                call commute_array(w1, 1)
                call commute_array(w2, 1)
                call commute_array(w3, 1)
            end if
            ! minimization u
            do k = n3beg, n3end
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == 1) then
                            i1 = 0
                            ii1 = i
                        else
                            i1 = 1
                            ii1 = i - 1
                        end if
                        if (i == n1) then
                            i2 = 0
                            ii2 = i
                        else
                            i2 = 1
                            ii2 = i + 1
                        end if
                        if (j == 1) then
                            j1 = 0
                            jj1 = j
                        else
                            j1 = 1
                            jj1 = j - 1
                        end if
                        if (j == n2) then
                            j2 = 0
                            jj2 = j
                        else
                            j2 = 1
                            jj2 = j + 1
                        end if
                        if (k == 1) then
                            k1 = 0
                            kk1 = k
                        else
                            k1 = 1
                            kk1 = k - 1
                        end if
                        if (k == n3) then
                            k2 = 0
                            kk2 = k
                        else
                            k2 = 1
                            kk2 = k + 1
                        end if
                        sumu = lambda0*( &
                            +i2*u(ii2, j, k) + i1*u(ii1, j, k) &
                            + j2*u(i, jj2, k) + j1*u(i, jj1, k) &
                            + k2*u(i, j, kk2) + k1*u(i, j, kk1)) &
                            + lambda0*( &
                            +(i1*d1(ii1, j, k) - i2*d1(i, j, k)) &
                            + (j1*d2(i, jj1, k) - j2*d2(i, j, k)) &
                            + (k1*d3(i, j, kk1) - k2*d3(i, j, k)) &
                            - (i1*d1t(ii1, j, k) - i2*d1t(i, j, k)) &
                            - (j1*d2t(i, jj1, k) - j2*d2t(i, j, k)) &
                            - (k1*d3t(i, j, kk1) - k2*d3t(i, j, k)) &
                            + (i1*w1(ii1, j, k) - i2*w1(i, j, k)) &
                            + (j1*w2(i, jj1, k) - j2*w2(i, j, k)) &
                            + (k1*w3(i, j, kk1) - k2*w3(i, j, k)) &
                            ) &
                            + mu*f(i, j, k)
                        u(i, j, k) = sumu/(mu + (6.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2) - (1 - k1) - (1 - k2))*lambda0)
                    end do
                end do
            end do
            if (alpha1 /= 0) then
                ! minimization w
                do k = n3beg, n3end
                    do j = n2beg, n2end
                        do i = n1beg, n1end
                            if (i == n1) then
                                i1 = i - 1
                                i2 = i
                            else
                                i1 = i
                                i2 = i + 1
                            end if
                            if (j == n2) then
                                j1 = j - 1
                                j2 = j
                            else
                                j1 = j
                                j2 = j + 1
                            end if
                            if (k == n3) then
                                k1 = k - 1
                                k2 = k
                            else
                                k1 = k
                                k2 = k + 1
                            end if
                            tmpd1w2(i, j, k) = 0.5*(w2(i2, j, k) - w2(i1, j, k))
                            tmpd1w3(i, j, k) = 0.5*(w3(i2, j, k) - w3(i1, j, k))
                            tmpd2w1(i, j, k) = 0.5*(w1(i, j2, k) - w1(i, j1, k))
                            tmpd2w3(i, j, k) = 0.5*(w3(i, j2, k) - w3(i, j1, k))
                            tmpd3w1(i, j, k) = 0.5*(w1(i, j, k2) - w1(i, j, k1))
                            tmpd3w2(i, j, k) = 0.5*(w2(i, j, k2) - w2(i, j, k1))
                        end do
                    end do
                end do
                if (nonempty) then
                    call commute_array(u, 1)
                    call commute_array(tmpd1w2, 1)
                    call commute_array(tmpd1w3, 1)
                    call commute_array(tmpd2w1, 1)
                    call commute_array(tmpd2w3, 1)
                    call commute_array(tmpd3w1, 1)
                    call commute_array(tmpd3w2, 1)
                    call commute_array(s11, 1)
                    call commute_array(s12, 1)
                    call commute_array(s13, 1)
                    call commute_array(s22, 1)
                    call commute_array(s23, 1)
                    call commute_array(s33, 1)
                    call commute_array(s11t, 1)
                    call commute_array(s12t, 1)
                    call commute_array(s13t, 1)
                    call commute_array(s22t, 1)
                    call commute_array(s23t, 1)
                    call commute_array(s33t, 1)
                end if
                ! minimization of w
                do k = n3beg, n3end
                    do j = n2beg, n2end
                        do i = n1beg, n1end
                            if (i == 1) then
                                i1 = 0
                                ii1 = i
                            else
                                i1 = 1
                                ii1 = i - 1
                            end if
                            if (i == n1) then
                                i2 = 0
                                ii2 = i
                            else
                                i2 = 1
                                ii2 = i + 1
                            end if
                            if (j == 1) then
                                j1 = 0
                                jj1 = j
                            else
                                j1 = 1
                                jj1 = j - 1
                            end if
                            if (j == n2) then
                                j2 = 0
                                jj2 = j
                            else
                                j2 = 1
                                jj2 = j + 1
                            end if
                            if (k == 1) then
                                k1 = 0
                                kk1 = k
                            else
                                k1 = 1
                                kk1 = k - 1
                            end if
                            if (k == n3) then
                                k2 = 0
                                kk2 = k
                            else
                                k2 = 1
                                kk2 = k + 1
                            end if
                            ! w1
                            sumw1 = &
                                +lambda1*(i2*w1(ii2, j, k) + i1*w1(ii1, j, k)) &
                                + 0.5*lambda1*(j2*w1(i, jj2, k) + j1*w1(i, jj1, k)) &
                                + 0.5*lambda1*(k2*w1(i, j, kk2) + k1*w1(i, j, kk1)) &
                                - lambda0*(d1(i, j, k) - d1t(i, j, k) - (i2*u(ii2, j, k) - u(i, j, k))) &
                                + lambda1*(i1*s11(ii1, j, k) - i2*s11(i, j, k) - (i1*s11t(ii1, j, k) - i2*s11t(i, j, k))) &
                                + lambda1*(j1*s12(i, jj1, k) - j2*s12(i, j, k) - (j1*s12t(i, jj1, k) - j2*s12t(i, j, k)) - (j1*tmpd1w2(i, jj1, k) - j2*tmpd1w2(i, j, k))) &
                                + lambda1*(k1*s13(i, j, kk1) - k2*s13(i, j, k) - (k1*s13t(i, j, kk1) - k2*s13t(i, j, k)) - (k1*tmpd1w3(i, j, kk1) - k2*tmpd1w3(i, j, k)))
                            w1(i, j, k) = sumw1/(lambda0 + (2.0 - (1-i1) - (1-i2))*lambda1 + (2.0 - (1-j1) - (1-j2))*0.5*lambda1 + (2.0 - (1-k1) - (1-k2))*0.5*lambda1)
                            ! w2
                            sumw2 = &
                                +0.5*lambda1*(i2*w2(ii2, j, k) + i1*w2(ii1, j, k)) &
                                + lambda1*(j2*w2(i, jj2, k) + j1*w2(i, jj1, k)) &
                                + 0.5*lambda1*(k2*w2(i, j, kk2) + k1*w2(i, j, kk1)) &
                                - lambda0*(d2(i, j, k) - d2t(i, j, k) - (j2*u(i, jj2, k) - u(i, j, k))) &
                                + lambda1*(i1*s12(ii1, j, k) - i2*s12(i, j, k) - (i1*s12t(ii1, j, k) - i2*s12t(i, j, k)) - (i1*tmpd2w1(ii1, j, k) - i2*tmpd2w1(i, j, k))) &
                                + lambda1*(j1*s22(i, jj1, k) - j2*s22(i, j, k) - (j1*s22t(i, jj1, k) - j2*s22t(i, j, k))) &
                                + lambda1*(k1*s23(i, j, kk1) - k2*s23(i, j, k) - (k1*s23t(i, j, kk1) - k2*s23t(i, j, k)) - (k1*tmpd2w3(i, j, kk1) - k2*tmpd2w3(i, j, k)))
                            w2(i, j, k) = sumw2/(lambda0 + (2.0 - (1-i1) - (1-i2))*0.5*lambda1 + (2.0 - (1-j1) - (1-j2))*lambda1 + (2.0 - (1-k1) - (1-k2))*0.5*lambda1)
                            ! w3
                            sumw3 = &
                                +0.5*lambda1*(i2*w3(ii2, j, k) + i1*w3(ii1, j, k)) &
                                + 0.5*lambda1*(j2*w3(i, jj2, k) + j1*w3(i, jj1, k)) &
                                + lambda1*(k2*w3(i, j, kk2) + k1*w3(i, j, kk1)) &
                                - lambda0*(d3(i, j, k) - d3t(i, j, k) - (k2*u(i, j, kk2) - u(i, j, k))) &
                                + lambda1*(i1*s13(ii1, j, k) - i2*s13(i, j, k) - (i1*s13t(ii1, j, k) - i2*s13t(i, j, k)) - (i1*tmpd3w1(ii1, j, k) - i2*tmpd3w1(i, j, k))) &
                                + lambda1*(j1*s23(i, jj1, k) - j2*s23(i, j, k) - (j1*s23t(i, jj1, k) - j2*s23t(i, j, k)) - (j1*tmpd3w2(i, jj1, k) - j2*tmpd3w2(i, j, k))) &
                                + lambda1*(k1*s33(i, j, kk1) - k2*s33(i, j, k) - (k1*s33t(i, j, kk1) - k2*s33t(i, j, k)))
                            w3(i, j, k) = sumw3/(lambda0 + (2.0 - (1-i1) - (1-i2))*0.5*lambda1 + (2.0 - (1-j1) - (1-j2))*0.5*lambda1 + (2.0 - (1-k1) - (1-k2))*lambda1)
                        end do
                    end do
                end do
            end if
            ! update multipliers
            ! minimization d
            do k = n3beg, n3end
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        tmp = (u(i2, j, k) - u(i1, j, k)) - w1(i, j, k) + d1t(i, j, k)
                        d1(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmp = (u(i, j2, k) - u(i, j1, k)) - w2(i, j, k) + d2t(i, j, k)
                        d2(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                        if (k == n3) then
                            k1 = k - 1
                            k2 = k
                        else
                            k1 = k
                            k2 = k + 1
                        end if
                        tmp = (u(i, j, k2) - u(i, j, k1)) - w3(i, j, k) + d3t(i, j, k)
                        d3(i, j, k) = max(1.0 - (lambda0/alpha0*abs(tmp))**(p - 2), 0.0)*tmp
                    end do
                end do
            end do
            if (alpha1 /= 0) then
                if (nonempty) then
                    call commute_array(w1, 1)
                    call commute_array(w2, 1)
                    call commute_array(w3, 1)
                end if
                do k = n3beg, n3end
                    do j = n2beg, n2end
                        do i = n1beg, n1end
                            if (i == n1) then
                                i1 = i - 1
                                i2 = i
                            else
                                i1 = i
                                i2 = i + 1
                            end if
                            if (j == n2) then
                                j1 = j - 1
                                j2 = j
                            else
                                j1 = j
                                j2 = j + 1
                            end if
                            if (k == n3) then
                                k1 = k - 1
                                k2 = k
                            else
                                k1 = k
                                k2 = k + 1
                            end if
                            tmp = w1(i2, j, k) - w1(i1, j, k) + s11t(i, j, k)
                            s11(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = w2(i, j2, k) - w2(i, j1, k) + s22t(i, j, k)
                            s22(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = w3(i, j, k2) - w3(i, j, k1) + s33t(i, j, k)
                            s33(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w2(i2, j, k) - w2(i1, j, k) + w1(i, j2, k) - w1(i, j1, k)) + s12t(i, j, k)
                            s12(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w3(i2, j, k) - w3(i1, j, k) + w1(i, j, k2) - w1(i, j, k1)) + s13t(i, j, k)
                            s13(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                            tmp = 0.5*(w3(i, j2, k) - w3(i, j1, k) + w2(i, j, k2) - w2(i, j, k1)) + s23t(i, j, k)
                            s23(i, j, k) = max(1.0 - (lambda1/alpha1*abs(tmp))**(p - 2), 0.0)*tmp
                        end do
                    end do
                end do
            end if
            call mpi_barrier(mpi_comm_world, mpi_ierr)
        end do
        ! d
        do k = n3beg, n3end
            do j = n2beg, n2end
                do i = n1beg, n1end
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    d1t(i, j, k) = d1t(i, j, k) + (u(i2, j, k) - u(i1, j, k) - w1(i, j, k) - d1(i, j, k))
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    d2t(i, j, k) = d2t(i, j, k) + (u(i, j2, k) - u(i, j1, k) - w2(i, j, k) - d2(i, j, k))
                    if (k == n3) then
                        k1 = k - 1
                        k2 = k
                    else
                        k1 = k
                        k2 = k + 1
                    end if
                    d3t(i, j, k) = d3t(i, j, k) + (u(i, j, k2) - u(i, j, k1) - w3(i, j, k) - d3(i, j, k))
                end do
            end do
        end do
        if (alpha1 /= 0) then
            do k = n3beg, n3end
                do j = n2beg, n2end
                    do i = n1beg, n1end
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        if (k == n3) then
                            k1 = k - 1
                            k2 = k
                        else
                            k1 = k
                            k2 = k + 1
                        end if
                        s11t(i, j, k) = s11t(i, j, k) + (w1(i2, j, k) - w1(i1, j, k)) - s11(i, j, k)
                        s22t(i, j, k) = s22t(i, j, k) + (w2(i, j2, k) - w2(i, j1, k)) - s22(i, j, k)
                        s33t(i, j, k) = s33t(i, j, k) + (w3(i, j, k2) - w3(i, j, k1)) - s33(i, j, k)
                        s12t(i, j, k) = s12t(i, j, k) + 0.5*(w1(i, j2, k) - w1(i, j1, k) + w2(i2, j, k) - w2(i1, j, k)) - s12(i, j, k)
                        s13t(i, j, k) = s13t(i, j, k) + 0.5*(w1(i, j, k2) - w1(i, j, k1) + w3(i2, j, k) - w3(i1, j, k)) - s13(i, j, k)
                        s23t(i, j, k) = s23t(i, j, k) + 0.5*(w2(i, j, k2) - w2(i, j, k1) + w3(i, j2, k) - w3(i, j1, k)) - s23(i, j, k)
                    end do
                end do
            end do
        end if
        call mpi_barrier(mpi_comm_world, mpi_ierr)
        ! progress
        if (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1) then
            if (nonempty) then
                bsum = sum((u - pu)**2)
            else
                bsum = 0.0
            end if
            call allreduce(bsum)
            if (rankid == 0) then
                call warn(' >> TGpV iteration '//num2str(iter, '(i)') &
                    //' of '//num2str(niter, '(i)')//' relative l2-norm difference = '//num2str(sqrt(bsum), '(es)'))
            end if
        end if
    end do
    call mpi_barrier(mpi_comm_world, mpi_ierr)
    call alloc_array(u0, [1, n1, 1, n2, 1, n3])
    if (nonempty) then
        u0(n1beg:n1end, n2beg:n2end, n3beg:n3end) = u(n1beg:n1end, n2beg:n2end, n3beg:n3end)
    end if
    call allreduce_array(u0)
    call mpi_barrier(mpi_comm_world, mpi_ierr)
end function tgpv_filt_3d_mpi_double
!
!> 2D TV + sparsity denoising
!
function sparse_tv_filt_1d_double(f, mu, l_sparse, l_tv, niter, verbose) result(u)
    double precision, dimension(:) :: f
    double precision :: mu, l_sparse, l_tv
    integer :: niter
    logical, optional :: verbose
    double precision, allocatable, dimension(:) :: u
    integer :: i, iter
    double precision :: tmp, tmpx, sk, sumu
    double precision, allocatable, dimension(:) :: d1, d1t, s, st, pu
    integer :: n1
    double precision :: alpha0, alpha1, p
    integer :: inner, ninner
    integer :: i1, i2
    integer :: ii1, ii2
    double precision :: lambda0, lambda1
    logical :: tv_verbose
    ! read parameter
    n1 = size(f)
    alpha0 = l_tv
    alpha1 = l_sparse
    call assert(alpha0 <= 1, ' <sparse_tv_filt_2d_> Error: l_tv must >= 0 and <= 1')
    call assert(alpha1 <= 1, ' <sparse_tv_filt_2d_> Error: l_sparse must >= 0 and <= 1')
    p = 1.0
    ninner = 2
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1)
    d1t = zeros(n1)
    s = zeros(n1)
    st = zeros(n1)
    u = zeros(n1)
    pu = zeros(n1)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda0 = 2*mu*alpha0
    lambda1 = 2*mu*alpha1
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! minimization u
            do i = 1, n1
                if (i == 1) then
                    i1 = 0
                    ii1 = i
                else
                    i1 = 1
                    ii1 = i - 1
                end if
                if (i == n1) then
                    i2 = 0
                    ii2 = i
                else
                    i2 = 1
                    ii2 = i + 1
                end if
                sumu = lambda0*( &
                    +i2*u(ii2) + i1*u(ii1)) &
                    + lambda1*( &
                    +s(i) - st(i)) &
                    + lambda0*( &
                    +(i1*d1(ii1) - i2*d1(i)) &
                    - (i1*d1t(ii1) - i2*d1t(i)) &
                    ) &
                    + mu*f(i)
                u(i) = sumu/(mu + lambda1 + (2.0 - (1 - i1) - (1 - i2))*lambda0)
            end do
            ! update multipliers
            !$omp parallel do private(i, i1, i2, tmpx, sk)
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                tmpx = (u(i2) - u(i1)) + d1t(i)
                sk = abs(tmpx)
                d1(i) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpx/(sk + float_tiny)
            end do
            !$omp end parallel do
            !$omp parallel do private(i, tmp, sk)
            do i = 1, n1
                tmp = u(i) + st(i)
                sk = abs(tmp)
                s(i) = max(sk - 1.0/(lambda1/alpha1), 0.0)*tmp/(sk + float_tiny)
            end do
            !$omp end parallel do
        end do
        ! update split-Bregman variables
        !$omp parallel do private(i, i1, i2)
        do i = 1, n1
            if (i == n1) then
                i1 = i - 1
                i2 = i
            else
                i1 = i
                i2 = i + 1
            end if
            d1t(i) = d1t(i) + (u(i2) - u(i1) - d1(i))
        end do
        !$omp end parallel do
        !$omp parallel do private(i)
        do i = 1, n1
            st(i) = st(i) + u(i) - s(i)
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Sparse TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function sparse_tv_filt_1d_double
!
!> 2D TV + sparsity denoising
!
function sparse_tv_filt_2d_double(f, mu, l_sparse, l_tv, niter, verbose) result(u)
    double precision, dimension(:, :) :: f
    double precision :: mu, l_sparse, l_tv
    integer :: niter
    logical, optional :: verbose
    double precision, allocatable, dimension(:, :) :: u
    integer :: i, j, iter
    double precision :: tmp, tmpx, tmpz, sk, sumu
    double precision, allocatable, dimension(:, :) :: d1, d2, d1t, d2t, s, st, pu
    integer :: n1, n2
    double precision :: alpha0, alpha1, p
    integer :: inner, ninner
    integer :: i1, i2, j1, j2
    integer :: ii1, ii2, jj1, jj2
    double precision :: lambda0, lambda1
    logical :: tv_verbose
    ! read parameter
    n1 = size(f, 1)
    n2 = size(f, 2)
    alpha0 = l_tv
    alpha1 = l_sparse
    call assert(alpha0 <= 1, ' <sparse_tv_filt_2d_> Error: l_tv must >= 0 and <= 1')
    call assert(alpha1 <= 1, ' <sparse_tv_filt_2d_> Error: l_sparse must >= 0 and <= 1')
    p = 1.0
    ninner = 2
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1, n2)
    d2 = zeros(n1, n2)
    d1t = zeros(n1, n2)
    d2t = zeros(n1, n2)
    s = zeros(n1, n2)
    st = zeros(n1, n2)
    u = zeros(n1, n2)
    pu = zeros(n1, n2)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda0 = 2*mu*alpha0
    lambda1 = 2*mu*alpha1
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! minimization u
            do j = 1, n2
                do i = 1, n1
                    if (i == 1) then
                        i1 = 0
                        ii1 = i
                    else
                        i1 = 1
                        ii1 = i - 1
                    end if
                    if (i == n1) then
                        i2 = 0
                        ii2 = i
                    else
                        i2 = 1
                        ii2 = i + 1
                    end if
                    if (j == 1) then
                        j1 = 0
                        jj1 = j
                    else
                        j1 = 1
                        jj1 = j - 1
                    end if
                    if (j == n2) then
                        j2 = 0
                        jj2 = j
                    else
                        j2 = 1
                        jj2 = j + 1
                    end if
                    sumu = lambda0*( &
                        +i2*u(ii2, j) + i1*u(ii1, j) &
                        + j2*u(i, jj2) + j1*u(i, jj1)) &
                        + lambda1*( &
                        +s(i, j) - st(i, j)) &
                        + lambda0*( &
                        +(i1*d1(ii1, j) - i2*d1(i, j)) &
                        + (j1*d2(i, jj1) - j2*d2(i, j)) &
                        - (i1*d1t(ii1, j) - i2*d1t(i, j)) &
                        - (j1*d2t(i, jj1) - j2*d2t(i, j)) &
                        ) &
                        + mu*f(i, j)
                    u(i, j) = sumu/(mu + lambda1 + (4.0 - (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2))*lambda0)
                end do
            end do
            ! update multipliers
            !$omp parallel do private(i, j, i1, i2, j1, j2, tmpx, tmpz, sk)
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    tmpx = (u(i2, j) - u(i1, j)) + d1t(i, j)
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    tmpz = (u(i, j2) - u(i, j1)) + d2t(i, j)
                    sk = sqrt(tmpx**2 + tmpz**2)
                    d1(i, j) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpx/(sk + float_tiny)
                    d2(i, j) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpz/(sk + float_tiny)
                end do
            end do
            !$omp end parallel do
            !$omp parallel do private(i, j, tmp, sk)
            do j = 1, n2
                do i = 1, n1
                    tmp = u(i, j) + st(i, j)
                    sk = abs(tmp)
                    s(i, j) = max(sk - 1.0/(lambda1/alpha1), 0.0)*tmp/(sk + float_tiny)
                end do
            end do
            !$omp end parallel do
        end do
        ! update split-Bregman variables
        !$omp parallel do private(i, j, i1, i2, j1, j2)
        do j = 1, n2
            do i = 1, n1
                if (i == n1) then
                    i1 = i - 1
                    i2 = i
                else
                    i1 = i
                    i2 = i + 1
                end if
                d1t(i, j) = d1t(i, j) + (u(i2, j) - u(i1, j) - d1(i, j))
                if (j == n2) then
                    j1 = j - 1
                    j2 = j
                else
                    j1 = j
                    j2 = j + 1
                end if
                d2t(i, j) = d2t(i, j) + (u(i, j2) - u(i, j1) - d2(i, j))
            end do
        end do
        !$omp end parallel do
        !$omp parallel do private(i, j)
        do j = 1, n2
            do i = 1, n1
                st(i, j) = st(i, j) + u(i, j) - s(i, j)
            end do
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Sparse TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function sparse_tv_filt_2d_double
!
!> 3D TV + sparsity denoising
!
function sparse_tv_filt_3d_double(f, mu, l_sparse, l_tv, niter, verbose) result(u)
    double precision, dimension(:, :, :) :: f
    double precision :: mu, l_sparse, l_tv
    integer :: niter
    logical, optional :: verbose
    double precision, allocatable, dimension(:, :, :) :: u
    integer :: i, j, k, iter
    double precision :: tmp, tmpx, tmpy, tmpz, sk, sumu
    double precision, allocatable, dimension(:, :, :) :: d1, d2, d3, d1t, d2t, d3t, s, st, pu
    integer :: n1, n2, n3
    double precision :: alpha0, alpha1, p
    integer :: inner, ninner
    integer :: i1, i2, j1, j2, k1, k2
    integer :: ii1, ii2, jj1, jj2, kk1, kk2
    double precision :: lambda0, lambda1
    logical :: tv_verbose
    ! read parameter
    n1 = size(f, 1)
    n2 = size(f, 2)
    n3 = size(f, 3)
    alpha0 = l_tv
    alpha1 = l_sparse
    call assert(alpha0 <= 1, ' <sparse_tv_filt_3d_> Error: l_tv must >= 0 and <= 1')
    call assert(alpha1 <= 1, ' <sparse_tv_filt_3d_> Error: l_sparse must >= 0 and <= 1')
    p = 1.0
    ninner = 2
    if (present(verbose)) then
        tv_verbose = verbose
    else
        tv_verbose = .true.
    end if
    ! memory
    d1 = zeros(n1, n2, n3)
    d2 = zeros(n1, n2, n3)
    d3 = zeros(n1, n2, n3)
    d1t = zeros(n1, n2, n3)
    d2t = zeros(n1, n2, n3)
    d3t = zeros(n1, n2, n3)
    s = zeros(n1, n2, n3)
    st = zeros(n1, n2, n3)
    u = zeros(n1, n2, n3)
    pu = zeros(n1, n2, n3)
    ! read noisy image
    u = f
    ! lambdas for u and w subproblems
    lambda0 = 2*mu*alpha0
    lambda1 = 2*mu*alpha1
    ! TGpV iteration
    do iter = 1, niter
        pu = u
        do inner = 1, ninner
            ! minimization u
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        if (i == 1) then
                            i1 = 0
                            ii1 = i
                        else
                            i1 = 1
                            ii1 = i - 1
                        end if
                        if (i == n1) then
                            i2 = 0
                            ii2 = i
                        else
                            i2 = 1
                            ii2 = i + 1
                        end if
                        if (j == 1) then
                            j1 = 0
                            jj1 = j
                        else
                            j1 = 1
                            jj1 = j - 1
                        end if
                        if (j == n2) then
                            j2 = 0
                            jj2 = j
                        else
                            j2 = 1
                            jj2 = j + 1
                        end if
                        if (k == 1) then
                            k1 = 0
                            kk1 = k
                        else
                            k1 = 1
                            kk1 = k - 1
                        end if
                        if (k == n3) then
                            k2 = 0
                            kk2 = k
                        else
                            k2 = 1
                            kk2 = k + 1
                        end if
                        sumu = lambda0*( &
                            +i2*u(ii2, j, k) + i1*u(ii1, j, k) &
                            + j2*u(i, jj2, k) + j1*u(i, jj1, k) &
                            + k2*u(i, j, kk2) + k1*u(i, j, kk1)) &
                            + lambda1*( &
                            +s(i, j, k) - st(i, j, k)) &
                            + lambda0*( &
                            +(i1*d1(ii1, j, k) - i2*d1(i, j, k)) &
                            + (j1*d2(i, jj1, k) - j2*d2(i, j, k)) &
                            + (k1*d3(i, j, kk1) - k2*d3(i, j, k)) &
                            - (i1*d1t(ii1, j, k) - i2*d1t(i, j, k)) &
                            - (j1*d2t(i, jj1, k) - j2*d2t(i, j, k)) &
                            - (k1*d3t(i, j, kk1) - k2*d3t(i, j, k))) &
                            + mu*f(i, j, k)
                        u(i, j, k) = sumu/(mu + lambda1 + (6.0 - &
                            (1 - i1) - (1 - i2) - (1 - j1) - (1 - j2) - (1 - k1) - (1 - k2))*lambda0)
                    end do
                end do
            end do
            ! update multipliers
            !$omp parallel do private(i, j, k, i1, i2, j1, j2, k1, k2, tmpx, tmpy, tmpz, sk)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        if (i == n1) then
                            i1 = i - 1
                            i2 = i
                        else
                            i1 = i
                            i2 = i + 1
                        end if
                        tmpx = (u(i2, j, k) - u(i1, j, k)) + d1t(i, j, k)
                        if (j == n2) then
                            j1 = j - 1
                            j2 = j
                        else
                            j1 = j
                            j2 = j + 1
                        end if
                        tmpy = (u(i, j2, k) - u(i, j1, k)) + d2t(i, j, k)
                        if (k == n3) then
                            k1 = k - 1
                            k2 = k
                        else
                            k1 = k
                            k2 = k + 1
                        end if
                        tmpz = (u(i, j, k2) - u(i, j, k1)) + d3t(i, j, k)
                        sk = sqrt(tmpx**2 + tmpy**2 + tmpz**2)
                        d1(i, j, k) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpx/(sk + float_tiny)
                        d2(i, j, k) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpy/(sk + float_tiny)
                        d3(i, j, k) = max(sk - 1.0/(lambda0/alpha0), 0.0)*tmpz/(sk + float_tiny)
                    end do
                end do
            end do
            !$omp end parallel do
            !$omp parallel do private(i, j, k, tmp, sk)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        tmp = u(i, j, k) + st(i, j, k)
                        sk = abs(tmp)
                        s(i, j, k) = max(sk - 1.0/(lambda1/alpha1), 0.0)*tmp/(sk + float_tiny)
                    end do
                end do
            end do
            !$omp end parallel do
        end do
        ! update split-Bregman variables
        !$omp parallel do private(i, j, k, i1, i2, j1, j2, k1, k2)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    if (i == n1) then
                        i1 = i - 1
                        i2 = i
                    else
                        i1 = i
                        i2 = i + 1
                    end if
                    d1t(i, j, k) = d1t(i, j, k) + (u(i2, j, k) - u(i1, j, k) - d1(i, j, k))
                    if (j == n2) then
                        j1 = j - 1
                        j2 = j
                    else
                        j1 = j
                        j2 = j + 1
                    end if
                    d2t(i, j, k) = d2t(i, j, k) + (u(i, j2, k) - u(i, j1, k) - d2(i, j, k))
                    if (k == n3) then
                        k1 = k - 1
                        k2 = k
                    else
                        k1 = k
                        k2 = k + 1
                    end if
                    d3t(i, j, k) = d3t(i, j, k) + (u(i, j, k2) - u(i, j, k1) - d3(i, j, k))
                end do
            end do
        end do
        !$omp end parallel do
        !$omp parallel do private(i, j, k)
        do k = 1, n3
            do j = 1, n2
                do i = 1, n1
                    st(i, j, k) = st(i, j, k) + u(i, j, k) - s(i, j, k)
                end do
            end do
        end do
        !$omp end parallel do
        ! progress
        if (tv_verbose .and. (mod(iter, max(nint(niter/10.0), 1)) == 0 .or. iter == 1)) then
            call warn(' >> Sparse TV iteration '//tidy(num2str(iter, '(i)')) &
                //' of '//tidy(num2str(niter, '(i)')//' relative l2-norm difference = ' &
                //tidy(num2str(norm2(u - pu), '(es)'))))
        end if
    end do
end function sparse_tv_filt_3d_double
!
!> 1D soft thresholding
!
function soft_shrinkage_1d_double(x, b) result(y)
    double precision, dimension(:), intent(in) :: x
    double precision, intent(in) :: b
    double precision, allocatable, dimension(:) :: y
    y = x*0
    where (abs(x) >= b)
        y = x*(1.0 - b/abs(x))
    end where
end function soft_shrinkage_1d_double
!
!> 2D soft thresholding
!
function soft_shrinkage_2d_double(x, b) result(y)
    double precision, dimension(:, :), intent(in) :: x
    double precision, intent(in) :: b
    double precision, allocatable, dimension(:, :) :: y
    y = x*0
    where (abs(x) >= b)
        y = x*(1.0 - b/abs(x))
    end where
end function soft_shrinkage_2d_double
!
!> 3D soft thresholding
!
function soft_shrinkage_3d_double(x, b) result(y)
    double precision, dimension(:, :, :), intent(in) :: x
    double precision, intent(in) :: b
    double precision, allocatable, dimension(:, :, :) :: y
    y = x*0
    where (abs(x) >= b)
        y = x*(1.0 - b/abs(x))
    end where
end function soft_shrinkage_3d_double
end module libflit_tvfilt
