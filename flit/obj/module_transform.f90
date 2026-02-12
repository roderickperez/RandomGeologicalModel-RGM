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
include "mkl_dfti.f90"
! include "mkl_trig_transforms.f90"
module libflit_transform
    use mkl_dfti
    use omp_lib
    use libflit_constants
    use libflit_taper
    use libflit_array
    use libflit_error
    use libflit_array_operation
    ! use mkl_trig_transforms
    implicit none
    ! If using FFTW, then the following module is required:
    ! use iso_c_binding
!
! FFTW3 Fortran interface constants (Free-form compatible)
!
integer, parameter :: FFTW_FORWARD = -1
integer, parameter :: FFTW_BACKWARD = 1
integer, parameter :: FFTW_MEASURE = 0
integer, parameter :: FFTW_ESTIMATE = 64
integer, parameter :: FFTW_REDFT00 = 3
integer, parameter :: FFTW_REDFT10 = 5
integer, parameter :: FFTW_REDFT01 = 4
integer, parameter :: FFTW_REDFT11 = 6
integer, parameter :: FFTW_RODFT00 = 7
integer, parameter :: FFTW_RODFT10 = 9
integer, parameter :: FFTW_RODFT01 = 8
integer, parameter :: FFTW_RODFT11 = 10
! #include "fftw3_mkl.f"
    private
    interface convd
        module procedure :: discrete_conv_1d_float
        module procedure :: discrete_conv_2d_float
        module procedure :: discrete_conv_3d_float
        module procedure :: discrete_conv_1d_double
        module procedure :: discrete_conv_2d_double
        module procedure :: discrete_conv_3d_double
        module procedure :: discrete_conv_1d_complex
        module procedure :: discrete_conv_2d_complex
        module procedure :: discrete_conv_3d_complex
        module procedure :: discrete_conv_1d_dcomplex
        module procedure :: discrete_conv_2d_dcomplex
        module procedure :: discrete_conv_3d_dcomplex
    end interface convd
    ! Fourier and inverse Fourier transform
    interface fourier_transform
        module procedure :: fourier1_complex_to_complex
        module procedure :: fourier2_complex_to_complex
        module procedure :: fourier3_complex_to_complex
        module procedure :: fourier4_complex_to_complex
        module procedure :: fourier1_dcomplex_to_dcomplex
        module procedure :: fourier2_dcomplex_to_dcomplex
        module procedure :: fourier3_dcomplex_to_dcomplex
        module procedure :: fourier4_dcomplex_to_dcomplex
    end interface fourier_transform
    interface inverse_fourier_transform
        module procedure :: ifourier1_complex_to_complex
        module procedure :: ifourier2_complex_to_complex
        module procedure :: ifourier3_complex_to_complex
        module procedure :: ifourier4_complex_to_complex
        module procedure :: ifourier1_dcomplex_to_dcomplex
        module procedure :: ifourier2_dcomplex_to_dcomplex
        module procedure :: ifourier3_dcomplex_to_dcomplex
        module procedure :: ifourier4_dcomplex_to_dcomplex
    end interface inverse_fourier_transform
    interface fft
        ! 1D
        module procedure :: fft1_complex_to_complex
        module procedure :: fft1_pad_complex_to_complex
        module procedure :: fft1_dcomplex_to_dcomplex
        module procedure :: fft1_pad_dcomplex_to_dcomplex
        module procedure :: fft1_float_to_complex
        module procedure :: fft1_pad_float_to_complex
        module procedure :: fft1_double_to_dcomplex
        module procedure :: fft1_pad_double_to_dcomplex
        ! 2D
        module procedure :: fft2_complex_to_complex
        module procedure :: fft2_pad_complex_to_complex
        module procedure :: fft2_dcomplex_to_dcomplex
        module procedure :: fft2_pad_dcomplex_to_dcomplex
        module procedure :: fft2_float_to_complex
        module procedure :: fft2_pad_float_to_complex
        module procedure :: fft2_double_to_dcomplex
        module procedure :: fft2_pad_double_to_dcomplex
        ! 3D
        module procedure :: fft3_complex_to_complex
        module procedure :: fft3_pad_complex_to_complex
        module procedure :: fft3_dcomplex_to_dcomplex
        module procedure :: fft3_pad_dcomplex_to_dcomplex
        module procedure :: fft3_float_to_complex
        module procedure :: fft3_pad_float_to_complex
        module procedure :: fft3_double_to_dcomplex
        module procedure :: fft3_pad_double_to_dcomplex
    end interface fft
    interface ifft
        ! 1D
        module procedure :: ifft1_complex_to_complex
        module procedure :: ifft1_pad_complex_to_complex
        module procedure :: ifft1_dcomplex_to_dcomplex
        module procedure :: ifft1_pad_dcomplex_to_dcomplex
        module procedure :: ifft1_complex_to_float
        module procedure :: ifft1_pad_complex_to_float
        module procedure :: ifft1_dcomplex_to_double
        module procedure :: ifft1_pad_dcomplex_to_double
        ! 2D
        module procedure :: ifft2_complex_to_complex
        module procedure :: ifft2_pad_complex_to_complex
        module procedure :: ifft2_dcomplex_to_dcomplex
        module procedure :: ifft2_pad_dcomplex_to_dcomplex
        module procedure :: ifft2_complex_to_float
        module procedure :: ifft2_pad_complex_to_float
        module procedure :: ifft2_dcomplex_to_double
        module procedure :: ifft2_pad_dcomplex_to_double
        ! 2D
        module procedure :: ifft3_complex_to_complex
        module procedure :: ifft3_pad_complex_to_complex
        module procedure :: ifft3_dcomplex_to_dcomplex
        module procedure :: ifft3_pad_dcomplex_to_dcomplex
        module procedure :: ifft3_complex_to_float
        module procedure :: ifft3_pad_complex_to_float
        module procedure :: ifft3_dcomplex_to_double
        module procedure :: ifft3_pad_dcomplex_to_double
    end interface ifft
    ! FFT shift
    interface fftshift
        module procedure :: fftshift_1d_float
        module procedure :: fftshift_2d_float
        module procedure :: fftshift_3d_float
        module procedure :: fftshift_1d_double
        module procedure :: fftshift_2d_double
        module procedure :: fftshift_3d_double
        module procedure :: fftshift_1d_complex
        module procedure :: fftshift_2d_complex
        module procedure :: fftshift_3d_complex
        module procedure :: fftshift_1d_dcomplex
        module procedure :: fftshift_2d_dcomplex
        module procedure :: fftshift_3d_dcomplex
    end interface fftshift
    ! Hilbert transform
    interface hilbert
        module procedure :: hilbert_1d_float
        module procedure :: hilbert_1d_double
    end interface hilbert
    interface hilbert_transform
        module procedure :: hilbert_transform_1d_float
        module procedure :: hilbert_transform_1d_double
    end interface hilbert_transform
    ! Cosine and inverse cosine transform
    interface cosine_transform
        module procedure :: dct_1d_float
        module procedure :: dct_2d_float
        module procedure :: dct_3d_float
        module procedure :: dct_1d_double
        module procedure :: dct_2d_double
        module procedure :: dct_3d_double
    end interface cosine_transform
    interface inverse_cosine_transform
        module procedure :: idct_1d_float
        module procedure :: idct_2d_float
        module procedure :: idct_3d_float
        module procedure :: idct_1d_double
        module procedure :: idct_2d_double
        module procedure :: idct_3d_double
    end interface inverse_cosine_transform
    interface dct
        module procedure :: dct1_float
        module procedure :: dct2_float
        module procedure :: dct3_float
        module procedure :: dct1_double
        module procedure :: dct2_double
        module procedure :: dct3_double
    end interface dct
    interface idct
        module procedure :: idct1_float
        module procedure :: idct2_float
        module procedure :: idct3_float
        module procedure :: idct1_double
        module procedure :: idct2_double
        module procedure :: idct3_double
    end interface idct
    ! Envelope transform
    interface envelope
        module procedure :: envelope_1d_float
        module procedure :: envelope_1d_double
    end interface
    interface envelope_transform
        module procedure :: envelope_transform_1d_float
        module procedure :: envelope_transform_1d_double
    end interface
    interface conv
        module procedure :: conv_1d_float
        module procedure :: conv_2d_float
        module procedure :: conv_3d_float
        module procedure :: conv_1d_double
        module procedure :: conv_2d_double
        module procedure :: conv_3d_double
        module procedure :: conv_1d_complex
        module procedure :: conv_2d_complex
        module procedure :: conv_3d_complex
        module procedure :: conv_1d_dcomplex
        module procedure :: conv_2d_dcomplex
        module procedure :: conv_3d_dcomplex
    end interface conv
    interface fft_deriv
        module procedure :: fft_deriv_1d_float
        module procedure :: fft_deriv_1d_double
    end interface fft_deriv
    interface deconv
        module procedure :: deconv_1d_float
        module procedure :: deconv_2d_float
        module procedure :: deconv_3d_float
        module procedure :: deconv_1d_double
        module procedure :: deconv_2d_double
        module procedure :: deconv_3d_double
        module procedure :: deconv_1d_complex
        module procedure :: deconv_2d_complex
        module procedure :: deconv_3d_complex
        module procedure :: deconv_1d_dcomplex
        module procedure :: deconv_2d_dcomplex
        module procedure :: deconv_3d_dcomplex
    end interface deconv
    interface phase_shift
        module procedure :: phase_shift_float
        module procedure :: phase_shift_double
    end interface phase_shift
    interface time_shift
        module procedure :: time_shift_float
        module procedure :: time_shift_double
    end interface time_shift
    interface unwrap
        module procedure :: unwrap_float
        module procedure :: unwrap_double
    end interface unwrap
    public :: fourier_transform, inverse_fourier_transform
    public :: fft, ifft, fftd, ifftd
    public :: fftshift, fft_omega
    public :: hilbert, hilbert_transform
    public :: cosine_transform, inverse_cosine_transform
    public :: dct, idct
    public :: envelope, envelope_transform
    public :: next_power_2
    public :: next_power_235
    public :: next_power_2357
    public :: next_power_235711
    public :: phase_shift, time_shift
    public :: convd, conv
    public :: fft_deriv
    public :: deconv
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
!> Fast 1D discrete convolution
!
subroutine convfast_1d_float(lx, kx, x, ly, ky, y, lz, kz, z)
    integer, intent(in) :: lx, ly, lz
    integer, intent(in) :: kx, ky, kz
    real, dimension(0:), intent(in) :: x, y
    real, dimension(0:), intent(inout) :: z
    ! Bounds for index i.
    integer :: imin
    integer :: imax
    ! Variables that we expect to reside in registers.
    integer :: i, ilo, ihi, j, jlo, jhi, iz
    real :: sa, sb, xa, xb, ya, yb
    imin = kz - kx - ky
    imax = imin + lz - 1
    ! Off left: imin <= i <= -1
    ilo = imin
    ihi = min(-1, imax)
    ! for (i=ilo,iz=i-imin i<=ihi ++i,++iz)
    do i = ilo, ihi
        iz = i - imin
        z(iz) = 0.0
    end do
    ! Rolling on: 0 <= i <= lx-2 and 0 <= j <= i
    ilo = max(0, imin)
    ihi = min(lx - 2, imax)
    jlo = 0
    jhi = ilo
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2,jhi+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jhi) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
        jhi = jhi + 2
    end do
    if (i == ihi) then
        iz = i - imin
        jlo = 0
        jhi = i
        sa = 0.0
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Middle: lx-1 <= i <= ly-1 and 0 <= j <= lx-1
    ilo = max(lx - 1, imin)
    ihi = min(ly - 1, imax)
    jlo = 0
    jhi = lx - 1
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        if (j == jhi) then
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
    end do
    if (i == ihi) then
        iz = i - imin
        sa = 0.0
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Rolling off: ly <= i <= lx+ly-2 and i-ly+1 <= j <= lx-1
    ilo = max(ly, imin)
    ihi = min(lx + ly - 2, imax)
    jlo = ihi - ly + 1
    jhi = lx - 1
    ! for (i=ihi,iz=i-imin i>ilo i-=2,iz-=2,jlo-=2) {
    do i = ihi, ilo + 1, -2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jhi - 1)
        ! for (j=jhi j>jlo j-=2) {
        do j = jhi, jlo + 1, -2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
            yb = y(i - j + 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jlo) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz - 1) = sb
        jlo = jlo - 2
    end do
    if (i == ilo) then
        iz = i - imin
        jlo = i - ly + 1
        jhi = lx - 1
        sa = 0.0
        ! for (j=jhi j>=jlo --j)
        do j = jhi, jlo, -1
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Off right: lx+ly-1 <= i <= imax
    ilo = max(lx + ly - 1, imin)
    ihi = imax
    ! for (i=ilo,iz=i-imin i<=ihi ++i,++iz)
    do i = ilo, ihi
        iz = i - imin
        z(iz) = 0.0
    end do
end subroutine convfast_1d_float
!
!> Fast 1D discrete convolution
!
subroutine convsum_1d_float(lx, kx, x, ly, ky, y, lz, kz, z)
    integer, intent(in) :: lx, ly, lz
    integer, intent(in) :: kx, ky, kz
    real, dimension(0:), intent(in) :: x, y
    real, dimension(0:), intent(inout) :: z
    ! Bounds for index i.
    integer :: imin
    integer :: imax
    ! Variables that we expect to reside in registers.
    integer :: i, ilo, ihi, j, jlo, jhi, iz
    real :: sa, sb, xa, xb, ya, yb
    imin = kz - kx - ky
    imax = imin + lz - 1
    ! Rolling on: 0 <= i <= lx-2 and 0 <= j <= i
    ilo = max(0, imin)
    ihi = min(lx - 2, imax)
    jlo = 0
    jhi = ilo
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2,jhi+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = z(iz)
        sb = z(iz + 1)
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jhi) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
        jhi = jhi + 2
    end do
    if (i == ihi) then
        iz = i - imin
        jlo = 0
        jhi = i
        sa = z(iz)
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Middle: lx-1 <= i <= ly-1 and 0 <= j <= lx-1
    ilo = max(lx - 1, imin)
    ihi = min(ly - 1, imax)
    jlo = 0
    jhi = lx - 1
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = z(iz)
        sb = z(iz + 1)
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        if (j == jhi) then
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
    end do
    if (i == ihi) then
        iz = i - imin
        sa = z(iz)
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Rolling off: ly <= i <= lx+ly-2 and i-ly+1 <= j <= lx-1
    ilo = max(ly, imin)
    ihi = min(lx + ly - 2, imax)
    jlo = ihi - ly + 1
    jhi = lx - 1
    ! for (i=ihi,iz=i-imin i>ilo i-=2,iz-=2,jlo-=2) {
    do i = ihi, ilo + 1, -2
        iz = i - imin
        sa = z(iz)
        sb = z(iz - 1)
        yb = y(i - jhi - 1)
        ! for (j=jhi j>jlo j-=2) {
        do j = jhi, jlo + 1, -2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
            yb = y(i - j + 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jlo) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz - 1) = sb
        jlo = jlo - 2
    end do
    if (i == ilo) then
        iz = i - imin
        jlo = i - ly + 1
        jhi = lx - 1
        sa = z(iz)
        ! for (j=jhi j>=jlo --j)
        do j = jhi, jlo, -1
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
end subroutine convsum_1d_float
!
!> Discrete 1D convolution translated from convFast in minesjtk
!
subroutine discrete_conv_1d_float(x, y, z, kx, ky, kz)
    real, dimension(:), intent(in) :: x, y
    real, dimension(:), intent(inout) :: z
    integer, intent(in) :: kx, ky, kz
    integer :: lx, ly, lz
    lx = size(x, 1)
    ly = size(y, 1)
    lz = size(z, 1)
    z = 0.0
    if (lx > ly) then
        call convfast_1d_float( &
            ly, ky - 1, y, &
            lx, kx - 1, x, &
            lz, kz - 1, z)
    else
        call convfast_1d_float( &
            lx, kx - 1, x, &
            ly, ky - 1, y, &
            lz, kz - 1, z)
    end if
end subroutine discrete_conv_1d_float
!
!> Discrete 2D convolution translated from convFast in minesjtk
!
subroutine discrete_conv_2d_float(x, y, z, &
        kx1, kx2, &
        ky1, ky2, &
        kz1, kz2)
    real, dimension(:, :), intent(in) :: x, y
    real, dimension(:, :), intent(inout) :: z
    integer, intent(in) :: kx1, ky1, kz1
    integer, intent(in) :: kx2, ky2, kz2
    integer :: lx1, ly1, lz1
    integer :: lx2, ly2, lz2
    integer :: ilo2, ihi2, jlo2, jhi2
    integer :: i2, j2
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    z = 0.0
    ilo2 = (kz2 - 1) - (kx2 - 1) - (ky2 - 1)
    ihi2 = ilo2 + lz2 - 1
    if (lx1 > ly1) then
        ! !$omp parallel do private(i2,j2,jlo2,jhi2)
        do i2 = ilo2, ihi2
            jlo2 = max(0, i2 - ly2 + 1)
            jhi2 = min(lx2 - 1, i2)
            do j2 = jlo2, jhi2
                call convsum_1d_float( &
                    ly1, ky1 - 1, y(:, i2 - j2 + 1), &
                    lx1, kx1 - 1, x(:, j2 + 1), &
                    lz1, kz1 - 1, z(:, i2 - ilo2 + 1))
            end do
        end do
        ! !$omp end parallel do
    else
        ! !$omp parallel do private(i2,j2,jlo2,jhi2)
        do i2 = ilo2, ihi2
            jlo2 = max(0, i2 - ly2 + 1)
            jhi2 = min(lx2 - 1, i2)
            do j2 = jlo2, jhi2
                call convsum_1d_float( &
                    lx1, kx1 - 1, x(:, j2 + 1), &
                    ly1, ky1 - 1, y(:, i2 - j2 + 1), &
                    lz1, kz1 - 1, z(:, i2 - ilo2 + 1))
            end do
        end do
        ! !$omp end parallel do
    end if
end subroutine discrete_conv_2d_float
!
!> Discrete 3D convolution
!
subroutine discrete_conv_3d_float(x, y, z, &
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
    integer :: ilo2, ihi2, ilo3, ihi3
    integer :: jlo2, jhi2, jlo3, jhi3
    integer :: i2, j2, i3, j3
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    lx3 = size(x, 3)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    ly3 = size(y, 3)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    lz3 = size(z, 3)
    z = 0.0
    ilo2 = (kz2 - 1) - (kx2 - 1) - (ky2 - 1)
    ilo3 = (kz3 - 1) - (kx3 - 1) - (ky3 - 1)
    ihi2 = ilo2 + lz2 - 1
    ihi3 = ilo3 + lz3 - 1
    if (lx1 > ly1) then
        ! !$omp parallel do private(i2,i3,j2,j3,jlo2,jhi2,jlo3,jhi3)
        do i3 = ilo3, ihi3
            jlo3 = max(0, i3 - ly3 + 1)
            jhi3 = min(lx3 - 1, i3)
            do j3 = jlo3, jhi3
                do i2 = ilo2, ihi2
                    jlo2 = max(0, i2 - ly2 + 1)
                    jhi2 = min(lx2 - 1, i2)
                    do j2 = jlo2, jhi2
                        call convsum_1d_float( &
                            ly1, ky1 - 1, y(:, i2 - j2 + 1, i3 - j3 + 1), &
                            lx1, kx1 - 1, x(:, j2 + 1, j3 + 1), &
                            lz1, kz1 - 1, z(:, i2 - ilo2 + 1, i3 - ilo3 + 1))
                    end do
                end do
            end do
        end do
        ! !$omp end parallel do
    else
        ! !$omp parallel do private(i2,i3,j2,j3,jlo2,jhi2,jlo3,jhi3)
        do i3 = ilo3, ihi3
            jlo3 = max(0, i3 - ly3 + 1)
            jhi3 = min(lx3 - 1, i3)
            do j3 = jlo3, jhi3
                do i2 = ilo2, ihi2
                    jlo2 = max(0, i2 - ly2 + 1)
                    jhi2 = min(lx2 - 1, i2)
                    do j2 = jlo2, jhi2
                        call convsum_1d_float( &
                            lx1, kx1 - 1, x(:, j2 + 1, j3 + 1), &
                            ly1, ky1 - 1, y(:, i2 - j2 + 1, i3 - j3 + 1), &
                            lz1, kz1 - 1, z(:, i2 - ilo2 + 1, i3 - ilo3 + 1))
                    end do
                end do
            end do
        end do
        ! !$omp end parallel do
    end if
end subroutine discrete_conv_3d_float
function conv_1d_float(x, y, method) result(z)
    real, dimension(:), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    real, allocatable, dimension(:) :: z
    integer :: nx, ny, n
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx = size(x)
    ny = size(y)
    n = next_power_235(nx + ny - 1)
    allocate (z(1:n))
    z = real(ifft(fft(pad(x, [0, n - nx], ['const', 'const']))*fft(pad(y, [0, n - ny], ['const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, [1, nx + ny - 1], source=z(1:nx + ny - 1))
        case ('same')
            n = nint((ny + 1)/2.0)
            call alloc_array(z, [1, nx], source=z(n:n + nx - 1))
    end select
end function conv_1d_float
function conv_2d_float(x, y, method) result(z)
    real, dimension(:, :), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    real, allocatable, dimension(:, :) :: z
    integer :: nx1, nx2, ny1, ny2, n1, n2
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    n1 = next_power_235(nx1 + ny1 - 1)
    n2 = next_power_235(nx2 + ny2 - 1)
    allocate (z(1:n1, 1:n2))
    z = real(ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2], ['const', 'const', 'const', 'const']))* &
        fft(pad(y, [0, n1 - ny1, 0, n2 - ny2], ['const', 'const', 'const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, [1, nx1 + ny1 - 1, 1, nx2 + ny2 - 1], &
                source=z(1:nx1 + ny1 - 1, 1:nx2 + ny2 - 1))
        case ('same')
            n1 = nint((ny1 + 1)/2.0)
            n2 = nint((ny2 + 1)/2.0)
            call alloc_array(z, [1, nx1, 1, nx2], &
                source=z(n1:n1 + nx1 - 1, n2:n2 + nx2 - 1))
    end select
end function conv_2d_float
function conv_3d_float(x, y, method) result(z)
    real, dimension(:, :, :), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    real, allocatable, dimension(:, :, :) :: z
    integer :: nx1, nx2, nx3, ny1, ny2, ny3, n1, n2, n3
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    nx3 = size(x, 3)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    ny3 = size(y, 3)
    n1 = next_power_235(nx1 + ny1 - 1)
    n2 = next_power_235(nx2 + ny2 - 1)
    n3 = next_power_235(nx3 + ny3 - 1)
    allocate (z(1:n1, 1:n2, 1:n3))
    z = real(ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2, 0, n3 - nx3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))* &
        fft(pad(y, [0, n1 - ny1, 0, n2 - ny2, 0, n3 - ny3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, &
                [1, nx1 + ny1 - 1, 1, nx2 + ny2 - 1, 1, nx3 + ny3 - 1], &
                source=z(1:nx1 + ny1 - 1, 1:nx2 + ny2 - 1, 1:nx3 + ny3 - 1))
        case ('same')
            n1 = nint((ny1 + 1)/2.0)
            n2 = nint((ny2 + 1)/2.0)
            n3 = nint((ny3 + 1)/2.0)
            call alloc_array(z, [1, nx1, 1, nx2, 1, nx3], &
                source=z(n1:n1 + nx1 - 1, n2:n2 + nx2 - 1, n3:n3 + nx3 - 1))
    end select
end function conv_3d_float
!
!> Shift 1D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_1d_float(w) result(wr)
    real, dimension(:) :: w
    integer :: n1
    real, allocatable, dimension(:) :: wr
    n1 = size(w)
    wr = w
    wr = cshift(wr, nint(n1/2.0))
end function fftshift_1d_float
!
!> Shift 2D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_2d_float(w) result(wr)
    real, dimension(:, :) :: w
    integer :: n1, n2
    real, allocatable, dimension(:, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    wr = w
    wr = cshift(wr, nint(n1/2.0), dim=1)
    wr = cshift(wr, nint(n2/2.0), dim=2)
end function fftshift_2d_float
!
!> Shift 2D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_3d_float(w) result(wr)
    real, dimension(:, :, :) :: w
    integer :: n1, n2, n3
    real, allocatable, dimension(:, :, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    wr = w
    wr = cshift(wr, nint(n1/2.0), dim=1)
    wr = cshift(wr, nint(n2/2.0), dim=2)
    wr = cshift(wr, nint(n3/2.0), dim=3)
end function fftshift_3d_float
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_1d_float(x, y, maxlag, eps) result(z)
    real, dimension(:), intent(in) :: x, y
    integer, intent(in), optional :: maxlag
    real, intent(in), optional :: eps
    real, allocatable, dimension(:) :: z
    integer :: nx, ny, n, nlag
    complex, allocatable, dimension(:) :: a, b
    ! Dimensions
    nx = size(x)
    ny = size(y)
    if (present(maxlag)) then
        nlag = min(maxlag, max(nx, ny))
    else
        nlag = 0
    end if
    ! Pad to next power 235
    n = next_power_235(max(nx, ny) + nlag)
    ! Prepare arrays
    allocate (a(1:n))
    allocate (b(1:n))
    b = fft(pad(y, [0, n - ny], ['const', 'const']))
    a = fft(pad(x, [0, n - nx], ['const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        ! b = b + 1.0e-8*sum(b)/n
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n))
    if (present(maxlag)) then
        z = fftshift(real(ifft(a/b)))
        call alloc_array(z, [-nlag, nlag], &
            source=z(nint((n + 1)/2.0) - nlag:nint((n + 1)/2.0) + nlag))
    else
        z = real(ifft(a/b))
        call alloc_array(z, [1, nx], source=z(1:nx))
    end if
end function deconv_1d_float
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_2d_float(x, y, maxlag, eps) result(z)
    real, dimension(:, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    real, intent(in), optional :: eps
    real, allocatable, dimension(:, :) :: z
    integer :: n1x, n1y, n2x, n2y, n1, n2, nlag1, nlag2
    complex, allocatable, dimension(:, :) :: a, b
    ! Dimensions
    n1x = size(x, 1)
    n2x = size(x, 2)
    n1y = size(y, 1)
    n2y = size(y, 2)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(n1x, n1y))
        nlag2 = min(maxlag(2), max(n2x, n2y))
    else
        nlag1 = 0
        nlag2 = 0
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(n1x, n1y) + nlag1)
    n2 = next_power_235(max(n2x, n2y) + nlag2)
    ! Prepare arrays
    allocate (a(1:n1, 1:n2))
    allocate (b(1:n1, 1:n2))
    b = fft(pad(y, [0, n1 - n1y, 0, n2 - n2y], ['const', 'const', 'const', 'const']))
    a = fft(pad(x, [0, n1 - n1x, 0, n2 - n2x], ['const', 'const', 'const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        !b = b + 1.0e-8*sum(b)/(n1*n2)
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n1, 1:n2))
    if (present(maxlag)) then
        z = fftshift(real(ifft(a/b)))
        call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2], &
            source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
            nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2))
    else
        z = real(ifft(a/b))
        call alloc_array(z, [1, n1x, 1, n2x], source=z(1:n1x, 1:n2x))
    end if
end function deconv_2d_float
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_3d_float(x, y, maxlag, eps) result(z)
    real, dimension(:, :, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    real, intent(in), optional :: eps
    real, allocatable, dimension(:, :, :) :: z
    integer :: n1x, n1y, n2x, n2y, n3x, n3y, n1, n2, n3, nlag1, nlag2, nlag3
    complex, allocatable, dimension(:, :, :) :: a, b
    ! Dimensions
    n1x = size(x, 1)
    n2x = size(x, 2)
    n3x = size(x, 3)
    n1y = size(y, 1)
    n2y = size(y, 2)
    n3y = size(y, 3)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(n1x, n1y))
        nlag2 = min(maxlag(2), max(n2x, n2y))
        nlag3 = min(maxlag(3), max(n3x, n3y))
    else
        nlag1 = 0
        nlag2 = 0
        nlag3 = 0
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(n1x, n1y) + nlag1)
    n2 = next_power_235(max(n2x, n2y) + nlag2)
    n3 = next_power_235(max(n3x, n3y) + nlag3)
    ! Prepare arrays
    allocate (a(1:n1, 1:n2, 1:n3))
    allocate (b(1:n1, 1:n2, 1:n3))
    b = fft(pad(y, [0, n1 - n1y, 0, n2 - n2y, 0, n3 - n3y], ['const', 'const', 'const', 'const', 'const', 'const']))
    a = fft(pad(x, [0, n1 - n1x, 0, n2 - n2x, 0, n3 - n3x], ['const', 'const', 'const', 'const', 'const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        !b = b + 1.0e-8*sum(b)/(n1*n2)
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n1, 1:n2, 1:n3))
    if (present(maxlag)) then
        z = fftshift(real(ifft(a/b)))
        call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2, -nlag3, nlag3], &
            source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
            nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2, &
            nint((n3 + 1)/2.0) - nlag3:nint((n3 + 1)/2.0) + nlag3))
    else
        z = real(ifft(a/b))
        call alloc_array(z, [1, n1x, 1, n2x, 1, n3x], source=z(1:n1x, 1:n2x, 1:n3x))
    end if
end function deconv_3d_float
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
!> Fast 1D discrete convolution
!
subroutine convfast_1d_double(lx, kx, x, ly, ky, y, lz, kz, z)
    integer, intent(in) :: lx, ly, lz
    integer, intent(in) :: kx, ky, kz
    double precision, dimension(0:), intent(in) :: x, y
    double precision, dimension(0:), intent(inout) :: z
    ! Bounds for index i.
    integer :: imin
    integer :: imax
    ! Variables that we expect to reside in registers.
    integer :: i, ilo, ihi, j, jlo, jhi, iz
    double precision :: sa, sb, xa, xb, ya, yb
    imin = kz - kx - ky
    imax = imin + lz - 1
    ! Off left: imin <= i <= -1
    ilo = imin
    ihi = min(-1, imax)
    ! for (i=ilo,iz=i-imin i<=ihi ++i,++iz)
    do i = ilo, ihi
        iz = i - imin
        z(iz) = 0.0
    end do
    ! Rolling on: 0 <= i <= lx-2 and 0 <= j <= i
    ilo = max(0, imin)
    ihi = min(lx - 2, imax)
    jlo = 0
    jhi = ilo
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2,jhi+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jhi) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
        jhi = jhi + 2
    end do
    if (i == ihi) then
        iz = i - imin
        jlo = 0
        jhi = i
        sa = 0.0
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Middle: lx-1 <= i <= ly-1 and 0 <= j <= lx-1
    ilo = max(lx - 1, imin)
    ihi = min(ly - 1, imax)
    jlo = 0
    jhi = lx - 1
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        if (j == jhi) then
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
    end do
    if (i == ihi) then
        iz = i - imin
        sa = 0.0
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Rolling off: ly <= i <= lx+ly-2 and i-ly+1 <= j <= lx-1
    ilo = max(ly, imin)
    ihi = min(lx + ly - 2, imax)
    jlo = ihi - ly + 1
    jhi = lx - 1
    ! for (i=ihi,iz=i-imin i>ilo i-=2,iz-=2,jlo-=2) {
    do i = ihi, ilo + 1, -2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jhi - 1)
        ! for (j=jhi j>jlo j-=2) {
        do j = jhi, jlo + 1, -2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
            yb = y(i - j + 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jlo) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz - 1) = sb
        jlo = jlo - 2
    end do
    if (i == ilo) then
        iz = i - imin
        jlo = i - ly + 1
        jhi = lx - 1
        sa = 0.0
        ! for (j=jhi j>=jlo --j)
        do j = jhi, jlo, -1
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Off right: lx+ly-1 <= i <= imax
    ilo = max(lx + ly - 1, imin)
    ihi = imax
    ! for (i=ilo,iz=i-imin i<=ihi ++i,++iz)
    do i = ilo, ihi
        iz = i - imin
        z(iz) = 0.0
    end do
end subroutine convfast_1d_double
!
!> Fast 1D discrete convolution
!
subroutine convsum_1d_double(lx, kx, x, ly, ky, y, lz, kz, z)
    integer, intent(in) :: lx, ly, lz
    integer, intent(in) :: kx, ky, kz
    double precision, dimension(0:), intent(in) :: x, y
    double precision, dimension(0:), intent(inout) :: z
    ! Bounds for index i.
    integer :: imin
    integer :: imax
    ! Variables that we expect to reside in registers.
    integer :: i, ilo, ihi, j, jlo, jhi, iz
    double precision :: sa, sb, xa, xb, ya, yb
    imin = kz - kx - ky
    imax = imin + lz - 1
    ! Rolling on: 0 <= i <= lx-2 and 0 <= j <= i
    ilo = max(0, imin)
    ihi = min(lx - 2, imax)
    jlo = 0
    jhi = ilo
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2,jhi+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = z(iz)
        sb = z(iz + 1)
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jhi) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
        jhi = jhi + 2
    end do
    if (i == ihi) then
        iz = i - imin
        jlo = 0
        jhi = i
        sa = z(iz)
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Middle: lx-1 <= i <= ly-1 and 0 <= j <= lx-1
    ilo = max(lx - 1, imin)
    ihi = min(ly - 1, imax)
    jlo = 0
    jhi = lx - 1
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = z(iz)
        sb = z(iz + 1)
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        if (j == jhi) then
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
    end do
    if (i == ihi) then
        iz = i - imin
        sa = z(iz)
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Rolling off: ly <= i <= lx+ly-2 and i-ly+1 <= j <= lx-1
    ilo = max(ly, imin)
    ihi = min(lx + ly - 2, imax)
    jlo = ihi - ly + 1
    jhi = lx - 1
    ! for (i=ihi,iz=i-imin i>ilo i-=2,iz-=2,jlo-=2) {
    do i = ihi, ilo + 1, -2
        iz = i - imin
        sa = z(iz)
        sb = z(iz - 1)
        yb = y(i - jhi - 1)
        ! for (j=jhi j>jlo j-=2) {
        do j = jhi, jlo + 1, -2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
            yb = y(i - j + 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jlo) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz - 1) = sb
        jlo = jlo - 2
    end do
    if (i == ilo) then
        iz = i - imin
        jlo = i - ly + 1
        jhi = lx - 1
        sa = z(iz)
        ! for (j=jhi j>=jlo --j)
        do j = jhi, jlo, -1
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
end subroutine convsum_1d_double
!
!> Discrete 1D convolution translated from convFast in minesjtk
!
subroutine discrete_conv_1d_double(x, y, z, kx, ky, kz)
    double precision, dimension(:), intent(in) :: x, y
    double precision, dimension(:), intent(inout) :: z
    integer, intent(in) :: kx, ky, kz
    integer :: lx, ly, lz
    lx = size(x, 1)
    ly = size(y, 1)
    lz = size(z, 1)
    z = 0.0
    if (lx > ly) then
        call convfast_1d_double( &
            ly, ky - 1, y, &
            lx, kx - 1, x, &
            lz, kz - 1, z)
    else
        call convfast_1d_double( &
            lx, kx - 1, x, &
            ly, ky - 1, y, &
            lz, kz - 1, z)
    end if
end subroutine discrete_conv_1d_double
!
!> Discrete 2D convolution translated from convFast in minesjtk
!
subroutine discrete_conv_2d_double(x, y, z, &
        kx1, kx2, &
        ky1, ky2, &
        kz1, kz2)
    double precision, dimension(:, :), intent(in) :: x, y
    double precision, dimension(:, :), intent(inout) :: z
    integer, intent(in) :: kx1, ky1, kz1
    integer, intent(in) :: kx2, ky2, kz2
    integer :: lx1, ly1, lz1
    integer :: lx2, ly2, lz2
    integer :: ilo2, ihi2, jlo2, jhi2
    integer :: i2, j2
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    z = 0.0
    ilo2 = (kz2 - 1) - (kx2 - 1) - (ky2 - 1)
    ihi2 = ilo2 + lz2 - 1
    if (lx1 > ly1) then
        ! !$omp parallel do private(i2,j2,jlo2,jhi2)
        do i2 = ilo2, ihi2
            jlo2 = max(0, i2 - ly2 + 1)
            jhi2 = min(lx2 - 1, i2)
            do j2 = jlo2, jhi2
                call convsum_1d_double( &
                    ly1, ky1 - 1, y(:, i2 - j2 + 1), &
                    lx1, kx1 - 1, x(:, j2 + 1), &
                    lz1, kz1 - 1, z(:, i2 - ilo2 + 1))
            end do
        end do
        ! !$omp end parallel do
    else
        ! !$omp parallel do private(i2,j2,jlo2,jhi2)
        do i2 = ilo2, ihi2
            jlo2 = max(0, i2 - ly2 + 1)
            jhi2 = min(lx2 - 1, i2)
            do j2 = jlo2, jhi2
                call convsum_1d_double( &
                    lx1, kx1 - 1, x(:, j2 + 1), &
                    ly1, ky1 - 1, y(:, i2 - j2 + 1), &
                    lz1, kz1 - 1, z(:, i2 - ilo2 + 1))
            end do
        end do
        ! !$omp end parallel do
    end if
end subroutine discrete_conv_2d_double
!
!> Discrete 3D convolution
!
subroutine discrete_conv_3d_double(x, y, z, &
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
    integer :: ilo2, ihi2, ilo3, ihi3
    integer :: jlo2, jhi2, jlo3, jhi3
    integer :: i2, j2, i3, j3
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    lx3 = size(x, 3)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    ly3 = size(y, 3)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    lz3 = size(z, 3)
    z = 0.0
    ilo2 = (kz2 - 1) - (kx2 - 1) - (ky2 - 1)
    ilo3 = (kz3 - 1) - (kx3 - 1) - (ky3 - 1)
    ihi2 = ilo2 + lz2 - 1
    ihi3 = ilo3 + lz3 - 1
    if (lx1 > ly1) then
        ! !$omp parallel do private(i2,i3,j2,j3,jlo2,jhi2,jlo3,jhi3)
        do i3 = ilo3, ihi3
            jlo3 = max(0, i3 - ly3 + 1)
            jhi3 = min(lx3 - 1, i3)
            do j3 = jlo3, jhi3
                do i2 = ilo2, ihi2
                    jlo2 = max(0, i2 - ly2 + 1)
                    jhi2 = min(lx2 - 1, i2)
                    do j2 = jlo2, jhi2
                        call convsum_1d_double( &
                            ly1, ky1 - 1, y(:, i2 - j2 + 1, i3 - j3 + 1), &
                            lx1, kx1 - 1, x(:, j2 + 1, j3 + 1), &
                            lz1, kz1 - 1, z(:, i2 - ilo2 + 1, i3 - ilo3 + 1))
                    end do
                end do
            end do
        end do
        ! !$omp end parallel do
    else
        ! !$omp parallel do private(i2,i3,j2,j3,jlo2,jhi2,jlo3,jhi3)
        do i3 = ilo3, ihi3
            jlo3 = max(0, i3 - ly3 + 1)
            jhi3 = min(lx3 - 1, i3)
            do j3 = jlo3, jhi3
                do i2 = ilo2, ihi2
                    jlo2 = max(0, i2 - ly2 + 1)
                    jhi2 = min(lx2 - 1, i2)
                    do j2 = jlo2, jhi2
                        call convsum_1d_double( &
                            lx1, kx1 - 1, x(:, j2 + 1, j3 + 1), &
                            ly1, ky1 - 1, y(:, i2 - j2 + 1, i3 - j3 + 1), &
                            lz1, kz1 - 1, z(:, i2 - ilo2 + 1, i3 - ilo3 + 1))
                    end do
                end do
            end do
        end do
        ! !$omp end parallel do
    end if
end subroutine discrete_conv_3d_double
function conv_1d_double(x, y, method) result(z)
    double precision, dimension(:), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    double precision, allocatable, dimension(:) :: z
    integer :: nx, ny, n
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx = size(x)
    ny = size(y)
    n = next_power_235(nx + ny - 1)
    allocate (z(1:n))
    z = dble(ifft(fft(pad(x, [0, n - nx], ['const', 'const']))*fft(pad(y, [0, n - ny], ['const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, [1, nx + ny - 1], source=z(1:nx + ny - 1))
        case ('same')
            n = nint((ny + 1)/2.0)
            call alloc_array(z, [1, nx], source=z(n:n + nx - 1))
    end select
end function conv_1d_double
function conv_2d_double(x, y, method) result(z)
    double precision, dimension(:, :), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    double precision, allocatable, dimension(:, :) :: z
    integer :: nx1, nx2, ny1, ny2, n1, n2
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    n1 = next_power_235(nx1 + ny1 - 1)
    n2 = next_power_235(nx2 + ny2 - 1)
    allocate (z(1:n1, 1:n2))
    z = dble(ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2], ['const', 'const', 'const', 'const']))* &
        fft(pad(y, [0, n1 - ny1, 0, n2 - ny2], ['const', 'const', 'const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, [1, nx1 + ny1 - 1, 1, nx2 + ny2 - 1], &
                source=z(1:nx1 + ny1 - 1, 1:nx2 + ny2 - 1))
        case ('same')
            n1 = nint((ny1 + 1)/2.0)
            n2 = nint((ny2 + 1)/2.0)
            call alloc_array(z, [1, nx1, 1, nx2], &
                source=z(n1:n1 + nx1 - 1, n2:n2 + nx2 - 1))
    end select
end function conv_2d_double
function conv_3d_double(x, y, method) result(z)
    double precision, dimension(:, :, :), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    double precision, allocatable, dimension(:, :, :) :: z
    integer :: nx1, nx2, nx3, ny1, ny2, ny3, n1, n2, n3
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    nx3 = size(x, 3)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    ny3 = size(y, 3)
    n1 = next_power_235(nx1 + ny1 - 1)
    n2 = next_power_235(nx2 + ny2 - 1)
    n3 = next_power_235(nx3 + ny3 - 1)
    allocate (z(1:n1, 1:n2, 1:n3))
    z = dble(ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2, 0, n3 - nx3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))* &
        fft(pad(y, [0, n1 - ny1, 0, n2 - ny2, 0, n3 - ny3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, &
                [1, nx1 + ny1 - 1, 1, nx2 + ny2 - 1, 1, nx3 + ny3 - 1], &
                source=z(1:nx1 + ny1 - 1, 1:nx2 + ny2 - 1, 1:nx3 + ny3 - 1))
        case ('same')
            n1 = nint((ny1 + 1)/2.0)
            n2 = nint((ny2 + 1)/2.0)
            n3 = nint((ny3 + 1)/2.0)
            call alloc_array(z, [1, nx1, 1, nx2, 1, nx3], &
                source=z(n1:n1 + nx1 - 1, n2:n2 + nx2 - 1, n3:n3 + nx3 - 1))
    end select
end function conv_3d_double
!
!> Shift 1D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_1d_double(w) result(wr)
    double precision, dimension(:) :: w
    integer :: n1
    double precision, allocatable, dimension(:) :: wr
    n1 = size(w)
    wr = w
    wr = cshift(wr, nint(n1/2.0))
end function fftshift_1d_double
!
!> Shift 2D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_2d_double(w) result(wr)
    double precision, dimension(:, :) :: w
    integer :: n1, n2
    double precision, allocatable, dimension(:, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    wr = w
    wr = cshift(wr, nint(n1/2.0), dim=1)
    wr = cshift(wr, nint(n2/2.0), dim=2)
end function fftshift_2d_double
!
!> Shift 2D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_3d_double(w) result(wr)
    double precision, dimension(:, :, :) :: w
    integer :: n1, n2, n3
    double precision, allocatable, dimension(:, :, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    wr = w
    wr = cshift(wr, nint(n1/2.0), dim=1)
    wr = cshift(wr, nint(n2/2.0), dim=2)
    wr = cshift(wr, nint(n3/2.0), dim=3)
end function fftshift_3d_double
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_1d_double(x, y, maxlag, eps) result(z)
    double precision, dimension(:), intent(in) :: x, y
    integer, intent(in), optional :: maxlag
    double precision, intent(in), optional :: eps
    double precision, allocatable, dimension(:) :: z
    integer :: nx, ny, n, nlag
    double complex, allocatable, dimension(:) :: a, b
    ! Dimensions
    nx = size(x)
    ny = size(y)
    if (present(maxlag)) then
        nlag = min(maxlag, max(nx, ny))
    else
        nlag = 0
    end if
    ! Pad to next power 235
    n = next_power_235(max(nx, ny) + nlag)
    ! Prepare arrays
    allocate (a(1:n))
    allocate (b(1:n))
    b = fft(pad(y, [0, n - ny], ['const', 'const']))
    a = fft(pad(x, [0, n - nx], ['const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        ! b = b + 1.0e-8*sum(b)/n
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n))
    if (present(maxlag)) then
        z = fftshift(dble(ifft(a/b)))
        call alloc_array(z, [-nlag, nlag], &
            source=z(nint((n + 1)/2.0) - nlag:nint((n + 1)/2.0) + nlag))
    else
        z = dble(ifft(a/b))
        call alloc_array(z, [1, nx], source=z(1:nx))
    end if
end function deconv_1d_double
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_2d_double(x, y, maxlag, eps) result(z)
    double precision, dimension(:, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    double precision, intent(in), optional :: eps
    double precision, allocatable, dimension(:, :) :: z
    integer :: n1x, n1y, n2x, n2y, n1, n2, nlag1, nlag2
    double complex, allocatable, dimension(:, :) :: a, b
    ! Dimensions
    n1x = size(x, 1)
    n2x = size(x, 2)
    n1y = size(y, 1)
    n2y = size(y, 2)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(n1x, n1y))
        nlag2 = min(maxlag(2), max(n2x, n2y))
    else
        nlag1 = 0
        nlag2 = 0
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(n1x, n1y) + nlag1)
    n2 = next_power_235(max(n2x, n2y) + nlag2)
    ! Prepare arrays
    allocate (a(1:n1, 1:n2))
    allocate (b(1:n1, 1:n2))
    b = fft(pad(y, [0, n1 - n1y, 0, n2 - n2y], ['const', 'const', 'const', 'const']))
    a = fft(pad(x, [0, n1 - n1x, 0, n2 - n2x], ['const', 'const', 'const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        !b = b + 1.0e-8*sum(b)/(n1*n2)
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n1, 1:n2))
    if (present(maxlag)) then
        z = fftshift(dble(ifft(a/b)))
        call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2], &
            source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
            nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2))
    else
        z = dble(ifft(a/b))
        call alloc_array(z, [1, n1x, 1, n2x], source=z(1:n1x, 1:n2x))
    end if
end function deconv_2d_double
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_3d_double(x, y, maxlag, eps) result(z)
    double precision, dimension(:, :, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    double precision, intent(in), optional :: eps
    double precision, allocatable, dimension(:, :, :) :: z
    integer :: n1x, n1y, n2x, n2y, n3x, n3y, n1, n2, n3, nlag1, nlag2, nlag3
    double complex, allocatable, dimension(:, :, :) :: a, b
    ! Dimensions
    n1x = size(x, 1)
    n2x = size(x, 2)
    n3x = size(x, 3)
    n1y = size(y, 1)
    n2y = size(y, 2)
    n3y = size(y, 3)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(n1x, n1y))
        nlag2 = min(maxlag(2), max(n2x, n2y))
        nlag3 = min(maxlag(3), max(n3x, n3y))
    else
        nlag1 = 0
        nlag2 = 0
        nlag3 = 0
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(n1x, n1y) + nlag1)
    n2 = next_power_235(max(n2x, n2y) + nlag2)
    n3 = next_power_235(max(n3x, n3y) + nlag3)
    ! Prepare arrays
    allocate (a(1:n1, 1:n2, 1:n3))
    allocate (b(1:n1, 1:n2, 1:n3))
    b = fft(pad(y, [0, n1 - n1y, 0, n2 - n2y, 0, n3 - n3y], ['const', 'const', 'const', 'const', 'const', 'const']))
    a = fft(pad(x, [0, n1 - n1x, 0, n2 - n2x, 0, n3 - n3x], ['const', 'const', 'const', 'const', 'const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        !b = b + 1.0e-8*sum(b)/(n1*n2)
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n1, 1:n2, 1:n3))
    if (present(maxlag)) then
        z = fftshift(dble(ifft(a/b)))
        call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2, -nlag3, nlag3], &
            source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
            nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2, &
            nint((n3 + 1)/2.0) - nlag3:nint((n3 + 1)/2.0) + nlag3))
    else
        z = dble(ifft(a/b))
        call alloc_array(z, [1, n1x, 1, n2x, 1, n3x], source=z(1:n1x, 1:n2x, 1:n3x))
    end if
end function deconv_3d_double
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
!> Fast 1D discrete convolution
!
subroutine convfast_1d_complex(lx, kx, x, ly, ky, y, lz, kz, z)
    integer, intent(in) :: lx, ly, lz
    integer, intent(in) :: kx, ky, kz
    complex, dimension(0:), intent(in) :: x, y
    complex, dimension(0:), intent(inout) :: z
    ! Bounds for index i.
    integer :: imin
    integer :: imax
    ! Variables that we expect to reside in registers.
    integer :: i, ilo, ihi, j, jlo, jhi, iz
    complex :: sa, sb, xa, xb, ya, yb
    imin = kz - kx - ky
    imax = imin + lz - 1
    ! Off left: imin <= i <= -1
    ilo = imin
    ihi = min(-1, imax)
    ! for (i=ilo,iz=i-imin i<=ihi ++i,++iz)
    do i = ilo, ihi
        iz = i - imin
        z(iz) = 0.0
    end do
    ! Rolling on: 0 <= i <= lx-2 and 0 <= j <= i
    ilo = max(0, imin)
    ihi = min(lx - 2, imax)
    jlo = 0
    jhi = ilo
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2,jhi+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jhi) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
        jhi = jhi + 2
    end do
    if (i == ihi) then
        iz = i - imin
        jlo = 0
        jhi = i
        sa = 0.0
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Middle: lx-1 <= i <= ly-1 and 0 <= j <= lx-1
    ilo = max(lx - 1, imin)
    ihi = min(ly - 1, imax)
    jlo = 0
    jhi = lx - 1
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        if (j == jhi) then
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
    end do
    if (i == ihi) then
        iz = i - imin
        sa = 0.0
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Rolling off: ly <= i <= lx+ly-2 and i-ly+1 <= j <= lx-1
    ilo = max(ly, imin)
    ihi = min(lx + ly - 2, imax)
    jlo = ihi - ly + 1
    jhi = lx - 1
    ! for (i=ihi,iz=i-imin i>ilo i-=2,iz-=2,jlo-=2) {
    do i = ihi, ilo + 1, -2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jhi - 1)
        ! for (j=jhi j>jlo j-=2) {
        do j = jhi, jlo + 1, -2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
            yb = y(i - j + 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jlo) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz - 1) = sb
        jlo = jlo - 2
    end do
    if (i == ilo) then
        iz = i - imin
        jlo = i - ly + 1
        jhi = lx - 1
        sa = 0.0
        ! for (j=jhi j>=jlo --j)
        do j = jhi, jlo, -1
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Off right: lx+ly-1 <= i <= imax
    ilo = max(lx + ly - 1, imin)
    ihi = imax
    ! for (i=ilo,iz=i-imin i<=ihi ++i,++iz)
    do i = ilo, ihi
        iz = i - imin
        z(iz) = 0.0
    end do
end subroutine convfast_1d_complex
!
!> Fast 1D discrete convolution
!
subroutine convsum_1d_complex(lx, kx, x, ly, ky, y, lz, kz, z)
    integer, intent(in) :: lx, ly, lz
    integer, intent(in) :: kx, ky, kz
    complex, dimension(0:), intent(in) :: x, y
    complex, dimension(0:), intent(inout) :: z
    ! Bounds for index i.
    integer :: imin
    integer :: imax
    ! Variables that we expect to reside in registers.
    integer :: i, ilo, ihi, j, jlo, jhi, iz
    complex :: sa, sb, xa, xb, ya, yb
    imin = kz - kx - ky
    imax = imin + lz - 1
    ! Rolling on: 0 <= i <= lx-2 and 0 <= j <= i
    ilo = max(0, imin)
    ihi = min(lx - 2, imax)
    jlo = 0
    jhi = ilo
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2,jhi+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = z(iz)
        sb = z(iz + 1)
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jhi) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
        jhi = jhi + 2
    end do
    if (i == ihi) then
        iz = i - imin
        jlo = 0
        jhi = i
        sa = z(iz)
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Middle: lx-1 <= i <= ly-1 and 0 <= j <= lx-1
    ilo = max(lx - 1, imin)
    ihi = min(ly - 1, imax)
    jlo = 0
    jhi = lx - 1
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = z(iz)
        sb = z(iz + 1)
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        if (j == jhi) then
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
    end do
    if (i == ihi) then
        iz = i - imin
        sa = z(iz)
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Rolling off: ly <= i <= lx+ly-2 and i-ly+1 <= j <= lx-1
    ilo = max(ly, imin)
    ihi = min(lx + ly - 2, imax)
    jlo = ihi - ly + 1
    jhi = lx - 1
    ! for (i=ihi,iz=i-imin i>ilo i-=2,iz-=2,jlo-=2) {
    do i = ihi, ilo + 1, -2
        iz = i - imin
        sa = z(iz)
        sb = z(iz - 1)
        yb = y(i - jhi - 1)
        ! for (j=jhi j>jlo j-=2) {
        do j = jhi, jlo + 1, -2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
            yb = y(i - j + 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jlo) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz - 1) = sb
        jlo = jlo - 2
    end do
    if (i == ilo) then
        iz = i - imin
        jlo = i - ly + 1
        jhi = lx - 1
        sa = z(iz)
        ! for (j=jhi j>=jlo --j)
        do j = jhi, jlo, -1
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
end subroutine convsum_1d_complex
!
!> Discrete 1D convolution translated from convFast in minesjtk
!
subroutine discrete_conv_1d_complex(x, y, z, kx, ky, kz)
    complex, dimension(:), intent(in) :: x, y
    complex, dimension(:), intent(inout) :: z
    integer, intent(in) :: kx, ky, kz
    integer :: lx, ly, lz
    lx = size(x, 1)
    ly = size(y, 1)
    lz = size(z, 1)
    z = 0.0
    if (lx > ly) then
        call convfast_1d_complex( &
            ly, ky - 1, y, &
            lx, kx - 1, x, &
            lz, kz - 1, z)
    else
        call convfast_1d_complex( &
            lx, kx - 1, x, &
            ly, ky - 1, y, &
            lz, kz - 1, z)
    end if
end subroutine discrete_conv_1d_complex
!
!> Discrete 2D convolution translated from convFast in minesjtk
!
subroutine discrete_conv_2d_complex(x, y, z, &
        kx1, kx2, &
        ky1, ky2, &
        kz1, kz2)
    complex, dimension(:, :), intent(in) :: x, y
    complex, dimension(:, :), intent(inout) :: z
    integer, intent(in) :: kx1, ky1, kz1
    integer, intent(in) :: kx2, ky2, kz2
    integer :: lx1, ly1, lz1
    integer :: lx2, ly2, lz2
    integer :: ilo2, ihi2, jlo2, jhi2
    integer :: i2, j2
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    z = 0.0
    ilo2 = (kz2 - 1) - (kx2 - 1) - (ky2 - 1)
    ihi2 = ilo2 + lz2 - 1
    if (lx1 > ly1) then
        ! !$omp parallel do private(i2,j2,jlo2,jhi2)
        do i2 = ilo2, ihi2
            jlo2 = max(0, i2 - ly2 + 1)
            jhi2 = min(lx2 - 1, i2)
            do j2 = jlo2, jhi2
                call convsum_1d_complex( &
                    ly1, ky1 - 1, y(:, i2 - j2 + 1), &
                    lx1, kx1 - 1, x(:, j2 + 1), &
                    lz1, kz1 - 1, z(:, i2 - ilo2 + 1))
            end do
        end do
        ! !$omp end parallel do
    else
        ! !$omp parallel do private(i2,j2,jlo2,jhi2)
        do i2 = ilo2, ihi2
            jlo2 = max(0, i2 - ly2 + 1)
            jhi2 = min(lx2 - 1, i2)
            do j2 = jlo2, jhi2
                call convsum_1d_complex( &
                    lx1, kx1 - 1, x(:, j2 + 1), &
                    ly1, ky1 - 1, y(:, i2 - j2 + 1), &
                    lz1, kz1 - 1, z(:, i2 - ilo2 + 1))
            end do
        end do
        ! !$omp end parallel do
    end if
end subroutine discrete_conv_2d_complex
!
!> Discrete 3D convolution
!
subroutine discrete_conv_3d_complex(x, y, z, &
        kx1, kx2, kx3, &
        ky1, ky2, ky3, &
        kz1, kz2, kz3)
    integer, intent(in) :: kx1, ky1, kz1
    integer, intent(in) :: kx2, ky2, kz2
    integer, intent(in) :: kx3, ky3, kz3
    complex, dimension(:, :, :), intent(in) :: x, y
    complex, dimension(:, :, :), intent(inout) :: z
    integer :: lx1, ly1, lz1
    integer :: lx2, ly2, lz2
    integer :: lx3, ly3, lz3
    integer :: ilo2, ihi2, ilo3, ihi3
    integer :: jlo2, jhi2, jlo3, jhi3
    integer :: i2, j2, i3, j3
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    lx3 = size(x, 3)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    ly3 = size(y, 3)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    lz3 = size(z, 3)
    z = 0.0
    ilo2 = (kz2 - 1) - (kx2 - 1) - (ky2 - 1)
    ilo3 = (kz3 - 1) - (kx3 - 1) - (ky3 - 1)
    ihi2 = ilo2 + lz2 - 1
    ihi3 = ilo3 + lz3 - 1
    if (lx1 > ly1) then
        ! !$omp parallel do private(i2,i3,j2,j3,jlo2,jhi2,jlo3,jhi3)
        do i3 = ilo3, ihi3
            jlo3 = max(0, i3 - ly3 + 1)
            jhi3 = min(lx3 - 1, i3)
            do j3 = jlo3, jhi3
                do i2 = ilo2, ihi2
                    jlo2 = max(0, i2 - ly2 + 1)
                    jhi2 = min(lx2 - 1, i2)
                    do j2 = jlo2, jhi2
                        call convsum_1d_complex( &
                            ly1, ky1 - 1, y(:, i2 - j2 + 1, i3 - j3 + 1), &
                            lx1, kx1 - 1, x(:, j2 + 1, j3 + 1), &
                            lz1, kz1 - 1, z(:, i2 - ilo2 + 1, i3 - ilo3 + 1))
                    end do
                end do
            end do
        end do
        ! !$omp end parallel do
    else
        ! !$omp parallel do private(i2,i3,j2,j3,jlo2,jhi2,jlo3,jhi3)
        do i3 = ilo3, ihi3
            jlo3 = max(0, i3 - ly3 + 1)
            jhi3 = min(lx3 - 1, i3)
            do j3 = jlo3, jhi3
                do i2 = ilo2, ihi2
                    jlo2 = max(0, i2 - ly2 + 1)
                    jhi2 = min(lx2 - 1, i2)
                    do j2 = jlo2, jhi2
                        call convsum_1d_complex( &
                            lx1, kx1 - 1, x(:, j2 + 1, j3 + 1), &
                            ly1, ky1 - 1, y(:, i2 - j2 + 1, i3 - j3 + 1), &
                            lz1, kz1 - 1, z(:, i2 - ilo2 + 1, i3 - ilo3 + 1))
                    end do
                end do
            end do
        end do
        ! !$omp end parallel do
    end if
end subroutine discrete_conv_3d_complex
function conv_1d_complex(x, y, method) result(z)
    complex, dimension(:), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    complex, allocatable, dimension(:) :: z
    integer :: nx, ny, n
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx = size(x)
    ny = size(y)
    n = next_power_235(nx + ny - 1)
    allocate (z(1:n))
    z = (ifft(fft(pad(x, [0, n - nx], ['const', 'const']))*fft(pad(y, [0, n - ny], ['const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, [1, nx + ny - 1], source=z(1:nx + ny - 1))
        case ('same')
            n = nint((ny + 1)/2.0)
            call alloc_array(z, [1, nx], source=z(n:n + nx - 1))
    end select
end function conv_1d_complex
function conv_2d_complex(x, y, method) result(z)
    complex, dimension(:, :), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    complex, allocatable, dimension(:, :) :: z
    integer :: nx1, nx2, ny1, ny2, n1, n2
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    n1 = next_power_235(nx1 + ny1 - 1)
    n2 = next_power_235(nx2 + ny2 - 1)
    allocate (z(1:n1, 1:n2))
    z = (ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2], ['const', 'const', 'const', 'const']))* &
        fft(pad(y, [0, n1 - ny1, 0, n2 - ny2], ['const', 'const', 'const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, [1, nx1 + ny1 - 1, 1, nx2 + ny2 - 1], &
                source=z(1:nx1 + ny1 - 1, 1:nx2 + ny2 - 1))
        case ('same')
            n1 = nint((ny1 + 1)/2.0)
            n2 = nint((ny2 + 1)/2.0)
            call alloc_array(z, [1, nx1, 1, nx2], &
                source=z(n1:n1 + nx1 - 1, n2:n2 + nx2 - 1))
    end select
end function conv_2d_complex
function conv_3d_complex(x, y, method) result(z)
    complex, dimension(:, :, :), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    complex, allocatable, dimension(:, :, :) :: z
    integer :: nx1, nx2, nx3, ny1, ny2, ny3, n1, n2, n3
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    nx3 = size(x, 3)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    ny3 = size(y, 3)
    n1 = next_power_235(nx1 + ny1 - 1)
    n2 = next_power_235(nx2 + ny2 - 1)
    n3 = next_power_235(nx3 + ny3 - 1)
    allocate (z(1:n1, 1:n2, 1:n3))
    z = (ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2, 0, n3 - nx3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))* &
        fft(pad(y, [0, n1 - ny1, 0, n2 - ny2, 0, n3 - ny3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, &
                [1, nx1 + ny1 - 1, 1, nx2 + ny2 - 1, 1, nx3 + ny3 - 1], &
                source=z(1:nx1 + ny1 - 1, 1:nx2 + ny2 - 1, 1:nx3 + ny3 - 1))
        case ('same')
            n1 = nint((ny1 + 1)/2.0)
            n2 = nint((ny2 + 1)/2.0)
            n3 = nint((ny3 + 1)/2.0)
            call alloc_array(z, [1, nx1, 1, nx2, 1, nx3], &
                source=z(n1:n1 + nx1 - 1, n2:n2 + nx2 - 1, n3:n3 + nx3 - 1))
    end select
end function conv_3d_complex
!
!> Shift 1D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_1d_complex(w) result(wr)
    complex, dimension(:) :: w
    integer :: n1
    complex, allocatable, dimension(:) :: wr
    n1 = size(w)
    wr = w
    wr = cshift(wr, nint(n1/2.0))
end function fftshift_1d_complex
!
!> Shift 2D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_2d_complex(w) result(wr)
    complex, dimension(:, :) :: w
    integer :: n1, n2
    complex, allocatable, dimension(:, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    wr = w
    wr = cshift(wr, nint(n1/2.0), dim=1)
    wr = cshift(wr, nint(n2/2.0), dim=2)
end function fftshift_2d_complex
!
!> Shift 2D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_3d_complex(w) result(wr)
    complex, dimension(:, :, :) :: w
    integer :: n1, n2, n3
    complex, allocatable, dimension(:, :, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    wr = w
    wr = cshift(wr, nint(n1/2.0), dim=1)
    wr = cshift(wr, nint(n2/2.0), dim=2)
    wr = cshift(wr, nint(n3/2.0), dim=3)
end function fftshift_3d_complex
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_1d_complex(x, y, maxlag, eps) result(z)
    complex, dimension(:), intent(in) :: x, y
    integer, intent(in), optional :: maxlag
    complex, intent(in), optional :: eps
    complex, allocatable, dimension(:) :: z
    integer :: nx, ny, n, nlag
    complex, allocatable, dimension(:) :: a, b
    ! Dimensions
    nx = size(x)
    ny = size(y)
    if (present(maxlag)) then
        nlag = min(maxlag, max(nx, ny))
    else
        nlag = 0
    end if
    ! Pad to next power 235
    n = next_power_235(max(nx, ny) + nlag)
    ! Prepare arrays
    allocate (a(1:n))
    allocate (b(1:n))
    b = fft(pad(y, [0, n - ny], ['const', 'const']))
    a = fft(pad(x, [0, n - nx], ['const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        ! b = b + 1.0e-8*sum(b)/n
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n))
    if (present(maxlag)) then
        z = fftshift((ifft(a/b)))
        call alloc_array(z, [-nlag, nlag], &
            source=z(nint((n + 1)/2.0) - nlag:nint((n + 1)/2.0) + nlag))
    else
        z = (ifft(a/b))
        call alloc_array(z, [1, nx], source=z(1:nx))
    end if
end function deconv_1d_complex
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_2d_complex(x, y, maxlag, eps) result(z)
    complex, dimension(:, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    complex, intent(in), optional :: eps
    complex, allocatable, dimension(:, :) :: z
    integer :: n1x, n1y, n2x, n2y, n1, n2, nlag1, nlag2
    complex, allocatable, dimension(:, :) :: a, b
    ! Dimensions
    n1x = size(x, 1)
    n2x = size(x, 2)
    n1y = size(y, 1)
    n2y = size(y, 2)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(n1x, n1y))
        nlag2 = min(maxlag(2), max(n2x, n2y))
    else
        nlag1 = 0
        nlag2 = 0
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(n1x, n1y) + nlag1)
    n2 = next_power_235(max(n2x, n2y) + nlag2)
    ! Prepare arrays
    allocate (a(1:n1, 1:n2))
    allocate (b(1:n1, 1:n2))
    b = fft(pad(y, [0, n1 - n1y, 0, n2 - n2y], ['const', 'const', 'const', 'const']))
    a = fft(pad(x, [0, n1 - n1x, 0, n2 - n2x], ['const', 'const', 'const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        !b = b + 1.0e-8*sum(b)/(n1*n2)
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n1, 1:n2))
    if (present(maxlag)) then
        z = fftshift((ifft(a/b)))
        call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2], &
            source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
            nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2))
    else
        z = (ifft(a/b))
        call alloc_array(z, [1, n1x, 1, n2x], source=z(1:n1x, 1:n2x))
    end if
end function deconv_2d_complex
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_3d_complex(x, y, maxlag, eps) result(z)
    complex, dimension(:, :, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    complex, intent(in), optional :: eps
    complex, allocatable, dimension(:, :, :) :: z
    integer :: n1x, n1y, n2x, n2y, n3x, n3y, n1, n2, n3, nlag1, nlag2, nlag3
    complex, allocatable, dimension(:, :, :) :: a, b
    ! Dimensions
    n1x = size(x, 1)
    n2x = size(x, 2)
    n3x = size(x, 3)
    n1y = size(y, 1)
    n2y = size(y, 2)
    n3y = size(y, 3)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(n1x, n1y))
        nlag2 = min(maxlag(2), max(n2x, n2y))
        nlag3 = min(maxlag(3), max(n3x, n3y))
    else
        nlag1 = 0
        nlag2 = 0
        nlag3 = 0
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(n1x, n1y) + nlag1)
    n2 = next_power_235(max(n2x, n2y) + nlag2)
    n3 = next_power_235(max(n3x, n3y) + nlag3)
    ! Prepare arrays
    allocate (a(1:n1, 1:n2, 1:n3))
    allocate (b(1:n1, 1:n2, 1:n3))
    b = fft(pad(y, [0, n1 - n1y, 0, n2 - n2y, 0, n3 - n3y], ['const', 'const', 'const', 'const', 'const', 'const']))
    a = fft(pad(x, [0, n1 - n1x, 0, n2 - n2x, 0, n3 - n3x], ['const', 'const', 'const', 'const', 'const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        !b = b + 1.0e-8*sum(b)/(n1*n2)
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n1, 1:n2, 1:n3))
    if (present(maxlag)) then
        z = fftshift((ifft(a/b)))
        call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2, -nlag3, nlag3], &
            source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
            nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2, &
            nint((n3 + 1)/2.0) - nlag3:nint((n3 + 1)/2.0) + nlag3))
    else
        z = (ifft(a/b))
        call alloc_array(z, [1, n1x, 1, n2x, 1, n3x], source=z(1:n1x, 1:n2x, 1:n3x))
    end if
end function deconv_3d_complex
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
!> Fast 1D discrete convolution
!
subroutine convfast_1d_dcomplex(lx, kx, x, ly, ky, y, lz, kz, z)
    integer, intent(in) :: lx, ly, lz
    integer, intent(in) :: kx, ky, kz
    double complex, dimension(0:), intent(in) :: x, y
    double complex, dimension(0:), intent(inout) :: z
    ! Bounds for index i.
    integer :: imin
    integer :: imax
    ! Variables that we expect to reside in registers.
    integer :: i, ilo, ihi, j, jlo, jhi, iz
    double complex :: sa, sb, xa, xb, ya, yb
    imin = kz - kx - ky
    imax = imin + lz - 1
    ! Off left: imin <= i <= -1
    ilo = imin
    ihi = min(-1, imax)
    ! for (i=ilo,iz=i-imin i<=ihi ++i,++iz)
    do i = ilo, ihi
        iz = i - imin
        z(iz) = 0.0
    end do
    ! Rolling on: 0 <= i <= lx-2 and 0 <= j <= i
    ilo = max(0, imin)
    ihi = min(lx - 2, imax)
    jlo = 0
    jhi = ilo
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2,jhi+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jhi) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
        jhi = jhi + 2
    end do
    if (i == ihi) then
        iz = i - imin
        jlo = 0
        jhi = i
        sa = 0.0
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Middle: lx-1 <= i <= ly-1 and 0 <= j <= lx-1
    ilo = max(lx - 1, imin)
    ihi = min(ly - 1, imax)
    jlo = 0
    jhi = lx - 1
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        if (j == jhi) then
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
    end do
    if (i == ihi) then
        iz = i - imin
        sa = 0.0
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Rolling off: ly <= i <= lx+ly-2 and i-ly+1 <= j <= lx-1
    ilo = max(ly, imin)
    ihi = min(lx + ly - 2, imax)
    jlo = ihi - ly + 1
    jhi = lx - 1
    ! for (i=ihi,iz=i-imin i>ilo i-=2,iz-=2,jlo-=2) {
    do i = ihi, ilo + 1, -2
        iz = i - imin
        sa = 0.0
        sb = 0.0
        yb = y(i - jhi - 1)
        ! for (j=jhi j>jlo j-=2) {
        do j = jhi, jlo + 1, -2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
            yb = y(i - j + 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jlo) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz - 1) = sb
        jlo = jlo - 2
    end do
    if (i == ilo) then
        iz = i - imin
        jlo = i - ly + 1
        jhi = lx - 1
        sa = 0.0
        ! for (j=jhi j>=jlo --j)
        do j = jhi, jlo, -1
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Off right: lx+ly-1 <= i <= imax
    ilo = max(lx + ly - 1, imin)
    ihi = imax
    ! for (i=ilo,iz=i-imin i<=ihi ++i,++iz)
    do i = ilo, ihi
        iz = i - imin
        z(iz) = 0.0
    end do
end subroutine convfast_1d_dcomplex
!
!> Fast 1D discrete convolution
!
subroutine convsum_1d_dcomplex(lx, kx, x, ly, ky, y, lz, kz, z)
    integer, intent(in) :: lx, ly, lz
    integer, intent(in) :: kx, ky, kz
    double complex, dimension(0:), intent(in) :: x, y
    double complex, dimension(0:), intent(inout) :: z
    ! Bounds for index i.
    integer :: imin
    integer :: imax
    ! Variables that we expect to reside in registers.
    integer :: i, ilo, ihi, j, jlo, jhi, iz
    double complex :: sa, sb, xa, xb, ya, yb
    imin = kz - kx - ky
    imax = imin + lz - 1
    ! Rolling on: 0 <= i <= lx-2 and 0 <= j <= i
    ilo = max(0, imin)
    ihi = min(lx - 2, imax)
    jlo = 0
    jhi = ilo
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2,jhi+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = z(iz)
        sb = z(iz + 1)
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jhi) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
        jhi = jhi + 2
    end do
    if (i == ihi) then
        iz = i - imin
        jlo = 0
        jhi = i
        sa = z(iz)
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Middle: lx-1 <= i <= ly-1 and 0 <= j <= lx-1
    ilo = max(lx - 1, imin)
    ihi = min(ly - 1, imax)
    jlo = 0
    jhi = lx - 1
    ! for (i=ilo,iz=i-imin i<ihi i+=2,iz+=2) {
    do i = ilo, ihi - 1, 2
        iz = i - imin
        sa = z(iz)
        sb = z(iz + 1)
        yb = y(i - jlo + 1)
        ! for (j=jlo j<jhi j+=2) {
        do j = jlo, jhi - 1, 2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j + 1)
            sb = sb + xb*ya
            yb = y(i - j - 1)
            sa = sa + xb*yb
        end do
        if (j == jhi) then
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
        end if
        z(iz) = sa
        z(iz + 1) = sb
    end do
    if (i == ihi) then
        iz = i - imin
        sa = z(iz)
        ! for (j=jlo j<=jhi ++j)
        do j = jlo, jhi
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
    ! Rolling off: ly <= i <= lx+ly-2 and i-ly+1 <= j <= lx-1
    ilo = max(ly, imin)
    ihi = min(lx + ly - 2, imax)
    jlo = ihi - ly + 1
    jhi = lx - 1
    ! for (i=ihi,iz=i-imin i>ilo i-=2,iz-=2,jlo-=2) {
    do i = ihi, ilo + 1, -2
        iz = i - imin
        sa = z(iz)
        sb = z(iz - 1)
        yb = y(i - jhi - 1)
        ! for (j=jhi j>jlo j-=2) {
        do j = jhi, jlo + 1, -2
            xa = x(j)
            sb = sb + xa*yb
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
            yb = y(i - j + 1)
            sa = sa + xb*yb
        end do
        xa = x(j)
        sb = sb + xa*yb
        if (j == jlo) then
            ya = y(i - j)
            sa = sa + xa*ya
            xb = x(j - 1)
            sb = sb + xb*ya
        end if
        z(iz) = sa
        z(iz - 1) = sb
        jlo = jlo - 2
    end do
    if (i == ilo) then
        iz = i - imin
        jlo = i - ly + 1
        jhi = lx - 1
        sa = z(iz)
        ! for (j=jhi j>=jlo --j)
        do j = jhi, jlo, -1
            sa = sa + x(j)*y(i - j)
        end do
        z(iz) = sa
    end if
end subroutine convsum_1d_dcomplex
!
!> Discrete 1D convolution translated from convFast in minesjtk
!
subroutine discrete_conv_1d_dcomplex(x, y, z, kx, ky, kz)
    double complex, dimension(:), intent(in) :: x, y
    double complex, dimension(:), intent(inout) :: z
    integer, intent(in) :: kx, ky, kz
    integer :: lx, ly, lz
    lx = size(x, 1)
    ly = size(y, 1)
    lz = size(z, 1)
    z = 0.0
    if (lx > ly) then
        call convfast_1d_dcomplex( &
            ly, ky - 1, y, &
            lx, kx - 1, x, &
            lz, kz - 1, z)
    else
        call convfast_1d_dcomplex( &
            lx, kx - 1, x, &
            ly, ky - 1, y, &
            lz, kz - 1, z)
    end if
end subroutine discrete_conv_1d_dcomplex
!
!> Discrete 2D convolution translated from convFast in minesjtk
!
subroutine discrete_conv_2d_dcomplex(x, y, z, &
        kx1, kx2, &
        ky1, ky2, &
        kz1, kz2)
    double complex, dimension(:, :), intent(in) :: x, y
    double complex, dimension(:, :), intent(inout) :: z
    integer, intent(in) :: kx1, ky1, kz1
    integer, intent(in) :: kx2, ky2, kz2
    integer :: lx1, ly1, lz1
    integer :: lx2, ly2, lz2
    integer :: ilo2, ihi2, jlo2, jhi2
    integer :: i2, j2
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    z = 0.0
    ilo2 = (kz2 - 1) - (kx2 - 1) - (ky2 - 1)
    ihi2 = ilo2 + lz2 - 1
    if (lx1 > ly1) then
        ! !$omp parallel do private(i2,j2,jlo2,jhi2)
        do i2 = ilo2, ihi2
            jlo2 = max(0, i2 - ly2 + 1)
            jhi2 = min(lx2 - 1, i2)
            do j2 = jlo2, jhi2
                call convsum_1d_dcomplex( &
                    ly1, ky1 - 1, y(:, i2 - j2 + 1), &
                    lx1, kx1 - 1, x(:, j2 + 1), &
                    lz1, kz1 - 1, z(:, i2 - ilo2 + 1))
            end do
        end do
        ! !$omp end parallel do
    else
        ! !$omp parallel do private(i2,j2,jlo2,jhi2)
        do i2 = ilo2, ihi2
            jlo2 = max(0, i2 - ly2 + 1)
            jhi2 = min(lx2 - 1, i2)
            do j2 = jlo2, jhi2
                call convsum_1d_dcomplex( &
                    lx1, kx1 - 1, x(:, j2 + 1), &
                    ly1, ky1 - 1, y(:, i2 - j2 + 1), &
                    lz1, kz1 - 1, z(:, i2 - ilo2 + 1))
            end do
        end do
        ! !$omp end parallel do
    end if
end subroutine discrete_conv_2d_dcomplex
!
!> Discrete 3D convolution
!
subroutine discrete_conv_3d_dcomplex(x, y, z, &
        kx1, kx2, kx3, &
        ky1, ky2, ky3, &
        kz1, kz2, kz3)
    integer, intent(in) :: kx1, ky1, kz1
    integer, intent(in) :: kx2, ky2, kz2
    integer, intent(in) :: kx3, ky3, kz3
    double complex, dimension(:, :, :), intent(in) :: x, y
    double complex, dimension(:, :, :), intent(inout) :: z
    integer :: lx1, ly1, lz1
    integer :: lx2, ly2, lz2
    integer :: lx3, ly3, lz3
    integer :: ilo2, ihi2, ilo3, ihi3
    integer :: jlo2, jhi2, jlo3, jhi3
    integer :: i2, j2, i3, j3
    lx1 = size(x, 1)
    lx2 = size(x, 2)
    lx3 = size(x, 3)
    ly1 = size(y, 1)
    ly2 = size(y, 2)
    ly3 = size(y, 3)
    lz1 = size(z, 1)
    lz2 = size(z, 2)
    lz3 = size(z, 3)
    z = 0.0
    ilo2 = (kz2 - 1) - (kx2 - 1) - (ky2 - 1)
    ilo3 = (kz3 - 1) - (kx3 - 1) - (ky3 - 1)
    ihi2 = ilo2 + lz2 - 1
    ihi3 = ilo3 + lz3 - 1
    if (lx1 > ly1) then
        ! !$omp parallel do private(i2,i3,j2,j3,jlo2,jhi2,jlo3,jhi3)
        do i3 = ilo3, ihi3
            jlo3 = max(0, i3 - ly3 + 1)
            jhi3 = min(lx3 - 1, i3)
            do j3 = jlo3, jhi3
                do i2 = ilo2, ihi2
                    jlo2 = max(0, i2 - ly2 + 1)
                    jhi2 = min(lx2 - 1, i2)
                    do j2 = jlo2, jhi2
                        call convsum_1d_dcomplex( &
                            ly1, ky1 - 1, y(:, i2 - j2 + 1, i3 - j3 + 1), &
                            lx1, kx1 - 1, x(:, j2 + 1, j3 + 1), &
                            lz1, kz1 - 1, z(:, i2 - ilo2 + 1, i3 - ilo3 + 1))
                    end do
                end do
            end do
        end do
        ! !$omp end parallel do
    else
        ! !$omp parallel do private(i2,i3,j2,j3,jlo2,jhi2,jlo3,jhi3)
        do i3 = ilo3, ihi3
            jlo3 = max(0, i3 - ly3 + 1)
            jhi3 = min(lx3 - 1, i3)
            do j3 = jlo3, jhi3
                do i2 = ilo2, ihi2
                    jlo2 = max(0, i2 - ly2 + 1)
                    jhi2 = min(lx2 - 1, i2)
                    do j2 = jlo2, jhi2
                        call convsum_1d_dcomplex( &
                            lx1, kx1 - 1, x(:, j2 + 1, j3 + 1), &
                            ly1, ky1 - 1, y(:, i2 - j2 + 1, i3 - j3 + 1), &
                            lz1, kz1 - 1, z(:, i2 - ilo2 + 1, i3 - ilo3 + 1))
                    end do
                end do
            end do
        end do
        ! !$omp end parallel do
    end if
end subroutine discrete_conv_3d_dcomplex
function conv_1d_dcomplex(x, y, method) result(z)
    double complex, dimension(:), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    double complex, allocatable, dimension(:) :: z
    integer :: nx, ny, n
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx = size(x)
    ny = size(y)
    n = next_power_235(nx + ny - 1)
    allocate (z(1:n))
    z = (ifft(fft(pad(x, [0, n - nx], ['const', 'const']))*fft(pad(y, [0, n - ny], ['const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, [1, nx + ny - 1], source=z(1:nx + ny - 1))
        case ('same')
            n = nint((ny + 1)/2.0)
            call alloc_array(z, [1, nx], source=z(n:n + nx - 1))
    end select
end function conv_1d_dcomplex
function conv_2d_dcomplex(x, y, method) result(z)
    double complex, dimension(:, :), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    double complex, allocatable, dimension(:, :) :: z
    integer :: nx1, nx2, ny1, ny2, n1, n2
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    n1 = next_power_235(nx1 + ny1 - 1)
    n2 = next_power_235(nx2 + ny2 - 1)
    allocate (z(1:n1, 1:n2))
    z = (ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2], ['const', 'const', 'const', 'const']))* &
        fft(pad(y, [0, n1 - ny1, 0, n2 - ny2], ['const', 'const', 'const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, [1, nx1 + ny1 - 1, 1, nx2 + ny2 - 1], &
                source=z(1:nx1 + ny1 - 1, 1:nx2 + ny2 - 1))
        case ('same')
            n1 = nint((ny1 + 1)/2.0)
            n2 = nint((ny2 + 1)/2.0)
            call alloc_array(z, [1, nx1, 1, nx2], &
                source=z(n1:n1 + nx1 - 1, n2:n2 + nx2 - 1))
    end select
end function conv_2d_dcomplex
function conv_3d_dcomplex(x, y, method) result(z)
    double complex, dimension(:, :, :), intent(in) :: x, y
    character(len=*), intent(in), optional :: method
    double complex, allocatable, dimension(:, :, :) :: z
    integer :: nx1, nx2, nx3, ny1, ny2, ny3, n1, n2, n3
    character(len=12) :: conv_method
    if (present(method)) then
        conv_method = method
    else
        conv_method = 'full'
    end if
    nx1 = size(x, 1)
    nx2 = size(x, 2)
    nx3 = size(x, 3)
    ny1 = size(y, 1)
    ny2 = size(y, 2)
    ny3 = size(y, 3)
    n1 = next_power_235(nx1 + ny1 - 1)
    n2 = next_power_235(nx2 + ny2 - 1)
    n3 = next_power_235(nx3 + ny3 - 1)
    allocate (z(1:n1, 1:n2, 1:n3))
    z = (ifft( &
        fft(pad(x, [0, n1 - nx1, 0, n2 - nx2, 0, n3 - nx3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))* &
        fft(pad(y, [0, n1 - ny1, 0, n2 - ny2, 0, n3 - ny3], &
        ['const', 'const', 'const', 'const', 'const', 'const']))))
    select case (conv_method)
        case ('full')
            call alloc_array(z, &
                [1, nx1 + ny1 - 1, 1, nx2 + ny2 - 1, 1, nx3 + ny3 - 1], &
                source=z(1:nx1 + ny1 - 1, 1:nx2 + ny2 - 1, 1:nx3 + ny3 - 1))
        case ('same')
            n1 = nint((ny1 + 1)/2.0)
            n2 = nint((ny2 + 1)/2.0)
            n3 = nint((ny3 + 1)/2.0)
            call alloc_array(z, [1, nx1, 1, nx2, 1, nx3], &
                source=z(n1:n1 + nx1 - 1, n2:n2 + nx2 - 1, n3:n3 + nx3 - 1))
    end select
end function conv_3d_dcomplex
!
!> Shift 1D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_1d_dcomplex(w) result(wr)
    double complex, dimension(:) :: w
    integer :: n1
    double complex, allocatable, dimension(:) :: wr
    n1 = size(w)
    wr = w
    wr = cshift(wr, nint(n1/2.0))
end function fftshift_1d_dcomplex
!
!> Shift 2D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_2d_dcomplex(w) result(wr)
    double complex, dimension(:, :) :: w
    integer :: n1, n2
    double complex, allocatable, dimension(:, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    wr = w
    wr = cshift(wr, nint(n1/2.0), dim=1)
    wr = cshift(wr, nint(n2/2.0), dim=2)
end function fftshift_2d_dcomplex
!
!> Shift 2D FFT result so that low-high-high-low becomes high-low-low-high,
!> or vice versa
!
function fftshift_3d_dcomplex(w) result(wr)
    double complex, dimension(:, :, :) :: w
    integer :: n1, n2, n3
    double complex, allocatable, dimension(:, :, :) :: wr
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    wr = w
    wr = cshift(wr, nint(n1/2.0), dim=1)
    wr = cshift(wr, nint(n2/2.0), dim=2)
    wr = cshift(wr, nint(n3/2.0), dim=3)
end function fftshift_3d_dcomplex
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_1d_dcomplex(x, y, maxlag, eps) result(z)
    double complex, dimension(:), intent(in) :: x, y
    integer, intent(in), optional :: maxlag
    double complex, intent(in), optional :: eps
    double complex, allocatable, dimension(:) :: z
    integer :: nx, ny, n, nlag
    double complex, allocatable, dimension(:) :: a, b
    ! Dimensions
    nx = size(x)
    ny = size(y)
    if (present(maxlag)) then
        nlag = min(maxlag, max(nx, ny))
    else
        nlag = 0
    end if
    ! Pad to next power 235
    n = next_power_235(max(nx, ny) + nlag)
    ! Prepare arrays
    allocate (a(1:n))
    allocate (b(1:n))
    b = fft(pad(y, [0, n - ny], ['const', 'const']))
    a = fft(pad(x, [0, n - nx], ['const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        ! b = b + 1.0e-8*sum(b)/n
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n))
    if (present(maxlag)) then
        z = fftshift((ifft(a/b)))
        call alloc_array(z, [-nlag, nlag], &
            source=z(nint((n + 1)/2.0) - nlag:nint((n + 1)/2.0) + nlag))
    else
        z = (ifft(a/b))
        call alloc_array(z, [1, nx], source=z(1:nx))
    end if
end function deconv_1d_dcomplex
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_2d_dcomplex(x, y, maxlag, eps) result(z)
    double complex, dimension(:, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    double complex, intent(in), optional :: eps
    double complex, allocatable, dimension(:, :) :: z
    integer :: n1x, n1y, n2x, n2y, n1, n2, nlag1, nlag2
    double complex, allocatable, dimension(:, :) :: a, b
    ! Dimensions
    n1x = size(x, 1)
    n2x = size(x, 2)
    n1y = size(y, 1)
    n2y = size(y, 2)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(n1x, n1y))
        nlag2 = min(maxlag(2), max(n2x, n2y))
    else
        nlag1 = 0
        nlag2 = 0
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(n1x, n1y) + nlag1)
    n2 = next_power_235(max(n2x, n2y) + nlag2)
    ! Prepare arrays
    allocate (a(1:n1, 1:n2))
    allocate (b(1:n1, 1:n2))
    b = fft(pad(y, [0, n1 - n1y, 0, n2 - n2y], ['const', 'const', 'const', 'const']))
    a = fft(pad(x, [0, n1 - n1x, 0, n2 - n2x], ['const', 'const', 'const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        !b = b + 1.0e-8*sum(b)/(n1*n2)
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n1, 1:n2))
    if (present(maxlag)) then
        z = fftshift((ifft(a/b)))
        call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2], &
            source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
            nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2))
    else
        z = (ifft(a/b))
        call alloc_array(z, [1, n1x, 1, n2x], source=z(1:n1x, 1:n2x))
    end if
end function deconv_2d_dcomplex
!
!> Simple deconvolution (a.k.a. inverse filtering) using FFT
!
function deconv_3d_dcomplex(x, y, maxlag, eps) result(z)
    double complex, dimension(:, :, :), intent(in) :: x, y
    integer, dimension(:), intent(in), optional :: maxlag
    double complex, intent(in), optional :: eps
    double complex, allocatable, dimension(:, :, :) :: z
    integer :: n1x, n1y, n2x, n2y, n3x, n3y, n1, n2, n3, nlag1, nlag2, nlag3
    double complex, allocatable, dimension(:, :, :) :: a, b
    ! Dimensions
    n1x = size(x, 1)
    n2x = size(x, 2)
    n3x = size(x, 3)
    n1y = size(y, 1)
    n2y = size(y, 2)
    n3y = size(y, 3)
    if (present(maxlag)) then
        nlag1 = min(maxlag(1), max(n1x, n1y))
        nlag2 = min(maxlag(2), max(n2x, n2y))
        nlag3 = min(maxlag(3), max(n3x, n3y))
    else
        nlag1 = 0
        nlag2 = 0
        nlag3 = 0
    end if
    ! Pad to next power 235
    n1 = next_power_235(max(n1x, n1y) + nlag1)
    n2 = next_power_235(max(n2x, n2y) + nlag2)
    n3 = next_power_235(max(n3x, n3y) + nlag3)
    ! Prepare arrays
    allocate (a(1:n1, 1:n2, 1:n3))
    allocate (b(1:n1, 1:n2, 1:n3))
    b = fft(pad(y, [0, n1 - n1y, 0, n2 - n2y, 0, n3 - n3y], ['const', 'const', 'const', 'const', 'const', 'const']))
    a = fft(pad(x, [0, n1 - n1x, 0, n2 - n2x, 0, n3 - n3x], ['const', 'const', 'const', 'const', 'const', 'const']))*conjg(b)
    b = b*conjg(b)
    if (present(eps)) then
        b = b + eps*maxval(abs(b))
    else
        !b = b + 1.0e-8*sum(b)/(n1*n2)
        b = b + 1.0e-3*maxval(abs(b))
    end if
    ! Do the deconvolution
    ! Traditionally, deconvolution is u/d where u is the ground truth and
    ! d is the filter. u/d is to recover the groud truth from blurred measurements.
    ! Translated into the variables used here,
    ! a/b is a like a stabilized deconvolution.
    ! If a maximum time lag is given, then the following first option
    ! is more like a normalized cross-correlation, because the results
    ! embraces both positive and negative time lags.
    ! The second option is more of the traditional deconvolution, whose length
    ! is from 1 to nt (the same with the origional signal)
    allocate (z(1:n1, 1:n2, 1:n3))
    if (present(maxlag)) then
        z = fftshift((ifft(a/b)))
        call alloc_array(z, [-nlag1, nlag1, -nlag2, nlag2, -nlag3, nlag3], &
            source=z(nint((n1 + 1)/2.0) - nlag1:nint((n1 + 1)/2.0) + nlag1, &
            nint((n2 + 1)/2.0) - nlag2:nint((n2 + 1)/2.0) + nlag2, &
            nint((n3 + 1)/2.0) - nlag3:nint((n3 + 1)/2.0) + nlag3))
    else
        z = (ifft(a/b))
        call alloc_array(z, [1, n1x, 1, n2x, 1, n3x], source=z(1:n1x, 1:n2x, 1:n3x))
    end if
end function deconv_3d_dcomplex
    !
    !> Use FFT to compute derivative
    !
    function fft_deriv_1d_float(u, order) result(du)
        real, dimension(:), intent(in) :: u
        integer, optional, intent(in) :: order
        real, allocatable, dimension(:) :: du
        integer :: nu, n, pn, dn
        integer :: fft_deriv_order
        real, allocatable, dimension(:) :: k
        if (present(order)) then
            fft_deriv_order = order
        else
            fft_deriv_order = 1
        end if
        nu = size(u)
        n = nu
        pn = next_power_235(2*n)
        n = nint(n/2.0)
        pn = pn - n - nu
        du = [ones(n)*u(1), u, ones(pn)*u(nu)]
        du = taper(du, [n, pn], ['blackman', 'blackman'])
        dn = size(du)
        k = fft_omega(dn, 1.0)
        du = ifft((const_i*k)**fft_deriv_order*fft(du), real=.true.)
        du = du(n + 1:n + nu)
    end function fft_deriv_1d_float
    function fft_deriv_1d_double(u, order) result(du)
        double precision, dimension(:), intent(in) :: u
        integer, optional, intent(in) :: order
        double precision, allocatable, dimension(:) :: du
        integer :: nu, n, pn, dn
        integer :: fft_deriv_order
        double precision, allocatable, dimension(:) :: k
        if (present(order)) then
            fft_deriv_order = order
        else
            fft_deriv_order = 1
        end if
        nu = size(u)
        n = nu
        pn = next_power_235(2*n)
        n = nint(n/2.0)
        pn = pn - n - nu
        du = [ones(n)*u(1), u, ones(pn)*u(nu)]
        du = taper(du, [n, pn], ['blackman', 'blackman'])
        dn = size(du)
        k = fft_omega(dn, 1.0)
        du = ifft((const_i*k)**fft_deriv_order*fft(du), real=.true.)
        du = du(n + 1:n + nu)
    end function fft_deriv_1d_double
    !
    !> Next power 2^m integer
    !
    elemental function next_power_2(a) result(l)
        integer, intent(in) :: a
        integer, dimension(1:17) :: qv
        integer :: i, l
        qv = [2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, &
            2048, 4096, 8192, 16384, 32768, 65536, 131072]
        if (a <= 131072) then
            l = qv(1)
            do i = 1, 16
                if (a > qv(i) .and. a <= qv(i + 1)) then
                    l = qv(i + 1)
                    return
                end if
            end do
        else
            l = 2**ceiling(log(a*1.0d0)/log(2.0d0))
        end if
    end function next_power_2
    !
    !> Next power (2^m)*(3^n)*(5^l) integer
    !
    elemental function next_power_235(a) result(l)
        integer, intent(in) :: a
        integer, dimension(1:310) :: qv
        integer :: n2, n3, n5
        integer :: i, j, k
        integer :: l, v
        qv = [2, 3, 4, 5, 6, 8, 9, 10, 12, 15, &
            16, 18, 20, 24, 25, 27, 30, 32, 36, 40, &
            45, 48, 50, 54, 60, 64, 72, 75, 80, 81, &
            90, 96, 100, 108, 120, 125, 128, 135, 144, 150, &
            160, 162, 180, 192, 200, 216, 225, 240, 243, 250, &
            256, 270, 288, 300, 320, 324, 360, 375, 384, 400, &
            405, 432, 450, 480, 486, 500, 512, 540, 576, 600, &
            625, 640, 648, 675, 720, 729, 750, 768, 800, 810, &
            864, 900, 960, 972, 1000, 1024, 1080, 1125, 1152, 1200, &
            1215, 1250, 1280, 1296, 1350, 1440, 1458, 1500, 1536, 1600, &
            1620, 1728, 1800, 1875, 1920, 1944, 2000, 2025, 2048, 2160, &
            2187, 2250, 2304, 2400, 2430, 2500, 2560, 2592, 2700, 2880, &
            2916, 3000, 3072, 3125, 3200, 3240, 3375, 3456, 3600, 3645, &
            3750, 3840, 3888, 4000, 4050, 4096, 4320, 4374, 4500, 4608, &
            4800, 4860, 5000, 5120, 5184, 5400, 5625, 5760, 5832, 6000, &
            6075, 6144, 6250, 6400, 6480, 6561, 6750, 6912, 7200, 7290, &
            7500, 7680, 7776, 8000, 8100, 8192, 8640, 8748, 9000, 9216, &
            9375, 9600, 9720, 10000, 10125, 10240, 10368, 10800, 10935, 11250, &
            11520, 11664, 12000, 12150, 12288, 12500, 12800, 12960, 13122, 13500, &
            13824, 14400, 14580, 15000, 15360, 15552, 15625, 16000, 16200, 16384, &
            16875, 17280, 17496, 18000, 18225, 18432, 18750, 19200, 19440, 19683, &
            20000, 20250, 20480, 20736, 21600, 21870, 22500, 23040, 23328, 24000, &
            24300, 24576, 25000, 25600, 25920, 26244, 27000, 27648, 28125, 28800, &
            29160, 30000, 30375, 30720, 31104, 31250, 32000, 32400, 32768, 32805, &
            33750, 34560, 34992, 36000, 36450, 36864, 37500, 38400, 38880, 39366, &
            40000, 40500, 40960, 41472, 43200, 43740, 45000, 46080, 46656, 46875, &
            48000, 48600, 49152, 50000, 50625, 51200, 51840, 52488, 54000, 54675, &
            55296, 56250, 57600, 58320, 59049, 60000, 60750, 61440, 62208, 62500, &
            64000, 64800, 65610, 67500, 69120, 69984, 72000, 72900, 73728, 75000, &
            76800, 77760, 78125, 78732, 80000, 81000, 81920, 82944, 84375, 86400, &
            87480, 90000, 91125, 92160, 93312, 93750, 96000, 97200, 98304, 98415]
        if (a <= 98415) then
            l = qv(1)
            do i = 1, 309
                if (a > qv(i) .and. a <= qv(i + 1)) then
                    l = qv(i + 1)
                    return
                end if
            end do
        else
            n2 = ceiling(log(a*1.0d0)/log(2.0d0))
            n3 = ceiling(log(a*1.0d0)/log(3.0d0))
            n5 = ceiling(log(a*1.0d0)/log(5.0d0))
            l = 10*a
            do i = 0, n2
                do j = 0, n3
                    do k = 0, n5
                        v = (2**i)*(3**j)*(5**k)
                        if (v >= a .and. v <= l) then
                            l = v
                        end if
                    end do
                end do
            end do
        end if
    end function next_power_235
    !
    !> Next power (2^m)*(3^n)*(5^l)*(7^h) integer
    !
    elemental function next_power_2357(a) result(l)
        integer, intent(in) :: a
        integer :: n2, n3, n5, n7
        integer :: i, j, k, h
        integer :: l, v
        n2 = ceiling(log(a*1.0d0)/log(2.0d0))
        n3 = ceiling(log(a*1.0d0)/log(3.0d0))
        n5 = ceiling(log(a*1.0d0)/log(5.0d0))
        n7 = ceiling(log(a*1.0d0)/log(7.0d0))
        l = 10*a
        do i = 0, n2
            do j = 0, n3
                do k = 0, n5
                    do h = 0, n7
                        v = (2**i)*(3**j)*(5**k)*(7**h)
                        if (v >= a .and. v <= l) then
                            l = v
                        end if
                    end do
                end do
            end do
        end do
    end function next_power_2357
    !
    !> Next power (2^m)*(3^n)*(5^l)*(7^h)*(11^i) integer
    !
    elemental function next_power_235711(a) result(l)
        integer, intent(in) :: a
        integer :: n2, n3, n5, n7, n11
        integer :: i, j, k, h, m
        integer :: l, v
        n2 = ceiling(log(a*1.0d0)/log(2.0d0))
        n3 = ceiling(log(a*1.0d0)/log(3.0d0))
        n5 = ceiling(log(a*1.0d0)/log(5.0d0))
        n7 = ceiling(log(a*1.0d0)/log(7.0d0))
        n11 = ceiling(log(a*1.0d0)/log(11.0d0))
        l = 10*a
        do i = 0, n2
            do j = 0, n3
                do k = 0, n5
                    do h = 0, n7
                        do m = 0, n11
                            v = (2**i)*(3**j)*(5**k)*(7**h)*(11**m)
                            if (v >= a .and. v <= l) then
                                l = v
                            end if
                        end do
                    end do
                end do
            end do
        end do
    end function next_power_235711
    ! ======================================================
    ! FFT subroutine version
    subroutine fourier1_complex_to_complex(array)
        ! Parameters
        complex, dimension(:), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n
        ! Size of the array
        n = size(array)
        ! Forward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_single, dfti_complex, 1, n)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputeforward(dft_handler, array)
        dft_status = dftifreedescriptor(dft_handler)
    end subroutine fourier1_complex_to_complex
    subroutine fourier1_dcomplex_to_dcomplex(array)
        ! Parameters
        double complex, dimension(:), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n
        ! Size of the array
        n = size(array)
        ! Forward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_double, dfti_complex, 1, n)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputeforward(dft_handler, array)
        dft_status = dftifreedescriptor(dft_handler)
    end subroutine fourier1_dcomplex_to_dcomplex
    subroutine ifourier1_complex_to_complex(array)
        ! Parameters
        complex, dimension(:), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n
        ! Size of the array
        n = size(array)
        ! Backward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_single, dfti_complex, 1, n)
        dft_status = dftisetvalue(dft_handler, dfti_backward_scale, 1.0/n)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputebackward(dft_handler, array)
        dft_status = dftifreedescriptor(dft_handler)
    end subroutine ifourier1_complex_to_complex
    subroutine ifourier1_dcomplex_to_dcomplex(array)
        ! Parameters
        double complex, dimension(:), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n
        ! Size of the array
        n = size(array)
        ! Backward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_double, dfti_complex, 1, n)
        dft_status = dftisetvalue(dft_handler, dfti_backward_scale, 1.0/n)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputebackward(dft_handler, array)
        dft_status = dftifreedescriptor(dft_handler)
    end subroutine ifourier1_dcomplex_to_dcomplex
    subroutine fourier2_complex_to_complex(array)
        ! arguments
        complex, dimension(:, :), intent(inout) :: array
        ! local variables
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:2), m
        complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Forward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_single, dfti_complex, 2, n)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputeforward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assign value
        array = reshape(tarray, n)
    end subroutine fourier2_complex_to_complex
    !
    !> FFT for 2D array from double complex to double complex
    !
    subroutine fourier2_dcomplex_to_dcomplex(array)
        ! arguments
        double complex, dimension(:, :), intent(inout) :: array
        ! local variables
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:2), m
        double complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Forward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_double, dfti_complex, 2, n)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputeforward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assign value
        array = reshape(tarray, n)
    end subroutine fourier2_dcomplex_to_dcomplex
    !
    !> Inverse FFT for 2D array from complex to complex
    !
    subroutine ifourier2_complex_to_complex(array)
        ! Parameters
        complex, dimension(:, :), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:2), m
        complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Backward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_single, dfti_complex, 2, n)
        dft_status = dftisetvalue(dft_handler, dfti_backward_scale, 1.0/m)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputebackward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assian values
        array = reshape(tarray, n)
    end subroutine ifourier2_complex_to_complex
    !
    !> Inverse FFT for 2D array from double complex to double complex
    !
    subroutine ifourier2_dcomplex_to_dcomplex(array)
        ! Parameters
        double complex, dimension(:, :), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:2), m
        double complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Backward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_double, dfti_complex, 2, n)
        dft_status = dftisetvalue(dft_handler, dfti_backward_scale, 1.0/m)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputebackward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assian values
        array = reshape(tarray, n)
    end subroutine ifourier2_dcomplex_to_dcomplex
    !
    !> FFT for 3D array from complex to complex
    !
    subroutine fourier3_complex_to_complex(array)
        ! Parameters
        complex, dimension(:, :, :), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:3), m
        complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Forward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_single, dfti_complex, 3, n)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputeforward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assign value
        array = reshape(tarray, n)
    end subroutine fourier3_complex_to_complex
    !
    !> FFT for 3D array from double complex to double complex
    !
    subroutine fourier3_dcomplex_to_dcomplex(array)
        ! Parameters
        double complex, dimension(:, :, :), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:3), m
        double complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Forward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_double, dfti_complex, 3, n)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputeforward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assign value
        array = reshape(tarray, n)
    end subroutine fourier3_dcomplex_to_dcomplex
    !
    !> Inverse FFT for 3D array from complex to complex
    !
    subroutine ifourier3_complex_to_complex(array)
        ! Parameters
        complex, dimension(:, :, :), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:3), m
        complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Backward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_single, dfti_complex, 3, n)
        dft_status = dftisetvalue(dft_handler, dfti_backward_scale, 1.0/m)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputebackward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assian values
        array = reshape(tarray, n)
    end subroutine ifourier3_complex_to_complex
    !
    !> Inverse FFT for 3D array from double complex to double complex
    !
    subroutine ifourier3_dcomplex_to_dcomplex(array)
        ! Parameters
        double complex, dimension(:, :, :), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:3), m
        double complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Backward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_double, dfti_complex, 3, n)
        dft_status = dftisetvalue(dft_handler, dfti_backward_scale, 1.0/m)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputebackward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assian values
        array = reshape(tarray, n)
    end subroutine ifourier3_dcomplex_to_dcomplex
    !
    !> FFT for 3D array from complex to complex
    !
    subroutine fourier4_complex_to_complex(array)
        ! Parameters
        complex, dimension(:, :, :, :), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:4), m
        complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Forward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_single, dfti_complex, 4, n)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputeforward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assign value
        array = reshape(tarray, n)
    end subroutine fourier4_complex_to_complex
    !
    !> FFT for 3D array from double complex to double complex
    !
    subroutine fourier4_dcomplex_to_dcomplex(array)
        ! Parameters
        double complex, dimension(:, :, :, :), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:4), m
        double complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Forward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_double, dfti_complex, 4, n)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputeforward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assign value
        array = reshape(tarray, n)
    end subroutine fourier4_dcomplex_to_dcomplex
    !
    !> Inverse FFT for 3D array from complex to complex
    !
    subroutine ifourier4_complex_to_complex(array)
        ! Parameters
        complex, dimension(:, :, :, :), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:4), m
        complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Backward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_single, dfti_complex, 4, n)
        dft_status = dftisetvalue(dft_handler, dfti_backward_scale, 1.0/m)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputebackward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assian values
        array = reshape(tarray, n)
    end subroutine ifourier4_complex_to_complex
    !
    !> Inverse FFT for 3D array from double complex to double complex
    !
    subroutine ifourier4_dcomplex_to_dcomplex(array)
        ! Parameters
        double complex, dimension(:, :, :, :), intent(inout) :: array
        type(dfti_descriptor), pointer :: dft_handler
        integer :: dft_status
        integer :: n(1:4), m
        double complex, allocatable, dimension(:) :: tarray
        ! Size of the array
        n = shape(array)
        m = size(array)
        ! Allocate new array
        allocate (tarray(1:m))
        tarray = reshape(array, [m])
        ! Backward FFT
        dft_status = dfticreatedescriptor(dft_handler, dfti_double, dfti_complex, 4, n)
        dft_status = dftisetvalue(dft_handler, dfti_backward_scale, 1.0/m)
        dft_status = dfticommitdescriptor(dft_handler)
        dft_status = dfticomputebackward(dft_handler, tarray)
        dft_status = dftifreedescriptor(dft_handler)
        ! Assian values
        array = reshape(tarray, n)
    end subroutine ifourier4_dcomplex_to_dcomplex
    ! ======================================================
    ! FFT -- complex/float to complex, and their padded versioin
    function fft1_complex_to_complex(w) result(wf)
        complex, dimension(:), intent(in) :: w
        complex, allocatable, dimension(:) :: wf
        wf = w
        call fourier1_complex_to_complex(wf)
    end function fft1_complex_to_complex
    function fft1_float_to_complex(w) result(wf)
        real, dimension(:), intent(in) :: w
        complex, allocatable, dimension(:) :: wf
        wf = cmplx(w)
        call fourier1_complex_to_complex(wf)
    end function fft1_float_to_complex
    function fft1_pad_complex_to_complex(w, n) result(wf)
        complex, dimension(:), intent(in) :: w
        integer, intent(in) :: n
        complex, allocatable, dimension(:) :: wf
        wf = adjust(w, n)
        call fourier1_complex_to_complex(wf)
    end function fft1_pad_complex_to_complex
    function fft1_pad_float_to_complex(w, n) result(wf)
        real, dimension(:), intent(in) :: w
        integer, intent(in) :: n
        complex, allocatable, dimension(:) :: wf
        wf = adjust(cmplx(w), n)
        call fourier1_complex_to_complex(wf)
    end function fft1_pad_float_to_complex
    function ifft1_complex_to_complex(w) result(wf)
        complex, dimension(:), intent(in) :: w
        complex, allocatable, dimension(:) :: wf
        wf = w
        call ifourier1_complex_to_complex(wf)
    end function ifft1_complex_to_complex
    function ifft1_complex_to_float(w, real) result(wf)
        complex, dimension(:), intent(in) :: w
        logical, intent(in) :: real
        real, allocatable, dimension(:) :: wf
        complex, allocatable, dimension(:) :: ww
        call assert(real, 'Warning: real must be true.')
        ww = w
        call ifourier1_complex_to_complex(ww)
        wf = ww%re
    end function ifft1_complex_to_float
    function ifft1_pad_complex_to_complex(w, n) result(wf)
        complex, dimension(:), intent(in) :: w
        integer, intent(in) :: n
        complex, allocatable, dimension(:) :: wf
        wf = adjust(w, n)
        call ifourier1_complex_to_complex(wf)
    end function ifft1_pad_complex_to_complex
    function ifft1_pad_complex_to_float(w, n, real) result(wf)
        complex, dimension(:), intent(in) :: w
        integer, intent(in) :: n
        logical, intent(in) :: real
        real, allocatable, dimension(:) :: wf
        complex, allocatable, dimension(:) :: ww
        call assert(real, 'Warning: real must be true.')
        ww = adjust(w, n)
        call ifourier1_complex_to_complex(ww)
        wf = ww%re
    end function ifft1_pad_complex_to_float
    ! ======================================================
    ! FFT -- dcomplex/double to dcomplex, and their padded versioin
    function fft1_dcomplex_to_dcomplex(w) result(wf)
        double complex, dimension(:), intent(in) :: w
        double complex, allocatable, dimension(:) :: wf
        allocate (wf(1:size(w)), source=w)
        call fourier1_dcomplex_to_dcomplex(wf)
    end function fft1_dcomplex_to_dcomplex
    function fft1_double_to_dcomplex(w) result(wf)
        double precision, dimension(:), intent(in) :: w
        double complex, allocatable, dimension(:) :: wf
        allocate (wf(1:size(w)), source=dcmplx(w))
        call fourier1_dcomplex_to_dcomplex(wf)
    end function fft1_double_to_dcomplex
    function fft1_pad_dcomplex_to_dcomplex(w, n) result(wf)
        double complex, dimension(:), intent(in) :: w
        integer, intent(in) :: n
        double complex, allocatable, dimension(:) :: wf
        wf = adjust(w, n)
        call fourier1_dcomplex_to_dcomplex(wf)
    end function fft1_pad_dcomplex_to_dcomplex
    function fft1_pad_double_to_dcomplex(w, n) result(wf)
        double precision, dimension(:), intent(in) :: w
        integer, intent(in) :: n
        double complex, allocatable, dimension(:) :: wf
        wf = adjust(dcmplx(w), n)
        call fourier1_dcomplex_to_dcomplex(wf)
    end function fft1_pad_double_to_dcomplex
    function ifft1_dcomplex_to_dcomplex(w) result(wf)
        double complex, dimension(:), intent(in) :: w
        double complex, allocatable, dimension(:) :: wf
        allocate (wf(1:size(w)), source=w)
        call ifourier1_dcomplex_to_dcomplex(wf)
    end function ifft1_dcomplex_to_dcomplex
    function ifft1_pad_dcomplex_to_dcomplex(w, n) result(wf)
        double complex, dimension(:), intent(in) :: w
        integer, intent(in) :: n
        double complex, allocatable, dimension(:) :: wf
        wf = adjust(w, n)
        call ifourier1_dcomplex_to_dcomplex(wf)
    end function ifft1_pad_dcomplex_to_dcomplex
    function ifft1_dcomplex_to_double(w, real) result(wf)
        double complex, dimension(:), intent(in) :: w
        logical, intent(in) :: real
        double precision, allocatable, dimension(:) :: wf
        double complex, allocatable, dimension(:) :: ww
        call assert(real, 'Warning: real must be true.')
        ww = w
        call ifourier1_dcomplex_to_dcomplex(ww)
        wf = ww%re
    end function ifft1_dcomplex_to_double
    function ifft1_pad_dcomplex_to_double(w, n, real) result(wf)
        double complex, dimension(:), intent(in) :: w
        integer, intent(in) :: n
        logical, intent(in) :: real
        double precision, allocatable, dimension(:) :: wf
        double complex, allocatable, dimension(:) :: ww
        call assert(real, 'Warning: real must be true.')
        ww = adjust(w, n)
        call ifourier1_dcomplex_to_dcomplex(ww)
        wf = ww%re
    end function ifft1_pad_dcomplex_to_double
    ! ======================================================
    ! FFT -- complex/float to complex, and their padded versioin
    function fft2_complex_to_complex(w, along) result(wf)
        complex, dimension(:, :), intent(in) :: w
        integer, optional :: along
        complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 2)
                        call fourier1_complex_to_complex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 1)
                        call fourier1_complex_to_complex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier2_complex_to_complex(wf)
        end if
    end function fft2_complex_to_complex
    function fft2_float_to_complex(w, along) result(wf)
        real, dimension(:, :), intent(in) :: w
        integer, optional :: along
        complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = cmplx(w)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 2)
                        call fourier1_complex_to_complex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 1)
                        call fourier1_complex_to_complex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier2_complex_to_complex(wf)
        end if
    end function fft2_float_to_complex
    function fft2_pad_complex_to_complex(w, n, along) result(wf)
        complex, dimension(:, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 2)
                        call fourier1_complex_to_complex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 1)
                        call fourier1_complex_to_complex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier2_complex_to_complex(wf)
        end if
    end function fft2_pad_complex_to_complex
    function fft2_pad_float_to_complex(w, n, along) result(wf)
        real, dimension(:, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = adjust(cmplx(w), n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 2)
                        call fourier1_complex_to_complex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 1)
                        call fourier1_complex_to_complex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier2_complex_to_complex(wf)
        end if
    end function fft2_pad_float_to_complex
    function fft2_dcomplex_to_dcomplex(w, along) result(wf)
        double complex, dimension(:, :), intent(in) :: w
        integer, optional :: along
        double complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 2)
                        call fourier1_dcomplex_to_dcomplex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 1)
                        call fourier1_dcomplex_to_dcomplex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier2_dcomplex_to_dcomplex(wf)
        end if
    end function fft2_dcomplex_to_dcomplex
    function fft2_double_to_dcomplex(w, along) result(wf)
        double precision, dimension(:, :), intent(in) :: w
        integer, optional :: along
        double complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = dcmplx(w)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 2)
                        call fourier1_dcomplex_to_dcomplex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 1)
                        call fourier1_dcomplex_to_dcomplex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier2_dcomplex_to_dcomplex(wf)
        end if
    end function fft2_double_to_dcomplex
    function fft2_pad_dcomplex_to_dcomplex(w, n, along) result(wf)
        double complex, dimension(:, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        double complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 2)
                        call fourier1_dcomplex_to_dcomplex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 1)
                        call fourier1_dcomplex_to_dcomplex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier2_dcomplex_to_dcomplex(wf)
        end if
    end function fft2_pad_dcomplex_to_dcomplex
    function fft2_pad_double_to_dcomplex(w, n, along) result(wf)
        double precision, dimension(:, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        double complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = adjust(dcmplx(w), n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 2)
                        call fourier1_dcomplex_to_dcomplex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 1)
                        call fourier1_dcomplex_to_dcomplex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier2_dcomplex_to_dcomplex(wf)
        end if
    end function fft2_pad_double_to_dcomplex
    function ifft2_complex_to_complex(w, along) result(wf)
        complex, dimension(:, :), intent(in) :: w
        integer, optional :: along
        complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 2)
                        call ifourier1_complex_to_complex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 1)
                        call ifourier1_complex_to_complex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier2_complex_to_complex(wf)
        end if
    end function ifft2_complex_to_complex
    function ifft2_complex_to_float(w, real, along) result(wf)
        complex, dimension(:, :), intent(in) :: w
        logical, intent(in) :: real
        integer, optional :: along
        real, allocatable, dimension(:, :) :: wf
        integer :: i
        complex, allocatable, dimension(:, :) :: ww
        call assert(real, 'Error: real must be true.')
        ww = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 2)
                        call ifourier1_complex_to_complex(ww(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 1)
                        call ifourier1_complex_to_complex(ww(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier2_complex_to_complex(ww)
        end if
        wf = ww%re
    end function ifft2_complex_to_float
    function ifft2_pad_complex_to_complex(w, n, along) result(wf)
        complex, dimension(:, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 2)
                        call ifourier1_complex_to_complex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 1)
                        call ifourier1_complex_to_complex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier2_complex_to_complex(wf)
        end if
    end function ifft2_pad_complex_to_complex
    function ifft2_pad_complex_to_float(w, n, real, along) result(wf)
        complex, dimension(:, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        logical, intent(in) :: real
        integer, optional :: along
        real, allocatable, dimension(:, :) :: wf
        integer :: i
        complex, allocatable, dimension(:, :) :: ww
        call assert(real, 'Error: real must be true.')
        ww = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(ww, 2)
                        call ifourier1_complex_to_complex(ww(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(ww, 1)
                        call ifourier1_complex_to_complex(ww(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier2_complex_to_complex(ww)
        end if
        wf = ww%re
    end function ifft2_pad_complex_to_float
    function ifft2_dcomplex_to_dcomplex(w, along) result(wf)
        double complex, dimension(:, :), intent(in) :: w
        integer, optional :: along
        double complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 2)
                        call ifourier1_dcomplex_to_dcomplex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 1)
                        call ifourier1_dcomplex_to_dcomplex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier2_dcomplex_to_dcomplex(wf)
        end if
    end function ifft2_dcomplex_to_dcomplex
    function ifft2_dcomplex_to_double(w, real, along) result(wf)
        double complex, dimension(:, :), intent(in) :: w
        logical, intent(in) :: real
        integer, optional :: along
        double precision, allocatable, dimension(:, :) :: wf
        integer :: i
        double complex, allocatable, dimension(:, :) :: ww
        call assert(real, 'Error: double precision must be true.')
        ww = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 2)
                        call ifourier1_dcomplex_to_dcomplex(ww(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(w, 1)
                        call ifourier1_dcomplex_to_dcomplex(ww(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier2_dcomplex_to_dcomplex(ww)
        end if
        wf = ww%re
    end function ifft2_dcomplex_to_double
    function ifft2_pad_dcomplex_to_dcomplex(w, n, along) result(wf)
        double complex, dimension(:, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        double complex, allocatable, dimension(:, :) :: wf
        integer :: i
        wf = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 2)
                        call ifourier1_dcomplex_to_dcomplex(wf(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(wf, 1)
                        call ifourier1_dcomplex_to_dcomplex(wf(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier2_dcomplex_to_dcomplex(wf)
        end if
    end function ifft2_pad_dcomplex_to_dcomplex
    function ifft2_pad_dcomplex_to_double(w, n, real, along) result(wf)
        double complex, dimension(:, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        logical, intent(in) :: real
        integer, optional :: along
        double precision, allocatable, dimension(:, :) :: wf
        integer :: i
        double complex, allocatable, dimension(:, :) :: ww
        call assert(real, 'Error: double precision must be true.')
        ww = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i)
                    do i = 1, size(ww, 2)
                        call ifourier1_dcomplex_to_dcomplex(ww(:, i))
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i)
                    do i = 1, size(ww, 1)
                        call ifourier1_dcomplex_to_dcomplex(ww(i, :))
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier2_dcomplex_to_dcomplex(ww)
        end if
        wf = ww%re
    end function ifft2_pad_dcomplex_to_double
    function fft3_complex_to_complex(w, along) result(wf)
        complex, dimension(:, :, :), intent(in) :: w
        integer, optional :: along
        complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 2)
                            call fourier1_complex_to_complex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 1)
                            call fourier1_complex_to_complex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 2)
                        do i = 1, size(w, 1)
                            call fourier1_complex_to_complex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier3_complex_to_complex(wf)
        end if
    end function fft3_complex_to_complex
    function fft3_float_to_complex(w, along) result(wf)
        real, dimension(:, :, :), intent(in) :: w
        integer, optional :: along
        complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = cmplx(w)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 2)
                            call fourier1_complex_to_complex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 1)
                            call fourier1_complex_to_complex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 2)
                        do i = 1, size(w, 1)
                            call fourier1_complex_to_complex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier3_complex_to_complex(wf)
        end if
    end function fft3_float_to_complex
    function fft3_pad_complex_to_complex(w, n, along) result(wf)
        complex, dimension(:, :, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 2)
                            call fourier1_complex_to_complex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 1)
                            call fourier1_complex_to_complex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 2)
                        do i = 1, size(wf, 1)
                            call fourier1_complex_to_complex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier3_complex_to_complex(wf)
        end if
    end function fft3_pad_complex_to_complex
    function fft3_pad_float_to_complex(w, n, along) result(wf)
        real, dimension(:, :, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = adjust(cmplx(w), n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 2)
                            call fourier1_complex_to_complex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 1)
                            call fourier1_complex_to_complex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 2)
                        do i = 1, size(wf, 1)
                            call fourier1_complex_to_complex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier3_complex_to_complex(wf)
        end if
    end function fft3_pad_float_to_complex
    function ifft3_complex_to_complex(w, along) result(wf)
        complex, dimension(:, :, :), intent(in) :: w
        integer, optional :: along
        complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 2)
                            call ifourier1_complex_to_complex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 1)
                            call ifourier1_complex_to_complex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 2)
                        do i = 1, size(w, 1)
                            call ifourier1_complex_to_complex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier3_complex_to_complex(wf)
        end if
    end function ifft3_complex_to_complex
    function ifft3_complex_to_float(w, real, along) result(wf)
        complex, dimension(:, :, :), intent(in) :: w
        logical, intent(in) :: real
        integer, optional :: along
        real, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        complex, allocatable, dimension(:, :, :) :: ww
        call assert(real, 'Error: real must be true.')
        ww = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 2)
                            call ifourier1_complex_to_complex(ww(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 1)
                            call ifourier1_complex_to_complex(ww(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 2)
                        do i = 1, size(w, 1)
                            call ifourier1_complex_to_complex(ww(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier3_complex_to_complex(ww)
        end if
        wf = ww%re
    end function ifft3_complex_to_float
    function ifft3_pad_complex_to_complex(w, n, along) result(wf)
        complex, dimension(:, :, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 2)
                            call ifourier1_complex_to_complex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 1)
                            call ifourier1_complex_to_complex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 2)
                        do i = 1, size(wf, 1)
                            call ifourier1_complex_to_complex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier3_complex_to_complex(wf)
        end if
    end function ifft3_pad_complex_to_complex
    function ifft3_pad_complex_to_float(w, n, real, along) result(wf)
        complex, dimension(:, :, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        logical, intent(in) :: real
        integer, optional :: along
        real, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        complex, allocatable, dimension(:, :, :) :: ww
        call assert(real, 'Error: real must be true.')
        ww = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(ww, 3)
                        do i = 1, size(ww, 2)
                            call ifourier1_complex_to_complex(ww(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(ww, 3)
                        do i = 1, size(ww, 1)
                            call ifourier1_complex_to_complex(ww(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(ww, 2)
                        do i = 1, size(ww, 1)
                            call ifourier1_complex_to_complex(ww(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier3_complex_to_complex(ww)
        end if
        wf = ww%re
    end function ifft3_pad_complex_to_float
    function fft3_dcomplex_to_dcomplex(w, along) result(wf)
        double complex, dimension(:, :, :), intent(in) :: w
        integer, optional :: along
        double complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 2)
                            call fourier1_dcomplex_to_dcomplex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 1)
                            call fourier1_dcomplex_to_dcomplex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 2)
                        do i = 1, size(w, 1)
                            call fourier1_dcomplex_to_dcomplex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier3_dcomplex_to_dcomplex(wf)
        end if
    end function fft3_dcomplex_to_dcomplex
    function fft3_double_to_dcomplex(w, along) result(wf)
        double precision, dimension(:, :, :), intent(in) :: w
        integer, optional :: along
        double complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = dcmplx(w)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 2)
                            call fourier1_dcomplex_to_dcomplex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 1)
                            call fourier1_dcomplex_to_dcomplex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 2)
                        do i = 1, size(w, 1)
                            call fourier1_dcomplex_to_dcomplex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier3_dcomplex_to_dcomplex(wf)
        end if
    end function fft3_double_to_dcomplex
    function fft3_pad_dcomplex_to_dcomplex(w, n, along) result(wf)
        double complex, dimension(:, :, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        double complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 2)
                            call fourier1_dcomplex_to_dcomplex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 1)
                            call fourier1_dcomplex_to_dcomplex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 2)
                        do i = 1, size(wf, 1)
                            call fourier1_dcomplex_to_dcomplex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier3_dcomplex_to_dcomplex(wf)
        end if
    end function fft3_pad_dcomplex_to_dcomplex
    function fft3_pad_double_to_dcomplex(w, n, along) result(wf)
        double precision, dimension(:, :, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        double complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = adjust(dcmplx(w), n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 2)
                            call fourier1_dcomplex_to_dcomplex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 1)
                            call fourier1_dcomplex_to_dcomplex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 2)
                        do i = 1, size(wf, 1)
                            call fourier1_dcomplex_to_dcomplex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call fourier3_dcomplex_to_dcomplex(wf)
        end if
    end function fft3_pad_double_to_dcomplex
    function ifft3_dcomplex_to_dcomplex(w, along) result(wf)
        double complex, dimension(:, :, :), intent(in) :: w
        integer, optional :: along
        double complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 2)
                            call ifourier1_dcomplex_to_dcomplex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 1)
                            call ifourier1_dcomplex_to_dcomplex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 2)
                        do i = 1, size(w, 1)
                            call ifourier1_dcomplex_to_dcomplex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier3_dcomplex_to_dcomplex(wf)
        end if
    end function ifft3_dcomplex_to_dcomplex
    function ifft3_dcomplex_to_double(w, real, along) result(wf)
        double complex, dimension(:, :, :), intent(in) :: w
        logical, intent(in) :: real
        integer, optional :: along
        double precision, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        double complex, allocatable, dimension(:, :, :) :: ww
        call assert(real, 'Error: real must be true.')
        ww = w
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 2)
                            call ifourier1_dcomplex_to_dcomplex(ww(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 3)
                        do i = 1, size(w, 1)
                            call ifourier1_dcomplex_to_dcomplex(ww(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(w, 2)
                        do i = 1, size(w, 1)
                            call ifourier1_dcomplex_to_dcomplex(ww(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier3_dcomplex_to_dcomplex(ww)
        end if
        wf = ww%re
    end function ifft3_dcomplex_to_double
    function ifft3_pad_dcomplex_to_dcomplex(w, n, along) result(wf)
        double complex, dimension(:, :, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        integer, optional :: along
        double complex, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        wf = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 2)
                            call ifourier1_dcomplex_to_dcomplex(wf(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 3)
                        do i = 1, size(wf, 1)
                            call ifourier1_dcomplex_to_dcomplex(wf(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(wf, 2)
                        do i = 1, size(wf, 1)
                            call ifourier1_dcomplex_to_dcomplex(wf(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier3_dcomplex_to_dcomplex(wf)
        end if
    end function ifft3_pad_dcomplex_to_dcomplex
    function ifft3_pad_dcomplex_to_double(w, n, real, along) result(wf)
        double complex, dimension(:, :, :), intent(in) :: w
        integer, dimension(:), intent(in) :: n
        logical, intent(in) :: real
        integer, optional :: along
        double precision, allocatable, dimension(:, :, :) :: wf
        integer :: i, j
        double complex, allocatable, dimension(:, :, :) :: ww
        call assert(real, 'Error: real must be true.')
        ww = adjust(w, n)
        if (present(along)) then
            select case (along)
                case (1)
                    !$omp parallel do private(i, j)
                    do j = 1, size(ww, 3)
                        do i = 1, size(ww, 2)
                            call ifourier1_dcomplex_to_dcomplex(ww(:, i, j))
                        end do
                    end do
                    !$omp end parallel do
                case (2)
                    !$omp parallel do private(i, j)
                    do j = 1, size(ww, 3)
                        do i = 1, size(ww, 1)
                            call ifourier1_dcomplex_to_dcomplex(ww(i, :, j))
                        end do
                    end do
                    !$omp end parallel do
                case (3)
                    !$omp parallel do private(i, j)
                    do j = 1, size(ww, 2)
                        do i = 1, size(ww, 1)
                            call ifourier1_dcomplex_to_dcomplex(ww(i, j, :))
                        end do
                    end do
                    !$omp end parallel do
            end select
        else
            call ifourier3_dcomplex_to_dcomplex(ww)
        end if
        wf = ww%re
    end function ifft3_pad_dcomplex_to_double
    !
    !> Compute frequencies corresponding to FFT
    !
    function fft_omega(n, dx, s) result(k)
        integer, intent(in) :: n
        real, intent(in), optional :: dx
        real, intent(in), optional :: s
        real, allocatable, dimension(:) :: k
        integer :: i, nq
        real :: dk
        real :: ss
        if (present(dx)) then
            dk = (2.0*const_pi)/n/dx
        else
            dk = (2.0*const_pi)/n
        end if
        if (present(s)) then
            ss = s
        else
            ss = 0.0
        end if
        allocate (k(1:n))
        do i = 1, n
            k(i) = (i - 1 - ss)*dk
        end do
        nq = nint(n/2.0)
        k(nq + 1:n) = k(nq + 1:n) - n*dk
        ! if (mod(n, 2) == 0) then
        ! k(n/2 + 1:n) = k(n/2 + 1:n) - n*dk
        ! else
        ! k((n + 1)/2 + 1:n) = k((n + 1)/2 + 1:n) - n*dk
        ! end if
    end function fft_omega
    !=================================================================
    ! Hilbert transform
    subroutine hilbert_transform_1d_float(array, hthalf)
        ! Parameters
        real, dimension(:), intent(inout) :: array
        integer, intent(in), optional :: hthalf
        integer :: hbhalf, n, i
        real, allocatable, dimension(:) :: hb, w
        real :: hamming
        if (present(hthalf)) then
            hbhalf = hthalf
        else
            hbhalf = 30
        end if
        ! Optimal Hamming window coefficient
        ! See https://en.wikipedia.org/wiki/Window_function#Hann_and_Hamming_windows
        ! and
        ! https://ccrma.stanford.edu/~jos/sasp/Hamming_Window.html
        hamming = 0.53836
        ! Approximate Hilbert transform using Hamming window
        allocate (hb(0:2*hbhalf))
        hb(hbhalf) = 0.0
        do i = 1, hbhalf
            hb(hbhalf + i) = (hamming + (1.0 - hamming)*cos(const_pi*i/hbhalf))*(-real(mod(i, 2))*2.0/(const_pi*i))
            hb(hbhalf - i) = -hb(hbhalf + i)
        end do
        ! temporary array to hold array
        n = size(array)
        allocate (w(1 - hbhalf:n + hbhalf))
        w = 0.0
        w(1:n) = array
        ! convolution
        do i = 1, n
            array(i) = sum(w(i - hbhalf:i + hbhalf)*hb)
        end do
        deallocate (hb, w)
    end subroutine hilbert_transform_1d_float
    subroutine hilbert_transform_1d_double(array, hthalf)
        ! Parameters
        double precision, dimension(:), intent(inout) :: array
        integer, intent(in), optional :: hthalf
        integer :: hbhalf, n, i
        double precision, allocatable, dimension(:) :: hb, w
        double precision :: hamming
        if (present(hthalf)) then
            hbhalf = hthalf
        else
            hbhalf = 30
        end if
        ! Optimal Hamming window coefficient
        hamming = 0.53836
        ! Approximate Hilbert transform using Hamming window
        allocate (hb(0:2*hbhalf))
        hb(hbhalf) = 0.0
        do i = 1, hbhalf
            hb(hbhalf + i) = (hamming + (1.0 - hamming)*cos(const_pi*i/hbhalf))*(-dble(mod(i, 2))*2.0/(const_pi*i))
            hb(hbhalf - i) = -hb(hbhalf + i)
        end do
        ! temporary array to hold array
        n = size(array)
        allocate (w(1 - hbhalf:n + hbhalf))
        w = 0.0
        w(1:n) = array
        ! convolution
        do i = 1, n
            array(i) = sum(w(i - hbhalf:i + hbhalf)*hb)
        end do
        deallocate (hb, w)
    end subroutine hilbert_transform_1d_double
    ! function hilbert_1d_fft(w) result(wh)
    !
    ! real, dimension(:) :: w
    ! real, allocatable, dimension(:) :: wh
    !
    ! integer :: n, nn, i
    ! double precision, allocatable, dimension(:) :: h
    !
    ! n = size(w)
    ! nn = n !next_power_2(n)
    !
    ! allocate(h(-nn/2 + 1:nn/2))
    ! do i = -nn/2 + 1, nn/2
    ! h(i) = -(i + 1.0)*log(i*1.0/(i + 1.0)) + (i - 1.0)*log((i - 1.0)/i)
    ! end do
    ! i = -1
    ! h(i) = + (i - 1)*log((i - 1.0)/i)
    ! h(0) = 0.0d0
    ! i = +1
    ! h(i) = -(i + 1)*log(i/(i + 1.0))
    ! h = h/const_pi
    !
    ! allocate(wh(1:n))
    ! wh = crop(conv(pad(w, [0, nn - n]), real(h)), [1, n])
    ! wh(1:n/2) = wh(1:n/2) + wh(n/2+1:n)
    !
    ! end function hilbert_1d_fft
    function hilbert_1d_float(array, hthalf) result(w)
        ! Parameters
        real, dimension(:), intent(in) :: array
        integer, intent(in), optional :: hthalf
        integer :: hbhalf
        real, allocatable, dimension(:) :: w
        if (present(hthalf)) then
            hbhalf = hthalf
        else
            hbhalf = 30
        end if
        allocate (w(1:size(array)), source=array)
        call hilbert_transform_1d_float(w, hbhalf)
    end function hilbert_1d_float
    function hilbert_1d_double(array, hthalf) result(w)
        ! Parameters
        double precision, dimension(:), intent(in) :: array
        integer, intent(in), optional :: hthalf
        integer :: hbhalf
        double precision, allocatable, dimension(:) :: w
        if (present(hthalf)) then
            hbhalf = hthalf
        else
            hbhalf = 30
        end if
        allocate (w(1:size(array)), source=array)
        call hilbert_transform_1d_double(w, hbhalf)
    end function hilbert_1d_double
    !
    !> Calculate the envelope of an 1D array
    !
    subroutine envelope_transform_1d_float(array)
        real, dimension(:), intent(inout) :: array
        array = sqrt(array**2 + hilbert(array, nint(size(array)/5.0))**2)
    end subroutine envelope_transform_1d_float
    function envelope_1d_float(array) result(w)
        real, dimension(:), intent(in) :: array
        real, allocatable, dimension(:) :: w
        call alloc_array(w, [1, size(array)], &
            source=sqrt(array**2 + hilbert(array, nint(size(array)/5.0))**2))
    end function envelope_1d_float
    subroutine envelope_transform_1d_double(array)
        double precision, dimension(:), intent(inout) :: array
        array = sqrt(array**2 + hilbert(array, nint(size(array)/5.0))**2)
    end subroutine envelope_transform_1d_double
    function envelope_1d_double(array) result(w)
        double precision, dimension(:), intent(in) :: array
        double precision, allocatable, dimension(:) :: w
        call alloc_array(w, [1, size(array)], &
            source=sqrt(array**2 + hilbert(array, nint(size(array)/5.0))**2))
    end function envelope_1d_double
    !===========================================================================
    !
    !> Convert time-domain regularly sampled data to
    !> frequency domain data (not necessarily with
    !> regular frequency sampling)
    !
    function fftd(w, t, f) result(wf)
        real, dimension(:), intent(in) :: w, t, f
        complex, allocatable, dimension(:) :: wf
        integer :: i, j, nf, nt
        nt = size(t)
        call assert(size(w) == nt, 'Error: size(w) /= nt')
        nf = size(f)
        call alloc_array(wf, [1, nf])
        do i = 1, nf
            do j = 1, nt
                wf(i) = wf(i) + w(j)*exp(-2*const_pi*const_i*f(i)*t(j))
            end do
        end do
    end function fftd
    !
    !> Convert frequency domain data (not necessarily with
    !> regular frequency sampling) to time-domain regularly
    !> sampled data
    !
    function ifftd(wf, f, t) result(w)
        complex, dimension(:), intent(in) :: wf
        real, dimension(:), intent(in) :: f, t
        real, allocatable, dimension(:) :: w
        integer :: i, j, nf, nt
        nf = size(f)
        call assert(size(wf) == nf, 'Error: size(wf) /= nf')
        nt = size(t)
        call alloc_array(w, [1, nt])
        do j = 1, nt
            do i = 1, nf
                w(j) = w(j) + real(wf(i)*exp(2*const_pi*const_i*f(i)*t(j)))/nt
            end do
        end do
    end function ifftd
    !
    !> 1D forward discrete cosine transform using FFTW
    !
    subroutine dct_1d_float(w)
        real, dimension(:), intent(inout) :: w
        integer(kind=8) :: r2r
        integer :: n
        real, allocatable, dimension(:) :: wt
        n = size(w)
        r2r = 0
        allocate (wt, source=w)
        call sfftw_plan_r2r_1d(r2r, n, wt, wt, fftw_redft10, fftw_estimate)
        call sfftw_execute(r2r)
        call sfftw_destroy_plan(r2r)
        w = wt/2.0
    end subroutine dct_1d_float
    !
    !> 1D inverse discrete cosine transform using FFTW
    !
    subroutine idct_1d_float(w)
        real, dimension(:), intent(inout) :: w
        integer(kind=8) :: r2r
        integer :: n
        real, allocatable, dimension(:) :: wt
        n = size(w)
        r2r = 0
        allocate (wt, source=w)
        call sfftw_plan_r2r_1d(r2r, n, wt, wt, fftw_redft01, fftw_estimate)
        call sfftw_execute(r2r)
        call sfftw_destroy_plan(r2r)
        w = wt/n
    end subroutine idct_1d_float
    !
    !> 2D forward discrete cosine transform
    !
    subroutine dct_2d_float(w)
        real, dimension(:, :), intent(inout) :: w
        integer :: i, j, n1, n2
        ! real, allocatable, dimension(:) :: wt1, wt2
        n1 = size(w, 1)
        n2 = size(w, 2)
        ! allocate(wt1(1:n1))
        ! allocate(wt2(1:n2))
        ! !$omp parallel do private(j, wt1)
        do j = 1, n2
            ! wt1 = w(:,j)
            ! call dct_1d_float(wt1)
            ! w(:,j) = wt1
            call dct_1d_float(w(:, j))
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, wt2)
        do i = 1, n1
            ! wt2 = w(i,:)
            ! call dct_1d_float(wt2)
            ! w(i,:) = wt2
            call dct_1d_float(w(i, :))
        end do
        ! !$omp end parallel do
    end subroutine dct_2d_float
    !
    !> 2D forward discrete cosine transform
    !
    !
    subroutine idct_2d_float(w)
        real, dimension(:, :), intent(inout) :: w
        integer :: i, j, n1, n2
        ! real, allocatable, dimension(:) :: wt1, wt2
        n1 = size(w, 1)
        n2 = size(w, 2)
        ! allocate(wt1(1:n1))
        ! allocate(wt2(1:n2))
        ! !$omp parallel do private(j, wt1)
        do j = 1, n2
            ! wt1 = w(:,j)
            ! call idct_1d_float(wt1)
            ! w(:,j) = wt1
            call idct_1d_float(w(:, j))
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, wt2)
        do i = 1, n1
            ! wt2 = w(i,:)
            ! call idct_1d_float(wt2)
            ! w(i,:) = wt2
            call idct_1d_float(w(i, :))
        end do
        ! !$omp end parallel do
    end subroutine idct_2d_float
    !
    !> 3D forward discrete cosine transform
    !
    !
    subroutine dct_3d_float(w)
        real, dimension(:, :, :), intent(inout) :: w
        integer :: i, j, k, n1, n2, n3
        ! real, allocatable, dimension(:) :: wt1, wt2, wt3
        n1 = size(w, 1)
        n2 = size(w, 2)
        n3 = size(w, 3)
        ! allocate(wt1(1:n1))
        ! allocate(wt2(1:n2))
        ! allocate(wt3(1:n3))
        ! !$omp parallel do private(j, k, wt1)
        do k = 1, n3
            do j = 1, n2
                ! wt1 = w(:,j,k)
                ! call dct_1d_float(wt1)
                ! w(:,j,k) = wt1
                call dct_1d_float(w(:, j, k))
            end do
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, k, wt2)
        do k = 1, n3
            do i = 1, n1
                ! wt2 = w(i,:,k)
                ! call dct_1d_float(wt2)
                ! w(i,:,k) = wt2
                call dct_1d_float(w(i, :, k))
            end do
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, j, wt3)
        do j = 1, n2
            do i = 1, n1
                ! wt3 = w(i,j,:)
                ! call dct_1d_float(wt3)
                ! w(i,j,:) = wt3
                call dct_1d_float(w(i, j, :))
            end do
        end do
        ! !$omp end parallel do
    end subroutine dct_3d_float
    !
    !> 3D forward discrete cosine transform
    !
    !
    subroutine idct_3d_float(w)
        real, dimension(:, :, :), intent(inout) :: w
        integer :: i, j, k, n1, n2, n3
        ! real, allocatable, dimension(:) :: wt1, wt2, wt3
        n1 = size(w, 1)
        n2 = size(w, 2)
        n3 = size(w, 3)
        ! allocate(wt1(1:n1))
        ! allocate(wt2(1:n2))
        ! allocate(wt3(1:n3))
        ! !$omp parallel do private(j, k, wt1)
        do k = 1, n3
            do j = 1, n2
                ! wt1 = w(:,j,k)
                ! call dct_1d_float(wt1)
                ! w(:,j,k) = wt1
                call idct_1d_float(w(:, j, k))
            end do
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, k, wt2)
        do k = 1, n3
            do i = 1, n1
                ! wt2 = w(i,:,k)
                ! call dct_1d_float(wt2)
                ! w(i,:,k) = wt2
                call idct_1d_float(w(i, :, k))
            end do
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, j, wt3)
        do j = 1, n2
            do i = 1, n1
                ! wt3 = w(i,j,:)
                ! call dct_1d_float(wt3)
                ! w(i,j,:) = wt3
                call idct_1d_float(w(i, j, :))
            end do
        end do
        ! !$omp end parallel do
    end subroutine idct_3d_float
    !
    !> 1D forward DCT function scheme
    !
    !
    function dct1_float(w) result(wdct)
        real, dimension(:), intent(in) :: w
        real, dimension(:), allocatable :: wdct
        allocate (wdct(1:size(w)), source=w)
        call dct_1d_float(wdct)
    end function dct1_float
    !
    !> 1D inverse DCT function scheme
    !
    !
    function idct1_float(w) result(wdct)
        real, dimension(:), intent(in) :: w
        real, dimension(:), allocatable :: wdct
        allocate (wdct(1:size(w)), source=w)
        call idct_1d_float(wdct)
    end function idct1_float
    !
    !> 2D forward DCT function scheme
    !
    !
    function dct2_float(w) result(wdct)
        real, dimension(:, :), intent(in) :: w
        real, dimension(:, :), allocatable :: wdct
        allocate (wdct(1:size(w, 1), 1:size(w, 2)), source=w)
        call dct_2d_float(wdct)
    end function dct2_float
    !
    !> 2D inverse DCT function scheme
    !
    !
    function idct2_float(w) result(wdct)
        real, dimension(:, :), intent(in) :: w
        real, dimension(:, :), allocatable :: wdct
        allocate (wdct(1:size(w, 1), 1:size(w, 2)), source=w)
        call idct_2d_float(wdct)
    end function idct2_float
    !
    !> 3D forward DCT function scheme
    !
    !
    function dct3_float(w) result(wdct)
        real, dimension(:, :, :), intent(in) :: w
        real, dimension(:, :, :), allocatable :: wdct
        allocate (wdct(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)), source=w)
        call dct_3d_float(wdct)
    end function dct3_float
    !
    !> 3D inverse DCT function scheme
    !
    function idct3_float(w) result(wdct)
        real, dimension(:, :, :), intent(in) :: w
        real, dimension(:, :, :), allocatable :: wdct
        allocate (wdct(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)), source=w)
        call idct_3d_float(wdct)
    end function idct3_float
    !
    !> 1D forward discrete cosine transform using FFTW
    !
    subroutine dct_1d_double(w)
        double precision, dimension(:), intent(inout) :: w
        integer(kind=8) :: r2r
        integer :: n
        double precision, allocatable, dimension(:) :: wt
        n = size(w)
        r2r = 0
        allocate (wt, source=w)
        call dfftw_plan_r2r_1d(r2r, n, wt, wt, fftw_redft10, fftw_estimate)
        call dfftw_execute(r2r)
        call dfftw_destroy_plan(r2r)
        w = wt/2.0
    end subroutine dct_1d_double
    !
    !> 1D inverse discrete cosine transform using FFTW
    !
    subroutine idct_1d_double(w)
        double precision, dimension(:), intent(inout) :: w
        integer(kind=8) :: r2r
        integer :: n
        double precision, allocatable, dimension(:) :: wt
        n = size(w)
        r2r = 0
        allocate (wt, source=w)
        call dfftw_plan_r2r_1d(r2r, n, wt, wt, fftw_redft01, fftw_estimate)
        call dfftw_execute(r2r)
        call dfftw_destroy_plan(r2r)
        w = wt/n
    end subroutine idct_1d_double
    !
    !> 2D forward discrete cosine transform
    !
    subroutine dct_2d_double(w)
        double precision, dimension(:, :), intent(inout) :: w
        integer :: i, j, n1, n2
        ! double precision, allocatable, dimension(:) :: wt1, wt2
        n1 = size(w, 1)
        n2 = size(w, 2)
        ! allocate(wt1(1:n1))
        ! allocate(wt2(1:n2))
        ! !$omp parallel do private(j, wt1)
        do j = 1, n2
            ! wt1 = w(:,j)
            ! call dct_1d_double(wt1)
            ! w(:,j) = wt1
            call dct_1d_double(w(:, j))
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, wt2)
        do i = 1, n1
            ! wt2 = w(i,:)
            ! call dct_1d_double(wt2)
            ! w(i,:) = wt2
            call dct_1d_double(w(i, :))
        end do
        ! !$omp end parallel do
    end subroutine dct_2d_double
    !
    !> 2D forward discrete cosine transform
    !
    !
    subroutine idct_2d_double(w)
        double precision, dimension(:, :), intent(inout) :: w
        integer :: i, j, n1, n2
        ! double precision, allocatable, dimension(:) :: wt1, wt2
        n1 = size(w, 1)
        n2 = size(w, 2)
        ! allocate(wt1(1:n1))
        ! allocate(wt2(1:n2))
        ! !$omp parallel do private(j, wt1)
        do j = 1, n2
            ! wt1 = w(:,j)
            ! call idct_1d_double(wt1)
            ! w(:,j) = wt1
            call idct_1d_double(w(:, j))
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, wt2)
        do i = 1, n1
            ! wt2 = w(i,:)
            ! call idct_1d_double(wt2)
            ! w(i,:) = wt2
            call idct_1d_double(w(i, :))
        end do
        ! !$omp end parallel do
    end subroutine idct_2d_double
    !
    !> 3D forward discrete cosine transform
    !
    !
    subroutine dct_3d_double(w)
        double precision, dimension(:, :, :), intent(inout) :: w
        integer :: i, j, k, n1, n2, n3
        ! double precision, allocatable, dimension(:) :: wt1, wt2, wt3
        n1 = size(w, 1)
        n2 = size(w, 2)
        n3 = size(w, 3)
        ! allocate(wt1(1:n1))
        ! allocate(wt2(1:n2))
        ! allocate(wt3(1:n3))
        ! !$omp parallel do private(j, k, wt1)
        do k = 1, n3
            do j = 1, n2
                ! wt1 = w(:,j,k)
                ! call dct_1d_double(wt1)
                ! w(:,j,k) = wt1
                call dct_1d_double(w(:, j, k))
            end do
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, k, wt2)
        do k = 1, n3
            do i = 1, n1
                ! wt2 = w(i,:,k)
                ! call dct_1d_double(wt2)
                ! w(i,:,k) = wt2
                call dct_1d_double(w(i, :, k))
            end do
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, j, wt3)
        do j = 1, n2
            do i = 1, n1
                ! wt3 = w(i,j,:)
                ! call dct_1d_double(wt3)
                ! w(i,j,:) = wt3
                call dct_1d_double(w(i, j, :))
            end do
        end do
        ! !$omp end parallel do
    end subroutine dct_3d_double
    !
    !> 3D forward discrete cosine transform
    !
    !
    subroutine idct_3d_double(w)
        double precision, dimension(:, :, :), intent(inout) :: w
        integer :: i, j, k, n1, n2, n3
        ! double precision, allocatable, dimension(:) :: wt1, wt2, wt3
        n1 = size(w, 1)
        n2 = size(w, 2)
        n3 = size(w, 3)
        ! allocate(wt1(1:n1))
        ! allocate(wt2(1:n2))
        ! allocate(wt3(1:n3))
        ! !$omp parallel do private(j, k, wt1)
        do k = 1, n3
            do j = 1, n2
                ! wt1 = w(:,j,k)
                ! call dct_1d_double(wt1)
                ! w(:,j,k) = wt1
                call idct_1d_double(w(:, j, k))
            end do
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, k, wt2)
        do k = 1, n3
            do i = 1, n1
                ! wt2 = w(i,:,k)
                ! call dct_1d_double(wt2)
                ! w(i,:,k) = wt2
                call idct_1d_double(w(i, :, k))
            end do
        end do
        ! !$omp end parallel do
        ! !$omp parallel do private(i, j, wt3)
        do j = 1, n2
            do i = 1, n1
                ! wt3 = w(i,j,:)
                ! call dct_1d_double(wt3)
                ! w(i,j,:) = wt3
                call idct_1d_double(w(i, j, :))
            end do
        end do
        ! !$omp end parallel do
    end subroutine idct_3d_double
    !
    !> 1D forward DCT function scheme
    !
    !
    function dct1_double(w) result(wdct)
        double precision, dimension(:), intent(in) :: w
        double precision, dimension(:), allocatable :: wdct
        allocate (wdct(1:size(w)), source=w)
        call dct_1d_double(wdct)
    end function dct1_double
    !
    !> 1D inverse DCT function scheme
    !
    !
    function idct1_double(w) result(wdct)
        double precision, dimension(:), intent(in) :: w
        double precision, dimension(:), allocatable :: wdct
        allocate (wdct(1:size(w)), source=w)
        call idct_1d_double(wdct)
    end function idct1_double
    !
    !> 2D forward DCT function scheme
    !
    !
    function dct2_double(w) result(wdct)
        double precision, dimension(:, :), intent(in) :: w
        double precision, dimension(:, :), allocatable :: wdct
        allocate (wdct(1:size(w, 1), 1:size(w, 2)), source=w)
        call dct_2d_double(wdct)
    end function dct2_double
    !
    !> 2D inverse DCT function scheme
    !
    !
    function idct2_double(w) result(wdct)
        double precision, dimension(:, :), intent(in) :: w
        double precision, dimension(:, :), allocatable :: wdct
        allocate (wdct(1:size(w, 1), 1:size(w, 2)), source=w)
        call idct_2d_double(wdct)
    end function idct2_double
    !
    !> 3D forward DCT function scheme
    !
    !
    function dct3_double(w) result(wdct)
        double precision, dimension(:, :, :), intent(in) :: w
        double precision, dimension(:, :, :), allocatable :: wdct
        allocate (wdct(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)), source=w)
        call dct_3d_double(wdct)
    end function dct3_double
    !
    !> 3D inverse DCT function scheme
    !
    function idct3_double(w) result(wdct)
        double precision, dimension(:, :, :), intent(in) :: w
        double precision, dimension(:, :, :), allocatable :: wdct
        allocate (wdct(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)), source=w)
        call idct_3d_double(wdct)
    end function idct3_double
    !
    !> Arbitrary phase shift using discrete Hilbert transform
    !> May not be perfectly accurate due to errors in the Hilbert transform
    !
    !> @note [From StackOverflow] You can't design a filter that creates a phase
    !> shift that's constant with frequency for real valued input
    !> (if that's what you are trying to do).
    !> A Hilbert transformer appears to be doing this.
    !> However, the problem is, you can't implement a perfect
    !> Hilbert transformer since it's non-causal with an infinite length impulse response.
    !> The tricky part is that the DC and Nyquist components
    !> of the spectrum have to be real valued and that the phase
    !> at these frequencies is constrained to 0 or pi.
    !> Any phase shifter would have to have a transition band
    !> from DC to the to target phase and from the target phase to Nyquist.
    !> That's exactly what happens if you design a real-world Hilbert transformer:
    !> You trade off the size of the transition bands against
    !> the length & complexity of the filter.
    !> The class of filters that only manipulates phase
    !> and not the amplitude are called allpass filters. One can easily show that
    !> 1) Allpass filters have a phase that's monotonously decreasing
    !> 2) For an allpass filter of oder N the phase from DC to Nyquist decreases by N*pi
    !> 3) Allpass have zeros that are inverses of the poles,
    !> 4) The z-transform numerator polynomial is the reverse of denominator polynomial
    !> Of course you can always design an "approximation" that's
    !> good enough for your requirement. You'd have to specify
    !> the target bandwidth, max phase error, max amplitude
    !> error and then deploy a suitable FIR or IIR design method
    !> Even an approximate design of absolute phase shift
    !> is quite difficult, especially if you have latency
    !> or causality constraints. Constant relative phase shift
    !> is a lot easier. A popular technique is to run a signal
    !> through two parallel allpass filters which are designed
    !> to have a constant phase difference over the frequency area of interest.
    !> The outputs have identical amplitude as the input but have approximately
    !> constant phase shift with respect to each other (although not with respect to the input).
    !
    function phase_shift_float(w, shift) result(wr)
        real, dimension(:) :: w
        real :: shift
        real, allocatable, dimension(:) :: wr
        allocate (wr(1:size(w)))
        wr = cos(shift)*w + sin(shift)*hilbert(w)
    end function phase_shift_float
    function phase_shift_double(w, shift) result(wr)
        double precision, dimension(:) :: w
        double precision :: shift
        double precision, allocatable, dimension(:) :: wr
        allocate (wr(1:size(w)))
        wr = cos(shift)*w + sin(shift)*hilbert(w)
    end function phase_shift_double
    !
    ! # 0. Pad
    ! n = w.shape[0]
    ! n0 = n
    ! nn = next_power_of_2(1.2 * n)
    ! wp = torch.zeros(nn)
    ! wp[:n] = w
    !
    ! # 1. Take the FFT
    ! fftData = fourier.fft(wp)
    !
    ! # 2. Construct the phase shift
    ! tDelayInSamples = shift / dt
    ! N = fftData.shape[0]
    ! k = torch.linspace(0.0, N - 1.0, N)
    ! timeDelayPhaseShift = torch.exp(((-2 * np.pi * 1j * k * tDelayInSamples) / (N)) +
    ! (tDelayInSamples * np.pi * 1j))
    !
    ! # 3. Do the fftshift on the phase shift coefficients
    ! timeDelayPhaseShift = fourier.fftshift(timeDelayPhaseShift)
    !
    ! # 4. Multiply the fft data with the coefficients to apply the time shift
    ! fftWithDelay = torch.mul(fftData, timeDelayPhaseShift)
    !
    ! # 5. Do the IFFT
    ! shiftedWaveform = fourier.ifft(fftWithDelay)
    !
    ! return torch.real(shiftedWaveform[:n0])
    !
    function time_shift_float(w, dt, shift, method) result(wr)
        real, dimension(:) :: w
        real :: dt, shift
        character(len=*), intent(in), optional :: method
        real, allocatable, dimension(:) :: wr
        integer :: n, nn, dl
        real, allocatable, dimension(:) :: k
        complex, allocatable, dimension(:) :: wp
        character(len=12) :: shift_method
        if (present(method)) then
            shift_method = method
        else
            shift_method = 'time'
        end if
        n = size(w)
        dl = nint(shift/dt)
        select case (shift_method)
            case ('time')
                call alloc_array(wr, [1, n], source=cshift(w, shift=-dl))
                wr = taper(wr, [dl, 0])
            case ('fft')
                nn = next_power_2(nint(1.25*n))
                wr = zeros(nn)
                wr(1:n) = w
                k = regspace(0.0, 1.0, nn - 1.0)
                wp = fft(wr)
                wp = wp*fftshift(exp(-2.0*const_pi*const_i*k*dl/nn + dl*const_pi*const_i))
                wp = ifft(wp)
                call alloc_array(wr, [1, n], source=real(wp(1:n)))
        end select
    end function time_shift_float
    function time_shift_double(w, dt, shift, method) result(wr)
        double precision, dimension(:) :: w
        double precision :: dt, shift
        character(len=*), intent(in), optional :: method
        double precision, allocatable, dimension(:) :: wr
        integer :: n, nn, dl
        double precision, allocatable, dimension(:) :: k
        complex, allocatable, dimension(:) :: wp
        character(len=12) :: shift_method
        if (present(method)) then
            shift_method = method
        else
            shift_method = 'time'
        end if
        n = size(w)
        dl = nint(shift/dt)
        select case (shift_method)
            case ('time')
                call alloc_array(wr, [1, n], source=cshift(w, shift=-dl))
                wr = taper(wr, [dl, 0])
            case ('fft')
                nn = next_power_2(nint(1.25*n))
                wr = zeros(nn)
                wr(1:n) = w
                k = regspace(0.0, 1.0, nn - 1.0)
                wp = fft(wr)
                wp = wp*fftshift(exp(-2.0*const_pi*const_i*k*dl/nn + dl*const_pi*const_i))
                wp = ifft(wp)
                call alloc_array(wr, [1, n], source=dble(wp(1:n)))
        end select
    end function time_shift_double
    !
    !> Phase unwrap
    !
    function unwrap_float(p) result(pu)
        real, dimension(:) :: p
        real, allocatable, dimension(:) :: pu
        real :: angle_jump, angle, dx
        integer :: i, n
        n = size(p)
        allocate (pu(1:n), source=p)
        angle_jump = 0.0
        angle = const_pi
        do i = 2, n
            dx = pu(i) - (pu(i - 1) - angle_jump)
            if (abs(dx) > angle) then
                angle_jump = angle_jump - sign(const_pi*2, real(dx, kind=kind(const_pi)))
            end if
            pu(i) = pu(i) + angle_jump
        end do
    end function unwrap_float
    function unwrap_double(p) result(pu)
        double precision, dimension(:) :: p
        double precision, allocatable, dimension(:) :: pu
        double precision :: angle_jump, angle, dx
        integer :: i, n
        n = size(p)
        allocate (pu(1:n), source=p)
        angle_jump = 0.0
        angle = const_pi
        do i = 2, n
            dx = pu(i) - (pu(i - 1) - angle_jump)
            if (abs(dx) > angle) then
                angle_jump = angle_jump - sign(const_pi*2, real(dx, kind=kind(const_pi)))
            end if
            pu(i) = pu(i) + angle_jump
        end do
    end function unwrap_double
end module libflit_transform
