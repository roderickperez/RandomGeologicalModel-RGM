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
module libflit_fourierfilt
    use libflit_constants
    use libflit_transform
    use libflit_array
    use libflit_error
    use libflit_taper
    use libflit_array_operation
    implicit none
    private
    ! 1D Fourier filter defined by freqs and amps
    interface fourier_filter
        module procedure :: fourier_filter_1d_float
        module procedure :: fourier_filter_1d_double
    end interface fourier_filter
    ! Dimension-separable Fourier filtering defined by freqs and amps
    interface fourier_filt
        module procedure :: fourier_filt_1d_float
        module procedure :: fourier_filt_2d_float
        module procedure :: fourier_filt_3d_float
        module procedure :: fourier_filt_1d_double
        module procedure :: fourier_filt_2d_double
        module procedure :: fourier_filt_3d_double
    end interface fourier_filt
    ! Fourier high-frequency emphasis filtering
    interface fourier_sharpen
        module procedure :: fourier_sharpen_1d_float
        module procedure :: fourier_sharpen_2d_float
        module procedure :: fourier_sharpen_3d_float
        module procedure :: fourier_sharpen_1d_double
        module procedure :: fourier_sharpen_2d_double
        module procedure :: fourier_sharpen_3d_double
    end interface fourier_sharpen
    ! Fourier low-frequency emphasis filtering
    interface fourier_smooth
        module procedure :: fourier_smooth_1d_float
        module procedure :: fourier_smooth_2d_float
        module procedure :: fourier_smooth_3d_float
        module procedure :: fourier_smooth_1d_double
        module procedure :: fourier_smooth_2d_double
        module procedure :: fourier_smooth_3d_double
    end interface fourier_smooth
    public :: fourier_filter
    public :: fourier_filt
    public :: fourier_sharpen
    public :: fourier_smooth
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
!> Define a frequency-domain filter using freqs and amps
!
function fourier_filter_1d_float(f, a, nt, dt, method, alpha) result(w)
    real, dimension(:), intent(in) :: f, a
    integer, intent(in) :: nt
    real, intent(in) :: dt
    character(len=*), optional, intent(in) :: method
    real, optional, intent(in) :: alpha
    real, allocatable, dimension(:) :: w
    integer :: n, i, nf, f1, f2
    real :: df, a1, a2
    integer, allocatable, dimension(:) :: findex
    character(len=24) :: window_method
    real :: window_alpha
    real, allocatable, dimension(:) :: window
    call assert(size(f) == size(a), ' <fourier_filter_1d_> Error: size(f) /= size(a). ')
    call assert(all(f >= 0), ' <fourier_filter_1d_> Error: f must >= 0. ')
    call assert(all(a >= 0), ' <fourier_filter_1d_> Error: a must > 0. ')
    n = size(f)
    nf = nint((nt + 1.0)/2.0)
    allocate (w(1:nt))
    df = 1.0/dt/nt
    findex = min(max(nint(f/df) + 1, 1), nf)
    if (present(method)) then
        window_method = method
    else
        window_method = 'hann'
    end if
    if (present(alpha)) then
        window_alpha = alpha
    else
        window_alpha = 4.0
    end if
    w(1:findex(1)) = a(1)
    do i = 1, n - 1
        f1 = findex(i)
        f2 = findex(i + 1)
        a1 = a(i)
        a2 = a(i + 1)
        if (a2 > a1) then
            call alloc_array(window, [f1, f2], &
                source=real(taper_window(f2 - f1 + 1, [f2 - f1 + 1, 0], &
                [character(len=24) :: window_method, ''], [window_alpha, 0.0])))
            w(f1:f2) = rescale(window, [a1, a2])
        else
            call alloc_array(window, [f1, f2], &
                source=real(taper_window(f2 - f1 + 1, [0, f2 - f1 + 1], &
                [character(len=24) :: '', window_method], [0.0, window_alpha])))
            w(f1:f2) = rescale(window, [a2, a1])
        end if
    end do
    w(f2 + 1:nf) = a(n)
    if (mod(nt, 2) == 0) then
        w(nf:nt) = w(nf:2:-1)
    else
        w(nf + 1:nt) = w(nf:2:-1)
    end if
end function fourier_filter_1d_float
!
!> Zero-phase, frequency-domain, dimension-separable filtering
!
function fourier_filt_1d_float(w, dt, freqs, amps, method, alpha) result(wt)
    real, dimension(:), intent(in) :: w
    real, intent(in) :: dt
    real, dimension(:), intent(in) :: freqs, amps
    character(len=*), optional :: method
    real, optional :: alpha
    real, allocatable, dimension(:) :: wt
    integer :: nt, nnt
    character(len=24) :: window_method
    real :: window_alpha
    if (present(method)) then
        window_method = method
    else
        window_method = 'hann'
    end if
    if (present(alpha)) then
        window_alpha = alpha
    else
        window_alpha = 4.0
    end if
    nt = size(w)
    nnt = next_power_235(nint(1.5*nt))
    wt = ifft(fft([w, real(zeros(nnt - nt))]) &
        *fourier_filter(freqs, amps, nnt, dt, window_method, window_alpha), &
        real=.true.)
    wt = wt(1:nt)
end function fourier_filt_1d_float
function fourier_filt_2d_float(w, d1, freqs1, amps1, d2, freqs2, amps2, &
        method, alpha) result(wt)
    real, dimension(:, :), intent(inout) :: w
    real, intent(in) :: d1, d2
    real, dimension(:), intent(in) :: freqs1, amps1, freqs2, amps2
    character(len=*), optional :: method
    real, optional :: alpha
    real, allocatable, dimension(:, :) :: wt
    integer :: i, j
    integer :: n1, n2
    character(len=24) :: window_method
    real :: window_alpha
    if (present(method)) then
        window_method = method
    else
        window_method = 'hann'
    end if
    if (present(alpha)) then
        window_alpha = alpha
    else
        window_alpha = 4.0
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate (wt(1:n1, 1:n2))
    !$omp parallel do private(i, j)
    do j = 1, n2
        wt(:, j) = fourier_filt_1d_float(w(:, j), d1, freqs1, amps1, &
            window_method, window_alpha)
    end do
    !$omp end parallel do
    !$omp parallel do private(i, j)
    do i = 1, n1
        wt(i, :) = fourier_filt_1d_float(wt(i, :), d2, freqs2, amps2, &
            window_method, window_alpha)
    end do
    !$omp end parallel do
end function fourier_filt_2d_float
function fourier_filt_3d_float(w, d1, freqs1, amps1, &
        d2, freqs2, amps2, d3, freqs3, amps3, method, alpha) result(wt)
    real, dimension(:, :, :), intent(inout) :: w
    real, intent(in) :: d1, d2, d3
    real, dimension(:), intent(in) :: freqs1, amps1, freqs2, amps2, freqs3, amps3
    character(len=*), optional :: method
    real, optional :: alpha
    real, allocatable, dimension(:, :, :) :: wt
    integer :: i, j, k
    integer :: n1, n2, n3
    character(len=24) :: window_method
    real :: window_alpha
    if (present(method)) then
        window_method = method
    else
        window_method = 'hann'
    end if
    if (present(alpha)) then
        window_alpha = alpha
    else
        window_alpha = 4.0
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate (wt(1:n1, 1:n2, 1:n3))
    !$omp parallel do private(i, j, k) collapse(2)
    do k = 1, n3
        do j = 1, n2
            wt(:, j, k) = fourier_filt_1d_float(w(:, j, k), d1, freqs1, amps1, &
                window_method, window_alpha)
        end do
    end do
    !$omp end parallel do
    !$omp parallel do private(i, j, k) collapse(2)
    do k = 1, n3
        do i = 1, n1
            wt(i, :, k) = fourier_filt_1d_float(wt(i, :, k), d2, freqs2, amps2, &
                window_method, window_alpha)
        end do
    end do
    !$omp end parallel do
    !$omp parallel do private(i, j, k) collapse(2)
    do j = 1, n2
        do i = 1, n1
            wt(i, j, :) = fourier_filt_1d_float(wt(i, j, :), d3, freqs3, amps3, &
                window_method, window_alpha)
        end do
    end do
    !$omp end parallel do
end function fourier_filt_3d_float
!
!> High-frequency emphasis filtering (I name it sharpen)
!
function fourier_sharpen_1d_float(f, k1, k2, sigma) result(g)
    real, dimension(:), intent(in) :: f
    real, intent(in) :: k1, k2, sigma
    real, allocatable, dimension(:) :: g
    real, allocatable, dimension(:) :: h
    integer :: i
    integer :: n1
    n1 = size(f, 1)
    h = zeros(n1)
    !$omp parallel do private(i)
    do i = 1, n1
        h(i) = 1.0 - exp(-(i - n1/2.0)**2/(2*sigma**2))
    end do
    !$omp end parallel do
    g = ifft(fftshift((k1 + k2*h)*fftshift(fft(f))), real=.true.)
end function fourier_sharpen_1d_float
function fourier_sharpen_2d_float(f, k1, k2, sigma) result(g)
    real, dimension(:, :), intent(in) :: f
    real, intent(in) :: k1, k2
    real, dimension(1:2), intent(in) :: sigma
    real, allocatable, dimension(:, :) :: g
    real, allocatable, dimension(:, :) :: h
    integer :: i, j
    integer :: n1, n2
    n1 = size(f, 1)
    n2 = size(f, 2)
    h = zeros(n1, n2)
    !$omp parallel do private(i, j)
    do j = 1, n2
        do i = 1, n1
            h(i, j) = 1.0 - exp(-(i - n1/2.0)**2/(2*sigma(1)**2) - (j - n2/2.0)**2/(2*sigma(2)**2))
        end do
    end do
    !$omp end parallel do
    g = ifft(fftshift((k1 + k2*h)*fftshift(fft(f))), real=.true.)
end function fourier_sharpen_2d_float
function fourier_sharpen_3d_float(f, k1, k2, sigma) result(g)
    real, dimension(:, :, :), intent(in) :: f
    real, intent(in) :: k1, k2
    real, dimension(1:3), intent(in) :: sigma
    real, allocatable, dimension(:, :, :) :: g
    real, allocatable, dimension(:, :, :) :: h
    integer :: i, j, k
    integer :: n1, n2, n3
    n1 = size(f, 1)
    n2 = size(f, 2)
    n3 = size(f, 3)
    h = zeros(n1, n2, n3)
    !$omp parallel do private(i, j, k)
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                h(i, j, k) = 1.0 - exp(-(i - n1/2.0)**2/(2*sigma(1)**2) - (j - n2/2.0)**2/(2*sigma(2)**2) &
                    - (k - n3/2.0)**2/(2*sigma(3)**2))
            end do
        end do
    end do
    !$omp end parallel do
    g = ifft(fftshift((k1 + k2*h)*fftshift(fft(f))), real=.true.)
end function fourier_sharpen_3d_float
!
!> Low-frequency emphasis filtering (I name it smooth)
!
function fourier_smooth_1d_float(f, sigma) result(g)
    real, dimension(:), intent(in) :: f
    real, intent(in) :: sigma
    real, allocatable, dimension(:) :: g
    real, allocatable, dimension(:) :: h
    integer :: i
    integer :: n1
    n1 = size(f)
    h = zeros(n1)
    !$omp parallel do private(i)
    do i = 1, n1
        h(i) = exp(-(i - n1/2.0)**2/(2*sigma**2))
    end do
    !$omp end parallel do
    g = ifft(fftshift(h*fftshift(fft(f))), real=.true.)
end function fourier_smooth_1d_float
function fourier_smooth_2d_float(f, sigma) result(g)
    real, dimension(:, :), intent(in) :: f
    real, dimension(1:2), intent(in) :: sigma
    real, allocatable, dimension(:, :) :: g
    real, allocatable, dimension(:, :) :: h
    integer :: i, j
    integer :: n1, n2
    n1 = size(f, 1)
    n2 = size(f, 2)
    h = zeros(n1, n2)
    !$omp parallel do private(i, j)
    do j = 1, n2
        do i = 1, n1
            h(i, j) = exp(-(i - n1/2.0)**2/(2*sigma(1)**2) - (j - n2/2.0)**2/(2*sigma(2)**2))
        end do
    end do
    !$omp end parallel do
    g = ifft(fftshift(h*fftshift(fft(f))), real=.true.)
end function fourier_smooth_2d_float
function fourier_smooth_3d_float(f, sigma) result(g)
    real, dimension(:, :, :), intent(in) :: f
    real, dimension(1:3), intent(in) :: sigma
    real, allocatable, dimension(:, :, :) :: g
    real, allocatable, dimension(:, :, :) :: h
    integer :: i, j, k
    integer :: n1, n2, n3
    n1 = size(f, 1)
    n2 = size(f, 2)
    n3 = size(f, 3)
    h = zeros(n1, n2, n3)
    !$omp parallel do private(i, j, k)
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                h(i, j, k) = exp(-(i - n1/2.0)**2/(2*sigma(1)**2) - (j - n2/2.0)**2/(2*sigma(2)**2) &
                    - (k - n3/2.0)**2/(2*sigma(3)**2))
            end do
        end do
    end do
    !$omp end parallel do
    g = ifft(fftshift(h*fftshift(fft(f))), real=.true.)
end function fourier_smooth_3d_float
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
!> Define a frequency-domain filter using freqs and amps
!
function fourier_filter_1d_double(f, a, nt, dt, method, alpha) result(w)
    double precision, dimension(:), intent(in) :: f, a
    integer, intent(in) :: nt
    double precision, intent(in) :: dt
    character(len=*), optional, intent(in) :: method
    double precision, optional, intent(in) :: alpha
    double precision, allocatable, dimension(:) :: w
    integer :: n, i, nf, f1, f2
    double precision :: df, a1, a2
    integer, allocatable, dimension(:) :: findex
    character(len=24) :: window_method
    real :: window_alpha
    double precision, allocatable, dimension(:) :: window
    call assert(size(f) == size(a), ' <fourier_filter_1d_> Error: size(f) /= size(a). ')
    call assert(all(f >= 0), ' <fourier_filter_1d_> Error: f must >= 0. ')
    call assert(all(a >= 0), ' <fourier_filter_1d_> Error: a must > 0. ')
    n = size(f)
    nf = nint((nt + 1.0)/2.0)
    allocate (w(1:nt))
    df = 1.0/dt/nt
    findex = min(max(nint(f/df) + 1, 1), nf)
    if (present(method)) then
        window_method = method
    else
        window_method = 'hann'
    end if
    if (present(alpha)) then
        window_alpha = alpha
    else
        window_alpha = 4.0
    end if
    w(1:findex(1)) = a(1)
    do i = 1, n - 1
        f1 = findex(i)
        f2 = findex(i + 1)
        a1 = a(i)
        a2 = a(i + 1)
        if (a2 > a1) then
            call alloc_array(window, [f1, f2], &
                source=dble(taper_window(f2 - f1 + 1, [f2 - f1 + 1, 0], &
                [character(len=24) :: window_method, ''], [window_alpha, 0.0])))
            w(f1:f2) = rescale(window, [a1, a2])
        else
            call alloc_array(window, [f1, f2], &
                source=dble(taper_window(f2 - f1 + 1, [0, f2 - f1 + 1], &
                [character(len=24) :: '', window_method], [0.0, window_alpha])))
            w(f1:f2) = rescale(window, [a2, a1])
        end if
    end do
    w(f2 + 1:nf) = a(n)
    if (mod(nt, 2) == 0) then
        w(nf:nt) = w(nf:2:-1)
    else
        w(nf + 1:nt) = w(nf:2:-1)
    end if
end function fourier_filter_1d_double
!
!> Zero-phase, frequency-domain, dimension-separable filtering
!
function fourier_filt_1d_double(w, dt, freqs, amps, method, alpha) result(wt)
    double precision, dimension(:), intent(in) :: w
    double precision, intent(in) :: dt
    double precision, dimension(:), intent(in) :: freqs, amps
    character(len=*), optional :: method
    double precision, optional :: alpha
    double precision, allocatable, dimension(:) :: wt
    integer :: nt, nnt
    character(len=24) :: window_method
    double precision :: window_alpha
    if (present(method)) then
        window_method = method
    else
        window_method = 'hann'
    end if
    if (present(alpha)) then
        window_alpha = alpha
    else
        window_alpha = 4.0
    end if
    nt = size(w)
    nnt = next_power_235(nint(1.5*nt))
    wt = ifft(fft([w, dble(zeros(nnt - nt))]) &
        *fourier_filter(freqs, amps, nnt, dt, window_method, window_alpha), &
        real=.true.)
    wt = wt(1:nt)
end function fourier_filt_1d_double
function fourier_filt_2d_double(w, d1, freqs1, amps1, d2, freqs2, amps2, &
        method, alpha) result(wt)
    double precision, dimension(:, :), intent(inout) :: w
    double precision, intent(in) :: d1, d2
    double precision, dimension(:), intent(in) :: freqs1, amps1, freqs2, amps2
    character(len=*), optional :: method
    double precision, optional :: alpha
    double precision, allocatable, dimension(:, :) :: wt
    integer :: i, j
    integer :: n1, n2
    character(len=24) :: window_method
    double precision :: window_alpha
    if (present(method)) then
        window_method = method
    else
        window_method = 'hann'
    end if
    if (present(alpha)) then
        window_alpha = alpha
    else
        window_alpha = 4.0
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate (wt(1:n1, 1:n2))
    !$omp parallel do private(i, j)
    do j = 1, n2
        wt(:, j) = fourier_filt_1d_double(w(:, j), d1, freqs1, amps1, &
            window_method, window_alpha)
    end do
    !$omp end parallel do
    !$omp parallel do private(i, j)
    do i = 1, n1
        wt(i, :) = fourier_filt_1d_double(wt(i, :), d2, freqs2, amps2, &
            window_method, window_alpha)
    end do
    !$omp end parallel do
end function fourier_filt_2d_double
function fourier_filt_3d_double(w, d1, freqs1, amps1, &
        d2, freqs2, amps2, d3, freqs3, amps3, method, alpha) result(wt)
    double precision, dimension(:, :, :), intent(inout) :: w
    double precision, intent(in) :: d1, d2, d3
    double precision, dimension(:), intent(in) :: freqs1, amps1, freqs2, amps2, freqs3, amps3
    character(len=*), optional :: method
    double precision, optional :: alpha
    double precision, allocatable, dimension(:, :, :) :: wt
    integer :: i, j, k
    integer :: n1, n2, n3
    character(len=24) :: window_method
    double precision :: window_alpha
    if (present(method)) then
        window_method = method
    else
        window_method = 'hann'
    end if
    if (present(alpha)) then
        window_alpha = alpha
    else
        window_alpha = 4.0
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate (wt(1:n1, 1:n2, 1:n3))
    !$omp parallel do private(i, j, k) collapse(2)
    do k = 1, n3
        do j = 1, n2
            wt(:, j, k) = fourier_filt_1d_double(w(:, j, k), d1, freqs1, amps1, &
                window_method, window_alpha)
        end do
    end do
    !$omp end parallel do
    !$omp parallel do private(i, j, k) collapse(2)
    do k = 1, n3
        do i = 1, n1
            wt(i, :, k) = fourier_filt_1d_double(wt(i, :, k), d2, freqs2, amps2, &
                window_method, window_alpha)
        end do
    end do
    !$omp end parallel do
    !$omp parallel do private(i, j, k) collapse(2)
    do j = 1, n2
        do i = 1, n1
            wt(i, j, :) = fourier_filt_1d_double(wt(i, j, :), d3, freqs3, amps3, &
                window_method, window_alpha)
        end do
    end do
    !$omp end parallel do
end function fourier_filt_3d_double
!
!> High-frequency emphasis filtering (I name it sharpen)
!
function fourier_sharpen_1d_double(f, k1, k2, sigma) result(g)
    double precision, dimension(:), intent(in) :: f
    double precision, intent(in) :: k1, k2, sigma
    double precision, allocatable, dimension(:) :: g
    double precision, allocatable, dimension(:) :: h
    integer :: i
    integer :: n1
    n1 = size(f, 1)
    h = zeros(n1)
    !$omp parallel do private(i)
    do i = 1, n1
        h(i) = 1.0 - exp(-(i - n1/2.0)**2/(2*sigma**2))
    end do
    !$omp end parallel do
    g = ifft(fftshift((k1 + k2*h)*fftshift(fft(f))), real=.true.)
end function fourier_sharpen_1d_double
function fourier_sharpen_2d_double(f, k1, k2, sigma) result(g)
    double precision, dimension(:, :), intent(in) :: f
    double precision, intent(in) :: k1, k2
    double precision, dimension(1:2), intent(in) :: sigma
    double precision, allocatable, dimension(:, :) :: g
    double precision, allocatable, dimension(:, :) :: h
    integer :: i, j
    integer :: n1, n2
    n1 = size(f, 1)
    n2 = size(f, 2)
    h = zeros(n1, n2)
    !$omp parallel do private(i, j)
    do j = 1, n2
        do i = 1, n1
            h(i, j) = 1.0 - exp(-(i - n1/2.0)**2/(2*sigma(1)**2) - (j - n2/2.0)**2/(2*sigma(2)**2))
        end do
    end do
    !$omp end parallel do
    g = ifft(fftshift((k1 + k2*h)*fftshift(fft(f))), real=.true.)
end function fourier_sharpen_2d_double
function fourier_sharpen_3d_double(f, k1, k2, sigma) result(g)
    double precision, dimension(:, :, :), intent(in) :: f
    double precision, intent(in) :: k1, k2
    double precision, dimension(1:3), intent(in) :: sigma
    double precision, allocatable, dimension(:, :, :) :: g
    double precision, allocatable, dimension(:, :, :) :: h
    integer :: i, j, k
    integer :: n1, n2, n3
    n1 = size(f, 1)
    n2 = size(f, 2)
    n3 = size(f, 3)
    h = zeros(n1, n2, n3)
    !$omp parallel do private(i, j, k)
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                h(i, j, k) = 1.0 - exp(-(i - n1/2.0)**2/(2*sigma(1)**2) - (j - n2/2.0)**2/(2*sigma(2)**2) &
                    - (k - n3/2.0)**2/(2*sigma(3)**2))
            end do
        end do
    end do
    !$omp end parallel do
    g = ifft(fftshift((k1 + k2*h)*fftshift(fft(f))), real=.true.)
end function fourier_sharpen_3d_double
!
!> Low-frequency emphasis filtering (I name it smooth)
!
function fourier_smooth_1d_double(f, sigma) result(g)
    double precision, dimension(:), intent(in) :: f
    double precision, intent(in) :: sigma
    double precision, allocatable, dimension(:) :: g
    double precision, allocatable, dimension(:) :: h
    integer :: i
    integer :: n1
    n1 = size(f)
    h = zeros(n1)
    !$omp parallel do private(i)
    do i = 1, n1
        h(i) = exp(-(i - n1/2.0)**2/(2*sigma**2))
    end do
    !$omp end parallel do
    g = ifft(fftshift(h*fftshift(fft(f))), real=.true.)
end function fourier_smooth_1d_double
function fourier_smooth_2d_double(f, sigma) result(g)
    double precision, dimension(:, :), intent(in) :: f
    double precision, dimension(1:2), intent(in) :: sigma
    double precision, allocatable, dimension(:, :) :: g
    double precision, allocatable, dimension(:, :) :: h
    integer :: i, j
    integer :: n1, n2
    n1 = size(f, 1)
    n2 = size(f, 2)
    h = zeros(n1, n2)
    !$omp parallel do private(i, j)
    do j = 1, n2
        do i = 1, n1
            h(i, j) = exp(-(i - n1/2.0)**2/(2*sigma(1)**2) - (j - n2/2.0)**2/(2*sigma(2)**2))
        end do
    end do
    !$omp end parallel do
    g = ifft(fftshift(h*fftshift(fft(f))), real=.true.)
end function fourier_smooth_2d_double
function fourier_smooth_3d_double(f, sigma) result(g)
    double precision, dimension(:, :, :), intent(in) :: f
    double precision, dimension(1:3), intent(in) :: sigma
    double precision, allocatable, dimension(:, :, :) :: g
    double precision, allocatable, dimension(:, :, :) :: h
    integer :: i, j, k
    integer :: n1, n2, n3
    n1 = size(f, 1)
    n2 = size(f, 2)
    n3 = size(f, 3)
    h = zeros(n1, n2, n3)
    !$omp parallel do private(i, j, k)
    do k = 1, n3
        do j = 1, n2
            do i = 1, n1
                h(i, j, k) = exp(-(i - n1/2.0)**2/(2*sigma(1)**2) - (j - n2/2.0)**2/(2*sigma(2)**2) &
                    - (k - n3/2.0)**2/(2*sigma(3)**2))
            end do
        end do
    end do
    !$omp end parallel do
    g = ifft(fftshift(h*fftshift(fft(f))), real=.true.)
end function fourier_smooth_3d_double
end module libflit_fourierfilt
