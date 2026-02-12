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
module libflit_taper
    use libflit_constants
    use libflit_array
    use libflit_specialfunc
    use libflit_utility
    use libflit_error
    implicit none
    private
    interface window_function
        module procedure :: window_function_float
        module procedure :: window_function_double
    end interface window_function
    interface taper
        module procedure :: taper_1d_float
        module procedure :: taper_2d_float
        module procedure :: taper_3d_float
        module procedure :: taper_1d_double
        module procedure :: taper_2d_double
        module procedure :: taper_3d_double
        module procedure :: taper_1d_complex
        module procedure :: taper_2d_complex
        module procedure :: taper_3d_complex
        module procedure :: taper_1d_dcomplex
        module procedure :: taper_2d_dcomplex
        module procedure :: taper_3d_dcomplex
    end interface taper
    public :: taper_window
    public :: taper
    public :: window_function
contains
    !
    !> Various types of taper window functions
    !
    function left_taper(len, method, alpha) result(taper)
        integer :: len
        character(len=*), optional :: method
        real, optional :: alpha
        real, dimension(:), allocatable :: taper
        integer :: n, i
        double precision :: a0, a1, a2, a3, a
        character(len=24) :: taper_method
        if (present(alpha)) then
            a = alpha
        else
            a = 0.0
        end if
        if (present(method)) then
            taper_method = method
        else
            taper_method = 'hann'
        end if
        taper = ones(len)
        n = 2*len - 1
        do i = 0, len - 1
            select case (taper_method)
                case ('step')
                    taper(i + 1) = 0.0
                case ('linear')
                    taper(i + 1) = i*1.0/(len - 1.0)
                case ('parzen')
                    if (i <= n/4.0) then
                        taper(i + 1) = 6.0*(i/(n/2.0))**2*(1.0 - i/(n/2.0))
                    else
                        taper(i + 1) = 1.0 - 2.0*(1.0 - i/(n/2.0))**3
                    end if
                case ('welch')
                    taper(i + 1) = 1.0 - ((i - (n - 1.0)/2.0)/((n - 1.0)/2.0))**2
                case ('sine')
                    taper(i + 1) = sin(const_pi*i/(n - 1.0))
                case ('power-sine')
                    taper(i + 1) = sin(const_pi*i/(n - 1.0))**a
                case ('hann')
                    taper(i + 1) = sin(const_pi*i/(n - 1.0))**2
                case ('hamming')
                    a0 = 25.0d0/46.0d0
                    taper(i + 1) = a0 - (1.0 - a0)*cos(2.0*const_pi*i/(n - 1.0))
                case ('blackman')
                    a0 = 0.42d0
                    a1 = 0.5d0
                    a2 = 0.08d0
                    taper(i + 1) = a0 - a1*cos(2*const_pi*i/(n - 1.0)) + a2*cos(4*const_pi*i/(n - 1.0))
                case ('nuttall')
                    a0 = 0.355768d0
                    a1 = 0.487396d0
                    a2 = 0.144232d0
                    a3 = 0.012604d0
                    taper(i + 1) = a0 - a1*cos(2*const_pi*i/(n - 1.0)) + a2*cos(4*const_pi*i/(n - 1.0)) &
                        - a3*cos(6*const_pi*i/(n - 1.0))
                case ('blackman-nuttall')
                    a0 = 0.3635819d0
                    a1 = 0.4891775d0
                    a2 = 0.1365995d0
                    a3 = 0.0106411d0
                    taper(i + 1) = a0 - a1*cos(2*const_pi*i/(n - 1.0)) + a2*cos(4*const_pi*i/(n - 1.0)) &
                        - a3*cos(6*const_pi*i/(n - 1.0))
                case ('blackman-harris')
                    a0 = 0.35875d0
                    a1 = 0.48829d0
                    a2 = 0.14128d0
                    a3 = 0.01168d0
                    taper(i + 1) = a0 - a1*cos(2*const_pi*i/(n - 1.0)) + a2*cos(4*const_pi*i/(n - 1.0)) &
                        - a3*cos(6*const_pi*i/(n - 1.0))
                case ('kaiser')
                    taper(i + 1) = bessel_i0(dble(const_pi*a*sqrt(1.0 - (2.0*i/(n - 1.0) - 1.0)**2)))/bessel_i0(dble(a*const_pi))
                case ('gauss')
                    taper(i + 1) = exp(-0.5*((i - (n - 1.0)/2.0)/(a*(n - 1.0)/2.0))**2)
            end select
        end do
        select case(method)
            case('step', 'linear', 'welch', 'sine', 'power-sine', 'hann', 'blackman', 'nuttall')
                taper(1) = 0.0
        end select
    end function left_taper
    ! Window function
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
function window_function_float(x0, method, alpha) result(w)
    !> Distance to zero; normalized to [0, 1]
    real, dimension(:) :: x0
    !> Window type
    character(len=*) :: method
    !> Coefficient that has different meanings for different windows
    real, optional :: alpha
    !> Value of window function
    real, allocatable, dimension(:) :: w
    real, allocatable, dimension(:) :: x
    real :: a0, a1, a2, a3
    x = x0
    w = ones_like(x)
    select case (method)
        case ('step')
            w = 0
        case ('linear')
            where (x <= 0.5d0)
                w = 2*x
            end where
            where (x >= 0.5d0)
                w = 1 - 2*(x - 0.5d0)
            end where
        case ('parzen')
            x = x - 0.5d0
            x = x/0.5d0
            where (abs(x) <= 1.0d0/2.0d0)
                w = 1.0d0 - 6.0d0*x**2*(1.0d0 - abs(x))
            end where
            where (abs(x) > 1.0d0/2.0d0)
                w = 2*(1 - abs(x))**3
            end where
        case ('welch')
            x = x - 0.5d0
            x = x/0.5d0
            w = 1 - x**2
        case ('sine')
            w = sin(const_pi*x)
        case ('power-sine')
            w = sin(const_pi*x)**alpha
        case ('hann')
            w = sin(const_pi*x)**2
        case ('hamming')
            a0 = 25.0d0/46.0d0
            w = a0 - (1 - a0)*cos(2*const_pi*x)
        case ('blackman')
            a0 = 0.42d0
            a1 = 0.5d0
            a2 = 0.08d0
            w = a0 - a1*cos(2*const_pi*x) + a2*cos(4*const_pi*x)
        case ('nuttall')
            a0 = 0.355768d0
            a1 = 0.487396d0
            a2 = 0.144232d0
            a3 = 0.012604d0
            w = a0 - a1*cos(2*const_pi*x) + a2*cos(4*const_pi*x) - a3*cos(6*const_pi*x)
        case ('blackman-nuttall')
            a0 = 0.3635819d0
            a1 = 0.4891775d0
            a2 = 0.1365995d0
            a3 = 0.0106411d0
            w = a0 - a1*cos(2*const_pi*x) + a2*cos(4*const_pi*x) - a3*cos(6*const_pi*x)
        case ('blackman-harris')
            a0 = 0.35875d0
            a1 = 0.48829d0
            a2 = 0.14128d0
            a3 = 0.01168d0
            w = a0 - a1*cos(2*const_pi*x) + a2*cos(4*const_pi*x) - a3*cos(6*const_pi*x)
        case ('kaiser')
            x = x - 0.5d0
            x = x/0.5d0
            w = bessel_i0(const_pi*alpha*sqrt(1.0d0 - x**2))/bessel_i0(const_pi*alpha)
        case ('gauss')
            x = x - 0.5d0
            x = x/0.5d0
            w = exp(-0.5d0*(x/alpha)**2)
    end select
    where (w < 0)
        w = 0
    end where
    where (w > 1)
        w = 1
    end where
end function window_function_float
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
function window_function_double(x0, method, alpha) result(w)
    !> Distance to zero; normalized to [0, 1]
    double precision, dimension(:) :: x0
    !> Window type
    character(len=*) :: method
    !> Coefficient that has different meanings for different windows
    double precision, optional :: alpha
    !> Value of window function
    double precision, allocatable, dimension(:) :: w
    double precision, allocatable, dimension(:) :: x
    double precision :: a0, a1, a2, a3
    x = x0
    w = ones_like(x)
    select case (method)
        case ('step')
            w = 0
        case ('linear')
            where (x <= 0.5d0)
                w = 2*x
            end where
            where (x >= 0.5d0)
                w = 1 - 2*(x - 0.5d0)
            end where
        case ('parzen')
            x = x - 0.5d0
            x = x/0.5d0
            where (abs(x) <= 1.0d0/2.0d0)
                w = 1.0d0 - 6.0d0*x**2*(1.0d0 - abs(x))
            end where
            where (abs(x) > 1.0d0/2.0d0)
                w = 2*(1 - abs(x))**3
            end where
        case ('welch')
            x = x - 0.5d0
            x = x/0.5d0
            w = 1 - x**2
        case ('sine')
            w = sin(const_pi*x)
        case ('power-sine')
            w = sin(const_pi*x)**alpha
        case ('hann')
            w = sin(const_pi*x)**2
        case ('hamming')
            a0 = 25.0d0/46.0d0
            w = a0 - (1 - a0)*cos(2*const_pi*x)
        case ('blackman')
            a0 = 0.42d0
            a1 = 0.5d0
            a2 = 0.08d0
            w = a0 - a1*cos(2*const_pi*x) + a2*cos(4*const_pi*x)
        case ('nuttall')
            a0 = 0.355768d0
            a1 = 0.487396d0
            a2 = 0.144232d0
            a3 = 0.012604d0
            w = a0 - a1*cos(2*const_pi*x) + a2*cos(4*const_pi*x) - a3*cos(6*const_pi*x)
        case ('blackman-nuttall')
            a0 = 0.3635819d0
            a1 = 0.4891775d0
            a2 = 0.1365995d0
            a3 = 0.0106411d0
            w = a0 - a1*cos(2*const_pi*x) + a2*cos(4*const_pi*x) - a3*cos(6*const_pi*x)
        case ('blackman-harris')
            a0 = 0.35875d0
            a1 = 0.48829d0
            a2 = 0.14128d0
            a3 = 0.01168d0
            w = a0 - a1*cos(2*const_pi*x) + a2*cos(4*const_pi*x) - a3*cos(6*const_pi*x)
        case ('kaiser')
            x = x - 0.5d0
            x = x/0.5d0
            w = bessel_i0(const_pi*alpha*sqrt(1.0d0 - x**2))/bessel_i0(const_pi*alpha)
        case ('gauss')
            x = x - 0.5d0
            x = x/0.5d0
            w = exp(-0.5d0*(x/alpha)**2)
    end select
    where (w < 0)
        w = 0
    end where
    where (w > 1)
        w = 1
    end where
end function window_function_double
    !
    !> Various types of taper window functions
    !
    function taper_window(ns, len, method, alpha) result(taper)
        integer :: ns
        integer, dimension(:), optional :: len
        character(len=*), dimension(:), optional :: method
        real, dimension(:), optional :: alpha
        real, dimension(:), allocatable :: taper
        integer :: n1, n2
        real, dimension(1:2) :: taper_alpha
        character(len=24), dimension(1:2) :: taper_method
        if (present(len)) then
            n1 = len(1)
            n2 = len(2)
        else
            n1 = min(ns, max(5, nint(0.05*ns)))
            n2 = min(ns, max(5, nint(0.05*ns)))
        end if
        call assert(all(len >= 0), ' <taper_window> Error: Taper length must >= 0.')
        call assert(ns >= n1 + n2 - 1, ' <taper_window> Error: Number of samples is too small for creating the taper.')
        if (present(alpha)) then
            taper_alpha = alpha
        else
            taper_alpha = 0.0
        end if
        if (present(method)) then
            taper_method = method
        else
            taper_method = ['hann', 'hann']
        end if
        taper = ones(ns)
        ! The following two approaches are essentially the same
        if (n1 >= 1) then
            taper(1:n1) = window_function(linspace(0.0, 0.5, n1), taper_method(1), taper_alpha(1))
        end if
        if (n2 >= 1) then
            taper(ns - n2 + 1:ns) = window_function(linspace(0.5, 1.0, n2), taper_method(2), taper_alpha(2))
        end if
        ! if (n1 >= 1) then
        ! taper(1:n1) = left_taper(n1, taper_method(1), taper_alpha(1))
        ! end if
        ! if (n2 >= 1) then
        ! taper(ns:ns - n2 + 1:-1) = left_taper(n2, taper_method(2), taper_alpha(2))
        ! end if
    end function taper_window
    ! Tapering array
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
!> Taper 1D array
!
function taper_1d_float(w, len, method, alpha, protect) result(wt)
    real, dimension(:), intent(in) :: w
    integer, dimension(:), intent(in), optional :: protect, len
    character(len=*), intent(in), dimension(:), optional :: method
    real, dimension(:), intent(in), optional :: alpha
    real, allocatable, dimension(:) :: wt
    real, allocatable, dimension(:) :: taper
    integer, dimension(1:2) :: taper_protect, taper_len
    integer :: n, np, i, taper_beg, taper_end
    character(len=24), dimension(1:2) :: taper_method
    real, dimension(1:2) :: taper_alpha
    ! Size of the array
    n = size(w)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len = [max(5, nint(0.05*n)), max(5, nint(0.05*n))]
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect = [taper_len(1), n - taper_len(2) + 1]
    end if
    taper_protect = clip(taper_protect, 1, n)
    call assert(taper_protect(2) >= taper_protect(1), ' <taper_1d> Error: Protect range specification error.')
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = alpha
    else
        taper_alpha = [0.0, 0.0]
    end if
    ! Setup the taper
    np = taper_len(1) + taper_protect(2) - taper_protect(1) + 1 + taper_len(2) - count(taper_len /= 0)
    taper = taper_window(np, taper_len, taper_method, taper_alpha)
    ! Tapering the array by multiplication
    wt = w
    if (taper_len(1) == 0) then
        taper_beg = taper_protect(1)
    else
        taper_beg = taper_protect(1) - taper_len(1) + 1
    end if
    if (taper_len(2) == 0) then
        taper_end = taper_protect(2)
    else
        taper_end = taper_protect(2) + taper_len(2) - 1
    end if
    wt(1:taper_beg - 1) = 0.0
    do i = max(1, taper_beg), min(n, taper_end)
        wt(i) = wt(i)*taper(i - taper_beg + 1)
    end do
    wt(taper_end + 1:n) = 0.0
end function taper_1d_float
!
!> Taper 2D array
!
function taper_2d_float(w, len, method, alpha, protect) result(wt)
    real, dimension(:, :) :: w
    integer, dimension(:), optional :: protect, len
    character(len=*), dimension(:), optional :: method
    real, dimension(:), optional :: alpha
    real, allocatable, dimension(:, :) :: wt
    integer :: n1, n2, i, j
    integer, dimension(1:4) :: taper_protect, taper_len
    character(len=24), dimension(1:4) :: taper_method
    real, dimension(1:4) :: taper_alpha
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len(1:2) = max(5, nint(0.05*n1))
        taper_len(3:4) = max(5, nint(0.05*n2))
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect(1) = taper_len(1)
        taper_protect(2) = n1 - taper_len(2) + 1
        taper_protect(3) = taper_len(3)
        taper_protect(4) = n2 - taper_len(4) + 1
    end if
    taper_protect(1:2) = clip(taper_protect(1:2), 1, n1)
    taper_protect(3:4) = clip(taper_protect(3:4), 1, n2)
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann', 'hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = real(alpha)
    else
        taper_alpha = [0.0, 0.0, 0.0, 0.0]
    end if
    ! Tapering the array by multiplication
    wt = w
    ! dim = 1 tapering
    !$omp parallel do private(j)
    do j = 1, n2
        wt(:, j) = taper_1d_float(w(:, j), &
            taper_len(1:2), taper_method(1:2), taper_alpha(1:2), taper_protect(1:2))
    end do
    !$omp end parallel do
    ! dim = 2 tapering
    !$omp parallel do private(i)
    do i = 1, n1
        wt(i, :) = taper_1d_float(wt(i, :), &
            taper_len(3:4), taper_method(3:4), taper_alpha(3:4), taper_protect(3:4))
    end do
    !$omp end parallel do
end function taper_2d_float
!
!> Taper 3D array
!
function taper_3d_float(w, len, method, alpha, protect) result(wt)
    real, dimension(:, :, :) :: w
    integer, dimension(:), optional :: protect, len
    character(len=*), dimension(:), optional :: method
    real, dimension(:), optional :: alpha
    real, allocatable, dimension(:, :, :) :: wt
    integer :: n1, n2, n3, i, j, k
    integer, dimension(1:6) :: taper_protect, taper_len
    character(len=24), dimension(1:6) :: taper_method
    real, dimension(1:6) :: taper_alpha
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len(1:2) = max(5, nint(0.05*n1))
        taper_len(3:4) = max(5, nint(0.05*n2))
        taper_len(5:6) = max(5, nint(0.05*n3))
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect(1) = taper_len(1)
        taper_protect(2) = n1 - taper_len(2) + 1
        taper_protect(3) = taper_len(3)
        taper_protect(4) = n2 - taper_len(4) + 1
        taper_protect(5) = taper_len(5)
        taper_protect(6) = n3 - taper_len(6) + 1
    end if
    taper_protect(1:2) = clip(taper_protect(1:2), 1, n1)
    taper_protect(3:4) = clip(taper_protect(3:4), 1, n2)
    taper_protect(5:6) = clip(taper_protect(5:6), 1, n3)
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann', 'hann', 'hann', 'hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = real(alpha)
    else
        taper_alpha = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    end if
    ! Tapering the array by multiplication
    wt = w
    ! dim = 3 tapering
    !$omp parallel do private(i, j) collapse(2)
    do i = 1, n1
        do j = 1, n2
            wt(i, j, :) = taper_1d_float(w(i, j, :), &
                taper_len(5:6), taper_method(5:6), taper_alpha(5:6), taper_protect(5:6))
        end do
    end do
    !$omp end parallel do
    ! dim = 2 tapering
    !$omp parallel do private(i, k) collapse(2)
    do i = 1, n1
        do k = 1, n3
            wt(i, :, k) = taper_1d_float(wt(i, :, k), &
                taper_len(3:4), taper_method(3:4), taper_alpha(3:4), taper_protect(3:4))
        end do
    end do
    !$omp end parallel do
    ! dim = 1 tapering
    !$omp parallel do private(j, k) collapse(2)
    do j = 1, n2
        do k = 1, n3
            wt(:, j, k) = taper_1d_float(wt(:, j, k), &
                taper_len(1:2), taper_method(1:2), taper_alpha(1:2), taper_protect(1:2))
        end do
    end do
    !$omp end parallel do
end function taper_3d_float
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
!> Taper 1D array
!
function taper_1d_double(w, len, method, alpha, protect) result(wt)
    double precision, dimension(:), intent(in) :: w
    integer, dimension(:), intent(in), optional :: protect, len
    character(len=*), intent(in), dimension(:), optional :: method
    real, dimension(:), intent(in), optional :: alpha
    double precision, allocatable, dimension(:) :: wt
    real, allocatable, dimension(:) :: taper
    integer, dimension(1:2) :: taper_protect, taper_len
    integer :: n, np, i, taper_beg, taper_end
    character(len=24), dimension(1:2) :: taper_method
    real, dimension(1:2) :: taper_alpha
    ! Size of the array
    n = size(w)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len = [max(5, nint(0.05*n)), max(5, nint(0.05*n))]
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect = [taper_len(1), n - taper_len(2) + 1]
    end if
    taper_protect = clip(taper_protect, 1, n)
    call assert(taper_protect(2) >= taper_protect(1), ' <taper_1d> Error: Protect range specification error.')
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = alpha
    else
        taper_alpha = [0.0, 0.0]
    end if
    ! Setup the taper
    np = taper_len(1) + taper_protect(2) - taper_protect(1) + 1 + taper_len(2) - count(taper_len /= 0)
    taper = taper_window(np, taper_len, taper_method, taper_alpha)
    ! Tapering the array by multiplication
    wt = w
    if (taper_len(1) == 0) then
        taper_beg = taper_protect(1)
    else
        taper_beg = taper_protect(1) - taper_len(1) + 1
    end if
    if (taper_len(2) == 0) then
        taper_end = taper_protect(2)
    else
        taper_end = taper_protect(2) + taper_len(2) - 1
    end if
    wt(1:taper_beg - 1) = 0.0
    do i = max(1, taper_beg), min(n, taper_end)
        wt(i) = wt(i)*taper(i - taper_beg + 1)
    end do
    wt(taper_end + 1:n) = 0.0
end function taper_1d_double
!
!> Taper 2D array
!
function taper_2d_double(w, len, method, alpha, protect) result(wt)
    double precision, dimension(:, :) :: w
    integer, dimension(:), optional :: protect, len
    character(len=*), dimension(:), optional :: method
    real, dimension(:), optional :: alpha
    double precision, allocatable, dimension(:, :) :: wt
    integer :: n1, n2, i, j
    integer, dimension(1:4) :: taper_protect, taper_len
    character(len=24), dimension(1:4) :: taper_method
    real, dimension(1:4) :: taper_alpha
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len(1:2) = max(5, nint(0.05*n1))
        taper_len(3:4) = max(5, nint(0.05*n2))
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect(1) = taper_len(1)
        taper_protect(2) = n1 - taper_len(2) + 1
        taper_protect(3) = taper_len(3)
        taper_protect(4) = n2 - taper_len(4) + 1
    end if
    taper_protect(1:2) = clip(taper_protect(1:2), 1, n1)
    taper_protect(3:4) = clip(taper_protect(3:4), 1, n2)
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann', 'hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = real(alpha)
    else
        taper_alpha = [0.0, 0.0, 0.0, 0.0]
    end if
    ! Tapering the array by multiplication
    wt = w
    ! dim = 1 tapering
    !$omp parallel do private(j)
    do j = 1, n2
        wt(:, j) = taper_1d_double(w(:, j), &
            taper_len(1:2), taper_method(1:2), taper_alpha(1:2), taper_protect(1:2))
    end do
    !$omp end parallel do
    ! dim = 2 tapering
    !$omp parallel do private(i)
    do i = 1, n1
        wt(i, :) = taper_1d_double(wt(i, :), &
            taper_len(3:4), taper_method(3:4), taper_alpha(3:4), taper_protect(3:4))
    end do
    !$omp end parallel do
end function taper_2d_double
!
!> Taper 3D array
!
function taper_3d_double(w, len, method, alpha, protect) result(wt)
    double precision, dimension(:, :, :) :: w
    integer, dimension(:), optional :: protect, len
    character(len=*), dimension(:), optional :: method
    real, dimension(:), optional :: alpha
    double precision, allocatable, dimension(:, :, :) :: wt
    integer :: n1, n2, n3, i, j, k
    integer, dimension(1:6) :: taper_protect, taper_len
    character(len=24), dimension(1:6) :: taper_method
    real, dimension(1:6) :: taper_alpha
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len(1:2) = max(5, nint(0.05*n1))
        taper_len(3:4) = max(5, nint(0.05*n2))
        taper_len(5:6) = max(5, nint(0.05*n3))
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect(1) = taper_len(1)
        taper_protect(2) = n1 - taper_len(2) + 1
        taper_protect(3) = taper_len(3)
        taper_protect(4) = n2 - taper_len(4) + 1
        taper_protect(5) = taper_len(5)
        taper_protect(6) = n3 - taper_len(6) + 1
    end if
    taper_protect(1:2) = clip(taper_protect(1:2), 1, n1)
    taper_protect(3:4) = clip(taper_protect(3:4), 1, n2)
    taper_protect(5:6) = clip(taper_protect(5:6), 1, n3)
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann', 'hann', 'hann', 'hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = real(alpha)
    else
        taper_alpha = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    end if
    ! Tapering the array by multiplication
    wt = w
    ! dim = 3 tapering
    !$omp parallel do private(i, j) collapse(2)
    do i = 1, n1
        do j = 1, n2
            wt(i, j, :) = taper_1d_double(w(i, j, :), &
                taper_len(5:6), taper_method(5:6), taper_alpha(5:6), taper_protect(5:6))
        end do
    end do
    !$omp end parallel do
    ! dim = 2 tapering
    !$omp parallel do private(i, k) collapse(2)
    do i = 1, n1
        do k = 1, n3
            wt(i, :, k) = taper_1d_double(wt(i, :, k), &
                taper_len(3:4), taper_method(3:4), taper_alpha(3:4), taper_protect(3:4))
        end do
    end do
    !$omp end parallel do
    ! dim = 1 tapering
    !$omp parallel do private(j, k) collapse(2)
    do j = 1, n2
        do k = 1, n3
            wt(:, j, k) = taper_1d_double(wt(:, j, k), &
                taper_len(1:2), taper_method(1:2), taper_alpha(1:2), taper_protect(1:2))
        end do
    end do
    !$omp end parallel do
end function taper_3d_double
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
!> Taper 1D array
!
function taper_1d_complex(w, len, method, alpha, protect) result(wt)
    complex, dimension(:), intent(in) :: w
    integer, dimension(:), intent(in), optional :: protect, len
    character(len=*), intent(in), dimension(:), optional :: method
    real, dimension(:), intent(in), optional :: alpha
    complex, allocatable, dimension(:) :: wt
    real, allocatable, dimension(:) :: taper
    integer, dimension(1:2) :: taper_protect, taper_len
    integer :: n, np, i, taper_beg, taper_end
    character(len=24), dimension(1:2) :: taper_method
    real, dimension(1:2) :: taper_alpha
    ! Size of the array
    n = size(w)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len = [max(5, nint(0.05*n)), max(5, nint(0.05*n))]
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect = [taper_len(1), n - taper_len(2) + 1]
    end if
    taper_protect = clip(taper_protect, 1, n)
    call assert(taper_protect(2) >= taper_protect(1), ' <taper_1d> Error: Protect range specification error.')
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = alpha
    else
        taper_alpha = [0.0, 0.0]
    end if
    ! Setup the taper
    np = taper_len(1) + taper_protect(2) - taper_protect(1) + 1 + taper_len(2) - count(taper_len /= 0)
    taper = taper_window(np, taper_len, taper_method, taper_alpha)
    ! Tapering the array by multiplication
    wt = w
    if (taper_len(1) == 0) then
        taper_beg = taper_protect(1)
    else
        taper_beg = taper_protect(1) - taper_len(1) + 1
    end if
    if (taper_len(2) == 0) then
        taper_end = taper_protect(2)
    else
        taper_end = taper_protect(2) + taper_len(2) - 1
    end if
    wt(1:taper_beg - 1) = 0.0
    do i = max(1, taper_beg), min(n, taper_end)
        wt(i) = wt(i)*taper(i - taper_beg + 1)
    end do
    wt(taper_end + 1:n) = 0.0
end function taper_1d_complex
!
!> Taper 2D array
!
function taper_2d_complex(w, len, method, alpha, protect) result(wt)
    complex, dimension(:, :) :: w
    integer, dimension(:), optional :: protect, len
    character(len=*), dimension(:), optional :: method
    real, dimension(:), optional :: alpha
    complex, allocatable, dimension(:, :) :: wt
    integer :: n1, n2, i, j
    integer, dimension(1:4) :: taper_protect, taper_len
    character(len=24), dimension(1:4) :: taper_method
    real, dimension(1:4) :: taper_alpha
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len(1:2) = max(5, nint(0.05*n1))
        taper_len(3:4) = max(5, nint(0.05*n2))
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect(1) = taper_len(1)
        taper_protect(2) = n1 - taper_len(2) + 1
        taper_protect(3) = taper_len(3)
        taper_protect(4) = n2 - taper_len(4) + 1
    end if
    taper_protect(1:2) = clip(taper_protect(1:2), 1, n1)
    taper_protect(3:4) = clip(taper_protect(3:4), 1, n2)
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann', 'hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = real(alpha)
    else
        taper_alpha = [0.0, 0.0, 0.0, 0.0]
    end if
    ! Tapering the array by multiplication
    wt = w
    ! dim = 1 tapering
    !$omp parallel do private(j)
    do j = 1, n2
        wt(:, j) = taper_1d_complex(w(:, j), &
            taper_len(1:2), taper_method(1:2), taper_alpha(1:2), taper_protect(1:2))
    end do
    !$omp end parallel do
    ! dim = 2 tapering
    !$omp parallel do private(i)
    do i = 1, n1
        wt(i, :) = taper_1d_complex(wt(i, :), &
            taper_len(3:4), taper_method(3:4), taper_alpha(3:4), taper_protect(3:4))
    end do
    !$omp end parallel do
end function taper_2d_complex
!
!> Taper 3D array
!
function taper_3d_complex(w, len, method, alpha, protect) result(wt)
    complex, dimension(:, :, :) :: w
    integer, dimension(:), optional :: protect, len
    character(len=*), dimension(:), optional :: method
    real, dimension(:), optional :: alpha
    complex, allocatable, dimension(:, :, :) :: wt
    integer :: n1, n2, n3, i, j, k
    integer, dimension(1:6) :: taper_protect, taper_len
    character(len=24), dimension(1:6) :: taper_method
    real, dimension(1:6) :: taper_alpha
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len(1:2) = max(5, nint(0.05*n1))
        taper_len(3:4) = max(5, nint(0.05*n2))
        taper_len(5:6) = max(5, nint(0.05*n3))
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect(1) = taper_len(1)
        taper_protect(2) = n1 - taper_len(2) + 1
        taper_protect(3) = taper_len(3)
        taper_protect(4) = n2 - taper_len(4) + 1
        taper_protect(5) = taper_len(5)
        taper_protect(6) = n3 - taper_len(6) + 1
    end if
    taper_protect(1:2) = clip(taper_protect(1:2), 1, n1)
    taper_protect(3:4) = clip(taper_protect(3:4), 1, n2)
    taper_protect(5:6) = clip(taper_protect(5:6), 1, n3)
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann', 'hann', 'hann', 'hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = real(alpha)
    else
        taper_alpha = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    end if
    ! Tapering the array by multiplication
    wt = w
    ! dim = 3 tapering
    !$omp parallel do private(i, j) collapse(2)
    do i = 1, n1
        do j = 1, n2
            wt(i, j, :) = taper_1d_complex(w(i, j, :), &
                taper_len(5:6), taper_method(5:6), taper_alpha(5:6), taper_protect(5:6))
        end do
    end do
    !$omp end parallel do
    ! dim = 2 tapering
    !$omp parallel do private(i, k) collapse(2)
    do i = 1, n1
        do k = 1, n3
            wt(i, :, k) = taper_1d_complex(wt(i, :, k), &
                taper_len(3:4), taper_method(3:4), taper_alpha(3:4), taper_protect(3:4))
        end do
    end do
    !$omp end parallel do
    ! dim = 1 tapering
    !$omp parallel do private(j, k) collapse(2)
    do j = 1, n2
        do k = 1, n3
            wt(:, j, k) = taper_1d_complex(wt(:, j, k), &
                taper_len(1:2), taper_method(1:2), taper_alpha(1:2), taper_protect(1:2))
        end do
    end do
    !$omp end parallel do
end function taper_3d_complex
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
!> Taper 1D array
!
function taper_1d_dcomplex(w, len, method, alpha, protect) result(wt)
    double complex, dimension(:), intent(in) :: w
    integer, dimension(:), intent(in), optional :: protect, len
    character(len=*), intent(in), dimension(:), optional :: method
    real, dimension(:), intent(in), optional :: alpha
    double complex, allocatable, dimension(:) :: wt
    real, allocatable, dimension(:) :: taper
    integer, dimension(1:2) :: taper_protect, taper_len
    integer :: n, np, i, taper_beg, taper_end
    character(len=24), dimension(1:2) :: taper_method
    real, dimension(1:2) :: taper_alpha
    ! Size of the array
    n = size(w)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len = [max(5, nint(0.05*n)), max(5, nint(0.05*n))]
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect = [taper_len(1), n - taper_len(2) + 1]
    end if
    taper_protect = clip(taper_protect, 1, n)
    call assert(taper_protect(2) >= taper_protect(1), ' <taper_1d> Error: Protect range specification error.')
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = alpha
    else
        taper_alpha = [0.0, 0.0]
    end if
    ! Setup the taper
    np = taper_len(1) + taper_protect(2) - taper_protect(1) + 1 + taper_len(2) - count(taper_len /= 0)
    taper = taper_window(np, taper_len, taper_method, taper_alpha)
    ! Tapering the array by multiplication
    wt = w
    if (taper_len(1) == 0) then
        taper_beg = taper_protect(1)
    else
        taper_beg = taper_protect(1) - taper_len(1) + 1
    end if
    if (taper_len(2) == 0) then
        taper_end = taper_protect(2)
    else
        taper_end = taper_protect(2) + taper_len(2) - 1
    end if
    wt(1:taper_beg - 1) = 0.0
    do i = max(1, taper_beg), min(n, taper_end)
        wt(i) = wt(i)*taper(i - taper_beg + 1)
    end do
    wt(taper_end + 1:n) = 0.0
end function taper_1d_dcomplex
!
!> Taper 2D array
!
function taper_2d_dcomplex(w, len, method, alpha, protect) result(wt)
    double complex, dimension(:, :) :: w
    integer, dimension(:), optional :: protect, len
    character(len=*), dimension(:), optional :: method
    real, dimension(:), optional :: alpha
    double complex, allocatable, dimension(:, :) :: wt
    integer :: n1, n2, i, j
    integer, dimension(1:4) :: taper_protect, taper_len
    character(len=24), dimension(1:4) :: taper_method
    real, dimension(1:4) :: taper_alpha
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len(1:2) = max(5, nint(0.05*n1))
        taper_len(3:4) = max(5, nint(0.05*n2))
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect(1) = taper_len(1)
        taper_protect(2) = n1 - taper_len(2) + 1
        taper_protect(3) = taper_len(3)
        taper_protect(4) = n2 - taper_len(4) + 1
    end if
    taper_protect(1:2) = clip(taper_protect(1:2), 1, n1)
    taper_protect(3:4) = clip(taper_protect(3:4), 1, n2)
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann', 'hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = real(alpha)
    else
        taper_alpha = [0.0, 0.0, 0.0, 0.0]
    end if
    ! Tapering the array by multiplication
    wt = w
    ! dim = 1 tapering
    !$omp parallel do private(j)
    do j = 1, n2
        wt(:, j) = taper_1d_dcomplex(w(:, j), &
            taper_len(1:2), taper_method(1:2), taper_alpha(1:2), taper_protect(1:2))
    end do
    !$omp end parallel do
    ! dim = 2 tapering
    !$omp parallel do private(i)
    do i = 1, n1
        wt(i, :) = taper_1d_dcomplex(wt(i, :), &
            taper_len(3:4), taper_method(3:4), taper_alpha(3:4), taper_protect(3:4))
    end do
    !$omp end parallel do
end function taper_2d_dcomplex
!
!> Taper 3D array
!
function taper_3d_dcomplex(w, len, method, alpha, protect) result(wt)
    double complex, dimension(:, :, :) :: w
    integer, dimension(:), optional :: protect, len
    character(len=*), dimension(:), optional :: method
    real, dimension(:), optional :: alpha
    double complex, allocatable, dimension(:, :, :) :: wt
    integer :: n1, n2, n3, i, j, k
    integer, dimension(1:6) :: taper_protect, taper_len
    character(len=24), dimension(1:6) :: taper_method
    real, dimension(1:6) :: taper_alpha
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Taper parameters
    if (present(len)) then
        taper_len = len
    else
        taper_len(1:2) = max(5, nint(0.05*n1))
        taper_len(3:4) = max(5, nint(0.05*n2))
        taper_len(5:6) = max(5, nint(0.05*n3))
    end if
    if (present(protect)) then
        taper_protect = protect
    else
        taper_protect(1) = taper_len(1)
        taper_protect(2) = n1 - taper_len(2) + 1
        taper_protect(3) = taper_len(3)
        taper_protect(4) = n2 - taper_len(4) + 1
        taper_protect(5) = taper_len(5)
        taper_protect(6) = n3 - taper_len(6) + 1
    end if
    taper_protect(1:2) = clip(taper_protect(1:2), 1, n1)
    taper_protect(3:4) = clip(taper_protect(3:4), 1, n2)
    taper_protect(5:6) = clip(taper_protect(5:6), 1, n3)
    if (present(method)) then
        taper_method = method
    else
        taper_method = ['hann', 'hann', 'hann', 'hann', 'hann', 'hann']
    end if
    if (present(alpha)) then
        taper_alpha = real(alpha)
    else
        taper_alpha = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    end if
    ! Tapering the array by multiplication
    wt = w
    ! dim = 3 tapering
    !$omp parallel do private(i, j) collapse(2)
    do i = 1, n1
        do j = 1, n2
            wt(i, j, :) = taper_1d_dcomplex(w(i, j, :), &
                taper_len(5:6), taper_method(5:6), taper_alpha(5:6), taper_protect(5:6))
        end do
    end do
    !$omp end parallel do
    ! dim = 2 tapering
    !$omp parallel do private(i, k) collapse(2)
    do i = 1, n1
        do k = 1, n3
            wt(i, :, k) = taper_1d_dcomplex(wt(i, :, k), &
                taper_len(3:4), taper_method(3:4), taper_alpha(3:4), taper_protect(3:4))
        end do
    end do
    !$omp end parallel do
    ! dim = 1 tapering
    !$omp parallel do private(j, k) collapse(2)
    do j = 1, n2
        do k = 1, n3
            wt(:, j, k) = taper_1d_dcomplex(wt(:, j, k), &
                taper_len(1:2), taper_method(1:2), taper_alpha(1:2), taper_protect(1:2))
        end do
    end do
    !$omp end parallel do
end function taper_3d_dcomplex
end module libflit_taper
