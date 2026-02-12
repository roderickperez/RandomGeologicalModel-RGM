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
module libflit_iirfilt
    use libflit_specialfunc
    use libflit_constants
    use libflit_array
    use libflit_unique
    use libflit_error
    use libflit_utility
    implicit none
    ! External interface
    interface
        ! Butterworth lowpass filtering
        subroutine butterworth_lowpass_filt(x, xx, n, d, f, order) bind(c, name='butterworth_lowpass')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine butterworth_lowpass_filt
        ! Chebyshev-I lowpass filtering
        subroutine chebyshev1_lowpass_filt(x, xx, n, d, f, order, pass_ripple_db) bind(c, name='chebyshev1_lowpass')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f, pass_ripple_db
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine chebyshev1_lowpass_filt
        ! Chebyshev-II lowpass filtering
        subroutine chebyshev2_lowpass_filt(x, xx, n, d, f, order, stop_ripple_db) bind(c, name='chebyshev2_lowpass')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f, stop_ripple_db
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine chebyshev2_lowpass_filt
        ! Butterworth highpass filtering
        subroutine butterworth_highpass_filt(x, xx, n, d, f, order) bind(c, name='butterworth_highpass')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine butterworth_highpass_filt
        ! Chebyshev-I highpass filtering
        subroutine chebyshev1_highpass_filt(x, xx, n, d, f, order, pass_ripple_db) bind(c, name='chebyshev1_highpass')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f, pass_ripple_db
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine chebyshev1_highpass_filt
        ! Chebyshev-II highpass filtering
        subroutine chebyshev2_highpass_filt(x, xx, n, d, f, order, stop_ripple_db) bind(c, name='chebyshev2_highpass')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f, stop_ripple_db
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine chebyshev2_highpass_filt
        ! Butterworth bandpass filtering
        subroutine butterworth_bandpass_filt(x, xx, n, d, f1, f2, order) bind(c, name='butterworth_bandpass')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f1, f2
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine butterworth_bandpass_filt
        ! Chebyshev-I bandpass filtering
        subroutine chebyshev1_bandpass_filt(x, xx, n, d, f1, f2, order, pass_ripple_db) bind(c, name='chebyshev1_bandpass')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f1, f2, pass_ripple_db
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine chebyshev1_bandpass_filt
        ! Chebyshev-II bandpass filtering
        subroutine chebyshev2_bandpass_filt(x, xx, n, d, f1, f2, order, stop_ripple_db) bind(c, name='chebyshev2_bandpass')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f1, f2, stop_ripple_db
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine chebyshev2_bandpass_filt
        ! Butterworth bandstop filtering
        subroutine butterworth_bandstop_filt(x, xx, n, d, f1, f2, order) bind(c, name='butterworth_bandstop')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f1, f2
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine butterworth_bandstop_filt
        ! Chebyshev-I bandstop filtering
        subroutine chebyshev1_bandstop_filt(x, xx, n, d, f1, f2, order, pass_ripple_db) bind(c, name='chebyshev1_bandstop')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f1, f2, pass_ripple_db
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine chebyshev1_bandstop_filt
        ! Chebyshev-II bandstop filtering
        subroutine chebyshev2_bandstop_filt(x, xx, n, d, f1, f2, order, stop_ripple_db) bind(c, name='chebyshev2_bandstop')
            use iso_c_binding, only: c_int, c_float
            integer(kind=c_int), value :: n, order
            real(kind=c_float), value :: d, f1, f2, stop_ripple_db
            real, dimension(*), intent(in) :: x
            real, dimension(*), intent(out) :: xx
        end subroutine chebyshev2_bandstop_filt
    end interface
    interface iir_filt
        module procedure :: iir_filt_float
    end interface iir_filt
    private
    public :: iir_filt
contains
    !===================================================
    ! IIR filtering
    function iir_filt_float(x, dt, type, flow, fhigh, order, db, method) result(xx)
        real, dimension(:), intent(in) :: x
        real, intent(in) :: dt
        character(len=*), intent(in) :: type
        real, intent(in) :: flow, fhigh
        integer, intent(in), optional :: order
        real, intent(in), optional :: db
        character(len=*), intent(in), optional :: method
        real, allocatable, dimension(:) :: xx
        integer :: iir_order
        character(len=24) :: iir_method
        real :: iir_db
        if (present(method)) then
            iir_method = method
        else
            iir_method = 'butter'
        end if
        if (present(order)) then
            iir_order = clip(order, 1, 10)
        else
            iir_order = 8
        end if
        if (present(db)) then
            iir_db = db
        else
            select case (iir_method)
                case ('cheby1')
                    iir_db = 0.5
                case ('cheby2')
                    iir_db = 40
            end select
        end if
        select case (iir_method)
            case ('cheby1')
                call assert(iir_db <= 1.0, 'Error: Chebyshev-I passband ripple dB too large.')
            case ('cheby2')
                call assert(iir_db >= 10.0, 'Error: Chebyshev-II stopband ripple dB too small.')
        end select
        xx = zeros_like(x)
        select case (type)
            case ('lowpass')
                select case (iir_method)
                    case ('butter')
                        call butterworth_lowpass_filt(x, xx, size(x), dt, fhigh, order=iir_order)
                    case ('cheby1')
                        call chebyshev1_lowpass_filt(x, xx, size(x), dt, fhigh, order=iir_order, pass_ripple_db=iir_db)
                    case ('cheby2')
                        call chebyshev2_lowpass_filt(x, xx, size(x), dt, fhigh, order=iir_order, stop_ripple_db=iir_db)
                end select
            case ('highpass')
                select case (iir_method)
                    case ('butter')
                        call butterworth_highpass_filt(x, xx, size(x), dt, flow, order=iir_order)
                    case ('cheby1')
                        call chebyshev1_highpass_filt(x, xx, size(x), dt, flow, order=iir_order, pass_ripple_db=iir_db)
                    case ('cheby2')
                        call chebyshev2_highpass_filt(x, xx, size(x), dt, flow, order=iir_order, stop_ripple_db=iir_db)
                end select
            case ('bandpass')
                select case (iir_method)
                    case ('butter')
                        call butterworth_bandpass_filt(x, xx, size(x), dt, flow, fhigh, order=iir_order)
                    case ('cheby1')
                        call chebyshev1_bandpass_filt(x, xx, size(x), dt, flow, fhigh, order=iir_order, pass_ripple_db=iir_db)
                    case ('cheby2')
                        call chebyshev2_bandpass_filt(x, xx, size(x), dt, flow, fhigh, order=iir_order, stop_ripple_db=iir_db)
                end select
            case ('bandstop')
                select case (iir_method)
                    case ('butter')
                        call butterworth_bandstop_filt(x, xx, size(x), dt, flow, fhigh, order=iir_order)
                    case ('cheby1')
                        call chebyshev1_bandstop_filt(x, xx, size(x), dt, flow, fhigh, order=iir_order, pass_ripple_db=iir_db)
                    case ('cheby2')
                        call chebyshev2_bandstop_filt(x, xx, size(x), dt, flow, fhigh, order=iir_order, stop_ripple_db=iir_db)
                end select
        end select
    end function iir_filt_float
end module libflit_iirfilt
