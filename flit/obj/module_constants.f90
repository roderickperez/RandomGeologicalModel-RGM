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
!> Constants for modeling, inversion and imaging
!
module libflit_constants
    use iso_fortran_env
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
    implicit none
    ! Constant unit imaginary number
    complex, parameter :: const_i = cmplx(0.0, 1.0)
    ! Constant complex zero
    complex, parameter :: const_complex_zero = cmplx(0.0, 0.0)
    complex, parameter :: const_dcomplex_zero = dcmplx(0.0d0, 0.0d0)
    ! Constant Pi
    double precision, parameter :: const_pi = 3.141592653589793238462643
    double precision, parameter :: const_pi_half = 1.570796326794896619231321
    double precision, parameter :: const_pi_inv = 0.318309886183790671537767
    double precision, parameter :: const_pi_sqr = 9.869604401089358618834491
    double precision, parameter :: const_pi_sqrt = 1.772453850905516027298167
    double precision, parameter :: const_pi_ln = 1.144729885849400174143427
    double precision, parameter :: const_pi_log10 = 0.497149872694133854351268
    ! Euler's number e
    double precision, parameter :: const_ee = 2.71828182845904523560287
    double precision, parameter :: const_ee_inv = 0.367879441171442321595523
    double precision, parameter :: const_ee_sqr = 7.389056098930650227230427
    double precision, parameter :: const_ee_log10 = 0.434294481903251827651129
    ! sqrt(2)
    double precision, parameter :: const_sqrt2 = 1.4142135623730951
    double precision, parameter :: const_sqrt2_inv = 0.7071067811865475
    ! sqrt(3)
    double precision, parameter :: const_sqrt3 = 1.7320508075688772
    double precision, parameter :: const_sqrt3_inv = 0.5773502691896258
    ! sqrt(5)
    double precision, parameter :: const_sqrt5 = 2.23606797749979
    double precision, parameter :: const_sqrt5_inv = 0.4472135954999579
    ! degree radian
    double precision, parameter :: const_deg2rad = 0.017453292519943295
    double precision, parameter :: const_rad2deg = 57.29577951308232
    ! Volumes
    double precision, parameter :: const_oz2cbm = 0.0000295703125
    double precision, parameter :: const_cbm2oz = 33817.70145310435931307794
    ! Length
    real, parameter :: const_ft2m = 0.3048
    real, parameter :: const_m2ft = 3.28084
    ! Area
    double precision, parameter :: const_sqft2sqm = 0.09290304
    double precision, parameter :: const_sqm2sqft = 10.76391041670972230833
    ! golden ratio
    double precision, parameter :: const_golden = 1.618033988749894
    ! light in vacuum
    real, parameter :: const_lightc = 299752458.0
    ! Planck constant
    double precision, parameter :: const_planckh = 6.62607004081d-34
    ! Vacuum permitivity
    double precision, parameter :: const_vacuum_permittivity = 8.8541878128d-12
    ! Vacuum permeability
    double precision, parameter :: const_vacuum_permeability = 1.25663706212d-6
    ! Electron charge
    double precision, parameter :: const_elementary_charge = 1.602176634d-19
    ! Fine structure constant
    double precision, parameter :: const_fine_structure = 0.00729735256278713575
    double precision, parameter :: const_fine_structure_reciprocial = 137.035999206
    ! Scales
    double precision, parameter :: const_yotta = 1.0d24
    double precision, parameter :: const_zetta = 1.0d21
    double precision, parameter :: const_exa = 1.0d18
    double precision, parameter :: const_peta = 1.0d15
    double precision, parameter :: const_tera = 1.0d12
    double precision, parameter :: const_giga = 1.0d9
    double precision, parameter :: const_mega = 1.0d6
    double precision, parameter :: const_kilo = 1.0d3
    double precision, parameter :: const_milli = 1.0d-3
    double precision, parameter :: const_micro = 1.0d-6
    double precision, parameter :: const_nano = 1.0d-9
    double precision, parameter :: const_pico = 1.0d-12
    double precision, parameter :: const_femto = 1.0d-15
    double precision, parameter :: const_atto = 1.0d-18
    double precision, parameter :: const_zepto = 1.0d-21
    double precision, parameter :: const_yocto = 1.0d-24
    ! tiny, small and huge value
    real, parameter :: float_tiny = tiny(0.0)
    real, parameter :: float_small = 1.0e-6
    real, parameter :: float_large = 1.0e+6
    real, parameter :: float_huge = huge(0.0)
    double precision, parameter :: double_tiny = tiny(0.0d0)
    double precision, parameter :: double_small = 1.0d-6
    double precision, parameter :: double_large = 1.0d+6
    double precision, parameter :: double_huge = huge(0.0d0)
    integer(4), parameter :: int4_tiny = 0
    integer(4), parameter :: int4_huge = huge(int(0, kind=4))
    integer(2), parameter :: int2_tiny = 0
    integer(2), parameter :: int2_huge = huge(int(0, kind=2))
    integer(1), parameter :: int1_tiny = 0
    integer(1), parameter :: int1_huge = huge(int(0, kind=1))
    ! ! The following definitions don't work... so have to define a nan function which is nan = sqrt(-1.0)
    ! integer, parameter :: int_nan = huge(0) + huge(0)
    ! real, parameter :: float_nan = transfer(-4194304_int32, 1._real32)
    ! double precision, parameter :: double_nan = transfer(-2251799813685248_int64, 1._real64)
    ! complex, parameter :: complex_nan = cmplx(float_nan, float_nan)
    ! double complex, parameter :: dcomplex_nan = dcmplx(double_nan, double_nan)
    character(len=1), parameter :: char_space = ' '
    character(len=1), parameter :: char_newline = achar(10)
    character(len=1), parameter :: char_vrectangle = achar(219)
    character(len=1), parameter :: char_hrectangle = achar(220)
    character(len=1), parameter :: char_square = achar(254)
    character(len=1), parameter :: char_degree = achar(248)
    character(len=1), parameter :: char_inf = achar(236)
    character(len=1), parameter :: char_delta = achar(235)
    character(len=1), parameter :: char_approx = achar(247)
    character(len=1), parameter :: char_ge = achar(242)
    character(len=1), parameter :: char_le = achar(243)
    character(len=1), parameter :: char_pm = achar(241)
    character(len=1), parameter :: char_define = achar(240)
    character(len=1), parameter :: char_pi = achar(227)
    character(len=1), parameter :: char_sum = achar(228)
    ! For reference, in C++, Greek letters can be defined through unicodes.
    ! But at the same time, if one want to print out a Greek letter,
    ! it is always more convenient to directly copy the letters directly
    ! to the code and let it print...
    !! Letter Description Escape-Sequence
    ! -------------------------------------
    ! A Alpha \U00000391
    ! B Beta \U00000392
    ! \U00000393 Gamma \U00000393
    ! \U00000394 Delta \U00000394
    ! \U00000395 Epsilon \U00000395
    ! \U00000396 Zeta \U00000396
    ! \U00000397 Eta \U00000397
    ! \U00000398 Theta \U00000398
    ! \U00000399 Iota \U00000399
    ! \U0000039a Kappa \U0000039a
    ! \U0000039b Lambda \U0000039b
    ! \U0000039c Mu \U0000039c
    ! \U0000039d Nu \U0000039d
    ! \U0000039e Xi \U0000039e
    ! \U0000039f Omicron \U0000039f
    ! \U000003a0 Pi \U000003a0
    ! \U000003a1 Rho \U000003a1
    ! \U000003a3 Sigma \U000003a3
    ! \U000003a4 Tau \U000003a4
    ! \U000003a5 Upsilon \U000003a5
    ! \U000003a6 Phi \U000003a6
    ! \U000003a7 Chi \U000003a7
    ! \U000003a8 Psi \U000003a8
    ! \U000003a9 Omega \U000003a9
    ! \U000003b1 Alpha \U000003b1
    ! \U000003b2 Beta \U000003b2
    ! \U000003b3 Gamma \U000003b3
    ! \U000003b4 Delta \U000003b4
    ! \U000003b5 Epsilon \U000003b5
    ! \U000003b6 Zeta \U000003b6
    ! \U000003b7 Eta \U000003b7
    ! \U000003b8 Theta \U000003b8
    ! \U000003b9 Iota \U000003b9
    ! \U000003ba Kappa \U000003ba
    ! \U000003bb Lambda \U000003bb
    ! \U000003bc Mu \U000003bc
    ! \U000003bd Nu \U000003bd
    ! \U000003be Xi \U000003be
    ! \U000003bf Omicron \U000003bf
    ! \U000003c0 Pi \U000003c0
    ! \U000003c1 Rho \U000003c1
    ! \U000003c3 Sigma \U000003c3
    ! \U000003c4 Tau \U000003c4
    ! \U000003c5 Upsilon \U000003c5
    ! \U000003c6 Phi \U000003c6
    ! \U000003c7 Chi \U000003c7
    ! \U000003c8 Psi \U000003c8
    ! \U000003c9 Omega \U000003c9
contains
    function nan() result(x)
        real :: x
        x = ieee_value(x, ieee_quiet_nan)
    end function nan
end module libflit_constants
