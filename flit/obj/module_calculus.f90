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
module libflit_calculus
    use libflit_array
    use libflit_error
    implicit none
    interface deriv
        module procedure :: differentiate_1d_float
        module procedure :: differentiate_2d_float
        module procedure :: differentiate_3d_float
        module procedure :: differentiate_1d_double
        module procedure :: differentiate_2d_double
        module procedure :: differentiate_3d_double
        module procedure :: differentiate_1d_complex
        module procedure :: differentiate_2d_complex
        module procedure :: differentiate_3d_complex
        module procedure :: differentiate_1d_dcomplex
        module procedure :: differentiate_2d_dcomplex
        module procedure :: differentiate_3d_dcomplex
    end interface deriv
    interface integ
        module procedure :: integrate_1d_float
        module procedure :: integrate_2d_float
        module procedure :: integrate_3d_float
        module procedure :: integrate_1d_double
        module procedure :: integrate_2d_double
        module procedure :: integrate_3d_double
        module procedure :: integrate_1d_complex
        module procedure :: integrate_2d_complex
        module procedure :: integrate_3d_complex
        module procedure :: integrate_1d_dcomplex
        module procedure :: integrate_2d_dcomplex
        module procedure :: integrate_3d_dcomplex
    end interface integ
    interface cumsum
        module procedure :: cumsum_1d_float
        module procedure :: cumsum_2d_float
        module procedure :: cumsum_3d_float
        module procedure :: cumsum_1d_double
        module procedure :: cumsum_2d_double
        module procedure :: cumsum_3d_double
        module procedure :: cumsum_1d_complex
        module procedure :: cumsum_2d_complex
        module procedure :: cumsum_3d_complex
        module procedure :: cumsum_1d_dcomplex
        module procedure :: cumsum_2d_dcomplex
        module procedure :: cumsum_3d_dcomplex
    end interface cumsum
    private
    public :: taylor_fd_coefs
    public :: deriv
    public :: integ
    public :: cumsum
    ! Center finite-difference coefficients
    double precision, dimension(1:3), parameter, public :: &
        fdcoef_1c10 = [ &
        -5.0000000000000000E-01, &
        0.0000000000000000E+00, &
        5.0000000000000000E-01]
    double precision, dimension(1:5), parameter, public :: &
        fdcoef_1c20 = [ &
        8.3333333333333329E-02, &
        -6.6666666666666663E-01, &
        0.0000000000000000E+00, &
        6.6666666666666663E-01, &
        -8.3333333333333329E-02]
    double precision, dimension(1:7), parameter, public :: &
        fdcoef_1c30 = [ &
        -1.6666666666666666E-02, &
        1.4999999999999999E-01, &
        -7.5000000000000000E-01, &
        -7.4014868308343765E-17, &
        7.5000000000000000E-01, &
        -1.5000000000000002E-01, &
        1.6666666666666666E-02]
    double precision, dimension(1:9), parameter, public :: &
        fdcoef_1c40 = [ &
        3.5714285714285718E-03, &
        -3.8095238095238092E-02, &
        1.9999999999999998E-01, &
        -8.0000000000000004E-01, &
        -3.0531133177191805E-16, &
        8.0000000000000016E-01, &
        -2.0000000000000001E-01, &
        3.8095238095238092E-02, &
        -3.5714285714285709E-03]
    double precision, dimension(1:11), parameter, public :: &
        fdcoef_1c50 = [ &
        -7.9365079365079365E-04, &
        9.9206349206349201E-03, &
        -5.9523809523809521E-02, &
        2.3809523809523808E-01, &
        -8.3333333333333337E-01, &
        -8.8817841970012528E-17, &
        8.3333333333333326E-01, &
        -2.3809523809523805E-01, &
        5.9523809523809521E-02, &
        -9.9206349206349201E-03, &
        7.9365079365079365E-04]
    double precision, dimension(1:13), parameter, public :: &
        fdcoef_1c60 = [ &
        1.8037518037518038E-04, &
        -2.5974025974025978E-03, &
        1.7857142857142856E-02, &
        -7.9365079365079361E-02, &
        2.6785714285714285E-01, &
        -8.5714285714285710E-01, &
        1.4802973661668753E-16, &
        8.5714285714285698E-01, &
        -2.6785714285714285E-01, &
        7.9365079365079361E-02, &
        -1.7857142857142860E-02, &
        2.5974025974025978E-03, &
        -1.8037518037518040E-04]
    double precision, dimension(1:15), parameter, public :: &
        fdcoef_1c70 = [ &
        -4.1625041625041618E-05, &
        6.7987567987567988E-04, &
        -5.3030303030303034E-03, &
        2.6515151515151516E-02, &
        -9.7222222222222238E-02, &
        2.9166666666666674E-01, &
        -8.7500000000000000E-01, &
        -3.6478756523397999E-16, &
        8.7500000000000000E-01, &
        -2.9166666666666663E-01, &
        9.7222222222222224E-02, &
        -2.6515151515151516E-02, &
        5.3030303030303034E-03, &
        -6.7987567987567988E-04, &
        4.1625041625041625E-05]
    double precision, dimension(1:17), parameter, public :: &
        fdcoef_1c80 = [ &
        9.7125097125097091E-06, &
        -1.7760017760017757E-04, &
        1.5540015540015540E-03, &
        -8.7024087024087024E-03, &
        3.5353535353535352E-02, &
        -1.1313131313131311E-01, &
        3.1111111111111112E-01, &
        -8.8888888888888884E-01, &
        -3.4694469519536142E-16, &
        8.8888888888888895E-01, &
        -3.1111111111111106E-01, &
        1.1313131313131312E-01, &
        -3.5353535353535352E-02, &
        8.7024087024087024E-03, &
        -1.5540015540015540E-03, &
        1.7760017760017760E-04, &
        -9.7125097125097125E-06]
    ! forward finite-difference coefficients
    double precision, dimension(1:2), parameter, public :: &
        fdcoef_1f10 = [ &
        -1.0000000000000000E+00, &
        1.0000000000000000E+00]
    double precision, dimension(1:3), parameter, public :: &
        fdcoef_1f20 = [ &
        -1.5000000000000000E+00, &
        2.0000000000000000E+00, &
        -5.0000000000000000E-01]
    double precision, dimension(1:4), parameter, public :: &
        fdcoef_1f30 = [ &
        -1.8333333333333333E+00, &
        3.0000000000000000E+00, &
        -1.5000000000000000E+00, &
        3.3333333333333331E-01]
    double precision, dimension(1:5), parameter, public :: &
        fdcoef_1f40 = [ &
        -2.0833333333333330E+00, &
        4.0000000000000000E+00, &
        -3.0000000000000000E+00, &
        1.3333333333333333E+00, &
        -2.5000000000000000E-01]
    double precision, dimension(1:6), parameter, public :: &
        fdcoef_1f50 = [ &
        -2.2833333333333328E+00, &
        5.0000000000000000E+00, &
        -5.0000000000000000E+00, &
        3.3333333333333330E+00, &
        -1.2500000000000000E+00, &
        2.0000000000000001E-01]
    double precision, dimension(1:7), parameter, public :: &
        fdcoef_1f60 = [ &
        -2.4499999999999993E+00, &
        6.0000000000000000E+00, &
        -7.5000000000000000E+00, &
        6.6666666666666670E+00, &
        -3.7500000000000000E+00, &
        1.2000000000000002E+00, &
        -1.6666666666666666E-01]
    double precision, dimension(1:8), parameter, public :: &
        fdcoef_1f70 = [ &
        -2.5928571428571421E+00, &
        7.0000000000000000E+00, &
        -1.0500000000000000E+01, &
        1.1666666666666668E+01, &
        -8.7500000000000000E+00, &
        4.2000000000000011E+00, &
        -1.1666666666666665E+00, &
        1.4285714285714285E-01]
    double precision, dimension(1:9), parameter, public :: &
        fdcoef_1f80 = [ &
        -2.7178571428571421E+00, &
        8.0000000000000000E+00, &
        -1.4000000000000000E+01, &
        1.8666666666666668E+01, &
        -1.7500000000000000E+01, &
        1.1200000000000003E+01, &
        -4.6666666666666661E+00, &
        1.1428571428571428E+00, &
        -1.2500000000000000E-01]
    ! Backward finite-difference coefficients
    double precision, dimension(1:2), parameter, public :: &
        fdcoef_1b10 = [ &
        -1.0000000000000000E+00, &
        1.0000000000000000E+00]
    double precision, dimension(1:3), parameter, public :: &
        fdcoef_1b20 = [ &
        5.0000000000000000E-01, &
        -2.0000000000000000E+00, &
        1.5000000000000000E+00]
    double precision, dimension(1:4), parameter, public :: &
        fdcoef_1b30 = [ &
        -3.3333333333333331E-01, &
        1.5000000000000000E+00, &
        -3.0000000000000000E+00, &
        1.8333333333333333E+00]
    double precision, dimension(1:5), parameter, public :: &
        fdcoef_1b40 = [ &
        2.5000000000000000E-01, &
        -1.3333333333333333E+00, &
        3.0000000000000000E+00, &
        -4.0000000000000000E+00, &
        2.0833333333333330E+00]
    double precision, dimension(1:6), parameter, public :: &
        fdcoef_1b50 = [ &
        -2.0000000000000001E-01, &
        1.2500000000000000E+00, &
        -3.3333333333333335E+00, &
        5.0000000000000000E+00, &
        -5.0000000000000000E+00, &
        2.2833333333333332E+00]
    double precision, dimension(1:7), parameter, public :: &
        fdcoef_1b60 = [ &
        1.6666666666666666E-01, &
        -1.2000000000000000E+00, &
        3.7500000000000000E+00, &
        -6.6666666666666670E+00, &
        7.5000000000000000E+00, &
        -6.0000000000000000E+00, &
        2.4500000000000002E+00]
    double precision, dimension(1:8), parameter, public :: &
        fdcoef_1b70 = [ &
        -1.4285714285714285E-01, &
        1.1666666666666667E+00, &
        -4.2000000000000002E+00, &
        8.7500000000000000E+00, &
        -1.1666666666666666E+01, &
        1.0500000000000000E+01, &
        -7.0000000000000000E+00, &
        2.5928571428571425E+00]
    double precision, dimension(1:9), parameter, public :: &
        fdcoef_1b80 = [ &
        1.2500000000000000E-01, &
        -1.1428571428571428E+00, &
        4.6666666666666670E+00, &
        -1.1199999999999999E+01, &
        1.7500000000000000E+01, &
        -1.8666666666666668E+01, &
        1.4000000000000000E+01, &
        -8.0000000000000000E+00, &
        2.7178571428571425E+00]
    ! Staggered-grid central finite-difference coefficients
    double precision, dimension(1:2), parameter, public :: &
        fdcoef_1s10 = [ &
        -1.0000000000000000E+00, &
        1.0000000000000000E+00]
    double precision, dimension(1:4), parameter, public :: &
        fdcoef_1s20 = [ &
        4.1666666666666664E-02, &
        -1.1250000000000000E+00, &
        1.1250000000000000E+00, &
        -4.1666666666666664E-02]
    double precision, dimension(1:6), parameter, public :: &
        fdcoef_1s30 = [ &
        -4.6874999999999998E-03, &
        6.5104166666666657E-02, &
        -1.1718750000000000E+00, &
        1.1718750000000000E+00, &
        -6.5104166666666644E-02, &
        4.6874999999999972E-03]
    double precision, dimension(1:8), parameter, public :: &
        fdcoef_1s40 = [ &
        6.9754464285714287E-04, &
        -9.5703124999999990E-03, &
        7.9752604166666671E-02, &
        -1.1962890625000000E+00, &
        1.1962890625000000E+00, &
        -7.9752604166666671E-02, &
        9.5703124999999972E-03, &
        -6.9754464285714233E-04]
    double precision, dimension(1:10), parameter, public :: &
        fdcoef_1s50 = [ &
        -1.1867947048611101E-04, &
        1.7656598772321430E-03, &
        -1.3842773437499999E-02, &
        8.9721679687500042E-02, &
        -1.2112426757812500E+00, &
        1.2112426757812500E+00, &
        -8.9721679687500000E-02, &
        1.3842773437500004E-02, &
        -1.7656598772321434E-03, &
        1.1867947048611115E-04]
    double precision, dimension(1:12), parameter, public :: &
        fdcoef_1s60 = [ &
        2.1847811612215930E-05, &
        -3.5900539822048704E-04, &
        2.9672895159040180E-03, &
        -1.7447662353515622E-02, &
        9.6931457519531278E-02, &
        -1.2213363647460938E+00, &
        1.2213363647460938E+00, &
        -9.6931457519531167E-02, &
        1.7447662353515608E-02, &
        -2.9672895159040119E-03, &
        3.5900539822048536E-04, &
        -2.1847811612215849E-05]
    double precision, dimension(1:14), parameter, public :: &
        fdcoef_1s70 = [ &
        -4.2365147517277731E-06, &
        7.6922503384676844E-05, &
        -6.8945354885525283E-04, &
        4.1789327348981577E-03, &
        -2.0476770401000974E-02, &
        1.0238385200500491E-01, &
        -1.2286062240600586E+00, &
        1.2286062240600586E+00, &
        -1.0238385200500486E-01, &
        2.0476770401000967E-02, &
        -4.1789327348981525E-03, &
        6.8945354885525077E-04, &
        -7.6922503384676709E-05, &
        4.2365147517277553E-06]
    double precision, dimension(1:16), parameter, public :: &
        fdcoef_1s80 = [ &
        8.5234642028808598E-07, &
        -1.7021711056049085E-05, &
        1.6641887751492568E-04, &
        -1.0772711700863316E-03, &
        5.3423855985913959E-03, &
        -2.3036366701126087E-02, &
        1.0664984583854678E-01, &
        -1.2340910732746124E+00, &
        1.2340910732746126E+00, &
        -1.0664984583854666E-01, &
        2.3036366701126097E-02, &
        -5.3423855985913968E-03, &
        1.0772711700863297E-03, &
        -1.6641887751492573E-04, &
        1.7021711056049027E-05, &
        -8.5234642028808460E-07]
    ! Center FD, 2nd order
    double precision, dimension(1:3), parameter, public :: &
        fdcoef_2c10 = [ &
        1.0000000000000000E+00, &
        -2.0000000000000000E+00, &
        1.0000000000000000E+00]
    double precision, dimension(1:5), parameter, public :: &
        fdcoef_2c20 = [ &
        -8.3333333333333329E-02, &
        1.3333333333333333E+00, &
        -2.5000000000000000E+00, &
        1.3333333333333335E+00, &
        -8.3333333333333343E-02]
    double precision, dimension(1:7), parameter, public :: &
        fdcoef_2c30 = [ &
        1.1111111111111108E-02, &
        -1.4999999999999999E-01, &
        1.5000000000000000E+00, &
        -2.7222222222222219E+00, &
        1.5000000000000000E+00, &
        -1.4999999999999999E-01, &
        1.1111111111111108E-02]
    double precision, dimension(1:9), parameter, public :: &
        fdcoef_2c40 = [ &
        -1.7857142857142863E-03, &
        2.5396825396825379E-02, &
        -1.9999999999999996E-01, &
        1.6000000000000001E+00, &
        -2.8472222222222214E+00, &
        1.5999999999999999E+00, &
        -1.9999999999999993E-01, &
        2.5396825396825373E-02, &
        -1.7857142857142833E-03]
    double precision, dimension(1:11), parameter, public :: &
        fdcoef_2c50 = [ &
        3.1746031746031789E-04, &
        -4.9603174603174618E-03, &
        3.9682539682539694E-02, &
        -2.3809523809523814E-01, &
        1.6666666666666670E+00, &
        -2.9272222222222219E+00, &
        1.6666666666666665E+00, &
        -2.3809523809523800E-01, &
        3.9682539682539666E-02, &
        -4.9603174603174566E-03, &
        3.1746031746031724E-04]
    double precision, dimension(1:13), parameter, public :: &
        fdcoef_2c60 = [ &
        -6.0125060125060141E-05, &
        1.0389610389610368E-03, &
        -8.9285714285714246E-03, &
        5.2910052910052983E-02, &
        -2.6785714285714285E-01, &
        1.7142857142857140E+00, &
        -2.9827777777777786E+00, &
        1.7142857142857142E+00, &
        -2.6785714285714296E-01, &
        5.2910052910052942E-02, &
        -8.9285714285714420E-03, &
        1.0389610389610407E-03, &
        -6.0125060125060222E-05]
    double precision, dimension(1:15), parameter, public :: &
        fdcoef_2c70 = [ &
        1.1892869035726177E-05, &
        -2.2662522662522704E-04, &
        2.1212121212121227E-03, &
        -1.3257575757575756E-02, &
        6.4814814814814797E-02, &
        -2.9166666666666669E-01, &
        1.7500000000000000E+00, &
        -3.0235941043083892E+00, &
        1.7499999999999998E+00, &
        -2.9166666666666663E-01, &
        6.4814814814814728E-02, &
        -1.3257575757575739E-02, &
        2.1212121212121175E-03, &
        -2.2662522662522606E-04, &
        1.1892869035726143E-05]
    double precision, dimension(1:17), parameter, public :: &
        fdcoef_2c80 = [ &
        -2.4281274281274256E-06, &
        5.0742907885765017E-05, &
        -5.1800051800051999E-04, &
        3.4809634809634810E-03, &
        -1.7676767676767680E-02, &
        7.5420875420875402E-02, &
        -3.1111111111111123E-01, &
        1.7777777777777781E+00, &
        -3.0548441043083896E+00, &
        1.7777777777777768E+00, &
        -3.1111111111111084E-01, &
        7.5420875420875347E-02, &
        -1.7676767676767652E-02, &
        3.4809634809634736E-03, &
        -5.1800051800051663E-04, &
        5.0742907885764868E-05, &
        -2.4281274281274197E-06]
    ! Forward FD, 2nd order
    double precision, dimension(1:3), parameter, public :: &
        fdcoef_2f10 = [ &
        1.0000000000000000E+00, &
        -2.0000000000000000E+00, &
        1.0000000000000000E+00]
    double precision, dimension(1:4), parameter, public :: &
        fdcoef_2f20 = [ &
        2.0000000000000000E+00, &
        -5.0000000000000000E+00, &
        4.0000000000000000E+00, &
        -1.0000000000000000E+00]
    double precision, dimension(1:5), parameter, public :: &
        fdcoef_2f30 = [ &
        2.9166666666666665E+00, &
        -8.6666666666666661E+00, &
        9.5000000000000000E+00, &
        -4.6666666666666670E+00, &
        9.1666666666666663E-01]
    double precision, dimension(1:6), parameter, public :: &
        fdcoef_2f40 = [ &
        3.7500000000000000E+00, &
        -1.2833333333333332E+01, &
        1.7833333333333332E+01, &
        -1.3000000000000002E+01, &
        5.0833333333333330E+00, &
        -8.3333333333333326E-01]
    double precision, dimension(1:7), parameter, public :: &
        fdcoef_2f50 = [ &
        4.5111111111111111E+00, &
        -1.7399999999999999E+01, &
        2.9250000000000000E+01, &
        -2.8222222222222229E+01, &
        1.6500000000000000E+01, &
        -5.4000000000000004E+00, &
        7.6111111111111107E-01]
    double precision, dimension(1:8), parameter, public :: &
        fdcoef_2f60 = [ &
        5.2111111111111104E+00, &
        -2.2299999999999997E+01, &
        4.3950000000000003E+01, &
        -5.2722222222222236E+01, &
        4.1000000000000000E+01, &
        -2.0100000000000001E+01, &
        5.6611111111111105E+00, &
        -6.9999999999999984E-01]
    double precision, dimension(1:9), parameter, public :: &
        fdcoef_2f70 = [ &
        5.8593253968253958E+00, &
        -2.7485714285714284E+01, &
        6.2100000000000001E+01, &
        -8.9022222222222240E+01, &
        8.6375000000000000E+01, &
        -5.6400000000000006E+01, &
        2.3811111111111110E+01, &
        -5.8857142857142843E+00, &
        6.4821428571428552E-01]
    double precision, dimension(1:10), parameter, public :: &
        fdcoef_2f80 = [ &
        6.4632936507936494E+00, &
        -3.2921428571428571E+01, &
        8.3842857142857142E+01, &
        -1.3975555555555559E+02, &
        1.6247499999999999E+02, &
        -1.3250000000000000E+02, &
        7.4544444444444437E+01, &
        -2.7628571428571423E+01, &
        6.0839285714285696E+00, &
        -6.0396825396825371E-01]
    ! Backward FD, 2nd order
    double precision, dimension(1:3), parameter, public :: &
        fdcoef_2b10 = [ &
        1.0000000000000000E+00, &
        -2.0000000000000000E+00, &
        1.0000000000000000E+00]
    double precision, dimension(1:4), parameter, public :: &
        fdcoef_2b20 = [ &
        -1.0000000000000000E+00, &
        4.0000000000000000E+00, &
        -5.0000000000000000E+00, &
        2.0000000000000000E+00]
    double precision, dimension(1:5), parameter, public :: &
        fdcoef_2b30 = [ &
        9.1666666666666663E-01, &
        -4.6666666666666670E+00, &
        9.5000000000000000E+00, &
        -8.6666666666666661E+00, &
        2.9166666666666665E+00]
    double precision, dimension(1:6), parameter, public :: &
        fdcoef_2b40 = [ &
        -8.3333333333333326E-01, &
        5.0833333333333330E+00, &
        -1.3000000000000000E+01, &
        1.7833333333333332E+01, &
        -1.2833333333333332E+01, &
        3.7500000000000000E+00]
    double precision, dimension(1:7), parameter, public :: &
        fdcoef_2b50 = [ &
        7.6111111111111107E-01, &
        -5.4000000000000004E+00, &
        1.6500000000000000E+01, &
        -2.8222222222222218E+01, &
        2.9250000000000000E+01, &
        -1.7400000000000002E+01, &
        4.5111111111111111E+00]
    double precision, dimension(1:8), parameter, public :: &
        fdcoef_2b60 = [ &
        -6.9999999999999996E-01, &
        5.6611111111111105E+00, &
        -2.0100000000000001E+01, &
        4.1000000000000000E+01, &
        -5.2722222222222221E+01, &
        4.3950000000000003E+01, &
        -2.2300000000000001E+01, &
        5.2111111111111104E+00]
    double precision, dimension(1:9), parameter, public :: &
        fdcoef_2b70 = [ &
        6.4821428571428563E-01, &
        -5.8857142857142852E+00, &
        2.3811111111111106E+01, &
        -5.6399999999999999E+01, &
        8.6375000000000000E+01, &
        -8.9022222222222226E+01, &
        6.2099999999999994E+01, &
        -2.7485714285714280E+01, &
        5.8593253968253958E+00]
    double precision, dimension(1:10), parameter, public :: &
        fdcoef_2b80 = [ &
        -6.0396825396825404E-01, &
        6.0839285714285722E+00, &
        -2.7628571428571430E+01, &
        7.4544444444444451E+01, &
        -1.3250000000000000E+02, &
        1.6247499999999999E+02, &
        -1.3975555555555556E+02, &
        8.3842857142857127E+01, &
        -3.2921428571428564E+01, &
        6.4632936507936494E+00]
    ! Staggered-grid center FD, 2nd order
    double precision, dimension(1:2), parameter, public :: &
        fdcoef_2s10 = [ &
        0.0000000000000000E+00, &
        0.0000000000000000E+00]
    double precision, dimension(1:4), parameter, public :: &
        fdcoef_2s20 = [ &
        5.0000000000000000E-01, &
        -5.0000000000000000E-01, &
        -5.0000000000000000E-01, &
        5.0000000000000000E-01]
    double precision, dimension(1:6), parameter, public :: &
        fdcoef_2s30 = [ &
        -1.0416666666666667E-01, &
        8.1249999999999989E-01, &
        -7.0833333333333337E-01, &
        -7.0833333333333315E-01, &
        8.1249999999999978E-01, &
        -1.0416666666666666E-01]
    double precision, dimension(1:8), parameter, public :: &
        fdcoef_2s40 = [ &
        2.2482638888888889E-02, &
        -2.1657986111111108E-01, &
        1.0148437500000003E+00, &
        -8.2074652777777724E-01, &
        -8.2074652777777768E-01, &
        1.0148437500000000E+00, &
        -2.1657986111111110E-01, &
        2.2482638888888885E-02]
    double precision, dimension(1:10), parameter, public :: &
        fdcoef_2s50 = [ &
        -5.0052703373015886E-03, &
        5.7519531250000006E-02, &
        -3.1668526785714285E-01, &
        1.1549913194444443E+00, &
        -8.9082031250000004E-01, &
        -8.9082031250000038E-01, &
        1.1549913194444446E+00, &
        -3.1668526785714285E-01, &
        5.7519531250000006E-02, &
        -5.0052703373015877E-03]
    double precision, dimension(1:12), parameter, public :: &
        fdcoef_2s60 = [ &
        1.1380537729414682E-03, &
        -1.5247754293774795E-02, &
        9.7351413302951384E-02, &
        -4.0203930082775285E-01, &
        1.2574161590091766E+00, &
        -9.3861857096354184E-01, &
        -9.3861857096354129E-01, &
        1.2574161590091761E+00, &
        -4.0203930082775280E-01, &
        9.7351413302951342E-02, &
        -1.5247754293774795E-02, &
        1.1380537729414678E-03]
    double precision, dimension(1:14), parameter, public :: &
        fdcoef_2s70 = [ &
        -2.6262464060010436E-04, &
        4.0269248195426172E-03, &
        -2.9429484886180437E-02, &
        1.3779560795536747E-01, &
        -4.7426107699278158E-01, &
        1.3354156772674073E+00, &
        -9.7328502352275470E-01, &
        -9.7328502352275603E-01, &
        1.3354156772674082E+00, &
        -4.7426107699278164E-01, &
        1.3779560795536747E-01, &
        -2.9429484886180447E-02, &
        4.0269248195426190E-03, &
        -2.6262464060010458E-04]
    double precision, dimension(1:16), parameter, public :: &
        fdcoef_2s80 = [ &
        6.1269042621576244E-05, &
        -1.0591221946805954E-03, &
        8.7446411014039868E-03, &
        -4.6155933521870758E-02, &
        1.7682398810531150E-01, &
        -5.3559138865697942E-01, &
        1.3967459889316052E+00, &
        -9.9956944280741133E-01, &
        -9.9956944280741311E-01, &
        1.3967459889316054E+00, &
        -5.3559138865697942E-01, &
        1.7682398810531155E-01, &
        -4.6155933521870751E-02, &
        8.7446411014039851E-03, &
        -1.0591221946805957E-03, &
        6.1269042621576230E-05]
contains
    !
    !> Compute weights for Taylor-expansion-based finite difference
    !
    function taylor_fd_coefs(m, x0, x) result(w)
        ! m is the order of the difference
        integer, intent(in) :: m
        ! x0 is the location of the difference to be determined
        real, intent(in) :: x0
        ! x is the locations of the difference coefficients to be determined
        real, dimension(:), intent(in) :: x
        ! w is the array of coefficients
        double precision, allocatable, dimension(:) :: w
        integer :: n, nu, mi, ni
        double precision, allocatable, dimension(:) :: a
        double precision, allocatable, dimension(:, :, :) :: delta
        double precision :: c1, c2, c3
        n = size(x) - 1
        call alloc_array(a, [0, n], source=dble(x(:)))
        call alloc_array(delta, [0, m, 0, n, 0, n])
        delta(0, 0, 0) = 1.0d0
        c1 = 1.0d0
        do ni = 1, n
            c2 = 1.0d0
            do nu = 0, ni - 1
                c3 = a(ni) - a(nu)
                c2 = c2*c3
                do mi = 0, min(ni, m)
                    delta(mi, ni, nu) = ((a(ni) - x0)*delta(mi, ni - 1, nu) &
                        - mi*delta(mi - 1, ni - 1, nu))/c3
                end do
            end do
            do mi = 0, min(ni, m)
                delta(mi, ni, ni) = c1/c2*(mi*delta(mi - 1, ni - 1, ni - 1) &
                    - (a(ni - 1) - x0)*delta(mi, ni - 1, ni - 1))
            end do
            c1 = c2
        end do
        call alloc_array(w, [1, n + 1], source=delta(m, n, :))
    end function taylor_fd_coefs
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
!> Differentiate 1D array using finite difference
!
function differentiate_1d_float(w, order, method, accuracy) result(wt)
    real, dimension(:), intent(in) :: w
    integer, intent(in), optional :: order, accuracy
    character(len=*), intent(in), optional :: method
    real, allocatable, dimension(:) :: wt
    integer :: i, n
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n = size(w)
    allocate (wt(1:n))
    select case (deriv_order)
        case (1)
            select case (deriv_method)
                case ('forward')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n - 1
                                wt(i) = sum(fdcoef_1f10*w(i:i + 1))
                            end do
                            wt(n) = w(n) - w(n - 1)
                        case (2)
                            do i = 1, n - 2
                                wt(i) = sum(fdcoef_1f20*w(i:i + 2))
                            end do
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (3)
                            do i = 1, n - 3
                                wt(i) = sum(fdcoef_1f30*w(i:i + 3))
                            end do
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (4)
                            do i = 1, n - 4
                                wt(i) = sum(fdcoef_1f40*w(i:i + 4))
                            end do
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (5)
                            do i = 1, n - 5
                                wt(i) = sum(fdcoef_1f50*w(i:i + 5))
                            end do
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (6)
                            do i = 1, n - 6
                                wt(i) = sum(fdcoef_1f60*w(i:i + 6))
                            end do
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (7)
                            do i = 1, n - 7
                                wt(i) = sum(fdcoef_1f70*w(i:i + 7))
                            end do
                            wt(n - 6) = sum(fdcoef_1f60*w(n - 6:n))
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (8)
                            do i = 1, n - 8
                                wt(i) = sum(fdcoef_1f80*w(i:i + 8))
                            end do
                            wt(n - 7) = sum(fdcoef_1f70*w(n - 7:n))
                            wt(n - 6) = sum(fdcoef_1f60*w(n - 6:n))
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                    end select
                case ('center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n - 1
                                wt(i) = sum(fdcoef_1c10(1:3)*w(i - 1:i + 1))
                            end do
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (2)
                            do i = 3, n - 2
                                wt(i) = sum(fdcoef_1c20(1:5)*w(i - 2:i + 2))
                            end do
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (3)
                            do i = 4, n - 3
                                wt(i) = sum(fdcoef_1c30(1:7)*w(i - 3:i + 3))
                            end do
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (4)
                            do i = 5, n - 4
                                wt(i) = sum(fdcoef_1c40(1:9)*w(i - 4:i + 4))
                            end do
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (5)
                            do i = 6, n - 5
                                wt(i) = sum(fdcoef_1c50(1:11)*w(i - 5:i + 5))
                            end do
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (6)
                            do i = 7, n - 6
                                wt(i) = sum(fdcoef_1c60(1:13)*w(i - 6:i + 6))
                            end do
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (7)
                            do i = 8, n - 7
                                wt(i) = sum(fdcoef_1c70(1:15)*w(i - 7:i + 7))
                            end do
                            wt(7) = sum(fdcoef_1c60(1:13)*w(1:13))
                            wt(n - 6) = sum(-fdcoef_1c60(13:1:-1)*w(n - 12:n))
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (8)
                            do i = 9, n - 8
                                wt(i) = sum(fdcoef_1c80(1:17)*w(i - 8:i + 8))
                            end do
                            wt(8) = sum(fdcoef_1c70(1:15)*w(1:15))
                            wt(n - 7) = sum(-fdcoef_1c70(15:1:-1)*w(n - 14:n))
                            wt(7) = sum(fdcoef_1c60(1:13)*w(1:13))
                            wt(n - 6) = sum(-fdcoef_1c60(13:1:-1)*w(n - 12:n))
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                    end select
                case ('backward')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n
                                wt(i) = sum(fdcoef_1b10*w(i - 1:i))
                            end do
                            wt(1) = w(2) - w(1)
                        case (2)
                            do i = 3, n
                                wt(i) = sum(fdcoef_1b20*w(i - 2:i))
                            end do
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (3)
                            do i = 4, n
                                wt(i) = sum(fdcoef_1b30*w(i - 3:i))
                            end do
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (4)
                            do i = 5, n
                                wt(i) = sum(fdcoef_1b40*w(i - 4:i))
                            end do
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (5)
                            do i = 6, n
                                wt(i) = sum(fdcoef_1b50*w(i - 5:i))
                            end do
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (6)
                            do i = 7, n
                                wt(i) = sum(fdcoef_1b60*w(i - 6:i))
                            end do
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (7)
                            do i = 8, n
                                wt(i) = sum(fdcoef_1b70*w(i - 7:i))
                            end do
                            wt(7) = sum(fdcoef_1b60*w(1:7))
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (8)
                            do i = 9, n
                                wt(i) = sum(fdcoef_1b80*w(i - 8:i))
                            end do
                            wt(8) = sum(fdcoef_1b70*w(1:8))
                            wt(7) = sum(fdcoef_1b60*w(1:7))
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                    end select
                case ('staggered-center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n - 1
                                wt(i) = sum(fdcoef_1s10*w(i:i + 1))
                            end do
                        case (2)
                            do i = 2, n - 2
                                wt(i) = sum(fdcoef_1s20*w(i - 1:i + 2))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                        case (3)
                            do i = 3, n - 3
                                wt(i) = sum(fdcoef_1s30*w(i - 2:i + 3))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                            wt(2) = sum(fdcoef_1s20*w(1:4))
                            wt(n - 2) = sum(fdcoef_1s20*w(n - 3:n))
                        case (4)
                            do i = 4, n - 4
                                wt(i) = sum(fdcoef_1s40*w(i - 3:i + 4))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                            wt(2) = sum(fdcoef_1s20*w(1:4))
                            wt(n - 2) = sum(fdcoef_1s20*w(n - 3:n))
                            wt(3) = sum(fdcoef_1s30*w(1:6))
                            wt(n - 3) = sum(fdcoef_1s30*w(n - 5:n))
                    end select
            end select
        case (2)
            select case (deriv_method)
                case ('forward')
                    do i = 1, n - 2
                        wt(i) = w(i + 2) - 2*w(i + 1) + w(i)
                    end do
                    wt(n - 1) = wt(n - 2) + (wt(n - 2) - wt(n - 3))
                    wt(n) = wt(n - 1) + (wt(n - 1) - wt(n - 2))
                case ('center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n - 1
                                wt(i) = sum(fdcoef_2c10*w(i - 1:i + 1))
                            end do
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (2)
                            do i = 3, n - 2
                                wt(i) = sum(fdcoef_2c20*w(i - 2:i + 2))
                            end do
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (3)
                            do i = 4, n - 3
                                wt(i) = sum(fdcoef_2c30*w(i - 3:i + 3))
                            end do
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (4)
                            do i = 5, n - 4
                                wt(i) = sum(fdcoef_2c40*w(i - 4:i + 4))
                            end do
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (5)
                            do i = 6, n - 5
                                wt(i) = sum(fdcoef_2c50*w(i - 5:i + 5))
                            end do
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (6)
                            do i = 7, n - 6
                                wt(i) = sum(fdcoef_2c60*w(i - 6:i + 6))
                            end do
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (7)
                            do i = 8, n - 7
                                wt(i) = sum(fdcoef_2c70*w(i - 7:i + 7))
                            end do
                            wt(7) = sum(fdcoef_2c60*w(1:13))
                            wt(n - 6) = sum(fdcoef_2c60*w(n - 12:n))
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (8)
                            do i = 9, n - 8
                                wt(i) = sum(fdcoef_2c80*w(i - 8:i + 8))
                            end do
                            wt(8) = sum(fdcoef_2c70*w(1:15))
                            wt(n - 7) = sum(fdcoef_2c70*w(n - 14:n))
                            wt(7) = sum(fdcoef_2c60*w(1:13))
                            wt(n - 6) = sum(fdcoef_2c60*w(n - 12:n))
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                    end select
                case ('backward')
                    do i = 3, n
                        wt(i) = w(i - 2) - 2*w(i - 1) + w(i)
                    end do
                    wt(1) = wt(2) - (wt(3) - wt(2))
                    wt(2) = wt(3) - (wt(4) - wt(3))
                case ('staggered-center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n
                            end do
                    end select
            end select
    end select
end function differentiate_1d_float
!
!> Differentiate 2D array using finite difference
!
function differentiate_2d_float(w, dim, order, method, accuracy) result(wt)
    real, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: dim, order, accuracy
    character(len=*), intent(in), optional :: method
    real, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    wt = w
    select case (along)
        case (1)
            !$omp parallel do private(j)
            do j = 1, n2
                wt(:, j) = differentiate_1d_float(wt(:, j), deriv_order, deriv_method, deriv_accuracy)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i, :) = differentiate_1d_float(wt(i, :), deriv_order, deriv_method, deriv_accuracy)
            end do
            !$omp end parallel do
    end select
end function differentiate_2d_float
!
!> Differentiate 3D array using finite difference
!
function differentiate_3d_float(w, dim, order, method, accuracy) result(wt)
    real, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: dim, order, accuracy
    character(len=*), intent(in), optional :: method
    real, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    wt = w
    select case (along)
        case (1)
            !$omp parallel do private(j, k)
            do k = 1, n3
                do j = 1, n2
                    wt(:, j, k) = differentiate_1d_float(wt(:, j, k), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, k)
            do k = 1, n3
                do i = 1, n1
                    wt(i, :, k) = differentiate_1d_float(wt(i, :, k), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j, :) = differentiate_1d_float(wt(i, j, :), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
    end select
end function differentiate_3d_float
!
!> Integrate 1D array using cumulative trapezoidal rule
!
function integrate_1d_float(w) result(wt)
    real, dimension(:) :: w
    real, allocatable, dimension(:) :: wt
    integer :: i, n
    n = size(w)
    allocate (wt(1:n))
    wt(1) = 0
    do i = 2, n
        wt(i) = wt(i - 1) + 0.5d0*(w(i - 1) + w(i))
    end do
end function integrate_1d_float
!
!> Integrate 2D array using cumulative trapezoidal rule
!
function integrate_2d_float(w, dim) result(wt)
    real, dimension(:, :) :: w
    integer, optional :: dim
    real, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate (wt(1:n1, 1:n2))
    select case (along)
        case (1)
            !$omp parallel do private(j)
            do j = 1, n2
                wt(:, j) = integrate_1d_float(w(:, j))
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i, :) = integrate_1d_float(w(i, :))
            end do
            !$omp end parallel do
    end select
end function integrate_2d_float
!
!> Integrate 3D array using cumulative trapezoidal rule
!
function integrate_3d_float(w, dim) result(wt)
    real, dimension(:, :, :) :: w
    integer, optional :: dim
    real, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate (wt(1:n1, 1:n2, 1:n3))
    select case (along)
        case (1)
            !$omp parallel do private(j, k)
            do k = 1, n3
                do j = 1, n2
                    wt(:, j, k) = integrate_1d_float(w(:, j, k))
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, k)
            do k = 1, n3
                do i = 1, n1
                    wt(i, :, k) = integrate_1d_float(w(i, :, k))
                end do
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j, :) = integrate_1d_float(w(i, j, :))
                end do
            end do
            !$omp end parallel do
    end select
end function integrate_3d_float
!
!> Integrate 1D array using cumulative summation
!
function cumsum_1d_float(w) result(wt)
    real, dimension(:) :: w
    real, allocatable, dimension(:) :: wt
    integer :: i, n
    n = size(w)
    allocate (wt(1:n))
    wt(1) = w(1)
    do i = 2, n
        wt(i) = wt(i - 1) + w(i)
    end do
end function cumsum_1d_float
!
!> Integrate 2D array using cumulative summation
!
function cumsum_2d_float(w, dim) result(wt)
    real, dimension(:, :) :: w
    integer, optional :: dim
    real, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate (wt(1:n1, 1:n2))
    select case (along)
        case (1)
            wt(1, :) = w(1, :)
            !$omp parallel do private(i)
            do i = 2, n1
                wt(i, :) = wt(i - 1, :) + w(i, :)
            end do
            !$omp end parallel do
        case (2)
            wt(:, 1) = w(:, 1)
            !$omp parallel do private(j)
            do j = 2, n2
                wt(:, j) = wt(:, j - 1) + w(:, j)
            end do
            !$omp end parallel do
    end select
end function cumsum_2d_float
!
!> Integrate 3D array using cumulative summation
!
function cumsum_3d_float(w, dim) result(wt)
    real, dimension(:, :, :) :: w
    integer, optional :: dim
    real, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate (wt(1:n1, 1:n2, 1:n3))
    select case (along)
        case (1)
            wt(1, :, :) = w(1, :, :)
            !$omp parallel do private(i)
            do i = 2, n1
                wt(i, :, :) = wt(i - 1, :, :) + w(i, :, :)
            end do
            !$omp end parallel do
        case (2)
            wt(:, 1, :) = w(:, 1, :)
            !$omp parallel do private(j)
            do j = 2, n2
                wt(:, j, :) = wt(:, j - 1, :) + w(:, j, :)
            end do
            !$omp end parallel do
        case (3)
            wt(:, :, 1) = w(:, :, 1)
            !$omp parallel do private(k)
            do k = 2, n3
                wt(:, :, k) = wt(:, :, k - 1) + w(:, :, k)
            end do
            !$omp end parallel do
    end select
end function cumsum_3d_float
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
!> Differentiate 1D array using finite difference
!
function differentiate_1d_double(w, order, method, accuracy) result(wt)
    double precision, dimension(:), intent(in) :: w
    integer, intent(in), optional :: order, accuracy
    character(len=*), intent(in), optional :: method
    double precision, allocatable, dimension(:) :: wt
    integer :: i, n
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n = size(w)
    allocate (wt(1:n))
    select case (deriv_order)
        case (1)
            select case (deriv_method)
                case ('forward')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n - 1
                                wt(i) = sum(fdcoef_1f10*w(i:i + 1))
                            end do
                            wt(n) = w(n) - w(n - 1)
                        case (2)
                            do i = 1, n - 2
                                wt(i) = sum(fdcoef_1f20*w(i:i + 2))
                            end do
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (3)
                            do i = 1, n - 3
                                wt(i) = sum(fdcoef_1f30*w(i:i + 3))
                            end do
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (4)
                            do i = 1, n - 4
                                wt(i) = sum(fdcoef_1f40*w(i:i + 4))
                            end do
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (5)
                            do i = 1, n - 5
                                wt(i) = sum(fdcoef_1f50*w(i:i + 5))
                            end do
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (6)
                            do i = 1, n - 6
                                wt(i) = sum(fdcoef_1f60*w(i:i + 6))
                            end do
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (7)
                            do i = 1, n - 7
                                wt(i) = sum(fdcoef_1f70*w(i:i + 7))
                            end do
                            wt(n - 6) = sum(fdcoef_1f60*w(n - 6:n))
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (8)
                            do i = 1, n - 8
                                wt(i) = sum(fdcoef_1f80*w(i:i + 8))
                            end do
                            wt(n - 7) = sum(fdcoef_1f70*w(n - 7:n))
                            wt(n - 6) = sum(fdcoef_1f60*w(n - 6:n))
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                    end select
                case ('center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n - 1
                                wt(i) = sum(fdcoef_1c10(1:3)*w(i - 1:i + 1))
                            end do
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (2)
                            do i = 3, n - 2
                                wt(i) = sum(fdcoef_1c20(1:5)*w(i - 2:i + 2))
                            end do
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (3)
                            do i = 4, n - 3
                                wt(i) = sum(fdcoef_1c30(1:7)*w(i - 3:i + 3))
                            end do
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (4)
                            do i = 5, n - 4
                                wt(i) = sum(fdcoef_1c40(1:9)*w(i - 4:i + 4))
                            end do
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (5)
                            do i = 6, n - 5
                                wt(i) = sum(fdcoef_1c50(1:11)*w(i - 5:i + 5))
                            end do
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (6)
                            do i = 7, n - 6
                                wt(i) = sum(fdcoef_1c60(1:13)*w(i - 6:i + 6))
                            end do
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (7)
                            do i = 8, n - 7
                                wt(i) = sum(fdcoef_1c70(1:15)*w(i - 7:i + 7))
                            end do
                            wt(7) = sum(fdcoef_1c60(1:13)*w(1:13))
                            wt(n - 6) = sum(-fdcoef_1c60(13:1:-1)*w(n - 12:n))
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (8)
                            do i = 9, n - 8
                                wt(i) = sum(fdcoef_1c80(1:17)*w(i - 8:i + 8))
                            end do
                            wt(8) = sum(fdcoef_1c70(1:15)*w(1:15))
                            wt(n - 7) = sum(-fdcoef_1c70(15:1:-1)*w(n - 14:n))
                            wt(7) = sum(fdcoef_1c60(1:13)*w(1:13))
                            wt(n - 6) = sum(-fdcoef_1c60(13:1:-1)*w(n - 12:n))
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                    end select
                case ('backward')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n
                                wt(i) = sum(fdcoef_1b10*w(i - 1:i))
                            end do
                            wt(1) = w(2) - w(1)
                        case (2)
                            do i = 3, n
                                wt(i) = sum(fdcoef_1b20*w(i - 2:i))
                            end do
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (3)
                            do i = 4, n
                                wt(i) = sum(fdcoef_1b30*w(i - 3:i))
                            end do
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (4)
                            do i = 5, n
                                wt(i) = sum(fdcoef_1b40*w(i - 4:i))
                            end do
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (5)
                            do i = 6, n
                                wt(i) = sum(fdcoef_1b50*w(i - 5:i))
                            end do
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (6)
                            do i = 7, n
                                wt(i) = sum(fdcoef_1b60*w(i - 6:i))
                            end do
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (7)
                            do i = 8, n
                                wt(i) = sum(fdcoef_1b70*w(i - 7:i))
                            end do
                            wt(7) = sum(fdcoef_1b60*w(1:7))
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (8)
                            do i = 9, n
                                wt(i) = sum(fdcoef_1b80*w(i - 8:i))
                            end do
                            wt(8) = sum(fdcoef_1b70*w(1:8))
                            wt(7) = sum(fdcoef_1b60*w(1:7))
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                    end select
                case ('staggered-center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n - 1
                                wt(i) = sum(fdcoef_1s10*w(i:i + 1))
                            end do
                        case (2)
                            do i = 2, n - 2
                                wt(i) = sum(fdcoef_1s20*w(i - 1:i + 2))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                        case (3)
                            do i = 3, n - 3
                                wt(i) = sum(fdcoef_1s30*w(i - 2:i + 3))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                            wt(2) = sum(fdcoef_1s20*w(1:4))
                            wt(n - 2) = sum(fdcoef_1s20*w(n - 3:n))
                        case (4)
                            do i = 4, n - 4
                                wt(i) = sum(fdcoef_1s40*w(i - 3:i + 4))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                            wt(2) = sum(fdcoef_1s20*w(1:4))
                            wt(n - 2) = sum(fdcoef_1s20*w(n - 3:n))
                            wt(3) = sum(fdcoef_1s30*w(1:6))
                            wt(n - 3) = sum(fdcoef_1s30*w(n - 5:n))
                    end select
            end select
        case (2)
            select case (deriv_method)
                case ('forward')
                    do i = 1, n - 2
                        wt(i) = w(i + 2) - 2*w(i + 1) + w(i)
                    end do
                    wt(n - 1) = wt(n - 2) + (wt(n - 2) - wt(n - 3))
                    wt(n) = wt(n - 1) + (wt(n - 1) - wt(n - 2))
                case ('center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n - 1
                                wt(i) = sum(fdcoef_2c10*w(i - 1:i + 1))
                            end do
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (2)
                            do i = 3, n - 2
                                wt(i) = sum(fdcoef_2c20*w(i - 2:i + 2))
                            end do
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (3)
                            do i = 4, n - 3
                                wt(i) = sum(fdcoef_2c30*w(i - 3:i + 3))
                            end do
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (4)
                            do i = 5, n - 4
                                wt(i) = sum(fdcoef_2c40*w(i - 4:i + 4))
                            end do
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (5)
                            do i = 6, n - 5
                                wt(i) = sum(fdcoef_2c50*w(i - 5:i + 5))
                            end do
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (6)
                            do i = 7, n - 6
                                wt(i) = sum(fdcoef_2c60*w(i - 6:i + 6))
                            end do
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (7)
                            do i = 8, n - 7
                                wt(i) = sum(fdcoef_2c70*w(i - 7:i + 7))
                            end do
                            wt(7) = sum(fdcoef_2c60*w(1:13))
                            wt(n - 6) = sum(fdcoef_2c60*w(n - 12:n))
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (8)
                            do i = 9, n - 8
                                wt(i) = sum(fdcoef_2c80*w(i - 8:i + 8))
                            end do
                            wt(8) = sum(fdcoef_2c70*w(1:15))
                            wt(n - 7) = sum(fdcoef_2c70*w(n - 14:n))
                            wt(7) = sum(fdcoef_2c60*w(1:13))
                            wt(n - 6) = sum(fdcoef_2c60*w(n - 12:n))
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                    end select
                case ('backward')
                    do i = 3, n
                        wt(i) = w(i - 2) - 2*w(i - 1) + w(i)
                    end do
                    wt(1) = wt(2) - (wt(3) - wt(2))
                    wt(2) = wt(3) - (wt(4) - wt(3))
                case ('staggered-center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n
                            end do
                    end select
            end select
    end select
end function differentiate_1d_double
!
!> Differentiate 2D array using finite difference
!
function differentiate_2d_double(w, dim, order, method, accuracy) result(wt)
    double precision, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: dim, order, accuracy
    character(len=*), intent(in), optional :: method
    double precision, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    wt = w
    select case (along)
        case (1)
            !$omp parallel do private(j)
            do j = 1, n2
                wt(:, j) = differentiate_1d_double(wt(:, j), deriv_order, deriv_method, deriv_accuracy)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i, :) = differentiate_1d_double(wt(i, :), deriv_order, deriv_method, deriv_accuracy)
            end do
            !$omp end parallel do
    end select
end function differentiate_2d_double
!
!> Differentiate 3D array using finite difference
!
function differentiate_3d_double(w, dim, order, method, accuracy) result(wt)
    double precision, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: dim, order, accuracy
    character(len=*), intent(in), optional :: method
    double precision, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    wt = w
    select case (along)
        case (1)
            !$omp parallel do private(j, k)
            do k = 1, n3
                do j = 1, n2
                    wt(:, j, k) = differentiate_1d_double(wt(:, j, k), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, k)
            do k = 1, n3
                do i = 1, n1
                    wt(i, :, k) = differentiate_1d_double(wt(i, :, k), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j, :) = differentiate_1d_double(wt(i, j, :), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
    end select
end function differentiate_3d_double
!
!> Integrate 1D array using cumulative trapezoidal rule
!
function integrate_1d_double(w) result(wt)
    double precision, dimension(:) :: w
    double precision, allocatable, dimension(:) :: wt
    integer :: i, n
    n = size(w)
    allocate (wt(1:n))
    wt(1) = 0
    do i = 2, n
        wt(i) = wt(i - 1) + 0.5d0*(w(i - 1) + w(i))
    end do
end function integrate_1d_double
!
!> Integrate 2D array using cumulative trapezoidal rule
!
function integrate_2d_double(w, dim) result(wt)
    double precision, dimension(:, :) :: w
    integer, optional :: dim
    double precision, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate (wt(1:n1, 1:n2))
    select case (along)
        case (1)
            !$omp parallel do private(j)
            do j = 1, n2
                wt(:, j) = integrate_1d_double(w(:, j))
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i, :) = integrate_1d_double(w(i, :))
            end do
            !$omp end parallel do
    end select
end function integrate_2d_double
!
!> Integrate 3D array using cumulative trapezoidal rule
!
function integrate_3d_double(w, dim) result(wt)
    double precision, dimension(:, :, :) :: w
    integer, optional :: dim
    double precision, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate (wt(1:n1, 1:n2, 1:n3))
    select case (along)
        case (1)
            !$omp parallel do private(j, k)
            do k = 1, n3
                do j = 1, n2
                    wt(:, j, k) = integrate_1d_double(w(:, j, k))
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, k)
            do k = 1, n3
                do i = 1, n1
                    wt(i, :, k) = integrate_1d_double(w(i, :, k))
                end do
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j, :) = integrate_1d_double(w(i, j, :))
                end do
            end do
            !$omp end parallel do
    end select
end function integrate_3d_double
!
!> Integrate 1D array using cumulative summation
!
function cumsum_1d_double(w) result(wt)
    double precision, dimension(:) :: w
    double precision, allocatable, dimension(:) :: wt
    integer :: i, n
    n = size(w)
    allocate (wt(1:n))
    wt(1) = w(1)
    do i = 2, n
        wt(i) = wt(i - 1) + w(i)
    end do
end function cumsum_1d_double
!
!> Integrate 2D array using cumulative summation
!
function cumsum_2d_double(w, dim) result(wt)
    double precision, dimension(:, :) :: w
    integer, optional :: dim
    double precision, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate (wt(1:n1, 1:n2))
    select case (along)
        case (1)
            wt(1, :) = w(1, :)
            !$omp parallel do private(i)
            do i = 2, n1
                wt(i, :) = wt(i - 1, :) + w(i, :)
            end do
            !$omp end parallel do
        case (2)
            wt(:, 1) = w(:, 1)
            !$omp parallel do private(j)
            do j = 2, n2
                wt(:, j) = wt(:, j - 1) + w(:, j)
            end do
            !$omp end parallel do
    end select
end function cumsum_2d_double
!
!> Integrate 3D array using cumulative summation
!
function cumsum_3d_double(w, dim) result(wt)
    double precision, dimension(:, :, :) :: w
    integer, optional :: dim
    double precision, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate (wt(1:n1, 1:n2, 1:n3))
    select case (along)
        case (1)
            wt(1, :, :) = w(1, :, :)
            !$omp parallel do private(i)
            do i = 2, n1
                wt(i, :, :) = wt(i - 1, :, :) + w(i, :, :)
            end do
            !$omp end parallel do
        case (2)
            wt(:, 1, :) = w(:, 1, :)
            !$omp parallel do private(j)
            do j = 2, n2
                wt(:, j, :) = wt(:, j - 1, :) + w(:, j, :)
            end do
            !$omp end parallel do
        case (3)
            wt(:, :, 1) = w(:, :, 1)
            !$omp parallel do private(k)
            do k = 2, n3
                wt(:, :, k) = wt(:, :, k - 1) + w(:, :, k)
            end do
            !$omp end parallel do
    end select
end function cumsum_3d_double
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
!> Differentiate 1D array using finite difference
!
function differentiate_1d_complex(w, order, method, accuracy) result(wt)
    complex, dimension(:), intent(in) :: w
    integer, intent(in), optional :: order, accuracy
    character(len=*), intent(in), optional :: method
    complex, allocatable, dimension(:) :: wt
    integer :: i, n
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n = size(w)
    allocate (wt(1:n))
    select case (deriv_order)
        case (1)
            select case (deriv_method)
                case ('forward')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n - 1
                                wt(i) = sum(fdcoef_1f10*w(i:i + 1))
                            end do
                            wt(n) = w(n) - w(n - 1)
                        case (2)
                            do i = 1, n - 2
                                wt(i) = sum(fdcoef_1f20*w(i:i + 2))
                            end do
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (3)
                            do i = 1, n - 3
                                wt(i) = sum(fdcoef_1f30*w(i:i + 3))
                            end do
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (4)
                            do i = 1, n - 4
                                wt(i) = sum(fdcoef_1f40*w(i:i + 4))
                            end do
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (5)
                            do i = 1, n - 5
                                wt(i) = sum(fdcoef_1f50*w(i:i + 5))
                            end do
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (6)
                            do i = 1, n - 6
                                wt(i) = sum(fdcoef_1f60*w(i:i + 6))
                            end do
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (7)
                            do i = 1, n - 7
                                wt(i) = sum(fdcoef_1f70*w(i:i + 7))
                            end do
                            wt(n - 6) = sum(fdcoef_1f60*w(n - 6:n))
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (8)
                            do i = 1, n - 8
                                wt(i) = sum(fdcoef_1f80*w(i:i + 8))
                            end do
                            wt(n - 7) = sum(fdcoef_1f70*w(n - 7:n))
                            wt(n - 6) = sum(fdcoef_1f60*w(n - 6:n))
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                    end select
                case ('center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n - 1
                                wt(i) = sum(fdcoef_1c10(1:3)*w(i - 1:i + 1))
                            end do
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (2)
                            do i = 3, n - 2
                                wt(i) = sum(fdcoef_1c20(1:5)*w(i - 2:i + 2))
                            end do
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (3)
                            do i = 4, n - 3
                                wt(i) = sum(fdcoef_1c30(1:7)*w(i - 3:i + 3))
                            end do
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (4)
                            do i = 5, n - 4
                                wt(i) = sum(fdcoef_1c40(1:9)*w(i - 4:i + 4))
                            end do
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (5)
                            do i = 6, n - 5
                                wt(i) = sum(fdcoef_1c50(1:11)*w(i - 5:i + 5))
                            end do
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (6)
                            do i = 7, n - 6
                                wt(i) = sum(fdcoef_1c60(1:13)*w(i - 6:i + 6))
                            end do
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (7)
                            do i = 8, n - 7
                                wt(i) = sum(fdcoef_1c70(1:15)*w(i - 7:i + 7))
                            end do
                            wt(7) = sum(fdcoef_1c60(1:13)*w(1:13))
                            wt(n - 6) = sum(-fdcoef_1c60(13:1:-1)*w(n - 12:n))
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (8)
                            do i = 9, n - 8
                                wt(i) = sum(fdcoef_1c80(1:17)*w(i - 8:i + 8))
                            end do
                            wt(8) = sum(fdcoef_1c70(1:15)*w(1:15))
                            wt(n - 7) = sum(-fdcoef_1c70(15:1:-1)*w(n - 14:n))
                            wt(7) = sum(fdcoef_1c60(1:13)*w(1:13))
                            wt(n - 6) = sum(-fdcoef_1c60(13:1:-1)*w(n - 12:n))
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                    end select
                case ('backward')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n
                                wt(i) = sum(fdcoef_1b10*w(i - 1:i))
                            end do
                            wt(1) = w(2) - w(1)
                        case (2)
                            do i = 3, n
                                wt(i) = sum(fdcoef_1b20*w(i - 2:i))
                            end do
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (3)
                            do i = 4, n
                                wt(i) = sum(fdcoef_1b30*w(i - 3:i))
                            end do
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (4)
                            do i = 5, n
                                wt(i) = sum(fdcoef_1b40*w(i - 4:i))
                            end do
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (5)
                            do i = 6, n
                                wt(i) = sum(fdcoef_1b50*w(i - 5:i))
                            end do
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (6)
                            do i = 7, n
                                wt(i) = sum(fdcoef_1b60*w(i - 6:i))
                            end do
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (7)
                            do i = 8, n
                                wt(i) = sum(fdcoef_1b70*w(i - 7:i))
                            end do
                            wt(7) = sum(fdcoef_1b60*w(1:7))
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (8)
                            do i = 9, n
                                wt(i) = sum(fdcoef_1b80*w(i - 8:i))
                            end do
                            wt(8) = sum(fdcoef_1b70*w(1:8))
                            wt(7) = sum(fdcoef_1b60*w(1:7))
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                    end select
                case ('staggered-center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n - 1
                                wt(i) = sum(fdcoef_1s10*w(i:i + 1))
                            end do
                        case (2)
                            do i = 2, n - 2
                                wt(i) = sum(fdcoef_1s20*w(i - 1:i + 2))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                        case (3)
                            do i = 3, n - 3
                                wt(i) = sum(fdcoef_1s30*w(i - 2:i + 3))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                            wt(2) = sum(fdcoef_1s20*w(1:4))
                            wt(n - 2) = sum(fdcoef_1s20*w(n - 3:n))
                        case (4)
                            do i = 4, n - 4
                                wt(i) = sum(fdcoef_1s40*w(i - 3:i + 4))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                            wt(2) = sum(fdcoef_1s20*w(1:4))
                            wt(n - 2) = sum(fdcoef_1s20*w(n - 3:n))
                            wt(3) = sum(fdcoef_1s30*w(1:6))
                            wt(n - 3) = sum(fdcoef_1s30*w(n - 5:n))
                    end select
            end select
        case (2)
            select case (deriv_method)
                case ('forward')
                    do i = 1, n - 2
                        wt(i) = w(i + 2) - 2*w(i + 1) + w(i)
                    end do
                    wt(n - 1) = wt(n - 2) + (wt(n - 2) - wt(n - 3))
                    wt(n) = wt(n - 1) + (wt(n - 1) - wt(n - 2))
                case ('center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n - 1
                                wt(i) = sum(fdcoef_2c10*w(i - 1:i + 1))
                            end do
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (2)
                            do i = 3, n - 2
                                wt(i) = sum(fdcoef_2c20*w(i - 2:i + 2))
                            end do
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (3)
                            do i = 4, n - 3
                                wt(i) = sum(fdcoef_2c30*w(i - 3:i + 3))
                            end do
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (4)
                            do i = 5, n - 4
                                wt(i) = sum(fdcoef_2c40*w(i - 4:i + 4))
                            end do
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (5)
                            do i = 6, n - 5
                                wt(i) = sum(fdcoef_2c50*w(i - 5:i + 5))
                            end do
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (6)
                            do i = 7, n - 6
                                wt(i) = sum(fdcoef_2c60*w(i - 6:i + 6))
                            end do
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (7)
                            do i = 8, n - 7
                                wt(i) = sum(fdcoef_2c70*w(i - 7:i + 7))
                            end do
                            wt(7) = sum(fdcoef_2c60*w(1:13))
                            wt(n - 6) = sum(fdcoef_2c60*w(n - 12:n))
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (8)
                            do i = 9, n - 8
                                wt(i) = sum(fdcoef_2c80*w(i - 8:i + 8))
                            end do
                            wt(8) = sum(fdcoef_2c70*w(1:15))
                            wt(n - 7) = sum(fdcoef_2c70*w(n - 14:n))
                            wt(7) = sum(fdcoef_2c60*w(1:13))
                            wt(n - 6) = sum(fdcoef_2c60*w(n - 12:n))
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                    end select
                case ('backward')
                    do i = 3, n
                        wt(i) = w(i - 2) - 2*w(i - 1) + w(i)
                    end do
                    wt(1) = wt(2) - (wt(3) - wt(2))
                    wt(2) = wt(3) - (wt(4) - wt(3))
                case ('staggered-center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n
                            end do
                    end select
            end select
    end select
end function differentiate_1d_complex
!
!> Differentiate 2D array using finite difference
!
function differentiate_2d_complex(w, dim, order, method, accuracy) result(wt)
    complex, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: dim, order, accuracy
    character(len=*), intent(in), optional :: method
    complex, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    wt = w
    select case (along)
        case (1)
            !$omp parallel do private(j)
            do j = 1, n2
                wt(:, j) = differentiate_1d_complex(wt(:, j), deriv_order, deriv_method, deriv_accuracy)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i, :) = differentiate_1d_complex(wt(i, :), deriv_order, deriv_method, deriv_accuracy)
            end do
            !$omp end parallel do
    end select
end function differentiate_2d_complex
!
!> Differentiate 3D array using finite difference
!
function differentiate_3d_complex(w, dim, order, method, accuracy) result(wt)
    complex, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: dim, order, accuracy
    character(len=*), intent(in), optional :: method
    complex, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    wt = w
    select case (along)
        case (1)
            !$omp parallel do private(j, k)
            do k = 1, n3
                do j = 1, n2
                    wt(:, j, k) = differentiate_1d_complex(wt(:, j, k), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, k)
            do k = 1, n3
                do i = 1, n1
                    wt(i, :, k) = differentiate_1d_complex(wt(i, :, k), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j, :) = differentiate_1d_complex(wt(i, j, :), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
    end select
end function differentiate_3d_complex
!
!> Integrate 1D array using cumulative trapezoidal rule
!
function integrate_1d_complex(w) result(wt)
    complex, dimension(:) :: w
    complex, allocatable, dimension(:) :: wt
    integer :: i, n
    n = size(w)
    allocate (wt(1:n))
    wt(1) = 0
    do i = 2, n
        wt(i) = wt(i - 1) + 0.5d0*(w(i - 1) + w(i))
    end do
end function integrate_1d_complex
!
!> Integrate 2D array using cumulative trapezoidal rule
!
function integrate_2d_complex(w, dim) result(wt)
    complex, dimension(:, :) :: w
    integer, optional :: dim
    complex, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate (wt(1:n1, 1:n2))
    select case (along)
        case (1)
            !$omp parallel do private(j)
            do j = 1, n2
                wt(:, j) = integrate_1d_complex(w(:, j))
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i, :) = integrate_1d_complex(w(i, :))
            end do
            !$omp end parallel do
    end select
end function integrate_2d_complex
!
!> Integrate 3D array using cumulative trapezoidal rule
!
function integrate_3d_complex(w, dim) result(wt)
    complex, dimension(:, :, :) :: w
    integer, optional :: dim
    complex, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate (wt(1:n1, 1:n2, 1:n3))
    select case (along)
        case (1)
            !$omp parallel do private(j, k)
            do k = 1, n3
                do j = 1, n2
                    wt(:, j, k) = integrate_1d_complex(w(:, j, k))
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, k)
            do k = 1, n3
                do i = 1, n1
                    wt(i, :, k) = integrate_1d_complex(w(i, :, k))
                end do
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j, :) = integrate_1d_complex(w(i, j, :))
                end do
            end do
            !$omp end parallel do
    end select
end function integrate_3d_complex
!
!> Integrate 1D array using cumulative summation
!
function cumsum_1d_complex(w) result(wt)
    complex, dimension(:) :: w
    complex, allocatable, dimension(:) :: wt
    integer :: i, n
    n = size(w)
    allocate (wt(1:n))
    wt(1) = w(1)
    do i = 2, n
        wt(i) = wt(i - 1) + w(i)
    end do
end function cumsum_1d_complex
!
!> Integrate 2D array using cumulative summation
!
function cumsum_2d_complex(w, dim) result(wt)
    complex, dimension(:, :) :: w
    integer, optional :: dim
    complex, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate (wt(1:n1, 1:n2))
    select case (along)
        case (1)
            wt(1, :) = w(1, :)
            !$omp parallel do private(i)
            do i = 2, n1
                wt(i, :) = wt(i - 1, :) + w(i, :)
            end do
            !$omp end parallel do
        case (2)
            wt(:, 1) = w(:, 1)
            !$omp parallel do private(j)
            do j = 2, n2
                wt(:, j) = wt(:, j - 1) + w(:, j)
            end do
            !$omp end parallel do
    end select
end function cumsum_2d_complex
!
!> Integrate 3D array using cumulative summation
!
function cumsum_3d_complex(w, dim) result(wt)
    complex, dimension(:, :, :) :: w
    integer, optional :: dim
    complex, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate (wt(1:n1, 1:n2, 1:n3))
    select case (along)
        case (1)
            wt(1, :, :) = w(1, :, :)
            !$omp parallel do private(i)
            do i = 2, n1
                wt(i, :, :) = wt(i - 1, :, :) + w(i, :, :)
            end do
            !$omp end parallel do
        case (2)
            wt(:, 1, :) = w(:, 1, :)
            !$omp parallel do private(j)
            do j = 2, n2
                wt(:, j, :) = wt(:, j - 1, :) + w(:, j, :)
            end do
            !$omp end parallel do
        case (3)
            wt(:, :, 1) = w(:, :, 1)
            !$omp parallel do private(k)
            do k = 2, n3
                wt(:, :, k) = wt(:, :, k - 1) + w(:, :, k)
            end do
            !$omp end parallel do
    end select
end function cumsum_3d_complex
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
!> Differentiate 1D array using finite difference
!
function differentiate_1d_dcomplex(w, order, method, accuracy) result(wt)
    double complex, dimension(:), intent(in) :: w
    integer, intent(in), optional :: order, accuracy
    character(len=*), intent(in), optional :: method
    double complex, allocatable, dimension(:) :: wt
    integer :: i, n
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n = size(w)
    allocate (wt(1:n))
    select case (deriv_order)
        case (1)
            select case (deriv_method)
                case ('forward')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n - 1
                                wt(i) = sum(fdcoef_1f10*w(i:i + 1))
                            end do
                            wt(n) = w(n) - w(n - 1)
                        case (2)
                            do i = 1, n - 2
                                wt(i) = sum(fdcoef_1f20*w(i:i + 2))
                            end do
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (3)
                            do i = 1, n - 3
                                wt(i) = sum(fdcoef_1f30*w(i:i + 3))
                            end do
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (4)
                            do i = 1, n - 4
                                wt(i) = sum(fdcoef_1f40*w(i:i + 4))
                            end do
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (5)
                            do i = 1, n - 5
                                wt(i) = sum(fdcoef_1f50*w(i:i + 5))
                            end do
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (6)
                            do i = 1, n - 6
                                wt(i) = sum(fdcoef_1f60*w(i:i + 6))
                            end do
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (7)
                            do i = 1, n - 7
                                wt(i) = sum(fdcoef_1f70*w(i:i + 7))
                            end do
                            wt(n - 6) = sum(fdcoef_1f60*w(n - 6:n))
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                        case (8)
                            do i = 1, n - 8
                                wt(i) = sum(fdcoef_1f80*w(i:i + 8))
                            end do
                            wt(n - 7) = sum(fdcoef_1f70*w(n - 7:n))
                            wt(n - 6) = sum(fdcoef_1f60*w(n - 6:n))
                            wt(n - 5) = sum(fdcoef_1f50*w(n - 5:n))
                            wt(n - 4) = sum(fdcoef_1f40*w(n - 4:n))
                            wt(n - 3) = sum(fdcoef_1f30*w(n - 3:n))
                            wt(n - 2) = sum(fdcoef_1f20*w(n - 2:n))
                            wt(n - 1) = sum(fdcoef_1f10*w(n - 1:n))
                            wt(n) = w(n) - w(n - 1)
                    end select
                case ('center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n - 1
                                wt(i) = sum(fdcoef_1c10(1:3)*w(i - 1:i + 1))
                            end do
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (2)
                            do i = 3, n - 2
                                wt(i) = sum(fdcoef_1c20(1:5)*w(i - 2:i + 2))
                            end do
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (3)
                            do i = 4, n - 3
                                wt(i) = sum(fdcoef_1c30(1:7)*w(i - 3:i + 3))
                            end do
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (4)
                            do i = 5, n - 4
                                wt(i) = sum(fdcoef_1c40(1:9)*w(i - 4:i + 4))
                            end do
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (5)
                            do i = 6, n - 5
                                wt(i) = sum(fdcoef_1c50(1:11)*w(i - 5:i + 5))
                            end do
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (6)
                            do i = 7, n - 6
                                wt(i) = sum(fdcoef_1c60(1:13)*w(i - 6:i + 6))
                            end do
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (7)
                            do i = 8, n - 7
                                wt(i) = sum(fdcoef_1c70(1:15)*w(i - 7:i + 7))
                            end do
                            wt(7) = sum(fdcoef_1c60(1:13)*w(1:13))
                            wt(n - 6) = sum(-fdcoef_1c60(13:1:-1)*w(n - 12:n))
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                        case (8)
                            do i = 9, n - 8
                                wt(i) = sum(fdcoef_1c80(1:17)*w(i - 8:i + 8))
                            end do
                            wt(8) = sum(fdcoef_1c70(1:15)*w(1:15))
                            wt(n - 7) = sum(-fdcoef_1c70(15:1:-1)*w(n - 14:n))
                            wt(7) = sum(fdcoef_1c60(1:13)*w(1:13))
                            wt(n - 6) = sum(-fdcoef_1c60(13:1:-1)*w(n - 12:n))
                            wt(6) = sum(fdcoef_1c50(1:11)*w(1:11))
                            wt(n - 5) = sum(-fdcoef_1c50(11:1:-1)*w(n - 10:n))
                            wt(5) = sum(fdcoef_1c40(1:9)*w(1:9))
                            wt(n - 4) = sum(-fdcoef_1c40(9:1:-1)*w(n - 8:n))
                            wt(4) = sum(fdcoef_1c30(1:7)*w(1:7))
                            wt(n - 3) = sum(-fdcoef_1c30(7:1:-1)*w(n - 6:n))
                            wt(3) = sum(fdcoef_1c20(1:5)*w(1:5))
                            wt(n - 2) = sum(-fdcoef_1c20(5:1:-1)*w(n - 4:n))
                            wt(2) = sum(fdcoef_1c10(1:3)*w(1:3))
                            wt(n - 1) = sum(-fdcoef_1c10(3:1:-1)*w(n - 2:n))
                            wt(1) = w(2) - w(1)
                            wt(n) = w(n) - w(n - 1)
                    end select
                case ('backward')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n
                                wt(i) = sum(fdcoef_1b10*w(i - 1:i))
                            end do
                            wt(1) = w(2) - w(1)
                        case (2)
                            do i = 3, n
                                wt(i) = sum(fdcoef_1b20*w(i - 2:i))
                            end do
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (3)
                            do i = 4, n
                                wt(i) = sum(fdcoef_1b30*w(i - 3:i))
                            end do
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (4)
                            do i = 5, n
                                wt(i) = sum(fdcoef_1b40*w(i - 4:i))
                            end do
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (5)
                            do i = 6, n
                                wt(i) = sum(fdcoef_1b50*w(i - 5:i))
                            end do
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (6)
                            do i = 7, n
                                wt(i) = sum(fdcoef_1b60*w(i - 6:i))
                            end do
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (7)
                            do i = 8, n
                                wt(i) = sum(fdcoef_1b70*w(i - 7:i))
                            end do
                            wt(7) = sum(fdcoef_1b60*w(1:7))
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                        case (8)
                            do i = 9, n
                                wt(i) = sum(fdcoef_1b80*w(i - 8:i))
                            end do
                            wt(8) = sum(fdcoef_1b70*w(1:8))
                            wt(7) = sum(fdcoef_1b60*w(1:7))
                            wt(6) = sum(fdcoef_1b50*w(1:6))
                            wt(5) = sum(fdcoef_1b40*w(1:5))
                            wt(4) = sum(fdcoef_1b30*w(1:4))
                            wt(3) = sum(fdcoef_1b20*w(1:3))
                            wt(2) = sum(fdcoef_1b10*w(1:2))
                            wt(1) = w(2) - w(1)
                    end select
                case ('staggered-center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n - 1
                                wt(i) = sum(fdcoef_1s10*w(i:i + 1))
                            end do
                        case (2)
                            do i = 2, n - 2
                                wt(i) = sum(fdcoef_1s20*w(i - 1:i + 2))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                        case (3)
                            do i = 3, n - 3
                                wt(i) = sum(fdcoef_1s30*w(i - 2:i + 3))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                            wt(2) = sum(fdcoef_1s20*w(1:4))
                            wt(n - 2) = sum(fdcoef_1s20*w(n - 3:n))
                        case (4)
                            do i = 4, n - 4
                                wt(i) = sum(fdcoef_1s40*w(i - 3:i + 4))
                            end do
                            wt(1) = sum(fdcoef_1s10*w(1:2))
                            wt(n - 1) = sum(fdcoef_1s10*w(n - 1:n))
                            wt(2) = sum(fdcoef_1s20*w(1:4))
                            wt(n - 2) = sum(fdcoef_1s20*w(n - 3:n))
                            wt(3) = sum(fdcoef_1s30*w(1:6))
                            wt(n - 3) = sum(fdcoef_1s30*w(n - 5:n))
                    end select
            end select
        case (2)
            select case (deriv_method)
                case ('forward')
                    do i = 1, n - 2
                        wt(i) = w(i + 2) - 2*w(i + 1) + w(i)
                    end do
                    wt(n - 1) = wt(n - 2) + (wt(n - 2) - wt(n - 3))
                    wt(n) = wt(n - 1) + (wt(n - 1) - wt(n - 2))
                case ('center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 2, n - 1
                                wt(i) = sum(fdcoef_2c10*w(i - 1:i + 1))
                            end do
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (2)
                            do i = 3, n - 2
                                wt(i) = sum(fdcoef_2c20*w(i - 2:i + 2))
                            end do
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (3)
                            do i = 4, n - 3
                                wt(i) = sum(fdcoef_2c30*w(i - 3:i + 3))
                            end do
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (4)
                            do i = 5, n - 4
                                wt(i) = sum(fdcoef_2c40*w(i - 4:i + 4))
                            end do
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (5)
                            do i = 6, n - 5
                                wt(i) = sum(fdcoef_2c50*w(i - 5:i + 5))
                            end do
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (6)
                            do i = 7, n - 6
                                wt(i) = sum(fdcoef_2c60*w(i - 6:i + 6))
                            end do
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (7)
                            do i = 8, n - 7
                                wt(i) = sum(fdcoef_2c70*w(i - 7:i + 7))
                            end do
                            wt(7) = sum(fdcoef_2c60*w(1:13))
                            wt(n - 6) = sum(fdcoef_2c60*w(n - 12:n))
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                        case (8)
                            do i = 9, n - 8
                                wt(i) = sum(fdcoef_2c80*w(i - 8:i + 8))
                            end do
                            wt(8) = sum(fdcoef_2c70*w(1:15))
                            wt(n - 7) = sum(fdcoef_2c70*w(n - 14:n))
                            wt(7) = sum(fdcoef_2c60*w(1:13))
                            wt(n - 6) = sum(fdcoef_2c60*w(n - 12:n))
                            wt(6) = sum(fdcoef_2c50*w(1:11))
                            wt(n - 5) = sum(fdcoef_2c50*w(n - 10:n))
                            wt(5) = sum(fdcoef_2c40*w(1:9))
                            wt(n - 4) = sum(fdcoef_2c40*w(n - 8:n))
                            wt(4) = sum(fdcoef_2c30*w(1:7))
                            wt(n - 3) = sum(fdcoef_2c30*w(n - 6:n))
                            wt(3) = sum(fdcoef_2c20*w(1:5))
                            wt(n - 2) = sum(fdcoef_2c20*w(n - 4:n))
                            wt(2) = sum(fdcoef_2c10*w(1:3))
                            wt(n - 1) = sum(fdcoef_2c10*w(n - 2:n))
                            wt(1) = w(1) - 2*w(2) + w(3)
                            wt(n) = w(n) - 2*w(n - 1) + w(n - 2)
                    end select
                case ('backward')
                    do i = 3, n
                        wt(i) = w(i - 2) - 2*w(i - 1) + w(i)
                    end do
                    wt(1) = wt(2) - (wt(3) - wt(2))
                    wt(2) = wt(3) - (wt(4) - wt(3))
                case ('staggered-center')
                    select case (deriv_accuracy)
                        case (1)
                            do i = 1, n
                            end do
                    end select
            end select
    end select
end function differentiate_1d_dcomplex
!
!> Differentiate 2D array using finite difference
!
function differentiate_2d_dcomplex(w, dim, order, method, accuracy) result(wt)
    double complex, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: dim, order, accuracy
    character(len=*), intent(in), optional :: method
    double complex, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    wt = w
    select case (along)
        case (1)
            !$omp parallel do private(j)
            do j = 1, n2
                wt(:, j) = differentiate_1d_dcomplex(wt(:, j), deriv_order, deriv_method, deriv_accuracy)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i, :) = differentiate_1d_dcomplex(wt(i, :), deriv_order, deriv_method, deriv_accuracy)
            end do
            !$omp end parallel do
    end select
end function differentiate_2d_dcomplex
!
!> Differentiate 3D array using finite difference
!
function differentiate_3d_dcomplex(w, dim, order, method, accuracy) result(wt)
    double complex, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: dim, order, accuracy
    character(len=*), intent(in), optional :: method
    double complex, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    character(len=12) :: deriv_method
    integer :: deriv_order, deriv_accuracy
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    if (present(order)) then
        deriv_order = order
    else
        deriv_order = 1
    end if
    if (present(accuracy)) then
        deriv_accuracy = accuracy
    else
        deriv_accuracy = 1
    end if
    if (present(method)) then
        deriv_method = trim(adjustl((method)))
    else
        deriv_method = 'center'
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    wt = w
    select case (along)
        case (1)
            !$omp parallel do private(j, k)
            do k = 1, n3
                do j = 1, n2
                    wt(:, j, k) = differentiate_1d_dcomplex(wt(:, j, k), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, k)
            do k = 1, n3
                do i = 1, n1
                    wt(i, :, k) = differentiate_1d_dcomplex(wt(i, :, k), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j, :) = differentiate_1d_dcomplex(wt(i, j, :), deriv_order, deriv_method, deriv_accuracy)
                end do
            end do
            !$omp end parallel do
    end select
end function differentiate_3d_dcomplex
!
!> Integrate 1D array using cumulative trapezoidal rule
!
function integrate_1d_dcomplex(w) result(wt)
    double complex, dimension(:) :: w
    double complex, allocatable, dimension(:) :: wt
    integer :: i, n
    n = size(w)
    allocate (wt(1:n))
    wt(1) = 0
    do i = 2, n
        wt(i) = wt(i - 1) + 0.5d0*(w(i - 1) + w(i))
    end do
end function integrate_1d_dcomplex
!
!> Integrate 2D array using cumulative trapezoidal rule
!
function integrate_2d_dcomplex(w, dim) result(wt)
    double complex, dimension(:, :) :: w
    integer, optional :: dim
    double complex, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate (wt(1:n1, 1:n2))
    select case (along)
        case (1)
            !$omp parallel do private(j)
            do j = 1, n2
                wt(:, j) = integrate_1d_dcomplex(w(:, j))
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i, :) = integrate_1d_dcomplex(w(i, :))
            end do
            !$omp end parallel do
    end select
end function integrate_2d_dcomplex
!
!> Integrate 3D array using cumulative trapezoidal rule
!
function integrate_3d_dcomplex(w, dim) result(wt)
    double complex, dimension(:, :, :) :: w
    integer, optional :: dim
    double complex, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate (wt(1:n1, 1:n2, 1:n3))
    select case (along)
        case (1)
            !$omp parallel do private(j, k)
            do k = 1, n3
                do j = 1, n2
                    wt(:, j, k) = integrate_1d_dcomplex(w(:, j, k))
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, k)
            do k = 1, n3
                do i = 1, n1
                    wt(i, :, k) = integrate_1d_dcomplex(w(i, :, k))
                end do
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j, :) = integrate_1d_dcomplex(w(i, j, :))
                end do
            end do
            !$omp end parallel do
    end select
end function integrate_3d_dcomplex
!
!> Integrate 1D array using cumulative summation
!
function cumsum_1d_dcomplex(w) result(wt)
    double complex, dimension(:) :: w
    double complex, allocatable, dimension(:) :: wt
    integer :: i, n
    n = size(w)
    allocate (wt(1:n))
    wt(1) = w(1)
    do i = 2, n
        wt(i) = wt(i - 1) + w(i)
    end do
end function cumsum_1d_dcomplex
!
!> Integrate 2D array using cumulative summation
!
function cumsum_2d_dcomplex(w, dim) result(wt)
    double complex, dimension(:, :) :: w
    integer, optional :: dim
    double complex, allocatable, dimension(:, :) :: wt
    integer :: along, n1, n2, i, j
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate (wt(1:n1, 1:n2))
    select case (along)
        case (1)
            wt(1, :) = w(1, :)
            !$omp parallel do private(i)
            do i = 2, n1
                wt(i, :) = wt(i - 1, :) + w(i, :)
            end do
            !$omp end parallel do
        case (2)
            wt(:, 1) = w(:, 1)
            !$omp parallel do private(j)
            do j = 2, n2
                wt(:, j) = wt(:, j - 1) + w(:, j)
            end do
            !$omp end parallel do
    end select
end function cumsum_2d_dcomplex
!
!> Integrate 3D array using cumulative summation
!
function cumsum_3d_dcomplex(w, dim) result(wt)
    double complex, dimension(:, :, :) :: w
    integer, optional :: dim
    double complex, allocatable, dimension(:, :, :) :: wt
    integer :: along, n1, n2, n3, i, j, k
    if (present(dim)) then
        along = dim
    else
        along = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate (wt(1:n1, 1:n2, 1:n3))
    select case (along)
        case (1)
            wt(1, :, :) = w(1, :, :)
            !$omp parallel do private(i)
            do i = 2, n1
                wt(i, :, :) = wt(i - 1, :, :) + w(i, :, :)
            end do
            !$omp end parallel do
        case (2)
            wt(:, 1, :) = w(:, 1, :)
            !$omp parallel do private(j)
            do j = 2, n2
                wt(:, j, :) = wt(:, j - 1, :) + w(:, j, :)
            end do
            !$omp end parallel do
        case (3)
            wt(:, :, 1) = w(:, :, 1)
            !$omp parallel do private(k)
            do k = 2, n3
                wt(:, :, k) = wt(:, :, k - 1) + w(:, :, k)
            end do
            !$omp end parallel do
    end select
end function cumsum_3d_dcomplex
end module libflit_calculus
