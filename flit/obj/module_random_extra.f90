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
module libflit_random_extra
    use libflit_array
    use libflit_array_operation
    use libflit_random
    use libflit_calculus
    use libflit_interp
    implicit none
    ! The module is to generate random values with customized PDF
    interface random_pdf
        module procedure :: random_pdf_1d_float
        module procedure :: random_pdf_1d_double
        module procedure :: random_pdf_2d_float
        module procedure :: random_pdf_2d_double
        module procedure :: random_pdf_3d_float
        module procedure :: random_pdf_3d_double
    end interface
    private
    public :: random_pdf
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
function random_pdf_1d_float(n, pdf, seed, range) result(y)
    integer, intent(in) :: n
    real, dimension(:), intent(in) :: pdf
    integer, intent(in), optional :: seed
    real, dimension(:), intent(in), optional :: range
    real, allocatable, dimension(:) :: y
    real, allocatable, dimension(:) :: r, x, xx, yy, cdf
    integer :: np
    if (present(range)) then
        r = range
    else
        r = [0.0, 1.0]
    end if
    np = size(pdf)
    x = linspace(r(1), r(2), np)
    xx = linspace(r(1), r(2), max(np, 1000))
    yy = ginterp(x, pdf, xx, 'linear')
    cdf = integ(yy)
    cdf = cdf/maxval(cdf)
    if (present(seed)) then
        y = random(n, seed=seed)
    else
        y = random(n)
    end if
    y = ginterp(cdf, xx, y, 'linear')
end function
function random_pdf_2d_float(n1, n2, pdf, seed, range) result(y)
    integer, intent(in) :: n1, n2
    real, dimension(:), intent(in) :: pdf
    integer, intent(in), optional :: seed
    real, dimension(:), intent(in), optional :: range
    real, allocatable, dimension(:, :) :: y
    real, allocatable, dimension(:) :: r, x, xx, yy, cdf
    integer :: np
    if (present(range)) then
        r = range
    else
        r = [0.0, 1.0]
    end if
    np = size(pdf)
    x = linspace(r(1), r(2), np)
    xx = linspace(r(1), r(2), max(np, 1000))
    yy = ginterp(x, pdf, xx, 'linear')
    cdf = integ(yy)
    cdf = cdf/maxval(cdf)
    if (present(seed)) then
        y = random(n1, n2, seed=seed)
    else
        y = random(n1, n2)
    end if
    y = reshape(ginterp(cdf, xx, flatten(y), 'linear'), shape(y))
end function
function random_pdf_3d_float(n1, n2, n3, pdf, seed, range) result(y)
    integer, intent(in) :: n1, n2, n3
    real, dimension(:), intent(in) :: pdf
    integer, intent(in), optional :: seed
    real, dimension(:), intent(in), optional :: range
    real, allocatable, dimension(:, :, :) :: y
    real, allocatable, dimension(:) :: r, x, xx, yy, cdf
    integer :: np
    if (present(range)) then
        r = range
    else
        r = [0.0, 1.0]
    end if
    np = size(pdf)
    x = linspace(r(1), r(2), np)
    xx = linspace(r(1), r(2), max(np, 1000))
    yy = ginterp(x, pdf, xx, 'linear')
    cdf = integ(yy)
    cdf = cdf/maxval(cdf)
    if (present(seed)) then
        y = random(n1, n2, n3, seed=seed)
    else
        y = random(n1, n2, n3)
    end if
    y = reshape(ginterp(cdf, xx, flatten(y), 'linear'), shape(y))
end function
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
function random_pdf_1d_double(n, pdf, seed, range) result(y)
    integer, intent(in) :: n
    double precision, dimension(:), intent(in) :: pdf
    integer, intent(in), optional :: seed
    double precision, dimension(:), intent(in), optional :: range
    double precision, allocatable, dimension(:) :: y
    double precision, allocatable, dimension(:) :: r, x, xx, yy, cdf
    integer :: np
    if (present(range)) then
        r = range
    else
        r = [0.0, 1.0]
    end if
    np = size(pdf)
    x = linspace(r(1), r(2), np)
    xx = linspace(r(1), r(2), max(np, 1000))
    yy = ginterp(x, pdf, xx, 'linear')
    cdf = integ(yy)
    cdf = cdf/maxval(cdf)
    if (present(seed)) then
        y = drandom(n, seed=seed)
    else
        y = drandom(n)
    end if
    y = ginterp(cdf, xx, y, 'linear')
end function
function random_pdf_2d_double(n1, n2, pdf, seed, range) result(y)
    integer, intent(in) :: n1, n2
    double precision, dimension(:), intent(in) :: pdf
    integer, intent(in), optional :: seed
    double precision, dimension(:), intent(in), optional :: range
    double precision, allocatable, dimension(:, :) :: y
    double precision, allocatable, dimension(:) :: r, x, xx, yy, cdf
    integer :: np
    if (present(range)) then
        r = range
    else
        r = [0.0, 1.0]
    end if
    np = size(pdf)
    x = linspace(r(1), r(2), np)
    xx = linspace(r(1), r(2), max(np, 1000))
    yy = ginterp(x, pdf, xx, 'linear')
    cdf = integ(yy)
    cdf = cdf/maxval(cdf)
    if (present(seed)) then
        y = drandom(n1, n2, seed=seed)
    else
        y = drandom(n1, n2)
    end if
    y = reshape(ginterp(cdf, xx, flatten(y), 'linear'), shape(y))
end function
function random_pdf_3d_double(n1, n2, n3, pdf, seed, range) result(y)
    integer, intent(in) :: n1, n2, n3
    double precision, dimension(:), intent(in) :: pdf
    integer, intent(in), optional :: seed
    double precision, dimension(:), intent(in), optional :: range
    double precision, allocatable, dimension(:, :, :) :: y
    double precision, allocatable, dimension(:) :: r, x, xx, yy, cdf
    integer :: np
    if (present(range)) then
        r = range
    else
        r = [0.0, 1.0]
    end if
    np = size(pdf)
    x = linspace(r(1), r(2), np)
    xx = linspace(r(1), r(2), max(np, 1000))
    yy = ginterp(x, pdf, xx, 'linear')
    cdf = integ(yy)
    cdf = cdf/maxval(cdf)
    if (present(seed)) then
        y = drandom(n1, n2, n3, seed=seed)
    else
        y = drandom(n1, n2, n3)
    end if
    y = reshape(ginterp(cdf, xx, flatten(y), 'linear'), shape(y))
end function
end module
