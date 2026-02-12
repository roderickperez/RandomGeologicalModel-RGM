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
module libflit_array_operation
    use libflit_error
    use libflit_array
    use libflit_constants
    implicit none
    ! Adjust the shape of an array
    interface adjust
        module procedure :: adjust_1d_int
        module procedure :: adjust_2d_int
        module procedure :: adjust_3d_int
        module procedure :: adjust_4d_int
        module procedure :: adjust_1d_float
        module procedure :: adjust_2d_float
        module procedure :: adjust_3d_float
        module procedure :: adjust_4d_float
        module procedure :: adjust_1d_double
        module procedure :: adjust_2d_double
        module procedure :: adjust_3d_double
        module procedure :: adjust_4d_double
        module procedure :: adjust_1d_complex
        module procedure :: adjust_2d_complex
        module procedure :: adjust_3d_complex
        module procedure :: adjust_4d_complex
        module procedure :: adjust_1d_dcomplex
        module procedure :: adjust_2d_dcomplex
        module procedure :: adjust_3d_dcomplex
        module procedure :: adjust_4d_dcomplex
    end interface adjust
    ! Convert a n-dimensional 1-sized array to a scalar
    interface as_scalar
        module procedure :: as_scalar_1d_int
        module procedure :: as_scalar_2d_int
        module procedure :: as_scalar_3d_int
        module procedure :: as_scalar_4d_int
        module procedure :: as_scalar_1d_float
        module procedure :: as_scalar_2d_float
        module procedure :: as_scalar_3d_float
        module procedure :: as_scalar_4d_float
        module procedure :: as_scalar_1d_double
        module procedure :: as_scalar_2d_double
        module procedure :: as_scalar_3d_double
        module procedure :: as_scalar_4d_double
        module procedure :: as_scalar_1d_complex
        module procedure :: as_scalar_2d_complex
        module procedure :: as_scalar_3d_complex
        module procedure :: as_scalar_4d_complex
        module procedure :: as_scalar_1d_dcomplex
        module procedure :: as_scalar_2d_dcomplex
        module procedure :: as_scalar_3d_dcomplex
        module procedure :: as_scalar_4d_dcomplex
    end interface as_scalar
    ! Crop an array by range in-place
    ! After cropping, the array lower and upper bounds may change.
    ! This is in constrast to functions crop, where the lower bound of
    ! the new array is always 1.
    interface crop_array
        module procedure :: crop_array_1d_int
        module procedure :: crop_array_2d_int
        module procedure :: crop_array_3d_int
        module procedure :: crop_array_4d_int
        module procedure :: crop_array_1d_float
        module procedure :: crop_array_2d_float
        module procedure :: crop_array_3d_float
        module procedure :: crop_array_4d_float
        module procedure :: crop_array_1d_double
        module procedure :: crop_array_2d_double
        module procedure :: crop_array_3d_double
        module procedure :: crop_array_4d_double
        module procedure :: crop_array_1d_complex
        module procedure :: crop_array_2d_complex
        module procedure :: crop_array_3d_complex
        module procedure :: crop_array_4d_complex
    end interface crop_array
    ! Crop an array by range
    interface crop
        module procedure :: crop_1d_int
        module procedure :: crop_2d_int
        module procedure :: crop_3d_int
        module procedure :: crop_4d_int
        module procedure :: crop_1d_float
        module procedure :: crop_2d_float
        module procedure :: crop_3d_float
        module procedure :: crop_4d_float
        module procedure :: crop_1d_double
        module procedure :: crop_2d_double
        module procedure :: crop_3d_double
        module procedure :: crop_4d_double
        module procedure :: crop_1d_complex
        module procedure :: crop_2d_complex
        module procedure :: crop_3d_complex
        module procedure :: crop_4d_complex
    end interface crop
    ! Cross product of 1D vectors
    interface cross2
        module procedure :: cross_product_2d_int
        module procedure :: cross_product_2d_float
        module procedure :: cross_product_2d_double
        module procedure :: cross_product_2d_complex
        module procedure :: cross_product_2d_dcomplex
    end interface cross2
    interface cross3
        module procedure :: cross_product_3d_int
        module procedure :: cross_product_3d_float
        module procedure :: cross_product_3d_double
        module procedure :: cross_product_3d_complex
        module procedure :: cross_product_3d_dcomplex
    end interface cross3
    ! Flatten an n-dimensional array to 1D
    interface flatten
        module procedure :: flatten_2d_int
        module procedure :: flatten_3d_int
        module procedure :: flatten_4d_int
        module procedure :: flatten_2d_float
        module procedure :: flatten_3d_float
        module procedure :: flatten_4d_float
        module procedure :: flatten_2d_double
        module procedure :: flatten_3d_double
        module procedure :: flatten_4d_double
        module procedure :: flatten_2d_complex
        module procedure :: flatten_3d_complex
        module procedure :: flatten_4d_complex
        module procedure :: flatten_2d_dcomplex
        module procedure :: flatten_3d_dcomplex
        module procedure :: flatten_4d_dcomplex
    end interface flatten
    ! Flip array along one or several axes
    interface flip
        module procedure :: flip_1d_int
        module procedure :: flip_2d_int
        module procedure :: flip_3d_int
        module procedure :: flip_1d_float
        module procedure :: flip_2d_float
        module procedure :: flip_3d_float
        module procedure :: flip_1d_double
        module procedure :: flip_2d_double
        module procedure :: flip_3d_double
        module procedure :: flip_1d_complex
        module procedure :: flip_2d_complex
        module procedure :: flip_3d_complex
        module procedure :: flip_1d_dcomplex
        module procedure :: flip_2d_dcomplex
        module procedure :: flip_3d_dcomplex
    end interface flip
    ! If-then-else ternary operation, including scalar and array
    interface ifelse
        module procedure :: ifelse_int
        module procedure :: ifelse_float
        module procedure :: ifelse_double
        module procedure :: ifelse_complex
        module procedure :: ifelse_dcomplex
        module procedure :: ifelse_1d_int
        module procedure :: ifelse_2d_int
        module procedure :: ifelse_3d_int
        module procedure :: ifelse_4d_int
        module procedure :: ifelse_1d_float
        module procedure :: ifelse_2d_float
        module procedure :: ifelse_3d_float
        module procedure :: ifelse_4d_float
        module procedure :: ifelse_1d_double
        module procedure :: ifelse_2d_double
        module procedure :: ifelse_3d_double
        module procedure :: ifelse_4d_double
        module procedure :: ifelse_1d_complex
        module procedure :: ifelse_2d_complex
        module procedure :: ifelse_3d_complex
        module procedure :: ifelse_4d_complex
        module procedure :: ifelse_1d_dcomplex
        module procedure :: ifelse_2d_dcomplex
        module procedure :: ifelse_3d_dcomplex
        module procedure :: ifelse_4d_dcomplex
        module procedure :: ifelse_string
    end interface ifelse
    ! Mask an array
    interface mask
        module procedure :: mask_1d_int
        module procedure :: mask_2d_int
        module procedure :: mask_3d_int
        module procedure :: mask_4d_int
        module procedure :: mask_1d_float
        module procedure :: mask_2d_float
        module procedure :: mask_3d_float
        module procedure :: mask_4d_float
        module procedure :: mask_1d_double
        module procedure :: mask_2d_double
        module procedure :: mask_3d_double
        module procedure :: mask_4d_double
        module procedure :: mask_1d_complex
        module procedure :: mask_2d_complex
        module procedure :: mask_3d_complex
        module procedure :: mask_4d_complex
! module procedure :: mask_1d_dcomplex
! module procedure :: mask_2d_dcomplex
! module procedure :: mask_3d_dcomplex
! module procedure :: mask_4d_dcomplex
    end interface mask
    ! Pad an array
    interface pad_array
        module procedure :: pad_array_1d_int
        module procedure :: pad_array_2d_int
        module procedure :: pad_array_3d_int
        module procedure :: pad_array_1d_float
        module procedure :: pad_array_2d_float
        module procedure :: pad_array_3d_float
        module procedure :: pad_array_1d_double
        module procedure :: pad_array_2d_double
        module procedure :: pad_array_3d_double
        module procedure :: pad_array_1d_complex
        module procedure :: pad_array_2d_complex
        module procedure :: pad_array_3d_complex
        module procedure :: pad_array_1d_dcomplex
        module procedure :: pad_array_2d_dcomplex
        module procedure :: pad_array_3d_dcomplex
        module procedure :: pad_array_1d_string
        module procedure :: pad_array_2d_string
        module procedure :: pad_array_3d_string
    end interface pad_array
    ! Pad an array
    interface pad
        module procedure :: pad_1d_int
        module procedure :: pad_2d_int
        module procedure :: pad_3d_int
        module procedure :: pad_1d_float
        module procedure :: pad_2d_float
        module procedure :: pad_3d_float
        module procedure :: pad_1d_double
        module procedure :: pad_2d_double
        module procedure :: pad_3d_double
        module procedure :: pad_1d_complex
        module procedure :: pad_2d_complex
        module procedure :: pad_3d_complex
        module procedure :: pad_1d_dcomplex
        module procedure :: pad_2d_dcomplex
        module procedure :: pad_3d_dcomplex
        module procedure :: pad_1d_string
        module procedure :: pad_2d_string
        module procedure :: pad_3d_string
    end interface pad
    ! Permute an array in-place
    interface permute_array
        module procedure :: permute_array_3d_int
        module procedure :: permute_array_3d_float
        module procedure :: permute_array_3d_double
        module procedure :: permute_array_3d_complex
        module procedure :: permute_array_3d_dcomplex
        module procedure :: permute_array_4d_int
        module procedure :: permute_array_4d_float
        module procedure :: permute_array_4d_double
        module procedure :: permute_array_4d_complex
        module procedure :: permute_array_4d_dcomplex
    end interface permute_array
    ! Permute an array out-of-place
    interface permute
        module procedure :: permute_3d_int
        module procedure :: permute_3d_float
        module procedure :: permute_3d_double
        module procedure :: permute_3d_complex
        module procedure :: permute_3d_dcomplex
        module procedure :: permute_4d_int
        module procedure :: permute_4d_float
        module procedure :: permute_4d_double
        module procedure :: permute_4d_complex
        module procedure :: permute_4d_dcomplex
    end interface permute
    ! Linearly scale array to a specific range
    interface rescale
        module procedure :: rescale_1d_float
        module procedure :: rescale_2d_float
        module procedure :: rescale_3d_float
        module procedure :: rescale_4d_float
        module procedure :: rescale_1d_double
        module procedure :: rescale_2d_double
        module procedure :: rescale_3d_double
        module procedure :: rescale_4d_double
    end interface rescale
    ! Rotate array CW or CCW by 90 degrees
    interface rot90
        module procedure :: rot90_1d_int
        module procedure :: rot90_2d_int
        module procedure :: rot90_3d_int
        module procedure :: rot90_1d_float
        module procedure :: rot90_2d_float
        module procedure :: rot90_3d_float
        module procedure :: rot90_1d_double
        module procedure :: rot90_2d_double
        module procedure :: rot90_3d_double
        module procedure :: rot90_1d_complex
        module procedure :: rot90_2d_complex
        module procedure :: rot90_3d_complex
        module procedure :: rot90_1d_dcomplex
        module procedure :: rot90_2d_dcomplex
        module procedure :: rot90_3d_dcomplex
    end interface rot90
    ! Slice an (n - 1)-dimensional array from an n-dimensional array
    interface slice
        module procedure :: slice_1d_int
        module procedure :: slice_1d_float
        module procedure :: slice_1d_double
        module procedure :: slice_1d_complex
        module procedure :: slice_1d_dcomplex
        module procedure :: slice_1d_logical
        module procedure :: slice_2d_int
        module procedure :: slice_2d_float
        module procedure :: slice_2d_double
        module procedure :: slice_2d_complex
        module procedure :: slice_2d_dcomplex
        module procedure :: slice_2d_logical
        module procedure :: slice_3d_int
        module procedure :: slice_3d_float
        module procedure :: slice_3d_double
        module procedure :: slice_3d_complex
        module procedure :: slice_3d_dcomplex
        module procedure :: slice_3d_logical
    end interface slice
    ! Any element in another array
    interface any_in
        module procedure :: any_in_1d_int
        module procedure :: any_in_1d_float
        module procedure :: any_in_1d_double
        module procedure :: any_in_1d_complex
        module procedure :: any_in_1d_dcomplex
        module procedure :: any_in_1d_string
    end interface any_in
    ! All elements must in another array
    interface all_in
        module procedure :: all_in_1d_int
        module procedure :: all_in_1d_float
        module procedure :: all_in_1d_double
        module procedure :: all_in_1d_complex
        module procedure :: all_in_1d_dcomplex
        module procedure :: all_in_1d_string
    end interface all_in
    interface remove_any_in
        module procedure :: remove_any_in_1d_int
        module procedure :: remove_any_in_1d_float
        module procedure :: remove_any_in_1d_double
        module procedure :: remove_any_in_1d_complex
        module procedure :: remove_any_in_1d_dcomplex
        module procedure :: remove_any_in_1d_string
    end interface remove_any_in
    interface binarize
        module procedure :: binarize_1d_int
        module procedure :: binarize_1d_float
        module procedure :: binarize_1d_double
        module procedure :: binarize_2d_int
        module procedure :: binarize_2d_float
        module procedure :: binarize_2d_double
        module procedure :: binarize_3d_int
        module procedure :: binarize_3d_float
        module procedure :: binarize_3d_double
        module procedure :: binarize_4d_int
        module procedure :: binarize_4d_float
        module procedure :: binarize_4d_double
    end interface binarize
    ! Repeat an array
    interface tile
        module procedure :: tile_1d_int
        module procedure :: tile_1d_float
        module procedure :: tile_1d_double
        module procedure :: tile_1d_complex
        module procedure :: tile_1d_dcomplex
        module procedure :: tile_1d_logical
        module procedure :: tile_2d_int
        module procedure :: tile_2d_float
        module procedure :: tile_2d_double
        module procedure :: tile_2d_complex
        module procedure :: tile_2d_dcomplex
        module procedure :: tile_2d_logical
        module procedure :: tile_3d_int
        module procedure :: tile_3d_float
        module procedure :: tile_3d_double
        module procedure :: tile_3d_complex
        module procedure :: tile_3d_dcomplex
        module procedure :: tile_3d_logical
        module procedure :: tile_4d_int
        module procedure :: tile_4d_float
        module procedure :: tile_4d_double
        module procedure :: tile_4d_complex
        module procedure :: tile_4d_dcomplex
        module procedure :: tile_4d_logical
        ! For string, the only useful might be 1d tiling
        module procedure :: tile_1d_string
    end interface tile
    private
    public :: adjust
    public :: as_scalar
    public :: crop_array
    public :: crop
    public :: cross2, cross3
    public :: flatten
    public :: flip
    public :: ifelse
    public :: mask
    public :: pad_array
    public :: pad
    public :: permute_array
    public :: permute
    public :: rescale
    public :: rot90
    public :: slice
    public :: ndigits
    public :: breakint
    public :: any_in
    public :: all_in
    public :: remove_any_in
    public :: binarize
    public :: tile
contains
    !================================================================
    ! Adjust the shape of an array
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
function adjust_1d_int(w, n) result(wp)
    integer, dimension(:), intent(in) :: w
    integer, intent(in) :: n
    integer, allocatable, dimension(:) :: wp
    integer :: nw
    nw = size(w)
    allocate (wp(1:n))
    wp = 0
    wp(1:min(nw, n)) = w(1:min(nw, n))
end function adjust_1d_int
function adjust_2d_int(w, n) result(wp)
    integer, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    integer, allocatable, dimension(:, :) :: wp
    integer :: nw(1:2)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2)))
    wp = 0
    wp(1:nw(1), 1:nw(2)) = w(1:nw(1), 1:nw(2))
end function adjust_2d_int
function adjust_3d_int(w, n) result(wp)
    integer, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    integer, allocatable, dimension(:, :, :) :: wp
    integer :: nw(1:3)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2), 1:n(3)))
    wp = 0
    wp(1:nw(1), 1:nw(2), 1:nw(3)) = w(1:nw(1), 1:nw(2), 1:nw(3))
end function adjust_3d_int
function adjust_4d_int(w, n) result(wp)
    integer, dimension(:, :, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    integer, allocatable, dimension(:, :, :, :) :: wp
    integer :: nw(1:4)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2), 1:n(3), 1:n(4)))
    wp = 0
    wp(1:nw(1), 1:nw(2), 1:nw(3), 1:nw(4)) = w(1:nw(1), 1:nw(2), 1:nw(3), 1:nw(4))
end function adjust_4d_int
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
function adjust_1d_float(w, n) result(wp)
    real, dimension(:), intent(in) :: w
    integer, intent(in) :: n
    real, allocatable, dimension(:) :: wp
    integer :: nw
    nw = size(w)
    allocate (wp(1:n))
    wp = 0
    wp(1:min(nw, n)) = w(1:min(nw, n))
end function adjust_1d_float
function adjust_2d_float(w, n) result(wp)
    real, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    real, allocatable, dimension(:, :) :: wp
    integer :: nw(1:2)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2)))
    wp = 0
    wp(1:nw(1), 1:nw(2)) = w(1:nw(1), 1:nw(2))
end function adjust_2d_float
function adjust_3d_float(w, n) result(wp)
    real, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    real, allocatable, dimension(:, :, :) :: wp
    integer :: nw(1:3)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2), 1:n(3)))
    wp = 0
    wp(1:nw(1), 1:nw(2), 1:nw(3)) = w(1:nw(1), 1:nw(2), 1:nw(3))
end function adjust_3d_float
function adjust_4d_float(w, n) result(wp)
    real, dimension(:, :, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    real, allocatable, dimension(:, :, :, :) :: wp
    integer :: nw(1:4)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2), 1:n(3), 1:n(4)))
    wp = 0
    wp(1:nw(1), 1:nw(2), 1:nw(3), 1:nw(4)) = w(1:nw(1), 1:nw(2), 1:nw(3), 1:nw(4))
end function adjust_4d_float
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
function adjust_1d_double(w, n) result(wp)
    double precision, dimension(:), intent(in) :: w
    integer, intent(in) :: n
    double precision, allocatable, dimension(:) :: wp
    integer :: nw
    nw = size(w)
    allocate (wp(1:n))
    wp = 0
    wp(1:min(nw, n)) = w(1:min(nw, n))
end function adjust_1d_double
function adjust_2d_double(w, n) result(wp)
    double precision, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    double precision, allocatable, dimension(:, :) :: wp
    integer :: nw(1:2)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2)))
    wp = 0
    wp(1:nw(1), 1:nw(2)) = w(1:nw(1), 1:nw(2))
end function adjust_2d_double
function adjust_3d_double(w, n) result(wp)
    double precision, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    double precision, allocatable, dimension(:, :, :) :: wp
    integer :: nw(1:3)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2), 1:n(3)))
    wp = 0
    wp(1:nw(1), 1:nw(2), 1:nw(3)) = w(1:nw(1), 1:nw(2), 1:nw(3))
end function adjust_3d_double
function adjust_4d_double(w, n) result(wp)
    double precision, dimension(:, :, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    double precision, allocatable, dimension(:, :, :, :) :: wp
    integer :: nw(1:4)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2), 1:n(3), 1:n(4)))
    wp = 0
    wp(1:nw(1), 1:nw(2), 1:nw(3), 1:nw(4)) = w(1:nw(1), 1:nw(2), 1:nw(3), 1:nw(4))
end function adjust_4d_double
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
function adjust_1d_complex(w, n) result(wp)
    complex, dimension(:), intent(in) :: w
    integer, intent(in) :: n
    complex, allocatable, dimension(:) :: wp
    integer :: nw
    nw = size(w)
    allocate (wp(1:n))
    wp = 0
    wp(1:min(nw, n)) = w(1:min(nw, n))
end function adjust_1d_complex
function adjust_2d_complex(w, n) result(wp)
    complex, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    complex, allocatable, dimension(:, :) :: wp
    integer :: nw(1:2)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2)))
    wp = 0
    wp(1:nw(1), 1:nw(2)) = w(1:nw(1), 1:nw(2))
end function adjust_2d_complex
function adjust_3d_complex(w, n) result(wp)
    complex, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    complex, allocatable, dimension(:, :, :) :: wp
    integer :: nw(1:3)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2), 1:n(3)))
    wp = 0
    wp(1:nw(1), 1:nw(2), 1:nw(3)) = w(1:nw(1), 1:nw(2), 1:nw(3))
end function adjust_3d_complex
function adjust_4d_complex(w, n) result(wp)
    complex, dimension(:, :, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    complex, allocatable, dimension(:, :, :, :) :: wp
    integer :: nw(1:4)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2), 1:n(3), 1:n(4)))
    wp = 0
    wp(1:nw(1), 1:nw(2), 1:nw(3), 1:nw(4)) = w(1:nw(1), 1:nw(2), 1:nw(3), 1:nw(4))
end function adjust_4d_complex
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
function adjust_1d_dcomplex(w, n) result(wp)
    double complex, dimension(:), intent(in) :: w
    integer, intent(in) :: n
    double complex, allocatable, dimension(:) :: wp
    integer :: nw
    nw = size(w)
    allocate (wp(1:n))
    wp = 0
    wp(1:min(nw, n)) = w(1:min(nw, n))
end function adjust_1d_dcomplex
function adjust_2d_dcomplex(w, n) result(wp)
    double complex, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    double complex, allocatable, dimension(:, :) :: wp
    integer :: nw(1:2)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2)))
    wp = 0
    wp(1:nw(1), 1:nw(2)) = w(1:nw(1), 1:nw(2))
end function adjust_2d_dcomplex
function adjust_3d_dcomplex(w, n) result(wp)
    double complex, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    double complex, allocatable, dimension(:, :, :) :: wp
    integer :: nw(1:3)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2), 1:n(3)))
    wp = 0
    wp(1:nw(1), 1:nw(2), 1:nw(3)) = w(1:nw(1), 1:nw(2), 1:nw(3))
end function adjust_3d_dcomplex
function adjust_4d_dcomplex(w, n) result(wp)
    double complex, dimension(:, :, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: n
    double complex, allocatable, dimension(:, :, :, :) :: wp
    integer :: nw(1:4)
    nw = min(shape(w), n)
    allocate (wp(1:n(1), 1:n(2), 1:n(3), 1:n(4)))
    wp = 0
    wp(1:nw(1), 1:nw(2), 1:nw(3), 1:nw(4)) = w(1:nw(1), 1:nw(2), 1:nw(3), 1:nw(4))
end function adjust_4d_dcomplex
! #define T logical
! #define TT logical
! #include "template_adjust.f90"
    !================================================================
    ! As scalar
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
function as_scalar_1d_int(w, i) result(s)
    integer, intent(in), dimension(:) :: w
    integer, intent(in), optional :: i
    integer :: s
    if (present(i)) then
        s = w(i)
    else
        s = w(1)
    end if
end function as_scalar_1d_int
function as_scalar_2d_int(w, i) result(s)
    integer, intent(in), dimension(:, :) :: w
    integer, dimension(:), intent(in), optional :: i
    integer :: s
    if (present(i)) then
        s = w(i(1), i(2))
    else
        s = w(1, 1)
    end if
end function as_scalar_2d_int
function as_scalar_3d_int(w, i) result(s)
    integer, intent(in), dimension(:, :, :) :: w
    integer, dimension(:), intent(in), optional :: i
    integer :: s
    if (present(i)) then
        s = w(i(1), i(2), i(3))
    else
        s = w(1, 1, 1)
    end if
end function as_scalar_3d_int
function as_scalar_4d_int(w, i) result(s)
    integer, intent(in), dimension(:, :, :, :) :: w
    integer, dimension(:), intent(in), optional :: i
    integer :: s
    if (present(i)) then
        s = w(i(1), i(2), i(3), i(4))
    else
        s = w(1, 1, 1, 1)
    end if
end function as_scalar_4d_int
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
function as_scalar_1d_float(w, i) result(s)
    real, intent(in), dimension(:) :: w
    integer, intent(in), optional :: i
    real :: s
    if (present(i)) then
        s = w(i)
    else
        s = w(1)
    end if
end function as_scalar_1d_float
function as_scalar_2d_float(w, i) result(s)
    real, intent(in), dimension(:, :) :: w
    integer, dimension(:), intent(in), optional :: i
    real :: s
    if (present(i)) then
        s = w(i(1), i(2))
    else
        s = w(1, 1)
    end if
end function as_scalar_2d_float
function as_scalar_3d_float(w, i) result(s)
    real, intent(in), dimension(:, :, :) :: w
    integer, dimension(:), intent(in), optional :: i
    real :: s
    if (present(i)) then
        s = w(i(1), i(2), i(3))
    else
        s = w(1, 1, 1)
    end if
end function as_scalar_3d_float
function as_scalar_4d_float(w, i) result(s)
    real, intent(in), dimension(:, :, :, :) :: w
    integer, dimension(:), intent(in), optional :: i
    real :: s
    if (present(i)) then
        s = w(i(1), i(2), i(3), i(4))
    else
        s = w(1, 1, 1, 1)
    end if
end function as_scalar_4d_float
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
function as_scalar_1d_double(w, i) result(s)
    double precision, intent(in), dimension(:) :: w
    integer, intent(in), optional :: i
    double precision :: s
    if (present(i)) then
        s = w(i)
    else
        s = w(1)
    end if
end function as_scalar_1d_double
function as_scalar_2d_double(w, i) result(s)
    double precision, intent(in), dimension(:, :) :: w
    integer, dimension(:), intent(in), optional :: i
    double precision :: s
    if (present(i)) then
        s = w(i(1), i(2))
    else
        s = w(1, 1)
    end if
end function as_scalar_2d_double
function as_scalar_3d_double(w, i) result(s)
    double precision, intent(in), dimension(:, :, :) :: w
    integer, dimension(:), intent(in), optional :: i
    double precision :: s
    if (present(i)) then
        s = w(i(1), i(2), i(3))
    else
        s = w(1, 1, 1)
    end if
end function as_scalar_3d_double
function as_scalar_4d_double(w, i) result(s)
    double precision, intent(in), dimension(:, :, :, :) :: w
    integer, dimension(:), intent(in), optional :: i
    double precision :: s
    if (present(i)) then
        s = w(i(1), i(2), i(3), i(4))
    else
        s = w(1, 1, 1, 1)
    end if
end function as_scalar_4d_double
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
function as_scalar_1d_complex(w, i) result(s)
    complex, intent(in), dimension(:) :: w
    integer, intent(in), optional :: i
    complex :: s
    if (present(i)) then
        s = w(i)
    else
        s = w(1)
    end if
end function as_scalar_1d_complex
function as_scalar_2d_complex(w, i) result(s)
    complex, intent(in), dimension(:, :) :: w
    integer, dimension(:), intent(in), optional :: i
    complex :: s
    if (present(i)) then
        s = w(i(1), i(2))
    else
        s = w(1, 1)
    end if
end function as_scalar_2d_complex
function as_scalar_3d_complex(w, i) result(s)
    complex, intent(in), dimension(:, :, :) :: w
    integer, dimension(:), intent(in), optional :: i
    complex :: s
    if (present(i)) then
        s = w(i(1), i(2), i(3))
    else
        s = w(1, 1, 1)
    end if
end function as_scalar_3d_complex
function as_scalar_4d_complex(w, i) result(s)
    complex, intent(in), dimension(:, :, :, :) :: w
    integer, dimension(:), intent(in), optional :: i
    complex :: s
    if (present(i)) then
        s = w(i(1), i(2), i(3), i(4))
    else
        s = w(1, 1, 1, 1)
    end if
end function as_scalar_4d_complex
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
function as_scalar_1d_dcomplex(w, i) result(s)
    double complex, intent(in), dimension(:) :: w
    integer, intent(in), optional :: i
    double complex :: s
    if (present(i)) then
        s = w(i)
    else
        s = w(1)
    end if
end function as_scalar_1d_dcomplex
function as_scalar_2d_dcomplex(w, i) result(s)
    double complex, intent(in), dimension(:, :) :: w
    integer, dimension(:), intent(in), optional :: i
    double complex :: s
    if (present(i)) then
        s = w(i(1), i(2))
    else
        s = w(1, 1)
    end if
end function as_scalar_2d_dcomplex
function as_scalar_3d_dcomplex(w, i) result(s)
    double complex, intent(in), dimension(:, :, :) :: w
    integer, dimension(:), intent(in), optional :: i
    double complex :: s
    if (present(i)) then
        s = w(i(1), i(2), i(3))
    else
        s = w(1, 1, 1)
    end if
end function as_scalar_3d_dcomplex
function as_scalar_4d_dcomplex(w, i) result(s)
    double complex, intent(in), dimension(:, :, :, :) :: w
    integer, dimension(:), intent(in), optional :: i
    double complex :: s
    if (present(i)) then
        s = w(i(1), i(2), i(3), i(4))
    else
        s = w(1, 1, 1, 1)
    end if
end function as_scalar_4d_dcomplex
! #define T logical
! #define TT logical
! #include "template_as_scalar.f90"
    !================================================================
    ! Cross product
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
function cross_product_2d_int(a, b) result(c)
    integer, dimension(1:2), intent(in) :: a, b
    integer :: c
    c = a(1)*b(2) - a(2)*b(1)
end function cross_product_2d_int
function cross_product_3d_int(a, b) result(c)
    integer, dimension(1:3), intent(in) :: a, b
    integer, allocatable, dimension(:) :: c
    allocate(c(1:3))
    c(1) = a(2)*b(3) - a(3)*b(2)
    c(2) = a(3)*b(1) - a(1)*b(3)
    c(3) = a(1)*b(2) - a(2)*b(1)
end function cross_product_3d_int
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
function cross_product_2d_float(a, b) result(c)
    real, dimension(1:2), intent(in) :: a, b
    real :: c
    c = a(1)*b(2) - a(2)*b(1)
end function cross_product_2d_float
function cross_product_3d_float(a, b) result(c)
    real, dimension(1:3), intent(in) :: a, b
    real, allocatable, dimension(:) :: c
    allocate(c(1:3))
    c(1) = a(2)*b(3) - a(3)*b(2)
    c(2) = a(3)*b(1) - a(1)*b(3)
    c(3) = a(1)*b(2) - a(2)*b(1)
end function cross_product_3d_float
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
function cross_product_2d_double(a, b) result(c)
    double precision, dimension(1:2), intent(in) :: a, b
    double precision :: c
    c = a(1)*b(2) - a(2)*b(1)
end function cross_product_2d_double
function cross_product_3d_double(a, b) result(c)
    double precision, dimension(1:3), intent(in) :: a, b
    double precision, allocatable, dimension(:) :: c
    allocate(c(1:3))
    c(1) = a(2)*b(3) - a(3)*b(2)
    c(2) = a(3)*b(1) - a(1)*b(3)
    c(3) = a(1)*b(2) - a(2)*b(1)
end function cross_product_3d_double
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
function cross_product_2d_complex(a, b) result(c)
    complex, dimension(1:2), intent(in) :: a, b
    complex :: c
    c = a(1)*b(2) - a(2)*b(1)
end function cross_product_2d_complex
function cross_product_3d_complex(a, b) result(c)
    complex, dimension(1:3), intent(in) :: a, b
    complex, allocatable, dimension(:) :: c
    allocate(c(1:3))
    c(1) = a(2)*b(3) - a(3)*b(2)
    c(2) = a(3)*b(1) - a(1)*b(3)
    c(3) = a(1)*b(2) - a(2)*b(1)
end function cross_product_3d_complex
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
function cross_product_2d_dcomplex(a, b) result(c)
    double complex, dimension(1:2), intent(in) :: a, b
    double complex :: c
    c = a(1)*b(2) - a(2)*b(1)
end function cross_product_2d_dcomplex
function cross_product_3d_dcomplex(a, b) result(c)
    double complex, dimension(1:3), intent(in) :: a, b
    double complex, allocatable, dimension(:) :: c
    allocate(c(1:3))
    c(1) = a(2)*b(3) - a(3)*b(2)
    c(2) = a(3)*b(1) - a(1)*b(3)
    c(3) = a(1)*b(2) - a(2)*b(1)
end function cross_product_3d_dcomplex
    !================================================================
    ! Crop
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
subroutine crop_array_1d_int(w, range)
    integer, allocatable, dimension(:), intent(inout) :: w
    integer, dimension(1:2), intent(in) :: range
    call assert(size(range) == 2 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1), &
        ' <crop_array_1d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2)))
end subroutine crop_array_1d_int
subroutine crop_array_2d_int(w, range)
    integer, allocatable, dimension(:, :), intent(inout) :: w
    integer, dimension(1:4), intent(in) :: range
    call assert(size(range) == 4 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2), &
        ' <crop_array_2d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4)))
end subroutine crop_array_2d_int
subroutine crop_array_3d_int(w, range)
    integer, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, dimension(1:6), intent(in) :: range
    call assert(size(range) == 6 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3), &
        ' <crop_array_3d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4), &
        range(5):range(6)))
end subroutine crop_array_3d_int
subroutine crop_array_4d_int(w, range)
    integer, allocatable, dimension(:, :, :, :), intent(inout) :: w
    integer, dimension(1:8), intent(in) :: range
    call assert(size(range) == 8 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3) &
        .and. range(7) >= lbound(w, 4) .and. range(8) <= ubound(w, 4), &
        ' <crop_array_4d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4), &
        range(5):range(6), &
        range(7):range(8)))
end subroutine crop_array_4d_int
function crop_1d_int(w, range) result(wt)
    integer, dimension(:), intent(in) :: w
    integer, dimension(1:2), intent(in) :: range
    integer, allocatable, dimension(:) :: wt
    call assert(size(range) == 2 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1), &
        ' <crop_array_1d> Error: range specification is wrong. ')
    call alloc_array(wt, [1, range(2) - range(1) + 1], &
        source=w(range(1):range(2)))
end function crop_1d_int
function crop_2d_int(w, range) result(wt)
    integer, dimension(:, :), intent(in) :: w
    integer, dimension(1:4), intent(in) :: range
    integer, allocatable, dimension(:, :) :: wt
    call assert(size(range) == 4 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2), &
        ' <crop_array_2d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4)))
end function crop_2d_int
function crop_3d_int(w, range) result(wt)
    integer, dimension(:, :, :), intent(in) :: w
    integer, dimension(1:6), intent(in) :: range
    integer, allocatable, dimension(:, :, :) :: wt
    call assert(size(range) == 6 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3), &
        ' <crop_array_3d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1, &
        1, range(6) - range(5) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4), &
        range(5):range(6)))
end function crop_3d_int
function crop_4d_int(w, range) result(wt)
    integer, dimension(:, :, :, :), intent(in) :: w
    integer, dimension(1:8), intent(in) :: range
    integer, allocatable, dimension(:, :, :, :) :: wt
    call assert(size(range) == 8 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3) &
        .and. range(7) >= lbound(w, 4) .and. range(8) <= ubound(w, 4), &
        ' <crop_array_4d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1, &
        1, range(6) - range(5) + 1, &
        1, range(8) - range(7) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4), &
        range(5):range(6), &
        range(7):range(8)))
end function crop_4d_int
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
subroutine crop_array_1d_float(w, range)
    real, allocatable, dimension(:), intent(inout) :: w
    integer, dimension(1:2), intent(in) :: range
    call assert(size(range) == 2 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1), &
        ' <crop_array_1d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2)))
end subroutine crop_array_1d_float
subroutine crop_array_2d_float(w, range)
    real, allocatable, dimension(:, :), intent(inout) :: w
    integer, dimension(1:4), intent(in) :: range
    call assert(size(range) == 4 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2), &
        ' <crop_array_2d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4)))
end subroutine crop_array_2d_float
subroutine crop_array_3d_float(w, range)
    real, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, dimension(1:6), intent(in) :: range
    call assert(size(range) == 6 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3), &
        ' <crop_array_3d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4), &
        range(5):range(6)))
end subroutine crop_array_3d_float
subroutine crop_array_4d_float(w, range)
    real, allocatable, dimension(:, :, :, :), intent(inout) :: w
    integer, dimension(1:8), intent(in) :: range
    call assert(size(range) == 8 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3) &
        .and. range(7) >= lbound(w, 4) .and. range(8) <= ubound(w, 4), &
        ' <crop_array_4d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4), &
        range(5):range(6), &
        range(7):range(8)))
end subroutine crop_array_4d_float
function crop_1d_float(w, range) result(wt)
    real, dimension(:), intent(in) :: w
    integer, dimension(1:2), intent(in) :: range
    real, allocatable, dimension(:) :: wt
    call assert(size(range) == 2 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1), &
        ' <crop_array_1d> Error: range specification is wrong. ')
    call alloc_array(wt, [1, range(2) - range(1) + 1], &
        source=w(range(1):range(2)))
end function crop_1d_float
function crop_2d_float(w, range) result(wt)
    real, dimension(:, :), intent(in) :: w
    integer, dimension(1:4), intent(in) :: range
    real, allocatable, dimension(:, :) :: wt
    call assert(size(range) == 4 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2), &
        ' <crop_array_2d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4)))
end function crop_2d_float
function crop_3d_float(w, range) result(wt)
    real, dimension(:, :, :), intent(in) :: w
    integer, dimension(1:6), intent(in) :: range
    real, allocatable, dimension(:, :, :) :: wt
    call assert(size(range) == 6 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3), &
        ' <crop_array_3d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1, &
        1, range(6) - range(5) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4), &
        range(5):range(6)))
end function crop_3d_float
function crop_4d_float(w, range) result(wt)
    real, dimension(:, :, :, :), intent(in) :: w
    integer, dimension(1:8), intent(in) :: range
    real, allocatable, dimension(:, :, :, :) :: wt
    call assert(size(range) == 8 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3) &
        .and. range(7) >= lbound(w, 4) .and. range(8) <= ubound(w, 4), &
        ' <crop_array_4d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1, &
        1, range(6) - range(5) + 1, &
        1, range(8) - range(7) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4), &
        range(5):range(6), &
        range(7):range(8)))
end function crop_4d_float
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
subroutine crop_array_1d_double(w, range)
    double precision, allocatable, dimension(:), intent(inout) :: w
    integer, dimension(1:2), intent(in) :: range
    call assert(size(range) == 2 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1), &
        ' <crop_array_1d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2)))
end subroutine crop_array_1d_double
subroutine crop_array_2d_double(w, range)
    double precision, allocatable, dimension(:, :), intent(inout) :: w
    integer, dimension(1:4), intent(in) :: range
    call assert(size(range) == 4 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2), &
        ' <crop_array_2d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4)))
end subroutine crop_array_2d_double
subroutine crop_array_3d_double(w, range)
    double precision, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, dimension(1:6), intent(in) :: range
    call assert(size(range) == 6 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3), &
        ' <crop_array_3d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4), &
        range(5):range(6)))
end subroutine crop_array_3d_double
subroutine crop_array_4d_double(w, range)
    double precision, allocatable, dimension(:, :, :, :), intent(inout) :: w
    integer, dimension(1:8), intent(in) :: range
    call assert(size(range) == 8 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3) &
        .and. range(7) >= lbound(w, 4) .and. range(8) <= ubound(w, 4), &
        ' <crop_array_4d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4), &
        range(5):range(6), &
        range(7):range(8)))
end subroutine crop_array_4d_double
function crop_1d_double(w, range) result(wt)
    double precision, dimension(:), intent(in) :: w
    integer, dimension(1:2), intent(in) :: range
    double precision, allocatable, dimension(:) :: wt
    call assert(size(range) == 2 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1), &
        ' <crop_array_1d> Error: range specification is wrong. ')
    call alloc_array(wt, [1, range(2) - range(1) + 1], &
        source=w(range(1):range(2)))
end function crop_1d_double
function crop_2d_double(w, range) result(wt)
    double precision, dimension(:, :), intent(in) :: w
    integer, dimension(1:4), intent(in) :: range
    double precision, allocatable, dimension(:, :) :: wt
    call assert(size(range) == 4 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2), &
        ' <crop_array_2d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4)))
end function crop_2d_double
function crop_3d_double(w, range) result(wt)
    double precision, dimension(:, :, :), intent(in) :: w
    integer, dimension(1:6), intent(in) :: range
    double precision, allocatable, dimension(:, :, :) :: wt
    call assert(size(range) == 6 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3), &
        ' <crop_array_3d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1, &
        1, range(6) - range(5) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4), &
        range(5):range(6)))
end function crop_3d_double
function crop_4d_double(w, range) result(wt)
    double precision, dimension(:, :, :, :), intent(in) :: w
    integer, dimension(1:8), intent(in) :: range
    double precision, allocatable, dimension(:, :, :, :) :: wt
    call assert(size(range) == 8 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3) &
        .and. range(7) >= lbound(w, 4) .and. range(8) <= ubound(w, 4), &
        ' <crop_array_4d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1, &
        1, range(6) - range(5) + 1, &
        1, range(8) - range(7) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4), &
        range(5):range(6), &
        range(7):range(8)))
end function crop_4d_double
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
subroutine crop_array_1d_complex(w, range)
    complex, allocatable, dimension(:), intent(inout) :: w
    integer, dimension(1:2), intent(in) :: range
    call assert(size(range) == 2 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1), &
        ' <crop_array_1d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2)))
end subroutine crop_array_1d_complex
subroutine crop_array_2d_complex(w, range)
    complex, allocatable, dimension(:, :), intent(inout) :: w
    integer, dimension(1:4), intent(in) :: range
    call assert(size(range) == 4 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2), &
        ' <crop_array_2d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4)))
end subroutine crop_array_2d_complex
subroutine crop_array_3d_complex(w, range)
    complex, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, dimension(1:6), intent(in) :: range
    call assert(size(range) == 6 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3), &
        ' <crop_array_3d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4), &
        range(5):range(6)))
end subroutine crop_array_3d_complex
subroutine crop_array_4d_complex(w, range)
    complex, allocatable, dimension(:, :, :, :), intent(inout) :: w
    integer, dimension(1:8), intent(in) :: range
    call assert(size(range) == 8 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3) &
        .and. range(7) >= lbound(w, 4) .and. range(8) <= ubound(w, 4), &
        ' <crop_array_4d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4), &
        range(5):range(6), &
        range(7):range(8)))
end subroutine crop_array_4d_complex
function crop_1d_complex(w, range) result(wt)
    complex, dimension(:), intent(in) :: w
    integer, dimension(1:2), intent(in) :: range
    complex, allocatable, dimension(:) :: wt
    call assert(size(range) == 2 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1), &
        ' <crop_array_1d> Error: range specification is wrong. ')
    call alloc_array(wt, [1, range(2) - range(1) + 1], &
        source=w(range(1):range(2)))
end function crop_1d_complex
function crop_2d_complex(w, range) result(wt)
    complex, dimension(:, :), intent(in) :: w
    integer, dimension(1:4), intent(in) :: range
    complex, allocatable, dimension(:, :) :: wt
    call assert(size(range) == 4 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2), &
        ' <crop_array_2d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4)))
end function crop_2d_complex
function crop_3d_complex(w, range) result(wt)
    complex, dimension(:, :, :), intent(in) :: w
    integer, dimension(1:6), intent(in) :: range
    complex, allocatable, dimension(:, :, :) :: wt
    call assert(size(range) == 6 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3), &
        ' <crop_array_3d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1, &
        1, range(6) - range(5) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4), &
        range(5):range(6)))
end function crop_3d_complex
function crop_4d_complex(w, range) result(wt)
    complex, dimension(:, :, :, :), intent(in) :: w
    integer, dimension(1:8), intent(in) :: range
    complex, allocatable, dimension(:, :, :, :) :: wt
    call assert(size(range) == 8 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3) &
        .and. range(7) >= lbound(w, 4) .and. range(8) <= ubound(w, 4), &
        ' <crop_array_4d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1, &
        1, range(6) - range(5) + 1, &
        1, range(8) - range(7) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4), &
        range(5):range(6), &
        range(7):range(8)))
end function crop_4d_complex
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
subroutine crop_array_1d_dcomplex(w, range)
    double complex, allocatable, dimension(:), intent(inout) :: w
    integer, dimension(1:2), intent(in) :: range
    call assert(size(range) == 2 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1), &
        ' <crop_array_1d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2)))
end subroutine crop_array_1d_dcomplex
subroutine crop_array_2d_dcomplex(w, range)
    double complex, allocatable, dimension(:, :), intent(inout) :: w
    integer, dimension(1:4), intent(in) :: range
    call assert(size(range) == 4 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2), &
        ' <crop_array_2d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4)))
end subroutine crop_array_2d_dcomplex
subroutine crop_array_3d_dcomplex(w, range)
    double complex, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, dimension(1:6), intent(in) :: range
    call assert(size(range) == 6 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3), &
        ' <crop_array_3d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4), &
        range(5):range(6)))
end subroutine crop_array_3d_dcomplex
subroutine crop_array_4d_dcomplex(w, range)
    double complex, allocatable, dimension(:, :, :, :), intent(inout) :: w
    integer, dimension(1:8), intent(in) :: range
    call assert(size(range) == 8 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3) &
        .and. range(7) >= lbound(w, 4) .and. range(8) <= ubound(w, 4), &
        ' <crop_array_4d> Error: range specification is wrong. ')
    call alloc_array(w, range, &
        source=w(range(1):range(2), &
        range(3):range(4), &
        range(5):range(6), &
        range(7):range(8)))
end subroutine crop_array_4d_dcomplex
function crop_1d_dcomplex(w, range) result(wt)
    double complex, dimension(:), intent(in) :: w
    integer, dimension(1:2), intent(in) :: range
    double complex, allocatable, dimension(:) :: wt
    call assert(size(range) == 2 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1), &
        ' <crop_array_1d> Error: range specification is wrong. ')
    call alloc_array(wt, [1, range(2) - range(1) + 1], &
        source=w(range(1):range(2)))
end function crop_1d_dcomplex
function crop_2d_dcomplex(w, range) result(wt)
    double complex, dimension(:, :), intent(in) :: w
    integer, dimension(1:4), intent(in) :: range
    double complex, allocatable, dimension(:, :) :: wt
    call assert(size(range) == 4 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2), &
        ' <crop_array_2d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4)))
end function crop_2d_dcomplex
function crop_3d_dcomplex(w, range) result(wt)
    double complex, dimension(:, :, :), intent(in) :: w
    integer, dimension(1:6), intent(in) :: range
    double complex, allocatable, dimension(:, :, :) :: wt
    call assert(size(range) == 6 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3), &
        ' <crop_array_3d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1, &
        1, range(6) - range(5) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4), &
        range(5):range(6)))
end function crop_3d_dcomplex
function crop_4d_dcomplex(w, range) result(wt)
    double complex, dimension(:, :, :, :), intent(in) :: w
    integer, dimension(1:8), intent(in) :: range
    double complex, allocatable, dimension(:, :, :, :) :: wt
    call assert(size(range) == 8 &
        .and. range(1) >= lbound(w, 1) .and. range(2) <= ubound(w, 1) &
        .and. range(3) >= lbound(w, 2) .and. range(4) <= ubound(w, 2) &
        .and. range(5) >= lbound(w, 3) .and. range(6) <= ubound(w, 3) &
        .and. range(7) >= lbound(w, 4) .and. range(8) <= ubound(w, 4), &
        ' <crop_array_4d> Error: range specification is wrong. ')
    call alloc_array(wt, [ &
        1, range(2) - range(1) + 1, &
        1, range(4) - range(3) + 1, &
        1, range(6) - range(5) + 1, &
        1, range(8) - range(7) + 1], &
        source=w( &
        range(1):range(2), &
        range(3):range(4), &
        range(5):range(6), &
        range(7):range(8)))
end function crop_4d_dcomplex
! #define T logical
! #define TT logical
! #include "template_crop.f90"
    !================================================================
    ! Flatten array
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
function flatten_2d_int(w) result(wr)
    integer :: w(:, :)
    integer, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)))
    wr = reshape(w, [size(wr)])
end function flatten_2d_int
function flatten_3d_int(w) result(wr)
    integer :: w(:, :, :)
    integer, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)*size(w, 3)))
    wr = reshape(w, [size(wr)])
end function flatten_3d_int
function flatten_4d_int(w) result(wr)
    integer :: w(:, :, :, :)
    integer, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)*size(w, 3)*size(w, 4)))
    wr = reshape(w, [size(wr)])
end function flatten_4d_int
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
function flatten_2d_float(w) result(wr)
    real :: w(:, :)
    real, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)))
    wr = reshape(w, [size(wr)])
end function flatten_2d_float
function flatten_3d_float(w) result(wr)
    real :: w(:, :, :)
    real, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)*size(w, 3)))
    wr = reshape(w, [size(wr)])
end function flatten_3d_float
function flatten_4d_float(w) result(wr)
    real :: w(:, :, :, :)
    real, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)*size(w, 3)*size(w, 4)))
    wr = reshape(w, [size(wr)])
end function flatten_4d_float
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
function flatten_2d_double(w) result(wr)
    double precision :: w(:, :)
    double precision, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)))
    wr = reshape(w, [size(wr)])
end function flatten_2d_double
function flatten_3d_double(w) result(wr)
    double precision :: w(:, :, :)
    double precision, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)*size(w, 3)))
    wr = reshape(w, [size(wr)])
end function flatten_3d_double
function flatten_4d_double(w) result(wr)
    double precision :: w(:, :, :, :)
    double precision, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)*size(w, 3)*size(w, 4)))
    wr = reshape(w, [size(wr)])
end function flatten_4d_double
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
function flatten_2d_complex(w) result(wr)
    complex :: w(:, :)
    complex, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)))
    wr = reshape(w, [size(wr)])
end function flatten_2d_complex
function flatten_3d_complex(w) result(wr)
    complex :: w(:, :, :)
    complex, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)*size(w, 3)))
    wr = reshape(w, [size(wr)])
end function flatten_3d_complex
function flatten_4d_complex(w) result(wr)
    complex :: w(:, :, :, :)
    complex, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)*size(w, 3)*size(w, 4)))
    wr = reshape(w, [size(wr)])
end function flatten_4d_complex
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
function flatten_2d_dcomplex(w) result(wr)
    double complex :: w(:, :)
    double complex, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)))
    wr = reshape(w, [size(wr)])
end function flatten_2d_dcomplex
function flatten_3d_dcomplex(w) result(wr)
    double complex :: w(:, :, :)
    double complex, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)*size(w, 3)))
    wr = reshape(w, [size(wr)])
end function flatten_3d_dcomplex
function flatten_4d_dcomplex(w) result(wr)
    double complex :: w(:, :, :, :)
    double complex, allocatable :: wr(:)
    allocate (wr(1:size(w, 1)*size(w, 2)*size(w, 3)*size(w, 4)))
    wr = reshape(w, [size(wr)])
end function flatten_4d_dcomplex
! #define T logical
! #define TT logical
! #include "template_flatten.f90"
    !================================================================
    ! Flip array
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
function flip_1d_int(w) result(wf)
    integer, dimension(:), intent(in) :: w
    integer, allocatable, dimension(:) :: wf
    integer :: n1
    n1 = size(w, 1)
    allocate (wf(1:n1))
    wf = w(n1:1:-1)
end function flip_1d_int
function flip_2d_int(w, axis) result(wf)
    integer, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in) :: axis
    integer, allocatable, dimension(:, :) :: wf
    integer :: n1, n2, nf
    integer, allocatable, dimension(:, :) :: f
    integer :: i, r(1:3)
    n1 = size(w, 1)
    n2 = size(w, 2)
    nf = size(axis)
    allocate (wf(1:n1, 1:n2))
    allocate (f(1:2, 1:3))
    f(1, :) = [1, n1, 1]
    f(2, :) = [1, n2, 1]
    do i = 1, size(axis)
        select case (axis(i))
            case (1)
                r = f(1, :)
                f(1, :) = [r(2), r(1), -r(3)]
            case (2)
                r = f(2, :)
                f(2, :) = [r(2), r(1), -r(3)]
        end select
    end do
    wf = w(f(1, 1):f(1, 2):f(1, 3), f(2, 1):f(2, 2):f(2, 3))
end function flip_2d_int
function flip_3d_int(w, axis) result(wf)
    integer, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: axis
    integer, allocatable, dimension(:, :, :) :: wf
    integer :: n1, n2, n3, nf
    integer, allocatable, dimension(:, :) :: f
    integer :: i, r(1:3)
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    nf = size(axis)
    allocate (wf(1:n1, 1:n2, 1:n3))
    allocate (f(1:3, 1:3))
    f(1, :) = [1, n1, 1]
    f(2, :) = [1, n2, 1]
    f(3, :) = [1, n3, 1]
    do i = 1, size(axis)
        select case (axis(i))
            case (1)
                r = f(1, :)
                f(1, :) = [r(2), r(1), -r(3)]
            case (2)
                r = f(2, :)
                f(2, :) = [r(2), r(1), -r(3)]
            case (3)
                r = f(3, :)
                f(3, :) = [r(2), r(1), -r(3)]
        end select
    end do
    wf = w(f(1, 1):f(1, 2):f(1, 3), &
        f(2, 1):f(2, 2):f(2, 3), &
        f(3, 1):f(3, 2):f(3, 3))
end function flip_3d_int
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
function flip_1d_float(w) result(wf)
    real, dimension(:), intent(in) :: w
    real, allocatable, dimension(:) :: wf
    integer :: n1
    n1 = size(w, 1)
    allocate (wf(1:n1))
    wf = w(n1:1:-1)
end function flip_1d_float
function flip_2d_float(w, axis) result(wf)
    real, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in) :: axis
    real, allocatable, dimension(:, :) :: wf
    integer :: n1, n2, nf
    integer, allocatable, dimension(:, :) :: f
    integer :: i, r(1:3)
    n1 = size(w, 1)
    n2 = size(w, 2)
    nf = size(axis)
    allocate (wf(1:n1, 1:n2))
    allocate (f(1:2, 1:3))
    f(1, :) = [1, n1, 1]
    f(2, :) = [1, n2, 1]
    do i = 1, size(axis)
        select case (axis(i))
            case (1)
                r = f(1, :)
                f(1, :) = [r(2), r(1), -r(3)]
            case (2)
                r = f(2, :)
                f(2, :) = [r(2), r(1), -r(3)]
        end select
    end do
    wf = w(f(1, 1):f(1, 2):f(1, 3), f(2, 1):f(2, 2):f(2, 3))
end function flip_2d_float
function flip_3d_float(w, axis) result(wf)
    real, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: axis
    real, allocatable, dimension(:, :, :) :: wf
    integer :: n1, n2, n3, nf
    integer, allocatable, dimension(:, :) :: f
    integer :: i, r(1:3)
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    nf = size(axis)
    allocate (wf(1:n1, 1:n2, 1:n3))
    allocate (f(1:3, 1:3))
    f(1, :) = [1, n1, 1]
    f(2, :) = [1, n2, 1]
    f(3, :) = [1, n3, 1]
    do i = 1, size(axis)
        select case (axis(i))
            case (1)
                r = f(1, :)
                f(1, :) = [r(2), r(1), -r(3)]
            case (2)
                r = f(2, :)
                f(2, :) = [r(2), r(1), -r(3)]
            case (3)
                r = f(3, :)
                f(3, :) = [r(2), r(1), -r(3)]
        end select
    end do
    wf = w(f(1, 1):f(1, 2):f(1, 3), &
        f(2, 1):f(2, 2):f(2, 3), &
        f(3, 1):f(3, 2):f(3, 3))
end function flip_3d_float
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
function flip_1d_double(w) result(wf)
    double precision, dimension(:), intent(in) :: w
    double precision, allocatable, dimension(:) :: wf
    integer :: n1
    n1 = size(w, 1)
    allocate (wf(1:n1))
    wf = w(n1:1:-1)
end function flip_1d_double
function flip_2d_double(w, axis) result(wf)
    double precision, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in) :: axis
    double precision, allocatable, dimension(:, :) :: wf
    integer :: n1, n2, nf
    integer, allocatable, dimension(:, :) :: f
    integer :: i, r(1:3)
    n1 = size(w, 1)
    n2 = size(w, 2)
    nf = size(axis)
    allocate (wf(1:n1, 1:n2))
    allocate (f(1:2, 1:3))
    f(1, :) = [1, n1, 1]
    f(2, :) = [1, n2, 1]
    do i = 1, size(axis)
        select case (axis(i))
            case (1)
                r = f(1, :)
                f(1, :) = [r(2), r(1), -r(3)]
            case (2)
                r = f(2, :)
                f(2, :) = [r(2), r(1), -r(3)]
        end select
    end do
    wf = w(f(1, 1):f(1, 2):f(1, 3), f(2, 1):f(2, 2):f(2, 3))
end function flip_2d_double
function flip_3d_double(w, axis) result(wf)
    double precision, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: axis
    double precision, allocatable, dimension(:, :, :) :: wf
    integer :: n1, n2, n3, nf
    integer, allocatable, dimension(:, :) :: f
    integer :: i, r(1:3)
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    nf = size(axis)
    allocate (wf(1:n1, 1:n2, 1:n3))
    allocate (f(1:3, 1:3))
    f(1, :) = [1, n1, 1]
    f(2, :) = [1, n2, 1]
    f(3, :) = [1, n3, 1]
    do i = 1, size(axis)
        select case (axis(i))
            case (1)
                r = f(1, :)
                f(1, :) = [r(2), r(1), -r(3)]
            case (2)
                r = f(2, :)
                f(2, :) = [r(2), r(1), -r(3)]
            case (3)
                r = f(3, :)
                f(3, :) = [r(2), r(1), -r(3)]
        end select
    end do
    wf = w(f(1, 1):f(1, 2):f(1, 3), &
        f(2, 1):f(2, 2):f(2, 3), &
        f(3, 1):f(3, 2):f(3, 3))
end function flip_3d_double
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
function flip_1d_complex(w) result(wf)
    complex, dimension(:), intent(in) :: w
    complex, allocatable, dimension(:) :: wf
    integer :: n1
    n1 = size(w, 1)
    allocate (wf(1:n1))
    wf = w(n1:1:-1)
end function flip_1d_complex
function flip_2d_complex(w, axis) result(wf)
    complex, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in) :: axis
    complex, allocatable, dimension(:, :) :: wf
    integer :: n1, n2, nf
    integer, allocatable, dimension(:, :) :: f
    integer :: i, r(1:3)
    n1 = size(w, 1)
    n2 = size(w, 2)
    nf = size(axis)
    allocate (wf(1:n1, 1:n2))
    allocate (f(1:2, 1:3))
    f(1, :) = [1, n1, 1]
    f(2, :) = [1, n2, 1]
    do i = 1, size(axis)
        select case (axis(i))
            case (1)
                r = f(1, :)
                f(1, :) = [r(2), r(1), -r(3)]
            case (2)
                r = f(2, :)
                f(2, :) = [r(2), r(1), -r(3)]
        end select
    end do
    wf = w(f(1, 1):f(1, 2):f(1, 3), f(2, 1):f(2, 2):f(2, 3))
end function flip_2d_complex
function flip_3d_complex(w, axis) result(wf)
    complex, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: axis
    complex, allocatable, dimension(:, :, :) :: wf
    integer :: n1, n2, n3, nf
    integer, allocatable, dimension(:, :) :: f
    integer :: i, r(1:3)
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    nf = size(axis)
    allocate (wf(1:n1, 1:n2, 1:n3))
    allocate (f(1:3, 1:3))
    f(1, :) = [1, n1, 1]
    f(2, :) = [1, n2, 1]
    f(3, :) = [1, n3, 1]
    do i = 1, size(axis)
        select case (axis(i))
            case (1)
                r = f(1, :)
                f(1, :) = [r(2), r(1), -r(3)]
            case (2)
                r = f(2, :)
                f(2, :) = [r(2), r(1), -r(3)]
            case (3)
                r = f(3, :)
                f(3, :) = [r(2), r(1), -r(3)]
        end select
    end do
    wf = w(f(1, 1):f(1, 2):f(1, 3), &
        f(2, 1):f(2, 2):f(2, 3), &
        f(3, 1):f(3, 2):f(3, 3))
end function flip_3d_complex
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
function flip_1d_dcomplex(w) result(wf)
    double complex, dimension(:), intent(in) :: w
    double complex, allocatable, dimension(:) :: wf
    integer :: n1
    n1 = size(w, 1)
    allocate (wf(1:n1))
    wf = w(n1:1:-1)
end function flip_1d_dcomplex
function flip_2d_dcomplex(w, axis) result(wf)
    double complex, dimension(:, :), intent(in) :: w
    integer, dimension(:), intent(in) :: axis
    double complex, allocatable, dimension(:, :) :: wf
    integer :: n1, n2, nf
    integer, allocatable, dimension(:, :) :: f
    integer :: i, r(1:3)
    n1 = size(w, 1)
    n2 = size(w, 2)
    nf = size(axis)
    allocate (wf(1:n1, 1:n2))
    allocate (f(1:2, 1:3))
    f(1, :) = [1, n1, 1]
    f(2, :) = [1, n2, 1]
    do i = 1, size(axis)
        select case (axis(i))
            case (1)
                r = f(1, :)
                f(1, :) = [r(2), r(1), -r(3)]
            case (2)
                r = f(2, :)
                f(2, :) = [r(2), r(1), -r(3)]
        end select
    end do
    wf = w(f(1, 1):f(1, 2):f(1, 3), f(2, 1):f(2, 2):f(2, 3))
end function flip_2d_dcomplex
function flip_3d_dcomplex(w, axis) result(wf)
    double complex, dimension(:, :, :), intent(in) :: w
    integer, dimension(:), intent(in) :: axis
    double complex, allocatable, dimension(:, :, :) :: wf
    integer :: n1, n2, n3, nf
    integer, allocatable, dimension(:, :) :: f
    integer :: i, r(1:3)
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    nf = size(axis)
    allocate (wf(1:n1, 1:n2, 1:n3))
    allocate (f(1:3, 1:3))
    f(1, :) = [1, n1, 1]
    f(2, :) = [1, n2, 1]
    f(3, :) = [1, n3, 1]
    do i = 1, size(axis)
        select case (axis(i))
            case (1)
                r = f(1, :)
                f(1, :) = [r(2), r(1), -r(3)]
            case (2)
                r = f(2, :)
                f(2, :) = [r(2), r(1), -r(3)]
            case (3)
                r = f(3, :)
                f(3, :) = [r(2), r(1), -r(3)]
        end select
    end do
    wf = w(f(1, 1):f(1, 2):f(1, 3), &
        f(2, 1):f(2, 2):f(2, 3), &
        f(3, 1):f(3, 2):f(3, 3))
end function flip_3d_dcomplex
! #define T logical
! #define TT logical
! #include "template_flip.f90"
    !================================================================
    ! If-then-else ternary operation for scalar and array
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
pure function ifelse_int(condition, a, b) result(c)
    logical, intent(in) :: condition
    integer, intent(in) :: a, b
    integer :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_1d_int(condition, a, b) result(c)
    logical, intent(in) :: condition
    integer, dimension(:), intent(in) :: a, b
    integer, allocatable, dimension(:) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_2d_int(condition, a, b) result(c)
    logical, intent(in) :: condition
    integer, dimension(:, :), intent(in) :: a, b
    integer, allocatable, dimension(:, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_3d_int(condition, a, b) result(c)
    logical, intent(in) :: condition
    integer, dimension(:, :, :), intent(in) :: a, b
    integer, allocatable, dimension(:, :, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_4d_int(condition, a, b) result(c)
    logical, intent(in) :: condition
    integer, dimension(:, :, :, :), intent(in) :: a, b
    integer, allocatable, dimension(:, :, :, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
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
pure function ifelse_float(condition, a, b) result(c)
    logical, intent(in) :: condition
    real, intent(in) :: a, b
    real :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_1d_float(condition, a, b) result(c)
    logical, intent(in) :: condition
    real, dimension(:), intent(in) :: a, b
    real, allocatable, dimension(:) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_2d_float(condition, a, b) result(c)
    logical, intent(in) :: condition
    real, dimension(:, :), intent(in) :: a, b
    real, allocatable, dimension(:, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_3d_float(condition, a, b) result(c)
    logical, intent(in) :: condition
    real, dimension(:, :, :), intent(in) :: a, b
    real, allocatable, dimension(:, :, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_4d_float(condition, a, b) result(c)
    logical, intent(in) :: condition
    real, dimension(:, :, :, :), intent(in) :: a, b
    real, allocatable, dimension(:, :, :, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
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
pure function ifelse_double(condition, a, b) result(c)
    logical, intent(in) :: condition
    double precision, intent(in) :: a, b
    double precision :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_1d_double(condition, a, b) result(c)
    logical, intent(in) :: condition
    double precision, dimension(:), intent(in) :: a, b
    double precision, allocatable, dimension(:) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_2d_double(condition, a, b) result(c)
    logical, intent(in) :: condition
    double precision, dimension(:, :), intent(in) :: a, b
    double precision, allocatable, dimension(:, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_3d_double(condition, a, b) result(c)
    logical, intent(in) :: condition
    double precision, dimension(:, :, :), intent(in) :: a, b
    double precision, allocatable, dimension(:, :, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_4d_double(condition, a, b) result(c)
    logical, intent(in) :: condition
    double precision, dimension(:, :, :, :), intent(in) :: a, b
    double precision, allocatable, dimension(:, :, :, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
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
pure function ifelse_complex(condition, a, b) result(c)
    logical, intent(in) :: condition
    complex, intent(in) :: a, b
    complex :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_1d_complex(condition, a, b) result(c)
    logical, intent(in) :: condition
    complex, dimension(:), intent(in) :: a, b
    complex, allocatable, dimension(:) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_2d_complex(condition, a, b) result(c)
    logical, intent(in) :: condition
    complex, dimension(:, :), intent(in) :: a, b
    complex, allocatable, dimension(:, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_3d_complex(condition, a, b) result(c)
    logical, intent(in) :: condition
    complex, dimension(:, :, :), intent(in) :: a, b
    complex, allocatable, dimension(:, :, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_4d_complex(condition, a, b) result(c)
    logical, intent(in) :: condition
    complex, dimension(:, :, :, :), intent(in) :: a, b
    complex, allocatable, dimension(:, :, :, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
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
pure function ifelse_dcomplex(condition, a, b) result(c)
    logical, intent(in) :: condition
    double complex, intent(in) :: a, b
    double complex :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_1d_dcomplex(condition, a, b) result(c)
    logical, intent(in) :: condition
    double complex, dimension(:), intent(in) :: a, b
    double complex, allocatable, dimension(:) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_2d_dcomplex(condition, a, b) result(c)
    logical, intent(in) :: condition
    double complex, dimension(:, :), intent(in) :: a, b
    double complex, allocatable, dimension(:, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_3d_dcomplex(condition, a, b) result(c)
    logical, intent(in) :: condition
    double complex, dimension(:, :, :), intent(in) :: a, b
    double complex, allocatable, dimension(:, :, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
pure function ifelse_4d_dcomplex(condition, a, b) result(c)
    logical, intent(in) :: condition
    double complex, dimension(:, :, :, :), intent(in) :: a, b
    double complex, allocatable, dimension(:, :, :, :) :: c
    if (condition) then
        c = a
    else
        c = b
    end if
end function
! #define T logical
! #define TT logical
! #include "template_ifelse.f90"
    ! String requires special routine
    function ifelse_string(condition, a, b) result(c)
        logical :: condition
        character(len=*) :: a, b
        character(:), allocatable :: c
        if (condition) then
            c = a
        else
            c = b
        end if
    end function ifelse_string
    !================================================================
    ! Mask array
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
subroutine mask_array_1d_int(w, mask, zero_to_nan)
    integer, dimension(:), intent(inout) :: w
    integer, dimension(:), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_1d_int
subroutine mask_array_2d_int(w, mask, zero_to_nan)
    integer, dimension(:, :), intent(inout) :: w
    integer, dimension(:, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_2d_int
subroutine mask_array_3d_int(w, mask, zero_to_nan)
    integer, dimension(:, :, :), intent(inout) :: w
    integer, dimension(:, :, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_3d_int
subroutine mask_array_4d_int(w, mask, zero_to_nan)
    integer, dimension(:, :, :, :), intent(inout) :: w
    integer, dimension(:, :, :, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_4d_int
function mask_1d_int(w, mask, zero_to_nan) result(wm)
    integer, dimension(:), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    integer, allocatable, dimension(:) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_1d_int
function mask_2d_int(w, mask, zero_to_nan) result(wm)
    integer, dimension(:, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    integer, allocatable, dimension(:, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_2d_int
function mask_3d_int(w, mask, zero_to_nan) result(wm)
    integer, dimension(:, :, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    integer, allocatable, dimension(:, :, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_3d_int
function mask_4d_int(w, mask, zero_to_nan) result(wm)
    integer, dimension(:, :, :, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    integer, allocatable, dimension(:, :, :, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_4d_int
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
subroutine mask_array_1d_float(w, mask, zero_to_nan)
    real, dimension(:), intent(inout) :: w
    real, dimension(:), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_1d_float
subroutine mask_array_2d_float(w, mask, zero_to_nan)
    real, dimension(:, :), intent(inout) :: w
    real, dimension(:, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_2d_float
subroutine mask_array_3d_float(w, mask, zero_to_nan)
    real, dimension(:, :, :), intent(inout) :: w
    real, dimension(:, :, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_3d_float
subroutine mask_array_4d_float(w, mask, zero_to_nan)
    real, dimension(:, :, :, :), intent(inout) :: w
    real, dimension(:, :, :, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_4d_float
function mask_1d_float(w, mask, zero_to_nan) result(wm)
    real, dimension(:), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    real, allocatable, dimension(:) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_1d_float
function mask_2d_float(w, mask, zero_to_nan) result(wm)
    real, dimension(:, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    real, allocatable, dimension(:, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_2d_float
function mask_3d_float(w, mask, zero_to_nan) result(wm)
    real, dimension(:, :, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    real, allocatable, dimension(:, :, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_3d_float
function mask_4d_float(w, mask, zero_to_nan) result(wm)
    real, dimension(:, :, :, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    real, allocatable, dimension(:, :, :, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_4d_float
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
subroutine mask_array_1d_double(w, mask, zero_to_nan)
    double precision, dimension(:), intent(inout) :: w
    double precision, dimension(:), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_1d_double
subroutine mask_array_2d_double(w, mask, zero_to_nan)
    double precision, dimension(:, :), intent(inout) :: w
    double precision, dimension(:, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_2d_double
subroutine mask_array_3d_double(w, mask, zero_to_nan)
    double precision, dimension(:, :, :), intent(inout) :: w
    double precision, dimension(:, :, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_3d_double
subroutine mask_array_4d_double(w, mask, zero_to_nan)
    double precision, dimension(:, :, :, :), intent(inout) :: w
    double precision, dimension(:, :, :, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_4d_double
function mask_1d_double(w, mask, zero_to_nan) result(wm)
    double precision, dimension(:), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    double precision, allocatable, dimension(:) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_1d_double
function mask_2d_double(w, mask, zero_to_nan) result(wm)
    double precision, dimension(:, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    double precision, allocatable, dimension(:, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_2d_double
function mask_3d_double(w, mask, zero_to_nan) result(wm)
    double precision, dimension(:, :, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    double precision, allocatable, dimension(:, :, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_3d_double
function mask_4d_double(w, mask, zero_to_nan) result(wm)
    double precision, dimension(:, :, :, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    double precision, allocatable, dimension(:, :, :, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_4d_double
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
subroutine mask_array_1d_complex(w, mask, zero_to_nan)
    complex, dimension(:), intent(inout) :: w
    complex, dimension(:), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_1d_complex
subroutine mask_array_2d_complex(w, mask, zero_to_nan)
    complex, dimension(:, :), intent(inout) :: w
    complex, dimension(:, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_2d_complex
subroutine mask_array_3d_complex(w, mask, zero_to_nan)
    complex, dimension(:, :, :), intent(inout) :: w
    complex, dimension(:, :, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_3d_complex
subroutine mask_array_4d_complex(w, mask, zero_to_nan)
    complex, dimension(:, :, :, :), intent(inout) :: w
    complex, dimension(:, :, :, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_4d_complex
function mask_1d_complex(w, mask, zero_to_nan) result(wm)
    complex, dimension(:), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    complex, allocatable, dimension(:) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_1d_complex
function mask_2d_complex(w, mask, zero_to_nan) result(wm)
    complex, dimension(:, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    complex, allocatable, dimension(:, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_2d_complex
function mask_3d_complex(w, mask, zero_to_nan) result(wm)
    complex, dimension(:, :, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    complex, allocatable, dimension(:, :, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_3d_complex
function mask_4d_complex(w, mask, zero_to_nan) result(wm)
    complex, dimension(:, :, :, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    complex, allocatable, dimension(:, :, :, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_4d_complex
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
subroutine mask_array_1d_dcomplex(w, mask, zero_to_nan)
    double complex, dimension(:), intent(inout) :: w
    double complex, dimension(:), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_1d_dcomplex
subroutine mask_array_2d_dcomplex(w, mask, zero_to_nan)
    double complex, dimension(:, :), intent(inout) :: w
    double complex, dimension(:, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_2d_dcomplex
subroutine mask_array_3d_dcomplex(w, mask, zero_to_nan)
    double complex, dimension(:, :, :), intent(inout) :: w
    double complex, dimension(:, :, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_3d_dcomplex
subroutine mask_array_4d_dcomplex(w, mask, zero_to_nan)
    double complex, dimension(:, :, :, :), intent(inout) :: w
    double complex, dimension(:, :, :, :), intent(in) :: mask
    logical, intent(in), optional :: zero_to_nan
    w = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                w = nan()
            end where
        end if
    end if
end subroutine mask_array_4d_dcomplex
function mask_1d_dcomplex(w, mask, zero_to_nan) result(wm)
    double complex, dimension(:), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    double complex, allocatable, dimension(:) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_1d_dcomplex
function mask_2d_dcomplex(w, mask, zero_to_nan) result(wm)
    double complex, dimension(:, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    double complex, allocatable, dimension(:, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_2d_dcomplex
function mask_3d_dcomplex(w, mask, zero_to_nan) result(wm)
    double complex, dimension(:, :, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    double complex, allocatable, dimension(:, :, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_3d_dcomplex
function mask_4d_dcomplex(w, mask, zero_to_nan) result(wm)
    double complex, dimension(:, :, :, :), intent(in) :: w, mask
    logical, intent(in), optional :: zero_to_nan
    double complex, allocatable, dimension(:, :, :, :) :: wm
    wm = w*mask
    if (present(zero_to_nan)) then
        if (zero_to_nan) then
            where (mask == 0)
                wm = nan()
            end where
        end if
    end if
end function mask_4d_dcomplex
! #define T logical
! #define TT logical
! #include "template_mask.f90"
    !================================================================
    ! Pad array
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
!#define pad_array_4d_ pad_array_4d_int
!
!#define pad_4d_ pad_4d_int
subroutine pad_array_1d_int(w, pad, method, const)
    ! arguments
    integer, allocatable, dimension(:), intent(inout) :: w
    integer, dimension(1:2), intent(in) :: pad
    character(len=*), dimension(1:2), intent(in), optional :: method
    integer, intent(in), optional :: const
    ! local variables
    integer :: i
    integer :: n1beg, n1end
    integer, allocatable, dimension(:) :: wp
    integer :: l1, u1
    character(len=16), dimension(1:2) :: pad_method
    integer :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    l1 = pad(1)
    u1 = pad(2)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1))
    do i = n1beg, n1end
        wp(i) = w(i)
    end do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_1d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg)
            end do
        case ('symm')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg + i - 1)
            end do
        case ('const')
            do i = 1, l1
                wp(n1beg - i) = pad_const
            end do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            do i = 1, u1
                wp(n1end + i) = wp(n1end)
            end do
        case ('symm')
            do i = 1, u1
                wp(n1end + i) = wp(n1end - i + 1)
            end do
        case ('const')
            do i = 1, u1
                wp(n1end + i) = pad_const
            end do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_1d_int
subroutine pad_array_2d_int(w, pad, method, const)
    ! arguments
    integer, allocatable, dimension(:, :), intent(inout) :: w
    integer, dimension(1:4), intent(in) :: pad
    character(len=*), dimension(1:4), intent(in), optional :: method
    integer, intent(in), optional :: const
    ! local variables
    integer :: i, j
    integer :: n1beg, n1end, n2beg, n2end
    integer, allocatable, dimension(:, :) :: wp
    integer :: l1, u1, l2, u2
    character(len=16), dimension(1:4) :: pad_method
    integer :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2))
    !$omp parallel do private(i, j) collapse(2)
    do j = n2beg, n2end
        do i = n1beg, n1end
            wp(i, j) = w(i, j)
        end do
    end do
    !$omp end parallel do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_2d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg + i - 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end - i + 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 lower boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg + j - 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 upper boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end - j + 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_2d_int
subroutine pad_array_3d_int(w, pad, method, const)
    ! arguments
    integer, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, dimension(1:6), intent(in) :: pad
    character(len=*), dimension(1:6), intent(in), optional :: method
    integer, intent(in), optional :: const
    ! local variables
    integer :: i, j, k
    integer :: n1beg, n1end, n2beg, n2end, n3beg, n3end
    integer :: l1, u1, l2, u2, l3, u3
    integer, allocatable, dimension(:, :, :) :: wp
    character(len=16), dimension(1:6) :: pad_method
    integer :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    n3beg = lbound(w, 3)
    n3end = ubound(w, 3)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    l3 = pad(5)
    u3 = pad(6)
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_3d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2, n3beg - l3:n3end + u3))
    !$omp parallel do private(i, j, k) collapse(3)
    do k = n3beg, n3end
        do j = n2beg, n2end
            do i = n1beg, n1end
                wp(i, j, k) = w(i, j, k)
            end do
        end do
    end do
    !$omp end parallel do
    ! Left boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg + i - 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Right boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end - i + 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Front boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg + j - 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Back boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end - j + 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Top boundary
    select case (pad_method(5))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg + k - 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Bottom boundary
    select case (pad_method(6))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end - k + 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate array
    w = wp
end subroutine pad_array_3d_int
function pad_1d_int(w, pad, method, const) result(wp)
    ! arguments
    integer, dimension(:) :: w
    integer, dimension(1:2) :: pad
    character(len=*), dimension(1:2), optional :: method
    integer, optional :: const
    integer, allocatable, dimension(:) :: wp
    ! local variables
    character(len=16), dimension(1:2) :: pad_method
    integer :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    wp = w
    call pad_array_1d_int(wp, pad, pad_method, pad_const)
end function pad_1d_int
function pad_2d_int(w, pad, method, const) result(wp)
    ! arguments
    integer, dimension(:, :) :: w
    integer, dimension(1:4) :: pad
    character(len=*), dimension(1:4), optional :: method
    integer, optional :: const
    integer, allocatable, dimension(:, :) :: wp
    ! local variables
    character(len=16), dimension(1:4) :: pad_method
    integer :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_2d_int(wp, pad, pad_method, pad_const)
end function pad_2d_int
function pad_3d_int(w, pad, method, const) result(wp)
    ! arguments
    integer, dimension(:, :, :) :: w
    integer, dimension(1:6) :: pad
    character(len=*), dimension(1:6), optional :: method
    integer, optional :: const
    integer, allocatable, dimension(:, :, :) :: wp
    ! local variables
    character(len=16), dimension(1:6) :: pad_method
    integer :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_3d_int(wp, pad, pad_method, pad_const)
end function pad_3d_int
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
!#define pad_array_4d_ pad_array_4d_float
!
!#define pad_4d_ pad_4d_float
subroutine pad_array_1d_float(w, pad, method, const)
    ! arguments
    real, allocatable, dimension(:), intent(inout) :: w
    integer, dimension(1:2), intent(in) :: pad
    character(len=*), dimension(1:2), intent(in), optional :: method
    real, intent(in), optional :: const
    ! local variables
    integer :: i
    integer :: n1beg, n1end
    real, allocatable, dimension(:) :: wp
    integer :: l1, u1
    character(len=16), dimension(1:2) :: pad_method
    real :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    l1 = pad(1)
    u1 = pad(2)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1))
    do i = n1beg, n1end
        wp(i) = w(i)
    end do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_1d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg)
            end do
        case ('symm')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg + i - 1)
            end do
        case ('const')
            do i = 1, l1
                wp(n1beg - i) = pad_const
            end do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            do i = 1, u1
                wp(n1end + i) = wp(n1end)
            end do
        case ('symm')
            do i = 1, u1
                wp(n1end + i) = wp(n1end - i + 1)
            end do
        case ('const')
            do i = 1, u1
                wp(n1end + i) = pad_const
            end do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_1d_float
subroutine pad_array_2d_float(w, pad, method, const)
    ! arguments
    real, allocatable, dimension(:, :), intent(inout) :: w
    integer, dimension(1:4), intent(in) :: pad
    character(len=*), dimension(1:4), intent(in), optional :: method
    real, intent(in), optional :: const
    ! local variables
    integer :: i, j
    integer :: n1beg, n1end, n2beg, n2end
    real, allocatable, dimension(:, :) :: wp
    integer :: l1, u1, l2, u2
    character(len=16), dimension(1:4) :: pad_method
    real :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2))
    !$omp parallel do private(i, j) collapse(2)
    do j = n2beg, n2end
        do i = n1beg, n1end
            wp(i, j) = w(i, j)
        end do
    end do
    !$omp end parallel do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_2d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg + i - 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end - i + 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 lower boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg + j - 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 upper boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end - j + 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_2d_float
subroutine pad_array_3d_float(w, pad, method, const)
    ! arguments
    real, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, dimension(1:6), intent(in) :: pad
    character(len=*), dimension(1:6), intent(in), optional :: method
    real, intent(in), optional :: const
    ! local variables
    integer :: i, j, k
    integer :: n1beg, n1end, n2beg, n2end, n3beg, n3end
    integer :: l1, u1, l2, u2, l3, u3
    real, allocatable, dimension(:, :, :) :: wp
    character(len=16), dimension(1:6) :: pad_method
    real :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    n3beg = lbound(w, 3)
    n3end = ubound(w, 3)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    l3 = pad(5)
    u3 = pad(6)
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_3d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2, n3beg - l3:n3end + u3))
    !$omp parallel do private(i, j, k) collapse(3)
    do k = n3beg, n3end
        do j = n2beg, n2end
            do i = n1beg, n1end
                wp(i, j, k) = w(i, j, k)
            end do
        end do
    end do
    !$omp end parallel do
    ! Left boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg + i - 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Right boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end - i + 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Front boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg + j - 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Back boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end - j + 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Top boundary
    select case (pad_method(5))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg + k - 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Bottom boundary
    select case (pad_method(6))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end - k + 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate array
    w = wp
end subroutine pad_array_3d_float
function pad_1d_float(w, pad, method, const) result(wp)
    ! arguments
    real, dimension(:) :: w
    integer, dimension(1:2) :: pad
    character(len=*), dimension(1:2), optional :: method
    real, optional :: const
    real, allocatable, dimension(:) :: wp
    ! local variables
    character(len=16), dimension(1:2) :: pad_method
    real :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    wp = w
    call pad_array_1d_float(wp, pad, pad_method, pad_const)
end function pad_1d_float
function pad_2d_float(w, pad, method, const) result(wp)
    ! arguments
    real, dimension(:, :) :: w
    integer, dimension(1:4) :: pad
    character(len=*), dimension(1:4), optional :: method
    real, optional :: const
    real, allocatable, dimension(:, :) :: wp
    ! local variables
    character(len=16), dimension(1:4) :: pad_method
    real :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_2d_float(wp, pad, pad_method, pad_const)
end function pad_2d_float
function pad_3d_float(w, pad, method, const) result(wp)
    ! arguments
    real, dimension(:, :, :) :: w
    integer, dimension(1:6) :: pad
    character(len=*), dimension(1:6), optional :: method
    real, optional :: const
    real, allocatable, dimension(:, :, :) :: wp
    ! local variables
    character(len=16), dimension(1:6) :: pad_method
    real :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_3d_float(wp, pad, pad_method, pad_const)
end function pad_3d_float
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
!#define pad_array_4d_ pad_array_4d_double
!
!#define pad_4d_ pad_4d_double
subroutine pad_array_1d_double(w, pad, method, const)
    ! arguments
    double precision, allocatable, dimension(:), intent(inout) :: w
    integer, dimension(1:2), intent(in) :: pad
    character(len=*), dimension(1:2), intent(in), optional :: method
    double precision, intent(in), optional :: const
    ! local variables
    integer :: i
    integer :: n1beg, n1end
    double precision, allocatable, dimension(:) :: wp
    integer :: l1, u1
    character(len=16), dimension(1:2) :: pad_method
    double precision :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0d0
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    l1 = pad(1)
    u1 = pad(2)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1))
    do i = n1beg, n1end
        wp(i) = w(i)
    end do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_1d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg)
            end do
        case ('symm')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg + i - 1)
            end do
        case ('const')
            do i = 1, l1
                wp(n1beg - i) = pad_const
            end do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            do i = 1, u1
                wp(n1end + i) = wp(n1end)
            end do
        case ('symm')
            do i = 1, u1
                wp(n1end + i) = wp(n1end - i + 1)
            end do
        case ('const')
            do i = 1, u1
                wp(n1end + i) = pad_const
            end do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_1d_double
subroutine pad_array_2d_double(w, pad, method, const)
    ! arguments
    double precision, allocatable, dimension(:, :), intent(inout) :: w
    integer, dimension(1:4), intent(in) :: pad
    character(len=*), dimension(1:4), intent(in), optional :: method
    double precision, intent(in), optional :: const
    ! local variables
    integer :: i, j
    integer :: n1beg, n1end, n2beg, n2end
    double precision, allocatable, dimension(:, :) :: wp
    integer :: l1, u1, l2, u2
    character(len=16), dimension(1:4) :: pad_method
    double precision :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0d0
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2))
    !$omp parallel do private(i, j) collapse(2)
    do j = n2beg, n2end
        do i = n1beg, n1end
            wp(i, j) = w(i, j)
        end do
    end do
    !$omp end parallel do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_2d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg + i - 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end - i + 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 lower boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg + j - 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 upper boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end - j + 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_2d_double
subroutine pad_array_3d_double(w, pad, method, const)
    ! arguments
    double precision, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, dimension(1:6), intent(in) :: pad
    character(len=*), dimension(1:6), intent(in), optional :: method
    double precision, intent(in), optional :: const
    ! local variables
    integer :: i, j, k
    integer :: n1beg, n1end, n2beg, n2end, n3beg, n3end
    integer :: l1, u1, l2, u2, l3, u3
    double precision, allocatable, dimension(:, :, :) :: wp
    character(len=16), dimension(1:6) :: pad_method
    double precision :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0d0
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    n3beg = lbound(w, 3)
    n3end = ubound(w, 3)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    l3 = pad(5)
    u3 = pad(6)
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_3d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2, n3beg - l3:n3end + u3))
    !$omp parallel do private(i, j, k) collapse(3)
    do k = n3beg, n3end
        do j = n2beg, n2end
            do i = n1beg, n1end
                wp(i, j, k) = w(i, j, k)
            end do
        end do
    end do
    !$omp end parallel do
    ! Left boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg + i - 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Right boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end - i + 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Front boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg + j - 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Back boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end - j + 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Top boundary
    select case (pad_method(5))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg + k - 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Bottom boundary
    select case (pad_method(6))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end - k + 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate array
    w = wp
end subroutine pad_array_3d_double
function pad_1d_double(w, pad, method, const) result(wp)
    ! arguments
    double precision, dimension(:) :: w
    integer, dimension(1:2) :: pad
    character(len=*), dimension(1:2), optional :: method
    double precision, optional :: const
    double precision, allocatable, dimension(:) :: wp
    ! local variables
    character(len=16), dimension(1:2) :: pad_method
    double precision :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0d0
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    wp = w
    call pad_array_1d_double(wp, pad, pad_method, pad_const)
end function pad_1d_double
function pad_2d_double(w, pad, method, const) result(wp)
    ! arguments
    double precision, dimension(:, :) :: w
    integer, dimension(1:4) :: pad
    character(len=*), dimension(1:4), optional :: method
    double precision, optional :: const
    double precision, allocatable, dimension(:, :) :: wp
    ! local variables
    character(len=16), dimension(1:4) :: pad_method
    double precision :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0d0
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_2d_double(wp, pad, pad_method, pad_const)
end function pad_2d_double
function pad_3d_double(w, pad, method, const) result(wp)
    ! arguments
    double precision, dimension(:, :, :) :: w
    integer, dimension(1:6) :: pad
    character(len=*), dimension(1:6), optional :: method
    double precision, optional :: const
    double precision, allocatable, dimension(:, :, :) :: wp
    ! local variables
    character(len=16), dimension(1:6) :: pad_method
    double precision :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = 0.0d0
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_3d_double(wp, pad, pad_method, pad_const)
end function pad_3d_double
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
!#define pad_array_4d_ pad_array_4d_complex
!
!#define pad_4d_ pad_4d_complex
subroutine pad_array_1d_complex(w, pad, method, const)
    ! arguments
    complex, allocatable, dimension(:), intent(inout) :: w
    integer, dimension(1:2), intent(in) :: pad
    character(len=*), dimension(1:2), intent(in), optional :: method
    complex, intent(in), optional :: const
    ! local variables
    integer :: i
    integer :: n1beg, n1end
    complex, allocatable, dimension(:) :: wp
    integer :: l1, u1
    character(len=16), dimension(1:2) :: pad_method
    complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = cmplx(0.0, 0.0)
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    l1 = pad(1)
    u1 = pad(2)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1))
    do i = n1beg, n1end
        wp(i) = w(i)
    end do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_1d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg)
            end do
        case ('symm')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg + i - 1)
            end do
        case ('const')
            do i = 1, l1
                wp(n1beg - i) = pad_const
            end do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            do i = 1, u1
                wp(n1end + i) = wp(n1end)
            end do
        case ('symm')
            do i = 1, u1
                wp(n1end + i) = wp(n1end - i + 1)
            end do
        case ('const')
            do i = 1, u1
                wp(n1end + i) = pad_const
            end do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_1d_complex
subroutine pad_array_2d_complex(w, pad, method, const)
    ! arguments
    complex, allocatable, dimension(:, :), intent(inout) :: w
    integer, dimension(1:4), intent(in) :: pad
    character(len=*), dimension(1:4), intent(in), optional :: method
    complex, intent(in), optional :: const
    ! local variables
    integer :: i, j
    integer :: n1beg, n1end, n2beg, n2end
    complex, allocatable, dimension(:, :) :: wp
    integer :: l1, u1, l2, u2
    character(len=16), dimension(1:4) :: pad_method
    complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = cmplx(0.0, 0.0)
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2))
    !$omp parallel do private(i, j) collapse(2)
    do j = n2beg, n2end
        do i = n1beg, n1end
            wp(i, j) = w(i, j)
        end do
    end do
    !$omp end parallel do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_2d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg + i - 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end - i + 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 lower boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg + j - 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 upper boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end - j + 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_2d_complex
subroutine pad_array_3d_complex(w, pad, method, const)
    ! arguments
    complex, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, dimension(1:6), intent(in) :: pad
    character(len=*), dimension(1:6), intent(in), optional :: method
    complex, intent(in), optional :: const
    ! local variables
    integer :: i, j, k
    integer :: n1beg, n1end, n2beg, n2end, n3beg, n3end
    integer :: l1, u1, l2, u2, l3, u3
    complex, allocatable, dimension(:, :, :) :: wp
    character(len=16), dimension(1:6) :: pad_method
    complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = cmplx(0.0, 0.0)
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    n3beg = lbound(w, 3)
    n3end = ubound(w, 3)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    l3 = pad(5)
    u3 = pad(6)
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_3d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2, n3beg - l3:n3end + u3))
    !$omp parallel do private(i, j, k) collapse(3)
    do k = n3beg, n3end
        do j = n2beg, n2end
            do i = n1beg, n1end
                wp(i, j, k) = w(i, j, k)
            end do
        end do
    end do
    !$omp end parallel do
    ! Left boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg + i - 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Right boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end - i + 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Front boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg + j - 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Back boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end - j + 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Top boundary
    select case (pad_method(5))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg + k - 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Bottom boundary
    select case (pad_method(6))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end - k + 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate array
    w = wp
end subroutine pad_array_3d_complex
function pad_1d_complex(w, pad, method, const) result(wp)
    ! arguments
    complex, dimension(:) :: w
    integer, dimension(1:2) :: pad
    character(len=*), dimension(1:2), optional :: method
    complex, optional :: const
    complex, allocatable, dimension(:) :: wp
    ! local variables
    character(len=16), dimension(1:2) :: pad_method
    complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = cmplx(0.0, 0.0)
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    wp = w
    call pad_array_1d_complex(wp, pad, pad_method, pad_const)
end function pad_1d_complex
function pad_2d_complex(w, pad, method, const) result(wp)
    ! arguments
    complex, dimension(:, :) :: w
    integer, dimension(1:4) :: pad
    character(len=*), dimension(1:4), optional :: method
    complex, optional :: const
    complex, allocatable, dimension(:, :) :: wp
    ! local variables
    character(len=16), dimension(1:4) :: pad_method
    complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = cmplx(0.0, 0.0)
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_2d_complex(wp, pad, pad_method, pad_const)
end function pad_2d_complex
function pad_3d_complex(w, pad, method, const) result(wp)
    ! arguments
    complex, dimension(:, :, :) :: w
    integer, dimension(1:6) :: pad
    character(len=*), dimension(1:6), optional :: method
    complex, optional :: const
    complex, allocatable, dimension(:, :, :) :: wp
    ! local variables
    character(len=16), dimension(1:6) :: pad_method
    complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = cmplx(0.0, 0.0)
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_3d_complex(wp, pad, pad_method, pad_const)
end function pad_3d_complex
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
!#define pad_array_4d_ pad_array_4d_dcomplex
!
!#define pad_4d_ pad_4d_dcomplex
subroutine pad_array_1d_dcomplex(w, pad, method, const)
    ! arguments
    double complex, allocatable, dimension(:), intent(inout) :: w
    integer, dimension(1:2), intent(in) :: pad
    character(len=*), dimension(1:2), intent(in), optional :: method
    double complex, intent(in), optional :: const
    ! local variables
    integer :: i
    integer :: n1beg, n1end
    double complex, allocatable, dimension(:) :: wp
    integer :: l1, u1
    character(len=16), dimension(1:2) :: pad_method
    double complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = dcmplx(0.0d0, 0.0d0)
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    l1 = pad(1)
    u1 = pad(2)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1))
    do i = n1beg, n1end
        wp(i) = w(i)
    end do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_1d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg)
            end do
        case ('symm')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg + i - 1)
            end do
        case ('const')
            do i = 1, l1
                wp(n1beg - i) = pad_const
            end do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            do i = 1, u1
                wp(n1end + i) = wp(n1end)
            end do
        case ('symm')
            do i = 1, u1
                wp(n1end + i) = wp(n1end - i + 1)
            end do
        case ('const')
            do i = 1, u1
                wp(n1end + i) = pad_const
            end do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_1d_dcomplex
subroutine pad_array_2d_dcomplex(w, pad, method, const)
    ! arguments
    double complex, allocatable, dimension(:, :), intent(inout) :: w
    integer, dimension(1:4), intent(in) :: pad
    character(len=*), dimension(1:4), intent(in), optional :: method
    double complex, intent(in), optional :: const
    ! local variables
    integer :: i, j
    integer :: n1beg, n1end, n2beg, n2end
    double complex, allocatable, dimension(:, :) :: wp
    integer :: l1, u1, l2, u2
    character(len=16), dimension(1:4) :: pad_method
    double complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = dcmplx(0.0d0, 0.0d0)
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2))
    !$omp parallel do private(i, j) collapse(2)
    do j = n2beg, n2end
        do i = n1beg, n1end
            wp(i, j) = w(i, j)
        end do
    end do
    !$omp end parallel do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_2d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg + i - 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end - i + 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 lower boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg + j - 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 upper boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end - j + 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_2d_dcomplex
subroutine pad_array_3d_dcomplex(w, pad, method, const)
    ! arguments
    double complex, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, dimension(1:6), intent(in) :: pad
    character(len=*), dimension(1:6), intent(in), optional :: method
    double complex, intent(in), optional :: const
    ! local variables
    integer :: i, j, k
    integer :: n1beg, n1end, n2beg, n2end, n3beg, n3end
    integer :: l1, u1, l2, u2, l3, u3
    double complex, allocatable, dimension(:, :, :) :: wp
    character(len=16), dimension(1:6) :: pad_method
    double complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = dcmplx(0.0d0, 0.0d0)
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    n3beg = lbound(w, 3)
    n3end = ubound(w, 3)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    l3 = pad(5)
    u3 = pad(6)
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_3d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2, n3beg - l3:n3end + u3))
    !$omp parallel do private(i, j, k) collapse(3)
    do k = n3beg, n3end
        do j = n2beg, n2end
            do i = n1beg, n1end
                wp(i, j, k) = w(i, j, k)
            end do
        end do
    end do
    !$omp end parallel do
    ! Left boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg + i - 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Right boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end - i + 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Front boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg + j - 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Back boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end - j + 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Top boundary
    select case (pad_method(5))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg + k - 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Bottom boundary
    select case (pad_method(6))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end - k + 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate array
    w = wp
end subroutine pad_array_3d_dcomplex
function pad_1d_dcomplex(w, pad, method, const) result(wp)
    ! arguments
    double complex, dimension(:) :: w
    integer, dimension(1:2) :: pad
    character(len=*), dimension(1:2), optional :: method
    double complex, optional :: const
    double complex, allocatable, dimension(:) :: wp
    ! local variables
    character(len=16), dimension(1:2) :: pad_method
    double complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = dcmplx(0.0d0, 0.0d0)
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    wp = w
    call pad_array_1d_dcomplex(wp, pad, pad_method, pad_const)
end function pad_1d_dcomplex
function pad_2d_dcomplex(w, pad, method, const) result(wp)
    ! arguments
    double complex, dimension(:, :) :: w
    integer, dimension(1:4) :: pad
    character(len=*), dimension(1:4), optional :: method
    double complex, optional :: const
    double complex, allocatable, dimension(:, :) :: wp
    ! local variables
    character(len=16), dimension(1:4) :: pad_method
    double complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = dcmplx(0.0d0, 0.0d0)
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_2d_dcomplex(wp, pad, pad_method, pad_const)
end function pad_2d_dcomplex
function pad_3d_dcomplex(w, pad, method, const) result(wp)
    ! arguments
    double complex, dimension(:, :, :) :: w
    integer, dimension(1:6) :: pad
    character(len=*), dimension(1:6), optional :: method
    double complex, optional :: const
    double complex, allocatable, dimension(:, :, :) :: wp
    ! local variables
    character(len=16), dimension(1:6) :: pad_method
    double complex :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = dcmplx(0.0d0, 0.0d0)
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_3d_dcomplex(wp, pad, pad_method, pad_const)
end function pad_3d_dcomplex
! #define T logical
! #define TT logical
! #define TTT logical
! #define DEFAULT_VALUE .false.
! #include "template_pad.f90"
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
!#define pad_array_4d_ pad_array_4d_string
!
!#define pad_4d_ pad_4d_string
subroutine pad_array_1d_string(w, pad, method, const)
    ! arguments
    character(len=*), allocatable, dimension(:), intent(inout) :: w
    integer, dimension(1:2), intent(in) :: pad
    character(len=*), dimension(1:2), intent(in), optional :: method
    character(len=*), intent(in), optional :: const
    ! local variables
    integer :: i
    integer :: n1beg, n1end
    character(len=1024), allocatable, dimension(:) :: wp
    integer :: l1, u1
    character(len=16), dimension(1:2) :: pad_method
    character(len=1024) :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = ''
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    l1 = pad(1)
    u1 = pad(2)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1))
    do i = n1beg, n1end
        wp(i) = w(i)
    end do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_1d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg)
            end do
        case ('symm')
            do i = 1, l1
                wp(n1beg - i) = wp(n1beg + i - 1)
            end do
        case ('const')
            do i = 1, l1
                wp(n1beg - i) = pad_const
            end do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            do i = 1, u1
                wp(n1end + i) = wp(n1end)
            end do
        case ('symm')
            do i = 1, u1
                wp(n1end + i) = wp(n1end - i + 1)
            end do
        case ('const')
            do i = 1, u1
                wp(n1end + i) = pad_const
            end do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_1d_string
subroutine pad_array_2d_string(w, pad, method, const)
    ! arguments
    character(len=*), allocatable, dimension(:, :), intent(inout) :: w
    integer, dimension(1:4), intent(in) :: pad
    character(len=*), dimension(1:4), intent(in), optional :: method
    character(len=*), intent(in), optional :: const
    ! local variables
    integer :: i, j
    integer :: n1beg, n1end, n2beg, n2end
    character(len=1024), allocatable, dimension(:, :) :: wp
    integer :: l1, u1, l2, u2
    character(len=16), dimension(1:4) :: pad_method
    character(len=1024) :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = ''
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2))
    !$omp parallel do private(i, j) collapse(2)
    do j = n2beg, n2end
        do i = n1beg, n1end
            wp(i, j) = w(i, j)
        end do
    end do
    !$omp end parallel do
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_2d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! axis 1 lower boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = wp(n1beg + i - 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, l1
                    wp(n1beg - i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 1 upper boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end, j)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = wp(n1end - i + 1, j)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = n2beg - l2, n2end + u2
                do i = 1, u1
                    wp(n1end + i, j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 lower boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = wp(i, n2beg + j - 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, l2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2beg - j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! axis 2 upper boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end)
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = wp(i, n2end - j + 1)
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j) collapse(2)
            do j = 1, u2
                do i = n1beg - l1, n1end + u1
                    wp(i, n2end + j) = pad_const
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate input array
    w = wp
end subroutine pad_array_2d_string
subroutine pad_array_3d_string(w, pad, method, const)
    ! arguments
    character(len=*), allocatable, dimension(:, :, :), intent(inout) :: w
    integer, dimension(1:6), intent(in) :: pad
    character(len=*), dimension(1:6), intent(in), optional :: method
    character(len=*), intent(in), optional :: const
    ! local variables
    integer :: i, j, k
    integer :: n1beg, n1end, n2beg, n2end, n3beg, n3end
    integer :: l1, u1, l2, u2, l3, u3
    character(len=1024), allocatable, dimension(:, :, :) :: wp
    character(len=16), dimension(1:6) :: pad_method
    character(len=1024) :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = ''
    end if
    ! bounds
    n1beg = lbound(w, 1)
    n1end = ubound(w, 1)
    n2beg = lbound(w, 2)
    n2end = ubound(w, 2)
    n3beg = lbound(w, 3)
    n3end = ubound(w, 3)
    l1 = pad(1)
    u1 = pad(2)
    l2 = pad(3)
    u2 = pad(4)
    l3 = pad(5)
    u3 = pad(6)
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    do i = 1, size(pad_method)
        call assert(any(pad_method(i) == ['edge ', 'symm ', 'const']), &
            ' <pad_3d> Error: Padding method must be one of edge, symm, const.')
    end do
    ! new array
    allocate(wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2, n3beg - l3:n3end + u3))
    !$omp parallel do private(i, j, k) collapse(3)
    do k = n3beg, n3end
        do j = n2beg, n2end
            do i = n1beg, n1end
                wp(i, j, k) = w(i, j, k)
            end do
        end do
    end do
    !$omp end parallel do
    ! Left boundary
    select case (pad_method(1))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = wp(n1beg + i - 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Right boundary
    select case (pad_method(2))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = wp(n1end - i + 1, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Front boundary
    select case (pad_method(3))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = wp(i, n2beg + j - 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Back boundary
    select case (pad_method(4))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = wp(i, n2end - j + 1, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = n3beg - l3, n3end + u3
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j, k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Top boundary
    select case (pad_method(5))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = wp(i, j, n3beg + k - 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, l3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3beg - k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! Bottom boundary
    select case (pad_method(6))
        case ('edge')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('symm')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = wp(i, j, n3end - k + 1)
                    end do
                end do
            end do
            !$omp end parallel do
        case ('const')
            !$omp parallel do private(i, j, k) collapse(3)
            do k = 1, u3
                do j = n2beg - l2, n2end + u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, j, n3end + k) = pad_const
                    end do
                end do
            end do
            !$omp end parallel do
    end select
    ! reallocate array
    w = wp
end subroutine pad_array_3d_string
function pad_1d_string(w, pad, method, const) result(wp)
    ! arguments
    character(len=*), dimension(:) :: w
    integer, dimension(1:2) :: pad
    character(len=*), dimension(1:2), optional :: method
    character(len=*), optional :: const
    character(len=1024), allocatable, dimension(:) :: wp
    ! local variables
    character(len=16), dimension(1:2) :: pad_method
    character(len=1024) :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = ''
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge']
    end if
    wp = w
    call pad_array_1d_string(wp, pad, pad_method, pad_const)
end function pad_1d_string
function pad_2d_string(w, pad, method, const) result(wp)
    ! arguments
    character(len=*), dimension(:, :) :: w
    integer, dimension(1:4) :: pad
    character(len=*), dimension(1:4), optional :: method
    character(len=*), optional :: const
    character(len=1024), allocatable, dimension(:, :) :: wp
    ! local variables
    character(len=16), dimension(1:4) :: pad_method
    character(len=1024) :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = ''
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_2d_string(wp, pad, pad_method, pad_const)
end function pad_2d_string
function pad_3d_string(w, pad, method, const) result(wp)
    ! arguments
    character(len=*), dimension(:, :, :) :: w
    integer, dimension(1:6) :: pad
    character(len=*), dimension(1:6), optional :: method
    character(len=*), optional :: const
    character(len=1024), allocatable, dimension(:, :, :) :: wp
    ! local variables
    character(len=16), dimension(1:6) :: pad_method
    character(len=1024) :: pad_const
    if (present(const)) then
        pad_const = const
    else
        pad_const = ''
    end if
    ! padding method
    if (present(method)) then
        pad_method = method
    else
        pad_method = ['edge', 'edge', 'edge', 'edge', 'edge', 'edge']
    end if
    wp = w
    call pad_array_3d_string(wp, pad, pad_method, pad_const)
end function pad_3d_string
    !================================================================
    ! Set operations
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
!function intersect_1d_(a, b) result(c)
! ! arguments
! integer, dimension(:) :: a, b
! integer, allocatable, dimension(:) :: c
! ! local variables
! integer :: i
!end subroutine intersect_1d_
function any_in_1d_int(a, b) result(y)
    integer, dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .false.
    do i = 1, size(a)
        y = y .or. any(a(i) == b)
    end do
end function any_in_1d_int
function all_in_1d_int(a, b) result(y)
    integer, dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .true.
    do i = 1, size(a)
        y = y .and. any(a(i) == b)
        if (.not. y) then
            return
        end if
    end do
end function all_in_1d_int
function remove_any_in_1d_int(a, b) result(c)
    integer, dimension(:) :: a, b
    integer, allocatable, dimension(:) :: c
    integer :: i
    c = a
    do i = 1, size(b)
        c = pack(c, c /= b(i))
    end do
end function remove_any_in_1d_int
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
!function intersect_1d_(a, b) result(c)
! ! arguments
! real, dimension(:) :: a, b
! real, allocatable, dimension(:) :: c
! ! local variables
! integer :: i
!end subroutine intersect_1d_
function any_in_1d_float(a, b) result(y)
    real, dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .false.
    do i = 1, size(a)
        y = y .or. any(a(i) == b)
    end do
end function any_in_1d_float
function all_in_1d_float(a, b) result(y)
    real, dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .true.
    do i = 1, size(a)
        y = y .and. any(a(i) == b)
        if (.not. y) then
            return
        end if
    end do
end function all_in_1d_float
function remove_any_in_1d_float(a, b) result(c)
    real, dimension(:) :: a, b
    real, allocatable, dimension(:) :: c
    integer :: i
    c = a
    do i = 1, size(b)
        c = pack(c, c /= b(i))
    end do
end function remove_any_in_1d_float
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
!function intersect_1d_(a, b) result(c)
! ! arguments
! double precision, dimension(:) :: a, b
! double precision, allocatable, dimension(:) :: c
! ! local variables
! integer :: i
!end subroutine intersect_1d_
function any_in_1d_double(a, b) result(y)
    double precision, dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .false.
    do i = 1, size(a)
        y = y .or. any(a(i) == b)
    end do
end function any_in_1d_double
function all_in_1d_double(a, b) result(y)
    double precision, dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .true.
    do i = 1, size(a)
        y = y .and. any(a(i) == b)
        if (.not. y) then
            return
        end if
    end do
end function all_in_1d_double
function remove_any_in_1d_double(a, b) result(c)
    double precision, dimension(:) :: a, b
    double precision, allocatable, dimension(:) :: c
    integer :: i
    c = a
    do i = 1, size(b)
        c = pack(c, c /= b(i))
    end do
end function remove_any_in_1d_double
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
!function intersect_1d_(a, b) result(c)
! ! arguments
! complex, dimension(:) :: a, b
! complex, allocatable, dimension(:) :: c
! ! local variables
! integer :: i
!end subroutine intersect_1d_
function any_in_1d_complex(a, b) result(y)
    complex, dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .false.
    do i = 1, size(a)
        y = y .or. any(a(i) == b)
    end do
end function any_in_1d_complex
function all_in_1d_complex(a, b) result(y)
    complex, dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .true.
    do i = 1, size(a)
        y = y .and. any(a(i) == b)
        if (.not. y) then
            return
        end if
    end do
end function all_in_1d_complex
function remove_any_in_1d_complex(a, b) result(c)
    complex, dimension(:) :: a, b
    complex, allocatable, dimension(:) :: c
    integer :: i
    c = a
    do i = 1, size(b)
        c = pack(c, c /= b(i))
    end do
end function remove_any_in_1d_complex
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
!function intersect_1d_(a, b) result(c)
! ! arguments
! double complex, dimension(:) :: a, b
! double complex, allocatable, dimension(:) :: c
! ! local variables
! integer :: i
!end subroutine intersect_1d_
function any_in_1d_dcomplex(a, b) result(y)
    double complex, dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .false.
    do i = 1, size(a)
        y = y .or. any(a(i) == b)
    end do
end function any_in_1d_dcomplex
function all_in_1d_dcomplex(a, b) result(y)
    double complex, dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .true.
    do i = 1, size(a)
        y = y .and. any(a(i) == b)
        if (.not. y) then
            return
        end if
    end do
end function all_in_1d_dcomplex
function remove_any_in_1d_dcomplex(a, b) result(c)
    double complex, dimension(:) :: a, b
    double complex, allocatable, dimension(:) :: c
    integer :: i
    c = a
    do i = 1, size(b)
        c = pack(c, c /= b(i))
    end do
end function remove_any_in_1d_dcomplex
! #define T logical
! #define TT logical
! #define TTT logical
! #include "template_set.f90"
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
!function intersect_1d_(a, b) result(c)
! ! arguments
! character(len=*), dimension(:) :: a, b
! character(len=*), allocatable, dimension(:) :: c
! ! local variables
! integer :: i
!end subroutine intersect_1d_
function any_in_1d_string(a, b) result(y)
    character(len=*), dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .false.
    do i = 1, size(a)
        y = y .or. any(a(i) == b)
    end do
end function any_in_1d_string
function all_in_1d_string(a, b) result(y)
    character(len=*), dimension(:) :: a, b
    logical :: y
    integer :: i
    y = .true.
    do i = 1, size(a)
        y = y .and. any(a(i) == b)
        if (.not. y) then
            return
        end if
    end do
end function all_in_1d_string
function remove_any_in_1d_string(a, b) result(c)
    character(len=*), dimension(:) :: a, b
    character(len=1024), allocatable, dimension(:) :: c
    integer :: i
    c = a
    do i = 1, size(b)
        c = pack(c, c /= b(i))
    end do
end function remove_any_in_1d_string
    !================================================================
    ! Permute array
    !
    ! ................ Fortran reshape usage ................
    !
    ! reshape(w, shape=[], order=[])
    !
    ! Note that the order indicates "where do the old dims go in new array"
    ! e.g., reshape the array w(35,30,40) to w(40,35,30):
    !
    ! reshape(w, shape=[40,35,30], order=[?])
    !
    ! then order should be order=[2, 3, 1], because:
    ! 35 becomes dimension 2 in the new array
    ! 30 becomes dimension 3 in the new array
    ! 40 becomes dimension 1 in the new array
    !
    function ndigits(x) result(n)
        integer, intent(in) :: x
        integer :: n
        if (x == 0) then
            n = 1
        else
            n = ceiling(log10(abs(x) + 1.0))
        end if
    end function ndigits
    function breakint(x) result(w)
        integer, intent(in) :: x
        integer, allocatable, dimension(:) :: w
        integer :: rem, i
        integer :: nw
        nw = ndigits(x)
        allocate (w(1:nw))
        rem = x
        do i = 1, nw
            ! Take advantage of integer division
            w(nw - i + 1) = rem - (rem/10)*10
            rem = rem/10
        end do
    end function breakint
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
subroutine permute_array_3d_int(w, order)
    integer, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    w = reshape(w, shape=[n1, n2, n3], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end subroutine permute_array_3d_int
subroutine permute_array_4d_int(w, order)
    integer, allocatable, dimension(:, :, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    w = reshape(w, shape=[n1, n2, n3, n4], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
end subroutine permute_array_4d_int
function permute_3d_int(w, order) result(pw)
    integer, dimension(:, :, :), intent(in) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    allocate (pw(1:n1, 1:n2, 1:n3))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end function permute_3d_int
function permute_4d_int(w, order) result(pw)
    integer, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:, :, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    allocate (pw(1:n1, 1:n2, 1:n3, 1:n4))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
    ! order=[ &
        ! findloc(sorder,1), &
        ! findloc(sorder,2), &
        ! findloc(sorder,3), &
        ! findloc(sorder,4)])
end function permute_4d_int
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
subroutine permute_array_3d_float(w, order)
    real, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    w = reshape(w, shape=[n1, n2, n3], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end subroutine permute_array_3d_float
subroutine permute_array_4d_float(w, order)
    real, allocatable, dimension(:, :, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    w = reshape(w, shape=[n1, n2, n3, n4], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
end subroutine permute_array_4d_float
function permute_3d_float(w, order) result(pw)
    real, dimension(:, :, :), intent(in) :: w
    integer, intent(in) :: order
    real, allocatable, dimension(:, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    allocate (pw(1:n1, 1:n2, 1:n3))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end function permute_3d_float
function permute_4d_float(w, order) result(pw)
    real, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in) :: order
    real, allocatable, dimension(:, :, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    allocate (pw(1:n1, 1:n2, 1:n3, 1:n4))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
    ! order=[ &
        ! findloc(sorder,1), &
        ! findloc(sorder,2), &
        ! findloc(sorder,3), &
        ! findloc(sorder,4)])
end function permute_4d_float
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
subroutine permute_array_3d_double(w, order)
    double precision, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    w = reshape(w, shape=[n1, n2, n3], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end subroutine permute_array_3d_double
subroutine permute_array_4d_double(w, order)
    double precision, allocatable, dimension(:, :, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    w = reshape(w, shape=[n1, n2, n3, n4], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
end subroutine permute_array_4d_double
function permute_3d_double(w, order) result(pw)
    double precision, dimension(:, :, :), intent(in) :: w
    integer, intent(in) :: order
    double precision, allocatable, dimension(:, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    allocate (pw(1:n1, 1:n2, 1:n3))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end function permute_3d_double
function permute_4d_double(w, order) result(pw)
    double precision, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in) :: order
    double precision, allocatable, dimension(:, :, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    allocate (pw(1:n1, 1:n2, 1:n3, 1:n4))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
    ! order=[ &
        ! findloc(sorder,1), &
        ! findloc(sorder,2), &
        ! findloc(sorder,3), &
        ! findloc(sorder,4)])
end function permute_4d_double
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
subroutine permute_array_3d_complex(w, order)
    complex, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    w = reshape(w, shape=[n1, n2, n3], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end subroutine permute_array_3d_complex
subroutine permute_array_4d_complex(w, order)
    complex, allocatable, dimension(:, :, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    w = reshape(w, shape=[n1, n2, n3, n4], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
end subroutine permute_array_4d_complex
function permute_3d_complex(w, order) result(pw)
    complex, dimension(:, :, :), intent(in) :: w
    integer, intent(in) :: order
    complex, allocatable, dimension(:, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    allocate (pw(1:n1, 1:n2, 1:n3))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end function permute_3d_complex
function permute_4d_complex(w, order) result(pw)
    complex, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in) :: order
    complex, allocatable, dimension(:, :, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    allocate (pw(1:n1, 1:n2, 1:n3, 1:n4))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
    ! order=[ &
        ! findloc(sorder,1), &
        ! findloc(sorder,2), &
        ! findloc(sorder,3), &
        ! findloc(sorder,4)])
end function permute_4d_complex
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
subroutine permute_array_3d_dcomplex(w, order)
    double complex, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    w = reshape(w, shape=[n1, n2, n3], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end subroutine permute_array_3d_dcomplex
subroutine permute_array_4d_dcomplex(w, order)
    double complex, allocatable, dimension(:, :, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    w = reshape(w, shape=[n1, n2, n3, n4], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
end subroutine permute_array_4d_dcomplex
function permute_3d_dcomplex(w, order) result(pw)
    double complex, dimension(:, :, :), intent(in) :: w
    integer, intent(in) :: order
    double complex, allocatable, dimension(:, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    allocate (pw(1:n1, 1:n2, 1:n3))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end function permute_3d_dcomplex
function permute_4d_dcomplex(w, order) result(pw)
    double complex, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in) :: order
    double complex, allocatable, dimension(:, :, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    allocate (pw(1:n1, 1:n2, 1:n3, 1:n4))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
    ! order=[ &
        ! findloc(sorder,1), &
        ! findloc(sorder,2), &
        ! findloc(sorder,3), &
        ! findloc(sorder,4)])
end function permute_4d_dcomplex
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
subroutine permute_array_3d_logical(w, order)
    logical, allocatable, dimension(:, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    w = reshape(w, shape=[n1, n2, n3], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end subroutine permute_array_3d_logical
subroutine permute_array_4d_logical(w, order)
    logical, allocatable, dimension(:, :, :, :), intent(inout) :: w
    integer, intent(in) :: order
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    w = reshape(w, shape=[n1, n2, n3, n4], &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
end subroutine permute_array_4d_logical
function permute_3d_logical(w, order) result(pw)
    logical, dimension(:, :, :), intent(in) :: w
    integer, intent(in) :: order
    logical, allocatable, dimension(:, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3
    allocate (sorder(1:3))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    allocate (pw(1:n1, 1:n2, 1:n3))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3))])
end function permute_3d_logical
function permute_4d_logical(w, order) result(pw)
    logical, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in) :: order
    logical, allocatable, dimension(:, :, :, :) :: pw
    integer, allocatable, dimension(:) :: sorder
    integer :: n1, n2, n3, n4
    allocate (sorder(1:4))
    sorder = breakint(order)
    n1 = size(w, sorder(1))
    n2 = size(w, sorder(2))
    n3 = size(w, sorder(3))
    n4 = size(w, sorder(4))
    allocate (pw(1:n1, 1:n2, 1:n3, 1:n4))
    pw = reshape(w, shape=shape(pw), &
        order=[ &
        maxloc(sorder, mask=(sorder == 1)), &
        maxloc(sorder, mask=(sorder == 2)), &
        maxloc(sorder, mask=(sorder == 3)), &
        maxloc(sorder, mask=(sorder == 4))])
    ! order=[ &
        ! findloc(sorder,1), &
        ! findloc(sorder,2), &
        ! findloc(sorder,3), &
        ! findloc(sorder,4)])
end function permute_4d_logical
    !================================================================
    ! Rescale array
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
function rescale_1d_float(w, range) result(ws)
    real, dimension(:), intent(in) :: w
    real, dimension(:), intent(in) :: range
    real :: dr
    real, allocatable, dimension(:) :: ws
    ws = w
    if (maxval(ws) /= minval(ws) .and. range(1) /= range(2)) then
        dr = range(2) - range(1)
        ws = ws - minval(ws)
        ws = ws/maxval(ws)
        ws = ws*dr + range(1)
    else
        ws = range(1)
    end if
end function rescale_1d_float
function rescale_2d_float(w, range) result(ws)
    real, dimension(:, :), intent(in) :: w
    real, dimension(:), intent(in) :: range
    real :: dr
    real, allocatable, dimension(:, :) :: ws
    ws = w
    if (maxval(ws) /= minval(ws) .and. range(1) /= range(2)) then
        dr = range(2) - range(1)
        ws = ws - minval(ws)
        ws = ws/maxval(ws)
        ws = ws*dr + range(1)
    else
        ws = range(1)
    end if
end function rescale_2d_float
function rescale_3d_float(w, range) result(ws)
    real, dimension(:, :, :), intent(in) :: w
    real, dimension(:), intent(in) :: range
    real :: dr
    real, allocatable, dimension(:, :, :) :: ws
    ws = w
    if (maxval(ws) /= minval(ws) .and. range(1) /= range(2)) then
        dr = range(2) - range(1)
        ws = ws - minval(ws)
        ws = ws/maxval(ws)
        ws = ws*dr + range(1)
    else
        ws = range(1)
    end if
end function rescale_3d_float
function rescale_4d_float(w, range) result(ws)
    real, dimension(:, :, :, :), intent(in) :: w
    real, dimension(:), intent(in) :: range
    real :: dr
    real, allocatable, dimension(:, :, :, :) :: ws
    ws = w
    if (maxval(ws) /= minval(ws) .and. range(1) /= range(2)) then
        dr = range(2) - range(1)
        ws = ws - minval(ws)
        ws = ws/maxval(ws)
        ws = ws*dr + range(1)
    else
        ws = range(1)
    end if
end function rescale_4d_float
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
function rescale_1d_double(w, range) result(ws)
    double precision, dimension(:), intent(in) :: w
    double precision, dimension(:), intent(in) :: range
    double precision :: dr
    double precision, allocatable, dimension(:) :: ws
    ws = w
    if (maxval(ws) /= minval(ws) .and. range(1) /= range(2)) then
        dr = range(2) - range(1)
        ws = ws - minval(ws)
        ws = ws/maxval(ws)
        ws = ws*dr + range(1)
    else
        ws = range(1)
    end if
end function rescale_1d_double
function rescale_2d_double(w, range) result(ws)
    double precision, dimension(:, :), intent(in) :: w
    double precision, dimension(:), intent(in) :: range
    double precision :: dr
    double precision, allocatable, dimension(:, :) :: ws
    ws = w
    if (maxval(ws) /= minval(ws) .and. range(1) /= range(2)) then
        dr = range(2) - range(1)
        ws = ws - minval(ws)
        ws = ws/maxval(ws)
        ws = ws*dr + range(1)
    else
        ws = range(1)
    end if
end function rescale_2d_double
function rescale_3d_double(w, range) result(ws)
    double precision, dimension(:, :, :), intent(in) :: w
    double precision, dimension(:), intent(in) :: range
    double precision :: dr
    double precision, allocatable, dimension(:, :, :) :: ws
    ws = w
    if (maxval(ws) /= minval(ws) .and. range(1) /= range(2)) then
        dr = range(2) - range(1)
        ws = ws - minval(ws)
        ws = ws/maxval(ws)
        ws = ws*dr + range(1)
    else
        ws = range(1)
    end if
end function rescale_3d_double
function rescale_4d_double(w, range) result(ws)
    double precision, dimension(:, :, :, :), intent(in) :: w
    double precision, dimension(:), intent(in) :: range
    double precision :: dr
    double precision, allocatable, dimension(:, :, :, :) :: ws
    ws = w
    if (maxval(ws) /= minval(ws) .and. range(1) /= range(2)) then
        dr = range(2) - range(1)
        ws = ws - minval(ws)
        ws = ws/maxval(ws)
        ws = ws*dr + range(1)
    else
        ws = range(1)
    end if
end function rescale_4d_double
    !================================================================
    ! Rotate array
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
function rot90_1d_int(w) result(wr)
    integer, dimension(:) :: w
    integer, allocatable, dimension(:, :) :: wr
    integer :: n
    n = size(w)
    allocate(wr(n, 1))
    wr(:, 1) = w(:)
end function rot90_1d_int
function rot90_2d_int(w, cw) result(wr)
    integer, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: cw
    integer, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    integer :: rotate_direction
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        allocate (wr(1:n2, 1:n1))
    else
        allocate (wr(1:n1, 1:n2))
    end if
    ! If no rotation eventually
    if (rotate_direction == 0) then
        wr = w
        return
    end if
    ! Otherwise
    select case (rotate_direction)
        case (1, -3)
            ! Clockwise by 90 degree or equivalently counter-clockwise by 270 degree
            do j = 1, n2
                do i = 1, n1
                    wr(j, n1 - i + 1) = w(i, j)
                end do
            end do
        case (2, -2)
            ! Clockwise by 180 degree or equivalently counter-clockwise by 180 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n1 - i + 1, n2 - j + 1) = w(i, j)
                end do
            end do
        case (3, -1)
            ! Clockwise by 270 degree or equivalently counter-clockwise by 90 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n2 - j + 1, i) = w(i, j)
                end do
            end do
    end select
end function rot90_2d_int
function rot90_3d_int(w, cw, dim) result(wr)
    integer, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: cw, dim
    integer, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    integer :: m1, m2, m3
    integer :: rotate_direction, rotate_axis
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    if (present(dim)) then
        call assert(dim == 1 .or. dim == 2 .or. dim == 3, ' dim /= 1, 2, 3')
        rotate_axis = dim
    else
        rotate_axis = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        select case (dim)
            case (1)
                m1 = n1
                m2 = n3
                m3 = n2
            case (2)
                m1 = n3
                m2 = n2
                m3 = n1
            case (3)
                m1 = n2
                m2 = n1
                m3 = n3
        end select
    else
        m1 = n1
        m2 = n2
        m3 = n3
    end if
    allocate (wr(1:m1, 1:m2, 1:m3))
    select case (dim)
        case (1)
            !$omp parallel do private(i)
            do i = 1, m1
                wr(i, :, :) = rot90_2d_int(w(i, :, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(j)
            do j = 1, m2
                wr(:, j, :) = rot90_2d_int(w(:, j, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(k)
            do k = 1, m3
                wr(:, :, k) = rot90_2d_int(w(:, :, k), cw=rotate_direction)
            end do
            !$omp end parallel do
    end select
end function rot90_3d_int
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
function rot90_1d_float(w) result(wr)
    real, dimension(:) :: w
    real, allocatable, dimension(:, :) :: wr
    integer :: n
    n = size(w)
    allocate(wr(n, 1))
    wr(:, 1) = w(:)
end function rot90_1d_float
function rot90_2d_float(w, cw) result(wr)
    real, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: cw
    real, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    integer :: rotate_direction
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        allocate (wr(1:n2, 1:n1))
    else
        allocate (wr(1:n1, 1:n2))
    end if
    ! If no rotation eventually
    if (rotate_direction == 0) then
        wr = w
        return
    end if
    ! Otherwise
    select case (rotate_direction)
        case (1, -3)
            ! Clockwise by 90 degree or equivalently counter-clockwise by 270 degree
            do j = 1, n2
                do i = 1, n1
                    wr(j, n1 - i + 1) = w(i, j)
                end do
            end do
        case (2, -2)
            ! Clockwise by 180 degree or equivalently counter-clockwise by 180 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n1 - i + 1, n2 - j + 1) = w(i, j)
                end do
            end do
        case (3, -1)
            ! Clockwise by 270 degree or equivalently counter-clockwise by 90 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n2 - j + 1, i) = w(i, j)
                end do
            end do
    end select
end function rot90_2d_float
function rot90_3d_float(w, cw, dim) result(wr)
    real, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: cw, dim
    real, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    integer :: m1, m2, m3
    integer :: rotate_direction, rotate_axis
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    if (present(dim)) then
        call assert(dim == 1 .or. dim == 2 .or. dim == 3, ' dim /= 1, 2, 3')
        rotate_axis = dim
    else
        rotate_axis = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        select case (dim)
            case (1)
                m1 = n1
                m2 = n3
                m3 = n2
            case (2)
                m1 = n3
                m2 = n2
                m3 = n1
            case (3)
                m1 = n2
                m2 = n1
                m3 = n3
        end select
    else
        m1 = n1
        m2 = n2
        m3 = n3
    end if
    allocate (wr(1:m1, 1:m2, 1:m3))
    select case (dim)
        case (1)
            !$omp parallel do private(i)
            do i = 1, m1
                wr(i, :, :) = rot90_2d_float(w(i, :, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(j)
            do j = 1, m2
                wr(:, j, :) = rot90_2d_float(w(:, j, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(k)
            do k = 1, m3
                wr(:, :, k) = rot90_2d_float(w(:, :, k), cw=rotate_direction)
            end do
            !$omp end parallel do
    end select
end function rot90_3d_float
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
function rot90_1d_double(w) result(wr)
    double precision, dimension(:) :: w
    double precision, allocatable, dimension(:, :) :: wr
    integer :: n
    n = size(w)
    allocate(wr(n, 1))
    wr(:, 1) = w(:)
end function rot90_1d_double
function rot90_2d_double(w, cw) result(wr)
    double precision, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: cw
    double precision, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    integer :: rotate_direction
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        allocate (wr(1:n2, 1:n1))
    else
        allocate (wr(1:n1, 1:n2))
    end if
    ! If no rotation eventually
    if (rotate_direction == 0) then
        wr = w
        return
    end if
    ! Otherwise
    select case (rotate_direction)
        case (1, -3)
            ! Clockwise by 90 degree or equivalently counter-clockwise by 270 degree
            do j = 1, n2
                do i = 1, n1
                    wr(j, n1 - i + 1) = w(i, j)
                end do
            end do
        case (2, -2)
            ! Clockwise by 180 degree or equivalently counter-clockwise by 180 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n1 - i + 1, n2 - j + 1) = w(i, j)
                end do
            end do
        case (3, -1)
            ! Clockwise by 270 degree or equivalently counter-clockwise by 90 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n2 - j + 1, i) = w(i, j)
                end do
            end do
    end select
end function rot90_2d_double
function rot90_3d_double(w, cw, dim) result(wr)
    double precision, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: cw, dim
    double precision, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    integer :: m1, m2, m3
    integer :: rotate_direction, rotate_axis
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    if (present(dim)) then
        call assert(dim == 1 .or. dim == 2 .or. dim == 3, ' dim /= 1, 2, 3')
        rotate_axis = dim
    else
        rotate_axis = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        select case (dim)
            case (1)
                m1 = n1
                m2 = n3
                m3 = n2
            case (2)
                m1 = n3
                m2 = n2
                m3 = n1
            case (3)
                m1 = n2
                m2 = n1
                m3 = n3
        end select
    else
        m1 = n1
        m2 = n2
        m3 = n3
    end if
    allocate (wr(1:m1, 1:m2, 1:m3))
    select case (dim)
        case (1)
            !$omp parallel do private(i)
            do i = 1, m1
                wr(i, :, :) = rot90_2d_double(w(i, :, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(j)
            do j = 1, m2
                wr(:, j, :) = rot90_2d_double(w(:, j, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(k)
            do k = 1, m3
                wr(:, :, k) = rot90_2d_double(w(:, :, k), cw=rotate_direction)
            end do
            !$omp end parallel do
    end select
end function rot90_3d_double
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
function rot90_1d_complex(w) result(wr)
    complex, dimension(:) :: w
    complex, allocatable, dimension(:, :) :: wr
    integer :: n
    n = size(w)
    allocate(wr(n, 1))
    wr(:, 1) = w(:)
end function rot90_1d_complex
function rot90_2d_complex(w, cw) result(wr)
    complex, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: cw
    complex, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    integer :: rotate_direction
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        allocate (wr(1:n2, 1:n1))
    else
        allocate (wr(1:n1, 1:n2))
    end if
    ! If no rotation eventually
    if (rotate_direction == 0) then
        wr = w
        return
    end if
    ! Otherwise
    select case (rotate_direction)
        case (1, -3)
            ! Clockwise by 90 degree or equivalently counter-clockwise by 270 degree
            do j = 1, n2
                do i = 1, n1
                    wr(j, n1 - i + 1) = w(i, j)
                end do
            end do
        case (2, -2)
            ! Clockwise by 180 degree or equivalently counter-clockwise by 180 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n1 - i + 1, n2 - j + 1) = w(i, j)
                end do
            end do
        case (3, -1)
            ! Clockwise by 270 degree or equivalently counter-clockwise by 90 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n2 - j + 1, i) = w(i, j)
                end do
            end do
    end select
end function rot90_2d_complex
function rot90_3d_complex(w, cw, dim) result(wr)
    complex, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: cw, dim
    complex, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    integer :: m1, m2, m3
    integer :: rotate_direction, rotate_axis
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    if (present(dim)) then
        call assert(dim == 1 .or. dim == 2 .or. dim == 3, ' dim /= 1, 2, 3')
        rotate_axis = dim
    else
        rotate_axis = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        select case (dim)
            case (1)
                m1 = n1
                m2 = n3
                m3 = n2
            case (2)
                m1 = n3
                m2 = n2
                m3 = n1
            case (3)
                m1 = n2
                m2 = n1
                m3 = n3
        end select
    else
        m1 = n1
        m2 = n2
        m3 = n3
    end if
    allocate (wr(1:m1, 1:m2, 1:m3))
    select case (dim)
        case (1)
            !$omp parallel do private(i)
            do i = 1, m1
                wr(i, :, :) = rot90_2d_complex(w(i, :, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(j)
            do j = 1, m2
                wr(:, j, :) = rot90_2d_complex(w(:, j, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(k)
            do k = 1, m3
                wr(:, :, k) = rot90_2d_complex(w(:, :, k), cw=rotate_direction)
            end do
            !$omp end parallel do
    end select
end function rot90_3d_complex
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
function rot90_1d_dcomplex(w) result(wr)
    double complex, dimension(:) :: w
    double complex, allocatable, dimension(:, :) :: wr
    integer :: n
    n = size(w)
    allocate(wr(n, 1))
    wr(:, 1) = w(:)
end function rot90_1d_dcomplex
function rot90_2d_dcomplex(w, cw) result(wr)
    double complex, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: cw
    double complex, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    integer :: rotate_direction
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        allocate (wr(1:n2, 1:n1))
    else
        allocate (wr(1:n1, 1:n2))
    end if
    ! If no rotation eventually
    if (rotate_direction == 0) then
        wr = w
        return
    end if
    ! Otherwise
    select case (rotate_direction)
        case (1, -3)
            ! Clockwise by 90 degree or equivalently counter-clockwise by 270 degree
            do j = 1, n2
                do i = 1, n1
                    wr(j, n1 - i + 1) = w(i, j)
                end do
            end do
        case (2, -2)
            ! Clockwise by 180 degree or equivalently counter-clockwise by 180 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n1 - i + 1, n2 - j + 1) = w(i, j)
                end do
            end do
        case (3, -1)
            ! Clockwise by 270 degree or equivalently counter-clockwise by 90 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n2 - j + 1, i) = w(i, j)
                end do
            end do
    end select
end function rot90_2d_dcomplex
function rot90_3d_dcomplex(w, cw, dim) result(wr)
    double complex, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: cw, dim
    double complex, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    integer :: m1, m2, m3
    integer :: rotate_direction, rotate_axis
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    if (present(dim)) then
        call assert(dim == 1 .or. dim == 2 .or. dim == 3, ' dim /= 1, 2, 3')
        rotate_axis = dim
    else
        rotate_axis = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        select case (dim)
            case (1)
                m1 = n1
                m2 = n3
                m3 = n2
            case (2)
                m1 = n3
                m2 = n2
                m3 = n1
            case (3)
                m1 = n2
                m2 = n1
                m3 = n3
        end select
    else
        m1 = n1
        m2 = n2
        m3 = n3
    end if
    allocate (wr(1:m1, 1:m2, 1:m3))
    select case (dim)
        case (1)
            !$omp parallel do private(i)
            do i = 1, m1
                wr(i, :, :) = rot90_2d_dcomplex(w(i, :, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(j)
            do j = 1, m2
                wr(:, j, :) = rot90_2d_dcomplex(w(:, j, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(k)
            do k = 1, m3
                wr(:, :, k) = rot90_2d_dcomplex(w(:, :, k), cw=rotate_direction)
            end do
            !$omp end parallel do
    end select
end function rot90_3d_dcomplex
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
function rot90_1d_logical(w) result(wr)
    logical, dimension(:) :: w
    logical, allocatable, dimension(:, :) :: wr
    integer :: n
    n = size(w)
    allocate(wr(n, 1))
    wr(:, 1) = w(:)
end function rot90_1d_logical
function rot90_2d_logical(w, cw) result(wr)
    logical, dimension(:, :), intent(in) :: w
    integer, intent(in), optional :: cw
    logical, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    integer :: rotate_direction
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        allocate (wr(1:n2, 1:n1))
    else
        allocate (wr(1:n1, 1:n2))
    end if
    ! If no rotation eventually
    if (rotate_direction == 0) then
        wr = w
        return
    end if
    ! Otherwise
    select case (rotate_direction)
        case (1, -3)
            ! Clockwise by 90 degree or equivalently counter-clockwise by 270 degree
            do j = 1, n2
                do i = 1, n1
                    wr(j, n1 - i + 1) = w(i, j)
                end do
            end do
        case (2, -2)
            ! Clockwise by 180 degree or equivalently counter-clockwise by 180 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n1 - i + 1, n2 - j + 1) = w(i, j)
                end do
            end do
        case (3, -1)
            ! Clockwise by 270 degree or equivalently counter-clockwise by 90 degree
            do j = 1, n2
                do i = 1, n1
                    wr(n2 - j + 1, i) = w(i, j)
                end do
            end do
    end select
end function rot90_2d_logical
function rot90_3d_logical(w, cw, dim) result(wr)
    logical, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: cw, dim
    logical, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    integer :: m1, m2, m3
    integer :: rotate_direction, rotate_axis
    if (present(cw)) then
        rotate_direction = cw
    else
        rotate_direction = 1
    end if
    rotate_direction = mod(rotate_direction, 4)
    if (present(dim)) then
        call assert(dim == 1 .or. dim == 2 .or. dim == 3, ' dim /= 1, 2, 3')
        rotate_axis = dim
    else
        rotate_axis = 1
    end if
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Allocate memory
    if (abs(rotate_direction) == 1 .or. abs(rotate_direction) == 3) then
        select case (dim)
            case (1)
                m1 = n1
                m2 = n3
                m3 = n2
            case (2)
                m1 = n3
                m2 = n2
                m3 = n1
            case (3)
                m1 = n2
                m2 = n1
                m3 = n3
        end select
    else
        m1 = n1
        m2 = n2
        m3 = n3
    end if
    allocate (wr(1:m1, 1:m2, 1:m3))
    select case (dim)
        case (1)
            !$omp parallel do private(i)
            do i = 1, m1
                wr(i, :, :) = rot90_2d_logical(w(i, :, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(j)
            do j = 1, m2
                wr(:, j, :) = rot90_2d_logical(w(:, j, :), cw=rotate_direction)
            end do
            !$omp end parallel do
        case (3)
            !$omp parallel do private(k)
            do k = 1, m3
                wr(:, :, k) = rot90_2d_logical(w(:, :, k), cw=rotate_direction)
            end do
            !$omp end parallel do
    end select
end function rot90_3d_logical
    !================================================================
    ! Slice array
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
!> Take a slice from a 1D array
!
!> @param[in] w The input 1D array in
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The scalar sliced from the 1D array
!
function slice_1d_int(w, index) result(wr)
    integer, dimension(:) :: w
    integer :: index
    integer :: wr
    wr = w(index)
end function slice_1d_int
!
!> Take a slice from a 2D array
!
!> @param[in] w The input 2D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 1D array sliced from the 2D array
!
function slice_2d_int(w, dim, index) result(wr)
    integer, dimension(:, :) :: w
    integer :: dim, index
    integer, allocatable, dimension(:) :: wr
    integer :: n1, n2
    n1 = size(w, 1)
    n2 = size(w, 2)
    select case (dim)
        case (1)
            allocate (wr(1:n2))
            wr(:) = w(index, :)
        case (2)
            allocate (wr(1:n1))
            wr(:) = w(:, index)
    end select
end function slice_2d_int
!
!> Take a slice from a 3D array
!
!> @param[in] w The input 3D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 2D array sliced from the 3D array
!
function slice_3d_int(w, dim, index) result(wr)
    integer, dimension(:, :, :) :: w
    integer :: dim, index
    integer, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, n3
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    select case (dim)
        case (1)
            allocate (wr(1:n2, 1:n3))
            wr(:, :) = w(index, :, :)
        case (2)
            allocate (wr(1:n1, 1:n3))
            wr(:, :) = w(:, index, :)
        case (3)
            allocate (wr(1:n1, 1:n2))
            wr(:, :) = w(:, :, index)
    end select
end function slice_3d_int
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
!> Take a slice from a 1D array
!
!> @param[in] w The input 1D array in
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The scalar sliced from the 1D array
!
function slice_1d_float(w, index) result(wr)
    real, dimension(:) :: w
    integer :: index
    real :: wr
    wr = w(index)
end function slice_1d_float
!
!> Take a slice from a 2D array
!
!> @param[in] w The input 2D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 1D array sliced from the 2D array
!
function slice_2d_float(w, dim, index) result(wr)
    real, dimension(:, :) :: w
    integer :: dim, index
    real, allocatable, dimension(:) :: wr
    integer :: n1, n2
    n1 = size(w, 1)
    n2 = size(w, 2)
    select case (dim)
        case (1)
            allocate (wr(1:n2))
            wr(:) = w(index, :)
        case (2)
            allocate (wr(1:n1))
            wr(:) = w(:, index)
    end select
end function slice_2d_float
!
!> Take a slice from a 3D array
!
!> @param[in] w The input 3D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 2D array sliced from the 3D array
!
function slice_3d_float(w, dim, index) result(wr)
    real, dimension(:, :, :) :: w
    integer :: dim, index
    real, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, n3
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    select case (dim)
        case (1)
            allocate (wr(1:n2, 1:n3))
            wr(:, :) = w(index, :, :)
        case (2)
            allocate (wr(1:n1, 1:n3))
            wr(:, :) = w(:, index, :)
        case (3)
            allocate (wr(1:n1, 1:n2))
            wr(:, :) = w(:, :, index)
    end select
end function slice_3d_float
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
!> Take a slice from a 1D array
!
!> @param[in] w The input 1D array in
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The scalar sliced from the 1D array
!
function slice_1d_double(w, index) result(wr)
    double precision, dimension(:) :: w
    integer :: index
    double precision :: wr
    wr = w(index)
end function slice_1d_double
!
!> Take a slice from a 2D array
!
!> @param[in] w The input 2D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 1D array sliced from the 2D array
!
function slice_2d_double(w, dim, index) result(wr)
    double precision, dimension(:, :) :: w
    integer :: dim, index
    double precision, allocatable, dimension(:) :: wr
    integer :: n1, n2
    n1 = size(w, 1)
    n2 = size(w, 2)
    select case (dim)
        case (1)
            allocate (wr(1:n2))
            wr(:) = w(index, :)
        case (2)
            allocate (wr(1:n1))
            wr(:) = w(:, index)
    end select
end function slice_2d_double
!
!> Take a slice from a 3D array
!
!> @param[in] w The input 3D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 2D array sliced from the 3D array
!
function slice_3d_double(w, dim, index) result(wr)
    double precision, dimension(:, :, :) :: w
    integer :: dim, index
    double precision, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, n3
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    select case (dim)
        case (1)
            allocate (wr(1:n2, 1:n3))
            wr(:, :) = w(index, :, :)
        case (2)
            allocate (wr(1:n1, 1:n3))
            wr(:, :) = w(:, index, :)
        case (3)
            allocate (wr(1:n1, 1:n2))
            wr(:, :) = w(:, :, index)
    end select
end function slice_3d_double
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
!> Take a slice from a 1D array
!
!> @param[in] w The input 1D array in
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The scalar sliced from the 1D array
!
function slice_1d_complex(w, index) result(wr)
    complex, dimension(:) :: w
    integer :: index
    complex :: wr
    wr = w(index)
end function slice_1d_complex
!
!> Take a slice from a 2D array
!
!> @param[in] w The input 2D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 1D array sliced from the 2D array
!
function slice_2d_complex(w, dim, index) result(wr)
    complex, dimension(:, :) :: w
    integer :: dim, index
    complex, allocatable, dimension(:) :: wr
    integer :: n1, n2
    n1 = size(w, 1)
    n2 = size(w, 2)
    select case (dim)
        case (1)
            allocate (wr(1:n2))
            wr(:) = w(index, :)
        case (2)
            allocate (wr(1:n1))
            wr(:) = w(:, index)
    end select
end function slice_2d_complex
!
!> Take a slice from a 3D array
!
!> @param[in] w The input 3D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 2D array sliced from the 3D array
!
function slice_3d_complex(w, dim, index) result(wr)
    complex, dimension(:, :, :) :: w
    integer :: dim, index
    complex, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, n3
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    select case (dim)
        case (1)
            allocate (wr(1:n2, 1:n3))
            wr(:, :) = w(index, :, :)
        case (2)
            allocate (wr(1:n1, 1:n3))
            wr(:, :) = w(:, index, :)
        case (3)
            allocate (wr(1:n1, 1:n2))
            wr(:, :) = w(:, :, index)
    end select
end function slice_3d_complex
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
!> Take a slice from a 1D array
!
!> @param[in] w The input 1D array in
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The scalar sliced from the 1D array
!
function slice_1d_dcomplex(w, index) result(wr)
    double complex, dimension(:) :: w
    integer :: index
    double complex :: wr
    wr = w(index)
end function slice_1d_dcomplex
!
!> Take a slice from a 2D array
!
!> @param[in] w The input 2D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 1D array sliced from the 2D array
!
function slice_2d_dcomplex(w, dim, index) result(wr)
    double complex, dimension(:, :) :: w
    integer :: dim, index
    double complex, allocatable, dimension(:) :: wr
    integer :: n1, n2
    n1 = size(w, 1)
    n2 = size(w, 2)
    select case (dim)
        case (1)
            allocate (wr(1:n2))
            wr(:) = w(index, :)
        case (2)
            allocate (wr(1:n1))
            wr(:) = w(:, index)
    end select
end function slice_2d_dcomplex
!
!> Take a slice from a 3D array
!
!> @param[in] w The input 3D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 2D array sliced from the 3D array
!
function slice_3d_dcomplex(w, dim, index) result(wr)
    double complex, dimension(:, :, :) :: w
    integer :: dim, index
    double complex, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, n3
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    select case (dim)
        case (1)
            allocate (wr(1:n2, 1:n3))
            wr(:, :) = w(index, :, :)
        case (2)
            allocate (wr(1:n1, 1:n3))
            wr(:, :) = w(:, index, :)
        case (3)
            allocate (wr(1:n1, 1:n2))
            wr(:, :) = w(:, :, index)
    end select
end function slice_3d_dcomplex
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
!> Take a slice from a 1D array
!
!> @param[in] w The input 1D array in
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The scalar sliced from the 1D array
!
function slice_1d_logical(w, index) result(wr)
    logical, dimension(:) :: w
    integer :: index
    logical :: wr
    wr = w(index)
end function slice_1d_logical
!
!> Take a slice from a 2D array
!
!> @param[in] w The input 2D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 1D array sliced from the 2D array
!
function slice_2d_logical(w, dim, index) result(wr)
    logical, dimension(:, :) :: w
    integer :: dim, index
    logical, allocatable, dimension(:) :: wr
    integer :: n1, n2
    n1 = size(w, 1)
    n2 = size(w, 2)
    select case (dim)
        case (1)
            allocate (wr(1:n2))
            wr(:) = w(index, :)
        case (2)
            allocate (wr(1:n1))
            wr(:) = w(:, index)
    end select
end function slice_2d_logical
!
!> Take a slice from a 3D array
!
!> @param[in] w The input 3D array in
!> @param[in] dim Which dimension to slice
!> @param[in] index The index of slicing, must be 1 <= index <= dim_size
!> @return wr The 2D array sliced from the 3D array
!
function slice_3d_logical(w, dim, index) result(wr)
    logical, dimension(:, :, :) :: w
    integer :: dim, index
    logical, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, n3
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    select case (dim)
        case (1)
            allocate (wr(1:n2, 1:n3))
            wr(:, :) = w(index, :, :)
        case (2)
            allocate (wr(1:n1, 1:n3))
            wr(:, :) = w(:, index, :)
        case (3)
            allocate (wr(1:n1, 1:n2))
            wr(:, :) = w(:, :, index)
    end select
end function slice_3d_logical
    !================================================================
    ! Repeat array
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
!> Repeat a 1D araay
!
function tile_1d_int(w, n) result(wr)
    integer, dimension(:) :: w
    integer :: n
    integer, allocatable, dimension(:) :: wr
    integer :: n1, i
    n1 = size(w)
    allocate(wr(1:n*n1))
    do i = 1, n
        wr((i - 1)*n1 + 1:i*n1) = w
    end do
end function tile_1d_int
!
!> Repeat a 2D araay
!
function tile_2d_int(w, n) result(wr)
    integer, dimension(:, :) :: w
    integer, dimension(1:2) :: n
    integer, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2))
    !$omp parallel do private(i, j)
    do j = 1, n(2)
        do i = 1, n(1)
            wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2) = w
        end do
    end do
    !$omp end parallel do
end function tile_2d_int
!
!> Repeat a 3D araay
!
function tile_3d_int(w, n) result(wr)
    integer, dimension(:, :, :) :: w
    integer, dimension(1:3) :: n
    integer, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3))
    !$omp parallel do private(i, j, k)
    do k = 1, n(3)
        do j = 1, n(2)
            do i = 1, n(1)
                wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3) = w
            end do
        end do
    end do
    !$omp end parallel do
end function tile_3d_int
!
!> Repeat a 4D araay
!
function tile_4d_int(w, n) result(wr)
    integer, dimension(:, :, :, :) :: w
    integer, dimension(1:4) :: n
    integer, allocatable, dimension(:, :, :, :) :: wr
    integer :: n1, n2, n3, n4, i, j, k, l
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    n4 = size(w, 4)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3, 1:n(4)*n4))
    !$omp parallel do private(i, j, k, l)
    do l = 1, n(4)
        do k = 1, n(3)
            do j = 1, n(2)
                do i = 1, n(1)
                    wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3, (l - 1)*n4:l*n4) = w
                end do
            end do
        end do
    end do
    !$omp end parallel do
end function tile_4d_int
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
!> Repeat a 1D araay
!
function tile_1d_float(w, n) result(wr)
    real, dimension(:) :: w
    integer :: n
    real, allocatable, dimension(:) :: wr
    integer :: n1, i
    n1 = size(w)
    allocate(wr(1:n*n1))
    do i = 1, n
        wr((i - 1)*n1 + 1:i*n1) = w
    end do
end function tile_1d_float
!
!> Repeat a 2D araay
!
function tile_2d_float(w, n) result(wr)
    real, dimension(:, :) :: w
    integer, dimension(1:2) :: n
    real, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2))
    !$omp parallel do private(i, j)
    do j = 1, n(2)
        do i = 1, n(1)
            wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2) = w
        end do
    end do
    !$omp end parallel do
end function tile_2d_float
!
!> Repeat a 3D araay
!
function tile_3d_float(w, n) result(wr)
    real, dimension(:, :, :) :: w
    integer, dimension(1:3) :: n
    real, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3))
    !$omp parallel do private(i, j, k)
    do k = 1, n(3)
        do j = 1, n(2)
            do i = 1, n(1)
                wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3) = w
            end do
        end do
    end do
    !$omp end parallel do
end function tile_3d_float
!
!> Repeat a 4D araay
!
function tile_4d_float(w, n) result(wr)
    real, dimension(:, :, :, :) :: w
    integer, dimension(1:4) :: n
    real, allocatable, dimension(:, :, :, :) :: wr
    integer :: n1, n2, n3, n4, i, j, k, l
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    n4 = size(w, 4)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3, 1:n(4)*n4))
    !$omp parallel do private(i, j, k, l)
    do l = 1, n(4)
        do k = 1, n(3)
            do j = 1, n(2)
                do i = 1, n(1)
                    wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3, (l - 1)*n4:l*n4) = w
                end do
            end do
        end do
    end do
    !$omp end parallel do
end function tile_4d_float
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
!> Repeat a 1D araay
!
function tile_1d_double(w, n) result(wr)
    double precision, dimension(:) :: w
    integer :: n
    double precision, allocatable, dimension(:) :: wr
    integer :: n1, i
    n1 = size(w)
    allocate(wr(1:n*n1))
    do i = 1, n
        wr((i - 1)*n1 + 1:i*n1) = w
    end do
end function tile_1d_double
!
!> Repeat a 2D araay
!
function tile_2d_double(w, n) result(wr)
    double precision, dimension(:, :) :: w
    integer, dimension(1:2) :: n
    double precision, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2))
    !$omp parallel do private(i, j)
    do j = 1, n(2)
        do i = 1, n(1)
            wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2) = w
        end do
    end do
    !$omp end parallel do
end function tile_2d_double
!
!> Repeat a 3D araay
!
function tile_3d_double(w, n) result(wr)
    double precision, dimension(:, :, :) :: w
    integer, dimension(1:3) :: n
    double precision, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3))
    !$omp parallel do private(i, j, k)
    do k = 1, n(3)
        do j = 1, n(2)
            do i = 1, n(1)
                wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3) = w
            end do
        end do
    end do
    !$omp end parallel do
end function tile_3d_double
!
!> Repeat a 4D araay
!
function tile_4d_double(w, n) result(wr)
    double precision, dimension(:, :, :, :) :: w
    integer, dimension(1:4) :: n
    double precision, allocatable, dimension(:, :, :, :) :: wr
    integer :: n1, n2, n3, n4, i, j, k, l
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    n4 = size(w, 4)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3, 1:n(4)*n4))
    !$omp parallel do private(i, j, k, l)
    do l = 1, n(4)
        do k = 1, n(3)
            do j = 1, n(2)
                do i = 1, n(1)
                    wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3, (l - 1)*n4:l*n4) = w
                end do
            end do
        end do
    end do
    !$omp end parallel do
end function tile_4d_double
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
!> Repeat a 1D araay
!
function tile_1d_complex(w, n) result(wr)
    complex, dimension(:) :: w
    integer :: n
    complex, allocatable, dimension(:) :: wr
    integer :: n1, i
    n1 = size(w)
    allocate(wr(1:n*n1))
    do i = 1, n
        wr((i - 1)*n1 + 1:i*n1) = w
    end do
end function tile_1d_complex
!
!> Repeat a 2D araay
!
function tile_2d_complex(w, n) result(wr)
    complex, dimension(:, :) :: w
    integer, dimension(1:2) :: n
    complex, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2))
    !$omp parallel do private(i, j)
    do j = 1, n(2)
        do i = 1, n(1)
            wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2) = w
        end do
    end do
    !$omp end parallel do
end function tile_2d_complex
!
!> Repeat a 3D araay
!
function tile_3d_complex(w, n) result(wr)
    complex, dimension(:, :, :) :: w
    integer, dimension(1:3) :: n
    complex, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3))
    !$omp parallel do private(i, j, k)
    do k = 1, n(3)
        do j = 1, n(2)
            do i = 1, n(1)
                wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3) = w
            end do
        end do
    end do
    !$omp end parallel do
end function tile_3d_complex
!
!> Repeat a 4D araay
!
function tile_4d_complex(w, n) result(wr)
    complex, dimension(:, :, :, :) :: w
    integer, dimension(1:4) :: n
    complex, allocatable, dimension(:, :, :, :) :: wr
    integer :: n1, n2, n3, n4, i, j, k, l
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    n4 = size(w, 4)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3, 1:n(4)*n4))
    !$omp parallel do private(i, j, k, l)
    do l = 1, n(4)
        do k = 1, n(3)
            do j = 1, n(2)
                do i = 1, n(1)
                    wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3, (l - 1)*n4:l*n4) = w
                end do
            end do
        end do
    end do
    !$omp end parallel do
end function tile_4d_complex
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
!> Repeat a 1D araay
!
function tile_1d_dcomplex(w, n) result(wr)
    double complex, dimension(:) :: w
    integer :: n
    double complex, allocatable, dimension(:) :: wr
    integer :: n1, i
    n1 = size(w)
    allocate(wr(1:n*n1))
    do i = 1, n
        wr((i - 1)*n1 + 1:i*n1) = w
    end do
end function tile_1d_dcomplex
!
!> Repeat a 2D araay
!
function tile_2d_dcomplex(w, n) result(wr)
    double complex, dimension(:, :) :: w
    integer, dimension(1:2) :: n
    double complex, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2))
    !$omp parallel do private(i, j)
    do j = 1, n(2)
        do i = 1, n(1)
            wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2) = w
        end do
    end do
    !$omp end parallel do
end function tile_2d_dcomplex
!
!> Repeat a 3D araay
!
function tile_3d_dcomplex(w, n) result(wr)
    double complex, dimension(:, :, :) :: w
    integer, dimension(1:3) :: n
    double complex, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3))
    !$omp parallel do private(i, j, k)
    do k = 1, n(3)
        do j = 1, n(2)
            do i = 1, n(1)
                wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3) = w
            end do
        end do
    end do
    !$omp end parallel do
end function tile_3d_dcomplex
!
!> Repeat a 4D araay
!
function tile_4d_dcomplex(w, n) result(wr)
    double complex, dimension(:, :, :, :) :: w
    integer, dimension(1:4) :: n
    double complex, allocatable, dimension(:, :, :, :) :: wr
    integer :: n1, n2, n3, n4, i, j, k, l
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    n4 = size(w, 4)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3, 1:n(4)*n4))
    !$omp parallel do private(i, j, k, l)
    do l = 1, n(4)
        do k = 1, n(3)
            do j = 1, n(2)
                do i = 1, n(1)
                    wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3, (l - 1)*n4:l*n4) = w
                end do
            end do
        end do
    end do
    !$omp end parallel do
end function tile_4d_dcomplex
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
!> Repeat a 1D araay
!
function tile_1d_logical(w, n) result(wr)
    logical, dimension(:) :: w
    integer :: n
    logical, allocatable, dimension(:) :: wr
    integer :: n1, i
    n1 = size(w)
    allocate(wr(1:n*n1))
    do i = 1, n
        wr((i - 1)*n1 + 1:i*n1) = w
    end do
end function tile_1d_logical
!
!> Repeat a 2D araay
!
function tile_2d_logical(w, n) result(wr)
    logical, dimension(:, :) :: w
    integer, dimension(1:2) :: n
    logical, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j
    n1 = size(w, 1)
    n2 = size(w, 2)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2))
    !$omp parallel do private(i, j)
    do j = 1, n(2)
        do i = 1, n(1)
            wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2) = w
        end do
    end do
    !$omp end parallel do
end function tile_2d_logical
!
!> Repeat a 3D araay
!
function tile_3d_logical(w, n) result(wr)
    logical, dimension(:, :, :) :: w
    integer, dimension(1:3) :: n
    logical, allocatable, dimension(:, :, :) :: wr
    integer :: n1, n2, n3, i, j, k
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3))
    !$omp parallel do private(i, j, k)
    do k = 1, n(3)
        do j = 1, n(2)
            do i = 1, n(1)
                wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3) = w
            end do
        end do
    end do
    !$omp end parallel do
end function tile_3d_logical
!
!> Repeat a 4D araay
!
function tile_4d_logical(w, n) result(wr)
    logical, dimension(:, :, :, :) :: w
    integer, dimension(1:4) :: n
    logical, allocatable, dimension(:, :, :, :) :: wr
    integer :: n1, n2, n3, n4, i, j, k, l
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    n4 = size(w, 4)
    allocate(wr(1:n(1)*n1, 1:n(2)*n2, 1:n(3)*n3, 1:n(4)*n4))
    !$omp parallel do private(i, j, k, l)
    do l = 1, n(4)
        do k = 1, n(3)
            do j = 1, n(2)
                do i = 1, n(1)
                    wr((i - 1)*n1 + 1:i*n1, (j - 1)*n2 + 1:j*n2, (k - 1)*n3 + 1:k*n3, (l - 1)*n4:l*n4) = w
                end do
            end do
        end do
    end do
    !$omp end parallel do
end function tile_4d_logical
    function tile_1d_string(w, n) result(wr)
        character(len=*) :: w
        integer :: n
        character(len=:), allocatable :: wr
        integer :: n1, i
        n1 = len(trim(adjustl(w)))
        allocate (character(len=n*n1) :: wr)
        do i = 1, n
            wr((i - 1)*n1 + 1:i*n1) = w
        end do
    end function tile_1d_string
    !================================================================
    ! Binarize array
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
function binarize_1d_int(w, sep, value) result(wt)
    integer, dimension(:), intent(in) :: w
    integer, intent(in) :: sep
    integer, dimension(:), intent(in) :: value
    integer, allocatable, dimension(:) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_1d_int
function binarize_2d_int(w, sep, value) result(wt)
    integer, dimension(:, :), intent(in) :: w
    integer, intent(in) :: sep
    integer, dimension(:), intent(in) :: value
    integer, allocatable, dimension(:, :) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_2d_int
function binarize_3d_int(w, sep, value) result(wt)
    integer, dimension(:, :, :), intent(in) :: w
    integer, intent(in) :: sep
    integer, dimension(:), intent(in) :: value
    integer, allocatable, dimension(:, :, :) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_3d_int
function binarize_4d_int(w, sep, value) result(wt)
    integer, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in) :: sep
    integer, dimension(:), intent(in) :: value
    integer, allocatable, dimension(:, :, :, :) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_4d_int
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
function binarize_1d_float(w, sep, value) result(wt)
    real, dimension(:), intent(in) :: w
    real, intent(in) :: sep
    real, dimension(:), intent(in) :: value
    real, allocatable, dimension(:) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_1d_float
function binarize_2d_float(w, sep, value) result(wt)
    real, dimension(:, :), intent(in) :: w
    real, intent(in) :: sep
    real, dimension(:), intent(in) :: value
    real, allocatable, dimension(:, :) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_2d_float
function binarize_3d_float(w, sep, value) result(wt)
    real, dimension(:, :, :), intent(in) :: w
    real, intent(in) :: sep
    real, dimension(:), intent(in) :: value
    real, allocatable, dimension(:, :, :) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_3d_float
function binarize_4d_float(w, sep, value) result(wt)
    real, dimension(:, :, :, :), intent(in) :: w
    real, intent(in) :: sep
    real, dimension(:), intent(in) :: value
    real, allocatable, dimension(:, :, :, :) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_4d_float
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
function binarize_1d_double(w, sep, value) result(wt)
    double precision, dimension(:), intent(in) :: w
    double precision, intent(in) :: sep
    double precision, dimension(:), intent(in) :: value
    double precision, allocatable, dimension(:) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_1d_double
function binarize_2d_double(w, sep, value) result(wt)
    double precision, dimension(:, :), intent(in) :: w
    double precision, intent(in) :: sep
    double precision, dimension(:), intent(in) :: value
    double precision, allocatable, dimension(:, :) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_2d_double
function binarize_3d_double(w, sep, value) result(wt)
    double precision, dimension(:, :, :), intent(in) :: w
    double precision, intent(in) :: sep
    double precision, dimension(:), intent(in) :: value
    double precision, allocatable, dimension(:, :, :) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_3d_double
function binarize_4d_double(w, sep, value) result(wt)
    double precision, dimension(:, :, :, :), intent(in) :: w
    double precision, intent(in) :: sep
    double precision, dimension(:), intent(in) :: value
    double precision, allocatable, dimension(:, :, :, :) :: wt
    wt = zeros_like(w)
    where (w < sep)
        wt = value(1)
    end where
    where (w >= sep)
        wt = value(2)
    end where
end function binarize_4d_double
end module libflit_array_operation
