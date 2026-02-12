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
module libflit_array
    use libflit_constants
    use libflit_error
    implicit none
    interface alloc_array
        module procedure :: alloc_array_1d_int
        module procedure :: alloc_array_2d_int
        module procedure :: alloc_array_3d_int
        module procedure :: alloc_array_4d_int
        module procedure :: alloc_array_1d_float
        module procedure :: alloc_array_2d_float
        module procedure :: alloc_array_3d_float
        module procedure :: alloc_array_4d_float
        module procedure :: alloc_array_1d_double
        module procedure :: alloc_array_2d_double
        module procedure :: alloc_array_3d_double
        module procedure :: alloc_array_4d_double
        module procedure :: alloc_array_1d_complex
        module procedure :: alloc_array_2d_complex
        module procedure :: alloc_array_3d_complex
        module procedure :: alloc_array_4d_complex
        module procedure :: alloc_array_1d_dcomplex
        module procedure :: alloc_array_2d_dcomplex
        module procedure :: alloc_array_3d_dcomplex
        module procedure :: alloc_array_4d_dcomplex
        module procedure :: alloc_array_1d_logical
        module procedure :: alloc_array_2d_logical
        module procedure :: alloc_array_3d_logical
        module procedure :: alloc_array_4d_logical
        module procedure :: alloc_array_1d_string
        module procedure :: alloc_array_2d_string
        module procedure :: alloc_array_3d_string
        module procedure :: alloc_array_4d_string
        module procedure :: alloc_large_array_1d_int
        module procedure :: alloc_large_array_1d_float
        module procedure :: alloc_large_array_1d_double
        module procedure :: alloc_large_array_1d_complex
        module procedure :: alloc_large_array_1d_dcomplex
        module procedure :: alloc_large_array_1d_logical
        module procedure :: alloc_large_array_1d_string
    end interface alloc_array
    interface regspace
        module procedure :: regspace_int
        module procedure :: regspace_float
        module procedure :: regspace_double
    end interface regspace
    interface linspace
        module procedure :: linspace_float
        module procedure :: linspace_double
        module procedure :: linspace_complex
        module procedure :: linspace_dcomplex
    end interface linspace
    interface logspace
        module procedure :: logspace_float
        module procedure :: logspace_double
        module procedure :: logspace_complex
        module procedure :: logspace_dcomplex
    end interface logspace
    interface const
        module procedure :: const_1d_int
        module procedure :: const_2d_int
        module procedure :: const_3d_int
        module procedure :: const_4d_int
        module procedure :: const_1d_float
        module procedure :: const_2d_float
        module procedure :: const_3d_float
        module procedure :: const_4d_float
        module procedure :: const_1d_double
        module procedure :: const_2d_double
        module procedure :: const_3d_double
        module procedure :: const_4d_double
        module procedure :: const_1d_complex
        module procedure :: const_2d_complex
        module procedure :: const_3d_complex
        module procedure :: const_4d_complex
        module procedure :: const_1d_dcomplex
        module procedure :: const_2d_dcomplex
        module procedure :: const_3d_dcomplex
        module procedure :: const_4d_dcomplex
        module procedure :: const_1d_logical
        module procedure :: const_2d_logical
        module procedure :: const_3d_logical
        module procedure :: const_4d_logical
        module procedure :: const_1d_string
        module procedure :: const_2d_string
        module procedure :: const_3d_string
        module procedure :: const_4d_string
    end interface const
    interface const_like
        module procedure :: const_like_1d_int
        module procedure :: const_like_2d_int
        module procedure :: const_like_3d_int
        module procedure :: const_like_4d_int
        module procedure :: const_like_1d_float
        module procedure :: const_like_2d_float
        module procedure :: const_like_3d_float
        module procedure :: const_like_4d_float
        module procedure :: const_like_1d_double
        module procedure :: const_like_2d_double
        module procedure :: const_like_3d_double
        module procedure :: const_like_4d_double
        module procedure :: const_like_1d_complex
        module procedure :: const_like_2d_complex
        module procedure :: const_like_3d_complex
        module procedure :: const_like_4d_complex
        module procedure :: const_like_1d_dcomplex
        module procedure :: const_like_2d_dcomplex
        module procedure :: const_like_3d_dcomplex
        module procedure :: const_like_4d_dcomplex
        module procedure :: const_like_1d_logical
        module procedure :: const_like_2d_logical
        module procedure :: const_like_3d_logical
        module procedure :: const_like_4d_logical
        module procedure :: const_like_1d_string
        module procedure :: const_like_2d_string
        module procedure :: const_like_3d_string
        module procedure :: const_like_4d_string
    end interface const_like
    interface zeros
        module procedure :: zeros_1d_float
        module procedure :: zeros_2d_float
        module procedure :: zeros_3d_float
        module procedure :: zeros_4d_float
        module procedure :: zeros_5d_float
    end interface zeros
    interface zeros_like
        module procedure :: zeros_like_1d_int
        module procedure :: zeros_like_2d_int
        module procedure :: zeros_like_3d_int
        module procedure :: zeros_like_4d_int
        module procedure :: zeros_like_1d_float
        module procedure :: zeros_like_2d_float
        module procedure :: zeros_like_3d_float
        module procedure :: zeros_like_4d_float
        module procedure :: zeros_like_1d_double
        module procedure :: zeros_like_2d_double
        module procedure :: zeros_like_3d_double
        module procedure :: zeros_like_4d_double
        module procedure :: zeros_like_1d_complex
        module procedure :: zeros_like_2d_complex
        module procedure :: zeros_like_3d_complex
        module procedure :: zeros_like_4d_complex
        module procedure :: zeros_like_1d_dcomplex
        module procedure :: zeros_like_2d_dcomplex
        module procedure :: zeros_like_3d_dcomplex
        module procedure :: zeros_like_4d_dcomplex
    end interface zeros_like
    interface ones
        module procedure :: ones_1d_float
        module procedure :: ones_2d_float
        module procedure :: ones_3d_float
        module procedure :: ones_4d_float
    end interface ones
    interface ones_like
        module procedure :: ones_like_1d_int
        module procedure :: ones_like_2d_int
        module procedure :: ones_like_3d_int
        module procedure :: ones_like_4d_int
        module procedure :: ones_like_1d_float
        module procedure :: ones_like_2d_float
        module procedure :: ones_like_3d_float
        module procedure :: ones_like_4d_float
        module procedure :: ones_like_1d_double
        module procedure :: ones_like_2d_double
        module procedure :: ones_like_3d_double
        module procedure :: ones_like_4d_double
        module procedure :: ones_like_1d_complex
        module procedure :: ones_like_2d_complex
        module procedure :: ones_like_3d_complex
        module procedure :: ones_like_4d_complex
        module procedure :: ones_like_1d_dcomplex
        module procedure :: ones_like_2d_dcomplex
        module procedure :: ones_like_3d_dcomplex
        module procedure :: ones_like_4d_dcomplex
    end interface ones_like
    interface falses
        module procedure :: falses_1d
        module procedure :: falses_2d
        module procedure :: falses_3d
        module procedure :: falses_4d
    end interface falses
    interface falses_like
        module procedure :: falses_like_1d
        module procedure :: falses_like_2d
        module procedure :: falses_like_3d
        module procedure :: falses_like_4d
    end interface falses_like
    interface trues
        module procedure :: trues_1d
        module procedure :: trues_2d
        module procedure :: trues_3d
        module procedure :: trues_4d
    end interface trues
    interface trues_like
        module procedure :: trues_like_1d
        module procedure :: trues_like_2d
        module procedure :: trues_like_3d
        module procedure :: trues_like_4d
    end interface trues_like
    interface diag
        module procedure :: get_diagonal_2d_int
        module procedure :: get_diagonal_2d_float
        module procedure :: get_diagonal_2d_double
        module procedure :: get_diagonal_2d_complex
        module procedure :: get_diagonal_2d_dcomplex
        module procedure :: get_diagonal_2d_logical
    end interface diag
    interface eye
        module procedure :: eye_2d_int
        module procedure :: eye_2d_float
        module procedure :: eye_2d_complex
        module procedure :: eye_2d_double
        module procedure :: eye_2d_dcomplex
        module procedure :: eye_2d_logical
        module procedure :: eye_2d_from_vector_int
        module procedure :: eye_2d_from_vector_float
        module procedure :: eye_2d_from_vector_complex
        module procedure :: eye_2d_from_vector_double
        module procedure :: eye_2d_from_vector_dcomplex
        module procedure :: eye_2d_from_vector_logical
    end interface eye
    private
    public :: alloc_array
    public :: const
    public :: zeros
    public :: ones
    public :: const_like
    public :: zeros_like
    public :: ones_like
    public :: falses
    public :: trues
    public :: falses_like
    public :: trues_like
    public :: eye
    public :: diag
    public :: regspace
    public :: linspace
    public :: logspace
    ! - Array allocation
    ! allocatable array can directly assign, like x = y even if x
    ! is not allocated, and x's shape and bounds will be same
    ! as thos of y
    ! - Array fast allocation using constant
    ! b = [(0., integer :: i = 1,5)]
contains
    ! Allocate array with safe deallocation
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
subroutine alloc_array_1d_int(array, n, pad, source)
    integer, allocatable, dimension(:), intent(inout) :: array
    integer, dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    integer, dimension(:), intent(in), optional :: source
    integer :: l
    integer, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = 0
    end if
end subroutine alloc_array_1d_int
subroutine alloc_large_array_1d_int(array, n, pad, source)
    integer, allocatable, dimension(:), intent(inout) :: array
    integer(kind=8), dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    integer, dimension(:), intent(in), optional :: source
    integer :: l
    integer, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = 0
    end if
end subroutine alloc_large_array_1d_int
subroutine alloc_array_2d_int(array, n, pad, source)
    integer, allocatable, dimension(:, :), intent(inout) :: array
    integer, dimension(1:4), intent(in) :: n
    integer, intent(in), optional :: pad
    integer, dimension(:, :), intent(in), optional :: source
    integer :: l
    integer, allocatable, dimension(:, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l))
    if (present(source)) then
        array(:, :) = w(:, :)
    else
        array = 0
    end if
end subroutine alloc_array_2d_int
subroutine alloc_array_3d_int(array, n, pad, source)
    integer, allocatable, dimension(:, :, :), intent(inout) :: array
    integer, dimension(1:6), intent(in) :: n
    integer, intent(in), optional :: pad
    integer, dimension(:, :, :), intent(in), optional :: source
    integer :: l
    integer, allocatable, dimension(:, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l))
    if (present(source)) then
        array(:, :, :) = w(:, :, :)
    else
        array = 0
    end if
end subroutine alloc_array_3d_int
subroutine alloc_array_4d_int(array, n, pad, source)
    integer, allocatable, dimension(:, :, :, :), intent(inout) :: array
    integer, dimension(1:8), intent(in) :: n
    integer, intent(in), optional :: pad
    integer, dimension(:, :, :, :), intent(in), optional :: source
    integer :: l
    integer, allocatable, dimension(:, :, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3), size(source, 4)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l, n(7) - l:n(8) + l))
    if (present(source)) then
        array = w(:, :, :, :)
    else
        array = 0
    end if
end subroutine alloc_array_4d_int
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
subroutine alloc_array_1d_float(array, n, pad, source)
    real, allocatable, dimension(:), intent(inout) :: array
    integer, dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    real, dimension(:), intent(in), optional :: source
    integer :: l
    real, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = 0.0
    end if
end subroutine alloc_array_1d_float
subroutine alloc_large_array_1d_float(array, n, pad, source)
    real, allocatable, dimension(:), intent(inout) :: array
    integer(kind=8), dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    real, dimension(:), intent(in), optional :: source
    integer :: l
    real, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = 0.0
    end if
end subroutine alloc_large_array_1d_float
subroutine alloc_array_2d_float(array, n, pad, source)
    real, allocatable, dimension(:, :), intent(inout) :: array
    integer, dimension(1:4), intent(in) :: n
    integer, intent(in), optional :: pad
    real, dimension(:, :), intent(in), optional :: source
    integer :: l
    real, allocatable, dimension(:, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l))
    if (present(source)) then
        array(:, :) = w(:, :)
    else
        array = 0.0
    end if
end subroutine alloc_array_2d_float
subroutine alloc_array_3d_float(array, n, pad, source)
    real, allocatable, dimension(:, :, :), intent(inout) :: array
    integer, dimension(1:6), intent(in) :: n
    integer, intent(in), optional :: pad
    real, dimension(:, :, :), intent(in), optional :: source
    integer :: l
    real, allocatable, dimension(:, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l))
    if (present(source)) then
        array(:, :, :) = w(:, :, :)
    else
        array = 0.0
    end if
end subroutine alloc_array_3d_float
subroutine alloc_array_4d_float(array, n, pad, source)
    real, allocatable, dimension(:, :, :, :), intent(inout) :: array
    integer, dimension(1:8), intent(in) :: n
    integer, intent(in), optional :: pad
    real, dimension(:, :, :, :), intent(in), optional :: source
    integer :: l
    real, allocatable, dimension(:, :, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3), size(source, 4)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l, n(7) - l:n(8) + l))
    if (present(source)) then
        array = w(:, :, :, :)
    else
        array = 0.0
    end if
end subroutine alloc_array_4d_float
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
subroutine alloc_array_1d_double(array, n, pad, source)
    double precision, allocatable, dimension(:), intent(inout) :: array
    integer, dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    double precision, dimension(:), intent(in), optional :: source
    integer :: l
    double precision, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = 0.0d0
    end if
end subroutine alloc_array_1d_double
subroutine alloc_large_array_1d_double(array, n, pad, source)
    double precision, allocatable, dimension(:), intent(inout) :: array
    integer(kind=8), dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    double precision, dimension(:), intent(in), optional :: source
    integer :: l
    double precision, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = 0.0d0
    end if
end subroutine alloc_large_array_1d_double
subroutine alloc_array_2d_double(array, n, pad, source)
    double precision, allocatable, dimension(:, :), intent(inout) :: array
    integer, dimension(1:4), intent(in) :: n
    integer, intent(in), optional :: pad
    double precision, dimension(:, :), intent(in), optional :: source
    integer :: l
    double precision, allocatable, dimension(:, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l))
    if (present(source)) then
        array(:, :) = w(:, :)
    else
        array = 0.0d0
    end if
end subroutine alloc_array_2d_double
subroutine alloc_array_3d_double(array, n, pad, source)
    double precision, allocatable, dimension(:, :, :), intent(inout) :: array
    integer, dimension(1:6), intent(in) :: n
    integer, intent(in), optional :: pad
    double precision, dimension(:, :, :), intent(in), optional :: source
    integer :: l
    double precision, allocatable, dimension(:, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l))
    if (present(source)) then
        array(:, :, :) = w(:, :, :)
    else
        array = 0.0d0
    end if
end subroutine alloc_array_3d_double
subroutine alloc_array_4d_double(array, n, pad, source)
    double precision, allocatable, dimension(:, :, :, :), intent(inout) :: array
    integer, dimension(1:8), intent(in) :: n
    integer, intent(in), optional :: pad
    double precision, dimension(:, :, :, :), intent(in), optional :: source
    integer :: l
    double precision, allocatable, dimension(:, :, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3), size(source, 4)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l, n(7) - l:n(8) + l))
    if (present(source)) then
        array = w(:, :, :, :)
    else
        array = 0.0d0
    end if
end subroutine alloc_array_4d_double
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
subroutine alloc_array_1d_complex(array, n, pad, source)
    complex, allocatable, dimension(:), intent(inout) :: array
    integer, dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    complex, dimension(:), intent(in), optional :: source
    integer :: l
    complex, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = cmplx(0.0, 0.0)
    end if
end subroutine alloc_array_1d_complex
subroutine alloc_large_array_1d_complex(array, n, pad, source)
    complex, allocatable, dimension(:), intent(inout) :: array
    integer(kind=8), dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    complex, dimension(:), intent(in), optional :: source
    integer :: l
    complex, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = cmplx(0.0, 0.0)
    end if
end subroutine alloc_large_array_1d_complex
subroutine alloc_array_2d_complex(array, n, pad, source)
    complex, allocatable, dimension(:, :), intent(inout) :: array
    integer, dimension(1:4), intent(in) :: n
    integer, intent(in), optional :: pad
    complex, dimension(:, :), intent(in), optional :: source
    integer :: l
    complex, allocatable, dimension(:, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l))
    if (present(source)) then
        array(:, :) = w(:, :)
    else
        array = cmplx(0.0, 0.0)
    end if
end subroutine alloc_array_2d_complex
subroutine alloc_array_3d_complex(array, n, pad, source)
    complex, allocatable, dimension(:, :, :), intent(inout) :: array
    integer, dimension(1:6), intent(in) :: n
    integer, intent(in), optional :: pad
    complex, dimension(:, :, :), intent(in), optional :: source
    integer :: l
    complex, allocatable, dimension(:, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l))
    if (present(source)) then
        array(:, :, :) = w(:, :, :)
    else
        array = cmplx(0.0, 0.0)
    end if
end subroutine alloc_array_3d_complex
subroutine alloc_array_4d_complex(array, n, pad, source)
    complex, allocatable, dimension(:, :, :, :), intent(inout) :: array
    integer, dimension(1:8), intent(in) :: n
    integer, intent(in), optional :: pad
    complex, dimension(:, :, :, :), intent(in), optional :: source
    integer :: l
    complex, allocatable, dimension(:, :, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3), size(source, 4)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l, n(7) - l:n(8) + l))
    if (present(source)) then
        array = w(:, :, :, :)
    else
        array = cmplx(0.0, 0.0)
    end if
end subroutine alloc_array_4d_complex
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
subroutine alloc_array_1d_dcomplex(array, n, pad, source)
    double complex, allocatable, dimension(:), intent(inout) :: array
    integer, dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    double complex, dimension(:), intent(in), optional :: source
    integer :: l
    double complex, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = dcmplx(0.0d0, 0.0d0)
    end if
end subroutine alloc_array_1d_dcomplex
subroutine alloc_large_array_1d_dcomplex(array, n, pad, source)
    double complex, allocatable, dimension(:), intent(inout) :: array
    integer(kind=8), dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    double complex, dimension(:), intent(in), optional :: source
    integer :: l
    double complex, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = dcmplx(0.0d0, 0.0d0)
    end if
end subroutine alloc_large_array_1d_dcomplex
subroutine alloc_array_2d_dcomplex(array, n, pad, source)
    double complex, allocatable, dimension(:, :), intent(inout) :: array
    integer, dimension(1:4), intent(in) :: n
    integer, intent(in), optional :: pad
    double complex, dimension(:, :), intent(in), optional :: source
    integer :: l
    double complex, allocatable, dimension(:, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l))
    if (present(source)) then
        array(:, :) = w(:, :)
    else
        array = dcmplx(0.0d0, 0.0d0)
    end if
end subroutine alloc_array_2d_dcomplex
subroutine alloc_array_3d_dcomplex(array, n, pad, source)
    double complex, allocatable, dimension(:, :, :), intent(inout) :: array
    integer, dimension(1:6), intent(in) :: n
    integer, intent(in), optional :: pad
    double complex, dimension(:, :, :), intent(in), optional :: source
    integer :: l
    double complex, allocatable, dimension(:, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l))
    if (present(source)) then
        array(:, :, :) = w(:, :, :)
    else
        array = dcmplx(0.0d0, 0.0d0)
    end if
end subroutine alloc_array_3d_dcomplex
subroutine alloc_array_4d_dcomplex(array, n, pad, source)
    double complex, allocatable, dimension(:, :, :, :), intent(inout) :: array
    integer, dimension(1:8), intent(in) :: n
    integer, intent(in), optional :: pad
    double complex, dimension(:, :, :, :), intent(in), optional :: source
    integer :: l
    double complex, allocatable, dimension(:, :, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3), size(source, 4)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l, n(7) - l:n(8) + l))
    if (present(source)) then
        array = w(:, :, :, :)
    else
        array = dcmplx(0.0d0, 0.0d0)
    end if
end subroutine alloc_array_4d_dcomplex
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
subroutine alloc_array_1d_logical(array, n, pad, source)
    logical, allocatable, dimension(:), intent(inout) :: array
    integer, dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    logical, dimension(:), intent(in), optional :: source
    integer :: l
    logical, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = .false.
    end if
end subroutine alloc_array_1d_logical
subroutine alloc_large_array_1d_logical(array, n, pad, source)
    logical, allocatable, dimension(:), intent(inout) :: array
    integer(kind=8), dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    logical, dimension(:), intent(in), optional :: source
    integer :: l
    logical, allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = .false.
    end if
end subroutine alloc_large_array_1d_logical
subroutine alloc_array_2d_logical(array, n, pad, source)
    logical, allocatable, dimension(:, :), intent(inout) :: array
    integer, dimension(1:4), intent(in) :: n
    integer, intent(in), optional :: pad
    logical, dimension(:, :), intent(in), optional :: source
    integer :: l
    logical, allocatable, dimension(:, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l))
    if (present(source)) then
        array(:, :) = w(:, :)
    else
        array = .false.
    end if
end subroutine alloc_array_2d_logical
subroutine alloc_array_3d_logical(array, n, pad, source)
    logical, allocatable, dimension(:, :, :), intent(inout) :: array
    integer, dimension(1:6), intent(in) :: n
    integer, intent(in), optional :: pad
    logical, dimension(:, :, :), intent(in), optional :: source
    integer :: l
    logical, allocatable, dimension(:, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l))
    if (present(source)) then
        array(:, :, :) = w(:, :, :)
    else
        array = .false.
    end if
end subroutine alloc_array_3d_logical
subroutine alloc_array_4d_logical(array, n, pad, source)
    logical, allocatable, dimension(:, :, :, :), intent(inout) :: array
    integer, dimension(1:8), intent(in) :: n
    integer, intent(in), optional :: pad
    logical, dimension(:, :, :, :), intent(in), optional :: source
    integer :: l
    logical, allocatable, dimension(:, :, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3), size(source, 4)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l, n(7) - l:n(8) + l))
    if (present(source)) then
        array = w(:, :, :, :)
    else
        array = .false.
    end if
end subroutine alloc_array_4d_logical
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
subroutine alloc_array_1d_string(array, n, pad, source)
    character(len=*), allocatable, dimension(:), intent(inout) :: array
    integer, dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    character(len=*), dimension(:), intent(in), optional :: source
    integer :: l
    character(len=1024), allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = ''
    end if
end subroutine alloc_array_1d_string
subroutine alloc_large_array_1d_string(array, n, pad, source)
    character(len=*), allocatable, dimension(:), intent(inout) :: array
    integer(kind=8), dimension(1:2), intent(in) :: n
    integer, intent(in), optional :: pad
    character(len=*), dimension(:), intent(in), optional :: source
    integer :: l
    character(len=1024), allocatable, dimension(:) :: w
    if (present(source)) then
        allocate (w(size(source)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l))
    if (present(source)) then
        array(:) = w(:)
    else
        array = ''
    end if
end subroutine alloc_large_array_1d_string
subroutine alloc_array_2d_string(array, n, pad, source)
    character(len=*), allocatable, dimension(:, :), intent(inout) :: array
    integer, dimension(1:4), intent(in) :: n
    integer, intent(in), optional :: pad
    character(len=*), dimension(:, :), intent(in), optional :: source
    integer :: l
    character(len=1024), allocatable, dimension(:, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l))
    if (present(source)) then
        array(:, :) = w(:, :)
    else
        array = ''
    end if
end subroutine alloc_array_2d_string
subroutine alloc_array_3d_string(array, n, pad, source)
    character(len=*), allocatable, dimension(:, :, :), intent(inout) :: array
    integer, dimension(1:6), intent(in) :: n
    integer, intent(in), optional :: pad
    character(len=*), dimension(:, :, :), intent(in), optional :: source
    integer :: l
    character(len=1024), allocatable, dimension(:, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l))
    if (present(source)) then
        array(:, :, :) = w(:, :, :)
    else
        array = ''
    end if
end subroutine alloc_array_3d_string
subroutine alloc_array_4d_string(array, n, pad, source)
    character(len=*), allocatable, dimension(:, :, :, :), intent(inout) :: array
    integer, dimension(1:8), intent(in) :: n
    integer, intent(in), optional :: pad
    character(len=*), dimension(:, :, :, :), intent(in), optional :: source
    integer :: l
    character(len=1024), allocatable, dimension(:, :, :, :) :: w
    if (present(source)) then
        allocate (w(size(source, 1), size(source, 2), size(source, 3), size(source, 4)), source=source)
    end if
    if (present(pad)) then
        l = pad
    else
        l = 0
    end if
    if (allocated(array)) then
        deallocate (array)
    end if
    allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l, n(5) - l:n(6) + l, n(7) - l:n(8) + l))
    if (present(source)) then
        array = w(:, :, :, :)
    else
        array = ''
    end if
end subroutine alloc_array_4d_string
    ! Ones and zeros array
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
! Zeros-like array
function zeros_like_1d_int(w) result(wr)
    integer, dimension(:), intent(in) :: w
    integer, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = 0
end function zeros_like_1d_int
function zeros_like_2d_int(w) result(wr)
    integer, dimension(:, :), intent(in) :: w
    integer, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = 0
end function zeros_like_2d_int
function zeros_like_3d_int(w) result(wr)
    integer, dimension(:, :, :), intent(in) :: w
    integer, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = 0
end function zeros_like_3d_int
function zeros_like_4d_int(w) result(wr)
    integer, dimension(:, :, :, :), intent(in) :: w
    integer, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = 0
end function zeros_like_4d_int
! Ones-like array
function ones_like_1d_int(w) result(wr)
    integer, dimension(:), intent(in) :: w
    integer, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = 1
end function ones_like_1d_int
function ones_like_2d_int(w) result(wr)
    integer, dimension(:, :), intent(in) :: w
    integer, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = 1
end function ones_like_2d_int
function ones_like_3d_int(w) result(wr)
    integer, dimension(:, :, :), intent(in) :: w
    integer, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = 1
end function ones_like_3d_int
function ones_like_4d_int(w) result(wr)
    integer, dimension(:, :, :, :), intent(in) :: w
    integer, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = 1
end function ones_like_4d_int
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
! Zeros-like array
function zeros_like_1d_float(w) result(wr)
    real, dimension(:), intent(in) :: w
    real, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = 0
end function zeros_like_1d_float
function zeros_like_2d_float(w) result(wr)
    real, dimension(:, :), intent(in) :: w
    real, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = 0
end function zeros_like_2d_float
function zeros_like_3d_float(w) result(wr)
    real, dimension(:, :, :), intent(in) :: w
    real, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = 0
end function zeros_like_3d_float
function zeros_like_4d_float(w) result(wr)
    real, dimension(:, :, :, :), intent(in) :: w
    real, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = 0
end function zeros_like_4d_float
! Ones-like array
function ones_like_1d_float(w) result(wr)
    real, dimension(:), intent(in) :: w
    real, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = 1
end function ones_like_1d_float
function ones_like_2d_float(w) result(wr)
    real, dimension(:, :), intent(in) :: w
    real, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = 1
end function ones_like_2d_float
function ones_like_3d_float(w) result(wr)
    real, dimension(:, :, :), intent(in) :: w
    real, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = 1
end function ones_like_3d_float
function ones_like_4d_float(w) result(wr)
    real, dimension(:, :, :, :), intent(in) :: w
    real, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = 1
end function ones_like_4d_float
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
! Zeros-like array
function zeros_like_1d_double(w) result(wr)
    double precision, dimension(:), intent(in) :: w
    double precision, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = 0
end function zeros_like_1d_double
function zeros_like_2d_double(w) result(wr)
    double precision, dimension(:, :), intent(in) :: w
    double precision, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = 0
end function zeros_like_2d_double
function zeros_like_3d_double(w) result(wr)
    double precision, dimension(:, :, :), intent(in) :: w
    double precision, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = 0
end function zeros_like_3d_double
function zeros_like_4d_double(w) result(wr)
    double precision, dimension(:, :, :, :), intent(in) :: w
    double precision, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = 0
end function zeros_like_4d_double
! Ones-like array
function ones_like_1d_double(w) result(wr)
    double precision, dimension(:), intent(in) :: w
    double precision, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = 1
end function ones_like_1d_double
function ones_like_2d_double(w) result(wr)
    double precision, dimension(:, :), intent(in) :: w
    double precision, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = 1
end function ones_like_2d_double
function ones_like_3d_double(w) result(wr)
    double precision, dimension(:, :, :), intent(in) :: w
    double precision, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = 1
end function ones_like_3d_double
function ones_like_4d_double(w) result(wr)
    double precision, dimension(:, :, :, :), intent(in) :: w
    double precision, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = 1
end function ones_like_4d_double
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
! Zeros-like array
function zeros_like_1d_complex(w) result(wr)
    complex, dimension(:), intent(in) :: w
    complex, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = 0
end function zeros_like_1d_complex
function zeros_like_2d_complex(w) result(wr)
    complex, dimension(:, :), intent(in) :: w
    complex, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = 0
end function zeros_like_2d_complex
function zeros_like_3d_complex(w) result(wr)
    complex, dimension(:, :, :), intent(in) :: w
    complex, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = 0
end function zeros_like_3d_complex
function zeros_like_4d_complex(w) result(wr)
    complex, dimension(:, :, :, :), intent(in) :: w
    complex, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = 0
end function zeros_like_4d_complex
! Ones-like array
function ones_like_1d_complex(w) result(wr)
    complex, dimension(:), intent(in) :: w
    complex, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = 1
end function ones_like_1d_complex
function ones_like_2d_complex(w) result(wr)
    complex, dimension(:, :), intent(in) :: w
    complex, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = 1
end function ones_like_2d_complex
function ones_like_3d_complex(w) result(wr)
    complex, dimension(:, :, :), intent(in) :: w
    complex, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = 1
end function ones_like_3d_complex
function ones_like_4d_complex(w) result(wr)
    complex, dimension(:, :, :, :), intent(in) :: w
    complex, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = 1
end function ones_like_4d_complex
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
! Zeros-like array
function zeros_like_1d_dcomplex(w) result(wr)
    double complex, dimension(:), intent(in) :: w
    double complex, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = 0
end function zeros_like_1d_dcomplex
function zeros_like_2d_dcomplex(w) result(wr)
    double complex, dimension(:, :), intent(in) :: w
    double complex, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = 0
end function zeros_like_2d_dcomplex
function zeros_like_3d_dcomplex(w) result(wr)
    double complex, dimension(:, :, :), intent(in) :: w
    double complex, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = 0
end function zeros_like_3d_dcomplex
function zeros_like_4d_dcomplex(w) result(wr)
    double complex, dimension(:, :, :, :), intent(in) :: w
    double complex, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = 0
end function zeros_like_4d_dcomplex
! Ones-like array
function ones_like_1d_dcomplex(w) result(wr)
    double complex, dimension(:), intent(in) :: w
    double complex, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = 1
end function ones_like_1d_dcomplex
function ones_like_2d_dcomplex(w) result(wr)
    double complex, dimension(:, :), intent(in) :: w
    double complex, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = 1
end function ones_like_2d_dcomplex
function ones_like_3d_dcomplex(w) result(wr)
    double complex, dimension(:, :, :), intent(in) :: w
    double complex, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = 1
end function ones_like_3d_dcomplex
function ones_like_4d_dcomplex(w) result(wr)
    double complex, dimension(:, :, :, :), intent(in) :: w
    double complex, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = 1
end function ones_like_4d_dcomplex
    !===========================================================================
    ! Eye array
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
function eye_2d_int(n, val) result(w)
    integer :: n
    integer :: val
    integer, allocatable, dimension(:, :) :: w
    integer :: i
    allocate (w(1:n, 1:n))
    w = 0
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = val
    end do
    !$omp end parallel do
end function eye_2d_int
function eye_2d_from_vector_int(v) result(w)
    integer, dimension(:) :: v
    integer, allocatable, dimension(:, :) :: w
    integer :: i, n
    n = size(v)
    allocate (w(1:n, 1:n))
    w = 0
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = v(i)
    end do
    ! forall(i = 1:n) w(i, i) = v(i)
    !$omp end parallel do
end function eye_2d_from_vector_int
function get_diagonal_2d_int(w) result(d)
    integer, dimension(:, :), intent(in) :: w
    integer, allocatable, dimension(:) :: d
    integer :: n, i
    n = min(size(w, 1), size(w, 2))
    allocate (d(1:n))
    !$omp parallel do private(i)
    do i = 1, n
        d(i) = w(i, i)
    end do
    !$omp end parallel do
    ! forall(i = 1:n) d(i) = w(i, i)
end function get_diagonal_2d_int
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
function eye_2d_float(n, val) result(w)
    integer :: n
    real :: val
    real, allocatable, dimension(:, :) :: w
    integer :: i
    allocate (w(1:n, 1:n))
    w = 0.0
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = val
    end do
    !$omp end parallel do
end function eye_2d_float
function eye_2d_from_vector_float(v) result(w)
    real, dimension(:) :: v
    real, allocatable, dimension(:, :) :: w
    integer :: i, n
    n = size(v)
    allocate (w(1:n, 1:n))
    w = 0.0
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = v(i)
    end do
    ! forall(i = 1:n) w(i, i) = v(i)
    !$omp end parallel do
end function eye_2d_from_vector_float
function get_diagonal_2d_float(w) result(d)
    real, dimension(:, :), intent(in) :: w
    real, allocatable, dimension(:) :: d
    integer :: n, i
    n = min(size(w, 1), size(w, 2))
    allocate (d(1:n))
    !$omp parallel do private(i)
    do i = 1, n
        d(i) = w(i, i)
    end do
    !$omp end parallel do
    ! forall(i = 1:n) d(i) = w(i, i)
end function get_diagonal_2d_float
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
function eye_2d_double(n, val) result(w)
    integer :: n
    double precision :: val
    double precision, allocatable, dimension(:, :) :: w
    integer :: i
    allocate (w(1:n, 1:n))
    w = 0.0d0
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = val
    end do
    !$omp end parallel do
end function eye_2d_double
function eye_2d_from_vector_double(v) result(w)
    double precision, dimension(:) :: v
    double precision, allocatable, dimension(:, :) :: w
    integer :: i, n
    n = size(v)
    allocate (w(1:n, 1:n))
    w = 0.0d0
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = v(i)
    end do
    ! forall(i = 1:n) w(i, i) = v(i)
    !$omp end parallel do
end function eye_2d_from_vector_double
function get_diagonal_2d_double(w) result(d)
    double precision, dimension(:, :), intent(in) :: w
    double precision, allocatable, dimension(:) :: d
    integer :: n, i
    n = min(size(w, 1), size(w, 2))
    allocate (d(1:n))
    !$omp parallel do private(i)
    do i = 1, n
        d(i) = w(i, i)
    end do
    !$omp end parallel do
    ! forall(i = 1:n) d(i) = w(i, i)
end function get_diagonal_2d_double
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
function eye_2d_complex(n, val) result(w)
    integer :: n
    complex :: val
    complex, allocatable, dimension(:, :) :: w
    integer :: i
    allocate (w(1:n, 1:n))
    w = cmplx(0.0, 0.0)
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = val
    end do
    !$omp end parallel do
end function eye_2d_complex
function eye_2d_from_vector_complex(v) result(w)
    complex, dimension(:) :: v
    complex, allocatable, dimension(:, :) :: w
    integer :: i, n
    n = size(v)
    allocate (w(1:n, 1:n))
    w = cmplx(0.0, 0.0)
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = v(i)
    end do
    ! forall(i = 1:n) w(i, i) = v(i)
    !$omp end parallel do
end function eye_2d_from_vector_complex
function get_diagonal_2d_complex(w) result(d)
    complex, dimension(:, :), intent(in) :: w
    complex, allocatable, dimension(:) :: d
    integer :: n, i
    n = min(size(w, 1), size(w, 2))
    allocate (d(1:n))
    !$omp parallel do private(i)
    do i = 1, n
        d(i) = w(i, i)
    end do
    !$omp end parallel do
    ! forall(i = 1:n) d(i) = w(i, i)
end function get_diagonal_2d_complex
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
function eye_2d_dcomplex(n, val) result(w)
    integer :: n
    double complex :: val
    double complex, allocatable, dimension(:, :) :: w
    integer :: i
    allocate (w(1:n, 1:n))
    w = dcmplx(0.0d0, 0.0d0)
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = val
    end do
    !$omp end parallel do
end function eye_2d_dcomplex
function eye_2d_from_vector_dcomplex(v) result(w)
    double complex, dimension(:) :: v
    double complex, allocatable, dimension(:, :) :: w
    integer :: i, n
    n = size(v)
    allocate (w(1:n, 1:n))
    w = dcmplx(0.0d0, 0.0d0)
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = v(i)
    end do
    ! forall(i = 1:n) w(i, i) = v(i)
    !$omp end parallel do
end function eye_2d_from_vector_dcomplex
function get_diagonal_2d_dcomplex(w) result(d)
    double complex, dimension(:, :), intent(in) :: w
    double complex, allocatable, dimension(:) :: d
    integer :: n, i
    n = min(size(w, 1), size(w, 2))
    allocate (d(1:n))
    !$omp parallel do private(i)
    do i = 1, n
        d(i) = w(i, i)
    end do
    !$omp end parallel do
    ! forall(i = 1:n) d(i) = w(i, i)
end function get_diagonal_2d_dcomplex
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
function eye_2d_logical(n, val) result(w)
    integer :: n
    logical :: val
    logical, allocatable, dimension(:, :) :: w
    integer :: i
    allocate (w(1:n, 1:n))
    w = .false.
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = val
    end do
    !$omp end parallel do
end function eye_2d_logical
function eye_2d_from_vector_logical(v) result(w)
    logical, dimension(:) :: v
    logical, allocatable, dimension(:, :) :: w
    integer :: i, n
    n = size(v)
    allocate (w(1:n, 1:n))
    w = .false.
    !$omp parallel do private(i)
    do i = 1, n
        w(i, i) = v(i)
    end do
    ! forall(i = 1:n) w(i, i) = v(i)
    !$omp end parallel do
end function eye_2d_from_vector_logical
function get_diagonal_2d_logical(w) result(d)
    logical, dimension(:, :), intent(in) :: w
    logical, allocatable, dimension(:) :: d
    integer :: n, i
    n = min(size(w, 1), size(w, 2))
    allocate (d(1:n))
    !$omp parallel do private(i)
    do i = 1, n
        d(i) = w(i, i)
    end do
    !$omp end parallel do
    ! forall(i = 1:n) d(i) = w(i, i)
end function get_diagonal_2d_logical
    !===========================================================================
    ! Constant-value array
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
! Constant array
function const_1d_int(n1, val) result(w)
    integer, intent(in) :: val
    integer, intent(in) :: n1
    integer, allocatable, dimension(:) :: w
    allocate (w(1:n1))
    w = val
end function const_1d_int
function const_2d_int(n1, n2, val) result(w)
    integer, intent(in) :: val
    integer, intent(in) :: n1, n2
    integer, allocatable, dimension(:, :) :: w
    allocate (w(1:n1, 1:n2))
    w = val
end function const_2d_int
function const_3d_int(n1, n2, n3, val) result(w)
    integer, intent(in) :: val
    integer, intent(in) :: n1, n2, n3
    integer, allocatable, dimension(:, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3))
    w = val
end function const_3d_int
function const_4d_int(n1, n2, n3, n4, val) result(w)
    integer, intent(in) :: val
    integer, intent(in) :: n1, n2, n3, n4
    integer, allocatable, dimension(:, :, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3, 1:n4))
    w = val
end function const_4d_int
! Constant-like array
function const_like_1d_int(w, val) result(wr)
    integer, dimension(:), intent(in) :: w
    integer, intent(in) :: val
    integer, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = val
end function const_like_1d_int
function const_like_2d_int(w, val) result(wr)
    integer, dimension(:, :), intent(in) :: w
    integer, intent(in) :: val
    integer, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = val
end function const_like_2d_int
function const_like_3d_int(w, val) result(wr)
    integer, dimension(:, :, :), intent(in) :: w
    integer, intent(in) :: val
    integer, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = val
end function const_like_3d_int
function const_like_4d_int(w, val) result(wr)
    integer, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in) :: val
    integer, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = val
end function const_like_4d_int
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
! Constant array
function const_1d_float(n1, val) result(w)
    real, intent(in) :: val
    integer, intent(in) :: n1
    real, allocatable, dimension(:) :: w
    allocate (w(1:n1))
    w = val
end function const_1d_float
function const_2d_float(n1, n2, val) result(w)
    real, intent(in) :: val
    integer, intent(in) :: n1, n2
    real, allocatable, dimension(:, :) :: w
    allocate (w(1:n1, 1:n2))
    w = val
end function const_2d_float
function const_3d_float(n1, n2, n3, val) result(w)
    real, intent(in) :: val
    integer, intent(in) :: n1, n2, n3
    real, allocatable, dimension(:, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3))
    w = val
end function const_3d_float
function const_4d_float(n1, n2, n3, n4, val) result(w)
    real, intent(in) :: val
    integer, intent(in) :: n1, n2, n3, n4
    real, allocatable, dimension(:, :, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3, 1:n4))
    w = val
end function const_4d_float
! Constant-like array
function const_like_1d_float(w, val) result(wr)
    real, dimension(:), intent(in) :: w
    real, intent(in) :: val
    real, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = val
end function const_like_1d_float
function const_like_2d_float(w, val) result(wr)
    real, dimension(:, :), intent(in) :: w
    real, intent(in) :: val
    real, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = val
end function const_like_2d_float
function const_like_3d_float(w, val) result(wr)
    real, dimension(:, :, :), intent(in) :: w
    real, intent(in) :: val
    real, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = val
end function const_like_3d_float
function const_like_4d_float(w, val) result(wr)
    real, dimension(:, :, :, :), intent(in) :: w
    real, intent(in) :: val
    real, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = val
end function const_like_4d_float
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
! Constant array
function const_1d_double(n1, val) result(w)
    double precision, intent(in) :: val
    integer, intent(in) :: n1
    double precision, allocatable, dimension(:) :: w
    allocate (w(1:n1))
    w = val
end function const_1d_double
function const_2d_double(n1, n2, val) result(w)
    double precision, intent(in) :: val
    integer, intent(in) :: n1, n2
    double precision, allocatable, dimension(:, :) :: w
    allocate (w(1:n1, 1:n2))
    w = val
end function const_2d_double
function const_3d_double(n1, n2, n3, val) result(w)
    double precision, intent(in) :: val
    integer, intent(in) :: n1, n2, n3
    double precision, allocatable, dimension(:, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3))
    w = val
end function const_3d_double
function const_4d_double(n1, n2, n3, n4, val) result(w)
    double precision, intent(in) :: val
    integer, intent(in) :: n1, n2, n3, n4
    double precision, allocatable, dimension(:, :, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3, 1:n4))
    w = val
end function const_4d_double
! Constant-like array
function const_like_1d_double(w, val) result(wr)
    double precision, dimension(:), intent(in) :: w
    double precision, intent(in) :: val
    double precision, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = val
end function const_like_1d_double
function const_like_2d_double(w, val) result(wr)
    double precision, dimension(:, :), intent(in) :: w
    double precision, intent(in) :: val
    double precision, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = val
end function const_like_2d_double
function const_like_3d_double(w, val) result(wr)
    double precision, dimension(:, :, :), intent(in) :: w
    double precision, intent(in) :: val
    double precision, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = val
end function const_like_3d_double
function const_like_4d_double(w, val) result(wr)
    double precision, dimension(:, :, :, :), intent(in) :: w
    double precision, intent(in) :: val
    double precision, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = val
end function const_like_4d_double
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
! Constant array
function const_1d_complex(n1, val) result(w)
    complex, intent(in) :: val
    integer, intent(in) :: n1
    complex, allocatable, dimension(:) :: w
    allocate (w(1:n1))
    w = val
end function const_1d_complex
function const_2d_complex(n1, n2, val) result(w)
    complex, intent(in) :: val
    integer, intent(in) :: n1, n2
    complex, allocatable, dimension(:, :) :: w
    allocate (w(1:n1, 1:n2))
    w = val
end function const_2d_complex
function const_3d_complex(n1, n2, n3, val) result(w)
    complex, intent(in) :: val
    integer, intent(in) :: n1, n2, n3
    complex, allocatable, dimension(:, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3))
    w = val
end function const_3d_complex
function const_4d_complex(n1, n2, n3, n4, val) result(w)
    complex, intent(in) :: val
    integer, intent(in) :: n1, n2, n3, n4
    complex, allocatable, dimension(:, :, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3, 1:n4))
    w = val
end function const_4d_complex
! Constant-like array
function const_like_1d_complex(w, val) result(wr)
    complex, dimension(:), intent(in) :: w
    complex, intent(in) :: val
    complex, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = val
end function const_like_1d_complex
function const_like_2d_complex(w, val) result(wr)
    complex, dimension(:, :), intent(in) :: w
    complex, intent(in) :: val
    complex, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = val
end function const_like_2d_complex
function const_like_3d_complex(w, val) result(wr)
    complex, dimension(:, :, :), intent(in) :: w
    complex, intent(in) :: val
    complex, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = val
end function const_like_3d_complex
function const_like_4d_complex(w, val) result(wr)
    complex, dimension(:, :, :, :), intent(in) :: w
    complex, intent(in) :: val
    complex, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = val
end function const_like_4d_complex
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
! Constant array
function const_1d_dcomplex(n1, val) result(w)
    double complex, intent(in) :: val
    integer, intent(in) :: n1
    double complex, allocatable, dimension(:) :: w
    allocate (w(1:n1))
    w = val
end function const_1d_dcomplex
function const_2d_dcomplex(n1, n2, val) result(w)
    double complex, intent(in) :: val
    integer, intent(in) :: n1, n2
    double complex, allocatable, dimension(:, :) :: w
    allocate (w(1:n1, 1:n2))
    w = val
end function const_2d_dcomplex
function const_3d_dcomplex(n1, n2, n3, val) result(w)
    double complex, intent(in) :: val
    integer, intent(in) :: n1, n2, n3
    double complex, allocatable, dimension(:, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3))
    w = val
end function const_3d_dcomplex
function const_4d_dcomplex(n1, n2, n3, n4, val) result(w)
    double complex, intent(in) :: val
    integer, intent(in) :: n1, n2, n3, n4
    double complex, allocatable, dimension(:, :, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3, 1:n4))
    w = val
end function const_4d_dcomplex
! Constant-like array
function const_like_1d_dcomplex(w, val) result(wr)
    double complex, dimension(:), intent(in) :: w
    double complex, intent(in) :: val
    double complex, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = val
end function const_like_1d_dcomplex
function const_like_2d_dcomplex(w, val) result(wr)
    double complex, dimension(:, :), intent(in) :: w
    double complex, intent(in) :: val
    double complex, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = val
end function const_like_2d_dcomplex
function const_like_3d_dcomplex(w, val) result(wr)
    double complex, dimension(:, :, :), intent(in) :: w
    double complex, intent(in) :: val
    double complex, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = val
end function const_like_3d_dcomplex
function const_like_4d_dcomplex(w, val) result(wr)
    double complex, dimension(:, :, :, :), intent(in) :: w
    double complex, intent(in) :: val
    double complex, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = val
end function const_like_4d_dcomplex
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
! Constant array
function const_1d_logical(n1, val) result(w)
    logical, intent(in) :: val
    integer, intent(in) :: n1
    logical, allocatable, dimension(:) :: w
    allocate (w(1:n1))
    w = val
end function const_1d_logical
function const_2d_logical(n1, n2, val) result(w)
    logical, intent(in) :: val
    integer, intent(in) :: n1, n2
    logical, allocatable, dimension(:, :) :: w
    allocate (w(1:n1, 1:n2))
    w = val
end function const_2d_logical
function const_3d_logical(n1, n2, n3, val) result(w)
    logical, intent(in) :: val
    integer, intent(in) :: n1, n2, n3
    logical, allocatable, dimension(:, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3))
    w = val
end function const_3d_logical
function const_4d_logical(n1, n2, n3, n4, val) result(w)
    logical, intent(in) :: val
    integer, intent(in) :: n1, n2, n3, n4
    logical, allocatable, dimension(:, :, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3, 1:n4))
    w = val
end function const_4d_logical
! Constant-like array
function const_like_1d_logical(w, val) result(wr)
    logical, dimension(:), intent(in) :: w
    logical, intent(in) :: val
    logical, allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = val
end function const_like_1d_logical
function const_like_2d_logical(w, val) result(wr)
    logical, dimension(:, :), intent(in) :: w
    logical, intent(in) :: val
    logical, allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = val
end function const_like_2d_logical
function const_like_3d_logical(w, val) result(wr)
    logical, dimension(:, :, :), intent(in) :: w
    logical, intent(in) :: val
    logical, allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = val
end function const_like_3d_logical
function const_like_4d_logical(w, val) result(wr)
    logical, dimension(:, :, :, :), intent(in) :: w
    logical, intent(in) :: val
    logical, allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = val
end function const_like_4d_logical
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
! Constant array
function const_1d_string(n1, val) result(w)
    character(len=*), intent(in) :: val
    integer, intent(in) :: n1
    character(len=1024), allocatable, dimension(:) :: w
    allocate (w(1:n1))
    w = val
end function const_1d_string
function const_2d_string(n1, n2, val) result(w)
    character(len=*), intent(in) :: val
    integer, intent(in) :: n1, n2
    character(len=1024), allocatable, dimension(:, :) :: w
    allocate (w(1:n1, 1:n2))
    w = val
end function const_2d_string
function const_3d_string(n1, n2, n3, val) result(w)
    character(len=*), intent(in) :: val
    integer, intent(in) :: n1, n2, n3
    character(len=1024), allocatable, dimension(:, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3))
    w = val
end function const_3d_string
function const_4d_string(n1, n2, n3, n4, val) result(w)
    character(len=*), intent(in) :: val
    integer, intent(in) :: n1, n2, n3, n4
    character(len=1024), allocatable, dimension(:, :, :, :) :: w
    allocate (w(1:n1, 1:n2, 1:n3, 1:n4))
    w = val
end function const_4d_string
! Constant-like array
function const_like_1d_string(w, val) result(wr)
    character(len=*), dimension(:), intent(in) :: w
    character(len=*), intent(in) :: val
    character(len=1024), allocatable, dimension(:) :: wr
    allocate (wr(1:size(w)))
    wr = val
end function const_like_1d_string
function const_like_2d_string(w, val) result(wr)
    character(len=*), dimension(:, :), intent(in) :: w
    character(len=*), intent(in) :: val
    character(len=1024), allocatable, dimension(:, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2)))
    wr = val
end function const_like_2d_string
function const_like_3d_string(w, val) result(wr)
    character(len=*), dimension(:, :, :), intent(in) :: w
    character(len=*), intent(in) :: val
    character(len=1024), allocatable, dimension(:, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
    wr = val
end function const_like_3d_string
function const_like_4d_string(w, val) result(wr)
    character(len=*), dimension(:, :, :, :), intent(in) :: w
    character(len=*), intent(in) :: val
    character(len=1024), allocatable, dimension(:, :, :, :) :: wr
    allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
    wr = val
end function const_like_4d_string
    !
    !> Return a linearly increasing or decreasing integer array
    !> specified by beg, end, and interval
    !
    function regspace_int(beg, step, end) result(w)
        integer, intent(in) :: beg, step, end
        integer, allocatable, dimension(:) :: w
        integer :: n, i
        n = nint((end - beg + 0.0d0)/step) + 1
        if (beg + (n - 1)*step > end) then
            n = n - 1
        end if
        if (n == 0) then
            n = 1
        end if
        allocate (w(1:n))
        do i = 1, n
            w(i) = beg + (i - 1)*step
        end do
    end function regspace_int
    !
    !> Return a linearly increasing or decreasing float array
    !> specified by beg, end, and interval
    !
    function regspace_float(beg, step, end) result(w)
        real, intent(in) :: beg, step, end
        real, allocatable, dimension(:) :: w
        integer :: n, i
        n = nint((end - beg)/step) + 1
        if (beg + (n - 1)*step > end) then
            n = n - 1
        end if
        if (n == 0) then
            n = 1
        end if
        allocate (w(1:n))
        do i = 1, n
            w(i) = beg + (i - 1)*step
        end do
    end function regspace_float
    !
    !> Return a linearly increasing or decreasing double array
    !> specified by beg, end, and interval
    !
    function regspace_double(beg, step, end) result(w)
        double precision, intent(in) :: beg, step, end
        double precision, allocatable, dimension(:) :: w
        integer :: n, i
        n = nint((end - beg)/step) + 1
        if (beg + (n - 1)*step > end) then
            n = n - 1
        end if
        if (n == 0) then
            n = 1
        end if
        allocate (w(1:n))
        do i = 1, n
            w(i) = beg + (i - 1)*step
        end do
    end function regspace_double
    !
    !> Return a linearly increasing or decreasing float array
    !> specified by beg, end, and number of output elements
    !
    function linspace_float(beg, end, nstep) result(w)
        real, intent(in) :: beg, end
        integer, intent(in) :: nstep
        real, allocatable, dimension(:) :: w
        real :: step
        integer :: i
        if (nstep == 1) then
            step = end - beg
        else
            step = (end - beg)/(nstep - 1.0)
        end if
        allocate (w(1:nstep))
        do i = 1, nstep
            w(i) = beg + (i - 1)*step
        end do
    end function linspace_float
    !
    !> Return a linearly increasing or decreasing double array
    !> specified by beg, end, and number of output elements
    !
    function linspace_double(beg, end, nstep) result(w)
        double precision, intent(in) :: beg, end
        integer, intent(in) :: nstep
        double precision, allocatable, dimension(:) :: w
        double precision :: step
        integer :: i
        if (nstep == 1) then
            step = end - beg
        else
            step = (end - beg)/(nstep - 1.0)
        end if
        allocate (w(1:nstep))
        do i = 1, nstep
            w(i) = beg + (i - 1)*step
        end do
    end function linspace_double
    !
    !> Return a linearly increasing or decreasing complex array
    !> specified by beg, end, and number of output elements
    !
    function linspace_complex(beg, end, nstep) result(w)
        complex, intent(in) :: beg, end
        integer, intent(in) :: nstep
        complex, allocatable, dimension(:) :: w
        complex :: step
        integer :: i
        if (nstep == 1) then
            step = end - beg
        else
            step = (end - beg)/(nstep - 1.0)
        end if
        allocate (w(1:nstep))
        do i = 1, nstep
            w(i) = beg + (i - 1)*step
        end do
    end function linspace_complex
    !
    !> Return a linearly increasing or decreasing double complex array
    !> specified by beg, end, and number of output elements
    !
    function linspace_dcomplex(beg, end, nstep) result(w)
        double complex, intent(in) :: beg, end
        integer, intent(in) :: nstep
        double complex, allocatable, dimension(:) :: w
        double complex :: step
        integer :: i
        if (nstep == 1) then
            step = end - beg
        else
            step = (end - beg)/(nstep - 1.0)
        end if
        allocate (w(1:nstep))
        do i = 1, nstep
            w(i) = beg + (i - 1)*step
        end do
    end function linspace_dcomplex
    !
    !> Return a linearly increasing or decreasing float array
    !> specified by beg, end, and number of output elements
    !
    function logspace_float(beg, end, nstep, base) result(w)
        real, intent(in) :: beg, end
        integer, intent(in) :: nstep
        real, intent(in), optional :: base
        real, allocatable, dimension(:) :: w
        real :: step, logbase
        integer :: i
        if (nstep == 1) then
            step = end - beg
        else
            step = (end - beg)/(nstep - 1.0)
        end if
        if (present(base)) then
            logbase = base
        else
            logbase = 10.0
        end if
        allocate (w(1:nstep))
        do i = 1, nstep
            w(i) = logbase**(beg + (i - 1)*step)
        end do
    end function logspace_float
    !
    !> Return a linearly increasing or decreasing double array
    !> specified by beg, end, and number of output elements
    !
    function logspace_double(beg, end, nstep, base) result(w)
        double precision, intent(in) :: beg, end
        integer, intent(in) :: nstep
        double precision, intent(in), optional :: base
        double precision, allocatable, dimension(:) :: w
        double precision :: step, logbase
        integer :: i
        if (nstep == 1) then
            step = end - beg
        else
            step = (end - beg)/(nstep - 1.0)
        end if
        if (present(base)) then
            logbase = base
        else
            logbase = 10.0d0
        end if
        allocate (w(1:nstep))
        do i = 1, nstep
            w(i) = logbase**(beg + (i - 1)*step)
        end do
    end function logspace_double
    !
    !> Return a linearly increasing or decreasing complex array
    !> specified by beg, end, and number of output elements
    !
    function logspace_complex(beg, end, nstep, base) result(w)
        complex, intent(in) :: beg, end
        integer, intent(in) :: nstep
        complex, intent(in), optional :: base
        complex, allocatable, dimension(:) :: w
        complex :: step, logbase
        integer :: i
        if (nstep == 1) then
            step = end - beg
        else
            step = (end - beg)/(nstep - 1.0)
        end if
        if (present(base)) then
            logbase = base
        else
            logbase = cmplx(10.0, 0.0)
        end if
        allocate (w(1:nstep))
        do i = 1, nstep
            w(i) = logbase**(beg + (i - 1)*step)
        end do
    end function logspace_complex
    !
    !> Return a linearly increasing or decreasing double complex array
    !> specified by beg, end, and number of output elements
    !
    function logspace_dcomplex(beg, end, nstep, base) result(w)
        double complex, intent(in) :: beg, end
        integer, intent(in) :: nstep
        double complex, intent(in), optional :: base
        double complex, allocatable, dimension(:) :: w
        double complex :: step, logbase
        integer :: i
        if (nstep == 1) then
            step = end - beg
        else
            step = (end - beg)/(nstep - 1.0)
        end if
        if (present(base)) then
            logbase = base
        else
            logbase = dcmplx(10.0d0, 0.0d0)
        end if
        allocate (w(1:nstep))
        do i = 1, nstep
            w(i) = logbase**(beg + (i - 1)*step)
        end do
    end function logspace_dcomplex
    !============================================================
    ! Zero-valued array
    function zeros_1d_float(n1) result(w)
        integer :: n1
        real, allocatable, dimension(:) :: w
        allocate (w(1:n1))
        w = 0.0
    end function zeros_1d_float
    function zeros_2d_float(n1, n2) result(w)
        integer :: n1, n2
        real, allocatable, dimension(:, :) :: w
        allocate (w(1:n1, 1:n2))
        w = 0.0
    end function zeros_2d_float
    function zeros_3d_float(n1, n2, n3) result(w)
        integer :: n1, n2, n3
        real, allocatable, dimension(:, :, :) :: w
        allocate (w(1:n1, 1:n2, 1:n3))
        w = 0.0
    end function zeros_3d_float
    function zeros_4d_float(n1, n2, n3, n4) result(w)
        integer :: n1, n2, n3, n4
        real, allocatable, dimension(:, :, :, :) :: w
        allocate (w(1:n1, 1:n2, 1:n3, 1:n4))
        w = 0.0
    end function zeros_4d_float
    function zeros_5d_float(n1, n2, n3, n4, n5) result(w)
        integer :: n1, n2, n3, n4, n5
        real, allocatable, dimension(:, :, :, :, :) :: w
        allocate (w(1:n1, 1:n2, 1:n3, 1:n4, 1:n5))
        w = 0.0
    end function zeros_5d_float
    !============================================================
    ! 1-valued array
    function ones_1d_float(n1) result(w)
        integer :: n1
        real, allocatable, dimension(:) :: w
        allocate (w(1:n1))
        w = 1.0
    end function ones_1d_float
    function ones_2d_float(n1, n2) result(w)
        integer :: n1, n2
        real, allocatable, dimension(:, :) :: w
        allocate (w(1:n1, 1:n2))
        w = 1.0
    end function ones_2d_float
    function ones_3d_float(n1, n2, n3) result(w)
        integer :: n1, n2, n3
        real, allocatable, dimension(:, :, :) :: w
        allocate (w(1:n1, 1:n2, 1:n3))
        w = 1.0
    end function ones_3d_float
    function ones_4d_float(n1, n2, n3, n4) result(w)
        integer :: n1, n2, n3, n4
        real, allocatable, dimension(:, :, :, :) :: w
        allocate (w(1:n1, 1:n2, 1:n3, 1:n4))
        w = 1.0
    end function ones_4d_float
    !===================================================
    ! trues
    function falses_1d(n1) result(w)
        integer :: n1
        logical, allocatable, dimension(:) :: w
        allocate (w(1:n1))
        w = .false.
    end function falses_1d
    function falses_2d(n1, n2) result(w)
        integer :: n1, n2
        logical, allocatable, dimension(:, :) :: w
        allocate (w(1:n1, 1:n2))
        w = .false.
    end function falses_2d
    function falses_3d(n1, n2, n3) result(w)
        integer :: n1, n2, n3
        logical, allocatable, dimension(:, :, :) :: w
        allocate (w(1:n1, 1:n2, 1:n3))
        w = .false.
    end function falses_3d
    function falses_4d(n1, n2, n3, n4) result(w)
        integer :: n1, n2, n3, n4
        logical, allocatable, dimension(:, :, :, :) :: w
        allocate (w(1:n1, 1:n2, 1:n3, 1:n4))
        w = .false.
    end function falses_4d
    function trues_1d(n1) result(w)
        integer :: n1
        logical, allocatable, dimension(:) :: w
        allocate (w(1:n1))
        w = .true.
    end function trues_1d
    function trues_2d(n1, n2) result(w)
        integer :: n1, n2
        logical, allocatable, dimension(:, :) :: w
        allocate (w(1:n1, 1:n2))
        w = .true.
    end function trues_2d
    function trues_3d(n1, n2, n3) result(w)
        integer :: n1, n2, n3
        logical, allocatable, dimension(:, :, :) :: w
        allocate (w(1:n1, 1:n2, 1:n3))
        w = .true.
    end function trues_3d
    function trues_4d(n1, n2, n3, n4) result(w)
        integer :: n1, n2, n3, n4
        logical, allocatable, dimension(:, :, :, :) :: w
        allocate (w(1:n1, 1:n2, 1:n3, 1:n4))
        w = .true.
    end function trues_4d
    function falses_like_1d(w) result(wr)
        logical, dimension(:), intent(in) :: w
        logical, allocatable, dimension(:) :: wr
        allocate (wr(1:size(w)))
        wr = .false.
    end function falses_like_1d
    function trues_like_1d(w) result(wr)
        logical, dimension(:), intent(in) :: w
        logical, allocatable, dimension(:) :: wr
        allocate (wr(1:size(w)))
        wr = .true.
    end function trues_like_1d
    function falses_like_2d(w) result(wr)
        logical, dimension(:, :), intent(in) :: w
        logical, allocatable, dimension(:, :) :: wr
        allocate (wr(1:size(w, 1), 1:size(w, 2)))
        wr = .false.
    end function falses_like_2d
    function trues_like_2d(w) result(wr)
        logical, dimension(:, :), intent(in) :: w
        logical, allocatable, dimension(:, :) :: wr
        allocate (wr(1:size(w, 1), 1:size(w, 2)))
        wr = .true.
    end function trues_like_2d
    function falses_like_3d(w) result(wr)
        logical, dimension(:, :, :), intent(in) :: w
        logical, allocatable, dimension(:, :, :) :: wr
        allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
        wr = .false.
    end function falses_like_3d
    function trues_like_3d(w) result(wr)
        logical, dimension(:, :, :), intent(in) :: w
        logical, allocatable, dimension(:, :, :) :: wr
        allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3)))
        wr = .true.
    end function trues_like_3d
    function falses_like_4d(w) result(wr)
        logical, dimension(:, :, :, :), intent(in) :: w
        logical, allocatable, dimension(:, :, :, :) :: wr
        allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
        wr = .false.
    end function falses_like_4d
    function trues_like_4d(w) result(wr)
        logical, dimension(:, :, :, :), intent(in) :: w
        logical, allocatable, dimension(:, :, :, :) :: wr
        allocate (wr(1:size(w, 1), 1:size(w, 2), 1:size(w, 3), 1:size(w, 4)))
        wr = .true.
    end function trues_like_4d
end module libflit_array
! real :: a(n) ! An explicit shape array
! real :: b(:) ! An assumed shape array
! real, allocatable :: c(:) ! A deferred shape array (allocatable)
! real, pointer :: d(:) ! A deferred shape array (pointer)
! real :: e(*) ! An assumed size array
! real :: f(..) ! An assumed rank array/scalar
