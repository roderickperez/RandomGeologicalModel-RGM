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
module libflit_unique
    use libflit_array
    use ieee_arithmetic
    implicit none
    private
    interface unique
        module procedure :: unique_1d_int
        module procedure :: unique_1d_float
        module procedure :: unique_1d_double
        module procedure :: unique_1d_complex
        module procedure :: unique_1d_dcomplex
        module procedure :: unique_1d_string
        module procedure :: unique_2d_int
        module procedure :: unique_2d_float
        module procedure :: unique_2d_double
        module procedure :: unique_2d_complex
        module procedure :: unique_2d_dcomplex
        module procedure :: unique_2d_string
    end interface unique
    public :: unique
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
function unique_1d_int(w) result(wr)
    integer, dimension(:) :: w
    integer, allocatable, dimension(:) :: wr
    integer :: n, i, l
    logical, allocatable, dimension(:) :: q
    n = size(w)
    call alloc_array(wr, [1, n])
    q = trues(n)
    wr(1) = w(1)
    q(1) = .false.
    l = 2
    ! Check every element in the source array
    do i = 1, n
        ! If the element has not been checked to be unique
        if (q(i)) then
            ! To be a unique element, it must not be equivalent with any confirmed unique element
            if (.not. any(w(i) == wr(1:l - 1))) then
                wr(l) = w(i)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1)
end function unique_1d_int
function unique_2d_int(w, cols) result(wr)
    integer, dimension(:, :) :: w
    integer, optional :: cols(:)
    integer, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j, k, l
    logical, allocatable, dimension(:) :: q, duplicate
    integer, allocatable, dimension(:) :: cl
    n1 = size(w, 1)
    n2 = size(w, 2)
    call alloc_array(wr, [1, n1, 1, n2])
    q = trues(n1)
    if (present(cols)) then
        cl = cols
    else
        cl = regspace(1, 1, n2)
    end if
    duplicate = falses(size(cl))
    wr(1, :) = w(1, :)
    l = 2
    ! Check every row in the source array
    do i = 1, n1
        ! If the row has not been checked to be unique
        if (q(i)) then
            ! Check duplication with all previous unique rows
            check_duplicate: do j = 1, l - 1
                ! For each row, assume the row is unique
                duplicate = .false.
                ! Check duplication of each column
                do k = 1, size(cl)
                    if (w(i, cl(k)) == wr(j, cl(k))) then
                        duplicate(k) = .true.
                    end if
                end do
                ! If all elements are the same with some previous row, then this
                ! row is a duplicate row and there is no need to further check
                if (all(duplicate)) then
                    exit check_duplicate
                end if
            end do check_duplicate
            ! If a unique row, then add to unique list
            if (any(.not. duplicate)) then
                wr(l, :) = w(i, :)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1, :)
end function unique_2d_int
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
function unique_1d_float(w) result(wr)
    real, dimension(:) :: w
    real, allocatable, dimension(:) :: wr
    integer :: n, i, l
    logical, allocatable, dimension(:) :: q
    n = size(w)
    call alloc_array(wr, [1, n])
    q = trues(n)
    wr(1) = w(1)
    q(1) = .false.
    l = 2
    ! Check every element in the source array
    do i = 1, n
        ! If the element has not been checked to be unique
        if (q(i)) then
            ! To be a unique element, it must not be equivalent with any confirmed unique element
            if (.not. any(w(i) == wr(1:l - 1))) then
                wr(l) = w(i)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1)
end function unique_1d_float
function unique_2d_float(w, cols) result(wr)
    real, dimension(:, :) :: w
    integer, optional :: cols(:)
    real, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j, k, l
    logical, allocatable, dimension(:) :: q, duplicate
    integer, allocatable, dimension(:) :: cl
    n1 = size(w, 1)
    n2 = size(w, 2)
    call alloc_array(wr, [1, n1, 1, n2])
    q = trues(n1)
    if (present(cols)) then
        cl = cols
    else
        cl = regspace(1, 1, n2)
    end if
    duplicate = falses(size(cl))
    wr(1, :) = w(1, :)
    l = 2
    ! Check every row in the source array
    do i = 1, n1
        ! If the row has not been checked to be unique
        if (q(i)) then
            ! Check duplication with all previous unique rows
            check_duplicate: do j = 1, l - 1
                ! For each row, assume the row is unique
                duplicate = .false.
                ! Check duplication of each column
                do k = 1, size(cl)
                    if (w(i, cl(k)) == wr(j, cl(k))) then
                        duplicate(k) = .true.
                    end if
                end do
                ! If all elements are the same with some previous row, then this
                ! row is a duplicate row and there is no need to further check
                if (all(duplicate)) then
                    exit check_duplicate
                end if
            end do check_duplicate
            ! If a unique row, then add to unique list
            if (any(.not. duplicate)) then
                wr(l, :) = w(i, :)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1, :)
end function unique_2d_float
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
function unique_1d_double(w) result(wr)
    double precision, dimension(:) :: w
    double precision, allocatable, dimension(:) :: wr
    integer :: n, i, l
    logical, allocatable, dimension(:) :: q
    n = size(w)
    call alloc_array(wr, [1, n])
    q = trues(n)
    wr(1) = w(1)
    q(1) = .false.
    l = 2
    ! Check every element in the source array
    do i = 1, n
        ! If the element has not been checked to be unique
        if (q(i)) then
            ! To be a unique element, it must not be equivalent with any confirmed unique element
            if (.not. any(w(i) == wr(1:l - 1))) then
                wr(l) = w(i)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1)
end function unique_1d_double
function unique_2d_double(w, cols) result(wr)
    double precision, dimension(:, :) :: w
    integer, optional :: cols(:)
    double precision, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j, k, l
    logical, allocatable, dimension(:) :: q, duplicate
    integer, allocatable, dimension(:) :: cl
    n1 = size(w, 1)
    n2 = size(w, 2)
    call alloc_array(wr, [1, n1, 1, n2])
    q = trues(n1)
    if (present(cols)) then
        cl = cols
    else
        cl = regspace(1, 1, n2)
    end if
    duplicate = falses(size(cl))
    wr(1, :) = w(1, :)
    l = 2
    ! Check every row in the source array
    do i = 1, n1
        ! If the row has not been checked to be unique
        if (q(i)) then
            ! Check duplication with all previous unique rows
            check_duplicate: do j = 1, l - 1
                ! For each row, assume the row is unique
                duplicate = .false.
                ! Check duplication of each column
                do k = 1, size(cl)
                    if (w(i, cl(k)) == wr(j, cl(k))) then
                        duplicate(k) = .true.
                    end if
                end do
                ! If all elements are the same with some previous row, then this
                ! row is a duplicate row and there is no need to further check
                if (all(duplicate)) then
                    exit check_duplicate
                end if
            end do check_duplicate
            ! If a unique row, then add to unique list
            if (any(.not. duplicate)) then
                wr(l, :) = w(i, :)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1, :)
end function unique_2d_double
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
function unique_1d_complex(w) result(wr)
    complex, dimension(:) :: w
    complex, allocatable, dimension(:) :: wr
    integer :: n, i, l
    logical, allocatable, dimension(:) :: q
    n = size(w)
    call alloc_array(wr, [1, n])
    q = trues(n)
    wr(1) = w(1)
    q(1) = .false.
    l = 2
    ! Check every element in the source array
    do i = 1, n
        ! If the element has not been checked to be unique
        if (q(i)) then
            ! To be a unique element, it must not be equivalent with any confirmed unique element
            if (.not. any(w(i) == wr(1:l - 1))) then
                wr(l) = w(i)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1)
end function unique_1d_complex
function unique_2d_complex(w, cols) result(wr)
    complex, dimension(:, :) :: w
    integer, optional :: cols(:)
    complex, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j, k, l
    logical, allocatable, dimension(:) :: q, duplicate
    integer, allocatable, dimension(:) :: cl
    n1 = size(w, 1)
    n2 = size(w, 2)
    call alloc_array(wr, [1, n1, 1, n2])
    q = trues(n1)
    if (present(cols)) then
        cl = cols
    else
        cl = regspace(1, 1, n2)
    end if
    duplicate = falses(size(cl))
    wr(1, :) = w(1, :)
    l = 2
    ! Check every row in the source array
    do i = 1, n1
        ! If the row has not been checked to be unique
        if (q(i)) then
            ! Check duplication with all previous unique rows
            check_duplicate: do j = 1, l - 1
                ! For each row, assume the row is unique
                duplicate = .false.
                ! Check duplication of each column
                do k = 1, size(cl)
                    if (w(i, cl(k)) == wr(j, cl(k))) then
                        duplicate(k) = .true.
                    end if
                end do
                ! If all elements are the same with some previous row, then this
                ! row is a duplicate row and there is no need to further check
                if (all(duplicate)) then
                    exit check_duplicate
                end if
            end do check_duplicate
            ! If a unique row, then add to unique list
            if (any(.not. duplicate)) then
                wr(l, :) = w(i, :)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1, :)
end function unique_2d_complex
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
function unique_1d_dcomplex(w) result(wr)
    double complex, dimension(:) :: w
    double complex, allocatable, dimension(:) :: wr
    integer :: n, i, l
    logical, allocatable, dimension(:) :: q
    n = size(w)
    call alloc_array(wr, [1, n])
    q = trues(n)
    wr(1) = w(1)
    q(1) = .false.
    l = 2
    ! Check every element in the source array
    do i = 1, n
        ! If the element has not been checked to be unique
        if (q(i)) then
            ! To be a unique element, it must not be equivalent with any confirmed unique element
            if (.not. any(w(i) == wr(1:l - 1))) then
                wr(l) = w(i)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1)
end function unique_1d_dcomplex
function unique_2d_dcomplex(w, cols) result(wr)
    double complex, dimension(:, :) :: w
    integer, optional :: cols(:)
    double complex, allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j, k, l
    logical, allocatable, dimension(:) :: q, duplicate
    integer, allocatable, dimension(:) :: cl
    n1 = size(w, 1)
    n2 = size(w, 2)
    call alloc_array(wr, [1, n1, 1, n2])
    q = trues(n1)
    if (present(cols)) then
        cl = cols
    else
        cl = regspace(1, 1, n2)
    end if
    duplicate = falses(size(cl))
    wr(1, :) = w(1, :)
    l = 2
    ! Check every row in the source array
    do i = 1, n1
        ! If the row has not been checked to be unique
        if (q(i)) then
            ! Check duplication with all previous unique rows
            check_duplicate: do j = 1, l - 1
                ! For each row, assume the row is unique
                duplicate = .false.
                ! Check duplication of each column
                do k = 1, size(cl)
                    if (w(i, cl(k)) == wr(j, cl(k))) then
                        duplicate(k) = .true.
                    end if
                end do
                ! If all elements are the same with some previous row, then this
                ! row is a duplicate row and there is no need to further check
                if (all(duplicate)) then
                    exit check_duplicate
                end if
            end do check_duplicate
            ! If a unique row, then add to unique list
            if (any(.not. duplicate)) then
                wr(l, :) = w(i, :)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1, :)
end function unique_2d_dcomplex
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
function unique_1d_string(w) result(wr)
    character(len=*), dimension(:) :: w
    character(len=1024), allocatable, dimension(:) :: wr
    integer :: n, i, l
    logical, allocatable, dimension(:) :: q
    n = size(w)
    call alloc_array(wr, [1, n])
    q = trues(n)
    wr(1) = w(1)
    q(1) = .false.
    l = 2
    ! Check every element in the source array
    do i = 1, n
        ! If the element has not been checked to be unique
        if (q(i)) then
            ! To be a unique element, it must not be equivalent with any confirmed unique element
            if (.not. any(w(i) == wr(1:l - 1))) then
                wr(l) = w(i)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1)
end function unique_1d_string
function unique_2d_string(w, cols) result(wr)
    character(len=*), dimension(:, :) :: w
    integer, optional :: cols(:)
    character(len=1024), allocatable, dimension(:, :) :: wr
    integer :: n1, n2, i, j, k, l
    logical, allocatable, dimension(:) :: q, duplicate
    integer, allocatable, dimension(:) :: cl
    n1 = size(w, 1)
    n2 = size(w, 2)
    call alloc_array(wr, [1, n1, 1, n2])
    q = trues(n1)
    if (present(cols)) then
        cl = cols
    else
        cl = regspace(1, 1, n2)
    end if
    duplicate = falses(size(cl))
    wr(1, :) = w(1, :)
    l = 2
    ! Check every row in the source array
    do i = 1, n1
        ! If the row has not been checked to be unique
        if (q(i)) then
            ! Check duplication with all previous unique rows
            check_duplicate: do j = 1, l - 1
                ! For each row, assume the row is unique
                duplicate = .false.
                ! Check duplication of each column
                do k = 1, size(cl)
                    if (w(i, cl(k)) == wr(j, cl(k))) then
                        duplicate(k) = .true.
                    end if
                end do
                ! If all elements are the same with some previous row, then this
                ! row is a duplicate row and there is no need to further check
                if (all(duplicate)) then
                    exit check_duplicate
                end if
            end do check_duplicate
            ! If a unique row, then add to unique list
            if (any(.not. duplicate)) then
                wr(l, :) = w(i, :)
                q(i) = .false.
                l = l + 1
            end if
        end if
    end do
    wr = wr(1:l - 1, :)
end function unique_2d_string
end module libflit_unique
