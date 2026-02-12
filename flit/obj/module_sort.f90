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
module libflit_sort
    implicit none
    interface sort
        module procedure :: qksort_int
        module procedure :: qksort_float
        module procedure :: qksort_double
        module procedure :: qksort_2d_int
        module procedure :: qksort_2d_float
        module procedure :: qksort_2d_double
    end interface sort
    interface sort_index
        module procedure :: qksort_index_int
        module procedure :: qksort_index_float
        module procedure :: qksort_index_double
    end interface sort_index
    interface mergesort_index
        module procedure :: merge_sort_index_int
        module procedure :: merge_sort_index_float
        module procedure :: merge_sort_index_double
    end interface mergesort_index
    private
    public :: sort
    public :: sort_index
    public :: mergesort_index
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
subroutine merge_sort_index_int(r, d)
    integer, intent(in), dimension(:) :: r
    integer, intent(out), dimension(size(r)) :: d
    integer, dimension(size(r)) :: il
    integer :: stepsize
    integer :: i, j, n, left, k, ksize
    n = size(r)
    do i = 1, n
        d(i) = i
    end do
    if (n == 1) then
        return
    end if
    stepsize = 1
    do while (stepsize < n)
        do left = 1, n - stepsize, stepsize*2
            i = left
            j = left + stepsize
            ksize = min(stepsize*2, n - left + 1)
            k = 1
            do while (i < left + stepsize .and. j < left + ksize)
                if (r(d(i)) > r(d(j))) then
                    il(k) = d(i)
                    i = i + 1
                    k = k + 1
                else
                    il(k) = d(j)
                    j = j + 1
                    k = k + 1
                endif
            enddo
            if (i < left + stepsize) then
                ! fill up remaining from left
                il(k:ksize) = d(i:left + stepsize - 1)
            else
                ! fill up remaining from right
                il(k:ksize) = d(j:left + ksize - 1)
            endif
            d(left:left + ksize - 1) = il(1:ksize)
        end do
        stepsize = stepsize*2
    end do
end subroutine merge_sort_index_int
!
!> Recursive quick-sort routine sorting array into ascending or descending order
!
!> Reformatted and modified from
!> https://gist.github.com/t-nissie/479f0f16966925fa29ea#file-quicksort-f
!> Original author:
!> Takeshi Nishimatsu
!> License:
!> GPLv3
!
recursive subroutine quick_sort_int(a, order)
    integer, dimension(:), intent(inout) :: a
    integer, intent(in), optional :: order
    integer :: x, t
    integer :: first, last
    integer :: i, j
    first = 1
    last = size(a, 1)
    x = a((first + last)/2)
    i = first
    j = last
    do
        do while (a(i) < x)
            i = i + 1
        end do
        do while (x < a(j))
            j = j - 1
        end do
        if (i >= j) exit
        t = a(i)
        a(i) = a(j)
        a(j) = t
        i = i + 1
        j = j - 1
    end do
    if (first < i - 1) then
        call quick_sort_int(a(first:i - 1))
    end if
    if (j + 1 < last) then
        call quick_sort_int(a(j + 1:last))
    end if
    if (present(order)) then
        if (order == -1) then
            a(1:size(a, 1)) = a(size(a, 1):1:-1)
        end if
    end if
end subroutine quick_sort_int
!
!> Quick sort array into ascending or descending order
!
function qksort_int(a, order) result(b)
    integer, dimension(:) :: a
    integer, intent(in), optional :: order
    integer, allocatable, dimension(:) :: b
    allocate (b(1:size(a)), source=a)
    if (present(order)) then
        call quick_sort_int(b, order)
    else
        call quick_sort_int(b)
    end if
end function qksort_int
!
! Split a list into two sublists, using the first element
! as a pivot, and return the position of the element about which the
! list was divided. Local variables used are:
! left : position of the first element
! right : position of the last element
! pivot : pivot element
! swap : used to swap elements
!
! accepts: array a and positions low and high of the first and last elements
! returns: array a (modified) with elements in ascending order
!
subroutine split_int(a, low, high, mid, indices)
    integer, dimension(:), intent(inout) :: a
    integer, intent(in) :: low, high
    integer, intent(out) :: mid
    integer, dimension(:), intent(inout) :: indices
    integer :: left, right
    integer :: pivot, swap
    integer :: ipivot, iswap
    integer :: n
    left = low
    right = high
    pivot = a(low)
    ipivot = indices(low)
    n = size(a)
    ! Repeat the following while left and right haven't met
    do
        if (left >= right) then
            exit
        endif
        ! Scan right to left to find element < pivot
        do while (right >= 1)
            if (left >= right .or. a(right) < pivot) then
                exit
            end if
            right = right - 1
        end do
        ! Scan left to right to find element > pivot
        do while (left <= n)
            if (a(left) > pivot) then
                exit
            end if
            left = left + 1
        end do
        ! If left and right haven't met, then exchange the element and index
        if (left < right) then
            swap = a(left)
            a(left) = a(right)
            a(right) = swap
            iswap = indices(left)
            indices(left) = indices(right)
            indices(right) = iswap
        end if
    end do
    ! Switch element and index in split position with pivot
    a(low) = a(right)
    a(right) = pivot
    mid = right
    indices(low) = indices(right)
    indices(right) = ipivot
end subroutine split_int
recursive subroutine quick_sort_index_int(a, first, last, indices)
    integer, dimension(:), intent(inout) :: a
    integer, intent(in) :: first, last
    integer, dimension(:), intent(inout) :: indices
    integer :: mid
    ! If list size >= 2
    if (first < last) then
        ! Split it
        call split_int(a, first, last, mid, indices)
        ! Sort left half
        call quick_sort_index_int(a, first, mid - 1, indices)
        ! Sort right half
        call quick_sort_index_int(a, mid + 1, last, indices)
    end if
end subroutine quick_sort_index_int
subroutine qksort_index_int(a, indices, order)
    integer, dimension(:), intent(inout) :: a
    integer, allocatable, dimension(:), intent(inout) :: indices
    integer, optional, intent(in) :: order
    integer :: i
    indices = [(i, i=1, size(a))]
    call quick_sort_index_int(a, 1, size(a), indices)
    ! The default order is ascending
    if (present(order)) then
        if (order == -1) then
            a = a(size(a):1:-1)
            indices = indices(size(indices):1:-1)
        end if
    end if
end subroutine qksort_index_int
!
!> Sort a float matrix based on one of the columns
!
function qksort_2d_int(a, col, order) result(asort)
    integer, dimension(:, :) :: a
    integer, optional :: col, order
    integer, allocatable :: asort(:, :)
    integer, allocatable, dimension(:) :: b
    integer, allocatable, dimension(:) :: indices
    integer :: which_col, n, i, sort_order
    ! Size of array
    n = size(a, 1)
    ! Sort based on which column
    if (present(col)) then
        which_col = col
    else
        which_col = 1
    end if
    which_col = max(1, min(size(a, 2), which_col))
    ! Sort based on which order
    if (present(order)) then
        sort_order = order
    else
        sort_order = 1
    end if
    ! Sort
    allocate (b(1:n), source=a(:, which_col))
    allocate (indices(1:n))
    call qksort_index_int(b, indices, sort_order)
    ! Output
    allocate (asort(1:n, 1:size(a, 2)))
    do i = 1, n
        asort(i, :) = a(indices(i), :)
    end do
end function qksort_2d_int
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
subroutine merge_sort_index_float(r, d)
    real, intent(in), dimension(:) :: r
    integer, intent(out), dimension(size(r)) :: d
    integer, dimension(size(r)) :: il
    integer :: stepsize
    integer :: i, j, n, left, k, ksize
    n = size(r)
    do i = 1, n
        d(i) = i
    end do
    if (n == 1) then
        return
    end if
    stepsize = 1
    do while (stepsize < n)
        do left = 1, n - stepsize, stepsize*2
            i = left
            j = left + stepsize
            ksize = min(stepsize*2, n - left + 1)
            k = 1
            do while (i < left + stepsize .and. j < left + ksize)
                if (r(d(i)) > r(d(j))) then
                    il(k) = d(i)
                    i = i + 1
                    k = k + 1
                else
                    il(k) = d(j)
                    j = j + 1
                    k = k + 1
                endif
            enddo
            if (i < left + stepsize) then
                ! fill up remaining from left
                il(k:ksize) = d(i:left + stepsize - 1)
            else
                ! fill up remaining from right
                il(k:ksize) = d(j:left + ksize - 1)
            endif
            d(left:left + ksize - 1) = il(1:ksize)
        end do
        stepsize = stepsize*2
    end do
end subroutine merge_sort_index_float
!
!> Recursive quick-sort routine sorting array into ascending or descending order
!
!> Reformatted and modified from
!> https://gist.github.com/t-nissie/479f0f16966925fa29ea#file-quicksort-f
!> Original author:
!> Takeshi Nishimatsu
!> License:
!> GPLv3
!
recursive subroutine quick_sort_float(a, order)
    real, dimension(:), intent(inout) :: a
    integer, intent(in), optional :: order
    real :: x, t
    integer :: first, last
    integer :: i, j
    first = 1
    last = size(a, 1)
    x = a((first + last)/2)
    i = first
    j = last
    do
        do while (a(i) < x)
            i = i + 1
        end do
        do while (x < a(j))
            j = j - 1
        end do
        if (i >= j) exit
        t = a(i)
        a(i) = a(j)
        a(j) = t
        i = i + 1
        j = j - 1
    end do
    if (first < i - 1) then
        call quick_sort_float(a(first:i - 1))
    end if
    if (j + 1 < last) then
        call quick_sort_float(a(j + 1:last))
    end if
    if (present(order)) then
        if (order == -1) then
            a(1:size(a, 1)) = a(size(a, 1):1:-1)
        end if
    end if
end subroutine quick_sort_float
!
!> Quick sort array into ascending or descending order
!
function qksort_float(a, order) result(b)
    real, dimension(:) :: a
    integer, intent(in), optional :: order
    real, allocatable, dimension(:) :: b
    allocate (b(1:size(a)), source=a)
    if (present(order)) then
        call quick_sort_float(b, order)
    else
        call quick_sort_float(b)
    end if
end function qksort_float
!
! Split a list into two sublists, using the first element
! as a pivot, and return the position of the element about which the
! list was divided. Local variables used are:
! left : position of the first element
! right : position of the last element
! pivot : pivot element
! swap : used to swap elements
!
! accepts: array a and positions low and high of the first and last elements
! returns: array a (modified) with elements in ascending order
!
subroutine split_float(a, low, high, mid, indices)
    real, dimension(:), intent(inout) :: a
    integer, intent(in) :: low, high
    integer, intent(out) :: mid
    integer, dimension(:), intent(inout) :: indices
    integer :: left, right
    real :: pivot, swap
    integer :: ipivot, iswap
    integer :: n
    left = low
    right = high
    pivot = a(low)
    ipivot = indices(low)
    n = size(a)
    ! Repeat the following while left and right haven't met
    do
        if (left >= right) then
            exit
        endif
        ! Scan right to left to find element < pivot
        do while (right >= 1)
            if (left >= right .or. a(right) < pivot) then
                exit
            end if
            right = right - 1
        end do
        ! Scan left to right to find element > pivot
        do while (left <= n)
            if (a(left) > pivot) then
                exit
            end if
            left = left + 1
        end do
        ! If left and right haven't met, then exchange the element and index
        if (left < right) then
            swap = a(left)
            a(left) = a(right)
            a(right) = swap
            iswap = indices(left)
            indices(left) = indices(right)
            indices(right) = iswap
        end if
    end do
    ! Switch element and index in split position with pivot
    a(low) = a(right)
    a(right) = pivot
    mid = right
    indices(low) = indices(right)
    indices(right) = ipivot
end subroutine split_float
recursive subroutine quick_sort_index_float(a, first, last, indices)
    real, dimension(:), intent(inout) :: a
    integer, intent(in) :: first, last
    integer, dimension(:), intent(inout) :: indices
    integer :: mid
    ! If list size >= 2
    if (first < last) then
        ! Split it
        call split_float(a, first, last, mid, indices)
        ! Sort left half
        call quick_sort_index_float(a, first, mid - 1, indices)
        ! Sort right half
        call quick_sort_index_float(a, mid + 1, last, indices)
    end if
end subroutine quick_sort_index_float
subroutine qksort_index_float(a, indices, order)
    real, dimension(:), intent(inout) :: a
    integer, allocatable, dimension(:), intent(inout) :: indices
    integer, optional, intent(in) :: order
    integer :: i
    indices = [(i, i=1, size(a))]
    call quick_sort_index_float(a, 1, size(a), indices)
    ! The default order is ascending
    if (present(order)) then
        if (order == -1) then
            a = a(size(a):1:-1)
            indices = indices(size(indices):1:-1)
        end if
    end if
end subroutine qksort_index_float
!
!> Sort a float matrix based on one of the columns
!
function qksort_2d_float(a, col, order) result(asort)
    real, dimension(:, :) :: a
    integer, optional :: col, order
    real, allocatable :: asort(:, :)
    real, allocatable, dimension(:) :: b
    integer, allocatable, dimension(:) :: indices
    integer :: which_col, n, i, sort_order
    ! Size of array
    n = size(a, 1)
    ! Sort based on which column
    if (present(col)) then
        which_col = col
    else
        which_col = 1
    end if
    which_col = max(1, min(size(a, 2), which_col))
    ! Sort based on which order
    if (present(order)) then
        sort_order = order
    else
        sort_order = 1
    end if
    ! Sort
    allocate (b(1:n), source=a(:, which_col))
    allocate (indices(1:n))
    call qksort_index_float(b, indices, sort_order)
    ! Output
    allocate (asort(1:n, 1:size(a, 2)))
    do i = 1, n
        asort(i, :) = a(indices(i), :)
    end do
end function qksort_2d_float
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
subroutine merge_sort_index_double(r, d)
    double precision, intent(in), dimension(:) :: r
    integer, intent(out), dimension(size(r)) :: d
    integer, dimension(size(r)) :: il
    integer :: stepsize
    integer :: i, j, n, left, k, ksize
    n = size(r)
    do i = 1, n
        d(i) = i
    end do
    if (n == 1) then
        return
    end if
    stepsize = 1
    do while (stepsize < n)
        do left = 1, n - stepsize, stepsize*2
            i = left
            j = left + stepsize
            ksize = min(stepsize*2, n - left + 1)
            k = 1
            do while (i < left + stepsize .and. j < left + ksize)
                if (r(d(i)) > r(d(j))) then
                    il(k) = d(i)
                    i = i + 1
                    k = k + 1
                else
                    il(k) = d(j)
                    j = j + 1
                    k = k + 1
                endif
            enddo
            if (i < left + stepsize) then
                ! fill up remaining from left
                il(k:ksize) = d(i:left + stepsize - 1)
            else
                ! fill up remaining from right
                il(k:ksize) = d(j:left + ksize - 1)
            endif
            d(left:left + ksize - 1) = il(1:ksize)
        end do
        stepsize = stepsize*2
    end do
end subroutine merge_sort_index_double
!
!> Recursive quick-sort routine sorting array into ascending or descending order
!
!> Reformatted and modified from
!> https://gist.github.com/t-nissie/479f0f16966925fa29ea#file-quicksort-f
!> Original author:
!> Takeshi Nishimatsu
!> License:
!> GPLv3
!
recursive subroutine quick_sort_double(a, order)
    double precision, dimension(:), intent(inout) :: a
    integer, intent(in), optional :: order
    double precision :: x, t
    integer :: first, last
    integer :: i, j
    first = 1
    last = size(a, 1)
    x = a((first + last)/2)
    i = first
    j = last
    do
        do while (a(i) < x)
            i = i + 1
        end do
        do while (x < a(j))
            j = j - 1
        end do
        if (i >= j) exit
        t = a(i)
        a(i) = a(j)
        a(j) = t
        i = i + 1
        j = j - 1
    end do
    if (first < i - 1) then
        call quick_sort_double(a(first:i - 1))
    end if
    if (j + 1 < last) then
        call quick_sort_double(a(j + 1:last))
    end if
    if (present(order)) then
        if (order == -1) then
            a(1:size(a, 1)) = a(size(a, 1):1:-1)
        end if
    end if
end subroutine quick_sort_double
!
!> Quick sort array into ascending or descending order
!
function qksort_double(a, order) result(b)
    double precision, dimension(:) :: a
    integer, intent(in), optional :: order
    double precision, allocatable, dimension(:) :: b
    allocate (b(1:size(a)), source=a)
    if (present(order)) then
        call quick_sort_double(b, order)
    else
        call quick_sort_double(b)
    end if
end function qksort_double
!
! Split a list into two sublists, using the first element
! as a pivot, and return the position of the element about which the
! list was divided. Local variables used are:
! left : position of the first element
! right : position of the last element
! pivot : pivot element
! swap : used to swap elements
!
! accepts: array a and positions low and high of the first and last elements
! returns: array a (modified) with elements in ascending order
!
subroutine split_double(a, low, high, mid, indices)
    double precision, dimension(:), intent(inout) :: a
    integer, intent(in) :: low, high
    integer, intent(out) :: mid
    integer, dimension(:), intent(inout) :: indices
    integer :: left, right
    double precision :: pivot, swap
    integer :: ipivot, iswap
    integer :: n
    left = low
    right = high
    pivot = a(low)
    ipivot = indices(low)
    n = size(a)
    ! Repeat the following while left and right haven't met
    do
        if (left >= right) then
            exit
        endif
        ! Scan right to left to find element < pivot
        do while (right >= 1)
            if (left >= right .or. a(right) < pivot) then
                exit
            end if
            right = right - 1
        end do
        ! Scan left to right to find element > pivot
        do while (left <= n)
            if (a(left) > pivot) then
                exit
            end if
            left = left + 1
        end do
        ! If left and right haven't met, then exchange the element and index
        if (left < right) then
            swap = a(left)
            a(left) = a(right)
            a(right) = swap
            iswap = indices(left)
            indices(left) = indices(right)
            indices(right) = iswap
        end if
    end do
    ! Switch element and index in split position with pivot
    a(low) = a(right)
    a(right) = pivot
    mid = right
    indices(low) = indices(right)
    indices(right) = ipivot
end subroutine split_double
recursive subroutine quick_sort_index_double(a, first, last, indices)
    double precision, dimension(:), intent(inout) :: a
    integer, intent(in) :: first, last
    integer, dimension(:), intent(inout) :: indices
    integer :: mid
    ! If list size >= 2
    if (first < last) then
        ! Split it
        call split_double(a, first, last, mid, indices)
        ! Sort left half
        call quick_sort_index_double(a, first, mid - 1, indices)
        ! Sort right half
        call quick_sort_index_double(a, mid + 1, last, indices)
    end if
end subroutine quick_sort_index_double
subroutine qksort_index_double(a, indices, order)
    double precision, dimension(:), intent(inout) :: a
    integer, allocatable, dimension(:), intent(inout) :: indices
    integer, optional, intent(in) :: order
    integer :: i
    indices = [(i, i=1, size(a))]
    call quick_sort_index_double(a, 1, size(a), indices)
    ! The default order is ascending
    if (present(order)) then
        if (order == -1) then
            a = a(size(a):1:-1)
            indices = indices(size(indices):1:-1)
        end if
    end if
end subroutine qksort_index_double
!
!> Sort a float matrix based on one of the columns
!
function qksort_2d_double(a, col, order) result(asort)
    double precision, dimension(:, :) :: a
    integer, optional :: col, order
    double precision, allocatable :: asort(:, :)
    double precision, allocatable, dimension(:) :: b
    integer, allocatable, dimension(:) :: indices
    integer :: which_col, n, i, sort_order
    ! Size of array
    n = size(a, 1)
    ! Sort based on which column
    if (present(col)) then
        which_col = col
    else
        which_col = 1
    end if
    which_col = max(1, min(size(a, 2), which_col))
    ! Sort based on which order
    if (present(order)) then
        sort_order = order
    else
        sort_order = 1
    end if
    ! Sort
    allocate (b(1:n), source=a(:, which_col))
    allocate (indices(1:n))
    call qksort_index_double(b, indices, sort_order)
    ! Output
    allocate (asort(1:n, 1:size(a, 2)))
    do i = 1, n
        asort(i, :) = a(indices(i), :)
    end do
end function qksort_2d_double
end module libflit_sort
