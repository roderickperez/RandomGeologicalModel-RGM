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
! Copyright (c) 2014, Daniel Pena
! All rights reserved.
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
! 1. Redistributions of source code must retain the above copyright notice, this
! list of conditions and the following disclaimer.
! 2. Redistributions in binary form must reproduce the above copyright notice,
! this list of conditions and the following disclaimer in the documentation
! and/or other materials provided with the distribution.
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
! ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
! ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!
! Note by Kai Gao:
! This is a reformatted and modified version of the original code
! to accommodate libflit's template programming.
! It must be inserted into libflit compilation flow to work.
!
module mheap_int
    implicit none
    abstract interface
        function heapfunc_(node1, node2) result(res)
            integer, intent(in) :: node1(:)
            integer, intent(in) :: node2(:)
            logical :: res
        end function heapfunc_
    end interface
    type :: heap_int
        integer :: nmax ! max size
        integer :: n ! current heap size
        integer :: m ! current tree size
        integer :: nlen ! node size in real units
        integer, allocatable :: data(:, :) ! node data
        integer, allocatable :: indx(:) ! nodes index
        procedure(heapfunc_), nopass, pointer :: fun ! heap function to find root node
    contains
        procedure :: init => heap_init_
        procedure :: insert => heap_insert_
        procedure :: peek => heap_peek_
        procedure :: pop => heap_pop_
        procedure :: reheap => heap_reheap_
        procedure :: size => heap_size_
        procedure :: free => heap_free_
    end type heap_int
    private
    public :: heap_int
contains
    ! returns the heap current size
    integer function heap_size_(heap)
        class(heap_int) :: heap
        heap_size_ = heap%n
    end function heap_size_
    ! initializes the heap
    ! nmax - max size of the heap
    ! nlen - size of each node
    ! hpfun - the heap function (provides comparison between two nodes' data)
    subroutine heap_init_(heap, nmax, nlen, hpfun)
        class(heap_int) :: heap
        integer, intent(in) :: nmax, nlen
        procedure(heapfunc_) :: hpfun
        integer :: i
        heap%nmax = nmax
        heap%n = 0
        heap%m = 0
        heap%nlen = nlen
        heap%fun => hpfun
        allocate (heap%indx(nmax))
        allocate (heap%data(nlen, nmax))
        do i = 1, nmax
            heap%indx(i) = i
        end do
    end subroutine heap_init_
    ! releases all the allocated memory and resets the heap
    subroutine heap_free_(heap)
        class(heap_int) :: heap
        deallocate (heap%indx)
        deallocate (heap%data)
        heap%n = 0
        heap%m = 0
        heap%nmax = 0
        heap%fun => null()
    end subroutine heap_free_
    ! insert a node into a heap. the resulting tree is re-heaped.
    ! input
    ! heap - the heap
    ! node - a real array, nlen long, which
    ! contains the node's information to be inserted.
    subroutine heap_insert_(heap, node)
        class(heap_int) :: heap
        integer, intent(in) :: node(heap%nlen)
        integer :: k1, k2, il, ir
        if (heap%n == heap%nmax) then
            return
        end if
        ! add one element and copy node data to new element
        heap%n = heap%n + 1
        heap%m = heap%m + 1
        heap%data(:, heap%indx(heap%n)) = node(:)
        ! re-index the heap from the bottom up
        k2 = heap%n
        do while (k2 /= 1)
            k1 = k2/2
            ir = heap%indx(k2)
            il = heap%indx(k1)
            if (heap%fun(heap%data(:, il), heap%data(:, ir))) then
                return
            end if
            call swapint(heap%indx(k2), heap%indx(k1))
            k2 = k2/2
        end do
    end subroutine heap_insert_
    ! retrieve the root element off the heap. the resulting tree is re-heaped.
    ! no data is deleted, thus the original
    ! input
    ! heap - the heap
    ! output
    ! node - the deleted node
    subroutine heap_pop_(heap, node)
        class(heap_int) :: heap
        integer, optional :: node(heap%nlen)
        if (heap%n == 0) then
            return
        end if
        if (present(node)) then
            node(:) = heap%data(:, heap%indx(1))
        end if
        call swapint(heap%indx(1), heap%indx(heap%n))
        heap%n = heap%n - 1
        call heap_grow_(heap, 1)
    end subroutine heap_pop_
    ! access the k-th node of the heap
    subroutine heap_peek_(heap, k, node)
        class(heap_int) :: heap
        integer, intent(in) :: k
        integer, intent(out) :: node(heap%nlen)
        if (k < 1 .or. k > heap%n .or. heap%n > heap%nmax) then
            return
        end if
        node(:) = heap%data(:, heap%indx(k))
    end subroutine heap_peek_
    ! forms a heap out of a tree. used privately by heap_reheap.
    ! the root node of the tree is stored in the location indx(ktemp).
    ! the first child node is in location indx(2*ktemp)...
    ! the next child node is in location indx(2*ktemp+1).
    ! this subroutines assumes each branch of the tree is itself a heap.
    subroutine heap_grow_(heap, ktemp)
        integer :: i, k, il, ir
        type(heap_int) :: heap
        integer :: ktemp
        if (heap%n > heap%nmax) return
        k = ktemp
        do while (2*k <= heap%n)
            i = 2*k
            ! if there is more than one child node, find which is the smallest.
            if (2*k /= heap%n) then
                il = heap%indx(2*k + 1)
                ir = heap%indx(2*k)
                if (heap%fun(heap%data(:, il), heap%data(:, ir))) then
                    i = i + 1
                end if
            end if
            ! if a child is larger than its parent, interchange them... this destroys
            ! the heap property, so the remaining elements must be re-heaped.
            il = heap%indx(k)
            ir = heap%indx(i)
            if (heap%fun(heap%data(:, il), heap%data(:, ir))) then
                return
            end if
            call swapint(heap%indx(i), heap%indx(k))
            k = i
        end do
    end subroutine heap_grow_
    !
    ! builds the heap from the element data using the provided heap function.
    ! at exit, the root node satisfies the heap condition:
    ! hpfun( root_node, node ) = .true. for any other node
    !
    subroutine heap_reheap_(heap, hpfun)
        class(heap_int) :: heap
        procedure(heapfunc_), optional :: hpfun
        integer :: k
        heap%n = heap%m
        if (present(hpfun)) then
            heap%fun => hpfun
        end if
        if (heap%nmax < heap%n) then
            return
        end if
        do k = heap%n/2, 1, -1
            call heap_grow_(heap, k)
        end do
    end subroutine heap_reheap_
    subroutine swapint(i, k)
        integer :: i, k, t
        t = i
        i = k
        k = t
    end subroutine swapint
end module mheap_int
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
! Copyright (c) 2014, Daniel Pena
! All rights reserved.
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
! 1. Redistributions of source code must retain the above copyright notice, this
! list of conditions and the following disclaimer.
! 2. Redistributions in binary form must reproduce the above copyright notice,
! this list of conditions and the following disclaimer in the documentation
! and/or other materials provided with the distribution.
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
! ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
! ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!
! Note by Kai Gao:
! This is a reformatted and modified version of the original code
! to accommodate libflit's template programming.
! It must be inserted into libflit compilation flow to work.
!
module mheap_float
    implicit none
    abstract interface
        function heapfunc_(node1, node2) result(res)
            real, intent(in) :: node1(:)
            real, intent(in) :: node2(:)
            logical :: res
        end function heapfunc_
    end interface
    type :: heap_float
        integer :: nmax ! max size
        integer :: n ! current heap size
        integer :: m ! current tree size
        integer :: nlen ! node size in real units
        real, allocatable :: data(:, :) ! node data
        integer, allocatable :: indx(:) ! nodes index
        procedure(heapfunc_), nopass, pointer :: fun ! heap function to find root node
    contains
        procedure :: init => heap_init_
        procedure :: insert => heap_insert_
        procedure :: peek => heap_peek_
        procedure :: pop => heap_pop_
        procedure :: reheap => heap_reheap_
        procedure :: size => heap_size_
        procedure :: free => heap_free_
    end type heap_float
    private
    public :: heap_float
contains
    ! returns the heap current size
    integer function heap_size_(heap)
        class(heap_float) :: heap
        heap_size_ = heap%n
    end function heap_size_
    ! initializes the heap
    ! nmax - max size of the heap
    ! nlen - size of each node
    ! hpfun - the heap function (provides comparison between two nodes' data)
    subroutine heap_init_(heap, nmax, nlen, hpfun)
        class(heap_float) :: heap
        integer, intent(in) :: nmax, nlen
        procedure(heapfunc_) :: hpfun
        integer :: i
        heap%nmax = nmax
        heap%n = 0
        heap%m = 0
        heap%nlen = nlen
        heap%fun => hpfun
        allocate (heap%indx(nmax))
        allocate (heap%data(nlen, nmax))
        do i = 1, nmax
            heap%indx(i) = i
        end do
    end subroutine heap_init_
    ! releases all the allocated memory and resets the heap
    subroutine heap_free_(heap)
        class(heap_float) :: heap
        deallocate (heap%indx)
        deallocate (heap%data)
        heap%n = 0
        heap%m = 0
        heap%nmax = 0
        heap%fun => null()
    end subroutine heap_free_
    ! insert a node into a heap. the resulting tree is re-heaped.
    ! input
    ! heap - the heap
    ! node - a real array, nlen long, which
    ! contains the node's information to be inserted.
    subroutine heap_insert_(heap, node)
        class(heap_float) :: heap
        real, intent(in) :: node(heap%nlen)
        integer :: k1, k2, il, ir
        if (heap%n == heap%nmax) then
            return
        end if
        ! add one element and copy node data to new element
        heap%n = heap%n + 1
        heap%m = heap%m + 1
        heap%data(:, heap%indx(heap%n)) = node(:)
        ! re-index the heap from the bottom up
        k2 = heap%n
        do while (k2 /= 1)
            k1 = k2/2
            ir = heap%indx(k2)
            il = heap%indx(k1)
            if (heap%fun(heap%data(:, il), heap%data(:, ir))) then
                return
            end if
            call swapint(heap%indx(k2), heap%indx(k1))
            k2 = k2/2
        end do
    end subroutine heap_insert_
    ! retrieve the root element off the heap. the resulting tree is re-heaped.
    ! no data is deleted, thus the original
    ! input
    ! heap - the heap
    ! output
    ! node - the deleted node
    subroutine heap_pop_(heap, node)
        class(heap_float) :: heap
        real, optional :: node(heap%nlen)
        if (heap%n == 0) then
            return
        end if
        if (present(node)) then
            node(:) = heap%data(:, heap%indx(1))
        end if
        call swapint(heap%indx(1), heap%indx(heap%n))
        heap%n = heap%n - 1
        call heap_grow_(heap, 1)
    end subroutine heap_pop_
    ! access the k-th node of the heap
    subroutine heap_peek_(heap, k, node)
        class(heap_float) :: heap
        integer, intent(in) :: k
        real, intent(out) :: node(heap%nlen)
        if (k < 1 .or. k > heap%n .or. heap%n > heap%nmax) then
            return
        end if
        node(:) = heap%data(:, heap%indx(k))
    end subroutine heap_peek_
    ! forms a heap out of a tree. used privately by heap_reheap.
    ! the root node of the tree is stored in the location indx(ktemp).
    ! the first child node is in location indx(2*ktemp)...
    ! the next child node is in location indx(2*ktemp+1).
    ! this subroutines assumes each branch of the tree is itself a heap.
    subroutine heap_grow_(heap, ktemp)
        integer :: i, k, il, ir
        type(heap_float) :: heap
        integer :: ktemp
        if (heap%n > heap%nmax) return
        k = ktemp
        do while (2*k <= heap%n)
            i = 2*k
            ! if there is more than one child node, find which is the smallest.
            if (2*k /= heap%n) then
                il = heap%indx(2*k + 1)
                ir = heap%indx(2*k)
                if (heap%fun(heap%data(:, il), heap%data(:, ir))) then
                    i = i + 1
                end if
            end if
            ! if a child is larger than its parent, interchange them... this destroys
            ! the heap property, so the remaining elements must be re-heaped.
            il = heap%indx(k)
            ir = heap%indx(i)
            if (heap%fun(heap%data(:, il), heap%data(:, ir))) then
                return
            end if
            call swapint(heap%indx(i), heap%indx(k))
            k = i
        end do
    end subroutine heap_grow_
    !
    ! builds the heap from the element data using the provided heap function.
    ! at exit, the root node satisfies the heap condition:
    ! hpfun( root_node, node ) = .true. for any other node
    !
    subroutine heap_reheap_(heap, hpfun)
        class(heap_float) :: heap
        procedure(heapfunc_), optional :: hpfun
        integer :: k
        heap%n = heap%m
        if (present(hpfun)) then
            heap%fun => hpfun
        end if
        if (heap%nmax < heap%n) then
            return
        end if
        do k = heap%n/2, 1, -1
            call heap_grow_(heap, k)
        end do
    end subroutine heap_reheap_
    subroutine swapint(i, k)
        integer :: i, k, t
        t = i
        i = k
        k = t
    end subroutine swapint
end module mheap_float
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
! Copyright (c) 2014, Daniel Pena
! All rights reserved.
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
! 1. Redistributions of source code must retain the above copyright notice, this
! list of conditions and the following disclaimer.
! 2. Redistributions in binary form must reproduce the above copyright notice,
! this list of conditions and the following disclaimer in the documentation
! and/or other materials provided with the distribution.
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
! ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
! ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!
! Note by Kai Gao:
! This is a reformatted and modified version of the original code
! to accommodate libflit's template programming.
! It must be inserted into libflit compilation flow to work.
!
module mheap_double
    implicit none
    abstract interface
        function heapfunc_(node1, node2) result(res)
            double precision, intent(in) :: node1(:)
            double precision, intent(in) :: node2(:)
            logical :: res
        end function heapfunc_
    end interface
    type :: heap_double
        integer :: nmax ! max size
        integer :: n ! current heap size
        integer :: m ! current tree size
        integer :: nlen ! node size in real units
        double precision, allocatable :: data(:, :) ! node data
        integer, allocatable :: indx(:) ! nodes index
        procedure(heapfunc_), nopass, pointer :: fun ! heap function to find root node
    contains
        procedure :: init => heap_init_
        procedure :: insert => heap_insert_
        procedure :: peek => heap_peek_
        procedure :: pop => heap_pop_
        procedure :: reheap => heap_reheap_
        procedure :: size => heap_size_
        procedure :: free => heap_free_
    end type heap_double
    private
    public :: heap_double
contains
    ! returns the heap current size
    integer function heap_size_(heap)
        class(heap_double) :: heap
        heap_size_ = heap%n
    end function heap_size_
    ! initializes the heap
    ! nmax - max size of the heap
    ! nlen - size of each node
    ! hpfun - the heap function (provides comparison between two nodes' data)
    subroutine heap_init_(heap, nmax, nlen, hpfun)
        class(heap_double) :: heap
        integer, intent(in) :: nmax, nlen
        procedure(heapfunc_) :: hpfun
        integer :: i
        heap%nmax = nmax
        heap%n = 0
        heap%m = 0
        heap%nlen = nlen
        heap%fun => hpfun
        allocate (heap%indx(nmax))
        allocate (heap%data(nlen, nmax))
        do i = 1, nmax
            heap%indx(i) = i
        end do
    end subroutine heap_init_
    ! releases all the allocated memory and resets the heap
    subroutine heap_free_(heap)
        class(heap_double) :: heap
        deallocate (heap%indx)
        deallocate (heap%data)
        heap%n = 0
        heap%m = 0
        heap%nmax = 0
        heap%fun => null()
    end subroutine heap_free_
    ! insert a node into a heap. the resulting tree is re-heaped.
    ! input
    ! heap - the heap
    ! node - a real array, nlen long, which
    ! contains the node's information to be inserted.
    subroutine heap_insert_(heap, node)
        class(heap_double) :: heap
        double precision, intent(in) :: node(heap%nlen)
        integer :: k1, k2, il, ir
        if (heap%n == heap%nmax) then
            return
        end if
        ! add one element and copy node data to new element
        heap%n = heap%n + 1
        heap%m = heap%m + 1
        heap%data(:, heap%indx(heap%n)) = node(:)
        ! re-index the heap from the bottom up
        k2 = heap%n
        do while (k2 /= 1)
            k1 = k2/2
            ir = heap%indx(k2)
            il = heap%indx(k1)
            if (heap%fun(heap%data(:, il), heap%data(:, ir))) then
                return
            end if
            call swapint(heap%indx(k2), heap%indx(k1))
            k2 = k2/2
        end do
    end subroutine heap_insert_
    ! retrieve the root element off the heap. the resulting tree is re-heaped.
    ! no data is deleted, thus the original
    ! input
    ! heap - the heap
    ! output
    ! node - the deleted node
    subroutine heap_pop_(heap, node)
        class(heap_double) :: heap
        double precision, optional :: node(heap%nlen)
        if (heap%n == 0) then
            return
        end if
        if (present(node)) then
            node(:) = heap%data(:, heap%indx(1))
        end if
        call swapint(heap%indx(1), heap%indx(heap%n))
        heap%n = heap%n - 1
        call heap_grow_(heap, 1)
    end subroutine heap_pop_
    ! access the k-th node of the heap
    subroutine heap_peek_(heap, k, node)
        class(heap_double) :: heap
        integer, intent(in) :: k
        double precision, intent(out) :: node(heap%nlen)
        if (k < 1 .or. k > heap%n .or. heap%n > heap%nmax) then
            return
        end if
        node(:) = heap%data(:, heap%indx(k))
    end subroutine heap_peek_
    ! forms a heap out of a tree. used privately by heap_reheap.
    ! the root node of the tree is stored in the location indx(ktemp).
    ! the first child node is in location indx(2*ktemp)...
    ! the next child node is in location indx(2*ktemp+1).
    ! this subroutines assumes each branch of the tree is itself a heap.
    subroutine heap_grow_(heap, ktemp)
        integer :: i, k, il, ir
        type(heap_double) :: heap
        integer :: ktemp
        if (heap%n > heap%nmax) return
        k = ktemp
        do while (2*k <= heap%n)
            i = 2*k
            ! if there is more than one child node, find which is the smallest.
            if (2*k /= heap%n) then
                il = heap%indx(2*k + 1)
                ir = heap%indx(2*k)
                if (heap%fun(heap%data(:, il), heap%data(:, ir))) then
                    i = i + 1
                end if
            end if
            ! if a child is larger than its parent, interchange them... this destroys
            ! the heap property, so the remaining elements must be re-heaped.
            il = heap%indx(k)
            ir = heap%indx(i)
            if (heap%fun(heap%data(:, il), heap%data(:, ir))) then
                return
            end if
            call swapint(heap%indx(i), heap%indx(k))
            k = i
        end do
    end subroutine heap_grow_
    !
    ! builds the heap from the element data using the provided heap function.
    ! at exit, the root node satisfies the heap condition:
    ! hpfun( root_node, node ) = .true. for any other node
    !
    subroutine heap_reheap_(heap, hpfun)
        class(heap_double) :: heap
        procedure(heapfunc_), optional :: hpfun
        integer :: k
        heap%n = heap%m
        if (present(hpfun)) then
            heap%fun => hpfun
        end if
        if (heap%nmax < heap%n) then
            return
        end if
        do k = heap%n/2, 1, -1
            call heap_grow_(heap, k)
        end do
    end subroutine heap_reheap_
    subroutine swapint(i, k)
        integer :: i, k, t
        t = i
        i = k
        k = t
    end subroutine swapint
end module mheap_double
module libflit_heap
    use mheap_int
    use mheap_float
    use mheap_double
end module libflit_heap
