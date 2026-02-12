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
module libflit_meta_array_int
    use libflit_array
    use libflit_error
    use libflit_string
    implicit none
    type meta_array1_int
        character(len=24) :: name
        integer :: id
        integer :: n1
        integer, allocatable, dimension(:) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array1_
        procedure, private :: copy_meta_array1_
        procedure, public :: init => initialize_meta_array1_
    end type meta_array1_int
    type meta_array2_int
        character(len=24) :: name
        integer :: id
        integer :: n1, n2
        integer, allocatable, dimension(:, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array2_
        procedure, private :: copy_meta_array2_
        procedure, public :: init => initialize_meta_array2_
    end type meta_array2_int
    type meta_array3_int
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3
        integer, allocatable, dimension(:, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array3_
        procedure, private :: copy_meta_array3_
        procedure, public :: init => initialize_meta_array3_
    end type meta_array3_int
    type meta_array4_int
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3, n4
        integer, allocatable, dimension(:, :, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array4_
        procedure, private :: copy_meta_array4_
        procedure, public :: init => initialize_meta_array4_
    end type meta_array4_int
    type list_meta_array1_int
        integer :: n
        type(meta_array1_int), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array1_
        procedure, public :: init => initialize_list_meta_array1_
        procedure, public :: size => size_of_list_meta_array1_
        procedure, public :: has => probe_existence_list_meta_array1_
        procedure, public :: get_core => get_core_from_list_meta_array1_
    end type list_meta_array1_int
    type list_meta_array2_int
        integer :: n
        type(meta_array2_int), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array2_
        procedure, public :: init => initialize_list_meta_array2_
        procedure, public :: size => size_of_list_meta_array2_
        procedure, public :: has => probe_existence_list_meta_array2_
        procedure, public :: get_core => get_core_from_list_meta_array2_
    end type list_meta_array2_int
    type list_meta_array3_int
        integer :: n
        type(meta_array3_int), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array3_
        procedure, public :: init => initialize_list_meta_array3_
        procedure, public :: size => size_of_list_meta_array3_
        procedure, public :: has => probe_existence_list_meta_array3_
        procedure, public :: get_core => get_core_from_list_meta_array3_
    end type list_meta_array3_int
    type list_meta_array4_int
        integer :: n
        type(meta_array4_int), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array4_
        procedure, public :: init => initialize_list_meta_array4_
        procedure, public :: size => size_of_list_meta_array4_
        procedure, public :: has => probe_existence_list_meta_array4_
        procedure, public :: get_core => get_core_from_list_meta_array4_
    end type list_meta_array4_int
    private
    public :: meta_array1_int
    public :: meta_array2_int
    public :: meta_array3_int
    public :: meta_array4_int
    public :: list_meta_array1_int
    public :: list_meta_array2_int
    public :: list_meta_array3_int
    public :: list_meta_array4_int
    public :: get_meta_array1_int
    public :: get_meta_array2_int
    public :: get_meta_array3_int
    public :: get_meta_array4_int
    public :: get_meta_array_core1_int
    public :: get_meta_array_core2_int
    public :: get_meta_array_core3_int
    public :: get_meta_array_core4_int
    public :: set_meta_array_core1_int
    public :: set_meta_array_core2_int
    public :: set_meta_array_core3_int
    public :: set_meta_array_core4_int
contains
    subroutine set_meta_array_core1_int(w, name, x)
        type(meta_array1_int), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        integer, dimension(:), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core1_int
    subroutine set_meta_array_core2_int(w, name, x)
        type(meta_array2_int), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        integer, dimension(:, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core2_int
    subroutine set_meta_array_core3_int(w, name, x)
        type(meta_array3_int), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        integer, dimension(:, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core3_int
    subroutine set_meta_array_core4_int(w, name, x)
        type(meta_array4_int), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        integer, dimension(:, :, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core4_int
    function get_meta_array_core1_int(w, name) result(x)
        type(meta_array1_int), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        integer, allocatable, dimension(:) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core1_int
    function get_meta_array_core2_int(w, name) result(x)
        type(meta_array2_int), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        integer, allocatable, dimension(:, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core2_int
    function get_meta_array_core3_int(w, name) result(x)
        type(meta_array3_int), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        integer, allocatable, dimension(:, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core3_int
    function get_meta_array_core4_int(w, name) result(x)
        type(meta_array4_int), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        integer, allocatable, dimension(:, :, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core4_int
    function get_meta_array1_int(w, name) result(x)
        type(meta_array1_int), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array1_int) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array1_int
    function get_meta_array2_int(w, name) result(x)
        type(meta_array2_int), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array2_int) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array2_int
    function get_meta_array3_int(w, name) result(x)
        type(meta_array3_int), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array3_int) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array3_int
    function get_meta_array4_int(w, name) result(x)
        type(meta_array4_int), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array4_int) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array4_int
    !
    !> Initialize meta array
    !
    subroutine initialize_meta_array1_(this, n1, name, id, value)
        class(meta_array1_int), intent(inout) :: this
        integer, intent(in) :: n1
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        integer, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        allocate(this%array(1:this%n1))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array1_
    subroutine initialize_meta_array2_(this, n1, n2, name, id, value)
        class(meta_array2_int), intent(inout) :: this
        integer, intent(in) :: n1, n2
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        integer, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        allocate(this%array(1:this%n1, 1:this%n2))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array2_
    subroutine initialize_meta_array3_(this, n1, n2, n3, name, id, value)
        class(meta_array3_int), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        integer, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array3_
    subroutine initialize_meta_array4_(this, n1, n2, n3, n4, name, id, value)
        class(meta_array4_int), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3, n4
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        integer, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        this%n4 = n4
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3, 1:this%n4))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array4_
    !
    !> Copy (=) meta array
    !
    subroutine copy_meta_array1_(this, from)
        class(meta_array1_int), intent(inout) :: this
        type(meta_array1_int), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1)))
        this%array = from%array
    end subroutine copy_meta_array1_
    subroutine copy_meta_array2_(this, from)
        class(meta_array2_int), intent(inout) :: this
        type(meta_array2_int), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2)))
        this%array = from%array
    end subroutine copy_meta_array2_
    subroutine copy_meta_array3_(this, from)
        class(meta_array3_int), intent(inout) :: this
        type(meta_array3_int), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3)))
        this%array = from%array
    end subroutine copy_meta_array3_
    subroutine copy_meta_array4_(this, from)
        class(meta_array4_int), intent(inout) :: this
        type(meta_array4_int), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        this%n4 = from%n4
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3), &
            lbound(from%array, 4):ubound(from%array, 4)))
        this%array = from%array
    end subroutine copy_meta_array4_
    !
    !> Initialize a list of meta array structure
    !
    subroutine initialize_list_meta_array1_(this, n, source)
        class(list_meta_array1_int), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array1_int), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array1_
    subroutine initialize_list_meta_array2_(this, n, source)
        class(list_meta_array2_int), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array2_int), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array2_
    subroutine initialize_list_meta_array3_(this, n, source)
        class(list_meta_array3_int), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array3_int), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array3_
    subroutine initialize_list_meta_array4_(this, n, source)
        class(list_meta_array4_int), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array4_int), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_int), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array1_int) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array1_
    function get_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_int), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array2_int) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array2_
    function get_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_int), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array3_int) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array3_
    function get_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_int), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array4_int) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array4_
    !
    !> Probe the shape of a meta array
    !
    function size_of_list_meta_array1_(this, name, dim) result(n)
        class(list_meta_array1_int), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array1_
    function size_of_list_meta_array2_(this, name, dim) result(n)
        class(list_meta_array2_int), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array2_
    function size_of_list_meta_array3_(this, name, dim) result(n)
        class(list_meta_array3_int), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array3_
    function size_of_list_meta_array4_(this, name, dim) result(n)
        class(list_meta_array4_int), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array4_
    !
    !> Probe if has a meta array with a specific name
    !
    function probe_existence_list_meta_array1_(this, name) result(n)
        class(list_meta_array1_int), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array1_
    function probe_existence_list_meta_array2_(this, name) result(n)
        class(list_meta_array2_int), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array2_
    function probe_existence_list_meta_array3_(this, name) result(n)
        class(list_meta_array3_int), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array3_
    function probe_existence_list_meta_array4_(this, name) result(n)
        class(list_meta_array4_int), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_core_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_int), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, allocatable, dimension(:) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array1_
    function get_core_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_int), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, allocatable, dimension(:, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array2_
    function get_core_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_int), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, allocatable, dimension(:, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array3_
    function get_core_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_int), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, allocatable, dimension(:, :, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3), &
                    lbound(this%meta_array(i)%array, 4):ubound(this%meta_array(i)%array, 4)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array4_
end module libflit_meta_array_int
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
module libflit_meta_array_real
    use libflit_array
    use libflit_error
    use libflit_string
    implicit none
    type meta_array1_real
        character(len=24) :: name
        integer :: id
        integer :: n1
        real, allocatable, dimension(:) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array1_
        procedure, private :: copy_meta_array1_
        procedure, public :: init => initialize_meta_array1_
    end type meta_array1_real
    type meta_array2_real
        character(len=24) :: name
        integer :: id
        integer :: n1, n2
        real, allocatable, dimension(:, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array2_
        procedure, private :: copy_meta_array2_
        procedure, public :: init => initialize_meta_array2_
    end type meta_array2_real
    type meta_array3_real
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3
        real, allocatable, dimension(:, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array3_
        procedure, private :: copy_meta_array3_
        procedure, public :: init => initialize_meta_array3_
    end type meta_array3_real
    type meta_array4_real
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3, n4
        real, allocatable, dimension(:, :, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array4_
        procedure, private :: copy_meta_array4_
        procedure, public :: init => initialize_meta_array4_
    end type meta_array4_real
    type list_meta_array1_real
        integer :: n
        type(meta_array1_real), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array1_
        procedure, public :: init => initialize_list_meta_array1_
        procedure, public :: size => size_of_list_meta_array1_
        procedure, public :: has => probe_existence_list_meta_array1_
        procedure, public :: get_core => get_core_from_list_meta_array1_
    end type list_meta_array1_real
    type list_meta_array2_real
        integer :: n
        type(meta_array2_real), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array2_
        procedure, public :: init => initialize_list_meta_array2_
        procedure, public :: size => size_of_list_meta_array2_
        procedure, public :: has => probe_existence_list_meta_array2_
        procedure, public :: get_core => get_core_from_list_meta_array2_
    end type list_meta_array2_real
    type list_meta_array3_real
        integer :: n
        type(meta_array3_real), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array3_
        procedure, public :: init => initialize_list_meta_array3_
        procedure, public :: size => size_of_list_meta_array3_
        procedure, public :: has => probe_existence_list_meta_array3_
        procedure, public :: get_core => get_core_from_list_meta_array3_
    end type list_meta_array3_real
    type list_meta_array4_real
        integer :: n
        type(meta_array4_real), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array4_
        procedure, public :: init => initialize_list_meta_array4_
        procedure, public :: size => size_of_list_meta_array4_
        procedure, public :: has => probe_existence_list_meta_array4_
        procedure, public :: get_core => get_core_from_list_meta_array4_
    end type list_meta_array4_real
    private
    public :: meta_array1_real
    public :: meta_array2_real
    public :: meta_array3_real
    public :: meta_array4_real
    public :: list_meta_array1_real
    public :: list_meta_array2_real
    public :: list_meta_array3_real
    public :: list_meta_array4_real
    public :: get_meta_array1_real
    public :: get_meta_array2_real
    public :: get_meta_array3_real
    public :: get_meta_array4_real
    public :: get_meta_array_core1_real
    public :: get_meta_array_core2_real
    public :: get_meta_array_core3_real
    public :: get_meta_array_core4_real
    public :: set_meta_array_core1_real
    public :: set_meta_array_core2_real
    public :: set_meta_array_core3_real
    public :: set_meta_array_core4_real
contains
    subroutine set_meta_array_core1_real(w, name, x)
        type(meta_array1_real), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        real, dimension(:), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core1_real
    subroutine set_meta_array_core2_real(w, name, x)
        type(meta_array2_real), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        real, dimension(:, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core2_real
    subroutine set_meta_array_core3_real(w, name, x)
        type(meta_array3_real), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        real, dimension(:, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core3_real
    subroutine set_meta_array_core4_real(w, name, x)
        type(meta_array4_real), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        real, dimension(:, :, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core4_real
    function get_meta_array_core1_real(w, name) result(x)
        type(meta_array1_real), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        real, allocatable, dimension(:) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core1_real
    function get_meta_array_core2_real(w, name) result(x)
        type(meta_array2_real), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        real, allocatable, dimension(:, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core2_real
    function get_meta_array_core3_real(w, name) result(x)
        type(meta_array3_real), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        real, allocatable, dimension(:, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core3_real
    function get_meta_array_core4_real(w, name) result(x)
        type(meta_array4_real), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        real, allocatable, dimension(:, :, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core4_real
    function get_meta_array1_real(w, name) result(x)
        type(meta_array1_real), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array1_real) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array1_real
    function get_meta_array2_real(w, name) result(x)
        type(meta_array2_real), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array2_real) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array2_real
    function get_meta_array3_real(w, name) result(x)
        type(meta_array3_real), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array3_real) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array3_real
    function get_meta_array4_real(w, name) result(x)
        type(meta_array4_real), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array4_real) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array4_real
    !
    !> Initialize meta array
    !
    subroutine initialize_meta_array1_(this, n1, name, id, value)
        class(meta_array1_real), intent(inout) :: this
        integer, intent(in) :: n1
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        real, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        allocate(this%array(1:this%n1))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array1_
    subroutine initialize_meta_array2_(this, n1, n2, name, id, value)
        class(meta_array2_real), intent(inout) :: this
        integer, intent(in) :: n1, n2
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        real, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        allocate(this%array(1:this%n1, 1:this%n2))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array2_
    subroutine initialize_meta_array3_(this, n1, n2, n3, name, id, value)
        class(meta_array3_real), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        real, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array3_
    subroutine initialize_meta_array4_(this, n1, n2, n3, n4, name, id, value)
        class(meta_array4_real), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3, n4
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        real, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        this%n4 = n4
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3, 1:this%n4))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array4_
    !
    !> Copy (=) meta array
    !
    subroutine copy_meta_array1_(this, from)
        class(meta_array1_real), intent(inout) :: this
        type(meta_array1_real), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1)))
        this%array = from%array
    end subroutine copy_meta_array1_
    subroutine copy_meta_array2_(this, from)
        class(meta_array2_real), intent(inout) :: this
        type(meta_array2_real), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2)))
        this%array = from%array
    end subroutine copy_meta_array2_
    subroutine copy_meta_array3_(this, from)
        class(meta_array3_real), intent(inout) :: this
        type(meta_array3_real), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3)))
        this%array = from%array
    end subroutine copy_meta_array3_
    subroutine copy_meta_array4_(this, from)
        class(meta_array4_real), intent(inout) :: this
        type(meta_array4_real), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        this%n4 = from%n4
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3), &
            lbound(from%array, 4):ubound(from%array, 4)))
        this%array = from%array
    end subroutine copy_meta_array4_
    !
    !> Initialize a list of meta array structure
    !
    subroutine initialize_list_meta_array1_(this, n, source)
        class(list_meta_array1_real), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array1_real), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array1_
    subroutine initialize_list_meta_array2_(this, n, source)
        class(list_meta_array2_real), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array2_real), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array2_
    subroutine initialize_list_meta_array3_(this, n, source)
        class(list_meta_array3_real), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array3_real), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array3_
    subroutine initialize_list_meta_array4_(this, n, source)
        class(list_meta_array4_real), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array4_real), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_real), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array1_real) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array1_
    function get_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_real), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array2_real) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array2_
    function get_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_real), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array3_real) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array3_
    function get_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_real), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array4_real) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array4_
    !
    !> Probe the shape of a meta array
    !
    function size_of_list_meta_array1_(this, name, dim) result(n)
        class(list_meta_array1_real), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array1_
    function size_of_list_meta_array2_(this, name, dim) result(n)
        class(list_meta_array2_real), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array2_
    function size_of_list_meta_array3_(this, name, dim) result(n)
        class(list_meta_array3_real), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array3_
    function size_of_list_meta_array4_(this, name, dim) result(n)
        class(list_meta_array4_real), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array4_
    !
    !> Probe if has a meta array with a specific name
    !
    function probe_existence_list_meta_array1_(this, name) result(n)
        class(list_meta_array1_real), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array1_
    function probe_existence_list_meta_array2_(this, name) result(n)
        class(list_meta_array2_real), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array2_
    function probe_existence_list_meta_array3_(this, name) result(n)
        class(list_meta_array3_real), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array3_
    function probe_existence_list_meta_array4_(this, name) result(n)
        class(list_meta_array4_real), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_core_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_real), intent(in) :: this
        character(len=*), intent(in) :: name
        real, allocatable, dimension(:) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array1_
    function get_core_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_real), intent(in) :: this
        character(len=*), intent(in) :: name
        real, allocatable, dimension(:, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array2_
    function get_core_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_real), intent(in) :: this
        character(len=*), intent(in) :: name
        real, allocatable, dimension(:, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array3_
    function get_core_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_real), intent(in) :: this
        character(len=*), intent(in) :: name
        real, allocatable, dimension(:, :, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3), &
                    lbound(this%meta_array(i)%array, 4):ubound(this%meta_array(i)%array, 4)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array4_
end module libflit_meta_array_real
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
module libflit_meta_array_double
    use libflit_array
    use libflit_error
    use libflit_string
    implicit none
    type meta_array1_double
        character(len=24) :: name
        integer :: id
        integer :: n1
        double precision, allocatable, dimension(:) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array1_
        procedure, private :: copy_meta_array1_
        procedure, public :: init => initialize_meta_array1_
    end type meta_array1_double
    type meta_array2_double
        character(len=24) :: name
        integer :: id
        integer :: n1, n2
        double precision, allocatable, dimension(:, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array2_
        procedure, private :: copy_meta_array2_
        procedure, public :: init => initialize_meta_array2_
    end type meta_array2_double
    type meta_array3_double
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3
        double precision, allocatable, dimension(:, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array3_
        procedure, private :: copy_meta_array3_
        procedure, public :: init => initialize_meta_array3_
    end type meta_array3_double
    type meta_array4_double
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3, n4
        double precision, allocatable, dimension(:, :, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array4_
        procedure, private :: copy_meta_array4_
        procedure, public :: init => initialize_meta_array4_
    end type meta_array4_double
    type list_meta_array1_double
        integer :: n
        type(meta_array1_double), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array1_
        procedure, public :: init => initialize_list_meta_array1_
        procedure, public :: size => size_of_list_meta_array1_
        procedure, public :: has => probe_existence_list_meta_array1_
        procedure, public :: get_core => get_core_from_list_meta_array1_
    end type list_meta_array1_double
    type list_meta_array2_double
        integer :: n
        type(meta_array2_double), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array2_
        procedure, public :: init => initialize_list_meta_array2_
        procedure, public :: size => size_of_list_meta_array2_
        procedure, public :: has => probe_existence_list_meta_array2_
        procedure, public :: get_core => get_core_from_list_meta_array2_
    end type list_meta_array2_double
    type list_meta_array3_double
        integer :: n
        type(meta_array3_double), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array3_
        procedure, public :: init => initialize_list_meta_array3_
        procedure, public :: size => size_of_list_meta_array3_
        procedure, public :: has => probe_existence_list_meta_array3_
        procedure, public :: get_core => get_core_from_list_meta_array3_
    end type list_meta_array3_double
    type list_meta_array4_double
        integer :: n
        type(meta_array4_double), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array4_
        procedure, public :: init => initialize_list_meta_array4_
        procedure, public :: size => size_of_list_meta_array4_
        procedure, public :: has => probe_existence_list_meta_array4_
        procedure, public :: get_core => get_core_from_list_meta_array4_
    end type list_meta_array4_double
    private
    public :: meta_array1_double
    public :: meta_array2_double
    public :: meta_array3_double
    public :: meta_array4_double
    public :: list_meta_array1_double
    public :: list_meta_array2_double
    public :: list_meta_array3_double
    public :: list_meta_array4_double
    public :: get_meta_array1_double
    public :: get_meta_array2_double
    public :: get_meta_array3_double
    public :: get_meta_array4_double
    public :: get_meta_array_core1_double
    public :: get_meta_array_core2_double
    public :: get_meta_array_core3_double
    public :: get_meta_array_core4_double
    public :: set_meta_array_core1_double
    public :: set_meta_array_core2_double
    public :: set_meta_array_core3_double
    public :: set_meta_array_core4_double
contains
    subroutine set_meta_array_core1_double(w, name, x)
        type(meta_array1_double), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        double precision, dimension(:), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core1_double
    subroutine set_meta_array_core2_double(w, name, x)
        type(meta_array2_double), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        double precision, dimension(:, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core2_double
    subroutine set_meta_array_core3_double(w, name, x)
        type(meta_array3_double), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        double precision, dimension(:, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core3_double
    subroutine set_meta_array_core4_double(w, name, x)
        type(meta_array4_double), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        double precision, dimension(:, :, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core4_double
    function get_meta_array_core1_double(w, name) result(x)
        type(meta_array1_double), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        double precision, allocatable, dimension(:) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core1_double
    function get_meta_array_core2_double(w, name) result(x)
        type(meta_array2_double), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        double precision, allocatable, dimension(:, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core2_double
    function get_meta_array_core3_double(w, name) result(x)
        type(meta_array3_double), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        double precision, allocatable, dimension(:, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core3_double
    function get_meta_array_core4_double(w, name) result(x)
        type(meta_array4_double), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        double precision, allocatable, dimension(:, :, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core4_double
    function get_meta_array1_double(w, name) result(x)
        type(meta_array1_double), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array1_double) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array1_double
    function get_meta_array2_double(w, name) result(x)
        type(meta_array2_double), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array2_double) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array2_double
    function get_meta_array3_double(w, name) result(x)
        type(meta_array3_double), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array3_double) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array3_double
    function get_meta_array4_double(w, name) result(x)
        type(meta_array4_double), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array4_double) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array4_double
    !
    !> Initialize meta array
    !
    subroutine initialize_meta_array1_(this, n1, name, id, value)
        class(meta_array1_double), intent(inout) :: this
        integer, intent(in) :: n1
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        double precision, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        allocate(this%array(1:this%n1))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array1_
    subroutine initialize_meta_array2_(this, n1, n2, name, id, value)
        class(meta_array2_double), intent(inout) :: this
        integer, intent(in) :: n1, n2
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        double precision, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        allocate(this%array(1:this%n1, 1:this%n2))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array2_
    subroutine initialize_meta_array3_(this, n1, n2, n3, name, id, value)
        class(meta_array3_double), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        double precision, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array3_
    subroutine initialize_meta_array4_(this, n1, n2, n3, n4, name, id, value)
        class(meta_array4_double), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3, n4
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        double precision, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        this%n4 = n4
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3, 1:this%n4))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array4_
    !
    !> Copy (=) meta array
    !
    subroutine copy_meta_array1_(this, from)
        class(meta_array1_double), intent(inout) :: this
        type(meta_array1_double), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1)))
        this%array = from%array
    end subroutine copy_meta_array1_
    subroutine copy_meta_array2_(this, from)
        class(meta_array2_double), intent(inout) :: this
        type(meta_array2_double), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2)))
        this%array = from%array
    end subroutine copy_meta_array2_
    subroutine copy_meta_array3_(this, from)
        class(meta_array3_double), intent(inout) :: this
        type(meta_array3_double), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3)))
        this%array = from%array
    end subroutine copy_meta_array3_
    subroutine copy_meta_array4_(this, from)
        class(meta_array4_double), intent(inout) :: this
        type(meta_array4_double), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        this%n4 = from%n4
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3), &
            lbound(from%array, 4):ubound(from%array, 4)))
        this%array = from%array
    end subroutine copy_meta_array4_
    !
    !> Initialize a list of meta array structure
    !
    subroutine initialize_list_meta_array1_(this, n, source)
        class(list_meta_array1_double), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array1_double), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array1_
    subroutine initialize_list_meta_array2_(this, n, source)
        class(list_meta_array2_double), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array2_double), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array2_
    subroutine initialize_list_meta_array3_(this, n, source)
        class(list_meta_array3_double), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array3_double), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array3_
    subroutine initialize_list_meta_array4_(this, n, source)
        class(list_meta_array4_double), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array4_double), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_double), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array1_double) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array1_
    function get_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_double), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array2_double) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array2_
    function get_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_double), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array3_double) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array3_
    function get_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_double), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array4_double) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array4_
    !
    !> Probe the shape of a meta array
    !
    function size_of_list_meta_array1_(this, name, dim) result(n)
        class(list_meta_array1_double), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array1_
    function size_of_list_meta_array2_(this, name, dim) result(n)
        class(list_meta_array2_double), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array2_
    function size_of_list_meta_array3_(this, name, dim) result(n)
        class(list_meta_array3_double), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array3_
    function size_of_list_meta_array4_(this, name, dim) result(n)
        class(list_meta_array4_double), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array4_
    !
    !> Probe if has a meta array with a specific name
    !
    function probe_existence_list_meta_array1_(this, name) result(n)
        class(list_meta_array1_double), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array1_
    function probe_existence_list_meta_array2_(this, name) result(n)
        class(list_meta_array2_double), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array2_
    function probe_existence_list_meta_array3_(this, name) result(n)
        class(list_meta_array3_double), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array3_
    function probe_existence_list_meta_array4_(this, name) result(n)
        class(list_meta_array4_double), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_core_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_double), intent(in) :: this
        character(len=*), intent(in) :: name
        double precision, allocatable, dimension(:) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array1_
    function get_core_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_double), intent(in) :: this
        character(len=*), intent(in) :: name
        double precision, allocatable, dimension(:, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array2_
    function get_core_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_double), intent(in) :: this
        character(len=*), intent(in) :: name
        double precision, allocatable, dimension(:, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array3_
    function get_core_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_double), intent(in) :: this
        character(len=*), intent(in) :: name
        double precision, allocatable, dimension(:, :, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3), &
                    lbound(this%meta_array(i)%array, 4):ubound(this%meta_array(i)%array, 4)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array4_
end module libflit_meta_array_double
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
module libflit_meta_array_complex
    use libflit_array
    use libflit_error
    use libflit_string
    implicit none
    type meta_array1_complex
        character(len=24) :: name
        integer :: id
        integer :: n1
        complex, allocatable, dimension(:) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array1_
        procedure, private :: copy_meta_array1_
        procedure, public :: init => initialize_meta_array1_
    end type meta_array1_complex
    type meta_array2_complex
        character(len=24) :: name
        integer :: id
        integer :: n1, n2
        complex, allocatable, dimension(:, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array2_
        procedure, private :: copy_meta_array2_
        procedure, public :: init => initialize_meta_array2_
    end type meta_array2_complex
    type meta_array3_complex
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3
        complex, allocatable, dimension(:, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array3_
        procedure, private :: copy_meta_array3_
        procedure, public :: init => initialize_meta_array3_
    end type meta_array3_complex
    type meta_array4_complex
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3, n4
        complex, allocatable, dimension(:, :, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array4_
        procedure, private :: copy_meta_array4_
        procedure, public :: init => initialize_meta_array4_
    end type meta_array4_complex
    type list_meta_array1_complex
        integer :: n
        type(meta_array1_complex), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array1_
        procedure, public :: init => initialize_list_meta_array1_
        procedure, public :: size => size_of_list_meta_array1_
        procedure, public :: has => probe_existence_list_meta_array1_
        procedure, public :: get_core => get_core_from_list_meta_array1_
    end type list_meta_array1_complex
    type list_meta_array2_complex
        integer :: n
        type(meta_array2_complex), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array2_
        procedure, public :: init => initialize_list_meta_array2_
        procedure, public :: size => size_of_list_meta_array2_
        procedure, public :: has => probe_existence_list_meta_array2_
        procedure, public :: get_core => get_core_from_list_meta_array2_
    end type list_meta_array2_complex
    type list_meta_array3_complex
        integer :: n
        type(meta_array3_complex), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array3_
        procedure, public :: init => initialize_list_meta_array3_
        procedure, public :: size => size_of_list_meta_array3_
        procedure, public :: has => probe_existence_list_meta_array3_
        procedure, public :: get_core => get_core_from_list_meta_array3_
    end type list_meta_array3_complex
    type list_meta_array4_complex
        integer :: n
        type(meta_array4_complex), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array4_
        procedure, public :: init => initialize_list_meta_array4_
        procedure, public :: size => size_of_list_meta_array4_
        procedure, public :: has => probe_existence_list_meta_array4_
        procedure, public :: get_core => get_core_from_list_meta_array4_
    end type list_meta_array4_complex
    private
    public :: meta_array1_complex
    public :: meta_array2_complex
    public :: meta_array3_complex
    public :: meta_array4_complex
    public :: list_meta_array1_complex
    public :: list_meta_array2_complex
    public :: list_meta_array3_complex
    public :: list_meta_array4_complex
    public :: get_meta_array1_complex
    public :: get_meta_array2_complex
    public :: get_meta_array3_complex
    public :: get_meta_array4_complex
    public :: get_meta_array_core1_complex
    public :: get_meta_array_core2_complex
    public :: get_meta_array_core3_complex
    public :: get_meta_array_core4_complex
    public :: set_meta_array_core1_complex
    public :: set_meta_array_core2_complex
    public :: set_meta_array_core3_complex
    public :: set_meta_array_core4_complex
contains
    subroutine set_meta_array_core1_complex(w, name, x)
        type(meta_array1_complex), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        complex, dimension(:), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core1_complex
    subroutine set_meta_array_core2_complex(w, name, x)
        type(meta_array2_complex), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        complex, dimension(:, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core2_complex
    subroutine set_meta_array_core3_complex(w, name, x)
        type(meta_array3_complex), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        complex, dimension(:, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core3_complex
    subroutine set_meta_array_core4_complex(w, name, x)
        type(meta_array4_complex), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        complex, dimension(:, :, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core4_complex
    function get_meta_array_core1_complex(w, name) result(x)
        type(meta_array1_complex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        complex, allocatable, dimension(:) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core1_complex
    function get_meta_array_core2_complex(w, name) result(x)
        type(meta_array2_complex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        complex, allocatable, dimension(:, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core2_complex
    function get_meta_array_core3_complex(w, name) result(x)
        type(meta_array3_complex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        complex, allocatable, dimension(:, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core3_complex
    function get_meta_array_core4_complex(w, name) result(x)
        type(meta_array4_complex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        complex, allocatable, dimension(:, :, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core4_complex
    function get_meta_array1_complex(w, name) result(x)
        type(meta_array1_complex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array1_complex) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array1_complex
    function get_meta_array2_complex(w, name) result(x)
        type(meta_array2_complex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array2_complex) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array2_complex
    function get_meta_array3_complex(w, name) result(x)
        type(meta_array3_complex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array3_complex) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array3_complex
    function get_meta_array4_complex(w, name) result(x)
        type(meta_array4_complex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array4_complex) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array4_complex
    !
    !> Initialize meta array
    !
    subroutine initialize_meta_array1_(this, n1, name, id, value)
        class(meta_array1_complex), intent(inout) :: this
        integer, intent(in) :: n1
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        complex, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        allocate(this%array(1:this%n1))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array1_
    subroutine initialize_meta_array2_(this, n1, n2, name, id, value)
        class(meta_array2_complex), intent(inout) :: this
        integer, intent(in) :: n1, n2
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        complex, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        allocate(this%array(1:this%n1, 1:this%n2))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array2_
    subroutine initialize_meta_array3_(this, n1, n2, n3, name, id, value)
        class(meta_array3_complex), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        complex, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array3_
    subroutine initialize_meta_array4_(this, n1, n2, n3, n4, name, id, value)
        class(meta_array4_complex), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3, n4
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        complex, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        this%n4 = n4
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3, 1:this%n4))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array4_
    !
    !> Copy (=) meta array
    !
    subroutine copy_meta_array1_(this, from)
        class(meta_array1_complex), intent(inout) :: this
        type(meta_array1_complex), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1)))
        this%array = from%array
    end subroutine copy_meta_array1_
    subroutine copy_meta_array2_(this, from)
        class(meta_array2_complex), intent(inout) :: this
        type(meta_array2_complex), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2)))
        this%array = from%array
    end subroutine copy_meta_array2_
    subroutine copy_meta_array3_(this, from)
        class(meta_array3_complex), intent(inout) :: this
        type(meta_array3_complex), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3)))
        this%array = from%array
    end subroutine copy_meta_array3_
    subroutine copy_meta_array4_(this, from)
        class(meta_array4_complex), intent(inout) :: this
        type(meta_array4_complex), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        this%n4 = from%n4
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3), &
            lbound(from%array, 4):ubound(from%array, 4)))
        this%array = from%array
    end subroutine copy_meta_array4_
    !
    !> Initialize a list of meta array structure
    !
    subroutine initialize_list_meta_array1_(this, n, source)
        class(list_meta_array1_complex), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array1_complex), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array1_
    subroutine initialize_list_meta_array2_(this, n, source)
        class(list_meta_array2_complex), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array2_complex), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array2_
    subroutine initialize_list_meta_array3_(this, n, source)
        class(list_meta_array3_complex), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array3_complex), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array3_
    subroutine initialize_list_meta_array4_(this, n, source)
        class(list_meta_array4_complex), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array4_complex), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array1_complex) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array1_
    function get_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array2_complex) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array2_
    function get_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array3_complex) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array3_
    function get_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array4_complex) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array4_
    !
    !> Probe the shape of a meta array
    !
    function size_of_list_meta_array1_(this, name, dim) result(n)
        class(list_meta_array1_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array1_
    function size_of_list_meta_array2_(this, name, dim) result(n)
        class(list_meta_array2_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array2_
    function size_of_list_meta_array3_(this, name, dim) result(n)
        class(list_meta_array3_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array3_
    function size_of_list_meta_array4_(this, name, dim) result(n)
        class(list_meta_array4_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array4_
    !
    !> Probe if has a meta array with a specific name
    !
    function probe_existence_list_meta_array1_(this, name) result(n)
        class(list_meta_array1_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array1_
    function probe_existence_list_meta_array2_(this, name) result(n)
        class(list_meta_array2_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array2_
    function probe_existence_list_meta_array3_(this, name) result(n)
        class(list_meta_array3_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array3_
    function probe_existence_list_meta_array4_(this, name) result(n)
        class(list_meta_array4_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_core_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        complex, allocatable, dimension(:) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array1_
    function get_core_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        complex, allocatable, dimension(:, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array2_
    function get_core_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        complex, allocatable, dimension(:, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array3_
    function get_core_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_complex), intent(in) :: this
        character(len=*), intent(in) :: name
        complex, allocatable, dimension(:, :, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3), &
                    lbound(this%meta_array(i)%array, 4):ubound(this%meta_array(i)%array, 4)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array4_
end module libflit_meta_array_complex
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
module libflit_meta_array_dcomplex
    use libflit_array
    use libflit_error
    use libflit_string
    implicit none
    type meta_array1_dcomplex
        character(len=24) :: name
        integer :: id
        integer :: n1
        double complex, allocatable, dimension(:) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array1_
        procedure, private :: copy_meta_array1_
        procedure, public :: init => initialize_meta_array1_
    end type meta_array1_dcomplex
    type meta_array2_dcomplex
        character(len=24) :: name
        integer :: id
        integer :: n1, n2
        double complex, allocatable, dimension(:, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array2_
        procedure, private :: copy_meta_array2_
        procedure, public :: init => initialize_meta_array2_
    end type meta_array2_dcomplex
    type meta_array3_dcomplex
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3
        double complex, allocatable, dimension(:, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array3_
        procedure, private :: copy_meta_array3_
        procedure, public :: init => initialize_meta_array3_
    end type meta_array3_dcomplex
    type meta_array4_dcomplex
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3, n4
        double complex, allocatable, dimension(:, :, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array4_
        procedure, private :: copy_meta_array4_
        procedure, public :: init => initialize_meta_array4_
    end type meta_array4_dcomplex
    type list_meta_array1_dcomplex
        integer :: n
        type(meta_array1_dcomplex), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array1_
        procedure, public :: init => initialize_list_meta_array1_
        procedure, public :: size => size_of_list_meta_array1_
        procedure, public :: has => probe_existence_list_meta_array1_
        procedure, public :: get_core => get_core_from_list_meta_array1_
    end type list_meta_array1_dcomplex
    type list_meta_array2_dcomplex
        integer :: n
        type(meta_array2_dcomplex), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array2_
        procedure, public :: init => initialize_list_meta_array2_
        procedure, public :: size => size_of_list_meta_array2_
        procedure, public :: has => probe_existence_list_meta_array2_
        procedure, public :: get_core => get_core_from_list_meta_array2_
    end type list_meta_array2_dcomplex
    type list_meta_array3_dcomplex
        integer :: n
        type(meta_array3_dcomplex), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array3_
        procedure, public :: init => initialize_list_meta_array3_
        procedure, public :: size => size_of_list_meta_array3_
        procedure, public :: has => probe_existence_list_meta_array3_
        procedure, public :: get_core => get_core_from_list_meta_array3_
    end type list_meta_array3_dcomplex
    type list_meta_array4_dcomplex
        integer :: n
        type(meta_array4_dcomplex), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array4_
        procedure, public :: init => initialize_list_meta_array4_
        procedure, public :: size => size_of_list_meta_array4_
        procedure, public :: has => probe_existence_list_meta_array4_
        procedure, public :: get_core => get_core_from_list_meta_array4_
    end type list_meta_array4_dcomplex
    private
    public :: meta_array1_dcomplex
    public :: meta_array2_dcomplex
    public :: meta_array3_dcomplex
    public :: meta_array4_dcomplex
    public :: list_meta_array1_dcomplex
    public :: list_meta_array2_dcomplex
    public :: list_meta_array3_dcomplex
    public :: list_meta_array4_dcomplex
    public :: get_meta_array1_dcomplex
    public :: get_meta_array2_dcomplex
    public :: get_meta_array3_dcomplex
    public :: get_meta_array4_dcomplex
    public :: get_meta_array_core1_dcomplex
    public :: get_meta_array_core2_dcomplex
    public :: get_meta_array_core3_dcomplex
    public :: get_meta_array_core4_dcomplex
    public :: set_meta_array_core1_dcomplex
    public :: set_meta_array_core2_dcomplex
    public :: set_meta_array_core3_dcomplex
    public :: set_meta_array_core4_dcomplex
contains
    subroutine set_meta_array_core1_dcomplex(w, name, x)
        type(meta_array1_dcomplex), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        double complex, dimension(:), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core1_dcomplex
    subroutine set_meta_array_core2_dcomplex(w, name, x)
        type(meta_array2_dcomplex), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        double complex, dimension(:, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core2_dcomplex
    subroutine set_meta_array_core3_dcomplex(w, name, x)
        type(meta_array3_dcomplex), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        double complex, dimension(:, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core3_dcomplex
    subroutine set_meta_array_core4_dcomplex(w, name, x)
        type(meta_array4_dcomplex), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        double complex, dimension(:, :, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core4_dcomplex
    function get_meta_array_core1_dcomplex(w, name) result(x)
        type(meta_array1_dcomplex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        double complex, allocatable, dimension(:) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core1_dcomplex
    function get_meta_array_core2_dcomplex(w, name) result(x)
        type(meta_array2_dcomplex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        double complex, allocatable, dimension(:, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core2_dcomplex
    function get_meta_array_core3_dcomplex(w, name) result(x)
        type(meta_array3_dcomplex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        double complex, allocatable, dimension(:, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core3_dcomplex
    function get_meta_array_core4_dcomplex(w, name) result(x)
        type(meta_array4_dcomplex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        double complex, allocatable, dimension(:, :, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core4_dcomplex
    function get_meta_array1_dcomplex(w, name) result(x)
        type(meta_array1_dcomplex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array1_dcomplex) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array1_dcomplex
    function get_meta_array2_dcomplex(w, name) result(x)
        type(meta_array2_dcomplex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array2_dcomplex) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array2_dcomplex
    function get_meta_array3_dcomplex(w, name) result(x)
        type(meta_array3_dcomplex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array3_dcomplex) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array3_dcomplex
    function get_meta_array4_dcomplex(w, name) result(x)
        type(meta_array4_dcomplex), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array4_dcomplex) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array4_dcomplex
    !
    !> Initialize meta array
    !
    subroutine initialize_meta_array1_(this, n1, name, id, value)
        class(meta_array1_dcomplex), intent(inout) :: this
        integer, intent(in) :: n1
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        double complex, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        allocate(this%array(1:this%n1))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array1_
    subroutine initialize_meta_array2_(this, n1, n2, name, id, value)
        class(meta_array2_dcomplex), intent(inout) :: this
        integer, intent(in) :: n1, n2
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        double complex, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        allocate(this%array(1:this%n1, 1:this%n2))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array2_
    subroutine initialize_meta_array3_(this, n1, n2, n3, name, id, value)
        class(meta_array3_dcomplex), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        double complex, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array3_
    subroutine initialize_meta_array4_(this, n1, n2, n3, n4, name, id, value)
        class(meta_array4_dcomplex), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3, n4
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        double complex, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        this%n4 = n4
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3, 1:this%n4))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array4_
    !
    !> Copy (=) meta array
    !
    subroutine copy_meta_array1_(this, from)
        class(meta_array1_dcomplex), intent(inout) :: this
        type(meta_array1_dcomplex), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1)))
        this%array = from%array
    end subroutine copy_meta_array1_
    subroutine copy_meta_array2_(this, from)
        class(meta_array2_dcomplex), intent(inout) :: this
        type(meta_array2_dcomplex), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2)))
        this%array = from%array
    end subroutine copy_meta_array2_
    subroutine copy_meta_array3_(this, from)
        class(meta_array3_dcomplex), intent(inout) :: this
        type(meta_array3_dcomplex), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3)))
        this%array = from%array
    end subroutine copy_meta_array3_
    subroutine copy_meta_array4_(this, from)
        class(meta_array4_dcomplex), intent(inout) :: this
        type(meta_array4_dcomplex), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        this%n4 = from%n4
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3), &
            lbound(from%array, 4):ubound(from%array, 4)))
        this%array = from%array
    end subroutine copy_meta_array4_
    !
    !> Initialize a list of meta array structure
    !
    subroutine initialize_list_meta_array1_(this, n, source)
        class(list_meta_array1_dcomplex), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array1_dcomplex), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array1_
    subroutine initialize_list_meta_array2_(this, n, source)
        class(list_meta_array2_dcomplex), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array2_dcomplex), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array2_
    subroutine initialize_list_meta_array3_(this, n, source)
        class(list_meta_array3_dcomplex), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array3_dcomplex), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array3_
    subroutine initialize_list_meta_array4_(this, n, source)
        class(list_meta_array4_dcomplex), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array4_dcomplex), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array1_dcomplex) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array1_
    function get_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array2_dcomplex) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array2_
    function get_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array3_dcomplex) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array3_
    function get_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array4_dcomplex) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array4_
    !
    !> Probe the shape of a meta array
    !
    function size_of_list_meta_array1_(this, name, dim) result(n)
        class(list_meta_array1_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array1_
    function size_of_list_meta_array2_(this, name, dim) result(n)
        class(list_meta_array2_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array2_
    function size_of_list_meta_array3_(this, name, dim) result(n)
        class(list_meta_array3_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array3_
    function size_of_list_meta_array4_(this, name, dim) result(n)
        class(list_meta_array4_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array4_
    !
    !> Probe if has a meta array with a specific name
    !
    function probe_existence_list_meta_array1_(this, name) result(n)
        class(list_meta_array1_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array1_
    function probe_existence_list_meta_array2_(this, name) result(n)
        class(list_meta_array2_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array2_
    function probe_existence_list_meta_array3_(this, name) result(n)
        class(list_meta_array3_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array3_
    function probe_existence_list_meta_array4_(this, name) result(n)
        class(list_meta_array4_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_core_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        double complex, allocatable, dimension(:) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array1_
    function get_core_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        double complex, allocatable, dimension(:, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array2_
    function get_core_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        double complex, allocatable, dimension(:, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array3_
    function get_core_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_dcomplex), intent(in) :: this
        character(len=*), intent(in) :: name
        double complex, allocatable, dimension(:, :, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3), &
                    lbound(this%meta_array(i)%array, 4):ubound(this%meta_array(i)%array, 4)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array4_
end module libflit_meta_array_dcomplex
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
module libflit_meta_array_logical
    use libflit_array
    use libflit_error
    use libflit_string
    implicit none
    type meta_array1_logical
        character(len=24) :: name
        integer :: id
        integer :: n1
        logical, allocatable, dimension(:) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array1_
        procedure, private :: copy_meta_array1_
        procedure, public :: init => initialize_meta_array1_
    end type meta_array1_logical
    type meta_array2_logical
        character(len=24) :: name
        integer :: id
        integer :: n1, n2
        logical, allocatable, dimension(:, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array2_
        procedure, private :: copy_meta_array2_
        procedure, public :: init => initialize_meta_array2_
    end type meta_array2_logical
    type meta_array3_logical
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3
        logical, allocatable, dimension(:, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array3_
        procedure, private :: copy_meta_array3_
        procedure, public :: init => initialize_meta_array3_
    end type meta_array3_logical
    type meta_array4_logical
        character(len=24) :: name
        integer :: id
        integer :: n1, n2, n3, n4
        logical, allocatable, dimension(:, :, :, :) :: array
    contains
        generic, public :: assignment(=) => copy_meta_array4_
        procedure, private :: copy_meta_array4_
        procedure, public :: init => initialize_meta_array4_
    end type meta_array4_logical
    type list_meta_array1_logical
        integer :: n
        type(meta_array1_logical), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array1_
        procedure, public :: init => initialize_list_meta_array1_
        procedure, public :: size => size_of_list_meta_array1_
        procedure, public :: has => probe_existence_list_meta_array1_
        procedure, public :: get_core => get_core_from_list_meta_array1_
    end type list_meta_array1_logical
    type list_meta_array2_logical
        integer :: n
        type(meta_array2_logical), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array2_
        procedure, public :: init => initialize_list_meta_array2_
        procedure, public :: size => size_of_list_meta_array2_
        procedure, public :: has => probe_existence_list_meta_array2_
        procedure, public :: get_core => get_core_from_list_meta_array2_
    end type list_meta_array2_logical
    type list_meta_array3_logical
        integer :: n
        type(meta_array3_logical), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array3_
        procedure, public :: init => initialize_list_meta_array3_
        procedure, public :: size => size_of_list_meta_array3_
        procedure, public :: has => probe_existence_list_meta_array3_
        procedure, public :: get_core => get_core_from_list_meta_array3_
    end type list_meta_array3_logical
    type list_meta_array4_logical
        integer :: n
        type(meta_array4_logical), allocatable, dimension(:) :: meta_array
    contains
        procedure, public :: get => get_from_list_meta_array4_
        procedure, public :: init => initialize_list_meta_array4_
        procedure, public :: size => size_of_list_meta_array4_
        procedure, public :: has => probe_existence_list_meta_array4_
        procedure, public :: get_core => get_core_from_list_meta_array4_
    end type list_meta_array4_logical
    private
    public :: meta_array1_logical
    public :: meta_array2_logical
    public :: meta_array3_logical
    public :: meta_array4_logical
    public :: list_meta_array1_logical
    public :: list_meta_array2_logical
    public :: list_meta_array3_logical
    public :: list_meta_array4_logical
    public :: get_meta_array1_logical
    public :: get_meta_array2_logical
    public :: get_meta_array3_logical
    public :: get_meta_array4_logical
    public :: get_meta_array_core1_logical
    public :: get_meta_array_core2_logical
    public :: get_meta_array_core3_logical
    public :: get_meta_array_core4_logical
    public :: set_meta_array_core1_logical
    public :: set_meta_array_core2_logical
    public :: set_meta_array_core3_logical
    public :: set_meta_array_core4_logical
contains
    subroutine set_meta_array_core1_logical(w, name, x)
        type(meta_array1_logical), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        logical, dimension(:), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core1_logical
    subroutine set_meta_array_core2_logical(w, name, x)
        type(meta_array2_logical), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        logical, dimension(:, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core2_logical
    subroutine set_meta_array_core3_logical(w, name, x)
        type(meta_array3_logical), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        logical, dimension(:, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core3_logical
    subroutine set_meta_array_core4_logical(w, name, x)
        type(meta_array4_logical), dimension(:), intent(inout) :: w
        character(len=*), intent(in) :: name
        logical, dimension(:, :, :, :), intent(in) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                w(i)%array = x
                exit
            end if
        end do
    end subroutine set_meta_array_core4_logical
    function get_meta_array_core1_logical(w, name) result(x)
        type(meta_array1_logical), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        logical, allocatable, dimension(:) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core1_logical
    function get_meta_array_core2_logical(w, name) result(x)
        type(meta_array2_logical), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        logical, allocatable, dimension(:, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core2_logical
    function get_meta_array_core3_logical(w, name) result(x)
        type(meta_array3_logical), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        logical, allocatable, dimension(:, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core3_logical
    function get_meta_array_core4_logical(w, name) result(x)
        type(meta_array4_logical), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        logical, allocatable, dimension(:, :, :, :) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)%array
                exit
            end if
        end do
    end function get_meta_array_core4_logical
    function get_meta_array1_logical(w, name) result(x)
        type(meta_array1_logical), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array1_logical) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array1_logical
    function get_meta_array2_logical(w, name) result(x)
        type(meta_array2_logical), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array2_logical) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array2_logical
    function get_meta_array3_logical(w, name) result(x)
        type(meta_array3_logical), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array3_logical) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array3_logical
    function get_meta_array4_logical(w, name) result(x)
        type(meta_array4_logical), dimension(:), intent(in) :: w
        character(len=*), intent(in) :: name
        type(meta_array4_logical) :: x
        integer :: i
        do i = 1, size(w)
            if (tidy(w(i)%name) == tidy(name)) then
                x = w(i)
                exit
            end if
        end do
    end function get_meta_array4_logical
    !
    !> Initialize meta array
    !
    subroutine initialize_meta_array1_(this, n1, name, id, value)
        class(meta_array1_logical), intent(inout) :: this
        integer, intent(in) :: n1
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        logical, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        allocate(this%array(1:this%n1))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array1_
    subroutine initialize_meta_array2_(this, n1, n2, name, id, value)
        class(meta_array2_logical), intent(inout) :: this
        integer, intent(in) :: n1, n2
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        logical, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        allocate(this%array(1:this%n1, 1:this%n2))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array2_
    subroutine initialize_meta_array3_(this, n1, n2, n3, name, id, value)
        class(meta_array3_logical), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        logical, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array3_
    subroutine initialize_meta_array4_(this, n1, n2, n3, n4, name, id, value)
        class(meta_array4_logical), intent(inout) :: this
        integer, intent(in) :: n1, n2, n3, n4
        character(len=*), intent(in), optional :: name
        integer, intent(in), optional :: id
        logical, intent(in), optional :: value
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        this%n1 = n1
        this%n2 = n2
        this%n3 = n3
        this%n4 = n4
        allocate(this%array(1:this%n1, 1:this%n2, 1:this%n3, 1:this%n4))
        if (present(name)) then
            this%name = tidy(name)
        else
            this%name = 'meta array'
        end if
        if (present(id)) then
            this%id = id
        else
            this%id = 0
        end if
        if (present(value)) then
            this%array = value
        else
            this%array = 0
        end if
    end subroutine initialize_meta_array4_
    !
    !> Copy (=) meta array
    !
    subroutine copy_meta_array1_(this, from)
        class(meta_array1_logical), intent(inout) :: this
        type(meta_array1_logical), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1)))
        this%array = from%array
    end subroutine copy_meta_array1_
    subroutine copy_meta_array2_(this, from)
        class(meta_array2_logical), intent(inout) :: this
        type(meta_array2_logical), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2)))
        this%array = from%array
    end subroutine copy_meta_array2_
    subroutine copy_meta_array3_(this, from)
        class(meta_array3_logical), intent(inout) :: this
        type(meta_array3_logical), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3)))
        this%array = from%array
    end subroutine copy_meta_array3_
    subroutine copy_meta_array4_(this, from)
        class(meta_array4_logical), intent(inout) :: this
        type(meta_array4_logical), intent(in) :: from
        this%name = from%name
        this%id = from%id
        this%n1 = from%n1
        this%n2 = from%n2
        this%n3 = from%n3
        this%n4 = from%n4
        if (allocated(this%array)) then
            deallocate(this%array)
        end if
        allocate(this%array(lbound(from%array, 1):ubound(from%array, 1), &
            lbound(from%array, 2):ubound(from%array, 2), &
            lbound(from%array, 3):ubound(from%array, 3), &
            lbound(from%array, 4):ubound(from%array, 4)))
        this%array = from%array
    end subroutine copy_meta_array4_
    !
    !> Initialize a list of meta array structure
    !
    subroutine initialize_list_meta_array1_(this, n, source)
        class(list_meta_array1_logical), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array1_logical), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array1_
    subroutine initialize_list_meta_array2_(this, n, source)
        class(list_meta_array2_logical), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array2_logical), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array2_
    subroutine initialize_list_meta_array3_(this, n, source)
        class(list_meta_array3_logical), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array3_logical), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array3_
    subroutine initialize_list_meta_array4_(this, n, source)
        class(list_meta_array4_logical), intent(inout) :: this
        integer, intent(in) :: n
        type(meta_array4_logical), intent(in), optional :: source
        integer :: i
        this%n = n
        if (allocated(this%meta_array)) then
            deallocate(this%meta_array)
        end if
        allocate(this%meta_array(1:this%n))
        if (present(source)) then
            do i = 1, this%n
                this%meta_array(i) = source
            end do
        end if
    end subroutine initialize_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array1_logical) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array1_
    function get_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array2_logical) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array2_
    function get_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array3_logical) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array3_
    function get_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        type(meta_array4_logical) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                m = this%meta_array(i)
                return
            end if
        end do
    end function get_from_list_meta_array4_
    !
    !> Probe the shape of a meta array
    !
    function size_of_list_meta_array1_(this, name, dim) result(n)
        class(list_meta_array1_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array1_
    function size_of_list_meta_array2_(this, name, dim) result(n)
        class(list_meta_array2_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array2_
    function size_of_list_meta_array3_(this, name, dim) result(n)
        class(list_meta_array3_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array3_
    function size_of_list_meta_array4_(this, name, dim) result(n)
        class(list_meta_array4_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: dim
        integer :: n
        integer :: i
        do i = 1, this%n
            if (this%meta_array(i)%name == name) then
                n = size(this%meta_array(i)%array, dim=dim)
                return
            end if
        end do
    end function size_of_list_meta_array4_
    !
    !> Probe if has a meta array with a specific name
    !
    function probe_existence_list_meta_array1_(this, name) result(n)
        class(list_meta_array1_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array1_
    function probe_existence_list_meta_array2_(this, name) result(n)
        class(list_meta_array2_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array2_
    function probe_existence_list_meta_array3_(this, name) result(n)
        class(list_meta_array3_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array3_
    function probe_existence_list_meta_array4_(this, name) result(n)
        class(list_meta_array4_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: n
        if (allocated(this%meta_array)) then
            n = any(this%meta_array(:)%name == name)
        else
            n = .false.
        end if
    end function probe_existence_list_meta_array4_
    !
    !> Get a meta array from a list of meta array structure
    !
    function get_core_from_list_meta_array1_(this, name) result(m)
        class(list_meta_array1_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        logical, allocatable, dimension(:) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array1_
    function get_core_from_list_meta_array2_(this, name) result(m)
        class(list_meta_array2_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        logical, allocatable, dimension(:, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array2_
    function get_core_from_list_meta_array3_(this, name) result(m)
        class(list_meta_array3_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        logical, allocatable, dimension(:, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array3_
    function get_core_from_list_meta_array4_(this, name) result(m)
        class(list_meta_array4_logical), intent(in) :: this
        character(len=*), intent(in) :: name
        logical, allocatable, dimension(:, :, :, :) :: m
        integer :: i
        do i = 1, this%n
            if (tidy(this%meta_array(i)%name) == tidy(name)) then
                allocate(m(lbound(this%meta_array(i)%array, 1):ubound(this%meta_array(i)%array, 1), &
                    lbound(this%meta_array(i)%array, 2):ubound(this%meta_array(i)%array, 2), &
                    lbound(this%meta_array(i)%array, 3):ubound(this%meta_array(i)%array, 3), &
                    lbound(this%meta_array(i)%array, 4):ubound(this%meta_array(i)%array, 4)))
                m = this%meta_array(i)%array
                return
            end if
        end do
    end function get_core_from_list_meta_array4_
end module libflit_meta_array_logical
module libflit_array_extension
    use libflit_meta_array_int
    use libflit_meta_array_real
    use libflit_meta_array_double
    use libflit_meta_array_complex
    use libflit_meta_array_dcomplex
    use libflit_meta_array_logical
    implicit none
    ! Get a meta-array from an array of meta-arrays by name; the return is a meta-array
    interface get_meta_array
        module procedure :: get_meta_array1_int
        module procedure :: get_meta_array1_real
        module procedure :: get_meta_array1_double
        module procedure :: get_meta_array1_complex
        module procedure :: get_meta_array1_dcomplex
        module procedure :: get_meta_array1_logical
        module procedure :: get_meta_array2_int
        module procedure :: get_meta_array2_real
        module procedure :: get_meta_array2_double
        module procedure :: get_meta_array2_complex
        module procedure :: get_meta_array2_dcomplex
        module procedure :: get_meta_array2_logical
        module procedure :: get_meta_array3_int
        module procedure :: get_meta_array3_real
        module procedure :: get_meta_array3_double
        module procedure :: get_meta_array3_complex
        module procedure :: get_meta_array3_dcomplex
        module procedure :: get_meta_array3_logical
        module procedure :: get_meta_array4_int
        module procedure :: get_meta_array4_real
        module procedure :: get_meta_array4_double
        module procedure :: get_meta_array4_complex
        module procedure :: get_meta_array4_dcomplex
        module procedure :: get_meta_array4_logical
    end interface
    ! Get a meta-array core from an array of meta-arrays by name; the return is the core array of a meta-array
    interface get_meta_array_core
        module procedure :: get_meta_array_core1_int
        module procedure :: get_meta_array_core1_real
        module procedure :: get_meta_array_core1_double
        module procedure :: get_meta_array_core1_complex
        module procedure :: get_meta_array_core1_dcomplex
        module procedure :: get_meta_array_core1_logical
        module procedure :: get_meta_array_core2_int
        module procedure :: get_meta_array_core2_real
        module procedure :: get_meta_array_core2_double
        module procedure :: get_meta_array_core2_complex
        module procedure :: get_meta_array_core2_dcomplex
        module procedure :: get_meta_array_core2_logical
        module procedure :: get_meta_array_core3_int
        module procedure :: get_meta_array_core3_real
        module procedure :: get_meta_array_core3_double
        module procedure :: get_meta_array_core3_complex
        module procedure :: get_meta_array_core3_dcomplex
        module procedure :: get_meta_array_core3_logical
        module procedure :: get_meta_array_core4_int
        module procedure :: get_meta_array_core4_real
        module procedure :: get_meta_array_core4_double
        module procedure :: get_meta_array_core4_complex
        module procedure :: get_meta_array_core4_dcomplex
        module procedure :: get_meta_array_core4_logical
    end interface
    ! Assign an array to some meta-array in an array of meta-arrays by name
    interface set_meta_array_core
        module procedure :: set_meta_array_core1_int
        module procedure :: set_meta_array_core1_real
        module procedure :: set_meta_array_core1_double
        module procedure :: set_meta_array_core1_complex
        module procedure :: set_meta_array_core1_dcomplex
        module procedure :: set_meta_array_core1_logical
        module procedure :: set_meta_array_core2_int
        module procedure :: set_meta_array_core2_real
        module procedure :: set_meta_array_core2_double
        module procedure :: set_meta_array_core2_complex
        module procedure :: set_meta_array_core2_dcomplex
        module procedure :: set_meta_array_core2_logical
        module procedure :: set_meta_array_core3_int
        module procedure :: set_meta_array_core3_real
        module procedure :: set_meta_array_core3_double
        module procedure :: set_meta_array_core3_complex
        module procedure :: set_meta_array_core3_dcomplex
        module procedure :: set_meta_array_core3_logical
        module procedure :: set_meta_array_core4_int
        module procedure :: set_meta_array_core4_real
        module procedure :: set_meta_array_core4_double
        module procedure :: set_meta_array_core4_complex
        module procedure :: set_meta_array_core4_dcomplex
        module procedure :: set_meta_array_core4_logical
    end interface
end module libflit_array_extension
