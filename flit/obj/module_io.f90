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
! The following operation is permitted:
!
! w = reshape(w, shape=..., order=...)
!
! as long as w is allocatable array
!
module libflit_io
    use libflit_string
    use libflit_utility
    use libflit_date_time
    use libflit_filedir
    use libflit_array
    use libflit_error
    use iso_fortran_env
    use libflit_array_operation
    implicit none
    private
    interface input_array
        module procedure :: input_array_1d_int
        module procedure :: input_array_2d_int
        module procedure :: input_array_3d_int
        module procedure :: input_array_4d_int
        module procedure :: input_array_1d_float
        module procedure :: input_array_2d_float
        module procedure :: input_array_3d_float
        module procedure :: input_array_4d_float
        module procedure :: input_array_1d_double
        module procedure :: input_array_2d_double
        module procedure :: input_array_3d_double
        module procedure :: input_array_4d_double
        module procedure :: input_array_1d_complex
        module procedure :: input_array_2d_complex
        module procedure :: input_array_3d_complex
        module procedure :: input_array_4d_complex
        module procedure :: input_array_1d_dcomplex
        module procedure :: input_array_2d_dcomplex
        module procedure :: input_array_3d_dcomplex
        module procedure :: input_array_4d_dcomplex
    end interface input_array
    interface stdin_array
        module procedure :: stdin_array_1d_int
        module procedure :: stdin_array_2d_int
        module procedure :: stdin_array_3d_int
        module procedure :: stdin_array_4d_int
        module procedure :: stdin_array_1d_float
        module procedure :: stdin_array_2d_float
        module procedure :: stdin_array_3d_float
        module procedure :: stdin_array_4d_float
        module procedure :: stdin_array_1d_double
        module procedure :: stdin_array_2d_double
        module procedure :: stdin_array_3d_double
        module procedure :: stdin_array_4d_double
        module procedure :: stdin_array_1d_complex
        module procedure :: stdin_array_2d_complex
        module procedure :: stdin_array_3d_complex
        module procedure :: stdin_array_4d_complex
        module procedure :: stdin_array_1d_dcomplex
        module procedure :: stdin_array_2d_dcomplex
        module procedure :: stdin_array_3d_dcomplex
        module procedure :: stdin_array_4d_dcomplex
    end interface stdin_array
    interface output_array
        module procedure :: output_array_1d_int
        module procedure :: output_array_2d_int
        module procedure :: output_array_3d_int
        module procedure :: output_array_4d_int
        module procedure :: output_array_1d_float
        module procedure :: output_array_2d_float
        module procedure :: output_array_3d_float
        module procedure :: output_array_4d_float
        module procedure :: output_array_1d_double
        module procedure :: output_array_2d_double
        module procedure :: output_array_3d_double
        module procedure :: output_array_4d_double
        module procedure :: output_array_1d_complex
        module procedure :: output_array_2d_complex
        module procedure :: output_array_3d_complex
        module procedure :: output_array_4d_complex
        module procedure :: output_array_1d_dcomplex
        module procedure :: output_array_2d_dcomplex
        module procedure :: output_array_3d_dcomplex
        module procedure :: output_array_4d_dcomplex
    end interface output_array
    interface stdout_array
        module procedure :: stdout_array_1d_int
        module procedure :: stdout_array_2d_int
        module procedure :: stdout_array_3d_int
        module procedure :: stdout_array_4d_int
        module procedure :: stdout_array_1d_float
        module procedure :: stdout_array_2d_float
        module procedure :: stdout_array_3d_float
        module procedure :: stdout_array_4d_float
        module procedure :: stdout_array_1d_double
        module procedure :: stdout_array_2d_double
        module procedure :: stdout_array_3d_double
        module procedure :: stdout_array_4d_double
        module procedure :: stdout_array_1d_complex
        module procedure :: stdout_array_2d_complex
        module procedure :: stdout_array_3d_complex
        module procedure :: stdout_array_4d_complex
        module procedure :: stdout_array_1d_dcomplex
        module procedure :: stdout_array_2d_dcomplex
        module procedure :: stdout_array_3d_dcomplex
        module procedure :: stdout_array_4d_dcomplex
    end interface stdout_array
    integer :: ioerr
    interface load
        module procedure :: load_array_1d_float
        module procedure :: load_array_2d_float
        module procedure :: load_array_3d_float
    end interface load
    public :: input_array
    public :: stdin_array
    public :: output_array
    public :: stdout_array
    public :: load
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
subroutine input_array_1d_int(w, filename, pos, endian)
    integer, dimension(:), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_1d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_1d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1)), &
        ' <input_array_1d> Error: '//num2str(size(w)*sizeof(w(1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! Read binary file
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    read (funit, pos=ppos) w
    close (funit)
end subroutine input_array_1d_int
subroutine stdin_array_1d_int(w, endian)
    integer, dimension(:), intent(inout) :: w
    character(len=*), intent(in), optional :: endian
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_1d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1)), &
        ' <stdin_array_1d> Error: '//num2str(size(w)*sizeof(w(1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    ! Read stream
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    read (input_unit) w
end subroutine stdin_array_1d_int
subroutine input_array_2d_int(w, filename, transp, pos, endian)
    integer, dimension(:, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: transp
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    integer, allocatable, dimension(:, :) :: tw
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_2d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_2d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1)), &
        ' <input_array_2d> Error: '//num2str(size(w)*sizeof(w(1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(transp)) then
        if (transp) then
            ! if data stored in different order
            allocate (tw(1:size(w, 2), 1:size(w, 1)))
            read (funit, pos=ppos) tw
            w = transpose(tw)
            deallocate (tw)
        end if
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_2d_int
subroutine stdin_array_2d_int(w, transp, endian)
    integer, dimension(:, :), intent(inout) :: w
    logical, intent(in), optional :: transp
    character(len=*), intent(in), optional :: endian
    integer, allocatable, dimension(:, :) :: tw
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_2d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1)), &
        ' <stdin_array_2d> Error: '//num2str(size(w)*sizeof(w(1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(transp)) then
        if (transp) then
            ! if data stored in different order
            allocate (tw(1:size(w, 2), 1:size(w, 1)))
            read (input_unit) tw
            w = transpose(tw)
            deallocate (tw)
        end if
    else
        read (input_unit) w
    end if
end subroutine stdin_array_2d_int
subroutine input_array_3d_int(w, filename, store, pos, endian)
    integer, dimension(:, :, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    integer, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_3d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_3d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1, 1)), &
        ' <input_array_3d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (funit, pos=ppos) tw
        allocate (sorder(1:3))
        call get_integer_digits(store, 3, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_3d_int
subroutine stdin_array_3d_int(w, store, endian)
    integer, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: store
    character(len=*), intent(in), optional :: endian
    integer, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_3d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1, 1)), &
        ' <stdin_array_3d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (input_unit, iostat=ioerr) tw
        allocate (sorder(1:3))
        call get_integer_digits(store, 3, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (input_unit) w
    end if
end subroutine stdin_array_3d_int
subroutine input_array_4d_int(w, filename, store, pos, endian)
    integer, dimension(:, :, :, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    integer, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_4d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_4d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1, 1, 1)), &
        ' <input_array_4d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (funit, pos=ppos) tw
        allocate (sorder(1:4))
        call get_integer_digits(store, 4, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_4d_int
subroutine stdin_array_4d_int(w, store, endian)
    integer, dimension(:, :, :, :), intent(inout) :: w
    integer, intent(in), optional :: store
    character(len=*), intent(in), optional :: endian
    integer, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_4d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1, 1, 1)), &
        ' <stdin_array_4d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (input_unit, iostat=ioerr) tw
        allocate (sorder(1:4))
        call get_integer_digits(store, 4, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (input_unit) w
    end if
end subroutine stdin_array_4d_int
subroutine output_array_1d_int(w, filename, append, ascii, format)
    integer, dimension(:), intent(in) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: append, ascii
    character(len=*), optional :: format
    integer :: funit, i
    logical :: output_ascii
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), access='stream', &
                form='formatted', action='write', &
                status='replace')
        end if
        do i = 1, size(w)
            write (funit, '('//tidy(output_format)//')') w(i)
        end do
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='replace')
        end if
        write (funit) w
    end if
    close (funit)
end subroutine output_array_1d_int
subroutine stdout_array_1d_int(w, append, ascii, format)
    integer, dimension(:), intent(in) :: w
    logical, intent(in), optional :: append, ascii
    character(len=*), optional :: format
    logical :: output_ascii
    integer :: i
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    close (output_unit)
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='formatted', action='write', access='stream')
        end if
        do i = 1, size(w)
            write (output_unit, '('//tidy(output_format)//')') w(i)
        end do
    else
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream')
        end if
        write (output_unit) w
    end if
end subroutine stdout_array_1d_int
subroutine output_array_2d_int(w, filename, transp, append, ascii, format)
    integer, dimension(:, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: transp, append, ascii
    character(len=*), optional :: format
    integer :: funit, i
    logical :: output_ascii
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), access='stream', &
                form='formatted', action='write', &
                status='replace')
        end if
        if (present(transp)) then
            if (transp) then
                do i = 1, size(w, 2)
                    write (funit, '('//num2str(size(w, 1))//tidy(output_format)//')') w(:, i)
                end do
            end if
        else
            do i = 1, size(w, 1)
                write (funit, '('//num2str(size(w, 1))//tidy(output_format)//')') w(i, :)
            end do
        end if
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='replace')
        end if
        if (present(transp)) then
            if (transp) then
                write (funit) transpose(w)
            end if
        else
            write (funit) w
        end if
    end if
    close (funit)
end subroutine output_array_2d_int
subroutine stdout_array_2d_int(w, transp, append, ascii, format)
    integer, dimension(:, :), intent(in) :: w
    logical, intent(in), optional :: transp, append, ascii
    character(len=*), optional :: format
    logical :: output_ascii
    integer :: i
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    close (output_unit)
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='formatted', action='write', access='stream')
        end if
        if (present(transp)) then
            if (transp) then
                do i = 1, size(w, 2)
                    write (output_unit, '('//num2str(size(w, 1))//'es)') w(:, i)
                end do
            end if
        else
            do i = 1, size(w, 1)
                write (output_unit, '('//num2str(size(w, 1))//'es)') w(i, :)
            end do
        end if
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream')
        end if
        if (present(transp)) then
            if (transp) then
                write (output_unit) transpose(w)
            end if
        else
            write (output_unit) w
        end if
    end if
end subroutine stdout_array_2d_int
subroutine output_array_3d_int(w, filename, store, append)
    integer, dimension(:, :, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    integer :: funit
    if (present(append)) then
        if (append) then
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='write', access='stream', &
            status='replace')
    end if
    if (present(store)) then
        write (funit) permute(w, store)
    else
        write (funit) w
    end if
    close (funit)
end subroutine output_array_3d_int
subroutine stdout_array_3d_int(w, store, append)
    integer, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    close (output_unit)
    if (present(append)) then
        if (append) then
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (unit=output_unit, &
            form='unformatted', action='write', access='stream')
    end if
    if (present(store)) then
        write (output_unit) permute(w, store)
    else
        write (output_unit) w
    end if
end subroutine stdout_array_3d_int
subroutine output_array_4d_int(w, filename, store, append)
    integer, dimension(:, :, :, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    integer :: funit
    if (present(append)) then
        if (append) then
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='write', access='stream', &
            status='replace')
    end if
    if (present(store)) then
        write (funit) permute(w, store)
    else
        write (funit) w
    end if
    close (funit)
end subroutine output_array_4d_int
subroutine stdout_array_4d_int(w, store, append)
    integer, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    close (output_unit)
    if (present(append)) then
        if (append) then
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (unit=output_unit, &
            form='unformatted', action='write', access='stream')
    end if
    if (present(store)) then
        write (output_unit) permute(w, store)
    else
        write (output_unit) w
    end if
end subroutine stdout_array_4d_int
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
subroutine input_array_1d_float(w, filename, pos, endian)
    real, dimension(:), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_1d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_1d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1)), &
        ' <input_array_1d> Error: '//num2str(size(w)*sizeof(w(1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! Read binary file
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    read (funit, pos=ppos) w
    close (funit)
end subroutine input_array_1d_float
subroutine stdin_array_1d_float(w, endian)
    real, dimension(:), intent(inout) :: w
    character(len=*), intent(in), optional :: endian
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_1d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1)), &
        ' <stdin_array_1d> Error: '//num2str(size(w)*sizeof(w(1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    ! Read stream
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    read (input_unit) w
end subroutine stdin_array_1d_float
subroutine input_array_2d_float(w, filename, transp, pos, endian)
    real, dimension(:, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: transp
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    real, allocatable, dimension(:, :) :: tw
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_2d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_2d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1)), &
        ' <input_array_2d> Error: '//num2str(size(w)*sizeof(w(1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(transp)) then
        if (transp) then
            ! if data stored in different order
            allocate (tw(1:size(w, 2), 1:size(w, 1)))
            read (funit, pos=ppos) tw
            w = transpose(tw)
            deallocate (tw)
        end if
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_2d_float
subroutine stdin_array_2d_float(w, transp, endian)
    real, dimension(:, :), intent(inout) :: w
    logical, intent(in), optional :: transp
    character(len=*), intent(in), optional :: endian
    real, allocatable, dimension(:, :) :: tw
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_2d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1)), &
        ' <stdin_array_2d> Error: '//num2str(size(w)*sizeof(w(1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(transp)) then
        if (transp) then
            ! if data stored in different order
            allocate (tw(1:size(w, 2), 1:size(w, 1)))
            read (input_unit) tw
            w = transpose(tw)
            deallocate (tw)
        end if
    else
        read (input_unit) w
    end if
end subroutine stdin_array_2d_float
subroutine input_array_3d_float(w, filename, store, pos, endian)
    real, dimension(:, :, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    real, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_3d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_3d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1, 1)), &
        ' <input_array_3d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (funit, pos=ppos) tw
        allocate (sorder(1:3))
        call get_integer_digits(store, 3, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_3d_float
subroutine stdin_array_3d_float(w, store, endian)
    real, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: store
    character(len=*), intent(in), optional :: endian
    real, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_3d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1, 1)), &
        ' <stdin_array_3d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (input_unit, iostat=ioerr) tw
        allocate (sorder(1:3))
        call get_integer_digits(store, 3, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (input_unit) w
    end if
end subroutine stdin_array_3d_float
subroutine input_array_4d_float(w, filename, store, pos, endian)
    real, dimension(:, :, :, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    real, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_4d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_4d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1, 1, 1)), &
        ' <input_array_4d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (funit, pos=ppos) tw
        allocate (sorder(1:4))
        call get_integer_digits(store, 4, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_4d_float
subroutine stdin_array_4d_float(w, store, endian)
    real, dimension(:, :, :, :), intent(inout) :: w
    integer, intent(in), optional :: store
    character(len=*), intent(in), optional :: endian
    real, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_4d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1, 1, 1)), &
        ' <stdin_array_4d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (input_unit, iostat=ioerr) tw
        allocate (sorder(1:4))
        call get_integer_digits(store, 4, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (input_unit) w
    end if
end subroutine stdin_array_4d_float
subroutine output_array_1d_float(w, filename, append, ascii, format)
    real, dimension(:), intent(in) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: append, ascii
    character(len=*), optional :: format
    integer :: funit, i
    logical :: output_ascii
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), access='stream', &
                form='formatted', action='write', &
                status='replace')
        end if
        do i = 1, size(w)
            write (funit, '('//tidy(output_format)//')') w(i)
        end do
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='replace')
        end if
        write (funit) w
    end if
    close (funit)
end subroutine output_array_1d_float
subroutine stdout_array_1d_float(w, append, ascii, format)
    real, dimension(:), intent(in) :: w
    logical, intent(in), optional :: append, ascii
    character(len=*), optional :: format
    logical :: output_ascii
    integer :: i
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    close (output_unit)
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='formatted', action='write', access='stream')
        end if
        do i = 1, size(w)
            write (output_unit, '('//tidy(output_format)//')') w(i)
        end do
    else
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream')
        end if
        write (output_unit) w
    end if
end subroutine stdout_array_1d_float
subroutine output_array_2d_float(w, filename, transp, append, ascii, format)
    real, dimension(:, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: transp, append, ascii
    character(len=*), optional :: format
    integer :: funit, i
    logical :: output_ascii
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), access='stream', &
                form='formatted', action='write', &
                status='replace')
        end if
        if (present(transp)) then
            if (transp) then
                do i = 1, size(w, 2)
                    write (funit, '('//num2str(size(w, 1))//tidy(output_format)//')') w(:, i)
                end do
            end if
        else
            do i = 1, size(w, 1)
                write (funit, '('//num2str(size(w, 1))//tidy(output_format)//')') w(i, :)
            end do
        end if
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='replace')
        end if
        if (present(transp)) then
            if (transp) then
                write (funit) transpose(w)
            end if
        else
            write (funit) w
        end if
    end if
    close (funit)
end subroutine output_array_2d_float
subroutine stdout_array_2d_float(w, transp, append, ascii, format)
    real, dimension(:, :), intent(in) :: w
    logical, intent(in), optional :: transp, append, ascii
    character(len=*), optional :: format
    logical :: output_ascii
    integer :: i
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    close (output_unit)
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='formatted', action='write', access='stream')
        end if
        if (present(transp)) then
            if (transp) then
                do i = 1, size(w, 2)
                    write (output_unit, '('//num2str(size(w, 1))//'es)') w(:, i)
                end do
            end if
        else
            do i = 1, size(w, 1)
                write (output_unit, '('//num2str(size(w, 1))//'es)') w(i, :)
            end do
        end if
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream')
        end if
        if (present(transp)) then
            if (transp) then
                write (output_unit) transpose(w)
            end if
        else
            write (output_unit) w
        end if
    end if
end subroutine stdout_array_2d_float
subroutine output_array_3d_float(w, filename, store, append)
    real, dimension(:, :, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    integer :: funit
    if (present(append)) then
        if (append) then
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='write', access='stream', &
            status='replace')
    end if
    if (present(store)) then
        write (funit) permute(w, store)
    else
        write (funit) w
    end if
    close (funit)
end subroutine output_array_3d_float
subroutine stdout_array_3d_float(w, store, append)
    real, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    close (output_unit)
    if (present(append)) then
        if (append) then
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (unit=output_unit, &
            form='unformatted', action='write', access='stream')
    end if
    if (present(store)) then
        write (output_unit) permute(w, store)
    else
        write (output_unit) w
    end if
end subroutine stdout_array_3d_float
subroutine output_array_4d_float(w, filename, store, append)
    real, dimension(:, :, :, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    integer :: funit
    if (present(append)) then
        if (append) then
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='write', access='stream', &
            status='replace')
    end if
    if (present(store)) then
        write (funit) permute(w, store)
    else
        write (funit) w
    end if
    close (funit)
end subroutine output_array_4d_float
subroutine stdout_array_4d_float(w, store, append)
    real, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    close (output_unit)
    if (present(append)) then
        if (append) then
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (unit=output_unit, &
            form='unformatted', action='write', access='stream')
    end if
    if (present(store)) then
        write (output_unit) permute(w, store)
    else
        write (output_unit) w
    end if
end subroutine stdout_array_4d_float
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
subroutine input_array_1d_double(w, filename, pos, endian)
    double precision, dimension(:), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_1d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_1d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1)), &
        ' <input_array_1d> Error: '//num2str(size(w)*sizeof(w(1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! Read binary file
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    read (funit, pos=ppos) w
    close (funit)
end subroutine input_array_1d_double
subroutine stdin_array_1d_double(w, endian)
    double precision, dimension(:), intent(inout) :: w
    character(len=*), intent(in), optional :: endian
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_1d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1)), &
        ' <stdin_array_1d> Error: '//num2str(size(w)*sizeof(w(1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    ! Read stream
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    read (input_unit) w
end subroutine stdin_array_1d_double
subroutine input_array_2d_double(w, filename, transp, pos, endian)
    double precision, dimension(:, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: transp
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    double precision, allocatable, dimension(:, :) :: tw
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_2d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_2d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1)), &
        ' <input_array_2d> Error: '//num2str(size(w)*sizeof(w(1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(transp)) then
        if (transp) then
            ! if data stored in different order
            allocate (tw(1:size(w, 2), 1:size(w, 1)))
            read (funit, pos=ppos) tw
            w = transpose(tw)
            deallocate (tw)
        end if
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_2d_double
subroutine stdin_array_2d_double(w, transp, endian)
    double precision, dimension(:, :), intent(inout) :: w
    logical, intent(in), optional :: transp
    character(len=*), intent(in), optional :: endian
    double precision, allocatable, dimension(:, :) :: tw
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_2d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1)), &
        ' <stdin_array_2d> Error: '//num2str(size(w)*sizeof(w(1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(transp)) then
        if (transp) then
            ! if data stored in different order
            allocate (tw(1:size(w, 2), 1:size(w, 1)))
            read (input_unit) tw
            w = transpose(tw)
            deallocate (tw)
        end if
    else
        read (input_unit) w
    end if
end subroutine stdin_array_2d_double
subroutine input_array_3d_double(w, filename, store, pos, endian)
    double precision, dimension(:, :, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    double precision, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_3d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_3d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1, 1)), &
        ' <input_array_3d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (funit, pos=ppos) tw
        allocate (sorder(1:3))
        call get_integer_digits(store, 3, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_3d_double
subroutine stdin_array_3d_double(w, store, endian)
    double precision, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: store
    character(len=*), intent(in), optional :: endian
    double precision, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_3d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1, 1)), &
        ' <stdin_array_3d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (input_unit, iostat=ioerr) tw
        allocate (sorder(1:3))
        call get_integer_digits(store, 3, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (input_unit) w
    end if
end subroutine stdin_array_3d_double
subroutine input_array_4d_double(w, filename, store, pos, endian)
    double precision, dimension(:, :, :, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    double precision, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_4d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_4d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1, 1, 1)), &
        ' <input_array_4d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (funit, pos=ppos) tw
        allocate (sorder(1:4))
        call get_integer_digits(store, 4, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_4d_double
subroutine stdin_array_4d_double(w, store, endian)
    double precision, dimension(:, :, :, :), intent(inout) :: w
    integer, intent(in), optional :: store
    character(len=*), intent(in), optional :: endian
    double precision, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_4d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1, 1, 1)), &
        ' <stdin_array_4d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (input_unit, iostat=ioerr) tw
        allocate (sorder(1:4))
        call get_integer_digits(store, 4, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (input_unit) w
    end if
end subroutine stdin_array_4d_double
subroutine output_array_1d_double(w, filename, append, ascii, format)
    double precision, dimension(:), intent(in) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: append, ascii
    character(len=*), optional :: format
    integer :: funit, i
    logical :: output_ascii
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), access='stream', &
                form='formatted', action='write', &
                status='replace')
        end if
        do i = 1, size(w)
            write (funit, '('//tidy(output_format)//')') w(i)
        end do
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='replace')
        end if
        write (funit) w
    end if
    close (funit)
end subroutine output_array_1d_double
subroutine stdout_array_1d_double(w, append, ascii, format)
    double precision, dimension(:), intent(in) :: w
    logical, intent(in), optional :: append, ascii
    character(len=*), optional :: format
    logical :: output_ascii
    integer :: i
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    close (output_unit)
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='formatted', action='write', access='stream')
        end if
        do i = 1, size(w)
            write (output_unit, '('//tidy(output_format)//')') w(i)
        end do
    else
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream')
        end if
        write (output_unit) w
    end if
end subroutine stdout_array_1d_double
subroutine output_array_2d_double(w, filename, transp, append, ascii, format)
    double precision, dimension(:, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: transp, append, ascii
    character(len=*), optional :: format
    integer :: funit, i
    logical :: output_ascii
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), access='stream', &
                form='formatted', action='write', &
                status='replace')
        end if
        if (present(transp)) then
            if (transp) then
                do i = 1, size(w, 2)
                    write (funit, '('//num2str(size(w, 1))//tidy(output_format)//')') w(:, i)
                end do
            end if
        else
            do i = 1, size(w, 1)
                write (funit, '('//num2str(size(w, 1))//tidy(output_format)//')') w(i, :)
            end do
        end if
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='replace')
        end if
        if (present(transp)) then
            if (transp) then
                write (funit) transpose(w)
            end if
        else
            write (funit) w
        end if
    end if
    close (funit)
end subroutine output_array_2d_double
subroutine stdout_array_2d_double(w, transp, append, ascii, format)
    double precision, dimension(:, :), intent(in) :: w
    logical, intent(in), optional :: transp, append, ascii
    character(len=*), optional :: format
    logical :: output_ascii
    integer :: i
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    close (output_unit)
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='formatted', action='write', access='stream')
        end if
        if (present(transp)) then
            if (transp) then
                do i = 1, size(w, 2)
                    write (output_unit, '('//num2str(size(w, 1))//'es)') w(:, i)
                end do
            end if
        else
            do i = 1, size(w, 1)
                write (output_unit, '('//num2str(size(w, 1))//'es)') w(i, :)
            end do
        end if
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream')
        end if
        if (present(transp)) then
            if (transp) then
                write (output_unit) transpose(w)
            end if
        else
            write (output_unit) w
        end if
    end if
end subroutine stdout_array_2d_double
subroutine output_array_3d_double(w, filename, store, append)
    double precision, dimension(:, :, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    integer :: funit
    if (present(append)) then
        if (append) then
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='write', access='stream', &
            status='replace')
    end if
    if (present(store)) then
        write (funit) permute(w, store)
    else
        write (funit) w
    end if
    close (funit)
end subroutine output_array_3d_double
subroutine stdout_array_3d_double(w, store, append)
    double precision, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    close (output_unit)
    if (present(append)) then
        if (append) then
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (unit=output_unit, &
            form='unformatted', action='write', access='stream')
    end if
    if (present(store)) then
        write (output_unit) permute(w, store)
    else
        write (output_unit) w
    end if
end subroutine stdout_array_3d_double
subroutine output_array_4d_double(w, filename, store, append)
    double precision, dimension(:, :, :, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    integer :: funit
    if (present(append)) then
        if (append) then
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='write', access='stream', &
            status='replace')
    end if
    if (present(store)) then
        write (funit) permute(w, store)
    else
        write (funit) w
    end if
    close (funit)
end subroutine output_array_4d_double
subroutine stdout_array_4d_double(w, store, append)
    double precision, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    close (output_unit)
    if (present(append)) then
        if (append) then
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (unit=output_unit, &
            form='unformatted', action='write', access='stream')
    end if
    if (present(store)) then
        write (output_unit) permute(w, store)
    else
        write (output_unit) w
    end if
end subroutine stdout_array_4d_double
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
subroutine input_array_1d_complex(w, filename, pos, endian)
    complex, dimension(:), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_1d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_1d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1)), &
        ' <input_array_1d> Error: '//num2str(size(w)*sizeof(w(1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! Read binary file
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    read (funit, pos=ppos) w
    close (funit)
end subroutine input_array_1d_complex
subroutine stdin_array_1d_complex(w, endian)
    complex, dimension(:), intent(inout) :: w
    character(len=*), intent(in), optional :: endian
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_1d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1)), &
        ' <stdin_array_1d> Error: '//num2str(size(w)*sizeof(w(1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    ! Read stream
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    read (input_unit) w
end subroutine stdin_array_1d_complex
subroutine input_array_2d_complex(w, filename, transp, pos, endian)
    complex, dimension(:, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: transp
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    complex, allocatable, dimension(:, :) :: tw
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_2d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_2d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1)), &
        ' <input_array_2d> Error: '//num2str(size(w)*sizeof(w(1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(transp)) then
        if (transp) then
            ! if data stored in different order
            allocate (tw(1:size(w, 2), 1:size(w, 1)))
            read (funit, pos=ppos) tw
            w = transpose(tw)
            deallocate (tw)
        end if
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_2d_complex
subroutine stdin_array_2d_complex(w, transp, endian)
    complex, dimension(:, :), intent(inout) :: w
    logical, intent(in), optional :: transp
    character(len=*), intent(in), optional :: endian
    complex, allocatable, dimension(:, :) :: tw
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_2d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1)), &
        ' <stdin_array_2d> Error: '//num2str(size(w)*sizeof(w(1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(transp)) then
        if (transp) then
            ! if data stored in different order
            allocate (tw(1:size(w, 2), 1:size(w, 1)))
            read (input_unit) tw
            w = transpose(tw)
            deallocate (tw)
        end if
    else
        read (input_unit) w
    end if
end subroutine stdin_array_2d_complex
subroutine input_array_3d_complex(w, filename, store, pos, endian)
    complex, dimension(:, :, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    complex, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_3d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_3d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1, 1)), &
        ' <input_array_3d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (funit, pos=ppos) tw
        allocate (sorder(1:3))
        call get_integer_digits(store, 3, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_3d_complex
subroutine stdin_array_3d_complex(w, store, endian)
    complex, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: store
    character(len=*), intent(in), optional :: endian
    complex, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_3d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1, 1)), &
        ' <stdin_array_3d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (input_unit, iostat=ioerr) tw
        allocate (sorder(1:3))
        call get_integer_digits(store, 3, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (input_unit) w
    end if
end subroutine stdin_array_3d_complex
subroutine input_array_4d_complex(w, filename, store, pos, endian)
    complex, dimension(:, :, :, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    complex, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_4d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_4d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1, 1, 1)), &
        ' <input_array_4d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (funit, pos=ppos) tw
        allocate (sorder(1:4))
        call get_integer_digits(store, 4, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_4d_complex
subroutine stdin_array_4d_complex(w, store, endian)
    complex, dimension(:, :, :, :), intent(inout) :: w
    integer, intent(in), optional :: store
    character(len=*), intent(in), optional :: endian
    complex, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_4d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1, 1, 1)), &
        ' <stdin_array_4d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (input_unit, iostat=ioerr) tw
        allocate (sorder(1:4))
        call get_integer_digits(store, 4, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (input_unit) w
    end if
end subroutine stdin_array_4d_complex
subroutine output_array_1d_complex(w, filename, append, ascii, format)
    complex, dimension(:), intent(in) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: append, ascii
    character(len=*), optional :: format
    integer :: funit, i
    logical :: output_ascii
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), access='stream', &
                form='formatted', action='write', &
                status='replace')
        end if
        do i = 1, size(w)
            write (funit, '('//tidy(output_format)//')') w(i)
        end do
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='replace')
        end if
        write (funit) w
    end if
    close (funit)
end subroutine output_array_1d_complex
subroutine stdout_array_1d_complex(w, append, ascii, format)
    complex, dimension(:), intent(in) :: w
    logical, intent(in), optional :: append, ascii
    character(len=*), optional :: format
    logical :: output_ascii
    integer :: i
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    close (output_unit)
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='formatted', action='write', access='stream')
        end if
        do i = 1, size(w)
            write (output_unit, '('//tidy(output_format)//')') w(i)
        end do
    else
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream')
        end if
        write (output_unit) w
    end if
end subroutine stdout_array_1d_complex
subroutine output_array_2d_complex(w, filename, transp, append, ascii, format)
    complex, dimension(:, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: transp, append, ascii
    character(len=*), optional :: format
    integer :: funit, i
    logical :: output_ascii
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), access='stream', &
                form='formatted', action='write', &
                status='replace')
        end if
        if (present(transp)) then
            if (transp) then
                do i = 1, size(w, 2)
                    write (funit, '('//num2str(size(w, 1))//tidy(output_format)//')') w(:, i)
                end do
            end if
        else
            do i = 1, size(w, 1)
                write (funit, '('//num2str(size(w, 1))//tidy(output_format)//')') w(i, :)
            end do
        end if
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='replace')
        end if
        if (present(transp)) then
            if (transp) then
                write (funit) transpose(w)
            end if
        else
            write (funit) w
        end if
    end if
    close (funit)
end subroutine output_array_2d_complex
subroutine stdout_array_2d_complex(w, transp, append, ascii, format)
    complex, dimension(:, :), intent(in) :: w
    logical, intent(in), optional :: transp, append, ascii
    character(len=*), optional :: format
    logical :: output_ascii
    integer :: i
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    close (output_unit)
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='formatted', action='write', access='stream')
        end if
        if (present(transp)) then
            if (transp) then
                do i = 1, size(w, 2)
                    write (output_unit, '('//num2str(size(w, 1))//'es)') w(:, i)
                end do
            end if
        else
            do i = 1, size(w, 1)
                write (output_unit, '('//num2str(size(w, 1))//'es)') w(i, :)
            end do
        end if
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream')
        end if
        if (present(transp)) then
            if (transp) then
                write (output_unit) transpose(w)
            end if
        else
            write (output_unit) w
        end if
    end if
end subroutine stdout_array_2d_complex
subroutine output_array_3d_complex(w, filename, store, append)
    complex, dimension(:, :, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    integer :: funit
    if (present(append)) then
        if (append) then
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='write', access='stream', &
            status='replace')
    end if
    if (present(store)) then
        write (funit) permute(w, store)
    else
        write (funit) w
    end if
    close (funit)
end subroutine output_array_3d_complex
subroutine stdout_array_3d_complex(w, store, append)
    complex, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    close (output_unit)
    if (present(append)) then
        if (append) then
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (unit=output_unit, &
            form='unformatted', action='write', access='stream')
    end if
    if (present(store)) then
        write (output_unit) permute(w, store)
    else
        write (output_unit) w
    end if
end subroutine stdout_array_3d_complex
subroutine output_array_4d_complex(w, filename, store, append)
    complex, dimension(:, :, :, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    integer :: funit
    if (present(append)) then
        if (append) then
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='write', access='stream', &
            status='replace')
    end if
    if (present(store)) then
        write (funit) permute(w, store)
    else
        write (funit) w
    end if
    close (funit)
end subroutine output_array_4d_complex
subroutine stdout_array_4d_complex(w, store, append)
    complex, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    close (output_unit)
    if (present(append)) then
        if (append) then
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (unit=output_unit, &
            form='unformatted', action='write', access='stream')
    end if
    if (present(store)) then
        write (output_unit) permute(w, store)
    else
        write (output_unit) w
    end if
end subroutine stdout_array_4d_complex
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
subroutine input_array_1d_dcomplex(w, filename, pos, endian)
    double complex, dimension(:), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_1d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_1d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1)), &
        ' <input_array_1d> Error: '//num2str(size(w)*sizeof(w(1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! Read binary file
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    read (funit, pos=ppos) w
    close (funit)
end subroutine input_array_1d_dcomplex
subroutine stdin_array_1d_dcomplex(w, endian)
    double complex, dimension(:), intent(inout) :: w
    character(len=*), intent(in), optional :: endian
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_1d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1)), &
        ' <stdin_array_1d> Error: '//num2str(size(w)*sizeof(w(1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    ! Read stream
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    read (input_unit) w
end subroutine stdin_array_1d_dcomplex
subroutine input_array_2d_dcomplex(w, filename, transp, pos, endian)
    double complex, dimension(:, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: transp
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    double complex, allocatable, dimension(:, :) :: tw
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_2d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_2d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1)), &
        ' <input_array_2d> Error: '//num2str(size(w)*sizeof(w(1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(transp)) then
        if (transp) then
            ! if data stored in different order
            allocate (tw(1:size(w, 2), 1:size(w, 1)))
            read (funit, pos=ppos) tw
            w = transpose(tw)
            deallocate (tw)
        end if
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_2d_dcomplex
subroutine stdin_array_2d_dcomplex(w, transp, endian)
    double complex, dimension(:, :), intent(inout) :: w
    logical, intent(in), optional :: transp
    character(len=*), intent(in), optional :: endian
    double complex, allocatable, dimension(:, :) :: tw
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_2d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1)), &
        ' <stdin_array_2d> Error: '//num2str(size(w)*sizeof(w(1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(transp)) then
        if (transp) then
            ! if data stored in different order
            allocate (tw(1:size(w, 2), 1:size(w, 1)))
            read (input_unit) tw
            w = transpose(tw)
            deallocate (tw)
        end if
    else
        read (input_unit) w
    end if
end subroutine stdin_array_2d_dcomplex
subroutine input_array_3d_dcomplex(w, filename, store, pos, endian)
    double complex, dimension(:, :, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    double complex, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_3d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_3d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1, 1)), &
        ' <input_array_3d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (funit, pos=ppos) tw
        allocate (sorder(1:3))
        call get_integer_digits(store, 3, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_3d_dcomplex
subroutine stdin_array_3d_dcomplex(w, store, endian)
    double complex, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: store
    character(len=*), intent(in), optional :: endian
    double complex, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_3d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1, 1)), &
        ' <stdin_array_3d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (input_unit, iostat=ioerr) tw
        allocate (sorder(1:3))
        call get_integer_digits(store, 3, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (input_unit) w
    end if
end subroutine stdin_array_3d_dcomplex
subroutine input_array_4d_dcomplex(w, filename, store, pos, endian)
    double complex, dimension(:, :, :, :), intent(inout) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: endian
    integer :: funit, ppos
    double complex, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (.not. present(pos)) then
        ppos = 1
    else
        ppos = pos
    end if
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <intput_array_4d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(file_exists(filename), &
        ' <input_array_4d> Error: '//tidy(filename)//' does not exist.')
    call assert(get_file_size(filename) >= size(w)*sizeof(w(1, 1, 1, 1)), &
        ' <input_array_4d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1, 1))) &
        //' bytes expected while '//num2str(get_file_size(filename)) &
        //' bytes found. ')
    ! if filename exists and file fize sufficient
    open (newunit=funit, file=tidy(filename), &
        form='unformatted', action='read', access='stream', &
        status='old', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (funit, pos=ppos) tw
        allocate (sorder(1:4))
        call get_integer_digits(store, 4, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (funit, pos=ppos) w
    end if
    close (funit)
end subroutine input_array_4d_dcomplex
subroutine stdin_array_4d_dcomplex(w, store, endian)
    double complex, dimension(:, :, :, :), intent(inout) :: w
    integer, intent(in), optional :: store
    character(len=*), intent(in), optional :: endian
    double complex, allocatable, dimension(:) :: tw
    integer, allocatable, dimension(:) :: sorder
    character(len=24) :: endianness
    if (present(endian)) then
        call assert(tidy(endian) == 'little' .or. tidy(endian) == 'big', &
            ' <stdin_array_4d> Error: endian must be little or big. ')
        endianness = tidy(endian)//'_endian'
    else
        endianness = 'little_endian'
    end if
    call assert(get_stdin_size() >= size(w)*sizeof(w(1, 1, 1, 1)), &
        ' <stdin_array_4d> Error: '//num2str(size(w)*sizeof(w(1, 1, 1, 1))) &
        //' bytes expected while '//num2str(get_stdin_size()) &
        //' bytes found. ')
    close (input_unit)
    open (unit=input_unit, form='unformatted', access='stream', convert=tidy(endianness))
    if (present(store)) then
        ! if data stored in different order
        allocate (tw(1:size(w)))
        read (input_unit, iostat=ioerr) tw
        allocate (sorder(1:4))
        call get_integer_digits(store, 4, sorder)
        w = reshape(tw, shape=shape(w), order=sorder)
        deallocate (tw)
    else
        read (input_unit) w
    end if
end subroutine stdin_array_4d_dcomplex
subroutine output_array_1d_dcomplex(w, filename, append, ascii, format)
    double complex, dimension(:), intent(in) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: append, ascii
    character(len=*), optional :: format
    integer :: funit, i
    logical :: output_ascii
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), access='stream', &
                form='formatted', action='write', &
                status='replace')
        end if
        do i = 1, size(w)
            write (funit, '('//tidy(output_format)//')') w(i)
        end do
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='replace')
        end if
        write (funit) w
    end if
    close (funit)
end subroutine output_array_1d_dcomplex
subroutine stdout_array_1d_dcomplex(w, append, ascii, format)
    double complex, dimension(:), intent(in) :: w
    logical, intent(in), optional :: append, ascii
    character(len=*), optional :: format
    logical :: output_ascii
    integer :: i
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    close (output_unit)
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='formatted', action='write', access='stream')
        end if
        do i = 1, size(w)
            write (output_unit, '('//tidy(output_format)//')') w(i)
        end do
    else
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream')
        end if
        write (output_unit) w
    end if
end subroutine stdout_array_1d_dcomplex
subroutine output_array_2d_dcomplex(w, filename, transp, append, ascii, format)
    double complex, dimension(:, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: transp, append, ascii
    character(len=*), optional :: format
    integer :: funit, i
    logical :: output_ascii
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), access='stream', &
                form='formatted', action='write', &
                status='replace')
        end if
        if (present(transp)) then
            if (transp) then
                do i = 1, size(w, 2)
                    write (funit, '('//num2str(size(w, 1))//tidy(output_format)//')') w(:, i)
                end do
            end if
        else
            do i = 1, size(w, 1)
                write (funit, '('//num2str(size(w, 1))//tidy(output_format)//')') w(i, :)
            end do
        end if
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (newunit=funit, file=tidy(filename), &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='replace')
        end if
        if (present(transp)) then
            if (transp) then
                write (funit) transpose(w)
            end if
        else
            write (funit) w
        end if
    end if
    close (funit)
end subroutine output_array_2d_dcomplex
subroutine stdout_array_2d_dcomplex(w, transp, append, ascii, format)
    double complex, dimension(:, :), intent(in) :: w
    logical, intent(in), optional :: transp, append, ascii
    character(len=*), optional :: format
    logical :: output_ascii
    integer :: i
    character(len=12) :: output_format
    if (present(ascii)) then
        output_ascii = ascii
    else
        output_ascii = .false.
    end if
    if (present(format)) then
        output_format = tidy(format)
    else
        output_format = 'es'
    end if
    close (output_unit)
    if (output_ascii) then
        ! Output ASCII
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='formatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='formatted', action='write', access='stream')
        end if
        if (present(transp)) then
            if (transp) then
                do i = 1, size(w, 2)
                    write (output_unit, '('//num2str(size(w, 1))//'es)') w(:, i)
                end do
            end if
        else
            do i = 1, size(w, 1)
                write (output_unit, '('//num2str(size(w, 1))//'es)') w(i, :)
            end do
        end if
    else
        ! Output binary
        if (present(append)) then
            if (append) then
                open (unit=output_unit, &
                    form='unformatted', action='write', access='stream', &
                    status='old', position='append')
            end if
        else
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream')
        end if
        if (present(transp)) then
            if (transp) then
                write (output_unit) transpose(w)
            end if
        else
            write (output_unit) w
        end if
    end if
end subroutine stdout_array_2d_dcomplex
subroutine output_array_3d_dcomplex(w, filename, store, append)
    double complex, dimension(:, :, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    integer :: funit
    if (present(append)) then
        if (append) then
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='write', access='stream', &
            status='replace')
    end if
    if (present(store)) then
        write (funit) permute(w, store)
    else
        write (funit) w
    end if
    close (funit)
end subroutine output_array_3d_dcomplex
subroutine stdout_array_3d_dcomplex(w, store, append)
    double complex, dimension(:, :, :), intent(in) :: w
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    close (output_unit)
    if (present(append)) then
        if (append) then
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (unit=output_unit, &
            form='unformatted', action='write', access='stream')
    end if
    if (present(store)) then
        write (output_unit) permute(w, store)
    else
        write (output_unit) w
    end if
end subroutine stdout_array_3d_dcomplex
subroutine output_array_4d_dcomplex(w, filename, store, append)
    double complex, dimension(:, :, :, :), intent(in) :: w
    character(len=*), intent(in) :: filename
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    integer :: funit
    if (present(append)) then
        if (append) then
            open (newunit=funit, file=tidy(filename), &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='write', access='stream', &
            status='replace')
    end if
    if (present(store)) then
        write (funit) permute(w, store)
    else
        write (funit) w
    end if
    close (funit)
end subroutine output_array_4d_dcomplex
subroutine stdout_array_4d_dcomplex(w, store, append)
    double complex, dimension(:, :, :, :), intent(in) :: w
    integer, intent(in), optional :: store
    logical, intent(in), optional :: append
    close (output_unit)
    if (present(append)) then
        if (append) then
            open (unit=output_unit, &
                form='unformatted', action='write', access='stream', &
                status='old', position='append')
        end if
    else
        open (unit=output_unit, &
            form='unformatted', action='write', access='stream')
    end if
    if (present(store)) then
        write (output_unit) permute(w, store)
    else
        write (output_unit) w
    end if
end subroutine stdout_array_4d_dcomplex
    function load_array_1d_float(filename, n1, ascii, pos, endian) result(w)
        integer, intent(in) :: n1
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: ascii
        integer, intent(in), optional :: pos
        character(len=*), intent(in), optional :: endian
        real, allocatable, dimension(:) :: w
        integer :: funit, fpos, i
        character(len=24) :: endianness
        logical :: file_ascii
        integer :: target_size
        if (present(ascii)) then
            file_ascii = ascii
        else
            file_ascii = .false.
        end if
        if (.not. present(pos)) then
            fpos = 1
        else
            fpos = pos
        end if
        if (.not. file_ascii) then
            if (.not. present(endian)) then
                endianness = 'little_endian'
            else
                endianness = tidy(endian)//'_endian'
                if (tidy(endianness) /= 'little_endian' &
                        .and. tidy(endianness) /= 'big_endian') then
                    endianness = 'little_endian'
                end if
            end if
        end if
        target_size = n1*4
        if (file_ascii) then
            ! ASCII file
            if (.not. file_exists(filename)) then
                ! if filename does not exist
                call warn(' <load_array_1d_float> Error: '//tidy(filename)//' does not exist.')
                stop
            else
                w = zeros(n1)
                open (newunit=funit, file=tidy(filename), action='read', status='old')
                do i = 1, n1
                    if (i >= fpos) then
                        read (funit, *) w(i)
                    end if
                end do
                close(funit)
            end if
        else
            ! If binary file
            if (.not. file_exists(filename)) then
                ! if filename does not exist
                call warn(' <load_array_1d_float> Error: '//tidy(filename)//' does not exist.')
                stop
            else
                if (get_file_size(filename) < target_size) then
                    ! if file size insufficient
                    call warn(' <load_array_1d_float> Error: '//num2str(get_file_size(filename)) &
                        //' bytes found while '//num2str(target_size) &
                        //' bytes required.')
                    stop
                else
                    ! if filename exists and file fize sufficient
                    open (newunit=funit, file=tidy(filename), &
                        form='unformatted', action='read', access='stream', &
                        status='old', convert=tidy(endianness))
                    w = zeros(n1)
                    read (funit, pos=fpos) w
                    close (funit)
                end if
            end if
        end if
    end function load_array_1d_float
    function load_array_2d_float(filename, n1, n2, transp, ascii, pos, endian) result(w)
        integer, intent(in) :: n1, n2
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: transp, ascii
        integer, intent(in), optional :: pos
        character(len=*), intent(in), optional :: endian
        real, allocatable, dimension(:, :) :: w
        integer :: funit, fpos, i
        character(len=24) :: endianness
        logical :: file_ascii
        integer :: target_size
        if (present(ascii)) then
            file_ascii = ascii
        else
            file_ascii = .false.
        end if
        if (.not. present(pos)) then
            fpos = 1
        else
            fpos = pos
        end if
        if (.not. file_ascii) then
            if (.not. present(endian)) then
                endianness = 'little_endian'
            else
                endianness = tidy(endian)//'_endian'
                if (tidy(endianness) /= 'little_endian' &
                        .and. tidy(endianness) /= 'big_endian') then
                    endianness = 'little_endian'
                end if
            end if
        end if
        target_size = n1*n2*4
        if (file_ascii) then
            ! ASCII file
            if (.not. file_exists(filename)) then
                ! if filename does not exist
                call warn(' <load_array_2d_float> Error: '//tidy(filename)//' does not exist.')
                stop
            else
                w = zeros(n1, n2)
                open (newunit=funit, file=tidy(filename), action='read', status='old')
                do i = 1, n1
                    if (i >= fpos) then
                        read (funit, *) w(i, :)
                    end if
                end do
                close(funit)
            end if
        else
            ! If binary file
            if (.not. file_exists(filename)) then
                ! if filename does not exist
                call warn(' <load_array_2d_float> Error: '//tidy(filename)//' does not exist.')
                stop
            else
                if (get_file_size(filename) < target_size) then
                    ! if file size insufficient
                    call warn(' <load_array_2d_float> Error: '//num2str(get_file_size(filename)) &
                        //' bytes found while '//num2str(target_size) &
                        //' bytes required.')
                    stop
                else
                    ! if filename exists and file fize sufficient
                    open (newunit=funit, file=tidy(filename), &
                        form='unformatted', action='read', access='stream', &
                        status='old', convert=tidy(endianness))
                    if (present(transp)) then
                        if (transp) then
                            ! if data stored in different order
                            w = zeros(n2, n1)
                            read (funit, pos=fpos) w
                            w = transpose(w)
                        end if
                    else
                        w = zeros(n1, n2)
                        read (funit, pos=fpos) w
                    end if
                    close (funit)
                end if
            end if
        end if
    end function load_array_2d_float
    function load_array_3d_float(filename, n1, n2, n3, store, ascii, pos, endian) result(w)
        integer, intent(in) :: n1, n2, n3
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: store
        logical, intent(in), optional :: ascii
        integer, intent(in), optional :: pos
        character(len=*), intent(in), optional :: endian
        real, allocatable, dimension(:, :, :) :: w
        real, allocatable, dimension(:) :: ww
        integer, allocatable, dimension(:) :: sorder
        integer :: funit, fpos, i, j, l
        character(len=24) :: endianness
        logical :: file_ascii
        integer :: target_size
        if (present(ascii)) then
            file_ascii = ascii
        else
            file_ascii = .false.
        end if
        if (.not. present(pos)) then
            fpos = 1
        else
            fpos = pos
        end if
        if (.not. file_ascii) then
            if (.not. present(endian)) then
                endianness = 'little_endian'
            else
                endianness = tidy(endian)//'_endian'
                if (tidy(endianness) /= 'little_endian' &
                        .and. tidy(endianness) /= 'big_endian') then
                    endianness = 'little_endian'
                end if
            end if
        end if
        target_size = n1*n2*n3*4
        if (file_ascii) then
            ! ASCII file
            if (.not. file_exists(filename)) then
                ! if filename does not exist
                call warn(' <load_array_3d_float> Error: '//tidy(filename)//' does not exist.')
                stop
            else
                w = zeros(n1, n2, n3)
                open (newunit=funit, file=tidy(filename), action='read', status='old')
                l = 1
                do i = 1, n1
                    do j = 1, n2
                        if (l >= fpos) then
                            read (funit, *) w(i, j, :)
                            l = l + 1
                        end if
                    end do
                end do
                close(funit)
            end if
        else
            ! If binary file
            if (.not. file_exists(filename)) then
                ! if filename does not exist
                call warn(' <load_array_3d_float> Error: '//tidy(filename)//' does not exist.')
                stop
            else
                if (get_file_size(filename) < target_size) then
                    ! if file size insufficient
                    call warn(' <load_array_3d_float> Error: '//num2str(get_file_size(filename)) &
                        //' bytes found while '//num2str(target_size) &
                        //' bytes required.')
                    stop
                else
                    ! if filename exists and file fize sufficient
                    open (newunit=funit, file=tidy(filename), &
                        form='unformatted', action='read', access='stream', &
                        status='old', convert=tidy(endianness))
                    if (present(store)) then
                        ! if data stored in different order
                        allocate (ww(1:n1*n2*n3))
                        read (funit, pos=fpos) ww
                        allocate (sorder(1:3))
                        call get_integer_digits(store, 3, sorder)
                        w = reshape(ww, shape=[n1, n2, n3], order=sorder)
                    else
                        w = zeros(n1, n2, n3)
                        read (funit, pos=fpos) w
                    end if
                    close (funit)
                end if
            end if
        end if
    end function load_array_3d_float
end module libflit_io
