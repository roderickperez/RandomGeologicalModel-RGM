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
submodule(filesystem) find_filesystem
    !! procedures that find files
    implicit none
contains
    module procedure get_filename
    character(:), allocatable :: path1
    character(4), parameter :: suff(3) = [character(4) :: '.h5', '.nc', '.dat']
    integer :: i
    allocate (character(get_max_path()) :: get_filename)
    get_filename = path
    !! avoid undefined return
    if (len(path) == 0) return
    if (present(name)) then
        if (index(path, name, back=.true.) == 0) then
            !> assume we wish to append stem to path
            get_filename = path//'/'//name
        elseif (index(get_filename, '.', back=.true.) > 4) then
            !> it's a stem-matching full path with a suffix
            if (.not. is_file(get_filename)) get_filename = ''
            return
        end if
    end if
    if (is_file(get_filename)) return
    allocate (character(get_max_path()) :: path1)
    path1 = get_filename
    do i = 1, size(suff)
        get_filename = path1//trim(suff(i))
        if (is_file(get_filename)) return
    end do
    get_filename = ''
    if (present(name)) then
        write (stderr, *) 'filesystem:get_filename: ', name, ' not found in ', path
    else
        write (stderr, *) 'filesystem:get_filename: file not found: ', path
    end if
    end procedure get_filename
end submodule find_filesystem
