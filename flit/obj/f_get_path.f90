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
submodule(filesystem) get_path_smod
    use, intrinsic :: iso_c_binding, only: C_CHAR, C_SIZE_T
    implicit none
    interface !< get_path.c
        integer(C_SIZE_T) function max_path() bind(C, name="fs_get_maxp")
            import
        end function
        integer(C_SIZE_T) function fs_lib_path(path, buffer_size) bind(C)
            import
            character(kind=C_CHAR), intent(out) :: path(*)
            integer(C_SIZE_T), intent(in), value :: buffer_size
        end function
        integer(C_SIZE_T) function fs_exe_path(path, buffer_size) bind(C)
            import
            character(kind=C_CHAR), intent(out) :: path(*)
            integer(C_SIZE_T), intent(in), value :: buffer_size
        end function
    end interface
contains
    module procedure exe_path
    character(kind=C_CHAR, len=:), allocatable :: cbuf
    integer(C_SIZE_T) :: N
    allocate (character(max_path()) :: cbuf)
    N = fs_exe_path(cbuf, len(cbuf, kind=C_SIZE_T))
    allocate (character(N) :: exe_path)
    exe_path = cbuf(:N)
    end procedure
    module procedure lib_path
    character(kind=C_CHAR, len=:), allocatable :: cbuf
    integer(C_SIZE_T) :: N
    allocate (character(max_path()) :: cbuf)
    N = fs_lib_path(cbuf, len(cbuf, kind=C_SIZE_T))
    allocate (character(N) :: lib_path)
    lib_path = cbuf(:N)
    end procedure
end submodule get_path_smod
