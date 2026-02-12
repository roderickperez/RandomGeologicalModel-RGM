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
module libflit_readpar
    use libflit_string
    use libflit_utility
    use libflit_io
    use iso_fortran_env
    implicit none
    private
    integer, parameter :: nstring_max = 100
    ! To improve readness of an end-user code,
    ! here I still make the subroutines publicly accessible
    interface readpar_int
        module procedure :: readpar_int2
        module procedure :: readpar_int4
        module procedure :: readpar_int8
    end interface
    interface readpar_nint
        module procedure :: readnpar_int2
        module procedure :: readnpar_int4
        module procedure :: readnpar_int8
    end interface readpar_nint
    interface readpar_xint
        module procedure :: readxpar_int2
        module procedure :: readxpar_int4
        module procedure :: readxpar_int8
    end interface
    interface readpar_nfloat
        module procedure :: readnpar_float
    end interface
    interface readpar_xfloat
        module procedure :: readxpar_float
    end interface
    interface readpar_ndouble
        module procedure :: readnpar_double
    end interface
    interface readpar_xdouble
        module procedure :: readxpar_double
    end interface
    interface readpar_ncomplex
        module procedure :: readnpar_complex
    end interface
    interface readpar_xcomplex
        module procedure :: readxpar_complex
    end interface
    interface readpar_ndcomplex
        module procedure :: readnpar_dcomplex
    end interface
    interface readpar_xdcomplex
        module procedure :: readxpar_dcomplex
    end interface
    interface readpar_nlogical
        module procedure :: readnpar_logical
    end interface
    interface readpar_xlogical
        module procedure :: readxpar_logical
    end interface
    interface readpar_nstring
        module procedure :: readnpar_string
    end interface
    interface readpar_xstring
        module procedure :: readxpar_string
    end interface
    interface getpar_int
        module procedure :: getpar_int2
        module procedure :: getpar_int4
        module procedure :: getpar_int8
    end interface
    interface getpar_nint
        module procedure :: getnpar_int2
        module procedure :: getnpar_int4
        module procedure :: getnpar_int8
    end interface getpar_nint
    interface getpar_nfloat
        module procedure :: getnpar_float
    end interface
    interface getpar_ndouble
        module procedure :: getnpar_double
    end interface
    interface getpar_ncomplex
        module procedure :: getnpar_complex
    end interface
    interface getpar_ndcomplex
        module procedure :: getnpar_dcomplex
    end interface
    interface getpar_nlogical
        module procedure :: getnpar_logical
    end interface
    interface getpar_nstring
        module procedure :: getnpar_string
    end interface
    interface getpar_xint
        module procedure :: getxpar_int2
        module procedure :: getxpar_int4
        module procedure :: getxpar_int8
    end interface getpar_xint
    interface getpar_xfloat
        module procedure :: getxpar_float
    end interface
    interface getpar_xdouble
        module procedure :: getxpar_double
    end interface
    interface getpar_xcomplex
        module procedure :: getxpar_complex
    end interface
    interface getpar_xdcomplex
        module procedure :: getxpar_dcomplex
    end interface
    interface getpar_xlogical
        module procedure :: getxpar_logical
    end interface
    interface getpar_xstring
        module procedure :: getxpar_string
    end interface
    interface parsepar_int
        module procedure :: parsepar_int2
        module procedure :: parsepar_int4
        module procedure :: parsepar_int8
    end interface
    interface parsepar_nint
        module procedure :: parsenpar_int2
        module procedure :: parsenpar_int4
        module procedure :: parsenpar_int8
    end interface parsepar_nint
    interface parsepar_nfloat
        module procedure :: parsenpar_float
    end interface
    interface parsepar_ndouble
        module procedure :: parsenpar_double
    end interface
    interface parsepar_ncomplex
        module procedure :: parsenpar_complex
    end interface
    interface parsepar_ndcomplex
        module procedure :: parsenpar_dcomplex
    end interface
    interface parsepar_nlogical
        module procedure :: parsenpar_logical
    end interface
    interface parsepar_nstring
        module procedure :: parsenpar_string
    end interface
    interface parsepar_xint
        module procedure :: parsexpar_int2
        module procedure :: parsexpar_int4
        module procedure :: parsexpar_int8
    end interface parsepar_xint
    interface parsepar_xfloat
        module procedure :: parsexpar_float
    end interface
    interface parsepar_xdouble
        module procedure :: parsexpar_double
    end interface
    interface parsepar_xcomplex
        module procedure :: parsexpar_complex
    end interface
    interface parsepar_xdcomplex
        module procedure :: parsexpar_dcomplex
    end interface
    interface parsepar_xlogical
        module procedure :: parsexpar_logical
    end interface
    interface parsepar_xstring
        module procedure :: parsexpar_string
    end interface
    public :: readpar_int
    public :: readpar_nint
    public :: readpar_xint
    public :: readpar_float
    public :: readpar_nfloat
    public :: readpar_xfloat
    public :: readpar_double
    public :: readpar_ndouble
    public :: readpar_xdouble
    public :: readpar_complex
    public :: readpar_ncomplex
    public :: readpar_xcomplex
    public :: readpar_dcomplex
    public :: readpar_ndcomplex
    public :: readpar_xdcomplex
    public :: readpar_logical
    public :: readpar_nlogical
    public :: readpar_xlogical
    public :: readpar_string
    public :: readpar_nstring
    public :: readpar_xstring
    public :: getpar_int
    public :: getpar_nint
    public :: getpar_xint
    public :: getpar_float
    public :: getpar_nfloat
    public :: getpar_xfloat
    public :: getpar_double
    public :: getpar_ndouble
    public :: getpar_xdouble
    public :: getpar_complex
    public :: getpar_ncomplex
    public :: getpar_xcomplex
    public :: getpar_ndcomplex
    public :: getpar_xdcomplex
    public :: getpar_logical
    public :: getpar_nlogical
    public :: getpar_xlogical
    public :: getpar_string
    public :: getpar_nstring
    public :: getpar_xstring
    public :: parsepar_int
    public :: parsepar_nint
    public :: parsepar_xint
    public :: parsepar_float
    public :: parsepar_nfloat
    public :: parsepar_xfloat
    public :: parsepar_double
    public :: parsepar_ndouble
    public :: parsepar_xdouble
    public :: parsepar_complex
    public :: parsepar_ncomplex
    public :: parsepar_xcomplex
    public :: parsepar_ndcomplex
    public :: parsepar_xdcomplex
    public :: parsepar_logical
    public :: parsepar_nlogical
    public :: parsepar_xlogical
    public :: parsepar_string
    public :: parsepar_nstring
    public :: parsepar_xstring
    public :: checkpar
    public :: checkarg
contains
    !
    !> Check if a parameter file contains any invalid parameter
    !
    subroutine checkpar(filename, valid_parnames)
        character(len=*), intent(in) :: filename
        character(len=*), dimension(:), intent(in) :: valid_parnames
        character(len=1024) :: line
        integer :: funit, eqindex
        integer :: ioerr
        logical :: invalid
        open (newunit=funit, file=tidy(filename), status='old', action='read')
        do
            read (funit, '(a)', iostat=ioerr) line
            if (ioerr /= 0) exit
            if (tidy(line) == 'exit') exit
            if (len(tidy(line)) > 0) then
                eqindex = index(line, '=')
                if (eqindex >= 1 .and. .not. any(to_lower(tidy(line(1:eqindex - 1))) == valid_parnames)) then
                    write (error_unit, *) ' Invalid parameter: '//to_lower(tidy(line(1:eqindex - 1)))
                    invalid = .true.
                end if
            end if
        end do
        close (funit)
        if (invalid) then
            write (error_unit, *) ' Parameter file contains invalid parameters. Exit.'
            stop
        end if
    end subroutine checkpar
    !
    !> Check if a list of command-line arguments contains any invalid parameter
    !
    subroutine checkarg(valid_parnames)
        character(len=*), dimension(:), intent(in) :: valid_parnames
        integer :: i
        integer :: len_arg, eqindex
        character(len=256) :: arg
        logical :: invalid
        invalid = .false.
        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            len_arg = len_trim(arg)
            if (len_arg > 1) then
                eqindex = index(arg, '=')
                if (.not. any(tidy(arg(1:eqindex - 1)) == valid_parnames)) then
                    write(error_unit, *) ' Invalid argument: '//tidy(arg(1:eqindex - 1))
                    invalid = .true.
                end if
            end if
        end do
        if(invalid) then
            write(error_unit, *) ' Argument list contains invalid arguments. Exit.'
            stop
        end if
    end subroutine checkarg
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
!> Read a parameter from file by name, e.g.,
!> parname = 10
!
subroutine readpar_int2(filename, parname, par, default_value, required)
    character(len=*), intent(in) :: filename, parname
    integer(kind=2), intent(inout) :: par
    integer(kind=2), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    integer :: ioerr
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                par = extract_int2(tidy(line(eqindex + 1:)))
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readpar_int2
!
!> Read a parameter from file by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine readnpar_int2(filename, parname, par, default_value, required, separator)
    character(len=*), intent(in) :: filename, parname
    integer(kind=2), allocatable, dimension(:), intent(inout) :: par
    integer(kind=2), dimension(:), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=*), intent(in), optional :: separator
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    character(len=10) :: sepr
    integer :: ioerr
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nint2(tidy(line(eqindex + 1:)), trim(adjustl(sepr)), par)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                if (allocated(par)) then
                    deallocate (par)
                else
                    par = default_value
                end if
            end if
        else
            ! if not required then set to default value
            if (allocated(par)) then
                deallocate (par)
            end if
            par = default_value
        end if
    end if
end subroutine readnpar_int2
!
!> Read a parameter from file using the format
!> parname = range1:value1, range2:value2, ...
!> e.g., parname = 1~10:0.0, 11~20:1.0, 21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine readxpar_int2(filename, parname, par, default_value, var, required)
    character(len=*), intent(in) :: filename, parname
    integer(kind=2), intent(inout) :: par
    integer(kind=2), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    integer(kind=2), allocatable, dimension(:) :: rpar
    character(len=1024) :: line
    integer :: npar, funit, eqindex, i, l, nr
    integer :: ioerr
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nstring(tidy(line(eqindex + 1:)), ',', tmpar)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar >= 1) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xint2(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readxpar_int2
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10
!
subroutine getpar_int2(parname, par, default_value, required)
    character(len=*), intent(in) :: parname
    integer(kind=2), intent(out) :: par
    integer(kind=2), intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                par = extract_int2(arg(len_par + 2:len_arg))
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getpar_int2
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine getnpar_int2(parname, par, default_value, separator, required)
    character(len=*), intent(in) :: parname
    integer(kind=2), allocatable, dimension(:), intent(out) :: par
    integer(kind=2), dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    getpar_success = .false.
    len_par = len_trim(parname)
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nint2(arg(len_par + 2:len_arg), trim(adjustl(sepr)), par)
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getnpar_int2
!
!> Read a parameter from command line arguments using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine getxpar_int2(parname, par, default_value, var, required)
    character(len=*), intent(in) :: parname
    integer(kind=2), intent(inout) :: par
    integer(kind=2), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    integer(kind=2), allocatable, dimension(:) :: rpar
    integer :: i, l, nr
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nstring(tidy(arg(len_par + 2:)), ',', tmpar)
                getpar_success = .true.
            end if
        end if
    end do
    if (getpar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xint2(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getxpar_int2
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10
!
subroutine parsepar_int2(source, parname, par, default_value, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    integer(kind=2), intent(out) :: par
    integer(kind=2), intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            par = extract_int2(tidy(arg(eqindex + 1:)))
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsepar_int2
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine parsenpar_int2(source, parname, par, default_value, separator, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    integer(kind=2), allocatable, dimension(:), intent(out) :: par
    integer(kind=2), dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    parsepar_success = .false.
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nint2(tidy(arg(eqindex + 1:)), trim(adjustl(sepr)), par)
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsenpar_int2
!
!> Read a parameter from a string using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine parsexpar_int2(source, parname, par, default_value, var, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    integer(kind=2), allocatable, dimension(:), intent(out) :: par
    integer(kind=2), dimension(:), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    integer(kind=2), allocatable, dimension(:) :: rpar
    integer :: i, j, l, nr, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nstring(tidy(arg(eqindex + 1:)), ',', tmpar)
            parsepar_success = .true.
        end if
    end do
    if (parsepar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xint2(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsexpar_int2
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
!> Read a parameter from file by name, e.g.,
!> parname = 10
!
subroutine readpar_int4(filename, parname, par, default_value, required)
    character(len=*), intent(in) :: filename, parname
    integer(kind=4), intent(inout) :: par
    integer(kind=4), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    integer :: ioerr
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                par = extract_int4(tidy(line(eqindex + 1:)))
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readpar_int4
!
!> Read a parameter from file by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine readnpar_int4(filename, parname, par, default_value, required, separator)
    character(len=*), intent(in) :: filename, parname
    integer(kind=4), allocatable, dimension(:), intent(inout) :: par
    integer(kind=4), dimension(:), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=*), intent(in), optional :: separator
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    character(len=10) :: sepr
    integer :: ioerr
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nint4(tidy(line(eqindex + 1:)), trim(adjustl(sepr)), par)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                if (allocated(par)) then
                    deallocate (par)
                else
                    par = default_value
                end if
            end if
        else
            ! if not required then set to default value
            if (allocated(par)) then
                deallocate (par)
            end if
            par = default_value
        end if
    end if
end subroutine readnpar_int4
!
!> Read a parameter from file using the format
!> parname = range1:value1, range2:value2, ...
!> e.g., parname = 1~10:0.0, 11~20:1.0, 21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine readxpar_int4(filename, parname, par, default_value, var, required)
    character(len=*), intent(in) :: filename, parname
    integer(kind=4), intent(inout) :: par
    integer(kind=4), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    integer(kind=4), allocatable, dimension(:) :: rpar
    character(len=1024) :: line
    integer :: npar, funit, eqindex, i, l, nr
    integer :: ioerr
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nstring(tidy(line(eqindex + 1:)), ',', tmpar)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar >= 1) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xint4(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readxpar_int4
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10
!
subroutine getpar_int4(parname, par, default_value, required)
    character(len=*), intent(in) :: parname
    integer(kind=4), intent(out) :: par
    integer(kind=4), intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                par = extract_int4(arg(len_par + 2:len_arg))
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getpar_int4
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine getnpar_int4(parname, par, default_value, separator, required)
    character(len=*), intent(in) :: parname
    integer(kind=4), allocatable, dimension(:), intent(out) :: par
    integer(kind=4), dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    getpar_success = .false.
    len_par = len_trim(parname)
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nint4(arg(len_par + 2:len_arg), trim(adjustl(sepr)), par)
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getnpar_int4
!
!> Read a parameter from command line arguments using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine getxpar_int4(parname, par, default_value, var, required)
    character(len=*), intent(in) :: parname
    integer(kind=4), intent(inout) :: par
    integer(kind=4), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    integer(kind=4), allocatable, dimension(:) :: rpar
    integer :: i, l, nr
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nstring(tidy(arg(len_par + 2:)), ',', tmpar)
                getpar_success = .true.
            end if
        end if
    end do
    if (getpar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xint4(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getxpar_int4
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10
!
subroutine parsepar_int4(source, parname, par, default_value, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    integer(kind=4), intent(out) :: par
    integer(kind=4), intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            par = extract_int4(tidy(arg(eqindex + 1:)))
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsepar_int4
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine parsenpar_int4(source, parname, par, default_value, separator, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    integer(kind=4), allocatable, dimension(:), intent(out) :: par
    integer(kind=4), dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    parsepar_success = .false.
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nint4(tidy(arg(eqindex + 1:)), trim(adjustl(sepr)), par)
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsenpar_int4
!
!> Read a parameter from a string using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine parsexpar_int4(source, parname, par, default_value, var, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    integer(kind=4), allocatable, dimension(:), intent(out) :: par
    integer(kind=4), dimension(:), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    integer(kind=4), allocatable, dimension(:) :: rpar
    integer :: i, j, l, nr, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nstring(tidy(arg(eqindex + 1:)), ',', tmpar)
            parsepar_success = .true.
        end if
    end do
    if (parsepar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xint4(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsexpar_int4
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
!> Read a parameter from file by name, e.g.,
!> parname = 10
!
subroutine readpar_int8(filename, parname, par, default_value, required)
    character(len=*), intent(in) :: filename, parname
    integer(kind=8), intent(inout) :: par
    integer(kind=8), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    integer :: ioerr
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                par = extract_int8(tidy(line(eqindex + 1:)))
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readpar_int8
!
!> Read a parameter from file by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine readnpar_int8(filename, parname, par, default_value, required, separator)
    character(len=*), intent(in) :: filename, parname
    integer(kind=8), allocatable, dimension(:), intent(inout) :: par
    integer(kind=8), dimension(:), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=*), intent(in), optional :: separator
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    character(len=10) :: sepr
    integer :: ioerr
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nint8(tidy(line(eqindex + 1:)), trim(adjustl(sepr)), par)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                if (allocated(par)) then
                    deallocate (par)
                else
                    par = default_value
                end if
            end if
        else
            ! if not required then set to default value
            if (allocated(par)) then
                deallocate (par)
            end if
            par = default_value
        end if
    end if
end subroutine readnpar_int8
!
!> Read a parameter from file using the format
!> parname = range1:value1, range2:value2, ...
!> e.g., parname = 1~10:0.0, 11~20:1.0, 21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine readxpar_int8(filename, parname, par, default_value, var, required)
    character(len=*), intent(in) :: filename, parname
    integer(kind=8), intent(inout) :: par
    integer(kind=8), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    integer(kind=8), allocatable, dimension(:) :: rpar
    character(len=1024) :: line
    integer :: npar, funit, eqindex, i, l, nr
    integer :: ioerr
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nstring(tidy(line(eqindex + 1:)), ',', tmpar)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar >= 1) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xint8(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readxpar_int8
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10
!
subroutine getpar_int8(parname, par, default_value, required)
    character(len=*), intent(in) :: parname
    integer(kind=8), intent(out) :: par
    integer(kind=8), intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                par = extract_int8(arg(len_par + 2:len_arg))
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getpar_int8
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine getnpar_int8(parname, par, default_value, separator, required)
    character(len=*), intent(in) :: parname
    integer(kind=8), allocatable, dimension(:), intent(out) :: par
    integer(kind=8), dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    getpar_success = .false.
    len_par = len_trim(parname)
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nint8(arg(len_par + 2:len_arg), trim(adjustl(sepr)), par)
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getnpar_int8
!
!> Read a parameter from command line arguments using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine getxpar_int8(parname, par, default_value, var, required)
    character(len=*), intent(in) :: parname
    integer(kind=8), intent(inout) :: par
    integer(kind=8), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    integer(kind=8), allocatable, dimension(:) :: rpar
    integer :: i, l, nr
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nstring(tidy(arg(len_par + 2:)), ',', tmpar)
                getpar_success = .true.
            end if
        end if
    end do
    if (getpar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xint8(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getxpar_int8
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10
!
subroutine parsepar_int8(source, parname, par, default_value, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    integer(kind=8), intent(out) :: par
    integer(kind=8), intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            par = extract_int8(tidy(arg(eqindex + 1:)))
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsepar_int8
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine parsenpar_int8(source, parname, par, default_value, separator, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    integer(kind=8), allocatable, dimension(:), intent(out) :: par
    integer(kind=8), dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    parsepar_success = .false.
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nint8(tidy(arg(eqindex + 1:)), trim(adjustl(sepr)), par)
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsenpar_int8
!
!> Read a parameter from a string using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine parsexpar_int8(source, parname, par, default_value, var, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    integer(kind=8), allocatable, dimension(:), intent(out) :: par
    integer(kind=8), dimension(:), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    integer(kind=8), allocatable, dimension(:) :: rpar
    integer :: i, j, l, nr, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nstring(tidy(arg(eqindex + 1:)), ',', tmpar)
            parsepar_success = .true.
        end if
    end do
    if (parsepar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xint8(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsexpar_int8
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
!> Read a parameter from file by name, e.g.,
!> parname = 10
!
subroutine readpar_float(filename, parname, par, default_value, required)
    character(len=*), intent(in) :: filename, parname
    real, intent(inout) :: par
    real, intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    integer :: ioerr
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                par = extract_float(tidy(line(eqindex + 1:)))
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readpar_float
!
!> Read a parameter from file by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine readnpar_float(filename, parname, par, default_value, required, separator)
    character(len=*), intent(in) :: filename, parname
    real, allocatable, dimension(:), intent(inout) :: par
    real, dimension(:), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=*), intent(in), optional :: separator
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    character(len=10) :: sepr
    integer :: ioerr
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nfloat(tidy(line(eqindex + 1:)), trim(adjustl(sepr)), par)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                if (allocated(par)) then
                    deallocate (par)
                else
                    par = default_value
                end if
            end if
        else
            ! if not required then set to default value
            if (allocated(par)) then
                deallocate (par)
            end if
            par = default_value
        end if
    end if
end subroutine readnpar_float
!
!> Read a parameter from file using the format
!> parname = range1:value1, range2:value2, ...
!> e.g., parname = 1~10:0.0, 11~20:1.0, 21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine readxpar_float(filename, parname, par, default_value, var, required)
    character(len=*), intent(in) :: filename, parname
    real, intent(inout) :: par
    real, intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    real, allocatable, dimension(:) :: rpar
    character(len=1024) :: line
    integer :: npar, funit, eqindex, i, l, nr
    integer :: ioerr
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nstring(tidy(line(eqindex + 1:)), ',', tmpar)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar >= 1) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xfloat(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readxpar_float
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10
!
subroutine getpar_float(parname, par, default_value, required)
    character(len=*), intent(in) :: parname
    real, intent(out) :: par
    real, intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                par = extract_float(arg(len_par + 2:len_arg))
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getpar_float
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine getnpar_float(parname, par, default_value, separator, required)
    character(len=*), intent(in) :: parname
    real, allocatable, dimension(:), intent(out) :: par
    real, dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    getpar_success = .false.
    len_par = len_trim(parname)
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nfloat(arg(len_par + 2:len_arg), trim(adjustl(sepr)), par)
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getnpar_float
!
!> Read a parameter from command line arguments using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine getxpar_float(parname, par, default_value, var, required)
    character(len=*), intent(in) :: parname
    real, intent(inout) :: par
    real, intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    real, allocatable, dimension(:) :: rpar
    integer :: i, l, nr
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nstring(tidy(arg(len_par + 2:)), ',', tmpar)
                getpar_success = .true.
            end if
        end if
    end do
    if (getpar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xfloat(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getxpar_float
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10
!
subroutine parsepar_float(source, parname, par, default_value, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    real, intent(out) :: par
    real, intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            par = extract_float(tidy(arg(eqindex + 1:)))
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsepar_float
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine parsenpar_float(source, parname, par, default_value, separator, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    real, allocatable, dimension(:), intent(out) :: par
    real, dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    parsepar_success = .false.
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nfloat(tidy(arg(eqindex + 1:)), trim(adjustl(sepr)), par)
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsenpar_float
!
!> Read a parameter from a string using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine parsexpar_float(source, parname, par, default_value, var, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    real, allocatable, dimension(:), intent(out) :: par
    real, dimension(:), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    real, allocatable, dimension(:) :: rpar
    integer :: i, j, l, nr, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nstring(tidy(arg(eqindex + 1:)), ',', tmpar)
            parsepar_success = .true.
        end if
    end do
    if (parsepar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xfloat(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsexpar_float
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
!> Read a parameter from file by name, e.g.,
!> parname = 10
!
subroutine readpar_double(filename, parname, par, default_value, required)
    character(len=*), intent(in) :: filename, parname
    double precision, intent(inout) :: par
    double precision, intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    integer :: ioerr
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                par = extract_double(tidy(line(eqindex + 1:)))
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readpar_double
!
!> Read a parameter from file by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine readnpar_double(filename, parname, par, default_value, required, separator)
    character(len=*), intent(in) :: filename, parname
    double precision, allocatable, dimension(:), intent(inout) :: par
    double precision, dimension(:), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=*), intent(in), optional :: separator
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    character(len=10) :: sepr
    integer :: ioerr
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_ndouble(tidy(line(eqindex + 1:)), trim(adjustl(sepr)), par)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                if (allocated(par)) then
                    deallocate (par)
                else
                    par = default_value
                end if
            end if
        else
            ! if not required then set to default value
            if (allocated(par)) then
                deallocate (par)
            end if
            par = default_value
        end if
    end if
end subroutine readnpar_double
!
!> Read a parameter from file using the format
!> parname = range1:value1, range2:value2, ...
!> e.g., parname = 1~10:0.0, 11~20:1.0, 21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine readxpar_double(filename, parname, par, default_value, var, required)
    character(len=*), intent(in) :: filename, parname
    double precision, intent(inout) :: par
    double precision, intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    double precision, allocatable, dimension(:) :: rpar
    character(len=1024) :: line
    integer :: npar, funit, eqindex, i, l, nr
    integer :: ioerr
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nstring(tidy(line(eqindex + 1:)), ',', tmpar)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar >= 1) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xdouble(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readxpar_double
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10
!
subroutine getpar_double(parname, par, default_value, required)
    character(len=*), intent(in) :: parname
    double precision, intent(out) :: par
    double precision, intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                par = extract_double(arg(len_par + 2:len_arg))
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getpar_double
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine getnpar_double(parname, par, default_value, separator, required)
    character(len=*), intent(in) :: parname
    double precision, allocatable, dimension(:), intent(out) :: par
    double precision, dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    getpar_success = .false.
    len_par = len_trim(parname)
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_ndouble(arg(len_par + 2:len_arg), trim(adjustl(sepr)), par)
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getnpar_double
!
!> Read a parameter from command line arguments using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine getxpar_double(parname, par, default_value, var, required)
    character(len=*), intent(in) :: parname
    double precision, intent(inout) :: par
    double precision, intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    double precision, allocatable, dimension(:) :: rpar
    integer :: i, l, nr
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nstring(tidy(arg(len_par + 2:)), ',', tmpar)
                getpar_success = .true.
            end if
        end if
    end do
    if (getpar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xdouble(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getxpar_double
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10
!
subroutine parsepar_double(source, parname, par, default_value, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    double precision, intent(out) :: par
    double precision, intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            par = extract_double(tidy(arg(eqindex + 1:)))
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsepar_double
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine parsenpar_double(source, parname, par, default_value, separator, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    double precision, allocatable, dimension(:), intent(out) :: par
    double precision, dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    parsepar_success = .false.
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_ndouble(tidy(arg(eqindex + 1:)), trim(adjustl(sepr)), par)
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsenpar_double
!
!> Read a parameter from a string using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine parsexpar_double(source, parname, par, default_value, var, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    double precision, allocatable, dimension(:), intent(out) :: par
    double precision, dimension(:), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    double precision, allocatable, dimension(:) :: rpar
    integer :: i, j, l, nr, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nstring(tidy(arg(eqindex + 1:)), ',', tmpar)
            parsepar_success = .true.
        end if
    end do
    if (parsepar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xdouble(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsexpar_double
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
!> Read a parameter from file by name, e.g.,
!> parname = 10
!
subroutine readpar_complex(filename, parname, par, default_value, required)
    character(len=*), intent(in) :: filename, parname
    complex, intent(inout) :: par
    complex, intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    integer :: ioerr
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                par = extract_complex(tidy(line(eqindex + 1:)))
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readpar_complex
!
!> Read a parameter from file by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine readnpar_complex(filename, parname, par, default_value, required, separator)
    character(len=*), intent(in) :: filename, parname
    complex, allocatable, dimension(:), intent(inout) :: par
    complex, dimension(:), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=*), intent(in), optional :: separator
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    character(len=10) :: sepr
    integer :: ioerr
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_ncomplex(tidy(line(eqindex + 1:)), trim(adjustl(sepr)), par)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                if (allocated(par)) then
                    deallocate (par)
                else
                    par = default_value
                end if
            end if
        else
            ! if not required then set to default value
            if (allocated(par)) then
                deallocate (par)
            end if
            par = default_value
        end if
    end if
end subroutine readnpar_complex
!
!> Read a parameter from file using the format
!> parname = range1:value1, range2:value2, ...
!> e.g., parname = 1~10:0.0, 11~20:1.0, 21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine readxpar_complex(filename, parname, par, default_value, var, required)
    character(len=*), intent(in) :: filename, parname
    complex, intent(inout) :: par
    complex, intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    complex, allocatable, dimension(:) :: rpar
    character(len=1024) :: line
    integer :: npar, funit, eqindex, i, l, nr
    integer :: ioerr
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nstring(tidy(line(eqindex + 1:)), ',', tmpar)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar >= 1) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xcomplex(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readxpar_complex
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10
!
subroutine getpar_complex(parname, par, default_value, required)
    character(len=*), intent(in) :: parname
    complex, intent(out) :: par
    complex, intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                par = extract_complex(arg(len_par + 2:len_arg))
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getpar_complex
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine getnpar_complex(parname, par, default_value, separator, required)
    character(len=*), intent(in) :: parname
    complex, allocatable, dimension(:), intent(out) :: par
    complex, dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    getpar_success = .false.
    len_par = len_trim(parname)
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_ncomplex(arg(len_par + 2:len_arg), trim(adjustl(sepr)), par)
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getnpar_complex
!
!> Read a parameter from command line arguments using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine getxpar_complex(parname, par, default_value, var, required)
    character(len=*), intent(in) :: parname
    complex, intent(inout) :: par
    complex, intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    complex, allocatable, dimension(:) :: rpar
    integer :: i, l, nr
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nstring(tidy(arg(len_par + 2:)), ',', tmpar)
                getpar_success = .true.
            end if
        end if
    end do
    if (getpar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xcomplex(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getxpar_complex
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10
!
subroutine parsepar_complex(source, parname, par, default_value, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    complex, intent(out) :: par
    complex, intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            par = extract_complex(tidy(arg(eqindex + 1:)))
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsepar_complex
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine parsenpar_complex(source, parname, par, default_value, separator, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    complex, allocatable, dimension(:), intent(out) :: par
    complex, dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    parsepar_success = .false.
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_ncomplex(tidy(arg(eqindex + 1:)), trim(adjustl(sepr)), par)
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsenpar_complex
!
!> Read a parameter from a string using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine parsexpar_complex(source, parname, par, default_value, var, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    complex, allocatable, dimension(:), intent(out) :: par
    complex, dimension(:), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    complex, allocatable, dimension(:) :: rpar
    integer :: i, j, l, nr, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nstring(tidy(arg(eqindex + 1:)), ',', tmpar)
            parsepar_success = .true.
        end if
    end do
    if (parsepar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xcomplex(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsexpar_complex
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
!> Read a parameter from file by name, e.g.,
!> parname = 10
!
subroutine readpar_dcomplex(filename, parname, par, default_value, required)
    character(len=*), intent(in) :: filename, parname
    double complex, intent(inout) :: par
    double complex, intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    integer :: ioerr
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                par = extract_dcomplex(tidy(line(eqindex + 1:)))
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readpar_dcomplex
!
!> Read a parameter from file by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine readnpar_dcomplex(filename, parname, par, default_value, required, separator)
    character(len=*), intent(in) :: filename, parname
    double complex, allocatable, dimension(:), intent(inout) :: par
    double complex, dimension(:), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=*), intent(in), optional :: separator
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    character(len=10) :: sepr
    integer :: ioerr
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_ndcomplex(tidy(line(eqindex + 1:)), trim(adjustl(sepr)), par)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                if (allocated(par)) then
                    deallocate (par)
                else
                    par = default_value
                end if
            end if
        else
            ! if not required then set to default value
            if (allocated(par)) then
                deallocate (par)
            end if
            par = default_value
        end if
    end if
end subroutine readnpar_dcomplex
!
!> Read a parameter from file using the format
!> parname = range1:value1, range2:value2, ...
!> e.g., parname = 1~10:0.0, 11~20:1.0, 21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine readxpar_dcomplex(filename, parname, par, default_value, var, required)
    character(len=*), intent(in) :: filename, parname
    double complex, intent(inout) :: par
    double complex, intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    double complex, allocatable, dimension(:) :: rpar
    character(len=1024) :: line
    integer :: npar, funit, eqindex, i, l, nr
    integer :: ioerr
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nstring(tidy(line(eqindex + 1:)), ',', tmpar)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar >= 1) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xdcomplex(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readxpar_dcomplex
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10
!
subroutine getpar_dcomplex(parname, par, default_value, required)
    character(len=*), intent(in) :: parname
    double complex, intent(out) :: par
    double complex, intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                par = extract_dcomplex(arg(len_par + 2:len_arg))
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getpar_dcomplex
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine getnpar_dcomplex(parname, par, default_value, separator, required)
    character(len=*), intent(in) :: parname
    double complex, allocatable, dimension(:), intent(out) :: par
    double complex, dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    getpar_success = .false.
    len_par = len_trim(parname)
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_ndcomplex(arg(len_par + 2:len_arg), trim(adjustl(sepr)), par)
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getnpar_dcomplex
!
!> Read a parameter from command line arguments using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine getxpar_dcomplex(parname, par, default_value, var, required)
    character(len=*), intent(in) :: parname
    double complex, intent(inout) :: par
    double complex, intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    double complex, allocatable, dimension(:) :: rpar
    integer :: i, l, nr
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nstring(tidy(arg(len_par + 2:)), ',', tmpar)
                getpar_success = .true.
            end if
        end if
    end do
    if (getpar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xdcomplex(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getxpar_dcomplex
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10
!
subroutine parsepar_dcomplex(source, parname, par, default_value, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    double complex, intent(out) :: par
    double complex, intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            par = extract_dcomplex(tidy(arg(eqindex + 1:)))
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsepar_dcomplex
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine parsenpar_dcomplex(source, parname, par, default_value, separator, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    double complex, allocatable, dimension(:), intent(out) :: par
    double complex, dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    parsepar_success = .false.
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_ndcomplex(tidy(arg(eqindex + 1:)), trim(adjustl(sepr)), par)
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsenpar_dcomplex
!
!> Read a parameter from a string using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine parsexpar_dcomplex(source, parname, par, default_value, var, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    double complex, allocatable, dimension(:), intent(out) :: par
    double complex, dimension(:), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    double complex, allocatable, dimension(:) :: rpar
    integer :: i, j, l, nr, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nstring(tidy(arg(eqindex + 1:)), ',', tmpar)
            parsepar_success = .true.
        end if
    end do
    if (parsepar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xdcomplex(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation applies to float, double, complex
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rbeg(i + 1)) then
                        par = rpar(i) + (rpar(i + 1) - rpar(i))*(var - rend(i))/(rbeg(i + 1) - rend(i))
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsexpar_dcomplex
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
!> Read a parameter from file by name, e.g.,
!> parname = 10
!
subroutine readpar_logical(filename, parname, par, default_value, required)
    character(len=*), intent(in) :: filename, parname
    logical, intent(inout) :: par
    logical, intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    integer :: ioerr
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                par = extract_logical(tidy(line(eqindex + 1:)))
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readpar_logical
!
!> Read a parameter from file by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine readnpar_logical(filename, parname, par, default_value, required, separator)
    character(len=*), intent(in) :: filename, parname
    logical, allocatable, dimension(:), intent(inout) :: par
    logical, dimension(:), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=*), intent(in), optional :: separator
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    character(len=10) :: sepr
    integer :: ioerr
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nlogical(tidy(line(eqindex + 1:)), trim(adjustl(sepr)), par)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                if (allocated(par)) then
                    deallocate (par)
                else
                    par = default_value
                end if
            end if
        else
            ! if not required then set to default value
            if (allocated(par)) then
                deallocate (par)
            end if
            par = default_value
        end if
    end if
end subroutine readnpar_logical
!
!> Read a parameter from file using the format
!> parname = range1:value1, range2:value2, ...
!> e.g., parname = 1~10:0.0, 11~20:1.0, 21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine readxpar_logical(filename, parname, par, default_value, var, required)
    character(len=*), intent(in) :: filename, parname
    logical, intent(inout) :: par
    logical, intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    logical, allocatable, dimension(:) :: rpar
    character(len=1024) :: line
    integer :: npar, funit, eqindex, i, l, nr
    integer :: ioerr
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nstring(tidy(line(eqindex + 1:)), ',', tmpar)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar >= 1) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xlogical(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readxpar_logical
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10
!
subroutine getpar_logical(parname, par, default_value, required)
    character(len=*), intent(in) :: parname
    logical, intent(out) :: par
    logical, intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                par = extract_logical(arg(len_par + 2:len_arg))
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getpar_logical
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine getnpar_logical(parname, par, default_value, separator, required)
    character(len=*), intent(in) :: parname
    logical, allocatable, dimension(:), intent(out) :: par
    logical, dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    getpar_success = .false.
    len_par = len_trim(parname)
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nlogical(arg(len_par + 2:len_arg), trim(adjustl(sepr)), par)
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getnpar_logical
!
!> Read a parameter from command line arguments using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine getxpar_logical(parname, par, default_value, var, required)
    character(len=*), intent(in) :: parname
    logical, intent(inout) :: par
    logical, intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    logical, allocatable, dimension(:) :: rpar
    integer :: i, l, nr
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nstring(tidy(arg(len_par + 2:)), ',', tmpar)
                getpar_success = .true.
            end if
        end if
    end do
    if (getpar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xlogical(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getxpar_logical
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10
!
subroutine parsepar_logical(source, parname, par, default_value, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    logical, intent(out) :: par
    logical, intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            par = extract_logical(tidy(arg(eqindex + 1:)))
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsepar_logical
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine parsenpar_logical(source, parname, par, default_value, separator, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    logical, allocatable, dimension(:), intent(out) :: par
    logical, dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    parsepar_success = .false.
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nlogical(tidy(arg(eqindex + 1:)), trim(adjustl(sepr)), par)
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsenpar_logical
!
!> Read a parameter from a string using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine parsexpar_logical(source, parname, par, default_value, var, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    logical, allocatable, dimension(:), intent(out) :: par
    logical, dimension(:), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    logical, allocatable, dimension(:) :: rpar
    integer :: i, j, l, nr, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nstring(tidy(arg(eqindex + 1:)), ',', tmpar)
            parsepar_success = .true.
        end if
    end do
    if (parsepar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xlogical(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsexpar_logical
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
!> Read a parameter from file by name, e.g.,
!> parname = 10
!
subroutine readpar_string(filename, parname, par, default_value, required)
    character(len=*), intent(in) :: filename, parname
    character(len=*), intent(inout) :: par
    character(len=*), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    integer :: ioerr
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                par = extract_string(tidy(line(eqindex + 1:)))
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readpar_string
!
!> Read a parameter from file by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine readnpar_string(filename, parname, par, default_value, required, separator)
    character(len=*), intent(in) :: filename, parname
    character(len=*), allocatable, dimension(:), intent(inout) :: par
    character(len=*), dimension(:), intent(in) :: default_value
    logical, intent(in), optional :: required
    character(len=*), intent(in), optional :: separator
    character(len=1024) :: line
    integer :: npar, funit, eqindex
    character(len=10) :: sepr
    integer :: ioerr
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nstring(tidy(line(eqindex + 1:)), trim(adjustl(sepr)), par)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar == 0) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                if (allocated(par)) then
                    deallocate (par)
                else
                    par = default_value
                end if
            end if
        else
            ! if not required then set to default value
            if (allocated(par)) then
                deallocate (par)
            end if
            par = default_value
        end if
    end if
end subroutine readnpar_string
!
!> Read a parameter from file using the format
!> parname = range1:value1, range2:value2, ...
!> e.g., parname = 1~10:0.0, 11~20:1.0, 21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine readxpar_string(filename, parname, par, default_value, var, required)
    character(len=*), intent(in) :: filename, parname
    character(len=*), intent(inout) :: par
    character(len=*), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    character(len=64), allocatable, dimension(:) :: rpar
    character(len=1024) :: line
    integer :: npar, funit, eqindex, i, l, nr
    integer :: ioerr
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    npar = 0
    open (newunit=funit, file=tidy(filename), status='old', action='read')
    do
        read (funit, '(a)', iostat=ioerr) line
        if (ioerr /= 0) exit
        if (tidy(line) == 'exit') exit
        if (len(tidy(line)) > 0) then
            eqindex = index(line, '=')
            if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                call extract_nstring(tidy(line(eqindex + 1:)), ',', tmpar)
                npar = npar + 1
            end if
        end if
    end do
    close (funit)
    if (npar >= 1) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xstring(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine readxpar_string
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10
!
subroutine getpar_string(parname, par, default_value, required)
    character(len=*), intent(in) :: parname
    character(len=*), intent(out) :: par
    character(len=*), intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                par = extract_string(arg(len_par + 2:len_arg))
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getpar_string
!
!> Read a parameter from command line arguments by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine getnpar_string(parname, par, default_value, separator, required)
    character(len=*), intent(in) :: parname
    character(len=*), allocatable, dimension(:), intent(out) :: par
    character(len=*), dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    getpar_success = .false.
    len_par = len_trim(parname)
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nstring(arg(len_par + 2:len_arg), trim(adjustl(sepr)), par)
                getpar_success = .true.
            end if
        end if
    end do
    if (.not.getpar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getnpar_string
!
!> Read a parameter from command line arguments using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine getxpar_string(parname, par, default_value, var, required)
    character(len=*), intent(in) :: parname
    character(len=*), intent(inout) :: par
    character(len=*), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    character(len=64), allocatable, dimension(:) :: rpar
    integer :: i, l, nr
    integer :: len_arg
    integer :: len_par
    logical :: getpar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    getpar_success = .false.
    len_par = len_trim(parname)
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        len_arg = len_trim(arg)
        if (len_arg > len_par + 1) then
            ! For getpar, the parname must be followed by = without any space.
            if (to_lower(tidy(arg(1:len_par + 1))) == to_lower(tidy(parname))//'=') then
                call extract_nstring(tidy(arg(len_par + 2:)), ',', tmpar)
                getpar_success = .true.
            end if
        end if
    end do
    if (getpar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xstring(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine getxpar_string
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10
!
subroutine parsepar_string(source, parname, par, default_value, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    character(len=*), intent(out) :: par
    character(len=*), intent(in) :: default_value
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            par = extract_string(tidy(arg(eqindex + 1:)))
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsepar_string
!
!> Read a parameter from a string by name, e.g.,
!> parname = 10, 20, 30, ...
!
subroutine parsenpar_string(source, parname, par, default_value, separator, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    character(len=*), allocatable, dimension(:), intent(out) :: par
    character(len=*), dimension(:), intent(in) :: default_value
    character(len=*), intent(in), optional :: separator
    logical, intent(in), optional :: required
    integer :: i, j, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    character(len=10) :: sepr
    parsepar_success = .false.
    if (present(separator)) then
        sepr = separator
    else
        sepr = ','
    end if
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nstring(tidy(arg(eqindex + 1:)), trim(adjustl(sepr)), par)
            parsepar_success = .true.
        end if
    end do
    if (.not. parsepar_success) then
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsenpar_string
!
!> Read a parameter from a string using the format
!> parname=range1:value1,range2:value2,...
!> e.g., par=1~10:0.0,11~20:1.0,21~30:2.0
!> where the variable ranges from 1~10, the parameter value is 10, ...
!
subroutine parsexpar_string(source, parname, par, default_value, var, required)
    character(len=*), dimension(:), intent(in) :: source
    character(len=*), intent(in) :: parname
    character(len=*), allocatable, dimension(:), intent(out) :: par
    character(len=*), dimension(:), intent(in) :: default_value
    real, intent(in) :: var
    logical, intent(in), optional :: required
    character(len=64), allocatable, dimension(:) :: tmpar
    real, allocatable, dimension(:) :: rbeg, rend
    character(len=64), allocatable, dimension(:) :: rpar
    integer :: i, j, l, nr, eqindex
    logical :: parsepar_success
    character(len=256) :: arg
    ! this initialization is necessary
    ! otherwise, segmentation fault
    allocate (tmpar(1:nstring_max))
    parsepar_success = .false.
    do i = 1, size(source)
        arg = source(i)
        do j = 1, len(arg)
            if (arg(j:j) == achar(13) .or. arg(j:j) == achar(10)) then
                arg(j:j) = ' '
            end if
        end do
        eqindex = index(arg, '=')
        if (to_lower(tidy(arg(1:eqindex - 1))) == to_lower(tidy(parname))) then
            call extract_nstring(tidy(arg(eqindex + 1:)), ',', tmpar)
            parsepar_success = .true.
        end if
    end do
    if (parsepar_success) then
        nr = size(tmpar)
        ! extracting range and values
        allocate (rbeg(1:nr))
        allocate (rend(1:nr))
        allocate (rpar(1:nr))
        do i = 1, nr
            call extract_xstring(tidy(tmpar(i)), rbeg(i), rend(i), rpar(i))
        end do
        ! find target value correponding to var
        if (var < rbeg(1)) then
            ! if var < min of given range
            par = rpar(1)
        else if (var > rend(nr)) then
            ! if var > max of given range
            par = rpar(nr)
        else
            ! if var between min and max of given range
            ! and in some given interval
            l = 0
            do i = 1, nr
                if (var >= rbeg(i) .and. var <= rend(i)) then
                    par = rpar(i)
                    l = l + 1
                end if
            end do
            ! if var is located somewhere between some non-defined interval [rend, rbeg_next]
            ! then linearly interpolate to obtain par value
            if (l == 0) then
                ! Linear interpolation does not apply to int, logical, or string
                do i = 1, nr - 1
                    if (var > rend(i) .and. var < rend(i) + 0.5*(rbeg(i + 1) - rend(i))) then
                        par = rpar(i)
                        exit
                    else if (var >= rend(i) + 0.5*(rbeg(i + 1) - rend(i)) .and. var < rbeg(i + 1)) then
                        par = rpar(i + 1)
                        exit
                    end if
                end do
            end if
        end if
    else
        ! if does not exist then set to default
        if (present(required)) then
            if (required) then
                ! if required then stop
                call warn(' Error: Parameter '//tidy(parname)//' is required but not set. ')
                stop
            else
                ! if not required then set to default
                par = default_value
            end if
        else
            ! if not required then set to default value
            par = default_value
        end if
    end if
end subroutine parsexpar_string
end module libflit_readpar
