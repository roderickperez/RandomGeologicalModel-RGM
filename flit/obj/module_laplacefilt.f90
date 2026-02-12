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
module libflit_laplacefilt
    use libflit_array
    use libflit_error
    use libflit_array_operation
    implicit none
    interface laplace_filt
        module procedure :: laplace_filt_1d_float
        module procedure :: laplace_filt_2d_float
        module procedure :: laplace_filt_3d_float
        module procedure :: laplace_filt_1d_double
        module procedure :: laplace_filt_2d_double
        module procedure :: laplace_filt_3d_double
        module procedure :: laplace_filt_1d_complex
        module procedure :: laplace_filt_2d_complex
        module procedure :: laplace_filt_3d_complex
        module procedure :: laplace_filt_1d_dcomplex
        module procedure :: laplace_filt_2d_dcomplex
        module procedure :: laplace_filt_3d_dcomplex
    end interface
    private
    public :: laplace_filt
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
!
!> 1D discrete Laplacian filter
!
function laplace_filt_1d_float(w, type) result(wt)
    ! Arguments
    real, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, n1
    real, allocatable, dimension(:) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension
    n1 = size(w)
    ! Allocate memory
    wt = zeros(n1)
    ww = w
    call pad_array(ww, [1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i) = ww(i - 1) + ww(i + 1) - 2.0*ww(i)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i) = sum(ww(i - 1:i + 1)) - 5.0*ww(i)
            end do
            !$omp end parallel do
    end select
end function laplace_filt_1d_float
!
!> 2D discrete Laplacian filter
!
function laplace_filt_2d_float(w, type) result(wt)
    ! Arguments
    real, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, j, n1, n2
    real, allocatable, dimension(:, :) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Allocate memory
    wt = zeros(n1, n2)
    ww = w
    call pad_array(ww, [1, 1, 1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j) = ww(i - 1, j) + ww(i + 1, j) &
                        + ww(i, j - 1) + ww(i, j + 1) &
                        - 4.0*ww(i, j)
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j) = sum(ww(i - 1:i + 1, j - 1:j + 1)) &
                        - 9.0*ww(i, j)
                end do
            end do
            !$omp end parallel do
    end select
    deallocate (ww)
end function laplace_filt_2d_float
!
!> 3D discrete Laplacian filter
!
function laplace_filt_3d_float(w, type) result(wt)
    ! Arguments
    real, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, j, k, n1, n2, n3
    real, allocatable, dimension(:, :, :) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Allocate memory
    wt = zeros(n1, n2, n3)
    ww = w
    call pad_array(ww, [1, 1, 1, 1, 1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i, j, k)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        wt(i, j, k) = ww(i - 1, j, k) + ww(i + 1, j, k) &
                            + ww(i, j - 1, k) + ww(i, j + 1, k) &
                            + ww(i, j, k - 1) + ww(i, j, k + 1) &
                            -6.0*ww(i, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, j, k)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        wt(i, j, k) = sum(ww(i - 1:i + 1, j - 1:j + 1, k - 1:k + 1)) &
                            -13.0*ww(i, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
    end select
end function laplace_filt_3d_float
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
!> 1D discrete Laplacian filter
!
function laplace_filt_1d_double(w, type) result(wt)
    ! Arguments
    double precision, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, n1
    double precision, allocatable, dimension(:) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension
    n1 = size(w)
    ! Allocate memory
    wt = zeros(n1)
    ww = w
    call pad_array(ww, [1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i) = ww(i - 1) + ww(i + 1) - 2.0*ww(i)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i) = sum(ww(i - 1:i + 1)) - 5.0*ww(i)
            end do
            !$omp end parallel do
    end select
end function laplace_filt_1d_double
!
!> 2D discrete Laplacian filter
!
function laplace_filt_2d_double(w, type) result(wt)
    ! Arguments
    double precision, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, j, n1, n2
    double precision, allocatable, dimension(:, :) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Allocate memory
    wt = zeros(n1, n2)
    ww = w
    call pad_array(ww, [1, 1, 1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j) = ww(i - 1, j) + ww(i + 1, j) &
                        + ww(i, j - 1) + ww(i, j + 1) &
                        - 4.0*ww(i, j)
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j) = sum(ww(i - 1:i + 1, j - 1:j + 1)) &
                        - 9.0*ww(i, j)
                end do
            end do
            !$omp end parallel do
    end select
    deallocate (ww)
end function laplace_filt_2d_double
!
!> 3D discrete Laplacian filter
!
function laplace_filt_3d_double(w, type) result(wt)
    ! Arguments
    double precision, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, j, k, n1, n2, n3
    double precision, allocatable, dimension(:, :, :) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Allocate memory
    wt = zeros(n1, n2, n3)
    ww = w
    call pad_array(ww, [1, 1, 1, 1, 1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i, j, k)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        wt(i, j, k) = ww(i - 1, j, k) + ww(i + 1, j, k) &
                            + ww(i, j - 1, k) + ww(i, j + 1, k) &
                            + ww(i, j, k - 1) + ww(i, j, k + 1) &
                            -6.0*ww(i, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, j, k)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        wt(i, j, k) = sum(ww(i - 1:i + 1, j - 1:j + 1, k - 1:k + 1)) &
                            -13.0*ww(i, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
    end select
end function laplace_filt_3d_double
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
!> 1D discrete Laplacian filter
!
function laplace_filt_1d_complex(w, type) result(wt)
    ! Arguments
    complex, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, n1
    complex, allocatable, dimension(:) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension
    n1 = size(w)
    ! Allocate memory
    wt = zeros(n1)
    ww = w
    call pad_array(ww, [1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i) = ww(i - 1) + ww(i + 1) - 2.0*ww(i)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i) = sum(ww(i - 1:i + 1)) - 5.0*ww(i)
            end do
            !$omp end parallel do
    end select
end function laplace_filt_1d_complex
!
!> 2D discrete Laplacian filter
!
function laplace_filt_2d_complex(w, type) result(wt)
    ! Arguments
    complex, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, j, n1, n2
    complex, allocatable, dimension(:, :) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Allocate memory
    wt = zeros(n1, n2)
    ww = w
    call pad_array(ww, [1, 1, 1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j) = ww(i - 1, j) + ww(i + 1, j) &
                        + ww(i, j - 1) + ww(i, j + 1) &
                        - 4.0*ww(i, j)
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j) = sum(ww(i - 1:i + 1, j - 1:j + 1)) &
                        - 9.0*ww(i, j)
                end do
            end do
            !$omp end parallel do
    end select
    deallocate (ww)
end function laplace_filt_2d_complex
!
!> 3D discrete Laplacian filter
!
function laplace_filt_3d_complex(w, type) result(wt)
    ! Arguments
    complex, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, j, k, n1, n2, n3
    complex, allocatable, dimension(:, :, :) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Allocate memory
    wt = zeros(n1, n2, n3)
    ww = w
    call pad_array(ww, [1, 1, 1, 1, 1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i, j, k)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        wt(i, j, k) = ww(i - 1, j, k) + ww(i + 1, j, k) &
                            + ww(i, j - 1, k) + ww(i, j + 1, k) &
                            + ww(i, j, k - 1) + ww(i, j, k + 1) &
                            -6.0*ww(i, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, j, k)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        wt(i, j, k) = sum(ww(i - 1:i + 1, j - 1:j + 1, k - 1:k + 1)) &
                            -13.0*ww(i, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
    end select
end function laplace_filt_3d_complex
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
!> 1D discrete Laplacian filter
!
function laplace_filt_1d_dcomplex(w, type) result(wt)
    ! Arguments
    double complex, dimension(:), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, n1
    double complex, allocatable, dimension(:) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension
    n1 = size(w)
    ! Allocate memory
    wt = zeros(n1)
    ww = w
    call pad_array(ww, [1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i) = ww(i - 1) + ww(i + 1) - 2.0*ww(i)
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i)
            do i = 1, n1
                wt(i) = sum(ww(i - 1:i + 1)) - 5.0*ww(i)
            end do
            !$omp end parallel do
    end select
end function laplace_filt_1d_dcomplex
!
!> 2D discrete Laplacian filter
!
function laplace_filt_2d_dcomplex(w, type) result(wt)
    ! Arguments
    double complex, dimension(:, :), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, j, n1, n2
    double complex, allocatable, dimension(:, :) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension
    n1 = size(w, 1)
    n2 = size(w, 2)
    ! Allocate memory
    wt = zeros(n1, n2)
    ww = w
    call pad_array(ww, [1, 1, 1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j) = ww(i - 1, j) + ww(i + 1, j) &
                        + ww(i, j - 1) + ww(i, j + 1) &
                        - 4.0*ww(i, j)
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, j)
            do j = 1, n2
                do i = 1, n1
                    wt(i, j) = sum(ww(i - 1:i + 1, j - 1:j + 1)) &
                        - 9.0*ww(i, j)
                end do
            end do
            !$omp end parallel do
    end select
    deallocate (ww)
end function laplace_filt_2d_dcomplex
!
!> 3D discrete Laplacian filter
!
function laplace_filt_3d_dcomplex(w, type) result(wt)
    ! Arguments
    double complex, dimension(:, :, :), intent(inout) :: w
    integer, intent(in), optional :: type
    ! Local variables
    integer :: i, j, k, n1, n2, n3
    double complex, allocatable, dimension(:, :, :) :: wt, ww
    integer :: filter_type
    if (present(type)) then
        filter_type = type
    else
        filter_type = 1
    end if
    ! dimension of arrays
    n1 = size(w, 1)
    n2 = size(w, 2)
    n3 = size(w, 3)
    ! Allocate memory
    wt = zeros(n1, n2, n3)
    ww = w
    call pad_array(ww, [1, 1, 1, 1, 1, 1])
    ! Finte-difference Laplacian operator
    select case (filter_type)
        case (1)
            !$omp parallel do private(i, j, k)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        wt(i, j, k) = ww(i - 1, j, k) + ww(i + 1, j, k) &
                            + ww(i, j - 1, k) + ww(i, j + 1, k) &
                            + ww(i, j, k - 1) + ww(i, j, k + 1) &
                            -6.0*ww(i, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
        case (2)
            !$omp parallel do private(i, j, k)
            do k = 1, n3
                do j = 1, n2
                    do i = 1, n1
                        wt(i, j, k) = sum(ww(i - 1:i + 1, j - 1:j + 1, k - 1:k + 1)) &
                            -13.0*ww(i, j, k)
                    end do
                end do
            end do
            !$omp end parallel do
    end select
end function laplace_filt_3d_dcomplex
end module libflit_laplacefilt
