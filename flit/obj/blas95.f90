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
module blas95
    implicit none
    interface gemv
        module procedure sgemv, dgemv, cgemv, zgemv
    end interface
    interface gemm
        module procedure sgemm, dgemm, cgemm, zgemm
    end interface
contains
    subroutine sgemv(a, x, y, alpha, beta, trans)
        real, intent(in) :: a(:, :)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        real, intent(in), optional :: alpha, beta
        character(len=1), intent(in), optional :: trans
        real :: al, be
        character :: tr
        al = 1.0; be = 0.0; tr = 'N'
        if (present(alpha)) al = alpha
        if (present(beta)) be = beta
        if (present(trans)) tr = trans
        call sgemv_f77(tr, size(a, 1), size(a, 2), al, a, size(a, 1), x, 1, be, y, 1)
    end subroutine sgemv
    subroutine dgemv(a, x, y, alpha, beta, trans)
        double precision, intent(in) :: a(:, :)
        double precision, intent(in) :: x(:)
        double precision, intent(out) :: y(:)
        double precision, intent(in), optional :: alpha, beta
        character(len=1), intent(in), optional :: trans
        double precision :: al, be
        character :: tr
        al = 1.0d0; be = 0.0d0; tr = 'N'
        if (present(alpha)) al = alpha
        if (present(beta)) be = beta
        if (present(trans)) tr = trans
        call dgemv_f77(tr, size(a, 1), size(a, 2), al, a, size(a, 1), x, 1, be, y, 1)
    end subroutine dgemv
    subroutine cgemv(a, x, y, alpha, beta, trans)
        complex, intent(in) :: a(:, :)
        complex, intent(in) :: x(:)
        complex, intent(out) :: y(:)
        complex, intent(in), optional :: alpha, beta
        character(len=1), intent(in), optional :: trans
        complex :: al, be
        character :: tr
        al = (1.0, 0.0); be = (0.0, 0.0); tr = 'N'
        if (present(alpha)) al = alpha
        if (present(beta)) be = beta
        if (present(trans)) tr = trans
        call cgemv_f77(tr, size(a, 1), size(a, 2), al, a, size(a, 1), x, 1, be, y, 1)
    end subroutine cgemv
    subroutine zgemv(a, x, y, alpha, beta, trans)
        double complex, intent(in) :: a(:, :)
        double complex, intent(in) :: x(:)
        double complex, intent(out) :: y(:)
        double complex, intent(in), optional :: alpha, beta
        character(len=1), intent(in), optional :: trans
        double complex :: al, be
        character :: tr
        al = (1.0d0, 0.0d0); be = (0.0d0, 0.0d0); tr = 'N'
        if (present(alpha)) al = alpha
        if (present(beta)) be = beta
        if (present(trans)) tr = trans
        call zgemv_f77(tr, size(a, 1), size(a, 2), al, a, size(a, 1), x, 1, be, y, 1)
    end subroutine zgemv
    subroutine sgemm(a, b, c, alpha, beta, transa, transb)
        real, intent(in) :: a(:, :), b(:, :)
        real, intent(inout) :: c(:, :)
        real, intent(in), optional :: alpha, beta
        character(len=1), intent(in), optional :: transa, transb
        real :: al, be
        character :: ta, tb
        al = 1.0; be = 0.0; ta = 'N'; tb = 'N'
        if (present(alpha)) al = alpha
        if (present(beta)) be = beta
        if (present(transa)) ta = transa
        if (present(transb)) tb = transb
        call sgemm_f77(ta, tb, size(c, 1), size(c, 2), size(a, 2), al, a, size(a, 1), b, size(b, 1), be, c, size(c, 1))
    end subroutine sgemm
    subroutine dgemm(a, b, c, alpha, beta, transa, transb)
        double precision, intent(in) :: a(:, :), b(:, :)
        double precision, intent(inout) :: c(:, :)
        double precision, intent(in), optional :: alpha, beta
        character(len=1), intent(in), optional :: transa, transb
        double precision :: al, be
        character :: ta, tb
        al = 1.0d0; be = 0.0d0; ta = 'N'; tb = 'N'
        if (present(alpha)) al = alpha
        if (present(beta)) be = beta
        if (present(transa)) ta = transa
        if (present(transb)) tb = transb
        call dgemm_f77(ta, tb, size(c, 1), size(c, 2), size(a, 2), al, a, size(a, 1), b, size(b, 1), be, c, size(c, 1))
    end subroutine dgemm
    subroutine cgemm(a, b, c, alpha, beta, transa, transb)
        complex, intent(in) :: a(:, :), b(:, :)
        complex, intent(inout) :: c(:, :)
        complex, intent(in), optional :: alpha, beta
        character(len=1), intent(in), optional :: transa, transb
        complex :: al, be
        character :: ta, tb
        al = (1.0, 0.0); be = (0.0, 0.0); ta = 'N'; tb = 'N'
        if (present(alpha)) al = alpha
        if (present(beta)) be = beta
        if (present(transa)) ta = transa
        if (present(transb)) tb = transb
        call cgemm_f77(ta, tb, size(c, 1), size(c, 2), size(a, 2), al, a, size(a, 1), b, size(b, 1), be, c, size(c, 1))
    end subroutine cgemm
    subroutine zgemm(a, b, c, alpha, beta, transa, transb)
        double complex, intent(in) :: a(:, :), b(:, :)
        double complex, intent(inout) :: c(:, :)
        double complex, intent(in), optional :: alpha, beta
        character(len=1), intent(in), optional :: transa, transb
        double complex :: al, be
        character :: ta, tb
        al = (1.0d0, 0.0d0); be = (0.0d0, 0.0d0); ta = 'N'; tb = 'N'
        if (present(alpha)) al = alpha
        if (present(beta)) be = beta
        if (present(transa)) ta = transa
        if (present(transb)) tb = transb
        call zgemm_f77(ta, tb, size(c, 1), size(c, 2), size(a, 2), al, a, size(a, 1), b, size(b, 1), be, c, size(c, 1))
    end subroutine zgemm
end module blas95
! Wrapper to actual BLAS symbols
subroutine sgemv_f77(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
    character :: trans
    integer :: m, n, lda, incx, incy
    real :: alpha, beta, a(*), x(*), y(*)
    external sgemv
    call sgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
end subroutine
subroutine dgemv_f77(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
    character :: trans
    integer :: m, n, lda, incx, incy
    double precision :: alpha, beta, a(*), x(*), y(*)
    external dgemv
    call dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
end subroutine
subroutine sgemm_f77(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
    character :: transa, transb
    integer :: m, n, k, lda, ldb, ldc
    real :: alpha, beta, a(*), b(*), c(*)
    external sgemm
    call sgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
end subroutine
subroutine dgemm_f77(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
    character :: transa, transb
    integer :: m, n, k, lda, ldb, ldc
    double precision :: alpha, beta, a(*), b(*), c(*)
    external dgemm
    call dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
end subroutine
subroutine cgemv_f77(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
    character :: trans
    integer :: m, n, lda, incx, incy
    complex :: alpha, beta, a(*), x(*), y(*)
    external cgemv
    call cgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
end subroutine
subroutine zgemv_f77(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
    character :: trans
    integer :: m, n, lda, incx, incy
    double complex :: alpha, beta, a(*), x(*), y(*)
    external zgemv
    call zgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
end subroutine
subroutine cgemm_f77(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
    character :: transa, transb
    integer :: m, n, k, lda, ldb, ldc
    complex :: alpha, beta, a(*), b(*), c(*)
    external cgemm
    call cgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
end subroutine
subroutine zgemm_f77(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
    character :: transa, transb
    integer :: m, n, k, lda, ldb, ldc
    double complex :: alpha, beta, a(*), b(*), c(*)
    external zgemm
    call zgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
end subroutine
