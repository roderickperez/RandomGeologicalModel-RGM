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
module lapack95
    implicit none
    interface getrf
        module procedure sgetrf_f95, dgetrf_f95, cgetrf_f95, zgetrf_f95
    end interface
    interface getri
        module procedure sgetri_f95, dgetri_f95, cgetri_f95, zgetri_f95
    end interface
    interface gesv
        module procedure sgesv_1d_f95, dgesv_1d_f95, cgesv_1d_f95, zgesv_1d_f95
        module procedure sgesv_2d_f95, dgesv_2d_f95, cgesv_2d_f95, zgesv_2d_f95
    end interface
    interface gesvd
        module procedure sgesvd_f95, dgesvd_f95, cgesvd_f95, zgesvd_f95
    end interface
    interface gels
        module procedure sgels_1d_f95, dgels_1d_f95, cgels_1d_f95, zgels_1d_f95
        module procedure sgels_2d_f95, dgels_2d_f95, cgels_2d_f95, zgels_2d_f95
    end interface
    interface gbsv
        module procedure sgbsv_f95, dgbsv_f95, cgbsv_f95, zgbsv_f95
    end interface
    interface geev
        module procedure sgeev_f95, dgeev_f95, cgeev_f95, zgeev_f95
    end interface
    interface ggev
        module procedure sggev_f95, dggev_f95, cggev_f95, zggev_f95
    end interface
contains
    subroutine sgetrf_f95(a, ipiv, info)
        real, intent(inout) :: a(:, :)
        integer, intent(out) :: ipiv(:)
        integer, intent(out), optional :: info
        integer :: i
        call sgetrf_f77(size(a, 1), size(a, 2), a, size(a, 1), ipiv, i)
        if (present(info)) info = i
    end subroutine
    subroutine dgetrf_f95(a, ipiv, info)
        double precision, intent(inout) :: a(:, :)
        integer, intent(out) :: ipiv(:)
        integer, intent(out), optional :: info
        integer :: i
        call dgetrf_f77(size(a, 1), size(a, 2), a, size(a, 1), ipiv, i)
        if (present(info)) info = i
    end subroutine
    subroutine cgetrf_f95(a, ipiv, info)
        complex, intent(inout) :: a(:, :)
        integer, intent(out) :: ipiv(:)
        integer, intent(out), optional :: info
        integer :: i
        call cgetrf_f77(size(a, 1), size(a, 2), a, size(a, 1), ipiv, i)
        if (present(info)) info = i
    end subroutine
    subroutine zgetrf_f95(a, ipiv, info)
        double complex, intent(inout) :: a(:, :)
        integer, intent(out) :: ipiv(:)
        integer, intent(out), optional :: info
        integer :: i
        call zgetrf_f77(size(a, 1), size(a, 2), a, size(a, 1), ipiv, i)
        if (present(info)) info = i
    end subroutine
    subroutine sgetri_f95(a, ipiv, info)
        real, intent(inout) :: a(:, :)
        integer, intent(in) :: ipiv(:)
        integer, intent(out), optional :: info
        integer :: i, lwork
        real, allocatable :: work(:)
        real :: qwork(1)
        call sgetri_f77(size(a, 1), a, size(a, 1), ipiv, qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call sgetri_f77(size(a, 1), a, size(a, 1), ipiv, work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine dgetri_f95(a, ipiv, info)
        double precision, intent(inout) :: a(:, :)
        integer, intent(in) :: ipiv(:)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double precision, allocatable :: work(:)
        double precision :: qwork(1)
        call dgetri_f77(size(a, 1), a, size(a, 1), ipiv, qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call dgetri_f77(size(a, 1), a, size(a, 1), ipiv, work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine cgetri_f95(a, ipiv, info)
        complex, intent(inout) :: a(:, :)
        integer, intent(in) :: ipiv(:)
        integer, intent(out), optional :: info
        integer :: i, lwork
        complex, allocatable :: work(:)
        complex :: qwork(1)
        call cgetri_f77(size(a, 1), a, size(a, 1), ipiv, qwork, -1, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call cgetri_f77(size(a, 1), a, size(a, 1), ipiv, work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine zgetri_f95(a, ipiv, info)
        double complex, intent(inout) :: a(:, :)
        integer, intent(in) :: ipiv(:)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double complex, allocatable :: work(:)
        double complex :: qwork(1)
        call zgetri_f77(size(a, 1), a, size(a, 1), ipiv, qwork, -1, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call zgetri_f77(size(a, 1), a, size(a, 1), ipiv, work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine sgesv_1d_f95(a, b, info)
        real, intent(inout) :: a(:, :)
        real, intent(inout) :: b(:)
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 1))
        call sgesv_f77(size(a, 1), 1, a, size(a, 1), ipiv, b, size(a, 1), i)
        if (present(info)) info = i
    end subroutine
    subroutine dgesv_1d_f95(a, b, info)
        double precision, intent(inout) :: a(:, :)
        double precision, intent(inout) :: b(:)
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 1))
        call dgesv_f77(size(a, 1), 1, a, size(a, 1), ipiv, b, size(a, 1), i)
        if (present(info)) info = i
    end subroutine
    subroutine cgesv_1d_f95(a, b, info)
        complex, intent(inout) :: a(:, :)
        complex, intent(inout) :: b(:)
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 1))
        call cgesv_f77(size(a, 1), 1, a, size(a, 1), ipiv, b, size(a, 1), i)
        if (present(info)) info = i
    end subroutine
    subroutine zgesv_1d_f95(a, b, info)
        double complex, intent(inout) :: a(:, :)
        double complex, intent(inout) :: b(:)
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 1))
        call zgesv_f77(size(a, 1), 1, a, size(a, 1), ipiv, b, size(a, 1), i)
        if (present(info)) info = i
    end subroutine
    subroutine sgesv_2d_f95(a, b, info)
        real, intent(inout) :: a(:, :)
        real, intent(inout) :: b(:, :)
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 1))
        call sgesv_f77(size(a, 1), size(b, 2), a, size(a, 1), ipiv, b, size(b, 1), i)
        if (present(info)) info = i
    end subroutine
    subroutine dgesv_2d_f95(a, b, info)
        double precision, intent(inout) :: a(:, :)
        double precision, intent(inout) :: b(:, :)
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 1))
        call dgesv_f77(size(a, 1), size(b, 2), a, size(a, 1), ipiv, b, size(b, 1), i)
        if (present(info)) info = i
    end subroutine
    subroutine cgesv_2d_f95(a, b, info)
        complex, intent(inout) :: a(:, :)
        complex, intent(inout) :: b(:, :)
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 1))
        call cgesv_f77(size(a, 1), size(b, 2), a, size(a, 1), ipiv, b, size(b, 1), i)
        if (present(info)) info = i
    end subroutine
    subroutine zgesv_2d_f95(a, b, info)
        double complex, intent(inout) :: a(:, :)
        double complex, intent(inout) :: b(:, :)
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 1))
        call zgesv_f77(size(a, 1), size(b, 2), a, size(a, 1), ipiv, b, size(b, 1), i)
        if (present(info)) info = i
    end subroutine
    subroutine sgels_1d_f95(a, b, info)
        real, intent(inout) :: a(:, :)
        real, intent(inout) :: b(:)
        integer, intent(out), optional :: info
        integer :: i, lwork
        real, allocatable :: work(:)
        real :: qwork(1)
        call sgels_f77('N', size(a, 1), size(a, 2), 1, a, size(a, 1), b, size(b, 1), qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call sgels_f77('N', size(a, 1), size(a, 2), 1, a, size(a, 1), b, size(b, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine dgels_1d_f95(a, b, info)
        double precision, intent(inout) :: a(:, :)
        double precision, intent(inout) :: b(:)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double precision, allocatable :: work(:)
        double precision :: qwork(1)
        call dgels_f77('N', size(a, 1), size(a, 2), 1, a, size(a, 1), b, size(b, 1), qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call dgels_f77('N', size(a, 1), size(a, 2), 1, a, size(a, 1), b, size(b, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine cgels_1d_f95(a, b, info)
        complex, intent(inout) :: a(:, :)
        complex, intent(inout) :: b(:)
        integer, intent(out), optional :: info
        integer :: i, lwork
        complex, allocatable :: work(:)
        complex :: qwork(1)
        call cgels_f77('N', size(a, 1), size(a, 2), 1, a, size(a, 1), b, size(b, 1), qwork, -1, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call cgels_f77('N', size(a, 1), size(a, 2), 1, a, size(a, 1), b, size(b, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine zgels_1d_f95(a, b, info)
        double complex, intent(inout) :: a(:, :)
        double complex, intent(inout) :: b(:)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double complex, allocatable :: work(:)
        double complex :: qwork(1)
        call zgels_f77('N', size(a, 1), size(a, 2), 1, a, size(a, 1), b, size(b, 1), qwork, -1, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call zgels_f77('N', size(a, 1), size(a, 2), 1, a, size(a, 1), b, size(b, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine sgels_2d_f95(a, b, info)
        real, intent(inout) :: a(:, :)
        real, intent(inout) :: b(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        real, allocatable :: work(:)
        real :: qwork(1)
        call sgels_f77('N', size(a, 1), size(a, 2), size(b, 2), a, size(a, 1), b, size(b, 1), qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call sgels_f77('N', size(a, 1), size(a, 2), size(b, 2), a, size(a, 1), b, size(b, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine dgels_2d_f95(a, b, info)
        double precision, intent(inout) :: a(:, :)
        double precision, intent(inout) :: b(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double precision, allocatable :: work(:)
        double precision :: qwork(1)
        call dgels_f77('N', size(a, 1), size(a, 2), size(b, 2), a, size(a, 1), b, size(b, 1), qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call dgels_f77('N', size(a, 1), size(a, 2), size(b, 2), a, size(a, 1), b, size(b, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine cgels_2d_f95(a, b, info)
        complex, intent(inout) :: a(:, :)
        complex, intent(inout) :: b(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        complex, allocatable :: work(:)
        complex :: qwork(1)
        call cgels_f77('N', size(a, 1), size(a, 2), size(b, 2), a, size(a, 1), b, size(b, 1), qwork, -1, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call cgels_f77('N', size(a, 1), size(a, 2), size(b, 2), a, size(a, 1), b, size(b, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine zgels_2d_f95(a, b, info)
        double complex, intent(inout) :: a(:, :)
        double complex, intent(inout) :: b(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double complex, allocatable :: work(:)
        double complex :: qwork(1)
        call zgels_f77('N', size(a, 1), size(a, 2), size(b, 2), a, size(a, 1), b, size(b, 1), qwork, -1, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call zgels_f77('N', size(a, 1), size(a, 2), size(b, 2), a, size(a, 1), b, size(b, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine sgbsv_f95(a, b, kl, info)
        real, intent(inout) :: a(:, :)
        real, intent(inout) :: b(:)
        integer, intent(in) :: kl
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 2)), ku
        ku = size(a, 1) - 2*kl - 1
        call sgbsv_f77(size(a, 2), kl, ku, 1, a, size(a, 1), ipiv, b, size(a, 2), i)
        if (present(info)) info = i
    end subroutine
    subroutine dgbsv_f95(a, b, kl, info)
        double precision, intent(inout) :: a(:, :)
        double precision, intent(inout) :: b(:)
        integer, intent(in) :: kl
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 2)), ku
        ku = size(a, 1) - 2*kl - 1
        call dgbsv_f77(size(a, 2), kl, ku, 1, a, size(a, 1), ipiv, b, size(a, 2), i)
        if (present(info)) info = i
    end subroutine
    subroutine cgbsv_f95(a, b, kl, info)
        complex, intent(inout) :: a(:, :)
        complex, intent(inout) :: b(:)
        integer, intent(in) :: kl
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 2)), ku
        ku = size(a, 1) - 2*kl - 1
        call cgbsv_f77(size(a, 2), kl, ku, 1, a, size(a, 1), ipiv, b, size(a, 2), i)
        if (present(info)) info = i
    end subroutine
    subroutine zgbsv_f95(a, b, kl, info)
        double complex, intent(inout) :: a(:, :)
        double complex, intent(inout) :: b(:)
        integer, intent(in) :: kl
        integer, intent(out), optional :: info
        integer :: i, ipiv(size(a, 2)), ku
        ku = size(a, 1) - 2*kl - 1
        call zgbsv_f77(size(a, 2), kl, ku, 1, a, size(a, 1), ipiv, b, size(a, 2), i)
        if (present(info)) info = i
    end subroutine
    subroutine sgeev_f95(a, wr, wi, vl, vr, info)
        real, intent(inout) :: a(:, :)
        real, intent(out) :: wr(:), wi(:)
        real, intent(out), optional :: vl(:, :), vr(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        real, allocatable :: work(:)
        real :: qwork(1)
        character :: jobl, jobr
        jobl = 'N'; jobr = 'N'
        if (present(vl)) jobl = 'V'
        if (present(vr)) jobr = 'V'
        call sgeev_f77(jobl, jobr, size(a, 1), a, size(a, 1), wr, wi, vl, size(a, 1), vr, size(a, 1), qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call sgeev_f77(jobl, jobr, size(a, 1), a, size(a, 1), wr, wi, vl, size(a, 1), vr, size(a, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine dgeev_f95(a, wr, wi, vl, vr, info)
        double precision, intent(inout) :: a(:, :)
        double precision, intent(out) :: wr(:), wi(:)
        double precision, intent(out), optional :: vl(:, :), vr(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double precision, allocatable :: work(:)
        double precision :: qwork(1)
        character :: jobl, jobr
        jobl = 'N'; jobr = 'N'
        if (present(vl)) jobl = 'V'
        if (present(vr)) jobr = 'V'
        call dgeev_f77(jobl, jobr, size(a, 1), a, size(a, 1), wr, wi, vl, size(a, 1), vr, size(a, 1), qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call dgeev_f77(jobl, jobr, size(a, 1), a, size(a, 1), wr, wi, vl, size(a, 1), vr, size(a, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine cgeev_f95(a, w, vl, vr, info)
        complex, intent(inout) :: a(:, :)
        complex, intent(out) :: w(:)
        complex, intent(out), optional :: vl(:, :), vr(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        complex, allocatable :: work(:)
        complex :: qwork(1)
        real, allocatable :: rwork(:)
        character :: jobl, jobr
        jobl = 'N'; jobr = 'N'
        if (present(vl)) jobl = 'V'
        if (present(vr)) jobr = 'V'
        allocate(rwork(max(1, 2*size(a, 1))))
        call cgeev_f77(jobl, jobr, size(a, 1), a, size(a, 1), w, vl, size(a, 1), vr, size(a, 1), qwork, -1, rwork, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call cgeev_f77(jobl, jobr, size(a, 1), a, size(a, 1), w, vl, size(a, 1), vr, size(a, 1), work, lwork, rwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine zgeev_f95(a, w, vl, vr, info)
        double complex, intent(inout) :: a(:, :)
        double complex, intent(out) :: w(:)
        double complex, intent(out), optional :: vl(:, :), vr(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double complex, allocatable :: work(:)
        double complex :: qwork(1)
        double precision, allocatable :: rwork(:)
        character :: jobl, jobr
        jobl = 'N'; jobr = 'N'
        if (present(vl)) jobl = 'V'
        if (present(vr)) jobr = 'V'
        allocate(rwork(max(1, 2*size(a, 1))))
        call zgeev_f77(jobl, jobr, size(a, 1), a, size(a, 1), w, vl, size(a, 1), vr, size(a, 1), qwork, -1, rwork, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call zgeev_f77(jobl, jobr, size(a, 1), a, size(a, 1), w, vl, size(a, 1), vr, size(a, 1), work, lwork, rwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine sggev_f95(a, b, alphar, alphai, beta, vl, vr, info)
        real, intent(inout) :: a(:, :), b(:, :)
        real, intent(out) :: alphar(:), alphai(:), beta(:)
        real, intent(out), optional :: vl(:, :), vr(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        real, allocatable :: work(:)
        real :: qwork(1)
        character :: jobl, jobr
        jobl = 'N'; jobr = 'N'
        if (present(vl)) jobl = 'V'
        if (present(vr)) jobr = 'V'
        call sggev_f77(jobl, jobr, size(a, 1), a, size(a, 1), b, size(a, 1), alphar, alphai, beta, vl, size(a, 1), vr, size(a, 1), qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call sggev_f77(jobl, jobr, size(a, 1), a, size(a, 1), b, size(a, 1), alphar, alphai, beta, vl, size(a, 1), vr, size(a, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine dggev_f95(a, b, alphar, alphai, beta, vl, vr, info)
        double precision, intent(inout) :: a(:, :), b(:, :)
        double precision, intent(out) :: alphar(:), alphai(:), beta(:)
        double precision, intent(out), optional :: vl(:, :), vr(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double precision, allocatable :: work(:)
        double precision :: qwork(1)
        character :: jobl, jobr
        jobl = 'N'; jobr = 'N'
        if (present(vl)) jobl = 'V'
        if (present(vr)) jobr = 'V'
        call dggev_f77(jobl, jobr, size(a, 1), a, size(a, 1), b, size(a, 1), alphar, alphai, beta, vl, size(a, 1), vr, size(a, 1), qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call dggev_f77(jobl, jobr, size(a, 1), a, size(a, 1), b, size(a, 1), alphar, alphai, beta, vl, size(a, 1), vr, size(a, 1), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine cggev_f95(a, b, alpha, beta, vl, vr, info)
        complex, intent(inout) :: a(:, :), b(:, :)
        complex, intent(out) :: alpha(:), beta(:)
        complex, intent(out), optional :: vl(:, :), vr(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        complex, allocatable :: work(:)
        complex :: qwork(1)
        real, allocatable :: rwork(:)
        character :: jobl, jobr
        jobl = 'N'; jobr = 'N'
        if (present(vl)) jobl = 'V'
        if (present(vr)) jobr = 'V'
        allocate(rwork(8*size(a, 1)))
        call cggev_f77(jobl, jobr, size(a, 1), a, size(a, 1), b, size(a, 1), alpha, beta, vl, size(a, 1), vr, size(a, 1), qwork, -1, rwork, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call cggev_f77(jobl, jobr, size(a, 1), a, size(a, 1), b, size(a, 1), alpha, beta, vl, size(a, 1), vr, size(a, 1), work, lwork, rwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine zggev_f95(a, b, alpha, beta, vl, vr, info)
        double complex, intent(inout) :: a(:, :), b(:, :)
        double complex, intent(out) :: alpha(:), beta(:)
        double complex, intent(out), optional :: vl(:, :), vr(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double complex, allocatable :: work(:)
        double complex :: qwork(1)
        double precision, allocatable :: rwork(:)
        character :: jobl, jobr
        jobl = 'N'; jobr = 'N'
        if (present(vl)) jobl = 'V'
        if (present(vr)) jobr = 'V'
        allocate(rwork(8*size(a, 1)))
        call zggev_f77(jobl, jobr, size(a, 1), a, size(a, 1), b, size(a, 1), alpha, beta, vl, size(a, 1), vr, size(a, 1), qwork, -1, rwork, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call zggev_f77(jobl, jobr, size(a, 1), a, size(a, 1), b, size(a, 1), alpha, beta, vl, size(a, 1), vr, size(a, 1), work, lwork, rwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine sgesvd_f95(a, s, u, vt, info)
        real, intent(inout) :: a(:, :)
        real, intent(out) :: s(:)
        real, intent(out), optional :: u(:, :), vt(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        real, allocatable :: work(:)
        real :: qwork(1)
        character :: jobu, jobvt
        jobu = 'N'; jobvt = 'N'
        if (present(u)) jobu = 'A'
        if (present(vt)) jobvt = 'A'
        call sgesvd_f77(jobu, jobvt, size(a, 1), size(a, 2), a, size(a, 1), s, u, size(a, 1), vt, size(a, 2), qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call sgesvd_f77(jobu, jobvt, size(a, 1), size(a, 2), a, size(a, 1), s, u, size(a, 1), vt, size(a, 2), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine dgesvd_f95(a, s, u, vt, info)
        double precision, intent(inout) :: a(:, :)
        double precision, intent(out) :: s(:)
        double precision, intent(out), optional :: u(:, :), vt(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double precision, allocatable :: work(:)
        double precision :: qwork(1)
        character :: jobu, jobvt
        jobu = 'N'; jobvt = 'N'
        if (present(u)) jobu = 'A'
        if (present(vt)) jobvt = 'A'
        call dgesvd_f77(jobu, jobvt, size(a, 1), size(a, 2), a, size(a, 1), s, u, size(a, 1), vt, size(a, 2), qwork, -1, i)
        lwork = int(qwork(1))
        allocate(work(max(1, lwork)))
        call dgesvd_f77(jobu, jobvt, size(a, 1), size(a, 2), a, size(a, 1), s, u, size(a, 1), vt, size(a, 2), work, lwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine cgesvd_f95(a, s, u, vt, info)
        complex, intent(inout) :: a(:, :)
        real, intent(out) :: s(:)
        complex, intent(out), optional :: u(:, :), vt(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        complex, allocatable :: work(:)
        complex :: qwork(1)
        real, allocatable :: rwork(:)
        character :: jobu, jobvt
        jobu = 'N'; jobvt = 'N'
        if (present(u)) jobu = 'A'
        if (present(vt)) jobvt = 'A'
        allocate(rwork(5*min(size(a, 1), size(a, 2))))
        call cgesvd_f77(jobu, jobvt, size(a, 1), size(a, 2), a, size(a, 1), s, u, size(a, 1), vt, size(a, 2), qwork, -1, rwork, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call cgesvd_f77(jobu, jobvt, size(a, 1), size(a, 2), a, size(a, 1), s, u, size(a, 1), vt, size(a, 2), work, lwork, rwork, i)
        if (present(info)) info = i
    end subroutine
    subroutine zgesvd_f95(a, s, u, vt, info)
        double complex, intent(inout) :: a(:, :)
        double precision, intent(out) :: s(:)
        double complex, intent(out), optional :: u(:, :), vt(:, :)
        integer, intent(out), optional :: info
        integer :: i, lwork
        double complex, allocatable :: work(:)
        double complex :: qwork(1)
        double precision, allocatable :: rwork(:)
        character :: jobu, jobvt
        jobu = 'N'; jobvt = 'N'
        if (present(u)) jobu = 'A'
        if (present(vt)) jobvt = 'A'
        allocate(rwork(5*min(size(a, 1), size(a, 2))))
        call zgesvd_f77(jobu, jobvt, size(a, 1), size(a, 2), a, size(a, 1), s, u, size(a, 1), vt, size(a, 2), qwork, -1, rwork, i)
        lwork = int(real(qwork(1)))
        allocate(work(max(1, lwork)))
        call zgesvd_f77(jobu, jobvt, size(a, 1), size(a, 2), a, size(a, 1), s, u, size(a, 1), vt, size(a, 2), work, lwork, rwork, i)
        if (present(info)) info = i
    end subroutine
end module lapack95
! F77 Wrappers
subroutine sgetrf_f77(m, n, a, lda, ipiv, info)
    integer :: m, n, lda, ipiv(*), info
    real :: a(*)
    external sgetrf
    call sgetrf(m, n, a, lda, ipiv, info)
end subroutine
subroutine dgetrf_f77(m, n, a, lda, ipiv, info)
    integer :: m, n, lda, ipiv(*), info
    double precision :: a(*)
    external dgetrf
    call dgetrf(m, n, a, lda, ipiv, info)
end subroutine
subroutine cgetrf_f77(m, n, a, lda, ipiv, info)
    integer :: m, n, lda, ipiv(*), info
    complex :: a(*)
    external cgetrf
    call cgetrf(m, n, a, lda, ipiv, info)
end subroutine
subroutine zgetrf_f77(m, n, a, lda, ipiv, info)
    integer :: m, n, lda, ipiv(*), info
    double complex :: a(*)
    external zgetrf
    call zgetrf(m, n, a, lda, ipiv, info)
end subroutine
subroutine sgetri_f77(n, a, lda, ipiv, work, lwork, info)
    integer :: n, lda, ipiv(*), lwork, info
    real :: a(*), work(*)
    external sgetri
    call sgetri(n, a, lda, ipiv, work, lwork, info)
end subroutine
subroutine dgetri_f77(n, a, lda, ipiv, work, lwork, info)
    integer :: n, lda, ipiv(*), lwork, info
    double precision :: a(*), work(*)
    external dgetri
    call dgetri(n, a, lda, ipiv, work, lwork, info)
end subroutine
subroutine cgetri_f77(n, a, lda, ipiv, work, lwork, info)
    integer :: n, lda, ipiv(*), lwork, info
    complex :: a(*), work(*)
    external cgetri
    call cgetri(n, a, lda, ipiv, work, lwork, info)
end subroutine
subroutine zgetri_f77(n, a, lda, ipiv, work, lwork, info)
    integer :: n, lda, ipiv(*), lwork, info
    double complex :: a(*), work(*)
    external zgetri
    call zgetri(n, a, lda, ipiv, work, lwork, info)
end subroutine
subroutine sgesv_f77(n, nrhs, a, lda, ipiv, b, ldb, info)
    integer :: n, nrhs, lda, ipiv(*), ldb, info
    real :: a(*), b(*)
    external sgesv
    call sgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
end subroutine
subroutine dgesv_f77(n, nrhs, a, lda, ipiv, b, ldb, info)
    integer :: n, nrhs, lda, ipiv(*), ldb, info
    double precision :: a(*), b(*)
    external dgesv
    call dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
end subroutine
subroutine cgesv_f77(n, nrhs, a, lda, ipiv, b, ldb, info)
    integer :: n, nrhs, lda, ipiv(*), ldb, info
    complex :: a(*), b(*)
    external cgesv
    call cgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
end subroutine
subroutine zgesv_f77(n, nrhs, a, lda, ipiv, b, ldb, info)
    integer :: n, nrhs, lda, ipiv(*), ldb, info
    double complex :: a(*), b(*)
    external zgesv
    call zgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
end subroutine
subroutine sgels_f77(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
    character :: trans
    integer :: m, n, nrhs, lda, ldb, lwork, info
    real :: a(*), b(*), work(*)
    external sgels
    call sgels(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
end subroutine
subroutine dgels_f77(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
    character :: trans
    integer :: m, n, nrhs, lda, ldb, lwork, info
    double precision :: a(*), b(*), work(*)
    external dgels
    call dgels(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
end subroutine
subroutine cgels_f77(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
    character :: trans
    integer :: m, n, nrhs, lda, ldb, lwork, info
    complex :: a(*), b(*), work(*)
    external cgels
    call cgels(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
end subroutine
subroutine zgels_f77(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
    character :: trans
    integer :: m, n, nrhs, lda, ldb, lwork, info
    double complex :: a(*), b(*), work(*)
    external zgels
    call zgels(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
end subroutine
subroutine sgbsv_f77(n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info)
    integer :: n, kl, ku, nrhs, ldab, ipiv(*), ldb, info
    real :: ab(*), b(*)
    external sgbsv
    call sgbsv(n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info)
end subroutine
subroutine dgbsv_f77(n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info)
    integer :: n, kl, ku, nrhs, ldab, ipiv(*), ldb, info
    double precision :: ab(*), b(*)
    external dgbsv
    call dgbsv(n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info)
end subroutine
subroutine cgbsv_f77(n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info)
    integer :: n, kl, ku, nrhs, ldab, ipiv(*), ldb, info
    complex :: ab(*), b(*)
    external cgbsv
    call cgbsv(n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info)
end subroutine
subroutine zgbsv_f77(n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info)
    integer :: n, kl, ku, nrhs, ldab, ipiv(*), ldb, info
    double complex :: ab(*), b(*)
    external zgbsv
    call zgbsv(n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info)
end subroutine
subroutine sgeev_f77(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr, work, lwork, info)
    character :: jobvl, jobvr
    integer :: n, lda, ldvl, ldvr, lwork, info
    real :: a(*), wr(*), wi(*), vl(*), vr(*), work(*)
    external sgeev
    call sgeev(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr, work, lwork, info)
end subroutine
subroutine dgeev_f77(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr, work, lwork, info)
    character :: jobvl, jobvr
    integer :: n, lda, ldvl, ldvr, lwork, info
    double precision :: a(*), wr(*), wi(*), vl(*), vr(*), work(*)
    external dgeev
    call dgeev(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr, work, lwork, info)
end subroutine
subroutine cgeev_f77(jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr, work, lwork, rwork, info)
    character :: jobvl, jobvr
    integer :: n, lda, ldvl, ldvr, lwork, info
    complex :: a(*), w(*), vl(*), vr(*), work(*)
    real :: rwork(*)
    external cgeev
    call cgeev(jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr, work, lwork, rwork, info)
end subroutine
subroutine zgeev_f77(jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr, work, lwork, rwork, info)
    character :: jobvl, jobvr
    integer :: n, lda, ldvl, ldvr, lwork, info
    double complex :: a(*), w(*), vl(*), vr(*), work(*)
    double precision :: rwork(*)
    external zgeev
    call zgeev(jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr, work, lwork, rwork, info)
end subroutine
subroutine sggev_f77(jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai, beta, vl, ldvl, vr, ldvr, work, lwork, info)
    character :: jobvl, jobvr
    integer :: n, lda, ldb, ldvl, ldvr, lwork, info
    real :: a(*), b(*), alphar(*), alphai(*), beta(*), vl(*), vr(*), work(*)
    external sggev
    call sggev(jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai, beta, vl, ldvl, vr, ldvr, work, lwork, info)
end subroutine
subroutine dggev_f77(jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai, beta, vl, ldvl, vr, ldvr, work, lwork, info)
    character :: jobvl, jobvr
    integer :: n, lda, ldb, ldvl, ldvr, lwork, info
    double precision :: a(*), b(*), alphar(*), alphai(*), beta(*), vl(*), vr(*), work(*)
    external dggev
    call dggev(jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai, beta, vl, ldvl, vr, ldvr, work, lwork, info)
end subroutine
subroutine cggev_f77(jobvl, jobvr, n, a, lda, b, ldb, alpha, beta, vl, ldvl, vr, ldvr, work, lwork, rwork, info)
    character :: jobvl, jobvr
    integer :: n, lda, ldb, ldvl, ldvr, lwork, info
    complex :: a(*), b(*), alpha(*), beta(*), vl(*), vr(*), work(*)
    real :: rwork(*)
    external cggev
    call cggev(jobvl, jobvr, n, a, lda, b, ldb, alpha, beta, vl, ldvl, vr, ldvr, work, lwork, rwork, info)
end subroutine
subroutine zggev_f77(jobvl, jobvr, n, a, lda, b, ldb, alpha, beta, vl, ldvl, vr, ldvr, work, lwork, rwork, info)
    character :: jobvl, jobvr
    integer :: n, lda, ldb, ldvl, ldvr, lwork, info
    double complex :: a(*), b(*), alpha(*), beta(*), vl(*), vr(*), work(*)
    double precision :: rwork(*)
    external zggev
    call zggev(jobvl, jobvr, n, a, lda, b, ldb, alpha, beta, vl, ldvl, vr, ldvr, work, lwork, rwork, info)
end subroutine
subroutine sgesvd_f77(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)
    character :: jobu, jobvt
    integer :: m, n, lda, ldu, ldvt, lwork, info
    real :: a(*), s(*), u(*), vt(*), work(*)
    external sgesvd
    call sgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)
end subroutine
subroutine dgesvd_f77(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)
    character :: jobu, jobvt
    integer :: m, n, lda, ldu, ldvt, lwork, info
    double precision :: a(*), s(*), u(*), vt(*), work(*)
    external dgesvd
    call dgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)
end subroutine
subroutine cgesvd_f77(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, rwork, info)
    character :: jobu, jobvt
    integer :: m, n, lda, ldu, ldvt, lwork, info
    complex :: a(*), u(*), vt(*), work(*)
    real :: s(*), rwork(*)
    external cgesvd
    call cgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, rwork, info)
end subroutine
subroutine zgesvd_f77(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, rwork, info)
    character :: jobu, jobvt
    integer :: m, n, lda, ldu, ldvt, lwork, info
    double complex :: a(*), u(*), vt(*), work(*)
    double precision :: s(*), rwork(*)
    external zgesvd
    call zgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, rwork, info)
end subroutine
