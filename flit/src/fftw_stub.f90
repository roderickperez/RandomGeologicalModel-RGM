! Stub for FFTW routines to satisfy linker
! WARNING: This stub does NOTHING. Code using FFT will produce wrong results.

subroutine sfftw_plan_r2r_1d(plan, n, in, out, kind, flags)
    implicit none
    integer(8) :: plan
    integer :: n
    real(4) :: in(*), out(*)
    integer :: kind, flags
    ! Do nothing
end subroutine

subroutine sfftw_execute(plan)
    implicit none
    integer(8) :: plan
    ! Do nothing
end subroutine

subroutine sfftw_destroy_plan(plan)
    implicit none
    integer(8) :: plan
    ! Do nothing
end subroutine

! Double precision
subroutine dfftw_plan_r2r_1d(plan, n, in, out, kind, flags)
    implicit none
    integer(8) :: plan
    integer :: n
    real(8) :: in(*), out(*)
    integer :: kind, flags
    ! Do nothing
end subroutine

subroutine dfftw_execute(plan)
    implicit none
    integer(8) :: plan
    ! Do nothing
end subroutine

subroutine dfftw_destroy_plan(plan)
    implicit none
    integer(8) :: plan
    ! Do nothing
end subroutine
