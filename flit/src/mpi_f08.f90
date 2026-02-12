module mpi_f08
    implicit none

    type mpi_comm
        integer :: id
    end type mpi_comm

    type mpi_status
        integer :: dummy
    end type mpi_status

    type mpi_datatype
        integer :: id
    end type mpi_datatype

    type mpi_op
        integer :: id
    end type mpi_op

    type(mpi_comm), parameter :: mpi_comm_world = mpi_comm(1)
    integer, parameter :: mpi_proc_null = -1
    integer, parameter :: mpi_err_other = 1
    integer, parameter :: mpi_in_place = -1

    type(mpi_datatype), parameter :: mpi_integer = mpi_datatype(1)
    type(mpi_datatype), parameter :: mpi_real = mpi_datatype(2)
    type(mpi_datatype), parameter :: mpi_double_precision = mpi_datatype(3)
    type(mpi_datatype), parameter :: mpi_complex = mpi_datatype(4)
    type(mpi_datatype), parameter :: mpi_double_complex = mpi_datatype(5)
    type(mpi_datatype), parameter :: mpi_character = mpi_datatype(6)
    type(mpi_datatype), parameter :: mpi_logical = mpi_datatype(7)

    type(mpi_op), parameter :: mpi_min = mpi_op(1)
    type(mpi_op), parameter :: mpi_max = mpi_op(2)
    type(mpi_op), parameter :: mpi_sum = mpi_op(5)
    type(mpi_op), parameter :: mpi_land = mpi_op(3)
    type(mpi_op), parameter :: mpi_lor = mpi_op(4)

contains

    subroutine mpi_init(ierr)
        integer, intent(out) :: ierr
        ierr = 0
    end subroutine

    subroutine mpi_comm_size(comm, size, ierr)
        type(mpi_comm), intent(in) :: comm
        integer, intent(out) :: size, ierr
        size = 1
        ierr = 0
    end subroutine

    subroutine mpi_comm_rank(comm, rank, ierr)
        type(mpi_comm), intent(in) :: comm
        integer, intent(out) :: rank, ierr
        rank = 0
        ierr = 0
    end subroutine

    subroutine mpi_barrier(comm, ierr)
        type(mpi_comm), intent(in) :: comm
        integer, intent(out) :: ierr
        ierr = 0
    end subroutine

    subroutine mpi_finalize(ierr)
        integer, intent(out) :: ierr
        ierr = 0
    end subroutine

    subroutine mpi_abort(comm, errorcode, ierr)
        type(mpi_comm), intent(in) :: comm
        integer, intent(in) :: errorcode
        integer, intent(out) :: ierr
        ierr = 0
    end subroutine

    subroutine mpi_bcast(buffer, count, datatype, root, comm, ierr)
        type(*), dimension(*) :: buffer
        integer, intent(in) :: count, root
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_comm), intent(in) :: comm
        integer, intent(out) :: ierr
        ierr = 0
    end subroutine

    subroutine mpi_allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierr)
        type(*), dimension(*) :: sendbuf
        type(*), dimension(*) :: recvbuf
        integer, intent(in) :: count
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        integer, intent(out) :: ierr
        ierr = 0
    end subroutine

    subroutine mpi_reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierr)
        type(*), dimension(*) :: sendbuf
        type(*), dimension(*) :: recvbuf
        integer, intent(in) :: count, root
        type(mpi_datatype), intent(in) :: datatype
        type(mpi_op), intent(in) :: op
        type(mpi_comm), intent(in) :: comm
        integer, intent(out) :: ierr
        ierr = 0
    end subroutine

    subroutine mpi_sendrecv(sendbuf, sendcount, sendtype, dest, sendtag, &
                            recvbuf, recvcount, recvtype, source, recvtag, &
                            comm, status, ierr)
        type(*), dimension(*) :: sendbuf
        integer, intent(in) :: sendcount
        type(mpi_datatype), intent(in) :: sendtype
        integer, intent(in) :: dest, sendtag
        type(*), dimension(*) :: recvbuf
        integer, intent(in) :: recvcount
        type(mpi_datatype), intent(in) :: recvtype
        integer, intent(in) :: source, recvtag
        type(mpi_comm), intent(in) :: comm
        type(mpi_status), intent(out) :: status
        integer, intent(out) :: ierr
        ierr = 0
    end subroutine

end module mpi_f08
