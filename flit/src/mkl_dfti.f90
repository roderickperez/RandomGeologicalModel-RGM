module mkl_dfti
    implicit none
    
    type :: dfti_descriptor
        integer :: dummy
    end type dfti_descriptor
    
    integer, parameter :: dfti_single = 1
    integer, parameter :: dfti_double = 2
    integer, parameter :: dfti_complex = 3
    integer, parameter :: dfti_real = 4
    integer, parameter :: dfti_backward_scale = 5
    integer, parameter :: dfti_forward_scale = 6
    integer, parameter :: dfti_number_of_transforms = 7
    integer, parameter :: dfti_input_strides = 8

    interface dfticreatedescriptor
        module procedure dfticreatedescriptor_s, dfticreatedescriptor_v
    end interface
    
    interface dfticomputeforward
        module procedure dfticomputeforward_s, dfticomputeforward_d
    end interface

    interface dfticomputebackward
        module procedure dfticomputebackward_s, dfticomputebackward_d
    end interface

contains

    integer function dfticreatedescriptor_s(h, precision, domain, dim, length)
        type(dfti_descriptor), pointer :: h
        integer, intent(in) :: precision, domain, dim
        integer, intent(in) :: length
        allocate(h)
        dfticreatedescriptor_s = 0
    end function

    integer function dfticreatedescriptor_v(h, precision, domain, dim, length)
        type(dfti_descriptor), pointer :: h
        integer, intent(in) :: precision, domain, dim
        integer, intent(in) :: length(*)
        allocate(h)
        dfticreatedescriptor_v = 0
    end function

    integer function dfticommitdescriptor(h)
        type(dfti_descriptor), pointer :: h
        dfticommitdescriptor = 0
    end function

    integer function dfticomputeforward_s(h, array)
        type(dfti_descriptor), pointer :: h
        complex, dimension(*) :: array
        dfticomputeforward_s = 0
    end function

    integer function dfticomputeforward_d(h, array)
        type(dfti_descriptor), pointer :: h
        double complex, dimension(*) :: array
        dfticomputeforward_d = 0
    end function

    integer function dfticomputebackward_s(h, array)
        type(dfti_descriptor), pointer :: h
        complex, dimension(*) :: array
        dfticomputebackward_s = 0
    end function

    integer function dfticomputebackward_d(h, array)
        type(dfti_descriptor), pointer :: h
        double complex, dimension(*) :: array
        dfticomputebackward_d = 0
    end function

    integer function dftisetvalue(h, param, value)
        type(dfti_descriptor), pointer :: h
        integer, intent(in) :: param
        class(*), intent(in) :: value
        dftisetvalue = 0
    end function

    integer function dftifreedescriptor(h)
        type(dfti_descriptor), pointer :: h
        if (associated(h)) deallocate(h)
        dftifreedescriptor = 0
    end function

end module mkl_dfti
