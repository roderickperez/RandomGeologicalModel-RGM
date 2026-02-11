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
!    Kai Gao, kaigao@lanl.gov
!


#define CONCAT_HELPER(X, Y) X ## _ ## Y
#define CONCAT(X, Y)        CONCAT_HELPER(X, Y)

#define swap_     CONCAT(swap, T)

elemental subroutine swap_(a, b)

    TT, intent(inout) :: a, b

    TT :: t

    t = b
    b = a
    a = t

end subroutine swap_

#undef T
#undef TT

#undef CONCAT_HELPER
#undef CONCAT

#undef swap_

