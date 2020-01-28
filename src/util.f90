module utilmodule
    implicit none
    !
    public :: inc
contains
    subroutine inc(a, b)
        implicit none
        !
        integer, intent(inout) :: a
        integer, intent(in)    :: b
        !
        a = a + b
    end subroutine inc
end module utilmodule
