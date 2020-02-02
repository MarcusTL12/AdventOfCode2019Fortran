module hashmodule
    implicit none
    private
    !
    integer :: seed1 = 543748773, seed2 = 442244848
    !
    public :: hash
    interface hash
        module procedure hashint
        module procedure hashstr
        module procedure hasharr_integer
        module procedure hasharr_character
    end interface
    !
    public :: fusehash
contains
    pure function hashint(a) result(h)
        implicit none
        !
        integer, intent(in) :: a
        integer             :: h
        !
        h = a
    end
    !
    pure subroutine fusehash(a, b)
        implicit none
        !
        integer, intent(inout) :: a
        integer, intent(in)    :: b
        !
        a = seed1 * a + seed2 * b + seed1 * seed2
    end subroutine fusehash
    !
    pure function hashstr(a) result(h)
        implicit none
        !
        character(len=*), intent(in) :: a
        integer :: h, i
        !
        h = seed1
        !
        do i = 1, len_trim(a)
            call fusehash(h, ichar(a(i:i)))
        end do
    end function hashstr
    !
#define TYPE_NAME integer
#include "hasharr.f90_template"

#define TYPE_NAME character
#include "hasharr.f90_template"
    !
end module hashmodule
