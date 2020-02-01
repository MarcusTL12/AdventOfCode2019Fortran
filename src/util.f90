module utilmodule
    implicit none
    !
    public :: inc
    !
    type astring
        character, allocatable :: data(:)
    contains
        procedure       :: new_astring_from_arr
        procedure       :: new_astring_from_string
        generic, public :: new => new_astring_from_arr, &
            new_astring_from_string
        final           :: astringfinalizer
    end type
    !
    type astring_arr
        type(astring), allocatable :: data(:)
    contains
        final :: astring_arrfinalizer
    end type
    !
    interface inc
        module procedure inc_int, inc_real
    end interface
contains
    subroutine inc_int(a, b)
        implicit none
        !
        integer, intent(inout) :: a
        integer, intent(in)    :: b
        !
        a = a + b
    end subroutine inc_int
    !
    subroutine inc_real(a, b)
        implicit none
        !
        real, intent(inout) :: a
        real, intent(in)    :: b
        !
        a = a + b
    end subroutine inc_real
    !
    subroutine new_astring_from_string(self, str)
        implicit none
        !
        class(astring), intent(inout) :: self
        character(len=*), intent(in) :: str
        integer        :: i
        !
        allocate (self%data(len_trim(str)))
        !
        do i = 1, size(self%data)
            self%data(i) = str(i:i)
        end do
    end subroutine new_astring_from_string
    !
    subroutine new_astring_from_arr(self, str)
        implicit none
        !
        class(astring), intent(out) :: self
        character :: str(:)
        !
        self%data = str
    end subroutine new_astring_from_arr
    !
    subroutine astringfinalizer(self)
        implicit none
        !
        type(astring), intent(inout) :: self
        !
        if (allocated(self%data)) deallocate (self%data)
    end subroutine astringfinalizer
    !
    subroutine astring_arrfinalizer(self)
        implicit none
        !
        type(astring_arr), intent(inout) :: self
        !
        if (allocated(self%data)) then
            deallocate (self%data)
        end if
    end subroutine astring_arrfinalizer
    !
    subroutine arr_to_string(arr, str)
        implicit none
        !
        character, intent(in) :: arr(:)
        character(len=*), intent(out) :: str
        integer :: i
        !
        str = ''
        do i = 1, size(arr)
            str(i:i) = arr(i)
        end do
    end subroutine arr_to_string
end module utilmodule
