module utilmodule
    implicit none
    !
    public :: inc
    !
    type char_arr
        character, allocatable :: data(:)
    contains
        procedure       :: new_char_arr_from_arr
        procedure       :: new_char_arr_from_string
        generic, public :: New => new_char_arr_from_arr, &
            new_char_arr_from_string
    end type
contains
    subroutine inc(a, b)
        implicit none
        !
        integer, intent(inout) :: a
        integer, intent(in)    :: b
        !
        a = a + b
    end subroutine inc
    !
    subroutine new_char_arr_from_string(self, str)
        implicit none
        !
        class(char_arr), intent(inout) :: self
        character(len=*), intent(in) :: str
        integer        :: i
        !
        allocate (self%data(len_trim(str)))
        !
        do i = 1, size(self%data)
            self%data(i) = str(i:i)
        end do
    end subroutine new_char_arr_from_string
    !
    subroutine new_char_arr_from_arr(self, str)
        implicit none
        !
        class(char_arr), intent(out) :: self
        character :: str(:)
        !
        self%data = str
    end subroutine new_char_arr_from_arr
end module utilmodule
