module utilmodule
    implicit none
    private
    !
    public :: inc, astring, astring_arr, arr_to_string
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
        module procedure inc_int, inc_real, inc_int8
    end interface
    !
    interface write(unformatted)
            module procedure astring_write_u
    end interface
    !
    interface write(formatted)
            module procedure astring_write_f
    end interface
contains
    pure subroutine inc_int(a, b)
        implicit none
        !
        integer, intent(inout) :: a
        integer, intent(in)    :: b
        !
        a = a + b
    end subroutine inc_int
    !
    pure subroutine inc_real(a, b)
        implicit none
        !
        real, intent(inout) :: a
        real, intent(in)    :: b
        !
        a = a + b
    end subroutine inc_real
    !
    pure subroutine inc_int8(a, b)
        implicit none
        !
        integer(1), intent(inout) :: a
        integer(1), intent(in)    :: b
        !
        a = a + b
    end subroutine inc_int8
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
    pure function astring_eq(self, rhs) result(eq)
        implicit none
        !
        class(astring), intent(in) :: self, rhs
    end function astring_eq
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
    subroutine astring_write_u(self, unit, iostat, iomsg)
        implicit none
        !
        class(astring), intent(in)    :: self
        integer, intent(in)    :: unit
        integer, intent(out)   :: iostat
        character(len=*), intent(inout) :: iomsg
        !
        write (unit, iostat=iostat, iomsg=iomsg) self%data
    end subroutine
    !
    subroutine astring_write_f(self, unit, iotype, v_list, iostat, iomsg)
        implicit none
        !
        class(astring), intent(in)    :: self
        integer, intent(in)    :: unit
        character(len=*), intent(in)    :: iotype
        integer, intent(in)    :: v_list(:)
        integer, intent(out)   :: iostat
        character(len=*), intent(inout) :: iomsg
        integer :: dummy1
        character :: dummy2
        !
        dummy1 = v_list(1)
        dummy2 = iotype(1:1)
        !
        write (unit, '(A)', iostat=iostat, iomsg=iomsg) self%data
    end subroutine
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
