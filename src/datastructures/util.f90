module utilmodule
    use hashmodule
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
        procedure       :: astring_eq
        generic, public :: operator(==) => astring_eq
        procedure       :: astring_neq
        generic, public :: operator(/=) => astring_neq
    end type
    !
    public :: size
    interface size
        module procedure astring_size
    end interface
    !
    public :: hash
    interface hash
        module procedure astring_hash
    end interface
    !
    type astring_arr
        type(astring), allocatable :: data(:)
    contains
        final :: astring_arrfinalizer
    end type
    !
    public :: write (unformatted)
    interface write (unformatted)
        module procedure astring_write_u
    end interface
    !
    public :: write (formatted)
    interface write (formatted)
        module procedure astring_write_f
    end interface
    !
    interface inc
        module procedure inc_int, inc_real, inc_int8
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
        call astringfinalizer(self)
        allocate (self%data(len_trim(str)))
        !
        do i = 1, size(self%data)
            self%data(i) = str(i:i)
        end do
    end subroutine new_astring_from_string
    !
    pure integer function astring_size(self) result(s)
        implicit none
        !
        type(astring), intent(in) :: self
        !
        s = size(self%data)
    end function astring_size
    !
    pure logical function astring_eq(self, rhs) result(eq)
        implicit none
        !
        class(astring), intent(in) :: self
        type(astring), intent(in) :: rhs
        integer :: i
        !
        eq = .true.
        if (size(self) /= size(rhs)) then
            eq = .false.
        else
            do i = 1, size(self)
                eq = self%data(i) == rhs%data(i)
                if (.not. eq) return
            end do
        end if
    end function astring_eq
    !
    pure logical function astring_neq(self, rhs) result(eq)
        implicit none
        !
        class(astring), intent(in) :: self
        type(astring), intent(in)  :: rhs
        !
        eq = .not. (self == rhs)
    end function astring_neq
    !
    pure integer function astring_hash(self) result(h)
        implicit none
        !
        type(astring), intent(in) :: self
        !
        h = hash(self%data)
    end function astring_hash
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
        print *, self%data
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
        !
        dummy1 = size(v_list)
        dummy1 = len(iotype)
        !
        write (unit, *, iostat=iostat, iomsg=iomsg) self%data
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
