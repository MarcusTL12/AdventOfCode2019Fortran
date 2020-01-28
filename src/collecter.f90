module collectermodule
    implicit none
    !
    public :: charCollecter, charCollecter_new
    !
    integer :: initial_blockamt = 8, scale_factor = 2
    !
    type charArr
        character, allocatable :: data(:)
    end type
    !
    type charCollecter
        integer :: blocksize, blockindex, subindex
        type(charArr), allocatable :: data(:)
    contains
        procedure, public :: New => charCollecter_new
        final             :: finalizer
    end type
contains
    subroutine charCollecter_new(self, blocksize)
        implicit none
        class(charCollecter), intent(inout) :: self
        integer, intent(in)                 :: blocksize
        !
        self%blocksize = blocksize
        self%blockindex = 1
        self%subindex = 1
        allocate (self%data(initial_blockamt))
        allocate (self%data(1)%data(blocksize))
    end subroutine charCollecter_new
    !
    subroutine finalizer(self)
        implicit none
        !
        type(charCollecter), intent(inout) :: self
        !
        integer :: i
        !
        do i = 1, self%blockindex - 1
            deallocate (self%data(i)%data)
        end do
    end subroutine finalizer
end module collectermodule
