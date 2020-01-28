module collectermodule
    use utilmodule
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
        procedure, public :: push => charCollecter_push
        procedure, public :: len => charCollecter_len
        final             :: finalizer
    end type
    !
    public :: Size
    interface Size
        module procedure charCollecter_len
    end interface
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
    subroutine expand(arr)
        implicit none
        !
        type(charArr), allocatable, intent(inout) :: arr(:)
        type(charArr), allocatable                :: buffer(:)
        integer :: i
        !
        allocate (buffer(size(arr)*scale_factor))
        !
        do i = 1, size(arr)
            buffer(i) = arr(i)
        end do
        deallocate (arr)
        arr = buffer
    end subroutine expand
    !
    subroutine charCollecter_push(self, val)
        implicit none
        !
        class(charCollecter), intent(inout) :: self
        character, intent(in) :: val
        !
        self%data(self%blockindex)%data(self%subindex) = val
        !
        call inc(self%subindex, 1)
        if (self%subindex > self%blocksize) then
            self%subindex = 1
            call inc(self%blockindex, 1)
            if (self%blockindex > size(self%data)) then
                call expand(self%data)
            end if
            allocate (self%data(self%blockindex)%data(self%blocksize))
        end if
    end subroutine charCollecter_push
    !
    subroutine finalizer(self)
        implicit none
        !
        type(charCollecter), intent(inout) :: self
        !
        integer :: i
        !
        do i = 1, self%blockindex
            deallocate (self%data(i)%data)
        end do
        deallocate (self%data)
    end subroutine finalizer
    !
    function charCollecter_len(self) result(ret)
        implicit none
        !
        class(charCollecter), intent(in) :: self
        integer :: ret
        !
        ret = (self%blockindex - 1) * self%blocksize + self%subindex - 1
    end function charCollecter_len
    !
    function charCollecter_toarray(self) result(ret)
        implicit none
        !
        class(charCollecter), intent(in) :: self
        character, allocatable :: ret(:)
        !
        allocate (ret(self%len()))
    end function charCollecter_toarray
end module collectermodule
