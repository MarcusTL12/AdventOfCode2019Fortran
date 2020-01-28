module day3
    use ftlstringmodule
    use collectermodule
    implicit none
    !
    public :: part1, part2
    !
    type step
        character   :: dir
        integer     :: length
    end type
    !
    type strArr
        type(ftlString), allocatable :: data(:)
    end type
    !
contains
    subroutine loadfile(filename, arr1, arr2)
        implicit none
        !
        character (len=*), intent(in) :: filename
        type(ftlString), allocatable, intent(out) :: arr1(:), arr2(:)
        !
        type(ftlString) :: lines(2)
        integer :: iostatus
        !
        open (unit=1, file=filename)
        read (1, *, iostat=iostatus) lines
        close (1)
        !
        arr1 = lines(1)%Split(',')
        arr2 = lines(2)%Split(',')
    end subroutine loadfile
    !
    subroutine parsepath(pathin, pathout)
        implicit none
        !
        type(ftlString), intent(in)          :: pathin(:)
        type(step), allocatable, intent(out) :: pathout(:)
        !
        integer     :: i
        type(step)  :: step_buffer
        !
        allocate (pathout(size(pathin)))
        !
        do i = 1, size(pathin)
            read (pathin(i)%raw, '(A1, I10.1)') &
                step_buffer%dir, step_buffer%length
            !
            pathout(i) = step_buffer
        end do
    end subroutine parsepath
    !
    subroutine loadandparse(filename, path1, path2)
        implicit none
        !
        character (len=*), intent(in)        :: filename
        type(step), allocatable, intent(out) :: path1(:), path2(:)
        !
        type(ftlString), allocatable :: arr1(:), arr2(:)
        !
        call loadfile(filename, arr1, arr2)
        !
        call parsepath(arr1, path1)
        call parsepath(arr2, path2)
        !
        deallocate (arr1, arr2)
    end subroutine loadandparse
    !
    subroutine loadfile2(filename, out)
        implicit none
        !
        character (len=*), intent(in) :: filename
        type(strArr), allocatable, intent(out) :: out
        !
        
    end subroutine loadfile2
    !
    pure integer function countlines(filename)
        implicit none
        !
        character (len=*), intent(in) :: filename
        !
        countlines = len(filename)
    end function
    !
    subroutine part1()
        implicit none
        !
        type(step), allocatable :: path1(:), path2(:)
        !
        call loadandparse('inputfiles/day3/example1.txt', path1, path2)
        !
        print *, path1, path2
        !
        deallocate (path1, path2)
    end subroutine part1
    !
    subroutine part2()
        implicit none
        !
        type(charCollecter) :: tempcol
        !
        call tempcol%New(100)
        !
    end subroutine part2
    !
    ! Want this to be here for future reference
    pure function countcomma(str) result(ans)
        implicit none
        !
        type(ftlString), intent(in) :: str
        integer :: ans, i
        !
        ans = 0
        do i = 1, len(str)
            if (str%raw(i:i) == ',') ans = ans + 1
        end do
    end function countcomma
end module day3
