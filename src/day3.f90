module day3
    use collectormodule_char
    use collectormodule_char_spes
    use utilmodule
    implicit none
    !
    public :: part1, part2
    !
    type step
        character   :: dir
        integer     :: length
    end type
    !
contains
    function loadfile(filename) result(lines)
        implicit none
        !
        character(len=*), intent(in) :: filename
        type(astring_arr) :: lines(2)
        type(collector_char) :: linecollectors(2)
        integer :: ios, i
        character, allocatable :: buffer(:)
        !
        call linecollectors(1)%new(128)
        call linecollectors(2)%new(128)
        !
        open (1, file=filename)
        read (1, *, iostat=ios) linecollectors
        do i = 1, size(lines)
            buffer = linecollectors(i)%toarray()
            lines(i)%data = splitwithdelimiter(buffer, ',')
            deallocate (buffer)
        end do
        close (1)
    end function loadfile
    !
    function parsepath(path) result(res)
        implicit none
        !
        type(astring), intent(in) :: path(:)
        type(step), allocatable   :: res(:)
        integer :: i
        character(len=5) :: buffer
        !
        allocate (res(size(path)))
        !
        do i = 1, size(path)
            call arr_to_string(path(i)%data, buffer)
            read (buffer, '(A1, I10.1)') res(i)%dir, res(i)%length
        end do
    end function parsepath
    !
    subroutine part1()
        implicit none
        !
        type(astring_arr) :: paths(2)
        type(step), allocatable :: path1(:), path2(:)
        integer :: i
        !
        paths = loadfile('inputfiles/day3/example1.txt')
        !
        path1 = parsepath(paths(1)%data)
        path2 = parsepath(paths(2)%data)
        !
        deallocate (paths(1)%data)
        deallocate (paths(2)%data)
        !
        do i = 1, size(path1)
            print *, path1(i)%dir, path1(i)%length
        end do
        do i = 1, size(path2)
            print *, path2(i)%dir, path2(i)%length
        end do
    end subroutine part1
    !
    subroutine testfunc(arr)
        implicit none
        !
        character, intent(in) :: arr(:)
        !
        print *, arr
    end subroutine testfunc
    !
    subroutine part2()
        implicit none
    end subroutine part2
end module day3
