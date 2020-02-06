module day3
    use collectormodule_char
    use collectormodule_char_spes
    use utilmodule
    use hashmapmodule_int_int
    use hashmapmodule_str_int
    implicit none
    !
    public :: part1, part2
    !
    type step
        character   :: dir
        integer     :: length
    end type
    !
    ! type point
    !     integer :: data(2)
    ! end type
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
    pure function walkpath(path) result(ret)
        implicit none
        !
        type(step), intent(in) :: path(:)
        integer, allocatable :: ret(:, :)
        !
        integer :: i, j, curind, pathlen, curdir(2), curpos(2)
        !
        pathlen = 0
        !
        do i = 1, size(path)
            pathlen = pathlen + path(i)%length
        end do
        !
        allocate (ret(pathlen, 2))
        curpos = (/0, 0/)
        curind = 1
        !
        do i = 1, size(path)
            curdir = dir2point(path(i)%dir)
            do j = 1, path(i)%length
                curpos = curpos + curdir
                ret(curind, :) = curpos
                curind = curind + 1
            end do
        end do
    contains
        pure function dir2point(dir) result(ret)
            implicit none
            !
            character, intent(in) :: dir
            integer :: ret(2)
            !
            select case (dir)
            case ('R')
                ret = (/1, 0/)
            case ('U')
                ret = (/0, 1/)
            case ('L')
                ret = (/-1, 0/)
            case ('D')
                ret = (/0, -1/)
            case default
                ret = (/0, 0/)
            end select
        end function dir2point
    end function walkpath
    !
    subroutine filetopathpoints(filename, path1points, path2points)
        implicit none
        !
        character(len=*), intent(in) :: filename
        integer, allocatable, intent(out) :: path1points(:, :), &
                                             path2points(:, :)
        type(astring_arr) :: paths(2)
        type(step), allocatable :: path1(:), path2(:)
        !
        paths = loadfile(filename)
        path1 = parsepath(paths(1)%data)
        path2 = parsepath(paths(2)%data)
        path1points = walkpath(path1)
        path2points = walkpath(path2)
        deallocate (paths(1)%data, paths(2)%data, path1, path2)
    end subroutine filetopathpoints
    !
    pure function minmax_xy(points) result(ret)
        implicit none
        !
        integer, intent(in) :: points(:, :)
        integer             :: ret(4)
        !
        ret = (/minval(points(:, 1)), minval(points(:, 2)), &
                maxval(points(:, 1)), maxval(points(:, 2))/)
    end function minmax_xy
    !
    pure function minmax_xy_paths(path1, path2) result(ret)
        implicit none
        !
        integer, intent(in) :: path1(:, :), path2(:, :)
        integer             :: ret(4), b1(4), b2(4)
        !
        b1 = minmax_xy(path1)
        b2 = minmax_xy(path2)
        !
        ret(1:2) = min(b1(1:2), b2(1:2))
        ret(3:4) = max(b1(3:4), b2(3:4))
    end function minmax_xy_paths
    !
    function offset_bound(bound, path1, path2) result(origin)
        implicit none
        !
        integer, intent(out) :: path1(:, :), path2(:, :)
        integer, intent(inout) :: bound(4)
        !
        integer :: offset(2), origin(2), i
        offset = (/1, 1/) - bound(1:2)
        origin = offset
        !
        bound(1:2) = bound(1:2) + offset
        bound(3:4) = bound(3:4) + offset
        !
        do i = 1, size(path1, 1)
            path1(i, :) = path1(i, :) + offset
        end do
        do i = 1, size(path2, 1)
            path2(i, :) = path2(i, :) + offset
        end do
    end function offset_bound
    !
    subroutine fillmap(map, path)
        implicit none
        !
        integer, intent(inout) :: map(:, :)
        integer, intent(in)    :: path(:, :)
        integer :: i
        !
        do i = 1, size(path, 1)
            if (map(path(i, 1), path(i, 2)) == 0) then
                map(path(i, 1), path(i, 2)) = i
            end if
        end do
    end subroutine fillmap
    !
    pure function mandist(p1, p2)
        implicit none
        !
        integer, intent(in) :: p1(2), p2(2)
        integer :: mandist, rel(2)
        !
        rel = p2 - p1
        mandist = abs(rel(1)) + abs(rel(2))
    end function mandist
    !
    pure function findclosestintersect(map, path, origin) result(ret)
        implicit none
        !
        integer, dimension(:, :), intent(in) :: map, path
        integer, intent(in) :: origin(2)
        integer :: ret(3), i, curdist
        !
        ret(3) = -1
        do i = 1, size(path, 1)
            if (map(path(i, 1), path(i, 2)) /= 0) then
                curdist = mandist(origin, path(i, :))
                if (ret(3) == -1 .or. curdist < ret(3)) then
                    ret(1:2) = path(i, :)
                    ret(3) = curdist
                end if
            end if
        end do
    end function findclosestintersect
    !
    subroutine part1_()
        implicit none
        !
        integer, allocatable :: path1(:, :), path2(:, :), map(:, :)
        integer :: bounds(4), w, h, origin(2), closest(3)
        !
        call filetopathpoints('inputfiles/day3/input.txt', path1, path2)
        !
        bounds = minmax_xy_paths(path1, path2)
        origin = offset_bound(bounds, path1, path2)
        !
        w = bounds(3)
        h = bounds(4)
        !
        allocate (map(w, h))
        map = 0
        !
        call fillmap(map(:, :), path1)
        !
        closest = findclosestintersect(map, path2, origin)
        !
        print *, closest(3)
        !
        deallocate (path1, path2, map)
    end subroutine part1_
    !
    function findclosestintersect2(path1, path2) result(ret)
        implicit none
        !
        integer :: path1(:, :), path2(:, :)
        integer :: ret(3), i, ind, curdist
        !
        ret(3) = -1
        do i = 1, size(path1, 1)
            ind = findinpath(path2, path1(i, :))
            if (ind /= 0) then
                curdist = pdist(path1(i, :))
                if (ret(3) == -1 .or. curdist < ret(3)) then
                    ret(3) = curdist
                    ret(1:2) = path1(i, :)
                end if
            end if
        end do
    contains
        pure function findinpath(path, p) result(ret)
            implicit none
            !
            integer, intent(in) :: path(:, :), p(2)
            integer :: ret, i
            !
            ret = 0
            do i = 1, size(path, 1)
                if (path(i, 1) == p(1) .and. path(i, 2) == p(2)) then
                    ret = i
                    return
                end if
            end do
        end function findinpath
        !
        pure function pdist(p)
            implicit none
            !
            integer, intent(in) :: p(2)
            integer :: pdist
            !
            pdist = abs(p(1)) + abs(p(2))
        end function
    end function findclosestintersect2
    !
    subroutine part1()
        implicit none
        !
        integer, allocatable :: path1(:, :), path2(:, :)
        integer :: closest(3)
        !
        call filetopathpoints('inputfiles/day3/input.txt', path1, path2)
        !
        closest = findclosestintersect2(path1, path2)
        !
        print *, closest
        !
        deallocate (path1, path2)
    end subroutine part1
    !
    subroutine temprout(a)
        implicit none
        !
        integer :: a(:)
        !
        print *, a
    end subroutine temprout
    !
    subroutine part2()
        implicit none
        !
        type(hashmap_str_int) :: testmap
        type(astring) :: buffer
        !
        call testmap%new()
        !
        call buffer%new('Hei')
        print *, buffer, hash(buffer), modulo(hash(buffer), 16)
        call testmap%set(buffer, 5)
        call buffer%new('Hade')
        print *, buffer, hash(buffer), modulo(hash(buffer), 16)
        call testmap%set(buffer, 7)
        call buffer%new('Foo')
        print *, buffer, hash(buffer), modulo(hash(buffer), 16)
        call testmap%set(buffer, 6)
        call buffer%new('Bar')
        print *, buffer, hash(buffer), modulo(hash(buffer), 16)
        call testmap%set(buffer, 8)
        call buffer%new('Lol')
        print *, buffer, hash(buffer), modulo(hash(buffer), 16)
        call testmap%set(buffer, 9)
        !
        print *, testmap%meta
        print *, testmap%keys
        print *, testmap%vals
        call testmap%show()
        !
        call buffer%new('Kvante')
        print *, buffer, hash(buffer), modulo(hash(buffer), 16)
        call testmap%set(buffer, 10)
        !
        print *, testmap%meta
        print *, testmap%keys
        print *, testmap%vals
        !
        print *, 's: ', size(testmap)
        !
        call testmap%show()
    end subroutine part2
end module day3
