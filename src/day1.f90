module day1
    implicit none
    !
    public :: part1, part2
    !
contains
    function modfuel1(mass) result(fuel)
        implicit none
        integer :: mass, fuel
        !
        fuel = mass / 3 - 2
    end function modfuel1
    !
    recursive function modfuel2(mass) result(fuel)
        implicit none
        integer mass, fuel
        !
        fuel = modfuel1(mass)
        !
        if (fuel > 0) then
            fuel = fuel + modfuel2(fuel)
        else
            fuel = 0
        end if
    end function modfuel2
    !
    subroutine part1()
        implicit none
        !
        integer :: reader, io_reason, accum
        !
        open (unit=1, file='inputfiles/day1/input.txt')
        !
        do
            read (1, *, iostat=io_reason) reader
            if (io_reason == 0) then
                accum = accum + modfuel1(reader)
            else
                exit
            end if
        end do
        !
        close (1)
        !
        print *, accum
    end subroutine part1
    !
    subroutine part2()
        implicit none
        !
        integer :: reader, io_reason, accum
        !
        open (unit=1, file='inputfiles/day1/input.txt')
        !
        do
            read (1, *, iostat=io_reason) reader
            if (io_reason == 0) then
                accum = accum + modfuel2(reader)
            else
                exit
            end if
        end do
        !
        close (1)
        !
        print *, accum
    end subroutine part2
end module day1
