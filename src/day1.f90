module day1
    implicit none
    
    public :: part1, part2
    
contains
    function modfuel1(mass) result(fuel)
        implicit none
        integer :: mass, fuel
        
        fuel = mass / 3 - 2
    end function modfuel1
    
    subroutine part1()
        implicit none
        integer :: a, b
        a = 14
        b = modfuel1(a)
        
        print *, b
    end subroutine part1
    
    subroutine part2()
        implicit none
        
        print *, 'd1p2'
    end subroutine part2
end module day1
