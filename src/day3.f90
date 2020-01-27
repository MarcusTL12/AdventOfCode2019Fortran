module day3
    use ftlstringmodule
    implicit none
    
    public :: part1, part2
    
contains
    subroutine part1()
        implicit none
        
        type(ftlString) :: line
        
        open (unit=1, file='inputfiles/day3/example1.txt')
        
        read (1, *) line
        print *, line
        
        do
            
        end do
        
        close (1)
    end subroutine part1
    
    subroutine part2()
        implicit none
        
        print *, 'd3p2'
    end subroutine part2
end module day3
