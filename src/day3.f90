module day3
    use ftlstringmodule
    implicit none
    
    public :: part1, part2
    
contains
    subroutine part1()
        use ftlstringmodule
        implicit none
        
        integer :: io_reason
        ! character (len=4) :: into1, into2, into3, into4
        ! character :: into3
        ! integer :: into2
        ! character (len=20) :: temp = 'R8,U5,L5,D3'
        ! character (len=20) :: format = '(A1, I10.1)'
        
        open (unit=1, file='inputfiles/day3/example1.txt')
        
        ! read (1, *, iostat=io_reason) into1, into2, into3, into4
        ! print *, into1, into2, into3, into4
        
        
        
    end subroutine part1
    
    subroutine part2()
        implicit none
        
        print *, 'd3p2'
    end subroutine part2
end module day3
