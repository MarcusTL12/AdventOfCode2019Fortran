program main
    use day1, only: d1p1 => part1, d1p2 => part2
    use day3, only: d3p1 => part1, d3p2 => part2
    implicit none
    
    character (len=10) :: arg1, arg2
    call getarg(1, arg1)
    call getarg(2, arg2)
    
    select case (arg1)
    case ('1')
        select case (arg2)
        case ('1')
            call d1p1()
        case ('2')
            call d1p2()
        case default
            print *, 'Not Implemented'
        end select
    case ('3')
        select case (arg2)
        case ('1')
            call d3p1()
        case ('2')
            call d3p2()
        case default
            print *, 'Not Implemented'
        end select
    case default
        print *, 'Not Implemented'
    end select
    
end program main
