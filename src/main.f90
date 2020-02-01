program main
    use day1, only: d1a => part1, d1b => part2
    use day3, only: d3a => part1, d3b => part2
    implicit none
    !
    character (len=10) :: arg1, arg2
    !
    call getarg(1, arg1)
    call getarg(2, arg2)
    !
    select case (arg1)
    case ('1')
        select case (arg2)
        case ('1')
            call d1a()
        case ('2')
            call d1b()
        case default
            print *, 'Not Implemented'
        end select
    case ('3')
        select case (arg2)
        case ('1')
            call d3a()
        case ('2')
            call d3b()
        case default
            print *, 'Not Implemented'
        end select
    case default
        print *, 'Not Implemented'
    end select
end program main
