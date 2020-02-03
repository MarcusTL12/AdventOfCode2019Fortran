module hashmapmodule_int_int
    use hashmodule
    use utilmodule, only: inc
    implicit none
    private
    !
    integer :: initsize = 8 !, resizefactor = 2
    real    :: fillthreshhold = 2.0 / 3
    !
    public :: hashmap_int_int
    type hashmap_int_int
        integer(1), allocatable :: meta(:)
        integer, allocatable    :: keys(:)
        integer, allocatable    :: vals(:)
        integer :: amtelements
    contains
        procedure, public :: new => map_new
        procedure, public :: get => map_get_index
        procedure, public :: set => map_set_index
        procedure, public :: deleteat => map_deleteat
        final :: map_finalizer
        procedure :: key_to_index
        procedure :: hashmod
        procedure :: insert => map_insert
    end type
contains
    pure subroutine map_new(self)
        implicit none
        !
        class(hashmap_int_int), intent(inout) :: self
        !
        call map_finalizer(self)
        !
        allocate (self%meta(initsize), self%keys(initsize), self%vals(initsize))
    end subroutine map_new
    !
    pure subroutine map_new_size(self, size)
        implicit none
        !
        class(hashmap_int_int), intent(inout) :: self
        integer, intent(in) :: size
        integer :: size2
        !
        call map_finalizer(self)
        !
        size2 = initsize
        !
        do while (size2 < size / fillthreshhold)
            size2 = ishft(size2, 1)
        end do
        !
        allocate (self%meta(size2), self%keys(size2), self%vals(size2))
    end subroutine map_new_size
    !
    pure subroutine map_finalizer(self)
        implicit none
        !
        type(hashmap_int_int), intent(inout) :: self
        !
        ! print *, 'Map deallocated!'
        !
        if (allocated(self%meta)) then
            ! print *, 'Actually deallocated!'
            deallocate (self%meta, self%keys, self%vals)
        end if
    end subroutine map_finalizer
    !
    pure function hashmod(self, key) result(hm)
        implicit none
        !
        class(hashmap_int_int), intent(in) :: self
        integer, intent(in) :: key
        integer :: hm
        !
        hm = modulo(hash(key), size(self%meta)) + 1
    end function hashmod
    !
    pure subroutine key_to_index(self, key, ind, status, jumps)
        implicit none
        !
        class(hashmap_int_int), intent(in) :: self
        integer, intent(in) :: key
        integer, intent(out) :: ind
        logical, intent(inout) :: status
        integer, optional, intent(out) :: jumps
        integer :: origind, amt, jumpbuff
        !
        jumpbuff = 0
        status = .true.
        origind = self%hashmod(key)
        ind = origind
        amt = ishft(self%meta(ind), -1)
        !
        if (self%meta(ind) == 0) then
            status = .false.
            return
        end if
        !
        do while (self%keys(ind) /= key .and. amt >= 0)
            if (self%meta(ind) == 0) then
                status = .false.
                return
            else if (self%hashmod(self%keys(ind)) == origind) then
                amt = amt - 1
                jumpbuff = jumpbuff + 1
            end if
            ind = ind + 1
            if (ind > size(self%meta)) ind = 1
        end do
        status = self%meta(ind) /= 0
        if (present(jumps)) jumps = jumpbuff
    end subroutine key_to_index
    !
    pure subroutine map_insert(self, key, val, ind)
        implicit none
        !
        class(hashmap_int_int), intent(inout) :: self
        integer, intent(in) :: key
        integer, intent(in) :: val
        integer, intent(in) :: ind
        integer             :: h
        !
        h = self%hashmod(key)
        call inc(self%meta(h), int(2, 1))
        if (h /= ind) call inc(self%meta(ind), int(1, 1))
        !
        self%keys(ind) = key
        self%vals(ind) = val
    end subroutine map_insert
    !
    function map_get_index(self, key) result(ret)
        implicit none
        !
        class(hashmap_int_int), intent(in) :: self
        integer, intent(in) :: key
        integer :: ret, ind
        logical :: status
        !
        call self%key_to_index(key, ind, status)
        !
        ret = self%vals(ind)
    end function map_get_index
    !
    subroutine map_set_index(self, key, val)
        implicit none
        !
        class(hashmap_int_int), intent(inout) :: self
        integer, intent(in) :: key
        integer, intent(in) :: val
        logical :: status
        integer :: ind
        !
        call self%key_to_index(key, ind, status)
        !
        if (status) then
            self%vals(ind) = val
        else
            call self%insert(key, val, ind)
        end if
    end subroutine map_set_index
    !
    subroutine map_deleteat(self, key)
        implicit none
        !
        class(hashmap_int_int), intent(inout) :: self
        integer, intent(in) :: key
        integer :: amt, ind, origind, jumps, i, j
        logical :: status
        !
        origind = self%hashmod(key)
        call self%key_to_index(key, ind, status, jumps)
        !
        print *, 'jumps: ', jumps
        if (status) then
            amt = ishft(self%meta(origind), -1) - jumps
            print *, 'amt:   ', amt
            do i = 1, amt
                
            end do
        end if
    contains
        function findnext(ind) result(nextind)
            implicit none
            !
            integer, intent(in) :: ind
            integer :: nextind
            ! TODO stuff
        end function findnext
    end subroutine map_deleteat
end module
