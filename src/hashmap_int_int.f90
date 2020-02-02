module hashmapmodule_int_int
    implicit none
    private
    !
    public :: hashmap_int_int
    type hashmap_int_int
        integer, allocatable :: data(:)
    end type
contains
end module