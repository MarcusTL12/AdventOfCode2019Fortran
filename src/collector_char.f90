#define COLLECTOR_TYPE character
#define TYPE_NAME char
#include "collector.f90_template"

module collectormodule_char_spes
    use collectormodule_char
    implicit none
    !
    public collector_char_print
    !
    interface read (unformatted)
        module procedure readUnformatted
    end interface
    !
    interface read (formatted)
        module procedure readFormatted
    end interface
contains
    subroutine readUnformatted(self, unit, iostat, iomsg)
        class(collector_char), intent(inout) :: self
        integer, intent(in)    :: unit
        integer, intent(out)   :: iostat
        character(len=*), intent(inout) :: iomsg
        !
        iomsg = 'No msg'
        !
        call collector_readline(self, unit, iostat)
    end subroutine readUnformatted
    !
    subroutine readFormatted(self, unit, iotype, vlist, iostat, iomsg)
        class(collector_char), intent(inout) :: self
        integer, intent(in)    :: unit
        character(len=*), intent(in)    :: iotype
        integer, intent(in)    :: vlist(:)
        integer, intent(out)   :: iostat
        character(len=*), intent(inout) :: iomsg
        !
        integer :: dummy
        !
        iomsg = iotype
        dummy = size(vlist)
        !
        call collector_readline(self, unit, iostat)
    end subroutine readFormatted
    !
    subroutine collector_readline(self, unit, iostat)
        implicit none
        !
        class(collector_char), intent(inout) :: self
        integer, intent(in)  :: unit
        integer, intent(out), optional :: iostat
        !
        character(len=256) :: buffer
        !
        do
            read (unit, '(A)', advance='no', err=7, end=7, eor=7, &
                iostat=iostat) buffer
            call push_str(self, buffer, 256)
        end do
        !
        7 call push_str(self, buffer, len_trim(buffer))
    end subroutine collector_readline
    !
    subroutine push_str(self, str, l)
        implicit none
        !
        type(collector_char), intent(inout) :: self
        character(len=*), intent(in)        :: str
        integer, intent(in)                 :: l
        !
        integer :: i
        !
        do i = 1, l
            call self%push(str(i:i))
        end do
    end subroutine push_str
    !
    subroutine collector_char_print(self)
        implicit none
        !
        type(collector_char), intent(in) :: self
        integer :: i
        !
        do i = 1, size(self)
            write (*, '(A)', advance='no') self%at(i)
        end do
        print *
    end subroutine collector_char_print
end module collectormodule_char_spes
