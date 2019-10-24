module smart_container
    implicit none
    contains
    function append(array, elem) result(tmp)
        implicit none
        integer :: elem, length, i
        integer, dimension(:), allocatable :: array, tmp

        length = size(array,1)
        allocate(tmp(length+1))

        do i=1,length
            tmp(i) = array(i)
        end do

        tmp(length+1) = elem

    end function
end module
