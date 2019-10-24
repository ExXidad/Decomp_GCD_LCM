module search_procedures
    implicit none
    type search_result
        logical :: foundQ
        integer :: elem_pos
    end type
    contains
    function binsearch(array,elem) result (search_res)
        implicit none
        integer :: divider_position, low, up
        integer :: length, elem, divider
        logical :: elem_is_there, sorted_upQ = .false.
        type(search_result) :: search_res
        integer, dimension(:) :: array

        length = size(array,1)
        low = 1; up = length; divider_position = (up + low)/2
        elem_is_there = .false.
        if (array(low)<array(up)) then
            sorted_upQ = .true.
        end if

        do while ((low<=up) .and. (elem_is_there .eqv. .false.))
            divider = array(divider_position)

            if (elem == divider) then
                elem_is_there = .true.


            else if ((elem < divider) .eqv. (sorted_upQ .eqv. .true.)) then
                up = divider_position - 1
                divider_position = low + (up - low)/2
                length = up - low + 1


            else if ((elem > divider) .eqv. (sorted_upQ .eqv. .true.)) then
                low = divider_position + 1
                divider_position = low + (up - low)/2
                length = up - low + 1

            end if
        end do
!
        divider = array(divider_position)
        if (elem == divider) then
            elem_is_there = .true.
        end if

        search_res%foundQ = elem_is_there
        if (elem_is_there) then
            search_res%elem_pos = divider_position
        else
            search_res%elem_pos = -1
        end if
    end function


    function linsearch(array,elem) result (search_res)
        implicit none
        integer :: length, elem, i
        type(search_result) :: search_res
        integer, dimension(:) :: array

        length = size(array,1)
        search_res%foundQ = .false.
        search_res%elem_pos = -1

        do i = 1, length
            if (array(i) == elem) then
                search_res%foundQ = .true.
                search_res%elem_pos = i
                exit
            end if
        end do
    end function
end module
