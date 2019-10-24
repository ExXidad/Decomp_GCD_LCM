module decomposition
    implicit none
    contains
    subroutine decompose(input,output_array)
        use smart_container
        implicit none
        integer :: input, tmp, input_copy
        integer :: i, j, m
        integer, dimension(:), allocatable :: output_array
        integer, dimension(:), allocatable :: array

        if (input <= 0) stop "Wrong input"

        allocate(array(input))
        allocate(output_array(1))
        output_array(1) = 1

        do i=1,input
            array(i) = i
        end do


        do m=2,input
            if (array(m)/=0) then
                do j=2*m,input,m
                    array(j) = 0
                end do
            end if
        end do

        tmp = input
        do i = 2, (input)
            if (array(i) /= 0) then
                tmp = array(i)
                input_copy = input
                do while (mod(input_copy,tmp) == 0)
                        output_array = append(output_array,tmp)
                        input_copy = input_copy / tmp
                end do
            end if
        end do
    end subroutine
end module
