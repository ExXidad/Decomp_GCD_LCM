program main
    use decomposition
    use search_procedures
    implicit none
    integer :: first, second
    integer :: len1, len2
    integer :: i, gcd = 1, lcm = 1
    type(search_result) :: search_res
    integer, dimension(:), allocatable :: first_array, second_array

    write(*,"(A,$)") "Put numbers to find their decomposition, GCD, LCM and check for mutal simplicity: "

    read(*,*) first, second

    write(*,*) "Their decompositions are: "

    call decompose(first,first_array)
    call decompose(second,second_array)

    len1=size(first_array,1)
    len2=size(second_array,1)

    write(*,*) first,second
    write(*,*) "------------------------------------------------------------"

    do i = 1,max(len1,len2)
        if (len1 >= i) then
            write(*,"(I20,$)") first_array(i)
        else
            write(*,"(A20,$)") "                    "
        end if

        if (len2 >= i) then
            write(*,"(I20,$)") second_array(i)
        else
            write(*,"(A20,$)") "                    "
        end if
        write(*,*)
    end do

    write(*,*) "------------------------------------------------------------"

    do i = 1, len1
        search_res = linsearch(second_array,first_array(i))
        if (search_res%foundQ) then
            gcd = gcd * first_array(i)
            second_array(search_res%elem_pos) = -1
            first_array(i) = -1
        end if
    end do

    write(*,"(A,$)") "GCD is: "
    write(*,*) gcd

    if (first <= second) then
        do i = 1, len1
            if (first_array(i) /= -1) lcm = lcm * first_array(i)
        end do
        lcm = lcm * second
    else
        do i = 1, len2
            if (second_array(i) /= -1) lcm = lcm * second_array(i)
        end do
        lcm = lcm * first
    end if

    write(*,"(A,$)") "LCM is: "
    write(*,*) lcm

    write(*,"(A,$)") "Mutually simple: "
    if (gcd == 1) write(*,*) "Yes"
    if (gcd /= 1) write(*,*) "No"
end
