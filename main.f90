program sparse_matrix
    implicit none

    ! 定义OLNode类型,包括left和up指针
    type OLNode
        integer :: i, j
        real :: e
        type(OLNode), pointer :: right, down, left, up
    end type OLNode

    type(OLNode), allocatable, target :: rhead(:), chead(:)
    type(OLNode), pointer :: p, q, prev
    integer :: m, n, t, i, j, row, col
    real :: e

    ! 初始化矩阵
    print *, 'Please enter the number of rows, columns, and non-zero elements count of the matrix:'
    read(*, *) m, n, t
    allocate(rhead(m), chead(n))
    do i = 1, m
        nullify(rhead(i)%right)
    end do
    do j = 1, n
        nullify(chead(j)%down)
    end do

    ! 读取非零元素并构建十字链表
    do i = 1, t
        do
            print *, 'Please enter the row, column, and value of a non-zero element:'
            read(*, *) row, col, e
            if (row < 1 .or. row > m .or. col < 1 .or. col > n) then
                print *, 'Invalid row or column index!'
                cycle
            end if
            exit
        end do
        allocate(p)
        p%i = row
        p%j = col
        p%e = e
        nullify(p%right, p%down, p%left, p%up)

        ! 链接到行链表
        if (associated(rhead(row)%right)) then
            q => rhead(row)%right
            prev => rhead(row)
            do while(associated(q) .and. q%j < col)
                prev => q
                q => q%right
            end do
            if (.not. associated(q)) then
                prev%right => p
                p%left => prev
            else
                p%right => q
                q%left => p
                if (associated(q%up)) q%up%down => p
            end if
        else
            rhead(row)%right => p
        end if

        ! 链接到列链表
        if (associated(chead(col)%down)) then
            q => chead(col)%down
            prev => chead(col)
            do while(associated(q) .and. q%i < row)
                prev => q
                q => q%down
            end do
            if (.not. associated(q)) then
                prev%down => p
                p%left => prev
            else
                p%down => q
                if (associated(q%up)) q%up => p
            end if
        else
            chead(col)%down => p
        end if
    end do

    ! 显示矩阵
    print *, 'Output of the matrix:'
    do j = 1, n
        if (associated(chead(j)%down)) then
            q => chead(j)%down
            do while(associated(q))
                print '(i0, i0, f0.2)', q%i, q%j, q%e
                q => q%down
            end do
        end if
    end do

    ! 释放内存
    do i = 1, m
        if (associated(rhead(i)%right)) then
            p => rhead(i)%right
            do while(associated(p))
                q => p%right
                nullify(p%right, p%down, p%left, p%up)
                deallocate(p)
                p => q
            end do
        end if
    end do
    do j = 1, n
        if (associated(chead(j)%down)) then
            p => chead(j)%down
            do while(associated(p))
                q => p%down
                nullify(p%right, p%down, p%left, p%up)
                deallocate(p)
                p => q
            end do
        end if
    end do
    deallocate(rhead, chead)
end program sparse_matrix