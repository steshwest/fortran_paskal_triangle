Program PaskalTriangle
implicit none
integer a, b, c, i, j, k, n
integer, allocatable :: radPrev(:), radNext(:)

print*, "Input expected precision: n = "
read*, n
print*, "Your precision parameter is ", n

allocate(radPrev(n), radNext(n))
radPrev(1) = 1

do k = 1, n, 1
    print*, "The line ", k-1, " is ", radNext
    radNext(1) = 1
    radNext(k) = 1
    do j = 2, k-1, 1
        radNext(j) = radPrev(j-1) + radPrev(j)
    end do
    do a = 1, n, 1
        radPrev(a) = radNext(a)   
    end do
end do

print*, "The final answer is ", radNext
deallocate(radPrev, radNext)
End Program PaskalTriangle
