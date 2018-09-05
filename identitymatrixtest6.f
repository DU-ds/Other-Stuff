cy
      program idmat
      implicit none
      integer :: n !user supplied rank of identity matrix
      integer, allocatable, dimension(:,:) :: x1 
      print *, "How big do you want the identity matrix to be?"
      read *, n
      allocate(x1(n,n))
      call identityMatrix(x1, n)
      call printMatrix(x1, n)
      end program idmat







      subroutine identityMatrix(X, n) ! takes an n x n matrix x1 and turns it into an identity matrix
!changed this subroutine the args arent in the same order, conforms with the arg ordering of printMatrix 
!also, changed from x1 to X, to make it
      implicit none
      integer :: col, row, n 
      integer, dimension(n,n) :: X !creating an identity matrix of rank n
      do col = 1, n !changed col and row position to access in column-major order
        do row = 1, n !http://kea.princeton.edu/ChE422/arrays.htm !details about column-major order
          X (row, col) = 0
        X (col, col ) = 1
        end do
      end do
      end subroutine identityMatrix


      subroutine printMatrix(array, n2) ! if this was to print a n x k matrix, I'd put a k on line 3, a k arg
        implicit none !in the subroutine and repalce the second n in the array(n,n) with a k
        integer, intent(in) :: array(n2,n2)  !creating an n x n identity matrix
        integer, intent(in) :: n2 !declaring that it's an input and not to be changed by running the subroutine
        integer :: i
        do i = 1, n2
          print*, array(i,:)
        end do
      end subroutine printMatrix