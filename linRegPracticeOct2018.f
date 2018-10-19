      program linReg1
      
      integer, parameter :: rp = SELECTED_REAL_KIND(15)
      integer, parameter :: n = 1000
      real (kind = rp) , dimension(n, 4):: x       ! it's a 1000 row, 4 column matrix
      real (kind = rp) , dimension(n, 1):: y       ! the dv, column vector
      real (kind = rp) , dimension(4, n):: x_T     ! transpose of the 1000 row, 4 column matrix
      real (kind = rp) , dimension(1, n):: y_T     ! the dv, row vector
      real (kind = rp) , dimension(4, 4):: x_Tx    !the product of transpose(x) and x
      real (kind = rp) , dimension(4, 4):: x_Txinv !copy of x_Tx to prevent lapack from mutating it
      real (kind = rp) , dimension(n)   :: work    ! work array for LAPACK
      integer, dimension(n)             :: ipiv    ! pivot indices
      integer :: info
      integer :: readstatus, readstatus2

      ! External procedures defined in LAPACK
      external DGETRF
      external DGETRI


      open(file = "R:/DataSets/Matrix-10-18-18.txt", unit = 50)
      read (50,*) x_T
      x = transpose(x_T)

C       print *, y
      open(file = "R:/DataSets/vector-10-18-18.txt", unit = 52)
      read(52,*) y_T
      y = transpose(y_T)
C http://www.lahey.com/docs/lfprohelp/F95AROPENStmt.htm
C https://stackoverflow.com/questions/17323628/read-data-from-a-csv-file-in-fortran
C https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnaf/index.html
C file:///R:/Fortran_WD/FortranCrashCourseSeminar/ReadingArrayData.pdf
      
      x_Tx = matmul(x_T, x)
      x_Txinv = inv(x_Tx)
C      x_Txinv = matinv4( x_Tx )
C      x_Txinv = inv(x_Txinv)
C      print *, x_Txinv
! DGETRF computes an LU factorization of a general M-by-N matrix A
! using partial pivoting with row interchanges.
C       call DGETRF(n, n, x_Txinv, n, ipiv, info)
C       if (info /= 0) then
C          stop 'Matrix is numerically singular!'
C       end if
C       ! DGETRI computes the inverse of a matrix using the LU factorization
C       ! computed by DGETRF.
C       call DGETRI(n, x_Txinv, n, ipiv, work, n, info)

C       if (info /= 0) then
C          stop 'Matrix inversion failed!'
C       end if
      






      write (*,*) "program works"
      end program linReg1

      ! Returns the inverse of a matrix calculated by finding the LU
      ! decomposition.  Depends on LAPACK.
      function inv(A) result(Ainv)
         integer, parameter :: rp = SELECTED_REAL_KIND(15)
         real(kind = rp), dimension(:,:), intent(in) :: A
         real(kind = rp), dimension(size(A,1),size(A,2)) :: Ainv

         real(kind = rp), dimension(size(A,1)) :: work  ! work array for LAPACK
         integer, dimension(size(A,1)) :: ipiv   ! pivot indices
         integer :: n, info

        ! External procedures defined in LAPACK
         external DGETRF
         external DGETRI

        ! Store A in Ainv to prevent it from being overwritten by LAPACK
         Ainv = A
         n = size(A,1)

        ! DGETRF computes an LU factorization of a general M-by-N matrix A
        ! using partial pivoting with row interchanges.
         call DGETRF(n, n, Ainv, n, ipiv, info)

         if (info /= 0) then
            stop 'Matrix is numerically singular!'
         end if

        ! DGETRI computes the inverse of a matrix using the LU factorization
        ! computed by DGETRF.
         call DGETRI(n, Ainv, n, ipiv, work, n, info)

         if (info /= 0) then
            stop 'Matrix inversion failed!'
         end if
      end function inv


      pure function matinv4(A) result(B)
         integer, parameter :: rp = SELECTED_REAL_KIND(15)
         !! Performs a direct calculation of the inverse of a 4×4 matrix.
         real (kind = rp) , intent(in)  :: A(4,4)   !! Matrix
         real (kind = rp) , intent(out) :: B(4,4)   !! Inverse matrix
         real (kind = rp)               :: detinv

         ! Calculate the inverse determinant of the matrix
         detinv = &
           1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
            - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
            + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
            - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

         ! Calculate the inverse of the matrix
         B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
         B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
         B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
         B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
         B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
         B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
         B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
         B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
         B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
         B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
         B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
         B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
         B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
         B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
         B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
         B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
       end function




C  , recl = 10, iostat = readstatus 
C  access='sequential',form="formatted", are defaults      
C       do i = 1,n
C          read(50,*) X
C       end do
C       open(file = "R:/DataSets/vector-10-18-18.csv", unit = 52)
C  , recl = 100, iostat = res 
C  defaults: access='sequential',form="formatted", 
C       do i = 1,n
C          read(52,*) y
C       end do

C $ gfortran R:/Fortran_WD/linRegPracticeOct2018.f -o R:/Fortran_WD/linRegPracticeOct2018.exe -L/R:/Cygwin/http%3a%2f%2fmirror.cs.vt.edu%2fpub%2fcygwin%2fcygwin%2f/x86_64/release/lapack/liblapack0/usr/lib/lapack -llapack
C https://stackoverflow.com/questions/35007006/error-in-linking-gfortran-to-lapack-and-blas
C https://stackoverflow.com/questions/23130045/fortran-90-segmentation-fault-invalid-memory-reference-with-scalable-3d-ar
C https://stackoverflow.com/questions/20453315/segmentation-fault-core-dumped-error-in-fortran-gfortran-linux



