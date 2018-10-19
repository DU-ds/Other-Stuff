      program linReg1
      implicit none
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
      x_Txinv = matinv4( x_Tx )
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



