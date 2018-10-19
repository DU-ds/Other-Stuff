      program linReg1
      integer, parameter :: rp = SELECTED_REAL_KIND(15)
      integer, parameter :: n = 1000
      real (kind = rp) , dimension(n, 4):: x  ! it's a 1000 row, 4 column matrix
      real (kind = rp) , dimension(n, 1):: y ! the dv, column vector
      real (kind = rp) , dimension(4, n):: x_T  ! transpose of the 1000 row, 4 column matrix
      real (kind = rp) , dimension(1, n):: y_T ! the dv, row vector
      integer :: readstatus, readstatus2

      open(file = "R:/DataSets/Matrix-10-18-18.txt", unit = 50)
      read (50,*) x_T
      x = transpose(x_T)

C       print *, y
      open(file = "R:/DataSets/vector-10-18-18.txt", unit = 52)
      read(52,*) y_T
      y = transpose(y_T)




      write (*,*) "works"
      end program linReg1



C  , recl = 10, iostat = readstatus 
C  access='sequential',form="formatted", are defaults      
C       do i = 1,n
C           read(50,*) X
C       end do
C       open(file = "R:/DataSets/vector-10-18-18.csv", unit = 52)
C  , recl = 100, iostat = res 
C  defaults: access='sequential',form="formatted", 
C       do i = 1,n
C           read(52,*) y
C       end do
