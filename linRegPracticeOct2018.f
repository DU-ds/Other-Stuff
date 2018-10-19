      program linReg1
      integer, parameter :: rp = SELECTED_REAL_KIND(15)
      integer, parameter :: n = 1000
      real (kind = rp) , dimension(n, 4):: X  ! it's a 1000 row, 4 column matrix
      real (kind = rp) , dimension(n, 1):: y ! the dv, column vector
      integer :: readstatus, readstatus2
      write (*,*) "works"
      end program linReg1



