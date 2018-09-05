      program main
      implicit none
      integer :: i 
      integer :: n
      integer :: Fizz
      integer :: Buzz

      fizz = 3
      Buzz = 5
      n = 100

      do i = 1, n
        if ( mod (i, Fizz) == 0 .AND. mod(i , Buzz ) == 0 ) then
          print *, "FizzBuzz"
        else if(mod(i , Fizz) == 0) then
          print *, "Fizz"
        else if ( mod(i , Buzz) == 0) then
          print *, "Buzz"
        else
          print *, i
        end if
      end do
      end program main
!! gfortran R:/Fortran_WD/FizzBuzz.f -o R:/Fortran_WD/FizzBuzz.exe