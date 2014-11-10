program golden_ratio
  use nrtype
  implicit none
!   The second problem of Fatih's homework.  Calculate the Golden Ratio
!   using two different methods.  
!   1. Write a recursion for phi
!   2. Calculate it directly


  real(dp), parameter :: phi= 0.61803398
  real(dp) :: golden_rec
  real(dp) :: golden
  integer :: n

  do n=2,20
    print *, n
    print *, golden_rec(n)
    print *, golden(n)
  end do   



end program golden_ratio

recursive function golden_rec(n) result(x)
  use nrtype
  implicit none
!   Basic recursive formulation for powers of the golden ratio using the 
!   fact that phi(n+1) = phi(n-1) - phi(n)
  integer, intent(in) :: n
  real(dp) :: x
  if ( n == 0 ) then
    x = 1
  else if (n == 1) then
    x = 0.61803398
  else 
    x = golden_rec(n-2) - golden_rec(n-1)
  end if
end function golden_rec

function golden(n) result(x)
  use nrtype
  implicit none
!   Calculate powers of the golden ratio directly
  integer, intent(in) :: n
  real(dp) :: x
  x = 0.61803398**n
end function golden


