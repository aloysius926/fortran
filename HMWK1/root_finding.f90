program root_finding
  use nrtype
  implicit none
  real(dp), dimension(2) :: x
  real(dp) :: a, b, c
  integer :: n 
  a = 1.0_dp
  b = 100000.0_dp
  c = 10

  do n=-1,-8,-1
    print *, n
    call find_roots_1step(a,b,c**n,x)
    print *, x
    call find_roots_2step(a,b,c**n,x)
    print *, x
  end do

  contains
  subroutine find_roots_1step(a,b,c,x)
    ! Calculate roots of a quadratic equation by solving
    ! q = .5*(b+sign(b)sqrt(b^2 - 4ac))
    ! Then roots are x = q/a and y = c /q
    real(dp), intent(in) :: a,b,c
    real(dp), intent(out), dimension(2) :: x
    x(1) = (-b + sqrt(b**2 - 4 * a * c))/(2 * a)
    x(2) = (-b - sqrt(b**2 - 4 * a * c))/(2 * a)
  end subroutine find_roots_1step

  subroutine find_roots_2step(a,b,c,x)
    ! Calculate roots of a quadratic equation by solving
    ! q = .5*(b+sign(b)sqrt(b^2 - 4ac))
    ! Then roots are x = q/a and y = c /q
    real(dp), intent(in) :: a,b,c
    real(dp) :: q
    real(dp), parameter :: one = 1.0D0
    real(dp), intent(out), dimension(2) :: x
    q = .5 * (b+ sign(one,b)*sqrt(b**2 - 4 * a * c) )
    x(1) = q / a
    x(2) = c / q
  end subroutine find_roots_2step



end program root_finding
