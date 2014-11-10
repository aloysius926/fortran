program test_foo2
use foo2

real, dimension(2) :: x = [1.0, 2.0]

call solve(x, g)

contains

  real pure function g(y)
    real, dimension(2), intent(in) :: y

    g = sum(y)
  end function

end program test_foo2

