module foo
  implicit none
  real, parameter :: eps = 1e-6
  real, parameter :: pi = 3.14159
contains
  real function f(x)
    real, intent(in) :: x
    f = sin(2*pi*x) - 2*x
  end function
  real function findroot(fun, x1, x2, start)
    real, intent(in) :: x1, x2, start
    integer :: i
    interface
      real function fun(x)
        real, intent(in) :: x
      end function fun
    end interface
    findroot = start
    do i = 1, 15
    print *, "fun(x)=", fun(findroot), " at x=", findroot
! Use a damped Newton method.
      findroot = findroot - 1.0*eps*fun(findroot)/ &
                 (fun (findroot + eps) - fun (findroot - eps))
      if ((findroot > x2) .or. (findroot < x1)) then
        print *, "root out of range"
        call abort ()
      end if
    end do
  end function
end module foo
