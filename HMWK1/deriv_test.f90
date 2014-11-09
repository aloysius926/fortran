! Copyright (c) 2005-2010, 2012-2013, Andrew Hang Chen and contributors,
! All rights reserved.
! Licensed under the 3-clause BSD license.

module deriv_test
  use fruit
  implicit none

contains                          !fortran 95 limits subroutine name to 31 char.
  subroutine test_deriv_should_produce_2
    use math_tools
    use nrtype
    real(dp) :: result
    interface 
      function func(z)
        use nrtype
        implicit none
        real(dp) :: func
        real(dp), intent(in) :: z
      end function func
    end interface

    procedure (func), pointer :: f_ptr => null()
    f_ptr => x_sqr

    result = deriv_1sided(1.0_dp,f_ptr)
    call assert_equals (2.0_dp, result,10.0_dp**(-5))
  end subroutine test_deriv_should_produce_2


function x_sqr(p) 
    use nrtype
    implicit none
    real(dp), intent(in) :: p
    real(dp) :: x_sqr
    x_sqr = p**2
end function x_sqr

end module deriv_test

