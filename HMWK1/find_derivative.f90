program find_derivative
  use nrtype
  use math_tools
  
  implicit none
  
  interface 
    function func(z)
      use nrtype
      implicit none
      real(dp) :: func
      real, intent(in) :: z
    end function func
  end interface

  procedure (func), pointer :: f_ptr => null()
  f_ptr => p_fun
  real(dp) :: deriv_1sided, deriv_2sided
  print *, deriv_1sided(1.0D0,f_ptr,1.0_dp)


contains





end program find_derivative



