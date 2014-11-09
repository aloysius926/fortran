program find_derivative
  use nrtype
  use math_tools
  
  implicit none
  integer :: n
  real(dp) :: eps
  real(dp)  :: true_answer
  interface 
    function func(z)
      use nrtype
      implicit none
      real(dp) :: func
      real(dp), intent(in) :: z
    end function func
  end interface

  procedure (func), pointer :: f_ptr => null()
  f_ptr => p_fun
!  real(dp) :: deriv_1sided, deriv_2sided
  true_answer = 0.5_dp * (-0.5_dp) * 1.5_dp**(-1.5_dp) + 0.5_dp * (-0.2_dp) * 1.5_dp**(-1.2_dp)
  do n=-1,-10,-1
    print *, n
    eps = 10.0_dp**(n)
    print *, deriv_1sided(1.5_dp,f_ptr,eps)
    print *, "Error 1: ", true_answer - deriv_1sided(1.5_dp,f_ptr,eps)
    print *, deriv_2sided(1.5_dp,f_ptr,eps)
    print *, "Error 2: ",true_answer - deriv_2sided(1.5_dp,f_ptr,eps)
  end do
  print *, "Errors when eps is automatically chosen"
  print *, "Error 1: ", true_answer - deriv_1sided(1.5_dp,f_ptr)
  print *, "Error 2: ",deriv_2sided(1.5_dp,f_ptr)
  print *, "Error 2: ",true_answer - deriv_2sided(1.5_dp,f_ptr)
  
contains

function p_fun(p) 
    use nrtype
    implicit none
    real(dp), intent(in) :: p
    real(dp) :: p_fun
    p_fun = .5 * p**(-0.5) + 0.5 * p**(-0.2_dp)
end function p_fun

function x_sqr(p) 
    use nrtype
    implicit none
    real(dp), intent(in) :: p
    real(dp) :: x_sqr
    x_sqr = p**2
end function x_sqr



end program find_derivative



