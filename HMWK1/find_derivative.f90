program find_derivative
  use nrtype
  implicit none
  real(dp) :: p_fun, deriv_1sided, deriv_2sided



end program find_derivative


function p_fun(p) result(gp)
  use nrtype
  implicit none
  real(dp) :: gp
  real(dp) :: p
  gp = .5 * p**(--0.5) + 0.5 * p**(-0.2)

end p_fun



function deriv_1sided(x,f,e) result(fx)
  use nrtype
  implicit none
  real(dp) :: fx
  real(dp), intent(in) :: x,f,e

end function seriv_1sided


function deriv_2sided(x,f,e) result(fx)
  use nrtype
  implicit none
  real(dp) :: fx
  real(dp), intent(in) :: x,f,e

end function deriv_2sided