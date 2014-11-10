module functions 
  implicit none
  interface p_fun
    module procedure p_fun
  end interface

contains
real(8) function p_fun(p) 
    use nrtype
    implicit none
    real(dp), intent(in) :: p
    p_fun = .5 * p**(-0.5) + 0.5 * p**(-0.2)
end function p_fun


end module functions    