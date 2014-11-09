module math_tools
implicit none
contains

function deriv_1sided(x,f,e) result(fx)
  use nrtype
  implicit none
  real(dp) :: f
  real(dp) :: fx
  real(dp), intent(in) :: x,e
  interface AFunc
    function func(y)
      use nrtype
      implicit none
      real(dp) :: func
      real, intent(in) :: y
    end function func
  end interface AFunc
  fx = f(x) + e

end function deriv_1sided


! function deriv_2sided(x,f,e) result(fx)
!   use nrtype
!   implicit none
!   external f
!   real(dp) :: fx, f
!   real(dp), intent(in) :: x,e
!   fx = f(x) + e
! 
! end function deriv_2sided

function p_fun(p) 
    use nrtype
    implicit none
    real(dp), intent(in) :: p
    real(dp) :: p_fun
    p_fun = .5 * p**(-0.5) + 0.5 * p**(-0.2)
end function p_fun


end