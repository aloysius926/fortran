module math_tools
implicit none



contains


function deriv_1sided(x,f,eps) result(fx)
  use nrtype
  implicit none
  real(dp) :: f, epsil
  real(dp) :: fx, xtest, temp_e
  real(dp), intent(in) :: x
  real(dp), optional, intent(in) :: eps
  interface BFunc
    function func(y)
      use nrtype
      implicit none
      real(dp) :: func
      real, intent(in) :: y
    end function func
  end interface BFunc
  
  if (present(eps)) then
    ! Trust that the programmer knew what they wanted
    epsil = eps
  else
    ! Use the machine precision rule from Fatih's notes
    epsil = (10.0_dp**(-precision(x)))**(.5_dp)
  endif

  ! Make sure we use and epsilon that has an exact binary representation
  xtest = x + epsil
  temp_e = xtest - x
  fx = (f(x + temp_e)  - f(x))/temp_e
  

end function deriv_1sided


 function deriv_2sided(x,f,eps) result(fx)
  use nrtype
  implicit none
  real(dp) :: f, epsil
  real(dp) :: fx, xtest, temp_e
  real(dp), intent(in) :: x
  real(dp), optional, intent(in) :: eps
  interface AFunc
    function func(y)
      use nrtype
      implicit none
      real(dp) :: func
      real, intent(in) :: y
    end function func
  end interface AFunc
  
  if (present(eps)) then
    ! Trust that the programmer knew what they wanted
    epsil = eps
  else
    ! Use the machine precision rule from Fatih's notes
    epsil = (10.0_dp**(-precision(x)))**(1.0_dp/3.0_dp)
  endif

  ! Make sure we use and epsilon that has an exact binary representation
  xtest = x + epsil
  temp_e = xtest - x
  fx = (f(x + temp_e)  - f(x- temp_e))/(2*temp_e)
  
  
 end function deriv_2sided





end