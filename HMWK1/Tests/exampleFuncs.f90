module ExampleFuncs

   implicit none

contains

function f1 (x)
  real :: f1
  real, intent (in) :: x

  f1 = 2.0 * x

  return
end function f1


function f2 (x)
   real :: f2
   real, intent (in) :: x

   f2 = 3.0 * x**2

   return
end function f2

end module ExampleFuncs