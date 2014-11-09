module test_functions
      implicit none
      
      interface x
         module procedure x
      end interface

contains
      real(8) function x(z)
             real(8), intent(in) :: z
	     x = z/5.0
      end function x
end module test_functions