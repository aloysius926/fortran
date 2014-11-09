
program test_func_ptrs

    use ExampleFuncs
    implicit none

   abstract interface
      function func (z)
         real :: func
         real, intent (in) :: z
      end function func
   end interface

   procedure (func), pointer :: f_ptr => null ()

   real :: input

   write (*, '( / "Input test value: ")', advance="no" )
   read (*, *) input

   if ( input < 0 ) then
      f_ptr => f1
   else
      f_ptr => f2
   end if

   write (*, '(/ "evaluate function: ", ES14.4 )' )  f_ptr (input)

   stop

end program test_func_ptrs