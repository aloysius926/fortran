module foo2

contains

  subroutine solve(x, f)
    real, dimension(:), intent(inout) :: x
    interface
      real pure function f(y)
        real, dimension(:), intent(in) :: y
      end function
    end interface

    print *, x
    print *, f(x)
  end subroutine

end module