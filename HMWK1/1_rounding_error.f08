module root_finding

  function find_roots_2step(a,b,c,x)
    ! Calculate roots of a quadratic equation by solving
    ! q = .5*(b+sign(b)sqrt(b^2 - 4ac))
    ! Then roots are x = q/a and y = c /q
    real(dp) :: a,b,c
    real(dp), dimension(2) :: x
  end function 

end model root_finding
