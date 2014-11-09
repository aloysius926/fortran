program finder
  use foo
  implicit none
  real :: val
  val = findroot (f, -10.0, +10.0, -pi)
  print *, "The root is at", val
end