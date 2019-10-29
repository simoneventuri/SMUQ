subroutine uniform_nsphere_sample ( n, seed, x )

!*****************************************************************************80
!
!! UNIFORM_NSPHERE_SAMPLE samples the Uniform Unit Sphere PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 168.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the sphere.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), a point on the unit N sphere, chosen
!    with a uniform probability.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  do i = 1, n
    call normal_01_sample ( seed, x(i) )
  end do

  x(1:n) = x(1:n) / sqrt ( sum ( x(1:n) ** 2 ) )

  return
end
