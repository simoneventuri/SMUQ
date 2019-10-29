subroutine fisher_sample ( kappa, mu, n, seed, xyz )

!*****************************************************************************80
!
!! FISHER_SAMPLE samples the Fisher distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Nicholas Fisher, Toby Lewis, Brian Embleton,
!    Statistical Analysis of Spherical Data,
!    Cambridge, 2003,
!    ISBN13: 978-0521456999,
!    LC: QA276.F489.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) KAPPA, the concentration parameter.
!
!    Input, real ( kind = 8 ) MU(3), the mean direction.
!    MU should have unit Euclidean norm, but this routine will
!    automatically work with a normalized version of MU.
!
!    Input, integer ( kind = 4 ) N, the number of samples to choose.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) XYZ(3,N), a sample of the Fisher distribution.
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) ALPHA, BETA, the colatitude (theta) and
!    longitude (phi) of the mean direction.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  real ( kind = 8 ) kappa
  real ( kind = 8 ) lambda
  real ( kind = 8 ) mu(3)
  real ( kind = 8 ) mu_norm
  real ( kind = 8 ) phi(1:n)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ) theta(1:n)
  real ( kind = 8 ) xyz(3,n)

  mu_norm = sqrt ( sum ( mu(1:3) ** 2 ) )

  if ( mu_norm == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FISHER_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  MU(1:3) = 0.'
    stop 1
  end if

  alpha = - acos ( mu(3) / mu_norm )
  beta = atan2 ( mu(2), mu(1) )

  lambda = exp ( - 2.0D+00 * kappa )

  call r8vec_uniform_01 ( n, seed, theta )

  if ( kappa == 0.0D+00 ) then

    theta(1:n) = 2.0D+00 * asin ( sqrt ( 1.0D+00 - theta(1:n) ) )

  else

    theta(1:n) = 2.0D+00 * asin ( sqrt ( &
      - log ( theta(1:n) * ( 1.0D+00 - lambda ) + lambda ) &
      / ( 2.0D+00 * kappa ) ) )

  end if

  call r8vec_uniform_01 ( n, seed, phi )

  phi(1:n) = 2.0D+00 * r8_pi * phi(1:n)
!
!  Compute the unrotated points.
!
  xyz(1,1:n) = sin ( theta(1:n) ) * cos ( phi(1:n) )
  xyz(2,1:n) = sin ( theta(1:n) ) * sin ( phi(1:n) )
  xyz(3,1:n) = cos ( theta(1:n) )
!
!  Compute the rotation matrix.
!
  a(1,1) =   cos ( alpha ) * cos ( beta )
  a(2,1) =                 - sin ( beta )
  a(3,1) =   sin ( alpha ) * cos ( beta )

  a(1,2) =   cos ( alpha ) * sin ( beta )
  a(2,2) =                 + cos ( beta )
  a(3,2) =   sin ( alpha ) * sin ( beta )

  a(1,3) = - sin ( alpha )
  a(2,3) =   0.0D+00
  a(3,3) =   cos ( alpha )
!
!  Rotate the points.
!
  xyz(1:3,1:n) = matmul ( a(1:3,1:3), xyz(1:3,1:n) )

  return
end
