subroutine fisher_pdf ( x, kappa, mu, pdf )

!*****************************************************************************80
!
!! FISHER_PDF evaluates the Fisher PDF.
!
!  Discussion:
!
!    The formula for the PDF is:
!
!      PDF(KAPPA,MU;X) = C(KAPPA) * exp ( KAPPA * MU' * X )
!
!    where:
!
!      0 <= KAPPA is the concentration parameter,
!      MU is a point on the unit sphere, the mean direction,
!      X is any point on the unit sphere,
!      and C(KAPPA) is a normalization factor:
!
!      C(KAPPA) = sqrt ( KAPPA ) / ( ( 2 * pi )^(3/2) * I(0.5,KAPPA) )
!
!    where
!
!      I(nu,X) is the Bessel function of order NU and argument X.
!
!    For a fixed value of MU, the value of KAPPA determines the
!    tendency of sample points to tend to be near MU.  In particular,
!    KAPPA = 0 corresponds to a uniform distribution of points on the
!    sphere, but as KAPPA increases, the sample points will tend to
!    cluster more closely to MU.
!
!    The Fisher distribution for points on the unit sphere is
!    analogous to the normal distribution of points on a line,
!    and, more precisely, to the von Mises distribution on a circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Kanti Mardia, Peter Jupp,
!    Directional Statistics,
!    Wiley, 2000,
!    LC: QA276.M335
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(3), the argument of the PDF.
!    X should have unit Euclidean norm, but this routine will
!    automatically work with a normalized version of X.
!
!    Input, real ( kind = 8 ) KAPPA, the concentration parameter.
!    0 <= KAPPA is required.
!
!    Input, real ( kind = 8 ) MU(3), the mean direction.
!    MU should have unit Euclidean norm, but this routine will
!    automatically work with a normalized version of MU.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ), parameter :: nb = 1

  real ( kind = 8 ) alpha
  real ( kind = 8 ) arg
  real ( kind = 8 ) b(nb)
  real ( kind = 8 ) cf
  integer ( kind = 4 ) ize
  real ( kind = 8 ) kappa
  real ( kind = 8 ) mu(3)
  real ( kind = 8 ) mu_norm
  integer ( kind = 4 ) ncalc
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(3)
  real ( kind = 8 ) x_norm

  if ( kappa < 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FISHER_PDF - Fatal error!'
    write ( *, '(a)' ) '  KAPPA must be nonnegative.'
    write ( *, '(a,g14.6)' ) '  Input KAPPA = ', kappa
    stop 1
  end if

  if ( kappa == 0.0D+00 ) then
    pdf = 1.0D+00 / ( 4.0D+00 * r8_pi )
    return
  end if
!
!  Compute the normalization factor CF.
!
  alpha = 0.5D+00
  ize = 1

  call ribesl ( kappa, alpha, nb, ize, b, ncalc )

  cf = sqrt ( kappa ) / ( sqrt ( ( 2.0D+00 * r8_pi ) ** 3 ) * b(1) )
!
!  Normalize MU.
!
  mu_norm = sqrt ( sum ( mu(1:3) ** 2 ) )

  if ( mu_norm == 0.0D+00 ) then
    pdf = cf
    return
  end if
!
!  Normalize X.
!
  x_norm = sqrt ( sum ( x(1:3) ** 2 ) )

  if ( x_norm == 0.0D+00 ) then
    pdf = cf
    return
  end if
!
!  Evaluate the PDF.
!
  arg = kappa * dot_product ( x(1:3), mu(1:3) ) / ( x_norm * mu_norm )

  pdf = cf * exp ( arg )

  return
end
