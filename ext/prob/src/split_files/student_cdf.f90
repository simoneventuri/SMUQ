subroutine student_cdf ( x, a, b, c, cdf )

!*****************************************************************************80
!
!! STUDENT_CDF evaluates the central Student T CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, shape parameters of the PDF,
!    used to transform the argument X to a shifted and scaled
!    value Y = ( X - A ) / B.  It is required that B be nonzero.
!    For the standard distribution, A = 0 and B = 1.
!
!    Input, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) b
  real ( kind = 8 ) b2
  real ( kind = 8 ) beta_inc
  real ( kind = 8 ) c
  real ( kind = 8 ) c2
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  y = ( x - a ) / b

  a2 = 0.5D+00 * c
  b2 = 0.5D+00
  c2 = c / ( c + y * y )

  if ( y <= 0.0D+00 ) then
    cdf = 0.5D+00 * beta_inc ( a2, b2, c2 )
  else
    cdf = 1.0D+00 - 0.5D+00 * beta_inc ( a2, b2, c2 )
  end if

  return
end
