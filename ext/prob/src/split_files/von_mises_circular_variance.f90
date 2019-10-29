subroutine von_mises_circular_variance ( a, b, circular_variance )

!*****************************************************************************80
!
!! VON_MISES_CIRCULAR_VARIANCE: circular variance of the von Mises PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Output, real ( kind = 8 ) CIRCULAR_VARIANCE, the circular variance
!    of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) bessel_i0
  real ( kind = 8 ) bessel_i1
  real ( kind = 8 ) circular_variance

  circular_variance = 1.0D+00 - bessel_i1 ( b ) / bessel_i0 ( b )

  return
end
