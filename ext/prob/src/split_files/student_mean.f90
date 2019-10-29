subroutine student_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! STUDENT_MEAN returns the mean of the central Student T PDF.
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
!    Input, real ( kind = 8 ) X, the argument of the PDF.
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
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) mean

  mean = a

  return
end
