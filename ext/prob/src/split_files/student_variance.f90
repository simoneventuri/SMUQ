subroutine student_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! STUDENT_VARIANCE returns the variance of the central Student T PDF.
!
!  Discussion:
!
!    The variance is not defined unless 2 < C.
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
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) variance

  if ( c <= 2.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'STUDENT_VARIANCE - Fatal error!'
    write ( *, '(a)' ) '  Variance not defined for C <= 2.'
    stop 1
  end if

  variance = b * b * c / ( c - 2.0D+00 )

  return
end
