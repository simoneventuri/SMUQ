subroutine student_sample ( a, b, c, seed, x )

!*****************************************************************************80
!
!! STUDENT_SAMPLE samples the central Student T PDF.
!
!  Discussion:
!
!    For the sampling algorithm, it is necessary that 2 < C.
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
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) b
  real ( kind = 8 ) b2
  real ( kind = 8 ) c
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3

  if ( c < 3.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'STUDENT_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  Sampling fails for C <= 2.'
    return
  end if

  a2 = 0.0D+00
  b2 = c / ( c - 2 )

  call normal_sample ( a2, b2, seed, x2 )

  call chi_square_sample ( c, seed, x3 )
  x3 = x3 * c / ( c - 2.0D+00 )

  x = a + b * x2 * sqrt ( c ) / x3

  return
end
