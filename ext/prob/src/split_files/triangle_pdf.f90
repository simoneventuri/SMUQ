subroutine triangle_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! TRIANGLE_PDF evaluates the Triangle PDF.
!
!  Discussion:
!
!    Given points A <= B <= C, the probability is 0 to the left of A,
!    rises linearly to a maximum of 2/(C-A) at B, drops linearly to zero
!    at C, and is zero for all values greater than C.
!
!    PDF(A,B,C;X)
!      = 2 * ( X - A ) / ( B - A ) / ( C - A ) for A <= X <= B
!      = 2 * ( C - X ) / ( C - B ) / ( C - A ) for B <= X <= C.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A <= B <= C and A < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( x <= a ) then

    pdf = 0.0D+00

  else if ( x <= b ) then

    if ( a == b ) then
      pdf = 0.0D+00
    else
      pdf = 2.0D+00 * ( x - a ) / ( b - a ) / ( c - a )
    end if

  else if ( x <= c ) then

    if ( b == c ) then
      pdf = 0.0D+00
    else
      pdf = 2.0D+00 * ( c - x ) / ( c - b ) / ( c - a )
    end if

  else
    pdf = 0.0D+00
  end if

  return
end
