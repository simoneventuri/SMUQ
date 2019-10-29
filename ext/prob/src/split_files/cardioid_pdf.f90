subroutine cardioid_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! CARDIOID_PDF evaluates the Cardioid PDF.
!
!  Discussion:
!
!    The cardioid PDF can be thought of as being applied to points on
!    a circle.  Compare this distribution with the "Cosine PDF".
!
!    PDF(A,B;X) = ( 1 / ( 2 * PI ) ) * ( 1 + 2 * B * COS ( X - A ) )
!    for  A - PI <= X <= A + PI, -1/2 <= B <= 1/2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Nicholas Fisher,
!    Statistical Analysis of Circular Data,
!    Cambridge, 1993.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A - PI <= X <= A + PI.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    -0.5 <= B <= 0.5.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  pdf = ( 1.0D+00 + 2.0D+00 * b * cos ( x - a ) ) / ( 2.0D+00 * r8_pi )

  return
end
