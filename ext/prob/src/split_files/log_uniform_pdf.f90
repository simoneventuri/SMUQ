subroutine log_uniform_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! LOG_UNIFORM_PDF evaluates the Log Uniform PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = 1 / ( X * ( log ( B ) - log ( A ) ) ) for A <= X <= B
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1.0 < A < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( x < a ) then
    pdf = 0.0D+00
  else if ( x <= b ) then
    pdf = 1.0D+00 / ( x * ( log ( b ) - log ( a ) ) )
  else
    pdf = 0.0D+00
  end if

  return
end
