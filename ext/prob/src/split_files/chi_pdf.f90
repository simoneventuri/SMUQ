subroutine chi_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! CHI_PDF evaluates the Chi PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = EXP ( - 0.5D+00 * ( ( X - A ) / B )^2 )
!      * ( ( X - A ) / B )^( C - 1 ) /
!      ( 2^( 0.5D+00 * C - 1 ) * B * GAMMA ( 0.5D+00 * C ) )
!
!    CHI(A,B,1) is the Half Normal PDF;
!    CHI(0,B,2) is the Rayleigh PDF;
!    CHI(0,B,3) is the Maxwell PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x <= a ) then

    pdf = 0.0D+00

  else

    y = ( x - a ) / b

    pdf = exp ( - 0.5D+00 * y * y ) * y ** ( c - 1.0D+00 ) / &
      ( 2.0D+00 ** ( 0.5D+00 * c - 1.0D+00 ) * b * gamma ( 0.5D+00 * c ) )

  end if

  return
end
