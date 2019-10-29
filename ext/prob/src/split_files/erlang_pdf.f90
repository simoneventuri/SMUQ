subroutine erlang_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! ERLANG_PDF evaluates the Erlang PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = ( ( X - A ) / B )^( C - 1 )
!      / ( B * Gamma ( C ) * EXP ( ( X - A ) / B ) )
!
!    for 0 < B, 0 < C integer, A <= X.
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
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x <= a ) then

    pdf = 0.0D+00

  else

    y = ( x - a ) / b

    pdf = y ** ( c - 1 ) / ( b * lgamma ( real ( c, kind = 8 ) ) * exp ( y ) )

  end if

  return
end
