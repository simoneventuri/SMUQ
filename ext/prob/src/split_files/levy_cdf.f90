subroutine levy_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! LEVY_CDF evaluates the Levy CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    Normally, A <= X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_error_f
  real ( kind = 8 ) x

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LEVY_CDF - Fatal error!'
    write ( *, '(a)' ) '  Input parameter B <= 0.0'
    stop 1
  end if

  if ( x <= a ) then
    cdf = 0.0D+00
  else
    cdf = 1.0D+00 - r8_error_f ( sqrt ( b / ( 2.0D+00 * ( x - a ) ) ) )
  end if

  return
end
