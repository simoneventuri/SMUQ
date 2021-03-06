subroutine log_normal_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! LOG_NORMAL_CDF evaluates the Lognormal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 < X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) logx
  real ( kind = 8 ) x

  if ( x <= 0.0D+00 ) then

    cdf = 0.0D+00

  else

    logx = log ( x )

    call normal_cdf ( logx, a, b, cdf )

  end if

  return
end
