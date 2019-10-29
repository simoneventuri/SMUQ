subroutine f_cdf ( x, m, n, cdf )

!*****************************************************************************80
!
!! F_CDF evaluates the F central CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) arg1
  real ( kind = 8 ) arg2
  real ( kind = 8 ) arg3
  real ( kind = 8 ) beta_inc
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) x

  if ( x <= 0.0D+00 ) then

    cdf = 0.0D+00

  else

    arg1 = 0.5D+00 * real ( n, kind = 8 )
    arg2 = 0.5D+00 * real ( m, kind = 8 )
    arg3 = real ( n, kind = 8 ) &
      / ( real ( n, kind = 8 ) + real ( m, kind = 8 ) * x )

    cdf = 1.0D+00 - beta_inc ( arg1, arg2, arg3 )

  end if

  return
end
