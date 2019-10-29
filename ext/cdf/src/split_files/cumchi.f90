subroutine cumchi ( x, df, cum, ccum )

!*****************************************************************************80
!
!! CUMCHI evaluates the cumulative chi-square distribution.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom of the
!    chi-square distribution.
!
!    Output, real ( kind = 8 ) CUM, the cumulative chi-square distribution.
!
!    Output, real ( kind = 8 ) CCUM, the complement of the cumulative
!    chi-square distribution.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) x
  real ( kind = 8 ) xx

  a = df * 0.5D+00
  xx = x * 0.5D+00

  call cumgam ( xx, a, cum, ccum )

  return
end
