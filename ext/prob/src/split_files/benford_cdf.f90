subroutine benford_cdf ( x, cdf )

!*****************************************************************************80
!
!! BENFORD_CDF returns the Benford CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the string of significant digits to be
!    checked.  If X is 1, then we are asking for the Benford probability that
!    a value will have first digit 1.  If X is 123, we are asking for
!    the probability that the first three digits will be 123, and so on.
!
!    Output, real ( kind = 8 ) CDF, the Benford probability that an item taken
!    from a real world distribution will have the initial digit X or less.
!
  implicit none

  real ( kind = 8 ) cdf
  logical ( kind = 4 ) i4_is_power_of_10
  integer ( kind = 4 ) x

  if ( x <= 0 ) then
    cdf = 0.0D+00
  else if ( i4_is_power_of_10 ( x + 1 ) ) then
    cdf = 1.0D+00
  else
    cdf = log10 ( real ( x + 1, kind = 8 ) )
    cdf = mod ( cdf, 1.0D+00 )
  end if

  return
end
