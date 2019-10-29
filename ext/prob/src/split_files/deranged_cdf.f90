subroutine deranged_cdf ( x, a, cdf )

!*****************************************************************************80
!
!! DERANGED_CDF evaluates the Deranged CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the maximum number of items in
!    their correct places.
!    0 <= X <= A.
!
!    Input, integer ( kind = 4 ) A, the number of items.
!    1 <= A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) cnk
  integer ( kind = 4 ) deranged_enum
  integer ( kind = 4 ) dnmk
  integer ( kind = 4 ) i4_choose
  real ( kind = 8 ) r8_factorial
  integer ( kind = 4 ) sum2
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  if ( x < 0 .or. a < x ) then
    cdf = 0.0D+00
  else
    sum2 = 0
    do x2 = 0, x
      cnk = i4_choose ( a, x2 )
      dnmk = deranged_enum ( a - x2 )
      sum2 = sum2 + cnk * dnmk
    end do
    cdf = real ( sum2, kind = 8 ) / r8_factorial ( a )
  end if

  return
end
