function quasigeometric_check ( a, b )

!*****************************************************************************80
!
!! QUASIGEOMETRIC_CHECK checks the parameters of the Quasigeometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Output, logical QUASIGEOMETRIC_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical quasigeometric_check

  if ( a < 0.0D+00 .or. 1.0D+00 < a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'QUASIGEOMETRIC_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A < 0 or 1 < A.'
    quasigeometric_check = .false.
    return
  end if

  if ( b < 0.0D+00 .or. 1.0D+00 <= b ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'QUASIGEOMETRIC_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B < 0 or 1 <= B.'
    quasigeometric_check = .false.
    return
  end if

  quasigeometric_check = .true.

  return
end
