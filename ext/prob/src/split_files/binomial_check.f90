function binomial_check ( a, b )

!*****************************************************************************80
!
!! BINOMIAL_CHECK checks the parameter of the Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
!
!    Output, logical BINOMIAL_CHECK, is true if the parameters are legal.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  logical binomial_check

  if ( a < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BINOMIAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A < 1.'
    binomial_check = .false.
    return
  end if

  if ( b < 0.0D+00 .or. 1.0D+00 < b ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BINOMIAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B < 0 or 1 < B.'
    binomial_check = .false.
    return
  end if

  binomial_check = .true.

  return
end
