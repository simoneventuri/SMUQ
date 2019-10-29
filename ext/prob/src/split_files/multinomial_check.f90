function multinomial_check ( a, b, c )

!*****************************************************************************80
!
!! MULTINOMIAL_CHECK checks the parameters of the Multinomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!
!    Input, integer ( kind = 4 ) B, the number of outcomes possible on one
!    trial.  1 <= B.
!
!    Input, real ( kind = 8 ) C(B).  C(I) is the probability of outcome I on
!    any trial.
!    0.0 <= C(I) <= 1.0D+00,
!    Sum ( 1 <= I <= B ) C(I) = 1.0.
!
!    Output, logical MULTINOMIAL_CHECK, is true if the parameters are legal.
!
  implicit none

  integer ( kind = 4 ) b

  integer ( kind = 4 ) a
  real ( kind = 8 ) c(b)
  real ( kind = 8 ) c_sum
  integer ( kind = 4 ) i
  logical multinomial_check

  if ( b < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTINOMIAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B < 1.'
    multinomial_check = .false.
    return
  end if

  do i = 1, b

    if ( c(i) < 0.0D+00 .or. 1.0D+00 < c(i) ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MULTINOMIAL_CHECK - Fatal error!'
      write ( *, '(a)' ) '  Input C(I) is out of range.'
      multinomial_check = .false.
      return
    end if

  end do

  c_sum = sum ( c )

  if ( 0.0001D+00 < abs ( 1.0D+00 - c_sum ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTINOMIAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  The probabilities do not sum to 1.'
    multinomial_check = .false.
    return
  end if

  multinomial_check = .true.

  return
end
