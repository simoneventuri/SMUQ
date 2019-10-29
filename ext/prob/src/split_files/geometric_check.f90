function geometric_check ( a )

!*****************************************************************************80
!
!! GEOMETRIC_CHECK checks the parameter of the Geometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, logical GEOMETRIC_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  logical geometric_check

  if ( a < 0.0D+00 .or. 1.0D+00 < a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GEOMETRIC_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A < 0 or 1 < A.'
    geometric_check = .false.
    return
  end if

  geometric_check = .true.

  return
end
