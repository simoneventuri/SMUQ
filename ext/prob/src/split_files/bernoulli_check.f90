function bernoulli_check ( a )

!*****************************************************************************80
!
!! BERNOULLI_CHECK checks the parameter of the Bernoulli CDF.
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
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 <= A <= 1.0.
!
!    Output, logical BERNOULLI_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  logical bernoulli_check

  if ( a < 0.0D+00 .or. 1.0D+00 < a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BERNOULLI_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A < 0 or 1 < A.'
    bernoulli_check = .false.
    return
  end if

  bernoulli_check = .true.

  return
end
