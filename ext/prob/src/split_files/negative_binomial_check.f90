function negative_binomial_check ( a, b )

!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_CHECK checks the parameters of the Negative Binomial PDF.
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
!    Input, integer ( kind = 4 ) A, a parameter of the PDF.
!    0 <= A.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    0 < B <= 1.
!
!    Output, logical NEGATIVE_BINOMIAL_CHECK, is true if the
!    parameters are legal.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  logical negative_binomial_check

  if ( a < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'NEGATIVE_BINOMIAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A < 0.'
    negative_binomial_check = .false.
    return
  end if

  if ( b <= 0.0D+00 .or. 1.0D+00 < b ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'NEGATIVE_BINOMIAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0 or 1 < B.'
    negative_binomial_check = .false.
    return
  end if

  negative_binomial_check = .true.

  return
end
