function beta_binomial_check ( a, b, c )

!*****************************************************************************80
!
!! BETA_BINOMIAL_CHECK checks the parameters of the Beta Binomial PDF.
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
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Output, logical BETA_BINOMIAL_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical beta_binomial_check
  integer ( kind = 4 ) c

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_BINOMIAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    beta_binomial_check = .false.
    return
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_BINOMIAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    beta_binomial_check = .false.
    return
  end if

  if ( c < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_BINOMIAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  C < 0.'
    beta_binomial_check = .false.
    return
  end if

  beta_binomial_check = .true.

  return
end
