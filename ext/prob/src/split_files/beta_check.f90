function beta_check ( a, b )

!*****************************************************************************80
!
!! BETA_CHECK checks the parameters of the Beta PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, logical BETA_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical beta_check

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    beta_check = .false.
    return
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    beta_check = .false.
    return
  end if

  beta_check = .true.

  return
end
