function logistic_check ( a, b )

!*****************************************************************************80
!
!! LOGISTIC_CHECK checks the parameters of the Logistic CDF.
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
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, logical LOGISTIC_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical logistic_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LOGISTIC_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    logistic_check = .false.
    return
  end if

  logistic_check = .true.

  return
end
