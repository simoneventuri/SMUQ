function log_normal_check ( a, b )

!*****************************************************************************80
!
!! LOG_NORMAL_CHECK checks the parameters of the Lognormal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
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
!    Output, logical LOG_NORMAL_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical log_normal_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LOG_NORMAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    log_normal_check = .false.
    return
  end if

  log_normal_check = .true.

  return
end
