function log_uniform_check ( a, b )

!*****************************************************************************80
!
!! LOG_UNIFORM_CHECK checks the parameters of the Log Uniform CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1.0 < A < B.
!
!    Output, logical LOG_UNIFORM_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical log_uniform_check

  if ( a <= 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LOG_UNIFORM_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 1.'
    log_uniform_check = .false.
    return
  end if

  if ( b <= a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LOG_UNIFORM_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= A.'
    log_uniform_check = .false.
    return
  end if

  log_uniform_check = .true.

  return
end
