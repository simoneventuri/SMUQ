function log_series_check ( a )

!*****************************************************************************80
!
!! LOG_SERIES_CHECK checks the parameter of the Logarithmic Series PDF.
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
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Output, logical LOG_SERIES_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  logical log_series_check

  if ( a <= 0.0D+00 .or. 1.0D+00 <= a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LOG_SERIES_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.0D+00 or 1.0D+00 <= A'
    log_series_check = .false.
    return
  end if

  log_series_check = .true.

  return
end
