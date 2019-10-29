subroutine zipf_mean ( a, mean )

!*****************************************************************************80
!
!! ZIPF_MEAN returns the mean of the Zipf PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0D+00 < A.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!    The mean is only defined for 2 < A.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) mean
  real ( kind = 8 ) r8_zeta

  if ( a <= 2.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ZIPF_MEAN - Fatal error!'
    write ( *, '(a)' ) '  No mean defined for A <= 2.'
    stop 1
  end if

  mean = r8_zeta ( a - 1.0D+00 ) / r8_zeta ( a )

  return
end
