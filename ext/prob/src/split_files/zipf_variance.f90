subroutine zipf_variance ( a, variance )

!*****************************************************************************80
!
!! ZIPF_VARIANCE returns the variance of the Zipf PDF.
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
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!    The variance is only defined for 3 < A.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) mean
  real ( kind = 8 ) r8_zeta
  real ( kind = 8 ) variance

  if ( a <= 3.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ZIPF_VARIANCE - Fatal error!'
    write ( *, '(a)' ) '  No variance defined for A <= 3.0.'
    stop 1
  end if

  call zipf_mean ( a, mean )

  variance = r8_zeta ( a - 2.0D+00 ) / r8_zeta ( a ) - mean * mean

  return
end
