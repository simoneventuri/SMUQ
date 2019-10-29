subroutine burr_variance ( a, b, c, d, variance )

!*****************************************************************************80
!
!! BURR_VARIANCE returns the variance of the Burr PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) mu1
  real ( kind = 8 ) mu2
  real ( kind = 8 ) r8_beta
  real ( kind = 8 ) variance

  if ( c <= 2.0D+00 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BURR_VARIANCE - Warning!'
    write ( *, '(a)' ) '  Variance undefined for C <= 2.'
    variance = huge ( variance )

  else

    mu1 = b     * d * r8_beta ( ( c * d - 1.0D+00 ) / c, ( c + 1.0D+00 ) / c )
    mu2 = b * b * d * r8_beta ( ( c * d - 2.0D+00 ) / c, ( c + 2.0D+00 ) / c )
    variance = - mu1 * mu1 + mu2

  end if

  return
end
