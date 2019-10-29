function r8_error_f_inverse ( y )

!*****************************************************************************80
!
!! R8_ERROR_F_INVERSE inverts the error function ERF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Y, the value of the error function.
!
!    Output, real ( kind = 8 ) R8_ERROR_F_INVERSE, the value X such that
!    ERF(X) = Y.
!
  implicit none

  real ( kind = 8 ) r8_error_f_inverse
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  z = ( y + 1.0D+00 ) / 2.0D+00

  call normal_01_cdf_inv ( z, x )

  r8_error_f_inverse = x / sqrt ( 2.0D+00 )

  return
end
