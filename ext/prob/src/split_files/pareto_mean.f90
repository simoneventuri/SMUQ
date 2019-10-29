subroutine pareto_mean ( a, b, mean )

!*****************************************************************************80
!
!! PARETO_MEAN returns the mean of the Pareto PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
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
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mean

  if ( b <= 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PARETO_MEAN - Fatal error!'
    write ( *, '(a)' ) '  For B <= 1, the mean does not exist.'
    mean = 0.0D+00
    return
  end if

  mean = b * a / ( b - 1.0D+00 )

  return
end
