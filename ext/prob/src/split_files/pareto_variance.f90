subroutine pareto_variance ( a, b, variance )

!*****************************************************************************80
!
!! PARETO_VARIANCE returns the variance of the Pareto PDF.
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
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) variance

  if ( b <= 2.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PARETO_VARIANCE - Warning!'
    write ( *, '(a)' ) '  For B <= 2, the variance does not exist.'
    variance = 0.0D+00
    return
  end if

  variance = a * a * b / ( ( b - 1.0D+00 ) ** 2 * ( b - 2.0D+00 ) )

  return
end
