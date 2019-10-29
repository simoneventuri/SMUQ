subroutine f_variance ( m, n, variance )

!*****************************************************************************80
!
!! F_VARIANCE returns the variance of the F central PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!    Note, however, that the variance is not defined unless 5 <= N.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) variance

  if ( n < 5 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F_VARIANCE - Fatal error!'
    write ( *, '(a)' ) '  The variance is not defined for N < 5.'
    stop 1
  end if

  variance = real ( 2 * n * n * ( m + n - 2 ), kind = 8 ) / &
    real ( m * ( n - 2 ) ** 2 * ( n - 4 ), kind = 8 )

  return
end
