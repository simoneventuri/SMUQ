subroutine f_noncentral_variance ( a, m, n, variance )

!*****************************************************************************80
!
!! F_NONCENTRAL_VARIANCE returns the variance of the F noncentral PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!
!    Input, integer ( kind = 4 ) M, N, parameters of the PDF.
!    1 <= M,
!    1 <= N.
!    Note, however, that the variance is not defined unless 5 <= N.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) m
  real ( kind = 8 ) mr
  integer ( kind = 4 ) n
  real ( kind = 8 ) nr
  real ( kind = 8 ) variance

  if ( n < 5 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F_NONCENTRAL_VARIANCE - Fatal error!'
    write ( *, '(a)' ) '  The variance is not defined for N < 5.'
    stop 1
  end if

  mr = real ( m, kind = 8 )
  nr = real ( n, kind = 8 )

  variance = ( ( mr + a ) ** 2 + 2.0D+00 * ( mr + a ) * nr ** 2 ) &
    / ( ( nr - 2.0D+00 ) * ( nr - 4.0D+00 ) * mr ** 2 ) - &
    ( mr + a ) ** 2 * nr ** 2 / ( ( nr - 2.0D+00 ) ** 2 * mr ** 2 )

  return
end
