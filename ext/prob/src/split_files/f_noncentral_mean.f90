subroutine f_noncentral_mean ( a, m, n, mean )

!*****************************************************************************80
!
!! F_NONCENTRAL_MEAN returns the mean of the F noncentral PDF.
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
!    Note, however, that the mean is not defined unless 3 <= N.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) m
  real ( kind = 8 ) mean
  integer ( kind = 4 ) n

  if ( n < 3 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F_NONCENTRAL_MEAN - Fatal error!'
    write ( *, '(a)' ) '  The mean is not defined for N < 3.'
    stop 1
  end if

  mean = ( real ( m, kind = 8 ) + a ) * real ( n, kind = 8 ) &
    / ( real ( m, kind = 8 ) * real ( n - 2, kind = 8 ) )

  return
end
