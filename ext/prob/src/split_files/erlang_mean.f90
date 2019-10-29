subroutine erlang_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! ERLANG_MEAN returns the mean of the Erlang PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  real ( kind = 8 ) mean

  mean =  a + b * real ( c, kind = 8 )

  return
end
