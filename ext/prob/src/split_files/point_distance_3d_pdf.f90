subroutine point_distance_3d_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! POINT_DISTANCE_3D_PDF evaluates the point distance PDF in the 3D.
!
!  Discussion:
!
!    It is assumed that a set of points has been generated in 3D
!    according to a Poisson process.  The number of points in a region
!    of size VOLUME is a Poisson variate with mean value B * VOLUME.
!
!    For a point chosen at random, we may now find the nearest
!    Poisson point, the second nearest and so on.  We are interested
!    in the PDF that governs the expected behavior of the distances
!    of rank A = 1, 2, 3, ... with Poisson density B.
!
!    PDF(A,B;X) = 3 * ( (4/3) * B * PI )^A * X^( 3 * A - 1 )
!      * EXP ( - (4/3) * B * PI * X * X * X ) / ( A - 1 )!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2002
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996, pages 580.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X.
!
!    Input, integer ( kind = 4 ) A, indicates the degree of nearness of the
!    point.  A = 1 means the nearest point, A = 2 the second nearest, and so on.
!    0 < A.
!
!    Input, real ( kind = 8 ) B, the Poisson point density.  0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_factorial
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  if ( a < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POINT_DISTANCE_3D_PDF - Fatal error!'
    write ( *, '(a)' ) '  Input parameter A < 1.'
    stop 1
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POINT_DISTANCE_3D_PDF - Fatal error!'
    write ( *, '(a)' ) '  Input parameter B <= 0.0.'
    stop 1
  end if

  if ( x < 0.0D+00 ) then
    pdf = 0.0D+00
  else
    pdf = 3.0D+00 * ( ( 4.0D+00 / 3.0D+00 ) * b * r8_pi ) ** a &
      * x ** ( 3 * a - 1 ) * exp ( - ( 4.0D+00 / 3.0D+00 ) * b &
      * r8_pi * x ** 3 ) / r8_factorial ( a - 1 )
  end if

  return
end
