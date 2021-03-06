subroutine point_distance_1d_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! POINT_DISTANCE_1D_PDF evaluates the point distance PDF in 1D.
!
!  Discussion:
!
!    It is assumed that a set of points has been generated in 1D
!    according to a Poisson process.  The number of points in a region
!    of size LENGTH is a Poisson variate with mean value B * LENGTH.
!
!    For a point chosen at random, we may now find the nearest
!    Poisson point, the second nearest and so on.  We are interested
!    in the PDF that governs the expected behavior of the distances
!    of rank A = 1, 2, 3, ... with Poisson density B.
!
!    Note that this PDF is a form of the Gamma PDF.???
!
!    PDF(A,B;X) = B^A * X^( A - 1 ) * exp ( - B * X ) / ( A - 1 )!
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
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X.
!
!    Input, integer ( kind = 4 ) A, indicates the degree of nearness of the
!    point.  A = 1 means the nearest point, A = 2 the second nearest, and so on.
!    0 < A.
!
!    Input, real ( kind = 8 ) B, the point density.  0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_factorial
  real ( kind = 8 ) x

  if ( a < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POINT_DISTANCE_1D_PDF - Fatal error!'
    write ( *, '(a)' ) '  Input parameter A < 1.'
    stop 1
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POINT_DISTANCE_1D_PDF - Fatal error!'
    write ( *, '(a)' ) '  Input parameter B <= 0.0.'
    stop 1
  end if

  if ( x < 0.0D+00 ) then
    pdf = 0.0D+00
  else
    pdf = b ** a * x ** ( a - 1 ) * exp ( - b * x ) / r8_factorial ( a - 1 )
  end if

  return
end
