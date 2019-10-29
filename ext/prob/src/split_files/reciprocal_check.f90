function reciprocal_check ( a, b )

!*****************************************************************************80
!
!! RECIPROCAL_CHECK checks the parameters of the Reciprocal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A <= B.
!
!    Output, logical RECIPROCAL_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical reciprocal_check

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'RECIPROCAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.0'
    reciprocal_check = .false.
    return
  end if

  if ( b < a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'RECIPROCAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B < A'
    reciprocal_check = .false.
    return
  end if

  reciprocal_check = .true.

  return
end
