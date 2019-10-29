function pearson_05_check ( a, b, c )

!*****************************************************************************80
!
!! PEARSON_05_CHECK checks the parameters of the Pearson 5 PDF.
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
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, logical PEARSON_05_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  logical pearson_05_check

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PEARSON_05_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    pearson_05_check = .false.
    return
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PEARSON_05_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    pearson_05_check = .false.
    return
  end if

  pearson_05_check = .true.

  return
end
