function extreme_values_check ( a, b )

!*****************************************************************************80
!
!! EXTREME_VALUES_CHECK checks the parameters of the Extreme Values CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, logical EXTREME_VALUES_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical extreme_values_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'EXTREME_VALUES_CHECK - Warning!'
    write ( *, '(a)' ) '  B <= 0.'
    extreme_values_check = .false.
    return
  end if

  extreme_values_check = .true.

  return
end
