function cardioid_check ( a, b )

!*****************************************************************************80
!
!! CARDIOID_CHECK checks the parameters of the Cardioid CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    -0.5 <= B <= 0.5.
!
!    Output, logical CARDIOID_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical cardioid_check

  if ( b < -0.5D+00 .or. 0.5D+00 < b ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CARDIOID_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B < -0.5 or 0.5 < B.'
    cardioid_check = .false.
    return
  end if

  cardioid_check = .true.

  return
end
