function sech_check ( a, b )

!*****************************************************************************80
!
!! SECH_CHECK checks the parameters of the Hyperbolic Secant CDF.
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
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, logical SECH_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical sech_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SECH_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.0'
    sech_check = .false.
    return
  end if

  sech_check = .true.

  return
end
