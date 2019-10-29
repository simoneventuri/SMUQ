function cauchy_check ( a, b )

!*****************************************************************************80
!
!! CAUCHY_CHECK checks the parameters of the Cauchy CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 1999
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
!    Output, logical CAUCHY_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical cauchy_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CAUCHY_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    cauchy_check = .false.
    return
  end if

  cauchy_check = .true.

  return
end
