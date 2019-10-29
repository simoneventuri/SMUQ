function exponential_check ( a, b )

!*****************************************************************************80
!
!! EXPONENTIAL_CHECK checks the parameters of the Exponential CDF.
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
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, logical EXPONENTIAL_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical exponential_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'EXPONENTIAL_CHECK - Warning!'
    write ( *, '(a)' ) '  B <= 0.0'
    exponential_check = .false.
    return
  end if

  exponential_check = .true.

  return
end
