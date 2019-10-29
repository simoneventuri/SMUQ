function gompertz_check ( a, b )

!*****************************************************************************80
!
!! GOMPERTZ_CHECK checks the parameters of the Gompertz PDF.
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
!  Reference:
!
!    Norman Johnson, Samuel Kotz, Balakrishnan,
!    Continuous Univariate Distributions, second edition,
!    Wiley, 1994,
!    QA273.6.J6
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1 < A, 0 < B.
!
!    Output, logical GOMPERTZ_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical gompertz_check

  if ( a <= 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GOMPERTZ_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 1.0!'
    gompertz_check = .false.
    return
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GOMPERTZ_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.0!'
    gompertz_check = .false.
    return
  end if

  gompertz_check = .true.

  return
end
