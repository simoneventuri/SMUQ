function genlogistic_check ( a, b, c )

!*****************************************************************************80
!
!! GENLOGISTIC_CHECK checks the parameters of the Generalized Logistic CDF.
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
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, logical GENLOGISTIC_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  logical genlogistic_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENLOGISTIC_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    genlogistic_check = .false.
    return
  end if

  if ( c <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GENLOGISTIC_CHECK - Fatal error!'
    write ( *, '(a)' ) '  C <= 0.'
    genlogistic_check = .false.
    return
  end if

  genlogistic_check = .true.

  return
end
