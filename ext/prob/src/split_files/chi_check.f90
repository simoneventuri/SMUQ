function chi_check ( a, b, c )

!*****************************************************************************80
!
!! CHI_CHECK checks the parameters of the Chi CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, logical CHI_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  logical chi_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHI_CHECK - Warning!'
    write ( *, '(a)' ) '  B <= 0.0.'
    chi_check = .false.
    return
  end if

  if ( c <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHI_CHECK - Warning!'
    write ( *, '(a)' ) '  C <= 0.0.'
    chi_check = .false.
    return
  end if

  chi_check = .true.

  return
end
