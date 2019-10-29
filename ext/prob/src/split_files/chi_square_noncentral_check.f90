function chi_square_noncentral_check ( a, b )

!*****************************************************************************80
!
!! CHI_SQUARE_NONCENTRAL_CHECK check parameters of noncentral Chi Squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the parameter of the PDF.
!    1.0D+00 <= A.
!
!    Input, real ( kind = 8 ) B, the noncentrality parameter of the PDF.
!    0.0 <= B.
!
!    Output, logical CHI_SQUARE_NONCENTRAL_CHECK, is true if the parameters
!    are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical chi_square_noncentral_check

  if ( a < 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHI_SQUARE_NONCENTRAL_CHECK - Warning!'
    write ( *, '(a)' ) '  A < 1.'
    chi_square_noncentral_check = .false.
    return
  end if

  if ( b < 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHI_SQUARE_NONCENTRAL_CHECK - Warning!'
    write ( *, '(a)' ) '  B < 0.'
    chi_square_noncentral_check = .false.
    return
  end if

  chi_square_noncentral_check = .true.

  return
end
