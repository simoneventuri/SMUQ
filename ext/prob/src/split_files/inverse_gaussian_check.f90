function inverse_gaussian_check ( a, b )

!*****************************************************************************80
!
!! INVERSE_GAUSSIAN_CHECK checks the parameters of the Inverse Gaussian CDF.
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
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, logical INVERSE_GAUSSIAN_CHECK, is true if the parameters
!    are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical inverse_gaussian_check

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'INVERSE_GAUSSIAN_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    inverse_gaussian_check = .false.
    return
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'INVERSE_GAUSSIAN_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    inverse_gaussian_check = .false.
    return
  end if

  inverse_gaussian_check = .true.

  return
end
