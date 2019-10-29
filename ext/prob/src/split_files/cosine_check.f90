function cosine_check ( a, b )

!*****************************************************************************80
!
!! COSINE_CHECK checks the parameters of the Cosine CDF.
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
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, logical COSINE_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical cosine_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COSINE_CHECK - Warning!'
    write ( *, '(a)' ) '  B <= 0.0'
    cosine_check = .false.
    return
  end if

  cosine_check = .true.

  return
end
