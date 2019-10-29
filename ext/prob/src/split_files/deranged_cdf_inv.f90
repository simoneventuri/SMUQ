subroutine deranged_cdf_inv ( cdf, a, x )

!*****************************************************************************80
!
!! DERANGED_CDF_INV inverts the Deranged CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, integer ( kind = 4 ) A, the number of items.
!    1 <= A.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf2
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DERANGED_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  cdf2 = 0.0D+00

  do x2 = 0, a

    call deranged_pdf ( x2, a, pdf )

    cdf2 = cdf2 + pdf

    if ( cdf <= cdf2 ) then
      x = x2
      return
    end if

  end do

  x = a

  return
end
