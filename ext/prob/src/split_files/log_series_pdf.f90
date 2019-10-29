subroutine log_series_pdf ( x, a, pdf )

!*****************************************************************************80
!
!! LOG_SERIES_PDF evaluates the Logarithmic Series PDF.
!
!  Discussion:
!
!    PDF(A;X) = - A**X / ( X * log ( 1 - A ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    0 < X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x

  if ( x <= 0 ) then
    pdf = 0.0D+00
  else
    pdf = - a**x / ( real ( x, kind = 8 ) * log ( 1.0D+00 - a ) )
  end if

  return
end
