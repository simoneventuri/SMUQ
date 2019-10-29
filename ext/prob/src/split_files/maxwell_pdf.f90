subroutine maxwell_pdf ( x, a, pdf )

!*****************************************************************************80
!
!! MAXWELL_PDF evaluates the Maxwell PDF.
!
!  Discussion:
!
!    PDF(A;X) = exp ( - 0.5D+00 * ( X / A )^2 ) * ( X / A )^2 /
!      ( sqrt ( 2 ) * A * GAMMA ( 1.5D+00 ) )
!
!    MAXWELL_PDF(A;X) = CHI_PDF(0,A,3;X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0 < X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x <= 0.0D+00 ) then

    pdf = 0.0D+00

  else

    y = x / a

    pdf = exp ( - 0.5D+00 * y * y ) * y * y &
      / ( sqrt ( 2.0D+00 ) * a * gamma ( 1.5D+00 ) )

  end if

  return
end
