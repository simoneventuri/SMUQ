subroutine quasigeometric_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! QUASIGEOMETRIC_PDF evaluates the Quasigeometric PDF.
!
!  Discussion:
!
!    PDF(A,B;X) =    A                     if 0  = X;
!               = (1-A) * (1-B) * B^(X-1)  if 1 <= X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Darren Glass, Philip Lowry,
!    Quasiquasigeometric Distributions and Extra Inning Baseball Games,
!    Mathematics Magazine,
!    Volume 81, Number 2, April 2008, pages 127-137.
!
!    Paul Nahin,
!    Digital Dice: Computational Solutions to Practical Probability Problems,
!    Princeton University Press, 2008,
!    ISBN13: 978-0-691-12698-2,
!    LC: QA273.25.N34.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the independent variable.
!    0 <= X
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x

  if ( x < 0 ) then

    pdf = 0.0D+00

  else if ( x == 0 ) then

    pdf = a

  else if ( b == 0.0D+00 ) then

    if ( x == 1 ) then
      pdf = 1.0D+00
    else
      pdf = 0.0D+00
    end if

  else

    pdf = ( 1.0D+00 - a ) * ( 1.0D+00 - b ) * b ** ( x - 1 )

  end if

  return
end
