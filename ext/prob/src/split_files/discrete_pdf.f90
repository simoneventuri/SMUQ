subroutine discrete_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! DISCRETE_PDF evaluates the Discrete PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = B(X) if 1 <= X <= A
!                = 0    otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the item whose probability is desired.
!
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of
!    outcomes 1 through A.  Each entry must be nonnegative.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) b_sum
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x

  b_sum = sum ( b(1:a) )

  if ( 1 <= x .and. x <= a ) then
    pdf = b(x) / b_sum
  else
    pdf = 0.0D+00
  end if

  return
end
