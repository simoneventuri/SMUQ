subroutine f_pdf ( x, m, n, pdf )

!*****************************************************************************80
!
!! F_PDF evaluates the F central PDF.
!
!  Discussion:
!
!    PDF(M,N;X) = M^(M/2) * X^((M-2)/2)
!      / ( Beta(M/2,N/2) * N^(M/2) * ( 1 + (M/N) * X )^((M+N)/2)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) bot1
  real ( kind = 8 ) bot2
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_beta
  real ( kind = 8 ) top
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then

    pdf = 0.0D+00

  else

    a = real ( m, kind = 8 )
    b = real ( n, kind = 8 )

    top = sqrt ( a ** m * b ** n * x ** ( m - 2 ) )
    bot1 = r8_beta ( a / 2.0D+00, b / 2.0D+00 )
    bot2 =  sqrt ( ( b + a * x ) ** ( m + n ) )

    pdf = top / ( bot1 * bot2 )

  end if

  return
end
