subroutine dirichlet_pdf ( x, n, a, pdf )

!*****************************************************************************80
!
!! DIRICHLET_PDF evaluates the Dirichlet PDF.
!
!  Discussion:
!
!    PDF(N,A;X) = Product ( 1 <= I <= N ) X(I)^( A(I) - 1 )
!      * Gamma ( A_SUM ) / A_PROD
!
!    where
!
!      0 <= A(I) for all I;
!      0 <= X(I) for all I;
!      Sum ( 1 <= I <= N ) X(I) = 1;
!      A_SUM = Sum ( 1 <= I <= N ) A(I).
!      A_PROD = Product ( 1 <= I <= N ) Gamma ( A(I) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the argument of the PDF.  Each X(I) should
!    be greater than 0.0D+00, and the X(I)'s must add up to 1.0.
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be nonnegative, and at least one should be
!    positive.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_prod
  real ( kind = 8 ) a_sum
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: tol = 0.0001D+00
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_sum

  do i = 1, n
    if ( x(i) <= 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DIRICHLET_PDF - Fatal error!'
      write ( *, '(a)' ) '  X(I) <= 0.'
      stop 1
    end if
  end do

  x_sum = sum ( x(1:n) )

  if ( tol < abs ( x_sum - 1.0D+00 ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DIRICHLET_PDF - Fatal error!'
    write ( *, '(a)' ) '  SUM X(I) =/= 1.'
    stop 1
  end if

  a_sum = sum ( a(1:n) )

  a_prod = 1.0D+00
  do i = 1, n
    a_prod = a_prod * gamma ( a(i) )
  end do

  pdf = gamma ( a_sum ) / a_prod
  do i = 1, n
    pdf = pdf * x(i) ** ( a(i) - 1.0D+00 )
  end do

  return
end
