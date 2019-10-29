function eval_pol ( a, n, x )

!*****************************************************************************80
!
!! EVAL_POL evaluates a polynomial at X.
!
!  Discussion:
!
!    EVAL_POL = A(0) + A(1)*X + ... + A(N)*X**N
!
!  Modified:
!
!    15 December 1999
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(0:N), coefficients of the polynomial.
!
!    Input, integer ( kind = 4 ) N, length of A.
!
!    Input, real ( kind = 8 ) X, the point at which the polynomial
!    is to be evaluated.
!
!    Output, real ( kind = 8 ) EVAL_POL, the value of the polynomial at X.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(0:n)
  real ( kind = 8 ) eval_pol
  integer ( kind = 4 ) i
  real ( kind = 8 ) term
  real ( kind = 8 ) x

  term = a(n)
  do i = n - 1, 0, -1
    term = term * x + a(i)
  end do

  eval_pol = term

  return
end
