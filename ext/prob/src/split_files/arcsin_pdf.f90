subroutine arcsin_pdf ( x, a, pdf )

!*****************************************************************************80
!
!! ARCSIN_PDF evaluates the Arcsin PDF.
!
!  Discussion:
!
!    The LOGISTIC EQUATION has the form:
!
!      X(N+1) = 4 * LAMBDA * ( 1 - X(N) ).
!
!    where 0 < LAMBDA <= 1.  This nonlinear difference equation maps
!    the unit interval into itself, and is a simple example of a system
!    exhibiting chaotic behavior.  Ulam and von Neumann studied the
!    logistic equation with LAMBDA = 1, and showed that iterates of the
!    function generated a sequence of pseudorandom numbers with
!    the Arcsin probability density function.
!
!    The derived sequence
!
!      Y(N) = ( 2 / PI ) * Arcsin ( SQRT ( X(N) ) )
!
!    is a pseudorandom sequence with the uniform probability density
!    function on [0,1].  For certain starting values, such as X(0) = 0, 0.75,
!    or 1.0D+00, the sequence degenerates into a constant sequence, and for
!    values very near these, the sequence takes a while before becoming
!    chaotic.
!
!    The formula is:
!
!      PDF(X) = 1 / ( pi * sqrt ( A^2 - X^2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Daniel Zwillinger, Stephen Kokoska,
!    CRC Standard Probability and Statistics Tables and Formulae,
!    Chapman and Hall/CRC, 2000, pages 114-115.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    -A < X < A.
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ARCSIN_PDF - Fatal error!'
    write ( *, '(a)' ) '  Parameter A must be positive.'
    stop 1
  end if

  if ( x <= -a .or. a <= x ) then
    pdf = 0.0D+00
  else
    pdf = 1.0D+00 / ( r8_pi * sqrt ( a * a - x * x ) )
  end if

  return
end
