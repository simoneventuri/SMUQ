subroutine benford_pdf ( x, pdf )

!*****************************************************************************80
!
!! BENFORD_PDF returns the Benford PDF.
!
!  Discussion:
!
!    Benford's law is an empirical formula explaining the observed
!    distribution of initial digits in lists culled from newspapers,
!    tax forms, stock market prices, and so on.  It predicts the observed
!    high frequency of the initial digit 1, for instance.
!
!    Note that the probabilities of digits 1 through 9 are guaranteed
!    to add up to 1, since
!      LOG10 ( 2/1 ) + LOG10 ( 3/2) + LOG10 ( 4/3 ) + ... + LOG10 ( 10/9 )
!      = LOG10 ( 2/1 * 3/2 * 4/3 * ... * 10/9 ) = LOG10 ( 10 ) = 1.
!
!    The formula is:
!
!      PDF(X) = LOG10 ( ( X + 1 ) / X ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Frank Benford,
!    The Law of Anomalous Numbers,
!    Proceedings of the American Philosophical Society,
!    Volume 78, pages 551-572, 1938.
!
!    Ted Hill,
!    The First Digit Phenomenon,
!    American Scientist,
!    Volume 86, July/August 1998, pages 358 - 363.
!
!    Ralph Raimi,
!    The Peculiar Distribution of First Digits,
!    Scientific American,
!    December 1969, pages 109-119.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the string of significant digits to be
!    checked.  If X is 1, then we are asking for the Benford probability that
!    a value will have first digit 1.  If X is 123, we are asking for
!    the probability that the first three digits will be 123, and so on.
!
!    Output, real ( kind = 8 ) PDF, the Benford probability that an item taken
!    from a real world distribution will have the initial digits X.
!
  implicit none

  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x

  if ( x <= 0 ) then
    pdf = 0.0D+00
  else
    pdf = log10 ( real ( x + 1, kind = 8 ) / real ( x, kind = 8 ) )
  end if

  return
end
