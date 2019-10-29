subroutine ribesl ( x, alpha, nb, ize, b, ncalc )

!*****************************************************************************80
!
!! RIBESL calculates I Bessel function with non-integer orders.
!
!  Discussion:
!
!    This routine calculates Bessel functions I SUB(N+ALPHA) (X)
!    for non-negative argument X, and non-negative order N+ALPHA,
!    with or without exponential scaling.
!
!    This program is based on a program written by David
!    Sookne that computes values of the Bessel functions J or
!    I of real argument and integer order.  Modifications include
!    the restriction of the computation to the I Bessel function
!    of non-negative real argument, the extension of the computation
!    to arbitrary positive order, the inclusion of optional
!    exponential scaling, and the elimination of most underflow.
!
!    In case of an error, NCALC will not equal NB, and not all I's are
!    calculated to the desired accuracy.
!
!    If NCALC < 0:  An argument is out of range. For example,
!    NB <= 0, IZE is not 1 or 2, or IZE = 1 and EXPARG <= ABS(X)
!    In this case, the B-vector is not calculated, and NCALC is
!    set to MIN(NB,0)-1 so that NCALC /= NB.
!
!    If 0 < NCALC < NB, then not all requested function values could
!    be calculated accurately.  This usually occurs because NB is
!    much larger than ABS(X).  In this case, B(N) is calculated
!    to the desired accuracy for N <= NCALC, but precision
!    is lost for NCALC < N <= NB.  If B(N) does not vanish
!    for NCALC < N (because it is too small to be represented),
!    and B(N)/B(NCALC) = 10 ^ (-K), then only the first NSIG-K
!    significant figures of B(N) can be trusted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Frank Olver, David Sookne,
!    A Note on Backward Recurrence Algorithms,
!    Mathematics of Computation,
!    Volume 26, 1972, pages 941-947.
!
!    David Sookne,
!    Bessel Functions of Real Argument and Integer Order,
!    NBS Journal of Research B,
!    Volume 77B, 1973, pages 125-132.
!
!    William Cody,
!    Algorithm 597:
!    Sequence of Modified Bessel Functions of the First Kind,
!    ACM Transactions of Mathematical Software,
!    Volume 9, Number 2, June 1983, pages 242-245.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument for which the functions
!    are to be calculated.
!
!    Input, real ( kind = 8 ) ALPHA,the fractional part of the order
!    for which the functions are to be calculated.
!    0 <= ALPHA < 1.0.
!
!    Input, integer ( kind = 4 ) NB, the number of functions to be calculated.
!    The first function calculated is of order ALPHA, and the
!    last is of order (NB - 1 + ALPHA).  1 <= NB.
!
!    Input, integer ( kind = 4 ) IZE, scaling option.
!    1, unscaled I's are to calculated,
!    2, exponentially scaled I's are to be calculated.
!
!    Output, real ( kind = 8 ) B(NB), the values of the functions
!    I(ALPHA,X) through I(NB-1+ALPHA,X), with scaling if requested.
!
!    Output, integer ( kind = 4 ) NCALC, error indicator.
!    If NCALC = NB, then all the requested values were calculated
!    to the desired accuracy.
!
!  Local Parameeters:
!
!    BETA, the radix for the floating-point system.
!
!    MINEXP, smallest representable power of BETA.
!
!    MAXEXP, smallest power of BETA that overflows
!
!    IT, number of bits in the mantissa of a working precision variable.
!
!    NSIG, decimal significance desired.  Should be set to
!    INT(LOG10(2)*IT+1).  Setting NSIG lower will result
!    in decreased accuracy while setting NSIG higher will
!    increase CPU time without increasing accuracy.  The
!    truncation error is limited to a relative error of
!    T=.5*10^(-NSIG).
!
!    ENTEN, 10.0^K, where K is the largest integer such that
!    ENTEN is machine-representable in working precision
!
!    ENSIG, 10.0^NSIG
!
!    RTNSIG, 10.0^(-K) for the smallest integer K such that
!    NSIG/4 <= K.
!
!    ENMTEN, smallest ABS(X) such that X/4 does not underflow
!
!    XLARGE, upper limit on the magnitude of X when IZE=2.  Bear
!    in mind that if ABS(X)=N, then at least N iterations
!    of the backward recursion will be executed.  The value
!    of 10.0^4 is used on every machine.
!
!    EXPARG, largest working precision argument that the library
!    EXP routine can handle and upper limit on the
!    magnitude of X when IZE=1; approximately log(BETA^MAXEXP).
!
  implicit none

  integer ( kind = 4 ) nb

  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(nb)
  real ( kind = 8 ), parameter :: const = 1.585D+00
  real ( kind = 8 ) em
  real ( kind = 8 ) empal
  real ( kind = 8 ) emp2al
  real ( kind = 8 ) en
  real ( kind = 8 ), parameter :: enmten = 8.9D-308
  real ( kind = 8 ), parameter :: ensig = 1.0D+16
  real ( kind = 8 ), parameter :: enten = 1.0D+308
  real ( kind = 8 ), parameter :: exparg = 709.0D+00
  logical flag
  real ( kind = 8 ), parameter :: half = 0.5D+00
  real ( kind = 8 ) halfx
  integer ( kind = 4 ) ize
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) magx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nbmx
  integer ( kind = 4 ) ncalc
  integer ( kind = 4 ) nend
  integer ( kind = 4 ), parameter :: nsig = 16
  integer ( kind = 4 ) nstart
  real ( kind = 8 ), parameter :: one = 1.0D+00
  real ( kind = 8 ) p
  real ( kind = 8 ) plast
  real ( kind = 8 ) pold
  real ( kind = 8 ) psave
  real ( kind = 8 ) psavel
  real ( kind = 8 ), parameter :: rtnsig = 1.0D-04
  real ( kind = 8 ) tempa
  real ( kind = 8 ) tempb
  real ( kind = 8 ) tempc
  real ( kind = 8 ) test
  real ( kind = 8 ) total
  real ( kind = 8 ) tover
  real ( kind = 8 ), parameter :: two = 2.0D+00
  real ( kind = 8 ) x
  real ( kind = 8 ), parameter :: xlarge = 1.0D+04
  real ( kind = 8 ), parameter :: zero = 0.0D+00
!
!  Check for X, NB, OR IZE out of range.
!
  if ( nb <= 0 ) then
    ncalc = min ( nb, 0 ) - 1
    return
  end if

  if ( x < 0.0D+00 ) then
    ncalc = min ( nb, 0 ) - 1
    return
  end if

  if ( alpha < 0.0D+00 ) then
    ncalc = min ( nb, 0 ) - 1
    return
  end if

  if ( 1.0D+00 <= alpha ) then
    ncalc = min ( nb, 0 ) - 1
    return
  end if

  if ( ize == 1 .and. exparg < x ) then
    ncalc = min ( nb, 0 ) - 1
    return
  end if

  if ( ize == 2 .and. xlarge < x ) then
    ncalc = min ( nb, 0 ) - 1
    return
  end if
!
!  Use 2-term ascending series for small X.
!
  ncalc = nb
  magx = int ( x )
!
!  Initialize the forward sweep, the P-sequence of Olver.
!
  if ( rtnsig <= x ) then

    nbmx = nb - magx
    n = magx + 1
    en = real ( n + n, kind = 8 ) + ( alpha + alpha )
    plast = one
    p = en / x
!
!  Calculate general significance test.
!
    test = ensig + ensig

    if ( 5 * nsig < 2 * magx ) then
      test = sqrt ( test * p )
    else
      test = test / const**magx
    end if
!
!  Calculate P-sequence until N = NB-1.  Check for possible overflow.
!
    flag = .false.

    if ( 3 <= nbmx ) then

      tover = enten / ensig
      nstart = magx + 2
      nend = nb - 1

      do k = nstart, nend

        n = k
        en = en + two
        pold = plast
        plast = p
        p = en * plast / x + pold
!
!  To avoid overflow, divide P-sequence by TOVER.  Calculate
!  P-sequence until 1 < ABS(P).
!
        if ( tover < p ) then

          tover = enten
          p = p / tover
          plast = plast / tover
          psave = p
          psavel = plast
          nstart = n + 1

          do

            n = n + 1
            en = en + two
            pold = plast
            plast = p
            p = en * plast / x + pold

            if ( 1.0D+00 < p ) then
              exit
            end if

          end do

          tempb = en / x
!
!  Calculate backward test, and find NCALC, the highest N
!  such that the test is passed.
!
          test = pold * plast / ensig
          test = test * ( half - half / ( tempb * tempb ) )
          p = plast * tover
          n = n - 1
          en = en - two
          nend = min ( nb, n )

          ncalc = nend + 1

          do l = nstart, nend

            pold = psavel
            psavel = psave
            psave = en * psavel / x + pold

            if ( test < psave * psavel ) then
              ncalc = l
              exit
            end if

          end do

          ncalc = ncalc - 1
          flag = .true.
          exit

        end if

      end do

      if ( .not. flag ) then

        n = nend
        en = real ( n + n, kind = 8 ) + ( alpha + alpha )
!
!  Calculate special significance test for 2 < NBMX.
!
        test = max ( test, sqrt ( plast * ensig ) * sqrt ( p + p ) )

      end if

    end if
!
!  Calculate P-sequence until significance test passed.
!
    if ( .not. flag ) then

      do

        n = n + 1
        en = en + two
        pold = plast
        plast = p
        p = en * plast / x + pold

        if ( test <= p ) then
          exit
        end if

      end do

    end if
!
!  Initialize the backward recursion and the normalization sum.
!
    n = n + 1
    en = en + two
    tempb = zero
    tempa = one / p
    em = real ( n, kind = 8 ) - one
    empal = em + alpha
    emp2al = ( em - one ) + ( alpha + alpha )
    total = tempa * empal * emp2al / em
    nend = n - nb
!
!  N < NB, so store B(N) and set higher orders to zero.
!
    if ( nend < 0 ) then

      b(n) = tempa
      nend = -nend

      do l = 1, nend
        b(n+l) = zero
      end do

      nend = n - 2
!
!  Calculate via difference equation and store B(N), until N = 2.
!
      if ( 0 < nend ) then

        do l = 1, nend
          n = n - 1
          en = en - two
          b(n) = ( en * b(n+1) ) / x + b(n+2)
          em = em - one
          emp2al = emp2al - one
          if ( n == 2 ) then
            emp2al = one
          end if
          empal = empal - one
          total = ( total + b(n) * empal ) * emp2al / em
        end do

      end if
!
!  Calculate B(1).
!
      b(1) = two * empal * b(2) / x + b(3)

      total = ( total + total ) + b(1)
!
!  Recur backward via difference equation, calculating (but
!  not storing) B(N), until N = NB.
!
    else

      if ( 0 < nend ) then

        do l = 1, nend

          n = n - 1
          en = en - two
          tempc = tempb
          tempb = tempa
          tempa = ( en * tempb ) / x + tempc
          em = em - one
          emp2al = emp2al - one

          if ( n == 1 ) then
            exit
          end if

          if ( n == 2 ) then
            emp2al = one
          end if

          empal = empal - one
          total = ( total + tempa * empal ) * emp2al / em

        end do

      end if
!
!  Store B(NB).
!
      b(n) = tempa

      if ( nb <= 1 ) then

        total = ( total + total ) + tempa
!
!  Calculate and Store B(NB-1).
!
      else

        n = n - 1
        en = en - two
        b(n) = ( en * tempa ) / x + tempb

        if ( 1 < n  ) then

          em = em - one
          emp2al = emp2al - one

          if ( n == 2 ) then
            emp2al = one
          end if

          empal = empal - one
          total = ( total + b(n) * empal ) * emp2al / em

          nend = n - 2
!
!  Calculate via difference equation and store B(N), until N = 2.
!
          if ( 0 < nend ) then

            do l = 1, nend
              n = n - 1
              en = en - two
              b(n) = ( en * b(n+1) ) / x + b(n+2)
              em = em - one
              emp2al = emp2al - one
              if ( n == 2 ) then
                emp2al = one
              end if
              empal = empal - one
              total = ( total + b(n) * empal ) * emp2al / em
            end do

          end if
!
!  Calculate B(1).
!
          b(1) = two * empal * b(2) / x + b(3)

        end if

        total = ( total + total ) + b(1)

      end if

    end if
!
!  Normalize.  Divide all B(N) by TOTAL.
!


    if ( alpha /= zero ) then
       total = total * gamma ( one + alpha ) * ( x * half ) ** ( - alpha )
    end if

    if ( ize == 1 ) then
      total = total * exp ( -x )
    end if

    tempa = enmten

    if ( 1.0D+00 < total ) then
      tempa = tempa * total
    end if

    do n = 1, nb
      if ( b(n) < tempa ) then
        b(n) = zero
      end if
      b(n) = b(n) / total
    end do

    return
!
!  Two-term ascending series for small X.
!
  else

    tempa = one
    empal = one + alpha
    halfx = zero

    if ( enmten < x ) then
      halfx = half * x
    end if

    if ( alpha /= zero ) then
      tempa = halfx ** alpha / gamma ( empal )
    end if

    if ( ize == 2 ) then
      tempa = tempa * exp ( - x )
    end if

    tempb = zero

    if ( one < x + one ) then
      tempb = halfx * halfx
    end if

    b(1) = tempa + tempa * tempb / empal

    if ( x /= zero .and. b(1) == zero ) then
      ncalc = 0
    end if

    if ( 1 < nb ) then

      if ( x == zero ) then

        b(2:nb) = zero
!
!  Calculate higher-order functions.
!
      else

        tempc = halfx
        tover = ( enmten + enmten ) / x

        if ( tempb /= zero ) then
          tover = enmten / tempb
        end if

        do n = 2, nb

          tempa = tempa / empal
          empal = empal + one
          tempa = tempa * tempc

          if ( tempa <= tover * empal ) then
            tempa = zero
          end if

          b(n) = tempa + tempa * tempb / empal

          if ( b(n) == zero .and. n < ncalc ) then
            ncalc = n - 1
          end if

        end do

      end if

    end if

  end if

  return
end
