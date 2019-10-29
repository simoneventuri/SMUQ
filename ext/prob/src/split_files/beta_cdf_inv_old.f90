subroutine beta_cdf_inv_old ( cdf, a, b, x )

!*****************************************************************************80
!
!! BETA_CDF_INV_OLD inverts the Beta CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2001
!
!  Author:
!
!    Original FORTRAN77 version by Roger Abernathy, Robert Smith.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Roger Abernathy, Robert Smith,
!    Algorithm 724,
!    Program to Calculate F Percentiles,
!    ACM Transactions on Mathematical Software,
!    Volume 19, Number 4, December 1993, pages 481-483.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the argument of the CDF.
!
  implicit none

  integer ( kind = 4 ), parameter :: maxk = 20

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) bcoeff
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf_x
  real ( kind = 8 ) d(2:maxk,0:maxk-2)
  real ( kind = 8 ), parameter :: error = 0.0001D+00
  real ( kind = 8 ), parameter :: errapp = 0.01D+00
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) loopct
  real ( kind = 8 ) pdf_x
  real ( kind = 8 ) q
  real ( kind = 8 ) s1
  real ( kind = 8 ) s2
  real ( kind = 8 ) sum2
  real ( kind = 8 ) t
  real ( kind = 8 ) tail
  real ( kind = 8 ) x
  real ( kind = 8 ) xold

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if
!
!  Estimate the solution.
!
  x = a / ( a + b )

  xold = 0.0D+00
  loopct = 2

  do while ( errapp <= abs ( ( x - xold ) / x ) .and. loopct /= 0 )

    xold = x
    loopct = loopct - 1
!
!  CDF_X = PROB { BETA(A,B) <= X }.
!  Q = ( CDF - CDF_X ) / PDF_X.
!
    call beta_cdf ( x, a, b, cdf_x )

    call beta_pdf ( x, a, b, pdf_x )

    q = ( cdf - cdf_x ) / pdf_x
!
!  D(N,K) = C(N,K) * Q^(N+K-1) / (N-1)!
!
    t = 1.0D+00 - x
    s1 = q * ( b - 1.0D+00 ) / t
    s2 = q * ( 1.0D+00 - a ) / x
    d(2,0) = s1 + s2
    tail = d(2,0) * q / 2.0D+00
    x = x + q + tail

    k = 3

    do while ( error < abs ( tail / x ) .and. k <= maxk )
!
!  Find D(2,K-2).
!
      s1 = q * ( real ( k, kind = 8 ) - 2.0D+00 ) * s1 / t
      s2 = q * ( 2.0D+00 - real ( k, kind = 8 ) ) * s2 / x
      d(2,k-2) = s1 + s2
!
!  Find D(3,K-3), D(4,K-4), D(5,K-5), ... , D(K-1,1).
!
      do i = 3, k-1
        sum2 = d(2,0) * d(i-1,k-i)
        bcoeff = 1.0D+00
        do j = 1, k-i
          bcoeff = ( bcoeff * real ( k - i - j + 1, kind = 8 ) ) &
            / real ( j, kind = 8 )
          sum2 = sum2 + bcoeff * d(2,j) * d(i-1,k-i-j)
        end do
        d(i,k-i) = sum2 + d(i-1,k-i+1) / real ( i - 1, kind = 8 )
      end do
!
!  Compute D(K,0) and use it to expand the series.
!
      d(k,0) = d(2,0) * d(k-1,0) + d(k-1,1) / real ( k - 1, kind = 8 )
      tail = d(k,0) * q / real ( k, kind = 8 )
      x = x + tail
!
!  Check for divergence.
!
      if ( x <= 0.0D+00 .or. 1.0D+00 <= x )  then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  The series has diverged.'
        write ( *, '(a,g14.6)' ) '  X = ', x
        x = - 1.0D+00
        return
      end if

      k = k + 1

    end do

  end do

  return
end
