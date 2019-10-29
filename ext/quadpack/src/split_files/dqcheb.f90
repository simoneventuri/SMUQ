subroutine dqcheb ( x, fval, cheb12, cheb24 )

!*****************************************************************************80
!
!! DQCHEB computes the Chebyshev series expansion.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***purpose  this routine computes the chebyshev series expansion
!      of degrees 12 and 24 of a function using a
!      fast fourier transform method
!      f(x) = sum(k=1,..,13) (cheb12(k)*t(k-1,x)),
!      f(x) = sum(k=1,..,25) (cheb24(k)*t(k-1,x)),
!      where t(k,x) is the chebyshev polynomial of degree k.
!
!  Parameters:
!
!    on entry
!     x      - real ( kind = 8 )
!              vector of dimension 11 containing the
!              values cos(k*pi/24), k = 1, ..., 11
!
!     fval   - real ( kind = 8 )
!              vector of dimension 25 containing the
!              function values at the points
!              (b+a+(b-a)*cos(k*pi/24))/2, k = 0, ...,24,
!              where (a,b) is the approximation interval.
!              fval(1) and fval(25) are divided by two
!              (these values are destroyed at output).
!
!    on return
!     cheb12 - real ( kind = 8 )
!              vector of dimension 13 containing the
!              chebyshev coefficients for degree 12
!
!     cheb24 - real ( kind = 8 )
!              vector of dimension 25 containing the
!              chebyshev coefficients for degree 24
!
  implicit none

  real ( kind = 8 ) alam,alam1,alam2,cheb12,cheb24,fval,part1,part2, &
    part3,v,x
  integer ( kind = 4 ) i,j

  dimension cheb12(13),cheb24(25),fval(25),v(12),x(11)

  do i=1,12
    j = 26-i
    v(i) = fval(i)-fval(j)
    fval(i) = fval(i)+fval(j)
  end do

  alam1 = v(1)-v(9)
  alam2 = x(6)*(v(3)-v(7)-v(11))
  cheb12(4) = alam1+alam2
  cheb12(10) = alam1-alam2
  alam1 = v(2)-v(8)-v(10)
  alam2 = v(4)-v(6)-v(12)
  alam = x(3)*alam1+x(9)*alam2
  cheb24(4) = cheb12(4)+alam
  cheb24(22) = cheb12(4)-alam
  alam = x(9)*alam1-x(3)*alam2
  cheb24(10) = cheb12(10)+alam
  cheb24(16) = cheb12(10)-alam
  part1 = x(4)*v(5)
  part2 = x(8)*v(9)
  part3 = x(6)*v(7)
  alam1 = v(1)+part1+part2
  alam2 = x(2)*v(3)+part3+x(10)*v(11)
  cheb12(2) = alam1+alam2
  cheb12(12) = alam1-alam2
  alam = x(1)*v(2)+x(3)*v(4)+x(5)*v(6)+x(7)*v(8) &
    +x(9)*v(10)+x(11)*v(12)
  cheb24(2) = cheb12(2)+alam
  cheb24(24) = cheb12(2)-alam
  alam = x(11)*v(2)-x(9)*v(4)+x(7)*v(6)-x(5)*v(8) &
    +x(3)*v(10)-x(1)*v(12)
  cheb24(12) = cheb12(12)+alam
  cheb24(14) = cheb12(12)-alam
  alam1 = v(1)-part1+part2
  alam2 = x(10)*v(3)-part3+x(2)*v(11)
  cheb12(6) = alam1+alam2
  cheb12(8) = alam1-alam2
  alam = x(5)*v(2)-x(9)*v(4)-x(1)*v(6) &
    -x(11)*v(8)+x(3)*v(10)+x(7)*v(12)
  cheb24(6) = cheb12(6)+alam
  cheb24(20) = cheb12(6)-alam
  alam = x(7)*v(2)-x(3)*v(4)-x(11)*v(6)+x(1)*v(8) &
    -x(9)*v(10)-x(5)*v(12)
  cheb24(8) = cheb12(8)+alam
  cheb24(18) = cheb12(8)-alam

  do i=1,6
    j = 14-i
    v(i) = fval(i)-fval(j)
    fval(i) = fval(i)+fval(j)
  end do

  alam1 = v(1)+x(8)*v(5)
  alam2 = x(4)*v(3)
  cheb12(3) = alam1+alam2
  cheb12(11) = alam1-alam2
  cheb12(7) = v(1)-v(5)
  alam = x(2)*v(2)+x(6)*v(4)+x(10)*v(6)
  cheb24(3) = cheb12(3)+alam
  cheb24(23) = cheb12(3)-alam
  alam = x(6)*(v(2)-v(4)-v(6))
  cheb24(7) = cheb12(7)+alam
  cheb24(19) = cheb12(7)-alam
  alam = x(10)*v(2)-x(6)*v(4)+x(2)*v(6)
  cheb24(11) = cheb12(11)+alam
  cheb24(15) = cheb12(11)-alam

  do i=1,3
    j = 8-i
    v(i) = fval(i)-fval(j)
    fval(i) = fval(i)+fval(j)
  end do

  cheb12(5) = v(1)+x(8)*v(3)
  cheb12(9) = fval(1)-x(8)*fval(3)
  alam = x(4)*v(2)
  cheb24(5) = cheb12(5)+alam
  cheb24(21) = cheb12(5)-alam
  alam = x(8)*fval(2)-fval(4)
  cheb24(9) = cheb12(9)+alam
  cheb24(17) = cheb12(9)-alam
  cheb12(1) = fval(1)+fval(3)
  alam = fval(2)+fval(4)
  cheb24(1) = cheb12(1)+alam
  cheb24(25) = cheb12(1)-alam
  cheb12(13) = v(1)-v(3)
  cheb24(13) = cheb12(13)
  alam = 0.1D+01/0.6D+01

  do i=2,12
    cheb12(i) = cheb12(i)*alam
  end do

  alam = 0.5D+00*alam
  cheb12(1) = cheb12(1)*alam
  cheb12(13) = cheb12(13)*alam

  do i=2,24
    cheb24(i) = cheb24(i)*alam
  end do

  cheb24(1) = 0.5D+00*alam*cheb24(1)
  cheb24(25) = 0.5D+00*alam*cheb24(25)

  return
end
