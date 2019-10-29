Module Numerics_Matrix

  implicit none

  private
  public  ::  inv_matrix

  integer ,parameter :: hpc_4 = 4
  integer ,parameter :: hpc_8 = 8

  contains

!---------------------------------------------------!
!> This Subroutine inverts a general non-symmetric matrix. The LU
!! decomposition algorithm is used to speed up the calculation
Subroutine inv_matrix(n, a, b)

  integer(kind=hpc_4)               ,intent(in)     :: n
  real(kind=hpc_8)  ,dimension(:,:) ,intent(inout)  :: a
  real(kind=hpc_8)  ,dimension(:,:) ,intent(out)    :: b

  integer(kind=hpc_4)                             :: i, j
  integer(kind=hpc_4), dimension(n)               :: indx
  real(kind=hpc_8)                                :: d

  ! Initialize b matrix with identity matrix
  do j = 1,n

     do i = 1,j - 1
        b(i,j) = 0.d0
     enddo

     b(j,j) = 1.d0

     do i = j + 1,n
         b(i,j) = 0.d0
     enddo

  enddo

  ! LU decomposition
  call LU_dcmp(a, n, indx, d)

  ! Columns of inverse matrix b = inv(a)
  do i = 1,n
     call LU_solver(a, n, indx, b(1:n,i))
  enddo

End Subroutine

!---------------------------------------------------!
!> This subrouine solves a linear symmetric transport system A*x = b by a direct method (Cholesky decomposition).
!! The matrix A is stored symmetric form (i.e. A(i,j) => A(ij) with ij = n*(i - 1) - i*(i - 1)/2 + j for j >= i).
!! The Subroutines used are taken from the EGLIB library (A. Earn and V. Giovangili)
Subroutine solve_sym_transp_systD(n, A, b, x)

  integer(kind=hpc_4), intent(in)               :: n
  real(kind=hpc_8), dimension(:), intent(inout) :: A
  real(kind=hpc_8), dimension(:), intent(in)    :: b
  real(kind=hpc_8), dimension(:), intent(out)   :: x

  integer(kind=hpc_4)                           :: ier
  real(kind=hpc_8), dimension(n)                :: dum_vec

  call sym_LUdcmp(n, A, dum_vec, ier)
  x = b
  call sym_LUsolver(n, A, x)

End Subroutine

!---------------------------------------------------!
!> This Subroutine peforms an LU decomposition of symmetric positive definite matrix (from EGLIB)
Subroutine sym_LUdcmp(n, A, w, ier)

   integer(kind=hpc_4), intent(in)               :: n
   real(kind=hpc_8), dimension(:), intent(inout) :: A
   real(kind=hpc_8), dimension(:), intent(out)   :: w
   integer(kind=hpc_4), intent(out)              :: ier

   integer(kind=hpc_4)                           :: i, j, k
   integer(kind=hpc_4)                           :: ij, jj, kk, kj, ik
   integer(kind=hpc_4)                           :: km1, kp1
   real(kind=hpc_8)                              :: fac

   ier = 0
   do  k = 1,n
       km1 = k - 1
       kp1 = k + 1
       do j = 1,km1
          jj = n*(j-1) - (j*(j-1))/2 + j
          kj = n*(j-1) - (j*(j-1))/2 + k
          w(j)= A(jj)*A(kj)
       enddo
       kk = n*(k - 1) - (k*(k - 1))/2 + k
       do j = 1,km1
          kj = n*(j - 1) - (j*(j - 1))/2 + k
          A(kk) = A(kk) - w(j)*A(kj)
       enddo
       if (A(kk).eq.0.d0) then
           write(6,'(''Singular matrix in sym_LUdcmp'')' )
           ier = k
           return
       endif
       fac = 1.d0/A(kk)
       do j = 1, km1
          do i = kp1, N
             ik = n*(k - 1) - (k*(k - 1))/2 + i
             ij = n*(j  -1) - (j*(j - 1))/2 + i
             A(ik) = A(ik) - A(ij)*w(j)
          enddo
       enddo
       do i = kp1,n
          ik = n*(k - 1) - (k*(k - 1))/2 + i
          A(ik) = A(ik)/A(kk)
      enddo
   enddo

End Subroutine

!---------------------------------------------------!
!> This Subroutine peforms a backward substitution given an LU decomposition of a symmetric positive definite matrix (from EGLIB)
pure Subroutine sym_LUsolver(n, A, b)

  integer(kind=hpc_4), intent(in)               :: n
  real(kind=hpc_8), dimension(:), intent(in)    :: A
  real(kind=hpc_8), dimension(:), intent(inout) :: b

  integer(kind=hpc_4)                           :: j, k
  integer(kind=hpc_4)                           :: jj, jb, jk, kj
  integer(kind=hpc_4)                           :: jm1, jp1, nm1
  real(kind=hpc_8)                              :: fac

  nm1 = n - 1
  do J = 1,nm1
     jp1 = J + 1
     fac = -b(j)
     do k = jp1,n
         kj = n*(j - 1) - (j*(j - 1))/2 + k
         b(K) = b(k) + A(kj)*fac
     enddo
  enddo

  do j = 1,n
     jj = n*(j - 1) - (j*(j - 1))/2 + j
     b(j) = b(j)/A(jj)
  enddo

  do jb = 1,nm1
     j = n + 1 - jb
     jm1 = j - 1
     fac = -B(j)
     do k = 1,jm1
        jk = n*(k - 1) - (k*(k - 1))/2 + j
        b(k) = b(k) + A(jk)*fac
     enddo
  enddo

End Subroutine

!--------------------------------------------------------!
!> This Subroutine performs the LU decomposition of the a matrix by means of the Crout's algorithm.
!! In order to save memory the LU decomposition is stored in the original matrix a.
Subroutine LU_dcmp(a, np, indx, d)

  integer(kind=hpc_4), intent(in)                 :: np
  real(kind=hpc_8), intent(out)                   :: d
  real(kind=hpc_8), dimension(:,:), intent(inout) :: a
  integer(kind=hpc_4), dimension(:), intent(out)  :: indx

  integer(kind=hpc_4)                             :: i, j, k, i_max
  real(kind=hpc_8)                                :: toll, aamax, sum, dum
  real(kind=hpc_8), dimension(size(a,1))          :: vv

  ! Useful parameters
  d = 1.d0 ; toll = 1.d-20

  ! Loop over the rows to get the implicit scaling information
  do i = 1,np
     aamax = 0.d0
     do j = 1,np
        if (abs(a(i,j)).gt.aamax) then
           aamax = abs(a(i,j))
        endif
     enddo
     if (aamax.EQ.0.d0) then
        print*
        write(*,"(a)")'Sinlgular matrix in LU decomposition...'
        print*,a
        print*
        stop
      endif
      vv(i) = 1.d0/aamax     ! scaling is saved
  enddo

  ! Loop over columns for Crout's method.
  do j = 1,np
     do i = 1,j - 1
        sum = a(i,j)
        do k = 1,i - 1
           sum = sum - a(i,k)*a(k,j)
        enddo
        a(i,j) = sum
     enddo
     aamax = 0.d0
     do i = j,np
        sum = a(i,j)
        do k = 1,j - 1
           sum = sum - a(i,k)*a(k,j)
        enddo
        a(i,j) = sum
        dum = vv(i)*abs(sum)
        if (dum.ge.aamax) then
           i_max = i
           aamax = dum
        endif
     enddo
     if (j.ne.i_max) then
        do k = 1,np
           dum = a(i_max,k)
           a(i_max,k) = a(j,k)
           a(j,k) = dum
        enddo
        d = - d
        vv(i_max) = vv(j)
     endif
     indx(j) = i_max
     if (a(j,j).EQ.0.d0) then
        a(j,j)= toll
     endif
     if (j.ne.np) then
        dum = 1.d0/a(j,j)
        do i = j + 1,np
           a(i,j) = a(i,j)*dum
        enddo
     endif
  enddo

End Subroutine

!-----------------------------------------------------------------------
!> This Subroutine computes the solution of the algebraic system of
!! equation once the LU decomposition of the a matrix has been
!! perfomed. The solution is found by means backward and forward
!! substitution on matrices L and U, respectively
pure Subroutine LU_solver(a, np, indx, b)

  integer(kind=hpc_4), intent(in)               :: np
  integer(kind=hpc_4), dimension(:), intent(in) :: indx
  real(kind=hpc_8), dimension(:,:), intent(in)  :: a
  real(kind=hpc_8), dimension(:), intent(out)   :: b

  integer(kind=hpc_4)                           :: i, j, ii, ll
  real(kind=hpc_8)                              :: sum

  ! Data inizialization
  sum = 0.d0

  ! Forward substitution
  ii = 0
  do i = 1,np
     ll = indx(i)
     sum = b(ll)
     b(ll) = b(i)
     if (ii.ne.0) then
        do j = ii,i - 1
           sum = sum - a(i,j)*b(j)
        enddo
     elseif (sum.ne.0.d0) then
        ii = i
     endif
     b(i) = sum
  enddo

  ! Backward substitution
  do i = np,1,-1
     sum = b(i)
     do j = i + 1,np
        sum = sum - a(i,j)*b(j)
     enddo
     b(i) = sum/a(i,i)
  enddo

End Subroutine

End module