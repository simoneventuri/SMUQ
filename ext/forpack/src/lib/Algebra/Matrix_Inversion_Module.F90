Module Matrix_Inversion_Module

  implicit none

  private
  public    ::  Inverse_Matrix

  contains

! Returns the inverse of a matrix calculated by finding the LU decomposition.  Depends on LAPACK.
! Downloaded at: http://fortranwiki.org/fortran/show/inv
Function Inverse_Matrix( A ) result( Ainv )

  real(8)   ,dimension(:,:)                             ,intent(in)     ::  A                               !< Matrix to be inverted
  real(8)   ,dimension(size(A,1),size(A,2))                             ::  Ainv                            !< Invserse matrix

  real(8)   ,dimension(size(A,1))                                       ::  Work                            ! Work array for LAPACK
  integer   ,dimension(size(A,1))                                       ::  ipiv                            ! Pivot indices
  integer                                                               ::  N
  integer                                                               ::  Info

! External procedures defined in LAPACK
  external DGETRF
  external DGETRI

  Ainv    =     A                                                                                               ! Store A in Ainv to prevent it from being overwritten by LAPACK
  N       =     size(A,1)

! ==============================================================================================================
!    COMPUTING AN LU FACTORIZATION USING PARTIAL PIVOTING
! ==============================================================================================================
! This section computes an LU factorization of a general M-by-N matrix A using partial pivoting with row
! interchanges. It uses the 'DGETRF' procedure from Lapack.
! ==============================================================================================================
  call DGETRF( n, n, Ainv, n, ipiv, Info )
  if (Info /= 0) then
     stop 'Matrix is numerically singular!'
  end if
! ==============================================================================================================


! ==============================================================================================================
!    COMPUTING THE INVERSE OF A MATRIX USING THE LU FACTORIZATION
! ==============================================================================================================
! This section computes the inverse of a matrix using the LU factorization computed by DGETRF.
! It uses the 'DGETRI' procedure from Lapack.
! ==============================================================================================================
  call DGETRI(n, Ainv, n, ipiv, Work, n, Info)
  if (Info /= 0) then
     stop 'Matrix inversion failed!'
  end if
! ==============================================================================================================

End Function

End Module