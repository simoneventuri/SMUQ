Module Tools_Module

  use Kind_Parameters      ,only:  rkp

  implicit none

  private

  public  ::  Sort, INTERP1, UniVal, NonZero, Mean, MinStep, Median

  interface SORT
    Module Procedure Sort_Real_1D, Sort_Real_2D
  end interface SORT

  Interface INTERP1
    Module Procedure INTERP1_0D, INTERP1_1D
  End Interface

  Interface UniVal
    Module Procedure UniVal_R, UniVal_I
  End Interface

  Interface NonZero
    Module Procedure NonZero_R, NonZero_I
  End Interface

  contains

Pure Function Mean (Array)
  implicit none
  real(rkp)  ,dimension(:)  ,intent(in)  ::  Array
  real(rkp)          ::  Mean
  integer          ::  i
  Mean  =  sum(Array) / max(1,size(array))
End Function

Pure Function Median (Array)
  implicit none
  real(rkp)  ,dimension(:)  ,intent(in)  ::  Array
  real(rkp)          ::  Median
  real(rkp)  ,dimension( size(Array) )  ::  A
  integer          ::  N
  A  =  Sort (Array)                        ! Sorting elements in the input array
  N  =  size(A,1)
  if  ( mod(N,2) == 0 )  then
    Median  =  0.5_rkp * ( A(N/2+1) + A(N/2) )
  else
    Median  =  A(N/2+1)
  end if
End Function

! -------------------------------------------------------------------------------------------------
!> @brief  Routine that sorts a 2D-array in ascending order
!!
!> @details  This routine sorts a one dimensional array in ascending order
!>     First, an pivot sort is performed. This type of sorting is all the more efficient that the array dimension is large.
!>     However, for very small array dimension, the pivot sort can become very ineficient.
!>     Therefore, a insertion sort is performed after the Pivot sort in order to performed the final sorting on the remaiing unsorted values.
!!
!> @author  Bruno LOPEZ, Bruno.Lopez@ist.utl.pt
!!
!> @version  0.1
!!
!> @date  01/04/00 - Michel Olagnon  - Creation of the Matlab function
!> @date  15/10/11 - Bruno LOPEZ    - Adaptation of routine for present purpose
!> @date  22/10/11 - Bruno LOPEZ    - Moving the routine in the 'Tools_Module' module
!>
!!
!> @remark  This Sort_Real_1D procedure has a 2D clone called Sort_Real_2D. Both procedures are invoked using the SORT generic name
!> @remark  This program unit was originally written by Michel Olagnon in April 2000 for the ORDERPACK 2.0 package.
!>     The ORDERPACK package contains a set of Fortran90 routines for "Unconditional, Unique, and Partial Ranking, Sorting, and Permutation".
!>     The present module has been adapted from the m_refsor module in the m_refsor.f90 file.
!!
!<--------------------------------------------------------------------------------------------------
Pure Function Sort_Real_1D (A)  result (B)
  implicit none
  real(kind=rkp)  ,dimension(:)  ,intent(in)  ::  A            ! Array to be sorted in ascending order
  real(kind=rkp)  ,dimension(size(A))    ::  B            ! Sorted array
  B  =  Sort_Real_1D_Pivot  ( A, 1, size(A) )              ! Pivot sorting
  B  =  Sort_Real_1D_Insert  ( B )                  ! Insertion sorting
End Function

! -------------------------------------------------------------------------------------------------
!> @brief  Routine that sorts a 2D-array in ascending order
!!
!> @details  This routine sorts a two dimensional array in ascending order
!>     First, an pivot sort is performed. This type of sorting is all the more efficient that the array dimension is large.
!>     However, for very small array dimension, the pivot sort can become very ineficient.
!>     Therefore, a insertion sort is performed after the Pivot sort in order to performed the final sorting on the remaiing unsorted values.
!!
!> @author  Bruno LOPEZ, Bruno.Lopez@ist.utl.pt
!!
!> @version  0.1
!!
!> @date  01/04/00 - Michel Olagnon  - Creation of the Matlab function
!> @date  15/10/11 - Bruno LOPEZ    - Adaptation of routine for present purpose
!> @date  22/10/11 - Bruno LOPEZ    - Moving the routine in the 'Tools_Module' module
!>
!!
!> @remark  This Sort_Real_2D procedure has a 1D clone called Sort_Real_1D. Both procedures are invoked using the SORT generic name
!> @remark  This program unit was originally written by Michel Olagnon in April 2000 for the ORDERPACK 2.0 package.
!>     The ORDERPACK package contains a set of Fortran90 routines for "Unconditional, Unique, and Partial Ranking, Sorting, and Permutation".
!>     The present module has been adapted from the m_refsor module in the m_refsor.f90 file.
!!
!<--------------------------------------------------------------------------------------------------
Pure Function Sort_Real_2D (A)  result (B)
  implicit none
  real(kind=rkp)  ,dimension(:,:)  ,intent(in)  ::  A            ! Array to be sorted in ascending order
  real(kind=rkp)  ,dimension(size(A,1),size(A,2))  ::  B            ! Sorted array
  B  =  Sort_Real_2D_Pivot  ( A, 1, size(A,2) )              ! Pivot sorting
  B  =  Sort_Real_2D_Insert  ( B )                  ! Insertion sorting
End Function

! -------------------------------------------------------------------------------------------------
!> @brief  Routine that sorts a 1D-array in ascending order using a Pivot sorting method
!!
!> @details  This routine sorts a 1D-array in ascending order using a Pivot sorting method.
!>     If the array dimension is too small, then the routine is exit leaving an unsorted array.
!>     This array will then be sorted by the insertion sorting routine.
!>     The Pivot sorting process is divided in 3 step.
!>     First, the routine chooses a "pivot" in the array elements.
!>     Then, all array elements are explored from both ends, looking for a value greater than pivot with the increasing index,
!>     for a value smaller or equal than pivot with the decreasing index, and swapping them when it has found one of each.
!>     Finally, the array is subdivided in 2 subsets, {values<=pivot} and {values>pivot},
!>     and the routine is then called recursively to sort each subset.
!>     When the size of the subarray is small enough, one uses an insertion sort that is faster for very small sets.
!!
!> @author  Bruno LOPEZ, Bruno.Lopez@ist.utl.pt
!!
!> @version  0.1
!!
!> @date  01/04/00 - Michel Olagnon  - Creation of the Matlab function
!> @date  15/10/11 - Bruno LOPEZ    - Adaptation of routine for present purpose
!> @date  22/10/11 - Bruno LOPEZ    - Moving the routine in the 'Tools_Module' module
!!
!> @remark  This Sort_Real_1D_Pivot procedure has a 2D clone called Sort_Real_2D_Pivot.
!> @remark  This program unit was originally written by Michel Olagnon in April 2000 for the ORDERPACK 2.0 package.
!>     The ORDERPACK package contains a set of Fortran90 routines for "Unconditional, Unique, and Partial Ranking, Sorting, and Permutation".
!>     The present module has been adapted from the m_refsor module in the m_refsor.f90 file.
!!
!> @param[in]    ideb
!> @param[in]    ifin
!> @param[in]    A
!<--------------------------------------------------------------------------------------------------
Recursive Pure Function Sort_Real_1D_Pivot ( A, ideb, ifin )  result (B)

  implicit none

  real(kind=rkp)  ,dimension (:)    ,intent(in)  ::  A          ! Array to be sorted
  integer          ,intent(in)  ::  ideb          ! Index of initial point from which the sort is performed
  integer          ,intent(in)  ::  ifin          ! Index of final point to which the sort is performed
  real(kind=rkp)  ,dimension(size(A))      ::  B          ! Sorted array

  integer  ,parameter  ::  Nins=16                  ! Maximum number of value in the array for insertion sort
  integer      ::  imil                  ! Index of middle point int he sorting range
  integer      ::  icrs                  ! Index
  integer      ::  idcr                  ! Index
  real(kind=rkp)    ::  B_piv                  ! Pivot value
  real(kind=rkp)    ::  B_wrk                  ! Working value

  B  =  A                        ! Storing the input array into the output array
  if  ((ifin - ideb) < Nins)  return                    ! If too few value for pivot sorting, leaving array unsorted and quitting the routine (the final insertion sort will do the job)

! *****************************************************************
! *     CHOSING PIVOT, MEDIAN OF 1ST, LAST, AND MIDDLE VALUES     *
! *****************************************************************
  imil  =  (ideb + ifin) / 2                    ! Index of middle point
  if  (B(imil) < B(ideb))  then                    ! If middle element smaller than fist element
    B_wrk  =  B(ideb)                      ! Saving first element value in working array
    B(ideb)  =  B(imil)                      ! Moving middle element to first position
    B(imil)  =  B_wrk                      ! Moving fist element to middle position
  end if                          ! End if case Amid<Adeb
  if  (B(imil) > B(ifin))  then                    ! If middle element greater than last element
    B_wrk  =  B(ifin)                      ! Saving last element value in working array
    B(ifin)  =  B(imil)                      ! Moving middle element to last position
    B(imil)  =  B_wrk                      ! Moving last element to middle position
    if  (B(imil) < B(ideb))  then                    ! If middle element smaller than fist element
      B_wrk  =  B(ideb)                      ! Saving first element value in working array
      B(ideb)  =  B(imil)                      ! Moving middle element to first position
      B(imil)  =  B_wrk                      ! Moving fist element to middle position
    end if                          ! End if case Amid<Adeb
  end if                          ! End if case Amid>Afin
  B_piv    =  B(imil)                      ! Affecting pivot value to middle element

! *********************************************************************************
! *     PUTTING VALUES > THAN PIVOT IN THE END AND VALUES >= ET THE BEGINNING     *
! *********************************************************************************
  icrs    =  ideb                      ! Initialisation of the index of the last element <= pivot is icrs-1
  idcr    =  ifin                      ! Initialisation of the index of the first element > pivot is idcr
  PivotLoop: do                          ! Pivot loop
    do                            ! Inner loop to find the index of last element <= pivot
      icrs  =  icrs + 1                    ! Index of the last element <= pivot
      if  (icrs   >= idcr  )  exit PivotLoop                ! Exit pivot loop if icrs >= idcr
      if  (B(icrs)>  B_piv )  exit                  ! Exit inner loop if B(icrs) >  B_piv
    end do                          ! End inner loop
    do                            ! Inner loop to find the index of the first element > pivot
      if  (B(idcr)<=B_piv )  exit                  ! Exit inner loop if B(idcr) <=  B_piv
      idcr  =  idcr - 1                    ! Index of the first element > pivot (last value < pivot is always icrs-1)
      if  (icrs   >= idcr )  exit PivotLoop                ! Exit pivot loop if icrs >= idcr
    end do                          ! End inner loop
    B_wrk  =  B(idcr)                      ! Copie B(idcr)
    B(idcr)  =  B(icrs)                      ! Exchange B(idcr)
    B(icrs)  =  B_wrk                      !  and B(icrs) values
  end do PivotLoop                        ! End pivot loop

! **************************************
! *     SORTING EACH SUB-INTERVALS     *
! **************************************
  B  =  Sort_Real_1D_Pivot (B, ideb, icrs-1)                ! Sorting the lower sub-interval
  B  =  Sort_Real_1D_Pivot (B, idcr, ifin)                ! Sorting the upper sub-interval

End Function

! -------------------------------------------------------------------------------------------------
!> @brief  Routine that sorts a 2D-array in ascending order using a Pivot sorting method
!!
!> @details  This routine sorts a 2D-array in ascending order using a Pivot sorting method.
!>     If the array dimension is too small, then the routine is exit leaving an unsorted array.
!>     This array will then be sorted by the insertion sorting routine.
!>     The Pivot sorting process is divided in 3 step.
!>     First, the routine chooses a "pivot" in the array elements.
!>     Then, all array elements are explored from both ends, looking for a value greater than pivot with the increasing index,
!>     for a value smaller or equal than pivot with the decreasing index, and swapping them when it has found one of each.
!>     Finally, the array is subdivided in 2 subsets, {values<=pivot} and {values>pivot},
!>     and the routine is then called recursively to sort each subset.
!>     When the size of the subarray is small enough, one uses an insertion sort that is faster for very small sets.
!!
!> @author  Bruno LOPEZ, Bruno.Lopez@ist.utl.pt
!!
!> @version  0.1
!!
!> @date  01/04/00 - Michel Olagnon  - Creation of the Matlab function
!> @date  15/10/11 - Bruno LOPEZ    - Adaptation of routine for present purpose
!> @date  22/10/11 - Bruno LOPEZ    - Moving the routine in the 'Tools_Module' module
!!
!> @remark  This Sort_Real_2D_Pivot procedure has a 1D clone called Sort_Real_1D_Pivot.
!> @remark  This program unit was originally written by Michel Olagnon in April 2000 for the ORDERPACK 2.0 package.
!>     The ORDERPACK package contains a set of Fortran90 routines for "Unconditional, Unique, and Partial Ranking, Sorting, and Permutation".
!>     The present module has been adapted from the m_refsor module in the m_refsor.f90 file.
!!
!> @param[in]    ideb
!> @param[in]    ifin
!> @param[in]    A
!<--------------------------------------------------------------------------------------------------
Recursive Pure Function Sort_Real_2D_Pivot  ( A, ideb, ifin )  result (B)

  implicit none

  real(kind=rkp)  ,dimension ( :, : )  ,intent(in)  ::  A          ! Array to be sorted
  integer          ,intent(in)  ::  ideb          ! Index of initial point from which the sort is performed
  integer          ,intent(in)  ::  ifin          ! Index of final point to which the sort is performed
  real(kind=rkp)  ,dimension ( size(A,1), size(A,2) )  ::  B          ! Sorted array

  integer    ,parameter        ::  Nins=16          ! Maximum number of value in the array for insertion sort
  integer              ::  imil          ! Index of middle point int he sorting range
  integer              ::  icrs
  integer              ::  idcr
  integer    ,parameter        ::  iSorDim=1        ! Index of the dimension used for sorting
  real(kind=rkp)            ::  B_piv
  real(kind=rkp)  ,dimension( size(A,1) )      ::  B_wrk

  B  =  A                        ! Storing the input array into the output array
  if  ((ifin - ideb) < Nins)  return                    ! If too few value for pivot sorting, leaving array unsorted and quitting the routine (the final insertion sort will do the job)

! *****************************************************************
! *     CHOSING PIVOT, MEDIAN OF 1ST, LAST, AND MIDDLE VALUES     *
! *****************************************************************
  imil  =  (ideb + ifin) / 2                    ! Index of middle point
  if  (B(iSorDim,imil) < B(iSorDim,ideb))  then                ! If middle element smaller than fist element
    B_wrk(:)  =  B(:,ideb)                    ! Saving first element value in working array
    B(:,ideb)  =  B(:,imil)                    ! Moving middle element to first position
    B(:,imil)  =  B_wrk(:)                    ! Moving fist element to middle position
  end if                          ! End if case Amid<Adeb
  if  (B(iSorDim,imil) > B(iSorDim,ifin))  then                ! If middle element greater than last element
    B_wrk(:)  =  B(:,ifin)                    ! Saving last element value in working array
    B(:,ifin)  =  B(:,imil)                    ! Moving middle element to last position
    B(:,imil)  =  B_wrk(:)                    ! Moving last element to middle position
    if  (B(iSorDim,imil) < B(iSorDim,ideb))  then                ! If middle element smaller than fist element
      B_wrk(:)  =  B(:,ideb)                    ! Saving first element value in working array
      B(:,ideb)  =  B(:,imil)                    ! Moving middle element to first position
      B(:,imil)  =  B_wrk(:)                    ! Moving fist element to middle position
    end if                          ! End if case Amid<Adeb
  end if                          ! End if case Amid>Afin
  B_piv    =  B(iSorDim,imil)                    ! Affecting pivot value to middle element

! *********************************************************************************
! *     PUTTING VALUES > THAN PIVOT IN THE END AND VALUES >= ET THE BEGINNING     *
! *********************************************************************************
  icrs    =  ideb                      ! Initialisation of the index of the last element <= pivot is icrs-1
  idcr    =  ifin                      ! Initialisation of the index of the first element > pivot is idcr
  PivotLoop: do                          ! Pivot loop
    do                            ! Inner loop to find the index of last element <= pivot
      icrs  =  icrs + 1                    ! Index of the last element <= pivot
      if  (icrs   >= idcr  )  exit PivotLoop                ! Exit pivot loop if icrs >= idcr
      if  (B(iSorDim,icrs)>  B_piv )  exit                ! Exit inner loop if B(:,icrs) >  B_piv
    end do                          ! End inner loop
    do                            ! Inner loop to find the index of the first element > pivot
      if  (B(iSorDim,idcr)<=B_piv )  exit                ! Exit inner loop if B(:,idcr) <=  B_piv
      idcr  =  idcr - 1                    ! Index of the first element > pivot (last value < pivot is always icrs-1)
      if  (icrs   >= idcr )  exit PivotLoop                ! Exit pivot loop if icrs >= idcr
    end do                          ! End inner loop
    B_wrk(:)  =  B(:,idcr)                    ! Copie B(:,idcr)
    B(:,idcr)  =  B(:,icrs)                    ! Exchange B(:,idcr)
    B(:,icrs)  =  B_wrk(:)                    ! and B(:,icrs) values
  end do PivotLoop                        ! End pivot loop

! **************************************
! *     SORTING EACH SUB-INTERVALS     *
! **************************************
  B  =  Sort_Real_2D_Pivot (B, ideb, icrs-1)                ! Sorting the lower sub-interval
  B  =  Sort_Real_2D_Pivot (B, idcr, ifin)                ! Sorting the upper sub-interval

End Function

! -------------------------------------------------------------------------------------------------
!> @brief  Routine that sorts a 2D-array in ascending order using a Insertion sorting method
!!
!> @details  This routine sorts a one dimensional array in ascending order using a Insertion sorting method.
!>     This sorting method is very efficient for very small datasets.
!>     Therefore, this routine is only called once the dataset to be sorted as been sufficiently reduced by the Pivot sorting method.
!!
!> @author  Bruno LOPEZ, Bruno.Lopez@ist.utl.pt
!!
!> @version  0.1
!!
!> @date  01/04/00 - Michel Olagnon  - Creation of the Matlab function
!> @date  15/10/11 - Bruno LOPEZ    - Adaptation of routine for present purpose
!> @date  22/10/11 - Bruno LOPEZ    - Moving the routine in the 'Tools_Module' module
!!
!> @remark  This Sort_Real_1D_Insert procedure has a 2D clone called Sort_Real_2D_Insert.
!> @remark  This program unit was originally written by Michel Olagnon in April 2000 for the ORDERPACK 2.0 package.
!>     The ORDERPACK package contains a set of Fortran90 routines for "Unconditional, Unique, and Partial Ranking, Sorting, and Permutation".
!>     The present module has been adapted from the m_refsor module in the m_refsor.f90 file.
!!
!> @param[in]    ideb
!> @param[in]    ifin
!> @param[in]    A
!<--------------------------------------------------------------------------------------------------
Pure Function Sort_Real_1D_Insert (A)  result (B)
  implicit none
  real(kind=rkp)  ,dimension (:)    ,intent(in)  ::  A          ! Array to be sorted
  real(kind=rkp)  ,dimension ( size(A,1) )    ::  B          ! Sorted array
  integer              ::  icrs          ! Index
  integer              ::  idcr          ! Index
  real(kind=rkp)            ::  B_wrk          ! Working value
  B  =  A                        ! Storing the input array into the output array
  do icrs = 2,size(B)
     B_wrk  =  B(icrs)
     if  (B_wrk >= B(icrs-1))  cycle
     B(icrs)  =  B(icrs-1)
     Do idcr = icrs-2, 1, - 1
        if  (B_wrk >= B(idcr))  exit
        B(idcr+1)  =  B(idcr)
     end do
     B(idcr+1)    =  B_wrk
  end do
End Function

! -------------------------------------------------------------------------------------------------
!> @brief  Routine that sorts a 1D-array in ascending order using a Insertion sorting method
!!
!> @details  This routine sorts a one dimensional array in ascending order using a Insertion sorting method.
!>     This sorting method is very efficient for very small datasets.
!>     Therefore, this routine is only called once the dataset to be sorted as been sufficiently reduced by the Pivot sorting method.
!!
!> @author  Bruno LOPEZ, Bruno.Lopez@ist.utl.pt
!!
!> @version  0.1
!!
!> @date  01/04/00 - Michel Olagnon  - Creation of the Matlab function
!> @date  15/10/11 - Bruno LOPEZ    - Adaptation of routine for present purpose
!> @date  22/10/11 - Bruno LOPEZ    - Moving the routine in the 'Tools_Module' module
!!
!> @remark  This Sort_Real_2D_Insert procedure has a 1D clone called Sort_Real_1D_Insert.
!> @remark  This program unit was originally written by Michel Olagnon in April 2000 for the ORDERPACK 2.0 package.
!>     The ORDERPACK package contains a set of Fortran90 routines for "Unconditional, Unique, and Partial Ranking, Sorting, and Permutation".
!>     The present module has been adapted from the m_refsor module in the m_refsor.f90 file.
!!
!> @param[in]    ideb
!> @param[in]    ifin
!> @param[in]    A
!<--------------------------------------------------------------------------------------------------
Pure Function Sort_Real_2D_Insert (A)  result (B)
  implicit none
  real(kind=rkp)  ,dimension( :, : )  ,intent(in)  ::  A          ! Array to be sorted
  real(kind=rkp)  ,dimension ( size(A,1), size(A,2) )  ::  B          ! Sorted array
  integer              ::  icrs          ! Index
  integer              ::  idcr          ! Index
  real(kind=rkp)  ,dimension( size(A,1) )      ::  B_wrk          ! Working value
  integer    ,parameter        ::  iSorDim=1        ! Index of the dimension used for sorting
  B  =  A                        ! Storing the input array into the output array
  do icrs = 2,size(A,2)
     B_wrk(:)  =  B(:,icrs)
     if  (B_wrk(iSorDim) >= B(iSorDim,icrs-1))  cycle
     B(:,icrs)  =  B(:,icrs-1)
     Do idcr = icrs-2, 1, - 1
        if  (B_wrk(iSorDim) >= B(iSorDim,idcr))  exit
        B(:,idcr+1)  =  B(:,idcr)
     end do
     B(:,idcr+1)  =  B_wrk(:)
  end do
End Function

Function INTERP1_0D  (  Xinp, Yinp, Xout  )
  implicit none
  real(rkp)  ,dimension( : )      ,intent(in)  ::  Xinp          !
  real(rkp)  ,dimension( : )      ,intent(in)  ::  Yinp          !
  real(rkp)          ,intent(in)  ::  Xout          !
  real(rkp)              ::  INTERP1_0D        !
  integer              ::  iinp          !< Index of elements in the input dataset
  integer              ::  Ninp          !< Number of elements in the input dataset
  Ninp  =  size(Xinp)                      ! Number of elements in the input dataset
  if    (Xout.le.Xinp(1))    then                ! If the abscisse of the considered output point is smaller than the lower abscisse of the input points: Imposition to lower bound
     INTERP1_0D    =  Yinp(1)                    ! Imposition to lower bound
  else if  (Xout.ge.Xinp(Ninp))    then                ! If the abscisse of the considered output point is greater than the upper abscisse of the input points: Imposition to upper bound
     INTERP1_0D    =  Yinp(Ninp)                  ! Imposition to upper bound
  else                            ! If the required point value is within the lower and upper bound: Linear interpolation
    do iinp = 1,Ninp                        ! Loop on all input elements
      if  (Xout.le.Xinp(iinp))    then                ! If the value to be interpolated is lower than the considere input data
        INTERP1_0D  =  Yinp(iinp-1)  &                 ! Interpolation
      +  (Xout-Xinp(iinp-1)) * (Yinp(iinp)-Yinp(iinp-1)) / (Xinp(iinp)-Xinp(iinp-1))  ! using linear approximation
        return                          ! Quitting hte function
      end if                          ! End of if case
    end do                          ! End of do loop
  end if                          ! End of if case
End Function INTERP1_0D

! I_nu  =  interp1  ( nu_line, I_line, nu_temp )      ! Line interpolation over the relevant points

Function INTERP1_1D  (  Xinp, Yinp, Xout  )
  implicit none
  real(rkp)  ,dimension( : )      ,intent(in)  ::  Xinp          !
  real(rkp)  ,dimension( : )      ,intent(in)  ::  Yinp          !
  real(rkp)  ,dimension( : )      ,intent(in)  ::  Xout          !
  real(rkp)  ,dimension( size(Xout) )      ::  INTERP1_1D        !
  integer              ::  iinp          !< Index of elements in the input dataset
  integer              ::  iout          !< Index of elements in the output dataset
  integer              ::  Ninp          !< Number of elements in the input dataset
  integer              ::  Nout          !< Number of elements in the output dataset
  Ninp  =  size(Xinp)                      ! Number of elements in the input dataset
  Nout  =  size(Xout)                      ! Number of elements in the output dataset
  Output: do iout = 1,Nout                      ! Loop on all output elements
!     write(6,"(3x,'INTERP1_1D: iout=',i6,3x,'Xout=',e22.15)") iout, Xout(iout)
    if    (Xout(iout).le.Xinp(1))    then                ! If the abscisse of the considered output point is smaller than the lower abscisse of the input points: Imposition to lower bound
       INTERP1_1D(iout)  =  Yinp(1)                    ! Imposition to lower bound
    else if  (Xout(iout).ge.Xinp(Ninp))  then                ! If the abscisse of the considered output point is greater than the upper abscisse of the input points: Imposition to upper bound
       INTERP1_1D(iout)  =  Yinp(Ninp)                  ! Imposition to upper bound
    else                          ! If the required point value is within the lower and upper bound: Linear interpolation
      do iinp = 1,Ninp                        ! Loop on all input elements
        if  (Xout(iout).le.Xinp(iinp))  then                ! If the value to be interpolated is lower than the considere input data
          INTERP1_1D(iout)  =  Yinp(iinp-1)  &                ! Interpolation
        +  (Xout(iout)-Xinp(iinp-1)) * (Yinp(iinp)-Yinp(iinp-1)) / (Xinp(iinp)-Xinp(iinp-1)) ! using linear approximation
          cycle Output                        ! Quitting hte function
        end if                          ! End of if case
      end do                          ! End of do loop
    end if                          ! End of if case
  end do Output                          ! End of loop on output values
End Function INTERP1_1D

! -------------------------------------------------------------------------------------------------
!> @brief  Routine that reduces a real 1D-array to a 1D-array with only unique elements
!!
!> @details  This routine reduces a one dimensional array to a vector containing only real unique elements.
!>     The procedure is as follow:
!>      - The array elements are considered one by one from the first to the before-last element
!>      - Each array element are compared to all the following array elements
!>      - If non-identical values are found then the next array element is considered
!>      - If identical values are found then
!>         - the second values is removed from the vector
!>         - a new vector is created excluding the redundant value
!>         - the element is treated again in order to find other eventual redundant values
!>     This processus is reapeted until all elements are treated and that all redundant element have been excluded from the output vector.
!!
!> @author  Bruno LOPEZ, Bruno.Lopez@ist.utl.pt
!!
!> @version  0.1
!!
!> @date  24/10/11 - Bruno LOPEZ    - Creation of the Fortran Subroutine
!!
!> @remark  This procedure has a clone called 'UniVal_I' whose input and output arguments are integer arrays. Both procedures are invoked using the 'UniVal' generic name
!> @remark  This program unit was originally written to remplace the Matlab unique function.
!!
!> @param[in]    A  Array to be reduced with only unique elements
!> @param[in]    B  Array reduced with only unique elements
!<--------------------------------------------------------------------------------------------------
subroutine UniVal_R  (  A, B  )

  implicit none

  real(rkp)  ,dimension( : )      ,intent(in)  ::  A          ! Array to be reduced with only unique elements
  real(rkp)  ,dimension( : )  ,allocatable  ,intent(out)  ::  B          ! Array reduced with only unique elements
  integer              ::  Nelt          !< Number of elements of the array
  integer              ::  i          !< Index of the array element to be compared
  integer              ::  j          !< Index of the array element being compared
  real(rkp)  ,dimension( : )  ,allocatable      ::  B_temp          !< Temporary array

  if  (allocated(B))  deallocate(B)                    ! Deallocated the output array if already allocated
  allocate  (  B(size(A))  )                  ! Allocation of the working array (because to preserve side effects, the intent(in) attribut has bee given to the input argument)
  B  =  A                        ! Storing the input values int the working array
  i  =  0                        ! Initialisation of the index element
  do                            ! Infinit loop on all array elements to compare their values to the value of the following elements
    i    =  i  +  1                  ! Incrementation of the array elements index
    Nelt  =  size(B)                      ! Number of array elements
    if  (i>=Nelt)  exit                      ! If the elements index is greater than the array dimension, then exiting the loop
    j  =  i                        ! Initialisation of the index of the element to be compred to the index of the considered element
    do                            ! Infinit loop on all array elements be be compared the element i
      j    =  j  +  1                  ! Incrementation of the index j of the element to be compared with the element i
      if  (j>Nelt)  exit                    ! If the elements index is greater than the array dimension, then exiting the loop
      if  (B(i)/=B(j))  cycle                    ! If both elements have different values, then exiting the loop
      allocate  (  B_temp(Nelt-1)  )                  ! Allocation of the temporary array
      B_temp(1:j-1)  =  B(1:j-1)                  ! Storing previouse values
      B_temp(j:Nelt-1)  =  B(j+1:Nelt)                  ! Storing next values except value j which is to be discarded since it is identical to the value i
      call move_alloc( from = B_temp, to = B )                  ! Transfering allocation from the temporary to the final array
      i  =  i  -  1                    ! Go back in the index cound number to find other identical values (if other identical values follow in the array elements)
      exit                          ! Exiting infinit do loop
    end do                          ! End of loop
  end do                          ! End of loop

End Subroutine UniVal_R

! -------------------------------------------------------------------------------------------------
!> @brief  Routine that reduces a integer 1D-array to a 1D-array with only unique elements
!!
!> @details  This routine reduces a one dimensional array to a vector containing only integer unique elements.
!>     The procedure is as follow:
!>      - The array elements are considered one by one from the first to the before-last element
!>      - Each array element are compared to all the following array elements
!>      - If non-identical values are found then the next array element is considered
!>      - If identical values are found then
!>         - the second values is removed from the vector
!>         - a new vector is created excluding the redundant value
!>         - the element is treated again in order to find other eventual redundant values
!>     This processus is reapeted until all elements are treated and that all redundant element have been excluded from the output vector.
!!
!> @author  Bruno LOPEZ, Bruno.Lopez@ist.utl.pt
!!
!> @version  0.1
!!
!> @date  24/10/11 - Bruno LOPEZ    - Creation of the Fortran Subroutine
!!
!> @remark  This procedure has a clone called 'UniVal_R' whose input and output arguments are real arrays. Both procedures are invoked using the 'UniVal' generic name
!> @remark  This program unit was originally written to remplace the Matlab unique function.
!!
!> @param[in]    A  Array to be reduced with only unique elements
!> @param[out]    B  Array reduced with only unique elements
!<--------------------------------------------------------------------------------------------------
subroutine UniVal_I  (  A, B  )

  implicit none

  integer  ,dimension( : )      ,intent(in)  ::  A          ! Array to be reduced with only unique elements
  integer  ,dimension( : )  ,allocatable  ,intent(out)  ::  B          ! Array reduced with only unique elements
  integer              ::  Nelt          !< Number of elements of the array
  integer              ::  i          !< Index of the array element to be compared
  integer              ::  j          !< Index of the array element being compared
  integer  ,dimension( : )  ,allocatable      ::  B_temp          !< Temporary array

  if  (allocated(B))  deallocate(B)                    ! Deallocated the output array if already allocated
  allocate  (  B(size(A))  )                  ! Allocation of the working array (because to preserve side effects, the intent(in) attribut has bee given to the input argument)
  B  =  A                        ! Storing the input values int the working array
  i  =  0                        ! Initialisation of the index element
  do                            ! Infinit loop on all array elements to compare their values to the value of the following elements
    i    =  i  +  1                  ! Incrementation of the array elements index
    Nelt  =  size(B)                      ! Number of array elements
    if  (i>=Nelt)  exit                      ! If the elements index is greater than the array dimension, then exiting the loop
    j  =  i                        ! Initialisation of the index of the element to be compred to the index of the considered element
    do                            ! Infinit loop on all array elements be be compared the element i
      j    =  j  +  1                  ! Incrementation of the index j of the element to be compared with the element i
      if  (j>Nelt)  exit                    ! If the elements index is greater than the array dimension, then exiting the loop
      if  (B(i)/=B(j))  cycle                    ! If both elements have different values, then exiting the loop
      allocate  (  B_temp(Nelt-1)  )                  ! Allocation of the temporary array
      B_temp(1:j-1)  =  B(1:j-1)                  ! Storing previouse values
      B_temp(j:Nelt-1)  =  B(j+1:Nelt)                  ! Storing next values except value j which is to be discarded since it is identical to the value i
      call move_alloc( from = B_temp, to = B )                  ! Transfering allocation from the temporary to the final array
      i  =  i  -  1                    ! Go back in the index cound number to find other identical values (if other identical values follow in the array elements)
      exit                          ! Exiting infinit do loop
    end do                          ! End of loop
  end do                          ! End of loop

End Subroutine UniVal_I

! -------------------------------------------------------------------------------------------------
!> @brief  Routine that reduces a integer 1D-array to a 1D-array with only non-zero real elements
!!
!> @details  This routine reduces a one dimensional array to a vector containing only real non-zero elements.
!>     The procedure is as follow:
!>      - The array elements are considered one by one from the first to the last element
!>      - If a non-zero value is found then the next array element is considered
!>      - If zero value is found then a new vector is created excluding the zero value
!>     This processus is reapeted until all elements are treated and that all zero elements have been excluded from the output vector.
!!
!> @author  Bruno LOPEZ, Bruno.Lopez@ist.utl.pt
!!
!> @version  0.1
!!
!> @date  25/10/11 - Bruno LOPEZ    - Creation of the Fortran Subroutine
!!
!> @remark  This procedure has a clone called 'NonZero_I' whose input and output arguments are integer arrays. Both procedures are invoked using the 'NonZero' generic name
!!
!> @param[in]    A  Array to be reduced with only non-zero elements
!> @param[out]    B  Array reduced with only non-zero elements
!<--------------------------------------------------------------------------------------------------
subroutine NonZero_R  (  A, B  )

  implicit none

  real(rkp)  ,dimension( : )      ,intent(in)  ::  A          ! Array to be reduced with only non-zeroelements
  real(rkp)  ,dimension( : )  ,allocatable  ,intent(out)  ::  B          ! Array reduced with only non-zero elements
  integer              ::  Nelt          !< Number of elements of the array
  integer              ::  i          !< Index of the array element to be compared
  real(rkp)  ,dimension( : )  ,allocatable      ::  B_temp          !< Temporary array

  if  (allocated(B))  deallocate(B)                    ! Deallocated the output array if already allocated
  allocate  (  B(size(A))  )                  ! Allocation of the working array (because to preserve side effects, the intent(in) attribut has bee given to the input argument)
  B  =  A                        ! Storing the input values int the working array
  i  =  0                        ! Initialisation of the index element
  do                            ! Infinit loop on all array elements
    i    =  i  +  1                  ! Incrementation of the array elements index
    Nelt  =  size(B)                      ! Number of array elements
    if  (i>=Nelt)  exit                      ! If the elements index is greater than the array dimension, then exiting the loop
    if  (B(i)/=0)  cycle                      ! If the considered element is non-zero, then considering next element
    allocate  (  B_temp(Nelt-1)  )                  ! Allocation of the temporary array
    B_temp(1:i-1)  =  B(1:i-1)                  ! Storing previouse values
    B_temp(i:Nelt-1)  =  B(i+1:Nelt)                  ! Storing next values except value j which is to be discarded since its value is zero
    call move_alloc( from = B_temp, to = B )                  ! Transfering allocation from the temporary to the final array
    i  =  i  -  1                    ! Go back in the index cound number to find other zero values
  end do                          ! End of loop

End Subroutine NonZero_R

! -------------------------------------------------------------------------------------------------
!> @brief  Routine that reduces a integer 1D-array to a 1D-array with only non-zero integer elements
!!
!> @details  This routine reduces a one dimensional array to a vector containing only integer non-zero elements.
!>     The procedure is as follow:
!>      - The array elements are considered one by one from the first to the last element
!>      - If a non-zero value is found then the next array element is considered
!>      - If zero value is found then a new vector is created excluding the zero value
!>     This processus is reapeted until all elements are treated and that all zero elements have been excluded from the output vector.
!!
!> @author  Bruno LOPEZ, Bruno.Lopez@ist.utl.pt
!!
!> @version  0.1
!!
!> @date  25/10/11 - Bruno LOPEZ    - Creation of the Fortran Subroutine
!!
!> @remark  This procedure has a clone called 'NonZero_R' whose input and output arguments are real arrays. Both procedures are invoked using the 'NonZero' generic name
!!
!> @param[in]    A  Array to be reduced with only non-zero elements
!> @param[out]    B  Array reduced with only non-zero elements
!<--------------------------------------------------------------------------------------------------
subroutine NonZero_I  (  A, B  )

  implicit none

  integer  ,dimension( : )      ,intent(in)  ::  A          ! Array to be reduced with only non-zero elements
  integer  ,dimension( : )  ,allocatable  ,intent(out)  ::  B          ! Array reduced with only non-zero elements
  integer              ::  Nelt          !< Number of elements of the array
  integer              ::  i          !< Index of the array element
  integer  ,dimension( : )  ,allocatable      ::  B_temp          !< Temporary array

  if  (allocated(B))  deallocate(B)                    ! Deallocated the output array if already allocated
  allocate  (  B(size(A))  )                  ! Allocation of the working array (because to preserve side effects, the intent(in) attribut has bee given to the input argument)
  B  =  A                        ! Storing the input values int the working array
  i  =  0                        ! Initialisation of the index element
  do                            ! Infinit loop on all array elements
    i    =  i  +  1                  ! Incrementation of the array elements index
    Nelt  =  size(B)                      ! Number of array elements
    if  (i>=Nelt)  exit                      ! If the elements index is greater than the array dimension, then exiting the loop
    if  (B(i)/=0)  cycle                      ! If the considered element is non-zero, then considering next element
    allocate  (  B_temp(Nelt-1)  )                  ! Allocation of the temporary array
    B_temp(1:i-1)  =  B(1:i-1)                  ! Storing previouse values
    B_temp(i:Nelt-1)  =  B(i+1:Nelt)                  ! Storing next values except value j which is to be discarded since its value is zero
    call move_alloc( from = B_temp, to = B )                  ! Transfering allocation from the temporary to the final array
    i  =  i  -  1                    ! Go back in the index cound number to find other zero values
  end do                          ! End of loop

End Subroutine NonZero_I

! *****************************************************************************************************
! * <---------------(imin-1)|(imin)---------------- A ----------------(imax)|(imax+1)---------------> * Input array whose elements between the (imin) and (imax) index must be removed if their are closer than the minimum step allowed (Dmin)
! *                         |                                               |                         *
! *                         <(imin)---------------------------------(--imax)>                         * Sub-array which corresponds to the array whose elements must be removed if their are closer than the minimum step allowed (Dmin)
! *                         |                                               |                         *
! *                         |               <------ B ------>               |                         * Reduced sub-array in which all elements closer than the minimum step allowed (Dmin) have been removed
! *                         |                                               |                         *
! * <--------------(imin-1)>|      +        <------ B ------>      +        |<(imax+1)--------------> * Representation of the 3 array to be summed: the lower, middle and upper parts
! *                         |                                               |                         *
! * <---------------------->+<--------------->+<---------------------->                               * Output array composed of
! *  1                imin-1 |               | |                      |                               *    the lower part of the input array
! *                       imin  imin+Size(B)-1 |                      |                               *    the reduced sub-array in which elements have been removed
! *                                     imin+Size(B)      imin+Size(B)+Size(A)-imax                   *    the upper part of the input array
! *****************************************************************************************************

! <u>

! -------------------------------------------------------------------------------------------------
!> @brief  Routine removing from an input array all elements which are closer that the minimum step allowed
!!
!> @details  This routine removes from an input array all elements which are closer that the minimum step allowed. \n
!>     The removal is performed for all elements of the input array between the \c imin and \c imax index, which are optional input arguments. \n
!>     If these optional index are absent, then the lower and upper bound of the input array are used so that the removal process consideres the entire input array. \n
!>     The removal is performed for all elements which the value to the next element is less than an the \c Dmin minimum step value, which is an optional input argument. \n
!>     If this optional minimum step value is absent, then the value <c>Dmin=1</c> is taken.
!>     This routine works as followed:
!>       - <b>Determination of the array reduction parameters: </b>\n
!>          According to the presence or not of the \c imin, \c imax and \c Dmin optional argument, a value is affected to these parameters. \n
!>          These parameters are store in the variable \c i_min, \c i_max and \c Delta respectively. \n \n
!>       - <b>Extraction of the sub-array to be reduced: </b>\n
!>          A sub-array \c B corresponding to the array whose elements must be removed is extracted from the input array \c A.  \n \n
!>       - <b>Reduction of the sub-array: </b>\n
!>          The sub-array \c B is reduced by removing all elements whose step to the next element is less than the minimum step value. \n
!>          This reduction operation is performed using two infinit do construct.
!>          The first do construct loops on all the array elements until the local index exceed the size of the reduced array. \n
!>          The second do construct loops on all the following array elements. \n
!>          The step is computed and the second element is removed if the two elements have values within the minimum step. \n
!>       - <b>Storing the lower, reduced and upper sub-arrays in the output array: </b>\n
!>          The number of elements of the output variable is calculated using from the dimension of the lower, reduced and upper sub-arrays. \n
!>          Then, the values of the lower, reduced and upper sub-arrays are stored in a temporary array.
!>          Finally, the allocation and values is transfered from the temporary array to the output array
!>
!!
!> @remark  The index used to delimit the lower, reduced, upper and output arrays are as follow:
!! <pre>
!! <---------------(imin-1)|(imin)---------------- A ----------------(imax)|(imax+1)--------------->   Input array whose elements between the min. and max. index must be removed if their are closer than the minimum step allowed
!!                         |                                               |
!!                         |<(imin)---------------------------------(imax)>|                           Sub-array which corresponds to the array whose elements must be removed if their are closer than the minimum step allowed
!!                         |                                               |
!!                         |               <------ B ------>               |                           Reduced sub-array in which all elements closer than the minimum step allowed have been removed
!!                         |                                               |
!! <--------------(imin-1)>|      +        <------ B ------>      +        |<(imax+1)-------------->   Representation of the 3 array to be summed: the lower, middle and upper parts
!!                         |                                               |
!! <---------------------->+<--------------->+<---------------------->                                 Output array composed of
!!  1                imin-1 |               | |                      |                                    - the lower part of the input array
!!                       imin  imin+Size(B)-1 |                      |                                    - the reduced sub-array in which elements have been removed
!!                                     imin+Size(B)      imin+Size(B)+Size(A)-imax                        - the upper part of the input array
!! </pre>
!!
!> @author  Bruno LOPEZ, Bruno.Lopez@ist.utl.pt
!!
!> @version  0.1
!!
!> @date  30/10/11 - Bruno LOPEZ    - Creation of the Fortran Subroutine
!<--------------------------------------------------------------------------------------------------
Subroutine MinStep  (  A, Dmin, imin, imax  )

  implicit none

  real(rkp)  ,dimension( : )  ,allocatable    ,intent(inout)  ::  A           !< <c>[inout]</c>  Input array whose elements between the 'imin' and 'imax' index must be removed if their are closer than the minimum step allowed 'Dmin'
  real(rkp)                         ,optional ,intent(in)  ::  Dmin          !< <c>[in-opt]</c> Minimum step allowed between two consecutive elements of the input array 'A'
  integer                           ,optional ,intent(in)  ::  imin          !< <c>[in-opt]</c> Index of the lower element from which the input array is to be reduced
  integer                           ,optional ,intent(in)  ::  imax          !< <c>[in-opt]</c> Index of the upper element to which the input array is to be reduced
  integer              ::  Nelt          !< Number of elements of the array
  integer              ::  i          !< Index of the current array element
  integer              ::  j          !< Index of the following array element
  integer              ::  i_min          !< Actual value for the index of the lower element from which the input array is to be reduced
  integer              ::  i_max          !< Actual value for the index of the upper element to which the input array is to be reduced
  real(rkp)              ::  Delta          !< Actual value for the minimum step allowed between two consecutive elements
  real(rkp)  ,dimension( : )  ,allocatable      ::  B          !< Temporary array
  real(rkp)  ,dimension( : )  ,allocatable      ::  C          !< Temporary array

! **********************************************************
! *     DETERMINATION OF THE ARRAY REDUCTION PARAMETERS    *
! **********************************************************
  i_min    =  lbound(A,1)                    ! Initialisation of the minimum index from which the input array is to be reduced
  i_max    =  ubound(A,1)                    ! Initialisation of the maximum index to which the input array is to be reduced
  Delta    =  1.0_rkp                      ! Initialisation of the minimum allowed step value between two elements
  if  ( present(imin) )  i_min  =  imin                ! If the \c imin optional input argument is present, then affect its value to the local variable
  if  ( present(imax) )  i_max  =  imax                ! If the \c imax optional input argument is present, then affect its value to the local variable
  if  ( present(Dmin) )  Delta  =  Dmin                ! If the \c dmin optional input argument is present, then affect its value to the local variable

! *****************************************************
! *     EXTRACTION OF THE SUB-ARRAY TO BE REDUCED     *
! *****************************************************
  allocate  (  B(imax-imin+1)  )                  ! Allocation of the working array (because to preserve side effects, the intent(in) attribut has bee given to the input argument)
  B  =  A(imin:imax)                      ! Storing the input values int the working array

! **************************************
! *     REDUCTION OF THE SUB-ARRAY     *
! **************************************
  i  =  0                        ! Initialisation of the index element
  do                            ! Infinit loop on all array elements to compare their values to the value of the following elements
    i    =  i  +  1                  ! Incrementation of the array elements index
    Nelt  =  size(B)                      ! Number of array elements
    if  (i>=Nelt)  exit                      ! If the elements index is greater than the array dimension, then exiting the loop
    j  =  i                        ! Initialisation of the index of the element to be compred to the index of the considered element
    do                            ! Infinit loop on all array elements be be compared the element i
      j    =  j  +  1                  ! Incrementation of the index j of the element to be compared with the element i
      if  (j>Nelt)  exit                    ! If the elements index is greater than the array dimension, then exiting the loop
      if  (abs(B(j)-B(i))>Delta)  cycle                  ! If both elements have different values, then exiting the loop
      allocate  (  C(Nelt-1)  )                  ! Allocation of the temporary array
      C(1:j-1)    =  B(1:j-1)                  ! Storing previouse values
      C(j:Nelt-1)  =  B(j+1:Nelt)                  ! Storing next values except value j which is to be discarded since it is identical to the value i
      call move_alloc( from = C, to = B )                  ! Transfering allocation from the temporary to the final array
      i  =  i  -  1                    ! Go back in the index cound number to find other identical values (if other identical values follow in the array elements)
      exit                          ! Exiting infinit do loop
    end do                          ! End of loop
  end do                          ! End of loop

! *******************************************************************************
! *     STORING THE LOWER, REDUCED AND UPPER SUB-ARRAYS IN THE OUTPUT ARRAY     *
! *******************************************************************************
  Nelt  =  (i_min - 1)  +  (size(A) - imax)  +  size(B)          ! Number of elements of the output variable (Lower, reduced and upper sub-arrays)
  allocate  (  C(Nelt)  )                    ! Allocation of the temporary array
  if  (i_min/=1)  C(1:i_min-1)    =  A(1:i_min-1)            ! Storing the values of the lower sub-array
  C( i_min : i_min+size(B)-1 )  =  B                  ! Storing the values of the reduced sub-array
  if  (i_max/=size(A)) C( i_min+size(B) : i_min+size(B)+size(A)-(i_max+1) )  =  A(i_max+1:size(A))  ! Storing the values of the upper sub-array
  call move_alloc( from = C, to = A )                    ! Transfering allocation and values from the temporary to the output array

End Subroutine MinStep

End Module Tools_Module