SubModule(FitterInterval_Class) FitterInterval_SubClass

  use Logger_Class              ,only:  Logger

  implicit none

  logical                                                               ::  Debug_Default = .False.       !< Global debugging indicator

  contains

Module Procedure SetFitModel

  logical                                                               ::  Debug_Loc                       ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='SetFitModel'        ! Procedure name

  Debug_Loc = .False.; if (present(Debug)) Debug_Loc = Debug                                            ! Setting local debugging indicator
  if (Debug_Loc) call Logger%Entering( ProcName )

  if (Debug_Loc) call Logger%Write( "Setting the 'FitModel' polymorphic component" )

  if (Debug_Loc) call Logger%Write( "-> Source allocation to the input variable" )
  if ( allocated(This%FitModel) ) deallocate(This%FitModel)
  allocate( This%FitModel, source=FitModel )                                                                    ! Allocating the This%FitModel component

! ! !   if (Debug_Loc) call Logger%Write( "-> Calling This%FitModel%Initialize" )
! ! !   call This%FitModel%Initialize()                                                                               ! Initializing the This%FitModel component
!   Only initialize the FitModel object if it is not initialized so that we can kept the fit parameters
!   which are passed along with the FitModel object.
!   If we were to initialize the FitModel object, these parameter would be reset to zero.
  if (.Not. This%FitModel%Initialized ) then
    if (Debug_Loc) call Logger%Write( "-> Calling This%FitModel%Initialize" )
    call This%FitModel%Initialize()                                                                               ! Initializing the This%FitModel component
  end if

  if (Debug_Loc) call Logger%Exiting()

End Procedure

! This procedure set the starting/ending index of the elements of x which are in the current fitting interval.
! If there is only one fitting interval (that is, if 'Bounded=False'), then the whole set of index is considered.
Module Procedure Set_Local_Index
  integer                                                               ::  i
  if (.Not.This%Bounded) then     ! Only one interval
    This%is    =     1
    This%ie    =     size(x)
  else
    This%is    =     0
    This%ie    =     0
    do i = 1,size(x)
      if ( (This%is == 0) .And. (x(i) >= This%Xmin) ) This%is = i
      if (                      (x(i) <= This%Xmax) ) This%ie = i
    end do
  end if
End Procedure

Module Procedure InitializeLinearSystem

  logical                                                               ::  Debug_Loc                       ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='InitializeLinearSystem'        ! Procedure name

  Debug_Loc = .False.; if (present(Debug)) Debug_Loc = Debug                                            ! Setting local debugging indicator
  if (Debug_Loc) call Logger%Entering( ProcName )

! ! ==============================================================================================================
! !    SETTING THE INDEX OF ELEMENTS WHICH ARE INSIDE THE CURRENT INTERVAL
! ! ==============================================================================================================
!   if (Debug_Loc) call Logger%Write( "Setting the index of elements which are inside the current interval" )
!   if (Debug_Loc) call Logger%Write( "-> Calling This%Set_Local_Index" )
!   call This%Set_Local_Index( x )                                                                                ! Setting the starting/ending index of the elements of x which are inside the current interval
!   if (Debug_Loc) then
!     call Logger%Write( "-> Total number of points: ", size(x) )
!     call Logger%Write( "-> Local number of points: ", This%ie-This%ie+1 )
!     call Logger%Write( "-> Index of first point:   ", This%is )
!     call Logger%Write( "-> Index of last point:    ", This%ie )
!   end if
!   if ( This%is < lbound(x,1) ) call Error( 'Local index is < lbound(x,1)' , ProcName )
!   if ( This%ie > ubound(x,1) ) call Error( 'Local index ie > ubound(x,1)' , ProcName )
! ! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE DIMENSION OF THE LOCAL LINEAR LEAST SQUARE SYSTEM
! ==============================================================================================================
  if (Debug_Loc) call Logger%Write( "Setting the dimension of the local linear least square system" )
  if (Debug_Loc) call Logger%Write( "-> Starting index of points: This%is = ",This%is )
  if (Debug_Loc) call Logger%Write( "-> Ending index of points:   This%ie = ",This%ie )
  This%M      =     This%ie - This%is + 1

  if (Debug_Loc) call Logger%Write( "-> allocated(This%FitModel) = ", allocated(This%FitModel)  )
  if ( .Not. allocated(This%FitModel) ) then
!   ERROR
  end if

  This%N      =     This%FitModel%Get_NParam()
  if ( allocated(This%x) ) deallocate( This%x )
  if ( allocated(This%y) ) deallocate( This%y )
  allocate( This%x , source = x(This%is:This%ie) )
  allocate( This%y , source = y(This%is:This%ie) )
  if (Debug_Loc) then
    call Logger%Write( "-> Local number of points: This%M = ", This%M )
    call Logger%Write( "-> Local number of coeff.: This%N = ", This%N )
  end if
! ==============================================================================================================

! ==============================================================================================================
!    SETTING THE INDEX MAPPING VARIABLES FROM LOCAL TO GLOBAL UNKNOWNS
! ==============================================================================================================
! This section sets the mapping between the local unknowns, ie. the ones associated to the current interval,
! and the global unknowns, ie the ones associated to all intervals.
! The local unknowns corresponds to the fitting coefficients associated to the current interval only.
! The way this mapping is computed assumes that all intervals have the same fitting fonction.
! These can be easiliy generalized if required.
! ==============================================================================================================
  if (Debug_Loc) call Logger%Write( "Setting the index mapping variables from local to global unknowns" )
  This%iIniGlo    =     (This%Index - 1) * This%N + 1                                                           ! Setting the index of the global unknown associated to the first local unknown
  This%iFinGlo    =     This%iIniGlo + This%N - 1                                                               ! Setting the index of the global unknown associated to the last local unknown
  if (Debug_Loc) then
    call Logger%Write( "-> This%iIniGlo =  ", This%iIniGlo )
    call Logger%Write( "-> This%iFinGlo =  ", This%iFinGlo )
  end if
! ==============================================================================================================

  if (Debug_Loc) call Logger%Exiting()

End Procedure

! This procedure updates the elements of the linear least square system to be solved;
! These elements are:
! * the M-by-N LHS matrix A
! * the M RHS vector b
! where M is the number of fitting positions and N is number of model parameters (Unknows).
! Pure
Module Procedure Update_A_b

  use ieee_arithmetic   !,only:  ieee_Is_Finite
!   use Error_Class         ,only:  Error
  use Arithmetic_Library  ,only:  IsFinite, Get_IEEE_Class_Name
  use String_Library      ,only:  Convert_To_String, Convert_Ratio, Add_Line_To_String, Inline

  logical                                                               ::  Debug_Loc                     ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='Update_A_b'     ! Procedure name

  integer                                                               ::  is
  integer                                                               ::  ie
  integer                                                               ::  js
  integer                                                               ::  je
  integer                                                               ::  Ml                                ! Local value for M: Number of fitting point associated to current interval
  integer                                                               ::  Nl                                ! Local value for N: Number of model parameters associated to current interval (ie. number of unknows)
  real(rkp)     ,dimension(:,:) ,allocatable                            ::  Al                                ! Local LHS matrix: Contribution of current interval to the LHS matrix of the linear system A x = b
  real(rkp)     ,dimension(:)   ,allocatable                            ::  bl                                ! Local RHS vector: Contribution of current interval to the RHS vector of the linear system A x = b
  character(:)  ,allocatable                                            ::  String
!   type(IEEE_CLASS_TYPE) ::  class_type
  integer                                   ::  Number, i, j
  character(:)  ,allocatable  ,dimension(:) ::  ClassNames

  Debug_Loc = Debug_Default; if ( present(Debug) ) Debug_Loc = Debug                                  ! Setting local debugging indicator
  if (Debug_Loc) call Logger%Entering( ProcName )

! Computing the local elements of the system
  if (Debug_Loc) call Logger%Write( "Computing the local elements of the system" )
  if (Debug_Loc) call Logger%Write( "-> This%FitModel%Compute_LHS_Matrix" )
  call This%FitModel%Compute_LHS_Matrix( This%x, Al )
  if (Debug_Loc) call Logger%Write( "-> This%FitModel%Compute_RHS_Vector" )
  call This%FitModel%Compute_RHS_Vector( This%y, bl )

!   associate( Variable => Al, VariableName => 'Al' )
!   if ( .Not. IsFinite(Variable) ) then
!     Number  =   count( .Not. ieee_Is_Finite(Variable) )
!     j = 0
!     do i = 1,Number
!       if ( IsFinite(Variable(i)) ) cycle
!       j               =   j + 1
!       call Add_Line_To_String( ClassNames, Get_IEEE_Class_Name( Variable(i) ) )
!     end do
!     call Error%Raise(                                                                               &
!       [ character(1000) :: "Non finite values are found."                              , &
!         " * Variable name:                  " // VariableName                               , &
!         " * Number of non fintie elements:  " // Convert_Ratio( Number, size(VariableName) )  , &
!         " * ClassNames:  " // Inline( ClassNames )  , &
!         "Parameters of current 'FitInterval' object:"                               , &
!         " * Bounded:                   " // Convert_To_String(This%Bounded )        , &
!         " * Index:                     " // Convert_To_String(This%Index   )        , &
!         " * Xmin:                      " // Convert_To_String(This%Xmin    )        , &
!         " * Xmax:                      " // Convert_To_String(This%Xmax    )        , &
!         " * is:                        " // Convert_To_String(This%is      )        , &
!         " * ie:                        " // Convert_To_String(This%ie      )        , &
!         " * iIniGlo:                   " // Convert_To_String(This%iIniGlo )        , &
!         " * iFinGlo:                   " // Convert_To_String(This%iFinGlo )        , &
!         " * M:                         " // Convert_To_String(This%M       )        , &
!         " * N:                         " // Convert_To_String(This%N       )        , &
!         " * FitModel%Name:             " // This%FitModel%Name        , &
!         "Details about non-finite elements" ]                                      , &
!       Title     =   "Non-finite values found"                                       , &
!       ProcName  =   ProcName                                                        )
!   end if
!   end associate

!   associate( Variable => bl, VariableName => 'bl' )
  if ( .Not. IsFinite(bl) ) then
    call Logger%Write( "<Error> Non-finite values have been found in variable 'bl' " )
    call Logger%Write( "<Error> in the procedure 'Update_A_b'." )
    Number  =   count( .Not. ieee_Is_Finite(bl) )
    j = 0
    do i = 1,size(bl)
      if ( IsFinite(bl(i)) ) cycle
      call Logger%Write( "<Error> i = ", i, "This%x(i) = ", This%x(i), "This%y(i) = ", This%y(i), "bl(i) = ", bl(i), "IEEE:", Get_IEEE_Class_Name(bl(i)), Fi="i6", Fr="es15.8" )
      j               =   j + 1
      call Add_Line_To_String( ClassNames, Get_IEEE_Class_Name( bl(i) ) )
      if (Debug_Loc) call Logger%Write( "-> i = ", i, "bl(i) = ", bl(i), "This%y = ", This%y, Fr="es15.8" )
    end do
    String    =   Convert_Ratio( Number, size(bl) )
!     call Error%Raise(                                                                               &
!       [ character(1000) :: "Non finite values are found."                              , &
!         " * Variable name:                  bl"                               , &
! !         " * Number of non fintie elements:  " // Convert_To_String(Number)  , &
! !         " * Number of non fintie elements:  " // String  , &
!         " * ClassNames:  " // Inline( ClassNames )  , &
!         "Parameters of current 'FitInterval' object:"                               , &
!         " * Bounded:                   " // Convert_To_String(This%Bounded )        , &
!         " * Index:                     " // Convert_To_String(This%Index   )        , &
!         " * Xmin:                      " // Convert_To_String(This%Xmin    )        , &
!         " * Xmax:                      " // Convert_To_String(This%Xmax    )        , &
!         " * is:                        " // Convert_To_String(This%is      )        , &
!         " * ie:                        " // Convert_To_String(This%ie      )        , &
!         " * iIniGlo:                   " // Convert_To_String(This%iIniGlo )        , &
!         " * iFinGlo:                   " // Convert_To_String(This%iFinGlo )        , &
!         " * M:                         " // Convert_To_String(This%M       )        , &
!         " * N:                         " // Convert_To_String(This%N       )        , &
!         " * FitModel%Name:             " // This%FitModel%Name        , &
!         "Details about non-finite elements" ]                                      , &
!       Title     =   "Non-finite values found"                                       , &
!       ProcName  =   ProcName                                                        )
  end if
!   end associate


! Setting the index mapping between local/global elements
  is    =     This%is
  ie    =     This%ie
  js    =     This%iIniGlo
  je    =     This%iFinGlo
  Ml    =     ubound(Al,1)                                                                                      ! Getting the number of fitting point associated to current interval
  Nl    =     ubound(Al,2)                                                                                      ! Getting the number of model parameters associated to current interval (ie. number of unknows)

! Setting the contribution of current interval to the global system
  A(is:ie,js:je)    =     Al(1:Ml,1:Nl)
  b(is:ie)          =     bl(1:Ml)

  if (Debug_Loc) call Logger%Exiting()

End Procedure

Module Procedure Compute_A_b
  call This%FitModel%Compute_LHS_Matrix( This%x, A )
  call This%FitModel%Compute_RHS_Vector( This%y, b )
End Procedure


! This procedure sets the mapping between the local unknowns (the ones associated to the current interval)
! and the global unknowns (ones associated to all intervals). The local unknowns corresponds to the
! fitting coefficients associated to the current interval only. Since this procedure needs to know the number
! of unknowns of each interval, it can only be called once the 'FitModel' component has been set for all
! the intervals. The mapping from local to global unknowns is fully described by only 2 integer variables:
!  - iIniGlo: the index of the global unknown associated to the first local unknown
!  - iFinGlo: the index of the global unknown associated to the last local unknown
! Note that the way the mapping is computed assumes that all intervals have the same fitting fonction.
! These can be easiliy generalized if required.
Module Procedure Set_MappingLocalToGlobalUnknows
  use Error_Class       ,only:  Error
  character(*)                                              ,parameter  ::  ProcName='Set_MappingLocalToGlobalUnknows'        ! Procedure name
  logical                                                               ::  Debug_Loc                       ! Local debugging indicator
  integer                                                               ::  N, i
  Debug_Loc = .False.; if (present(Debug)) Debug_Loc = Debug                                            ! Setting local debugging indicator
  if (Debug_Loc) call Logger%Entering( ProcName )
  if ( .Not. allocated(This%FitModel) ) call Error%Raise( "The 'FitModel' object has not been allocated" , ProcName=ProcName )   ! Checking pre-conditions
  i               =     This%Index                                                                              ! Getting the index of current interval
  N               =     This%FitModel%Get_NParam()                                                              ! Getting the local number of unknows
  This%iIniGlo    =     1 + (i - 1) * N                                                                         ! Setting the index of the global unknown associated to the first local unknown
  This%iFinGlo    =     This%iIniGlo + N - 1                                                                    ! Setting the index of the global unknown associated to the last local unknown
  if (Debug_Loc) then
    call Logger%Write( "Local-to-global index mapping for unknowns of interval ", i )
    call Logger%Write( "-> This%iIniGlo =  ", This%iIniGlo )
    call Logger%Write( "-> This%iFinGlo =  ", This%iFinGlo )
  end if
  if (Debug_Loc) call Logger%Exiting()
End Procedure

Module Procedure GetSummary
  use String_Library    ,only:  Convert_To_String, SetLength, Inline
  character(*)                                              ,parameter  ::  Spaces = "   "
  real(rkp)   ,dimension(:) ,allocatable                                ::  Param                           ! Fit parameters
  Summary = ''
  if ( .Not. allocated(This%FitModel) ) return
  allocate( Param , source = This%FitModel%Get_Param() )              ! Getting the fit parameters
  Summary =   "[" // Convert_To_String( This%Xmin ) // ":" // Convert_To_String( This%Xmax ) // "]"
  Summary =   SetLength( Summary, max(20,len_trim(Summary)) )
  Summary =   Summary // Spaces // This%FitModel%Get_Name()
  Summary =   Summary // Spaces // Inline( Param, Separator=" , ", Fmt="es15.8")
End Procedure

End SubModule