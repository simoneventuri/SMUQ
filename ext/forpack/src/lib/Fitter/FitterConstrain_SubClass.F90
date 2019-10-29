SubModule(FitterConstrain_Class) FitterConstrain_SubClass

  use Logger_Class    ,only:  Logger

  implicit none

  contains

Module Procedure Initialize_Constrain_Value

  logical                                                               ::  Debug_Loc                       ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='Initialize_Constrain_Value'   ! Procedure name
  integer                                                               ::  iL, iR                            ! Index of the left and right intervals of the interface associated to current constrain
  real(rkp)     ,dimension(:)   ,allocatable                            ::  C_LHS                             ! Vector corresponding the contribution of the left-hand-side interval (computed at the intervals interface) of current constrain to the LHS constrain matrix 'C'. Corresponds to a elements of a single row of the LHS constrain matrix 'C'.
  real(rkp)     ,dimension(:)   ,allocatable                            ::  C_RHS                             ! Vector corresponding the contribution of the right-hand-side interval (computed at the intervals interface) of current constrain to the LHS constrain matrix 'C'. Corresponds to a elements of a single row of the LHS constrain matrix 'C'.

  Debug_Loc = .False.; if (present(Debug)) Debug_Loc = Debug                                            ! Setting local debugging indicator
  if (Debug_Loc) call Logger%Entering( ProcName )

  if (Debug_Loc) call Logger%Write( "Calling This%Initialize_Constrain_Parameters" )
  call This%Initialize_Constrain_Parameters( IntervalIndex, Intervals, Debug )

  iL    =     IntervalIndex(1)                                                                                  ! Setting the index of the left interval in a new variable for commodity
  iR    =     IntervalIndex(2)                                                                                  ! Setting the index of the right interval in a new variable for commodity

! ==============================================================================================================
!    COMPUTING THE CONTRIBUTION TO THE CONSTRAIN MATRIX
! ==============================================================================================================
  if (Debug_Loc) call Logger%Write( "Computing the contributions to the LHS constrain matrix C in the constrain system 'C x = d'" )
  call Intervals(iL)%FitModel%Compute_LHS_RowVector( This%x, C_LHS )                                            ! Computing the contribution of the left-hand-side interval (computed at the intervals interface) of current constrain to the LHS constrain matrix 'C'
  call Intervals(iR)%FitModel%Compute_LHS_RowVector( This%x, C_RHS )                                            ! Computing the contribution of the right-hand-side interval (computed at the intervals interface) of current constrain to the LHS constrain matrix 'C'
  This%N   =     size(C_LHS) + size(C_RHS)                                                                      ! Setting the number of unknown associated to the current Constrain object
  if ( allocated(This%C) ) deallocate(This%C)
  allocate( This%C, source = [C_LHS,-C_RHS] )
  if (Debug_Loc) then
    call Logger%Write( "-> Left-hand-side  interval:         size(C_LHS) = ", size(C_LHS), "C_LHS = ", C_LHS, F2="i3", F4="es9.2" )
    call Logger%Write( "-> Right-hand-side interval:         size(C_RHS) = ", size(C_RHS), "C_RHS = ", C_RHS, F2="i3", F4="es9.2" )
    call Logger%Write( "-> Number of constrained unknowns:   This%N = ", This%N )
    call Logger%Write( "-> Contribution to constrain matrix: This%C = ", This%C, F2="es9.2" )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    COMPUTING THE CONTRIBUTION TO THE CONSTRAIN VECTOR
! ==============================================================================================================
  if (Debug_Loc) call Logger%Write( "Computing the contributions to the RHS constrain vector d in the constrain system 'C x = d'" )
  if ( allocated(This%d) ) deallocate(This%d)
  allocate( This%d, source = 0.0_rkp   )
  if (Debug_Loc) call Logger%Write( "-> Contribution to constrain vector: This%d = ", This%d, F2="es9.2" )
! ==============================================================================================================

  if (Debug_Loc) call Logger%Exiting()

End Procedure


Module Procedure Initialize_Constrain_Derivative

  logical                                                               ::  Debug_Loc                       ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='Initialize_Constrain_Derivative'   ! Procedure name
  integer                                                               ::  iL, iR                            ! Index of the left and right intervals of the interface associated to current constrain
  real(rkp)     ,dimension(:)   ,allocatable                            ::  C_LHS                             ! Vector corresponding the contribution of the left-hand-side interval (computed at the intervals interface) of current constrain to the LHS constrain matrix 'C'. Corresponds to a elements of a single row of the LHS constrain matrix 'C'.
  real(rkp)     ,dimension(:)   ,allocatable                            ::  C_RHS                             ! Vector corresponding the contribution of the right-hand-side interval (computed at the intervals interface) of current constrain to the LHS constrain matrix 'C'. Corresponds to a elements of a single row of the LHS constrain matrix 'C'.

  Debug_Loc = .False.; if (present(Debug)) Debug_Loc = Debug                                            ! Setting local debugging indicator
  if (Debug_Loc) call Logger%Entering( ProcName )

  if (Debug_Loc) call Logger%Write( "Calling This%Initialize_Constrain_Parameters" )
  call This%Initialize_Constrain_Parameters( IntervalIndex, Intervals, Debug )

  iL    =     IntervalIndex(1)                                                                                  ! Setting the index of the left interval in a new variable for commodity
  iR    =     IntervalIndex(2)                                                                                  ! Setting the index of the right interval in a new variable for commodity

! ==============================================================================================================
!    COMPUTING THE CONTRIBUTION TO THE CONSTRAIN MATRIX
! ==============================================================================================================
    if (Debug_Loc) call Logger%Write( "Computing the contributions to the LHS constrain matrix C in the constrain system 'C x = d'" )
    call Intervals(iL)%FitModel%Compute_LHS_Jacobian_Vector( This%x, C_LHS )                                                 ! Computing the contribution of the left-hand-side interval (computed at the intervals interface) of current constrain to the LHS constrain matrix 'C'
    call Intervals(iR)%FitModel%Compute_LHS_Jacobian_Vector( This%x, C_RHS )                                                 ! Computing the contribution of the right-hand-side interval (computed at the intervals interface) of current constrain to the LHS constrain matrix 'C'
    This%N   =     size(C_LHS) + size(C_RHS)                                                                    ! Setting the number of unknown associated to the current Constrain object
    if ( allocated(This%C) ) deallocate(This%C)
    allocate( This%C, source = [C_LHS,-C_RHS] )
    if (Debug_Loc) then
      call Logger%Write( "-> Left-hand-side  interval:         size(C_LHS) = ", size(C_LHS), "C_LHS = ", C_LHS, F2="i3", F4="es9.2" )
      call Logger%Write( "-> Right-hand-side interval:         size(C_RHS) = ", size(C_RHS), "C_RHS = ", C_RHS, F2="i3", F4="es9.2" )
      call Logger%Write( "-> Number of constrained unknowns:   This%N = ", This%N )
      call Logger%Write( "-> Contribution to constrain matrix: This%C = ", This%C, F2="es9.2" )
    end if
! ==============================================================================================================


! ==============================================================================================================
!    COMPUTING THE CONTRIBUTION TO THE CONSTRAIN VECTOR
! ==============================================================================================================
  if (Debug_Loc) call Logger%Write( "Computing the contributions to the RHS constrain vector d in the constrain system 'C x = d'" )
  if ( allocated(This%d) ) deallocate(This%d)
  allocate( This%d, source = 0.0_rkp   )
  if (Debug_Loc) call Logger%Write( "-> Contribution to constrain vector: This%d = ", This%d, F2="es9.2" )
! ==============================================================================================================

  if (Debug_Loc) call Logger%Exiting()

End Procedure


Module Procedure Initialize_Constrain_Parameters

  use Error_Class       ,only:  Error
  use String_Library    ,only:  Inline

  logical                                                               ::  Debug_Loc                       ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='Initialize_Constrain_Parameters'   ! Procedure name
  integer                                                               ::  iL, iR                            ! Index of the left and right intervals of the interface associated to current constrain
  character(:)  ,allocatable                                            ::  String

  Debug_Loc = .False.; if (present(Debug)) Debug_Loc = Debug                                            ! Setting local debugging indicator
  if (Debug_Loc) call Logger%Entering( ProcName )

  iL      =     IntervalIndex(1)                                                                                  ! Setting the index of the left interval in a new variable for commodity
  iR      =     IntervalIndex(2)                                                                                  ! Setting the index of the right interval in a new variable for commodity

  if (Debug_Loc) then
    String  =   Inline( [Intervals(iL)%Xmin,Intervals(iL)%Xmax,Intervals(iR)%Xmax],  Separator="            ", Fmt="es9.2,12x" )
    call Logger%Write( "Initializing constrain associated to the interface between" )
    call Logger%Write( "-> Left  interval: Index = ", iL, "Xmin = ", Intervals(iL)%Xmin, "Xmax = ", Intervals(iL)%Xmax, F2="i3", F4="es15.8", F6="es15.8" )
    call Logger%Write( "-> Right interval: Index = ", iR, "Xmin = ", Intervals(iR)%Xmin, "Xmax = ", Intervals(iR)%Xmax, F2="i3", F4="es15.8", F6="es15.8" )
    call Logger%Write( "    |---Left interval----|---Right interval---|" )
    call Logger%Write( String )
  end if

! ==============================================================================================================
!    CHECKING CONSISTENCY BETWEEN THE LEFT AND RIGHT INTERVALS AND SETTING THE BOUNDARY VALUE
! ==============================================================================================================
  if ( Intervals(iL)%Xmax /= Intervals(iR)%Xmin )  call Error%Raise( 'Intervals(iL)%Xmax /= Intervals(iR)%Xmin', ProcName=ProcName )
  This%x    =     Intervals(iL)%XMax
  if (Debug_Loc) call Logger%Write( "-> Interface position: This%x = ", This%x, F2="es15.8" )
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE INDEX MAPPING VARIABLES FROM LOCAL TO GLOBAL UNKNOWNS
! ==============================================================================================================
  if (Debug_Loc) call Logger%Write( "Setting the local-to-global index mapping for unknowns" )
  if (Debug_Loc) call Logger%Write( "-> Global index of the initial/final unknowns which are associated to current constrain" )
  This%iIniGlo    =   min( Intervals(iL)%iIniGlo , Intervals(iR)%iIniGlo )
  This%iFinGlo    =   max( Intervals(iL)%iFinGlo , Intervals(iR)%iFinGlo )
  if (Debug_Loc) then
    call Logger%Write( "-> This%iIniGlo =  ", This%iIniGlo )
    call Logger%Write( "-> This%iFinGlo =  ", This%iFinGlo )
  end if
! ==============================================================================================================

  if (Debug_Loc) call Logger%Exiting()

End Procedure


Module Procedure Update_C_d
  d(This%Index)                             =     This%d
  C(This%Index,This%iIniGlo:This%iFinGlo)   =     This%C
End Procedure

End SubModule