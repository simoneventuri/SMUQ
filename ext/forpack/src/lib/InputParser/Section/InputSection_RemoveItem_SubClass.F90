SubModule(InputSection_Class) InputSection_RemoveItem_SubClass

  use Logger_Class          ,only:  Logger, LogLevel_HEAVYDEBUG
  use Utilities_Library     ,only:  GetOptArgValue
  !use InputParameter_Class  ,only:  InputParameter_Type

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

! **************************************************************************************************************
! **************************************************************************************************************
!                               PROCEDURES FOR REMOVING ITEMS FROM A SECTION
! **************************************************************************************************************
! **************************************************************************************************************
Module Procedure RemoveItems
End Procedure

Module Procedure RemoveParameters

  use String_Library        ,only:  UpperCase, GetPosition, EmptyString
  use Utilities_Library     ,only:  IsIncluded, AddElementToArray
  use InputSection_Tools    ,only:  Set_Procedure_Path

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName = "RemoveParameters" ! Name of current procedure
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable                                            ::  ParameterName
  integer                                                               ::  i, j
  logical                                                               ::  KeepParam                           ! Indicator that parameters need to be kept
  logical                                                               ::  FoundSection                   ! Indicator whether a section (either the extraction or target section) has been found
  logical                                                               ::  IsRootSection
  type(InputSection_Type)    ,pointer                                   ::  TargetSection         ! Section from which the items need to be removed
  integer                                                               ::  NParams
  integer ,dimension(:) ,allocatable                                    ::  IndexParamToKeep
  integer ,dimension(:) ,allocatable                                    ::  IndexParamToRemove
  character(:)  ,allocatable  ,dimension(:)                             ::  ParametersNames, ListParamToRemove, ListParamToKeep
  type(InputParameter_Type)   ,dimension(:)   ,allocatable              ::  NewParameters

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  ProcPath      =   Set_Procedure_Path( ProcName, CallProc )

  call EmptyString(ListParamToRemove)
  call EmptyString(ListParamToKeep)

  if ( present(ParameterToRemove ) ) call AddElementToArray( ParameterToRemove , ListParamToRemove )
  if ( present(ParametersToRemove) ) call AddElementToArray( ParametersToRemove, ListParamToRemove )
  if ( present(ParameterToKeep )   ) call AddElementToArray( ParameterToKeep   , ListParamToKeep )
  if ( present(ParametersToKeep)   ) call AddElementToArray( ParametersToKeep  , ListParamToKeep )

  if (Dbg) then
    call Logger%Write( "Input parameters:" )
    if ( size(ListParamToRemove) > 0 ) call Logger%Write( "-> List of parameters to be removed:  ListParamToRemove = ", ListParamToRemove )
    if ( size(ListParamToKeep)   > 0 ) call Logger%Write( "-> List of parameters to be kept:     ListParamToKeep   = ", ListParamToKeep   )
  end if

! ==============================================================================================================
!       GETTING THE SECTION FROM WHICH ITEMS HAVE TO BE REMOVED
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Finding the section from where the items need to be removed: TargetSection" )
  if (Dbg) call Logger%Write( "-> Calling FindTargetSection" )
  call FindTargetSection( This, TargetSection,  &
          FromSubSection  =   FromSubSection,   &
          FoundSection    =   FoundSection,     &
          IsRootSection   =   IsRootSection,    &
          Debug         =   Debug           )
  if (FoundSection) then
    if (Dbg) call Logger%Write( "-> Name of the target section from which items need to be removed: TargetSection%Name = ", TargetSection%Name )
  else
    if (Dbg) call Logger%Write( "-> No target section found => Exiting with no item removal" )
    if (Dbg) call Logger%Exiting()
    return
  end if
! ==============================================================================================================


! ==============================================================================================================
!       CASE WHEN SPECIFIC PARAMETERS NEED TO BE KEPT
! ==============================================================================================================
  if ( size(ListParamToKeep)   > 0 ) then
    if (IsRootSection) then
      allocate( IndexParamToKeep(TargetSection%NParameters) ); IndexParamToKeep = 0
      allocate( character(TargetSection%GetMaxLengthParameterName()) :: ParametersNames(TargetSection%NParameters) )
      do i = 1,TargetSection%NParameters
        ParametersNames(i)  =   trim(TargetSection%Parameters(i)%Name)
      end do
      NParams         =   0
      do j = 1,size(ListParamToKeep)
!         ParameterName =   trim( ListParamToKeep(j) )   ! @COMPILER_BUG: gcc-7.2.0
        ParameterName =   trim( ListParamToKeep(j) )
        ParameterName =   UpperCase( ParameterName )   ! Assume Parameter is NOT case-sensitive
        i             =   GetPosition( ParameterName, ParametersNames, CaseSensitive=.False. )
        if ( i /= 0 ) then
          NParams     =   NParams + 1
          IndexParamToKeep(NParams)  =   i
        end if
      end do
      if (Dbg) call Logger%Write( "Setting the list of index of Parameters to be kept" )
      if (Dbg) call Logger%Write( "-> Number of parameter to be kept: NParams = ", NParams )
      allocate( NewParameters(NParams) )
      do j = 1,NParams
        i                 =   IndexParamToKeep(j)
        NewParameters(j)  =   TargetSection%Parameters(i)
        if (Dbg) call Logger%Write( "-> i = ", j, "Param = ", TargetSection%Parameters(i)%Name )
      end do
      if (Dbg) call Logger%Write( "-> Pushing new parameter in the target section" )
      call move_alloc( NewParameters, TargetSection%Parameters )
      TargetSection%NParameters   =   size(TargetSection%Parameters)
      if (Dbg) call Logger%Write( "-> Done updating Parameters to keep" )
    else
      if (Dbg) call Logger%Write( "-> Calling TargetSection%RemoveParameters" )
      call TargetSection%RemoveParameters(        &
        ParametersToKeep  =   ListParamToKeep   , &
        CallProc          =   ProcPath          , &
        Debug             =   Debug               )
    end if
  end if
! ==============================================================================================================


! ==============================================================================================================
!       CASE WHEN SPECIFIC PARAMETERS NEED TO BE REMOVED
! ==============================================================================================================
  if ( size(ListParamToRemove) > 0 ) then
    if (IsRootSection) then
      if (Dbg) call Logger%Write( "Setting the list of index of Parameters to be removed" )
      if ( allocated(IndexParamToRemove)  ) deallocate(IndexParamToRemove)
      if ( allocated(IndexParamToKeep)    ) deallocate(IndexParamToKeep)
      if ( allocated(ParametersNames)     ) deallocate(ParametersNames)
      allocate( IndexParamToRemove(TargetSection%NParameters) );  IndexParamToRemove  = 0
      allocate( IndexParamToKeep(TargetSection%NParameters) );    IndexParamToKeep    = 0
      allocate( character(TargetSection%GetMaxLengthParameterName()) :: ParametersNames(TargetSection%NParameters) )
      do i = 1,TargetSection%NParameters
        ParametersNames(i)  =   trim(TargetSection%Parameters(i)%Name)
      end do
      NParams         =   0
      do j = 1,size(ListParamToRemove)
!         ParameterName =   trim( ListParamToRemove(j) )   ! @COMPILER_BUG: gcc-7.2.0
        ParameterName =   trim( ListParamToRemove(j) )
        ParameterName =   UpperCase( ParameterName )   ! Assume Parameter is NOT case-sensitive
        i             =   GetPosition( ParameterName, ParametersNames, CaseSensitive=.False. )
        if ( i /= 0 ) then
          NParams     =   NParams + 1     ! NParams is the number of parameter to be removed
          IndexParamToRemove(NParams)  =   i
        end if
      end do
      if (Dbg) call Logger%Write( "-> Number of parameter to be removed: NParams = ", NParams )
!     Take the complement
      NParams   =   TargetSection%NParameters - NParams   ! Setting the number of parameters to keep
      j         =   0
      do i = 1,TargetSection%NParameters
        if ( IsIncluded(i,IndexParamToRemove(1:NParams)) ) then ! If current Parameter is in the list of Parameters to be removed, then cycle
          if (Dbg) call Logger%Write( "-> i = ", i, "Param = ", TargetSection%Parameters(i)%Name )
        else
          j       =   j + 1
          IndexParamToKeep(j) = i
        end if
      end do
      if (Dbg) call Logger%Write( "Setting the list of index of Parameters to be kept" )
      if (Dbg) call Logger%Write( "-> Number of parameter to be kept: NParams = ", NParams )
      allocate( NewParameters(NParams) )
      do j = 1,NParams
        i                 =   IndexParamToKeep(j)
        NewParameters(j)  =   TargetSection%Parameters(i)
        if (Dbg) call Logger%Write( "-> i = ", j, "Param = ", TargetSection%Parameters(i)%Name )
      end do
      if (Dbg) call Logger%Write( "-> Pushing new parameter in the target section" )
      call move_alloc( NewParameters, TargetSection%Parameters )
      TargetSection%NParameters   =   size(TargetSection%Parameters)
      if (Dbg) call Logger%Write( "-> Done updating Parameters to remove" )
    else
      if (Dbg) call Logger%Write( "-> Calling TargetSection%RemoveParameters" )
      call TargetSection%RemoveParameters(          &
        ParametersToRemove  =   ListParamToRemove , &
        CallProc            =   ProcPath          , &
        Debug               =   Debug               )
    end if
  end if
! ==============================================================================================================

  nullify( TargetSection )

  if (Dbg) call Logger%Exiting()
End Procedure

Module Procedure RemoveSections
End Procedure

End SubModule