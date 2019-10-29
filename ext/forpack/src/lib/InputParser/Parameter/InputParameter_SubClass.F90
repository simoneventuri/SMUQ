SubModule(InputParameter_Class) Parameter_SubClass

  use Logger_Class        ,only:  Logger
  use Utilities_Library    ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure FinalizeParameter
  This%Defined      = .False.
  if ( allocated(  This%Name          ) ) deallocate( This%Name           )
  if ( allocated(  This%Value         ) ) deallocate( This%Value          )
  if ( allocated(  This%ParentSection ) ) deallocate( This%ParentSection  )
  call This%Properties%Free()
End Procedure

! @MEMORY_LEAK: Commenting everything here removes the memory leak
Module Procedure InitializeParameter
  call This%Free()
  call This%SetName( Name )
  This%Value    =   Value
  This%Defined  =   .True.
End Procedure

Module Procedure NewParameterFromNameValue
  call This%SetName( Name )
  This%Value    =   Value
  This%Defined  =   .True.
  This%Raw      =   Name//"="//Value
End Procedure

Module Procedure NewParameterFromRaw
  character(:)  ,allocatable                                            ::  Name
  character(:)  ,allocatable                                            ::  Value
  call Extract_Name_Value( Raw, Name, Value )
  call This%SetName( Name )
  This%Value    =   Value
  This%Defined  =   .True.
  This%Raw      =   Raw
End Procedure

Module Procedure FreeParameter
  This%Defined      = .False.
  if ( allocated( This%Name          ) ) deallocate( This%Name          )
  if ( allocated( This%Value         ) ) deallocate( This%Value         )
  if ( allocated( This%ParentSection ) ) deallocate( This%ParentSection )
  call This%Properties%Free()
End Procedure

Module Procedure SetProperties
  call This%Properties%Set(                               &
                Properties        =   Properties        , &
                Name              =   Name              , &
                CaseSensitive     =   CaseSensitive     , &
                Mandatory         =   Mandatory         , &
                DefaultValue      =   DefaultValue      , &
                ValidValues       =   ValidValues       , &
                DataType          =   DataType          , &
                VariableKind      =   VariableKind      , &
                CheckValidValues  =   CheckValidValues  , &
                LowerThan         =   LowerThan         , &
                LowerEqualThan    =   LowerEqualThan    , &
                GreaterThan       =   GreaterThan       , &
                GreaterEqualThan  =   GreaterEqualThan    )
End Procedure

Module Procedure SetParameterName
  This%Name   =   Name
  call This%Properties%SetName( Name )
  if ( allocated(This%Value) ) then
    if ( len_trim(This%Value) /= 0 ) This%Raw  =   This%Name//" = "//This%Value
  end if
End Procedure

Module Procedure SetParameterValue

  use String_Library      ,only:  UpperCase, Inline
  use Utilities_Library   ,only:  IsIncluded

  character(*)                                              ,parameter  ::  ProcName = "SetParameterValue"
  logical                                                               ::  Dbg
  character(:)  ,allocatable                                            ::  Value_
  character(:)  ,dimension(:)   ,allocatable                            ::  ValidValues

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Setting a parameter value" )
  if (Dbg) call Logger%Write( "-> This%Name = ", This%Name )
  if (Dbg) call Logger%Write( "-> Value     = ", Value )

  This%Value    =   Value
  This%Defined  =   .True.
  This%Raw      =   This%Name//" = "//This%Value

  if (Dbg) call Logger%Write( "Checking value correctness" )
  if (Dbg) call Logger%Write( "-> This%Properties%HasValidValues = ", This%Properties%HasValidValues )
  if ( This%Properties%HasValidValues ) then
    if (Dbg) call Logger%Write( "-> Parameter possible values: This%Properties%ValidValues = " // Inline( This%Properties%ValidValues, Separator="  ") )
# ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
    allocate( character(This%Properties%GetValidValuesLength()) :: ValidValues(size(This%Properties%ValidValues)) )
    ValidValues = This%Properties%ValidValues
# else
    allocate( ValidValues , source = This%Properties%ValidValues )
# endif
    Value_            =   trim(Value)
    if ( .Not. This%Properties%CaseSensitive ) then
      ValidValues(:)  =   UpperCase(ValidValues)   ! @COMPILER_BUG: This line is causing a ICE with ifort ifort version 16.0.0 => This is because of the "Elemental" attribute of the UpperCase procedure
      Value_          =   UpperCase(Value_)
    end if
    if (Dbg) call Logger%Write( "-> This%Properties%CheckValidValues = ", This%Properties%CheckValidValues )
    if (Dbg) call Logger%Write( "-> IsIncluded(Value_,ValidValues) = ", IsIncluded(Value_,ValidValues) )
    if ( This%Properties%CheckValidValues .and. (.Not.IsIncluded(Value_,ValidValues)) ) then
      call Error_Value_Not_In_ValidValues( This, CallProc=ProcName )
    end if
  end if

  if (Dbg) call Logger%Write( "On output: This%Value = ", This%Value )

  if (Dbg) call Logger%Exiting()
End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
!                                       NON-TYPE-BOUND PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

! Module Procedure AddParameterFromNameValue
!   character(*)                                              ,parameter  ::  ProcName = "AddParameterFromNameValue"
!   logical                                                               ::  Dbg
!   type(InputParameter_Type)                                                  ::  New_Parameter
!   type(InputParameter_Type)  ,dimension(:)   ,allocatable                    ::  List_Parameters
!   Dbg   =   GetOptArgValue(DefaultDebug,Debug)
!   if (Dbg) call Logger%Entering( ProcName )
!   if (Dbg) call Logger%Write( "Calling New_Parameter%Initialize: Name = " // Name // "   Value = " // Value )
!   call New_Parameter%Initialize( Name, Value )                                                                  ! Initializing the scalar Parameter object to be added to the array of Parameters object
!   if ( .Not. allocated(Parameters) ) allocate( Parameters(0) )                                                  ! Allocated the Paramater array if it is still deallocated (This enable the current procedure to be called with both, an allocated and a non-allocated array of Parameter objects)
!   allocate( List_Parameters, source = [Parameters,New_Parameter] )                                              ! Source allocation of a temporary array of Parameter object containing the both the previous and the new parameter objects
!   call move_alloc( List_Parameters, Parameters )                                                                ! Transfering the values from the temporary variable to the output variable
!   if (Dbg) call Logger%Write( "size(Parameters) = " , size(Parameters) )
!   if (Dbg) call Logger%Exiting()
! End Procedure

Module Procedure AddParameterFromCharacter

  use String_Library      ,only:  Parse

  character(*)                                              ,parameter  ::  ProcName = "AddParameterFromCharacter"
  logical                                                               ::  Dbg
  character(:)  ,allocatable                                            ::  Name
  character(:)  ,allocatable                                            ::  Value
  type(InputParameter_Type)                                             ::  NewParam
  type(InputParameter_Type)   ,allocatable                              ::  ListParams(:)



  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling Extract_Name_Value: Raw = ", Raw )
  call Extract_Name_Value( Raw, Name, Value )

  if (Dbg) call Logger%Write( "Calling AddParameterFromNameValue: Name = " // Name // "   Value = " // Value )
!?  call AddParameterFromNameValue( Parameters, Name, Value, Debug=Debug )


!     Module Procedure AddParameterFromNameValue
!       character(*)                                              ,parameter  ::  ProcName = "AddParameterFromNameValue"
!       logical                                                               ::  Dbg
!       Dbg   =   GetOptArgValue(DefaultDebug,Debug)
!       if (Dbg) call Logger%Entering( ProcName )
      if (Dbg) call Logger%Write( "Calling NewParam%Initialize: Name = " // Name // "   Value = " // Value )
      call NewParam%Initialize( Name, Value )                                                                  ! Initializing the scalar Parameter object to be added to the array of Parameters object
      NewParam%Raw   =   Raw

      allocate( ListParams, source = [Parameters,NewParam] )                                              ! Source allocation of a temporary array of Parameter object containing the both the previous and the new parameter objects
      call move_alloc( ListParams, Parameters )                                                                ! Transfering the values from the temporary variable to the output variable
      if (Dbg) call Logger%Write( "size(Parameters) = " , size(Parameters) )
!       if (Dbg) call Logger%Exiting()
!     End Procedure


  if (Dbg) call Logger%Exiting()
End Procedure

! This procedure bug in some cases...
Module Procedure AddParameterToParameter
  use Utilities_Library    ,only:  PresentAndTrue
  type(InputParameter_Type)  ,allocatable                               ::  ListParams(:)
  if ( .Not. allocated(Parameters) ) allocate( Parameters(0) )                                                  ! Allocated the Paramater array if it is still deallocated (This enable the current procedure to be called with both, an allocated and a non-allocated array of Parameter objects)
  if ( PresentAndTrue(AtStart) ) then
    allocate( ListParams, source = [NewParam,Parameters] )
  else
    allocate( ListParams, source = [Parameters,NewParam] )                                              ! Source allocation of a temporary array of Parameter object containing the both the previous and the new parameter objects
  end if
  call move_alloc( ListParams, Parameters )
End Procedure

Module Procedure AddParametersToParameter
  use Utilities_Library    ,only:  PresentAndTrue
  type(InputParameter_Type)  ,allocatable                               ::  ListParams(:)
  if ( .Not. allocated(Parameters) ) allocate( Parameters(0) )
  if ( PresentAndTrue(AtStart) ) then
    allocate( ListParams, source = [NewParams,Parameters] )
  else
    allocate( ListParams, source = [Parameters,NewParams] )
  end if
  call move_alloc( ListParams, Parameters )
End Procedure

! There is a compiler bug when calling 'call NewParams(i)%Initialize( Name, Value )'
! So, I'm treating each 'Parameter' one-by-one.
Module Procedure AddParametersFromStrings

  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Name
  character(:)  ,allocatable                                            ::  Value
  character(:)  ,allocatable                                            ::  Raw
  type(InputParameter_Type)  ,allocatable                               ::  NewParams(:)
  type(InputParameter_Type)  ,allocatable                               ::  ListParams(:)
!   type(InputParameter_Type)  ,allocatable                               ::  NewParam

!   allocate(NewParam)
!   do i = 1,size(Strings)
!     call Extract_Name_Value( Strings(i)%GetValue(), Name, Value )
!     call NewParam%Initialize( Name, Value )
!     call AddParameterToParameter( Parameters, NewParam )
! !     call AddParameter( Parameters, NewParam )
!   end do
!   deallocate(NewParam)

  allocate( NewParams(size(Strings)) )                                              ! Source allocation of a temporary array of Parameter object containing the both the previous and the new parameter objects
  do i = 1,size(NewParams)
    Raw   =   Strings(i)%GetValue()
    call Extract_Name_Value( Raw, Name, Value )
    call NewParams(i)%Initialize( Name, Value )
    NewParams(i)%Raw   =   Raw
  end do

  if ( .Not. allocated(Parameters) ) allocate( Parameters(0) )
  allocate( ListParams, source = [Parameters,NewParams] )                                              ! Source allocation of a temporary array of Parameter object containing the both the previous and the new parameter objects
  call move_alloc( ListParams, Parameters )
!
!   if ( .Not. allocated(Parameters) ) allocate( Parameters(0) )                                                  ! Allocated the Paramater array if it is still deallocated (This enable the current procedure to be called with both, an allocated and a non-allocated array of Parameter objects)
!   allocate( ListParams, source = [Parameters,NewParam] )                                              ! Source allocation of a temporary array of Parameter object containing the both the previous and the new parameter objects
!   if ( allocated(Parameters) ) deallocate(Parameters)
!   allocate( Parameters, source = ListParams )

End Procedure



! **************************************************************************************************************
! **************************************************************************************************************
!                                       PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************


Subroutine Extract_Name_Value( String, Name, Value )
  character(*)                                          ,intent(in)     ::  String
  character(:)  ,allocatable                            ,intent(out)    ::  Name
  character(:)  ,allocatable                            ,intent(out)    ::  Value
  character(*)                                              ,parameter  ::  Separator = "="
  integer                                                               ::  iSep                            ! Index of the separation character
  iSep          =   index(String,Separator)                                                                 ! Getting the index of the separation character
  if ( iSep == 0 ) then                                                                                         ! If the separation character is not found in the input string, then ...
    Name        =   trim(adjustl(String))                                                                   ! ... setting the name to the input string and
    Value       =   ""                                                                                      ! ... setting an empty string in the parameter value
  else                                                                                                          ! If the separation character is found in the input string, then ...
    Name        =   trim(adjustl(String(1:iSep-1)))                                                         ! ... setting the name of the parameter to the LHS string
    Value       =   trim(adjustl(String(iSep+1:)))                                                          ! ... setting the value of the parameter to the RHS string
  end if                                                                                                        ! End if case on the index of the separation character
End Subroutine

Subroutine Error_Value_Not_In_ValidValues( Param, CallProc )

  use Error_Class                ,only:  Error

  type(InputParameter_Type)                     ,intent(in)     ::  Param
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Value_Not_In_ValidValues"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines

  ProcPath      =   Logger%GetPath()                                                                        ! Getting the path of all calling procedures stored in the Logger object
  call Param%Properties%Output( Lines )                                                                         ! Extracting the information from the parameter properties object

  call Error%Set_Title( "Error while processing a parameter" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  The parameter being processed has a value which does not match any possible values." )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  This error can be fixed by either:" )
  call Error%Add_Line( "  * add new valid values to the current parameter" )
  call Error%Add_Line( "  * give this parameter a value which corresponds one of its possible values." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( allocated(Param%ParentSection) )  call Error%Add_Line( "  Parent section:              " // Param%ParentSection )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:              " // ProcPath )
  call Error%Add_Line( "  Parameter properties:" )
  call Error%Add_Line( "  * Name = " // Param%Name )
  call Error%Add_Line( "  * Value = " // Param%Value )
  call Error%Add_Line( "  * " // Lines )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

End SubModule