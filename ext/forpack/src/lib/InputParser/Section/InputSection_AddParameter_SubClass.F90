SubModule(InputSection_Class) InputSection_AddParameter_SubClass

  use Logger_Class          ,only:  Logger
  use Utilities_Library     ,only:  GetOptArgValue
  use InputParameter_Class  ,only:  InputParameter_Type

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

! **************************************************************************************************************
!                            PROCEDURES FOR ADDING PARAMETERS TO A SECTION
! **************************************************************************************************************

! This procedure adds a Parameter object inside a section object from a InputParamProperties object an its
! associated value.
Module Procedure AddParameterFromProperties
  use InputParameter_Class        ,only:  AddParameter
  use String_Library              ,only:  UpperCase

  character(*)                                              ,parameter  ::  ProcName = "AddParameterFromProperties" ! Name of current procedure
  logical                                                               ::  Dbg
  logical                                                               ::  AppendParameter
  logical                                                               ::  Found
  integer                                                               ::  i                                   ! Index of sections/parameters
  character(:)  ,allocatable                                            ::  TargetSectionName                   ! Name of the target section
  character(:)  ,allocatable                                            ::  Action_                             ! Local indicator of the action to be performed when adding the element
  character(:)  ,allocatable                                            ::  Value_                              ! Local parameter value
  type(InputParameter_Type)                                             ::  Param                               ! Parameter object
  type(InputParameter_Type)  ,allocatable      ,dimension(:)            ::  List_Para
  type(InputSection_Type)    ,pointer                                   ::  TargetSection

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if ( len_trim(Properties%GetName()) == 0 ) then
    if (Dbg) call Logger%Write( "Too bad...")
    call Error_Nameless_Properties_Add( Properties, CallProc=ProcName )
  end if

! ==============================================================================================================
!   SETTING THE ACTION TO BE PERFORMED WHEN ADDING THE ELEMENT
! ==============================================================================================================
  Action_ =   "APPEND"
  if ( present(Action) ) then
    select case ( UpperCase(Action) )
    case ("APPEND");  Action_ = "APPEND"
    case ("MERGE");   Action_ = "MERGE"
    case ("REPLACE"); Action_ = "REPLACE"
    end select
  end if
!   @TODO: Check that the input arguemnt 'Action' has a valid value [APPEND,MERGE,REPLACE]
! ==============================================================================================================

  if (Dbg) then
    call Logger%Write( "Adding parameter to section: " )
    call Logger%Write( "-> Properties%GetName()  = ", Properties%GetName(), "Value = ", Value, "Action_ = ", Action_ )
  end if

! ==============================================================================================================
!       SETTING THE TARGET SECTION WHERE THE PARAMETER HAS TO BE ADDED
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting target section" )
  if ( present(SectionName) ) then
    if (Dbg) call Logger%Write( "-> Adding to sub-section '"// SectionName // "' of section '" // This%Name // "'  parameter '" // Properties%GetName() // "' with the value '" // Value // "'" )
    if (Dbg) call Logger%Write( "-> Calling This%HasSection: '"//SectionName//"'" )
    Found       =   This%HasSection( SectionName, Debug=Debug )
    if (Dbg) call Logger%Write( "-> Found = ", Found )
    if ( .Not. Found ) then
      if (Dbg) call Logger%Write( "-> Sub-section '"//SectionName//"' not found in section '"//This%Name//"' => Creating it: Calling This%CreateSubSections" )
      if (Dbg) call Logger%Write( "-> Calling This%CreateSubSections to create sub-section '"//SectionName//"'" )
      call This%CreateSubSections( SectionName, Debug=Debug )
    end if
    if (Dbg) call Logger%Write( "-> Getting the Section pointer to the target section: Calling This%GetSectionPointer" )
    call This%GetSectionPointer( SectionName, TargetSection, Debug=Debug )
  else
    if (Dbg) call Logger%Write( "-> Adding to section '" // This%Name // "' parameter '" // Properties%GetName() // "' with the value '" // Value // "'" )
    TargetSection     =>    This
  end if
  TargetSectionName   =   TargetSection%Name
! ==============================================================================================================


! ==============================================================================================================
!       SETTING THE PARAMETER OBJECT TO BE ADDED TO THE TARGET SECTION
! ==============================================================================================================
! This section sets the Parameter object to be added to the target section.
! The Properties object must be set before the value of the parameter is set.
! This is because when setting the value of a Parameter object using the procedure "Param%SetValue", the code
! checks if the value to be set is valid. The correctness of a value is evaluated as followed:
! * if a ParameterProperties object has no ValidValues, then all values will be considered as correct.
! * if a ParameterProperties object has a set of ValidValues, then only the values which correspond to one of
!   the  valid values will be considered as correct and all other value will be considered as incorrect.
! There is a particular case when the Value variable corresponds to an empty string and Parameter contains
! a set of valid values. Without additional processing an error will when setting the Parameter value since
! the empty value may not be a valid value. In such case, we would like the Parameter value to take the value
! of the default value, if any. This allows to add some empty parameters to a section and when the value of
! those parameters will be extracted the default value will be output.
! If the Value variable corresponds to an empty string and the ParameterProperties object has a set of valid
! values but no default value, then an error should be raised if the empty string is not a valid value.
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting Parameter object to be added to target section" )
  Param%Name            =   Properties%GetName()
  Param%Properties      =   Properties
  Value_                =   Value
  if ( ( len_trim(Value) == 0 ) .and. Param%Properties%HasValidValues .and. Param%Properties%HasDefaultValue ) Value_  =  Param%Properties%DefaultValue
  call Param%SetValue( Value_, Debug=Debug )
  Param%ParentSection   =   TargetSectionName
  Param%Raw   =   Param%Name // " = " // Param%Value !@HACK
  if (Dbg) call Logger%Write( "-> Param%Name          = ", Param%Name )
  if (Dbg) call Logger%Write( "-> Param%Value         = ", Param%Value )
  if (Dbg) call Logger%Write( "-> Param%Raw           = ", Param%Raw )
  if (Dbg) call Logger%Write( "-> Param%ParentSection = ", Param%ParentSection )
! ==============================================================================================================


! ==============================================================================================================
!       ADDING THE PARAMETER TO THE TARGET SECTION
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Adding Parameter object to target section" )
  if (Dbg) call Logger%Write( "-> Action_ = ", Action_ )
  select case ( Action_ )
  case ("APPEND")
    AppendParameter      =     .True.
  case ("REPLACE")
    AppendParameter      =     .True.
    do i = 1,TargetSection%NParameters                                                 ! Loop on all parameters in the target section
      if ( trim(TargetSection%Parameters(i)%Name) /= Param%Name ) cycle                ! If current parameter is different than the parameter to be added, then going to the next parameter
      TargetSection%Parameters(i)  =     Param
      AppendParameter    =     .False.                                                  ! If we reached thios point, this means that the current parameter in the target section is the same (at least, it has the same name) than the parameter to be added
      exit                                                                              ! Exiting the loop on all the parameters of the target section
    end do                                                                              ! End loop on parameters in the target section
!   case ("MERGE")    ! Merging make no sense for parameter  !!! I think ?
  end select

  if (AppendParameter) then
    if (Dbg) call Logger%Write( "-> Number of parameters before adding new parameter: ", size(TargetSection%Parameters) )
    call AddParameter( TargetSection%Parameters, Param, AtStart=AtStart )
! ! *****************************
!     allocate( List_Para( TargetSection%NParameters+1 ) )
!     do i = 1,TargetSection%NParameters
!       List_Para(i)        =   TargetSection%Parameters(i)
!     end do
!     List_Para(TargetSection%NParameters+1)         =   Param
!     call move_alloc( List_Para, TargetSection%Parameters )
! ! *****************************
    TargetSection%NParameters      =   size(TargetSection%Parameters)
  end if
  if (Dbg) call Logger%Write( "-> Number of parameters after  adding new parameter: ", size(TargetSection%Parameters) )
  TargetSection%Empty            =   ( TargetSection%NSections == 0 ) .and. ( TargetSection%NParameters == 0 )
  if (Dbg) call Logger%Write( "-> TargetSection%Empty = ", TargetSection%Empty )
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure

! This procedure adds a Parameter object inside a section object from the parameter's name and value.
! A ParameterProperties object is first constructed using the input Parameter name.
! Then, the parameter is added to the input section using the newly ParameterProperties object and the
! input parameter value. This is done by calling the "AddParameterFromProperties" procedure.
Module Procedure AddParameterFromNameValue
  use InputParamProperties_Class        ,only:  ConstructorInputParamProperties
  character(*)                                              ,parameter  ::  ProcName = "AddParameterFromNameValue" ! Name of current procedure
  logical                                                               ::  Dbg
  type(InputParamProperties_Type)                                       ::  Properties                          ! Parameter-Properties object associated to the parameter to be added
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  Properties    =   ConstructorInputParamProperties( Name = Name )
  call This%AddParameter( Properties, Value &
            , SectionName =   SectionName   &
            , Action      =   Action        &
            , AtStart     =   AtStart       &
            , Debug       =   Debug         )

  if (Dbg) call Logger%Exiting()
End Procedure


! ! This procedure adds a Parameter object inside a section object from a raw parameter string.
! Module Procedure AddParameterFromRaw
!   use InputParamProperties_Class        ,only:  ConstructorInputParamProperties
!   character(*)                                              ,parameter  ::  ProcName = "AddParameterFromRaw" ! Name of current procedure
!   logical                                                               ::  Dbg
!   type(InputParamProperties_Type)                                       ::  Properties                          ! Parameter-Properties object associated to the parameter to be added
!   Dbg   =   GetOptArgValue(DefaultDebug,Debug)
!   if (Dbg) call Logger%Entering( ProcName )
!   Properties    =   ConstructorInputParamProperties( Name = "-" )
!   call This%AddParameter( Properties, Raw, SectionName, Action, Debug )
!   if (Dbg) call Logger%Exiting()
! End Procedure

! This procedure adds a Parameter object inside a Section object from a Parameter object.
! IF the option al input argument OnlyIfDefined is present and is true, and if the input Parameter object
! if not defined, then the parameter is not added to the Section object.
! @BUG @TODO
! However, in some situations, it appears that the ParamProperties object has no name, that is, it is an empty
! string of the variable is not allocated. I guess this occurs if the name of the Parameter object is not copied
! in the name of the Properties object. These two names should always be the same.
! The problem with a empty property name appear in the 'AddParameterFromProperties' procedure.
! If the Parameter name is also empty, then it is definitly an error.
! In Spark, the error appear in procedure 'InitializeChemicalSpeciesFromInput' in the module 'ChemicalSpecies_Class' when calling
! the procedure "call Properties%AddParameter( Models%Parameters )"
! The fix the problem, once should find where the property name is NOT set correctly.
! A simple workaround has been found and consists of copying the name of the Properties to the PArameter object.
Module Procedure AddParameterFromParameter
  use Utilities_Library   ,only:  PresentAndTrue
  character(*)                                              ,parameter  ::  ProcName = "AddParameterFromParameter" ! Name of current procedure
  logical                                                               ::  Dbg
  character(:)  ,allocatable                                            ::  Value                               ! Character string corresponding to the value of the parameter to be added
  type(InputParamProperties_Type)                                       ::  Properties                          ! Parameter-Properties object associated to the parameter to be added
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "-> Param%Name = ", Param%Name )
  if (Dbg) call Logger%Write( "-> Param%Properties%GetName() = ", Param%Properties%GetName() )
  if (Dbg) call Logger%Write( "-> Param%Defined = ", Param%Defined )
  if (Dbg) call Logger%Write( "-> PresentAndTrue(OnlyIfDefined) = ", PresentAndTrue(OnlyIfDefined) )

  if (Dbg) then
    call Logger%Write( "-> present(SectionName  ) = ", present(SectionName  ) )
    call Logger%Write( "-> present(Action       ) = ", present(Action       ) )
    call Logger%Write( "-> present(OnlyIfDefined) = ", present(OnlyIfDefined) )
    call Logger%Write( "-> present(HasBeenAdded ) = ", present(HasBeenAdded ) )
    call Logger%Write( "-> present(Debug      ) = ", present(Debug      ) )
  end if


  if ( PresentAndTrue(OnlyIfDefined) .and. .Not.Param%Defined ) then
    if (present(HasBeenAdded)) HasBeenAdded = .False.
    if (Dbg) call Logger%Write( "-> Exiting" )
    if (Dbg) call Logger%Exiting()
    return
  end if

  Value         =   Param%Value
! ***************************************** WORKAROUND (cf comments) ******************************************
  Properties    =   Param%Properties
  if ( len_trim(Properties%GetName()) == 0 ) call Properties%SetName( Param%Name )
  if (Dbg) call Logger%Write( "-> Calling This%AddParameter" )
  call This%AddParameter( Properties, Value &
            , SectionName =   SectionName   &
            , Action      =   Action        &
            , AtStart     =   AtStart       &
            , Debug       =   Debug         )
! ************************************** CODE ONCE THE PROBLEM IS FIXED ***************************************
!   call This%AddParameter( Param%Properties, Value, SectionName, Action, Debug )
! *************************************************************************************************************
  if (present(HasBeenAdded)) HasBeenAdded = .True.
  if (Dbg) call Logger%Exiting()

End Procedure

! This procedure adds several Parameter objects inside a section object from a list of Parameter objects.
Module Procedure AddParameterFromParameters
  use Utilities_Library   ,only:  PresentAndTrue
  character(*)                                              ,parameter  ::  ProcName = "AddParameterFromParameters" ! Name of current procedure
  logical                                                               ::  Dbg
  integer                                                               ::  i
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  if (Dbg) call Logger%Write( "Number of parameters to be added: size(Params) = ", size(Params) )
  do i = 1,size(Params)
    if ( PresentAndTrue(OnlyIfDefined) .and. .Not.Params(i)%Defined ) cycle
    if (Dbg) call Logger%Write( "Adding parameter: i = ", i, "Params(i)%Name = " // Params(i)%Name // "   Params(i)%Value = ", Params(i)%Value )
    call This%AddParameter( Params(i)         &
              , SectionName =   SectionName   &
              , Action      =   Action        &
              , AtStart     =   AtStart       &
              , Debug       =   Debug         )
  end do
  if (Dbg) call Logger%Exiting()
End Procedure


Module Procedure MoveParameterFromTo

  use Utilities_Library       ,only:  PresentAndTrue
  use InputSection_Tools      ,only:  ProcessListSectionsParam

  character(*)                                              ,parameter  ::  ProcName = "MoveParameterFromTo" ! Name of current procedure
  character(*)                                              ,parameter  ::  DefaultAction = "REPLACE"
  logical                                                               ::  Dbg, ParamAdded
  character(:)  ,allocatable                                            ::  InpSecName, OutSecName, InpParName, OutParName
  character(:)  ,allocatable                                            ::  Action_
  type(InputParameter_Type)                                             ::  Param

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Move parameter from '"//From//"' to '"//To//"'")

! ==============================================================================================================
!   GETTING NAME OF INPUT SECTION AND PARAMETER
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Getting name of input section and parameter" )
  if (Dbg) call Logger%Write( "-> Calling ProcessListSectionsParam" )
  call ProcessListSectionsParam( From, InpSecName, InpParName )
  if (Dbg) call Logger%Write( "-> InpSecName  = ", InpSecName )
  if (Dbg) call Logger%Write( "-> InpParName  = ", InpParName )
! ==============================================================================================================


! ==============================================================================================================
!   GETTING NAME OF OUTPUT SECTION AND PARAMETER
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Getting name of output section and parameter" )
  if (Dbg) call Logger%Write( "-> Calling ProcessListSectionsParam" )
  call ProcessListSectionsParam( To, OutSecName, OutParName )
  if (Dbg) call Logger%Write( "-> OutSecName  = ", OutSecName )
  if (Dbg) call Logger%Write( "-> OutParName  = ", OutParName )
! ==============================================================================================================


! ==============================================================================================================
!   EXTRACTING INPUT PARAMETER FROM INPUT SECTION
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Extracting input parameter from input section" )
  if (Dbg) call Logger%Write( "-> Calling This%GetParameter: Name = ", InpParName, "Section = ", InpSecName )
  Param         =   This%GetParameter( InpParName, SectionName=InpSecName, Debug=.False. )
  if (Dbg) call Logger%Write( "-> Param%Defined = ", Param%Defined )
  if ( .Not. Param%Defined ) then
    if (Dbg) call Logger%Write( "-> Input parameter not found => Nothing to do => Exiting" )
    if (Dbg) call Logger%Exiting()
    return
  end if
! ==============================================================================================================


! ==============================================================================================================
!   CHANGING PARAMETER NAME
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Changing parameter name" )
  if (Dbg) call Logger%Write( "-> Calling Param%SetName: Name = ", OutParName )
  call Param%SetName(OutParName)
  if (Dbg) call Logger%Write( "-> Param%GetName     = ", Param%GetName()     )
  if (Dbg) call Logger%Write( "-> Param%GetRawValue = ", Param%GetRawValue() )
  if (Dbg) call Logger%Write( "-> Param%GetRaw      = ", Param%GetRaw()      )
! ==============================================================================================================


! ==============================================================================================================
!   ADDING INPUT PARAMETER TO NEW SECTION
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Adding input parameter to new section" )
  if (Dbg) call Logger%Write( "-> Calling This%AddParameter: OutSecName = ", OutSecName )
  Action_   =   GetOptArgValue(DefaultAction,Action)
  if (Dbg) call Logger%Write( "-> Action_= ", Action_ )
  if ( len_trim(OutSecName) == 0 ) then
    call This%AddParameter( Param         , &
            Action        =   Action_     , &
            OnlyIfDefined =   .True.      , &
            HasBeenAdded  =   ParamAdded  , &
            Debug         =   .False.       )
  else
    call This%AddParameter( Param         , &
            SectionName   =   OutSecName  , &
            Action        =   Action_     , &
            OnlyIfDefined =   .True.      , &
            HasBeenAdded  =   ParamAdded  , &
            Debug         =   .False.       )
  end if
  if (Dbg) call Logger%Write( "-> ParamAdded = ", ParamAdded )
! ==============================================================================================================


! ==============================================================================================================
!   REMOVING INPUT PARAMETER FROM INPUT SECTION
! ==============================================================================================================
  if ( PresentAndTrue(Remove) ) then
    if (Dbg) call Logger%Write( "Removing input parameter from input section" )
    if (Dbg) call Logger%Write( "-> Calling This%RemoveParameters: ParameterToRemove = ", InpParName, "FromSubSection = ", InpSecName )
    call This%RemoveParameters( ParameterToRemove=InpParName, FromSubSection=InpSecName, Debug=.False. )
  end if
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
!                                       PROCEDURES RELATED TO ERRORS
! **************************************************************************************************************
! **************************************************************************************************************


Subroutine Error_Nameless_Properties_Add( Properties, CallProc )

  use Error_Class          ,only:  Error

  class(InputParamProperties_Type)                      ,intent(in)     ::  Properties
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Nameless_Properties_Add"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object
  call Properties%Output( Lines )                                                                               ! Extracting the information from the parameter properties object

  call Error%Set_Title( "Error while processing a parameter" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  A Parameter-Properties object has an empty name while this parameter is mandatory." )
  call Error%Add_Line( "  The objective here is to add a Parameter object to a section object using a Parameter-Properties object." )
  call Error%Add_Line( "  Thus, is the Parameter-Properties object has no defined name, the Parameter object cannot be added." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:              " // ProcPath )
  call Error%Add_Line( "  Parameter-Properties properties:" )
  call Error%Add_Line( "  * " // Lines )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

End SubModule