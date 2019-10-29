SubModule(InputSection_Class) InputSection_GetValue_SubClass

  use Logger_Class          ,only:  Logger
  use Utilities_Library     ,only:  GetOptArgValue
  use InputSection_Tools    ,only:  Set_Procedure_Path

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

! **************************************************************************************************************
! **************************************************************************************************************
!                            PROCEDURES FOR EXTRACTING THE VALUE FROM A PARAMETER PROPERTIES OBJECT
! **************************************************************************************************************
! **************************************************************************************************************

! This procedure extracts the value associated to a parameter using a
! Parameter-Properties object. The parameter to be extracted is called
! the "target parameter" and the section from which this parameter is
! extracted is called the "target section". The following workflow is
! considered:
! * If target parameter found in target section:  parameter value is extracted
! * If target parameter not found in target section
!   * if default value in target parameter: its default value is returned
!   * if no default value in target parameter: the variable "Value" unchanged
! This procedure has 3 steps:
! 1) Finding the target section
!   The target parameter is extracted from the target section and stored
!   in a Parameter object. The target section depends on the presence of
!   the optional input argument "SectionName". This argument, if provided,
!   should corresponds to the name of a valid section contained in the
!   current section object (the passed-object dummy argument).
!   The following cases can occur:
!   - If present:  the target section is the sub-section "SectionName" of current section.
!   - If absent:   the target section is the current section.
!   Both cases are handled by calling the "GetParameter" procedure passing in
!   the optional input argument "SectionName".
! 2) Extracting the target parameter from the target section
!   Once the target section have been identified, the parameter value is
!   extracted from the Parameter object using the "GetValue" procedure.
!   If something goes wrong during the value extraction, then an error is
!   raised inside this "GetValue" procedure if the "Status" optional argument
!   is present. If it is absent, then no error is raised and the error status
!   and messag are passed along to the calling procedure.
! 3) Finally, the optional output variables are set.

Module Procedure GetValueFromProp_LOG_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_LOG_0d"
# include "inline-GetValueFromProp_0d.F90"
End Procedure

Module Procedure GetValueFromProp_INT8_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_INT8_0d"
# include "inline-GetValueFromProp_0d.F90"
End Procedure

Module Procedure GetValueFromProp_INT16_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_INT16_0d"
# include "inline-GetValueFromProp_0d.F90"
End Procedure

Module Procedure GetValueFromProp_INT32_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_INT32_0d"
# include "inline-GetValueFromProp_0d.F90"
End Procedure

Module Procedure GetValueFromProp_INT64_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_INT64_0d"
# include "inline-GetValueFromProp_0d.F90"
End Procedure

Module Procedure GetValueFromProp_REAL32_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_REAL32_0d"
# include "inline-GetValueFromProp_0d.F90"
End Procedure

Module Procedure GetValueFromProp_REAL64_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_REAL64_0d"
# include "inline-GetValueFromProp_0d.F90"
End Procedure

Module Procedure GetValueFromProp_REAL128_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_REAL128_0d"
# include "inline-GetValueFromProp_0d.F90"
End Procedure

!
! Same than 'GetValueFromProp_REAL64_0d' but for an allocatable scalar real
Module Procedure GetValueFromProp_REAL64_0d_Alloc

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_REAL64_0d"
  logical                                                               ::  LocalFound
  real(8)                                                               ::  Val                             !< Value of the parameter

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  call This%GetValue(                   &
        Val, Properties               , &
        SectionName   =   SectionName , &
        CallProc      =   CallProc    , &
        Found         =   LocalFound  , &
        Defined       =   Defined     , &
        Mandatory     =   Mandatory   , &
        Status        =   Status      , &
        Debug         =   Debug         )

  if ( LocalFound ) then
    if ( .Not. allocated(Value) ) allocate(Value)
    Value   =   Val
  end if

  if (present(Found)) Found = LocalFound
  if ( UpdateStatus(Status,Proc=ProcName,ExitLogger=Dbg) ) return

  if (Dbg) call Logger%Exiting()

End Procedure



! This procedure extracts the value associated to a parameter, assuming its value is character scalar, and using
! a Parameter-Properties object. The parameter to be extracted is refered as the "target parameter" and the
! section from which this parameter is extracted is called the "target section".
! If the target parameter is found in the target section, then the parameter value is extracted.
! If the target parameter not is found in the target section, then:
! * its default value is returned is the target parameter has a default value
! * the variable "Value" where the parameter value should be stored is not changed.
! This procedure has 3 steps:
! 1) The target parameter is extracted from the target section and stored in a Parameter object. The target
!    section depends on the presence of the optional input argument "SectionName". This argument, if provided,
!    should corresponds to the name of a valid section contained in the current section object, ie. the
!    passed-object dummy argument. The following cases can occur:
!    - If this argument is present, then the target section is the sub-section "SectionName" of current section.
!    - If this argument is absent,  then the target section is the current section.
!    Both cases are handled by calling the generic TBP "GetParameter" procedure of the Section object with the
!    optional input argument "SectionName".
! 2) Then, the parameter value is extracted from the Parameter object using the generic TBP "GetValue".
!    If something goes wrong during the value extraction, an error is raised inside this "GetValue" procedure.
! 3) Finally, the optional output variables are set.
Module Procedure GetValueFromProp_CHAR_0d

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_CHAR_0d"
  character(:)  ,allocatable                                            ::  ProcPath                        ! Procedure path
  type(InputParameter_Type)                                             ::  Param                           ! Parameter object

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  ProcPath      =   Set_Procedure_Path( ProcName, CallProc )
  if (Dbg) call Logger%Write( "Extracting the parameter " // Properties%GetName() )
  if (Dbg) call Logger%Write( "-> Calling This%GetParameter" )
  Param   =   This%GetParameter( Properties, SectionName, Mandatory, Debug=Debug )

  if (Dbg) call Logger%Write( "-> Param%Defined = ", Param%Defined )
  if (Dbg) call Logger%Write( "Extracting the value from parameter " // Properties%GetName() )
  if (Dbg) call Logger%Write( "-> Calling Param%GetValue" )
  call Param%GetValue(              &
          Value                   , &
          CallProc  =   ProcPath  , &
          Status    =   Status    , &
          Debug     =   Debug       )
  if (Dbg) call Logger%Write( "-> Value = ", Value )
  if ( present(Found) )   Found   = Param%Defined
  if ( present(Defined) ) Defined = Param%Defined .or. Param%Properties%HasDefaultValue
  if ( UpdateStatus(Status,Proc=ProcName,ExitLogger=Dbg) ) return

  if (Dbg) call Logger%Exiting()

End Procedure

! This procedure extracts the value associated to a parameter, assuming its value is real(8) vector, and using
! a Parameter-Properties object. The parameter to be extracted is refered as the "target parameter" and the
! section from which this parameter is extracted is called the "target section".
! If the target parameter is found in the target section, then the parameter value is extracted.
! If the target parameter not is found in the target section, then:
! * its default value is returned is the target parameter has a default value
! * the variable "Value" where the parameter value should be stored is not changed.
! This procedure has 3 steps:
! 1) The target parameter is extracted from the target section and stored in a Parameter object. The target
!    section depends on the presence of the optional input argument "SectionName". This argument, if provided,
!    should corresponds to the name of a valid section contained in the current section object, ie. the
!    passed-object dummy argument. The following cases can occur:
!    - If this argument is present, then the target section is the sub-section "SectionName" of current section.
!    - If this argument is absent,  then the target section is the current section.
!    Both cases are handled by calling the generic TBP "GetParameter" procedure of the Section object with the
!    optional input argument "SectionName".
! 2) Then, the parameter value is extracted from the Parameter object using the generic TBP "GetValue".
!    If something goes wrong during the value extraction, an error is raised inside this "GetValue" procedure.
! 3) Finally, the optional output variables are set.
Module Procedure GetValueFromProp_LOG_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_LOG_1d"
# include "inline-GetValueFromProp_1d.F90"
End Procedure

Module Procedure GetValueFromProp_INT8_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_INT8_1d"
# include "inline-GetValueFromProp_1d.F90"
End Procedure

Module Procedure GetValueFromProp_INT16_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_INT16_1d"
# include "inline-GetValueFromProp_1d.F90"
End Procedure

Module Procedure GetValueFromProp_INT32_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_INT32_1d"
# include "inline-GetValueFromProp_1d.F90"
End Procedure

Module Procedure GetValueFromProp_INT64_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_INT64_1d"
# include "inline-GetValueFromProp_1d.F90"
End Procedure

Module Procedure GetValueFromProp_REAL32_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_REAL32_1d"
# include "inline-GetValueFromProp_1d.F90"
End Procedure

Module Procedure GetValueFromProp_REAL64_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_REAL64_1d"
# include "inline-GetValueFromProp_1d.F90"
End Procedure

Module Procedure GetValueFromProp_REAL128_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_REAL128_1d"
# include "inline-GetValueFromProp_1d.F90"
End Procedure


! This procedure extracts the value associated to a parameter, assuming its value is character vector, and using
! a Parameter-Properties object. The parameter to be extracted is refered as the "target parameter" and the
! section from which this parameter is extracted is called the "target section".
! If the target parameter is found in the target section, then the parameter value is extracted.
! If the target parameter not is found in the target section, then:
! * its default value is returned is the target parameter has a default value
! * the variable "Value" where the parameter value should be stored is not changed.
! This procedure has 3 steps:
! 1) The target parameter is extracted from the target section and stored in a Parameter object. The target
!    section depends on the presence of the optional input argument "SectionName". This argument, if provided,
!    should corresponds to the name of a valid section contained in the current section object, ie. the
!    passed-object dummy argument. The following cases can occur:
!    - If this argument is present, then the target section is the sub-section "SectionName" of current section.
!    - If this argument is absent,  then the target section is the current section.
!    Both cases are handled by calling the generic TBP "GetParameter" procedure of the Section object with the
!    optional input argument "SectionName".
! 2) Then, the parameter value is extracted from the Parameter object using the generic TBP "GetValue".
!    If something goes wrong during the value extraction, an error is raised inside this "GetValue" procedure.
! 3) Finally, the optional output variables are set.
Module Procedure GetValueFromProp_CHAR_1d
  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromProp_CHAR_1d"
  character(:)  ,allocatable                                            ::  ProcPath                        ! Procedure path
  type(InputParameter_Type)                                             ::  Param                           ! Parameter object
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  ProcPath      =   Set_Procedure_Path( ProcName, CallProc )
  if (Dbg) call Logger%Write( "Extracting the parameter " // Properties%GetName() )
  if (Dbg) call Logger%Write( "-> Calling This%GetParameter" )
  Param   =   This%GetParameter( Properties, SectionName, Mandatory, Debug=Debug )
  if (Dbg) call Logger%Write( "-> Param%Defined = ", Param%Defined )
  if (Dbg) call Logger%Write( "Extracting the value from parameter " // Properties%GetName() )
  if (Dbg) call Logger%Write( "-> Calling Param%GetValue" )
  call Param%GetValue(            &
        Values                  , &
        CallProc  =   ProcPath  , &
        Status    =   Status    , &
        Debug     =   Debug       )
  if (Dbg) call Logger%Write( "-> Values = ", Values )
  if ( present(Found) )   Found   = Param%Defined
  if ( present(Defined) ) Defined = Param%Defined .or. Param%Properties%HasDefaultValue
  if ( UpdateStatus(Status,Proc=ProcName,ExitLogger=Dbg) ) return
  if (Dbg) call Logger%Exiting()
End Procedure



! **************************************************************************************************************
! **************************************************************************************************************
!                            PROCEDURES FOR EXTRACTING THE VALUE FROM A PARAMETER NAME
! **************************************************************************************************************
! **************************************************************************************************************

! This procedure extracts the value associated to a parameter using the
! parameter name. The parameter to be extracted is refered as the "target parameter" and the
! section from which this parameter is extracted is called the "target section".
! If the target parameter is found in the target section, then the parameter value is extracted.
! If the target parameter not is found in the target section, then:
! * its default value is returned is the target parameter has a default value
! * the variable "Value" where the parameter value should be stored is not changed.
! This procedure has 2 steps:
! 1) In a first step, a Parameter-Properties object is created using the parameter name and all the optional
!    input arguments which are realted to a Parameter-Properties object.
! 2) In a second step, the parameter value is extracted by calling the generic binding "GetValue" using the
!    Parameter-Properties object. This calls the "GetValueFromProp_INT32_0d" procedure.
!    If something goes wrong during the value extraction, an error is raised inside this "GetValue" procedure.


Module Procedure GetValueFromName_CHAR_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_CHAR_0d"
  logical                                                               ::  Dbg
  type(InputParamProperties_Type)                                       ::  Parameter_Properties            ! Parameter-Properties object
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  call Parameter_Properties%Set(            &
        Name            =   ParameterName , &
        DefaultValue    =   DefaultValue    )
  call This%GetValue( &
        Value,        &
        Parameter_Properties   , &
        SectionName , &
        CallProc    , &
        Found       , &
        Defined     , &
        Mandatory   , &
        Status      , &
        Debug         )
  if ( UpdateStatus(Status,Proc=ProcName,ExitLogger=Dbg) ) return
  if (Dbg) call Logger%Exiting()
End Procedure

Module Procedure GetValueFromName_LOG_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_LOG_0d"
# include "inline-GetValueFromName_0d.F90"
End Procedure

Module Procedure GetValueFromName_INT8_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_INT8_0d"
# include "inline-GetValueFromName_0d.F90"
End Procedure

Module Procedure GetValueFromName_INT16_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_INT16_0d"
# include "inline-GetValueFromName_0d.F90"
End Procedure

Module Procedure GetValueFromName_INT32_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_INT32_0d"
# include "inline-GetValueFromName_0d.F90"
End Procedure

Module Procedure GetValueFromName_INT64_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_INT64_0d"
# include "inline-GetValueFromName_0d.F90"
End Procedure

Module Procedure GetValueFromName_REAL32_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_REAL32_0d"
# include "inline-GetValueFromName_0d.F90"
End Procedure

Module Procedure GetValueFromName_REAL64_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_REAL64_0d"
# include "inline-GetValueFromName_0d.F90"
End Procedure

Module Procedure GetValueFromName_REAL128_0d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_REAL128_0d"
# include "inline-GetValueFromName_0d.F90"
End Procedure


! Same than 'GetValueFromName_REAL64_0d' but for an allocatable scalar real
Module Procedure GetValueFromName_REAL64_0d_Alloc

  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_REAL64_0d"
  logical                                                               ::  Dbg
  logical                                                               ::  LocalFound
  real(8)                                                               ::  Val                             !< Value of the parameter

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  call This%GetValue(                   &
          Val, ParameterName,           &
          SectionName =   SectionName , &
          CallProc    =   CallProc    , &
          Found       =   LocalFound  , &
          Defined     =   Defined     , &
          Mandatory   =   Mandatory   , &
          Status      =   Status      , &
          Debug       =   Debug         )

  if ( LocalFound ) then
    if ( .Not. allocated(Value) ) allocate(Value)
    Value   =   Val
  end if

  if (present(Found)) Found = LocalFound
  if (present(DefaultValue)) then; end if !@TODO: Implement default values
  if ( UpdateStatus(Status,Proc=ProcName,ExitLogger=Dbg) ) return

  if (Dbg) call Logger%Exiting()

End Procedure


! This procedure extracts the value associated to a parameter, assuming its value is character vector, and using
! a the parameter name. The parameter to be extracted is refered as the "target parameter" and the
! section from which this parameter is extracted is called the "target section".
! If the target parameter is found in the target section, then the parameter value is extracted.
! If the target parameter not is found in the target section, then:
! * its default value is returned is the target parameter has a default value
! * the variable "Value" where the parameter value should be stored is not changed.
! This procedure has 2 steps:
! 1) In a first step, a Parameter-Properties object is created using the parameter name and all the optional
!    input arguments which are realted to a Parameter-Properties object.
! 2) In a second step, the parameter value is extracted by calling the generic binding "GetValue" using the
!    Parameter-Properties object. This calls the "GetValueFromProp_CHAR_1d" procedure.
!    If something goes wrong during the value extraction, an error is raised inside this "GetValue" procedure.
Module Procedure GetValueFromName_CHAR_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_CHAR_1d"
  logical                                                               ::  Dbg
  type(InputParamProperties_Type)                                       ::  Parameter_Properties            ! Parameter-Properties object
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  call Parameter_Properties%Set(            &
        Name            =   ParameterName , &
        DefaultValue    =   DefaultValue    )
  call This%GetValue(   &
        Values        , &
        Parameter_Properties   , &
        SectionName   , &
        CallProc      , &
        Found         , &
        Defined       , &
        Mandatory     , &
        Status        , &
        Debug           )
  if (Dbg) call Logger%Exiting()
End Procedure


Module Procedure GetValueFromName_LOG_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_LOG_1d"
# include "inline-GetValueFromName_1d.F90"
End Procedure

Module Procedure GetValueFromName_INT8_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_INT8_1d"
# include "inline-GetValueFromName_1d.F90"
End Procedure

Module Procedure GetValueFromName_INT16_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_INT16_1d"
# include "inline-GetValueFromName_1d.F90"
End Procedure

Module Procedure GetValueFromName_INT32_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_INT32_1d"
# include "inline-GetValueFromName_1d.F90"
End Procedure

Module Procedure GetValueFromName_INT64_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_INT64_1d"
# include "inline-GetValueFromName_1d.F90"
End Procedure

Module Procedure GetValueFromName_REAL32_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_REAL32_1d"
# include "inline-GetValueFromName_1d.F90"
End Procedure

Module Procedure GetValueFromName_REAL64_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_REAL64_1d"
# include "inline-GetValueFromName_1d.F90"
End Procedure

Module Procedure GetValueFromName_REAL128_1d
  character(*)                                              ,parameter  ::  ProcName = "GetValueFromName_REAL128_1d"
# include "inline-GetValueFromName_1d.F90"
End Procedure


Module Procedure GetValue_REAL64_d2

  use String_Library              ,only:  Convert_To_String

  character(*)                                              ,parameter  ::  ProcName = "GetValue_REAL64_d2"
  logical                                                               ::  Dbg, FoundMatrix, FoundSection
  integer                                                               ::  i, NRows, NColumns, NElements
  real(REAL64)  ,allocatable                                            ::  Vector(:)
  character(:)  ,allocatable                                            ::  ProcPath
  type(InputParameter_Type)                                             ::  Param
  type(InputSection_Type)    ,pointer                                   ::  TargetSection

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  ProcPath  =   Set_Procedure_Path( ProcName, CallProc )

  if (Dbg) call Logger%Write( "Getting the Section object where the matrix has to be extracted from" )
  if (Dbg) call Logger%Write( "-> Calling FindTargetSection" )
  call FindTargetSection( This, TargetSection , &
          FromSubSection    =   SectionName   , &
          FoundSection      =   FoundSection  , &
          Mandatory         =   Mandatory     , &
          CallProc          =   ProcPath      , &
          Debug             =   Debug           )
  if (Dbg) call Logger%Write( "-> FoundSection = ", FoundSection )

  if ( FoundSection ) then
    if ( allocated(TargetSection%Parameters) ) then
      NRows           =   TargetSection%NParameters
      FoundMatrix     =   .True.
      if (Dbg) call Logger%Write( "Loop on all parameters: TargetSection%NParameters = ", TargetSection%NParameters )
      do i = 1,size(TargetSection%Parameters)
        Param         =   TargetSection%Parameters(i)
        Param%Value   =   Param%Name
        Param%Name    =   Convert_To_String(i)
        if (Dbg) call Logger%Write( "-> i = ", i, "Param%Name = ", Param%Name, "Param%Value = ", Param%Value )
        if (Dbg) call Logger%Write( "-> Calling Param%GetValue" )
        call Param%GetValue( Vector   , &
              Status    =   Status    , &
              CallProc  =   ProcPath    )
        NElements     =   size(Vector)
        if (Dbg) call Logger%Write( "-> NElements = ", NElements )
        if ( .Not. allocated(Values) ) then
          NColumns    =   NElements
          if (Dbg) call Logger%Write( "-> Allocating output variable 'Values' to [NRows,NColumns] = ", [NRows,NColumns] )
          allocate( Values(NRows,NColumns) )
        else
          if ( NElements /= NColumns ) then ! there is no matrix: each parameter has different numbers of elements
            FoundMatrix  =   .False.
            if (Dbg) call Logger%Write( "-> Rows have different number of element => No matrix can be extracted" )
            exit
          end if
        end if
        Values(i,:)    =   Vector
      end do
    else
      FoundMatrix  =   .False.
    end if
  end if

  nullify( TargetSection )

  if ( .Not. FoundMatrix ) then      ! If the matrix has not been found
    if (Dbg) call Logger%Write( "No matrix have been extracted from the current section" )
    if (Dbg) call Logger%Write( "=> Deallocating the output variable" )
    if ( allocated(Values) )  deallocate( Values )
    if ( GetOptArgValue(.True.,Mandatory) ) then      ! ... and if it is mandatory, then error
      call Logger%Write( "... and since this matrix is mandatory => Error" )
      error stop    !       call Error_Mandatory_Parameter_Not_Found( Param, CallProc=ProcName )
    else
      if ( present(DefaultValues) ) then
        call Logger%Write( "There is a default matrix" )
        allocate( Values, source = DefaultValues )
      end if
    end if
  end if

  if ( present(Found) )   Found   = FoundMatrix
  if ( present(Defined) ) Defined = allocated(Values)

  if (Dbg) call Logger%Exiting()

End Procedure


Module Procedure GetRawValueFromIndex
  character(*)                                              ,parameter  ::  ProcName = "GetRawValueFromIndex"
  logical                                                               ::  Dbg
  type(InputParameter_Type)                                             ::  Param                           ! Parameter object
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  Raw   =   ""
  if (Dbg) call Logger%Write( "Extracting parameter: Index = ", ParameterIndex )
  if (Dbg) call Logger%Write( "-> Calling This%GetParameter" )
  Param   =   This%GetParameter( ParameterIndex       &
                , FromSubSection  =   FromSubSection  &
                , Mandatory       =   Mandatory       &
                , Found           =   Found           &
                , Debug           =   Debug           )
  if (Dbg) call Logger%Write( "-> Param%Defined = ", Param%Defined )
  if ( Param%Defined ) Raw = Param%GetRaw()
  if (Dbg) call Logger%Write( "-> Raw = ", Raw )
  if (Dbg) call Logger%Exiting()
End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
!                                       PROCEDURES RELATED TO ERRORS
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Error_Mandatory_Parameter_Not_Found( Param, CallProc )
  use Error_Class    ,only:  Error
  type(InputParameter_Type)                             ,intent(in)     ::  Param
  character(*)                                ,optional ,intent(in)     ::  CallProc
  character(*)                                              ,parameter  ::  ProcName = "Error_Mandatory_Parameter_Not_Found"
  character(:)  ,allocatable                                            ::  ProcPath
  ProcPath      =   Logger%GetPath()
  call Error%Set_Title( "Error while trying to extract a section" )
  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  A mandatory parameter is not found." )
  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  Potential solutions for this problem are:" )
  call Error%Add_Line( "  * Check the name of the parameter to be extracted," )
  call Error%Add_Line( "  * Check the section where the parameter is being extracted," )
  call Error%Add_Line( "  * Check whether the parameter really needs to be mandatory." )
  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:          " // CallProc )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:             " // ProcPath )
  if ( allocated(Param%ParentSection) )   call Error%Add_Line( "  Parent section:             " // Param%ParentSection      )
                                          call Error%Add_Line( "  Parameter name:             " // Param%Name )
  if ( Param%Properties%HasDefaultValue ) call Error%Add_Line( "  Parameter default value:    " // Param%Properties%DefaultValue )
  call Error%Raise( Unit=Logger%GetUnit() )
End Subroutine

End SubModule