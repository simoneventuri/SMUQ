Module InputSection_Class

! @TODO: Remove all occurence of "GetSubSectionIndex" and replace with "GetSectionPointer"
! @TODO: Always set parameter name using call Param%SetName(Name) so that the name of the Properties is also set
! @TODO: use ProcessCaseSensitiveString when applicable
! @TODO: For procedure GetValueFromName_CHAR_0d and 'GetValueFromName_CHAR_0d' add the input argument: LowerCase, UpperCase
! @TODO: Add the following optional input arguments to the "GetValueFromName_CHAR_1d" and "GetValueFromProp_CHAR_1d" procedures
!    * RemoveEmptyElements          logical     Indicator whether empty elements should be removed
!    * RemoveDuplicateElements      logical     Indicator whether empty elements should be removed
!    * AllowedElements              character   List of allowed elements

  use Logger_Class                ,only:  Logger_Type
  use InputParameter_Class        ,only:  InputParameter_Type
  use InputParamProperties_Class  ,only:  InputParamProperties_Type
  use String_Library              ,only:  String_Type
  use Status_Library              ,only:  Status_Type, UpdateStatus
  use iso_fortran_env             ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

  implicit none

  private
  public  ::  InputSection_Type
  public  ::  Construct_Sections
  public  ::  ConstructSection
  public  ::  GetSubsectionFromSection_WORKAROUND

  Type                                      ::  InputSection_Type
    logical                                 ::  Mandatory       =       .False.
    logical                                 ::  Empty           =       .True.
    logical                                 ::  Defined         =       .False.
    integer                                 ::  NSections       =       0                       ! Number of sections
    integer                                 ::  NParameters     =       0                       ! Number of parameters
    character(:)  ,allocatable  ,private    ::  Name                                            ! Name of current section
    type(InputParameter_Type) ,allocatable  ::  Parameters(:)                                      ! Arrays of Parameter objects
#     ifdef SUPPORTED_RECURSIVE_ALLOCATABLE_DERIVEDTYPE
        type(InputSection_Type), allocatable  ::  Sections(:) 
#     else
        type(InputSection_Type), dimension(:), pointer  ::  Sections=>null()
#     endif
  contains
! # ifndef GCC_COMPILER
    Final                       ::  FinalizeSection
! # endif
    generic     ,public         ::  Initialize    =>  InitializeSectionFromLines, InitializeSectionFromCharacters, InitializeSectionFromSectionsAndParameters, InitializeSectionFromParam
    procedure   ,public         ::  Free          =>  FreeSection
    procedure   ,public         ::  ExpandFunctionsToSections
    procedure   ,public         ::  ProcessMacroInSections
    procedure   ,public         ::  ProcessEnvironmentVariables
!     procedure   ,public         ::  HasMacro
    procedure   ,public         ::  GetName       =>  GetSectionName
    procedure   ,public         ::  SetName       =>  SetSectionName
    generic     ,public         ::  GetParameter  =>  GetParameterFromIndex, GetParameterFromName, GetParameterFromProperties, GetParameterFromNameProperties
    generic     ,public         ::  GetParameters =>  GetParametersFromSection
    generic     ,public         ::  AddParameter  =>  AddParameterFromProperties, AddParameterFromNameValue, AddParameterFromParameter, AddParameterFromParameters  ! , AddParameterFromRaw
    generic     ,public         ::  MoveParameter =>  MoveParameterFromTo
    generic     ,public         ::  Write         =>  WriteSectionToString, WriteSectionToFileFromFileUnit, WriteSectionToFileFromFileName, WriteSectionToLogger
    generic     ,public         ::  Output        =>  OutputSectionToFile, OutputSectionToString
    procedure   ,public         ::  GetSection    =>  GetSubsectionFromSection
    procedure   ,public         ::  GetSections   =>  GetSubsectionsFromSection
    procedure   ,public         ::  GetFunctionSection
    procedure   ,public         ::  GetFunctionSections
    generic     ,public         ::  GetRawValue   =>  GetRawValueFromIndex !, GetRawValueFromName, GetRawValueFromProperties, GetRawValueFromNameProperties
    generic     ,public         ::  Get_Matrix    =>  GetValue_REAL64_d2
    generic     ,public         ::  GetValue      =>  GetValueFromProp_LOG_0d, GetValueFromProp_CHAR_0d, &
                                                      GetValueFromProp_INT8_0d, GetValueFromProp_INT16_0d, GetValueFromProp_INT32_0d, GetValueFromProp_INT64_0d   , &
                                                      GetValueFromProp_REAL32_0d, GetValueFromProp_REAL64_0d, GetValueFromProp_REAL128_0d  , &!GetValueFromProp_REAL64_0d_Alloc,  &
                                                      GetValueFromProp_LOG_1d, GetValueFromProp_CHAR_1d, &
                                                      GetValueFromProp_INT8_1d, GetValueFromProp_INT16_1d, GetValueFromProp_INT32_1d, GetValueFromProp_INT64_1d   , &
                                                      GetValueFromProp_REAL32_1d, GetValueFromProp_REAL64_1d, GetValueFromProp_REAL128_1d , &
                                                      GetValueFromName_LOG_0d, GetValueFromName_CHAR_0d, &!GetValueFromName_REAL64_0d_Alloc,  &
                                                      GetValueFromName_INT8_0d, GetValueFromName_INT16_0d, GetValueFromName_INT32_0d, GetValueFromName_INT64_0d   , &
                                                      GetValueFromName_REAL32_0d, GetValueFromName_REAL64_0d, GetValueFromName_REAL128_0d  , &
                                                      GetValueFromName_LOG_1d, GetValueFromName_CHAR_1d, &
                                                      GetValueFromName_INT8_1d, GetValueFromName_INT16_1d, GetValueFromName_INT32_1d, GetValueFromName_INT64_1d   , &
                                                      GetValueFromName_REAL32_1d, GetValueFromName_REAL64_1d, GetValueFromName_REAL128_1d
                                                    !,  GetValueFromProperties, GetValueFromProp_CHAR_1d, GetValueFromName_CHAR_0d
    generic     ,public         ::  HasParameter  =>  SectionHasParameterFromName, SectionHasParameterFromProperties  ! Indicator whether a section contains a given parameter
    generic     ,public         ::  assignment(=) =>  AssignSectionFromSection    !@COMPILER_BUG:ifort-19.0.1: Required due to seg. fault when assigning Section obejcts with ifort-19.0.1
    procedure   ,private        ::  AssignSectionFromSection
    procedure   ,private        ::  InitializeSectionFromLines
    procedure   ,private        ::  InitializeSectionFromCharacters
    procedure   ,private        ::  InitializeSectionFromSectionsAndParameters
    procedure   ,private        ::  InitializeSectionFromParam
    procedure   ,public         ::  CreateSubSections
    procedure   ,public         ::  GetSectionPointer
!   Output
    procedure   ,private        ::  WriteSectionToString
    procedure   ,private        ::  WriteSectionToFileFromFileUnit
    procedure   ,private        ::  WriteSectionToFileFromFileName
    procedure   ,private        ::  WriteSectionToLogger
    procedure   ,private        ::  OutputSectionToFile
    procedure   ,private        ::  OutputSectionToString
!   Properties
    procedure   ,public         ::  HasSection
    procedure   ,public         ::  FindTargetSection
    procedure   ,public         ::  GetNumberOfSubSections
    procedure   ,private        ::  GetMaxLengthParameterName
    procedure   ,private        ::  GetMaxLengthParameterValue
    procedure   ,private        ::  GetSubSectionIndex        ! TO BE REMOVED AND REPLACED BY "GetSectionPointer" ???
!   Properties: Parameter
    procedure   ,public         ::  GetNumberOfParameters         !@TODO Rename  GetNumberOfParam
    procedure   ,public         ::  GetParameterIndex
    procedure   ,private        ::  GetParameterFromIndex
    procedure   ,private        ::  GetParameterFromName
    procedure   ,private        ::  GetParameterFromProperties
    procedure   ,private        ::  GetParameterFromNameProperties
    procedure   ,private        ::  GetParametersFromSection
    procedure   ,private        ::  GetRawValueFromIndex
!     procedure   ,private        ::  GetRawValueFromName
!     procedure   ,private        ::  GetRawValueFromProperties
!     procedure   ,private        ::  GetRawValueFromNameProperties
!     procedure   ,private        ::  GetRawValuesFromSection
!   Properties: Section
    procedure   ,private        ::  SectionHasParameterFromName
    procedure   ,private        ::  SectionHasParameterFromProperties
!   Procedures for adding sections: InputSection_AddSection_SubClass
    generic     ,public         ::  AddSection              =>  AddSectionFromSection, AddSectionFromSectionName, AddSectionsFromSections
    procedure   ,private        ::  AddSectionFromSection
    procedure   ,private        ::  AddSectionFromSectionName
    procedure   ,private        ::  AddSectionsFromSections
    procedure   ,public         ::  AddCommandLineArguments
!   Procedures for adding parameters: InputSection_AddParameter_SubClass
    procedure   ,private        ::  AddParameterFromProperties
    procedure   ,private        ::  AddParameterFromNameValue
!     procedure   ,private        ::  AddParameterFromRaw
    procedure   ,private        ::  AddParameterFromParameter
    procedure   ,private        ::  AddParameterFromParameters
    procedure   ,private        ::  MoveParameterFromTo
!   Procedures for removing an item: InputSection_RemoveItem_SubClass
    procedure   ,public         ::  RemoveItems
    procedure   ,public         ::  RemoveParameters
    procedure   ,public         ::  RemoveSections
! !   Procedures for extracting a value: InputSection_GetValue_SubClass
    procedure   ,private        ::  GetValueFromProp_LOG_0d, GetValueFromProp_CHAR_0d
    procedure   ,private        ::  GetValueFromProp_INT8_0d, GetValueFromProp_INT16_0d, GetValueFromProp_INT32_0d, GetValueFromProp_INT64_0d
    procedure   ,private        ::  GetValueFromProp_REAL32_0d, GetValueFromProp_REAL64_0d, GetValueFromProp_REAL128_0d !, GetValueFromProp_REAL64_0d_Alloc
    procedure   ,private        ::  GetValueFromProp_LOG_1d, GetValueFromProp_CHAR_1d
    procedure   ,private        ::  GetValueFromProp_INT8_1d, GetValueFromProp_INT16_1d, GetValueFromProp_INT32_1d, GetValueFromProp_INT64_1d
    procedure   ,private        ::  GetValueFromProp_REAL32_1d, GetValueFromProp_REAL64_1d, GetValueFromProp_REAL128_1d
    procedure   ,private        ::  GetValueFromName_LOG_0d, GetValueFromName_CHAR_0d
    procedure   ,private        ::  GetValueFromName_INT8_0d, GetValueFromName_INT16_0d, GetValueFromName_INT32_0d, GetValueFromName_INT64_0d
    procedure   ,private        ::  GetValueFromName_REAL32_0d, GetValueFromName_REAL64_0d, GetValueFromName_REAL128_0d   ! GetValueFromName_REAL64_0d_Alloc
    procedure   ,private        ::  GetValueFromName_LOG_1d, GetValueFromName_CHAR_1d
    procedure   ,private        ::  GetValueFromName_INT8_1d, GetValueFromName_INT16_1d, GetValueFromName_INT32_1d, GetValueFromName_INT64_1d
    procedure   ,private        ::  GetValueFromName_REAL32_1d, GetValueFromName_REAL64_1d, GetValueFromName_REAL128_1d
    procedure   ,private        ::  GetValue_REAL64_d2
!     procedure   ,public         ::  WriteSectionUDIO
!     generic     ,public         ::  write(formatted)        =>  WriteSectionUDIO
  End Type

  Interface           ConstructSection
    Module Procedure  ConstructSectionFromCharac
    Module Procedure  ConstructSectionFromName
    Module Procedure  ConstructSectionFromParameter
  End Interface

  Interface

    Module Subroutine Construct_Sections( Lines, Sections, Parameters, FileName, Debug )
      type(String_Type)                                     ,intent(in)     ::  Lines(:)
      type(InputSection_Type)   ,allocatable                ,intent(out)    ::  Sections(:)
      type(InputParameter_Type) ,allocatable                ,intent(out)    ::  Parameters(:)
      character(*)                                ,optional ,intent(in)     ::  FileName                        !< Name of the file containing this section (Only used to print a nice error message if something goes wrong)
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetSubsectionFromSection_WORKAROUND( TargetSection, This, SectionName, FromSubSection, Mandatory, Found, CallProc, Section_Index, Debug )
      type(InputSection_Type)                               ,intent(out)    ::  TargetSection
      class(InputSection_Type)   ,target                    ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Section object a sub-section need to be extracted
      character(*)                                          ,intent(in)     ::  SectionName                     !< Name of the section to be extracted from the input Section object (The passed-object dummy argument)
      character(*)                                ,optional ,intent(in)     ::  FromSubSection                  !< Name (Eventually nested names) of the sub-section from which the target section has to be extracted from
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the target section is mandatory
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the target section has been found
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      integer                                     ,optional ,intent(out)    ::  Section_Index                   !< Index of the subsection
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

  End Interface

  ! **************************************************************************************************************
  !                                       INITIALIZATION PROCEDURES
  ! **************************************************************************************************************
  Interface

    Module Function ConstructSectionFromCharac( Charac, LogLevel ) result(Section)
      character(*)  ,dimension(:)                           ,intent(in)     ::  Charac                          !< Name of the section to be constructed
      integer                             ,target ,optional ,intent(in)     ::  LogLevel                        !< Indicator of the log level
      type(InputSection_Type)                                               ::  Section                         !< Section object to be constructed
    End Function

    Module Pure Elemental Function ConstructSectionFromName( Name ) result(Section)
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the section to be constructed
      type(InputSection_Type)                                               ::  Section                         !< Section object to be constructed
    End Function

    Module Function ConstructSectionFromParameter( Param, DefArgNames, FctSep, ArgSep, ValSep, DefToVal, Debug ) result(Section)
      type(InputParameter_Type)                             ,intent(in)     ::  Param
      character(*)                                ,optional ,intent(in)     ::  DefArgNames(:)
      character(*)                                ,optional ,intent(in)     ::  FctSep
      character(*)                                ,optional ,intent(in)     ::  ArgSep
      character(*)                                ,optional ,intent(in)     ::  ValSep
      logical                                     ,optional ,intent(in)     ::  DefToVal
      logical                                     ,optional ,intent(in)     ::  Debug
      type(InputSection_Type)                                               ::  Section
    End Function

    Module Recursive Subroutine ProcessMacroInSections( This, Params, Debug )
      class(InputSection_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument
      type(InputSection_Type)                     ,optional ,intent(in)     ::  Params
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Recursive Subroutine ProcessEnvironmentVariables( This, Debug )
      class(InputSection_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
!
!     Module Recursive Pure Function HasMacro( This ) result(Indicator)
!       class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
!       logical                                                               ::  Indicator
!     End Function

    Module Subroutine ExpandFunctionsToSections( This, Debug )
      class(InputSection_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine InitializeSectionFromLines( This, Name, Lines, FileName, Debug )
      class(InputSection_Type)                              ,intent(out)    ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the section to be constructed
      type(String_Type)     ,dimension(:)                   ,intent(in)     ::  Lines                           !< Lines associated to the section to be constructed
      character(*)                                ,optional ,intent(in)     ::  FileName                        !< Name of the file containing this section (Only used to print a nice error message if something goes wrong)
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine InitializeSectionFromCharacters( This, Name, Charac, FileName, Debug )
      class(InputSection_Type)                              ,intent(out)    ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the section to be constructed
      character(*)  ,dimension(:)                           ,intent(in)     ::  Charac                           !< Lines associated to the section to be constructed
      character(*)                                ,optional ,intent(in)     ::  FileName                        !< Name of the file containing this section (Only used to print a nice error message if something goes wrong)
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
!
    Module Subroutine InitializeSectionFromSectionsAndParameters( This, Name, Sections, Parameters, Debug )
      class(InputSection_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the section to be constructed
      type(InputSection_Type)    ,dimension(:)              ,intent(in)     ::  Sections                        !< Arrays of Section objects to be stored in the "Sections" componetns of the Section object being contructed
      type(InputParameter_Type)  ,dimension(:)              ,intent(in)     ::  Parameters                      !< Arrays of Parameter objects to be stored in the "Parameters" componetns of the Section object being contructed
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
!
    Module Subroutine InitializeSectionFromParam( This, Param, Debug )
      class(InputSection_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument
      type(InputParameter_Type)                             ,intent(in)     ::  Param
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
!
    Pure Recursive Module Subroutine FreeSection( This )
      class(InputSection_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Section object to be finalized
    End Subroutine

!     Pure Recursive Module Subroutine FinalizeSection( This )
    Pure Recursive Module Subroutine FinalizeSection( This )
      type(InputSection_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Section object to be finalized
    End Subroutine
!
    Pure Module Subroutine SetSectionName( This, SectionName )
      class(InputSection_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  SectionName                     !< Name of the section
    End Subroutine

    Recursive Module Subroutine CreateSubSections( This, SubSections, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  SubSections                     !< Name of the section to be added
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Recursive Module Subroutine GetSectionPointer( This, SubSectionName, TargetSection, CaseSensitive, Debug )
      class(InputSection_Type)   ,target                    ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Section object where a sub-section need to be added
      character(*)                                          ,intent(in)     ::  SubSectionName                  !< Name of the sub-section where Section object has to be added
      type(InputSection_Type)    ,pointer                   ,intent(out)    ::  TargetSection                   !< Pointer to the sub-section
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the section name is case sensitive (@TODO: This property should be stored in the object itself)
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

!     Pure
    Recursive Module Subroutine AssignSectionFromSection( lhs, rhs )
!       class(InputSection_Type)                              ,intent(inout)  ::  lhs
      class(InputSection_Type)                              ,intent(out)  ::  lhs
      type(InputSection_Type)                               ,intent(in)     ::  rhs
    End Subroutine

  End Interface

  ! **************************************************************************************************************
  !                                       Properties
  ! **************************************************************************************************************
  Interface

    Pure Module Function GetSectionName( This ) result(SectionName)
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  SectionName                     !< Name of the section
    End Function

!     @TODO: Add a procedure which takes in input an array of SubSectionName and return and array of SectionFound of the same size
    Recursive Module Function HasSection( This, SubSectionName, CaseSensitive, Debug ) result(SectionFound)
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  SubSectionName                  !< Name of the sub-section to be found in current section
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the section name is case sensitive (@TODO: This property should be stored in the object itself)
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      logical                                                               ::  SectionFound                    !< Indicator whether the subsection was found
    End Function

    Module Subroutine FindTargetSection( This, TargetSection, FromSubSection, FoundSection, IsRootSection, Mandatory, CallProc, Debug )
      class(InputSection_Type)                    ,target   ,intent(in)     ::  This                            !< Passed-object dummy argument
      type(InputSection_Type)                     ,pointer  ,intent(out)    ::  TargetSection                   !< Section object corresponding to the target section
      character(*)                                ,optional ,intent(in)     ::  FromSubSection                  !< Name (Eventually nested names) of the sub-section where the removal is to be performed
      logical                                     ,optional ,intent(out)    ::  FoundSection                    !< Indicator whether a section (either the extraction or target section) has been found
      logical                                     ,optional ,intent(out)    ::  IsRootSection                   !< Indicator whether the target section corresponds to the root section
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the target section is mandatory
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
!
    Pure Elemental Module Function GetNumberOfSubSections( This, Name ) result(NSections)
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  Name                            !< Name of the sub-sections to be considered
      integer                                                               ::  NSections                       !< Number of sub-sections (either all, or the ones with a given name)
    End Function

    Pure Module Function GetMaxLengthParameterName( This ) result(Length)
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                                               ::  Length
    End Function

    Pure Module Function GetMaxLengthParameterValue( This ) result(Length)
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                                               ::  Length
    End Function

    Module Function GetSubSectionIndex( This, SectionName, CaseSensitive, Section_Found, Mandatory, Debug ) result(SectionIndex)
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  SectionName                     !< Name of the section whose index is to be returned
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the section name is case sensitive (@TODO: This property should be stored in the object itself)
      logical                                     ,optional ,intent(out)    ::  Section_Found                   !< Indicator whether the subsection was found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      integer                                                               ::  SectionIndex                    !< Index of the sub-section whose name matches the "SectionName" name. Return 0 is no match
    End Function

!     !@TODO: Add  a procedure which kate an array of section names "SectionNames" and return a single section
!             So that we can gert a section whose name matches several keys
!             In spark: @TODO:GetSubsectionFromSectionS
    Module Function GetSubsectionFromSection( This, SectionName, FromSubSection, Mandatory, Found, CallProc, Section_Index, Debug ) result(TargetSection)
      class(InputSection_Type)   ,target                    ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Section object a sub-section need to be extracted
      character(*)                                          ,intent(in)     ::  SectionName                     !< Name of the section to be extracted from the input Section object (The passed-object dummy argument)
      character(*)                                ,optional ,intent(in)     ::  FromSubSection                  !< Name (Eventually nested names) of the sub-section from which the target section has to be extracted from
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the target section is mandatory
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the target section has been found
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      integer                                     ,optional ,intent(out)    ::  Section_Index                   !< Index of the subsection
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      type(InputSection_Type)                                               ::  TargetSection
    End Function

    Module Subroutine GetSubsectionsFromSection( This, Sections, SectionName, FromSubSection, Mandatory, Found, CallProc, Debug )
      class(InputSection_Type)   ,target                    ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Section object a sub-section need to be extracted
      type(InputSection_Type) ,allocatable  ,dimension(:)   ,intent(out)    ::  Sections                        !< List of extracted Section objects
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section to be extracted from the input Section object (The passed-object dummy argument)
      character(*)                                ,optional ,intent(in)     ::  FromSubSection                  !< Name (Eventually nested names) of the sub-section from which the target sections have to be extracted from
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the target section is mandatory
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the target section has been found
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Function GetFunctionSection( This, FunctionName, SectionName, CaseSensitive, Mandatory, Found, CallProc, Status, Debug ) result(TargetSection)
      class(InputSection_Type)   ,target                    ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Section object a sub-section need to be extracted
      character(*)                                          ,intent(in)     ::  FunctionName                    !< Name of the section to be extracted from the input Section object (The passed-object dummy argument)
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name (Eventually nested names) of the sub-section from which the target section has to be extracted from
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the function name is case sensitive
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the target section is mandatory
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the target section has been found
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      type(InputSection_Type)                                               ::  TargetSection
    End Function

    Module Function GetFunctionSections( This, FunctionName, SectionName, DefArgNames, CaseSensitive, Mandatory, Found, CallProc, Status, Debug ) result(TargetSections)
      class(InputSection_Type)   ,target                    ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Section object a sub-section need to be extracted
      character(*)                                          ,intent(in)     ::  FunctionName                    !< Name of the section to be extracted from the input Section object (The passed-object dummy argument)
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name (Eventually nested names) of the sub-section from which the target section has to be extracted from
      character(*)                                ,optional ,intent(in)     ::  DefArgNames(:)                  !< Default argument names
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the function name is case sensitive
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the target section is mandatory
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the target section has been found
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      type(InputSection_Type) ,allocatable                                  ::  TargetSections(:)
    End Function


  End Interface

  ! **************************************************************************************************************
  !                                       Properties
  ! **************************************************************************************************************
  Interface

    Module Function GetNumberOfParameters( This, FromSubSection, CallProc, Debug ) result(NParameters)
      class(InputSection_Type)   ,target                    ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Section object a sub-section need to be extracted
      character(*)                                ,optional ,intent(in)     ::  FromSubSection                  !< Name (Eventually nested names) of the sub-section whose number of parameters has to be returned
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      integer                                                               ::  NParameters
    End Function

!   @TODO: Add a 'FromSubSection' inoput argument to extract the parameter index from a given sub-section
    Module Function GetParameterIndex( This, Properties, SectionName, CallProc, Found, Defined, Status, Debug ) result(iParam)
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value) (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      integer                                                               ::  iParam
    End Function

    Recursive Module Function GetParameterFromIndex( This, ParameterIndex, FromSubSection, Mandatory, Found, Debug ) result(Param)
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  ParameterIndex                  !< Index of the target parameter
      character(*)                                ,optional ,intent(in)     ::  FromSubSection                  !< Name of the section where to search for the target parameter (if absent, searching in current section only)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameterr is mandatory
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether some parameterrs have been found
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      type(InputParameter_Type)                                             ::  Param                           !< Parameter object corresponding to the target parameter if it has been found or to an empty parameter
    End Function

    Recursive Module Function GetParameterFromName( This, ParameterName, SectionName, CaseSensitive, Mandatory, Debug ) result(Param)
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the target parameter
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section where to search for the target parameter (if absent, searching in current section only)
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the parameter name is case sensitive
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameterr is mandatory
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      type(InputParameter_Type)                                             ::  Param                           !< Parameter object corresponding to the target parameter if it has been found or to an empty parameter
    End Function

    Recursive Module Function GetParameterFromProperties( This, Properties, SectionName, Mandatory, Debug ) result(Param)
      class(InputSection_Type)  ,target                     ,intent(in)     ::  This                            !< Passed-object dummy argument
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section where to search for the target parameter (if absent, searching in current section only)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameterr is mandatory
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      type(InputParameter_Type)                                             ::  Param                           !< Parameter object corresponding to the target parameter if it has been found or to an empty parameter
    End Function

    Recursive Module Function GetParameterFromNameProperties( This, Name, Properties, SectionName, Mandatory, Debug ) result(Param)
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the target parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section where to search for the target parameter (if absent, searching in current section only)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameterr is mandatory
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      type(InputParameter_Type)                                             ::  Param                           !< Parameter object corresponding to the target parameter if it has been found or to an empty parameter
    End Function

    Module Subroutine GetParametersFromSection( This, Parameters, FromSubSection, Mandatory, Found, Debug )
      class(InputSection_Type)  ,target                     ,intent(in)     ::  This                            !< Passed-object dummy argument
      type(InputParameter_Type) ,allocatable  ,dimension(:) ,intent(out)    ::  Parameters                      !< Parameter objects extracted from the target section
      character(*)                                ,optional ,intent(in)     ::  FromSubSection                  !< Name of the section where to search for the target parameter (if absent, searching in current section only)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameterr is mandatory
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether some parameterrs have been found
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

  End Interface

!   ! **************************************************************************************************************
!   !                                       Properties: Section
!   ! **************************************************************************************************************
  Interface

    Recursive Module Function SectionHasParameterFromProperties( This, Properties, SectionName, CaseSensitive, Debug ) result( FoundParameter )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section where to search for the target parameter (if absent, searching in current section only)
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the parameter name is case sensitive
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      logical                                                               ::  FoundParameter                  !< Parameter object corresponding to the target parameter if it has been found or to an empty parameter
    End Function

    Recursive Module Function SectionHasParameterFromName( This, ParameterName, SectionName, CaseSensitive, Debug ) result( FoundParameter )
      class(InputSection_Type)  ,target                     ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the target parameter
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section where to search for the target parameter (if absent, searching in current section only)
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the parameter name is case sensitive
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      logical                                                               ::  FoundParameter                  !< Parameter object corresponding to the target parameter if it has been found or to an empty parameter
    End Function
!
  End Interface

  ! **************************************************************************************************************
  !                               PROCEDURES FOR EXTRACTING SUB-SECTIONS FROM A SECTION
  ! **************************************************************************************************************
  Interface
    Module Subroutine AddSectionToSectionsFromSectionObject( Sections, NewSection )
      type(InputSection_Type) RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ATTRIBUTE            ,intent(inout)  ::  Sections(:)
      type(InputSection_Type)                               ,intent(in)     ::  NewSection
    End Subroutine


    Module Subroutine AddSectionToSectionsFromSectionName( Sections, NewSectionName )
      type(InputSection_Type) RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ATTRIBUTE            ,intent(inout)  ::  Sections(:)
      character(*)                                          ,intent(in)     ::  NewSectionName
    End Subroutine

    Module Subroutine AddSectionsToSections( Sections, NewSections )
      type(InputSection_Type) RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ATTRIBUTE            ,intent(inout)  ::  Sections(:)
      type(InputSection_Type)                               ,intent(in)     ::  NewSections(:)
    End Subroutine

  End Interface

! **************************************************************************************************************
!                             PROCEDURES FOR ADDING SECTIONS TO A SECTION
! **************************************************************************************************************
  Interface

    Module Subroutine AddCommandLineArguments( This, To_SubSection, LogLevel )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  To_SubSection                   !< Name (Eventually nested names) of the sub-section where Section object has to be added
      integer                                     ,optional ,intent(in)     ::  LogLevel                        !< Indicator of the log level
    End Subroutine

    Recursive Module Subroutine AddSectionFromSection( This, Section, To_SubSection, CallProc, Action, OnlyContent, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Section object where a sub-section need to be added
      type(InputSection_Type)                               ,intent(in)     ::  Section                         !< Section object to be added
      character(*)                                ,optional ,intent(in)     ::  To_SubSection                   !< Name (Eventually nested names) of the sub-section where Section object has to be added
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      character(*)                                ,optional ,intent(in)     ::  Action                          !< Action to be performed when adding the element
      logical                                     ,optional ,intent(in)     ::  OnlyContent                     !< Indicator that only the "internal content" of the Section should be copied, not the parent section
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine AddSectionsFromSections( This, Sections, To_SubSection, CallProc, Action, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Section object where a sub-section need to be added
      type(InputSection_Type) ,dimension(:)                 ,intent(in)     ::  Sections                        !< Section objects to be added
      character(*)                                ,optional ,intent(in)     ::  To_SubSection                   !< Name (Eventually nested names) of the sub-section where Section object has to be added
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      character(*)                                ,optional ,intent(in)     ::  Action                          !< Action to be performed when adding the element
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine AddSectionFromSectionName( This, SectionName, To_SubSection, CallProc, Action, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Section object where a sub-section need to be added
      character(*)                                          ,intent(in)     ::  SectionName                     !< Name of the section to be added
      character(*)                                ,optional ,intent(in)     ::  To_SubSection                   !< Name of the sub-section where Section object has to be added
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      character(*)                                ,optional ,intent(in)     ::  Action                          !< Action to be performed when adding the section
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

  End Interface


! **************************************************************************************************************
!                            PROCEDURES FOR ADDING PARAMETERS TO A SECTION
! **************************************************************************************************************
  Interface

    Module Subroutine AddParameterFromProperties( This, Properties, Value, SectionName, Action, AtStart, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object associated to the parameter to be added
      character(*)                                          ,intent(in)     ::  Value                           !< Character string corresponding to the value of the parameter to be added
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section where to add the parameter
      character(*)                                ,optional ,intent(in)     ::  Action                          !< Action to be performed when adding the element
      logical                                     ,optional ,intent(in)     ::  AtStart
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine AddParameterFromNameValue( This, Name, Value, SectionName, Action, AtStart, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Name                            !< Name of the parameter to be added
      character(*)                                          ,intent(in)     ::  Value                           !< Character string corresponding to the value of the parameter to be added
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section where to add the parameter
      character(*)                                ,optional ,intent(in)     ::  Action                          !< Action to be performed when adding the element
      logical                                     ,optional ,intent(in)     ::  AtStart
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

!     Module Subroutine AddParameterFromRaw( This, Raw, DummyArg, SectionName, Action, Debug )
!       class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
!       character(*)                                          ,intent(in)     ::  Raw
!       logical                                     ,optional ,intent(in)     ::  DummyArg
!       character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section where to add the parameter
!       character(*)                                ,optional ,intent(in)     ::  Action                          !< Action to be performed when adding the element
!       logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
!     End Subroutine

    Module Subroutine AddParameterFromParameter( This, Param, SectionName, Action, OnlyIfDefined, HasBeenAdded, AtStart, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      type(InputParameter_Type)                             ,intent(in)     ::  Param                           !< Parameter object to be added
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section where to add the parameter
      character(*)                                ,optional ,intent(in)     ::  Action                          !< Action to be performed when adding the element
      logical                                     ,optional ,intent(in)     ::  OnlyIfDefined                   !< Indicator that the parameter should only be added if it is defined
      logical                                     ,optional ,intent(out)    ::  HasBeenAdded                    !< Indicator that the parameter has been added
      logical                                     ,optional ,intent(in)     ::  AtStart
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine AddParameterFromParameters( This, Params, SectionName, Action, OnlyIfDefined, AtStart, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      type(InputParameter_Type)  ,dimension(:)              ,intent(in)     ::  Params                          !< Parameter objects to be added
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the section where to add the parameter
      character(*)                                ,optional ,intent(in)     ::  Action                          !< Action to be performed when adding the element
      logical                                     ,optional ,intent(in)     ::  OnlyIfDefined                   !< Indicator that the parameter should only be added if it is defined
      logical                                     ,optional ,intent(in)     ::  AtStart
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine MoveParameterFromTo( This, From, To, Action, Remove, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  From                            !< Name and location of the parameter to be moved: SectionNames>ParamName
      character(*)                                          ,intent(in)     ::  To                              !< Name and location of the parameter after moving it: SectionNames>ParamName
      character(*)                                ,optional ,intent(in)     ::  Action                          !< Action to be performed when adding the parameter in its new section
      logical                                     ,optional ,intent(in)     ::  Remove                          !< Indicator whether the input parameter "From" have to be removed
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

  End Interface

! **************************************************************************************************************
!                            PROCEDURES FOR OUTPUTING SECTION PROPERTIES
! **************************************************************************************************************
  Interface

    Recursive Module Subroutine WriteSectionToString( This, Lines, Indent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth, MaxLines, MaxParam )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable  ,dimension(:)             ,intent(out)    ::  Lines                           !< Character array where to write the Section object
      integer                                     ,optional ,intent(in)     ::  Indent                          !< Initial indentation level. Default: 0
      logical                                     ,optional ,intent(in)     ::  Align                           !< Indicator whether parameter should be align together inside each sections. Default: True
      logical                                     ,optional ,intent(in)     ::  AlignAll                        !< Indicator whether parameter should be align together inside all sections. Default: False
      logical                                     ,optional ,intent(in)     ::  ShowEmpty                       !< Indicator whether empty sections should be written. Default: True
      character(*)                                ,optional ,intent(in)     ::  Include(:)                      !< List of section names which should inclued in the output. Default: none
      character(*)                                ,optional ,intent(in)     ::  Exclude(:)                      !< List of section names which should excluded from the output. Default: none
      integer                                     ,optional ,intent(in)     ::  Depth                           !< Maximum depth of recursive sub-section to bw considered for the output. Default: none
      integer                                     ,optional ,intent(in)     ::  MaxLines                        !< Maximum number of lines to be printed: Default: 1000
      integer                                     ,optional ,intent(in)     ::  MaxParam                        !< Maximum number of parameters per sections: Default: unlimited
    End Subroutine

    Recursive Module Subroutine WriteSectionToFileFromFileUnit( This, FileUnit, Indent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth, MaxLines, MaxParam )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  FileUnit                        !< FileUnit number where to write the Section object
      integer                                     ,optional ,intent(in)     ::  Indent                          !< Initial indentation level. Default: 0
      logical                                     ,optional ,intent(in)     ::  Align                           !< Indicator whether parameter should be align together inside each sections. Default: True
      logical                                     ,optional ,intent(in)     ::  AlignAll                        !< Indicator whether parameter should be align together inside all sections. Default: False
      logical                                     ,optional ,intent(in)     ::  ShowEmpty                       !< Indicator whether empty sections should be written. Default: True
      character(*)                                ,optional ,intent(in)     ::  Include(:)                      !< List of section names which should inclued in the output. Default: none
      character(*)                                ,optional ,intent(in)     ::  Exclude(:)                      !< List of section names which should excluded from the output. Default: none
      integer                                     ,optional ,intent(in)     ::  Depth                           !< Maximum depth of recursive sub-section to bw considered for the output. Default: none
      integer                                     ,optional ,intent(in)     ::  MaxLines                        !< Maximum number of lines to be printed: Default: 1000
      integer                                     ,optional ,intent(in)     ::  MaxParam                        !< Maximum number of parameters per sections: Default: unlimited
    End Subroutine

    Recursive Module Subroutine WriteSectionToFileFromFileName( This, FileName, Indent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth, MaxLines, MaxParam )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  FileName                        !< File name where to write the Section object
      integer                                     ,optional ,intent(in)     ::  Indent                          !< Initial indentation level. Default: 0
      logical                                     ,optional ,intent(in)     ::  Align                           !< Indicator whether parameter should be align together inside each sections. Default: True
      logical                                     ,optional ,intent(in)     ::  AlignAll                        !< Indicator whether parameter should be align together inside all sections. Default: False
      logical                                     ,optional ,intent(in)     ::  ShowEmpty                       !< Indicator whether empty sections should be written. Default: True
      character(*)                                ,optional ,intent(in)     ::  Include(:)                      !< List of section names which should inclued in the output. Default: none
      character(*)                                ,optional ,intent(in)     ::  Exclude(:)                      !< List of section names which should excluded from the output. Default: none
      integer                                     ,optional ,intent(in)     ::  Depth                           !< Maximum depth of recursive sub-section to bw considered for the output. Default: none
      integer                                     ,optional ,intent(in)     ::  MaxLines                        !< Maximum number of lines to be printed: Default: 1000
      integer                                     ,optional ,intent(in)     ::  MaxParam                        !< Maximum number of parameters per sections: Default: unlimited
    End Subroutine

    Recursive Module Subroutine WriteSectionToLogger( This, Logger, Indent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth, MaxLines, MaxParam )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      type(Logger_Type)                                     ,intent(inout)  ::  Logger                          !< Logger object where to write the Section object
      integer                                     ,optional ,intent(in)     ::  Indent                          !< Initial indentation level. Default: 0
      logical                                     ,optional ,intent(in)     ::  Align                           !< Indicator whether parameter should be align together inside each sections. Default: True
      logical                                     ,optional ,intent(in)     ::  AlignAll                        !< Indicator whether parameter should be align together inside all sections. Default: False
      logical                                     ,optional ,intent(in)     ::  ShowEmpty                       !< Indicator whether empty sections should be written. Default: True
      character(*)                                ,optional ,intent(in)     ::  Include(:)                      !< List of section names which should inclued in the output. Default: none
      character(*)                                ,optional ,intent(in)     ::  Exclude(:)                      !< List of section names which should excluded from the output. Default: none
      integer                                     ,optional ,intent(in)     ::  Depth                           !< Maximum depth of recursive sub-section to bw considered for the output. Default: none
      integer                                     ,optional ,intent(in)     ::  MaxLines                        !< Maximum number of lines to be printed: Default: 1000
      integer                                     ,optional ,intent(in)     ::  MaxParam                        !< Maximum number of parameters per sections: Default: unlimited
    End Subroutine

    Module Subroutine WriteSectionUDIO( This, Unit, iotype, v_list, iostat, iomsg )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  unit
      character(*)                                          ,intent(in)     ::  iotype
      integer                                               ,intent(in)     ::  v_list (:)
      integer                                               ,intent(out)    ::  iostat
      character(*)                                          ,intent(inout)  ::  iomsg
    End Subroutine

    Recursive Module Subroutine OutputSectionToFile( This, Unit, Indentation )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  Unit
      integer                                     ,optional ,intent(in)     ::  Indentation
    End Subroutine

    Recursive Module Subroutine OutputSectionToString( This, Lines, Indentation )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable  ,dimension(:)             ,intent(out)    ::  Lines
      integer                                     ,optional ,intent(in)     ::  Indentation
    End Subroutine

  End Interface

  ! **************************************************************************************************************
  !                               PROCEDURES FOR REMOVING ITEMS FROM A SECTION
  ! **************************************************************************************************************
  Interface

    Recursive Module Subroutine RemoveItems( This, ParametersToRemove, ParametersToKeep, SectionsToRemove, SectionsToKeep, FromSubSection, CallProc, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  ParametersToRemove              !< Names of the Parameters to be removed if present
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  ParametersToKeep                !< Names of the Parameters to be kept if present
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  SectionsToRemove                !< Names of the SubSections to be removed if present
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  SectionsToKeep                  !< Names of the SubSections to be kept if present
      character(*)                                ,optional ,intent(in)     ::  FromSubSection                  !< Name (Eventually nested names) of the sub-section where the removal is to be performed
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Recursive Module Subroutine RemoveParameters( This, ParametersToRemove, ParameterToRemove, ParametersToKeep, ParameterToKeep, FromSubSection, CallProc, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  ParametersToRemove              !< Name of the Parameters to be removed if present
      character(*)                                ,optional ,intent(in)     ::  ParameterToRemove               !< Name of the Parameter to be removed if present
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  ParametersToKeep                !< Name of the Parameters to be kept if present
      character(*)                                ,optional ,intent(in)     ::  ParameterToKeep                 !< Name of the Parameter to be kept if present
      character(*)                                ,optional ,intent(in)     ::  FromSubSection                  !< Name (Eventually nested names) of the sub-section where the removal is to be performed
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Recursive Module Subroutine RemoveSections( This, SectionsToRemove, SectionsToKeep, FromSubSection, CallProc, Debug )
      class(InputSection_Type)   ,target                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  SectionsToRemove                !< Names of the SubSections to be removed if present
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  SectionsToKeep                  !< Names of the SubSections to be kept if present
      character(*)                                ,optional ,intent(in)     ::  FromSubSection                  !< Name (Eventually nested names) of the sub-section where the removal is to be performed
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

  End Interface

! **************************************************************************************************************
!                            PROCEDURES FOR EXTRACTING THE VALUE FROM A PARAMETER PROPERTIES OBJECT
! **************************************************************************************************************
  Interface

    Module Subroutine GetValueFromProp_CHAR_0d( This, Value, Properties, SectionName, CallProc, Found, Defined, Mandatory, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                            ,intent(inout)  ::  Value                           !< Value of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_LOG_0d( This, Value, Properties, SectionName, CallProc, Found, Defined, Mandatory, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      logical                                               ,intent(inout)  ::  Value                           !< Value of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_INT8_0d( This, Value, Properties, SectionName, CallProc, Found, Defined, Mandatory, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT8)                                         ,intent(inout)  ::  Value                           !< Value of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_INT16_0d( This, Value, Properties, SectionName, CallProc, Found, Defined, Mandatory, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT16)                                        ,intent(inout)  ::  Value                           !< Value of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_INT32_0d( This, Value, Properties, SectionName, CallProc, Found, Defined, Mandatory, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT32)                                        ,intent(inout)  ::  Value                           !< Value of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_INT64_0d( This, Value, Properties, SectionName, CallProc, Found, Defined, Mandatory, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT64)                                        ,intent(inout)  ::  Value                           !< Value of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_REAL32_0d( This, Value, Properties, SectionName, CallProc, Found, Defined, Mandatory, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL32)                                          ,intent(inout)  ::  Value                           !< Value of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_REAL64_0d( This, Value, Properties, SectionName, CallProc, Found, Defined, Mandatory, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL64)                                          ,intent(inout)  ::  Value                           !< Value of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_REAL128_0d( This, Value, Properties, SectionName, CallProc, Found, Defined, Mandatory, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL128)                                         ,intent(inout)  ::  Value                           !< Value of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_REAL64_0d_Alloc( This, Value, Properties, SectionName, CallProc, Found, Defined, Mandatory, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(8)   ,allocatable                                ,intent(inout)  ::  Value                           !< Value of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_CHAR_1d( This, Values, Properties, SectionName, CallProc, Found, Defined, Mandatory, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)    ,allocatable                          ,intent(inout)  ::  Values(:)                       !< Values of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_LOG_1d( This, Values, Properties, SectionName, CallProc, Found, Defined, Mandatory, Separator, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      logical         ,allocatable                          ,intent(inout)  ::  Values(:)                       !< Values of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_INT8_1d( This, Values, Properties, SectionName, CallProc, Found, Defined, Mandatory, Separator, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT8)     ,allocatable                        ,intent(inout)  ::  Values(:)                       !< Values of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_INT16_1d( This, Values, Properties, SectionName, CallProc, Found, Defined, Mandatory, Separator, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT16)    ,allocatable                        ,intent(inout)  ::  Values(:)                       !< Values of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_INT32_1d( This, Values, Properties, SectionName, CallProc, Found, Defined, Mandatory, Separator, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT32)    ,allocatable                        ,intent(inout)  ::  Values(:)                       !< Values of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_INT64_1d( This, Values, Properties, SectionName, CallProc, Found, Defined, Mandatory, Separator, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT64)    ,allocatable                        ,intent(inout)  ::  Values(:)                       !< Values of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_REAL32_1d( This, Values, Properties, SectionName, CallProc, Found, Defined, Mandatory, Separator, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL32)    ,allocatable                          ,intent(inout)  ::  Values(:)                       !< Values of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_REAL64_1d( This, Values, Properties, SectionName, CallProc, Found, Defined, Mandatory, Separator, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL64)    ,allocatable                          ,intent(inout)  ::  Values(:)                       !< Values of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromProp_REAL128_1d( This, Values, Properties, SectionName, CallProc, Found, Defined, Mandatory, Separator, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL128)    ,allocatable                         ,intent(inout)  ::  Values(:)                       !< Values of the parameter
      class(InputParamProperties_Type)                      ,intent(in)     ::  Properties                      !< Parameter-Properties object used to defined the parameter to be found
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_CHAR_0d( This, Value, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, DefaultValue, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                            ,intent(inout)  ::  Value                           !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_LOG_0d( This, Value, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, DefaultValue, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      logical                                               ,intent(inout)  ::  Value                           !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_INT8_0d( This, Value, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, DefaultValue, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT8)                                         ,intent(inout)  ::  Value                           !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_INT16_0d( This, Value, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, DefaultValue, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT16)                                        ,intent(inout)  ::  Value                           !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_INT32_0d( This, Value, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, DefaultValue, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT32)                                        ,intent(inout)  ::  Value                           !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_INT64_0d( This, Value, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, DefaultValue, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT64)                                        ,intent(inout)  ::  Value                           !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_REAL32_0d( This, Value, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, DefaultValue, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL32)                                          ,intent(inout)  ::  Value                           !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_REAL64_0d( This, Value, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, DefaultValue, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL64)                                          ,intent(inout)  ::  Value                           !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_REAL128_0d( This, Value, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, DefaultValue, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL128)                                         ,intent(inout)  ::  Value                           !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_REAL64_0d_Alloc( This, Value, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, DefaultValue, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(8)   ,allocatable                                ,intent(inout)  ::  Value                           !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_CHAR_1d( This, Values, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, DefaultValue, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable    ,dimension(:)           ,intent(inout)  ::  Values                          !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_LOG_1d( This, Values, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, Separator, DefaultValue, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      logical       ,allocatable    ,dimension(:)           ,intent(inout)  ::  Values                          !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_INT8_1d( This, Values, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, Separator, DefaultValue, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT8)     ,allocatable                        ,intent(inout)  ::  Values(:)                       !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_INT16_1d( This, Values, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, Separator, DefaultValue, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT16)    ,allocatable                        ,intent(inout)  ::  Values(:)                       !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_INT32_1d( This, Values, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, Separator, DefaultValue, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT32)    ,allocatable                        ,intent(inout)  ::  Values(:)                       !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_INT64_1d( This, Values, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, Separator, DefaultValue, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer(INT64)    ,allocatable                        ,intent(inout)  ::  Values(:)                       !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_REAL32_1d( This, Values, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, Separator, DefaultValue, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL32)    ,allocatable                          ,intent(inout)  ::  Values(:)                       !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_REAL64_1d( This, Values, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, Separator, DefaultValue, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL64)    ,allocatable                          ,intent(inout)  ::  Values(:)                       !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine GetValueFromName_REAL128_1d( This, Values, ParameterName, SectionName, CallProc, Found, Defined, Mandatory, Separator, DefaultValue, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL128)   ,allocatable                          ,intent(inout)  ::  Values(:)                       !< Value of the parameter
      character(*)                                          ,intent(in)     ::  ParameterName                   !< Name of the parameter whose value is to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the parameter (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used to separate the element in the vector
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter if undefined
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Recursive Module Subroutine GetValue_REAL64_d2( This, Values, SectionName, CallProc, Found, Defined, Mandatory, DefaultValues, Status, Debug )
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(REAL64)    ,allocatable                          ,intent(inout)  ::  Values(:,:)                     !< Matrix to be extracted
      character(*)                                ,optional ,intent(in)     ::  SectionName                     !< Name of the (sub)section where to search for the Matrix (if absent, searching in current section)
      character(*)                                ,optional ,intent(in)     ::  CallProc                        !< Name of the calling procedure
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether the parameter has been found (This indicator is true if the parameter has been found but it is false if it has not been found but has a default value)
      logical                                     ,optional ,intent(out)    ::  Defined                         !< Indicator whether the parameter is defined in output (This indicator is true if the parameter has been found or if it has a default value)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory (overrides the default mandatory indicator inside 'Properties')
      real(REAL64)                                ,optional ,intent(in)     ::  DefaultValues(:,:)              !< Default value of the parameter if undefined
      type(Status_Type)                           ,optional ,intent(out)    ::  Status                          !< Status indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Recursive Module Function GetRawValueFromIndex( This, ParameterIndex, FromSubSection, Mandatory, Found, Debug ) result(Raw)
      class(InputSection_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  ParameterIndex                  !< Index of the target parameter
      character(*)                                ,optional ,intent(in)     ::  FromSubSection                  !< Name of the section where to search for the target parameter (if absent, searching in current section only)
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameterr is mandatory
      logical                                     ,optional ,intent(out)    ::  Found                           !< Indicator whether some parameterrs have been found
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
      character(:)  ,allocatable                                            ::  Raw                           !< Parameter object corresponding to the target parameter if it has been found or to an empty parameter
    End Function

  End Interface

End Module
