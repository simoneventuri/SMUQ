Module InputParamProperties_Class

  implicit none

  private
  public        ::  InputParamProperties_Type
  public        ::  ConstructorInputParamProperties

  Type                                    ::  InputParamProperties_Type
    logical                               ::  CaseSensitive     =   .False.   !< Indicator whether the parameter name and value are case sensitive
    logical                               ::  Mandatory         =   .False.   !< Indicator whether the parameter is mandatory
    logical                               ::  HasDefaultValue   =   .False.   !< Indicator whether the parameter has a default value (If it has, the default value is stored in the 'DefaultValue' component)
    logical                               ::  HasValidValues    =   .False.   !< Indicator whether the parameter has a set of valid values (If it has, the set of valid values is stored in the 'Valid_Value' component)
    logical                               ::  CheckValidValues  =   .False.   !< Indicator whether the set of valid values of the parameter have to be check
    logical                               ::  HasBounds         =   .False.
    logical                               ::  ilt               =   .False.
    logical                               ::  ile               =   .False.
    logical                               ::  igt               =   .False.
    logical                               ::  ige               =   .False.
    real(8)                               ::  Val_lt            =   0
    real(8)                               ::  Val_le            =   0
    real(8)                               ::  Val_gt            =   0
    real(8)                               ::  Val_ge            =   0
    character(:)  ,allocatable  ,private  ::  Name                            !< Name of the parameter
    character(:)  ,allocatable            ::  DefaultValue                    !< Default value of the parameter
!     class(*)      ,allocatable            ::  DefaultValue                    !< Default value of the parameter
    character(:)  ,allocatable            ::  ValidValues(:)                  !< Vector of possible values for the parameter
    character(:)  ,allocatable            ::  DataType                        !< Type of variable: logical, integer, real, charatcer
    class(*)      ,allocatable            ::  VariableKind
!     class(*)      ,allocatable            ::  LowerThan
!     class(*)      ,allocatable            ::  LowerEqualThan
!     class(*)      ,allocatable            ::  GreaterThan
!     class(*)      ,allocatable            ::  GreaterEqualThan
  contains
    private
    Final             ::  FinalizeInputParamProperties
    procedure ,public ::  Free          =>  FreeInputParamProperties
    procedure ,public ::  Set           =>  SetInputParamProperties
    generic   ,public ::  assignment(=) =>  AssignInputParamProperties
    generic   ,public ::  Output        =>  OutputInputParamPropertiesToString, OutputInputParamPropertiesToFile
    generic   ,public ::  IsValid       =>  IsIndexValid, IsValueValid
    procedure ,public ::  SetName
    procedure ,public ::  GetName
    procedure ,public ::  GetValue
    procedure ,public ::  GetDescription
    procedure ,public ::  GetValueIndex
    procedure ,public ::  GetDefaultValue
    procedure ,public ::  GetIndexDefaultValue
    procedure ,public ::  GetDefault
    procedure ,public ::  GetDefaultLogical
    procedure ,public ::  GetDefaultInteger
    procedure ,public ::  GetDefaultReal8
    procedure ,public ::  GetDefaultCharacter
    procedure ,public ::  GetValidValuesLength
    procedure ,public ::  CheckValueValidity
    procedure         ::  OutputInputParamPropertiesToFile
    procedure         ::  OutputInputParamPropertiesToString
    procedure         ::  AssignInputParamProperties
    procedure         ::  IsIndexValid
    procedure         ::  IsValueValid
  End Type

  Interface             InputParamProperties_Type
    Module Procedure    ConstructorInputParamProperties
  End Interface

! @TODO: Add the optional input parameters to the ConstructorInputParamProperties procedure


  Interface

!     Pure
    Module Function ConstructorInputParamProperties(                                             &
                            Properties, Name, CaseSensitive, Mandatory, DefaultValue, ValidValues,    &
                            DataType, VariableKind, CheckValidValues,               &
                            LowerThan, LowerEqualThan, GreaterThan, GreaterEqualThan ) result(This)
      type(InputParamProperties_Type)             ,optional ,intent(in)     ::  Properties                      !< Parameter-Properties object
      character(*)                                ,optional ,intent(in)     ::  Name                            !< Name of the parameter
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the parameter name and value are case sensitive
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter
!       class(*)                                    ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  ValidValues                     !< Vector of possible values for the parameter
      character(*)                                ,optional ,intent(in)     ::  DataType                        !< Type of variable: logical, integer, real, charatcer
      class(*)                                    ,optional ,intent(in)     ::  VariableKind
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      class(*)                                    ,optional ,intent(in)     ::  LowerThan                       !< Constrains: Parameter value sould be <  this value
      class(*)                                    ,optional ,intent(in)     ::  LowerEqualThan                  !< Constrains: Parameter value sould be <= this value
      class(*)                                    ,optional ,intent(in)     ::  GreaterThan                     !< Constrains: Parameter value sould be >  this value
      class(*)                                    ,optional ,intent(in)     ::  GreaterEqualThan                !< Constrains: Parameter value sould be >= this value
      type(InputParamProperties_Type)                                       ::  This                            !< Parameter-Properties object
    End Function

    Pure Module Subroutine FinalizeInputParamProperties( This )
      type(InputParamProperties_Type)                       ,intent(inout)  ::  This
    End Subroutine

    Pure Module Subroutine FreeInputParamProperties( This )
      class(InputParamProperties_Type)                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
    End Subroutine

    Module Subroutine SetInputParamProperties( This,                                                  &
                            Properties, Name, CaseSensitive, Mandatory, DefaultValue, ValidValues,    &
                            DataType, VariableKind, CheckValidValues,               &
                            LowerThan, LowerEqualThan, GreaterThan, GreaterEqualThan )
      class(InputParamProperties_Type)                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
      type(InputParamProperties_Type)             ,optional ,intent(in)     ::  Properties                      !< Parameter-Properties object
      character(*)                                ,optional ,intent(in)     ::  Name                            !< Name of the parameter
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                  !< Indicator whether the parameter name and value are case sensitive
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                   !< Default value of the parameter
!       class(*)                                    ,optional ,intent(in)     ::  DefaultValue                    !< Default value of the parameter
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  ValidValues                    !< Vector of possible values for the parameter
      character(*)                                ,optional ,intent(in)     ::  DataType                        !< Type of variable: logical, integer, real, charatcer
      class(*)                                    ,optional ,intent(in)     ::  VariableKind
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      class(*)                                    ,optional ,intent(in)     ::  LowerThan                       !< Constrains: Parameter value sould be <  this value
      class(*)                                    ,optional ,intent(in)     ::  LowerEqualThan                  !< Constrains: Parameter value sould be <= this value
      class(*)                                    ,optional ,intent(in)     ::  GreaterThan                     !< Constrains: Parameter value sould be >  this value
      class(*)                                    ,optional ,intent(in)     ::  GreaterEqualThan                !< Constrains: Parameter value sould be >= this value
    End Subroutine

    Module Subroutine OutputInputParamPropertiesToString( This, String )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      character(:)  ,allocatable    ,dimension(:)           ,intent(inout)  ::  String
    End Subroutine

    Module Subroutine OutputInputParamPropertiesToFile( This, Unit )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      integer                                     ,optional ,intent(in)     ::  Unit
    End Subroutine

    Pure Module Subroutine AssignInputParamProperties( lhs, rhs )
      class(InputParamProperties_Type)                      ,intent(inout)  ::  lhs                     ! Parameter-Properties object on the LHS of the "=" sign
      class(InputParamProperties_Type)                      ,intent(in)     ::  rhs                     ! Parameter-Properties object on the RHS of the "=" sign
    End Subroutine

    Pure Module Subroutine GetValueIndex( This, Value, iValue, CaseSensitive, Found )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      character(:)  ,allocatable                            ,intent(in)     ::  Value                   ! Value to be search for in the array of strings "ValidValues"
      integer                                               ,intent(inout)  ::  iValue                  ! Index of the element in "ValidValues" which corresponds to the string "Value", or
      logical                                     ,optional ,intent(in)     ::  CaseSensitive           ! Indicator whether the seach should be case sensitive. By default, the componenet value "CaseSensitive" is used.
      logical                                     ,optional ,intent(out)    ::  Found                   ! Indicator whether the string 'Value' has been found in the array of strings 'ValidValues'
    End Subroutine

    Pure Module Function GetName( This ) result(Name)
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Name
    End Function

    Pure Module Subroutine SetName( This, Name )
      class(InputParamProperties_Type)                      ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
    End Subroutine

    Pure Module Function GetValue( This, iVal ) result(Value)
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      integer                                               ,intent(in)     ::  iVal
      character(:)  ,allocatable                                            ::  Value
    End Function

    Pure Module Function GetDescription( This, iVal ) result(Summary)
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      integer                                               ,intent(in)     ::  iVal
      character(:)  ,allocatable                                            ::  Summary
    End Function

    Pure Module Function IsIndexValid( This, Index ) result(IsValid)
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      integer                                               ,intent(in)     ::  Index
      logical                                                               ::  IsValid
    End Function

    Pure Module Function IsValueValid( This, Value ) result(IsValid)
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Value
      logical                                                               ::  IsValid
    End Function

    Pure Module Function GetDefault( This ) result( Indicator )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      integer                                                               ::  Indicator
    End Function

    Pure Module Subroutine CheckValueValidity( This, Value, IsValid, Status, ErrMsg )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      class(*)                                              ,intent(in)     ::  Value
      logical                                               ,intent(out)    ::  IsValid
      logical                                     ,optional ,intent(out)    ::  Status
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  ErrMsg
    End Subroutine

    Module Subroutine GetDefaultValue( This, Value, Status, ErrMsg, Debug )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      class(*)                                              ,intent(inout)  ::  Value
      integer                                     ,optional ,intent(out)    ::  Status
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  ErrMsg
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetIndexDefaultValue( This, IndexDefaultValue, ErrMsg, Error, Debug )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      integer                                               ,intent(inout)  ::  IndexDefaultValue
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  ErrMsg
      logical                                     ,optional ,intent(out)    ::  Error
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Pure Module Function GetDefaultLogical( This ) result( Value )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      logical                                                               ::  Value
    End Function

    Pure Module Function GetDefaultInteger( This ) result( Value )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      integer                                                               ::  Value
    End Function

    Pure Module Function GetDefaultReal8( This ) result( Value )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      real(8)                                                               ::  Value
    End Function

    Pure Module Function GetDefaultCharacter( This ) result( Value )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Value
    End Function

    Pure Module Function GetValidValuesLength( This ) result( Length )
      class(InputParamProperties_Type)                      ,intent(in)     ::  This
      integer                                                               ::  Length
    End Function

  End Interface

End Module