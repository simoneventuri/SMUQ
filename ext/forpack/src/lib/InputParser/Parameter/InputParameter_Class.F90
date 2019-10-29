Module InputParameter_Class

  use InputParamProperties_Class  ,only:  InputParamProperties_Type
  use String_Library              ,only:  String_Type
  use Status_Library              ,only:  Status_Type, UpdateStatus
  use iso_fortran_env             ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

  implicit none

  private
  public        ::  InputParameter_Type
  public        ::  AddParameter
  public        ::  NewParameter

  Type                                                  ::  InputParameter_Type
    logical                                             ::  Defined      = .False.
    character(:)        ,allocatable                    ::  Name
    character(:)        ,allocatable                    ::  Value
    character(:)        ,allocatable                    ::  Raw
    character(:)        ,allocatable                    ::  ParentSection
    type(InputParamProperties_Type)                     ::  Properties
  contains
!     final                       ::  FinalizeParameter
    procedure   ,public         ::  Initialize      =>  InitializeParameter
    procedure   ,public         ::  Free            =>  FreeParameter
    procedure   ,public         ::  SetName         =>  SetParameterName
    procedure   ,public         ::  SetValue        =>  SetParameterValue
    procedure   ,public         ::  GetName         =>  GetParameterName
    procedure   ,public         ::  GetRawValue     =>  GetRawParameterValue
    procedure   ,public         ::  GetRaw          =>  GetParameterRaw
    procedure   ,public         ::  SetProperties
    procedure   ,public         ::  GetParameterIndex
    generic     ,public         ::  IsFunction      =>  IsParameterFunctionNoName, IsParameterFunctionFromName, IsParameterFunctionFromNames
    generic     ,public         ::  GetValue        =>  GetValue_LOG_0d, GetValue_CHAR_0d,                                          &
                                                        GetValue_INT8_0d, GetValue_INT16_0d, GetValue_INT32_0d, GetValue_INT64_0d,  &
                                                        GetValue_REAL32_0d, GetValue_REAL64_0d, GetValue_REAL128_0d,                &
                                                        GetValue_LOG_1d, GetValue_CHAR_1d,                                          &
                                                        GetValue_INT8_1d, GetValue_INT16_1d, GetValue_INT32_1d, GetValue_INT64_1d,  &
                                                        GetValue_REAL32_1d, GetValue_REAL64_1d, GetValue_REAL128_1d,                &
                                                        GetValue_REAL32_2d, GetValue_REAL64_2d, GetValue_REAL128_2d
    procedure   ,private        ::  GetValue_LOG_0d, GetValue_CHAR_0d      ! @COMPILER_BUG: This procedure is causing a ICE with ifort ifort version 16.0.0
    procedure   ,private        ::  GetValue_INT8_0d, GetValue_INT16_0d, GetValue_INT32_0d, GetValue_INT64_0d
    procedure   ,private        ::  GetValue_REAL32_0d, GetValue_REAL64_0d, GetValue_REAL128_0d
    procedure   ,private        ::  GetValue_LOG_1d, GetValue_CHAR_1d
    procedure   ,private        ::  GetValue_INT8_1d, GetValue_INT16_1d, GetValue_INT32_1d, GetValue_INT64_1d
    procedure   ,private        ::  GetValue_REAL32_1d, GetValue_REAL64_1d, GetValue_REAL128_1d
    procedure   ,private        ::  GetValue_INT8_2d, GetValue_INT16_2d, GetValue_INT32_2d, GetValue_INT64_2d
    procedure   ,private        ::  GetValue_REAL32_2d, GetValue_REAL64_2d, GetValue_REAL128_2d
    procedure   ,private        ::  IsParameterFunctionNoName
    procedure   ,private        ::  IsParameterFunctionFromName
    procedure   ,private        ::  IsParameterFunctionFromNames
  End Type

  Interface             AddParameter
!     Module Procedure    AddParameterFromNameValue
    Module Procedure    AddParameterFromCharacter
    Module Procedure    AddParameterToParameter
    Module Procedure    AddParametersToParameter
    Module Procedure    AddParametersFromStrings
  End Interface

  Interface             NewParameter
    Module Procedure    NewParameterFromNameValue
    Module Procedure    NewParameterFromRaw
  End Interface

  Interface
    Pure Module Subroutine FinalizeParameter( This )
      type(InputParameter_Type)                             ,intent(inout)  ::  This
    End Subroutine
    Pure Module Subroutine InitializeParameter( This, Name, Value )
      class(InputParameter_Type)                            ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
      character(*)                                          ,intent(in)     ::  Value
    End Subroutine
    Pure Module Subroutine FreeParameter( This )
      class(InputParameter_Type)                            ,intent(inout)  ::  This
    End Subroutine
    Module Function NewParameterFromNameValue( Name, Value ) result(This)
      character(*)                                          ,intent(in)     ::  Name
      character(*)                                          ,intent(in)     ::  Value
      type(InputParameter_Type)                                             ::  This
    End Function
    Module Function NewParameterFromRaw( Raw ) result(This)
      character(*)                                          ,intent(in)     ::  Raw
      type(InputParameter_Type)                                             ::  This
    End Function
    Pure Module Subroutine SetParameterName( This, Name )
      class(InputParameter_Type)                            ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
    End Subroutine

    Module Subroutine SetParameterValue( This, Value, Debug )
      class(InputParameter_Type)                            ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Value
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine
    Module Function GetParameterName( This ) result(Name)
      class(InputParameter_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Name
    End Function
    Module Function GetParameterRaw( This ) result(Raw)
      class(InputParameter_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Raw
    End Function
    ! **************************************************************************************************************
    ! **************************************************************************************************************
    !                            PROCEDURES FROM EXTRATING THE VALUE FROM A PARAMETER OBJECT
    ! **************************************************************************************************************
    ! **************************************************************************************************************
    Pure Module Function GetRawParameterValue( This ) result(RawValue)
      class(InputParameter_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  RawValue
    End Function

    Module Subroutine GetValue_LOG_0d( This, Value, CallProc, Found, Mandatory, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      logical                                               ,intent(inout)  ::  Value
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT8_0d( This, Value, CallProc, Found, Mandatory, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT8)                                         ,intent(inout)  ::  Value
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT16_0d( This, Value, CallProc, Found, Mandatory, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT16)                                        ,intent(inout)  ::  Value
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT32_0d( This, Value, CallProc, Found, Mandatory, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT32)                                        ,intent(inout)  ::  Value
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT64_0d( This, Value, CallProc, Found, Mandatory, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT64)                                        ,intent(inout)  ::  Value
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_REAL32_0d( This, Value, CallProc, Found, Mandatory, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      real(REAL32)                                          ,intent(inout)  ::  Value
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_REAL64_0d( This, Value, CallProc, Found, Mandatory, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      real(REAL64)                                          ,intent(inout)  ::  Value
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_REAL128_0d( This, Value, CallProc, Found, Mandatory, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      real(REAL128)                                         ,intent(inout)  ::  Value
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_CHAR_0d( This, Value, CallProc, Found, Mandatory, CheckValidValues, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                            ,intent(inout)  ::  Value
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_LOG_1d( This, Values, CallProc, Found, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      logical       ,allocatable                            ,intent(inout)  ::  Values(:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT8_1d( This, Values, CallProc, Found, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT8)   ,allocatable                          ,intent(inout)  ::  Values(:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT16_1d( This, Values, CallProc, Found, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT16)  ,allocatable                          ,intent(inout)  ::  Values(:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT32_1d( This, Values, CallProc, Found, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT32)  ,allocatable                          ,intent(inout)  ::  Values(:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT64_1d( This, Values, CallProc, Found, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT64)  ,allocatable                          ,intent(inout)  ::  Values(:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_REAL32_1d( This, Values, CallProc, Found, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      real(REAL32)  ,allocatable                            ,intent(inout)  ::  Values(:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_REAL64_1d( This, Values, CallProc, Found, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      real(REAL64)  ,allocatable                            ,intent(inout)  ::  Values(:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_REAL128_1d( This, Values, CallProc, Found, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      real(REAL128) ,allocatable                            ,intent(inout)  ::  Values(:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_CHAR_1d( This, Values, CallProc, Found, Mandatory, Separator, CheckValidValues, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                            ,intent(inout)  ::  Values(:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT8_2d( This, Values, CallProc, Found, Shape, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT8) ,allocatable                            ,intent(inout)  ::  Values(:,:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      integer                                     ,optional ,intent(in)     ::  Shape(:)
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT16_2d( This, Values, CallProc, Found, Shape, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT16)  ,allocatable                ,intent(inout)  ::  Values(:,:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      integer                                     ,optional ,intent(in)     ::  Shape(:)
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT32_2d( This, Values, CallProc, Found, Shape, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT32)  ,allocatable                ,intent(inout)  ::  Values(:,:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      integer                                     ,optional ,intent(in)     ::  Shape(:)
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_INT64_2d( This, Values, CallProc, Found, Shape, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      integer(INT64)  ,allocatable                ,intent(inout)  ::  Values(:,:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      integer                                     ,optional ,intent(in)     ::  Shape(:)
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_REAL32_2d( This, Values, CallProc, Found, Shape, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      real(REAL32)  ,allocatable                            ,intent(inout)  ::  Values(:,:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      integer                                     ,optional ,intent(in)     ::  Shape(:)
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_REAL64_2d( This, Values, CallProc, Found, Shape, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      real(REAL64)  ,allocatable                            ,intent(inout)  ::  Values(:,:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      integer                                     ,optional ,intent(in)     ::  Shape(:)
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine GetValue_REAL128_2d( This, Values, CallProc, Found, Shape, Mandatory, Separator, CheckValidValues, IncreasingOrder, DecreasingOrder, Status, Debug )
      class(InputParameter_Type)                            ,intent(in)     ::  This
      real(REAL128)  ,allocatable                           ,intent(inout)  ::  Values(:,:)
      character(*)                                ,optional ,intent(in)     ::  CallProc
      logical                                     ,optional ,intent(out)    ::  Found
      integer                                     ,optional ,intent(in)     ::  Shape(:)
      logical                                     ,optional ,intent(in)     ::  Mandatory
      character(*)                                ,optional ,intent(in)     ::  Separator
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      logical                                     ,optional ,intent(in)     ::  IncreasingOrder                 !< Indicator whether the output values should be sorted in an increasing order
      logical                                     ,optional ,intent(in)     ::  DecreasingOrder                 !< Indicator whether the output values should be sorted in an decreasing order
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine SetProperties( This, Properties, Name, CaseSensitive, Mandatory,        &
                        DefaultValue, ValidValues, DataType, VariableKind, CheckValidValues,  &
                        LowerThan, LowerEqualThan, GreaterThan, GreaterEqualThan              )
      class(InputParameter_Type)                            ,intent(inout)  ::  This
      class(InputParamProperties_Type)            ,optional ,intent(in)     ::  Properties                      !< Parameter-properties object
      character(*)                                ,optional ,intent(in)     ::  Name                            !< Name of the parameter
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                  !< Indicator whether the parameter name and value are case sensitive
      logical                                     ,optional ,intent(in)     ::  Mandatory                       !< Indicator whether the parameter is mandatory
      character(*)                                ,optional ,intent(in)     ::  DefaultValue                   !< Default value of the parameter
      character(*)                                ,optional ,intent(in)     ::  ValidValues(:)                    !< Vector of possible values for the parameter
      character(*)                                ,optional ,intent(in)     ::  DataType                        !< Type of variable: logical, integer, real, charatcer
      class(*)                                    ,optional ,intent(in)     ::  VariableKind
      logical                                     ,optional ,intent(in)     ::  CheckValidValues
      class(*)                                    ,optional ,intent(in)     ::  LowerThan                       !< Constrains: Parameter value sould be <  this value
      class(*)                                    ,optional ,intent(in)     ::  LowerEqualThan                  !< Constrains: Parameter value sould be <= this value
      class(*)                                    ,optional ,intent(in)     ::  GreaterThan                     !< Constrains: Parameter value sould be >  this value
      class(*)                                    ,optional ,intent(in)     ::  GreaterEqualThan                !< Constrains: Parameter value sould be >= this value
    End Subroutine

    Module Function GetParameterIndex( This, CallProc, Status, Debug ) result(iParam)
      class(InputParameter_Type)                            ,intent(in)     ::  This
      character(*)                                ,optional ,intent(in)     ::  CallProc
      type(Status_Type)                           ,optional ,intent(out)    ::  Status
      logical                                     ,optional ,intent(in)     ::  Debug
      integer                                                               ::  iParam
    End Function

    Module Function IsParameterFunctionNoName( This, CaseSensitive, Debug ) result(IsFct)
      class(InputParameter_Type)                            ,intent(in)     ::  This
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      logical                                     ,optional ,intent(in)     ::  Debug
      logical                                                               ::  IsFct
    End Function

    Module Function IsParameterFunctionFromName( This, Name, CaseSensitive, Debug ) result(IsFct)
      class(InputParameter_Type)                            ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Name
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      logical                                     ,optional ,intent(in)     ::  Debug
      logical                                                               ::  IsFct
    End Function

    Module Function IsParameterFunctionFromNames( This, Names, CaseSensitive, Debug ) result(IsFct)
      class(InputParameter_Type)                            ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Names(:)
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      logical                                     ,optional ,intent(in)     ::  Debug
      logical                                                               ::  IsFct
    End Function


!     Module Subroutine AddParameterFromNameValue( Parameters, Name, Value, Debug )
!       type(InputParameter_Type)  ,allocatable               ,intent(inout)  ::  Parameters(:)
!       character(*)                                          ,intent(in)     ::  Name
!       character(*)                                          ,intent(in)     ::  Value
!       logical                                     ,optional ,intent(in)     ::  Debug
!     End Subroutine

    Module Subroutine AddParameterFromCharacter( Parameters, Raw, Debug )
      type(InputParameter_Type) ,allocatable                ,intent(inout)  ::  Parameters(:)
      character(*)                                          ,intent(in)     ::  Raw
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine AddParameterToParameter( Parameters, NewParam, AtStart )
      type(InputParameter_Type) ,allocatable                ,intent(inout)  ::  Parameters(:)
      type(InputParameter_Type)                             ,intent(in)     ::  NewParam
      logical                                     ,optional ,intent(in)     ::  AtStart
    End Subroutine

    Module Subroutine AddParametersToParameter( Parameters, NewParams, AtStart )
      type(InputParameter_Type) ,allocatable                ,intent(inout)  ::  Parameters(:)
      type(InputParameter_Type)                             ,intent(in)     ::  NewParams(:)
      logical                                     ,optional ,intent(in)     ::  AtStart
    End Subroutine

    Module Subroutine AddParametersFromStrings( Parameters, Strings )
      type(InputParameter_Type) ,allocatable                ,intent(inout)  ::  Parameters(:)
      type(String_Type)                                     ,intent(in)     ::  Strings(:)
    End Subroutine
  End Interface

End Module