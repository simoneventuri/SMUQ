Module Logger_Tools_Module

! Include file needed for '_ASSIGN_ALLOCATABLE_CHARACTER_' in Convert_Var2d_To_Str1d
# include "forpack-include.inc"

  use String_Library            ,only:  VecTrim

! #define Purity Pure
#define Purity

  implicit none

  private
  public  ::  GetScalarFormat
  public  ::  GetVectorFormat
  public  ::  GetIntegerFormat
  public  ::  Convert_Variable_To_String

  public  ::  Get_OptOrDef_Value
  public  ::  Is_Valid
  public  ::  GetPrefixLogLevel
!   public  ::  PresentAndTrue

  Interface             Convert_Variable_To_String
    Module Procedure    Convert_Var0d_To_Str0d
    Module Procedure    Convert_Var1d_To_Str0d
    Module Procedure    Convert_Var1d_To_Str1d
    Module Procedure    Convert_Var2d_To_Str1d
  End Interface

  Interface             Convert_To_String
    Module Procedure    Convert_Logical_To_String_0D, Convert_Logical_To_String_1D, Convert_Logical_To_Strings_1D
    Module Procedure    Convert_INT8_To_String_0D   , Convert_INT8_To_String_1D   , Convert_INT8_To_Strings_1D
    Module Procedure    Convert_INT16_To_String_0D  , Convert_INT16_To_String_1D  , Convert_INT16_To_Strings_1D
    Module Procedure    Convert_INT32_To_String_0D  , Convert_INT32_To_String_1D  , Convert_INT32_To_Strings_1D
    Module Procedure    Convert_INT64_To_String_0D  , Convert_INT64_To_String_1D  , Convert_INT64_To_Strings_1D
    Module Procedure    Convert_REAL32_To_String_0D , Convert_REAL32_To_String_1D , Convert_REAL32_To_Strings_1D
    Module Procedure    Convert_REAL64_To_String_0D , Convert_REAL64_To_String_1D , Convert_REAL64_To_Strings_1D
    Module Procedure    Convert_REAL128_To_String_0D, Convert_REAL128_To_String_1D, Convert_REAL128_To_Strings_1D
    Module Procedure    Convert_String_To_String_0D , Convert_String_To_String_1D , Convert_String_To_Strings_1D
  End Interface

  character(*)  ,parameter                      ::  Default_Logical_Format    =   "l1"
  character(*)  ,parameter                      ::  Default_Integer_Format    =   "i0"
  character(*)  ,parameter                      ::  Default_Real_Format       =   "g0"
  character(*)  ,parameter                      ::  Default_Character_Format  =   "a"

  contains


! **************************************************************************************************************
! **************************************************************************************************************
!                                   PROCEDURES RELATED TO FORMATS
! **************************************************************************************************************
! **************************************************************************************************************

! This procedures set the format associated to a given variable which is returned in the character sting
! variable 'Format'. This variable is set to the following input variable using the following priority:
!  1) VarFmt: Used if present. It corresponds to a format specific to a given variable
!  2) TypFormat: Used if present and if 'VarFmt' is absent. It corresponds to a format specific to all variable of a given type.
!  2) ComFmt: Used if present and if 'VarFmt'/'TypeFormat' are absent. It corresponds to a format in common to all variables.
!  3) DefFormat: Used if all optional input variables are absent.
! TODO:
!   What should we do when a input format is not valid, that is, when the procedure 'Is_Valid_Format' returns a False value ?
!   Possible choices are: (1) an error, (2) something a bit less critical like taking the default format
!   For now, option (2) is conisdered

Purity Function GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt ) result(Format)
  character(*)                                          ,intent(in)     ::  DefFormat                     !< Default format:            Used if all optional format are absent
  character(*)                                ,optional ,intent(in)     ::  VarFmt                     !< Variable-specific format:  Used if present
  character(*)                                ,optional ,intent(in)     ::  TypFormat                     !< Type-specific format:      Used if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  ComFmt                     !< Common format:             Used if both the variable-specific and type-specific formats are absent
  character(:)  ,allocatable                                            ::  Format                        !< Selected format
  Format        =     DefFormat                                                                               ! Selecting the default format
  if ( present(VarFmt) ) then                                                                              ! If variable-specific format is present, then select it
    if ( Is_Valid_Format(VarFmt) ) Format = VarFmt                                                      ! Selecting the variable-specific format
    return                                                                                                    ! Exiting since the format has been selected
  end if                                                                                                      ! End if case on variable-specific format presence
  if ( present(TypFormat) ) then                                                                              ! If type-specific format is present, then select it
    if ( Is_Valid_Format(TypFormat) ) Format = TypFormat                                                      ! Selecting the type-specific format
    return                                                                                                    ! Exiting since the format has been selected
  end if                                                                                                      ! End if case on type-specific format presence
  if ( present(ComFmt) ) then                                                                              ! If common format is present, then select it
    if ( Is_Valid_Format(ComFmt) ) Format = ComFmt                                                      ! Selecting the common format
    return                                                                                                    ! Exiting since the format has been selected
  end if                                                                                                      ! End if case on common format presence
End Function

Purity Function GetVectorFormat( DefFormat, VarFmt, TypFormat, ComFmt ) result(Format)
  character(*)                                          ,intent(in)     ::  DefFormat                     !< Default format:            Used if all optional format are absent
  character(*)                                ,optional ,intent(in)     ::  VarFmt                     !< Variable-specific format:  Used if present
  character(*)                                ,optional ,intent(in)     ::  TypFormat                     !< Type-specific format:      Used if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  ComFmt                     !< Common format:             Used if both the variable-specific and type-specific formats are absent
  character(:)  ,allocatable                                            ::  Format                        !< Selected format
  Format        =     "*(" // DefFormat // ",3x)"                                                             ! Selecting the default format
  if ( present(VarFmt) ) then                                                                              ! If variable-specific format is present, then select it
    if ( Is_Valid_Format(VarFmt) ) Format = "*(" // VarFmt // ",3x)"                                    ! Selecting the variable-specific format
    return                                                                                                    ! Exiting since the format has been selected
  end if                                                                                                      ! End if case on variable-specific format presence
  if ( present(TypFormat) ) then                                                                              ! If type-specific format is present, then select it
    if ( Is_Valid_Format(TypFormat) ) Format = "*(" // TypFormat // ",3x)"                                    ! Selecting the type-specific format
    return                                                                                                    ! Exiting since the format has been selected
  end if                                                                                                      ! End if case on type-specific format presence
  if ( present(ComFmt) ) then                                                                              ! If common format is present, then select it
    if ( Is_Valid_Format(ComFmt) ) Format = "*(" // ComFmt // ",3x)"                                    ! Selecting the common format
    return                                                                                                    ! Exiting since the format has been selected
  end if                                                                                                      ! End if case on common format presence
End Function

Subroutine GetIntegerFormat( Variable, Input_Format, Output_Format, Status )
  integer                                               ,intent(in)     ::  Variable
  character(*)                                ,optional ,intent(in)     ::  Input_Format
  character(:)  ,allocatable                            ,intent(out)    ::  Output_Format
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(:)  ,allocatable                                            ::  String
  integer                                                               ::  NDigits
  integer                                                               ::  ios
  if ( present(Input_Format) ) then
    Output_Format   =     Input_Format
    ios             =     0
  else
    NDigits         =       floor( log10( real( abs(Variable) ) ) ) + 1                                                     ! Getting the number of digits of the input integer number
    call Convert_To_String( NDigits, String, Status=ios )
    Output_Format   =       "(i" // trim(String) // ")"
  end if
  if ( present(Status) ) Status = ios
End Subroutine


! **************************************************************************************************************
! **************************************************************************************************************
!                                           TOOLS
! **************************************************************************************************************
! **************************************************************************************************************

! ! This Function set and check a valid open status.
! ! If a valid optional open status is passed, then it is set other wise the default open status is taken
! Function Get_OptOrDef_Value( Default_Value, Valid_Values, Optional_Value ) result( Output_Value )
!   character(*)                                          ,intent(in)     ::  Default_Value                   !< Default value used if no optional value
!   character(*)  ,dimension(:)                           ,intent(in)     ::  Valid_Values                    !< Valid values used to check validity of optional values if present
!   character(*)                                ,optional ,intent(in)     ::  Optional_Value                  !< Optional values used if present and valid
!   character(:)  ,allocatable                                            ::  Output_Value                    !< Output values
!   Output_Value  =       Default_Value
!   if ( present(Optional_Value) ) then
!     if ( Is_Valid(Optional_Value,Valid_Values) ) then
!       Output_Value = Optional_Value
!     else
!       call Error_Unvalid_Value( "", Optional_Value, Valid_Values )
!     end if
!   end if
! End Function
!
! Purity Function Is_Valid( Value, Valid_Values ) result(Valid)
!   implicit none
!   character(*)                                  ,intent(in)     ::  Value                                   !< Value to be checked for validity
!   character(*)          ,dimension( : )         ,intent(in)     ::  Valid_Values                            !< Valid values used for validity check
!   logical                                                       ::  Valid                                   !< Indicator of input object validity
!   integer                                                       ::  i                                       ! Index of valid strings
!   Valid         =       .False.                                                                                 ! Initialization of the object validity indicator to false
!   do i = 1,size(Valid_Values)                                                                                   ! Loop on all valid strings
!     if ( trim(Value) == trim(Valid_Values(i)) ) Valid = .True.                                                  ! If the object if found in the list of valid strings, then setting validity indicator to True
!   end do                                                                                                        ! End do loop on valid strings
! End Function


! Subroutine AddElementToArray( Element, Array )
!   implicit none
!   character(*)                                          ,intent(in)     ::  Element
!   character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
!   integer                                                               ::  Length
!   character(:)  ,dimension(:)   ,allocatable                            ::  Array_tmp
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!   integer                                                               ::  i
! #endif
!   if ( .not. allocated(Array) ) allocate( character(0) :: Array(0) )
!   Length        =       max( len(Array), len(Element) )
!   allocate( character(Length) :: Array_tmp(size(Array)+1) )
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
! !   ------------------------------
!   do i = 1,size(Array)
!     Array_tmp(i)    =       Array(i)
!   end do
! !   ------------------------------
! #else
!   Array_tmp(1:size(Array))    =       Array     ! COMPILER_BUG:GFORTRAN
! #endif
!   Array_tmp(size(Array)+1)    =       Element
!   call move_alloc( Array_tmp, Array )
! End Subroutine
!
! Purity Subroutine AddElementToArray_C0( Element, Array )
!   character(*)                                          ,intent(in)     ::  Element
!   character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
!   character(:)  ,dimension(:)   ,allocatable                            ::  List_Elements
!   if ( .not. allocated(Array) ) allocate( character(0) :: Array(0) )
!   allocate( List_Elements, source = [Array,Element] )
!   call move_alloc( List_Elements, Array )
! End Subroutine
!
! Purity Subroutine AddElementToArray_C1( Elements, Array )
!   character(*)  ,dimension(:)                           ,intent(in)     ::  Elements
!   character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
!   character(:)  ,dimension(:)   ,allocatable                            ::  List_Elements
!   if ( .not. allocated(Array) ) allocate( character(0) :: Array(0) )
!   allocate( List_Elements, source = [Array,Elements] )
!   call move_alloc( List_Elements, Array )
! End Subroutine


Purity Subroutine Remove_Element_From_Array( Array )
  implicit none
  character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
  integer                                                               ::  NElements
  character(:)  ,dimension(:)   ,allocatable                            ::  Array_tmp
  if ( allocated(Array) ) then
    NElements = size(Array)
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
    allocate( Array_tmp(NElements-1), source=Array(1:NElements-1) )
#else
    allocate( Array_tmp, source=Array(1:NElements-1) )
#endif
    call move_alloc( Array_tmp, Array )
  end if
End Subroutine

! Purity Function VecTrim( Input_String ) result(Output_String)
!   character(*)  ,dimension(:)                           ,intent(in)     ::  Input_String
!   character(:)  ,dimension(:)           ,allocatable                    ::  Output_String
!   integer                                                               ::  i
!   allocate( character(LenTrim(Input_String)) :: Output_String(size(Input_String)) )
!   do i = 1,size(Input_String)
!     Output_String(i)    =       trim( Input_String(i) )
!   end do
! End Function

Purity Function LenTrim( Strings ) result( Length )
  character(*)  ,dimension(:)                   ,intent(in)             ::  Strings                         !< Array of character string
  integer                                                               ::  Length                          !< Maximum length without trailling blanks along all elements of the input string array
  integer                                                               ::  i                               ! Index of string' elements
  Length        =       0                                                                                       ! Initialization of maximum length of string
  do i = 1,size(Strings,1)                                                                                      ! Loop on all elements
    Length      =       max( Length, len_trim(Strings(i)) )                                                     ! Setting the maximum length
  end do                                                                                                        ! End loop on all elements
End Function



! **************************************************************************************************************
! **************************************************************************************************************
!                                       CONVERSION PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

! These procedures converts an input variable into a string using a given format.
! If all the optional input format are absent from the calling sequence, then the default format is used which
! depende on the type of variable. Otherwise, then format 'VarFmt' is used if present, or the format 'TypFormat'
! is used if present.

Purity Subroutine Convert_Var0d_To_Str0d( Variable, String, VarFmt, ChaFmt, IntFmt, ReaFmt, ComFmt, Status )
  use iso_fortran_env ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
  class(*)                                              ,intent(in)     ::  Variable                        !< Variable to be converted into a character string
  character(:)  ,allocatable                            ,intent(out)    ::  String                          !< Output character string corresponding to the input variable
  character(*)                                ,optional ,intent(in)     ::  VarFmt                          !< Variable-specific format: Used if present
  character(*)                                ,optional ,intent(in)     ::  ChaFmt                          !< Character format: Used if variable is of type 'character(*)'and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  IntFmt                          !< Integer format: Used if variable is of type 'integer(*)'and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  ReaFmt                          !< Reals format: Used if variable is of type 'real(*)' and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  ComFmt                          !< Common format: Used if the variable-specific format 'Fv'  and the type-specific format 'Fc/Fi/Fr' associated to current variable's type are absent
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  integer                                                               ::  Local_Status
  select type (Variable)
    type is (logical);        call Convert_To_String( Variable, String, VarFmt=VarFmt,                   ComFmt=ComFmt, Status=Local_Status )
    type is (integer(INT8));  call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (integer(INT16)); call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (integer(INT32)); call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (integer(INT64)); call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (real(REAL32));   call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (real(REAL64));   call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (real(REAL128));  call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (character(*));   call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ChaFmt, ComFmt=ComFmt, Status=Local_Status )
    class default;       ! Error
      String        =       "?"
      Local_Status  =       -1
  end select
  if ( present(Status) ) Status = Local_Status
End Subroutine

Purity Subroutine Convert_Var1d_To_Str0d( Variable, String, VarFmt, ChaFmt, IntFmt, ReaFmt, ComFmt, Status, NItemMax )
  use iso_fortran_env ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
  class(*)      ,dimension(:)                           ,intent(in)     ::  Variable                        !< Variable to be converted into a character string
  character(:)  ,allocatable                            ,intent(out)    ::  String                          !< Output character string corresponding to the input variable
  character(*)                                ,optional ,intent(in)     ::  VarFmt                          !< Variable-specific format: Used if present
  character(*)                                ,optional ,intent(in)     ::  ChaFmt                          !< Character format: Used if variable is of type 'character(*)'and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  IntFmt                          !< Integer format: Used if variable is of type 'integer(*)'and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  ReaFmt                          !< Reals format: Used if variable is of type 'real(*)' and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  ComFmt                          !< Common format: Used if the variable-specific format 'Fv'  and the type-specific format 'Fc/Fi/Fr' associated to current variable's type are absent
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  integer                                     ,optional ,intent(in)     ::  NItemMax
  integer                                                               ::  Local_Status
  select type (Variable)
    type is (logical);        call Convert_To_String( Variable, String, VarFmt=VarFmt,                   ComFmt=ComFmt, Status=Local_Status, NItemMax=NItemMax )
    type is (integer(INT8));  call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status, NItemMax=NItemMax )
    type is (integer(INT16)); call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status, NItemMax=NItemMax )
    type is (integer(INT32)); call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status, NItemMax=NItemMax )
    type is (integer(INT64)); call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status, NItemMax=NItemMax )
    type is (real(REAL32));   call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status, NItemMax=NItemMax )
    type is (real(REAL64));   call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status, NItemMax=NItemMax )
    type is (real(REAL128));  call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status, NItemMax=NItemMax )
    type is (character(*));   call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ChaFmt, ComFmt=ComFmt, Status=Local_Status, NItemMax=NItemMax )
    class default;       ! Error
      String        =       "?"
      Local_Status  =       -1
  end select
  if ( present(Status) ) Status = Local_Status
End Subroutine

Purity Subroutine Convert_Var1d_To_Str1d( Variable, String, VarFmt, ChaFmt, IntFmt, ReaFmt, ComFmt, Status )
  use iso_fortran_env ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
  class(*)      ,dimension(:)                           ,intent(in)     ::  Variable                        !< Variable to be converted into a character string
  character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  String                          !< Output character string corresponding to the input variable
  character(*)                                ,optional ,intent(in)     ::  VarFmt                          !< Variable-specific format: Used if present
  character(*)                                ,optional ,intent(in)     ::  ChaFmt                          !< Character format: Used if variable is of type 'character(*)'and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  IntFmt                          !< Integer format: Used if variable is of type 'integer(*)'and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  ReaFmt                          !< Reals format: Used if variable is of type 'real(*)' and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  ComFmt                          !< Common format: Used if the variable-specific format 'Fv'  and the type-specific format 'Fc/Fi/Fr' associated to current variable's type are absent
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  integer                                                               ::  Local_Status
  select type (Variable)
    type is (logical);        call Convert_To_String( Variable, String, VarFmt=VarFmt,                   ComFmt=ComFmt, Status=Local_Status )
    type is (integer(INT8));  call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (integer(INT16)); call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (integer(INT32)); call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (integer(INT64)); call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=IntFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (real(REAL32));   call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (real(REAL64));   call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (real(REAL128));  call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ReaFmt, ComFmt=ComFmt, Status=Local_Status )
    type is (character(*));   call Convert_To_String( Variable, String, VarFmt=VarFmt, TypFormat=ChaFmt, ComFmt=ComFmt, Status=Local_Status )
    class default;       ! Error
      String        =       "?"
      Local_Status  =       -1
  end select
  if ( present(Status) ) Status = Local_Status
End Subroutine


Purity Subroutine Convert_Var2d_To_Str1d( Variable, String, VarFmt, ChaFmt, IntFmt, ReaFmt, ComFmt, Status, NRowMax, NColMax )

  use iso_fortran_env     ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
  use Utilities_Library   ,only:  SelectedItem, FirstSkippedItem, SkippedItemsRange

  class(*)      ,dimension(:,:)                         ,intent(in)     ::  Variable                        !< Variable to be converted into a character string
  character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  String                          !< Output character string corresponding to the input variable
  character(*)                                ,optional ,intent(in)     ::  VarFmt                          !< Variable-specific format: Used if present
  character(*)                                ,optional ,intent(in)     ::  ChaFmt                          !< Character format: Used if variable is of type 'character(*)'and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  IntFmt                          !< Integer format: Used if variable is of type 'integer(*)'and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  ReaFmt                          !< Reals format: Used if variable is of type 'real(*)' and if no variable-specific format
  character(*)                                ,optional ,intent(in)     ::  ComFmt                          !< Common format: Used if the variable-specific format 'Fv'  and the type-specific format 'Fc/Fi/Fr' associated to current variable's type are absent
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  integer                                     ,optional ,intent(in)     ::  NRowMax
  integer                                     ,optional ,intent(in)     ::  NColMax

  integer                                                               ::  Local_Status
  integer                                                               ::  i, j, Length, NEltInp, NEltOut, NEltMax
  integer                                                               ::  NRowInp, NColInp, i1, i2, i3, i4
  logical                                                               ::  SelectRowSubSet, SelectColSubSet
!   logical       ,allocatable                                            ::  PickColumn(:)
  character(:)  ,allocatable                                            ::  Columns, FirstColumns, LastColumns
  character(:)  ,allocatable                                            ::  StringTmp(:)
  character(:)  ,allocatable                                            ::  SkippedColumnString

! Dealing with the case when at least one rank of the variable 'Variable' has a 0-dimension (No elements).
! In shuch case, set an empty string and return a error code in the Status if present.
  if ( size(Variable) <= 0 ) then
!     allocate( String, source = [""] ) ! Ok for ifort 17/18... error gort gcc 6.3
    allocate( character(0) :: String(1) )
    Local_Status  =       -1
    return
  end if

  Length    =   10000
  NRowInp   =   size(Variable,1)    ! Getting the number of input rows
  NEltOut   =   NRowInp

! Setting the number of output elements
!   The number of output elements will be equal to the number of input elements
!   if there is no limitation on the number of elements, that is, if the optional
!   input argument is not specified. The input and output number of elements
!   will also be then same if the input number ogf element is smaller than ...
!   If there are more elements than requested, then remove the ones in the middle
  SelectRowSubSet       =   .False.
  if ( present(NRowMax) ) then
    NEltMax             =   2 * NRowMax + 1   ! Setting the maximum number of rows
    if ( NRowInp > NEltMax ) then
      SelectRowSubSet   =   .True.
      NEltOut           =   NEltMax
    end if
  end if


  SelectColSubSet   =   .False.
  if ( present(NColMax) ) then
    NEltMax   =   2 * NColMax + 1   ! Setting the maximum number of columns
    NColInp   =   size(Variable,2)

    if ( NColInp > NEltMax ) then
      SelectColSubSet       =   .True.
      SkippedColumnString   =   "   ..." // SkippedItemsRange(NColInp,NColMax) // "...   "
      i1      =   1
      i2      =   NColMax
      i3      =   NColInp - NColMax + 1
      i4      =   NColInp
    end if

!     allocate( PickColumn(NColInp) )
!     do j = 1,NColInp
!       PickColumn(j) =   SelectedItem(j,NColInp,NColMax)
!     end do
  end if

  allocate( character(Length) :: String(NEltOut) )  ! Allocating to the number of rows
!   allocate( character(Length) :: StringTmp(NEltOut) )  ! Allocating to the number of rows

  select type (Variable)
    type is (logical)
#     define  _TypFormat
#     include "ConvertVar2dToChar1d_Inline.F90"

    type is (integer(INT8))
#     define  _TypFormat    TypFormat=IntFmt,
#     include "ConvertVar2dToChar1d_Inline.F90"

    type is (integer(INT16))
#     define  _TypFormat    TypFormat=IntFmt,
#     include "ConvertVar2dToChar1d_Inline.F90"

    type is (integer(INT32))
#     define  _TypFormat    TypFormat=IntFmt,
#     include "ConvertVar2dToChar1d_Inline.F90"

    type is (integer(INT64))
#     define  _TypFormat    TypFormat=IntFmt,
#     include "ConvertVar2dToChar1d_Inline.F90"

    type is (real(REAL32))
#     define  _TypFormat    TypFormat=ReaFmt,
#     include "ConvertVar2dToChar1d_Inline.F90"

    type is (real(REAL64))
#     define  _TypFormat    TypFormat=ReaFmt,
#     include "ConvertVar2dToChar1d_Inline.F90"

    type is (real(REAL128))
#     define  _TypFormat    TypFormat=ReaFmt,
#     include "ConvertVar2dToChar1d_Inline.F90"

    type is (character(*))
#     define  _TypFormat    TypFormat=ChaFmt,
#     include "ConvertVar2dToChar1d_Inline.F90"

    class default;       ! Error
      deallocate( String )
      allocate( String, source = ["?"] )
      Local_Status  =       -1

  end select

# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
    _ASSIGN_ALLOCATABLE_CHARACTER_(String,VecTrim(String))
# else
  String    =   VecTrim(String)
# endif
  if ( present(Status) ) Status = Local_Status
End Subroutine


Purity Subroutine Convert_Logical_To_String_0D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  logical                                               ,intent(in)     ::  Variable
  character(:)  ,allocatable                            ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)                                                      ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios
  character(*)                                              ,parameter  ::  DefFormat = Default_Logical_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  write( Long_String , Local_Format , iostat=ios ) Variable
  if ( ios /= 0 ) write(Long_String ,"(g0)") Variable
  String          =       trim(Long_String)
  if ( present(Status) ) Status = ios
End Subroutine

Purity Subroutine Convert_INT8_To_String_0D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  INT8
  integer(INT8)                                         ,intent(in)     ::  Variable
  character(:)  ,allocatable                            ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)                                                      ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios
  character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  write( Long_String , Local_Format , iostat=ios ) Variable
  if ( ios /= 0 ) write(Long_String ,"(g0)") Variable
  String          =       trim(Long_String)
  if ( present(Status) ) Status = ios
End Subroutine

Purity Subroutine Convert_INT16_To_String_0D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  INT16
  integer(INT16)                                        ,intent(in)     ::  Variable
  character(:)  ,allocatable                            ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)                                                      ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios
  character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  write( Long_String , Local_Format , iostat=ios ) Variable
  if ( ios /= 0 ) write(Long_String ,"(g0)") Variable
  String          =       trim(Long_String)
  if ( present(Status) ) Status = ios
End Subroutine

Purity Subroutine Convert_INT32_To_String_0D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  INT32
  integer(INT32)                                        ,intent(in)     ::  Variable
  character(:)  ,allocatable                            ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)                                                      ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios
  character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  write( Long_String , Local_Format , iostat=ios ) Variable
  if ( ios /= 0 ) write(Long_String ,"(g0)") Variable
  String          =       trim(Long_String)
  if ( present(Status) ) Status = ios
End Subroutine

Purity Subroutine Convert_INT64_To_String_0D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  INT64
  integer(INT64)                                        ,intent(in)     ::  Variable
  character(:)  ,allocatable                            ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)                                                      ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios
  character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  write( Long_String , Local_Format , iostat=ios ) Variable
  if ( ios /= 0 ) write(Long_String ,"(g0)") Variable
  String          =       trim(Long_String)
  if ( present(Status) ) Status = ios
End Subroutine

Purity Subroutine Convert_REAL32_To_String_0D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  REAL32
  real(REAL32)                                          ,intent(in)     ::  Variable                             !< Real number to be converted into a string
  character(:)  ,allocatable                            ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)                                                      ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios
  character(*)                                              ,parameter  ::  DefFormat = Default_Real_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  write( Long_String , Local_Format , iostat=ios ) Variable
  if ( ios /= 0 ) write(Long_String ,"(g0)") Variable
  String          =       trim(Long_String)
  if ( present(Status) ) Status = ios
End Subroutine

Purity Subroutine Convert_REAL64_To_String_0D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  REAL64
  real(REAL64)                                          ,intent(in)     ::  Variable                             !< Real number to be converted into a string
  character(:)  ,allocatable                            ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)                                                      ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios
  character(*)                                              ,parameter  ::  DefFormat = Default_Real_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  write( Long_String , Local_Format , iostat=ios ) Variable
  if ( ios /= 0 ) write(Long_String ,"(g0)") Variable
  String          =       trim(Long_String)
  if ( present(Status) ) Status = ios
End Subroutine

Purity Subroutine Convert_REAL128_To_String_0D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  REAL128
  real(REAL128)                                         ,intent(in)     ::  Variable                             !< Real number to be converted into a string
  character(:)  ,allocatable                            ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)                                                      ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios
  character(*)                                              ,parameter  ::  DefFormat = Default_Real_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  write( Long_String , Local_Format , iostat=ios ) Variable
  if ( ios /= 0 ) write(Long_String ,"(g0)") Variable
  String          =       trim(Long_String)
  if ( present(Status) ) Status = ios
End Subroutine

! Note: the 'present(ComFmt)' is removed from the if case because it will lead the a trim of the character
! when a common format is specified but is inteneted only for numeric variable (real, integer).

Purity Subroutine Convert_String_To_String_0D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  character(*)                                          ,intent(in)     ::  Variable                             !< Real number to be converted into a string
  character(:)  ,allocatable                            ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)                                                      ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios
  character(*)                                              ,parameter  ::  DefFormat = Default_Character_Format
  if ( present(VarFmt) .or. present(TypFormat) .or. present(ComFmt) ) then
    Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
    Local_Format    =       "(" // Local_Format // ")"
    write( Long_String , Local_Format , iostat=ios ) Variable
    if ( ios /= 0 ) then
      String          =       Variable
    else
      String          =       trim(Long_String)
    end if
  else
    String          =       Variable
    ios             =       0
  end if
  if ( present(Status) ) Status = ios
End Subroutine


# define  _VarType_         logical
# define  _DefFormat        Default_Logical_Format
# define  _ProcedureName_   Convert_Logical_To_String_1D
# include "ConvertVar1dToChar0d_Inline.F90"

# define  _VarType_         integer(INT8)
# define  _DefFormat        Default_Integer_Format
# define  _ProcedureName_   Convert_INT8_To_String_1D
# include "ConvertVar1dToChar0d_Inline.F90"

# define  _VarType_         integer(INT16)
# define  _DefFormat        Default_Integer_Format
# define  _ProcedureName_   Convert_INT16_To_String_1D
# include "ConvertVar1dToChar0d_Inline.F90"

# define  _VarType_         integer(INT32)
# define  _DefFormat        Default_Integer_Format
# define  _ProcedureName_   Convert_INT32_To_String_1D
# include "ConvertVar1dToChar0d_Inline.F90"

# define  _VarType_         integer(INT64)
# define  _DefFormat        Default_Integer_Format
# define  _ProcedureName_   Convert_INT64_To_String_1D
# include "ConvertVar1dToChar0d_Inline.F90"

# define  _VarType_         real(REAL32)
# define  _DefFormat        Default_Real_Format
# define  _ProcedureName_   Convert_REAL32_To_String_1D
# include "ConvertVar1dToChar0d_Inline.F90"

# define  _VarType_         real(REAL64)
# define  _DefFormat        Default_Real_Format
# define  _ProcedureName_   Convert_REAL64_To_String_1D
# include "ConvertVar1dToChar0d_Inline.F90"

# define  _VarType_         real(REAL128)
# define  _DefFormat        Default_Real_Format
# define  _ProcedureName_   Convert_REAL128_To_String_1D
# include "ConvertVar1dToChar0d_Inline.F90"



!
!
! Purity Subroutine Convert_Logical_To_String_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
!   logical       ,dimension(:)                           ,intent(in)     ::  Variable
!   character(*)                                              ,parameter  ::  DefFormat = Default_Logical_Format
! #include "Logger_NumberToString_Inline.F90"
! End Subroutine
!
! Purity Subroutine Convert_INT8_To_String_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
!   use iso_fortran_env ,only:  INT8
!   integer(INT8) ,dimension(:)                           ,intent(in)     ::  Variable
!   character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
! #include "Logger_NumberToString_Inline.F90"
! End Subroutine
!
! Purity Subroutine Convert_INT16_To_String_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
!   use iso_fortran_env ,only:  INT16
!   integer(INT16) ,dimension(:)                          ,intent(in)     ::  Variable
!   character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
! #include "Logger_NumberToString_Inline.F90"
! End Subroutine
!
! Purity Subroutine Convert_INT32_To_String_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
!   use iso_fortran_env ,only:  INT32
!   integer(INT32) ,dimension(:)                          ,intent(in)     ::  Variable
!   character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
! #include "Logger_NumberToString_Inline.F90"
! End Subroutine
!
! Purity Subroutine Convert_INT64_To_String_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
!   use iso_fortran_env ,only:  INT64
!   integer(INT64),dimension(:)                           ,intent(in)     ::  Variable
!   character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
! #include "Logger_NumberToString_Inline.F90"
! End Subroutine
!
! Purity Subroutine Convert_REAL32_To_String_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
!   use iso_fortran_env ,only:  REAL32
!   real(REAL32)  ,dimension(:)                           ,intent(in)     ::  Variable
!   character(*)                                              ,parameter  ::  DefFormat = Default_Real_Format
! #include "Logger_NumberToString_Inline.F90"
! End Subroutine
!
! Purity Subroutine Convert_REAL64_To_String_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
!   use iso_fortran_env ,only:  REAL64
!   real(REAL64)  ,dimension(:)                           ,intent(in)     ::  Variable
!   character(*)                                              ,parameter  ::  DefFormat = Default_Real_Format
! #include "Logger_NumberToString_Inline.F90"
! End Subroutine

! Purity Subroutine Convert_REAL128_To_String_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
!   use iso_fortran_env ,only:  REAL128
!   real(REAL128) ,dimension(:)                           ,intent(in)     ::  Variable
!   character(*)                                              ,parameter  ::  DefFormat = Default_Real_Format
! #include "Logger_NumberToString_Inline.F90"
! #ifdef NODEF
!   character(10000)                                                      ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
!   character(:)  ,allocatable                                            ::  Local_Format
!   integer                                                               ::  ios
!   Local_Format    =       GetVectorFormat( DefFormat, VarFmt, TypFormat, ComFmt )
!   Local_Format    =       "(" // Local_Format // ")"
!   write( Long_String , Local_Format , iostat=ios ) Variable
!   if ( ios /= 0 ) write(Long_String ,"(g0)") Variable
!   String          =       trim(Long_String)
!   if ( present(Status) ) Status = ios
! #endif
! End Subroutine

Purity Subroutine Convert_String_To_String_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status, NItemMax )
  character(*)  ,dimension(:)                           ,intent(in)     ::  Variable
  character(:)  ,allocatable                            ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  integer                                     ,optional ,intent(in)     ::  NItemMax
  character(10000)                                                      ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios
  integer                                                               ::  i
  character(*)                                              ,parameter  ::  DefFormat = Default_Character_Format
  if ( present(VarFmt) .or. present(TypFormat) .or. present(ComFmt) ) then
    Local_Format    =       GetVectorFormat( DefFormat, VarFmt, TypFormat, ComFmt )
    Local_Format    =       "(" // Local_Format // ")"
    write( Long_String , Local_Format , iostat=ios ) Variable
    if ( ios == 0 ) then
      String          =       ""
      do i = 1,size(Variable)
        String        =       String // Variable(i) // "   "
      end do
      String          =       trim(String)
    else
      write(Long_String ,"(*(a,1x))") Variable
      String          =       trim(Long_String)
    end if
  else
    String          =       ""
    do i = 1,size(Variable)
      String        =       String // Variable(i) // "   "
    end do
    String          =       trim(String)
    ios             =       0
  end if
  if ( present(Status) ) Status = ios
End Subroutine

Purity Subroutine Convert_Logical_To_Strings_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  logical       ,dimension(:)                           ,intent(in)     ::  Variable
  character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)  ,dimension( size(Variable) )                        ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios, Stat, i
  character(*)                                              ,parameter  ::  DefFormat = Default_Logical_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  Stat            =       0
  do i = 1,size(Variable)
    write( Long_String(i) , Local_Format , iostat=ios ) Variable(i)
    if ( ios /= 0 ) then
      write(Long_String(i) ,"(g0)") Variable(i)
      Stat        =       ios
    end if
  end do
  allocate( String , source = VecTrim(Long_String), Stat=ios )
  if ( ios /= 0 ) Stat = ios
  if ( present(Status) ) Status = Stat
End Subroutine

Purity Subroutine Convert_INT8_To_Strings_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  INT8
  integer(INT8) ,dimension(:)                           ,intent(in)     ::  Variable
  character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)  ,dimension( size(Variable) )                        ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios, Stat
  integer                                                               ::  i
  character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  Stat            =       0
  do i = 1,size(Variable)
    write( Long_String(i) , Local_Format , iostat=ios ) Variable(i)
    if ( ios /= 0 ) then
      write(Long_String(i) ,"(g0)") Variable(i)
      Stat        =       ios
    end if
  end do
  allocate( String , source = VecTrim(Long_String), Stat=ios )
  if ( ios /= 0 ) Stat = ios
  if ( present(Status) ) Status = Stat
End Subroutine

Purity Subroutine Convert_INT16_To_Strings_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  INT16
  integer(INT16),dimension(:)                           ,intent(in)     ::  Variable
  character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)  ,dimension( size(Variable) )                        ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios, Stat
  integer                                                               ::  i
  character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  Stat            =       0
  do i = 1,size(Variable)
    write( Long_String(i) , Local_Format , iostat=ios ) Variable(i)
    if ( ios /= 0 ) then
      write(Long_String(i) ,"(g0)") Variable(i)
      Stat        =       ios
    end if
  end do
  allocate( String , source = VecTrim(Long_String), Stat=ios )
  if ( ios /= 0 ) Stat = ios
  if ( present(Status) ) Status = Stat
End Subroutine

Purity Subroutine Convert_INT32_To_Strings_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  INT32
  integer(INT32),dimension(:)                           ,intent(in)     ::  Variable
  character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)  ,dimension( size(Variable) )                        ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios, Stat
  integer                                                               ::  i
  character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  Stat            =       0
  do i = 1,size(Variable)
    write( Long_String(i) , Local_Format , iostat=ios ) Variable(i)
    if ( ios /= 0 ) then
      write(Long_String(i) ,"(g0)") Variable(i)
      Stat        =       ios
    end if
  end do
  allocate( String , source = VecTrim(Long_String), Stat=ios )
  if ( ios /= 0 ) Stat = ios
  if ( present(Status) ) Status = Stat
End Subroutine

Purity Subroutine Convert_INT64_To_Strings_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  INT64
  integer(INT64),dimension(:)                           ,intent(in)     ::  Variable
  character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)  ,dimension( size(Variable) )                        ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios, Stat
  integer                                                               ::  i
  character(*)                                              ,parameter  ::  DefFormat = Default_Integer_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  Stat            =       0
  do i = 1,size(Variable)
    write( Long_String(i) , Local_Format , iostat=ios ) Variable(i)
    if ( ios /= 0 ) then
      write(Long_String(i) ,"(g0)") Variable(i)
      Stat        =       ios
    end if
  end do
  allocate( String , source = VecTrim(Long_String), Stat=ios )
  if ( ios /= 0 ) Stat = ios
  if ( present(Status) ) Status = Stat
End Subroutine

Purity Subroutine Convert_REAL32_To_Strings_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  REAL32
  real(REAL32)  ,dimension(:)                           ,intent(in)     ::  Variable
  character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)  ,dimension( size(Variable) )                        ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios, Stat, i
  character(*)                                              ,parameter  ::  DefFormat = Default_Real_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  Stat            =       0
  do i = 1,size(Variable)
    write( Long_String(i) , Local_Format , iostat=ios ) Variable(i)
    if ( ios /= 0 ) then
      write(Long_String(i) ,"(g0)") Variable(i)
      Stat        =       ios
    end if
  end do
  allocate( String , source = VecTrim(Long_String), Stat=ios )
  if ( ios /= 0 ) Stat = ios
  if ( present(Status) ) Status = Stat
End Subroutine

Purity Subroutine Convert_REAL64_To_Strings_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  REAL64
  real(REAL64)  ,dimension(:)                           ,intent(in)     ::  Variable
  character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)  ,dimension( size(Variable) )                        ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios, Stat, i
  character(*)                                              ,parameter  ::  DefFormat = Default_Real_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  Stat            =       0
  do i = 1,size(Variable)
    write( Long_String(i) , Local_Format , iostat=ios ) Variable(i)
    if ( ios /= 0 ) then
      write(Long_String(i) ,"(g0)") Variable(i)
      Stat        =       ios
    end if
  end do
  allocate( String , source = VecTrim(Long_String), Stat=ios )
  if ( ios /= 0 ) Stat = ios
  if ( present(Status) ) Status = Stat
End Subroutine

Purity Subroutine Convert_REAL128_To_Strings_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  use iso_fortran_env ,only:  REAL128
  real(REAL128) ,dimension(:)                           ,intent(in)     ::  Variable
  character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)  ,dimension( size(Variable) )                        ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios, Stat, i
  character(*)                                              ,parameter  ::  DefFormat = Default_Real_Format
  Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
  Local_Format    =       "(" // Local_Format // ")"
  Stat            =       0
  do i = 1,size(Variable)
    write( Long_String(i) , Local_Format , iostat=ios ) Variable(i)
    if ( ios /= 0 ) then
      write(Long_String(i) ,"(g0)") Variable(i)
      Stat        =       ios
    end if
  end do
  allocate( String , source = VecTrim(Long_String), Stat=ios )
  if ( ios /= 0 ) Stat = ios
  if ( present(Status) ) Status = Stat
End Subroutine

Purity Subroutine Convert_String_To_Strings_1D( Variable, String, VarFmt, TypFormat, ComFmt, Status )
  character(*)  ,dimension(:)                           ,intent(in)     ::  Variable
  character(:)  ,dimension(:) ,allocatable              ,intent(out)    ::  String
  character(*)                                ,optional ,intent(in)     ::  VarFmt
  character(*)                                ,optional ,intent(in)     ::  TypFormat
  character(*)                                ,optional ,intent(in)     ::  ComFmt
  integer                                     ,optional ,intent(out)    ::  Status                          !< Error status indicator (=0 if everthing is ok, /=0 if error)
  character(10000)  ,dimension( size(Variable) )                        ::  Long_String                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Local_Format
  integer                                                               ::  ios, Stat, i
  character(*)                                              ,parameter  ::  DefFormat = Default_Character_Format
  if ( present(VarFmt) .or. present(TypFormat) .or. present(ComFmt) ) then
    Local_Format    =       GetScalarFormat( DefFormat, VarFmt, TypFormat, ComFmt )
    Local_Format    =       "(" // Local_Format // ")"
    Stat            =       0
    do i = 1,size(Variable)
      write( Long_String(i) , Local_Format , iostat=ios ) Variable(i)
      if ( ios /= 0 ) then
        write(Long_String(i) ,"(g0)") Variable(i)
        Stat        =       ios
      end if
    end do
    allocate( String , source = VecTrim(Long_String), Stat=ios )
    if ( ios /= 0 ) Stat = ios
  else
    allocate( String , source = VecTrim(Variable), Stat=Stat )
  end if
  if ( present(Status) ) Status = Stat
End Subroutine

! Function Set_Scalar_Format_0D( Variable, Optional_Format ) result(Format)
!   class(*)                                              ,intent(in)     ::  Variable
!   character(*)                                ,optional ,intent(in)     ::  Optional_Format
!   character(:)  ,allocatable                                            ::  Format
!   select type (Variable)
!     type is (logical);          Format = GetScalarFormat( Default_Logical_Format,    Optional_Format )
!     type is (integer);          Format = GetScalarFormat( Default_Integer_Format,    Optional_Format )
!     type is (real(REAL32));     Format = GetScalarFormat( Default_Real_Format,       Optional_Format )
!     type is (real(REAL64));     Format = GetScalarFormat( Default_Real_Format,       Optional_Format )
!     type is (character(*));     Format = GetScalarFormat( Default_Character_Format,  Optional_Format )
!     class default       ! Error
!   end select
! End Function
!
! Function Set_Scalar_Format_1D( Variable, Optional_Format ) result(Format)
!   class(*)      ,dimension(:)                           ,intent(in)     ::  Variable
!   character(*)                                ,optional ,intent(in)     ::  Optional_Format
!   character(:)  ,allocatable                                            ::  Format
!   select type (Variable)
!     type is (logical);          Format = GetScalarFormat( Default_Logical_Format,    Optional_Format )
!     type is (integer);          Format = GetScalarFormat( Default_Integer_Format,    Optional_Format )
!     type is (real(REAL32));     Format = GetScalarFormat( Default_Real_Format,       Optional_Format )
!     type is (real(REAL64));     Format = GetScalarFormat( Default_Real_Format,       Optional_Format )
!     type is (character(*));     Format = GetScalarFormat( Default_Character_Format,  Optional_Format )
!     class default       ! Error
!   end select
! End Function

! Function Set_Vector_Format_0D( Variable, Optional_Format ) result(Format)
!   class(*)                                              ,intent(in)     ::  Variable
!   character(*)                                ,optional ,intent(in)     ::  Optional_Format
!   character(:)  ,allocatable                                            ::  Format
!   select type (Variable)
!     type is (logical);          Format = GetVectorFormat( Default_Logical_Format,    Optional_Format )
!     type is (integer);          Format = GetVectorFormat( Default_Integer_Format,    Optional_Format )
!     type is (real(REAL32));     Format = GetVectorFormat( Default_Real_Format,       Optional_Format )
!     type is (real(REAL64));     Format = GetVectorFormat( Default_Real_Format,       Optional_Format )
!     type is (character(*));     Format = GetVectorFormat( Default_Character_Format,  Optional_Format )
!     class default       ! Error
!   end select
! End Function
!
! Function Set_Vector_Format_1D( Variable, Optional_Format ) result(Format)
!   class(*)      ,dimension(:)                           ,intent(in)     ::  Variable
!   character(*)                                ,optional ,intent(in)     ::  Optional_Format
!   character(:)  ,allocatable                                            ::  Format
!   select type (Variable)
!     type is (logical);          Format = GetVectorFormat( Default_Logical_Format,    Optional_Format )
!     type is (integer);          Format = GetVectorFormat( Default_Integer_Format,    Optional_Format )
!     type is (real(REAL32));     Format = GetVectorFormat( Default_Real_Format,       Optional_Format )
!     type is (real(REAL64));     Format = GetVectorFormat( Default_Real_Format,       Optional_Format )
!     type is (character(*));     Format = GetVectorFormat( Default_Character_Format,  Optional_Format )
!     class default       ! Error
!   end select
! End Function

Pure Function PresentAndFalse( OptionalArgument ) result(Indicator)
  logical                                     ,optional ,intent(in)     ::  OptionalArgument
  logical                                                               ::  Indicator
  if ( Present(OptionalArgument) ) then
    Indicator =   .Not. OptionalArgument
  else
    Indicator =   .False.
  end if
End Function


! TODO: Implement the algo telling if a given string is a valid fortran format.
Purity Elemental Function Is_Valid_Format( String ) result(Valid_Format)
  character(*)                                          ,intent(in)     ::  String
  logical                                                               ::  Valid_Format
  character(:)  ,allocatable                                            ::  String_Loc
  String_Loc    =   String
  Valid_Format  =   .True.
End Function










! This Function set and check a valid open status.
! If a valid optional open status is passed, then it is set other wise the default open status is taken
Function Get_OptOrDef_Value( Default_Value, Valid_Values, Optional_Value ) result( Output_Value )
  character(*)                                          ,intent(in)     ::  Default_Value                   !< Default value used if no optional value
  character(*)  ,dimension(:)                           ,intent(in)     ::  Valid_Values                    !< Valid values used to check validity of optional values if present
  character(*)                                ,optional ,intent(in)     ::  Optional_Value                  !< Optional values used if present and valid
  character(:)  ,allocatable                                            ::  Output_Value                    !< Output values
  Output_Value  =       Default_Value
  if ( present(Optional_Value) ) then
    if ( Is_Valid(Optional_Value,Valid_Values) ) then
      Output_Value = Optional_Value
    else
      call Error_Unvalid_Value( "", Optional_Value, Valid_Values )
    end if
  end if
End Function

Purity Function Is_Valid( Value, Valid_Values ) result(Valid)
  implicit none
  character(*)                                  ,intent(in)     ::  Value                                   !< Value to be checked for validity
  character(*)          ,dimension( : )         ,intent(in)     ::  Valid_Values                            !< Valid values used for validity check
  logical                                                       ::  Valid                                   !< Indicator of input object validity
  integer                                                       ::  i                                       ! Index of valid strings
  Valid         =       .False.                                                                                 ! Initialization of the object validity indicator to false
  do i = 1,size(Valid_Values)                                                                                   ! Loop on all valid strings
    if ( trim(Value) == trim(Valid_Values(i)) ) Valid = .True.                                                  ! If the object if found in the list of valid strings, then setting validity indicator to True
  end do                                                                                                        ! End do loop on valid strings
End Function


! This procedure sets the "LogLevel" part of the format's prefix.
Purity Function GetPrefixLogLevel( Error, Warning, Info, Debug, HeavyDebug ) result(Prefix)
  use Utilities_Library    ,only:  PresentAndTrue
  logical                                     ,optional ,intent(in)     ::  Error                               !< Indicator of an 'Error' log message
  logical                                     ,optional ,intent(in)     ::  Warning                             !< Indicator of an 'Warning' log message
  logical                                     ,optional ,intent(in)     ::  Info                                !< Indicator of an 'Info' log message
  logical                                     ,optional ,intent(in)     ::  Debug                               !< Indicator of an 'Debug' log message
  logical                                     ,optional ,intent(in)     ::  HeavyDebug                          !< Indicator of an 'Debug' log message
  character(:)  ,allocatable                                            ::  Prefix
  Prefix  = ""
  if ( PresentAndTrue(Error) )       Prefix = ",'<ERROR> '"
  if ( PresentAndTrue(Warning) )     Prefix = ",'<WARNING> '"
  if ( PresentAndTrue(Info) )        Prefix = ",'<INFO> '"
  if ( PresentAndTrue(Debug) )       Prefix = ",'<DEBUG> '"
  if ( PresentAndTrue(HeavyDebug) )  Prefix = ",'<HEAVYDEBUG> '"
End Function


! Pure Function PresentAndTrue( OptionalArgument ) result(Indicator)
!   logical                                     ,optional ,intent(in)     ::  OptionalArgument
!   logical                                                               ::  Indicator
!   Indicator   =   .False.
!   if ( Present(OptionalArgument) ) Indicator  = OptionalArgument
! End Function


Subroutine Error_Unvalid_Value( Type_Value, Value, Valid_Values )
  character(*)                                          ,intent(in)     ::  Type_Value                      !< Type of value
  character(*)                                          ,intent(in)     ::  Value                           !< Erroneous value
  character(*)  ,dimension(:)                           ,intent(in)     ::  Valid_Values                    !< Valid values
  write(*,"(4x,'[Error_Unvalid_Value]: Error: Unvalid value ')")
  write(*,"(4x,'[Error_Unvalid_Value]: Type_Value   = ',a)") Type_Value
  write(*,"(4x,'[Error_Unvalid_Value]: Value        = ',a)") Value
  write(*,"(4x,'[Error_Unvalid_Value]: Valid_Values = ',*(a,3x))") Valid_Values(:)
  write(*,"(4x,'[Error_Unvalid_Value]: Stopping the code')")
  stop
End Subroutine



End Module



! Subroutine AddElementToArray( Element, Array )
!   implicit none
!   character(*)                                          ,intent(in)     ::  Element
!   character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
!   integer                                                               ::  Length
!   character(:)  ,dimension(:)   ,allocatable                            ::  Array_tmp
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!   integer                                                               ::  i
! #endif
!   if ( .not. allocated(Array) ) allocate( character(0) :: Array(0) )
!   Length        =       max( len(Array), len(Element) )
!   allocate( character(Length) :: Array_tmp(size(Array)+1) )
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
! !   ------------------------------
!   do i = 1,size(Array)
!     Array_tmp(i)    =       Array(i)
!   end do
! !   ------------------------------
! #else
!   Array_tmp(1:size(Array))    =       Array     ! COMPILER_BUG:GFORTRAN
! #endif
!   Array_tmp(size(Array)+1)    =       Element
!   call move_alloc( Array_tmp, Array )
! End Subroutine
!
! Purity Subroutine AddElementToArray_C0( Element, Array )
!   character(*)                                          ,intent(in)     ::  Element
!   character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
!   character(:)  ,dimension(:)   ,allocatable                            ::  List_Elements
!   if ( .not. allocated(Array) ) allocate( character(0) :: Array(0) )
!   allocate( List_Elements, source = [Array,Element] )
!   call move_alloc( List_Elements, Array )
! End Subroutine
!
! Purity Subroutine AddElementToArray_C1( Elements, Array )
!   character(*)  ,dimension(:)                           ,intent(in)     ::  Elements
!   character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
!   character(:)  ,dimension(:)   ,allocatable                            ::  List_Elements
!   if ( .not. allocated(Array) ) allocate( character(0) :: Array(0) )
!   allocate( List_Elements, source = [Array,Elements] )
!   call move_alloc( List_Elements, Array )
! End Subroutine

! Purity Subroutine Remove_Element_From_Array( Array )
!   implicit none
!   character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
!   integer                                                               ::  NElements
!   character(:)  ,dimension(:)   ,allocatable                            ::  Array_tmp
!   if ( allocated(Array) ) then
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     allocate( Array_tmp(NElements-1), source=Array(1:NElements-1) )
! #else
!     allocate( Array_tmp, source=Array(1:NElements-1) )
! #endif
!     call move_alloc( Array_tmp, Array )
!   end if
! End Subroutine
!
! Purity Function VecTrim( Input_String ) result(Output_String)
!   character(*)  ,dimension(:)                           ,intent(in)     ::  Input_String
!   character(:)  ,dimension(:)           ,allocatable                    ::  Output_String
!   integer                                                               ::  i
!   allocate( character(LenTrim(Input_String)) :: Output_String(size(Input_String)) )
!   do i = 1,size(Input_String)
!     Output_String(i)    =       trim( Input_String(i) )
!   end do
! End Function
!
! Purity Function LenTrim( Strings ) result( Length )
!   character(*)  ,dimension(:)                   ,intent(in)             ::  Strings                         !< Array of character string
!   integer                                                               ::  Length                          !< Maximum length without trailling blanks along all elements of the input string array
!   integer                                                               ::  i                               ! Index of string' elements
!   Length        =       0                                                                                       ! Initialization of maximum length of string
!   do i = 1,size(Strings,1)                                                                                      ! Loop on all elements
!     Length      =       max( Length, len_trim(Strings(i)) )                                                     ! Setting the maximum length
!   end do                                                                                                        ! End loop on all elements
! End Function
!
