Module String_Module

! Include file needed for: CONCAT (inside 'ParseNamesValues_Interface_Inlined.F90')
# include "forpack-include.inc"

  use, intrinsic :: ISO_Fortran_Env ,only: Output_Unit
  use iso_fortran_env ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

  implicit none

  private

! Tested procedures
  public  ::  LenTrim
  public  ::  VecTrim


! From submodule: String_Parsing_SubModule
  public  ::  Split
  public  ::  Parse
  public  ::  ParseFunction
  public  ::  GetFunctionName
  public  ::  IsFunction
  public  ::  GetNumberOfItems
  public  ::  GetLettersRightOfNumbers
  public  ::  GetNumbersLeftOfLetters
  public  ::  ParseNumbersLetters
  public  ::  GetNumberLetterPairs
  public  ::  ParseNamesValues


!   String_Format_SubModule
  public  ::  GetLengthFromFormat
  public  ::  Real_Format_From_Length
  public  ::  GetFormat




  public  ::  RemoveLeftChar
  public  ::  RemoveSpace
  public  ::  LowerCase
  public  ::  UpperCase
  public  ::  Tab2Space
  public  ::  Reorder
  public  ::  Compact
  public  ::  RemoveDuplicateCharacters
  public  ::  Swap
  public  ::  GetPosition     !@TODO To be remove and put in Utilities
  public  ::  GetPositions    !@TODO To be remove and put in Utilities
  public  ::  CountPresence
  public  ::  Convert
  public  ::  Convert_To_String
  public  ::  ConvertFromString
  public  ::  Convert_To_Real
  public  ::  Convert_To_Integer
  public  ::  Convert_To_Logical
  public  ::  ConvertVariableKind
  public  ::  Add_Line_To_String
  public  ::  GetNumberOfDigits
  public  ::  RemoveDuplicate
  public  ::  Remove_Empty
  public  ::  Remove_Trailing_Zeros
  public  ::  TrimCharacter
  public  ::  ReplaceCharacter
  public  ::  EscapeFileCharacters
  public  ::  Add_Element_If_Absent   !@TODO: Put this in Utilities and merge capability with AddElementTotArray
  public  ::  Set_Enhanced_Name
  public  ::  Remove_Last_Directory_From_Path
  public  ::  GetLength
  public  ::  SetLength
  public  ::  SetSameLength
  public  ::  Make_Strings_Equal_Len_C0_C0
  public  ::  Inline
  public  ::  GetSubString
  public  ::  GetSubStringIndexes
  public  ::  ReplaceElement
  public  ::  Indent
  public  ::  AddPrefix
  public  ::  RemovePrefix
  public  ::  Add_Suffix
  public  ::  Set_LogUnit
  public  ::  Convert_Ratio
  public  ::  Is_A_Number
  public  ::  IsIntegerNumber
  public  ::  IsRealNumber
  public  ::  RemoveCharacter
  public  ::  CountCharacter
  public  ::  Equal
  public  ::  Is_Numeric
  public  ::  Is_Digit
  public  ::  Is_Letter
  public  ::  GetFilePath
  public  ::  GetBaseName
  public  ::  RemoveQuotes
  public  ::  Quote
  public  ::  StartWith
  public  ::  GetAssociatedString
  public  ::  GetFinalIndexExponentPart
  public  ::  IsExponentCharacter
  public  ::  IsPostExponentCharacter
  public  ::  NiceInteger
  public  ::  GetSeconds
  public  ::  EmptyString
  public  ::  RemoveComment

! Decoration procedures
  public  ::  Frame
  public  ::  Justify

! @TODO: To be removed
  public  ::  Get_Characters_AtRightOf_Numbers    ! TO BE REMOVE: Replaced by:  GetLettersRightOfNumbers
  public  ::  Get_Numbers_AtLeftOf_Characters     ! TO BE REMOVE: Replaced by:  GetNumbersLeftOfLetters
! @TODO: To be removed

! # ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!   public  ::  SetLength_Sub
! # endif

  Interface             GetNumberOfDigits
    Module Procedure    GetNumberOfDigits_INT8
    Module Procedure    GetNumberOfDigits_INT16
    Module Procedure    GetNumberOfDigits_INT32
    Module Procedure    GetNumberOfDigits_INT64
  End Interface

  Interface             UpperCase
    Module Procedure    UpperCase_0d
    Module Procedure    UpperCase_1d
  End Interface

  Interface             Reorder
    Module Procedure    Reorder_Character, Reorder_Real_8, Reorder_Real_4
  End Interface

  Interface             CountPresence
    Module Procedure    CountPresence_Character_0d, CountPresence_Integer_0d, CountPresence_Real4_0d, CountPresence_Real8_0d
    Module Procedure    CountPresence_Character_1d, CountPresence_Integer_1d, CountPresence_Real4_1d, CountPresence_Real8_1d
  End Interface

  Interface             GetPosition
    Module Procedure    GetPosition_0d, GetPosition_1d
  End Interface

  Interface             GetPositions
    Module Procedure    GetPositions_0d
  End Interface

  !@TODO: Add in String_Module 1d version of the convert subroutine
  Interface             Convert
    Module Procedure    Convert_From_CHAR_0d_To_LOG_0d
    Module Procedure    Convert_From_CHAR_0d_To_INT8_0d
    Module Procedure    Convert_From_CHAR_0d_To_INT16_0d
    Module Procedure    Convert_From_CHAR_0d_To_INT32_0d
    Module Procedure    Convert_From_CHAR_0d_To_INT64_0d
    Module Procedure    Convert_From_CHAR_0d_To_REAL32_0d
    Module Procedure    Convert_From_CHAR_0d_To_REAL64_0d
    Module Procedure    Convert_From_CHAR_0d_To_REAL128_0d
  End Interface

  Interface             ConvertVariableKind
    Module Procedure    ConvertVariableKind_0d
  End Interface

  Interface             Convert_To_String
    Module Procedure    Convert_LOG_0d_To_CHAR_0d
    Module Procedure    Convert_INT8_0d_To_CHAR_0d
    Module Procedure    Convert_INT16_0d_To_CHAR_0d
    Module Procedure    Convert_INT32_0d_To_CHAR_0d
    Module Procedure    Convert_INT64_0d_To_CHAR_0d
    Module Procedure    Convert_REAL32_0d_To_CHAR_0d
    Module Procedure    Convert_REAL64_0d_To_CHAR_0d
    Module Procedure    Convert_REAL128_0d_To_CHAR_0d
    Module Procedure    Convert_CHAR_0d_To_CHAR_0d
    Module Procedure    Convert_LOG_1d_To_CHAR_0d
    Module Procedure    Convert_INT8_1d_To_CHAR_0d
    Module Procedure    Convert_INT16_1d_To_CHAR_0d
    Module Procedure    Convert_INT32_1d_To_CHAR_0d
    Module Procedure    Convert_INT64_1d_To_CHAR_0d
    Module Procedure    Convert_REAL32_1d_To_CHAR_0d
    Module Procedure    Convert_REAL64_1d_To_CHAR_0d
    Module Procedure    Convert_REAL128_1d_To_CHAR_0d
  End Interface


  Interface             Convert_To_Integer
    Module Procedure    Convert_CHAR_0d_To_INT32_0d
    Module Procedure    Convert_CHAR_1d_To_INT32_1d
  End Interface

  Interface             Convert_To_Real
    Module Procedure    Convert_CHAR_0d_To_REAL64_0d
    Module Procedure    Convert_CHAR_1d_To_REAL64_1d
  End Interface

  Interface             Convert_To_Logical
    Module Procedure    Convert_CHAR_0d_To_LOG_0d
    Module Procedure    Convert_CHAR_1d_To_LOG_1d
  End Interface

  Interface             Add_Line_To_String
    Module Procedure    Add_Line_To_String, Add_Lines_To_String
  End Interface


  Interface             ReplaceElement
    Module Procedure    ReplaceElement_0d, ReplaceElement_1d
  End Interface

  Interface             LenTrim
    Module Procedure    LenTrim_0d, LenTrim_1d, LenTrim_2d
  End Interface

  Interface             VecTrim
    Module Procedure    VecTrim_0d, VecTrim_1d, VecTrim_2d
!     Module Procedure    VecTrimAlloc_1d
  End Interface

  Interface             TrimCharacter
    Module Procedure    TrimCharacter_0d, TrimCharacter_1d, TrimCharacter_2d
  End Interface



          ! @TODO: To be removed
          Interface             Get_Characters_AtRightOf_Numbers
            Module Procedure    Get_Characters_AtRightOf_Numbers_0d
            Module Procedure    Get_Characters_AtRightOf_Numbers_1d
          End Interface
          Interface             Get_Numbers_AtLeftOf_Characters
            Module Procedure    Get_Numbers_AtLeftOf_Characters_0d
            Module Procedure    Get_Numbers_AtLeftOf_Characters_1d
          End Interface
          ! @TODO: To be removed


  Interface             Add_Element_If_Absent
    Module Procedure    Add_Element_If_Absent_Integer
    Module Procedure    Add_Element_If_Absent_C0,       Add_Element_If_Absent_C1
  End Interface


  ! **************************************************************************************************************
  !     PROCEDURES FOR REPLACING CHARACTER(S) IN A STRING
  ! **************************************************************************************************************

  Interface             ReplaceCharacter
    Module Procedure    ReplaceCharacter_C0d_From_C0d
    Module Procedure    ReplaceCharacter_C0d_From_C1d
    Module Procedure    ReplaceCharacter_C1d_From_C0d
    Module Procedure    ReplaceCharacter_C1d_From_C1d
  End Interface

  Interface
    Pure Module Function ReplaceCharacter_C0d_From_C0d( String, Old, New, Trimed ) result(NewString)
      character(*)                                          ,intent(in)     ::  String                      !< String in which the character should be replaced
      character(*)                                          ,intent(in)     ::  Old                         !< Old character to be replace
      character(*)                                          ,intent(in)     ::  New                         !< New character used for replacement
      logical                                     ,optional ,intent(in)     ::  Trimed                      !< Indicator whether the new string should be trim before replacement (Default: True)
      character(:)  ,allocatable                                            ::  NewString                   !< New string in which the character 'Old' has been replaced by 'New'
    End Function
    Pure Module Function ReplaceCharacter_C0d_From_C1d( String, Old, New, Trimed ) result(NewString)
      character(*)                                          ,intent(in)     ::  String                      !< String in which the character should be replaced
      character(*)                                          ,intent(in)     ::  Old(:)                      !< Set of old character to be replace
      character(*)                                          ,intent(in)     ::  New(:)                      !< Set of new character used for replacement
      logical                                     ,optional ,intent(in)     ::  Trimed                      !< Indicator whether the new string should be trim before replacement (Default: True)
      character(:)  ,allocatable                                            ::  NewString                   !< New string in which each character in 'Old' has been replaced by the ones in 'New'
    End Function
    Pure Module Function ReplaceCharacter_C1d_From_C0d( Strings, Old, New, Trimed ) result(NewStrings)
      character(*)                                          ,intent(in)     ::  Strings(:)                  !< Strings in which the character should be replaced
      character(*)                                          ,intent(in)     ::  Old                         !< Old character to be replace
      character(*)                                          ,intent(in)     ::  New                         !< New character used for replacement
      logical                                     ,optional ,intent(in)     ::  Trimed                      !< Indicator whether the new string should be trim before replacement (Default: True)
      character(:)  ,allocatable                                            ::  NewStrings(:)               !< New strings in which the character 'Old' has been replaced by 'New'
    End Function
    Pure Module Function ReplaceCharacter_C1d_From_C1d( Strings, Old, New, Trimed ) result(NewStrings)
      character(*)                                          ,intent(in)     ::  Strings(:)                  !< Strings in which the character should be replaced
      character(*)                                          ,intent(in)     ::  Old(:)                      !< Old character to be replace
      character(*)                                          ,intent(in)     ::  New(:)                      !< New character used for replacement
      logical                                     ,optional ,intent(in)     ::  Trimed                      !< Indicator whether the new string should be trim before replacement (Default: True)
      character(:)  ,allocatable                                            ::  NewStrings(:)               !< New strings in which the character 'Old' has been replaced by 'New'
    End Function
  End Interface

  Interface             EscapeFileCharacters
    Module Procedure    EscapeFileCharacters_C0d_From_C0d
!     Module Procedure    EscapeFileCharacters_C0d_From_C1d
!     Module Procedure    EscapeFileCharacters_C1d_From_C0d
    Module Procedure    EscapeFileCharacters_C1d_From_C1d
  End Interface
!
  Interface
    Pure Module Function EscapeFileCharacters_C0d_From_C0d( String, Old, New ) result(NewString)
      character(*)                                          ,intent(in)     ::  String                      !< String in which the character should be replaced
      character(*)                                ,optional ,intent(in)     ::  Old                         !< Old character to be replace
      character(*)                                ,optional ,intent(in)     ::  New                         !< New character used for replacement
      character(:)  ,allocatable                                            ::  NewString                   !< New string in which the character 'Old' has been replaced by 'New'
    End Function
!     Pure Module Function EscapeFileCharacters_C0d_From_C1d( String, Old, New ) result(NewString)
!       character(*)                                          ,intent(in)     ::  String                      !< String in which the character should be replaced
!       character(*)                                ,optional ,intent(in)     ::  Old(:)                      !< Set of old character to be replace
!       character(*)                                ,optional ,intent(in)     ::  New(:)                      !< Set of new character used for replacement
!       character(:)  ,allocatable                                            ::  NewString                   !< New string in which each character in 'Old' has been replaced by the ones in 'New'
!     End Function
!     Pure Module Function EscapeFileCharacters_C1d_From_C0d( Strings, Old, New ) result(NewStrings)
!       character(*)                                          ,intent(in)     ::  Strings(:)                  !< Strings in which the character should be replaced
!       character(*)                                ,optional ,intent(in)     ::  Old                         !< Old character to be replace
!       character(*)                                ,optional ,intent(in)     ::  New                         !< New character used for replacement
!       character(:)  ,allocatable                                            ::  NewStrings(:)               !< New strings in which the character 'Old' has been replaced by 'New'
!     End Function
    Pure Module Function EscapeFileCharacters_C1d_From_C1d( Strings, Old, New ) result(NewStrings)
      character(*)                                          ,intent(in)     ::  Strings(:)                  !< Strings in which the character should be replaced
      character(*)                                ,optional ,intent(in)     ::  Old                         !< Old character to be replace
      character(*)                                ,optional ,intent(in)     ::  New                         !< New character used for replacement
      character(:)  ,allocatable                                            ::  NewStrings(:)               !< New strings in which the character 'Old' has been replaced by 'New'
    End Function
  End Interface



  Interface             EmptyString
    Module Procedure    EmptyString_0d
    Module Procedure    EmptyString_1d
    Module Procedure    EmptyString_2d
  End Interface



  Interface             GetLength
    Module Procedure    GetLength_0d
    Module Procedure    GetLength_1d
  End Interface

! # ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!   Interface             SetLength
!     Module Procedure    SetLength_0d
!   End Interface
!   Interface             SetLength_Sub
!     Module Procedure    SetLength_1d
!   End Interface
! # else
  Interface             SetLength
    Module Procedure    SetLength_0d
    Module Procedure    SetLength_1d
  End Interface
! # endif


  Interface             Inline
    Module Procedure    Inline_Strings_1d
    Module Procedure    Inline_Reals_1d
    Module Procedure    Inline_Integers_1d
  End Interface

  Interface             Indent
    Module Procedure    Indent_0d
    Module Procedure    Indent_1d
  End Interface

  Interface             AddPrefix
    Module Procedure    AddPrefix_0d
    Module Procedure    AddPrefix_1d
  End Interface

  Interface             RemovePrefix
    Module Procedure    RemovePrefix_0d
  End Interface

  Interface             Add_Suffix
    Module Procedure    Add_Suffix_0d
    Module Procedure    Add_Suffix_1d
  End Interface

  Interface             RemoveCharacter
    Module Procedure    RemoveCharacter_0d
    Module Procedure    RemoveCharacter_1d
  End Interface

  Interface             CountCharacter
    Module Procedure    CountCharacter_0d
!     Module Procedure    CountCharacter_1d
  End Interface

  Interface             SetSameLength
    Module Procedure    SetSameLength_C0_C0
    Module Procedure    SetSameLength_C1_C1
    Module Procedure    SetSameLength_C0_C1
    Module Procedure    SetSameLength_C1_C0
  End Interface

  Interface             RemoveQuotes
    Module Procedure    RemoveQuotes_0d
    Module Procedure    RemoveQuotes_1d
  End Interface

  Interface             Quote
    Module Procedure    Quote_0d
    Module Procedure    Quote_1d
  End Interface

  Interface             StartWith
    Module Procedure    StartWith_0d
    Module Procedure    StartWith_1d
  End Interface



    ! **************************************************************************************************************
    !       Submodule: String_Parsing_SubModule
    ! **************************************************************************************************************

  Interface             ParseFunction
    Module Procedure    ParseFunctionNameArgument_0d
    Module Procedure    ParseFunctionNameArgument_1d
    Module Procedure    ParseFunctionNameArgument_2d
    Module Procedure    ParseFunctionNameArgument_1d_1d
  End Interface

  Interface             GetLettersRightOfNumbers
    Module Procedure    GetLettersRightOfNumbers_0d
    Module Procedure    GetLettersRightOfNumbers_1d
  End Interface

  Interface             GetNumbersLeftOfLetters
    Module Procedure    GetNumbersLeftOfLetters_0d
    Module Procedure    GetNumbersLeftOfLetters_1d
  End Interface

  Interface             ParseNumbersLetters
    Module Procedure    ParseNumbersLetters_CHAR_0d
    Module Procedure    ParseNumbersLetters_INT8_0d
    Module Procedure    ParseNumbersLetters_INT16_0d
    Module Procedure    ParseNumbersLetters_INT32_0d
    Module Procedure    ParseNumbersLetters_INT64_0d
    Module Procedure    ParseNumbersLetters_REAL32_0d
    Module Procedure    ParseNumbersLetters_REAL64_0d
    Module Procedure    ParseNumbersLetters_REAL128_0d
  End Interface

  Interface             IsFunction
    Module Procedure    IsFunction_NoName
    Module Procedure    IsFunction_Name0d
    Module Procedure    IsFunction_Name1d
  End Interface

  Interface             GetNumberLetterPairs
    Module Procedure    GetNumberLetterPairs_CHAR
    Module Procedure    GetNumberLetterPairs_REAL64
  End Interface

  Interface             ParseNamesValues
    Module Procedure    ParseNamesValues_INT8
    Module Procedure    ParseNamesValues_INT16
    Module Procedure    ParseNamesValues_INT32
    Module Procedure    ParseNamesValues_INT64
    Module Procedure    ParseNamesValues_REAL32
    Module Procedure    ParseNamesValues_REAL64
    Module Procedure    ParseNamesValues_REAL128
  End Interface

  Interface

    Pure Module Subroutine Split( Input, LHS, Separator, RHS, EscLHS, EscRHS, i_Length_Eq_Inp, IgnoreBetween, i_Debug )
      character(*)                                          ,intent(in)     ::  Input                           !< Input character string to be splitted into a LHS and a RHS parts
      character(:)  ,allocatable                            ,intent(out)    ::  LHS                             !< Left-hand-side part of the splitted string
      character(*)                                          ,intent(in)     ::  Separator                       !< Separation character string
      character(:)  ,allocatable                            ,intent(out)    ::  RHS                             !< Right-hand-side part of the splitted string
      character(1)                                ,optional ,intent(in)     ::  EscLHS                          !< Left-hand-side escape character string
      character(1)  ,dimension(:)                 ,optional ,intent(in)     ::  EscRHS                          !< Right-hand-side escape character string
      logical                                     ,optional ,intent(in)     ::  i_Length_Eq_Inp                 ! Indicator to force the length of the output string to equal the one of the input string (required because otherwise the error:: fort: (4): Variable STRLHS has substring ending point 1 which is greater than the variable length of 0) Mayu be possible to
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  IgnoreBetween
      logical                                     ,optional ,intent(in)     ::  i_Debug
    End Subroutine

    Module Pure Subroutine Parse( Input, Separator, Output, EscRHS, IgnoreBetween )
      character(*)                                          ,intent(in)     ::  Input                    !< Input scalar character string to be proceeded
      character(*)                                          ,intent(in)     ::  Separator                       !< Separation character string
      character(:)  ,dimension(:)   ,allocatable            ,intent(out)    ::  Output                   !< Output vector character string containing sub-strings
      character(1)  ,dimension(:)                 ,optional ,intent(in)     ::  EscRHS                          !< RHS escape character
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  IgnoreBetween
    End Subroutine

    Module Pure Function GetFunctionName( String, NoArg, FctSep ) result(Name)
      character(*)                                          ,intent(in)     ::  String
      logical                                     ,optional ,intent(in)     ::  NoArg
      character(*)                                ,optional ,intent(in)     ::  FctSep
      character(:)  ,allocatable                                            ::  Name
    End Function

    Module Pure Subroutine ParseFunctionNameArgument_0d( String, Name, Argument, FctSep )
      character(*)                                          ,intent(in)     ::  String                    !< Input scalar character string to be proceeded
      character(:)  ,allocatable                            ,intent(out)    ::  Name
      character(:)  ,allocatable                            ,intent(out)    ::  Argument
      character(*)                                ,optional ,intent(in)     ::  FctSep
    End Subroutine

!     Pure
    Module  Subroutine ParseFunctionNameArgument_1d( String, Name, Argument, FctSep, ArgSep )
      character(*)                                          ,intent(in)     ::  String
      character(:)  ,allocatable                            ,intent(out)    ::  Name
      character(:)  ,allocatable                            ,intent(out)    ::  Argument(:)
      character(*)                                ,optional ,intent(in)     ::  FctSep
      character(*)                                ,optional ,intent(in)     ::  ArgSep
    End Subroutine

!     Pure
    Module  Subroutine ParseFunctionNameArgument_2d( String, Name, Argument, FctSep, ArgSep, ValSep, DefToVal )
      character(*)                                          ,intent(in)     ::  String
      character(:)  ,allocatable                            ,intent(out)    ::  Name
      character(:)  ,allocatable                            ,intent(out)    ::  Argument(:,:) ! Dim=(2,NArg). Argument(1,:) -> Arguement names. Argument(2,:) -> Arguement values
      character(*)                                ,optional ,intent(in)     ::  FctSep
      character(*)                                ,optional ,intent(in)     ::  ArgSep
      character(*)                                ,optional ,intent(in)     ::  ValSep
      logical                                     ,optional ,intent(in)     ::  DefToVal
    End Subroutine

!     Pure
    Module Subroutine ParseFunctionNameArgument_1d_1d( String, Name, ArgNames, ArgValues, DefArgNames, FctSep, ArgSep, ValSep, DefToVal )
      character(*)                                          ,intent(in)     ::  String
      character(:)  ,allocatable                            ,intent(out)    ::  Name
      character(:)  ,allocatable                            ,intent(out)    ::  ArgNames(:)
      character(:)  ,allocatable                            ,intent(out)    ::  ArgValues(:)
      character(*)                                ,optional ,intent(in)     ::  DefArgNames(:)
      character(*)                                ,optional ,intent(in)     ::  FctSep
      character(*)                                ,optional ,intent(in)     ::  ArgSep
      character(*)                                ,optional ,intent(in)     ::  ValSep
      logical                                     ,optional ,intent(in)     ::  DefToVal
    End Subroutine

    Module Pure Function IsFunction_NoName( String, NoArg, FctSep, CaseSensitive ) result(IsFct)
      character(*)                                          ,intent(in)     ::  String                    !< Input scalar character string to be proceeded
      logical                                     ,optional ,intent(in)     ::  NoArg
      character(*)                                ,optional ,intent(in)     ::  FctSep
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      logical                                                               ::  IsFct
    End Function

    Module Pure Function IsFunction_Name0d( String, Name, NoArg, FctSep, CaseSensitive ) result(IsFct)
      character(*)                                          ,intent(in)     ::  String                    !< Input scalar character string to be proceeded
      character(*)                                          ,intent(in)     ::  Name
      logical                                     ,optional ,intent(in)     ::  NoArg
      character(*)                                ,optional ,intent(in)     ::  FctSep
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      logical                                                               ::  IsFct
    End Function

    Module Pure Function IsFunction_Name1d( String, Names, NoArg, FctSep, CaseSensitive ) result(IsFct)
      character(*)                                          ,intent(in)     ::  String                    !< Input scalar character string to be proceeded
      character(*)                                          ,intent(in)     ::  Names(:)
      logical                                     ,optional ,intent(in)     ::  NoArg
      character(*)                                ,optional ,intent(in)     ::  FctSep
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      logical                                                               ::  IsFct
    End Function

    Module Function GetNumberOfItems( String, Separator ) result(NItems)
      character(*)                                          ,intent(in)     ::  String                          !< Input character string to be converted
      character(*)                                ,optional ,intent(in)     ::  Separator                          !< Separation character string
      integer                                                               ::  NItems                          !< Integer number corresponding to the number of item in the input string
    End Function

    Pure Module Function GetLettersRightOfNumbers_0d( NumbersLetters ) result(Letters)
      character(*)                                          ,intent(in)     ::  NumbersLetters                  !< Character string whose letters on the right need to be to be extracted
      character(:)  ,allocatable                                            ::  Letters                         !< Character string corresponding to the extracted letters on the right
    End Function

    Pure Module Function GetLettersRightOfNumbers_1d( NumbersLetters ) result(Letters)
      character(*)  ,dimension(:)                           ,intent(in)     ::  NumbersLetters                  !< Character string whose letters on the right need to be to be extracted
      character(:)  ,dimension(:)   ,allocatable                            ::  Letters                         !< Character string corresponding to the extracted letters on the right
    End Function

    Pure Module Function GetNumbersLeftOfLetters_0d( NumbersLetters, Default ) result(Numbers)
      character(*)                                          ,intent(in)     ::  NumbersLetters                  !< Character string whose number on the left need to be extracted
      character(*)                                ,optional ,intent(in)     ::  Default                         !< Default value for Number of no numbers on the left
      character(:)  ,allocatable                                            ::  Numbers                         !< Character string corresponding to the extracted numbers on the left
    End Function

    Pure Module Function GetNumbersLeftOfLetters_1d( NumbersLetters, Default ) result(Numbers)
      character(*)  ,dimension(:)                           ,intent(in)     ::  NumbersLetters                  !< Character string whose letters on the left need to be extracted
      character(*)                                ,optional ,intent(in)     ::  Default                         !< Default value for Number of no numbers on the left
      character(:)  ,dimension(:)   ,allocatable                            ::  Numbers                         !< Character string corresponding to the extracted numbers on the left
    End Function

    Module Subroutine ParseNumbersLetters_CHAR_0d( NumbersLetters, Numbers, Letters )
      character(*)                                          ,intent(in)     ::  NumbersLetters
      character(:)  ,allocatable                            ,intent(out)    ::  Numbers
      character(:)  ,allocatable                            ,intent(out)    ::  Letters
    End Subroutine

    Module Subroutine ParseNumbersLetters_INT8_0d( NumbersLetters, Numbers, Letters )
      character(*)                                          ,intent(in)     ::  NumbersLetters
      integer(INT8)                                         ,intent(out)    ::  Numbers
      character(:)  ,allocatable                            ,intent(out)    ::  Letters
    End Subroutine

    Module Subroutine ParseNumbersLetters_INT16_0d( NumbersLetters, Numbers, Letters )
      character(*)                                          ,intent(in)     ::  NumbersLetters
      integer(INT16)                                        ,intent(out)    ::  Numbers
      character(:)  ,allocatable                            ,intent(out)    ::  Letters
    End Subroutine

    Module Subroutine ParseNumbersLetters_INT32_0d( NumbersLetters, Numbers, Letters )
      character(*)                                          ,intent(in)     ::  NumbersLetters
      integer(INT32)                                        ,intent(out)    ::  Numbers
      character(:)  ,allocatable                            ,intent(out)    ::  Letters
    End Subroutine

    Module Subroutine ParseNumbersLetters_INT64_0d( NumbersLetters, Numbers, Letters )
      character(*)                                          ,intent(in)     ::  NumbersLetters
      integer(INT64)                                        ,intent(out)    ::  Numbers
      character(:)  ,allocatable                            ,intent(out)    ::  Letters
    End Subroutine

    Module Subroutine ParseNumbersLetters_REAL32_0d( NumbersLetters, Numbers, Letters )
      character(*)                                          ,intent(in)     ::  NumbersLetters
      real(REAL32)                                          ,intent(out)    ::  Numbers
      character(:)  ,allocatable                            ,intent(out)    ::  Letters
    End Subroutine

    Module Subroutine ParseNumbersLetters_REAL64_0d( NumbersLetters, Numbers, Letters )
      character(*)                                          ,intent(in)     ::  NumbersLetters
      real(REAL64)                                          ,intent(out)    ::  Numbers
      character(:)  ,allocatable                            ,intent(out)    ::  Letters
    End Subroutine

    Module Subroutine ParseNumbersLetters_REAL128_0d( NumbersLetters, Numbers, Letters )
      character(*)                                          ,intent(in)     ::  NumbersLetters
      real(REAL128)                                         ,intent(out)    ::  Numbers
      character(:)  ,allocatable                            ,intent(out)    ::  Letters
    End Subroutine

    Module Pure Subroutine GetNumberLetterPairs_CHAR( NumbersLetters, Numbers, Letters )
      character(*)                                          ,intent(in)     ::  NumbersLetters
      character(:)  ,allocatable                            ,intent(out)    ::  Numbers(:)
      character(:)  ,allocatable                            ,intent(out)    ::  Letters(:)
    End Subroutine

    Module Pure Subroutine GetNumberLetterPairs_REAL64( String, Numbers, Letters )
      character(*)                                          ,intent(in)     ::  String
      real(REAL64)  ,allocatable                            ,intent(out)    ::  Numbers(:)
      character(:)  ,allocatable                            ,intent(out)    ::  Letters(:)
    End Subroutine

#   define  _VarType_         integer(INT8)
#   define  _VarKind_         INT8
#   include "ParseNamesValues_Interface_Inlined.F90"

#   define  _VarType_         integer(INT16)
#   define  _VarKind_         INT16
#   include "ParseNamesValues_Interface_Inlined.F90"

#   define  _VarType_         integer(INT32)
#   define  _VarKind_         INT32
#   include "ParseNamesValues_Interface_Inlined.F90"

#   define  _VarType_         integer(INT64)
#   define  _VarKind_         INT64
#   include "ParseNamesValues_Interface_Inlined.F90"

#   define  _VarType_         real(REAL32)
#   define  _VarKind_         REAL32
#   include "ParseNamesValues_Interface_Inlined.F90"

#   define  _VarType_         real(REAL64)
#   define  _VarKind_         REAL64
#   include "ParseNamesValues_Interface_Inlined.F90"

#   define  _VarType_         real(REAL128)
#   define  _VarKind_         REAL128
#   include "ParseNamesValues_Interface_Inlined.F90"

  End Interface







  Interface

    Module Subroutine Set_LogUnit( Unit )
      integer                                               ,intent(in)     ::  Unit
    End Subroutine
    ! **************************************************************************************************************
    !       PROCEDURES FOR INQUIRING TYPE OF STRINGS
    ! **************************************************************************************************************
    Pure Elemental Module Function Is_Digit(String) result(Indicator)
      character(*)                                          ,intent(in)     ::  String                          !< Input character string to be checked for didgits
      logical                                                               ::  Indicator                       !< Output indicator whether of not the input character is a number
    End Function
    Pure Elemental Module Function Is_Letter(String) result(Indicator)
      character(*)                                          ,intent(in)     ::  String                          !< Input character string to be checked for letters
      logical                                                               ::  Indicator                       !< Output indicator whether of not the input character is a letter
    End Function

    ! **************************************************************************************************************
    !       PROCEDURES FOR COUNTING THE NUMBER OF TIME A VARIABLE IS PRESENT IN A LIST OF VARIABLES
    ! **************************************************************************************************************
    Pure Module Function CountPresence_Character_0d( Var, List_Var, Trimed, CaseSensitive ) result(NCounts)
      character(*)                                          ,intent(in)     ::  Var                             !< Variable whose presence in the list of variables is to be counted
      character(*)          ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variable used for counting
      logical                                     ,optional ,intent(in)     ::  Trimed                          !< Indicator whether
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                  !< Indicator whether the search should be case sensitive
      integer                                                               ::  NCounts                         !< Number of occurence of the input variable in the list of variables
    End Function
    Pure Module Function CountPresence_Integer_0d( Var, List_Var ) result(NCounts)
      integer                                               ,intent(in)     ::  Var                             !< Variable whose presence in the list of variables is to be counted
      integer               ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variable used for counting
      integer                                                               ::  NCounts                         !< Number of occurence of the input variable in the list of variables
    End Function
    Pure Module Function CountPresence_Real4_0d( Var, List_Var ) result(NCounts)
      real(4)                                               ,intent(in)     ::  Var                             !< Variable whose presence in the list of variables is to be counted
      real(4)               ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variable used for counting
      integer                                                               ::  NCounts                         !< Number of occurence of the input variable in the list of variables
    End Function
    Pure Module Function CountPresence_Real8_0d( Var, List_Var ) result(NCounts)
      real(8)                                               ,intent(in)     ::  Var                             !< Variable whose presence in the list of variables is to be counted
      real(8)               ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variable used for counting
      integer                                                               ::  NCounts                         !< Number of occurence of the input variable in the list of variables
    End Function
    Pure Module Function CountPresence_Character_1d( Var, List_Var, Trimed, CaseSensitive ) result(NCounts)
      character(*)          ,dimension(:)                   ,intent(in)     ::  Var                             !< Array of variables whose presence in the list of variable is to be counted
      character(*)          ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variables used for counting
      logical                                     ,optional ,intent(in)     ::  Trimed                          !< Indicator whether
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the search should be case sensitive
      integer               ,dimension( size(Var) )                         ::  NCounts                         !< Number of occurence of each element of the array of variable in the list of variables
    End Function
    Pure Module Function CountPresence_Integer_1d( Var, List_Var ) result(NCounts)
      integer               ,dimension(:)                   ,intent(in)     ::  Var                             !< Array of variables whose presence in the list of variable is to be counted
      integer               ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variables used for counting
      integer               ,dimension( size(Var) )                         ::  NCounts                         !< Number of occurence of each element of the array of variable in the list of variables
    End Function
    Pure Module Function CountPresence_Real4_1d( Var, List_Var ) result(NCounts)
      real(4)               ,dimension(:)                   ,intent(in)     ::  Var                             !< Array of variables whose presence in the list of variable is to be counted
      real(4)               ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variables used for counting
      integer               ,dimension( size(Var) )                         ::  NCounts                         !< Number of occurence of each element of the array of variable in the list of variables
    End Function
    Pure Module Function CountPresence_Real8_1d( Var, List_Var ) result(NCounts)
      real(8)               ,dimension(:)                   ,intent(in)     ::  Var                             !< Array of variables whose presence in the list of variable is to be counted
      real(8)               ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variables used for counting
      integer               ,dimension( size(Var) )                         ::  NCounts                         !< Number of occurence of each element of the array of variable in the list of variables
    End Function

    ! **************************************************************************************************************
    !       PROCEDURES FOR GETTING THE POSITION OF A STRING IN A LIST OF STRINGS
    ! **************************************************************************************************************
    Pure Module Function GetPosition_0d( Element, Array, Back, CaseSensitive ) result(iLoc)
      character(*)                                          ,intent(in)     ::  Element                             !< String to be checked for presence
      character(*)                                          ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator whether the position index is counted from the end of the list of strings
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                       !< Indicator whether the search should be case sensitive
      integer                                                               ::  iLoc                                !< Index of the position of the input string in the list of strings
    End Function
    Pure Module Function GetPosition_1d( Elements, Array, Back, CaseSensitive ) result(iLoc)
      character(*)                                          ,intent(in)     ::  Elements(:)                         !< String to be checked for presence
      character(*)                                          ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator than the position index is counted from the end of the list of strings
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                       !< Indicator whether the search should be case sensitive
      integer   ,dimension(size(Elements))                                  ::  iLoc                                !< Index of positions of input strings in the list of strings
    End Function
    Pure Module Subroutine GetPositions_0d( Element, Array, Positions, CaseSensitive )
      character(*)                                          ,intent(in)     ::  Element                             !< String to be checked for presence
      character(*)                                          ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      integer ,allocatable                                  ,intent(out)    ::  Positions(:)                        !< Index of all positions of the input element in the array
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                       !< Indicator whether the search should be case sensitive
    End Subroutine

    ! **************************************************************************************************************
    !       PROCEDURES FOR REORDERING AN ARRAY OF STRING ACCORDING TO A REFERENCE ARRAY OF STRING
    ! **************************************************************************************************************
    Module Subroutine Reorder_Character( Array, ValRef, IdxRef, Mapping, i_Debug )
      character(*)                  ,dimension(:)           ,intent(inout)  ::  Array                           !< Input array on which the ordering is performed
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  ValRef                          !< Reference value array from which the ordering is performed
      integer ,dimension(:)                       ,optional ,intent(in)     ::  IdxRef                          !< Reference index array from which the ordering is performed
      integer ,dimension(:)                       ,optional ,intent(out)    ::  Mapping                         !< Index mapping from old to new order
      logical                                     ,optional ,intent(in)     ::  i_Debug                         !< Debugging indicator
    End Subroutine
    Module Subroutine Reorder_Real_8( Inp, ValRef, IdxRef, Mapping )
      real(8)                       ,dimension(:)           ,intent(inout)  ::  Inp                             !< Input array on which the ordering is performed (called Inp-array)
      real(8) ,dimension(:)                       ,optional ,intent(in)     ::  ValRef                          !< Reference value array from which the ordering is performed
      integer ,dimension(:)                       ,optional ,intent(in)     ::  IdxRef                          !< Reference index array from which the ordering is performed
      integer ,dimension(:)                       ,optional ,intent(out)    ::  Mapping                             !< Output array giving the reordering index
    End Subroutine
    Module Subroutine Reorder_Real_4( Inp, ValRef, IdxRef, Mapping )
      real(4)                       ,dimension(:)           ,intent(inout)  ::  Inp                             !< Input array on which the ordering is performed (called Inp-array)
      real(4) ,dimension(:)                       ,optional ,intent(in)     ::  ValRef                          !< Reference value array from which the ordering is performed
      integer ,dimension(:)                       ,optional ,intent(in)     ::  IdxRef                          !< Reference index array from which the ordering is performed
      integer ,dimension(:)                       ,optional ,intent(out)    ::  Mapping                             !< Output array giving the reordering index
    End Subroutine



!               @TODO: Numbers be removed
              Pure Module Subroutine Get_Characters_AtRightOf_Numbers_1d( Input_String, Output_String )
                character(*)  ,dimension(:)                           ,intent(in)     ::  Input_String                    !< Array of string whose all letters of the RHS of the first set of numbers have to be extracted
                character(:)  ,dimension(:)   ,allocatable            ,intent(out)    ::  Output_String                   !<
              End Subroutine

              Pure Module Subroutine Get_Characters_AtRightOf_Numbers_0d( Input_String, Output_String )
                character(*)                                          ,intent(in)     ::  Input_String                    !< Array of string whose all letters of the RHS of the first set of numbers have to be extracted
                character(:)  ,allocatable                            ,intent(out)    ::  Output_String                   !<
              End Subroutine

              Pure Elemental Module Subroutine Get_Numbers_AtLeftOf_Characters_0d( Input_String, Output_Number, default )
                character(*)                                          ,intent(in)     ::  Input_String                    !< String whose all numbers at the LHS of the first set of letters have to be extracted
                real(8)                                               ,intent(out)    ::  Output_Number                   !<
                character(*)                                ,optional ,intent(in)     ::  default                         !<
              End Subroutine

              Module Subroutine Get_Numbers_AtLeftOf_Characters_1d( Input_String, Output_Number, default )
                character(*)  ,dimension(:)                           ,intent(in)     ::  Input_String                    !< Array of string whose all numbers at the LHS of the first set of letters have to be extracted
                real(8)       ,dimension(:)   ,allocatable            ,intent(out)    ::  Output_Number                   !<
                character(*)                                ,optional ,intent(in)     ::  default                         !<
              End Subroutine
!               @TODO: To be removed

    Module Function RemoveLeftChar( String, Separator, i_Debug ) result(StrOut)
      character(*)                                          ,intent(in)     ::  String                          !< Input character string
      character(*)                                          ,intent(in)     ::  Separator                          !< Separation character string
      logical                                     ,optional ,intent(in)     ::  i_Debug
      character(:)  ,allocatable                                            ::  StrOut                          ! Output character string
    End Function


    Module Pure Subroutine Swap( String1, String2 )
      character(:)  ,allocatable                            ,intent(inout)  ::  String1
      character(:)  ,allocatable                            ,intent(inout)  ::  String2
    End Subroutine



    Pure Module Function Compact( StrInp ) result(StrOut)
      character(*)                                          ,intent(in)     ::  StrInp                          !< Input character string
      character(:)  ,allocatable                                            ::  StrOut                          ! Output character string
!       character(len(StrInp))                                                ::  StrOut                          ! Output character string
    End Function
    Pure Module Function Tab2Space( StrInp ) result(StrOut)
      character(*)                                          ,intent(in)     ::  StrInp                          !< Input character string
      character(:)  ,allocatable                                            ::  StrOut                          ! Output character string
!       character(len(StrInp))                                                ::  StrOut                          ! Output character string
    End Function

    Pure Module Function RemoveSpace( StrInp ) result(StrOut)
      character(*)                                          ,intent(in)     ::  StrInp                          !< Input character string
      character(:)  ,allocatable                                            ::  StrOut                          ! Output character string
!       character(len(StrInp))                                                ::  StrOut                          ! Output character string
    End Function


    ! **************************************************************************************************************
    !       PROCEDURES RELATED TO FORMATS: String_Format_SubModule
    ! **************************************************************************************************************

    Pure Module Function GetLengthFromFormat( Format ) result(Length)
      character(*)                                          ,intent(in)     ::  Format
      integer                                                               ::  Length
    End Function

    Pure Module Function Real_Format_From_Length( Length ) result( Format )
      integer                                               ,intent(in)     ::  Length
      character(:)  ,allocatable                                            ::  Format
    End Function

    Pure Module Function GetFormat( Variable, Full ) result( Format )
      class(*)                                              ,intent(in)     ::  Variable
      logical                                     ,optional ,intent(in)     ::  Full      ! Indicator for adding the opening and cloing parenthesis to have a full format.
      character(:)  ,allocatable                                            ::  Format
    End Function

!     Pure Module Function GetFormat_INT8( Variable ) result( Format )
!       integer(INT8)                                         ,intent(in)     ::  Variable
!       character(:)  ,allocatable                                            ::  Format
!     End Function
!
!     Pure Module Function GetFormat_INT16( Variable ) result( Format )
!       integer(INT16)                                        ,intent(in)     ::  Variable
!       character(:)  ,allocatable                                            ::  Format
!     End Function
!
!     Pure Module Function GetFormat_INT32( Variable ) result( Format )
!       integer(INT32)                                        ,intent(in)     ::  Variable
!       character(:)  ,allocatable                                            ::  Format
!     End Function
!
!     Pure Module Function GetFormat_INT64( Variable ) result( Format )
!       integer(INT64)                                        ,intent(in)     ::  Variable
!       character(:)  ,allocatable                                            ::  Format
!     End Function


    ! **************************************************************************************************************
    !       PROCEDURES FOR CONVERTING UPPER TO LOWER CASE AND RRESIPROCALLY
    ! **************************************************************************************************************
    Pure Module Function UpperCase_0d(StrInp) result(StrOut)
      character(*)                  ,intent(in)     ::  StrInp                                                  !<
      character(:)  ,allocatable                                            ::  StrOut                          ! Output character string
!       character(len(StrInp))                                                ::  StrOut                          ! Output character string
    End Function
    Pure Module Function LowerCase(StrInp)        result(StrOut)
      character(*)                  ,intent(in)     ::  StrInp                                                  !<
      character(:)  ,allocatable                                            ::  StrOut                          ! Output character string
!       character(len(StrInp))                                                ::  StrOut                          ! Output character string
    End Function
    Pure Module Function UpperCase_1d(StrInp) result(StrOut)
      character(*)                  ,dimension(:)                   ,intent(in)     ::  StrInp                                                  !<
    !   character(len(StrInp))        ,dimension(size(StrInp))                        ::  StrOut                                                  !<
      character(:)  ,allocatable    ,dimension(:)                                   ::  StrOut
    End Function


    ! **************************************************************************************************************
    !       PROCEDURES FOR ACCFESSING THE LENGTH OF A STRING
    ! **************************************************************************************************************
    Pure Module Function GetLength_0d( String, Trim ) result(Length)
      character(*)                                          ,intent(in)     ::  String
      logical                                     ,optional ,intent(in)     ::  Trim
      integer                                                               ::  Length
    End Function
    Pure Module Function GetLength_1d( Strings, Trim, Include, Exclude ) result(Length)
      character(*)                                          ,intent(in)     ::  Strings(:)
      logical                                     ,optional ,intent(in)     ::  Trim
      integer                                     ,optional ,intent(in)     ::  Exclude(:)
      integer                                     ,optional ,intent(in)     ::  Include(:)
      integer                                                               ::  Length
    End Function

    ! **************************************************************************************************************
    !       PROCEDURES FOR CHANGING THE LENGTH OF CHARACTER STRINGS
    ! **************************************************************************************************************


    Pure Module Function SetLength_0d( S1, Length, Pad ) result(S2)
      character(*)                                          ,intent(in)     ::  S1
      integer                                               ,intent(in)     ::  Length
      character(1)                                ,optional ,intent(in)     ::  Pad
      character(:)  ,allocatable                                            ::  S2
    End Function
! # ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     Pure Module Subroutine SetLength_1d( S2, Length, Pad )
!       character(:)  ,allocatable                            ,intent(inout)  ::  S2(:)
!       integer                                               ,intent(in)     ::  Length
!       character(1)                                ,optional ,intent(in)     ::  Pad
!     End Subroutine
! # else
    Pure Module Function SetLength_1d( S1, Length, Pad ) result(S2)
      character(*)                                          ,intent(in)     ::  S1(:)
      integer                                               ,intent(in)     ::  Length
      character(1)                                ,optional ,intent(in)     ::  Pad
      character(:)  ,allocatable                                            ::  S2(:)
    End Function
! # endif
    Module Subroutine SetSameLength_C0_C0( S1, S2, Pad )
      character(:)  ,allocatable  ,target                  ,intent(inout)  ::  S1
      character(:)  ,allocatable  ,target                  ,intent(inout)  ::  S2
      character(1)                               ,optional ,intent(in)     ::  Pad ! Default set to a space ' '
    End Subroutine
    Module Subroutine SetSameLength_C1_C1( S1, S2, Pad )
      character(:)  ,allocatable  ,target                  ,intent(inout)  ::  S1(:)
      character(:)  ,allocatable  ,target                  ,intent(inout)  ::  S2(:)
      character(1)                               ,optional ,intent(in)     ::  Pad ! Default set to a space ' '
    End Subroutine
    Module Subroutine SetSameLength_C0_C1( S1, S2, Pad )
      character(:)  ,allocatable  ,target                  ,intent(inout)  ::  S1
      character(:)  ,allocatable  ,target                  ,intent(inout)  ::  S2(:)
      character(1)                               ,optional ,intent(in)     ::  Pad ! Default set to a space ' '
    End Subroutine
    Module Subroutine SetSameLength_C1_C0( S1, S2, Pad )
      character(:)  ,allocatable  ,target                  ,intent(inout)  ::  S1(:)
      character(:)  ,allocatable  ,target                  ,intent(inout)  ::  S2
      character(1)                               ,optional ,intent(in)     ::  Pad ! Default set to a space ' '
    End Subroutine

    Module Subroutine Make_Strings_Equal_Len_C0_C0( String1, String2, BufferChar)
      character(:)  ,allocatable                           ,intent(inout)  ::  String1
      character(:)  ,allocatable                           ,intent(inout)  ::  String2
      character(1)                               ,optional ,intent(in)     ::  BufferChar ! Default set to a space ' '
    End Subroutine

    ! **************************************************************************************************************
    !       PROCEDURES FOR REPLACING AN ELEMENT ON AN ARRAY OF STRING BY ONE OR SEVERAL ELEMENTS
    ! **************************************************************************************************************
    Pure Module Subroutine ReplaceElement_0d( Array, OldElement, NewElement )
      character(:)  ,allocatable                            ,intent(inout)  ::  Array(:)
      character(*)                                          ,intent(in)     ::  OldElement
      character(*)                                          ,intent(in)     ::  NewElement
    End Subroutine
    Pure Module Subroutine ReplaceElement_1d( Array, OldElement, NewElements )
      character(:)  ,allocatable                            ,intent(inout)  ::  Array(:)
      character(*)                                          ,intent(in)     ::  OldElement
      character(*)                                          ,intent(in)     ::  NewElements(:)
    End Subroutine

    ! **************************************************************************************************************
    !       PROCEDURES FOR ADDING A ELEMENT TO AN VECTOR OF STRINGS
    ! **************************************************************************************************************
    Pure Module Subroutine Add_Line_To_String( OutVar, InpVar, At_End, At_Start, At_Position )
      character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  OutVar
      character(*)                                          ,intent(in)     ::  InpVar
      logical                                     ,optional ,intent(in)     ::  At_End  ! Default behavior
      logical                                     ,optional ,intent(in)     ::  At_Start
      integer                                     ,optional ,intent(in)     ::  At_Position
    End Subroutine
    Pure Module Subroutine Add_Lines_To_String( String, Lines, At_Start, At_Position )
      character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  String
      character(*)  ,dimension(:)                           ,intent(in)     ::  Lines
      logical                                     ,optional ,intent(in)     ::  At_Start
      integer                                     ,optional ,intent(in)     ::  At_Position
    End Subroutine
    Module Subroutine Add_Element_If_Absent_Integer( Element, List )
      integer                                               ,intent(in)     ::  Element
      integer       ,dimension(:)   ,allocatable            ,intent(inout)  ::  List
    End Subroutine
    Module Subroutine Add_Element_If_Absent_C0( Array, Element )
      character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
      character(*)                                          ,intent(in)     ::  Element
    End Subroutine
    Module Subroutine Add_Element_If_Absent_C1( Array, Elements )
      character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
      character(*)  ,dimension(:)                           ,intent(in)     ::  Elements
    End Subroutine





    Pure Module Function GetNumberOfDigits_INT8( Number ) result(NumberOfDigits)
      integer(INT8)                                         ,intent(in)     ::  Number                          !< Interger number whose number of digits is to be counted
      integer                                                               ::  NumberOfDigits                  !< Number of digits of the input integer number
    End Function

    Pure Module Function GetNumberOfDigits_INT16( Number ) result(NumberOfDigits)
      integer(INT16)                                        ,intent(in)     ::  Number                          !< Interger number whose number of digits is to be counted
      integer                                                               ::  NumberOfDigits                  !< Number of digits of the input integer number
    End Function

    Pure Module Function GetNumberOfDigits_INT32( Number ) result(NumberOfDigits)
      integer(INT32)                                        ,intent(in)     ::  Number                          !< Interger number whose number of digits is to be counted
      integer                                                               ::  NumberOfDigits                  !< Number of digits of the input integer number
    End Function

    Pure Module Function GetNumberOfDigits_INT64( Number ) result(NumberOfDigits)
      integer(INT64)                                        ,intent(in)     ::  Number                          !< Interger number whose number of digits is to be counted
      integer                                                               ::  NumberOfDigits                  !< Number of digits of the input integer number
    End Function



    Module Pure Function RemoveDuplicate( InpStr ) result(OutStr)
      character(*)                                          ,intent(in)     ::  InpStr(:)
      character(:)  ,allocatable                                            ::  OutStr(:)
    End Function

    Module Subroutine Remove_Empty( Array )
      character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array
    End Subroutine

    Pure Module Function Remove_Trailing_Zeros( StrInp ) result(StrOut)
      character(*)                                          ,intent(in)     ::  StrInp
      character(:)  ,allocatable                                            ::  StrOut
    End Function

    ! **************************************************************************************************************
    !       PROCEDURES FOR TRIMING STRINGS
    ! **************************************************************************************************************
    Pure Module Function LenTrim_0d( Strings ) result( Length )
      character(*)                                  ,intent(in)             ::  Strings                         !< Character string
      integer                                                               ::  Length                          !< Maximum length without trailling blanks
    End Function
    Pure Module Function LenTrim_1d( Strings ) result( Length )
      character(*)  ,dimension(:)                   ,intent(in)             ::  Strings                         !< Array of character string
      integer                                                               ::  Length                          !< Maximum length without trailling blanks along all elements of the input string array
    End Function
    Pure Module Function LenTrim_2d( Strings ) result( Length )
      character(*)  ,dimension(:,:)                 ,intent(in)             ::  Strings                         !< Array of character string
      integer                                                               ::  Length                          !< Maximum length without trailling blanks along all elements of the input string array
    End Function

    Pure Module Subroutine TrimCharacter_0d( String )
      character(:)                          ,allocatable    ,intent(inout)  ::  String
    End Subroutine
    Pure Module Subroutine TrimCharacter_1d( String )
      character(:)  ,dimension(:)           ,allocatable    ,intent(inout)  ::  String
    End Subroutine
    Pure Module Subroutine TrimCharacter_2d( String )
      character(:)  ,dimension(:,:)         ,allocatable    ,intent(inout)  ::  String
    End Subroutine









    Module Function Set_Enhanced_Name(Name) result(Enhanced_Name)
      character(*)                                          ,intent(in)     ::  Name                            !< Input name
      character(:)  ,allocatable                                            ::  Enhanced_Name                   !< Output name
    End Function

    Pure Module Function Is_Numeric( String )
      logical                                                       ::  Is_Numeric                              !< Indicator of numeric value stored in the input string
      character(*)                                  ,intent(in)     ::  String                                  !< String to be checked for validity as a numeric
    End Function

    Pure Module Function Is_A_Number( String )
      character(*)                                          ,intent(in)     ::  String                                  !< String to be checked for validity as a numeric
      logical                                                               ::  Is_A_Number                              !< Indicator of numeric value stored in the input string
    End Function

    Pure Module Function IsIntegerNumber( String, Equiv )
      character(*)                                          ,intent(in)     ::  String                        !< String to be identified as a valid integer number of not
      logical                                     ,optional ,intent(in)     ::  Equiv                         !< Indicator whether the string is equivalent to an integer. This will cath real numbers which actually corresponds to integers (The string "1.0" will return true if Equiv is present and true)
      logical                                                               ::  IsIntegerNumber               !< Indicator the that input string is a valid integer number
    End Function

    Pure Module Function IsRealNumber( String, Equiv )
      character(*)                                          ,intent(in)     ::  String                        !< String to be identified as a valid real number of not
      logical                                     ,optional ,intent(in)     ::  Equiv                         !< Indicator whether the string is equivalent to an real. This will cath integer numbers (The string "1" will return true if Equiv is present and true)
      logical                                                               ::  IsRealNumber                  !< Indicator the that input string is a valid real number
    End Function

    Pure Module Function Remove_Last_Directory_From_Path( DirInp ) result(DirOut)
      character(*)                                          ,intent(in)     ::  DirInp
      character(:)  ,allocatable                                            ::  DirOut
    End Function


    Pure Module Function GetFilePath( FileName ) result(FilePath)
      character(*)                                          ,intent(in)     ::  FileName
      character(:)  ,allocatable                                            ::  FilePath
    End Function

    Pure Module Function GetBaseName( FileName ) result(BaseName)
      character(*)                                          ,intent(in)     ::  FileName
      character(:)  ,allocatable                                            ::  BaseName
    End Function

    ! **************************************************************************************************************
    !       PROCEDURES FOR INLINING AN ARRAY OF CHARACTER STRINGS INTO A SCALAR CHARACTER STRING
    ! **************************************************************************************************************

    Module Pure Function GetSubString( String, InBetween, Protect, Inner ) result(SubString)
      character(*)                                          ,intent(in)     ::  String
      character(*)  ,dimension(2)                           ,intent(in)     ::  InBetween
      logical                                     ,optional ,intent(in)     ::  Protect
      logical                                     ,optional ,intent(in)     ::  Inner
      character(:)  ,allocatable                                            ::  SubString
    End Function

    Module Pure Function GetSubStringIndexes( InpStr, InBetween ) result(Indexes)
      character(*)                                          ,intent(in)     ::  InpStr
      character(*)  ,dimension(2)                           ,intent(in)     ::  InBetween
      integer       ,dimension(2)                                           ::  Indexes
    End Function
    Module Function Indent_0d( String, Indentation ) result(IndentedString)
      character(*)                                          ,intent(in)     ::  String
      integer                                               ,intent(in)     ::  Indentation
      character(:)  ,allocatable                                            ::  IndentedString
    End Function
    Module Function Indent_1d( Strings, Indentation ) result(IndentedStrings)
      character(*)                                          ,intent(in)     ::  Strings(:)
      integer                                               ,intent(in)     ::  Indentation
      character(:)  ,allocatable                                            ::  IndentedStrings(:)
    End Function
    Module Function AddPrefix_0d( String, Prefix ) result(OutputString)
      character(*)                                          ,intent(in)     ::  String
      character(*)                                          ,intent(in)     ::  Prefix
      character(:)  ,allocatable                                            ::  OutputString
    End Function
    Module Function AddPrefix_1d( Strings, Prefix ) result(OutputStrings)
      character(*)  ,dimension(:)                           ,intent(in)     ::  Strings
      character(*)                                          ,intent(in)     ::  Prefix
      character(:)  ,allocatable  ,dimension(:)                             ::  OutputStrings
    End Function


    Module Function RemovePrefix_0d( InputString, Prefix ) result(OutputString)
      character(*)                                          ,intent(in)     ::  InputString
      character(*)                                          ,intent(in)     ::  Prefix
      character(:)  ,allocatable                                            ::  OutputString
    End Function


    Module Function Add_Suffix_0d( String, Suffix ) result(OutputString)
      character(*)                                          ,intent(in)     ::  String
      character(*)                                          ,intent(in)     ::  Suffix
      character(:)  ,allocatable                                            ::  OutputString
    End Function
    Module Function Add_Suffix_1d( Strings, Suffix ) result(OutputStrings)
      character(*)  ,dimension(:)                           ,intent(in)     ::  Strings
      character(*)                                          ,intent(in)     ::  Suffix
      character(:)  ,allocatable  ,dimension(:)                             ::  OutputStrings
    End Function




    Pure Module Function Convert_Ratio( Numerator, Denominator, Length, MaxLength ) result(Ratio)
      integer                                               ,intent(in)     ::  Numerator
      integer                                               ,intent(in)     ::  Denominator
      integer                                     ,optional ,intent(in)     ::  Length                          !< Length of each individual elements
      logical                                     ,optional ,intent(in)     ::  MaxLength
      character(:)  ,allocatable                                            ::  Ratio
    End Function

    Pure Module Function RemoveCharacter_0d( Input, String ) result(Output)
      character(*)                                          ,intent(in)     ::  Input
      character(*)                                          ,intent(in)     ::  String
      character(:)  ,allocatable                                            ::  Output
    End Function

    Pure Module Function RemoveCharacter_1d( Input, String ) result(Output)
      character(*)                                          ,intent(in)     ::  Input
      character(*)  ,dimension(:)                           ,intent(in)     ::  String
      character(:)  ,allocatable                                            ::  Output
    End Function

    Pure Module Function CountCharacter_0d( Input, String ) result(Number)
      character(*)                                          ,intent(in)     ::  Input
      character(*)                                          ,intent(in)     ::  String
      integer                                                               ::  Number
    End Function

    Pure Module Function Equal( S1, S2, Trimed, CaseSensitive )
      character(*)                                          ,intent(in)     ::  S1
      character(*)                                          ,intent(in)     ::  S2
      logical                                     ,optional ,intent(in)     ::  Trimed                          !< Indicator whether
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                  !< Indicator whether the search should be case sensitive
      logical                                                               ::  Equal
    End Function

    Pure Module Function Inline_Strings_1d( InpStr, Separator, Trimed, Invert, Fmt, Length, NMax ) result(OutStr)
      character(*)  ,dimension(:)                           ,intent(in)     ::  InpStr                          !< Array of strings to be inlined
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used between elements
      logical                                     ,optional ,intent(in)     ::  Trimed                          !< Indicator whether the strings should be trimed
      logical                                     ,optional ,intent(in)     ::  Invert                          !< Indicator whether the strings elements should be inverted (NOT IMPLEMENTED)
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number ! @COMPILER_BUG gcc-6.3.1: move devlaration of top to avoid problem... still a mistery
      integer                                     ,optional ,intent(in)     ::  Length                          !< Length of each individual elements
      integer                                     ,optional ,intent(in)     ::  NMax                            !< Maximum number of items to display
      character(:)  ,allocatable                                            ::  OutStr                          !< Scalar character string corresponding to the inlined input string
    End Function

    Pure Module Function Inline_Reals_1d( InpVar, Separator, Trimed, Invert, Fmt, Length, NMax ) result(OutStr)
      real(8)       ,dimension(:)                           ,intent(in)     ::  InpVar                          !< Array of real variable to be converted to character and inlined
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used between elements
      logical                                     ,optional ,intent(in)     ::  Trimed                          !< Indicator whether the strings should be trimed
      logical                                     ,optional ,intent(in)     ::  Invert                          !< Indicator whether the strings elements should be inverted (NOT IMPLEMENTED)
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      integer                                     ,optional ,intent(in)     ::  Length                          !< Length of each individual elements
      integer                                     ,optional ,intent(in)     ::  NMax                            !< Maximum number of items to display
      character(:)  ,allocatable                                            ::  OutStr                          !< Scalar character string corresponding to the inlined input string
    End Function

    Pure Module Function Inline_Integers_1d( InpVar, Separator, Trimed, Invert, Fmt, Length, NMax ) result(OutStr)
      integer       ,dimension(:)                           ,intent(in)     ::  InpVar                          !< Array of real variable to be converted to character and inlined
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator used between elements
      logical                                     ,optional ,intent(in)     ::  Trimed                          !< Indicator whether the strings should be trimed
      logical                                     ,optional ,intent(in)     ::  Invert                          !< Indicator whether the strings elements should be inverted (NOT IMPLEMENTED)
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      integer                                     ,optional ,intent(in)     ::  Length                          !< Length of each individual elements
      integer                                     ,optional ,intent(in)     ::  NMax                            !< Maximum number of items to display
      character(:)  ,allocatable                                            ::  OutStr                          !< Scalar character string corresponding to the inlined input string
    End Function










    ! **************************************************************************************************************
    !       PROCEDURES FOR CONVERTING VARIABLES-0D TO CHARACTER-0D
    ! **************************************************************************************************************

    Pure Recursive Module Function Convert_INT8_0d_To_CHAR_0d( Var, LeadingZeros, Len, Pos, Fmt ) result(String)
      integer(INT8)                                         ,intent(in)     ::  Var                             !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  LeadingZeros                    !< Number of leading zeros
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Recursive Module Function Convert_INT16_0d_To_CHAR_0d( Var, LeadingZeros, Len, Pos, Fmt ) result(String)
      integer(INT16)                                        ,intent(in)     ::  Var                             !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  LeadingZeros                    !< Number of leading zeros
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Recursive Module Function Convert_INT32_0d_To_CHAR_0d( Var, LeadingZeros, Len, Pos, Fmt ) result(String)
      integer(INT32)                                        ,intent(in)     ::  Var                             !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  LeadingZeros                    !< Number of leading zeros
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Recursive Module Function Convert_INT64_0d_To_CHAR_0d( Var, LeadingZeros, Len, Pos, Fmt ) result(String)
      integer(INT64)                                        ,intent(in)     ::  Var                             !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  LeadingZeros                    !< Number of leading zeros
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Module Function Convert_REAL32_0d_To_CHAR_0d( Var, Len, Pos, Fmt ) result(String)
      real(4)                                               ,intent(in)     ::  Var                             !< Real number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Module Function Convert_REAL64_0d_To_CHAR_0d( Var, Len, Pos, Fmt ) result(String)
      real(8)                                               ,intent(in)     ::  Var                             !< Real number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Module Function Convert_REAL128_0d_To_CHAR_0d( Var, Len, Pos, Fmt ) result(String)
      real(16)                                              ,intent(in)     ::  Var                             !< Real number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Module Function Convert_CHAR_0d_To_CHAR_0d( Var, Len, Pos, Fmt ) result(String)
      character(*)                                          ,intent(in)     ::  Var                             !< String to be converted into a string (with possibly a different length
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    ! **************************************************************************************************************
    !       PROCEDURES FOR CONVERTING VARIABLES-1D TO CHARACTER-0D
    ! **************************************************************************************************************

    Pure Module Function Convert_LOG_1d_To_CHAR_0d( Var, T, F, Separator ) result(String)
      logical                                               ,intent(in)     ::  Var(:)                          !< Logical to be converted into a string
      character(*)                                ,optional ,intent(in)     ::  T                               !< String used to indicates "True"
      character(*)                                ,optional ,intent(in)     ::  F                               !< String used to indicates "False"
      character(*)                                ,optional ,intent(in)     ::  Separator                       !< Separator between elements
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function
    Pure Recursive Module Function Convert_INT8_1d_To_CHAR_0d( Var, LeadingZeros, Len, Pos, Fmt ) result(String)
      integer(INT8)                                         ,intent(in)     ::  Var(:)                          !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  LeadingZeros                    !< Number of leading zeros
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Recursive Module Function Convert_INT16_1d_To_CHAR_0d( Var, LeadingZeros, Len, Pos, Fmt ) result(String)
      integer(INT16)                                        ,intent(in)     ::  Var(:)                          !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  LeadingZeros                    !< Number of leading zeros
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Recursive Module Function Convert_INT32_1d_To_CHAR_0d( Var, LeadingZeros, Len, Pos, Fmt ) result(String)
      integer(INT32)                                        ,intent(in)     ::  Var(:)                          !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  LeadingZeros                    !< Number of leading zeros
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Recursive Module Function Convert_INT64_1d_To_CHAR_0d( Var, LeadingZeros, Len, Pos, Fmt ) result(String)
      integer(INT64)                                        ,intent(in)     ::  Var(:)                          !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  LeadingZeros                    !< Number of leading zeros
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Recursive Module Function Convert_REAL32_1d_To_CHAR_0d( Var, Len, Pos, Fmt ) result(String)
      real(REAL32)                                          ,intent(in)     ::  Var(:)                          !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Recursive Module Function Convert_REAL64_1d_To_CHAR_0d( Var, Len, Pos, Fmt ) result(String)
      real(REAL64)                                          ,intent(in)     ::  Var(:)                          !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    Pure Recursive Module Function Convert_REAL128_1d_To_CHAR_0d( Var, Len, Pos, Fmt ) result(String)
      real(REAL128)                                         ,intent(in)     ::  Var(:)                          !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      character(:)  ,allocatable                                            ::  String                          !< Character corresponding to the input number
    End Function

    ! **************************************************************************************************************
    !       PROCEDURES FOR CONVERTING VARIABLES-1D TO CHARACTER-1D
    ! **************************************************************************************************************

    Pure Module Function Convert_CHAR_1d_To_LOG_1d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
      logical                                                               ::  Output(size(Input))               !< Converted output variable
    End Function

    Pure Module Function Convert_CHAR_1d_To_INT8_1d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
      integer(INT8)                                                         ::  Output(size(Input))               !< Converted output variable
    End Function

    Pure Module Function Convert_CHAR_1d_To_INT16_1d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
      integer(INT16)                                                        ::  Output(size(Input))               !< Converted output variable
    End Function

    Pure Module Function Convert_CHAR_1d_To_INT32_1d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
      integer(INT32)                                                        ::  Output(size(Input))               !< Converted output variable
    End Function

    Pure Module Function Convert_CHAR_1d_To_INT64_1d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
      integer(INT64)                                                        ::  Output(size(Input))               !< Converted output variable
    End Function

    Pure Module Function Convert_CHAR_1d_To_REAL32_1d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
      real(REAL32)                                                          ::  Output(size(Input))               !< Converted output variable
    End Function

    Pure Module Function Convert_CHAR_1d_To_REAL64_1d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
      real(REAL64)                                                          ::  Output(size(Input))               !< Converted output variable
    End Function

    Pure Module Function Convert_CHAR_1d_To_REAL128_1d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
      real(REAL128)                                                         ::  Output(size(Input))               !< Converted output variable
    End Function


    ! **************************************************************************************************************
    !       PROCEDURES FOR CONVERTING CHARACTERS TO OTHER TYPES: LOGICAL, INTEGER, REAL
    ! **************************************************************************************************************

    Pure Elemental Module Function Convert_CHAR_0d_To_LOG_0d( String ) result(Value)
      character(*)                                          ,intent(in)     ::  String                          !< Input character string to be converted
      logical                                                               ::  Value                           !< Logical variable corresponding to the input string
    End Function

    Pure Elemental Module Function Convert_CHAR_0d_To_INT8_0d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input                           !< Input character to be converted
      integer(INT8)                                                         ::  Output                          !< Output integer corresponding to the input character
    End Function

    Pure Elemental Module Function Convert_CHAR_0d_To_INT16_0d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input                           !< Input character to be converted
      integer(INT16)                                                        ::  Output                          !< Output integer corresponding to the input character
    End Function

    Pure Elemental Module Function Convert_CHAR_0d_To_INT32_0d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input                           !< Input character to be converted
      integer(INT32)                                                        ::  Output                          !< Output integer corresponding to the input character
    End Function

    Pure Elemental Module Function Convert_CHAR_0d_To_INT64_0d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input                           !< Input character to be converted
      integer(INT64)                                                        ::  Output                          !< Output integer corresponding to the input character
    End Function

    Pure Elemental Module Function Convert_CHAR_0d_To_REAL32_0d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input                           !< Input character to be converted
      real(REAL32)                                                          ::  Output                          !< Output real corresponding to the input string
    End Function

    Pure Elemental Module Function Convert_CHAR_0d_To_REAL64_0d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input                           !< Input character to be converted
      real(REAL64)                                                          ::  Output                          !< Output real corresponding to the input string
    End Function

    Pure Elemental Module Function Convert_CHAR_0d_To_REAL128_0d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input                           !< Input character to be converted
      real(REAL128)                                                         ::  Output                          !< Output real corresponding to the input string
    End Function


    ! **************************************************************************************************************
    !       PROCEDURES FOR CONVERTING LOGICAL TO OTHER TYPES: INTEGER, REAL, CHARACTER
    ! **************************************************************************************************************

    Pure Elemental Module Function Convert_LOG_0d_To_INT8_0d( Input ) result(Output)
      logical                                               ,intent(in)     ::  Input                           !< Input variable to be converted
      integer(INT8)                                                         ::  Output                          !< Convert output variable
    End Function

    Pure Elemental Module Function Convert_LOG_0d_To_INT16_0d( Input ) result(Output)
      logical                                               ,intent(in)     ::  Input                           !< Input variable to be converted
      integer(INT16)                                                        ::  Output                          !< Convert output variable
    End Function

    Pure Elemental Module Function Convert_LOG_0d_To_INT32_0d( Input ) result(Output)
      logical                                               ,intent(in)     ::  Input                           !< Input variable to be converted
      integer(INT32)                                                        ::  Output                          !< Convert output variable
    End Function

    Pure Elemental Module Function Convert_LOG_0d_To_INT64_0d( Input ) result(Output)
      logical                                               ,intent(in)     ::  Input                           !< Input variable to be converted
      integer(INT64)                                                        ::  Output                          !< Convert output variable
    End Function

    Pure Elemental Module Function Convert_LOG_0d_To_REAL32_0d( Input ) result(Output)
      logical                                               ,intent(in)     ::  Input                           !< Input variable to be converted
      real(REAL32)                                                          ::  Output                          !< Convert output variable
    End Function

    Pure Elemental Module Function Convert_LOG_0d_To_REAL64_0d( Input ) result(Output)
      logical                                               ,intent(in)     ::  Input                           !< Input variable to be converted
      real(REAL64)                                                          ::  Output                          !< Convert output variable
    End Function

    Pure Elemental Module Function Convert_LOG_0d_To_REAL128_0d( Input ) result(Output)
      logical                                               ,intent(in)     ::  Input                           !< Input variable to be converted
      real(REAL128)                                                         ::  Output                          !< Convert output variable
    End Function

    Pure Module Function Convert_LOG_0d_To_CHAR_0d( Input, T, F ) result(Output)
      logical                                               ,intent(in)     ::  Input                           !< Input variable to be converted
      character(*)                                ,optional ,intent(in)     ::  T                               !< Input used to indicates "True"
      character(*)                                ,optional ,intent(in)     ::  F                               !< Input used to indicates "False"
      character(:)  ,allocatable                                            ::  Output
    End Function





    Module Subroutine ConvertFromString( String, Value )
      character(*)                                          ,intent(in)     ::  String
      class(*)                                              ,intent(out)    ::  Value
    End Subroutine

    Pure Module Subroutine ConvertVariableKind_0d( From, To, Status )
      class(*)                                              ,intent(in)     ::  From
      class(*)                                              ,intent(inout)  ::  To
      integer                                     ,optional ,intent(out)    ::  Status
    End Subroutine

    Pure Module Subroutine Convert_From_CHAR_0d_To_LOG_0d( From, To, Status )
      character(*)                                          ,intent(in)     ::  From
      logical                                               ,intent(out)    ::  To
      integer                                     ,optional ,intent(out)    ::  Status
    End Subroutine
    Pure Module Subroutine Convert_From_CHAR_0d_To_INT8_0d( From, To, Status )
      character(*)                                          ,intent(in)     ::  From
      integer(INT8)                                         ,intent(out)    ::  To
      integer                                     ,optional ,intent(out)    ::  Status
    End Subroutine
    Pure Module Subroutine Convert_From_CHAR_0d_To_INT16_0d( From, To, Status )
      character(*)                                          ,intent(in)     ::  From
      integer(INT16)                                        ,intent(out)    ::  To
      integer                                     ,optional ,intent(out)    ::  Status
    End Subroutine
    Pure Module Subroutine Convert_From_CHAR_0d_To_INT32_0d( From, To, Status )
      character(*)                                          ,intent(in)     ::  From
      integer(INT32)                                        ,intent(out)    ::  To
      integer                                     ,optional ,intent(out)    ::  Status
    End Subroutine
    Pure Module Subroutine Convert_From_CHAR_0d_To_INT64_0d( From, To, Status )
      character(*)                                          ,intent(in)     ::  From
      integer(INT64)                                        ,intent(out)    ::  To
      integer                                     ,optional ,intent(out)    ::  Status
    End Subroutine
    Pure Module Subroutine Convert_From_CHAR_0d_To_REAL32_0d( From, To, Status )
      character(*)                                          ,intent(in)     ::  From
      real(REAL32)                                          ,intent(out)    ::  To
      integer                                     ,optional ,intent(out)    ::  Status
    End Subroutine
    Pure Module Subroutine Convert_From_CHAR_0d_To_REAL64_0d( From, To, Status )
      character(*)                                          ,intent(in)     ::  From
      real(REAL64)                                          ,intent(out)    ::  To
      integer                                     ,optional ,intent(out)    ::  Status
    End Subroutine
    Pure Module Subroutine Convert_From_CHAR_0d_To_REAL128_0d( From, To, Status )
      character(*)                                          ,intent(in)     ::  From
      real(REAL128)                                         ,intent(out)    ::  To
      integer                                     ,optional ,intent(out)    ::  Status
    End Subroutine






















    Pure Module Function RemoveQuotes_0d( InputString ) result(OutputString)
      character(*)                                          ,intent(in)     ::  InputString
      character(:)  ,allocatable                                            ::  OutputString
    End Function
    Pure Module Function RemoveQuotes_1d( InputString ) result(OutputString)
      character(*)  ,dimension(:)                           ,intent(in)     ::  InputString
      character(:)  ,dimension(:) ,allocatable                              ::  OutputString
    End Function

    Pure Module Function Quote_0d( String ) result(QuotedString)
      character(*)                                          ,intent(in)     ::  String
      character(:)  ,allocatable                                            ::  QuotedString
    End Function
    Pure Module Function Quote_1d( String ) result(QuotedString)
      character(*)  ,dimension(:)                           ,intent(in)     ::  String
      character(:)  ,dimension(:) ,allocatable                              ::  QuotedString
    End Function

    Pure Module Function RemoveDuplicateCharacters( String, SubString ) result(CleanedString)
      character(*)                                          ,intent(in)     ::  String
      character(*)                                          ,intent(in)     ::  SubString
      character(:)  ,allocatable                                            ::  CleanedString
    End Function

    Pure Module Function StartWith_0d( String, SubString, Trimed, CaseSensitive ) result(Indicator)
      character(*)                                          ,intent(in)     ::  String
      character(*)                                          ,intent(in)     ::  SubString
      logical                                     ,optional ,intent(in)     ::  Trimed                          !< Indicator whether the strings should be trimed (Default: True)
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the strings should be case sensitive (Default: True)
      logical                                                               ::  Indicator                       !< Indicator whether 'String' starts witgh the sub-string 'SubString'
    End Function

    Pure Module Function StartWith_1d( String, SubStrings, Trimed, CaseSensitive ) result(Indicator)
      character(*)                                          ,intent(in)     ::  String
      character(*)  ,dimension(:)                           ,intent(in)     ::  SubStrings
      logical                                     ,optional ,intent(in)     ::  Trimed                          !< Indicator whether the strings should be trimed (Default: True)
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the strings should be case sensitive (Default: True)
      logical                                                               ::  Indicator                       !< Indicator whether 'String' starts witgh the sub-string 'SubString'
    End Function

    Pure Module Function GetAssociatedString(A) result(B)
      character(*)                                          ,intent(in)     ::  A
      character(:)  ,allocatable                                            ::  B
    End Function

    Pure Module Function GetFinalIndexExponentPart( String ) result(Last)
      character(*)                                          ,intent(in)     ::  String
      integer                                                               ::  Last
    End Function

    Pure Module Function IsExponentCharacter( s ) result(Indicator)
      character(1)                                          ,intent(in)     ::  s
      logical                                                               ::  Indicator
    End Function

    Pure Module Function IsPostExponentCharacter( s ) result(Indicator)
      character(1)                                          ,intent(in)     ::  s
      logical                                                               ::  Indicator
    End Function

    Pure Module Function NiceInteger(Number) result(String)
      integer                                               ,intent(in)     ::  Number
      character(:)  ,allocatable                                            ::  String
    End Function

    Module Pure Function GetSeconds(String) result(Seconds)
      character(*)                                          ,intent(in)     ::  String
      real(REAL64)                                                          ::  Seconds
    End Function

    Module Pure Subroutine EmptyString_0d( String )
      character(:)  ,allocatable                            ,intent(out)    ::  String
    End Subroutine

    Module Pure Subroutine EmptyString_1d( String )
      character(:)  ,allocatable                            ,intent(out)    ::  String(:)
    End Subroutine

    Module Pure Subroutine EmptyString_2d( String )
      character(:)  ,allocatable                            ,intent(out)    ::  String(:,:)
    End Subroutine

    Module Pure Function RemoveComment( InputString, Comment ) result(OutputString)
      character(*)                                          ,intent(in)     ::  InputString
      character(*)                                          ,intent(in)     ::  Comment
      character(:)  ,allocatable                                            ::  OutputString
    End Function

  End Interface


!   Decoration
  Interface

    Module Subroutine Frame( InputString, OutputString )
      character(*)                                          ,intent(in)     ::  InputString                   !< Character string around which a frame is to be created
      character(:)  ,allocatable                            ,intent(out)    ::  OutputString(:)               !< Character string representing the framed text
    End Subroutine

    Module Function Justify( InputString, N, M ) result(OutputString)
      character(*)                                          ,intent(in)     ::  InputString(:)
      integer                                               ,intent(in)     ::  N
      integer                                     ,optional ,intent(in)     ::  M
      character(:)  ,allocatable                                            ::  OutputString(:)
    End Function

  End Interface

contains

! *** Modification by Przemyslaw Rostkowski 
! 06/24/2019
! moved VecTrim procedures to the module to ensure that allocation works

    Pure Module Function VecTrim_0d( Input ) result(Output)
      character(*)                                          ,intent(in)     ::  Input
      character(:)                          ,allocatable                    ::  Output
      Output = trim(Input)
    End Function

    Pure Module Function VecTrim_1d( Input ) result(Output)
      character(*)  ,dimension(:)                           ,intent(in)     ::  Input
      character(:)  ,dimension(:)           ,allocatable                    ::  Output
      integer                                                               ::  i
!    # ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!      Block
!        character(:) ,allocatable   :: WASTR(:)
!        integer                     :: N
!        N   =   LenTrim(Input)
!        allocate( character(N) :: WASTR(size(Input)) )
!        do i = 1,size(Input)
!          WASTR(i)    =   trim( Input(i) )
!        end do
!        call move_alloc( WASTR, Output )
!    !     allocate( character(100) :: Output(100) )
!    !     Output    =   WASTR
!      End Block
!!    # else
      allocate( character(LenTrim(Input)) :: Output(size(Input)) )
      do i = 1,size(Input)
        Output(i)    =   trim( Input(i) )
      end do
!    # endif
    End Function

    Pure Module Function VecTrim_2D( Input ) result(Output)
      character(*)  ,dimension(:,:)                           ,intent(in)   ::  Input
      character(:)  ,dimension(:,:)           ,allocatable                  ::  Output
      integer                                                               ::  i, j
!    # ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!      character(:)  ,dimension(:,:)         ,allocatable                    ::  tmp
!      allocate( character(LenTrim(Input)) :: tmp(size(Input,1),size(Input,2)) )
!      do j = 1,size(Input,2)
!      do i = 1,size(Input,1)
!        tmp(i,j)  =   trim( Input(i,j) )
!      end do
!      end do
!      call move_alloc( tmp, Output )
!    # else
      allocate( character(LenTrim(Input)) :: Output(size(Input,1),size(Input,2)) )
      do j = 1,size(Input,2)
      do i = 1,size(Input,1)
        Output(i,j)  =   trim( Input(i,j) )
      end do
      end do
!    # endif
    End Function

End Module
