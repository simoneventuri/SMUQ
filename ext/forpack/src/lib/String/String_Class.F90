Module String_Class

  implicit none

  private
  public        ::  String_Type
  public        ::  LoadCharactersInStrings

  Type  ::  String_Type
    character(:)        ,allocatable            ::  Value
    logical                                     ::  Is_Set          =       .False.
    logical                                     ::  i_Trim          =       .True.
    logical                                     ::  i_AdjustL       =       .True.
    logical                                     ::  i_AdjustR       =       .False.
    logical                                     ::  i_Compact       =       .False.
    logical                                     ::  i_UpperCase     =       .False.
  contains
    procedure   ,public         ::  Initialize      =>      InitializeStringFromOptions
    procedure   ,public         ::  Free            =>      FreeString
    procedure   ,public         ::  Finalize        =>      Finalize_String
    generic     ,public         ::  Set_Value       =>      Set_Value_From_Character, Set_Value_From_Object
    procedure   ,private        ::  Set_Value_From_Character
    procedure   ,private        ::  Set_Value_From_Object
    procedure   ,public         ::  Read_From_File  =>      Read_String_From_File
    procedure   ,public         ::  GetValue        =>      GetStringValue
    procedure   ,public         ::  Length          =>      GetStringLength
    procedure   ,public         ::  Output          =>      Output_String
    procedure   ,public         ::  Crop            =>      Crop_String
    procedure   ,public         ::  Parse           =>      Parse_String
    procedure   ,public         ::  UpperCase       =>      UpperCase_String
    procedure   ,public         ::  Compact         =>      Compact_String
    procedure   ,public         ::  Is_SubString_Present
    procedure   ,public         ::  Is_Empty
    procedure   ,public         ::  Is_Equal
    procedure   ,public         ::  Is_NotEqual
    procedure   ,public         ::  GetSubStringIndex
    procedure   ,public         ::  GetSubString
    procedure   ,public         ::  Remove_At_Left_Of

    generic     ,public         ::  assignment(=)   =>      Assign_Character, Assign_String
    procedure   ,private        ::  Assign_Character
    procedure   ,private        ::  Assign_String

    procedure   ,public         ::  Reverse
    procedure   ,public         ::  RemoveComment =>  RemoveCommentFromString

    generic :: operator(//) => string_concat_string,    &
                               string_concat_character, &
                               character_concat_string              !< Concatenation operator overloading.
    ! concatenation operators
    procedure, private, pass(lhs) :: string_concat_string           !< Concatenation with string.
    procedure, private, pass(lhs) :: string_concat_character        !< Concatenation with character.
    procedure, private, pass(rhs) :: character_concat_string        !< Concatenation with character (inverted).
!     procedure, private, pass(lhs) :: string_concat_string_string    !< Concatenation with string (string output).
!     procedure, private, pass(lhs) :: string_concat_character_string !< Concatenation with character (string output).
!     procedure, private, pass(rhs) :: character_concat_string_string !< Concatenation with character (inverted, string output).

  End Type

  Interface           String_Type
    Module Procedure  NewStringFromString0d
    Module Procedure  NewStringFromLogical0d
    Module Procedure  NewStringFromInteger
    Module Procedure  NewStringFromReal4
    Module Procedure  NewStringFromReal8
    Module Procedure  NewStringFromReal16
  End Interface

! Procedure for intializing a new String object
  Interface
    Module Subroutine InitializeStringFromOptions( This, Value, i_Trim, i_AdjustL, i_AdjustR, i_Compact, i_UpperCase )
      class(String_Type)                                    ,intent(out)    ::  This
      character(*)                                ,optional ,intent(in)     ::  Value
      logical                                     ,optional ,intent(in)     ::  i_Trim
      logical                                     ,optional ,intent(in)     ::  i_AdjustL
      logical                                     ,optional ,intent(in)     ::  i_AdjustR
      logical                                     ,optional ,intent(in)     ::  i_Compact
      logical                                     ,optional ,intent(in)     ::  i_UpperCase
    End Subroutine
    Pure Module Function NewStringFromString0d( Var, Len, Pos, Fmt ) result(String)
      character(*)                                          ,intent(in)     ::  Var                             !< String to be converted into a string (with possibly a different length
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      type(String_Type)                                                     ::  String                          !< String object
    End Function
    Pure Module Function NewStringFromLogical0d( Var ) result(String)
      logical                                               ,intent(in)     ::  Var                             !< Logical to be converted into a string
      type(String_Type)                                                     ::  String                          !< String object
    End Function
    Pure Module Function NewStringFromInteger( Var, LeadingZeros, Len, Pos, Fmt ) result(String)
      integer                                               ,intent(in)     ::  Var                             !< Number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  LeadingZeros                    !< Number of leading zeros
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      type(String_Type)                                                     ::  String                          !< String object
    End Function
    Pure Module Function NewStringFromReal4( Var, Len, Pos, Fmt ) result(String)
      real(4)                                               ,intent(in)     ::  Var                             !< Real number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      type(String_Type)                                                     ::  String                          !< String object
    End Function
    Pure Module Function NewStringFromReal8( Var, Len, Pos, Fmt ) result(String)
      real(8)                                               ,intent(in)     ::  Var                             !< Real number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      type(String_Type)                                                     ::  String                          !< String object
    End Function
    Pure Module Function NewStringFromReal16( Var, Len, Pos, Fmt ) result(String)
      real(16)                                              ,intent(in)     ::  Var                             !< Real number to be converted into a string
      integer                                     ,optional ,intent(in)     ::  Len                             !< Length of the output string
      character(1)                                ,optional ,intent(in)     ::  Pos                             !< Position of the string: L:Left, C:Center, R:Right
      character(*)                                ,optional ,intent(in)     ::  Fmt                             !< Format used to describe the number
      type(String_Type)                                                     ::  String                          !< String object
    End Function
  End Interface

  Interface
    Pure Module Subroutine FreeString( This )
      class(String_Type)                                    ,intent(inout)  ::  This
    End Subroutine

    Module Subroutine Finalize_String( This )
      class(String_Type)                                    ,intent(out)    ::  This
    End Subroutine

    Elemental Pure Module Subroutine Assign_Character( Lhs, Rhs )
      class(String_Type)                                    ,intent(inout)  ::  Lhs
      character(*)                                          ,intent(in)     ::  Rhs
    End Subroutine

    Elemental Pure Module Subroutine Assign_String( Lhs, Rhs )
      class(String_Type)                                    ,intent(inout)  ::  Lhs
      type(String_Type)                                     ,intent(in)     ::  Rhs
    End Subroutine

    Module Subroutine Output_String( This, Unit )
      class(String_Type)                                    ,intent(in)     ::  This
      integer                                               ,intent(in)     ::  Unit
    End Subroutine

    Module Subroutine Read_String_From_File( This, Unit, ios, Continuation_Character, Comment_Character, i_Debug )
      class(String_Type)                                    ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  Unit
      integer                                     ,optional ,intent(out)    ::  ios
      character(*)                                ,optional ,intent(in)     ::  Continuation_Character
      character(*)                                ,optional ,intent(in)     ::  Comment_Character
      logical                                     ,optional ,intent(in)     ::  i_Debug                         !< Debugging indicator
    End Subroutine

    Pure Module Subroutine Set_Value_From_Character( This, Value, i_Trim, i_AdjustL, i_AdjustR, i_Compact, i_UpperCase )
      class(String_Type)                                    ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Value
      logical                                     ,optional ,intent(in)     ::  i_Trim
      logical                                     ,optional ,intent(in)     ::  i_AdjustL
      logical                                     ,optional ,intent(in)     ::  i_AdjustR
      logical                                     ,optional ,intent(in)     ::  i_Compact
      logical                                     ,optional ,intent(in)     ::  i_UpperCase
    End Subroutine

    Pure Module Subroutine Set_Value_From_Object( This, Object, i_Trim, i_AdjustL, i_AdjustR, i_Compact, i_UpperCase )
      class(String_Type)                                    ,intent(inout)  ::  This
      type(String_Type)                                     ,intent(in)     ::  Object
      logical                                     ,optional ,intent(in)     ::  i_Trim
      logical                                     ,optional ,intent(in)     ::  i_AdjustL
      logical                                     ,optional ,intent(in)     ::  i_AdjustR
      logical                                     ,optional ,intent(in)     ::  i_Compact
      logical                                     ,optional ,intent(in)     ::  i_UpperCase
    End Subroutine

    Module Subroutine Crop_String( This, iCrop )
      class(String_Type)                                    ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  iCrop   ! Index where the character string has to be cropped
    End Subroutine

    Module Subroutine Parse_String( This, Strings, Separator, EscRHS, IgnoreBetween  )
      class(String_Type)                                    ,intent(in)     ::  This
      type(String_Type) ,allocatable                        ,intent(out)    ::  Strings(:)
      character(*)                                          ,intent(in)     ::  Separator                       !< Separation character string
      character(1)                                ,optional ,intent(in)     ::  EscRHS(:)                          !< RHS escape character
      character(*)                                ,optional ,intent(in)     ::  IgnoreBetween(:)
    End Subroutine

    Module Subroutine Remove_At_Left_Of( This, SubStr )
      class(String_Type)                                    ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  SubStr
    End Subroutine

    Pure Module Function GetStringValue( This, Length, Trimed ) result(Value)
      class(String_Type)                                    ,intent(in)     ::  This
      integer                                     ,optional ,intent(in)     ::  Length
      logical                                     ,optional ,intent(in)     ::  Trimed
      character(:)  ,allocatable                                            ::  Value
    End Function

    Pure Module Function GetStringLength( This, Trimed ) result(Length)
      class(String_Type)                                    ,intent(in)     ::  This
      logical                                     ,optional ,intent(in)     ::  Trimed
      integer                                                               ::  Length
    End Function

    Module Function GetSubStringIndex( This, SubStr, CaseSensitive ) result(iSubStr)
      class(String_Type)                                    ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  SubStr
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      integer                                                               ::  iSubStr
    End Function

    Module Function Is_SubString_Present( This, SubStr ) result(Indicator)
      class(String_Type)                                    ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  SubStr
      logical                                                               ::  Indicator
    End Function

    Module Function Is_Empty( This ) result(Indicator)
      class(String_Type)                                    ,intent(in)     ::  This
      logical                                                               ::  Indicator
    End Function

    Module Function Is_Equal( This, Value ) result(Indicator)
      class(String_Type)                                    ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Value
      logical                                                               ::  Indicator
    End Function

    Module Function Is_NotEqual( This, Value ) result(Indicator)
      class(String_Type)                                    ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Value
      logical                                                               ::  Indicator
    End Function

    Module Function GetSubString( This, Str1, Str2, CaseSensitive ) result(Value)
      class(String_Type)                                    ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Str1
      character(*)                                          ,intent(in)     ::  Str2
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      character(:)  ,allocatable                                            ::  Value
    End Function

    Pure Module Subroutine UpperCase_String( This )
      class(String_Type)                                    ,intent(inout)  ::  This
    End Subroutine

    Pure Module Subroutine Compact_String( This )
      class(String_Type)                                    ,intent(inout)  ::  This
    End Subroutine

    Elemental Module Function Reverse(This) result(reversed)
      class(String_Type)                                    ,intent(in)     ::  This
      type(String_Type)                                                     ::  reversed        !< The reversed string.
    End Function

    Pure Module Function string_concat_string(lhs, rhs) result(concat)
      class(String_Type)                                    ,intent(in)     :: lhs    !< Left hand side.
      type(String_Type)                                     ,intent(in)     :: rhs    !< Right hand side.
      character(:)  ,allocatable                                            :: concat !< Concatenated string.
    End Function

    Pure Module Function string_concat_character(lhs, rhs) result(concat)
      class(String_Type)                                    ,intent(in)     :: lhs    !< Left hand side.
      character(*)                                          ,intent(in)     :: rhs    !< Right hand side.
      character(:)  ,allocatable                                            :: concat !< Concatenated string.
    End Function

    Pure Module Function character_concat_string(lhs, rhs) result(concat)
      character(*)                                          ,intent(in)     :: lhs    !< Left hand side.
      class(String_Type)                                    ,intent(in)     :: rhs    !< Right hand side.
      character(:)  ,allocatable                                        :: concat !< Concatenated string.
    End Function

    Module Pure Subroutine RemoveCommentFromString( This, Comment )
      class(String_Type)                                    ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Comment
    End Subroutine

  End Interface

  Interface

    Module Subroutine LoadCharactersInStrings( Charac, Strings, LogLevel )
      character(*)  ,dimension(:)                           ,intent(in)     ::  Charac                          !<
      type(String_Type)     ,allocatable    ,dimension(:)   ,intent(out)    ::  Strings                           !< Vector of String object corresponding to lines stored in a file
      integer                                     ,optional ,intent(in)     ::  LogLevel                        !< Indicator of the log level
    End Subroutine

  End Interface


End Module
