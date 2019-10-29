SubModule(String_Class) String_SubClass

  implicit none

  contains

Module Procedure InitializeStringFromOptions
  call This%Free()
  if ( present(i_Trim)    )     This%i_Trim        =       i_Trim
  if ( present(i_AdjustL) )     This%i_AdjustL     =       i_AdjustL
  if ( present(i_AdjustR) )     This%i_AdjustR     =       i_AdjustR
  if ( present(i_Compact) )     This%i_Compact     =       i_Compact
  if ( present(i_UpperCase) )   This%i_UpperCase   =       i_UpperCase
  if ( present(Value)     )  call This%Set_Value( Value )
End Procedure


Module Procedure NewStringFromString0d
  use String_Module     ,only:  Convert_To_String
  String    =   Convert_To_String( Var, Len, Pos, Fmt )
End Procedure
Module Procedure NewStringFromLogical0d
  use String_Module     ,only:  Convert_To_String
  String    =   Convert_To_String( Var )
End Procedure
Module Procedure NewStringFromInteger
  use String_Module     ,only:  Convert_To_String
  String    =   Convert_To_String( Var, LeadingZeros, Len, Pos, Fmt )
End Procedure
Module Procedure NewStringFromReal4
  use String_Module     ,only:  Convert_To_String
  String    =   Convert_To_String( Var, Len, Pos, Fmt )
End Procedure
Module Procedure NewStringFromReal8
  use String_Module     ,only:  Convert_To_String
  String    =   Convert_To_String( Var, Len, Pos, Fmt )
End Procedure
Module Procedure NewStringFromReal16
  use String_Module     ,only:  Convert_To_String
  String    =   Convert_To_String( Var, Len, Pos, Fmt )
End Procedure











Module Procedure FreeString
  if ( allocated(This%Value) ) deallocate(This%Value)
  This%Is_Set          =       .False.
  This%i_Trim          =       .True.
  This%i_AdjustL       =       .True.
  This%i_AdjustR       =       .False.
  This%i_Compact       =       .False.
  This%i_UpperCase     =       .False.
End Procedure

Module Subroutine Finalize_String( This )
  class(String_Type)                    ,intent(out)    ::  This
  if ( allocated(This%Value) ) deallocate(This%Value)
  This%Is_Set          =       .False.
  This%i_Trim          =       .True.
  This%i_AdjustL       =       .True.
  This%i_AdjustR       =       .False.
  This%i_Compact       =       .False.
  This%i_UpperCase     =       .False.
End Subroutine

! This procedure assigns a character string to a String object.
! This procedure should be used with caution since its call will reset all the indicators contains in
! the String object to their default values because of the intent(out) attribute.
! In lots of cases, this is indesirable.
Elemental Pure Module Subroutine Assign_Character( Lhs, Rhs )
  class(String_Type)                    ,intent(inout)  ::  Lhs
  character(*)                          ,intent(in)     ::  Rhs
  call Lhs%Free()
  call Lhs%Set_Value( Rhs )
End Subroutine

! This procedure assigns a String object to another String object. A copy is done.
Elemental Pure Module Subroutine Assign_String( Lhs, Rhs )
  class(String_Type)                    ,intent(inout)  ::  Lhs
  type(String_Type)                     ,intent(in)     ::  Rhs
  call Lhs%Free()
  Lhs%Value             =       Rhs%Value
  Lhs%Is_Set            =       Rhs%Is_Set
  Lhs%i_Trim            =       Rhs%i_Trim
  Lhs%i_AdjustL         =       Rhs%i_AdjustL
  Lhs%i_AdjustR         =       Rhs%i_AdjustR
  Lhs%i_Compact         =       Rhs%i_Compact
  Lhs%i_UpperCase       =       Rhs%i_UpperCase
End Subroutine

Module Subroutine Output_String( This, Unit )
  class(String_Type)                    ,intent(in)     ::  This
  integer                               ,intent(in)     ::  Unit

  write(Unit,"(2x'Output for String_Type')")
  write(Unit,"(2x'This%i_Trim      = ',g0)") This%i_Trim
  write(Unit,"(2x'This%i_AdjustL   = ',g0)") This%i_AdjustL
  write(Unit,"(2x'This%i_AdjustR   = ',g0)") This%i_AdjustR
  write(Unit,"(2x'This%i_Compact   = ',g0)") This%i_Compact
  write(Unit,"(2x'This%i_UpperCase = ',g0)") This%i_UpperCase
  if ( This%Is_Set ) then
   write(Unit,"(2x'This%Value        = ',g0)") This%Value
   write(Unit,"(2x'This%Length() = ',g0)") This%Length()
  end if

End Subroutine


Module Subroutine Read_String_From_File( This, Unit, ios, Continuation_Character, Comment_Character, i_Debug )

  class(String_Type)                    ,intent(inout)  ::  This
  integer                               ,intent(in)     ::  Unit
  integer                     ,optional ,intent(out)    ::  ios
  character(*)                ,optional ,intent(in)     ::  Continuation_Character
  character(*)                ,optional ,intent(in)     ::  Comment_Character
  logical                     ,optional ,intent(in)     ::  i_Debug                         !< Debugging indicator

  character(*)  ,parameter                              ::  ProcName = "Read_String_From_File"
  logical                                               ::  i_Debug_Loc

  integer                                               ::  i, j
  integer                                               ::  ios_Local
  character(1000)                                       ::  Long_String
  character(:)  ,allocatable                            ::  String
  character(:)  ,allocatable                            ::  String_Continu
  character(:)  ,allocatable                            ::  String_Comment

  i_Debug_Loc = .False.
  if ( present(i_Debug) ) i_Debug_Loc = i_Debug

  if (i_Debug_Loc) write(*,"('[',g0,']: ',g0)") ProcName, 'Entering'

  read(Unit,"(a)",iostat=ios_Local) Long_String                                                                 ! Reading a line from a file and storing it in a regular character string

  if (i_Debug_Loc) write(*,"('[',g0,']: Long_String = ',g0)") ProcName, Long_String

  if ( present(ios) ) ios = ios_Local                                                                           ! If the io status indicator is present in the list of optional argument, then set its value
  if ( ios_Local /= 0 ) return                                                                                        ! If non-zero status, then exiting the procedure
  call This%Set_Value( Long_String )                                                                            ! Assigning the character string which has been read from the file to the String object


  String_Continu    =     ""
  String_Comment    =     ""
  if ( present(Continuation_Character) ) String_Continu = Continuation_Character
  if ( present(Comment_Character     ) ) String_Comment = Comment_Character

!   if (i_Debug_Loc) then
!     write(*,"('[',g0,']: String_Continu = ',g0)") ProcName, String_Continu
!     write(*,"('[',g0,']: String_Comment = ',g0)") ProcName, String_Comment
!   end if

  if (i_Debug_Loc) write(*,"('[',g0,']: Before removing comment: This%Value = ',g0)") ProcName, This%Value


! ==============================================================================================================
!   REMOVING THE ALL THE CHARACTERS AT THE RIGHT OF THE COMMENT CHARACTER, IF ANY
! ==============================================================================================================
  if ( len_trim(String_Comment) /= 0 ) then                                                                     ! If a comment character is specified
!     j     =     This%GetSubStringIndex( String_Comment )                                                      ! Getting the position of the comment character, if any: If the comment character is not found, then zero is returned
    j     =     index( This%Value, String_Comment )

    if (i_Debug_Loc) write(*,"('[',g0,']: j = ',g0)") ProcName, j
    if (i_Debug_Loc) write(*,"('[',g0,']: This%Value(1:j-1) = ',g0)") ProcName, This%Value(1:j-1)
    if (i_Debug_Loc) write(*,"('[',g0,']: This%Value(j:)    = ',g0)") ProcName, This%Value(j:)

    if ( j /= 0 ) then
      String      =     trim( This%Value(1:j-1) )
      call This%Set_Value( String )                                                      ! If a comment character is found, then removing all characters at the RHS of the comment character
    end if

  end if                                                                                                        ! End if case on specified comment character
  if (i_Debug_Loc) write(*,"('[',g0,']: After removing comment: This%Value = ',g0)") ProcName, This%Value
! ==============================================================================================================


  if ( len_trim(String_Continu) /= 0 ) then                                                                     ! If the "Continuation_Character" optional argument is present, then check if current line has a continuation character, and if so, add the next lines to it
    i     =     This%GetSubStringIndex( String_Continu )                                                      ! Getting the position of the continuation character, if any: If it is not found, then zero is returned
    if ( i /= 0 ) then                                                                                          ! If current line contains the continuation character, then ...
      do                                                                                                        ! Loop on the next lines to add them to the current line
        i       =       This%GetSubStringIndex(String_Continu)                                                ! Getting the position of the continuation character, if any
        if ( i == 0 ) exit                                                                                      ! If no continuation character in current line, then exit the loop
        read(Unit,"(a)",iostat=ios_Local) Long_String                                                           ! Reading the new line which should corresponds to the continued line
        if ( ios_Local /= 0 ) exit                                                                              ! If error during real, then exiting the loop
        String      =     trim(Long_String)
        if ( len_trim(String_Comment) /= 0 ) then                                                                     ! If a comment character is specified
          j     =     index( String, String_Comment )
          if ( j /= 0 ) String = String(1:j-1)                                                      ! If a comment character is found, then removing all characters at the RHS of the comment character
        end if                                                                                                        ! End if case on specified comment character
        call This%Set_Value( This%Value(1:i-1) // String )                                                 ! Adding the continued line to the character string: taking aonly the character at the LHS of the continuation character (excluding the continuation character itself)
      end do                                                                                                    ! End do loop on continuation lines
    end if                                                                                                      ! End if case on presence of the continuation character
  end if                                                                                                        ! End if case on presence optional argument

  if (i_Debug_Loc) write(*,"('[',g0,']: After dealing with continuation lines: This%Value = ',g0)") ProcName, This%Value
  if (i_Debug_Loc) write(*,"('[',g0,']: ',g0)") ProcName, 'Exiting'

End Subroutine



Pure Module Subroutine Set_Value_From_Character( This, Value, i_Trim, i_AdjustL, i_AdjustR, i_Compact, i_UpperCase )

  class(String_Type)                    ,intent(inout)  ::  This
  character(*)                          ,intent(in)     ::  Value
  logical                     ,optional ,intent(in)     ::  i_Trim
  logical                     ,optional ,intent(in)     ::  i_AdjustL
  logical                     ,optional ,intent(in)     ::  i_AdjustR
  logical                     ,optional ,intent(in)     ::  i_Compact
  logical                     ,optional ,intent(in)     ::  i_UpperCase

  logical                                     ::  i_Trim_Loc
  logical                                     ::  i_AdjustL_Loc
  logical                                     ::  i_AdjustR_Loc
  logical                                     ::  i_Compact_Loc
  logical                                     ::  i_UpperCase_Loc

  This%Value    =       Value
  This%Is_Set   =       .True.

  i_Compact_Loc = This%i_Compact                ! This action must be the 1st one to be performed
  if ( present(i_Compact) ) i_Compact_Loc = i_Compact
  if ( i_Compact_Loc ) call This%Compact()

  i_Trim_Loc = This%i_Trim
  if ( present(i_Trim) ) i_Trim_Loc = i_Trim
  if ( i_Trim_Loc ) This%Value = trim(This%Value)

  i_AdjustL_Loc = This%i_AdjustL
  if ( present(i_AdjustL) ) i_AdjustL_Loc = i_AdjustL
  if ( i_AdjustL_Loc ) This%Value = adjustl(This%Value)

  i_AdjustR_Loc = This%i_AdjustR
  if ( present(i_AdjustR) ) i_AdjustR_Loc = i_AdjustR
  if ( i_AdjustR_Loc ) This%Value = adjustr(This%Value)

  i_UpperCase_Loc = This%i_UpperCase
  if ( present(i_UpperCase) ) i_UpperCase_Loc = i_UpperCase
  if ( i_UpperCase_Loc ) call This%UpperCase()

End Subroutine

Pure Module Subroutine Set_Value_From_Object( This, Object, i_Trim, i_AdjustL, i_AdjustR, i_Compact, i_UpperCase )
  class(String_Type)                    ,intent(inout)  ::  This
  type(String_Type)                     ,intent(in)     ::  Object
  logical                     ,optional ,intent(in)     ::  i_Trim
  logical                     ,optional ,intent(in)     ::  i_AdjustL
  logical                     ,optional ,intent(in)     ::  i_AdjustR
  logical                     ,optional ,intent(in)     ::  i_Compact
  logical                     ,optional ,intent(in)     ::  i_UpperCase
  character(:)  ,allocatable                            ::  Value
  Value         =       Object%GetValue()
  call This%Set_Value( Value, i_Trim, i_AdjustL, i_AdjustR, i_Compact, i_UpperCase )
End Subroutine

Module Subroutine Crop_String( This, iCrop )
  class(String_Type)                    ,intent(inout)  ::  This
  integer                               ,intent(in)     ::  iCrop   ! Index where the character string has to be cropped
  This%Value    =       This%Value(1:iCrop)
End Subroutine

Module Subroutine Parse_String( This, Strings, Separator, EscRHS, IgnoreBetween  )

  use String_Module     ,only:  Parse

  class(String_Type)                                    ,intent(in)     ::  This
  type(String_Type)     ,dimension(:)   ,allocatable    ,intent(out)    ::  Strings
  character(*)                                          ,intent(in)     ::  Separator                       !< Separation character string
  character(1)  ,dimension(:)                 ,optional ,intent(in)     ::  EscRHS                          !< RHS escape character
  character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  IgnoreBetween

  character(:)  ,allocatable                                            ::  Value
  character(:)  ,allocatable    ,dimension(:)                           ::  Values

  Value         =       This%GetValue()
  call Parse( Value, Separator, Values, EscRHS, IgnoreBetween )
  allocate( Strings(size(Values)) )
  Strings       =       Values

End Subroutine



! This procedure removes all characters from the String at the left of a given SubString
Module Subroutine Remove_At_Left_Of( This, SubStr )
  use String_Module     ,only:  RemoveLeftChar
  class(String_Type)                    ,intent(inout)  ::  This
  character(*)                          ,intent(in)     ::  SubStr
  character(:)  ,allocatable                            ::  Value
  Value         =       RemoveLeftChar( This%Value, SubStr )
  call This%Set_Value( Value )
End Subroutine

Module Procedure GetStringValue
  use String_Module     ,only:  SetLength
  Value     =   This%Value
  if ( present(Length) ) then
    Value   =   SetLength( Value, Length )
  end if
  if ( present(Trimed) ) then
    if ( Trimed ) Value = trim(Value)
  end if
End Procedure

Module Procedure GetStringLength
  Length    =   Len(This%Value)
  if ( present(Trimed) ) then
    if ( Trimed ) Length  = len_trim(This%Value)
  end if
End Procedure

! This procedure returns the starting position of a substring of string, or zero if it does not occur as a substring
Module Function GetSubStringIndex( This, SubStr, CaseSensitive ) result(iSubStr)

  use String_Module     ,only:  UpperCase

  class(String_Type)                    ,intent(in)     ::  This
  character(*)                          ,intent(in)     ::  SubStr
  logical                     ,optional ,intent(in)     ::  CaseSensitive
  integer                                               ::  iSubStr

  logical                                               ::  CaseSensitive_
  character(:)  ,allocatable                            ::  Value, SubStr_

  CaseSensitive_  = .False.
  if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive

!   write(*,"('[',g0,']: ',g0)") 'GetSubStringIndex', 'Entering'
!   write(*,"('[',g0,']: This%Value = ',g0)") 'GetSubStringIndex', This%Value
!   write(*,"('[',g0,']: CaseSensitive_ = ',g0)") 'GetSubStringIndex', CaseSensitive_
  if ( CaseSensitive_ ) then
!   write(*,"('[',g0,']: Value = ',g0)") 'GetSubStringIndex', Value
!   write(*,"('[',g0,']: SubStr_ = ',g0)") 'GetSubStringIndex', SubStr_
!   write(*,"('[',g0,']: iSubStr = ',g0)") 'GetSubStringIndex', iSubStr
    iSubStr   =       index( This%Value, SubStr )
  else
    Value     =       UpperCase( This%Value )
    SubStr_   =       UpperCase( SubStr )
    iSubStr   =       index( Value, SubStr_ )
!   write(*,"('[',g0,']: Value = ',g0)") 'GetSubStringIndex', Value
!   write(*,"('[',g0,']: SubStr_ = ',g0)") 'GetSubStringIndex', SubStr_
!   write(*,"('[',g0,']: iSubStr = ',g0)") 'GetSubStringIndex', iSubStr
  end if
End Function

! This procedure returns true if the SubStr is present in the string value
Module Function Is_SubString_Present( This, SubStr ) result(Indicator)
  class(String_Type)                    ,intent(in)     ::  This
  character(*)                          ,intent(in)     ::  SubStr
  logical                                               ::  Indicator
  Indicator     =       This%GetSubStringIndex( SubStr ) /= 0
End Function

! This procedure returns true if the String is empty, that is if has a zero length
Module Function Is_Empty( This ) result(Indicator)
  class(String_Type)                    ,intent(in)     ::  This
  logical                                               ::  Indicator
  Indicator     =       This%Length() == 0
End Function

! This procedure returns true if the value of the String object is equal to an input character string
Module Function Is_Equal( This, Value ) result(Indicator)
  class(String_Type)                    ,intent(in)     ::  This
  character(*)                          ,intent(in)     ::  Value
  logical                                               ::  Indicator
  Indicator     =       ( This%Value == Value )
End Function

! This procedure returns true if the value of the String object is not equal to an input character string
Module Function Is_NotEqual( This, Value ) result(Indicator)
  class(String_Type)                    ,intent(in)     ::  This
  character(*)                          ,intent(in)     ::  Value
  logical                                               ::  Indicator
  Indicator     =       ( This%Value /= Value )
End Function



Module Function GetSubString( This, Str1, Str2, CaseSensitive ) result(Value)
  class(String_Type)                    ,intent(in)     ::  This
  character(*)                          ,intent(in)     ::  Str1
  character(*)                          ,intent(in)     ::  Str2
  logical                     ,optional ,intent(in)     ::  CaseSensitive
  character(:)  ,allocatable                            ::  Value
  integer                                               ::  iIni
  integer                                               ::  iFin
!   logical                                               ::  CaseSensitive_
!   CaseSensitive_  = .False.
!   if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive_
!   write(*,"('[',g0,']: ',g0)") 'GetSubString', 'Entering'
  Value         =       ""
  iIni          =       This%GetSubStringIndex( Str1, CaseSensitive )
!   write(*,"('[',g0,']: iIni = ',g0)") 'GetSubString', iIni
  if ( iIni == 0 ) return
  iIni          =       iIni + len(Str1)
  iFin          =       This%GetSubStringIndex( Str2, CaseSensitive )
!   write(*,"('[',g0,']: iFin = ',g0)") 'GetSubString', iFin
  if ( iFin == 0 ) return
  iFin          =       iFin - 1
  Value =       This%Value(iIni:iFin)
!   write(*,"('[',g0,']: Value = ',g0)") 'GetSubString', Value
End Function


Pure Module Subroutine UpperCase_String( This )
  use String_Module      ,only:  UpperCase
  class(String_Type)                    ,intent(inout)  ::  This
  This%Value    =       UpperCase( This%Value )
End Subroutine

Pure Module Subroutine Compact_String( This )
  use String_Module      ,only:  Compact
  class(String_Type)                    ,intent(inout)  ::  This
  This%Value    =       Compact( This%Value )
End Subroutine












!  Return a reversed string.
Elemental Module Function Reverse(This) result(reversed)
  class(String_Type)                    ,intent(in)     ::  This
  type(String_Type)                                     ::  reversed        !< The reversed string.
  integer                                               ::  Length          !< Length of the string.
  integer                                               ::  i
  if (allocated(This%Value)) then
    reversed = This
    Length = len(This%Value)
    do i = 1,Length
      reversed%Value(i:i) = This%Value(Length-i+1:Length-i+1)
    enddo
  endif
End Function





! contatenation operators
Pure Module Function string_concat_string(lhs, rhs) result(concat)
  class(String_Type)                    ,intent(in)     :: lhs    !< Left hand side.
  type(String_Type)                     ,intent(in)     :: rhs    !< Right hand side.
  character(:)  ,allocatable                            :: concat !< Concatenated string.
  concat = ''
  if (allocated(lhs%Value)) concat = lhs%Value
  if (allocated(rhs%Value)) concat = concat//rhs%Value
End Function

Pure Module Function string_concat_character(lhs, rhs) result(concat)
  class(String_Type)                    ,intent(in)     :: lhs    !< Left hand side.
  character(*)                          ,intent(in)     :: rhs    !< Right hand side.
  character(:)  ,allocatable                            :: concat !< Concatenated string.
  if (allocated(lhs%Value)) then
    concat = lhs%Value//rhs
  else
    concat = rhs
  endif
End Function

Pure Module Function character_concat_string(lhs, rhs) result(concat)
  character(*)                          ,intent(in)     :: lhs    !< Left hand side.
  class(String_Type)                    ,intent(in)     :: rhs    !< Right hand side.
  character(:)  ,allocatable                            :: concat !< Concatenated string.
  if (allocated(rhs%Value)) then
    concat = lhs//rhs%Value
  else
    concat = lhs
  endif
End Function


! @TODO: This procedure is duplicated in InputReader_SubClass... maybe it could got in String Library
Module Procedure LoadCharactersInStrings

!   character(*)                                              ,parameter  ::  ProcName = "LoadCharactersInStrings"
!   logical                                                               ::  Debug
  integer                                                               ::  NLines                          ! Number of lines to be added: This will be the dimension of the output variable "Lines"
  integer                                                               ::  i
  type(String_Type)                                                     ::  String

!   Debug   =   .False.
!   if ( present(LogLevel) ) then
!     call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_HEAVYDEBUG)
!     Debug   =   Logger%On()
!   end if


!   if ( Debug ) call Logger%Write( "Initializing the string object" )
!   if ( Debug ) call Logger%Write( "-> Calling String%Initialize" )
  call String%Initialize( i_Trim=.True., i_AdjustL=.True., i_Compact=.True. )


!   if ( Debug ) call Logger%Write( "Getting the total number of lines to be considered" )
  NLines    =       size(Charac)
!   if ( Debug ) call Logger%Write( "-> Number of lines: NLines = ", NLines )

!   if ( Debug ) call Logger%Write( "Converting Characters to Strings" )
  allocate( Strings(NLines) )
  do i = 1,NLines
    call String%Set_Value( Charac(i) )
    Strings(i)    =   String
!     if ( Debug ) call Logger%Write( "-> i = ", i, "Strings(i)%GetValue = ", Strings(i)%GetValue() )
  end do

!   if ( present(LogLevel) ) call Logger%Exiting()

End Procedure


!   REMOVING THE ALL THE CHARACTERS AT THE RIGHT OF THE COMMENT CHARACTER, IF ANY
Module Procedure RemoveCommentFromString
  use String_Module     ,only:  RemoveComment
  character(:)  ,allocatable                                            ::  String
  String  =   RemoveComment( This%Value, Comment )
  call This%Set_Value( String )
End Procedure


End SubModule
