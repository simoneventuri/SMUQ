SubModule(InputSection_Class) InputSection_Output_SubClass

  use Logger_Class          ,only:  Logger, LogLevel_HEAVYDEBUG
  use Utilities_Library     ,only:  GetOptArgValue

  implicit none

  integer                                                   ,parameter  ::  NItemMaxInLogs = 1000

  contains

! This procedures writes the Section object into a character variable.
Module Procedure WriteSectionToString

  use String_Library      ,only:  Convert_To_String, SetLength
  use Utilities_Library   ,only:  GetOptArgValue, IsIncluded, AddElementToArray, FirstSkippedItem, SelectedItem, SkippedItemsRange

  character(*)                                              ,parameter  ::  ProcName = "WriteSectionToString" ! Name of current procedure
  logical                                                   ,parameter  ::  DefaultAlign      = .True.
  logical                                                   ,parameter  ::  DefaultShowEmpty  = .True.
  integer                                                   ,parameter  ::  DefaultIndent     =  0
  integer                                                   ,parameter  ::  IndentStep        =  2

  logical                                                               ::  Align_, WriteMe
  integer                                                               ::  i, Indent_, NewIndent, Depth_
  integer                                                               ::  MaxLines_, MaxParam_
  integer                                                               ::  NameLength, ValueLength
  integer                                                               ::  NParam
  character(:)  ,allocatable                                            ::  Prefix, Line, Name, Value
  character(:)  ,allocatable                                            ::  SubLines(:)

  allocate( Character(0) :: Lines(0) )

  WriteMe   =   (.Not.This%Empty) .or. GetOptArgValue(DefaultShowEmpty,ShowEmpty)
  if ( present(Include) ) WriteMe = WriteMe .and. (       IsIncluded(This%Name,Include,CaseSensitive=.False.) )
  if ( present(Exclude) ) WriteMe = WriteMe .and. ( .Not. IsIncluded(This%Name,Exclude,CaseSensitive=.False.) )
  if ( .Not. WriteMe ) return

  Align_        =   GetOptArgValue( DefaultAlign  , Align  )
  Indent_       =   GetOptArgValue( DefaultIndent , Indent )

  NameLength    =   This%GetMaxLengthParameterName()
  ValueLength   =   This%GetMaxLengthParameterValue()
  Prefix        =   ""
  if ( Indent_ /= 0 ) Prefix = repeat(" ", Indent_)
  NewIndent     =   Indent_ + IndentStep

  Line          =   Prefix//"Start("//trim(This%Name)//")"
  call AddElementToArray( Line, Lines)

! For performance, first count the number of lines to be added, allocate the tmp string, and then add then.
! This is to avoid to allocate for each parameter
  NParam    =   0
  MaxParam_     =   GetOptArgValue( This%NParameters, MaxParam )
  do i = 1,This%NParameters
    if ( FirstSkippedItem(i,This%NParameters,MaxParam_) ) then
      NParam  =   NParam + 1
    else if ( SelectedItem(i,This%NParameters,MaxParam_) ) then
      NParam  =   NParam + 1
    end if
  end do
  allocate( Character(1000) :: SubLines(NParam) )
  NParam    =   0
  do i = 1,This%NParameters
    if ( FirstSkippedItem(i,This%NParameters,MaxParam_) ) then
      NParam      =   NParam + 1
      Line        =   Prefix // "  ... skipping parameter "//SkippedItemsRange(This%NParameters,MaxParam_ )//" ..."
    else if ( SelectedItem(i,This%NParameters,MaxParam_) ) then
      NParam      =   NParam + 1
      Name        =   trim(This%Parameters(i)%Name)
      Value       =   trim(This%Parameters(i)%Value)
      if ( Align_ ) Name = SetLength(Name,NameLength)
      Line        =   Prefix // "  " // Name // " = " // Value
    else
      cycle
    end if
    SubLines(NParam)    =   Line
  end do
  call AddElementToArray( SubLines, Lines)

!   call Logger%Write( "This%NParameters = ", This%NParameters )
!   do i = 1,This%NParameters
!     Name        =   trim(This%Parameters(i)%Name)
!     Value       =   trim(This%Parameters(i)%Value)
!     if ( Align_ ) Name = SetLength(Name,NameLength)
!     Line        =   Prefix // "  " // Name // " = " // Value
!     call AddElementToArray( Line, Lines)
!     call Logger%Write( "Line = ", Line )
!   end do


  do i = 1,This%NSections
    call This%Sections(i)%Write( SubLines, NewIndent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth, MaxLines, MaxParam )
    if ( size(SubLines) == 0 ) cycle
    call AddElementToArray( SubLines, Lines)
  end do

  Line          =   Prefix//"End("//trim(This%Name)//")"
  call AddElementToArray( Line, Lines)

End Procedure

! This procedures writes the Section object to the Logger.
Module Procedure WriteSectionToLogger
  use Utilities_Library   ,only:  GetOptArgValue
  character(:)  ,allocatable  ,dimension(:)                             ::  Lines
  integer                                                               ::  i
  integer                                                               ::  NMax, N
  call This%Write( Lines, Indent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth, MaxLines, MaxParam )
  NMax  =   GetOptArgValue(NItemMaxInLogs,MaxLines)
  N     =   size(Lines)
  do i = 1,size(Lines)
    if ( Logger%FirstSkippedLine(i,N,NMax) ) then
      call Logger%Write( "... skipping items "//Logger%SkippedLinesRange(N,NMax)//" ..." )
    else if ( Logger%SkippedLine(i,N,NMax) ) then
      cycle
    end if
    call Logger%Write( trim(Lines(i)), LogLevel=0 )
  end do
End Procedure

! This procedures writes the Section object to a file from a file unit.
Module Procedure WriteSectionToFileFromFileUnit
  character(:)  ,allocatable  ,dimension(:)                             ::  Lines
  integer                                                               ::  i
  call This%Write( Lines, Indent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth, MaxLines, MaxParam )
  do i = 1,size(Lines)
    write(FileUnit,"(g0)") trim( Lines(i) )
  end do
End Procedure

! This procedures writes the Section object to a file from a file name.
Module Procedure WriteSectionToFileFromFileName
  use Error_Class    ,only:  Error
  integer                                                               ::  FileUnit, ios, i
  character(:)  ,allocatable  ,dimension(:)                             ::  Lines
  character(:)  ,allocatable                                            ::  ProcPath, ErrMsg
  open( NewUnit=FileUnit, File=FileName, Form='FORMATTED', Status='UNKNOWN', Action="WRITE", iostat=ios )
  if ( ios /= 0 ) then
    ProcPath  =   Logger%GetPath()
    ErrMsg    =   "Error opening file to write Section object '"//This%Name//"'"
    call Error%Open( Unit=FileUnit, File=FileName, ProcName=ProcPath, Line=ErrMsg )
  end if
  call This%Write( FileUnit, Indent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth, MaxLines, MaxParam )
  close(FileUnit)
End Procedure

Module Procedure WriteSectionUDIO
  character(:)  ,allocatable  ,dimension(:)                             ::  Lines
  integer                                                               ::  i
  character(6)                                                          ::  Fmt
  call This%Write( Lines )
  do i = 1,size(Lines)
    Fmt  =  "(g0,/)"
    if ( i == size(Lines) ) Fmt = "(g0)"
    write(Unit,Fmt,iostat=iostat) trim( Lines(i) )
    if ( iostat /= 0  ) then
      iomsg   =   "Error writing Section object '"//This%Name//"'"
    end if
  end do
End Procedure

Module Procedure OutputSectionToFile

  use String_Library      ,only:  Convert_To_String

  character(*)                                              ,parameter  ::  ProcName = "OutputSectionToFile" ! Name of current procedure
  logical                                                   ,parameter  ::  Dbg=.False.
  integer                                                               ::  i
  integer                                                               ::  Indent
  integer                                                               ::  Indentation_Local
  integer                                                               ::  ParName_Length
  integer                                                               ::  ParValu_Length
  integer                                                               ::  ParPref_Length
  character(:)  ,allocatable                                            ::  Prefix
  character(:)  ,allocatable                                            ::  ParNum
  character(:)  ,allocatable                                            ::  Name
  character(:)  ,allocatable                                            ::  Value

  if (Dbg) call Logger%Entering( ProcName )
  if (Dbg) call Logger%Write( "This%Name = ", This%Name )
  if (Dbg) call Logger%Write( "This%Empty = ", This%Empty )

  if ( This%Empty ) then
!     write(Unit,"(2x'Empty section !')")
    if (Dbg) call Logger%Exiting()
    return
  end if

  Indentation_Local     =   0
  if ( Present(Indentation) ) Indentation_Local = Indentation
  Indent                =   Indentation_Local + 2
  Prefix                =   "(" // Convert_To_String(Indent) // "x,"
  ParPref_Length        =   len_trim(  "3x,'P" // Convert_To_String(This%NParameters ) // ":',3x,"  )
  ParName_Length        =   This%GetMaxLengthParameterName()
  ParValu_Length        =   This%GetMaxLengthParameterValue()

!   write(Unit,*)
  write(Unit,Prefix//"'Section: ',g0)") trim(This%Name)
!   write(Unit,Prefix//"'NParameters = ',g0)") This%NParameters
!   write(Unit,Prefix//"'NSections   = ',g0)") This%NSections
  do i = 1,This%NParameters
    ParNum      =   Convert_To_String( "2x,'P" // Convert_To_String(i,Pos='L') // ":',3x," , Len=ParPref_Length  )
    Name        =   Convert_To_String( This%Parameters(i)%Name,   Len=ParName_Length, Pos='L' )
    Value       =   Convert_To_String( This%Parameters(i)%Value, Len=ParValu_Length, Pos='L' )
!     write(Unit,Prefix//ParNum//"g0,3x,' = ',3x,g0)") Name, Value
    write(Unit,Prefix//"2X,g0,3x,' = ',3x,g0)") Name, trim(Value)
  end do

!   if (Dbg) call Logger%Write( "This%NSections       = ", This%NSections )
!   if (Dbg) call Logger%Write( "size(This%Sections)  = ", size(This%Sections) )
  do i = 1,This%NSections
  associate( Sec => This%Sections(i) )
!     if (Dbg) then
!       call Logger%Write( "i = ", i )
!       call Logger%Write( "  Sec%NParameters = ", Sec%NParameters )
!       call Logger%Write( "  Sec%NSections   = ", Sec%NSections )
!       call Logger%Write( "  allocated(Sec%Name) = ", allocated(Sec%Name) )
!       if ( allocated(Sec%Name) ) call Logger%Write( "Sec%Name = ", Sec%Name )
!     end if
    call Sec%Write( Unit, Indent )
  end associate
  end do

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure OutputSectionToString

  use String_Library      ,only:  Convert_To_String, Add_Line_To_String

  character(*)                                              ,parameter  ::  ProcName = "OutputSectionToString" ! Name of current procedure
  logical                                                   ,parameter  ::  Dbg=.False.
  integer                                                               ::  i
  integer                                                               ::  Indent
  integer                                                               ::  Indentation_Local
  integer                                                               ::  ParName_Length
  integer                                                               ::  ParValu_Length
  character(:)  ,allocatable                                            ::  Prefix
  character(:)  ,allocatable                                            ::  Name
  character(:)  ,allocatable                                            ::  Value
  character(:)  ,allocatable  ,dimension(:)                             ::  Text

  if (Dbg) call Logger%Entering( ProcName )
  if (Dbg) call Logger%Write( "This%Name = ", This%Name )
  if (Dbg) call Logger%Write( "This%Empty = ", This%Empty )

! ==============================================================================================================
!   DEALING WITH THE CASE OF AN EMPTY SECTION
! ==============================================================================================================
  if ( This%Empty ) then
    allocate( Character(0) :: Lines(0) )    ! Initializing to an empty string
    if (Dbg) call Logger%Exiting()
    return
  end if
! ==============================================================================================================


! ==============================================================================================================
!   SETTING THE INDENTATION LEVEL
! ==============================================================================================================
  Indentation_Local =     0
  if ( Present(Indentation) ) Indentation_Local = Indentation
  Indent            =     Indentation_Local + 2
  Prefix            =     repeat(" ", Indent)
! ==============================================================================================================

!   call Text%Add_Line( Prefix // "Section: " // trim(This%Name) )
  call Add_Line_To_String( Text, Prefix // "Section: " // trim(This%Name) )

! ==============================================================================================================
!   WRITING THE PARAMETERS CONTAINED IN CURRENT SECTION
! ==============================================================================================================
  ParName_Length    =     This%GetMaxLengthParameterName()
  ParValu_Length    =     This%GetMaxLengthParameterValue()
  do i = 1,This%NParameters
    Name            =     Convert_To_String( This%Parameters(i)%Name,   Len=ParName_Length, Pos='L' )
    Value           =     trim( Convert_To_String( This%Parameters(i)%Value, Len=ParValu_Length, Pos='L' ) )
!     call Text%Add_Line( Prefix // "  " // Name // "   =   " // Value )
    call Add_Line_To_String( Text, Prefix // "  " // Name // "   =   " // Value )
  end do
! ==============================================================================================================


! ==============================================================================================================
!   WRITING THE SUBSECTIONS CONTAINED IN CURRENT SECTION
! ==============================================================================================================
  do i = 1,This%NSections
  associate( Sec => This%Sections(i) )
    call Sec%Write( Lines, Indent )
!     call Text%Add_Line( Lines )
    call Add_Line_To_String( Text, Lines )
  end associate
  end do
! ==============================================================================================================

!   call move_alloc( Text%Lines, Lines )
  call move_alloc( Text, Lines )

  if (Dbg) call Logger%Exiting()

End Procedure














































! ! This procedures writes the Section object into a character variable.
! Module Procedure WriteSectionToString
!
!   use String_Library      ,only:  Convert_To_String, Add_Line_To_String, SetLength
!   use Utilities_Library   ,only:  GetOptArgValue, IsIncluded
!
!   character(*)                                              ,parameter  ::  ProcName = "WriteSectionToString" ! Name of current procedure
!   logical                                                   ,parameter  ::  DefaultAlign      = .True.
!   logical                                                   ,parameter  ::  DefaultShowEmpty  = .True.
!   integer                                                   ,parameter  ::  DefaultIndent     =  0
!   integer                                                   ,parameter  ::  IndentStep        =  2
!
!   logical                                                               ::  Align_, WriteMe
!   integer                                                               ::  i, Indent_, NewIndent, Depth_
!   integer                                                               ::  NameLength, ValueLength
!   character(:)  ,allocatable                                            ::  Prefix, Line, Name, Value
!   character(:)  ,allocatable  ,dimension(:)                             ::  SubLines
!
!   allocate( Character(0) :: Lines(0) )
!
!   WriteMe   =   (.Not.This%Empty) .or. GetOptArgValue(DefaultShowEmpty,ShowEmpty)
!   if ( present(Include) ) WriteMe = WriteMe .and. (       IsIncluded(This%Name,Include,CaseSensitive=.False.) )
!   if ( present(Exclude) ) WriteMe = WriteMe .and. ( .Not. IsIncluded(This%Name,Exclude,CaseSensitive=.False.) )
!   if ( .Not. WriteMe ) return
!
!   Align_        =   GetOptArgValue( DefaultAlign  , Align  )
!   Indent_       =   GetOptArgValue( DefaultIndent , Indent )
!   NameLength    =   This%GetMaxLengthParameterName()
!   ValueLength   =   This%GetMaxLengthParameterValue()
!   Prefix        =   ""
!   if ( Indent_ /= 0 ) Prefix = repeat(" ", Indent_)
!   NewIndent       =   Indent_ + IndentStep
!
!   Line          =   Prefix//"Start("//trim(This%Name)//")"
!   call Add_Line_To_String(Lines,Line)
!
!   do i = 1,This%NParameters
!     Name        =   trim(This%Parameters(i)%Name)
!     Value       =   trim(This%Parameters(i)%Value)
!     if ( Align_ ) Name = SetLength(Name,NameLength)
!     Line        =   Prefix // "  " // Name // " = " // Value
!     call Add_Line_To_String(Lines,Line)
!   end do
!
!   do i = 1,This%NSections
!     call This%Sections(i)%Write( SubLines, NewIndent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth )
!     if ( size(SubLines) > 0 ) call Add_Line_To_String(Lines,SubLines)
!   end do
!
!   Line          =   Prefix//"End("//trim(This%Name)//")"
!   call Add_Line_To_String(Lines,Line)
!
! End Procedure
!
! ! This procedures writes the Section object to the Logger.
! Module Procedure WriteSectionToLogger
!   character(:)  ,allocatable  ,dimension(:)                             ::  Lines
!   integer                                                               ::  i
!   call This%Write( Lines, Indent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth )
!   do i = 1,size(Lines)
!     call Logger%Write( trim(Lines(i)),LogLevel=0)
!   end do
! End Procedure
!
! ! This procedures writes the Section object to a file from a file unit.
! Module Procedure WriteSectionToFileFromFileUnit
!   character(:)  ,allocatable  ,dimension(:)                             ::  Lines
!   integer                                                               ::  i
!   call This%Write( Lines, Indent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth )
!   do i = 1,size(Lines)
!     write(FileUnit,"(g0)") trim( Lines(i) )
!   end do
! End Procedure
!
! ! This procedures writes the Section object to a file from a file name.
! Module Procedure WriteSectionToFileFromFileName
!   use Error_Class    ,only:  Error
!   integer                                                               ::  FileUnit, ios, i
!   character(:)  ,allocatable  ,dimension(:)                             ::  Lines
!   character(:)  ,allocatable                                            ::  ProcPath, ErrMsg
!   open( NewUnit=FileUnit, File=FileName, Form='FORMATTED', Status='UNKNOWN', Action="WRITE", iostat=ios )
!   if ( ios /= 0 ) then
!     ProcPath  =   Logger%GetPath()
!     ErrMsg    =   "Error opening file to write Section object '"//This%Name//"'"
!     call Error%Open( Unit=FileUnit, File=FileName, ProcName=ProcPath, Line=ErrMsg )
!   end if
!   call This%Write( FileUnit, Indent, Align, AlignAll, ShowEmpty, Include, Exclude, Depth )
!   close(FileUnit)
! End Procedure
!
! Module Procedure WriteSectionUDIO
!   character(:)  ,allocatable  ,dimension(:)                             ::  Lines
!   integer                                                               ::  i
!   character(6)                                                          ::  Fmt
!   call This%Write( Lines )
!   do i = 1,size(Lines)
!     Fmt  =  "(g0,/)"
!     if ( i == size(Lines) ) Fmt = "(g0)"
!     write(Unit,Fmt,iostat=iostat) trim( Lines(i) )
!     if ( iostat /= 0  ) then
!       iomsg   =   "Error writing Section object '"//This%Name//"'"
!     end if
!   end do
! End Procedure
!
!
! Module Procedure OutputSectionToFile
!
!   use String_Library      ,only:  Convert_To_String
!
!   character(*)                                              ,parameter  ::  ProcName = "OutputSectionToFile" ! Name of current procedure
!   logical                                                   ,parameter  ::  Dbg=.False.
!   integer                                                               ::  i
!   integer                                                               ::  Indent
!   integer                                                               ::  Indentation_Local
!   integer                                                               ::  ParName_Length
!   integer                                                               ::  ParValu_Length
!   integer                                                               ::  ParPref_Length
!   character(:)  ,allocatable                                            ::  Prefix
!   character(:)  ,allocatable                                            ::  ParNum
!   character(:)  ,allocatable                                            ::  Name
!   character(:)  ,allocatable                                            ::  Value
!
!   if (Dbg) call Logger%Entering( ProcName )
!   if (Dbg) call Logger%Write( "This%Name = ", This%Name )
!   if (Dbg) call Logger%Write( "This%Empty = ", This%Empty )
!
!   if ( This%Empty ) then
! !     write(Unit,"(2x'Empty section !')")
!     if (Dbg) call Logger%Exiting()
!     return
!   end if
!
!   Indentation_Local     =   0
!   if ( Present(Indentation) ) Indentation_Local = Indentation
!   Indent                =   Indentation_Local + 2
!   Prefix                =   "(" // Convert_To_String(Indent) // "x,"
!   ParPref_Length        =   len_trim(  "3x,'P" // Convert_To_String(This%NParameters ) // ":',3x,"  )
!   ParName_Length        =   This%GetMaxLengthParameterName()
!   ParValu_Length        =   This%GetMaxLengthParameterValue()
!
! !   write(Unit,*)
!   write(Unit,Prefix//"'Section: ',g0)") trim(This%Name)
! !   write(Unit,Prefix//"'NParameters = ',g0)") This%NParameters
! !   write(Unit,Prefix//"'NSections   = ',g0)") This%NSections
!   do i = 1,This%NParameters
!     ParNum      =   Convert_To_String( "2x,'P" // Convert_To_String(i,Pos='L') // ":',3x," , Len=ParPref_Length  )
!     Name        =   Convert_To_String( This%Parameters(i)%Name,   Len=ParName_Length, Pos='L' )
!     Value       =   Convert_To_String( This%Parameters(i)%Value, Len=ParValu_Length, Pos='L' )
! !     write(Unit,Prefix//ParNum//"g0,3x,' = ',3x,g0)") Name, Value
!     write(Unit,Prefix//"2X,g0,3x,' = ',3x,g0)") Name, trim(Value)
!   end do
!
! !   if (Dbg) call Logger%Write( "This%NSections       = ", This%NSections )
! !   if (Dbg) call Logger%Write( "size(This%Sections)  = ", size(This%Sections) )
!   do i = 1,This%NSections
!   associate( Sec => This%Sections(i) )
! !     if (Dbg) then
! !       call Logger%Write( "i = ", i )
! !       call Logger%Write( "  Sec%NParameters = ", Sec%NParameters )
! !       call Logger%Write( "  Sec%NSections   = ", Sec%NSections )
! !       call Logger%Write( "  allocated(Sec%Name) = ", allocated(Sec%Name) )
! !       if ( allocated(Sec%Name) ) call Logger%Write( "Sec%Name = ", Sec%Name )
! !     end if
!     call Sec%Write( Unit, Indent )
!   end associate
!   end do
!
!   if (Dbg) call Logger%Exiting()
!
! End Procedure
!
! Module Procedure OutputSectionToString
!
!   use String_Library      ,only:  Convert_To_String, Add_Line_To_String
!
!   character(*)                                              ,parameter  ::  ProcName = "OutputSectionToString" ! Name of current procedure
!   logical                                                   ,parameter  ::  Dbg=.False.
!   integer                                                               ::  i
!   integer                                                               ::  Indent
!   integer                                                               ::  Indentation_Local
!   integer                                                               ::  ParName_Length
!   integer                                                               ::  ParValu_Length
!   character(:)  ,allocatable                                            ::  Prefix
!   character(:)  ,allocatable                                            ::  Name
!   character(:)  ,allocatable                                            ::  Value
!   character(:)  ,allocatable  ,dimension(:)                             ::  Text
!
!   if (Dbg) call Logger%Entering( ProcName )
!   if (Dbg) call Logger%Write( "This%Name = ", This%Name )
!   if (Dbg) call Logger%Write( "This%Empty = ", This%Empty )
!
! ! ==============================================================================================================
! !   DEALING WITH THE CASE OF AN EMPTY SECTION
! ! ==============================================================================================================
!   if ( This%Empty ) then
!     allocate( Character(0) :: Lines(0) )    ! Initializing to an empty string
!     if (Dbg) call Logger%Exiting()
!     return
!   end if
! ! ==============================================================================================================
!
!
! ! ==============================================================================================================
! !   SETTING THE INDENTATION LEVEL
! ! ==============================================================================================================
!   Indentation_Local =     0
!   if ( Present(Indentation) ) Indentation_Local = Indentation
!   Indent            =     Indentation_Local + 2
!   Prefix            =     repeat(" ", Indent)
! ! ==============================================================================================================
!
! !   call Text%Add_Line( Prefix // "Section: " // trim(This%Name) )
!   call Add_Line_To_String( Text, Prefix // "Section: " // trim(This%Name) )
!
! ! ==============================================================================================================
! !   WRITING THE PARAMETERS CONTAINED IN CURRENT SECTION
! ! ==============================================================================================================
!   ParName_Length    =     This%GetMaxLengthParameterName()
!   ParValu_Length    =     This%GetMaxLengthParameterValue()
!   do i = 1,This%NParameters
!     Name            =     Convert_To_String( This%Parameters(i)%Name,   Len=ParName_Length, Pos='L' )
!     Value           =     trim( Convert_To_String( This%Parameters(i)%Value, Len=ParValu_Length, Pos='L' ) )
! !     call Text%Add_Line( Prefix // "  " // Name // "   =   " // Value )
!     call Add_Line_To_String( Text, Prefix // "  " // Name // "   =   " // Value )
!   end do
! ! ==============================================================================================================
!
!
! ! ==============================================================================================================
! !   WRITING THE SUBSECTIONS CONTAINED IN CURRENT SECTION
! ! ==============================================================================================================
!   do i = 1,This%NSections
!   associate( Sec => This%Sections(i) )
!     call Sec%Write( Lines, Indent )
! !     call Text%Add_Line( Lines )
!     call Add_Line_To_String( Text, Lines )
!   end associate
!   end do
! ! ==============================================================================================================
!
! !   call move_alloc( Text%Lines, Lines )
!   call move_alloc( Text, Lines )
!
!   if (Dbg) call Logger%Exiting()
!
! End Procedure


End SubModule