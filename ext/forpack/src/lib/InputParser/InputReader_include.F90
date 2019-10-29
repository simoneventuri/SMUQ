
! Include file needed for _ASSIGN_ALLOCATABLE_CHARACTER_ in ReadInputFromText
# include "forpack-include.inc"

Module Procedure ReadInputFromFile

  character(*)                                              ,parameter  ::  ProcName = "ReadInputFromFile"      ! Name of current procedure
  logical                                                               ::  Debug
  integer ,pointer                                                      ::  LogLevelPtr
  logical                                                               ::  FileExist                           ! File existence indicator
  type(String_Type)         ,allocatable                                ::  Strings(:)                          ! Arrays of String objects

  Debug   =   .False.
  nullify(LogLevelPtr)
  if ( present(LogLevel) ) then
    LogLevelPtr => LogLevel
    call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)
    Debug   =   Logger%On()
  end if

! ==============================================================================================================
!    OPENING INPUT FILE
! ==============================================================================================================
! ! @TODO: Check everything went ok when opening the file: use the file utilities
! ==============================================================================================================
  if (Debug) call Logger%Write( "Opening input file" )
  if (Debug) call Logger%Write( "-> FileName = ", FileName )
  Inquire( File=FileName, Exist=FileExist )
  if ( present(Found) ) Found = FileExist
  if ( .Not. FileExist ) then
    if ( .Not. present(Found) ) then
      call Error_FileNotFound( FileName )
    else
      if ( present(LogLevel) ) call Logger%Exiting()
      nullify(LogLevelPtr)
      return
    end if
  end if
  This%FileName         =       FileName
  if (Debug) call Logger%Write( "-> Opening file" )
  open( NewUnit=This%Unit, File=This%FileName, Form='FORMATTED', Status='OLD', Action="READ" )
! ==============================================================================================================


! ==============================================================================================================
!    LOADING FILE INTO VECTOR OF STRING OBJECTS
! ==============================================================================================================
  if (Debug) call Logger%Write( "Loading file into vector of string objects" )
  if (Debug) call Logger%Write( "-> Calling LoadInputFileInString" )
  call LoadInputFileInString( This, Strings, LogLevel=LogLevelPtr )
  if (Debug) call Logger%Write( "-> Number of lines found: size(Strings) = ", size(Strings) )
! ==============================================================================================================


! ==============================================================================================================
!    PROCESSING VECTOR OF STRING OBJECTS
! ==============================================================================================================
  if (Debug) call Logger%Write( "Processing vector of string objects" )
  if (Debug) call Logger%Write( "-> Calling ProcessStrings" )
  call ProcessStrings( This, Strings  , &
        SectionName   =   SectionName , &
        AllowMacro    =   AllowMacro  , &
        AllowEnvVar   =   AllowEnvVar , &
        LogLevel      =   LogLevelPtr   )
  if (Debug) call Logger%Write( "-> Done processing Strings" )
! ==============================================================================================================

  if ( present(LogLevel) ) call Logger%Exiting()
  nullify(LogLevelPtr)

End Procedure


Module Procedure ReadInputFromCharacters

  character(*)                                              ,parameter  ::  ProcName = "ReadInputFromCharacters"      ! Name of current procedure
  logical                                                               ::  Debug
  integer ,pointer                                                      ::  LogLevelPtr
  type(String_Type)         ,allocatable                                ::  Strings(:)                          ! Arrays of String objects

  Debug   =   .False.
  nullify(LogLevelPtr)
  if ( present(LogLevel) ) then
    LogLevelPtr => LogLevel
    call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)
    Debug   =   Logger%On()
  end if

  This%FileName   =       ''
  This%Unit       =       0

! ==============================================================================================================
!    LOADING THE VECTOR OF CHARACTERS INTO A VECTOR OF STRING OBJECTS
! ==============================================================================================================
  if (Debug) call Logger%Write( "Loading the vector of characters into a vector of string objects" )
  if (Debug) call Logger%Write( "-> Calling LoadCharactersInStrings" )
  call LoadCharactersInStrings( Charac, Strings, LogLevel=LogLevelPtr )
  if (Debug) call Logger%Write( "-> Number of lines found: size(Strings) = ", size(Strings) )
! ==============================================================================================================


! ==============================================================================================================
!    PROCESSING THE VECTOR OF STRING OBJECTS
! ==============================================================================================================
  if (Debug) call Logger%Write( "Processing the vector of string objects" )
  if (Debug) call Logger%Write( "-> Calling ProcessStrings" )
  call ProcessStrings( This, Strings  , &
        SectionName   =   SectionName , &
        AllowMacro    =   AllowMacro  , &
        AllowEnvVar   =   AllowEnvVar , &
        LogLevel      =   LogLevelPtr   )

  if (Debug) call Logger%Write( "-> Done processing Strings" )
! ==============================================================================================================

  if ( present(LogLevel) ) call Logger%Exiting()
  nullify(LogLevelPtr)

End Procedure

Module Procedure ReadInputFromText
  character(*)                                              ,parameter  ::  ProcName = "ReadInputFromText"
  character(:)  ,dimension(:)   ,allocatable                            ::  Charac
  call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG )
# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
    _ASSIGN_ALLOCATABLE_CHARACTER_(Charac,Text%GetLines())
# else
    Charac  =   Text%GetLines()
# endif
  call This%Read( Charac                , &
          SectionName   =   SectionName , &
          AllowMacro    =   AllowMacro  , &
          AllowEnvVar   =   AllowEnvVar , &
          LogLevel      =   LogLevel      )
  call Logger%Exiting()
End Procedure

! Module Procedure OutputInputReader
!   integer       ,parameter                                              ::  Indentation = 2
!   if ( len(This%FileName) /= 0 ) write(Unit,"(2x'This%FileName = ',g0)") This%FileName
!   write(Unit,"(2x'This%Name         = ',    g0)") This%GetName()
!   write(Unit,"(2x'This%NSections    = ',    g0)") This%NSections
!   write(Unit,"(2x'This%NParameters  = ',    g0)") This%NParameters
! !   if ( This%Empty ) then
! ! !     write(Unit,"(2x'Empty section !')")
! !   else
!     call This%InputSection_Type%Write( Unit, Indentation=Indentation )
! !   end if
! End Procedure




! Module Procedure WriteInputReaderToUnit
! !   call This%InputSection_Type%Write( Unit, Indentation=Indentation )
! End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
!                                       PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

! This procedures convert the data from a file into a vector of character strings, each element corresponding
! to a given line in the file.
! Note that one could simply do an infinite loop, add each line as they come, and exit the loop when the end
! of file is encountered. However, this would required to re-allocate the output vector of string "Lines" each
! time a new line is added. It will thus be very inefficient.
! An infinit loop is done until the end-of-file is reached.
! Each line is stored in a very long character string variable 'Long_String'.
! This character variable is the processed as followed:
!  * All character at the left of the comment character '#' are removed,
!  * If the continuation character '&' is found, then the next lines are added to the current one,
!  * All empty lines are ignored (lines with no characters or only blanks)
! Then, the resulting line is stored in a vector of 'Line_Type' object.
! Finaly, the input file is closed since all its useful information has been extracted.
Subroutine LoadInputFileInString( This, Lines, LogLevel )

  use, intrinsic :: iso_fortran_env     ,only:  IOStat_End

  type(InputReader_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument
  type(String_Type)     ,allocatable    ,dimension(:)   ,intent(out)    ::  Lines                           !< Vector of String object corresponding to lines stored in a file
  integer                             ,target ,optional ,intent(in)     ::  LogLevel                        !< Indicator of the log level

  character(*)                                              ,parameter  ::  ProcName = "LoadInputFileInString"
  logical                                                               ::  Debug
  integer ,pointer                                                      ::  LogLevelPtr
  integer                                                               ::  NLines                          ! Number of lines to be added: This will be the dimension of the output variable "Lines"
  integer                                                               ::  iLine
  integer                                                               ::  ios
  type(String_Type)                                                     ::  Line
  character(*)                                              ,parameter  ::  Continuation_Character  =       "&"
  character(*)                                              ,parameter  ::  Comment_Character       =       "#"

  Debug   =   .False.
  nullify(LogLevelPtr)
  if ( present(LogLevel) ) then
    LogLevelPtr => LogLevel
    call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)
    Debug   =   Logger%On()
  end if

! ==============================================================================================================
!   INITIALIZING THE STRING OBJECT
! ==============================================================================================================
  if (Debug) call Logger%Write( "Calling Line%Initialize" )
  call Line%Initialize( i_Trim=.True., i_AdjustL=.True., i_Compact=.True. )
! ==============================================================================================================


! ==============================================================================================================
!   GETTING THE TOTAL NUMBER OF LINES TO BE CONSIDERED
! ==============================================================================================================
! This section computes the total number of lines to be extracted from the file.
! ==============================================================================================================
  if (Debug) call Logger%Write( "Getting the total number of lines to be considered" )
  rewind(This%Unit)                                                                                             ! Rewinding the file
  NLines        =       0                                                                                       ! Initializing the number of lines to zero (Required)
!   Fi  =   GetFormat(NL)
  do                                                                                                            ! Loop on all the lines in the input file
    call Line%Read_From_File( This%Unit,                            &                                           ! Reading a line from the file and storing it in the String object
            ios                     =     ios,                      &
            Continuation_Character  =     Continuation_Character,   &
            Comment_Character       =     Comment_Character         )
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
!     call Line%Remove_At_Left_Of( Comment_Character )                                                            ! Removing all characters at the left of the comment character
    if ( Line%Is_Empty() ) cycle                                                                                ! If the line corresponds to an empty characters string, then going to the next line (Note that this instruction should be called after continuation lines have been treated)
    NLines      =       NLines + 1                                                                              ! Incrementing the number of lines

    if (Debug) then
      if ( NLines <= 100 ) call Logger%Write( "-> i = ", NLines, " : ", Line%GetValue(), F2="i6" )
      if ( NLines == 101 ) call Logger%Write( "-> i = ... skipping lines ..." )
!       if ( Logger%FirstSkippedLine(i,XXX,10) ) then
!         call Logger%Write( "-> i =  "//Logger%SkippedLinesRange(XXX,10)//" ... skipping lines ..." )
!       else if ( Logger%SelectedLine(i,XXX,10) ) then
!         call Logger%Write( "-> i = ", NLines, " : ", Line%GetValue(), F2="i6" )
!       end if
    end if
  end do                                                                                                        ! End loop on the lines in the input file
  if (Debug) call Logger%Write( "Number of lines to be converted: NLines = ", NLines )
  allocate( Lines(NLines) )                                                                                     ! Allocating the zoutput vector of lines to the number of lines
! ==============================================================================================================


! ==============================================================================================================
!   READING A SINGLE LINE FROM THE FILE, CONVERTING IT INTO AN OBJECT AND REMOVING COMMENTS
! ==============================================================================================================
  iLine         =       0                                                                                       ! Initializing the line index to zero (Required)
  rewind(This%Unit)                                                                                             ! Rewinding the file
  do                                                                                                            ! Loop on all the lines in the input file
!     call Line%Read_From_File( This%Unit, ios=ios, Continuation_Character=Continuation_Character )               ! Reading a line from the file and storing it in the String object
    call Line%Read_From_File( This%Unit,                                              &                         ! Reading a line from the file and storing it in the String object
                              ios                     =     ios,                      &
                              Continuation_Character  =     Continuation_Character,   &
                              Comment_Character       =     Comment_Character         )
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
!     call Line%Remove_At_Left_Of( Comment_Character )                                                            ! Removing all characters at the left of the comment character
    if ( Line%Is_Empty() ) cycle                                                                                ! If the line corresponds to an empty characters string, then going to the next line (Note that this instruction should be called after continuation lines have been treated)
    iLine               =       iLine + 1                                                                       ! Incrementing the line index
    Lines(iLine)        =       Line
  end do                                                                                                        ! End loop on the lines in the input file
! ==============================================================================================================

  close(This%Unit)                                                                                              ! Closing the file
  if (Debug) call Logger%Write( "size(Lines) = ", size(Lines) )

  if ( present(LogLevel) ) call Logger%Exiting()
  nullify(LogLevelPtr)

End Subroutine

! @TODO: Remove comment characters from the string when loading a inputReader object from a string
Subroutine LoadCharactersInStrings( Charac, Strings, LogLevel )

  use String_Library                    ,only:  GetFormat

  character(*)  ,dimension(:)                           ,intent(in)     ::  Charac                          !<
  type(String_Type)     ,allocatable    ,dimension(:)   ,intent(out)    ::  Strings                           !< Vector of String object corresponding to lines stored in a file
  integer                                     ,optional ,intent(in)     ::  LogLevel                        !< Indicator of the log level

  character(*)                                              ,parameter  ::  ProcName = "LoadCharactersInStrings"
  logical                                                               ::  Debug
  integer                                                               ::  i, NLines                          ! Number of lines to be added: This will be the dimension of the output variable "Lines"
  character(:)  ,allocatable                                            ::  Fi
  type(String_Type)                                                     ::  String

  Debug   =   .False.
  if ( present(LogLevel) ) then
    call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)
    Debug   =   Logger%On()
  end if


  if (Debug) call Logger%Write( "Initializing the string object" )
  if (Debug) call Logger%Write( "-> Calling String%Initialize" )
  call String%Initialize( i_Trim=.True., i_AdjustL=.True., i_Compact=.True. )


  if (Debug) call Logger%Write( "Getting the total number of lines to be considered" )
  NLines    =       size(Charac)
  if (Debug) call Logger%Write( "-> Number of lines: NLines = ", NLines )

  if (Debug) call Logger%Write( "Converting Characters to Strings" )
  Fi    =   GetFormat(NLines)
  allocate( Strings(NLines) )
  do i = 1,NLines
    call String%Set_Value( Charac(i) )
    Strings(i)    =   String
    if (Debug) call Logger%Write( "-> i = ", i, "Strings(i)%GetValue = ", Strings(i)%GetValue(), Fi=Fi )
  end do

  if ( present(LogLevel) ) call Logger%Exiting()

End Subroutine



Subroutine FindInputSection( Lines, SectionName, LogLevel )

  use String_Library      ,only:  UpperCase, GetSubString, GetSubStringIndexes

  type(String_Type)     ,dimension(:)                   ,intent(in)     ::  Lines
  character(:)  ,allocatable                            ,intent(out)    ::  SectionName
  integer                                     ,optional ,intent(in)     ::  LogLevel                        !< Indicator of the log level

  character(*)                                              ,parameter  ::  ProcName = "FindInputSection"
  logical                                                               ::  Debug
  character(*)                                              ,parameter  ::  Key_Start_Initial = "START("
  character(*)                                              ,parameter  ::  Key_Start_Final   = ")     "
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  SectionNameUC
  character(:)  ,allocatable                                            ::  Key_Ini
  character(:)  ,allocatable                                            ::  Key_Fin
  character(:)  ,allocatable  ,dimension(:)                             ::  InBetween
  integer       ,dimension(2)                                           ::  Indexes
!   type(String_Type)                                                     ::  Line

  Debug   =   .False.
  if ( present(LogLevel) ) then
    call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)
    Debug   =   Logger%On()
  end if

  SectionName  =       ""

  if ( size(Lines) == 0 ) then
    if (Debug) call Logger%Write( "No line in the vector of string: size(Lines) = ", size(Lines) )
    if ( present(LogLevel) ) call Logger%Exiting()
    return
  end if

  Key_Ini       =       UpperCase(  Key_Start_Initial )
  Key_Fin       =       UpperCase(  Key_Start_Final   )
  allocate( InBetween, source = [Key_Ini,Key_Fin] )

!   call Line%Initialize( i_UpperCase=.True. )


  do i = 1,size(Lines)
!     call Line%Set_Value( Lines(i) )
!     SectionName        =       Line%GetSubString( Key_Ini, Key_Fin )
!     if (SectionName /= "" ) exit
    SectionNameUC   =   UpperCase(Lines(i)%Value)
    SectionName    =   GetSubString( SectionNameUC, InBetween=InBetween )
    if (SectionName == "" ) cycle
    Indexes         =   GetSubStringIndexes( SectionNameUC, InBetween=InBetween )
    SectionName    =   Lines(i)%Value(Indexes(1):Indexes(2))
    exit
  end do
  if (Debug) call Logger%Write( "SectionName = ", SectionName )

  if ( present(LogLevel) ) call Logger%Exiting()

End Subroutine


Subroutine ProcessStrings( This, Strings, SectionName, AllowMacro, AllowEnvVar, LogLevel )

  use InputSection_Class    ,only:  InputSection_Type, Construct_Sections
  use InputParameter_Class  ,only:  InputParameter_Type
  use Utilities_Library     ,only:  GetOptArgValue

  type(InputReader_Type)                                ,intent(inout)  ::  This                                !< InputReader object
  type(String_Type)     ,allocatable                    ,intent(inout)  ::  Strings(:)                          !< Vector of String object corresponding to lines stored in a file
  character(*)                                ,optional ,intent(in)     ::  SectionName                         !< Name of the main section to be read
  logical                                     ,optional ,intent(in)     ::  AllowMacro                          !< Indicator whether macro substitution is allowed. Default: True
  logical                                     ,optional ,intent(in)     ::  AllowEnvVar                         !< Indicator whether environment variable substitution is allowed. Default: True
  integer                             ,target ,optional ,intent(in)     ::  LogLevel                            !< Indicator of the log level

  character(*)                                              ,parameter  ::  ProcName = "ProcessStrings"         ! Name of current procedure
  logical                                                               ::  Debug
  integer ,pointer                                                      ::  LogLevelPtr
  integer                                                               ::  i                                   ! Index of lines
  character(:)  ,allocatable                                            ::  SectionName_                        ! Name of the main section of the input file
  type(InputSection_Type)   ,allocatable                                ::  Sections(:)                         ! Array of Section objects
  type(InputParameter_Type) ,allocatable                                ::  Parameters(:)                       ! Arrays of Parameter objects

  Debug   =   .False.
  nullify(LogLevelPtr)
  if ( present(LogLevel) ) then
    LogLevelPtr => LogLevel
    call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)
    Debug   =   Logger%On()
  end if

! ==============================================================================================================
!    SETTING MAIN SECTION TO BE READ
! ==============================================================================================================
! This section sets the name of the section to be read. If the optional input argument is absent, then the
! first section found in the file is considered.
! ==============================================================================================================
  if (Debug) call Logger%Write( "Setting name of main section to be read" )
  if ( present(SectionName) ) then
    if (Debug) call Logger%Write( "-> Using input section name" )
    SectionName_    =   SectionName
  else
    if (Debug) call Logger%Write( "-> Using first section name found in file (FindInputSection)" )
    call FindInputSection( Strings, SectionName_, LogLevel=LogLevelPtr )
  end if
  call This%SetName( SectionName_ )
  if (Debug) call Logger%Write( "-> This%Name = ", This%GetName() )
! ==============================================================================================================

! ==============================================================================================================
!   EXTRACTING LINES ASSOCIATED TO CURRENT SECTION
! ==============================================================================================================
  if (Debug) call Logger%Write( "Extracting lines associated to current section" )
  if (Debug) call Logger%Write( "-> Calling ExtractLinesFromSection" )
  call ExtractLinesFromSection( This, Strings, LogLevel=LogLevelPtr )
  if (Debug) call Logger%Write( "-> Number of extracted lines: size(Strings) = ", size(Strings) )
! ==============================================================================================================

! ==============================================================================================================
!   CONSTRUCTING RECURSIVE STRUCTURE OF SECTIONS
! ==============================================================================================================
  if (Debug) call Logger%Write( "Constructing recursive structure of sections" )
  if (Debug) call Logger%Write( "-> Calling Construct_Sections" )
  call Construct_Sections( Strings, Sections, Parameters, FileName=This%FileName, Debug=Debug )
  if (Debug) then
    call Logger%Write( "-> Number of sections:    size(Sections)    = ", size(Sections) )
    call Logger%Write( "-> Number of parameters:  size(Parameters)  = ", size(Parameters) )
    do i = 1,size(Sections)
      call Logger%Write( "i = ", i )
      call Logger%Write( "Sections(i)%Name        = ", Sections(i)%GetName() )
      call Logger%Write( "Sections(i)%NSections   = ", Sections(i)%NSections )
      call Logger%Write( "Sections(i)%NParameters = ", Sections(i)%NParameters )
      call Logger%Write( "allocated(Sections(i)%Parameters) = ", allocated(Sections(i)%Parameters) )
      call Logger%Write( "RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(Sections(i)%Sections) = ", RECURSIVE_ALLOCATABLE_DERIVEDTYPE_ALLOCATED(Sections(i)%Sections) )
    end do
  end if
  if (Debug) call Logger%Write( "-> Calling This%Initialize" )
  call This%Initialize( This%GetName(), Sections, Parameters, Debug=Debug )
! ==============================================================================================================


  if ( GetOptArgValue(DefaultAllowMacro,AllowMacro) ) then
    if (Debug) call Logger%Write( "Calling This%ProcessMacroInSections" )
    call This%ProcessMacroInSections( Debug=Debug )
  end if

  if ( GetOptArgValue(DefaultAllowEnvVar,AllowEnvVar) ) then
    if (Debug) call Logger%Write( "Calling This%ProcessEnvironmentVariables" )
    call This%ProcessEnvironmentVariables( Debug=Debug )
  end if

  if ( allocated(Strings) )     deallocate( Strings )
  if ( allocated(Sections) )    deallocate( Sections )
  if ( allocated(Parameters) )  deallocate( Parameters )

  if (Debug) then
    call Logger%Write( "This%Name        = ", This%GetName() )
    call Logger%Write( "This%NSections   = ", This%NSections )
    call Logger%Write( "This%NParameters = ", This%NParameters )
    do i = 1,This%NParameters
      call Logger%Write( "   Name = '" // This%Parameters(i)%Name // "' Value = '" // This%Parameters(i)%Value // "'" )
    end do
  end if

  if ( present(LogLevel) ) call Logger%Exiting()
  nullify(LogLevelPtr)

End Subroutine


! This section extracts the lines associated to the considered section.
! It extract the lines in-between the starting and ending keywords
Subroutine ExtractLinesFromSection( This, Lines, LogLevel )

  use String_Library      ,only:  GetFormat
  use String_Library      ,only:  UpperCase

  type(InputReader_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument
  type(String_Type)     ,dimension(:)   ,allocatable    ,intent(inout)  ::  Lines
  integer                                     ,optional ,intent(in)     ::  LogLevel                        !< Indicator of the log level

  character(*)                                              ,parameter  ::  ProcName = "ExtractLinesFromSection"
  logical                                                               ::  Debug
  integer                                                               ::  i, Start_Index, End_Index
  character(:)  ,allocatable                                            ::  Fi
  type(String_Type)                                                     ::  Line
  type(String_Type)     ,allocatable    ,dimension(:)                   ::  List_Lines

  Debug   =   .False.
  if ( present(LogLevel) ) then
    call Logger%Entering( ProcName, LogLevel=LogLevel, MsgLogLevel=LogLevel_DEBUG)
    Debug   =   Logger%On()
  end if

  if (Debug) call Logger%Write( "Treating case when input vector is either unallocated or empty" )
  if ( .Not. allocated(Lines) ) then
    if (Debug) call Logger%Write( "-> Unallocated input vector of string" )
    if (Debug) call Logger%Exiting()
    return
  end if
  if ( size(Lines) == 0 ) then
    if (Debug) call Logger%Write( "-> No line in vector of string: size(Lines) = ", size(Lines) )
    if (Debug) call Logger%Exiting()
    return
  end if


  if (Debug) call Logger%Write( "Getting line index where section starts/ends" )
  call Line%Initialize( i_UpperCase=.True. )
  Start_Index   =       0
  End_Index     =       0
  do i = 1,size(Lines)
!     if (Debug) call Logger%Write( "-> i = ", i, LogLevel=LogLevel_HEAVYDEBUG )
    call Line%Set_Value( Lines(i) )
!     if (Debug) call Logger%Write( "  -> Line%GetValue() =", Line%GetValue(), LogLevel=LogLevel_HEAVYDEBUG )
!     if (Debug) call Logger%Write( "  -> Line%Is_Equal( 'START(' // UpperCase(This%GetName()) // ')' ) =", Line%Is_Equal( 'START(' // UpperCase(This%GetName()) // ')' ), LogLevel=LogLevel_HEAVYDEBUG )
!     if (Debug) call Logger%Write( "  -> Line%Is_Equal( 'END('   // UpperCase(This%GetName()) // ')' ) =", Line%Is_Equal( 'END('   // UpperCase(This%GetName()) // ')' ), LogLevel=LogLevel_HEAVYDEBUG )
    if ( Line%Is_Equal( "START(" // UpperCase(This%GetName()) // ")" ) ) Start_Index = i + 1
    if ( Line%Is_Equal( "END("   // UpperCase(This%GetName()) // ")" ) ) End_Index   = i - 1
  end do
  if ( Start_Index == End_Index+1 ) then ! Case of an empty section in whcih the 'Start(...)' and 'End(...)' lines are one after the other
    if (Debug) call Logger%Write( "-> Empty section" )
    allocate( List_Lines(0) )
    call move_alloc( List_Lines, Lines )
    if ( present(LogLevel) ) call Logger%Exiting()
    return
  end if
  if (Debug) call Logger%Write( "-> Start_Index = ", Start_Index )
  if (Debug) call Logger%Write( "-> End_Index   = ", End_Index   )
! ==============================================================================================================


! ==============================================================================================================
!   TREATING THE CASE WHEN THE SECTION HAS NOT BEEN FOUND OR IS NOT WELL DEFINED
! ==============================================================================================================
  if (( Start_Index == 0 ) .and. ( End_Index == 0 )) then
    if (Debug) call Logger%Write( "Section not found => Exiting" )
    if ( present(LogLevel) ) call Logger%Exiting()
    return
  end if
  if ( Start_Index > End_Index )  call Error_SectionendBeforeStart( This, Lines, Start_Index, End_Index )
  if ( Start_Index == 0 )         call Error_SectionWithoutStart( This, Lines, Start_Index, End_Index )
  if ( End_Index == 0 )           call Error_SectionWithoutEnd(  This, Lines, Start_Index, End_Index )
! ==============================================================================================================

! ==============================================================================================================
!   SETTING THE OUTPUT VARIABLE
! ==============================================================================================================
  if (Debug) call Logger%Write( "Setting lines of currernt section" )
  allocate( List_Lines, source = Lines(Start_Index:End_Index) )
  call move_alloc( List_Lines, Lines )
  if (Debug) then
    Fi  =   GetFormat(size(Lines))
    call Logger%Write( "-> Number of lines: size(Lines) = ", size(Lines) )
    do i = 1,size(Lines)
      if ( Logger%FirstSkippedLine(i,size(Lines),50) ) then
        call Logger%Write( "-> i =  "//Logger%SkippedLinesRange(size(Lines),50)//" ... skipping lines ..." )
      else if ( Logger%SelectedLine(i,size(Lines),50) ) then
        call Logger%Write( "-> i = ", i, "Line = ", Lines(i)%GetValue(), Fi=Fi )
      end if
    end do
  end if
! ==============================================================================================================

  if ( present(LogLevel) ) call Logger%Exiting()

End Subroutine


Subroutine Error_FileNotFound( FileName )
  use Error_Class    ,only:  Error
  character(*)                                          ,intent(in)     ::  FileName
  character(*)                                              ,parameter  ::  ProcName = "Error_FileNotFound"
  character(:)  ,allocatable                                            ::  ProcPath
  logical                                                               ::  FileExist                      ! File existence indicator
  ProcPath      =       Logger%GetPath(ProcName)
  Inquire( File=FileName, Exist=FileExist )                                                                    ! Checking if the file exists
  call Error%Set_Title( "Error while opening a file" )
  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  FileName:          " // FileName )
!   call Error%Add_Line( "  FileExist:        " // FileName ) ! @TODO
  call Error%Add_Line( "  Procedure path:    " // ProcPath )
  call Error%Raise( Unit=Logger%GetUnit() )
End Subroutine


Subroutine Error_SectionWithoutStart( Input, Lines, Start_Index, End_Index )
  use Error_Class         ,only:  Error
  use String_Library      ,only:  Convert_To_String
  type(InputReader_Type)                                ,intent(in)     ::  Input
  type(String_Type)     ,dimension(:)                   ,intent(in)     ::  Lines
  integer                                               ,intent(in)     ::  Start_Index
  integer                                               ,intent(in)     ::  End_Index
  character(*)                                              ,parameter  ::  ProcName = "Error_SectionWithoutStart"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable                                            ::  FileName
  integer                                                               ::  i
  ProcPath      =   Logger%GetPath(ProcName)
  FileName      =   Input%FileName
  if ( len_trim(FileName) == 0 ) FileName = "<Input object not created from input file>"
  call Error%Set_Title( "Input file error" )
  call Error%Add_Line( "The input file is not well formatted since the main section has no stating line." )
  call Error%Add_Line( "" )
  call Error%Add_Line( "Solution" )
  call Error%Add_Line( "-------" )
  call Error%Add_Line( "Add a starting line for the main section at the top of the input file." )
  call Error%Add_Line( "This starting line will be:  'Start("//Input%GetName()//")'" )
  call Error%Add_Line( "" )
  call Error%Add_Line( "Details" )
  call Error%Add_Line( "-------" )
  call Error%Add_Line( "  Name of input file:           " // FileName )
  call Error%Add_Line( "  Name of the section:          " // Input%GetName() )
  call Error%Add_Line( "  Section start line index:     " // Convert_To_String(Start_Index) )
  call Error%Add_Line( "  Section end line index:       " // Convert_To_String(End_Index) )
  call Error%Add_Line( "  Procedure path:               " // ProcPath )
  call Error%Add_Line( "  Content of the input file:" )
  do i = 1,size(Lines)
    call Error%Add_Line( "    " // Lines(i)%GetValue() )
  end do
  call Error%Raise( Unit=Logger%GetUnit() )
End Subroutine

Subroutine Error_SectionWithoutEnd( Input, Lines, Start_Index, End_Index )
  use Error_Class         ,only:  Error
  use String_Library      ,only:  Convert_To_String
  type(InputReader_Type)                                ,intent(in)     ::  Input
  type(String_Type)     ,dimension(:)                   ,intent(in)     ::  Lines
  integer                                               ,intent(in)     ::  Start_Index
  integer                                               ,intent(in)     ::  End_Index
  character(*)                                              ,parameter  ::  ProcName = "Error_SectionWithoutEnd"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable                                            ::  FileName
  integer                                                               ::  i
  ProcPath      =   Logger%GetPath(ProcName)
  FileName      =   Input%FileName
  if ( len_trim(FileName) == 0 ) FileName = "<Input object not created from input file>"
  call Error%Set_Title( "Input file error" )
  call Error%Add_Line( "The input file is not well formatted since the main section has no ending line." )
  call Error%Add_Line( "" )
  call Error%Add_Line( "Solution" )
  call Error%Add_Line( "-------" )
  call Error%Add_Line( "Add an ending line for the main section at the bottom of the input file." )
  call Error%Add_Line( "This ending line will be:  'End("//Input%GetName()//")'" )
  call Error%Add_Line( "" )
  call Error%Add_Line( "Details" )
  call Error%Add_Line( "-------" )
  call Error%Add_Line( "  Name of input file:           " // FileName )
  call Error%Add_Line( "  Name of the section:          " // Input%GetName() )
  call Error%Add_Line( "  Section start line index:     " // Convert_To_String(Start_Index) )
  call Error%Add_Line( "  Section end line index:       " // Convert_To_String(End_Index) )
  call Error%Add_Line( "  Procedure path:               " // ProcPath )
  call Error%Add_Line( "  Content of the input file:" )
  do i = 1,size(Lines)
    call Error%Add_Line( "    " // Lines(i)%GetValue() )
  end do
  call Error%Raise( Unit=Logger%GetUnit() )
End Subroutine


Subroutine Error_SectionendBeforeStart( Input, Lines, Start_Index, End_Index )
  use Error_Class         ,only:  Error
  use String_Library      ,only:  Convert_To_String
  type(InputReader_Type)                                ,intent(in)     ::  Input
  type(String_Type)     ,dimension(:)                   ,intent(in)     ::  Lines
  integer                                               ,intent(in)     ::  Start_Index
  integer                                               ,intent(in)     ::  End_Index
  character(*)                                              ,parameter  ::  ProcName = "Error_SectionendBeforeStart"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable                                            ::  FileName
  integer                                                               ::  i
  ProcPath      =   Logger%GetPath(ProcName)
  FileName      =   Input%FileName
  if ( len_trim(FileName) == 0 ) FileName = "<Input object not created from input file>"
  call Error%Set_Title( "Input file error" )
  call Error%Add_Line( "The input file is not well formatted since the ending line is found before the starting line for the main section." )
  call Error%Add_Line( "" )
  call Error%Add_Line( "Solution" )
  call Error%Add_Line( "-------" )
  call Error%Add_Line( "Invert the starting line and ending line for the main section at the bottom of the input file." )
  call Error%Add_Line( "The input file should" )
  call Error%Add_Line( " - start with the line: 'Start("//Input%GetName()//")'" )
  call Error%Add_Line( " - end with the line:   'End("//Input%GetName()//")'" )
  call Error%Add_Line( "" )
  call Error%Add_Line( "Details" )
  call Error%Add_Line( "-------" )
  call Error%Add_Line( "  Name of input file:           " // FileName )
  call Error%Add_Line( "  Name of the section:          " // Input%GetName() )
  call Error%Add_Line( "  Section start line index:     " // Convert_To_String(Start_Index) )
  call Error%Add_Line( "  Section end line index:       " // Convert_To_String(End_Index) )
  call Error%Add_Line( "  Procedure path:               " // ProcPath )
  call Error%Add_Line( "  Content of the input file:" )
  do i = 1,size(Lines)
    call Error%Add_Line( "    " // Lines(i)%GetValue() )
  end do
  call Error%Raise( Unit=Logger%GetUnit() )
End Subroutine
