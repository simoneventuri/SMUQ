SubModule(File_Class) File_SubClass

  implicit none

!   logical ,parameter  ::  Dbg = .True.

  contains

Module Procedure OpenFile
  use Utilities_Library     ,only:  PresentAndTrue
  character(:)  ,allocatable                                            ::  Action
  This%Name   =   trim(adjustl(FileName))
  if      ( PresentAndTrue(ReadOnly)  ) then; Action = "READ"
  else if ( PresentAndTrue(WriteOnly) ) then; Action = "WRITE"
  else;                                       Action = "READWRITE"; end if
  open( NewUnit=This%Unit, File=This%Name, iostat=This%Status, Action=Action )
  if ( present(iostat) ) iostat = This%Status
End Procedure



! !   logical                                                       ::  i_Exist                                 ! File existence indicator
!   This%Name     =       trim(adjustl(FileName))                                                                 ! Setting the file name
! !   inquire( File=This%Name, Exist=i_Exist )                                                                      ! Setting the file existence indicator
! !   if ( .not.i_Exist ) call Error%Raise( "The file '"//This%Name//"' does not exists" )                                ! If the current FILE DOES NOT EXIST, then error
!   if ( PresentAndTrue(ReadOnly) ) then
!     open( NewUnit=This%Unit, File=This%Name, iostat=This%Status, Action="READ" )                            ! Opening file for "read only mode"
!   else ( PresentAndTrue(WriteOnly) ) then
!     open( NewUnit=This%Unit, File=This%Name, iostat=This%Status, Action="READ" )                            ! Opening file for "read only mode"
!   else
!     open( NewUnit=This%Unit, File=This%Name, iostat=This%Status )                            ! Opening file for "read only mode"
!   end if
! !   if ( ios /= 0 ) call Error%Open( Unit=This%Unit, File=This%Name )                                             ! Checking for errors during file opening
!   if ( present(iostat) ) iostat = This%Status


Module Procedure SetFileProperties
  if ( present(Comment) ) then
    This%i_Comment  =   .True.
    This%Comment    =   Comment
  end if
  if ( present(Separator) ) then
    This%i_Separator  =   .True.
    This%Separator    =   Separator
  end if
  if ( present(IgnoreBlankLines) ) then
    This%i_IgnoreBlankLines =   .True.
    This%IgnoreBlankLines   =   IgnoreBlankLines
  end if
End Procedure


Module Procedure Close_File
  if ( present(Status) ) then
    close( This%Unit, iostat=This%Status, Status=Status )
  else
    close( This%Unit, iostat=This%Status )
  end if
  if ( present(iostat) ) iostat = This%Status
End Procedure

Module Procedure RewindFile
  rewind( This%Unit, iostat=This%Status )
  if ( present(iostat) ) iostat = This%Status
End Procedure

Module Procedure Write_CHAR_0d
  write(This%Unit,"(a)",iostat=This%Status) trim(Variable)
End Procedure

Module Procedure GetStatus
  Status    =   This%Status
End Procedure

Module Procedure GetNumberOfLines
  use, intrinsic :: iso_fortran_env ,only:  IOStat_End
  logical                                                               ::  IgnoreComments_
  logical                                                               ::  IgnoreBlankLines_
  character(500000)                                                     ::  Line
  character(:)  ,allocatable                                            ::  Tmp
  character(1)                                                          ::  FirstChar
  character(1)                                                          ::  Comment_

!   if (Dbg) write(*,"(a)") "  [GetNumberOfLines] Entering"

  if ( This%Formatted ) then

    IgnoreBlankLines_   =   .False.
    if ( This%i_IgnoreBlankLines )    IgnoreBlankLines_ = This%IgnoreBlankLines
    if ( present(IgnoreBlankLines) )  IgnoreBlankLines_ = IgnoreBlankLines

    IgnoreComments_     =   .False.
    Comment_            =   "!"
    if ( This%i_Comment) then
      IgnoreComments_   =   .True.
      Comment_          =   This%Comment
    end if
    if ( present(Comment) )           then
      IgnoreComments_   =   .True.
      Comment_          =   Comment
    end if

    call This%Rewind()
    NumberOfLines   =   0
    do
      read(This%Unit,"(a)",iostat=This%Status) Line
      if ( This%Status == IOStat_End ) exit
      if ( This%Status > 0 ) exit
      if ( IgnoreComments_ ) then
        Tmp         =   adjustl(Line)
        FirstChar   =   Tmp(1:1)
        if ( FirstChar == Comment_ ) cycle
      end if
      if ( IgnoreBlankLines_ ) then
        if ( trim(Line) == "" ) cycle
      end if
      NumberOfLines = NumberOfLines + 1
! !       if (Dbg.and.mod(NumberOfLines,1000) == 0) write(*,"(a,i9)") "-> ", NumberOfLines
    end do
    call This%Rewind()

  else

    call This%Rewind()
    NumberOfLines   =   0
    do
      read(This%Unit,iostat=This%Status)
      if ( This%Status == IOStat_End ) exit
      if ( This%Status > 0 ) exit
      NumberOfLines   =   NumberOfLines + 1
    end do
    call This%Rewind()

  end if

!   if (Dbg) write(*,"(a,g0)") "  [GetNumberOfLines] NumberOfLines = ", NumberOfLines
!   if (Dbg) write(*,"(a)") "  [GetNumberOfLines] Exiting"

End Procedure

Module Procedure GetNumberOfColumns
  use, intrinsic :: iso_fortran_env ,only:  IOStat_End
  use String_Library      ,only:  GetNumberOfItems
  logical                                                               ::  IgnoreComments_
  logical                                                               ::  IgnoreBlankLines_
  character(500000)                                                     ::  Line
  character(:)  ,allocatable                                            ::  Tmp
  character(1)                                                          ::  FirstChar

  character(1)                                                          ::  Comment_, Separator_

  IgnoreBlankLines_   =   .False.
  if ( This%i_IgnoreBlankLines )    IgnoreBlankLines_ = This%IgnoreBlankLines
  if ( present(IgnoreBlankLines) )  IgnoreBlankLines_ = IgnoreBlankLines

  IgnoreComments_     =   .False.
  if ( This%i_Comment) then
    IgnoreComments_   =   .True.
    Comment_          =   This%Comment
  end if
  if ( present(Comment) )           then
    IgnoreComments_   =   .True.
    Comment_          =   Comment
  end if

  Separator_ = " "
  if ( This%i_Separator)    Separator_ = This%Separator
  if ( present(Separator) ) Separator_ = Separator

  call This%Rewind()
  NumberOfColumns   =   0
  do
    read(This%Unit,"(a)",iostat=This%Status) Line
    if ( This%Status == IOStat_End ) exit
    if ( This%Status > 0 ) exit
    if ( IgnoreComments_ ) then
      Tmp         =   adjustl(Line)
      FirstChar   =   Tmp(1:1)
      if ( FirstChar == Comment_ ) cycle
    end if
    if ( IgnoreBlankLines_ ) then
      if ( trim(Line) == "" ) cycle
    end if
    NumberOfColumns   =   GetNumberOfItems(Line,Separator=Separator_)
    exit
  end do
  call This%Rewind()
End Procedure



Module Procedure GetFileData_REAL64

  use, intrinsic :: iso_fortran_env ,only:  IOStat_End

  integer                                                               ::  i
  integer                                                               ::  NRows
  integer                                                               ::  NColumns
  logical                                                               ::  IgnoreComments_
  logical                                                               ::  IgnoreBlankLines_
  character(500000)                                                     ::  Line
  character(:)  ,allocatable                                            ::  Tmp
  character(1)                                                          ::  FirstChar
  character(1)                                                          ::  Comment_

!   if (Dbg) write(*,"(a)") "[GetFileData_REAL64] Entering"

  IgnoreBlankLines_   =   .False.
  if ( This%i_IgnoreBlankLines )    IgnoreBlankLines_ = This%IgnoreBlankLines

  IgnoreComments_     =   .False.
  if ( This%i_Comment) then
    IgnoreComments_   =   .True.
    Comment_          =   This%Comment
  end if

!   if (Dbg) write(*,"(a)") "[GetFileData_REAL64] Calling This%GetNumberOfLines"
  NRows     =   This%GetNumberOfLines(  Comment=This%Comment )

!   if (Dbg) write(*,"(a)") "[GetFileData_REAL64] Calling This%GetNumberOfColumns"
  NColumns  =   This%GetNumberOfColumns(Comment=This%Comment )

!   if (Dbg) write(*,"('NRows    = ',g0)") NRows
!   if (Dbg) write(*,"('NColumns = ',g0)") NColumns

  allocate( Data(NRows,NColumns) )

  call This%Rewind()
  i   =   0
  do
    read(This%Unit,"(a)",iostat=This%Status) Line
    if ( This%Status == IOStat_End ) exit
    if ( This%Status > 0 ) exit
    if ( IgnoreComments_ ) then
      Tmp         =   adjustl(Line)
      FirstChar   =   Tmp(1:1)
      if ( FirstChar == Comment_ ) cycle
    end if
    if ( IgnoreBlankLines_ ) then
      if ( trim(Line) == "" ) cycle
    end if
    i = i + 1
!     if (Dbg.and.mod(i,1000) == 0) write(*,"(f8.4)") real(i*100,kind(Data))/NRows
    backspace( This%Unit )
    read(This%Unit,*,iostat=This%Status) Data(i,:)
  end do
  call This%Rewind()

End Procedure

End SubModule