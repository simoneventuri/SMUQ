SubModule(Text_Class) Text_SubClass

  implicit none

  contains

Module Procedure FinalizeText
  This%Length    =       0
  This%NLines    =       0
  if ( allocated(This%Lines) ) deallocate(This%Lines)
End Procedure

Module Procedure FreeText
  call FinalizeText(This)
End Procedure

Module Procedure InitializeText
  call FinalizeText(This)
End Procedure

Module Procedure InitializeTextFromChar1D
  call FinalizeText(This)
  call This%AddLine( Lines )
End Procedure


Module Procedure ConstructText
  call This%Initialize( Lines )
End Procedure

Module Procedure AddLineFromChar0D
  character(len(Line))  ,dimension(1)                                   ::  Lines
  Lines         =       [Line]
  call This%AddLine( Lines, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
End Procedure

Module Procedure AddLineFromChar1D

  use String_Module              ,only:  Add_Line_To_String

  character(len(Lines)) ,dimension(size(Lines))                         ::  Lines_Local

  Lines_Local   =       Lines
  if ( present(Left_Adjust) ) then
  if (Left_Adjust) then
    Lines_Local =       adjustl(Lines_Local)
  end if
  end if

  Lines_Local   =       Lines
  if ( present(Right_Adjust) ) then
  if (Right_Adjust) then
    Lines_Local =       adjustr(Lines_Local)
  end if
  end if

  call Add_Line_To_String( This%Lines, Lines_Local, At_Start, At_Position )
  This%NLines   =       size(This%Lines)
  This%Length   =       len(This%Lines)

End Procedure

Module Procedure AddLineFromString0D
  call This%AddLine( Line%Value, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
End Procedure

Module Procedure AddLineFromText
  integer                                                               ::  i
  do i = 1,Lines%GetNumberOfLines()
    call This%AddLine( Lines%GetLine(i), At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
  end do
End Procedure

! This procedure assigns a scalar character to a Text object.
Module Procedure AssignToChar0D
  call Lhs%AddLine( Rhs )
End Procedure

! This procedure assigns a rank 1 character array to a Text object.
Module Procedure AssignToChar1D
  call Lhs%AddLine( Rhs )
End Procedure

Module Procedure AddPrefix
  use Utilities_Library    ,only:  GetOptArgValue
  character(:)        ,dimension(:)   ,allocatable                      ::  Lines
  integer                                                               ::  iLine
  integer                                                               ::  iLineIni
  integer                                                               ::  iLineFin
  if ( .Not. allocated(This%Lines) ) return
  This%Length     =   This%Length + len(Prefix)                               ! Setting the new length of the lines, including the prefix
  allocate( Character(This%Length) :: Lines(This%NLines) )
  iLineIni        =   GetOptArgValue( 1, From )
  iLineFin        =   GetOptArgValue( size(This%Lines), To )
  Lines(:)        =   This%Lines
  do iLine = iLineIni,iLineFin
    Lines(iLine)  =   Prefix // This%Lines(iLine)
  end do
  call FinalizeText(This)
  call This%AddLine( Lines )
End Procedure

Module Procedure AddSuffix
  use Utilities_Library    ,only:  GetOptArgValue
  character(:)        ,dimension(:)   ,allocatable                      ::  Lines
  integer                                                               ::  iLine
  integer                                                               ::  iLineIni
  integer                                                               ::  iLineFin
  if ( .Not. allocated(This%Lines) ) return
  This%Length     =       This%Length + len(Suffix)                               ! Setting the new length of the lines, including the prefix
  allocate( Character(This%Length) :: Lines(This%NLines) )
  iLineIni        =   GetOptArgValue( 1, From )
  iLineFin        =   GetOptArgValue( size(This%Lines), To )
  Lines(:)        =   This%Lines
  do iLine = iLineIni,iLineFin
    Lines(iLine)  =   This%Lines(iLine) // Suffix
  end do
  call FinalizeText(This)
  call This%AddLine( Lines )
End Procedure

Module Procedure AddFrame
  character(:)  ,allocatable                                            ::  FC
  integer                                                               ::  Local_HSpace      ! Frame horizontal spacing
  integer                                                               ::  Local_VSpace      ! Frame vertical spacing
  character(:)  ,allocatable                                            ::  Horizontal_Line
  character(:)  ,allocatable                                            ::  H_Spacing
  integer                                                               ::  i

  if ( .Not. allocated(This%Lines) ) return

  FC    =    "*"
  if ( present(FChar) ) FC    =    FChar

  Local_HSpace    =     0
  Local_VSpace    =     0

  if ( present(Space) ) then
    Local_HSpace  =     Space
    Local_VSpace  =     Space
    H_Spacing     =     repeat(" ",Local_VSpace)
  end if

  if ( present(HSpace) ) then
    Local_HSpace  =     HSpace
    H_Spacing     =     repeat(" ",Local_HSpace)
  end if

  if ( present(VSpace) ) then
    Local_VSpace  =     VSpace
  end if


  do i = 1,Local_VSpace
    call This%AddLine( "", At_Start = .True. )
    call This%AddLine( "", At_End   = .True. )
  end do


  call This%AddPrefix( FC // H_Spacing )
  call This%AddSuffix( H_Spacing // FC )

  Horizontal_Line   = repeat( FC, This%Length )

  call This%AddLine( Horizontal_Line, At_Start = .True. )
  call This%AddLine( Horizontal_Line, At_End   = .True. )

End Procedure

Module Procedure GetLine
  integer                                                               ::  i

  Line  =   ""
  i     =   0

  if ( present(iLine) ) i = iLine
  if ( present(First) ) i = 1
  if ( present(Last) )  i = This%NLines

  if ( i /= 0 ) then
    if ( (i<=0) .or. (i>This%NLines) ) return
    Line          =       trim( This%Lines(i) )
  end if

End Procedure

Pure Module Function GetLines( This ) result(Lines)
  class(Text_Type)                                      ,intent(in)     ::  This
  character(This%Length)  ,dimension(This%NLines)                       ::  Lines
  Lines   =   This%Lines
End Function

Module Procedure GetNumberOfLines
  NLines        =       This%NLines
End Procedure

Module Procedure GetLength
  Length        =       This%Length
End Procedure

End SubModule