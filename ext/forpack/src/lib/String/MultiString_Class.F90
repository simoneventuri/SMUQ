Module MultiString_Class

  implicit none

  private
  public  ::  MultiString_Type

  Type                                                  ::  MultiString_Type
    integer                                             ::  NLines  =       0
    integer                                             ::  Length  =       0
    character(:)        ,dimension(:)   ,allocatable    ::  Lines
  contains
    private
    Final                 ::  FinalizeMultiString
    procedure   ,public   ::  Initialize    =>    InitializeMultiString
    procedure   ,public   ::  Free          =>    FreeMultiString
    generic     ,public   ::  Add_Line      =>    Add_Line_0D, Add_Line_1D, Add_Line_From_MultiString
    procedure   ,public   ::  AddPrefix
    procedure   ,public   ::  Add_Suffix
    procedure   ,public   ::  Add_Frame
    procedure   ,public   ::  Get_NLines
    procedure   ,public   ::  Get_Length
    procedure   ,public   ::  Get_Line
    generic     ,public   ::  assignment(=)   =>      Assign_To_Character_0D, Assign_To_Character_1D
    procedure             ::  Add_Line_0D
    procedure             ::  Add_Line_1D
    procedure             ::  Add_Line_From_MultiString
    procedure             ::  Assign_To_Character_0D
    procedure             ::  Assign_To_Character_1D
  End Type

  contains

Pure Subroutine FinalizeMultiString( This )
  type(MultiString_Type)                                ,intent(inout)  ::  This
  This%Length    =       0
  This%NLines    =       0
  if ( allocated(This%Lines) ) deallocate(This%Lines)
End Subroutine

Pure Subroutine FreeMultiString( This )
  class(MultiString_Type)                               ,intent(inout)  ::  This
  call FinalizeMultiString(This)
End Subroutine

Pure Subroutine InitializeMultiString( This )
  class(MultiString_Type)                               ,intent(inout)  ::  This
  call FinalizeMultiString(This)
End Subroutine

Pure Subroutine Add_Line_0D( This, Line, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
  class(MultiString_Type)                               ,intent(inout)  ::  This
  character(*)                                          ,intent(in)     ::  Line
  logical                                     ,optional ,intent(in)     ::  At_End  ! Default behavior
  logical                                     ,optional ,intent(in)     ::  At_Start
  integer                                     ,optional ,intent(in)     ::  At_Position
  logical                                     ,optional ,intent(in)     ::  Left_Adjust
  logical                                     ,optional ,intent(in)     ::  Right_Adjust
!   logical                                     ,optional ,intent(in)     ::  Centered
  character(len(Line))  ,dimension(1)                                   ::  Lines
  Lines         =       [Line]
  call This%Add_Line( Lines, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
End Subroutine

Pure Subroutine Add_Line_1D( This, Lines, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
  use String_Module              ,only:  Add_Line_To_String
  class(MultiString_Type)                               ,intent(inout)  ::  This
  character(*)  ,dimension(:)                           ,intent(in)     ::  Lines
  logical                                     ,optional ,intent(in)     ::  At_End  ! Default behavior
  logical                                     ,optional ,intent(in)     ::  At_Start
  integer                                     ,optional ,intent(in)     ::  At_Position
  logical                                     ,optional ,intent(in)     ::  Left_Adjust
  logical                                     ,optional ,intent(in)     ::  Right_Adjust
!   logical                                     ,optional ,intent(in)     ::  Centered

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

  call Add_Line_To_String( This%Lines, Lines_Local, At_End, At_Start, At_Position )
  This%NLines   =       size(This%Lines)
  This%Length   =       len(This%Lines)
End Subroutine

Pure Subroutine Add_Line_From_String( This, Line, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
  use String_Class              ,only:  String_Type
  class(MultiString_Type)                               ,intent(inout)  ::  This
  type(String_Type)                                     ,intent(in)     ::  Line
  logical                                     ,optional ,intent(in)     ::  At_End  ! Default behavior
  logical                                     ,optional ,intent(in)     ::  At_Start
  integer                                     ,optional ,intent(in)     ::  At_Position
  logical                                     ,optional ,intent(in)     ::  Left_Adjust
  logical                                     ,optional ,intent(in)     ::  Right_Adjust
!   logical                                     ,optional ,intent(in)     ::  Centered
  call This%Add_Line( Line%Value, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
End Subroutine

Pure Subroutine Add_Line_From_MultiString( This, Lines, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
  use String_Module              ,only:  Add_Line_To_String
  class(MultiString_Type)                               ,intent(inout)  ::  This
  type(MultiString_Type)                                ,intent(in)     ::  Lines
  logical                                     ,optional ,intent(in)     ::  At_End  ! Default behavior
  logical                                     ,optional ,intent(in)     ::  At_Start
  integer                                     ,optional ,intent(in)     ::  At_Position
  logical                                     ,optional ,intent(in)     ::  Left_Adjust
  logical                                     ,optional ,intent(in)     ::  Right_Adjust
!   logical                                     ,optional ,intent(in)     ::  Centered
  integer                                                               ::  i
  do i = 1,Lines%Get_NLines()
    call This%Add_Line( Lines%Get_Line(i), At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
  end do
End Subroutine

! This procedure assigns a scalar character to a MultiString object.
Pure Subroutine Assign_To_Character_0D( Lhs, Rhs )
  use String_Module              ,only:  Add_Line_To_String
  class(MultiString_Type)                       ,intent(inout)  ::  Lhs
  character(*)                                  ,intent(in)     ::  Rhs
  call Lhs%Add_Line( Rhs )
End Subroutine

! This procedure assigns a rank 1 character array to a MultiString object.
Pure Subroutine Assign_To_Character_1D( Lhs, Rhs )
  use String_Module              ,only:  Add_Line_To_String
  class(MultiString_Type)                       ,intent(inout)  ::  Lhs
  character(*)  ,dimension(:)                   ,intent(in)     ::  Rhs
  call Lhs%Add_Line( Rhs )
End Subroutine

Pure Subroutine AddPrefix( This, Prefix ) !, From, To )
  class(MultiString_Type)                               ,intent(inout)  ::  This
  character(*)                                          ,intent(in)     ::  Prefix
!   integer                                     ,optional ,intent(in)     ::  From            ! NOT IMPLEMENTED
!   integer                                     ,optional ,intent(in)     ::  To              ! NOT IMPLEMENTED
  character(:)        ,dimension(:)   ,allocatable                      ::  Lines
  if ( .Not. allocated(This%Lines) ) return
  This%Length   =       This%Length + len(Prefix)                               ! Setting the new length of the lines, including the prefix
  allocate( Character(This%Length) :: Lines(This%NLines) )
  Lines         =       Prefix // This%Lines
  call FinalizeMultiString(This)
  call This%Add_Line( Lines )
End Subroutine

Pure Subroutine Add_Suffix( This, Suffix ) !, From, To )
  class(MultiString_Type)                               ,intent(inout)  ::  This
  character(*)                                          ,intent(in)     ::  Suffix
!   integer                                     ,optional ,intent(in)     ::  From            ! NOT IMPLEMENTED
!   integer                                     ,optional ,intent(in)     ::  To              ! NOT IMPLEMENTED
  character(:)        ,dimension(:)   ,allocatable                      ::  Lines
  if ( .Not. allocated(This%Lines) ) return
  This%Length   =       This%Length + len(Suffix)                               ! Setting the new length of the lines, including the prefix
  allocate( Character(This%Length) :: Lines(This%NLines) )
  Lines         =       This%Lines // Suffix
  call FinalizeMultiString(This)
  call This%Add_Line( Lines )
End Subroutine

Pure Subroutine Add_Frame( This, FChar, Space, HSpace, VSpace )
  class(MultiString_Type)                               ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  FChar
  integer                                     ,optional ,intent(in)     ::  Space
  integer                                     ,optional ,intent(in)     ::  HSpace
  integer                                     ,optional ,intent(in)     ::  VSpace
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
    call This%Add_Line( "", At_Start = .True. )
    call This%Add_Line( "", At_End   = .True. )
  end do


  call This%AddPrefix( FC // H_Spacing )
  call This%Add_Suffix( H_Spacing // FC )

  Horizontal_Line   = repeat( FC, This%Length )

  call This%Add_Line( Horizontal_Line, At_Start = .True. )
  call This%Add_Line( Horizontal_Line, At_End   = .True. )

End Subroutine



Pure Function Get_Line( This, iLine ) result(Line)
  use String_Module              ,only:  Add_Line_To_String
  class(MultiString_Type)                               ,intent(in)     ::  This
  integer                                               ,intent(in)     ::  iLine
  character(:)  ,allocatable                                            ::  Line
  Line          =       ""
  if ( (iLine<=0) .or. (iLine>This%NLines) ) return
  Line          =       trim( This%Lines(iLine) )
End Function

Pure Elemental Function Get_NLines( This ) result(NLines)
  class(MultiString_Type)                               ,intent(in)     ::  This
  integer                                                               ::  NLines
  NLines        =       This%NLines
End Function

Pure Elemental Function Get_Length( This ) result(Length)
  class(MultiString_Type)                               ,intent(in)     ::  This
  integer                                                               ::  Length
  Length        =       This%Length
End Function


End Module