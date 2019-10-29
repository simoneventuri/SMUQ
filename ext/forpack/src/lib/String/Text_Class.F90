Module Text_Class

  use String_Class              ,only:  String_Type

  implicit none

  private
  public        ::  Text_Type

  Type                          ::  Text_Type
    integer                     ::  NLines  =       0
    integer                     ::  Length  =       0
    character(:)  ,allocatable  ::  Lines(:)
  contains
    private
    Final                 ::  FinalizeText
    generic     ,public   ::  Initialize    =>    InitializeText, InitializeTextFromChar1D
    procedure   ,public   ::  Free          =>    FreeText
    generic     ,public   ::  AddLine       =>    AddLineFromChar0D, AddLineFromChar1D, AddLineFromString0D, AddLineFromText
    procedure   ,public   ::  AddPrefix
    procedure   ,public   ::  AddSuffix
    procedure   ,public   ::  AddFrame
    procedure   ,public   ::  GetNumberOfLines
    procedure   ,public   ::  GetLength
    procedure   ,public   ::  GetLine
    procedure   ,public   ::  GetLines
    generic     ,public   ::  assignment(=)   =>      AssignToChar0D, AssignToChar1D
    procedure             ::  AddLineFromChar0D
    procedure             ::  AddLineFromChar1D
    procedure             ::  AddLineFromString0D
    procedure             ::  AddLineFromText
    procedure             ::  AssignToChar0D
    procedure             ::  AssignToChar1D
    procedure             ::  InitializeText
    procedure             ::  InitializeTextFromChar1D
  End Type

  Interface             Text_Type
    Module Procedure    ConstructText
  End Interface

  Interface
    Pure Module Subroutine InitializeText( This )
      class(Text_Type)                                      ,intent(inout)  ::  This
    End Subroutine

    Pure Module Subroutine InitializeTextFromChar1D( This, Lines )
      class(Text_Type)                                      ,intent(inout)  ::  This
      character(*)  ,dimension(:)                           ,intent(in)     ::  Lines
    End Subroutine

    Pure Module Function ConstructText( Lines ) result(This)
      character(*)  ,dimension(:)                           ,intent(in)     ::  Lines
      type(Text_Type)                                                       ::  This
    End Function


    Pure Module Subroutine FinalizeText( This )
      type(Text_Type)                                ,intent(inout)  ::  This
    End Subroutine

    Pure Module Subroutine FreeText( This )
      class(Text_Type)                                      ,intent(inout)  ::  This
    End Subroutine

    Pure Module Subroutine AddLineFromChar0D( This, Line, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
      class(Text_Type)                                      ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Line
      logical                                     ,optional ,intent(in)     ::  At_End  ! Default behavior
      logical                                     ,optional ,intent(in)     ::  At_Start
      integer                                     ,optional ,intent(in)     ::  At_Position
      logical                                     ,optional ,intent(in)     ::  Left_Adjust
      logical                                     ,optional ,intent(in)     ::  Right_Adjust
    End Subroutine

    Pure Module Subroutine AddLineFromChar1D( This, Lines, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
      class(Text_Type)                                      ,intent(inout)  ::  This
      character(*)  ,dimension(:)                           ,intent(in)     ::  Lines
      logical                                     ,optional ,intent(in)     ::  At_End  ! Default behavior
      logical                                     ,optional ,intent(in)     ::  At_Start
      integer                                     ,optional ,intent(in)     ::  At_Position
      logical                                     ,optional ,intent(in)     ::  Left_Adjust
      logical                                     ,optional ,intent(in)     ::  Right_Adjust
    End Subroutine

    Pure Module Subroutine AddLineFromString0D( This, Line, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
      class(Text_Type)                                      ,intent(inout)  ::  This
      type(String_Type)                                     ,intent(in)     ::  Line
      logical                                     ,optional ,intent(in)     ::  At_End  ! Default behavior
      logical                                     ,optional ,intent(in)     ::  At_Start
      integer                                     ,optional ,intent(in)     ::  At_Position
      logical                                     ,optional ,intent(in)     ::  Left_Adjust
      logical                                     ,optional ,intent(in)     ::  Right_Adjust
    End Subroutine

    Pure Module Subroutine AddLineFromText( This, Lines, At_End, At_Start, At_Position, Left_Adjust, Right_Adjust ) !, Centered )
      class(Text_Type)                                      ,intent(inout)  ::  This
      type(Text_Type)                                       ,intent(in)     ::  Lines
      logical                                     ,optional ,intent(in)     ::  At_End  ! Default behavior
      logical                                     ,optional ,intent(in)     ::  At_Start
      integer                                     ,optional ,intent(in)     ::  At_Position
      logical                                     ,optional ,intent(in)     ::  Left_Adjust
      logical                                     ,optional ,intent(in)     ::  Right_Adjust
    End Subroutine

    Pure Module Subroutine AssignToChar0D( Lhs, Rhs )
      class(Text_Type)                                      ,intent(inout)  ::  Lhs
      character(*)                                          ,intent(in)     ::  Rhs
    End Subroutine

    Pure Module Subroutine AssignToChar1D( Lhs, Rhs )
      class(Text_Type)                                      ,intent(inout)  ::  Lhs
      character(*)  ,dimension(:)                           ,intent(in)     ::  Rhs
    End Subroutine

    Pure Module Subroutine AddPrefix( This, Prefix, From, To )
      class(Text_Type)                                      ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Prefix
      integer                                     ,optional ,intent(in)     ::  From
      integer                                     ,optional ,intent(in)     ::  To
    End Subroutine

    Pure Module Subroutine AddSuffix( This, Suffix, From, To )
      class(Text_Type)                                      ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Suffix
      integer                                     ,optional ,intent(in)     ::  From
      integer                                     ,optional ,intent(in)     ::  To
    End Subroutine

!     Pure
    Module Subroutine AddFrame( This, FChar, Space, HSpace, VSpace )
      class(Text_Type)                                      ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  FChar
      integer                                     ,optional ,intent(in)     ::  Space
      integer                                     ,optional ,intent(in)     ::  HSpace
      integer                                     ,optional ,intent(in)     ::  VSpace
    End Subroutine

    Pure Module Function GetLine( This, iLine, Last, First ) result(Line)
      class(Text_Type)                                      ,intent(in)     ::  This
      integer                                     ,optional ,intent(in)     ::  iLine
      logical                                     ,optional ,intent(in)     ::  First
      logical                                     ,optional ,intent(in)     ::  Last
      character(:)  ,allocatable                                            ::  Line
    End Function

    Pure Module Function GetLines( This ) result(Lines)
      class(Text_Type)                                      ,intent(in)     ::  This
      character(This%Length)  ,dimension(This%NLines)                       ::  Lines
    End Function

    Pure Elemental Module Function GetNumberOfLines( This ) result(NLines)
      class(Text_Type)                                      ,intent(in)     ::  This
      integer                                                               ::  NLines
    End Function

    Pure Elemental Module Function GetLength( This ) result(Length)
      class(Text_Type)                                      ,intent(in)     ::  This
      integer                                                               ::  Length
    End Function

  End Interface

End Module