Module Table_Class

  use TableCell_Class          ,only: TableCell_Type

  implicit none

  private
  public        ::  Table_Type

  Type  ::  Table_Type
    integer                                                     ::  NCol
    integer                                                     ::  NRow
    type(TableCell_Type)       ,dimension(:,:) ,allocatable    ::  Cells
  contains
    generic     ,public         ::  Initialize    =>    InitializeTable, InitializeTablePolymorphic
    generic     ,public         ::  SetCellValue  =>    SetCellValuePolymorphic
    generic     ,public         ::  Output        =>    OutputTableToFile
    procedure   ,public         ::  SetSameCellRowLength
    procedure   ,public         ::  AdjustToLeftColumn      ! Adjust to the left all row of the input column
    procedure   ,public         ::  AdjustToRightColumn
!   Initialization procedures
    procedure   ,private        ::  InitializeTable
    procedure   ,private        ::  InitializeTablePolymorphic
!   Procedures to set cells value
    procedure   ,private        ::  SetCellValuePolymorphic
!   Procedures to output the table
    procedure   ,private        ::  OutputTableToFile

  End Type

  Interface

    ! **************************************************************************************************************
    !                               INITIALIZATION PROCEDURES
    ! **************************************************************************************************************
    Module Subroutine InitializeTable( This, NRow, NCol )
      class(Table_Type)                                     ,intent(out)    ::  This
      integer                                               ,intent(in)     ::  NRow
      integer                                               ,intent(in)     ::  NCol
    End Subroutine
    Module Subroutine InitializeTablePolymorphic( This, Matrix )
      class(Table_Type)                                     ,intent(out)    ::  This
      class(*)  ,dimension(:,:)                             ,intent(in)     ::  Matrix
    End Subroutine
!     Module Subroutine InitializeTableCharacter( This, Matrix )
!       class(Table_Type)                                     ,intent(out)    ::  This
!       character(*)  ,dimension(:,:)                         ,intent(in)     ::  Matrix
!     End Subroutine

    ! **************************************************************************************************************
    !                               PROCEDURES TO WRITE THE TABLE
    ! **************************************************************************************************************
    Module Subroutine OutputTableToFile( This, Unit )
      class(Table_Type)                                     ,intent(in)     ::  This
      integer                                               ,intent(in)     ::  Unit
    End Subroutine

    ! **************************************************************************************************************
    !                               PROCEDURES TO SET CELLS VALUE
    ! **************************************************************************************************************
    Module Subroutine SetCellValuePolymorphic( This, iRow, iCol, Value )
      class(Table_Type)                                     ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  iRow
      integer                                               ,intent(in)     ::  iCol
      class(*)                                              ,intent(in)     ::  Value
    End Subroutine
!     Module Subroutine SetCellValueLogical( This, iRow, iCol, Value )
!       class(Table_Type)                                     ,intent(inout)  ::  This
!       integer                                               ,intent(in)     ::  iRow
!       integer                                               ,intent(in)     ::  iCol
!       logical                                               ,intent(in)     ::  Value
!     End Subroutine
!     Module Subroutine SetCellValueInteger( This, iRow, iCol, Value )
!       class(Table_Type)                                     ,intent(inout)  ::  This
!       integer                                               ,intent(in)     ::  iRow
!       integer                                               ,intent(in)     ::  iCol
!       integer                                               ,intent(in)     ::  Value
!     End Subroutine
!     Module Subroutine SetCellValueReal4( This, iRow, iCol, Value )
!       class(Table_Type)                                     ,intent(inout)  ::  This
!       integer                                               ,intent(in)     ::  iRow
!       integer                                               ,intent(in)     ::  iCol
!       real(4)                                               ,intent(in)     ::  Value
!     End Subroutine
!     Module Subroutine SetCellValueReal8( This, iRow, iCol, Value )
!       class(Table_Type)                                     ,intent(inout)  ::  This
!       integer                                               ,intent(in)     ::  iRow
!       integer                                               ,intent(in)     ::  iCol
!       real(8)                                               ,intent(in)     ::  Value
!     End Subroutine
!     Module Subroutine SetCellValueCharacter( This, iRow, iCol, Value )
!       class(Table_Type)                                     ,intent(inout)  ::  This
!       integer                                               ,intent(in)     ::  iRow
!       integer                                               ,intent(in)     ::  iCol
!       character(*)                                          ,intent(in)     ::  Value
!     End Subroutine


    Module Subroutine SetSameCellRowLength( This, j )
      class(Table_Type)                                     ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  j                               ! Index of rows
    End Subroutine
    Module Subroutine AdjustToLeftColumn( This, j )
      class(Table_Type)                                     ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  j                               ! Index of rows
    End Subroutine
    Module Subroutine AdjustToRightColumn( This, j )
      class(Table_Type)                                     ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  j                               ! Index of rows
    End Subroutine
  End Interface

End Module