
  Type  ::  Table_Column_Type
    integer                                                     ::  Length
    character(:)        ,allocatable                            ::  Separator
  contains
    procedure   ,public                 ::  SetLength              =>      Set_Column_Length
  End Type

  Type  ::  Table_Border_Type
    character(:)        ,allocatable                            ::  Left_Separator
    character(:)        ,allocatable                            ::  Right_Separator
    character(:)        ,allocatable                            ::  Top_Separator
    character(:)        ,allocatable                            ::  Bottom_Separator
  End Type

  Type  ::  Table_Type
    integer                                                     ::  Unit
    integer                                                     ::  NColumns
    integer                                                     ::  NCharacter_Row
    character(:)        ,allocatable                            ::  Char_NCharacter_Row
    type(Table_Column_Type)     ,dimension(:)   ,allocatable    ::  Columns
    type(Table_Border_Type)                                     ::  Border
  contains
    procedure   ,public                 ::  Set_Columns             =>      Set_Table_Columns
    generic     ,public                 ::  Set_Columns_Length      =>      Set_Table_Columns_Length_From_Lengths, Set_Table_Columns_Length_From_iColumn_Length
    procedure   ,public                 ::  Set_Column_Separator    =>      Set_Table_Column_Separator
    procedure   ,public                 ::  Set_Left_Border         =>      Set_Table_Left_Border
    procedure   ,public                 ::  Set_Right_Border        =>      Set_Table_Right_Border
    procedure   ,public                 ::  Compute_Row_Length      =>      Compute_Table_Row_Length
    procedure   ,public                 ::  Write_Row               =>      Write_Table_Row
    procedure   ,private                ::  Set_Table_Columns_Length_From_Lengths
    procedure   ,private                ::  Set_Table_Columns_Length_From_iColumn_Length
  End Type

  contains

Subroutine Set_Table_Columns( This, NColumns )
  class(Table_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Table object
  integer                                               ,intent(in)     ::  NColumns
  This%NColumns =       NColumns
  if ( allocated(This%Columns) ) deallocate( This%Columns )
  allocate( This%Columns(This%NColumns) )
End Subroutine

Subroutine Set_Table_Columns_Length_From_Lengths( This, Lengths )
  class(Table_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Table object
  integer       ,dimension(:)                           ,intent(in)     ::  Lengths
  integer                                                               ::  i
  if ( size(Lengths) /= This%NColumns ) return  ! @TODO: Error
  do i =1,This%NColumns
    call This%Columns(i)%SetLength( Lengths(i) )
  end do
End Subroutine

Subroutine Set_Table_Columns_Length_From_iColumn_Length( This, iColumn, Length )
  class(Table_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Table object
  integer                                               ,intent(in)     ::  iColumn
  integer                                               ,intent(in)     ::  Length
  if ( iColumn > This%NColumns ) return  ! @TODO: Error
  call This%Columns(iColumn)%SetLength( Length )
End Subroutine

Subroutine Set_Table_Column_Separator( This, Separator )
  class(Table_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Table object
  character(*)                                          ,intent(in)     ::  Separator
  integer                                                               ::  i
  do i =1,This%NColumns
    This%Columns(i)%Separator  =       Separator
  end do
End Subroutine

Subroutine Set_Table_Left_Border( This, String )
  class(Table_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Table object
  character(*)                                          ,intent(in)     ::  String
  This%Border%Left_Separator    =       String
End Subroutine

Subroutine Set_Table_Right_Border( This, String )
  class(Table_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Table object
  character(*)                                          ,intent(in)     ::  String
  This%Border%Right_Separator   =       String
End Subroutine

Subroutine Compute_Table_Row_Length( This )
  class(Table_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Table object
  integer                                                               ::  i
  character(100)                                                        ::  Long_String
  if ( .not. allocated(This%Columns) ) return
  This%NCharacter_Row   =       sum(This%Columns%Length)        ! Length of each column
  do i =1,This%NColumns-1
    This%NCharacter_Row =       This%NCharacter_Row + len(This%Columns(i)%Separator)
  end do
  This%NCharacter_Row =       This%NCharacter_Row + len(This%Border%Left_Separator) + len(This%Border%Right_Separator)
  write(Long_String,"(g0)") This%NCharacter_Row
  This%Char_NCharacter_Row   =       trim(Long_String)
End Subroutine

Subroutine Write_Table_Row( This, Strings )
  class(Table_Type)                                     ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Table object
  character(*)  ,dimension(:)                                           ::  Strings
  integer                                                               ::  i
!   character(:)  ,allocatable                                            ::  Line
  if ( .not. allocated(This%Columns) ) return
  if ( size(Strings) /= This%NColumns ) return
!   Line          =       ""
  do i = 1,This%NColumns
!     Line        =       Line // String_To_String( Strings(i), Length=This%Columns(i)%Length )
!     if ( i /= This%NColumns ) Line = Line // This%Columns(i)%Separator
  end do
!   Line          =       This%Border%Left_Separator // Line // This%Border%Right_Separator
!   write(This%Unit,"(a)") Line
End Subroutine



Subroutine Set_Column_Length( This, Length )
  class(Table_Column_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Table-Column object
  integer                                               ,intent(in)     ::  Length
  This%Length   =       Length
End Subroutine



Subroutine Test_Table()

  use   ,intrinsic      :: iso_fortran_env      ,only:  Output_Unit

  integer                                                               ::  File_Unit

  integer                                                               ::  i
  integer                                                               ::  Length
  character(:)  ,allocatable                                            ::  Line

  integer                                                               ::  NColumns, iColumn, NCharacter_Row
  integer       ,dimension(:)   ,allocatable                            ::  Column_Length
  character(:)  ,allocatable                                            ::  Column_Separator
  character(:)  ,allocatable                                            ::  LHS_Separator, RHS_Separator


  character(100)  ::  Long_String, Char_NCharacter_Row

  type(Table_Type)      ::  Table


  File_Unit     =       Output_Unit
  write(File_Unit,*)


  NColumns      =       6
  call Table%Set_Columns( NColumns )
  Table%Unit    =       File_Unit

  iColumn       =       1
  Length        =       len_trim("A given name")
  call Table%Set_Columns_Length( iColumn, Length )

  iColumn                       =       iColumn + 1
  call Table%Set_Columns_Length( iColumn, 6 )

  iColumn                       =       iColumn + 1
  call Table%Set_Columns_Length( iColumn, 15 )

  iColumn                       =       iColumn + 1
  call Table%Set_Columns_Length( iColumn, 6 )

  iColumn                       =       iColumn + 1
  call Table%Set_Columns_Length( iColumn, 15 )

  iColumn                       =       iColumn + 1
  call Table%Set_Columns_Length( iColumn, 6 )


  call Table%Set_Column_Separator( ' | ' )
  call Table%Set_Left_Border( '| ' )
  call Table%Set_Right_Border( ' |' )

  call Table%Compute_Row_Length()
!   call Table%Set_Horizontal_Line_Sepa

!   call Table%Write_HLine( '-' )
  call Table%Write_Row(  ["", "", "Time [s]", "[%]", "Time [s]", "[%]"] )
!   call Table%Write_HLine( '-' )
  call Table%Write_Row( [       "Name1",        &
                                "Name2",        &
                                "Name3",        &
                                "Name4",        &
                                "Name5",        &
                                "Name6"  ]        )

End Subroutine
