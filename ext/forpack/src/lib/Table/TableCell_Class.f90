Module TableCell_Class

  implicit none

  private
  public  ::  TableCell_Type

  Type  ::  TableCell_Type
    integer                                                     ::  Length
    character(:)        ,allocatable                            ::  Value
  contains
    generic     ,public         ::  SetValue      =>      SetValueCharacter
    procedure   ,public         ::  SetLength    =>      Set_Cell_Length
    procedure   ,public         ::  Adjust_Left   =>      Adjust_Cell_Left
    procedure   ,public         ::  Adjust_Right  =>      Adjust_Cell_Right
    procedure   ,private        ::  SetValueCharacter
  End Type

  Interface
    Module Subroutine SetValueCharacter( This, Value )
      class(TableCell_Type)                                 ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Value
    End Subroutine
    Module Subroutine Set_Cell_Length( This, Length )
      class(TableCell_Type)                                 ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  Length
    End Subroutine
    Module Subroutine Adjust_Cell_Left( This )
      class(TableCell_Type)                                 ,intent(inout)  ::  This
    End Subroutine
    Module Subroutine Adjust_Cell_Right( This )
      class(TableCell_Type)                                 ,intent(inout)  ::  This
    End Subroutine
  End Interface

End Module