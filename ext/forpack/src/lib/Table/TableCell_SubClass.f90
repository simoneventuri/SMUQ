SubModule(TableCell_Class) TableCell_SubClass

  implicit none

  contains

Module Subroutine SetValueCharacter( This, Value )
  class(TableCell_Type)                                 ,intent(inout)  ::  This
  character(*)                                          ,intent(in)     ::  Value
  This%Value    =   Value
End Subroutine

Module Procedure Set_Cell_Length
  character(:)  ,allocatable                                            ::  String
  allocate( character(Length) :: String )
  String(:)     =       This%Value
  if ( allocated(This%Value) ) deallocate( This%Value )
  allocate( character(Length) :: This%Value )
  This%Value(:) =       String
End Procedure

Module Procedure Adjust_Cell_Left
  character(:)  ,allocatable                                            ::  String
  String        =       This%Value
  This%Value(:) =       adjustl(String)
End Procedure

Module Procedure Adjust_Cell_Right
  character(:)  ,allocatable                                            ::  String
  String        =       This%Value
  This%Value(:) =       adjustr(String)
End Procedure

End SubModule