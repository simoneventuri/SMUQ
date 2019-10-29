SubModule(GPF_DataBlocks_Class) GPF_DataBlocks_SubClass

  use Logger_Class              ,only:  Logger

  implicit none

  contains

Module Procedure FinalizeDataBlocks
  This%NItems   =   0
  if ( allocated(This%Items) ) deallocate(This%Items)
End Procedure

Module Procedure FreeDataBlocks
  This%NItems   =   0
  if ( allocated(This%Items) ) deallocate(This%Items)
End Procedure


Module Procedure AddDataBlockFromXYZ
  use GPF_DataBlock_Class    ,only:  NewDataBlock
  type(GPF_DataBlock_Type)                                              ::  DataBlock
  DataBlock   =   NewDataBlock( x, y, z )
  call This%Add( DataBlock )
End Procedure

Module Procedure AddDataBlockFromDataBlock
  integer                                                               ::  i, N
  type(GPF_DataBlock_Type) ,allocatable                                 ::  List(:)
  if ( .Not. allocated(This%Items) ) allocate( This%Items(0) )
  N   =   size(This%Items)
  allocate( List(N+1) )
  do i = 1,N
    List(i)   =   This%Items(i)
  end do
  i           =   N + 1
  List(i)     =   DataBlock
  call move_alloc( List, This%Items )
  This%NItems =   size(This%Items)
End Procedure

End SubModule