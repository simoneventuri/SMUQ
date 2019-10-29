SubModule(GPF_DataBlock_Class) GPF_DataBlock_SubClass

  implicit none

  contains

Module Procedure FinalizeDataBlock
  if ( allocated( This%x ) ) deallocate( This%x )
  if ( allocated( This%y ) ) deallocate( This%y )
  if ( allocated( This%z ) ) deallocate( This%z )
End Procedure

Module Procedure InitializeDataBlock
  allocate( This%x(size(x,1),size(x,2)) ); This%x(:,:) = x
  allocate( This%y(size(y,1),size(y,2)) ); This%y(:,:) = y
  allocate( This%z(size(z,1),size(z,2)) ); This%z(:,:) = z
End Procedure

Module Procedure NewDataBlockFromXYZ
  allocate( This%x(size(x,1),size(x,2)) ); This%x(:,:) = x
  allocate( This%y(size(y,1),size(y,2)) ); This%y(:,:) = y
  allocate( This%z(size(z,1),size(z,2)) ); This%z(:,:) = z
End Procedure

End SubModule