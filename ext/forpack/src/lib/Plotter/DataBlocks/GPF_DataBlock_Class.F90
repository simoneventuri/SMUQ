Module GPF_DataBlock_Class

  use GPF_Parameters            ,only:  rkp

  implicit none

  private
  public  ::  GPF_DataBlock_Type
  public  ::  NewDataBlock

  Type  ::  GPF_DataBlock_Type
    real(rkp) ,allocatable  ::  x(:,:)
    real(rkp) ,allocatable  ::  y(:,:)
    real(rkp) ,allocatable  ::  z(:,:)
  contains
    private
    Final               ::  FinalizeDataBlock
    procedure ,public   ::  Initialize    =>    InitializeDataBlock
  End Type

  Interface           NewDataBlock
    Module Procedure  NewDataBlockFromXYZ
  End Interface

  Interface

    Module Subroutine FinalizeDataBlock( This )
      type(GPF_DataBlock_Type)                                  ,intent(inout)  ::  This
    End Subroutine

    Module Subroutine InitializeDataBlock( This, x, y, z )
      class(GPF_DataBlock_Type)                                 ,intent(out)    ::  This
      real(rkp)                                                 ,intent(in)     ::  x(:,:)
      real(rkp)                                                 ,intent(in)     ::  y(:,:)
      real(rkp)                                                 ,intent(in)     ::  z(:,:)
    End Subroutine

    Module Function NewDataBlockFromXYZ( x, y, z ) result(This)
      real(rkp)                                                 ,intent(in)     ::  x(:,:)
      real(rkp)                                                 ,intent(in)     ::  y(:,:)
      real(rkp)                                                 ,intent(in)     ::  z(:,:)
      type(GPF_DataBlock_Type)                                                  ::  This
    End Function
  End Interface

End Module