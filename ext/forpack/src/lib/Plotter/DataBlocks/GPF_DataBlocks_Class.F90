Module GPF_DataBlocks_Class

  use GPF_Parameters            ,only:  rkp
  use GPF_DataBlock_Class       ,only:  GPF_DataBlock_Type

  implicit none

  private
  public  ::  GPF_DataBlocks_Type

  Type  ::  GPF_DataBlocks_Type
    integer                                 ::  NItems = 0
    type(GPF_DataBlock_Type)  ,allocatable  ::  Items(:)
  contains
    private
    Final               ::  FinalizeDataBlocks
    procedure ,public   ::  Free  =>    FreeDataBlocks
    generic   ,public   ::  Add   =>    AddDataBlockFromXYZ, AddDataBlockFromDataBlock
    procedure           ::  AddDataBlockFromXYZ
    procedure           ::  AddDataBlockFromDataBlock
  End Type

  Interface
    Module Subroutine FinalizeDataBlocks( This )
      type(GPF_DataBlocks_Type)                                 ,intent(inout)  ::  This
    End Subroutine

    Module Subroutine FreeDataBlocks( This )
      class(GPF_DataBlocks_Type)                                ,intent(out)    ::  This
    End Subroutine

    Module Subroutine AddDataBlockFromXYZ( This, x, y, z )
      class(GPF_DataBlocks_Type)                                ,intent(inout)  ::  This
      real(rkp)                                                 ,intent(in)     ::  x(:,:)
      real(rkp)                                                 ,intent(in)     ::  y(:,:)
      real(rkp)                                                 ,intent(in)     ::  z(:,:)
    End Subroutine

    Module Subroutine AddDataBlockFromDataBlock( This, DataBlock )
      class(GPF_DataBlocks_Type)                                ,intent(inout)  ::  This
      type(GPF_DataBlock_Type)                                  ,intent(in)     ::  DataBlock
    End Subroutine
  End Interface

End Module