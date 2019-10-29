Module GPF_Data_Module

  use GPF_Parameters            ,only:  rkp
  use GPF_Data_Base_Class       ,only:  GPF_Data_Base_Type
  use GPF_DataBlocks_Class      ,only:  GPF_DataBlocks_Type

  implicit none

  private
  public  ::  Construct_Data

  Interface
    Module Subroutine Construct_Data( This, X_1D, X_2D, X_2Dp, Y_1D, Y_2D, Z_1D, Z_2D, DataBlocks, Debug )
      class(GPF_Data_Base_Type)             ,allocatable    ,intent(out)    ::  This                            !< Data structure
      real(rkp)     ,dimension(:)                 ,optional ,intent(in)     ::  X_1D                            !< X-Data for 1D variable
      real(rkp)     ,dimension(:,:)               ,optional ,intent(in)     ::  X_2D                            !< X-Data for 2D variable
      real(rkp)     ,dimension(:,:)               ,optional ,intent(in)     ::  X_2Dp
      real(rkp)     ,dimension(:)                 ,optional ,intent(in)     ::  Y_1D                            !< Y-Data for 1D variable
      real(rkp)     ,dimension(:,:)               ,optional ,intent(in)     ::  Y_2D                            !< Y-Data for 2D variable
      real(rkp)     ,dimension(:)                 ,optional ,intent(in)     ::  Z_1D                            !< Z-Data for 1D variable
      real(rkp)     ,dimension(:,:)               ,optional ,intent(in)     ::  Z_2D                            !< Z-Data for 2D variable
      type(GPF_DataBlocks_Type)                     ,optional ,intent(in)     ::  DataBlocks
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
  End Interface

End Module