SubModule(GPF_Data_DataBlocks_Class) GPF_Data_DataBlocks_SubClass

  implicit none

  contains

Module Procedure Construct_Data_DataBlocks
  integer                                                               ::  k
  allocate( GPF_Data_DataBlocks_Type :: This )
  select type (This)
    type is (GPF_Data_DataBlocks_Type)
      This%DataBlocks   =   DataBlocks
      This%NPoint     =   0
      do k = 1,size(This%DataBlocks%Items)
        This%NPoint   =   This%NPoint + size(This%DataBlocks%Items(k)%x,1)
      end do
  end select
  This%NLine    =   1
  This%NAbsci   =   1
  This%NAxes    =   3
  This%X_Dim    =   2
  This%Y_Dim    =   2
  This%Z_Dim    =   2
End Procedure

Module Procedure Write_Data_DataBlocks
  integer                                                               ::  i, j, k
  do k = 1,size(This%DataBlocks%Items)
  associate( Data => This%DataBlocks%Items(k) )
    write(Unit,"(a,g0)") "# Datablock ", k
    do i = 1,size(Data%x,1)
      do j = 1,size(Data%y,2)
        write(Unit,This%Format) Data%x(i,j), Data%y(i,j), Data%z(i,j)
      end do
      write(Unit,*)
    end do
    write(Unit,*)
  end associate
  end do

End Procedure

Module Procedure Get_X_Min_DataBlocks
  integer                                                               ::  k
  X_Min     =   huge(1.0_rkp)
  do k = 1,size(This%DataBlocks%Items)
    X_Min   =   min( X_Min , minval(This%DataBlocks%Items(k)%x) )
  end do
End Procedure

Module Procedure Get_X_Max_DataBlocks
  integer                                                               ::  k
  X_Max     =   -epsilon(1.0_rkp)
  do k = 1,size(This%DataBlocks%Items)
    X_Max   =   max( X_Max , maxval(This%DataBlocks%Items(k)%x) )
  end do
End Procedure

Module Procedure Get_Y_Min_DataBlocks
  integer                                                               ::  k
  Y_Min     =   huge(1.0_rkp)
  do k = 1,size(This%DataBlocks%Items)
    Y_Min   =   min( Y_Min , minval(This%DataBlocks%Items(k)%y) )
  end do
End Procedure

Module Procedure Get_Y_Max_DataBlocks
  integer                                                               ::  k
  Y_Max     =   -epsilon(1.0_rkp)
  do k = 1,size(This%DataBlocks%Items)
    Y_Max   =   max( Y_Max , maxval(This%DataBlocks%Items(k)%y) )
  end do
End Procedure

Module Procedure Get_Z_Min_DataBlocks
  integer                                                               ::  k
  Z_Min     =   huge(1.0_rkp)
  do k = 1,size(This%DataBlocks%Items)
    Z_Min   =   min( Z_Min , minval(This%DataBlocks%Items(k)%z) )
  end do
End Procedure

Module Procedure Get_Z_Max_DataBlocks
  integer                                                               ::  k
  Z_Max     =   -epsilon(1.0_rkp)
  do k = 1,size(This%DataBlocks%Items)
    Z_Max   =   max( Z_Max, maxval(This%DataBlocks%Items(k)%z) )
  end do
End Procedure

Module Procedure Set_NColumns_DataBlocks
  This%NColumns =   3
End Procedure

End SubModule