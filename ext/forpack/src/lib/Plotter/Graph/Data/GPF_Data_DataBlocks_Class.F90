Module GPF_Data_DataBlocks_Class

  use GPF_Parameters            ,only:  rkp
  use GPF_Data_Base_Class       ,only:  GPF_Data_Base_Type
  use GPF_DataBlocks_Class        ,only:  GPF_DataBlocks_Type

  implicit none

  private
  public  ::  GPF_Data_DataBlocks_Type
  public  ::  Construct_Data_DataBlocks

  Type  ,extends(GPF_Data_Base_Type)                    ::  GPF_Data_DataBlocks_Type
    type(GPF_DataBlocks_Type)   ::  DataBlocks
  contains                                                                                                      !  Starting derived-type procedure declarations
    private                                                                                                     !  Setting private type-bound procedures
    procedure   ,public   ::  Get_X_Min       =>  Get_X_Min_DataBlocks        ! Gets the minimum value for X
    procedure   ,public   ::  Get_X_Max       =>  Get_X_Max_DataBlocks        ! Gets the maximum value for X
    procedure   ,public   ::  Get_Y_Min       =>  Get_Y_Min_DataBlocks        ! Gets the minimum value for Y
    procedure   ,public   ::  Get_Y_Max       =>  Get_Y_Max_DataBlocks        ! Gets the maximum value for Y
    procedure   ,public   ::  Get_Z_Min       =>  Get_Z_Min_DataBlocks        ! Gets the minimum value for Z
    procedure   ,public   ::  Get_Z_Max       =>  Get_Z_Max_DataBlocks        ! Gets the maximum value for Z
    procedure   ,public   ::  Set_NColumns    =>  Set_NColumns_DataBlocks     ! Sets the number of columns
    procedure   ,public   ::  Write           =>  Write_Data_DataBlocks       ! Writes the data into a file
  End Type

  Interface             Construct_Data_DataBlocks
    Module Procedure    Construct_Data_DataBlocks
  End Interface

  Interface

    Module Subroutine Construct_Data_DataBlocks( This, DataBlocks )
      class(GPF_Data_Base_Type)     ,allocatable            ,intent(out)    ::  This                            !< Passed-object dummy argument
      type(GPF_DataBlocks_Type)                               ,intent(in)     ::  DataBlocks
    End Subroutine

    Module Subroutine Write_Data_DataBlocks(  This, Unit )
      class(GPF_Data_DataBlocks_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                                 ,intent(in)     ::  Unit                            !< File unit number
    End Subroutine

    Module Function Get_X_Min_DataBlocks( This ) result( X_Min )
      class(GPF_Data_DataBlocks_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(rkp)                                                               ::  X_Min                           !< Minimum value for X
    End Function

    Module Function Get_X_Max_DataBlocks( This ) result( X_Max )
      class(GPF_Data_DataBlocks_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(rkp)                                                               ::  X_Max                           !< Maximum value for X
    End Function

    Module Function Get_Y_Min_DataBlocks( This ) result( Y_Min )
      class(GPF_Data_DataBlocks_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(rkp)                                                               ::  Y_Min                           !< Minimum value for X
    End Function

    Module Function Get_Y_Max_DataBlocks( This ) result( Y_Max )
      class(GPF_Data_DataBlocks_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(rkp)                                                               ::  Y_Max                           !< Maximum value for Y
    End Function

    Module Function Get_Z_Min_DataBlocks( This ) result( Z_Min )
      class(GPF_Data_DataBlocks_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(rkp)                                                               ::  Z_Min                           !< Minimum value for Z
    End Function

    Module Function Get_Z_Max_DataBlocks( This ) result( Z_Max )
      class(GPF_Data_DataBlocks_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(rkp)                                                               ::  Z_Max                           !< Maximum value for Z
    End Function

    Module Subroutine Set_NColumns_DataBlocks(  This )
      class(GPF_Data_DataBlocks_Type)                           ,intent(inout)  ::  This                            !< Passed-object dummy argument
    End Subroutine

  End Interface

End Module