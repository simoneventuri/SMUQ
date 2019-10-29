Module GPF_Data_X2Y2Z2_Class

  use GPF_Parameters            ,only:  rkp
  use GPF_Data_Base_Class       ,only:  GPF_Data_Base_Type

  implicit none

  private
  public  ::  GPF_Data_X2Y2Z2_Type
  public  ::  Construct_Data_X2Y2Z2

  Type  ,extends(GPF_Data_Base_Type)                    ::  GPF_Data_X2Y2Z2_Type
    real(rkp)   ,dimension( :, : )      ,allocatable    ::  X                                               !< X-Data
    real(rkp)   ,dimension( :, : )      ,allocatable    ::  Y                                               !< Y-Data
    real(rkp)   ,dimension( :, : )      ,allocatable    ::  Z                                               !< Z-Data
  contains
    private
    procedure   ,public   ::  Get_X_Min       =>  Get_X_Min_X2Y2Z2        ! Gets the minimum value for X
    procedure   ,public   ::  Get_X_Max       =>  Get_X_Max_X2Y2Z2        ! Gets the maximum value for X
    procedure   ,public   ::  Get_Y_Min       =>  Get_Y_Min_X2Y2Z2        ! Gets the minimum value for Y
    procedure   ,public   ::  Get_Y_Max       =>  Get_Y_Max_X2Y2Z2        ! Gets the maximum value for Y
    procedure   ,public   ::  Get_Z_Min       =>  Get_Z_Min_X2Y2Z2        ! Gets the minimum value for Z
    procedure   ,public   ::  Get_Z_Max       =>  Get_Z_Max_X2Y2Z2        ! Gets the maximum value for Z
    procedure   ,public   ::  Set_NColumns    =>  Set_NColumns_X2Y2Z2     ! Sets the number of columns
    procedure   ,public   ::  Write           =>  Write_Data_X2Y2Z2       ! Writes the data into a file
  End Type

  Interface             Construct_Data_X2Y2Z2
    Module Procedure    Construct_Data_X2Y2Z2
  End Interface

  contains

Subroutine Construct_Data_X2Y2Z2( This, X, Y, Z )
  implicit none
  class(GPF_Data_Base_Type)     ,allocatable            ,intent(out)    ::  This                            !< Passed-object dummy argument
  real(rkp)     ,dimension( :, :    )                   ,intent(in)     ::  X                               !< X-Data
  real(rkp)     ,dimension( :, :    )                   ,intent(in)     ::  Y                               !< Y-Data
  real(rkp)     ,dimension( :, :    )                   ,intent(in)     ::  Z                               !< Z-Data
  allocate( GPF_Data_X2Y2Z2_Type :: This )                                                                      ! Allocating the structure
  select type (This)
    type is (GPF_Data_X2Y2Z2_Type)
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!       allocate( This%X(size(X,1),size(X,2)) )                                                                   ! Allocating and setting X-data
!       allocate( This%Y(size(Y,1),size(Y,2)) )                                                                   ! Allocating and setting Y-data
!       allocate( This%Z(size(Z,1),size(Z,2)) )                                                                   ! Allocating and setting Z-data
!       This%X  =  X
!       This%Y  =  Y
!       This%Z  =  Z
      allocate( This%X(size(X,1),size(X,2)), source=X )                                                                                  ! Allocating and setting X-data
      allocate( This%Y(size(Y,1),size(Y,2)), source=Y )                                                                                  ! Allocating and setting Y-data
      allocate( This%Z(size(Z,1),size(Z,2)), source=Z )                                                                                  ! Allocating and setting Z-data
#else
      allocate( This%X, source=X )                                                                                  ! Allocating and setting X-data
      allocate( This%Y, source=Y )                                                                                  ! Allocating and setting Y-data
      allocate( This%Z, source=Z )                                                                                  ! Allocating and setting Z-data
#endif
  end select

  This%NPoint   =       size(X,1)
  This%NLine    =       1
  This%NAbsci   =       1
  This%NAxes    =       3

  This%X_Dim    =       2
  This%Y_Dim    =       2
  This%Z_Dim    =       2


End Subroutine

Subroutine Write_Data_X2Y2Z2(  This, Unit )
  implicit none
  class(GPF_Data_X2Y2Z2_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  integer                                                               ::  iX, iY
  do iX = 1,size(This%X,1)
    do iY = 1,size(This%Y,2)
      write(Unit,This%Format) This%X(iX,iY), This%Y(iX,iY), This%Z(iX,iY)
    end do
    write(Unit,*)
  end do
End Subroutine


Function Get_X_Min_X2Y2Z2( This ) result( X_Min )
  implicit none
  class(GPF_Data_X2Y2Z2_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  X_Min                           !< Minimum value for X
  X_Min         =       minval(This%X)                                                                          ! Getting the minimum value for X
End Function

Function Get_X_Max_X2Y2Z2( This ) result( X_Max )
  implicit none
  class(GPF_Data_X2Y2Z2_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  X_Max                           !< Maximum value for X
  X_Max         =       maxval(This%X)                                                                          ! Getting the maximum value for X
End Function

Function Get_Y_Min_X2Y2Z2( This ) result( Y_Min )
  implicit none
  class(GPF_Data_X2Y2Z2_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Y_Min                           !< Minimum value for X
  Y_Min         =       minval(This%Y)                                                                          ! Getting the minimum value for X
End Function

Function Get_Y_Max_X2Y2Z2( This ) result( Y_Max )
  implicit none
  class(GPF_Data_X2Y2Z2_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Y_Max                           !< Maximum value for Y
  Y_Max         =       maxval(This%Y)                                                                          ! Getting the maximum value for Y
End Function

Function Get_Z_Min_X2Y2Z2( This ) result( Z_Min )
  implicit none
  class(GPF_Data_X2Y2Z2_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Z_Min                           !< Minimum value for Z
  Z_Min         =       minval(This%Z)                                                                          ! Getting the minimum value for Y
End Function

Function Get_Z_Max_X2Y2Z2( This ) result( Z_Max )
  implicit none
  class(GPF_Data_X2Y2Z2_Type)                           ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Z_Max                           !< Maximum value for Z
  Z_Max         =       maxval(This%Z)                                                                          ! Getting the maximum value for Y
End Function

Subroutine Set_NColumns_X2Y2Z2(  This )
  implicit none
  class(GPF_Data_X2Y2Z2_Type)                           ,intent(inout)  ::  This                            !< Passed-object dummy argument
  This%NColumns =       3                                                                                       ! Setting the number of columns in the data file to 3 corresponding to X, Y and Z data
End Subroutine

End Module