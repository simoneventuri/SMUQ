! This is a workaround to plotfigure with several x coordinates.
! The object GPF_Data_X2Y2Z2_Type cannot be used since it is currently used to plot grid data.
! The 'p' stands for 'prime'.


Module GPF_Data_X2Y2_p_Class

  use GPF_Parameters            ,only:  rkp
  use GPF_Data_Base_Class       ,only:  GPF_Data_Base_Type

  implicit none

  private
  public  ::  GPF_Data_X2Y2_p_Type
  public  ::  Construct_Data_X2Y2_p

  Type  ,extends(GPF_Data_Base_Type)                    ::  GPF_Data_X2Y2_p_Type
    real(rkp)   ,dimension(:,:)         ,allocatable    ::  X                                               !< X-Data
    real(rkp)   ,dimension(:,:)         ,allocatable    ::  Y                                               !< Y-Data
  contains
    private
    procedure   ,public   ::  Get_X_Min       =>  Get_X_Min_X2Y2          ! Gets the minimum value for X
    procedure   ,public   ::  Get_X_Max       =>  Get_X_Max_X2Y2          ! Gets the maximum value for X
    procedure   ,public   ::  Get_Y_Min       =>  Get_Y_Min_X2Y2          ! Gets the minimum value for Y
    procedure   ,public   ::  Get_Y_Max       =>  Get_Y_Max_X2Y2          ! Gets the maximum value for Y
    procedure   ,public   ::  Set_NColumns    =>  Set_NColumns_X2Y2       ! Sets the number of columns
    procedure   ,public   ::  Write           =>  Write_Data_X2Y2         ! Writes the data into a file
  End Type

  contains

Subroutine Construct_Data_X2Y2_p( This, X, Y )
  implicit none
  class(GPF_Data_Base_Type)     ,allocatable            ,intent(out)    ::  This                            !< Passed-object dummy argument
  real(rkp)     ,dimension(:,:)                         ,intent(in)     ::  X                               !< X-Data
  real(rkp)     ,dimension(:,:)                         ,intent(in)     ::  Y                               !< Y-Data
  allocate( GPF_Data_X2Y2_p_Type :: This )                                                                      ! Allocating the structure
  select type (This)
    type is (GPF_Data_X2Y2_p_Type)
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!       allocate( This%X(size(X,1),size(X,2)) )                                                                   ! Allocating and setting X-data
!       allocate( This%Y(size(Y,1),size(Y,2)) )                                                                   ! Allocating and setting Y-data
!       This%X  =  X
!       This%Y  =  Y
      allocate( This%X(size(X,1),size(X,2)), source=X )                                                                                  ! Allocating and setting X-data
      allocate( This%Y(size(Y,1),size(Y,2)), source=Y )                                                                                  ! Allocating and setting Y-data
#else
      allocate( This%X, source=X )                                                                                  ! Allocating and setting X-data
      allocate( This%Y, source=Y )                                                                                  ! Allocating and setting Y-data
#endif
  end select

!   here, X and Y should have the same number of points

  This%NLine    =       size(X,1)
  This%NPoint   =       size(X,2)
  This%NAbsci   =       size(X,1)
  This%NAxes    =       2

  This%X_Dim    =       2
  This%Y_Dim    =       2
  This%Z_Dim    =       0

End Subroutine

Subroutine Write_Data_X2Y2(  This, Unit )
  implicit none
  class(GPF_Data_X2Y2_p_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  integer                                                               ::  iPoint, iLine
  do iPoint = 1,This%Npoint
    write(Unit,This%Format) ( This%X(iLine,iPoint), This%Y(iLine,iPoint), iLine =1,This%NLine  )
  end do
End Subroutine

Function Get_X_Min_X2Y2( This ) result( X_Min )
  implicit none
  class(GPF_Data_X2Y2_p_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  X_Min                           !< Minimum value for X
  X_Min         =       minval(This%X)                                                                          ! Getting the minimum value for X
End Function

Function Get_X_Max_X2Y2( This ) result( X_Max )
  implicit none
  class(GPF_Data_X2Y2_p_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  X_Max                           !< Maximum value for X
  X_Max         =       maxval(This%X)                                                                          ! Getting the maximum value for X
End Function

Function Get_Y_Min_X2Y2( This ) result( Y_Min )
  implicit none
  class(GPF_Data_X2Y2_p_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Y_Min                           !< Minimum value for X
  Y_Min         =       minval(This%Y)                                                                          ! Getting the minimum value for X
End Function

Function Get_Y_Max_X2Y2( This ) result( Y_Max )
  implicit none
  class(GPF_Data_X2Y2_p_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Y_Max                           !< Maximum value for Y
  Y_Max         =       maxval(This%Y)                                                                          ! Getting the maximum value for Y
End Function

Subroutine Set_NColumns_X2Y2(  This )
  implicit none
  class(GPF_Data_X2Y2_p_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  This%NColumns =       This%NLine + This%Nabsci                                                                ! Setting the number of columns in the data file to the number of X-data plus the number of Y-data, ie to sum of the number of abscisse and lines
End Subroutine

End Module