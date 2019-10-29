Module GPF_Data_X1Y1_Class

  use GPF_Parameters            ,only:  rkp
  use GPF_Data_Base_Class       ,only:  GPF_Data_Base_Type

  implicit none

  private
  public  ::  GPF_Data_X1Y1_Type
  public  ::  Construct_Data_X1Y1

  Type  ,extends(GPF_Data_Base_Type)                    ::  GPF_Data_X1Y1_Type
    real(rkp)   ,dimension(:)   ,allocatable            ::  X                                               !< X-Data
    real(rkp)   ,dimension(:)   ,allocatable            ::  Y                                               !< Y-Data
  contains
    private
    procedure   ,public   ::  Get_X_Min       =>  Get_X_Min_X1Y1          ! Gets the minimum value for X
    procedure   ,public   ::  Get_X_Max       =>  Get_X_Max_X1Y1          ! Gets the maximum value for X
    procedure   ,public   ::  Get_Y_Min       =>  Get_Y_Min_X1Y1          ! Gets the minimum value for Y
    procedure   ,public   ::  Get_Y_Max       =>  Get_Y_Max_X1Y1          ! Gets the maximum value for Y
    procedure   ,public   ::  Set_NColumns    =>  Set_NColumns_X1Y1       ! Sets the number of columns
    procedure   ,public   ::  Write           =>  Write_Data_X1Y1         ! Writes the data into a file
  End Type

  contains

Subroutine Construct_Data_X1Y1( This, X, Y )
  implicit none
  class(GPF_Data_Base_Type)     ,allocatable            ,intent(out)    ::  This                            !< Passed-object dummy argument
  real(rkp)     ,dimension( :       )                   ,intent(in)     ::  X                               !< X-Data
  real(rkp)     ,dimension( :       )                   ,intent(in)     ::  Y                               !< Y-Data

  allocate( GPF_Data_X1Y1_Type :: This )                                                                        ! Allocating the structure
  select type (This)
    type is (GPF_Data_X1Y1_Type)
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!       allocate( This%X(size(X)) )                                                                                  ! Allocating and setting X-data
!       allocate( This%Y(size(Y)) )                                                                                  ! Allocating and setting Y-data
!       This%X  =  X
!       This%Y  =  Y
      allocate( This%X(size(X)), source=X )                                                                                  ! Allocating and setting X-data
      allocate( This%Y(size(Y)), source=Y )                                                                                  ! Allocating and setting Y-data
#else
      allocate( This%X, source=X )                                                                                  ! Allocating and setting X-data
      allocate( This%Y, source=Y )                                                                                  ! Allocating and setting Y-data
#endif
  end select

  This%NPoint   =       size(X,1)
  This%NLine    =       1
  This%NAbsci   =       1
  This%NAxes    =       2

  This%X_Dim    =       1
  This%Y_Dim    =       1
  This%Z_Dim    =       0
!   This%PlotType

! !   This%NLine   =       0
! !   This%NAbsci  =       0
! !   This%X_Dim   =       0
! !   This%Y_Dim   =       0
! !   This%Z_Dim   =       0

End Subroutine

Subroutine Write_Data_X1Y1(  This, Unit )
  implicit none
  class(GPF_Data_X1Y1_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  integer                                                               ::  iPoint
  do iPoint = 1,This%Npoint
    write(Unit,This%Format) This%X(iPoint), This%Y(iPoint)
  end do
End Subroutine


Function Get_X_Min_X1Y1( This ) result( X_Min )
  implicit none
  class(GPF_Data_X1Y1_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  X_Min                           !< Minimum value for X
  X_Min         =       minval(This%X)                                                                          ! Getting the minimum value for X
End Function

Function Get_X_Max_X1Y1( This ) result( X_Max )
  implicit none
  class(GPF_Data_X1Y1_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  X_Max                           !< Maximum value for X
  X_Max         =       maxval(This%X)                                                                          ! Getting the maximum value for X
End Function

Function Get_Y_Min_X1Y1( This ) result( Y_Min )
  implicit none
  class(GPF_Data_X1Y1_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Y_Min                           !< Minimum value for Y
  Y_Min         =       minval(This%Y)                                                                          ! Getting the minimum value for Y
End Function

Function Get_Y_Max_X1Y1( This ) result( Y_Max )
  implicit none
  class(GPF_Data_X1Y1_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Y_Max                           !< Maximum value for Y
  Y_Max         =       maxval(This%Y)                                                                          ! Getting the maximum value for Y
End Function

Subroutine Set_NColumns_X1Y1(  This )
  implicit none
  class(GPF_Data_X1Y1_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument
  This%NColumns =       This%NLine + This%Nabsci      ! Corresponds to 2

End Subroutine

End Module