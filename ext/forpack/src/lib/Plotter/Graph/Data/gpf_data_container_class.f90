Module GPF_Data_Container_Class

  use GPF_Data_Base_Class       ,only:  GPF_Data_Base_Type
  use GPF_Parameters            ,only:  rkp, DbgUnit

  implicit none

  private

  public  ::  GPF_Data_Container_Type


  Type                                                                  ::  GPF_DatVec_Type
    class(GPF_Data_Base_Type)   ,allocatable                            ::  Data
  End Type

  Type                                                                  ::  GPF_Data_Container_Type
    type(GPF_DatVec_Type)      ,dimension(:)   ,allocatable            ::  DatVec
  contains
    procedure             ::  Construct       =>  Construct_Data_Container
  End Type


  contains

Subroutine Construct_Data_Container( This, X_1D, X_2D, Y_1D, Y_2D, Z_1D, Z_2D, Debug )

  use GPF_Parameters            ,only:  KEY_plot, KEY_splot, DataType_X1Y1, DataType_X1Y2, DataType_X2Y2, DataType_X2Y2Z2, DataType_Valide
  use GPF_Data_Base_Class       ,only:  GPF_Data_Base_Type
  use GPF_Data_X1Y1_Class       ,only:  GPF_Data_X1Y1_Type,     Construct_Data_X1Y1
  use GPF_Data_X1Y2_Class       ,only:  GPF_Data_X1Y2_Type,     Construct_Data_X1Y2
  use GPF_Data_X2Y2_Class       ,only:  GPF_Data_X2Y2_Type,     Construct_Data_X2Y2
  use GPF_Data_X2Y2Z2_Class     ,only:  GPF_Data_X2Y2Z2_Type,   Construct_Data_X2Y2Z2

  implicit none

  class(GPF_Data_Container_Type)                        ,intent(out)    ::  This                            !< Data container object
  real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  X_1D                            !< X-Data for 1D variable
  real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  X_2D                            !< X-Data for 2D variable
  real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Y_1D                            !< Y-Data for 1D variable
  real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Y_2D                            !< Y-Data for 2D variable
  real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Z_1D                            !< Z-Data for 1D variable
  real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Z_2D                            !< Z-Data for 2D variable
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  logical                                                               ::  i_Debug_Loc     =       .False. ! Local debugging indicator
  integer                                                               ::  DataType

  i_Debug_Loc   =       .False.
  if ( present(Debug) ) i_Debug_Loc     =       Debug

  if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: Entering')")

  if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: Calling Select_DataType')")
  DataType      =       Select_DataType( X_1D, X_2D, Y_1D, Y_2D, Z_1D, Z_2D )
  if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: DataType = ',i0,' => ',a)") DataType, DataType_Valide(DataType)

  if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: Allocating to one')")
  allocate( This%DatVec(1) )

  associate( DatVec1 => This%DatVec(1) )


    if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: Calling the associated constructor')")
    select case (DataType)

      case(DataType_X1Y1)
! ! ! ! !         allocate( GPF_Data_X1Y1_Type :: This%Data(1) )
! ! ! ! !         select type (This)
! ! ! ! !           type is (GPF_Data_X1Y1_Type)
! ! ! ! !             allocate( This%X, source=X )                                                                                  ! Allocating and setting X-data
! ! ! ! !             allocate( This%Y, source=Y )                                                                                  ! Allocating and setting Y-data
! ! ! ! !         end select
! ! ! ! !
! ! ! ! !         This%NPoint   =       size(X,1)
! ! ! ! !         This%NLine    =       1
! ! ! ! !         This%NAbsci   =       1
! ! ! ! !         This%NAxes    =       2
! ! ! ! !
! ! ! ! !         This%X_Dim    =       1
! ! ! ! !         This%Y_Dim    =       1
! ! ! ! !         This%Z_Dim    =       0
        if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: Calling Construct_Data_Container_X1Y1')")
        call Construct_Data_Container_X1Y1( DatVec1, X_1D, Y_1D )

      case(DataType_X1Y2)
! ! ! !         allocate( GPF_Data_X1Y2_Type :: This%Data(1) )
        if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: Calling Construct_Data_Container_X1Y2')")
        call Construct_Data_Container_X1Y2( DatVec1, X_1D, Y_2D )
  !
      case(DataType_X2Y2)
! ! !         allocate( GPF_Data_X2Y2_Type :: This%Data(1) )
        if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: Calling Construct_Data_Container_X2Y2')")
        call Construct_Data_Container_X2Y2( DatVec1, X_2D, Y_2D )
  !
      case(DataType_X2Y2Z2)
! ! ! !         allocate( GPF_Data_X2Y2Z2_Type :: This%Data(1) )
        if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: Calling Construct_Data_Container_X2Y2Z2')")
        call Construct_Data_Container_X2Y2Z2( DatVec1, X_2D, Y_2D, Z_2D )

      case default
  !     ERROR
    end select

    if (i_Debug_Loc) then
      select type (Data => DatVec1%Data)
        type is (GPF_Data_Base_Type);     write(DbgUnit,"(10x,'[Construct_Data_Container]: The Data variable is of type: GPF_Data_Base_Type')")
        type is (GPF_Data_X1Y1_Type);     write(DbgUnit,"(10x,'[Construct_Data_Container]: The Data variable is of type: GPF_Data_X1Y1_Type')")
        type is (GPF_Data_X1Y2_Type);     write(DbgUnit,"(10x,'[Construct_Data_Container]: The Data variable is of type: GPF_Data_X1Y2_Type')")
        type is (GPF_Data_X2Y2_Type);     write(DbgUnit,"(10x,'[Construct_Data_Container]: The Data variable is of type: GPF_Data_X2Y2_Type')")
        type is (GPF_Data_X2Y2Z2_Type);   write(DbgUnit,"(10x,'[Construct_Data_Container]: The Data variable is of type: GPF_Data_X2Y2Z2_Type')")
        class default;                    write(DbgUnit,"(10x,'[Construct_Data_Container]: The Data variable is of type: default')")
      end select
    end if

  ! REMARK:
  ! The "plot" command can also be used with GPF_Data_X2Y2_Type data type.
  ! (For example, when one want ot plot 2 lines with different data for the X-axis)
  ! But, for the time being, the PlotType variable is defined according to the dimension
  !
!
!     DatVec1%PlotType         =       KEY_plot                                                                                ! Setting PlotType to default value
!     if ( DatVec1%Z_Dim == 0 ) DatVec1%PlotType   =       KEY_plot                                                  ! It no Z-axis, then setting the "plot' PlotType value
!     if ( DatVec1%Z_Dim == 2 ) DatVec1%PlotType   =       KEY_splot                                                 ! It Z-axis, then setting the "splot' PlotType value
!
!     if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: Calling DatVec1%Set_NColumns')")
!     call DatVec1%Set_NColumns()
!
!     if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: Calling DatVec1%Set_Format')")
!     call DatVec1%Set_Format()
!
!
!     if (i_Debug_Loc) then
!       write(DbgUnit,"(10x,'[Construct_Data_Container]: DatVec1%X_Dim    = ',i0)") DatVec1%X_Dim
!       write(DbgUnit,"(10x,'[Construct_Data_Container]: DatVec1%Y_Dim    = ',i0)") DatVec1%Y_Dim
!       write(DbgUnit,"(10x,'[Construct_Data_Container]: DatVec1%Z_Dim    = ',i0)") DatVec1%Z_Dim
!       write(DbgUnit,"(10x,'[Construct_Data_Container]: DatVec1%NAbsci   = ',i0)") DatVec1%NAbsci
!       write(DbgUnit,"(10x,'[Construct_Data_Container]: DatVec1%NLine    = ',i0)") DatVec1%NLine
!       write(DbgUnit,"(10x,'[Construct_Data_Container]: DatVec1%Npoint   = ',i0)") DatVec1%Npoint
!       write(DbgUnit,"(10x,'[Construct_Data_Container]: DatVec1%NColumns = ',i0)") DatVec1%NColumns
!       write(DbgUnit,"(10x,'[Construct_Data_Container]: DatVec1%PlotType = ',a )") DatVec1%PlotType
!     end if

  end associate

  if (i_Debug_Loc) write(DbgUnit,"(10x,'[Construct_Data_Container]: Exiting',/)")

End Subroutine

Function Select_DataType( X_1D, X_2D, Y_1D, Y_2D, Z_1D, Z_2D ) result(DataType)

  use GPF_Parameters            ,only:  DataType_X1Y1, DataType_X1Y2, DataType_X2Y2, DataType_X2Y2Z2, DataType_Valide

  implicit none

  integer                                                               ::  DataType                        !< Type of data
  real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  X_1D                            !< X-Data for 1D variable
  real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  X_2D                            !< X-Data for 2D variable
  real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Y_1D                            !< Y-Data for 1D variable
  real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Y_2D                            !< Y-Data for 2D variable
  real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Z_1D                            !< Z-Data for 1D variable
  real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Z_2D                            !< Z-Data for 2D variable

! ==============================================================================================================
!       CHECK CONSISTENCY
! ==============================================================================================================
  if ((.not.present(X_1D)) .and. (.not.present(X_2D))) stop '[Construct_Data_Container]: ERROR: no X data'
  if ((.not.present(Y_1D)) .and. (.not.present(Y_2D))) stop '[Construct_Data_Container]: ERROR: no Y data'
  if ( (present(X_1D)) .and. (present(X_2D)) ) stop '[Construct_Data_Container]: ERROR: no X data'
  if ( (present(Y_1D)) .and. (present(Y_2D)) ) stop '[Construct_Data_Container]: ERROR: no Y data'
  if ( (present(X_2D)) .and. (present(Y_1D)) ) stop '[Construct_Data_Container]: ERROR: no Y data'

  if ( present(Z_1D) ) then
  end if

! ==============================================================================================================


! ==============================================================================================================
!       X1Y1
! ==============================================================================================================
  if ( (present(X_1D)) .and. (present(Y_1D)) ) then
    if ( size(X_1D,1) /= size(Y_1D,1) ) write(DbgUnit,"(10x,'[Construct_Data_Container]: ERROR: X_1D and Y_1D have different number of points')")
    DataType    =       DataType_X1Y1
    return
  end if


! ==============================================================================================================
!       X1Y2
! ==============================================================================================================
  if ( (present(X_1D)) .and. (present(Y_2D)) ) then
    if ( size(X_1D,1) /= size(Y_2D,2) ) write(DbgUnit,"(10x,'[Construct_Data_Container]: ERROR: X_1D and Y_2D have different number of points')")
!     if ( size(Y_2D,1) == 1 ) then     ! Cannot use DataType_X1Y1 because Y_1D is absent as optional argument
!       DataType    =       DataType_X1Y1
!     else
      DataType    =       DataType_X1Y2
!     end if
    return
  end if

! ==============================================================================================================
!       X2Y2
! ==============================================================================================================
  if ( (present(X_2D)) .and. (present(Y_2D)) .and.(.not.present(Z_2D)) ) then

    if ( size(X_2D,2) /= size(Y_2D,2) ) write(DbgUnit,"(10x,'[Construct_Data_Container]: ERROR: X_1D and Y_2D have different number of points')")
    if ( size(X_2D,1) /= size(Y_2D,1) ) write(DbgUnit,"(10x,'[Construct_Data_Container]: ERROR: X_1D and Y_2D have different number of data')")
    DataType    =       DataType_X2Y2
    return
  end if


! ==============================================================================================================
!       X2Y2Z2
! ==============================================================================================================
  if ( (present(X_2D)) .and. (present(Y_2D)) .and. (present(Z_2D)) ) then
    DataType    =       DataType_X2Y2Z2
    return
  end if


End Function

End Module