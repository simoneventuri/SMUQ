SubModule(GPF_Data_Module) GPF_Data_SubModule

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure Construct_Data

  use GPF_Parameters            ,only:  KEY_plot, KEY_splot, DataType_UNKNOWN, DataType_X1Y1, DataType_X1Y2, DataType_X2Y2, DataType_X2Y2p, DataType_X2Y2Z2, DataType_DataBlocks, DataType_Valide
  use GPF_Data_X1Y1_Class       ,only:  GPF_Data_X1Y1_Type,     Construct_Data_X1Y1
  use GPF_Data_X1Y2_Class       ,only:  GPF_Data_X1Y2_Type,     Construct_Data_X1Y2
  use GPF_Data_X2Y2_Class       ,only:  GPF_Data_X2Y2_Type,     Construct_Data_X2Y2
  use GPF_Data_X2Y2_p_Class     ,only:  GPF_Data_X2Y2_p_Type,   Construct_Data_X2Y2_p
  use GPF_Data_X2Y2Z2_Class     ,only:  GPF_Data_X2Y2Z2_Type,   Construct_Data_X2Y2Z2
  use GPF_Data_DataBlocks_Class   ,only:  GPF_Data_DataBlocks_Type, Construct_Data_DataBlocks

  character(*)                                              ,parameter  ::  ProcName = "Construct_Data"
  logical                                                               ::  Dbg
  integer                                                               ::  DataType

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling Select_DataType" )
  DataType      =   Select_DataType( X_1D, X_2D, X_2Dp, Y_1D, Y_2D, Z_1D, Z_2D, DataBlocks )
  if (Dbg) call Logger%Write( "DataType = ", DataType )

  if (Dbg) call Logger%Write( "Calling the associated constructor" )
  select case (DataType)

    case(DataType_X1Y1)
      if (Dbg) call Logger%Write( "Calling Construct_Data_X1Y1" )
      call Construct_Data_X1Y1( This, X_1D, Y_1D )

    case(DataType_X1Y2)
      if (Dbg) call Logger%Write( "Calling Construct_Data_X1Y2" )
      call Construct_Data_X1Y2( This, X_1D, Y_2D )

    case(DataType_X2Y2)
      if (Dbg) call Logger%Write( "Calling Construct_Data_X2Y2" )
      call Construct_Data_X2Y2( This, X_2D, Y_2D )

    case(DataType_X2Y2p)
      if (Dbg) call Logger%Write( "Calling Construct_Data_X2Y2_p" )
      call Construct_Data_X2Y2_p( This, X_2Dp, Y_2D )

    case(DataType_X2Y2Z2)
      if (Dbg) call Logger%Write( "Calling Construct_Data_X2Y2Z2" )
      call Construct_Data_X2Y2Z2( This, X_2D, Y_2D, Z_2D )

    case(DataType_DataBlocks)
      if (Dbg) call Logger%Write( "Calling Construct_Data_DataBlocks" )
      call Construct_Data_DataBlocks( This, DataBlocks )

    case(DataType_UNKNOWN)
      if (Dbg) call Logger%Write( "Error: Unknown datatype" )

    case default
!     ERROR
  end select

  if (Dbg) then
    select type (This)
      type is (GPF_Data_X1Y1_Type);     call Logger%Write( "The Data variable is of type: GPF_Data_X1Y1_Type" )
      type is (GPF_Data_X1Y2_Type);     call Logger%Write( "The Data variable is of type: GPF_Data_X1Y2_Type" )
      type is (GPF_Data_X2Y2_Type);     call Logger%Write( "The Data variable is of type: GPF_Data_X2Y2_Type" )
      type is (GPF_Data_X2Y2_p_Type);   call Logger%Write( "The Data variable is of type: GPF_Data_X2Y2_p_Type" )
      type is (GPF_Data_X2Y2Z2_Type);   call Logger%Write( "The Data variable is of type: GPF_Data_X2Y2Z2_Type" )
      type is (GPF_Data_DataBlocks_Type); call Logger%Write( "The Data variable is of type: GPF_Data_DataBlocks_Type" )
      class is (GPF_Data_Base_Type);    call Logger%Write( "The Data variable is of type: GPF_Data_Base_Type" )
      class default;                    call Logger%Write( "The Data variable is of type: default" )
    end select
  end if

! REMARK:
! The "plot" command can also be used with GPF_Data_X2Y2_Type data type.
! (For example, when one want ot plot 2 lines with different data for the X-axis)
! But, for the time being, the PlotType variable is defined according to the dimension
!

  This%PlotType         =   KEY_plot                                                                                ! Setting PlotType to default value
  if ( This%Z_Dim == 0 ) This%PlotType   =   KEY_plot                                                  ! It no Z-axis, then setting the "plot' PlotType value
  if ( This%Z_Dim == 2 ) This%PlotType   =   KEY_splot                                                 ! It Z-axis, then setting the "splot' PlotType value

  if (Dbg) call Logger%Write( "Calling This%Set_NColumns" )
  call This%Set_NColumns()

  if (Dbg) call Logger%Write( "Calling This%Set_Format" )
  call This%Set_Format()

  if (Dbg) then
    call Logger%Write( "This%X_Dim    = ", This%X_Dim    )
    call Logger%Write( "This%Y_Dim    = ", This%Y_Dim    )
    call Logger%Write( "This%Z_Dim    = ", This%Z_Dim    )
    call Logger%Write( "This%NAbsci   = ", This%NAbsci   )
    call Logger%Write( "This%NLine    = ", This%NLine    )
    call Logger%Write( "This%Npoint   = ", This%Npoint   )
    call Logger%Write( "This%NColumns = ", This%NColumns )
    call Logger%Write( "This%PlotType = ", This%PlotType )
  end if

  if (Dbg) call Logger%Exiting()

End Procedure

Function Select_DataType( X_1D, X_2D, X_2Dp, Y_1D, Y_2D, Z_1D, Z_2D, DataBlocks ) result(DataType)

  use GPF_Parameters            ,only:  DataType_UNKNOWN, DataType_X1Y1, DataType_X1Y2, DataType_X2Y2, DataType_X2Y2p, DataType_X2Y2Z2, DataType_DataBlocks, DataType_Valide

  implicit none

  integer                                                               ::  DataType                        !< Type of data
  real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  X_1D                            !< X-Data for 1D variable
  real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  X_2D                            !< X-Data for 2D variable
  real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  X_2Dp
  real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Y_1D                            !< Y-Data for 1D variable
  real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Y_2D                            !< Y-Data for 2D variable
  real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Z_1D                            !< Z-Data for 1D variable
  real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Z_2D                            !< Z-Data for 2D variable
  type(GPF_DataBlocks_Type)                     ,optional ,intent(in)     ::  DataBlocks

  DataType  =   DataType_UNKNOWN

! ==============================================================================================================
!       CHECK CONSISTENCY
! ==============================================================================================================
! These checking have been temporarily turned off since they do not work when X_2Dp is specified.
! ==============================================================================================================
!   if ((.not.present(X_1D)) .and. (.not.present(X_2D))) stop '[Construct_Data]: ERROR: no X data'
!   if ((.not.present(Y_1D)) .and. (.not.present(Y_2D))) stop '[Construct_Data]: ERROR: no Y data'
!   if ( (present(X_1D)) .and. (present(X_2D)) ) stop '[Construct_Data]: ERROR: no X data'
!   if ( (present(Y_1D)) .and. (present(Y_2D)) ) stop '[Construct_Data]: ERROR: no Y data'
!   if ( (present(X_2D)) .and. (present(Y_1D)) ) stop '[Construct_Data]: ERROR: no Y data'
! ==============================================================================================================



  if ( present(Z_1D) ) then
  end if

! ==============================================================================================================


! ==============================================================================================================
!       X1Y1
! ==============================================================================================================
  if ( (present(X_1D)) .and. (present(Y_1D)) ) then
    if ( size(X_1D,1) /= size(Y_1D,1) ) call Logger%Write( "ERROR: X_1D and Y_1D have different number of points" )
    DataType    =   DataType_X1Y1
    return
  end if


! ==============================================================================================================
!       X1Y2
! ==============================================================================================================
  if ( (present(X_1D)) .and. (present(Y_2D)) ) then
    if ( size(X_1D,1) /= size(Y_2D,2) ) call Logger%Write( "ERROR: X_1D and Y_2D have different number of points" )
!     if ( size(Y_2D,1) == 1 ) then     ! Cannot use DataType_X1Y1 because Y_1D is absent as optional argument
!       DataType    =   DataType_X1Y1
!     else
      DataType    =   DataType_X1Y2
!     end if
    return
  end if

! ==============================================================================================================
!       X2Y2
! ==============================================================================================================
  if ( (present(X_2D)) .and. (present(Y_2D)) .and.(.not.present(Z_2D)) ) then

    if ( size(X_2D,2) /= size(Y_2D,2) ) call Logger%Write( "ERROR: X_1D and Y_2D have different number of points" )
    if ( size(X_2D,1) /= size(Y_2D,1) ) call Logger%Write( "ERROR: X_1D and Y_2D have different number of data" )
    DataType    =   DataType_X2Y2
    return
  end if

! ==============================================================================================================
!       X2Y2_p
! ==============================================================================================================
  if ( (present(X_2Dp)) .and. (present(Y_2D)) .and.(.not.present(Z_2D)) ) then

    if ( size(X_2Dp,2) /= size(Y_2D,2) ) call Logger%Write( "ERROR: X_1D and Y_2D have different number of points" )
    if ( size(X_2Dp,1) /= size(Y_2D,1) ) call Logger%Write( "ERROR: X_1D and Y_2D have different number of data" )
    DataType    =   DataType_X2Y2p
    return
  end if

! ==============================================================================================================
!       X2Y2Z2
! ==============================================================================================================
  if ( (present(X_2D)) .and. (present(Y_2D)) .and. (present(Z_2D)) ) then
    DataType    =   DataType_X2Y2Z2
    return
  end if


  if ( (present(DataBlocks)) ) then
    DataType    =   DataType_DataBlocks
    return
  end if



End Function

End SubModule