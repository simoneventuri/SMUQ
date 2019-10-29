SubModule(GPF_Graph_Class) GPF_Graph_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

! @COMPILER_BUG:
! The following error occures "SIGSEGV, segmentation fault occurred" when trying to construct a array of Graph.
! The calling sequence is:
!       Graphs(1) = GPF_Graph_Type( ... )
! in order to avoid the segmentation fault , the following workaround has to be applied.
! 1) Define scalar object using:                       type(GPF_Graph_Type) ::  Graph_1, Graph_2, ..., Graph_N
! 2) Construct each Graph object individually using:    Graph_1 = GPF_Graph_Type( ... ); Graph_2 = GPF_Graph_Type( ... ); ...
! 3) Construct the Graph array using:                   allocate( Graphs, source = [ Graph_1, Graph_2, ..., Graph_N ] )

! REMARK:
! In order to avoid the above compiler bug, the Graph constructor procedure is turned into a subroutine.
! @TODO: In procedure "InitializeGraph", the optional input argument "NPoints" is not being used.
Module Procedure InitializeGraph

  use GPF_Parameters            ,only:  Axis_Type_Default
  use GPF_Data_Module           ,only:  Construct_Data
  use GPF_LineStyle_Class       ,only:  Construct_LineStyle

  character(*)                                              ,parameter  ::  ProcName = "InitializeGraph"
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

! ******************************************************************************************************************************
  if ( present(NPoints) ) then
    if (Dbg) call Logger%Write( "The NPoints optional input argument is present but is not being used" )
  end if
! ******************************************************************************************************************************

  if (Dbg) call Logger%Write( "Calling Construct_Data" )
  call Construct_Data( This%Data, X_1D, X_2D, X_2Dp, Y_1D, Y_2D, Z_1D, Z_2D, DataBlocks, Debug )

  if (Dbg) call Logger%Write( "Calling SetGraphIndex" )
  call SetGraphIndex( This, Indexx )

  if (Dbg) call Logger%Write( "Calling This%SetDataFileName" )
  call This%SetDataFileName( DataFile )

  if (Dbg) call Logger%Write( "Calling This%Size%Initialize" )
  call This%Size%Initialize( Size_Ratio, Size_X_Scale, Size_Y_Scale, Size_Scales, Size_Isometric, Debug )

  if (Dbg) call Logger%Write( "Calling This%Origin%Initialize" )
  call This%Origin%Initialize( Origin_X_Coord, Origin_Y_Coord, Origin_Coordinates, Debug )

  if (Dbg) call Logger%Write( "Calling This%Margin%Initialize" )
  call This%Margins%Initialize( Margin_Top, Margin_Bottom, Margin_Left, Margin_Right, Debug )

  if (Dbg) call Logger%Write( "Calling This%View%Initialize" )
  call This%View%Initialize( View_Rot_x, View_Rot_z, View_Scale, View_Scale_z, View_Map, View_Equal_xy, View_Equal_xyz, Debug )

  if (Dbg) call Logger%Write( "Calling This%Title%Initialize" )
  call This%Title%Initialize( Title, Title_Offset, Title_Font, Title_Font_Size, Title_Color, Title_Enhanced, Debug )

  if (Dbg) call Logger%Write( "Calling This%Key%Initialize" )
  call This%Key%Initialize( Key_Font, Key_Font_Size, Key_Position, Key_Space, Key_Setting, Debug )

  if (Dbg) call Logger%Write( "Calling This%Grid%Initialize" )
  call This%Grid%Initialize( Grid_Command, Debug )

  if (Dbg) call Logger%Write( "Calling This%Set_Arrow" )
  call SetGraphArrow( This, Arrow )

  if (Dbg) call Logger%Write( "Calling Construct_Axis" )
  This%Axes     =   GPF_Axes_Type( This%Data, Axis_Type_Default, null(),                                         &       ! Constructing the Axis object
                X_Min, X_Max, X_Reverse,                                                                        &       ! Arguments related to the X-Axis range
                X_LogScale, X_LogBase,                                                                          &       ! Arguments related to the X-Axis scale
                X_Label, X_Label_Offset, X_Label_Font, X_Label_Font_Size, X_Label_Color, X_Label_Enhanced,      &       ! Arguments related to the X-Axis label
                X_Format,                                                                                       &       ! Arguments related to the X-Axis format
                Y_Min, Y_Max, Y_Reverse,                                                                        &       ! Arguments related to the Y-Axis range
                Y_LogScale, Y_LogBase,                                                                          &       ! Arguments related to the Y-Axis scale
                Y_Label, Y_Label_Offset, Y_Label_Font, Y_Label_Font_Size, Y_Label_Color, Y_Label_Enhanced,      &       ! Arguments related to the Y-Axis label
                Y_Format,                                                                                       &       ! Arguments related to the Y-Axis format
                Z_Min, Z_Max, Z_Reverse,                                                                        &       ! Arguments related to the Z-Axis range
                Z_LogScale, Z_LogBase,                                                                          &       ! Arguments related to the Z-Axis scale
                Z_Label, Z_Label_Offset, Z_Label_Font, Z_Label_Font_Size, Z_Label_Color, Z_Label_Enhanced,      &       ! Arguments related to the Z-Axis label
                Z_Format                                                                                        )       ! Arguments related to the Z-Axis format

  if (Dbg) call Logger%Write( "Calling GPF_ColorBar_Type" )
  This%ColorBar =   GPF_ColorBar_Type( null(),                                                                   &       ! Setting ColorBar object
                CB_Values, CB_Min, CB_Max, CB_Reverse,                                                          &       ! Arguments related to the ColorBar-Axis range
                CB_LogScale, CB_LogBase,                                                                        &       ! Arguments related to the ColorBar-Axis scale
                CB_Label, CB_Label_Offset, CB_Label_Font, CB_Label_Font_Size, CB_Label_Color,CB_Label_Enhanced, &       ! Arguments related to the ColorBar-Axis label
                CB_Format,                                                                                      &       ! Arguments related to the ColorBar-Axis format
                CB_Orientation, CB_Origin, CB_Size, CB_Position, CB_Border, CB_UnSetting                        )       ! Arguments related to the ColorBar-Box

  if (Dbg) call Logger%Write( "Calling GPF_Pm3D_Type" )
  This%Pm3D     =   GPF_Pm3D_Type( This%Data%Get_PlotType(), Pm3D )                                         ! Constructing the Graph-Pm3D object

  if (Dbg) call Logger%Write( "Calling Construct_LineStyle" )
  call Construct_LineStyle(                                                                                     &       ! Constructing the LineStyle object
                This%LineStyle, This%Data%Get_NLine(), LS_Type, LS_Width, LS_Color,                             &
                LS_PointType, LS_PointSize, LS_PointInterval, null()                                             )

  if (Dbg) call Logger%Write( "Calling GPF_Plot_Type" )
  This%Plot    =   GPF_Plot_Type( This%Data%Get_NLine(), This%Data%Get_NAbsci(),            &               ! Constructing the Plot object
                                      This%DataFile, This%Data%Get_PlotType(),                  &               !
                                      This%LineStyle, CurveStyle, LineTitle, Every, FillStyle, CB_Values, null() )   !


! ! Commented in order to prevent error when plots with multiple x-coodinates.
! ! Currently, the way the SetDataHeader procedure is implement, it do not work
!   if (Dbg) call Logger%Write( "Calling This%SetDataHeader" )
!   call SetDataHeader(This)                                                                                   ! Setting the header for data column in the DataFile


  call This%Set_Command()

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetGraphCommand
  This%Command  =   '# Graph commands'
End Procedure

Module Procedure WriteGraphCommands
  integer                                                               ::  i
  if ( len_trim(This%Command) /= 0 ) write(Unit,"(/,a)") This%Command
  call This%Size%Write( Unit )
  call This%Origin%Write( Unit )
  call This%Margins%Write( Unit )
  call This%View%Write( Unit )
  call This%Title%Write( Unit )
  call This%Grid%Write( Unit )
  call This%Arrow%Write( Unit )
  call This%Key%Write(  Unit )
  call This%Axes%Write( Unit )
  call This%ColorBar%Write( Unit )
  call This%Pm3D%Write( Unit )
  do i = 1,size(This%LineStyle)
    call This%LineStyle(i)%Write( Unit )
  end do
  call This%Plot%Write( Unit )
End Procedure

Module Procedure WriteGraphData
  call This%Data%Write_Header( Unit )
  call This%Data%Write( Unit )
  call This%Data%Write_Header( Unit )
End Procedure

Module Procedure GetGraphDataFileName
  DataFile      =   This%DataFile
End Procedure

Module Procedure GetGraphIndex
  iGraph        =   This%Index
End Procedure



! ! REMARK:
! ! This procedure updates the name of the DataFile in both the associated object component This%DataFile and in
! ! the command string This%Command.
! ! This procedure is used only for MultiPlots. Whenever, MultiPlots are performed, this procedure is required
! ! because the Graph object (which is an array) is created before creating the File object.
! ! As a consequence, during the Graph object creation, the name of the DataFile is not yet known
! ! and the DataFile component thus corresponds to an empty string.
! ! Once all elements of the Graph object, the File object is then created. Since the name of the DataFile is
! ! specified during the File object creation, it is only known after the File object construction is performed.
! ! Then, the name of the DataFile is update in the Graph object by copying the value stored in the File object.
Module Procedure InsertGraphDataFile

  character(*)                                              ,parameter  ::  ProcName = "InsertGraphDataFile"
  logical                                                               ::  Dbg

  Dbg   =   .False.!GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling This%SetDataFileName" )
  call This%SetDataFileName( DataFile, Add_Index=.True. )

  if (Dbg) call Logger%Write( "Calling This%Plot%InsertGraphDataFile" )
  call This%Plot%Insert_DataFile( This%DataFile )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetGraphDataFile
  use GPF_Parameters            ,only:  Extension_DataFile
  use GPF_Tools                 ,only:  Convert_To_String
  character(*)                                              ,parameter  ::  ProcName = "SetGraphDataFile"
  logical                                                               ::  Dbg

  Dbg   =   .False.!GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  This%DataFile         =   ""
  if ( present(DataFile) ) then
    This%DataFile       =   trim(DataFile)
    if ( present(Add_Index) ) then
      if (Dbg) call Logger%Write( "Add_Index     = ", Add_Index     )
      if (Dbg) call Logger%Write( "This%Index    = ", This%Index    )
      if (Dbg) call Logger%Write( "This%DataFile = ", This%DataFile )
      if ( Add_Index ) then
        This%DataFile   =   trim(This%DataFile) // "_" // Convert_To_String(This%Index)
      end if
    end if
    This%DataFile       =   trim(This%DataFile) // '.' // Extension_DataFile
  end if
  if (Dbg) call Logger%Write( "This%DataFile = ", This%DataFile )

  if (Dbg) call Logger%Exiting()

End Procedure






! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine SetGraphIndex( This, Index_ )
  type(GPF_Graph_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Graph object
  integer                                     ,optional ,intent(in)     ::  Index_                          !< Graph Index
  This%Index    =   1                                                                                       ! Initializing the graph index to unity
  if ( present(Index_) ) This%Index = Index_                                                                    ! If present optional input argument, then setting the graph index to the input value
End Subroutine

Subroutine SetDataHeader( This )

  use GPF_Tools                 ,only:  Extract_Label

  type(GPF_Graph_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Graph object

  character(*)                                              ,parameter  ::  ProcName = "SetDataHeader"
  logical                                                               ::  Dbg
  integer                                                               ::  Length
  integer                                                               ::  iXaxis
  integer                                                               ::  iYaxis
  integer                                                               ::  iZaxis
  integer                                                               ::  iLine
  character(:)  ,dimension(:)   ,allocatable                            ::  X_Axis_Label
  character(:)  ,dimension(:)   ,allocatable                            ::  Y_Axis_Label
  character(:)  ,dimension(:)   ,allocatable                            ::  Z_Axis_Label
  character(:)  ,dimension(:)   ,allocatable                            ::  Lines_Title

  integer       ,parameter      ::  NYAxis=1
  integer       ,parameter      ::  NZAxis=1

  Dbg   =   .False. !GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

! ==============================================================================================================
!   SETTING THE X-AXIS LABELS
! ==============================================================================================================
  Length        =   0
  do iXaxis = 1,This%Data%NAbsci
    Length      =   max( Length, len_trim( Extract_Label(This%Axes%Axis(iXaxis)%Label%Text) ) )
  end do
  allocate( character(Length) :: X_Axis_Label(This%Data%NAbsci) )
  do iXaxis = 1,This%Data%NAbsci
    X_Axis_Label(iXaxis) = Extract_Label(This%Axes%Axis(iXaxis)%Label%Text)
  end do

  if (Dbg) then
    do iXaxis = 1,This%Data%NAbsci
      call Logger%Write( "iXaxis = ", iXaxis, "X_Axis_Label(iXaxis) = ", X_Axis_Label(iXaxis) )
    end do
  end if


! ==============================================================================================================
!   SETTING THE Y-AXIS LABELS
! ==============================================================================================================
! @TODO: Use the NYAxis variable which is not yet implemented
  Length        =   len_trim( Extract_Label(This%Axes%Axis(2)%Label%Text) )
  allocate( character(Length) :: Y_Axis_Label(NYAxis) )
  do iYaxis = 1,NYAxis
    Y_Axis_Label(iYaxis)        =   Extract_Label( This%Axes%Axis(2)%Label%Text )
  end do

  if (Dbg) then
    do iYaxis = 1,NYAxis
      call Logger%Write( "iYaxis = ", iYaxis, "Y_Axis_Label(iYaxis) = ", Y_Axis_Label(iYaxis) )
    end do
  end if

! ==============================================================================================================
!   SETTING THE Z-AXIS LABELS
! ==============================================================================================================
  if ( This%Axes%NAxes >= 3 ) then
    iZaxis        =   1
    Length        =   len_trim( Extract_Label(This%Axes%Axis(3)%Label%Text) )
    allocate( character(Length) :: Z_Axis_Label(NZAxis) )
    do iZaxis = 1,NZAxis
      Z_Axis_Label(iZaxis)        =   Extract_Label( This%Axes%Axis(3)%Label%Text )
    end do
    if (Dbg) then
      do iZaxis = 1,NZAxis
        call Logger%Write( "iZaxis = ", iZaxis, "Z_Axis_Label(iZaxis) = ", Z_Axis_Label(iZaxis) )
      end do
    end if
  else
    allocate( character(0) :: Z_Axis_Label(0) )
  end if

  if (Dbg) call Logger%Write( "Calling This%Plot%Get_Lines_Title')")
  call This%Plot%Get_Lines_Title( Lines_Title )

  if (Dbg) then
    do iLine = 1,size(Lines_Title)
      call Logger%Write( "iLine = ", iLine, "Lines_Title(iLine) = ", Lines_Title(iLine) )
    end do
  end if

  if (Dbg) call Logger%Write( "Calling This%Data%Set_Columns_Label')")
  call This%Data%Set_Columns_Label( X_Axis_Label, Y_Axis_Label, Z_Axis_Label, Lines_Title )

  if (Dbg) call Logger%Exiting()

End Subroutine

Subroutine SetGraphArrow( This, Arrow )
  type(GPF_Graph_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Graph object
  type(GPF_Arrow_Type)                        ,optional ,intent(in)     ::  Arrow                           !< Arrow object
  if ( present(Arrow) ) This%Arrow = Arrow                                                                      ! Setting the Arrow object if present
End Subroutine

End SubModule