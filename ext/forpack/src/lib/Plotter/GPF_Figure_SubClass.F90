SubModule(GPF_Figure_Class) GPF_Figure_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure splot

  character(*)                                              ,parameter  ::  ProcName = "splot"
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling SetFigureFile" )
  call SetFigureFile( This, Debug,                                                            &                       ! Setting the File object
                Name, Directory, HardCopy,                                              &                       ! Arguments related to the Output object
                Terminal, FontType, FontSize, Enhanced, Color, Term_Size                )                       ! Arguments related to the Terminal object

  if (Dbg) call Logger%Write( "Calling This%Set_Graph" )
  call This%Set_Graph( Debug,                                                                   &               ! Setting the Graph objects
                Graph, Graphs,                                                                  &               ! Arguments related to the Graph object
                Size_Ratio, Size_X_Scale, Size_Y_Scale, Size_Scales, Size_Isometric,            &               ! Arguments related to the Size object
                Origin_X_Coord, Origin_Y_Coord, Origin_Coordinates,                             &               ! Arguments related to the Origin object
                Margin_Top, Margin_Bottom, Margin_Left, Margin_Right,                           &               ! Arguments related to the Marging object
                View_Rot_x, View_Rot_z, View_Scale, View_Scale_z, View_Map, View_Equal_xy, View_Equal_xyz, &    ! Arguments related to the View object
                Title, Title_Offset, Title_Font, Title_Font_Size, Title_Color, Title_Enhanced,  &               ! Arguments related to the Title object
                Key_Font, Key_Font_Size, Key_Space, Key_Position, Key_Setting,                  &               ! Arguments related to the Key object
                Grid_Command,                                                                   &               ! Arguments related to the Grid object
!               Arguments related to the X-Axis object
                X_Min, X_Max, X_Reverse,                                                                        &       ! Arguments related to the X-Axis range
                X_LogScale, X_LogBase,                                                                          &       ! Arguments related to the X-Axis scale
                X_Label, X_Label_Offset, X_Label_Font, X_Label_Font_Size, X_Label_Color, X_Label_Enhanced,      &       ! Arguments related to the X-Axis label
                X_Format,                                                                                       &       ! Arguments related to the X-Axis format
!               Arguments related to the Y-Axis object
                Y_Min, Y_Max, Y_Reverse,                                                                        &       ! Arguments related to the Y-Axis range
                Y_LogScale, Y_LogBase,                                                                          &       ! Arguments related to the Y-Axis scale
                Y_Label, Y_Label_Offset, Y_Label_Font, Y_Label_Font_Size, Y_Label_Color, Y_Label_Enhanced,      &       ! Arguments related to the Y-Axis label
                Y_Format,                                                                                       &       ! Arguments related to the Y-Axis format
!               Arguments related to the Z-Axis object
                Z_Min, Z_Max, Z_Reverse,                                                                        &       ! Arguments related to the Z-Axis range
                Z_LogScale, Z_LogBase,                                                                          &       ! Arguments related to the Z-Axis scale
                Z_Label, Z_Label_Offset, Z_Label_Font, Z_Label_Font_Size, Z_Label_Color, Z_Label_Enhanced,      &       ! Arguments related to the Z-Axis label
                Z_Format,                                                                                       &       ! Arguments related to the Z-Axis format
!               Arguments related to the ColorBar-Axis
                CB_Values, CB_Min, CB_Max, CB_Reverse,                                                          &       ! Arguments related to the ColorBar-Axis range
                CB_LogScale, CB_LogBase,                                                                        &       ! Arguments related to the ColorBar-Axis scale
                CB_Label, CB_Label_Offset, CB_Label_Font, CB_Label_Font_Size, CB_Label_Color,CB_Label_Enhanced, &       ! Arguments related to the ColorBar-Axis label
                CB_Format,                                                                                      &       ! Arguments related to the ColorBar-Axis format
                CB_Orientation, CB_Origin, CB_Size, CB_Position, CB_Border, CB_UnSetting,                       &       ! Arguments related to the ColorBar-Box object
                LS_Type, LS_Width, LS_Color, LS_PointType, LS_PointSize, LS_PointInterval,                      &       ! Arguments related to the LineStyle object
!
                Indexx, Pm3D,                                   &                       ! Arguments related to Graph Info
                X_1D, X_2D, X_2Dp, Y_1D, Y_2D, Z_1D, Z_2D, NPoints, DataBlocks,   &                       ! Arguments related to Graph Data
                CurveStyle, LineTitle, Every, FillStyle,            &
!
                Arrow                                                                           )               ! Arguments related to the Arrow object

  if (Dbg) call Logger%Write( "Calling SetFigureMultiplot" )
  call SetFigureMultiplot( This, Debug,                                                       &                       ! Setting the Multiplot object
                Multiplot_Title, Multiplot_Title_Font_Name, Multiplot_Title_Font_Size, Multiplot_Title_Enhanced,        &                       ! Arguments related to the Multiplot title
                Multiplot_NRows, Multiplot_NCols,                                       &                       ! Arguments related to the Multiplot layout dimensions
                Multiplot_Xscale, Multiplot_Yscale,                                     &                       ! Arguments related to the Multiplot layout scales
                Multiplot_Xoffset, Multiplot_Yoffset                                    )                       ! Arguments related to the Multiplot layout offsets


  if (Dbg) call Logger%Write( "Calling WriteCommandFile" )
  call WriteCommandFile( This, Debug=Debug )

  if (Dbg) call Logger%Write( "Calling WriteDataFile" )
  call WriteDataFile( This, Debug=Debug )

  if ( GetOptArgValue(.False.,Generate) ) then
    if (Dbg) call Logger%Write( "Calling CreateFigure" )
    call CreateFigure( This, Debug )
  end if

  return

  if (Dbg) call Logger%Write( "Calling PostGeneration" )
  call PostGeneration( This, Debug )

  if (Dbg) call Logger%Write( "Calling This%Finalize" )
  call This%Finalize()                                                                                          ! Finalizing the figure object (Required in order to deallocate the Graph object)

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure Plot

  character(*)                                              ,parameter  ::  ProcName = "Plot"
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling SetFigureFile" )
  call SetFigureFile( This, Debug,                                                            &                       ! Setting the File object
                Name, Directory, HardCopy,                                              &                       ! Arguments related to the Output object
                Terminal, FontType, FontSize, Enhanced, Color, Term_Size                )                       ! Arguments related to the Terminal object

  if (Dbg) call Logger%Write( "Calling This%Set_Graph" )
  call This%Set_Graph( Debug,                                                                   &               ! Setting the Graph objects
                Graph, Graphs,                                                                  &               ! Arguments related to the Graph object
                Size_Ratio, Size_X_Scale, Size_Y_Scale, Size_Scales, Size_Isometric,            &               ! Arguments related to the Size object
                Origin_X_Coord, Origin_Y_Coord, Origin_Coordinates,                             &               ! Arguments related to the Origin object
                Margin_Top, Margin_Bottom, Margin_Left, Margin_Right,                           &               ! Arguments related to the Marging object
                View_Rot_x, View_Rot_z, View_Scale, View_Scale_z, View_Map, View_Equal_xy, View_Equal_xyz, &    ! Arguments related to the View object
                Title, Title_Offset, Title_Font, Title_Font_Size, Title_Color, Title_Enhanced,  &               ! Arguments related to the Title object
                Key_Font, Key_Font_Size, Key_Space, Key_Position, Key_Setting,                  &               ! Arguments related to the Key object
                Grid_Command,                                                                   &               ! Arguments related to the Grid object
!               Arguments related to the X-Axis object
                X_Min, X_Max, X_Reverse,                                                                        &       ! Arguments related to the X-Axis range
                X_LogScale, X_LogBase,                                                                          &       ! Arguments related to the X-Axis scale
                X_Label, X_Label_Offset, X_Label_Font, X_Label_Font_Size, X_Label_Color, X_Label_Enhanced,      &       ! Arguments related to the X-Axis label
                X_Format,                                                                                       &       ! Arguments related to the X-Axis format
!               Arguments related to the Y-Axis object
                Y_Min, Y_Max, Y_Reverse,                                                                        &       ! Arguments related to the Y-Axis range
                Y_LogScale, Y_LogBase,                                                                          &       ! Arguments related to the Y-Axis scale
                Y_Label, Y_Label_Offset, Y_Label_Font, Y_Label_Font_Size, Y_Label_Color, Y_Label_Enhanced,      &       ! Arguments related to the Y-Axis label
                Y_Format,                                                                                       &       ! Arguments related to the Y-Axis format
!               Arguments related to the Z-Axis object
                Z_Min, Z_Max, Z_Reverse,                                                                        &       ! Arguments related to the Z-Axis range
                Z_LogScale, Z_LogBase,                                                                          &       ! Arguments related to the Z-Axis scale
                Z_Label, Z_Label_Offset, Z_Label_Font, Z_Label_Font_Size, Z_Label_Color, Z_Label_Enhanced,      &       ! Arguments related to the Z-Axis label
                Z_Format,                                                                                       &       ! Arguments related to the Z-Axis format
!               Arguments related to the ColorBar-Axis
                CB_Values, CB_Min, CB_Max, CB_Reverse,                                                          &       ! Arguments related to the ColorBar-Axis range
                CB_LogScale, CB_LogBase,                                                                        &       ! Arguments related to the ColorBar-Axis scale
                CB_Label, CB_Label_Offset, CB_Label_Font, CB_Label_Font_Size, CB_Label_Color,CB_Label_Enhanced, &       ! Arguments related to the ColorBar-Axis label
                CB_Format,                                                                                      &       ! Arguments related to the ColorBar-Axis format
                CB_Orientation, CB_Origin, CB_Size, CB_Position, CB_Border, CB_UnSetting,                       &       ! Arguments related to the ColorBar-Box object
                LS_Type, LS_Width, LS_Color, LS_PointType, LS_PointSize, LS_PointInterval,                      &       ! Arguments related to the LineStyle object
!
                Indexx, Pm3D,                                   &                       ! Arguments related to Graph Info
                X_1D, X_2D, X_2Dp, Y_1D, Y_2D, Z_1D, Z_2D, NPoints, null(),    &                       ! Arguments related to Graph Data
                CurveStyle, LineTitle, Every, FillStyle,            &
!
                Arrow                                                                           )               ! Arguments related to the Arrow object

  if (Dbg) call Logger%Write( "Calling SetFigureMultiplot" )
  call SetFigureMultiplot( This, Debug,                                                       &                       ! Setting the Multiplot object
                Multiplot_Title, Multiplot_Title_Font_Name, Multiplot_Title_Font_Size, Multiplot_Title_Enhanced,        &                       ! Arguments related to the Multiplot title
                Multiplot_NRows, Multiplot_NCols,                                       &                       ! Arguments related to the Multiplot layout dimensions
                Multiplot_Xscale, Multiplot_Yscale,                                     &                       ! Arguments related to the Multiplot layout scales
                Multiplot_Xoffset, Multiplot_Yoffset                                    )                       ! Arguments related to the Multiplot layout offsets

  if (Dbg) call Logger%Write( "Calling WriteCommandFile" )
  call WriteCommandFile( This, Debug=Debug )

  if (Dbg) call Logger%Write( "Calling WriteDataFile" )
  call WriteDataFile( This, Debug=Debug )

  if ( GetOptArgValue(.False.,Generate) ) then
    if (Dbg) call Logger%Write( "Calling CreateFigure" )
    call CreateFigure( This, Debug )
  end if

  if (Dbg) call Logger%Write( "Calling PostGeneration" )
  call PostGeneration( This, Debug )

  if (Dbg) call Logger%Write( "Calling This%Finalize" )
  call This%Finalize()

  if (Dbg) call Logger%Exiting()

End Procedure




Subroutine SetFigureFile( This, Debug,                                    &
                Name, Directory, HardCopy,                                &                       ! Arguments related to the Output object
                Terminal, FontType, FontSize, Enhanced, Color, Term_Size  )                       ! Arguments related to the Terminal object


  type(GPF_Figure_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Figure object
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
  character(*)                                ,optional ,intent(in)     ::  Name                            !< File Name
  character(*)                                ,optional ,intent(in)     ::  Directory                       !< Directory Name
  logical                                     ,optional ,intent(in)     ::  HardCopy                        !< Hardcopy indicator
  character(*)                                ,optional ,intent(in)     ::  Terminal                        !< Terminal
  character(*)                                ,optional ,intent(in)     ::  FontType                        !< File FontType
  character(*)                                ,optional ,intent(in)     ::  FontSize                        !< File FontSize
  logical                                     ,optional ,intent(in)     ::  Enhanced                        !< File Enhancement indicator
  logical                                     ,optional ,intent(in)     ::  Color                           !< File color indicator
  character(*)                                ,optional ,intent(in)     ::  Term_Size                       !< Terminal size

  character(*)                                              ,parameter  ::  ProcName = "SetFigureFile"
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling This%File%Initialize" )
  call This%File%Initialize(  Debug,                                      &
                Name, Directory, HardCopy,                                &
                Terminal, FontType, FontSize, Enhanced, Color, Term_Size  )

  if (Dbg) call Logger%Exiting()

End Subroutine

Subroutine SetFigureMultiplot( This, Debug,                                           &
                Multiplot_Title, Multiplot_Title_Font_Name, Multiplot_Title_Font_Size, Multiplot_Title_Enhanced,        &                       ! Arguments related to the Multiplot title
                Multiplot_NRows, Multiplot_NCols,                                       &                       ! Arguments related to the Multiplot layout dimensions
                Multiplot_Xscale, Multiplot_Yscale,                                     &                       ! Arguments related to the Multiplot layout scales
                Multiplot_Xoffset, Multiplot_Yoffset                                    )

  use GPF_Parameters            ,only:  i_Debug_Default

  type(GPF_Figure_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Figure object
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
  character(*)                                ,optional ,intent(in)     ::  Multiplot_Title                 !< Title of the multiplot graph
  character(*)                                ,optional ,intent(in)     ::  Multiplot_Title_Font_Name       !< Font name for the multiplot title
  character(*)                                ,optional ,intent(in)     ::  Multiplot_Title_Font_Size       !< Font size for the multiplot title
  logical                                     ,optional ,intent(in)     ::  Multiplot_Title_Enhanced        !< Enhanced indicator for the multiplot graph
  integer                                     ,optional ,intent(in)     ::  Multiplot_NRows                 !< Number of rows for the muliplot layout
  integer                                     ,optional ,intent(in)     ::  Multiplot_NCols                 !< Number of colums for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Xscale                !< Scale along X for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Yscale                !< Scale along Y for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Xoffset               !< Offset along X for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Yoffset               !< Offset along Y for the muliplot layout



  character(*)                                              ,parameter  ::  ProcName = "Set_Figure_Multiplot"
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling GPF_Multiplot_Type" )
  This%Multiplot        =                                                           &                       ! Constructing Muliplot object
    GPF_Multiplot_Type( Debug,                                                          &
                Multiplot_Title, Multiplot_Title_Font_Name, Multiplot_Title_Font_Size, Multiplot_Title_Enhanced,        &                       ! Arguments related to the Multiplot title
                Multiplot_NRows, Multiplot_NCols,                                       &                       ! Arguments related to the Multiplot layout dimensions
                Multiplot_Xscale, Multiplot_Yscale,                                     &                       ! Arguments related to the Multiplot layout scales
                Multiplot_Xoffset, Multiplot_Yoffset,                                   &                       ! Arguments related to the Multiplot layout offsets
                This%Graph )

  if (Dbg) call Logger%Exiting()

End Subroutine

Module Procedure SetFigureGraph

  use GPF_Parameters            ,only:  i_Debug_Default

  character(*)                                              ,parameter  ::  ProcName = "SetFigureGraph"
  logical                                                               ::  Dbg
  type(GPF_Graph_Type)                                                  ::  Graph_Loc                           !< Graph object
  type(GPF_Graph_Type)  ,dimension(:)   ,allocatable                    ::  Graphs_Loc
  character(:)  ,allocatable                                            ::  DataFileName
  integer                                                               ::  iGraph                          ! Index of graph

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  DataFileName    =   This%File%GetDataFileName()
  if (Dbg) call Logger%Write( "(From the File object) DataFileName = ", DataFileName )

  if ( present(Graphs) ) then

    if (Dbg) call Logger%Write( "Allocating the local Graph objects" )
                                                                      ! If the Graph structure is not yet allocated
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     allocate( Graphs_Loc(size(Graphs)) )                                                                     ! Allocating the Graph structure and setting it value to the input Graph structure
!     Graphs_Loc = Graphs                                                                      ! Allocating the Graph structure and setting it value to the input Graph structure
    allocate( Graphs_Loc(size(Graphs)), source = Graphs )                                                                     ! Allocating the Graph structure and setting it value to the input Graph structure
#else
    allocate( Graphs_Loc, source = Graphs )                                                                     ! Allocating the Graph structure and setting it value to the input Graph structure
#endif

    do iGraph = 1,size(Graphs_Loc)
      if (Dbg) call Logger%Write( "iGraph = ", iGraph, "Calling Insert_DataFile" )
      call Graphs_Loc(iGraph)%Insert_DataFile( DataFileName )
    end do

    if (Dbg) call Logger%Write( "Calling This%AddGraph" )
    call This%AddGraph( Graphs_Loc, Debug=Debug )

  else if ( present(Graph) ) then

    if (Dbg) call Logger%Write( "Copying the input Graph object into a local variable" )
    Graph_Loc   =   Graph

    if (Dbg) call Logger%Write( "Calling Graph_Loc%Insert_DataFile" )
    call Graph_Loc%Insert_DataFile( DataFileName )

    if (Dbg) call Logger%Write( "Calling This%AddGraph" )
    call This%AddGraph( Graph_Loc, Debug=Debug )

  else

    if (Dbg) call Logger%Write( "Calling Graph_Loc%Initialize" )
    call Graph_Loc%Initialize( Debug, DataFileName,                                                  &
                Size_Ratio, Size_X_Scale, Size_Y_Scale, Size_Scales, Size_Isometric,            &               ! Arguments related to the Size object
                Origin_X_Coord, Origin_Y_Coord, Origin_Coordinates,                             &               ! Arguments related to the Origin object
                Margin_Top, Margin_Bottom, Margin_Left, Margin_Right,                           &               ! Arguments related to the Marging object
                View_Rot_x, View_Rot_z, View_Scale, View_Scale_z, View_Map, View_Equal_xy, View_Equal_xyz, &    ! Arguments related to the View object
                Title, Title_Offset, Title_Font, Title_Font_Size, Title_Color, Title_Enhanced,  &               ! Arguments related to the Title object
                Key_Font, Key_Font_Size, Key_Space, Key_Position, Key_Setting,                  &               ! Arguments related to the Key object
                Grid_Command,                                                                   &               ! Arguments related to the Grid object
!               Arguments related to the X-Axis object
                X_Min, X_Max, X_Reverse,                                                                        &       ! Arguments related to the X-Axis range
                X_LogScale, X_LogBase,                                                                          &       ! Arguments related to the X-Axis scale
                X_Label, X_Label_Offset, X_Label_Font, X_Label_Font_Size, X_Label_Color, X_Label_Enhanced,      &       ! Arguments related to the X-Axis label
                X_Format,                                                                                       &       ! Arguments related to the X-Axis format
!               Arguments related to the Y-Axis object
                Y_Min, Y_Max, Y_Reverse,                                                                        &       ! Arguments related to the Y-Axis range
                Y_LogScale, Y_LogBase,                                                                          &       ! Arguments related to the Y-Axis scale
                Y_Label, Y_Label_Offset, Y_Label_Font, Y_Label_Font_Size, Y_Label_Color, Y_Label_Enhanced,      &       ! Arguments related to the Y-Axis label
                Y_Format,                                                                                       &       ! Arguments related to the Y-Axis format
!               Arguments related to the Z-Axis object
                Z_Min, Z_Max, Z_Reverse,                                                                        &       ! Arguments related to the Z-Axis range
                Z_LogScale, Z_LogBase,                                                                          &       ! Arguments related to the Z-Axis scale
                Z_Label, Z_Label_Offset, Z_Label_Font, Z_Label_Font_Size, Z_Label_Color, Z_Label_Enhanced,      &       ! Arguments related to the Z-Axis label
                Z_Format,                                                                                       &       ! Arguments related to the Z-Axis format
!               Arguments related to the ColorBar-Axis
                CB_Values, CB_Min, CB_Max, CB_Reverse,                                                          &       ! Arguments related to the ColorBar-Axis range
                CB_LogScale, CB_LogBase,                                                                        &       ! Arguments related to the ColorBar-Axis scale
                CB_Label, CB_Label_Offset, CB_Label_Font, CB_Label_Font_Size, CB_Label_Color,CB_Label_Enhanced, &       ! Arguments related to the ColorBar-Axis label
                CB_Format,                                                                                      &       ! Arguments related to the ColorBar-Axis format
                CB_Orientation, CB_Origin, CB_Size, CB_Position, CB_Border, CB_UnSetting,                       &       ! Arguments related to the ColorBar-Box object
                LS_Type, LS_Width, LS_Color, LS_PointType, LS_PointSize, LS_PointInterval,                      &       ! Arguments related to the LineStyle object
!
                Indexx, Pm3D,                                                                                   &       ! Arguments related to Graph Info
                X_1D, X_2D, X_2Dp, Y_1D, Y_2D, Z_1D, Z_2D, NPoints, DataBlocks,                                                    &       ! Arguments related to Data structure
                CurveStyle, LineTitle, Every, FillStyle,                                                        &       ! Arguments related to Plot structure
!
                Arrow                                                                           )               ! Arguments related to the Arrow object


    if (Dbg) call Logger%Write( "Calling This%AddGraph" )
    call This%AddGraph( Graph_Loc, Debug=Debug )

  end if

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure AddGraphToFigure_0D

  character(*)                                              ,parameter  ::  ProcName = "AddGraphToFigure_0D"
  logical                                                               ::  Dbg
  integer                                                               ::  iGraph                          ! Index of graph
  integer                                                               ::  NGraph                          ! Number of elements loaded in the Graph structure before taking into account the new Graph element
  type(GPF_Graph_Type)  ,dimension(:)   ,allocatable                    ::  Graph_tmp                       ! Temporary graph structure

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "allocated(This%Graph) = ", allocated(This%Graph) )
  if ( .not.allocated(This%Graph) ) then                                                                        ! If the Graph structure is not yet allocated
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     allocate( This%Graph(1) )
!     This%Graph = [Graph]
    allocate( This%Graph(1), source = [Graph] )                                                                    ! Allocating the Graph structure and setting it value to the input Graph structure
#else
    allocate( This%Graph, source = [Graph] )                                                                    ! Allocating the Graph structure and setting it value to the input Graph structure
#endif
  else                                                                                                          ! If the Graph structure is already allocated
    iGraph      =   Graph%Get_Index()                                                                       ! Getting the current graph index to be loaded in the Graph structure
    NGraph      =   size(This%Graph)                                                                        ! Getting the current number of elements loaded in the Graph structure
    if (Dbg) call Logger%Write( "iGraph = ", iGraph )
    if (Dbg) call Logger%Write( "NGraph = ", NGraph )
    if ( NGraph >= iGraph ) then                                                                                ! If Graph size >= than current graph index, then structure reallocation is not required
      This%Graph(iGraph)     =   Graph                                                                      ! Setting current graph structure to input value
    else                                                                                                        ! If Graph size < than current graph index, then structure reallocation is required
      allocate( Graph_tmp(1:NGraph+1), source = [This%Graph, Graph] )
      call move_alloc( from=Graph_tmp, to=This%Graph )                                                          ! Transfering allocation from the temporary to the final graph structure
    end if                                                                                                      ! End if case on number of graph loaded in the structure
  end if                                                                                                        ! End if case on allocation status of the Graph structure
  This%i_Multiplot      =   ( size(This%Graph) > 1)

  if (Dbg) call Logger%Write( "size(This%Graph) = ", size(This%Graph) )
  if (Dbg) call Logger%Write( "This%i_Multiplot = ", This%i_Multiplot )

  if (Dbg) call Logger%Exiting()

End Procedure

! @TODO: Implement the case when adding a Graph object array to a Figure whose Graph component is already allocated (not sure is will be used)
Module Procedure AddGraphToFigure_1D

  character(*)                                              ,parameter  ::  ProcName = "AddGraphToFigure_1D"
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if ( .not.allocated(This%Graph) ) then
    allocate( This%Graph, source = Graph )
  end if
  This%i_Multiplot      =   ( size(This%Graph) > 1)

  if (Dbg) call Logger%Write( "size(This%Graph) = ", size(This%Graph) )
  if (Dbg) call Logger%Write( "This%i_Multiplot = ", This%i_Multiplot )

  if (Dbg) call Logger%Exiting()

End Procedure


Subroutine WriteDataFile( This, Debug )

  use SystemCommand_Library   ,only:  SystemCommand_Type
  use File_Library            ,only:  FileExist, AddPathToFile

  type(GPF_Figure_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Figure object
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName = "WriteDataFile"
  logical                                                               ::  Dbg
  integer                                                               ::  i, Unit, ios
  character(:)  ,allocatable                                            ::  FileName, Directory
  type(SystemCommand_Type)                                              ::  Command

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if ( .not.allocated(This%Unit_Data) ) allocate( This%Unit_Data(size(This%Graph)) )

  Directory   =   This%File%GetDirectory()

  if ( len_trim(Directory) /= 0 ) then
    if ( .Not. FileExist(Directory) ) then
      if (Dbg) call Logger%Write( "-> Calling Command%mkdir: Directory = ", Directory )
      call Command%mkdir( Directory, Options='-p' )
    end if
  end if

  if (Dbg) call Logger%Write( "Number of datafile to write: size(This%Graph) = ", size(This%Graph) )

  do i = 1,size(This%Graph)
  associate( Graph => This%Graph(i) )
    FileName    =   Graph%GetDataFileName()
    if ( len_trim(FileName) == 0 ) call Graph%SetDataFileName(FileName)
    if (Dbg) call Logger%Write( "-> i = ", i, "FileName = ", FileName )
    if (Dbg) call Logger%Write( "  -> Graph%GetDataFileName() = ", Graph%GetDataFileName() )
    FileName    =   AddPathToFile( Directory , FileName )
    if (Dbg) call Logger%Write( "  -> FileName = ", FileName )

    open(NewUnit=This%Unit_Data(i), File=FileName, Status='REPLACE', Iostat=ios)
    Unit    =   This%Unit_Data(i)

!     if (Dbg) call Logger%Write( "Calling WriteCommandFileHeader" )
!     call WriteCommandFileHeader( Unit )

    if (Dbg) call Logger%Write( "Calling Graph%Write_Data" )
    call Graph%Write_Data( Unit )

  end associate
  end do

  if (Dbg) call Logger%Exiting()

End Subroutine


Subroutine CreateFigure( This, Debug )

  use GPF_Parameters            ,only:  DbgFile
  use SystemCommand_Library     ,only:  SystemCommand_Type
  use String_Library            ,only:  EscapeFileCharacters

  type(GPF_Figure_Type)                                 ,intent(in)     ::  This
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName = "CreateFigure"
  logical                                                               ::  Dbg
  integer                                                               ::  CmdStat
  integer                                                               ::  ExitStat
  character(:)  ,allocatable                                            ::  Instruction
!   character(:)  ,allocatable                                            ::  Redirection                     ! Command for redirecting the standard output and standard error output into the debug file
  character(:)  ,allocatable                                            ::  CommandFile
  character(:)  ,allocatable                                            ::  GnuplotExec
!   character(:)  ,allocatable                                            ::  PwdDir
  character(:)  ,allocatable                                            ::  RunDir
  type(SystemCommand_Type)                                              ::  Command

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

!   Redirection   =   ''                                                                                      ! Initialisation of the redirection command to an input string (Required when no debug file name has been defined)
!   if ( allocated(DbgFile) ) Redirection = ' >> ' // trim(adjustl(DbgFile)) // ' 2>&1'                           ! If the Debug file name is allocated, then setting the redirection of standard output and standard error output into the debug file
!   if (Dbg) call Logger%Write( "Redirection = ", Redirection )


  CommandFile     =   EscapeFileCharacters( This%File%GetCommandFileName() )
  RunDir =   This%File%GetDirectory()
  if (Dbg) call Logger%Write( "-> CommandFile = ", CommandFile )
  if (Dbg) call Logger%Write( "-> RunDir      = ", RunDir )

!   if (Dbg) call Logger%Write( "Getting current directory" )
!   if (Dbg) call Logger%Write( "-> Calling Command%pwd" )
!   call Command%pwd( PwdDir )
!   if (Dbg) call Logger%Write( "-> PwdDir      = ", PwdDir )

  GnuplotExec   =   "gnuplot"
  if ( This%File%Term%GetName() == "wxt" ) GnuplotExec   =   "gnuplot-wx"

!   Instruction   =   'GNUPLOT_LIB='//This%File%GetDirectory()//" "
  Instruction   =   "cd "//RunDir//"; "
  Instruction   =   Instruction // GnuplotExec // ' ' // CommandFile      ! // Redirection
!   Instruction   =   Instruction // "; cd "//PwdDir
  if (Dbg) call Logger%Write( "-> Instruction = ", Instruction )

  call Command%Initialize()
  call Command%Set( Command=Instruction )
  call Command%Execute()

  if (Dbg) call Logger%Exiting()

End Subroutine


! The data and command files could be removed here if needed.
Subroutine PostGeneration( This, Debug )

  use SystemCommand_Library   ,only:  SystemCommand_Type
  use File_Library            ,only:  AddPathToFile
  use String_Library          ,only:  EscapeFileCharacters

  type(GPF_Figure_Type)                                 ,intent(in)     ::  This                            !< Passed-object dummy argument
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName = "PostGeneration"
  logical                                                               ::  Dbg
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Directory, FileName
  type(SystemCommand_Type)                                              ::  Command

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if ( .Not. This%File%GetHardCopy() ) then
    if (Dbg) call Logger%Write( "Removing gnuplot command and data files" )
    Directory   =   This%File%GetDirectory()
    FileName    =   This%File%GetFullName()
    if (Dbg) call Logger%Write( "-> Calling Command%rm: ", FileName )
    FileName    =   EscapeFileCharacters( FileName )
    call Command%rm( FileName, DryRun=.True., Debug=.True. )
    do i = 1,size(This%Graph)
      FileName  =   AddPathToFile( Directory , This%Graph(i)%GetDataFileName() )
      if (Dbg) call Logger%Write( "-> Calling Command%rm: ", FileName )
      FileName    =   EscapeFileCharacters( FileName )
      call Command%rm( FileName, DryRun=.True., Debug=.True. )
    end do
    if (Dbg) call Logger%Write( "-> Done removing files" )
  end if

  if (Dbg) call Logger%Exiting()

End Subroutine

Module Procedure Finalize_Figure
  use GPF_LineStyle_Class       ,only:  LS_Global_Units
  if ( allocated(This%Graph) )          deallocate( This%Graph )
  if ( allocated(This%Unit_Data) )      deallocate( This%Unit_Data )
  if ( allocated(LS_Global_Units) )     deallocate( LS_Global_Units )
End Procedure


Module Procedure Allocate_Data

  if ( present(X_1D) ) then
    if ( allocated(X_1D) ) deallocate( X_1D )
    allocate( X_1D(Npoint) )
  end if

  if ( present(Y_1D) ) then
    if ( allocated(Y_1D) ) deallocate( Y_1D )
    allocate( Y_1D(Npoint) )
  end if

  if ( present(X_2D) ) then
    if ( allocated(X_2D) ) deallocate( X_2D )
    allocate( X_2D(NLines,Npoint) )
  end if

  if ( present(X_2Dp) ) then
    if ( allocated(X_2Dp) ) deallocate( X_2Dp )
    allocate( X_2Dp(NLines,Npoint) )
  end if

  if ( present(Y_2D) ) then
    if ( allocated(Y_2D) ) deallocate( Y_2D )
    allocate( Y_2D(NLines,Npoint) )
  end if

  if ( present(LineTitle) ) then
    if ( allocated(LineTitle) ) deallocate( LineTitle )
    allocate( LineTitle(NLines) )
  end if

  if ( present(CurveStyle) ) then
    if ( allocated(CurveStyle) ) deallocate( CurveStyle )
    allocate( CurveStyle(NLines) )
  end if

  if ( present(LS_Color) ) then
    if ( allocated(LS_Color) ) deallocate( LS_Color )
    allocate( LS_Color(NLines) )
  end if

  if ( present(LS_Type) ) then
    if ( allocated(LS_Type) ) deallocate( LS_Type )
    allocate( LS_Type(NLines) )
  end if

  if ( present(LS_Width) ) then
    if ( allocated(LS_Width) ) deallocate( LS_Width )
    allocate( LS_Width(NLines) )
  end if

  if ( present(LS_PointType) ) then
    if ( allocated(LS_PointType) ) deallocate( LS_PointType )
    allocate( LS_PointType(NLines) )
  end if

  if ( present(LS_PointSize) ) then
    if ( allocated(LS_PointSize) ) deallocate( LS_PointSize )
    allocate( LS_PointSize(NLines) )
  end if

  if ( present(LS_PointInterval) ) then
    if ( allocated(LS_PointInterval) ) deallocate( LS_PointInterval )
    allocate( LS_PointInterval(NLines) )
  end if

  if ( present(CB_Values) ) then
    if ( allocated(CB_Values) ) deallocate( CB_Values )
    allocate( CB_Values(NLines) )
  end if

  if ( present(NPoints) ) then
    if ( allocated(NPoints) ) deallocate( NPoints )
    allocate( NPoints(NLines) )
  end if

End Procedure

Module Procedure Is_Figure_Multiplot
  i_Multiplot   =   This%Multiplot%Get_Presence()                                                           ! Setting the multiplot indicator
End Procedure

Module Procedure Set_Figure_Command
End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine WriteCommandFile( This, Debug )

  use SystemCommand_Library   ,only:  SystemCommand_Type
  use File_Library            ,only:  FileExist

  type(GPF_Figure_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Figure object
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName = "WriteCommandFile"
  logical                                                               ::  Dbg
  integer                                                               ::  i, Unit, ios
  character(:)  ,allocatable                                            ::  FileName, Directory
  type(SystemCommand_Type)                                              ::  Command

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  Directory   =   This%File%GetDirectory()
  FileName    =   This%File%GetFullName()

  if (Dbg) call Logger%Write( "Writing command file" )

  if ( len_trim(Directory) /= 0 ) then
    if ( .Not. FileExist(Directory) ) then
      if (Dbg) call Logger%Write( "-> Calling Command%mkdir: Directory = ", Directory )
      call Command%mkdir( Directory, Options='-p' )
    end if
  end if

  if (Dbg) call Logger%Write( "-> Opening command file '"//FileName//"'" )
  open(NewUnit=Unit, File=FileName, Status='REPLACE', Iostat=ios)

  if (Dbg) call Logger%Write( "Calling WriteCommandFileHeader" )
  call WriteCommandFileHeader( Unit )

  if (Dbg) call Logger%Write( "Calling This%File%Write" )
  call This%File%Write( Unit )

  if (Dbg) call Logger%Write( "Calling This%Multiplot%Initialize" )
  call This%Multiplot%Initialize( Unit )

  do i = 1,size(This%Graph)
    if (Dbg) call Logger%Write( "-> i = ", i, "Calling This%Graph(i)%Write" )
    call This%Multiplot%Write_Comment( Unit, i )
    call This%Graph(i)%Write( Unit )
  end do

  if (Dbg) call Logger%Write( "Calling This%Multiplot%Finalize" )
  call This%Multiplot%Finalize( Unit )

  write(Unit,"(/,a)") '  q'
  close(Unit)

  if (Dbg) call Logger%Exiting()

End Subroutine

Subroutine WriteCommandFileHeader( Unit )
  use GPF_Parameters            ,only:  GPF_Version
  use DateAndTime_Library       ,only:  DateAndTime_Type
  use Utilities_Library         ,only:  AddElementToArray
  integer                                               ,intent(in)     ::  Unit                    !< File unit number
  type(DateAndTime_Type)                                                ::  DateAndTime
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Date, Year, Header(:)
  call DateAndTime%Initialize()
  Date    =   DateAndTime%Get_DayMonthYear( "-" )
  Year    =   DateAndTime%Get_Year()
  call AddElementToArray( "######################################################################", Header )
  call AddElementToArray( "## Gnuplot file created using the GnuPlotFortran (GPF) Program       #", Header )
  call AddElementToArray( "## Copyright(C) "//Year//" Bruno Lopez -- Version "//GPF_Version//"                      #", Header )
  call AddElementToArray( "## Creation date: "//Date//"                                         #", Header )
  call AddElementToArray( "######################################################################", Header )
  do i = 1,size(Header)
    write(Unit,*) Header(i)
  end do
  write(Unit,*)
End Subroutine

End SubModule