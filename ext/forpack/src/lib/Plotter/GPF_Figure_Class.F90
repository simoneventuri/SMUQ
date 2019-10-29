Module GPF_Figure_Class

  use GPF_Parameters            ,only:  rkp
  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_File_Class            ,only:  GPF_File_Type
  use GPF_Multiplot_Class       ,only:  GPF_Multiplot_Type
  use GPF_Graph_Class           ,only:  GPF_Graph_Type
  use GPF_Arrow_Class           ,only:  GPF_Arrow_Type
  use GPF_DataBlocks_Class        ,only:  GPF_DataBlocks_Type

  implicit none

  private

  public  ::  GPF_Figure_Type

  Type  ,extends(GPF_Command_Type)                              ::  GPF_Figure_Type
    private
    type(GPF_File_Type)                                         ::  File                                    !< File object
    type(GPF_Multiplot_Type)                                    ::  Multiplot                               !< Multiplot object
    type(GPF_Graph_Type)        ,dimension(:)   ,allocatable    ::  Graph                                   !< Graph object
    logical                                                     ::  i_Multiplot=.False.
    integer                                                     ::  Unit_Command
    integer                     ,dimension(:)   ,allocatable    ::  Unit_Data
  contains
    private
    procedure   ,public   ::  Plot                                            !< Plotting a figure
    procedure   ,public   ::  splot
    procedure   ,public   ::  Set_Graph       =>  SetFigureGraph        !< Setting the Graph object
    procedure   ,public ,nopass         ::  Allocate_Data                                   !< Allocating working data
    procedure   ,public   ::  Set_Command     =>  Set_Figure_Command
    generic               ::  AddGraph        =>  AddGraphToFigure_0D, AddGraphToFigure_1D !< Adding a Graph object to the Figure object
    procedure             ::  AddGraphToFigure_0D                             !< Adding a scalar Graph object to the Figure object
    procedure             ::  AddGraphToFigure_1D                             !< Adding a vector Graph object to the Figure object
    procedure             ::  Finalize        =>  Finalize_Figure
    procedure   ,private  ::  Is_Multiplot    =>  Is_Figure_Multiplot
  End Type

  Interface
    Module Subroutine splot( &
                This, Debug,                                                           &
!               *******************
!               * File parameters *
!               *******************
                Name, Directory, HardCopy,                                              &                       ! Arguments related to the Output object
                Terminal, FontType, FontSize, Enhanced, Color, Term_Size,               &                       ! Arguments related to the Terminal object
!               ************************
!               * Multiplot parameters *
!               ************************
                Multiplot_Title, Multiplot_Title_Font_Name, Multiplot_Title_Font_Size, Multiplot_Title_Enhanced,        &                       ! Arguments related to the Multiplot title
                Multiplot_NRows, Multiplot_NCols,                                       &                       ! Arguments related to the Multiplot layout dimensions
                Multiplot_Xscale, Multiplot_Yscale,                                     &                       ! Arguments related to the Multiplot layout scales
                Multiplot_Xoffset, Multiplot_Yoffset,                                   &                       ! Arguments related to the Multiplot layout offsets
!               ********************
!               * Graph parameters *
!               ********************
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
                Indexx, Pm3D,                                   &                       ! Arguments related to Info structure
                X_1D, X_2D, X_2Dp, Y_1D, Y_2D, Z_1D, Z_2D, NPoints, DataBlocks,   &                       ! Arguments related to Graph Data
                CurveStyle, LineTitle, Every, FillStyle,            &                       ! Arguments related to Graph Line
!
                Arrow,          & ! Arguments related to Arrow object
                Generate  )

      class(GPF_Figure_Type)                                ,intent(out)    ::  This                            !< Figure to be plotted
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

    ! *******************
    ! * File parameters *
    ! *******************
      character(*)                                ,optional ,intent(in)     ::  Name                            !< File Name
      character(*)                                ,optional ,intent(in)     ::  Directory                       !< Directory Name
      logical                                     ,optional ,intent(in)     ::  HardCopy                        !< Hardcopy indicator
      character(*)                                ,optional ,intent(in)     ::  Terminal                        !< Terminal
      character(*)                                ,optional ,intent(in)     ::  FontType                        !< File FontType
      character(*)                                ,optional ,intent(in)     ::  FontSize                        !< File FontSize
      logical                                     ,optional ,intent(in)     ::  Enhanced                        !< File Enhancement indicator
      logical                                     ,optional ,intent(in)     ::  Color                           !< File color indicator
      character(*)                                ,optional ,intent(in)     ::  Term_Size                       !< Terminal size

    ! ************************
    ! * Multiplot parameters *
    ! ************************
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

    ! ********************
    ! * Graph parameters *
    ! ********************
      type(GPF_Graph_Type)                        ,optional ,intent(in)     ::  Graph
      type(GPF_Graph_Type)  ,dimension(:)         ,optional ,intent(in)     ::  Graphs
    ! Arguments related to Size object
      character(*)                                ,optional ,intent(in)     ::  Size_Ratio                      !< Ratio of the graph
      character(*)                                ,optional ,intent(in)     ::  Size_X_Scale                    !< X scale of the graph size
      character(*)                                ,optional ,intent(in)     ::  Size_Y_Scale                    !< Y scale of the graph size
      character(*)                                ,optional ,intent(in)     ::  Size_Scales                     !< Scales
      logical                                     ,optional ,intent(in)     ::  Size_Isometric                  !< Isometric size indicator (equal unit length for the x/y/z-axis)
    ! Arguments related to the Origin object
      character(*)                                ,optional ,intent(in)     ::  Origin_X_Coord                  !< X coordinates of the graph origin
      character(*)                                ,optional ,intent(in)     ::  Origin_Y_Coord                  !< Y coordinates of the graph origin
      character(*)                                ,optional ,intent(in)     ::  Origin_Coordinates              !< Coordinates of the graph origin
    ! Arguments related to Maring object
      character(*)                                ,optional ,intent(in)     ::  Margin_Top                      !< Top margin
      character(*)                                ,optional ,intent(in)     ::  Margin_Bottom                   !< Bottom margin
      character(*)                                ,optional ,intent(in)     ::  Margin_Left                     !< Left margin
      character(*)                                ,optional ,intent(in)     ::  Margin_Right                    !< Right margin
    ! Arguments related to View object
      character(*)                                ,optional ,intent(in)     ::  View_Rot_x                      !< View angle around the x-axis
      character(*)                                ,optional ,intent(in)     ::  View_Rot_z                      !< View angle around the z-axis
      character(*)                                ,optional ,intent(in)     ::  View_Scale                      !< View Y scale
      character(*)                                ,optional ,intent(in)     ::  View_Scale_z                    !< View scale of the entire graph
      logical                                     ,optional ,intent(in)     ::  View_Map                        !< Map view indicator
      logical                                     ,optional ,intent(in)     ::  View_Equal_xy                   !< Indicator of equal unit length for the x/y axis
      logical                                     ,optional ,intent(in)     ::  View_Equal_xyz                  !< Indicator of equal unit length for the x/y/z axis
    ! Arguments related to the Title object
      character(*)                                ,optional ,intent(in)     ::  Title                           !< Title text
      character(*)                                ,optional ,intent(in)     ::  Title_Offset                    !< Title offset
      character(*)                                ,optional ,intent(in)     ::  Title_Font                      !< Title font name
      character(*)                                ,optional ,intent(in)     ::  Title_Font_Size                 !< Title font size
      character(*)                                ,optional ,intent(in)     ::  Title_Color                     !< Title color
      logical                                     ,optional ,intent(in)     ::  Title_Enhanced                  !< Title enhanced indicator
    ! Arguments related to the Key object
      character(*)                                ,optional ,intent(in)     ::  Key_Font                        !< Key font name
      character(*)                                ,optional ,intent(in)     ::  Key_Font_Size                   !< Key font Size
      character(*)                                ,optional ,intent(in)     ::  Key_Space                       !< Key spacing
      character(*)                                ,optional ,intent(in)     ::  Key_Position                    !< Key position
      logical                                     ,optional ,intent(in)     ::  Key_Setting                     !< Key setting indicator
    ! Arguments related to the Grid object
      character(*)                                ,optional ,intent(in)     ::  Grid_Command                    !< Graph grid indicator
    ! Arguments related to the X-Axis object
      real(rkp)                                   ,optional ,intent(in)     ::  X_Min                           !< X-Axis minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  X_Max                           !< X-Axis maximum bound
      logical                                     ,optional ,intent(in)     ::  X_Reverse                       !< X-Axis reversing indicator
      logical                                     ,optional ,intent(in)     ::  X_LogScale                      !< X-Axis log scale indicator
      integer                                     ,optional ,intent(in)     ::  X_LogBase                       !< X-Axis log scale base
      character(*)                                ,optional ,intent(in)     ::  X_Label                         !< X-Axis label text
      character(*)                                ,optional ,intent(in)     ::  X_Label_Offset                  !< X-Axis label offset
      character(*)                                ,optional ,intent(in)     ::  X_Label_Font                    !< X-Axis label font name
      character(*)                                ,optional ,intent(in)     ::  X_Label_Font_Size               !< X-Axis label font size
      character(*)                                ,optional ,intent(in)     ::  X_Label_Color                   !< X-Axis label color
      logical                                     ,optional ,intent(in)     ::  X_Label_Enhanced                !< X-Axis label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  X_Format                        !< X-Axis format
    ! Arguments related to the Y-Axis object
      real(rkp)                                   ,optional ,intent(in)     ::  Y_Min                           !< Y-Axis minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  Y_Max                           !< Y-Axis maximum bound
      logical                                     ,optional ,intent(in)     ::  Y_Reverse                       !< Y-Axis reversing indicator
      logical                                     ,optional ,intent(in)     ::  Y_LogScale                      !< Y-Axis log scale indicator
      integer                                     ,optional ,intent(in)     ::  Y_LogBase                       !< Y-Axis log scale base
      character(*)                                ,optional ,intent(in)     ::  Y_Label                         !< Y-Axis label text
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Offset                  !< Y-Axis label offset
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Font                    !< Y-Axis label font name
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Font_Size               !< Y-Axis label font size
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Color                   !< Y-Axis label color
      logical                                     ,optional ,intent(in)     ::  Y_Label_Enhanced                !< Y-Axis label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  Y_Format                        !< Y-Axis format
    ! Arguments related to the Z-Axis object
      real(rkp)                                   ,optional ,intent(in)     ::  Z_Min                           !< Z-Axis minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  Z_Max                           !< Z-Axis maximum bound
      logical                                     ,optional ,intent(in)     ::  Z_Reverse                       !< Z-Axis reversing indicator
      logical                                     ,optional ,intent(in)     ::  Z_LogScale                      !< Z-Axis log scale indicator
      integer                                     ,optional ,intent(in)     ::  Z_LogBase                       !< Z-Axis log scale base
      character(*)                                ,optional ,intent(in)     ::  Z_Label                         !< Z-Axis label text
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Offset                  !< Z-Axis label offset
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Font                    !< Z-Axis label font name
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Font_Size               !< Z-Axis label font size
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Color                   !< Z-Axis label color
      logical                                     ,optional ,intent(in)     ::  Z_Label_Enhanced                !< Z-Axis label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  Z_Format                        !< Z-Axis format
    ! Arguments related to the ColorBar-Axis object
      real(rkp)     ,dimension(:)                 ,optional ,intent(in)     ::  CB_Values                       !< ColorBar values (DIM=NLine)
      real(rkp)                                   ,optional ,intent(in)     ::  CB_Min                          !< ColorBar minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  CB_Max                          !< ColorBar maximum bound
      logical                                     ,optional ,intent(in)     ::  CB_Reverse                      !< ColorBar reversing indicator
      logical                                     ,optional ,intent(in)     ::  CB_LogScale                     !< ColorBar log scale indicator
      integer                                     ,optional ,intent(in)     ::  CB_LogBase                      !< ColorBar log scale base
      character(*)                                ,optional ,intent(in)     ::  CB_Label                        !< ColorBar label text
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Offset                 !< ColorBar label offset
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Font                   !< ColorBar label font name
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Font_Size              !< ColorBar label font size
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Color                  !< ColorBar label color
      logical                                     ,optional ,intent(in)     ::  CB_Label_Enhanced               !< ColorBar label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  CB_Format                       !< ColorBar format
    ! Arguments related to the ColorBar-Box object
      character(*)                                ,optional ,intent(in)     ::  CB_Orientation                  !< ColorBox orientation (either 'vertical' or 'horizontal')
      character(*)                                ,optional ,intent(in)     ::  CB_Origin                       !< ColorBox origin coordinates ('x,y')
      character(*)                                ,optional ,intent(in)     ::  CB_Size                         !< ColorBox size ('h,w')
      character(*)                                ,optional ,intent(in)     ::  CB_Position                     !< ColorBox position (either 'front' or 'back')
      character(*)                                ,optional ,intent(in)     ::  CB_Border                       !< ColorBox borders type
      logical                                     ,optional ,intent(in)     ::  CB_UnSetting                    !< ColorBox unsetting indicator
    ! Arguments related to the LineStyle object
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_Type                         !< LineStyle Type
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_Width                        !< LineStyle Width
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_Color                        !< LineStyle Color
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_PointType                    !< LineStyle PointType
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_PointSize                    !< LineStyle PointSize
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_PointInterval                !< LineStyle PointInterval

      integer                                     ,optional ,intent(in)     ::  Indexx                          !< Graph indexx
      character(*)                                ,optional ,intent(in)     ::  Pm3D                            !< Graph Pm3D
      real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  X_1D                            !< X-Data for 1D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  X_2D                            !< X-Data for 2D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  X_2Dp                            !< X-Data for 2D variable
      real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Y_1D                            !< Y-Data for 1D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Y_2D                            !< Y-Data for 2D variable
      real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Z_1D                            !< Z-Data for 1D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Z_2D                            !< Z-Data for 2D variable
      integer       ,dimension( : )               ,optional ,intent(in)     ::  NPoints                         !< Number of points per lines
      type(GPF_DataBlocks_Type)                     ,optional ,intent(in)     ::  DataBlocks


      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  CurveStyle                      !< Style of curves
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LineTitle                       !< Line legend
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  Every                           !< Line every
      character(*)                                ,optional ,intent(in)     ::  FillStyle                       !< Line FillStyle (for filled line)

    ! Arguments related to the Arrow object
      type(GPF_Arrow_Type)                        ,optional ,intent(in)     ::  Arrow                           !< Arrow object

      logical                                     ,optional ,intent(in)     ::  Generate                        !< Indicator whether the plot has to be generated
    End Subroutine

    Module Subroutine Plot( This, Debug,                                                           &
    !               *******************
    !               * File parameters *
    !               *******************
                    Name, Directory, HardCopy,                                              &                       ! Arguments related to the Output object
                    Terminal, FontType, FontSize, Enhanced, Color, Term_Size,               &                       ! Arguments related to the Terminal object
    !               ************************
    !               * Multiplot parameters *
    !               ************************
                    Multiplot_Title, Multiplot_Title_Font_Name, Multiplot_Title_Font_Size, Multiplot_Title_Enhanced,        &                       ! Arguments related to the Multiplot title
                    Multiplot_NRows, Multiplot_NCols,                                       &                       ! Arguments related to the Multiplot layout dimensions
                    Multiplot_Xscale, Multiplot_Yscale,                                     &                       ! Arguments related to the Multiplot layout scales
                    Multiplot_Xoffset, Multiplot_Yoffset,                                   &                       ! Arguments related to the Multiplot layout offsets
    !               ********************
    !               * Graph parameters *
    !               ********************
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
                    Indexx, Pm3D,                                   &                       ! Arguments related to Info structure
                    X_1D, X_2D, X_2Dp, Y_1D, Y_2D, Z_1D, Z_2D, NPoints,    &                       ! Arguments related to Graph Data
                    CurveStyle, LineTitle, Every, FillStyle,            &                       ! Arguments related to Graph Line
    !
                    Arrow,          & ! Arguments related to Arrow object
                    Generate  )

      class(GPF_Figure_Type)                                ,intent(out)    ::  This                            !< Figure to be plotted
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    ! *******************
    ! * File parameters *
    ! *******************
      character(*)                                ,optional ,intent(in)     ::  Name                            !< File Name
      character(*)                                ,optional ,intent(in)     ::  Directory                       !< Directory Name
      logical                                     ,optional ,intent(in)     ::  HardCopy                        !< Hardcopy indicator
      character(*)                                ,optional ,intent(in)     ::  Terminal                        !< Terminal
      character(*)                                ,optional ,intent(in)     ::  FontType                        !< File FontType
      character(*)                                ,optional ,intent(in)     ::  FontSize                        !< File FontSize
      logical                                     ,optional ,intent(in)     ::  Enhanced                        !< File Enhancement indicator
      logical                                     ,optional ,intent(in)     ::  Color                           !< File color indicator
      character(*)                                ,optional ,intent(in)     ::  Term_Size                       !< Terminal size

    ! ************************
    ! * Multiplot parameters *
    ! ************************
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

    ! ********************
    ! * Graph parameters *
    ! ********************
      type(GPF_Graph_Type)                        ,optional ,intent(in)     ::  Graph
      type(GPF_Graph_Type)  ,dimension(:)         ,optional ,intent(in)     ::  Graphs
    ! Arguments related to Size object
      character(*)                                ,optional ,intent(in)     ::  Size_Ratio                      !< Ratio of the graph
      character(*)                                ,optional ,intent(in)     ::  Size_X_Scale                    !< X scale of the graph size
      character(*)                                ,optional ,intent(in)     ::  Size_Y_Scale                    !< Y scale of the graph size
      character(*)                                ,optional ,intent(in)     ::  Size_Scales                     !< Scales
      logical                                     ,optional ,intent(in)     ::  Size_Isometric                  !< Isometric size indicator (equal unit length for the x/y/z-axis)
    ! Arguments related to the Origin object
      character(*)                                ,optional ,intent(in)     ::  Origin_X_Coord                  !< X coordinates of the graph origin
      character(*)                                ,optional ,intent(in)     ::  Origin_Y_Coord                  !< Y coordinates of the graph origin
      character(*)                                ,optional ,intent(in)     ::  Origin_Coordinates              !< Coordinates of the graph origin
    ! Arguments related to Maring object
      character(*)                                ,optional ,intent(in)     ::  Margin_Top                      !< Top margin
      character(*)                                ,optional ,intent(in)     ::  Margin_Bottom                   !< Bottom margin
      character(*)                                ,optional ,intent(in)     ::  Margin_Left                     !< Left margin
      character(*)                                ,optional ,intent(in)     ::  Margin_Right                    !< Right margin
    ! Arguments related to View object
      character(*)                                ,optional ,intent(in)     ::  View_Rot_x                      !< View angle around the x-axis
      character(*)                                ,optional ,intent(in)     ::  View_Rot_z                      !< View angle around the z-axis
      character(*)                                ,optional ,intent(in)     ::  View_Scale                      !< View Y scale
      character(*)                                ,optional ,intent(in)     ::  View_Scale_z                    !< View scale of the entire graph
      logical                                     ,optional ,intent(in)     ::  View_Map                        !< Map view indicator
      logical                                     ,optional ,intent(in)     ::  View_Equal_xy                   !< Indicator of equal unit length for the x/y axis
      logical                                     ,optional ,intent(in)     ::  View_Equal_xyz                  !< Indicator of equal unit length for the x/y/z axis
    ! Arguments related to the Title object
      character(*)                                ,optional ,intent(in)     ::  Title                           !< Title text
      character(*)                                ,optional ,intent(in)     ::  Title_Offset                    !< Title offset
      character(*)                                ,optional ,intent(in)     ::  Title_Font                      !< Title font name
      character(*)                                ,optional ,intent(in)     ::  Title_Font_Size                 !< Title font size
      character(*)                                ,optional ,intent(in)     ::  Title_Color                     !< Title color
      logical                                     ,optional ,intent(in)     ::  Title_Enhanced                  !< Title enhanced indicator
    ! Arguments related to the Key object
      character(*)                                ,optional ,intent(in)     ::  Key_Font                        !< Key font name
      character(*)                                ,optional ,intent(in)     ::  Key_Font_Size                   !< Key font Size
      character(*)                                ,optional ,intent(in)     ::  Key_Space                       !< Key spacing
      character(*)                                ,optional ,intent(in)     ::  Key_Position                    !< Key position
      logical                                     ,optional ,intent(in)     ::  Key_Setting                     !< Key setting indicator
    ! Arguments related to the Grid object
      character(*)                                ,optional ,intent(in)     ::  Grid_Command                    !< Graph grid indicator
    ! Arguments related to the X-Axis object
      real(rkp)                                   ,optional ,intent(in)     ::  X_Min                           !< X-Axis minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  X_Max                           !< X-Axis maximum bound
      logical                                     ,optional ,intent(in)     ::  X_Reverse                       !< X-Axis reversing indicator
      logical                                     ,optional ,intent(in)     ::  X_LogScale                      !< X-Axis log scale indicator
      integer                                     ,optional ,intent(in)     ::  X_LogBase                       !< X-Axis log scale base
      character(*)                                ,optional ,intent(in)     ::  X_Label                         !< X-Axis label text
      character(*)                                ,optional ,intent(in)     ::  X_Label_Offset                  !< X-Axis label offset
      character(*)                                ,optional ,intent(in)     ::  X_Label_Font                    !< X-Axis label font name
      character(*)                                ,optional ,intent(in)     ::  X_Label_Font_Size               !< X-Axis label font size
      character(*)                                ,optional ,intent(in)     ::  X_Label_Color                   !< X-Axis label color
      logical                                     ,optional ,intent(in)     ::  X_Label_Enhanced                !< X-Axis label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  X_Format                        !< X-Axis format
    ! Arguments related to the Y-Axis object
      real(rkp)                                   ,optional ,intent(in)     ::  Y_Min                           !< Y-Axis minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  Y_Max                           !< Y-Axis maximum bound
      logical                                     ,optional ,intent(in)     ::  Y_Reverse                       !< Y-Axis reversing indicator
      logical                                     ,optional ,intent(in)     ::  Y_LogScale                      !< Y-Axis log scale indicator
      integer                                     ,optional ,intent(in)     ::  Y_LogBase                       !< Y-Axis log scale base
      character(*)                                ,optional ,intent(in)     ::  Y_Label                         !< Y-Axis label text
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Offset                  !< Y-Axis label offset
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Font                    !< Y-Axis label font name
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Font_Size               !< Y-Axis label font size
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Color                   !< Y-Axis label color
      logical                                     ,optional ,intent(in)     ::  Y_Label_Enhanced                !< Y-Axis label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  Y_Format                        !< Y-Axis format
    ! Arguments related to the Z-Axis object
      real(rkp)                                   ,optional ,intent(in)     ::  Z_Min                           !< Z-Axis minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  Z_Max                           !< Z-Axis maximum bound
      logical                                     ,optional ,intent(in)     ::  Z_Reverse                       !< Z-Axis reversing indicator
      logical                                     ,optional ,intent(in)     ::  Z_LogScale                      !< Z-Axis log scale indicator
      integer                                     ,optional ,intent(in)     ::  Z_LogBase                       !< Z-Axis log scale base
      character(*)                                ,optional ,intent(in)     ::  Z_Label                         !< Z-Axis label text
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Offset                  !< Z-Axis label offset
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Font                    !< Z-Axis label font name
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Font_Size               !< Z-Axis label font size
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Color                   !< Z-Axis label color
      logical                                     ,optional ,intent(in)     ::  Z_Label_Enhanced                !< Z-Axis label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  Z_Format                        !< Z-Axis format
    ! Arguments related to the ColorBar-Axis object
      real(rkp)     ,dimension(:)                 ,optional ,intent(in)     ::  CB_Values                       !< ColorBar values (DIM=NLine)
      real(rkp)                                   ,optional ,intent(in)     ::  CB_Min                          !< ColorBar minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  CB_Max                          !< ColorBar maximum bound
      logical                                     ,optional ,intent(in)     ::  CB_Reverse                      !< ColorBar reversing indicator
      logical                                     ,optional ,intent(in)     ::  CB_LogScale                     !< ColorBar log scale indicator
      integer                                     ,optional ,intent(in)     ::  CB_LogBase                      !< ColorBar log scale base
      character(*)                                ,optional ,intent(in)     ::  CB_Label                        !< ColorBar label text
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Offset                 !< ColorBar label offset
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Font                   !< ColorBar label font name
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Font_Size              !< ColorBar label font size
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Color                  !< ColorBar label color
      logical                                     ,optional ,intent(in)     ::  CB_Label_Enhanced               !< ColorBar label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  CB_Format                       !< ColorBar format
    ! Arguments related to the ColorBar-Box object
      character(*)                                ,optional ,intent(in)     ::  CB_Orientation                  !< ColorBox orientation (either 'vertical' or 'horizontal')
      character(*)                                ,optional ,intent(in)     ::  CB_Origin                       !< ColorBox origin coordinates ('x,y')
      character(*)                                ,optional ,intent(in)     ::  CB_Size                         !< ColorBox size ('h,w')
      character(*)                                ,optional ,intent(in)     ::  CB_Position                     !< ColorBox position (either 'front' or 'back')
      character(*)                                ,optional ,intent(in)     ::  CB_Border                       !< ColorBox borders type
      logical                                     ,optional ,intent(in)     ::  CB_UnSetting                    !< ColorBox unsetting indicator
    ! Arguments related to the LineStyle object
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_Type                         !< LineStyle Type
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_Width                        !< LineStyle Width
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_Color                        !< LineStyle Color
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_PointType                    !< LineStyle PointType
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_PointSize                    !< LineStyle PointSize
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_PointInterval                !< LineStyle PointInterval

      integer                                     ,optional ,intent(in)     ::  Indexx                          !< Graph indexx
      character(*)                                ,optional ,intent(in)     ::  Pm3D                            !< Graph Pm3D
      real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  X_1D                            !< X-Data for 1D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  X_2D                            !< X-Data for 2D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  X_2Dp                            !< X-Data for 2D variable
      real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Y_1D                            !< Y-Data for 1D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Y_2D                            !< Y-Data for 2D variable
      real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Z_1D                            !< Z-Data for 1D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Z_2D                            !< Z-Data for 2D variable
      integer       ,dimension( : )               ,optional ,intent(in)     ::  NPoints                         !< Number of points per lines
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  CurveStyle                      !< Style of curves
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LineTitle                       !< Line legend
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  Every                           !< Line every
      character(*)                                ,optional ,intent(in)     ::  FillStyle                       !< Line FillStyle (for filled line)

    ! Arguments related to the Arrow object
      type(GPF_Arrow_Type)                        ,optional ,intent(in)     ::  Arrow                           !< Arrow object

      logical                                     ,optional ,intent(in)     ::  Generate                        !< Indicator whether the plot has to be generated
    End Subroutine

    Module Subroutine SetFigureGraph( This, Debug,                                                       &
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
                    CurveStyle, LineTitle, Every, FillStyle,            &                       ! Arguments related to Graph Line
    !
                    Arrow                                                                           )               ! Arguments related to the Arrow object

      class(GPF_Figure_Type)                                ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Figure object
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

      type(GPF_Graph_Type)                        ,optional ,intent(in)     ::  Graph                           !< Graph object
      type(GPF_Graph_Type)  ,dimension(:)         ,optional ,intent(in)     ::  Graphs                          !< Graph objects

    ! Arguments related to Size object
      character(*)                                ,optional ,intent(in)     ::  Size_Ratio                      !< Ratio of the graph
      character(*)                                ,optional ,intent(in)     ::  Size_X_Scale                    !< X scale of the graph size
      character(*)                                ,optional ,intent(in)     ::  Size_Y_Scale                    !< Y scale of the graph size
      character(*)                                ,optional ,intent(in)     ::  Size_Scales                     !< Scales
      logical                                     ,optional ,intent(in)     ::  Size_Isometric                  !< Isometric size indicator (equal unit length for the x/y/z-axis)
    ! Arguments related to the Origin object
      character(*)                                ,optional ,intent(in)     ::  Origin_X_Coord                  !< X coordinates of the graph origin
      character(*)                                ,optional ,intent(in)     ::  Origin_Y_Coord                  !< Y coordinates of the graph origin
      character(*)                                ,optional ,intent(in)     ::  Origin_Coordinates              !< Coordinates of the graph origin
    ! Arguments related to Maring object
      character(*)                                ,optional ,intent(in)     ::  Margin_Top                      !< Top margin
      character(*)                                ,optional ,intent(in)     ::  Margin_Bottom                   !< Bottom margin
      character(*)                                ,optional ,intent(in)     ::  Margin_Left                     !< Left margin
      character(*)                                ,optional ,intent(in)     ::  Margin_Right                    !< Right margin
    ! Arguments related to View object
      character(*)                                ,optional ,intent(in)     ::  View_Rot_x                      !< View angle around the x-axis
      character(*)                                ,optional ,intent(in)     ::  View_Rot_z                      !< View angle around the z-axis
      character(*)                                ,optional ,intent(in)     ::  View_Scale                      !< View Y scale
      character(*)                                ,optional ,intent(in)     ::  View_Scale_z                    !< View scale of the entire graph
      logical                                     ,optional ,intent(in)     ::  View_Map                        !< Map view indicator
      logical                                     ,optional ,intent(in)     ::  View_Equal_xy                   !< Indicator of equal unit length for the x/y axis
      logical                                     ,optional ,intent(in)     ::  View_Equal_xyz                  !< Indicator of equal unit length for the x/y/z axis
    ! Arguments related to the Title object
      character(*)                                ,optional ,intent(in)     ::  Title                           !< Title text
      character(*)                                ,optional ,intent(in)     ::  Title_Offset                    !< Title offset
      character(*)                                ,optional ,intent(in)     ::  Title_Font                      !< Title font name
      character(*)                                ,optional ,intent(in)     ::  Title_Font_Size                 !< Title font size
      character(*)                                ,optional ,intent(in)     ::  Title_Color                     !< Title color
      logical                                     ,optional ,intent(in)     ::  Title_Enhanced                  !< Title enhanced indicator
    ! Arguments related to the Key object
      character(*)                                ,optional ,intent(in)     ::  Key_Font                        !< Key font name
      character(*)                                ,optional ,intent(in)     ::  Key_Font_Size                   !< Key font Size
      character(*)                                ,optional ,intent(in)     ::  Key_Space                       !< Key spacing
      character(*)                                ,optional ,intent(in)     ::  Key_Position                    !< Key position
      logical                                     ,optional ,intent(in)     ::  Key_Setting                     !< Key setting indicator
    ! Arguments related to the Grid object
      character(*)                                ,optional ,intent(in)     ::  Grid_Command                    !< Graph grid indicator
    ! Arguments related to the X-Axis object
      real(rkp)                                   ,optional ,intent(in)     ::  X_Min                           !< X-Axis minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  X_Max                           !< X-Axis maximum bound
      logical                                     ,optional ,intent(in)     ::  X_Reverse                       !< X-Axis reversing indicator
      logical                                     ,optional ,intent(in)     ::  X_LogScale                      !< X-Axis log scale indicator
      integer                                     ,optional ,intent(in)     ::  X_LogBase                       !< X-Axis log scale base
      character(*)                                ,optional ,intent(in)     ::  X_Label                         !< X-Axis label text
      character(*)                                ,optional ,intent(in)     ::  X_Label_Offset                  !< X-Axis label offset
      character(*)                                ,optional ,intent(in)     ::  X_Label_Font                    !< X-Axis label font name
      character(*)                                ,optional ,intent(in)     ::  X_Label_Font_Size               !< X-Axis label font size
      character(*)                                ,optional ,intent(in)     ::  X_Label_Color                   !< X-Axis label color
      logical                                     ,optional ,intent(in)     ::  X_Label_Enhanced                !< X-Axis label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  X_Format                        !< X-Axis format
    ! Arguments related to the Y-Axis object
      real(rkp)                                   ,optional ,intent(in)     ::  Y_Min                           !< Y-Axis minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  Y_Max                           !< Y-Axis maximum bound
      logical                                     ,optional ,intent(in)     ::  Y_Reverse                       !< Y-Axis reversing indicator
      logical                                     ,optional ,intent(in)     ::  Y_LogScale                      !< Y-Axis log scale indicator
      integer                                     ,optional ,intent(in)     ::  Y_LogBase                       !< Y-Axis log scale base
      character(*)                                ,optional ,intent(in)     ::  Y_Label                         !< Y-Axis label text
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Offset                  !< Y-Axis label offset
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Font                    !< Y-Axis label font name
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Font_Size               !< Y-Axis label font size
      character(*)                                ,optional ,intent(in)     ::  Y_Label_Color                   !< Y-Axis label color
      logical                                     ,optional ,intent(in)     ::  Y_Label_Enhanced                !< Y-Axis label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  Y_Format                        !< Y-Axis format
    ! Arguments related to the Z-Axis object
      real(rkp)                                   ,optional ,intent(in)     ::  Z_Min                           !< Z-Axis minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  Z_Max                           !< Z-Axis maximum bound
      logical                                     ,optional ,intent(in)     ::  Z_Reverse                       !< Z-Axis reversing indicator
      logical                                     ,optional ,intent(in)     ::  Z_LogScale                      !< Z-Axis log scale indicator
      integer                                     ,optional ,intent(in)     ::  Z_LogBase                       !< Z-Axis log scale base
      character(*)                                ,optional ,intent(in)     ::  Z_Label                         !< Z-Axis label text
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Offset                  !< Z-Axis label offset
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Font                    !< Z-Axis label font name
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Font_Size               !< Z-Axis label font size
      character(*)                                ,optional ,intent(in)     ::  Z_Label_Color                   !< Z-Axis label color
      logical                                     ,optional ,intent(in)     ::  Z_Label_Enhanced                !< Z-Axis label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  Z_Format                        !< Z-Axis format
    ! Arguments related to the ColorBar-Axis object
      real(rkp)     ,dimension(:)                 ,optional ,intent(in)     ::  CB_Values                       !< ColorBar values (DIM=NLine)
      real(rkp)                                   ,optional ,intent(in)     ::  CB_Min                          !< ColorBar minimum bound
      real(rkp)                                   ,optional ,intent(in)     ::  CB_Max                          !< ColorBar maximum bound
      logical                                     ,optional ,intent(in)     ::  CB_Reverse                      !< ColorBar reversing indicator
      logical                                     ,optional ,intent(in)     ::  CB_LogScale                     !< ColorBar log scale indicator
      integer                                     ,optional ,intent(in)     ::  CB_LogBase                      !< ColorBar log scale base
      character(*)                                ,optional ,intent(in)     ::  CB_Label                        !< ColorBar label text
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Offset                 !< ColorBar label offset
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Font                   !< ColorBar label font name
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Font_Size              !< ColorBar label font size
      character(*)                                ,optional ,intent(in)     ::  CB_Label_Color                  !< ColorBar label color
      logical                                     ,optional ,intent(in)     ::  CB_Label_Enhanced               !< ColorBar label enhanced indicator
      character(*)                                ,optional ,intent(in)     ::  CB_Format                       !< ColorBar format
    ! Arguments related to the ColorBar-Box object
      character(*)                                ,optional ,intent(in)     ::  CB_Orientation                  !< ColorBox orientation (either 'vertical' or 'horizontal')
      character(*)                                ,optional ,intent(in)     ::  CB_Origin                       !< ColorBox origin coordinates ('x,y')
      character(*)                                ,optional ,intent(in)     ::  CB_Size                         !< ColorBox size ('h,w')
      character(*)                                ,optional ,intent(in)     ::  CB_Position                     !< ColorBox position (either 'front' or 'back')
      character(*)                                ,optional ,intent(in)     ::  CB_Border                       !< ColorBox borders type
      logical                                     ,optional ,intent(in)     ::  CB_UnSetting                    !< ColorBox unsetting indicator
    ! Arguments related to the LineStyle object
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_Type                         !< LineStyle Type
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_Width                        !< LineStyle Width
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_Color                        !< LineStyle Color
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_PointType                    !< LineStyle PointType
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_PointSize                    !< LineStyle Point size
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LS_PointInterval                !< LineStyle PointInterval




      integer                                     ,optional ,intent(in)     ::  Indexx                          !< Graph indexx
      character(*)                                ,optional ,intent(in)     ::  Pm3D                            !< Graph Pm3D
      real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  X_1D                            !< X-Data for 1D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  X_2D                            !< X-Data for 2D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  X_2Dp                           !< X-Data for 2D variable
      real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Y_1D                            !< Y-Data for 1D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Y_2D                            !< Y-Data for 2D variable
      real(rkp)     ,dimension( :       )         ,optional ,intent(in)     ::  Z_1D                            !< Z-Data for 1D variable
      real(rkp)     ,dimension( :, :    )         ,optional ,intent(in)     ::  Z_2D                            !< Z-Data for 2D variable
      integer       ,dimension( : )               ,optional ,intent(in)     ::  NPoints                         !< Number of points per lines
      type(GPF_DataBlocks_Type)                     ,optional ,intent(in)     ::  DataBlocks

      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  CurveStyle                          !< Line nature
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  LineTitle                       !< Line legend
      character(*)          ,dimension(:)         ,optional ,intent(in)     ::  Every                           !< Line every
      character(*)                                ,optional ,intent(in)     ::  FillStyle                       !< Line FillStyle (for filled line)

    ! Arguments related to the Arrow object
      type(GPF_Arrow_Type)                        ,optional ,intent(in)     ::  Arrow                           !< Arrow object
    End Subroutine

    Module Subroutine AddGraphToFigure_0D( This, Graph, Debug )
      class(GPF_Figure_Type)                                ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Figure object
      type(GPF_Graph_Type)                                  ,intent(in)     ::  Graph                           !< Graph object (Scalar)
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine AddGraphToFigure_1D( This, Graph, Debug )
      class(GPF_Figure_Type)                                ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Figure object
      type(GPF_Graph_Type)  ,dimension(:)                   ,intent(in)     ::  Graph                           !< Graph object (Array)
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine Finalize_Figure( This )
      class(GPF_Figure_Type)                                ,intent(out)    ::  This                            !< Passed-object dummy argument corresponding to the Figure object
    End Subroutine

    Module Subroutine Allocate_Data( NLines, Npoint, X_1D, Y_1D, X_2D, X_2Dp, Y_2D, LineTitle, CurveStyle, LS_Color, LS_Type, LS_Width, LS_PointType, LS_PointSize, LS_PointInterval, CB_Values, NPoints )
      integer                                                       ,intent(in)     ::  NLines          !< Number of lines to be plotted
      integer                                                       ,intent(in)     ::  Npoint          !< Maximum number of point per line
      real(rkp)     ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  X_1D            !<
      real(rkp)     ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  Y_1D            !<
      real(rkp)     ,dimension(:,:) ,allocatable          ,optional ,intent(inout)  ::  X_2D            !<
      real(rkp)     ,dimension(:,:) ,allocatable          ,optional ,intent(inout)  ::  X_2Dp           !<
      real(rkp)     ,dimension(:,:) ,allocatable          ,optional ,intent(inout)  ::  Y_2D            !<
      character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  LineTitle       !<
      character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  CurveStyle
      character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  LS_Color        !<
      character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  LS_Type         !<
      character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  LS_Width
      character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  LS_PointType
      character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  LS_PointSize
      character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  LS_PointInterval
      real(rkp)     ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  CB_Values
      integer       ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  NPoints         !< Number of points per line
    End Subroutine

    Module Function Is_Figure_Multiplot( This ) result(i_Multiplot)
      class(GPF_Figure_Type)                                ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Figure object
      logical                                                               ::  i_Multiplot                     ! Multipot indicator
    End Function

    Module Subroutine Set_Figure_Command( This )
      class(GPF_Figure_Type)                                ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Figure object
    End Subroutine

  End Interface

End Module
