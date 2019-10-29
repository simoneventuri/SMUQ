Module GPF_Parameters

  use iso_fortran_env           ,only:  Output_Unit

  implicit none

  public

! **************************
! *     GFP PARAMETERS     *
! **************************
  character(*)                  ,parameter      ::  GPF_Name                =       'GnuPlot-Fortran'       !< GPF name
  character(*)                  ,parameter      ::  GPF_Version             =       '2.0'                   !< GPF version


! ********************************
! *     PRECISION PARAMETERS     *
! ********************************
  integer       ,parameter      ,private        ::  Ndig    =       15                                      ! Number of digits for kind
  integer       ,parameter      ,private        ::  Ndec    =       307                                     ! Number of representable decades
  integer       ,parameter                      ::  rkp     =       selected_real_kind(Ndig,Ndec)           ! Real kind precision
!   integer       ,parameter                  ::  rkp     =       16

! ********************************
! *     DEBUGGING PARAMETERS     *
! ********************************
!   integer                       ,protected  ::  DbgUnit                 =       Output_Unit                     !< File unit number for debugging
!   character(:)  ,allocatable    ,protected  ::  DbgFile
  integer                                       ::  DbgUnit                 =       Output_Unit                     !< File unit number for debugging
  character(:)  ,allocatable                    ::  DbgFile
  character(*)                  ,parameter      ::  DbgFile_Default         =       'gpf.log'
  logical                       ,parameter      ::  i_Debug_Default         =       .False.                         !< Default debugging indicator
  character(*)                  ,parameter      ::  Empty_String                 =       ""

! **********************************
! *     COORDINATES PARAMETERS     *
! **********************************
  character(*)                  ,parameter      ::  c_first_                =        'first'                        !<
  character(*)                  ,parameter      ::  c_second_               =        'second'                       !<
  character(*)                  ,parameter      ::  c_graph_                =        'graph'                        !<
  character(*)                  ,parameter      ::  c_screen_               =        'screen'                       !<
  character(*)                  ,parameter      ::  c_character_            =        'character'                    !<
  character(*)  ,dimension(5)   ,parameter      ::  Coordinates_System_Valid=   [ character(9) ::   &               !< All valid coordinates systems
                                                                c_first_,       &
                                                                c_second_,      &
                                                                c_graph_,       &
                                                                c_screen_,      &
                                                                c_character_    ]

! ****************************
! *     ARROW PARAMETERS     *
! ****************************
  character(*)                  ,parameter      ::  c_from_                 =       'from'                          !<
  character(*)                  ,parameter      ::  c_to_                   =       'to'                            !<
  character(*)                  ,parameter      ::  c_front_                =       'front'                         !<
  character(*)                  ,parameter      ::  c_back_                 =       'back'                          !<
  character(*)                  ,parameter      ::  c_nohead_               =       'nohead'                        !<
  character(*)                  ,parameter      ::  c_head_                 =       'head'                          !<
  character(*)                  ,parameter      ::  c_backhead_             =       'backhead'                      !<
  character(*)                  ,parameter      ::  c_heads_                =       'heads'                         !<
  character(*)  ,dimension(4)   ,parameter      ::  Arrow_Head_Valid        =   [ character(8) ::   &               !< Valid arrow's head
                                                                c_nohead_,      &
                                                                c_head_,        &
                                                                c_backhead_,    &
                                                                c_heads_        ]

! ********************************
! *     EXTENSION PARAMETERS     *
! ********************************
  character(*)                  ,parameter      ::  KEY_dat                 =       'dat'
  character(*)                  ,parameter      ::  KEY_gp                  =       'gp'
  character(*)                  ,parameter      ::  KEY_png                 =       'png'                           !< File extension for 'png' image
  character(*)                  ,parameter      ::  KEY_eps                 =       'eps'                           !< File extension for 'eps' image
  character(*)                  ,parameter      ::  KEY_pdf                 =       'pdf'                           !< File extension for 'pdf' image
  character(*)                  ,parameter      ::  KEY_postscript          =       'postscript'
  character(*)                  ,parameter      ::  Extension_DataFile      =       KEY_dat                         !< Extension of the data file
  character(*)                  ,parameter      ::  Extension_CommandFile   =       KEY_gp                        !< Extension of the command file
  character(*)                  ,parameter      ::  Extension_Default       =       KEY_eps                         !< Default extension
  character(*)  ,dimension(4)   ,parameter      ::  Extension_Valid         =   [ character(3) ::   &               !< All valid extensions
                                                                Empty_String,   &
                                                                KEY_png,        &
                                                                KEY_eps,        &
                                                                KEY_pdf         ]

! *******************************
! *     TERMINAL PARAMETERS     *
! *******************************
  character(*)                  ,parameter      ::  KEY_wxt                 =       'wxt'
  character(*)                  ,parameter      ::  KEY_pngcairo            =       "pngcairo"
  character(*)                  ,parameter      ::  Terminal_Default        =       KEY_wxt
  character(*)  ,dimension(4)   ,parameter      ::  Terminal_Valid          =       [ character(10) :: KEY_wxt, KEY_pngcairo, KEY_postscript, KEY_pdf ]
  character(*)  ,dimension(4)   ,parameter      ::  Terminal_To_LineWidth   =       [ '3',     '3',          '3',            '5'     ]
  character(*)  ,dimension(4)   ,parameter      ::  Extension_To_Terminal   =       [ character(10) :: KEY_wxt, KEY_pngcairo, KEY_postscript, KEY_pdf ]
  character(*)                  ,parameter      ::  KEY_dashed              =       'dashed'
  character(*)                  ,parameter      ::  KEY_solid               =       'solid'
  character(*)                  ,parameter      ::  KEY_persist             =       'persist'
  character(*)                  ,parameter      ::  KEY_enhanced            =       'enhanced'
  character(*)                  ,parameter      ::  KEY_noenhanced          =       'noenhanced'
  character(*)                  ,parameter      ::  KEY_landscape           =       'landscape'

! ***************************
! *     FILE PARAMETERS     *
! ***************************
  character(*)                  ,parameter      ::  KEY_command             =       'command'                       !< Prefix of the name of the Command File
  character(*)                  ,parameter      ::  KEY_data                =       'data'                          !< Prefix of the name of the graph data file
  character(*)                  ,parameter      ::  KEY_figure              =       'figure'                        !< Default value for file name

! ***************************
! *     FONT PARAMETERS     *
! ***************************
  character(*)                  ,parameter      ::  FontSize_Default        =       "15"                            !< Default value for file font size
  character(*)                  ,parameter      ::  FontType_Times_Roman    =       'Times-Roman'                   !< File font-type of type "Times-Roman"
  character(*)                  ,parameter      ::  FontType_Default        =       FontType_Times_Roman            !< Default value for file font type
  character(*)  ,dimension(1)   ,parameter      ::  FontType_Valid          =       [FontType_Times_Roman]
  character(*)                  ,parameter      ::  KEY_front               =       'front'
  character(*)                  ,parameter      ::  KEY_back                =       'back'
  character(*)                  ,parameter      ::  Position_Default        =       KEY_front
  character(*)  ,dimension(2)   ,parameter      ::  Position_Valid          =       [ character(5) :: KEY_front, KEY_back ]

! **************************
! *     KEY PARAMETERS     *
! **************************
  character(*)                  ,parameter      ::  KEY_key                 =       'key'
  character(*)                  ,parameter      ::  KEY_bottom_right        =       'bottom right'
  character(*)                  ,parameter      ::  KEY_right_bottom        =       'right bottom'
  character(*)                  ,parameter      ::  KEY_bottom_left         =       'bottom left'
  character(*)                  ,parameter      ::  KEY_left_bottom         =       'left bottom'
  character(*)                  ,parameter      ::  KEY_top_right           =       'top right'
  character(*)                  ,parameter      ::  KEY_right_top           =       'right top'
  character(*)                  ,parameter      ::  KEY_top_left            =       'top left'
  character(*)                  ,parameter      ::  KEY_left_top            =       'left top'
  character(*)  ,dimension(8)   ,parameter      ::  KEY_Position_Valid      =   [ character(12) ::  &               !< All possible value for legend position
                                                                KEY_bottom_right,       &
                                                                KEY_right_bottom,       &
                                                                KEY_bottom_left ,       &
                                                                KEY_left_bottom ,       &
                                                                KEY_top_right   ,       &
                                                                KEY_right_top   ,       &
                                                                KEY_top_left    ,       &
                                                                KEY_left_top            ]

! ***************************
! *     AXIS PARAMETERS     *
! ***************************
  integer                       ,parameter      ::  Axis_X                  =       1                               !< X-axis index
  integer                       ,parameter      ::  Axis_Y                  =       2                               !< Y-axis index
  integer                       ,parameter      ::  Axis_Z                  =       3                               !< Z-axis index
  character(*)  ,dimension(3)   ,parameter      ::  Axis_Type_Default       =       ['x','y','z']                   !< Default value for axis names
  integer                       ,parameter      ::  Naxis                   =       3                               !< Axis number
  character(*)                  ,parameter      ::  fmt_Range               =       '(es11.4)'                      !< Real format: "00.00E+00"
  integer                       ,parameter      ::  LenStrRge               =       11                              !< Length of the character string forming the axis range (Must be set according to the fmt_AxisRange variable)
  character(*)                  ,parameter      ::  KEY_AutoBound           =       '*'                             !< Key character corresponding to auto-determination of axis bounds

! ***************************************
! *     COLORBOX PARAMETERS     *
! ***************************************
  character(*)                  ,parameter      ::  KEY_vertical            =       'vertical'
  character(*)                  ,parameter      ::  KEY_horizontal          =       'horizontal'
  character(*)                  ,parameter      ::  Orientation_Default     =       KEY_vertical
  character(*)  ,dimension(2)   ,parameter      ::  Orientation_Valid       =       [ character(10) :: KEY_vertical, KEY_horizontal ]


! ****************************************
! *     LINESTYLE PARAMETERS     *
! ****************************************
  integer                       ,parameter      ::  i_LineStyle_Object_Unknown=     9000                            !< Starting index unit for Unknown LineStyle objects
  integer                       ,parameter      ::  i_LineStyle_Object_Line   =     1                               !< Starting index unit for Line LineStyle objects
  integer                       ,parameter      ::  i_LineStyle_Object_Grid   =     1100                            !< Starting index unit for Grid LineStyle objects
  integer                       ,parameter      ::  i_LineStyle_Object_ColorBox=    1200                            !< Starting index unit for Colorbox LineStyle objects
  character(*)                  ,parameter      ::  LineStyle_Object_Unknown=       'Unknown'                       !< Keyword for Unknown LineStyle objects
  character(*)                  ,parameter      ::  LineStyle_Object_Line   =       'line'                          !< Keyword for Line LineStyle objects
  character(*)                  ,parameter      ::  LineStyle_Object_Grid   =       'grid'                          !< Keyword for Grid LineStyle objects
  character(*)                  ,parameter      ::  LineStyle_Object_ColorBox=      'colorbox'                      !< Keyword for Colorbox LineStyle objects
  character(*)  ,dimension(3)   ,parameter      ::  LineStyle_Object_ALL    =   [   character(9) ::       &
                                                                                        LineStyle_Object_Line,    &     !< All value for LineStyle objects
                                                                                        LineStyle_Object_Grid,    &
                                                                                        LineStyle_Object_ColorBox ]


! ****************************
! *     COLOR PARAMETERS     *
! ****************************
  character(*)                  ,parameter      ::  Key_PALETTE             =       'palette'
  character(*)                  ,parameter      ::  LineStyle_Color_Default =       'black'                         !< Default value for curve LineColor
  integer                       ,parameter      ::  NTrueCol                =       20
  character(*)  ,dimension(21)  ,parameter      ::  LineStyle_Color_Valid   =    [  character(15) ::  &       !< Default value for curve LineColor
                                                                                        'black',                &
                                                                                        'blue',                 &
                                                                                        'red',                  &
                                                                                        'green',                &
                                                                                        'magenta',              &
                                                                                        'cyan',                 &
                                                                                        'brown',                &
                                                                                        'yellow',               &
                                                                                        'dark-green',           &
                                                                                        'dark-yellow',          &
                                                                                        'dark-blue',            &
                                                                                        'dark-red',             &
                                                                                        'dark-cyan',            &
                                                                                        'dark-magenta',         &
                                                                                        'dark-turquoise',       &
                                                                                        'dark-pink',            &
                                                                                        'dark-salmon',          &
                                                                                        'dark-khaki',           &
                                                                                        'light-red',            &
                                                                                        'light-blue',           &
                                                                                        Key_PALETTE             ]
  character(*)  ,dimension(NTrueCol), parameter ::  LineStyle_Color_ALL     =       LineStyle_Color_Valid(1:NTrueCol)

! ***************************
! *     LINE PARAMETERS     *
! ***************************
  character(*)                  ,parameter      ::  KEY_LineWidth           =       'lw'                            !< Keyword for "linewidth"
  character(*)                  ,parameter      ::  KEY_LineType            =       'lt'                            !< Keyword for "linetype"
  character(*)                  ,parameter      ::  KEY_LineColor           =       'lc'                            !< Keyword for "linecolor"
  character(*)                  ,parameter      ::  KEY_ls                  =       'ls'                            !< Keyword for "line style"
  character(*)                  ,parameter      ::  KEY_rgb                 =       'rgb'                           !< Keyword for "rgb"
  character(*)                  ,parameter      ::  LineTitle_Default       =       'notitle '                      !< Default title
  integer                       ,parameter      ::  LineType_Default        =       1                               !< Default value for curve LineType
  character(*)                  ,parameter      ::  Default_LineStyle_Width =       '3'                             !< Default value for curve LineWidth

! Potting type options
  character(*)                  ,parameter      ::  KEY_lines               =       'l'                             !< Curve Nature for lines curve
  character(*)                  ,parameter      ::  KEY_points              =       'p'                             !< Curve Nature for points curve
  character(*)                  ,parameter      ::  KEY_linespoints         =       'lp'                            !< Curve Nature for linespoints curve
  character(*)                  ,parameter      ::  KEY_filledcurve         =       'filledcurve'                   !< Curve Nature for filledcurve curve
  character(*)                  ,parameter      ::  KEY_pm3d                =       'pm3d'                          !< Curve Nature for pm3D curve

  character(*)                  ,parameter      ::  Line_Type_Default       =       KEY_lines                       !< Default value for curve Nature
  character(*)                  ,parameter      ::  Line_Type_LS_Comp_DEF   =       KEY_lines                       !< Default value for curve Nature
  character(*)  ,dimension(3)   ,parameter      ::  Line_Type_LS_Compatible =    [  character(20) ::  &       !< All value for Curve Nature
                                                                                        KEY_lines,              &
                                                                                        KEY_POINTS,             &
                                                                                        KEY_linesPOINTS         ]

  character(*)  ,dimension(5)   ,parameter      ::  Line_Type_ALL           =   [   character(20) ::  &       !< All value for Curve Nature
                                                                                        KEY_lines,              &
                                                                                        KEY_points,             &
                                                                                        KEY_linespoints,        &
                                                                                        KEY_filledcurve,        &
                                                                                        KEY_pm3d                ]


  character(*)                  ,parameter      ::  Plot_LineType_Default   =       KEY_lines
  character(*)  ,dimension(5)   ,parameter      ::  Plot_LineType_Valid     =   [   character(20) ::  &       !< All value for Curve Nature
                                                                                        KEY_lines,              &
                                                                                        KEY_points,             &
                                                                                        KEY_linespoints,        &
                                                                                        KEY_filledcurve,        &
                                                                                        KEY_pm3d                ]



  character(*)                  ,parameter      ::  Plot_LineStype_Default  =       KEY_LS
  character(*)  ,dimension(1)   ,parameter      ::  Plot_LineStype_Valid    =   [   character(20) ::  &
                                                                                        KEY_LS                  ]


! ***************************
! *     PLOT PARAMETERS     *
! ***************************
  character(*)                  ,parameter      ::  KEY_plot                =       'plot'
  character(*)                  ,parameter      ::  KEY_splot               =       'splot'
  character(*)                  ,parameter      ::  PlotType_DEF            =       KEY_plot                        !< Default value for PlotType

! *********************************
! *     DIRECTRORY PARAMETERS     *
! *********************************
  character(:)                  ,allocatable    ::  Directory_Name
  character(*)                  ,parameter      ::  File_Output_Directory_Default   =       ''                      !< Default value for the name of the files output directory
  character(*)                  ,parameter      ::  Image_File_Directory            =       'Image_Files'           !< Default value for the name of the image files directory
  character(*)                  ,parameter      ::  Data_File_Directory             =       'Data_Files'            !< Default value for the name of the data files directory
  character(*)                  ,parameter      ::  Command_File_Directory          =       'Command_Files'         !< Default value for the name of the command files directory

! ***************************
! *     GRID PARAMETERS     *
! ***************************
  character(*)                  ,parameter      ::  Key_Grid                        =       'grid'
  logical                       ,parameter      ::  i_Graph_Grid_Default            =       .True.


! ***************************
! *     DATA PARAMETERS     *
! ***************************
  character(*)  ,dimension(6)   ,parameter      ::  DataType_Valide         =    [ character(8) ::  &
                                                                "X1-Y1",        &
                                                                "X1-Y2",        &
                                                                "X2-Y2",        &
                                                                "X2-Y2-Z2",     &
                                                                "X2-Y2p"  ,     &
                                                                "DataBlocks"      ]
  integer       ,parameter                  ::  DataType_UNKNOWN        =       0
  integer       ,parameter                  ::  DataType_X1Y1           =       1
  integer       ,parameter                  ::  DataType_X1Y2           =       2
  integer       ,parameter                  ::  DataType_X2Y2           =       3
  integer       ,parameter                  ::  DataType_X2Y2Z2         =       4
  integer       ,parameter                  ::  DataType_X2Y2p          =       5
  integer       ,parameter                  ::  DataType_DataBlocks       =       6

End Module