Module GPF_View_Class

  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private
  public  ::  GPF_View_Type

! ==============================================================================================================
!   THE VIEW COMMAND
! ==============================================================================================================
! The set view command sets the viewing angle for splots. It controls how the 3D coordinates of the plot are
! mapped into the 2D screen space. It provides controls for both rotation and scaling of the plotted data, but
! supports orthographic projections only. It supports both 3D projection or orthogonal 2D projection into a
! 2D plot-like map.
! Syntax:
! set view <rot_x>{,{<rot_z>}{,{<scale>}{,<scale_z>}}}
! set view map
! set view {no}equal {xy|xyz}
! show view
! where <rot x> and <rot z> control the rotation angles (in degrees) in a virtual 3D coordinate system
! aligned with the screen such that initially (that is, before the rotations are performed) the screen horizontal
! axis is x, screen vertical axis is y, and the axis perpendicular to the screen is z. The first rotation applied is
! <rot x> around the x axis. The second rotation applied is <rot z> around the new z axis.
! Command set view map is used to represent the drawing as a map. It can be used for contour plots, or
! for color pm3d maps. In the latter, take care that you properly use zrange and cbrange for input data
! point filtering and color range scaling, respectively.
! <rot x> is bounded to the [0:180] range with a default of 60 degrees, while <rot z> is bounded to the [0:360]
! range with a default of 30 degrees. <scale> controls the scaling of the entire splot, while <scale z> scales
! the z axis only. Both scales default to 1.0.
! Examples:
! set view 60, 30, 1, 1
! set view ,,0.5
! The first sets all the four default values. The second changes only scale, to 0.5.
! The command set view equal xy forces the unit length of the x and y axes to be on the same scale, and
! chooses that scale so that the plot will fit on the page. The command set view equal xyz additionally sets
! the z axis scale to match the x and y axes; however there is no guarantee that the current z axis range will
! fit within the plot boundary. By default all three axes are scaled independently to fill the available area.
! ==============================================================================================================

  Type  ,extends(GPF_Command_Type) ::  GPF_View_Type
    private
    character(:)  ,allocatable  ::  Value                                           !< Viewing angles and scales
    character(:)  ,allocatable  ::  Map                                             !< Mapping view string
    character(:)  ,allocatable  ::  Equal                                           !< Indicator of equal unit length axis
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeView
    procedure   ,public   ::  Set_Command   =>  SetViewCommand
  End Type

  Interface
    Module Subroutine InitializeView( This, Rot_x, Rot_z, Scale, Scale_z, i_Map, i_Equal_xy, i_Equal_xyz, Debug )
      class(GPF_View_Type)                                  ,intent(out)    ::  This                            !< View object to be constructed
      character(*)                                ,optional ,intent(in)     ::  Rot_x                           !< Viewing angle around the x-axis
      character(*)                                ,optional ,intent(in)     ::  Rot_z                           !< Viewing angle around the z-axis
      character(*)                                ,optional ,intent(in)     ::  Scale                           !< Y scale
      character(*)                                ,optional ,intent(in)     ::  Scale_z                         !< Scales
      logical                                     ,optional ,intent(in)     ::  i_Map                           !< Map view indicator
      logical                                     ,optional ,intent(in)     ::  i_Equal_xy                      !< Indicator of equal unit length for the x/y axis
      logical                                     ,optional ,intent(in)     ::  i_Equal_xyz                     !< Indicator of equal unit length for the x/y/z axis
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine SetViewCommand( This )
      class(GPF_View_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the View object
    End Subroutine

  End Interface

End Module