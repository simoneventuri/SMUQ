Module GPF_Axis_Range_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_Class                 ,only:  GPF

  implicit none

  private

  public  ::  GPF_Axis_Range_Type

! ==============================================================================================================
!   COMMAND DESCRIPTION: RANGE
! ==============================================================================================================
! The set xrange command sets the horizontal range that will be displayed. A similar command exists for
! each of the other axes, as well as for the polar radius r and the parametric variables t, u, and v.
! Syntax:
!       set xrange { [{{<min>}:{<max>}}] {{no}reverse} {{no}writeback} }
!                  | restore
!       show xrange
! where <min> and <max> terms are constants, expressions or an asterisk to set autoscaling. See below for
! full autoscaling syntax. If the data are time/date, you must give the range as a quoted string according to
! the set timefmt format. Any value omitted will not be changed.
! The same syntax applies to yrange, zrange, x2range, y2range, cbrange, rrange, trange, urange and vrange.
! The reverse option reverses the direction of the axis, e.g., set xrange [0:1] reverse will produce an axis
! with 1 on the left and 0 on the right. This is identical to the axis produced by set xrange [1:0], of course.
! reverse is intended primarily for use with autoscale.
! Autoscaling: If <min> (the same applies for correspondingly to <max>) is an asterisk "*" autoscaling is
! turned on. The range in which autoscaling is being performed may be limited by a lower bound <lb> or an
! upper bound <ub> or both. The syntax is
!       { <lb> < } * { < <ub> }
! For example,
!       0 < * < 200
! sets <lb> = 0 and <ub> = 200. With such a setting <min> would be autoscaled, but its final value will
! be between 0 and 200 (both inclusive despite the ’<’ sign). If no lower or upper bound is specified, the ’<’
! to also be ommited. If <ub> is lower than <lb> the constraints will be turned off and full autoscaling will
! happen. This feature is useful to plot measured data with autoscaling but providing a limit on the range, to
! clip outliers, or to guarantee a minimum range that will be displayed even if the data would not need such
! a big range.
! The writeback option essentially saves the range found by autoscale in the buffers that would be filled
! by set xrange. This is useful if you wish to plot several functions together but have the range determined
! by only some of them. The writeback operation is performed during the plot execution, so it must be
! specified before that command. To restore, the last saved horizontal range use set xrange restore. For
! example,
!       set xrange [-10:10]
!       set yrange [] writeback
!       plot sin(x)
!       set yrange restore
!       replot x/2
!  in a yrange of [-1:1] as found only from the range of sin(x); the [-5:5] range of x/2 is ignored. Executing
! show yrange after each command in the above example should help you understand what is going on.
! In 2D, xrange and yrange determine the extent of the axes, trange determines the range of the parametric
! variable in parametric mode or the range of the angle in polar mode. Similarly in parametric 3D, xrange,
! yrange, and zrange govern the axes and urange and vrange govern the parametric variables.
! In polar mode, rrange determines the radial range plotted. <rmin> acts as an additive constant to the
! radius, whereas <rmax> acts as a clip to the radius — no point with radius greater than <rmax> will be
! plotted. xrange and yrange are affected — the ranges can be set as if the graph was of r(t)-rmin, with
! rmin added to all the labels.
! Any range may be partially or totally autoscaled, although it may not make sense to autoscale a parametric
! variable unless it is plotted with data.
! Ranges may also be specified on the plot command line. A range given on the plot line will be used for
! that single plot command; a range given by a set command will be used for all subsequent plots that do
! not specify their own ranges. The same holds true for splot.
! Examples:
! To set the xrange to the default:
!       set xrange [-10:10]
! To set the yrange to increase downwards:
!       set yrange [10:-10]
! To change zmax to 10 without affecting zmin (which may still be autoscaled):
!       set zrange [:10]
! To autoscale xmin while leaving xmax unchanged:
!       set xrange [*:]
! To autoscale xmin but keeping xmin positive:
!       set xrange [0<*:]
! To autoscale x but keep minimum range of 10 to 50 (actual might be larger):
!       set xrange [*<10:50<*]
! Autoscaling but limit maximum xrange to -1000 to 1000, i.e. autoscaling within [-1000:1000]
!       set xrange [-1000<*:*<1000]
! Make sure xmin is somewhere between -200 and 100:
!       set xrange [-200<*<100:]
! ==============================================================================================================

  Type  ,extends(GPF_Command_Type) ::  GPF_Axis_Range_Type
    character(:)        ,allocatable                    ::  Bound_Inf                                       !< Character string corresponding to the inferior bound of current axis
    character(:)        ,allocatable                    ::  Bound_Sup                                       !< Character string corresponding to the superior bound of current axis
    character(:)        ,allocatable                    ::  Value                                           !< Character string corresponding to the range value
    character(:)        ,allocatable                    ::  Reverse                                         !< Character string corresponding to the reversing indicator
  contains
    private
    procedure   ,public   ::  Update          =>  Update_Range                                    !< Updates the object components
    procedure             ::  Initialize      =>  Initialize_Range                                !< Initializes the object components
    procedure             ::  Set_Bound_Inf   =>  Set_Range_Bound_Inf                             !< Sets the inferior bound of current axis
    procedure             ::  Set_Bound_Sup   =>  Set_Range_Bound_Sup                             !< Sets the superior bound of current axis
    procedure             ::  Set_Value       =>  Set_Range_Value                                 !< Sets the range value
    procedure             ::  Set_Reverse     =>  Set_Range_Reverse                               !< Sets the reversing indicator of current axis
    procedure   ,public   ::  Set_Command     =>  Set_Range_Command                               !< Sets the range command
  End Type

  Interface             GPF_Axis_Range_Type
    Module Procedure    Construct_Axis_Range
  End Interface

  character(*)  ,parameter      ::  AutoScaling='*'

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Axis_Range( AxisName, Data_Min, Data_Max, Axis_Min, Axis_Max, Reverse, Debug ) result(This)

  implicit none

  type(GPF_Axis_Range_Type)                                             ::  This                            !< Axis-Range object to be constructed
  character(*)                                          ,intent(in)     ::  AxisName                        !< Axis name
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Min                        !< Data minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Max                        !< Data maximum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Axis_Min                        !< Axis minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Axis_Max                        !< Axis maximum bound
  logical                                     ,optional ,intent(in)     ::  Reverse                         !< Axis reversing indicator
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting debugging indicator
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Range]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Range]: Calling This%Initialize')")
  call This%Initialize()                                                                                        ! Initializing the object components to default values

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Range]: Calling This%Set_Keyword')")
  call This%Set_Keyword( AxisName, i_Trim=.True. )                                                              ! Setting the axis name in the Keyword component

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Range]: Calling This%Set_Bound_Inf')")
  call This%Set_Bound_Inf( Axis_Min, Data_Max )                                                                 ! Setting the inferior bound of current axis

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Range]: Calling This%Set_Bound_Sup')")
  call This%Set_Bound_Sup( Axis_Max, Data_Min )                                                                 ! Setting the superior bound of current axis

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Range]: Calling This%Set_Value')")
  call This%Set_Value()                                                                                         ! Setting the range value

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Range]: Calling This%Set_Reverse')")
  call This%Set_Reverse( Reverse )                                                                              ! Setting the range reversing indicator

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Range]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the axis command (actually, it is just a comment)

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Range]: Exiting')")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Update_Range( This, AxisName, Data_Min, Data_Max, Axis_Min, Axis_Max, Reverse, Debug )

  implicit none

  class(GPF_Axis_Range_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Reverse object
  character(*)                                ,optional ,intent(in)     ::  AxisName                        !< Axis name
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Min                        !< Data minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Max                        !< Data maximum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Axis_Min                        !< Axis minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Axis_Max                        !< Axis maximum bound
  logical                                     ,optional ,intent(in)     ::  Reverse                         !< Axis reversing indicator
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Range]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Range]: Calling This%Set_Keyword')")
  call This%Set_Keyword( AxisName, i_Trim=.True. )                                                              ! Setting the axis name in the Keyword component

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Range]: Calling This%Set_Bound_Inf')")
  call This%Set_Bound_Inf( Axis_Min, Data_Max )                                                                 ! Setting the inferior bound of current axis

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Range]: Calling This%Set_Bound_Sup')")
  call This%Set_Bound_Sup( Axis_Max, Data_Min )                                                                 ! Setting the superior bound of current axis

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Range]: Calling This%Set_Value')")
  call This%Set_Value()                                                                                         ! Setting the range value

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Range]: Calling This%Set_Reverse')")
  call This%Set_Reverse( Reverse )                                                                              ! Setting the range reversing indicator

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Range]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the axis command

  if (This%i_Debug) write(DbgUnit,"(18x,'[Update_Range]: Exiting',/)")

End Subroutine

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Initialize_Range( This )
  implicit none
  class(GPF_Axis_Range_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Range object
  if (This%i_Debug) write(DbgUnit,"(16x,'[Initialize_Range]: Entering')")
  This%Bound_Inf        =       AutoScaling                                                                     ! Initializing to an empty string: default value => Auto-scaling
  This%Bound_Sup        =       AutoScaling                                                                     ! Initializing to an empty string: default value => Auto-scaling
  This%Value            =       ''                                                                              ! Initializing to an empty string: default value => Auto-scaling
  This%Reverse          =       ''                                                                              ! Initializing to an empty string: default value => No reversing
  if (This%i_Debug) write(DbgUnit,"(16x,'[Initialize_Range]: Exiting',/)")
End Subroutine

Subroutine Set_Range_Bound_Inf( This, Axis_Min, Data_Max )
  implicit none
  class(GPF_Axis_Range_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Range object
  real(rkp)                                   ,optional ,intent(in)     ::  Axis_Min                        !< Axis minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Max                        !< Data maximum bound
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Range_Bound_Inf]: Entering')")
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Range_Bound_Inf]: present(Axis_Min) = ',l3)") present(Axis_Min)
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Range_Bound_Inf]: present(Data_Max) = ',l3)") present(Data_Max)
  if ( present(Axis_Min) ) then                                                                                 ! If present optional argument for current axis bound
!     This%Bound_Inf       =       Get_Bound_Inf( Axis_Min, Data_Max )                                                 ! Setting current bound to input value or to the automicatic bound if not consistent with the data
    This%Bound_Inf       =       Get_Bound_Inf( Axis_Min )     ! Required in order to imposed a lower bound (it can be usefull for movies for example)                                             ! Setting current bound to input value or to the automicatic bound if not consistent with the data
  else                                                                                                          ! If absent optional argument for current axis bound
    select case (This%Keyword)                                                                                  ! Selecting the name of current axis in order to set default value for axis bound
      case ('x');  if (GPF%Default%i_X_Min)  This%Bound_Inf = Get_Bound_Inf( GPF%Default%X_Min,  Data_Max )          ! Case of the X-axis
    end select                                                                                                  ! End of axis name selection
  end if                                                                                                        ! End of if case on optional argument presence
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_Range_Bound_Inf]: This%Bound_Inf = ',a)") This%Bound_Inf
    write(DbgUnit,"(16x,'[Set_Range_Bound_Inf]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Range_Bound_Sup( This, Axis_Max, Data_Min )
  implicit none
  class(GPF_Axis_Range_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Range object
  real(rkp)                                   ,optional ,intent(in)     ::  Axis_Max                        !< Axis maximum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Min                        !< Data minimum bound
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Range_Bound_Sup]: Entering')")
  if ( present(Axis_Max) ) then                                                                                 ! If present optional argument for current axis bound
!     This%Bound_Sup       =       Get_Bound_Sup( Axis_Max, Data_Min )                                            ! Setting current bound to input value or to the automicatic bound if not consistent with the data
    This%Bound_Sup       =       Get_Bound_Sup( Axis_Max )                                            ! Setting current bound to input value or to the automicatic bound if not consistent with the data
  else                                                                                                          ! If absent optional argument for current axis bound
    select case (This%Keyword)                                                                                  ! Selecting the name of current axis in order to set default value for axis bound
      case ('x');  if (GPF%Default%i_X_Max)  This%Bound_Sup = Get_Bound_Sup( GPF%Default%X_Max,  Data_Min )     ! Case of the X-axis
    end select                                                                                                  ! End of axis name selection
  end if                                                                                                        ! End of if case on optional argument presence
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_Range_Bound_Sup]: This%Bound_Sup = ',a)") This%Bound_Sup
    write(DbgUnit,"(16x,'[Set_Range_Bound_Sup]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Range_Value( This )
  implicit none
  class(GPF_Axis_Range_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Range object
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Range_Value]: Entering')")
  if ( This%Bound_Inf == This%Bound_Sup ) then                                                                  ! If the lower and upper axis bound are equal
    This%Bound_Inf      =       AutoScaling
    This%Bound_Sup      =       AutoScaling
  end if                                                                                                        ! End of if case on lower and upper axis bound values
  This%Value            =       '[' // This%Bound_Inf // ':' // This%Bound_Sup // ']'                           ! Setting the Axis range using gnuplot format
  if ( len_trim(This%Value) /= 0 ) This%Value = 'range ' // This%Value // ' '                                   ! If the axis range character is not an empty string, then setting the axis ranges using the gnuplot format
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_Range_Value]: This%Value   = ',a )") This%Value
    write(DbgUnit,"(16x,'[Set_Range_Value]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Range_Reverse( This, Reverse )
  implicit none
  class(GPF_Axis_Range_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Range object
  logical                                     ,optional ,intent(in)     ::  Reverse                         !< Axis reversing indicator
  if (This%i_Debug) write(DbgUnit,"(20x,'[Set_Range_Reverse]: Entering')")
  if ( present(Reverse) ) then                                                                                  ! If present input optional arguments
    if (Reverse) This%Reverse = 'reverse '                                                                      ! Setting the reverse keyword if axis reversing
  end if                                                                                                        ! End if case on optional argument presence
  if (This%i_Debug) then
    write(DbgUnit,"(20x,'[Set_Range_Reverse]: This%Reverse = ',a)") This%Reverse
    write(DbgUnit,"(20x,'[Set_Range_Reverse]: Exiting',/)")
  end if
End Subroutine

Function Get_Bound_Inf( Axis_Min, Data_Max ) result(Bound_Inf)
  use GPF_Tools                 ,only:  Convert_To_String
  implicit none
  character(:)  ,allocatable                                            ::  Bound_Inf                       !< Character string corresponding to the inferior bound of current axis
  real(rkp)                                             ,intent(in)     ::  Axis_Min                        !< Axis minimum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Max                        !< Data maximum bound
  Bound_Inf     =       Convert_To_String( Axis_Min )                                                           ! Converting the real variable into a character string
  if ( present(Data_Max) ) then                                                                                 ! If present input optional argument
    if ( Axis_Min > Data_Max ) Bound_Inf = AutoScaling                                                          ! If inferior bound greater than the maximum value in the data, then data out of plotting region and so setting the automatic bound determination
  end if                                                                                                        ! End of if case on input optional argument presence
End Function

Function Get_Bound_Sup( Axis_Max, Data_Min ) result(Bound_Sup)
  use GPF_Tools                 ,only:  Convert_To_String
  implicit none
  character(:)  ,allocatable                                            ::  Bound_Sup                       !< Character string corresponding to the superior bound of current axis
  real(rkp)                                             ,intent(in)     ::  Axis_Max                        !< Axis maximum bound
  real(rkp)                                   ,optional ,intent(in)     ::  Data_Min                        !< Data minium bound
  Bound_Sup     =       Convert_To_String( Axis_Max )                                                           ! Converting the real variable into a character string
  if ( present(Data_Min) ) then                                                                                 ! If present input optional argument
    if ( Axis_Max < Data_Min ) Bound_Sup = AutoScaling                                                          ! If superior bound greater than the minium value in the data, then data out of plotting region and so setting the automatic bound determination
  end if                                                                                                        ! End of if case on input optional argument presence
End Function

Subroutine Set_Range_Command( This )
  implicit none
  class(GPF_Axis_Range_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Range object
  if (This%i_Debug) write(DbgUnit,"(20x,'[Set_Range_Command]: Entering')")
  if ( len_trim(This%Value) /= 0 ) then
    This%Command        =       'set ' // This%Keyword // This%Value                                            ! If the axis range character is not an empty string, then setting the axis ranges using the gnuplot format
    if ( len_trim(This%Reverse) /= 0 ) This%Command = This%Command // This%Reverse
  end if
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator
  if (This%i_Debug) then
    write(DbgUnit,"(20x,'[Set_Range_Command]: This%Command  = ',a )") This%Command
    write(DbgUnit,"(20x,'[Set_Range_Command]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(20x,'[Set_Range_Command]: Exiting',/)")
  end if
End Subroutine

End Module