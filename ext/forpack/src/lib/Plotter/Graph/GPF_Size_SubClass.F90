
! ==============================================================================================================
!   THE SIZE COMMAND
! ==============================================================================================================
! Syntax:
! set size {{no}square | ratio <r> | noratio} {<xscale>,<yscale>}
! show size
! The <xscale> and <yscale> values are scale factors for the size of the plot, which includes the graph, labels, and margins.
! In earlier versions of gnuplot, some terminal types used the values from ‘set size‘ to control also the size of the output canvas; others did not.
! In version 4.6 almost all terminals now follow the following convention:
! set term <terminal type> size <XX>, <YY> controls the size of the output file, or canvas.
! Please see individual terminal documentation for allowed values of the size parameters.
! By default, the plot will fill this canvas.
! set size <XX>, <YY> scales the plot itself relative to the size of the canvas. Scale values less than 1 will
! cause the plot to not fill the entire canvas. Scale values larger than 1 will cause only a portion of the plot
! to fit on the canvas. Please be aware that setting scale values larger than 1 may cause problems on some
! terminal types.
! ratio causes gnuplot to try to create a graph with an aspect ratio of <r> (the ratio of the y-axis length to
! the x-axis length) within the portion of the plot specified by <xscale> and <yscale>.
! The meaning of a negative value for <r> is different. If <r>=-1, gnuplot tries to set the scales so that the
! unit has the same length on both the x and y axes. This is equivalent to set view equal xy. See set view
! equal (p. 156). If <r>=-2, the unit on y has twice the length of the unit on x, and so on.
! The success of gnuplot in producing the requested aspect ratio depends on the terminal selected. The graph
! area will be the largest rectangle of aspect ratio <r> that will fit into the specified portion of the output
! (leaving adequate margins, of course).
! square is a synonym for ratio 1.
! Both noratio and nosquare return the graph to the default aspect ratio of the terminal, but do not return
! <xscale> or <yscale> to their default values (1.0).
! ratio and square have no effect on 3D plots, but do affect 3D projections created using set view map.
! See also set view equal (p. 156), which forces the x and y axes of a 3D onto the same scale.
! Examples:
! To set the size so that the plot fills the available canvas:
! set size 1,1
! To make the graph half size and square use:
! set size square 0.5,0.5
! To make the graph twice as high as wide use:
! set size ratio 2
! ==============================================================================================================
SubModule(GPF_Size_Class) GPF_Size_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure InitializeSize

  character(*)                                              ,parameter  ::  ProcName = "InitializeSize"
  character(*)                                              ,parameter  ::  Keyword='size'
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling SetSizeRatio" )
  call SetSizeRatio( This, Ratio, i_Isometric )

  if (Dbg) call Logger%Write( "Calling SetSizeScales" )
  call SetSizeScales( This, X_Scale, Y_Scale, Scales )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Keyword )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Write( "Command = ", trim(This%Command) )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetSizeCommand
  This%Command          =       ''
  if ( len_trim(This%Ratio )  /= 0 )    This%Command = This%Command // This%Ratio
  if ( len_trim(This%Scales)  /= 0 )    This%Command = This%Command // This%Scales
  if ( len_trim(This%Command) /= 0 )    This%Command = 'set ' // This%Keyword // This%Command
  This%Presence         =       ( len_trim(This%Command) /= 0 )
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************
! REMARK:
! The Ratio optional input argument can be either "nosquare", "square", "ratio <r>" or "noratio" where <r>
! correspond to a real number.
! The i_Isometric optional input argument is a shortcut for "ratio -1"

Subroutine SetSizeRatio( This, Ratio, i_Isometric )
  type(GPF_Size_Type)                                   ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Size object
  character(*)                                ,optional ,intent(in)     ::  Ratio                           !< Ratio of the graph
  logical                                     ,optional ,intent(in)     ::  i_Isometric                     !< Isometric ratio indicator (equal unit length for the x/y-axis)
  This%Ratio    =       ''                                                                                      ! Initialisation to an empty string: default value
  if ( present(Ratio) ) This%Ratio = Ratio                                                                      ! If present optional input argument, then setting the graph ratio
  if ( present(i_Isometric) ) then                                                                              ! If present optional input argument
    if (i_Isometric) This%Ratio = "ratio -1"
  end if                                                                                                        ! End if case on optional input argument presence
  if ( len_trim(This%Ratio) /= 0 ) This%Ratio = This%Ratio // " "                                               ! Adding an extra space at the end of the string
End Subroutine

! REMARK:
! The Scales optional input argument must be a string with "<xscale>,<yscale>" where <xscale> and <yscale> are
! real numbers. If the Scales optional input argument is present, then the associated component is set to its value.
! If it is absent, then the code looks for the presence of the X_Scale and Y_Scale optional input arguments.
! If none of X_Scale and Y_Scale are present, then no scale is defined for the graph and the associated component
! is set to an empty string. If X_Scale or Y_Scale is present, then the graph scales are set by setting the absent
! argument for X_Scale or Y_Scale to one.
!
! @TODO: Check that the optional input argument corresponds to valid scale strings.

Subroutine SetSizeScales( This, X_Scale, Y_Scale, Scales )
  type(GPF_Size_Type)                                   ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Size object
  character(*)                                ,optional ,intent(in)     ::  X_Scale                         !< X scale
  character(*)                                ,optional ,intent(in)     ::  Y_Scale                         !< Y scale
  character(*)                                ,optional ,intent(in)     ::  Scales                          !< Scales
  character(:)  ,allocatable                                            ::  X_Scale_Loc                     ! Local value for the graph X-scale
  character(:)  ,allocatable                                            ::  Y_Scale_Loc                     ! Local value for the graph Y-scale
  This%Scales   =       ''                                                                                      ! Initialisation to an empty string: default value
  if ( present(Scales) ) then                                                                                   ! If present "Scales" optional input argument
    This%Scales =       Scales                                                                                  ! Then setting the graph scales
  else                                                                                                          ! If absent "Scales" optional input argument, then checking if either the "X_Scale" or "Y_Scale" optional input arguments are present
    if ( present(X_Scale) .or. present(Y_Scale) ) then                                                          ! If either the "X_Scale" or "Y_Scale" optional input arguments are present, then setting the graph scales according to input values
      X_Scale_Loc       =       "1"                                                                             ! Initializing the local X-scale to one
      Y_Scale_Loc       =       "1"                                                                             ! Initializing the local Y-scale to one
      if ( present(X_Scale) ) X_Scale_Loc = X_Scale                                                             ! Setting the local X-scale to the optional input value if present
      if ( present(Y_Scale) ) Y_Scale_Loc = Y_Scale                                                             ! Setting the local Y-scale to the optional input value if present
      This%Scales       =       X_Scale_Loc // ',' // Y_Scale_Loc                                               ! Setting the graph scales according to input values
    end if                                                                                                      ! End if case on X/Y scales optional argument presence
  end if                                                                                                        ! End if case on scales optional argument presence
  if ( len_trim(This%Scales) /= 0 ) This%Scales = This%Scales // " "                                            ! Adding an extra space at the end of the string
End Subroutine

End SubModule