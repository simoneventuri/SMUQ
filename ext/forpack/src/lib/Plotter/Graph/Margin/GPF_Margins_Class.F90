Module GPF_Margins_Class

  use GPF_Command_Class       ,only:  GPF_Command_Type
  use GPF_Margin_Class        ,only:  GPF_Margin_Type

  implicit none

! ==============================================================================================================
!   THE MARGIN COMMAND
! ==============================================================================================================
! The margin is the distance between the plot border and the outer edge of the canvas. The size of the margin
! is chosen automatically, but can be overridden by the set margin commands. show margin shows the
! current settings. To alter the distance between the inside of the plot border and the data in the plot itself,
! see set offsets.
! Syntax:
! set bmargin {{at screen} <margin>}
! set lmargin {{at screen} <margin>}
! set rmargin {{at screen} <margin>}
! set tmargin {{at screen} <margin>}
! show margin
!
! The default units of <margin> are character heights or widths, as appropriate. A positive value defines the
! absolute size of the margin. A negative value (or none) causes gnuplot to revert to the computed value.
! For 3D plots, only the left margin can be set using character units.
! The keywords at screen indicates that the margin is specified as a fraction of the full drawing area. This
! can be used to precisely line up the corners of individual 2D and 3D graphs in a multiplot. This placement
! ignores the current values of set origin and set size, and is intended as an alternative method for positioning
! graphs within a multiplot.
! Normally the margins of a plot are automatically calculated based on tics, tic labels, axis labels, the plot
! title, the timestamp and the size of the key if it is outside the borders. If, however, tics are attached to the
! axes (set xtics axis, for example), neither the tics themselves nor their labels will be included in either the
! margin calculation or the calculation of the positions of other text to be written in the margin. This can
! lead to tic labels overwriting other text if the axis is very close to the border.
! ==============================================================================================================

  private
  public  ::  GPF_Margins_Type

  Type  ,extends(GPF_Command_Type) ::  GPF_Margins_Type
    private
    type(GPF_Margin_Type) ::  Top                                             !< Top margin
    type(GPF_Margin_Type) ::  Bottom                                          !< Bottom margin
    type(GPF_Margin_Type) ::  Left                                            !< Left margin
    type(GPF_Margin_Type) ::  Right                                           !< Right margin
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeMargins
    procedure   ,public   ::  Write         =>  WriteMargins
    procedure   ,public   ::  Set_Command   =>  SetMarginsCommand
  End Type

  Interface

    Module Subroutine InitializeMargins( This, Top, Bottom, Left, Right, Debug )
      class(GPF_Margins_Type)                               ,intent(out)    ::  This
      character(*)                                ,optional ,intent(in)     ::  Top                             !< Top margin coordinates
      character(*)                                ,optional ,intent(in)     ::  Bottom                          !< Bottom margin coordinates
      character(*)                                ,optional ,intent(in)     ::  Left                            !< Left margin coordinates
      character(*)                                ,optional ,intent(in)     ::  Right                           !< Right margin coordinates
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine WriteMargins( This, Unit )
      class(GPF_Margins_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Margin object whose command need to be written
      integer                                               ,intent(in)     ::  Unit                            !< File unit number
    End Subroutine

    Module Subroutine SetMarginsCommand( This )
      class(GPF_Margins_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Margin object
    End Subroutine

  End Interface

End Module