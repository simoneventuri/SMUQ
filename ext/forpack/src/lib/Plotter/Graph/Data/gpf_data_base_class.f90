Module GPF_Data_Base_Class

  use GPF_Parameters            ,only:  rkp
  use GPF_Parameters            ,only:  DbgUnit

  implicit none

  private
  public  ::  GPF_Data_Base_Type

! ****************************************************************************************************
! @TODO: Store in a parameter module
! ****************************************************************************************************
!   character(*)                          ,parameter                      ::  Format_real_data = "es24.16e3"
  character(*)                          ,parameter                      ::  Format_real_data = "es20.12e3"
  integer                               ,parameter                      ::  Length_Real=24
! ****************************************************************************************************

!
!   Type                                                  ::  GPF_Curve_Data_Type
!
!   End Type


  Type  ,abstract                                       ::  GPF_Data_Base_Type

    logical     ,public                                 ::  i_Debug         =       .False.                 !< Debugging indicator
    character(:)        ,allocatable                    ::  PlotType
    integer                                             ::  NLine   =       0
    integer                                             ::  NPoint  =       0
    integer                                             ::  NAbsci  =       0       ! @TODO: Change the variable "NAbsci" by "NXAxis"
    integer                                             ::  NXAxis  =       0                               !< Number of X-axis
    integer                                             ::  NYAxis  =       0                               !< Number of Y-axis
    integer                                             ::  NZAxis  =       0                               !< Number of Z-axis
    integer                                             ::  NAxes   =       0                               !< Number of axes
    integer                                             ::  X_Dim   =       0
    integer                                             ::  Y_Dim   =       0
    integer                                             ::  Z_Dim   =       0
!       @SPARK_BUG Graph%NColumns is wrong for the XYZ plot since it is equal to 2 instead of 3
!       @SPARK_BUG Graph%Format is wrong for the XYZ plot since it is has 2 column instead of 3
    integer                                             ::  NColumns
    character(:)        ,dimension(:)   ,allocatable    ::  Column_Label
    character(:)        ,allocatable                    ::  Format
  contains
    procedure   ,public   ::  Set_Debug                                       !< Setting debugging indicator
    procedure   ,public   ::  Set_Columns_Label

    procedure   ,public   ::  Get_NLine                                       !< Gets the number of lines
    procedure   ,public   ::  Get_NAbsci                                      !< Gets the number of abscisses
    procedure   ,public   ::  Get_NAxes                                       !< Gets the number of axes
    procedure   ,public   ::  Get_PlotType                                    !< Gets the plot type

    procedure   ,public   ::  Write_Header
    procedure   ,public   ::  Set_Format      =>  Set_Data_Format
    procedure   ,public   ::  Get_X_Min
    procedure   ,public   ::  Get_X_Max
    procedure   ,public   ::  Get_Y_Min
    procedure   ,public   ::  Get_Y_Max
    procedure   ,public   ::  Get_Z_Min
    procedure   ,public   ::  Get_Z_Max
!     procedure   ,public   ::  Get_X_Min
!     procedure   ,public   ::  Get_X_Max
!     procedure(Get_R1D)                  ,deferred       ::  Get_Y_Min
!     procedure(Get_R1D)                  ,deferred       ::  Get_Y_Max
!     procedure(Get_R1D)                  ,deferred       ::  Get_Z_Min
!     procedure(Get_R1D)                  ,deferred       ::  Get_Z_Max
    procedure(Setter_NColumns)          ,deferred       ::  Set_NColumns
    procedure(Writing_Interface)       ,deferred       ::  Write
  End Type

  Abstract Interface
    Subroutine Writing_Interface( This, Unit )
      import                    ::  GPF_Data_Base_Type
      class(GPF_Data_Base_Type)         ,intent(in)     ::  This
      integer                           ,intent(in)     ::  Unit
    End Subroutine
    Subroutine Setter_NColumns( This )
      import                    ::  GPF_Data_Base_Type
      class(GPF_Data_Base_Type)         ,intent(inout)  ::  This
    End Subroutine
!     Function Get_R1D( This ) result( R1D )
!       use GPF_Parameters        ,only:  rkp
!       import                    ::  GPF_Data_Base_Type
!       class(GPF_Data_Base_Type)         ,intent(in)     ::  This
!       real(rkp)                                         ::  R1D
!     End Function
  End Interface

  contains

Subroutine Set_Debug( This, Debug )
  use GPF_Parameters            ,only:  i_Debug_Default
  class(GPF_Data_Base_Type)             ,intent(inout)  ::  This                                            !< Derived-type structure
  logical                     ,optional ,intent(in)     ::  Debug                                           !< Derived-type debugging indicator
  This%i_Debug = i_Debug_Default                                                                                ! Setting debugging indicator to default value
  if ( present(Debug) ) This%i_Debug = Debug                                                                    ! If present optional input argument, then setting debugging indicator to input value
End Subroutine

Function Get_NLine( This ) result (NLine)
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Graph-Data object
  integer                                                               ::  NLine                           !< Number of lines
  NLine         =       This%NLine                                                                              ! Getting the number of lines
End Function

Function Get_NAbsci( This ) result (NAbsci)
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Graph-Data object
  integer                                                               ::  NAbsci                          !< Number of x-axis
  NAbsci        =       This%NAbsci                                                                             ! Getting the number of x-axis
End Function

Function Get_NAxes( This ) result (NAxes)
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Graph-Data object
  integer                                                               ::  NAxes                           !< Number of axes
  NAxes        =       This%NAxes                                                                               ! Getting the number of axes
End Function

Function Get_PlotType( This ) result (PlotType)
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Graph-Data object
  character(:)          ,allocatable                                    ::  PlotType                        !< Character string corresponding to the type of plot (the plotting function)
  PlotType       =       This%PlotType                                                                          ! Getting the plotting function
End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *             PROCEDURES FOR COMPUTING THE MINIMUM AND MAXIMUM VALUE OF THE X, Y AND Z DATA                   *
! **************************************************************************************************************
! **************************************************************************************************************

Function Get_X_Min( This ) result( X_Min )
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  X_Min                           !< Minimum value for X
  X_Min         =       0.0_rkp                                                                                 ! Initializing the minimum X-value to zero
End Function

Function Get_X_Max( This ) result( X_Max )
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  X_Max                           !< Maximum value for X
  X_Max         =       0.0_rkp                                                                                 ! Initializing the maximum X-value to zero
End Function

Function Get_Y_Min( This ) result( Y_Min )
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Y_Min                           !< Minimum value for Y
  Y_Min         =       0.0_rkp                                                                                 ! Initializing the minimum Y-value to zero
End Function

Function Get_Y_Max( This ) result( Y_Max )
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Y_Max                           !< Maximum value for Y
  Y_Max         =       0.0_rkp                                                                                 ! Initializing the maximum Y-value to zero
End Function

Function Get_Z_Min( This ) result( Z_Min )
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Z_Min                           !< Minimum value for Z
  Z_Min         =       0.0_rkp                                                                                 ! Initializing the minimum Z-value to zero
End Function

Function Get_Z_Max( This ) result( Z_Max )
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)                                                             ::  Z_Max                           !< Maximum value for Z
  Z_Max         =       0.0_rkp                                                                                 ! Initializing the maximum Z-value to zero
End Function




Subroutine Set_Data_Format( This )
  use GPF_Tools                 ,only:  Convert_To_String
  class(GPF_Data_Base_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Graph-Data object
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Data_Format]: Entering')")
  This%Format      =       "(" // Convert_To_String(This%NColumns) // "(3x,"// Format_real_data // "))"         ! Setting the data format
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Data_Format]: This%Format = ',a)") This%Format
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Data_Format]: Exiting')")
End Subroutine

!       @SPARK_BUG Graph%NColumn is wrong for the XYZ plot since it is equal to 2 instead of 3
!       @SPARK_BUG Graph%Format is wrong for the XYZ plot since it is has 2 column instead of 3



! ! ****************************************************************************************************************************
! ! FOR SPLOT
! ! ****************************************************************************************************************************
!     if ( allocated(ColumnLabels) ) deallocate(ColumnLabels)
!     allocate( ColumnLabels, source = ['X','Y','Z'] )
!     if (i_Debug_Loc) write(DbgUnit,"(6x,'[Write_Data_Splot]: ColumnLabels=',*(a3,3x))") ColumnLabels(:)
!     write(Char_NCol, "(g0)")  size(ColumnLabels)                                          ! Integer-to-character conversion of the number of column in the data file
!     Format     =       "(" // trim(Char_NCol) // "(3x,"// Format_real_data // "))"
!     if (i_Debug_Loc) write(DbgUnit,"(6x,'[Write_Data_Splot]: Format       = ',a)") Format
!     if (i_Debug_Loc) write(DbgUnit,"(6x,'[Write_Data_Splot]: Graph%Format = ',a)") Graph%Format
! ! ****************************************************************************************************************************
!
Subroutine Set_Columns_Label( This, X_Axis_Label, Y_Axis_Label, Z_Axis_Label, Lines_Title )

  use GPF_Parameters            ,only:  Axis_Type_Default, Axis_X, Axis_Y, Axis_Z, LineTitle_Default
  use GPF_Tools                 ,only:  Convert_To_String, Extract_Label


  class(GPF_Data_Base_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Graph-Data object
  character(*)  ,dimension( : )                         ,intent(in)     ::  X_Axis_Label
  character(*)  ,dimension( : )                         ,intent(in)     ::  Y_Axis_Label
  character(*)  ,dimension( : )                         ,intent(in)     ::  Z_Axis_Label
  character(*)  ,dimension( : )                         ,intent(in)     ::  Lines_Title

  logical       ,parameter                                              ::  i_Debug_Loc=.False.
  integer                                                               ::  iCol                            ! Index of columns
  integer                                                               ::  iLine                           ! Index of lines
  integer                                                               ::  iXaxis                          ! Index of X-axis
  integer                                                               ::  iYaxis                          ! Index of Y-axis
  integer                                                               ::  iZaxis                          ! Index of Z-axis
  character(:)  ,allocatable                                            ::  Line_Label
  character(:)  ,allocatable                                            ::  Axis_Label
  character(:)  ,allocatable                                            ::  Default_Label


  if (i_Debug_Loc) write(DbgUnit,"(12x,'[Set_Columns_Label]: Entering')")

  if (i_Debug_Loc) write(DbgUnit,"(12x,'[Set_Columns_Label]: This%NColumns = ',i3)") This%NColumns

  allocate( character(Length_Real) :: This%Column_Label( This%NColumns ) )                                      ! Allocating the columns label variable


!   select type (This)
!   type is ( GPF_Data_X1Y1_Type, GPF_Data_X1Y2_Type )
!   type is ( GPF_Data_X2Y2_Type )
!   type is ( GPF_Data_X2Y2Z2_Type )
!   end select

  This%Column_Label     =       "???"

! ==============================================================================================================
!    SETTING LABELS ASSOCIATED TO THE X-AXIS COLUMNS
! ==============================================================================================================
  iCol  =       0                                                                                               ! Initialisation of columns index
  do iXaxis = 1,This%Nabsci                                                                                     ! Loop on all X-axis
    iCol                =       iCol + 1                                                                        ! Incrementing the column index
    Axis_Label          =       X_Axis_Label(iXaxis)                                                            ! Getting the label from X-axis labels (empty string if any)
    Default_Label       =       trim(Axis_Type_Default(Axis_X)) // Convert_To_String(iXaxis)                    ! Getting the default label for X-axis
!     if (i_Debug_Loc) write(DbgUnit,"(12x,'[Set_Columns_Label]: Axis_Label       = ',a)") Axis_Label
!     if (i_Debug_Loc) write(DbgUnit,"(12x,'[Set_Columns_Label]: Default_Label    = ',a)") Default_Label
    if ( len_trim(Axis_Label) /= 0 ) then                                                                       ! If the current Axis_Label is defined
      This%Column_Label(iCol)   =       Axis_Label                                                              ! Then setting the current column header to its value
    else                                                                                                        ! If the current Axis_Label is indefined
      This%Column_Label(iCol)   =       Default_Label                                                           ! Then setting the current column header to its default value
    end if                                                                                                      ! End if case on label strings
  end do                                                                                                        ! End loop on X-axis

! ==============================================================================================================
!    SETTING LABELS ASSOCIATED TO THE Y-AXIS COLUMNS
! ==============================================================================================================

! @TODO: The label associated to the different Y-axis are not extracted, only the label associated to the 1st Y axis is considered
!       One should define and index correspondance from a given line to the associated Y-axis.
!       This index correspondance variable should look like:
!                               iYaxis = Line_To_YAxis(iLine)   ! Getting the index of the Y-axis associated to current line
!       where iLine and iYaxis are the index of the lines and the Y-axis respectively.

  do iLine = 1,This%NLine                                                                                       ! Loop on all the lines to be plotted
    iCol                =       iCol + 1                                                                        ! Incrementing the column index
    iYaxis              =       1                                                                               ! Imposing the index of Y-axis (TODO: To be change if multiple Y-axis)
    Line_Label          =       Lines_Title(iLine)                                                              ! Getting the label from line-titles (empty string if any)
    Axis_Label          =       Y_Axis_Label(iYaxis)                                                            ! Getting the label from Y-axis labels (empty string if any)
    Default_Label       =       trim(Axis_Type_Default(Axis_Y)) // Convert_To_String(iLine)                     ! Getting the default label for Y-axis
!     if (i_Debug_Loc) write(DbgUnit,"(12x,'[Set_Columns_Label]: Line_Label       = ',a)") Line_Label
!     if (i_Debug_Loc) write(DbgUnit,"(12x,'[Set_Columns_Label]: Axis_Label       = ',a)") Axis_Label
!     if (i_Debug_Loc) write(DbgUnit,"(12x,'[Set_Columns_Label]: Default_Label    = ',a)") Default_Label
    This%Column_Label(iCol) =       Default_Label                                                           ! Then setting the current column header to its default value
!     if ( len_trim(Line_Label) /= 0 ) then                                                                       ! If the current Line_Label is defined
!       This%Column_Label(iCol)   =       Line_Label                                                              ! Then setting the current column header to its value
!     else                                                                                                        ! If the current Line_Label is undefined, then setting the column header to either the axis label, or the default column label
!       if ( len_trim(Axis_Label) /= 0 ) then                                                                     ! If the current Axis_Label is defined
!         This%Column_Label(iCol) =       Axis_Label                                                              ! Then setting the current column header to its value
!       else                                                                                                      ! If the current Axis_Label is indefined
!         This%Column_Label(iCol) =       Default_Label                                                           ! Then setting the current column header to its default value
!       end if                                                                                                    ! End if case on label strings
!     end if                                                                                                      ! End if case on label strings
  end do                                                                                                        ! End loop on lines



! ==============================================================================================================
!    SETTING LABELS ASSOCIATED TO THE Y-AXIS COLUMNS
! ==============================================================================================================
  if ( This%Z_Dim /= 0 ) then                                                                                   ! If a Z-axis exists
    iCol                =       iCol + 1                                                                        ! Incrementing the column index
    iZaxis              =       1                                                                               ! Imposing the index of Z-axis (TODO: To be change if multiple Z-axis)
    Axis_Label          =       Z_Axis_Label(iYaxis)                                                            ! Getting the label from Y-axis labels (empty string if any)
    Default_Label       =       trim(Axis_Type_Default(Axis_Z)) // Convert_To_String(iZaxis)                    ! Getting the default label for Z-axis
    if ( len_trim(Axis_Label) /= 0 ) then                                                                       ! If the current Axis_Label is defined
      This%Column_Label(iCol)   =       Axis_Label                                                              ! Then setting the current column header to its value
    else                                                                                                        ! If the current Axis_Label is indefined
      This%Column_Label(iCol)   =       Default_Label                                                           ! Then setting the current column header to its default value
    end if                                                                                                      ! End if case on label strings
  end if


  if (i_Debug_Loc) then
    do iCol = 1,size(This%Column_Label)
      write(DbgUnit,"(12x,'[Set_Columns_Label]: iCol = ',i3,3x,'Column_Label=',g0)") iCol, This%Column_Label(iCol)
    end do
  end if

!   This%Format      =       "(" // Convert_To_String(This%NColumns) // "(3x,"// Format_real_data // "))"
!   if (i_Debug_Loc) write(DbgUnit,"(12x,'[Set_Columns_Label]: This%Format = ',a)") This%Format

  if (i_Debug_Loc) write(DbgUnit,"(12x,'[Set_Columns_Label]: Exiting')")

End Subroutine

Subroutine Write_Header( This, Unit )
  class(GPF_Data_Base_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  integer                                                               ::  iCol                            ! Index of columns
  if ( allocated(This%Column_Label) ) &
  write(Unit,"('#  ',*(a,3x))") (This%Column_Label(iCol), iCol=1,This%NColumns)                                 ! Writing a comment line containing the column labels
End Subroutine

End Module
