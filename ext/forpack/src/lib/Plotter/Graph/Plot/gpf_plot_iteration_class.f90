Module GPF_Plot_Iteration_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_LineStyle_Class       ,only:  GPF_LineStyle_Type

  implicit none

  private

  public  ::  GPF_Plot_Iteration_Type

  Type  ,extends(GPF_Command_Type)                         ::  GPF_Plot_Iteration_Type
    character(:)        ,allocatable                    ::  Initial_Index
    character(:)        ,allocatable                    ::  Final_Index
    character(:)        ,allocatable                    ::  Increment
  contains
    private
    procedure             ::  Set_Initial_Index       =>  Set_Iteration_Initial_Index
    procedure             ::  Set_Final_Index         =>  Set_Iteration_Final_Index
    procedure   ,public   ::  Set_Command             =>  Set_Iteration_Command                           !< Sets the Iteration command
  End Type

  Interface             GPF_Plot_Iteration_Type
    Module Procedure    Construct_Plot_Iteration
  End Interface

  contains


! REMARK:
! This procedure sets the iterative options if required.
! For of the plot command is required whenever some LineStyle elements refers to multiple lines.
! The NAbsci variable is added to the line index in order to have the column index.
! The output is something like:
!      plot for [n=1:61] "data.dat" using 1:(column(n)+2)*1E-3:(n-1) notitle with lines       ls 100,\

Function Construct_Plot_Iteration( NAbsci, LineStyle, Debug ) result(This)

  implicit none

  type(GPF_Plot_Iteration_Type)                                         ::  This                            !< Object to be constructed
  integer                                               ,intent(in)     ::  NAbsci                          !< Number of abscisses
  type(GPF_LineStyle_Type)                              ,intent(in)     ::  LineStyle                       !< LineStyle object
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Iteration]: Entering')")

  call This%Set_Initial_Index( NAbsci + LineStyle%iLine_Ini )

  This%Command          =       ''                                                                              ! Initializaing the commabnd to an empty string

  if ( LineStyle%i_Multiple_Lines ) then                                                                        ! If current LineStyle element refers to multiple lines, then setting the iterative option of the plot command to avoid redundancy

    if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Iteration]: Calling This%Set_Initial_Index')")
    call This%Set_Initial_Index( NAbsci + LineStyle%iLine_Ini )                                                 ! Setting the initial index

    if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Iteration]: Calling This%Set_Final_Index')")
    call This%Set_Final_Index( NAbsci + LineStyle%iLine_Fin )                                                   ! Setting the final index

    if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Iteration]: Calling This%Set_Final_Index')")
    call This%Set_Final_Index( NAbsci + LineStyle%iLine_Fin )                                                   ! Setting the final index

    This%Increment      =       ""

    if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Iteration]: Calling This%Set_Command')")
    call This%Set_Command()                                                                     ! Setting the command from components

  end if

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Iteration]: Exiting',/)")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************


Subroutine Set_Iteration_Initial_Index( This, Value )
  use GPF_Tools                 ,only:  Convert_To_String
  implicit none
  class(GPF_Plot_Iteration_Type)                        ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Iteration object
  integer                                               ,intent(in)     ::  Value                           !< Integer value corresponding to the initial index
  This%Initial_Index    =       Convert_To_String( Value )
End Subroutine

Subroutine Set_Iteration_Final_Index( This, Value )
  use GPF_Tools                 ,only:  Convert_To_String
  implicit none
  class(GPF_Plot_Iteration_Type)                        ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Iteration object
  integer                                               ,intent(in)     ::  Value                           !< Integer value corresponding to the final index
  This%Final_Index    =       Convert_To_String( Value )
End Subroutine

Subroutine Set_Iteration_Command( This )
  implicit none
  class(GPF_Plot_Iteration_Type)                        ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Iteration object
  character(*)                                              ,parameter  ::  Prefix   =      ' for [i='      ! Prefix of the command
  character(*)                                              ,parameter  ::  Suffix  =      '] '             ! Suffix of the command
  character(*)                                              ,parameter  ::  Separator=":"                   ! Seprator character between each value
  if (This%i_Debug) write(DbgUnit,"(16x,'[Set_Iteration_Command]: Entering')")
  This%Command  =       Prefix // This%Initial_Index // Separator // This%Final_Index
  if ( len_trim(This%Increment) /= 0 ) This%Command = This%Command // Separator // This%Increment
  This%Command  =       This%Command // Suffix
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator
  if (This%i_Debug) then
    write(DbgUnit,"(16x,'[Set_Iteration_Command]: This%Command  = ',a )") This%Command
    write(DbgUnit,"(16x,'[Set_Iteration_Command]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(16x,'[Set_Iteration_Command]: Entering')")
  end if
End Subroutine

End Module