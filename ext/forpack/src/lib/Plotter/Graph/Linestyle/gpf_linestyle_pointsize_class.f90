Module GPF_LineStyle_PointSize_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private
  public  ::  GPF_LineStyle_PointSize_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_LineStyle_PointSize_Type
    character(:)        ,allocatable                    ::  Value                                           !< Character string containing the point-size value
  contains
    private                                                                                                     ! Setting private type-bound procedures
    procedure   ,public   ::  GetValue               =>  Get_PointSize_Value                             ! Gets the point-size value
    procedure   ,public   ::  Set_Command             =>  Set_PointSize_Command                           ! Sets the command from object components
  End Type

  Interface             GPF_LineStyle_PointSize_Type
    Module Procedure    Construct_LineStyle_PointSize
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************
Function Construct_LineStyle_PointSize( Value, Debug ) result(This)
  use GPF_Tools                 ,only:  Is_Numeric
  type(GPF_LineStyle_PointSize_Type)                                    ::  This                            !< Passed-object dummy argument corresponding to the LineStyle-PointSize object
  character(*)                                ,optional ,intent(in)     ::  Value                           !< Value of the point-size
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator
  character(*)                                              ,parameter  ::  Keyword='ps'
  call This%Set_Debug( Debug )
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointSize]: Entering')")

  This%Value    =       ""                                                                                      ! Initializing to an empty string => by default, no point-size is written
  if ( present(Value) ) then                                                                                    ! If present optional input argument
    if ( Is_Numeric(trim(Value)) ) then                                                                         ! If input argument is a valid numeric character variable
      This%Value = trim(Value)                                                                                  ! Setting the local variable to the optional input value
    else                                                                                                        ! If it is not a numeric character variable => error and so ignoring the point-size value
      if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointSize]: WARNING: The point-size is specific but the argument is not a numeric character variable => ignoring the point-size')")
    end if                                                                                                      ! Enf if case on numeric value
  end if                                                                                                        ! End if case on optional input argument presence

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointSize]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointSize]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command from object components

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointSize]: This%Command   = ',a)") This%Command
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointSize]: Exiting')")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Function Get_PointSize_Value( This ) result(Value)
  implicit none
  class(GPF_LineStyle_PointSize_Type)                   ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the PointSize object
  character(:)  ,allocatable                                            ::  Value                           !< Output character string corresponding to the point-size value
  Value         =       This%Value                                                                              ! Setting the point-size value in the output string
End Function


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_PointSize_Command( This )
  use GPF_Tools                 ,only:  Add_Apostroph
  implicit none
  class(GPF_LineStyle_PointSize_Type)                   ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the PointSize object
  This%Command          =       ""                                                                              ! Initializing to an empty string
  if ( len_trim(This%Value) /= 0 ) This%Command = " " // This%Keyword // " " // This%Value // " "               ! Setting the value associated to the point-size option if any
  This%Presence =       ( len_trim(This%Command) /= 0 )                                                         !
End Subroutine

End Module