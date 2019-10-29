Module GPF_LineStyle_PointInterval_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private
  public  ::  GPF_LineStyle_PointInterval_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_LineStyle_PointInterval_Type
    character(:)        ,allocatable                    ::  Value                                           !< Character string containing the point-interval value
  contains
    private                                                                                                     ! Setting private type-bound procedures
    procedure   ,public   ::  GetValue      =>  Get_PointInterval_Value                         ! Gets the point-interval value
    procedure   ,public   ::  Set_Command   =>  Set_PointInterval_Command                       ! Sets the command from object components
  End Type

  Interface             GPF_LineStyle_PointInterval_Type
    Module Procedure    Construct_LineStyle_PointInterval
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************
Function Construct_LineStyle_PointInterval( Value, Debug ) result(This)
  use GPF_Tools                 ,only:  Is_Numeric
  type(GPF_LineStyle_PointInterval_Type)                                ::  This                            !< Passed-object dummy argument corresponding to the LineStyle-PointInterval object
  character(*)                                ,optional ,intent(in)     ::  Value                           !< Value of the point-interval
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator
  character(*)                                              ,parameter  ::  Keyword='pi'
  call This%Set_Debug( Debug )
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointInterval]: Entering')")

  This%Value    =       ""                                                                                      ! Initializing to an empty string => by default, no point-interval is written
  if ( present(Value) ) then                                                                                    ! If present optional input argument
    if ( Is_Numeric(trim(Value)) ) then                                                                         ! If input argument is a valid numeric character variable
      This%Value = trim(Value)                                                                                  ! Setting the local variable to the optional input value
    else                                                                                                        ! If it is not a numeric character variable => error and so ignoring the point-interval value
      if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointInterval]: WARNING: The point-interval is specific but the argument is not a numeric character variable => ignoring the point-interval')")
    end if                                                                                                      ! Enf if case on numeric value
  end if                                                                                                        ! End if case on optional input argument presence

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointInterval]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointInterval]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command from object components

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointInterval]: This%Command   = ',a)") This%Command
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointInterval]: Exiting')")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Function Get_PointInterval_Value( This ) result(Value)
  implicit none
  class(GPF_LineStyle_PointInterval_Type)               ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the PointInterval object
  character(:)  ,allocatable                                            ::  Value                           !< Output character string corresponding to the point-interval value
  Value         =       This%Value                                                                              ! Setting the point-interval value in the output string
End Function


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_PointInterval_Command( This )
  use GPF_Tools                 ,only:  Add_Apostroph
  implicit none
  class(GPF_LineStyle_PointInterval_Type)               ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the PointInterval object
  This%Command          =       ""                                                                              ! Initializing to an empty string
  if ( len_trim(This%Value) /= 0 ) This%Command = " " // This%Keyword // " " // This%Value // " "               ! Setting the value associated to the point-interval option if any
  This%Presence =       ( len_trim(This%Command) /= 0 )                                                         !
End Subroutine

End Module