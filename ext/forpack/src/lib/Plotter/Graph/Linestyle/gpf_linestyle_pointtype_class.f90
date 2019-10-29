Module GPF_LineStyle_PointType_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private
  public  ::  GPF_LineStyle_PointType_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_LineStyle_PointType_Type
    character(:)        ,allocatable                    ::  Value                                           !< Character string containing the point-type value
  contains
    private                                                                                                     ! Setting private type-bound procedures
    procedure   ,public   ::  GetValue               =>  Get_PointType_Value                             ! Gets the point-type value
    procedure   ,public   ::  Set_Command             =>  Set_PointType_Command                           ! Sets the command from object components
  End Type

  Interface             GPF_LineStyle_PointType_Type
    Module Procedure    Construct_LineStyle_PointType
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************
Function Construct_LineStyle_PointType( Value, Debug ) result(This)
  use GPF_Tools                 ,only:  Is_Numeric
  type(GPF_LineStyle_PointType_Type)                                    ::  This                            !< Passed-object dummy argument corresponding to the LineStyle-PointType object
  character(*)                                ,optional ,intent(in)     ::  Value                           !< Value of the point-type
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator
  character(*)                                              ,parameter  ::  Keyword='pt'
  call This%Set_Debug( Debug )
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointType]: Entering')")

  This%Value    =       ""                                                                                      ! Initializing to an empty string => by default, no point-type is written
  if ( present(Value) ) then                                                                                    ! If present optional input argument
    if ( Is_Numeric(trim(Value)) ) then                                                                         ! If input argument is a valid numeric character variable
      This%Value = trim(Value)                                                                                  ! Setting the local variable to the optional input value
    else                                                                                                        ! If it is not a numeric character variable => error and so ignoring the point-type value
      if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointType]: WARNING: The point-type is specific but the argument is not a numeric character variable => ignoring the point-type')")
    end if                                                                                                      ! Enf if case on numeric value
  end if                                                                                                        ! End if case on optional input argument presence

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointType]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointType]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command from object components

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointType]: This%Command   = ',a)") This%Command
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_LineStyle_PointType]: Exiting')")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Function Get_PointType_Value( This ) result(Value)
  implicit none
  class(GPF_LineStyle_PointType_Type)                   ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the PointType object
  character(:)  ,allocatable                                            ::  Value                           !< Output character string corresponding to the point-type value
  Value         =       This%Value                                                                              ! Setting the point-type value in the output string
End Function


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_PointType_Command( This )
  use GPF_Tools                 ,only:  Add_Apostroph
  implicit none
  class(GPF_LineStyle_PointType_Type)                   ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the PointType object
  This%Command          =       ""                                                                              ! Initializing to an empty string
  if ( len_trim(This%Value) /= 0 ) This%Command = " " // This%Keyword // " " // This%Value // " "               ! Setting the value associated to the point-type option if any
  This%Presence =       ( len_trim(This%Command) /= 0 )                                                         !
End Subroutine

End Module