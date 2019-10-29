Module Status_Module

  use Status_Class        ,only:  Status_Type

  implicit none

  private
!   public  ::  AddProcName
  public  ::  UpdateStatusAndRetrun

  Interface

!     Module Pure Subroutine AddProcName( ProcName, Status )
!       character(*)                                ,optional ,intent(in)     ::  ProcName
!       type(Status_Type)                           ,optional ,intent(inout)  ::  Status
!     End Subroutine

    Module Function UpdateStatusAndRetrun( Status, ProcName, Message, ExitLogger ) result(Indicator)
      type(Status_Type)                           ,optional ,intent(inout)  ::  Status
      character(*)                                ,optional ,intent(in)     ::  ProcName
      character(*)                                ,optional ,intent(in)     ::  Message
      logical                                     ,optional ,intent(in)     ::  ExitLogger
      logical                                                               ::  Indicator
    End Function

  End Interface

End Module