Module Status_Library

  use Status_Class        ,only:  Status_Type, UpdateStatus
!   use Status_Module       ,only:  UpdateStatusAndRetrun

  implicit none

  private
  public  ::  Status_Type
  public  ::  UpdateStatus
!   public  ::  UpdateStatusAndRetrun

End Module