Module gpf_Configuration_Module

  implicit none

  private
  public  ::  Set_GPF_Output_Directory

  contains

Subroutine Set_GPF_Output_Directory( Directory )
  implicit none
  character(*)                                          ,intent(in)     ::  Directory                       !< Name of the output directory where files should be stored
End Subroutine

End Module
