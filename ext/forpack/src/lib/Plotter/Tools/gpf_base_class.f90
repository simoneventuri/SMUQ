Module GPF_Base_Class

  implicit none

  private

  public        GPF_Base_Type

  Type  ::  GPF_Base_Type
    logical     ,public                                 ::  i_Debug         =       .False.                 !< Debugging indicator
    logical                                             ::  Presence        =       .False.                 !< Presence indicator
  contains
    procedure   ,public   ::  Set_Debug                                       !< Setting debugging indicator
    procedure   ,public   ::  Get_Presence                                    !< Getting presence indicator
  End Type

  contains

Subroutine Set_Debug( This, Debug )
  use GPF_Parameters            ,only:  i_Debug_Default
  implicit none
  class(GPF_Base_Type)                  ,intent(inout)  ::  This                                            !< Derived-type structure
  logical                     ,optional ,intent(in)     ::  Debug                                           !< Derived-type debugging indicator
  This%i_Debug = i_Debug_Default                                                                                ! Setting debugging indicator to default value
  if ( present(Debug) ) This%i_Debug = Debug                                                                    ! If present optional input argument, then setting debugging indicator to input value
End Subroutine

Function Get_Presence( This ) result(Presence)
  implicit none
  class(GPF_Base_Type)                  ,intent(in)     ::  This                                            !< Derived-type structure
  logical                                               ::  Presence                                        !< Presence indicator
  Presence      =       This%Presence                                                                           ! Getting presence indicator
End Function

End Module