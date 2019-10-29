SubModule(EnvironmentVariable_Class) EnvironmentVariable_SubClass

! @TODO: Add the 'Save' procedure to save an environment variable as a component (both its name and value)

  implicit none

  contains

Module Procedure InitializeEnvironmentVariable
  integer                                                               ::  i
  This%NItems   =   0

  if ( allocated(This%Items) ) deallocate(This%Items)

  if ( .Not. present(Names) ) then
    allocate( This%Items(This%NItems) )
    return
  end if

  This%NItems   =   size(Names)
  allocate( This%Items(This%NItems) )

  if ( present(Descriptions) ) then
    do i = 1,This%NItems
    associate( EnvVar => This%Items(i) )
      if ( i <= size(Descriptions) ) then
        call EnvVar%Initialize( Names(i), Descriptions(i) )
      else
        call EnvVar%Initialize( Names(i) )
      end if
    end associate
    end do
  else
    do i = 1,This%NItems
    associate( EnvVar => This%Items(i) )
      call EnvVar%Initialize( Names(i) )
    end associate
    end do
  end if

End Procedure

Module Procedure AddEnvironmentVariable
  type(EnvVar_Type)   ,dimension(:) ,allocatable                        ::  List
  type(EnvVar_Type)                                                     ::  Item
  if ( .not. allocated(This%Items) ) allocate( This%Items(0) )
  call Item%Initialize( Name, Description )
  allocate( List, source = [This%Items,Item] )
  call move_alloc( List, This%Items )
  This%NItems   =   size(This%Items)
End Procedure

Module Procedure GetEnvironmentVariable
!   use Error_Class               ,only:  Error
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  TargetName
  character(:)  ,allocatable                                            ::  Description_
!   character(:)  ,allocatable                                            ::  ErrMsg
  character(*)                                              ,parameter  ::  ProcName='GetEnvironmentVariable' ! Name of the current procedure
  TargetName    =   trim(Name)
  Value         =   ""
  Description_  =   ""
  do i = 1,This%NItems
  associate( EnvVar => This%Items(i) )
    if ( EnvVar%Name /= TargetName ) cycle
    Value         =   EnvVar%Value
    Description_  =   EnvVar%Description
    exit
  end associate
  end do
  if ( present(Description) ) Description = Description_
  if ( present(Mandatory) ) then
    if (Mandatory .and. Value=="") then
!       ErrMsg    =     "The '"//TargetName//"' environment variable is mandatory but it has not been found"
!       call Error%Raise( ErrMsg, ProcName = ProcName )                                                  ! If an non-zero status indicator is returned, raising an error and stopping the code
!       call Error%Raise( "The '"//TargetName//"' environment variable is mandatory but it has not been found", ProcName = ProcName )                                                  ! If an non-zero status indicator is returned, raising an error and stopping the code
    end if
  end if
End Procedure

End SubModule