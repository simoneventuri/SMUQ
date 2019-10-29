! File: test_AddVar3dToVar4d_Inline.F90
Block
  _VarType_     ,allocatable  ::  Expected(:,:,:,:), Obtained(:,:,:,:)
  _VarType_     ,allocatable  ::  A3(:,:,:), B3(:,:,:)
  integer                     ::  i, i1, i2, i3, i4
  integer                     ::  AddDim
! =======================================================================================
  RealName    =   "AddVar3dToVar4d" // "_" // STRINGIFY(_VarKind_)
  Description =   "'"//RealName//"': add 3d-var to unallocated 4d-var."
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A3       ) ) deallocate(A3     ); allocate( A3(2,2,2) )
  if ( allocated(Obtained) ) deallocate(Obtained)
  if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(2,2,2,1) )
  A3(:,:,:)           =   1!reshape([(1,i=1,size(A3))],[2,2,2])
  Expected(:,:,:,1)  =   A3
  call AddElementToArray( A3, Obtained )
  if (Detailed) then
    call Logger%Write( "-> Results" )
    do i4 = 1,size(Expected,4)
      do i3 = 1,size(Expected,3)
        do i2 = 1,size(Expected,2)
          do i1 = 1,size(Expected,1)
              call Logger%Write( "  -> " // &
                    "Expected("//Inline([i1,i2,i3,i4],Separator=",")//") = ", Expected(i1,i2,i3,i4), &
                    "Obtained("//Inline([i1,i2,i3,i4],Separator=",")//") = ", Obtained(i1,i2,i3,i4), Fi="i1", Fr="f4.1" )
          end do
        end do
      end do
    end do
  end if
  @assertEqual( Obtained , Expected )
  call Logger%Write( "[ok]" )
! =======================================================================================
  do i = 1,4
    RealName    =   "AddVar3dToVar4d" // "_" // STRINGIFY(_VarKind_)
    AddDim      =   i
    Description =   "'"//RealName//"': add 3d-var to unallocated 4d-var with 'AddDim="//Convert_To_String(AddDim)//"'"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    if (Detailed) call Logger%Write( "" )
    if ( allocated(A3)       ) deallocate(A3     ); allocate( A3(2,2,2) )
    if ( allocated(Obtained) ) deallocate(Obtained)
    if ( allocated(Expected) ) deallocate(Expected)
    A3(:,:,:)           =   1!reshape([(1,i=1,size(A3))],[2,2,2])
    select case (i)
      case (1); allocate( Expected(1,2,2,2) ); Expected(1,:,:,:) = A3
      case (2); allocate( Expected(2,1,2,2) ); Expected(:,1,:,:) = A3
      case (3); allocate( Expected(2,2,1,2) ); Expected(:,:,1,:) = A3
      case (4); allocate( Expected(2,2,2,1) ); Expected(:,:,:,1) = A3
    end select
    call AddElementToArray( A3, Obtained, Dim=AddDim )
    if (Detailed) then
      call Logger%Write( "-> Results" )
      do i4 = 1,size(Expected,4)
        do i3 = 1,size(Expected,3)
          do i2 = 1,size(Expected,2)
            do i1 = 1,size(Expected,1)
              call Logger%Write( "  -> " // &
                    "Expected("//Inline([i1,i2,i3,i4],Separator=",")//") = ", Expected(i1,i2,i3,i4), &
                    "Obtained("//Inline([i1,i2,i3,i4],Separator=",")//") = ", Obtained(i1,i2,i3,i4), Fi="i1", Fr="f4.1" )
            end do
          end do
        end do
      end do
    end if
    @assertEqual( Obtained , Expected )
    call Logger%Write( "[ok]" )
  end do
! =======================================================================================
  do i = 4,4
    RealName    =   "AddVar3dToVar4d" // "_" // STRINGIFY(_VarKind_)
    AddDim      =   i
    Description =   "'"//RealName//"': add 3d-var to 4d-var with 'AddDim="//Convert_To_String(AddDim)//"'"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    if (Detailed) call Logger%Write( "" )
    if ( allocated(A3)       ) deallocate(A3      ); allocate( A3(2,2,2) )
    if ( allocated(Obtained) ) deallocate(Obtained); allocate( Obtained(2,2,2,1) )
    if ( allocated(Expected) ) deallocate(Expected)
    Obtained  =   1
    A3        =   2
    select case (i)
      case (1); allocate( Expected(1,2,2,2) ); Expected(1,:,:,:) = A3
      case (2); allocate( Expected(2,1,2,2) ); Expected(:,1,:,:) = A3
      case (3); allocate( Expected(2,2,1,2) ); Expected(:,:,1,:) = A3
      case (4)
        allocate( Expected(2,2,2,2) )
        Expected(:,:,:,1) = 1
        Expected(:,:,:,2) = 2
    end select
    call AddElementToArray( A3, Obtained, Dim=AddDim )
    if (Detailed) then
      call Logger%Write( "-> Results" )
      do i4 = 1,size(Expected,4)
        do i3 = 1,size(Expected,3)
          do i2 = 1,size(Expected,2)
            do i1 = 1,size(Expected,1)
              call Logger%Write( "  -> " // &
                    "Expected("//Inline([i1,i2,i3,i4],Separator=",")//") = ", Expected(i1,i2,i3,i4), &
                    "Obtained("//Inline([i1,i2,i3,i4],Separator=",")//") = ", Obtained(i1,i2,i3,i4), Fi="i1", Fr="f4.1" )
            end do
          end do
        end do
      end do
    end if
    @assertEqual( Obtained , Expected )
    call Logger%Write( "[ok]" )
  end do
! =======================================================================================
  RealName    =   "AddVar3dToVar4d" // "_" // STRINGIFY(_VarKind_)
  AddDim      =   4
  Description =   "'"//RealName//"': add two 3d-var to 4d-var with 'AddDim="//Convert_To_String(AddDim)//"' with new larger than old"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A3)       ) deallocate(A3      ); allocate( A3(3,3,3) ); A3 = 2
  if ( allocated(B3)       ) deallocate(B3      ); allocate( B3(3,4,3) ); B3 = 3
  if ( allocated(Obtained) ) deallocate(Obtained); allocate( Obtained(2,2,2,1) ); Obtained(1:2,1:2,1:2,1)  =   1
  if ( allocated(Expected) ) deallocate(Expected)

  select case (AddDim)
!     case (1); allocate( Expected(1,2,2,2) ); Expected(1,:,:,:) = A3
!     case (2); allocate( Expected(2,1,2,2) ); Expected(:,1,:,:) = A3
!     case (3); allocate( Expected(2,2,1,2) ); Expected(:,:,1,:) = A3
    case (4)
      allocate( Expected(3,4,3,3) )
!       allocate( Expected(3,3,3,2) )
      Expected                        = 0 ! Default values
      Expected( 1:2 , 1:2 , 1:2 , 1 ) = 1 ! Initial values
      Expected( 1:3 , 1:3 , 1:3 , 2 ) = 2 ! Values from A3
      Expected( 1:3 , 1:4 , 1:3 , 3 ) = 3 ! Values from B3
  end select
!   call Logger%Write( "-> Calling AddElementToArray" )
  call AddElementToArray( A3, Obtained, Dim=AddDim )
!   call Logger%Write( "-> Calling AddElementToArray" )
  call AddElementToArray( B3, Obtained, Dim=AddDim )
  if (Detailed) then
    call Logger%Write( "-> Results" )
    do i4 = 1,size(Expected,4)
      do i3 = 1,size(Expected,3)
        do i2 = 1,size(Expected,2)
          do i1 = 1,size(Expected,1)
            call Logger%Write( "  -> " // &
                  "Expected("//Inline([i1,i2,i3,i4],Separator=",")//") = ", Expected(i1,i2,i3,i4), &
                  "Obtained("//Inline([i1,i2,i3,i4],Separator=",")//") = ", Obtained(i1,i2,i3,i4), Fi="i1", Fr="f4.1" )
          end do
        end do
      end do
    end do
  end if
  @assertEqual( Obtained , Expected )
  call Logger%Write( "[ok]" )
End Block

# undef _VarType_
# undef _VarKind_
