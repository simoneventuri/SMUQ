! The include file 'AddVar3dToVar4d_Inline.F90' is called from the file 'Utilities_SubModule.f90'.
! Before calling the procedure, the following marco should be set:
! * _ProcedureName_   Name of the procedure
! * _VarType_         Type of the varibale variable to be added. Possible values: 'logical', 'integer', 'real(8)', 'character(:)'
! * _Is_Character_  Indicator whether variable is character. Possible values: '0' or '1' if the variable is / is not a character
Module Procedure _ProcedureName_


!   use Logger_Class      ,only:  Logger

  integer                                                               ::  iD, i, i1, i2, i3, i4
  integer                                                               ::  NewDim(4), OldDim(4)
  _VarType_   ,allocatable                                              ::  NewOutVar(:,:,:,:)
!@TODO: Set default value ad optional input argument
# if defined _Is_Character_
  _VarType_                                                 ,parameter  ::  DefaultValue = ""
# elif defined _Is_Logical_
  _VarType_                                                 ,parameter  ::  DefaultValue = .False.
# else
  _VarType_                                                 ,parameter  ::  DefaultValue = 0
# endif


!   call Logger%Entering( "AddVar3dToVar4d" )


  iD        =   4
  if ( present(Dim) ) iD = Dim

# if _Is_Character_
  if ( .not. allocated(OutVar) ) allocate( character(0) :: OutVar(0,0,0,0) )
# else
  if ( .not. allocated(OutVar) ) allocate( OutVar(0,0,0,0) )
# endif


!   call Logger%Write( "ID = ", ID )
!   call Logger%Write( "-> size(InpVar)    = ", [size(InpVar,1),size(InpVar,2),size(InpVar,3)] )
!   call Logger%Write( "-> size(OutVar)    = ", [size(OutVar,1),size(OutVar,2),size(OutVar,3),size(OutVar,4)] )



  OldDim    =   [size(OutVar,1),size(OutVar,2),size(OutVar,3),size(OutVar,4)]
  NewDim    =   OldDim
  if ( size(OutVar) == 0 ) then
    select case (iD)
      case (1)
        NewDim(1) = 1
        NewDim(2) = size(InpVar,1)
        NewDim(3) = size(InpVar,2)
        NewDim(4) = size(InpVar,3)
      case (2)
        NewDim(1) = size(InpVar,1)
        NewDim(2) = 1
        NewDim(3) = size(InpVar,2)
        NewDim(4) = size(InpVar,3)
      case (3)
        NewDim(1) = size(InpVar,1)
        NewDim(2) = size(InpVar,2)
        NewDim(3) = 1
        NewDim(4) = size(InpVar,3)
      case (4)
        NewDim(1) = max( size(InpVar,1), size(OutVar,1) )
        NewDim(2) = size(InpVar,2)
        NewDim(3) = size(InpVar,3)
        NewDim(4) = 1
    end select
  else

    select case (iD)
!       case (1)
!         NewDim(1) = 1
!         NewDim(2) = size(InpVar,1)
!         NewDim(3) = size(InpVar,2)
!         NewDim(4) = size(InpVar,3)
!       case (2)
!         NewDim(1) = size(InpVar,1)
!         NewDim(2) = 1
!         NewDim(3) = size(InpVar,2)
!         NewDim(4) = size(InpVar,3)
!       case (3)
!         NewDim(1) = size(InpVar,1)
!         NewDim(2) = size(InpVar,2)
!         NewDim(3) = 1
!         NewDim(4) = size(InpVar,3)
      case (4)
        NewDim(1)   =   max( size(InpVar,1), size(OutVar,1) )
        NewDim(2)   =   max( size(InpVar,2), size(OutVar,2) )
        NewDim(3)   =   max( size(InpVar,3), size(OutVar,3) )
        NewDim(iD)  =   NewDim(iD) + 1
    end select


!     NewDim(1) = max( size(InpVar,1), size(OutVar,1) )
!     NewDim(2) = size(InpVar,2)
!     NewDim(3) = size(InpVar,3)
!     NewDim(4) = 1
!     NewDim(iD)  =   NewDim(iD) + 1
  end if

!   call Logger%Write( "OldDim = ", OldDim )
!   call Logger%Write( "NewDim = ", NewDim )

# if _Is_Character_
  Block
    integer   ::  Length
    Length      =   max( maxval(len_trim(InpVar)) , maxval(len_trim(OutVar)) )
    allocate( Character(Length) :: NewOutVar(NewDim(1),NewDim(2),NewDim(3),NewDim(4)) )
  End Block
# else
  allocate( NewOutVar(NewDim(1),NewDim(2),NewDim(3),NewDim(4)) )
# endif

  NewOutVar   =   DefaultValue

!   call Logger%Write( "size(NewOutVar) = ", [size(NewOutVar,1),size(NewOutVar,2),size(NewOutVar,3),size(NewOutVar,4)] )

! Copying old values
  do i4 = 1,OldDim(4)
    do i3 = 1,OldDim(3)
      do i2 = 1,OldDim(2)
        do i1 = 1,OldDim(1)
          NewOutVar(i1,i2,i3,i4)  =   OutVar(i1,i2,i3,i4)
        end do
      end do
    end do
  end do

! Copying new values
  select case (iD)
    case(1)
      NewOutVar( NewDim(1)   , 1:NewDim(2) , 1:NewDim(3) , 1:NewDim(4) ) = InpVar
    case(2)
      NewOutVar( 1:NewDim(1) , NewDim(2)   , 1:NewDim(3) , 1:NewDim(4) ) = InpVar
    case(3)
      NewOutVar( 1:NewDim(1) , 1:NewDim(2) , NewDim(3)   , 1:NewDim(4) ) = InpVar
    case(4)
      NewOutVar( 1:NewDim(1) , 1:NewDim(2) , 1:NewDim(3) , NewDim(4)   ) = InpVar
  end select

  call move_alloc( NewOutVar, OutVar )

!   call Logger%Exiting()

End Procedure

! Un-defining the marco for next call
# undef   _ProcedureName_