! The include file 'AddVar1dToVar2d_Inline.F90' is called from the file 'Utilities_SubModule.f90'.
! Before calling the procedure, the following marco should be set:
! * _ProcedureName_   Name of the procedure
! * _VarType_         Type of the varibale variable to be added. Possible values: 'logical', 'integer', 'real(8)', 'character(:)'
! * _Is_Character_  Indicator whether variable is character. Possible values: '0' or '1' if the variable is / is not a character
Module Procedure _ProcedureName_
  integer                                                               ::  iD
  integer ,dimension(2)                                                 ::  NewDim, OldDim
  _VarType_     ,dimension(:,:) ,allocatable                            ::  NewOutVar
  iD        =   1
  if ( present(Dim) ) iD = Dim
# if _Is_Character_
  if ( .not. allocated(OutVar) ) allocate( character(0) :: OutVar(0,0) )
# else
  if ( .not. allocated(OutVar) ) allocate( OutVar(0,0) )
# endif
  OldDim      =   [size(OutVar,1),size(OutVar,2)]
  NewDim      =   OldDim
  if ( size(OutVar) == 0 ) then
    if (iD == 1) then
      NewDim(1) =   1
      NewDim(2) =   size(InpVar)
    else
      NewDim(1) =   size(InpVar)
      NewDim(2) =   1
    end if
  else
    NewDim(iD)  =   NewDim(iD) + 1
  end if
!   write(*,"(6x,'[AddVar1dToVar2d]: OldDim = ',*(i3,3x))") OldDim
!   write(*,"(6x,'[AddVar1dToVar2d]: NewDim = ',*(i3,3x))") NewDim
# if _Is_Character_
  Block
    integer   ::  Length
    Length      =   max( maxval(len_trim(InpVar)) , maxval(len_trim(OutVar)) )
    allocate( Character(Length) :: NewOutVar(NewDim(1),NewDim(2)) )
  End Block
# else
  allocate( NewOutVar(NewDim(1),NewDim(2)) )
# endif
  NewOutVar(1:OldDim(1),1:OldDim(2))   =   OutVar   ! Copying old values
  select case (iD)
    case(1);  NewOutVar(NewDim(1),1:NewDim(2)) = InpVar
    case(2);  NewOutVar(1:NewDim(1),NewDim(2)) = InpVar
  end select
  call move_alloc( NewOutVar, OutVar )
End Procedure

! Un-defining the marco for next call
# undef   _ProcedureName_