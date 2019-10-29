! The include file 'AddVar0dToVar1d_Inline.F90' is called from the file 'Utilities_SubModule.f90'.
! Before calling the procedure, the following marco should be set:
! * _ProcedureName_   Name of the procedure
! * _VarType_         Type of the varibale variable to be added. Possible values: 'logical', 'integer', 'real(8)', 'character(:)'
! * _Is_Character_    Indicator whether variable is character. Possible values: '0' or '1' if the variable is / is not a character
Module Procedure _ProcedureName_
  _VarType_     ,allocatable                                            ::  TmpVar(:)
  logical                                                               ::  PushEnd_
  integer                                                               ::  NInp, NOut, NNew
  PushEnd_  =   PresentAndTrue(PushEnd)
  NInp      =   1
# if _Is_Character_
    Block
      integer   ::  Length
      if ( .not. allocated(OutVar) ) allocate( character(0) :: OutVar(0) )
      if ( PresentAndTrue(OnlyIfAbsent) ) then
        if ( IsIncluded(InpVar,OutVar) ) return
      end if
      NOut    =   size(OutVar)
      Length  =   max( len(OutVar), len(InpVar) )
      if ( PushEnd_ ) then
        NNew  =   NOut
      else
        NNew  =   NOut + NInp
      end if
      allocate( character(Length) :: TmpVar(NNew) )
    End Block
# else
    if ( .not. allocated(OutVar) ) allocate( OutVar(0) )
    if ( PresentAndTrue(OnlyIfAbsent) ) then
      if ( IsIncluded(InpVar,OutVar) ) return
    end if
    NOut    =   size(OutVar)
    if ( PushEnd_ ) then
      NNew  =   NOut
    else
      NNew  =   NOut + NInp
    end if
    allocate( TmpVar(NNew) )
# endif
  if ( PushEnd_ ) then
    TmpVar(1:NOut-1)      =   OutVar(2:NOut)
    TmpVar(NOut:NOut)     =   InpVar
  else if ( present(At) ) then
    TmpVar(1:At-1)        =   OutVar(1:At-1)
    TmpVar(At:At-1+NInp)  =   InpVar
    TmpVar(At+NInp:NNew)  =   OutVar(At:NOut)
  else
    TmpVar(1:NOut)        =   OutVar
    TmpVar(NOut+1:NNew)   =   InpVar
  end if
! Setting the output variable
  call move_alloc( TmpVar, OutVar )
End Procedure

! Un-defining the name of the procedure for thew next include
# undef   _ProcedureName_