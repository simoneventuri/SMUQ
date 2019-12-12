! -*-f90-*-
!!--------------------------------------------------------------------------------------------------------------------------------
!!
!! Stochastic Modeling & Uncertainty Quantification (SMUQ)
!!
!! Copyright (C) 2016 Venturi, Simone & Rostkowski, Przemyslaw (University of Illinois at Urbana-Champaign)
!!
!! This program is free software; you can redistribute it and/or modify it under the terms of the Version 2.1 GNU Lesser General
!! Public License as published by the Free Software Foundation.
!!
!! This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
!!
!! You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to the Free 
!! Software Foundation, Inc. 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!!
!!--------------------------------------------------------------------------------------------------------------------------------

module MParamTableContainer_Class

use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MParamTable_Class                                             ,only:    MParamTable_Type
use MParamTable_Factory_Class                                     ,only:    MParamTable_Factory

implicit none

private

public                                                                ::    MParamTableContainer_Type

type                                                                  ::    MParamTableContainer_Type
  class(MParamTable_Type), allocatable                                ::    Object
contains
  procedure, public                                                   ::    Get
  procedure, public                                                   ::    GetPointer
  procedure, public                                                   ::    Set
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Set( This, Object )

    class(MParamTableContainer_Type), intent(inout)                   ::    This
    class(MParamTable_Type), intent(in)                               ::    Object

    character(*), parameter                                           ::    ProcName='Set'
    integer                                                           ::    StatLoc=0
    allocate(This%Object, source=Object, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Object', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Get( This )

    class(MParamTable_Type), allocatable                              ::    Get

    class(MParamTableContainer_Type), intent(in)                      ::    This

    character(*), parameter                                           ::    ProcName='Get'
    integer                                                           ::    StatLoc=0
    if ( .not. allocated(This%Object) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    allocate(Get, source=This%Object, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get', ProcName=ProcName, stat=StatLoc )

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetPointer( This )

    class(MParamTable_Type), pointer                                  ::    GetPointer

    class(MParamTableContainer_Type), target, intent(in)              ::    This

    character(*), parameter                                           ::    ProcName='GetPointer'

    if ( .not. allocated(This%Object) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    GetPointer => This%Object

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(MParamTableContainer_Type), intent(inout)                   ::    LHS
    class(MParamTableContainer_Type), intent(in)                      ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (MParamTableContainer_Type)
        if ( allocated(RHS%Object) ) then
          if ( allocated(LHS%Object) ) deallocate( LHS%Object, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='LHS%Object', Procname=ProcName, stat=StatLoc )
          allocate(LHS%Object, source=RHS%Object, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Object', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!----------------------------------------------------------------------------------------------------------------------------!!
  impure elemental subroutine Finalizer( This )

    type(MParamTableContainer_Type), intent(inout)                    ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc

    if ( allocated(This%Object) ) deallocate(This%Object, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( name='This%Object', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
