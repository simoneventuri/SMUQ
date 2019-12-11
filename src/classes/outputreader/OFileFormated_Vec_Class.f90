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

module OFileFormated_Vec_Class

use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use OFileFormated_Class                                           ,only:    OFileFormated_Type
use OFileFormated_Factory_Class                                   ,only:    OFileFormated_Factory

implicit none

private

public                                                                ::    OFileFormated_Vec_Type

type                                                                  ::    OFileFormated_Vec_Type
  class(OFileFormated_Type), pointer                                  ::    OFileFormated=>null()
contains
  procedure, public                                                   ::    Get
  procedure, public                                                   ::    GetPointer
  procedure, public                                                   ::    Set
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Set( This, Object )

    class(OFileFormated_Vec_Type), intent(inout)                      ::    This
    class(OFileFormated_Type), intent(in)                             ::    Object

    character(*), parameter                                           ::    ProcName='Set'
    integer                                                           ::    StatLoc=0
    allocate(This%OFileFormated, source=Object, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%OFileFormated', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Get( This )

    class(OFileFormated_Type), allocatable                            ::    Get

    class(OFileFormated_Vec_Type), intent(in)                         ::    This

    character(*), parameter                                           ::    ProcName='Get'
    integer                                                           ::    StatLoc=0
    if ( .not. associated(This%OFileFormated) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    allocate(Get, source=This%OFileFormated, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetPointer( This )

    class(OFileFormated_Type), pointer                                ::    GetPointer

    class(OFileFormated_Vec_Type), intent(in)                         ::    This

    character(*), parameter                                           ::    ProcName='GetPointer'
    integer                                                           ::    StatLoc=0
    if ( .not. associated(This%OFileFormated) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    GetPointer => This%OFileFormated

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(OFileFormated_Vec_Type), intent(inout)                      ::    LHS
    class(OFileFormated_Vec_Type), intent(in)                         ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (OFileFormated_Vec_Type)
        if ( associated(RHS%OFileFormated) ) then
          if ( associated(LHS%OFileFormated) ) deallocate( LHS%OFileFormated, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='LHS%OFileFormated', Procname=ProcName, stat=StatLoc )
          allocate(LHS%OFileFormated, source=RHS%OFileFormated, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%OFileFormated', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(OFileFormated_Vec_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( associated(This%OFileFormated) ) deallocate(This%OFileFormated, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( name='This%OFileFormated', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
