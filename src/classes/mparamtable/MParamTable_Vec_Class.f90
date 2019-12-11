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

module MParamTable_Vec_Class

use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MParamTable_Class                                             ,only:    MParamTable_Type
use MParamTable_Factory_Class                                     ,only:    MParamTable_Factory

implicit none

private

public                                                                ::    MParamTable_Vec_Type

type                                                                  ::    MParamTable_Vec_Type
  class(MParamTable_Type), pointer                                    ::    MParamTable=>null()
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

    class(MParamTable_Vec_Type), intent(inout)                        ::    This
    class(MParamTable_Type), intent(in)                               ::    Object

    character(*), parameter                                           ::    ProcName='Set'
    integer                                                           ::    StatLoc=0
    allocate(This%MParamTable, source=Object, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MParamTable', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Get( This )

    class(MParamTable_Type), allocatable                              ::    Get

    class(MParamTable_Vec_Type), intent(in)                           ::    This

    character(*), parameter                                           ::    ProcName='Get'
    integer                                                           ::    StatLoc=0
    if ( .not. associated(This%MParamTable) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    allocate(Get, source=This%MParamTable, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get', ProcName=ProcName, stat=StatLoc )

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetPointer( This )

    class(MParamTable_Type), pointer                                  ::    GetPointer

    class(MParamTable_Vec_Type), intent(in)                           ::    This

    character(*), parameter                                           ::    ProcName='GetPointer'
    if ( .not. associated(This%MParamTable) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    GetPointer => This%MParamTable

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(MParamTable_Vec_Type), intent(inout)                        ::    LHS
    class(MParamTable_Vec_Type), intent(in)                           ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (MParamTable_Vec_Type)
        if ( associated(RHS%MParamTable) ) then
          if ( associated(LHS%MParamTable) ) deallocate( LHS%MParamTable, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='LHS%MParamTable', Procname=ProcName, stat=StatLoc )
          allocate(LHS%MParamTable, source=RHS%MParamTable, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%MParamTable', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!----------------------------------------------------------------------------------------------------------------------------!!
  impure elemental subroutine Finalizer( This )

    type(MParamTable_Vec_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc

    if ( associated(This%MParamTable) ) deallocate(This%MParamTable, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( name='This%MParamTable', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
