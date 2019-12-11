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

module HierDistProb_Vec_Class

use Logger_Class                                                  ,only:  Logger
use Error_Class                                                   ,only:  Error
use HierDistProb_Class                                            ,only:  HierDistProb_Type
use HierDistProb_Factory_Class                                    ,only:  HierDistProb_Factory

implicit none

private

public                                                                ::    HierDistProb_Vec_Type

type                                                                  ::    HierDistProb_Vec_Type
  class(HierDistProb_Type), pointer                                   ::    HierDistProb=>null()
contains
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Get
  procedure, public                                                   ::    GetPointer
  procedure, public                                                   ::    Set
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Set( This, Object )

    class(HierDistProb_Vec_Type), intent(inout)                       ::    This
    class(HierDistProb_Type), intent(in)                              ::    Object

    character(*), parameter                                           ::    ProcName='Set'
    integer                                                           ::    StatLoc=0

    if ( associated(This%HierDistProb) ) deallocate(This%HierDistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%HierDistProb', ProcName=ProcName, stat=StatLoc)
    
    allocate(This%HierDistProb, source=Object, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%HierDistProb', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Get( This )

    class(HierDistProb_Type), allocatable                             ::    Get

    class(HierDistProb_Vec_Type), intent(in)                          ::    This

    character(*), parameter                                           ::    ProcName='Get'
    integer                                                           ::    StatLoc=0

    if ( .not. associated(This%HierDistProb) ) call Error%Raise( Line='Probability distribution never defined', ProcName=ProcName)

    allocate(Get, source=This%HierDistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get', ProcName=ProcName, stat=StatLoc )

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetPointer( This )

    class(HierDistProb_Type), pointer                                 ::    GetPointer

    class(HierDistProb_Vec_Type), intent(in)                          ::    This

    character(*), parameter                                           ::    ProcName='GetPointer'

    if ( .not. associated(This%HierDistProb) ) call Error%Raise( Line='Probability distribution never defined', ProcName=ProcName)

    GetPointer => This%HierDistProb

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(HierDistProb_Vec_Type), intent(inout)                       ::    LHS
    class(HierDistProb_Vec_Type), intent(in)                          ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (HierDistProb_Vec_Type)
        if ( associated(RHS%HierDistProb) ) then
          if ( associated(LHS%HierDistProb) ) deallocate( LHS%HierDistProb, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='LHS%HierDistProb', Procname=ProcName, stat=StatLoc )
          allocate(LHS%HierDistProb, source=RHS%HierDistProb, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%HierDistProb', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer( This )

    type(HierDistProb_Vec_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc

    if ( associated(This%HierDistProb) ) deallocate(This%HierDistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( name='This%HierDistProb', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
