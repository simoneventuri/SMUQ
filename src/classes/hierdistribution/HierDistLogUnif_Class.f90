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

module HierDistLogUnif_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use HierDistUnif_Class                                            ,only:    HierDistUnif_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistLogUnif_Class                                             ,only:    DistLogUnif_Type
use DistProb_Class                                                ,only:    DistProb_Type
use IScalarValueClass                                             ,only:    IScalarValue_Type
use IScalarFixedClass                                             ,only:    IScalarFixed_Type
use IScalarValue_Factory_Class                                    ,only:    IScalarValue_Factory

implicit none

private

public                                                                ::    HierDistLogUnif_Type

type, extends(HierDistUnif_Type)                                      ::    HierDistLogUnif_Type
  class(IScalarValue_Type), allocatable                               ::    A
  class(IScalarValue_Type), allocatable                               ::    B
contains
  procedure, public                                                   ::    Initialize
  procedure, private                                                  ::    GenerateDistribution
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(HierDistLogUnif_Type), intent(inout)                          ::    This

  character(*), parameter                                             ::    ProcName='Initialize'

  if (.not. This%Initialized) then
    This%Name = 'hierarchical_loguniform'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine GenerateDistribution(This, A, B, Distribution)

  class(HierDistLogUnif_Type), intent(in)                             ::    This
  real(rkp), intent(in)                                               ::    A
  real(rkp), intent(in)                                               ::    B
  class(DistProb_Type), allocatable, intent(out)                      ::    Distribution

  character(*), parameter                                             ::    ProcName='GenerateDistribution'
  integer                                                             ::    StatLoc=0  

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  allocate(DistLogUnif_Type :: Distribution)

  select type (Distribution)
    type is (DistLogUnif_Type) 
      call Distribution%Construct(A=A, B=B)
    class default
      call Error%Raise("Something went wrong", ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
