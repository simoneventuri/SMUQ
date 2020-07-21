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

module HierDistLogNorm_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use HierDistNorm_Class                                            ,only:    HierDistNorm_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistLogNorm_Class                                             ,only:    DistLogNorm_Type
use DistProb_Class                                                ,only:    DistProb_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    HierDistLogNorm_Type

type, extends(HierDistNorm_Type)                                      ::    HierDistLogNorm_Type
contains
  procedure, public                                                   ::    Reset
  procedure, private                                                  ::    GenerateDistribution
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(HierDistLogNorm_Type), intent(inout)                          ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed = .false.

  if (allocated(This%A)) deallocate(This%A, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%A', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%B)) deallocate(This%B, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%B', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Mu)) deallocate(This%Mu, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Mu', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Sigma)) deallocate(This%Sigma, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Sigma', ProcName=ProcName, stat=StatLoc)

  This%TruncatedRight = .false.
  This%TruncatedLeft = .true.
  
end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine GenerateDistribution(This, Mu, Sigma, A, B, Distribution)

  class(HierDistLogNorm_Type), intent(in)                             ::    This
  real(rkp), intent(in)                                               ::    Mu
  real(rkp), intent(in)                                               ::    Sigma
  real(rkp), intent(in)                                               ::    A
  real(rkp), intent(in)                                               ::    B
  class(DistProb_Type), allocatable, intent(out)                      ::    Distribution

  character(*), parameter                                             ::    ProcName='GenerateDistribution'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  allocate(DistLogNorm_Type :: Distribution)

  select type (Distribution)
    type is (DistLogNorm_Type) 
      if (This%TruncatedLeft .and. This%TruncatedRight) then
        call Distribution%Construct(Mu=Mu, Sigma=Sigma, A=A, B=B)
      else
        call Distribution%Construct(Mu=Mu, Sigma=Sigma, A=A)
      end if
    class default
      call Error%Raise("Something went wrong", ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
