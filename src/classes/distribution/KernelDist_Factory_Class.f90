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

module KernelDist_Factory_Class

use DistProb_Class                                                ,only:    DistProb_Type
use DistUnif_Class                                                ,only:    DistUnif_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use Input_Library
use String_Module
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    KernelDist_Factory
public                                                                ::    KernelDist_Factory_Type

type                                                                  ::    KernelDist_Factory_Type
contains
  generic, public                                                     ::    Construct               =>    Construct_C0D
  procedure, nopass, public                                           ::    Construct_C0D
  procedure, nopass, public                                           ::    GetOption
End Type

type(KernelDist_Factory_Type)                                         ::    KernelDist_Factory
logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Construct_C0D(Object, DesiredType)

    class(DistProb_Type), allocatable, intent(inout)                  ::    Object                                             
    character(*), intent(in)                                          ::    DesiredType                                               

    character(*), parameter                                           ::    ProcName='Construct_C0D' 

    if (allocated(Object)) call Error%Raise(Line="Object already allocated", ProcName=ProcName)

    select case (LowerCase(DesiredType))

      case('uniform')
        allocate(DistUnif_Type :: Object)
        select type (Object) 
          type is (DistUnif_Type)
            call Object%Construct(A=-One, B=One)
          class default
            call Error%Raise('Something went wrong', ProcName=ProcName)
        end select
      case('normal')
        allocate(DistNorm_Type :: Object)
        select type (Object) 
          type is (DistNorm_Type)
            call Object%Construct(Mu=Zero, Sigma=One)
          class default
            call Error%Raise('Something went wrong', ProcName=ProcName)
        end select

      case default
        call Error%Raise(Line="Type not supported: DesiredType = " // DesiredType, ProcName=ProcName)

    end select

    call Object%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetOption(Object)

    character(:), allocatable                                         ::    GetOption

    class(DistProb_Type), intent(in)                                  ::    Object                                                                                            

    character(*), parameter                                           ::    ProcName='GetOption' 

    select type (Object)

      type is (DistUnif_Type)
        GetOption = 'uniform'

      type is (DistNorm_Type)
        GetOption = 'normal'

      class default
        call Error%Raise(Line="Object is either not allocated/associated or definitions are not up to date", ProcName=ProcName)

    end select

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
