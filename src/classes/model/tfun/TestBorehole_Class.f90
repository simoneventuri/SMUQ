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

module TestBorehole_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use TestFunction_Class                                            ,only:    TestFunction_Type
use Output_Class                                                  ,only:    Output_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    TestBorehole_Type

type, extends(TestFunction_Type)                                      ::    TestBorehole_Type
  real(rkp)                                                           ::    R
  real(rkp)                                                           ::    RW
  real(rkp)                                                           ::    TU
  real(rkp)                                                           ::    HU
  real(rkp)                                                           ::    TL
  real(rkp)                                                           ::    HL
  real(rkp)                                                           ::    L
  real(rkp)                                                           ::    KW
  character(:), allocatable                                           ::    R_Dependency
  character(:), allocatable                                           ::    RW_Dependency
  character(:), allocatable                                           ::    TU_Dependency
  character(:), allocatable                                           ::    HU_Dependency
  character(:), allocatable                                           ::    TL_Dependency
  character(:), allocatable                                           ::    HL_Dependency
  character(:), allocatable                                           ::    L_Dependency
  character(:), allocatable                                           ::    KW_Dependency
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, nopass, public                                           ::    ComputeBorehole
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize(This)

    class(TestBorehole_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'borehole'
      This%Initialized = .true.
    end if

    call This%SetDefaults()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset(This)

    class(TestBorehole_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults(This)

    class(TestBorehole_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

    This%Label = 'borehole'
    This%R = 2000.0
    This%RW = 0.1
    This%TU = 70000.0
    This%HU = 1050.0
    This%TL = 76.0
    This%HL = 760.0
    This%L = 1400.0
    This%KW = 11000.0
    This%R_Dependency = ''
    This%RW_Dependency = ''
    This%TU_Dependency = ''
    This%HU_Dependency = ''
    This%TL_Dependency = ''
    This%HL_Dependency = ''
    This%L_Dependency = ''
    This%KW_Dependency = ''

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput(This, Input, Prefix)

    class(TestBorehole_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i
    logical                                                           ::    MandatoryLoc

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'label'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
    This%Label = VarC0D

    SectionName = 'parameters'

    ParameterName = 'r_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%R_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'r'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%R = VarR0D

    ParameterName = 'rw_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%RW_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'rw'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%RW = VarR0D

    ParameterName = 'tu_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%TU_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'tu'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%TU = VarR0D

    ParameterName = 'hu_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%HU_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'hu'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%HU = VarR0D

    ParameterName = 'tl_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%TL_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'tl'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%TL = VarR0D

    ParameterName = 'hl_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%HL_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'hl'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%HL = VarR0D

    ParameterName = 'l_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%L_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'l'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%L = VarR0D

    ParameterName = 'kw_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%KW_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'kw'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%KW = VarR0D

    This%Constructed = .true.

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput(This, Name, Prefix, Directory)

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(TestBorehole_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))
   
    call GetInput%AddParameter(Name='label', Value=This%Label)

    SectionName='parameters'
    call GetInput%AddSection(SectionName=SectionName)
    call GetInput%AddParameter(Name='r', Value=ConvertToString(Value=This%R), SectionName=SectionName)
    call GetInput%AddParameter(Name='rw', Value=ConvertToString(Value=This%RW), SectionName=SectionName)
    call GetInput%AddParameter(Name='tu', Value=ConvertToString(Value=This%TU), SectionName=SectionName)
    call GetInput%AddParameter(Name='hu', Value=ConvertToString(Value=This%HU), SectionName=SectionName)
    call GetInput%AddParameter(Name='tl', Value=ConvertToString(Value=This%TL), SectionName=SectionName)
    call GetInput%AddParameter(Name='hl', Value=ConvertToString(Value=This%HL), SectionName=SectionName)
    call GetInput%AddParameter(Name='l', Value=ConvertToString(Value=This%L), SectionName=SectionName)
    call GetInput%AddParameter(Name='kw', Value=ConvertToString(Value=This%KW), SectionName=SectionName)
    if (len_trim(This%R_Dependency) /= 0) call GetInput%AddParameter(Name='r_dependency', Value=This%R_Dependency,             &
                                                                                                         SectionName=SectionName)
    if (len_trim(This%RW_Dependency) /= 0) call GetInput%AddParameter(Name='rw_dependency', Value=This%RW_Dependency,          &
                                                                                                         SectionName=SectionName)
    if (len_trim(This%TU_Dependency) /= 0) call GetInput%AddParameter(Name='tu_dependency', Value=This%TU_Dependency,          &
                                                                                                         SectionName=SectionName)
    if (len_trim(This%HU_Dependency) /= 0) call GetInput%AddParameter(Name='hu_dependency', Value=This%HU_Dependency,          &
                                                                                                         SectionName=SectionName)
    if (len_trim(This%TL_Dependency) /= 0) call GetInput%AddParameter(Name='tl_dependency', Value=This%TL_Dependency,          &
                                                                                                         SectionName=SectionName)
    if (len_trim(This%HL_Dependency) /= 0) call GetInput%AddParameter(Name='hl_dependency', Value=This%HL_Dependency,          &
                                                                                                         SectionName=SectionName)
    if (len_trim(This%L_Dependency) /= 0) call GetInput%AddParameter(Name='l_dependency', Value=This%L_Dependency,             &
                                                                                                         SectionName=SectionName)
    if (len_trim(This%KW_Dependency) /= 0) call GetInput%AddParameter(Name='kw_dependency', Value=This%KW_Dependency,          &
                                                                                                         SectionName=SectionName)

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Run(This, Input, Output)

    class(TestBorehole_Type), intent(inout)                           ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), intent(inout)                                  ::    Output

    character(*), parameter                                           ::    ProcName='ProcessInput'
    real(rkp), allocatable, dimension(:,:)                            ::    Ordinate
    real(rkp)                                                         ::    R
    real(rkp)                                                         ::    RW
    real(rkp)                                                         ::    TU
    real(rkp)                                                         ::    HU
    real(rkp)                                                         ::    TL
    real(rkp)                                                         ::    HL
    real(rkp)                                                         ::    L
    real(rkp)                                                         ::    KW
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    allocate(Ordinate(1,1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Ordinate', ProcName=ProcName, stat=StatLoc)
    if (len_trim(This%R_Dependency) /= 0) then
      call Input%GetValue(Value=R, Label=This%R_Dependency)
    else
      R = This%R
    end if
    if (len_trim(This%RW_Dependency) /= 0) then
      call Input%GetValue(Value=RW, Label=This%RW_Dependency)
    else
      RW = This%RW
    end if
    if (len_trim(This%TU_Dependency) /= 0) then
      call Input%GetValue(Value=TU, Label=This%TU_Dependency)
    else
      TU = This%TU
    end if
    if (len_trim(This%HU_Dependency) /= 0) then
      call Input%GetValue(Value=HU, Label=This%HU_Dependency)
    else
      HU = This%HU
    end if
    if (len_trim(This%TL_Dependency) /= 0) then
      call Input%GetValue(Value=TL, Label=This%TL_Dependency)
    else
      TL = This%TL
    end if
    if (len_trim(This%HL_Dependency) /= 0) then
      call Input%GetValue(Value=HL, Label=This%HL_Dependency)
    else
      HL = This%HL
    end if
    if (len_trim(This%L_Dependency) /= 0) then
      call Input%GetValue(Value=L, Label=This%L_Dependency)
    else
      L = This%L
    end if
    if (len_trim(This%KW_Dependency) /= 0) then
      call Input%GetValue(Value=KW, Label=This%KW_Dependency)
    else
      KW = This%KW
    end if

    Ordinate(1,1) = This%ComputeBorehole(r, rw, tu, hu, tl, hl, l, kw)

    call Output%Construct(Values=Ordinate, Label=This%Label)

    deallocate(Ordinate, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Ordinate', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputeBorehole(r, rw, tu, hu, tl, hl, l, kw)

    real(rkp)                                                         ::    ComputeBorehole

    real(rkp), intent(in)                                             ::    r
    real(rkp), intent(in)                                             ::    rw
    real(rkp), intent(in)                                             ::    tu
    real(rkp), intent(in)                                             ::    hu
    real(rkp), intent(in)                                             ::    tl
    real(rkp), intent(in)                                             ::    hl
    real(rkp), intent(in)                                             ::    l
    real(rkp), intent(in)                                             ::    kw

    character(*), parameter                                           ::    ProcName='ComputeBorehole'

    ComputeBorehole = (Two*pi*tu*(hu-hl)) / (dlog(r/rw) * (One+(Two*l*tu)/(dlog(r/rw)*(rw**2)*kw) + tu/tl))

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  impure elemental subroutine Copy(LHS, RHS)

    class(TestBorehole_Type), intent(out)                             ::    LHS
    class(TestFunction_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
      type is (TestBorehole_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if(RHS%Constructed) then
          LHS%Label = RHS%Label
          LHS%R = RHS%R
          LHS%RW = RHS%RW
          LHS%TU = RHS%TU
          LHS%HU = RHS%HU
          LHS%TL = RHS%TL
          LHS%HL = RHS%HL
          LHS%L = RHS%L
          LHS%KW = RHS%KW
          LHS%R_Dependency = RHS%R_Dependency
          LHS%RW_Dependency = RHS%RW_Dependency
          LHS%TU_Dependency = RHS%TU_Dependency
          LHS%HU_Dependency = RHS%HU_Dependency
          LHS%TL_Dependency = RHS%TL_Dependency
          LHS%HL_Dependency = RHS%HL_Dependency
          LHS%L_Dependency = RHS%L_Dependency
          LHS%KW_Dependency = RHS%KW_Dependency
        end if
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)
    end select

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  impure elemental subroutine Finalizer(This)

    type(TestBorehole_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
