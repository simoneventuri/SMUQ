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
use StringConversion_Module
use TestFunction_Class                                            ,only:    TestFunction_Type
use Output_Class                                                  ,only:    Output_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use IScalarValue_Class                                            ,only:    IScalarValue_Type
use IScalarValue_Factory_Class                                    ,only:    IScalarValue_Factory
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    TestBorehole_Type

type, extends(TestFunction_Type)                                      ::    TestBorehole_Type
  class(IScalarValue_Type), allocatable                               ::    R
  class(IScalarValue_Type), allocatable                               ::    RW
  class(IScalarValue_Type), allocatable                               ::    TU
  class(IScalarValue_Type), allocatable                               ::    HU
  class(IScalarValue_Type), allocatable                               ::    TL
  class(IScalarValue_Type), allocatable                               ::    HL
  class(IScalarValue_Type), allocatable                               ::    L
  class(IScalarValue_Type), allocatable                               ::    KW
contains
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, nopass, public                                           ::    ComputeBorehole
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(TestBorehole_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed = .false.

  if (allocated(This%R)) deallocate(This%R, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%R', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%RW)) deallocate(This%RW, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%RW', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%TU)) deallocate(This%TU, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%TU', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%HU)) deallocate(This%HU, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%HU', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%TL)) deallocate(This%TL, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%TL', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%HL)) deallocate(This%HL, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%HL', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%L)) deallocate(This%L, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%L', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%KW)) deallocate(This%KW, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%KW', ProcName=ProcName, stat=StatLoc)

  This%Label = 'borehole'

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(TestBorehole_Type), intent(inout)                             ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  logical                                                             ::    Found
  real(rkp)                                                           ::    VarR0D
  integer                                                             ::    VarI0D
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    i
  logical                                                             ::    MandatoryLoc
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  ParameterName = 'response_label'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Label = VarC0D

  SectionName = 'parameters'
  call InputVerifier%AddSection(Section=SectionName)

  SubSectionName = SectionName // '>r'
  call InputVerifier%AddSection(Section='r', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%R, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SubSectionName = SectionName // '>rw'
  call InputVerifier%AddSection(Section='rw', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%RW, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SubSectionName = SectionName // '>tu'
  call InputVerifier%AddSection(Section='tu', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%TU, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SubSectionName = SectionName // '>hu'
  call InputVerifier%AddSection(Section='hu', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%HU, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SubSectionName = SectionName // '>tl'
  call InputVerifier%AddSection(Section='tl', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%TL, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SubSectionName = SectionName // '>hl'
  call InputVerifier%AddSection(Section='hl', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%HL, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SubSectionName = SectionName // '>l'
  call InputVerifier%AddSection(Section='l', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%L, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SubSectionName = SectionName // '>kw'
  call InputVerifier%AddSection(Section='kw', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%KW, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput
  class(TestBorehole_Type), intent(in)                                ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))
  
  call GetInput%AddParameter(Name='response_label', Value=This%Label)

  SectionName='parameters'
  call GetInput%AddSection(SectionName=SectionName)

  SubSectionName = 'r'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%R, Name=SubSectionName, Prefix=PrefixLoc,      &
                                                                       Directory=DirectorySub), To_SubSection=SectionName)
  
  SubSectionName = 'rw'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%RW, Name=SubSectionName, Prefix=PrefixLoc,     &
                                                                       Directory=DirectorySub), To_SubSection=SectionName)

  SubSectionName = 'tu'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%TU, Name=SubSectionName, Prefix=PrefixLoc,     &
                                                                       Directory=DirectorySub), To_SubSection=SectionName)

  SubSectionName = 'hu'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%HU, Name=SubSectionName, Prefix=PrefixLoc,     &
                                                                       Directory=DirectorySub), To_SubSection=SectionName)

  SubSectionName = 'tl'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%TL, Name=SubSectionName, Prefix=PrefixLoc,     &
                                                                      Directory=DirectorySub), To_SubSection=SectionName)
  
  SubSectionName = 'hl'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%HL, Name=SubSectionName, Prefix=PrefixLoc,     &
                                                                      Directory=DirectorySub), To_SubSection=SectionName)

  SubSectionName = 'l'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%L, Name=SubSectionName, Prefix=PrefixLoc,      &
                                                                      Directory=DirectorySub), To_SubSection=SectionName)

  SubSectionName = 'kw'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%KW, Name=SubSectionName, Prefix=PrefixLoc,     &
                                                                      Directory=DirectorySub), To_SubSection=SectionName)

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Run(This, Input, Output)

  class(TestBorehole_Type), intent(inout)                             ::    This
  type(Input_Type), intent(in)                                        ::    Input
  type(Output_Type), intent(inout)                                    ::    Output

  character(*), parameter                                             ::    ProcName='ProcessInput'
  real(rkp), allocatable, dimension(:,:)                              ::    Ordinate
  real(rkp)                                                           ::    R
  real(rkp)                                                           ::    RW
  real(rkp)                                                           ::    TU
  real(rkp)                                                           ::    HU
  real(rkp)                                                           ::    TL
  real(rkp)                                                           ::    HL
  real(rkp)                                                           ::    L
  real(rkp)                                                           ::    KW
  integer                                                             ::    i
  integer                                                             ::    ii
  integer                                                             ::    iii
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  allocate(Ordinate(1,1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Ordinate', ProcName=ProcName, stat=StatLoc)

  R = This%R%GetValue(Input=Input)
  RW = This%RW%GetValue(Input=Input)
  TU = This%TU%GetValue(Input=Input)
  HU = This%HU%GetValue(Input=Input)
  TL = This%TL%GetValue(Input=Input)
  HL = This%HL%GetValue(Input=Input)
  L = This%L%GetValue(Input=Input)
  KW = This%KW%GetValue(Input=Input)

  Ordinate(1,1) = This%ComputeBorehole(r, rw, tu, hu, tl, hl, l, kw)

  call Output%Construct(Values=Ordinate, Label=This%Label)

  deallocate(Ordinate, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Ordinate', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function ComputeBorehole(r, rw, tu, hu, tl, hl, l, kw)

  real(rkp)                                                           ::    ComputeBorehole

  real(rkp), intent(in)                                               ::    r
  real(rkp), intent(in)                                               ::    rw
  real(rkp), intent(in)                                               ::    tu
  real(rkp), intent(in)                                               ::    hu
  real(rkp), intent(in)                                               ::    tl
  real(rkp), intent(in)                                               ::    hl
  real(rkp), intent(in)                                               ::    l
  real(rkp), intent(in)                                               ::    kw

  character(*), parameter                                             ::    ProcName='ComputeBorehole'

  ComputeBorehole = (Two*pi*tu*(hu-hl)) / (dlog(r/rw) * (One+(Two*l*tu)/(dlog(r/rw)*(rw**2)*kw) + tu/tl))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(TestBorehole_Type), intent(out)                               ::    LHS
  class(TestFunction_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)
    type is (TestBorehole_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed
      if(RHS%Constructed) then
        LHS%Label = RHS%Label
        allocate(LHS%R, source=RHS%R, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%R', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%RW, source=RHS%RW, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%RW', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%TU, source=RHS%TU, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%TU', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%HU, source=RHS%HU, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%HU', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%TL, source=RHS%TL, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%TL', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%HL, source=RHS%HL, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%HL', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%L, source=RHS%L, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%L', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%KW, source=RHS%KW, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%KW', ProcName=ProcName, stat=StatLoc)
      end if
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(TestBorehole_Type), intent(inout)                              ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%R)) deallocate(This%R, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%R', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%RW)) deallocate(This%RW, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%RW', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%TU)) deallocate(This%TU, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%TU', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%HU)) deallocate(This%HU, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%HU', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%TL)) deallocate(This%TL, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%TL', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%HL)) deallocate(This%HL, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%HL', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%L)) deallocate(This%L, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%L', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%KW)) deallocate(This%KW, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%KW', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
