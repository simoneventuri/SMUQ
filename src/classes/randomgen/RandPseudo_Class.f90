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

module RandPseudo_Class

use Parameters_Library
use Input_Library
use ArrayIORoutines_Module
use StringConversion_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MT64_Class                                                    ,only:    MT64_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    RandPseudo_Type

type                                                                  ::    RandPseudo_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Name
  integer(8)                                                          ::    Seed=1
  integer(8)                                                          ::    SeedDefault=1
  type(MT64_Type)                                                     ::    PRNG
  integer                                                             ::    DrawType=2
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput, &
                                                                                                          ConstructCase1
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  generic, public                                                     ::    Draw                    =>    DrawScalar, &
                                                                                                          DrawVec, &
                                                                                                          DrawMat
  procedure, private                                                  ::    DrawScalar
  procedure, private                                                  ::    DrawVec
  procedure, private                                                  ::    DrawMat
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!----------------------------------------------------------------------------------------------------------------------------!!
subroutine Initialize(This)

  class(RandPseudo_Type), intent(inout)                               ::    This

  character(*), parameter                                             ::    ProcName='Initialize'
  integer(8)                                                          ::    SysTimeCount

  if (.not. This%Initialized) then
    This%Initialized = .true.
    This%Name = 'pseudo'

    call system_clock(SysTimeCount)
    This%SeedDefault = SysTimeCount

    call This%SetDefaults()
  end if

end subroutine
!!----------------------------------------------------------------------------------------------------------------------------!!

!!----------------------------------------------------------------------------------------------------------------------------!!
subroutine Reset(This)

  class(RandPseudo_Type), intent(inout)                               ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Initialized=.false.
  This%Constructed=.false.

  call This%Initialize()

end subroutine
!!----------------------------------------------------------------------------------------------------------------------------!!

!!----------------------------------------------------------------------------------------------------------------------------!!
subroutine SetDefaults(This)

  class(RandPseudo_Type), intent(inout)                               ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

  This%Seed = This%SeedDefault
  call This%PRNG%init_genrand64(This%Seed)
  This%DrawType=2

end subroutine
!!----------------------------------------------------------------------------------------------------------------------------!!

!!----------------------------------------------------------------------------------------------------------------------------!!
subroutine ConstructInput (This, Input, Prefix)

  class(RandPseudo_Type), intent(inout)                               ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  logical                                                             ::    Found
  character(:), allocatable                                           ::    VarC0D
  logical                                                             ::    VarL0D
  integer                                                             ::    VarI0D
  integer(8), allocatable, dimension(:)                               ::    VarI1D_8
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  ParameterName = 'seed'
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) then 
    This%Seed = ConvertToInteger8(String=VarC0D)
    call This%PRNG%init_genrand64(This%Seed)
  end if

  ParameterName = 'draw_type'
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%DrawType=VarI0D

  SectionName = 'internal_generator'
  if (Input%HasSection(SubSectionName=SectionName)) then
    ParameterName = 'mti'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName)
    This%PRNG%mti = VarI0D

    SubSectionName = SectionName // '>mt'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    call ImportArray(Input=InputSection, Array=VarI1D_8, Prefix=PrefixLoc)
    nullify(InputSection)
    This%PRNG%mt = VarI1D_8
    deallocate(VarI1D_8, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D_8', ProcName=ProcName, stat=StatLoc)
  end if

  This%Constructed = .true.

end subroutine 
!!----------------------------------------------------------------------------------------------------------------------------!!

!!----------------------------------------------------------------------------------------------------------------------------!!
subroutine ConstructCase1 (This, Seed, DrawType)

  class(RandPseudo_Type), intent(inout)                               ::    This
  integer(8), optional, intent(in)                                    ::    Seed
  integer, optional, intent(in)                                       ::    DrawType

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  if (present(seed)) then
    This%Seed = Seed
    call This%PRNG%init_genrand64(This%Seed)
  end if

  if (present(DrawType)) This%DrawType = DrawType

  This%Constructed = .true.

end subroutine 
!!----------------------------------------------------------------------------------------------------------------------------!!

!!----------------------------------------------------------------------------------------------------------------------------!!
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput
  class(RandPseudo_Type), intent(in)                                  ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    FileName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  type(SMUQFile_Type)                                                 ::    File
  character(:), allocatable                                           ::    VarC0D

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

  call GetInput%AddParameter(Name='seed', Value=ConvertToString(Value=This%Seed))
  call GetInput%AddParameter(Name='draw_type', Value=ConvertToString(Value=This%DrawType))

  SectionName = 'internal_generator'
  call GetInput%AddSection(SectionName=SectionName)

  call GetInput%AddParameter(Name='mti', Value=ConvertToString(Value=This%PRNG%mti), SectionName=SectionName)

  SubSectionName = 'mt'
  call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
  SubSectionName = SectionName // '>' // SubSectionName
  call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  if (ExternalFlag) then
    FileName = DirectoryLoc // '/mt.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Input=InputSection, Array=This%PRNG%mt, File=File)
  else
    call ExportArray(Input=InputSection, Array=This%PRNG%mt)
  end if
  nullify(InputSection)

end function
!!----------------------------------------------------------------------------------------------------------------------------!!

!!----------------------------------------------------------------------------------------------------------------------------!!
subroutine DrawScalar(This, Sample, DrawType)

  class(RandPseudo_Type), intent(inout)                               ::    This
  real(rkp), intent(out)                                              ::    Sample
  integer, optional, intent(in)                                       ::    DrawType

  character(*), parameter                                             ::    ProcName='DrawScalar'
  integer                                                             ::    DrawTypeLoc

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  if (present(DrawType)) then 
    DrawTypeLoc = DrawType
  else
    DrawTypeLoc = This%DrawType
  end if

  select case (DrawTypeLoc)
    case (1)
      Sample = real(This%PRNG%genrand64_real1(),rkp)
    case (2)
      Sample = real(This%PRNG%genrand64_real2(),rkp)
    case (3)
      Sample = real(This%PRNG%genrand64_real3(),rkp)
    case default
      call Error%Raise(Line='Something went wrong when selecting draw type', ProcName=ProcName)
  end select

end subroutine
!!----------------------------------------------------------------------------------------------------------------------------!!

!!----------------------------------------------------------------------------------------------------------------------------!!
subroutine DrawVec(This, Samples, NbSamples, DrawType)

  class(RandPseudo_Type), intent(inout)                               ::    This
  real(rkp), dimension(:), intent(inout)                              ::    Samples
  integer, intent(in)                                                 ::    NbSamples
  integer, optional, intent(in)                                       ::    DrawType

  character(*), parameter                                             ::    ProcName='DrawVec'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    DrawTypeLoc

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  if (size(Samples,1) /= NbSamples) call Error%Raise('Incompatible array', ProcName=ProcName)

  if (present(DrawType)) then 
    DrawTypeLoc = DrawType
  else
    DrawTypeLoc = This%DrawType
  end if

  i = 1
  select case (DrawTypeLoc)
    case (1)
      do i = 1, size(Samples,1)
        Samples(i) = real(This%PRNG%genrand64_real1(),rkp)
      end do
    case (2)
      do i = 1, size(Samples,1)
        Samples(i) = real(This%PRNG%genrand64_real2(),rkp)
      end do
    case (3)
      do i = 1, size(Samples,1)
        Samples(i) = real(This%PRNG%genrand64_real3(),rkp)
      end do
    case default
      call Error%Raise(Line='Something went wrong when selecting draw type', ProcName=ProcName)
  end select

end subroutine
!!----------------------------------------------------------------------------------------------------------------------------!!

!!----------------------------------------------------------------------------------------------------------------------------!!
subroutine DrawMat(This, Samples, NbSamples, NbDim, DrawType)

  class(RandPseudo_Type), intent(inout)                               ::    This
  real(rkp), dimension(:,:), intent(inout)                            ::    Samples
  integer, intent(in)                                                 ::    NbSamples
  integer, intent(in)                                                 ::    NbDim
  integer, optional, intent(in)                                       ::    DrawType

  character(*), parameter                                             ::    ProcName='DrawMat'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    ii
  integer                                                             ::    DrawTypeLoc

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  if (present(DrawType)) then 
    DrawTypeLoc = DrawType
  else
    DrawTypeLoc = This%DrawType
  end if

  if (size(Samples,1) /= NbDim) call Error%Raise('Incompatible array', ProcName=ProcName)
  if (size(Samples,2) /= NbSamples) call Error%Raise('Incompatible array', ProcName=ProcName)

  i = 1
  ii = 1
  select case (DrawTypeLoc)
    case (1)
      ii = 1
      do ii = 1, size(Samples,2)
        do i = 1, size(Samples,1)
          Samples(i,ii) = real(This%PRNG%genrand64_real1(),rkp)
        end do
      end do
    case (2)
      ii = 1
      do ii = 1, size(Samples,2)
        do i = 1, size(Samples,1)
          Samples(i,ii) = real(This%PRNG%genrand64_real2(),rkp)
        end do
      end do
    case (3)
      ii = 1
      do ii = 1, size(Samples,2)
        do i = 1, size(Samples,1)
          Samples(i,ii) = real(This%PRNG%genrand64_real3(),rkp)
        end do
      end do
    case default
      call Error%Raise(Line='Something went wrong when selecting draw type', ProcName=ProcName)
  end select

end subroutine
!!----------------------------------------------------------------------------------------------------------------------------!!

!!----------------------------------------------------------------------------------------------------------------------------!!
impure elemental subroutine Copy(LHS, RHS)

  class(RandPseudo_Type), intent(out)                                 ::    LHS
  class(Randpseudo_Type), intent(in)                                  ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  call LHS%Reset()
  LHS%Initialized = RHS%Initialized
  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%Seed = RHS%Seed
    LHS%SeedDefault = RHS%SeedDefault
    LHS%PRNG = RHS%PRNG
    LHS%DrawType = RHS%DrawType
  end if

end subroutine
!!----------------------------------------------------------------------------------------------------------------------------!!

!!----------------------------------------------------------------------------------------------------------------------------!!
impure elemental subroutine Finalizer(This)

  type(RandPseudo_Type), intent(inout)                                ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

end subroutine
!!----------------------------------------------------------------------------------------------------------------------------!!

end module
