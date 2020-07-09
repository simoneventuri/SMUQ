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

module Histogram_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use ArrayIORoutines_Module
use ArrayRoutines_Module
use StringConversion_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    Histogram_Type
public                                                                ::    BinValues

type                                                                  ::    Histogram_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbBins
  real(rkp), allocatable, dimension(:)                                ::    BinEdges
  integer, allocatable, dimension(:)                                  ::    BinCounts
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  generic, public                                                     ::    Bin                     =>    Bin_R0D,                &
                                                                                                          Bin_R1D
  procedure, public                                                   ::    Bin_R0D
  procedure, public                                                   ::    Bin_R1D
  procedure, public                                                   ::    GetBinCounts
  procedure, public                                                   ::    GetBinCountsPointer
  procedure, public                                                   ::    GetBinEdges
  procedure, public                                                   ::    GetBinEdgesPointer
  procedure, public                                                   ::    GetNbBins
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

interface BinValues
  module procedure                                                    ::    BinValues_R0D
  module procedure                                                    ::    BinValues_R1D
end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(Histogram_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    if (.not. This%Initialized) then
      This%Initialized = .true.
      This%Name = 'response'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(Histogram_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if (allocated(This%BinEdges)) deallocate(This%BinEdges, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%BinEdges', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%BinCounts)) deallocate(This%BinCounts, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%BinCounts', ProcName=ProcName, stat=StatLoc)

    This%NbBins = 0

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(Histogram_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(Histogram_Type), intent(inout)                              ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    character(:), allocatable                                         ::    VarC0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer, allocatable, dimension(:)                                ::    VarI1D
    type(InputSection_Type), pointer                                  ::    InputSection=>null()

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()
    
    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix
    
    ParameterName = 'name'
    call Input%GetValue(Value=VarC0D, ParameterName=Parametername, Mandatory=.false., Found=Found)
    if (Found) This%Name = VarC0D

    SectionName = 'bin_edges'
    ParameterName = 'source'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
    SubSectionName = SectionName // '>source'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    select case (VarC0D)
      case('computed')
        call InterSpace(Input=InputSection, Values=This%BinEdges)
      case('imported')
        call ImportArray(Input=InputSection, Array=This%BinEdges, Prefix=PrefixLoc)
      case default
        call Error%Raise(Line='Source not recognized', ProcName=ProcName)
    end select

    This%NbBins = size(This%BinEdges,1)-1

    SectionName = 'bin_counts'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=VarI1D, Prefix=PrefixLoc)
      allocate(This%BinCounts, source=VarI1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%BinCounts', ProcName=ProcName, stat=StatLoc)
      deallocate(VarI1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
      if (size(This%BinCounts,1) /= This%NbBins) call Error%Raise('Mismatch between size of bincounts and number of bins',     &
                                                                                                               ProcName=ProcName)
    else
      allocate(This%BinCounts(This%NbBins), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%BinCounts', ProcName=ProcName, stat=StatLoc)
      This%BinCounts = 0
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(Histogram_Type), intent(in)                                 ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    integer                                                           ::    i

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    if (len_trim(This%Name) /= 0) call GetInput%AddParameter(Name='name', Value=This%Name)

    SectionName = 'bin_edges'
    call GetInput%AddSection(SectionName=SectionName)
    call GetInput%AddParameter(Name='source', Value='imported', SectionName=SectionName)
    SubSectionName = 'source'
    call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
    call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                              Mandatory=.true.)
    if (ExternalFlag) then
      FileName = DirectoryLoc // '/bin_edges.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call ExportArray(Input=InputSection, Array=This%BinEdges, File=File)
    else
      call ExportArray(Input=InputSection, Array=This%BinEdges)
    end if
    nullify(InputSection)

    SectionName = 'bin_counts'
    call GetInput%AddSection(SectionName=SectionName)
    call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    if (ExternalFlag) then
      FileName = DirectoryLoc // '/bin_counts.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call ExportArray(Input=InputSection, Array=This%BinCounts, File=File)
    else
      call ExportArray(Input=InputSection, Array=This%BinCounts)
    end if
    nullify(InputSection)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Bin_R0D(This, Value)

    class(Histogram_Type), intent(inout)                              ::    This
    real(rkp), intent(in)                                             ::    Value

    character(*), parameter                                           ::    ProcName='BinR0D'
    integer                                                           ::    StatLoc

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    This%BinCounts = 0

    call BinValues(Value=Value, BinEdges=This%BinEdges, BinCounts=This%BinCounts)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Bin_R1D(This, Values)

    class(Histogram_Type), intent(inout)                              ::    This
    real(rkp), dimension(:), intent(in)                               ::    Values

    character(*), parameter                                           ::    ProcName='BinR1D'
    integer                                                           ::    StatLoc

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    This%BinCounts = 0

    call BinValues(Values=Values, BinEdges=This%BinEdges, BinCounts=This%BinCounts)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine BinValues_R0D(Value, BinEdges, BinCounts)

    real(rkp), intent(in)                                             ::    Value
    real(rkp), dimension(:), intent(in)                               ::    BinEdges
    integer, dimension(:), intent(inout)                              ::    BinCounts

    character(*), parameter                                           ::    ProcName='BinValue'
    integer                                                           ::    StatLoc
    integer                                                           ::    NbBins
    integer                                                           ::    i

    if (size(BinCounts) /= size(BinEdges)-1) call Error%Raise('Mismatch between bin counts and number of bins',                &
                                                                                                               ProcName=ProcName)

    NbBins = size(BinCounts)

    if (Value < BinEdges(1)) then
      BinCounts(1) = BinCounts(1) + 1
    elseif (Value > BinEdges(NbBins+1)) then
      BinCounts(NbBins) = BinCounts(NbBins) + 1
    else
      i = 1
      do i = 1, NbBins
        if (Value < BinEdges(i+1) .and. Value >= BinEdges(i)) then
          BinCounts(i) = BinCounts(i) + 1
          exit
        end if
      end do
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine BinValues_R1D(Values, BinEdges, BinCounts)

    real(rkp), dimension(:), intent(in)                               ::    Values
    real(rkp), dimension(:), intent(in)                               ::    BinEdges
    integer, dimension(:), intent(inout)                              ::    BinCounts

    character(*), parameter                                           ::    ProcName='BinValues'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbBins
    integer                                                           ::    NbValues
    integer                                                           ::    i
    integer                                                           ::    ii

    if (size(BinCounts) /= size(BinEdges)-1) call Error%Raise('Mismatch between bin counts and number of bins',                &
                                                                                                               ProcName=ProcName)

    NbBins = size(BinCounts)
    NbValues = size(Values)

    ii = 1
    do ii = 1, NbValues
      if (Values(ii) < BinEdges(1)) then
        BinCounts(1) = BinCounts(1) + 1
      elseif (Values(ii) > BinEdges(NbBins+1)) then
        BinCounts(NbBins) = BinCounts(NbBins) + 1
      else
        i = 1
        do i = 1, NbBins
          if (Values(ii) < BinEdges(i+1) .and. Values(ii) >= BinEdges(i)) then
            BinCounts(i) = BinCounts(i) + 1
            exit
          end if
        end do
      end if
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetBinEdges(This)

    real(rkp), allocatable, dimension(:)                              ::    GetBinEdges

    class(Histogram_Type), intent(in)                                 ::    This

    character(*), parameter                                           ::    ProcName='GetBinEdges'
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    allocate(GetBinEdges, source=This%BinEdges, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='GetBinEdges', ProcName=ProcName, stat=StatLoc)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetBinEdgesPointer(This)

    real(rkp), pointer, dimension(:)                                  ::    GetBinEdgesPointer

    class(Histogram_Type), target, intent(in)                         ::    This

    character(*), parameter                                           ::    ProcName='GetBinEdgesPointer'
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    GetBinEdgesPointer => This%BinEdges

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetBinCounts(This)

    integer, allocatable, dimension(:)                                ::    GetBinCounts

    class(Histogram_Type), intent(in)                                 ::    This

    character(*), parameter                                           ::    ProcName='GetBinCounts'
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    allocate(GetBinCounts, source=This%BinCounts, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='GetBinCounts', ProcName=ProcName, stat=StatLoc)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetBinCountsPointer(This)

    integer, pointer, dimension(:)                                    ::    GetBinCountsPointer

    class(Histogram_Type), target, intent(in)                         ::    This

    character(*), parameter                                           ::    ProcName='GetBinCountsPointer'
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    GetBinCountsPointer => This%BinCounts

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName(This)

    character(:), allocatable                                         ::    GetName

    class(Histogram_Type), intent(in)                                 ::    This

    character(*), parameter                                           ::    ProcName='GetName'
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbBins(This)

    integer                                                           ::    GetNbBins

    class(Histogram_Type), intent(in)                                 ::    This

    character(*), parameter                                           ::    ProcName='GetNbBins'
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    GetNbBins = This%NbBins

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(Histogram_Type), intent(out)                                ::    LHS
    class(Histogram_Type), intent(in)                                 ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (Histogram_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%Name = RHS%Name
          LHS%NbBins = RHS%NbBins
          allocate(LHS%BinEdges, source=RHS%BinEdges, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%BinEdges', ProcName=ProcName, stat=StatLoc)
          allocate(LHS%BinCounts, source=RHS%BinCounts, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%BinCounts', ProcName=ProcName, stat=StatLoc)
        end if
      
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(Histogram_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%BinEdges)) deallocate(This%BinEdges, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%BinEdges', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%BinCounts)) deallocate(This%BinCounts, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%BinCounts', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
