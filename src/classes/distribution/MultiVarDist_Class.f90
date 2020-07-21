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

module MultiVarDist_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringConversion_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use DistProbContainer_Class                                       ,only:    DistProbContainer_Type
use DistProb_Class                                                ,only:    DistProb_Type
use DistProb_Factory_Class                                        ,only:    DistProb_Factory
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    MultiVarDist_Type

type                                                                  ::    MultiVarDist_Type
  logical                                                             ::    Constructed=.false.
  type(DistProbContainer_Type), allocatable, dimension(:)             ::    Distributions
  real(rkp), dimension(:,:), allocatable                              ::    CorrMat
  integer                                                             ::    NbDim
  logical                                                             ::    Correlated
contains
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1,         &
                                                                                                          ConstructCase2,         &
                                                                                                          ConstructCase3
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    ConstructCase2
  procedure, public                                                   ::    ConstructCase3
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    PDF
  procedure, public                                                   ::    GetNbDim
  procedure, public                                                   ::    IsConstructed
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(MultiVarDist_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed = .false.

  if (allocated(This%Distributions)) deallocate(This%Distributions, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Distributions', ProcName=ProcName, stat=StatLoc)
  This%NbDim = 0

  if (allocated(This%CorrMat)) deallocate(This%CorrMat, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)
  This%Correlated = .false.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(MultiVarDist_Type), intent(inout)                             ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  logical                                                             ::    Found
  integer                                                             ::    i
  character(:), allocatable                                           ::    PrefixLoc
  class(DistProb_Type), allocatable                                   ::    DistProb
  type(InputVerifier_Type)                                            ::    InputVerifier 

  call This%Reset()
  
  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  SectionName = 'distributions'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  This%NbDim = InputSection%GetNumberOfSubSections()

  allocate(This%Distributions(This%NbDim), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Distributions', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, This%NbDim
    SubSectionName = SectionName // '>distribution' // ConvertToString(Value=i)
    call InputVerifier%AddSection(Section='distribution' // ConvertToString(Value=i), ToSubSection=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    call DistProb_Factory%Construct(Object=DistProb, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)
    call This%Distributions(i)%Set(Object=DistProb)
    deallocate(DistProb, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='DistProb', ProcName=ProcName, stat=StatLoc)
  end do 

  SectionName = 'correlation_matrix'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName=SectionName)) then
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call ImportArray(Input=InputSection, Array=This%CorrMat, Prefix=PrefixLoc)
    nullify(InputSection)
    This%Correlated = .not. IsDiagonal(Array=This%CorrMat)
  else
    call Eye(Array=This%CorrMat)
    This%Correlated = .false.
  end if

  if (size(This%Corrmat,1) /= This%NbDim .or. size(This%CorrMat,2) /= This%NbDim) call Error%Raise(                         &
                                                      Line='Improper sizes for the input correlation matrix', ProcName=ProcName) 

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()
                                                      
  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------         
subroutine ConstructCase1(This, Distributions, CorrMat)

  class(MultiVarDist_Type), intent(inout)                             ::    This
  type(DistProbContainer_Type), dimension(:), intent(in)                   ::    Distributions
  real(rkp), dimension(:,:), optional, intent(in)                     ::    CorrMat
  
  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  call This%Reset()

  This%NbDim = size(Distributions,1)

  allocate(This%Distributions, source=Distributions, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Distributions', ProcName=ProcName, stat=StatLoc)

  allocate(This%CorrMat(This%NbDim,This%NbDim), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)

  if (present(CorrMat)) then
    if (size(CorrMat,1) /= This%NbDim .or. size(CorrMat,1) /= This%NbDim) call Error%Raise(Line='Incorrect shape' //         &
                                                                                ' of the correlation matrix', ProcName=ProcName)
    This%CorrMat = CorrMat
    This%Correlated = .not. IsDiagonal(Array=This%CorrMat)
  else
    call Eye(Array=This%CorrMat)
    This%Correlated = .false.
  end if

  This%Constructed=.true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------         
subroutine ConstructCase2(This, Distributions, CorrMat)

  class(MultiVarDist_Type), intent(inout)                             ::    This
  class(DistProb_Type), dimension(:), intent(in)                      ::    Distributions
  real(rkp), dimension(:,:), optional, intent(in)                     ::    CorrMat
  
  character(*), parameter                                             ::    ProcName='ConstructCase2'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  call This%Reset()

  This%NbDim = size(Distributions,1)

  allocate(This%Distributions(This%NbDim), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Distributions', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, This%NbDim
    call This%Distributions(i)%Set(Object=Distributions(i))
  end do

  allocate(This%CorrMat(This%NbDim,This%NbDim), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)

  if (present(CorrMat)) then
    if (size(CorrMat,1) /= This%NbDim .or. size(CorrMat,1) /= This%NbDim) call Error%Raise(Line='Incorrect shape' //         &
                                                                                ' of the correlation matrix', ProcName=ProcName)
    This%CorrMat = CorrMat
    This%Correlated = .not. IsDiagonal(Array=This%CorrMat)
  else
    call Eye(Array=This%CorrMat)
    This%Correlated = .false.
  end if

  This%Constructed=.true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------         
subroutine ConstructCase3(This, SampleSpace)

  class(MultiVarDist_Type), intent(inout)                             ::    This
  class(SampleSpace_Type), intent(in)                                 ::    SampleSpace
  
  character(*), parameter                                             ::    ProcName='ConstructCase3'
  integer                                                             ::    StatLoc=0

  call This%Reset()

  This%NbDim = SampleSpace%GetNbDim()

  allocate(This%Distributions(This%NbDim), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='TThis%Distributions', ProcName=ProcName, stat=StatLoc)
  call SampleSpace%GetDistributions(Distributions=This%Distributions)

  allocate(This%CorrMat(This%NbDim,This%NbDim), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)
  call SampleSpace%GetCorrMat(CorrMat=This%CorrMat)

  This%Correlated = SampleSpace%IsCorrelated()

  This%Constructed=.true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput

  class(MultiVarDist_Type), intent(in)                                ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  type(SMUQFile_Type)                                                 ::    File
  character(:), allocatable                                           ::    FileName
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  class(DistProb_Type), pointer                                       ::    DistProb=>null()
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  SectionName = 'distributions'
  call GetInput%AddSection(SectionName=SectionName)
  i = 1
  do i = 1, This%NbDim
    DistProb => This%Distributions(i)%GetPointer()
    if (ExternalFlag) DirectorySub = DirectoryLoc // 'distribution' // ConvertToString(i) // '/'
    call GetInput%AddSection(Section=DistProb_Factory%GetObjectInput(Object=DistProb, Name='distribution' //       &
                                ConvertToString(Value=i), Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SectionName)
    nullify(DistProb)
  end do

  SectionName='correlation_matrix'
  call GetInput%AddSection(SectionName=SectionName)
  call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  if (ExternalFlag) then
    FileName = DirectoryLoc // 'correlation_matrix.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Input=InputSection, Array=This%CorrMat, File=File)
  else
    call ExportArray(Input=InputSection, Array=This%CorrMat)
  end if
  nullify(InputSection)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function PDF(This, X)

  real(rkp)                                                           ::    PDF

  class(MultiVarDist_Type), intent(in)                                ::    This
  real(rkp), dimension(:), intent(in)                                 ::    X

  character(*), parameter                                             ::    ProcName='PDF'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  class(DistProb_Type), pointer                                       ::    DistProb=>null()

  if (This%Correlated) call Error%Raise('Correlated multivariate non-normal distribution pdf is not yet supported',          &
                                                                                                              ProcName=ProcName)

  PDF = One

  i = 1
  do i = 1, This%NbDim
    DistProb => This%Distributions(i)%GetPointer()
    PDF = PDF * DistProb%PDF(X=X(i))
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetNbDim(This)

  real(rkp)                                                           ::    GetNbDim

  class(MultiVarDist_Type), intent(in)                                ::    This

  character(*), parameter                                             ::    ProcName='GetNbDim'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetNbDim = This%NbDim

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function IsConstructed(This)

  logical                                                             ::    IsConstructed

  class(MultiVarDist_Type), intent(in)                                ::    This

  character(*), parameter                                             ::    ProcName='IsConstructed'
  integer                                                             ::    StatLoc=0

  IsConstructed = This%Constructed

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(MultiVarDist_Type), intent(out)                               ::    LHS
  class(MultiVarDist_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (MultiVarDist_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        allocate(LHS%Distributions, source=RHS%Distributions, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Distributions', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%CorrMat, source=RHS%CorrMat, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%CorrMat', ProcName=ProcName, stat=StatLoc)
        LHS%NbDim = RHS%NbDim
        LHS%Correlated = RHS%Correlated
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(MultiVarDist_Type), intent(inout)                              ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Distributions)) deallocate(This%Distributions, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Distributions', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%CorrMat)) deallocate(This%CorrMat, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
