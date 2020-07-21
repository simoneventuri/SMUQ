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

module MultiVarDistNorm_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringConversion_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    MultiVarDistNorm_Type

type                                                                  ::    MultiVarDistNorm_Type
  logical                                                             ::    Constructed=.false.
  real(rkp), dimension(:), allocatable                                ::    Mu
  real(rkp), dimension(:,:), allocatable                              ::    Cov
  logical                                                             ::    Truncated
  integer                                                             ::    NbDim
contains
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  generic, public                                                     ::    PDF                     =>    PDF_Full,               &
                                                                                                          PDF_Cholesky
  procedure, private                                                  ::    PDF_Full
  procedure, private                                                  ::    PDF_Cholesky
  procedure, nopass, public                                           ::    ComputePDF_Cholesky
  procedure, nopass, public                                           ::    ComputePDF
  generic, public                                                     ::    LogPDF                  =>    LogPDF_Cholesky
  procedure, private                                                  ::    LogPDF_Cholesky
  procedure, nopass, public                                           ::    LogComputePDF_Cholesky
  procedure, public                                                   ::    GetMu
  procedure, public                                                   ::    GetCov
  procedure, public                                                   ::    IsTruncated
  procedure, public                                                   ::    GetNbDim
  procedure, public                                                   ::    IsConstructed
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(MultiVarDistNorm_Type), intent(inout)                         ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed = .false.

  if (allocated(This%Mu)) deallocate(This%Mu, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Mu', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Cov)) deallocate(This%Cov, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Cov', ProcName=ProcName, stat=StatLoc)

  This%Truncated = .false.
  This%NbDim = 0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(MultiVarDistNorm_Type), intent(inout)                         ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  logical                                                             ::    Found
  character(:), allocatable                                           ::    VarC0D
  real(rkp)                                                           ::    VarR0D
  logical                                                             ::    VarL0D
  integer                                                             ::    VarI0D
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  integer                                                             ::    i
  character(:), allocatable                                           ::    PrefixLoc
  type(InputVerifier_Type)                                            ::    InputVerifier 

  call This%Reset()
  
  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  ParameterName = 'mean'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  call ConvertToReals(String=VarC0D, Values=VarR1D)
  allocate(This%Mu, source=VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Mu', ProcName=ProcName, stat=StatLoc)

  This%NbDim = size(This%Mu,1)

  SectionName = 'covariance'
  call InputVerifier%AddSection(Section=SectionName)
  ParameterName = 'format'
  call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
  call Input%GetValue(VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
  SubSectionName = SectionName // '>format'
  call InputVerifier%AddSection(Section='format', ToSubSection=SectionName)

  select case (VarC0D)
    case ('diagonals')
      ParameterName = 'values'
      call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SubSectionName)
      call Input%GetValue(VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
      call ConvertToReals(String=VarC0D, Values=VarR1D)
      if (size(VarR1D,1) /= This%NbDim) call Error%Raise(Line='Mismatching number of diagonal terms', ProcName=ProcName)
      allocate(This%Cov(This%NbDim,This%NbDim), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%Cov', ProcName=ProcName, stat=StatLoc)
      This%Cov = Zero
      i = 1
      do i = 1, This%NbDim
        This%Cov(i,i) = VarR1D(i)
      end do
    case ('full')
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=VarR2D, Prefix=PrefixLoc)
      allocate(This%Cov, source=VarR2D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%Cov', ProcName=ProcName, stat=StatLoc)
      deallocate(VarR2D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
    case default
      call Error%Raise(Line='Specified unknown format for covariance matrix', ProcName=ProcName)
  end select

  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()
  
  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, Mu, Cov)
  
  class(MultiVarDistNorm_Type), intent(inout)                         ::    This
  real(rkp), dimension(:), intent(in)                                 ::    Mu
  real(rkp), dimension(:,:), intent(in)                               ::    Cov 

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  if (This%NbDim == size(Mu,1) .and. This%NbDim == size(Cov,1) .and. This%NbDim == size(Cov,2)) then
    This%Mu = Mu
    This%Cov = Cov
  else
    call This%Reset()

    allocate(This%Mu, source=Mu, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Mu', ProcName=ProcName, stat=StatLoc)  

    This%NbDim = size(This%Mu,1)

    if (size(Cov,1) /= This%NbDim .or. size(Cov,2) /= This%NbDim) call Error%Raise(Line='Incorrect dimension Cov matrix',    &
                                                                                                              ProcName=ProcName)

    allocate(This%Cov, source=Cov, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Cov', ProcName=ProcName, stat=StatLoc)
  end if

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput

  class(MultiVarDistNorm_Type), intent(in)                            ::    This
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

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))
  call GetInput%AddParameter(Name='mean', Value=ConvertToString(Values=This%Mu))

  SectionName = 'covariance'
  call GetInput%AddSection(SectionName=SectionName)
  call GetInput%AddParameter(Name='format', Value='full', SectionName=SectionName)
  SubSectionName = 'format'
  call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
  call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,             &
                                                                                                              Mandatory=.true.)
  if (ExternalFlag) then
    FileName = DirectoryLoc // 'cov.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Input=InputSection, Array=This%Cov, File=File)
  else
    call ExportArray(Input=InputSection, Array=This%Cov)
  end if
  nullify(InputSection)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function PDF_Full(This, X)

  real(rkp)                                                           ::    PDF_Full

  class(MultiVarDistNorm_Type), intent(in)                            ::    This
  real(rkp), dimension(:), intent(in)                                 ::    X

  character(*), parameter                                             ::    ProcName='PDF_Full'
  integer                                                             ::    StatLoc=0

  PDF_Full = This%ComputePDF(X=X, Mu=This%Mu, Cov=This%Cov)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function PDF_Cholesky(This, X, L)

  real(rkp)                                                           ::    PDF_Cholesky

  class(MultiVarDistNorm_Type), intent(in)                            ::    This
  real(rkp), dimension(:), intent(in)                                 ::    X
  real(rkp), contiguous, dimension(:,:), intent(in)                   ::    L

  character(*), parameter                                             ::    ProcName='PDF_Cholesky'
  integer                                                             ::    StatLoc=0

  PDF_Cholesky = This%ComputePDF_Cholesky(X=X, Mu=This%Mu, Cov=This%Cov, L=L)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function ComputePDF(X, Mu, Cov)

  real(rkp)                                                           ::    ComputePDF

  real(rkp), dimension(:), intent(in)                                 ::    X
  real(rkp), dimension(:), intent(in)                                 ::    Mu
  real(rkp), dimension(:,:), intent(in)                               ::    Cov

  character(*), parameter                                             ::    ProcName='ComputePDF'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:,:)                              ::    L
  integer                                                             ::    NbDim
  integer                                                             ::    i
  real(rkp), allocatable, dimension(:,:)                              ::    XmMean
    
  NbDim = size(Mu,1)

  allocate(L, source=Cov, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='L', ProcName=ProcName, stat=StatLoc)    

  call DPOTRF('L', NbDim, L, NbDim, StatLoc)
  if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DPOTRF', ProcName=ProcName)

  ComputePDF = Zero

  i = 1
  do i = 1, NbDim
    ComputePDF = ComputePDF + dlog(L(i,i))
  end do

  allocate(XmMean(NbDim,1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='XmMean', ProcName=ProcName, stat=StatLoc)

  XmMean(:,1) = X - Mu

  call DTRTRS('L', 'N', 'N', NbDim, 1, L, NbDim, XmMean, NbDim, StatLoc)
  if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DTRTRS with code: ' // ConvertToString(Value=StatLoc),    &
                                                                                                              ProcName=ProcName)
  
  ComputePDF = - real(NbDim,rkp)/Two * dlog(Two*Pi) - ComputePDF - 0.5 * dot_product(XmMean(:,1), XmMean(:,1))

  ComputePDF = dexp(ComputePDF)

  deallocate(XmMean, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='XmMean(NbDim)', ProcName=ProcName, stat=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function ComputePDF_Cholesky(X, Mu, Cov, L)

  real(rkp)                                                           ::    ComputePDF_Cholesky

  real(rkp), dimension(:), intent(in)                                 ::    X
  real(rkp), dimension(:), intent(in)                                 ::    Mu
  real(rkp), dimension(:,:), intent(in)                               ::    Cov
  real(rkp), contiguous, dimension(:,:), intent(in)                   ::    L

  character(*), parameter                                             ::    ProcName='ComputePDF_Cholesky'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:,:)                              ::    XmMean
  integer                                                             ::    NbDim
  integer                                                             ::    i
    
  NbDim = size(Mu,1)

  ComputePDF_Cholesky = Zero

  i = 1
  do i = 1, NbDim
    ComputePDF_Cholesky = ComputePDF_Cholesky + dlog(L(i,i))
  end do

  allocate(XmMean(NbDim,1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='XmMean', ProcName=ProcName, stat=StatLoc)

  XmMean(:,1) = X - Mu

  call DTRTRS('L', 'N', 'N', NbDim, 1, L, NbDim, XmMean, NbDim, StatLoc)
  if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DTRTRS with code: ' // ConvertToString(Value=StatLoc),    &
                                                                                                              ProcName=ProcName)
  
  ComputePDF_Cholesky = - real(NbDim,rkp)/Two * dlog(Two*Pi) - ComputePDF_Cholesky - 0.5 * dot_product(XmMean(:,1), XmMean(:,1))

  ComputePDF_Cholesky = dexp(ComputePDF_Cholesky)

  deallocate(XmMean, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='XmMean(NbDim)', ProcName=ProcName, stat=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function LogPDF_Cholesky(This, X, L)

  real(rkp)                                                           ::    LogPDF_Cholesky

  class(MultiVarDistNorm_Type), intent(in)                            ::    This
  real(rkp), dimension(:), intent(in)                                 ::    X
  real(rkp), contiguous, dimension(:,:), intent(in)                   ::    L

  character(*), parameter                                             ::    ProcName='LogPDF_Cholesky'
  integer                                                             ::    StatLoc=0

  LogPDF_Cholesky = This%LogComputePDF_Cholesky(X=X, Mu=This%Mu, Cov=This%Cov, L=L)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function LogComputePDF_Cholesky(X, Mu, Cov, L)

  real(rkp)                                                           ::    LogComputePDF_Cholesky

  real(rkp), dimension(:), intent(in)                                 ::    X
  real(rkp), dimension(:), intent(in)                                 ::    Mu
  real(rkp), dimension(:,:), intent(in)                               ::    Cov
  real(rkp), contiguous, dimension(:,:), intent(in)                   ::    L

  character(*), parameter                                             ::    ProcName='LogComputePDF_Cholesky'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:,:)                              ::    XmMean
  integer                                                             ::    NbDim
  integer                                                             ::    i
    
  NbDim = size(Mu,1)

  LogComputePDF_Cholesky = Zero

  i = 1
  do i = 1, NbDim
    LogComputePDF_Cholesky = LogComputePDF_Cholesky + dlog(L(i,i))
  end do

  allocate(XmMean(NbDim,1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='XmMean', ProcName=ProcName, stat=StatLoc)

  XmMean(:,1) = X - Mu

  call DTRTRS('L', 'N', 'N', NbDim, 1, L, NbDim, XmMean, NbDim, StatLoc)
  if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DTRTRS with code: ' // ConvertToString(Value=StatLoc),    &
                                                                                                              ProcName=ProcName)
  
  LogComputePDF_Cholesky = - real(NbDim,rkp)/Two * dlog(Two*Pi) - LogComputePDF_Cholesky - 0.5 *                                &
                                                                                            dot_product(XmMean(:,1), XmMean(:,1))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetMu(This, Mu)

  class(MultiVarDistNorm_Type), intent(in)                            ::    This
  real(rkp), allocatable, dimension(:), intent(inout)                 ::    Mu

  character(*), parameter                                             ::    ProcName='GetMu'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (allocated(Mu)) then
    if (size(Mu) /= This%NbDim) then
      deallocate(Mu, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Mu', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  
  if (.not. allocated(Mu)) then
    allocate(Mu(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Mu', ProcName=ProcName, stat=StatLoc)
  end if

  Mu = This%Mu

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetCov(This, Cov)

  class(MultiVarDistNorm_Type), intent(in)                            ::    This
  real(rkp), allocatable, dimension(:,:), intent(inout)               ::    Cov

  character(*), parameter                                             ::    ProcName='GetCov'
  integer                                                             ::    StatLoc=0


  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (allocated(Cov)) then
    if (size(Cov,1) /= This%NbDim .or. size(Cov,2) /= This%NbDIm) then
      deallocate(Cov, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Cov', ProcName=ProcName, stat=StatLoc)
    end if
  end if

  if (.not. allocated(Cov)) then
    allocate(Cov(This%NbDim,This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Cov', ProcName=ProcName, stat=StatLoc)
  end if

  Cov = This%Cov

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function IsTruncated(This)

  logical                                                             ::    IsTruncated

  class(MultiVarDistNorm_Type), intent(in)                            ::    This

  character(*), parameter                                             ::    ProcName='IsTruncated'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  IsTruncated = This%Truncated

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetNbDim(This)

  real(rkp)                                                           ::    GetNbDim

  class(MultiVarDistNorm_Type), intent(in)                            ::    This

  character(*), parameter                                             ::    ProcName='GetNbDim'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetNbDim = This%NbDim

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function IsConstructed(This)

  logical                                                             ::    IsConstructed

  class(MultiVarDistNorm_Type), intent(in)                            ::    This

  character(*), parameter                                             ::    ProcName='IsConstructed'
  integer                                                             ::    StatLoc=0

  IsConstructed = This%Constructed

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(MultiVarDistNorm_Type), intent(out)                           ::    LHS
  class(MultiVarDistNorm_Type), intent(in)                            ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (MultiVarDistNorm_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        allocate(LHS%Mu, source=RHS%Mu, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Mu', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%Cov, source=RHS%Cov, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Cov', ProcName=ProcName, stat=StatLoc)
        LHS%Truncated = RHS%Truncated
        LHS%NbDim = RHS%NbDIm
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(MultiVarDistNorm_Type), intent(inout)                          ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Mu)) deallocate(This%Mu, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Mu', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Cov)) deallocate(This%Cov, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Cov', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
