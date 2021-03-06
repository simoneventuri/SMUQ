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

module DistUnif_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringConversion_Module
use DistProb_Class                                                ,only:    DistProb_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    DistUnif_Type

type, extends(DistProb_Type)                                          ::    DistUnif_Type

contains
  private
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    PDF
  procedure, nopass, public                                           ::    ComputeUnifPDF
  procedure, public                                                   ::    CDF
  procedure, nopass, public                                           ::    ComputeUnifCDF
  procedure, public                                                   ::    InvCDF
  procedure, nopass, public                                           ::    ComputeUnifInvCDF
  procedure, public                                                   ::    GetMoment
  procedure, public                                                   ::    WriteInfo
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(DistUnif_Type), intent(inout)                                 ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed = .false.

  This%A = Zero
  This%B = One
  This%TruncatedLeft=.true.
  This%TruncatedRight=.true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(DistUnif_Type), intent(inout)                                 ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    ParameterName
  logical                                                             ::    Found
  real(rkp)                                                           ::    VarR0D
  character(:), allocatable                                           ::    VarC0D
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  type(InputVerifier_Type)                                            ::    InputVerifier 

  call This%Reset()
  
  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  This%TruncatedLeft=.true.
  This%TruncatedRight=.true.

  ParameterName = 'a'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(VarR0D, ParameterName=ParameterName, Mandatory=.true.)
  This%A = VarR0D

  ParameterName = 'b'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(VarR0D, ParameterName=ParameterName, Mandatory=.true.)
  This%B = VarR0D

  if (This%B < This%A) call Error%Raise(Line='Upper limit < lower limit', ProcName=ProcName)

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()
  
  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, A, B)
  
  class(DistUnif_Type), intent(inout)                                 ::    This
  real(rkp), intent(in)                                               ::    A
  real(rkp), intent(in)                                               ::    B 

  character(*), parameter                                             ::    ProcName='ConstructCase1'

  call This%Reset()

  This%TruncatedLeft=.true.
  This%TruncatedRight=.true.

  This%A = A

  This%B = B

  if (This%B < This%A) call Error%Raise(Line='Upper limit < lower limit', ProcName=ProcName)

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput

  class(DistUnif_Type), intent(in)                                    ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddParameter(Name='a', Value=ConvertToString(Value=This%A))
  call GetInput%AddParameter(Name='b', Value=ConvertToString(Value=This%B))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function PDF(This, X)

  real(rkp)                                                           ::    PDF

  class(DistUnif_Type), intent(in)                                    ::    This
  real(rkp), intent(in)                                               ::    X

  character(*), parameter                                             ::    ProcName='PDF'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  PDF = This%ComputeUnifPDF(X, This%A, This%B)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!  !!------------------------------------------------------------------------------------------------------------------------------
!  function PDF_R2D(This, NbNodes)

!    use ComputingRoutines_Module

!    real(rkp), dimension(:,:), allocatable                              ::    PDF_R2D

!    class(DistUnif_Type), intent(in)                                    ::    This
!    integer, intent(in)                                                 ::    NbNodes

!    character(*), parameter                                             ::    ProcName='PDF_R2D'
!    real(rkp)                                                           ::    BinMass
!    integer                                                             ::    StatLoc=0
!    integer                                                             ::    i

!    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

!    if (NbNodes < 3) call Error%Raise(Line='Specified number of points lower than minimum of 3', ProcName=ProcName)

!    BinMass = One / real(NbNodes-1,rkp)

!    allocate(PDF_R2D(NbNodes,2), stat=StatLoc)
!    if (StatLoc /= 0) call Error%Allocate(Name='PDF_R2D', ProcName=ProcName, stat=StatLoc)

!    PDF_R2D(:,1) = LinSpace(InterMin=This%A, InterMax=This%B, NbNodes=NbNodes)
!    PDF_R2D(:,2) = One / (This%B - This%A)

!  end function
!  !!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function ComputeUnifPDF(X, A, B)

  real(rkp)                                                           ::    ComputeUnifPDF

  real(rkp), intent(in)                                               ::    X
  real(rkp), intent(in)                                               ::    A
  real(rkp), intent(in)                                               ::    B

  character(*), parameter                                             ::    ProcName='ComputeUnifPDF'

  if (X < A .or. X > B) then
    ComputeUnifPDF = Zero
  else
    ComputeUnifPDF = One/(B-A)
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function CDF(This, X)

  real(rkp)                                                           ::    CDF

  class(DistUnif_Type), intent(in)                                    ::    This
  real(rkp), intent(in)                                               ::    X

  character(*), parameter                                             ::    ProcName='CDF'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  CDF = ComputeUnifCDF(X, This%A, This%B)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function ComputeUnifCDF(X, A, B)

  real(rkp)                                                           ::    ComputeUnifCDF

  real(rkp), intent(in)                                               ::    X
  real(rkp), intent(in)                                               ::    A
  real(rkp), intent(in)                                               ::    B

  character(*), parameter                                             ::    ProcName='ComputeUnifCDF'

  if (X < A) then
    ComputeUnifCDF = Zero
  elseif (X > B) then
    ComputeUnifCDF = One
  else
    ComputeUnifCDF = (X-A) / (B-A)
  end if      

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function InvCDF(This, P)

  real(rkp)                                                           ::    InvCDF

  class(DistUnif_Type), intent(in)                                    ::    This
  real(rkp), intent(in)                                               ::    P

  character(*), parameter                                             ::    ProcName='InvCDF'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  InvCDF = ComputeUnifInvCDF(P, This%A, This%B)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function ComputeUnifInvCDF(P, A, B)

  real(rkp)                                                           ::    ComputeUnifInvCDF

  real(rkp), intent(in)                                               ::    P
  real(rkp), intent(in), optional                                     ::    A
  real(rkp), intent(in), optional                                     ::    B

  character(*), parameter                                             ::    ProcName='ComputeUnifInvCDF'
  real(rkp)                                                           ::    CDFLeft
  real(rkp)                                                           ::    CDFRight
  real(rkp)                                                           ::    VarR0D

  if (P < Zero) call Error%Raise(Line='P value below the minimum of 0 in the inverse CDF calculation', ProcName=ProcName)
  if (P > One) call Error%Raise(Line='P value above the maximum of 1 in the inverse CDF calculation', ProcName=ProcName)

  ComputeUnifInvCDF = A + P*(B-A)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetMoment(This, Moment)

  real(rkp)                                                           ::    GetMoment

  class(DistUnif_Type), intent(in)                                    ::    This
  integer, intent(in)                                                 ::    Moment

  character(*), parameter                                             ::    ProcName='GetMoment'
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (Moment < 0) call Error%Raise("Requested a distribution moment below 0", ProcName=ProcName)

  if (Moment > 0) then
    i = 0
    GetMoment = Zero
    do i = 0, Moment
      GetMoment = GetMoment + This%A**i*This%B**(Moment-i)
    end do
    GetMoment = GetMoment / (real(Moment,rkp)+One)
  else
    GetMoment = One
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine WriteInfo(This, File)

  class(DistUnif_Type), intent(in)                                    ::    This
  type(SMUQFile_Type), intent(inout)                                  ::    File

  character(*), parameter                                             ::    ProcName='WriteInfo'
  integer                                                             ::    i
  type(SMUQString_Type), dimension(3)                                 ::    Strings

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  Strings(1) = 'uniform'
  Strings(2) = ConvertToString(Value=This%A)
  Strings(3) = ConvertToString(Value=This%B)

  call File%Append(Strings=Strings)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(DistUnif_Type), intent(out)                                   ::    LHS
  class(DistProb_Type), intent(in)                                    ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (DistUnif_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%A = RHS%A
        LHS%B = RHS%B
        LHS%TruncatedLeft=RHS%TruncatedLeft
        LHS%TruncatedRight=RHS%TruncatedRight
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(DistUnif_Type), intent(inout)                                  ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
