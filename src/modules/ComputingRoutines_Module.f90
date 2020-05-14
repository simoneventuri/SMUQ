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

module ComputingRoutines_Module

use Parameters_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use RandPseudo_Class                                              ,only:    RandPseudo_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    IsFinite
public                                                                ::    LinSequence
public                                                                ::    LinSpace
public                                                                ::    Log10Space
public                                                                ::    LogSpace
public                                                                ::    Interpolate
public                                                                ::    DoubleFactorial
public                                                                ::    Factorial
public                                                                ::    SQRTFactorial
public                                                                ::    BinomialCoeff
public                                                                ::    ComputeEigenvalues
public                                                                ::    ComputeQR
public                                                                ::    ComputeQInvR
public                                                                ::    ComputeNorm
public                                                                ::    ScrambleArray
public                                                                ::    Transform
public                                                                ::    BernoulliNumbers
public                                                                ::    Pochhammer
public                                                                ::    RandomInteger

logical, parameter                                                    ::    DebugGlobal = .false.

interface IsFinite
  module procedure                                                    ::    IsFinite_R40D
  module procedure                                                    ::    IsFinite_R41D
  module procedure                                                    ::    IsFinite_R42D
  module procedure                                                    ::    IsFinite_R80D
  module procedure                                                    ::    IsFinite_R81D
  module procedure                                                    ::    IsFinite_R82D
end interface

interface Interpolate
  module procedure                                                    ::    Interpolate_R1D_R1D
  module procedure                                                    ::    Interpolate_R2D_R1D
  module procedure                                                    ::    Interpolate_R1D_R0D
  module procedure                                                    ::    Interpolate_R2D_R0D
end interface

interface Factorial
  module procedure                                                    ::    Factorial_I8
  module procedure                                                    ::    Factorial_I
  module procedure                                                    ::    Factorial_R
end interface

interface DoubleFactorial
  module procedure                                                    ::    DoubleFactorial_I
end interface

interface SQRTFactorial
  module procedure                                                    ::    SQRTFactorial_I8
  module procedure                                                    ::    SQRTFactorial_I
end interface

interface BinomialCoeff
  module procedure                                                    ::    BinomialCoeff_I8_I8
  module procedure                                                    ::    BinomialCoeff_I_I
  module procedure                                                    ::    BinomialCoeff_R_rkp_I
  module procedure                                                    ::    BinomialCoeff_R_rkp_I_ikp
end interface

interface ComputeEigenvalues ! square matrices
  module procedure                                                    ::    ComputeEigenvalues_REAL
  module procedure                                                    ::    ComputeEigenvalues_CMPLX
end interface

interface ComputeQR
  module procedure                                                    ::    ComputeQR_Matrix
  module procedure                                                    ::    ComputeQR_Q 
end interface

interface ComputeQInvR
  module procedure                                                    ::    ComputeQInvR_Matrix
  module procedure                                                    ::    ComputeQInvR_Q 
end interface

interface ComputeNorm
  module procedure                                                    ::    ComputeNorm_R1D_8
end interface

interface ScrambleArray
  module procedure                                                    ::    ScrambleArray_I1D
  module procedure                                                    ::    ScrambleArray_R1D
end interface

interface Transform
  module procedure                                                    ::    Transform_VarR0DChar
  module procedure                                                    ::    Transform_VarR0DString
  module procedure                                                    ::    Transform_VarR1DChar
  module procedure                                                    ::    Transform_VarR1DString
end interface

interface Pochhammer
  module procedure                                                    ::    Pochhammer_R0D_8
  module procedure                                                    ::    Pochhammer_R0D_4
  module procedure                                                    ::    Pochhammer_I0D_8
  module procedure                                                    ::    Pochhammer_I0D_4
end interface

interface RandomInteger
  module procedure                                                    ::    RandomInteger_I0D_4_RNG
  module procedure                                                    ::    RandomInteger_I0D_4_Intrinsic
  module procedure                                                    ::    RandomInteger_I0D_8_RNG
  module procedure                                                    ::    RandomInteger_I0D_8_Intrinsic
  module procedure                                                    ::    RandomInteger_I1D_4_RNG
  module procedure                                                    ::    RandomInteger_I1D_4_Intrinsic
  module procedure                                                    ::    RandomInteger_I1D_8_RNG
  module procedure                                                    ::    RandomInteger_I1D_8_Intrinsic
  module procedure                                                    ::    RandomInteger_I2D_4_RNG
  module procedure                                                    ::    RandomInteger_I2D_4_Intrinsic
  module procedure                                                    ::    RandomInteger_I2D_8_RNG
  module procedure                                                    ::    RandomInteger_I2D_8_Intrinsic
end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsFinite_R40D(Value)

    logical                                                           ::    IsFinite_R40D

    real(4), intent(in)                                               ::    Value

    real(4), parameter                                                ::    Big=huge(0.0_4)
    real(4), parameter                                                ::    Small=-huge(0.0_4)

    IsFinite_R40D = .false.
    if (Value >= Small .and. Value <= Big .and. Value == Value) IsFinite_R40D = .true.

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsFinite_R41D(Values)

    logical                                                           ::    IsFinite_R41D

    real(4), dimension(:), intent(in)                                 ::    Values

    real(4), parameter                                                ::    Big=huge(0.0_4)
    real(4), parameter                                                ::    Small=-huge(0.0_4)
    integer                                                           ::    i

    IsFinite_R41D = .true.

    i = 1
    do i = 1, size(Values,1)
      if (Values(i) >= Small .and. Values(i) <= Big .and. Values(i) == Values(i)) cycle
      IsFinite_R41D = .false.
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsFinite_R42D(Values)

    logical                                                           ::    IsFinite_R42D

    real(4), dimension(:,:), intent(in)                               ::    Values

    real(4), parameter                                                ::    Big=huge(0.0_4)
    real(4), parameter                                                ::    Small=-huge(0.0_4)
    integer                                                           ::    i
    integer                                                           ::    j
    integer                                                           ::    M 
    integer                                                           ::    N 

    M = size(Values,2)
    N = size(Values,1)

    IsFinite_R42D = .true.

    i = 1
    do i = 1, M
      j = 1
      do j = 1, N
        if (Values(j,i) >= Small .and. Values(j,i) <= Big .and. Values(j,i) == Values(j,i)) cycle
        IsFinite_R42D = .false.
      end do
      if (.not. IsFinite_R42D) exit
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsFinite_R80D(Value)

    logical                                                           ::    IsFinite_R80D

    real(8), intent(in)                                               ::    Value

    real(8), parameter                                                ::    Big=huge(0.0_8)
    real(8), parameter                                                ::    Small=-huge(0.0_8)

    IsFinite_R80D = .false.
    if (Value >= Small .and. Value <= Big .and. Value == Value) IsFinite_R80D = .true.

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
  function IsFinite_R81D(Values)

    logical                                                           ::    IsFinite_R81D

    real(8), dimension(:), intent(in)                                 ::    Values

    real(8), parameter                                                ::    Big=huge(0.0_8)
    real(8), parameter                                                ::    Small=-huge(0.0_8)
    integer                                                           ::    i

    IsFinite_R81D = .true.
    i = 1
    do i = 1, size(Values,1)
      if (Values(i) >= Small .and. Values(i) <= Big .and. Values(i) == Values(i)) cycle
      IsFinite_R81D = .false.
    end do
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsFinite_R82D(Values)

    logical                                                           ::    IsFinite_R82D

    real(8), dimension(:,:), intent(in)                               ::    Values

    real(8), parameter                                                ::    Big=huge(0.0_8)
    real(8), parameter                                                ::    Small=-huge(0.0_8)
    integer                                                           ::    i
    integer                                                           ::    j
    integer                                                           ::    M 
    integer                                                           ::    N 

    M = size(Values,2)
    N = size(Values,1)

    IsFinite_R82D = .true.

    i = 1
    do i = 1, M
      j = 1
      do j = 1, N
        if (Values(j,i) >= Small .and. Values(j,i) <= Big .and. Values(j,i) == Values(j,i)) cycle
        IsFinite_R82D = .false.
      end do
      if (.not. IsFinite_R82D) exit
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function LinSequence(SeqStart, SeqEnd, SeqSkip, Scrambled)
  
    integer, allocatable, dimension(:)                                ::    LinSequence

    integer, intent(in)                                               ::    SeqStart
    integer, intent(in)                                               ::    SeqEnd
    integer, optional, intent(in)                                     ::    SeqSkip
    logical, optional, intent(in)                                     ::    Scrambled

    character(*), parameter                                           ::    ProcName='LinSequence'
    integer                                                           ::    SeqSkipLoc
    logical                                                           ::    ScrambledLoc
    integer                                                           ::    NbNodes
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    SeqSkipLoc = 1
    if (present(SeqSkip)) SeqSkipLoc = SeqSkip
    ScrambledLoc = .false.
    if (present(Scrambled)) ScrambledLoc = Scrambled

    if (SeqSkipLoc == 0) call Error%Raise(Line='Skip value of 0 is not allowed', ProcName=ProcName)

    if ((SeqStart < SeqEnd .and. SeqSkipLoc>0) .or. (SeqStart > SeqEnd .and. SeqSkipLoc < 0)) then
      NbNodes = (SeqEnd - SeqStart + 1) / SeqSkipLoc
      if (mod(SeqEnd - SeqStart + 1,SeqSkipLoc) /= 0) NbNodes = NbNodes + 1
      allocate(LinSequence(NbNodes), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='LinSequence', ProcName=ProcName, stat=StatLoc)
      i = 1
      do i = 1, NbNodes-1
        LinSequence(i) = SeqStart + SeqSkipLoc*(i-1)
      end do
      LinSequence(NbNodes) = SeqEnd
      if (ScrambledLoc) call ScrambleArray(Array=LinSequence)
    else
      allocate(LinSequence(1), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='LinSequence', ProcName=ProcName, stat=StatLoc)
      LinSequence(1) = SeqStart
    end if
     
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function LinSpace(InterMin, InterMax, NbNodes)
  
    real(rkp), allocatable, dimension(:)                              ::    LinSpace

    real(rkp), intent(in)                                             ::    InterMin
    real(rkp), intent(in)                                             ::    InterMax
    integer, intent(in)                                               ::    NbNodes

    character(*), parameter                                           ::    ProcName='LinSpace'
    real(rkp)                                                         ::    h
    real(rkp)                                                         ::    d
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if (InterMax < InterMin) call Error%Raise(Line='Interval max is lower than min', ProcName=ProcName)
    if (NbNodes < 1) call Error%Raise(Line='Specified number of nodes below minimum of 1', ProcName=ProcName)

    allocate(LinSpace(NbNodes), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(name='LinSpace', stat=StatLoc)

    d = real(NbNodes - 1) 
    h = (InterMax - InterMin) / d

    LinSpace(1) = InterMin

    i = 2
    do i = 2, NbNodes
      LinSpace(i) = LinSpace(1) + (i-1)*h         
    end do
     
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Log10Space(InterMin, InterMax, NbNodes)
  
    real(rkp), allocatable, dimension(:)                              ::    Log10Space

    real(rkp), intent(in)                                             ::    InterMin
    real(rkp), intent(in)                                             ::    InterMax
    integer, intent(in)                                               ::    NbNodes

    character(*), parameter                                           ::    ProcName='Log10Space'
    real(rkp)                                                         ::    h
    real(rkp)                                                         ::    d
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if (InterMax < InterMin) call Error%Raise(Line='Interval max is lower than min', ProcName=ProcName)
    if (NbNodes < 1) call Error%Raise(Line='Specified number of nodes below minimum of 1', ProcName=ProcName)

    allocate(Log10Space(NbNodes), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(name='Log10Space', stat=StatLoc)

    Log10Space = Ten**Linspace(InterMin=InterMin, InterMax=InterMax, NbNodes=NbNodes)
     
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function LogSpace(InterMin, InterMax, NbNodes)
  
    real(rkp), allocatable, dimension(:)                              ::    LogSpace

    real(rkp), intent(in)                                             ::    InterMin
    real(rkp), intent(in)                                             ::    InterMax
    integer, intent(in)                                               ::    NbNodes

    character(*), parameter                                           ::    ProcName='LogSpace'
    real(rkp)                                                         ::    h
    real(rkp)                                                         ::    d
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if (InterMax < InterMin) call Error%Raise(Line='Interval max is lower than min', ProcName=ProcName)
    if (NbNodes < 1) call Error%Raise(Line='Specified number of nodes below minimum of 1', ProcName=ProcName)

    allocate(LogSpace(NbNodes), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(name='Log10Space', stat=StatLoc)

    LogSpace = dexp(Linspace(InterMin=InterMin, InterMax=InterMax, NbNodes=NbNodes))
     
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Interpolate_R1D_R1D(Abscissa, Ordinate, Nodes)
  
    real(rkp), allocatable, dimension(:)                              ::    Interpolate_R1D_R1D

    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    real(rkp), dimension(:), intent(in)                               ::    Ordinate
    real(rkp), dimension(:), intent(in)                               ::    Nodes

    character(*), parameter                                           ::    ProcName='Interpolate_R1D_R1D'
    integer                                                           ::    i, ii
    integer                                                           ::    NbNodes
    integer                                                           ::    Size1
    integer                                                           ::    StatLoc=0

    Size1 = size(Abscissa,1)

    if (Size1 /= size(Ordinate,1)) call Error%Raise(Line='Supplied abscissa and ordinate not the same length',      &
                                                                                                                ProcName=ProcName)

    NbNodes = size(Nodes,1)
    if (NbNodes < 1) call Error%Raise(Line='Supplied abscissa length below minimum of 1', ProcName=ProcName)

    allocate(Interpolate_R1D_R1D(NbNodes), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Interpolate_R1D_R1D', ProcName=ProcName, stat=StatLoc)

    if (Size1 == 1) then
      Interpolate_R1D_R1D = Ordinate(1)
    else
      ii = 1
      i = 1      
      do i = 1, NbNodes
        do 
          if (ii >= Size1) exit
          if (Abscissa(ii) >= Nodes(i)) exit
          ii=ii+1
        end do
        if (ii > 1) then
          Interpolate_R1D_R1D(i) = (Ordinate(ii)-Ordinate(ii-1))/(Abscissa(ii)-Abscissa(ii-1)) *                                  &
                                                                                        (Nodes(i)-Abscissa(ii-1)) + Ordinate(ii-1)
        else
          Interpolate_R1D_R1D(i) = (Ordinate(ii+1)-Ordinate(ii))/(Abscissa(ii+1)-Abscissa(ii)) *                                  &
                                                                                            (Nodes(i)-Abscissa(ii)) + Ordinate(ii)
        end if
      end do
    end if
  
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Interpolate_R2D_R1D(Abscissa, Ordinate, Nodes)
  
    real(rkp), allocatable, dimension(:,:)                            ::    Interpolate_R2D_R1D

    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    real(rkp), dimension(:,:), intent(in)                             ::    Ordinate
    real(rkp), dimension(:), intent(in)                               ::    Nodes

    character(*), parameter                                           ::    ProcName='Interpolate_R2D_R1D'
    integer                                                           ::    i, ii
    integer                                                           ::    NbNodes
    integer                                                           ::    Size2
    integer                                                           ::    StatLoc=0

    if (size(Abscissa,1) /= size(Ordinate,1)) call Error%Raise(Line='Supplied abscissa and ordinate not the same length',      &
                                                                                                                ProcName=ProcName)

    NbNodes = size(Nodes,1)
    if (NbNodes < 1) call Error%Raise(Line='Supplied abscissa length below minimum of 1', ProcName=ProcName)

    Size2 = size(Ordinate,2)

    allocate(Interpolate_R2D_R1D(NbNodes,Size2), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Interpolate_R2D', ProcName=ProcName, stat=StatLoc)

    if (size(Abscissa,1) == 1) then
      i = 1
      do i = 1, Size2
        Interpolate_R2D_R1D(i,:) = Ordinate(1,:)
      end do
    else
      i = 1      
      do i = 1, Size2
        Interpolate_R2D_R1D(:,i) = Interpolate(Abscissa, Ordinate(:,i), Nodes)
      end do
    end if
  
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Interpolate_R1D_R0D(Abscissa, Ordinate, Node)
  
    real(rkp)                                                         ::    Interpolate_R1D_R0D

    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    real(rkp), dimension(:), intent(in)                               ::    Ordinate
    real(rkp), intent(in)                                             ::    Node

    character(*), parameter                                           ::    ProcName='Interpolate_R1D_R0D'
    integer                                                           ::    i
    integer                                                           ::    Size1
    integer                                                           ::    StatLoc=0

    Size1 = size(Abscissa,1)

    if (Size1 /= size(Ordinate,1)) call Error%Raise(Line='Supplied abscissa and ordinate not the same length',      &
                                                                                                                ProcName=ProcName)

    if (Size1 == 1) then
      Interpolate_R1D_R0D = Ordinate(1)
    else
      i = 1
      do 
        if (i >= Size1) exit
        if (Abscissa(i) >= Node) exit
        i=i+1
      end do
      if (i > 1) then
        Interpolate_R1D_R0D = (Ordinate(i)-Ordinate(i-1))/(Abscissa(i)-Abscissa(i-1)) * (Node-Abscissa(i-1)) + Ordinate(i-1)
      else
        Interpolate_R1D_R0D = (Ordinate(i+1)-Ordinate(i))/(Abscissa(i+1)-Abscissa(i)) * (Node-Abscissa(i)) + Ordinate(i)
      end if
    end if
  
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Interpolate_R2D_R0D(Abscissa, Ordinate, Node)
  
    real(rkp), allocatable, dimension(:)                              ::    Interpolate_R2D_R0D

    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    real(rkp), dimension(:,:), intent(in)                             ::    Ordinate
    real(rkp), intent(in)                                             ::    Node

    character(*), parameter                                           ::    ProcName='Interpolate_R2D'
    integer                                                           ::    i
    integer                                                           ::    Size2
    integer                                                           ::    StatLoc=0

    if (size(Abscissa,1) /= size(Ordinate,1)) call Error%Raise(Line='Supplied abscissa and ordinate not the same length',      &
                                                                                                                ProcName=ProcName)

    Size2 = size(Ordinate,2)

    allocate(Interpolate_R2D_R0D(Size2), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Interpolate_R2D_R0D', ProcName=ProcName, stat=StatLoc)
    Interpolate_R2D_R0D = Zero

    if (size(Abscissa,1) == 1) then
      Interpolate_R2D_R0D = Ordinate(1,:)
    else
      i = 1      
      do i = 1, Size2
        Interpolate_R2D_R0D(i) = Interpolate(Abscissa, Ordinate(:,i), Node)
      end do
    end if
  
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Factorial_I(N)

    integer                                                           ::    Factorial_I
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='Factorial_I'
    integer                                                           ::    i

    if (N < 0) call Error%Raise(Line="Unable to compute the factorial given an integer less than 0")

    Factorial_I = 1

    do i = 1, N
      Factorial_I = Factorial_I * i
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Factorial_I8(N)

    integer(8)                                                        ::    Factorial_I8
    integer(8), intent(in)                                            ::    N

    character(*), parameter                                           ::    ProcName='Factorial_I8'
    integer(8)                                                        ::    i

    if (N < 0) call Error%Raise(Line="Unable to compute the factorial given an integer less than 0")

    Factorial_I8 = 1

    do i = 1, N
      Factorial_I8 = Factorial_I8 * i
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Factorial_R(N)

    real(rkp)                                                         ::    Factorial_R
    real(rkp), intent(in)                                             ::    N

    character(*), parameter                                           ::    ProcName='Factorial_R'

    if (N < Zero) call Error%Raise(Line="Unable to compute the factorial given an integer less than 0")

    Factorial_R = gamma(N + One)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function DoubleFactorial_I(N)

    integer                                                           ::    DoubleFactorial_I
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='DoubleFactorial_I'
    integer                                                           ::    Ni

    if (N < -1) call Error%Raise(Line="Unable to compute the double factorial given an integer less than -1")

    DoubleFactorial_I = 1

    if (N >= 1) then
      Ni = N
      do
        DoubleFactorial_I = DoubleFactorial_I * Ni
        Ni = Ni - 2
        if (Ni <= 0) exit
      end do
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function SQRTFactorial_I(N)

    real(rkp)                                                         ::    SQRTFactorial_I
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='SQRTFactorial_I'
    integer                                                           ::    i

    if (N < 0) call Error%Raise(Line="Unable to compute the factorial given an integer less than 0")

    SQRTFactorial_I = 1

    do i = 1, N
      SQRTFactorial_I = SQRTFactorial_I * sqrt(real(i,rkp))
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function SQRTFactorial_I8(N)

    real(rkp)                                                         ::    SQRTFactorial_I8
    integer(8), intent(in)                                            ::    N

    character(*), parameter                                           ::    ProcName='SQRTFactorial_I8'
    integer(8)                                                        ::    i

    if (N < 0) call Error%Raise(Line="Unable to compute the factorial given an integer less than 0")

    SQRTFactorial_I8 = 1

    do i = 1, N
      SQRTFactorial_I8 = SQRTFactorial_I8 * sqrt(real(i,rkp))
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function BinomialCoeff_I_I(Top, Bottom)

    integer                                                           ::    BinomialCoeff_I_I
    integer, intent(in)                                               ::    Top
    integer, intent(in)                                               ::    Bottom 

    character(*), parameter                                           ::    ProcName='BinomialCoeff_I_I'
    integer                                                           ::    i

    if (Bottom < 0) call Error%Raise(Line='Invalid Bottom option specification', ProcName=ProcName)

    BinomialCoeff_I_I = 1

    i = 1
    do i = 1, Bottom
      BinomialCoeff_I_I = BinomialCoeff_I_I * (Top + 1 - i) / i
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function BinomialCoeff_I8_I8(Top, Bottom)

    integer(8)                                                        ::    BinomialCoeff_I8_I8
    integer(8), intent(in)                                            ::    Top
    integer(8), intent(in)                                            ::    Bottom 

    character(*), parameter                                           ::    ProcName='BinomialCoeff_I8_I8'
    integer(8)                                                        ::    i

    if (Bottom < 0) call Error%Raise(Line='Invalid Bottom option specification', ProcName=ProcName)

    BinomialCoeff_I8_I8 = 1

    i = 1
    do i = 1, Bottom
      BinomialCoeff_I8_I8 = BinomialCoeff_I8_I8 * (Top + 1 - i) / i
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function BinomialCoeff_R_rkp_I_ikp(Top, Bottom)

    real(rkp)                                                         ::    BinomialCoeff_R_rkp_I_ikp
    real(rkp), intent(in)                                             ::    Top
    integer(ikp), intent(in)                                          ::    Bottom 

    character(*), parameter                                           ::    ProcName='BinomialCoeff_R_rkp_I_ikp'
    integer(ikp)                                                      ::    i
    real(rkp)                                                         ::    i_rkp

    if (Bottom < 0) call Error%Raise(Line='Invalid Bottom option specification', ProcName=ProcName)

    BinomialCoeff_R_rkp_I_ikp = One

    i = 1
    do i = 1, Bottom
      i_rkp = real(i,rkp)
      BinomialCoeff_R_rkp_I_ikp = BinomialCoeff_R_rkp_I_ikp * (Top + One - i_rkp) / i_rkp
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function BinomialCoeff_R_rkp_I(Top, Bottom)

    real(rkp)                                                         ::    BinomialCoeff_R_rkp_I
    real(rkp), intent(in)                                             ::    Top
    integer, intent(in)                                               ::    Bottom 

    character(*), parameter                                           ::    ProcName='BinomialCoeff_R_rkp_I'
    integer(ikp)                                                      ::    i
    real(rkp)                                                         ::    i_rkp

    if (Bottom < 0) call Error%Raise(Line='Invalid Bottom option specification', ProcName=ProcName)

    BinomialCoeff_R_rkp_I = One

    i = 1
    do i = 1, Bottom
      i_rkp = real(i,rkp)
      BinomialCoeff_R_rkp_I = BinomialCoeff_R_rkp_I * (Top + One - i_rkp) / i_rkp
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeEigenvalues_CMPLX(Matrix, EigenValues, EigenVectors)

    real(rkp), dimension(:,:), intent(inout)                          ::    Matrix
    complex, allocatable, dimension(:), intent(out)                   ::    EigenValues
    real(rkp), allocatable, dimension(:,:), optional, intent(out)     ::    EigenVectors

    character(*), parameter                                           ::    ProcName='ComputeEigenvalues_CMPLX'
    integer                                                           ::    N
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    WR
    real(rkp), allocatable, dimension(:)                              ::    WI
    real(rkp), allocatable, dimension(:,:)                            ::    VL
    real(rkp), allocatable, dimension(:,:)                            ::    VR
    real(rkp), allocatable, dimension(:)                              ::    WORK
    integer                                                           ::    LWORK=1

    N = size(Matrix,2)

    allocate(WR(N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WR', ProcName=ProcName, stat=StatLoc)

    allocate(WI(N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WI', ProcName=ProcName, stat=StatLoc)

    allocate(EigenValues(N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='EigenValues', ProcName=ProcName, stat=StatLoc)

    allocate(VR(1,1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VR', ProcName=ProcName, stat=StatLoc)

    if (present(EigenVectors)) then
      allocate(EigenVectors(N,N), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='EigenVectors', ProcName=ProcName, stat=StatLoc)
      allocate(WORK(1), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      EigenVectors = Zero
      call DGEEV('V', 'N', N, Matrix, N, WR, WI, EigenVectors, N, VR, 1, WORK, -1, StatLoc)
      if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DGEEV getting LWORK', ProcName=ProcName)
      LWORK = nint(WORK(1))
      deallocate(WORK, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      allocate(WORK(LWORK), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      call DGEEV('V', 'N', N, Matrix, N, WR, WI, EigenVectors, N, VR, 1, WORK, LWORK, StatLoc)
      if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DGEEV', ProcName=ProcName)
      deallocate(WORK, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      EigenValues = CMPLX(WR,WI)
    else
      allocate(WORK(1), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      EigenVectors = Zero
      call DGEEV('N', 'N', N, Matrix, N, WR, WI, VL, N, VR, 1, WORK, -1, StatLoc)
      if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DGEEV getting LWORK', ProcName=ProcName)
      LWORK = nint(WORK(1))
      deallocate(WORK, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      allocate(WORK(LWORK), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      call DGEEV('N', 'N', N, Matrix, N, WR, WI, VL, N, VR, 1, WORK, LWORK, StatLoc)
      if (StatLoc /= 0) call Error%Raise(Line='Something went wrong in DGEEV', ProcName=ProcName)
      deallocate(WORK, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      EigenValues = CMPLX(WR,WI)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeEigenvalues_REAL(Matrix, EigenValues, EigenVectors)

    real(rkp), dimension(:,:), intent(inout)                          ::    Matrix
    real(rkp), allocatable, dimension(:), intent(out)                 ::    EigenValues
    real(rkp), allocatable, dimension(:,:), optional, intent(out)     ::    EigenVectors

    character(*), parameter                                           ::    ProcName='ComputeEigenvalues_REAL'
    integer                                                           ::    N
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    WR
    real(rkp), allocatable, dimension(:)                              ::    WI
    real(rkp), allocatable, dimension(:,:)                            ::    VL
    real(rkp), allocatable, dimension(:,:)                            ::    VR
    real(rkp), allocatable, dimension(:)                              ::    WORK
    integer                                                           ::    LWORK=1
    integer                                                           ::    INFO=0
    integer                                                           ::    i

    N = size(Matrix,2)
    
    if (N /= size(Matrix,1)) call Error%Raise(Line='Matrix is not square', ProcName=ProcName)

    allocate(WR(N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WR', ProcName=ProcName, stat=StatLoc)

    allocate(WI(N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WI', ProcName=ProcName, stat=StatLoc)

    allocate(EigenValues(N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='EigenValues', ProcName=ProcName, stat=StatLoc)

    allocate(VL(1,1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VL', ProcName=ProcName, stat=StatLoc)

    allocate(VR(1,1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VR', ProcName=ProcName, stat=StatLoc)

    if (present(EigenVectors)) then
      allocate(EigenVectors(N,N), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='EigenVectors', ProcName=ProcName, stat=StatLoc)
      allocate(WORK(1), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      EigenVectors = Zero
      call DGEEV('V', 'N', N, Matrix, N, WR, WI, EigenVectors, N, VR, 1, WORK, -1, INFO)
      if (INFO /= 0) call Error%Raise(Line='Something went wrong in DGEEV getting LWORK', ProcName=ProcName)
      LWORK = nint(WORK(1))
      deallocate(WORK, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      allocate(WORK(LWORK), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      call DGEEV('V', 'N', N, Matrix, N, WR, WI, EigenVectors, N, VR, 1, WORK, LWORK, INFO)
      if (INFO /= 0) call Error%Raise(Line='Something went wrong in DGEEV', ProcName=ProcName)
      deallocate(WORK, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      i = 1
      do i = 1, N
        if (WI(i) > 1E-8) call Error%Raise(Line='Imaginary eigenvalues detected', ProcName=ProcName)
      end do
      EigenValues = WR
    else

      allocate(WORK(1), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      call DGEEV('N', 'N', N, Matrix, N, WR, WI, VL, 1, VR, 1, WORK, -1, INFO)
      if (INFO /= 0) call Error%Raise(Line='Something went wrong in DGEEV getting LWORK', ProcName=ProcName)
      LWORK = nint(WORK(1))
      deallocate(WORK, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      allocate(WORK(LWORK), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      call DGEEV('N', 'N', N, Matrix, N, WR, WI, VL, 1, VR, 1, WORK, LWORK, INFO)
      if (INFO /= 0) call Error%Raise(Line='Something went wrong in DGEEV', ProcName=ProcName)
      deallocate(WORK, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)
      i = 1
      do i = 1, N
        if (WI(i) > 1E-8) call Error%Raise(Line='Imaginary eigenvalues detected', ProcName=ProcName)
      end do
      EigenValues = WR
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeQR_Matrix(Matrix, Q, R, LowerR)

    real(rkp), dimension(:,:), intent(in)                             ::    Matrix
    real(rkp), dimension(:,:), intent(inout)                          ::    Q
    real(rkp), dimension(:,:), intent(inout)                          ::    R
    logical, optional, intent(in)                                     ::    LowerR

    character(*), parameter                                           ::    ProcName='ComputeQR_Matrix'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    M
    integer                                                           ::    N
    integer                                                           ::    i
    logical                                                           ::    LowerRLoc

    M = size(Matrix,1)
    N = size(Matrix,2)

    if (M < N) call Error%Raise(Line='This routine works only with tall matrices', ProcName=ProcName)

    LowerRLoc = .false.
    if (present(LowerR)) LowerRLoc = LowerR

    if (size(Q,1) /= M .or. size(Q,2) /= N) call Error%Raise('Incompatible Q', ProcName=ProcName)
    if (size(R,1) /= N .or. size(R,2) /= N) call Error%Raise('Incompatible R', ProcName=ProcName)

    Q = Matrix

    call ComputeQR(Q=Q, R=R, LowerR=LowerRLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeQR_Q(Q, R, LowerR)

    real(rkp), dimension(:,:), intent(inout)                          ::    Q
    real(rkp), dimension(:,:), intent(inout)                          ::    R
    logical, optional, intent(in)                                     ::    LowerR

    character(*), parameter                                           ::    ProcName='ComputeQR_Q'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    M
    integer                                                           ::    N
    real(rkp), allocatable, dimension(:)                              ::    TAU
    real(rkp), allocatable, dimension(:)                              ::    WORK
    real(rkp), dimension(1)                                           ::    WORKSIZE=0
    integer                                                           ::    LWORK
    integer                                                           ::    i
    logical                                                           ::    LowerRLoc

    M = size(Q,1)
    N = size(Q,2)

    if (M < N) call Error%Raise(Line='This routine works only with tall matrices', ProcName=ProcName)

    LowerRLoc = .false.
    if (present(LowerR)) LowerRLoc = LowerR

    if (size(Q,1) /= M .or. size(Q,2) /= N) call Error%Raise('Incompatible Q', ProcName=ProcName)
    if (size(R,1) /= N .or. size(R,2) /= N) call Error%Raise('Incompatible R', ProcName=ProcName)

    R = Zero

    allocate(TAU(min(M,N)), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='TAU', ProcName=ProcName, stat=StatLoc)

    call DGEQRF(M, N, Q, M, TAU, WORKSIZE, -1, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DGEQRF", ProcName=ProcName)

    LWORK = nint(WORKSIZE(1))

    allocate(WORK(LWORK), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    call DGEQRF(M, N, Q, M, TAU, WORK, LWORK, StatLoc)
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DGEQRF", ProcName=ProcName)

    deallocate(WORK, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    if (LowerRLoc) then
      i = 1
      do i = 1, N 
        R(N-i+1:N,N-i+1) = Q(1:i,i)
      end do
    else
      i = 1
      do i = 1, N
        R(1:i,i) = Q(1:i,i)
      end do
    end if

    call DORGQR(M, N, N, Q, M, TAU, WORKSIZE, -1, StatLoc) 
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DORMQR", ProcName=ProcName)

    LWORK = nint(WORKSIZE(1))

    allocate(WORK(LWORK), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='WORK', ProcName=ProcName, stat=StatLoc)

    call DORGQR(M, N, N, Q, M, TAU, WORK, LWORK, StatLoc) 
    if (StatLoc /= 0) call Error%Raise(Line="Something went wrong in DORMQR", ProcName=ProcName)

    deallocate(WORK, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='WORK', ProcName=ProcName, stat=StatLoc) 

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeQInvR_Matrix(Matrix, Q, InvR, LowerInvR)

    real(rkp), dimension(:,:), intent(in)                             ::    Matrix
    real(rkp), dimension(:,:), intent(inout)                          ::    Q
    real(rkp), dimension(:,:), intent(inout)                          ::    InvR
    logical, optional, intent(in)                                     ::    LowerInvR

    character(*), parameter                                           ::    ProcName='ComputeQInvR_Matrix'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    logical                                                           ::    LowerInvRLoc
    integer                                                           ::    N

    LowerInvRLoc = .false.
    if (present(LowerInvR)) LowerInvRLoc = LowerInvR

    N = size(Matrix,2)

    call ComputeQR(Matrix=Matrix, Q=Q, R=InvR, LowerR=LowerInvRLoc)

    if (LowerInvRLoc) then
      call DTRTRI('L', 'N', N, InvR, N, StatLoc)
      if (StatLoc /= 0) call Error%Raise('Something went wrong in DTRTRI : ' // ConvertToString(Value=StatLoc), &
                                          ProcName=ProcName)
    else
      call DTRTRI('U', 'N', N, InvR, N, StatLoc)
      if (StatLoc /= 0) call Error%Raise('Something went wrong in DTRTRI : ' // ConvertToString(Value=StatLoc), &
                                          ProcName=ProcName)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeQInvR_Q(Q, InvR, LowerInvR)

    real(rkp), dimension(:,:), intent(inout)                          ::    Q
    real(rkp), dimension(:,:), intent(out)                            ::    InvR
    logical, optional, intent(in)                                     ::    LowerInvR

    character(*), parameter                                           ::    ProcName='ComputeQInvR_Q'
    integer                                                           ::    StatLoc=0

    integer                                                           ::    i
    logical                                                           ::    LowerInvRLoc
    integer                                                           ::    N

    LowerInvRLoc = .false.
    if (present(LowerInvR)) LowerInvRLoc = LowerInvR

    N = size(Q,2)
    
    call ComputeQR(Q=Q, R=InvR, LowerR=LowerInvRLoc)

    if (LowerInvRLoc) then
      call DTRTRI('L', 'N', N, InvR, N, StatLoc)
      if (StatLoc /= 0) call Error%Raise('Something went wrong in DTRTRI : ' // ConvertToString(Value=StatLoc), &
                                          ProcName=ProcName)
    else
      call DTRTRI('U', 'N', N, InvR, N, StatLoc)
      if (StatLoc /= 0) call Error%Raise('Something went wrong in DTRTRI : ' // ConvertToString(Value=StatLoc), &
                                          ProcName=ProcName)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeNorm_R1D_8(Vector, Norm)

    use ieee_arithmetic

    real(8)                                                         ::    ComputeNorm_R1D_8    

    real(8), dimension(:), intent(in)                                 ::    Vector
    integer, intent(in)                                               ::    Norm
    
    character(*), parameter                                           ::    ProcName='ComputeQR'
    integer                                                           ::    StatLoc=0
    real(8), external                                                 ::    DNRM2

    if (Norm < 0) call Error%Raise(Line='Cannot take a negative norm', ProcName=ProcName)

    select case (Norm)
      case (0)
        ComputeNorm_R1D_8 = count(Vector == Zero,1,rkp)
      case (1)
        ComputeNorm_R1D_8 = sum(dabs(Vector))
      case (2)
        ComputeNorm_R1D_8 = DNRM2(size(Vector,1),Vector,1)
      case default
        ComputeNorm_R1D_8 = sum(Vector**Norm,1)**(One/real(Norm,rkp))
    end select

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Knuth shuffle
  ! https://rosettacode.org/wiki/Knuth_shuffle#Fortran
  subroutine ScrambleArray_I1D(Array, RNG)

    integer, dimension(:), intent(inout)                              ::    Array
    type(RandPseudo_Type), optional, intent(inout)                    ::    RNG

    character(*), parameter                                           ::    ProcName='ScrambleArray_I1D'
    integer                                                           ::    StatLoc
    integer                                                           ::    i
    integer                                                           ::    NbEntries
    real(rkp)                                                         ::    RandNum
    integer                                                           ::    VarI0D
    integer                                                           ::    RandIndex

    NbEntries = size(Array,1)

    do i = NbEntries, 2, -1
      if (present(RNG)) then
        RandNum = RNG%Draw(DrawType=2)
      else
        call random_number(RandNum)
      end if
      RandIndex = int(RandNum*i) + 1
      VarI0D = Array(RandIndex)
      Array(RandIndex) = Array(i)
      Array(i) = VarI0D
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Knuth shuffle
  ! https://rosettacode.org/wiki/Knuth_shuffle#Fortran
  subroutine ScrambleArray_R1D(Array, RNG)

    real(rkp), dimension(:), intent(inout)                            ::    Array
    type(RandPseudo_Type), optional, intent(inout)                    ::    RNG

    character(*), parameter                                           ::    ProcName='ScrambleArray_R1D'
    integer                                                           ::    StatLoc
    integer                                                           ::    i
    integer                                                           ::    NbEntries
    real(rkp)                                                         ::    RandNum
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    RandIndex

    NbEntries = size(Array,1)

    do i = NbEntries, 2, -1
      if (present(RNG)) then
        RandNum = RNG%Draw(DrawType=2)
      else
        call random_number(RandNum)
      end if
      RandIndex = int(RandNum*i) + 1
      VarR0D = Array(RandIndex)
      Array(RandIndex) = Array(i)
      Array(i) = VarR0D
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_VarR0DChar(Transformation, Value)

    character(*), intent(in)                                          ::    Transformation
    real(rkp), intent(inout)                                          ::    Value

    character(*), parameter                                           ::    ProcName='Transform_VarR0DChar'
    integer                                                           ::    StatLoc
    type(SMUQString_Type), allocatable, dimension(:)                  ::    TransformationsLoc
    integer                                                           ::    i

    allocate(TransformationsLoc, source=ConvertToStrings(Value=Transformation, Separator=' '), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='TransformationsLoc', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, size(TransformationsLoc,1)
      select case (TransformationsLoc(i)%Strip())
        case ('^2')
          Value = Value**2
        case('sqrt')
          if (Value < Zero) call Error%Raise(Line='Tried to take square root of a negative number', ProcName=ProcName)
          Value = dsqrt(Value)
        case('log')
          if (Value <= Zero) call Error%Raise(Line='Tried to take log of a number at or below zero', ProcName=ProcName)
          Value = dlog(Value)
        case('log10')
          if (Value <= Zero) call Error%Raise(Line='Tried to take log10 of a number at or below zero', ProcName=ProcName)
          Value = dlog10(Value)
        case('exp')
          Value = dexp(Value)
        case('10^')
          Value = Ten**Value
        case default
          call Error%Raise(Line='Did not recognize the transformation option', ProcName=ProcName)
      end select
    end do

    deallocate(TransformationsLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='TransformationsLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_VarR0DString(Transformation, Value)

    type(SMUQString_Type), intent(in)                                 ::    Transformation
    real(rkp), intent(inout)                                          ::    Value

    character(*), parameter                                           ::    ProcName='Transform_VarR0DString'
    integer                                                           ::    StatLoc
    type(SMUQString_Type), allocatable, dimension(:)                  ::    TransformationsLoc
    integer                                                           ::    i

    allocate(TransformationsLoc, source=Transformation%Split(Separator=' '), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='TransformationsLoc', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, size(TransformationsLoc,1)
      select case (TransformationsLoc(i)%Strip())
        case ('^2')
          Value = Value**2
        case('sqrt')
          if (Value < Zero) call Error%Raise(Line='Tried to take square root of a negative number', ProcName=ProcName)
          Value = dsqrt(Value)
        case('log')
          if (Value <= Zero) call Error%Raise(Line='Tried to take log of a number at or below zero', ProcName=ProcName)
          Value = dlog(Value)
        case('log10')
          if (Value <= Zero) call Error%Raise(Line='Tried to take log10 of a number at or below zero', ProcName=ProcName)
          Value = dlog10(Value)
        case('exp')
          Value = dexp(Value)
        case('10^')
          Value = Ten**Value
        case default
          call Error%Raise(Line='Did not recognize the transformation option', ProcName=ProcName)
      end select
    end do

    deallocate(TransformationsLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='TransformationsLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_VarR1DChar(Transformation, Values)

    character(*), intent(in)                                          ::    Transformation
    real(rkp), dimension(:), intent(inout)                            ::    Values

    character(*), parameter                                           ::    ProcName='Transform_VarR1DChar'
    integer                                                           ::    StatLoc
    type(SMUQString_Type), allocatable, dimension(:)                  ::    TransformationsLoc
    integer                                                           ::    i

    allocate(TransformationsLoc, source=ConvertToStrings(Value=Transformation, Separator=' '), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='TransformationsLoc', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, size(TransformationsLoc,1)
      select case (TransformationsLoc(i)%Strip())
        case ('^2')
          Values = Values**2
        case('sqrt')
          if (any(Values < Zero)) call Error%Raise(Line='Tried to take square root of a negative number', ProcName=ProcName)
          Values = dsqrt(Values)
        case('log')
          if (any(Values <= Zero)) call Error%Raise(Line='Tried to take log of a number at or below zero', ProcName=ProcName)
          Values = dlog(Values)
        case('log10')
          if (any(Values <= Zero)) call Error%Raise(Line='Tried to take log10 of a number at or below zero', ProcName=ProcName)
          Values = dlog10(Values)
        case('exp')
          Values = dexp(Values)
        case('10^')
          Values = Ten**Values
        case default
          call Error%Raise(Line='Did not recognize the transformation option', ProcName=ProcName)
      end select
    end do

    deallocate(TransformationsLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='TransformationsLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_VarR1DString(Transformation, Values)

    type(SMUQString_Type), intent(in)                                 ::    Transformation
    real(rkp), dimension(:), intent(inout)                            ::    Values

    character(*), parameter                                           ::    ProcName='Transform_VarR1DString'
    integer                                                           ::    StatLoc
    type(SMUQString_Type), allocatable, dimension(:)                  ::    TransformationsLoc
    integer                                                           ::    i

    allocate(TransformationsLoc, source=Transformation%Split(Separator=' '), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='TransformationsLoc', ProcName=ProcName, stat=StatLoc)
    i = 1
    do i = 1, size(TransformationsLoc,1)
      select case (TransformationsLoc(i)%Strip())
        case ('^2')
          Values = Values**2
        case('sqrt')
          if (any(Values < Zero)) call Error%Raise(Line='Tried to take square root of a negative number', ProcName=ProcName)
          Values = dsqrt(Values)
        case('log')
          if (any(Values <= Zero)) call Error%Raise(Line='Tried to take log of a number at or below zero', ProcName=ProcName)
          Values = dlog(Values)
        case('log10')
          if (any(Values <= Zero)) call Error%Raise(Line='Tried to take log10 of a number at or below zero', ProcName=ProcName)
          Values = dlog10(Values)
        case('exp')
          Values = dexp(Values)
        case('10^')
          Values = Ten**Values
        case default
          call Error%Raise(Line='Did not recognize the transformation option', ProcName=ProcName)
      end select
    end do

    deallocate(TransformationsLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='TransformationsLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function BernoulliNumbers(P)

    real(rkp), allocatable, dimension(:)                              ::    BernoulliNumbers
    
    integer, intent(in)                                               ::    P

    character(*), parameter                                           ::    ProcName='BernoulliNumbers'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii

    if (P < 1) call Error%Raise("Requested less than 1 Bernoulli number", ProcName=ProcName)

    allocate(BernoulliNumbers(P+1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='BernoulliNumbers', ProcName=ProcName, stat=StatLoc)

    BernoulliNumbers = Zero
    BernoulliNumbers(1) = One
    if (P > 1) BernoulliNumbers(2) = - One/Two

    i = 2
    do i = 2, P
      if (mod(i,2) /= 0) then
        BernoulliNumbers(i+1) = Zero
      else
        ii = 0
        do ii = 0, i-1
          BernoulliNumbers(i+1) = BernoulliNumbers(i+1) + real(BinomialCoeff(Top=i+1,Bottom=ii),rkp)*BernoulliNumbers(ii+1)
        end do   
        BernoulliNumbers(i+1) = -One / (real(i,rkp) + One) * BernoulliNumbers(i+1)
      end if
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Pochhammer_R0D_8(A, N)

    real(rkp)                                                         ::    Pochhammer_R0D_8
    
    real(8), intent(in)                                               ::    A
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='Pochhammer_R0D_8'
    integer                                                           ::    StatLoc=0

    Pochhammer_R0D_8 = gamma(A+real(N,8)) / gamma(A)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Pochhammer_R0D_4(A, N)

    real(rkp)                                                         ::    Pochhammer_R0D_4
    
    real, intent(in)                                                  ::    A
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='Pochhammer_R0D_4'
    integer                                                           ::    StatLoc=0

    Pochhammer_R0D_4 = gamma(A+real(N)) / gamma(A)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Pochhammer_I0D_8(A, N)

    real(rkp)                                                         ::    Pochhammer_I0D_8
    
    integer(8), intent(in)                                            ::    A
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='Pochhammer_I0D_8'
    integer                                                           ::    StatLoc=0

    Pochhammer_I0D_8 = real(Factorial(A+int(N-1,8)),rkp) / real(Factorial(A-int(1,8)),rkp)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Pochhammer_I0D_4(A, N)

    real(rkp)                                                         ::    Pochhammer_I0D_4
    
    integer, intent(in)                                               ::    A
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='Pochhammer_I0D_4'
    integer                                                           ::    StatLoc=0

    Pochhammer_I0D_4 = real(Factorial(A+N-1),rkp) / real(Factorial(A-1),rkp)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I0D_4_RNG(Min, Max, RNG)

    integer(4)                                                        ::    RandomInteger_I0D_4_RNG
    
    integer(4), intent(in)                                            ::    Min
    integer(4), intent(in)                                            ::    Max
    type(RandPseudo_Type), intent(inout)                              ::    RNG

    character(*), parameter                                           ::    ProcName='RandomInteger_I0D_4_RNG'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    VarR0D

    VarR0D = RNG%Draw(DrawType=2)

    RandomInteger_I0D_4_RNG = Min + floor(VarR0D * real(Max-Min+1,rkp))
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I0D_4_Intrinsic(Min, Max)

    integer(4)                                                        ::    RandomInteger_I0D_4_Intrinsic
    
    integer(4), intent(in)                                            ::    Min
    integer(4), intent(in)                                            ::    Max

    character(*), parameter                                           ::    ProcName='RandomInteger_I0D_4'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    VarR0D

    call random_number(VarR0D)

    RandomInteger_I0D_4_Intrinsic = Min + floor(VarR0D * real(Max-Min+1,rkp))
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I0D_8_RNG(Min, Max, RNG)

    integer(8)                                                        ::    RandomInteger_I0D_8_RNG
    
    integer(8), intent(in)                                            ::    Min
    integer(8), intent(in)                                            ::    Max
    type(RandPseudo_Type), intent(inout)                              ::    RNG

    character(*), parameter                                           ::    ProcName='RandomInteger_I0D_8_RNG'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    VarR0D

    VarR0D = RNG%Draw(DrawType=2)

    RandomInteger_I0D_8_RNG = Min + floor(VarR0D * real(Max-Min+1,rkp))
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I0D_8_Intrinsic(Min, Max)

    integer(8)                                                        ::    RandomInteger_I0D_8_Intrinsic
    
    integer(8), intent(in)                                            ::    Min
    integer(8), intent(in)                                            ::    Max

    character(*), parameter                                           ::    ProcName='RandomInteger_I0D_8_Intrinsic'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    VarR0D

    call random_number(VarR0D)

    RandomInteger_I0D_8_Intrinsic = Min + floor(VarR0D * real(Max-Min+1,rkp))
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I1D_4_RNG(Min, Max, M, RNG)

    integer(4), allocatable, dimension(:)                             ::    RandomInteger_I1D_4_RNG
    
    integer(4), intent(in)                                            ::    Min
    integer(4), intent(in)                                            ::    Max
    integer(4), intent(in)                                            ::    M
    type(RandPseudo_Type), intent(inout)                              ::    RNG

    character(*), parameter                                           ::    ProcName='RandomInteger_I1D_4_RNG'
    integer                                                           ::    StatLoc=0
    integer(4)                                                        ::    i

    allocate(RandomInteger_I1D_4_RNG(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='RandomInteger_I1D_4', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, M
      RandomInteger_I1D_4_RNG(i) = RandomInteger(Min=Min, Max=Max, RNG=RNG)
    end do
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I1D_4_Intrinsic(Min, Max, M)

    integer(4), allocatable, dimension(:)                             ::    RandomInteger_I1D_4_Intrinsic
    
    integer(4), intent(in)                                            ::    Min
    integer(4), intent(in)                                            ::    Max
    integer(4), intent(in)                                            ::    M

    character(*), parameter                                           ::    ProcName='RandomInteger_I1D_4_Intrinsic'
    integer                                                           ::    StatLoc=0
    integer(4)                                                        ::    i

    allocate(RandomInteger_I1D_4_Intrinsic(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='RandomInteger_I1D_4_Intrinsic', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, M
      RandomInteger_I1D_4_Intrinsic(i) = RandomInteger(Min=Min, Max=Max)
    end do
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I1D_8_RNG(Min, Max, M, RNG)

    integer(8), allocatable, dimension(:)                             ::    RandomInteger_I1D_8_RNG
    
    integer(8), intent(in)                                            ::    Min
    integer(8), intent(in)                                            ::    Max
    integer(8), intent(in)                                            ::    M
    type(RandPseudo_Type), intent(inout)                              ::    RNG

    character(*), parameter                                           ::    ProcName='RandomInteger_I1D_8_RNG'
    integer                                                           ::    StatLoc=0
    integer(8)                                                        ::    i

    allocate(RandomInteger_I1D_8_RNG(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='RandomInteger_I1D_8_RNG', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, M
      RandomInteger_I1D_8_RNG(i) = RandomInteger(Min=Min, Max=Max, RNG=RNG)
    end do
  
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I1D_8_Intrinsic(Min, Max, M)

    integer(8), allocatable, dimension(:)                             ::    RandomInteger_I1D_8_Intrinsic
    
    integer(8), intent(in)                                            ::    Min
    integer(8), intent(in)                                            ::    Max
    integer(8), intent(in)                                            ::    M

    character(*), parameter                                           ::    ProcName='RandomInteger_I1D_8_Intrinsic'
    integer                                                           ::    StatLoc=0
    integer(8)                                                        ::    i

    allocate(RandomInteger_I1D_8_Intrinsic(M), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='RandomInteger_I1D_8_Intrinsic', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, M
      RandomInteger_I1D_8_Intrinsic(i) = RandomInteger(Min=Min, Max=Max)
    end do
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I2D_4_RNG(Min, Max, M, N, RNG)

    integer(4), allocatable, dimension(:,:)                           ::    RandomInteger_I2D_4_RNG
    
    integer(4), intent(in)                                            ::    Min
    integer(4), intent(in)                                            ::    Max
    integer(4), intent(in)                                            ::    M
    integer(4), intent(in)                                            ::    N
    type(RandPseudo_Type), intent(inout)                              ::    RNG

    character(*), parameter                                           ::    ProcName='RandomInteger_I2D_4_RNG'
    integer                                                           ::    StatLoc=0
    integer(4)                                                        ::    i
    integer(4)                                                        ::    j

    allocate(RandomInteger_I2D_4_RNG(M,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='RandomInteger_I2D_4_RNG', ProcName=ProcName, stat=StatLoc)

    i = 1
    j = 1
    do i = 1, M
      do j = 1, N
        RandomInteger_I2D_4_RNG(i,j) = RandomInteger(Min=Min, Max=Max, RNG=RNG)
      end do
    end do
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I2D_4_Intrinsic(Min, Max, M, N)

    integer(4), allocatable, dimension(:,:)                           ::    RandomInteger_I2D_4_Intrinsic
    
    integer(4), intent(in)                                            ::    Min
    integer(4), intent(in)                                            ::    Max
    integer(4), intent(in)                                            ::    M
    integer(4), intent(in)                                            ::    N

    character(*), parameter                                           ::    ProcName='RandomInteger_I2D_4_Intrinsic'
    integer                                                           ::    StatLoc=0
    integer(4)                                                        ::    i
    integer(4)                                                        ::    j

    allocate(RandomInteger_I2D_4_Intrinsic(M,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='RandomInteger_I2D_4_Intrinsic', ProcName=ProcName, stat=StatLoc)

    do i = 1, M
      do j = 1, N
        RandomInteger_I2D_4_Intrinsic(i,j) = RandomInteger(Min=Min, Max=Max)
      end do
    end do
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I2D_8_RNG(Min, Max, M, N, RNG)

    integer(8), allocatable, dimension(:,:)                           ::    RandomInteger_I2D_8_RNG
    
    integer(8), intent(in)                                            ::    Min
    integer(8), intent(in)                                            ::    Max
    integer(8), intent(in)                                            ::    M
    integer(8), intent(in)                                            ::    N
    type(RandPseudo_Type), intent(inout)                              ::    RNG

    character(*), parameter                                           ::    ProcName='RandomInteger_I2D_8_RNG'
    integer                                                           ::    StatLoc=0
    integer(8)                                                        ::    i
    integer(8)                                                        ::    j

    allocate(RandomInteger_I2D_8_RNG(M,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='RandomInteger_I2D_8_RNG', ProcName=ProcName, stat=StatLoc)

    i = 1
    j = 1
    do i = 1, M
      do j = 1, N
        RandomInteger_I2D_8_RNG(i,j) = RandomInteger(Min=Min, Max=Max, RNG=RNG)
      end do
    end do
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function RandomInteger_I2D_8_Intrinsic(Min, Max, M, N)

    integer(8), allocatable, dimension(:,:)                           ::    RandomInteger_I2D_8_Intrinsic
    
    integer(8), intent(in)                                            ::    Min
    integer(8), intent(in)                                            ::    Max
    integer(8), intent(in)                                            ::    M
    integer(8), intent(in)                                            ::    N

    character(*), parameter                                           ::    ProcName='RandomInteger_I2D_8_Intrinsic'
    integer                                                           ::    StatLoc=0
    integer(8)                                                        ::    i
    integer(8)                                                        ::    j

    allocate(RandomInteger_I2D_8_Intrinsic(M,N), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='RandomInteger_I2D_8_Intrinsic', ProcName=ProcName, stat=StatLoc)

    do i = 1, M
      do j = 1, N
        RandomInteger_I2D_8_Intrinsic(i,j) = RandomInteger(Min=Min, Max=Max)
      end do
    end do
    
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
