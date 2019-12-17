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
use String_Library
use Logger_Class                  ,only: Logger
use Error_Class                   ,only: Error

implicit none

private

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
public                                                                ::    ComputeNorm
public                                                                ::    ScrambleArray
public                                                                ::    Transform
public                                                                ::    BernoulliNumbers
public                                                                ::    Pochhammer

logical, parameter                                                    ::    DebugGlobal = .false.

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
  module procedure                                                    ::    ComputeQR
end interface

interface ComputeNorm
  module procedure                                                    ::    ComputeNorm_R1D_8
end interface

interface ScrambleArray
  module procedure                                                    ::    ScrambleArray_I1D
end interface

interface Transform
  module procedure                                                    ::    Transform_1_VarR0D
  module procedure                                                    ::    Transform_N_VarR0D
  module procedure                                                    ::    Transform_1_VarR1D
  module procedure                                                    ::    Transform_N_VarR1D
end interface

interface Pochhammer
  module procedure                                                    ::    Pochhammer_R0D_8
  module procedure                                                    ::    Pochhammer_R0D_4
  module procedure                                                    ::    Pochhammer_I0D_8
  module procedure                                                    ::    Pochhammer_I0D_4
end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function LinSequence( SeqStart, SeqEnd, SeqSkip, Scrambled)
  
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
    if ( present(SeqSkip) ) SeqSkipLoc = SeqSkip
    ScrambledLoc = .false.
    if ( present(Scrambled) ) ScrambledLoc = Scrambled

    if ( SeqSkipLoc == 0 ) call Error%Raise( Line='Skip value of 0 is not allowed', ProcName=ProcName )

    if ( (SeqStart < SeqEnd .and. SeqSkipLoc>0) .or. (SeqStart > SeqEnd .and. SeqSkipLoc < 0 ) ) then
      NbNodes = (SeqEnd - SeqStart + 1) / SeqSkipLoc
      if ( mod(SeqEnd - SeqStart + 1,SeqSkipLoc) /= 0 ) NbNodes = NbNodes + 1
      allocate(LinSequence(NbNodes), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LinSequence', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, NbNodes-1
        LinSequence(i) = SeqStart + SeqSkipLoc*(i-1)
      end do
      LinSequence(NbNodes) = SeqEnd
      if ( ScrambledLoc ) call ScrambleArray( Array=LinSequence )
    else
      allocate(LinSequence(1), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LinSequence', ProcName=ProcName, stat=StatLoc )
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

    if ( InterMax < InterMin ) call Error%Raise( Line='Interval max is lower than min', ProcName=ProcName )
    if ( NbNodes < 1 ) call Error%Raise( Line='Specified number of nodes below minimum of 1', ProcName=ProcName )

    allocate( LinSpace(NbNodes), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( name='LinSpace', stat=StatLoc)

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

    if ( InterMax < InterMin ) call Error%Raise( Line='Interval max is lower than min', ProcName=ProcName )
    if ( NbNodes < 1 ) call Error%Raise( Line='Specified number of nodes below minimum of 1', ProcName=ProcName )

    allocate( Log10Space(NbNodes), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( name='Log10Space', stat=StatLoc)

    Log10Space = Ten**Linspace( InterMin=InterMin, InterMax=InterMax, NbNodes=NbNodes )
     
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

    if ( InterMax < InterMin ) call Error%Raise( Line='Interval max is lower than min', ProcName=ProcName )
    if ( NbNodes < 1 ) call Error%Raise( Line='Specified number of nodes below minimum of 1', ProcName=ProcName )

    allocate( LogSpace(NbNodes), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( name='Log10Space', stat=StatLoc)

    LogSpace = dexp( Linspace( InterMin=InterMin, InterMax=InterMax, NbNodes=NbNodes) )
     
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Interpolate_R1D_R1D( Abscissa, Ordinate, Nodes )
  
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

    if ( Size1 /= size(Ordinate,1) ) call Error%Raise( Line='Supplied abscissa and ordinate not the same length',      &
                                                                                                                ProcName=ProcName)

    NbNodes = size(Nodes,1)
    if ( NbNodes < 1 ) call Error%Raise( Line='Supplied abscissa length below minimum of 1', ProcName=ProcName)

    allocate(Interpolate_R1D_R1D(NbNodes), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Interpolate_R1D_R1D', ProcName=ProcName, stat=StatLoc )

    if ( Size1 == 1 ) then
      Interpolate_R1D_R1D = Ordinate(1)
    else
      ii = 1
      i = 1      
      do i = 1, NbNodes
        do 
          if ( ii >= Size1 ) exit
          if ( Abscissa(ii) >= Nodes(i) ) exit
          ii=ii+1
        end do
        if ( ii > 1 ) then
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

    if ( size(Abscissa,1) /= size(Ordinate,1) ) call Error%Raise( Line='Supplied abscissa and ordinate not the same length',      &
                                                                                                                ProcName=ProcName)

    NbNodes = size(Nodes,1)
    if ( NbNodes < 1 ) call Error%Raise( Line='Supplied abscissa length below minimum of 1', ProcName=ProcName)

    Size2 = size(Ordinate,2)

    allocate(Interpolate_R2D_R1D(NbNodes,Size2), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Interpolate_R2D', ProcName=ProcName, stat=StatLoc )

    if ( size(Abscissa,1) == 1 ) then
      i = 1
      do i = 1, Size2
        Interpolate_R2D_R1D(i,:) = Ordinate(1,:)
      end do
    else
      i = 1      
      do i = 1, Size2
        Interpolate_R2D_R1D(:,i) = Interpolate( Abscissa, Ordinate(:,i), Nodes )
      end do
    end if
  
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Interpolate_R1D_R0D( Abscissa, Ordinate, Node )
  
    real(rkp)                                                         ::    Interpolate_R1D_R0D

    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    real(rkp), dimension(:), intent(in)                               ::    Ordinate
    real(rkp), intent(in)                                             ::    Node

    character(*), parameter                                           ::    ProcName='Interpolate_R1D_R0D'
    integer                                                           ::    i
    integer                                                           ::    Size1
    integer                                                           ::    StatLoc=0

    Size1 = size(Abscissa,1)

    if ( Size1 /= size(Ordinate,1) ) call Error%Raise( Line='Supplied abscissa and ordinate not the same length',      &
                                                                                                                ProcName=ProcName)

    if ( Size1 == 1 ) then
      Interpolate_R1D_R0D = Ordinate(1)
    else
      i = 1
      do 
        if ( i >= Size1 ) exit
        if ( Abscissa(i) >= Node ) exit
        i=i+1
      end do
      if ( i > 1 ) then
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

    if ( size(Abscissa,1) /= size(Ordinate,1) ) call Error%Raise( Line='Supplied abscissa and ordinate not the same length',      &
                                                                                                                ProcName=ProcName)

    Size2 = size(Ordinate,2)

    allocate(Interpolate_R2D_R0D(Size2), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Interpolate_R2D_R0D', ProcName=ProcName, stat=StatLoc )
    Interpolate_R2D_R0D = Zero

    if ( size(Abscissa,1) == 1 ) then
      Interpolate_R2D_R0D = Ordinate(1,:)
    else
      i = 1      
      do i = 1, Size2
        Interpolate_R2D_R0D(i) = Interpolate( Abscissa, Ordinate(:,i), Node )
      end do
    end if
  
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Factorial_I( N )

    integer                                                           ::    Factorial_I
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='Factorial_I'
    integer                                                           ::    i

    if ( N < 0 ) call Error%Raise( Line="Unable to compute the factorial given an integer less than 0" )

    Factorial_I = 1

    do i = 1, N
      Factorial_I = Factorial_I * i
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Factorial_I8( N )

    integer(8)                                                        ::    Factorial_I8
    integer(8), intent(in)                                            ::    N

    character(*), parameter                                           ::    ProcName='Factorial_I8'
    integer(8)                                                        ::    i

    if ( N < 0 ) call Error%Raise( Line="Unable to compute the factorial given an integer less than 0" )

    Factorial_I8 = 1

    do i = 1, N
      Factorial_I8 = Factorial_I8 * i
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Factorial_R( N )

    real(rkp)                                                         ::    Factorial_R
    real(rkp), intent(in)                                             ::    N

    character(*), parameter                                           ::    ProcName='Factorial_R'

    if ( N < Zero ) call Error%Raise( Line="Unable to compute the factorial given an integer less than 0" )

    Factorial_R = gamma(N + One)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function DoubleFactorial_I( N )

    integer                                                           ::    DoubleFactorial_I
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='DoubleFactorial_I'
    integer                                                           ::    Ni

    if ( N < -1 ) call Error%Raise( Line="Unable to compute the double factorial given an integer less than -1" )

    DoubleFactorial_I = 1

    if ( N >= 1 ) then
      Ni = N
      do
        DoubleFactorial_I = DoubleFactorial_I * Ni
        Ni = Ni - 2
        if ( Ni <= 0 ) exit
      end do
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function SQRTFactorial_I( N )

    real(rkp)                                                         ::    SQRTFactorial_I
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='SQRTFactorial_I'
    integer                                                           ::    i

    if ( N < 0 ) call Error%Raise( Line="Unable to compute the factorial given an integer less than 0" )

    SQRTFactorial_I = 1

    do i = 1, N
      SQRTFactorial_I = SQRTFactorial_I * sqrt(real(i,rkp))
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function SQRTFactorial_I8( N )

    real(rkp)                                                         ::    SQRTFactorial_I8
    integer(8), intent(in)                                            ::    N

    character(*), parameter                                           ::    ProcName='SQRTFactorial_I8'
    integer(8)                                                        ::    i

    if ( N < 0 ) call Error%Raise( Line="Unable to compute the factorial given an integer less than 0" )

    SQRTFactorial_I8 = 1

    do i = 1, N
      SQRTFactorial_I8 = SQRTFactorial_I8 * sqrt(real(i,rkp))
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function BinomialCoeff_I_I( Top, Bottom )

    integer                                                           ::    BinomialCoeff_I_I
    integer, intent(in)                                               ::    Top
    integer, intent(in)                                               ::    Bottom 

    character(*), parameter                                           ::    ProcName='BinomialCoeff_I_I'
    integer                                                           ::    i

    if ( Bottom < 0 ) call Error%Raise( Line='Invalid Bottom option specification', ProcName=ProcName )

    BinomialCoeff_I_I = 1

    i = 1
    do i = 1, Bottom
      BinomialCoeff_I_I = BinomialCoeff_I_I * (Top + 1 - i) / i
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function BinomialCoeff_I8_I8( Top, Bottom )

    integer(8)                                                        ::    BinomialCoeff_I8_I8
    integer(8), intent(in)                                            ::    Top
    integer(8), intent(in)                                            ::    Bottom 

    character(*), parameter                                           ::    ProcName='BinomialCoeff_I8_I8'
    integer(8)                                                        ::    i

    if ( Bottom < 0 ) call Error%Raise( Line='Invalid Bottom option specification', ProcName=ProcName )

    BinomialCoeff_I8_I8 = 1

    i = 1
    do i = 1, Bottom
      BinomialCoeff_I8_I8 = BinomialCoeff_I8_I8 * (Top + 1 - i) / i
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function BinomialCoeff_R_rkp_I_ikp( Top, Bottom )

    real(rkp)                                                         ::    BinomialCoeff_R_rkp_I_ikp
    real(rkp), intent(in)                                             ::    Top
    integer(ikp), intent(in)                                          ::    Bottom 

    character(*), parameter                                           ::    ProcName='BinomialCoeff_R_rkp_I_ikp'
    integer(ikp)                                                      ::    i
    real(rkp)                                                         ::    i_rkp

    if ( Bottom < 0 ) call Error%Raise( Line='Invalid Bottom option specification', ProcName=ProcName )

    BinomialCoeff_R_rkp_I_ikp = One

    i = 1
    do i = 1, Bottom
      i_rkp = real(i,rkp)
      BinomialCoeff_R_rkp_I_ikp = BinomialCoeff_R_rkp_I_ikp * (Top + One - i_rkp) / i_rkp
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function BinomialCoeff_R_rkp_I( Top, Bottom )

    real(rkp)                                                         ::    BinomialCoeff_R_rkp_I
    real(rkp), intent(in)                                             ::    Top
    integer, intent(in)                                               ::    Bottom 

    character(*), parameter                                           ::    ProcName='BinomialCoeff_R_rkp_I'
    integer(ikp)                                                      ::    i
    real(rkp)                                                         ::    i_rkp

    if ( Bottom < 0 ) call Error%Raise( Line='Invalid Bottom option specification', ProcName=ProcName )

    BinomialCoeff_R_rkp_I = One

    i = 1
    do i = 1, Bottom
      i_rkp = real(i,rkp)
      BinomialCoeff_R_rkp_I = BinomialCoeff_R_rkp_I * (Top + One - i_rkp) / i_rkp
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeEigenvalues_CMPLX( Matrix, EigenValues, EigenVectors )

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

    allocate( WR(N), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='WR', ProcName=ProcName, stat=StatLoc )

    allocate( WI(N), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='WI', ProcName=ProcName, stat=StatLoc )

    allocate(EigenValues(N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='EigenValues', ProcName=ProcName, stat=StatLoc )

    allocate(VR(1,1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VR', ProcName=ProcName, stat=StatLoc )

    if ( present(EigenVectors) ) then
      allocate(EigenVectors(N,N), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='EigenVectors', ProcName=ProcName, stat=StatLoc )
      allocate(WORK(1), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      EigenVectors = Zero
      call DGEEV( 'V', 'N', N, Matrix, N, WR, WI, EigenVectors, N, VR, 1, WORK, -1, StatLoc  )
      if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DGEEV getting LWORK', ProcName=ProcName )
      LWORK = nint(WORK(1))
      deallocate(WORK, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      allocate(WORK(LWORK), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      call DGEEV( 'V', 'N', N, Matrix, N, WR, WI, EigenVectors, N, VR, 1, WORK, LWORK, StatLoc  )
      if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DGEEV', ProcName=ProcName )
      deallocate(WORK, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      EigenValues = CMPLX( WR,WI )
    else
      allocate(WORK(1), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      EigenVectors = Zero
      call DGEEV( 'N', 'N', N, Matrix, N, WR, WI, VL, N, VR, 1, WORK, -1, StatLoc  )
      if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DGEEV getting LWORK', ProcName=ProcName )
      LWORK = nint(WORK(1))
      deallocate(WORK, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      allocate(WORK(LWORK), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      call DGEEV( 'N', 'N', N, Matrix, N, WR, WI, VL, N, VR, 1, WORK, LWORK, StatLoc  )
      if ( StatLoc /= 0 ) call Error%Raise( Line='Something went wrong in DGEEV', ProcName=ProcName )
      deallocate(WORK, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      EigenValues = CMPLX( WR,WI )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeEigenvalues_REAL( Matrix, EigenValues, EigenVectors )

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
    
    if ( N /= size(Matrix,1) ) call Error%Raise( Line='Matrix is not square', ProcName=ProcName )

    allocate( WR(N), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='WR', ProcName=ProcName, stat=StatLoc )

    allocate( WI(N), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='WI', ProcName=ProcName, stat=StatLoc )

    allocate(EigenValues(N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='EigenValues', ProcName=ProcName, stat=StatLoc )

    allocate(VL(1,1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VL', ProcName=ProcName, stat=StatLoc )

    allocate(VR(1,1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VR', ProcName=ProcName, stat=StatLoc )

    if ( present(EigenVectors) ) then
      allocate(EigenVectors(N,N), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='EigenVectors', ProcName=ProcName, stat=StatLoc )
      allocate(WORK(1), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      EigenVectors = Zero
      call DGEEV( 'V', 'N', N, Matrix, N, WR, WI, EigenVectors, N, VR, 1, WORK, -1, INFO  )
      if ( INFO /= 0 ) call Error%Raise( Line='Something went wrong in DGEEV getting LWORK', ProcName=ProcName )
      LWORK = nint(WORK(1))
      deallocate(WORK, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      allocate(WORK(LWORK), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      call DGEEV( 'V', 'N', N, Matrix, N, WR, WI, EigenVectors, N, VR, 1, WORK, LWORK, INFO  )
      if ( INFO /= 0 ) call Error%Raise( Line='Something went wrong in DGEEV', ProcName=ProcName )
      deallocate(WORK, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, N
        if ( WI(i) > 1E-8 ) call Error%Raise( Line='Imaginary eigenvalues detected', ProcName=ProcName )
      end do
      EigenValues = WR
    else

      allocate(WORK(1), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      call DGEEV( 'N', 'N', N, Matrix, N, WR, WI, VL, 1, VR, 1, WORK, -1, INFO  )
      if ( INFO /= 0 ) call Error%Raise( Line='Something went wrong in DGEEV getting LWORK', ProcName=ProcName )
      LWORK = nint(WORK(1))
      deallocate(WORK, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      allocate(WORK(LWORK), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      call DGEEV( 'N', 'N', N, Matrix, N, WR, WI, VL, 1, VR, 1, WORK, LWORK, INFO  )
      if ( INFO /= 0 ) call Error%Raise( Line='Something went wrong in DGEEV', ProcName=ProcName )
      deallocate(WORK, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )
      i = 1
      do i = 1, N
        if ( WI(i) > 1E-8 ) call Error%Raise( Line='Imaginary eigenvalues detected', ProcName=ProcName )
      end do
      EigenValues = WR
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeQR( Matrix, Q, R )

    real(rkp), dimension(:,:), intent(in)                             ::    Matrix
    real(rkp), allocatable, dimension(:,:), intent(out)               ::    Q
    real(rkp), allocatable, dimension(:,:), intent(out)               ::    R

    character(*), parameter                                           ::    ProcName='ComputeQR'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    M
    integer                                                           ::    N
    real(rkp), allocatable, dimension(:)                              ::    TAU
    real(rkp), allocatable, dimension(:)                              ::    WORK
    real(rkp), dimension(1)                                           ::    WORKSIZE=0
    integer                                                           ::    LWORK
    integer                                                           ::    i

    M = size(Matrix,1)
    N = size(Matrix,2)

    if ( M < N ) call Error%Raise( Line='This routine works only with tall matrices', ProcName=ProcName )

    allocate(Q(M,N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Q', ProcName=ProcName, stat=StatLoc )
    Q = Matrix

    allocate(R(N,N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='R', ProcName=ProcName, stat=StatLoc )
    R = Zero

    allocate( TAU(min(M,N)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='TAU', ProcName=ProcName, stat=StatLoc )

    call DGEQRF( M, N, Q, M, TAU, WORKSIZE, -1, StatLoc  )
    if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DGEQRF", ProcName=ProcName )

    LWORK = nint(WORKSIZE(1))

    allocate(WORK(LWORK), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

    call DGEQRF( M, N, Q, M, TAU, WORK, LWORK, StatLoc  )
    if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DGEQRF", ProcName=ProcName )

    deallocate(WORK, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, N
      R(1:i,i) = Q(1:i,i)
    end do

    call DORGQR( M, N, N, Q, M, TAU, WORKSIZE, -1, StatLoc) 
    if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DORMQR", ProcName=ProcName )

    LWORK = nint(WORKSIZE(1))

    allocate(WORK(LWORK), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='WORK', ProcName=ProcName, stat=StatLoc )

    call DORGQR( M, N, N, Q, M, TAU, WORK, LWORK, StatLoc) 
    if ( StatLoc /= 0 ) call Error%Raise( Line="Something went wrong in DORMQR", ProcName=ProcName )

    deallocate(WORK, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='WORK', ProcName=ProcName, stat=StatLoc ) 

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeNorm_R1D_8( Vector, Norm )

    use ieee_arithmetic

    real(rkp)                                                         ::    ComputeNorm_R1D_8    

    real(rkp), dimension(:), intent(in)                               ::    Vector
    integer, intent(in)                                               ::    Norm
    
    character(*), parameter                                           ::    ProcName='ComputeQR'
    integer                                                           ::    StatLoc=0
    real(8), external                                                 ::    DNRM2

    if ( Norm < 0 ) call Error%Raise( Line='Cannot take a negative norm', ProcName=ProcName )

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
  subroutine ScrambleArray_I1D( Array )

    integer, dimension(:), intent(inout)                              ::    Array

    character(*), parameter                                           ::    ProcName='ScrambleArray_I1D'
    integer                                                           ::    StatLoc
    integer                                                           ::    i
    integer                                                           ::    NbEntries
    real(rkp)                                                         ::    RandNum
    integer                                                           ::    VarI0D
    integer                                                           ::    RandIndex

    NbEntries = size(Array,1)

    do i = NbEntries, 2, -1
      call random_number(RandNum)
      RandIndex = int(RandNum*i) + 1
      VarI0D = Array(RandIndex)
      Array(RandIndex) = Array(i)
      Array(i) = VarI0D
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_1_VarR0D( Transformation, Value )

    character(*), intent(in)                                          ::    Transformation
    real(rkp), intent(inout)                                          ::    Value

    character(*), parameter                                           ::    ProcName='Transform_1_VarR0D'
    integer                                                           ::    StatLoc

    select case (trim(adjustl(Transformation)))
      case ('^2')
        Value = Value**2
      case('sqrt')
        if ( Value < Zero ) call Error%Raise( Line='Tried to take square root of a negative number', ProcName=ProcName )
        Value = dsqrt(Value)
      case('log')
        if ( Value <= Zero ) call Error%Raise( Line='Tried to take log of a number at or below zero', ProcName=ProcName )
        Value = dlog(Value)
      case('log10')
        if ( Value <= Zero ) call Error%Raise( Line='Tried to take log10 of a number at or below zero', ProcName=ProcName )
        Value = dlog10(Value)
      case('exp')
        Value = dexp(Value)
      case('10^')
        Value = Ten**Value
      case default
        call Error%Raise( Line='Did not recognize the transformation option', ProcName=ProcName )
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_N_VarR0D( Transformations, Value )

    character(*), dimension(:), intent(in)                            ::    Transformations
    real(rkp), intent(inout)                                          ::    Value

    character(*), parameter                                           ::    ProcName='Transform_N_VarR0D'
    integer                                                           ::    StatLoc
    integer                                                           ::    NbTransformations
    integer                                                           ::    i

    NbTransformations = size(Transformations,1)

    i = NbTransformations
    do i = NbTransformations, 1, -1
      select case (trim(adjustl(Transformations(i))))
        case ('^2')
          Value = Value**2
        case('sqrt')
          if ( Value < Zero ) call Error%Raise( Line='Tried to take square root of a negative number', ProcName=ProcName )
          Value = dsqrt(Value)
        case('log')
          if ( Value <= Zero ) call Error%Raise( Line='Tried to take log of a number at or below zero', ProcName=ProcName )
          Value = dlog(Value)
        case('log10')
          if ( Value <= Zero ) call Error%Raise( Line='Tried to take log10 of a number at or below zero', ProcName=ProcName )
          Value = dlog10(Value)
        case('exp')
          Value = dexp(Value)
        case('10^')
          Value = Ten**Value
        case default
          call Error%Raise( Line='Did not recognize the transformation option', ProcName=ProcName )
      end select
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_1_VarR1D( Transformation, Values )

    character(*), intent(in)                                          ::    Transformation
    real(rkp), dimension(:), intent(inout)                            ::    Values

    character(*), parameter                                           ::    ProcName='Transform_1_VarR0D'
    integer                                                           ::    StatLoc

    select case (trim(adjustl(Transformation)))
      case ('^2')
        Values = Values**2
      case('sqrt')
        if ( any(Values < Zero) ) call Error%Raise( Line='Tried to take square root of a negative number', ProcName=ProcName )
        Values = dsqrt(Values)
      case('log')
        if ( any(Values <= Zero) ) call Error%Raise( Line='Tried to take log of a number at or below zero', ProcName=ProcName )
        Values = dlog(Values)
      case('log10')
        if ( any(Values <= Zero) ) call Error%Raise( Line='Tried to take log10 of a number at or below zero', ProcName=ProcName )
        Values = dlog10(Values)
      case('exp')
        Values = dexp(Values)
      case('10^')
        Values = Ten**Values
      case default
        call Error%Raise( Line='Did not recognize the transformation option', ProcName=ProcName )
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_N_VarR1D( Transformations, Values )

    character(*), dimension(:), intent(in)                            ::    Transformations
    real(rkp), dimension(:), intent(inout)                            ::    Values

    character(*), parameter                                           ::    ProcName='Transform_N_VarR0D'
    integer                                                           ::    StatLoc
    integer                                                           ::    NbTransformations
    integer                                                           ::    i

    NbTransformations = size(Transformations,1)

    i = NbTransformations
    do i = NbTransformations, 1, -1
      select case (trim(adjustl(Transformations(i))))
        case ('^2')
          Values = Values**2
        case('sqrt')
          if ( any(Values < Zero) ) call Error%Raise( Line='Tried to take square root of a negative number', ProcName=ProcName )
          Values = dsqrt(Values)
        case('log')
          if ( any(Values <= Zero) ) call Error%Raise( Line='Tried to take log of a number at or below zero', ProcName=ProcName )
          Values = dlog(Values)
        case('log10')
          if ( any(Values <= Zero) ) call Error%Raise( Line='Tried to take log10 of a number at or below zero', ProcName=ProcName )
          Values = dlog10(Values)
        case('exp')
          Values = dexp(Values)
        case('10^')
          Values = Ten**Values
        case default
          call Error%Raise( Line='Did not recognize the transformation option', ProcName=ProcName )
      end select
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function BernoulliNumbers( P )

    real(rkp), allocatable, dimension(:)                              ::    BernoulliNumbers
    
    integer, intent(in)                                               ::    P

    character(*), parameter                                           ::    ProcName='BernoulliNumbers'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii

    if ( P < 1 ) call Error%Raise( "Requested less than 1 Bernoulli number", ProcName=ProcName )

    allocate(BernoulliNumbers(P+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='BernoulliNumbers', ProcName=ProcName, stat=StatLoc )

    BernoulliNumbers = Zero
    BernoulliNumbers(1) = One
    if ( P > 1 ) BernoulliNumbers(2) = - One/Two

    i = 2
    do i = 2, P
      if ( mod(i,2) /= 0 ) then
        BernoulliNumbers(i+1) = Zero
      else
        ii = 0
        do ii = 0, i-1
          BernoulliNumbers(i+1) = BernoulliNumbers(i+1) + real(BinomialCoeff(Top=i+1,Bottom=ii),rkp)*BernoulliNumbers(ii+1)
        end do   
        BernoulliNumbers(i+1) = -One / ( real(i,rkp) + One ) * BernoulliNumbers(i+1)
      end if
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Pochhammer_R0D_8( A, N )

    real(rkp)                                                         ::    Pochhammer_R0D_8
    
    real(8), intent(in)                                               ::    A
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='Pochhammer_R0D_8'
    integer                                                           ::    StatLoc=0

    Pochhammer_R0D_8 = gamma(A+real(N,8)) / gamma(A)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Pochhammer_R0D_4( A, N )

    real(rkp)                                                         ::    Pochhammer_R0D_4
    
    real, intent(in)                                                  ::    A
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='Pochhammer_R0D_4'
    integer                                                           ::    StatLoc=0

    Pochhammer_R0D_4 = gamma(A+real(N)) / gamma(A)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Pochhammer_I0D_8( A, N )

    real(rkp)                                                         ::    Pochhammer_I0D_8
    
    integer(8), intent(in)                                            ::    A
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='Pochhammer_I0D_8'
    integer                                                           ::    StatLoc=0

    Pochhammer_I0D_8 = real(Factorial(A+integer(N-1,8)),rkp) / real(Factorial(A-integer(1,8)),rkp)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Pochhammer_I0D_4( A, N )

    real(rkp)                                                         ::    Pochhammer_I0D_4
    
    integer, intent(in)                                               ::    A
    integer, intent(in)                                               ::    N

    character(*), parameter                                           ::    ProcName='Pochhammer_I0D_4'
    integer                                                           ::    StatLoc=0

    Pochhammer_I0D_4 = real(Factorial(A+N-1),rkp) / real(Factorial(A-1),rkp)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
