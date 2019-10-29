program test

use Parameters_Library
use String_Library
use ComputingRoutines_Module
use TruncHyperbolic_Class         , only: TruncHyperbolic_Type
use TruncLowOrder_Class           , only: TruncLowOrder_Type
use TruncStandard_Class           , only: TruncStandard_Type
use Logger_Class                  , only: Logger
use Error_Class                   , only: Error

implicit none

type(TruncHyperbolic_Type)                                            ::    TruncHyperbolic
type(TruncLowOrder_Type)                                              ::    TruncLowOrder
type(TruncStandard_Type)                                              ::    TruncStandard
integer, dimension(:,:), allocatable                                  ::    Indices
character(:), allocatable                                             ::    FileName
character(:), allocatable                                             ::    Prefix
character(1000)                                                       ::    Argument
character(:), allocatable                                             ::    ArgumentLC, ArgumentShort
integer                                                               ::    i, ArgumentLength
logical                                                               ::    i_Debug_Loc = .false.              

i = 1
do i = 1, iargc()
  call get_command_argument(Number=i, Value=Argument, Length=ArgumentLength)
  if ( ArgumentLength > 7 ) then
    ArgumentShort = trim(adjustl(Argument))
    ArgumentLC = LowerCase(ArgumentShort)
    if ( ArgumentShort(1:7) == 'rundir=' ) then
      Prefix = ArgumentShort(8:ArgumentLength)
      exit
    end if
  end if
  if ( i == iargc() ) call Error%Raise( "Run directory command line argument not detected" )
end do

FileName = Prefix // '/runlog.log'

call Logger%Initialize( FileName=FileName,  &
            Status          =       'REPLACE',                                                   &
            Position        =       'REWIND',                                                    &
            Procedure       =       'test',                                                      &
            Indentation     =       2                                                            )

do i = 1, 10
call TruncLowOrder%SetM( 15 )
call TruncLowOrder%SetOrder( 8 )
call TruncLowOrder%SetNorm0( 6 )
call TruncLowOrder%GetIndices( Indices )
write(*,*) '-----------------------------------------------------------------------------------------------------------------'
write(*,*) size(Indices,2)
deallocate(Indices)
end do

do i = 1, 10
call TruncHyperbolic%SetM( 15 )
call TruncHyperbolic%SetOrder( 8 )
call TruncHyperbolic%SetNormQ( 0.4_rkp )
call TruncHyperbolic%GetIndices( Indices )
write(*,*) '-----------------------------------------------------------------------------------------------------------------'
write(*,*) size(Indices,2)
deallocate(Indices)
end do

do i = 1, 10
call TruncStandard%SetM( 15 )
call TruncStandard%SetOrder( 8 )
call TruncStandard%GetIndices( Indices )
write(*,*) '-----------------------------------------------------------------------------------------------------------------'
write(*,*) size(Indices,2)
deallocate(Indices)
end do

end program
