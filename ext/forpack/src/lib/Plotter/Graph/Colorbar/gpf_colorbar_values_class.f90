Module GPF_ColorBar_Values_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private

  public  ::  GPF_ColorBar_Values_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_ColorBar_Values_Type
    private
    character(:)        ,allocatable                    ::  Orientation                                     !< ColorBox orientation (either 'vertical' or 'horizontal')
    character(:)        ,allocatable                    ::  Origin                                          !< ColorBox origin coordinates
    character(:)        ,allocatable                    ::  Size_                                           !< ColorBox size (height and width)
    character(:)        ,allocatable                    ::  User                                            !< ColorBox user option
    character(:)        ,allocatable                    ::  Position                                        !< ColorBox position (either 'front' or 'back')
    character(:)        ,allocatable                    ::  Border                                          !< ColorBox borders type
  contains
    private
    procedure   ,public   ::  Set_Command     =>  Set_ColorBox_Command                            !< Sets the ColorBox command
  End Type

  Interface             GPF_ColorBar_Values_Type
    Module Procedure    Construct_ColorBar_Values
  End Interface

  contains

! CB_Values = "0.00000000E+00   1.00000000E-04   2.15443469E-04   4.64158883E-04   1.00000000E-03   2.15443469E-03   4.64158883E-03   1.00000000E-02   2.15443469E-02   4.64158883E-02   1.00000000E-01   2.15443469E-01   4.64158883E-01   1.00000000E+00"


! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_ColorBar_Values( Values, Debug ) result(This)

  implicit none

  type(GPF_ColorBar_Values_Type)                                        ::  This                            !< ColorBox-Values object to be constructed
  real(rkp)     ,dimension(:)                 ,optional ,intent(in)     ::  Values                          !< ColorBar values
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  character(*)                                              ,parameter  ::  Keyword='colorbox'

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBar_Values]: Entering')")

! The followinf steps have to eb implmented:
! 1) Write the colorbar values in a scalar string and add 'CB_Values = "' and '"' at the begining and at the end. All numbers must be separated by at least one space character.
!       CB_Values = "0.00000000E+00   1.00000000E-04   2.15443469E-04   4.64158883E-04   1.00000000E-03   2.15443469E-03   4.64158883E-03   1.00000000E-02   2.15443469E-02   4.64158883E-02   1.00000000E-01   2.15443469E-01   4.64158883E-01   1.00000000E+00"
!       CB_Values = "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 ... N"
! 2) Write the function:
!       colors(n) = real(word(CB_Values,n))
!

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBar_Values]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBar_Values]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting ColorBox command

  if (This%i_Debug) write(DbgUnit,"(12x,'[Construct_ColorBar_Values]: Exiting',/)")

End Function


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_ColorBox_Command( This )
  implicit none
  class(GPF_ColorBar_Values_Type)                       ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the ColorBox object
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_ColorBox_Command]: Entering')")
  This%Command  =       ''                                                                                      ! Initialisation to empty string: default value
  if (This%Presence) then
    This%Command        =       'set '                  // &
                                This%Keyword            // &
                                This%Orientation        // &
                                This%User               // &
                                This%Origin             // &
                                This%Size_              // &
                                This%Position
  end if
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_ColorBox_Command]: This%Command = ',a)") This%Command
    write(DbgUnit,"(12x,'[Set_ColorBox_Command]: Exiting',/)")
  end if
End Subroutine

End Module