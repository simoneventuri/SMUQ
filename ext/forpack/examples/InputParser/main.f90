Program InputParserExample

  use Input_Library
  use Logger_Class      ,only:  Logger

  implicit none

  logical ,parameter  :: i_Debug_Loc = .True.


  if (i_Debug_Loc) call Logger%Write( "Calling Example_GetValue_REAL64_1d" )
  call Example_GetValue_REAL64_1d()

  contains

Subroutine Example_GetValue_REAL64_1d

  character(*)  ,parameter              ::    ProcName='Example_GetValue_REAL64_1d'! Name of current procedure
  type(InputReader_Type)                ::    Input
  character(:)  ,allocatable            ::    FileName
  character(:)  ,allocatable            ::    Section_Name
  character(:)  ,allocatable            ::    ParameterName
!   real(8)                               ::    VarR0
  real(8)   ,dimension(:) ,allocatable  ::    VarR1

  if (i_Debug_Loc) call Logger%Entering( ProcName )

  FileName      =   'input.inp'
  if (i_Debug_Loc) call Logger%Write( "Calling Input%Read: FileName = ", FileName )
  call Input%Read( FileName=FileName, i_Debug=.False. )                                                 ! Loading current kinetic file in one element of the array of Input-Reader objects


  ParameterName =   'Vector-1'
  if (i_Debug_Loc) call Logger%Write( "Calling Input%GetValue: ParameterName = ", ParameterName )
  call Input%GetValue( VarR1, ParameterName )
  if (i_Debug_Loc) call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  ParameterName =   'Vector-2'
  if (i_Debug_Loc) call Logger%Write( "Calling Input%GetValue: ParameterName = ", ParameterName )
  call Input%GetValue( VarR1, ParameterName )
  if (i_Debug_Loc) call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  ParameterName =   'Vector-3'
  if (i_Debug_Loc) call Logger%Write( "Calling Input%GetValue: ParameterName = ", ParameterName )
  call Input%GetValue( VarR1, ParameterName, Separator="," )
  if (i_Debug_Loc) call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  ParameterName =   'Vector-4'
  if (i_Debug_Loc) call Logger%Write( "Calling Input%GetValue: ParameterName = ", ParameterName )
  call Input%GetValue( VarR1, ParameterName )
  if (i_Debug_Loc) call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  ParameterName =   'Vector-5'
  if (i_Debug_Loc) call Logger%Write( "Calling Input%GetValue: ParameterName = ", ParameterName )
  call Input%GetValue( VarR1, ParameterName )
  if (i_Debug_Loc) call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  ParameterName =   'Vector-6'
  if (i_Debug_Loc) call Logger%Write( "Calling Input%GetValue: ParameterName = ", ParameterName )
  call Input%GetValue( VarR1, ParameterName )
  if (i_Debug_Loc) call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  ParameterName =   'Vector-7'
  if (i_Debug_Loc) call Logger%Write( "Calling Input%GetValue: ParameterName = ", ParameterName )
  call Input%GetValue( VarR1, ParameterName )
  if (i_Debug_Loc) call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  ParameterName =   'Vector-8'
  if (i_Debug_Loc) call Logger%Write( "Calling Input%GetValue: ParameterName = ", ParameterName )
  call Input%GetValue( VarR1, ParameterName )
  if (i_Debug_Loc) call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  ParameterName =   'Vector-9'
  if (i_Debug_Loc) call Logger%Write( "Calling Input%GetValue: ParameterName = ", ParameterName )
  call Input%GetValue( VarR1, ParameterName )
  if (i_Debug_Loc) call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  ParameterName =   'Vector-10'
  if (i_Debug_Loc) call Logger%Write( "Calling Input%GetValue: ParameterName = ", ParameterName )
  call Input%GetValue( VarR1, ParameterName, IncreasingOrder=.True. )
  if (i_Debug_Loc) call Logger%Write( "-> VarR1 = ", VarR1, Fr="f4.1" )

  if (i_Debug_Loc) call Logger%Exiting()

End Subroutine

!
!   call Logger%Write( "-------------------------------------" )
!   if ( allocated(Vec) ) deallocate(Vec)
!   allocate( Vec(10) )
!   Vec   =   [   1.0,  6.0  ,&
!                 2.0,  7.0  ,&
!                 3.0,  8.0  ,&
!                 4.0,  9.0  ,&
!                 5.0, 10.0 ]
! !   call Logger%Write( "Vec                 = ", Vec, Fr="es15.8" )
! !   call Logger%Write( "reshape(Vec,[5,2])  = ", reshape(Vec,[5,2]), Fr="es15.8" )
! !   call Logger%Write( "reshape(Vec,[2,5])  = ", reshape(Vec,[2,5]), Fr="es15.8" )
! !   call Logger%Write( "transpose(reshape(Vec,[2,5]))  = ", transpose(reshape(Vec,[2,5])), Fr="es15.8" )
!
!   ArrayShape  = [5,2]
!   iSpecified  = 2 ! 2nd dimension fixed
!   FixedDim    = ArrayShape(iSpecified)
!   if ( allocated(Values) ) deallocate(Values)
!   allocate( Values(ArrayShape(1),ArrayShape(2)) )
!   call Logger%Write( "size(Values,1) = ", size(Values,1), "size(Values,2) = ", size(Values,2) )
!   call Logger%Write( "FixedDim = ", FixedDim )
!   Values    =   0.0_8
!   i = 1
!   j = 1
!   do k = 1,size(Vec)
!     n   =   mod(k,FixedDim)
!     call Logger%Write( "-> k = ", k, "n = ", n, "i = ", i, "j = ", j, Fi="i3" )
!     Values(i,j)   =   Vec(k)
!     if ( n /= 0 ) then
!       j = j + 1
!     else
!       i = i + 1
!       j = 1
!     end if
!   end do
!   call Logger%Write( "Values  = ", Values, Fr="es15.8" )
!
!
! !   error stop
!
!   call Logger%Write( "-------------------------------------" )
!   if ( allocated(Vec) ) deallocate(Vec)
!   allocate( Vec(12) )
!   Vec   =   [ 1.0,  5.0,   9.0, &
!               2.0,  6.0,  10.0, &
!               3.0,  7.0,  11.0, &
!               4.0,  8.0,  12.0  ]
! !   call Logger%Write( "Vec                 = ", Vec, Fr="es15.8" )
! !   call Logger%Write( "reshape(Vec,[4,3])  = ", reshape(Vec,[4,3]), Fr="es15.8" )
! !   call Logger%Write( "reshape(Vec,[3,4])  = ", reshape(Vec,[3,4]), Fr="es15.8" )
! !   call Logger%Write( "transpose(reshape(Vec,[3,4]))  = ", transpose(reshape(Vec,[3,4])), Fr="es15.8" )
!   ArrayShape  = [4,3]
!   iSpecified  = 2 ! 2nd dimension fixed
!   FixedDim    = ArrayShape(iSpecified)
!   if ( allocated(Values) ) deallocate(Values)
!   allocate( Values(ArrayShape(1),ArrayShape(2)) )
!   call Logger%Write( "size(Values,1) = ", size(Values,1), "size(Values,2) = ", size(Values,2) )
!   call Logger%Write( "FixedDim = ", FixedDim )
!   Values    =   0.0_8
!   i = 1
!   j = 1
!   do k = 1,size(Vec)
!     n   =   mod(k,FixedDim)
!     call Logger%Write( "-> k = ", k, "n = ", n, "i = ", i, "j = ", j, Fi="i3" )
!     Values(i,j)   =   Vec(k)
!     if ( n /= 0 ) then
!       j = j + 1
!     else
!       i = i + 1
!       j = 1
!     end if
!   end do
!   call Logger%Write( "Values  = ", Values, Fr="es15.8" )
!
!
!   call Logger%Write( "-------------------------------------" )
!   if ( allocated(Vec) ) deallocate(Vec)
!   allocate( Vec(10) )
!   Vec   =   [ 1.0,  5.0,   9.0, &
!               2.0,  6.0,  10.0, &
!               3.0,  7.0,  11.0, &
!               4.0               ]
! !   call Logger%Write( "Vec                 = ", Vec, Fr="es15.8" )
! !   call Logger%Write( "reshape(Vec,[4,3])  = ", reshape(Vec,[4,3]), Fr="es15.8" )
! !   call Logger%Write( "reshape(Vec,[3,4])  = ", reshape(Vec,[3,4]), Fr="es15.8" )
! !   call Logger%Write( "transpose(reshape(Vec,[3,4]))  = ", transpose(reshape(Vec,[3,4])), Fr="es15.8" )
!   ArrayShape  = [4,3]
!   iSpecified  = 2 ! 2nd dimension fixed
!   FixedDim    = ArrayShape(iSpecified)
!   if ( allocated(Values) ) deallocate(Values)
!   allocate( Values(ArrayShape(1),ArrayShape(2)) )
!   call Logger%Write( "size(Values,1) = ", size(Values,1), "size(Values,2) = ", size(Values,2) )
!   call Logger%Write( "FixedDim = ", FixedDim )
!   Values    =   0.0_8
!   i = 1
!   j = 1
! !   FillMatrix: do k = 1,product(ArrayShape)
!   do k = 1,size(Vec)
!     n   =   mod(k,FixedDim)
!     call Logger%Write( "-> k = ", k, "n = ", n, "i = ", i, "j = ", j, Fi="i3" )
!     Doit = .True.
!     if ( i > ArrayShape(1) .or. j > ArrayShape(2) ) Doit = .False.
!     if (.Not.Doit) exit
!     Values(i,j)   =   Vec(k)
!     if ( n /= 0 ) then
!       j = j + 1
!     else
!       i = i + 1
!       j = 1
!     end if
!   end do
!   call Logger%Write( "Values  = ", Values, Fr="es15.8" )
!



! Subroutine Example_Old
!
!   use Input_Library
!   use Logger_Class      ,only:  Logger
!
!   implicit none                                                         ::    Status
!
!   type(InputReader_Type)           ::    Input, Kinetic, Kin_Loc
!   type(InputReader_Type)           ,allocatable::    Kinetics(:)
!   type(InputSection_Type)          ::    Section, Reaction_Loc
!   type(InputParameter_Type)        ::    Param
!   type(InputParamProperties_Type)  ::    Properties
!
!   character(:)  ,allocatable                                            ::      String                          ! Arbitrary scalar charatcer
!   logical :: i_Debug_Loc
!   i_Debug_Loc=.True.
!
!   allocate( Kinetics(1) )                                                                 ! Allocating the array of Input-Reader objects associated to each kinetic file to be processed
!     call Kinetics(1)%Read( FileName='Air11-Park2001.kin', i_Debug=.False. )                                                 ! Loading current kinetic file in one element of the array of Input-Reader objects
!     call Kinetics(1)%Get_Value( String, "Name", Mandatory=.True. )                                              ! Getting the name of current kinetic scheme ...
!     Kinetics(1)%Name    =       String                                                                          ! ... and setting it
!
!
!   Kinetic%Name        =       Kinetics(1)%Name
!
!   Kin_Loc     =       Kinetics(1)
!
!   if (i_Debug_Loc) call Logger%Write( "-> Getting the reaction section from local kinetic '"//Kin_Loc%Name//"'" ) ! Debugging
!     Reaction_Loc        =       Kin_Loc%GetSection( Section_Name="Reactions" )                                  ! Extracting the "Reaction" section from the local kinetic
!     if ( Reaction_Loc%Defined ) then                                                                            ! If the section has been found, then merged it the the global kinetic
!       if (i_Debug_Loc) call Logger%Write( "-> Section has been found => Number of reactions: ", Reaction_Loc%NParameters ) ! Debugging
!       if (i_Debug_Loc) call Logger%Write( "-> Merging the reaction section from the local kinetic inside the global kinetic => Kinetic%Add_Section" ) ! Debugging
!       call Kinetic%Add_Section( Reaction_Loc, Action="MERGE", i_Debug=.True. )                                                  ! Merging the reaction in the ones of the merged kinetic object
!     else                                                                                                        ! If the section has not been found, then nothing to do
!       if (i_Debug_Loc) call Logger%Write( "-> Section has not been found" )                                     ! Debugging
!     end if                                                                                                      ! End if case on found indicator
!     if (i_Debug_Loc) then                                                                                       ! Debugging
!       Reaction_Loc      =       Kinetic%GetSection( Section_Name="Reactions" )                                  ! Debugging: Extracting the "Reaction" section from the global kinetic just to print the updated number of reactions
!       call Logger%Write( "-> Number of reactions in the global kinetic so far: ", Reaction_Loc%NParameters )    ! Debugging
!     end if                                                                                                      ! Debugging
! ! ==============================================================================================================
!
! End Subroutine

End Program
! Program Main
!
!   use Input_Library   ,only:  InputReader_Type, InputSection_Type
!   use Logger_Class    ,only:  Logger
!
!   implicit none
!
!   type(InputReader_Type)      ::  Input
!   type(InputSection_Type)     ::  Section, Section2
!   real(8)                     ::  R0d
!
!
!   call Logger%Write( "Reading input file", NewLine=.True. )
!   call Logger%Write( "-> Calling Input%Read" )
!   call Input%Read( FileName="new_input.dat" )
!   call Logger%Write( "-> Done reading input file" )
!
! !   call Logger%Write( "Outputing input file" )
! !   call Logger%Write( "-> Calling Input%Output" )
! !   call Input%Output( Logger%Unit )  ! Use "Logger%GetUnit()" in new version
! !   call Logger%Write( "-> Done outputing input file" )
!
!   call Logger%Write( "Extracting value from Input object", NewLine=.True. )
!   call Logger%Write( "-> Calling Input%Output" )
!   call Input%GetValue( R0d, "mu", SectionName="analysis_inputs>param_inputs>param3>param3_distribution_inputs" )
!   call Logger%Write( "-> R0d = ", R0d )
!
!   call Logger%Write( "Extracting value from Input object", NewLine=.True. )
!   call Logger%Write( "==================================")
!
!   call Logger%Write( "Method 1: Direclty extract the parameter value from the Input object (Recommended)", NewLine=.True. )
!   call Logger%Write( "-> Calling Input%Output" )
!   call Input%GetValue( R0d, "mu", SectionName="analysis_inputs>param_inputs>param3>param3_distribution_inputs" )
!   call Logger%Write( "-> R0d = ", R0d )
!
!   call Logger%Write( "Method 2: Extracting the last sub-section from the Input object, an then extracing the parameter value from it ", NewLine=.True. )
!   Section   =   Input%GetSection( "param3_distribution_inputs", FromSubSection="analysis_inputs>param_inputs>param3" )
!   call Section%GetValue( R0d, "mu" )
!   call Logger%Write( "-> R0d = ", R0d )
!
!   call Logger%Write( "Method 3: Extracting all sections last sub-section (NOT Recommended)", NewLine=.True. )
!   Section   =   Input%GetSection( "analysis_inputs" )
!   Section   =   Section%GetSection( "param_inputs" )
!   Section   =   Section%GetSection( "param3" )
!   call Section%GetValue( R0d, "mu" )
!   call Logger%Write( "-> R0d = ", R0d )
!
! End Program