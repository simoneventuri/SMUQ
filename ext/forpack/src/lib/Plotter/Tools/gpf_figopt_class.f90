! #include "preprocessor_variables.inc"
Module GPF_FigOpt_Class

  implicit none

  private

  public  ::  GPF_FigOpt_Type

  Type                          ::  GPF_FigOpt_Type
    logical                     ::  i_Directory     =       .False.
    logical                     ::  i_TitlePrefix   =       .False.
    logical                     ::  i_X_Label       =       .False.
    logical                     ::  i_X_Log         =       .False.
    logical                     ::  i_Y_Log         =       .False.
    logical                     ::  i_Z_Log         =       .False.
    character(:)        ,allocatable     ::  Directory                       ! Directory where to save the image
    character(:)        ,allocatable     ::  TitlePrefix                     ! Prefix for graph title
    character(:)        ,allocatable     ::  X_Label                         ! Label of X-axis
    logical                     ::  X_Log                           ! Indicator of logarithm scale for the X-axis
    logical                     ::  Y_Log                           ! Indicator of logarithm scale for the Y-axis
    logical                     ::  Z_Log                           ! Indicator of logarithm scale for the Y-axis
  End Type

  Interface           GPF_FigOpt_Type
    Module procedure  Construct_FigOpt
  End Interface

  contains

Function Construct_FigOpt( Directory, TitlePrefix, X_Label, X_Log, Y_Log, Z_Log ) result(This)

  implicit none

  type(GPF_FigOpt_Type)                                                 ::  This                            !<
  character(*)                                ,optional ,intent(in)     ::  Directory                       !< Directory where to save the image
  character(*)                                ,optional ,intent(in)     ::  TitlePrefix                     !< Prefix for graph title
  character(*)                                ,optional ,intent(in)     ::  X_Label                         !< Label of X-axis
  logical                                     ,optional ,intent(in)     ::  X_Log                           !<
  logical                                     ,optional ,intent(in)     ::  Y_Log                           !<
  logical                                     ,optional ,intent(in)     ::  Z_Log                           !<

!   This%Directory      =       ""
!   This%TitlePrefix    =       ""
!   This%X_Label        =       ""

  if ( present(Directory)       ) then
    This%i_Directory    =       .True.
    This%Directory      =       Directory
  end if

  if ( present(TitlePrefix)     ) then
    This%i_TitlePrefix  =       .True.
    This%TitlePrefix    =       TitlePrefix
  end if

  if ( present(X_Label)         ) then
    This%i_X_Label      =       .True.
    This%X_Label        =       X_Label
  end if

  if ( present(X_Log)         ) then
    This%i_X_Log        =       .True.
    This%X_Log          =       X_Log
  end if

  if ( present(Y_Log)         ) then
    This%i_Y_Log        =       .True.
    This%Y_Log          =       Y_Log
  end if

  if ( present(Z_Log)         ) then
    This%i_Z_Log        =       .True.
    This%Z_Log          =       Z_Log
  end if

End Function

End Module
