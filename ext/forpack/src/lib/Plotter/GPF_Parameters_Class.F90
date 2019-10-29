Module GPF_Parameters_Class

! @TODO: Some prameters are related to Graph objects... and so there will be a problem is a figure is made of several graphs

  use GPF_Parameters            ,only:  rkp

  implicit none

  private
  public  ::  GPF_Parameters_Type

  Type                                          ::  GPF_Parameters_Type
    private
    logical                                     ::  i_Directory             =       .False.
    logical                                     ::  i_FileName_Prefix       =       .False.
    logical                                     ::  i_File_Extension        =       .False.
    logical                                     ::  i_X_Label               =       .False.
    logical                                     ::  i_Y_Label               =       .False.
    logical                                     ::  i_Z_Label               =       .False.
    logical                                     ::  i_X_LogScale            =       .False.
    logical                                     ::  i_Y_LogScale            =       .False.
    logical                                     ::  i_Z_LogScale            =       .False.
    logical                                     ::  i_X_Format              =       .False.

    character(:)        ,allocatable            ::  Group_Name                                                      ! Name of a group of plots
    character(:)        ,allocatable            ::  Directory                                                       ! Directory where to save the image
    character(:)        ,allocatable            ::  FileName_Prefix                                                 ! Prefix of FileName
    character(:)        ,allocatable            ::  FileName_Suffix                                                 ! Suffix of FileName
    character(:)        ,allocatable            ::  File_Extension                                                  ! Extension of the File
    character(:)        ,allocatable            ::  X_Label                                                         ! Label of X-axis
    character(:)        ,allocatable            ::  Y_Label                                                         ! Label of Y-axis
    character(:)        ,allocatable            ::  Z_Label                                                         ! Label of Z-axis
    logical                                     ::  X_LogScale              =       .False.                         ! Indicator of logarithm scale for the X-axis
    logical                                     ::  Y_LogScale              =       .False.                         ! Indicator of logarithm scale for the Y-axis
    logical                                     ::  Z_LogScale              =       .False.                         ! Indicator of logarithm scale for the Y-axis
    character(:)        ,allocatable            ::  X_Format                                                        ! X-Axis format

    real(rkp)   ,dimension(:,:) ,allocatable    ::  X_2D
    real(rkp)   ,dimension(:,:) ,allocatable    ::  Y_2D

  contains
    private
    procedure   ,public   ::  Initialize      =>  Initialize_Parameters
    procedure   ,public   ::  Set             =>  Set_Parameters
    procedure   ,public   ::  Get_Group_Name
    procedure   ,public   ::  GetDirectory
    procedure   ,public   ::  GetTitle
    procedure   ,public   ::  GetFileName
    procedure   ,public   ::  Get_FileName_Prefix
    procedure   ,public   ::  Get_FileName_Suffix
    procedure   ,public   ::  Get_File_Extension
    procedure   ,public   ::  Get_X_Label
    procedure   ,public   ::  Get_Y_Label
    procedure   ,public   ::  Get_X_LogScale
    procedure   ,public   ::  Get_X_2D
    procedure   ,public   ::  Get_Y_2D
  End Type

  Interface           GPF_Parameters_Type
    Module Procedure  GPFParametersConstructor
  End Interface

  Interface
    Pure Module Function GPFParametersConstructor() result(This)
      type(GPF_Parameters_Type)                                             ::  This
    End Function
    Pure Module Subroutine Initialize_Parameters( This )
      class(GPF_Parameters_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument
    End Subroutine
    Module Subroutine Set_Parameters( This, Group_Name,                                                          &
                                      Directory, FileName_Prefix, FileName_Suffix, File_Extension,    &
                                      X_Label, Y_Label, Z_Label,                                      &
                                      X_LogScale, Y_LogScale, Z_LogScale,                             &
                                      X_Format,                                                       &
                                      X_2D, Y_2D                                                      )
      class(GPF_Parameters_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Plotter-Options object
      character(*)                                ,optional ,intent(in)     ::  Group_Name                      !< Nameof a group of images
      character(*)                                ,optional ,intent(in)     ::  Directory                       !< Directory where to save the image
      character(*)                                ,optional ,intent(in)     ::  FileName_Prefix                 !< Prefix for the file name
      character(*)                                ,optional ,intent(in)     ::  FileName_Suffix                 !< Suffix for the file name
      character(*)                                ,optional ,intent(in)     ::  File_Extension                  !< File Extension
      character(*)                                ,optional ,intent(in)     ::  X_Label                         !< Label of X-axis
      character(*)                                ,optional ,intent(in)     ::  Y_Label                         !< Label of Y-axis
      character(*)                                ,optional ,intent(in)     ::  Z_Label                         !< Label of Z-axis
      logical                                     ,optional ,intent(in)     ::  X_LogScale                      !<
      logical                                     ,optional ,intent(in)     ::  Y_LogScale                      !<
      logical                                     ,optional ,intent(in)     ::  Z_LogScale                      !<
      character(*)                                ,optional ,intent(in)     ::  X_Format                        !<
      real(rkp)           ,optional ,dimension(:,:)         ,intent(in)     ::  X_2D
      real(rkp)           ,optional ,dimension(:,:)         ,intent(in)     ::  Y_2D
    End Subroutine
    Pure Module Function Get_Group_Name( This ) result(Group_Name)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  Group_Name
    End Function
    Pure Module Function GetDirectory( This ) result(Directory)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  Directory
    End Function
    Pure Module Function GetTitle( This, BaseTitle ) result(Title)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  BaseTitle
      character(:)  ,allocatable                                            ::  Title
    End Function
    Pure Module Function GetFileName( This, BaseName ) result(FileName)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(*)                                ,optional ,intent(in)     ::  BaseName
      character(:)  ,allocatable                                            ::  FileName
    End Function
    Pure Module Function Get_FileName_Prefix( This ) result(FileName_Prefix)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  FileName_Prefix
    End Function
    Pure Module Function Get_FileName_Suffix( This ) result(FileName_Suffix)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  FileName_Suffix
    End Function
    Pure Module Function Get_File_Extension( This ) result(File_Extension)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  File_Extension
    End Function
    Pure Module Function Get_X_Label( This ) result(X_Label)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  X_Label
    End Function
    Pure Module Function Get_Y_Label( This ) result(Y_Label)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  Y_Label
    End Function
    Pure Module Function Get_X_LogScale( This ) result(X_LogScale)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      logical                                                               ::  X_LogScale
    End Function
    Pure Module Function Get_X_2D( This ) result(X_2D)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(rkp)     ,dimension(size(This%X_2D,1),size(This%X_2D,2))         ::  X_2D
    End Function
    Pure Module Function Get_Y_2D( This ) result(Y_2D)
      class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      real(rkp)     ,dimension(size(This%Y_2D,1),size(This%Y_2D,2))         ::  Y_2D
    End Function
  End Interface

End Module