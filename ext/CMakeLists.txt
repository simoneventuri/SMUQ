######################################################
# Linking QuadPack to smuqApp #
######################################################
ExternalProject_Add( quadpackExt
                     SOURCE_DIR ${SMUQ_SOURCE_DIR}/ext/quadpack
                     BINARY_DIR ${SMUQ_BINARY_DIR}/ext/quadpack
                     INSTALL_DIR ${SMUQ_BINARY_DIR}/ext_install/quadpack
                     CMAKE_ARGS -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
)

add_library( quadpack STATIC IMPORTED GLOBAL )

set_property(TARGET quadpack 
             PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/quadpack/lib/libquadpack.a"
)

target_include_directories( smuqApp 
                            PRIVATE "${SMUQ_BINARY_DIR}/ext_install/quadpack/include"
)

add_dependencies( smuqApp quadpack quadpackExt )

target_link_libraries( smuqApp quadpack )

######################################################
# Linking PROB to smuqApp #
######################################################
ExternalProject_Add( probExt
                     SOURCE_DIR ${SMUQ_SOURCE_DIR}/ext/prob
                     BINARY_DIR ${SMUQ_BINARY_DIR}/ext/prob
                     INSTALL_DIR ${SMUQ_BINARY_DIR}/ext_install/prob
                     CMAKE_ARGS -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
)
add_library( prob STATIC IMPORTED GLOBAL )

set_property( TARGET prob 
              PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/prob/lib/libprob.a"
)

target_include_directories( smuqApp 
                            PRIVATE "${SMUQ_BINARY_DIR}/ext_install/prob/include"
)

add_dependencies( smuqApp prob probExt )

target_link_libraries( smuqApp prob )

######################################################
# Linking Sobol to smuqApp #
######################################################
ExternalProject_Add( sobolExt
                     SOURCE_DIR ${SMUQ_SOURCE_DIR}/ext/sobol
                     BINARY_DIR ${SMUQ_BINARY_DIR}/ext/sobol
                     INSTALL_DIR ${SMUQ_BINARY_DIR}/ext_install/sobol
                     CMAKE_ARGS -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
)
add_library( sobol STATIC IMPORTED GLOBAL )

set_property( TARGET sobol 
              PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/sobol/lib/libsobol.a"
)

target_include_directories( smuqApp 
                            PRIVATE "${SMUQ_BINARY_DIR}/ext_install/sobol/include"
)

add_dependencies( smuqApp sobol sobolExt )

target_link_libraries( smuqApp sobol )

######################################################
# Linking QRUpdate to smuqApp #
######################################################
ExternalProject_Add( qrupdateExt
                     SOURCE_DIR ${SMUQ_SOURCE_DIR}/ext/qrupdate
                     BINARY_DIR ${SMUQ_BINARY_DIR}/ext/qrupdate
                     INSTALL_DIR ${SMUQ_BINARY_DIR}/ext_install/qrupdate
                     CMAKE_ARGS -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
)

add_library( qrupdate STATIC IMPORTED GLOBAL )

set_property( TARGET qrupdate 
              PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/qrupdate/lib/libqrupdate.a"
)

target_include_directories( smuqApp 
                            PRIVATE "${SMUQ_BINARY_DIR}/ext_install/qrupdate/include"
)

add_dependencies( smuqApp qrupdate qrupdateExt )

target_link_libraries( smuqApp qrupdate )

######################################################
# Linking MT64 to smuqApp #
######################################################
ExternalProject_Add( mt64Ext
                     SOURCE_DIR ${SMUQ_SOURCE_DIR}/ext/mt64
                     BINARY_DIR ${SMUQ_BINARY_DIR}/ext/mt64
                     INSTALL_DIR ${SMUQ_BINARY_DIR}/ext_install/mt64
                     CMAKE_ARGS -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
)

add_library( mt64 STATIC IMPORTED GLOBAL )

set_property( TARGET mt64 
              PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/mt64/lib/libmt64.a"
)

target_include_directories( smuqApp 
                            PRIVATE "${SMUQ_BINARY_DIR}/ext_install/mt64/include"
)

add_dependencies( smuqApp mt64 mt64Ext )

target_link_libraries( smuqApp mt64 )

######################################################
# Linking GLMNet to smuqApp #
######################################################
ExternalProject_Add( glmnetExt
                     SOURCE_DIR ${SMUQ_SOURCE_DIR}/ext/glmnet
                     BINARY_DIR ${SMUQ_BINARY_DIR}/ext/glmnet
                     INSTALL_DIR ${SMUQ_BINARY_DIR}/ext_install/glmnet
                     CMAKE_ARGS -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
)

add_library( glmnet STATIC IMPORTED GLOBAL )

set_property( TARGET glmnet 
              PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/glmnet/lib/libglmnet.a"
)

target_include_directories( smuqApp 
                            PRIVATE "${SMUQ_BINARY_DIR}/ext_install/glmnet/include"
)

add_dependencies( smuqApp glmnet glmnetExt )

target_link_libraries( smuqApp glmnet )

######################################################
# Linking CDF to smuqApp #
######################################################
ExternalProject_Add( cdfExt
                     SOURCE_DIR ${SMUQ_SOURCE_DIR}/ext/cdf
                     BINARY_DIR ${SMUQ_BINARY_DIR}/ext/cdf
                     INSTALL_DIR ${SMUQ_BINARY_DIR}/ext_install/cdf
                     CMAKE_ARGS -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
)

add_library( cdf STATIC IMPORTED GLOBAL )

set_property( TARGET cdf 
              PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/cdf/lib/libcdf.a"
)

target_include_directories( smuqApp 
                            PRIVATE "${SMUQ_BINARY_DIR}/ext_install/cdf/include"
)

add_dependencies( smuqApp cdf cdfExt )

target_link_libraries( smuqApp cdf )

######################################################
# Linking ODEPack to smuqApp #
######################################################
ExternalProject_Add( odepackExt
                     SOURCE_DIR ${SMUQ_SOURCE_DIR}/ext/odepack
                     BINARY_DIR ${SMUQ_BINARY_DIR}/ext/odepack
                     INSTALL_DIR ${SMUQ_BINARY_DIR}/ext_install/odepack
                     CMAKE_ARGS -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
)

add_library( odepack STATIC IMPORTED GLOBAL )

set_property( TARGET odepack 
              PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/odepack/lib/libodepack.a"
)

target_include_directories( smuqApp 
                            PRIVATE "${SMUQ_BINARY_DIR}/ext_install/odepack/include"
)

add_dependencies( smuqApp odepack odepackExt )

target_link_libraries( smuqApp odepack )

######################################################
# Linking Brent to smuqApp #
######################################################
ExternalProject_Add( brentExt
                     SOURCE_DIR ${SMUQ_SOURCE_DIR}/ext/brent
                     BINARY_DIR ${SMUQ_BINARY_DIR}/ext/brent
                     INSTALL_DIR ${SMUQ_BINARY_DIR}/ext_install/brent
                     CMAKE_ARGS -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
)

add_library( brent STATIC IMPORTED GLOBAL )

set_property( TARGET brent 
              PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/brent/lib/libbrent.a"
)

target_include_directories( smuqApp 
                            PRIVATE "${SMUQ_BINARY_DIR}/ext_install/brent/include"
)

add_dependencies( smuqApp brent brentExt )

target_link_libraries( smuqApp brent )

######################################################
# Linking FORPACK to smuqApp #
######################################################
ExternalProject_Add( forpackExt
                     SOURCE_DIR ${SMUQ_SOURCE_DIR}/ext/forpack
                     BINARY_DIR ${SMUQ_BINARY_DIR}/ext/forpack
                     INSTALL_DIR ${SMUQ_BINARY_DIR}/ext_install/forpack
                     CMAKE_ARGS -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
)

add_library( forpack STATIC IMPORTED GLOBAL )

set_property( TARGET forpack 
              PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/forpack/lib/libforpack.a"
)

target_include_directories( smuqApp 
                            PRIVATE "${SMUQ_BINARY_DIR}/ext_install/forpack/mod"
)

add_dependencies( smuqApp forpack forpackExt )

target_link_libraries( smuqApp forpack )

######################################################
# Linking LAPACK and BLAS to smuqApp #
######################################################
set( BLA_STATIC ON )
find_package( LAPACK )
find_package( BLAS )

if ( NOT (${LAPACK_FOUND} AND ${BLAS_FOUND}) )
  ExternalProject_Add( lapackExt
                       GIT_REPOSITORY https://github.com/Reference-LAPACK/lapack-release.git
                       GIT_TAG ba3779a6813d84d329b73aac86afc4e041170609
                       SOURCE_DIR ${SMUQ_BINARY_DIR}/ext_download_source/lapack
                       BINARY_DIR ${SMUQ_BINARY_DIR}/ext/lapack
                       INSTALL_DIR ${SMUQ_BINARY_DIR}/ext_install/lapack
                       CMAKE_ARGS -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
  )

  add_library( lapack STATIC IMPORTED GLOBAL )
  add_library( blas STATIC IMPORTED GLOBAL )

  set_property( TARGET lapack
                PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/lapack/lib/liblapack.a"
  )

  set_property( TARGET blas
                PROPERTY IMPORTED_LOCATION "${SMUQ_BINARY_DIR}/ext_install/lapack/lib/libblas.a"
  )

  add_dependencies( smuqApp lapack blas lapackExt )

  target_link_libraries( smuqApp lapack blas )
else()
  target_link_libraries( smuqApp ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES} )
endif()
