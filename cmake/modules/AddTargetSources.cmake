function( add_target_sources targetName )

  foreach( arg IN LISTS ARGN )
    if( EXISTS "${CMAKE_CURRENT_LIST_DIR}/${arg}" )
      target_sources( ${targetName}
        PRIVATE "${CMAKE_CURRENT_LIST_DIR}/${arg}"
      )
    else()
      message( FATAL_ERROR
        "Source file not found for target ${targetName}: ${CMAKE_CURRENT_LIST_DIR}/${arg}"
      )
    endif()
  endforeach()

endfunction()

