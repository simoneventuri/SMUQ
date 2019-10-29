
# Get the name of the component
get_filename_component( COMPONENT_NAME "${CMAKE_CURRENT_SOURCE_DIR}" ABSOLUTE)
get_filename_component( COMPONENT_NAME "${COMPONENT_NAME}" NAME)

# Set the target name
set ( TARGET_NAME "${PROJECT_NAME}-${COMPONENT_NAME}-Example.x" )

# Add the executable
add_executable ( ${TARGET_NAME}
    main.f90
)

# Link the library to the executable
target_link_libraries( ${TARGET_NAME} LINK_PUBLIC ${STATIC_LIBRARY} )

# Install the executable
install(  TARGETS       ${TARGET_NAME}
          DESTINATION   ${EXECUTABLE_INSTALL_DIR} )


