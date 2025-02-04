CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

PROJECT(glad LANGUAGES C Fortran)

SET(
    GLAD_CMAKE_DIR
    "${CMAKE_CURRENT_LIST_DIR}"
    CACHE STRING
    "Directory containing the glad generator CMakeLists.txt. Must be absolute."
)

SET(
    GLAD_SOURCES_DIR
     "${GLAD_CMAKE_DIR}/../"
    CACHE STRING
    "Directory containing the glad sources (python modules), used as working directory. Must be absolute."
)

MARK_AS_ADVANCED(GLAD_CMAKE_DIR)

INCLUDE(${GLAD_CMAKE_DIR}/GladConfig.cmake)
