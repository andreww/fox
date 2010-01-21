# m_wkml_coverage.f90: m_wkml_coverage.m4
#         m4 -I ../m4 $< > $@
IF(NOT ${M4_PROGRAM} MATCHES "NOTFOUND")
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/m_wkml_coverage.f90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_wkml_coverage.m4 > ${CMAKE_CURRENT_SOURCE_DIR}/m_wkml_coverage.f90
    DEPENDS m_wkml_coverage.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
ENDIF(NOT ${M4_PROGRAM} MATCHES "NOTFOUND")

set(fox_wkml_srcs
  FoX_wkml.f90
)

set(fox_wkml_srcs_pp
  m_wkml_lowlevel.f90
  m_wkml_color.f90
  m_wkml_styling.f90
  m_wkml_features.f90
  m_wkml_coverage.f90
  m_wkml_core.f90
  m_wkml_contours.f90
  m_contours.f90
  m_wkml_color_def.f90
  m_wkml_chart.f90
)

add_library(
  fox_wkml STATIC
  ${fox_wkml_srcs}
  ${fox_wkml_srcs_pp}
)

target_link_libraries(
  fox_wkml
  fox_utils
  fox_wxml
)

include_directories(
  ${fox_BINARY_DIR}/fsys
  ${fox_BINARY_DIR}/utils
  ${fox_BINARY_DIR}/wxml
  ${fox_BINARY_DIR}/common
)

