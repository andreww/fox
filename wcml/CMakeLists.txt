# m_wcml_coma.F90: m_wcml_coma.m4
#         m4 -I ../m4 $< > $@
IF(NOT ${M4_PROGRAM} MATCHES "NOTFOUND")
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_coma.F90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_coma.m4 > ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_coma.F90
    DEPENDS m_wcml_coma.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
  
  # m_wcml_geometry.F90: m_wcml_geometry.m4
  #         m4 -I ../m4 $< > $@
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_geometry.F90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_geometry.m4 > ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_geometry.F90
    DEPENDS m_wcml_geometry.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
  
  # m_wcml_lattice.F90: m_wcml_lattice.m4
  #         m4 -I ../m4 $< > $@
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_lattice.F90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_lattice.m4 > ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_lattice.F90
    DEPENDS m_wcml_lattice.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
  
  # m_wcml_lists.F90: m_wcml_lists.m4
  #         m4 -I ../m4 $< > $@
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_lists.F90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_lists.m4 > ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_lists.F90
    DEPENDS m_wcml_lists.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
  
  # m_wcml_molecule.F90: m_wcml_molecule.m4
  #         m4 -I ../m4 $< > $@
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_molecule.F90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_molecule.m4 > ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_molecule.F90
    DEPENDS m_wcml_molecule.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
  
  # m_wcml_parameter.F90: m_wcml_parameter.m4
  #         m4 -I ../m4 $< > $@
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_parameter.F90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_parameter.m4 > ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_parameter.F90
    DEPENDS m_wcml_parameter.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
  
  # m_wcml_property.F90: m_wcml_property.m4
  #         m4 -I ../m4 $< > $@
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_property.F90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_property.m4 > ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_property.F90
    DEPENDS m_wcml_property.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
  
  # m_wcml_stml.F90: m_wcml_stml.m4
  #         m4 -I ../m4 $< > $@
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_stml.F90
    COMMAND ${M4_PROGRAM} -I ../m4 ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_stml.m4 > ${CMAKE_CURRENT_SOURCE_DIR}/m_wcml_stml.F90
    DEPENDS m_wcml_stml.m4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
ENDIF(NOT ${M4_PROGRAM} MATCHES "NOTFOUND")


set(fox_wcml_srcs
  FoX_wcml.f90
)

set(fox_wcml_srcs_pp
  m_wcml_coma.F90
  m_wcml_core.F90
  m_wcml_geometry.F90
  m_wcml_lattice.F90
  m_wcml_lists.F90
  m_wcml_metadata.F90
  m_wcml_molecule.F90
  m_wcml_parameter.F90
  m_wcml_property.F90
  m_wcml_stml.F90
  m_wcml_inputdec.F90
)

MK_LIBS(wcml utils wxml)