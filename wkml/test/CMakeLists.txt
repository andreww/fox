
#################################################################
# FoX Wkml tests
#################################################################

FILE(GLOB_RECURSE TESTS_WKML_SRC *.f90 *.F90)

FOREACH(TEST_SRC ${TESTS_WKML_SRC})

    GET_FILENAME_COMPONENT(EXE_NAME ${TEST_SRC} NAME_WE)
    IF(NOT (${EXE_NAME} STREQUAL "m_contours_test_data_sp"))
        ADD_EXECUTABLE(${EXE_NAME} ${TEST_SRC})
        TARGET_SOURCES(${EXE_NAME} PUBLIC m_contours_test_data_sp.f90)
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${PROJECT_NAME}_wkml)
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${PROJECT_NAME}_wxml)
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${PROJECT_NAME}_common)
        TARGET_LINK_LIBRARIES(${EXE_NAME} ${PROJECT_NAME}_fsys)
        FOREACH (EXT_LIB ${EXT_LIBS})
            IF(DEFINED ${PROJECT_NAME}_ENABLE_${EXT_LIB} AND ${PROJECT_NAME}_ENABLE_${EXT_LIB} AND ${EXT_LIB}_FOUND)
                TARGET_LINK_LIBRARIES(${EXE_NAME} ${${EXT_LIB}_LIBRARIES})
            ENDIF()
        ENDFOREACH()
        ADD_CUSTOM_COMMAND(TARGET ${EXE_NAME} PRE_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/sax/test/testin.xml ${PROJECT_BINARY_DIR}/sax/test)
        ADD_TEST(${EXE_NAME}_TEST ${EXECUTABLE_OUTPUT_PATH}/${EXE_NAME})
    ENDIF()
ENDFOREACH()



