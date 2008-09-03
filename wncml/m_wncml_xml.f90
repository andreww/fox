module m_wncml_xml
!
! This module provides the basic primatives for writing a NetCDF
! ncml file. It's probably not very useful for the end user but
! makes converting from NetCDF calls to wxml easy.
!

  use FoX_wxml

  implicit none

  character(len=54), parameter :: NAMESPACE = 'http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2'
  character(len=4),  parameter :: PREFIX    = 'ncml:'

contains

  subroutine ncmlStartContainer( xf, path, id, title, enhance, addRecords)

    type(xmlf_t),               intent(inout) :: xf
    character(len=*),           intent(in   ) :: path
    character(len=*), optional, intent(in   ) :: id
    character(len=*), optional, intent(in   ) :: title
    logical,          optional, intent(in   ) :: enhance
    logical,          optional, intent(in   ) :: addRecords
    ! Also have aggrigation atts in NetCDF-4, ignore for now

    ! FIXME - probably should search around inside FoX's namespace dicts 
    !         to see if the namespace is already declared, and if it is
    !         then use this. 
    call xml_DeclareNamespace(xf, prefix=PREFIX, &
      & nsURI=NAMESPACE)

    call xml_NewElement(xf, name=PREFIX//'netcdf')

    call xml_AddAttribute(xf, name='location', value=path)

    if (present(id))         call xml_AddAttribute(xf, name='id', value=id)
    if (present(title))      call xml_AddAttribute(xf, name='title', value=title)
    if (present(enhance))    call xml_AddAttribute(xf, name='enhance', value=enhance)
    if (present(addRecords)) call xml_AddAttribute(xf, name='addRecords', value=addRecords)

  end subroutine ncmlStartContainer

  subroutine ncmlEndContainer( xf )

    type(xmlf_t),      intent(inout) :: xf

    call xml_EndElement(xf, name=PREFIX//'netcdf')

  end subroutine ncmlEndContainer

  subroutine ncmlStartGroup( xf, name, orgName )

    type(xmlf_t),               intent(inout) :: xf
    character(len=*),           intent(in   ) :: name
    character(len=*), optional, intent(in   ) :: orgname

    call xml_NewElement(xf, name=PREFIX//'group')
    call xml_AddAttribute(xf, name='name', value=name)

    if (present(orgName)) call xml_AddAttribute(xf, name='orgName', value=orgname)

  end subroutine ncmlStartGroup

  subroutine ncmlEndGroup ( xf )
    
    type(xmlf_t),               intent(inout) :: xf

    call xml_EndElement(xf, name=PREFIX//'group')

  end subroutine ncmlEndGroup

  subroutine ncmlAddRemove ( xf, name, objecttype )

    type(xmlf_t),               intent(inout) :: xf
    character(len=*),           intent(in   ) :: name
    character(len=*),           intent(in   ) :: objecttype

    ! ObjectType can only have some values.
    if ((trim(objecttype).ne.'attribute').or. &
      & (trim(objecttype).ne.'dimension').or. &
      & (trim(objecttype).ne.'variable' ).or. &
      & (trim(objecttype).ne.'group'    )) &
      & stop 'Type error in ncmlAddRemove' ! FIXME - use FoX errors 

    call xml_NewElement(xf, name=PREFIX//'remove')
    call xml_AddAttribute(xf, name='name', value=name)
    call xml_AddAttribute(xf, name='type', value=objecttype)
    call xml_EndElement(xf, name=PREFIX//'remove')

  end subroutine ncmlAddRemove 

  subroutine ncmlAddDimension( xf, name, len, unlim, varlen, shared, orgname )
  
    type(xmlf_t),               intent(inout) :: xf
    character(len=*),           intent(in   ) :: name
    integer,                    intent(in   ) :: len
    logical,          optional, intent(in   ) :: unlim
    logical,          optional, intent(in   ) :: varlen
    logical,          optional, intent(in   ) :: shared
    character(len=*), optional, intent(in   ) :: orgname

    call xml_NewElement(xf, name=PREFIX//'dimension')

    call xml_AddAttribute(xf, name='name', value=name)
    call xml_AddAttribute(xf, name='length', value=len)

    if (present(unlim))      call xml_AddAttribute(xf, name='isUnlimited', value=unlim)
    if (present(varlen))     call xml_AddAttribute(xf, name='isVariableLength', value=varlen)
    if (present(shared))     call xml_AddAttribute(xf, name='isShared', value=shared)
    if (present(orgname))    call xml_AddAttribute(xf, name='orgName', value=orgname)

    call xml_EndElement(xf, name=PREFIX//'dimension')

  end subroutine ncmlAddDimension

  subroutine ncmlStartVariable( xf, name, xtype, xshape, orgname )

    type(xmlf_t),               intent(inout) :: xf
    character(len=*),           intent(in   ) :: name
    character(len=*),           intent(in   ) :: xtype
    character(len=*),           intent(in   ) :: xshape
    character(len=*), optional, intent(in   ) :: orgname

    call xml_NewElement(xf, name=PREFIX//'variable')

    call xml_AddAttribute(xf, name='name',  value=trim(name))
    call xml_AddAttribute(xf, name='shape', value=xshape)
    call xml_AddAttribute(xf, name='type',  value=xtype)
    
    if (present(orgname)) call xml_AddAttribute(xf, name='orgName', value=orgname)

  end subroutine ncmlStartVariable

  subroutine  ncmlEndVariable ( xf )

    type(xmlf_t),               intent(inout) :: xf

    call xml_EndElement(xf,  name=PREFIX//'variable')

  end subroutine ncmlEndVariable

  subroutine ncmlAddAttribute( xf, name, atype, value, separator, orgName )

    type(xmlf_t),               intent(inout) :: xf
    character(len=*),           intent(in   ) :: name
    character(len=*), optional, intent(in   ) :: atype
    character(len=*), optional, intent(in   ) :: value
    character(len=*), optional, intent(in   ) :: separator
    character(len=*), optional, intent(in   ) :: orgname

    call xml_NewElement(xf, name=PREFIX//'attribute')

    call xml_AddAttribute(xf, name='name', value=name)

    if (present(atype))     call xml_AddAttribute(xf, name='type',      value=atype)
    if (present(value))     call xml_AddAttribute(xf, name='value',     value=value)
    if (present(separator)) call xml_AddAttribute(xf, name='seperator', value=separator)
    if (present(orgname))   call xml_AddAttribute(xf, name='orgName',   value=orgname)

    call xml_EndElement(xf, name=PREFIX//'attribute')

  end subroutine ncmlAddAttribute

end module m_wncml_xml
