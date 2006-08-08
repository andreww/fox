module m_wcml_coma
  ! Implements routines relating to electronic structure

  use m_common_realtypes, only: sp, dp

  use FoX_common, only: str
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement, xml_AddCharacters
  use m_wcml_stml, only: stmAddValue

  implicit none
  private

  public :: cmlAddEigenvalue
  public :: cmlAddBand
  public :: cmlAddKpoint

  interface cmlAddEigenvalue
    module procedure cmlAddEigenvalueSPSh
    module procedure cmlAddEigenvalueDPSh
    module procedure cmlAddEigenvalueSPSi
    module procedure cmlAddEigenvalueDPSi
  end interface

  interface cmlAddBand
    module procedure cmlAddBand_kpt_sp
    module procedure cmlAddBand_kptref_sp
    module procedure cmlAddBand_kpt_dp
    module procedure cmlAddBand_kptref_dp
  end interface

  interface cmlAddKpoint
    module procedure cmlAddKpoint_sp
    module procedure cmlAddKpoint_dp
  end interface

contains

  subroutine cmlAddBand_kpt_sp(xf, kpoint, kweight, bands, kptfmt, eigfmt)
    type(xmlf_t), intent(inout)            :: xf
    real(sp), intent(in) :: kpoint(3)
    real(sp), intent(in) :: bands(:)
    real(sp), intent(in), optional :: kweight
    character(len=*), intent(in), optional :: kptfmt
    character(len=*), intent(in), optional :: eigfmt

    call xml_NewElement(xf, "band")
    if (present(kweight)) call xml_AddAttribute(xf, "weight", kweight)
    if (present(kptfmt)) then
      call xml_AddAttribute(xf, "kpoint", str(kpoint, kptfmt))
    else
      call xml_AddAttribute(xf, "kpoint", str(kpoint, "r7"))
    endif

    call stmAddValue(xf, value=bands, fmt=eigfmt)
    call xml_EndElement(xf, "band")

  end subroutine cmlAddBand_kpt_sp

  subroutine cmlAddBand_kptref_sp(xf, kptref, bands, eigfmt)
    type(xmlf_t), intent(inout)            :: xf
    character(len=*), intent(in) :: kptref
    real(sp), intent(in) :: bands(:)
    character(len=*), intent(in), optional :: eigfmt

    call xml_NewElement(xf, "band")
    call xml_AddAttribute(xf, "kpointRef", kptref)
    call stmAddValue(xf, value=bands, fmt=eigfmt)
    call xml_EndElement(xf, "band")

  end subroutine cmlAddBand_kptref_sp

  subroutine cmlAddKPoint_sp(xf, kpoint, weight, id, title, conv, dictref, weightfmt, kptfmt)
    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), dimension(3), intent(in)  :: kpoint
    real(kind=sp), intent(in), optional  :: weight
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: weightfmt
    character(len=*), intent(in), optional :: kptfmt

    call xml_NewElement(xf, "kpoint")
    if (present(id))      call xml_AddAttribute(xf, "id", id)
    if (present(title))   call xml_AddAttribute(xf, "title", title)
    if (present(dictref)) call xml_AddAttribute(xf, "dictRef", dictref)
    if (present(conv))    call xml_AddAttribute(xf, "convention", conv)

    if (present(weight)) then
      if (present(weightfmt)) then
        call xml_AddAttribute(xf, "weight", str(weight, weightfmt))
      else
        call xml_AddAttribute(xf, "weight", str(weight, "r3"))
      endif
    endif

    if (present(kptfmt)) then
      call xml_AddCharacters(xf, kpoint, kptfmt)
    else
      call xml_AddCharacters(xf, kpoint, "r3")
    end if

    call xml_EndElement(xf, "kpoint")

  end subroutine cmlAddKPoint_sp

  subroutine cmlAddEigenvaluespSh(xf, eigvec, eigval, id, title, dictref, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: eigvec(:,:)
    real(kind=sp), intent(in)              :: eigval(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt

    call xml_NewElement(xf, "eigen")
    if (present(id))      call xml_AddAttribute(xf, "id", id)
    if (present(title))   call xml_AddAttribute(xf, "title", title)
    if (present(dictref)) call xml_AddAttribute(xf, "dictRef", dictRef)
    call stmAddValue(xf=xf, value=eigval, title="eigenvalues", dictref=dictRef, fmt=fmt)
    call stmAddValue(xf=xf, value=eigvec, title="eigenvectors", fmt=fmt)
    call xml_EndElement(xf, "eigen")
    
  end subroutine cmlAddEigenvaluespSh

  subroutine cmlAddEigenvaluespSi(xf, n, eigvec, eigval, id, title, dictref, fmt)
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: n
    real(kind=sp), intent(in)              :: eigvec(n,*)
    real(kind=sp), intent(in)              :: eigval(*)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt

    call xml_NewElement(xf, "eigen")
    if (present(id))      call xml_AddAttribute(xf, "id", id)
    if (present(title))   call xml_AddAttribute(xf, "title", title)
    if (present(dictref)) call xml_AddAttribute(xf, "dictRef", dictRef)
    call stmAddValue(xf=xf, value=eigval(:n), title="eigenvalues", dictref=dictRef, fmt=fmt)
    call stmAddValue(xf=xf, value=eigvec(:n,:n), title="eigenvectors", fmt=fmt)
    call xml_EndElement(xf, "eigen")
    
  end subroutine cmlAddEigenvaluespSi


  subroutine cmlAddBand_kpt_dp(xf, kpoint, kweight, bands, kptfmt, eigfmt)
    type(xmlf_t), intent(inout)            :: xf
    real(dp), intent(in) :: kpoint(3)
    real(dp), intent(in) :: bands(:)
    real(dp), intent(in), optional :: kweight
    character(len=*), intent(in), optional :: kptfmt
    character(len=*), intent(in), optional :: eigfmt

    call xml_NewElement(xf, "band")
    if (present(kweight)) call xml_AddAttribute(xf, "weight", kweight)
    if (present(kptfmt)) then
      call xml_AddAttribute(xf, "kpoint", str(kpoint, kptfmt))
    else
      call xml_AddAttribute(xf, "kpoint", str(kpoint, "r7"))
    endif

    call stmAddValue(xf, value=bands, fmt=eigfmt)
    call xml_EndElement(xf, "band")

  end subroutine cmlAddBand_kpt_dp

  subroutine cmlAddBand_kptref_dp(xf, kptref, bands, eigfmt)
    type(xmlf_t), intent(inout)            :: xf
    character(len=*), intent(in) :: kptref
    real(dp), intent(in) :: bands(:)
    character(len=*), intent(in), optional :: eigfmt

    call xml_NewElement(xf, "band")
    call xml_AddAttribute(xf, "kpointRef", kptref)
    call stmAddValue(xf, value=bands, fmt=eigfmt)
    call xml_EndElement(xf, "band")

  end subroutine cmlAddBand_kptref_dp

  subroutine cmlAddKPoint_dp(xf, kpoint, weight, id, title, conv, dictref, weightfmt, kptfmt)
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), dimension(3), intent(in)  :: kpoint
    real(kind=dp), intent(in), optional  :: weight
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: weightfmt
    character(len=*), intent(in), optional :: kptfmt

    call xml_NewElement(xf, "kpoint")
    if (present(id))      call xml_AddAttribute(xf, "id", id)
    if (present(title))   call xml_AddAttribute(xf, "title", title)
    if (present(dictref)) call xml_AddAttribute(xf, "dictRef", dictref)
    if (present(conv))    call xml_AddAttribute(xf, "convention", conv)

    if (present(weight)) then
      if (present(weightfmt)) then
        call xml_AddAttribute(xf, "weight", str(weight, weightfmt))
      else
        call xml_AddAttribute(xf, "weight", str(weight, "r3"))
      endif
    endif

    if (present(kptfmt)) then
      call xml_AddCharacters(xf, kpoint, kptfmt)
    else
      call xml_AddCharacters(xf, kpoint, "r3")
    end if

    call xml_EndElement(xf, "kpoint")

  end subroutine cmlAddKPoint_dp

  subroutine cmlAddEigenvaluedpSh(xf, eigvec, eigval, id, title, dictref, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: eigvec(:,:)
    real(kind=dp), intent(in)              :: eigval(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt

    call xml_NewElement(xf, "eigen")
    if (present(id))      call xml_AddAttribute(xf, "id", id)
    if (present(title))   call xml_AddAttribute(xf, "title", title)
    if (present(dictref)) call xml_AddAttribute(xf, "dictRef", dictRef)
    call stmAddValue(xf=xf, value=eigval, title="eigenvalues", dictref=dictRef, fmt=fmt)
    call stmAddValue(xf=xf, value=eigvec, title="eigenvectors", fmt=fmt)
    call xml_EndElement(xf, "eigen")
    
  end subroutine cmlAddEigenvaluedpSh

  subroutine cmlAddEigenvaluedpSi(xf, n, eigvec, eigval, id, title, dictref, fmt)
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: n
    real(kind=dp), intent(in)              :: eigvec(n,*)
    real(kind=dp), intent(in)              :: eigval(*)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt

    call xml_NewElement(xf, "eigen")
    if (present(id))      call xml_AddAttribute(xf, "id", id)
    if (present(title))   call xml_AddAttribute(xf, "title", title)
    if (present(dictref)) call xml_AddAttribute(xf, "dictRef", dictRef)
    call stmAddValue(xf=xf, value=eigval(:n), title="eigenvalues", dictref=dictRef, fmt=fmt)
    call stmAddValue(xf=xf, value=eigvec(:n,:n), title="eigenvectors", fmt=fmt)
    call xml_EndElement(xf, "eigen")
    
  end subroutine cmlAddEigenvaluedpSi


end module m_wcml_coma
