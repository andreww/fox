define(`TOHW_m4_wkml_createCells'`',`dnl
  subroutine $1_$2(xf, &
$3, values, &
    mask, colormap, height, contour_values, num_levels, name)
    type(xmlf_t) :: xf
    real($2), intent(in) :: $4
    real($2), intent(in) :: values(:,:)
    real($2), intent(in), optional :: mask
    type(color), target, optional :: colormap(:)
    real($2), intent(in), optional :: height
    real($2), intent(in), optional :: contour_values(:)
    integer, intent(in), optional :: num_levels
    character(len=*), intent(in), optional :: name

    integer  :: i, ic, j, k, m, n, numcolors
    real($2) :: square(3,4), lat, long, average
    real($2) :: minvalue, lat_inc, long_inc, valueres !resolution of input value
    character(len=15), allocatable :: styleURL(:) ! FIXME this ought to be dynamically sized,
                                     ! but this allows up to 10^9 separate IDs in one doc.
    type(color), pointer :: defaultMap(:), thisColor

    m = size(values, 1)
    n = size(values, 2)

    if (present(contour_values).and.present(num_levels)) then
      print*,"Cannot specify both contour_values and num_levels in kmlCreateCells"
      stop
    elseif (present(contour_values)) then
      if (present(colormap)) then
        if (size(colormap)/=size(contour_values)+1) then
          print*,"colormap must be one item longer than contour_values in kmlCreateCells"
          stop
        endif
      endif
      numcolors = size(contour_values)+1
    elseif (present(num_levels)) then
      if (present(colormap)) then
        if (size(colormap)/=num_levels+1) then
          print*,"colormap must be one item longer than num_levels in kmlCreateCells"
          stop
        endif
      endif
      numcolors = num_levels+1
    else
      if (present(colormap)) then
        numcolors = size(colormap)
      else
        numcolors = 5
      endif
    endif

    if (.not.present(colormap)) defaultMap => kmlMakeColorMap(numcolors)
    
    minvalue = minval(values)
    if (present(mask)) then
      valueres = (maxval(values, mask=(values<mask))-minvalue)/(numcolors-1)
    else
      valueres = (maxval(values, mask=(values<mask))-minvalue)/(numcolors-1)
    endif

    call kmlOpenFolder(xf, name=name)

! When we have a working style-handling policy, replace here.
!!$    allocate(styleURL(numcolors))
!!$    do i = 1, numcolors
!!$      styleURL(i) = xmlf_NewId(xf)
!!$      if (present(colormap)) then
!!$        call kmlCreatePolygonStyle(xf, color=colormap(i), id=trim(styleURL(i)))
!!$      else
!!$        call kmlCreatePolygonStyle(xf, color=defaultMap(i), id=trim(styleURL(i)))
!!$      endif
!!$    end do

$5

    call kmlCloseFolder(xf)

    if (.not.present(colormap)) deallocate(defaultMap)

  end subroutine $1_$2
')`'dnl
dnl
define(`TOHW_m4_wkml_spdp_createCells', `'`dnl
TOHW_m4_wkml_createCells(`$1', `sp', `$2', `$3', `$4')`'dnl
TOHW_m4_wkml_createCells(`$1', `dp', `$2', `$3', `$4')`'dnl
')`'dnl
dnl
module m_wkml_coverage

  use m_common_realtypes, only: sp, dp
  use m_common_error, only: FoX_error
  use FoX_wxml, only: xmlf_t, xmlf_NewId, xmlf_OpenTag
  use FoX_common, only: str

  use m_wkml_lowlevel, only: kmlOpenFolder, kmlCloseFolder
  use m_wkml_color, only: color, kmlSetCustomColor, kmlMakeColorMap
  use m_wkml_features, only: kmlStartRegion, kmlEndRegion
  use m_wkml_styling, only: kmlCreatePolygonStyle

  implicit none
  private

  interface kmlCreateRGBCells
    module procedure kmlCreateRGBCells_sp
    module procedure kmlCreateRGBCells_dp
  end interface kmlCreateRGBCells

  interface kmlCreateCells
    module procedure kmlCreateCells_sp
    module procedure kmlCreateCells_dp
    module procedure kmlCreateCells_longlat_sp
    module procedure kmlCreateCells_longlat_dp
    module procedure kmlCreateCells_longlat2_sp
    module procedure kmlCreateCells_longlat2_dp
  end interface kmlCreateCells

  public :: kmlCreateRGBCells
  public :: kmlCreateCells

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !this subroutine is used for creating cells (pixels),this version is used for SIG and assing rgb color auto

  subroutine kmlCreateRGBCells_sp(xf, east, west, south, north, reflectance, rgb, numbit)
    type(xmlf_t) :: xf
    real(sp), intent(in)  :: east, west, south, north
    real(sp), intent(in), optional :: reflectance(:,:)
    integer, intent(in), optional :: numbit !! color interval
    character, intent(in), optional :: rgb

    call kmlCreateRGBCells(xf, real(east, dp), real(west, dp), &
      real(south, dp), real(north, dp), real(reflectance, dp), rgb, numbit)
  end subroutine kmlCreateRGBCells_sp

  subroutine kmlCreateRGBCells_dp(xf, east, west, south, north, reflectance, rgb, numbit)
    type(xmlf_t) :: xf
    real(dp), intent(in)  :: east, west, south, north
    real(dp), intent(in), optional :: reflectance(:,:)
    integer, intent(in), optional :: numbit !! color interval
    character, intent(in), optional :: rgb

    integer :: numbit_
    integer :: i, dn

    type(color), allocatable :: colormap(:)

    if (rgb/="r" .and. rgb/="g" .and. rgb/="b") then
      print*, "Must use one of r, g, or b in CreateRGBCells"
      stop
    endif

    if (present(numbit)) then
      if (numbit<1.or.numbit>256) then
        print*, "numbit out of range"
        stop
      elseif (mod(256, numbit)/=0) then
        print*, "numbit must be a power of 2"
        stop
      endif
      numbit_ = numbit ! Check for sensible values
    else
      numbit_ = 256
    endif

    allocate(colormap(numbit_))
    do i = 1, numbit_
      dn = 256/i - 1
      if (rgb=="b") then
        call kmlSetCustomColor(colormap(i), "EE"//str(dn, "x")//"00"//"00")
      elseif (rgb=="g") then
        call kmlSetCustomColor(colormap(i), "EE"//"00"//str(dn, "x")//"00")
      elseif (rgb=="r") then
        call kmlSetCustomColor(colormap(i), "EE"//"00"//"00"//str(dn, "x"))
      endif
    enddo

    call kmlCreateCells(xf, east=east, west=west, south=south, north=north, &
      values=reflectance, mask=1.0d0, colormap=colormap)

  end subroutine kmlCreateRGBCells_dp

! createCells was called createCells2/createCells3
! Its function is to produce coloured cells, with the colour varying
! according to value.
! value must be passed in as a 2D array.
! x/y coords may be specified either by:
! 1: simple EWSN coords to specify the corners of the array
!     ie, a very simple rectangular spaced grid with equal spacing of points.
! 2: two 1-D arrays, one of longitude, one of latitude (of the same lengths as value(:,:))
!     ie a rectangular grid, but with irregular spacing
! 3: two 2-D arrays, one for longitude, one for latitude (both of the same size as value(:,:))
!     ie a topologically rectilinear, but otherwise irregular grid.

! FIXME in the simplest case (1: above) we should make sure and document where the EWNS are
! expected to be in relationship to the grid points. Currently we assume coincident.

TOHW_m4_wkml_spdp_createCells(`kmlCreateCells', `east, west, south, north', `east, west, south, north', `'`dnl
    lat_inc = (east-west)/m ! Increment in latitude
    long_inc = (north-south)/n ! Increment in longitude
    do i = 1, m
      long = west+long_inc*(i-0.5) ! Subtract 0.5 so that cells are *centred* on the long/lat point.
      do j = 1, n
        lat = south+lat_inc*(i-0.5)
        if (values(i,j)>=mask) cycle
        square(1, :) = (/long, long+long_inc, long+long_inc, long/) ! x-coords
        square(2, :) = (/lat, lat, lat+lat_inc, lat+lat_inc/)       ! y-coords
        if (present(height)) then                                           ! z-coords
          square(3,:) = height*((/values(i,j), values(i+1,j), values(i+1,j+1), values(i+1,j+1)/)-minValue)
        endif
        if (present(contour_values)) then
          if (values(i,j)<contour_values(1)) then
            k = 0
          else
            do ic = 1, size(contour_values)
              if (values(i,j)>contour_values(i)) then
                k = i
                exit
              endif
            enddo
          endif
        else
          k = int(floor((values(i, j)-minvalue)/valueres))
        endif
        if (present(colormap)) then
          thisColor => colormap(k+1)
        else
          thisColor => defaultMap(k+1)
        endif
        if (present(colormap)) then
          if (present(height)) then
            call kmlStartRegion(xf, square, &
              extrude=.true., altitudeMode="relativeToGround", fillcolor=colorMap(k+1))
          else
            call kmlStartRegion(xf, square(:2,:), fillcolor=colorMap(k+1))
          endif
        else
          if (present(height)) then
            call kmlStartRegion(xf, square, &
              extrude=.true., altitudeMode="relativeToGround", fillcolor=defaultMap(k+1))
          else
            call kmlStartRegion(xf, square(:2,:), fillcolor=defaultMap(k+1))
          endif
        endif
        call kmlEndRegion(xf)
      end do
    end do
')`'


TOHW_m4_wkml_spdp_createCells(`kmlCreateCells_longlat', `longitude, latitude', `longitude(:), latitude(:)', `'`dnl
    do i = 1, m-1
      do j = 1, n-1
        if (any(values(i:i+1, j:j+1)>=mask)) cycle ! Dont draw the cell if any of its vertices are masked out
        square(1, :) = (/longitude(i), longitude(i+1), longitude(i+1), longitude(i)/) ! x-coords
        square(2, :) = (/latitude(j), latitude(j), latitude(j+1), latitude(j+1)/)     ! y-coords
        if (present(height)) then                                           ! z-coords
          square(3,:) = height*((/values(i,j), values(i+1,j), values(i+1,j+1), values(i+1,j+1)/)-minValue)
        endif
        average = sum(values(i:i+1,j:j+1))/4.0d0
        ! Colour the cell according to the average of the 4 values defining the cell.
        if (present(contour_values)) then
          if (average<contour_values(1)) then
            k = 0
          else
            do ic = 1, size(contour_values)
              if (average>contour_values(i)) then
                k = i
                exit
              endif
            enddo
          endif
        else
          k = int(floor((average-minvalue)/valueres))
        endif
        if (present(colormap)) then
          thisColor => colormap(k+1)
        else
          thisColor => defaultMap(k+1)
        endif
        if (present(colormap)) then
          if (present(height)) then
            call kmlStartRegion(xf, square, &
              extrude=.true., altitudeMode="relativeToGround", fillcolor=colorMap(k+1))
          else
            call kmlStartRegion(xf, square(:2,:), fillcolor=colorMap(k+1))
          endif
        else
          if (present(height)) then
            call kmlStartRegion(xf, square, &
              extrude=.true., altitudeMode="relativeToGround", fillcolor=defaultMap(k+1))
          else
            call kmlStartRegion(xf, square(:2,:), fillcolor=defaultMap(k+1))
          endif
        endif
        call kmlEndRegion(xf)
      end do
    end do
')`'

TOHW_m4_wkml_spdp_createCells(`kmlCreateCells_longlat2', `longitude, latitude', `longitude(:,:), latitude(:,:)', `'`dnl
    do i = 1, m-1
      do j = 1, n-1
        if (any(values(i:i+1, j:j+1)>=mask)) cycle ! Dont draw the cell if any of its vertices are masked out
        square(1, :) = (/longitude(i,j), longitude(i+1,j), longitude(i+1,j+1), longitude(i,j+1)/) ! x-coords
        square(2, :) = (/latitude(i,j), latitude(i+1,j), latitude(i+1,j+1), latitude(i,j+1)/)     ! y-coords
        if (present(height)) then                                                                         ! z-coords
          square(3,:) = height*((/values(i,j), values(i+1,j), values(i+1,j+1), values(i+1,j+1)/)-minValue)
        endif
        average = sum(values(i:i+1,j:j+1))/4.0d0
        ! Colour the cell according to the average of the 4 values defining the cell.
        if (present(contour_values)) then
          if (average<contour_values(1)) then
            k = 0
          else
            do ic = 1, size(contour_values)
              if (average>contour_values(i)) then
                k = i
                exit
              endif
            enddo
          endif
        else
          k = int(floor((average-minvalue)/valueres))
        endif
        if (present(colormap)) then
          if (present(height)) then
            call kmlStartRegion(xf, square, &
              extrude=.true., altitudeMode="relativeToGround", fillcolor=colorMap(k+1))
          else
            call kmlStartRegion(xf, square(:2,:), fillcolor=colorMap(k+1))
          endif
        else
          if (present(height)) then
            call kmlStartRegion(xf, square, &
              extrude=.true., altitudeMode="relativeToGround", fillcolor=defaultMap(k+1))
          else
            call kmlStartRegion(xf, square(:2,:), fillcolor=defaultMap(k+1))
          endif
        endif
        call kmlEndRegion(xf)
      end do
    end do
')`'

end module m_wkml_coverage
