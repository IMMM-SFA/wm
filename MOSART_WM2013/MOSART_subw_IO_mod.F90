!
MODULE MOSART_subw_IO_mod
! Description: module to provide interface between MOSART and other CLM components
! 
! Developed by Hongyi Li, 12/29/2011
! REVISION HISTORY:
!	March 2012, Nathalie Voisin	modify the reading of the control file 
!					for WRM info - use updated MOSART code	
!-----------------------------------------------------------------------

! !USES:
  use shr_kind_mod  , only : r8 => shr_kind_r8, SHR_KIND_CL
  use shr_const_mod , only : SHR_CONST_REARTH, SHR_CONST_PI
  use MOSART_type_mod, only : ctlSubw, TUnit,liqWater, para
  use WRM_type_mod, only : ctlSubwWRM, WRMUnit, StorWater
  use MOSART_physics_mod
  use strings
  
  implicit none

  contains
  
  subroutine read_ctlFile
  ! !DESCRIPTION: read the control file
  
	  implicit none
	  integer :: nn, ierror      ! nn is tempory integer, ierror is for IO status flag
      character(len=10000) :: stmp, stmp1, stmp2   ! tempory string
      
      open (unit=1, file=ctlSubw%ctlFile, status="old", action="read", iostat=ierror)
      if(ierror /= 0) then
          print*, "Failed to open " // ctlSubw%ctlFile
	      stop
      end if
	  ctlSubw%NUnit = 0
	  stmp = trim(read_ctlLine(1)) ! 1
	  call str2num(stmp,ctlSubw%NUnit,ierror) 
      ctlSubw%DATAH = 0._r8
	  stmp = trim(read_ctlLine(1)) ! 2
	  call str2num(stmp,ctlSubw%DATAH,ierror) 
      ctlSubw%num_dt = 0
	  stmp = trim(read_ctlLine(1)) ! 3
	  call str2num(stmp,ctlSubw%num_dt,ierror) 
        print*, "reading ctl file nnum dt", ctlSubw%num_dt
      ctlSubw%deltaT = 0._r8
	  ctlSubw%deltaT = ctlSubw%DATAH / ctlSubw%num_dt
	  ctlSubw%DLevelH2R = 0
	  stmp = trim(read_ctlLine(1)) ! 4
	  call str2num(stmp,ctlSubw%DLevelH2R,ierror)
	  ctlSubw%DLevelR = 0
	  stmp = trim(read_ctlLine(1)) ! 5
	  call str2num(stmp,ctlSubw%DLevelR,ierror)
      ctlSubw%NSTART = 0
	  stmp = trim(read_ctlLine(1)) ! 6
	  call str2num(stmp,ctlSubw%NSTART,ierror) 
      ctlSubw%NSTEPS = 0
	  stmp = trim(read_ctlLine(1)) ! 7
	  call str2num(stmp,ctlSubw%NSTEPS,ierror) 
      ctlSubw%Restart = 0
	  stmp = trim(read_ctlLine(1)) ! 8
	  call str2num(stmp,ctlSubw%Restart,ierror) 
      ctlSubw%RoutingMethod = 0
	  stmp = trim(read_ctlLine(1)) ! 9
	  call str2num(stmp,ctlSubw%RoutingMethod,ierror) 
	  stmp = trim(read_ctlLine(1)) ! 10
	  call str2num(stmp,ctlSubw%RoutingFlag,ierror)
	  ctlSubw%paraPath = trim(read_ctlLine(1)) ! 11
	  ctlSubw%runoffPath = trim(read_ctlLine(1)) ! 12
	  ctlSubw%outPath = trim(read_ctlLine(1)) ! 13
      ctlSubw%numStation = 0
	  stmp = trim(read_ctlLine(1)) ! 14
	  call str2num(stmp,ctlSubw%numStation,ierror) 
	  ctlSubw%staListFile = trim(read_ctlLine(1)) ! 15

! WRM data now
          ctlSubwWRM%WRMFlag = 0
          stmp = trim(read_ctlLine(1)) ! 16
          call str2num(stmp,ctlSubwWRM%WRMFlag,ierror)
          stmp = trim(read_ctlLine(1)) ! 17
          call str2num(stmp,ctlSubwWRM%ExtractionFlag,ierror)
          stmp = trim(read_ctlLine(1)) ! 18
          call str2num(stmp,ctlSubwWRM%RegulationFlag,ierror)
          stmp = trim(read_ctlLine(1)) ! 19
          call str2num(stmp,ctlSubwWRM%ExtractionMainChannelFlag,ierror)
          stmp = trim(read_ctlLine(1)) ! 20
          call str2num(stmp,ctlSubwWRM%NDam,ierror)
          ctlSubwWRM%paraPath = trim(read_ctlLine(1)) ! 21
          ctlSubwWRM%demandPath = trim(read_ctlLine(1)) ! 22
          ctlSubwWRM%outPath = trim(read_ctlLine(1)) ! 23
	  close(unit=1)
	  
	  !print*, ctlSubw%ncols, ctlSubw%nrows, ctlSubw%xllcorner, ctlSubw%yllcorner, ctlSubw%maskPath

  end subroutine read_ctlFile
 
  subroutine read_paraFile
  ! !DESCRIPTION: read the parameter file
	  implicit none
	  integer :: ng, nio, ierror      ! ng is local indices, nio is file unit number, ierror is IO status flag
	  real(r8) :: rtemp1, rtemp2, rtemp3         ! tempory float number
      character(len=10000) :: stmp    ! tempory string
	  
	  allocate (para%c_nr(ctlSubw%NUnit), para%c_nh(ctlSubw%NUnit), para%c_twid(ctlSubw%NUnit))
	  para%c_nr = 0._r8
	  para%c_nh = 0._r8
	  para%c_twid = 0._r8

	  nio = 1
      open (unit=nio, file=ctlSubw%paraFile, status="old", action="read", iostat=ierror)
      if(ierror /= 0) then
          print*, "Failed to open " // ctlSubw%paraFile
	      stop
      end if
	  rtemp1 = 0._r8
	  rtemp2 = 0._r8
	  rtemp3 = 0._r8
	  stmp = read_ctlLine(nio)
	  read(stmp, '(f8.2)' ) rtemp1
	  stmp = read_ctlLine(nio)
	  read(stmp, '(f8.2)' ) rtemp2
	  stmp = read_ctlLine(nio)
	  read(stmp, '(f8.2)' ) rtemp3
	  do ng=1, ctlSubw%NUnit
	      para%c_nr(ng) = rtemp1
	      para%c_nh(ng) = rtemp2
	      para%c_twid(ng) = rtemp3
	  end do
	  
      close(unit=nio)	  
  end subroutine read_paraFile

  subroutine MOSART_init
      ! !DESCRIPTION: initilization of MOSART model
	  implicit none

      integer nn, iunit, j               ! local loop indices
	  integer nio,ierror                 ! unit number of a file, flag number of IO status
	  real(r8), pointer  :: ftemp(:)     ! tempory float array
	  character(len=1000) :: stemp, stmp1! tempory string
	  character(len=4)   :: delims       ! delimiter
	  
	  ! parameters
      ctlSubw%paraFile = trim(ctlSubw%paraPath)//trim(ctlSubw%baseName)//'.para'
	  call read_paraFile
          print*," read MOSART parameters ..."
	  ! landscape properties
	  nn = ctlSubw%NUnit
	  allocate (TUnit%frac(nn))
	  TUnit%frac = 1._r8    ! for grids, this values maybe be less than 1.0 if at basin boundary. for subbasins, this values always is 1.0
	  allocate (TUnit%fdir(nn))
	  TUnit%fdir = 1  ! 1 for subbasin-based representation,flow direction is not used. Assigning 1 here to let model go through
	  allocate (TUnit%mask(nn,1))
	  TUnit%mask = 1        ! assuming all spatial units are active land surface, TO DO

          allocate (TUnit%icell(nn,1))
          !allocate (TUnit%icell(nn,1), STAT = ierror )
          !if ( ierror /= 0 ) STOP "*** ERROR ALLOCATING ***"
	  TUnit%icell = 0
	  allocate (TUnit%area(nn))
	  TUnit%area = 0._r8
	  allocate (TUnit%areaTotal(nn))
	  TUnit%areaTotal = 0._r8
	  allocate (TUnit%rlenTotal(nn))
	  TUnit%rlenTotal = 0._r8
	  allocate (TUnit%nh(nn))
	  TUnit%nh = 0._r8
	  allocate (TUnit%hslp(nn))
	  TUnit%hslp = 0._r8
	  allocate (TUnit%Gxr(nn))
	  TUnit%Gxr = 0._r8
	  allocate (TUnit%hlen(nn))
	  TUnit%hlen = 0._r8
	  allocate (TUnit%tslp(nn))
	  TUnit%tslp = 0._r8
	  allocate (TUnit%tlen(nn))
	  TUnit%tlen = 0._r8
	  allocate (TUnit%twidth(nn))
	  TUnit%twidth = 0._r8
	  allocate (TUnit%nt(nn))
	  TUnit%nt = 0._r8
	  allocate (TUnit%rlen(nn))
	  TUnit%rlen = 0._r8
	  allocate (TUnit%rslp(nn))
	  TUnit%rslp = 0._r8
	  allocate (TUnit%rwidth(nn))
	  TUnit%rwidth = 0._r8
	  allocate (TUnit%rwidth0(nn))
	  TUnit%rwidth0 = 0._r8
	  allocate (TUnit%rdepth(nn))
	  TUnit%rdepth = 0._r8
	  allocate (TUnit%nr(nn))
	  TUnit%nr = 0._r8
	  allocate (TUnit%iDown(nn))
	  TUnit%iDown = 0
	  allocate (TUnit%nUp(nn))
	  TUnit%nUp = 0
	  allocate (TUnit%iUp(nn,8)) 
	  TUnit%iUp = 0
	  allocate (TUnit%indexDown(nn))
	  TUnit%indexDown = 0

	  ! water states and fluxes
	  allocate (liqWater%wh(nn))
	  liqWater%wh = 0._r8
	  allocate (liqWater%dwh(nn))
	  liqWater%dwh = 0._r8
	  allocate (liqWater%yh(nn))
	  liqWater%yh = 0._r8
	  allocate (liqWater%qsur(nn))
	  liqWater%qsur = 0._r8
	  allocate (liqWater%qsub(nn))
	  liqWater%qsub = 0._r8
	  allocate (liqWater%ehout(nn))
	  liqWater%ehout = 0._r8
	  allocate (liqWater%tarea(nn))
	  liqWater%tarea = 0._r8
	  allocate (liqWater%wt(nn))
	  liqWater%wt= 0._r8
	  allocate (liqWater%dwt(nn))
	  liqWater%dwt = 0._r8
	  allocate (liqWater%yt(nn))
	  liqWater%yt = 0._r8
	  allocate (liqWater%mt(nn))
	  liqWater%mt = 0._r8
	  allocate (liqWater%rt(nn))
	  liqWater%rt = 0._r8
	  allocate (liqWater%pt(nn))
	  liqWater%pt = 0._r8
	  allocate (liqWater%vt(nn))
	  liqWater%vt = 0._r8
	  allocate (liqWater%tt(nn))
	  liqWater%tt = 0._r8
	  allocate (liqWater%etin(nn))
	  liqWater%etin = 0._r8
	  allocate (liqWater%etout(nn))
	  liqWater%etout = 0._r8
	  allocate (liqWater%rarea(nn))
	  liqWater%rarea = 0._r8
	  allocate (liqWater%wr(nn))
	  liqWater%wr = 0._r8
	  allocate (liqWater%dwr(nn))
	  liqWater%dwr = 0._r8
	  allocate (liqWater%yr(nn))
	  liqWater%yr = 0._r8
	  allocate (liqWater%mr(nn))
	  liqWater%mr = 0._r8
	  allocate (liqWater%rr(nn))
	  liqWater%rr = 0._r8
	  allocate (liqWater%pr(nn))
	  liqWater%pr = 0._r8
	  allocate (liqWater%vr(nn))
	  liqWater%vr = 0._r8
	  allocate (liqWater%tr(nn))
	  liqWater%tr = 0._r8
	  allocate (liqWater%erlg(nn))
	  liqWater%erlg = 0._r8
	  allocate (liqWater%erlateral(nn))
	  liqWater%erlateral = 0._r8
	  allocate (liqWater%erin(nn))
	  liqWater%erin = 0._r8
	  allocate (liqWater%erout(nn))
	  liqWater%erout = 0._r8
	  allocate (liqWater%flow(nn))
          !allocate (liqwater%erout(nn), STAT = ierror )
          !if ( ierror /= 0 ) STOP "*** ERROR ALLOCATING ***"
	  liqWater%flow = 0._r8

      
	  delims = ' '
	  ! reading directly available inputs variables
          print*, "read .rnet ...."
	  stemp = trim(ctlSubw%paraPath)//trim(ctlSubw%baseName)//'.rnet'
	  nio = 1
      open (unit=nio, file=stemp, status="old", action="read", iostat=ierror)
      if(ierror /= 0) then
          print*, "Failed to open " // stemp
	      stop
      end if
	  call readline(nio, stemp, ierror)
	  call readline(nio, stemp, ierror)
      do iunit=1,ctlSubw%NUnit
	      stemp = ''
		  call readline(nio, stemp, ierror)
		  call compact(stemp)
		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%icell(iunit,1),ierror)    !1
		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%area(iunit),ierror)     !2
		  !TUnit%area(iunit) = TUnit%area(iunit) * 1e4_r8   ! ha-->m2, TO DO
                  !NV
		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%areaTotal(iunit),ierror)!3
		  !TUnit%areaTotal(iunit) = TUnit%areaTotal(iunit) * 1e4_r8   ! ha-->m2, TO DO
                  !NV
		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%hslp(iunit),ierror)     !4
                  if ( TUnit%hslp(iunit) < 0._r8) then
                    TUnit%hslp(iunit) = 0.0001
                  endif

		  !TUnit%hslp(iunit) = TUnit%hslp(iunit) * 1e-2_r8   ! % to decimal, TO DO
                  !NV
 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%tslp(iunit),ierror)     !5
                  if ( TUnit%tslp(iunit) < 0._r8) then
                    TUnit%tslp(iunit) = 0.0001
                  endif
		  !TUnit%tslp(iunit) = TUnit%tslp(iunit) * 1e-2_r8   ! % to decimal, TO DO
                  !NV
 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%twidth(iunit),ierror)   !6
 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%rlen(iunit),ierror)     !7
                  if ( TUnit%rlen(iunit) < 0._r8) then
                    TUnit%rlen(iunit) = 0._r8
                  endif

 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%rslp(iunit),ierror)     !8
		  !TUnit%rslp(iunit) = TUnit%rslp(iunit) * 1e-2_r8   ! % to decimal, TO DO
                  !NV
                  if ( TUnit%rslp(iunit) < 0._r8) then
                    TUnit%rslp(iunit) = 0.0001
                  endif
 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%rwidth(iunit),ierror)  !9
                  if ( TUnit%rwidth(iunit) < 0._r8) then
                    TUnit%rwidth(iunit) = 0._r8
                  endif
 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%rdepth(iunit),ierror) !10
                  if ( TUnit%rdepth(iunit) < 0._r8) then
                    TUnit%rdepth(iunit) = 0._r8
                  endif
 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%rwidth0(iunit),ierror)!11
 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%Gxr(iunit),ierror)    !12
		  !TUnit%Gxr(iunit) = TUnit%Gxr(iunit) * 1e-3_r8   ! 1/km-->1/m, TO DO
                  !NV
 		  call split(stemp,' ',stmp1)
 		  call split(stemp,' ',stmp1)
 		  call split(stemp,' ',stmp1)
 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%iDown(iunit),ierror)  !16
 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%nr(iunit),ierror)     !17
 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%nt(iunit),ierror)     !18
 		  call split(stemp,' ',stmp1)
          call str2num(stmp1, TUnit%nh(iunit),ierror)     !19

      !added warning for non active cell
      if (TUnit%rwidth(iunit)< 0._r8 .or. TUnit%rdepth(iunit)< 0._r8 .or. TUnit%rlen(iunit)< 0._r8 .or. TUnit%area(iunit)< 0._r8) then
         TUnit%fdir(iunit) = -1
         TUnit%rwidth(iunit) = 0._r8
         TUnit%rlen(iunit) = 0._r8
         TUnit%rdepth(iunit) = 0._r8
         TUnit%rslp(iunit) = 0._r8
      end if
      end do	  
	  close(unit=nio)

	  ! estimate derived input variables
          print*, "estimate derived input variables "
	  TUnit%tslp = TUnit%rslp      ! TO DO
	  do iunit=1,ctlSubw%NUnit
		  if(TUnit%Gxr(iunit) > 0._r8 .and. TUnit%area(iunit) >= 0._r8) then
		      TUnit%rlenTotal(iunit) = TUnit%area(iunit)*TUnit%Gxr(iunit)
		  end if
	  end do

	  do iunit=1,ctlSubw%NUnit
		  if(TUnit%rlen(iunit) > TUnit%rlenTotal(iunit)) then
		      TUnit%rlenTotal(iunit) = TUnit%rlen(iunit)
		  end if
          end do 	  

	  do iunit=1,ctlSubw%NUnit
        
		  if(TUnit%rlen(iunit) > 0._r8) then
		      TUnit%hlen(iunit) = TUnit%area(iunit) / TUnit%rlenTotal(iunit) / 2._r8
			  if(TUnit%hlen(iunit) > 50000_r8) then
			      TUnit%hlen(iunit) = 50000_r8   ! allievate the outlier in drainage density estimation. TO DO
			  end if
		      TUnit%tlen(iunit) = TUnit%area(iunit) / TUnit%rlen(iunit) / 2._r8 - TUnit%hlen(iunit)
			  if(TUnit%twidth(iunit) < 0._r8) then
			      TUnit%twidth(iunit) = 0._r8
			  end if
			  if(TUnit%tlen(iunit) > 0._r8 .and. (TUnit%rlenTotal(iunit)-TUnit%rlen(iunit))/TUnit%tlen(iunit) > 1._r8) then
			      TUnit%twidth(iunit) = para%c_twid(iunit)*TUnit%twidth(iunit)*((TUnit%rlenTotal(iunit)-TUnit%rlen(iunit))/TUnit%tlen(iunit))
			  end if
			  
			  if(TUnit%tlen(iunit) > 0._r8 .and. TUnit%twidth(iunit) <= 0._r8) then
			      TUnit%twidth(iunit) = 0._r8
			  end if
		  else
                      TUnit%rlen(iunit) = 0._r8
		      TUnit%hlen(iunit) = 0._r8
		      TUnit%tlen(iunit) = 0._r8
		      TUnit%twidth(iunit) = 0._r8
			  
		  end if
		  
		  if(TUnit%rslp(iunit) <= 0._r8) then
		      TUnit%rslp(iunit) = 0.0001_r8
		  end if
		  if(TUnit%tslp(iunit) <= 0._r8) then
		      TUnit%tslp(iunit) = 0.0001_r8
		  end if
		  if(TUnit%hslp(iunit) <= 0._r8) then
		      TUnit%hslp(iunit) = 0.005_r8
		  end if
      end do
	  
!Modified by NV first loop redundant
!	  do iunit=1,ctlSubw%NUnit
!          if(TUnit%iDown(nn) == iunit) then
!		      TUnit%indexDown(iunit) = iunit
!	  end if
!      end do
       do iunit=1,ctlSubw%NUnit
         do nn=1,ctlSubw%NUnit
           if(TUnit%iDown(iunit) == TUnit%icell(nn,1)) then
             TUnit%indexDown(iunit) = nn
! added NV
             TUnit%nUp(nn) = TUnit%nUp(nn) + 1
             ! issue is that delineation is not necessarily hydrologically consistent, the onew I did
             if (  TUnit%nUp(nn) > 8 ) then
               print*,"Error with rnet, NUnit, nUp,  nn, iunit", ctlSubw%NUnit, TUnit%nUp(nn), nn, iunit , TUnit%iDown(iunit)
               stop
             endif
             TUnit%iUp(nn,TUnit%nUp(nn)) = iunit
           end if
         end do
      end do
! modified by NV, redundant and dependent on the order of the grid cells
!      do iunit=1,ctlSubw%NUnit
!          do nn=1,ctlSubw%NUnit
!	      if(TUnit%iDown(iunit) == TUnit%icell(nn,1)) then
!		      TUnit%nUp(nn) = TUnit%nUp(nn) + 1
!			  TUnit%iUp(nn,TUnit%nUp(nn)) = iunit
!		  end if
!	  end do
!      end do

	  ! read the station info. Here station refers to the outlet of each basin.
      print*, "read the station info ..."
      allocate (ctlSubw%out_name(ctlSubw%numStation), ctlSubw%out_ID(ctlSubw%numStation))
      open (unit=2, file=ctlSubw%staListFile, status="old", action="read", iostat=ierror)
      if(ierror /= 0) then
          print*, "Failed to open "//trim(ctlSubw%staListFile)
	      stop
      end if
	  call readline(2,stemp,ierror)
	  do nn=1, ctlSubw%numStation
		  call readline(2,stemp,ierror)
		  call compact(stemp)
		  call split(stemp,' ',stmp1)
		  ctlSubw%out_name(nn) = stmp1
 	         call str2num(stemp,j,ierror)
	      ! in the outlist file, only the subbasin ID is required for each outlet
	      ! here convert it to the index used in the loop which some times may not be consistent with subbasin IDs
		  do iunit=1,ctlSubw%NUnit
		      if(TUnit%icell(iunit,1) == j) then
			      ctlSubw%out_ID(nn) = iunit
		      end if
		  end do
		  
	  end do
	  close(unit=2)
	  
         print*, "prepare for numerical computation ..."
	  call SubTimestep ! prepare for numerical computation
      !print*, TUnit%frac(1), TUnit%area(1), TUnit%area(ctlSubw%NUnit)
  end subroutine MOSART_init

  subroutine upstreamSubw
  ! !DESCRIPTION: find out the upstream subbasins
	  implicit none

      integer nn, iunit, j               ! local loop indices
	  integer nio,ierror                 ! unit number of a file, flag number of IO status
	  real(r8), pointer  :: ftemp(:)     ! tempory float array
	  character(len=1000) :: stemp, stmp1! tempory string
	  character(len=4)   :: delims       ! delimiter

	  stemp = adjustl(trim(ctlSubw%outPath))//trim(ctlSubw%basename)//'.upstreamSubbasin'
	  call createFile(1,stemp)
	  
	  do iunit=1,ctlSubw%NUnit
	      stemp = ''
	      stmp1 = ''
          call num2str(TUnit%icell(iunit,1),stmp1,'i8')
		  stemp = trim(adjustr(stmp1(1:8)))
		  stmp1 = ''
          call num2str(TUnit%nUp(iunit),stmp1,'i8')
          stemp = trim(stemp)//trim(adjustr(stmp1(1:8)))
		  do nn=1,TUnit%nUp(iunit)
			  stmp1 = ''
			  call num2str(TUnit%iUp(iunit,nn),stmp1,'i8')
              stemp = trim(stemp)//trim(adjustr(stmp1(1:8)))
		  end do
		  write(unit=1,fmt="((a))") stemp
      end do
	  close(unit=1)
  end subroutine upstreamSubw
  
  subroutine readRunoff(theTime)
  ! !DESCRIPTION: read in the runoff data for each time step
	  implicit none
      character(len=*), intent(in) :: theTime
      
      character(len=200) :: qsurFileName, qsubFileName  ! runoff file names
	  integer :: ios, iunit, ilat, ilon    ! flag of IO status, local indices
	  real(r8) :: ftemp1,ftemp2            ! tempory array
	  qsurFileName = adjustl(trim(ctlSubw%runoffPath))//'qsur/'//theTime//'.qsur'
	  open (unit=1, file=qsurFileName, status="old", action="read", iostat=ios)
	  if(ios /= 0) then
	      print*, "Cannot find file ", qsurFileName
		  stop
	  end if
	  qsubFileName = adjustl(trim(ctlSubw%runoffPath))//'qsub/'//theTime//'.qsub'
	  open (unit=2, file=qsubFileName, status="old", action="read", iostat=ios)
	  if(ios /= 0) then
	      print*, "Cannot find file ", qsubFileName
		  stop
	  end if
      iunit=0
	  do iunit=1, ctlSubw%NUnit
	      read(unit=1, *) liqWater%qsur(iunit)
	      read(unit=2, *) liqWater%qsub(iunit)
	  end do
	  close(unit=1)
	  close(unit=2)
	  liqWater%qsur = liqWater%qsur * 0.001_r8/ctlSubw%DATAH  !mm/day-->m/s, or mm/hr-->m/s, TO DO
	  liqWater%qsub = liqWater%qsub * 0.001_r8/ctlSubw%DATAH  !mm/day-->m/s, or mm/hr-->m/s, TO DO

	  !liqWater%qsur = 1.0e-4_r8  !mm/day-->m/s, TO DO
	  !liqWater%qsub = 0.0e-6_r8  !mm/day-->m/s, TO DO

  end subroutine readRunoff

  subroutine release_memory
  ! !DESCRIPTION: release the memories
	  implicit none
      !ctlSubw%maskFile = 
	  
  end subroutine release_memory
  
  function read_ctlLine(nio) result(varStr)
  ! !DESCRIPTION: read one line from the control file, and return the needed value in string format.
  ! !the structure of each line is : "description   variable"  
  ! !called from read_ctlFile
	  implicit none
      integer, intent(in) :: nio                 ! unit number
      character(len=100), intent(out) :: varStr  ! 

      integer :: ios                             ! status flags
      character(len=10000) :: stmp, stmp1, stmp2   ! tempory string
      
	  stmp = ''
	  stmp1 = ''
	  stmp2 = ''
      call readline(nio,stmp,ios)
	  if(ios /= 0) then
          print*, "cannot open the control file "
	      stop
	  end if
      call compact(stmp)
      call split(stmp,' ',stmp1) 
	  call compact(stmp)
      varStr = stmp
	  return
  end function read_ctlLine
  
  subroutine createFile(nio, fname)
      ! !DESCRIPTION: create a new file. if a file with the same name exists, delete it then create a new one
	  implicit none
	  character(len=*), intent(in) :: fname ! file name
      integer, intent(in) :: nio            !unit of the file to create
	  
	  integer :: ios
	  logical :: filefound
	  character(len=200) :: cmd
	  inquire (file=fname, exist=filefound)
	  if(filefound) then
	      cmd = 'rm '//fname
		  call system(cmd)
	  end if
	  open (unit=nio, file=fname, status="new", action="write", iostat=ios)
	  if(ios /= 0) then
	      print*, "cannot create file ", fname
	  end if
	  
  end subroutine createFile

  subroutine printFlow(theTime, strLen, nio)
      ! !DESCRIPTION: output the simulation results into external files
	  implicit none
      character(len=*), intent(in) :: theTime  ! the time step to output
	  integer, intent(in) :: strLen     ! length of each line to print
	  integer, intent(in) :: nio        ! unit of the file to print
	  
	  integer :: ios                    ! flag of io status
	  integer :: ista, theUnit          ! local index
	  character(len=strLen) :: strLine 
	  character(len=20) :: stemp 

	  strLine = ''
	  do ista=1, ctlSubw%numStation
	      theUnit = ctlSubw%out_ID(ista)
		  stemp = ''
		  call num2str(liqWater%flow(theUnit), stemp, 'e20.10')
	      strLine = trim(strLine)//adjustr(stemp)
	  end do
	  
	  write(unit=nio,fmt="((a10), (a))") theTime, strLine
  
  end subroutine printFlow

  

  subroutine printTest(theTime, theUnit, nio)
      ! !DESCRIPTION: output the simulation results into external files
	  implicit none
      character(len=*), intent(in) :: theTime  ! the time step to output
	  integer, intent(in) :: theUnit    ! the index of the unit to print 
	  integer, intent(in) :: nio        ! unit of the file to print
	  
	  integer :: ios,ii                    ! flag of io status
	  ii=theUnit !167 !191 !
	  !write(unit=nio,fmt="((a10),(e20.11))") theTime, liqWater%flow(ii)
	  write(unit=nio,fmt="((a10),6(e20.11))") theTime, liqWater%qsur(ii), liqWater%qsub(ii), liqWater%etin(ii)/(TUnit%area(ii)*TUnit%frac(ii)), liqWater%erlateral(ii), liqWater%erin(ii), liqWater%flow(ii)
	  !if(liqWater%yr(ii) > 0._r8) then
	  !    write(unit=nio,fmt="((a10),6(e20.11))") theTime, liqWater%mr(ii)/liqWater%yr(ii),liqWater%yr(ii), liqWater%vr(ii), liqWater%erin(ii), liqWater%erout(ii)/(TUnit%area(ii)*TUnit%frac(ii)), liqWater%flow(ii)
      !else
	  !    write(unit=nio,fmt="((a10),6(e20.11))") theTime, liqWater%mr(ii)-liqWater%mr(ii),liqWater%yr(ii), liqWater%vr(ii), liqWater%erin(ii), liqWater%erout(ii)/(TUnit%area(ii)*TUnit%frac(ii)), liqWater%flow(ii)
	  !end if
	  !write(unit=nio,fmt="((a10),7(e20.11))") theTime, liqWater%erlateral(ii)/(TUnit%area(ii)*TUnit%frac(ii)), liqWater%wr(ii),liqWater%mr(ii), liqWater%yr(ii), liqWater%pr(ii), liqWater%rr(ii), liqWater%flow(ii)
	  !write(unit=nio,fmt="((a10),7(e20.11))") theTime, liqWater%yh(ii), liqWater%dwh(ii),liqWater%etin(ii), liqWater%vr(ii), liqWater%erin(ii), liqWater%erout(ii)/(TUnit%area(ii)*TUnit%frac(ii)), liqWater%flow(ii)
  
  end subroutine printTest
   
end MODULE MOSART_subw_IO_mod
