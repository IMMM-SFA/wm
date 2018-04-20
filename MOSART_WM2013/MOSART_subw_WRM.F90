program MOSART_subw_WRM
! Description: main program for MOSART
! 
! Developed by Hongyi Li, 12/29/2011
! REVISION HISTORY:
!	Nathalie Voisin 2/2012 implementation of the WMR model within MOSART
!-----------------------------------------------------------------------

! !USES:
	use shr_kind_mod  , only : r8 => shr_kind_r8, SHR_KIND_CL
	use shr_const_mod , only : SHR_CONST_REARTH, SHR_CONST_PI
	use MOSART_type_mod, only : ctlSubw, TUnit, liqWater, para
	use MOSART_physics_mod
	use MOSART_subw_IO_mod
	use WRM_type_mod, only : ctlSubwWRM, WRMUnit, StorWater
	use WRM_subw_IO_mod
        use WRM_start_op_year
 	use WRM_modules

	implicit none
	!include 'netcdf.inc'

	integer iargc, numarg                      ! for reading into arguments from command line
	integer iout, istation, m, j               ! local indices
	integer :: ios, ierror                             ! flag for status
	character(len=20) :: theTime               ! tempory string
	character(len=200) :: timeFileName         ! name of the time file
	character(len=200) :: flowFileName         ! name of the output file
	character(len=200) :: supplyFileName       ! name of output file for water supply	
        character(len=4) :: year                   ! temporary string for the year
        character(len=2) :: month                  ! temporary string for the month
        character(len=2) :: day                  ! temporary string for the day
        integer :: d, idam                           !day in number
!while US`
!        character(len=20*126899) :: strLine
         character(len=20*20000) :: strLine
         character(len=20) :: stemp
         real(r8) :: sfloat
         integer iunit


	numarg = iargc ( )
	if(numarg.NE.2) then
	   PRINT*, 'Usage:  MOSART_WRM <basename> <control_file>'
	   STOP
	end if
	call getarg(1,ctlSubw%baseName)  
	call getarg(2,ctlSubw%ctlFile)  
 
	! read into model inputs  
	!! start the control file
	call read_ctlFile
	print*, "Start simulation for ",trim(ctlSubw%basename) 
	print*, "Initializing..."
	call MOSART_init
        if ( ctlSubwWRM%WRMFlag > 0 ) then
	  call WRM_init
        endif

       print*, "Initialization done"
	! open output file
	if(ctlSubw%RoutingMethod == 1) then
		flowFileName = '.KW'
	else if(ctlSubw%RoutingMethod == 2) then
		flowFileName = '.MC'
	else if(ctlSubw%RoutingMethod == 3) then
		flowFileName = '.THREW'
	else if(ctlSubw%RoutingMethod == 4) then
		flowFileName = '.DW'
	end if

	flowFileName = adjustl(trim(ctlSubw%outPath))//trim(ctlSubw%basename)//adjustl(trim(flowFileName))//'.flow'
	call createFile(11,flowFileName)
	flowFileName = adjustl(trim(ctlSubw%outPath))//trim(ctlSubw%basename)//'.test'
	call createFile(110,flowFileName)
	if ( ctlSubwWRM%WRMFlag > 0 ) then
           flowFileName = adjustl(trim(ctlSubwWRM%outPath))//trim(ctlSubw%basename)//'.supply'
          call createFile(21, flowFileName)
          print*, flowFileName
          flowFileName = adjustl(trim(ctlSubwWRM%outPath))//trim(ctlSubw%basename)//'.newdemand'
          call createFile(22, flowFileName)
          flowFileName = adjustl(trim(ctlSubwWRM%outPath))//trim(ctlSubw%basename)//'.demand'
          call createFile(23, flowFileName)
          flowFileName = adjustl(trim(ctlSubwWRM%outPath))//trim(ctlSubw%basename)//'.storage'
          call createFile(24, flowFileName)
        endif
        flowFileName = adjustl(trim(ctlSubw%outPath))//trim(ctlSubw%basename)//'.whwt'
        !call createFile(25, flowFileName)
        flowFileName = adjustl(trim(ctlSubw%outPath))//trim(ctlSubw%basename)//'.wr'
        !call createFile(26, flowFileName)
        flowFileName = adjustl(trim(ctlSubw%outPath))//trim(ctlSubw%basename)//'.erlateral'
        !call createFile(27, flowFileName)


	! start the routing from a certain time step, and pass over the previous ones
	timeFileName = trim(ctlSubw%paraPath)//trim(ctlSubw%baseName)//'.time'
	  open (unit=12, file=timeFileName, status="old", action="read", iostat=ios)
	if(ctlSubw%NSTART > 1) then
		do j=1,ctlSubw%NSTART-1
		    read(12,*) theTime
	    end do
	end if
	print*, "Running..."
	do iout=1,ctlSubw%NSTEPS
!	read(12,'(a8)') theTime
  		call readline(12,theTime, ios)
             year = theTime(1:4)
             month = theTime(5:6)
             day = theTime(7:8)
             call str2num(year, ctlSubwWRM%year,ierror) 
             call str2num(month, ctlSubwWRM%month,ierror)
             print*, "running ", ctlSubwWRM%year, ctlSubwWRM%month
	    call readRunoff(trim(theTime))
	    if ( (ctlSubwWRM%WRMFlag > 0) ) then
              !call readPotentialEvap(trim(theTime))
              call readIrrigDemand(trim(theTime))
              call printRemainDemand(theTime, 20*ctlSubw%NUnit, 23)

              flowFileName = adjustl(trim(ctlSubwWRM%outPath))//trim(ctlSubw%basename)//'.demand'
              if(mod(iout,1000) == 0) then ! here use a small trick to flush the output file
                   print*, iout, flowFileName
                   close(unit=23)
                   OPEN(UNIT=23, FILE=flowFileName, POSITION="APPEND",STATUS="OLD", iostat=ios)
              end if


              StorWater%supply = 0._r8
              StorWater%deficit=0._r8
              call str2num(day,d,ierror)
              if ( d .eq. 1 .and. ctlSubwWRM%WRMFlag > 0) then
                do idam=1,ctlSubwWRM%NDam
                  if ( ctlSubwWRM%month .eq. WRMUnit%MthStOp(idam)) then
                    WRMUnit%StorMthStOp(idam) = StorWater%Storage(idam)
                  end if
                enddo
                call RegulationRelease
                call WRM_storage_targets
              end if
              
            end if
            !print*, "before Euler", StorWater%demand(49)
            if ( ctlSubwWRM%WRMFlag > 0 ) then
              do m=1,ctlSubw%num_dt
                   call Euler_WRM
              end do
            else
              do m=1,ctlSubw%num_dt
                 call Euler
              end do
            end if
	    !do m=1,ctlSubw%num_dt
!			  !call Euler
!               if ( ctlSubwWRM%WRMFlag > 0 ) then
!                 call Euler_WRM
!               else  
!                 call Euler
!               end if
!			  !call printTest(theTime, 45, 110)
!            end do

	    call printFlow(theTime, 20*ctlSubw%numStation, 11)

        !for flush
        if(ctlSubw%RoutingMethod == 1) then
                flowFileName = '.KW'
        else if(ctlSubw%RoutingMethod == 2) then
                flowFileName = '.MC'
        else if(ctlSubw%RoutingMethod == 3) then
                flowFileName = '.THREW'
        else if(ctlSubw%RoutingMethod == 4) then
                flowFileName = '.DW'
        end if

            flowFileName = adjustl(trim(ctlSubw%outPath))//trim(ctlSubw%basename)//adjustl(trim(flowFileName))//'.flow'
            if(mod(iout,100) == 0) then ! here use a small trick to flush the output file
                   print*, iout, flowFileName
                   close(unit=11)
                   OPEN(UNIT=11, FILE=flowFileName, POSITION="APPEND",STATUS="OLD", iostat=ios)
            end if

	    !call printTest(theTime, 2, 110)
            if ( ctlSubwWRM%WRMFlag > 0 .and. ctlSubwWRM%ExtractionFlag > 0) then
	      call printIrrigSupply(theTime, 20*ctlSubw%NUnit, 21)
              flowFileName = adjustl(trim(ctlSubwWRM%outPath))//trim(ctlSubw%basename)//'.supply'
              if(mod(iout,100) == 0) then ! here use a small trick to flush the output file
                   print*, iout, flowFileName
                   close(unit=21)
                   OPEN(UNIT=21, FILE=flowFileName, POSITION="APPEND",STATUS="OLD", iostat=ios)
              end if

              call printRemainDemand(theTime, 20*ctlSubw%NUnit, 22)

              flowFileName = adjustl(trim(ctlSubwWRM%outPath))//trim(ctlSubw%basename)//'.newdemand'
              if(mod(iout,100) == 0) then ! here use a small trick to flush the output file
                   print*, iout, flowFileName
                   close(unit=22)
                   OPEN(UNIT=22, FILE=flowFileName, POSITION="APPEND",STATUS="OLD", iostat=ios)
              end if

              StorWater%deficit = StorWater%demand
              !call printIrrigSupplyTest(theTime, 20*ctlSubw%numStation, 21)
            end if
            if ( ctlSubwWRM%WRMFlag > 0 .and. ctlSubwWRM%RegulationFlag > 0) then
              call printStorage(theTime, 20*ctlSubwWRM%NDam, 24)

              flowFileName = adjustl(trim(ctlSubwWRM%outPath))//trim(ctlSubw%basename)//'.storage'
              if(mod(iout,100) == 0) then ! here use a small trick to flush the output file
                   print*, iout, flowFileName
                   close(unit=24)
                   OPEN(UNIT=24, FILE=flowFileName, POSITION="APPEND",STATUS="OLD", iostat=ios)
              end if
            end if

             ! print additional output files
            ! strLine = ''
            ! do iunit=1, ctlSubw%NUnit
            !      stemp = ''
            !      sfloat = liqWater%wh(iunit) + liqWater%wt(iunit)
            !      call num2str(sfloat, stemp, 'e20.10')
            !      strLine = trim(strLine)//adjustr(stemp)
            !end do
            !write(unit=25,fmt="((a10), (a))") trim(theTime), strLine

            !strLine = ''
            ! do iunit=1, ctlSubw%NUnit
            !      stemp = ''
            !      sfloat = liqWater%wr(iunit)
            !      call num2str(sfloat, stemp, 'e20.10')
            !      strLine = trim(strLine)//adjustr(stemp)
            !end do
            !write(unit=26,fmt="((a10), (a))") trim(theTime), strLine

            !strLine = ''
            ! do iunit=1, ctlSubw%NUnit
            !      stemp = ''
            !      sfloat = liqWater%erlateral(iunit)
            !      call num2str(sfloat, stemp, 'e20.10')
            !      strLine = trim(strLine)//adjustr(stemp)
            !end do
            !write(unit=27,fmt="((a10), (a))") trim(theTime), strLine

!	    if(mod(iout,1000) == 0) then ! here use a small trick to flush the output file
!                    print*, iout
!                        close(unit=11)
!                        OPEN(UNIT=11, FILE=flowFileName, POSITION="APPEND",STATUS="OLD", iostat=ios)
!	    end if
	end do

	close(unit=11)
	close(unit=12)
	close(unit=110)
	close(unit=21)
	close(unit=22)
	close(unit=23)
	close(unit=24)
       ! close(unit=25)
       ! close(unit=26)
       ! close(unit=27)
	print*, "Completed! "

end program MOSART_subw_WRM  
