!
MODULE WRM_subw_IO_mod
! Description: module to provide interface between WRM and other CLM components
! 
! Developed by Nathalie Voisin 2/1/2010
! REVISION HISTORY:
!-----------------------------------------------------------------------

! !USES:
  use shr_kind_mod  , only : r8 => shr_kind_r8, SHR_KIND_CL
  use shr_const_mod , only : SHR_CONST_REARTH, SHR_CONST_PI
  use MOSART_type_mod, only : ctlSubw, TUnit,liqWater, para
  use WRM_type_mod, only : ctlSubwWRM, WRMUnit, StorWater
  use WRM_start_op_year
  use strings
  
  implicit none

  contains
  
  subroutine WRM_init
      ! !DESCRIPTION: initilization of WRM model
	  implicit none

      integer nn,nd, iunit, j , mth, match, nsub, i              ! local loop indices
	  integer nio,ierror                 ! unit number of a file, flag number of IO status
	  real(r8), pointer  :: ftemp(:)     ! tempory float array
	  character(len=1000) :: stmp1! tempory string
          character(len=84000) :: stemp
	  character(len=4)   :: delims       ! delimiter
          real(r8) :: peak, prorata                   ! peak value to define the start of operationalyr
          integer :: sgn,curr_sgn, nsc, ct, ct_mx, mth_op , idepend      ! number of sign change
	  
	  ! grid properties
	  nn = ctlSubw%NUnit
         !dam properties
         nd = ctlSubwWRM%NDam
         ! max subw id number
         nsub = 264960

          allocate (WRMUnit%subw_Ndepend(nn))
          WRMUnit%subw_Ndepend = 0        ! 
          allocate (WRMUnit%subw_depend(nn,nd))
          WRMUnit%subw_depend = 0        !

          allocate (WRMUnit%dam_Ndepend(nd))
          WRMUnit%dam_Ndepend = 0        !
          allocate (WRMUnit%dam_depend(nd,nn))
          WRMUnit%dam_depend = 0        !
          allocate (WRMUnit%DamName(nd))
          allocate (WRMUnit%TotStorCapDepend(nn))
          WRMUnit%TotStorCapDepend = 0._r8
          allocate (WRMUnit%TotInflowDepend(nn))
          WRMUnit%TotInflowDepend = 0._r8


          allocate (WRMUnit%icell(nd))
          WRMUnit%icell = 0
          allocate (WRMUnit%INVicell(nn))
          WRMUnit%INVicell =-99 
          allocate (WRMUnit%mask(nd))
          WRMUnit%mask = 0
          allocate (WRMUnit%YEAR(nd))
          WRMUnit%YEAR = 1900
          allocate (WRMUnit%INVisubw(nsub))
          WRMUnit%INVisubw = -99

          allocate (WRMUnit%SurfArea(nd))
	  WRMUnit%SurfArea = 0._r8
	  allocate (WRMUnit%InstCap(nd))
          WRMUnit%InstCap = 0._r8
          allocate (WRMUnit%StorCap(nd))
          WRMUnit%StorCap = 0._r8
          allocate (WRMUnit%Height(nd))
          WRMUnit%Height = 0._r8
          allocate (WRMUnit%Length(nd))
          WRMUnit%Length = 0._r8
          allocate (WRMUnit%Depth(nd))
          WRMUnit%Depth = 0._r8


          allocate (WRMUnit%MeanMthFlow(nd,13))
          WRMUnit%MeanMthFlow = 0._r8
          allocate (WRMUnit%MeanMthDemand(nd,13))
          WRMUnit%MeanMthDemand = 0._r8

          allocate (WRMUnit%INVc(nd))
          WRMUnit%INVc = 0._r8
         allocate (WRMUnit%use_Irrig(nd))
          WRMUnit%use_Irrig = 0
         allocate (WRMUnit%use_Elec(nd))
          WRMUnit%use_Elec = 0
         allocate (WRMUnit%use_Supp(nd))
          WRMUnit%use_Supp = 0
         allocate (WRMUnit%use_FCon(nd))
          WRMUnit%use_FCon = 0
          allocate (WRMUnit%use_Fish(nd))
           WRMUnit%use_Fish = 0
          allocate (WRMUnit%use_Rec(nd))
           WRMUnit%use_Rec = 0
          allocate (WRMUnit%use_Navi(nd))
           WRMUnit%use_Navi = 0

         allocate (WRMUnit%Withdrawal(nd))
          WRMUnit%Withdrawal = 0
         allocate (WRMUnit%Conveyance(nd))
          WRMUnit%Conveyance = 0
         allocate (WRMUnit%MthStOp(nd))
          WRMUnit%MthStOp = 0
         allocate (WRMUnit%StorMthStOp(nd))
          WRMUnit%StorMthStOp = 0
         allocate (WRMUnit%MthStFC(nd))
          WRMUnit%MthStFC = 0
         allocate (WRMUnit%MthNdFC(nd))
          WRMUnit%MthNdFC = 0
         allocate (WRMUnit%FCtarget(nd))
          WRMUnit%FCtarget = 0
        allocate (WRMUnit%MthFCtarget(nd))
          WRMUnit%MthFCtarget = 0
        allocate (WRMUnit%MthFCtrack(nd))
          WRMUnit%MthFCtrack = 0


          allocate (StorWater%demand(nn))
           StorWater%demand=0._r8
          allocate (StorWater%supply(nn))
           StorWater%supply=0._r8
          allocate (StorWater%deficit(nn))
           StorWater%deficit=0._r8
          allocate (StorWater%pre_release(nd, 13))
            StorWater%pre_release = 0._r8
          allocate (StorWater%storage(nd))
            StorWater%storage = 0._r8
          allocate (StorWater%release(nd))
            StorWater%release = 0._r8
          allocate (StorWater%FCrelease(nd))
            StorWater%FCrelease = 0._r8
          allocate (StorWater%pot_evap(nn))
           StorWater%pot_evap=0._r8

          !allocate (TmpStoRelease(nd,:,:))


	  delims = ' '
	  ! reading directly available inputs variables
          if ( ctlSubwWRM%RegulationFlag > 0 ) then

          ! initialize parameters for regulation
	    stemp = trim(ctlSubwWRM%paraPath)//trim(ctlSubw%baseName)//'_reservoir.txt'
	    nio = 1
            open (unit=nio, file=stemp, status="old", action="read", iostat=ierror)
            if(ierror /= 0) then
              print*, "Failed to open " // stemp
	      stop
            end if
	    call readline(nio, stemp, ierror) ! read the first line with header
            do iunit=1,ctlSubwWRM%NDam
	      stemp = ''
		  call readline(nio, stemp, ierror)
		  call compact(stemp)
		  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%icell(iunit),ierror)    !location of the reservoir, subw ID
                  nn =  WRMUnit%icell(iunit)
		  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%mask(iunit),ierror)     !give to the mask[subbasinID] the GRandD ID for future referece
                  nd = WRMUnit%mask(iunit)
		  call split(stemp,' ',WRMUnit%DamName(iunit))
          !WRMUnit%DamName(iunit)=trim(stmp1)


                  do mth=1,13
                    call split(stemp,' ',stmp1)
                    call str2num(stmp1, WRMUnit%MeanMthFlow(iunit,mth), ierror) !mean monthly flow at the reservoir location
                  end do

		  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%INVc(iunit),ierror)     !4
                  !if ( WRMUnit%INVc(iunit) < 1 ) then
                  !  print*,"WARNING IN COMPUTATION OF MEAN ANNUAL INFLOW OR CAp, reset to 1"
                  !  WRMUnit%INVc(iunit)=1._r8
                  !endif
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%YEAR(iunit),ierror)   
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%Height(iunit),ierror)
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%Length(iunit),ierror)
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%SurfArea(iunit),ierror)
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%StorCap(iunit),ierror)
                  !in MCM
                  WRMUnit%StorCap(iunit)=WRMUnit%StorCap(iunit)*1000000
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%Depth(iunit),ierror)
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%use_Irrig(iunit),ierror)
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%use_Elec(iunit),ierror)
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%use_Supp(iunit),ierror)
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%use_FCon(iunit),ierror)
                 call split(stemp,' ',stmp1)
                 ! Recre, and naviga, and fish
                  call str2num(stmp1, WRMUnit%use_Rec(iunit),ierror)
                   call split(stemp,' ',stmp1)
                   call str2num(stmp1, WRMUnit%use_Navi(iunit),ierror)
                   call split(stemp,' ',stmp1)
                   call str2num(stmp1, WRMUnit%use_Fish(iunit),ierror)
                 call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%Withdrawal(iunit),ierror)
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, WRMUnit%Conveyance(iunit),ierror)

!need to read InstCap when available and monthly distribution of energy for targets and FContargets
            end do	  
	    close(unit=nio)

          !initialize INCVicell 
          do iunit=1,ctlSubwWRM%NDam
             j = WRMUnit%icell(iunit) ! cell number where dam is located, need indice
             match = 0
             do nn=1,ctlSubw%NUnit 
               match = TUnit%icell(nn,1)
               if (match .eq. j) then
                  WRMUnit%INVicell(nn) = iunit
               end if
             end do
             if ( match .eq. 0 ) then
               print*, "Error finding INVicell ", iunit, j, ctlSubw%NUnit, ctlSubwWRM%NDam
               stop
             endif
 
           end do

          !initialize INVsubw
          WRMUnit%NUnitID = 0 
          do iunit=1,ctlSubw%NUnit
             j = TUnit%icell(iunit, 1) ! cell number where dam is located, need indice
             if ( j > WRMUnit%NUnitID ) then
                WRMUnit%NUnitID = j
             endif
             WRMUnit%INVisubw(j)=iunit
          end do
          if ( WRMUnit%NUnitID > nsub ) then
            print*,"ATTENTION, ID max number is larger than the hard coded value of %d, adjust in full knowledge, nsub "
             stop
          endif

          ! initialize long term monthly demand - start with TOTAL DEMAND
            stemp = trim(ctlSubwWRM%demandPath)//trim(ctlSubw%baseName)//'_reservoir_demand_lgterm.total'
            !stemp = trim(ctlSubwWRM%demandPath)//trim(ctlSubw%baseName)//'_reservoir_demand_lgterm.irrig'

            nio = 1
            open (unit=nio, file=stemp, status="old", action="read", iostat=ierror)
            if(ierror /= 0) then
              print*, "Failed to open " // stemp
              stop
            end if
            !call readline(nio, stemp, ierror) ! read the first line with header
            do iunit=1,ctlSubwWRM%NDam
              stemp = ''
                  call readline(nio, stemp, ierror)
                  call compact(stemp)
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, nd,ierror)     !give to the mask[subbasinID] the GRandD ID for future referece
                  do mth=1,13
                    call split(stemp,' ',stmp1)
                    call str2num(stmp1, WRMUnit%MeanMthDemand(iunit,mth), ierror) !mean monthly flow at the reservoir location
                    !rthe demand is presently consumptive use and not withdrawal - needf to increase         
                    WRMUnit%MeanMthDemand(iunit,mth) = WRMUnit%MeanMthDemand(iunit,mth) * WRMUnit%Withdrawal(iunit)
                  end do
            end do
            close(unit=nio)
            print*, "read lg term demand 1",WRMUnit%MeanMthDemand(1,1), WRMUnit%MeanMthDemand(1,13)

            !initialize constant monthly pre-release based on longterm mean flow and demand (Biemans 2011) 
            do iunit=1,ctlSubwWRM%NDam
              do mth=1,12
                StorWater%pre_release(iunit,mth) = WRMUnit%MeanMthFlow(iunit,13)
              end do
              if ( WRMUnit%MeanMthDemand(iunit,13) >= (0.5_r8*WRMUnit%MeanMthFlow(iunit,13)) .and. WRMUnit%MeanMthFlow(iunit,13)>0._r8 ) then
                do mth=1,12
                  StorWater%pre_release(iunit,mth) = WRMUnit%MeanMthDemand(iunit,mth)/10._r8 + 9._r8/10._r8*WRMUnit%MeanMthFlow(iunit,13)*WRMUnit%MeanMthDemand(iunit,mth)/WRMUnit%MeanMthDemand(iunit, 13)
                  !TEST
                  !StorWater%pre_release(iunit,mth) = WRMUnit%MeanMthDemand(iunit,mth)/10._r8 + 9._r8/10._r8*WRMUnit%MeanMthFlow(iunit,13)*WRMUnit%MeanMthDemand(iunit,mth)/WRMUnit%MeanMthDemand(iunit, 13)*.5_r8
                end do
              else 
                do mth=1,12
                  if ( (WRMUnit%MeanMthFlow(iunit,13) + WRMUnit%MeanMthDemand(iunit,mth) - WRMUnit%MeanMthDemand(iunit,13))>0 ) then
                  StorWater%pre_release(iunit, mth) = WRMUnit%MeanMthFlow(iunit,13) + WRMUnit%MeanMthDemand(iunit,mth) - WRMUnit%MeanMthDemand(iunit,13)
                  endif 
                  ! test 2
                  !StorWater%pre_release(iunit, mth) = WRMUnit%MeanMthFlow(iunit,13)*0.5_r8 + WRMUnit%MeanMthDemand(iunit,mth) - WRMUnit%MeanMthDemand(iunit,13)
                  !TEST use pseudo regulated flow
                  !StorWater%pre_release(iunit, mth) = WRMUnit%MeanMthFlow(iunit,13)*.5_r8 + WRMUnit%MeanMthDemand(iunit,mth) - WRMUnit%MeanMthDemand(iunit,13)
                end do
              end if

             ! initialize storage in each reservoir - arbitrary 50%
             StorWater%storage(iunit) = 0.9_r8 * WRMUnit%StorCap(iunit)   
             if ( WRMUnit%StorCap(iunit) <= 0 ) then
               print*, "Error negative max cap for reservoir ", iunit, WRMUnit%StorCap(iunit)
               stop
             end if
            end do
            print*, "storage ",StorWater%pre_release(1,1), StorWater%storage(1)



            ! initialize start of the operationnal year based on long term simulation
            call WRM_init_StOp_FC

             !initialize dam dependencies
            print*, "initialize dam dependencies .."
            stemp = trim(ctlSubwWRM%paraPath)//trim(ctlSubw%baseName)//'_dam_dependence.txt'
            nio = 1
            open (unit=nio, file=stemp, status="old", action="read", iostat=ierror)
            if(ierror /= 0) then
              print*, "Failed to open " // stemp
              stop
            end if
            !call readline(nio, stemp, ierror) ! read the first line with header
            do iunit=1,ctlSubwWRM%NDam
              stemp = ''
                  call readline(nio, stemp, ierror)
                  call compact(stemp)
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, nn,ierror)    !reservoir ID
                  if (  nn .NE. WRMUnit%mask(iunit)) then
                    print*, "Error, " // stmp1 // "should have the same order as reservoir param file", nn, iunit, WRMUnit%mask(iunit)
                    stop
                  end if
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, nd,ierror)    !number of dependent subw for nn
                  WRMUnit%dam_Ndepend(iunit) = nd

! need to adjust for reservoir with zero inflow, do not  need to read the remaining
                  if ( WRMUnit%MeanMthFlow(iunit,13) <= 0._r8 ) then
                    WRMUnit%dam_Ndepend(iunit) = 0 ! this reservoir will not provide water to any subw, relieve database
                  end if

                  do j=1,WRMUnit%dam_Ndepend(iunit)
                    call split(stemp,' ',stmp1)
                    call str2num(stmp1, nd, ierror) 
!need additionnal check due to regionalization, need to remove non existing grid cell NV
                    if (  WRMUnit%INVisubw(nd) .lt. 1 ) then
                      WRMUnit%dam_Ndepend(iunit) = WRMUnit%dam_Ndepend(iunit) - 1
                      j = j -1
                    else
                      WRMUnit%dam_depend(iunit,j) = nd 
                    endif
                  end do
                  !if (iunit==1) print*,"END read dam dependence ",iunit, WRMUnit%mask(iunit),WRMUnit%dam_Ndepend(iunit),WRMUnit%dam_depend(iunit,403) 
             end do
             close(unit=nio)

             !initialize subw dependencies
            print*, "initialize subw dependencies   "
            stemp = trim(ctlSubwWRM%paraPath)//trim(ctlSubw%baseName)//'_subw_dependence.txt'
            nio = 1
            open (unit=nio, file=stemp, status="old", action="read", iostat=ierror)
            if(ierror /= 0) then
              print*, "Failed to open " // stemp
              stop
            end if
            !call readline(nio, stemp, ierror) ! read the first line with header
            do iunit=1,ctlSubw%NUnit
              stemp = ''
                  call readline(nio, stemp, ierror)
                  call compact(stemp)
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, nd,ierror)    !subw ID
                  call split(stemp,' ',stmp1)
                  call str2num(stmp1, nn,ierror)    !number of dependent dam for subw
                  WRMUnit%subw_Ndepend(iunit) = nn
                  do j=1,WRMUnit%subw_Ndepend(iunit)
                    call split(stemp,' ',stmp1)
!need additionnal check
                    call str2num(stmp1, WRMUnit%subw_depend(iunit,j), ierror) 
                    !call str2num(stmp1, nn, ierror)
                    !if (   .lt. 1 ) then
                    !  WRMUnit%subw_Ndepend(iunit) = WRMUnit%subw_Ndepend(iunit) - 1
                    !  j=j-1
                    !else
                    !  WRMUnit%subw_depend(iunit,j) = nn
                    !endif

! test starts
                  !  do i=1, ctlSubwWRM%NDam
                  !    if ( WRMUnit%subw_depend(iunit,j) .eq. WRMUnit%mask(i) .and. WRMUnit%MeanMthFlow(i,13) >= 0.001_r8) then
                  !      WRMUnit%TotStorCapDepend(iunit) = WRMUnit%TotStorCapDepend(iunit) + WRMUnit%StorCap(i)
                  !      WRMUnit%TotInflowDepend(iunit) = WRMUnit%TotInflowDepend(iunit) + WRMUnit%MeanMthFlow(i,13)
                  !    endif
                  !  end do
! end test
                  end do
                  !print*, iunit, WRMUnit%subw_Ndepend(iunit), WRMUnit%TotStorCapDepend(iunit), WRMUnit%TotInflowDepend(iunit)
                  !if ( WRMUnit%TotInflowDepend(iunit) <= 0._r8 .and. WRMUnit%subw_Ndepend(iunit) > 0 ) then
                  !   ! happens with the differetn setu up - reservoir not well placed or just not in the routing domain
                  !  print*, "reservoir has a null inflow", iunit
                  !  WRMUnit%subw_Ndepend(iunit) = 0
                  !  WRMUnit%TotInflowDepend(iunit) = 0._r8
                  !  !stop
                  !end if
             end do
             close(unit=nio)

              !check the dependence database consistencies
             print*, "check dependencies "
             do iunit=1,ctlSubwWRM%NDam
                do j=1,WRMUnit%dam_Ndepend(iunit)
                  idepend = WRMUnit%INVisubw(WRMUnit%dam_depend(iunit,j))
! testy starts
                  if ( idepend .lt. 0 ) then
                    print*,"Error checking dependency, zero idepend", iunit, WRMUnit%dam_Ndepend(iunit), j, idepend , WRMUnit%dam_depend(iunit,j)
                    stop
                  endif
                  WRMUnit%TotStorCapDepend(idepend) = WRMUnit%TotStorCapDepend(idepend) + WRMUnit%StorCap(iunit)
                  WRMUnit%TotInflowDepend(idepend) = WRMUnit%TotInflowDepend(idepend) + WRMUnit%MeanMthFlow(iunit,13)
! end starts
                    !if (WRMUnit%TotInflowDepend(idepend) > 0.001_r8) then
                    !prorata = WRMUnit%MeanMthFlow(iunit,13) / WRMUnit%TotInflowDepend(idepend)
                    !if ( prorata > 1 ) then
                    !  print*,"Error with database prorata "
                    !  print*,iunit, idepend, prorata, WRMUnit%MeanMthFlow(iunit,13) ,WRMUnit%TotInflowDepend(idepend)
                    !  stop
                    !endif
                  !endif
                end do
             end do

!                  match =  0
!                  if ( idepend <= 0 ) then
!                    print*, "Error dependence, idepend ",iunit,j, WRMUnit%dam_Ndepend(iunit), WRMUnit%dam_depend(iunit,j),idepend
!!                    stop
!                  endif
!                  if ( WRMUnit%subw_Ndepend(idepend) < 1 ) then
!                   print*, "Error dependence, Ndepend"
!                    print*, "idepend=", idepend, ", iunit=",iunit," j=",j, " WRMUnit%dam_Ndepend(",iunit,")=",WRMUnit%dam_Ndepend(iunit)
!                    print*,"Tunit%icell=" ,TUnit%icell(idepend,1)
!                    print*, "WRMUnit%mask(",iunit,")=",WRMUnit%mask(iunit), " WRMUnit%subw_Ndepend(",idepend,")=",WRMUnit%subw_Ndepend(idepend)
!                    print*, ", WRMUnit%dam_depend(",iunit,",",j,")=",WRMUnit%dam_depend(iunit,j)," ???"
!                    stop
!                  endif
!                  do  i=1,WRMUnit%subw_Ndepend(idepend)
!                    if ( WRMUnit%subw_depend(idepend,i) .eq. WRMUnit%mask(iunit)) match = 1
!                    !print*,iunit, WRMUnit%dam_Ndepend(iunit),idepend, i, WRMUnit%subw_Ndepend(idepend), WRMUnit%subw_depend(idepend,i), WRMUnit%mask(iunit),match
!                  end do
!                  if ( match .eq. 0 ) then
!                    print*, " issue in database indices ",iunit, WRMUnit%dam_Ndepend(iunit),WRMUnit%mask(iunit) 
!                    !print*, idepend, j, WRMUnit%subw_Ndepend(idepend), WRMUnit%subw_depend(idepend,i), WRMUnit%dam_Ndepend(iunit)  
!                    print*, "idepend=", idepend, ", iunit=",iunit, " WRMUnit%dam_Ndepend(",iunit,")=",WRMUnit%dam_Ndepend(iunit)
!                    print*,"Tunit%icell=" ,TUnit%icell(idepend,1)
!                    print*, "WRMUnit%mask(",iunit,")=",WRMUnit%mask(iunit), " WRMUnit%subw_Ndepend(",idepend,")=",WRMUnit%subw_Ndepend(idepend)
!                    !print*, ", WRMUnit%dam_depend(",iunit,",",j,")=",WRMUnit%dam_depend(iunit,j)," ???"
!                    stop
!                  end if
!                 end do
!             end do
!             print*, "dependency databases checked"

             

      end if !Regulation Flag

      ! check
      print*, "Done with WM init ..."
      !print*, WRMUnit%DamName(1), WRMUnit%Surfarea(1)
      !print*,WRMUnit%mask(1), WRMUnit%icell(1) 
      !print*, WRMUnit%dam_Ndepend(1), WRMUnit%dam_depend(1,2)
      !print*, "sub = 49",  TUnit%icell(49, 1),WRMUnit%subw_Ndepend(49),  WRMUnit%subw_depend(49,1) 
  end subroutine WRM_init

  subroutine readPotentialEvap(theTime)
  !! DESCRIPTION: read the simulated potential evaporation to adjust the storage of reservoir
          implicit none
      character(len=*), intent(in) :: theTime

      character(len=250) :: petFileName
          integer :: ios, iunit, ilat, ilon    ! flag of IO status, local indices
          real(r8) :: ftemp1            ! tempory array
          petFileName = adjustl(trim(ctlSubw%runoffPath))//'pet/'//theTime//'.pet'
          open (unit=1, file=petFileName, status="old", action="read", iostat=ios)
          if(ios /= 0) then
              print*, "Cannot find file ", petFileName
                  stop
          end if
      iunit=0
          do iunit=1, ctlSubw%NUnit
              read(unit=1, *) StorWater%pot_evap(iunit)
              if ( StorWater%pot_evap(iunit) < 0._r8 ) then
                print*, "negative evap, iunit ", iunit
                StorWater%pot_evap(iunit) = 0._r8
              end if
          end do
          close(unit=1)
          StorWater%pot_evap = 0.75_r8 * StorWater%pot_evap * 0.001_r8/ctlSubw%DATAH  !mm/day-->m/s, or mm/hr-->m/s, TO DO
          ! the 0.75 is due to the fact that raw potential evap overestimate the evaporation from large bodies of water
          ! USGS assume between 0.65 to 0.85 woith 0.7 when air temp = water temp
  end subroutine readPotentialEvap
  
  subroutine readIrrigDemand(theTime)
  ! !DESCRIPTION: read in the irrigation demand data for each time step
	  implicit none
      character(len=*), intent(in) :: theTime
      
      character(len=250) :: IrrdemFileName  ! water demand file names
	  integer :: ios, iunit, ilat, ilon    ! flag of IO status, local indices
	  real(r8) :: ftemp1            ! tempory array
	  IrrdemFileName = adjustl(trim(ctlSubwWRM%demandPath))//trim(ctlSubw%baseName)//'_'//theTime//'.total'
          !IrrdemFileName = adjustl(trim(ctlSubwWRM%demandPath))//trim(ctlSubw%baseName)//'_'//theTime//'.irrig'
	  open (unit=1, file=IrrdemFileName, status="old", action="read", iostat=ios)
	  if(ios /= 0) then
	      print*, "Cannot find file ", IrrdemFileName
	      stop
	  end if
       iunit=0
	  do iunit=1, ctlSubw%NUnit
	      read(unit=1, *) StorWater%demand(iunit)
              !StorWater%demand(iunit) = StorWater%demand(iunit) /ctlSubw%DATAH   / TUnit%area(iunit) !mm/day-->m/s, cubic meter/day -> m/s
              !StorWater%demand(iunit) = StorWater%demand(iunit) / ctlSubw%DATAH 
              !StorWater%demand(iunit) = StorWater%demand(iunit) / TUnit%area(iunit) *1000 ! keep it in mm/day
              !print*, "read demand  ", iunit, StorWater%demand(iunit)
              if ( StorWater%demand(iunit) .lt. 0._r8 ) then
                !print*,"reset demand "
              !  stop
                StorWater%demand(iunit) = 0._r8
              end if
              ! consumptive use - keep it even
              !!StorWater%demand(iunit) = StorWater%demand(iunit) * 3._r8
	  end do
	  close(unit=1)

	  !liqWater%qsur = 1.0e-6_r8  !mm/day-->m/s, TO DO
	  !liqWater%qsub = 0.0e-6_r8  !mm/day-->m/s, TO DO

          !print*, "StorWater%demand(49)", StorWater%demand(49), "read ", ctlSubw%NUnit
  end subroutine readIrrigDemand

  subroutine printRemainDemand(theTime, strLen, nio)
      ! !DESCRIPTION: output the simulation results into external files
          implicit none
      character(len=*), intent(in) :: theTime  ! the time step to output
          integer, intent(in) :: strLen     ! length of each line to print
          integer, intent(in) :: nio        ! unit of the file to print

          integer :: ios                    ! flag of io status
          integer :: iunit          ! local index
          character(len=strLen) :: strLine
          character(len=20) :: stemp

          strLine = ''
          do iunit=1, ctlSubw%NUnit
                  stemp = ''
                  call num2str(StorWater%demand(iunit), stemp, 'e20.10')
              strLine = trim(strLine)//adjustr(stemp)
          end do

          print*, "StorWater%demand(137)", StorWater%demand(137)
          write(unit=nio,fmt="((a10), (a))") theTime, strLine
          !write(unit=nio,fmt="((a10)") "supply"
  end subroutine printRemainDemand



  subroutine printIrrigSupply(theTime, strLen, nio)
      ! !DESCRIPTION: output the simulation results into external files
	  implicit none
      character(len=*), intent(in) :: theTime  ! the time step to output
	  integer, intent(in) :: strLen     ! length of each line to print
	  integer, intent(in) :: nio        ! unit of the file to print
	  
	  integer :: ios                    ! flag of io status
	  integer :: iunit          ! local index
	  character(len=strLen) :: strLine 
	  character(len=20) :: stemp 

	  strLine = ''
          do iunit=1, ctlSubw%NUnit
		  stemp = ''
		  call num2str(StorWater%supply(iunit), stemp, 'e20.10')
	      strLine = trim(strLine)//adjustr(stemp)
	  end do
	  
          print*, "print StorWater%supply(137)", StorWater%supply(137), ctlSubw%NUnit
	  write(unit=nio,fmt="((a10), (a))") theTime, strLine
          !write(unit=nio,fmt="((a10)") "supply"
  end subroutine printIrrigSupply

  subroutine printStorage(theTime, strLen, nio)
      ! !DESCRIPTION: output the simulation results into external files
          implicit none
      character(len=*), intent(in) :: theTime  ! the time step to output
          integer, intent(in) :: strLen     ! length of each line to print
          integer, intent(in) :: nio        ! unit of the file to print

          integer :: ios                    ! flag of io status
          integer :: iunit          ! local index
          character(len=strLen) :: strLine
          character(len=20) :: stemp

          strLine = ''
          do iunit=1, ctlSubwWRM%NDam
                  stemp = ''
                  call num2str(StorWater%storage(iunit), stemp, 'e20.10')
              strLine = trim(strLine)//adjustr(stemp)
          end do

          !print*, "StorWater%storage(1:5)", StorWater%storage(1), StorWater%storage(2),StorWater%storage(3),StorWater%storage(4),StorWater%storage(5)
          write(unit=nio,fmt="((a10), (a))") theTime, strLine
          !write(unit=nio,fmt="((a10)") "supply"
  end subroutine printStorage

 subroutine printIrrigSupplyTest(theTime, strLen, nio)
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
                  call num2str(StorWater%supply(theUnit), stemp, 'e20.10')
              strLine = trim(strLine)//adjustr(stemp)
          end do

          print*, "StorWater%supply(137)", StorWater%supply(137)
          write(unit=nio,fmt="((a10), (a))") theTime, strLine
  end subroutine printIrrigSupplyTest

  

  subroutine printTest2(theTime, theUnit, nio)
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
  
  end subroutine printTest2
   
end MODULE WRM_subw_IO_mod
