!
MODULE WRM_modules
! Description: core code of the WRM. 
! 
! Developed by Nathalie Voisin Feb 2012
! REVISION HISTORY:
!-----------------------------------------------------------------------

! !USES:
	use shr_kind_mod  , only : r8 => shr_kind_r8, SHR_KIND_CL
	use shr_const_mod , only : SHR_CONST_REARTH, SHR_CONST_PI
	use MOSART_type_mod, only : Tctl => ctlSubw, TUnit, liqWater, para
	use WRM_type_mod, only : TWRMctl => ctlSubwWRM, WRMUnit, StorWater
 	use MOSART_physics_mod
	
	implicit none
!	real(r8), parameter :: TINYVALUE = 1.0e-50_r8  ! double precisio variable has a significance of about 16 decimal digits
  
								   
! !PUBLIC MEMBER FUNCTIONS:
	contains

        subroutine Euler_WRM
        ! !DESCRIPTION: solve the ODEs with Euler algorithm
              implicit none

              integer :: iunit, m, k   !local index
              real(r8) :: temp_erout, localDeltaT

              if(Tctl%RoutingFlag == 1) then
                !print*, "Running WRM Euler"
                do iunit=1,Tctl%NUnit
                    call hillslopeRouting(iunit, Tctl%DeltaT)
                        liqWater%wh(iunit) = liqWater%wh(iunit) + liqWater%dwh(iunit) * Tctl%DeltaT
                        call UpdateState_hillslope(iunit)
                        liqWater%etin(iunit) = (-liqWater%ehout(iunit) + liqWater%qsub(iunit)) * TUnit%area(iunit) * TUnit%frac(iunit)
                end do
                ! exctraction from available surface runoff within the subw
                if (TWRMctl%ExtractionFlag > 0) then
                  call irrigationExtractionSubNetwork
                end if

                
                liqWater%flow = 0._r8
                do m=1,Tctl%DLevelH2R
                    do iunit=1,Tctl%NUnit
                            liqWater%erlateral(iunit) = 0._r8
                            do k=1,TUnit%numDT_t(iunit)
                                    localDeltaT = Tctl%DeltaT/Tctl%DLevelH2R/TUnit%numDT_t(iunit)
                                        call subnetworkRouting(iunit,localDeltaT)
                                        liqWater%wt(iunit) = liqWater%wt(iunit) + liqWater%dwt(iunit) * localDeltaT
                                        call UpdateState_subnetwork(iunit)
                                        liqWater%erlateral(iunit) = liqWater%erlateral(iunit)-liqWater%etout(iunit)
                            end do
                                liqWater%erlateral(iunit) = liqWater%erlateral(iunit) / TUnit%numDT_t(iunit)
                    end do

                        do iunit=1,Tctl%NUnit
                           if(TUnit%fdir(iunit) >= 0) then
                                temp_erout = 0._r8
                                do k=1,TUnit%numDT_r(iunit)
                                    localDeltaT = Tctl%DeltaT/Tctl%DLevelH2R/TUnit%numDT_r(iunit)
                                        call mainchannelRouting(iunit,localDeltaT)
                                        liqWater%wr(iunit) = liqWater%wr(iunit) + liqWater%dwr(iunit) * localDeltaT
                                        call UpdateState_mainchannel(iunit)
                                        temp_erout = temp_erout + liqWater%erout(iunit) ! erout here might be inflow to some downstream subbasin, so treat it differently than erlateral
                                end do
                                temp_erout = temp_erout / TUnit%numDT_r(iunit)
                                liqWater%erout(iunit) = temp_erout
                                localDeltaT = Tctl%DeltaT/Tctl%DLevelH2R
                                if (TWRMctl%ExtractionMainChannelFlag > 0 .AND. TWRMctl%ExtractionFlag > 0 ) then
                                     call IrrigationExtractionMainChannel(iunit, localDeltaT )
                                  ! update main channel storage as well
                                  temp_erout = temp_erout - liqWater%erout(iunit) ! change in erout after regulation and extraction
                                  liqWater%dwr(iunit) =  temp_erout
                                  liqWater%wr(iunit) = liqWater%wr(iunit) + liqWater%dwr(iunit) * localDeltaT
                                  call UpdateState_mainchannel(iunit)
                                endif
                                if ( TWRMctl%RegulationFlag>0 .and. WRMUnit%INVicell(iunit) > 0 .and. WRMUnit%MeanMthFlow(iunit,13) > 0.01_r8 ) then
                                    call Regulation(iunit, localDeltaT)
                                    if ( TWRMctl%ExtractionFlag > 0 ) then
                                      call ExtractionRegulatedFlow(iunit, localDeltaT)
                                    endif
                                endif

                                liqWater%flow(iunit) = liqWater%flow(iunit) - liqWater%erout(iunit)
                                ! do not update wr after regulation or extraction from reservoir release. Because of the regulation, the wr might get to crazy uncontrolled values, assume in this case wr is not changed. The storage in reservoir handles it.

                            end if
                        end do
                        !print*, "flow ", m ,liqWater%flow(137), liqWater%erlateral(137), StorWater%supply(137)
                end do
              else
                        liqWater%flow = 0._r8
                        do m=1,Tctl%DLevelH2R
                                do iunit=1,Tctl%NUnit
                                        liqWater%erlateral(iunit) = (liqWater%qsur(iunit) + liqWater%qsub(iunit)) * TUnit%area(iunit) * TUnit%frac(iunit)
                                end do
                               ! exctraction from available surface runoff within the subw
                               if (TWRMctl%ExtractionFlag > 0) then
                                 call irrigationExtraction
                               end if


                               do iunit=1,Tctl%NUnit
                                        temp_erout = 0._r8
                                        do k=1,TUnit%numDT_r(iunit)
                                                localDeltaT = Tctl%DeltaT/Tctl%DLevelH2R/TUnit%numDT_r(iunit)
                                                call mainchannelRouting(iunit,localDeltaT)
                                                liqWater%wr(iunit) = liqWater%wr(iunit) + liqWater%dwr(iunit) * localDeltaT
                                                call UpdateState_mainchannel(iunit)
                                                temp_erout = temp_erout + liqWater%erout(iunit) ! erout here might be inflow to some downstream subbasin, so treat it differently than erlateral
                                        end do
                                        temp_erout = temp_erout / TUnit%numDT_r(iunit)
                                        liqWater%erout(iunit) = temp_erout
                                        localDeltaT = Tctl%DeltaT/Tctl%DLevelH2R
                                        if (TWRMctl%ExtractionMainChannelFlag > 0 .AND. TWRMctl%ExtractionFlag > 0 ) then
                                          call IrrigationExtractionMainChannel(iunit, localDeltaT )
                                          ! update main channel storage as well
                                          temp_erout = temp_erout - liqWater%erout(iunit) ! change in erout after regulation and extraction
                                          liqWater%dwr(iunit) =  temp_erout
                                          liqWater%wr(iunit) = liqWater%wr(iunit) + liqWater%dwr(iunit) * localDeltaT
                                          call UpdateState_mainchannel(iunit)
                                        endif
                                        if ( TWRMctl%RegulationFlag>0 .and. WRMUnit%INVicell(iunit) > 0 .and. WRMUnit%subw_Ndepend(iunit) > 0 ) then
                                          call Regulation(iunit, localDeltaT)
                                          if ( TWRMctl%ExtractionFlag > 0 ) then
                                            call ExtractionRegulatedFlow(iunit, localDeltaT)
                                          endif
                                       endif
                                       liqWater%flow(iunit) = liqWater%flow(iunit) - liqWater%erout(iunit)

                                end do
                        end do

                end if

                liqWater%flow = liqWater%flow / Tctl%DLevelH2R
        end subroutine Euler_WRM


	subroutine irrigationExtraction
	! !DESCRIPTION: subnetwork channel routing irrigation extraction
		implicit none    
		integer :: iunit      ! local index
                real(r8) :: flow_vol, temp         ! flow in cubic meter rather than cms
		do iunit=1,Tctl%NUnit

                flow_vol = liqWater%erlateral(iunit) * Tctl%DeltaT/Tctl%DLevelH2R
                temp = flow_vol
		    if(TUnit%fdir(iunit) >= 0) then
			if ( flow_vol >= StorWater%demand(iunit) ) then 
                                StorWater%supply(iunit)= StorWater%supply(iunit) + StorWater%demand(iunit)
                                flow_vol = flow_vol - StorWater%demand(iunit)
                                StorWater%demand(iunit)= 0._r8
                        else
                                StorWater%supply(iunit)= StorWater%supply(iunit) + flow_vol
                                StorWater%demand(iunit)= StorWater%demand(iunit) - flow_vol
                                flow_vol = 0._r8
                        end if 
! dwt is not updated because extraction is taken from water getting out of subnetwork channel routing only
                    end if
                 liqWater%erlateral(iunit) = flow_vol / (Tctl%DeltaT/Tctl%DLevelH2R)
		end do
	end subroutine irrigationExtraction

        subroutine irrigationExtractionSubNetwork
        ! !DESCRIPTION: subnetwork channel routing irrigation extraction
                implicit none
                integer :: iunit      ! local index
                real(r8) :: flow_vol, temp, temp_vol         ! flow in cubic meter rather than cms
                do iunit=1,Tctl%NUnit

                    flow_vol = liqWater%etin(iunit)* Tctl%DeltaT/Tctl%DLevelH2R
                    if(TUnit%fdir(iunit) >= 0) then
                        if ( flow_vol >= StorWater%demand(iunit) ) then
                                StorWater%supply(iunit)= StorWater%supply(iunit) + StorWater%demand(iunit)
                                flow_vol = flow_vol - StorWater%demand(iunit)
                                StorWater%demand(iunit)= 0._r8
                        else
                                StorWater%supply(iunit)= StorWater%supply(iunit) + flow_vol
                                StorWater%demand(iunit)= StorWater%demand(iunit) - flow_vol
                                flow_vol = 0._r8
                        end if
                    end if
                   liqWater%etin(iunit) = flow_vol / (Tctl%DeltaT/Tctl%DLevelH2R)
                end do
        end subroutine irrigationExtractionSubNetwork

        subroutine irrigationExtractionMainChannel(iunit, TheDeltaT )
        ! !DESCRIPTION: main channel routing irrigation extraction - restrict to 50% of the flow, something needs to flow else instability
                implicit none
                integer :: match
                integer, intent(in) :: iunit
                real(r8), intent(in) :: theDeltaT
                real(r8) :: flow_vol, frac         ! flow in cubic meter rather than cms
                match = 0
                frac = 0.5_r8 ! control the fraction of the flow that can be extracted

                flow_vol = -liqWater%erout(iunit) * theDeltaT
                if ( (frac*flow_vol) >= StorWater%demand(iunit) ) then
                   StorWater%supply(iunit)= StorWater%supply(iunit) + StorWater%demand(iunit)
                   flow_vol = flow_vol - StorWater%demand(iunit)
                   StorWater%demand(iunit)= 0._r8
                else
                   StorWater%supply(iunit)= StorWater%supply(iunit) + frac*flow_vol
                   StorWater%demand(iunit)= StorWater%demand(iunit) - frac*flow_vol
                   flow_vol = (1._r8-frac)*flow_vol
                end if
                liqWater%erout(iunit) = -flow_vol / (theDeltaT)
                !if (  match > 0 ) then
                !   print*, "MAIN after extract", StorWater%demand(137), StorWater%supply(137), -liqWater%erout(137)
                !endif

        end subroutine irrigationExtractionMainChannel

  
        subroutine RegulationRelease
        !! DESCRIPTION: computes the expected monthly release based on Biemans (2011)
          implicit none
          integer :: iunit, mth
          real(r8) :: factor, k
          k = 1.0_r8

          mth = TWRMctl%month
          do iunit=1,TWRMctl%NDam

            !if ( WRMUnit%use_FCon(iunit) > 0 .or. WRMUnit%use_Supp(iunit) > 0) then
                StorWater%release(iunit) = WRMUnit%MeanMthFlow(iunit,13)
            !endif
            ! prioity to irrigation and other use since storage targets for FC
            if ( WRMUnit%use_Elec(iunit) > 0 .or. WRMUnit%use_Irrig(iunit) >0) then
                
              k = 1._r8
              factor = 0._r8
              k = WRMUnit%StorMthStOp(iunit) / ( 0.85 * WRMUnit%StorCap(iunit) )
              if ( WRMUnit%INVc(iunit) .gt. 0.1_r8 ) then
                factor = (1._r8/(0.5_r8*WRMUnit%INVc(iunit)))*(1._r8/(0.5_r8*WRMUnit%INVc(iunit)))
              endif
              
              !if ( (1._r8/WRMUnit%INVc(iunit)) >= 0.5_r8 ) then
               if ( WRMUnit%INVc(iunit) <= 2._r8 ) then
                StorWater%release(iunit) = k * Storwater%pre_release(iunit,mth)
              else
                StorWater%release(iunit) = k * factor*Storwater%pre_release(iunit,mth) + (1._r8-factor) * WRMUnit%MeanMthFlow(iunit,mth)  
              end if
             ! Run-on-the-river flow
            !if ( WRMUnit%use_FCon(iunit) .eq. 0 .and.  WRMUnit%use_Irrig(iunit).eq.0 .and. WRMUnit%use_Elec(iunit) > 0 ) then
            !  StorWater%release(iunit) = WRMUnit%MeanMthFlow(iunit,mth)
            end if

          end do

        end subroutine RegulationRelease
      subroutine WRM_storage_targets
      ! !DESCRIPTION: definr the necessary drop in storage based in sotrage at srta of the month
      ! NOT TO BE RUN IN EULER
          implicit none
          integer :: iunit, month
          integer nio,ierror                 ! unit number of a file, flag number of IO status
          integer :: mth, Nmth, Nmth_fill       ! number of sign change
          real(r8) :: drop, fill
          month = TWRMctl%month

          do iunit=1,TWRMctl%NDam

            drop = 0
            Nmth = 0
            if ( WRMUnit%use_FCon(iunit) > 0 .and. WRMUnit%MthStFC(iunit) > 0) then ! in the context of FC has priority
             ! modify release in order to mainteain a certaon storage level
              if ( WRMUnit%MthStFC(iunit) <= WRMUnit%MthNdFC(iunit) ) then
                do mth = 1,12
                  if ( mth >= WRMUnit%MthStFC(iunit) .and. mth < WRMUnit%MthNdFC(iunit)) then
                    if ( WRMUnit%MeanMthFlow(iunit, mth) >= WRMUnit%MeanMthFlow(iunit, 13) ) then
                      drop = drop + 0._r8
                    else
                      drop = drop + abs(WRMUnit%MeanMthFlow(iunit, 13) - WRMUnit%MeanMthFlow(iunit, mth))
                    endif
                    Nmth = Nmth + 1
                  endif
                enddo
              else if ( WRMUnit%MthStFC(iunit) > WRMUnit%MthNdFC(iunit) ) then
                do mth =1,12
                  if (mth >= WRMUnit%MthStFC(iunit) .or. mth < WRMUnit%MthNdFC(iunit)) then
                    if ( WRMUnit%MeanMthFlow(iunit, mth) >= WRMUnit%MeanMthFlow(iunit, 13) ) then
                      drop = drop + 0._r8
                    else
                      drop = drop + abs(WRMUnit%MeanMthFlow(iunit, 13) - WRMUnit%MeanMthFlow(iunit, mth))
                    endif
                    Nmth = Nmth + 1
                  endif
                enddo
              endif
                  
              if ( Nmth > 0 ) then
                if ( WRMUnit%MthStFC(iunit) <= WRMUnit%MthNdFC(iunit) ) then
                  if ( month >= WRMUnit%MthStFC(iunit) .and. month < WRMUnit%MthNdFC(iunit)) then
                    StorWater%release(iunit) = StorWater%release(iunit) + drop/Nmth
                  endif
                else if ( WRMUnit%MthStFC(iunit) > WRMUnit%MthNdFC(iunit) ) then
                  if ( month >= WRMUnit%MthStFC(iunit) .or. month < WRMUnit%MthNdFC(iunit)) then
                    StorWater%release(iunit) = StorWater%release(iunit) + drop/Nmth
                  endif
                endif
             endif

             ! now need to make sure that it will fill up but issue with spilling  in certain hydro-climatic conditions
             fill = 0  
             Nmth_fill = 0
             if ( WRMUnit%MthNdFC(iunit) <= WRMUnit%MthStOP(iunit) ) then
                if ( month >= WRMUnit%MthNdFC(iunit) .and. month < WRMUnit%MthStOp(iunit) ) then
                  do mth = WRMUnit%MthNdFC(iunit), WRMUnit%MthStOP(iunit)
                     if ( WRMUnit%MeanMthFlow(iunit, mth) > WRMUnit%MeanMthFlow(iunit, 13) ) then
                       fill = fill + abs(WRMUnit%MeanMthFlow(iunit, 13) - WRMUnit%MeanMthFlow(iunit, mth))
                       Nmth_fill = Nmth_fill + 1
                     endif
                  end do
                  ! does drop fill up the reservoir?
                  !if ( fill > drop .and. Nmth_fill > 0 ) then
                  !  StorWater%release(iunit) = WRMUnit%MeanMthFlow(iunit, 13) + (fill - drop) / Nmth_fill
                  !else  !need to fill this reservoir 
                    if ( StorWater%release(iunit) > WRMUnit%MeanMthFlow(iunit, 13) ) then
                      StorWater%release(iunit) = WRMUnit%MeanMthFlow(iunit, 13)
                    endif
                  !endif
                end if
             else if ( WRMUnit%MthNdFC(iunit) > WRMUnit%MthStOP(iunit) ) then
                if ( month >= WRMUnit%MthNdFC(iunit) .or. month < WRMUnit%MthStOp(iunit)) then
                  do mth = WRMUnit%MthNdFC(iunit), 12
                     if ( WRMUnit%MeanMthFlow(iunit, mth) > WRMUnit%MeanMthFlow(iunit, 13) ) then
                       fill = fill + abs(WRMUnit%MeanMthFlow(iunit, 13) - WRMUnit%MeanMthFlow(iunit, mth))
                       Nmth_fill = Nmth_fill + 1
                     endif
                  end do
                  do mth = 1, WRMUnit%MthStOP(iunit)
                     if ( WRMUnit%MeanMthFlow(iunit, mth) > WRMUnit%MeanMthFlow(iunit, 13) ) then
                       fill = fill + abs(WRMUnit%MeanMthFlow(iunit, 13) - WRMUnit%MeanMthFlow(iunit, mth))
                       Nmth_fill = Nmth_fill + 1
                     endif
                  end do
                  ! does drop fill up the reservoir?
                  !if ( fill > drop .and. Nmth_fill > 0 ) then
                  !  StorWater%release(iunit) = WRMUnit%MeanMthFlow(iunit, 13) + (fill - drop) / Nmth_fill
                  !else  !need to fill this reservoir
                    if ( StorWater%release(iunit) > WRMUnit%MeanMthFlow(iunit, 13) ) then
                      StorWater%release(iunit) = WRMUnit%MeanMthFlow(iunit, 13)
                    endif
                  !endif
                end if
             endif
           endif
         end do
      end subroutine WRM_storage_targets


	subroutine Regulation(iunit, TheDeltaT)
	!! DESCRIPTION: regulation of the flow from the reservoirs. The Regulation is applied to the flow entering the grid cell, i.e. the subw downstream of the reservoir. 
        ! !DESCRIPTION: CHANGE IN PLANS seems like erin get overwritten now play with erout
                implicit none
                integer :: match
                integer, intent(in) :: iunit
                real(r8), intent(in) :: TheDeltaT
                integer, damID
                real(r8) :: flow_vol, flow_res, min_flow, min_stor, evap
                match = 0

                damID = WRMUnit%INVicell(iunit) 
                if ( damID > TWRMctl%NDam .OR. damID <= 0 ) then
                  print*, "Error in Regulation with DamID ",damID
                  stop
                end if
                flow_vol = -liqWater%erout(iunit) * theDeltaT
                flow_res = StorWater%release(damID) * theDeltaT
                evap = StorWater%pot_evap(iunit) * theDeltaT * WRMUnit%SurfArea(damID) * 1000000._r8 ! potential evaporation in the grid cell the reservoir is
                min_flow = 0.1_r8 * WRMUnit%MeanMthFlow(damID, TWRMctl%month) * theDeltaT
                min_stor = 0.1_r8 * WRMUnit%StorCap(damID)
                !print*, "preregulation", damID, StorWater%storage(damID), flow_vol, flow_res, min_flow, TWRMctl%month

                if ( ( flow_vol + StorWater%storage(damID) - flow_res- evap) >= WRMUnit%StorCap(damID) ) then
                    flow_res =  flow_vol + StorWater%storage(damID) - WRMUnit%StorCap(damID) - evap
                    StorWater%storage(damID) = WRMUnit%StorCap(damID)
                else if ( ( flow_vol + StorWater%storage(damID) - flow_res - evap) < min_stor ) then
                    if ( flow_res<=(flow_vol-evap)) then
                      StorWater%storage(damID) = StorWater%storage(damID) + flow_vol - flow_res - evap
                    else if ( (flow_vol-evap)>=min_flow) then
                       StorWater%storage(damID) = StorWater%storage(damID) 
                       flow_res = flow_vol - evap
                       !print*, "WARNING  No regulation", flow_vol, min_flow, damID
                    else
                       flow_res = flow_vol
                       StorWater%storage(damID) = StorWater%storage(damID) - flow_res + flow_vol - evap
                       if (StorWater%storage(damID) < 0._r8) StorWater%storage(damID) = 0._r8

                    endif
                else
                   StorWater%storage(damID) = StorWater%storage(damID) + flow_vol - flow_res - evap 
                end if

                liqWater%erout(iunit) = -flow_res / (theDeltaT)
                !print*, "regulation", damID, StorWater%storage(damID), flow_vol, flow_res, min_flow

                !! evaporation from the reservoir
                !if ( StorWater%storage(damID) > evap ) then
                !  StorWater%storage(damID) = StorWater%storage(damID) - evap 
                !else
                !  StorWater%storage(damID) = 0._r8
                !  print*, "WARNING, POTENTIAL EVAP LARGER THEN RESERVOIR STORAGE, iunit ", iunit, evap
                !endif
                ! commented out because evap need to be takren into consideration in the releases
	end subroutine Regulation

  subroutine ExtractionRegulatedFlow(iunit, TheDeltaT)
        !! DESCRIPTION: extract water from the reservoir release
        ! !DESCRIPTION: the extraction needs to be distributed accross the dependent unit demand
        !! DESCRIPTION: do not extract more than 10% of the mean monthly flow
                implicit none
                integer, intent(in) :: iunit
                real(r8), intent(in) :: TheDeltaT
                integer, damID, idam, idepend, ct, idemand, ID
                real(r8) :: flow_vol,  min_flow, demand, supply, prorata
                demand = 0._r8
                supply = 0._r8 

                damID = WRMUnit%INVicell(iunit)
                if ( damID > TWRMctl%NDam .OR. damID <= 0 ) then
                  print*, "Error in Regulation with DamID ",damID
                  stop
                end if
                flow_vol = -liqWater%erout(iunit) * theDeltaT
                min_flow = 0.1_r8 * WRMUnit%MeanMthFlow(damID, TWRMctl%month) * theDeltaT
                !min_flow=10
            
               if ( (flow_vol .le. min_flow) .OR. ( WRMUnit%use_Irrig(damID)  .eq. 0  ) .OR. (flow_vol.lt.0.1_r8) ) then
               !if ( (flow_vol .le. min_flow) ) then
                  !print*,"No extraction from regulated flow permitted ", TWRMctl%month, damID 
                else

                  do idam = 1,WRMUnit%dam_Ndepend(damID)
                    !units where the other reservoirs are located
                    idepend = WRMUnit%INVisubw(WRMUnit%dam_depend(damID,idam))
                    if ( idepend < 0 .and. idepend <= Tctl%NUnit ) then
                      print*,"ERROR idepend DATABASE extract from reg. flow ",idepend,damID,idam,WRMUnit%dam_depend(damID,idam),WRMUnit%NUnitID
                       stop
                      end if
                      !pro rated demand - no interannual variability
                      !demand = demand + ( StorWater%demand(idepend) * WRMUnit%StorCap(damID) / WRMUnit%TotStorCapDepend(idepend))
                      !- need adjustement based on storage in each dam
                      !if ( StorWater%Storage(damID) > 0._r8 .and. WRMUnit%TotStorCapDepend(idepend) > 0.1_r8 ) then
                        prorata=1._r8
                        if ( StorWater%Storage(damID)  / WRMUnit%TotStorCapDepend(idepend) < 1._r8 ) then
                          prorata = StorWater%Storage(damID)  / WRMUnit%TotStorCapDepend(idepend)
                        end if
                      !end if

                      demand = demand +  StorWater%demand(idepend) * prorata
                  end do
                  if ( (flow_vol - min_flow) >= demand ) then
                    supply = demand
                  else 
                    if ( flow_vol >= min_flow ) then
                      supply = flow_vol - min_flow
                    end if
                  end if
                  if ( supply .eq. 0._r8  .and. demand > 0._r8) then
                     print*, "no regulation, no extraction ",flow_vol, min_flow, supply, demand
                  end if
                  
                  do idam = 1,WRMUnit%dam_Ndepend(damID)
                    idepend = WRMUnit%INVisubw(WRMUnit%dam_depend(damID,idam))
                    ! pro rated supply
                    if ( idepend > 0 .and. demand > 0._r8 .and. WRMUnit%TotStorCapDepend(idepend) > 0.1_r8 .and. WRMUnit%MeanMthFlow(damID, 13) >= 0.001_r8)  then 
                      prorata=1._r8
                      if ( StorWater%Storage(damID)  / WRMUnit%TotStorCapDepend(idepend) < 1._r8 ) then
                        prorata = StorWater%Storage(damID)  / WRMUnit%TotStorCapDepend(idepend)
                      end if
                      if ( prorata < 0._r8 ) prorata = 0._r8

                      StorWater%supply(idepend)= StorWater%supply(idepend) + (supply/demand * StorWater%demand(idepend) * prorata)
                      StorWater%demand(idepend)= StorWater%demand(idepend) - (supply/demand * StorWater%demand(idepend) * prorata)
                      if ( StorWater%supply(idepend) .lt. 0._r8 ) then
                        print*, "error supply"
                       stop
                      else if ( StorWater%demand(idepend) .lt. 0._r8 ) then
                        print*,"Error demand in first loop", StorWater%demand(idepend), supply/demand
                        stop
                      end if  
                    end if
                  end do

                  flow_vol = flow_vol - supply
                  liqWater%erout(iunit) = - flow_vol / (theDeltaT)
                  
                  supply = 0._r8
                  demand = 0._r8
! SECOND LOOP
                  do idam = 1,WRMUnit%dam_Ndepend(damID)
                     idepend = WRMUnit%INVisubw(WRMUnit%dam_depend(damID,idam))
                     if ( StorWater%demand(idepend) > 0._r8) then
                       demand = demand + StorWater%demand(idepend) ! total demand with no prorata
                     endif
                  end do
                 if ( demand > 0._r8) then
                    if ( (flow_vol - min_flow) >= demand ) then
                      supply = demand
                    else
                      if ( flow_vol > min_flow ) then
                        supply = flow_vol - min_flow
                      end if
                    end if

                  do idam = 1,WRMUnit%dam_Ndepend(damID)
                    idepend = WRMUnit%INVisubw(WRMUnit%dam_depend(damID,idam))
                    ! pro rated supply
                    if ( idepend > 0 .and. StorWater%demand(idepend) > 0._r8 ) then
                      StorWater%supply(idepend)= StorWater%supply(idepend) + (supply/demand * StorWater%demand(idepend))
                      !StorWater%demand(idepend)= StorWater%demand(idepend) - (supply/demand * StorWater%demand(idepend))
                      StorWater%demand(idepend)= StorWater%demand(idepend) * ( 1._r8 - supply/demand )
                      if ( StorWater%demand(idepend) .lt. 0._r8 ) then
                        print*,"Error demand", StorWater%demand(idepend), supply/demand, supply, demand
                        stop
                      end if
                    end if
                  end do
                 endif

! END SECOND LOOP
                  flow_vol = flow_vol - supply
                  liqWater%erout(iunit) = - flow_vol / (theDeltaT)
                  end if

        end subroutine ExtractionRegulatedFlow

end MODULE WRM_modules
