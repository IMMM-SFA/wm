!
MODULE MOSART_physics_mod
! Description: core code of MOSART. Can be incoporated within any land model via a interface module
! 
! Developed by Hongyi Li, 12/29/2011. 
! REVISION HISTORY:
! Jan 2012, only consider land surface water routing, no parallel computation
!-----------------------------------------------------------------------

! !USES:
	use shr_kind_mod  , only : r8 => shr_kind_r8, SHR_KIND_CL
	use shr_const_mod , only : SHR_CONST_REARTH, SHR_CONST_PI
	use MOSART_type_mod, only : Tctl => ctlSubw, TUnit, liqWater, para
	
	implicit none
	real(r8), parameter :: TINYVALUE = 1.0e-50_r8  ! double precision variable has a significance of about 16 decimal digits
  
								   
! !PUBLIC MEMBER FUNCTIONS:
	contains

	subroutine Euler
	! !DESCRIPTION: solve the ODEs with Euler algorithm
		implicit none    
		
		integer :: iunit, m, k   !local index
		real(r8) :: temp_erout, localDeltaT
		
		if(Tctl%RoutingFlag == 1) then
			do iunit=1,Tctl%NUnit
				call hillslopeRouting(iunit, Tctl%DeltaT)
				liqWater%wh(iunit) = liqWater%wh(iunit) + liqWater%dwh(iunit) * Tctl%DeltaT
				call UpdateState_hillslope(iunit)
				liqWater%etin(iunit) = (-liqWater%ehout(iunit) + liqWater%qsub(iunit)) * TUnit%area(iunit) * TUnit%frac(iunit)
			end do			
			
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
					liqWater%flow(iunit) = liqWater%flow(iunit) - liqWater%erout(iunit)
				end do
			end do
		else
			liqWater%flow = 0._r8
			do m=1,Tctl%DLevelH2R
				do iunit=1,Tctl%NUnit
					liqWater%erlateral(iunit) = (liqWater%qsur(iunit) + liqWater%qsub(iunit)) * TUnit%area(iunit) * TUnit%frac(iunit)
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
					liqWater%flow(iunit) = liqWater%flow(iunit) - liqWater%erout(iunit)
				end do
			end do
		
		end if
		liqWater%flow = liqWater%flow / Tctl%DLevelH2R
	end subroutine Euler

	subroutine hillslopeRouting(iunit, theDeltaT)
	! !DESCRIPTION: Hillslope routing considering uniform runoff generation across hillslope
		implicit none
		
		integer, intent(in) :: iunit
        real(r8), intent(in) :: theDeltaT		
		if(TUnit%fdir(iunit) >= 0 .and. TUnit%hlen(iunit) > 0._r8 ) then
			liqWater%ehout(iunit) = -CREHT(TUnit%hslp(iunit), TUnit%nh(iunit), TUnit%Gxr(iunit), liqWater%yh(iunit))
			if(liqWater%wh(iunit) + (liqWater%qsur(iunit) + liqWater%ehout(iunit)) * theDeltaT < TINYVALUE) then
				liqWater%ehout(iunit) = -(liqWater%qsur(iunit) + liqWater%wh(iunit) / theDeltaT)  
			end if
			liqWater%dwh(iunit) = (liqWater%qsur(iunit) + liqWater%ehout(iunit)) !* TUnit%area(iunit) * TUnit%frac(iunit)
			!liqWater%etin(iunit) = (liqWater%qsur(iunit) + liqWater%qsub(iunit)) * TUnit%area(iunit) * TUnit%frac(iunit)
		end if
	end subroutine hillslopeRouting

	subroutine subnetworkRouting(iunit, theDeltaT)
	! !DESCRIPTION: subnetwork channel routing
		implicit none    
		integer, intent(in) :: iunit      
        real(r8), intent(in) :: theDeltaT		
		if(TUnit%fdir(iunit) >= 0) then
			if(TUnit%tlen(iunit) <= TUnit%hlen(iunit) .or. TUnit%tlen(iunit) <= 0._r8) then ! if no tributaries, not subnetwork channel routing
!			if(TUnit%tlen(iunit) <= 1e100_r8) then ! if no tributaries, not subnetwork channel routing
				liqWater%etout(iunit) = -liqWater%etin(iunit)
			else
				liqWater%vt(iunit) = CRVRMAN(TUnit%tslp(iunit), TUnit%nt(iunit), liqWater%rt(iunit))
				liqWater%etout(iunit) = -liqWater%vt(iunit) * liqWater%mt(iunit)
				if(liqWater%wt(iunit) + (liqWater%etin(iunit) + liqWater%etout(iunit)) * theDeltaT < TINYVALUE) then
					liqWater%etout(iunit) = -(liqWater%etin(iunit) + liqWater%wt(iunit)/theDeltaT)
					if(liqWater%mt(iunit) > 0._r8) then
						liqWater%vt(iunit) = -liqWater%etout(iunit)/liqWater%mt(iunit)
					end if
				end if
			end if
			liqWater%dwt(iunit) = liqWater%etin(iunit) + liqWater%etout(iunit)
		end if
	end subroutine subnetworkRouting

	subroutine mainchannelRouting(iunit, theDeltaT)
	! !DESCRIPTION: main channel routing
		implicit none    
		integer, intent(in) :: iunit      
        real(r8), intent(in) :: theDeltaT		

		if(Tctl%RoutingMethod == 1) then
			call Routing_KW(iunit, theDeltaT)
		else if(Tctl%RoutingMethod == 2) then
			call Routing_MC(iunit, theDeltaT)
		else if(Tctl%RoutingMethod == 3) then
			call Routing_THREW(iunit, theDeltaT)
		else if(Tctl%RoutingMethod == 4) then
			call Routing_DW(iunit, theDeltaT)
		else
			print*, "Please check the routing method! There are only 4 methods available."
		end if

	end subroutine mainchannelRouting

	subroutine Routing_KW(iunit, theDeltaT)
	! !DESCRIPTION: classic kinematic wave routing method
		implicit none    
		
		integer, intent(in) :: iunit      
        real(r8), intent(in) :: theDeltaT		
		integer	:: k
		! estimate the inflow from upstream units
		liqWater%erin(iunit) = 0._r8
	    if(TUnit%fdir(iunit) >= 0 .and. TUnit%iDown(iunit) > 0 ) then
			do k=1,TUnit%nUp(iunit)
				liqWater%erin(iunit) = liqWater%erin(iunit) - liqWater%erout(TUnit%iUp(iunit,k))
			end do
			! estimate the outflow
			if(TUnit%rlen(iunit) <= 0._r8 .or. TUnit%rwidth(iunit) <= 0._r8 .or. TUnit%rwidth(iunit) <=0 .or. TUnit%areaTotal(iunit) <= 0 ) then !no river network, no channel routing
				liqWater%vr(iunit) = 0._r8
				liqWater%erout(iunit) = -liqWater%erin(iunit) - liqWater%erlateral(iunit)
			else
				if(TUnit%areaTotal(iunit)/TUnit%rwidth(iunit)/TUnit%rlen(iunit) > 1e6_r8) then
				    liqWater%erout(iunit) = -liqWater%erin(iunit)-liqWater%erlateral(iunit)
				else
				liqWater%vr(iunit) = CRVRMAN(TUnit%rslp(iunit), TUnit%nr(iunit), liqWater%rr(iunit))
				liqWater%erout(iunit) = -liqWater%vr(iunit) * liqWater%mr(iunit)
				if(-liqWater%erout(iunit) > TINYVALUE .and. liqWater%wr(iunit) + (liqWater%erlateral(iunit) + liqWater%erin(iunit) + liqWater%erout(iunit)) * theDeltaT < TINYVALUE) then
					liqWater%erout(iunit) = -(liqWater%erlateral(iunit) + liqWater%erin(iunit) + liqWater%wr(iunit) / theDeltaT)
					if(liqWater%mr(iunit) > 0._r8) then
						liqWater%vr(iunit) = -liqWater%erout(iunit) / liqWater%mr(iunit)
					end if
				end if
				end if
			end if
			liqWater%dwr(iunit) = liqWater%erlateral(iunit) + liqWater%erin(iunit) + liqWater%erout(iunit)
		end if
	end subroutine Routing_KW

	subroutine Routing_MC(iunit, theDeltaT)
	! !DESCRIPTION: Muskingum-Cunge routing method
		implicit none    
		integer, intent(in) :: iunit      
        real(r8), intent(in) :: theDeltaT		
	 
	end subroutine Routing_MC

	subroutine Routing_THREW(iunit, theDeltaT)
	! !DESCRIPTION: kinematic wave routing method from THREW model
		implicit none    
		integer, intent(in) :: iunit      
        real(r8), intent(in) :: theDeltaT		
	 
	end subroutine Routing_THREW

	subroutine Routing_DW(iunit, theDeltaT)
	! !DESCRIPTION: classic diffusion wave routing method
		implicit none    
		integer, intent(in) :: iunit      
        real(r8), intent(in) :: theDeltaT		
	 
	end subroutine Routing_DW

	subroutine updateState_hillslope(iunit)
	! !DESCRIPTION: update the state variables at hillslope
		implicit none    
		integer, intent(in) :: iunit      
		liqWater%yh(iunit) = liqWater%wh(iunit) !/ TUnit%area(iunit) / TUnit%frac(iunit) 
	end subroutine updateState_hillslope

	subroutine updateState_subnetwork(iunit)
	! !DESCRIPTION: update the state variables in subnetwork channel
		implicit none    
		integer, intent(in) :: iunit      
		if(TUnit%tlen(iunit) > 0._r8 .and. liqWater%wt(iunit) > 0._r8) then
		    liqWater%mt(iunit) = GRMR(liqWater%wt(iunit), TUnit%tlen(iunit)) 
		    liqWater%yt(iunit) = GRHT(liqWater%mt(iunit), TUnit%twidth(iunit))
		    liqWater%pt(iunit) = GRPT(liqWater%yt(iunit), TUnit%twidth(iunit))
		    liqWater%rt(iunit) = GRRR(liqWater%mt(iunit), liqWater%pt(iunit))
		else
		    liqWater%mt(iunit) = 0._r8
		    liqWater%yt(iunit) = 0._r8
		    liqWater%pt(iunit) = 0._r8
		    liqWater%rt(iunit) = 0._r8
		end if
	end subroutine updateState_subnetwork

	subroutine updateState_mainchannel(iunit)
	! !DESCRIPTION: update the state variables in main channel
		implicit none    
		integer, intent(in) :: iunit      

		if(TUnit%rlen(iunit) > 0._r8 .and. liqWater%wr(iunit) > 0._r8 .and. TUnit%rwidth0(iunit) > 0._r8) then
		    liqWater%mr(iunit) = GRMR(liqWater%wr(iunit), TUnit%rlen(iunit)) 
		    liqWater%yr(iunit) = GRHR(liqWater%mr(iunit), TUnit%rwidth(iunit), TUnit%rwidth0(iunit), TUnit%rdepth(iunit))
		    liqWater%pr(iunit) = GRPR(liqWater%yr(iunit), TUnit%rwidth(iunit), TUnit%rwidth0(iunit), TUnit%rdepth(iunit))
		    liqWater%rr(iunit) = GRRR(liqWater%mr(iunit), liqWater%pr(iunit))
		else
		    liqWater%mr(iunit) = 0._r8
		    liqWater%yr(iunit) = 0._r8
		    liqWater%pr(iunit) = 0._r8
		    liqWater%rr(iunit) = 0._r8
		end if
	end subroutine updateState_mainchannel

	subroutine SubTimestep
	! !DESCRIPTION: predescribe the sub-time-steps for channel routing
		implicit none    
		integer :: iunit   !local index
		allocate(TUnit%numDT_r(Tctl%NUnit),TUnit%numDT_t(Tctl%NUnit))
		TUnit%numDT_r = 1
		TUnit%numDT_t = 1
		allocate(TUnit%phi_r(Tctl%NUnit),TUnit%phi_t(Tctl%NUnit))
		TUnit%phi_r = 0._r8
		TUnit%phi_t = 0._r8
		
		do iunit=1,Tctl%NUnit
		    if(TUnit%rlen(iunit) > 0._r8 .and. TUnit%rwidth(iunit)>0._r8 .and. TUnit%areaTotal(iunit) >0._r8) then
		        TUnit%phi_r(iunit) = TUnit%areaTotal(iunit)*sqrt(TUnit%rslp(iunit))/(TUnit%rlen(iunit)*TUnit%rwidth(iunit))
				if(TUnit%phi_r(iunit) >= 10._r8) then
					TUnit%numDT_r(iunit) = (TUnit%numDT_r(iunit)*log10(TUnit%phi_r(iunit))*Tctl%DLevelR) + 1
				else 
				    TUnit%numDT_r(iunit) = TUnit%numDT_r(iunit)*1.0_r8*Tctl%DLevelR + 1
				end if
			end if
			if(TUnit%numDT_r(iunit) < 1) TUnit%numDT_r(iunit) = 1
			
			if(TUnit%tlen(iunit) > 0._r8 .and. TUnit%rwidth(iunit)>0._r8 .and.  TUnit%area(iunit)>0._r8) then
		        TUnit%phi_t(iunit) =      TUnit%area(iunit)*sqrt(TUnit%tslp(iunit))/(TUnit%tlen(iunit)*TUnit%twidth(iunit))
				if(TUnit%phi_t(iunit) >= 10._r8) then 
				    TUnit%numDT_t(iunit) = (TUnit%numDT_t(iunit)*log10(TUnit%phi_t(iunit))*Tctl%DLevelR) + 1
				else 
				    TUnit%numDT_t(iunit) = (TUnit%numDT_t(iunit)*1.0*Tctl%DLevelR) + 1
				end if
		    end if
		    if(TUnit%numDT_t(iunit) < 1) TUnit%numDT_t(iunit) = 1
		end do
		
		
		!open (unit=2, file='../outputs_subw/localTimestep.dat', status="new", action="write")
		!do iunit=1,Tctl%NUnit
		!    write(unit=2,fmt="2(i10)") TUnit%numDT_t(iunit), TUnit%numDT_r(iunit)
		!end do
		!close(unit=1)
		
	end subroutine SubTimestep
		
	function CRVRMAN(slp_, n_, rr_) result(v_)
	! Function for calculating channel velocity according to Manning's equation.
		implicit none
		real(r8), intent(in) :: slp_, n_, rr_ ! slope, manning's roughness coeff., hydraulic radius
		real(r8), intent(out) :: v_                 ! v_ is  discharge
		
		real(r8) :: ftemp
		if(rr_ <= 0._r8) then
			v_ = 0._r8
		else
			ftemp = 2._r8/3._r8
			v_ = (rr_**ftemp) * sqrt(slp_) / n_	
		end if
		return
	end function CRVRMAN

	function CREHT(hslp_, nh_, Gxr_, yh_) result(eht_)
	! Function for overland from hillslope into the sub-network channels
		implicit none
		real(r8), intent(in) :: hslp_, nh_, Gxr_, yh_ ! topographic slope, manning's roughness coeff., drainage density, overland flow depth
		real(r8), intent(out) :: eht_            ! velocity, specific discharge
		
		real(r8) :: vh_
		vh_ = CRVRMAN(hslp_,nh_,yh_)
		eht_ = Gxr_*yh_*vh_
        return
	end function CREHT

	function GRMR(wr_, rlen_) result(mr_)
	! Function for estimate wetted channel area
		implicit none
		real(r8), intent(in) :: wr_, rlen_       ! storage of water, channel length
		real(r8), intent(out) :: mr_             ! wetted channel area
		
		mr_ = wr_ / rlen_
        return
	end function GRMR
	
	function GRHT(mt_, twid_) result(ht_)
	! Function for estimating water depth assuming rectangular channel
		implicit none
		real(r8), intent(in) :: mt_, twid_       ! wetted channel area, channel width
		real(r8), intent(out) :: ht_             ! water depth
		
		if(mt_ <= TINYVALUE) then
		    ht_ = 0._r8
		else
		    ht_ = mt_ / twid_
		end if
        return
	end function GRHT

	function GRPT(ht_, twid_) result(pt_)
	! Function for estimating wetted perimeter assuming rectangular channel
		implicit none
		real(r8), intent(in) :: ht_, twid_       ! water depth, channel width
		real(r8), intent(out) :: pt_             ! wetted perimeter
		
		if(ht_ <= TINYVALUE) then
		    pt_ = 0._r8
		else
		    pt_ = twid_ + 2._r8 * ht_
		end if
        return
	end function GRPT

	function GRRR(mr_, pr_) result(rr_)
	! Function for estimating hydraulic radius
		implicit none
		real(r8), intent(in) :: mr_, pr_         ! wetted area and perimeter
		real(r8), intent(out) :: rr_             ! hydraulic radius
		
		if(pr_ <= TINYVALUE) then
		    rr_ = 0._r8
		else
		    rr_ = mr_ / pr_
		end if
        return
	end function GRRR

	function GRHR(mr_, rwidth_, rwidth0_, rdepth_) result(hr_)
	! Function for estimating maximum water depth assuming rectangular channel and tropezoidal flood plain
	! here assuming the channel cross-section consists of three parts, from bottom to up,
	! part 1 is a rectangular with bankfull depth (rdep) and bankfull width (rwid)
	! part 2 is a tropezoidal, bottom width rwid and top width rwid0, height 0.1*((rwid0-rwid)/2), assuming slope is 0.1
	! part 3 is a rectagular with the width rwid0
		implicit none
		real(r8), intent(in) :: mr_, rwidth_, rwidth0_, rdepth_ ! wetted channel area, channel width, flood plain wid, water depth
		real(r8), intent(out) :: hr_                           ! water depth
		
		real(r8) :: SLOPE1  ! slope of flood plain, TO DO
		real(r8) :: deltamr_
		SLOPE1 = 0.1_r8        ! here give it a small value in order to avoid the abrupt change of hydraulic radidus etc.
                if(mr_ <= TINYVALUE) then
                  hr_ = 0._r8
                else
                  if(mr_ - rdepth_*rwidth_ <= TINYVALUE) then ! not flooded
                    hr_ = mr_/rwidth_
                  else ! if flooded, the find out the equivalent depth
                    if(mr_ > rdepth_*rwidth_ + (rwidth_ + rwidth0_)*SLOPE1*((rwidth0_-rwidth_)/2._r8)/2._r8 + TINYVALUE) then
                      deltamr_ = mr_ - rdepth_*rwidth_ - (rwidth_ + rwidth0_)*SLOPE1*((rwidth0_ - rwidth_)/2._r8)/2._r8;
                      hr_ = rdepth_ + SLOPE1*((rwidth0_ - rwidth_)/2._r8) + deltamr_/(rwidth0_);
                    else
                      deltamr_ = mr_ - rdepth_*rwidth_;
                      hr_ = rdepth_ + (-rwidth_+sqrt(rwidth_**2._r8+4._r8*deltamr_/SLOPE1))*SLOPE1/2._r8
                    end if
                  end if
                end if
        return
	end function GRHR
	
	function GRPR(hr_, rwidth_, rwidth0_,rdepth_) result(pr_)
	! Function for estimating maximum water depth assuming rectangular channel and tropezoidal flood plain
	! here assuming the channel cross-section consists of three parts, from bottom to up,
	! part 1 is a rectangular with bankfull depth (rdep) and bankfull width (rwid)
	! part 2 is a tropezoidal, bottom width rwid and top width rwid0, height 0.1*((rwid0-rwid)/2), assuming slope is 0.1
	! part 3 is a rectagular with the width rwid0
		implicit none
		real(r8), intent(in) :: hr_, rwidth_, rwidth0_, rdepth_ ! wwater depth, channel width, flood plain wid, water depth
		real(r8), intent(out) :: pr_                           ! water depth
		
		real(r8) :: SLOPE1  ! slope of flood plain, TO DO
		real(r8) :: deltahr_
		SLOPE1 = 0.1_r8        ! here give it a small value in order to avoid the abrupt change of hydraulic radidus etc.

		if(hr_ < TINYVALUE) then
		    pr_ = 0._r8
        else
		    if(hr_ <= rdepth_ + TINYVALUE) then ! not flooded
			    pr_ = rwidth_ + 2._r8*hr_
			else
				if(hr_ > rdepth_ + ((rwidth0_-rwidth_)/2._r8)*SLOPE1 + TINYVALUE) then
					deltahr_ = hr_ - rdepth_ - ((rwidth0_-rwidth_)/2._r8)*SLOPE1
					pr_ = rwidth_ + 2._r8*(rdepth_ + ((rwidth0_-rwidth_)/2._r8)*SLOPE1/sin(atan(SLOPE1)) + deltahr_)
				else
					pr_ = rwidth_ + 2._r8*(rdepth_ + (hr_ - rdepth_)/sin(atan(SLOPE1)))
				end if
			end if
		end if
		return
	end function GRPR 
end MODULE MOSART_physics_mod
