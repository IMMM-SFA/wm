!
MODULE WRM_type_mod
! Description: module for public data structure
! 
! Developed by Nathalie Voisin 01/31/2012
! REVISION HISTORY:
!-----------------------------------------------------------------------

! !USES:
  use shr_kind_mod  , only :  r8 => shr_kind_r8, SHR_KIND_CL

  implicit none
  

! control information for subbasin-based representation
  type WRMcontrol_subw
	  integer :: NDam	                ! number of dams
		
      integer :: month            ! month of the simulation
      integer :: year             ! year of the simulation
      integer :: WRMFlag          ! Flag for  using the water resources management model or not
      integer :: ExtractionFlag            ! Flag for whether including Water Demand extraction : 1--> water extraction from each subbasin , uses extraction module ; 0 --> no extraction
      integer :: ExtractionMainChannelFlag ! Flag for whether including Water Demand extraction from the main channel, only if unregulated flow ( bells and whistle here based on RegulationFlag is 1, etc
      integer :: RegulationFlag            ! Flag whether to use reseervoir regulation or not : 1 --> use reservoir module; 0--> natural flow
	
	  character(len=250) :: paraFile    ! the path of the parameter files
	  character(len=250) :: paraPath    ! the path of the parameter files
	  character(len=250) :: demandPath  ! the path of the water demand data
	  character(len=250) :: outPath     ! the path of the output file(s)
      character(len=250) :: damListFile ! name of the file containing Dam list
	  integer, pointer :: out_ID(:)     ! the indices of the outlet subbasins whether the stations are located
 	  character(len=80), pointer :: out_name(:)  ! the name of the outlets  
	  character(len=80) :: curOutlet    ! the name of the current outlet
  end type WRMcontrol_subw
  
	! --- Topographic and geometric properties, applicable for both grid- and subbasin-based representations
	type WRMspatialunit
		! grid properties
		integer , pointer :: mask(:)	  ! mask of a cell, value = reservoir GRanD ID, 0=reservoir (later, can add run-on-the-river
                integer , pointer :: icell(:)   ! subbasin ID the Ndam
                integer , pointer :: INVicell(:) ! give the reservoir ID 1->NDAM located in icell
                integer, pointer :: INVisubw(:) ! need the equivelent for subw for the extraction module
                integer :: NUnitID ! max ID number of the units - needed by WRM
                character(len=100), pointer :: DamName(:)
		integer , pointer :: dam_depend(:,:)	  ! dependence of each dam to subbasins - array of IDs
		integer , pointer :: dam_Ndepend(:)      ! give the number of dependent subbasins to each reservoir
		integer , pointer :: subw_depend(:,:)    ! dependence of each subbasin to a certain number of dams, map IDs
		integer , pointer :: subw_Ndepend(:)     ! number of reservoir from which the subbasin depends               
		real(r8), pointer :: Surfarea(:)	 ! surface area of the reservoir
		real(r8), pointer :: InstCap(:) ! instance energy capacity (MW)
		real(r8), pointer :: StorCap(:) ! maximum storage capacity of the reservoir
		real(r8), pointer :: Height(:)	   ! height of the reservoir
                real(r8), pointer :: Length(:)     ! Length of the reservoir
                real(r8), pointer :: Depth(:)     ! depth of the reservoir
		real(r8), pointer :: MeanMthFlow(:,:)	  ! long term mean monthly flow
		real(r8), pointer :: INVc(:)        !  inverse of c of Biemans 2011 ands Hanasaki 2006 RUnoff/Capacity 
                real(r8), pointer :: TotStorCapDepend(:) ! sum of the reservoir capacities each subw depends on
                real(r8), pointer :: TotInflowDepend(:) ! sum of the total inflow to dependen reservoir each subw depends on

                integer , pointer :: YEAR(:)		! year dam was constructed and operationnal
                integer , pointer :: use_Irrig(:) !reservoir purpose irrigation 
                integer , pointer :: use_Elec(:) !hydropower
                integer , pointer :: use_FCon(:) !flood control
                integer , pointer :: use_Supp(:) !water supply
                 integer , pointer :: use_Fish(:) !fish protection
                 integer , pointer :: use_Navi(:) !use navigation
                 integer , pointer :: use_Rec(:) !use recreation
                real(r8), pointer :: Withdrawal(:) ! ratio of consumptive use over withdrawal - used to calibrate releases
                real(r8), pointer :: Conveyance(:) ! ratio loss / consumptive use - transport efficiency
                integer , pointer :: MthStOp(:) ! month showing the start of the operationnal year
                real(r8), pointer :: StorMthStOp(:) !storage at beginning of the start of the operationnal year
                real(r8), pointer :: MeanMthDemand(:,:) ! longterm mean monthly demand
               ! flood control
               integer , pointer :: MthStFC(:) ! month showing the strat of the flood control
               integer , pointer :: MthNdFC(:) ! month showing the stop of the flood control
               integer , pointer :: MthFCtarget(:) ! month target
               integer , pointer :: MthFCtrack(:) ! track if within FC or not
               real(r8) , pointer :: FCtarget(:) ! storage target
                
	end type WRMspatialunit

	! status and flux variables for liquid water
	type WRMwater
                real(r8), pointer :: TmpStoRelease(:,:,:) ! temporary store released water for 5 days in order to make it
!available for "storage release extraction" . If extraction from main stem then need to worry about environmental
!constraints based on points of extractions along the maoin stem. Here ensure the release is the environmental flow.

		real(r8), pointer :: supply(:)    ! supply of surface water [m3]
		real(r8), pointer :: deficit(:)   ! unmet demand [m3]
                real(r8), pointer :: demand(:)    ! irrigation water demand [m3]
                real(r8), pointer :: storage(:)    ! storage in the reservoir uhits
                real(r8), pointer :: pre_release(:,:) ! pre-release without the interannual fluctutation
                real(r8), pointer :: release(:) ! pre-release with the interannual fluctutation
                real(r8), pointer :: FCrelease (:) !Flood control release to get to storage target
                real(r8), pointer :: pot_evap(:)    ! potential evaporation in mm from the grid
               !real(r8), pointer :: Conveyance (:) !Conveyance loss flux
	end type WRMwater

	! parameters to be calibrated. Ideally, these parameters are supposed to be uniform for one region
	!type WRMparameter
	!end type WRMparameter 

	type (WRMcontrol_subw), public :: ctlSubwWRM
	type (WRMspatialunit) , public :: WRMUnit
	type (WRMwater)     ,   public :: StorWater
	!type (WRMparameter) ,   public :: WRMpara

  
end MODULE WRM_type_mod
