!
MODULE MOSART_type_mod
! Description: module for public data structure
! 
! Developed by Hongyi Li, 12/29/2011. 
! REVISION HISTORY:
! Jan 2012, only consider land surface water routing, no parallel computation
!-----------------------------------------------------------------------

! !USES:
  use shr_kind_mod  , only : r8 => shr_kind_r8, SHR_KIND_CL
  use shr_const_mod , only : SHR_CONST_REARTH, SHR_CONST_PI

  implicit none
  
! constrol information for grid-based representation
  type Tcontrol_grid
      integer :: ncols                  ! number of columns
      integer :: nrows                  ! number of rows
      real(r8) :: xllcorner             ! longitude of the left bottom corner
      real(r8) :: yllcorner             ! latitude of the left bottom corner
      real(r8) :: cellsize              ! size of the grid
	  integer :: NUnit	                ! numer of Grides in the model domain, which is equal to the number of cells, nrows*ncols
		
	  integer :: NSTART                 ! the # of the time step to start the routing. Previous NSTART - 1 steps will be passed over.
	  integer :: NSTEPS			        ! number of time steps specified in the modeling
	  integer :: NWARMUP			    ! time steps for model warming up
   
	  real(r8) :: DATAH				    ! time step of runoff generation in second provided by the user
      integer :: num_dt                 ! number of sub-steps within the current step interval, 
                                        ! i.e., if the time step of the incoming runoff data is 3-hr, and num_dt is set to 10, 
						                ! then deltaT = 3*3600/10 = 1080 seconds
      real(r8) :: DeltaT                ! Time step in seconds 
	  integer :: Restart                ! flag, Restart=1 means starting from the state of last run, =0 means starting from model-inset initial state.
	  integer :: RoutingMethod          ! Flag for routing methods. 1 --> variable storage method from SWAT model; 2 --> Muskingum method?
      integer :: RoutingFlag            ! Flag for whether including hillslope and sub-network routing. 1--> include routing through hillslope, sub-network and main channel; 0--> main channel routing only.
      integer :: batchProcessing        ! Flag for whether batchprocess multiple basins, 1--> yes, 0--> not
      integer :: numStation             ! number of basins to be simulated
      character(len=200) :: staListFile ! name of the file containing station list
	
      character(len=100) :: baseName    ! name of the case study, e.g., columbia
	  character(len=200) :: ctlFile     ! the name of the control file
	  character(len=200) :: ctlPath     ! the path of the control file
	  character(len=200) :: paraFile    ! the path of the parameter files
	  character(len=200) :: paraPath    ! the path of the parameter files
	  character(len=200) :: runoffPath  ! the path of the runoff data
	  character(len=200) :: outPath     ! the path of the output file(s)
	  character(len=80)  :: StationList ! the file of station list;
	  integer, pointer :: out_nlat(:)   ! the row number of the outlet cell in the cell matrix
	  integer, pointer :: out_nlon(:)   ! the column number of the outlet cell in the cell matrix
 	  character(len=180), pointer :: out_name(:)  ! the name of the outlets  
	  character(len=180) :: curOutlet    ! the name of the current outlet
	  character(len=100) :: maskPath    ! the path of the watershed mask files, here watershed means the controlling area of a flow station
	  character(len=200) :: maskFile    ! full name of the watershed mask files, here watershed means the controlling area of a flow station
  end type Tcontrol_grid

! constrol information for subbasin-based representation
  type Tcontrol_subw
	  integer :: NUnit	                ! numer of Grides in the model domain, which is equal to the number of cells, nrows*ncols
		
	  integer :: NSTART                 ! the # of the time step to start the routing. Previous NSTART - 1 steps will be passed over.
	  integer :: NSTEPS			        ! number of time steps specified in the modeling
	  integer :: NWARMUP			    ! time steps for model warming up
	  real(r8) :: DATAH				    ! time step of runoff generation in second provided by the user
      integer :: num_dt                 ! number of sub-steps within the current step interval, 
                                        ! i.e., if the time step of the incoming runoff data is 3-hr, and num_dt is set to 10, 
						                ! then deltaT = 3*3600/10 = 1080 seconds
      real(r8) :: DeltaT                ! Time step in seconds 
	  integer :: DLevelH2R              ! The base number of channel routing sub-time-steps within one hillslope routing step. 
	                                    ! Usually channel routing requires small time steps than hillslope routing.
	  integer :: DLevelR                ! The number of channel routing sub-time-steps at a higher level within one channel routing step at a lower level. 
	  integer :: Restart                ! flag, Restart=1 means starting from the state of last run, =0 means starting from model-inset initial state.
	  integer :: RoutingMethod          ! Flag for routing methods. 1 --> variable storage method from SWAT model; 2 --> Muskingum method?
      integer :: RoutingFlag            ! Flag for whether including hillslope and sub-network routing. 1--> include routing through hillslope, sub-network and main channel; 0--> main channel routing only.
	
      character(len=100) :: baseName    ! name of the case study, e.g., columbia
	  character(len=200) :: ctlFile     ! the name of the control file
	  character(len=100) :: ctlPath     ! the path of the control file
	  character(len=200) :: paraFile    ! the path of the parameter files
	  character(len=100) :: paraPath    ! the path of the parameter files
	  character(len=100) :: runoffPath  ! the path of the runoff data
	  character(len=100) :: outPath     ! the path of the output file(s)
      integer :: numStation             ! number of basins to be simulated
      character(len=200) :: staListFile ! name of the file containing station list
	  integer, pointer :: out_ID(:)     ! the indices of the outlet subbasins whether the stations are located
 	  character(len=80), pointer :: out_name(:)  ! the name of the outlets  
	  character(len=80) :: curOutlet    ! the name of the current outlet
  end type Tcontrol_subw
  
	! --- Topographic and geometric properties, applicable for both grid- and subbasin-based representations
	type Tspatialunit
		! grid properties
		integer , pointer :: mask(:,:)	  ! mask of a cell, 1=land, 2=ocean, 0=excluded cell
		integer , pointer :: icell(:,:)	  ! indices of cells in the above 2D matrix. This is used to map between the above 2D and following 1D arrays
		                                  ! ** the above two arrays are 2D for grid-based representation, and 1D for subbasin-based representation
		real(r8), pointer :: lat(:)       ! latitude of the centroid of the cell
		real(r8), pointer :: lon(:)       ! longitude of the centroid of the cell
		real(r8), pointer :: area(:)	  ! area of local cell, [m2]
		real(r8), pointer :: areaTotal(:) ! total upstream drainage area, [m2]
		real(r8), pointer :: rlenTotal(:) ! length of all reaches, [m]
		real(r8), pointer :: Gxr(:)	      ! drainage density within the cell, [1/m]
		real(r8), pointer :: frac(:)	  ! fraction of cell included in the study area, [-]
		! hillslope properties
		real(r8), pointer :: nh(:)        ! manning's roughness of the hillslope (channel network excluded) 
		real(r8), pointer :: hslp(:)	  ! slope of hillslope, [-]
		real(r8), pointer :: hlen(:)	  ! length of hillslope within the cell, [m] 
		! subnetwork channel properties
		real(r8), pointer :: tslp(:)	  ! average slope of tributaries, [-]
		real(r8), pointer :: tlen(:)	  ! length of all sub-network reach within the cell, [m] 
		real(r8), pointer :: twidth(:)	  ! bankfull width of the sub-reach, [m]
		real(r8), pointer :: nt(:)        ! manning's roughness of the subnetwork at hillslope  
		! main channel properties
		integer , pointer :: fdir(:)      ! flow direction, currently considering single direction only;
		real(r8), pointer :: rlen(:)	  ! length of main river reach, [m]
		real(r8), pointer :: rslp(:)	  ! slope of main river reach, [m]
		real(r8), pointer :: rwidth(:)	  ! bankfull width of main reach, [m]
		real(r8), pointer :: rwidth0(:)	  ! total width of the flood plain, [m]
		real(r8), pointer :: rdepth(:)	  ! bankfull depth of river cross section, [m]
		real(r8), pointer :: nr(:)        ! manning's roughness of the main reach
		integer , pointer :: iDown(:)     ! IDs of the downstream units, corresponding to the subbasin ID in the input table
		integer , pointer :: nUp(:)       ! number of upstream units, maximum 8
		integer , pointer :: iUp(:,:)     ! IDs of upstream units, corresponding to the subbasin ID in the input table
		
		integer , pointer :: indexDown(:) ! indices of the downstream units in the ID array. sometimes subbasins IDs may not be continuous
		
		integer , pointer :: numDT_r(:)   ! for a main reach, the number of sub-time-steps needed for numerical stability
		integer , pointer :: numDT_t(:)   ! for a subnetwork reach, the number of sub-time-steps needed for numerical stability
		real(r8), pointer :: phi_r(:)     ! the indicator used to define numDT_r
		real(r8), pointer :: phi_t(:)     ! the indicator used to define numDT_t
	end type Tspatialunit

	! status and flux variables for liquid water
	type Twater
		! hillsloope
		!! states
		real(r8), pointer :: wh(:)        ! storage of surface water, [m3]
		real(r8), pointer :: dwh(:)       ! change of water storage, [m3]
		real(r8), pointer :: yh(:)        ! depth of surface water, [m]
		real(r8), pointer :: wsat(:)      ! storage of surface water within saturated area at hillslope [m]
		real(r8), pointer :: wunsat(:)    ! storage of surface water within unsaturated area at hillslope [m]
		real(r8), pointer :: qhorton(:)	  ! Infiltration excess runoff generated from hillslope, [m/s]
		real(r8), pointer :: qdunne(:)	  ! Saturation excess runoff generated from hillslope, [m/s]
		real(r8), pointer :: qsur(:)	  ! Surface runoff generated from hillslope, [m/s]
		real(r8), pointer :: qsub(:)	  ! Subsurface runoff generated from hillslope, [m/s]
		!! fluxes
		real(r8), pointer :: ehout(:)	  ! overland flow from hillslope into the sub-channel, [m/s]
		real(r8), pointer :: asat(:)	  ! saturated area fraction from hillslope, [-]
		real(r8), pointer :: esat(:)	  ! evaporation from saturated area fraction at hillslope, [m/s]
		! subnetwork channel
		!! states
		real(r8), pointer :: tarea(:)	  ! area of channel water surface, [m2]
		real(r8), pointer :: wt(:)        ! storage of surface water, [m3]
		real(r8), pointer :: dwt(:)       ! change of water storage, [m3]
		real(r8), pointer :: yt(:)		  ! water depth, [m]
		real(r8), pointer :: mt(:)		  ! cross section area, [m2]
		real(r8), pointer :: rt(:)		  ! hydraulic radii, [m]
		real(r8), pointer :: pt(:)		  ! wetness perimeter, [m]
		real(r8), pointer :: vt(:)		  ! flow velocity, [m/s]
		real(r8), pointer :: tt(:)		  ! mean travel time of the water within the channel, [s]
		!! fluxes
		real(r8), pointer :: tevap(:)     ! evaporation, [m/s]
		real(r8), pointer :: etin(:)	  ! lateral inflow from hillslope, including surface and subsurface runoff generation components, [m3/s]
		real(r8), pointer :: etout(:)     ! discharge from sub-network into the main reach, [m3/s]
		! main channel
		!! states
		real(r8), pointer :: rarea(:)	  ! area of channel water surface, [m2] 
		real(r8), pointer :: wr(:)        ! storage of surface water, [m3]
		real(r8), pointer :: dwr(:)       ! change of water storage, [m3]
		real(r8), pointer :: yr(:)		  ! water depth. [m]
		real(r8), pointer :: mr(:)		  ! cross section area, [m2]
		real(r8), pointer :: rr(:)		  ! hydraulic radius, [m]
		real(r8), pointer :: pr(:)		  ! wetness perimeter, [m]
		real(r8), pointer :: vr(:)		  ! flow velocity, [m/s]
		real(r8), pointer :: tr(:)		  ! mean travel time of the water within the channel, [s]
		!! exchange fluxes
		real(r8), pointer :: erlg(:)      ! evaporation, [m/s]
		real(r8), pointer :: erlateral(:) ! lateral flow from hillslope, including surface and subsurface runoff generation components, [m3/s]
		real(r8), pointer :: erin(:)	  ! inflow from upstream links, [m3/s]
		real(r8), pointer :: erout(:)	  ! outflow into downstream links, [m3/s]
		real(r8), pointer :: flow(:)	  ! streamflow from the outlet of the reach, [m3/s]
		real(r8), pointer :: erin1(:)     ! inflow from upstream links during previous step, used for Muskingum method, [m3/s]
		real(r8), pointer :: erin2(:)     ! inflow from upstream links during current step, used for Muskingum method, [m3/s]
		!! for Runge-Kutta algorithm
		real(r8), pointer :: wrtemp(:)    ! temporary storage item, for 4th order Runge-Kutta  algorithm;
		real(r8), pointer :: erintemp(:)  ! 
		real(r8), pointer :: erouttemp(:)
		real(r8), pointer :: k1(:)
		real(r8), pointer :: k2(:)
		real(r8), pointer :: k3(:)
		real(r8), pointer :: k4(:)    	
	end type Twater

	! parameters to be calibrated. Ideally, these parameters are supposed to be uniform for one region
	type Tparameter
		real(r8), pointer :: c_nr(:)       ! coefficient to adjust the manning's roughness of channels
		real(r8), pointer :: c_nh(:)       ! coefficient to adjust the manning's roughness of overland flow across hillslopes
		real(r8), pointer :: c_twid(:)     ! coefficient to adjust the width of sub-reach channel
	end type Tparameter 

	type (Tcontrol_grid), public :: ctlGrid
	type (Tcontrol_subw), public :: ctlSubw
	type (Tspatialunit) , public :: TUnit
	type (Twater)     ,   public :: liqWater
	type (Tparameter) ,   public :: para

  
end MODULE MOSART_type_mod
