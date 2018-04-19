# wm
MOSART-WM - large scale river routing reservoir and water management model

## A geospatial model for assessing and analyzing future energy technology expansion feasibility

## Notice
This repository uses the Git Large File Storage (LFS) extension (see https://git-lfs.github.com/ for details).  Please run the following command before cloning if you do not already have Git LFS installed:
`git lfs install`

## Contact
For questions please contact:

Nathalie Voisin:  <nathalie.voisin@pnnl.gov>

## Overview
WM is a large scale spatially distributed reservoir and water management model which is coupled here with the river routing MOSART model and represents the spatially distributed withdrawals, as wall as withdrawals from river channels and diversion from reservoir releases. River regulation is represented through reservoirs ( up to one per grid cell) . Each reservoir release is independently calibrated using long term mean monthly inflow into the reservoir, long term mon monthly demand associated with this reservoir and reservoir goals (flood control, irrigation, other, combination of flood control with irrigation). Generic monthly pre-release rules and storage targets are set up for individual reservoirs however those releases are updated annually for inter-annual variability ( dry or wet years) and for daily constraints such as environmental flow minimun release, spill and minimum storage.  The WM model allows evaluating the impact of water management over multiple river basins at once ( global, continental scales) and with consistent representation of human operations over the full domain.  
