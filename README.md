# wm
### A spatially distributed macro scale water management model to evaluate the impact of water management at continental to global scale, seasonal to inter-annual scale.

## Current Version
[![DOI](https://zenodo.org/badge/130248784.svg)](https://zenodo.org/badge/latestdoi/130248784)

## Notice
This repository uses the Git Large File Storage (LFS) extension (see https://git-lfs.github.com/ for details).  Please run the following command before cloning if you do not already have Git LFS installed:
`git lfs install`
Cloning repositories that use GitLFS seems to work best using `git lfs clone <repo>`

## Contact
For questions please contact:

Nathalie Voisin:  <nathalie.voisin@pnnl.gov>

## Overview
WM is a large scale spatially distributed reservoir and water management model which is coupled here with the river routing MOSART model and represents the spatially distributed withdrawals, as well as withdrawals from river channels and diversion from reservoir releases. River regulation is represented through reservoirs ( up to one per grid cell) . Each reservoir release is independently calibrated using long term mean monthly inflow into the reservoir, long term mon monthly demand associated with this reservoir and reservoir goals (flood control, irrigation, other, combination of flood control with irrigation). Generic monthly pre-release rules and storage targets are set up for individual reservoirs however those releases are updated annually for inter-annual variability ( dry or wet years) and for daily constraints such as environmental flow minimun release, spill and minimum storage.  The WM model allows evaluating the impact of water management over multiple river basins at once ( global, continental scales) and with consistent representation of human operations over the full domain.  

## References
The code was developed :
Voisin, N., Li, H., Ward, D., Huang, M., Wigmosta, M., and Leung, L. R., 2013: On an improved sub-regional water resources management representation for integration into earth system models, Hydrol. Earth Syst. Sci., 17, 3605-3622, doi:10.5194/hess-17-3605-2013, 2013

The dataset and model set ups were used in :
Hejazi MI, N Voisin, L Liu, LM Bramer, DC Fortin, JE Hathaway, M Huang, GP Kyle, LYR Leung, H Li, Y Liu, PL Patel, TC Pulsipher, JS Rice, TK Tesfa, CR Vernon, and Y Zhou.  2015.  "21st Century United States Emissions Mitigation Could Increase Water Stress more than the Climate Change it is Mitigating."  Proceedings of the National Academy of Sciences of the United States of America 112(34):10635-10640.  doi:10.1073/pnas.1421675112
