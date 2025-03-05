[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/dmirman/gazer/master?urlpath=rstudio)
[![Open in Code Ocean](https://codeocean.com/codeocean-assets/badge/open-in-code-ocean.svg)](https://codeocean.com/capsule/4600160/tree/v1)
gazeR 0.0.1.2


# gazeR:A package to analyze gaze position and pupil size data

For a detailed overview of how to use gazeR, please see the vignettes and posted pre-print for SR Reports (https://psyarxiv.com/gvcxb/) and more generalized processing(https://psyarxiv.com/4qwr8)

<p align="center"><img src="https://user-images.githubusercontent.com/18429968/46034046-472caa80-c0c5-11e8-89c3-ff3f463a1868.jpeg" height="200px" width="200px" />
 
# Synopsis

This package contains functions for reading in raw eye-tracking data, formatting it for (growth curve) analysis, converting from gaze coordinates to areas of interest, binifying and aggregating data, and various helper functions for GCA and plotting. In addition to gaze-position data, it can handle pupillometric and other time course data.

# Installing Package

``` r
# install devtools
install.packages("devtools")

# install gazer from GitHub
remotes::install_github("dmirman/gazer")
``` 

# Help
  
Please use the issues tab (https://github.com/dmirman/gazer/issues) to file any bugs or suggestions.
 
# Updates (March 2025)

Added an upsampling function (`upsample_gaze()`) based on a request fromm user. You can now upsample pupil and gaze values. This was created for the webgazeR package, but thought it could be useful here, especially if you recorded pupil size with low frequency tracker. 

# Updates (March 2022)
 
It has been awhile! I have added some functionality for reading in asc files and pulling in important information to analyze with gazeR. This was with the help of Holger Mitterer! Right now the functionslity is a bit limited (only for events and gaze/VWP), but I am working on extending this. 
 
Also, I am planning to have a function/vignette for reading in data from Tobii Pro Lab and analyzing it with gazeR. I have some functionality showing how to work with Tobii data from E-Prime and the SDK/PsychoPy/Titta, but I am working with folks that use Tobii Pro Lab and think it would be helpful showing how to analyze the data from the actual software. I am not sure when this will happen but I am hoping to get to it soon. 
 
Stay safe and well :)
 
-J 
  
# Updates (April 2021)

I added a function to read in files taken from the Tobii X2-30 and modifies the data to work with gazeR (`merge_tobii`). I collected this data using PsychoPy and the Python Titta package. I am going to slowly work on expanding this function to work with other Tobii trackers. This function is kind of specfic to my needs at the moment but might be helpful for others using a Tobii X2-30 and wanting to use gazeR. 

# Updates (Janurary 2021)

Added Hershman et al.'s (2008) blink detection algo based on pupillary noise. I will be posting a walkthrough on how to use it with gazeR sometime soon. 

# Updates (May 2020)

Added functionality to scale pupil size to dynamic pupil range (Ayasse et al., 2017; Piquado et al., 2010). This requires you to record participant pupil size in bright and dark conditions. 

# Updates (March 2020)

Good news! Gazer is now up on Code Ocean! Click on the Code Ocean button at the top. You can download the capsule to run the package and analyze your data with it. No more worrying about package or OS incompatabilites--it will run on all systems! 

Here is a link showing how to use the gazeR package with Code Ocean: https://www.youtube.com/watch?v=5IQmP9Yrh1k
  
# Updates (Jan 2020)

Happy New Year!! The gazeR package will soon be up and running on Code Ocean! You can download the gazeR capsule and run your own data using our functions from the cloud! 

# Updates (December 2019)
  
 We just resubmitted our manuscript (find it here: https://psyarxiv.com/4qwr8/)! GazeR now has more generalized functionality. 
 
- GazeR can now read in edf files directly
- Baseline by messages
- Modified downsampling function
- Smoothing and interpolation merged into single function

# Updates (September 2019)

- Read in edf files directly 
- Blink/saccades/fixation algorithm 
- Comptability with other eye trackers
- add Binder functionality to run package in the cloud


# Citation
[1] Geller, J., Winn, M. B., Mahr, T., & Mirman, D. (2020). GazeR: A Package for Processing Gaze Position and Pupil Size Data. Behavior Research Methods. 

[2]Hershman, R., Henik, A., & Cohen, N. (2018). A novel blink detection method based on pupillometry noise. Behavior research methods, 50(1), 107–114. https://doi.org/10.3758/s13428-017-1008-1
