[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/dmirman/gazer/master?urlpath=rstudio)
[![Open in Code Ocean](https://codeocean.com/codeocean-assets/badge/open-in-code-ocean.svg)](https://codeocean.com/capsule/4600160/tree/v1)
gazeR 0.2.9


# gazeR:A package to analyze gaze position and pupil size data

For a detailed overview of how to use gazeR, please see the vignettes and posted pre-print for SR Reports (https://psyarxiv.com/gvcxb/) and more generalized processing(https://psyarxiv.com/4qwr8)

<p align="center"><img src="https://user-images.githubusercontent.com/18429968/46034046-472caa80-c0c5-11e8-89c3-ff3f463a1868.jpeg" height="200px" width="200px" />
 
# Synopsis

This package contains functions for reading in raw eye-tracking data, formatting it for (growth curve) analysis, converting from gaze coordinates to areas of interest, binifying and aggregating data, and various helper functions for GCA and plotting. In addition to gaze-position data, it can handle pupillometric and other time course data.

# Installing Package

From [r-universe](https://jgeller112.r-universe.dev) (binaries for Windows and macOS, no build tools needed):

``` r
install.packages("gazer",
                 repos = c("https://jgeller112.r-universe.dev", "https://cloud.r-project.org"))
```

Or from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("dmirman/gazer")
``` 

# Help
  
Please use the issues tab (https://github.com/dmirman/gazer/issues) to file any bugs or suggestions.
 

# Updates (October 2026)

**Install from r-universe.** gazeR is now built on [r-universe](https://jgeller112.r-universe.dev), with binaries for Windows and macOS, so no build tools are needed (see Installing Package above). It also installs with CRAN packages only: saccades (used only by `blink_detect()`) and lme4 (used only by `get_ranef()` and `get_pvalues(method = "KR")`) are now optional.

**New `read_asc()` function.** A fast, single-file reader for EyeLink `.asc` files. It returns one row per sample with messages, blink flags and Experiment Builder trial variables already attached, and needs no folder-per-subject layout. On a 1000 Hz binocular recording it is about 40× faster than `parse_asc()`. The ASC vignette has a new section on it, using a real example file (`inst/extdata/PN010BWS.asc.gz`).

**Bug fixes.** If you used any of these, re-run your pre-processing:

- `smooth_interpolate_pupil()` smoothed across trials: the first and last few samples of each trial (about n/2 for the moving average, 5 for Hanning) were blended with the neighbouring trial or participant. Smoothing is now done within each trial. In practice this rarely changes results, unless your baseline or analysis window includes the first or last ~20 ms of a recording. Some option combinations that always failed (cubic interpolation with `extendblinks = FALSE`) now work.
- `smooth_interpolate_pupil()` with cubic interpolation, `step.first = "interp"` and Hanning smoothing was filtering the un-interpolated signal.
- `lowpass_filter()` pulled the start and end of each filtered trial towards 0, because `signal::filtfilt()` zero-pads. It now centres and reflection-pads the signal first. This is the one with a large effect, so re-run it if you used it.
- `upsample_gaze()` copied values into every gap of other columns (e.g. one event message was repeated on every following sample). Original rows are now unchanged and each message appears once.
- `parse_asc()` can read calibration lines in the file header as samples, producing a few rows with large negative `time` values (often duplicated). These rows can be deleted; `read_asc()` is not affected.

See `NEWS.md` for full details.

**Point-and-click app.** [shiny gazeR](https://github.com/jgeller112/shiny-gazeR) runs gazeR's pupil pipeline in a Shiny app, on your computer (`shinygazer::run_app()`) or [in your browser](https://jgeller112.github.io/shiny-gazeR/).

# Updates (July 2026)

Added binocular support to the ASC workflow. `parse_asc()` now takes a `binocular` argument: when `TRUE` it returns separate x/y/pupil columns for the left and right eye plus per-eye blink flags (`x_left, y_left, pupil_left, x_right, y_right, pupil_right, blink_left, blink_right`); when `FALSE` (default) it keeps the original monocular output. `find_messages_asc()` gained a matching `binocular` argument for interface symmetry (message extraction is eye-independent). The parsed eye-data file is now written as `INPUTFILE_eye.csv` (previously `_vwp.csv`). See the ASC vignette for a binocular example.

# Updates (May 2026)

Fixed `merge_tobii()` function to work with Titta sample and msg reports (.csv) not .h5 files. Also fixed the noise-based blink detection function (`detect_blinks_noise()`).

# Updates (October 2025)

Support for Pupil Labs Neon devices 

# Updates (March 2025)

Added an upsampling function (`upsample_gaze()`) based on a request from user. You can now upsample pupil and gaze values. This was created for the webgazeR package, but thought it could be useful here, especially if you recorded pupil size with low frequency tracker. 

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
