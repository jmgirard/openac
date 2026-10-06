# Extract openface features

Extract openface features from a video with potentially multiple faces
using FaceLandmarkVidMulti.exe and the specified arguments.

## Usage

``` r
of_extract(
  infile,
  outfile,
  fp2D = TRUE,
  fp3D = TRUE,
  pdm = FALSE,
  pose = TRUE,
  gaze = TRUE,
  aus = TRUE,
  wild = FALSE,
  multiview = FALSE
)
```

## Arguments

- infile:

  (string) What is the filepath of the video file?

- outfile:

  (string) What filepath (.csv) should the output be written to?

- fp2D:

  (logical, default=TRUE) Should the output include 2D facial landmark
  points (in pixels)?

- fp3D:

  (logical, default=TRUE) Should the output include 3D facial landmark
  points (in millimeters)?

- pdm:

  (logical, default=FALSE) Should the output include the parameter
  estimates of the point distribution model?

- pose:

  (logical, default=TRUE) Should the output include head pose estimates?

- gaze:

  (logical, default=TRUE) Should the output include eye gaze estimates?

- aus:

  (logical, default=TRUE) Should the output include action unit
  estimates?

- wild:

  (logical, default=FALSE) Should the model consider extended search
  regions (for challenging images)?

- multiview:

  (logical, default=FALSE) Should multi-view initialisation be used
  (more robust but slower)?

## Value

A character vector containing openface output. Errors, naming the file,
if OpenFace exits non-zero.

## References

https://github.com/TadasBaltrusaitis/OpenFace/wiki/Command-line-arguments
