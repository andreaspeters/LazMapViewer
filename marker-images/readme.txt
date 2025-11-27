--------------------------------------------------------------------------------
                        LazMapviewer Marker Images
--------------------------------------------------------------------------------

The images in this folder are intended to be used as markers for 
points-of-interest in the maps displayed by the mapviewer.

There are several motifs 
- dollar
- camera
- clinic
- generic
- pharmacy
- restaurant
- gas station
- train
- euro
- shop
- airport
- hotel
- parking lot
- shopping center
- university
- bar
- post office
- court yard
- hiking trail
- theater

in several colors 
- blue 
- green
- orange
- red

and several sizes
- 32x32
- 48x48
- 64x64
- 96x96

The file name appendix _150, _200, _300 indicates the magnification factor in 
percent of the base image (32x32 px).

To display an image, add a layer to the MapView and a PointOfInterest to the
layer. Enter the GPS location of the point (longitude, latitude) and a
text to be displayed. Specify in the ImageIndex property the image to be shown;
this is the index in an ImageList which should contain all images needed. The
image list must be attached to the POIImages property of the MapView component.

To prepare the image list, set its Width and height Properties to the size an
image should have at standard monitor resolution of 96 ppi. Note that the
default value in TImageList is 16 which results in very small images - better to
set it to 32. In the ImageList editor add the additional resolutions that the
application should support, normally 48 (for 144 ppi), 64 (for 196 ppi). If you
additionally want to support intermediate resolutions, write a handler for the
OnGetWidthForPPI event of the imagelist as described in 
https://wiki.lazarus.freepascal.org/TImageList. Add the images by means of
the ImageList component editor. Finally set the Scaled property of the
ImageList to true to enable scaling.

--------------------------------------------------------------------------------
License
--------------------------------------------------------------------------------
The images were kindly provided by Roland Hahn (https://www.rhsoft.de/).

License: Creative Commons CC0 1.0 Universal
https://creativecommons.org/publicdomain/zero/1.0/
(freely available, no restrictions in usage)
