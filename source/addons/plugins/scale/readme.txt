--------------------
mvMapScalePlugin.pas
--------------------

The TMapScalePlugin draws a scale bar in the map the indicate the current
zoom magnification. The scale bar is labeled by its length in real-world units.

Note that due to the projection of the earth onto a plane the magnficiation
factor depends on the latitude. In particular at low zoom levels, the magnification
can vary betwen the top and bottom of the displayed map.

Properties
----------

AlignSet: TScaleAlignSet 
  a set of alignment values (alLeft, alTop, alRight, alBottom) determining the
  map edge at which the scale bar is positioned. 
  Including both alLeft and alTop centers the bar horizontally, 
  including alTop and alBottom centers it vertically.
  
Imperial: Boolean 
   Allows to switch between metric (km, m) and imperial (miles, feet) length 
   units
   
SpaceX: Integer
  Distance of the scale bar from  the left or right side of the map

SpaceY: Integer 
  Distance of the scale bar from the top or bottom side of the map
  
WidthMax: Integer 
  Determines the maximum width of the scale bar. The width, however, usually
  is reduced so that the length label has a "nice" value.
  
ZoomMin: Integer 
  Mininum zoom level at which or above which the scale bar is displayed. When
  the current zoom level is smaller than this limit the bar is hidden because 
  the magnfication varies noticeably across the map due to the map projection
  
BackgroundColor: TColor
  Background color of the scale bar. The background is transparent for the
  color clNone, or when the BackgroundOpacity is 0
  
BackgroundOpacity: single
  Determines the transparency of the background of the scale bar. 
  The value can range between 0 (fully transparent) and 1 (fully opaque).
  
Font: TFont
  Determines the font used for the length label of the scale bar
  
Pen: TPen
  Determines the color, width and style of the scale bar line
