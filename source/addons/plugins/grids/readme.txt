-----------------
mvGridPlugins.pas
-----------------

The TGridPlugin draws a grid of lines at constant and equidistant latitudes and
longitudes. The lines can be labeled at both sides by the longitude/latitude
values.

Properties
----------

BackgroundColor: TColor 
  Color of the (semi-transparent) label background

Font: TFont
  Font to be used to draw the labels at the grid lines

LabelDistacne: Integer
  Distance of the labels from the map edges
  
LabelPositions: TMvGridLabelPositions
  a set of [glpLeft, glpTop, glpRight, glpBottom] indicating the map sides 
  at which the labels are drawn
    
Increment: Double
  Distance between the grid lines, in (fractional) degress
  The same value Will be applied to both equal-longitude and equal-latitude lines
  Note that the distance between equal-latitude lines will grow towards the
  poles, due to the projection of the map.
  The default value, 0, instructs the plugin to calculate the increment 
  automatically (determined by MaxDistance and MinDistance parameters).
  
MaxDistance: Integer
  The maximum distance between vertical grid lines in pixels limiting the 
  automatic calculation of the Increment parameter.
  
MinDistance: Integer
  The minimum distance between vertical grid lines in pixels limiting the 
  automatic calculation of the Increment parameter.
  
Opacity: Single
  Determines the transparency of the label background 
  (0 = transparent, 1 = opaque)
  
Pen: TPen
  Determines the color, width and style of the grid lines
  
