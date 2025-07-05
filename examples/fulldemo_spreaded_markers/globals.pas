unit globals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvGeoMath, mvEngine;

type
  TPOIMode = (pmDefaultDrawing, pmDefaultImage, pmImageList, pmCustomDrawing);

const
  MAP_PROVIDER_FILENAME = 'map-providers.xml';

const
  DistanceUnit_Names: array[TDistanceUnits] of string = ('m', 'km', 'miles');
var
  DistanceUnit: TDistanceUnits = duKilometers;

implementation

end.

