{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazMapViewerPkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  mvCache, mvDownloadEngine, mvDragObj, mvEngine, mvGeoNames, mvGpsObj, 
  mvJobQueue, mvJobs, mvMapProvider, mvTypes, mvMapViewer, mvExtraData, 
  mvDLEFpc, mvMapViewerReg, mvGPX, mvDrawingEngine, mvDE_IntfGraphics, 
  mvDLEWin, mvMapViewerPropEdits, mvLayersPropEditForm, mvGeoMath, 
  mvMapViewerPathEditForm, mvMapViewerPathEditDsgForm, mvDLECache, 
  mvPluginEditors, mvClassRegistration, mvPluginCommon, mvPlugins, 
  mvspreadmarker_plugin, uInactivityAlarmTimer, mvMapGridPlugin, 
  mvMapScalePlugin, mvGreatCirclePainterPlugin, mvAreaSelectionPlugin, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('mvMapViewerReg', @mvMapViewerReg.Register);
end;

initialization
  RegisterPackage('lazMapViewerPkg', @Register);
end.
