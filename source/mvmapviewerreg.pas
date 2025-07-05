unit mvMapViewerReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, ComponentEditors;

procedure Register;

implementation

{$R mvmapviewer_icons.res}

uses
  Controls, ImgList,
  mvTypes, mvGeoNames, mvMapViewer, mvDLEFpc, mvDLECache, mvPluginCommon,
  mvMapViewerPropEdits, mvPlugins, mvPluginEditors;

procedure Register;
var
  i: Integer;
begin
  RegisterComponents(PALETTE_PAGE, [TMapView]);
  RegisterComponents(PALETTE_PAGE, [TMvGeoNames]);
  RegisterComponents(PALETTE_PAGE, [TMvDEFpc, TMvDECache]);
  RegisterComponents(PALETTE_PAGE, [TMvPluginManager]);

  RegisterComponentEditor(TMapView, TMapViewComponentEditor);

  RegisterPropertyEditor(TypeInfo(TMapLayers),
    TMapView, 'Layers', TMapLayersPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TMapTrackPoints),
    TMapTrack, 'Points', TMapTrackPointsPropertyEditor);

  RegisterPropertyEditor(TypeInfo(String),
    TMapView, 'MapProvider', TMapProviderPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),
    TMapLayer, 'MapProvider', TMapProviderPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex),
    TMapPointOfInterest, 'ImageIndex', TPointOfInterestImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDateTime),
    TMapPoint, 'DateTime', TPointDateTimePropertyEditor);
  RegisterPropertyEditor(TypeInfo(Double),
    TMapPoint, 'Elevation', TPointElevationPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Double),
    TMapPoint,'Latitude', TLatDMSPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Double),
    TMapPoint,'Longitude', TLonDMSPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Double),
    TMapCenter,'Latitude', TLatDMSPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Double),
    TMapCenter,'Longitude', TLonDMSPropertyEditor);

  RegisterComponentEditor(TMvPluginManager, TMvPluginManagerComponentEditor);
  RegisterPropertyEditor(TypeInfo(TMvPluginList),
    TMvPluginManager, 'PluginList', TMvPluginListPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCaption),
    TLegalNoticePlugin, 'LegalNotice', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCaption),
    TTileInfoPlugin, 'InfoMask', TStringMultilinePropertyEditor);

  for i := 0 to PluginClassRegistry.Count - 1 do
    RegisterNoIcon([TMvCustomPluginClass(PluginClassRegistry.GetClass(i))]);
end;

end.

