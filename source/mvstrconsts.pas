unit mvStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  // mvDE_IntfGraphics
  mvRS_PngJpegExpected = 'PNG/JPG expected.';

  // mvEngine
  mvRS_NoMapProvidersInFile = 'No map providers in file "%s".';
  mvRS_UnknownProvider = 'Unknown Provider: %s';

  // mvGeomath
  mvRS_EastAbbrev = 'E';
  mvRS_NorthAbbrev = 'N';
  mvRS_SouthAbbrev = 'S';
  mvRS_WestAbbrev = 'W';

  // mvGPX
  mvRS_InvalidISO8601Format = 'Invalid ISO8601 format.';

  // mvJobQueue
  mvRS_Timeout = 'Time-out';

  //mvLayersPropEditForm
  mvRS_DeleteCurrentPoint = 'Delete current point';
  mvRS_GPXFileFilter = 'GPX file|*.gpx|All files|*.*';
  mvRS_LoadGPX = 'Load GPX';
  mvRS_Load = 'Load...';
  mvRS_LoadAndZoom = 'Load and zoom';
  mvRS_NewArea = 'New area';
  mvRS_NewPOI = 'New POI';
  mvRS_NewTrack = 'New track';
  mvRS_SaveGPX = 'Save GPX...';
  mvRS_ZoomIn = 'Zoom In';
  mvRS_ZoomOut = 'Zoom Out';

  // mvMapViewer
  mvRS_DifferentProjectionTypeFromBaseMap = '%s has different projection type (%s) from the base map (%s).';
  mvRS_Invisible = 'invisible';
  mvRS_LatitudesOnlyAllowedBetweenPlusMinus90Deg  = 'Latitudes allowed only between +/-90°.';
  mvRS_LongitudesOnlyAllowedBetweenPlusMinus180Deg = 'Longitudes allowed only between +/-180°.';
  mvRS_MapProviderNotRegistered = 'Map provider "%s" is not registered.';
  mvRS_MapProviderNotSelected = 'Map provider is not selected.';
  mvRS_UseOpenStreetmapStandardInstead = 'Use "OpenStreetMap Standard" instead.';

  // mvMapViewerPropEdits
  mvRS_DoesntHaveEditorEnabledInOptions = '%s doesn''t have mvoEditorEnabled set in Options.';
  mvRS_DoYouWantToEnableTheEditor = 'Do you want to enable the editor?';
  mvRS_EnableAndContinue = '&Enable and Continue';
  mvRS_Cancel = '&Cancel';

  // mvMapViewerPathEditForm
  mvRS_Caption = 'Caption:';
  mvRS_DateTime = 'Date/time:';
  mvRS_Elevation = 'Elevation:';
  mvRS_Image = 'Image:';
  mvRS_Latitude = 'Latitude:';
  mvRS_Layer = 'Layer:';
  mvRS_Longitude = 'Longitude:';
  mvRS_Select = 'Select';
  mvRS_Selection = 'Selection';
  mvRS_SelectTool_Hint = 'Select tool';
  mvRS_NewPOI_Hint = 'New point of interest';
  mvRS_NewTrack_Hint = 'New track (non-closed polygon)';
  mvRS_NewArea_Hint = 'New area (closed polygon)';
  mvRS_InsertNewPoint = 'Insert new point';
  mvRS_InsertNewPoint_Hint = 'Insert new point';
  mvRS_DeletePoint = 'Delete point';
  mvRS_DeletePoint_Hint = 'Delete currently selected point';
  mvRS_ZoomIn_Hint = 'Zoom in';
  mvRS_ZoomOut_Hint = 'Zoom out';

  mvRS_SelectDragMode = 'Select/drag mode|Click point to select. CTRL-click to add to selection.';
  mvRS_POIMOde = 'POI mode|Click to add point.';
  mvRS_TrackMode = 'Track mode|Click to add point, CTRL-click to add last point.';
  mvRS_AreaMode = 'Area mode|Click to add point, CTRL-click to add last point.';

  mvRS_None = '(none)';
  mvRS_NotSelected = '(not selected)';
  mvRS_Varying = '(varying)';

  mvRS_ConfirmDeletion = 'Confirm deletion';
  mvRS_AreYouSureYouWantToDelete = 'Are you sure you want to delete "%s"?';
  mvRS_Area = 'area';
  mvRS_Track = 'track';
  mvRS_ClickToAddNew = 'Click on "OK" to add the new %s.';
  mvRS_ClickToDiscardIt = 'Click on "Cancel" to discard it.';
  mvRS_InvalidValue = 'Invalid value.';
  mvRS_More = '%d more';

  // nvMapViewerPropEdits
  mvRS_EditMapViewPoints = 'Edit MapView Points...';
  mvRS_LayerEditor = 'Layer Editor...';

  // mvPluginCommon
  mvRS_FailedToRenameComponents = 'Failed to rename components: %s';
  mvRS_PluginArgumentNil = 'Plugin argument must not be nil';
  mvRS_PluginListCanOnlyStoreDescendantsOf = 'The PluginList can only store descendants of TMvCustomPlugin.';
  mvRS_PluginListInsertNotSupported = 'TMvPluginList.Insert not supported.';

  // mvPluginEditors
  mvRS_AddPlugin = 'Add';
  mvRS_DeletePlugin = 'Delete';
  mvRS_EditPlugin = 'Edit plugin';
  mvRS_PluginUp = 'Up';
  mvRS_PluginDown = 'Down';
  mvRS_OneItem = '1 item';
  mvRS_Items = '%d items';

  // mvPlugins
  mvRS_CenterMarkerPlugin = 'Center marker';
  mvRS_TileInfoPlugin = 'Tile info';
  mvRS_LegalNoticePlugin = 'Legal notice';
  mvRS_LinkedMapsPlugin = 'Linked maps';
  mvRS_UserDefinedPlugin = 'User-defined';

  // mvMarkerPlugins
  mvRS_AreaSelectionNeedsAtLeast3Points = 'For an area, the selection must contain at least 3 points.';
  mvRS_TrackSelectionNeedsAtLeast2Points = 'For a track, the selection must contain at least 2 points.';
  mvRS_DoYouReallyWantToUnselectThePoints = 'Do you really want to unselect the point(s)?';
  mvRS_MarkerHintPlugin = 'Marker hint';
  mvRS_MarkerClickPlugin = 'Marker click';
  mvRS_MarkerEditorPlugin = 'Marker editor';
  mvRS_DraggableMarkerPlugin = 'Draggable marker';

  // mvMapGridPlugin
  mvRS_MapGridPlugin = 'Map grid';

  // mvMapScalePlugin
  mvRS_MapScalePlugin = 'Map scale';

  // mvSpreadMarkerPlugin
  mvRS_MapViewNotFound = 'MapView not found';
  mvRS_SpreadMarkerPlugin = 'Spread marker';

  // mvAreaSelectionPlugin
  mvRS_AreaSelectionPlugin = 'Area selection';

  // mvGreatCirclePainterPlugin
  mvRS_GreatCirclePlugin = 'Great circle painter';

implementation

end.

