{ Map Viewer Download Engine Free Pascal HTTP Client accessing on the cache.

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDLECache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvTypes, mvDownloadEngine;

type
  TMVDECache = class(TMVCustomDownloadEngine)
  protected
    procedure InternalDownloadFile(const Url: string; AStream: TStream); override;
  end;

implementation

procedure TMVDECache.InternalDownloadFile(const Url: String; AStream: TStream);
begin
  // Do not download map tiles from the internet --> use only tiles from the cache.
  Unused(Url, AStream);
end;

end.

