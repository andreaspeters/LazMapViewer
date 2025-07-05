{ Map Viewer Download Engine
  Copyright (C) 2011 Maciej Kaczkowski / keit.co

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDownloadEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMvCustomDownloadEngine }

  TMvCustomDownloadEngine = class(TComponent)
  protected
    FProxySupport: Boolean;
    FSystemProxySupport: Boolean;
    procedure InternalDownloadFile(const Url: String; AStream: TStream); virtual; abstract;
    procedure LoadFromLocalFile(const AFileName: String; AStream: TStream);
  public
    procedure DownloadFile(const Url: string; AStream: TStream); virtual;
    function EncodeURLElement(const s: String): String; virtual;
    procedure SetProxy(AUseSystemProxy, AUseProxy: Boolean; AProxyHost: String;
      AProxyPort: Word; AProxyUserName, AProxyPassword: String); virtual; abstract;
    property HasProxySupport: Boolean read FProxySupport;
    property SupportsSystemProxy: Boolean read FSystemProxySupport;
  end;


implementation

uses
  URIParser;

{ TMvCustomDownloadEngine }

procedure TMvCustomDownloadEngine.DownloadFile(const Url: string; AStream: TStream);
var
  fn: String;
begin
  if URIToFileName(Url, fn) then
    LoadFromLocalFile(fn, AStream)
  else
    InternalDownloadFile(Url, AStream);
end;

function TMvCustomDownloadEngine.EncodeURLElement(const s: String): String;
begin
  Result := s;
end;

procedure TMvCustomDownloadEngine.LoadFromLocalFile(const AFileName: String;
  AStream: TStream);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    AStream.CopyFrom(fs, fs.Size);
    AStream.Position := 0;
  finally
    fs.Free;
  end;
end;

end.

