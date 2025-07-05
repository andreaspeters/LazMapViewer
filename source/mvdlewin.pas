{ Map Viewer Download Engine Free Pascal HTTP Client

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDLEWin;

{$mode objfpc}{$H+}

interface

{$IFDEF MSWindows}

uses
  Classes, SysUtils,
  mvDownloadEngine;

type
  TMVDEWin = class(TMvCustomDownloadEngine)
  private
    FUseSystemProxy: Boolean;
    FUseProxy: Boolean;
    FProxyHost: string;
    FProxyPort: Word;
    FProxyUserName: String;
    FProxyPassWord: String;
  protected
    procedure InternalDownloadFile(const Url: string; AStream: TStream); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetProxy(AUseSystemProxy, AUseProxy: Boolean; AProxyHost: String;
      AProxyPort: Word; AProxyUserName, AProxyPassword: String); override;
    property UseProxy: Boolean read FUseProxy write FUseProxy default false;
    property UseSystemProxy: Boolean read FUseSystemProxy write FUseSystemproxy default true;
    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: Word read FProxyPort write FProxyPort;
    property ProxyUsername: String read FProxyUserName write FProxyUserName;
    property ProxyPassword: String read FProxyPassword write FProxyPassword;
  end;

{$ENDIF}

implementation

{$IFDEF MSWindows}
uses
  windows, wininet;

constructor TMVDEWin.Create(AOwner: TComponent);
begin
  inherited;
  FProxySupport := true;
  FSystemProxySupport := true;
  FUseSystemProxy := true;
end;

procedure TMVDEWin.InternalDownloadFile(const Url: string; AStream: TStream);
const
  KB = 1024;
var
  netHandle: HInternet;
  urlHandle: HInternet;
  buffer: array[0..4*KB-1] of Char;
  bytesRead: dWord = 0;
  header: String;
  accessType: Integer;
  proxy: String = '';
  username: WideString = '';
  pwd: WideString = '';
begin
  if FUseSystemProxy then
    accessType := INTERNET_OPEN_TYPE_PRECONFIG
  else
  begin
    accessType := INTERNET_OPEN_TYPE_DIRECT;
    if FUseProxy then
    begin
      if FProxyPort <> 0 then
        proxy := Format('%s:%d', [FProxyHost, FProxyPort])
      else
        proxy := FProxyHost;
      if proxy <> '' then
        accessType := INTERNET_OPEN_TYPE_PROXY;
    end;
  end;
  netHandle := InternetOpen('Mozilla/5.0(compatible; WinInet)', accessType, PChar(proxy), nil, 0);

  // NetHandle valid?
  if netHandle = nil then
    exit;

  if FUseProxy then
  begin
    if FProxyUsername <> '' then
    begin
      userName := WideString(FProxyUsername);
      InternetSetOptionW(netHandle, INTERNET_OPTION_PROXY_USERNAME, PWideChar(userName), Length(userName));
    end;
    if FProxyPassword <> '' then
    begin
      pwd := WideString(FProxyPassword);
      InternetSetOptionW(netHandle, INTERNET_OPTION_PROXY_PASSWORD, PWideChar(pwd), Length(pwd));
    end;
  end;

  try
    header := '';
    urlHandle := InternetOpenUrl(netHandle, PChar(URL), PChar(header), Length(header), INTERNET_FLAG_RELOAD, 0);

    // UrlHandle valid?
    if urlHandle = nil then
      exit;

    try
      repeat
        InternetReadFile(urlHandle, @buffer, SizeOf(buffer), bytesRead);
        if bytesRead > 0 then
          AStream.Write(buffer, bytesRead);
      until bytesRead = 0;
      AStream.Position := 0;
    finally
      InternetCloseHandle(urlHandle);
    end
  finally
    InternetCloseHandle(netHandle);
  end;
end;

procedure TMvDEWin.SetProxy(AUseSystemProxy, AUseProxy: Boolean; AProxyHost: String;
  AProxyPort: Word; AProxyUserName, AProxyPassword: String);
begin
  FUseSystemProxy := AUseSystemProxy;
  FUseProxy := AUseProxy;
  FProxyHost := AProxyHost;
  FProxyPort := AProxyPort;
  FProxyUserName := AProxyUserName;
  FProxyPassword := AProxyPassword;
end;

{$ENDIF}

end.

