{ uInactivityAlarmTimer
  Copyright (C) 2024 Ekkehard Domning (www.domis.de)

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  A Timer to detect inactivity.
  The German word "Totmanneinrichtung" was translated into english "Dead Man Device",
  a simmilar function was namend "Hold-to-run" or in Australia "Idle-Mode".
  The function could also as a retriggerable power-up-delay.
  Thus, as long within a userdefined time period an activitiy happens the timeout
  is shifted into the future.
  Once the inactivity is detected, an alarm is generated. To return into the normal
  operation the alarm must be manually quitted (shut off).
  The whole thing has an master On-Off switch, which implicit quit the alarm,
  when set to active.

  Example:
  Inactivity Time Out is 5 (.....)
  Time
  Active   _|------------------------------------------|______
  Acivity  ...A..A....A.A........A......A........A...A..A.....
  Alarm   ____________________|----|__________|----|_________
  Event                       E               E
  Quit                             Q               Q

}
unit uInactivityAlarmTimer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;
type
  TInactivityAlarmTimerThread = class;

  TInactivityAlarmQuitKind = (
    { The Alarmstate must be quit manually (by calling the QuitInactivityAlarm Method).
      This will generate one AlarmMessage on Inactivity. Even if the activity return no new
      AlarmMessage is sent until quit.}
    iaqManual,
    { The Alarmstate will be quit automatic by the next activity (by calling AliveTrigger).
      This will generate one AlarmMessage on inactivity. If the activity returns,
      after the next inactivity period a new message is sent.}
    iaqAutoActivity,
    { The Alarmstate will be quit instantly after the AlarmMessage message has been processed.
      This will give an repeated alarm.}
    iaqAutoMessage);

  { TInactivityAlarmTimer }

  TInactivityAlarmTimer = class(TObject)
  private
    FActive : Boolean;
    FLock : TCriticalSection;
    FInactivityTimerThread : TInactivityAlarmTimerThread;
    FInactivityTimeOutMS : Integer;
    FResponseGranularityMS : Integer;
    FLastActivityTick : Int64;
    FInactivityAlarmDetectedEvent : TNotifyEvent;
    FInactivityAlarmQuitKind : TInactivityAlarmQuitKind;
    FAlarmActive : Boolean;
    procedure DoAlert;
    procedure SetActive(Value : Boolean);
    procedure SetInactivityAlarmQuitKind(Value : TInactivityAlarmQuitKind);
    procedure SetInactivityTimeOutMS(Value : Integer);
    procedure SetResponseGranularityMS(Value : Integer);
    procedure LockEnter;inline;
    procedure LockLeave;inline;
  public
    { Active the master enable switch. If false, no alarm is generated.
       If true, the machine is running }
    property Active : Boolean read FActive write SetActive;
    property InactivityAlarmQuitKind : TInactivityAlarmQuitKind read FInactivityAlarmQuitKind write SetInactivityAlarmQuitKind;
    { InactivityTimeOutMS the time after the last activitity (or if no activity after
        Active becomes true) the Alarm is activated }
    property InactivityTimeOutMS : Integer read FInactivityTimeOutMS write SetInactivityTimeOutMS;
    { ResponseGranularityMS the period between two measurements for alarm detection.
        Actually this value is passed to a Sleep()-call. The minimum value is 0 ms,
        the maximum 500 ms, the default 100ms.
        CAUTION: A value of 0 will cause a high CPU-Load! Thus avoid 0 if every possible!
          A value of 1 will do nearly the same job, by consuming only 1% of the CPU load.
        The function Sleep() is called with the property value.
        The ResponseGranularityMS is added to the InactivityTimeOutMS, to avoid
        the Alarm detection caused by a long granularity.
        Caution: Too long times should be avoided, because of the possibly longer
        delay of thread termination. }
    property ResponseGranularityMS : Integer read FResponseGranularityMS write SetResponseGranularityMS;
    { LastActivityTick the timestamp of the last activity (or activation) }
    property LastActivityTick : Int64 read FLastActivityTick;
    { InactivityAlarmDetectedEvent this event is called, when the inactivity alarm
      is detected }
    property InactivityAlarmDetectedEvent : TNotifyEvent read FInactivityAlarmDetectedEvent write FInactivityAlarmDetectedEvent;
    { AlarmActive is True if the alarm is active. To reset the alarm call the
       QuitInactivityAlarm method }
    property AlarmActive : Boolean read FAlarmActive;
    { AliveTrigger, call this method to avoid the inactivity alarm. }
    procedure AliveTrigger;
    { QuitInactivityAlarm, to quit an occoured alarm }
    procedure QuitInactivityAlarm;
    { Create initialize the machine, including the working thread }
    constructor Create;
    { Destroy free the machine, including the working thread }
    destructor Destroy;override;
  end;

  { TInactivityAlarmTimerThread
    does the actual work by manipulation the Owners fields.
    In the case of the alarm detection RingAlarmBell of the Owner is called via
    Synchronize to have the action and event calling running in the main thread.}
  TInactivityAlarmTimerThread = class(TThread)
  private
    FOwner : TInactivityAlarmTimer;
    procedure DoAlertSynched;
  protected
    procedure Execute;override;
  public
    property Owner : TInactivityAlarmTimer read FOwner;
    constructor Create(const AOwner : TInactivityAlarmTimer);
    destructor Destroy;override;
  end;


const
  MinGranularityMS      = 0;
  DefaultGranularityMS  = 100;
  MaxGranularityMS      = 500;

  DefaultInactivityMS   = 1000;

implementation

{ TInactivityAlarmTimer }

procedure TInactivityAlarmTimer.DoAlert;
var
  lEvent : TNotifyEvent;
begin
  FAlarmActive := True;
  // It is essential to minimize the conflict, that the whole thing is shutdown,
  // but an alert is raised. If this happens during program termination
  // the Message might no be processed correct.
  // So in the Destroy procedure of the thread, the notification variable is cleared
  // prior termination.
  // So we copy the event variable to a local variable (this is an atom)
  // and work with the variable. We call the main program only if the thread is
  // not terminated.
  lEvent := FInactivityAlarmDetectedEvent;
  if Assigned(lEvent) and (not FInactivityTimerThread.Terminated) then
    lEvent(Self);
  if FInactivityAlarmQuitKind = iaqAutoMessage then
    QuitInactivityAlarm;
end;

procedure TInactivityAlarmTimer.SetActive(Value: Boolean);
var
  lOldActive : Boolean;
begin
  LockEnter;
  try
    lOldActive := FActive;
    FActive := Value;
    FAlarmActive := False;
    FLastActivityTick := GetTickCount64;
    // If the whole thing was inactive, but is now active
    // resume the thread from sleep
    if FActive and (not lOldActive) then
      FInactivityTimerThread.Suspended := False;
  finally
    LockLeave;
  end;
end;

procedure TInactivityAlarmTimer.SetInactivityAlarmQuitKind(
  Value: TInactivityAlarmQuitKind);
begin
  if FInactivityAlarmQuitKind = Value then Exit;
  LockEnter;
  try
    FInactivityAlarmQuitKind := Value;
  finally
    LockLeave;
  end;
end;

procedure TInactivityAlarmTimer.SetInactivityTimeOutMS(Value: Integer);
begin
  if FInactivityTimeOutMS = Value then Exit;
  LockEnter;
  try
    FInactivityTimeOutMS := Value;
  finally
    LockLeave;
  end;
end;

procedure TInactivityAlarmTimer.SetResponseGranularityMS(Value: Integer);
begin
  if Value < MinGranularityMS then
    Value := MinGranularityMS
  else if Value > MaxGranularityMS then
    Value := MaxGranularityMS;
  if FResponseGranularityMS = Value then Exit;
  LockEnter;
  try
    FResponseGranularityMS := Value;
  finally
    LockLeave;
  end;
end;

procedure TInactivityAlarmTimer.LockEnter;
begin
  FLock.Enter;
end;

procedure TInactivityAlarmTimer.LockLeave;
begin
  FLock.Leave;
end;

procedure TInactivityAlarmTimer.AliveTrigger;
begin
  LockEnter;
  try
    FLastActivityTick := GetTickCount64;
    if (FInactivityAlarmQuitKind = iaqAutoActivity) and
        FAlarmActive then
      FAlarmActive := False;
  finally
    LockLeave;
  end;
end;

procedure TInactivityAlarmTimer.QuitInactivityAlarm;
begin
  AliveTrigger;
  FAlarmActive := False;
end;

constructor TInactivityAlarmTimer.Create;
begin
  inherited Create;
  FInactivityTimeOutMS := DefaultInactivityMS;
  FResponseGranularityMS := DefaultGranularityMS;
  FLock := TCriticalSection.Create;
  FInactivityTimerThread := TInactivityAlarmTimerThread.Create(Self);
end;

destructor TInactivityAlarmTimer.Destroy;
begin
  // Drop the Thread to sleep
  FActive := False;
  // Prohibit notifications from the thread
  FInactivityAlarmDetectedEvent := Nil;
  // Free the thread
  FInactivityTimerThread.Free;
  FLock.Free;
  inherited Destroy;
end;

{ TInactivityAlarmTimerThread }

procedure TInactivityAlarmTimerThread.DoAlertSynched;
begin
  FOwner.DoAlert;
end;

procedure TInactivityAlarmTimerThread.Execute;
var
  lActive : Boolean;
  lInactivityTimeOutMS : Integer;
  lTickGranularityMS : Integer;
  lLastActivityTick : Int64;
  lAlarmActive : Boolean;
  t0, t1 : Int64;
  lLastSleepDuration : Integer;
begin
  try
    lLastSleepDuration := 0;
    while not Terminated do
    begin
      // First collect the onwers state into local variables
      // We do a LockEnter ... LockLeave bracket around, to have all the variables
      // in a defined state, which will additionally not be altered in further processing
      FOwner.LockEnter;
      try
        lActive := FOwner.FActive;
        lAlarmActive := FOwner.FAlarmActive;
        lInactivityTimeOutMS := FOwner.FInactivityTimeOutMS;
        lTickGranularityMS := FOwner.FResponseGranularityMS;
        lLastActivityTick := FOwner.FLastActivityTick;
      finally
        FOwner.LockLeave;
      end;
      // Not Active means, we should go to sleep. But we do this only if not terminated.
      if (not lActive) then
      begin
        if Terminated then Exit;
        Suspended := True;
        // Returning from sleep is caused by setting Active = True.
        // Terminate may becomes true in the meantime too, but this cas
        // is catched in the "while" confition above, so here just "continue".
        Continue;
      end;
      // Here we are active
      // Get the current tick
      t0 := GetTickCount64;
      // calculate the tick where the timeout occur.
      // This is the last activity plus defined timeout.
      // But due to the granularity, the alarm may caused because we
      // where sleeping, so we add the last sleep time, just to be sure.
      t1 := lLastActivityTick+lInactivityTimeOutMS+lLastSleepDuration;
      // If the current tick lies behind the time out, and not terminated and no previous
      // alarm is not quit, then call the synchronization and perform the alert.
      // It is essential to minimize the chance that a termination is in progress
      // and a synchronization is running in the main thread
      if (not Terminated) and (not lAlarmActive) and (t0 >= t1) then
        Synchronize(Self,@DoAlertSynched)
      else
      begin
        // normal operation, sleep a while
        Sleep(lTickGranularityMS);
        // memorize the last sleep time
        lLastSleepDuration := lTickGranularityMS;
      end;
    end;
  finally
    // Make sure to terminate if this procedure is left.
    if not Terminated then
      Terminate;
  end;
end;

constructor TInactivityAlarmTimerThread.Create(const AOwner: TInactivityAlarmTimer);
begin
  // Create suspended
  inherited Create(True);
  if not Assigned(AOwner) then
    raise Exception.Create('AOwner = Nil is not allowed!');
  FOwner := AOwner;
  // The thread starts running, but will suspend, soon, since the owner's property
  // Active is false.
  Suspended := False;
end;

destructor TInactivityAlarmTimerThread.Destroy;
begin
  // Set the terminate flag
  if not Terminated then
    Terminate;
  // wake the thread up
  Suspended := False;
  // Wait for the termination
  WaitFor();
  inherited Destroy;
end;

end.

