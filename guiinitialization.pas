unit GUIInitialization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Math, CastleUIState, Forms, Controls, Graphics, Dialogs,
  {$ifndef windows}
  CastleFilesUtils,
  {$endif}
  ExtCtrls, ComCtrls, StdCtrls, CastleControl, MainGameUnit, CastleControls,
  CastleColors, CastleUIControls, CastleTriangles, CastleShapes, CastleVectors,
  CastleViewport, CastleCameras, X3DNodes, X3DFields, X3DTIme, CastleImages,
  CastleGLImages, CastleApplicationProperties, CastleLog, CastleTimeUtils,
  CastleKeysMouse;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    TrackUnits: TComboBox;
    InfoPanel: TPanel;
    PositionLabel: TLabel;
    UpperPanel: TPanel;
    PositionPanel: TPanel;
    ContainerPanel: TPanel;
    LowerPanel: TPanel;
    VideoPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    PositionTrackBar: TTrackBar;
    Window: TCastleControlBase;
    procedure ContainerPanelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure Splitter1CanOffset(Sender: TObject; var NewOffset: Integer;
      var Accept: Boolean);
    procedure TrackUnitsChange(Sender: TObject);
    procedure WindowBeforeRender(Sender: TObject);
    procedure WindowClose(Sender: TObject);
    procedure WindowOpen(Sender: TObject);
    procedure UpdatePosition;
  end;

var
  CastleForm: TCastleForm;

implementation
{$R *.lfm}

procedure TCastleForm.FormCreate(Sender: TObject);
begin
  MinimumFPS := 999999;
  RecordedMinimumFPS := False;
  {$ifdef windows}
  WorkingDirectory := 'C:\backup\vids\tcc\tcc_znr_set_open\720p'; // Temp
  {$else}
  WorkingDirectory := HomePath + 'tcc_znr_set_open/720p'; // Temp
  {$endif}
  WriteLnLog('FormCreate : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$ifdef darwin}
  WindowState := wsFullScreen;
  {$endif}
  AppTime := CastleGetTickCount64;
  MinFrame := 1; // Temp
  MaxFrame := 45062; // Temp
  FrameDiff := 0; // Temp
  FrameCounter := 19369; // Temp
  FramesPerTC := 24000; // Temp
  CountsPerTC := 1001; // Temp
  UpdatePosition;
  PrepDone := False;
  Profiler.Enabled := true;
  InitializeLog;
  KeyPreview := True;
  Caption := 'MTGVideo';
end;

procedure TCastleForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_LEFT) then
    begin
      if ssShift in Shift then
        FrameDiff := -6
      else if ssCtrl in Shift then
        FrameDiff := -12
      else if ssAlt in Shift then
        FrameDiff := -24
      else
        FrameDiff := -1;
      Key := 0;
    end;
  if (Key = VK_RIGHT) then
    begin
      if ssShift in Shift then
        FrameDiff := 6
      else if ssCtrl in Shift then
        FrameDiff := 12
      else if ssAlt in Shift then
        FrameDiff := 24
      else
        FrameDiff := 1;
      Key := 0;
    end;
end;

procedure TCastleForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_LEFT) then
    begin
      if FrameDiff < 0 then
        FrameDiff := 0;
      Key := 0;
    end;
  if (Key = VK_RIGHT) then
    begin
      if FrameDiff > 0 then
        FrameDiff := 0;
      Key := 0;
    end;
end;

procedure TCastleForm.UpdatePosition;
var
  FrameInMs: Single;
begin
  if TrackUnits.ItemIndex = 0 then
    PositionLabel.Caption := IntToStr(FrameCounter)
  else
    begin
      FrameInMs := FrameCounter * (CountsPerTC / FramesPerTC);
      PositionLabel.Caption := FormatFloat('#####0.000', FrameInMs);
    end;
end;

procedure TCastleForm.FormResize(Sender: TObject);
begin
  PositionPanel.Height := PositionTrackBar.Height;
end;

procedure TCastleForm.Splitter1CanOffset(Sender: TObject;
  var NewOffset: Integer; var Accept: Boolean);
begin

end;

procedure TCastleForm.TrackUnitsChange(Sender: TObject);
begin
  UpdatePosition;
end;

procedure TCastleForm.WindowBeforeRender(Sender: TObject);
begin

end;

procedure TCastleForm.FormDestroy(Sender: TObject);
begin
  WriteLnLog('FormDestroy : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleForm.ContainerPanelClick(Sender: TObject);
begin

end;

procedure TCastleForm.WindowOpen(Sender: TObject);
begin
  WriteLnLog('WindowOpen : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  RenderReady := False;
  TCastleControlBase.MainControl := Window;
  CastleApp := TCastleApp.Create(Application);
  TUIState.Current := CastleApp;
  Window.Container.UIScaling := usDpiScale;
end;

procedure TCastleForm.WindowClose(Sender: TObject);
begin
  WriteLnLog('WindowClose : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

end.

