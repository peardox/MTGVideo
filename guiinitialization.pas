unit GUIInitialization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CastleUIState, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, CastleControl, MainGameUnit, CastleControls,
  CastleColors, CastleUIControls, CastleTriangles, CastleShapes, CastleVectors,
  CastleViewport, CastleCameras, X3DNodes, X3DFields, X3DTIme, CastleImages,
  CastleGLImages, CastleApplicationProperties, CastleLog, CastleTimeUtils,
  CastleKeysMouse;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    InfoPanel: TPanel;
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
    procedure FormResize(Sender: TObject);
    procedure Splitter1CanOffset(Sender: TObject; var NewOffset: Integer;
      var Accept: Boolean);
    procedure WindowBeforeRender(Sender: TObject);
    procedure WindowClose(Sender: TObject);
    procedure WindowOpen(Sender: TObject);
  end;

var
  CastleForm: TCastleForm;

implementation
{$R *.lfm}

procedure TCastleForm.FormCreate(Sender: TObject);
begin
  WriteLnLog('FormCreate : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$ifdef darwin}
  WindowState := wsFullScreen;
  {$endif}
  AppTime := CastleGetTickCount64;
  PrepDone := False;
  Profiler.Enabled := true;
  InitializeLog;
  Caption := 'MTGVideo CGE Lazarus Application';
end;

procedure TCastleForm.FormResize(Sender: TObject);
begin
  PositionPanel.Height := PositionTrackBar.Height;
end;

procedure TCastleForm.Splitter1CanOffset(Sender: TObject;
  var NewOffset: Integer; var Accept: Boolean);
begin

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

