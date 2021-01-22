unit MainGameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CastleUIState,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, CastleControl,
  {$else}
  CastleWindow,
  {$endif}
  CastleControls, CastleColors, CastleUIControls,
  CastleTriangles, CastleShapes, CastleVectors,
  CastleRectangles,
  X3DNodes, X3DFields, X3DTIme,
  CastleImages, CastleGLImages,
  CastleTextureImages, CastleCompositeImage,
  CastleApplicationProperties, CastleLog, CastleTimeUtils, CastleKeysMouse;

type
  { TCastleApp }

  TCastleApp = class(TUIState)
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TUIState
    function  Motion(const Event: TInputMotion): Boolean; override; // TUIState
    function  Press(const Event: TInputPressRelease): Boolean; override; // TUIState
    function  Release(const Event: TInputPressRelease): Boolean; override; // TUIState
  private
    PointlessButton: TCastleButton;
    LabelFPS: TCastleLabel;
    LabelImgSize: TCastleLabel;
    LabelViewSize: TCastleLabel;
    LabelRectSize: TCastleLabel;
    LabelWinSize: TCastleLabel;
    LabelEffectiveSize: TCastleLabel;
    LabelRender: TCastleLabel;
    LabelSceneLoad: TCastleLabel;
    CRect: TCastleRectangleControl;
    CurrentFrame: TCastleImageControl;
  public
    procedure PointlessButtonClick(Sender: TObject);
    procedure CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const ButtonCode: TNotifyEvent = nil);
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadView;
    procedure LoadFrame(filename: String);
  end;

var
  AppTime: Int64;
  PrepDone: Boolean;
  CastleApp: TCastleApp;
  RenderReady: Boolean;

const
  RotateScene: Boolean = False;
  SecsPerRot: Single = 12;
  SceneFile: String = 'castle-data:/frame.jpg';

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

procedure TCastleApp.PointlessButtonClick(Sender: TObject);
var
  ProcTimer: Int64;
begin
  PointlessButton.Exists := False;
  ProcTimer := CastleGetTickCount64;
  LoadFrame(SceneFile);
  ProcTimer := CastleGetTickCount64 - ProcTimer;
  WriteLnLog('ProcTimer (LoadScene) = ' + FormatFloat('####0.000', ProcTimer / 1000) + ' seconds');
  LabelSceneLoad.Caption := 'LoadScene = ' + FormatFloat('####0.000', ProcTimer / 1000) + ' seconds';
end;

procedure TCastleApp.CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const ButtonCode: TNotifyEvent = nil);
begin
  objButton := TCastleButton.Create(Application);
  objButton.Caption := ButtonText;
  objButton.Anchor(hpMiddle, 10);
  objButton.Anchor(vpBottom, 10 + (Line * 35));
  objButton.onClick := ButtonCode;
  InsertFront(objButton);
end;

procedure TCastleApp.CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);
begin
  objLabel := TCastleLabel.Create(Application);
  objLabel.Padding := 5;
  objLabel.Color := White;
  objLabel.Frame := True;
  objLabel.FrameColor := Black;
  objLabel.Anchor(hpLeft, 10);
  if BottomUp then
    objLabel.Anchor(vpBottom, 10 + (Line * 35))
  else
    objLabel.Anchor(vpTop, -(10 + (Line * 35)));
  InsertFront(objLabel);
end;

procedure TCastleApp.LoadView;
begin
  CRect := TCastleRectangleControl.Create(Application);
  CRect.Width := CastleForm.Window.Width;
  CRect.Height := CastleForm.Window.Height;
  CRect.Color := Vector4(0.25, 0, 0, 1);
  InsertFront(CRect);

  CurrentFrame := TCastleImageControl.Create(Application);
  CurrentFrame.Width := CastleForm.Window.Width;
  CurrentFrame.Height := CastleForm.Window.Height;
  CurrentFrame.Stretch := True;
  CurrentFrame.ProportionalScaling := psFit;
  InsertFront(CurrentFrame);

  CreateLabel(LabelViewSize, 0, False);
  CreateLabel(LabelRectSize, 1, False);
  CreateLabel(LabelWinSize, 2, False);
  CreateLabel(LabelImgSize, 3, False);
  CreateLabel(LabelEffectiveSize, 4, False);

  CreateLabel(LabelSceneLoad, 2);
  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);
  CreateButton(PointlessButton, 'The Completely Pointless Load Botton', 5, @PointlessButtonClick);
  WriteLnLog('LoadView #2 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleApp.LoadFrame(filename: String);
var
  ProfileStart: TCastleProfilerTime;
begin
  try
    ProfileStart := Profiler.Start('Scene loading Frame - ' + filename);
    CurrentFrame.URL := filename;
    Profiler.Stop(ProfileStart, True);
  except
    on E : Exception do
      begin
        WriteLnLog('Oops #1' + LineEnding + E.ClassName + LineEnding + E.Message);
       end;
  end;
end;

procedure TCastleApp.Start;
begin
  inherited;
  LogTextureCache := True;
  WriteLnLog('Start : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  CurrentFrame := nil;
  LoadView;
  PrepDone := True;
end;

procedure TCastleApp.Stop;
begin
  inherited;
  WriteLnLog('Stop : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleApp.BeforeRender;
var
  theta: Single;
  Pos, Dir, Up: TVector3;
begin
  inherited;
  LabelFPS.Caption := 'FPS = ' + FormatFloat('####0.00', Container.Fps.RealFps);
  LabelRender.Caption := 'Render = ' + FormatFloat('####0.00', Container.Fps.OnlyRenderFps);

  if not(CurrentFrame = nil) then
    begin
      LabelViewSize.Caption := 'Frame Size  : ' + FloatToStr(CurrentFrame.Width) + ' x ' + FloatToStr(CurrentFrame.Height);
      LabelRectSize.Caption := 'Rect Size  : ' + FloatToStr(CRect.Width) + ' x ' + FloatToStr(CRect.Height);
      LabelWinSize.Caption := 'Window Size : ' + FloatToStr(CastleForm.Window.Width) + ' x ' + FloatToStr(CastleForm.Window.Height);
      LabelImgSize.Caption := 'Image Size  : ' + FloatToStr(CurrentFrame.DrawableImage.Width) + ' x ' + FloatToStr(CurrentFrame.DrawableImage.Height);
      LabelEffectiveSize.Caption := 'Effective Size  : ' + FloatToStr(CurrentFrame.EffectiveWidth) + ' x ' + FloatToStr(CurrentFrame.EffectiveHeight);
      CurrentFrame.Left := (CurrentFrame.Width - CurrentFrame.EffectiveWidth) / 2;
      CurrentFrame.Bottom := (CurrentFrame.Height - CurrentFrame.EffectiveHeight) / 2;
    end;
end;

procedure TCastleApp.Render;
begin
  inherited;

  if PrepDone and GLInitialized and RenderReady then
    begin
      PrepDone := False;
      PointlessButtonClick(nil);
      WriteLnLog('Frame Loaded (displayed?) : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000));
    end;
  RenderReady := True;
end;

procedure TCastleApp.Resize;
begin
  inherited;
  CRect.Width := CastleForm.Window.Width;
  CRect.Height := CastleForm.Window.Height;
  CurrentFrame.Width := CastleForm.Window.Width;
  CurrentFrame.Height := CastleForm.Window.Height;
end;

procedure TCastleApp.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;

function TCastleApp.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

end.

