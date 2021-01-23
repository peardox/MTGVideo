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
  SceneFile: String = 'castle-data:/frame-540p.jpg';
//  SceneFile: String = 'castle-data:/frame-720p.jpg';
//  SceneFile: String = 'castle-data:/frame-1080p.jpg';

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
  CurrentFrame := TCastleImageControl.Create(Application);
  CurrentFrame.Width := StateContainer.Width;
  CurrentFrame.Height := StateContainer.Height;
  CurrentFrame.Stretch := True;
  CurrentFrame.ProportionalScaling := psFit;
  InsertFront(CurrentFrame);

  CreateLabel(LabelViewSize, 0, False);
  CreateLabel(LabelWinSize, 1, False);
  CreateLabel(LabelImgSize, 2, False);
  CreateLabel(LabelEffectiveSize, 3, False);

  CreateLabel(LabelSceneLoad, 2);
  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);
  CreateButton(PointlessButton, 'The Completely Pointless Load Botton', 5, @PointlessButtonClick);
  WriteLnLog('LoadView #2 : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleApp.LoadFrame(filename: String);
var
  ProfileStart: TCastleProfilerTime;
//  SourceImage: TDrawableImage;
begin
  try
    ProfileStart := Profiler.Start('Scene loading Frame - ' + filename);
{
    SourceImage := TDrawableImage.Create(filename);
    CurrentFrame.DrawableImage.DrawFrom(SourceImage,
      FloatRectangle(SourceImage.Rect),
      FloatRectangle(SourceImage.Rect));
}
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
      LabelWinSize.Caption := 'Window Size : ' + FloatToStr(StateContainer.Width) + ' x ' + FloatToStr(StateContainer.Height);
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
  CurrentFrame.Width := StateContainer.Width;
  CurrentFrame.Height := StateContainer.Height;
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

