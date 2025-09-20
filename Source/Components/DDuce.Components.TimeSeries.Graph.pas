unit DDuce.Components.TimeSeries.Graph;

{ Time series graph control with GDI/GDI+ rendering and touch gestures. }

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes, System.Math,
  System.Generics.Collections, System.DateUtils,
  Winapi.Windows, Winapi.Messages, Winapi.GDIPAPI, Winapi.GDIPOBJ,
  Vcl.Controls, Vcl.Graphics, Vcl.GraphUtil, Vcl.Themes, Vcl.Touch.GestureMgr;

type
  TGraphPoint = record
    Time  : TDateTime;
    Value : Double;
  end;

  TTimeSeries = class(TPersistent)
  private
    FName         : string;
    FColor        : TColor;
    FLineWidth    : Single;
    FVisible      : Boolean;
    FFill         : Boolean;
    FFillOpacity  : Byte;
    FData         : TList<TGraphPoint>;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure AddPoint(const ATime: TDateTime; const AValue: Double);
    function Count: Integer;

    property Data: TList<TGraphPoint>
      read FData;

  published
    property Name: string
      read FName write FName;

    property Color: TColor
      read FColor write FColor default clSkyBlue;

    property LineWidth: Single
      read FLineWidth write FLineWidth;

    property Visible: Boolean
      read FVisible write FVisible default True;

    property Fill: Boolean
      read FFill write FFill default False;

    property FillOpacity: Byte
      read FFillOpacity write FFillOpacity;

  end;

  TTimeSeriesGraph = class(TCustomControl)
  private
    FSeries                 : TObjectList<TTimeSeries>;
    FBackgroundColor        : TColor;
    FGridColor              : TColor;
    FAxisColor              : TColor;
    FAxisLabelColor         : TColor;
    FShowGrid               : Boolean;
    FShowLegend             : Boolean;
    FUseGDIPlus             : Boolean;
    // view range
    FViewStart              : TDateTime;
    FViewEnd                : TDateTime;
    FViewYMin               : Double;
    FViewYMax               : Double;
    // interaction
    FMouseDown              : Boolean;
    FLastMouse              : TPoint;
    FPanStartViewStart      : TDateTime;
    FPanStartViewEnd        : TDateTime;
    FPanStartYMin           : Double;
    FPanStartYMax           : Double;
    // layout
    FMarginLeft             : Integer;
    FMarginRight            : Integer;
    FMarginTop              : Integer;
    FMarginBottom           : Integer;
    // touch
    FTouchEnabled           : Boolean;
    FLastGesture            : TPoint;
    FLastZoomDistance       : Integer;
    FTouchPanSingleFinger   : Boolean;
    FTouchTwoFingerTapReset : Boolean;

    procedure SetShowGrid(const AValue: Boolean);
    procedure SetShowLegend(const AValue: Boolean);
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetGridColor(const AValue: TColor);
    procedure SetAxisColor(const AValue: TColor);
    procedure SetAxisLabelColor(const AValue: TColor);
    procedure SetUseGDIPlus(const AValue: Boolean);
    procedure SetTouchEnabled(const AValue: Boolean);
    procedure SetTouchPanSingleFinger(const AValue: Boolean);
    procedure SetTouchTwoFingerTapReset(const AValue: Boolean);

    function GetSeriesCount: Integer;
    function GetSeries(AIndex: Integer): TTimeSeries;

    procedure DrawBackground(ACanvas: TCanvas);
    procedure DrawGridAndAxes(ACanvas: TCanvas; const APlotRect: TRect);
    procedure DrawSeries(ACanvas: TCanvas; const APlotRect: TRect);
    procedure DrawLegend(ACanvas: TCanvas; const APlotRect: TRect);

    procedure ComputeAutoView;
    procedure EnsureValidView;
    procedure ResetViewInternal;

    function TimeToX(const ATime: TDateTime; const APlotRect: TRect): Integer;
    function ValueToY(const AValue: Double; const APlotRect: TRect): Integer;

    procedure ComputeXTickStep(
      const ARangeDays : Double;
      out AStepDays    : Double;
      out AFmt         : string
    );

    procedure ComputeYTicks(
      const AMinVal, AMaxVal    : Double;
      out ATickStart, ATickStep : Double
    );

  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
      AX, AY: Integer); override;
    procedure MouseMove(AShift: TShiftState; AX, AY: Integer); override;
    procedure MouseUp(
      AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer
    ); override;
    function DoMouseWheel(
      AShift: TShiftState; AWheelDelta: Integer; AMousePos: TPoint
    ): Boolean; override;
    procedure DoGesture(
      const AEventInfo: TGestureEventInfo; var AHandled: Boolean
    ); override;
    procedure DblClick; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddSeries(const AName: string; AColor: TColor): TTimeSeries;
    procedure ClearSeries;
    procedure AutoScale;
    procedure ResetView;

    property SeriesCount: Integer
      read GetSeriesCount;

    property Series[AIndex: Integer]: TTimeSeries
      read GetSeries; default;

  published
    property Align;

    property Font;

    property Color default clWindow;

    property BackgroundColor: TColor
      read FBackgroundColor write SetBackgroundColor default clWindow;

    property GridColor: TColor
      read FGridColor write SetGridColor default $00E0E0E0;

    property AxisColor: TColor
      read FAxisColor write SetAxisColor default $00606060;

    property AxisLabelColor: TColor
      read FAxisLabelColor write SetAxisLabelColor default $00606060;

    property ShowGrid: Boolean
      read FShowGrid write SetShowGrid default True;

    property ShowLegend: Boolean
      read FShowLegend write SetShowLegend default True;

    property UseGDIPlus: Boolean
      read FUseGDIPlus write SetUseGDIPlus default True;

    property EnableTouchGestures: Boolean
      read FTouchEnabled write SetTouchEnabled default True;

    property TouchPanSingleFinger: Boolean
      read FTouchPanSingleFinger write SetTouchPanSingleFinger default True;

    property TouchTwoFingerTapReset: Boolean
      read FTouchTwoFingerTapReset write SetTouchTwoFingerTapReset default True;

  end;

implementation

const
  EPS_TIME = 1.0 / (24 * 3600 * 10); // 0.1 sec
  EPS_VAL  = 1.0e-9;

{$REGION 'TTimeSeries'}
{$REGION 'construction and destruction'}
constructor TTimeSeries.Create;
begin
  inherited Create;
  FName        := '';
  FColor       := clSkyBlue;
  FLineWidth   := 2.0;
  FVisible     := True;
  FFill        := False;
  FFillOpacity := 64;
  FData        := TList<TGraphPoint>.Create;
end;

destructor TTimeSeries.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TTimeSeries.Clear;
begin
  FData.Clear;
end;

procedure TTimeSeries.AddPoint(
  const ATime: TDateTime; const AValue: Double
);
var
  LPoint: TGraphPoint;
begin
  LPoint.Time  := ATime;
  LPoint.Value := AValue;
  FData.Add(LPoint);
end;

function TTimeSeries.Count: Integer;
begin
  Result := FData.Count;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TTimeSeriesGraph'}
{$REGION 'construction and destruction'}
constructor TTimeSeriesGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ControlStyle   := ControlStyle + [csOpaque];
  Color          := clWindow;

  FSeries := TObjectList<TTimeSeries>.Create(True);

  FBackgroundColor        := clWindow;
  FGridColor              := $00E0E0E0;
  FAxisColor              := $00606060;
  FAxisLabelColor         := $00606060;
  FShowGrid               := True;
  FShowLegend             := True;
  FUseGDIPlus             := True;
  FTouchEnabled           := True;
  FLastZoomDistance       := 0;
  FTouchPanSingleFinger   := True;
  FTouchTwoFingerTapReset := True;

  FMarginLeft   := 64;
  FMarginRight  := 16;
  FMarginTop    := 12;
  FMarginBottom := 28;

  // init view to something sane
  FViewStart := Now - 1;
  FViewEnd   := Now;
  FViewYMin  := 0;
  FViewYMax  := 100;

  // enable common touch gestures by default
  SetTouchEnabled(True);
end;

destructor TTimeSeriesGraph.Destroy;
begin
  FSeries.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TTimeSeriesGraph.Resize;
begin
  inherited Resize;
  Invalidate;
end;

procedure TTimeSeriesGraph.MouseDown(AButton: TMouseButton; AShift: TShiftState;
  AX, AY: Integer);
begin
  inherited MouseDown(AButton, AShift, AX, AY);
  if AButton = TMouseButton.mbLeft then
  begin
    FMouseDown         := True;
    FLastMouse         := Point(AX, AY);
    FPanStartViewStart := FViewStart;
    FPanStartViewEnd   := FViewEnd;
    FPanStartYMin      := FViewYMin;
    FPanStartYMax      := FViewYMax;
    MouseCapture       := True;
  end;
end;

procedure TTimeSeriesGraph.MouseMove(AShift: TShiftState; AX, AY: Integer);
var
  LPlot     : TRect;
  LDxPix    : Integer;
  LDyPix    : Integer;
  LDtPerPix : Double;
  LDvPerPix : Double;
begin
  inherited MouseMove(AShift, AX, AY);
  if FMouseDown then
  begin
    LPlot := Rect(
      FMarginLeft, FMarginTop,
      ClientWidth - FMarginRight, ClientHeight - FMarginBottom
    );

    LDxPix    := AX - FLastMouse.X;
    LDyPix    := AY - FLastMouse.Y;
    LDtPerPix := (FPanStartViewEnd - FPanStartViewStart) / Max(1, LPlot.Width);
    LDvPerPix := (FPanStartYMax - FPanStartYMin) / Max(1, LPlot.Height);

    FViewStart := FPanStartViewStart - LDxPix * LDtPerPix;
    FViewEnd   := FPanStartViewEnd   - LDxPix * LDtPerPix;
    FViewYMin  := FPanStartYMin      + LDyPix * LDvPerPix;
    FViewYMax  := FPanStartYMax      + LDyPix * LDvPerPix;
    Invalidate;
  end;
end;

procedure TTimeSeriesGraph.MouseUp(AButton: TMouseButton; AShift: TShiftState;
  AX, AY: Integer);
begin
  inherited MouseUp(AButton, AShift, AX, AY);
  if AButton = TMouseButton.mbLeft then
  begin
    FMouseDown   := False;
    MouseCapture := False;
  end;
end;

function TTimeSeriesGraph.DoMouseWheel(AShift: TShiftState;
  AWheelDelta: Integer; AMousePos: TPoint): Boolean;
var
  LPlot       : TRect;
  LFactor     : Double;
  LCursorTime : TDateTime;
  LCursorVal  : Double;
  LTRange     : Double;
  LVRange     : Double;
  LRelX       : Double;
  LRelY       : Double;
begin
  Result := inherited DoMouseWheel(AShift, AWheelDelta, AMousePos);

  LPlot := Rect(
    FMarginLeft, FMarginTop,
    ClientWidth - FMarginRight, ClientHeight - FMarginBottom
  );
  if (LPlot.Width <= 0) or (LPlot.Height <= 0) then
    Exit;

  // zoom factor per wheel notch
  LFactor := Power(1.1, AWheelDelta / 120);

  // cursor normalized pos in plot (clamped)
  LRelX := (ScreenToClient(AMousePos).X - LPlot.Left) / Max(1, LPlot.Width);
  LRelY := (ScreenToClient(AMousePos).Y - LPlot.Top) / Max(1, LPlot.Height);
  if LRelX < 0 then
    LRelX := 0
  else if LRelX > 1 then
    LRelX := 1;
  if LRelY < 0 then
    LRelY := 0
  else if LRelY > 1 then
    LRelY := 1;

  LTRange := (FViewEnd - FViewStart);
  LVRange := (FViewYMax - FViewYMin);

  LCursorTime := FViewStart + LRelX * LTRange;
  LCursorVal  := FViewYMin + (1 - LRelY) * LVRange;

  if (ssCtrl in AShift) and (ssShift in AShift) then
  begin
    // zoom both axes
    FViewStart := LCursorTime - (LCursorTime - FViewStart) / LFactor;
    FViewEnd   := LCursorTime + (FViewEnd - LCursorTime) / LFactor;
    FViewYMin  := LCursorVal   - (LCursorVal   - FViewYMin) / LFactor;
    FViewYMax  := LCursorVal   + (FViewYMax    - LCursorVal) / LFactor;
  end
  else if (ssCtrl in AShift) then
  begin
    // zoom Y only
    FViewYMin := LCursorVal - (LCursorVal - FViewYMin) / LFactor;
    FViewYMax := LCursorVal + (FViewYMax  - LCursorVal) / LFactor;
  end
  else
  begin
    // zoom X only
    FViewStart := LCursorTime - (LCursorTime - FViewStart) / LFactor;
    FViewEnd   := LCursorTime + (FViewEnd - LCursorTime) / LFactor;
  end;

  EnsureValidView;
  Invalidate;
  Result := True;
end;

procedure TTimeSeriesGraph.DblClick;
begin
  inherited DblClick;
  ResetView;
end;

procedure TTimeSeriesGraph.DoGesture(const AEventInfo: TGestureEventInfo;
  var AHandled: Boolean);
var
  LPlot      : TRect;
  LPt        : TPoint;
  LDx, LDy   : Integer;
  LDtPerPix  : Double;
  LDvPerPix  : Double;
  LFactor    : Double;
  LRelX      : Double;
  LRelY      : Double;
  LCursorTim : TDateTime;
  LCursorVal : Double;
begin
  inherited DoGesture(AEventInfo, AHandled);
  if not FTouchEnabled then
    Exit;

  // pan with finger drag
  if AEventInfo.GestureID = igiPan then
  begin
    LPt := ScreenToClient(AEventInfo.Location);
    if gfBegin in AEventInfo.Flags then
    begin
      FLastGesture      := LPt;
      FPanStartViewStart := FViewStart;
      FPanStartViewEnd   := FViewEnd;
      FPanStartYMin      := FViewYMin;
      FPanStartYMax      := FViewYMax;
    end
    else
    begin
      LPlot := Rect(
        FMarginLeft, FMarginTop,
        ClientWidth - FMarginRight, ClientHeight - FMarginBottom
      );
      LDx      := LPt.X - FLastGesture.X;
      LDy      := LPt.Y - FLastGesture.Y;
      LDtPerPix := (FPanStartViewEnd - FPanStartViewStart) /
                   Max(1, LPlot.Width);
      LDvPerPix := (FPanStartYMax - FPanStartYMin) / Max(1, LPlot.Height);
      FViewStart := FPanStartViewStart - LDx * LDtPerPix;
      FViewEnd   := FPanStartViewEnd   - LDx * LDtPerPix;
      FViewYMin  := FPanStartYMin      + LDy * LDvPerPix;
      FViewYMax  := FPanStartYMax      + LDy * LDvPerPix;
      Invalidate;
    end;
    AHandled := True;
    Exit;
  end;

  // pinch zoom
  if AEventInfo.GestureID = igiZoom then
  begin
    if (gfBegin in AEventInfo.Flags) or (FLastZoomDistance <= 0) then
      FLastZoomDistance := Max(1, AEventInfo.Distance)
    else
    begin
      if AEventInfo.Distance > 0 then
      begin
        LFactor := AEventInfo.Distance / FLastZoomDistance;
        LPlot := Rect(
          FMarginLeft, FMarginTop,
          ClientWidth - FMarginRight, ClientHeight - FMarginBottom
        );
        LPt := ScreenToClient(AEventInfo.Location);
        LRelX := (LPt.X - LPlot.Left) / Max(1, LPlot.Width);
        LRelY := (LPt.Y - LPlot.Top) / Max(1, LPlot.Height);
        if LRelX < 0 then LRelX := 0 else if LRelX > 1 then LRelX := 1;
        if LRelY < 0 then LRelY := 0 else if LRelY > 1 then LRelY := 1;
        LCursorTim := FViewStart + LRelX * (FViewEnd - FViewStart);
        LCursorVal := FViewYMin + (1 - LRelY) * (FViewYMax - FViewYMin);
        FViewStart := LCursorTim - (LCursorTim - FViewStart) / LFactor;
        FViewEnd   := LCursorTim + (FViewEnd - LCursorTim) / LFactor;
        FViewYMin  := LCursorVal - (LCursorVal - FViewYMin) / LFactor;
        FViewYMax  := LCursorVal + (FViewYMax - LCursorVal) / LFactor;
        EnsureValidView;
        Invalidate;
      end;
      FLastZoomDistance := AEventInfo.Distance;
    end;
    if gfEnd in AEventInfo.Flags then
      FLastZoomDistance := 0;
    AHandled := True;
  end;

  // two-finger tap to reset view
  if AEventInfo.GestureID = igiTwoFingerTap then
  begin
    if FTouchTwoFingerTapReset then
    begin
      ResetView;
      AHandled := True;
      Exit;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TTimeSeriesGraph.SetAxisColor(const AValue: TColor);
begin
  if FAxisColor <> AValue then
  begin
    FAxisColor := AValue;
    Invalidate;
  end;
end;

procedure TTimeSeriesGraph.SetAxisLabelColor(const AValue: TColor);
begin
  if FAxisLabelColor <> AValue then
  begin
    FAxisLabelColor := AValue;
    Invalidate;
  end;
end;

procedure TTimeSeriesGraph.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Invalidate;
  end;
end;

procedure TTimeSeriesGraph.SetGridColor(const AValue: TColor);
begin
  if FGridColor <> AValue then
  begin
    FGridColor := AValue;
    Invalidate;
  end;
end;

procedure TTimeSeriesGraph.SetShowGrid(const AValue: Boolean);
begin
  if FShowGrid <> AValue then
  begin
    FShowGrid := AValue;
    Invalidate;
  end;
end;

procedure TTimeSeriesGraph.SetShowLegend(const AValue: Boolean);
begin
  if FShowLegend <> AValue then
  begin
    FShowLegend := AValue;
    Invalidate;
  end;
end;

procedure TTimeSeriesGraph.SetUseGDIPlus(const AValue: Boolean);
begin
  if FUseGDIPlus <> AValue then
  begin
    FUseGDIPlus := AValue;
    Invalidate;
  end;
end;

procedure TTimeSeriesGraph.SetTouchEnabled(const AValue: Boolean);
begin
  if FTouchEnabled <> AValue then
    FTouchEnabled := AValue;

  if FTouchEnabled then
  begin
    Touch.InteractiveGestures := [igPan, igZoom];
    if FTouchTwoFingerTapReset then
      Touch.InteractiveGestures := Touch.InteractiveGestures + [igTwoFingerTap];

    Touch.InteractiveGestureOptions := [
      igoPanInertia,
      igoPanGutter
    ];

    if FTouchPanSingleFinger then
      Touch.InteractiveGestureOptions :=
        Touch.InteractiveGestureOptions + [
          igoPanSingleFingerHorizontal,
          igoPanSingleFingerVertical
        ];
  end
  else
  begin
    Touch.InteractiveGestures := [];
  end;
end;

procedure TTimeSeriesGraph.SetTouchPanSingleFinger(const AValue: Boolean);
begin
  if FTouchPanSingleFinger <> AValue then
  begin
    FTouchPanSingleFinger := AValue;
    if FTouchEnabled then
      SetTouchEnabled(True);
  end;
end;

procedure TTimeSeriesGraph.SetTouchTwoFingerTapReset(const AValue: Boolean);
begin
  if FTouchTwoFingerTapReset <> AValue then
  begin
    FTouchTwoFingerTapReset := AValue;
    if FTouchEnabled then
      SetTouchEnabled(True);
  end;
end;
{$ENDREGION}

{$REGION 'non-interfaced routines'}
procedure TTimeSeriesGraph.ComputeAutoView;
var
  I     : Integer;
  J     : Integer;
  LTMin : TDateTime;
  LTMax : TDateTime;
  LVMin : Double;
  LVMax : Double;
  LAny  : Boolean;
  LSer  : TTimeSeries;
  LPt   : TGraphPoint;
begin
  LAny  := False;
  LTMin := 0;
  LTMax := 0;
  LVMin := 0;
  LVMax := 0;

  for I := 0 to FSeries.Count - 1 do
  begin
    LSer := FSeries[I];
    if (LSer.Visible) and (LSer.Data.Count > 0) then
    begin
      for J := 0 to LSer.Data.Count - 1 do
      begin
        LPt := LSer.Data[J];
        if not LAny then
        begin
          LTMin := LPt.Time;
          LTMax := LPt.Time;
          LVMin := LPt.Value;
          LVMax := LPt.Value;
          LAny  := True;
        end
        else
        begin
          if LPt.Time  < LTMin then
            LTMin := LPt.Time;
          if LPt.Time  > LTMax then
            LTMax := LPt.Time;
          if LPt.Value < LVMin then
            LVMin := LPt.Value;
          if LPt.Value > LVMax then
            LVMax := LPt.Value;
        end;
      end;
    end;
  end;

  if LAny then
  begin
    if SameValue(LTMin, LTMax) then
      LTMax := LTMin + EncodeTime(0, 30, 0, 0); // 30 minutes

    if SameValue(LVMin, LVMax) then
    begin
      LVMin := LVMin - 1;
      LVMax := LVMax + 1;
    end;

    // add padding
    FViewStart := LTMin - (LTMax - LTMin) * 0.05;
    FViewEnd   := LTMax + (LTMax - LTMin) * 0.05;
    FViewYMin  := LVMin - (LVMax - LVMin) * 0.1;
    FViewYMax  := LVMax + (LVMax - LVMin) * 0.1;
  end
  else
  begin
    FViewStart := Now - 1;
    FViewEnd   := Now;
    FViewYMin  := 0;
    FViewYMax  := 100;
  end;

  EnsureValidView;
end;

procedure TTimeSeriesGraph.EnsureValidView;
var
  LDX : Double;
  LDY : Double;
begin
  LDX := FViewEnd - FViewStart;
  if LDX < EPS_TIME then
    FViewEnd := FViewStart + 1 / 24; // 1 hour

  LDY := FViewYMax - FViewYMin;
  if LDY < EPS_VAL then
    FViewYMax := FViewYMin + 1.0;
end;

procedure TTimeSeriesGraph.ResetViewInternal;
begin
  ComputeAutoView;
end;

function TTimeSeriesGraph.TimeToX(const ATime: TDateTime;
  const APlotRect: TRect): Integer;
var
  LDX : Double;
begin
  LDX := (FViewEnd - FViewStart);
  if LDX < EPS_TIME then
    LDX := EPS_TIME;
  Result := APlotRect.Left + Round(
    (ATime - FViewStart) / LDX * (APlotRect.Width)
  );
end;

function TTimeSeriesGraph.ValueToY(const AValue: Double;
  const APlotRect: TRect): Integer;
var
  LDY : Double;
begin
  LDY := (FViewYMax - FViewYMin);
  if LDY < EPS_VAL then
    LDY := EPS_VAL;
  Result := APlotRect.Bottom - Round(
    (AValue - FViewYMin) / LDY * (APlotRect.Height)
  );
end;

procedure TTimeSeriesGraph.ComputeXTickStep(const ARangeDays: Double;
  out AStepDays: Double; out AFmt: string);
const
  SECS: array[0..20] of Integer = (
    1, 2, 5, 10, 15, 30,
    60, 120, 300, 600, 900, 1800,
    3600, 7200, 14400, 21600, 43200,
    86400, 172800, 604800, 2592000 // ~30d
  );
var
  LTotalSec : Double;
  I         : Integer;
  LBest     : Integer;
  LTickCnt  : Double;
begin
  LTotalSec := ARangeDays * 86400.0;
  LBest     := High(SECS);

  for I := Low(SECS) to High(SECS) do
  begin
    LTickCnt := LTotalSec / SECS[I];
    if (LTickCnt >= 4) and (LTickCnt <= 12) then
    begin
      LBest := I;
      Break;
    end;
    if LTickCnt < 4 then
    begin
      LBest := I;
      Break;
    end;
  end;

  AStepDays := SECS[LBest] / 86400.0;

  if SECS[LBest] < 60 then
    AFmt := 'hh:nn:ss'
  else if SECS[LBest] < 3600 then
    AFmt := 'hh:nn'
  else if SECS[LBest] < 86400 then
    AFmt := 'ddd hh:nn'
  else if SECS[LBest] < 2592000 then
    AFmt := 'dd mmm'
  else
    AFmt := 'mmm yyyy';
end;

procedure TTimeSeriesGraph.ComputeYTicks(const AMinVal, AMaxVal: Double;
  out ATickStart, ATickStep: Double);

  function NiceNum(const ARange: Double; const ARound: Boolean): Double;
  var
    LExp  : Double;
    LFrac : Double;
    LSel  : Double;
  begin
    if ARange <= 0 then
      Exit(1);
    LExp  := Power(10, Floor(Log10(ARange)));
    LFrac := ARange / LExp;
    if ARound then
    begin
      if LFrac < 1.5 then      LSel := 1
      else if LFrac < 3 then   LSel := 2
      else if LFrac < 7 then   LSel := 5
      else                     LSel := 10;
    end
    else
    begin
      if LFrac <= 1 then       LSel := 1
      else if LFrac <= 2 then  LSel := 2
      else if LFrac <= 5 then  LSel := 5
      else                     LSel := 10;
    end;
    Result := LSel * LExp;
  end;

var
  LRange : Double;
  LStep  : Double;
begin
  LRange := AMaxVal - AMinVal;
  if LRange <= 0 then
  begin
    ATickStep  := 1;
    ATickStart := AMinVal;
    Exit;
  end;

  LStep      := NiceNum(LRange / 6, True);
  ATickStart := Floor(AMinVal / LStep) * LStep;
  ATickStep  := LStep;
end;
{$ENDREGION}

{$REGION 'interfaced routines'}
procedure TTimeSeriesGraph.AutoScale;
begin
  ComputeAutoView;
  Invalidate;
end;

procedure TTimeSeriesGraph.ResetView;
begin
  ResetViewInternal;
  Invalidate;
end;

function TTimeSeriesGraph.AddSeries(const AName: string; AColor: TColor)
  : TTimeSeries;
begin
  Result        := TTimeSeries.Create;
  Result.Name   := AName;
  Result.Color  := AColor;
  FSeries.Add(Result);
end;

procedure TTimeSeriesGraph.ClearSeries;
begin
  FSeries.Clear;
  Invalidate;
end;

function TTimeSeriesGraph.GetSeries(AIndex: Integer): TTimeSeries;
begin
  Result := FSeries[AIndex];
end;

function TTimeSeriesGraph.GetSeriesCount: Integer;
begin
  Result := FSeries.Count;
end;

procedure TTimeSeriesGraph.Paint;
var
  LCanvas : TCanvas;
  LPlot   : TRect;
begin
  inherited Paint;

  LCanvas := Canvas;
  DrawBackground(LCanvas);

  LPlot := Rect(
    FMarginLeft, FMarginTop,
    ClientWidth - FMarginRight, ClientHeight - FMarginBottom
  );
  if (LPlot.Right  <= LPlot.Left + 10) or
     (LPlot.Bottom <= LPlot.Top  + 10) then
    Exit;

  DrawGridAndAxes(LCanvas, LPlot);
  DrawSeries(LCanvas, LPlot);
  DrawLegend(LCanvas, LPlot);
end;

procedure TTimeSeriesGraph.DrawBackground(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := FBackgroundColor;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(ClientRect);
end;

procedure TTimeSeriesGraph.DrawGridAndAxes(ACanvas: TCanvas;
  const APlotRect: TRect);
var
  LDXDays  : Double;
  LStep    : Double;
  LFmt     : string;
  LT0, LT  : TDateTime;
  LYStart  : Double;
  LYStep   : Double;
  LY        : Double;
  LR        : TRect;
  LTxt      : string;
  LX        : Integer;
  LYPix     : Integer;
  LSaveFont : TColor;
  LSavePenC : TColor;
  LSavePenW : Integer;
  LSaveBrSt : TBrushStyle;
  LSaveBrC  : TColor;
begin
  // save simple pen/brush state
  LSavePenC := ACanvas.Pen.Color;
  LSavePenW := ACanvas.Pen.Width;
  LSaveBrSt := ACanvas.Brush.Style;
  LSaveBrC  := ACanvas.Brush.Color;
  try
    // grid lines
    if FShowGrid then
    begin
      ACanvas.Pen.Style := psSolid;
      ACanvas.Pen.Color := FGridColor;
      ACanvas.Pen.Width := 1;

      // vertical (time) grid
      LDXDays := FViewEnd - FViewStart;
      ComputeXTickStep(LDXDays, LStep, LFmt);
      LT0 := EncodeDate(1970, 1, 1) +
        Trunc((FViewStart - EncodeDate(1970, 1, 1)) / LStep) * LStep;
      LT := LT0;
      while LT <= FViewEnd + LStep do
      begin
        LX := TimeToX(LT, APlotRect);
        ACanvas.MoveTo(LX, APlotRect.Top);
        ACanvas.LineTo(LX, APlotRect.Bottom);
        LT := LT + LStep;
      end;

      // horizontal (value) grid
      ComputeYTicks(FViewYMin, FViewYMax, LYStart, LYStep);
      LY := LYStart;
      while LY <= FViewYMax + LYStep * 0.5 do
      begin
        LYPix := ValueToY(LY, APlotRect);
        ACanvas.MoveTo(APlotRect.Left,  LYPix);
        ACanvas.LineTo(APlotRect.Right, LYPix);
        LY := LY + LYStep;
      end;
    end;

    // axes and labels
    ACanvas.Pen.Color := FAxisColor;
    ACanvas.Pen.Width := 1;
    ACanvas.MoveTo(APlotRect.Left,  APlotRect.Bottom);
    ACanvas.LineTo(APlotRect.Right, APlotRect.Bottom);
    ACanvas.MoveTo(APlotRect.Left,  APlotRect.Top);
    ACanvas.LineTo(APlotRect.Left,  APlotRect.Bottom);

    LSaveFont := ACanvas.Font.Color;
    ACanvas.Font.Assign(Font);
    ACanvas.Font.Color := FAxisLabelColor;

    // X labels
    LDXDays := FViewEnd - FViewStart;
    ComputeXTickStep(LDXDays, LStep, LFmt);
    LT0 := EncodeDate(1970, 1, 1) +
      Trunc((FViewStart - EncodeDate(1970, 1, 1)) / LStep) * LStep;
    LT := LT0;
    while LT <= FViewEnd + LStep do
    begin
      LX  := TimeToX(LT, APlotRect);
      LTxt := FormatDateTime(LFmt, LT);
      LR  := Rect(
        LX - 100, APlotRect.Bottom + 2,
        LX + 100, ClientHeight
      );
      DrawText(
        ACanvas.Handle, PChar(LTxt), Length(LTxt), LR,
        DT_CENTER or DT_SINGLELINE or DT_TOP or DT_END_ELLIPSIS
      );
      LT := LT + LStep;
    end;

    // Y labels
    ComputeYTicks(FViewYMin, FViewYMax, LYStart, LYStep);
    LY := LYStart;
    while LY <= FViewYMax + LYStep * 0.5 do
    begin
      LYPix := ValueToY(LY, APlotRect);
      LTxt  := FormatFloat('0.###', LY);
      LR    := Rect(
        2,
        LYPix - ACanvas.TextHeight(LTxt) div 2,
        APlotRect.Left - 6,
        LYPix + ACanvas.TextHeight(LTxt) div 2 + 1
      );
      DrawText(
        ACanvas.Handle, PChar(LTxt), Length(LTxt), LR,
        DT_RIGHT or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS
      );
      LY := LY + LYStep;
    end;

    ACanvas.Font.Color := LSaveFont;
  finally
    // restore state
    ACanvas.Pen.Color   := LSavePenC;
    ACanvas.Pen.Width   := LSavePenW;
    ACanvas.Brush.Style := LSaveBrSt;
    ACanvas.Brush.Color := LSaveBrC;
  end;
end;

procedure TTimeSeriesGraph.DrawSeries(ACanvas: TCanvas; const APlotRect: TRect);
var
  I        : Integer;
  J        : Integer;
  LSer     : TTimeSeries;
  LPts     : array of TGPPointF;
  LClipRgn : HRGN;
  LG       : TGPGraphics;
  LPen     : TGPPen;
  LBrush   : TGPSolidBrush;
  LPoly    : array of TGPPointF;
  LSavedDC : Integer;

  function MakeARGB(const AColor: TColor; AAlpha: Byte = 255): ARGB;
  var
    LCol     : TColor;
    LR, LGc, LB: Byte;
  begin
    LCol := ColorToRGB(AColor);
    LR   := GetRValue(LCol);
    LGc  := GetGValue(LCol);
    LB   := GetBValue(LCol);
    Result := MakeColor(AAlpha, LR, LGc, LB);
  end;
begin
  // clip to plot area while drawing series
  LClipRgn := CreateRectRgn(
    APlotRect.Left, APlotRect.Top, APlotRect.Right, APlotRect.Bottom
  );
  LSavedDC := SaveDC(ACanvas.Handle);
  try
    SelectClipRgn(ACanvas.Handle, LClipRgn);

    if FUseGDIPlus then
    begin
      LG := TGPGraphics.Create(ACanvas.Handle);
      try
        LG.SetSmoothingMode(SmoothingModeAntiAlias);
        for I := 0 to FSeries.Count - 1 do
        begin
          LSer := FSeries[I];
          if (not LSer.Visible) or (LSer.Data.Count = 0) then
            Continue;

          SetLength(LPts, LSer.Data.Count);
          for J := 0 to LSer.Data.Count - 1 do
          begin
            LPts[J].X := Single(TimeToX(LSer.Data[J].Time,  APlotRect));
            LPts[J].Y := Single(ValueToY(LSer.Data[J].Value, APlotRect));
          end;

          // fill (optional) to bottom of plot
          if LSer.Fill and (Length(LPts) >= 2) then
          begin
            LBrush := TGPSolidBrush.Create(MakeARGB(LSer.Color, LSer.FillOpacity));
            try
              // build polygon: series points + bottom-right + bottom-left
              SetLength(LPoly, Length(LPts) + 2);
              for J := 0 to High(LPts) do
              begin
                LPoly[J].X := LPts[J].X;
                LPoly[J].Y := LPts[J].Y;
              end;
              LPoly[High(LPoly) - 1].X := LPts[High(LPts)].X;
              LPoly[High(LPoly) - 1].Y := APlotRect.Bottom;
              LPoly[High(LPoly)].X     := LPts[0].X;
              LPoly[High(LPoly)].Y     := APlotRect.Bottom;
              LG.FillPolygon(LBrush, PGPPointF(@LPoly[0]), Length(LPoly));
            finally
              LBrush.Free;
            end;
          end;

          LPen := TGPPen.Create(MakeARGB(LSer.Color), LSer.LineWidth);
          try
            if Length(LPts) > 1 then
              LG.DrawLines(LPen, PGPPointF(@LPts[0]), Length(LPts));
          finally
            LPen.Free;
          end;
        end;
      finally
        LG.Free;
      end;
    end
    else
    begin
      // fallback GDI rendering
      for I := 0 to FSeries.Count - 1 do
      begin
        LSer := FSeries[I];
        if (not LSer.Visible) or (LSer.Data.Count = 0) then
          Continue;

        ACanvas.Pen.Color := LSer.Color;
        ACanvas.Pen.Width := Max(1, Round(LSer.LineWidth));
        ACanvas.Pen.Style := psSolid;

        ACanvas.MoveTo(
          TimeToX(LSer.Data[0].Time, APlotRect),
          ValueToY(LSer.Data[0].Value, APlotRect)
        );
        for J := 1 to LSer.Data.Count - 1 do
          ACanvas.LineTo(
            TimeToX(LSer.Data[J].Time, APlotRect),
            ValueToY(LSer.Data[J].Value, APlotRect)
          );
      end;
    end;
  finally
    RestoreDC(ACanvas.Handle, LSavedDC);
    DeleteObject(LClipRgn);
  end;
end;

procedure TTimeSeriesGraph.DrawLegend(ACanvas: TCanvas; const APlotRect: TRect);
var
  I    : Integer;
  H    : Integer;
  W    : Integer;
  X    : Integer;
  Y    : Integer;
  SW   : Integer;
  LTxt : string;
  LR   : TRect;
  CY   : Integer;
begin
  if not FShowLegend then
    Exit;

  ACanvas.Font.Assign(Font);
  H := ACanvas.TextHeight('Hg');

  W := 0;
  for I := 0 to FSeries.Count - 1 do
    if FSeries[I].Visible then
    begin
      LTxt := FSeries[I].Name;
      SW   := ACanvas.TextWidth(LTxt);
      if SW > W then
        W := SW;
    end;
  if W = 0 then
    Exit;

  // padding: 8px, sample line box width 18
  W := W + 18 + 12;
  X := APlotRect.Right - W - 12;
  Y := APlotRect.Top + 12;

  // background
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := RGB(248, 248, 248);
  ACanvas.Pen.Color   := RGB(200, 200, 200);
  LR := Rect(X, Y, X + W, Y + (H + 6) * FSeries.Count + 8);
  Winapi.Windows.RoundRect(
    ACanvas.Handle, LR.Left, LR.Top, LR.Right, LR.Bottom, 6, 6
  );

  // entries
  CY := LR.Top + 6;
  for I := 0 to FSeries.Count - 1 do
    if FSeries[I].Visible then
    begin
      // line sample
      ACanvas.Pen.Color := FSeries[I].Color;
      ACanvas.Pen.Width := Max(1, Round(FSeries[I].LineWidth));
      ACanvas.MoveTo(LR.Left + 8,  CY + H div 2);
      ACanvas.LineTo(LR.Left + 24, CY + H div 2);

      // text
      LTxt := FSeries[I].Name;
      ACanvas.Font.Color := clWindowText;
      ACanvas.TextOut(LR.Left + 28, CY + 1, LTxt);

      CY := CY + H + 6;
    end;
end;
{$ENDREGION}
{$ENDREGION}

end.
