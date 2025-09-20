unit DDuce.Components.Markdown.View.Edge;
{ Markdown viewer control backed by WebView2 with IE fallback. }

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.StrUtils,
  System.Math,
  System.Variants,
  Winapi.Windows, Winapi.ShellAPI, Winapi.ActiveX,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.OleCtrls, Vcl.Edge,
  SHDocVw, MSHTML,
  DDuce.Components.Markdown.Renderer;

type
  TMarkdownTheme = (mtLight, mtDark, mtCustom);
  TMarkdownViewEdge = class(TCustomControl)

  private
    FEdge                : TEdgeBrowser;
    FIE                  : TWebBrowser;
    FMarkdown            : string;
    FTheme               : TMarkdownTheme;
    FFontName            : string;
    FBaseFontSize        : Integer;
    FLineSpacing         : Double;
    FCustomCSS           : string;
    FOpenLinksExternally : Boolean;
    FPendingHTML         : string;
    FShowLineNumbers     : Boolean;
    FCodeFontScale       : Integer;
    FCodeFontName        : string;
    FReadyTimer          : TTimer;
    procedure SetMarkdown(const AValue: string);
    procedure SetTheme(const AValue: TMarkdownTheme);
    procedure SetFontName(const AValue: string);
    procedure SetBaseFontSize(const AValue: Integer);
    procedure SetLineSpacing(const AValue: Double);
    procedure SetCustomCSS(const AValue: string);
    procedure SetShowLineNumbers(const AValue: Boolean);
    procedure SetCodeFontScale(const AValue: Integer);
    procedure SetCodeFontName(const AValue: string);
    procedure EnsureBrowser;
    procedure EnsureFallbackIE;
    procedure EdgeCreated(ASender: TCustomEdgeBrowser; AResult: HRESULT);
    procedure UpdateHtml;
    function BuildCSS: string;
    function BuildHTML: string;
    procedure ReadyTimerTick(ASender: TObject);

  protected
    procedure CreateWnd; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadMarkdownFromString(const AText: string);
    procedure LoadMarkdownFromFile(const AFileName: string);

  published
    property Align;

    property Anchors;

    property Markdown: string
      read FMarkdown write SetMarkdown;

    property Theme: TMarkdownTheme
      read FTheme write SetTheme default mtLight;

    property FontName: string
      read FFontName write SetFontName;

    property BaseFontSize: Integer
      read FBaseFontSize write SetBaseFontSize;

    property LineSpacing: Double
      read FLineSpacing write SetLineSpacing;

    property CustomCSS: string
      read FCustomCSS write SetCustomCSS;

    property OpenLinksInExternalBrowser: Boolean
      read FOpenLinksExternally write FOpenLinksExternally default True;

    property ShowLineNumbers: Boolean
      read FShowLineNumbers write SetShowLineNumbers default False;

    property CodeFontScalePercent: Integer
      read FCodeFontScale write SetCodeFontScale;

    property CodeFontName: string
      read FCodeFontName write SetCodeFontName;

  end;

implementation

{$REGION 'construction and destruction'}
constructor TMarkdownViewEdge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Width        := 600;
  Height       := 400;
  FTheme               := mtLight;
  FFontName            := 'Segoe UI';
  FBaseFontSize        := 14;
  FLineSpacing         := 1.6;
  FOpenLinksExternally := True;
  FShowLineNumbers     := False;
  FCodeFontScale       := 100;
  FCodeFontName        := 'Consolas';
end;

destructor TMarkdownViewEdge.Destroy;
begin
  if Assigned(FReadyTimer) then
    FreeAndNil(FReadyTimer);
  if Assigned(FEdge) then
  begin
    try
      FEdge.CloseWebView;
    except
      // ignore shutdown exceptions
    end;
    FreeAndNil(FEdge);
  end;
  if Assigned(FIE) then
  begin
    try
      FIE.Stop;
      FIE.Navigate('about:blank');
    except
      // ignore
    end;
    FreeAndNil(FIE);
  end;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event dispatching methods'}
procedure TMarkdownViewEdge.CreateWnd;
begin
  inherited CreateWnd;
  EnsureBrowser;
  // if content was queued before handle existed, render it now
  if FPendingHTML <> '' then
    UpdateHtml;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TMarkdownViewEdge.SetBaseFontSize(const AValue: Integer);
begin
  if FBaseFontSize <> AValue then
  begin
    FBaseFontSize := AValue;
    UpdateHtml;
  end;
end;

procedure TMarkdownViewEdge.SetLineSpacing(const AValue: Double);

var
  LValue: Double;
begin
  LValue := AValue;
  if LValue < 1.0 then
    LValue := 1.0;
  if LValue > 3.0 then
    LValue := 3.0;
  if Abs(FLineSpacing - LValue) > 0.01 then
  begin
    FLineSpacing := LValue;
    UpdateHtml;
  end;
end;

procedure TMarkdownViewEdge.SetCustomCSS(const AValue: string);
begin
  if FCustomCSS <> AValue then
  begin
    FCustomCSS := AValue;
    UpdateHtml;
  end;
end;

procedure TMarkdownViewEdge.SetFontName(const AValue: string);
begin
  if FFontName <> AValue then
  begin
    FFontName := AValue;
    UpdateHtml;
  end;
end;

procedure TMarkdownViewEdge.SetMarkdown(const AValue: string);
begin
  if FMarkdown <> AValue then
  begin
    FMarkdown := AValue;
    UpdateHtml;
  end;
end;

procedure TMarkdownViewEdge.SetTheme(const AValue: TMarkdownTheme);
begin
  if FTheme <> AValue then
  begin
    FTheme := AValue;
    UpdateHtml;
  end;
end;

procedure TMarkdownViewEdge.SetShowLineNumbers(const AValue: Boolean);
begin
  if FShowLineNumbers <> AValue then
  begin
    FShowLineNumbers := AValue;
    UpdateHtml;
  end;
end;

procedure TMarkdownViewEdge.SetCodeFontScale(const AValue: Integer);

var
  LScale : Integer;
begin
  LScale := AValue;
  if LScale < 60 then
    LScale := 60;
  if LScale > 120 then
    LScale := 120;
  if FCodeFontScale <> LScale then
  begin
    FCodeFontScale := LScale;
    UpdateHtml;
  end;
end;

procedure TMarkdownViewEdge.SetCodeFontName(const AValue: string);
var
  LName : string;
begin
  LName := Trim(AValue);
  if LName = '' then
    LName := 'Consolas';
  if not SameText(FCodeFontName, LName) then
  begin
    FCodeFontName := LName;
    UpdateHtml;
  end;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TMarkdownViewEdge.EdgeCreated(ASender: TCustomEdgeBrowser;
  AResult: HRESULT);
begin
  if AResult <> S_OK then
  begin
    // fallback to IE-based renderer
    EnsureFallbackIE;
    UpdateHtml;
    Exit;
  end;
  if FPendingHTML <> '' then
  begin
    try
      FEdge.NavigateToString(FPendingHTML);
    except
      // ignore; will try again on next update
    end;
    FPendingHTML := '';
  end
  else if FMarkdown <> '' then
  begin
    UpdateHtml;
  end;
end;

procedure TMarkdownViewEdge.ReadyTimerTick(ASender: TObject);
var
  LDoc : IHTMLDocument2;
  LVar : OleVariant;
begin
  if FPendingHTML = '' then
  begin
    if Assigned(FReadyTimer) then
      FReadyTimer.Enabled := False;
    Exit;
  end;
  if Assigned(FIE) and Assigned(FIE.Document) then
  begin
    LDoc := FIE.Document as IHTMLDocument2;
    try
      LDoc.Open('about:blank', EmptyParam, EmptyParam, EmptyParam);
    except
      // ignore
    end;
    LVar := VarArrayCreate([0, 0], varVariant);
    LVar[0] := FPendingHTML;
    LDoc.Write(PSafeArray(TVarData(LVar).VArray));
    LDoc.Close;
    FPendingHTML := '';
    if Assigned(FReadyTimer) then
      FReadyTimer.Enabled := False;
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TMarkdownViewEdge.EnsureBrowser;
begin
  if Assigned(FEdge) then
    Exit;
  FEdge := TEdgeBrowser.Create(Self);
  FEdge.Parent := Self;
  FEdge.Align  := alClient;
  FEdge.OnCreateWebViewCompleted := EdgeCreated;
  try
    FEdge.CreateWebView;
  except
    // in case WebView2 runtime not installed; control will try later
  end;
end;

procedure TMarkdownViewEdge.EnsureFallbackIE;
begin
  if Assigned(FIE) then
    Exit;
  FIE        := TWebBrowser.Create(Self);
  FIE.Align  := alClient;
  FIE.Silent := True;
  TWinControl(FIE).Parent := Self;
end;

procedure TMarkdownViewEdge.UpdateHtml;
var
  LHtml: string;
begin
  if not HandleAllocated then
  begin
    // defer until the control has a window handle
    FPendingHTML := BuildHTML;
    Exit;
  end;
  if not Assigned(FEdge) and not Assigned(FIE) then
    EnsureBrowser;
  LHtml := BuildHTML;
  // always keep latest HTML; attempt to navigate if possible
  FPendingHTML := LHtml;
  try
    if Assigned(FEdge) and FEdge.WebViewCreated then
      FEdge.NavigateToString(LHtml)
    else if Assigned(FIE) then
    begin
      // trigger timer to write into IE document when ready
      FIE.Navigate('about:blank');
      if not Assigned(FReadyTimer) then
      begin
        FReadyTimer := TTimer.Create(Self);
        FReadyTimer.Interval := 100;
        FReadyTimer.OnTimer  := ReadyTimerTick;
      end;
      FReadyTimer.Enabled := True;
    end;
  except
    // ignore; EdgeCreated will pick up FPendingHTML later
  end;
end;

function TMarkdownViewEdge.BuildCSS: string;
const
  BASE_CSS_LIGHT: PChar =
    'html,body { height:100%%; }' +
    'body { margin:0; padding:0; background:#ffffff; color:#24292e; }' +
    '.container { padding:16px; box-sizing:border-box; }' +
    '.markdown-body { color:#24292e; background-color:#ffffff; }' +
    'body, .markdown-body { font-family:%s,Segoe UI,Tahoma,Arial,sans-serif; ' +
    'font-size:%dpx; line-height:%s; }' +
    '.markdown-body pre, .markdown-body code { font-size:%d%% !important; }' +
    '.markdown-body h1,.markdown-body h2,.markdown-body h3,.markdown-body h4,' +
    '.markdown-body h5,.markdown-body h6 { margin:1.2em 0 .6em; font-weight:' +
    '600; line-height:1.25; }' +
    '.markdown-body h1 { font-size:2em; border-bottom:1px solid #eaecef; ' +
    'padding-bottom:.3em; }' +
    '.markdown-body h2 { font-size:1.5em; border-bottom:1px solid #eaecef; ' +
    'padding-bottom:.3em; }' +
    '.markdown-body h3 { font-size:1.25em; }' +
    '.markdown-body p { margin:.6em 0; }' +
    '.markdown-body pre, .markdown-body code { font-family:"%s", ''Courier ' +
    'New'', monospace; }' +
    '.markdown-body code { background:#f6f8fa; border:1px solid #eaecef; ' +
    'padding:.2em .4em; border-radius:6px; color:#24292e; }' +
    '.markdown-body pre { background:#f6f8fa; border:1px solid #eaecef; ' +
    'padding:12px 16px; margin:1em 0; overflow:auto; border-radius:6px; ' +
    'line-height:1.3; }' +
    '.markdown-body pre code { background:transparent; border:0; padding:0; ' +
    'display:block; }' +
    '.markdown-body pre.code-block .cl { display:block; line-height:1.3; }' +
    '.markdown-body pre.code-block .ln { display:%s; width:2.5em; ' +
    'color:#6e7781; text-align:right; padding-right:12px; user-select:none; ' +
    '-ms-user-select:none; }' +
    '.markdown-body pre.code-block .tx { display:inline; }' +
    '.markdown-body pre.code-block .cl .ln, .markdown-body pre.code-block .cl ' +
    '.tx { vertical-align:top; }' +
    '.markdown-body .kw { color:#d73a49; } .markdown-body .str { color:#032f62; ' +
    '} .markdown-body .com { color:#6a737d; font-style:italic; } ' +
    '.markdown-body .num { color:#005cc5; }' +
    '.markdown-body a { color:#0366d6; text-decoration:none; }' +
    '.markdown-body a:hover { text-decoration:underline; }' +
    '.markdown-body blockquote { margin:.8em 0; padding:0 .8em; color:#6a737d; ' +
    'border-left:.25em solid #dfe2e5; }' +
    '.markdown-body hr { height:.25em; padding:0; margin:24px 0; ' +
    'background-color:#e1e4e8; border:0; }' +
    '.markdown-body ul, .markdown-body ol { padding-left:2em; margin:.4em 0; }' +
    '.markdown-body img { max-width:100%%; }';
  BASE_CSS_DARK: PChar =
    'html,body { height:100%%; }' +
    'body { margin:0; padding:0; background:#0d1117; color:#c9d1d9; }' +
    '.container { padding:16px; box-sizing:border-box; }' +
    '.markdown-body { color:#c9d1d9; background-color:#0d1117; }' +
    'body, .markdown-body { font-family:%s,Segoe UI,Tahoma,Arial,sans-serif; ' +
    'font-size:%dpx; line-height:%s; }' +
    '.markdown-body pre, .markdown-body code { font-size:%d%% !important; }' +
    '.markdown-body h1,.markdown-body h2,.markdown-body h3,.markdown-body h4,' +
    '.markdown-body h5,.markdown-body h6 { margin:1.2em 0 .6em; ' +
    'font-weight:600; line-height:1.25; color:#e6edf3; }' +
    '.markdown-body h1 { font-size:2em; border-bottom:1px solid #30363d; ' +
    'padding-bottom:.3em; }' +
    '.markdown-body h2 { font-size:1.5em; border-bottom:1px solid #30363d; ' +
    'padding-bottom:.3em; }' +
    '.markdown-body h3 { font-size:1.25em; }' +
    '.markdown-body p { margin:.6em 0; }' +
    '.markdown-body pre, .markdown-body code { font-family:"%s", ''Courier ' +
    'New'', monospace; }' +
    '.markdown-body code { background:#161b22; border:1px solid #30363d; ' +
    'color:#e6edf3; padding:.2em .4em; border-radius:6px; }' +
    '.markdown-body pre { background:#161b22; border:1px solid #30363d; ' +
    'color:#e6edf3; padding:12px 16px; margin:1em 0; overflow:auto; ' +
    'border-radius:6px; line-height:1.3; }' +
    '.markdown-body pre code { background:transparent; border:0; padding:0; ' +
    'display:block; }' +
    '.markdown-body pre.code-block .cl { display:block; line-height:1.3; }' +
    '.markdown-body pre.code-block .ln { display:%s; width:2.5em; ' +
    'color:#8b949e; text-align:right; padding-right:12px; user-select:none; ' +
    '-ms-user-select:none; }' +
    '.markdown-body pre.code-block .tx { display:inline; }' +
    '.markdown-body pre.code-block .cl .ln, .markdown-body pre.code-block .cl ' +
    '.tx { vertical-align:top; }' +
    '.markdown-body .kw { color:#ff7b72; } .markdown-body .str { color:#a5d6ff; ' +
    '} .markdown-body .com { color:#8b949e; font-style:italic; } ' +
    '.markdown-body .num { color:#79c0ff; }' +
    '.markdown-body a { color:#58a6ff; text-decoration:none; }' +
    '.markdown-body a:hover { text-decoration:underline; }' +
    '.markdown-body blockquote { margin:.8em 0; padding:0 .8em; color:#8b949e; ' +
    'border-left:.25em solid #30363d; }' +
    '.markdown-body hr { height:.25em; padding:0; margin:24px 0; ' +
    'background-color:#21262d; border:0; }' +
    '.markdown-body ul, .markdown-body ol { padding-left:2em; margin:.4em 0; }' +
    '.markdown-body img { max-width:100%%; }';

var
  LBase           : string;
  LLnDisplay      : string;
  LLineHeight     : string;
  LFormatSettings : TFormatSettings;
begin
  if FShowLineNumbers then
    LLnDisplay := 'inline'
  else
    LLnDisplay := 'none';
  LFormatSettings := TFormatSettings.Create;
  LFormatSettings.DecimalSeparator := '.';
  LLineHeight := Format('%.3f', [FLineSpacing], LFormatSettings);
  while (Length(LLineHeight) > 0) and (LLineHeight[Length(LLineHeight)] = '0') do
    Delete(LLineHeight, Length(LLineHeight), 1);
  if (Length(LLineHeight) > 0) and (LLineHeight[Length(LLineHeight)] = '.') then
    LLineHeight := LLineHeight + '0';
  case FTheme of
    mtLight:
      LBase := Format(
        string(BASE_CSS_LIGHT),
        [FFontName, FBaseFontSize, LLineHeight, FCodeFontScale, FCodeFontName, LLnDisplay]
      );
    mtDark:
      LBase := Format(
        string(BASE_CSS_DARK),
        [FFontName, FBaseFontSize, LLineHeight, FCodeFontScale, FCodeFontName, LLnDisplay]
      );
  else
    LBase := Format(
      string(BASE_CSS_LIGHT),
      [FFontName, FBaseFontSize, LLineHeight, FCodeFontScale, FCodeFontName, LLnDisplay]
    );
  end;
  if FCustomCSS <> '' then
    Result := LBase + sLineBreak + FCustomCSS
  else
    Result := LBase;
end;

function TMarkdownViewEdge.BuildHTML: string;
var
  LContent : string;
  LCss     : string;
begin
  LContent := MarkdownToHtml(FMarkdown);
  LCss     := BuildCSS;
  Result   :=
    '<!DOCTYPE html>' + sLineBreak +
    '<html>' + sLineBreak +
    '<head>' + sLineBreak +
    '<meta charset="utf-8">' + sLineBreak +
    '<meta http-equiv="X-UA-Compatible" content="IE=edge" />' + sLineBreak +
    '<meta name="viewport" content="width=device-width, initial-scale=1" />'
    + sLineBreak +
    (IfThen(FOpenLinksExternally, '<base target="_blank">' + sLineBreak, ''))
    +
    '<style>' + sLineBreak + LCss + sLineBreak + '</style>' + sLineBreak +
    '</head>' + sLineBreak +
    '<body>' + sLineBreak +
    '<div class="container">' + sLineBreak + LContent + sLineBreak +
    '</div>' + sLineBreak +
    '</body>' + sLineBreak +
    '</html>';
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TMarkdownViewEdge.LoadMarkdownFromFile(const AFileName: string);
begin
  FMarkdown := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  UpdateHtml;
end;

procedure TMarkdownViewEdge.LoadMarkdownFromString(const AText: string);
begin
  Markdown := AText;
end;
{$ENDREGION}
end.
