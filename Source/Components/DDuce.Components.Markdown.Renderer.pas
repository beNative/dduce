unit DDuce.Components.Markdown.Renderer;

{ Simple Markdown → HTML converter focused on common GitHub‑style markdown. }

interface

uses
  System.SysUtils, System.Classes, System.StrUtils;

{
  Supports headings, paragraphs, emphasis, code blocks, inline code, lists,
  blockquotes, links, images, horizontal rules. No external dependencies.
}

function MarkdownToHtml(const AMarkdown: string): string;

implementation

uses
  System.Math;

{$REGION 'non-interfaced routines'}
function HtmlEscape(const AText: string): string;
var
  I          : Integer;
  LOut       : string;
begin
  LOut := '';
  SetLength(LOut, Length(AText) * 6); // worst case expansion
  LOut := '';

  for I := 1 to Length(AText) do
  begin
    case AText[I] of
      '&': LOut := LOut + '&amp;';
      '<': LOut := LOut + '&lt;';
      '>': LOut := LOut + '&gt;';
      '"': LOut := LOut + '&quot;';
      '''': LOut := LOut + '&#39;';
    else
      LOut := LOut + AText[I];
    end;
  end;

  Result := LOut;
end;

function Lower(const AText: string): string; inline;
begin
  Result := System.SysUtils.LowerCase(AText);
end;

(* Naive highlighter: strings, // comments, { } comments, numbers, keywords *)

function ColorizePascalLine(const AText: string): string;
const
  KW: array[0..35] of string = (
    'program', 'unit', 'interface', 'implementation', 'uses', 'type', 'var',
    'const', 'resourceString', 'begin', 'end', 'class', 'record', 'object',
    'case', 'of', 'if', 'then', 'else', 'for', 'to', 'downto', 'while', 'do',
    'repeat', 'until', 'try', 'except', 'finally', 'in', 'is', 'with', 'out',
    'overload', 'override', 'nil'
  );
var
  I, N               : Integer;
  LLine              : string;

  function IsIdentChar(const ACh: Char): Boolean; inline;
  begin
    Result := CharInSet(ACh, ['A'..'Z', 'a'..'z', '0'..'9', '_']);
  end;

  function IsDigit(const ACh: Char): Boolean; inline;
  begin
    Result := CharInSet(ACh, ['0'..'9']);
  end;

  function IsKeyword(const AWord: string): Boolean;
  var
    K  : Integer;
    LW : string;
  begin
    LW := Lower(AWord);
    for K := Low(KW) to High(KW) do
      if LW = KW[K] then
        Exit(True);
    Result := False;
  end;

  procedure AppendEscaped(var AOut: string; const APart: string);
  begin
    AOut := AOut + HtmlEscape(APart);
  end;
var
  LOut, LToken           : string;
  LInString              : Boolean;
  LInLineComment         : Boolean;
  LInBlockComment        : Boolean;
begin
  LLine           := AText;
  LOut            := '';
  LInString       := False;
  LInLineComment  := False;
  LInBlockComment := False;
  I               := 1;
  N               := Length(LLine);

  while I <= N do
  begin
    if LInLineComment then
    begin
      LOut := LOut + '<span class="com">';
      AppendEscaped(LOut, Copy(LLine, I, N - I + 1));
      LOut := LOut + '</span>';
      Break;
    end
    else if LInBlockComment then
    begin
      LOut := LOut + '<span class="com">';
      while (I <= N) and (LLine[I] <> '}') do
      begin
        AppendEscaped(LOut, LLine[I]);
        Inc(I);
      end;
      if (I <= N) and (LLine[I] = '}') then
      begin
        AppendEscaped(LOut, '}');
        Inc(I);
        LInBlockComment := False;
      end;
      LOut := LOut + '</span>';
      Continue;
    end
    else if LInString then
    begin
      LOut := LOut + '<span class="str">''';
      Inc(I);
      while I <= N do
      begin
        if LLine[I] = '''' then
        begin
          LOut := LOut + HtmlEscape('''');
          Inc(I);
          if (I <= N) and (LLine[I] = '''') then
            Continue
          else
          begin
            LInString := False;
            Break;
          end;
        end
        else
        begin
          AppendEscaped(LOut, LLine[I]);
          Inc(I);
        end;
      end;
      LOut := LOut + '</span>';
      Continue;
    end;

    // Not in string/comment
    if (I < N) and (LLine[I] = '/') and (LLine[I + 1] = '/') then
    begin
      LInLineComment := True;
      Continue;
    end;
    if LLine[I] = '{' then
    begin
      LInBlockComment := True;
      Continue;
    end;
    if LLine[I] = '''' then
    begin
      LInString := True;
      Continue;
    end;

    if IsIdentChar(LLine[I]) then
    begin
      LToken := '';
      while (I <= N) and IsIdentChar(LLine[I]) do
      begin
        LToken := LToken + LLine[I];
        Inc(I);
      end;
      if IsKeyword(LToken) then
        LOut := LOut + '<span class="kw">' + HtmlEscape(LToken) + '</span>'
      else if IsDigit(LToken[1]) then
        LOut := LOut + '<span class="num">' + HtmlEscape(LToken) + '</span>'
      else
        AppendEscaped(LOut, LToken);
      Continue;
    end
    else
    begin
      AppendEscaped(LOut, LLine[I]);
      Inc(I);
    end;
  end;

  Result := LOut;
end;

{ Replace pairs of ADelim ... ADelim with AOpenTag ... ACloseTag (non-greedy). }

function ReplaceDelimited(const AText, ADelim: string; const AOpenTag,
  ACloseTag: string
): string;
var
  I         : Integer;
  LStartPos : Integer;
  LOut      : string;
begin
  LOut     := '';
  I        := 1;
  LStartPos := 0;

  while I <= Length(AText) do
  begin
    if (LStartPos = 0) and (Copy(AText, I, Length(ADelim)) = ADelim) then
    begin
      LStartPos := I + Length(ADelim);
      Inc(I, Length(ADelim));
      Continue;
    end
    else if (LStartPos <> 0) and
            (Copy(AText, I, Length(ADelim)) = ADelim) then
    begin
      LOut := LOut + AOpenTag + Copy(AText, LStartPos, I - LStartPos) +
        ACloseTag;
      LStartPos := 0;
      Inc(I, Length(ADelim));
      Continue;
    end
    else
    begin
      if LStartPos = 0 then
        LOut := LOut + AText[I];
      Inc(I);
    end;
  end;

  if LStartPos <> 0 then
    // unmatched opening delimiter; put it back
    LOut := LOut + ADelim + Copy(
      AText, LStartPos, Max(0, Length(AText) - LStartPos + 1)
    );

  Result := LOut;
end;

function ReplaceInlineCode(const AText: string): string;
var
  I         : Integer;
  LStartPos : Integer;
  LOut      : string;
  LCodeText : string;
begin
  LOut      := '';
  I         := 1;
  LStartPos := 0;

  while I <= Length(AText) do
  begin
    if (AText[I] = '`') and (LStartPos = 0) then
    begin
      LStartPos := I + 1;
      Inc(I);
      Continue;
    end
    else if (AText[I] = '`') and (LStartPos <> 0) then
    begin
      LCodeText := Copy(AText, LStartPos, I - LStartPos);
      LOut := LOut + '<code>' + HtmlEscape(LCodeText) + '</code>';
      LStartPos := 0;
      Inc(I);
      Continue;
    end
    else
    begin
      if LStartPos = 0 then
        LOut := LOut + AText[I];
      Inc(I);
    end;
  end;

  if LStartPos <> 0 then
    // unclosed code span; restore backticks
    LOut := LOut + '`' + Copy(
      AText, LStartPos, Max(0, Length(AText) - LStartPos + 1)
    );

  Result := LOut;
end;

function ReplaceImagesAndLinks(const AText: string): string;
var
  I          : Integer;
  LOut       : string;
  LAltOrText : string;
  LUrl       : string;
begin
  LOut := '';
  I    := 1;

  while I <= Length(AText) do
  begin
    if (AText[I] = '!') and (I + 1 <= Length(AText)) and
       (AText[I + 1] = '[') then
    begin
      // image
      Inc(I, 2); // skip ![
      LAltOrText := '';
      while (I <= Length(AText)) and (AText[I] <> ']') do
      begin
        LAltOrText := LAltOrText + AText[I];
        Inc(I);
      end;
      if (I <= Length(AText)) and (AText[I] = ']') and (I + 1 <= Length(AText))
         and (AText[I + 1] = '(') then
      begin
        Inc(I, 2); // skip ](
        LUrl := '';
        while (I <= Length(AText)) and (AText[I] <> ')') do
        begin
          LUrl := LUrl + AText[I];
          Inc(I);
        end;
        if (I <= Length(AText)) and (AText[I] = ')') then
        begin
          Inc(I);
          LOut := LOut + '<img alt="' + HtmlEscape(LAltOrText) + '" src="' +
            HtmlEscape(LUrl) + '" />';
          Continue;
        end;
      end;
      // fallback if malformed
      LOut := LOut + '![' + LAltOrText;
      Continue;
    end
    else if AText[I] = '[' then
    begin
      // link
      Inc(I);
      LAltOrText := '';
      while (I <= Length(AText)) and (AText[I] <> ']') do
      begin
        LAltOrText := LAltOrText + AText[I];
        Inc(I);
      end;
      if (I <= Length(AText)) and (AText[I] = ']') and (I + 1 <= Length(AText))
         and (AText[I + 1] = '(') then
      begin
        Inc(I, 2); // skip ](
        LUrl := '';
        while (I <= Length(AText)) and (AText[I] <> ')') do
        begin
          LUrl := LUrl + AText[I];
          Inc(I);
        end;
        if (I <= Length(AText)) and (AText[I] = ')') then
        begin
          Inc(I);
          LOut := LOut + '<a href="' + HtmlEscape(LUrl) +
            '" target="_blank">' + HtmlEscape(LAltOrText) + '</a>';
          Continue;
        end;
      end;
      // fallback if malformed
      LOut := LOut + '[' + LAltOrText;
      Continue;
    end
    else
    begin
      LOut := LOut + AText[I];
      Inc(I);
    end;
  end;

  Result := LOut;
end;

function ReplaceEmphasis(const AText: string): string;
var
  LOut : string;
begin
  // Order matters: strong before em to avoid partial capture
  LOut := ReplaceDelimited(AText, '~~', '<del>', '</del>');
  LOut := ReplaceDelimited(LOut, '**', '<strong>', '</strong>');
  LOut := ReplaceDelimited(LOut, '__', '<strong>', '</strong>');
  LOut := ReplaceDelimited(LOut, '*', '<em>', '</em>');
  LOut := ReplaceDelimited(LOut, '_', '<em>', '</em>');
  Result := LOut;
end;

{ Applies emphasis only outside <code>...</code> regions. }

function ApplyEmphasisOutsideCode(const AText: string): string;
var
  I         : Integer;
  LOpenPos  : Integer;
  LClosePos : Integer;
  LOut      : string;
  LChunk    : string;

  procedure ApplyOn(const APart: string);
  begin
    LOut := LOut + ReplaceEmphasis(APart);
  end;
begin
  LOut := '';
  I    := 1;

  while I <= Length(AText) do
  begin
    LOpenPos := PosEx('<code>', AText, I);
    if LOpenPos = 0 then
    begin
      LChunk := Copy(AText, I, Length(AText) - I + 1);
      ApplyOn(LChunk);
      Break;
    end
    else
    begin
      // apply on text before code
      LChunk := Copy(AText, I, LOpenPos - I);
      ApplyOn(LChunk);

      // append code section verbatim up to closing tag
      LClosePos := PosEx('</code>', AText, LOpenPos + 6);
      if LClosePos = 0 then
      begin
        LOut := LOut + Copy(AText, LOpenPos, Length(AText) - LOpenPos + 1);
        Break;
      end
      else
      begin
        LOut := LOut + Copy(AText, LOpenPos, (LClosePos - LOpenPos) + 7);
        I    := LClosePos + 7;
      end;
    end;
  end;

  Result := LOut;
end;

function AutoLink(const AText: string): string;
var
  I     : Integer;
  LOut  : string;
  LWord : string;

  function IsUrlChar(const ACh: Char): Boolean;
  begin
    Result := not CharInSet(ACh, [' ', #9, #10, #13, '<', '>', '"', '''']);
  end;

begin
  LOut := '';
  I    := 1;

  while I <= Length(AText) do
  begin
    if AText[I] = '<' then
    begin
      // Angle‑bracket autolink or raw tag passthrough
      if ((I + 7 <= Length(AText)) and SameText(Copy(AText, I + 1, 7),
          'http://')) or
         ((I + 8 <= Length(AText)) and SameText(Copy(AText, I + 1, 8),
          'https://')) then
      begin
        Inc(I); // skip '<'
        LWord := '';
        while (I <= Length(AText)) and (AText[I] <> '>') do
        begin
          LWord := LWord + AText[I];
          Inc(I);
        end;
        if (I <= Length(AText)) and (AText[I] = '>') then
          Inc(I);
        LOut := LOut + '<a href="' + HtmlEscape(LWord) + '" target="_blank"' +
          '>' + HtmlEscape(LWord) + '</a>';
        Continue;
      end
      else
      begin
        LOut := LOut + '<';
        Inc(I);
        while (I <= Length(AText)) and (AText[I] <> '>') do
        begin
          LOut := LOut + AText[I];
          Inc(I);
        end;
        if (I <= Length(AText)) and (AText[I] = '>') then
        begin
          LOut := LOut + '>';
          Inc(I);
        end;
        Continue;
      end;
    end
    else if ((I + 7 <= Length(AText)) and
              SameText(Copy(AText, I, 7), 'http://')) or
            ((I + 8 <= Length(AText)) and
              SameText(Copy(AText, I, 8), 'https://')) then
    begin
      LWord := '';
      while (I <= Length(AText)) and IsUrlChar(AText[I]) do
      begin
        LWord := LWord + AText[I];
        Inc(I);
      end;
      LOut := LOut + '<a href="' + HtmlEscape(LWord) + '" target="_blank"' +
        '>' + HtmlEscape(LWord) + '</a>';
      Continue;
    end
    else
    begin
      LOut := LOut + AText[I];
      Inc(I);
    end;
  end;

  Result := LOut;
end;

function RenderInline(const AText: string): string;
var
  LT : string;
begin
  // Apply in order: images/links → inline code → emphasis → autolinks
  LT := ReplaceImagesAndLinks(AText);
  LT := ReplaceInlineCode(LT);
  LT := HtmlEscape(LT); // escape anything not inside code or link/img tags

  // Unescape tags we deliberately inserted
  LT := LT.Replace('&lt;code&gt;', '<code>', [rfReplaceAll])
    .Replace('&lt;/code&gt;', '</code>', [rfReplaceAll])
    .Replace('&lt;a ', '<a ', [rfReplaceAll])
    .Replace('&lt;/a&gt;', '</a>', [rfReplaceAll])
    .Replace('&lt;img ', '<img ', [rfReplaceAll])
    .Replace('/&gt;', '/>', [rfReplaceAll])
    .Replace('&quot;', '"', [rfReplaceAll])
    .Replace('&#39;', '''', [rfReplaceAll])
    // decode double‑escaped entities sometimes present in source
    .Replace('&amp;#39;', '''', [rfReplaceAll])
    .Replace('&amp;quot;', '"', [rfReplaceAll])
    .Replace('&amp;lt;', '&lt;', [rfReplaceAll])
    .Replace('&amp;gt;', '&gt;', [rfReplaceAll])
    // safe to unescape '>' globally so tags open properly
    .Replace('&gt;', '>', [rfReplaceAll]);

  LT := ApplyEmphasisOutsideCode(LT);
  LT := AutoLink(LT);
  Result := LT;
end;

function StartsWith(const AText, APrefix: string): Boolean; inline;
begin
  Result := Copy(AText, 1, Length(APrefix)) = APrefix;
end;

function CountLeading(const AText: string; const AChar: Char): Integer;
var
  I : Integer;
begin
  Result := 0;
  for I := 1 to Length(AText) do
  begin
    if AText[I] = AChar then
      Inc(Result)
    else
      Break;
  end;
end;
{$ENDREGION}

{$REGION 'interfaced routines'}
function MarkdownToHtml(const AMarkdown: string): string;
var
  LLines           : TArray<string>;
  I                : Integer;
  LLine            : string;
  LInCodeBlock     : Boolean;
  LCodeFence       : string;
  LCodeLang        : string;
  LCodeBuffer      : string;
  LInBlockQuote    : Boolean;
  LInParagraph     : Boolean;
  LParagraphBuffer : string;
  LLevel           : Integer; // heading level temp
  LText            : string;
  LTrimLeft        : string;
  J                : Integer;
  LIsNum           : Boolean;
  LItem            : string;
  LItemText        : string;
  LBlockClass      : string;
  LCodeLines       : TArray<string>;
  K                : Integer;
  LLineHtml        : string;
  LListDepth       : Integer;
  LLiOpen          : array[0..15] of Boolean;
  LKindAt          : array[0..15] of Char; // 'u' or 'o'
  LSpaces          : Integer;
  LDepthT          : Integer;

  function LeadingSpaces(const AText: string): Integer;
  var
    P: Integer;
  begin
    P      := 1;
    Result := 0;
    while P <= Length(AText) do
    begin
      if AText[P] = ' ' then
        Inc(Result)
      else if AText[P] = #9 then
        Inc(Result, 2) // treat tab as 2 spaces
      else
        Break;
      Inc(P);
    end;
  end;

  procedure FlushParagraph;
  begin
    if LInParagraph then
    begin
      Result := Result + '<p>' +
        RenderInline(TrimRight(LParagraphBuffer)) + '</p>' + sLineBreak;
      LParagraphBuffer := '';
      LInParagraph     := False;
    end;
  end;

  procedure CloseToDepth(const ATargetDepth: Integer);
  begin
    while LListDepth > ATargetDepth do
    begin
      if LLiOpen[LListDepth] then
      begin
        Result := Result + '</li>' + sLineBreak;
        LLiOpen[LListDepth] := False;
      end;
      if LKindAt[LListDepth] = 'o' then
        Result := Result + '</ol>' + sLineBreak
      else
        Result := Result + '</ul>' + sLineBreak;
      Dec(LListDepth);
    end;
  end;

  function StripIndentUpTo3(const AText: string): string;
  var
    N : Integer;
  begin
    N := LeadingSpaces(AText);
    if N > 3 then
      N := 3;
    Result := Copy(AText, N + 1, Max(0, Length(AText) - N));
  end;

  procedure EnsureDepth(const ATargetDepth: Integer; const AOrdered: Boolean);
  var
    LDesiredKind : Char;
  begin
    // close lists to shallower depth if needed
    if ATargetDepth < LListDepth then
      CloseToDepth(ATargetDepth);

    // close previous item at same depth
    if (ATargetDepth = LListDepth) and (LListDepth > 0) and
       LLiOpen[LListDepth] then
    begin
      Result := Result + '</li>' + sLineBreak;
      LLiOpen[LListDepth] := False;
    end;

    // open deeper lists
    while ATargetDepth > LListDepth do
    begin
      FlushParagraph;
      if AOrdered then
      begin
        Result := Result + '<ol>' + sLineBreak;
        LKindAt[LListDepth + 1] := 'o';
      end
      else
      begin
        Result := Result + '<ul>' + sLineBreak;
        LKindAt[LListDepth + 1] := 'u';
      end;
      Inc(LListDepth);
      LLiOpen[LListDepth] := False;
    end;

    // switch kind at same depth if needed
    if AOrdered then
      LDesiredKind := 'o'
    else
      LDesiredKind := 'u';

    if (ATargetDepth > 0) and (LKindAt[LListDepth] <> LDesiredKind) then
    begin
      if LKindAt[LListDepth] = 'o' then
        Result := Result + '</ol>' + sLineBreak
      else
        Result := Result + '</ul>' + sLineBreak;

      if AOrdered then
      begin
        Result := Result + '<ol>' + sLineBreak;
        LKindAt[LListDepth] := 'o';
      end
      else
      begin
        Result := Result + '<ul>' + sLineBreak;
        LKindAt[LListDepth] := 'u';
      end;
    end;
  end;

  procedure EnsureBlockQuote(const AOpen: Boolean);
  begin
    if AOpen and (not LInBlockQuote) then
    begin
      FlushParagraph;
      Result       := Result + '<blockquote>' + sLineBreak;
      LInBlockQuote := True;
    end
    else if (not AOpen) and LInBlockQuote then
    begin
      Result       := Result + '</blockquote>' + sLineBreak;
      LInBlockQuote := False;
    end;
  end;

  procedure OpenParagraphIfNeeded;
  begin
    if not LInParagraph then
    begin
      LParagraphBuffer := '';
      LInParagraph     := True;
    end;
  end;

begin
  LLines := AMarkdown.Split([#10, #13], TStringSplitOptions.ExcludeEmpty);
  LInCodeBlock     := False;
  LCodeLang        := '';
  LCodeBuffer      := '';
  LInBlockQuote    := False;
  LInParagraph     := False;
  LParagraphBuffer := '';
  Result           := '';
  LListDepth       := 0; // list nesting level

  for I := 0 to High(LLines) do
  begin
    LLine := LLines[I];

    // trim right side to simplify handling
    while (Length(LLine) > 0) and
          CharInSet(LLine[Length(LLine)], [#9, ' ']) do
      Delete(LLine, Length(LLine), 1);

    if not LInCodeBlock then
    begin
      if StartsWith(LLine, '```') then
      begin
        FlushParagraph;
        LCodeFence  := '```';
        LCodeLang   := Trim(Copy(LLine, 4, Max(0, Length(LLine) - 3)));
        LInCodeBlock := True;
        LCodeBuffer := '';
        Continue;
      end;
    end
    else // in code block
    begin
      if StartsWith(LLine, LCodeFence) then
      begin
        // emit code block as lines for optional numbering
        LBlockClass := 'code-block';
        if LCodeLang <> '' then
          LBlockClass := LBlockClass + ' lang-' + LowerCase(LCodeLang);

        Result := Result + '<pre class="' + LBlockClass + '"><code>';
        LCodeLines := LCodeBuffer.Split([#10], TStringSplitOptions.None);
        for K := 0 to High(LCodeLines) do
        begin
          if (Lower(LCodeLang) = 'pascal') or (Lower(LCodeLang) = 'delphi') then
            LLineHtml := ColorizePascalLine(LCodeLines[K])
          else
            LLineHtml := HtmlEscape(LCodeLines[K]);

          // keep numeric content visible without CSS counters
          Result := Result +
            '<span class="cl"><span class="ln">' + IntToStr(K + 1) +
            '&nbsp;</span><span class="tx"> ' + LLineHtml + '</span></span>';
        end;
        Result := Result + '</code></pre>' + sLineBreak;
        LInCodeBlock := False;
        LCodeBuffer  := '';
        LCodeLang    := '';
      end
      else
      begin
        if LCodeBuffer = '' then
          LCodeBuffer := LLine
        else
          LCodeBuffer := LCodeBuffer + #10 + LLine;
      end;
      Continue;
    end;

    if Trim(LLine) = '' then
    begin
      FlushParagraph;
      EnsureBlockQuote(False);
      // close any open lists on blank line
      CloseToDepth(0);
      Continue;
    end;

    // Headings (allow up to 3 leading spaces)
    LText := StripIndentUpTo3(LLine);
    if StartsWith(LText, '# ') or StartsWith(LText, '## ') or
       StartsWith(LText, '### ') or StartsWith(LText, '#### ') or
       StartsWith(LText, '##### ') or StartsWith(LText, '###### ') then
    begin
      FlushParagraph;
      CloseToDepth(0);
      EnsureBlockQuote(False);
      LLevel := CountLeading(LText, '#');
      if LLevel > 6 then
        LLevel := 6;
      LText := Trim(Copy(LText, LLevel + 1, Max(0, Length(LText) - LLevel)));
      Result := Result + Format(
        '<h%d>%s</h%d>%s', [LLevel, RenderInline(LText), LLevel, sLineBreak]
      );
      Continue;
    end;

    // Horizontal rule (allow up to 3 leading spaces)
    LText := StripIndentUpTo3(LLine);
    if (LText = '---') or (LText = '***') or (LText = '___') or
       StartsWith(LText, '---') or StartsWith(LText, '***') or
       StartsWith(LText, '___') then
    begin
      FlushParagraph;
      CloseToDepth(0);
      EnsureBlockQuote(False);
      Result := Result + '<hr />' + sLineBreak;
      Continue;
    end;

    // Blockquote
    if StartsWith(System.SysUtils.TrimLeft(LLine), '>') then
    begin
      CloseToDepth(0);
      EnsureBlockQuote(True);
      LText := System.SysUtils.TrimLeft(LLine);
      LText := TrimLeft(Copy(LText, 2, Max(0, Length(LText) - 1)));
      Result := Result + '<p>' + RenderInline(LText) + '</p>' + sLineBreak;
      Continue;
    end
    else
    begin
      EnsureBlockQuote(False);
    end;

    // Lists with nesting (2‑space indent per level)
    LTrimLeft := System.SysUtils.TrimLeft(LLine);
    LSpaces   := LeadingSpaces(LLine);
    LDepthT   := LSpaces div 2;

    // Unordered list
    if StartsWith(LTrimLeft, '- ') or StartsWith(LTrimLeft, '* ') or
       StartsWith(LTrimLeft, '+ ') then
    begin
      EnsureDepth(LDepthT + 1, False);
      LItem := TrimLeft(Copy(LTrimLeft, 3, Max(0, Length(LTrimLeft) - 2)));
      Result := Result + '<li>' + RenderInline(LItem);
      LLiOpen[LListDepth] := True;
      Result := Result + sLineBreak;
      Continue;
    end;

    // Ordered list (digits + dot + space)
    LTrimLeft := System.SysUtils.TrimLeft(LLine);
    J := 1;
    LIsNum := False;
    while (J <= Length(LTrimLeft)) and CharInSet(LTrimLeft[J], ['0'..'9']) do
    begin
      LIsNum := True;
      Inc(J);
    end;
    if LIsNum and (J <= Length(LTrimLeft)) and (LTrimLeft[J] = '.') and
       (J + 1 <= Length(LTrimLeft)) and (LTrimLeft[J + 1] = ' ') then
    begin
      EnsureDepth(LDepthT + 1, True);
      LItemText := TrimLeft(
        Copy(LTrimLeft, J + 2, Max(0, Length(LTrimLeft) - (J + 1)))
      );
      Result := Result + '<li>' + RenderInline(LItemText);
      LLiOpen[LListDepth] := True;
      Result := Result + sLineBreak;
      Continue;
    end
    else
    begin
      // Continuation line for current list item if indented to current depth
      if (LListDepth > 0) and LLiOpen[LListDepth] and (Trim(LLine) <> '') and
         (LSpaces >= LListDepth * 2) then
      begin
        Result := Result + '<br />' + RenderInline(Trim(LLine)) + sLineBreak;
        Continue;
      end;
      // Close any open lists when encountering normal text at top level
      if LDepthT = 0 then
        CloseToDepth(0);
    end;

    // Paragraph text (merge consecutive lines)
    OpenParagraphIfNeeded;
    if LParagraphBuffer <> '' then
      LParagraphBuffer := LParagraphBuffer + ' ' + LLine
    else
      LParagraphBuffer := LLine;
  end;

  // close any open structures
  FlushParagraph;
  CloseToDepth(0);
  EnsureBlockQuote(False);

  // Wrap in minimal container so host can inject CSS around it
  Result := '<article class="markdown-body">' + sLineBreak + Result +
    '</article>';
end;
{$ENDREGION}

end.
