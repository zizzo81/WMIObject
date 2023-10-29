unit FormMainUnit;

{******************************************************************************}
{                                                                              }
{     WMI query simple class for Delphi Demo application                       }
{     Version 1.0 released October, 29th 2023                                  }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ MIT License                                                                  }
{                                                                              }
{ Copyright (c) 2023, Christian Cristofori <github@christiancristofori.it>     }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to     }
{ deal in the Software without restriction, including without limitation the   }
{ rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  }
{ sell copies of the Software, and to permit persons to whom the Software is   }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      }
{ FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS }
{ IN THE SOFTWARE.                                                             }
{                                                                              }
{ If you have this source, you can use it as you want, how the hell am I going }
{ to stop you from using it anyway? Being mentioned in your apps'documentation }
{ and/or about box will be greatly appreciated, although not necessary.        }
{ Feedback is appreciated too.                                                 }
{                                                                              }
{ You can get in touch with me at any time.                                    }
{                                                                              }
{******************************************************************************}

interface

uses
  Forms, Classes, Controls, StdCtrls, Grids, ExtCtrls, Windows;

type
  TFormMain = class(TForm)
    pQuery: TPanel;
    bExecute: TButton;
    cQuery: TComboBox;
    pResults: TPanel;
    dgResults: TDrawGrid;
    eComputer: TEdit;
    lComputer: TLabel;
    cNamespace: TComboBox;
    lNamespace: TLabel;
    eUsername: TEdit;
    ePassword: TEdit;
    lUsername: TLabel;
    lPassword: TLabel;
    lQuery: TLabel;
    procedure bExecuteClick(Sender: TObject);
    procedure pQueryResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cQueryChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure dgResultsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure dgResultsDblClick(Sender: TObject);
    procedure dgResultsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
  private
    FLastCol,
    FLastRow: Integer;
    FArraysValues: TStringList;
    FValues: Array of Array of String;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  SysUtils, Inifiles, Graphics, WMIObject, FormArrayUnit, FormFieldUnit;

const
  INI_FILE_EXTENSION = '.ini';

  SECTION_HISTORY = 'History';
  SECTION_NAMESPACE = 'History:Namespace';
  SECTION_QUERY = 'History:Query';

  IDENT_COMPUTER = 'Computer';
  IDENT_USERNAME = 'Username';

procedure TFormMain.bExecuteClick(Sender: TObject);
const
  FIELD_TYPE: Array[TWMIFieldType] Of String = (
    '(unknown/unsupported)', 'Boolean', 'DateTime', 'Extended', 'Integer', 'String'); { Do not localize. }
  WMI_RESULTS: Array[TWMIResult] Of String = (
    '', 'Could not open WMI connection.', 'Unknown/unhandled exception.', 'Query returned an empty recordset.',
    'Query returned no fields!', 'Access denied!', 'Failed.', 'Invalid parameter.', 'Invalid query.', 'Invalid query type.',
    'Invalid class.', 'Out of memory!');
var
  R: TWMIResult;
  X, Y, M, W: Integer;
  OC: TCursor;
  S: String;
  OFS, FS: TFontStyles;
  Query: TWMIObject;
begin
  OC := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  pResults.Caption := 'Executing query...';
  dgResults.Visible := False;
  dgResults.ColCount := 1;
  dgResults.RowCount := 1;
  Finalize(FValues);
  Application.ProcessMessages;

  Query := TWMIObject.Create;
  try
    // Setup object.
    Query.Computer := eComputer.Text;
    Query.Namespace := cNamespace.Text;
    Query.Username := eUsername.Text;
    Query.Password := ePassword.Text;

    // Execute query or get instance.
    if Pos('SELECT', UpperCase(cQuery.Text)) > 0 then { Do not localize. }
      R := Query.ExecQuery(cQuery.Text)
    else
      R := Query.InstancesOf(cQuery.Text);

    case R of
      wmirOk:
      begin
        // Store last used values in combo boxes.
        S := cNamespace.Text;
        X := cNamespace.Items.IndexOf(S);
        if X > -1 then
          cNamespace.Items.Delete(X);
        cNamespace.Items.Insert(0, S);
        cNamespace.ItemIndex := 0;

        S := cQuery.Text;
        X := cQuery.Items.IndexOf(S);
        if X > -1 then
          cQuery.Items.Delete(X);
        cQuery.Items.Insert(0, S);
        cQuery.ItemIndex := 0;

        // Initialize grid.
        dgResults.Visible := True;
        dgResults.ColCount := Query.Count + 2;
        dgResults.RowCount := Query.Fields.Count;
        SetLength(FValues, dgResults.ColCount, dgResults.RowCount);

        // Store records information.
        for Y := 0 to Query.Fields.Count - 1 do
        begin
          FValues[0, Y] := Query.Fields[Y].Name;
          FValues[1, Y] := FIELD_TYPE[Query.Fields[Y].FieldType];
        end;

        // Populate grid.
        X := 2;
        while not Query.EoF do
        begin
          for Y := 0 to Query.Fields.Count - 1 do
            if Query.Fields[Y].IsNull then
              FValues[X, Y] := '(NULL)' { Do not localize. }
            else if Query.Fields[Y].IsArray then
            begin
              W := FArraysValues.Add(Query.Fields[Y].AsRaw);
              FValues[X, Y] := Format('ARRAY:%d', [W]); { Do not localize. }
            end else
              FValues[X, Y] := Query.Fields[Y].AsString;

          Query.Next;

          Inc(X)
        end;

        // Expands columns width.
        OFS := dgResults.Canvas.Font.Style;
        try
          FS := OFS;
          for X := 0 to dgResults.ColCount - 1 do
          begin
            M := 0;
            Exclude(FS, fsBold);
            Exclude(FS, fsItalic);
            case X of
              0: Include(FS, fsBold);
              1: Include(FS, fsItalic)
            end;

            dgResults.Canvas.Font.Style := FS;
            for Y := 0 to dgResults.RowCount - 1 do
            begin
              W := dgResults.Canvas.TextWidth(FValues[X, Y]);
              if W > M then
                M := W
            end;

            dgResults.ColWidths[X] := M + 8
          end
        finally
          dgResults.Canvas.Font.Style := OFS
        end
      end;

      // Show error.
      wmirNoConnection, wmirException, wmirEmptyResult, wmirEmptyObject,
      wmirAccessDenied, wmirFailed, wmirInvalidParameter, wmirInvalidQuery,
      wmirInvalidQueryType, wmirInvalidClass, wmirOutOfMemory:
        pResults.Caption := WMI_RESULTS[R]
    end

  finally
    Query.Free;
    Screen.Cursor := OC
  end
end;

procedure TFormMain.cQueryChange(Sender: TObject);
begin
  bExecute.Enabled := Length(cQuery.Text) > 0
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  SL1, SL2: TStringList;
  S: String;
begin
  // Inizialize variables.
  FArraysValues := TStringList.Create;
  Finalize(FValues);

  // Load data from ini file.
  SL1 := TStringList.Create;
  SL2 := TStringList.Create;
  with TIniFile.Create(ChangeFileExt(Application.ExeName, INI_FILE_EXTENSION)) do
  try
    S := ReadString(SECTION_HISTORY, IDENT_COMPUTER, '.');
    if Length(Trim(S)) = 0 then
      S := '.';
    eComputer.Text := S;

    if SectionExists(SECTION_NAMESPACE) then
    begin
      SL1.Clear;
      SL2.Clear;

      ReadSection(SECTION_NAMESPACE, SL1);

      while SL1.Count > 0 do
      begin
        S := Trim(ReadString(SECTION_NAMESPACE, SL1[0], ''));
        SL1.Delete(0);
        if (Length(S) > 0) and (SL2.IndexOf(S) = -1) then
          SL2.Add(S)
      end;

      if SL2.Count > 0 then
        cNamespace.Items.Assign(SL2)
    end;

    if cNamespace.Items.Count > 0 then
      cNamespace.ItemIndex := 0;

    eUsername.Text := ReadString(SECTION_HISTORY, IDENT_USERNAME, '');

    {$IFNDEF DEBUG}
    cQuery.Clear;
    {$ENDIF}

    if SectionExists(SECTION_QUERY) then
    begin
      SL1.Clear;
      SL2.Clear;

      ReadSection(SECTION_QUERY, SL1);

      while SL1.Count > 0 do
      begin
        S := Trim(ReadString(SECTION_QUERY, SL1[0], ''));
        SL1.Delete(0);
        if (Length(S) > 0) and (SL2.IndexOf(S) = -1) then
          SL2.Add(S)
      end;

      if SL2.Count > 0 then
        cQuery.Items.Assign(SL2)
    end;

    if cQuery.Items.Count > 0 then
      cQuery.ItemIndex := 0

  finally
    SL1.Free;
    SL2.Free;
    Free { TIniFile }
  end;

  // Refresh UI.
  cQueryChange(cQuery)
end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  // Saves input values to ini file.
  with TIniFile.Create(ChangeFileExt(Application.ExeName, INI_FILE_EXTENSION)) do
  try
    WriteString(SECTION_HISTORY, IDENT_COMPUTER, eComputer.Text);

    EraseSection(SECTION_NAMESPACE);
    for I := 0 to cNamespace.Items.Count - 1 do
      WriteString(SECTION_NAMESPACE, IntToHex(I, 4), cNamespace.Items[I]);

    WriteString(SECTION_HISTORY, IDENT_USERNAME, eUsername.Text);

    EraseSection(SECTION_QUERY);
    for I := 0 to cQuery.Items.Count - 1 do
      WriteString(SECTION_QUERY, IntToHex(I, 4), cQuery.Items[I])
  finally
    Free { TIniFile }
  end;

    // Uninitializes variables.
  FArraysValues.Free
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  pQueryResize(pQuery)
end;

procedure TFormMain.pQueryResize(Sender: TObject);
var
  R: Integer;
begin
  eComputer.Width := (cQuery.Width - (3 * pQuery.BorderWidth)) div 4;
  R := cQuery.Width - ((eComputer.Width * 4) + (pQuery.BorderWidth * 3));
  cNamespace.Left := eComputer.Left + eComputer.Width + pQuery.BorderWidth;
  cNamespace.Width := eComputer.Width;
  lNamespace.Left := cNamespace.Left;
  eUsername.Left := cNamespace.Left + cNamespace.Width + pQuery.BorderWidth;
  eUsername.Width := eComputer.Width;
  lUsername.Left := eUsername.Left;
  ePassword.Left := eUsername.Left + eUsername.Width + pQuery.BorderWidth;
  ePassword.Width := eComputer.Width + R;
  lPassword.Left := ePassword.Left
end;

procedure TFormMain.dgResultsDblClick(Sender: TObject);
var
  S: String;
  I, X: Integer;
begin
  // Get current cell text.
  S := '';
  if (FLastCol >= 0) and (FLastCol < Length(FValues))and (FLastRow >= 0) and
    (FLastRow < Length(FValues[FLastCol])) then
    S := FValues[FLastCol, FLastRow];

  // Manage array type.
  if Copy(S, 1, 6) = 'ARRAY:' then { Do not localize. }
  begin
    // Get array length.
    Delete(S, 1, 6);
    I := StrToIntDef(S, -1);
    if (I >= 0) and (I < FArraysValues.Count) then
      // Show the detail form.
      with TFormArray.Create(Self) do
      try
        // Populate the grid.
        Items.Text := StringReplace(FArraysValues[I], #$7C, Items.LineBreak, [rfReplaceAll]); { Do not localize. }

        FieldName := FValues[0, FLastRow];
        FieldType := FValues[1, FLastRow];

        for X := 0 to Items.Count - 1 do
          Items[X] := StringReplace(Items[X], '#$7C', #$7C, [rfReplaceAll, rfIgnoreCase]); { Do not localize. }

        // Integer values get aligned to the right.
        if (1 < Length(FValues)) and (FLastRow >= 0) and (FLastRow < Length(FValues[1])) and (UpperCase(Trim(FValues[1, FLastRow])) = 'INTEGER') then { Do not localize. }
          AlignText := 1;

        ShowModal
      finally
        Free { TFormArray }
      end;

    Exit
  end;

  // When selecting a non-null, non-empty, non-header cell...
  if (Length(S) > 0) and (S <> '(NULL)') and (FLastCol > 1) then { Do not localize. }
    // ...show the detail form.
    with TFormField.Create(Self) do try
      // Popupate form.
      eField.Text := FValues[0, FLastRow];
      eIndex.Text := '(n/a)';
      eType.Text := FValues[1, FLastRow];
      eValue.Text := S;

      ShowModal
    finally
      Free { TFormField }
    end
end;

procedure TFormMain.dgResultsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  S, T: String;
  X, Y: Integer;
  OFS, FS: TFontStyles;
  OBC, OFC: TColor;
begin
  // Draws values in cells.
  OBC := TDrawGrid(Sender).Canvas.Brush.Color;
  OFS := TDrawGrid(Sender).Canvas.Font.Style;
  OFC := TDrawGrid(Sender).Canvas.Font.Color;
  try
    // Text style is chosen based on column index.
    FS := OFS;
    Exclude(FS, fsBold);
    Exclude(FS, fsItalic);
    case ACol of
      0: Include(FS, fsBold);
      1: Include(FS, fsItalic);
    end;
    TDrawGrid(Sender).Canvas.Font.Style := FS;

    // Give first two columns a grey background.
    if (ACol <= 1) and not(gdFocused in State) and not(gdSelected in State) then
      TDrawGrid(Sender).Canvas.Brush.Color := $00EEEEEE;
    TDrawGrid(Sender).Canvas.FillRect(Rect);

    // Gets cell text.
    S := '';
    if (ACol >= 0) and (ACol < Length(FValues)) and (ARow >= 0) and (ARow < Length(FValues[ACol])) then
      S := FValues[ACol, ARow];

    // Gets cell value typw.
    T := '';
    if (1 < Length(FValues)) and (ARow >= 0) and (ARow < Length(FValues[1])) then
      T := UpperCase(Trim(FValues[1, ARow]));

    // Based on type, chose the position of text.
    X := Rect.Left + 4;
    if (T = 'INTEGER') and (ACol > 1) then { Do not localize. }
      X := Rect.Right - 4 - TDrawGrid(Sender).Canvas.TextWidth(S);

    if (S = '(NULL)') or (Copy(S, 1, 6) = 'ARRAY:') then { Do not localize. }
    begin
      if S = '(NULL)' then { Do not localize. }
        TDrawGrid(Sender).Canvas.Font.Color := clSilver
      else begin
        S := '(Array...)';
        Include(FS, fsBold);
        TDrawGrid(Sender).Canvas.Font.Style := FS
      end;

      X := Rect.Left + ((Rect.Right - Rect.Left) - TDrawGrid(Sender).Canvas.TextWidth(S)) div 2
    end;

    // Outputs text vertically aligned to the middle.
    Y := Rect.Top + ((Rect.Bottom - Rect.Top) - TDrawGrid(Sender).Canvas.TextHeight(S)) div 2;
    TDrawGrid(Sender).Canvas.TextOut(X, Y, S)

  finally
    // Restore previous canvas status.
    TDrawGrid(Sender).Canvas.Brush.Color := OBC;
    TDrawGrid(Sender).Canvas.Font.Style := OFS;
    TDrawGrid(Sender).Canvas.Font.Color := OFC
  end
end;

procedure TFormMain.dgResultsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  // Save this as last selected cell.
  FLastCol := ACol;
  FLastRow := ARow
end;

end.
