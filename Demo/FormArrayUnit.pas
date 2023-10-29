unit FormArrayUnit;

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
  Forms, Classes, Controls, Grids, Windows;

type
  TFormArray = class(TForm)
    dwArray: TDrawGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure dwArrayDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure dwArrayDblClick(Sender: TObject);
  public
    AlignText: Byte;
    Items: TStringList;
    FieldName: String;
    FieldType: String;
  end;

implementation

{$R *.dfm}

uses
  Graphics, SysUtils, FormFieldUnit;

procedure TFormArray.dwArrayDblClick(Sender: TObject);
begin
  if dwArray.Row > 0 then
    with TFormField.Create(Self) do
    try
      eField.Text := FieldName;
      eIndex.Text := IntToStr(dwArray.Row - 1);
      eType.Text := FieldType;
      eValue.Text := Items[dwArray.Row - 1];

      ShowModal
    finally
      Free { TFormField }
    end
end;

procedure TFormArray.dwArrayDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  S: String;
  X, Y: Integer;
  OFS, FS: TFontStyles;
  OBC, OFC: TColor;
begin
  // Draw cells values.
  OBC := TDrawGrid(Sender).Canvas.Brush.Color;
  OFS := TDrawGrid(Sender).Canvas.Font.Style;
  OFC := TDrawGrid(Sender).Canvas.Font.Color;
  try
    // Text style is chosen based on column index.
    FS := OFS;
    Exclude(FS, fsBold);
    if (ACol = 0) or (ARow = 0) then
      Include(FS, fsBold);
    TDrawGrid(Sender).Canvas.Font.Style := FS;

    // Give first two columns a grey background.
    if ((ACol = 0) and not(gdFocused in State) and not(gdSelected in State)) or (ARow = 0) then
      TDrawGrid(Sender).Canvas.Brush.Color := $00EEEEEE;
    TDrawGrid(Sender).Canvas.FillRect(Rect);

    // Gets cell text.
    S := '';
    case ACol of
      0:
        if ARow > 0 then
          S := IntToStr(ARow - 1)
        else
          S := '#';

      1:
        if (ARow > 0) then begin
          if ARow - 1 < Items.Count then
            S := Items[ARow - 1]
          else
            S := '';
        end else
          S := 'Value'
    end;

    // Based on row, chose the position of text.
    X := Rect.Left + 4;
    if ARow = 0 then
      X := Rect.Left + ((Rect.Right - Rect.Left) - TDrawGrid(Sender).Canvas.TextWidth(S)) div 2
    else if AlignText = 1 then
      X := Rect.Right - 4 - TDrawGrid(Sender).Canvas.TextWidth(S);

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

procedure TFormArray.FormCreate(Sender: TObject);
begin
  // Initialize variables.
  AlignText := 0;
  Items := TStringList.Create
end;

procedure TFormArray.FormDestroy(Sender: TObject);
begin
  // Uninitialize variables.
  Items.Free
end;

procedure TFormArray.FormShow(Sender: TObject);
begin
  Caption := Format(Caption, [FieldType]);
  dwArray.RowCount := Items.Count + 1;
  dwArray.ColWidths[0] := 30;
  dwArray.ColWidths[1] := dwArray.ClientWidth - dwArray.ColWidths[0] - GetSystemMetrics(SM_CYVSCROLL)
end;

end.
