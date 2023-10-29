unit WMIObject;

interface

{******************************************************************************}
{                                                                              }
{     WMI query simple class for Delphi                                        }
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

uses
  Classes, WbemScripting_TLB, SysUtils;

type
  TWMIData = packed record
    Data: PChar;
    IsArray: Boolean;
  end;
  TWMIRecord = Array of TWMIData;
  TWMIRecordSet = Array of TWMIRecord;

  TWMIField = class;

  TWMIObject = class;

  TWMIFields = class(TCollection)
  private
    FOwner: TWMIObject;

    function GetItem(Index: Integer): TWMIField;
    procedure SetItem(Index: Integer; const Value: TWMIField);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TWMIObject); reintroduce;
    property Items[Index: Integer]: TWMIField read GetItem write SetItem; default;

    function Add: TWMIField;
  end;

  TWMIFieldType = (ftUnknown, ftBoolean, ftDateTime, ftExtended, ftInteger, ftString);

  TWMIField = class(TCollectionItem)
  private
    FFieldType: TWMIFieldType;
    FIndex: Integer;
    FName: String;
    FWMIObject: TWMIObject;

    function GetArray(Strings: TStrings): Boolean;
    function GetIsNull: Boolean;
    function GetIsArray: Boolean;
    function GetArrayCount: Integer;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TCollection); reintroduce;

    property FieldType: TWMIFieldType read FFieldType default ftUnknown;
    property Name: String read FName;

    property Count: Integer read GetArrayCount;

    property IsNull: Boolean read GetIsNull;
    property IsArray: Boolean read GetIsArray;

    function AsBoolean: Boolean;
    function AsDateTime(var AOffset: Integer; const ADefault: TDateTime = 0.0): TDateTime; overload;
    function AsDateTime(const ADefault: TDateTime = 0.0): TDateTime; overload;
    function AsExtended(const ADefault: Extended = 0.0): Extended;

    function AsInteger(const ADefault: Int64 = -1): Int64; overload;
    function AsInteger(const AIndex: Integer; const ADefault: Int64): Int64; overload;

    function AsString: String; overload;
    function AsString(const AIndex: Integer): String; overload;

    function AsRaw: String;
  end;

  TWMIMode = (wmimNone, wmimQuery, wmimClass);
  TWMIResult = (wmirOk, wmirNoConnection, wmirException, wmirEmptyResult,
    wmirEmptyObject, wmirAccessDenied, wmirFailed, wmirInvalidParameter,
    wmirInvalidQuery, wmirInvalidQueryType, wmirInvalidClass, wmirOutOfMemory);

  TWMIObject = class
  private
    FComputer: String;
    FCount: Integer;
    FCurrentRow: Integer;
    FError: Exception;
    FFields: TWMIFields;
    FNamespace: String;
    FPassword: String;
    FRecordSet: TWMIRecordSet;
    FUsername: String;
    FWbemLocator: TSWbemLocator;
    FWbemServices: ISWbemServices;
    FWMIMode: TWMIMode;
    FWMIObject: ISWbemObjectSet;

    function RecordData(const AFieldIndex: Integer): TWMIData;

    function OpenWMI: Boolean;
    procedure CloseWMI;
    function WMIIsOpen: Boolean;
    function WMIProcess: TWMIResult;
  protected
    function GetBoF: Boolean;
    function GetEoF: Boolean;
    function GetIsEmpty: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Close;

    function ExecQuery(const ASQL: String): TWMIResult;
    function InstancesOf(const AClass: String): TWMIResult;

    function First: Boolean;
    function Prev: Boolean;
    function Next: Boolean;
    function Last: Boolean;

    function FieldByName(const AName: String): TWMIField;
    function FieldExists(const AName: String): Boolean;

    property BoF: Boolean read GetBoF;
    property Computer: String read FComputer write FComputer;
    property Count: Integer read FCount;
    property EoF: Boolean read GetEoF;
    property Error: Exception read FError;
    property Fields: TWMIFields read FFields;
    property IsEmpty: Boolean read GetIsEmpty;
    property Mode: TWMIMode read FWMIMode;
    property Namespace: String read FNamespace write FNamespace;
    property Password: String read FPassword write FPassword;
    property Username: String read FUsername write FUsername;
  end;

implementation

uses
  ActiveX, Variants, ComObj;

{ TWMIObject }

function TWMIObject.GetBoF: Boolean;
begin
  Result := (FCount = 0) or (FCurrentRow = 0)
end;

procedure TWMIObject.Close;
var
  I, Y, Sz: Integer;
begin
  FCurrentRow := 0;

  FFields.Clear;

  for I := 0 to High(FRecordSet) do
    for Y := 0 to High(FRecordSet[I]) do
      if Assigned(FRecordSet[I, Y].Data) then
      begin
        Sz := (StrLen(FRecordSet[I, Y].Data) + 1) * SizeOf(Char);
        FreeMem(FRecordSet[I, Y].Data, Sz)
      end;

  Finalize(FRecordSet)
end;

procedure TWMIObject.CloseWMI;
begin
  FWMIObject := nil;

  if Assigned(FWbemLocator) then
  try
    FWbemLocator.Disconnect;
    FWbemLocator.Free
  except
  end;

  FWbemLocator := nil;
  FWbemServices := nil
end;

constructor TWMIObject.Create;
begin
  inherited Create;
  CoInitialize(nil);
  FComputer := '.';
  FCount := 0;
  FCurrentRow := 0;
  FError := nil;
  FFields := TWMIFields.Create(Self);
  FNamespace := 'root\CIMV2';
  FPassword := '';
  Finalize(FRecordSet);
  FUsername := '';
  FWbemLocator := nil;
  FWbemServices := nil;
  FWMIMode := wmimNone
end;

destructor TWMIObject.Destroy;
begin
  CloseWMI;
  CoUninitialize;
  Close;
  FFields.Free;
  inherited Destroy
end;

function TWMIObject.GetIsEmpty: Boolean;
begin
  Result := FCount = 0
end;

function TWMIObject.GetEoF: Boolean;
begin
  Result := (FCount = 0) or (FCurrentRow = FCount)
end;

function TWMIObject.ExecQuery(const ASQL: String): TWMIResult;
begin
  Result := wmirNoConnection;

  Close;

  if OpenWMI then
  try
    try
      FWMIMode := wmimQuery;

      FWMIObject := FWbemServices.ExecQuery(ASQL, 'WQL', wbemFlagReturnImmediately, nil);

      Result := WMIProcess
    except
      on E: Exception do
      begin
        FWMIMode := wmimNone;

        Result := wmirException
      end
    end
  finally
    CloseWMI
  end
end;

function TWMIObject.FieldByName(const AName: String): TWMIField;
var
  I: Integer;
  F: String;
begin
  Result := nil;
  F := UpperCase(Trim(AName));
  for I := 0 to FFields.Count - 1 do
    if UpperCase(Trim(FFields[I].FName)) = F then
    begin
      Result := FFields[I];
      Break
    end
end;

function TWMIObject.FieldExists(const AName: String): Boolean;
begin
  Result := FieldByName(AName) <> nil
end;

function TWMIObject.First: Boolean;
begin
  Result := not IsEmpty;
  if Result then
    FCurrentRow := 0
end;

function TWMIObject.InstancesOf(const AClass: String): TWMIResult;
begin
  Result := wmirNoConnection;

  Close;
  if OpenWMI then
  try
    try
      FWMIMode := wmimClass;

      FWMIObject := FWbemServices.InstancesOf(AClass, wbemFlagReturnImmediately or wbemQueryFlagShallow, nil) ;

      Result := WMIProcess
    except
      on E: Exception do
      begin
        FWMIMode := wmimNone;

        Result := wmirException
      end
    end
  finally
    CloseWMI
  end
end;

function TWMIObject.Last: Boolean;
begin
  Result := not IsEmpty;
  if Result then
    FCurrentRow := FCount - 1
end;

function TWMIObject.Next: Boolean;
begin
  Result := not IsEmpty and not EoF;
  if Result then
    Inc(FCurrentRow)
end;

function TWMIObject.OpenWMI: Boolean;
begin
  CloseWMI;

  try
    FWbemLocator := TSWbemLocator.Create(nil);

    FWbemServices := FWbemLocator.ConnectServer(FComputer, FNamespace, FUsername, FPassword, '', '', 0, nil);

    Result := WMIIsOpen
  except
    on E: Exception do
      Result := False
  end;

  if not Result then
    CloseWMI
end;

function TWMIObject.Prev: Boolean;
begin
  Result := not IsEmpty and not BoF;
  if Result then
    Dec(FCurrentRow)
end;

function TWMIObject.RecordData(const AFieldIndex: Integer): TWMIData;
begin
  Result.Data := nil;
  Result.IsArray := False;
  if (AFieldIndex >= 0) and (AFieldIndex < FFields.Count) and not EoF then
  begin
    Result.Data := FRecordSet[FCurrentRow, AFieldIndex].Data;
    Result.IsArray := FRecordSet[FCurrentRow, AFieldIndex].IsArray
  end
end;

function TWMIObject.WMIIsOpen: Boolean;
begin
  Result := Assigned(FWbemLocator) or Assigned(FWbemServices)
end;

function TWMIObject.WMIProcess: TWMIResult;

  function OleVariantToStringByType(AValue: OleVariant; const AType: WbemCimtypeEnum): String; inline;
  begin
    Result := '';
    case AType of
      wbemCimtypeSint8,
      wbemCimtypeUint8,
      wbemCimtypeSint16,
      wbemCimtypeUint16,
      wbemCimtypeSint32,
      wbemCimtypeUint32,
      wbemCimtypeSint64,
      wbemCimtypeUint64:
        Result := IntToStr(AValue);

      wbemCimtypeReal32,
      wbemCimtypeReal64:
        Result := FloatToStr(AValue);

      wbemCimtypeBoolean:
        Result := BoolToStr(AValue, True);

      wbemCimtypeDatetime,
      wbemCimtypeString,
      wbemCimtypeChar16:
        Result := AValue
    end
  end;

var
  I, X, Y, YF, YT, J: Integer;
  dwDummy: Cardinal;
  S, V: String;
  F: TWMIField;
  ObjectsEnum, PropertiesEnum: IEnumVariant;
  ovVar1, ovVar2: OleVariant;
  wmiObject: ISWbemObject;
  wmiProperty: ISWbemProperty;
  PropertySet: ISWbemPropertySet;
begin
  Result := wmirException;
  FCount := 0;
  try
    FCount := FWMIObject.Count
  except
    on E: EOleException do
    begin
      FError := E;

      case (E.ErrorCode) of
        HRESULT(wbemErrAccessDenied): Result := wmirAccessDenied;
        HRESULT(wbemErrFailed): Result := wmirFailed;
        HRESULT(wbemErrInvalidParameter): Result := wmirInvalidParameter;
        HRESULT(wbemErrInvalidQuery):
        begin
          Result := wmirInvalidQuery;
          if FWMIMode <> wmimQuery then
            Result := wmirException
        end;
        HRESULT(wbemErrInvalidQueryType):
        begin
          Result := wmirInvalidQueryType;
          if FWMIMode <> wmimQuery then
            Result := wmirException
        end;
        HRESULT(wbemErrInvalidClass): Result := wmirInvalidClass;
        HRESULT(wbemErrOutOfMemory): Result := wmirOutOfMemory
      end;

      Exit
    end
  end;

  if FCount = 0 then
  begin
    Result := wmirEmptyResult;
    Exit
  end;

  Result := wmirOk;

  // Scorro tra tutti gli oggetti.
  I := 0;
  ObjectsEnum := FWMIObject._NewEnum As IEnumVariant;
  try
    while ObjectsEnum.Next(1, ovVar1, dwDummy) = S_OK do
    try
      // Prendo le l'oggetto e le sue proprietà
      wmiObject := IUnknown(ovVar1) As SWBemObject;
      PropertySet := wmiObject.Properties_;

      if PropertySet.Count = 0 then
      begin
        Result := wmirEmptyObject;
        Exit
      end;

      PropertiesEnum := PropertySet._NewEnum As IEnumVariant;
      try
        // Alla prima interazione popolo gli array e i field
        if I = 0 then
        begin

          // Alloco la memoria.
          SetLength(FRecordSet, FCount, PropertySet.Count);
          // TODO: verificare questo.
          for X := 0 to FCount - 1 do
            for Y := 0 to PropertySet.Count - 1 do
            begin
              FRecordSet[X, Y].Data := nil;
              FRecordSet[X, Y].IsArray := False;
            end;

          // Aggiungo i campi
          X := 0;
          while PropertiesEnum.Next(1, ovVar2, dwDummy) = S_OK do
          try
            wmiProperty := IUnknown(ovVar2) As SWBemProperty;
            try
              F := FFields.Add;
              F.FIndex := X;
              F.FName := wmiProperty.Name;
              F.FFieldType := ftUnknown;
              case wmiProperty.CIMType of
                wbemCimtypeBoolean:
                  F.FFieldType := ftBoolean;
                wbemCimtypeDatetime:
                  F.FFieldType := ftDateTime;
                wbemCimtypeReal32,
                wbemCimtypeReal64:
                  F.FFieldType := ftExtended;
                wbemCimtypeSint8,
                wbemCimtypeUint8,
                wbemCimtypeSint16,
                wbemCimtypeUint16,
                wbemCimtypeSint32,
                wbemCimtypeUint32,
                wbemCimtypeSint64,
                wbemCimtypeUint64:
                  F.FFieldType := ftInteger;
                wbemCimtypeString,
                wbemCimtypeChar16:
                  F.FFieldType := ftString
              end
            finally
              wmiProperty := nil
            end;

            Inc(X)

          finally
            VarClear(ovVar2)
          end;

          // Torno all'inizio
          PropertiesEnum.Reset
        end;

        // Salva i dati nel recordset.
        X := 0;
        while PropertiesEnum.Next(1, ovVar2, dwDummy) = S_OK do
        try
          S := '';

          wmiProperty := IUnknown(ovVar2) As SWBemProperty;
          try
            if not VarIsNull(wmiProperty.Get_Value) then
            begin

              FRecordSet[I, X].IsArray := VarIsArray(wmiProperty.Get_Value);

              if FRecordSet[I, X].IsArray then
              begin

                YF := VarArrayLowBound(wmiProperty.Get_Value, 1);
                YT := VarArrayHighBound(wmiProperty.Get_Value, 1);
                for J := YF to YT do
                begin
                  V := OleVariantToStringByType(wmiProperty.Get_Value[J], wmiProperty.CIMType);
                  if wmiProperty.CIMType = wbemCimtypeString then
                    V := StringReplace(V, #$7C, '#$7C', [rfReplaceAll]); { Do not localize. }
                  S := S + V;
                  if J < YT then
                    S := S + #$7C { Do not localize. }
                end

              end else
                S := OleVariantToStringByType(wmiProperty.Get_Value, wmiProperty.CIMType)

            end
          finally
            wmiProperty := nil
          end;

          // Metto in memoria i dati.
          if Length(S) > 0 then
          begin
            // TODO: mettere a posto questo.
            Y := (Length(S) + 1) * SizeOf(Char);
            FRecordSet[I, X].Data := GetMemory(Y);
            FillChar(FRecordSet[I, X].Data^, Y, 0);
            Move(S[1], FRecordSet[I, X].Data^, Length(S) * SizeOf(Char))
          end;

          Inc(X)

        finally
          VarClear(ovVar2)
        end

      finally
        wmiObject := nil;
        PropertySet := nil;
        PropertiesEnum := nil
      end;

      Inc(I)

    finally
      if Result <> wmirOk then
        Close;
      VarClear(ovVar1);
      VarClear(ovVar2)
    end
  finally
    ObjectsEnum := nil
  end
end;

{ TWMIFields }

function TWMIFields.Add: TWMIField;
begin
  Result := TWMIField(inherited Add);

  Result.FWMIObject := FOwner
end;

procedure TWMIFields.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TWMIFields then
  begin
    Clear;
    for I := 0 to TWMIFields(Source).Count - 1 do
      Add.Assign(TWMIFields(Source)[I])
  end else
    inherited Assign(Source)
end;

constructor TWMIFields.Create(AOwner: TWMIObject);
begin
  inherited Create(TWMIField);

  FOwner := AOwner
end;

function TWMIFields.GetItem(Index: Integer): TWMIField;
begin
  Result := TWMIField(inherited GetItem(Index))
end;

procedure TWMIFields.SetItem(Index: Integer; const Value: TWMIField);
begin
  inherited SetItem(Index, Value)
end;

{ TWMIField }

function TWMIField.AsBoolean: Boolean;
begin
  Result := AsString = DefaultTrueBoolStr
end;

function TWMIField.AsDateTime(const ADefault: TDateTime): TDateTime;
var
  iOffset: Integer;
begin
  Result := AsDateTime(iOffset, ADefault)
end;

function TWMIField.AsDateTime(var AOffset: Integer; const ADefault: TDateTime): TDateTime;
var
  S: String;

  function _DT(var W: Word; const Len: Integer = 2): Boolean;
  begin
    W := $ffff;
    if Length(S) >= Len then
    begin
      W := Word(StrToIntDef(Copy(S, 1, Len), W));
      Delete(S, 1, Len)
    end;
    Result := W <> $ffff
  end;

var
  C: Char;
  DT: TDateTime;
  Y, M, D, H, N, Sc, MS, Dummy: Word;
begin
  AOffset := 0;
  Result := ADefault;
  S := AsString;

  if _DT(Y, 4) and _DT(M) and _DT(D) and _DT(H) and _DT(N) and _DT(Sc) then
  begin
    MS := 0;

    if Copy(S, 1, 1) = '.' then
    begin
      Delete(S, 1, 1);
      if not _DT(MS, 3) or not _DT(Dummy, 3) then
        Exit
    end;

    C := (Copy(S, 1, 1) + #32)[1];
    if CharInSet(C, ['+', '-']) then
    begin
      Delete(S, 1, 1);
      AOffset := StrToIntDef(S, -1);

      if (AOffset < 0) or (AOffset > 999) then
        Exit;

      if C = '-' then
        AOffset := -AOffset
    end;

    if TryEncodeDate(Y, M, D, Result) and TryEncodeTime(H, N, Sc, MS, DT) then
      ReplaceTime(Result, DT)
    else
      Result := ADefault
  end
end;

function TWMIField.AsExtended(const ADefault: Extended): Extended;
begin
  Result := StrToFloatDef(AsString, ADefault)
end;

function TWMIField.AsInteger(const AIndex: Integer; const ADefault: Int64): Int64;
begin
  Result := StrToInt64Def(AsString(AIndex), ADefault)
end;

function TWMIField.AsRaw: String;
begin
  Result := '';
  if not IsNull then
    Result := StrPas(FWMIObject.RecordData(FIndex).Data)
end;

function TWMIField.AsInteger(const ADefault: Int64): Int64;
begin
  Result := StrToInt64Def(AsString, ADefault)
end;

procedure TWMIField.Assign(Source: TPersistent);
begin
  if Source is TWMIField then
  begin
    FFieldType := TWMIField(Source).FFieldType;
    FName := TWMIField(Source).FName
  end else
    inherited Assign(Source)
end;

function TWMIField.AsString(const AIndex: Integer): String;
var
  SL: TStringList;
begin
  Result := '';
  if (AIndex >= 0) and IsArray then
  begin
    SL := TStringList.Create;
    try
      if GetArray(SL) and (AIndex < SL.Count) then
        Result := SL[AIndex]
    finally
      SL.Free
    end
  end
end;

function TWMIField.AsString: String;
begin
  Result := '';
  if (not IsNull) and (not IsArray) then
    Result := StrPas(FWMIObject.RecordData(FIndex).Data)
end;

constructor TWMIField.Create(AOwner: TCollection);
begin
  inherited Create(AOwner);

  FFieldType := ftString;
  FIndex := -1;
  FName := '';
  FWMIObject := nil
end;

function TWMIField.GetArray(Strings: TStrings): Boolean;
begin
  Result := False;

  if not Assigned(Strings) then
    Exit;

  Strings.BeginUpdate;
  try
    Strings.Clear;
    if IsArray then
    begin
      Strings.Text := StringReplace(StrPas(FWMIObject.RecordData(FIndex).Data), #$7C, Strings.LineBreak, [rfReplaceAll]); { Do not localize. }
      Strings.Text := StringReplace(Strings.Text, '#$7C', #$7C, [rfReplaceAll, rfIgnoreCase]); { Do not localize. }
      Result := True
    end
  finally
    Strings.EndUpdate
  end
end;

function TWMIField.GetArrayCount: Integer;
var
  SL: TStringList;
begin
  Result := 0;

  if not IsArray then
    Exit;

  SL := TStringList.Create;
  try
    if GetArray(SL) then
      Result := SL.Count
  finally
    SL.Free
  end
end;

function TWMIField.GetIsArray: Boolean;
begin
  Result := (not IsNull) and FWMIObject.RecordData(FIndex).IsArray
end;

function TWMIField.GetIsNull: Boolean;
begin
  Result := (FFieldType = ftUnknown) or not Assigned(FWMIObject.RecordData(FIndex).Data)
end;

end.