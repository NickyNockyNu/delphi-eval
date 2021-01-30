unit EvalLib;

interface

uses
  System.Math,
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  System.Generics.Defaults;

{$TYPEINFO ON}

type
{$REGION 'TRttiContextHelper'}
  TRttiContextHelper = record helper for TRttiContext
  public
    function FindPublishedType(const Name: String): TRttiType;
  end;
{$ENDREGION}

{$REGION 'TValueHelper'}
  TValueHelper = record helper for TValue
    function HumanReadable: String;
  end;
{$ENDREGION}

{$REGION 'TObjectRttiHelper'}
  TObjectRttiHelper = class helper for TObject
  private
    function GetThis: TObject; inline;
  public
    class procedure Use;

    function  ReadProperty (const Name: String): TValue;
    procedure WriteProperty(const Name: String; Value: TValue);

    function InvokeMethod(const Name: String; Args: TArray<TValue>): TValue;

    function IsReadable (const Name: String): Boolean;
    function IsWriteable(const Name: String): Boolean;
  published
    property This: TObject read GetThis;
  end;
{$ENDREGION}

  EReadOnlyProperty = class(Exception);
  EUnknownProperty  = class(Exception);
  EUnknownMethod    = class(Exception);
  EUnknownClass     = class(Exception);

{$REGION 'TExpandableObject'}
  TExpandableObject = class
  public
    function  ReadExpandedProperty (const Name: String): TValue;        virtual;
    procedure WriteExpandedProperty(const Name: String; Value: TValue); virtual;

    function InvokeExpandedMethod(const Name: String; Args: TArray<TValue>): TValue; virtual;

    function IsExpandedReadable (const Name: String): Boolean; virtual;
    function IsExpandedWriteable(const Name: String): Boolean; virtual;
  end;
{$ENDREGION}

{$REGION 'TClassFactory'}
  TClassFactory = class(TExpandableObject)
    function ReadExpandedProperty(const Name: String): TValue;  override;
    function IsExpandedReadable  (const Name: String): Boolean; override;
  published
    function New(const Name: String): TValue; inline;
  end;
{$ENDREGION}

{$REGION 'TVariableStore'}
  TVariableStore = class(TExpandableObject)
  private
    fVariables: TDictionary<String, TValue>;

    fAutoDeclare: Boolean;
  public
    constructor Create;
    destructor  Destroy; override;

    function  ReadExpandedProperty (const Name: String): TValue;        override;
    procedure WriteExpandedProperty(const Name: String; Value: TValue); override;

    function IsExpandedReadable (const Name: String): Boolean; override;
    function IsExpandedWriteable(const Name: String): Boolean; override;

    procedure Declare(const Name: String; Value: TValue); overload;
  published
    procedure Delete (const Name: String); inline;
    procedure Declare(const Name: String); overload;

    property Variables: TDictionary<String, TValue> read fVariables;

    property AutoDeclare: Boolean read fAutoDeclare write fAutoDeclare;
  end;
{$ENDREGION}

{$REGION 'TEvaluator'}
  TEvaluator = class(TVariableStore)
  private
    fNamespace: TArray<TObject>;
  public
    constructor Create;
  published
    procedure Push(const Namespace: TObject); inline;
    procedure Pop;                            inline;

    function ReadValue (const Name: String; var   Value: TValue): Boolean;
    function WriteValue(const Name: String; const Value: TValue): Boolean;

    function Eval(const Expression: String): TValue;
  end;
{$ENDREGION}

  EEvaluator = class(Exception);

var
  RttiContext: TRttiContext;

implementation

{$REGION 'TRttiContextHelper'}
function TRttiContextHelper.FindPublishedType(const Name: String): TRttiType;
var
  i:    Integer;
  f, n: String;
begin
  f := lowercase(Name);

  for Result in RttiContext.GetTypes do
  begin
    n := lowercase(Result.QualifiedName);

    for i := length(n) downto 1 do
      if n[i] = '.' then break;

    n := copy(n, i + 1, length(n));

    if n = f then exit;
  end;

  Result := nil;
end;
{$ENDREGION}

{$REGION 'TValueHelper'}
function TValueHelper.HumanReadable;
begin
  try
    Exit(AsString);
  except
    case Kind of
      tkInteger:     Result := IntToStr(AsInteger);
      tkEnumeration: Result := BoolToStr(AsBoolean);
      tkFloat:       Result := FloatToStr(AsExtended);
      tkSet:         Result := '<set>';
      tkClass:       Result := '<class>';
      tkMethod:      Result := '<method>';
      tkVariant:     Result := AsVariant;
      tkArray:       Result := '<array>';
      tkRecord:      Result := '<record>';
      tkInterface:   Result := '<interface>';
      tkInt64:       Result := IntToStr(AsInt64);
      tkDynArray:    Result := '<dynamicarray>';
      tkClassRef:    Result := '<classref>';
      tkPointer:     Result := '<pointer>';
      tkProcedure:   Result := '<procedure>';
    else
      Result := '<unknown>';
    end;
  end;
end;
{$ENDREGION}

{$REGION 'TObjectRttiHelper'}
function TObjectRttiHelper.GetThis: TObject;
begin
  Result := Self;
end;

class procedure TObjectRttiHelper.Use;
asm
  nop
end;

function TObjectRttiHelper.ReadProperty(const Name: String): TValue;
var
  RttiProperty: TRttiProperty;
begin
  TMonitor.Enter(Self);

  try
    if Self is TExpandableObject then
      with Self as TExpandableObject do
      begin
        if IsExpandedReadable(Name) then
          try
            Result := ReadExpandedProperty(Name);
            Exit;
          except end;
      end;

    RttiProperty := RttiContext.GetType(Self.ClassType).GetProperty(Name);

    if (RttiProperty = nil) or (RttiProperty.Visibility in [mvPrivate, mvProtected]) then
      raise EUnknownProperty.Create(Name);

    Result := RttiProperty.GetValue(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TObjectRttiHelper.WriteProperty(const Name: String; Value: TValue);
var
  RttiProperty: TRttiProperty;
begin
  TMonitor.Enter(Self);

  try
    if Self is TExpandableObject then
      with Self as TExpandableObject do
      begin
        if IsExpandedWriteable(Name) then
          try
            WriteExpandedProperty(Name, Value);
            Exit;
          except end;
      end;

    RttiProperty := RttiContext.GetType(Self.ClassType).GetProperty(Name);

    if (RttiProperty = nil) or (RttiProperty.Visibility in [mvPrivate, mvProtected]) then
      raise EUnknownProperty.Create(Name);

    if not RttiProperty.IsWritable then
      raise EReadOnlyProperty.Create(Name);

    RttiProperty.SetValue(Self, Value);
  finally
    TMonitor.Exit(Self);
  end;
end;

function TObjectRttiHelper.InvokeMethod(const Name: String; Args: TArray<TValue>): TValue;
var
  RttiMethod: TRttiMethod;
begin
  TMonitor.Enter(Self);

  try
    if Self is TExpandableObject then
      with Self as TExpandableObject do
      begin
        if IsExpandedReadable(Name) then
          try
            Result := InvokeExpandedMethod(Name, Args);
            Exit;
          except end;
      end;

    RttiMethod := RttiContext.GetType(Self.ClassType).GetMethod(Name);

    if (RttiMethod = nil) or (RttiMethod.Visibility in [mvPrivate, mvProtected]) then
      raise EUnknownMethod.Create(Name);

    Result := RttiMethod.Invoke(Self, Args);
  finally
    TMonitor.Exit(Self);
  end;
end;

function TObjectRttiHelper.IsReadable(const Name: String): Boolean;
var
  RttiProperty: TRttiProperty;
  RttiMethod:   TRttiMethod;
begin
  if Self is TExpandableObject then
  begin
    Result := TExpandableObject(Self).IsExpandedReadable(Name);
    if Result then Exit;
  end;

  RttiProperty := RttiContext.GetType(Self.ClassType).GetProperty(Name);

  if RttiProperty <> nil then
    Result := (RttiProperty.Visibility in [mvPublic, mvPublished]) and RttiProperty.IsReadable
  else
  begin
    RttiMethod := RttiContext.GetType(Self.ClassType).GetMethod(Name);
    Result := (RttiMethod <> nil) and (RttiMethod.Visibility in [mvPublic, mvPublished]);
  end;
end;

function TObjectRttiHelper.IsWriteable(const Name: String): Boolean;
var
  RttiProperty: TRttiProperty;
begin
  if Self is TExpandableObject then
  begin
    Result := TExpandableObject(Self).IsExpandedWriteable(Name);
    if Result then Exit;
  end;

  RttiProperty := RttiContext.GetType(Self.ClassType).GetProperty(Name);

  Result := (RttiProperty <> nil) and (RttiProperty.Visibility in [mvPublic, mvPublished]) and RttiProperty.IsWritable;
end;
{$ENDREGION}

{$REGION 'TExpandableObject'}
function TExpandableObject.ReadExpandedProperty(const Name: String): TValue;
begin
  raise EUnknownProperty.Create(Name);
end;

procedure TExpandableObject.WriteExpandedProperty(const Name: String; Value: TValue);
begin
  raise EUnknownProperty.Create(Name);
end;

function TExpandableObject.InvokeExpandedMethod(const Name: String; Args: TArray<TValue>): TValue;
begin
  raise EUnknownMethod.Create(Name);
end;

function TExpandableObject.IsExpandedReadable(const Name: String): Boolean;
begin
  Result := False;
end;

function TExpandableObject.IsExpandedWriteable(const Name: String): Boolean;
begin
  Result := False;
end;
{$ENDREGION}

{$REGION 'TClassFactory'}
function TClassFactory.ReadExpandedProperty(const Name: String): TValue;
var
  t: TRttiType;
begin
  TMonitor.Enter(Self);

  try
    t := RttiContext.FindPublishedType(Name);

    if t = nil then Exit(inherited);

    Result := TValue.From<TObject>(t.GetMethod('NewInstance').Invoke(t.AsInstance.MetaclassType, []).AsObject);
  finally
    TMonitor.Exit(Self);
  end;
end;

function TClassFactory.IsExpandedReadable(const Name: String): Boolean;
begin
  Result := RttiContext.FindPublishedType(Name) <> nil;
end;

function TClassFactory.New;
begin
  Result := ReadExpandedProperty(Name);
end;
{$ENDREGION}

{$REGION 'TVariableStore'}
constructor TVariableStore.Create;
begin
  inherited;

  fVariables := TDictionary<String, TValue>.Create;

  fAutoDeclare := False;
end;

destructor TVariableStore.Destroy;
begin
  fVariables.Free;

  inherited;
end;

function TVariableStore.ReadExpandedProperty(const Name: String): TValue;
begin
  if not fVariables.ContainsKey(LowerCase(Name)) then
  begin
    if fAutoDeclare then
      try
        Result := inherited;
      except
        Result := TValue.Empty;
      end
    else
      Result := inherited;
  end
  else
    Result := fVariables[LowerCase(Name)];
end;

procedure TVariableStore.WriteExpandedProperty(const Name: String; Value: TValue);
begin
  if not fVariables.ContainsKey(LowerCase(Name)) then
  begin
    if fAutoDeclare then
      try
        inherited;
      except
        fVariables.Add(LowerCase(Name), Value);
      end
    else
      inherited;
  end
  else
    fVariables[Name] := Value;
end;

function TVariableStore.IsExpandedReadable(const Name: String): Boolean;
begin
  if fVariables.ContainsKey(LowerCase(Name)) or fAutoDeclare then
    Result := True
  else
    Result := inherited;
end;

function TVariableStore.IsExpandedWriteable(const Name: String): Boolean;
begin
  if fVariables.ContainsKey(LowerCase(Name)) or fAutoDeclare then
    Result := True
  else
    Result := inherited;
end;

procedure TVariableStore.Delete(const Name: String);
begin
  fVariables.Remove(LowerCase(Name));
end;

procedure TVariableStore.Declare(const Name: String);
begin
  Declare(Name, 0);
end;

procedure TVariableStore.Declare(const Name: String; Value: TValue);
begin
  if not fVariables.ContainsKey(LowerCase(Name)) then
    fVariables.Add(LowerCase(Name), Value)
  else
    fVariables[LowerCase(Name)] := Value;
end;
{$ENDREGION}

{$REGION 'TEvaluator'}
constructor TEvaluator.Create;
begin
  inherited;

  Push(Self);
end;

procedure TEvaluator.Push(const Namespace: TObject);
begin
  SetLength(fNamespace, Length(fNamespace) + 1);
  fNamespace[High(fNamespace)] := Namespace;
end;

procedure TEvaluator.Pop;
begin
  if Length(fNamespace) = 1 then exit;
  SetLength(fNamespace, Length(fNamespace) - 1);
end;

function TEvaluator.ReadValue(const Name: String; var Value: TValue): Boolean;
  function ReadValueNS(Namespace: TObject): TValue;
  var
    i: Integer;
    p: TArray<TValue>;
    s: String;
    c: Char;
    f: Boolean;

    procedure Whitespace;
    begin
      repeat
        if not CharInSet(Name[i], [#32, #9]) then Exit;
        Inc(i);
      until i > Length(Name);
    end;

    procedure OverName;
    begin
      Whitespace;

      s := '';

      repeat
        c := Name[i];

        if not CharInSet(c, ['A'..'Z', 'a'..'z', '0'..'9', '_']) then Exit;

        s := s + c;

        Inc(i);
      until i > Length(Name);

      s := Lowercase(s);
    end;

    procedure OverParams;
    var
      q:  Boolean;
      n:  Integer;
      v:  String;
      l:  TValue;
    begin
      q := False;
      n := 0;
      v := '';

      SetLength(p, 0);

      Inc(i);

      repeat
        if not q then Whitespace;

        c := Name[i];

        case c of
          '"': q := not q;

          '(': if not q then Inc(n);
          ')': if not q then Dec(n);

          ',': if (not q) and (n = 0) then
               begin
                 l := Eval(v);

                 SetLength(p, Length(p) + 1);
                 p[High(p)] := l;

                 Inc(i);
                 v := '';
              end;
        end;

        if i > Length(Name) then Break;

        v := v + Name[i];

        Inc(i);
      until (n = -1) or (i > Length(Name));

      if (n = -1) and (v[Length(v)] = ')') then v := Copy(v, 1, Length(v) - 1);

      if Length(v) > 0 then
      begin
        l := Eval(v);

        SetLength(p, Length(p) + 1);
        p[High(p)] := l;
      end;
    end;
  begin
    Result := TValue.From<TObject>(Namespace);

    i := 1;

    repeat
      OverName;

      if Result.AsObject.IsReadable(s) then
      begin
        f := c = '(';

        if f then
        begin
          OverParams;
          c := Name[i];
        end;

        if f then
          Result := Result.AsObject.InvokeMethod(s, p)
        else
          Result := Result.AsObject.ReadProperty(s);
      end
      else
        raise EEvaluator.Create('Cannot read value "' + s + '"');

      Inc(i);
    until (c <> '.') or (i > Length(Name));
  end;
var
  i: Integer;
begin
  for i := High(fNamespace) downto 0 do
    try
      Value := ReadValueNS(fNamespace[i]);
      Exit(True);
    except
    end;

  Result := False;
end;

function TEvaluator.WriteValue(const Name: String; const Value: TValue): Boolean;
  procedure WriteValueNS(Namespace: TObject);
  var
    i: Integer;
    s: String;
    p: String;
    v: TValue;
  begin
    for i := Length(Name) downto 1 do
      if not CharInSet(Name[i], ['A'..'Z', 'a'..'z', '0'..'9', '_']) then Break;

    s := Lowercase(Copy(Name, i + 1, Length(Name)));
    p := Copy(Name, 1, i - 1);

    if Length(s) = 0 then raise EEvaluator.Create('Syntax error');

    if Length(p) = 0 then
      v := TValue.From<TObject>(Namespace)
    else if not ReadValue(p, v) then
      raise EEvaluator.Create(p);

    if not v.AsObject.IsWriteable(s) then
      raise EEvaluator.Create(p);

    v.AsObject.WriteProperty(s, Value);
  end;
var
  i: Integer;
begin
  for i := High(fNamespace) downto 0 do
    try
      WriteValueNS(fNamespace[i]);
      Exit(True);
    except
    end;

  Result := False;
end;

function BinToInt(const S: String): Cardinal;
var
  i, j: Integer;
begin
  Result := 0;

  if (Length(S) = 0) or (Pos(S[1], '01') = 0) then
    Exit(0);

  for i := 1 to Length(S) do
  begin
    j := Pos(S[i], '01');

    if j = 0 then
      Exit(0);

    Result := Result * 2 + (j - 1);
  end;
end;

function TEvaluator.Eval(const Expression: String): TValue;
var
  s, v: String;
  i:    Integer;

  function Compare: TValue; forward;

  procedure Whitespace;
  begin
    repeat
      if not CharInSet(s[i], [#32, #9]) then Exit;
      Inc(i);
    until i > Length(s);
  end;

  function CheckSymbol(Symbol: String; Follow: String = ''; State: Boolean = True): Boolean;
  begin
    Whitespace;

    Result := Copy(s, i, Length(Symbol)) = Symbol;

    if Result and (Length(Follow) > 0) then
      Result := (Pos(s[i + Length(Symbol)], Follow) = 0) = State;

    if Result then Inc(i, Length(Symbol));
  end;

  function GetConst(var Value: TValue): Boolean;
  var
    c: String;
  begin
    Whitespace;

    Result := True;

    case s[i] of
      '0'..'9':
      begin
        c := s[i];

        Inc(i);

        while CharInSet(s[i], ['0'..'9', '.']) and (i <= Length(s)) do
        begin
          c := c + s[i];
          Inc(i);
        end;

        if pos('.', c) = 0 then
          Value := TValue.From<Integer>(StrToInt(c))
        else
          Value := TValue.From<Single>(StrToFloat(c));
      end;

      '$':
      begin
        c := s[i];

        Inc(i);

        while CharInSet(s[i], ['0'..'9', 'a'..'f', 'A'..'F']) and (i <= Length(s)) do
        begin
          c := c + s[i];
          Inc(i);
        end;

        Value := TValue.From<Cardinal>(StrToInt(c));
      end;

      '#':
      begin
        c := '';//s[i];

        Inc(i);

        while CharInSet(s[i], ['0', '1']) and (i <= Length(s)) do
        begin
          c := c + s[i];
          Inc(i);
        end;

        Value := TValue.From<Cardinal>(BinToInt(c));
      end;

      '"':
      begin
        Inc(i);

        c := '';

        while s[i] <> '"' do
        begin
          c := c + s[i];

          Inc(i);

          if i > Length(s) then
            raise EEvaluator.Create('Expected "');
        end;

        Inc(i);

        Value := TValue.From<String>(c);
      end;
    else
      Result := False;
    end;

    if Result then v := '';
  end;

  function GetValue(var Value: TValue): Boolean;
  var
    c: Char;

    procedure OverName;
    begin
      Whitespace;

      repeat
        c := s[i];

        if not CharInSet(c, ['A'..'Z', 'a'..'z', '0'..'9', '_']) then exit;

        v := v + c;

        Inc(i);
      until i > Length(s);
    end;

    procedure OverParams;
    var
      q: Boolean;
      n: Integer;
    begin
      Whitespace;

      q := False;
      n := 0;

      repeat
        c := s[i];

        case c of
          '''': q := not q;

          '(': if not q then Inc(n);
          ')': if not q then
               begin
                 Dec(n);

                 if n = 0 then
                 begin
                   v := v + c; Inc(i);
                   Break;
                 end;
               end;
        end;

        v := v + c;

        Inc(i);
      until i > Length(s);
    end;
  begin
    Whitespace;

    Result := False;

    if not CharInSet(s[i], ['A'..'Z', 'a'..'z', '_']) then exit;

    v := '';

    repeat
      OverName;

      if i > Length(s) then Break;

      if c = '.' then
      begin
        v := v + c; Inc(i);

        Continue;
      end
      else
      begin
        Whitespace;

        if c = '(' then
        begin
          Whitespace;

          OverParams;

          if c = '.' then
          begin
            v := v + c; Inc(i);

            Continue;
          end;
        end

        else Break;
      end;
    until i > Length(s);

    Result := ReadValue(v, Value);
  end;

  function IsNot: Boolean;
  begin
    Whitespace;

    Result := s[i] = '!';
    if Result then Inc(i);
  end;

  function IsNeg: Boolean;
  begin
    Whitespace;

    Result := s[i] = '-';
    if Result then Inc(i);
  end;

  function Bracket: TValue;
  var
    nt: Boolean;
    ng: Boolean;
  begin
    Whitespace;

    nt := IsNot;
    ng := IsNeg;

    v := '';

    if CheckSymbol('(') then
    begin
      Result := Compare;

      if not CheckSymbol(')') then raise EEvaluator.Create('Expected )');
    end
    else
    begin
      if not GetConst(Result) then
        if not GetValue(Result) then
          raise EEvaluator.Create('Expected value');
    end;

    if ng then Result := TValue.FromVariant(-Result.AsVariant);
    if nt then Result := TValue.From<Boolean>(not Result.AsBoolean);
  end;

  function Assign: TValue;
  var
    n: String;

    procedure Check;
    begin
      if Length(v) = 0 then
        raise EEvaluator.Create('Expected variable');

      n := v;
    end;

    procedure GetValue;
    begin
      if not ReadValue(n, Result) then
        raise EEvaluator.Create('Expected variable');
    end;

    procedure SetValue;
    begin
      if not WriteValue(n, Result) then
        raise EEvaluator.Create('Expected variable');
    end;
  begin
    Whitespace;

    Result := Bracket;

    if CheckSymbol('=', '=') then
    begin
      Check;

      Result := Compare;
      SetValue;
    end
    else if CheckSymbol('*=') then
    begin
      Check;

      GetValue;
      Result := TValue.FromVariant(Result.AsVariant * Compare.AsVariant);
      SetValue;
    end
    else if CheckSymbol('/=') then
    begin
      Check;

      GetValue;
      Result := TValue.FromVariant(Result.AsVariant / Compare.AsVariant);
      SetValue;
    end
    else if CheckSymbol('\=') then
    begin
      Check;

      GetValue;
      Result := TValue.FromVariant(Result.AsVariant div Compare.AsVariant);
      SetValue;
    end
    else if CheckSymbol('%=') then
    begin
      Check;

      GetValue;
      Result := TValue.FromVariant(Result.AsVariant mod Compare.AsVariant);
      SetValue;
    end
    else if CheckSymbol('&=') then
    begin
      Check;

      GetValue;
      Result := TValue.FromVariant(Result.AsVariant and Compare.AsVariant);
      SetValue;
    end
    else if CheckSymbol('|=') then
    begin
      Check;

      GetValue;
      Result := TValue.FromVariant(Result.AsVariant or Compare.AsVariant);
      SetValue;
    end
    else if CheckSymbol('^=') then
    begin
      Check;

      GetValue;
      Result := TValue.FromVariant(Result.AsVariant xor Compare.AsVariant);
      SetValue;
    end
    else if CheckSymbol('+=') then
    begin
      Check;

      GetValue;
      Result := TValue.FromVariant(Result.AsVariant + Compare.AsVariant);
      SetValue;
    end
    else if CheckSymbol('-=') then
    begin
      Check;

      GetValue;
      Result := TValue.FromVariant(Result.AsVariant - Compare.AsVariant);
      SetValue;
    end
    else if CheckSymbol('++') then
    begin
      Check;

      GetValue;
      Result := TValue.FromVariant(Result.AsVariant + 1);
      SetValue;

      s := n + Copy(s, i, Length(s)); i := 1;
      Result := Compare;
    end
    else if CheckSymbol('--') then
    begin
      Check;

      GetValue;
      Result := TValue.FromVariant(Result.AsVariant - 1);
      SetValue;

      s := n + Copy(s, i, Length(s)); i := 1;
      Result := Compare;
    end
  end;

  function MulDiv: TValue;
  begin
    Whitespace;

    Result := Assign;

    while True do
           if CheckSymbol('*', '=') then Result := TValue.FromVariant(Result.AsVariant *   Assign.AsVariant)
      else if CheckSymbol('/', '=') then Result := TValue.FromVariant(Result.AsVariant /   Assign.AsVariant)
      else if CheckSymbol('\', '=') then Result := TValue.FromVariant(Result.AsVariant div Assign.AsVariant)
      else if CheckSymbol('%', '=') then Result := TValue.FromVariant(Result.AsVariant mod Assign.AsVariant)
      else if CheckSymbol('^')      then Result := TValue.FromVariant(Power(Result.AsVariant, Assign.AsVariant))
      else Break;
  end;

  function Bitwise: TValue;
  begin
    Whitespace;

    Result := MulDiv;

    while True do
           if CheckSymbol('<<')      then Result := TValue.From<Cardinal>(Result.AsOrdinal shl MulDiv.AsOrdinal)
      else if CheckSymbol('>>')      then Result := TValue.From<Cardinal>(Result.AsOrdinal shr MulDiv.AsOrdinal)
      else if CheckSymbol('&', '&=') then Result := TValue.From<Cardinal>(Result.AsOrdinal and MulDiv.AsOrdinal)
      else if CheckSymbol('|', '|=') then Result := TValue.From<Cardinal>(Result.AsOrdinal or  MulDiv.AsOrdinal)
      else if CheckSymbol('^', '=')  then Result := TValue.From<Cardinal>(Result.AsOrdinal xor MulDiv.AsOrdinal)
      else Break;
  end;

  function AddSub: TValue;
  begin
    Whitespace;

    Result := Bitwise;

    while True do
           if CheckSymbol('+', '+=') then Result := TValue.FromVariant(Result.AsVariant + Bitwise.AsVariant)
      else if CheckSymbol('-', '-=') then Result := TValue.FromVariant(Result.AsVariant - Bitwise.AsVariant)
      else Break;
  end;

  function Compare: TValue;
  begin
    Whitespace;

    Result := AddSub;

    while True do
           if CheckSymbol('==')      then Result := TValue.From<Boolean>(Result.AsVariant =  AddSub.AsVariant)
      else if CheckSymbol('>=')      then Result := TValue.From<Boolean>(Result.AsVariant >= AddSub.AsVariant)
      else if CheckSymbol('<=')      then Result := TValue.From<Boolean>(Result.AsVariant <= AddSub.AsVariant)
      else if CheckSymbol('<>')      then Result := TValue.From<Boolean>(Result.AsVariant <> AddSub.AsVariant)
      else if CheckSymbol('>', '>=') then Result := TValue.From<Boolean>(Result.AsVariant >  AddSub.AsVariant)
      else if CheckSymbol('<', '<=') then Result := TValue.From<Boolean>(Result.AsVariant <  AddSub.AsVariant)
      else if CheckSymbol('!=')      then Result := TValue.From<Boolean>(Result.AsVariant <> AddSub.AsVariant)
      else if CheckSymbol('&&')      then Result := TValue.From<Boolean>((Result.AsBoolean) and (AddSub.AsBoolean))
      else if CheckSymbol('||')      then Result := TValue.From<Boolean>((Result.AsBoolean) or  (AddSub.AsBoolean))
      else Break;

  end;
begin
  s := Expression;
  i := 1;

  Result := Compare;
end;
{$ENDREGION}

initialization
  RttiContext := TRttiContext.Create;

finalization
  RttiContext.Free;

end.
