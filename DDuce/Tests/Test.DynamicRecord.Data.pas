unit Test.DynamicRecord.Data;

interface

uses
  Spring,

  DDuce.DynamicRecord;

type
  TTestClass = class
  private
    FTestBoolean          : Boolean;
    FTestChar             : Char;
    FTestDateTime         : TDateTime;
    FTestDouble           : Double;
    FTestInteger          : Integer;
    FTestString           : string;
    FTestNullableBoolean  : Nullable<Boolean>;
    FTestNullableDateTime : Nullable<TDateTime>;
    FTestNullableDouble   : Nullable<Double>;
    FTestNullableInteger  : Nullable<Integer>;
    FTestNullableString   : Nullable<string>;
    FTestNullableChar     : Nullable<Char>;

  public
    property TestBoolean: Boolean
      read FTestBoolean write FTestBoolean;

    property TestChar: Char
      read FTestChar write FTestChar;

    property TestDateTime: TDateTime
      read FTestDateTime write FTestDateTime;

    property TestDouble: Double
      read FTestDouble write FTestDouble;

    property TestInteger: Integer
      read FTestInteger write FTestInteger;

    property TestNullableBoolean: Nullable<Boolean>
      read FTestNullableBoolean write FTestNullableBoolean;

    property TestNullableDateTime: Nullable<TDateTime>
      read FTestNullableDateTime write FTestNullableDateTime;

    property TestNullableDouble: Nullable<Double>
      read FTestNullableDouble write FTestNullableDouble;

    property TestNullableInteger: Nullable<Integer>
      read FTestNullableInteger write FTestNullableInteger;

    property TestNullableString: Nullable<string>
      read FTestNullableString write FTestNullableString;

    property TestString: string
      read FTestString write FTestString;

    property TestNullableChar: Nullable<Char>
      read FTestNullableChar write FTestNullableChar;
  end;

type
  TTestRecord = record
  private
    FTestBoolean          : Boolean;
    FTestChar             : Char;
    FTestDateTime         : TDateTime;
    FTestDouble           : Double;
    FTestInteger          : Integer;
    FTestString           : string;
//    FTestNullableBoolean  : Nullable<Boolean>;
//    FTestNullableDateTime : Nullable<TDateTime>;
//    FTestNullableDouble   : Nullable<Double>;
//    FTestNullableInteger  : Nullable<Integer>;
//    FTestNullableString   : Nullable<string>;
//    FTestNullableChar     : Nullable<Char>;

  public
    property TestBoolean: Boolean
      read FTestBoolean write FTestBoolean;

    property TestChar: Char
      read FTestChar write FTestChar;

    property TestDateTime: TDateTime
      read FTestDateTime write FTestDateTime;

    property TestDouble: Double
      read FTestDouble write FTestDouble;

    property TestInteger: Integer
      read FTestInteger write FTestInteger;

    property TestString: string
      read FTestString write FTestString;
//    property TestNullableBoolean: Nullable<Boolean>
//      read FTestNullableBoolean write FTestNullableBoolean;
//    property TestNullableDateTime: Nullable<TDateTime>
//      read FTestNullableDateTime write FTestNullableDateTime;
//    property TestNullableDouble: Nullable<Double>
//      read FTestNullableDouble write FTestNullableDouble;
//    property TestNullableInteger: Nullable<Integer>
//      read FTestNullableInteger write FTestNullableInteger;
//    property TestNullableString: Nullable<string>
//      read FTestNullableString write FTestNullableString;

//    property TestNullableChar: Nullable<Char>
//      read FTestNullableChar write FTestNullableChar;
  end;

implementation

end.
