{
  Copyright (C) 2013 Tim Sinaeve tim.sinaeve@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit DDuce.ScopedReference;

{
  Implementation of a scoped reference type (smart pointer).

  A scoped reference performs automatic freeing for enclosed objects. Once an
  object is stored in a scoped reference, it is guaranteed to be released when
  the reference goes out of scope.

  Original concept based on DeHL library by Ciobanu Alexandru. You may obtain a
  copy of this library at http://code.google.com/p/delphilhlplib/

  This simplified version only supports a fully managed instance and does not
  allow for shared or weak references to the enclosed object.

  In addition to the original implementation it features automatic instantiation
  of the enclosed object, so the managed object does not need to be created nor
  destroyed as both operations will be handled by the record instance.

  Optionally a factory function for the reference can be specified. If not
  specified the instance will be created using the default constructor of the
  object type.

  In the example 3 different objects are created to show their classname.

  The code below illustrates the traditional object creation and the equivalent
  version using scoped references.

  1. Standard way (using classic boilerplate code)
  ------------------------------------------------
      procedure ShowClassNames;
      var
        O: TObject;
        P: TPersistent;
        L: TList;
      begin
        O := TObject.Create;
        try
          P := TPersistent.Create;
          try
            L := TList.Create;
            try
              ShowMessage(O.ClassName);
              ShowMessage(P.ClassName);
              ShowMessage(L.ClassName);
            finally
              L.Free;
            end;
          finally
            P.Free;
          end;
        finally
          O.Free;
        end;
      end;

  2. Using scoped references (automatic bookkeeping of the enclosed objects)
  --------------------------------------------------------------------------
      procedure ShowClassNames;
      var
        O: Scoped<TObject>;
        P: Scoped<TPersistent>;
        L: Scoped<TList>;
      begin
        ShowMessage(O.Ref.ClassName);
        ShowMessage(P.Ref.ClassName);
        ShowMessage(L.Ref.ClassName);
      end;
}

{$I DDuce.inc}

//*****************************************************************************

interface

uses
  SysUtils;

type
  Scoped<T: class, constructor> = record
  private type
    IGuard = interface
      function GetInstance: T;

      property Instance: T
        read GetInstance;
    end;

    TGuard = class(TInterfacedObject, IGuard)
    private
      FInstance: T;
      function GetInstance: T;

    public
      constructor Create(const AInstance: T);
      destructor Destroy; override;

      property Instance: T
        read GetInstance;
    end;

  private
    { FGuard is an interface variable that will be <> nil when an instance is
      created (Unlike normal object reference variables interface variables in
      records are always initialized to nil.) }
    FGuard: IGuard;

    function GetRef: T; inline;

  public
    constructor Create(const ARefFactory: TFunc<T>);
    class operator Implicit(const AScoped: Scoped<T>): T; inline; static;
    procedure Reset;

    property Ref: T
      read GetRef;
  end;

//*****************************************************************************

implementation

{ TGuard<T> }

constructor Scoped<T>.TGuard.Create(const AInstance: T);
begin
  inherited Create;
  FInstance := AInstance;
end;

destructor Scoped<T>.TGuard.Destroy;
begin
  FInstance.Free;
  inherited;
end;

function Scoped<T>.TGuard.GetInstance: T;
begin
  Result := FInstance;
end;

{ Scoped<T> }

constructor Scoped<T>.Create(const ARefFactory: TFunc<T>);
begin
  Reset;
  FGuard := TGuard.Create(ARefFactory());
end;

function Scoped<T>.GetRef: T;
begin
  if FGuard = nil then
    FGuard := TGuard.Create(T.Create);
  Result := FGuard.Instance
end;

class operator Scoped<T>.Implicit(const AScoped: Scoped<T>): T;
begin
  Result := AScoped.GetRef;
end;

procedure Scoped<T>.Reset;
begin
  FGuard := nil;
end;

end.
