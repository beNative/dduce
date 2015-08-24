{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit Demo.Contact;

{$I ..\Source\DDuce.inc}

{ Sample model object that is typically a database entity object that can be
  persisted. }

interface

uses
{$IFDEF DSHARP}
  DSharp.Core.PropertyChangedBase, // TPropertyChangedBase
  DSharp.Bindings.Notifications,   // INotifyPropertyChanged
  DSharp.Collections.ObservableCollection
{$ELSE}
  {$IFDEF SPRING}
  Spring, Spring.Collections, Spring.Collections.Lists
  {$ENDIF}
{$ENDIF};

type
  TPhone = class
  private
    FPhone: string;
    FKind : string;

  public
    property Phone: string
      read FPhone write FPhone;

    property Kind: string
      read FKind write FKind;
  end;

type
  TContact = class{$IFDEF DSHARP}(TPropertyChangedBase){$ENDIF}
  private
    FLastName    : string;
    FFirstName   : string;
    FCompanyName : string;
    FEmail       : string;
    FAddress     : string;
    //FCountry     : TField<string>;
    FCountry     : string;
    FNumber      : Integer;
    FBirthDate   : TDate;
    //FPhones      : IList<TPhone>;
    FActive      : Boolean;

    // property access methods
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetCompanyName(const Value: string);
    procedure SetEmail(const Value: string);
    procedure SetAddress(const Value: string);
    procedure SetNumber(const Value: Integer);
    procedure SetBirthDate(const Value: TDate);
    procedure SetActive(const Value: Boolean);
    procedure SetCountry(const Value: string);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  published
    property FirstName: string
      read FFirstName write SetFirstName;

    property LastName: string
      read FLastName write SetLastName;

    property Email: string
      read FEmail write SetEmail;

    property CompanyName: string
      read FCompanyName write SetCompanyName;

    property Address: string
      read FAddress write SetAddress;

    property Number: Integer
      read FNumber write SetNumber;

    property BirthDate: TDate
      read FBirthDate write SetBirthDate;

    property Active: Boolean
      read FActive write SetActive;

    property Country: string
      read FCountry write SetCountry;

//    property Phones: IList<TPhone>
//      read FPhones;
  end;

{$IFDEF DSHARP}
  TContacts = TObservableCollection<TContact>;
{$ELSE}
  {$IFDEF SPRING}
  TContacts = TObjectList<TContact>;
  {$ENDIF}
{$ENDIF}

implementation

uses
  System.Classes,
  Vcl.Dialogs;

{$REGION 'construction and destruction'}
procedure TContact.AfterConstruction;
begin
  inherited;
  //FCountry.Initialize(Self);
  //FPhones := TObservableCollection<TPhone>.Create;
end;

procedure TContact.BeforeDestruction;
begin
  //FCountry.Finalize;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TContact.SetActive(const Value: Boolean);
begin
  FActive := Value;
//  DoPropertyChanged('Active');
end;

procedure TContact.SetAddress(const Value: string);
begin
  FAddress := Value;
//  DoPropertyChanged('Address');
end;

procedure TContact.SetBirthDate(const Value: TDate);
begin
  FBirthDate := Value;
//  DoPropertyChanged('BirthDate');
end;

procedure TContact.SetCompanyName(const Value: string);
begin
  FCompanyName := Value;
//  DoPropertyChanged('CompanyName');
end;

procedure TContact.SetCountry(const Value: string);
begin
  FCountry := Value;
//  DoPropertyChanged('Country');
end;

procedure TContact.SetEmail(const Value: string);
begin
  FEmail := Value;
//  DoPropertyChanged('Email');
end;

procedure TContact.SetFirstName(const Value: string);
begin
  FFirstName := Value;
//  DoPropertyChanged('Firstname');
end;

procedure TContact.SetLastName(const Value: string);
begin
  FLastName := Value;
//  DoPropertyChanged('Lastname');
end;

procedure TContact.SetNumber(const Value: Integer);
begin
  FNumber := Value;
//  DoPropertyChanged('Number');
end;
{$ENDREGION}

end.
