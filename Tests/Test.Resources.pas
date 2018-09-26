{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I Test.DDuce.inc}

unit Test.Resources;

interface

const
  TEXT_LOREM_IPSUM =
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod ' +
    'tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim '    +
    'veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea ' +
    'commodo consequat. Duis aute irure dolor in reprehenderit in voluptate '  +
    'velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint '       +
    'occaecat cupidatat non proident, sunt in culpa qui officia deserunt '     +
    'mollit anim id est laborum.';

  TEXT_SQL =
    'select'                                  + #13#10 +
    '  *'                                     + #13#10 +
    'from'                                    + #13#10 +
    '  InvoiceLine il'                        + #13#10 +
    '  inner join Invoice i'                  + #13#10 +
    '    on (i.InvoiceId = il.InvoiceId)'     + #13#10 +
    '  inner join Customer c'                 + #13#10 +
    '    on (c.CustomerId = i.CustomerId)'    + #13#10 +
    '  inner join Employee e'                 + #13#10 +
    '    on (e.EmployeeId = c.SupportRepId)'  + #13#10 +
    '  inner join Track t'                    + #13#10 +
    '    on (il.TrackId = t.TrackId)'         + #13#10 +
    '  inner join Album al'                   + #13#10 +
    '    on (al.AlbumId = t.AlbumId)'         + #13#10 +
    '  inner join MediaType mt'               + #13#10 +
    '    on (mt.MediaTypeId = t.MediaTypeId)' + #13#10 +
    '  inner join Genre g'                    + #13#10 +
    '    on (g.GenreId = t.GenreId)';

  TEXT_XML =
    '<?xml version="1.0"?>'                                               + #13#10 +
    '<template format="5" revision="3" name="Assets Folder">'             + #13#10 +
    '  <category value="Folder"/>'                                        + #13#10 +
    '  <parameter id="remapFolder" name="Change Folder" type="boolean"/>' + #13#10 +
    '  <parameter id="newLocation" name="New Folder" type="string" >'     + #13#10 +
    '  <execute file="recipe.xml.ftl"/>'                                  + #13#10 +
    '</template>';

  TEXT_JSON =
    '{'                                     + #13#10 +
    '  "firstName": "John",'                + #13#10 +
    '  "lastName": "Smith",'                + #13#10 +
    '  "age": 25,'                          + #13#10 +
    '  "address":'                          + #13#10 +
    '  {'                                   + #13#10 +
    '    "streetAddress": "21 2nd Street",' + #13#10 +
    '    "city": "New York",'               + #13#10 +
    '    "state": "NY",'                    + #13#10 +
    '    "postalCode": "10021"'             + #13#10 +
    '  },'                                  + #13#10 +
    '  "phoneNumber":'                      + #13#10 +
    '  ['                                   + #13#10 +
    '    {'                                 + #13#10 +
    '      "type": "home",'                 + #13#10 +
    '      "number": "212 555-1234"'        + #13#10 +
    '    },'                                + #13#10 +
    '    {'                                 + #13#10 +
    '      "type": "fax",'                  + #13#10 +
    '      "number": "646 555-4567"'        + #13#10 +
    '    }'                                 + #13#10 +
    '  ]'                                   + #13#10 +
    '}';

  TEXT_INI =
    '; Comment' + #13#10 +
    '[Section]' + #13#10 +
    'Name=Value';

implementation

end.
