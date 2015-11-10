unit NativeXmlCodepages;

interface

type

  // codepage information (name and codepage record)
  TCodepageInfo = packed record
    Name: Utf8String;
    Codepage: integer;
  end;

const

  // Codepages defined in Windows
  cCodepageInfoCount = 143;
  cCodePageInfo: array[0..cCodepageInfoCount - 1] of TCodepageInfo =
  ( (Name: 'IBM037';                  Codepage:    37), //1
    (Name: 'IBM437';                  Codepage:   437),
    (Name: 'IBM500';                  Codepage:   500),
    (Name: 'ASMO-708';                Codepage:   708),
    (Name: 'ASMO-449+';               Codepage:   709), //5
    (Name: 'BCON V4';                 Codepage:   709),
    (Name: 'Arabic';                  Codepage:   710),
    (Name: 'DOS-720';                 Codepage:   720),
    (Name: 'ibm737';                  Codepage:   737),
    (Name: 'ibm775';                  Codepage:   775), //10
    (Name: 'ibm850';                  Codepage:   850),
    (Name: 'ibm852';                  Codepage:   852),
    (Name: 'IBM855';                  Codepage:   855),
    (Name: 'ibm857';                  Codepage:   857),
    (Name: 'IBM00858';                Codepage:   858),
    (Name: 'IBM860';                  Codepage:   860),
    (Name: 'ibm861';                  Codepage:   861),
    (Name: 'DOS-862';                 Codepage:   862),
    (Name: 'IBM863';                  Codepage:   863),
    (Name: 'IBM864';                  Codepage:   864), //20
    (Name: 'IBM865';                  Codepage:   865),
    (Name: 'cp866';                   Codepage:   866),
    (Name: 'ibm869';                  Codepage:   869),
    (Name: 'IBM870';                  Codepage:   870),
    (Name: 'windows-874';             Codepage:   874),
    (Name: 'cp875';                   Codepage:   875),
    (Name: 'shift_jis';               Codepage:   932),
    (Name: 'gb2312';                  Codepage:   936),
    (Name: 'ks_c_5601-1987';          Codepage:   949),
    (Name: 'big5';                    Codepage:   950), //30
    (Name: 'IBM1026';                 Codepage:  1026),
    (Name: 'IBM01047';                Codepage:  1047),
    (Name: 'IBM01140';                Codepage:  1140),
    (Name: 'IBM01141';                Codepage:  1141),
    (Name: 'IBM01142';                Codepage:  1142),
    (Name: 'IBM01143';                Codepage:  1143),
    (Name: 'IBM01144';                Codepage:  1144),
    (Name: 'IBM01145';                Codepage:  1145),
    (Name: 'IBM01146';                Codepage:  1146),
    (Name: 'IBM01147';                Codepage:  1147), //40
    (Name: 'IBM01148';                Codepage:  1148),
    (Name: 'IBM01149';                Codepage:  1149),
    (Name: 'utf-16';                  Codepage:  1200),
    (Name: 'unicodeFFFE';             Codepage:  1201),
    (Name: 'windows-1250';            Codepage:  1250),
    (Name: 'windows-1251';            Codepage:  1251),
    (Name: 'windows-1252';            Codepage:  1252),
    (Name: 'windows-1253';            Codepage:  1253),
    (Name: 'windows-1254';            Codepage:  1254),
    (Name: 'windows-1255';            Codepage:  1255), //50
    (Name: 'windows-1256';            Codepage:  1256),
    (Name: 'windows-1257';            Codepage:  1257),
    (Name: 'windows-1258';            Codepage:  1258),
    (Name: 'Johab';                   Codepage:  1361),
    (Name: 'macintosh';               Codepage: 10000),
    (Name: 'x-mac-japanese';          Codepage: 10001),
    (Name: 'x-mac-chinesetrad';       Codepage: 10002),
    (Name: 'x-mac-korean';            Codepage: 10003),
    (Name: 'x-mac-arabic';            Codepage: 10004),
    (Name: 'x-mac-hebrew';            Codepage: 10005), //60
    (Name: 'x-mac-greek';             Codepage: 10006),
    (Name: 'x-mac-cyrillic';          Codepage: 10007),
    (Name: 'x-mac-chinesesimp';       Codepage: 10008),
    (Name: 'x-mac-romanian';          Codepage: 10010),
    (Name: 'x-mac-ukrainian';         Codepage: 10017),
    (Name: 'x-mac-thai';              Codepage: 10021),
    (Name: 'x-mac-ce';                Codepage: 10029),
    (Name: 'x-mac-icelandic';         Codepage: 10079),
    (Name: 'x-mac-turkish';           Codepage: 10081),
    (Name: 'x-mac-croatian';          Codepage: 10082), //70
    (Name: 'utf-32';                  Codepage: 12000),
    (Name: 'utf-32BE';                Codepage: 12001),
    (Name: 'x-Chinese_CNS';           Codepage: 20000),
    (Name: 'x-cp20001';               Codepage: 20001),
    (Name: 'x_Chinese-Eten';          Codepage: 20002),
    (Name: 'x-cp20003';               Codepage: 20003),
    (Name: 'x-cp20004';               Codepage: 20004),
    (Name: 'x-cp20005';               Codepage: 20005),
    (Name: 'x-IA5';                   Codepage: 20105),
    (Name: 'x-IA5-German';            Codepage: 20106), //80
    (Name: 'x-IA5-Swedish';           Codepage: 20107),
    (Name: 'x-IA5-Norwegian';         Codepage: 20108),
    (Name: 'us-ascii';                Codepage: 20127),
    (Name: 'x-cp20261';               Codepage: 20261),
    (Name: 'x-cp20269';               Codepage: 20269),
    (Name: 'IBM273';                  Codepage: 20273),
    (Name: 'IBM277';                  Codepage: 20277),
    (Name: 'IBM278';                  Codepage: 20278),
    (Name: 'IBM280';                  Codepage: 20280),
    (Name: 'IBM284';                  Codepage: 20284), //90
    (Name: 'IBM285';                  Codepage: 20285),
    (Name: 'IBM290';                  Codepage: 20290),
    (Name: 'IBM297';                  Codepage: 20297),
    (Name: 'IBM420';                  Codepage: 20420),
    (Name: 'IBM423';                  Codepage: 20423),
    (Name: 'IBM424';                  Codepage: 20424),
    (Name: 'x-EBCDIC-KoreanExtended'; Codepage: 20833),
    (Name: 'IBM-Thai';                Codepage: 20838),
    (Name: 'koi8-r';                  Codepage: 20866),
    (Name: 'IBM871';                  Codepage: 20871), //100
    (Name: 'IBM880';                  Codepage: 20880),
    (Name: 'IBM905';                  Codepage: 20905),
    (Name: 'IBM00924';                Codepage: 20924),
    (Name: 'EUC-JP';                  Codepage: 20932),
    (Name: 'x-cp20936';               Codepage: 20936),
    (Name: 'x-cp20949';               Codepage: 20949),
    (Name: 'cp1025';                  Codepage: 21025),
    (Name: 'koi8-u';                  Codepage: 21866),
    (Name: 'iso-8859-1';              Codepage: 28591),
    (Name: 'iso-8859-2';              Codepage: 28592), //110
    (Name: 'iso-8859-3';              Codepage: 28593),
    (Name: 'iso-8859-4';              Codepage: 28594),
    (Name: 'iso-8859-5';              Codepage: 28595),
    (Name: 'iso-8859-6';              Codepage: 28596),
    (Name: 'iso-8859-7';              Codepage: 28597),
    (Name: 'iso-8859-8';              Codepage: 28598),
    (Name: 'iso-8859-9';              Codepage: 28599),
    (Name: 'iso-8859-13';             Codepage: 28603),
    (Name: 'iso-8859-15';             Codepage: 28605),
    (Name: 'x-Europa';                Codepage: 29001), //120
    (Name: 'iso-8859-8-i';            Codepage: 38598),
    (Name: 'iso-2022-jp';             Codepage: 50220),
    (Name: 'csISO2022JP';             Codepage: 50221),
    (Name: 'iso-2022-jp';             Codepage: 50222),
    (Name: 'iso-2022-kr';             Codepage: 50225),
    (Name: 'x-cp50227';               Codepage: 50227),
    (Name: 'euc-jp';                  Codepage: 51932),
    (Name: 'EUC-CN';                  Codepage: 51936),
    (Name: 'euc-kr';                  Codepage: 51949),
    (Name: 'hz-gb-2312';              Codepage: 52936), //130
    (Name: 'GB18030';                 Codepage: 54936),
    (Name: 'x-iscii-de';              Codepage: 57002),
    (Name: 'x-iscii-be';              Codepage: 57003),
    (Name: 'x-iscii-ta';              Codepage: 57004),
    (Name: 'x-iscii-te';              Codepage: 57005),
    (Name: 'x-iscii-as';              Codepage: 57006),
    (Name: 'x-iscii-or';              Codepage: 57007),
    (Name: 'x-iscii-ka';              Codepage: 57008),
    (Name: 'x-iscii-ma';              Codepage: 57009),
    (Name: 'x-iscii-gu';              Codepage: 57010), //140
    (Name: 'x-iscii-pa';              Codepage: 57011),
    (Name: 'utf-7';                   Codepage: 65000),
    (Name: 'utf-8';                   Codepage: 65001));//143

implementation

end.





