unit NativeXml;

interface

{$i simdesign.inc}
uses
  Graphics,
  Classes,
  Contnrs,
  SysUtils,
  sdStreams,
  sdStringTable,
  sdDebug,
  NativeXmlCodepages,
{$IFDEF MSWINDOWS}
  Windows, WinInet;
{$ELSE}
  NativeXmlUtilsForLinux;
{$ENDIF}

const

  // Current version of the NativeXml unit
  cNativeXmlVersion = 'v3.32';
  cNativeXmlDate    = '18feb2015';

type
  // An event that is used to indicate load or save progress.
  TXmlProgressEvent = procedure(Sender: TObject; Position: int64) of object;

  // TsdElementType enumerates the different kinds of elements that can be found
  // in the XML document.
  TsdElementType = (
    xeElement,     // Normal element <name {attr}>[value][sub-elements]</name>
    xeAttribute,   // Attribute ( name='value' or name="value")
    xeComment,     // Comment <!--{comment}-->
    xeCData,       // literal data <![CDATA[{data}]]>
    xeCondSection, // conditional section <![ IGNORE / INCLUDE [ markup ]]>
    xeDeclaration, // XML declaration <?xml{declaration}?>
    xeStylesheet,  // Stylesheet <?xml-stylesheet{stylesheet}?>
    xeDocType,     // DOCTYPE DTD declaration <!DOCTYPE{spec}>
    xeDtdElement,  // <!ELEMENT >
    xeDtdAttList,  // <!ATTLIST >
    xeDtdEntity,   // <!ENTITY >
    xeDtdNotation, // <!NOTATION >
    xeInstruction, // <?...?> processing instruction
    xeCharData,    // Character data in a node
    xeWhiteSpace,  // chardata with only whitespace
    xeQuotedText,  // "bla" or 'bla'
    xeUnknown,     // Any <data>
    xeEndTag,      // </...>
    xeError        // some error
  );

  TsdElementTypes = set of TsdElementType;

  // Definition of different methods of string encoding.
  TsdStringEncoding = (
    seAnsi,      // Ansi encoding, e.g. "Windows-1252" or other codepage (1 byte per character)
    seUTF8,      // UTF-8 (1, 2, 3 or 4 bytes per character)
    seUTF16BE,   // UTF-16 Big Endian (2 or 4 bytes per character)
    seUTF16LE,   // UTF-16 Little Endian (2 or 4  bytes per character)
    seUTF32BE,   // ucs-4 Big Endian (4 bytes per character)
    seUTF32LE,   // ucs-4 Little Endian (4 bytes per character)
    seUCS4_2143, // UCS-4 unusual octet order - 2143 (4 bytes per character)
    seUCS4_3412, // UCS-4 unusual octet order - 3412 (4 bytes per character)
    seEBCDIC     // Extended Binary Coded Decimal Interchange Code (1 byte per character)
  );

  // Choose what kind of binary encoding will be used when calling
  // TXmlNode BufferRead and BufferWrite.
  TsdBinaryEncoding = (
    xbeBase64,  { With this encoding, each group of 3 bytes are stored as 4
                  characters, requiring 64 different characters. - DEFAULT}
    xbeBinHex   { With this encoding, each byte is stored as a hexadecimal
                  number, e.g. 0 = 00 and 255 = FF.                        }
  );

  // Node closing style:
  //  ncDefault defaults to what is parsed per element
  //  ncFull  looks like <node a="bla"></node> and
  //  ncClose looks like <node a="bla"/>
  //  ncUnknown defaults to what is parsed per element
  TsdNodeClosingStyle = (
    ncDefault,
    ncFull,
    ncClose
  );

  // End-Of-Line style
  TsdEolStyle = (
    esLinux,    // write End-Of-Line as just LF (#$0A)
    esWindows   // write End-Of-Line as CR + LF (#$0D + #$0A)
  );

  // Note on TNativeXml.XmlFormat:
  // - xfCompact (default) to save the xml fully compliant and at smallest size
  // - xfReadable writes additional nonsignificant whitespace so the client can
  //     easily read the xml file with a standard editor.
  // - xfPreserve aims to preserve whitespace data just as it is parsed
  TsdXmlFormatType = (
    xfCompact,  // Save without any control chars except LF after declaration
    xfReadable, // Save in readable format with indents and end-of-lines
    xfPreserve  // Preserve whitespace whenever possible
  );

  // record with info from a Byte order Mark (BOM)
  TsdBomInfo = packed record
    BOM: array[0..3] of byte;    // 4 bytes possibly containing the BOM
    Len: integer;                // byte length of the BOM
    Encoding: TsdStringEncoding; // which string encoding does the file have?
    HasBOM: boolean;             // does a file have a BOM?
  end;

  TXmlCompareOption = (
    xcNodeName,
    xcNodeType,
    xcNodeValue,
    xcAttribCount,
    xcAttribNames,
    xcAttribValues,
    xcChildCount,
    xcChildNames,
    xcChildValues,
    xcRecursive
  );

  TXmlCompareOptions = set of TXmlCompareOption;

  // codepage information (name and codepage record)
  TCodepageInfo = packed record
    Name: Utf8String;
    Codepage: integer;
  end;

  // default charset names for TsdStringEncoding
const

  cStringEncodingCharsetNames: array[TsdStringEncoding] of Utf8String =
    ('ansi',
     'utf-8',
     'unicodeFFFE',
     'utf-16',
     'utf-32BE',
     'utf-32',
     'ucs4_2143',
     'ucs4_3412',
     'ebcdic');

  // default codecs for TsdStringEncoding if no codepage is given
  cStringEncodingCodePages: array[TsdStringEncoding] of integer =
    (    0 {ansi can be any codepage},
     65001 {utf-8},
      1201 {unicodeFFFE},
      1200 {utf-16},
     12001 {utf-32BE},
     12000 {utf-32},
         0 {no codepage for UCS4_2143},
         0 {no codepage for UCS4_3412},
         0 {ebcdic can be any codepage});

  // all xml compare options
  xcAll: TXmlCompareOptions = [xcNodeName, xcNodeType, xcNodeValue, xcAttribCount,
    xcAttribNames, xcAttribValues, xcChildCount, xcChildNames, xcChildValues,
    xcRecursive];

  // "signature" that defines the binary XML file/stream
  cBinaryXmlCookie: array[0..3] of AnsiChar = '$BXM';

// Delphi unicode compatibility
{$ifndef UNICODE}
type
  UnicodeString = WideString;
  RawByteString = AnsiString;
{$endif UNICODE}

type
  // XML buffered parser. It buffers the source stream into
  // a memory buffer of limited size and reads from the stream chunk-wise.
  // This way, it can do string comparisons in memory, directly on the buffer.
  TsdXmlParser = class(TDebugPersistent)
  protected
    FBomInfo: TsdBomInfo;
    FSource: TStream;
    FChunkSize: integer;
    FRawBuffer: array of byte;      // raw data buffer
    FUtf8Buffer: array of AnsiChar; // utf8 data buffer
    FEncoding: TsdStringEncoding;
    FCodePage: integer;
    FRawFirstIdx: integer;
    FRawLastIdx: integer;
    FUtf8FirstIdx: integer;
    FUtf8CurrentIdx: integer;
    FUtf8LastIdx: integer;
    FUtf8BasePosition: int64;
    FBaseLineNumber: int64;
    FEndOfStream: boolean;
    FNormaliseEOLEnabled: boolean;
    FOnDebugOut: TsdDebugEvent;
    function LoCase(Ch: AnsiChar): AnsiChar;
    procedure IncCurrentIdxCheck(var BytesAvail: integer);
    function ReadString(AIndex, ACount: integer): Utf8String;
    function ReadNextChunk: integer;
    procedure EncodeChunk;
    function GetPosition: int64;
    function GetLineNumber: int64;
    procedure SetCodePage(const Value: integer);
  public
    constructor Create(ASource: TStream; AChunkSize: integer); virtual;
    destructor Destroy; override;
    property OnDebugOut: TsdDebugEvent read FOnDebugOut write FOnDebugOut;
    property Owner: TDebugComponent read FOwner write FOwner;
    // Call flush once in a while, to check if data can be flushed out. Flushing
    // means that the part before the current pointer is removed and the bytes
    // following are moved to 0 position. It is only actually done when enough
    // chunks are read, and the flushing happens chunk-wise.
    procedure Flush(Force: boolean = False);
    // Is the stream from binary xml?
    function IsBinaryXml: boolean;
    // Make at least one byte available from current position
    function MakeDataAvailable: integer;
    // Get the next character from the stream
    function NextChar: AnsiChar;
    // collapse all EOL to #$0A
    procedure NormaliseEOL;
    // Check if the stream at this position contains string S. If so, the stream
    // will be positioned after, if not, it will remain where it is.
    function CheckString(const S: Utf8String): boolean;
    // Move one position back in the stream
    procedure MoveBack;
    // Read a string from the stream until Terminator is found. The string returned
    // will be the part before Terminator, the stream is positioned after Terminator
    function ReadStringUntil(const Terminator: Utf8String): Utf8String;
    // Read a quoted string from the stream, return the unquoted string
    function ReadQuotedString(AQuote: AnsiChar): Utf8String;
    // Read a string from the stream until character AChar is encountered.
    // var EOS will be True if the stream reached the end.
    function ReadStringUntilChar(AChar: AnsiChar): Utf8String;
    // The encoding detected in the source stream (valid after ReadBOM or after
    // the declaration).
    property Encoding: TsdStringEncoding read FEncoding write FEncoding;
    // CodePage used in text processing
    property CodePage: integer read FCodePage write SetCodePage;
    // Position in the stream in bytes from the start.
    property Position: int64 read GetPosition;
    // Line number in the stream. Lines are detected by analysing the stream
    // for occurances of #13 (CR). The line number is *calculated* when this
    // property is read, so it should not be read very regularly.
    property LineNumber: int64 read GetLineNumber;
    // Is the end of the stream detected?
    property EndOfStream: boolean read FEndOfStream;

    // Special parser procedures to parse XML content.

    // Read the next character, skip any blanks inbetween. Blanks are:
    // #$09, #$0A, #$0D, #$20
    function NextCharSkipBlanks(var Blanks: Utf8String): AnsiChar;
    // Read BOM (Byte Order Mark) from the start of the file in order to detect which
    // encoding is used.
    procedure ReadBOM;
    // Read an new tag from the stream (from the position afer "<")
    function ReadOpenTag: TsdElementType;
    // Read a string from the stream until a blank char, or a "/" or a ">" is
    // encountered.
    function ReadStringUntilBlankOrEndTag: Utf8String;
    // Info from Byte Order Mark (BOM)
    property BomInfo: TsdBomInfo read FBomInfo;
  end;

  // specialized buffered writer that obeys encoding and codepage
  TsdXmlWriter = class(TsdBufferWriter)
  private
    FOwner: TDebugComponent;
    FRawBuffer: array of byte;
    FRawBufferSize: integer;
    procedure DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
  public
    FEncoding: TsdStringEncoding;
    FCodePage: integer;
    // overridden Write for all supported encodings (ansi, utf8, utf16le, utf16be)
    function Write(const Buffer; Count: Longint): Longint; override;
    constructor Create(AOwner: TDebugComponent; ASource: TStream; AChunkSize: integer);
    destructor Destroy; override;
  end;

  // Forward declaration TsdAttribute (needed by TXmlNode)
  TsdAttribute = class;

  // TXmlNode metaclass
  TsdNodeClass = class of TXmlNode;

  // Forward declaration of TNativeXml
  TNativeXml = class;

  // Forward declaration of TXmlNode
  TXmlNode = class;

  // Pass a function of this kind to TXmlNode.SortChildNodes. The function should
  // return -1 if Node1 < Node2, 0 if Node1 = Node2 and 1 if Node1 > Node2.
  TXmlNodeCompareFunction = function(Node1, Node2: TXmlNode): integer;

  // TXmlNode is the ancestor for all nodes in the xml document. See TsdElement
  // for the elements, TsdAttribute for the attributes.
  TXmlNode = class(TDebugPersistent)
  private
    // inherited from TDebugPersistent: FOwner: TDebugComponent
    FParent: TXmlNode;
    function GetAttributeByName(const AName: Utf8String): TsdAttribute;
    function GetAttributeValueByName(const AName: Utf8String): Utf8String;
    function GetAttributeValueByNameWide(const AName: Utf8String): UnicodeString;
    procedure SetAttributeValueByName(const AName, Value: Utf8String);
    procedure SetAttributeValueByNameWide(const AName: Utf8String; const Value: UnicodeString);
    function GetBinaryString: RawByteString;
    procedure SetBinaryString(const Value: RawByteString);
    function GetValueUnicode: UnicodeString;
    procedure SetValueUnicode(const Value: UnicodeString);
    function GetAttributes(Index: integer): TsdAttribute;
    function GetAttributeName(Index: integer): Utf8String;
    function GetAttributeValue(Index: integer): Utf8String;
    procedure SetAttributeName(Index: integer; const Value: Utf8String);
    procedure SetAttributeValue(Index: integer; const Value: Utf8String);
    function GetAttributeValueAsInteger(Index: integer): integer;
    procedure SetAttributeValueAsInteger(Index: integer; const Value: integer);
    function GetWriteOnDefault: boolean;
    procedure SetWriteOnDefault(const Value: boolean);
    function GetName: Utf8String; virtual;
    function GetNameUnicode: UnicodeString; virtual;
    function GetValue: Utf8String; virtual;
    procedure SetName(const Value: Utf8String); virtual;
    procedure SetNameUnicode(const Value: UnicodeString); virtual;
    procedure SetValue(const Value: Utf8String); virtual;
    procedure DoProgress(Position: int64);
    function GetParentNode(ADepth: integer): TXmlNode;
    function GetEolStyle: TsdEolStyle;
    function GetPreserveWhiteSpace: boolean;
    function GetSkipNormalisation: boolean;
    function GetXmlFormat: TsdXmlFormatType;
    procedure DoNodeNew(ANode: TXmlNode);
    procedure DoNodeLoaded(ANode: TXmlNode);
    function GetContent: Utf8String; virtual;
    function GetDirectNodeCount: integer; virtual;
    function GetContainerCount: integer; virtual;
    function GetContainers(Index: integer): TXmlNode; virtual;
    function GetDocument: TNativeXml;
  protected
    FTag: pointer;
    FSourcePos: int64;
    // string table lookup methods
    function GetString(AID: integer): Utf8String;
    function AddString(const S: Utf8String): integer;
    function GetNodeCount: integer; virtual;
    function GetAttributeCount: integer; virtual;
    function GetNodes(Index: integer): TXmlNode; virtual;
    class function EscapeString(const S: Utf8String): Utf8String;
    class function ReplaceString(const S: Utf8String): Utf8String;
    function GetIndent: Utf8String; virtual;
    function GetEndOfLine: Utf8String; virtual;
    function GetSeparator: Utf8String; virtual;
    function NodeIndexByName(const AName: Utf8String): integer; virtual;
    procedure WriteValue(const AName, AValue: Utf8String); virtual;
    procedure WriteContent(S: TStream); virtual;
    // copy the data and subnodes from ANode; this node is cleared first
    procedure CopyFrom(ANode: TXmlNode); virtual;
    function CompareNodeName(const NodeName: Utf8String): integer;
    function GetFullPath: Utf8String;
    property WriteOnDefault: boolean read GetWriteOnDefault write SetWriteOnDefault;
    function GetParentNodeName(ADepth: integer): Utf8String;
  public
    // for compat: assign to source XmlNode
    procedure Assign(Source: TPersistent); override;
    // Create a new node object. AOwner must be the TNativeXml that is
    // going to hold this new node. Make sure to use the correct class when
    // creating, e.g. TsdElement.Create(Owner) for an element.
    constructor Create(AOwner: TNativeXml); virtual;
    // Create a new TXmlNode with name AName. AOwner must be the TNativeXml
    // that is going to hold this new node.
    constructor CreateName(AOwner: TNativeXml; const AName: Utf8String); virtual;
    // Create a new TXmlNode with name AName and UTF8String value AValue. AOwner
    // must be the TNativeXml that is going to hold this new node.
    constructor CreateNameValue(AOwner: TNativeXml; const AName, AValue: Utf8String); virtual;
    // Convert the Utf8String S to a UnicodeString
    class function Utf8ToWide(const S: Utf8String): UnicodeString;
    // Convert the UnicodeString W to an Utf8String
    class function WideToUtf8(const W: UnicodeString): Utf8String;
    // parse this node with parser P, result is the endnode and should be identical
    function ParseStream(Parser: TsdXmlParser): TXmlNode; virtual;
    // write this node to stream S
    procedure WriteStream(S: TStream); virtual;
    // The element type
    function ElementType: TsdElementType; virtual;
    // name of the element type
    function ElementTypeName: Utf8String; virtual;
    // write the node to a Utf8String
    function WriteToString: Utf8String;
    // Pointer to the owner document NativeXml
    property Document: TNativeXml read GetDocument;
    // Tag is a pointer value the developer can use in any way. Tag does not get
    // saved to the XML. Tag is often used to point to a GUI element.
    property Tag: pointer read FTag write FTag;
    // SourcePos (int64) points to the position in the source file where the
    // nodes text begins.
    property SourcePos: int64 read FSourcePos write FSourcePos;
    // Parent points to the parent node of the current XML node.
    property Parent: TXmlNode read FParent;
    // This function returns True if the node has no subnodes and no attributes,
    // and if the node Name and value are empty.
    function IsClear: boolean;
    // clear the node
    procedure Clear; virtual;
    // recursively delete empty nodes
    procedure DeleteEmptyNodes; virtual;
    // Call Delete to delete this node completely from the parent node list. This
    // call only succeeds if the node has a parent. It has no effect when called for
    // the root node.
    procedure Delete; virtual;
    // This function returns True if the node has no subnodes and no attributes,
    // and if the node value is empty.
    function IsEmpty: boolean;
    // Test whether ANode is equal to another node based on compare options. If
    // MismatchNodes is provided, a list of mismatching subnodes is filled.
    function IsEqualTo(ANode: TXmlNode; Options: TXmlCompareOptions;
      MismatchNodes: TList = nil): boolean;
    // Use this method to add an attribute with name AName and string value AValue
    // to the node. AName and AValue must be UTF8 encoded.
    procedure AttributeAdd(const AName, AValue: Utf8String); overload;
    // Use this method to add the attribute AAttribute. AAttribute must be owned by
    // the xml document beforehand.
    procedure AttributeAdd(AAttribute: TsdAttribute); overload;
    // Add an open array of TsdAttribute objects. Attributes must be owned by
    // the xml document beforehand.
    procedure AttributesAdd(Attributes: array of TsdAttribute);
    // Clear all attributes from the current node.
    procedure AttributesClear; virtual;
    // Use this method to delete the attribute at Index in the list. Index must be
    // equal or greater than 0, and smaller than AttributeCount. Using an index
    // outside of that range has no effect.
    procedure AttributeDelete(Index: integer);
    // Use this method to find the index of an attribute with name AName.
    function AttributeIndexByName(const AName: Utf8String): integer; virtual;
    // Add the node ANode to the nodelist. It will be added at the end, unless
    // it is an attribute, in that case it will be added at the end of the current
    // list of attributes. NodeAdd will set the parent of ANode to itself.
    function NodeAdd(ANode: TXmlNode): integer; virtual;
    // This function returns a pointer to the first subnode that has an attribute with
    // name AttribName and value AttribValue. If ShouldRecurse = True (default), the
    // function works recursively, using the depthfirst method.
    function NodeByAttributeValue(const NodeName, AttribName, AttribValue: Utf8String;
      ShouldRecurse: boolean = True): TXmlNode; overload;
    function NodeByAttributeValue(const NodeName, AttribName: Utf8String;
      const AttribValue: UnicodeString; ShouldRecurse: boolean = True): TXmlNode; overload;
    // Return a reference to the first subnode in the nodelist that has name AName.
    // If no subnodes with AName are found, the function returns nil.
    function NodeByName(const AName: Utf8String): TXmlNode;
    // Use this procedure to retrieve all nodes that have name AName. Pointers to
    // these nodes are added to the list in AList. AList must be initialized
    // before calling this procedure. If you use a TsdNodeList you don't need
    // to cast the list items to TXmlNode.
    procedure NodesByName(const AName: Utf8String; const AList: TList);
    // Add an open array of TXmlNode objects. Nodes must be owned by the xml document
    // beforehand.
    procedure NodesAdd(Nodes: array of TXmlNode);
    // Delete the subnode at Index. The node will also be freed, so do not free the
    // node in the application.
    procedure NodeDelete(Index: integer); virtual;
    // Extract the subnode at Index. The node will not be freed.
    function NodeExtract(ANode: TXmlNode): TXmlNode; virtual;
    // Remove the subnode. The node will also be freed, so do not free the
    // node in the application.
    procedure NodeRemove(ANode: TXmlNode); virtual;
    // Call NodeIndexOf to get the index for ANode in the Nodes list. The first
    // node in the list has index 0, the second item has index 1, and so on. If
    // a node is not in the list, NodeIndexOf returns -1.
    function NodeIndexOf(ANode: TXmlNode): integer; virtual;
    // Insert the node ANode at location Index in the list. Make sure to honour
    // the fact that attributes are also nodes, and should always be first in
    // the list. You can find the number of attributes with AttributeCount.
    procedure NodeInsert(Index: integer; ANode: TXmlNode); virtual;
    // Switch position of the nodes at Index1 and Index2.
    procedure NodeExchange(Index1, Index2: integer); virtual;
    // This function returns a pointer to the first node with AName. If this node
    // is not found, then it creates a new node with AName and returns its pointer.
    function NodeFindOrCreate(const AName: Utf8String): TXmlNode; virtual;
    // Create a new node with AName, add it to the subnode list, and return a
    // pointer to it.
    function NodeNew(const AName: Utf8String): TXmlNode; virtual;
    // Create a new node with AName, and insert it into the subnode list at location
    // Index, and return a pointer to it.
    function NodeNewAtIndex(Index: integer; const AName: Utf8String): TXmlNode; virtual;
    // Clear (and free) the complete list of subnodes.
    procedure NodesClear; virtual;
    // Find the first node which has name NodeName. Contrary to the NodeByName
    // function, this function will search the whole subnode tree, using the
    // DepthFirst method. It is possible to search for a full path too, e.g.
    // FoundNode := MyNode.FindNode('/Root/SubNode1/SubNode2/ThisNode');
    function FindNode(const NodeName: Utf8String): TXmlNode; virtual;
    // Find all nodes which have name NodeName. Contrary to the NodesByName
    // function, this function will search the whole subnode tree. If you use
    // a TsdNodeList for the AList parameter, you don't need to cast the list
    // items to TXmlNode.
    procedure FindNodes(const NodeName: Utf8String; const AList: TList); virtual;
    // Iterates the next sibling of Node
    function NextSibling(ANode: TXmlNode): TXmlNode; virtual;
    // Return the first subnode with AType, or nil if none
    function FirstNodeByType(AType: TsdElementType): TXmlNode; virtual;
    // Read TreeDepth to find out many nested levels there are for the current XML
    // node. Root has a TreeDepth of zero.
    function TreeDepth: integer;
    // The name of the node. For elements this is the element name. The string
    // is encoded as UTF8.
    property Name: Utf8String read GetName write SetName;
    // The name of the node. For elements this is the element name. The string
    // is encoded as UTF8.
    property NameUnicode: UnicodeString read GetNameUnicode write SetNameUnicode;
    // The value of the node. For elements this is the element value (based on
    // first chardata fragment), for attributes this is the attribute value. The
    // string is encoded as UTF8. Use ToWide(Node.Value) or Node.ValueUnicode
    // to get a UnicodeString compatible with "unicode" windows methods.
    property Value: Utf8String read GetValue write SetValue;
    // ValueUnicode returns the value of the node as a UnicodeString.
    property ValueUnicode: UnicodeString read GetValueUnicode write SetValueUnicode;
    // List of attributes present in this element. Use AttributeCount to iterate.
    property Attributes[Index: integer]: TsdAttribute read GetAttributes;
    // Get or set the name of the attribute at Index (as UTF8).
    property AttributeName[Index: integer]: Utf8String read GetAttributeName write SetAttributeName;
    // Get or set the value of the attribute at Index (as UTF8).
    property AttributeValue[Index: integer]: Utf8String read GetAttributeValue write SetAttributeValue;
    // Read this property to get the integer value of the attribute at index Index.
    // If the value cannot be converted, 0 will be returned. Write to it to set the
    // integer value.
    property AttributeValueAsInteger[Index: integer]: integer read GetAttributeValueAsInteger write SetAttributeValueAsInteger;
    // Get a reference to an attribute node by its name. If there is no attribute
    // with that name, nil will be returned.
    property AttributeByName[const AName: Utf8String]: TsdAttribute read GetAttributeByName;
    // Get the value of an attribute with name AName. If no attribute is present,
    // an empty string is returned. When setting this value, an attribute is
    // created if it does not yet exist.
    property AttributeValueByName[const AName: Utf8String]: Utf8String read
      GetAttributeValueByName write SetAttributeValueByName;
    property AttributeValueByNameWide[const AName: Utf8String]: UnicodeString read
      GetAttributeValueByNameWide write SetAttributeValueByNameWide;
    // Use HasAttribute to determine if the node has an attribute with name AName.
    function HasAttribute(const AName: Utf8String): boolean; virtual;
    // List of subnodes, by index. Iterate through the list using NodeCount
    // and this property. The attributes are listed first, then followed by
    // all other node types, in the order as found in the XML document.
    property Nodes[Index: integer]: TXmlNode read GetNodes; default;
    // Get number of subnodes present in this node (this includes attributes,
    // cdata, char-data, sub-elements, etcetera).
    property NodeCount: integer read GetNodeCount;
    // Get the number of attributes in this node
    property AttributeCount: integer read GetAttributeCount;
    // content of the node (raw source without the pre- and post matter)
    property Content: Utf8String read GetContent;
    // Fullpath will return the complete path of the node from the root, e.g.
    // /Root/SubNode1/SubNode2/ThisNode
    property FullPath: Utf8String read GetFullPath;
    // direct node count (aka the attributes and optional whitespace inbetween)
    property DirectNodeCount: integer read GetDirectNodeCount;
    // (child) container count
    property Containers[Index: integer]: TXmlNode read GetContainers;
    property ContainerCount: integer read GetContainerCount;

    // Get/Set ValueAsXYZ functions

    // Convert the node's value to boolean and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function GetValueAsBoolDef(ADefault: boolean): boolean; virtual;
    // Convert the node's value to a double and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function GetValueAsFloatDef(ADefault: double): double; virtual;
    // Convert the node's value to a TDateTime and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function GetValueAsDateTimeDef(ADefault: TDateTime): TDateTime; virtual;
    // Convert the node's value to integer and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function GetValueAsIntegerDef(ADefault: integer): integer; virtual;
    // Convert the node's value to int64 and return the result. If this conversion
    // fails, or no value is found, then the function returns ADefault.
    function GetValueAsInt64Def(ADefault: int64): int64; virtual;
    // Convert the node's value to boolean and return the result.
    function GetValueAsBool: boolean; virtual;
    // Convert the node's value to a double and return the result.
    function GetValueAsFloat: double; virtual;
    // Convert the node's value to a TDateTime and return the result.
    function GetValueAsDateTime: TDateTime; virtual;
    // Convert the node's value to integer and return the result.
    function GetValueAsInteger: integer; virtual;
    // Convert the node's value to int64 and return the result.
    function GetValueAsInt64: int64; virtual;
    // Store AValue as boolean
    procedure SetValueAsBool(const AValue: boolean); virtual;
    // Store AValue as float
    procedure SetValueAsFloat(const AValue: double); virtual;
    // Store AValue as Date
    procedure SetValueAsDate(const AValue: TDateTime); virtual;
    // Store AValue as Time
    procedure SetValueAsTime(const AValue: TDateTime); virtual;
    // Store AValue as DateTime
    procedure SetValueAsDateTime(const AValue: TDateTime); virtual;
    // Store AValue as Integer
    procedure SetValueAsInteger(const AValue: integer); virtual;
    // Store AValue as Int64
    procedure SetValueAsInt64(const AValue: int64); virtual;

    // ValueAsXYZ properties

    // Read and store existent value as boolean
    property ValueAsBool: boolean read GetValueAsBool write SetValueAsBool;
    // Read and store existent value as float
    property ValueAsFloat: double read GetValueAsFloat write SetValueAsFloat;
    // Store existent value as Date
    property ValueAsDate: TDateTime write SetValueAsDate;
    // Store existent value as Time
    property ValueAsTime: TDateTime write SetValueAsTime;
    // Read and store existent value as DateTime
    property ValueAsDateTime: TDateTime read GetValueAsDateTime write SetValueAsDateTime;
    // Read and store existent value as Integer
    property ValueAsInteger: integer read GetValueAsInteger write SetValueAsInteger;
    // Read and store existent value as Int64
    property ValueAsInt64: int64 read GetValueAsInt64 write SetValueAsInt64;

    // ReadXYZ functions

    // Find the attribute with AName, and convert its value to a boolean. If the
    // attribute is not found, or cannot be converted, the default ADefault will
    // be returned.
    function ReadAttributeBool(const AName: Utf8String; ADefault: boolean = False): boolean; virtual;
    // Find the attribute with AName, and convert its value to an integer. If the
    // attribute is not found, or cannot be converted, the default ADefault will
    // be returned.
    function ReadAttributeInteger(const AName: Utf8String; ADefault: integer = 0): integer; virtual;
    function ReadAttributeInt64(const AName: Utf8String; ADefault: int64 = 0): int64; virtual;       // added by hdk
    // Find the attribute with AName, and convert its value to a float. If the
    // attribute is not found, or cannot be converted, the default ADefault will
    // be returned.
    function ReadAttributeFloat(const AName: Utf8String; ADefault: double = 0): double; virtual;
    // Find the attribute with AName. If the attribute is not found, ADefault will
    // be returned.
    function ReadAttributeString(const AName: Utf8String; ADefault: Utf8String = ''): Utf8String; virtual;
    function ReadAttributeUnicodeString(const AName: Utf8String; ADefault: UnicodeString = ''): UnicodeString; virtual;   // added by hdk
    function ReadAttributeAnsiString(const AName: Utf8String; ADefault: AnsiString = ''): AnsiString; virtual;   // added by hdk
    // Read the subnode with AName and convert it to a boolean value. If the
    // subnode is not found, or cannot be converted, the boolean ADefault will
    // be returned.
    function ReadAttributeDateTime(const AName: Utf8String; ADefault: TDateTime = 0): TDateTime; virtual;     // added by hdk

    function ReadBool(const AName: Utf8String; ADefault: boolean = False): boolean; virtual;
    // Read the properties Color, Mode, Style and Width for the TPen object APen
    // from the subnode with AName.
    procedure ReadPen(const AName: Utf8String; APen: TPen); virtual;
    // Read the properties Color and Style for the TBrush object ABrush from the
    // subnode with AName.
    procedure ReadBrush(const AName: Utf8String; ABrush: TBrush); virtual;
    // Read the subnode with AName and convert its value to TColor. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadColor(const AName: Utf8String; ADefault: TColor = clBlack): TColor; virtual;
    // Read the subnode with AName and convert its value to TDateTime. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadDateTime(const AName: Utf8String; ADefault: TDateTime = 0): TDateTime; virtual;
    // Read the subnode with AName and convert its value to a double. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadFloat(const AName: Utf8String; ADefault: double = 0.0): double; virtual;
    // Read the subnode with AName and convert its value to an integer. If the
    // subnode is not found, or cannot be converted, ADefault will be returned.
    function ReadInteger(const AName: Utf8String; ADefault: integer = 0): integer; virtual;
    function ReadInt64(const AName: Utf8String; ADefault: int64 = 0): int64; virtual;           // added by hdk
    // Read the subnode with AName and return its UTF8String value. If the subnode is
    // not found, ADefault will be returned.
    function ReadString(const AName: Utf8String; const ADefault: Utf8String = ''): Utf8String; virtual;
    // Read the subnode with AName and return its UnicodeString value. If the subnode is
    // not found, ADefault will be returned.
    function ReadUnicodeString(const AName: Utf8String; const ADefault: UnicodeString = ''): UnicodeString; virtual;
    function ReadAnsiString(const AName: Utf8String; const ADefault: AnsiString = ''): AnsiString; virtual;   // added by hdk

    // WriteXYZ functions

    // If the attribute with name AName exists, then set its value to the integer
    // AValue. If it does not exist, then create a new attribute AName with the
    // integer value converted to a quoted string. If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeInteger(const AName: Utf8String; AValue: integer; ADefault: integer = 0); virtual;
    procedure WriteAttributeInt64(const AName: UTF8String; AValue: int64; ADefault: int64 = 0); virtual;       // added by hdk
    // If the attribute with name AName exists, then set its value to the float
    // AValue. If it does not exist, then create a new attribute AName with the
    // float value converted to a quoted string. If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeFloat(const AName: Utf8String; AValue: double; ADefault: double = 0); virtual;
    // If the attribute with name AName exists, then set its value to the string
    // AValue. If it does not exist, then create a new attribute AName with the
    // string value with quotes. If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeString(const AName: Utf8String; AValue: Utf8String; ADefault: Utf8String = ''); virtual;
    procedure WriteAttributeUnicodeString(const AName: Utf8String; const AValue: UnicodeString; const ADefault: UnicodeString = ''); virtual;
    procedure WriteAttributeAnsiString(const AName: Utf8String; const AValue: AnsiString; const ADefault: AnsiString = ''); virtual;   // added by hdk
   // If the attribute with name AName exists, then set its value to the TDateTime
    // AValue. If it does not exist, then create a new attribute AName with the
    // TDateTime value converted to a quoted string. If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeDateTime(const AName: Utf8String; AValue: TDateTime; ADefault: TDateTime = 0); virtual;  // changed by hdk
   // If the attribute with name AName exists, then set its value to the boolean
    // AValue. If it does not exist, then create a new attribute AName with the
    // boolean value converted to a quoted string. If ADefault = AValue, and
    // WriteOnDefault = False, no attribute will be added.
    procedure WriteAttributeBool(const AName: Utf8String; AValue: boolean; ADefault: boolean = False); virtual;
    // Add or replace the subnode with AName and set its value to represent the boolean
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteBool(const AName: Utf8String; AValue: boolean; ADefault: boolean = False); virtual;
    // Write properties Color, Mode, Style and Width of the TPen object APen to
    // the subnode with AName. If AName does not exist, it will be created.
    procedure WritePen(const AName: Utf8String; APen: TPen); virtual;
    // Write properties Color and Style of the TBrush object ABrush to the subnode
    // with AName. If AName does not exist, it will be created.
    procedure WriteBrush(const AName: Utf8String; ABrush: TBrush); virtual;
    // Add or replace the subnode with AName and set its value to represent the TColor
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteColor(const AName: Utf8String; AValue: TColor; ADefault: TColor = clBlack); virtual;
    // Add or replace the subnode with AName and set its value to represent the TDateTime
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    // The XML format used is compliant with W3C's specification of date and time.
    procedure WriteDateTime(const AName: Utf8String; AValue: TDateTime; ADefault: TDateTime = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the double
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteFloat(const AName: Utf8String; AValue: double; ADefault: double = 0.0); virtual;
    // Add or replace the subnode with AName and set its value to represent the hexadecimal representation of
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteHex(const AName: Utf8String; AValue, Digits: integer; ADefault: integer = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the integer
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteInteger(const AName: Utf8String; AValue: integer; ADefault: integer = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the int64
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteInt64(const AName: Utf8String; AValue: int64; ADefault: int64 = 0); virtual;
    // Add or replace the subnode with AName and set its value to represent the UTF8String
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteString(const AName, AValue: Utf8String; const ADefault: Utf8String = ''); virtual;
    // Add or replace the subnode with AName and set its value to represent the UnicodeString
    // AValue. If AValue = ADefault, and WriteOnDefault = False, no subnode will be added.
    procedure WriteUnicodeString(const AName: Utf8String; const AValue: UnicodeString; const ADefault: UnicodeString = ''); virtual;
    procedure WriteAnsiString(const AName: Utf8String; const AValue: AnsiString; const ADefault: AnsiString = ''); virtual;    // added by hdk

    // Returns the length of the data in the buffer, once it would be decoded by
    // the Base64 method. The length of the unencoded data is determined from the
    // length of the encoded data. Base64 must use the padding characters.
    function BufferLength: integer; virtual;
    // Use this method to read binary data from the node into Buffer with a length of Count.
    procedure BufferRead(var Buffer; Count: Integer; BinaryEncoding: TsdBinaryEncoding = xbeBase64); virtual;
    // Use this method to write binary data in Buffer with a length of Count to the
    // current node. The data will appear as text using Base64 method in the final XML document.
    procedure BufferWrite(const Buffer; Count: Integer); virtual;
    // Use BinaryString to add/extract binary data in an easy way to/from the node. Internally the
    // data gets stored as Base64-encoded data. Do not use this method for normal textual
    // information, it is better to use ValueAsString in that case (adds less overhead).
    property BinaryString: RawByteString read GetBinaryString write SetBinaryString;
    // return the index of the node in its parent
    function IndexInParent: integer;
    // sort the child nodes based on a compare function. If Compare = nil, just
    // alphabetical compare is used.
    procedure SortChildNodes(Compare: TXmlNodeCompareFunction);
  end;

  // List of nodes
  TsdNodeList = class(TObjectList)
  private
    function GetItems(Index: integer): TXmlNode;
    function GetNextSiblingOf(ANode: TXmlNode): TXmlNode;
    function GetLastSiblingOf(ANode: TXmlNode): TXmlNode;
  public
    // TsdNodeList has a different default than TObjectList
    // since 'AOwnsObjects' should usually be false in client code
    constructor Create(AOwnsObjects: boolean = false); virtual;
    // ByType returns the first item in the list that has element type AType.
    // If no item is found, the function returns nil.
    function ByType(AType: TsdElementType): TXmlNode;
    function FindFirst: TXmlNode;
    function FindNext(ANode: TXmlNode): TXmlNode;
    property Items[Index: integer]: TXmlNode read GetItems; default;
  end;

  TsdXmlNodeEvent = procedure(Sender: TObject; ANode: TXmlNode) of object;

  // Node representing a xml char-data fragment
  TsdCharData = class(TXmlNode)
  private
    function GetName: Utf8String; override;
    function GetValue: Utf8String; override;
    procedure SetName(const Value: Utf8String); override;
    procedure SetValue(const Value: Utf8String); override;
  protected
    FCoreValueID: integer;
    function GetCoreValue: Utf8String; virtual;
    procedure SetCoreValue(const Value: Utf8String); virtual;
    procedure CopyFrom(ANode: TXmlNode); override;
  public
    destructor Destroy; override;
    function GetValueUsingReferences(Nodes: array of TXmlNode): Utf8String;
    function ElementType: TsdElementType; override;
    function HasNonStandardReferences: boolean;
    procedure WriteStream(S: TStream); override;
  end;

  // Node representing whitespace chardata
  TsdWhiteSpace = class(TsdCharData)
  public
    function ElementType: TsdElementType; override;
  end;

  // Node representing quoted text ('bla' or "bla")
  TsdQuotedText = class(TsdCharData)
  private
    FQuoteChar: AnsiChar;
    function GetName: Utf8String; override;
  protected
    procedure CopyFrom(ANode: TXmlNode); override;
  public
    constructor Create(AOwner: TNativeXml); override;
    function ParseStream(Parser: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
  end;

  // Node representing an xml attribute.
  TsdAttribute = class(TXmlNode)
  private
    FNameID: integer;
    FCoreValue: TsdQuotedText;
    function GetName: Utf8String; override;
    procedure SetName(const Value: Utf8String); override;
    function GetValue: Utf8String; override;
    procedure SetValue(const Value: Utf8String); override;
  protected
    procedure CopyFrom(ANode: TXmlNode); override;
  public
    constructor Create(AOwner: TNativeXml); override;
    destructor Destroy; override;
    function ParseStream(Parser: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
  end;

  // TsdContainerNode is the base class for all element types that can have
  // sub-nodes.
  TsdContainerNode = class(TXmlNode)
  private
    FNodes: TsdNodeList;
    FDirectNodeCount: integer;
    FValueIndex: integer;
  protected
    function ParseAttributeList(Parser: TsdXmlParser): AnsiChar; virtual;
    function ParseQuotedTextList(Parser: TsdXmlParser): AnsiChar; virtual;
    procedure WriteAttributeList(S: TStream; Count: integer); virtual;
    function GetNodeCount: integer; override;
    function GetNodes(Index: integer): TXmlNode; override;
    function HasSubContainers: boolean; virtual;
    procedure CopyFrom(ANode: TXmlNode); override;
    property NodeList: TsdNodeList read FNodes;
    // count of the attributes
    function GetDirectNodeCount: integer; override;
    function GetContainers(Index: integer): TXmlNode; override;
    function GetContainerCount: integer; override;
  public
    constructor Create(AOwner: TNativeXml); override;
    destructor Destroy; override;
    procedure Clear; override;
    function NodeAdd(ANode: TXmlNode): integer; override;
    procedure NodeDelete(Index: integer); override;
    function NodeExtract(ANode: TXmlNode): TXmlNode; override;
    function NodeIndexOf(ANode: TXmlNode): integer; override;
    procedure NodeInsert(Index: integer; ANode: TXmlNode); override;
    procedure NodeExchange(Index1, Index2: integer); override;
    procedure NodesClear; override;
    function FirstNodeByType(AType: TsdElementType): TXmlNode; override;
  end;

  // Node representing an xml element.
  TsdElement = class(TsdContainerNode)
  private
    FNameID: integer;
    FNodeClosingStyle: TsdNodeClosingStyle;
  protected
    function GetName: Utf8String; override;
    function GetNodeClosingStyle: TsdNodeClosingStyle; virtual;
    function GetValue: Utf8String; override;
    procedure SetName(const Value: Utf8String); override;
    procedure SetNodeClosingStyle(const Value: TsdNodeClosingStyle); virtual;
    procedure SetValue(const Value: Utf8String); override;
    procedure ParseIntermediateData(Parser: TsdXmlParser); virtual;
    // parse the element list; the result (endtag) should be this element
    function ParseElementList(Parser: TsdXmlParser; const SupportedTags: TsdElementTypes): TXmlNode; virtual;
    procedure CopyFrom(ANode: TXmlNode); override;
  public
    function ParseStream(Parser: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
    property NodeClosingStyle: TsdNodeClosingStyle read GetNodeClosingStyle write SetNodeClosingStyle;
  end;

  // Node representing an xml declaration, e.g. <?xml version="1.0"?>
  TsdDeclaration = class(TsdContainerNode)
  private
    function GetEncoding: Utf8String;
    function GetVersion: Utf8String;
    procedure SetEncoding(const Value: Utf8String);
    procedure SetVersion(const Value: Utf8String);
  protected
    function GetName: Utf8String; override;
  public
    function ParseStream(Parser: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
    property Version: Utf8String read GetVersion write SetVersion;
    // encoding aka charset
    property Encoding: Utf8String read GetEncoding write SetEncoding;
  end;

  // Node representing an xml comment. Get/set Value for the comment.
  TsdComment = class(TsdCharData)
  protected
    function GetName: Utf8String; override;
  public
    function ParseStream(Parser: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
  end;

  // Node representing a CData element. Get/Set value for the data in CDATA.
  TsdCData = class(TsdComment)
  protected
    function GetName: Utf8String; override;
    function GetValue: Utf8String; override;
    procedure SetValue(const Value: Utf8String); override;
  public
    function ParseStream(Parser: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
  end;

  // Conditional Section
  TsdConditionalSection = class(TsdComment)
  end;

  // DocType declaration element. It can have sub-nodes with dtd elements,
  // entities, notations, etc.
  TsdDocType = class(TsdElement)
  private
    FExternalId: TsdCharData;
    FSystemLiteral: TsdQuotedText;
    FPubIDLiteral: TsdQuotedText;
  protected
    procedure ParseIntermediateData(Parser: TsdXmlParser); override;
    procedure CopyFrom(ANode: TXmlNode); override;
  public
    constructor Create(AOwner: TNativeXml); override;
    destructor Destroy; override;
    function ParseStream(Parser: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
    // External ID: either SYSTEM or PUBLIC
    property ExternalId: TsdCharData read FExternalId;
    // The system literal without quotes
    property SystemLiteral: TsdQuotedText read FSystemLiteral;
    // The PubID literal without quotes
    property PubIDLiteral: TsdQuotedText read FPubIDLiteral;
  end;

  // DTD Element declaration
  TsdDtdElement = class(TsdElement)
  private
  protected
    function GetValue: Utf8String; override;
    procedure WriteContent(S: TStream); override;
  public
    function ElementType: TsdElementType; override;
    function ParseStream(Parser: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
  end;

  // DTD AttList declaration
  TsdDtdAttList = class(TsdDtdElement)
  public
    function ElementType: TsdElementType; override;
  end;

  // DTD Entity declaration
  TsdDtdEntity = class(TsdDtdElement)
  public
    function ElementType: TsdElementType; override;
  end;

  // DTD Notation declaration
  TsdDtdNotation = class(TsdDtdElement)
  public
    function ElementType: TsdElementType; override;
  end;

  // (processing) instruction
  TsdInstruction = class(TsdCharData)
  protected
    function GetName: Utf8String; override;
  public
    function ElementType: TsdElementType; override;
    function ParseStream(Parser: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
  end;

  // TsdStyleSheet
  TsdStyleSheet = class(TsdInstruction)
  protected
    function GetName: Utf8String; override;
  public
    function ParseStream(Parser: TsdXmlParser): TXmlNode; override;
    procedure WriteStream(S: TStream); override;
    function ElementType: TsdElementType; override;
  end;

  // TNativeXml is a very fast XML reader (on typical hardware storage
  // 15 Mb per second), because it loads external data in chunks and buffers it in
  // memory. Use Create to create a new instance, use LoadFromFile/LoadFromStream to
  // load the XML document from a file or stream, and use SaveToFile and SaveToStream to
  // save the XML document.
  TNativeXml = class(TDebugComponent)
  private
    FOnDebugOut: TsdDebugEvent;
    //FOnDebugOut: TsdDebugEvent;
    procedure SetPreserveWhiteSpace(Value: boolean);
    procedure SetExternalEncoding(const Value: TsdStringEncoding);
  protected
    FRootNodes: TsdNodeList;
    FStringTable: TsdSymbolTable;
    //
    FAbortParsing: boolean;
    FDirectCloseTag: Utf8String;
    FDropCommentsOnParse: boolean;
    FEolStyle: TsdEolStyle;
    FFloatAllowScientific: boolean;
    FFloatSignificantDigits: integer;
    FExternalBomInfo: TsdBomInfo;
    FExternalCodePage: integer;
    FExternalEncoding: TsdStringEncoding;
    FFixStructuralErrors: boolean;
    FIndentString: Utf8String;
    FNodeClosingStyle: TsdNodeClosingStyle;
    FParserWarnings: boolean;
    FPreserveWhitespace: boolean;
    FSkipNormalisation: boolean;
    FXmlFormat: TsdXmlFormatType;
    FUseLocalBias: boolean;
    FWriteOnDefault: boolean;
    FSplitSecondDigits: integer;
    // events
    FOnNodeNew: TsdXmlNodeEvent;
    FOnNodeLoaded: TsdXmlNodeEvent;
    FOnProgress: TXmlProgressEvent;
    procedure DoNodeNew(ANode: TXmlNode);
    procedure DoNodeLoaded(ANode: TXmlNode);
    // GetParserPosition gives the parser's current position in the stream when
    // loading.
    function GetParserPosition(Parser: TsdXmlParser): int64;
    function GetCommentString: Utf8String;
    procedure SetCommentString(const Value: Utf8String);
    function GetStyleSheet: TsdStyleSheet;
    function GetCharset: Utf8String;
    procedure SetCharset(const Value: Utf8String);
    function GetRoot: TsdElement;
    function GetRootNodeCount: integer;
    function GetRootNodeClass: TsdNodeClass; virtual;
    function GetRootContainers(Index: integer): TsdContainerNode; virtual;
    function GetRootContainerCount: integer; virtual;
    function GetVersionString: Utf8String;
    procedure SetVersionString(const Value: Utf8String);
    // GetParserLineNumber gives the parser's current line number in the stream
    // when loading.
    function GetParserLineNumber(Parser: TsdXmlParser): int64;
    procedure MoveSubNodes(AList: TsdNodeList; FromNode, ToNode: TXmlNode);
    procedure DoProgress(Position: int64);
    function LineFeed: Utf8String;
    // ParseStream is called from any of the XmlNode descendants
    // and is the core method to get the xml data from external data to
    // the document object model.
    procedure ParseStream(Parser: TsdXmlParser);
    // WriteStream is called from any of the XmlNode descendants
    // and is the core method to write the xml data to the stream
    procedure WriteStream(S: TStream);
  public

    // constructors

    // Create an xml document with options for declaration and root element.
    constructor CreateEx(HasDeclaration, HasRootElement: boolean; AOwner: TComponent);
    // Use CreateName to Create a new Xml document that will automatically
    // contain a root element with name ARootName. This constructor also adds
    // the default declaration
    constructor CreateName(const ARootName: Utf8String; AOwner: TComponent = nil);
    // constructor with just the root element with an empty name
    constructor Create(AOwner: TComponent); override;
    // Destroys a TNativeXml instance
    destructor Destroy; override;

    // general methods

    // canonicalize XML (C14N process): after canonicalization of the document,
    // it will be.. encoded in utf-8 only, xml declaration removed, entities
    // expanded to their character equivalent, CDATA sections replaced by character
    // equivalent, special &lt; &gt; and &quot; entities encoded, attributes
    // normalized as if by validating parser, empty elements opened with start
    // and end tags, namespace declarations and attributes sorted.
    // The function returns the number of entities expanded.
    function Canonicalize: integer;
    // Clear all the nodes in the xml document
    procedure Clear; virtual;
    // class method: Decode base64-encoded data (Utf8String) to binary data (RawByteString)
    class function DecodeBase64(const Source: Utf8String; OnDebug: TsdDebugEvent): RawByteString;
    // class method: encode binary data (RawByteString) to Utf8String, adding a
    // control character (default #$0A) each 76 characters
    class function EncodeBase64(const Source: RawByteString; const ControlChars: Utf8String = #$0A): Utf8String;
    // Find first TXmlNode instance in the document, or nil if none found (aka document is empty)
    function FindFirst: TXmlNode;
    // Find next TXmlNode instance in the document, based on previous TXmlNode instance ANode
    function FindNext(ANode: TXmlNode): TXmlNode;
    // fire AEvent for each node in the document
    procedure ForEach(Sender: TObject; AEvent: TsdXmlNodeEvent);
    // IndentString is the string used for indentations. By default, it is a
    // tab (#$09). Set IndentString to something else if you need to have
    // specific indentation, or set it to an empty string to avoid indentation.
    property IndentString: Utf8String read FIndentString write FIndentString;
    // Insert a doctype right after the encoding.
    function InsertDocType(const AName: Utf8String): TsdDocType;
    // Function IsEmpty returns true if the root is clear, or in other words, the
    // root contains no value, no name, no subnodes and no attributes.
    function IsEmpty: boolean;
    // load from binary xml file (bxm). The advisory file extension is *.BXM
    // load the xml from a URL, and return the loaded size in bytes
    function LoadFromURL(const URL: Utf8String): int64; virtual;
    // Call procedure LoadFromFile to load an XML document from the filename
    // specified. See Create for an example. The LoadFromFile procedure will raise
    // an exception when it encounters non-wellformed XML.
    procedure LoadFromFile(const AFileName: string); virtual;
    // Load an XML document from the stream AStream. The LoadFromStream
    // procedure will raise an exception when it encounters non-wellformed XML.
    // This method can be used with any TStream descendant. The stream is read
    // chunk-wise (using 64K chunks). See also LoadFromFile and ReadFromString.
    procedure LoadFromStream(AStream: TStream); virtual;
    // Use New to make a new xml document
    procedure New; virtual;
    // parse substitute content from ANode (usually a TsdCharData). ANode will be
    // removed and the substitute content gets parsed and becomes part of the object model.
    function ParseSubstituteContentFromNode(ANode: TXmlNode; const ASubstitute: Utf8String): TXmlNode;
    // Call procedure ReadFromString to load an XML document from the UTF8String AValue.
    // The ReadFromString procedure will raise an exception of type EFilerError
    // when it encounters non-wellformed XML.
    procedure ReadFromString(const AValue: Utf8String); virtual;
    // Call SaveToFile to save the XML document to a file with FileName. If the
    // filename exists, it will be overwritten without warning. If the file cannot
    // be created, a standard I/O exception will be generated. Set XmlFormat to
    // xfReadable if you want the file to contain indentations to make the XML
    // more human-readable. This is not the default and also not compliant with
    // the XML specification.
    procedure SaveToFile(const AFileName: string); virtual;
    // Call SaveToStream to save the XML document to the Stream. Stream
    // can be any TStream descendant. Set XmlFormat to xfReadable if you want
    // the stream to contain indentations to make the XML more human-readable. This
    // is not the default and also not compliant with the XML specification. See
    // SaveToFile for information on how to save in special encoding.
    procedure SaveToStream(Stream: TStream); virtual;
    // Skip EOL normalisation (can be faster, but not compatible with xml spec).
    // Default value is FALSE.
    property SkipNormalisation: boolean read FSkipNormalisation write FSkipNormalisation;
    // Call WriteToString to write the entire XML document stream including
    // optional BOM to a generic string.
    function WriteToString: string; virtual;
    // Call WriteToLocalString to write the XML document to a Utf8String.
    function WriteToLocalString: Utf8String; virtual;
    // Call WriteToLocalUnicodeString to write the XML document to a UnicodeString.
    function WriteToLocalUnicodeString: UnicodeString; virtual;
    // Root is the topmost element in the XML document. Access Root to read any
    // child elements. When creating a new XML document, you can automatically
    // include a Root element, by creating using CreateName.
    property Root: TsdElement read GetRoot;
    // RootNodes can be used to directly access the nodes in the root of the
    // XML document. Usually this list consists of one declaration node followed
    // by an element node which is the Root. You can use this property to add or
    // delete comments, stylesheets, dtd's etc.
    property RootNodes: TsdNodeList read FRootNodes;
    // Payload rootnode class (TsdElement by default, but apps may create
    // a class that descends from TsdElement)
    property RootNodeClass: TsdNodeClass read GetRootNodeClass;
    // item count of the RootNodeList, ie usually max 3: the declaration, the DTD,
    // the Root (TsdElement or RootNodeClass descendant).
    property RootNodeCount: integer read GetRootNodeCount;
    // root containers
    property RootContainers[Index: integer]: TsdContainerNode read GetRootContainers;
    // number of root containers (as opposed to all root nodes)
    property RootContainerCount: integer read GetRootContainerCount;
    // A comment string above the root element <!--{comment}--> can be accessed with
    // this property. Assign a comment to this property to add it to the XML document.
    // Use property RootNodes to add/insert/extract multiple comments.
    property CommentString: Utf8String read GetCommentString write SetCommentString;
    // Set DropCommentsOnParse if you're not interested in any comment nodes in your object
    // model data. All comments encountered during parsing will simply be skipped and
    // not added as a node with ElementType = xeComment (which is default). Note that
    // when you set this option, you cannot later reconstruct an XML file with the comments
    // back in place.
    property DropCommentsOnParse: boolean read FDropCommentsOnParse write FDropCommentsOnParse;
    // After reading, this property contains the XML version (usually "1.0").
    property VersionString: Utf8String read GetVersionString write SetVersionString;
    // Charset (e.g. 'UTF-8', 'UTF-16' or any other multibyte/ansi codepage description.
    // This charset description is stored in the declaration node.
    // Example: In order to get this header:
    // <?xml version="1.0" encoding="UTF-16"?>
    // enter this code:
    // <CODE>MyXmlDocument.Charset := 'UTF-16';</CODE>
    // When reading a file, Charset will contain the encoding used.
    property Charset: Utf8String read GetCharset write SetCharset;
    // StringTable holds all the content (strings) in the xml tree
    property StringTable: TsdSymbolTable read FStringTable;
    // Get the stylesheet used for this XML document. If the node does not
    // exist yet, it will be created (thus if you use this property, and don't
    // set any of the attributes, an empty stylesheet node will be the result).
    property StyleSheet: TsdStyleSheet read GetStyleSheet;
    // External encoding is valid after loading, and indicates the encoding
    // detected in the external xml document. Internally, all string values are always
    // encoded in UTF8, so if the external stream is Ansi with codepage or UTF16, a conversion
    // is done. When writing to a file/stream, a BOM is generated for the two-byte
    // character encodings (UTF16LE and UUTF16BE). UTF8 uses *no BOM* according to
    // the XML specification.
    // Any conversion is done from UTF8 to external encodings if necessary. You can
    // *set* ExternalEncoding too but only for welldefined encodings (seUTF8, seUTF16LE,
    // seUTF16BE). If you want to use an ansi encoding, then set ExternalCodepage.
    property ExternalEncoding: TsdStringEncoding read FExternalEncoding write SetExternalEncoding;
    // the codepage used in the external xml document
    property ExternalCodepage: integer read FExternalCodepage write FExternalCodepage;
    // if ncUnknown (default), parsed setting will be preserved per element
    // if ncFull, single tags will be left full (eg '<bla x="1"></bla>')
    // if ncClose , single tags will be closed (eg '<bla x="1"/>')
    property NodeClosingStyle: TsdNodeClosingStyle read FNodeClosingStyle write FNodeClosingStyle;
    // XmlFormat by default is set to xfCompact. This setting is compliant to the spec,
    // and NativeXml will only generate XML files with #$0A as control character
    // after the declaration.
    // By setting XmlFormat to xfReadable, you can generate readable XML
    // files that contain indentation and end-of-lines after each element.
    property XmlFormat: TsdXmlFormatType read FXmlFormat write FXmlFormat;
    // EolStyle by default is set to esWindows.
    // esLinux writes just a LF (#$0A) as end-of-line
    // esWindows writes a CRLF (#$0D#$0A) as end-of-line
    property EolStyle: TsdEolStyle read FEolStyle write FEolStyle;
    // OnProgress event
    property OnProgress: TXmlProgressEvent read FOnProgress write FOnProgress;
    // Set PreserveWhiteSpace to True to preserve all whitespace present in the
    // file when reading. The blocks of whitespace are stored as CharData nodes.
    property PreserveWhiteSpace: boolean read FPreserveWhiteSpace write SetPreserveWhiteSpace;
    // Set AbortParsing to True if you use the OnNodeNew and OnNodeLoaded events in
    // a SAX-like manner, and you want to abort the parsing process halfway.
    property AbortParsing: boolean read FAbortParsing write FAbortParsing;
    // when true, NativeXmlEx will try to fix certain structural errors that usually
    // come from single tags in HTML (default = False)
    property FixStructuralErrors: boolean read FFixStructuralErrors write FFixStructuralErrors;
    // Set WriteOnDefault to False if you do not want to write default values to
    // the XML document. This option can avoid creating huge documents with
    // redundant info, and will speed up writing.
    property WriteOnDefault: boolean read FWriteOnDefault write FWriteOnDefault;
    // When converting floating point values to strings (e.g. in WriteFloat),
    // NativeXml will allow to output scientific notation in some cases, if the
    // result is significantly shorter than normal output, but only if the value
    // of FloatAllowScientific is True (default).
    property FloatAllowScientific: boolean read FFloatAllowScientific write FFloatAllowScientific;
    // When converting floating point values to strings (e.g. in WriteFloat),
    // NativeXml will use this number of significant digits. The default is
    // cDefaultFloatSignificantDigits, and set to 6.
    property FloatSignificantDigits: integer read FFloatSignificantDigits write FFloatSignificantDigits;
    // When converting date/time values to strings, NativeXml will use this
    // number of digits after the seconds. The default is cDefaultSplitSecondDigits,
    // and set to 0. With this default, no tens/hundreds/thousands after the second are used
    property SplitSecondDigits: integer read FSplitSecondDigits write FSplitSecondDigits;
    // When converting date/time values to strings, NativeXml will use a local bias
    // towards UTC if this option is True. Default is False.
    property UseLocalBias: boolean read FUseLocalBias write FUseLocalBias;
    // Connect to OnNodeNew to get informed of new nodes being added while loading.
    property OnNodeNew: TsdXmlNodeEvent read FOnNodeNew write FOnNodeNew;
    // Connect to OnNodeLoaded to get informed of nodes being finished loading.
    property OnNodeLoaded: TsdXmlNodeEvent read FOnNodeLoaded write FOnNodeLoaded;
    // Connect to OnDebugOut to get debug information in the client application
    property OnDebugOut: TsdDebugEvent read FOnDebugOut write FOnDebugOut;
  end;
 
const

  cNodeClass: array[TsdElementType] of TsdNodeClass =
    (TsdElement, TsdAttribute, TsdComment, TsdCData, TsdConditionalSection,
     TsdDeclaration, TsdStyleSheet, TsdDocType, TsdDtdElement, TsdDtdAttList,
     TsdDtdEntity, TsdDtdNotation, TsdInstruction, TsdCharData, TsdWhiteSpace,
     TsdQuotedText, nil, nil, nil);

const
  // chunk sizes: external stream is loaded/saved in these chunks of memory data
  // valid values are $4 - unbounded till memory size
  // sane values are $20 - $1000
  cParserChunkSize = $100;
  cWriterChunkSize = $100;

{
  NativeXmlUtils:
  Types, constants and utility functions of NativeXml
}
const

  // Count of different escape characters
  cEscapeCount = 5;

  // These are phrases that must be escaped. Note that "&" is first since
  // when another would be replaced first (eg ">" by "&lt;") this could
  // cause the new "&" in "&lt;" to be replaced by "&amp;";
  cXmlEscapePhrases: array[0..cEscapeCount - 1] of Utf8String =
    ('&',
     '<',
     '>',
     '''',
     '"');

  // These are the phrases that replace the escape phrases - in the same order
  // As a result, these phrases are visible in the core xml source
  cXmlReplacePhrases: array[0..cEscapeCount - 1] of Utf8String =
    ('&amp;',
     '&lt;',
     '&gt;',
     '&apos;',
     '&quot;');

  // special characters used for whitespace / blanks
  cXmlBlankChars: set of AnsiChar =
    [#$09, #$0A, #$0D, #$20];

  cXmlBlankCharsOrEndTag: set of AnsiChar =
    [#$09, #$0A, #$0D, #$20, '[', '/', '>'];

  cXmlQuoteChars: set of AnsiChar =
    ['''', '"'];

  // codepage IBM852, used for GUI implementations
  CP_852: integer = 852;

  // Windows-1250 codepage, used for GUI implementations
  CP_1250: integer = 1250;

  // Windows-1252 codepage, used for GUI implementations
  CP_1252: integer = 1252;

  // UTF8 codepage (outcommented to avoid clash in BCB - it is already defined
  // in windows)
  //CP_UTF8: integer = 65001;

  // UTF16 codepage
  CP_UTF16: integer = 1200;

  // ISO 8859-1 codepage, used for GUI implementations
  CP_ISO8859_1: integer = 28591;

  // These characters are used when generating BASE64 AnsiChars from buffer data
  cBase64Char: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  cBase64PadChar: AnsiChar = '=';

  cBomInfoListCount = 15;
  // array with Byte Order Mark (BOM) info
  cBomInfoList: array[0..cBomInfoListCount - 1] of TsdBomInfo =
  ( (BOM: ($3C,$3F,$78,$6D); Len: 4; Encoding: seAnsi;      HasBOM: false), // 0
    (BOM: ($EF,$BB,$BF,$00); Len: 3; Encoding: seUTF8;      HasBOM: true),
    (BOM: ($00,$00,$FE,$FF); Len: 4; Encoding: seUTF32BE;   HasBOM: true),
    (BOM: ($FF,$FE,$00,$00); Len: 4; Encoding: seUTF32LE;   HasBOM: true),
    (BOM: ($00,$00,$FF,$FE); Len: 4; Encoding: seUCS4_2143; HasBOM: true),
    (BOM: ($FE,$FF,$00,$00); Len: 4; Encoding: seUCS4_3412; HasBOM: true),
    (BOM: ($FE,$FF,$00,$00); Len: 2; Encoding: seUTF16BE;   HasBOM: true), //  6
    (BOM: ($FF,$FE,$00,$00); Len: 2; Encoding: seUTF16LE;   HasBOM: true), //  7
    (BOM: ($00,$00,$00,$3C); Len: 4; Encoding: seUTF32BE;   HasBOM: false),
    (BOM: ($3C,$00,$00,$00); Len: 4; Encoding: seUTF32LE;   HasBOM: false),
    (BOM: ($00,$00,$3C,$00); Len: 4; Encoding: seUCS4_2143; HasBOM: false),
    (BOM: ($00,$3C,$00,$00); Len: 4; Encoding: seUCS4_3412; HasBOM: false),
    (BOM: ($00,$3C,$00,$3F); Len: 4; Encoding: seUTF16BE;   HasBOM: false),
    (BOM: ($3C,$00,$3F,$00); Len: 4; Encoding: seUTF16LE;   HasBOM: false),
    (BOM: ($4C,$6F,$A7,$94); Len: 4; Encoding: seEBCDIC;    HasBOM: false)
  );
  cBomInfoIdxUTF16BE = 6;
  cBomInfoIdxUTF16LE = 7;

  cElementTypeNames: array[TsdElementType] of Utf8String =
    ('Element',  'Attribute', 'Comment', 'CData', 'ConditionalSection',
     'Declaration', 'Stylesheet', 'DocType', 'DtdElement', 'DtdAttList', 'DtdEntity',
     'DtdNotation', 'Instruction', 'CharData', 'WhiteSpace', 'QuotedText', 'Unknown',
     'EndTag', 'Error');
  // binary xml version
  // v1: stylesheet based on chardata
  // v2: stylesheet based on containernode
  cBinaryXmlVersion: cardinal = 2;

resourcestring

  sPrematureEnd                = 'stream terminated prematurely at pos %d';
  sInvalidStream               = 'invalid stream';
  sUnknownEncoding             = 'unknown encoding';
  sUnsupportedEncoding         = 'unsupported encoding (%s)';
  sDefaultCharUsed             = 'default char used for codepage substitution';
  sNotSupported                = 'feature is not supported yet';
  sIllegalTag                  = 'illegal tag ("%s") at pos %d';
  sUnsupportedTag              = 'unsupported tag ("%s") at pos %d';
  sIllegalEndTag               = 'illegal end tag ("%s") at line %d (streampos %d)';
  sQuoteCharExpected           = 'quote char expected at pos %d';
  sCannotAddNode               = 'cannot add node to this type of element';
  sCannotSetName               = 'cannot set name on this type of element';
  sCannotSetValue              = 'cannot set value on this type of element';
  sCannotManipulate            = 'cannot manipulate nodes in this type of element';
  sBeginEndMismatch            = 'begin and end tag mismatch: "%s" and "%s" at line %d (pos %d)';
  sLevelMismatch               = 'level mismatch between subnode "%s" and endnode "%s" at line %d (pos %d)';
  sRootElementNotDefined       = 'XML root element not defined.';
  sNonDefaultChardata          = 'non-default chardata at line %d (pos %d)';
  sSignificantDigitsOutOfRange = 'significant digits out of range';
  sMissingDataInBinaryStream   = 'missing data in binary stream';
  sErrorCalcStreamLength       = 'error while calculating streamlength';
  sXmlNodeNotAssigned          = 'XML node is not assigned';
  sXmlOwnerNotAssigned         = 'XML owner is not assigned';
  sUnknownBinaryEncodingBinhex = 'unknown encoding: xbeBinHex (deprecated)';


var

  // XML Defaults
  cDefaultDirectCloseTag:          Utf8String          = '/>';
  cDefaultDropCommentsOnParse:     boolean             = False;
  cDefaultFloatAllowScientific:    boolean             = True;
  cDefaultFloatSignificantDigits:  integer             = 6;
  cDefaultEncodingString:          Utf8String          = 'UTF-8';
  cDefaultEolStyle:                TsdEolStyle         = esWindows;
  cDefaultExternalEncoding:        TsdStringEncoding   = seUTF8;
  cDefaultFixStructuralErrors:     boolean             = False;
  cDefaultIndentString:            Utf8String          = #$09; // tab
  cDefaultNodeClosingStyle:        TsdNodeClosingStyle = ncClose;
  cDefaultPreserveWhiteSpace:      boolean             = False;
  cDefaultSortAttributes:          boolean             = False;
  cDefaultVersionString:           Utf8String          = '1.0';
  cDefaultXmlFormat:               TsdXmlFormatType    = xfCompact;
  cDefaultUseLocalBias:            boolean             = False;
  cDefaultWriteOnDefault:          boolean             = True;

  // helpful XML addtions
  cReadableDirectCloseTag:         Utf8String          = ' />';
  // see GetXmlFormatSettings in initialization section
  cXmlFormatSettings:              TFormatSettings;

{ Utility functions }

// Convert UnicodeString to Utf8String
function sdWideToUtf8(const W: UnicodeString): Utf8String;

// Convert UTF8 string to UnicodeString
function sdUtf8ToWide(const U: Utf8String): UnicodeString;

// Convert Ansi to Utf8 string
function sdAnsiToUtf8(const A: AnsiString; ACodePage: integer): Utf8String;

// Convert Utf8 to Ansi string
function sdUtf8ToAnsi(const U: Utf8String; ACodePage: integer): AnsiString;

function sdTrim(const S: Utf8String): Utf8String; overload;
function sdTrim(const S: Utf8String; var IsTrimmed: boolean): Utf8String; overload;

function sdNormaliseEol(const S: Utf8String): Utf8String;

function sdUnNormaliseEol(const S: Utf8String; EolStyle: TsdEolStyle): Utf8String;

function sdEscapeString(const AValue: Utf8String): Utf8String;

// replace escaped phrases and references written in the core xml source
// with replacement characters
function sdReplaceString(const AValue: Utf8String; var HasNonStandardReferences: boolean; References: array of TXmlNode): Utf8String; overload;
function sdReplaceString(const AValue: Utf8String; var HasNonStandardReferences: boolean): Utf8String; overload;
function sdReplaceString(const AValue: Utf8String): Utf8String; overload;

function sdCommaToDot(const AValue: Utf8String): Utf8String;

procedure sdWriteToStream(S: TStream; const Value: Utf8String);

// Based on the charset, find the codepage. If no charset is
// matched, the function returns ADefaultCodepage (default UTF-8, 65001)
function sdCharsetToCodePage(ACharset: Utf8String; ADefaultCodepage: integer = 65001): integer;
// Based on the charset, find the TsdStringEncoding. If no charset is
// matched, the function returns a encoding of seUTF8
function sdCharsetToStringEncoding(ACharset: Utf8String): TsdStringEncoding;

// find the encoding string corresponding to windows codepage
function sdCodepageToCharset(ACodepage: integer): Utf8String;

function Utf8CompareText(const S1, S2: Utf8String): integer;

// type conversions

// get the timezone bias
function GetTimeZoneBias: Integer;

// Convert the TDateTime ADate to a string according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
function sdDateTimeToString(ADate: TDateTime; UseDate: boolean = True; UseTime: boolean = True;
  SplitSecondDigits: integer = 0; UseLocalBias: boolean = False): Utf8String;

function sdBoolToString(Value: boolean): Utf8String;

// Convert a number to a Utf8String, using SignificantDigits to indicate the number of
// significant digits, and AllowScientific to allow for scientific notation if that
// results in much shorter notation.
function sdFloatToString(Value: double; SignificantDigits: integer; AllowScientific: boolean): Utf8String;

function sdIntToString(Value: integer): Utf8String;

function sdInt64ToString(Value: int64): Utf8String;

// Convert the Utf8String ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// If there is a conversion error, an exception will be raised.
function sdStringToDateTime(const ADate: Utf8String; UseLocalBias: Boolean = False): TDateTime;

// Convert the UTF8String ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// If there is a conversion error, the default value ADefault is returned.
function sdStringToDateTimeDef(const ADate: Utf8String; ADefault: TDateTime;
  UseLocalBias: Boolean = False): TDateTime;

// Encode binary data in Source as BASE64. The function returns the BASE64 encoded
// data as UTF8String, without any linebreaks.
function EncodeBase64(const Source: RawByteString): Utf8String;
function EncodeBase64Buf(const Buffer; Count: Integer): Utf8String;

// Decode BASE64 data in Source into binary data. The function returns the binary
// data as Utf8String. The Source Utf8String may contain linebreaks and control characters,
// these will be stripped.
function DecodeBase64(const Source: Utf8String): RawByteString;
procedure DecodeBase64Buf(var Source: Utf8String; var Buffer; Count: Integer);

// Decode BINHEX data in Source into RawByteStrng with binary data (for compatibility with old NativeXml)
function DecodeBinHex(const Source: Utf8String): RawByteString;
procedure DecodeBinhexBuf(var Source: Utf8String; var Buffer; Count: Integer);

// This function removes control characters from Utf8String AValue (Tab, CR, LF and Space)
function sdRemoveControlChars(const AValue: Utf8String): Utf8String;

// This function adds control characters Chars repeatedly after each Interval
// of characters to UTF8String Value. Default interval is 76 (seems to be used in many
// applications)
function sdAddControlChars(const AValue: Utf8String; const ControlChars: Utf8String; Interval: integer = 76): Utf8String;


// Convert Ansi to Utf8 using buffers
// please note: Utf8Buf can use 3x more size than AnsiBuf in extreme cases.
// Result is the Utf8Buf bytecount
function sdAnsiToUtf8Buffer(const AnsiBuf; var Utf8Buf; ACodePage, AnsiCount: integer): integer;

// Convert Utf8 to Ansi using buffers
function sdUtf8ToAnsiBuffer(const Utf8Buf; var AnsiBuf; ACodePage, Utf8Count: integer;
  var DefaultCharUsed: boolean): integer;

// determine the character length of the first Utf8 character in the buffer
function sdUtf8CharacterLength(const Buffer): integer;

// Convert a "WideString" (UTF16 LE) buffer to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the Utf8Buf must be at least 1.5 the size of the WideBuf.
// The function returns the number of *bytes* written.
function sdWideToUtf8Buffer(const WideBuf; var Utf8Buf; WideCount: integer): integer;

// Convert an UTF8 memory block to Unicode (UTF16 LE). This routine will process
// Count *bytes* of UTF8 (each character 1-3 bytes) into UTF16 (each char 2 bytes).
// Therefore, the block at Dst must be at least 2 times the size of Count, since
// many UTF8 characters consist of just one byte, and are mapped to 2 bytes. The
// function returns the number of *wide chars* written. Note that the Src block must
// have an exact number of UTF8 characters in it, if Count doesn't match then
// the last character will be converted anyway (going past the block boundary!)
function sdUtf8ToWideBuffer(const Utf8Buf; var WideBuf; ByteCount: integer): integer;

implementation

uses
  NativeXmlNodes;

type
  TAnsiCharArray = array[0..32767] of AnsiChar;


{ TXmlNode }

procedure TXmlNode.AttributeAdd(const AName, AValue: Utf8String);
var
  A: TsdAttribute;
begin
  A := TsdAttribute.Create(TNativeXml(FOwner));
  A.Name := AName;
  A.Value := AValue;
  NodeAdd(A);
end;

procedure TXmlNode.AttributeAdd(AAttribute: TsdAttribute);
begin
  if (AAttribute = nil) or (AAttribute.FOwner <> FOwner) then
  begin
    DoDebugOut(Self, wsFail, sXmlOwnerNotAssigned);
    exit;
  end;
  NodeAdd(AAttribute);
end;

procedure TXmlNode.AttributesAdd(Attributes: array of TsdAttribute);
var
  x: integer;
begin
  for x := Low(Attributes) to High(Attributes) do
    AttributeAdd(Attributes[x]);
end;

function TXmlNode.GetAttributeCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to GetNodeCount - 1 do
    if GetNodes(i) is TsdAttribute then
      inc(Result);
end;

constructor TXmlNode.Create(AOwner: TNativeXml);
begin
  inherited Create;
  FOwner := AOwner;
  if not assigned(FOwner) then
    raise Exception.Create(sXmlOwnerNotAssigned);
end;

constructor TXmlNode.CreateName(AOwner: TNativeXml; const AName: Utf8String);
begin
  Create(AOwner);
  Name := AName;
end;

constructor TXmlNode.CreateNameValue(AOwner: TNativeXml; const AName, AValue: Utf8String);
begin
  Create(AOwner);
  Name := AName;
  Value := AValue;
end;

function TXmlNode.ElementType: TsdElementType;
begin
  Result := xeUnknown;
end;

function TXmlNode.ElementTypeName: Utf8String;
begin
  Result := cElementTypeNames[ElementType];
end;

class function TXmlNode.EscapeString(const S: Utf8String): Utf8String;
begin
  Result := sdEscapeString(S);
end;

function TXmlNode.FirstNodeByType(AType: TsdElementType): TXmlNode;
begin
  Result := nil;
end;

class function TXmlNode.WideToUtf8(const W: UnicodeString): Utf8String;
begin
  Result := sdWideToUtf8(W);
end;

function TXmlNode.GetAttributeByName(const AName: Utf8String): TsdAttribute;
var
  i: integer;
  A: TsdAttribute;
begin
  for i := 0 to GetAttributeCount - 1 do
  begin
    A := GetAttributes(i);
    if Utf8CompareText(A.Name, AName) = 0 then
    begin
      Result := A;
      exit;
    end;
  end;
  Result := nil;
end;

function TXmlNode.GetAttributeName(Index: integer): Utf8String;
var
  A: TsdAttribute;
begin
  A := GetAttributes(Index);
  if assigned(A) then
    Result := A.Name
  else
    Result := '';
end;

function TXmlNode.GetAttributes(Index: integer): TsdAttribute;
var
  i, Idx: integer;
begin
  Idx := 0;
  Result := nil;
  for i := 0 to GetNodeCount - 1 do
  begin
    if GetNodes(i) is TsdAttribute then
    begin
      if Idx = Index then
      begin
        Result := TsdAttribute(GetNodes(i));
        exit;
      end;
      inc(Idx);
    end;
  end;
end;

function TXmlNode.GetAttributeValue(Index: integer): Utf8String;
var
  A: TsdAttribute;
begin
  A := GetAttributes(Index);
  if assigned(A) then
    Result := A.Value
  else
    Result := '';
end;

function TXmlNode.GetAttributeValueAsInteger(Index: integer): integer;
begin
  Result := StrToIntDef(GetAttributeValue(Index), 0);
end;

function TXmlNode.GetAttributeValueByName(const AName: Utf8String): Utf8String;
var
  A: TsdAttribute;
begin
  A := GetAttributeByName(AName);
  if assigned(A) then
    Result := A.Value
  else
    Result := '';
end;

function TXmlNode.GetIndent: Utf8String;
var
  i: integer;
begin
  Result := '';
  if assigned(FOwner) then
  begin
    case GetXmlFormat of
    xfCompact: Result := '';
    xfReadable:
      for i := 0 to TreeDepth - 1 do
        Result := Result + TNativeXml(FOwner).IndentString;
    end; //case
  end;
end;

function TXmlNode.GetEndOfLine: Utf8String;
begin
  Result := '';
  if GetXmlFormat = xfReadable then
  begin
    case GetEolStyle of
    esLinux:   Result := #$0A;
    esWindows: Result := #$0D#$0A;
    end; //case
  end;
end;

function TXmlNode.GetSeparator: Utf8String;
begin
  Result := #$0A;
  if GetEolStyle = esWindows then
    Result := #$0D#$0A;
end;

function TXmlNode.GetName: Utf8String;
begin
  Result := '';
end;

function TXmlNode.GetNameUnicode: UnicodeString;
begin
  Result := sdUtf8ToWide(GetName);
end;

function TXmlNode.GetNodes(Index: integer): TXmlNode;
begin
  Result := nil;
end;

function TXmlNode.GetParentNode(ADepth: integer): TXmlNode;
var
  i: integer;
begin
  Result := Self;
  for i := 0 to ADepth do
  begin
    Result := Result.FParent;
    if not assigned(Result) then
      exit;
  end;
end;

function TXmlNode.GetParentNodeName(ADepth: integer): Utf8String;
var
  Node: TXmlNode;
begin
  // parent node name
  Node := GetParentNode(ADepth);
  if assigned(Node) then
    Result := Node.GetName
  else
    Result := '';
end;

function TXmlNode.GetValue: Utf8String;
begin
  Result := '';
end;

function TXmlNode.GetValueUnicode: UnicodeString;
begin
  Result := sdUtf8ToWide(GetValue);
end;

function TXmlNode.IsClear: boolean;
begin
  Result := IsEmpty and (length(Name) = 0);
end;

function TXmlNode.IsEmpty: boolean;
begin
  Result := (GetNodeCount = 0) and (length(Value) = 0)
end;

function TXmlNode.IsEqualTo(ANode: TXmlNode; Options: TXmlCompareOptions; MismatchNodes: TList): boolean;
{  TXmlCompareOption = (
    xcNodeName,
    xcNodeType,
    xcNodeValue,
    xcAttribCount,
    xcAttribNames,
    xcAttribValues,
    xcChildCount,
    xcChildNames,
    xcChildValues,
    xcRecursive
  );}
var
  ThisSubNode, ThatSubNode: TXmlNode;
  NodeResult, ChildResult: boolean;
  // local
  procedure AddMismatchNode(ANode: TXmlNode);
  begin
    if assigned(MismatchNodes) then
      MismatchNodes.Add(ANode);
  end;
  // local
  function NodeCompareOptions: boolean;
  begin
    // We assume there are differences
    Result := False;

    // node name
    if xcNodeName in Options then
      if Utf8CompareText(Name, ANode.Name) <> 0 then
        exit;

    // node type
    if xcNodeType in Options then
      if ElementType <> ANode.ElementType then
        exit;

    // node value
    if xcNodeValue in Options then
      if Utf8CompareText(Value, ANode.Value) <> 0 then
        exit;

    // attribute count
    if xcAttribCount in Options then
      if AttributeCount <> ANode.AttributeCount then
        exit;

    // child container count
    if xcChildCount in Options then
      if ContainerCount <> ANode.ContainerCount then
        exit;

    // If we arrive here, it means no differences were found, return True
    Result := True;
  end;
  // local
  function ChildCompareOptions: boolean;
  var
    i: integer;
  begin
    Result := True;

    // child and attribute node names and values
    if Options * [xcChildNames, xcChildValues, xcAttribNames, xcAttribValues] <> [] then
    begin
      // iterate nodes
      for i := 0 to NodeCount - 1 do
      begin
        ThisSubNode := Nodes[i];
        if (ThisSubNode is TsdAttribute) or (ThisSubNode is TsdElement) then
        begin
          ThatSubNode := ANode.NodeByName(ThisSubNode.Name);
          if not assigned(ThatSubNode) then
          begin
            // No we dont have it
            if (xcChildNames in Options) or (xcAttribNames in Options) then
            begin
              AddMismatchNode(ThisSubNode);
              Result := False;
            end;
          end else
          begin
            // Do child and attribute value check
            if (xcChildValues in Options) or (xcAttribValues in Options) then
            begin
              if Utf8CompareText(ThisSubNode.Value, ThatSubNode.Value) <> 0 then
              begin
                AddMismatchNode(ThisSubNode);
                Result := False;
              end;
            end;
            // Do recursive check
            if xcRecursive in Options then
              if not ThisSubNode.IsEqualTo(ThatSubNode, Options, MismatchNodes) then
                Result := False;
          end;
        end;
      end;
    end;
  end;
// main
begin
  Result := False;
  if not assigned(ANode) then
    exit;

  // node compare options
  NodeResult := NodeCompareOptions;
  if NodeResult = False then
    AddMismatchNode(Self);

  // child compare options
  ChildResult := ChildCompareOptions;

  // final result
  Result := NodeResult and ChildResult;
end;

function TXmlNode.NodeAdd(ANode: TXmlNode): integer;
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotAddNode);
end;

function TXmlNode.NodeByName(const AName: Utf8String): TXmlNode;
var
  i: integer;
begin
  for i := 0 to GetNodeCount - 1 do
    if Utf8CompareText(GetNodes(i).Name, AName) = 0 then
    begin
      Result := GetNodes(i);
      exit;
    end;
  Result := nil;
end;

function TXmlNode.GetNodeCount: integer;
begin
  // functionality is in descendant TsdContainerNode
  Result := 0;
end;

procedure TXmlNode.NodeDelete(Index: integer);
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotManipulate);
end;

procedure TXmlNode.NodesClear;
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotManipulate);
end;

procedure TXmlNode.NodeRemove(ANode: TXmlNode);
var
  Idx: integer;
begin
  Idx := NodeIndexOf(ANode);
  if Idx >= 0 then
    NodeDelete(Idx);
end;

function TXmlNode.NodeExtract(ANode: TXmlNode): TXmlNode;
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotManipulate);
end;

procedure TXmlNode.NodeExchange(Index1, Index2: integer);
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotManipulate);
end;

function TXmlNode.NodeIndexOf(ANode: TXmlNode): integer;
begin
  // functionality is in descendant TsdContainerNode
  Result := -1;
end;

procedure TXmlNode.NodeInsert(Index: integer; ANode: TXmlNode);
begin
  // functionality is in descendant TsdContainerNode
  raise Exception.Create(sCannotAddNode);
end;

function TXmlNode.NodeNew(const AName: Utf8String): TXmlNode;
// Add a new child node and return its pointer
var
  NodeClass: TsdNodeClass;
begin
  NodeClass := cNodeClass[ElementType];
  if not assigned(NodeClass) then
  begin
    Result := nil;
    exit;
  end;

  // Create new node
  Result := NodeClass.Create(TNativeXml(FOwner));
  if assigned(Result) then
  begin
    Result.Name := AName;
    NodeAdd(Result);
  end;
end;

function TXmlNode.NodeNewAtIndex(Index: integer; const AName: Utf8String): TXmlNode;
// Create a new node with AName, and insert it into the subnode list at location
// Index, and return a pointer to it.
var
  NodeClass: TsdNodeClass;
begin
  NodeClass := cNodeClass[ElementType];
  if not assigned(NodeClass) then
  begin
    Result := nil;
    exit;
  end;

  // Create new node
  Result := NodeClass.Create(TNativeXml(FOwner));
  if assigned(Result) then
  begin
    Result.Name := AName;
    NodeInsert(Index, Result);
  end;
end;

function TXmlNode.ParseStream(Parser: TsdXmlParser): TXmlNode;
// Result = EndNode
begin
  // functionality in descendants
  Result := Self;
end;

procedure TXmlNode.SetAttributeName(Index: integer; const Value: Utf8String);
var
  A: TsdAttribute;
begin
  A := GetAttributes(Index);
  if not assigned(A) then
    exit;
  A.Name := Value;
end;

procedure TXmlNode.SetAttributeValue(Index: integer; const Value: Utf8String);
var
  A: TsdAttribute;
begin
  A := GetAttributes(Index);
  if not assigned(A) then
    exit;
  A.Value := Value;
end;

procedure TXmlNode.SetAttributeValueAsInteger(Index: integer; const Value: integer);
begin
  SetAttributeValue(Index, IntToStr(Value));
end;

procedure TXmlNode.SetAttributeValueByName(const AName, Value: Utf8String);
var
  A: TsdAttribute;
begin
  A := GetAttributeByName(AName);
  if not assigned(A) then
  begin
    A := TsdAttribute.Create(TNativeXml(FOwner));
    A.Name := AName;
    NodeAdd(A);
  end;
  A.Value := Value;
end;

procedure TXmlNode.SetName(const Value: Utf8String);
begin
  // functionality in descendants
  raise Exception.Create(sCannotSetName);
end;

procedure TXmlNode.SetNameUnicode(const Value: UnicodeString);
begin
  SetName(sdWideToUtf8(Value));
end;

procedure TXmlNode.SetValue(const Value: Utf8String);
begin
  // functionality in descendants
  raise Exception.Create(sCannotSetValue);
end;

procedure TXmlNode.SetValueUnicode(const Value: UnicodeString);
begin
  SetValue(sdWideToUtf8(Value));
end;

function TXmlNode.GetString(AID: integer): Utf8String;
var
  Table: TsdSymbolTable;
begin
  Result := '';
  if assigned(FOwner) then
  begin
    Table := TNativeXml(FOwner).FStringTable;
    if assigned(Table) then
      Result := Table.GetString(AID);
  end;
end;

function TXmlNode.AddString(const S: Utf8String): integer;
var
  Table: TsdSymbolTable;
begin
  Result := 0;
  if assigned(FOwner) then
  begin
    Table := TNativeXml(FOwner).FStringTable;
    if assigned(Table) then
      Result := Table.AddString(S)
  end;
end;

class function TXmlNode.Utf8ToWide(const S: Utf8String): UnicodeString;
begin
  Result := sdUtf8ToWide(S);
end;

function TXmlNode.TreeDepth: integer;
begin
  if assigned(FParent) then
    Result := FParent.TreeDepth + 1
  else
    Result := 0;
end;

class function TXmlNode.ReplaceString(const S: Utf8String): Utf8String;
begin
  Result := sdReplaceString(S);
end;

procedure TXmlNode.WriteStream(S: TStream);
begin
// functionality is in descendants
end;

function TXmlNode.ReadAttributeBool(const AName: Utf8String; ADefault: boolean = False): boolean;
begin
  Result := StrToBoolDef(AttributeValueByName[AName], ADefault);
end;

function TXmlNode.ReadAttributeInteger(const AName: Utf8String; ADefault: integer = 0): integer;
begin
  Result := StrToIntDef(AttributeValueByName[AName], ADefault);
end;

function TXmlNode.ReadAttributeInt64(const AName: Utf8String; ADefault: int64): int64;  // added by hdk
begin
  Result := StrToInt64Def(AttributeValueByName[AName], ADefault);
end;

function TXmlNode.ReadAttributeFloat(const AName: Utf8String; ADefault: double = 0): double;
begin
  Result := StrToFloatDef(AttributeValueByName[AName], ADefault, cXmlFormatSettings); // changed by hdk
end;

function TXmlNode.ReadAttributeString(const AName: Utf8String; ADefault: Utf8String = ''): Utf8String;
begin
  Result := AttributeValueByName[AName];
  if Length(Result) = 0 then
    Result := ADefault;
end;

function TXmlNode.ReadAttributeUnicodeString(const AName: Utf8String; ADefault: UnicodeString): UnicodeString;  // added by hdk
begin
  Result := sdUtf8ToWide(AttributeValueByName[AName]);
  if Length(Result) = 0 then
    Result := ADefault;
end;

function TXmlNode.ReadAttributeAnsiString(const AName: Utf8String; ADefault: AnsiString): AnsiString;   // added by hdk
begin
  Result := sdUtf8ToAnsi(AttributeValueByName[AName], CP_ACP);
  if Length(Result) = 0 then
    Result := ADefault;
end;

function TXmlNode.ReadAttributeDateTime(const AName: Utf8String; ADefault: TDateTime): TDateTime;  // added by hdk
begin
  Result := sdStringToDateTimeDef(AttributeValueByName[AName], ADefault, TNativeXml(FOwner).FUseLocalBias);
end;

function TXmlNode.ReadBool(const AName: Utf8String; ADefault: boolean = False): boolean;
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.GetValueAsBoolDef(ADefault);
end;

procedure TXmlNode.ReadPen(const AName: UTF8String; APen: TPen);
var
  Child: TXmlNode;
begin
  Child := NodeByName(AName);
  if assigned(Child) then with Child do
  begin
    // Read values
    APen.Color := ReadColor('Color', clBlack);
    APen.Mode  := TPenMode(ReadInteger('Mode', integer(pmCopy)));
    APen.Style := TPenStyle(ReadInteger('Style', integer(psSolid)));
    APen.Width := ReadInteger('Width', 1);
  end else
  begin
    // Defaults
    APen.Color := clBlack;
    APen.Mode := pmCopy;
    APen.Style := psSolid;
    APen.Width := 1;
  end;
end;

procedure TXmlNode.ReadBrush(const AName: Utf8String; ABrush: TBrush);
var
  Child: TXmlNode;
begin
  Child := NodeByName(AName);
  if assigned(Child) then with Child do
  begin
    // Read values
    ABrush.Color  := ReadColor('Color', clWhite);
    ABrush.Style  := TBrushStyle(ReadInteger('Style', integer(bsSolid)));
  end else
  begin
    // Defaults
    ABrush.Bitmap := nil;
    ABrush.Color  := clWhite;
    ABrush.Style  := bsSolid;
  end;
end;

function TXmlNode.ReadColor(const AName: Utf8String; ADefault: TColor = 0): TColor;
begin
  Result := ReadInteger(AName, integer(ADefault));
end;

function TXmlNode.ReadDateTime(const AName: Utf8String; ADefault: TDateTime): TDateTime;
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.GetValueAsDateTimeDef(ADefault);
end;

function TXmlNode.ReadFloat(const AName: UTF8String; ADefault: double): double;
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.GetValueAsFloatDef(ADefault);
end;

function TXmlNode.ReadInteger(const AName: Utf8String; ADefault: integer): integer;
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.GetValueAsIntegerDef(ADefault);
end;

function TXmlNode.ReadInt64(const AName: Utf8String; ADefault: int64): int64;    // added by hdk
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.GetValueAsInt64Def(ADefault);
end;

function TXmlNode.ReadString(const AName: Utf8String; const ADefault: Utf8String = ''): Utf8String;
var
  Child: TXmlNode;
begin
  Result := ADefault;
  Child := NodeByName(AName);
  if assigned(Child) then
    Result := Child.Value;
end;

function TXmlNode.ReadUnicodeString(const AName: UTF8String; const ADefault: UnicodeString): UnicodeString;
begin
  Result := sdUtf8ToWide(ReadString(AName, sdWideToUtf8(ADefault)));
end;

function TXmlNode.ReadAnsiString(const AName: Utf8String; const ADefault: AnsiString): AnsiString;    // added by hdk
begin
  Result := sdUtf8ToAnsi(ReadString(AName, sdAnsiToUtf8(ADefault, CP_ACP)), CP_ACP);
end;

function TXmlNode.GetValueAsBoolDef(ADefault: boolean): boolean;
begin
  Result := StrToBoolDef(GetValue, ADefault);
end;

function TXmlNode.GetValueAsDateTimeDef(ADefault: TDateTime): TDateTime;
begin
  Result := sdStringToDateTimeDef(GetValue, ADefault);
end;

function TXmlNode.GetValueAsFloatDef(ADefault: double): double;
var
  V: Utf8String;
begin
  // backwards compat: old version used to allow commas in floats
  V := sdCommaToDot(GetValue);
  Result := StrToFloatDef(V, ADefault, cXmlFormatSettings);    // changed by hdk
end;

function TXmlNode.GetValueAsIntegerDef(ADefault: integer): integer;
begin
  Result := StrToIntDef(GetValue, ADefault);
end;

function TXmlNode.GetValueAsInt64Def(ADefault: int64): int64;
begin
  Result := StrToInt64Def(GetValue, ADefault);
end;

function TXmlNode.GetValueAsBool: boolean;
begin
  Result := StrToBool(GetValue);
end;

function TXmlNode.GetValueAsDateTime: TDateTime;
begin
  Result := sdStringToDateTime(GetValue);
end;

function TXmlNode.GetValueAsFloat: double;
begin
  Result := StrToFloat(GetValue, cXmlFormatSettings);    // changed by hdk
end;

function TXmlNode.GetValueAsInteger: integer;
begin
  Result := StrToInt(GetValue);
end;

function TXmlNode.GetValueAsInt64: int64;
begin
  Result := StrToInt64(GetValue);
end;

procedure TXmlNode.SetValueAsBool(const AValue: boolean);
begin
  SetValue(sdBoolToString(AValue));
end;

procedure TXmlNode.SetValueAsDate(const AValue: TDateTime);
begin
  SetValue(sdDateTimeToString(AValue, True, False, 0, False));
end;

procedure TXmlNode.SetValueAsTime(const AValue: TDateTime);
begin
  SetValue(sdDateTimeToString(AValue, False, True,
    0, TNativeXml(FOwner).FUseLocalBias));
end;

procedure TXmlNode.SetValueAsDateTime(const AValue: TDateTime);
begin
  SetValue(sdDateTimeToString(AValue, TNativeXml(FOwner).FUseLocalBias));
end;

procedure TXmlNode.SetValueAsFloat(const AValue: double);
begin
  SetValue(sdFloatToString(AValue,
    TNativeXml(FOwner).FFloatSignificantDigits,
    TNativeXml(FOwner).FFloatAllowScientific));
end;

procedure TXmlNode.SetValueAsInteger(const AValue: integer);
begin
  SetValue(sdIntToString(AValue));
end;

procedure TXmlNode.SetValueAsInt64(const AValue: int64);
begin
  SetValue(sdInt64ToString(AValue));
end;

procedure TXmlNode.NodesByName(const AName: Utf8String; const AList: TList);
// Fill AList with nodes that have name AName
var
  i: integer;
begin
  if not assigned(AList) or not assigned(Self) then
    exit;
  AList.Clear;
  for i := 0 to GetNodeCount - 1 do
    if Utf8CompareText(Nodes[i].Name, AName) = 0 then
      AList.Add(Nodes[i]);
end;

procedure TXmlNode.WriteBool(const AName: Utf8String; AValue, ADefault: boolean);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdBoolToString(AValue));
end;

procedure TXmlNode.WriteDateTime(const AName: Utf8String; AValue, ADefault: TDateTime);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdDateTimeToString(AValue,
      TNativeXml(FOwner).FUseLocalBias));
end;

procedure TXmlNode.WriteFloat(const AName: UTF8String; AValue, ADefault: double);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdFloatToString(AValue,
      TNativeXml(FOwner).FFloatSignificantDigits,
      TNativeXml(FOwner).FFloatAllowScientific));
end;

procedure TXmlNode.WriteHex(const AName: UTF8String; AValue, Digits: integer; ADefault: integer);
var
  HexString: Utf8String;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    HexString := '$' + Utf8String(IntToHex(AValue, Digits));
    WriteValue(AName, HexString);
  end;
end;

procedure TXmlNode.WriteInteger(const AName: Utf8String; AValue, ADefault: integer);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdIntToString(AValue));
end;

procedure TXmlNode.WriteInt64(const AName: Utf8String; AValue, ADefault: int64);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, sdInt64ToString(AValue));
end;

procedure TXmlNode.WriteString(const AName, AValue, ADefault: Utf8String);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteValue(AName, AValue);
end;

procedure TXmlNode.WriteUnicodeString(const AName: Utf8String; const AValue, ADefault: UnicodeString);
begin
  WriteString(AName, sdWideToUtf8(AValue), sdWideToUtf8(ADefault));
end;

procedure TXmlNode.WriteAnsiString(const AName: Utf8String; const AValue, ADefault: AnsiString);   // added by hdk
begin
  WriteString(AName, sdAnsiToUtf8(AValue, CP_ACP), sdAnsiToUtf8(ADefault, CP_ACP));
end;

procedure TXmlNode.NodesAdd(Nodes: array of TXmlNode);
var
  x: integer;
begin
  for x := Low(Nodes) to High(Nodes) do
    NodeAdd(Nodes[x]);
end;

function TXmlNode.GetWriteOnDefault: boolean;
begin
  if assigned(FOwner) then
    Result := TNativeXml(FOwner).WriteOnDefault
  else
    Result := False;
end;

procedure TXmlNode.SetWriteOnDefault(const Value: boolean);
begin
  if assigned(FOwner) then
    TNativeXml(FOwner).WriteOnDefault := Value;
end;

function TXmlNode.NodeFindOrCreate(const AName: Utf8String): TXmlNode;
// Find the node with AName, and if not found, add new one
begin
  Result := NodeByName(AName);
  if not assigned(Result) then
    Result := NodeNew(AName);
end;

function TXmlNode.NodeIndexByName(const AName: Utf8String): integer;
begin
  Result := 0;
  while Result < NodeCount do
  begin
    if Utf8CompareText(Nodes[Result].Name, AName) = 0 then
      exit;
    inc(Result);
  end;
  if Result = NodeCount then
    Result := -1;
end;

function TXmlNode.AttributeIndexByName(const AName: Utf8String): integer;
begin
  Result := 0;
  // attributes are nodes from 0 to DirectNodeCount - 1
  while Result < DirectNodeCount do
  begin
    if Utf8CompareText(Nodes[Result].Name, AName) = 0 then
      exit;
    inc(Result);
  end;
  if Result = DirectNodeCount then
    Result := -1;
end;

procedure TXmlNode.WriteValue(const AName, AValue: Utf8String);
var
  Child: TXmlNode;
begin
  Child := NodeFindOrCreate(AName);
  if assigned(Child) then
    Child.Value := AValue;
end;

procedure TXmlNode.DoProgress(Position: int64);
begin
  // Call the onprogress
  if assigned(FOwner) then
    TNativeXml(FOwner).DoProgress(Position);
end;

function TXmlNode.BufferLength: integer;
var
  BufData: Utf8String;
  BufPos: integer;
begin
  BufData := sdRemoveControlChars(GetValue);
  Result := length(BufData) div 4;
  if Result * 4 <> length(BufData) then
    raise EFilerError.Create(sErrorCalcStreamLength);
  Result := Result * 3;
  // Check padding chars
  BufPos := length(BufData);
  if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
  begin
    dec(BufPos);
    dec(Result);
    if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
      dec(Result);
  end;
end;

procedure TXmlNode.BufferRead(var Buffer; Count: Integer; BinaryEncoding: TsdBinaryEncoding);
// Read data from XML base64/Binhex to the buffer (default is xbeBase64)
var
  BufData: Utf8String;
begin
  BufData := sdRemoveControlChars(GetValue);
  case BinaryEncoding of
  xbeBase64:
    // this is the default method
    DecodeBase64Buf(BufData, Buffer, Count);
  xbeBinHex:
    // for compat with older versions
    DecodeBinhexBuf(BufData, Buffer, Count);
  end;
end;

procedure TXmlNode.BufferWrite(const Buffer; Count: Integer);
// Write data from the buffer to XML in base64 format
var
  BufData: Utf8String;
begin
  if Count > 0 then
    BufData := EncodeBase64Buf(Buffer, Count);

  // For comformity with Base64, we must add linebreaks
  SetValue(sdAddControlChars(BufData, GetEndOfLine + GetIndent));
end;

procedure TXmlNode.WriteAttributeInteger(const AName: Utf8String; AValue, ADefault: integer);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdIntToString(AValue);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeInt64(const AName: UTF8String; AValue, ADefault: int64);  // added by hdk
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdInt64ToString(AValue);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeFloat(const AName: Utf8String; AValue, ADefault: double);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdFloatToString(AValue,
            TNativeXml(FOwner).FFloatSignificantDigits,
            TNativeXml(FOwner).FFloatAllowScientific);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeString(const AName: Utf8String; AValue, ADefault: Utf8String);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := AValue;
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeUnicodeString(const AName: Utf8String; const AValue, ADefault: UnicodeString);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdWideToUtf8(AValue);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeAnsiString(const AName: Utf8String; const AValue, ADefault: AnsiString);   // added by hdk
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdAnsiToUtf8(AValue, CP_ACP);          
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeDateTime(const AName: Utf8String; AValue, ADefault: TDateTime);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdDateTimeToString(AValue, TNativeXml(FOwner).FUseLocalBias);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WriteAttributeBool(const AName: Utf8String; AValue, ADefault: boolean);
var
  S: Utf8String;
  A: TsdAttribute;
begin
  if WriteOnDefault or (AValue <> ADefault) then
  begin
    A := AttributeByName[AName];
    S := sdBoolToString(AValue);
    if assigned(A) then
      A.Value := S
    else
      AttributeAdd(AName, S);
  end;
end;

procedure TXmlNode.WritePen(const AName: Utf8String; APen: TPen);
begin
  with NodeFindOrCreate(AName) do
  begin
    WriteColor('Color', APen.Color, clBlack);
    WriteInteger('Mode', integer(APen.Mode), 0);
    WriteInteger('Style', integer(APen.Style), 0);
    WriteInteger('Width', APen.Width, 0);
  end;
end;

procedure TXmlNode.WriteBrush(const AName: Utf8String; ABrush: TBrush);
begin
  with NodeFindOrCreate(AName) do
  begin
    WriteColor('Color', ABrush.Color, clBlack);
    WriteInteger('Style', integer(ABrush.Style), 0);
  end;
end;

procedure TXmlNode.WriteColor(const AName: UTF8String; AValue, ADefault: TColor);
begin
  if WriteOnDefault or (AValue <> ADefault) then
    WriteHex(AName, ColorToRGB(AValue), 8, 0);
end;

function TXmlNode.GetBinaryString: RawByteString;
begin
  SetLength(Result, BufferLength);
  if length(Result) > 0 then
    BufferRead(Result[1], length(Result));
end;

procedure TXmlNode.SetBinaryString(const Value: RawByteString);
begin
  if length(Value) = 0 then
  begin
    SetValue('');
    exit;
  end;
  // fill the buffer
  BufferWrite(Value[1], length(Value));
end;

function TXmlNode.GetEolStyle: TsdEolStyle;
begin
  if assigned(FOwner) then
    Result := TNativeXml(FOwner).FEolStyle
  else
    Result := cDefaultEolStyle;
end;

function TXmlNode.GetPreserveWhiteSpace: boolean;
begin
  if assigned(FOwner) then
    Result := TNativeXml(FOwner).FPreserveWhiteSpace
  else
    Result := cDefaultPreserveWhiteSpace;
end;

function TXmlNode.GetSkipNormalisation: boolean;
begin
  if assigned(FOwner) then
    Result := TNativeXml(FOwner).FSkipNormalisation
  else
    Result := False;
end;

function TXmlNode.GetXmlFormat: TsdXmlFormatType;
begin
  if assigned(FOwner) then
    Result := TNativeXml(FOwner).FXmlFormat
  else
    Result := cDefaultXmlFormat;
end;

procedure TXmlNode.DoNodeLoaded(ANode: TXmlNode);
begin
  if assigned(FOwner) then
    TNativeXml(FOwner).DoNodeLoaded(ANode);
end;

procedure TXmlNode.DoNodeNew(ANode: TXmlNode);
begin
  if assigned(FOwner) then
    TNativeXml(FOwner).DoNodeNew(ANode);
end;

function TXmlNode.GetContent: Utf8String;
var
  S: TsdStringStream;
begin
  S := TsdStringStream.Create('');
  try
    WriteContent(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

procedure TXmlNode.WriteContent(S: TStream);
begin
// functionality in descendants
end;

function TXmlNode.NodeByAttributeValue(const NodeName, AttribName,
  AttribValue: Utf8String; ShouldRecurse: boolean): TXmlNode;
// This function returns a pointer to the first subnode that has an attribute with
// name AttribName and value AttribValue.
var
  i: integer;
  Node: TXmlNode;
begin
  Result := nil;
  // Find all nodes that are potential results
  for i := 0 to NodeCount - 1 do
  begin
    Node := Nodes[i];
    if (UTF8CompareText(Node.Name, NodeName) = 0) and
        Node.HasAttribute(AttribName) and
       (UTF8CompareText(Node.AttributeValueByName[AttribName], AttribValue) = 0) then
    begin
      Result := Node;
      exit;
    end;
    // Recursive call
    if ShouldRecurse then
      Result := Node.NodeByAttributeValue(NodeName, AttribName, AttribValue, True);
    if assigned(Result) then
      exit;
  end;
end;

function TXmlNode.HasAttribute(const AName: Utf8String): boolean;
var
  i: integer;
begin
  for i := 0 to AttributeCount - 1 do
    if AttributeName[i] = AName then
    begin
      Result := True;
      exit;
    end;
  Result := False;
end;

procedure TXmlNode.Clear;
begin
// functionality in descendants
end;

procedure TXmlNode.DeleteEmptyNodes;
var
  i: integer;
  Node: TXmlNode;
begin
  for i := NodeCount - 1 downto 0 do
  begin
    Node := Nodes[i];
    // Recursive call
    Node.DeleteEmptyNodes;
    // Check if we should delete child node
    if Node.IsEmpty then
      NodeDelete(i);
  end;
end;

procedure TXmlNode.Assign(Source: TPersistent);
begin
  if Source is TXmlNode then
  begin
    CopyFrom(TXmlNode(Source));
  end else
    inherited;
end;

function TXmlNode.WriteToString: Utf8String;
var
  SS: TsdStringStream;
begin
  SS := TsdStringStream.Create('');
  WriteStream(SS);
  Result := SS.DataString;
end;

procedure TXmlNode.CopyFrom(ANode: TXmlNode);
begin
  Clear;
// other functionality is in descendants
end;

function TXmlNode.FindNode(const NodeName: Utf8String): TXmlNode;
// Find the first node which has name NodeName. Contrary to the NodeByName
// function, this function will search the whole subnode tree, using the
// DepthFirst method.
var
  i: integer;
begin
  Result := nil;
  // Loop through all subnodes
  for i := 0 to NodeCount - 1 do
  begin
    Result := Nodes[i];
    // If the subnode has name NodeName then we have a result, exit
    if Result.CompareNodeName(NodeName) = 0 then
      exit;
    // If not, we will search the subtree of this node
    Result := Result.FindNode(NodeName);
    if assigned(Result) then
      exit;
  end;
end;

procedure TXmlNode.FindNodes(const NodeName: Utf8String; const AList: TList);
  // local
  procedure FindNodesRecursive(ANode: TXmlNode; AList: TList);
  var
    i: integer;
    SubNode: TXmlNode;
  begin
    for i := 0 to ANode.NodeCount - 1 do
    begin
      SubNode := ANode.Nodes[i];
      if SubNode.CompareNodeName(NodeName) = 0 then
        AList.Add(SubNode);
      FindNodesRecursive(SubNode, AList);
    end;
  end;
// main
begin
  AList.Clear;
  FindNodesRecursive(Self, AList);
end;

function TXmlNode.CompareNodeName(const NodeName: Utf8String): integer;
begin
  // Compare with FullPath or local name based on NodeName's first character
  if length(NodeName) > 0 then
  begin
    if NodeName[1] = '/' then
    begin
      // FullPath
      Result := Utf8CompareText(FullPath, NodeName);
      exit;
    end;
  end;
  // local name
  Result := Utf8CompareText(Name, NodeName);
end;

function TXmlNode.GetFullPath: Utf8String;
// GetFullpath will return the complete path of the node from the root, e.g.
// /Root/SubNode1/SubNode2/ThisNode
begin
  Result := '/' + Name;
  if Treedepth > 0 then
    // Recursive call
    Result := Parent.GetFullPath + Result;
end;

procedure TXmlNode.Delete;
begin
  if assigned(Parent) then
    Parent.NodeRemove(Self);
end;

function TXmlNode.GetDirectNodeCount: integer;
begin
// functionality in descendants
  Result := 0;
end;

function TXmlNode.GetContainerCount: integer;
begin
// functionality in descendants
  Result := 0;
end;

function TXmlNode.GetContainers(Index: integer): TXmlNode;
begin
// functionality in descendants
  Result := nil;
end;

function TXmlNode.GetDocument: TNativeXml;
begin
  if FOwner is TNativeXml then
    Result := TNativeXml(FOwner)
  else
    Result := nil;
end;

procedure TXmlNode.SetAttributeValueByNameWide(const AName: Utf8String; const Value: UnicodeString);
begin
  AttributeValueByName[AName] := sdWidetoUTF8(Value);
end;

function TXmlNode.GetAttributeValueByNameWide(const AName: Utf8String): UnicodeString;
begin
  Result := sdUtf8ToWide(AttributeValueByName[AName]);
end;

function TXmlNode.IndexInParent: integer;
// Retrieve our index in the parent's nodelist
begin
  Result := -1;
   if assigned(Parent) then
    Result := Parent.NodeIndexOf(Self);
end;

function TXmlNode.NodeByAttributeValue(const NodeName, AttribName: Utf8String; const AttribValue: UnicodeString;
  ShouldRecurse: boolean): TXmlNode;
begin
  Result := NodeByAttributeValue(NodeName, AttribName, sdWideToUtf8(AttribValue), ShouldRecurse);
end;

procedure TXmlNode.SortChildNodes(Compare: TXmlNodeCompareFunction);
// Sort the child nodes using the quicksort algorithm
  //local
  function DoNodeCompare(Node1, Node2: TXmlNode): integer;
  begin
    if assigned(Compare) then
      Result := Compare(Node1, Node2)
    else
      Result := Utf8CompareText(Node1.Name, Node2.Name);
  end;
  // local
  procedure QuickSort(iLo, iHi: Integer);
  var
    Lo, Hi, Mid: longint;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid:= (Lo + Hi) div 2;
    repeat
      while DoNodeCompare(Nodes[Lo], Nodes[Mid]) < 0 do
        Inc(Lo);
      while DoNodeCompare(Nodes[Hi], Nodes[Mid]) > 0 do
        Dec(Hi);
      if Lo <= Hi then
      begin
        // Swap pointers;
        NodeExchange(Lo, Hi);
        if Mid = Lo then
          Mid := Hi
        else
          if Mid = Hi then
            Mid := Lo;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then
      QuickSort(iLo, Hi);
    if Lo < iHi then
      QuickSort(Lo, iHi);
  end;
// main
begin
  if NodeCount > 1 then
    QuickSort(0, NodeCount - 1);
end;

procedure TXmlNode.AttributeDelete(Index: integer);
var
  Attribute: TsdAttribute;
begin
  Attribute := Attributes[Index];
  if assigned(Attribute) then
    NodeRemove(Attribute);
end;

procedure TXmlNode.AttributesClear;
begin
  while AttributeCount > 0 do
  begin
    AttributeDelete(0);
  end;
end;

{ TsdCharData }

destructor TsdCharData.Destroy;
begin
  FCoreValueID := 0;
  inherited;
end;

function TXmlNode.NextSibling(ANode: TXmlNode): TXmlNode;
begin
  // default is nil, iterating only starts from TsdContainerNode
  Result := nil;
end;


function TsdCharData.ElementType: TsdElementType;
begin
  Result := xeCharData;
end;

function TsdCharData.GetName: Utf8String;
begin
  Result := ElementTypeName;
end;

function TsdCharData.GetCoreValue: Utf8String;
begin
  Result := GetString(FCoreValueID);
end;

function TsdCharData.GetValue: Utf8String;
begin
  Result := sdReplaceString(GetCoreValue)
end;

function TsdCharData.GetValueUsingReferences(Nodes: array of TXmlNode): Utf8String;
var
  HasNonStandardReferences: boolean;
begin
  Result := sdReplaceString(GetCoreValue, HasNonStandardReferences, Nodes);
end;

procedure TsdCharData.SetCoreValue(const Value: Utf8String);
begin
  FCoreValueID := AddString(Value);
end;

procedure TsdCharData.SetValue(const Value: Utf8String);
begin
  SetCoreValue(sdEscapeString(Value))
end;

procedure TsdCharData.WriteStream(S: TStream);
begin
  sdWriteToStream(S, GetCoreValue);
end;

procedure TsdCharData.SetName(const Value: Utf8String);
begin
  // since the API is general with LINQ style, we allow a setter but in the
  // XML the chardata name will not be present
  if Length(Value) > 0 then
    DoDebugOut(Self, wsHint, sCannotSetName);
end;

procedure TsdCharData.CopyFrom(ANode: TXmlNode);
begin
  inherited;
  SetCoreValue(TsdCharData(ANode).GetCoreValue);
end;

function TsdCharData.HasNonStandardReferences: boolean;
var
  Res: boolean;
begin
  sdReplaceString(GetCoreValue, Res);
  Result := Res;
end;

{ TsdWhitespace }

function TsdWhitespace.ElementType: TsdElementType;
begin
  Result := xeWhiteSpace;
end;

{ TsdAttribute }

procedure TsdAttribute.CopyFrom(ANode: TXmlNode);
begin
  inherited;
  // copy depending data
  FCoreValue.CopyFrom(TsdAttribute(ANode).FCoreValue);
  // copy other data
  SetName(TsdAttribute(ANode).GetName);
end;

constructor TsdAttribute.Create(AOwner: TNativeXml);
begin
  inherited Create(AOwner);
  FCoreValue := TsdQuotedText.Create(AOwner);
  FCoreValue.FParent := Self;
end;

destructor TsdAttribute.Destroy;
begin
  FNameID := 0;
  FreeAndNil(FCoreValue);
  inherited;
end;

function TsdAttribute.ElementType: TsdElementType;
begin
  Result := xeAttribute;
end;

function TsdAttribute.GetName: Utf8String;
begin
  Result := GetString(FNameID);
end;

function TsdAttribute.GetValue: Utf8String;
begin
  if assigned(FCoreValue) then
    Result := sdReplaceString(FCoreValue.GetCoreValue)
  else
    Result := '';
end;

function TsdAttribute.ParseStream(Parser: TsdXmlParser): TXmlNode;
var
  IsTrimmed: boolean;
begin
  Result := Self;
  FSourcePos := Parser.Position;
  // Get the attribute name
  FNameID := AddString(sdTrim(Parser.ReadStringUntilChar('='), IsTrimmed));
  if assigned(FCoreValue) then
    // value
    FCoreValue.ParseStream(Parser);
end;

procedure TsdAttribute.SetName(const Value: Utf8String);
begin
  FNameID := AddString(Value);
end;

procedure TsdAttribute.SetValue(const Value: Utf8String);
begin
  if assigned(FCoreValue) then
    FCoreValue.SetCoreValue(sdEscapeString(Value));
end;

procedure TsdAttribute.WriteStream(S: TStream);
begin
  sdWriteToStream(S, GetName + '=');
  // now add the quoted value
  if assigned(FCoreValue) then
    FCoreValue.WriteStream(S);
end;

{ TsdQuotedText }

procedure TsdQuotedText.CopyFrom(ANode: TXmlNode);
begin
  inherited;
  FQuoteChar := TsdQuotedText(ANode).FQuoteChar;
end;

constructor TsdQuotedText.Create(AOwner: TNativeXml);
begin
  inherited Create(AOwner);
  FQuoteChar := '"';
end;

function TsdQuotedText.ElementType: TsdElementType;
begin
  Result := xeQuotedText;
end;

function TsdQuotedText.GetName: Utf8String;
begin
  Result := ElementTypeName;
end;

function TsdQuotedText.ParseStream(Parser: TsdXmlParser): TXmlNode;
var
  Blanks: Utf8String;
begin
  Result := Self;
  // Get the quoted value
  FQuoteChar := Parser.NextCharSkipBlanks(Blanks);
  if not (FQuoteChar in cXmlQuoteChars) then
  begin
    DoDebugOut(Self, wsWarn, Format(sQuoteCharExpected, [Parser.Position]));
    exit;
  end;
  FCoreValueID := AddString(Parser.ReadQuotedString(FQuoteChar));
end;

procedure TsdQuotedText.WriteStream(S: TStream);
begin
  sdWriteToStream(S, FQuoteChar + GetCoreValue + FQuoteChar);
  DoProgress(S.Position);
end;

{ TsdContainerNode }

constructor TsdContainerNode.Create(AOwner: TNativeXml);
begin
  inherited Create(AOwner);
  FNodes := TsdNodeList.Create(True);
  FDirectNodeCount := 0;
  FValueIndex := -1;
end;

destructor TsdContainerNode.Destroy;
begin
  FreeAndNil(FNodes);
  inherited;
end;

function TsdContainerNode.FirstNodeByType(AType: TsdElementType): TXmlNode;
begin
  Result := FNodes.ByType(AType);
end;

function TsdContainerNode.GetNodes(Index: integer): TXmlNode;
begin
  if (Index >= 0) and (Index < FNodes.Count) then
    Result := FNodes[Index]
  else
    Result := nil;
end;

function TsdContainerNode.HasSubContainers: boolean;
var
  i: integer;
begin
  // determine if there is at least one subcontainer
  Result := False;
  for i := FDirectNodeCount to FNodes.Count - 1 do
  begin
    if FNodes[i] is TsdContainerNode then
    begin
      Result := True;
      break;
    end;
  end;
end;

function TsdContainerNode.NodeAdd(ANode: TXmlNode): integer;
begin
  Result := -1;
  if not assigned(ANode) then
    exit;
  // attributes and whitespace are handled separately because NodeAdd may be called with attributes
  // after elements in client apps (even tho this is not best practice)
  if (ANode is TsdAttribute) or (ANode is TsdWhiteSpace) then
  begin
    // attributes inserted at FDirectNodeCount (and this value incremented)
    FNodes.Insert(FDirectNodeCount, ANode);
    Result := FDirectNodeCount;
    inc(FDirectNodeCount);
  end else
  begin
    // other subnodes like elements and CharData: add at the end of the list
    Result := FNodes.Add(ANode);
  end;
  ANode.FParent := Self;
end;

function TsdContainerNode.GetNodeCount: integer;
begin
  Result := FNodes.Count
end;

procedure TsdContainerNode.NodeDelete(Index: integer);
begin
  FNodes.Delete(Index);
end;

procedure TsdContainerNode.NodeExchange(Index1, Index2: integer);
begin
  FNodes.Exchange(Index1, Index2);
end;

function TsdContainerNode.NodeExtract(ANode: TXmlNode): TXmlNode;
begin
  Result := TXmlNode(FNodes.Extract(ANode));
end;

function TsdContainerNode.NodeIndexOf(ANode: TXmlNode): integer;
begin
  Result := FNodes.IndexOf(ANode);
end;

procedure TsdContainerNode.NodesClear;
var
  i: integer;
begin
  for i := NodeCount - 1 downto 0 do
  begin
    NodeDelete(i);
  end;
  FDirectNodeCount := 0;
  FValueIndex := -1;
end;

procedure TsdContainerNode.NodeInsert(Index: integer; ANode: TXmlNode);
begin
  FNodes.Insert(Index, ANode);
  ANode.FParent := Self;
end;

function TsdContainerNode.ParseAttributeList(Parser: TsdXmlParser): AnsiChar;
var
  Blanks: Utf8String;
  AttributeNode: TsdAttribute;
  WhiteSpaceNode: TsdWhiteSpace;
begin
  repeat
    Result := Parser.NextCharSkipBlanks(Blanks);
    if Length(Blanks) > 0 then
    begin
      if Blanks <> ' ' then
      begin
        DoDebugOut(Self, wsHint, Format(sNonDefaultChardata, [Parser.LineNumber, Parser.Position]));
        // add non-default blank chardata
        if GetPreserveWhiteSpace then
        begin
          WhiteSpaceNode := TsdWhiteSpace.Create(TNativeXml(FOwner));
          NodeAdd(WhiteSpaceNode);
          WhiteSpaceNode.SetValue(Blanks);
        end;
      end
    end;

    // Are any of the characters determining the end?
    if Result in ['!', '/', '>' ,'?'] then
      exit;

    Parser.MoveBack;
    AttributeNode := TsdAttribute.Create(TNativeXml(FOwner));
    NodeAdd(AttributeNode);
    DoNodeNew(AttributeNode);
    AttributeNode.ParseStream(Parser);
    DoNodeLoaded(AttributeNode);
  until Parser.EndOfStream;
end;

function TsdContainerNode.ParseQuotedTextList(Parser: TsdXmlParser): AnsiChar;
var
  Blanks: Utf8String;
  QuotedTextNode: TsdQuotedText;
begin
  repeat
    Result := Parser.NextCharSkipBlanks(Blanks);
    if (Length(Blanks) > 0) and (Blanks <> ' ') then
    begin
      DoDebugOut(Self, wsHint, Format(sNonDefaultChardata, [Parser.Position]));
    end;

    // Are any of the characters determining the end?
    if Result in ['!', '/', '>' ,'?'] then
      exit;

    Parser.MoveBack;
    QuotedTextNode := TsdQuotedText.Create(TNativeXml(FOwner));
    QuotedTextNode.ParseStream(Parser);
    NodeAdd(QuotedTextNode);
    DoNodeNew(QuotedTextNode);
    DoNodeLoaded(QuotedTextNode);
  until Parser.EndOfStream;
end;

procedure TsdContainerNode.WriteAttributeList(S: TStream; Count: integer);
var
  i: integer;
  PrevSubNode, ThisSubNode: TXmlNode;
begin
  PrevSubNode := nil;
  for i := 0 to Count - 1 do
  begin
    ThisSubNode := FNodes[i];
    // write attributes and intermingled chardata
    if ThisSubNode is TsdAttribute then
    begin
      if not (PrevSubNode is TsdCharData) then
        // write blank if there is no previous chardata
        sdWriteToStream(S, ' ');
      // write attribute
      ThisSubNode.WriteStream(S);
    end;
    if ThisSubNode is TsdCharData then
      // write chardata
      ThisSubNode.WriteStream(S);

    // next iteration
    PrevSubNode := ThisSubNode;
  end;
end;

procedure TsdContainerNode.Clear;
begin
  inherited;
  FNodes.Clear;
  FDirectNodeCount := 0;
  FValueIndex := -1;
end;

procedure TsdContainerNode.CopyFrom(ANode: TXmlNode);
var
  i: integer;
  ThisSubNode, ThatSubNode: TXmlNode;
  NodeClass: TsdNodeClass;
begin
  inherited;
  // copy nodes
  for i := 0 to TsdContainerNode(ANode).FNodes.Count - 1 do
  begin
    ThatSubNode := TsdContainerNode(ANode).FNodes[i];
    NodeClass := TsdNodeClass(ThatSubNode.ClassType);
    ThisSubNode := NodeClass.Create(TNativeXml(FOwner));
    FNodes.Add(ThisSubNode);
    ThisSubNode.FParent := Self;
    ThisSubNode.CopyFrom(ThatSubNode);
  end;
  // copy other data
  FDirectNodeCount := TsdContainerNode(ANode).FDirectNodeCount;
  FValueIndex := TsdContainerNode(ANode).FValueIndex;
end;

function TsdContainerNode.GetContainers(Index: integer): TXmlNode;
var
  i, Idx: integer;
begin
  Result := nil;
  Idx := 0;
  for i := FDirectNodeCount to FNodes.Count - 1 do
  begin
    if FNodes[i] is TsdContainerNode then
    begin
      if Idx = Index then
      begin
        Result := TsdContainerNode(FNodes[i]);
        exit;
      end;
      inc(Idx);
    end;
  end;
end;

function TsdContainerNode.GetContainerCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := FDirectNodeCount to FNodes.Count - 1 do
  begin
    if FNodes[i] is TsdContainerNode then
      inc(Result);
  end;
end;

function TsdContainerNode.GetDirectNodeCount: integer;
begin
  Result := FDirectNodeCount;
end;

{ TsdElement }

procedure TsdElement.CopyFrom(ANode: TXmlNode);
begin
  inherited;
  // copy other data
  SetName(TsdElement(ANode).GetName);
  FNodeClosingStyle :=  TsdElement(ANode).FNodeClosingStyle;
end;

function TsdElement.ElementType: TsdElementType;
begin
  Result := xeElement;
end;

function TsdElement.GetName: Utf8String;
begin
  Result := GetString(FNameID);
end;

function TsdElement.GetNodeClosingStyle: TsdNodeClosingStyle;
begin
  Result := TNativeXml(FOwner).NodeClosingStyle;
  if Result = ncDefault then
    Result := FNodeClosingStyle;
end;

function TsdElement.GetValue: Utf8String;
var
  IsTrimmed: boolean;
begin
  // Return the value of the CharData subnode designated by the parser
  if (FValueIndex >= 0) and (FValueIndex < FNodes.Count) then
  begin
    // chardata value at FValueIndex
    Result := FNodes[FValueIndex].Value;

    // Preserve whitespace?
    if not GetPreserveWhiteSpace then
      // do trimming
      Result := sdTrim(Result, IsTrimmed);

    // the SkipNormalisation option allows faster retrieval but is not compat with
    // the xml spec.
    if not GetSkipNormalisation then
      // do un-normalisation
      Result := sdUnNormaliseEol(Result, GetEolStyle);

  end else
    // default value
    Result := '';
end;

function TsdElement.ParseElementList(Parser: TsdXmlParser; const SupportedTags: TsdElementTypes): TXmlNode;
// parse the element list, the result (endnode) should be this element
var
  B: AnsiChar;
  BeginTagName, EndTagName: Utf8String;
  Tag: TsdElementType;
  NodeClass: TsdNodeClass;
  SubNode, EndNode: TXmlNode;
  Depth: integer;
  EndNodeName: Utf8String;
  DeeperNodeName: Utf8String;
  IsTrimmed: boolean;
begin
  Result := nil;
  repeat
    // Process char data
    ParseIntermediateData(Parser);

    // Process subtags and end tag
    if Parser.EndOfStream then
    begin
      DoDebugOut(Self, wsFail, Format(sPrematureEnd, [Parser.Position]));
      exit;
    end;
    Parser.MoveBack;

    B := Parser.NextChar;
    if B = '<' then
    begin

      // Determine tag type
      Tag := Parser.ReadOpenTag;
      if not (Tag in SupportedTags) then
      begin
        DoDebugOut(Self, wsWarn, Format(sIllegalTag, [cElementTypeNames[Tag], Parser.Position]));
        exit;
      end;

      // End tag?
      if Tag = xeEndTag then
      begin
        // up front this is the end tag so the result is this node
        Result := Self;

        // Read end tag
        EndTagName := sdTrim(Parser.ReadStringUntilChar('>'), IsTrimmed);
        NodeClosingStyle := ncFull;

        // Check if begin and end tags match
        if GetName <> EndTagName then
        begin
          BeginTagName := GetName;

          // usually a user error with omitted direct end tag
          DoDebugOut(Self, wsWarn, Format(sBeginEndMismatch,
            [GetName, EndTagName, Parser.LineNumber, Parser.Position]));

          if not TNativeXml(FOwner).FFixStructuralErrors then
            exit;

          // try to fix endtag mismatch:
          // check if there is a parent node with this name that is already parsed
          Depth := 0;
          repeat
            if assigned(FParent) then
              DeeperNodeName := FParent.Name
            else
              DeeperNodeName := '';

            if DeeperNodeName = EndTagName then
            begin
              // this is the parent's node name, so we must defer execution to the parent
              DoDebugOut(Self, wsHint,
                Format('parent%d = "%s", this endtag = "%s": maybe "%s" should be closed',
                [Depth, DeeperNodeName, EndTagName, GetName]));

              // we now break
              break;
            end;

            // move the node to a lower hierarchy
            if assigned(FParent) and assigned(FParent.Parent) then
            begin
              DoDebugOut(Self, wsInfo,
                Format('moving node "%s" from parent "%s" to grandparent "%s"',
                  [Name, FParent.Name, FParent.Parent.Name]));
              FParent.NodeExtract(Self);
              FParent.Parent.NodeAdd(Self);
            end;

            inc(Depth);

          until Length(DeeperNodeName) = 0;

          // signal that this parser hierarchy is no longer valid
          Result := FParent;

        end;

        // We're done reading this element, so we will set the capacity of the
        // nodelist to just the amount of items to avoid having overhead.
        FNodes.SetCapacity(FNodes.Count);
        exit;
      end;

      // Determine node class
      NodeClass := cNodeClass[Tag];
      if not assigned(NodeClass) then
        raise Exception.CreateFmt(sUnsupportedTag, [Parser.Position]);

      // Create new node and add
      SubNode := NodeClass.Create(TNativeXml(FOwner));
      NodeAdd(SubNode);
      if Tag <> xeElement then
        DoNodeNew(SubNode);

      // The node will parse itself
      EndNode := SubNode.ParseStream(Parser);
      if EndNode <> SubNode then
      begin
        if assigned(EndNode) then
          EndNodeName := EndNode.GetName
        else
          EndNodeName := 'nil';
        DoDebugOut(Self, wsWarn, Format(sLevelMismatch,
          [SubNode.GetName, EndNodeName, Parser.LineNumber, Parser.Position]));
        Result := EndNode;
        Exit;
      end;

      // CDATA subnodes could provide the value of the element
      if SubNode is TsdCData then
      begin
        if FValueIndex < 0 then
          FValueIndex := FNodes.Count - 1;
      end;

      DoNodeLoaded(SubNode);

    end else
    begin
      // Since this virtual proc is also used for doctype parsing.. check
      // end char here
      if (B = ']') and (ElementType = xeDocType) then
        break;
    end;
  until TNativeXml(FOwner).FAbortParsing or Parser.EndOfStream;
end;

procedure TsdElement.ParseIntermediateData(Parser: TsdXmlParser);
var
  CharDataString: Utf8String;
  CharDataNode: TsdCharData;
  SourcePos: int64;
  IsTrimmed: boolean;
begin
  SourcePos := Parser.Position;

  CharDataString := Parser.ReadStringUntilChar('<');
  if not GetPreserveWhiteSpace then
    CharDataString := sdTrim(CharDataString, IsTrimmed);

  if length(CharDataString) > 0 then
  begin
    // Insert CharData node
    CharDataNode := TsdCharData.Create(TNativeXml(FOwner));
    CharDataNode.FSourcePos := SourcePos;
    CharDataNode.FCoreValueID := AddString(CharDataString);
    NodeAdd(CharDataNode);

    // if there was no chardata node yet before, this is the value idx
    if FValueIndex = -1 then
    begin
      FValueIndex := FNodes.Count - 1;
    end;

    DoNodeNew(CharDataNode);
    DoNodeLoaded(CharDataNode);
  end;
end;

function TsdElement.ParseStream(Parser: TsdXmlParser): TXmlNode;
var
  Ch: AnsiChar;
  AName: Utf8String;
  IsTrimmed: boolean;
begin
  Result := Self;

  // Flush the reader.
  Parser.Flush;

  // the index of the chardata subnode that will hold the value, initially -1
  FValueIndex := -1;

  FSourcePos := Parser.Position;

  // Parse name
  AName := sdTrim(Parser.ReadStringUntilBlankOrEndTag, IsTrimmed);
  SetName(AName);

  DoNodeNew(Self);

  // Parse attribute list
  Ch := ParseAttributeList(Parser);

  // up till now attributes and optional chardata are direct nodes
  FDirectNodeCount := FNodes.Count;

  if Ch = '/' then
  begin
    // Direct tag
    Ch := Parser.NextChar;
    if Ch <> '>' then
    begin
      DoDebugOut(Self, wsWarn, Format(sIllegalEndTag, [Ch, Parser.LineNumber, Parser.Position]));
      exit;
    end;
    NodeClosingStyle := ncClose;
  end else
  begin
    if Ch <> '>' then
    begin
      DoDebugOut(Self, wsWarn, Format(sIllegalEndTag, [Ch, Parser.LineNumber, Parser.Position]));
      exit;
    end;

    // parse subelements
    Result := ParseElementList(Parser, [xeElement..xeCData, xeInstruction..xeEndTag]);
  end;

  // progress for elements
  DoProgress(Parser.Position);
end;

procedure TsdElement.SetName(const Value: Utf8String);
begin
  FNameID := AddString(Value);
end;

procedure TsdElement.SetNodeClosingStyle(const Value: TsdNodeClosingStyle);
begin
  FNodeClosingStyle := Value;
end;

procedure TsdElement.SetValue(const Value: Utf8String);
var
  Res: Utf8String;
  Node: TXmlNode;
begin
  if Length(Value) > 0 then
  begin
    // value that will be set
    Res := Value;

    // normalise if not skipping normalisation
    if not GetSkipNormalisation then
      Res := sdNormaliseEol(Res);

    // add or update a value
    if FValueIndex < 0 then
    begin

      // we do not have a value node, so we will add it after
      // FDirectNodeCount
      Node := TsdCharData.Create(TNativeXml(FOwner));
      Node.Value := Res;
      NodeInsert(FDirectNodeCount, Node);
      FValueIndex := FDirectNodeCount;

    end else
    begin

      // just update the value
      FNodes[FValueIndex].Value := Res;

    end;
  end else
  begin
    // remove the value
    if FValueIndex >= 0 then
    begin
      NodeDelete(FValueIndex);
      FValueIndex := -1;
    end;
  end;
end;

procedure TsdElement.WriteStream(S: TStream);
var
  i: integer;
  SubNode: TXmlNode;
  HasSubElements: boolean;
begin
  // determine if there is at least one subelement
  HasSubElements := HasSubContainers;

  // write element
  sdStreamWriteString(S, GetIndent + '<' + GetName);

  // write attributes
  WriteAttributeList(S, FDirectNodeCount);

  if (FNodes.Count = FDirectNodeCount) and (NodeClosingStyle = ncClose) then
  begin

    // directly write close tag
    sdStreamWriteString(S, TNativeXml(FOwner).FDirectCloseTag);
    sdStreamWriteString(S, GetEndOfLine);

  end else
  begin
    // indirect node
    sdStreamWriteString(S, '>');

    // write sub-nodes
    for i := FDirectNodeCount to FNodes.Count - 1 do
    begin
      SubNode := FNodes[i];

      // due to optional chardatas after the parent we use these ifs
      if (i = FDirectNodeCount) and not (SubNode is TsdCharData) then
      begin
        sdStreamWriteString(S, GetEndOfLine);
      end;
      if (i > FDirectNodeCount) and (SubNode is TsdCharData) and HasSubElements then
      begin
        sdStreamWriteString(S, SubNode.GetIndent);
      end;

      if (SubNode is TsdElement) or (SubNode is TsdCharData) then
        SubNode.WriteStream(S);

      if HasSubElements and (SubNode is TsdCharData) then
        sdStreamWriteString(S, GetEndOfLine);
    end;

    // endtag
    if HasSubElements then
      sdStreamWriteString(S, GetIndent);

    sdStreamWriteString(S, '</' + GetName + '>' + GetEndOfLine);
  end;

  DoProgress(S.Position);
end;

{ TsdDeclaration }

function TsdDeclaration.ElementType: TsdElementType;
begin
  Result := xeDeclaration;
end;

function TsdDeclaration.GetEncoding: Utf8String;
begin
  Result := AttributeValueByName['encoding'];
end;

function TsdDeclaration.GetName: Utf8String;
begin
  Result := 'xml';
end;

function TsdDeclaration.GetVersion: Utf8String;
begin
  Result := AttributeValueByName['version'];
end;

function TsdDeclaration.ParseStream(Parser: TsdXmlParser): TXmlNode;
var
  B: AnsiChar;
begin
  Result := Self;
  // Directly parse the attribute list
  B := ParseAttributeList(Parser);
  if B <> '?' then
  begin
    DoDebugOut(Self, wsWarn, Format(sIllegalEndTag, [B, Parser.LineNumber, Parser.Position]));
    exit;
  end;
  B := Parser.NextChar;
  if B <> '>' then
  begin
    DoDebugOut(Self, wsWarn, Format(sIllegalEndTag, [B, Parser.LineNumber, Parser.Position]));
    exit;
  end;
end;

procedure TsdDeclaration.SetEncoding(const Value: Utf8String);
begin
  AttributeValueByName['encoding'] := Value;
end;

procedure TsdDeclaration.SetVersion(const Value: Utf8String);
begin
  AttributeValueByName['version'] := Value;
end;

procedure TsdDeclaration.WriteStream(S: TStream);
begin
  // XML declaration <?xml{declaration}?>
  sdWriteToStream(S, GetIndent + '<?xml');
  WriteAttributeList(S, GetNodeCount);
  sdWriteToStream(S, '?>' + GetEndOfLine);
  DoProgress(S.Position);
end;

{ TsdComment }

function TsdComment.ElementType: TsdElementType;
begin
  Result := xeComment;
end;

function TsdComment.GetName: Utf8String;
begin
  Result := ElementTypeName;
end;

function TsdComment.ParseStream(Parser: TsdXmlParser): TXmlNode;
begin
  Result := Self;
  FCoreValueID := AddString(Parser.ReadStringUntil('-->'));
end;

procedure TsdComment.WriteStream(S: TStream);
begin
  // Comment <!--{comment}-->
  sdWriteToStream(S,  '<!--' + GetCoreValue + '-->');
  DoProgress(S.Position);
end;

{ TsdCData }

function TsdCData.ElementType: TsdElementType;
begin
  Result := xeCData;
end;

function TsdCData.GetName: Utf8String;
begin
  Result := ElementTypeName;
end;

function TsdCData.GetValue: Utf8String;
begin
  Result := GetString(FCoreValueID);
end;

function TsdCData.ParseStream(Parser: TsdXmlParser): TXmlNode;
begin
  Result := Self;
  // assumes that the "<![CDATA[" is aleady parsed
  FCoreValueID := AddString(Parser.ReadStringUntil(']]>'));
end;

procedure TsdCData.SetValue(const Value: Utf8String);
begin
  FCoreValueID := AddString(Value);
end;

procedure TsdCData.WriteStream(S: TStream);
begin
  // literal data <![CDATA[{data}]]>
  sdWriteToStream(S, '<![CDATA[' + GetCoreValue + ']]>');
  DoProgress(S.Position);
end;

{ TsdDocType }

procedure TsdDocType.CopyFrom(ANode: TXmlNode);
begin
  inherited;
  // copy depending data
  FExternalID.CopyFrom(TsdDocType(ANode).FExternalID);
  FSystemLiteral.CopyFrom(TsdDocType(ANode).FSystemLiteral);
  FPubIDLiteral.CopyFrom(TsdDocType(ANode).FPubIDLiteral);
  // copy other data
end;

constructor TsdDocType.Create(AOwner: TNativeXml);
begin
  inherited;
  FExternalID := TsdCharData.Create(AOwner);
  FSystemLiteral := TsdQuotedText.Create(AOwner);
  FPubIDLiteral := TsdQuotedText.Create(AOwner);
end;

destructor TsdDocType.Destroy;
begin
  FreeAndNil(FExternalID);
  FreeAndNil(FSystemLiteral);
  FreeAndNil(FPubIDLiteral);
  inherited;
end;

function TsdDocType.ElementType: TsdElementType;
begin
  Result := xeDocType;
end;

procedure TsdDocType.ParseIntermediateData(Parser: TsdXmlParser);
// in DTD's we do not allow chardata, but PE instead. Not implemented yet
var
  Blanks: Utf8String;
  B: AnsiChar;
begin
  repeat
    B := Parser.NextCharSkipBlanks(Blanks);
    if (Length(Blanks) > 0) and (Blanks <> ' ') then
    begin
      DoDebugOut(Self, wsHint, Format(sNonDefaultChardata, [Parser.LineNumber, Parser.Position]));
    end;
    // todo: PERef
    if not (B in [']', '<']) then
      Parser.ReadStringUntilBlankOrEndTag
    else
      break;
  until False;
end;

function TsdDocType.ParseStream(Parser: TsdXmlParser): TXmlNode;
var
  Blanks1, Blanks2, Blanks3, Blanks4: Utf8String;
  B: AnsiChar;
  IsTrimmed: boolean;
begin
  Result := Self;
  // sequence <!DOCTYPE is already parsed here
  // Parse name
  Parser.NextCharSkipBlanks(Blanks1);
  Parser.MoveBack;
  SetName(sdTrim(Parser.ReadStringUntilBlankOrEndTag, IsTrimmed));
  Parser.NextCharSkipBlanks(Blanks2);
  Parser.MoveBack;
  B := Parser.NextChar;
  if not (B in ['[', '>']) then
  begin
    Parser.MoveBack;
    // Parse external ID
    if Parser.CheckString('SYSTEM') then
    begin
      FExternalId.Value := 'SYSTEM';
      FSystemLiteral.ParseStream(Parser);
    end else
    begin
      if Parser.CheckString('PUBLIC') then
      begin
        FExternalID.Value := 'PUBLIC';
        FPubIDLiteral.ParseStream(Parser);
        FSystemLiteral.ParseStream(Parser);
      end else
      begin
        DoDebugOut(Self, wsWarn, Format(sIllegalTag, [B, Parser.Position]));
        exit;
      end;
    end;
    B := Parser.NextCharSkipBlanks(Blanks3);
  end;
  if B = '[' then
  begin
    Result := ParseElementList(Parser,
      // we allow these elements in the DTD
      [xeComment, xeDtdElement, xeDtdAttList, xeDtdEntity, xeDtdNotation, xeInstruction, xeCharData]);
    B := Parser.NextCharSkipBlanks(Blanks4);
  end;
  if B <> '>' then
  begin
    DoDebugOut(Self, wsWarn, Format(sIllegalTag, [B, Parser.Position]));
  end;
end;

procedure TsdDocType.WriteStream(S: TStream);
var
  i: integer;
  Line: Utf8String;
begin
  if FExternalID.Value = 'SYSTEM' then
  begin
    Line := GetIndent + '<!DOCTYPE ' + GetName + ' SYSTEM ';
    sdWriteToStream(S, Line);
    FSystemLiteral.WriteStream(S);
  end;
  if FExternalID.Value = 'PUBLIC' then
  begin
    Line := GetIndent + '<!DOCTYPE ' + GetName + ' PUBLIC ';
    sdWriteToStream(S, Line);
    FPubIDLiteral.WriteStream(S);
    sdWriteToStream(S, ' ');
    FSystemLiteral.WriteStream(S);
  end;
  sdWriteToStream(S, ' ' {+ GetEndOfLine});

  if GetNodeCount > 0 then
  begin
    sdWriteToStream(S, {FBlanks3 +} '[ ' + GetEndOfLine);
    for i := 0 to GetNodeCount - 1 do
    begin
      Nodes[i].WriteStream(S);
    end;
    sdWriteToStream(S, ']');
  end;
  sdWriteToStream(S, '>' + GetEndOfLine);
  DoProgress(S.Position);
end;

{ TsdDtdElement }

function TsdDtdElement.ElementType: TsdElementType;
begin
  Result := xeDtdElement;
end;

function TsdDtdElement.GetValue: Utf8String;
var
  S: TsdStringStream;
begin
  S := TsdStringStream.Create('');
  try
    WriteContent(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

function TsdDtdElement.ParseStream(Parser: TsdXmlParser): TXmlNode;
var
  Blanks1, Blanks2: Utf8string;
  Ch: AnsiChar;
  IsTrimmed: boolean;
begin
  Result := Self;
  Parser.NextCharSkipBlanks(Blanks1);
  Parser.MoveBack;
  SetName(sdTrim(Parser.ReadStringUntilBlankOrEndTag, IsTrimmed));
  Parser.NextCharSkipBlanks(Blanks2);
  Parser.MoveBack;
  // list of quotedtext
  Ch := ParseQuotedTextList(Parser);
  if Ch <> '>' then
    raise Exception.CreateFmt(sIllegalEndTag, [Ch, Parser.LineNumber, Parser.Position]);
end;

procedure TsdDtdElement.WriteContent(S: TStream);
var
  i: integer;
begin
  if GetNodeCount > 0 then
  begin
    for i := 0 to GetNodeCount - 1 do
    begin
      Nodes[i].WriteStream(S);
    end;
  end;
end;

procedure TsdDtdElement.WriteStream(S: TStream);
var
  ElementTypeString: Utf8String;
begin
  case ElementType of
  xeDtdElement:  ElementTypeString := 'ELEMENT';
  xeDtdAttList:  ElementTypeString := 'ATTLIST';
  xeDtdEntity:   ElementTypeString := 'ENTITY';
  xeDtdNotation: ElementTypeString := 'NOTATION';
  else
    raise EFilerError.Create(sUnsupportedTag);
  end; //case

  // write front matter
  sdStreamWriteString(S, '<!' + ElementTypeString + ' ' + GetName + ' ');

  // write content
  WriteContent(S);

  // write end matter
  sdStreamWriteString(S, '>' + GetEndOfLine);
  DoProgress(S.Position);
end;

{ TsdDtdAttList }

function TsdDtdAttList.ElementType: TsdElementType;
begin
  Result := xeDtdAttList;
end;

{ TsdDtdEntity }

function TsdDtdEntity.ElementType: TsdElementType;
begin
  Result := xeDtdEntity;
end;

{ TsdDtdNotation }

function TsdDtdNotation.ElementType: TsdElementType;
begin
  Result := xeDtdNotation;
end;

{ TsdInstruction }

function TsdInstruction.ElementType: TsdElementType;
begin
  Result := xeInstruction;
end;

function TsdInstruction.GetName: Utf8String;
begin
  Result := 'PI';
end;

function TsdInstruction.ParseStream(Parser: TsdXmlParser): TXmlNode;
begin
  Result := Self;
  FCoreValueID := AddString(Parser.ReadStringUntil('?>'));
end;

procedure TsdInstruction.WriteStream(S: TStream);
var
  Line: Utf8String;
begin
  // processing instruction <?{value}?>
  Line := GetIndent + '<?' + GetValue + '?>' + GetEndOfLine;
  sdWriteToStream(S, Line);
  DoProgress(S.Position);
end;

{ TsdStyleSheet }

function TsdStyleSheet.ElementType: TsdElementType;
begin
  Result := xeStyleSheet;
end;

function TsdStyleSheet.GetName: Utf8String;
begin
  Result := 'xml-stylesheet';
end;

function TsdStyleSheet.ParseStream(Parser: TsdXmlParser): TXmlNode;
begin
  Result := Self;
  SetValue(Parser.ReadStringUntil('?>'));
end;

procedure TsdStyleSheet.WriteStream(S: TStream);
var
  Line: Utf8String;
begin
  // Stylesheet <?xml-stylesheet{stylesheet}?>, deprecated but backwards compat
  Line := GetIndent + '<?' + GetName + GetValue + '?>' + GetEndOfLine;
  sdWriteToStream(S, Line);
  DoProgress(S.Position);
end;

{ TsdNodeList }

function TsdNodeList.ByType(AType: TsdElementType): TXmlNode;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ElementType = AType then
    begin
      Result := Items[i];
      exit;
    end;
  Result := nil;
end;

constructor TsdNodeList.Create(AOwnsObjects: boolean);
begin
  inherited Create(AOwnsObjects);
end;

function TsdNodeList.FindFirst: TXmlNode;
begin
  if Count = 0 then
    Result := nil
  else
    Result := Items[0];
end;

function TsdNodeList.FindNext(ANode: TXmlNode): TXmlNode;
var
  Last: TXmlNode;
begin
  Result := nil;
  if not assigned(ANode) then
    exit;

  if ANode.NodeCount > 0 then
  begin
    Result := ANode.Nodes[0];
    exit;
  end;

  while assigned(ANode) do
  begin
    Last := GetLastSiblingOf(ANode);
    if ANode = Last then
    begin
      ANode := ANode.Parent;
    end else
    begin
      Result := GetNextSiblingOf(ANode);
      exit;
    end;
  end;

end;

function TsdNodeList.GetItems(Index: integer): TXmlNode;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

function TsdNodeList.GetLastSiblingOf(ANode: TXmlNode): TXmlNode;
var
  Parent: TXmlNode;
  LastIdx: integer;
begin
  Result := nil;
  if ANode = nil then
    exit;

  Parent := ANode.Parent;
  if Parent = nil then
  begin
    LastIdx := Count - 1;
    if LastIdx >= 0 then
      Result := Items[LastIdx];
  end else
  begin
    LastIdx := Parent.NodeCount - 1;
    if LastIdx >= 0 then
      Result := Parent.Nodes[LastIdx];
  end;
end;

function TsdNodeList.GetNextSiblingOf(ANode: TXmlNode): TXmlNode;
var
  Parent: TXmlNode;
  Idx: integer;
begin
  Parent := ANode.Parent;
  if Parent = nil then
  begin
    Idx := IndexOf(ANode);
    if Idx < 0 then
      raise Exception.Create('index must be >= 0');
    Result := Items[Idx + 1];
  end else
  begin
    Idx := Parent.NodeIndexOf(ANode);
    Result := Parent.Nodes[Idx + 1];
  end;
end;


{ TNativeXml }

procedure TNativeXml.Clear;
begin
  FStringTable.Clear;
  FRootNodes.Clear;

  // Defaults
  FDirectCloseTag         := cDefaultDirectCloseTag;
  FDropCommentsOnParse    := cDefaultDropCommentsOnParse;
  FEolStyle               := cDefaultEolStyle;
  FExternalEncoding       := cDefaultExternalEncoding;
  FFloatAllowScientific   := cDefaultFloatAllowScientific;
  FFloatSignificantDigits := cDefaultFloatSignificantDigits;
  FIndentString           := cDefaultIndentString;
  FNodeClosingStyle       := cDefaultNodeClosingStyle;
  FPreserveWhiteSpace     := cDefaultPreserveWhiteSpace;
  FUseLocalBias           := cDefaultUseLocalBias;
  FWriteOnDefault         := cDefaultWriteOnDefault;
  FXmlFormat              := cDefaultXmlFormat;

end;

constructor TNativeXml.CreateEx(HasDeclaration, HasRootElement: boolean; AOwner: TComponent);
var
  Declaration: TsdDeclaration;
  Element: TsdElement;
begin
  inherited Create(AOwner);

  // FRootNodes is an owned list
  FRootNodes := TsdNodeList.Create(True);
  FStringTable := TsdSymbolTable.Create(AOwner);

  // this sets defaults
  Clear;

  // defaults that should not be cleared
  FFixStructuralErrors := cDefaultFixStructuralErrors;

  // Build default items in RootNodes
  if HasDeclaration then
  begin
    Declaration := TsdDeclaration.Create(Self);
    Declaration.Version  := cDefaultVersionString;
    Declaration.Encoding := cDefaultEncodingString;
    FRootNodes.Add(Declaration);
  end;

  // then the root element
  if HasRootElement then
  begin
    Element := TsdElement.Create(Self);
    FRootNodes.Add(Element);
  end;
end;

constructor TNativeXml.CreateName(const ARootName: Utf8String; AOwner: TComponent);
begin
  // we create a standard declaration and root element
  CreateEx(True, True, AOwner);
  Root.Name := ARootName;
end;

constructor TNativeXml.Create(AOwner: TComponent);
begin
  // simple constructor without declaration, but with a standard root element
  CreateEx(False, True, AOwner);
end;

destructor TNativeXml.Destroy;
begin
  FreeAndNil(FRootNodes);
  FreeAndNil(FStringTable);
  inherited;
end;

procedure TNativeXml.DoNodeLoaded(ANode: TXmlNode);
begin
  if assigned(FOnNodeLoaded) then
    FOnNodeLoaded(Self, ANode);
end;

procedure TNativeXml.DoNodeNew(ANode: TXmlNode);
begin
  if assigned(FOnNodeNew) then
    FOnNodeNew(Self, ANode);
end;

procedure TNativeXml.DoProgress(Position: int64);
begin
  if assigned(FOnProgress) then
    FOnProgress(Self, Position);
end;

function TNativeXml.GetCommentString: Utf8String;
// Get the first comment node, and return its value
var
  Node: TXmlNode;
begin
  Result := '';
  Node := FRootNodes.ByType(xeComment);
  if assigned(Node) then
    Result := Node.Value;
end;

function TNativeXml.GetCharset: Utf8String;
begin
  Result := '';
  if FRootNodes.Count > 0 then
    if FRootNodes[0] is TsdDeclaration then
      Result := TsdDeclaration(FRootNodes[0]).Encoding;
end;

function TNativeXml.GetParserLineNumber(Parser: TsdXmlParser): int64;
begin
  if assigned(Parser) then
    Result := Parser.LineNumber
  else
    Result := 0;
end;

function TNativeXml.GetParserPosition(Parser: TsdXmlParser): int64;
begin
  if assigned(Parser) then
    Result := Parser.Position
  else
    Result := 0;
end;

function TNativeXml.GetRoot: TsdElement;
begin
  // the first xeElement node in the root nodes
  Result := TsdElement(FRootNodes.ByType(xeElement));
end;

function TNativeXml.GetRootNodeClass: TsdNodeClass;
begin
  // default node class is TsdElement
  Result := TsdElement;
end;

function TNativeXml.GetRootNodeCount: integer;
begin
  Result := FRootNodes.Count;
end;

function TNativeXml.GetRootContainers(Index: integer): TsdContainerNode;
var
  i, Idx: integer;
begin
  Result := nil;
  Idx := 0;
  for i := 0 to FRootNodes.Count - 1 do
  begin
    if FRootNodes[i] is TsdContainerNode then
    begin
      if Idx = Index then
      begin
        Result := TsdContainerNode(FRootNodes[i]);
        exit;
      end;
      inc(Idx);
    end;
  end;
end;

function TNativeXml.GetRootContainerCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FRootNodes.Count - 1 do
  begin
    if FRootNodes[i] is TsdContainerNode then
      inc(Result);
  end;
end;

function TNativeXml.GetStyleSheet: TsdStyleSheet;
begin
  Result := TsdStyleSheet(FRootNodes.ByType(xeStylesheet));
  if not assigned(Result) then
  begin
    // Add a stylesheet node as second one if none present
    Result := TsdStyleSheet.Create(Self);
    FRootNodes.Insert(1, Result);
  end;
end;

function TNativeXml.GetVersionString: Utf8String;
begin
  Result := '';
  if FRootNodes.Count > 0 then
    if FRootNodes[0] is TsdDeclaration then
      Result := TsdDeclaration(FRootNodes[0]).Version;
end;

function TNativeXml.IsEmpty: boolean;
var
  R: TXmlNode;
begin
  R := GetRoot;
  Result := not assigned(R) or R.IsClear;
end;

function TNativeXml.LineFeed: Utf8String;
begin
  case FXmlFormat of
  xfReadable:
    Result := #13#10;
  xfCompact:
    Result := #10;
  else
    Result := #10;
  end;//case
end;

function TNativeXml.LoadFromURL(const URL: Utf8String): int64;
var
  M: TMemoryStream;
  NetHandle, UrlHandle: HINTERNET;
  Buffer: array[0..$400 - 1] of AnsiChar;
  BytesRead: cardinal;
begin
  Result := 0;

  NetHandle := InternetOpenA('nativexml', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if not assigned(NetHandle) then
  begin
    // NetHandle is not valid.
    DoDebugOut(Self, wsFail, 'Unable to initialize WinInet');
    exit;
  end;

  try
    UrlHandle := InternetOpenUrlA(NetHandle, PAnsiChar(Url), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if not assigned(UrlHandle) then
    begin
      // UrlHandle is not valid.
      DoDebugOut(Self, wsFail, format('Cannot open URL %s', [Url]));
      exit;
    end;

    M := TMemoryStream.Create;
    try
      // UrlHandle valid? Proceed with download
      FillChar(Buffer, SizeOf(Buffer), 0);
      repeat
        InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), BytesRead);
        if BytesRead > 0 then
          M.Write(Buffer, BytesRead);
      until BytesRead = 0;

      InternetCloseHandle(UrlHandle);

      // now load the stream
      M.Position := 0;
      LoadFromStream(M);
      // final size in bytes of the url stream
      Result := M.Size;

    finally
      M.Free;
    end;

  finally
    InternetCloseHandle(NetHandle);
  end;
end;

procedure TNativeXml.LoadFromFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TNativeXml.LoadFromStream(AStream: TStream);
var
  Parser: TsdXmlParser;
begin
  FStringTable.Clear;
  FRootNodes.Clear;

  Parser := TsdXmlParser.Create(AStream, cParserChunkSize);
  try
    Parser.Owner := Self;
    ParseStream(Parser);

    // copy encoding data from the parser
    FExternalEncoding := Parser.Encoding;
    FExternalCodePage := Parser.CodePage;
    FExternalBomInfo  := Parser.BomInfo;

    // final onprogress
    DoProgress(AStream.Size);
  finally
    FreeAndNil(Parser);
  end;
end;

procedure TNativeXml.MoveSubNodes(AList: TsdNodeList; FromNode,
  ToNode: TXmlNode);
var
  i: integer;
  Node: TXmlNode;
begin
  if (AList = nil) or (FromNode = nil) or (ToNode = nil) then
    exit;
  if AList.Count = 0 then
    exit;
  // move subnodes
  for i := 0 to AList.Count - 1 do
  begin
    Node := AList[i];
    if Node.Parent = FromNode then
    begin
      FromNode.NodeExtract(Node);
      ToNode.NodeAdd(Node);
    end;
  end;
end;

procedure TNativeXml.New;
var
  Declaration: TsdDeclaration;
  Element: TsdElement;
begin
  Clear;

  // Build default items in RootNodes: first the declaration
  Declaration := TsdDeclaration.Create(Self);
  Declaration.Version  := cDefaultVersionString;
  Declaration.Encoding := cDefaultEncodingString;
  FExternalEncoding := cDefaultExternalEncoding;
  FRootNodes.Add(Declaration);

  // then the root node
  Element := TsdElement.Create(Self);
  FRootNodes.Add(Element);
end;

procedure TNativeXml.ParseStream(Parser: TsdXmlParser);
var
  B: AnsiChar;
  ElementType: TsdElementType;
  NodeClass: TsdNodeClass;
  Node: TXmlNode;
  StringData: Utf8String;
  CD: TsdCharData;
  SP: int64;
  IsTrimmed: boolean;
  DeclarationEncodingString: Utf8String;
begin
  FAbortParsing := False;

  // read BOM
  Parser.ReadBOM;

  // store external bominfo for use later when writing
  FExternalBomInfo := Parser.BomInfo;

  // Read next tag
  repeat
    SP := Parser.Position;
    StringData := Parser.ReadStringUntilChar('<');
    if not FPreserveWhiteSpace then
      StringData := sdTrim(StringData, IsTrimmed);

    if length(StringData) > 0 then
    begin
      // Add chardata node
      CD := TsdCharData.Create(Self);
      CD.SourcePos := SP;
      CD.Value := StringData;
      FRootNodes.Add(CD);
      DoNodeNew(CD);
      DoNodeLoaded(CD);
    end;

    // At the end of the stream? Then stop
    if Parser.EndOfStream then
      break;
    Parser.MoveBack;

    B := Parser.NextChar;
    if B = '<' then
    begin
      // Determine tag type
      ElementType := Parser.ReadOpenTag;
      if ElementType = xeError then
      begin
        DoDebugOut(Self, wsWarn, Format(sIllegalTag, [B, Parser.Position]));
        exit;
      end;

      // Determine node class
      NodeClass := cNodeClass[ElementType];
      if not assigned(NodeClass) then
      begin
        DoDebugOut(Self, wsWarn, Format(sUnsupportedTag,
          [cElementTypeNames[ElementType], Parser.Position]));
        exit;
      end;

      // Create new node and add
      Node := NodeClass.Create(Self);
      FRootNodes.Add(Node);
      if ElementType <> xeElement then
        DoNodeNew(Node);

      // The node will parse itself
      Node.ParseStream(Parser);
      DoNodeLoaded(Node);

      // After adding nodes:
      // see if we added the declaration node
      if Node.ElementType = xeDeclaration then
      begin
        // give the parser the codepage from encoding in the declaration.
        // The .SetCodePage setter cares for the re-encoding of the chunk.
        DeclarationEncodingString := TsdDeclaration(Node).Encoding;
        Parser.Encoding := sdCharsetToStringEncoding(DeclarationEncodingString);
        Parser.CodePage := sdCharsetToCodePage(DeclarationEncodingString);

        DoDebugOut(Self, wsInfo, Format('declaration with encoding "%s" and codepage %d',
          [TsdDeclaration(Node).Encoding, Parser.CodePage]));
      end;

      // drop comments when parsing?
      if (Node.ElementType = xeComment) and FDropCommentsOnParse then
      begin
        // drop comment on parse
        DoDebugOut(Self, wsInfo, 'option DropCommentsOnParse is true, deleting comment');
        FRootNodes.Remove(Node);
      end;

    end;

    // Check if application has aborted parsing
  until FAbortParsing or Parser.EndOfStream;
end;

function TNativeXml.ParseSubstituteContentFromNode(ANode: TXmlNode; const ASubstitute: Utf8String): TXmlNode;
// this is a simple version of TNativeXml.ParseStream, in order to re-parse
// substituted chardata (e.g. from entities, see also NativeXmlC14n.pas)
var
  S: TsdStringStream;
  Parser: TsdXmlParser;
  Parent: TXmlNode;

  // local
  function ParseSubstituteStream(Parser: TsdXmlParser): TXmlNode;
  var
    B: AnsiChar;
    ElementType: TsdElementType;
    NodeClass: TsdNodeClass;
    Node: TXmlNode;
    StringData: Utf8String;
    CD: TsdCharData;
    IsTrimmed: boolean;
  begin
    FAbortParsing := False;

    // result will have the first re-parsed node
    Result := nil;

    Parser.EncodeChunk;

    // Read next tag
    repeat
      StringData := sdTrim(Parser.ReadStringUntilChar('<'), IsTrimmed);

      if length(StringData) > 0 then
      begin
        // Add chardata node
        CD := TsdCharData.Create(Self);
        CD.Value := StringData;
        Parent.NodeAdd(CD);
        if not assigned(Result) then
          Result := CD;
      end;

      // At the end of the stream? Then stop
      if Parser.EndOfStream then
        break;
      Parser.MoveBack;

      B := Parser.NextChar;
      if B = '<' then
      begin
        // Determine tag type
        ElementType := Parser.ReadOpenTag;
        if ElementType = xeError then
        begin
          DoDebugOut(Self, wsWarn, Format(sIllegalTag, [B, Parser.Position]));
          exit;
        end;

        // Determine node class
        NodeClass := cNodeClass[ElementType];
        if not assigned(NodeClass) then
        begin
          DoDebugOut(Self, wsWarn, Format(sUnsupportedTag,
            [cElementTypeNames[ElementType], Parser.Position]));
          exit;
        end;

        // Create new node and add
        Node := NodeClass.Create(Self);
        Parent.NodeAdd(Node);
        if not assigned(Result) then
          Result := Node;

        // The node will parse itself
        Node.ParseStream(Parser);
      end;

      // Check if application has aborted parsing
    until FAbortParsing or Parser.EndOfStream;
  end;

// main
begin
  Result := nil;
  Parent := ANode.Parent;
  if not assigned(Parent) then
    exit;

  // remove the node that gets substituted
  Parent.NodeRemove(ANode);

  S := TsdStringStream.Create(ASubstitute);
  try
    S.Position := 0;
    Parser := TsdXmlParser.Create(S, cParserChunkSize);
    try
      Parser.Owner := Self;
      Result := ParseSubstituteStream(Parser);
    finally
      FreeAndNil(Parser);
    end;
  finally
    S.Free;
  end;
end;

procedure TNativeXml.ReadFromString(const AValue: Utf8String);
var
  S: TStream;
begin
  S := TsdStringStream.Create(AValue);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TNativeXml.SaveToFile(const AFileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TNativeXml.SaveToStream(Stream: TStream);
var
  Writer: TsdXmlWriter;
  BomInfo: TsdBomInfo;
begin
  // Create xml writer, which enabes correct BOM, encoding and codepage.
  Writer := TsdXmlWriter.Create(Self, Stream, cWriterChunkSize);
  try

    // based on externalencoding, we create the external BOM
    case FExternalEncoding of
    seAnsi:
      begin
        BomInfo.Len := 0;
        BomInfo.Encoding := seAnsi;
        BomInfo.HasBOM := False;
      end;
    seUTF8:
      begin
        BomInfo.Len := 0;
        BomInfo.Encoding := seUTF8;
        BomInfo.HasBOM := False;
      end;
    seUTF16BE:
      begin
        // Len = 2 and HasBom = True
        BomInfo := cBomInfoList[cBomInfoIdxUTF16BE];
      end;
    seUTF16LE:
      begin
        // Len = 2 and HasBom = True
        BomInfo := cBomInfoList[cBomInfoIdxUTF16LE];
      end;
    else
      DoDebugOut(Self, wsFail, sUnsupportedEncoding);
      exit;
    end;
    FExternalBomInfo := BomInfo;

    // external byte order mark
    if FExternalBomInfo.HasBOM then
    begin
      Writer.FEncoding := seUTF8; // only to write BOM without hassle
      Writer.Write(FExternalBomInfo.BOM[0], FExternalBomInfo.Len);
    end;

    // set external encoding
    Writer.FEncoding := FExternalEncoding;
    Writer.FCodePage := FExternalCodePage;

    // write the stream
    WriteStream(Writer);
  finally
    Writer.Free;
  end;
end;

procedure TNativeXml.SetCommentString(const Value: Utf8String);
// Find first comment node and set it's value, otherwise add new comment node
// right below the xml declaration
var
  Node: TXmlNode;
begin
  Node := FRootNodes.ByType(xeComment);
  if not assigned(Node) and (length(Value) > 0) then
  begin
    Node := TsdComment.Create(Self);
    FRootNodes.Insert(1, Node);
  end;
  if assigned(Node) then
    Node.Value := Value;
end;

procedure TNativeXml.SetCharset(const Value: Utf8String);
var
  Node: TXmlNode;
begin
  if Value = GetCharset then
    exit;

  // write declaration (if not there)
  Node := FRootNodes[0];
  if not (Node is TsdDeclaration) then
  begin
    if length(Value) > 0 then
    begin
      Node := TsdDeclaration.Create(Self);
      FRootNodes.Insert(0, Node);
    end;
  end;

  // write charset
  if Node is TsdDeclaration then
    TsdDeclaration(Node).Encoding := Value;

  // write the external codepage
  FExternalCodepage := sdCharsetToCodepage(Value);
  // write external encoding
  FExternalEncoding := sdCharsetToStringEncoding(Value);
end;

procedure TNativeXml.SetPreserveWhiteSpace(Value: boolean);
begin
  FPreserveWhiteSpace := Value;
  if FPreserveWhiteSpace then
    FXmlFormat := xfPreserve;
end;

procedure TNativeXml.SetVersionString(const Value: Utf8String);
var
  Node: TXmlNode;
begin
  if Value = GetVersionString then
    exit;
  Node := FRootNodes[0];
  if not (Node is TsdDeclaration) then
  begin
    if length(Value) > 0 then
    begin
      Node := TsdDeclaration.Create(Self);
      FRootNodes.Insert(0, Node);
    end;
  end;
  if assigned(Node) then
    TsdDeclaration(Node).Version := Value;
end;

procedure TNativeXml.WriteStream(S: TStream);
var
  i: integer;
  Node: TXmlNode;
begin
  if not assigned(Root) and FParserWarnings then
    raise EFilerError.Create(sRootElementNotDefined);

  DoProgress(0);


  // write the root nodes
  for i := 0 to FRootNodes.Count - 1 do
  begin
    // external codepage info
    if i = 0 then
    begin
      Node := FRootNodes[i];
      if Node.ElementType = xeDeclaration then
        DoDebugOut(Self, wsInfo, Format('writing declaration with encoding "%s" and codepage %d',
          [TsdDeclaration(Node).Encoding, FExternalCodePage]));
    end;

    FRootNodes[i].WriteStream(S);
  end;

  DoProgress(S.Size);
end;

function TNativeXml.WriteToString: string;
var
  S: TsdStringStream;
begin
  S := TsdStringStream.Create('');
  try
    SaveToStream(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

function TNativeXml.WriteToLocalString: Utf8String;
var
  S: TsdStringStream;
begin
  S := TsdStringStream.Create('');
  try
    WriteStream(S);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

function TNativeXml.WriteToLocalUnicodeString: UnicodeString;
begin
  Result := sdUtf8ToWide(WriteToLocalString);
end;

function TNativeXml.FindFirst: TXmlNode;
begin
  if not assigned(FRootNodes) or (FRootNodes.Count = 0) then
    Result := nil
  else
    Result := FRootNodes[0];
end;

function TNativeXml.FindNext(ANode: TXmlNode): TXmlNode;
  // local
  function GetLastSiblingOf(ANode: TXmlNode): TXmlNode;
  var
    Parent: TXmlNode;
    LastIdx: integer;
  begin
    Result := nil;
    if ANode = nil then
      exit;

    Parent := ANode.Parent;
    if Parent = nil then
    begin
      LastIdx := RootNodes.Count - 1;
      if LastIdx >= 0 then
        Result := FRootNodes[LastIdx];
    end else
    begin
      LastIdx := Parent.NodeCount - 1;
      if LastIdx >= 0 then
        Result := Parent.Nodes[LastIdx];
    end;
  end;

  // local
  function GetNextSiblingOf(ANode: TXmlNode): TXmlNode;
  var
    Parent: TXmlNode;
    Idx: integer;
  begin
    Parent := ANode.Parent;
    if Parent = nil then
    begin
      Idx := FRootNodes.IndexOf(ANode);
      Result := FRootNodes[Idx + 1];
    end else
    begin
      Idx := Parent.NodeIndexOf(ANode);
      Result := Parent.Nodes[Idx + 1];
    end;
  end;

// main
var
  Last: TXmlNode;
begin
  Result := nil;
  if not assigned(ANode) then
    exit;

  if ANode.NodeCount > 0 then
  begin
    Result := ANode.Nodes[0];
    exit;
  end;

  while assigned(ANode) do
  begin
    Last := GetLastSiblingOf(ANode);
    if ANode = Last then
    begin
      ANode := ANode.Parent;
    end else
    begin
      Result := GetNextSiblingOf(ANode);
      exit;
    end;
  end;

end;

function TNativeXml.InsertDocType(const AName: Utf8String): TsdDocType;
begin
  Result := TsdDocType.Create(Self);
  Result.Name := AName;
  Result.ExternalId.Value := 'SYSTEM';
  FRootNodes.Insert(1, Result);
end;


class function TNativeXml.DecodeBase64(const Source: Utf8String; OnDebug: TsdDebugEvent): RawByteString;
begin
  try
    Result := NativeXml.DecodeBase64(Source);
  except
    on EFilerError do
      OnDebug(nil, wsFail, sErrorCalcStreamLength);
  end;
end;

class function TNativeXml.EncodeBase64(const Source: RawByteString; const ControlChars: Utf8String): Utf8String;
begin
  Result := sdAddControlChars(NativeXml.EncodeBase64(Source), ControlChars);
end;

procedure TNativeXml.SetExternalEncoding(const Value: TsdStringEncoding);
var
  Codepage: integer;
begin
  if FExternalEncoding = Value then
    exit;
  Codepage := cStringEncodingCodepages[Value];
  if Codepage = 0 then
  begin
    DoDebugOut(Self, wsFail,
      Format('external encoding "%s" is not allowed (use ExternalCodepage)',
      [cStringEncodingCharsetNames[Value]]));
    exit;
  end;
  FExternalEncoding := Value;
  FExternalCodepage := Codepage;
end;

function TNativeXml.Canonicalize: integer;
begin
//to-do longterm
  Result := 0;
end;

procedure TNativeXml.ForEach(Sender: TObject; AEvent: TsdXmlNodeEvent);
var
  Node: TXmlNode;
begin
  if not assigned(AEvent) or not assigned(Sender) then
    exit;
  Node := FindFirst;
  while assigned(Node) do
  begin
    AEvent(Sender, Node);
    Node := FindNext(Node);
  end;
end;


{ TsdXmlParser }

function TsdXmlParser.CheckString(const S: Utf8String): boolean;
// case-insensitive string check
var
  i, Count, StartIdx: integer;
begin
  Count := MakeDataAvailable;
  StartIdx := FUtf8CurrentIdx;
  Result := True;
  for i := 1 to length(S) do
  begin
    if FEndOfStream then
    begin
      Result := False;
      exit;
    end;
    // case-insensitive, so we use LoCase in both sides (LoCase is
    // faster than function LowerCase, since it deals directly with chars).
    if LoCase(S[i]) <> LoCase(FUtf8Buffer[FUtf8CurrentIdx]) then
    begin
      Result := False;
      // revert
      FUtf8CurrentIdx := StartIdx;
      exit;
    end;
    IncCurrentIdxCheck(Count);
  end;
end;

constructor TsdXmlParser.Create(ASource: TStream; AChunkSize: integer);
begin
  inherited Create;
  FSource := ASource;
  FChunkSize := AChunkSize;
  SetLength(FRawBuffer, FChunkSize);

  // Read from the stream directly to the raw buffer
  FRawFirstIdx := 0;
  FRawLastIdx := FSource.Read(FRawBuffer[0], FChunkSize);
  FUtf8FirstIdx := 0;
  FUtf8CurrentIdx := 0;

  // Normalise end-of-line is enabled by default (True)
  FNormaliseEOLEnabled := True;
end;

destructor TsdXmlParser.Destroy;
begin
  SetLength(FRawBuffer, 0);
  SetLength(FUtf8Buffer, 0);
  inherited;
end;

procedure TsdXmlParser.EncodeChunk;

  // local
  procedure EncodeAnsiChunk;
  var
    RawLen, Utf8Len: integer;
  begin
    RawLen := FRawLastIdx - FRawFirstIdx;
    SetLength(FRawBuffer, FRawFirstIdx + RawLen);
    // Utf8 buffer might be 3x ansi size at max
    SetLength(FUtf8Buffer, FUtf8FirstIdx + 3 * RawLen);
    Utf8Len := sdAnsiToUtf8Buffer(FRawBuffer[FRawFirstIdx], FUtf8Buffer[FUtf8FirstIdx], FCodePage, RawLen);
    FUtf8LastIdx := FUtf8FirstIdx + Utf8Len;
  end;

  // local
  procedure EncodeUtf8Chunk;
  var
    RawLen, Utf8Len: integer;
  begin
    RawLen := FRawLastIdx - FRawFirstIdx;
    // buffers
    SetLength(FRawBuffer, FRawFirstIdx + RawLen);
    SetLength(FUtf8Buffer, FUtf8FirstIdx + RawLen);
    Move(FRawBuffer[FRawFirstIdx], FUtf8Buffer[FUtf8FirstIdx], RawLen);
    Utf8Len := RawLen;
    FUtf8LastIdx := FUtf8FirstIdx + Utf8Len;
  end;

  // local
  procedure EncodeUtf16Chunk;
  type
    TWordArray = array of word;
  var
    RawLen, Utf8Len: integer;
    i: integer;
    W: word;
  begin
    RawLen := FRawLastIdx - FRawFirstIdx;

    // If UTF16 BE (Big Endian), we must swap byte order
    if FEncoding = seUTF16BE then
    begin
      for i := FRawFirstIdx div 2 to FRawLastIdx div 2 - 1 do
      begin
        W := TWordArray(FRawBuffer)[i];
        TWordArray(FRawBuffer)[i] := Swap(W);
      end;
    end;

    // Utf8 buffer might be 2x utf16 size at max
    SetLength(FRawBuffer, FRawFirstIdx + RawLen);
    SetLength(FUtf8Buffer, FUtf8FirstIdx + (2 * RawLen));

    // Now convert from UTF16 to UTF8
    Utf8Len := sdWideToUtf8Buffer(FRawBuffer[FRawFirstIdx], FUtf8Buffer[FUtf8FirstIdx], RawLen div 2);
    FUtf8LastIdx := FUtf8FirstIdx + Utf8Len;
  end;
// main
begin
  // call EncodeChunk methods based on encoding
  case FEncoding of
  seAnsi:
    begin
      if (FCodePage = 0) or (FCodePage = 65001{CP_UTF8}) then
      begin
        EncodeUtf8Chunk;
      end else
      begin
        EncodeAnsiChunk;
      end;
    end;

  seUTF8:
    begin
      EncodeUtf8Chunk;
    end;

  seUTF16BE, seUTF16LE:
    begin
      EncodeUtf16Chunk;
    end;
  end;

  // collapse all end-of-line to a single LineFeed (#$0A)
  if FNormaliseEOLEnabled then
    NormaliseEOL;

end;

procedure TsdXmlParser.Flush(Force: boolean);
var
  i: integer;
  RawLen, Utf8Len: integer;
begin
  // Number of bytes to move
  RawLen := FRawLastIdx - FRawFirstIdx;
  Utf8Len := FUtf8LastIdx - FUtf8FirstIdx;
  if FUtf8CurrentIdx - FUtf8FirstIdx > 0 then
  begin
    // Calcuate base line number and base position
    for i := 0 to FUtf8FirstIdx - 1 do
    begin
      // linefeed
      if FUtf8Buffer[i] = #$0A then
        inc(FBaseLineNumber);
    end;
    inc(FUtf8BasePosition, FUtf8FirstIdx);
    // moves
    Move(FRawBuffer[FRawFirstIdx], FRawBuffer[0], RawLen);
    Move(FUtf8Buffer[FUtf8FirstIdx], FUtf8Buffer[0], Utf8Len);
    // update current idx
    dec(FUtf8CurrentIdx, FUtf8FirstIdx);
    // update first/last indices
    FRawFirstIdx := 0;
    FRawLastIdx := RawLen;
    FUtf8FirstIdx := 0;
    FUtf8LastIdx := Utf8Len;
  end;
end;

function TsdXmlParser.GetLineNumber: int64;
var
  i: integer;
begin
  Result := FBaseLineNumber;
  for i := 0 to FUtf8CurrentIdx - 1 do
  begin
    // linefeed
    if FUtf8Buffer[i] = #$0A then
      inc(Result);
  end;
end;

function TsdXmlParser.GetPosition: int64;
begin
  Result := FUtf8BasePosition + FUtf8CurrentIdx;
end;

procedure TsdXmlParser.IncCurrentIdxCheck(var BytesAvail: integer);
// increment FCurrentIdx and check bytes available
begin
  inc(FUtf8CurrentIdx);
  dec(BytesAvail);
  if BytesAvail <= 0 then
    BytesAvail := MakeDataAvailable
end;

function TsdXmlParser.IsBinaryXml: boolean;
var
  i: integer;
  Cookie: array[0..3] of AnsiChar;
begin
  Result := False;
  if FRawLastIdx <= length(cBinaryXmlCookie) then
    exit;

  // read binary cookie
  Move(FRawBuffer[0], Cookie, 4);

  for i := 0 to length(cBinaryXmlCookie) - 1 do
    if Cookie[i] <> cBinaryXmlCookie[i] then
      exit;

  // cookie for binary xml matches
  Result := True;
end;

function TsdXmlParser.LoCase(Ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  case Result of
    'A'..'Z':  inc(Result, Ord('a') - Ord('A'));
  end;
end;

function TsdXmlParser.MakeDataAvailable: integer;
var
  BytesRead: integer;
begin
  Result := FUtf8LastIdx - FUtf8CurrentIdx;
  while Result < 1 do
  begin
    // We must make data available
    BytesRead := ReadNextChunk;
    Result := FUtf8LastIdx - FUtf8CurrentIdx;
    // Still no data available?
    if BytesRead = 0 then
    begin
      FEndOfStream := True;
      exit;
    end;
  end;
end;

procedure TsdXmlParser.MoveBack;
begin
  assert(FUtf8CurrentIdx > 0);
  dec(FUtf8CurrentIdx);
end;

function TsdXmlParser.NextChar: AnsiChar;
begin
  MakeDataAvailable;
  if FEndOfStream then
  begin
    Result := #0;
    exit;
  end;
  Result := FUtf8Buffer[FUtf8CurrentIdx];
  inc(FUtf8CurrentIdx);
end;

procedure TsdXmlParser.NormaliseEOL;
var
  i: integer;
begin
  // collapse all end-of-line to a single LineFeed (#$0A)
  i := FUtf8FirstIdx;
  while i < FUtf8LastIdx do
  begin
    if FUtf8Buffer[i] = #$0A then
    begin
      if FUtf8Buffer[i - 1] = #$0D then
      begin
        Move(FUtf8Buffer[i], FUtf8Buffer[i - 1], FUtf8LastIdx - i);
        dec(FUtf8LastIdx);
      end;
    end;
    inc(i);
  end;
end;

function TsdXmlParser.ReadNextChunk: integer;
begin
  SetLength(FRawBuffer, FRawLastIdx + FChunkSize);

  // Read from the stream directly to our chunk
  // Result is the bytes read
  Result := FSource.Read(FRawBuffer[FRawLastIdx], FChunkSize);
  if Result > 0 then
  begin
    FRawFirstIdx := FRawLastIdx;
    FUtf8FirstIdx := FUtf8LastIdx;
    inc(FRawLastIdx, Result);
    EncodeChunk;
  end;
end;

function TsdXmlParser.ReadQuotedString(AQuote: AnsiChar): Utf8String;
begin
  // It seems that the xml spec simply does not allow double quotes as in
  // Delphi, so we do not need a complicated algo to do this. We can simply
  // search for the quote again as terminator.
  Result := ReadStringUntilChar(AQuote);
end;

function TsdXmlParser.ReadString(AIndex, ACount: integer): Utf8String;
begin
  SetLength(Result, ACount);
  if ACount > 0 then
    Move(FUtf8Buffer[AIndex], Result[1], ACount);
end;

function TsdXmlParser.ReadStringUntil(const Terminator: Utf8String): Utf8String;
var
  Count, MatchLen: integer;
  FirstChar: AnsiChar;
  StartIdx: integer;
begin
  FirstChar := Terminator[1];
  MatchLen := length(Terminator);
  StartIdx := FUtf8CurrentIdx;
  Count := MakeDataAvailable;
  while not FEndOfStream do
  begin
    if FUtf8Buffer[FUtf8CurrentIdx] = FirstChar then
    begin

      if CheckString(Terminator) then
      begin
        // We found the terminating string
        Result := ReadString(StartIdx, FUtf8CurrentIdx - StartIdx - MatchLen);
        exit;
      end;

    end;
    IncCurrentIdxCheck(Count);
  end;
  // when left here stream ended prematurely
  DoDebugOut(Self, wsWarn, Format(sPrematureEnd, [GetPosition]));
end;

function TsdXmlParser.ReadStringUntilChar(AChar: AnsiChar): Utf8String;
var
  Count: integer;
  StartIdx: integer;
begin
  Count := MakeDataAvailable;

  StartIdx := FUtf8CurrentIdx;
  while not FEndOfStream do
  begin
    if FUtf8Buffer[FUtf8CurrentIdx] = AChar then
    begin
      // We found AChar
      Result := ReadString(StartIdx, FUtf8CurrentIdx - StartIdx);
      // Adjust FUtf8CurrentIdx
      inc(FUtf8CurrentIdx);
      exit;
    end;
    IncCurrentIdxCheck(Count);
  end;

  // Arriving here: end of stream and AChar not reached
  Result := ReadString(StartIdx, FUtf8CurrentIdx - StartIdx);
end;

procedure TsdXmlParser.SetCodePage(const Value: integer);
begin
  FCodePage := Value;
  // re-encode the chunk (eg from default UTF-8 codepage to other ansi codepage)
  EncodeChunk;
end;

// TsdXmlParser 

function TsdXmlParser.NextCharSkipBlanks(var Blanks: Utf8String): AnsiChar;
var
  Count: integer;
begin
  Blanks := '';
  Count := MakeDataAvailable;
  while not FEndOfStream do
  begin
    Result := FUtf8Buffer[FUtf8CurrentIdx];
    IncCurrentIdxCheck(Count);
    if not (Result in cXmlBlankChars) then
      exit;
    Blanks := Blanks + Result;
  end;
  Result := #0;
end;

procedure TsdXmlParser.ReadBOM;
var
  i, j: integer;
  BOM: array[0..3] of byte;
  BomInfoFound: boolean;
begin
  if FRawLastIdx <= 4 then
  begin
    DoDebugOut(Self, wsWarn, Format(sPrematureEnd, [FRawLastIdx]));
    exit;
  end;

  // read the BOM if it is there
  Move(FRawBuffer[0], BOM, 4);

  i := 0;
  BomInfoFound := False;
  while i < cBomInfoListCount do
  begin
    BomInfoFound := True;
    for j := 0 to cBomInfoList[i].Len - 1 do
    begin
      if BOM[j] <> cBomInfoList[i].BOM[j] then
      begin
        BomInfoFound := False;
        break;
      end;
    end;
    if BomInfoFound then
    begin
      FBomInfo := cBomInfoList[i];
      FEncoding := FBomInfo.Encoding;
      break;
    end;
    inc(i);
  end;

  // BOM info not found?
  if BomInfoFound then
  begin
    // Non-supported encodings
    if not (FEncoding in [seAnsi, seUTF8, seUTF16BE, seUTF16LE]) then
    begin
      DoDebugOut(Self, wsFail, Format(sUnsupportedEncoding, [cStringEncodingCharsetNames[FEncoding]]));
      // avoid trying to read exotic encodings such as EBDIC
      exit;
    end;

    // Rewind based on BOM
    if FBomInfo.HasBom then
    begin
      FRawLastIdx := FChunkSize - FBomInfo.Len;
      Move(FRawBuffer[FBomInfo.Len], FRawBuffer[0], FRawLastIdx);
      SetLength(FRawBuffer, FRawLastIdx);
      DoDebugOut(Self, wsInfo, Format('BOM with encoding %s', [cStringEncodingCharsetNames[FEncoding]]));
    end;

  end else
  begin
    // No BOM, and unknown encoding, e.g. html instead of xml
    // we use UTF8 as default
    DoDebugOut(Self, wsWarn, sUnknownEncoding);
    FEncoding := seUTF8;
  end;

  // encode the first chunk
  EncodeChunk;
end;

function TsdXmlParser.ReadOpenTag: TsdElementType;
var
  AnsiCh: AnsiChar;
begin
  Result := xeError;
  AnsiCh := NextChar;
  if FEndOfStream then
    exit;

  case AnsiCh of
  '!':
    begin
      AnsiCh := LoCase(NextChar);
      case AnsiCh of
      '[': if CheckString('cdata[') then
        Result := xeCData;
      'd': if CheckString('octype') then
        Result := xeDocType;
      'e':
        begin
          if CheckString('lement') then
            Result := xeDtdElement;
          if CheckString('ntity') then
            Result := xeDtdEntity;
        end;
      'a': if CheckString('ttlist') then
        Result := xeDtdAttList;
      'n': if CheckString('otation') then
        Result := xeDtdNotation;
      '-': if CheckString('-') then
        Result := xeComment;
      else
        begin
          DoDebugOut(Self, wsFail, Format(sIllegalTag, [AnsiCh, GetPosition]));
          exit;
        end;
      end;
    end;
  '?':
    begin
      if CheckString('xml') then
      begin
        if CheckString('-stylesheet') then
          Result := xeStyleSheet
        else
          Result := xeDeclaration;
      end else
        Result := xeInstruction;
    end;
  '/': Result := xeEndTag;
  else
    Result := xeElement;
    MoveBack;
  end;
end;

function TsdXmlParser.ReadStringUntilBlankOrEndTag: Utf8String;
var
  Count: integer;
  StartIdx: integer;
begin
  Count := MakeDataAvailable;
  StartIdx := FUtf8CurrentIdx;
  while not FEndOfStream do
  begin
    if FUtf8Buffer[FUtf8CurrentIdx] in cXmlBlankCharsOrEndTag then
    begin
      // We found the termination
      Result := ReadString(StartIdx, FUtf8CurrentIdx - StartIdx);
      exit;
    end;
    IncCurrentIdxCheck(Count);
  end;
  // when left here stream ended prematurely
  DoDebugOut(Self, wsWarn, Format(sPrematureEnd, [GetPosition]));
end;

{ TsdXmlWriter }

constructor TsdXmlWriter.Create(AOwner: TDebugComponent; ASource: TStream; AChunkSize: integer);
begin
  inherited Create(ASource, AChunkSize);
  FOwner := AOwner;
end;

destructor TsdXmlWriter.Destroy;
begin
  SetLength(FRawBuffer, 0);
  inherited;
end;

procedure TsdXmlWriter.DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle;
  const AMessage: Utf8String);
begin
  if FOwner is TDebugComponent then
    TDebugComponent(FOwner).DoDebugOut(Sender, WarnStyle, AMessage);
end;

function TsdXmlWriter.Write(const Buffer; Count: Integer): Longint;
type
  PWord = ^Word;
var
  i, AnsiCount, WideCount: integer;
  DefaultCharUsed: boolean;
  W: PWord; // pointer to a word
  // local
  procedure AllocRawBuffer(ASize: integer);
  begin
    if FRawBufferSize < ASize then
    begin
      FRawBufferSize := ASize;
      SetLength(FRawBuffer, FRawBufferSize);
    end;
  end;
// main
begin
  case FEncoding of

  seAnsi:
    begin
      AllocRawBuffer(Count);
      AnsiCount := sdUtf8ToAnsiBuffer(Buffer, FRawBuffer[0], FCodepage, Count, DefaultCharUsed);
      Result := inherited Write(FRawBuffer[0], AnsiCount);
      if DefaultCharUsed then
      begin
        DoDebugOut(Self, wsWarn, sDefaultCharUsed);
      end;
    end;

  seUTF8:
    begin
      Result := inherited Write(Buffer, Count)
    end;

  seUTF16LE:
    begin
      AllocRawBuffer(2 * Count);
      WideCount := sdUtf8ToWideBuffer(Buffer, FRawBuffer[0], Count);
      Result := inherited Write(FRawBuffer[0], 2 * WideCount);
    end;

  seUTF16BE:
    begin
      AllocRawBuffer(2 * Count);
      WideCount := sdUtf8ToWideBuffer(Buffer, FRawBuffer[0], Count);
      // swap the byte order from little endian to big endian
      W := PWord(@FRawBuffer[0]);
      for i := 0 to WideCount - 1 do
      begin
        W^ := Swap(W^);
        inc(W);
      end;
      Result := inherited Write(FRawBuffer[0], 2 * WideCount);
    end;
  else
    // unsupported encoding
    DoDebugOut(Self, wsFail, sUnsupportedEncoding);
    Result := 0;
  end;
end;

{ Utility Functions }

function sdWideToUtf8(const W: UnicodeString): Utf8String;
var
  WideCount, Utf8Count: integer;
begin
  WideCount := length(W);
  SetLength(Result, WideCount * 3); // just to be sure
  if WideCount = 0 then
    exit;

  Utf8Count := sdWideToUtf8Buffer(W[1], Result[1], WideCount);
  SetLength(Result, Utf8Count);
end;

function sdUtf8ToWide(const U: Utf8String): UnicodeString;
var
  Utf8Count, WideCount: integer;
begin
  Utf8Count := length(U);
  SetLength(Result, Utf8Count);
  if Utf8Count = 0 then
    exit;

  WideCount := sdUtf8ToWideBuffer(U[1], Result[1], Utf8Count);
  SetLength(Result, WideCount);
end;

function sdWideToUtf8Buffer(const WideBuf; var Utf8Buf; WideCount: integer): integer;
// Convert an Unicode (UTF16 LE) memory block to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the block at Dst must be at least 1.5 the size of the source block.
// The function returns the number of *bytes* written.
var
  W: word;
  WideIdx, Utf8Idx: integer;
begin
  WideIdx := 0;
  Utf8Idx := 0;
  while WideIdx < WideCount do
  begin
    W := TWordArray(WideBuf)[WideIdx];
    if W <= $7F then
    begin
      TByteArray(Utf8Buf)[Utf8Idx] := byte(W);
      inc(Utf8Idx);
    end else
    begin
      if W > $7FF then
      begin
        TByteArray(Utf8Buf)[Utf8Idx] := byte($E0 or (W shr 12));
        inc(Utf8Idx);
        TByteArray(Utf8Buf)[Utf8Idx] := byte($80 or ((W shr 6) and $3F));
        inc(Utf8Idx);
        TByteArray(Utf8Buf)[Utf8Idx] := byte($80 or (W and $3F));
        inc(Utf8Idx);
      end else
      begin //  $7F < W <= $7FF
        TByteArray(Utf8Buf)[Utf8Idx] := byte($C0 or (W shr 6));
        inc(Utf8Idx);
        TByteArray(Utf8Buf)[Utf8Idx] := byte($80 or (W and $3F));
        inc(Utf8Idx);
      end;
    end;
    inc(WideIdx);
  end;
  Result := Utf8Idx;
end;

function sdUtf8ToWideBuffer(const Utf8Buf; var WideBuf; ByteCount: integer): integer;
// Convert an UTF8 buffer to Unicode (UTF16 LE) buffer. This routine will process
// Count *bytes* of UTF8 (each character 1-3 bytes) into UTF16 (each char 2 bytes).
// Therefore, the block at WideBuf must be at least 2 times the size of Count, since
// many UTF8 characters consist of just one byte, and are mapped to 2 bytes. The
// function returns the number of *wide chars* written. Note that the Utf8Buf block must
// have an exact number of UTF8 characters in it, if Count doesn't match then
// the last character will be converted anyway (going past the block boundary!)
var
  W: word;
  C: byte;
  WideIdx, Utf8Idx: integer;
begin
  Utf8Idx := 0;
  WideIdx := 0;
  while Utf8Idx < ByteCount do
  begin
    // 1st byte
    W := TByteArray(Utf8Buf)[Utf8Idx];
    inc(Utf8Idx);
    if W and $80 <> 0 then
    begin
      W := W and $3F;
      if W and $20 <> 0 then
      begin
        // 2nd byte
        C := TByteArray(Utf8Buf)[Utf8Idx];
        inc(Utf8Idx);
        if C and $C0 <> $80 then
          // malformed trail byte or out of range char
          Continue;
        W := (W shl 6) or (C and $3F);
      end;
      // 2nd or 3rd byte
      C := TByteArray(Utf8Buf)[Utf8Idx];
      inc(Utf8Idx);
      if C and $C0 <> $80 then
        // malformed trail byte
        Continue;
      TWordArray(WideBuf)[WideIdx] := (W shl 6) or (C and $3F);
      inc(WideIdx);
    end else
    begin
      TWordArray(WideBuf)[WideIdx] := W;
      inc(WideIdx);
    end;
  end;
  Result := WideIdx;
end;

function sdAnsiToUtf8(const A: AnsiString; ACodePage: integer): Utf8String;
var
  AnsiCount, Utf8Count: integer;
begin
  AnsiCount := length(A);
  SetLength(Result, AnsiCount * 3); // just to be sure
  if AnsiCount = 0 then
    exit;

  Utf8Count := sdAnsiToUtf8Buffer(A[1], Result[1], ACodePage, AnsiCount);
  SetLength(Result, Utf8Count);
end;

function sdAnsiToUtf8Buffer(const AnsiBuf; var Utf8Buf; ACodePage, AnsiCount: integer): integer;
var
  AnsiIdx, Utf8Idx: integer;
  AnsiCh: AnsiChar;
  WideCh: WideChar;
  Len: integer;
begin
  AnsiIdx := 0;
  Utf8Idx := 0;
  while AnsiIdx < AnsiCount do
  begin
    AnsiCh := TAnsiCharArray(AnsiBuf)[AnsiIdx];
    if ord(AnsiCh) < $80 then
    begin
      // characters < $80: just copy the single characters
      TAnsiCharArray(Utf8Buf)[Utf8Idx] := AnsiCh;
      inc(Utf8Idx);
    end else
    begin
      // characters >= $80: copy to widechar using codepage, then convert to Utf8
      // MultiByteToWideChar is in the Windows unit of Borland Delphi 7
      MultiByteToWideChar(ACodePage, 0, @AnsiCh, 1, @WideCh, 1);
      Len := sdWideToUtf8Buffer(WideCh, TAnsiCharArray(Utf8Buf)[Utf8Idx], 1);
      inc(Utf8Idx, Len);
    end;
    inc(AnsiIdx);
  end;
  Result := Utf8Idx;
end;

function sdUtf8ToAnsi(const U: Utf8String; ACodePage: integer): AnsiString;
// Convert UTF8 to Ansi string
var
  Utf8Count, AnsiCount: integer;
  DefaultCharUsed: boolean;
begin
  Utf8Count := length(U);
  SetLength(Result, Utf8Count);
  if Utf8Count = 0 then
    exit;

  AnsiCount := sdUtf8ToAnsiBuffer(U[1], Result[1], ACodePage, Utf8Count, DefaultCharUsed);
  SetLength(Result, AnsiCount);
end;

function sdUtf8ToAnsiBuffer(const Utf8Buf; var AnsiBuf; ACodePage, Utf8Count: integer;
  var DefaultCharUsed: boolean): integer;
var
  AnsiIdx, Utf8Idx: integer;
  Utf8Ch: AnsiChar;
  WideCh: WideChar;
  Len: integer;
  DU: pointer;
const
  cDefaultChar: AnsiChar = '?';
begin
  AnsiIdx := 0;
  Utf8Idx := 0;
  while Utf8Idx < Utf8Count do
  begin
    Utf8Ch := TAnsiCharArray(Utf8Buf)[Utf8Idx];
    if ord(Utf8Ch) < $80 then
    begin
      // characters < $80: just copy the single characters
      DefaultCharUsed := False;
      Len := 1;
      TAnsiCharArray(AnsiBuf)[AnsiIdx] := Utf8Ch;
      inc(AnsiIdx);
    end else
    begin
      Len := sdUtf8CharacterLength(TAnsiCharArray(Utf8Buf)[Utf8Idx]);
      sdUtf8ToWideBuffer(TAnsiCharArray(Utf8Buf)[Utf8Idx], WideCh, 1);
      // characters >= $80: copy to widechar using codepage, then convert to Utf8
      // WideCharToMultiByte is in the Windows unit of Borland Delphi 7
      DefaultCharUsed := False;
      DU := @DefaultCharUsed;
      WideCharToMultiByte(ACodePage, 0, @WideCh, 1, @TAnsiCharArray(AnsiBuf)[AnsiIdx], 1, @cDefaultChar, @DU);
      DefaultCharUsed := DU <> nil;
      inc(AnsiIdx);
    end;
    inc(Utf8Idx, Len);
  end;
  Result := AnsiIdx;
end;

function sdEscapeString(const AValue: Utf8String): Utf8String;
// contributor: Michael Cessna
var
  i, Len: Integer;
  P: PAnsiChar;
  HasEscapes: boolean;
  ScratchMem: TsdFastMemStream;
begin
  Result := '';
  Len := Length(AValue);
  if Len = 0 then
    Exit;

  HasEscapes := False;
  P := PAnsiChar(AValue);
  for i := 0 to Len - 1 do
  begin
    case P^ of
    '"'  : HasEscapes := True;
    '''' : HasEscapes := True;
    '&'  : HasEscapes := True;
    '<'  : HasEscapes := True;
    '>'  : HasEscapes := True;
    end;
    Inc(P);
  end;
  if not HasEscapes then
  begin
    Result := AValue;
    Exit;
  end;

  // ScratchMem is a TsdFastMemStream
  ScratchMem := TsdFastMemStream.Create(Len * 2);
  try
    P := PAnsiChar(AValue);
    for i := 0 to Len - 1 do
    begin
      case P^ of
      '"'  : ScratchMem.Write(AnsiString('&quot;'), 6);
      '''' : ScratchMem.Write(AnsiString('&apos;'), 6);
      '&'  : ScratchMem.Write(AnsiString('&amp;'), 5);
      '<'  : ScratchMem.Write(AnsiString('&lt;'), 4);
      '>'  : ScratchMem.Write(AnsiString('&gt;'), 4);
      else
        ScratchMem.Write(P^, 1);
      end;
      Inc(P);
    end;
    SetString(Result, PAnsiChar(ScratchMem.Memory), ScratchMem.Position);
  finally
    ScratchMem.Free;
  end;
end;

function sdReplaceString(const AValue: Utf8String; var HasNonStandardReferences: boolean;
  References: array of TXmlNode): Utf8String; overload;
var
  i, j, k, Len: Integer;
  P, Q: PAnsiChar;
  HasReferences, FoundReference: boolean;
  Reference, Replacement: Utf8String;
  ScratchMem: TsdFastMemStream;

  //local
  function FindNonStandardReferenceReplacement(AReference: Utf8String): Utf8String;
  var
    i: integer;
    Entity: TsdDtdEntity;
    ReferenceName, ReferenceValue: Utf8String;
  begin
    Result := '';
    if Length(References) = 0 then
      exit;
    for i := 0 to Length(References) - 1 do
    begin
      if References[i] is TsdDtdEntity then
      begin
        Entity := TsdDtdEntity(References[i]);
        ReferenceName := '&' + Entity.Name + ';';
        ReferenceValue :=  Entity.Value;
        if AReference = ReferenceName then
        begin
          Result := ReferenceValue;
          break;
        end;
      end;
    end;
  end;

// main
begin
  Result := '';
  Len := Length(AValue);
  if Len = 0 then
    Exit;

  HasReferences := False;
  HasNonStandardReferences := False;
  P := PAnsiChar(AValue);
  for i := 0 to Len - 1 do
  begin
    if P^ = '&' then
      HasReferences := True;
    Inc(P);
  end;
  if not HasReferences then
  begin
    Result := AValue;
    Exit;
  end;

  // ScratchMem is a TsdFastMemStream
  ScratchMem := TsdFastMemStream.Create(Len);
  try
    P := PAnsiChar(AValue);
    i := 0;
    while i < Len do
    begin
      FoundReference := False;
      if P^ = '&' then
      begin
        Q := P;
        inc(Q);
        for j := i + 1 to Len - 1 do
        begin
          if Q^ = '&' then
          begin
            // erronous duplicate quote! just let it be
            FoundReference := False;
            Break;
          end;
          if Q^ = ';' then
          begin
            // find reference
            Reference := Copy(AValue, i + 1, j - i + 1);
            inc(P, Length(Reference) - 1);
            inc(i, Length(Reference) - 1);

            // Look up standard escapes
            for k := 0 to cEscapeCount - 1 do
              if Reference = cXmlReplacePhrases[k] then
              begin
                // Replacement
                Replacement := cXmlEscapePhrases[k];
                ScratchMem.Write(Replacement[1], Length(Replacement));
                FoundReference := True;
                Break;
              end;
            if not FoundReference then
            begin

              // there was a non-standard reference, try to replace
              Replacement := FindNonStandardReferenceReplacement(Reference);
              if length(Replacement) = 0 then
              begin
                // replacement not found, so just write the reference
                ScratchMem.Write(Reference[1], Length(Reference));
              end else
              begin
                // write the replacement that was found :)
                ScratchMem.Write(Replacement[1], Length(Replacement));
              end;
              FoundReference := True;
              HasNonStandardReferences := True;

            end;
            Break;
          end;
          inc(Q);
        end;
      end;
      if not FoundReference then
        ScratchMem.Write(P^, 1);
      Inc(P);
      Inc(i);
    end;
    SetString(Result, PAnsiChar(ScratchMem.Memory), ScratchMem.Position);
  finally
    ScratchMem.Free;
  end;
end;

function sdReplaceString(const AValue: Utf8String; var HasNonStandardReferences: boolean): Utf8String;
var
  References: array of TXmlNode;
begin
  SetLength(References, 0);
  sdReplaceString(AValue, HasNonStandardReferences, References);
end;

function sdReplaceString(const AValue: Utf8String): Utf8String; overload;
var
  HasNonStandardReferences: boolean;
  References: array of TXmlNode;
begin
  SetLength(References, 0);
  Result := sdReplaceString(AValue, HasNonStandardReferences, References);
end;

function sdCommaToDot(const AValue: Utf8String): Utf8String;
var
  i: integer;
begin
  Result := AValue;
  for i := 1 to Length(AValue) do
    if AValue[i] = ',' then
      Result[i] := '.';
end;

function sdTrim(const S: Utf8String): Utf8String;
var
  IsTrimmed: boolean;
begin
  Result := sdTrim(S, IsTrimmed);
end;

function sdTrim(const S: Utf8String; var IsTrimmed: boolean): Utf8String;
var
  I, L: Integer;
begin
  IsTrimmed := False;
  L := Length(S);
  i := 1;
  while (i <= L) and (S[i] <= ' ') do
  begin
    inc(i);
    IsTrimmed := True;
  end;
  if i > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do
    begin
      dec(L);
      IsTrimmed := True;
    end;
    Result := Copy(S, i, L - i + 1);
  end;
end;

function sdNormaliseEol(const S: Utf8String): Utf8String;
var
  i, L: integer;
begin
  // collapse all end-of-line to a single LineFeed (#$0A)
  i := 1;
  L := Length(S);
  Result := S;
  while i <= L do
  begin
    if Result[i] = #$0A then
    begin
      if Result[i - 1] = #$0D then
      begin
        Move(Result[i], Result[i - 1], L - i + 1);
        dec(L);
      end;
    end;
    inc(i);
  end;
  SetLength(Result, L);
end;

function sdUnNormaliseEol(const S: Utf8String; EolStyle: TsdEolStyle): Utf8String;
// expand all single LineFeed (#$0A) to EOL defined by EolStyle
var
  i, L: integer;
begin
  Result := S;
  // only needs change if EolStyle = esWindows, in other words, if EolStyle = esLinux,
  // we are finished
  if EolStyle = esWindows then
  begin
    i := 1;
    L := Length(Result);
    while i <= L do
    begin
      if Result[i] = #$0A then
      begin
        inc(L);
        SetLength(Result, L);
        Move(Result[i], Result[i + 1], L - i);
        Result[i] := #$0D;
        inc(i);
      end;
      inc(i);
    end;
  end;
end;

procedure sdWriteToStream(S: TStream; const Value: Utf8String);
begin
  if Length(Value) > 0 then
  begin
    S.Write(Value[1], Length(Value));
  end;
end;

function sdCharsetToCodePage(ACharset: Utf8String; ADefaultCodepage: integer = 65001): integer;
var
  i: integer;
begin
  for i := 0 to cCodePageInfoCount - 1 do
  begin
    if AnsiCompareText(ACharset, cCodePageInfo[i].Name) = 0 then
    begin
      Result := cCodePageInfo[i].Codepage;
      exit;
    end;
  end;
  // Default
  Result := ADefaultCodepage;
end;

function sdCharsetToStringEncoding(ACharset: Utf8String): TsdStringEncoding;
var
  Codepage: integer;
begin
  Codepage := sdCharsetToCodePage(ACharset);
  case Codepage of
  1200: Result := seUTF16LE;
  1201: Result := seUTF16BE;
  65001: Result := seUTF8;
  else
    Result := seAnsi;
  end;
end;

function sdCodepageToCharset(ACodepage: integer): Utf8String;
// find the charset corresponding to windows codepage
var
  i: integer;
begin
  for i := 0 to cCodePageInfoCount - 1 do
  begin
    if cCodepageInfo[i].Codepage = ACodepage then
    begin
      Result := cCodepageInfo[i].Name;
      exit;
    end;
  end;
  // default to 'utf-8'
  Result := 'utf-8';
end;

function Utf8CompareText(const S1, S2: Utf8String): integer;
begin
  // AnsiCompareText is case-insensitive
  Result := AnsiCompareText(AnsiString(S1), AnsiString(S2));
end;

function GetTimeZoneBias: Integer;
// uses windows unit, func GetTimeZoneInformation
// contributor: Stefan Glienke
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TimeZoneInfo) of
    TIME_ZONE_ID_UNKNOWN: Result := TimeZoneInfo.Bias;
    TIME_ZONE_ID_STANDARD: Result := TimeZoneInfo.Bias + TimeZoneInfo.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: Result := TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias;
  else
    Result := 0;
  end;
end;

{ XYZ to string functions }

function sdDateTimeToString(ADate: TDateTime; UseDate: boolean = True; UseTime: boolean = True;
  SplitSecondDigits: integer = 0; UseLocalBias: boolean = False): Utf8String;
// Convert the TDateTime ADate to a string according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
var
  AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: word;
  ABias: Integer;
  DatePortion, TimePortion, SplitSecondPortion, LocalBiasPortion: Utf8String;
const
  Neg: array[Boolean] of string = ('+', '-');
begin
  DatePortion := '';
  TimePortion := '';
  
  if UseDate then
  begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    DatePortion := Utf8String(Format('%.4d-%.2d-%.2d', [AYear, AMonth, ADay]));
    // if we also use time, add the 'T' in advance
    if UseTime then
      DatePortion := DatePortion + 'T';
  end;
  if UseTime then
  begin
    DecodeTime(ADate, AHour, AMin, ASec, AMSec);
    if SplitSecondDigits > 0 then
    begin
      SplitSecondPortion := Utf8String(Format('%.3d', [AMSec]));
      if SplitSecondDigits < 3 then
      begin
        SplitSecondPortion := copy(SplitSecondPortion, 1, SplitSecondDigits);
      end;
      SplitSecondPortion := '.' + SplitSecondPortion;
    end else
    begin
      SplitSecondPortion := '';
    end;
    if UseLocalBias then
    begin
      ABias := GetTimeZoneBias;
      LocalBiasPortion := Utf8String(Format('%s%.2d:%.2d',
        [Neg[ABias > 0], Abs(ABias) div MinsPerHour, Abs(ABias) mod MinsPerHour]))
    end else
    begin
      LocalBiasPortion := 'Z';
    end;
    // final time portion
    TimePortion := Utf8String(Format('%.2d:%.2d:%.2d', [AHour, AMin, ASec]))
      + SplitSecondPortion + LocalBiasPortion;
  end;
  // final result
  Result := DatePortion + TimePortion;
end;

function sdBoolToString(Value: boolean): Utf8String;
const
  // do NOT localize! This is part of the W3 XML spec
  cBoolValues: array[boolean] of Utf8String = ('false', 'true');
begin
  Result := cBoolValues[Value];
end;

function sdFloatToString(Value: double; SignificantDigits: integer; AllowScientific: boolean): Utf8String;
const
  Limits: array[1..9] of integer =
    (10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);
var
  Limit, Limitd, PointPos, IntVal, ScPower: integer;
  Body: Utf8String;
begin
  if (SignificantDigits < 1) or (SignificantDigits > 9) then
    raise Exception.Create(sSignificantDigitsOutOfRange);

  // Zero
  if Value = 0 then
  begin
    Result := '0';
    exit;
  end;

  // Sign
  if Value < 0 then
  begin
    Result := '-';
    Value := -Value;
  end else
    Result := '';

  // Determine point position
  Limit := Limits[SignificantDigits];
  Limitd := Limit div 10;
  PointPos := SignificantDigits;
  while Value < Limitd do
  begin
    Value := Value * 10;
    dec(PointPos);
  end;
  while Value >= Limit do
  begin
    Value := Value * 0.1;
    inc(PointPos);
  end;

  // Round
  IntVal := round(Value);

  // Exceptional case which happens when the value rounds up to the limit
  if Intval = Limit then
  begin
    IntVal := IntVal div 10;
    inc(PointPos);
  end;

  // Strip off any zeros, these reduce significance count
  while (IntVal mod 10 = 0) and (PointPos < SignificantDigits) do
  begin
    dec(SignificantDigits);
    IntVal := IntVal div 10;
  end;

  // Check for scientific notation
  ScPower := 0;
  if AllowScientific and ((PointPos < -1) or (PointPos > SignificantDigits + 2)) then
  begin
    ScPower := PointPos - 1;
    dec(PointPos, ScPower);
  end;

  // Body
  Body := IntToStr(IntVal);
  while PointPos > SignificantDigits do
  begin
    Body := Body + '0';
    inc(SignificantDigits);
  end;
  while PointPos < 0 do
  begin
    Body := '0' + Body;
    inc(PointPos);
  end;
  if PointPos = 0 then
    Body := '.' + Body
  else
    if PointPos < SignificantDigits then
      Body := copy(Body, 1, PointPos) + '.' + copy(Body, PointPos + 1, SignificantDigits);

  // Final result
  if ScPower = 0 then
    Result := Result + Body
  else
    Result := Result + Body + 'E' + IntToStr(ScPower);
end;

function sdIntToString(Value: integer): Utf8String;
begin
  Result := Utf8String(IntToStr(Value));
end;

function sdInt64ToString(Value: int64): Utf8String;
begin
  // int64 can be used with IntToStr
  Result := Utf8String(IntToStr(Value));
end;

{ end XYZ to string functions }

{ string to XYZ functions }

function sdStringToDateTime(const ADate: Utf8String; UseLocalBias: Boolean): TDateTime;
// Convert the string ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// contributor: Stefan Glienke
var
  AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: word;
  ALocalBias, ABias: Integer;
begin
  AYear  := StrToInt(copy(ADate, 1, 4));
  AMonth := StrToInt(copy(ADate, 6, 2));
  ADay   := StrToInt(copy(ADate, 9, 2));
  if Length(ADate) > 16 then
  begin
    AHour := StrToInt(copy(ADate, 12, 2));
    AMin  := StrToInt(copy(ADate, 15, 2));
    ASec  := StrToIntDef(copy(ADate, 18, 2), 0); // They might be omitted, so default to 0
    AMSec := StrToIntDef(copy(ADate, 21, 3), 0); // They might be omitted, so default to 0
  end else
  begin
    AHour := 0;
    AMin  := 0;
    ASec  := 0;
    AMSec := 0;
  end;
  Result :=
    EncodeDate(AYear, AMonth, ADay) +
    EncodeTime(AHour, AMin, ASec, AMSec);
  ALocalBias := GetTimeZoneBias;
  if UseLocalBias then
  begin
    if (Length(ADate) > 24) then
    begin
      ABias := StrToInt(Copy(ADate, 25, 2)) * MinsPerHour +
        StrToInt(Copy(ADate, 28, 2));
      if ADate[24] = '+' then
        ABias := ABias * -1;
      Result := Result + ABias / MinsPerDay;
    end;
    Result := Result - ALocalBias / MinsPerDay;
  end;
end;

function sdStringToDateTimeDef(const ADate: Utf8String; ADefault: TDateTime; UseLocalBias: Boolean): TDateTime;
// Convert the string ADate to a TDateTime according to the W3C date/time specification
// as found here: http://www.w3.org/TR/NOTE-datetime
// If there is a conversion error, the default value ADefault is returned.
begin
  try
    Result := sdStringToDateTime(ADate, UseLocalBias);
  except
    Result := ADefault;
  end;
end;

function EncodeBase64(const Source: RawByteString): Utf8String;
// Encode binary data in Source as BASE64. The function returns the BASE64 encoded
// data as string, without any linebreaks.
begin
  if length(Source) > 0 then
    Result := EncodeBase64Buf(Source[1], length(Source))
  else
    Result := '';
end;

function EncodeBase64Buf(const Buffer; Count: Integer): Utf8String;
var
  i, j: integer;
  Core: integer;
  FourChar: cardinal;
  S: PByte;
begin
  // Make sure "Core" is always a multiple of 3, and this multiple
  // gets saved as 4 characters
  Core := (Count + 2) div 3;

  // Set the length of the string that stores encoded characters
  SetLength(Result, Core * 4);
  S := @Buffer;

  // Do the loop "Core" times
  for i := 0 to Core - 1 do
  begin
    FourChar := 0;
    for j := 0 to 2 do
    begin
      FourChar := FourChar shl 8 + S^;
      inc(S);
    end;
    for j := 0 to 3 do
    begin
      Result[i * 4 + 4 - j] := cBase64Char[FourChar and $3F];
      FourChar := FourChar shr 6;
    end;
  end;

  // For comformity to Base64, we must pad the data instead of zero out
  // when the size is not an exact multiple of 3
  case Core * 3 - Count of
  0:;// nothing to do
  1: // pad one char
    Result[Core * 4] := cBase64PadChar;
  2: // pad two chars
    begin
      Result[Core * 4 - 1] := cBase64PadChar;
      Result[Core * 4    ] := cBase64PadChar;
    end;
  end;//case
end;

function DecodeBase64(const Source: Utf8String): RawByteString;
// Decode BASE64 data in Source into binary data. The function returns the binary
// data as Utf8String.
var
  BufData: Utf8String;
  BufSize, BufPos: integer;
begin
  BufData := sdRemoveControlChars(Source);

  // Determine length of data
  BufSize := length(BufData) div 4;
  if BufSize * 4 <> length(BufData) then
    raise EFilerError.Create(sErrorCalcStreamLength);
  BufSize := BufSize * 3;

  // Check padding chars
  BufPos := length(BufData);
  if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
  begin
    dec(BufPos);
    dec(BufSize);
    if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
      dec(BufSize);
  end;
  Setlength(Result, BufSize);

  // Decode
  if BufSize > 0 then
    DecodeBase64Buf(BufData, Result[1], BufSize);
end;

procedure DecodeBase64Buf(var Source: Utf8String; var Buffer; Count: Integer);
var
  i, j: integer;
  BufPos, Core: integer;
  FourChar: cardinal;
  D: PByte;
  Map: array[AnsiChar] of byte;
begin
  // Core * 4 is the number of chars to read - check length
  Core := Length(Source) div 4;
  if Count > Core * 3 then
    raise EFilerError.Create(sMissingDataInBinaryStream);

  // Prepare map
  for i := 0 to 63 do
    Map[cBase64Char[i]] := i;
  D := @Buffer;

  // Check for final padding, and replace with "zeros". There can be
  // at max two pad chars ('=')
  BufPos := length(Source);
  if (BufPos > 0) and (Source[BufPos] = cBase64PadChar) then
  begin
    Source[BufPos] := cBase64Char[0];
    dec(BufPos);
    if (BufPos > 0) and (Source[BufPos] = cBase64PadChar) then
      Source[BufPos] := cBase64Char[0];
  end;

  // Do this "Core" times
  for i := 0 to Core - 1 do
  begin
    FourChar := 0;

    // Unroll the characters
    for j := 0 to 3 do
      FourChar := FourChar shl 6 + Map[Source[i * 4 + j + 1]];

    // and unroll the bytes
    for j := 2 downto 0 do
    begin
      // Check overshoot
      if integer(D) - integer(@Buffer) >= Count then
        exit;
      D^ := FourChar shr (j * 8) and $FF;
      inc(D);
    end;
  end;
end;

function EncodeBinHex(const Source: RawByteString): Utf8String;
// Encode binary data in Source as BINHEX. The function returns the BINHEX encoded
// data as UTF8String, without any linebreaks.
var
  Text: Utf8String;
begin
  SetLength(Text, Length(Source) * 2);
  BinToHex(PAnsiChar(Source), PAnsiChar(Text), Length(Source));
  Result := Text;
end;

function DecodeBinHex(const Source: Utf8String): RawByteString;
// Decode BINHEX data in Source into binary data. The function returns the binary
// data as RawByteString. Use a TStringStream to convert this data to a stream.
var
  Data: Utf8String;
  Size: integer;
  Buffer: RawByteString;
begin
  Data := sdRemoveControlChars(Source);

  // Determine length of data
  Size := length(Data) div 2;
  if Size * 2 <> length(Data) then
    raise EFilerError.Create(sErrorCalcStreamLength);

  SetLength(Buffer, Size);
  HexToBin(PAnsiChar(Data), PAnsiChar(Buffer), Size);
  Result := Buffer;
end;

procedure DecodeBinhexBuf(var Source: Utf8String; var Buffer; Count: Integer);
var
  Size: integer;
begin
  // Determine length of data
  Size := Count div 2;
  if Size * 2 <> Count then
    raise EFilerError.Create(sErrorCalcStreamLength);

  HexToBin(PAnsiChar(Source), PAnsiChar(Buffer), Count);
end;

function sdRemoveControlChars(const AValue: Utf8String): Utf8String;
// Remove control characters from Utf8String AValue
var
  i, j: integer;
begin
  Setlength(Result, Length(AValue));
  i := 1;
  j := 1;
  while i <= Length(AValue) do
    if AValue[i] in cXmlBlankChars then
      inc(i)
    else
    begin
      Result[j] := AValue[i];
      inc(i);
      inc(j);
    end;
  // Adjust length
  if i <> j then
    SetLength(Result, j - 1);
end;

function sdAddControlChars(const AValue: Utf8String; const ControlChars: Utf8String; Interval: integer): Utf8String;
// Insert Chars in AValue at each Interval
var
  i, j, L: integer;
  // local
  procedure InsertControlChars;
  var
    k: integer;
  begin
    for k := 1 to Length(ControlChars) do
    begin
      Result[j] := ControlChars[k];
      inc(j);
    end;
  end;
// main
begin
  if (Length(ControlChars) = 0) or (Interval <= 0) then
  begin
    Result := AValue;
    exit;
  end;

  // Calculate length based on original length and total extra length for control chars
  L := Length(AValue) + ((Length(AValue) - 1) div Interval + 3) * Length(ControlChars);
  SetLength(Result, L);

  // Copy and insert
  j := 1;
  for i := 1 to Length(AValue) do
  begin
    if (i mod Interval) = 1 then
      // Insert control chars
      InsertControlChars;
    Result[j] := AValue[i];
    inc(j);
  end;
  InsertControlChars;

  // Adjust length
  dec(j);
  if L > j then
    SetLength(Result, j);
end;

{ former unit sdStringEncodig }

function sdUtf8CharacterLength(const Buffer): integer;
// determine the character length (1..4 bytes) of the Utf8 character
// in the buffer
type
  TByteArray = array[0..3] of byte;
var
  P0, P1, P2, P3: byte;
begin
  P0 := TByteArray(Buffer)[0];
  Result := 1;
  if P0 < $C0 then // %11000000
  begin
    // regular single byte character
    exit;
  end;
  P1 := TByteArray(Buffer)[1];
  if (P0 and $E0) = $C0 then
  begin
    // could be 2 byte character
    if (P1 and $C0) = $80 then
    begin
      Result := 2;
    end;
    exit;
  end;
  P2 := TByteArray(Buffer)[2];
  if (P0 and $F0) = $E0 then
  begin
    // could be 3 byte character
    if ((P1 and $C0) = $80) and ((P2 and $C0) = $80) then
    begin
      Result := 3;
    end;
    exit;
  end;
  P3 := TByteArray(Buffer)[3];
  if (P0 and $F8) = $F0 then
  begin
    // could be 4 byte character
    // NB 4 byte chars are incompatible with Widechar since
    // they are outside the basic lingual plane
    if    ((P1 and $C0) = $80)
      and ((P2 and $C0) = $80)
      and ((P3 and $C0) = $80) then
    begin
      Result := 4;
    end;
  end;
end;

procedure GetXmlFormatSettings;
var
  TimePrefix, TimePostfix, HourFormat: string;
begin
  cXmlFormatSettings.CurrencyString := '';
  cXmlFormatSettings.CurrencyFormat := 0;
  cXmlFormatSettings.NegCurrFormat := 0;
  cXmlFormatSettings.ThousandSeparator := ',';
  cXmlFormatSettings.DecimalSeparator := '.';
  cXmlFormatSettings.CurrencyDecimals := 0;
  cXmlFormatSettings.DateSeparator := '/';
  cXmlFormatSettings.ShortDateFormat := 'm/d/yy';
  cXmlFormatSettings.LongDateFormat := 'mmmm d, yyyy';
  cXmlFormatSettings.TimeSeparator := ':';
  cXmlFormatSettings.TimeAMString := 'am';
  cXmlFormatSettings.TimePMString := 'pm';
  TimePrefix := '';
  HourFormat := 'h';
  TimePostfix := ' AMPM';
  cXmlFormatSettings.ShortTimeFormat := TimePrefix + HourFormat + ':mm' + TimePostfix;
  cXmlFormatSettings.LongTimeFormat := TimePrefix + HourFormat + ':mm:ss' + TimePostfix;
  cXmlFormatSettings.ListSeparator := ',';
end;

initialization

  // NativeXml's xml format settings (with decimal separator = '.')
  GetXmlFormatSettings;

end.
