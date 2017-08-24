unit CatEntities;

{
  Catarinka - HTML Entity Decoder & Encoder
  Copyright (c) 2003-2017 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.SysUtils,
  {$IFDEF DXE7_OR_UP}
  System.NetEncoding,
  {$ELSE}
  Web.HTTPApp,
  {$ENDIF}
  CatRegEx;
{$ELSE}
  SysUtils, HTTPApp, CatRegEx;
{$ENDIF}

type
  THTMLEntity = record
    char: string;
    name: string;
    code: integer;
    od: boolean; // true if compatibe with non-unicode
  end;

type
  THTMLEntities = class
  private
    function CanEncodeEntity(const e:THTMLEntity): boolean;
    function EntityAllowed(const e:THTMLEntity): boolean;
    function HextoDec(ARegExpr : TCatRegExpr): string;
    function DecodeEntity(ARegExpr : TCatRegExpr): string;
    function RemoveLeadingZeros(ARegExpr : TCatRegExpr): string;
  public
    function Decode(const s:string):string;
    function Encode(const s:string):string;
    constructor Create;
    destructor Destroy; override;
  end;

function HTML_Entity_Decode(const s: string): string;
function HTML_Entity_Encode(const s: string): string;

implementation

uses
  CatStrings;

const
  reUnicodeSymbol = '\&#(x|X)([A-Fa-f0-9]+){1,4}\;';
  reDecimalSymbol = '\&#([0-9]+){1,4}\;';

const
  THTMLEntityMap: array[1..276] of THTMLEntity = (
    (char: '"'; name: 'quot'; code: 34),
    (char: ''''; name: 'apos'; code: 39),
    (char: '<'; name: 'lt'; code: 60),
    (char: '>'; name: 'gt'; code: 62),
    // Codes 128-159 contain the Windows Latin-1 extended characters
    (char: '€'; name: 'euro'; code: 128),
    (char: '‚'; name: 'sbquo'; code: 130),
    (char: 'ƒ'; name: 'fnof'; code: 131),
    (char: '„'; name: 'bdquo'; code: 132),
    (char: '…'; name: 'hellip'; code: 133),
    (char: '†'; name: 'dagger'; code: 134),
    (char: '‡'; name: 'Dagger'; code: 135),
    (char: 'ˆ'; name: 'circ'; code: 136),
    (char: '‰'; name: 'permil'; code: 137),
    (char: 'Š'; name: 'Scaron'; code: 138),
    (char: '‹'; name: 'lsaquo'; code: 139),
    (char: 'Œ'; name: 'OElig'; code: 140),
    (char: '‘'; name: 'lsquo'; code: 145),
    (char: '’'; name: 'rsquo'; code: 146),
    (char: '“'; name: 'ldquo'; code: 147),
    (char: '”'; name: 'rdquo'; code: 148),
    (char: '•'; name: 'bull'; code: 149),
    (char: '–'; name: 'ndash'; code: 150),
    (char: '—'; name: 'mdash'; code: 151),
    (char: '˜'; name: 'tilde'; code: 152),
    (char: '™'; name: 'trade'; code: 153),
    (char: 'š'; name: 'scaron'; code: 154),
    (char: '›'; name: 'rsaquo'; code: 155),
    (char: 'œ'; name: 'oelig'; code: 156),
    (char: 'Ÿ'; name: 'Yuml'; code: 159),
    // End of Latin-1 extended characters
    (char: ' '; name: 'nbsp'; code: 160),
    (char: '¡'; name: 'iexcl'; code: 161),
    (char: '¢'; name: 'cent'; code: 162),
    (char: '£'; name: 'pound'; code: 163),
    (char: '¤'; name: 'curren'; code: 164),
    (char: '¥'; name: 'yen'; code: 165),
    (char: '¦'; name: 'brvbar'; code: 166),
    (char: '§'; name: 'sect'; code: 167),
    (char: '¨'; name: 'uml'; code: 168),
    (char: '©'; name: 'copy'; code: 169),
    (char: 'ª'; name: 'ordf'; code: 170),
    (char: '«'; name: 'laquo'; code: 171),
    (char: '¬'; name: 'not'; code: 172),
    (char: '­'; name: 'shy'; code: 173),
    (char: '®'; name: 'reg'; code: 174),
    (char: '¯'; name: 'macr'; code: 175),
    (char: '°'; name: 'deg'; code: 176),
    (char: '±'; name: 'plusmn'; code: 177),
    (char: '²'; name: 'sup2'; code: 178),
    (char: '³'; name: 'sup3'; code: 179),
    (char: '´'; name: 'acute'; code: 180),
    (char: 'µ'; name: 'micro'; code: 181),
    (char: '¶'; name: 'para'; code: 182),
    (char: '·'; name: 'middot'; code: 183),
    (char: '¸'; name: 'cedil'; code: 184),
    (char: '¹'; name: 'sup1'; code: 185),
    (char: 'º'; name: 'ordm'; code: 186),
    (char: '»'; name: 'raquo'; code: 187),
    (char: '¼'; name: 'frac14'; code: 188),
    (char: '½'; name: 'frac12'; code: 189),
    (char: '¾'; name: 'frac34'; code: 190),
    (char: '¿'; name: 'iquest'; code: 191),
    (char: 'À'; name: 'Agrave'; code: 192),
    (char: 'Á'; name: 'Aacute'; code: 193),
    (char: 'Â'; name: 'Acirc'; code: 194),
    (char: 'Ã'; name: 'Atilde'; code: 195),
    (char: 'Ä'; name: 'Auml'; code: 196),
    (char: 'Å'; name: 'Aring'; code: 197),
    (char: 'Æ'; name: 'AElig'; code: 198),
    (char: 'Ç'; name: 'Ccedil'; code: 199),
    (char: 'È'; name: 'Egrave'; code: 200),
    (char: 'É'; name: 'Eacute'; code: 201),
    (char: 'Ê'; name: 'Ecirc'; code: 202),
    (char: 'Ë'; name: 'Euml'; code: 203),
    (char: 'Ì'; name: 'Igrave'; code: 204),
    (char: 'Í'; name: 'Iacute'; code: 205),
    (char: 'Î'; name: 'Icirc'; code: 206),
    (char: 'Ï'; name: 'Iuml'; code: 207),
    (char: 'Ð'; name: 'ETH'; code: 208),
    (char: 'Ñ'; name: 'Ntilde'; code: 209),
    (char: 'Ò'; name: 'Ograve'; code: 210),
    (char: 'Ó'; name: 'Oacute'; code: 211),
    (char: 'Ô'; name: 'Ocirc'; code: 212),
    (char: 'Õ'; name: 'Otilde'; code: 213),
    (char: 'Ö'; name: 'Ouml'; code: 214),
    (char: '×'; name: 'times'; code: 215),
    (char: 'Ø'; name: 'Oslash'; code: 216),
    (char: 'Ù'; name: 'Ugrave'; code: 217),
    (char: 'Ú'; name: 'Uacute'; code: 218),
    (char: 'Û'; name: 'Ucirc'; code: 219),
    (char: 'Ü'; name: 'Uuml'; code: 220),
    (char: 'Ý'; name: 'Yacute'; code: 221),
    (char: 'Þ'; name: 'THORN'; code: 222),
    (char: 'ß'; name: 'szlig'; code: 223),
    (char: 'à'; name: 'agrave'; code: 224),
    (char: 'á'; name: 'aacute'; code: 225),
    (char: 'â'; name: 'acirc'; code: 226),
    (char: 'ã'; name: 'atilde'; code: 227),
    (char: 'ä'; name: 'auml'; code: 228),
    (char: 'å'; name: 'aring'; code: 229),
    (char: 'æ'; name: 'aelig'; code: 230),
    (char: 'ç'; name: 'ccedil'; code: 231),
    (char: 'è'; name: 'egrave'; code: 232),
    (char: 'é'; name: 'eacute'; code: 233),
    (char: 'ê'; name: 'ecirc'; code: 234),
    (char: 'ë'; name: 'euml'; code: 235),
    (char: 'ì'; name: 'igrave'; code: 236),
    (char: 'í'; name: 'iacute'; code: 237),
    (char: 'î'; name: 'icirc'; code: 238),
    (char: 'ï'; name: 'iuml'; code: 239),
    (char: 'ð'; name: 'eth'; code: 240),
    (char: 'ñ'; name: 'ntilde'; code: 241),
    (char: 'ò'; name: 'ograve'; code: 242),
    (char: 'ó'; name: 'oacute'; code: 243),
    (char: 'ô'; name: 'ocirc'; code: 244),
    (char: 'õ'; name: 'otilde'; code: 245),
    (char: 'ö'; name: 'ouml'; code: 246),
    (char: '÷'; name: 'divide'; code: 247),
    (char: 'ø'; name: 'oslash'; code: 248),
    (char: 'ù'; name: 'ugrave'; code: 249),
    (char: 'ú'; name: 'uacute'; code: 250),
    (char: 'û'; name: 'ucirc'; code: 251),
    (char: 'ü'; name: 'uuml'; code: 252),
    (char: 'ý'; name: 'yacute'; code: 253),
    (char: 'þ'; name: 'thorn'; code: 254),
    (char: 'ÿ'; name: 'yuml'; code: 255),
    (char: 'Œ'; name: 'OElig'; code: 338; od: true),
    (char: 'œ'; name: 'oelig'; code: 339; od: true),
    (char: 'Š'; name: 'Scaron'; code: 352; od: true),
    (char: 'š'; name: 'scaron'; code: 353; od: true),
    (char: 'Ÿ'; name: 'Yuml'; code: 376; od: true),
    (char: 'ƒ'; name: 'fnof'; code: 402; od: true),
    (char: 'ˆ'; name: 'circ'; code: 710; od: true),
    (char: '˜'; name: 'tilde'; code: 732; od: true),
    // ISOgrk3 starts
    (char: #913; name: 'Alpha'; code: 913),
    (char: #914; name: 'Beta'; code: 914),
    (char: #915; name: 'Gamma'; code: 915),
    (char: #916; name: 'Delta'; code: 916),
    (char: #917; name: 'Epsilon'; code: 917),
    (char: #918; name: 'Zeta'; code: 918),
    (char: #919; name: 'Eta'; code: 919),
    (char: #920; name: 'Theta'; code: 920),
    (char: #921; name: 'Iota'; code: 921),
    (char: #922; name: 'Kappa'; code: 922),
    (char: #923; name: 'Lambda'; code: 923),
    (char: #924; name: 'Mu'; code: 924),
    (char: #925; name: 'Nu'; code: 925),
    (char: #926; name: 'Xi'; code: 926),
    (char: #927; name: 'Omicron'; code: 927),
    (char: #928; name: 'Pi'; code: 928),
    (char: #929; name: 'Rho'; code: 929),
    (char: #931; name: 'Sigma'; code: 931),
    (char: #932; name: 'Tau'; code: 932),
    (char: #933; name: 'Upsilon'; code: 933),
    (char: #934; name: 'Phi'; code: 934),
    (char: #935; name: 'Chi'; code: 935),
    (char: #936; name: 'Psi'; code: 936),
    (char: #937; name: 'Omega'; code: 937),
    (char: #945; name: 'alpha'; code: 945),
    (char: #946; name: 'beta'; code: 946),
    (char: #947; name: 'gamma'; code: 947),
    (char: #948; name: 'delta'; code: 948),
    (char: #949; name: 'epsilon'; code: 949),
    (char: #950; name: 'zeta'; code: 950),
    (char: #951; name: 'eta'; code: 951),
    (char: #952; name: 'theta'; code: 952),
    (char: #953; name: 'iota'; code: 953),
    (char: #954; name: 'kappa'; code: 954),
    (char: #955; name: 'lambda'; code: 955),
    (char: #956; name: 'mu'; code: 956),
    (char: #957; name: 'nu'; code: 957),
    (char: #958; name: 'xi'; code: 958),
    (char: #959; name: 'omicron'; code: 959),
    (char: #960; name: 'pi'; code: 960),
    (char: #961; name: 'rho'; code: 961),
    (char: #962; name: 'sigmaf'; code: 962),
    (char: #963; name: 'sigma'; code: 963),
    (char: #964; name: 'tau'; code: 964),
    (char: #965; name: 'upsilon'; code: 965),
    (char: #966; name: 'phi'; code: 966),
    (char: #967; name: 'chi'; code: 967),
    (char: #968; name: 'psi'; code: 968),
    (char: #969; name: 'omega'; code: 969),
    (char: #977; name: 'thetasym'; code: 977),
    (char: #978; name: 'upsih'; code: 978),
    (char: #982; name: 'piv'; code: 982),
    // greek end
    //ISOpub
    (char: #8194; name: 'ensp'; code: 8194),
    (char: #8195; name: 'emsp'; code: 8195),
    (char: #8201; name: 'thinsp'; code: 8201),
    (char: #8204; name: 'zwnj'; code: 8204),
    (char: #8205; name: 'zwj'; code: 8205),
    (char: #8206; name: 'lrm'; code: 8206),
    (char: #8207; name: 'rlm'; code: 8207),
    (char: '–'; name: 'ndash'; code: 8211; od: true),
    (char: '—'; name: 'mdash'; code: 8212; od: true),
    (char: '‘'; name: 'lsquo'; code: 8216; od: true),
    (char: '’'; name: 'rsquo'; code: 8217; od: true),
    (char: '‚'; name: 'sbquo'; code: 8218; od: true),
    (char: '“'; name: 'ldquo'; code: 8220; od: true),
    (char: '”'; name: 'rdquo'; code: 8221; od: true),
    (char: '„'; name: 'bdquo'; code: 8222; od: true),
    (char: '†'; name: 'dagger'; code: 8224; od: true),
    (char: '‡'; name: 'Dagger'; code: 8225; od: true),
    (char: '•'; name: 'bull'; code: 8226; od: true),
    (char: '…'; name: 'hellip'; code: 8230; od: true),
    (char: '‰'; name: 'permil'; code: 8240; od: true),
    (char: ''''; name: 'prime'; code: 8242; od: true),
    (char: #8243; name: 'Prime'; code: 8243),
    (char: '‹'; name: 'lsaquo'; code: 8249; od: true),
    (char: '›'; name: 'rsaquo'; code: 8250; od: true),
    (char: #8254; name: 'oline'; code: 8254),
    (char: '/'; name: 'frasl'; code: 8260; od: true),
    (char: '€'; name: 'euro'; code: 8364; od: true),
    (char: #8465; name: 'image'; code: 8465),
    (char: #8472; name: 'weierp'; code: 8472),
    (char: #8476; name: 'real'; code: 8476),
    (char: '™'; name: 'trade'; code: 8482; od: true),
    (char: #8501; name: 'alefsym'; code: 8501),
    (char: #8592; name: 'larr'; code: 8592),
    (char: #8593; name: 'uarr'; code: 8593),
    (char: #8594; name: 'rarr'; code: 8594),
    (char: #8595; name: 'darr'; code: 8595),
    (char: #8596; name: 'harr'; code: 8596),
    (char: #8629; name: 'crarr'; code: 8629),
    (char: #8656; name: 'lArr'; code: 8656),
    (char: #8657; name: 'uArr'; code: 8657),
    (char: #8658; name: 'rArr'; code: 8658),
    (char: #8659; name: 'dArr'; code: 8659),
    (char: #8660; name: 'hArr'; code: 8660),
    (char: #8704; name: 'forall'; code: 8704),
    (char: #8706; name: 'part'; code: 8706),
    (char: #8707; name: 'exist'; code: 8707),
    (char: 'Ø'; name: 'empty'; code: 8709; od: true),
    (char: #8711; name: 'nabla'; code: 8711),
    (char: #8712; name: 'isin'; code: 8712),
    (char: #8713; name: 'notin'; code: 8713),
    (char: #8715; name: 'ni'; code: 8715),
    (char: #8719; name: 'prod'; code: 8719),
    (char: #8721; name: 'sum'; code: 8721),
    (char: '-'; name: 'minus'; code: 8722; od: true),
    (char: '*'; name: 'lowast'; code: 8727; od: true),
    (char: #8730; name: 'radic'; code: 8730),
    (char: #8733; name: 'prop'; code: 8733),
    (char: #8734; name: 'infin'; code: 8734),
    (char: #8736; name: 'ang'; code: 8736),
    (char: #8743; name: 'and'; code: 8743),
    (char: #8744; name: 'or'; code: 8744),
    (char: #8745; name: 'cap'; code: 8745),
    (char: #8746; name: 'cup'; code: 8746),
    (char: #8747; name: 'int'; code: 8747),
    (char: #8756; name: 'there4'; code: 8756),
    (char: '~'; name: 'sim'; code: 8764; od: true),
    (char: #8773; name: 'cong'; code: 8773),
    (char: #8776; name: 'asymp'; code: 8776),
    (char: #8800; name: 'ne'; code: 8800),
    (char: #8801; name: 'equiv'; code: 8801),
    (char: #8804; name: 'le'; code: 8804),
    (char: #8805; name: 'ge'; code: 8805),
    (char: #8834; name: 'sub'; code: 8834),
    (char: #8835; name: 'sup'; code: 8835),
    (char: #8836; name: 'nsub'; code: 8836),
    (char: #8838; name: 'sube'; code: 8838),
    (char: #8853; name: 'oplus'; code: 8853),
    (char: #8855; name: 'otimes'; code: 8855),
    (char: #8869; name: 'perp'; code: 8869),
    (char: '·'; name: 'sdot'; code: 8901; od: true),
    (char: #8968; name: 'lceil'; code: 8968),
    (char: #8969; name: 'rceil'; code: 8969),
    (char: #8970; name: 'lfloor'; code: 8970),
    (char: #8971; name: 'rfloor'; code: 8971),
    (char: #9001; name: 'lang'; code: 9001),
    (char: #9002; name: 'rang'; code: 9002),
    (char: #9674; name: 'loz'; code: 9674),
    (char: #9824; name: 'spades'; code: 9824),
    (char: #9827; name: 'clubs'; code: 9827),
    (char: #9829; name: 'hearts'; code: 9829),
    (char: #9830; name: 'diams'; code: 9830)
  );

function HTML_Entity_Decode(const s: string): string;
var
  d:THTMLEntities;
begin
  d := THTMLEntities.Create;
  result := d.Decode(s);
  d.Free;
end;

function HTML_Entity_Encode(const s: string): string;
var
  d:THTMLEntities;
begin
  d := THTMLEntities.Create;
  result := d.Encode(s);
  d.Free;
end;

function THTMLEntities.EntityAllowed(const e:THTMLEntity): boolean;
begin
  {$IFDEF UNICODE}
    // always replace when unicode Delphi
    result := true;
  {$ELSE}
    result := false;
    // only replace if character plays well with non-unicode Delphi
    if e.code <= 255 then
      result := true;
    if e.od = true then
      result := true;
  {$ENDIF}
end;

function THTMLEntities.CanEncodeEntity(const e:THTMLEntity): boolean;
begin
  result := true;
  if EntityAllowed(e) = false then
    result := false;
  case e.code of
    160 : result := false; // do not encode nbsp
  end;
end;

function THTMLEntities.Encode(const s:string):string;
var
  i: integer;
begin
  result := s;
  for i := Low(THTMLEntityMap) to High(THTMLEntityMap) do
    if CanEncodeEntity(THTMLEntityMap[I]) then
    result := ReplaceStr(result, THTMLEntityMap[I].char, '&'+THTMLEntityMap[I].name+';');
end;

function THTMLEntities.Decode(const s:string):string;
var
  i: integer;
begin
  result := s;

  // converts named entities &quot;, &copy;, etc
  for i := Low(THTMLEntityMap) to High(THTMLEntityMap) do begin
    if EntityAllowed(THTMLEntityMap[I]) then
    result := ReplaceStr(result, '&'+THTMLEntityMap[I].name+';', THTMLEntityMap[I].char);
  end;

  // converts &#nnnn; and &#xhhhh; (if any)
  if pos('&#', result) <> 0 then begin
      // converts an unicode code point like &#x27 or &#x0027 to decimal like &#39
      if pos('&#x', lowercase(result)) <> 0 then
        result := RegExpReplace(result, reUnicodeSymbol, HextoDec);
      // remove leading zeros (if any) from decimal code (eg &#0039)
      if pos('&#0', result) <> 0 then
        result := RegExpReplace(result, reDecimalSymbol, RemoveLeadingZeros);

    // converts the decimal code points to their corresponding characters
    for i := 0 to 255 do
      result := ReplaceStr(result, '&#'+ IntToStr(i)+';', chr(i));
    for i := Low(THTMLEntityMap) to High(THTMLEntityMap) do begin
      if EntityAllowed(THTMLEntityMap[I]) then
      result := ReplaceStr(result, '&#'+IntToStr(THTMLEntityMap[I].code)+';', THTMLEntityMap[I].char);
    end;
  end;

  // handles entities not covered by the above entity map (if any) 
  if pos('&#', result) <> 0 then
    result := RegExpReplace(result, reDecimalSymbol, DecodeEntity);
  
  // finally, converts the amp
  result := replacestr(result, '&#38;', '&');
  result := replacestr(result, '&amp;', '&');
end;

function THTMLEntities.HextoDec(ARegExpr : TCatRegExpr): string;
var
  code:string;
begin
  code := StripChars(aregexpr.Match[0], ['&','#','x',';']);
  result := '&#'+InttoStr(HexToInt(code))+';';
end;

function THTMLEntities.DecodeEntity(ARegExpr : TCatRegExpr): string;
var
  match:string;
begin
  match := aregexpr.Match[0];
  {$IFDEF UNICODE}
    {$IFDEF DXE7_OR_UP}
    result := TNetEncoding.HTML.Decode(match);
    {$ELSE}
    result := HtmlDecode(match);
    {$ENDIF}
  {$ELSE}
    // HtmlDecode() is not reliable with non-unicode Delphi, so return untouched
    result := match;
  {$ENDIF}
end;

function THTMLEntities.RemoveLeadingZeros(ARegExpr : TCatRegExpr): string;
var
  match, code:string;
begin
  match := aregexpr.Match[0];
  if pos('&#0', match)<>0 then begin
    code := StripChars(match, ['&','#',';']);
    result := '&#'+InttoStr(StrToInt(code))+';';
  end else
    result := match;
end;

constructor THTMLEntities.Create;
begin
 //
end;

destructor THTMLEntities.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------//
end.
