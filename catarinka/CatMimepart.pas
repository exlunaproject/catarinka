{
  Catarinka Multipart Form & MIME Utilities
  Copyright (c) 2003-2022 Felipe Daragon and Roberto Marc
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  This unit includes an improved TMultipartFormDataStream by me and Roberto.
  TMultipartFormDataStream was originally created by Alexandr Mikhnevich
  (aka ArhangeL) and designed to work with Synapse's THTTPSend.

  The Synapse library is available under a modified BSD style license.

  The changes and improvements include:
  * Default mode for bool to str conversion in AddFieldBool() changed to binary
  * Added overloaded AddFieldBool() that allows to set a different conversion mode
  * Allow to set a default conversion mode for AddFieldBool() using the new
  FieldBoolConvMode property
  * Disabled by default the use of the MIME cache and added the UseMIMETypeCache
  property allowing to enable its use if needed
  * Minor changes to line breaking for improved compatibility with today's web
  applications.
  * Replaced the content-type octet-string (?) with text/plain in AddFieldString().
}

unit CatMimepart;

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, Winapi.Windows, System.Win.Registry,
{$ELSE}
  Classes, SysUtils, Windows, Registry,
{$ENDIF}
  CatStrings;

type
  TBoolToStrMode = (btsBinary, btsNegative, btsString, btsStringLo, btsYN,
    btsCustom);

type
  TMultipartFormDataStream = class(TObject)
  private
    fBound: string;
    fBoundStr: string;
    fClosed: Boolean;
    fDefaultBoolToStrMode: TBoolToStrMode;
    fFalseValue:string;
    fTrueValue:string;
    fUseMIMETypeCache: boolean;
    function GetMimeProp: string;
  protected
    fStream: TStringStream;
  public
    function GetFileMIMEType(const aFile: string): string;
    procedure AddFieldString(aFieldName: string; aValue: string;includeCT:boolean=true);
    procedure AddFieldInteger(aFieldName: string; aValue: Integer);
    procedure AddFieldFloat(aFieldName: string; aValue: Extended);
    procedure AddFieldBool(aFieldName: string; aValue: Boolean); overload;
    procedure AddFieldBool(aFieldName: string; aValue: Boolean;
      mode:TBoolToStrMode); overload;
    procedure AddFile(aFieldName: string; aFileName: string); overload;
    procedure AddFile(aFieldName, aFileName: string; aFileContent: TStream); overload;
    procedure SetFieldBoolCustomValues(aTrueValue:string; aFalseValue:string);
    property FieldBoolConvMode: TBoolToStrMode read fDefaultBoolToStrMode
      write fDefaultBoolToStrMode;
    property Stream: TStringStream read fStream write fStream;
    property MIMEType: string read GetMimeProp;
    property IsClosed: Boolean read fClosed;
    property UseMIMETypeCache: Boolean read fUseMIMETypeCache write fUseMIMETypeCache;
    procedure DataEnd;
    constructor Create;
    destructor Destroy; override;
  end;

function GetFileMIMETypeFromCache(const aFile: string): string;

implementation

var
  MIMETypeCache: TStringList;

procedure LoadMIMETypeCache(MTL: TStrings);
var
  reg: TRegistry;
  KeyList: TStringList;
  i: Integer;
  s: String;
begin
  if not Assigned(MIMETypeCache) then
  begin
    MIMETypeCache := TStringList.Create;
    with MIMETypeCache do
    begin
      Add('.nml=animation/narrative');
      Add('.aac=audio/mp4');
      Add('.aif=audio/x-aiff');
      Add('.aifc=audio/x-aiff');
      Add('.aiff=audio/x-aiff');
      Add('.au=audio/basic');
      Add('.gsm=audio/x-gsm');
      Add('.kar=audio/midi');
      Add('.m3u=audio/mpegurl');
      Add('.m4a=audio/x-mpg');
      Add('.mid=audio/midi');
      Add('.midi=audio/midi');
      Add('.mpega=audio/x-mpg');
      Add('.mp2=audio/x-mpg');
      Add('.mp3=audio/x-mpg');
      Add('.mpga=audio/x-mpg');
      Add('.m3u=audio/x-mpegurl');
      Add('.pls=audio/x-scpls');
      Add('.qcp=audio/vnd.qcelp');
      Add('.ra=audio/x-realaudio');
      Add('.ram=audio/x-pn-realaudio');
      Add('.rm=audio/x-pn-realaudio');
      Add('.sd2=audio/x-sd2');
      Add('.sid=audio/prs.sid');
      Add('.snd=audio/basic');
      Add('.wav=audio/x-wav');
      Add('.wax=audio/x-ms-wax');
      Add('.wma=audio/x-ms-wma');
      Add('.mjf=audio/x-vnd.AudioExplosion.MjuiceMediaFile');
      Add('.art=image/x-jg');
      Add('.bmp=image/bmp');
      Add('.cdr=image/x-coreldraw');
      Add('.cdt=image/x-coreldrawtemplate');
      Add('.cpt=image/x-corelphotopaint');
      Add('.djv=image/vnd.djvu');
      Add('.djvu=image/vnd.djvu');
      Add('.gif=image/gif');
      Add('.ief=image/ief');
      Add('.ico=image/x-icon');
      Add('.jng=image/x-jng');
      Add('.jpg=image/jpeg');
      Add('.jpeg=image/jpeg');
      Add('.jpe=image/jpeg');
      Add('.pat=image/x-coreldrawpattern');
      Add('.pcx=image/pcx');
      Add('.pbm=image/x-portable-bitmap');
      Add('.pgm=image/x-portable-graymap');
      Add('.pict=image/x-pict');
      Add('.png=image/x-png');
      Add('.pnm=image/x-portable-anymap');
      Add('.pntg=image/x-macpaint');
      Add('.ppm=image/x-portable-pixmap');
      Add('.psd=image/x-psd');
      Add('.qtif=image/x-quicktime');
      Add('.ras=image/x-cmu-raster');
      Add('.rf=image/vnd.rn-realflash');
      Add('.rgb=image/x-rgb');
      Add('.rp=image/vnd.rn-realpix');
      Add('.sgi=image/x-sgi');
      Add('.svg=image/svg-xml');
      Add('.svgz=image/svg-xml');
      Add('.targa=image/x-targa');
      Add('.tif=image/x-tiff');
      Add('.wbmp=image/vnd.wap.wbmp');
      Add('.webp=image/webp');
      Add('.xbm=image/xbm');
      Add('.xbm=image/x-xbitmap');
      Add('.xpm=image/x-xpixmap');
      Add('.xwd=image/x-xwindowdump');
      Add('.323=text/h323');
      Add('.xml=text/xml');
      Add('.uls=text/iuls');
      Add('.txt=text/plain');
      Add('.rtx=text/richtext');
      Add('.wsc=text/scriptlet');
      Add('.rt=text/vnd.rn-realtext');
      Add('.htt=text/webviewhtml');
      Add('.htc=text/x-component');
      Add('.vcf=text/x-vcard');
      Add('.asf=video/x-ms-asf');
      Add('.asx=video/x-ms-asf');
      Add('.avi=video/x-msvideo');
      Add('.dl=video/dl');
      Add('.dv=video/dv');
      Add('.flc=video/flc');
      Add('.fli=video/fli');
      Add('.gl=video/gl');
      Add('.lsf=video/x-la-asf');
      Add('.lsx=video/x-la-asf');
      Add('.mng=video/x-mng');
      Add('.mp2=video/mpeg');
      Add('.mp3=video/mpeg');
      Add('.mp4=video/mpeg');
      Add('.mpeg=video/x-mpeg2a');
      Add('.mpa=video/mpeg');
      Add('.mpe=video/mpeg');
      Add('.mpg=video/mpeg');
      Add('.ogv=video/ogg');
      Add('.moov=video/quicktime');
      Add('.mov=video/quicktime');
      Add('.mxu=video/vnd.mpegurl');
      Add('.qt=video/quicktime');
      Add('.qtc=video/x-qtc'); { Do not loccalize }
      Add('.rv=video/vnd.rn-realvideo');
      Add('.ivf=video/x-ivf');
      Add('.webm=video/webm');
      Add('.wm=video/x-ms-wm');
      Add('.wmp=video/x-ms-wmp');
      Add('.wmv=video/x-ms-wmv');
      Add('.wmx=video/x-ms-wmx');
      Add('.wvx=video/x-ms-wvx');
      Add('.rms=video/vnd.rn-realvideo-secure');
      Add('.asx=video/x-ms-asf-plugin');
      Add('.movie=video/x-sgi-movie');
      Add('.7z=application/x-7z-compressed');
      Add('.a=application/x-archive');
      Add('.aab=application/x-authorware-bin');
      Add('.aam=application/x-authorware-map');
      Add('.aas=application/x-authorware-seg');
      Add('.abw=application/x-abiword');
      Add('.ace=application/x-ace-compressed');
      Add('.ai=application/postscript');
      Add('.alz=application/x-alz-compressed');
      Add('.ani=application/x-navi-animation');
      Add('.arj=application/x-arj');
      Add('.asf=application/vnd.ms-asf');
      Add('.bat=application/x-msdos-program');
      Add('.bcpio=application/x-bcpio');
      Add('.boz=application/x-bzip2');
      Add('.bz=application/x-bzip');
      Add('.bz2=application/x-bzip2');
      Add('.cab=application/vnd.ms-cab-compressed');
      Add('.cat=application/vnd.ms-pki.seccat');
      Add('.ccn=application/x-cnc');
      Add('.cco=application/x-cocoa');
      Add('.cdf=application/x-cdf');
      Add('.cer=application/x-x509-ca-cert');
      Add('.chm=application/vnd.ms-htmlhelp');
      Add('.chrt=application/vnd.kde.kchart');
      Add('.cil=application/vnd.ms-artgalry');
      Add('.class=application/java-vm');
      Add('.com=application/x-msdos-program');
      Add('.clp=application/x-msclip');
      Add('.cpio=application/x-cpio');
      Add('.cpt=application/mac-compactpro');
      Add('.cqk=application/x-calquick');
      Add('.crd=application/x-mscardfile');
      Add('.crl=application/pkix-crl');
      Add('.csh=application/x-csh');
      Add('.dar=application/x-dar');
      Add('.dbf=application/x-dbase');
      Add('.dcr=application/x-director');
      Add('.deb=application/x-debian-package');
      Add('.dir=application/x-director');
      Add('.dist=vnd.apple.installer+xml');
      Add('.distz=vnd.apple.installer+xml');
      Add('.dll=application/x-msdos-program');
      Add('.dmg=application/x-apple-diskimage');
      Add('.doc=application/msword');
      Add('.dot=application/msword');
      Add('.dvi=application/x-dvi');
      Add('.dxr=application/x-director');
      Add('.ebk=application/x-expandedbook');
      Add('.eps=application/postscript');
      Add('.evy=application/envoy');
      Add('.exe=application/x-msdos-program');
      Add('.fdf=application/vnd.fdf');
      Add('.fif=application/fractals');
      Add('.flm=application/vnd.kde.kivio');
      Add('.fml=application/x-file-mirror-list');
      Add('.gzip=application/x-gzip');
      Add('.gnumeric=application/x-gnumeric');
      Add('.gtar=application/x-gtar');
      Add('.gz=application/x-gzip');
      Add('.hdf=application/x-hdf');
      Add('.hlp=application/winhlp');
      Add('.hpf=application/x-icq-hpf');
      Add('.hqx=application/mac-binhex40');
      Add('.hta=application/hta');
      Add('.ims=application/vnd.ms-ims');
      Add('.ins=application/x-internet-signup');
      Add('.iii=application/x-iphone');
      Add('.iso=application/x-iso9660-image');
      Add('.jar=application/java-archive');
      Add('.karbon=application/vnd.kde.karbon');
      Add('.kfo=application/vnd.kde.kformula');
      Add('.kon=application/vnd.kde.kontour');
      Add('.kpr=application/vnd.kde.kpresenter');
      Add('.kpt=application/vnd.kde.kpresenter');
      Add('.kwd=application/vnd.kde.kword');
      Add('.kwt=application/vnd.kde.kword');
      Add('.latex=application/x-latex');
      Add('.lha=application/x-lzh');
      Add('.lcc=application/fastman');
      Add('.lrm=application/vnd.ms-lrm');
      Add('.lz=application/x-lzip');
      Add('.lzh=application/x-lzh');
      Add('.lzma=application/x-lzma');
      Add('.lzo=application/x-lzop');
      Add('.lzx=application/x-lzx');
      Add('.m13=application/x-msmediaview');
      Add('.m14=application/x-msmediaview');
      Add('.mpp=application/vnd.ms-project');
      Add('.mvb=application/x-msmediaview');
      Add('.man=application/x-troff-man');
      Add('.mdb=application/x-msaccess');
      Add('.me=application/x-troff-me');
      Add('.ms=application/x-troff-ms');
      Add('.msi=application/x-msi');
      Add('.mpkg=vnd.apple.installer+xml');
      Add('.mny=application/x-msmoney');
      Add('.nix=application/x-mix-transfer');
      Add('.o=application/x-object');
      Add('.oda=application/oda');
      Add('.odb=application/vnd.oasis.opendocument.database');
      Add('.odc=application/vnd.oasis.opendocument.chart');
      Add('.odf=application/vnd.oasis.opendocument.formula');
      Add('.odg=application/vnd.oasis.opendocument.graphics');
      Add('.odi=application/vnd.oasis.opendocument.image');
      Add('.odm=application/vnd.oasis.opendocument.text-master');
      Add('.odp=application/vnd.oasis.opendocument.presentation');
      Add('.ods=application/vnd.oasis.opendocument.spreadsheet');
      Add('.ogg=application/ogg');
      Add('.odt=application/vnd.oasis.opendocument.text');
      Add('.otg=application/vnd.oasis.opendocument.graphics-template');
      Add('.oth=application/vnd.oasis.opendocument.text-web');
      Add('.otp=application/vnd.oasis.opendocument.presentation-template');
      Add('.ots=application/vnd.oasis.opendocument.spreadsheet-template');
      Add('.ott=application/vnd.oasis.opendocument.text-template');
      Add('.p10=application/pkcs10');
      Add('.p12=application/x-pkcs12');
      Add('.p7b=application/x-pkcs7-certificates');
      Add('.p7m=application/pkcs7-mime');
      Add('.p7r=application/x-pkcs7-certreqresp');
      Add('.p7s=application/pkcs7-signature');
      Add('.package=application/vnd.autopackage');
      Add('.pfr=application/font-tdpfr');
      Add('.pkg=vnd.apple.installer+xml');
      Add('.pdf=application/pdf');
      Add('.pko=application/vnd.ms-pki.pko');
      Add('.pl=application/x-perl');
      Add('.pnq=application/x-icq-pnq');
      Add('.pot=application/mspowerpoint');
      Add('.pps=application/mspowerpoint');
      Add('.ppt=application/mspowerpoint');
      Add('.ppz=application/mspowerpoint');
      Add('.ps=application/postscript');
      Add('.pub=application/x-mspublisher');
      Add('.qpw=application/x-quattropro');
      Add('.qtl=application/x-quicktimeplayer');
      Add('.rar=application/rar');
      Add('.rdf=application/rdf+xml');
      Add('.rjs=application/vnd.rn-realsystem-rjs');
      Add('.rm=application/vnd.rn-realmedia');
      Add('.rmf=application/vnd.rmf');
      Add('.rmp=application/vnd.rn-rn_music_package');
      Add('.rmx=application/vnd.rn-realsystem-rmx');
      Add('.rnx=application/vnd.rn-realplayer');
      Add('.rpm=application/x-redhat-package-manager');
      Add('.rsml=application/vnd.rn-rsml');
      Add('.rtsp=application/x-rtsp');
      Add('.rss=application/rss+xml');
      Add('.scm=application/x-icq-scm');
      Add('.ser=application/java-serialized-object');
      Add('.scd=application/x-msschedule');
      Add('.sda=application/vnd.stardivision.draw');
      Add('.sdc=application/vnd.stardivision.calc');
      Add('.sdd=application/vnd.stardivision.impress');
      Add('.sdp=application/x-sdp');
      Add('.setpay=application/set-payment-initiation');
      Add('.setreg=application/set-registration-initiation');
      Add('.sh=application/x-sh');
      Add('.shar=application/x-shar');
      Add('.shw=application/presentations');
      Add('.sit=application/x-stuffit');
      Add('.sitx=application/x-stuffitx');
      Add('.skd=application/x-koan');
      Add('.skm=application/x-koan');
      Add('.skp=application/x-koan');
      Add('.skt=application/x-koan');
      Add('.smf=application/vnd.stardivision.math');
      Add('.smi=application/smil');
      Add('.smil=application/smil');
      Add('.spl=application/futuresplash');
      Add('.ssm=application/streamingmedia');
      Add('.sst=application/vnd.ms-pki.certstore');
      Add('.stc=application/vnd.sun.xml.calc.template');
      Add('.std=application/vnd.sun.xml.draw.template');
      Add('.sti=application/vnd.sun.xml.impress.template');
      Add('.stl=application/vnd.ms-pki.stl');
      Add('.stw=application/vnd.sun.xml.writer.template');
      Add('.svi=application/softvision');
      Add('.sv4cpio=application/x-sv4cpio');
      Add('.sv4crc=application/x-sv4crc');
      Add('.swf=application/x-shockwave-flash');
      Add('.swf1=application/x-shockwave-flash');
      Add('.sxc=application/vnd.sun.xml.calc');
      Add('.sxi=application/vnd.sun.xml.impress');
      Add('.sxm=application/vnd.sun.xml.math');
      Add('.sxw=application/vnd.sun.xml.writer');
      Add('.sxg=application/vnd.sun.xml.writer.global');
      Add('.t=application/x-troff');
      Add('.tar=application/x-tar');
      Add('.tcl=application/x-tcl');
      Add('.tex=application/x-tex');
      Add('.texi=application/x-texinfo');
      Add('.texinfo=application/x-texinfo');
      Add('.tbz=application/x-bzip-compressed-tar');
      Add('.tbz2=application/x-bzip-compressed-tar');
      Add('.tgz=application/x-compressed-tar');
      Add('.tlz=application/x-lzma-compressed-tar');
      Add('.tr=application/x-troff');
      Add('.trm=application/x-msterminal');
      Add('.troff=application/x-troff');
      Add('.tsp=application/dsptype');
      Add('.torrent=application/x-bittorrent');
      Add('.ttz=application/t-time');
      Add('.txz=application/x-xz-compressed-tar');
      Add('.udeb=application/x-debian-package');
      Add('.uin=application/x-icq');
      Add('.urls=application/x-url-list');
      Add('.ustar=application/x-ustar');
      Add('.vcd=application/x-cdlink');
      Add('.vor=application/vnd.stardivision.writer');
      Add('.vsl=application/x-cnet-vsl');
      Add('.wcm=application/vnd.ms-works');
      Add('.wb1=application/x-quattropro');
      Add('.wb2=application/x-quattropro');
      Add('.wb3=application/x-quattropro');
      Add('.wdb=application/vnd.ms-works');
      Add('.wks=application/vnd.ms-works');
      Add('.wmd=application/x-ms-wmd');
      Add('.wms=application/x-ms-wms');
      Add('.wmz=application/x-ms-wmz');
      Add('.wp5=application/wordperfect5.1');
      Add('.wpd=application/wordperfect');
      Add('.wpl=application/vnd.ms-wpl');
      Add('.wps=application/vnd.ms-works');
      Add('.wri=application/x-mswrite');
      Add('.xfdf=application/vnd.adobe.xfdf');
      Add('.xls=application/x-msexcel');
      Add('.xlb=application/x-msexcel');
      Add('.xpi=application/x-xpinstall');
      Add('.xps=application/vnd.ms-xpsdocument');
      Add('.xsd=application/vnd.sun.xml.draw');
      Add('.xul=application/vnd.mozilla.xul+xml');
      Add('.z=application/x-compress');
      Add('.zoo=application/x-zoo');
      Add('.zip=application/x-zip-compressed');
      Add('.wbmp=image/vnd.wap.wbmp');
      Add('.wml=text/vnd.wap.wml');
      Add('.wmlc=application/vnd.wap.wmlc');
      Add('.wmls=text/vnd.wap.wmlscript');
      Add('.wmlsc=application/vnd.wap.wmlscriptc');
      Add('.asm=text/x-asm');
      Add('.p=text/x-pascal');
      Add('.pas=text/x-pascal');
      Add('.cs=text/x-csharp');
      Add('.c=text/x-csrc');
      Add('.c++=text/x-c++src');
      Add('.cpp=text/x-c++src');
      Add('.cxx=text/x-c++src');
      Add('.cc=text/x-c++src');
      Add('.h=text/x-chdr');
      Add('.h++=text/x-c++hdr');
      Add('.hpp=text/x-c++hdr');
      Add('.hxx=text/x-c++hdr');
      Add('.hh=text/x-c++hdr');
      Add('.java=text/x-java');
      Add('.css=text/css');
      Add('.js=text/javascript');
      Add('.htm=text/html');
      Add('.html=text/html');
      Add('.xhtml=application/xhtml+xml');
      Add('.xht=application/xhtml+xml');
      Add('.rdf=application/rdf+xml');
      Add('.rss=application/rss+xml');
      Add('.ls=text/javascript');
      Add('.mocha=text/javascript');
      Add('.shtml=server-parsed-html');
      Add('.xml=text/xml');
      Add('.sgm=text/sgml');
      Add('.sgml=text/sgml');
    end;

    Reg := TRegistry.Create;
    try
      KeyList := TStringList.create;
      try
        Reg.RootKey := HKEY_CLASSES_ROOT;
        if Reg.OpenKeyreadOnly('\MIME\Database\Content Type') then {do not localize}
        begin
          // get a list of registered MIME types
          KeyList.Clear;

          Reg.GetKeyNames(KeyList);
          for i := 0 to KeyList.Count - 1 do
          begin
            if Reg.OpenKeyreadOnly('\MIME\Database\Content Type\' + KeyList[i]) then {do not localize}
            begin
              s := reg.ReadString('Extension');  {do not localize}
              if (Length(s)>0) then
                MIMETypeCache.Values[s] := KeyList[i];
            end;
          end;
        end;

        if Reg.OpenKeyReadOnly('\') then  {do not localize}
        begin
          Reg.GetKeyNames(KeyList);
        end;
        // get a list of registered extentions
        for i := 0 to KeyList.Count - 1 do
        begin
          if Copy(KeyList[i], 1, 1) = '.' then   {do not localize}
          begin
            if reg.OpenKeyReadOnly('\' + KeyList[i]) then
            begin
              s := Reg.ReadString('Content Type');  {do not localize}
              if Length(s) > 0 then
              begin
                MIMETypeCache.Values[KeyList[i]] := s;
              end;
            end;
          end;
        end;
      finally
        KeyList.Free;
      end;
    finally
      reg.free;
    end;

  end;
  MTL.AddStrings(MIMETypeCache);
end;

function GetFileMIMETypeFromCache(const aFile: string): string;
var
  sFileExt: String;
  sResult: String;
  MT: TStringList;
begin
  sFileExt := LowerCase(ExtractFileExt(aFile));
  sResult := EmptyStr;
  if sFileExt<>EmptyStr then
  begin
    MT := TStringList.Create;
    try
      LoadMIMETypeCache(MT);
      sResult := MT.Values[sFileExt];
    finally
      FreeAndNil(MT);
    end;
  end;
  if sResult<>EmptyStr then
    Result := sResult
  else
    Result := 'application/octet-stream';
end;

{ TMultiPartDataStream }

procedure TMultipartFormDataStream.AddFieldBool(aFieldName: string; aValue: Boolean;
  mode:TBoolToStrMode);
var
  aValueStr:string;
begin
  case mode of
    btsNegative: aValueStr := {$IFDEF DXE2_OR_UP}System.{$ENDIF}SysUtils.BoolToStr(aValue, false);
    btsString: AValueStr := CatStrings.BoolToStr(aValue);
    btsStringLo: AValueStr := lowercase(CatStrings.BoolToStr(aValue));
    btsBinary: AValueStr := IntToStr(IIF(aValue,1,0));
    btsYN: AValueStr := BoolToYN(aValue);
    btsCustom: AValueStr := IIF(aValue, fTrueValue, fFalseValue);
  else
    aValueStr := emptystr;
  end;
  AddFieldString(aFieldName, aValueStr);
end;

procedure TMultipartFormDataStream.SetFieldBoolCustomValues(aTrueValue:string; aFalseValue:string);
begin
  fTrueValue:=aTrueValue;
  fFalseValue:=aFalseValue;
end;

procedure TMultipartFormDataStream.AddFieldBool(aFieldName: string; aValue: Boolean);
begin
  AddFieldBool(aFieldName, aValue, fDefaultBoolToStrMode);
end;

procedure TMultipartFormDataStream.AddFieldFloat(aFieldName: string; aValue: Extended);
begin
  AddFieldString(aFieldName, FloatToStr(aValue));
end;

procedure TMultipartFormDataStream.AddFieldInteger(aFieldName: string; aValue: Integer);
begin
  AddFieldString(aFieldName,
{$IFDEF DXE2_OR_UP}UIntToStr(aValue){$ELSE}IntToStr(aValue){$ENDIF});
end;

procedure TMultipartFormDataStream.AddFieldString(aFieldName, aValue: string;includeCT:boolean=true);
begin
  fStream.WriteString(crlf+'content-disposition: form-data; name="' + aFieldName + '"'+crlf);
  if includeCT = true then
  fStream.WriteString('Content-Type: text/plain'+crlf);
  fStream.WriteString(crlf);
  fStream.WriteString(aValue + crlf);
  fStream.WriteString(fBoundStr);
end;

procedure TMultipartFormDataStream.AddFile(aFieldName, aFileName: string; aFileContent: TStream);
begin
  fStream.WriteString(crlf+'content-disposition: form-data; name="' + aFieldName +
    '"; Filename="' + aFileName + '"'+ crlf);
  fStream.WriteString('Content-Type: ' + GetFileMIMEType(aFileName) + crlf + crlf);
  fStream.CopyFrom(aFileContent, 0);
  fStream.WriteString(crlf + fBoundStr);
end;

procedure TMultipartFormDataStream.AddFile(aFieldName, aFileName: string);
var
  MS: TMemoryStream;
begin
  fStream.WriteString(crlf+'content-disposition: form-data; name="' + aFieldName +
    '"; filename="' + ExtractFileName(aFileName) + '"'+crlf);
  fStream.WriteString('Content-Type: ' + GetFileMIMEType(aFileName) + crlf + crlf);
  //fStream.WriteString(crlf);
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(aFileName);
    fStream.CopyFrom(MS, 0);
  finally
    FreeAndNil(MS);
  end;
  fStream.WriteString(crlf + fBoundStr);
end;

constructor TMultipartFormDataStream.Create;
begin
  fDefaultBoolToStrMode := btsBinary;
  fUseMIMETypeCache := false;
  fTrueValue:='true';
  fFalseValue:='false';
{$IFDEF D2009_OR_UP}
  fStream := TStringStream.Create;
{$ELSE}
  fStream := TStringStream.Create(emptystr);
{$ENDIF}
  fBound := IntToHex(StrToInt64(FormatDateTime('ddmmyyyyhhmmssszzz', Now)), 8);
  fBoundStr := '--' + fBound;
  fStream.WriteString(fBoundStr);
  fClosed := False;
end;

procedure TMultipartFormDataStream.DataEnd;
begin
  fStream.WriteString('--'+crlf);
  fClosed := True;
end;

destructor TMultipartFormDataStream.Destroy;
begin
  FreeAndNil(fStream);
  inherited;
end;

function TMultipartFormDataStream.GetFileMIMEType(const aFile: string): string;
begin
  if fUseMIMETypeCache = false then
  Result := 'application/octet-stream' else
  Result := GetFileMIMETypeFromCache(aFile);
end;

function TMultipartFormDataStream.GetMimeProp: string;
begin
  Result := 'multipart/form-data; boundary=' + fBound;
end;

initialization

finalization
  if Assigned(MIMETypeCache) then
    try
      MIMETypeCache.Free;
    finally
      MIMETypeCache := nil;
    end;

end.
