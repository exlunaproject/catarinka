-- This script performs some simple tests to make sure that functions
-- are working adequately
package.path = package.path..";R:/Win64/Lib/lua/?.lua"
package.cpath = package.cpath..";R:/Win64/Lib/clibs/?.dll"

local ctk = require "Catarinka"

local res = {
  passed = 0,
  failed = 0,
  total = 0
}

local str = {
  html = '<html><b>Demo</b></html>',
  html_escaped = '&lt;html&gt;&lt;b&gt;Demo&lt;/b&gt;&lt;/html&gt;',
  link_lua = '<a href="http://www.lua.org">http://www.lua.org</a>',
  url_re = [[((https?|ftp):((//)|(\\\\))+[\w\d:#@%/;$()~_?\+-=\\\.&]*)]]
}

function test(name,expected,got)
  res.total = res.total + 1
  if got == expected then
   res.passed = res.passed + 1
  else
   ctk.cs.printred(name..' failed.')
   ctk.cs.printred('  expected: "'..expected..'", got: "'..got..'"')
   res.failed = res.failed + 1
  end
end

function runtests()
  -- String Matching
  test('string.beginswith (1)', true,  ctk.string.beginswith('Selenite','Selen'))
  test('string.beginswith (2)', false, ctk.string.beginswith('Selenite','Anything'))
  test('string.ishex (1)', true,  ctk.string.ishex('ABAD1DEA'))
  test('string.ishex (2)', false, ctk.string.ishex('ABADIDEA'))
  test('string.isint (1)', true,  ctk.string.isint('123'))
  test('string.isint (2)', false, ctk.string.isint('I23'))
  test('string.endswith (1)', true,  ctk.string.endswith('Selenite','nite'))
  test('string.endswith (2)', false, ctk.string.endswith('Selenite','nit'))
  test('string.match (1)', true,  ctk.string.match('Selenite','????nite'))
  test('string.match (2)', false, ctk.string.match('Selenite','????nita'))
  test('string.match (3)', true,  ctk.string.match('Selenite','S*ni?e'))
  
  -- Basic String Operations
  test('string.after', 'City', ctk.string.after('SaltLakeCity','Lake'))
  test('string.before', 'Salt', ctk.string.before('SaltLakeCity','Lake'))
  test('string.between', 'Lake', ctk.string.between('SaltLakeCity','Salt','City'))
  test('string.gettoken', 'two', ctk.string.gettoken('one;two;three',';',2))
  test('string.lastchar', ':', ctk.string.lastchar('http:'))
  test('string.occur (1)', 3, ctk.string.occur('Selenite','e'))
  test('string.occur (2)', 2, ctk.string.occur('boohoo','oo'))
  test('string.random', 8, string.len(ctk.string.random(8)))
  test('string.replace', 'newstring',ctk.string.replace('somestring','some','new'))
  test('string.stripquotes', 'selenite',ctk.string.stripquotes('"selenite"'))
  test('string.trim', 'selenite',ctk.string.trim(' selenite '))
  
  -- Regular Expression
  test('re.find', '<b>Demo</b>, ', ctk.re.find(str.html,'<b>(.*?)</b>'))
  test('re.match (1)', true, ctk.re.match(str.html,'<b>(.*?)</b>'))
  test('re.match (2)', false, ctk.re.match(str.html,'<b>S(.*?)</b>'))
  test('re.replace', str.link_lua, ctk.re.replace('http://www.lua.org',str.url_re,'<a href="$1">$1</a>'))
  
  -- HTML Functions
  test('html.escape', str.html_escaped,ctk.html.escape(str.html))
  test('html.gettagcontents', 'Demo',ctk.html.gettagcontents(str.html,'b'))
  test('html.striptags', 'Demo',ctk.html.striptags(str.html))
  test('html.unescape', str.html,ctk.html.unescape(str.html_escaped))
  
  -- URL functions
  test('url.changepath', 'http://lua.org/demo/index.lp',ctk.url.changepath('http://lua.org/index.lp','/demo/index.lp'))
  test('url.combine', 'http://lua.org/demo/test.lp',ctk.url.combine('http://lua.org/demo/index.lp','test.lp'))
  test('url.encode', 'download.html%3Ftest%3D1',ctk.url.encode('download.html?test=1'))
  test('url.decode', 'download.html',ctk.url.decode('download%2Ehtml'))
  test('url.encodefull', '%64%6F%77%6E%6C%6F%61%64%2E%68%74%6D%6C',ctk.url.encodefull('download.html'))
  test('url.genfromhost (1)', 'http://www.lua.org',ctk.url.genfromhost('www.lua.org',80))
  test('url.genfromhost (2)', 'https://www.lua.org',ctk.url.genfromhost('www.lua.org',443))
  test('url.genfromhost (3)', 'http://www.lua.org:8080',ctk.url.genfromhost('www.lua.org',8080))
  test('url.getfileext', '.lp',ctk.url.getfileext('http://host/path/index.lp'))
  test('url.getfilename', 'index.lp',ctk.url.getfilename('http://host/path/index.lp'))
  
  local urlparts = ctk.url.crack('http://lua.org/demo/index.lp')
  test('url.crack (fileext)', '.lp', urlparts.fileext)
  test('url.crack (filename)', 'index.lp', urlparts.filename)
  test('url.crack (host)', 'lua.org', urlparts.host)
  test('url.crack (path)', 'demo/index.lp', urlparts.path)
  test('url.crack (port)', 80, urlparts.port)
  test('url.crack (proto)', 'http', urlparts.proto)
  
  -- Net functions
  test('net.iptoname', 'one.one.one.one', ctk.net.iptoname('1.1.1.1'))
  test('net.nametoip', '127.0.0.1', ctk.net.nametoip('localhost'))
  
  -- HTTP functions
  local req = [[GET /index.php HTTP/1.1
Host: www.example.org

test=1
]]
  test('http.crackrequest (path)', '/index.php',ctk.http.crackrequest(req).path)
  test('http.crackrequest (method)', 'GET',ctk.http.crackrequest(req).method)
  test('http.crackrequest (data)', 'test=1',ctk.http.crackrequest(req).data)
  
  local resp = [[HTTP/1.1 200 OK
Date: Mon, 23 May 2005 22:38:34 GMT
Server: Apache/1.3.3.7 (Unix) (Red-Hat/Linux)]]
  test('http.getheader', 'Apache/1.3.3.7 (Unix) (Red-Hat/Linux)',ctk.http.getheader(resp,'Server'))
  
  -- Base64 functions
  test('base64.encode', 'c2VsZW5pdGU=',ctk.base64.encode('selenite'))
  test('base64.decode', 'selenite',ctk.base64.decode('c2VsZW5pdGU='))
  
  -- Conversion functions
  -- test('Ana\nRoberto\nMaria Clara',ctk.convert.commastrtostr('Ana,Roberto,"Maria Clara"'))
  test('convert.strtoalphanum', 'astring2',ctk.convert.strtoalphanum('astring.2'))
  test('convert.strtocomma', 'Ana,Roberto,"Maria Clara"',ctk.convert.strtocomma('Ana\nRoberto\nMaria Clara'))
  test('convert.strtohex', '73656C656E697465',ctk.convert.strtohex('selenite'))
  test('convert.hextoint', 2013,ctk.convert.hextoint('7DD'))
  test('convert.hextostr', 'selenite',ctk.convert.hextostr('73656C656E697465'))
  
  -- Crypto functions
  test('crypto.md5', 'b97913643bdf6aa8a998f586ab044619',ctk.crypto.md5('Selenite'))
  test('crypto.sha1', '4955a5fa1ed51ba2cba0619af59f8952e4c60c1d',ctk.crypto.sha1('Selenite'))
  
  -- File functions
  local calc_exe = [[C:\Windows\System32\calc.exe]]
  test('file.getext', '.exe',ctk.file.getext('explorer.exe'))
  test('file.getname', 'calc.exe',ctk.file.getname(calc_exe))
  test('file.exists', true,ctk.file.exists(calc_exe))
  
  -- Task functions
  test('task.isrunning (explorer)', true,ctk.task.isrunning('explorer.exe'))
  test('task.isrunning (invalid)', false,ctk.task.isrunning('invalid.exe'))
  
  -- Version matching and comparison functions
  test('string.comparever1', 0, ctk.string.comparever('1', '1'))
  test('string.comparever2', 0, ctk.string.comparever('1.2', '1.2'))
  test('string.comparever3', 0, ctk.string.comparever('1.2.3', '1.2.3'))
  test('string.comparever4', -1, ctk.string.comparever('1', '2'))
  test('string.comparever5', -1, ctk.string.comparever('1', '3'))
  test('string.comparever6', -1, ctk.string.comparever('2', '3'))
  test('string.comparever7', 1, ctk.string.comparever('2', '1'))
  test('string.comparever8', 1, ctk.string.comparever('3', '1'))
  test('string.comparever9', 1, ctk.string.comparever('3', '2'))
  test('string.comparever10', -1, ctk.string.comparever('1', '12'))
  test('string.comparever11', 1, ctk.string.comparever('12', '2'))
  test('string.comparever12', 0, ctk.string.comparever('12', '12'))
  test('string.comparever13', -1, ctk.string.comparever('1.2', '1.3'))
  test('string.comparever14', 1, ctk.string.comparever('1.3', '1.2'))
  test('string.comparever15', -1, ctk.string.comparever('1.2', '11.2'))
  test('string.comparever16', 1, ctk.string.comparever('11.2', '1.2'))
  test('string.comparever17', -1, ctk.string.comparever('1.1', '1.12'))
  test('string.comparever18', -1, ctk.string.comparever('1.11', '1.12'))
  test('string.comparever19', 1, ctk.string.comparever('1.12', '1.1'))
  test('string.comparever20', 1, ctk.string.comparever('1.12', '1.11'))
  test('string.comparever21', 0, ctk.string.comparever('1', '1.0'))
  test('string.comparever22', 0, ctk.string.comparever('1.0', '1'))
  test('string.comparever23', -1, ctk.string.comparever('1', '1.1'))
  test('string.comparever24', 1, ctk.string.comparever('1.1', '1'))
  test('string.comparever25', -1, ctk.string.comparever('1.12.123', '1.12.124'))
  test('string.comparever26', 1, ctk.string.comparever('1.12.124', '1.12.123'))
  test('string.comparever27', -1, ctk.string.comparever('1.12.123', '1.13.1'))
  test('string.comparever28', 1, ctk.string.comparever('1.13.1', '1.12.123'))
  test('string.comparever29', -1, ctk.string.comparever('1.12.123', '1.13'))
  test('string.comparever30', 1, ctk.string.comparever('1.13', '1.12.123'))
  test('string.comparever31', -1, ctk.string.comparever('1.12', '1.13.1'))
  test('string.comparever32', 1, ctk.string.comparever('1.13.1', '1.12'))
  
  test('string.comparever33', -1, ctk.string.comparever('0.20.7', '0.20.8'))
  test('string.comparever34', 1, ctk.string.comparever('0.20.9', '0.20.8'))
  test('string.comparever35', 0, ctk.string.comparever('0.20.08', '0.20.8'))
  test('string.comparever36', -1, ctk.string.comparever('0.20.08', '0.20.8.1'))
  test('string.comparever37', 1, ctk.string.comparever('0.20.8.1', '0.20.8'))
  test('string.comparever38', 0, ctk.string.comparever('0.020', '0.20'))
  test('string.comparever39', -1, ctk.string.comparever(0.1, 0.2))
  test('string.comparever40', 0, ctk.string.comparever('0', '0'))
  
  test('string.comparever41', 0, ctk.string.comparever('1.0', 'v1.0'))
  test('string.comparever42', -1, ctk.string.comparever('1.0', 'v1.1'))  
  test('string.comparever43', 1, ctk.string.comparever('v1.1', '1.0'))    
  test('string.comparever44', 1, ctk.string.comparever('v1.1', 'v1.0'))   
  test('string.comparever45', -1, ctk.string.comparever('1.0 RC1', '1.0'))    
  test('string.comparever46', 1, ctk.string.comparever('1.0 RC3', '1.0 RC2')) 
  test('string.comparever47', 0, ctk.string.comparever('1.0 RC3', '1.0 RC3'))    
  test('string.comparever48', 0, ctk.string.comparever('1.0a', '1.0a'))       
  test('string.comparever49', 1, ctk.string.comparever('1.0b', '1.0a')) 
  test('string.comparever50', -1, ctk.string.comparever('1.0a', '1.0b'))      
  test('string.comparever51', -1, ctk.string.comparever('1.0a', '1.0B'))    
  test('string.comparever52', -1, ctk.string.comparever('1.0-beta1', '1.0-beta2'))      
  test('string.comparever53', 1, ctk.string.comparever('1.0', '1.0-beta2'))      
  test('string.comparever54', 1, ctk.string.comparever('1.0-beta1', '1.0-alpha1'))      
  test('string.comparever55', 1, ctk.string.comparever('1.0-rc1', '1.0-alpha1'))  
  test('string.comparever56', 1, ctk.string.comparever('1.0', '1.0-pre1'))       
  test('string.comparever57', 1, ctk.string.comparever('1.0-alpha1', '1.0-prealpha1'))
  test('string.comparever58', -1, ctk.string.comparever('1.0', '1.0a'))  
  test('string.comparever59', 0, ctk.string.comparever('1.0a', '1.0a'))    
  test('string.comparever60', 1, ctk.string.comparever('1.0a', '1.0'))     
  test('string.comparever61', 1, ctk.string.comparever('1.0b', '1.0'))    
  test('string.comparever62', 1, ctk.string.comparever('1.0b', '1.0a'))    
  test('string.comparever63', -1, ctk.string.comparever('1.0beta1', '1.0b'))     
  test('string.comparever64', -1, ctk.string.comparever('1.0beta1', '1.0c'))      
  test('string.comparever65', 1, ctk.string.comparever('1.0c', '1.0alpha1'))    
  
  test('string.matchver1', true, ctk.string.matchver('1.0','1.0'))  
  test('string.matchver2', false, ctk.string.matchver('2.0','1.0'))    
  test('string.matchver3', true, ctk.string.matchver('v1.0','1.0'))      
  test('string.matchver4', false, ctk.string.matchver('1.0','1.0-beta1'))   
  test('string.matchver5', false, ctk.string.matchver('1.0a','1.0b'))    
  test('string.matchver6', true, ctk.string.matchver('1.0a','1.0a')) 
  test('string.matchver7', false, ctk.string.matchver('1.0a','1.0'))      
  test('string.matchver8', false, ctk.string.matchver('1.0','1.0a'))    
  
  test('string.matchverpat1', true, ctk.string.matchverpat('1.0','=1.0'))
  test('string.matchverpat2', true, ctk.string.matchverpat('1.0','>=1.0'))
  test('string.matchverpat3', true, ctk.string.matchverpat('1.1','>=1.0'))  
  test('string.matchverpat4', false, ctk.string.matchverpat('0.9','>=1.0'))    
  test('string.matchverpat5', false, ctk.string.matchverpat('2.0','>=1.0 <2.0'))     
  test('string.matchverpat6', true, ctk.string.matchverpat('1.9','>=1.0 <2.0'))    
  test('string.matchverpat7', true, ctk.string.matchverpat('1.9','<2.0'))    
  test('string.matchverpat8', false, ctk.string.matchverpat('2.0','<1.9'))   
  test('string.matchverpat9', true, ctk.string.matchverpat('2.0','>1.9'))    
  test('string.matchverpat10', true, ctk.string.matchverpat('1.9','>=1.0 <2.0,=3.5'))   
  test('string.matchverpat11', true, ctk.string.matchverpat('3.5','>=1.0 <2.0,=3.5'))       
  test('string.matchverpat12', true, ctk.string.matchverpat('3.5','>=1.0 <2.0,>3.1 <=3.5'))     
  test('string.matchverpat13', false, ctk.string.matchverpat('3.0','>=1.0 <2.0,>3.1 <=3.5'))   
  
  -- JSON object
  local j1 = ctk.json.object:new()
  local j2 = ctk.json.object:new()
  j1.test = 'test'
  j2.name = 'somestring'
  j2.status = true
  j2.year = 2014
  test('json.object 1 (string)', 'test',j1.test)
  test('json.object 2 (string)', 'somestring',j2.name)
  test('json.object 2 (boolean)', true, j2.status)
  test('json.object 2 (integer)', 2014, j2.year)
  j1:release()
  j2:release()
end

function printresults()
  local p = ctk.cs.printgreen
  if res.total ~= res.passed then
    p = ctk.cs.printred
  end
  p(res.total..' tests performed.')
  p(res.passed..' passed.')
  p(res.failed..' failed.')
end

print('Starting tests...')
runtests()
printresults()
print('Done.')