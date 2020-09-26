-- This script performs some simple tests to make sure that functions
-- are working adequately
package.path = package.path..";C:/Hybrid/Win32/Lib/lua/?.lua"
package.cpath = package.cpath..";C:/Hybrid/Win32/Lib/clibs/?.dll"

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
  test('url.encode', 'download%2Ehtml',ctk.url.encode('download.html'))
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
  test('net.iptoname', 'localhost', ctk.net.iptoname('127.0.0.1'))
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