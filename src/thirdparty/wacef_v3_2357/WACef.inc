 (*
 *                       WaspAce Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : WaspAce.Architector <Architector.Waspace@gmail.com>
 * Web site   : http://www.waspace.net
 * Repository : https://bitbucket.org/WaspAce/wacef
 * Group      : http://groups.google.com/group/wacef
 *
 * The code of WACEF is based on DCEF3 code: http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

{$MINENUMSIZE 4}

{.$DEFINE CEF_STRING_TYPE_UTF8}
{$DEFINE CEF_STRING_TYPE_UTF16}
{.$DEFINE CEF_STRING_TYPE_WIDE}

{$DEFINE SUPPORTS_INLINE}

{$IFDEF MSWINDOWS}
  {.$DEFINE CEF_MULTI_THREADED_MESSAGE_LOOP}
{$ENDIF}
