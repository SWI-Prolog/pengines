% This system may only distributed using the GNU General Public License
% because the following components contain GPL-ed code:
%
%     /opt/local/lib/swipl-6.3.15/library/mime.pl
%     GNU Readline library
%
% See http://www.swi-prolog.org/license.html for details on
% SWI-Prolog licensing policies supporting both free and non-free
% Software.

:- use_module(library(pengines)).
:- use_module(library(http/http_error)).
:- use_module(server).
:- use_module(storage).

:- use_module(lib/admin/admin).
:- use_module(lib/admin/server_statistics).
:- use_module(lib/admin/change_passwd).

:- pengine_application(swish).
:- use_module(swish:apps/swish/swish).

:- prolog_ide(thread_monitor).

:- server(3020).
