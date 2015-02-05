% Main file to load the pengines demo.  This file is included from
%
%   - debug.pl for local debugging
%   - daemon.pl to run pengines as a Unix service

:- use_module(library(pengines)).
:- use_module(library(http/http_error)).
:- use_module(server).
:- use_module(storage).

:- use_module(lib/admin/admin).
:- use_module(lib/admin/server_statistics).
:- use_module(lib/admin/change_passwd).

:- if(exists_source(apps(swish/swish))).
:- multifile http:location/3.
http:location(swish, apps(swish), [priority(10)]).
:- use_module(apps(swish/swish)).
:- endif.

% access at /apps/genealogist/index.html
:- if(exists_source(apps(genealogist/app))).
:- use_module(apps(genealogist/app)).
:- endif.

