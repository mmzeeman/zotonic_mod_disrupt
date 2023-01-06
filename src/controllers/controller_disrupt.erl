%% Copyright 2023 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(controller_disrupt).

-export([
    service_available/1, 
    resource_exists/1, 
    auth_required/1, 
    is_authorized/1,
    forbidden/1,
    upgrades_provided/1,
    allow_missing_post/1,
    malformed_request/1,
    uri_too_long/1,
    known_content_type/1,
    valid_content_headers/1,
    valid_entity_length/1,
    options/1,
    allowed_methods/1,
    known_methods/1,
    validate_content_checksum/1,
    content_types_provided/1,
    content_types_accepted/1,
    delete_resource/1,
    delete_completed/1,
    post_is_create/1,
    create_path/1,
    base_uri/1,
    process_post/1,
    language_available/1,
    charsets_provided/1,
    content_encodings_provided/1,
    transfer_encodings_provided/1,
    variances/1,
    is_conflict/1,
    multiple_choices/1,
    previously_existed/1,
    moved_permanently/1,
    moved_temporarily/1,
    last_modified/1,
    expires/1,
    generate_etag/1,
    finish_request/1,

    websocket_start/1, % Extra export needed for mqtt controller.

    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% Delegate callbacks to originial controller.
service_available(Context) -> delegate(?FUNCTION_NAME, Context).
resource_exists(Context) -> delegate(?FUNCTION_NAME, Context). 
auth_required(Context) -> delegate(?FUNCTION_NAME, Context). 
is_authorized(Context) -> delegate(?FUNCTION_NAME, Context). 
forbidden(Context) -> delegate(?FUNCTION_NAME, Context). 
upgrades_provided(Context) -> delegate(?FUNCTION_NAME, Context). 
allow_missing_post(Context) -> delegate(?FUNCTION_NAME, Context). 
malformed_request(Context) -> delegate(?FUNCTION_NAME, Context). 
uri_too_long(Context) -> delegate(?FUNCTION_NAME, Context). 
known_content_type(Context) -> delegate(?FUNCTION_NAME, Context). 
valid_content_headers(Context) -> delegate(?FUNCTION_NAME, Context). 
valid_entity_length(Context) -> delegate(?FUNCTION_NAME, Context). 
options(Context) -> delegate(?FUNCTION_NAME, Context). 
known_methods(Context) -> delegate(?FUNCTION_NAME, Context). 
validate_content_checksum(Context) -> delegate(?FUNCTION_NAME, Context). 
content_types_provided(Context) -> delegate(?FUNCTION_NAME, Context). 
content_types_accepted(Context) -> delegate(?FUNCTION_NAME, Context). 
delete_resource(Context) -> delegate(?FUNCTION_NAME, Context). 
delete_completed(Context) -> delegate(?FUNCTION_NAME, Context). 
post_is_create(Context) -> delegate(?FUNCTION_NAME, Context). 
create_path(Context) -> delegate(?FUNCTION_NAME, Context). 
base_uri(Context) -> delegate(?FUNCTION_NAME, Context). 
process_post(Context) -> delegate(?FUNCTION_NAME, Context). 
language_available(Context) -> delegate(?FUNCTION_NAME, Context). 
charsets_provided(Context) -> delegate(?FUNCTION_NAME, Context). 
content_encodings_provided(Context) -> delegate(?FUNCTION_NAME, Context). 
transfer_encodings_provided(Context) -> delegate(?FUNCTION_NAME, Context). 
variances(Context) -> delegate(?FUNCTION_NAME, Context). 
is_conflict(Context) -> delegate(?FUNCTION_NAME, Context). 
multiple_choices(Context) -> delegate(?FUNCTION_NAME, Context). 
previously_existed(Context) -> delegate(?FUNCTION_NAME, Context). 
moved_permanently(Context) -> delegate(?FUNCTION_NAME, Context). 
moved_temporarily(Context) -> delegate(?FUNCTION_NAME, Context). 
last_modified(Context) -> delegate(?FUNCTION_NAME, Context). 
expires(Context) -> delegate(?FUNCTION_NAME, Context). 
generate_etag(Context) -> delegate(?FUNCTION_NAME, Context). 
finish_request(Context) -> delegate(?FUNCTION_NAME, Context). 

% extra export needed for controller_mqtt_transport
websocket_start(Context) -> delegate(?FUNCTION_NAME, Context).

%% 
allowed_methods(Context) ->
    maybe_disrupt(?FUNCTION_NAME, Context). 

process(Method, AcceptedCT, ProvidedCT, Context) ->
    Module = z_context:get(delegate, Context),
    case erlang:function_exported(Module, process, 4) of
        true ->
            Module:process(Method, AcceptedCT, ProvidedCT, Context);
        false ->
            {true, Context}
    end.

maybe_disrupt(Function, Context) ->
    case z_context:get(disrupt_propability, Context, 100) of
        0 -> delegate(Function, Context);
        100 -> disrupt(Function, Context);
        P ->
            case rand:uniform(100) =< P of
                true ->
                    disrupt(Function, Context);
                false ->
                    delegate(Function, Context)
            end
    end.

disrupt(Function, Context) ->
    case z_context:get(disrupt_type, Context, latency) of
        throw ->
            throw({controller_disrupt, throw});
        latency ->
            timer:sleep(latency(Context)),
            delegate(Function, Context)
    end.

%%
%% Helpers
%%

latency(Context) ->
    case rand:normal(latency_mean(Context), latency_variance(Context)) of
        N when N < 0 -> 0;
        M -> z_convert:to_integer(M)
    end.

latency_mean(Context) ->
    m_config:get_value(mod_disrupt, latency_mean, 300, Context).

latency_variance(Context) ->
    m_config:get_value(mod_disrupt, latency_variance, 400, Context).

delegate(Function, Context) ->
    Module = z_context:get(delegate, Context),
    case erlang:function_exported(Module, Function, 1) of
        true ->
            Module:Function(Context);
        false ->
            {default(Function, Context), Context}
    end.

%%
%% Helpers taken from controller_cowmachine.
%%

%% @doc Get default value by Key.
-spec default(DefaultID, Context) -> Result when
	DefaultID:: service_available |	resource_exists |	auth_required |	is_authorized |	forbidden |	upgrades_provided |	allow_missing_post |	malformed_request |	uri_too_long |	known_content_type |	valid_content_headers |	valid_entity_length |	options |	allowed_methods |	known_methods |	validate_content_checksum |	content_types_provided |	content_types_accepted |	delete_resource |	delete_completed |	post_is_create |	create_path |	base_uri |	process_post |	language_available |	charsets_provided |	content_encodings_provided |	transfer_encodings_provided |	variances |	is_conflict |	multiple_choices |	previously_existed |	moved_permanently |	moved_temporarily |	last_modified |	expires |	generate_etag |	finish_request,
	Context :: cowmachine_req:context(),
	Result :: no_charset | no_default | undefined | boolean() | list(binary()).
default(service_available, _Context) ->
    true;
default(resource_exists, _Context) ->
    true;
default(auth_required, _Context) ->
    true;
default(is_authorized, _Context) ->
    true;
default(forbidden, _Context) ->
    false;
default(upgrades_provided, _Context) ->
    [];
default(allow_missing_post, _Context) ->
    false;
default(malformed_request, _Context) ->
    false;
default(uri_too_long, _Context) ->
    false;
default(known_content_type, _Context) ->
    true;
default(valid_content_headers, _Context) ->
    true;
default(valid_entity_length, _Context) ->
    true;
default(options, _Context) ->
    [];
default(allowed_methods, _Context) ->
    [ <<"GET">>, <<"HEAD">> ];
default(known_methods, _Context) ->
    [ <<"GET">>, <<"HEAD">>,
      <<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>,
      <<"TRACE">>, <<"CONNECT">>, <<"OPTIONS">> ];
default(validate_content_checksum, _Context) ->
    not_validated;
default(content_types_provided, _Context) ->
    [ {<<"text">>, <<"html">>, []} ];
default(content_types_accepted, _Context) ->
    [];
default(delete_resource, _Context) ->
    false;
default(delete_completed, _Context) ->
    true;
default(post_is_create, _Context) ->
    false;
default(create_path, _Context) ->
    undefined;
default(base_uri, _Context) ->
    undefined;
default(process_post, _Context) ->
    false;
default(language_available, _Context) ->
    true;

% The default setting is needed for non-charset responses such as image/png
% An example of how one might do actual negotiation:
%    [ <<"iso-8859-1">>, <<"utf-8">> ];
default(charsets_provided, Context) ->
    case is_text( cowmachine_req:resp_content_type(Context) ) of
        true -> [ <<"utf-8">> ];
        false -> no_charset
    end;

% The content variations available to the controller.
default(content_encodings_provided, _Context) ->
    [<<"identity">>];

% How the content is transferred, this is handy for auto-gzip of GET-only resources.
% "identity" and "chunked" are always available to HTTP/1.1 clients.
% Example:
%    [{"gzip", fun(X) -> zlib:gzip(X) end}];
default(transfer_encodings_provided, _Context) ->
    [];

default(variances, _Context) ->
    [];
default(is_conflict, _Context) ->
    false;
default(multiple_choices, _Context) ->
    false;
default(previously_existed, _Context) ->
    false;
default(moved_permanently, _Context) ->
    false;
default(moved_temporarily, _Context) ->
    false;
default(last_modified, _Context) ->
    undefined;
default(expires, _Context) ->
    undefined;
default(generate_etag, _Context) ->
    undefined;
default(finish_request, _Context) ->
    true;
default(_, _Context) ->
    no_default.


-spec is_text(ContentType) -> Result when
	ContentType :: cow_http_hd:media_type(),
	Result :: boolean().
is_text({<<"text">>, _, _}) -> true;
is_text({<<"application">>, <<"x-javascript">>, _}) -> true;
is_text({<<"application">>, <<"javascript">>, _}) -> true;
is_text({<<"application">>, <<"xhtml+xml">>, _}) -> true;
is_text({<<"application">>, <<"xml">>, _}) -> true;
is_text(_Mime) -> false.

