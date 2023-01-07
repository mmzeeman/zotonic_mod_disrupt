%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2023 Maas-Maarten Zeeman
%% @doc Disrupt. Creates disorder in the system to improve robustness. 

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

-module(mod_disrupt).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Disrupt").
-mod_description("Creates disrupt in the system, like randomly killing things.").
-mod_provides([disrupt]).
-mod_depends([admin]).
-mod_prio(300).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    init/1,
    observe_m_config_update/2
]).

init(Context) ->
    maybe_enable_disruptor(Context),
    ok.

observe_m_config_update(#m_config_update{module = <<"mod_disrupt">>, key = <<"disruptor">>}, Context) ->
    Context1 = case maybe_enable_disruptor(Context) of
                   true ->
                       z_render:growl(?__("Disruptor enabled.", Context), Context);
                   false ->
                       z_render:growl(?__("Disruptor disabled.", Context), Context)
               end,
    Script = iolist_to_binary( z_render:get_script(Context1) ),
    z_mqtt:publish([ <<"~client">>, <<"zotonic-transport">>, <<"eval">> ], Script, Context);

observe_m_config_update(#m_config_update{}, _Context) ->
    undefined.
    
maybe_enable_disruptor(Context) ->
    Enable = m_config:get_boolean(mod_disrupt, disruptor, Context),
    activate_disruptor(Enable),
    Enable.

activate_disruptor(Enable) ->
    case {Enable, havoc:is_active()} of
        {true, false} -> havoc:on();
        {false, true} -> havoc:off();
        {true, true} -> ok;
        {false, false} -> ok 
    end.

