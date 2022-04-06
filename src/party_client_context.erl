-module(party_client_context).

-export([create/1]).
-export([get_woody_context/1]).
-export([set_woody_context/2]).

-opaque context() :: #{
    woody_context := woody_context()
}.

-type options() :: #{
    woody_context => woody_context()
}.

-export_type([context/0]).
-export_type([options/0]).

%% Internal types

-type woody_context() :: woody_context:ctx().

%% API

-spec create(options()) -> context().
create(Options) ->
    ensure_woody_context_exists(Options).

-spec get_woody_context(context()) -> woody_context().
get_woody_context(#{woody_context := WoodyContext}) ->
    WoodyContext.

-spec set_woody_context(woody_context(), context()) -> context().
set_woody_context(WoodyContext, Context) ->
    Context#{woody_context => WoodyContext}.

%% Internal functions

-spec ensure_woody_context_exists(options()) -> options().
ensure_woody_context_exists(#{woody_context := _WoodyContext} = Options) ->
    Options;
ensure_woody_context_exists(Options) ->
    Options#{woody_context => woody_context:new()}.
