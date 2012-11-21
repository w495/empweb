-module(empweb_norm_opt).

-include("empweb.hrl").

-include_lib("norm/include/norm.hrl").

%%
%% Описание записей событий и макросов
%%
-include_lib("evman/include/events.hrl").


%%
%% Трансформация для получения имени функции.
%%
-include_lib("evman/include/evman_transform.hrl").


-export([
    norm/1
]).

norm('get') ->
    [
        #norm_rule{
            key = alias,
            required = false,
            types = [string]
        },
        #norm_rule{
            key = id,
            required = false,
            types = [any]
        }
        |empweb_norm:norm('get')
    ];

norm('create') ->
    [
        #norm_rule{
            key         = alias,
            required    = false,
            types       = [string]
        },
        #norm_rule{
            key         = name_ti,
            required    = false,
            types       = [integer]
        }
        |empweb_norm:norm('create')
    ];

norm('update') ->
    [
        #norm_rule{
            key         = id,
            required    = false,
            types       = [integer]
        }
        |norm('create')
        %% 
        %% |empweb_norm:norm('create')
        %%
    ];

norm('delete') ->
    %%
    %% | empweb_norm:norm('delete')
    %%
    norm('update');

norm([]) ->
    %%
    %% | empweb_norm:norm([])
    %%
    [];

norm(_) ->
    %%
    %% | empweb_norm:norm(_)
    %%
    [].
