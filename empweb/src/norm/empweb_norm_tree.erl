-module(empweb_norm_tree).

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
            key         = parent_id,
            required    = false,
            types       = [nullable, integer]
        },
        #norm_rule{
            key         = nchildtargets,
            required    = false,
            types       = [nullable, integer]
        },
        #norm_rule{
            key         = nnodetargets,
            required    = false,
            types       = [nullable, integer]
        },
        #norm_rule{
            key         = nchildren,
            required    = false,
            types       = [nullable, integer]
        },
        #norm_rule{
            key         = nnodes,
            required    = false,
            types       = [nullable, integer]
        }
        |empweb_norm_opt:norm('get')
    ];

norm('create') ->
    [
        #norm_rule{
            key         = parent_id,
            required    = false,
            types       = [nullable, integer]
        }
        |empweb_norm_opt:norm('create')
    ];

norm('update') ->
    [
        #norm_rule{
            key         = parent_id,
            required    = false,
            types       = [nullable, integer]
        }
        |empweb_norm_opt:norm('update')
    ];

norm('delete') ->
    [
        #norm_rule{
            key         = parent_id,
            required    = false,
            types       = [nullable, integer]
        }
        |empweb_norm_opt:norm('delete')
    ];

norm(X) ->
    empweb_norm_opt:norm(X).

