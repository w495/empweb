-module(empweb_norm_doc).

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


%%
%% @doc Общие функции нормировки для всех документов
%%
norm('get') ->
    [
        #norm_rule{
            key = id,
            required = false,
            types = empweb_norm:filter([integer])
        },
        #norm_rule{
            rules       =   norm([]),
            required    =   false
        },
        #norm_rule{
            key = owner_id,
            required = false,
            types = empweb_norm:filter([nullable, allable, integer])
        }
        | empweb_norm:norm('get')
    ];

norm('create') ->
    [
        #norm_rule{
            key         = position,
            required    = false,
            types       = [integer]
        },
        #norm_rule{
            rules       =   norm([]),
            required    =   false
        },
        #norm_rule{
            key         = oktype_id,
            required    = false,
            types       = [integer]
        }
        | empweb_norm:norm('create')
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
        %% | empweb_norm:norm('update')
        %%
    ];


norm('delete') ->
    [
        #norm_rule{
            key         = id,
            required    = false,
            types       = [integer]
        }
        %%
        %% | empweb_norm:norm('delete')
        %%
    ];

norm([])->
    [
        #norm_rule{
            key         = head,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
        #norm_rule{
            key         = body,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
        #norm_rule{
            key         = parent_id,
            required    = false,
            types       = empweb_norm:filter([integer])
        },
        #norm_rule{
            key         = read_acctype_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = read_acctype_alias,
            required    = false,
            types       = empweb_norm:filter([nullable, atom])
        },
        #norm_rule{
            key         = comm_acctype_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = comm_acctype_alias,
            required    = false,
            types       = empweb_norm:filter([nullable, atom])
        },
        #norm_rule{
            key         = doctype_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = doctype_alias,
            required    = false,
            types       = empweb_norm:filter([nullable, atom])
        },
        #norm_rule{
            key         = oktype_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = oktype_alias,
            required    = false,
            types       = empweb_norm:filter([nullable, atom])
        },
        #norm_rule{
            key         = contype_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = contype_alias,
            required    = false,
            types       = empweb_norm:filter([nullable, atom])
        },
        #norm_rule{
            key         = nviews,
            required    = false,
            types       = empweb_norm:filter([float])
        },
        #norm_rule{
            key         = nvotes,
            required    = false,
            types       = empweb_norm:filter([float])
        }
        %%
        %% | empweb_norm:norm([])
        %%
    ];

norm(_) ->
    %%
    %% empweb_norm:norm(_)
    %%s
    [].

