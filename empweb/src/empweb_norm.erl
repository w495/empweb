-module(empweb_norm).

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
    norm/1,
    normlist/1,
    filter_owner/1
]).


%%
%% @spec filter_owner([{atom(), any()}]) -> [{atom(), any()}]
%%
%% @doc Фильтрует набор аргументов, на предмет того, кто является владельем
%%      документа. Если нет параметра owner_id, но есть параметр pers_id,
%%      то тогда, owner_id принимает значение pers_id.
%%      В данном случае owner_id указывается, если мы хотим узнать
%%      докуметы конкретного пользователя, pers_id --- список своих документов.
%%
filter_owner(Params) ->
    case {
        proplists:get_value(owner_id, Params),
        proplists:get_value(pers_id, Params)
    } of
        {all     , _      } ->
            proplists:delete(pers_id, proplists:delete(owner_id, Params));
        {undefined, Pers_id} ->
            [{owner_id, Pers_id}|proplists:delete(pers_id, Params)];
        {_owner_id, _      } ->
            proplists:delete(pers_id, Params)
    end.
    
%%
%%
%%
norm('get') ->
    [
        #norm_rule{
            key         = order,
            nkey        = order,
            required    = false,
            types       = [normpair([atom]), normlist([normpair([atom])])]
        },
        #norm_rule{
            key         = limit,
            nkey        = limit,
            required    = false,
            types       = [nullable, integer]
        },
        #norm_rule{
            key         = offset,
            nkey        = offset,
            required    = false,
            types       = [nullable, integer]
        },
        #norm_rule{
            key         = fields,
            nkey        = fields,
            required    = false,
            types       = [normlist([atom])]
        }
    ];

norm('create') ->
    [];

norm('update') ->
    norm('create');

norm('delete') ->
    [];

norm([]) ->
    [];

norm(_) ->
    [].

normlist(Types)->
    fun
        (null) ->
            [];
        (List) when erlang:is_list(List) ->
            lists:map(
                fun(Item)->
                    {ok, Res} = norm:to_rule_type(Item, Types),
                    Res
                end,
                List
            )
    end.

normpair(Types)->
    fun
        (null) ->
            {null, null};
        ({[{Fitem,Sitem}]}) ->
            {ok, Fres} = norm:to_rule_type(Fitem, Types),
            {ok, Sres} = norm:to_rule_type(Sitem, Types),
            {Fres, Sres}
    end.

normcond(Types)->
    fun(Params) ->
        norm:norm(Params, [
            #norm_rule{
                key         = iregex,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = regex,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = contains,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = icontains,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = startswith,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = istartswith,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = endswith,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = iendswith,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = lt,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = gt,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = lte,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = gte,
                required    = false,
                types       = [Types]
            },
            #norm_rule{
                key         = in,
                required    = false,
                types       = [normlist([Types])]
            },
            #norm_rule{
                key         = between,
                required    = false,
                types       = [normlist([Types])]
            }
        ])
    end.


