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
    normpair/1,
    normpair/2,
    normfilter/1,
    filter/1,
    fieldtrigger/2,
    fieldtrigger/3,
    filter_owner/1,
    filter_owner/2,
    filter_owner/3,
    filter_self/1
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
    filter_owner(Params, {owner_id, pers_id}).

filter_owner(Params, {F1, F2}) ->
    filter_owner(Params, {F1, F2}, []).

filter_owner(Params, {F1, F2}, Options) ->
    case {
        proplists:get_value(F1, Params),
        proplists:get_value(F2, Params)
    } of
        {all     , _      } ->
            proplists:delete(F2, proplists:delete(F1, Params));
        {undefined, Value2} ->
            [{F1, Value2}|proplists:delete(F2, Params)];
        {_value1, _      } ->
            proplists:delete(F2, Params)
    end.

fieldtrigger(Params, {F1, F2}) ->
    fieldtrigger(Params, {F1, F2}, []).

fieldtrigger(Params, {F1, F2}, Options) ->
    case {
        proplists:get_value(F1, Params),
        proplists:get_value(F2, Params)
    } of
        {all     , _      } ->
            proplists:delete(F2, proplists:delete(F1, Params));
        {undefined, Value2} ->
            [{F1, Value2}|proplists:delete(F2, Params)];
        {_value1, _      } ->
            proplists:delete(F2, Params)
    end.

filter_self(Params) ->
    case {
        proplists:get_value(id, Params),
        proplists:get_value(pers_id, Params)
    } of
        {all     , _      } ->
            proplists:delete(pers_id, proplists:delete(id, Params));
        {undefined, Pers_id} ->
            [{id, Pers_id}|proplists:delete(pers_id, Params)];
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
        },
        #norm_rule{
            key         = image_width,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
            %default     = null
        },
        #norm_rule{
            key         = image_scale_width,
            required    = false,
            types       = empweb_norm:filter([nullable, float])
            %default     = null
        },
        #norm_rule{
            key         = window_width,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
            %default     = null
        },
        #norm_rule{
            key         = image_height,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
            %default     = null
        },
        #norm_rule{
            key         = image_scale_height,
            required    = false,
            types       = empweb_norm:filter([nullable, float])
            %default     = null
        },
        #norm_rule{
            key         = window_height,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
            %default     = null
        },
        #norm_rule{
            key         = image_scale,
            required    = false,
            types       = empweb_norm:filter([nullable, float])
            %default     = null
        }
    ];

norm('create') ->
    [];

norm('update') ->
    norm('create');

norm('delete') ->
    norm('update');

norm([]) ->
    [];

norm(_) ->
    [].

normlist(Types)->
    fun
%         (null) ->
%             [];
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
%         (null) ->
%             {null, null};
        ({[{Fitem,Sitem}]}) ->
            {ok, Fres} = norm:to_rule_type(Fitem, Types),
            {ok, Sres} = norm:to_rule_type(Sitem, Types),
            {Fres, Sres}
    end.

normpair(Types1, Types2)->
    fun
%         (null) ->
%             {null, null};
        ({[{Fitem,Sitem}]}) ->
            {ok, Fres} = norm:to_rule_type(Fitem, Types1),
            {ok, Sres} = norm:to_rule_type(Sitem, Types2),
            {Fres, Sres}
    end.

normfilter(Types)->
    fun
%         (null) ->
%             {null, null};
        ({[{Fitem,Sitem}]}) ->
            {ok, Fres} = norm:to_rule_type(Fitem, [atom]),
            {ok, Sres} = norm:to_rule_type(Sitem, [normlist(Types)|Types]),
            {Fres, Sres}
    end.

filter(Types)->
    [normfilter(Types)|Types].

normcond(Types)->
    fun(Params) ->
        norm:norm(Params, [
            #norm_rule{
                key         = iregex,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = regex,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = contains,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = icontains,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = startswith,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = istartswith,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = endswith,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = iendswith,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = lt,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = gt,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = lte,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = gte,
                required    = false,
                types       = Types
            },
            #norm_rule{
                key         = in,
                required    = false,
                types       = [normlist(Types)]
            },
            #norm_rule{
                key         = between,
                required    = false,
                types       = [normlist(Types)]
            }
        ])
    end.


