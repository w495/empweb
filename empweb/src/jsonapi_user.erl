-module(jsonapi_user).

-include("empweb.hrl").

-include_lib("norm/include/norm.hrl").

-export([
    init/2,
    get/2,
    register/2,
    update/2,
    login/2,
    logout/2,
    delete_friend/2,
    add_friend/2,
    add_friend/2,
    get_friends/2,
    at_list_one/1,
    terminate/2
]).


-record(jsonapi_user,{
    params  =   [],
    is_auth =   false,
    action
}).

init(Req, Params)->
    {ok, Req, #jsonapi_user{params=Params}}.


register(Req, #jsonapi_user{params=Params}) ->
    jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key = nick,
                types = [string]
            },
            #norm_rule{
                key = description,
                types = [string]
            },
            #norm_rule{
                key = pass,
                types = [string]
            },
            #norm_rule{
                key = email,
                types = [email]
            },
            #norm_rule{
                key = phone,
                types = [string]
            },
            #norm_rule{
                key = fname,
                types = [string]
            },
            #norm_rule{
                key = sname,
                types = [string]
            },
            #norm_rule{
                key = birthday,
                types = [string]
            },
            #norm_rule{
                key = city,
                types = [string]
            }
        ]),
        fun(Data)->
            {ok, Body} = biz_user:register(Data#norm.return),
            {ok, #empweb_resp{
                status  = ok,
                format = json,
                body = Body
            }, []}
        end
    ).

get_friends(Req, #jsonapi_user{params=Params}) ->
    io:format("Params = ~p", [Params]),
    jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key = user_id,
                types = [integer]
            }
        ]),
        fun(Data)->
            jsonapi:is_auth(Req,
                fun
                    ({ok, Body})->
                        {ok, #empweb_resp{
                            status  = ok,
                            format = json,
                            body = {[{ok, Body}]}
                        }, []};
                    ({error, Error})->
                        {error, Error}
                end,
                [biz_user:get_friends(Data#norm.return)]
            )
        end
    ).

add_friend(Req, #jsonapi_user{params=Params}) ->
    io:format("Params = ~p", [Params]),
    jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key = user_id,
                types = [integer]
            },
            #norm_rule{
                key = friend_id,
                types = [integer]
            }
        ]),
        fun(Data)->
            jsonapi:is_auth(Req,
                fun
                    ({ok, Body})->
                        {ok, #empweb_resp{
                            status  = ok,
                            format = json,
                            body = {[{ok, Body}]}
                        }, []};
                    ({error, Error})->
                        {error, Error}
                end,
                [biz_user:add_friend(Data#norm.return)]
            )
        end
    ).

delete_friend(Req, #jsonapi_user{params=Params}) ->
    io:format("Params = ~p", [Params]),
    jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key = user_id,
                types = [integer]
            },
            #norm_rule{
                key = friend_id,
                types = [integer]
            }
        ]),
        fun(Data)->
            jsonapi:is_auth(Req,
                fun
                    ({ok, Body})->
                        {ok, #empweb_resp{
                            status  = ok,
                            format = json,
                            body = {[{ok, Body}]}
                        }, []};
                    ({error, Error})->
                        {error, Error}
                end,
                [biz_user:delete_friend(Data#norm.return)]
            )
        end
    ).


get(Req, #jsonapi_user{params=Params}) ->
    io:format("Params = ~p", [Params]),
    jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key = id,
                required = false,
                types = [integer]
            },
            #norm_rule{
                key = nick,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = name,
                required = false,
                types = [string]
            }
        ]),
        fun(Data)->
            jsonapi:is_auth(Req,
                fun
                    ({ok, Body})->
                        {ok, #empweb_resp{
                            status  = ok,
                            format = json,
                            body = {[{ok, Body}]}
                        }, []};
                    ({error, Error})->
                        {error, Error}
                end,
                [biz_user:get(at_list_one(Data#norm.return))]
            )
        end
    ).

at_list_one([]) ->
    {error, <<"no parm">>};
at_list_one([Return|_]) ->
    Return.


update(Req, #jsonapi_user{params=Params}) ->
    jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key = id,
                types = [integer]
            },
            #norm_rule{
                key = nick,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = description,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = pass,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = email,
                required = false,
                types = [email]
            },
            #norm_rule{
                key = phone,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = hobby,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = fname,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = sname,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = birthday,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = city,
                required = false,
                types = [string]
            }
        ]),
        fun(Data)->
            jsonapi:is_auth(Req,
                fun
                    ({ok, Body})->
                        {ok, #empweb_resp{
                            status  = ok,
                            format = json,
                            body = {[{ok, Body}]}
                        }, []};
                    ({error, Error})->
                        {error, Error}
                end,
                [biz_user:update(Data#norm.return)]
            )
        end
    ).

login(Req, #jsonapi_user{params=Params}) ->
    jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key = nick,
                types = [string]
            },
            #norm_rule{
                key = pass,
                types = [string]
            }
        ]),
        fun(Data)->
            {ok, Body} = biz_user:login(Data#norm.return),
            {ok, #empweb_resp{
                status  = ok,
                format = json,
                cookies = [empweb_http:make_auth_cookie(Body)],
                body = Body
            }, []}
        end
    ).

logout(Req, #jsonapi_user{params=Params}) ->
    jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key = nick,
                types = [string]
            }
        ]),
        fun(Data)->
            {ok, Body} = biz_user:logout([
                {session_id, empweb_http:auth_cookie(Req)}
                | Data#norm.return
            ]),
            {ok, #empweb_resp{
                status  = ok,
                format = json,
                body = Body
            }, []}
        end
    ).

terminate(Req, #jsonapi_user{params=Params})->

    ok.