%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Jekerl static site builder
%%%
%%% @end
%%% Created :  21st October 2013 by gordonguthrie@backawinner.gg

-module(jekerl).

-export([
        jekerl/2
       ]).

-define(INPUTDIR,  "./priv/jekerl").
-define(OUTPUTDIR, "./priv/html").
-define(ASSETSDIR, "./priv/assets").
-define(BLOGDIR,   "./priv/jekerl/blog").

jekerl(_A, B) ->
    code:add_patha("./ebin"),
    App = filename:basename(B),
    case App of
        "jekerl.app.src" ->
            make_site:make_site(?INPUTDIR, ?OUTPUTDIR, ?ASSETSDIR, ?BLOGDIR,
                                [
                                 {disqus, "blahbalbh"}
                                ]);
        _ ->
            ok
    end.
