%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       page defaults for Jekerl site builder
%%%
%%% @end
%%% Created :  21st October 2013 by gordonguthrie@backawinner.gg

-module(defaults).

-export([
         blog_dir/1,
         blank_page/0,
         make_navigation/1,
         make_sidebar/1
        ]).

-include("jekerl.hrl").

%%%
%%% API
%%%

%% gets data in - returns directory structure
blog_dir(#page{date = {{Year, Month, _Day}, _}}) ->
    [
     integer_to_list(Year),
     month(Month)
    ].

%% message to be written on blank page (returns html)
blank_page() ->
    "<div>This page left intentionally blank</div>".

%% makes the navigation line
make_navigation(_Nav) ->
    erk.

%% makes the sidebar
make_sidebar(_SB) ->
    berk.

%%%
%%% Internal fns
%%%

month(1)  -> "January";
month(2)  -> "February";
month(3)  -> "March";
month(4)  -> "April";
month(5)  -> "May";
month(6)  -> "June";
month(7)  -> "July";
month(8)  -> "August";
month(9)  -> "September";
month(10) -> "October";
month(11) -> "November";
month(12) -> "December".
