%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Gordon Guthrie
%%% @doc       jekerl reads cvs files and turns
%%%            them into jekyll websites
%%%
%%% @end
%%% Created : 28 Oct 2012 by gordon@vixo.com

-module(jekerl).

-export([
         convert_file/1
         ]).

-define(ROOTDIR, "/home/gordon/jekerl/").

-define(SPACE, 32).
-define(HYPHEN, $-).

convert_file(FileName) ->
    io:format("Converting ~p~n", [FileName]),
    Records = erfc_4180:parse_file(?ROOTDIR ++ "priv/upload/" ++ FileName, []),

    % get the file details from the first 2 records
    [Titles , Taxonomy| Body] = Records,
    Titles2 = tuple_to_list(Titles),
    Taxonomy2 = tuple_to_list(Taxonomy),
    Taxonomy3 = process_taxonomy(Taxonomy2, 1, []),
    {"Year", YearIdx} = lists:keyfind("Year", 1, Taxonomy3),
    {"Title", TitleIdx} = lists:keyfind("Title", 1, Taxonomy3),
    TagIdxs = [X || {"Tag", X} <- Taxonomy3 ],

    % now tidy up the posts directory
    Posts = ?ROOTDIR ++ "priv/_posts",
    file:del_dir(Posts),
    file:make_dir(Posts),

    % ok, now write pages
    write_pages(Body, Titles2, TitleIdx, YearIdx, TagIdxs).

process_taxonomy([], _N, Acc)      -> lists:reverse(Acc);
process_taxonomy([[] | T], N, Acc) -> process_taxonomy(T, N + 1, Acc);
process_taxonomy([H | T], N, Acc)  -> process_taxonomy(T, N + 1, [{H, N} | Acc]).

write_pages([], _Titles, _TitleIdx, _YearIdx, _TagIdxs) ->
    ok;
write_pages([H | T], Titles, TitleIdx, YearIdx, TagIdxs) ->
    Title = element(TitleIdx, H),
    Year = element(YearIdx, H),
    Tags = [element(X, H) || X <- TagIdxs],
    {DefYear, _, _} = date(),
    Year2 = case Year of
                ""  -> integer_to_list(DefYear);
                _   -> Year
            end,
    File = Year2 ++ "-01-01-" ++ flatten(Title, []) ++ ".md",
    Page = "---\n"
        ++ "layout: text\n"
        ++ "title: " ++ Title ++ "\n"
        ++ "tags: " ++ io_lib:format("~p", [Tags]) ++ "\n"
        ++ "---\n"
        ++ "\n"
        ++ make_page(H, Titles),
    ok = file:write_file(?ROOTDIR ++ "priv/_posts/" ++ File, Page),
    write_pages(T, Titles, TitleIdx, YearIdx, TagIdxs).

flatten([], Acc)                  -> lists:reverse(Acc);
flatten([?SPACE | T], Acc)        -> flatten(T, [?HYPHEN | Acc]);
flatten([H | T], Acc) ->
    if
        H >= 65 andalso H =< 90  -> flatten(T, [H | Acc]);
        H >= 97 andalso H =< 122 -> flatten(T, [H | Acc]);
        H >= 48 andalso H =< 57  -> flatten(T, [H | Acc]);
        true                     -> flatten(T, Acc)
    end.

make_page(Tuple, Titles) ->
    List = tuple_to_list(Tuple),
    "<dl class='dl=horizontal'>\n"
        ++ make_rows(List, Titles, [])
        ++ "</dl>\n".

make_rows([], [], Acc) -> string:join(lists:reverse(Acc), "\n");
make_rows([H1 | T1], [H2 | T2], Acc) ->
    Row = "<dt>" ++ H2 ++ "</dt>"
        ++ "<dd>" ++ H1 ++ "</dd>",
    make_rows(T1, T2, [Row | Acc]).
