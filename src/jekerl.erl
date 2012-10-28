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
-define(AMP, $&).

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
    ok = delete_directory(Posts),
    ok = file:make_dir(Posts),

    % ok, now write pages
    write_pages(Body, Titles2, TitleIdx, YearIdx, TagIdxs).

process_taxonomy([], _N, Acc)      -> lists:reverse(Acc);
process_taxonomy([[] | T], N, Acc) -> process_taxonomy(T, N + 1, Acc);
process_taxonomy([H | T], N, Acc)  -> process_taxonomy(T, N + 1, [{H, N} | Acc]).

write_pages([], _Titles, _TitleIdx, _YearIdx, _TagIdxs) ->
    ok;
write_pages([H | T], Titles, TitleIdx, YearIdx, TagIdxs) ->
    Title = normalize(element(TitleIdx, H)),
    % Fun = fun(Tag) ->
    %              Tag2 = element(Tag, H),
    %              Tag3 = normalize(Tag2),
    %              flatten(Tag3, partial)
    %      end,
    % Tags = [Fun(X) || X <- TagIdxs],
    {Year, Month, Day} = date(),
    Prefix = integer_to_list(Year) ++ "-" ++ pad(Month) ++ "-" ++ pad(Day),
    File = Prefix ++ "-" ++ flatten(Title, full) ++ ".md",
    Page = "---\n"
        ++ "layout: text\n"
        ++ "title: \"" ++ Title ++ "\"\n"
        ++ "description: \n"
        ++ "category: \n"
        ++ "tags: " ++ "[]\n" % io_lib:format("~p", [Tags]) ++ "\n"
        ++ "---\n"
        ++ "\n"
        ++ "{% include JB/setup %}\n"
        ++ "\n"
        ++ "## " ++ Title ++ "\n"
        ++ "\n"
        ++ make_page(H, Titles) ++ "\n",
    ok = file:write_file(?ROOTDIR ++ "priv/_posts/" ++ File, Page),
    write_pages(T, Titles, TitleIdx, YearIdx, TagIdxs).

flatten(List, Type) -> fl(List, Type, []).

fl([], _Type, Acc)             -> lists:reverse(Acc);
fl([?SPACE | T], full, Acc)    -> fl(T, full, [?HYPHEN | Acc]);
fl([?SPACE | T], partial, Acc) -> fl(T, partial, [?SPACE | Acc]);
fl([H | T], Type, Acc)         ->
    if
        H >= 65 andalso H =< 90  -> fl(T, Type, [H | Acc]);
        H >= 97 andalso H =< 122 -> fl(T, Type, [H | Acc]);
        H >= 48 andalso H =< 57  -> fl(T, Type, [H | Acc]);
        true                     -> fl(T, Type, Acc)
    end.

make_page(Tuple, Titles) ->
    List = tuple_to_list(Tuple),
    "<dl class='dl-horizontal'>\n"
        ++ make_rows(List, Titles, []) ++ "\n"
        ++ "</dl>\n".

make_rows([], [], Acc) -> string:join(lists:reverse(Acc), "\n");
make_rows([H1 | T1], [H2 | T2], Acc) ->
    Row = "<dt>" ++ H2 ++ "</dt>"
        ++ "<dd>" ++ normalize(H1) ++ "</dd>",
    make_rows(T1, T2, [Row | Acc]).

% shonky
normalize(List) -> norm(List, []).

norm([], Acc)                         -> lists:flatten(lists:reverse(Acc));
norm([?SPACE, ?AMP, ?SPACE | T], Acc) -> norm(T, [" and " | Acc]);
norm([?AMP, ?SPACE | T], Acc)         -> norm(T, ["and" | Acc]);
norm([?SPACE, ?AMP | T], Acc)         -> norm(T, ["and" | Acc]);
norm([H | T], Acc)                    -> norm(T, [H | Acc]).

pad(N) when N < 10 -> "0" ++ integer_to_list(N);
pad(N)             -> integer_to_list(N).

delete_directory(From) ->
    case file:list_dir(From) of
        {ok, Files} ->
            file:list_dir(From),
            [ok = delete_dir(filename:join(From, File)) || File <- Files],
            ok = file:del_dir(From);
        _Else ->
            ok
    end.

delete_dir(File) ->
    case filelib:is_dir(File) of
        true  -> delete_directory(File);
        false -> file:delete(File)
    end.
