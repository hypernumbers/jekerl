%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Jekerl static site builder
%%%
%%% @end
%%% Created :  21st October 2013 by gordonguthrie@backawinner.gg

-module(make_site).

-export([
         make_site/6,
         make_site/1
        ]).

-include("html.hrl").
-include("jekerl.hrl").

make_site(InputDir, OutputDir, AssetsDir, BlogDir, DefModule, Options) ->
    Site = #site{inputdir  = InputDir,
                 outputdir = OutputDir,
                 assetsdir = AssetsDir,
                 blogdir   = BlogDir,
                 defaults  = DefModule,
                 options   = Options
                },
    make_site(Site).

make_site(Site) when is_record(Site, site) ->
    ok = do_housekeeping(Site#site.outputdir),
    NewSite = process_files([Site#site.inputdir], Site),
    ok = write_site(NewSite),
    ok.

write_site(#site{pages = Pages} = Site) ->
    ok = write_files(Pages, [], false, Site),
    ok.

write_files([], _RevPath, _IsBlog, _Site) ->
    ok;
write_files([{Seg, {Pages, Contents}} | T], RevPath, IsBlog, Site) ->
    NewIsBlog = case {IsBlog, Seg} of
                    {true, _}    -> true;
                    {false, "_"} -> true;
                    _            -> false
                end,
    #site{outputdir = O} = Site,
    Dir = string:join([O | lists:reverse([Seg | RevPath])], "/"),
    case Pages of
        [] -> case NewIsBlog of
                  false -> ok = write_blank(Dir, Site);
                  true  -> ok = write_contents(Dir, Contents, Site)
              end;
        _  -> case NewIsBlog of
                  true  -> ok = write_index(Dir, Pages, Site);
                  false -> ok
              end,
              [ok = write_file(Dir, X, Site) || X <- Pages]
    end,
    ok = write_files(Contents, [Seg | RevPath], NewIsBlog, Site),
    %% cut back to the original IsBlog value
    write_files(T, RevPath, IsBlog, Site).

write_index(Dir, Pages, Site) ->
    Main = ["<p><a href='./" ++ X ++ "'>" ++ X ++ "</a></p>"
            || #page{outputfile = {_, X}} <- Pages],
    Page = #page{main       = lists:flatten(Main),
                 outputfile = {Dir, "index.html"}},
    write_file(Dir, Page, Site).

write_contents(Dir, Contents, Site) ->
    Main = ["<a href='./" ++ X ++ "/'>" ++ X ++ "</a>" || {X, _} <- Contents],
    Page = #page{main       = lists:flatten(Main),
                 outputfile = {Dir, "index.html"}},
    write_file(Dir, Page, Site).

write_blank(Dir, Site) ->
    #site{defaults = DefModule} = Site,
    Page = #page{main       = DefModule:blank_page(),
                 outputfile = {Dir, "index.html"}},
    write_file(Dir, Page, Site).

write_file(Dir, P, Site) ->
    {FileName, HTML} = make_html(P, Site),
    File = Dir ++ "/" ++ FileName,
    ok = filelib:ensure_dir(File),
    file:write_file(File, HTML).

make_html(#page{main = M, outputfile = {_, FileName}}, _Site) ->
    {FileName, M}.

process_files([], Site) ->
    Site;
process_files([H | T], Site) ->
    NewS = case filelib:is_dir(H) of
               true  -> case H of
                            "_" ++ _R -> Site;
                            _         -> {ok, Fs} = file:list_dir(H),
                                         Fs2 = [filename:join(H, X) || X <- Fs],
                                         process_files(Fs2, Site)
                        end;
               false -> process(H, #page{}, Site)
           end,
    process_files(T, NewS).

process(File, Page, #site{pages    = Pages,
                          inputdir = InputDir,
                          blogdir  = Blogdir,
                          defaults = DefModule} = Site) ->
    case filename:extension(File) of
        ".hyde" -> NewPage = p2(File, InputDir, Blogdir, DefModule, Page),
                   #page{outputfile = {Dir, _Name}} = NewPage,
                   Site#site{pages = add_page(Dir, NewPage, Pages)};
        _       -> Site
    end.

p2(File, InputDir, Blogdir, DefModule, Page) ->
    {ok, Page2} = read_lines(File, Page),
    #page{date = Date} = Page2,
    case filename:dirname(File) of
        Blogdir -> Dest = make_dest(File, InputDir, DefModule, Date, true),
                   Page2#page{is_blog    = true,
                              outputfile = Dest};
        _       -> Dest = make_dest(File, InputDir, DefModule, Date, false),
                   Page2#page{is_blog    = false,
                              outputfile = Dest}
    end.

make_dest(File, _InputDir, DefModule, Date, true) ->
    Dir = ["_" | DefModule:blog_dir(Date)],
    HTML = filename:basename(File, ".hyde") ++ ".html",
    {Dir, HTML};
make_dest(File, InputDir, _DefModule, _Date, false) ->
    Dir = string:tokens(filename:dirname(File), "/"),
    Path = make_path(string:tokens(InputDir, "/"), Dir),
    HTML = filename:basename(File, ".hyde") ++ ".html",
    {Path, HTML}.

make_path([H | T1], [H | T2]) ->
    make_path(T1, T2);
make_path(_, Dir) ->
    Dir.

read_lines(File, Page) ->
    case file:open(File, read) of
        {error, Err} -> {error, Err};
        {ok, Id}     -> InHeader = true,
                        read_l2(Id, InHeader, Page)
    end.

read_l2(Id, InHeader, Page) ->
    case file:read_line(Id) of
        {ok, Data}   -> {NewIH, NewPage} = handle(Data, InHeader, Page),
                        read_l2(Id, NewIH, NewPage);
        {error, Err} -> {error, Err};
        eof          -> {ok, finalise(Page)}
    end.

handle("%" ++ _Rest, _InHeader, Page) ->
    {false, Page};
handle(Line, true, Page) ->
    try
        {ok, Toks, _} = erl_scan:string(Line),
        {ok, Term} = erl_parse:parse_term(Toks),
        {true, extract(Term, Page)}
    catch
        exit:  _Reason -> {true, Page};
        error: _Reason -> {true, Page};
        throw: _Reason -> {true, Page}
    end;
handle(Data, false, #page{main = M} = Page) ->
    {false, Page#page{main = [Data | M]}}.

extract({date, Date}, Page) ->
    Page#page{date = dh_date:parse(Date)};
extract({author, Author}, Page) ->
    Page#page{author = Author};
extract({title, Title}, Page) ->
    Page#page{title = Title};
extract({type, Type}, Page) ->
    Page#page{type = Type};
extract({tags, Tags}, Page) ->
    Page#page{tags = Tags};
extract(_, Page) ->
    Page.

finalise(#page{type = Type,
               main = M} = P) ->
    NewM = lists:flatten(lists:reverse(M)),
    NewM2 = case Type of
                html     -> NewM;
                text     -> "<pre>" ++ NewM ++ "</pre>";
                markdown -> markdown:conv(NewM)
            end,
    P#page{main = NewM2}.

add_page([], Page, Pages) ->
    NewV = case lists:keyfind("", 1, Pages) of
               false                -> {[Page],      []};
               {"", {Ps, Contents}} -> {[Page | Ps], Contents}
           end,
    lists:keystore("", 1, Pages, {"", NewV});
add_page([H | []], Page, Pages) ->
    NewV = case lists:keyfind(H, 1, Pages) of
               false               -> {[Page],      []};
               {H, {Ps, Contents}} -> {[Page | Ps], Contents}
           end,
    lists:keystore(H, 1, Pages, {H, NewV});
add_page([H | T], Page, Pages) ->
    NewV = case lists:keyfind(H, 1, Pages) of
               false               -> {[], add_page(T, Page, [])};
               {H, {Ps, Contents}} -> {Ps, add_page(T, Page, Contents)}
           end,
    lists:keystore(H, 1, Pages, {H, NewV}).

do_housekeeping(Dir) ->
    case has_dir(Dir) of
        true  -> ok = clear_old_files(Dir);
        false -> filelib:ensure_dir(Dir ++ "/nonce.file")
    end.

has_dir(Dir) ->
    case file:list_dir(Dir) of
        {error, _} -> false;
        {ok, _}    -> true
    end.

clear_old_files(Dir) ->
    case file:list_dir(Dir) of
        {error, _}  -> ok; % directory doesn't exist, that's ok
        {ok, Files} -> [ok = file:delete(Dir ++ "/" ++ X) || X <- Files],
                       ok
    end.

%%%
%%% Unit Tests and stuff
%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_runner(Actions, Expected) ->
    Got = run_actions(Actions, []),
    io:format("Got is      ~p~nExpected is ~p~n", [Got, Expected]),
    ?assertEqual(Expected, Got).

run_actions([], Pages) ->
    Pages;
run_actions([{add_page, H} | T], Pages) ->
    #page{outputfile = {Dir, _Name}} = H,
    NewPs = add_page(Dir, H, Pages),
    run_actions(T, NewPs).

root_test() ->
    Path = [],
    Page = #page{outputfile = {Path, name}},
    Expected = [{"", {[Page], []}}],
    test_runner([{add_page, Page}], Expected).

root_2_test() ->
    Path1 = [],
    Page1 = #page{outputfile = {Path1, name}},
    Path2 = ["yando"],
    Page2 = #page{outputfile = {Path2, name}},
    Expected = [
                {"",      {[Page1], []}},
                {"yando", {[Page2], []}}
               ],
    test_runner([
                 {add_page, Page1},
                 {add_page, Page2}
                ], Expected).

%% swap the order of adding pages
root_2a_test() ->
    Path1 = [],
    Page1 = #page{outputfile = {Path1, name}},
    Path2 = ["yando"],
    Page2 = #page{outputfile = {Path2, name}},
    Expected = [
                {"yando", {[Page2], []}},
                {"",      {[Page1], []}}
               ],
    test_runner([
                 {add_page, Page2},
                 {add_page, Page1}
                ], Expected).

simple_test() ->
    Path = ["bish"],
    Page = #page{outputfile = {Path, name}},
    Expected = [{"bish", {[Page], []}}],
    test_runner([{add_page, Page}], Expected).

simple_1_test() ->
    Path = ["bish", "bosh", "bash"],
    Page = #page{outputfile = {Path, name}},
    Expected = [
                {"bish", {[], [
                               {"bosh", {[], [
                                              {"bash", {[Page], []}}
                                             ]
                                        }
                               }
                              ]
                         }
                }
               ],
    test_runner([{add_page, Page}], Expected).

simple_2_test() ->
    Path1 = ["bish"],
    Page1 = #page{outputfile = {Path1, name}},
    Path2 = ["bish", "bosh"],
    Page2 = #page{outputfile = {Path2, name}},
    Expected = [{"bish", {[Page1], [{"bosh", {[Page2], []}}]}}],
    test_runner([
                 {add_page, Page1},
                 {add_page, Page2}
                ], Expected).

%% swap the order of adding pages
simple_2a_test() ->
    Path1 = ["bish"],
    Page1 = #page{outputfile = {Path1, name}},
    Path2 = ["bish", "bosh"],
    Page2 = #page{outputfile = {Path2, name}},
    Expected = [{"bish", {[Page1], [{"bosh", {[Page2], []}}]}}],
    test_runner([
                 {add_page, Page2},
                 {add_page, Page1}
                ], Expected).

simple_3_test() ->
    Path1 = ["bish"],
    Page1 = #page{outputfile = {Path1, name}},
    Path2 = ["bish", "bosh"],
    Page2 = #page{outputfile = {Path2, name}},
    Path3 = ["bish", "boosh"],
    Page3 = #page{outputfile = {Path3, name}},
    Expected = [{"bish", {[Page1], [
                                    {"bosh",  {[Page2], []}},
                                    {"boosh", {[Page3], []}}
                                   ]
                         }
                }
               ],

    test_runner([
                 {add_page, Page1},
                 {add_page, Page2},
                 {add_page, Page3}
                ], Expected).


%% swap the order of adding pages
simple_3a_test() ->
    Path1 = ["bish"],
    Page1 = #page{outputfile = {Path1, name}},
    Path2 = ["bish", "bosh"],
    Page2 = #page{outputfile = {Path2, name}},
    Path3 = ["bish", "boosh"],
    Page3 = #page{outputfile = {Path3, name}},
    Expected = [{"bish", {[Page1], [
                                    {"bosh",  {[Page2], []}},
                                    {"boosh", {[Page3], []}}
                                   ]
                         }
                }
               ],
    test_runner([
                 {add_page, Page2},
                 {add_page, Page3},
                 {add_page, Page1}
                ], Expected).

simple_4_test() ->
    Path1 = ["bish"],
    Page1 = #page{outputfile = {Path1, name}},
    Path2 = ["bish", "bosh"],
    Page2a = #page{outputfile = {Path2, name}},
    Page2b = #page{outputfile = {Path2, nom}},
    Expected = [{"bish", {[Page1], [
                                    {"bosh",  {[Page2b, Page2a], []}}
                                   ]
                         }
                }
               ],
    test_runner([
                 {add_page, Page1},
                 {add_page, Page2a},
                 {add_page, Page2b}
                ], Expected).

%% swap the order of adding pages
simple_4a_test() ->
    Path1 = ["bish"],
    Page1 = #page{outputfile = {Path1, name}},
    Path2 = ["bish", "bosh"],
    Page2a = #page{outputfile = {Path2, name}},
    Page2b = #page{outputfile = {Path2, nom}},
    Expected = [{"bish", {[Page1], [
                                    {"bosh",  {[Page2b, Page2a], []}}
                                   ]
                         }
                }
               ],
    test_runner([
                 {add_page, Page2a},
                 {add_page, Page1},
                 {add_page, Page2b}
                ], Expected).

-endif.
