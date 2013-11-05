%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Jekerl static site builder
%%%
%%% @end
%%% Created :  21st October 2013 by gordonguthrie@backawinner.gg

-module(make_site).

-export([
         make_site/9,
         make_site/1,
         normalise/1
        ]).

-include("html.hrl").
-include("jekerl.hrl").

make_site(Title, URL, Description, InputDir, OutputDir, AssetsDir,
          BlogDir, DefModule, Options) ->
    Site = #site{title       = Title,
                 url         = URL,
                 description = Description,
                 inputdir    = InputDir,
                 outputdir   = OutputDir,
                 assetsdir   = AssetsDir,
                 blogdir     = BlogDir,
                 defaults    = DefModule,
                 options     = Options
                },
    make_site(Site).

make_site(Site) when is_record(Site, site) ->
    ok = do_housekeeping(Site#site.outputdir),
    NewSite = process_files([Site#site.inputdir], Site),
    NewSite2 = process_site(NewSite),
    ok = write_site(NewSite2),
    ok = copy_assets(Site#site.assetsdir, Site#site.outputdir),
    ok.

copy_assets(AssetsDir, OutputDir) ->
    [H | _T] = lists:reverse(string:tokens(AssetsDir, "/")),
    To = lists:flatten([OutputDir, "/_", H]),
    ok = delete_directory(To),
    ok = recursive_copy(AssetsDir, To).

process_site(#site{pages    = Pages,
                   defaults = DefModule} = Site) ->
    Cs = process_s2(Pages, [], false, #components{}),
    #components{navigation = N,
                sidebar    = SB,
                rss        = RSS,
                sitemap    = SM} = Cs,
    Cs2 = Cs#components{navigation = DefModule:make_navigation(N),
                          sidebar    = DefModule:make_sidebar(SB),
                          rss        = finalise_rss(Site, RSS),
                          sitemap    = finalise_sitemap(SM)},
    Site#site{components = Cs2}.

process_s2([], _RevPath, _IsBlog, Components) ->
    Components;
process_s2([{Seg, {Ps, Cnts}} | T], RevPath, IsBlog, Cmps) ->
    NewIsBlog = case {IsBlog, Seg} of
                    {true, _}    -> true;
                    {false, "_"} -> true;
                    _            -> false
                end,
    C3 = case Ps of
             [] -> Cmps;
             _  -> N2 = get_nav(Ps, []),
                   C2 = #components{navigation = N2},
                   merge(Cmps, C2, Seg, RevPath)
         end,
    {RSS2, SM2} = get_rss_sitemap(Ps, [], []),
    S2 = get_sidebar(Ps, IsBlog, []),
    C4 = #components{sidebar = S2,
                     rss     = RSS2,
                     sitemap = SM2},
    C5 = merge(C3, C4, Seg, RevPath),
    C7 = case Cnts of
             [] -> C5;
             _  -> C6 = process_s2(Cnts, [Seg | RevPath],
                                   NewIsBlog, #components{}),
                   merge(C5, C6, Seg, RevPath)
         end,
    process_s2(T, RevPath, IsBlog, C7).

merge(C1, C2, Seg, RevPath) ->
    #components{navigation = N1,
                sidebar    = SB1,
                rss        = R1,
                sitemap    = SM1} = C1,
    #components{navigation = N2,
                sidebar    = SB2,
                rss        = R2,
                sitemap    = SM2} = C2,
    Dir = string:join(lists:reverse(RevPath), "/"),
    Seg2 = case Seg of
               "_" -> "Blog";
               _   -> Seg
           end,
    SubNav = case N2 of
                 none -> none;
                 _    -> #navigation{dir    = Dir,
                                     file   = Seg2,
                                     subnav = N2}
             end,
    NewM = merge3(N1, SubNav),
    #components{navigation = NewM,
                sidebar    = merge2(SB1, SB2),
                rss        = merge2(R1, R2),
                sitemap    = merge2(SM1, SM2)}.

merge2(none, none) -> [];
merge2(none, M)    -> M;
merge2(M,    none) -> M;
merge2(M1,   M2)   -> lists:merge([M1, M2]).

merge3(none, none)                  -> [];
merge3(none, M)    when is_list(M)  -> M;
merge3(none, M)                     -> [M];
merge3(M,    none) when is_list(M)  -> M;
merge3(M,    none)                  -> [M];
merge3(M1,   M2)   when is_list(M1) -> #navigation{file = File} = M2,
                                       lists:keystore(File, 3, M1, M2);
merge3(M1,   M2)                    -> lists:merge([M1], [M2]).

finalise_rss(#site{title       = Title,
                   url         = URL,
                   description = Desc}, Items) ->
    RSS = "<rss version='2.0'>\n" ++
        "<channel>\n" ++
        "<title>" ++ Title ++ "</title>\n" ++
        "<link>" ++ URL ++ "</link>\n" ++
        "<description>" ++ Desc ++ "</desc>\n" ++
        "<pubDate>" ++ dh_date:format("Y M d H is") ++ "</pubDate>\n" ++
        "<docs>http://blogs.law.harvard.edu/tech/rss</docs>\n" ++
        "<generator>Jekerl http://github.com/hypernumbers/jekerl"
        ++ "</generator>\n" ++
        string:join(Items, "\n") ++ "\n" ++
        "</channel>\n" ++
        "</rss>\n",
    lists:flatten(RSS).

finalise_sitemap(Items) ->
    SM = "<?xml version='1.0' encoding='UTF-8'>\n" ++
        "<urlset xmlns='http://www.sitemaps.org/schemas/sitemap/0.9' \n" ++
        "xmlns:image='http://www.google.com/schemas/sitemap-image/1.1' \n" ++
        "xmlns:video='http://www.google.com/schemas/sitemap-video/1.1'>\n" ++
        string:join(Items, "\n") ++ "\n" ++
        "</urlset>\n",
    lists:flatten(SM).

get_rss_sitemap([], RSS, Sitemap) ->
    {RSS, Sitemap};
get_rss_sitemap([P | T], RSS, Sitemap) ->
    NewRSS = make_RSS(P),
    NewSM  = make_sitemap(P),
    get_rss_sitemap(T, [NewRSS | RSS], [NewSM  | Sitemap]).

get_nav([], Nav) ->
    Nav;
get_nav([P | T], Nav) ->
    NewNav = make_line(P),
    get_nav(T, [NewNav | Nav]).

make_RSS(#page{title      = Title,
               date       = Date,
               outputfile = {Dir, File},
               rss_hash   = Hash}) ->
    Name = normalise(File),
    Dir2 = case Dir of
               [] -> "";
               _  -> string:join(Dir, "/") ++ "/"
           end,
    "<item>\n" ++
        "<title>" ++ Title ++ "</title>\n" ++
        "<link><a href='/" ++ Dir2 ++ File ++ "'>" ++
        Name ++ "</a></link>\n" ++
        "<pubDate>" ++ dh_date:format("Y M d H i s", Date) ++ "</pubDate>\n" ++
        "<guid>" ++ Hash ++ "</guid>\n" ++
        "</item>\n".

normalise(File) ->
    File2 = filename:rootname(filename:basename(File)),
    re:replace(File2, "-", " ", [global, {return, list}]).

make_sitemap(#page{outputfile = {Dir, File}}) ->
    Dir2 = case Dir of
               [] -> "";
               _  -> string:join(Dir, "/") ++ "/"
           end,
    URL = "/" ++ Dir2 ++ File,
    "<url><loc>" ++ URL ++ "</loc></url>".

make_line(#page{outputfile = {Dir, File}}) ->
    Dir2 = string:join(Dir, "/"),
    #navigation{dir = Dir2, file = File}.

%% no blog no sidebar
get_sidebar(_, false, Acc) ->
    Acc;
get_sidebar([], true, Acc) ->
    Acc;
get_sidebar([#page{title = Title,
                   author = Author,
                   date = Date,
                   outputfile = {Dir, File}} | T], true, Acc ) ->
    URL = string:join(Dir, "/") ++ "/" ++ File,
    Sidebar = #sidebar{title  = Title,
                       url    = URL,
                       date   = Date,
                       author = Author},
    get_sidebar(T, true, [Sidebar | Acc]).

write_site(#site{pages      = Pages,
                 outputdir  = OutputDir,
                 components = Cs} = Site) ->
    #components{rss     = RSS,
                sitemap = SM} = Cs,
    ok = write_files(Pages, [], false, Site),
    ok = file:write_file(OutputDir ++ "/sitemap.xml", SM),
    ok = file:write_file(OutputDir ++ "/site.rss", RSS),
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
    #site{inputdir = InputDir} = Site,
    Main = ["<p><a href='./" ++ X ++ "'>" ++ X ++ "</a></p>"
            || #page{outputfile = {_, X}} <- Pages],
    Path = make_path(string:tokens(InputDir, "/"), string:tokens(Dir, "/")),
    Page = #page{main       = lists:flatten(Main),
                 outputfile = {Path, "index.html"}},
    write_file(Dir, Page, Site).

write_contents(Dir, Contents, Site) ->
    #site{inputdir = InputDir} = Site,
    Main = ["<a href='./" ++ X ++ "/'>" ++ X ++ "</a>" || {X, _} <- Contents],
    Path = make_path(string:tokens(InputDir, "/"), string:tokens(Dir, "/")),
    Page = #page{main       = lists:flatten(Main),
                 outputfile = {Path, "index.html"}},
    write_file(Dir, Page, Site).

write_blank(Dir, Site) ->
    #site{inputdir = InputDir,
          defaults = DefModule} = Site,
    Path = make_path(string:tokens(InputDir, "/"), string:tokens(Dir, "/")),
    Page = #page{main       = DefModule:blank_page(),
                 outputfile = {Path, "index.html"}},
    write_file(Dir, Page, Site).

write_file(Dir, P, Site) ->
    {FileName, HTML} = make_html(P, Site),
    File = Dir ++ "/" ++ FileName,
    ok = filelib:ensure_dir(File),
    file:write_file(File, HTML).

make_html(Page, Site) ->
    #site{title      = Title,
          defaults   = DefModule,
          components = Components} = Site,
    #page{title      = PageTitle,
          assets     = Assets,
          outputfile = {Crumbs, FileName},
          main       = Main} = Page,
    Signature = DefModule:signature(),
    BrowserTitle = case PageTitle of
                       [] -> Title;
                       _  -> PageTitle
                   end,
    Head = html:head([
                      DefModule:title(BrowserTitle),
                      DefModule:meta(Assets#assets.meta),
                      DefModule:js_head(Assets#assets.js_head),
                      DefModule:css(Assets#assets.css)
                     ]),
    Body = html:body([
                      DefModule:layout(PageTitle,
                                       Main,
                                       Components#components.navigation,
                                       DefModule:crumbs(Crumbs, FileName),
                                       Components#components.sidebar,
                                       DefModule:footer()),
                      DefModule:js_foot(Assets#assets.js_foot)
                      ]),
    HTML = lists:flatten([
                          DefModule:doctype(),
                          html:html([
                                     Head,
                                     Signature,
                                     Body
                                    ], "en")
                         ]),
    {FileName, HTML}.

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
    case filename:dirname(File) of
        Blogdir -> Dest = make_dest(File, InputDir, DefModule, Page2, true),
                   Page2#page{is_blog    = true,
                              outputfile = Dest};
        _       -> Dest = make_dest(File, InputDir, DefModule, Page2, false),
                   Page2#page{is_blog    = false,
                              outputfile = Dest}
    end.

make_dest(File, InputDir, DefModule, Page, true) ->
    Dir = ["_" | DefModule:blog_dir(Page)],
    Path = make_path(string:tokens(InputDir, "/"), Dir),
    HTML = filename:basename(File, ".hyde") ++ ".html",
    {Path, HTML};
make_dest(File, InputDir, _DefModule, _Page, false) ->
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
extract({meta, Meta}, #page{assets = Assets} = Page)
  when is_list(Meta) ->
    #assets{meta = M} = Assets,
    Page#page{assets = Assets#assets{meta = lists:merge(Meta, M)}};
extract({css, CSS}, #page{assets = Assets} = Page)
  when is_list(CSS) ->
    #assets{css = C} = Assets,
    Page#page{assets = Assets#assets{css = lists:merge(CSS, C)}};
extract({js_head, JS_Head}, #page{assets = Assets} = Page)
  when is_list(JS_Head) ->
    #assets{js_head = JSH} = Assets,
    Page#page{assets = Assets#assets{js_head = lists:merge(JS_Head, JSH)}};
extract({js_foot, JS_Foot}, #page{assets = Assets} = Page)
  when is_list(JS_Foot) ->
    #assets{js_foot = JSF} = Assets,
    Page#page{assets = Assets#assets{js_foot = lists:merge(JS_Foot, JSF)}};
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
    %% now get a hash to give a unique ID in RSS
    Block =term_to_binary([
                           P#page.main,
                           P#page.title,
                           P#page.author,
                           P#page.date
                          ]),
    Hash = binary_to_list(crypto:hash(md4, Block)),
    Hash2 = binary_to_list(base64:encode(Hash)),
    P#page{main     = NewM2,
           rss_hash = Hash2}.

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

%% Recursively copy directories
-spec recursive_copy(list(), list()) -> ok.
recursive_copy(From, To) ->
    {ok, Files} = file:list_dir(From),
    [ok = rec_copy1(From, To, X) || X <- Files],
    ok.

% ignore hidden
rec_copy1(_From, _To, [$. | _T]) ->
    ok;
rec_copy1(From, To, File) ->

    NewFrom = filename:join(From, File),
    NewTo   = filename:join(To, File),

    case filelib:is_dir(NewFrom) of

        true  ->
            ok = filelib:ensure_dir(NewTo),
            recursive_copy(NewFrom, NewTo);

        false ->
            case filelib:is_file(NewFrom) of
                true  ->
                    ok = filelib:ensure_dir(NewTo),
                    {ok, _} = file:copy(NewFrom, NewTo),
                    ok;
                false ->
                    ok
            end
    end.

%% Delete a directory (and all its children)
-spec delete_directory(string()) -> ok.
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
