%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Jekerl static site builder
%%%
%%% @end
%%% Created :  21st October 2013 by gordonguthrie@backawinner.gg

-module(make_site).

-export([
         make_site/5
        ]).

-include("html.hrl").
-include("jekerl.hrl").

make_site(InputDir, OutputDir, AssetsDir, BlogDir, _Options) ->
    Dirs = [InputDir, OutputDir, AssetsDir],
    [ok = filelib:ensure_dir(X ++ "/junk.txt") || X <- Dirs],
    Site = #site{outputdir = OutputDir,
                 assetsdir = AssetsDir,
                 blogdir   = BlogDir},
    NewSite = process_files([InputDir], Site),
    io:format("NewSite is ~p~n", [NewSite]),
    ok.

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

process(File, Page, #site{pages   = Pages,
                          blogdir = Blogdir} = Site) ->
    case filename:extension(File) of
        ".hyde" -> NewPage = p2(File, Blogdir, Page),
                   Site#site{pages = [NewPage | Pages]};
        _       -> Site
    end.

p2(File, Blogdir, Page) ->
    {ok, Page2} = read_lines(File, Page),
    #page{date = Date} = Page2,
    case filename:dirname(File) of
        Blogdir -> Dest = make_dest(File, Date, true),
                   Page2#page{is_blog    = true,
                              outputfile = Dest};
        _       -> Dest = make_dest(File, Date, false),
                   Page2#page{is_blog    = false,
                              outputfile = Dest}
    end.

make_dest(File, {{Y, M, D}, _}, true) ->
    Dir = "_"
     ++ integer_to_list(D)
     ++ "/"
     ++ integer_to_list(M)
     ++ "/"
     ++ integer_to_list(Y)
     ++ "/",
    HTML = filename:basename(File, ".hyde") ++ ".html",
    {Dir, HTML};
make_dest(File, _, false) ->
    Dir = filename:dirname(File) ++ "/",
    HTML = filename:basename(File, ".hyde") ++ ".html",
    {Dir, HTML}.

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

