-record(assets, {
          meta    = [] :: list(),
          css     = [] :: list(),
          js_head = [] :: list(),
          js_foot = [] :: list()
         }).

-record(page, {
          title      = []        :: list(),
          author     = []        :: list(),
          date       = []        :: list(),
          type       = text      :: html | text | markdown,
          is_blog    = false     :: boolean(),
          assets     = #assets{} :: #assets{},
          outputfile = []        :: list(),
          tags       = []        :: list(),
          main       = []        :: list()
         }).

-type def_module() :: atom().

-record(site, {
          timestamp = dh_date:format("D dS F Y H:g:s"),
          inputdir                                      :: list(),
          outputdir                                     :: list(),
          assetsdir                                     :: list(),
          blogdir                                       :: list(),
          assets     = #assets{}                        :: #assets{},
          defaults                                      :: def_module(),
          options    = []                               :: list(),
          navigation = []                               :: list(),
          pages      = []                               :: [#page{}]
         }).
