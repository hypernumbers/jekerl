-type def_module() :: atom().

-type hyde_type() :: html
                   | text
                   | markdown.

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
          type       = text      :: hyde_type(),
          is_blog    = false     :: boolean(),
          assets     = #assets{} :: #assets{},
          outputfile = []        :: list(),
          tags       = []        :: list(),
          rss_hash   = []        :: list(),
          main       = []        :: list()
         }).

-record(navigation, {
          dir    = [] :: list(),
          file   = [] :: list(),
          subnav = [] :: list()
         }).

-record(rss, {
          title   = [] :: list(),
          url     = [] :: list(),
          pubdate = [] :: list()
         }).

-record(sidebar, {
          title  = [] :: list(),
          url    = [] :: list(),
          date   = [] :: list(),
          author = [] :: list()
         }).

-record(sitemap, {
          url     = [] :: list(),
          lastmod = [] :: list()
         }).

-record(components, {
          navigation = none,
          sidebar    = none,
          rss        = none,
          sitemap    = none
         }).

-record(site, {
          title       = []                                :: list(),
          url         = []                                :: list(),
          description = []                                :: list(),
          timestamp   = dh_date:format("D dS F Y H:g:s")  :: list(),
          inputdir                                        :: list(),
          outputdir                                       :: list(),
          assetsdir                                       :: list(),
          blogdir                                         :: list(),
          assets     = #assets{}                          :: #assets{},
          defaults                                        :: def_module(),
          components = #components{}                      :: #components{},
          options    = []                                 :: list(),
          pages      = []                                 :: [#page{}]
         }).
