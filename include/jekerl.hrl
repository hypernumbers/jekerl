-record(page, {
          title      = []    :: list(),
          author     = []    :: list(),
          date       = []    :: list(),
          type       = text  :: html | text | markdown,
          is_blog    = false :: boolean(),
          outputfile = []    :: list(),
          tags       = []    :: list(),
          main       = []    :: list()
         }).

-record(site, {
          timestamp = dh_date:format("D dS F Y H:g:s"),
          outputdir                                     :: list(),
          assetsdir                                     :: list(),
          blogdir                                       :: list(),
          navigation = []                               :: list(),
          pages      = []                               :: [#page{}]
         }).
