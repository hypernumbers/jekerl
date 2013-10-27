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

-type def_module() :: atom().

-record(site, {
          timestamp = dh_date:format("D dS F Y H:g:s"),
          inputdir                                      :: list(),
          outputdir                                     :: list(),
          assetsdir                                     :: list(),
          blogdir                                       :: list(),
          defaults                                      :: def_module(),
          options    = []                               :: list(),
          navigation = []                               :: list(),
          pages      = []                               :: [#page{}]
         }).
