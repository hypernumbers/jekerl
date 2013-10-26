-type html() :: 'div' | span | h1 | h2 | h3 | h4 | h5 | h6
                | a | img.

-record(tag, {
          type         = 'div' :: html(),
          id           = []    :: string(),
          classes      = []    :: [string()],
          tag_attrs    = []    :: [{atom(), string()}],
          custom_attrs = []    :: [{atom(), string()}],
          contents     = []    :: [#tag{}]
         }).
