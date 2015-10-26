
%% ------------------------------------------------------------
%% Function specifications
%% ------------------------------------------------------------

-spec destroy_port( Port::port() ) -> ok.

-spec run( Lang, Script, Dir, OutList, ParamMap, TypeMap ) -> Port
when Lang     :: atom(),
     Script   :: iolist(),													    
     Dir      :: string(),
     OutList  :: [string()],
     ParamMap :: #{string() => string() | [string()]},
     TypeMap  :: #{string() => boolean()},
     Port     :: port().


