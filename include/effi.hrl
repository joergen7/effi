%% ------------------------------------------------------------
%% Type definitions
%% ------------------------------------------------------------

-type result() :: {finished, #{string() => [string()]}, string()}
                | {failed, string()}.

%% ------------------------------------------------------------
%% Function specifications
%% ------------------------------------------------------------

-spec destroy_port( Port::port() ) -> ok.

-spec create_port( Lang, Script, Dir, OutList, ParamMap, TypeMap ) -> Port
when Lang     :: atom(),
     Script   :: iodata(), 
     Dir      :: string(),
     OutList  :: [string()],
     ParamMap :: #{string() => string() | [string()]},
     TypeMap  :: #{string() => boolean()},
     Port     :: port().

-spec run( Lang, Script, Dir, OutList, ParamMap, TypeMap ) -> Result
when Lang     :: atom(),
     Script   :: iodata(), 
     Dir      :: string(),
     OutList  :: [string()],
     ParamMap :: #{string() => string() | [string()]},
     TypeMap  :: #{string() => boolean()},
     Result   :: result().

-spec listen_port( Port, LineAcc, ResultAcc, OutAcc ) -> Result
when Port      :: port(),
     LineAcc   :: iolist(),
     ResultAcc :: #{string() => [string()]},
     OutAcc    :: iolist(),
     Result    :: result().
				      
-spec parse_assoc( AssocStr ) -> ResultMap
when AssocStr  :: string(),
     ResultMap :: #{string => [string()]}.
				       
-spec spawn_link_run( Lang, Script, Dir, OutList, ParamMap, TypeMap ) -> Pid
when Lang     :: atom(),
     Script   :: iodata(), 
     Dir      :: string(),
     OutList  :: [string()],
     ParamMap :: #{string() => string() | [string()]},
     TypeMap  :: #{string() => boolean()},
     Pid      :: pid().
