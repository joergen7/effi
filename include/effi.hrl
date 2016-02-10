%% ------------------------------------------------------------
%% Type definitions
%% ------------------------------------------------------------

-type result() :: {finished, #{string() => [string()]}}
                | failed.

%% ------------------------------------------------------------
%% Function specifications
%% ------------------------------------------------------------

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

-spec listen_port( Port, LineAcc, ResultAcc ) -> Result
when Port      :: port(),
     LineAcc   :: iolist(),
     ResultAcc :: #{string() => [string()]},
     Result    :: result().
				      
-spec parse_assoc( AssocStr ) -> ResultMap
when AssocStr  :: string(),
     ResultMap :: #{string => [string()]}.
				       

