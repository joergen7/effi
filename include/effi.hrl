%% ------------------------------------------------------------
%% Type definitions
%% ------------------------------------------------------------

-type result() :: {finished, #{string() => [string()]}, [binary()]}
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
     Port     :: {port(), iolist()}.

-spec run( Lang, Script, Dir, OutList, ParamMap, TypeMap ) -> Result
when Lang     :: atom(),
     Script   :: iodata(), 
     Dir      :: string(),
     OutList  :: [string()],
     ParamMap :: #{string() => string() | [string()]},
     TypeMap  :: #{string() => boolean()},
     Result   :: result().

-spec listen_port( Port, ActScript, LineAcc, ResultAcc, OutAcc ) -> Result
when Port      :: port(),
     ActScript :: iolist(),
     LineAcc   :: binary(),
     ResultAcc :: #{string() => [string()]},
     OutAcc    :: [binary()],
     Result    :: result().
				      
-spec parse_assoc( AssocStr ) -> ResultMap
when AssocStr  :: string() | binary(),
     ResultMap :: #{string => [string()]}.
				       

