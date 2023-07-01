-module( effi_fs ).

-export( [decode/1, encode/2] ).

-include_lib( "kernel/include/file.hrl" ).

-define( FILENAME_LIST, "_array" ).
-define( PREFIX_INT, ":int:" ).
-define( PREFIX_REAL, ":real:" ).
-define( PREFIX_BOOL, ":bool:" ).

-type data_type() :: binary()
				   | integer()
				   | float()
				   | boolean()
				   | [data_type()]
				   | #{ atom() => data_type() }.

-spec decode( Filename :: string() ) -> data_type().

decode( Filename ) ->
	case file:read_file_info( Filename ) of
		{error, Reason} ->
			error( {Reason, Filename} );
		{ok, FileInfo} ->
			case FileInfo of

				%% a directory -> either object or list
				#file_info{ type = directory } ->
					case file:list_dir( Filename ) of
						{error, Reason} ->
							error( Reason );
						{ok, FilenameList} ->
							case lists:member( ?FILENAME_LIST, FilenameList ) of

								%% list
								true ->
									decode_list( Filename );

								%% object
								false ->
									decode_object( Filename, FilenameList )
							end
					end;

				%% a regular file -> string
				#file_info{ type = regular } ->
					decode_string( Filename );
			
				%% something else
				#file_info{ type = Type } ->
					error( {unknown_type, Type} )
			end
				
	end.

-spec decode_list( Dir :: string() ) -> data_type().

decode_list( Dir ) ->
	ListFile = string:join( [Dir, ?FILENAME_LIST], "/" ),
	case file:open( ListFile, [read] ) of
		{error, Reason} ->
			error( Reason );
		{ok, IoDevice} ->
			decode_list_on( Dir, IoDevice )
	end.

-spec decode_list_on( Dir :: string(), IoDevice :: pid() ) -> data_type().

decode_list_on( Dir, IoDevice ) ->
	case file:read_line( IoDevice ) of
		{error, Reason} ->
			error( Reason );
		eof ->
			[];
		{ok, Data} ->
			Filename = string:trim( Data, trailing, "\n" ),
			[decode( string:join( [Dir, Filename], "/" ) )|decode_list_on( Dir, IoDevice )]
	end.
	

-spec decode_object( Dir :: string(), FilenameList :: [string()] ) -> data_type().

decode_object( Dir, FilenameList ) ->

	F =
		fun( Filename, Acc ) ->
				Acc#{ list_to_atom( Filename ) => decode( string:join( [Dir, Filename], "/" ) ) }
		end,

	lists:foldl( F, #{}, FilenameList ).

-spec decode_string( Filename :: string() ) -> data_type().

decode_string( Filename ) ->
	case file:read_file( Filename ) of
		{ok, B} ->
			case B of
				<<?PREFIX_INT, B1/binary>> ->
					binary_to_integer( B1 );
				<<?PREFIX_BOOL, B2/binary>> ->
					case B2 of
						<<"true">> ->
							true;
						<<"false">> ->
							false;
						_ ->
							error( badarg )
					end;
				<<?PREFIX_REAL, B3/binary>> ->
					binary_to_float( B3 );
				_ ->
					B
			end;
		{error, Reason} ->
			error( Reason )
	end.
	
	
-spec encode( Filename :: string(), Data :: data_type() ) -> ok.

encode( Filename, Data ) when is_binary( Data ) ->
	file:write_file( Filename, Data );

encode( Filename, Data ) when is_float( Data ) ->
	B = float_to_binary( Data ),
	file:write_file( Filename, <<?PREFIX_REAL, B/binary>> );

encode( Filename, Data ) when is_integer( Data ) ->
	B = integer_to_binary( Data ),
	file:write_file( Filename, <<?PREFIX_INT, B/binary>> );

encode( Filename, true ) ->
	file:write_file( Filename, <<?PREFIX_BOOL, "true">> );

encode( Filename, false) ->
	file:write_file( Filename, <<?PREFIX_BOOL, "false">> );

encode( Filename, Data ) when is_list( Data ) ->
	case make_dir( Filename ) of
		{error, Reason} ->
			error( Reason );
		ok ->
			ListFile = string:join( [Filename, ?FILENAME_LIST], "/"),
			case file:open( ListFile, [write] ) of
				{error, Reason} ->
					error( Reason );
				{ok, IoDevice} ->
					encode_list( IoDevice, Data, 0 ),
					case file:close( IoDevice ) of
						ok ->
							ok;
						{error, Reason} ->
							error( Reason )
					end
			end
	end.

-spec encode_list( Filename, DataList, IoDevice, Id ) -> ok
			  when Filename :: string(),
				   DataList :: [datatype()],				   
				   IoDevice :: pid(),
				   Id       :: nonnegative_integer().


encode_list( _Filename, [], _IoDevice, _Id ) ->
	ok;

encode_list( Filename [First|Rest], IoDevice, Id ) ->
	S = io:format( "~p", [Id] ),
	file:write( IoDevice, File )


