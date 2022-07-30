-module(mgmtd_schema_tests).

-include_lib("eunit/include/eunit.hrl").

parse_ae_config_schema_test() ->
    %% io:format(user, "CWD ~p~n", [file:get_cwd()]),
    mgmtd_schema:load_json_schema_file("test/aeternity_config_schema.json"),
    mgmtd_schema:remove_schema().