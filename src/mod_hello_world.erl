-module(mod_hello_world).

-behaviour(gen_mod).

%% 日志宏
% ?INFO_MSG
-include("logger.hrl").

%% 翻译宏
% ?T
-include("translate.hrl").

-export([start/2, stop/1, depends/2, mod_options/1, mod_opt_type/1, mod_doc/0]).

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(test_abc) ->
        econf:any().

mod_options(Host) -> [
{test_abc, <<"http://", Host/binary, ":5443/upload">>}
].

start(_Host, _Opts) ->
        ?INFO_MSG("Mod Hello World Start...", []),
        ok.

stop(_Host) ->
        ?INFO_MSG("Mod Hello World Stopped...", []),
        ok.

depends(_Host,_Opts) ->
        [].

mod_doc() ->
        #{desc => 
                ?T("This is an example module.")}.

% 设置参数类型mod_opt_type， mod_options