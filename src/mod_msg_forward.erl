-module(mod_msg_forward).

-behaviour(gen_mod).

-include("logger.hrl").

-include("translate.hrl").

-export([start/2, 
         stop/1, 
         depends/2, 
         mod_options/1, 
         mod_opt_type/1, 
         mod_doc/0]).

-export([msg_forward/2]).

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(url) ->
        econf:any().

mod_options(Host) -> [
{url, <<"http://", Host/binary, ":8081/msg">>}
].

start(_Host, _Opts) ->
        % 添加挂钩函数
        ejabberd_hooks:add(msg_forward, ?MODULE, msg_forward, 50),
        ?INFO_MSG("Mod Hello World Start...", []),
        ok.

stop(_Host) ->
        ?INFO_MSG("Mod Hello World Stopped...", []),
        ok.

depends(_Host,_Opts) ->
        [].

% msg_forward(_Host) ->
%         URL = gen_mod:get_module_opt(_Host, mod_msg_forward, url),
%         ?INFO_MSG("[Asklv] [Run msg_forward]: url -> ", [URL]),
%         ok.

% todo: 这里可以拆分为进程间通信，将这部分的操作解耦合
msg_forward(LServer, EncodedJsonBody) ->
        ?INFO_MSG("aaaaaaaaaa", []),
	Method = post,
	% URL = "http://192.168.12.222:8081/msg",
	URL = atom_to_list(gen_mod:get_module_opt(LServer, mod_msg_forward, url)),
	Header = [],
	Type = "application/x-www-form-urlencoded",
	Body = "data=" ++ binary:bin_to_list(EncodedJsonBody),
	HTTPOptions = [],
	Options = [],
	case httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options) of
		{ok, {_, _, Body}} ->
			?INFO_MSG("[Asklv] [HTTP Post] receive body ~p", [Body]); % 这里是子句
		{error, Reason} ->
			?INFO_MSG("[Asklv] [HTTP Post] error cause ~p", [Reason])
	end,
        ok.

mod_doc() ->
        #{desc => 
                ?T("This is an example module.")}.
% 设置参数类型mod_opt_type， mod_options
