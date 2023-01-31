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

% mod_msg_forward:
% aws_access_key_id: *
% aws_secret_access_key: *
% aws_region: us-east-1
% stream_name: EjabberdMessage
% partition_key: abc

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(aws_access_key_id) ->
        econf:any();
mod_opt_type(aws_secret_access_key) ->
        econf:any();
mod_opt_type(aws_region) ->
        econf:any();
mod_opt_type(stream_name) ->
        econf:any();
mod_opt_type(partition_key) ->
        econf:any().

mod_options(_Host) -> [
% {aws_access_key_id, <<"http://", Host/binary, ":8081/msg">>}
        {aws_access_key_id, ""},
        {aws_secret_access_key, ""},
        {aws_region, "us-east-1"},
        {stream_name, "EjabberdMessage"},
        {partition_key, "abc"}
].

start(_Host, _Opts) ->
        % 添加钩子函数
        ejabberd_hooks:add(msg_forward, ?MODULE, msg_forward, 50),
        ?INFO_MSG("Mod msg_forward Start...", []),

        % 需要转换为字符串
        AWS_ACCESS_KEY_ID = atom_to_list(gen_mod:get_module_opt(_Host, mod_msg_forward, aws_access_key_id)),
        AWS_SECRET_ACCESS_KEY = atom_to_list(gen_mod:get_module_opt(_Host, mod_msg_forward, aws_secret_access_key)),

        application:set_env(erlcloud, aws_access_key_id, AWS_ACCESS_KEY_ID),
        application:set_env(erlcloud, aws_secret_access_key, AWS_SECRET_ACCESS_KEY),
        case application:ensure_all_started(erlcloud) of 
                {ok, _Started} -> 
                        ?INFO_MSG("[Asklv] [mod_msg_forward] Load erlcloud success ...", []);
                {error, Resaon} ->
                        ?INFO_MSG("[Asklv] [mod_msg_forward] Load erlcloud failed ..., ~p", [Resaon]);
                _ ->
                        ?INFO_MSG("[Asklv] [mod_msg_forward] Load erlcloud failed ...", [])
        end,
        ok.

stop(_Host) ->
        ?INFO_MSG("Mod msg_forward Stopped...", []),
        ok.

depends(_Host,_Opts) ->
        [].

% msg_forward(_Host) ->
%         URL = gen_mod:get_module_opt(_Host, mod_msg_forward, url),
%         ?INFO_MSG("[Asklv] [Run msg_forward]: url -> ", [URL]),
%         ok.

msg_forward(LServer, EncodedJsonBody) ->
        ?INFO_MSG("[Asklv] [Start Kinesis]", []),
        % 注意：这里拿到的数据默认是原子
        StreamName = atom_to_binary(gen_mod:get_module_opt(LServer, mod_msg_forward, stream_name)),
        PartitionKey = atom_to_binary(gen_mod:get_module_opt(LServer, mod_msg_forward, partition_key)),
        case erlcloud_kinesis:put_record(StreamName, PartitionKey, EncodedJsonBody) of
                {ok, Result} -> 
                        ?INFO_MSG("[Asklv] [Kinesis Put Record] success: ~p", [Result]);
                {error, Reason} ->
                        ?INFO_MSG("[Asklv] [Kinesis Put Record] failed: ~p", [Reason])
        end,
        ok.

% todo: 这里可以拆分为进程间通信，将这部分的操作解耦合
% msg_forward(LServer, EncodedJsonBody) ->

%         % Client = aws_client:make_client(AccessKeyID, SecretAccessKey, Region),
%         % {ok, Result, _Response} = aws_kinesis:list_streams(Client, 10),
%         ?INFO_MSG("aaaaaaaaaa", []),
% 	Method = post,
% 	% URL = "http://192.168.12.222:8081/msg",
% 	URL = atom_to_list(gen_mod:get_module_opt(LServer, mod_msg_forward, url)),
% 	Header = [],
% 	Type = "application/x-www-form-urlencoded",
% 	Body = "data=" ++ binary:bin_to_list(EncodedJsonBody),
% 	HTTPOptions = [],
% 	Options = [],
% 	case httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options) of
%                 % 是否需要在这里重试
%                 % 这里匹配有问题？，有点奇怪
% 		% {ok, {_, _, Body}} ->
% 			% ?INFO_MSG("[Asklv] [HTTP Post] receive body ~p", [Body]); % 这里是子句
%                 {ok, _} ->
%                         ?INFO_MSG("[Asklv] [HTTP Post] receive success", []);
% 		{error, Reason} ->
% 			?INFO_MSG("[Asklv] [HTTP Post] error cause ~p", [Reason])
% 	end,
%         ok.

mod_doc() ->
        #{desc => 
                ?T("Forward data to aws kinesis data stream.")}.
% 设置参数类型mod_opt_type， mod_options
