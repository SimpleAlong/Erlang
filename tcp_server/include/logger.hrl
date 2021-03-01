%%%-------------------------------------------------------------------
%%% @author yilin.jiang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 四月 2015 20:12
%%%-------------------------------------------------------------------
-ifndef(_logger_h_).
-define(_logger_h_, true).

-define(fmt(Format), "[~p_~p] " ++ Format ++ "~n").

-define(DEBUG_LOG(Format), ?DEBUG_LOG(Format, [])).
-define(DEBUG_LOG(Format, Args), lager:log(debug, self(), ?fmt(Format), [?MODULE, ?LINE | Args])).

-define(INFO_LOG(Format), ?INFO_LOG(Format, [])).
-define(INFO_LOG(Format, Args), lager:log(info, self(), ?fmt(Format), [?MODULE, ?LINE | Args])).

-define(NOTICE_LOG(Format), ?NOTICE_LOG(Format, [])).
-define(NOTICE_LOG(Format, Args), lager:log(notice, self(), ?fmt(Format), [?MODULE, ?LINE | Args])).

-define(WARNING_LOG(Format), ?WARNING_LOG(Format, [])).
-define(WARNING_LOG(Format, Args), lager:log(warning, self(), ?fmt(Format), [?MODULE, ?LINE | Args])).

-define(ERROR_LOG(Format), ?ERROR_LOG(Format, [])).
-define(ERROR_LOG(Format, Args), lager:log(error, self(), ?fmt(Format), [?MODULE, ?LINE | Args])).

-define(CRITICAL_LOG(Format), ?CRITICAL_LOG(Format, [])).
-define(CRITICAL_LOG(Format, Args), lager:log(critical, self(), ?fmt(Format), [?MODULE, ?LINE | Args])).

-define(ALERT_LOG(Format), ?ALERT_LOG(Format, [])).
-define(ALERT_LOG(Format, Args), lager:log(alert, self(), ?fmt(Format), [?MODULE, ?LINE | Args])).

-define(EMERGENCY_LOG(Format), ?EMERGENCY_LOG(Format, [])).
-define(EMERGENCY_LOG(Format, Args), lager:log(emergency, self(), ?fmt(Format), [?MODULE, ?LINE | Args])).

-define(TODO_LOG(), ?TODO_LOG("")).
-define(TODO_LOG(Format), ?TODO_LOG(Format, [])).
-define(TODO_LOG(Format, Args), ?ALERT_LOG("[~p][~ts] " ++ Format ++ "......", [?FUNCTION_NAME, <<"预留操作"/utf8>> | Args])).

-endif.
