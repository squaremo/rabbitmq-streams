-define(FEEDSHUB_VERSION, <<"prototype">>).

-define(FEEDSHUB_CONFIG_XNAME, <<"feedshub/config">>).
-define(FEEDSHUB_LOG_XNAME, <<"feedshub/log">>).
-define(FEEDSHUB_NOTIFY_XNAME, <<"feedshub/notify">>).

-ifdef(debug).
-define(DEBUGREPORT(Term), error_logger:info_report({?MODULE, 'DEBUG', Term})).
-else.
-define(DEBUGREPORT(Term), ok).
-endif.
