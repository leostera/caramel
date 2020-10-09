-module(expressions).

f() -> #{tag=>warning_msg, report_cb=>fun gen_server:format_log/1}.
f() -> ["** Generic server ",P," terminating \n",
           "** Last message in was ",P,"~n",
           "** When Server state == ",P,"~n",
           "** Reason for termination ==~n** ",P,"~n"].
