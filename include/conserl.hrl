
-define(API_VERSION, "v1").
-define(CONTENT_FORM, "application/x-www-form-urlencoded; charset=UTF-8").
-define(CONTENT_JSON, "application/json; charset=UTF-8").
-define(SCHEME, "http").

-record(state, {host, port, acl}).
