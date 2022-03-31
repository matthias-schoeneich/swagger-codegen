
# getting started

```
./run-in-docker.sh generate -t modules/swagger-codegen/src/main/resources/erlang-server -i modules/swagger-codegen/src/test/resources/2_0/petstore.yaml -l erlang-server -o /gen/out/petstore
```


* we get what is currently in .out/petstore
* to change
    * rebar.config
        * add cowboy 2.x
    * in swagger\_pet\_handler 
        * `is_authorized/2` is same like on stock codegen, so for local just remove them and add one that returns `{true, Req, State}.
        * it seems form params are not supported so for now.. add something in get_request_spec where errors occur... does not matter for playing
* then we can do:

```bash
$rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling swagger
Erlang/OTP 24 [erts-12.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V12.3  (abort with ^G)
1>
```

```erlang
application:ensure_all_started(swagger),
Swr = swagger_router:get_paths(swagger_logic_handler),
Dispatch_HTTP = cowboy_router:compile(Swr),
cowboy:start_clear(server,[{port, 8090}],#{env => #{dispatch => Dispatch_HTTP}}).
```

```bash
$curl -v --trace-time -H 'accept: application/json' localhost:8090/v2/pet/1 
23:34:25.069155 *   Trying ::1:8090...
23:34:25.069388 * TCP_NODELAY set
23:34:25.070133 * connect to ::1 port 8090 failed: Verbindungsaufbau abgelehnt
23:34:25.070275 *   Trying 127.0.0.1:8090...
23:34:25.070426 * TCP_NODELAY set
23:34:25.070816 * Connected to localhost (127.0.0.1) port 8090 (#0)
23:34:25.071104 > GET /v2/pet/1 HTTP/1.1
23:34:25.071104 > Host: localhost:8090
23:34:25.071104 > User-Agent: curl/7.68.0
23:34:25.071104 > accept: application/json
23:34:25.071104 > 
23:34:25.073860 * Mark bundle as not supporting multiuse
23:34:25.074185 < HTTP/1.1 200 OK
23:34:25.074820 < content-length: 141
23:34:25.075131 < content-type: application/json
23:34:25.075409 < date: Thu, 31 Mar 2022 21:34:25 GMT
23:34:25.075712 < server: Cowboy
23:34:25.076115 < 
23:34:25.076633 * Connection #0 to host localhost left intact
{"category":{"id":2,"name":"irgendwas"},"id":1,"name":"dagma","photoUrls":["http bla"],"status":"available","tags":[{"id":3,"name":"a tag"}]}
```
