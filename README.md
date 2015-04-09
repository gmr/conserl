Conserl
=======
An Erlang client library for [Consul](http://consul.io).

Requirements
------------
- Consul 0.5+
- Erlang 17.0+

Features
--------
- 

Environment Variables
---------------------

 Name     | Type          | Default       | Description
----------|---------------|---------------|----------------------------------------------------------
 hostname | list()        | ``127.0.0.1`` | The IP address of the Consul server
 port     | pos_integer() | ``8500``      | The HTTP port of the Consul server
 acl      | list()        | undefined     | The ACL to use when making requests to the Consul server

Todo
----
Implement endpoints for the following areas:

- kv
- agent
- catalog
- health
- sessions
- acl
- events
- status

Examples
--------
