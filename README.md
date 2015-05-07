

# Conserl #

An Erlang client library for [Consul](http://consul.io).

[![Build Status](https://travis-ci.org/gmr/conserl.svg?branch=master)](https://travis-ci.org/gmr/conserl)


## Requirements ##

- [Consul 0.5+](https://consul.io)
- Erlang 17.5+


## Environment Variables ##
Name     | Type          | Default       | Description
--------- | ------------- | ------------- | ---------------------------------------------------------
 hostname | list()        | `127.0.0.1` | The IP address of the Consul server
 port     | pos_integer() | `8500`      | The HTTP port of the Consul server
 acl      | list()        | undefined     | The ACL to use when making requests to the Consul server


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/gmr/conserl/blob/master/doc/conserl.md" class="module">conserl</a></td></tr>
<tr><td><a href="https://github.com/gmr/conserl/blob/master/doc/conserl_agent.md" class="module">conserl_agent</a></td></tr>
<tr><td><a href="https://github.com/gmr/conserl/blob/master/doc/conserl_catalog.md" class="module">conserl_catalog</a></td></tr>
<tr><td><a href="https://github.com/gmr/conserl/blob/master/doc/conserl_kv.md" class="module">conserl_kv</a></td></tr></table>

